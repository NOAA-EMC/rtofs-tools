#!/usr/bin/env python3

"""
Utility functions for creating CICE6 restart file.
"""
import os
import numpy as np
import sys
import datetime
import time

def read_cice4_grid(fl_grid, fld_read, IDM=4500, JDM=3297):
    """
    Read a field from a CICE4 grid file written as direct-access
    big-endian REAL*8 arrays.
    Available fields:
        kmt, ulati, uloni, htn, hte, anglet, tlati, tloni
    """
    field_index = {
        "kmt": 0,
        "ulati": 1,
        "uloni": 2,
        "htn": 3,
        "hte": 4,
        "anglet": 5,
        "tlati": 6,
        "tloni": 7,
    }

    try:
        iFld = field_index[fld_read]
    except KeyError:
        raise ValueError(
            f"Unknown field '{fld_read}'. "
            f"Available fields: {list(field_index)}"
        )

    print(f"Reading {fld_read} from {fl_grid}")
    print(f"Domain dimensions: IDM={IDM} JDM={JDM}")

    IJDM = IDM * JDM
    offset = iFld * 8 * IJDM
    with open(fl_grid, "rb") as fga:
        fga.seek(offset)
        AA = np.fromfile(fga, dtype=">f8", count=IJDM)

    if AA.size != IJDM:
        raise IOError(f"Expected {IJDM} values, got {AA.size}")

    return AA.reshape((JDM, IDM))

def grid_rad2dgr(ulat_rad, ulon_rad, f180 = True):
    """ 
    Convert CICE grid coordinates from radians to degrees.
    If f180 is True, convert longitude to the range
    -180 <= lon < 180.
    """
    rdn2dgr = 180.0 / np.pi
    ulat = ulat_rad * rdn2dgr
    ulon = ulon_rad * rdn2dgr

    # Normalize longitude to [0, 360)
    ulon = np.mod(ulon, 360.0)

    # Optionally convert to [-180, 180]
    if f180:
        ulon = np.where(ulon > 180.0, ulon - 360.0, ulon)

    ulat = np.clip(ulat, -89.99999, 89.99999)
    
    return ulat, ulon

def check_cice_grids(ulati4, uloni4, ulati6, uloni6,
                     frad=True, eps0=0.08):
    """
    Check CICE4 and CICE6 longitude/latitude grids.
    Parameters:
      ulati4, uloni4 : CICE4 latitude/longitude
      ulati6, uloni6 : CICE6 latitude/longitude
      frad : bool
        If True, input coordinates are in radians and are converted
        to degrees for the comparison.
      eps0 : float
        Maximum allowed coordinate difference in degrees.
    """
    print("Checking CICE4 & CICE6 grids")

    if frad:
        ulati4, uloni4 = grid_rad2dgr(ulati4, uloni4)
        ulati6, uloni6 = grid_rad2dgr(ulati6, uloni6)

    dlat = np.abs(ulati4 - ulati6)
    dlon = np.abs((uloni4 - uloni6 + 180.0) % 360.0 - 180.0)
    dlat_max = np.max(dlat)
    dlon_max = np.max(dlon)

    print(f"Max latitude difference  |CICE4-CICE6| = {dlat_max:.6f} deg")
    print(f"Max longitude difference |CICE4-CICE6| = {dlon_max:.6f} deg")

    if dlat_max > eps0 or dlon_max > eps0:
        print(f"Max grid-coordinate difference exceeds threshold {eps0} dgr")
        raise ValueError("CICE4/CICE6 grid mismatch")

    return

def compute_coszen(ulat, ulon, dnmb, frad=True, time_zone=0):
    """
    Compute the cosine of the solar zenith angle.

    The solar zenith angle is the angle between the sun and the
    local vertical. A negative coszen indicates that the sun is
    below the horizon.

    Uses the NOAA Global Monitoring Division low-accuracy solar
    position equations:

        https://gml.noaa.gov/grad/solcalc/solareqns.PDF

    Parameters
    ----------
    ulat : array_like
        Latitude of CICE grid points. Radians by default.
    ulon : array_like
        Longitude of CICE grid points. Radians by default.
    dnmb : float
        Date/time in MATLAB datenum format.
    frad : bool, default=True
        If True, ulat and ulon are in radians. If False, they are
        assumed to be in degrees.
    time_zone : float, default=0
        Time-zone offset from UTC in hours. For UTC use 0.

    Returns
    -------
    coszen : ndarray
        Cosine of the solar zenith angle.
    """
    dgr2rdn = np.pi / 180.0
    rdn2dgr = 180.0 / np.pi

    # Convert coordinates to degrees for the NOAA equations
    if frad:
        ulat = ulat * rdn2dgr
        ulon = ulon * rdn2dgr

    ulon = (ulon + 180.0) % 360.0 - 180.0
    DV = datevec(dnmb)
    hr = DV[3]
    if len(DV) > 4:
        mn = DV[4]
    else:
        mn = 0.0

    if len(DV) > 5:
        sc = DV[5]
    else:
        sc = 0.0

    # Julian day and number of days in the year
    jday = date2jday(DV)
    jd31 = date2jday([DV[0], 12, 31])

    # Fractional year (radians)
    gamma = (
        2.0 * np.pi / jd31
        * (jday - 1.0 + (hr - 12.0) / 24.0)
    )

    # Equation of time (minutes)
    eqtime = 229.18 * (
        7.5e-6
        + 0.001868 * np.cos(gamma)
        - 0.032077 * np.sin(gamma)
        - 0.014615 * np.cos(2.0 * gamma)
        - 0.040849 * np.sin(2.0 * gamma)
    )

    # Solar declination angle (radians)
    dlt = (
        0.006918
        - 0.399912 * np.cos(gamma)
        + 0.070257 * np.sin(gamma)
        - 0.006758 * np.cos(2.0 * gamma)
        + 0.000907 * np.sin(2.0 * gamma)
        - 0.002697 * np.cos(3.0 * gamma)
        + 0.00148 * np.sin(3.0 * gamma)
    )

    # True solar time (minutes)
    # Longitude is in degrees.
    # time_zone is hours east of UTC; use 0 for UTC.
    time_offset = eqtime + 4.0 * ulon - 60.0 * time_zone

    TST = hr * 60.0 + mn + sc / 60.0 + time_offset

    # Solar hour angle (degrees)
    SHA = TST / 4.0 - 180.0

    # Cosine of solar zenith angle
    coszen = (
        np.sin(ulat * dgr2rdn) * np.sin(dlt)
        + np.cos(ulat * dgr2rdn)
        * np.cos(dlt)
        * np.cos(SHA * dgr2rdn)
    )

    return coszen


def remap_enthalpy_bins(qicen4, nilyrs4, nilyrs6, eps_dq=1.e-5):
    """
    Remap snow or ice enthalpies from N layers to K layers (K > N).
    Enthalpy must be defined uniquely for this operation, i.e. J/m3
    (CICE6 enthalpy per volume).
    For CICE4 --> CICE6:
        qicen4 has already converted CICE4 esnon (J/m2)
        to qsnon (J/m3), then remapped onto CICE6 layers.
    Remapping is done by geometric binning. For every CICE6 layer,
    the overlap with every CICE4 layer is calculated. The CICE4
    enthalpy is weighted by the overlap thickness.
    This guarantees conservation of vertically integrated enthalpy
    (to numerical precision).

    qicen4 : ndarray (nilyrs4, ncat, ny, nx)
        Enthalpy in CICE4 layers [J/m3].
    nilyrs4 : int
        Number of CICE4 layers.
    nilyrs6 : int
        Number of CICE6 layers.
    eps_dq : float
        Maximum acceptable error in vertically integrated enthalpy.

    Returns
    qicen6 : ndarray (nilyrs6, ncat, ny, nx)
        Remapped enthalpy [J/m3].
    """
    # Check input:
    if nilyrs6 < nilyrs4:
        raise ValueError(f"nilyrs6 ({nilyrs6}) must be >= nilyrs4 ({nilyrs4})")

    if nilyrs6 == nilyrs4:
        qicen6 = qicen4.copy()
        return qicen6

    if qicen4.shape[0] != nilyrs4:
        raise ValueError(
            f"qicen4 has {qicen4.shape[0]} layers, "
            f"but nilyrs4={nilyrs4}"
        )

    _, ncat, ny, nx = qicen4.shape

    # Normalized vertical interfaces
    # z=0 - bottom, z=1 - top
    zz4 = np.linspace(0.0, 1.0, nilyrs4 + 1)
    zz6 = np.linspace(0.0, 1.0, nilyrs6 + 1)
    dh4 = 1.0 / nilyrs4
    dh6 = 1.0 / nilyrs6

    qicen6 = np.zeros((nilyrs6, ncat, ny, nx), dtype=qicen4.dtype)

    for n in range(ncat):
        for k6 in range(nilyrs6):
            # Remap each CICE6 layers

            z6btm = zz6[k6]
            z6top = zz6[k6 + 1]
        
            eta_tot = 0.0
            # Loop over all cice4 layers to find layers that overlap with cice6 layer k
            for k4 in range(nilyrs4):
              z4btm = zz4[k4]
              z4top = zz4[k4 + 1]

              eta = max(0.0, min(z6top, z4top) - max(z6btm, z4btm))

              if eta > 0.0:
                  # Fraction of CICE4 overlapping layer to CICE6 layer k6
                  # energy * fraction of layer thickness

                  qicen6[k6,n,:,:] += qicen4[k4,n,:,:] * eta / dh6
              
              eta_tot += eta

            # Check that the target layer is completely covered
            if abs(eta_tot - dh6) > 1.e-12:
                raise Exception(
                    f'Layer overlap error: '
                    f'cat={n+1}, lr={k6+1}, '
                    f'overlap={eta_tot}, '
                    f'expected={dh6}'
                )


        # Total energy conservation check for all CICE6 layers.
        # Vertically integrated CICE4 enthalpy should equal CICE6
        q4tot = np.sum(qicen4[:,n,:,:], axis=0) * dh4
        q6tot = np.sum(qicen6[:,n,:,:], axis=0) * dh6
        dq    = np.abs(q4tot - q6tot)

        print(f'Cat={n+1} Max abs dlt enthalipes in cice4 and cice6: {np.max(dq):.6e}')
        if np.max(dq) > eps_dq:
            raise Exception ('Large Error in interpolated enthalpy to cice6 ')

    return qicen6

def interp_uvelE_vvelN(uvel, vvel, aicen, puny=1.e-11, fstatus=False):
    """
    Interpolate B-grid velocity components to C-grid locations.

    U velocity is interpolated from B-grid U points to C-grid E points
    V velocity is interpolated from B-grid V points to C-grid N points

    Interpolation is performed only at grid cells with nonzero sea-ice
    concentration. At the southern and western boundaries, where the
    neighboring grid point is unavailable, the velocity at the current
    grid point is used.

    uvel : B-grid U velocity field, shape (ny, nx).
    vvel : B-grid V velocity field, shape (ny, nx).
    aicen : Sea-ice concentration by category, shape (ncat, ny, nx).

    Returns:
    uvelE : U velocity interpolated to C-grid E points.
    vvelN : V velocity interpolated to C-grid N points.
    """
    print('Interpolating uvelE and vvelN')
    uvelE = np.zeros_like(uvel)
    vvelN = np.zeros_like(vvel)
    
    aice = np.sum(aicen, axis=0).squeeze()
    ice_mask = aice > puny
    ice_indx = np.argwhere(ice_mask)
    nindx = ice_indx.shape[0]
    cntr = 0

    for jj, ii in ice_indx:
        cntr += 1
        if cntr % 100000 == 0 and fstatus:
            print(f'   processed {cntr/nindx * 100:.1f}% ...')

        # B-grid U --> C-grid E
        if jj > 0:
            uvelE[jj,ii] = 0.5 * (uvel[jj-1,ii] + uvel[jj,ii])
        else:
            uvelE[jj,ii] = uvel[jj,ii]

        # B-grid V --> C-grid N
        if ii > 0:
            vvelN[jj,ii] = 0.5 * (vvel[jj,ii-1] + vvel[jj,ii])
        else:
            vvelN[jj,ii] = vvel[jj,ii]

    if fstatus:
        print('100% Finished')
        print(f'uvel  min/max orig:   {np.nanmin(uvel):.2f}/{np.nanmax(uvel):.2f}')
        print(f'uvelE min/max interp: {np.nanmin(uvelE):.2f}/{np.nanmax(uvelE):.2f}')
        print(f'vvel  min/max orig:   {np.nanmin(vvel):.2f}/{np.nanmax(vvel):.2f}')
        print(f'vvelN min/max interp: {np.nanmin(vvelN):.2f}/{np.nanmax(vvelN):.2f}\n')

    return uvelE, vvelN

def dateint2datenum(dateInt):
    """
    Convert integer date YYYYMMDDhh to datenum
    """
    if dateInt > 1e10:
        raise ValueError("Unsupported date format: YYYYMMDD or YYYYMMDDhh")

    if dateInt > 1e8:
        year = dateInt // 1_000_000
        month = (dateInt % 1_000_000) // 10000
        day = dateInt % 10000 // 100
        hr  = dateInt % 100
        dnmb = datenum([year,month,day,hr,0])
    else:
        year = dateInt // 10000
        month = (dateInt % 10000) // 100
        day = dateInt % 100
        dnmb = int(datenum([year,month,day]))

    return dnmb

def datenum(ldate0, ldate_ref=None):
    """
    Compute the date number relative to a reference date.

    Input:
    ldate0 : list or tuple
        Current date/time as [YY, MM, DD], [YY, MM, DD, HR],
        or [YY, MM, DD, HR, MN].
    ldate_ref : list or tuple, optional
        Reference date/time in the same format as `ldate0`.
        If not provided, [1, 1, 1, 0, 0] is used.

    Output:
    dnmb : float
        Number of days relative to the reference date, with 1.0
        assigned to the reference date/time.

    Note: reference date number = 1, some conventions = 0
    For example, datenum([1, 1, 1]) returns 1.0.
    """
    if ldate_ref is None:
        ldate_ref = [1, 1, 1, 0, 0]

    # Fill missing time components with zero.
    ldate0 = list(ldate0) + [0] * (5 - len(ldate0))
    ldate_ref = list(ldate_ref) + [0] * (5 - len(ldate_ref))

    YR, MM, DD, HR, MN = map(int, ldate0[:5])
    YRr, MMr, DDr, HRr, MNr = map(int, ldate_ref[:5])

    time0 = datetime.datetime(YR, MM, DD, HR, MN)
    timeR = datetime.datetime(YRr, MMr, DDr, HRr, MNr)

    dnmb = (time0 - timeR).total_seconds() / 86400.0 + 1.0

    return dnmb

def date2jday(ldate0):
    """
    Derive day of the year for a given date 
    as a list or tuple [YYYY, MM, DD]
    """
    YR     = ldate0[0]
    dnmbJ1 = datenum([YR,1,1])
    dnmb0  = datenum(ldate0)
    jday   = dnmb0 - dnmbJ1 + 1.
    
    return jday

def datevec(dnmb, ldate_ref=None, round_hrs=False):
    """
    Convert a date number to a list: [YR, MM, DD, HR, MN].
    The date number is assumed to have been computed relative to
    ldate_ref using datenum function.
    Input:
      dnmb : int or float
    """
    if isinstance(dnmb, np.generic):
      dnmb = dnmb.item()

    if ldate_ref is None:
      ldate_ref = [1, 1, 1]

    if not (isinstance(dnmb, int) or isinstance(dnmb, float)):
      raise Exception('dnmb should be int or float, for array use datevec2D')

    YRr, MMr, DDr = ldate_ref[:3]

    if len(ldate_ref) >= 5:
      HRr, MNr = ldate_ref[3:5]
    else:
      HRr, MNr = 0, 0 

    timeR = datetime.datetime(YRr, MMr, DDr, HRr, MNr)

    # datenum convention: reference date = 1
    ndays = int(np.floor(dnmb))-1
    dfrct = dnmb-np.floor(dnmb)

    if abs(dfrct) < 1.e-6:
      HR = 0
      MN = 0
    else:
      HR = int(np.floor(dfrct * 24.))
      MN = int(np.floor(dfrct * 1440. - HR * 60.))

    if round_hrs:
      if MN >= 30:
        HR += 1
      elif MN < 30:
        MN = 0

      if HR > 24:
        HR -= 24
        ndays += 1

    time0 = timeR + datetime.timedelta(days=ndays, seconds=(HR * 3600 + MN * 60))

    dvec = [
        time0.year,
        time0.month,
        time0.day,
        time0.hour,
        time0.minute
    ]

    return dvec
