#!/usr/bin/env python3
"""
Convert a CICE4 binary restart file to a CICE6 NetCDF restart.

- Missing variables required by CICE6 are added and initialized.
- Ice and snow energies are converted to enthalpy and
  remapped from the CICE4 vertical discretization to the CICE6
  layer structure using a conservative remapping scheme that
  preserves total column energy.
"""

import os
import numpy as np
import sys
import time
import shutil
from netCDF4 import Dataset
from yaml import safe_load
import argparse
import mod_cice6rest as mc6rest

# ==============================================================================
# CICE / Icepack Thermodynamic and Other Constants
# ==============================================================================
puny      = 1.e-11
c0        = 0.0
c1        = 1.0
c2        = 2.0
p5        = 0.5
Lsub      = 2.835e6           # Latent heat sublimation fw (J/kg)
Lvap      = 2.501e6           # Latent heat vaporization fw (J/kg)
Lfresh    = Lsub - Lvap       # Latent heat of melting of fresh ice (J/kg)
cp_ice    = 2106.             # Specific heat of fresh ice (J/ kg/K)
rhos      = 330.              # Density of snow (kg/m3)
hs_min    = 1.e-4             # Min snow thickness for computing Tsno (m)
nsal      = 0.407             # Empirical constant in BL99 S profile formulation
msal      = 0.573             # Empirical constant in BL99 S profile formulation
min_salin = 0.1               # Threshold for brine pocket treatment
saltmax   = 3.2               # Max S at ice base
spval     = 1.e30             # Bad/Missing values
Tsn_min   = -100.             # minimum snow T

class CICE:
    def __init__(self, nx, ny, ncat, nilyr, nslyr):
        self.ncat = ncat
        self.nilyr = nilyr
        self.nslyr = nslyr
        self.nx = nx
        self.ny = ny
        self.ntilyr = self.ncat * self.nilyr
        self.ntslyr = self.ncat * self.nslyr

def read_cice6_grid(dirflnc, varnc):
    """Read CICE6 field varnc from a grid netcdf file."""
    with Dataset(dirflnc, 'r') as dset:
        AA = dset.variables[varnc][:].squeeze()
    return AA  

def read_rest_cice4(fid, nx, ny):
    """
    Read a 2D field from an open CICE4 restart file.
    CICE4 restart files are unformatted sequential binary records
    in big-endian format. 
    """
    recS = np.fromfile(fid, dtype='>i4', count=1)[0]
    A     = np.fromfile(fid, dtype='>f8', count=nx*ny)
    A     = np.reshape(A,(ny,nx), order='C')
    recE = np.fromfile(fid, dtype='>i4', count=1)[0]
    if recS != recE:
       raise ValueError(f"Record length mismatch: {recS} != {recE}")

    return A

def print_minmax(sfld, A):
    """Print min/max statistics of a numpy array."""
    print(f'   {sfld} min/max:  {np.nanmin(A)} / {np.nanmax(A)}')
    return

def read_cice4_layers(fid, nlrs, nx, ny, label):
    """ Read CICE4 fields by layers. """
    print(f'\n Reading {label}:')
    fld = np.zeros((nlrs, ny, nx), dtype=np.float64)

    for k in range(nlrs):
        A = read_rest_cice4(fid, nx, ny)
        fld[k, :, :] = A
        print_minmax(f"{k+1} {label}", A)

    return fld

def read_cice4_2D(fid, nx, ny, varnm):
    """Read CICE4 2D fields."""
    print(f'\nReading {varnm}:')
    A = read_rest_cice4(fid, nx, ny)
    print_minmax(varnm, A)

    return A

def energy_to_enthalpy(aicen, vicen, vsnon, eicen, esnon,
        cice4, rhos, Lfresh, cp_ice, puny, hs_min, Tsn_min):
    """
    Convert ice and snow energy used in CICE4 (J/m2)
    to enthalpy used in CICE6 (J/m3).
    The CICE4 number of snow and ice layers is preserved.
    """
    nilyr = cice4.nilyr
    nslyr = cice4.nslyr
    nx    = cice4.nx
    ny    = cice4.ny
    ncat  = cice4.ncat
    ilyr1 = np.arange(ncat) * nilyr   # ice layer index in eicen array for each cat
    slyr1 = np.arange(ncat) * nslyr   # snow lyaer index in esnon array for each cat
    qT0   = -Lfresh * rhos            # Enthalpy at the melting point
    hs_min_layer = hs_min / nslyr     # min snow thikness in a layer
    qsnon = np.zeros((nslyr, ncat, ny, nx), dtype=np.float64)
    qicen = np.zeros((nilyr, ncat, ny, nx), dtype=np.float64)
    vsnon_out = vsnon.copy()          # Working copy

    # Suppress numpy division-by-zero warnings for cleaner output and slight speedup
    with np.errstate(divide='ignore', invalid='ignore'):
        for n in range(ncat):
            vsn    = vsnon[n,:,:].copy()
            ai_cat = aicen[n,:,:].copy()
            mask_noice = ai_cat <= puny
            ai_cat[mask_noice] = puny
            hsn    = vsn / ai_cat
            vsn    = np.where(vsn < puny, puny, vsn)
            vsn    = np.where(hsn <= hs_min_layer, 0., vsn)
            mask_vol = vsn > puny

            # Snow enthalpy
            for k in range(nslyr):
                iesnon = slyr1[n] + k
                qsn   = esnon[iesnon,:,:] * nslyr / vsn
                qsn   = np.minimum(qsn, qT0)
                qsn   = np.where(mask_noice, 0., qsn)
                qsn   = np.where(hsn <= hs_min_layer, qT0, qsn)

                # Check upper / lower bounds deriving snow T from enthalpy.
                # Clip to the nearest valid value.
                zTsn  = (Lfresh + qsn / rhos) / cp_ice
                Tmax  = np.zeros_like(vsn)
                Tmax[mask_vol] = -qsn[mask_vol] * puny * nslyr / (rhos * cp_ice * vsn[mask_vol])
                qsn_Tmin = (rhos * (cp_ice * Tsn_min - Lfresh))
                qsn_Tmax = (rhos * (cp_ice * Tmax - Lfresh))
                mask_cold = zTsn < Tsn_min
                mask_warm = zTsn > Tmax

                if np.any(mask_cold):
                    print(f"cat={n+1} snow layer={k+1}: {np.count_nonzero(mask_cold)} cells "
                          f"below Tsn_min={Tsn_min}")
                    qsn = np.where(mask_cold, qsn_Tmin, qsn)

                if np.any(mask_warm):
                    print(f"cat={n+1} snow layer={k+1}: {np.count_nonzero(mask_warm)} cells "
                          f"above Tmax")
                    qsn = np.where(mask_warm, qsn_Tmax, qsn)

                qsnon[k,n,:,:]   = qsn
                vsnon_out[n,:,:] = vsn

        # Sea ice enthalpy.
        for n in range(ncat):
            ai_cat = aicen[n,:,:]
            vin    = vicen[n,:,:]
            vin    = np.where(vin < puny, puny, vin)
            for k in range(nilyr):
                iicen  = ilyr1[n]+k
                print(f"cat={n+1} ice layer={k+1}: " f"eicen index={iicen}")

                # Convert ice energy J/m2 --> J/m3.
                qin    = eicen[iicen,:,:] * nilyr / vin
                qin    = np.where(ai_cat <= puny, 0.0, qin)
                qicen[k,n,:,:] = qin

    return qsnon, qicen, vsnon_out

def sice_lr_BL99(klr, Ni, aicen, puny, Smax=3.2, a=0.407, b=0.573):
    """   
    Compute ice S in a single ice layer for all categories
    following the Bitz & Lipscomb (1999) formulation used in CICE4.
    aicen is a 3D array of ice partial areas by categories.
    """
    if klr > Ni: 
        raise Exception (f'klr {klr} cannot be > {Ni}')

    z       = (klr - 0.5) / Ni
    sice    = 0.5 * Smax * (1 - np.cos(np.pi * z**(a / (z + b))))
    sice_lr = np.where(aicen < puny, 0.0, sice)     
 
    return sice_lr


def main():
    fyaml = 'restart_cice6.yaml'
    parser = argparse.ArgumentParser()
    parser.add_argument("--fyaml",
        help=f"yaml file with params, default={fyaml}",
        default=fyaml)
    parser.add_argument("--machine", help="Machine ID (e.g. wcoss2, ursa)",
                        required=True, choices=["wcoss2", "ursa"])
    parser.add_argument("--rdate", help="Required restart date in CICE6: YYYYMMDD[hh], default hh=0",
                        required=True, type=int)
    parser.add_argument("--infile", help="Input CICE4 restart file name", required=True)
    parser.add_argument("--outfile", help="Output CICE6 restart file name (or output directory)", required=True)
    parser.add_argument("--tmpfile", help="Template CICE6 restart file name", required=True)
    args = parser.parse_args()

    # Determine paths based on machine ID cleanly
    if args.machine == "wcoss2":
        # RTOFS v2.5 production
        fgrdin4 = "/lfs/h1/ops/prod/packages/rtofs.v2.5.5/fix/rtofs_glo.navy_0.08.regional.cice.r"
        # UFS fix path
        fgrdin  = "/lfs/h2/emc/global/noscrub/emc.global/FIX/fix/cice/20240416/008/grid_cice_NEMS_mx008.nc"
    elif args.machine == "ursa":
        # RTOFS v2.5 copied from production on 09/26/2026
        fgrdin4 = "/scratch5/NCEPDEV/rstprod/Santha.Akella/data/rtofs/v2p5/fix/rtofs_glo.navy_0.08.regional.cice.r"
        # UFS fix path
        fgrdin  = "/scratch3/NCEPDEV/global/role.glopara/fix/cice/20240416/008/grid_cice_NEMS_mx008.nc"

    fyaml   = args.fyaml
    rdate6  = args.rdate
    infile  = args.infile
    outfile = args.outfile
    tmpfile = args.tmpfile

    dnmb6 = mc6rest.dateint2datenum(rdate6)
    YR6, MM6, DD6, HH6 = mc6rest.datevec(dnmb6, round_hrs=True)[:4]

    with open(fyaml) as ff:
        PATHS = safe_load(ff)

    fl_restart4 = infile
    fl_restartT = tmpfile

    # If outfile provided is a directory, append the standard filename
    if os.path.isdir(outfile) or outfile.endswith('/'):
        cicerst6 = f"rtofs_glo.{YR6}{MM6:02d}{DD6:02d}_{HH6:02d}000.restart_cice.nc"
        fl_restart6 = os.path.join(outfile, cicerst6)
    else:
        fl_restart6 = outfile

    os.makedirs(os.path.dirname(os.path.abspath(fl_restart6)), exist_ok=True)

    ice_grid4 = PATHS["cice_params"]["cice4"]["grid"]
    ice_grid6 = PATHS["cice_params"]["cice6"]["grid"]

    print(' \n===================================')
    print(f'Creating CICE6 restart for {YR6}/{MM6:02d}/{DD6:02d} {HH6:02d}hr UTC')
    print(f'Machine target:      {args.machine}')
    print(f'CICE4 restart:       {fl_restart4}')
    print(f'CICE6 template:      {fl_restartT}')
    print(f'New CICE6 restart:   {fl_restart6}')
    print(f'CICE4 grid:          {ice_grid4} ({fgrdin4})')
    print(f'CICE6 grid:          {ice_grid6} ({fgrdin})')
    print(' =================================== \n')

    # Sanity check of inputs
    if not os.path.exists(fl_restart4):
        raise FileNotFoundError(f"Does not exist: {fl_restart4}")

    if not os.path.isfile(fl_restartT):
        raise FileNotFoundError(f"CICE6 restart template not found {fl_restartT}")

    if not os.path.isfile(fgrdin4):
        raise FileNotFoundError(f"CICE4 grid file not found in fix directory: {fgrdin4}")

    if not os.path.isfile(fgrdin):
        raise FileNotFoundError(f"CICE6 grid file not found in fix directory: {fgrdin}")

    # Create object with CICE4 grid parameters.
    nx    = PATHS["cice_params"]["cice4"]["nx"]
    ny    = PATHS["cice_params"]["cice4"]["ny"]
    ncat  = PATHS["cice_params"]["cice4"]["ncat"]
    nilyr = PATHS["cice_params"]["cice4"]["nilyr"]
    nslyr = PATHS["cice_params"]["cice4"]["nslyr"]
    cice4 = CICE(nx, ny, ncat, nilyr, nslyr)

    # Create object with CICE6 grid parameters.
    nx    = PATHS["cice_params"]["cice6"]["nx"]
    ny    = PATHS["cice_params"]["cice6"]["ny"]
    ncat  = PATHS["cice_params"]["cice6"]["ncat"]
    nilyr = PATHS["cice_params"]["cice6"]["nilyr"]
    nslyr = PATHS["cice_params"]["cice6"]["nslyr"]
    cice6 = CICE(nx, ny, ncat, nilyr, nslyr)


    print(f'Reading restart: {fl_restart4}')
    with open(fl_restart4, 'rb') as fid:
        # Read the 1st sequential record of CICE4 restart file.
        recS    = np.fromfile(fid, dtype='>i4', count=1)[0]
        istep   = np.fromfile(fid, dtype='>i4', count=1)[0]
        runtime = np.fromfile(fid, dtype='>f8', count=1)[0]
        frtime  = np.fromfile(fid, dtype='>f8', count=1)[0]
        recE    = np.fromfile(fid, dtype='>i4', count=1)[0]

        if recS != recE:
          raise ValueError(f"Record length mismatch: {recS} != {recE}")

        print(
        f"Restart: step={istep}, "
        f"total time={runtime/(3600*24*365.25):.2f} yr, "
        f"forcing update={frtime/3600:.1f} hr ago"
        )

        nx     = cice4.nx
        ny     = cice4.ny
        ncat   = cice4.ncat
        ntilyr = cice4.ntilyr # total # of icelrs * cat
        ntslyr = cice4.ntslyr

        aicen = np.zeros((ncat,ny,nx), dtype='float64')
        vicen = np.zeros((ncat,ny,nx), dtype='float64')
        vsnon = np.zeros((ncat,ny,nx), dtype='float64')
        trcrn = np.zeros((ncat,ny,nx), dtype='float64')

        for n in range(ncat):
            print(f" Category {n+1}")
            for varname, arr in [
                ("ice area", aicen),
                ("ice vol",  vicen),
                ("snow vol", vsnon),
                ("surf T",   trcrn),
            ]:
                A = read_rest_cice4(fid, nx, ny)
                arr[n, :, :] = A
                print_minmax(varname, A)

        eicen = read_cice4_layers(fid, ntilyr, nx, ny, "eicen")
        esnon = read_cice4_layers(fid, ntslyr, nx, ny, "esnon")
        uvel  = read_cice4_2D(fid, nx, ny, 'uvel')
        vvel  = read_cice4_2D(fid, nx, ny, 'vvel')
        uvelE = None
        vvelN = None
        if ice_grid6 == 'C':
            uvelE, vvelN = mc6rest.interp_uvelE_vvelN(uvel, vvel, aicen) 

        scale_factor = read_cice4_2D(fid, nx, ny, 'scale factor')
        swvdr        = read_cice4_2D(fid, nx, ny, 'sh/wave vis direct')
        swvdf        = read_cice4_2D(fid, nx, ny, 'sh/wave vis diff')
        swidr        = read_cice4_2D(fid, nx, ny, 'sh/wave IR dir')
        swidf        = read_cice4_2D(fid, nx, ny, 'sh/wave IR diff')
        strocnxT     = read_cice4_2D(fid, nx, ny, 'ocean stress x-comp')
        strocnyT     = read_cice4_2D(fid, nx, ny, 'ocean stress y-comp')

        stressp = {}
        for fld in ["stressp_1", "stressp_3", "stressp_2", "stressp_4"]:
            stressp[fld] = read_cice4_2D(fid, nx, ny, fld)
        stressm = {}
        for fld in ["stressm_1", "stressm_3", "stressm_2", "stressm_4"]:
            stressm[fld] = read_cice4_2D(fid, nx, ny, fld)
        stress12 = {}
        for fld in ["stress12_1", "stress12_3", "stress12_2", "stress12_4"]:
            stress12[fld] = read_cice4_2D(fid, nx, ny, fld)

        iceumask = read_cice4_2D(fid, nx, ny, 'ice umask')             
        sst      = read_cice4_2D(fid, nx, ny, 'ocean mixed layer sst')
        frzmlt   = read_cice4_2D(fid, nx, ny, 'frzmlt')


    # Mask out land points:
    print(' Masking out fields ')
    maskval = 0.5 * spval
    for A in [
        aicen, vicen, vsnon, trcrn, eicen, esnon,
        uvel, vvel, scale_factor, swvdr, swvdf, swidr, swidf,
        strocnxT, strocnyT, sst, frzmlt
    ]:
        A[A > maskval] = 0.

    for A in stressp.values():
        A[A > maskval] = 0.

    for A in stressm.values():
        A[A > maskval] = 0.

    for A in stress12.values():
        A[A > maskval] = 0.

    # Read  CICE4 grid, Bu points.
    ulati4 = mc6rest.read_cice4_grid(fgrdin4, 'ulati', IDM=cice4.nx, JDM=cice4.ny)
    uloni4 = mc6rest.read_cice4_grid(fgrdin4, 'uloni', IDM=cice4.nx, JDM=cice4.ny)

    # Read CICE6 grid. 
    ulati6 = read_cice6_grid(fgrdin, 'ulat')
    uloni6 = read_cice6_grid(fgrdin, 'ulon')

    # Check grids in CICE4 and CICE6, expected to match.
    mc6rest.check_cice_grids(ulati4, uloni4, ulati6, uloni6)

    dnmb_new   = mc6rest.datenum([YR6, MM6, DD6, HH6])
    coszen_new = mc6rest.compute_coszen(ulati6, uloni6, dnmb_new, time_zone=0)

    # Make unknown fields 0, Level ice area and volume make 1 where aicen>0.
    fsnow = np.zeros((ny,nx), dtype=np.float64)
    iage  = np.zeros_like(aicen)
    apnd  = np.zeros_like(aicen)
    hpnd  = np.zeros_like(aicen)
    ipnd  = np.zeros_like(aicen)
    dhs   = np.zeros_like(aicen)
    ffrac = np.zeros_like(aicen)
    alvl = (aicen > 0.0).astype(np.float64)
    vlvl = (vicen > 0.0).astype(np.float64)

    # Ice and snow enthalpy derived from ice and snow energy in CICE4 layers.
    qsnon, qicen, vsnon_out =  energy_to_enthalpy(aicen, vicen, vsnon, eicen, esnon,
        cice4, rhos, Lfresh, cp_ice, puny, hs_min, Tsn_min)

    # Interpolate from CICE4 ice layers to layers in CICE6.
    if not cice6.nilyr == cice4.nilyr:
        qicen = mc6rest.remap_enthalpy_bins(qicen, cice4.nilyr, cice6.nilyr)

    # Snow enthaply interpolation
    if not cice6.nslyr == cice4.nslyr:
        qsnon = mc6rest.remap_enthalpy_bins(qsnon, cice4.nslyr, cice6.nslyr)


    # Collect updated fields with names and corresponding arrays
    updated_vars = {
    'uvel': uvel,
    'vvel': vvel,
    'scale_factor': scale_factor,
    'swvdr': swvdr,
    'swvdf': swvdf,
    'swidr': swidr,
    'swidf': swidf,
    'strocnxT': strocnxT,
    'strocnyT': strocnyT,
    'iceumask': iceumask,
    'fsnow': fsnow,
    'aicen': aicen,
    'vicen': vicen,
    'vsnon': vsnon,
    'iage': iage,
    'alvl': alvl,
    'vlvl': vlvl,
    'apnd': apnd,
    'hpnd': hpnd,
    'ipnd': ipnd,
    'dhs': dhs,
    'ffrac': ffrac,
    'Tsfcn': trcrn,
    'coszen': coszen_new,
    }

    # Add uvelE and vvelN only if they were calculated
    if uvelE is not None:
        updated_vars['uvelE'] = uvelE
    if vvelN is not None:
        updated_vars['vvelN'] = vvelN

    # Add all stress fields
    updated_vars.update(stressp)
    updated_vars.update(stressm)
    updated_vars.update(stress12)


    # =========================================================================
    # FAST WRITING: Copy OS file and modify in-place using netCDF4
    # =========================================================================
    print(' \n\n -------------')
    print('Creating CICE6 restart')

    print("Copying template restart to output location (OS level)...")
    t0 = time.time()
    shutil.copy(fl_restartT, fl_restart6)
    print(f"Copy completed in {time.time() - t0:.1f} seconds.")

    print("Modifying variables in-place using netCDF4...")
    # Open the newly copied file in Append/Modify mode
    with Dataset(fl_restart6, mode='r+') as nc:

        # 1. Update 2D/3D fields
        for varname, new_data in updated_vars.items():
            if varname in nc.variables:
                print(f'Updating {varname}')
                nc.variables[varname][:] = new_data
            else:
                print(f"WARNING: {varname} is not in {fl_restartT}")

        # 2. Update layered fields
        for ik in range(1, cice6.nilyr+1):
            print(f'Updating sice{ik:03d}')
            nc.variables[f'sice{ik:03d}'][:] = sice_lr_BL99(ik, cice6.nilyr, aicen, puny)
            print(f'Updating qice{ik:03d}')
            nc.variables[f'qice{ik:03d}'][:] = qicen[ik-1,:,:,:]

        for ik in range(1, cice6.nslyr+1):
            print(f'Updating qsno{ik:03d}')
            nc.variables[f'qsno{ik:03d}'][:] = qsnon[ik-1,:,:,:]

        # 3. Update global attributes
        print(
            f'Changing global attributes: restart time to '
            f'{YR6}/{MM6:02d}/{DD6:02d} {HH6*3600} sec'
        )
        nc.myear  = np.int32(YR6)
        nc.mmonth = np.int32(MM6)
        nc.mday   = np.int32(DD6)
        nc.msec   = np.int32(HH6 * 3600)
        nc.info1  = f"Restart created from CICE4: {os.path.basename(fl_restart4)} using https://github.com/NOAA-EMC/rtofs-tools"

    # Sanity check of output
    if not os.path.isfile(fl_restart6):
      raise Exception (f'ERR: CICE6 restart was NOT CREATED: {fl_restart6}')

    print(f'Created CICE6 restart: {fl_restart6}\n')

if __name__ == "__main__":
    main()
