#!/usr/bin/env python

#  Gemini was used to assist with developing this code.
# The code has been reviewed, edited, and validated by NWS staff.

import numpy as np
import regionmask
import xarray as xr
import xesmf

__all__ = ["create_regular_grid", "create_grid_OSTIA", "create_grid_RTOFS"]


# --- grid handling ---


def create_regular_grid(hres=1, Rearth=6378e3, c_cont=True):
    """Create a regular 2D latitude-longitude grid with a land/sea mask and cell area.

    Parameters
    ----------
    hres : float, optional
        Horizontal resolution of the grid in degrees. Default is 1.
    Rearth : float, optional
        Radius of the Earth in meters. Default is 6378e3.
    c_cont : bool, optional
        If True, returns the dataset variables as C-contiguous numpy arrays.
        This is useful for compatibility with xESMF's internal Fortran ordering.
        Default is True.

    Returns
    -------
    xarray.Dataset
        A dataset containing the grid variables:
        - 'lon', 'lat': 2D arrays of cell centers (degrees).
        - 'lon_b', 'lat_b': 2D arrays of cell boundaries (degrees).
        - 'area': 2D array of cell area in square meters.
        - 'mask': 2D land/sea mask.
    """
    ds = xesmf.util.grid_2d(0, 360, hres, -90, 90, hres)
    # compute cell area
    rfac = 2 * np.pi * Rearth / 360
    dx = rfac * hres * np.cos(2 * np.pi * ds["lat"] / 360)
    dy = rfac * hres
    ds["area"] = xr.DataArray(data=dx * dy, dims=("y", "x"))
    ds["mask"] = (
        regionmask.defined_regions.natural_earth_v5_0_0.land_110.mask(
            ds["lon"], ds["lat"]
        ).fillna(1.0)
    )

    if c_cont:
        out = xr.Dataset()
        # Force C-contiguous order so xESMF's internal transpose makes it F-contiguous
        out["lon"] = (ds["lon"].dims, np.ascontiguousarray(ds["lon"].values))
        out["lat"] = (ds["lat"].dims, np.ascontiguousarray(ds["lat"].values))
        out["lon_b"] = (ds["lon_b"].dims, np.ascontiguousarray(ds["lon_b"].values))
        out["lat_b"] = (ds["lat_b"].dims, np.ascontiguousarray(ds["lat_b"].values))
        out["area"] = (ds["area"].dims, np.ascontiguousarray(ds["area"].values))
        out["mask"] = (ds["mask"].dims, np.ascontiguousarray(ds["mask"].values))
        return out

    return ds


def create_grid_OSTIA(ostia_file, hres=0.05, c_cont=True):
    """Create a grid dataset compatible with xESMF from an OSTIA file.

    Extracts longitude and latitude coordinates from the OSTIA dataset,
    computes grid boundary coordinates, and constructs a land/ocean mask where
    water (1) and sea ice (9) are mapped to 1, and other categories are mapped to 0.

    Parameters
    ----------
    ostia_file : str or Path
        Path to the OSTIA netCDF file.
    hres : float, optional
        Horizontal resolution of the OSTIA grid in degrees. Default is 0.05.
    c_cont : bool, optional
        If True, returns the dataset variables as C-contiguous numpy arrays.
        Default is True.

    Returns
    -------
    xarray.Dataset
        A dataset containing the OSTIA grid variables:
        - 'lon', 'lat': 1D arrays of cell centers (degrees).
        - 'lon_b', 'lat_b': 1D arrays of cell boundaries (degrees).
        - 'mask': 2D land/ocean mask (1 for ocean/sea-ice, 0 otherwise).
    """
    ostia = xr.open_dataset(ostia_file)
    ds = xr.Dataset()
    ds["lon"] = xr.DataArray(data=ostia["lon"].values, dims=("x"))
    ds["lat"] = xr.DataArray(data=ostia["lat"].values, dims=("y"))
    ds["lon_b"] = xr.DataArray(
        data=np.arange(-180, 180 + hres, hres), dims=("x_b")
    )
    ds["lat_b"] = xr.DataArray(
        data=np.arange(-90, 90 + hres, hres), dims=("y_b")
    )
    # 1 = water, 2 = land, 6 = optional_lake_surface, 9 = sea_ice, 14 = optional_river_surface
    mask = xr.where(ostia["mask"].squeeze().isin([1, 9]), 1, 0)
    ds["mask"] = xr.DataArray(data=mask.values, dims=("y", "x"))

    assert ds["lon_b"].shape[0] == ds["lon"].shape[0] + 1
    assert ds["lat_b"].shape[0] == ds["lat"].shape[0] + 1
    if c_cont:
        out = xr.Dataset()
        out["lon"] = (ds["lon"].dims, np.ascontiguousarray(ds["lon"].values))
        out["lat"] = (ds["lat"].dims, np.ascontiguousarray(ds["lat"].values))
        out["lon_b"] = (ds["lon_b"].dims, np.ascontiguousarray(ds["lon_b"].values))
        out["lat_b"] = (ds["lat_b"].dims, np.ascontiguousarray(ds["lat_b"].values))
        out["mask"] = (ds["mask"].dims, np.ascontiguousarray(ds["mask"].values))
        return out

    return ds


def create_grid_CMEMS(cmems_file, hres=0.125, c_cont=True):
    """Create a grid dataset compatible with xESMF from an CMEMS file.

    Extracts longitude and latitude coordinates from the CMEMS dataset,
    computes grid boundary coordinates, and constructs a land/ocean mask where
    water (1) is mapped to 1, and land is mapped to 0.

    Parameters
    ----------
    cmems_file : str or Path
        Path to the CMEMS netCDF file.
    hres : float, optional
        Horizontal resolution of the CMEMS grid in degrees. Default is 0.125.
    c_cont : bool, optional
        If True, returns the dataset variables as C-contiguous numpy arrays.
        Default is True.

    Returns
    -------
    xarray.Dataset
        A dataset containing the CMEMS grid variables:
        - 'lon', 'lat': 1D arrays of cell centers (degrees).
        - 'lon_b', 'lat_b': 1D arrays of cell boundaries (degrees).
        - 'mask': 2D land/ocean mask (1 for ocean/sea-ice, 0 otherwise).
    """
    cmems = xr.open_dataset(cmems_file)
    ds = xr.Dataset()
    ds["lon"] = xr.DataArray(data=cmems["lon"].values, dims=("x"))
    ds["lat"] = xr.DataArray(data=cmems["lat"].values, dims=("y"))
    # my and nrt are not consistent in longitude definition...
    if ds["lon"][0] >= 0.:
        ds["lon_b"] = xr.DataArray(
            data=np.arange(0, 360 + hres, hres), dims=("x_b")
        )
    else:
        ds["lon_b"] = xr.DataArray(
            data=np.arange(-180, 180 + hres, hres), dims=("x_b")
        )
    ds["lat_b"] = xr.DataArray(
        data=np.arange(-90, 90 + hres, hres), dims=("y_b")
    )
    mask = xr.where(cmems["sos"].squeeze().fillna(-9999.) == -9999., 0, 1)
    ds["mask"] = xr.DataArray(data=mask.values, dims=("y", "x"))

    assert ds["lon_b"].shape[0] == ds["lon"].shape[0] + 1
    assert ds["lat_b"].shape[0] == ds["lat"].shape[0] + 1
    if c_cont:
        out = xr.Dataset()
        out["lon"] = (ds["lon"].dims, np.ascontiguousarray(ds["lon"].values))
        out["lat"] = (ds["lat"].dims, np.ascontiguousarray(ds["lat"].values))
        out["lon_b"] = (ds["lon_b"].dims, np.ascontiguousarray(ds["lon_b"].values))
        out["lat_b"] = (ds["lat_b"].dims, np.ascontiguousarray(ds["lat_b"].values))
        out["mask"] = (ds["mask"].dims, np.ascontiguousarray(ds["mask"].values))
        return out

    return ds


def create_grid_CMEMS_SSH(cmems_file, hres=0.125, c_cont=True):
    """Create a grid dataset compatible with xESMF from an CMEMS SSH file.

    Extracts longitude and latitude coordinates from the CMEMS dataset,
    computes grid boundary coordinates, and constructs a land/ocean mask where
    water (1) is mapped to 1, and land is mapped to 0.

    Parameters
    ----------
    cmems_file : str or Path
        Path to the CMEMS netCDF file.
    hres : float, optional
        Horizontal resolution of the CMEMS grid in degrees. Default is 0.125.
    c_cont : bool, optional
        If True, returns the dataset variables as C-contiguous numpy arrays.
        Default is True.

    Returns
    -------
    xarray.Dataset
        A dataset containing the CMEMS grid variables:
        - 'lon', 'lat': 1D arrays of cell centers (degrees).
        - 'lon_b', 'lat_b': 1D arrays of cell boundaries (degrees).
        - 'mask': 2D land/ocean mask (1 for ocean/sea-ice, 0 otherwise).
    """
    cmems = xr.open_dataset(cmems_file)
    ds = xr.Dataset()
    ds["lon"] = xr.DataArray(data=cmems["longitude"].values, dims=("x"))
    ds["lat"] = xr.DataArray(data=cmems["latitude"].values, dims=("y"))

    lon_b = np.concatenate([cmems["lon_bnds"].isel(nv=0).values, [cmems["lon_bnds"].isel(nv=1)[-1].values]], axis=0)
    lat_b = np.concatenate([cmems["lat_bnds"].isel(nv=0).values, [cmems["lat_bnds"].isel(nv=1)[-1].values]], axis=0)

    ds["lon_b"] = xr.DataArray(lon_b, dims=("x_b"))
    ds["lat_b"] = xr.DataArray(lat_b, dims=("y_b"))
    mask = xr.where(cmems["adt"].squeeze().fillna(-9999.) == -9999., 0, 1)
    ds["mask"] = xr.DataArray(data=mask.values, dims=("y", "x"))

    assert ds["lon_b"].shape[0] == ds["lon"].shape[0] + 1
    assert ds["lat_b"].shape[0] == ds["lat"].shape[0] + 1
    if c_cont:
        out = xr.Dataset()
        out["lon"] = (ds["lon"].dims, np.ascontiguousarray(ds["lon"].values))
        out["lat"] = (ds["lat"].dims, np.ascontiguousarray(ds["lat"].values))
        out["lon_b"] = (ds["lon_b"].dims, np.ascontiguousarray(ds["lon_b"].values))
        out["lat_b"] = (ds["lat_b"].dims, np.ascontiguousarray(ds["lat_b"].values))
        out["mask"] = (ds["mask"].dims, np.ascontiguousarray(ds["mask"].values))
        return out

    return ds


def create_grid_RTOFS(hgrid_path, sst, c_cont=True):
    """Create a grid dataset compatible with xESMF from RTOFS hgrid and SST data.

    Extracts longitude and latitude cell centers (using odd indices from the RTOFS
    supergrid) and boundaries (using even indices) from the horizontal grid file.
    The land/ocean mask is derived from the provided SST data, where non-missing
    values are mapped to 1 and missing/NaN values are mapped to 0.

    Parameters
    ----------
    hgrid_path : str or Path
        Path to the RTOFS horizontal grid (hgrid) netCDF file.
    sst : xarray.DataArray
        Sea surface temperature data array used to derive the land/ocean mask.
    c_cont : bool, optional
        If True, returns the dataset variables as C-contiguous numpy arrays.
        Default is True.

    Returns
    -------
    xarray.Dataset
        A dataset containing the RTOFS grid variables:
        - 'lon', 'lat': 2D arrays of cell centers (degrees).
        - 'lon_b', 'lat_b': 2D arrays of cell boundaries (degrees).
        - 'mask': 2D land/ocean mask derived from SST (1 for ocean, 0 for land).
    """
    hgrid = xr.open_dataset(hgrid_path)
    ds = xr.Dataset()
    ds["lon"] = xr.DataArray(
        data=hgrid["x"].values[1::2, 1::2], dims=("y", "x")
    )
    ds["lat"] = xr.DataArray(
        data=hgrid["y"].values[1::2, 1::2], dims=("y", "x")
    )
    ds["lon_b"] = xr.DataArray(
        data=hgrid["x"].values[0::2, 0::2], dims=("y_b", "x_b")
    )
    ds["lat_b"] = xr.DataArray(
        data=hgrid["y"].values[0::2, 0::2], dims=("y_b", "x_b")
    )
    mask = xr.where(sst.fillna(-9999.0) == -9999.0, 0, 1).values
    ds["mask"] = xr.DataArray(data=mask, dims=("y", "x"))
    if c_cont:
        out = xr.Dataset()
        out["lon"] = (ds["lon"].dims, np.ascontiguousarray(ds["lon"].values))
        out["lat"] = (ds["lat"].dims, np.ascontiguousarray(ds["lat"].values))
        out["lon_b"] = (ds["lon_b"].dims, np.ascontiguousarray(ds["lon_b"].values))
        out["lat_b"] = (ds["lat_b"].dims, np.ascontiguousarray(ds["lat_b"].values))
        out["mask"] = (ds["mask"].dims, np.ascontiguousarray(ds["mask"].values))
        return out
    return ds
