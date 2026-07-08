#!/usr/bin/env python

#  Gemini was used to assist with developing this code.
# The code has been reviewed, edited, and validated by NWS staff.

import argparse
from datetime import datetime
import xarray as xr
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import xesmf
import numpy as np
import os

# --- grid handling ---

def create_regular_grid(hres=1, Rearth=6378e3, c_cont=True):
    """ create a regular grid of horizontal resolution hres """
    import xesmf
    import xarray as xr
    import numpy as np
    import regionmask

    ds = xesmf.util.grid_2d(0,360,hres,-90,90,hres)
    # compute cell area
    rfac = 2 * np.pi * Rearth / 360
    dx = rfac * hres * np.cos(2 * np.pi * ds["lat"] / 360)
    dy = rfac * hres
    ds["area"] = xr.DataArray(data=dx*dy, dims=("y","x"))
    ds["mask"] = regionmask.defined_regions.natural_earth_v5_0_0.land_110.mask(ds["lon"], ds["lat"]).fillna(1.)
    
    if c_cont:
        out = xr.Dataset()
        # Force C-contiguous order so xESMF's internal transpose makes it F-contiguous
        out['lon'] = (ds['lon'].dims, np.ascontiguousarray(ds['lon'].values))
        out['lat'] = (ds['lat'].dims, np.ascontiguousarray(ds['lat'].values))
        out['lon_b'] = (ds['lon_b'].dims, np.ascontiguousarray(ds['lon_b'].values))
        out['lat_b'] = (ds['lat_b'].dims, np.ascontiguousarray(ds['lat_b'].values))
        out['area'] = (ds['area'].dims, np.ascontiguousarray(ds['area'].values))
        out['mask'] = (ds['mask'].dims, np.ascontiguousarray(ds['mask'].values))
        return out
        
    return ds


def create_grid_OSTIA(ostia_file, hres=0.05, c_cont=True):
    """ create a xesmf friendly grid for OSTIA """
    import xarray as xr
    import numpy as np

    ostia = xr.open_dataset(ostia_file)
    ds = xr.Dataset()
    ds["lon"] = xr.DataArray(data=ostia["lon"].values, dims=("x"))
    ds["lat"] = xr.DataArray(data=ostia["lat"].values, dims=("y"))
    ds["lon_b"] = xr.DataArray(data=np.arange(-180,180+hres,hres), dims=("x_b"))
    ds["lat_b"] = xr.DataArray(data=np.arange(-90,90+hres,hres), dims=("y_b"))
    # 1 = water, 2 = land, 6 = optional_lake_surface, 9 = sea_ice, 14 = optional_river_surface
    mask = xr.where(ostia["mask"].squeeze().isin([1, 9]), 1, 0)
    ds["mask"] = xr.DataArray(data=mask.values, dims=("y", "x"))

    assert ds["lon_b"].shape[0] == ds["lon"].shape[0] +1
    assert ds["lat_b"].shape[0] == ds["lat"].shape[0] +1
    if c_cont:
        out = xr.Dataset()
        out['lon'] = (ds['lon'].dims, np.ascontiguousarray(ds['lon'].values))
        out['lat'] = (ds['lat'].dims, np.ascontiguousarray(ds['lat'].values))
        out['lon_b'] = (ds['lon_b'].dims, np.ascontiguousarray(ds['lon_b'].values))
        out['lat_b'] = (ds['lat_b'].dims, np.ascontiguousarray(ds['lat_b'].values))
        out['mask'] = (ds['mask'].dims, np.ascontiguousarray(ds['mask'].values))
        return out

    return ds


def create_grid_RTOFS(hgrid_path, sst, c_cont=True):
    """ get coords from hgrid, mask derived from SST to allow different lsm """
    import xarray as xr
    import numpy as np

    hgrid = xr.open_dataset(hgrid_path)
    ds = xr.Dataset()
    ds["lon"] = xr.DataArray(data=hgrid["x"].values[1::2,1::2], dims=("y", "x"))
    ds["lat"] = xr.DataArray(data=hgrid["y"].values[1::2,1::2], dims=("y", "x"))
    ds["lon_b"] = xr.DataArray(data=hgrid["x"].values[0::2,0::2], dims=("y_b", "x_b"))
    ds["lat_b"] = xr.DataArray(data=hgrid["y"].values[0::2,0::2],dims=("y_b", "x_b"))
    mask = xr.where(sst.fillna(-9999.) == -9999., 0, 1).values
    ds["mask"] = xr.DataArray(data=mask, dims=("y", "x"))
    if c_cont:
        out = xr.Dataset()
        out['lon'] = (ds['lon'].dims, np.ascontiguousarray(ds['lon'].values))
        out['lat'] = (ds['lat'].dims, np.ascontiguousarray(ds['lat'].values))
        out['lon_b'] = (ds['lon_b'].dims, np.ascontiguousarray(ds['lon_b'].values))
        out['lat_b'] = (ds['lat_b'].dims, np.ascontiguousarray(ds['lat_b'].values))
        out['mask'] = (ds['mask'].dims, np.ascontiguousarray(ds['mask'].values))
        return out
    return ds

