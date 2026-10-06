#!/usr/bin/env python

import os
import xarray as xr
from glob import glob
from grids import create_grid_OSTIA, create_regular_grid, create_grid_CMEMS, create_grid_CMEMS_SSH
import xesmf

# -- regular grid

grid_1x1 = create_regular_grid()

# -----------------------------------------------------------------------------------------------
# -- OSTIA SST

dir_ostia = "/scratch3/NCEPDEV/marine/Raphael.Dussin/obs/gridded/OSTIA_reprocessed"

flist = glob(f"{dir_ostia}/2020*-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB_REP-v02.0-fv02.0.nc")
flist += glob(f"{dir_ostia}/2021*-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB_REP-v02.0-fv02.0.nc")

ostia = xr.open_mfdataset(flist)

# monthly means
ostia_1m = ostia.resample(time="1MS").mean()

# remap to 1x1deg
grid_ostia = create_grid_OSTIA(f"{dir_ostia}/20251218120000-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB_REP-v02.0-fv02.0.nc")

if os.path.exists("esmf_wgts_ostia_1x1.nc"):
    remap_ostia = xesmf.Regridder(
        grid_ostia,
        grid_1x1,
        "conservative_normed",
        periodic=True,
        weights="esmf_wgts_ostia_1x1.nc",
    )

remapped = remap_ostia(ostia_1m)

remapped.to_netcdf("sst_ostia_1m_2020-2021.nc")

# -----------------------------------------------------------------------------------------------
# -- CMEMS SSS

dir_cmems = "/scratch3/NCEPDEV/marine/Raphael.Dussin/obs/gridded/SSS_CMEMS/my"

flist = glob(f"{dir_cmems}/dataset-sss-ssd-rep-daily_2020*nc")
flist += glob(f"{dir_cmems}/dataset-sss-ssd-rep-daily_2021*nc")

cmems = xr.open_mfdataset(flist)

# monthly means
cmems_1m = cmems.resample(time="1MS").mean()

# remap to 1x1deg
grid_cmems = create_grid_CMEMS(f"{dir_cmems}/dataset-sss-ssd-rep-daily_20201231T1200Z_P20241017T0000Z.nc")

remap_cmems = xesmf.Regridder(
    grid_cmems,
    grid_1x1,
    "conservative_normed",
    periodic=True,
    )

remapped = remap_cmems(cmems_1m)

remapped.to_netcdf("sss_cmems_1m_2020-2021.nc")

# -----------------------------------------------------------------------------------------------
# -- CMEMS SSH

dir_cmems_ssh = "/scratch3/NCEPDEV/marine/Raphael.Dussin/obs/gridded/SSH_CMEMS/my"

flist = glob(f"{dir_cmems_ssh}/dt_global_allsat_phy_l4_2020*nc")
flist += glob(f"{dir_cmems_ssh}/dt_global_allsat_phy_l4_2021*nc")

cmems_ssh = xr.open_mfdataset(flist)

# monthly means
cmems_ssh_1m = cmems_ssh.resample(time="1MS").mean()

# remap to 1x1deg
grid_cmems_ssh = create_grid_CMEMS_SSH(f"{dir_cmems_ssh}/dt_global_allsat_phy_l4_20201231_20241017.nc")

if os.path.exists("esmf_wgts_cmems_ssh_1x1.nc"):
    remap_cmems_ssh = xesmf.Regridder(
        grid_cmems_ssh,
        grid_1x1,
        "conservative_normed",
        periodic=True,
        weights="esmf_wgts_cmems_ssh_1x1.nc",
    )

remapped = remap_cmems_ssh(cmems_ssh_1m.drop_vars(["crs", "lat_bnds", "lon_bnds"]))

remapped.to_netcdf("ssh_cmems_1m_2020-2021.nc")
