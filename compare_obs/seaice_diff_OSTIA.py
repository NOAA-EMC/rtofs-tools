#!/usr/bin/env python

#  Gemini was used to assist with developing this code.
# The code has been reviewed, edited, and validated by NWS staff.

import argparse
from datetime import datetime, timedelta
import os
from glob import glob
import warnings
import xarray as xr
import xesmf

from grids import create_grid_OSTIA, create_grid_RTOFS, create_regular_grid
from plotting import plot_polar_seaice_diff
from utils import str2bool, valid_date

warnings.filterwarnings("ignore", module="xesmf")


def read_OSTIA_seaice(args, var="sea_ice_fraction"):
    """Read sea-ice fraction from OSTIA observations.

    Parameters
    ----------
    args : argparse.Namespace
        Parsed command-line arguments containing the base date ('date'),
        observations directory ('obs'), and forecast/nowcast offset ('days_offset').
    var : str, optional
        The variable name to extract from the OSTIA file. Default is "sea_ice_fraction".

    Returns
    -------
    xarray.DataArray
        The 2D squeezed and offset-adjusted OSTIA SST data array.
    """
    # change obs date based on forecast/nowcast offset
    date_offset = args.date + timedelta(days=args.days_offset)
    cdate = datetime.strftime(date_offset, "%Y%m%d")
    ncfile = (
        f"{args.obs}/{cdate}120000-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB-v02.0-fv02.0.nc"
    )
    print(f"reading {var} from {ncfile}")
    out = xr.open_dataset(ncfile)[var].squeeze()
    return out


def read_HYCOM_seaice(args, var="ice_coverage"):
    """Read Ice coverage from HYCOM RTOFS output.

    Parameters
    ----------
    args : argparse.Namespace
        Parsed command-line arguments containing the reference model directory
        ('ref_model'), the base date ('date'), and the forecast/nowcast offset
        ('days_offset' or 'use_forecast_t0').
    var : str, optional
        The variable name to extract from the HYCOM file. Default is "ice_coverage".

    Returns
    -------
    xarray.DataArray
        The 2D HYCOM ice data array (with the extra boundary row removed).
    """
    cdate = datetime.strftime(args.date, "%Y%m%d")
    days_offset = args.days_offset
    if (days_offset > 0.0) or args.use_forecast_t0:  # is forecast
        tplus = 24 * days_offset
        ncfile = (
            f"{args.ref_model}/rtofs.{cdate}/rtofs_glo_2ds_f{tplus:03g}_ice.nc"
        )
    else:
        if days_offset < -1:
            raise ValueError("days offset cannot be less than -1 day")
        tminus = 24 * (1 + days_offset)
        ncfile = (
            f"{args.ref_model}/rtofs.{cdate}/rtofs_glo_2ds_n{tminus:03g}_ice.nc"
        )
    print(f"reading {var} from {ncfile}")
    out = xr.open_dataset(ncfile)[var].squeeze()[:-1, :]  # hycom has one more row
    return out


def read_MOM_seaice(args, var="ice_coverage"):
    """Read seaice coverage from MOM RTOFS output.

    Parameters
    ----------
    args : argparse.Namespace
        Parsed command-line arguments containing the dev model directory
        ('dev_model'), the base date ('date'), and the forecast/nowcast offset
        ('days_offset').
    var : str, optional
        The variable name to extract from the MOM file. Default is "ice_coverage".

    Returns
    -------
    xarray.DataArray
        The 2D MOM ice data array.
    """
    # days offset can be non-integer values
    cdate = datetime.strftime(args.date, "%Y%m%d")
    days_offset = args.days_offset
    if days_offset > 0.0:  # is forecast
        tplus = 24 * days_offset
        ncfile = (
            f"{args.dev_model}/rtofs.{cdate}/rtofs_glo_2ds.f{tplus:03g}.ice.nc"
        )
    else:  # days_offset <=0 is nowcast
        tminus = -1 * 24 * days_offset
        ncfile = (
            f"{args.dev_model}/rtofs.{cdate}/rtofs_glo_2ds.tm{tminus:03g}.ice.nc"
        )
    print(f"reading {var} from {ncfile}")
    out = xr.open_dataset(ncfile)[var].squeeze()
    return out


# --- main
def main():
    """Execute the main workflow to compute and plot RTOFS Seaice differences.

    Parses command-line arguments, reads observation and model grids,
    remaps seaice fields onto a regular 1x1 degree grid using xESMF (reusing or
    generating remapping weights), computes biases, and plots comparison results.
    """
    # Initialize the parser
    parser = argparse.ArgumentParser(
        description="Script to plot RTOFS SeaIce concentration against OSTIA."
    )

    # Required arguments
    parser.add_argument(
        "--ref_model", required=True, help="Path to the reference model."
    )
    parser.add_argument(
        "--dev_model", required=True, help="Path to the dev model."
    )
    parser.add_argument(
        "--obs", required=True, help="Path to the observations."
    )
    parser.add_argument(
        "--date",
        required=True,
        type=valid_date,
        help="Date to process (format: YYYY-MM-DD).",
    )
    parser.add_argument(
        "--days_offset",
        required=False,
        type=int,
        default=0,
        help="Add days for nowcast (down to -1) /forecast (up to 4). (default: 0)",
    )

    # Optional arguments
    parser.add_argument(
        "--grid",
        required=False,
        default=None,
        help="(Optional) Path to the model grid.",
    )

    parser.add_argument(
        "--ref_model_is_hycom",
        type=str2bool,
        required=False,
        default=True,
        help="(Optional) Is the reference model HYCOM? Accepts true/false. (default: true)",
    )

    parser.add_argument(
        "--use_forecast_t0",
        type=str2bool,
        required=False,
        default=False,
        help="(Optional) Use output from forecast for t = 0 (default: false). Only valid for RTOFS 2.X",
    )

    # Parse the arguments
    args = parser.parse_args()

    # Print out the variables to verify
    print("--- Parsed Arguments ---")
    print(f"Reference Model : {args.ref_model}")
    print(f"Dev Model       : {args.dev_model}")
    print(f"Observations    : {args.obs}")
    print(f"Date            : {args.date}")
    print(f"Offset in days  : {args.days_offset}")
    print(f"Ref Model is HYCOM  : {args.ref_model_is_hycom}")
    print(f"Use t0 from forecast (HYCOM only)  : {args.use_forecast_t0}")

    if args.grid:
        print(f"Model Grid      : {args.grid}")
    else:
        print("Model Grid      : Not provided")

    seaice_ostia = read_OSTIA_seaice(args)
    if args.ref_model_is_hycom:
        icecov_ref = read_HYCOM_seaice(args)
    icecov_dev = read_MOM_seaice(args)

    grid_1x1 = create_regular_grid()
    print("reg grid created")

    cdate = datetime.strftime(args.date, "%Y%m%d")
    grid_ostia = create_grid_OSTIA(
        f"{args.obs}/{cdate}120000-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB-v02.0-fv02.0.nc"
    )
    print("OSTIA grid created")

    grid_rtofs_ref = create_grid_RTOFS(args.grid, icecov_ref)
    print("RTOFS grid created")
    grid_rtofs_dev = create_grid_RTOFS(args.grid, icecov_ref)
    print("RTOFS grid created")

    # we should save weights offline
    if os.path.exists("esmf_wgts_ostia_1x1.nc"):
        remap_ostia = xesmf.Regridder(
            grid_ostia,
            grid_1x1,
            "conservative_normed",
            periodic=True,
            weights="esmf_wgts_ostia_1x1.nc",
        )
    else:
        print("remapping OSTIA, this may takes some time...")
        remap_ostia = xesmf.Regridder(
            grid_ostia, grid_1x1, "conservative_normed", periodic=True
        )
        remap_ostia.to_netcdf("esmf_wgts_ostia_1x1.nc")

    seaice_ostia_1x1deg = remap_ostia(seaice_ostia)

    if os.path.exists("esmf_wgts_rtofs_ref_1x1.nc"):
        remap_rtofs_ref = xesmf.Regridder(
            grid_rtofs_ref,
            grid_1x1,
            "conservative_normed",
            periodic=True,
            weights="esmf_wgts_rtofs_ref_1x1.nc",
        )
    else:
        print("remapping RTOFS ref, this may takes some time...")
        remap_rtofs_ref = xesmf.Regridder(
            grid_rtofs_ref, grid_1x1, "conservative_normed", periodic=True
        )
        remap_rtofs_ref.to_netcdf("esmf_wgts_rtofs_ref_1x1.nc")

    rtofs_ref_1x1 = remap_rtofs_ref(icecov_ref)

    if os.path.exists("esmf_wgts_rtofs_dev_1x1.nc"):
        remap_rtofs_dev = xesmf.Regridder(
            grid_rtofs_dev,
            grid_1x1,
            "conservative_normed",
            periodic=True,
            weights="esmf_wgts_rtofs_dev_1x1.nc",
        )
    else:
        print("remapping RTOFS dev, this may takes some time...")
        remap_rtofs_dev = xesmf.Regridder(
            grid_rtofs_dev, grid_1x1, "conservative_normed", periodic=True
        )
        remap_rtofs_dev.to_netcdf("esmf_wgts_rtofs_dev_1x1.nc")

    rtofs_dev_1x1 = remap_rtofs_dev(icecov_dev)

    fig = plot_polar_seaice_diff(grid_1x1, rtofs_ref_1x1, rtofs_dev_1x1, seaice_ostia_1x1deg, date=args.date, offset=args.days_offset)

    # change date and plot title to match nowcast/forecast
    if args.days_offset < 0:
        fout = (
            f"compare_seaice_{args.date}_tminus{-1 * args.days_offset:02g}days.png"
        )
    else:
        fout = f"compare_seaice_{args.date}_tplus{args.days_offset:02g}days.png"
    fig.savefig(fout, bbox_inches="tight", dpi=50)


if __name__ == "__main__":
    main()


# example:
#python seaice_diff_satelite.py --ref_model /scratch5/NCEPDEV/rstprod/Santha.Akella/data/rtofs/v2p5/output --dev_model /scratch5/NCEPDEV/rstprod/Santha.Akella/data/rtofs/v3_exp/May2026 --obs /scratch3/NCEPDEV/marine/Raphael.Dussin/obs/gridded/OSTIA_near_realtime --date 2025-12-21 --ref_model_is_hycom yes --grid /scratch3/NCEPDEV/global/role.glopara/fix/mom6/20250128/008/ocean_hgrid.nc --days_offset 0

