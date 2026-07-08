#!/usr/bin/env python

#  Gemini was used to assist with developing this code.
# The code has been reviewed, edited, and validated by NWS staff.

import argparse
from datetime import datetime, timedelta
import xarray as xr
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import xesmf
import numpy as np
import os

from stats import *

def plot_glo_sst_obs_bias_3rows(grid, obs, ref, dev, titles=["OSTIA", "RTOFS 2.5", "RTOFS 3"], date=None, offset=0):
    """ A 3 panel plots with obs and bias in reference and dev runs """
    import matplotlib
    matplotlib.rcParams.update({"font.size":20})

    # set up the plot
    f, axs = plt.subplots(nrows=3, figsize=[12,24],
                          subplot_kw=dict(projection=ccrs.Robinson(central_longitude=200)))
    # plot the various fields
    CN = plot_glo_sst( axs[0], grid["lon"], grid["lat"], obs)
    CD = plot_glo_bias(axs[1], grid["lon"], grid["lat"], ref - obs, grid["area"])
    _  = plot_glo_bias(axs[2], grid["lon"], grid["lat"], dev - obs, grid["area"])
    # add colorbars
    add_colorbar_obs(f, CN)
    add_colorbar_bias(f, CD)
    # cosmetics
    for k in range(3):
        add_land_coastlines(axs[k])
    add_titles_3rows(axs, titles=titles, date=date, offset=offset)
    return f


def add_colorbar_obs(fig, C, field="SST", units="degC"):
    """ add colorbar for observations """
    #cax = fig.add_axes([0.78, 0.655, 0.04, 0.225])
    cax = fig.add_axes([0.95, 0.655, 0.04, 0.225])
    fig.colorbar(C, cax=cax, label=f"{field} [{units}]")


def add_colorbar_bias(fig, C, field="SST", units="degC"):
    """ add colorbar for observations """
    #cax = fig.add_axes([0.78, 0.11, 0.04, 0.5])
    cax = fig.add_axes([0.95, 0.11, 0.04, 0.5])
    fig.colorbar(C, cax=cax, label=f"{field} bias [{units}]")


def add_titles_3rows(axs, titles=["obs", "ref", "dev"], date=None, offset=0):
    """ just add titles """

    date_offset = date + timedelta(days=offset)
    cdate = datetime.strftime(date_offset, "%Y-%m-%d")
    axs[0].set_title(f"{titles[0]} - {cdate}")
    if offset < 0:
        axs[1].set_title(f"{titles[1]} --- nowcast t -{offset} days")
        axs[2].set_title(f"{titles[2]} --- nowcast t -{offset} days")
    elif offset == 0:
        axs[1].set_title(f"{titles[1]} --- nowcast t + {offset} days")
        axs[2].set_title(f"{titles[2]} --- nowcast t + {offset} days")
    else: # offset > 0:
        axs[1].set_title(f"{titles[1]} --- forecast t + {offset} days")
        axs[2].set_title(f"{titles[2]} --- forecast t + {offset} days")


def plot_glo_sst(ax, lon, lat, array, vmin=-2, vmax=35, cmap="gist_ncar"):
    C = ax.pcolormesh(lon, lat, array,
                      vmin=vmin, vmax=vmax, cmap=cmap,
                      transform=ccrs.PlateCarree())
    return C


def plot_glo_bias(ax, lon, lat, array, area, vmin=-2, vmax=2, cmap="RdBu_r"):
    """ make the difference plot """
    C = ax.pcolormesh(lon, lat, array,
                      vmin=vmin, vmax=vmax, cmap=cmap,
                      transform=ccrs.PlateCarree())
    stats = basic_stats_region(array, area, mask_region=None, debug=False)
    add_stats(ax, stats)
    return C


def add_land_coastlines(ax):
    """ add cartopy land and coastlines to subplots """
    ax.add_feature(cfeature.LAND, color="grey")
    ax.coastlines()
    return None

def add_stats(ax, stats):
    """ add text box with stats """
    import matplotlib.pyplot as plt

    text_str = (
    f"min = {stats['min']:0.2f}\nmax = {stats['max']:0.2f}\n"
    f"mean = {stats['mean']:0.2f}\nstd = {stats['std']:0.2f}\n"
    f"rms = {stats['rms']:0.2f}"
    )

    props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
    ax.text(-0.15, 0.05, text_str, transform=ax.transAxes, fontsize=18,
            verticalalignment='center', horizontalalignment='left',
            bbox=props, family='monospace')

    return None

