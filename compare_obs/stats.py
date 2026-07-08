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

# --- stats

def basic_stats_region(data, area, mask_region=None, debug=False):
    """
    Calculates mean, standard deviation and root-mean-square of s.
    """
    import numpy as np
    import xarray

    # build a numpy masked array based on NaN
    if type(data) == xarray.core.dataarray.DataArray:
        data = data.to_numpy()
    if type(area) == xarray.core.dataarray.DataArray:
        area = area.to_numpy()
    if type(mask_region) == xarray.core.dataarray.DataArray:
        mask_region = mask_region.to_numpy()

    masked_data = np.ma.masked_invalid(data)

    dMin = np.ma.min(masked_data)
    dMax = np.ma.max(masked_data)

    # build weight from area and region mask
    if mask_region is not None:
        assert mask_region.shape == area.shape
        weight = mask_region * area
    else:
        weight = area.copy()

    if debug:
        print("stats: sum(area) =", np.ma.sum(weight))

    if not np.ma.getmask(masked_data).any() == np.ma.nomask:
        weight[masked_data.mask] = 0.0
    sumArea = np.ma.sum(weight)

    if debug:
        print("stats: sum(area) =", sumArea, "after masking")
        print("stats: sum(s) =", np.ma.sum(masked_data))
        print("stats: sum(area*s) =", np.ma.sum(weight * masked_data))

    mean = np.ma.sum(weight * masked_data) / sumArea
    std = np.sqrt(np.ma.sum(weight * ((masked_data - mean) ** 2)) / sumArea)
    rms = np.sqrt(np.ma.sum(weight * (masked_data ** 2)) / sumArea)
    if debug:
        print("stats: mean(s) =", mean)
    if debug:
        print("stats: std(s) =", std)
    if debug:
        print("stats: rms(s) =", rms)
    return dict(min=dMin, max=dMax, mean=mean, std=std, rms=rms)

