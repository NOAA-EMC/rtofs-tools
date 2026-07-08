#!/usr/bin/env python

#  Gemini was used to assist with developing this code.
# The code has been reviewed, edited, and validated by NWS staff.

import numpy as np
import xarray as xr

__all__ = ["basic_stats_region"]


# --- stats ---


def basic_stats_region(data, area, mask_region=None, debug=False):
    """Calculate basic statistics (min, max, mean, std, rms) for a region.

    Computes the minimum, maximum, area-weighted mean, area-weighted standard
    deviation, and area-weighted root-mean-square (RMS) of the input data field.
    Any NaN or invalid values in the data are automatically masked out and excluded
    from the statistical calculations.

    Parameters
    ----------
    data : xarray.DataArray or numpy.ndarray
        The input data field (e.g., temperature bias).
    area : xarray.DataArray or numpy.ndarray
        Grid cell areas used for weighting.
    mask_region : xarray.DataArray or numpy.ndarray, optional
        A binary mask (1 for active cells, 0 for inactive/masked cells).
        Default is None.
    debug : bool, optional
        If True, prints debug statements detailing intermediate area and data sums.
        Default is False.

    Returns
    -------
    dict
        A dictionary containing the calculated statistics with keys:
        - 'min': Minimum value.
        - 'max': Maximum value.
        - 'mean': Area-weighted mean.
        - 'std': Area-weighted standard deviation.
        - 'rms': Area-weighted root-mean-square.
    """
    # build a numpy masked array based on NaN
    if isinstance(data, xr.DataArray):
        data = data.to_numpy()
    if isinstance(area, xr.DataArray):
        area = area.to_numpy()
    if isinstance(mask_region, xr.DataArray):
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
