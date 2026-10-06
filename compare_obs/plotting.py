#!/usr/bin/env python

#  Gemini was used to assist with developing this code.
# The code has been reviewed, edited, and validated by NWS staff.

from datetime import datetime, timedelta

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib
import matplotlib.pyplot as plt
import cmocean

from stats import basic_stats_region

__all__ = [
    "plot_polar_seaice_diff",
    "plot_glo_sst_obs_bias_3rows",
    "plot_glo_sss_obs_bias_3rows",
    "plot_glo_ssh_obs_bias_3rows",
    "add_colorbar_obs",
    "add_colorbar_bias",
    "add_titles_3rows",
    "plot_glo_field",
    "plot_glo_bias",
    "tridepth_plot",
    "add_land_coastlines",
    "add_stats",
]


# --- plotting ---


def plot_polar_seaice_diff(grid, ref_data, dev_data, obs_data, date=None, offset=0):
    """
    Generates a 6-panel comparative figure comparing two sea ice concentration models
    using appropriate polar projections for the Arctic and Antarctic.

    Args:
        ref_data (np.ndarray): 2D array of Reference model sea ice concentration (0.0 to 1.0).
        dev_data (np.ndarray): 2D array of Development model sea ice concentration (0.0 to 1.0).
        obs_data (np.ndarray): 2D array of observed sea ice concentration (0.0 to 1.0).
        title_suffix (str): A suffix to append to the figure title for context.

    Returns:
        plt.Figure: The generated matplotlib Figure object.
    """
    matplotlib.rcParams.update({"font.size": 20})

    # --- 1. Setup the Figure (3x2 Grid) ---
    f = plt.figure(figsize=(24, 14)) # Large size for high-res printing
    gs = f.add_gridspec(2, 3) # Define the 3x2 grid layout

    # --- Constants & Projections ---
    PROJECTION = ccrs.NorthPolarStereo(central_longitude=-45)
    COLORMAP = 'Blues_r'
    COLORMAP_DIFF = 'RdBu_r'

    # --- 2. Plotting Helper Function ---
    def plot_panel(ax, lon, lat, data, colormap, vmin=None, vmax=None):
        """Helper function to apply styling and plot a single panel."""
        # The imshow command draws the data array onto the projected axis
        img = ax.pcolormesh(lon, lat, data, cmap=colormap, vmin=vmin, vmax=vmax,
                         transform=ccrs.PlateCarree())
        # Add geographical context (Coastlines and Gridlines)
        ax.coastlines(resolution='110m', color='black')
        #ax.gridlines(draw_default_x=False, draw_default_y=True, linestyle=':', alpha=0.6)
        return img

    lon = grid["lon"]
    lat = grid["lat"]

    # --- 3. Plotting the Four Panels (The Core Logic) ---

    # === TOP ROW: ARCTIC (North Polar Stereographic) ===

    # Panel 1: Top Left - Observations
    ax_arctic_dev = f.add_subplot(gs[0, 0], projection=PROJECTION)
    img1 = plot_panel(ax_arctic_dev, lon, lat, obs_data, COLORMAP, vmin=0, vmax=1)
    ax_arctic_dev.set_title("Observations")
    ax_arctic_dev.set_extent([0,360,55,90], crs=ccrs.PlateCarree())

    # Panel 2: Top Middle - Reference Arctic
    ax_arctic_ref = f.add_subplot(gs[0, 1], projection=PROJECTION)
    img2 = plot_panel(ax_arctic_ref, lon, lat, ref_data-obs_data, COLORMAP_DIFF, vmin=-1, vmax=1)
    ax_arctic_ref.set_title("Reference Model bias")
    ax_arctic_ref.set_extent([0,360,55,90], crs=ccrs.PlateCarree())

    # Panel 3: Top Right - Development Arctic
    ax_arctic_dev = f.add_subplot(gs[0, 2], projection=PROJECTION)
    img3 = plot_panel(ax_arctic_dev, lon, lat, dev_data-obs_data, COLORMAP_DIFF, vmin=-1, vmax=1)
    ax_arctic_dev.set_title("Development Model bias")
    ax_arctic_dev.set_extent([0,360,55,90], crs=ccrs.PlateCarree())

    # === BOTTOM ROW: ANTARCTIC (South Polar Stereographic) ===

    # Note: For the Antarctic, we keep the same image data but use a South Polar projection.
    # This makes the imagery "fit" to the South Pole.
    ANTARCTIC_PROJECTION = ccrs.SouthPolarStereo(central_longitude=-45)

    # Panel 4: Bottom Left - Observations Antarctic
    ax_antarctic_dev = f.add_subplot(gs[1, 0], projection=ANTARCTIC_PROJECTION)
    img4 = plot_panel(ax_antarctic_dev, lon, lat, obs_data, COLORMAP, vmin=0, vmax=1)
    ax_antarctic_dev.set_title("Observations")
    ax_antarctic_dev.set_extent([0,360,-90,-55], crs=ccrs.PlateCarree())

    # Panel 5: Bottom Middle - Reference Antarctic
    ax_antarctic_ref = f.add_subplot(gs[1, 1], projection=ANTARCTIC_PROJECTION)
    img5 = plot_panel(ax_antarctic_ref, lon, lat, ref_data-obs_data, COLORMAP_DIFF, vmin=-1, vmax=1)
    ax_antarctic_ref.set_title("Reference Model bias")
    ax_antarctic_ref.set_extent([0,360,-90,-55], crs=ccrs.PlateCarree())

    # Panel 6: Bottom Right - Development Antarctic
    ax_antarctic_dev = f.add_subplot(gs[1, 2], projection=ANTARCTIC_PROJECTION)
    img6 = plot_panel(ax_antarctic_dev, lon, lat, dev_data-obs_data, COLORMAP_DIFF, vmin=-1, vmax=1)
    ax_antarctic_dev.set_title("Development Model bias")
    ax_antarctic_dev.set_extent([0,360,-90,-55], crs=ccrs.PlateCarree())

    # --- 4. Colorbar and title ---

    f.subplots_adjust(right=0.9) # Make space on the right for the colorbar
    cbar_ax1 = f.add_axes([0.9, 0.11, 0.03, 0.77]) # Position the colorbar
    plt.colorbar(img6, cax=cbar_ax1, label='Sea Ice Concentration bias', orientation='vertical')

    f.subplots_adjust(left=0.17) # Make space on the left for the colorbar
    cbar_ax2 = f.add_axes([0.1, 0.11, 0.03, 0.77]) # Position the colorbar
    plt.colorbar(img1, cax=cbar_ax2, label='Sea Ice Concentration', orientation='vertical')

    # Global Title
    date_offset = date + timedelta(days=offset)
    cdate = datetime.strftime(date_offset, "%Y-%m-%d")

    if offset < 0:
        title_suffix = f"{cdate} nowcast t -{-offset} days"
    elif offset == 0:
        title_suffix = f"{cdate} nowcast t + {offset} days"
    else:  # offset > 0:
        title_suffix = f"{cdate} forecast t + {offset} days"

    f.suptitle(f"RTOFS Sea Ice Concentration bias {title_suffix}", fontweight='bold')

    return f


def plot_glo_sst_obs_bias_3rows(
    grid,
    obs,
    ref,
    dev,
    titles=["OSTIA", "RTOFS 2.5", "RTOFS 3"],
    date=None,
    offset=0,
):
    """Create a 3-panel figure comparing observed SST and model biases.

    The first row displays the observed SST. The second and third rows display
    the temperature bias (Model - Observation) for the reference and development
    runs, respectively.

    Parameters
    ----------
    grid : xarray.Dataset or dict-like
        Grid dataset containing 'lon', 'lat', and 'area'.
    obs : xarray.DataArray or numpy.ndarray
        Observed SST field.
    ref : xarray.DataArray or numpy.ndarray
        Reference model SST field.
    dev : xarray.DataArray or numpy.ndarray
        Development model SST field.
    titles : list of str, optional
        Titles for the three panels. Default is ["OSTIA", "RTOFS 2.5", "RTOFS 3"].
    date : datetime, optional
        Base date of the comparison. Default is None.
    offset : int, optional
        Offset in days to add to the base date. Default is 0.

    Returns
    -------
    matplotlib.figure.Figure
        The generated figure containing the three subplots.
    """
    matplotlib.rcParams.update({"font.size": 20})

    # set up the plot
    f, axs = plt.subplots(
        nrows=3,
        figsize=[12, 24],
        subplot_kw=dict(projection=ccrs.Robinson(central_longitude=200)),
    )
    # plot the various fields
    CN = plot_glo_field(axs[0], grid["lon"], grid["lat"], obs)
    CD = plot_glo_bias(axs[1], grid["lon"], grid["lat"], ref - obs, grid["area"])
    _ = plot_glo_bias(axs[2], grid["lon"], grid["lat"], dev - obs, grid["area"])
    # add colorbars
    add_colorbar_obs(f, CN)
    add_colorbar_bias(f, CD)
    # cosmetics
    for k in range(3):
        add_land_coastlines(axs[k])
    add_titles_3rows(axs, titles=titles, date=date, offset=offset)
    return f


def plot_glo_sss_obs_bias_3rows(
    grid,
    obs,
    ref,
    dev,
    titles=["CMEMS", "RTOFS 2.5", "RTOFS 3"],
    date=None,
    offset=0,
):
    """Create a 3-panel figure comparing observed SSS and model biases.

    The first row displays the observed SSS. The second and third rows display
    the salinity bias (Model - Observation) for the reference and development
    runs, respectively.

    Parameters
    ----------
    grid : xarray.Dataset or dict-like
        Grid dataset containing 'lon', 'lat', and 'area'.
    obs : xarray.DataArray or numpy.ndarray
        Observed SSS field.
    ref : xarray.DataArray or numpy.ndarray
        Reference model SSS field.
    dev : xarray.DataArray or numpy.ndarray
        Development model SSS field.
    titles : list of str, optional
        Titles for the three panels. Default is ["CMEMS", "RTOFS 2.5", "RTOFS 3"].
    date : datetime, optional
        Base date of the comparison. Default is None.
    offset : int, optional
        Offset in days to add to the base date. Default is 0.

    Returns
    -------
    matplotlib.figure.Figure
        The generated figure containing the three subplots.
    """
    matplotlib.rcParams.update({"font.size": 20})

    # set up the plot
    f, axs = plt.subplots(
        nrows=3,
        figsize=[12, 24],
        subplot_kw=dict(projection=ccrs.Robinson(central_longitude=200)),
    )
    # plot the various fields
    CN = plot_glo_field(axs[0], grid["lon"], grid["lat"], obs, vmin=10, vmax=36, cmap=cmocean.cm.haline)
    CD = plot_glo_bias(axs[1], grid["lon"], grid["lat"], ref - obs, grid["area"], cmap=cmocean.cm.balance)
    _ = plot_glo_bias(axs[2], grid["lon"], grid["lat"], dev - obs, grid["area"], cmap=cmocean.cm.balance)
    # add colorbars
    add_colorbar_obs(f, CN, field="SSS", units="PSU")
    add_colorbar_bias(f, CD, field="SSS", units="PSU")
    # cosmetics
    for k in range(3):
        add_land_coastlines(axs[k])
    add_titles_3rows(axs, titles=titles, date=date, offset=offset)
    return f


def plot_glo_ssh_obs_bias_3rows(
    grid,
    obs,
    ref,
    dev,
    titles=["CMEMS", "RTOFS 2.5", "RTOFS 3"],
    date=None,
    offset=0,
):
    """Create a 3-panel figure comparing observed SSH and model biases.

    The first row displays the observed SSH - global mean. The second and third rows display
    the SSH bias (Model - Observation) for the reference and development
    runs, respectively.

    Parameters
    ----------
    grid : xarray.Dataset or dict-like
        Grid dataset containing 'lon', 'lat', and 'area'.
    obs : xarray.DataArray or numpy.ndarray
        Observed SSH field minus global average.
    ref : xarray.DataArray or numpy.ndarray
        Reference model SSH field minus global average.
    dev : xarray.DataArray or numpy.ndarray
        Development model SSH field minus global average.
    titles : list of str, optional
        Titles for the three panels. Default is ["CMEMS", "RTOFS 2.5", "RTOFS 3"].
    date : datetime, optional
        Base date of the comparison. Default is None.
    offset : int, optional
        Offset in days to add to the base date. Default is 0.

    Returns
    -------
    matplotlib.figure.Figure
        The generated figure containing the three subplots.
    """
    matplotlib.rcParams.update({"font.size": 20})

    # set up the plot
    f, axs = plt.subplots(
        nrows=3,
        figsize=[12, 24],
        subplot_kw=dict(projection=ccrs.Robinson(central_longitude=200)),
    )
    # plot the various fields
    CN = plot_glo_field(axs[0], grid["lon"], grid["lat"], obs, vmin=-2, vmax=2, cmap=cmocean.cm.delta)
    CD = plot_glo_bias(axs[1], grid["lon"], grid["lat"], ref - obs, grid["area"], vmin=-0.5, vmax=0.5, cmap=cmocean.cm.balance)
    _ = plot_glo_bias(axs[2], grid["lon"], grid["lat"], dev - obs, grid["area"], vmin=-0.5, vmax=0.5, cmap=cmocean.cm.balance)
    # add colorbars
    add_colorbar_obs(f, CN, field="SSH", units="m")
    add_colorbar_bias(f, CD, field="SSH", units="m")
    # cosmetics
    for k in range(3):
        add_land_coastlines(axs[k])
    add_titles_3rows(axs, titles=titles, date=date, offset=offset)
    return f


def add_colorbar_obs(fig, C, field="SST", units="degC"):
    """Add a colorbar for the observation subplot.

    Parameters
    ----------
    fig : matplotlib.figure.Figure
        The figure to which the colorbar is added.
    C : matplotlib.cm.ScalarMappable
        The mappable object (e.g., QuadMesh) returned by pcolormesh.
    field : str, optional
        Field name to display in the label. Default is "SST".
    units : str, optional
        Units of the field to display in the label. Default is "degC".
    """
    cax = fig.add_axes([0.95, 0.655, 0.04, 0.225])
    fig.colorbar(C, cax=cax, label=f"{field} [{units}]")


def add_colorbar_bias(fig, C, field="SST", units="degC"):
    """Add a colorbar for the bias/difference subplots.

    Parameters
    ----------
    fig : matplotlib.figure.Figure
        The figure to which the colorbar is added.
    C : matplotlib.cm.ScalarMappable
        The mappable object (e.g., QuadMesh) returned by pcolormesh.
    field : str, optional
        Field name to display in the label. Default is "SST".
    units : str, optional
        Units of the field to display in the label. Default is "degC".
    """
    cax = fig.add_axes([0.95, 0.11, 0.04, 0.5])
    fig.colorbar(C, cax=cax, label=f"{field} bias [{units}]")


def add_titles_3rows(axs, titles=["obs", "ref", "dev"], date=None, offset=0):
    """Add descriptive titles to each of the three subplots.

    Calculates the target date using the base date and the offset, then formats
    the titles to indicate whether the model run is a nowcast or forecast.

    Parameters
    ----------
    axs : list of matplotlib.axes.Axes
        The three axes/subplots to receive titles.
    titles : list of str, optional
        Prefix titles for the panels. Default is ["obs", "ref", "dev"].
    date : datetime, optional
        Base date for the plot. Default is None.
    offset : int, optional
        Offset in days added to the base date. Default is 0.
    """
    date_offset = date + timedelta(days=offset)
    cdate = datetime.strftime(date_offset, "%Y-%m-%d")
    axs[0].set_title(f"{titles[0]} - {cdate}")
    if offset < 0:
        axs[1].set_title(f"{titles[1]} ---  nowcast t -{-offset} days")
        axs[2].set_title(f"{titles[2]} ---  nowcast t -{-offset} days")
    elif offset == 0:
        axs[1].set_title(f"{titles[1]} ---  nowcast t + {offset} days")
        axs[2].set_title(f"{titles[2]} ---  nowcast t + {offset} days")
    else:  # offset > 0:
        axs[1].set_title(f"{titles[1]} --- forecast t + {offset} days")
        axs[2].set_title(f"{titles[2]} --- forecast t + {offset} days")


def plot_glo_field(ax, lon, lat, array, vmin=-2, vmax=35, cmap="gist_ncar"):
    """Plot the global field on the specified axes.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        The target axes for the plot.
    lon : xarray.DataArray or numpy.ndarray
        2D longitude coordinates.
    lat : xarray.DataArray or numpy.ndarray
        2D latitude coordinates.
    array : xarray.DataArray or numpy.ndarray
        data to plot.
    vmin : float, optional
        Minimum value for the colormap scaling. Default is -2.
    vmax : float, optional
        Maximum value for the colormap scaling. Default is 35.
    cmap : str, optional
        Name of the colormap to use. Default is "gist_ncar".

    Returns
    -------
    matplotlib.collections.QuadMesh
        The plotted colormap object.
    """
    C = ax.pcolormesh(
        lon,
        lat,
        array,
        vmin=vmin,
        vmax=vmax,
        cmap=cmap,
        transform=ccrs.PlateCarree(),
    )
    return C


def plot_glo_bias(ax, lon, lat, array, area, vmin=-2, vmax=2, cmap="RdBu_r"):
    """Plot the global bias field and overlay regional statistics.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        The target axes for the plot.
    lon : xarray.DataArray or numpy.ndarray
        2D longitude coordinates.
    lat : xarray.DataArray or numpy.ndarray
        2D latitude coordinates.
    array : xarray.DataArray or numpy.ndarray
        Bias/difference data to plot.
    area : xarray.DataArray or numpy.ndarray
        Grid cell areas used to compute weighted statistics.
    vmin : float, optional
        Minimum value for the colormap scaling. Default is -2.
    vmax : float, optional
        Maximum value for the colormap scaling. Default is 2.
    cmap : str, optional
        Name of the colormap to use. Default is "RdBu_r".

    Returns
    -------
    matplotlib.collections.QuadMesh
        The plotted colormap object.
    """
    C = ax.pcolormesh(
        lon,
        lat,
        array,
        vmin=vmin,
        vmax=vmax,
        cmap=cmap,
        transform=ccrs.PlateCarree(),
    )
    stats = basic_stats_region(array, area, mask_region=None, debug=False)
    add_stats(ax, stats)
    return C


def tridepth_plot(da, vmin=0, vmax=99, cmap="jet", cbar_ticks=[], cbar_label="", title=""):
    """ my tri-depth plot for zonal means """

    import matplotlib.pyplot as plt

    plt.rcParams['font.family'] = 'sans-serif'
    plt.rcParams['font.sans-serif'] = ['Gill Sans', 'DejaVu Sans']

    plt.style.use('./styles/oldstyle.mplstyle')

    f, (a0, a1, a2) = plt.subplots(3, 1, height_ratios=[1, 2, 2], figsize=(12,8))
    kwargs = dict(yincrease=False, xincrease=True,
                  vmin=vmin, vmax=vmax,
                  cmap=cmap, add_colorbar=False)
    C = da.plot(ax=a0, y="pressure", ylim=[200,0], **kwargs)
    da.plot(ax=a1, y="pressure", ylim=[1000,200], **kwargs)
    da.plot(ax=a2, y="pressure", ylim=[7000,1000], **kwargs)

    for ax in [a0, a1]:
        ax.set_xticks([])
        ax.set_xlabel("")
    for ax in [a0, a2]:
        ax.set_ylabel("")
    for ax in [a1, a2]:
        ax.set_title("")

    a1.set_ylabel("Pressure [dbar]", fontsize=20)
    if "latitude" in da.dims:
        a2.set_xlabel("Latitude [$^{\circ}$N]", fontsize=20)
    elif "longitude" in da.dims:
        a2.set_xlabel("Longitude [$^{\circ}$E]", fontsize=20)
    a0.set_title("")

    cb_ax = f.add_axes([0.92, 0.08, 0.04, 0.80])
    cb = f.colorbar(C, cax=cb_ax)
    cb.set_ticks(cbar_ticks)
    cb.set_ticklabels(cbar_ticks)
    cb.set_label(cbar_label, fontsize=20)
    cb.ax.tick_params(labelsize=16)
    a0.set_facecolor([0.3,0.3,0.3])
    a1.set_facecolor([0.3,0.3,0.3])
    a2.set_facecolor([0.3,0.3,0.3])
    f.set_facecolor("cornsilk")
    f.suptitle(title, fontsize=22)
    return f


def add_land_coastlines(ax):
    """Add grey land features and black coastlines to the subplot.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        The target axes to add features to.
    """
    ax.add_feature(cfeature.LAND, color="grey")
    ax.coastlines()
    return None


def add_stats(ax, stats):
    """Overlay a wheat-colored text box with statistics on the subplot.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        The target axes where the text box will be placed.
    stats : dict
        A dictionary containing statistics:
        - 'min': Minimum bias value.
        - 'max': Maximum bias value.
        - 'mean': Area-weighted mean bias.
        - 'std': Area-weighted standard deviation of bias.
        - 'rms': Area-weighted root-mean-square of bias.
    """
    text_str = (
        f"min = {stats['min']:0.2f}\n"
        f"max = {stats['max']:0.2f}\n"
        f"mean = {stats['mean']:0.2f}\n"
        f"std = {stats['std']:0.2f}\n"
        f"rms = {stats['rms']:0.2f}"
    )

    props = dict(boxstyle="round", facecolor="wheat", alpha=0.5)
    ax.text(
        -0.15,
        0.05,
        text_str,
        transform=ax.transAxes,
        fontsize=18,
        verticalalignment="center",
        horizontalalignment="left",
        bbox=props,
        family="monospace",
    )

    return None
