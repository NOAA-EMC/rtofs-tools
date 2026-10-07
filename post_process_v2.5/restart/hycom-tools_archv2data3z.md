# Notes on `archv2data3z`
  - The version of this main program in [RTOFS (v2.5)](https://github.com/NOAA-EMC/RTOFS_GLO/tree/release/v2.5.5)
    has [not been kept up to date](https://github.com/NOAA-EMC/RTOFS_GLO/blob/release/v2.5.5/sorc/rtofs_code.fd/rtofs_archv2netCDF.fd/archv2data3z.f)
    with that in the authoritative [HYCOM-tools repository.](https://github.com/HYCOM/HYCOM-tools/blob/master/archive/src/archv2data3z.f)

  - The following pseudo-Code for `rtofs_archv2ncdf3z` (is for the latest Version) from HYCOM-tools, with
    with [NEW] and [UPDATED] tags to highlight what changed from the older 6-year-old version in RTOFSv2.5.
  
  - Has been generated using a LLM.

### Pseudo-Code for `rtofs_archv2ncdf3z` (Latest Version)

**1. Parse User Configuration (Standard Input)**
*   Read file paths and output format.
*   Read standard grid info: `idm`, `jdm`, `kdm`, experiment number, etc.
*   Read processing flags:
    *   `smooth`: Should fields be spatially smoothed?
    *   `baclin` / `xyward`: Baroclinic vs total velocity? East/North vs X/Y grid velocities?
    *   **[NEW]** `intfwv`: Calculate vertical velocity (w) at layer interfaces (True) or layer centers (False).
    *   **[NEW]** `itest` / `jtest`: Specific grid point coordinates to print debug text for.
    *   **[UPDATED]** `itype`: Interpolation method (0=sample, 1=linear PLM, 2=parabolic PPM, **[NEW] 3=cubic PCHIP**).
*   Read target Z-levels (`kz` or `kzi`).
*   **[UPDATED]** Read output toggles: Includes old fields plus **[NEW]** `sshio` (Sea Surface Height) and **[NEW]** `istio` (In-Situ Temperature).
*   **[UPDATED]** Read Mixed Layer Depth (MLD) toggles: Now supports **8 distinct formulations** (Temperature jump, Density jump, Equivalent Temp PLM/PQM, **[NEW]** Lorbacher Temp/Density, **[NEW]** NAVO Temp/Density).

**2. Read Native HYCOM Data**
*   Read bathymetry/depth (`regional.depth.a`/`.b`).
*   **[UPDATED]** Read the archive file. Detect **[NEW]** `lpvel` flag (if `artype < 0`), which indicates velocities are already on the P-grid (no unstaggering needed later).

**3. Initial Physical Transformations (Layer Space)**
*   *Velocity:* Add barotropic to baroclinic to get total velocity (logic depends on `lpvel`).
*   *Thickness:* Convert pressure thickness (`dp`) to physical meters (divide by 9806.0).
*   *Interface Depths:* Calculate 3D interface depths (`p`) via cumulative sum.
*   *Land Masking:* Replace land points with missing value flag (`2.0**100`).
*   **[NEW] *Depth Correction Squeeze:* Calculates the difference between the sum of the layer thicknesses (`p(kk+1)`) and the true bathymetry (`depths`). If they don't match exactly, applies a scaling factor `q = depths / p(kk+1)` to force the 3D layers to fit perfectly inside the 2D bathymetry without blowing out the bottom.**

**4. Compute Vertical Velocity (W) and Eddy Kinetic Energy**
*   **[UPDATED]** Calculate vertical velocity `w` from horizontal divergence. Now applies specific math based on the `intfwv` flag (interface vs layer center calculation).

**5. Handle "Massless" Layers**
*   Loop through the water column. If a layer is massless (pinches out), fill Temp, Saln, Density, U, and V with the vertically averaged values of the surrounding layers.

**6. Spatial Smoothing (Optional, if `smooth` is True)**
*   Apply spatial smoother to mass fields (Temp, Salinity, Density).
*   Convert velocities to volume fluxes, smooth, and convert back (logic adjusted by `lpvel`).

**7. Interpolate to Z-Levels and Write 3D Variables**
*   For each requested variable (U, V, Speed, W, Temp, Salinity, Density, Tracers):
    *   **[UPDATED]** *Unstaggering:* If `lpvel` is false, convert flux-form cell edges to cell centers. If `lpvel` is true, use data as-is.
    *   *Rotation:* Rotate U/V to Eastward/Northward if requested.
    *   **[NEW] *In-Situ Temperature:* If requested, calculate hydrostatic pressure (`PPSW_p80`) and use the WHOI CTD Equation of State (`PPSW_theta`) to back-calculate true In-Situ temperature from Potential Temperature and Salinity.**
    *   *Vertical Interpolation:* Map the 3D layer data onto 1D depth levels (`layer2z`/`layer2c`).
    *   *Output:* Write the interpolated Z-level array to NetCDF.

**8. Compute and Write 2D Diagnostic Fields**
*   Write Bathymetry.
*   Write vertically averaged column density.
*   **[NEW]** Write Sea Surface Height (SSH) converted to MKS units.
*   **[UPDATED]** Calculate and write MLD based on the expanded suite of 8 different algorithmic formulations.
