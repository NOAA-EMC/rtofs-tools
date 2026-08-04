# RTOFS/HYCOM Processing and Plotting Tools

This toolset provides memory-efficient, command-line utilities to convert standard HYCOM/RTOFS `.a` and `.b` archive files into CF-compliant NetCDF files, and to generate equatorial cross-section plots directly from the archive files.

## Dependencies
Ensure you have a Python 3 environment loaded with the following modules:
*   `numpy`
*   `xarray`
*   `matplotlib`
*   `argparse` (Standard library)
*   `os` (Standard library)

*Note: On an HPC system, these can typically be satisfied by loading standard Python/Anaconda modules.*

## File Overview

1.  **`hycom_io.py`**: The core low-level I/O library (adapted from the [BB86 package](https://github.com/abozec/BB86_PACKAGE)). Handles parsing `.b` headers, reading Fortran binary `.a` files, and applying grid mapping with appropriate padding.
2.  **`convert_hycom_arch_nc.py`**: The Python engine that extracts requested variables/layers from the HYCOM archives, attaches CF-compliant metadata (lat/lon/layers), and exports an `xarray` Dataset to `.nc`.
3.  **`arch_to_nc.sh`**: The Bash wrapper for the conversion tool. Simplifies argument passing and handles file extensions gracefully.
4.  **`plot_eq_sec.py`**: A plotting tool that reads HYCOM archives directly into memory and generates side-by-side, 2D cross-section plots of two variables at the Equator (latitude ≈ 0). 

---

## 1. Converting Archive Files to NetCDF

Use the `arch_to_nc.sh` wrapper to extract variables from the `.a`/`.b` files into a single `.nc` file. 

**Syntax:**
```bash
./arch_to_nc.sh <file_prefix> [variable] [layer_index] [output_dir]

## Notes:
- `file_prefix`: Absolute or relative path to the archive file (can safely include or exclude the .a/.b extension).
- `variable`: (Optional) The specific variable to extract. Check the .b file for valid names (e.g., temp, salin, u-vel., v-vel., thknss). Default: temp.
- `layer_index`: (Optional) Integer index of a specific layer to extract (0-based). Use all to extract the full 3D volume. Default: all.
- `output_dir`: (Optional) Directory to save the resulting .nc file. Default: current directory (.).

## Examples:
- Extract all layers of temp to the current directory:
  `./arch_to_nc.sh /lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.20260804/rtofs_glo.t00z.n-24.archv temp`
- Extract ONLY layer 0 (surface) of salin to the current directory:
  `./arch_to_nc.sh /lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.20260804/rtofs_glo.t00z.n-24.archv salin 0`
- Extract all layers of temp and save to a custom directory:
  `./arch_to_nc.sh /lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.20260804/rtofs_glo.t00z.n-24.archv temp all /lfs/h2/emc/ptmp/santha.akella/xx/`

2. Plotting Equatorial Cross-Sections
- `plot_eq_sec.py` plots 2D longitudinal cross-sections (Longitude vs. Layer Index) for two variables simultaneously at the Equator. 
- It reads directly from the binary archives, bypassing the need for intermediate NetCDF generation.

./plot_eq_sec.py <file_prefix> [--var1 VAR1] [--var2 VAR2] [--grid GRID_PREFIX]

## Examples:
- Plot the default variables (temp and salin) for a given archive file:
  `./plot_eq_sec.py /lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.20260804/rtofs_glo.t00z.n-24.archv`
- Plot custom variables (e.g., u-vel. and v-vel.):
  `./plot_eq_sec.py /lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.20260804/rtofs_glo.t00z.n-24.archv --var1 "u-vel." --var2 "v-vel."`
- Output:
  - The script generates a PNG file in your current working directory named using 
    the archive's base name and the extracted variables, 
    for example: `rtofs_glo.t00z.n-24.archv_temp_salin.png`.
