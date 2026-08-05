#!/usr/bin/env python3

# =============================================================================
# Script: convert_analinc_nc.py
# Purpose: Reads NCODA Fortran unformatted binary `_analinc` files, parses
#          dimensions from the filename, attaches lat/lon grids, and exports 
#          to an xarray-friendly CF-compliant NetCDF file.
# =============================================================================

import argparse
import os
import re
import numpy as np
import xarray as xr

# Import the existing grid reader
try:
    from hycom_io import read_hycom_grid
except ImportError:
    raise ImportError("Could not import hycom_io.py. Ensure it is in the same directory.")

def main():
    parser = argparse.ArgumentParser(description="Convert NCODA _analinc binary to CF-compliant NetCDF")
    parser.add_argument("input_file", help="Full path to the _analinc file")
    parser.add_argument("out_dir", help="Output directory path")
    parser.add_argument("--grid", default="FIX/hycom/rtofs_glo.navy_0.08.regional.grid", 
                        help="Path to grid file prefix (default: FIX/hycom/rtofs_glo.navy_0.08.regional.grid)")
    args = parser.parse_args()

    in_file = args.input_file
    out_dir = args.out_dir
    basename = os.path.basename(in_file)

    # 1. Parse filename for metadata (e.g., seatmp_lyr_1o4500x3298_2026080200_0000_analinc)
    parts = basename.split('_')
    var_name = parts[0]
    
    # Extract IDM and JDM dynamically from the filename string (e.g., "1o4500x3298")
    match = re.search(r'(\d+)x(\d+)', basename)
    if match:
        idm = int(match.group(1))
        jdm = int(match.group(2))
    else:
        raise ValueError(f"Could not parse grid dimensions (IDMxJDM) from filename: {basename}")

    print(f"Metadata extracted -> Variable: {var_name}, IDM: {idm}, JDM: {jdm}")

    # 2. Read the unformatted binary stream (>f4 = 32-bit big-endian float)
    print(f"Reading binary file: {in_file}")
    file_size = os.path.getsize(in_file)
    layer_size_bytes = idm * jdm * 4
    num_layers = file_size // layer_size_bytes

    if file_size % layer_size_bytes != 0:
        print(f"Warning: File size {file_size} is not a perfect multiple of the layer size. Reading valid layers only.")
    
    print(f"Detected {num_layers} layer(s).")

    data = np.fromfile(in_file, dtype='>f4', count=(num_layers * idm * jdm))
    data = data.reshape((num_layers, jdm, idm))
    
    # Clean massive Fortran missing-data values
    data[data > 1e20] = np.nan

    # 3. Read RTOFS Grid
    print(f"Loading grid coordinates from {args.grid}...")
    grid_fields = read_hycom_grid(args.grid, ['plon', 'plat'])
    lon = grid_fields['plon']
    lat = grid_fields['plat']

    # 4. Build CF-compliant xarray Dataset
    print("Building CF-compliant xarray Dataset...")
    if num_layers == 1:
        # Surface/2D variable
        data_slice = data[0, :, :]
        ds = xr.Dataset(
            data_vars={
                var_name: (["y", "x"], data_slice, {"coordinates": "longitude latitude"})
            },
            coords={
                "longitude": (["y", "x"], lon, {"standard_name": "longitude", "units": "degrees_east"}),
                "latitude": (["y", "x"], lat, {"standard_name": "latitude", "units": "degrees_north"}),
            }
        )
    else:
        # Volumetric/3D variable
        ds = xr.Dataset(
            data_vars={
                var_name: (["layer", "y", "x"], data, {"coordinates": "longitude latitude"})
            },
            coords={
                "longitude": (["y", "x"], lon, {"standard_name": "longitude", "units": "degrees_east"}),
                "latitude": (["y", "x"], lat, {"standard_name": "latitude", "units": "degrees_north"}),
                "layer": (["layer"], np.arange(num_layers), {"standard_name": "ocean_layer"}),
            }
        )

    # 5. Save output
    os.makedirs(out_dir, exist_ok=True)
    out_file = os.path.join(out_dir, f"{basename}.nc")
    print(f"Saving NetCDF to: {out_file}")
    ds.to_netcdf(out_file)
    print("Conversion complete!")

if __name__ == "__main__":
    main()
