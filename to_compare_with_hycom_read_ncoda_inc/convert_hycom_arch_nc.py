#!/usr/bin/env python3
import argparse
import os
import numpy as np
import xarray as xr

from hycom_io import read_hycom_fields, read_hycom_grid

def main():
    parser = argparse.ArgumentParser(description="Convert HYCOM archive to CF-compliant NetCDF")
    parser.add_argument("file_prefix", help="Path to the .a/.b file without extension")
    parser.add_argument("--var", default="temp", help="Variable to extract (default: temp)")
    parser.add_argument("--layer", type=int, default=None, help="Layer index to extract (default: all layers)")
    parser.add_argument("--grid", default="FIX/hycom/rtofs_glo.navy_0.08.regional.grid", 
                        help="Path to grid file prefix (default: FIX/hycom/rtofs_glo.navy_0.08.regional.grid)")
    parser.add_argument("--outdir", default=".", help="Directory to save output (default: current directory)")
    args = parser.parse_args()

    print(f"Reading grid from {args.grid}...")
    grid_fields = read_hycom_grid(args.grid, ['plon', 'plat'])
    lon = grid_fields['plon']
    lat = grid_fields['plat']

    # Determine if we are reading all layers or just one
    layers_to_read = [args.layer] if args.layer is not None else []
    layer_str = "all" if args.layer is None else str(args.layer)
    print(f"Reading {args.var} (layers: {layer_str}) from {args.file_prefix}...")
    
    data_fields = read_hycom_fields(args.file_prefix, [args.var], layers_to_read)
    data = data_fields[args.var] # Output shape from read_hycom_fields is (num_layers, lat, lon)

    print("Building CF-compliant xarray Dataset...")
    if args.layer is not None:
        # 2D dataset (Single Layer)
        data_2d = data[0, :, :]
        ds = xr.Dataset(
            data_vars={
                args.var: (["y", "x"], data_2d, {"coordinates": "longitude latitude"})
            },
            coords={
                "longitude": (["y", "x"], lon, {"standard_name": "longitude", "units": "degrees_east"}),
                "latitude": (["y", "x"], lat, {"standard_name": "latitude", "units": "degrees_north"}),
            }
        )
        out_filename = f"{os.path.basename(args.file_prefix)}_{args.var}_layer_{args.layer:02d}.nc"
    else:
        # 3D dataset (All Layers)
        num_layers = data.shape[0]
        ds = xr.Dataset(
            data_vars={
                args.var: (["layer", "y", "x"], data, {"coordinates": "longitude latitude"})
            },
            coords={
                "longitude": (["y", "x"], lon, {"standard_name": "longitude", "units": "degrees_east"}),
                "latitude": (["y", "x"], lat, {"standard_name": "latitude", "units": "degrees_north"}),
                "layer": (["layer"], np.arange(num_layers), {"standard_name": "ocean_layer"}),
            }
        )
        out_filename = f"{os.path.basename(args.file_prefix)}_{args.var}.nc"

    # Combine output directory with filename
    out_path = os.path.join(args.outdir, out_filename)
    print(f"Output NetCDF file will be: {out_path}")
    
    print("Saving...")
    ds.to_netcdf(out_path)
    print("Done!")

if __name__ == "__main__":
    main()
