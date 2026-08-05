#!/usr/bin/env python3
import argparse
import os
import numpy as np
import matplotlib
matplotlib.use('Agg')  # Fast, non-interactive backend
import matplotlib.pyplot as plt

# Import directly from your existing hycom_io.py
from hycom_io import read_hycom_fields, read_hycom_grid

def main():
    parser = argparse.ArgumentParser(description="Plot equatorial cross-section directly from HYCOM archive files")
    parser.add_argument("prefix", help="Path to the archive file (e.g., /path/to/rtofs_glo.t00z.n-24.archv)")
    parser.add_argument("--var1", default="temp", help="First variable (default: temp)")
    parser.add_argument("--var2", default="salin", help="Second variable (default: salin)")
    parser.add_argument("--grid", default="FIX/hycom/rtofs_glo.navy_0.08.regional.grid", 
                        help="Path to grid file prefix (default: FIX/hycom/...)")
    args = parser.parse_args()

    # Strip .a or .b if accidentally provided
    prefix = args.prefix
    if prefix.endswith('.a') or prefix.endswith('.b'):
        prefix = prefix[:-2]

    print(f"Reading grid from {args.grid}...")
    grid = read_hycom_grid(args.grid, ['plon', 'plat'])
    plon = grid['plon']
    plat = grid['plat']

    # Find the row (y-index) closest to the equator
    lat_mean = np.nanmean(plat, axis=1)
    y_eq_idx = int(np.nanargmin(np.abs(lat_mean)))
    actual_lat = lat_mean[y_eq_idx]
    print(f"Found Equator at y-index {y_eq_idx} (Mean Latitude: {actual_lat:.4f}°)")
    
    lon_slice = plon[y_eq_idx, :]

    print(f"Reading {args.var1} and {args.var2} from {prefix}...")
    # Read all layers directly into memory (empty layers list implies all layers)
    data = read_hycom_fields(prefix, [args.var1, args.var2], layers=[])
    
    # Extract the lat=0 slice for all layers. 
    # Shape of returned arrays is (num_layers, lat_size, lon_size)
    data1 = data[args.var1][:, y_eq_idx, :]
    data2 = data[args.var2][:, y_eq_idx, :]
    num_layers = data1.shape[0]
    layers = np.arange(num_layers)

    print("Rendering plot...")
    fig, (ax1, ax2) = plt.subplots(nrows=1, ncols=2, figsize=(16, 6), sharey=True)

    # Plot var1
    cmap1 = 'RdYlBu_r' if args.var1 == 'temp' else 'viridis'
    im1 = ax1.pcolormesh(lon_slice, layers, data1, cmap=cmap1, shading='auto')
    ax1.invert_yaxis()
    ax1.set_title(f"{args.var1.capitalize()} at Equator (lat ≈ {actual_lat:.2f}°)")
    ax1.set_xlabel("Longitude (Degrees East)")
    ax1.set_ylabel("Layer Index")
    fig.colorbar(im1, ax=ax1, label=args.var1)

    # Plot var2
    cmap2 = 'viridis' if args.var2 == 'salin' else 'plasma'
    im2 = ax2.pcolormesh(lon_slice, layers, data2, cmap=cmap2, shading='auto')
    ax2.set_title(f"{args.var2.capitalize()} at Equator (lat ≈ {actual_lat:.2f}°)")
    ax2.set_xlabel("Longitude (Degrees East)")
    fig.colorbar(im2, ax=ax2, label=args.var2)

    plt.tight_layout()

    # Save output
    base_prefix = os.path.basename(prefix)
    out_filename = f"{base_prefix}_{args.var1}_{args.var2}.png"
    print(f"Saving plot to {out_filename}...")
    plt.savefig(out_filename, dpi=150)
    plt.close()
    
    print("Done!")

if __name__ == "__main__":
    main()
