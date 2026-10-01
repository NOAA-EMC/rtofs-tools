#!/usr/bin/env python3

import sys
import os
import numpy as np

# Add path to hycom_io Python path
current_dir = os.path.dirname(os.path.abspath(__file__))
sibling_dir = os.path.join(current_dir, "..", "convert_archives_increments")
sys.path.insert(0, sibling_dir)

from hycom_io import read_hycom_depth, read_hycom_fields

# HYCOM Grid Dimensions
idm = 4500
jdm = 3298
kdm = 41

input_data_path = "/lfs/h2/emc/ptmp/santha.akella/restarts/in/20251215/"
depth_file   = input_data_path + "rtofs_glo.navy_0.08.regional.depth.a"  # No .a/.b, hycom_io handles it
archive_file = input_data_path + "rtofs_glo.t00z.n00.archv"

# 1. Read Bathymetry (Depth)
print(f"Reading depth from {depth_file}...")
depths = read_hycom_depth(depth_file, idm, jdm, replace_to_nan=True)

# Create Land/Sea mask (ip in Fortran: 1 for ocean, 0 for land)
# FORCE to float32 to prevent numpy from upcasting everything to 64-bit later
ip = np.where(np.isnan(depths), 0.0, 1.0).astype(np.float32)

# 2. Read Archive Fields
fields_to_read = ['srfhgt', 'u_btrop', 'v_btrop', 'u-vel.', 'v-vel.', 'thknss', 'temp', 'salin']

print(f"Reading archive fields from {archive_file}...")
arch_data = read_hycom_fields(archive_file, fields_to_read, layers=[], replace_to_nan=True)

# Extract 2D fields (shape: 1, 3298, 4500 -> slice to 3298, 4500)
srfhgt = arch_data['srfhgt'] [0, :, :]
ubaro  = arch_data['u_btrop'][0, :, :]
vbaro  = arch_data['v_btrop'][0, :, :]

# Extract 3D fields (shape: 41, 3298, 4500)
u    = arch_data['u-vel.']
v    = arch_data['v-vel.']
dp   = arch_data['thknss']
temp = arch_data['temp']
saln = arch_data['salin']

# =====================================================================
# 2.2 Echo Field Statistics
# =====================================================================
print(f"\n{'field':<8} {'k':>2} {'min':>17} {'max':>17}")

with np.errstate(invalid='ignore'):  # Ignore warnings if a layer is entirely NaNs
    # Print 2D fields (k=0)
    print(f"{'srfhgt':<8} {0:>2} {np.nanmin(srfhgt):17.7E} {np.nanmax(srfhgt):17.7E}")
    print(f"{'u_btrop':<8} {0:>2} {np.nanmin(ubaro):17.7E} {np.nanmax(ubaro):17.7E}")
    print(f"{'v_btrop':<8} {0:>2} {np.nanmin(vbaro):17.7E} {np.nanmax(vbaro):17.7E}")

    # Print 3D fields (k=1 to 41)
    for k in range(kdm):
        layer = k + 1
        print(f"{'u-vel.':<8} {layer:>2} {np.nanmin(u[k]):17.7E} {np.nanmax(u[k]):17.7E}")
        print(f"{'v-vel.':<8} {layer:>2} {np.nanmin(v[k]):17.7E} {np.nanmax(v[k]):17.7E}")
        print(f"{'thknss':<8} {layer:>2} {np.nanmin(dp[k]):17.7E} {np.nanmax(dp[k]):17.7E}")
        print(f"{'temp':<8} {layer:>2} {np.nanmin(temp[k]):17.7E} {np.nanmax(temp[k]):17.7E}")
        print(f"{'salin':<8} {layer:>2} {np.nanmin(saln[k]):17.7E} {np.nanmax(saln[k]):17.7E}")
print("")
# =====================================================================

# =====================================================================
# 2.5 Sanity Check: Bathymetry Mask vs Archive Mask
# =====================================================================
print("Checking bathymetry mask against archive surface height...")

# topo sea (ip == 1), srfhgt land (np.nan)
ibads = np.sum((ip == 1.0) & np.isnan(srfhgt))

# topo land (ip == 0), srfhgt sea (not np.nan)
ibadl = np.sum((ip == 0.0) & ~np.isnan(srfhgt))

if ibads != 0:
    print("\nERROR: Wrong bathymetry for this archive file")
    print(f"number of topo sea  mismatches = {ibads}")
    print(f"number of topo land mismatches = {ibadl}\n")
    sys.exit(1)

if ibadl != 0:
    print("\nWARNING: Wrong bathymetry for this archive file")
    print(f"number of topo sea  mismatches = {ibads}")
    print(f"number of topo land mismatches = {ibadl}\n")
else:
    print("Mask check passed cleanly.")
# =====================================================================

# 3. Apply Transformations (In-place & Memory Efficient)
print("Applying barotropic additions and SSH thickness squeeze...")

# A. Convert baroclinic to total velocities IN-PLACE
u += ubaro
v += vbaro

# B. Convert layer thickness from Pascals to meters IN-PLACE
dp /= np.float32(9806.0)

# C. Calculate SSH thickness correction factor (q)
# Note: SSH is actually g * SSH. 
#       https://github.com/NOAA-EMC/RTOFS_GLO/blob/a848e385ce8ec1c6e6dcac26ea010b07b7cc54db/sorc/rtofs_code.fd/rtofs_archv2netCDF.fd/common_blocks.h#L26
#       Therefore q = (SSH / 9.806 + depth)/depth.
with np.errstate(divide='ignore', invalid='ignore'):
    q = np.where(ip == 1.0, (srfhgt / np.float32(9.806) + depths) / depths, 0.0).astype(np.float32)

# D. Apply the SSH squeeze to the layer thicknesses IN-PLACE
dp *= q

# E. Zero out land values IN-PLACE to save RAM
temp *= ip
saln *= ip
u    *= ip
v    *= ip
dp   *= ip

print("Data processing complete!")
print(f"Max total U-Velocity: {np.nanmax(u):.3f} m/s")
print(f"Max Corrected Layer 1 Thickness: {np.nanmax(dp[0,:,:]):.3f} m")
