#!/bin/bash

# Wrapper to execute the RTOFS HYCOM to NetCDF converter
if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <path_to_hycom_file_prefix> [variable] [layer_index] [output_dir]"
    echo "Example (All layers, curr dir): $0 /lfs/h1/ops/.../rtofs_glo.t00z.n-24.archv temp"
    echo "Example (One layer, curr dir):  $0 /lfs/h1/ops/.../rtofs_glo.t00z.n-24.archv temp 0"
    echo "Example (All layers, cust dir): $0 /lfs/h1/ops/.../rtofs_glo.t00z.n-24.archv temp all /my/output/path"
    echo " "
    echo "See .b file, e.g., /lfs/h1/ops/prod/com/rtofs/v2.5/rtofs.20260804/rtofs_glo.t00z.n-24.archv.b for list of allowed variables."
    echo " "
    exit 1
fi

# Strip the .a or .b extension if the user accidentally included it
FILE_PREFIX="${1%.a}"
FILE_PREFIX="${FILE_PREFIX%.b}"
VAR_NAME="${2:-temp}"

# Only pass the layer argument if the user provided it and it isn't "all"
LAYER_ARG=""
if [ -n "$3" ] && [ "$3" != "all" ]; then
    LAYER_ARG="--layer $3"
fi

# Set output directory to current directory if not provided
OUT_DIR="${4:-.}"
mkdir -p "${OUT_DIR}"

# Execute the Python conversion tool
./convert_hycom_arch_nc.py "${FILE_PREFIX}" --var "${VAR_NAME}" $LAYER_ARG --outdir "${OUT_DIR}"
