#!/bin/bash

# =============================================================================
# Script: analinc_to_nc.sh
# Purpose: Wrapper to run the NCODA analinc to NetCDF Python converter.
#          Ensures directories exist and validates the output file.
# =============================================================================

if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <path_to_analinc_file> <output_directory>"
    echo "Example: $0 /lfs/h1/ops/.../seatmp_lyr_1o4500x3298_2026080200_0000_analinc /my/output/path"
    exit 1
fi

IN_FILE="$1"
OUT_DIR="$2"
BASENAME=$(basename "$IN_FILE")
OUT_FILE="${OUT_DIR}/${BASENAME}.nc"

# 1. Check if input file exists
if [ ! -f "$IN_FILE" ]; then
    echo "Error: Input file does not exist: $IN_FILE"
    exit 1
fi

# 2. Check and create output directory if it doesn't exist
if [ ! -d "$OUT_DIR" ]; then
    echo "Creating output directory: $OUT_DIR"
    mkdir -p "$OUT_DIR"
fi

# 3. Execute the Python script
echo "Executing Python conversion tool..."
./convert_analinc_nc.py "$IN_FILE" "$OUT_DIR"
PY_STATUS=$?

# 4. Validate success and check if the output file is > 0 bytes
if [ $PY_STATUS -eq 0 ] && [ -s "$OUT_FILE" ]; then
    echo "=================================================================="
    echo "Success! NetCDF generated and is not empty:"
    ls -lh "$OUT_FILE"
else
    echo "=================================================================="
    echo "Error: Conversion failed, or the output NetCDF is empty."
    exit 1
fi
