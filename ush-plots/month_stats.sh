#!/bin/bash

source /scratch4/NCEPDEV/marine/$USER/bin/setup_env.sh

# Exit on error, treat unset variables as an error
set -e
set -x

# 1. Validation: Ensure both arguments are provided
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 yyyymmdd00 ndays expt"
    echo "Example: $0 2026031000 10 v2.5"
    exit 1
fi

# 2. Assign Arguments
dtgend=$1
ndays=$2
expt=$3

# 3. Environment & Paths
export bin_dir="/scratch4/NCEPDEV/marine/$USER/bin"

home_dir="/scratch4/NCEPDEV/marine/$USER"
run="rtofs.prod.${expt}"
expt_dir="$home_dir/$run"

OCN_OUT_base_dir="/scratch5/NCEPDEV/rstprod/Santha.Akella/data/rtofs/v2p5/ncoda"
export OCN_OUTPUT_DIR="${OCN_OUT_base_dir}/dates_combined"
export OCN_CLIM_DIR="${OCN_OUT_base_dir}/codaclim"

# --- INTEGRITY CHECK ---
echo "------------------------------------------------"
echo ">>> Validating Ocean Directories..."

for dir in "$OCN_OUTPUT_DIR" "$OCN_CLIM_DIR"; do
    # Check existence
    if [ ! -d "$dir" ]; then
        echo "FATAL ERROR: Directory NOT FOUND: $dir"
        exit 1
    fi

    # Get file count for informative logging
    # ls -1 lists one file per line; wc -l counts them
    num_files=$(ls -1 "$dir" | wc -l)

    if [ "$num_files" -eq 0 ]; then
        echo "FATAL ERROR: Directory is EMPTY (0 files): $dir"
        exit 1
    else
        echo "SUCCESS: Found $num_files files in $dir"
    fi
done

echo ">>> All directories verified. Proceeding with execution."
echo "------------------------------------------------"

# 4. Calculate Date Variables
# RD: Target date (typically end date minus 1 day)
RD=$($bin_dir/rtofs_dtg "$dtgend" -d 1)
RD_short=${RD:0:8}

# dtgstart: Start date of the range
# Bash arithmetic $((ndays - 1)) is faster than calling awk
ndm1=$((ndays - 1))
dtgstart=$($bin_dir/rtofs_dtg "$dtgend" -d -"$ndm1")

# 5. Run Diagnostic (Heredoc)
# We use - before the delimiter to allow indentation (optional)
sh diagn.sh $dtgstart $ndays

# 6. Run Graphing
sh graph.sh "$RD_short"

echo ">>> Statistics and Graphing complete for $dtgend"

# 7. Verification
# We use the same variables defined earlier in the script
FINAL_OUT_DIR="$expt_dir/ncoda/logs/hycom_var/${RD_short}_${expt}_ncoda"
sh "verify_plots.sh" "$FINAL_OUT_DIR"

echo ">>> All steps complete for $dtgend."
