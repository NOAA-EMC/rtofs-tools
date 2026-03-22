#!/bin/bash

# --- 1. Argument Handling ---
# Usage: ./fetch_rtofs_hpss_full.sh 20260320 [/optional/path]
if [[ -z "$1" ]]; then
    echo "ERROR: Date argument required (YYYYMMDD)."
    echo "Usage: $0 <YYYYMMDD> [target_path]"
    exit 1
fi

rtofs_date="$1"
# Default path provided in request
default_path="/scratch5/NCEPDEV/rstprod/Santha.Akella/data/rtofs/v2p5/ncoda"
target_dir="${2:-$default_path}/${rtofs_date}"

# --- 2. HPSS Source Configuration ---
hpss_base="/NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.prod"
hpss_tar="${hpss_base}/rtofs.${rtofs_date}/rtofs.da.tar"

# --- 3. Execution ---
echo "------------------------------------------------"
echo ">>> HPSS FULL RETRIEVAL START"
echo "Date        : ${rtofs_date}"
echo "Source Tape : ${hpss_tar}"
echo "Target Dir  : ${target_dir}"
echo "------------------------------------------------"

# Create target directory and move into it
mkdir -p "${target_dir}"
cd "${target_dir}" || exit 1

log_file="htar_fetch_full_${rtofs_date}.log"

echo ">>> Extracting ENTIRE archive (400+ files) from HPSS..."
# -T 4 uses 4 threads. 
# Leaving off a trailing file/folder argument tells HTAR to extract EVERYTHING.
htar -T 4 -xvf "${hpss_tar}" 2>&1 | tee "${log_file}"

# Check exit status
if [ $? -eq 0 ]; then
    echo "------------------------------------------------"
    echo ">>> SUCCESS: All files retrieved to ${target_dir}"
    echo ">>> Final Disk Usage Summary:"
    du -sh "${target_dir}"
    echo "------------------------------------------------"
else
    echo "------------------------------------------------"
    echo ">>> ERROR: HTAR failed. Check ${target_dir}/${log_file}"
    echo "------------------------------------------------"
    exit 1
fi
