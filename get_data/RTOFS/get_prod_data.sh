#!/bin/bash

# RTOFS operational version
rtofs_version="v2.5"

# 1. Read start and end dates directly from the two input arguments
if [[ $# -ne 3 ]]; then
    echo "Usage: $0 <start_date> <end_date> <output_base_dir>"
    echo "Example: $0 20251201 20251205 /scratch5/NCEPDEV/rstprod/Santha.Akella/data/rtofs/v2p5/output"
    echo "Dates must be exactly 8 digits in YYYYMMDD format."
    echo "Path must be full path"
    exit 1
fi

start_date=$1
end_date=$2
output_base_dir=$3

# Validate input length (basic check)
if [[ ${#start_date} -ne 8 || ${#end_date} -ne 8 ]]; then
    echo "Error: Dates must be exactly 8 digits in YYYYMMDD format."
    exit 1
fi

current_date="$start_date"
failed_dates=()
successful_count=0
base_dir="$PWD"

echo "Starting HTAR extraction from $start_date to $end_date (Version: $rtofs_version)..."
echo "--------------------------------------------------------"

# 2. Loop through each date
while [[ "$current_date" -le "$end_date" ]]; do
    
    # Extract year, month, and day components for the file path
    YYYY="${current_date:0:4}"
    YYYYMM="${current_date:0:6}"
    YYYYMMDD="${current_date}"
    
    # Construct the exact file path, target directory, and log file path
    tar_file="/NCEPPROD/1year/hpssprod/runhistory/rh${YYYY}/${YYYYMM}/${YYYYMMDD}/com_rtofs_${rtofs_version}_rtofs.${YYYYMMDD}.nc.tar"
    target_dir="${output_base_dir}/rtofs.${YYYYMMDD}"
    log_file="${base_dir}/htar_run_${YYYYMMDD}.log"
    
    echo -n "Fetching data for $YYYYMMDD into $target_dir/... "
    
    # 3. Create the directory and move into it
    mkdir -p "$target_dir"
    cd "$target_dir" || { echo "FAILED to create or access $target_dir"; exit 1; }
    
    # 4. Execute the htar command and redirect stdout/stderr to the log file in the base directory
    htar -T 4 -xvf "$tar_file" > "$log_file" 2>&1
    
    # Move back to the base directory before continuing the loop
    cd "$base_dir" || exit 1
    
    # 5. Query the log for the success string
    if grep -q "HTAR: HTAR SUCCESSFUL" "$log_file"; then
        # If successful, find the line containing the time and bytes read
        stats_line=$(grep "total bytes read" "$log_file")
        
        echo "SUCCESS."
        echo "   -> $stats_line"
        
        ((successful_count++))
        
        # Clean up the log file if successful to save space
        rm "$log_file"
    else
        echo "FAILED."
        echo "   -> Could not find 'HTAR SUCCESSFUL' in the output. Check $log_file for details."
        
        # Track failures
        failed_dates+=("$YYYYMMDD")
        
        # Note: We do NOT delete the log file here so you can debug the error.
    fi
    
    # Increment the date by 1 day using GNU date
    current_date=$(date -d "$current_date + 1 day" +%Y%m%d)
done

echo "--------------------------------------------------------"
echo "Extraction Complete."
echo "Successfully fetched: $successful_count days."

# 6. Report any missing data/failures
if [[ ${#failed_dates[@]} -eq 0 ]]; then
    echo "All dates were fetched successfully! No missing data."
else
    echo "WARNING: The following dates FAILED to fetch properly:"
    for fd in "${failed_dates[@]}"; do
        echo " - $fd"
    done
fi
