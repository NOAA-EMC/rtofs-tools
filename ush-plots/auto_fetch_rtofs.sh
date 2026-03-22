#!/bin/bash

# Usage: ./auto_fetch_rtofs.sh 20260301 20260305
# nohup ./auto_fetch_rtofs.sh 20260301 20260310 > full_run.log 2>&1 &

if [[ $# -lt 2 ]]; then
    echo "Usage: $0 <START_DATE> <END_DATE>"
    exit 1
fi

# Load modules first to ensure 'ndate' and 'htar' are available
# source ./machine_modules.sh

curr_date="$1"
end_date="$2"
worker_script="./fetch_rtofs_hpss_full.sh"

# Ensure worker is executable
chmod +x "${worker_script}"

echo "===================================================="
echo "BEGIN AUTOMATED FETCH: ${curr_date} TO ${end_date}"
echo "===================================================="

while [[ "${curr_date}" -le "${end_date}" ]]; do
    
    # Execute the worker for the current date
    "${worker_script}" "${curr_date}"
    
    # Capture the worker's exit status (The "Drop Dead" signal)
    if [[ $? -ne 0 ]]; then
        echo "AUTOMATION HALTED: Failure detected on ${curr_date}."
        exit 1
    fi

    # Increment date by 24 hours
    curr_date=$(ndate 24 "${curr_date}")
    
done

echo "===================================================="
echo "ALL DATES COMPLETED SUCCESSFULLY"
echo "===================================================="
