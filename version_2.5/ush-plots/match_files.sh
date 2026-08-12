#!/bin/bash

# Matches files in the following paths: A and B.

# --- Check for Input ---
if [ -z "$1" ]; then
    echo "Usage: $0 YYYYMMDD"
    echo "Example: $0 20260310"
    exit 1
fi

# Assign the first argument to a variable
target_date="$1"

# --- Configuration ---
# Path A
path_A="/scratch4/NCEPDEV/marine/Zulema.Garraffo/rtofs_da/ncoda_new/restart/"

# Path B
path_B="/scratch5/NCEPDEV/rstprod/Santha.Akella/data/rtofs/v2p5/ncoda/${target_date}/hycom_var/restart/"

# --- Validation ---
if [[ ! -d "$path_B" ]]; then
    echo "Error: Path B does not exist for date ${target_date}"
    echo "Checked: ${path_B}"
    exit 1
fi

# --- Counters ---
count_B=0
matches=0

echo "Comparing files from: ${target_date}"
echo "---------------------------------------"

# Loop through files in Path B
for file_B in "${path_B}"*; do
    [[ -e "$file_B" ]] || continue
    ((count_B++))

    filename=$(basename "$file_B")

    # Check existence in the Trash Path A
    if [[ -f "${path_A}${filename}" ]]; then
        ((matches++))
    fi
done

# --- Verdict ---
echo "Total files found in B: $count_B"
echo "Matches found in A:     $matches"

if [ "$matches" -eq "$count_B" ] && [ "$count_B" -gt 0 ]; then
    echo "VERDICT: Match! All $count_B files exist in Path A."
else
    missing=$((count_B - matches))
    echo "VERDICT: No match. Path A is missing $missing files from this date."
fi
