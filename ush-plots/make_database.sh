#!/bin/bash

echo " "
echo "--- Creating a database of hycom_var/restart files ---" 
echo " "

# --- Configuration ---
base_dir="/scratch5/NCEPDEV/rstprod/Santha.Akella/data/rtofs/v2p5/ncoda"
target_dir="${base_dir}/dates_combined"

# Ensure the target exists
mkdir -p "$target_dir"

echo "Populating $target_dir with symlinks..."
echo "------------------------------------------------"

# Loop through all date directories (excluding the target itself)
for date_path in "${base_dir}"/2026*; do
    
    # Define the specific source subdirectory
    source_path="${date_path}/hycom_var/restart"

    # Only proceed if the source directory exists and has files
    if [[ -d "$source_path" ]]; then
        echo "Linking: $(basename "$date_path")"
        
        # ln -sf: s=symbolic, f=force (overwrites existing links if names collide)
        ln -sf "${source_path}"/* "$target_dir/"
    fi
done

echo "------------------------------------------------"
echo "Done. Total links in $(basename "$target_dir"): $(ls -1 "$target_dir" | wc -l)"

# ----
# CODACLIM is also needed.
# pwd: /scratch5/NCEPDEV/rstprod/Santha.Akella/data/rtofs/v2p5/ncoda
# one time: cp -r /scratch4/NCEPDEV/marine/Zulema.Garraffo/ncoda/fix/codaclim/* .
# ----
