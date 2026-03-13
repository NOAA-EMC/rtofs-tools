#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=01:30:00
#SBATCH --account=marine-cpu
#SBATCH --job-name=NCODA_GRAPH
#SBATCH --output=joblog/NCODA_GRAPH.joblog
#SBATCH -q batch

# Load Environment
source /scratch4/NCEPDEV/marine/$USER/bin/setup_env.sh

set -ex 

# Configuration
home_dir="/scratch4/NCEPDEV/marine/$USER"
exec_dir="$home_dir/rtofs.prod.v2.5/rtofs-tools/ncoda_graph/bin"
run="rtofs.prod.v2.5"
expt="v2.5"
expt_dir="$home_dir/$run"
bin_dir="$home_dir/bin"

# Input from parent script (month_stats.sh passes RD_short as $1)
RD=$1 

if [[ -z "$RD" ]]; then
    echo "Usage: graph.sh YYYYMMDD"
    exit 1
fi

log_dir="$expt_dir/graph/glbl_out"
log_dir2="$expt_dir/ncoda/logs/hycom_var"

export OCN_OUTPUT_DIR="/scratch4/NCEPDEV/marine/Zulema.Garraffo/rtofs_da/ncoda_new/restart"
export OCN_CLIM_DIR="/scratch4/NCEPDEV/marine/Zulema.Garraffo/ncoda/fix/codaclim"

# Calculate analysis date (24h before RD) using the correct bin
ddtg=$($bin_dir/rtofs_dtg "${RD}00" -d -1)

mkdir -p "$log_dir"
cd "$log_dir"

# --- Run NCODA Diagnostics ---
cat << EOF > omapnl
 &omapnl
  btm_max       = 2500.,
  do_ice_inc    = .false.,
  do_tmp_inc    = .false.,
  do_sal_inc    = .false.,
  do_vfy        = .true.,
  do_sla        = .false.,
  do_stats      = .true.,
  do_stats_fcst = .true.,
  do_stats_lvl  = .true.,
  dtg1          = $ddtg,
  n_plot        = 6,
  z_plot        = 1, 10, 13, 18, 32, 36,
 &end
EOF

# --- Run NCODA Mapping (The core binary) ---
$exec_dir/rtofs_ncoda_map "$ddtg" > gout

# --- Verification: Check if the NCAR Graphics gmeta was actually created
if [[ ! -s "gmeta" ]]; then
    echo "ERROR: rtofs_ncoda_map produced an empty or missing gmeta for $ddtg"
    exit 1
fi

# --- Move outputs to the archive log directory
mv gmeta "$log_dir/hycom_var_graph_verif_Global.$ddtg.gmeta"
mv gout  "$log_dir/hycom_var_graph_verif_Global.$ddtg.out"
rm -f omapnl

# --- Graphics Conversion ---
cd "$log_dir"
# Call the conversion script from your local bin
if [[ -f "$bin_dir/gmeta2png.sh" ]]; then
    sh "$bin_dir/gmeta2png.sh" "$RD"
else
    echo "ERROR: $bin_dir/gmeta2png.sh not found!"
    exit 1
fi

# --- Final Deployment & Sync ---
out_folder="${RD}_${expt}_ncoda"
mkdir -p "$out_folder"

# Point to where gmeta2png.sh actually created the PNGs
# Based on the gmeta2png structure:
png_src="$log_dir2/png_${ddtg}"

if [[ -d "$png_src" ]]; then
    echo ">>> Found PNGs in $png_src. Deploying..."
    chmod -R a+rX "$png_src"
    
    # Copy files from the source to our local out_folder
    cp -p "$png_src"/* "$out_folder/"
    
    # Mirror results to the final persistent log directory
    mkdir -p "$log_dir2"
    cp -pr "$out_folder" "$log_dir2/"
    
    echo ">>> Success! Results synced to: $log_dir2/$out_folder"
else
    echo "CRITICAL: Expected graphics directory $png_src was not found."
    # List contents of log_dir2 to help debug if it fails again
    echo "Contents of $log_dir2:"
    ls -F "$log_dir2"
    exit 1
fi

# --- Cleanup (Only if the copy was successful) ---
if [[ $? -eq 0 ]]; then
    echo ">>> Deployment successful. Cleaning up temp files..."
    rm -rf "$png_src" "$log_dir2/hycom_var.$ddtg"
else
    echo "WARNING: Deployment failed. Preserving temp files for debugging."
    exit 1
fi

exit 0
