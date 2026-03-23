#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=01:30:00
#SBATCH --account=marine-cpu
#SBATCH --job-name=NCODA_DIAGN
#SBATCH --output=joblog/NCODA_DIAGN.joblog
#SBATCH -q batch

# Load environment
source /scratch4/NCEPDEV/marine/$USER/bin/setup_env.sh

set -ex 

# --- Configuration ---
home_dir="/scratch4/NCEPDEV/marine/$USER"
exec_dir="$home_dir/rtofs.prod.v2.5/rtofs-tools/ncoda_graph/bin"
run="rtofs.prod.v2.5"
expt="v2.5"
expt_dir="$home_dir/$run"
bin_dir="$home_dir/bin"

dtg1=$1
ndays=$2

if [[ -z "$dtg1" || -z "$ndays" ]]; then
    echo "Usage: diagn.sh dtg1 ndays"
    exit 1
fi

log_dir="$expt_dir/graph/glbl_out"
log_dir2="$expt_dir/ncoda/logs/hycom_var"

mkdir -p "$log_dir"
cd "$log_dir"

# --- Run NCODA Diagnostics ---
cat << EOF > odiagnl
 &odiagnl
  do_vrfy = .true.,
  do_bias = .true.,
  title   = 'RTOFS v2.5',
 &end
EOF

diag_log="vrfy.${dtg1}.out"
$exec_dir/rtofs_ncoda_diagn "$dtg1" "$ndays" > "$diag_log"
rm -f odiagnl

# --- VERIFICATION CHECK ---
# Check if file exists and has a size greater than zero
if [[ ! -s "$diag_log" ]]; then
    echo "CRITICAL ERROR: $diag_log is missing or empty. rtofs_ncoda_diagn failed."
    exit 1
fi

# Optional: Check for a specific success keyword in the log
if ! grep -q "finished" "$diag_log" && ! grep -q "SUCCESS" "$diag_log"; then
    echo "WARNING: $diag_log does not contain 'finished' or 'SUCCESS'. Check for hidden errors."
    # Depending on how strict you want to be, you could exit here too
fi

# --- Calculate Final Date Labels ---
dtgl=$(rtofs_dtg "$dtg1" -d "$ndays")
dtgl=$(rtofs_dtg "$dtgl" -d -1)

RD_full=$(rtofs_dtg "$dtgl" -d 1)
RD=${RD_full:0:8}
out_folder="${RD}_${expt}_ncoda"

rm -rf "$out_folder"
mkdir -p "$out_folder"

# --- Process Graphics Loop ---
for case in rgn rgn_fcst; do
    [[ "$case" == "rgn" ]] && case2="GLBL" || case2="GLBL_fcst"
    gme="$case.${dtgl}.gmeta"

    if [[ -f "$gme" ]]; then
        ctrans -dev ps.color -font 20 -lscale 3 "$gme" > gmeta.ps
        psplit gmeta.ps
        "$bin_dir/pstopng2.sh" pict0001
        "$bin_dir/pstopng2.sh" pict0002
        mv pict0001.png "$out_folder/ArgoTemp_$case2.png"
        mv pict0002.png "$out_folder/ArgoSaln_$case2.png"
        rm -f pict* gmeta.ps
    else
        echo "Note: $gme not found. Skipping $case2 plots."
    fi
done

# --- Process Maps & Bias ---
# (Same logic as before, now safe because we verified the diagnostic ran)
map_gme="maps.$dtgl.gmeta"
if [[ -f "$map_gme" ]]; then
    ctrans -dev ps.color -font 20 -lscale 3 "$map_gme" > gmeta.ps
    psplit gmeta.ps
    "$bin_dir/pstopng2.sh" pict0001
    mv pict0001.png "$out_folder/map_ArgoTemp.png"
    rm -f pict* gmeta.ps
fi

bias_gme="bias.$dtgl.gmeta"
if [[ -f "$bias_gme" ]]; then
    ctrans -dev ps.color -font 20 -lscale 3 "$bias_gme" > gmeta.ps
    rm -f pic*ps pic*png
    psplit gmeta.ps
    declare -a bias_names=("bias_SST" "bias_SSS" "bias_NHicecov" "bias_SHicecov")
    for i in {0..3}; do
        fnum=$(printf "%04d" $((i+1)))
        "$bin_dir/pstopng2.sh" "pict$fnum"
        mv "pict$fnum.png" "$out_folder/${bias_names[$i]}.png"
    done
fi

# --- Cleanup and Deploy ---
rm -f *.ps *.gmeta
chmod -R a+rX "$out_folder"

mkdir -p "$log_dir2"
rm -rf "$log_dir2/$out_folder"
cp -pr "$out_folder" "$log_dir2/"

echo ">>> DIAGNOSTICS COMPLETE: Results in $log_dir2/$out_folder"
exit 0
