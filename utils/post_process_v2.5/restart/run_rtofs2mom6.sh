#!/bin/bash
# Wrapper script to run the RTOFS to MOM6 restart conversion

set -e
cd "$(dirname "$0")"

echo "----------------------------------------"
echo "Initializing rtofs2mom6 Conversion"
echo "----------------------------------------"

# --- 1. Global Configurations & Dimensions ---
if [ -z "$1" ]; then
    echo "ERROR: Please provide a date string."
    echo "Usage: ./run_rtofs2mom6.sh YYYYMMDD[HH]"
    exit 1
fi
DATE="$1"

# Grid Parameters (Handled as Fortran parameters internally, logged here for reference)
RTOFS_IDM=4500
RTOFS_JDM=3298
RTOFS_KDM=41

# --- 2. Machine Detection & Define Paths ---
HOSTNAME_F=$(hostname -f)
MACHINE_ID="UNKNOWN"

case $HOSTNAME_F in
    clogin*|dlogin*) 
        MACHINE_ID="wcoss2" 
        FIXDIR="/lfs/h1/ops/prod/packages/rtofs.v2.5.5/fix/"
        EXEC_PATH="/lfs/h2/emc/couple/save/santha.akella/rtofs_tools_sa_21Sep2026/utils/post_process_v2.5/restart/build/"
        MOM6_IC_DIR="/lfs/h2/emc/couple/noscrub/santha.akella/data/restart/zg"
        PTMP="/lfs/h2/emc/ptmp/santha.akella/restarts"
        ;;
    ufe*)            
        MACHINE_ID="ursa" 
        FIXDIR="" # Fix it.
        MOM6_IC_DIR="/scratch5/NCEPDEV/rstprod/Santha.Akella/data/restart/zg"
        PTMP="/scratch5/NCEPDEV/rstprod/Santha.Akella/ptmp/restarts"
        ;;
esac

if [[ "$MACHINE_ID" == "UNKNOWN" ]]; then
    echo "FATAL: This pipeline is only supported on WCOSS2 or Ursa."
    exit 1
fi

echo "Detected Platform: ${MACHINE_ID^^}"
echo "MOM6 Templates : ${MOM6_IC_DIR}"

# --- 3. Module Load ---
module use "$(pwd)/modulefiles"
if [[ "$MACHINE_ID" == "wcoss2" ]]; then
    module load ufs_wcoss2.intel
elif [[ "$MACHINE_ID" == "ursa" ]]; then
    module load ufs_ursa.intel
fi

# --- 4. Resolve Final Target Paths ---
EXEC="${EXEC_PATH}/rtofs2mom6"
INDIR="${PTMP}/in/${DATE:0:8}"
OUTDIR="${PTMP}/out/${DATE}"

# --- 5. Output Directory Management ("Drop Dead") ---
if [ -d "${OUTDIR}" ]; then
    echo "FATAL: Output directory already exists: ${OUTDIR}"
    echo "To prevent accidental overwrites, please wipe it manually (rm -rf ${OUTDIR}) before proceeding."
    exit 1
fi
mkdir -p "${OUTDIR}"

# --- 6. Pre-Run Sanity Checks ---
echo "Performing pre-run sanity checks..."
REQUIRED_FILES=("${EXEC}" "${INDIR}/rtofs_glo.t00z.n00.restart.a" "${INDIR}/rtofs_glo.t00z.n00.restart.b")

for f in "${REQUIRED_FILES[@]}"; do
    if [ ! -s "$f" ]; then
        echo "FATAL: Required file is missing or zero-size: $f"
        exit 1
    fi
done

# --- 7. Prepare MOM6 Restart Base (17 files) ---
echo ">>> Copying Initial Conditions to Output Directory..."
for ic_file in "${MOM6_IC_DIR}/"*MOM.res*.nc; do
    if [[ -f "$ic_file" ]]; then
        base_name=$(basename "$ic_file")
        target_name=${base_name#*MOM.res}
        cp -f "$ic_file" "${OUTDIR}/MOM.res${target_name}"
    fi
done
echo "All template files copied and renamed."

# --- 8. Execute the Conversion ---
echo "Starting Fortran conversion for ${DATE}..."
${EXEC} \
    --in "${INDIR}/rtofs_glo.t00z.n00.restart.a" \
    --depth $FIXDIR/rtofs_glo.navy_0.08.regional.depth.a \
    --outdir "${OUTDIR}" \
    --date "${DATE}"

# --- 9. Post-Run Sanity Checks ---
echo "Performing post-run sanity checks..."
for f in "${OUTDIR}/"*MOM.res*.nc; do
    if [[ ! -s "$f" ]]; then
        echo "FATAL: Restart file is missing or zero-size: $f"
        exit 1
    fi
done

echo "----------------------------------------"
echo "SUCCESS: Conversion Complete!"
echo "Location: ${OUTDIR}/"
echo "----------------------------------------"
