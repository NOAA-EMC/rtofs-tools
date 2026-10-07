#!/bin/bash
set -x

# ==============================================================================
# 0. Argument Parsing, Directory Setup & Machine Detection
# ==============================================================================
if [ "$#" -ne 2 ]; then
    echo "FATAL: Missing required arguments."
    echo "Usage: $0 <YYYYMMDD> <BASEDIR>"
    echo "Example: $0 20261006 /lfs/h2/emc/stmp/santha.akella"
    exit 1
fi
INPUT_DATE=$1
# Use readlink to ensure we get an absolute path just in case you pass a relative one
BASEDIR=$(readlink -f "$2") 

# Automatically detect the directory where this script is located
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

HOSTNAME_F=$(hostname -f)
MACHINE_ID="UNKNOWN"

case $HOSTNAME_F in
    clogin*|dlogin*) MACHINE_ID="wcoss2" ;;
    ufe*)            MACHINE_ID="ursa" ;;
esac

if [[ "$MACHINE_ID" == "UNKNOWN" ]]; then
    echo "FATAL: This pipeline is only supported on WCOSS2 (clogin/dlogin) or Ursa (ufe)."
    echo "Detected hostname: $HOSTNAME_F"
    exit 1
fi

# ==============================================================================
# 1. Machine-Specific Environment & Paths
# ==============================================================================
if [[ "$MACHINE_ID" == "wcoss2" ]]; then
    module load intel netcdf
    PROD_DIR="/lfs/h1/ops/prod/packages/rtofs.v2.5.5"
    
elif [[ "$MACHINE_ID" == "ursa" ]]; then
    # TODO: Add Ursa-specific module loads here
    # module load intel netcdf
    
    # TODO: Add Ursa-specific production path here
    PROD_DIR="/path/to/ursa/production/rtofs" 
    
    echo "FATAL: Ursa modules and PROD_DIR are not yet configured in this script. Please update them!"
    exit 1
fi

# Set dynamic local paths based on the user-provided BASEDIR and INPUT_DATE
OUTDIR="${BASEDIR}/rtofs_archv2ncdf3z_${INPUT_DATE}"

# If the output directory for this date already exists, purge it completely
if [ -d "${OUTDIR}" ]; then
    echo "Purging existing output directory: ${OUTDIR}"
    rm -rf "${OUTDIR}"
fi
mkdir -p "${OUTDIR}"

PROD_FIX="${PROD_DIR}/fix"
PROD_PARM="${PROD_DIR}/parm"
# Note: To use the default 33-level production template, you could do:
# TEMPLATE=${PROD_PARM}/rtofs_glo.navy_0.08.archv_full.in
PROD_EXEC="${PROD_DIR}/exec"

EXEC="${PROD_EXEC}/rtofs_archv2ncdf3z"

# Safely point to the template file in the SAME directory as this bash script
TEMPLATE="${SCRIPT_DIR}/rtofs_glo_glorys_depths.in"

echo "*** Started script $(basename $0) for date ${INPUT_DATE} at $(date) on $MACHINE_ID"
echo "*** Using BASEDIR (data): ${BASEDIR}"
echo "*** Using SCRIPT_DIR (code): ${SCRIPT_DIR}"
echo "*** Output will be saved to: ${OUTDIR}"

# ==============================================================================
# 2. Main Processing Loop
# ==============================================================================
# The FIVE variables we want to extract to 3D Z-levels (including interface depth)
VARS="uvl vvl tem sal inf"

for fnam in $VARS; do
    echo "=================================================="
    echo "Processing $fnam..."
    echo "=================================================="
    
    # 2.1 Create a dedicated workspace for this variable
    WORK_DIR=${OUTDIR}/${fnam}
    
    if [ -d "${WORK_DIR}" ]; then
        rm -rf "${WORK_DIR}"
    fi
    mkdir -p "${WORK_DIR}"
    cd "${WORK_DIR}"
    
    # 2.2 Symlink the input files directly from the production fix/ directory
    ln -sf ${PROD_FIX}/rtofs_glo.navy_0.08.regional.depth.a ./regional.depth.a
    ln -sf ${PROD_FIX}/rtofs_glo.navy_0.08.regional.depth.b ./regional.depth.b
    ln -sf ${PROD_FIX}/rtofs_glo.navy_0.08.regional.grid.a  ./regional.grid.a
    ln -sf ${PROD_FIX}/rtofs_glo.navy_0.08.regional.grid.b  ./regional.grid.b
    
    # Symlink the local archive files directly from your inputs/ directory using the new date variable
    ln -sf ${BASEDIR}/inputs/rtofs.${INPUT_DATE}/rtofs_glo.t00z.n00.archv.a ./archv.a
    ln -sf ${BASEDIR}/inputs/rtofs.${INPUT_DATE}/rtofs_glo.t00z.n00.archv.b ./archv.b

    # 2.3 Sanity check: Ensure all required inputs exist and are > 0 bytes
    REQUIRED_FILES=(
        "regional.depth.a"
        "regional.depth.b"
        "regional.grid.a"
        "regional.grid.b"
        "archv.a"
        "archv.b"
        "${TEMPLATE}"
        "${EXEC}"
    )

    for req_file in "${REQUIRED_FILES[@]}"; do
        if [ ! -s "${req_file}" ]; then
            echo "ERROR: Required input file '${req_file}' is missing, empty, or a broken symlink."
            exit 1
        fi
    done
    echo "Sanity check passed: All input files are present and valid."

    # 2.4 Assign Fortran I/O unit numbers AND the required CDFxxx environment variables
    uvl1=0; vvl1=0; tem1=0; sal1=0; inf1=0
    case $fnam in
        uvl) 
            uvl1=33; export CDF033="3zuio.nc" ;;
        vvl) 
            vvl1=34; export CDF034="3zvio.nc" ;;
        tem) 
            tem1=35; export CDF035="3ztio.nc" ;;
        sal) 
            sal1=36; export CDF036="3zsio.nc" ;;
        inf) 
            inf1=37; export CDF037="3dhio.nc" ;;
    esac

    # 2.5 Generate the specific input parameter file via sed
    rm -f change_archv.sed
    echo "s/1uvl/$uvl1/g" >> change_archv.sed
    echo "s/1vvl/$vvl1/g" >> change_archv.sed
    echo "s/1tem/$tem1/g" >> change_archv.sed
    echo "s/1sal/$sal1/g" >> change_archv.sed
    echo "s/1inf/$inf1/g" >> change_archv.sed

    sed -f change_archv.sed ${TEMPLATE} > archv2ncdf3z_${fnam}.in

    # 2.6 Run the executable and capture logs
    echo "Executing rtofs_archv2ncdf3z for ${fnam}..."
    ${EXEC} < archv2ncdf3z_${fnam}.in > rtofs_archv2ncdf3z_${fnam}.log 2>&1
    
    # 2.7 Sanity check & Consolidate: Ensure valid NetCDF output was created and move it
    nc_files=$(find . -maxdepth 1 -name "*.nc" -type f -size +0c)
    
    if [ -z "$nc_files" ]; then
        echo "ERROR: No valid NetCDF output was generated for ${fnam} (missing or 0 bytes)."
        echo "Please check the log file: ${WORK_DIR}/rtofs_archv2ncdf3z_${fnam}.log"
        exit 1
    else
        echo "SUCCESS: Generated valid output for ${fnam}:"
        ls -lh *.nc
        echo "Moving NetCDF files to ${OUTDIR}/"
        mv *.nc "${OUTDIR}/"
    fi

    # Go back to the base directory before the next loop
    cd ${BASEDIR}
done

echo "*** Finished script $(basename $0) at $(date)"
exit 0
