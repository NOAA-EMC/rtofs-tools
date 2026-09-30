#!/bin/bash
set -euo pipefail

usage() {
    echo "Usage: $0 --rdate <YYYYMMDD> [--in < IN_FILE> --out <OUT_FILE> --tmp <TMP_FILE>] [optional_args]"
    echo "Example: $0 --rdate 20260901 --in restart_in --out restart_out --tmp restart_tmp"
    exit 1
}

# ==============================================================================
# 0. Self-Contained Machine Detection
# ==============================================================================
HOSTNAME_F=$(hostname -f)
MACHINE_ID="UNKNOWN"

case $HOSTNAME_F in
    clogin*|dlogin*) MACHINE_ID="wcoss2" ;;
    ufe*)            MACHINE_ID="ursa" ;;
    gaea*)           MACHINE_ID="gaea" ;;
esac

if [[ "$MACHINE_ID" == "UNKNOWN" ]]; then
    echo "FATAL: This pipeline is only supported on WCOSS2 (clogin/dlogin) or Ursa (ufe)."
    echo "Detected hostname: $HOSTNAME_F"
    exit 1
fi

# ==============================================================================
# 1. Inputs & Path Construction
# ==============================================================================
TARGET_DATE=""
INFILE=""
OUTFILE=""
TMPFILE=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --rdate)
      TARGET_DATE="$2"
      shift 2
      ;;
    --in)
      INFILE="$2"
      shift 2
      ;;
    --out)
      OUTFILE="$2"
      shift 2
      ;;
    --tmp)
      TMPFILE="$2"
      shift 2
      ;;
    -h|--help)
      usage
      ;;
    *)
      echo "ERROR: Unknown option: $1"
      usage
      ;;
  esac
done


if [[ -z "$TARGET_DATE" ]]; then
    echo "ERROR: --sdate is required"
    usage
fi

# Format Date for CICE6 file naming
YYYY=${TARGET_DATE:0:4}
MM=${TARGET_DATE:4:2}
DD=${TARGET_DATE:6:2}

if [[ -z "$INFILE" ]]; then
    INFILE=rtofs_glo.t00z.n00.restart_cice
fi

if [[ -z "$OUTFILE" ]]; then
    OUTFILE="iced.${YYYY}-${MM}-${DD}-00000.nc"
fi

# ==============================================================================
# 2. Environment Setup (Python & Modules)
# ==============================================================================
echo "=================================================="
echo "Machine Detected: $MACHINE_ID ($HOSTNAME_F)"
echo "Input File      : $INFILE"
echo "Output File     : $OUTFILE"
echo "Setting up Python Environment..."
echo "=================================================="

module unload python
module load python/3.11

PYPATH=/ncrc/home1/Dmitry.Dukhovskoy/miniconda3
eval "$($PYPATH/bin/conda shell.bash hook)"
conda activate anls


# Find where the script actually lives to locate the python script and venv
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
VENV_DIR="${SCRIPT_DIR}/rtofs_venv"
PYCODE="convert_cice4_to_cice6_restart.py"

#if [ ! -d "$VENV_DIR" ]; then
#    echo "Creating isolated virtual environment in $VENV_DIR..."
#    python3 -m venv "$VENV_DIR"
#    source "$VENV_DIR/bin/activate"
#    
#    echo "Installing required Python packages (numpy, netCDF4)..."
#    pip install --upgrade pip
#    pip install numpy netCDF4
#else
#    source "$VENV_DIR/bin/activate"
#fi

# ==============================================================================
# 3. Execution
# ==============================================================================
echo "Executing CICE conversion..."
python3 "${SCRIPT_DIR}/${PYCODE}" --infile "$INFILE" --outfile "$OUTFILE" --rdate "$TARGET_DATE"



