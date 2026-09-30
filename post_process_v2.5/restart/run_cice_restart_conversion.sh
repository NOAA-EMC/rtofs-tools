#!/bin/bash
set -euo pipefail

usage() {
    echo "Usage: $0 --rdate <YYYYMMDD> [--in <IN_FILE> --out <OUT_DIR> --tmp <TMP_FILE>]"
    echo "Example: $0 --rdate 20251215 --in restart_cice_in --out iced.2025-12-15-00000.nc --tmp iced.template.nc"
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

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
FYAML="${SCRIPT_DIR}/restart_cice6.yaml"
PYCODE="${SCRIPT_DIR}/convert_cice4_to_cice6_restart.py"

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
    echo "ERROR: --rdate is required"
    usage
fi

# Format Date for CICE6 file naming
YYYY=${TARGET_DATE:0:4}
MM=${TARGET_DATE:4:2}
DD=${TARGET_DATE:6:2}

if [[ -z "$INFILE" ]]; then
    INFILE="rtofs_glo.t00z.n00.restart_cice"
fi

if [[ -z "$OUTFILE" ]]; then
    OUTFILE="iced.${YYYY}-${MM}-${DD}-00000.nc"
fi

if [[ -z "$TMPFILE" ]]; then
    echo "ERROR: --tmp (template file) is required."
    usage
fi

# ==============================================================================
# 2. Environment & Pre-Flight Checks
# ==============================================================================
echo "=================================================="
echo "Machine Detected: $MACHINE_ID ($HOSTNAME_F)"
echo "Input File      : $INFILE"
echo "Output File     : $OUTFILE"
echo "Template File   : $TMPFILE"
echo "=================================================="

# Check Python Environment
echo "Verifying Python environment..."
if ! command -v python3 &> /dev/null; then
    echo "FATAL: 'python3' command not found. Please load a valid Python module/environment."
    exit 1
fi

if ! python3 -c "import numpy, netCDF4" &> /dev/null; then
    echo "FATAL: Required Python packages (numpy, netCDF4) are missing in the current environment."
    exit 1
fi
echo "Python environment looks good."

# Check Input and Template Files (exist and > 0 bytes)
if [[ ! -s "$INFILE" ]]; then
    echo "FATAL: Input file does not exist or is empty -> $INFILE"
    exit 1
fi

if [[ ! -s "$TMPFILE" ]]; then
    echo "FATAL: Template file does not exist or is empty -> $TMPFILE"
    exit 1
fi

if [[ ! -s "$FYAML" ]]; then
    echo "FATAL: YAML configuration file is missing -> $FYAML"
    exit 1
fi

# Ensure output directory exists
OUTDIR=$(dirname "$OUTFILE")
mkdir -p "$OUTDIR"

# ==============================================================================
# 3. Execution
# ==============================================================================
echo "Executing CICE conversion..."
python3 "$PYCODE" \
    --fyaml "$FYAML" \
    --machine "$MACHINE_ID" \
    --rdate "$TARGET_DATE" \
    --infile "$INFILE" \
    --outfile "$OUTFILE" \
    --tmpfile "$TMPFILE"

PY_STATUS=$?

# ==============================================================================
# 4. Post-Execution Validation
# ==============================================================================
if [[ $PY_STATUS -ne 0 ]]; then
    echo "FATAL: Python script failed with exit code $PY_STATUS"
    exit $PY_STATUS
fi

if [[ -s "$OUTFILE" ]]; then
    echo "=================================================="
    echo "SUCCESS: CICE4 --> CICE6 Restart conversion complete. Output file generated:"
    ls -lh "$OUTFILE"
    echo "=================================================="
else
    echo "FATAL: Conversion finished but output file is missing or empty -> $OUTFILE"
    exit 1
fi
