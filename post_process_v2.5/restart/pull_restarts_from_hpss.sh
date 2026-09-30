#!/bin/bash
set -euo pipefail

# ==============================================================================
# Helper Functions
# See RTOFS versions list:
# https://github.com/NOAA-EMC/RTOFS_GLO/issues/56#issuecomment-3422479383
# ==============================================================================
get_rtofs_version() {
    local t_date=$1
    if (( t_date <= 20220627 )); then
        echo "prod"
    elif (( t_date >= 20220628 && t_date <= 20220801 )); then
        echo "v2.2"
    elif (( t_date >= 20220802 && t_date <= 20240912 )); then
        echo "v2.3"
    elif (( t_date >= 20240913 && t_date <= 20250726 )); then
        echo "v2.4"
    else
        echo "v2.5"
    fi
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
    echo "FATAL: HPSS operations are not supported on this node."
    echo "Detected hostname: $HOSTNAME_F"
    echo "Please run this script from WCOSS2 (clogin*/dlogin*) or Ursa (ufe*)."
    exit 1
fi

# ==============================================================================
# 1. Input Validation & Directory Setup
# ==============================================================================
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 YYYYMMDD OUTPUT_DIR"
    echo "Example: $0 20260901 /path/to/download/area"
    exit 1
fi

TARGET_DATE=$1
OUTPUT_DIR=$2

if ! [[ "$TARGET_DATE" =~ ^[0-9]{8}$ ]]; then
    echo "ERROR: Date must be in YYYYMMDD format."
    exit 1
fi

if [ ! -d "$OUTPUT_DIR" ]; then
    echo "Creating output directory: $OUTPUT_DIR"
    mkdir -p "$OUTPUT_DIR" || { echo "FATAL: Failed to create $OUTPUT_DIR"; exit 1; }
fi

if [ ! -w "$OUTPUT_DIR" ]; then
    echo "FATAL: No write permissions for $OUTPUT_DIR"
    exit 1
fi

OUTPUT_DIR=$(readlink -f "$OUTPUT_DIR")
YYYY=${TARGET_DATE:0:4}
YYYYMM=${TARGET_DATE:0:6}

# ==============================================================================
# 2. Determine RTOFS Version
# ==============================================================================
VERSION=$(get_rtofs_version "$TARGET_DATE")

# ==============================================================================
# 3. Construct HPSS Path
# ==============================================================================
TAR_FILE="/NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYYMM}/${TARGET_DATE}/com_rtofs_${VERSION}_rtofs.${TARGET_DATE}.restart.tar"

echo "=================================================="
echo "Machine Detected: $MACHINE_ID ($HOSTNAME_F)"
echo "Target Date     : $TARGET_DATE"
echo "RTOFS Version   : $VERSION"
echo "Archive Path    : $TAR_FILE"
echo "Output Directory: $OUTPUT_DIR"
echo "=================================================="

# ==============================================================================
# 4. Extract & Verify Specific n00 Restart Files
# ==============================================================================
FILE_A="rtofs_glo.t00z.n00.restart.a.tgz"
FILE_B="rtofs_glo.t00z.n00.restart.b"
FILE_CICE="rtofs_glo.t00z.n00.restart_cice.tgz"

cd "$OUTPUT_DIR"

echo "Initiating htar extraction for n00 files..."
htar -xvf "$TAR_FILE" \
    "./$FILE_A" \
    "./$FILE_B" \
    "./$FILE_CICE"

if [ $? -ne 0 ]; then
    echo "ERROR: htar extraction failed. Ensure you have valid HPSS credentials."
    exit 1
fi

echo "Verifying file integrities in $OUTPUT_DIR..."
MISSING_OR_EMPTY=0

for FILE in "$FILE_A" "$FILE_B" "$FILE_CICE"; do
    if [ ! -s "$FILE" ]; then
        echo "ERROR: $FILE is missing or 0 bytes!"
        MISSING_OR_EMPTY=1
    else
        echo " - Verified: $FILE ($(du -h "$FILE" | cut -f1))"
    fi
done

if [ $MISSING_OR_EMPTY -ne 0 ]; then
    echo "FATAL: One or more restart files failed to extract properly."
    exit 1
fi

# ==============================================================================
# 5. Unpack and Cleanup
# ==============================================================================
echo "=================================================="
echo "Unpacking nested tarballs..."
echo "=================================================="

tar -xzf "$FILE_A"
if [ $? -ne 0 ]; then echo "FATAL: Failed to unpack $FILE_A"; exit 1; fi

tar -xzf "$FILE_CICE"
if [ $? -ne 0 ]; then echo "FATAL: Failed to unpack $FILE_CICE"; exit 1; fi

echo "Cleaning up raw .tgz files to save space..."
rm -f "$FILE_A" "$FILE_CICE"

echo "SUCCESS: Raw HYCOM (.a/.b) and CICE4 binaries are ready in $OUTPUT_DIR."
