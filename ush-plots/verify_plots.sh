#!/bin/bash

# Target directory is the first argument
TARGET_DIR=$1

# Define expected files
EXPECTED_FILES=(
    "ArgoTemp_GLBL.png"
    "ArgoSaln_GLBL.png"
    "ArgoTemp_GLBL_fcst.png"
    "ArgoSaln_GLBL_fcst.png"
    "map_ArgoTemp.png"
    "bias_SST.png"
    "bias_SSS.png"
    "bias_NHicecov.png"
    "bias_SHicecov.png"
)

echo "------------------------------------------------"
echo ">>> Verifying results in: $TARGET_DIR"
echo "------------------------------------------------"

if [[ ! -d "$TARGET_DIR" ]]; then
    echo "ERROR: Directory $TARGET_DIR does not exist."
    exit 1
fi

MISSING_COUNT=0
FOUND_COUNT=0

for FNAME in "${EXPECTED_FILES[@]}"; do
    FPATH="$TARGET_DIR/$FNAME"
    
    if [[ -f "$FPATH" ]]; then
        # Check if file size is greater than 0
        if [[ -s "$FPATH" ]]; then
            SIZE=$(du -h "$FPATH" | cut -f1)
            printf "  [OK]      %-25s (%s)\n" "$FNAME" "$SIZE"
            ((FOUND_COUNT++))
        else
            printf "  [EMPTY]   %-25s !! Zero byte file !!\n" "$FNAME"
            ((MISSING_COUNT++))
        fi
    else
        printf "  [MISSING] %-25s !! File not found !!\n" "$FNAME"
        ((MISSING_COUNT++))
    fi
done

echo "------------------------------------------------"
if [[ $MISSING_COUNT -eq 0 ]]; then
    echo "SUCCESS: All $FOUND_COUNT plots generated correctly."
    exit 0
else
    echo "FAILURE: $MISSING_COUNT files missing or corrupt."
    exit 1
fi
