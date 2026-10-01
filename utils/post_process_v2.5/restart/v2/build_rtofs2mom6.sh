#!/bin/bash
# Automatically detects the platform, loads the correct environment, and compiles rtofs2mom6

# Stop execution if any command fails
set -e

# Change to the directory where this script is located
cd "$(dirname "$0")"

echo "----------------------------------------"
echo "Initializing rtofs2mom6 Build Process"
echo "----------------------------------------"

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

echo "Detected platform: ${MACHINE_ID^^}"

# Point to our custom modulefiles folder
module use "$(pwd)/modulefiles"

# Load the appropriate environment based on the machine
if [[ "$MACHINE_ID" == "wcoss2" ]]; then
    echo "Loading WCOSS2 (Cray/Intel) environment..."
    module load ufs_wcoss2.intel
elif [[ "$MACHINE_ID" == "ursa" ]]; then
    echo "Loading Ursa (Spack Stack/Intel) environment..."
    module load ufs_ursa.intel
fi

echo "Loaded modules: "
module list
echo " "
echo " "

# Create a clean build directory and compile
echo "Configuring CMake..."
mkdir -p build
cd build

# Clear out any old cached build files just in case
rm -rf *

cmake ..
echo "Compiling..."
make -j 4

echo "----------------------------------------"
echo "SUCCESS! Executable is ready at: $(pwd)/rtofs2mom6"
echo "----------------------------------------"
