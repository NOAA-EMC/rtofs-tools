#!/bin/bash

# Detect the machine name
machine_name=$(hostname)
echo ${machine_name}
mod_path="/home/Santha.Akella/modulefiles/"

# Check if the machine name starts with "ufe"
if [[ $machine_name == ufe* ]]; then
    echo ">>> Detected Machine: $machine_name"
    
    # Check if the module directory actually exists
    if [ -d "$mod_path" ]; then
        echo ">>> Found module directory: $mod_path"
        
        # Ensure 'module' command is available (common bash function check)
        if [ -n "$(type -t module)" ]; then
            module use "$mod_path"
            module load hafs.ursa 
            module load bufr
            module load pyncl
            module load imagemagick
            
            echo ">>> Modules loaded successfully:"
            module list
        else
            echo "Error: 'module' command not found in this shell."
        fi
    else
        echo "Warning: Module directory $mod_path not found. Skipping loads."
    fi
else
    echo ">>> Machine $machine_name does not match 'ufe'. No modules loaded."
fi

# -- Your build/execution commands go here --

if [[ $machine_name == ufe* ]]; then
  cd /scratch4/NCEPDEV/marine/Santha.Akella/rtofs.prod.v2.5/rtofs-tools/ncoda_graph/
  make ibm_intel 
fi
