#!/bin/bash

# Detect the machine name
machine_name=$(hostname)
echo ${machine_name}
mod_path="/home/Santha.Akella/modulefiles/"

# Capture the current physical directory
current_dir=$(pwd -P)

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
# Use the captured current directory instead of the hardcoded path
    echo ">>> Changing directory to: $current_dir"
    cd "$current_dir"
    
    echo ">>> Starting build: make ibm_intel"
    make ibm_intel
fi
