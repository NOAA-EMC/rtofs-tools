#!/bin/bash

# prereq: conda environment with copernicusmarine toolbox installed
# and credential saved to file:

# conda create --name copernicus --python=3.13
# conda activate copernicus
# conda install conda-forge::copernicusmarine --yes
# copernicusmarine login
# INFO - 2026-05-27T13:28:59Z - Downloading Copernicus Marine data requires a Copernicus Marine username and password, sign up for free at: https://data.marine.copernicus.eu/register
#Copernicus Marine username: ***********
#Copernicus Marine password: *******************
# INFO - 2026-05-27T13:30:44Z - Credentials file stored in ~/.copernicusmarine/.copernicusmarine

# --- download OSTIA ---
datasetID="cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D"

# write a text file with all files to download, to commit with script
# useful for keeping track when we update dataset
copernicusmarine get -i $datasetID --create-file-list files_to_download.txt

# separate requests for each month
for year in $( seq 2020 2025 ) ; do
  for month in $( seq -f %02g 1 12 ) ; do
    copernicusmarine get -i $datasetID --filter "*/$year/$month/*" -nd
  done
done

# move files to final location (URSA)
mv *.nc /scratch3/NCEPDEV/marine/Raphael.Dussin/obs/gridded/SSH_CMEMS/my/.
