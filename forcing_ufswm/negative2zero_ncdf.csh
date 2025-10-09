module load nco

cd /scratch2/NCEPDEV/marine/Zulema.Garraffo/FV3_RT/DATM_INPUT
setenv F GFS.2025032000-2025050918

#ncap2 -s 'where(x<100.) x=100;' file.nc -O file2.nc
ncap2 -s 'where(fprecp<0.) fprecp=0.;' ${F}.nc -O ${F}_positive.nc

