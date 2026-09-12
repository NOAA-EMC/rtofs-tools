#!/bin/csh

module purge
#module load nco/4.9.3
module load nco/5.1.6

#setenv Y 2023
#setenv Y2 2024
#setenv Y 2024
setenv Y 2025

cd /scratch2/NCEPDEV/stmp1/Zulema.Garraffo/DATM/GFS2DATM
set rec = 1
#foreach d(`ls -1 gfs.${Y}??????.nc gfs.${Y2}010100.nc`)
#foreach d(`ls -1 gfs.??????????.nc`)
foreach d(`ls -1 gfs.${Y}??????.nc`)
#foreach d(`ls -1 gfs.202[45]??????.nc`)
setenv f `basename ${d} .nc`
#/apps/nco/4.9.3/gnu/9.2.0/bin/ncks --mk_rec_dmn time ${d} ${f}_${rec}.nc
ncks --mk_rec_dmn time ${d} ${f}_${rec}.nc
echo ${f}
echo ${f}_${rec}
@ rec += 1
end
rm -f out.nc
#/apps/nco/4.9.3/gnu/9.2.0/bin/ncrcat gfs.??????????_*.nc out.nc
ncrcat gfs.??????????_*.nc out.nc
#rm -f out.nc
#ncra gfs.??????????_*.nc out.nc

#change negative precip to zero
#sh negative2zero_ncdf.csh

