#! /bin/csh -x
#-- hyun-chul.lee@noaa.gov

#module load NetCDF/4.5.0
#module load cdo/1.9.8
#module purge
#module load gnu/13.2.0 intel/2023.2.0 netcdf/4.7.0 wgrib2/3.1.2_ncep cdo/2.3.0
module list
which wgrib2
set echo
#set hdir = /scratch2/NCEPDEV/stmp1/Zulema.Garraffo/DATM
#set hperm = /scratch4/NCEPDEV/marine/Zulema.Garraffo/FV3_RT/forcing
set hdir = `echo $PWD`/DATM
set sdir = ${hdir}/Reanal
set tdir = ${hdir}/GFS2DATM
set wdir = ${hdir}/Work
#set grb2 = /apps/wgrib2/3.1.2/gnu_13.2.0/ncep/bin/wgrib2


#set symd = $1
#set eymd = $2
set symd = 20250331
set eymd = 20250401

echo $symd $eymd

if (! -d $wdir) mkdir -p $wdir
if (! -d $tdir) mkdir -p $tdir
cd $wdir

#/bin/cp -f ${hperm}/conv_gfs2datm_long_beta5_hCk conv_gfs2datm_long_beta5_hCk

set ymd = $symd
while ($ymd <= $eymd)
  foreach hh (00 06 12 18)
    set ymdh = ${ymd}${hh}
    echo ${ymdh}
    rm -f gfs_input*.nc
    #    ${grb2} ${sdir}/gfs.${ymd}.t${hh}z.sfcanl.grib2 -netcdf gfs_input1.nc
    #${grb2} ${sdir}/gfs.${ymd}.t${hh}z.puvflx.grib2 -netcdf gfs_input2.nc
    wgrib2 ${sdir}/gfs.${ymd}.t${hh}z.sfcanl.grib2 -netcdf gfs_input1.nc
    wgrib2 ${sdir}/gfs.${ymd}.t${hh}z.puvflx.grib2 -netcdf gfs_input2.nc
    cp ${sdir}/gfs.${ymd}.t${hh}z.atmf000.delz1.nc gfs_input3.nc
    cdo merge gfs_input1.nc gfs_input2.nc gfs_input3.nc gfs_input.nc

    echo ${ymdh}
     ./conv_gfs2datm_long_beta5_hCk

ncdump gfs_output.nc | sed -e "5s#^.time = 1 ;#time = UNLIMITED ; // (1 currently)#" | ncgen -o gfs_output2.nc
#    mv -f gfs_output2.nc gfs_output.nc
    /bin/mv gfs_output2.nc ${tdir}/gfs.${ymdh}.nc
 
   rm -f gfs_input.* gfs_output*
     end
       set ymd = `date -d "${ymd} 1 day" +%Y%m%d`
       end
#
