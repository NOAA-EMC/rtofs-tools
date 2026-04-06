#!/bin/bash
# This script populates the directory rtofs.$PDYm1 given $PDY
#
# Scripts called
# ./pull_lotsa_stuff.sh
# ./pull_and_create_hycom_var.sh
# ./pull_rtofs_archive_and_restart.sh
# ./pull_rtofs_ncgrb.sh
# 

runlotsa=0
runhycom=0
runarchv=1
runncgrb=0

# set directory where this simulation is being archived
# if a Production run, then set PRODUCTION=1 and some files pulled from there
#export hdir=/NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.paraD5b
export hdir=/NCEPDEV/emc-ocean/5year/Dan.Iredell/EMC.rtofs.v2.5.a
export PRODUCTION=0

if [ $# -eq 1 ]
then
  export PDY=$1
else
  echo USAGE: $0 YYYYMMDD
  exit -2
fi

#wcoss2
export OUTDIR=/lfs/h2/emc/couple/noscrub/dan.iredell/COMDIRL/prod/com/rtofs/v2.5/
export TMPDIR=/lfs/h2/emc/ptmp/dan.iredell/warmstart

#
# call script to pull NCODA ocnqc, nhem, shem, glbl files
if [ $runlotsa -eq 1 ]
then
  echo
  echo pull lotsa ncoda
  echo
  ./pull_lotsa_stuff.sh $PDY
fi

#
# call script to pull hycom_var data and recreate that directory
if [ $runhycom -eq 1 ]
then
  echo
  echo pull hycom_var
  echo
  ./pull_and_create_hycom_var.sh $PDY
fi

#
# call script to pull rtofs netcdf and grib2 files
if [ $runncgrb -eq 1 ]
then
  echo
  echo pull archive and restart
  echo
  ./pull_rtofs_ncgrb.sh $PDY
fi


#
# call script to pull rtofs archives and restart files and untar the the .tgz files
if [ $runarchv -eq 1 ]
then
  echo
  echo pull archive and restart
  echo
  ./pull_rtofs_archive_and_restart.sh $PDY
fi


