#!/bin/bash
# this script pulls the required RTOFS data for PDYm1
# - nc and grb files
#

if [ $# -eq 1 ]
then
export PDY=$1
else
echo USAGE: $0 PDY
exit -2
fi

# account=${account:-hurricane}
# OUTDIR=${OUTDIR:-/scratch2/NCEPDEV/$GROUP/$USER/COMDIR/com/rtofs/prod}

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel
module load intel

PDYm1=`$NDATE \` expr -1 \* 24 \` ${PDY}00 | cut -c1-8`
COMOUT=$OUTDIR/rtofs.$PDY

echo PDY is $PDY
echo COMOUT is $OUTDIR/rtofs.$PDY

mkdir -p $COMOUT/ncoda
mkdir -p $TMPDIR/stage.$PDY
cd $TMPDIR/stage.$PDY

#
# Submit transfer job to pull netcdf and grib2 files
cat << eofA > $TMPDIR/stage.$PDY/pull.ncgrb.stuff.sh
#!/bin/bash
#PBS -N Npull_ncgrb
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q dev_transfer
#PBS -l walltime=05:00:00
#PBS -l debug=true
#PBS -V

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel
module load intel
module list

set -x

pdy=$PDY
yyyy=`echo $PDY | cut -c1-4`
mm=`echo $PDY | cut -c5-6`

# get netcdf and grib files
mkdir -p $COMOUT
cd $COMOUT

# if production find version and set macros for ops
ntarball=rtofs.\$pdy/rtofs.ncgrb.tar
gtarball=
if [ $PRODUCTION == 1 ]
then
  vers=\$(hsi -P ls /NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/ | grep rtofs.\${pdy}.ncoda.tar.idx | cut -d_ -f3)
  hdir=/NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy
  ntarball=com_rtofs_\${vers}_rtofs.\$pdy.nc.tar
  gtarball=com_rtofs_\${vers}_rtofs.\$pdy.grb2.tar
fi

# get netcdf files
htar -xvf \$hdir/\$ntarball
# get grib2 files (if prod)
# if [ $gtarball != "" ];then
# htar -xvf \$hdir/\$gtarball
# fi

eofA

qsub $TMPDIR/stage.$PDY/pull.ncgrb.stuff.sh

exit

