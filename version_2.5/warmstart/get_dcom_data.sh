#!/bin/bash
# this script pulls the required dcom data for rtofs 
# it is recommended that for rtofs=pdy that the data is
# pulled N days earlier 
#

if [ $# -eq 1 ]
then
  export PDY=$1
else
  echo USAGE: $0 YYYYMMDD
  exit -2
fi

#. ./user.config
export inputroot=/lfs/h2/emc/eib/noscrub/dan.iredell

# OUTDIR is the directory dcom/prod under inputroot

OUTDIR=$inputroot/dcom/prod
TMPDIR=/lfs/h2/emc/ptmp/dan.iredell/warmstart
#ln -rnsf $inputroot/dcom/prod $inputroot/dcom/us007003

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel
module load intel
#module list

echo PDY is $PDY
echo OUTDIR is $OUTDIR/$PDY

mkdir -p $OUTDIR/$PDY
mkdir -p $TMPDIR/stage.$PDY

# Submit service job to pull dcom data
cat << eofA > $TMPDIR/stage.$PDY/pull.dcom.sh
#!/bin/ksh -l
#PBS -N dcom.$PDY
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1:mem=8GB
#PBS -q dev_transfer
#PBS -l walltime=00:50:00
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
yyyymm=`echo $PDY | cut -c1-6`

cd $OUTDIR/\$pdy

# see if there is a version
#vers=\$(hsi -P ls /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy | grep dcom | grep \$pdy.tar | cut -d_ -f2)
#if [ \$vers -eq \$pdy ]
#then
#  vers=
#else
#  vers=\${vers}_
#fi

# the heck with it... 
ddir=/NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy
dfile=\$(hsi -P ls \$ddir/dcom* | head -2 | tail -1) 

d1=./sst/*
d2=./wtxtbul/satSSS/SMOS/*
d3=./wtxtbul/satSSS/SMAP/*
d4=./seaice/pda/*
d5=./wgrdbul/adt/*ncoda*
d6=./wgrdbul/ndbc/*hfr*
d7=./b001/xx001
d8=./b001/xx013
d9=./b001/xx101
da=./b001/xx102
db=./b001/xx103
dc=./b001/xx113
dd=./b021/xx201

echo ddir/dfile \$ddir/\$dfile
echo datasets "\$d1 \$d2 \$d3 \$d4 \$d5 \$d6 \$d7 \$d8 \$d9 \$da \$db \$dc \$dd "

htar -xvf \$ddir/\$dfile \$d1 \$d2 \$d3 \$d4 \$d5 \$d6 \$d7 \$d8 \$d9 \$da \$db \$dc \$dd

eofA

cd $TMPDIR/stage.$PDY
qsub pull.dcom.sh

echo check info in $0 for monthly info
exit

# monthly data info
yyyymm=`echo $PDY | cut -c1-6`
yyyy=`echo $PDY | cut -c1-4`
mm=`echo $PDY | cut -c5-6`
if [ $yyyymm -le 202103 ]
then
  monthdata=/gpfs/dell2/emc/obsproc/noscrub/Shelley.Melchior/JW/dcom_d10/$yyyy/$mm
else
  monthdata=/gpfs/dell2/emc/obsproc/noscrub/Shelley.Melchior/dcom_d10/$yyyy/$mm
fi
mkdir -p $OUTDIR/$yyyymm/b031
echo
echo \#################################################
echo
echo Monthly data for this day can be found on WCOSS2
echo  approx last 6 months - /lfs/h1/ops/prod/dcom/YYYYMM/b031
echo  archive - /lfs/h2/emc/obsproc/noscrub/ashley.stanfield/MarineArchive/dcom_d10/YYYY/MM/b031/
echo
echo if from archive, then they should be unzipped and mapped the following way:
echo
echo bathy.$yyyymm.dcom.gz   to  xx001
echo tesac.$yyyymm.dcom.gz   to  xx002
echo subpfl.$yyyymm.dcom.gz  to  xx005
echo xbtctd.$yyyymm.dcom.gz  to  xx006
echo
echo \#################################################
echo
exit

