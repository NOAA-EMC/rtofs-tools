#!/bin/sh

######################################################################################

#wcoss2
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel

pdy=`$NDATE | cut -c1-8`
if [ $# -eq 1 ]
then
  pdy=$1
fi

tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/misc
mkdir -p $tmpdir
cat << eofn > $tmpdir/getforcing.$pdy.$$
#!/bin/bash
#PBS -N getf.$pdy
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1:mem=12GB
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

mkdir -p /lfs/h2/emc/eib/noscrub/dan.iredell/forcing/${pdy}
cd /lfs/h2/emc/eib/noscrub/dan.iredell/forcing/${pdy}

if [ $pdy -ge 20220530 ]
then
htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.prod/rtofs.${pdy}/rtofs.forcing.tar
fi

if [ $pdy -le 20220529 ]
then
htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.${pdy}/rtofs.forcing.tar
fi

eofn

cd $tmpdir
qsub ./getforcing.$pdy.$$
