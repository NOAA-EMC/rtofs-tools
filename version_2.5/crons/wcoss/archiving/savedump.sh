#!/bin/sh

######################################################################################

## Check if running on prod
h=$( hostname | cut -c1-1 )
case "$h" in
  t) host=tide  ;;
  g) host=gyre  ;;
  s) host=surge ;;
  l) host=luna  ;;
  c) host=cactus  ;;
  d) host=dogwood ;;
  *) host=nobody
esac

#wcoss2
hprod=$(grep primary /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)
hdev=$(grep backup /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)

if [[ $host == $hdev ]]; then
  echo "$host is the dev machine - I will exit"
elif [[ $host == $hprod ]]; then
  echo "$host is the prod machine - I will proceed"
  exit
else
  echo "Unknown machine - I will exit"
  exit
fi

#wcoss2
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel

set -x

pdy=`$NDATE | cut -c1-8`
if [ $# -eq 1 ]
then
  pdy=$1
fi

tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/archive
mkdir -p $tmpdir
cat << eofn > $tmpdir/ncoda_qc_dump.$pdy.$$
#!/bin/bash
#PBS -N jncoda_qc_dump
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

outdir=/lfs/h1/ops/prod/output
dirout=\`grep DIROUT \$outdir/$pdy/rtofs_global_ncoda_qc* | cut -d= -f2\`
tmpsav=/lfs/h2/emc/ptmp/$USER/save_ncoda_qc_dump
mkdir -p \$tmpsav/$pdy
cp -p \$dirout/* \$tmpsav/$pdy
dirm1=\`dirname \$dirout\`
cp -p \$dirm1/ice_nc/l2out.f285.51*.nc \$tmpsav/$pdy
cd \$tmpsav
htar -cvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/rtofs/ncoda/dump_${pdy}.tar $pdy/*

eofn

cd $tmpdir
qsub ./ncoda_qc_dump.$pdy.$$
