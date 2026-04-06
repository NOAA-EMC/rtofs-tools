#!/bin/sh

######################################################################################

## Check if running on prod
h=$( hostname | cut -c1-1 )
case "$h" in
  s) host=surge ;;
  l) host=luna  ;;
  m) host=mars  ;;
  v) host=venus ;;
  c) host=cactus ;;
  d) host=dogwood ;;
  *) host=nobody
esac

#wcoss2
hprod=$(grep primary /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)
hdev=$(grep backup /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)

if [[ $host == $hdev ]]; then
  echo "$host is the dev machine - I will exit"
  echo "just kidding - I will proceed"
elif [[ $host == $hprod ]]; then
  echo "$host is the prod machine - I will proceed"
else
  echo "Unknown machine - I will exit"
  exit
fi

#wcoss2
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel

########################################################################################

#input - if no command line input then look back starting with yesterday
PDY=`$NDATE | cut -c1-8`00
pdy=`$NDATE | cut -c1-8`

if [ $# -eq 1 ]
then
  pdy=$1
  PDY=${pdy}00
fi
bdir=/lfs/h1/ops/prod/dcom
# current HPSS location 
hdir=/NCEPDEV/emc-ocean/5year/Dan.Iredell/dcom

tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/archive
mkdir -p $tmpdir

cat << eofn > $tmpdir/dcom_monthly.$pdy.$$
#!/bin/bash
#PBS -N jdcom_monthly
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q dev_transfer
#PBS -l walltime=06:00:00
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

COMOUT=$bdir
cd \$COMOUT

for mdir in \$(ls -d \$COMOUT/20????)
do 
  yyyymm=\$(basename \$mdir)
  htar -cvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/dcom/\$yyyymm.tar \$yyyymm/*
done

eofn

cd $tmpdir
qsub ./dcom_monthly.$pdy.$$


