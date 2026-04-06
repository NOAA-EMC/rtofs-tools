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
  exit
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
bdir=/lfs/h1/ops/prod/dcom/$pdy/wgrdbul
# current HPSS location 
hdir=/NCEPDEV/emc-ocean/5year/Dan.Iredell/navy_restarts

goodtogo=no
#check if files exist
if [[ -s $bdir/cice.restart.${PDY}_930.gz 
   && -s $bdir/restart_r${PDY}_930.a.gz
   && -s $bdir/restart_r${PDY}_930.b.gz ]]
then 
   goodtogo=yes
fi

if [ $goodtogo = no ]
then
   echo
   echo NO NAVY RESTARTS $bdir/cice.restart.${PDY}_930.gz
   echo NO NAVY RESTARTS $bdir/restart_r${PDY}_930.a.gz
   echo NO NAVY RESTARTS $bdir/restart_r${PDY}_930.b.gz
   echo
   exit -99
fi

tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/archive_navy_restart
mkdir -p $tmpdir

cat << eofn > $tmpdir/navy_restarts.$pdy.$$
#!/bin/bash
#PBS -N jnavy_restarts
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

COMOUT=$bdir

cd \$COMOUT

htar -cvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/navy_restarts/restarts.$pdy.tar cice.restart.* restart_r*

eofn

cd $tmpdir
qsub ./navy_restarts.$pdy.$$


