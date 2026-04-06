#!/bin/sh
# 
# this script will clean the specified simulation for a specified day behind PDY
#

## Check if running on dev
h=$( hostname | cut -c1-1 )
case "$h" in
  c) host=cactus ;;
  d) host=dogwood ;;
  a) host=acorn ;;
  *) host=nobody
esac

#wcoss2
hprod=$(grep primary /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)
hdev=$(grep backup /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)

if [[ $host == $hdev ]]; then
  echo "$host is the dev machine - I will proceed"
elif [[ $host == $hprod ]]; then
  echo "prod machine - I will exit"
  exit
else
  echo "Unknown machine - I will exit"
  exit
fi

#modules
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel

if [ $# -ne 1 ]
then
  echo "USAGE: $0 <configname>"
  exit -2
fi
deletedaybehind=7

set -x

echo zero $0
echo base $(basename $0)
cfg=$1

# get config vars
. /lfs/h2/emc/eib/save/dan.iredell/tools/$cfg.config

# here is new stuff to determine version
. $projectroot/versions/run.ver
vers=$(echo $rtofs_glo_ver | cut -d. -f1-2)

TODAY=$(date +%Y%m%d)
let hours=deletedaybehind*24
PDYmD=$($NDATE -$hours ${TODAY}00 | cut -c1-8)

COMDIR=$comroot/prod/com/rtofs/$vers

echo details of $COMDIR/rtofs.PDYmD

ls $COMDIR/rtofs.$PDYmD | wc
du -sh $COMDIR/rtofs.$PDYmD

echo removing directory $COMDIR/rtofs.$PDYmD
rm -rf $COMDIR/rtofs.$PDYmD

