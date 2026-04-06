#!/bin/sh
# 
# this script will submit the specified archive for today
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
PDYm1=$($NDATE -24 ${TODAY}00 | cut -c1-8)
COMDIR=$comroot/prod/com/rtofs/$vers
TMPDIR=/lfs/h2/emc/ptmp/dan.iredell/$simulation/NC-$rtofs_glo_ver
LOGDIR=$TMPDIR/logs
mkdir -p $LOGDIR
pid=$$

now=$(date +%Y%m%d_%H%M%S)
outputlog=$LOGDIR/submit.$cfg.archive.$now.$$.log
echo " " >> $outputlog
echo "Starting this job at $now" >> $outputlog

# check if todays run ran OK (populated with enough arch files)
echo Checking number of archive files in $COMDIR/rtofs.$PDYm1
afiles=$(ls $COMDIR/rtofs.$PDYm1/rtofs_glo*arch*.? | wc -l)
if [ $afiles -lt 100 ]
then
  echo Todays run seems likes it is incomplete.
  echo Not submitting job.
  exit
fi

# now ready to submit job
echo Submitting archive for $simulation $TODAY >> $outputlog
cd /lfs/h2/emc/eib/save/dan.iredell/tools/archive
./archive.RTOFS.wcoss2.releasebranch.sh $TODAY

