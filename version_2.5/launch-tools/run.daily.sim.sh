#!/bin/sh
# 
# this script will submit the specified simulation for today
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

if [ $# -eq 0 ]
then
  echo "USAGE: $0 <configname> [<enddate>]"
  exit -2
fi

set -x

echo zero $0
echo base $(basename $0)
cfg=$1
enddate=20260405
if [ $# -gt 1 ]
then
  enddate=$2
fi

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

#if we are at enddate then do not run
if [ $TODAY -ge $enddate ]
then
   echo "Simulation $cfg has reached the enddate of $enddate -- no more cycles"
   exit -3
fi

now=$(date +%Y%m%d_%H%M%S)
outputlog=$LOGDIR/submit.$cfg.sim.$now.$$.log
echo " " >> $outputlog
echo "Starting this job at $now" >> $outputlog

# check if yesterday's run ran OK (populated with enough arch files)
afiles=$(ls $COMDIR/rtofs.$PDYm1/rtofs_glo*arch*.? | wc -l)
echo Checking number of archive files in $COMDIR/rtofs.$PDYm1
if [ $afiles -lt 100 ]
then
  echo Yesterdays run seems likes it is incomplete.
  echo Not submitting job. Disabling cron.
#  crontab -l | sed '/^[^#]'*run.daily.sim.sh'/s/^/#/' | crontab
  echo hm, there are multiple run.daily.sim strings in the crontab so not disabling it
  exit
fi

# if comdir for this day exists, then move it aside
echo Checking if $COMDIR/rtofs.$TODAY exists
if [ -d $COMDIR/rtofs.$TODAY ]
then
  mv $COMDIR/rtofs.$TODAY $COMDIR/rtofs.$TODAY.$pid
fi

# if tmpdir for this day exists, then move it aside
echo Checking if $TMPDIR/$TODAY exists
if [ -d $TMPDIR/$TODAY ]
then
  mv $TMPDIR/$TODAY $TMPDIR/$TODAY.$pid
fi

# check couple/noscrub usage
echo Checking /lfs/h2/emc/couple/noscrub usage
pct=$(/usr/local/bin/lsquota | grep couple/noscrub | awk  '{printf ("%i\n", $2)}')
if [ $pct -ge 95 ]
then
  echo couple noscrub over 95% full.
  echo Not submitting job. Disabling cron.
#  crontab -l | sed '/^[^#]'*run.daily.sim.sh'/s/^/#/' | crontab
  echo hmm, there are multiple run.daily.sim strings in the crontab so not disabling it
  exit
fi

# now ready to submit job
echo Submitting simulation for $simulation on $TODAY using $cfg.config >> $outputlog
cd /lfs/h2/emc/eib/save/dan.iredell/tools

#if [[ $cfg == rtofs.ALL || $cfg == release.v2.5.0 ]]
#then
  ./ncoda_rtofs_glo.launch.ALL.sh $cfg.config $TODAY
#else
#  ./ncoda_rtofs_glo.launch.sh $cfg.config $TODAY
#fi




