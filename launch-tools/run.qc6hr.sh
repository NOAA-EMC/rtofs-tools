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
chour=$($NDATE | cut -c9-10)
cyc=$(expr $chour / 6 \* 6)
if [ $cyc -le 9 ];then cyc=0$cyc;fi
COMDIR=$comroot/prod/com/rtofs/$vers
TMPDIR=/lfs/h2/emc/ptmp/dan.iredell/$simulation/NC-$rtofs_glo_ver
LOGDIR=$TMPDIR/logs
mkdir -p $LOGDIR
pid=$$

now=$(date +%Y%m%d_%H%M%S)
outputlog=$LOGDIR/submit.$cfg.sim.$now.$$.log
echo " " >> $outputlog
echo "Starting this job at $now" >> $outputlog

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

cd /lfs/h2/emc/eib/save/dan.iredell/tools
./ncoda_rtofs_glo.launch_2dvar.sh $cfg.config $TODAY$cyc



