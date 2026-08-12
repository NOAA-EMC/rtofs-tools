#!/bin/sh
# 
# this script will submit next job for paraD
#

#wcoss2
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel

set -x

run=paraD4
sim=sim
cfg=g18
lastday=20230430

COMDIR=/lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR/prod/com/rtofs/v2.3
TMPDIR=/lfs/h2/emc/ptmp/dan.iredell/$sim/NC-v2.3.3
LOGDIR=$TMPDIR/logs
mkdir -p $LOGDIR
pid=$$

now=$(date +%Y%m%d_%H%M%S)
outputlog=$LOGDIR/submitparaD.$now.$$.log
echo " " >> $outputlog
echo "Starting this job at $now" >> $outputlog

# check if emergency stop flag is on
if [ -s $TMPDIR/emergency.stop ]
  then
  echo "We are stopping - emergency stop file exists" >> $outputlog
  exit -5
fi

# check couple/noscrub usage
echo Checking /lfs/h2/emc/couple/noscrub usage
pct=$(/usr/local/bin/lsquota | grep couple/noscrub | awk  '{printf ("%i\n", $2)}')
if [ $pct -ge 95 ]
then
  echo "couple noscrub over 95% full" >> $outputlog
  echo "Not submitting job" >> $outputlog
  exit -2
fi

# find what last day is running (or completed)
clastday=$(basename $(ls -1dtr $COMDIR/rtofs.2023???? | tail -1) | cut -d. -f2)
tlastday=$(basename $(ls -1dtr $TMPDIR/2023???? | tail -1) | cut -d. -f2)
if [ $clastday -eq $tlastday ] # good candidate
then
  PDYm1=$clastday
else
  echo "We are stopping - $clastday and $tlastday do not agree" >> $outputlog
  echo "clastday is $COMDIR/$clastday" >> $outputlog
  echo "tlastday is $TMPDIR/$tlastday" >> $outputlog
  exit -4
fi

# check if previous day's run ran OK (populated with $efile)
efile=rtofs_glo.t00z.f06.archs.a.tgz
if [ -s $COMDIR/rtofs.$PDYm1/$efile ]
then
  echo "$COMDIR/rtofs.$PDYm1/$efile exists " >> $outputlog
  PDY=$($NDATE +24 ${PDYm1}00 | cut -c1-8)
else 
  echo "Sim $sim for $PDYm1 not complete" >> $outputlog
  exit -3
fi

# has job reached the lastday
if [ $PDY -gt $lastday ]
then
   echo "sim $sim has reached $lastday" >> $outputlog
   echo "disabling cron" >> $outputlog
   crontab -l | sed '/^[^#].*submitnextjob/s/^/#/' | crontab
   exit -2
# how to enable cron automatically
# crontab -l | sed "/^[^#].*submitnextjob/s/^/#/" | crontab
fi

# if comdir for this day exists, then move it aside
echo Checking if $COMDIR/rtofs.$PDY exists
if [ -d $COMDIR/rtofs.$PDY ]
then
  mv $COMDIR/rtofs.$PDY $COMDIR/rtofs.$PDY.$pid
fi

# if tmpdir for this day exists, then move it aside
echo Checking if $TMPDIR/$PDY exists
if [ -d $TMPDIR/$PDY ]
then
  mv $TMPDIR/$PDY $TMPDIR/$PDY.$pid
fi

# now ready to submit job
echo Submitting simulation for $run on $PDY using $cfg.config >> $outputlog
cd /lfs/h2/emc/eib/save/dan.iredell/tools
./ncoda_rtofs_glo.launch.sh $cfg.config $PDY


