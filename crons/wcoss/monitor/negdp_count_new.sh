#!/bin/sh

#set -x

# you can modify these:

envir=prod
distro_prod=Dan.Iredell@noaa.gov,Zulema.Garraffo@noaa.gov,Avichal.Mehra@noaa.gov
distro_dev=Dan.Iredell@noaa.gov

###########################################################################

## Check if running on prod
h=$( hostname | cut -c1-1 )
case "$h" in
  s) host=surge ;;
  l) host=luna  ;;
  m) host=mars  ;;
  v) host=venus ;;
  c) host=cactus ;;
  d) host=dogwood ;;
  a) host=acorn ;;
  *) host=nobody
esac

#wcoss2
hprod=$(grep primary /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)
hdev=$(grep backup /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)

if [[ $host == $hdev ]]; then
  echo "$host is the dev machine - I will send an email to "
  echo $distro_dev
  distro=$distro_dev
elif [[ $host == $hprod ]]; then
  echo "$host is the prod machine - I will send an email to "
  echo $distro_prod
  distro=$distro_prod
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

# find directory where to look (previous hour)

pdy=$($NDATE -1 | cut -c1-8)
hh=$($NDATE -1 | cut -c9-10)
pdym1=$($NDATE -24 | cut -c1-8)
if [ $# -eq 1 ]
then
  pdy=$(echo $1 | cut -c1-8)
  hh=$(echo $1 | cut -c9-10)
  pdym1=$($NDATE -24 $1 | cut -c1-8)
fi

rtofs_ver=$(grep rtofs /lfs/h1/ops/prod/config/compaths.list | sort | tail -1 | cut -d/ -f4)
comout=$(compath.py -o rtofs/${rtofs_ver})/rtofs.${pdy}

tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/monitor_negdp_new
mkdir -p $tmpdir

odir=/lfs/h1/ops/prod/output/$pdy

# find log files that have changed in this timespan (previous hour) and search them for the string neg. dp
foundnegdp=0
negdpfiles=
jobcomplete=0
now=$(date +%Y%m%d%H%M%S)
for logs in rtofs_global_incup.o rtofs_global_analysis.o rtofs_global_forecast_step1.o rtofs_global_forecast_step2.o
do
  if compgen -G "$odir/${logs}*" > /dev/null
  then
    for file in $odir/${logs}*
    do
#       mhour=$(stat -c %y $file | cut -c12-13)
#       if [ $mhour -eq $hh ]
       fdate=$(date -r $file +%Y%m%d%H%M%S)
       let fdiff=now-fdate
       if [ $fdiff -lt 12000 ]   # 10000 is one hour, 12000 is one hour, 20 minutes
       then
         n=$(grep -c "neg. dp" $file)
# check if any neg.dp in this past hour
         if [ $n -gt 0 ]
         then
           foundnegdp=1
           negdpfiles="$negdpfiles $file"
           echo $file $n
         fi
# check if all hycom jobs complete this past hour
#         if [[ -s $comout/rtofs_glo.ssmi.${pdym1}00.r      &&
#               -s $comout/rtofs_glo.t00z.n00.restart_cice  &&
#               -s $comout/rtofs_glo.t00z.f96.restart_cice  &&
#               -s $comout/rtofs_glo.t00z.f192.restart_cice ]]
         if [[ -s $comout/rtofs_glo.t00z.n-24.restart.b     &&
               -s $comout/rtofs_glo.t00z.n00.restart.b      &&
               -s $comout/rtofs_glo.t00z.f96.restart.b      &&
               -s $comout/rtofs_glo.t00z.f192.restart.b     ]]
         then
           jobcomplete=1
         fi
       fi
    done
  fi
done

# if neg. dp found then send out warning. 
# if no neg. dp found then check if all hycom jobs have completed (within the past hour). if true send out job ran message.
# files to look for to know that the job completed successfully
# incup ----- rtofs_glo.ssmi.${pdym1}00.r       (~ 0203)
# analysis -- rtofs_glo.t00z.n00.restart_cice   (~ 0309)
# f-step1 --- rtofs_glo.t00z.f96.restart_cice   (~ 0906)
# f-step2 --- rtofs_glo.t00z.f192.restart_cice  (~ 1504)

# create email with list of negdpfiles
sendanemail=0
if [ $foundnegdp -eq 1 ]
then
  #count of neg dp
  echo "Below is the number of occurrences of the string 'neg. dp' for $pdy." > $tmpdir/negdp_count_new.$pdy$hh
  echo "If any are over zero, then the RTOFS needs investigation " >> $tmpdir/negdp_count_new.$pdy$hh
  echo " " >> $tmpdir/negdp_count_new.$pdy$hh
  grep -Hc "neg. dp" $negdpfiles >> $tmpdir/negdp_count_new.$pdy$hh
  subject="WARNING: RTOFS neg. dp count for $rtofs_ver $pdy"
  sendanemail=1
fi

if [ $jobcomplete -eq 1 ]
then
  echo "All HYCOM jobs have completed for RTOFS $pdy" > $tmpdir/jobcomplete.$pdy$hh
  echo " " >> $tmpdir/jobcomplete.$pdy$hh
  subject="SUCCESS: RTOFS $rtofs_ver completed for $pdy"
  sendanemail=1
fi

if [ $sendanemail -eq 1 ]
then
# mail header
cat << eof1 > $tmpdir/mailheader.txt
MIME-Version: 1.0
Content-Type: text/html
Content-Disposition: inline
<html>
<body>
<pre style="font: monospace">
eof1

# mail footer
cat << eof2 > $tmpdir/mailfooter.txt
</pre>
</body>
</html>
eof2

if [ $foundnegdp -eq 1 ]
then
#and mail it
(
        echo "To: $distro";
        echo "From: rtofs_monitor@do.not.reply";
        echo "Subject: $subject"
        cat $tmpdir/mailheader.txt $tmpdir/negdp_count_new.$pdy$hh $tmpdir/mailfooter.txt
) | /usr/sbin/sendmail -t

rc=$?
echo
echo sendmail return code $rc
echo
fi #foundnegdp

if [ $jobcomplete -eq 1 ]
then
#and mail it
(
        echo "To: $distro";
        echo "From: rtofs_monitor@do.not.reply";
        echo "Subject: $subject"
        cat $tmpdir/mailheader.txt $tmpdir/jobcomplete.$pdy$hh $tmpdir/mailfooter.txt
) | /usr/sbin/sendmail -t

rc=$?
echo
echo sendmail return code $rc
echo
fi #jobcomplete
fi #sendanemail

