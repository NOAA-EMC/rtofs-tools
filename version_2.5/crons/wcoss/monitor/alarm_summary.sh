#!/bin/sh

set -x

# you can modify these:

envir=prod
distro_prod=Dan.Iredell@noaa.gov,Shastri.Paturi@noaa.gov,Zulema.Garraffo@noaa.gov,Avichal.Mehra@noaa.gov
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

#wcoss1
hprod=$( cat /etc/prod )
hdev=$( cat /etc/dev )

#wcoss2
hprod=$(grep primary /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)
hdev=$(grep backup /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)

if [[ $host == $hprod ]]; then
  echo "$host is the prod machine - I will send an email to "
  echo $distro_prod
  distro=$distro_prod
  subject="WCOSS2 NCODA alarm"
elif [[ $host == $hdev ]]; then
  echo "$host is the dev machine - I will send an email to "
  echo $distro_dev
  distro=$distro_dev
  subject="dev WCOSS2 NCODA alarm"
else
  echo "Unknown machine - I will exit"
  exit
fi

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

alarmday=`$NDATE -24 ${pdy}00`

rtofs_glo_ver=v2.2
dir=$(compath.py -o rtofs/${rtofs_glo_ver})
tmpdir=/lfs/h2/emc/ptmp/$LOGNAME
mkdir -p $tmpdir
rm -f $tmpdir/alarm_summary.$pdy
echo "Below is a summary of the alarm log for $pdy" > $tmpdir/alarm_summary.$pdy
echo "This summary file is located here: $tmpdir/alarm_summary.$pdy" >> $tmpdir/alarm_summary.$pdy
echo "The full alarm file is located here: $dir/rtofs.$pdy/ncoda/logs/alarm/ncoda_alarm.$alarmday.out" >> $tmpdir/alarm_summary.$pdy

# these line numbers are hardcoded to heading of the summary for each type
#for n in 8 31 54 77 100 123 146; do
#  head -$n $dir/rtofs.$pdy/ncoda/logs/alarm/ncoda_alarm.$alarmday.out | tail -4 >> $tmpdir/alarm_summary.$pdy
#done

# Sea Ice
# Satellite SST
# In Situ SST
# Satellite SSS
# Satellite SSH
# Profiles
# Ocean Color

for dets in \
   "n=8 t=4" \
   "n=31 t=4" \
   "n=54 t=4" \
   "n=77 t=4" \
   "n=101 t=5" \
   "n=123 t=4" \
   "n=146 t=4"
do
  eval $dets
  head -$n $dir/rtofs.$pdy/ncoda/logs/alarm/ncoda_alarm.$alarmday.out | tail -$t >> $tmpdir/alarm_summary.$pdy
done


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

#and mail it
(
        echo "To: $distro";
        echo "From: wcoss2.monitor@noaa.gov";
        echo "Subject: $subject $pdy"
        cat $tmpdir/mailheader.txt $tmpdir/alarm_summary.$pdy $tmpdir/mailfooter.txt
) | /usr/sbin/sendmail -t

rc=$?
echo
echo return code from sendmail: $rc
echo


