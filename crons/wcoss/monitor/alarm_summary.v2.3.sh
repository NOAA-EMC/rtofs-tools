#!/bin/sh

set -x

# you can modify these:

envir=prod
distro_prod=Dan.Iredell@noaa.gov,Zulema.Garraffo@noaa.gov,Avichal.Mehra@noaa.gov,Santha.Akella@noaa.gov
distro_dev=Dan.Iredell@noaa.gov

###########################################################################

## Check if running on prod
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

rtofs_glo_ver=v2.5
envir=prod

if [[ $host == $hdev ]]; then
  echo "$host is the dev machine - I will send an email to "
  echo $distro_dev
  distro=$distro_dev
  subject="dev NCODA alarm rtofs_glo $rtofs_glo_ver"
elif [[ $host == $hprod ]]; then
  echo "$host is the prod machine - I will send an email to "
  echo $distro_prod
  distro=$distro_prod
  subject="NCODA alarm rtofs_glo $rtofs_glo_ver"
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

# rtofs in para v2.3

dir=$(compath.py -e $envir rtofs/${rtofs_glo_ver})
tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/alarm.$rtofs_glo_ver
mkdir -p $tmpdir
rm -f $tmpdir/alarm_summary.$pdy

echo "Below is a summary of the alarm log for $pdy" > $tmpdir/alarm_summary.$pdy
echo "This summary file is located here: $tmpdir/alarm_summary.$pdy" >> $tmpdir/alarm_summary.$pdy
echo "The full alarm file is located here: $dir/rtofs.$pdy/ncoda/logs/alarm/ncoda_alarm.counts.$alarmday.out" >> $tmpdir/alarm_summary.$pdy

# Sea Ice
# Satellite SST
# In Situ SST
# Satellite SSS
# Velocity
# Satellite SSH
# Profiles

for heading in "Sea Ice" "Satellite SST" "In Situ SST" "Satellite SSS" "Velocity" "Satellite SSH" "Profiles"
do
   ln=$(grep -n "$heading" $dir/rtofs.$pdy/ncoda/logs/alarm/ncoda_alarm.counts.$alarmday.out | cut -d: -f1)
   let upto=ln+3
   if [[ $heading == "Satellite SSH" ]];then let upto=ln+4;fi
   head -$upto $dir/rtofs.$pdy/ncoda/logs/alarm/ncoda_alarm.counts.$alarmday.out | tail -5 >> $tmpdir/alarm_summary.$pdy
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


