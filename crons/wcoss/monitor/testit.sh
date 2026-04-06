#!/bin/sh

#set -x

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

# test for hdev first as a machine can be both prod and dev
# and in that case it will act as dev
echo
if [[ $host == $hdev ]]; then
  echo "$host is the dev machine - I will exit "
elif [[ $host == $hprod ]]; then
  echo "$host is the prod machine - I will proceed "
else
  echo "Unknown machine - I will exit"
  exit
fi

echo
echo host $host
echo hprod $hprod
echo
exit

#wcoss2
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel

pdy=$($NDATE | cut -c1-8)
hh=$($NDATE | cut -c9-10)
if [ $# -eq 1 ]
then
  pdy=$1
  hh=24
fi

tmpdir=/lfs/h2/emc/ptmp/$LOGNAME
mkdir -p $tmpdir

odir=/lfs/h1/ops/prod/output/$pdy

#check if run before today
prestat=99
preftot=99
prentot=-1
if [ -s $tmpdir/negdp_status.$pdy ]
then
  prestat=$(cat $tmpdir/negdp_status.$pdy | cut -d" " -f1)
  preftot=$(cat $tmpdir/negdp_status.$pdy | cut -d" " -f2)
  prentot=$(cat $tmpdir/negdp_status.$pdy | cut -d" " -f3)
fi

#check if more than one output file for any of the checked jobs
morethanone=0
inc=$(ls -1 $odir/rtofs_global_incup.o* | wc -l)
anc=$(ls -1 $odir/rtofs_global_analysis.o* | wc -l)
f1c=$(ls -1 $odir/rtofs_global_forecast_step1.o* | wc -l)
f2c=$(ls -1 $odir/rtofs_global_forecast_step2.o* | wc -l)

if [[ $inc -gt 1 || $anc -gt 1 || $f1c -gt 1 || $f2c -gt 1 ]]
then
   morethanone=1
fi

files=
ftot=0
for f in $odir/rtofs_global_incup.o* $odir/rtofs_global_analysis.o* $odir/rtofs_global_forecast_step?.o*
do
  if [ -s $f ]
  then
     files="$files $f"
     let ftot=ftot+1
  fi
done

#create list files to search
#case $hh in
#   02)
#       files=$odir/rtofs_global_incup.o*
#       ftot=1
#       ;;
#   0[345678])
#       files="$odir/rtofs_global_incup.o* $odir/rtofs_global_analysis.o*"
#       ftot=2
#       ;;
#   09|1[012345])
#       files="$odir/rtofs_global_incup.o* $odir/rtofs_global_analysis.o* $odir/rtofs_global_forecast_step1.o*"
#       ftot=3
#       ;;
#   *)
#       files="$odir/rtofs_global_incup.o* $odir/rtofs_global_analysis.o* $odir/rtofs_global_forecast_step?.o*"
#       ftot=4
#       ;;
#esac

#count of neg dp
echo "Below is the number of occurences of the string 'neg. dp' for $pdy." > $tmpdir/negdp_count.$pdy$hh
echo "If any over zero, then the RTOFS needs investigation " >> $tmpdir/negdp_count.$pdy$hh
echo " " >> $tmpdir/negdp_count.$pdy$hh
grep -Hc "neg. dp" $files >> $tmpdir/negdp_count.$pdy$hh

#for each file
ntot=0
for f in $files
do
   n=$(grep -c "neg. dp" $f)
   let ntot=ntot+n
   if [ $n -ne 0 ]
   then
      echo " " >> $tmpdir/negdp_count.$pdy$hh
      echo $f >> $tmpdir/negdp_count.$pdy$hh
      grep "neg. dp" $f 
      grep "neg. dp" $f >> $tmpdir/negdp_count.$pdy$hh
      echo " " >> $tmpdir/negdp_count.$pdy$hh
   fi
done

# send email if required - found errors or jobs done and not sent before
sendit=0
endofjob=0
newstat=0

# send if we have new errors 
if [[ $ntot -ne 0 && $ntot -ne $prentot ]]
then
  sendit=1
  newstat=1
fi
# send if end of run and not sent yet
if [[ $ftot -ge 4 && $prestat -ne 2 ]]
then
  sendit=1
  newstat=2
fi

if [[ $sendit -eq 1 ]]
then
echo "$newstat $ftot $ntot" > $tmpdir/negdp_status.$pdy
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
        echo "Subject: $subject rtofs.$pdy:  $ntot"
        cat $tmpdir/mailheader.txt $tmpdir/negdp_count.$pdy$hh $tmpdir/mailfooter.txt
) | /usr/sbin/sendmail -t

rc=$?
echo
echo sendmail return code $rc
echo

else
echo "1 $ftot $ntot" > $tmpdir/negdp_status.$pdy
fi
