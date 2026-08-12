#!/bin/sh

######################################################################################

## Check if running on prod
h=$( hostname | cut -c1-1 )
case "$h" in
  t) host=tide  ;;
  g) host=gyre  ;;
  s) host=surge ;;
  l) host=luna  ;;
  m) host=mars  ;;
  v) host=venus ;;
  *) host=nobody
esac
hprod=$( cat /etc/prod )
hdev=$( cat /etc/dev )
if [[ $host == $hprod ]]; then
  echo "$host is the prod machine - I will proceed"
elif [[ $host == $hdev ]]; then
  echo "$host is the dev machine - I will exit"
  exit
else
  echo "Unknown machine - I will exit"
  exit
fi

. /usrx/local/prod/lmod/lmod/init/profile
module load ips/18.0.5.274
module load prod_util/1.1.5

set -x

pdy=`$NDATE | cut -c1-8`
if [ $# -eq 1 ]
then
  pdy=$1
fi


mkdir -p /gpfs/dell2/emc/modeling/noscrub/$USER/save_restart_cice/$pdy

cp -p /gpfs/dell1/nco/ops/com/rtofs/prod/rtofs.$pdy/*restart*cice*tgz /gpfs/dell2/emc/modeling/noscrub/$USER/save_restart_cice/$pdy


