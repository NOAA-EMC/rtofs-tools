#!/bin/bash
# this script pulls the required RTOFS archives and restarts for PDYm1
#

if [ $# -eq 1 ]
then
export PDY=$1
else
echo USAGE: $0 PDY
exit -2
fi

# account=${account:-hurricane}
# OUTDIR=${OUTDIR:-/scratch2/NCEPDEV/$GROUP/$USER/COMDIR/com/rtofs/prod}

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel
module load intel

PDYm1=`$NDATE \` expr -1 \* 24 \` ${PDY}00 | cut -c1-8`
COMOUT=$OUTDIR/rtofs.$PDY

echo PDY is $PDY
echo COMOUT is $OUTDIR/rtofs.$PDY

mkdir -p $TMPDIR/stage.$PDY
cd $TMPDIR/stage.$PDY

#
# Submit transfer job to pull rtofs data restarts and archives
cat << eofA > $TMPDIR/stage.$PDY/pull.rtofs.data1.sh
#!/bin/bash
#PBS -N Npull_rtofsdata1
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q dev_transfer
#PBS -l walltime=05:00:00
#PBS -l debug=true
#PBS -V

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel
module load intel
module list

set -x

pdy=$PDY
yyyy=\`echo \$pdy | cut -c1-4\`
mm=\`echo \$pdy | cut -c5-6\`

# get rtofs data
mkdir -p $COMOUT
cd $COMOUT

hdirperm=$hdir
# if production find version and set macros for ops
if [ $PRODUCTION == 1 ]
then
  vers=\$(hsi -P ls /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/ | grep rtofs.\${pdy}.restart.tar.idx | cut -d_ -f3)
  hdirperm=/NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy
  hdir=/NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy
fi

# restart file (if available)
rlist=
rlist="rtofs_glo.t00z.n00.restart.b rtofs_glo.t00z.n00.restart.a.tgz rtofs_glo.t00z.n00.restart_cice.tgz "
rlist="\$rlist rtofs_glo.t00z.n-24.restart.b rtofs_glo.t00z.n-24.restart.a.tgz rtofs_glo.t00z.n-24.restart_cice.tgz "
if [ \$pdy -gt 20210427 ]
then
   rlist="\$rlist rtofs_glo.t00z.n-06.restart.b rtofs_glo.t00z.n-06.restart.a.tgz rtofs_glo.t00z.n-06.restart_cice.tgz "
else
   echo
   echo rtofs_glo.t00z.n-06.restart not in this archive.
   echo
fi

htar -xvf \$hdirperm/rtofs.\$pdy/rtofs.restart.tar

# ab in 5 year
alist=
for f in 00 01 02 03 04 05 06 07 8 9 10 11 12
do
  fd=\$f
  if [[ \$f -eq 8 || \$f -eq 9 ]]
  then
    fd=0\$f
  fi
  alist="\$alist rtofs_glo.t00z.f\$fd.arche.a.tgz rtofs_glo.t00z.f\$fd.arche.b"
  alist="\$alist rtofs_glo.t00z.f\$fd.archs.a.tgz rtofs_glo.t00z.f\$fd.archs.b"
  if [[ \$f -eq 06 || \$f -eq 12 ]]
  then
    alist="\$alist rtofs_glo.t00z.f\$fd.archv.a.tgz rtofs_glo.t00z.f\$fd.archv.b"
  fi
done
alist="\$alist rtofs_glo.t00z.n00.arche.a.tgz rtofs_glo.t00z.n00.arche.b"
alist="\$alist rtofs_glo.t00z.n00.archs.a.tgz rtofs_glo.t00z.n00.archs.b"
alist="\$alist rtofs_glo.t00z.n00.archv.a.tgz rtofs_glo.t00z.n00.archv.b"
for n in 01 02 03 04 05 06 07 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
do
  nd=\$n
  if [[ \$n -eq 8 || \$n -eq 9 ]]
  then
    nd=0\$n
  fi
  alist="\$alist rtofs_glo.t00z.n-\${nd}.arche.a.tgz rtofs_glo.t00z.n-\${nd}.arche.b"
  alist="\$alist rtofs_glo.t00z.n-\${nd}.archs.a.tgz rtofs_glo.t00z.n-\${nd}.archs.b"
  if [[ \$f -eq 06 || \$f -eq 12 || \$f -eq 18 || \$f -eq 24 ]]
  then
    alist="\$alist rtofs_glo.t00z.n-\${nd}.archv.a.tgz rtofs_glo.t00z.n-\${nd}.archv.b"
  fi
done
alist="\$alist rtofs_glo.t00z.f18.archv.a.tgz rtofs_glo.t00z.f24.archv.b"
alist="\$alist rtofs_glo.t00z.f24.archv.a.tgz rtofs_glo.t00z.f24.archv.b"
alist="\$alist rtofs_glo.t00z.f48.archv.a.tgz rtofs_glo.t00z.f48.archv.b"
alist="\$alist rtofs_glo.t00z.f72.archv.a.tgz rtofs_glo.t00z.f72.archv.b"
alist="\$alist rtofs_glo.t00z.f96.archv.a.tgz rtofs_glo.t00z.f96.archv.b"
alist="\$alist rtofs_glo.t00z.f120.archv.a.tgz rtofs_glo.t00z.f120.archv.b" 
alist="\$alist rtofs_glo.t00z.f144.archv.a.tgz rtofs_glo.t00z.f144.archv.b"
alist="\$alist rtofs_glo.t00z.f168.archv.a.tgz rtofs_glo.t00z.f168.archv.b"


htar -xvf $hdir/rtofs.\$pdy/rtofs.ab.tar

eofA

qsub $TMPDIR/stage.$PDY/pull.rtofs.data1.sh

exit

