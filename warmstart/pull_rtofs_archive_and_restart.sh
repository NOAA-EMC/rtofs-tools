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

hdirpermd=$hdir/rtofs.\$pdy
hdirpermf=rtofs.restart.tar
hdird=$hdir/rtofs.\$pdy
hdirf=rtofs.ab.tar
ds=
# if production find version and set macros for ops
if [ $PRODUCTION == 1 ]
then
  vers=\$(hsi -P ls /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/ | grep rtofs.\${pdy}.restart.tar.idx | cut -d_ -f3)
  hdirpermd=/NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy
  hdirpermf=com_rtofs_\${vers}_rtofs.\$pdy.restart.tar
  hdird=/NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy
  hdirf=com_rtofs_\${vers}_rtofs.\$pdy.ab.tar
  ds=./
fi

# restart file (if available)
rlist=
rlist="\${ds}rtofs_glo.t00z.n00.restart.b \${ds}rtofs_glo.t00z.n00.restart.a.tgz \${ds}rtofs_glo.t00z.n00.restart_cice.tgz "
rlist="\$rlist \${ds}rtofs_glo.t00z.n-24.restart.b \${ds}rtofs_glo.t00z.n-24.restart.a.tgz \${ds}rtofs_glo.t00z.n-24.restart_cice.tgz "
if [ \$pdy -gt 20210427 ]
then
   rlist="\$rlist \${ds}rtofs_glo.t00z.n-06.restart.b \${ds}rtofs_glo.t00z.n-06.restart.a.tgz \${ds}rtofs_glo.t00z.n-06.restart_cice.tgz "
else
   echo
   echo rtofs_glo.t00z.n-06.restart not in this archive.
   echo
fi

#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/rtofs.\$pdy/rtofs.restart.tar \$rlist
#htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/com_rtofs_\${vers}_rtofs.\$pdy.restart.tar \$rlist
pwd
htar -xvf \$hdirpermd/\$hdirpermf \$rlist
ls

# ab in 5 year
alist=
for f in 00 01 02 03 04 05 06 07 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42
do
  fd=\$f
  if [[ \$f -eq 8 || \$f -eq 9 ]]
  then
    fd=0\$f
  fi
  alist="\$alist \${ds}rtofs_glo.t00z.f\$fd.arche.a.tgz \${ds}rtofs_glo.t00z.f\$fd.arche.b"
  alist="\$alist \${ds}rtofs_glo.t00z.f\$fd.archs.a.tgz \${ds}rtofs_glo.t00z.f\$fd.archs.b"
  if [[ \$f -eq 06 || \$f -eq 12 ]]
  then
    alist="\$alist \${ds}rtofs_glo.t00z.f\$fd.archv.a.tgz \${ds}rtofs_glo.t00z.f\$fd.archv.b"
  fi
done
alist="\$alist \${ds}rtofs_glo.t00z.n00.arche.a.tgz \${ds}rtofs_glo.t00z.n00.arche.b"
alist="\$alist \${ds}rtofs_glo.t00z.n00.archs.a.tgz \${ds}rtofs_glo.t00z.n00.archs.b"
alist="\$alist \${ds}rtofs_glo.t00z.n00.archv.a.tgz \${ds}rtofs_glo.t00z.n00.archv.b"
for n in 01 02 03 04 05 06 07 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
do
  nd=\$n
  if [[ \$n -eq 8 || \$n -eq 9 ]]
  then
    nd=0\$n
  fi
  alist="\$alist \${ds}rtofs_glo.t00z.n-\${nd}.arche.a.tgz \${ds}rtofs_glo.t00z.n-\${nd}.arche.b"
  alist="\$alist \${ds}rtofs_glo.t00z.n-\${nd}.archs.a.tgz \${ds}rtofs_glo.t00z.n-\${nd}.archs.b"
  if [[ \$f -eq 06 || \$f -eq 12 || \$f -eq 18 || \$f -eq 24 ]]
  then
    alist="\$alist \${ds}rtofs_glo.t00z.n-\${nd}.archv.a.tgz \${ds}rtofs_glo.t00z.n-\${nd}.archv.b"
  fi
done
alist="\$alist \${ds}rtofs_glo.t00z.f18.archv.a.tgz \${ds}rtofs_glo.t00z.f18.archv.b"
alist="\$alist \${ds}rtofs_glo.t00z.f24.archv.a.tgz \${ds}rtofs_glo.t00z.f24.archv.b"
alist="\$alist \${ds}rtofs_glo.t00z.f48.archv.a.tgz \${ds}rtofs_glo.t00z.f48.archv.b"
alist="\$alist \${ds}rtofs_glo.t00z.f72.archv.a.tgz \${ds}rtofs_glo.t00z.f72.archv.b"
alist="\$alist \${ds}rtofs_glo.t00z.f96.archv.a.tgz \${ds}rtofs_glo.t00z.f96.archv.b"
alist="\$alist \${ds}rtofs_glo.t00z.f120.archv.a.tgz \${ds}rtofs_glo.t00z.f120.archv.b" 
alist="\$alist \${ds}rtofs_glo.t00z.f144.archv.a.tgz \${ds}rtofs_glo.t00z.f144.archv.b"
alist="\$alist \${ds}rtofs_glo.t00z.f168.archv.a.tgz \${ds}rtofs_glo.t00z.f168.archv.b"

#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/rtofs.\$pdy/rtofs.ab.tar \$alist
#htar -xvf /NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/com_rtofs_\${vers}_rtofs.\$pdy.ab.tar \$alist

htar -xvf \$hdird/\$hdirf \$alist

# check if dev queue is enabled
eofA

thisjob=$(qsub $TMPDIR/stage.$PDY/pull.rtofs.data1.sh)


# job to gunzip all the tgz files
queue=dev
if [ $(qstat -Q | grep ^"dev " | awk '{printf ("%s\n", $4)}') == no ]
then
   queue=dev_transfer
fi

cat << eofC > $TMPDIR/stage.$PDY/gunzip.$pdy.sh
#!/bin/bash
#PBS -N gunzip.$pdy
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q $queue
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

cd $COMOUT

# untar the .tgz files
for a in \`ls *arch*tgz\`;do echo \$a;tar xpvzf \$a;done &
for r in \`ls *restart*tgz\`;do echo \$r;tar xpvzf \$r;done &

wait

# rm the .tgz files
rm -f *.tgz

eofC

qsub -W depend=afterok:$thisjob $TMPDIR/stage.$PDY/gunzip.$pdy.sh

#
# Submit transfer job to pull rtofs data for PDYm2 - PDYm7
for d in 24 48 72 96 120 144 
do
  let hrs=-$d
  PDYmN=$($NDATE $hrs ${PDY}00 | cut -c1-8)
  pdy=$PDYmN

cat << eofB > $TMPDIR/stage.$PDY/pull.rtofs.data.$pdy.sh
#!/bin/bash
#PBS -N Npull_rtofsdata.$d
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

alist=

  pdy=$PDYmN
  yyyy=\`echo \$pdy | cut -c1-4\`
  mm=\`echo \$pdy | cut -c5-6\`

  COMOUT=$OUTDIR/rtofs.$PDYmN
  mkdir -p \$COMOUT
  cd \$COMOUT

hdird=$hdir/rtofs.\$pdy
hdirf=rtofs.ab.tar
ds=
if [ $PRODUCTION == 1 ]
then
  vers=\$(hsi -P ls /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/ | grep rtofs.\${pdy}.restart.tar.idx | cut -d_ -f3)
  hdird=/NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy
  hdirf=com_rtofs_\${vers}_rtofs.\$pdy.ab.tar
  ds=./
fi

#xxx  alist="\$alist rtofs_glo.t00z.f\$fd.arche.a.tgz rtofs_glo.t00z.f\$fd.arche.b"

  alist="\$alist \${ds}rtofs_glo.t00z.n00.archv.a.tgz \${ds}rtofs_glo.t00z.n00.archv.b"
  alist="\$alist \${ds}rtofs_glo.t00z.f24.archv.a.tgz \${ds}rtofs_glo.t00z.f24.archv.b"
  alist="\$alist \${ds}rtofs_glo.t00z.f48.archv.a.tgz \${ds}rtofs_glo.t00z.f48.archv.b"
  alist="\$alist \${ds}rtofs_glo.t00z.f72.archv.a.tgz \${ds}rtofs_glo.t00z.f72.archv.b"
  alist="\$alist \${ds}rtofs_glo.t00z.f96.archv.a.tgz \${ds}rtofs_glo.t00z.f96.archv.b"
  alist="\$alist \${ds}rtofs_glo.t00z.f120.archv.a.tgz \${ds}rtofs_glo.t00z.f120.archv.b" 
  alist="\$alist \${ds}rtofs_glo.t00z.f144.archv.a.tgz \${ds}rtofs_glo.t00z.f144.archv.b"
  alist="\$alist \${ds}rtofs_glo.t00z.f168.archv.a.tgz \${ds}rtofs_glo.t00z.f168.archv.b"
  #htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/rtofs.\$pdy/rtofs.ab.tar \$alist
  htar -xvf \$hdird/\$hdirf \$alist
  # untar the .tgz files
  for a in \`ls *arch*tgz\`;do echo \$a;tar xpvzf \$a;done &
  wait
# rm the .tgz files
rm -f *.tgz

  # get forcing from ops.
  #htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/rtofs.\$pdy/rtofs.forcing.tar rtofs_glo.anal.t00z.forcing.wndnwd.*

eofB

qsub $TMPDIR/stage.$PDY/pull.rtofs.data.$pdy.sh

done

exit

