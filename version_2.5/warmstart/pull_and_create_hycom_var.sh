#!/bin/bash
# this script pulls the required hycom_var data and recreates the hycom_var directory
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

mkdir -p $COMOUT/ncoda/hycom_var
mkdir -p $TMPDIR/stage.$PDY
cd $TMPDIR/stage.$PDY

#
# Submit service job to pull hycom listing
cat << eofB > $TMPDIR/stage.$PDY/pull.hycom.listing.sh
#!/bin/bash
#PBS -N Npull_hycom_listing
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
#cd $TMPDIR/stage.$PDY
mkdir -p $COMOUT/ncoda/hycom_var/restart
cd $COMOUT/ncoda/hycom_var

#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/ncoda.\$pdy/hycom_var_listing.tar
#if [ $pdy -le 20220629 ]
#then
#htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/hycom_var_listing.tar
#else
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.prod/rtofs.\$pdy/hycom_var_listing.tar
#fi

htar -xvf $hdir/\rtofs.\$pdy/hycom_var_listing.tar

eofB

alljobs=$(qsub $TMPDIR/stage.$PDY/pull.hycom.listing.sh)

#
# Submit 15 service jobs to pull hycom var
for i in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14
do
ipdy=`$NDATE \` expr -$i \* 24 \` ${PDY}00 | cut -c1-8`

cat << eofn > $TMPDIR/stage.$PDY/pullhycomvar.sub.$ipdy.sh
#!/bin/bash
#PBS -N Npull_hycomvar$i
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

pdy=$ipdy
#mkdir -p $TMPDIR/stage.$PDY/3dvar/rtofs.PDY/ncoda.\$pdy
#cd $TMPDIR/stage.$PDY/3dvar/rtofs.PDY/ncoda.\$pdy
mkdir -p $COMOUT/ncoda/3dvar/ncoda.\$pdy
cd $COMOUT/ncoda/3dvar/ncoda.\$pdy

#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/ncoda.\$pdy/hycom.tar
#if [ $pdy -le 20220629 ]
#then
#htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/hycom.tar
#else
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.prod/rtofs.\$pdy/hycom.tar
#fi

htar -xvf $hdir/rtofs.\$pdy/hycom.tar

eofn

thisjob=$(qsub $TMPDIR/stage.$PDY/pullhycomvar.sub.$ipdy.sh)
alljobs=${alljobs}:${thisjob}

done

#
# Submit batch job to remake hycom_var
pdy=$PDY

# wcoss1
#jobdep=\'done\(\"Npull_hycomvar0\"\)
#for i in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14
#do
#jobdep="${jobdep} && done(\"Npull_hycomvar${i}\")"
#done
#jobdep=${jobdep}\'
#####BSUB -w 'done("Npull_hycomvar")'
#####BSUB -w 'done("jrtofs_forecast_step2_pre") && done("jrtofs_forecast_step1")'

# check if dev queue is enabled
queue=dev
if [ $(qstat -Q | grep ^"dev " | awk '{printf ("%s\n", $4)}') == no ]
then
   queue=dev_transfer
fi

cat << eofC > $TMPDIR/stage.$PDY/remake.$pdy.sh
#!/bin/bash
#PBS -N remake_hycomvar.$pdy
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

mkdir -p $COMOUT/ncoda/hycom_var/restart
#cd $TMPDIR/stage.$PDY
cd $COMOUT/ncoda/hycom_var/restart

while read line
do
  str=\`echo \$line | awk '{printf ("%s\n", \$9)}'\`
  fn=\`echo \$str | cut -d/ -f4\`
  sz=\`echo \$line | awk '{printf ("%i\n", \$5)}'\`

  froms=\`find ../../3dvar -name \$fn | sort\`
  if [[ EMPTY\$froms == EMPTY ]]
  then
    echo FILENOTFOUND \$line
  else
    for v in \$froms;do from=\$v;done

    echo linking \$from 
    ln -sf \$from .
    rc=\$?
    if [ \$rc -ne 0 ];then echo return code \$rc \$from ;fi
  fi

done < ../listing.hycom_var.restart.$pdy

#echo clean up \$TMPDIR

date

eofC

qsub -W depend=afterok:$alljobs $TMPDIR/stage.$PDY/remake.$pdy.sh

