#!/bin/bash
# this script pulls data for HAFS

# for paraD5
for date in 20231017 20231019 20231031
do

export PDY=$date

export inputroot=/lfs/h2/emc/eib/noscrub/dan.iredell

# put hafs input files in OUTDIR
export OUTDIR=$inputroot/hafs-input/COMRTOFSv2/
export TMPDIR=/lfs/h2/emc/ptmp/dan.iredell/forhafs

module reset
module load prod_envir
module load prod_util
module load PrgEnv-intel
module load intel
module list

echo PDY is $PDY
echo OUTDIR is $OUTDIR

mkdir -p $OUTDIR
mkdir -p $TMPDIR

#Submit service job to get files
cat << eofA > $TMPDIR/pull.forhafs.$PDY.sh
#!/bin/ksh -l
#PBS -N hin.$PDY
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q dev_transfer
#PBS -l walltime=05:00:00
#PBS -l debug=true
#PBS -V

module reset
#module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel
module load intel
module list

set -x

pdy=$PDY
yyyy=`echo $PDY | cut -c1-4`
yyyymm=`echo $PDY | cut -c1-6`

mkdir -p \$OUTDIR/\rtofs.\$pdy
cd \$OUTDIR/rtofs.\$pdy

asrealtime=0
if [ $asrealtime -eq 1 ]
then  # realtime
#archv
htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.paraD5b/rtofs.\$pdy/rtofs.ab.tar rtofs_glo.t00z.f06.archv.a.tgz rtofs_glo.t00z.f06.archv.b rtofs_glo.t00z.f12.archv.a.tgz rtofs_glo.t00z.f12.archv.b rtofs_glo.t00z.f18.archv.a.tgz rtofs_glo.t00z.f18.archv.b rtofs_glo.t00z.n00.archv.a.tgz rtofs_glo.t00z.n00.archv.b

#restart
htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.paraD5b/rtofs.\$pdy/rtofs.restart.tar rtofs_glo.t00z.n00.restart.a.tgz rtofs_glo.t00z.n00.restart.b

else  # hindcast - use nowcast files
#archv
htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.paraD5b/rtofs.\$pdy/rtofs.ab.tar rtofs_glo.t00z.n-06.archv.a.tgz rtofs_glo.t00z.n-06.archv.b rtofs_glo.t00z.n-12.archv.a.tgz rtofs_glo.t00z.n-12.archv.b rtofs_glo.t00z.n-18.archv.a.tgz rtofs_glo.t00z.n-18.archv.b rtofs_glo.t00z.n-24.archv.a.tgz rtofs_glo.t00z.n-24.archv.b

#restart
htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.paraD5b/rtofs.\$pdy/rtofs.restart.tar rtofs_glo.t00z.n-24.restart.a.tgz rtofs_glo.t00z.n-24.restart.b rtofs_glo.t00z.n-06.restart.a.tgz rtofs_glo.t00z.n-06.restart.b

fi

eofA

cd $TMPDIR
qsub pull.forhafs.$PDY.sh

done

