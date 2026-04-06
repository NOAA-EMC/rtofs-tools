#!/bin/bash
# this script pulls the required RTOFS data for PDYm1
# - ncoda
# -- ocnqc
# -- shem_var
# -- nhem_var
# -- glbl_var
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

mkdir -p $COMOUT/ncoda
mkdir -p $TMPDIR/stage.$PDY
cd $TMPDIR/stage.$PDY

#
# Submit transfer job to pull ocnqc, nhem, shem, glbl, and gdas forcing
cat << eofA > $TMPDIR/stage.$PDY/pull.lotsa.stuff.sh
#!/bin/bash
#PBS -N Npull_lotsa
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
yyyy=`echo $PDY | cut -c1-4`
mm=`echo $PDY | cut -c5-6`

# get ocnqc (plus some other stuff) in nco tanks
mkdir -p $COMOUT/ncoda
cd $COMOUT/ncoda

# if production find version and set macros for ops
otarball=rtofs.\$pdy/ocnqc.tar
if [ $PRODUCTION == 1 ]
then
  vers=\$(hsi -P ls /NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/ | grep rtofs.\${pdy}.ncoda.tar.idx | cut -d_ -f3)
  hdir=/NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy
  otarball=com_rtofs_\${vers}_rtofs.\$pdy.ncoda.tar
fi

# no check on date for ocnqc?
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/ncoda.\$pdy/ocnqc.tar
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/rtofs.\$pdy\rtofs.ncoda.tar
#htar -xvf /NCEPPROD/5year/hpssprod/runhistory/rh\$yyyy/\$yyyy\$mm/\$pdy/com_rtofs_\${vers}_rtofs.\$pdy.ncoda.tar
htar -xvf \$hdir/\$otarball
htar -xvf $hdir/rtofs.\$pdy/logs.tar


# get 2dvar from emc tanks
cd $COMOUT/ncoda
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/ncoda.\$pdy/glbl.tar
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/ncoda.\$pdy/nhem.tar
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/emc_parallel6_wcoss2/ncoda.\$pdy/shem.tar
#if [ $pdy -le 20220629 ]
#then
#htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/glbl.tar
#htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/nhem.tar
#htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/shem.tar
#else
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.prod/rtofs.\$pdy/glbl.tar
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.prod/rtofs.\$pdy/nhem.tar
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.prod/rtofs.\$pdy/shem.tar
#fi

htar -xvf $hdir/rtofs.\$pdy/glbl.tar
htar -xvf $hdir/rtofs.\$pdy/nhem.tar
htar -xvf $hdir/rtofs.\$pdy/shem.tar

# get forcing (not always available) from emc tanks (re-create forcing from gdas/gfs)
cd $COMOUT
#if [ $pdy -le 20220629 ]
#then
#htar -xvf /NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2/rtofs.\$pdy/rtofs.forcing.tar
#else
#htar -xvf /NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.prod/rtofs.\$pdy/rtofs.forcing.tar
#fi

htar -xvf $hdir/rtofs.\$pdy/rtofs.forcing.tar

eofA

qsub $TMPDIR/stage.$PDY/pull.lotsa.stuff.sh

exit

