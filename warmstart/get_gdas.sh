#!/bin/bash
#
# this script pulls the gdas data for PDY -- see dates below for formats
#
# yyyymmdd to 20200225  
#      tarball:  gpfs_dell1_nco_ops_com_gfs_prod_gdas.[yyyymmdd]_[cc].gdas_flux.tar  
#      filename: ./gdas.[yyyymmdd]/[cc]/gdas.t[cc]z.sfluxgrbf[hhh]grib2
# 20200226 to 20210320  (add version)
#      tarball:  com_gfs_[vers]_gdas.[yyyymmdd]_[cc].gdas_flux.tar
#      filename: ./gdas.[yyyymmdd]/[cc]/gdas.t[cc]z.sfluxgrbf[hhh]grib2
# 20210321 to present (add atmos/wave to directory path (in filename))
#      tarball: com_gfs_[vers]_gdas.[yyyymmdd]_[cc].gdas_flux.tar
#      filename: ./gdas.[yyyymmdd]/[cc]/atmos/gdas.t[cc]z.sfluxgrbf[hhh]grib2
# 2026???? to present (add more directories)
#      tarball: 
#      filename: 


if [ $# -eq 1 ]
then
  export PDY=$1
else
  echo USAGE: $0 YYYYMMDD
  exit -2
fi

#. ./user.config
export inputroot=/lfs/h2/emc/eib/noscrub/dan.iredell

# gfs data is in gfs/prod under inputroot
OUTDIR=$inputroot/com/gfs
TMPDIR=/lfs/h2/emc/ptmp/dan.iredell/warmstart

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel
module load intel
module list

echo PDY is $PDY
echo OUTDIR is $OUTDIR

mkdir -p $OUTDIR
mkdir -p $TMPDIR/stage.$PDY

#Submit service job to get gdas sflux files
cat << eofA > $TMPDIR/stage.$PDY/pull.gdas.sh
#!/bin/ksh -l
#PBS -N gdas.$PDY
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1:mem=16GB
#PBS -q dev_transfer
#PBS -l walltime=04:00:00
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
yyyymm=`echo $PDY | cut -c1-6`

cd $OUTDIR

if [ \$pdy -ge 20200226 ]
then
# filename structure:
# com_gfs_[vers]_gdas.$pdy_$hh.gdas_flux.tar

# find version
vers=\$(hsi -P ls /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/ | grep gdas.\${pdy}_00.gdas_flux.tar.idx | cut -d_ -f3)

for c in 00 06 12 18
do
slist=
# as of 20250129, all gdas flux files (hours 00-09) are being archived
# RTOFS does not need 00, but getting for completeness
#for h in 3 6 9
#do
#slist="\$slist ./gdas.\$pdy/\$c/atmos/gdas.t\${c}z.sfluxgrbf00\$h.grib2"
#done
#echo
#echo \$slist
#echo
#htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/com_gfs_\${vers}_gdas.\${pdy}_\${c}.gdas_flux.tar \$slist
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/com_gfs_\${vers}_gdas.\${pdy}_\${c}.gdas_flux.tar
done
fi

if [ \$pdy -le 20200225 ]
then
# filename structure:
# gpfs_dell1_nco_ops_com_gfs_prod_gdas.$pdy_$hh.gdas_flux.tar

for c in 00 06 12 18
do
slist=
for h in 3 6 9
do
slist="\$slist ./gdas.\$pdy/\$c/gdas.t\${c}z.sfluxgrbf00\$h.grib2"
done
echo
echo \$slist
echo
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/gpfs_dell1_nco_ops_com_gfs_prod_gdas.\${pdy}_\${c}.gdas_flux.tar \$slist
done
fi

eofA

cd $TMPDIR/stage.$PDY
qsub pull.gdas.sh

