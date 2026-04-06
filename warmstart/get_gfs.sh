#!/bin/bash
# this script pulls the gfs data for PDY
#
# yyyymmdd to 20200225
#      tarball:  gpfs_dell1_nco_ops_com_gfs_prod_gfs.[yyyymmdd]_[cc].gfs_flux.tar
#      filename: ./gfs.[yyyymmdd]/[cc]/gfs.t[cc]z.sfluxgrbf[hhh]grib2
# 20200226 to 20210320  (add version)
#      tarball:  com_gfs_[vers]_gfs.[yyyymmdd]_[cc].gfs_flux.tar
#      filename: ./gfs.[yyyymmdd]/[cc]/gfs.t[cc]z.sfluxgrbf[hhh]grib2
# 20210321 to present (add atmos/wave to directory path (in filename))
#      tarball: com_gfs_[vers]_gfs.[yyyymmdd]_[cc].gfs_flux.tar
#      filename: ./gfs.[yyyymmdd]/[cc]/atmos/gfs.t[cc]z.sfluxgrbf[hhh]grib2


if [ $# -eq 1 ]
then
  export PDY=$1
else
  echo USAGE: $0 YYYYMMDD
  exit -2
fi

#. ./user.config
export inputroot=/lfs/h2/emc/eib/noscrub/dan.iredell

# OUTDIR is gfs/prod under inputroot
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

#Submit service job to get gfs sflux files
cat << eofA > $TMPDIR/stage.$PDY/pull.gfs.sh
#!/bin/ksh -l
#PBS -N gfs.$PDY
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
yyyy=$(echo $PDY | cut -c1-4)
yyyymm=$(echo $PDY | cut -c1-6)

cd $OUTDIR

if [ \$pdy -ge 20200226 ]
then
# filename structure:
# com_gfs_[vers]_gdas.$pdy_$hh.gfs_flux.tar

# find version
vers=\$(hsi -P ls /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/ | grep gfs.\${pdy}_00.gfs_flux.tar.idx | cut -d_ -f3)

slist=
for h in 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99
do
slist="\$slist ./gfs.\$pdy/00/atmos/gfs.t00z.sfluxgrbf0\$h.grib2"
done
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/com_gfs_\${vers}_gfs.\${pdy}_00.gfs_flux.tar \$slist

slist=
for h in 087 090 093 096 099 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189
do
slist="\$slist ./gfs.\$pdy/06/atmos/gfs.t06z.sfluxgrbf\$h.grib2"
done
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/com_gfs_\${vers}_gfs.\${pdy}_06.gfs_flux.tar \$slist

slist=
for h in 03 06
do
slist="\$slist ./gfs.\$pdy/18/atmos/gfs.t18z.sfluxgrbf0\$h.grib2"
done
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/com_gfs_\${vers}_gfs.\${pdy}_18.gfs_flux.tar \$slist
fi

if [ \$pdy -le 20200225 ]
then
# filename structure:
# gpfs_dell1_nco_ops_com_gfs_prod_gfs.$pdy_$hh.gfs_flux.tar

slist=
for h in 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99
do
slist="\$slist ./gfs.\$pdy/00/gfs.t00z.sfluxgrbf0\$h.grib2"
done
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/gpfs_dell1_nco_ops_com_gfs_prod_gfs.\${pdy}_00.gfs_flux.tar \$slist

slist= 
for h in 087 090 093 096 099 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189
do
slist="\$slist ./gfs.\$pdy/06/gfs.t06z.sfluxgrbf\$h.grib2"
done 
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/gpfs_dell1_nco_ops_com_gfs_prod_gfs.\${pdy}_06.gfs_flux.tar \$slist

slist=
for h in 03 06
do
slist="\$slist ./gfs.\$pdy/18/gfs.t18z.sfluxgrbf0\$h.grib2"
done
htar -xvf /NCEPPROD/hpssprod/runhistory/rh\$yyyy/\$yyyymm/\$pdy/gpfs_dell1_nco_ops_com_gfs_prod_gfs.\${pdy}_18.gfs_flux.tar \$slist
fi

eofA

cd $TMPDIR/stage.$PDY
qsub ./pull.gfs.sh

