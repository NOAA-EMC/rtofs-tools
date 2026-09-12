#!/bin/bash
#

#####

if [ $# -ne 2 ] 
then
  echo "USAGE: $0 <configname> <YYYYMMDD>"
  exit -2
fi

configname=$1
today=$2
now=$(date +%Y%m%d_%H%M%S)
if [ ! -s ./$configname ]
then
  echo cannot find $configname
  exit -2
fi
. ./$configname

# other vars not in config but can be modified on occasion
export account=RTOFS-DEV
export simulation=${simulation:-sim}
export sim=${sim:-zz}
export tmproot=/lfs/h2/emc/ptmp/$LOGNAME/$simulation
# change tmproot to stmp?
if [ $configname == XXX.release.v2.5.0.config ]; then
#if [ $configname == release.v2.5.0.config ]; then
  export tmproot=/lfs/h2/emc/stmp/$LOGNAME/$simulation
fi

echo
echo projectroot $projectroot
echo tmproot $tmproot
echo simulation $simulation
echo

batchloc=$tmproot/batchscripts
mkdir -p $batchloc

export KEEPDATA=YES

#######################################################

export PROJECTdir=$projectroot
. $PROJECTdir/versions/run.ver
export ver=$(echo $rtofs_glo_ver | cut -d. -f1-2)
echo 
echo projectroot $projectroot
echo rtofs_glo_ver $rtofs_glo_ver
echo ver $ver
echo
#exit

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load intel/${intel_ver}
module load craype/${craype_ver}
module load cray-pals/${cray_pals_ver}
module load cray-mpich/${cray_mpich_ver}
module load cfp/${cfp_ver}
module load bufr_dump/${bufr_dump_ver}
module load hdf5/${hdf5_ver}
module load netcdf/${netcdf4_ver}
module load wgrib2/${wgrib2_ver}
module load libjpeg/${libjpeg_ver}
module load grib_util/${grib_util_ver}
module load gempak/${gempak_ver}
module load cdo/${cdo_ver}
module list

#override COMs
export COMtmp=$comroot

# Set some run environment variables.
export SENDCOM=YES
export SENDDBN=NO
export model_ver=$rtofs_glo_ver
export projID=NC-${model_ver} #      `basename $PROJECTdir`

export cyc=00
export cycle=t${cyc}z
export envir=prod # prod or para or canned

export HOMErtofs=${PROJECTdir}
export HOMErtofs_glo=$HOMErtofs

# HERA mods - no gribbing or cdo outputs
#export for_opc=NO
#export grib_1hrly=NO
#--
####export rtofs_glo_ver=v${model_ver}

# Set system/model vars
export RUN=rtofs
export NET=rtofs
export modID=glo
export inputgrid=navy_0.08
. $PROJECTdir/parm/${RUN}_${modID}.${inputgrid}.config
export fcstdays=`expr ${fcstdays_step1} + ${fcstdays_step2}`

echo forecast days -- $analdays + $fcstdays_step1 + $fcstdays_step1
echo gzip $rungzip rungempak $rungempak

# let it default
#export NWROOT=$UTILROOT
export COMROOT=${COMtmp}/$envir/com/$NET/$ver
echo COMROOT $COMROOT

# observational TANKS -- these can to be changed... check envir
export DCOMROOT=$inputroot
export DCOMINAMSR=$DCOMROOT
export DCOMINSSH=$DCOMROOT
export DCOMINSSS=$DCOMROOT
export DCOMINSST=$DCOMROOT
export DCOMINHFR=$DCOMROOT
export TANK=$DCOMROOT

# where should we find GDAS/GFS surface flux file (should we override GETGES_COM)
# export envir=
# export envirges=
# export GETGES_COM=

# very important: redefinition of the default date (PDY !!!)
# if PDY is defined here, it will not be reset by setpdy utility.
export PDY=$today
export PDYm1=`$NDATE -24 ${PDY}'00' | cut -c1-8`
export myDATAROOT=$tmproot/${projID}/$PDY
mkdir -p ${myDATAROOT}
#export myCOMROOT=${COMtmp}/$envir/com/$NET/$ver
export myCOMROOT=${COMtmp}/$envir/com
mkdir -p ${myCOMROOT}

#override COMIN and COMINm1
export COMIN=$COMROOT/$RUN.$PDY
export COMINm1=$COMROOT/$RUN.$PDYm1

# Make logs directory if necessary.
test -d $COMtmp/logs/$today || mkdir -p $COMtmp/logs/$today

# Write out some info.
echo "LAUNCHER INFO: run: ${projID}, cycle: t${cyc}z, PDY=${PDY}."

pid=$$
cd ${myDATAROOT}
cd $COMtmp/logs/$today

# Submit the jobs.

# area for testing only one set of jobs
runqc=0
runglblvar=0
runpolarvar=0
run3dvar=0
runmom=1

echo DATAROOT is $myDATAROOT
echo

if [ $runqc -eq 1 ]
then
#############
jobname=rtofs_ncoda_qc
export jobid=$jobname.$pid
export job=$jobname
#mkdir -p ${myDATAROOT}/$jobid
cat << EOF_ncoda_qc > $batchloc/rtofs.ncoda_qc.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=12:mem=90GB
#PBS -q dev
#PBS -l walltime=00:59:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load intel/${intel_ver}
module load craype/${craype_ver}
module load cray-pals/${cray_pals_ver}
module load cfp/${cfp_ver}
module load bufr_dump/${bufr_dump_ver}
module load hdf5/${hdf5_ver}
module load netcdf/${netcdf4_ver}
module load wgrib2/${wgrib2_ver}
module load libjpeg/${libjpeg_ver}
module load grib_util/${grib_util_ver}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=12

$HOMErtofs/jobs/JRTOFS_GLO_NCODA_QC

EOF_ncoda_qc

jobid_qc=$(qsub $batchloc/rtofs.ncoda_qc.$pid)
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO ncoda qc is submitted - jobid $jobid_qc
else
  echo 'LAUNCHER ERROR: RTOFS-GLO ncoda qc not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runqc

if [ $runglblvar -eq 1 ]
then
#global var
jobname=rtofs_glbl_var
export jobid=$jobname.$pid
export job=$jobname
#mkdir -p ${myDATAROOT}/$jobid
cat << EOF_ncoda_glbl_var > $batchloc/rtofs.glblvar.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=72:mem=60GB
#PBS -q dev
#PBS -l walltime=00:59:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load intel/${intel_ver}
module load craype/${craype_ver}
module load cray-pals/${cray_pals_ver}
module load cray-mpich/${cray_mpich_ver}
module load cfp/${cfp_ver}
module load hdf5/${hdf5_ver}
module load netcdf/${netcdf4_ver}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=72

$HOMErtofs/jobs/JRTOFS_GLO_NCODA_GLBL_VAR

EOF_ncoda_glbl_var

if [ $runqc -eq 0 ]
then
   jobid_glbl=$(qsub $batchloc/rtofs.glblvar.$pid)
else
   jobid_glbl=$(qsub -W depend=afterok:$jobid_qc $batchloc/rtofs.glblvar.$pid)
fi
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO global var is submitted - jobid $jobid_glbl
else
  echo 'LAUNCHER ERROR: RTOFS-GLO glbl var not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runglblvar

if [ $runpolarvar -eq 1 ]
then
#polar var
jobname=rtofs_polar_var
export jobid=$jobname.$pid
export job=$jobname
#mkdir -p ${myDATAROOT}/$jobid
cat << EOF_ncoda_polar_var > $batchloc/rtofs.polarvar.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=24:mem=4GB
#PBS -q dev
#PBS -l walltime=00:59:00
#PBS -l debug=true
#PBS -V

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load intel/${intel_ver}
module load craype/${craype_ver}
module load cray-pals/${cray_pals_ver}
module load cray-mpich/${cray_mpich_ver}
module load cfp/${cfp_ver}
module load hdf5/${hdf5_ver}
module load netcdf/${netcdf4_ver}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=24

$HOMErtofs/jobs/JRTOFS_GLO_NCODA_POLAR_VAR

EOF_ncoda_polar_var

if [ $runqc -eq 0 ]
then
   jobid_polar=$(qsub $batchloc/rtofs.polarvar.$pid)
else
   jobid_polar=$(qsub -W depend=afterok:$jobid_qc $batchloc/rtofs.polarvar.$pid)
fi
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO polar var is submitted - jobid $jobid_polar
else
  echo 'LAUNCHER ERROR: RTOFS-GLO polar var not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runpolarvar

if [ $run3dvar -eq 1 ]
then
#3dvar
jobname=rtofs_3dvar
export jobid=$jobname.$pid
export job=$jobname
#mkdir -p ${myDATAROOT}/$jobid
cat << EOF_ncoda_3dvar > $batchloc/rtofs.3dvar.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter:exclhost,select=3:ncpus=120
#PBS -l place=excl
#PBS -q dev
#PBS -l walltime=01:59:00
#PBS -l debug=true
#PBS -V

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load intel/${intel_ver}
module load craype/${craype_ver}
module load cray-pals/${cray_pals_ver}
module load cray-mpich/${cray_mpich_ver}
module load cfp/${cfp_ver}
module load hdf5/${hdf5_ver}
module load netcdf/${netcdf4_ver}
module load wgrib2/${wgrib2_ver}
module load libjpeg/${libjpeg_ver}
module load grib_util/${grib_util_ver}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=360

$HOMErtofs/jobs/JRTOFS_GLO_NCODA_3DVAR

EOF_ncoda_3dvar

if [ $runqc -eq 0 ]
then
   jobid_3dvar=$(qsub $batchloc/rtofs.3dvar.$pid)
else
   jobid_3dvar=$(qsub -W depend=afterok:$jobid_qc $batchloc/rtofs.3dvar.$pid)
fi
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO 3dvar is submitted - jobid $jobid_3dvar
else
  echo 'LAUNCHER ERROR: RTOFS-GLO 3dvar not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # run3dvar

if [ $runmom -eq 1 ]
then
#mom
jobname=rtofs_mom
export jobid=$jobname.$pid
export job=$jobname
mkdir -p ${myDATAROOT}/$jobid
cat << EOF_mom > $batchloc/rtofs.mom.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter:exclhost,select=20:ncpus=116:mpiprocs=116
#PBS -q dev
#PBS -l walltime=04:00:00
#PBS -l debug=true
#PBS -V

###  frm denise
###  #PBS -l select=18:ncpus=128:mpiprocs=128:ompthreads=1
#######HYCOM
#####PBS -l place=vscatter:exclhost,select=15:ncpus=120:mpiprocs=120

#module purge
#module load envvar
#module load prod_envir
#module load prod_util
#module load PrgEnv-intel/${PrgEnv_intel_ver}
#module load craype/${craype_ver}
#module load intel/${intel_ver}
#module load cray-pals/${cray_pals_ver}
#module load cray-mpich/${cray_mpich_ver}
#module load cfp/${cfp_ver}
#module load netcdf/${netcdf3_ver}
#module load hdf5/${hdf5_ver}
#module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=2320
export NMPI=2320

cd \$myDATAROOT/\$jobid

inputdir=/lfs/h2/emc/couple/noscrub/dan.iredell/COMDIR/prod/com/rtofs/v2.5/dw.workdir
# copy some stuff
for d in data_table datm_in datm.streams diag_table fd_ufs.yaml ice_in input.nml job_card model_configure module-setup.sh noahmptable.tbl ufs.configure
do
  cp -p \$inputdir/\$d .
done

# link some stuff (files)
for d in cice_model.res.nc fv3.exe grid_cice_NEMS_mx008.nc kmtu_cice_NEMS_mx008.nc mesh.mx008.nc 
do
  ln -sf \$inputdir/\$d .
done

# link some stuff (dirs)
for d in INPUT modulefiles
do
  mkdir \$d
  ln -sf \$inputdir/\$d/* \$d/.
done

# create and populate MOM6_OUTPUT 
mkdir MOM6_OUTPUT
cp -p \$inputdir/MOM6_OUTPUT/MOM_parameter_doc.all MOM6_OUTPUT

# create and (donot) populate history 
mkdir history
#cp -p \$inputdir/history history

# create directory RESTART
mkdir RESTART



set -eux
echo " \$( date +%Y%m%d-%H:%M:%S )" >  job_timestamp.txt

set +x
MACHINE_ID=wcoss2
source ./module-setup.sh
module use ./modulefiles
module load modules.fv3
module load cray-pals
module list
set -x

export OMP_NUM_THREADS=1

export ESMF_RUNTIME_COMPLIANCECHECK=OFF:depth=4
export ESMF_RUNTIME_PROFILE=ON
export ESMF_RUNTIME_PROFILE_OUTPUT="SUMMARY"

export FI_OFI_RXM_RX_SIZE=40000
export FI_OFI_RXM_TX_SIZE=40000
export FI_OFI_RXM_SAR_LIMIT=3145728
export OMP_PLACES=cores
export OMP_STACKSIZE=2048M


mpiexec -np \$NMPI --cpu-bind core  ./fv3.exe

### from dw
### mpiexec -n 2304 -ppn 128 --cpu-bind depth -depth 1 ./fv3.exe

echo " \$( date +%Y%m%d-%H:%M:%S )" >> job_timestamp.txt

EOF_mom

jobid_mom=$(qsub $batchloc/rtofs.mom.$pid)
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO mom is submitted - jobid $jobid_mom
else
  echo 'LAUNCHER ERROR: RTOFS-GLO mom not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runmom

exit 0
