#!/bin/bash
#
# Launch 2dvar workflow
#

if [ $# -ne 2 ] 
then
  echo "USAGE: $0 <configname> <YYYYMMDDHH>"
  exit -2
fi

configname=$1
todayhh=$2
today=$(echo $todayhh | cut -c1-8)
icyc=$(echo $todayhh | cut -c9-10)
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
export tmproot=/lfs/h2/emc/ptmp/$LOGNAME/$simulation

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

export cyc=$icyc
export cycle=t${cyc}z
export envir=prod # prod or para or canned

export HOMErtofs=${PROJECTdir}
export HOMErtofs_glo=$HOMErtofs

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
export sim=${sim:-zz}
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
export myDATAROOT=$tmproot/${projID}/$PDY$cyc
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
testjustthisjob=0
if [ $testjustthisjob -eq 1 ]
then

#global var
jobname=rtofs_glbl_2dvar_${cyc}
export jobid=$jobname.$pid
export job=$jobname
#mkdir -p ${myDATAROOT}/$jobid
cat << EOF_ncoda_glbl_var > $batchloc/rtofs.glblvar.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=72:mem=160GB
#PBS -q dev
#PBS -l walltime=00:20:00
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

$HOMErtofs/jobs/JRTOFS_GLO_NCODA_GLBL_VAR_2DVAR

EOF_ncoda_glbl_var

jobid_glbl=$(qsub $batchloc/rtofs.glblvar.$pid)
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO global var is submitted - jobid $jobid_glbl
else
  echo 'LAUNCHER ERROR: RTOFS-GLO glbl var not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi


echo justthisonejob jobs

exit
fi #testjustthisjob

echo DATAROOT is $myDATAROOT
echo

#############
jobname=rtofs_qc_2dvar_${cyc}
export jobid=$jobname.$pid
export job=$jobname
#mkdir -p ${myDATAROOT}/$jobid
cat << EOF_ncoda_qc > $batchloc/rtofs.2dvar_qc.$pid
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

$HOMErtofs/jobs/JRTOFS_GLO_NCODA_QC_2DVAR

EOF_ncoda_qc

jobid_qc=$(qsub $batchloc/rtofs.2dvar_qc.$pid)
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO ncoda qc is submitted - jobid $jobid_qc
else
  echo 'LAUNCHER ERROR: RTOFS-GLO ncoda qc not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi

echo only QC
exit

#global var 
jobname=rtofs_glbl_2dvar_${cyc}
export jobid=$jobname.$pid
export job=$jobname
#mkdir -p ${myDATAROOT}/$jobid
cat << EOF_ncoda_glbl_var > $batchloc/rtofs.glblvar.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=72:mem=160GB
#PBS -q dev
#PBS -l walltime=00:20:00
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

$HOMErtofs/jobs/JRTOFS_GLO_NCODA_GLBL_VAR_2DVAR

EOF_ncoda_glbl_var

jobid_glbl=$(qsub -W depend=afterok:$jobid_qc $batchloc/rtofs.glblvar.$pid)
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO global var is submitted - jobid $jobid_glbl
else
  echo 'LAUNCHER ERROR: RTOFS-GLO glbl var not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi


exit 0
