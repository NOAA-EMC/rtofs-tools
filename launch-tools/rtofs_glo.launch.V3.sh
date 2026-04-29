#!/bin/bash

###  
# LAUNCH SCRIPT FOR RTOFS V3 on WCOSS.
# usage: configname yyyymmdd
# where
#   configname is the config file that specifies the run environment
#     and jobs to be run
#   yyyymmdd is the date to be run
# 
# This script creates here-scripts to submit each job in rtofs v3.
# The here-scripts created and launched is controlled by the config file.
#
# REQUIRED
# simulation - name of this simulation (anything you want)
# projectroot - location of the rtofs v3 package
# comroot - location of the com output (do not include rtofs/...)
#
# OPTIONAL
# inputroot - location of dcom data. Defaults to /lfs/h1/ops/prod/dcom.
# GETGES_COM - location of gfs data. Defaults to gfs com.
# queue - PBS queue where jobs will run. Defaults to dev
#
# JOB CONTROL - all these default to 0 if not specified. The run
# dependencies are automatic (this script will take care of that for you).
#
#* NCODA **
# runqc 
# runglblvar
# runpolarvar
# run3dvar
#* INCREMENTAL UPDATE **
# runncodainc
# runufsincup
#* ANALYSIS **
# runanalpre
# runufsanal
# runanalgribpost
#* FORECAST STEP1 **
# runfcst1pre
# runufsfcst1
# runfcst1gribpost
#* FORECAST STEP2 **
# runfcst2pre
# runufsfcst2
# runfcst2gribpost
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
queue=${queue:-dev}

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

## 20260226 - don't load these
#module purge
#module load envvar
#module load prod_envir
#module load prod_util
#module load PrgEnv-intel/${PrgEnv_intel_ver}
#module load intel/${intel_ver}
#module load craype/${craype_ver}
#module load cray-pals/${cray_pals_ver}
#module load cray-mpich/${cray_mpich_ver}
#module load cfp/${cfp_ver}
#module load bufr_dump/${bufr_dump_ver}
#module load hdf5/${hdf5_ver}
#module load netcdf/${netcdf4_ver}
#module load wgrib2/${wgrib2_ver}
#module load libjpeg/${libjpeg_ver}
#module load grib_util/${grib_util_ver}
#module load gempak/${gempak_ver}
#module load cdo/${cdo_ver}
#module list
module load prod_envir
module load prod_util

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

# Set system/model vars
export RUN=rtofs
export NET=rtofs
export modID=glo
export inputgrid=0p08
. $PROJECTdir/parm/${RUN}_${modID}.config
export fcstdays=`expr ${fcstdays_step1} + ${fcstdays_step2}`

echo forecast days -- $analdays + $fcstdays_step1 + $fcstdays_step1

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

# which jobs are we running (see config file for settings)
export simulation=${simulation:-sim}
runqc=${runqc:-0}
runglblvar=${runglblvar:-0}
runpolarvar=${runpolarvar:-0}
run3dvar=${run3dvar:-0}
runncodainc=${runncodainc:-0}
runufsincup=${runufsincup:-0}
runanalpre=${runanalpre:-0}
runufsanal=${runufsanal:-0}
runanalgribpost=${runanalgribpost:-0}
runfcst1pre=${runfcst1pre:-0}
runufsfcst1=${runufsfcst1:-0}
runfcst1gribpost=${runfcst1gribpost:-0}
runfcst2pre=${runfcst2pre:-0}
runufsfcst2=${runufsfcst2:-0}
runfcst2post=${runfcst2post:-0}
runspecial=${runspecial:-0}

runmom0=0   # old way

echo DATAROOT is $myDATAROOT
echo

####### justhis
#here

if [ $runspecial -eq 1 ]
then
echo no special job
fi # runspecial
#there

#################################################

if [ $runqc -eq 1 ]
then
jobname=rtofs_ncoda_qc
export jobid=$jobname.$pid
export job=$jobname
cat << EOF_ncoda_qc > $batchloc/rtofs.ncoda_qc.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=12:mem=90GB
#PBS -q $queue
#PBS -l walltime=00:59:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver_rtofs}
module load intel/${intel_ver_rtofs}
module load craype/${craype_ver_rtofs}
module load cray-pals/${cray_pals_ver_rtofs}
module load cfp/${cfp_ver_rtofs}
module load bufr_dump/${bufr_dump_ver_rtofs}
module load hdf5/${hdf5_ver_rtofs}
module load netcdf/${netcdf4_ver_rtofs}
module load wgrib2/${wgrib2_ver_rtofs}
module load libjpeg/${libjpeg_ver_rtofs}
module load grib_util/${grib_util_ver_rtofs}
#module load ve/rtofs/${ve_rtofs_ver}
module load ve/hafs/${ve_hafs_ver}
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
cat << EOF_ncoda_glbl_var > $batchloc/rtofs.glblvar.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=72:mem=60GB
#PBS -q $queue
#PBS -l walltime=00:59:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver_rtofs}
module load intel/${intel_ver_rtofs}
module load craype/${craype_ver_rtofs}
module load cray-pals/${cray_pals_ver_rtofs}
module load cray-mpich/${cray_mpich_ver_rtofs}
module load cfp/${cfp_ver_rtofs}
module load hdf5/${hdf5_ver_rtofs}
module load netcdf/${netcdf4_ver_rtofs}
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
cat << EOF_ncoda_polar_var > $batchloc/rtofs.polarvar.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=24:mem=4GB
#PBS -q $queue
#PBS -l walltime=00:59:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver_rtofs}
module load intel/${intel_ver_rtofs}
module load craype/${craype_ver_rtofs}
module load cray-pals/${cray_pals_ver_rtofs}
module load cray-mpich/${cray_mpich_ver_rtofs}
module load cfp/${cfp_ver_rtofs}
module load hdf5/${hdf5_ver_rtofs}
module load netcdf/${netcdf4_ver_rtofs}
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
cat << EOF_ncoda_3dvar > $batchloc/rtofs.3dvar.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter:exclhost,select=3:ncpus=120
#PBS -l place=excl
#PBS -q $queue
#PBS -l walltime=01:59:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver_rtofs}
module load intel/${intel_ver_rtofs}
module load craype/${craype_ver_rtofs}
module load cray-pals/${cray_pals_ver_rtofs}
module load cray-mpich/${cray_mpich_ver_rtofs}
module load cfp/${cfp_ver_rtofs}
module load HDf5/${hdf5_ver_rtofs}
module load netcdf/${netcdf4_ver_rtofs}
module load wgrib2/${wgrib2_ver_rtofs}
module load libjpeg/${libjpeg_ver_rtofs}
module load grib_util/${grib_util_ver_rtofs}
module load udunits/${udunits_ver_rtofs}
module load gsl/${gsl_ver_rtofs}
module load nco/${nco_ver_rtofs}
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

if [ $runncodainc -eq 1 ]
then
#ncoda increment
jobname=rtofs_ncoda_inc
export jobid=$jobname.$pid
export job=$jobname
cat << EOF_ncoda_inc > $batchloc/rtofs.ncoda.inc.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=1:mem=50GB
#PBS -q $queue
#PBS -l walltime=00:30:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver_rtofs}
module load intel/${intel_ver_rtofs}
module load craype/${craype_ver_rtofs}
module load cray-pals/${cray_pals_ver_rtofs}
module load cray-mpich/${cray_mpich_ver_rtofs}
module load hdf5/${hdf5_ver}
module load netcdf/${netcdf4_ver_rtofs}
module load wgrib2/${wgrib2_ver_rtofs}
module load libjpeg/${libjpeg_ver_rtofs}
module load grib_util/${grib_util_ver_rtofs}
#module load ve/rtofs/${ve_rtofs_ver}
module load ve/hafs/${ve_hafs_ver}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=1

$HOMErtofs/jobs/JRTOFS_GLO_NCODA_INC

EOF_ncoda_inc

if [ $run3dvar -eq 0 ]
then
   jobid_ncoda_inc=$(qsub $batchloc/rtofs.ncoda.inc.$pid)
else
   jobid_ncoda_inc=$(qsub -W depend=afterok:$jobid_3dvar $batchloc/rtofs.ncoda.inc.$pid)
fi
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO ncoda inc is submitted - jobid $jobid_ncoda_inc
else
  echo 'LAUNCHER ERROR: RTOFS-GLO ncoda_inc not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runncodainc

if [ $runufsincup -eq 1 ]
then
jobname=rtofs_incup
export jobid=$jobname.$pid
export job=$jobname
cat << EOF_ufsincup > $batchloc/rtofs.incup.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter:exclhost,select=20:ncpus=128:mpiprocs=128
#PBS -q $queue
#PBS -l walltime=04:00:00
#PBS -l debug=true
#PBS -V

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=2560
export NMPI=2560

set +x
module list
MACHINE_ID=wcoss2
source ${HOMErtofs_glo}/modulefiles/module-setup.sh
module use ${HOMErtofs_glo}/modulefiles
module load modules.fv3
module reset
module use ${HOMErtofs}/sorc/ufs_utils.fd/modulefiles
module load build.wcoss2.intel
module load cray-pals
module load prod_util
module load prod_envir
module load cfp
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
export FI_VERBS_PREFER_XRC=1

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=2560
export NMPI=2560

$HOMErtofs/jobs/JRTOFS_GLO_INCUP

EOF_ufsincup

if [ $runncodainc -eq 0 ]
then
   jobid_incup=$(qsub $batchloc/rtofs.incup.$pid)
else
   jobid_incup=$(qsub -W depend=afterok:$jobid_ncoda_inc $batchloc/rtofs.incup.$pid)
fi
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO incup is submitted - jobid $jobid_incup
else
  echo 'LAUNCHER ERROR: RTOFS-GLO incup not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runufsincup

#preanalysis
if [ $runanalpre -eq 1 ]
then
jobname=rtofs_analysis_pre
export jobid=$jobname.$pid
export job=$jobname

cat << EOF_analpre > $batchloc/rtofs.analpre.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=1:mem=10GB
#PBS -q $queue
#PBS -l walltime=00:50:00
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
module load cfp/${cfp_ver}
module load wgrib2/${wgrib2_ver}
module load libjpeg/${libjpeg_ver}
module load grib_util/${grib_util_ver}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=1

$HOMErtofs/jobs/JRTOFS_GLO_ANALYSIS_PRE

EOF_analpre

if [ $runqc -eq 0 ]
then
   jobid_preanal=$(qsub $batchloc/rtofs.analpre.$pid)
else
   jobid_preanal=$(qsub -W depend=afterok:$jobid_qc $batchloc/rtofs.analpre.$pid)
fi
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO pre-analysis var is submitted - jobid $jobid_polar
else
  echo 'LAUNCHER ERROR: RTOFS-GLO pre-analysis not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runanalpre

# analysis
if [ $runufsanal -eq 1 ]
then
jobname=rtofs_anal
export jobid=$jobname.$pid
export job=$jobname
#export numberofprocs=2688
#export numberofprocs=2944
export numberofprocs=2560
cat << EOF_ufsanal > $batchloc/rtofs.anal.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter:exclhost,select=20:ncpus=128:mpiprocs=128
#PBS -q $queue
#PBS -l walltime=04:00:00
#PBS -l debug=true
#PBS -V

######PBS -l place=vscatter:exclhost,select=20:ncpus=128:mpiprocs=128
######PBS -l place=vscatter:exclhost,select=21:ncpus=128:mpiprocs=128
######PBS -l place=vscatter:exclhost,select=23:ncpus=128:mpiprocs=128

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=$numberofprocs
export NMPI=$numberofprocs

set +x
MACHINE_ID=wcoss2
source ${HOMErtofs_glo}/modulefiles/module-setup.sh
module use ${HOMErtofs_glo}/modulefiles
module load modules.fv3
module reset
module use ${HOMErtofs}/sorc/ufs_utils.fd/modulefiles
module load build.wcoss2.intel
module load cray-pals
module load prod_util
module load prod_envir
module load cfp
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
export FI_VERBS_PREFER_XRC=1

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=$numberofprocs
export NMPI=$numberofprocs

$HOMErtofs/jobs/JRTOFS_GLO_ANALYSIS

EOF_ufsanal

if [[ $runufsincup -eq 0 && $runanalpre -eq 0 ]]
then
   jobid_anal=$(qsub $batchloc/rtofs.anal.$pid)
else
   dep="-W depend=afterok"
   if [ $runufsincup -eq 1 ]
   then
     dep="$dep:$jobid_incup"
   fi
   if [ $runanalpre -eq 1 ]
   then
     dep="$dep:$jobid_preanal"
   fi
   jobid_anal=$(qsub $dep $batchloc/rtofs.anal.$pid)
fi

if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO analysis is submitted - jobid $jobid_anal
else
  echo 'LAUNCHER ERROR: RTOFS-GLO analysis not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runufsanal

#anal grib post
if [ $runanalgribpost -eq 1 ]
then
jobname=rtofs_anal_grib_post.d01
export jobid=$jobname.$pid
export job=$jobname
cat << EOF_analgribpost > $batchloc/rtofs.anal.grib.post.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=11:mem=160GB
#PBS -q $queue
#PBS -l walltime=02:00:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver_rtofs}
module load intel/${intel_ver_rtofs}
module load craype/${craype_ver_rtofs}
module load cray-pals/${cray_pals_ver_rtofs}
module load cfp/${cfp_ver_rtofs}
module load hdf5/${hdf5_ver_rtofs}
module load netcdf/${netcdf4_ver_rtofs}
module load wgrib2/${wgrib2_ver_rtofs}
module load libjpeg/${libjpeg_ver_rtofs}
module load grib_util/${grib_util_ver_rtofs}
module load cdo/${cdo_ver_rtofs}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=11

$HOMErtofs/jobs/JRTOFS_GLO_ANALYSIS_GRIB2_POST

EOF_analgribpost

if [ $runufsanal -eq 0 ]
then
   jobid_analgribpost=$(qsub $batchloc/rtofs.anal.grib.post.$pid)
else
   jobid_analgribpost=$(qsub -W depend=afterok:$jobid_anal $batchloc/rtofs.anal.grib.post.$pid)
fi

if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO analysis grib post is submitted - jobid $jobid_analgribpost
else
  echo 'LAUNCHER ERROR: RTOFS-GLO analysis grib post not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runanalgribpost

# runfcst1pre
if [ $runfcst1pre -eq 1 ]
then
jobname=rtofs_fcst1_pre
export jobid=$jobname.$pid
export job=$jobname

cat << EOF_fcst1pre > $batchloc/rtofs.fcst1pre.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=1:mem=10GB
#PBS -q $queue
#PBS -l walltime=00:50:00
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
module load cfp/${cfp_ver}
module load wgrib2/${wgrib2_ver}
module load libjpeg/${libjpeg_ver}
module load grib_util/${grib_util_ver}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=1

$HOMErtofs/jobs/JRTOFS_GLO_FORECAST_STEP1_PRE

EOF_fcst1pre

if [ $runanalpre -eq 0 ]
then
   jobid_prefcst1=$(qsub $batchloc/rtofs.fcst1pre.$pid)
else
   jobid_prefcst1=$(qsub -W depend=afterok:$jobid_preanal $batchloc/rtofs.fcst1pre.$pid)
fi
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO pre-forecast1 var is submitted - jobid $jobid_polar
else
  echo 'LAUNCHER ERROR: RTOFS-GLO pre-forecast1 not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runfcst1pre

# runufsfcst1
if [ $runufsfcst1 -eq 1 ]
then
jobname=rtofs_fcst1
export jobid=$jobname.$pid
export job=$jobname
cat << EOF_ufsfcst1 > $batchloc/rtofs.fcst1.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter:exclhost,select=20:ncpus=128:mpiprocs=128
#PBS -q $queue
#PBS -l walltime=03:00:00
#PBS -l debug=true
#PBS -V

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=2560
export NMPI=2560

set +x
MACHINE_ID=wcoss2
source ${HOMErtofs_glo}/modulefiles/module-setup.sh
module use ${HOMErtofs_glo}/modulefiles
module load modules.fv3
module reset
module use ${HOMErtofs}/sorc/ufs_utils.fd/modulefiles
module load build.wcoss2.intel
module load cray-pals
module load prod_util
module load prod_envir
module load cfp
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
export FI_VERBS_PREFER_XRC=1

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=2560
export NMPI=2560

$HOMErtofs/jobs/JRTOFS_GLO_FORECAST_STEP1

EOF_ufsfcst1

if [ $runufsanal -eq 0 ]
then
   jobid_fcst1=$(qsub $batchloc/rtofs.fcst1.$pid)
else
   jobid_fcst1=$(qsub -W depend=afterok:$jobid_anal $batchloc/rtofs.fcst1.$pid)
fi

if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO fcst1 is submitted - jobid $jobid_fcst1
else
  echo 'LAUNCHER ERROR: RTOFS-GLO fcst1 not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runufsfcst1

# fcst1 grib post
if [ $runfcst1gribpost -eq 1 ]
then
fcst_grib_post_days=$fcstdays_step1
if [ $fcstdays_step1 -eq 4 ];then fcst_grib_post_days=3;fi
for NN in $(seq -w 01 01 $fcst_grib_post_days)
do
  jobname=rtofs_forecast_grib_post.d${NN}
  export jobid=$jobname.$pid
  export job=$jobname
  export NN
cat << EOF_fcst1gribpost > $batchloc/rtofs.fcst1.grib.post.$NN.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=11:mem=160GB
#PBS -q $queue
#PBS -l walltime=02:00:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver_rtofs}
module load intel/${intel_ver_rtofs}
module load craype/${craype_ver_rtofs}
module load cray-pals/${cray_pals_ver_rtofs}
module load cfp/${cfp_ver_rtofs}
module load hdf5/${hdf5_ver_rtofs}
module load netcdf/${netcdf4_ver_rtofs}
module load wgrib2/${wgrib2_ver_rtofs}
module load libjpeg/${libjpeg_ver_rtofs}
module load grib_util/${grib_util_ver_rtofs}
module load cdo/${cdo_ver_rtofs}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=11

$HOMErtofs/jobs/JRTOFS_GLO_FORECAST_GRIB2_POST

EOF_fcst1gribpost

if [ $runufsfcst1 -eq 0 ]
then
   jobid_fcst1gribpost=$(qsub $batchloc/rtofs.fcst1.grib.post.$NN.$pid)
else
   jobid_fcst1gribpost=$(qsub -W depend=afterok:$jobid_fcst1 $batchloc/rtofs.fcst1.grib.post.$NN.$pid)
fi

if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO forecast1 grib post is submitted - jobid $jobid_fcst1gribpost
else
  echo 'LAUNCHER ERROR: RTOFS-GLO forecast1 grib post not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
done
fi # runfcst1gribpost

# runfcst2pre
if [ $runfcst2pre -eq 1 ]
then
jobname=rtofs_fcst2_pre
export jobid=$jobname.$pid
export job=$jobname

cat << EOF_fcst2pre > $batchloc/rtofs.fcst2pre.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=1:mem=10GB
#PBS -q $queue
#PBS -l walltime=00:50:00
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
module load cfp/${cfp_ver}
module load wgrib2/${wgrib2_ver}
module load libjpeg/${libjpeg_ver}
module load grib_util/${grib_util_ver}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=1

$HOMErtofs/jobs/JRTOFS_GLO_FORECAST_STEP1_PRE

EOF_fcst2pre

if [ $runfcst1pre -eq 0 ]
then
   jobid_prefcst2=$(qsub $batchloc/rtofs.fcst2pre.$pid)
else
   jobid_prefcst2=$(qsub -W depend=afterok:$jobid_prefcst1 $batchloc/rtofs.fcst2pre.$pid)
fi
if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO pre-forecast1 var is submitted - jobid $jobid_polar
else
  echo 'LAUNCHER ERROR: RTOFS-GLO pre-forecast1 not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runfcst2pre

# runufsfcst2
if [ $runufsfcst2 -eq 1 ]
then
jobname=rtofs_fcst2
export jobid=$jobname.$pid
export job=$jobname
cat << EOF_ufsfcst2 > $batchloc/rtofs.fcst2.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter:exclhost,select=20:ncpus=128:mpiprocs=128
#PBS -q $queue
#PBS -l walltime=10:00:00
#PBS -l debug=true
#PBS -V

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=2560
export NMPI=2560

set +x
MACHINE_ID=wcoss2
source ${HOMErtofs_glo}/modulefiles/module-setup.sh
module use ${HOMErtofs_glo}/modulefiles
module load modules.fv3
module reset
module use ${HOMErtofs}/sorc/ufs_utils.fd/modulefiles
module load build.wcoss2.intel
module load cray-pals
module load prod_util
module load prod_envir
module load cfp
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
export FI_VERBS_PREFER_XRC=1

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=2560
export NMPI=2560

$HOMErtofs/jobs/JRTOFS_GLO_FORECAST_STEP2

EOF_ufsfcst2

if [ $runufsfcst1 -eq 0 ]
then
   jobid_fcst2=$(qsub $batchloc/rtofs.fcst2.$pid)
else
   jobid_fcst2=$(qsub -W depend=afterok:$jobid_fcst1 $batchloc/rtofs.fcst2.$pid)
fi

if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO fcst2 is submitted - jobid $jobid_fcst2
else
  echo 'LAUNCHER ERROR: RTOFS-GLO fcst2 not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
fi # runufsfcst2

# fcst2 grib post
if [ $runfcst2gribpost -eq 1 ]
then
for NN in 04
do
  jobname=rtofs_forecast_grib_post.d${NN}
  export jobid=$jobname.$pid
  export job=$jobname
  export NN
cat << EOF_fcst2gribpost > $batchloc/rtofs.fcst2.grib.post.$NN.$pid
#!/bin/bash
#PBS -N $jobname
#PBS -j oe
#PBS -A $account
#PBS -l place=vscatter,select=1:ncpus=11:mem=160GB
#PBS -q $queue
#PBS -l walltime=02:00:00
#PBS -l debug=true
#PBS -V

source ${HOMErtofs_glo}/versions/run.ver

module purge
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel/${PrgEnv_intel_ver_rtofs}
module load intel/${intel_ver_rtofs}
module load craype/${craype_ver_rtofs}
module load cray-pals/${cray_pals_ver_rtofs}
module load cfp/${cfp_ver_rtofs}
module load hdf5/${hdf5_ver_rtofs}
module load netcdf/${netcdf4_ver_rtofs}
module load wgrib2/${wgrib2_ver_rtofs}
module load libjpeg/${libjpeg_ver_rtofs}
module load grib_util/${grib_util_ver_rtofs}
module load cdo/${cdo_ver_rtofs}
module list

export COMROOT=$myCOMROOT
export DATAROOT=$myDATAROOT
export NPROCS=11

$HOMErtofs/jobs/JRTOFS_GLO_FORECAST_GRIB2_POST

EOF_fcst2gribpost

if [ $runufsfcst2 -eq 0 ]
then
   jobid_fcst2gribpost=$(qsub $batchloc/rtofs.fcst2.grib.post.$NN.$pid)
else
   jobid_fcst2gribpost=$(qsub -W depend=afterok:$jobid_fcst2 $batchloc/rtofs.fcst2.grib.post.$NN.$pid)
fi

if [ $# -gt 0 ]
then
  echo LAUNCHER: RTOFS-GLO forecast2 grib post is submitted - jobid $jobid_fcst2gribpost
else
  echo 'LAUNCHER ERROR: RTOFS-GLO forecast2 grib post not submitted at host '`hostname`' at '`date` "error is $#"
  exit
fi
done
fi # runfcst2gribpost


exit

