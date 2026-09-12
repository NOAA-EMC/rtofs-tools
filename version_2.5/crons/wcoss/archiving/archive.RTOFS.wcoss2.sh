#!/bin/sh

# you can modify these. 
# if envir is prod, then should set rtofs=N as NCO archives those.
rtofs=N
ncoda=Y
envir=prod
version=v2.4

# current HPSS location 
#hdir=/NCEPDEV/emc-ocean/5year/emc.ncodapa/rtofs.v2
hdir=/NCEPDEV/emc-ocean/5year/Dan.Iredell/wcoss2.prod

######################################################################################

## Check if running on prod
# note -- on wcoss2 could use /etc/cluster_name for full name
h=$( hostname | cut -c1-1 )
case "$h" in
  c) host=cactus ;;
  d) host=dogwood ;;
  *) host=nobody
esac

#wcoss1
#hprod=$( cat /etc/prod )
#hdev=$( cat /etc/dev )

#wcoss2
hprod=$(grep primary /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)
hdev=$(grep backup /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)

if [[ $host == $hprod ]]; then
  echo "$host is the prod machine - I will proceed"
elif [[ $host == $hdev ]]; then
  echo "$host is the dev machine - I will exit"
  exit
else
  echo "Unknown machine - I will exit"
  exit
fi

#wcoss2
module load envvar
module load prod_envir
module load prod_util
module load PrgEnv-intel

#############################################################################################################
#functions 

# build the rtofs archival script
function build_archive_rtofs_ab
{
  rday=$1
  echo building rtofs ab archive job for $rday
  tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/archive
  mkdir -p $tmpdir

cat << eofrab > $tmpdir/rtofs_archive_ab.$rday.$$
#!/bin/bash
#PBS -N jrtofs_archive_ab
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q dev_transfer
#PBS -l walltime=06:00:00
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

PDY=$rday
PDYm1=\`\$NDATE -24 \${PDY}00\`
PDYm2=\`\$NDATE -48 \${PDY}00\`
COMOUT=$bdir/rtofs.\$PDY
cd \$COMOUT

echo PDY \$PDY
echo COMOUT \$COMOUT

hsi mkd $hdir/rtofs.\$PDY

#ab in 5year
rm -f $tmpdir/rtofs.ab.\$PDY.list
for ab in \`ls rtofs_glo.t00z.*.arch?.*\`
do
  p12=\`echo \$ab | cut -d. -f1-2\`
  p3=\`echo \$ab | cut -d. -f3\`
  p3num=\`echo \$p3 | cut -c 2-\`
  if [ \$p3num -lt 10 ]
  then
     p3mod=0
  else
     p3mod=\$((\$p3num % 3 ))
  fi
  p5=\`echo \$ab | cut -d. -f5-\`
  qnum=no
  if [[ \$p3mod -eq 0 || \$p3num -le 24 ]]
  then
    qnum=yes
  fi
  qsuf=no
  if [[ \$p5 == 'b' || \$p5 == 'a.tgz' ]]
  then
    qsuf=yes
  fi
  if [[ \$qnum == 'yes' && \$qsuf == 'yes' ]]
  then
    echo \$ab >> $tmpdir/rtofs.ab.\$PDY.list
  fi
done
for ci in \`ls *cice_inst\`;
do
  echo \$ci >> /lfs/h2/emc/ptmp/$LOGNAME/archive/rtofs.ab.\$PDY.list
done

htar -cvf $hdir/rtofs.\$PDY/rtofs.ab.tar -L $tmpdir/rtofs.ab.\$PDY.list

eofrab

cd $tmpdir
qsub ./rtofs_archive_ab.$rday.$$

}

# build the rtofs archival (non-ab) script
function build_archivex_rtofs
{
  rday=$1
  echo building rtofs archive job for $rday
  tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/archive
  mkdir -p $tmpdir

cat << eofrx > $tmpdir/rtofs_archivex.$rday.$$
#!/bin/bash
#PBS -N jrtofs_archivex
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q dev_transfer
#PBS -l walltime=06:00:00
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

PDY=$rday
PDYm1=\`\$NDATE -24 \${PDY}00\`
PDYm2=\`\$NDATE -48 \${PDY}00\`
COMOUT=$bdir/rtofs.\$PDY
cd \$COMOUT

echo PDY \$PDY
echo COMOUT \$COMOUT

hsi mkd $hdir/rtofs.\$PDY

#restart in permanent
htar -cvf $hdir/rtofs.\$PDY/rtofs.restart.tar rtofs_glo.t00z.n00.restart.a.tgz rtofs_glo.t00z.n00.restart.b rtofs_glo.t00z.n00.restart_cice.tgz rtofs_glo.t00z.n-06.restart.a.tgz rtofs_glo.t00z.n-06.restart.b rtofs_glo.t00z.n-06.restart_cice.tgz rtofs_glo.t00z.n-24.restart.a.tgz rtofs_glo.t00z.n-24.restart.b rtofs_glo.t00z.n-24.restart_cice.tgz

# ncoda in ??  (new)
htar -cvf $hdir/rtofs.\$PDY/rtofs.ncoda.tar ./ncoda/ocnqc ./ncoda/logs ./ncoda/hycom_var/restart/*\${PDYm1}*analfld ./ncoda/hycom_var/restart/*\${PDYm1}*analinc

# ncoda for zulema
cd \$COMOUT/ncoda
htar -cvf $hdir/rtofs.\$PDY/rtofs.da.tar logs/*/*\${PDYm1}* hycom_var/restart/*\${PDYm1}* hycom_var/restart/*\${PDYm2}_000[1-9]* hycom_var/restart/*\${PDYm2}_00[1-3]* glbl_var/restart/*\${PDYm1}*

# archv_1_inc for zulema
cd \$COMOUT
htar -cvf $hdir/rtofs.\$PDY/rtofs_archv_1_inc.tar rtofs_glo.archv_1_inc.* rtofs_glo.incupd.*

# forcing for analysis and forecast-1
cd \$COMOUT
htar -cvf $hdir/rtofs.\$PDY/rtofs.forcing.tar rtofs_glo.anal.t00z* rtofs_glo.fcst1.t00z*

# dump files
cd \$COMOUT/dump
htar -cvf $hdir/rtofs.\$PDY/dump_\${PDY}.tar *

# nc and grb in 1year (run last)
htar -cvf $hdir/rtofs.\$PDY/rtofs.ncgrb.tar *nc *grb2

eofrx

cd $tmpdir
qsub ./rtofs_archivex.$rday.$$

}

# build the ncoda hycom_var archival script
function build_archive_ncoda_hycom_var
{
  nday=$1
  echo building ncoda archive job for $nday
  tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/archive
  mkdir -p $tmpdir

cat << eofnh > $tmpdir/ncoda_archive_hycom_var.$nday.$$
#!/bin/bash
#PBS -N jncoda_archive_hycomvar
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q dev_transfer
#PBS -l walltime=06:00:00
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

PDY=$nday
PDYm1=\`\$NDATE -24 \${PDY}00\`
PDYm2=\`\$NDATE -48 \${PDY}00\`
COMOUT=$bdir/rtofs.\$PDY

echo PDY \$PDY
echo COMOUT \$COMOUT

hsi mkd $hdir/rtofs.\$PDY
date

cd \$COMOUT/ncoda
ls -l ./hycom_var/restart/* > $tmpdir/listing.hycom_var.restart.\$PDY
cd $tmpdir
htar -cvf $hdir/rtofs.\$PDY/hycom_var_listing.tar ./listing.hycom_var.restart.\$PDY

#real time runs are run on PDY, but retrospectives and catch-up runs are behind in date
#so need to find the date the hycom_var was run.  This looks like it will work:

cd \$COMOUT/ncoda
rundate=\`stat -c %y ./logs/hycom_var/hycom_var.\${PDYm1}.out | cut -c1-4,6-7,9-10\`

rm -f $tmpdir/hycom_var.\$PDY.list
for f in \`ls ./hycom_var/restart\`
do
  ft=\`stat -c %y ./hycom_var/restart/\$f | cut -c1-4,6-7,9-10\`
  if [ \$rundate -eq \$ft ]
  then
    echo ./hycom_var/restart/\$f >> $tmpdir/hycom_var.\$PDY.list
  fi
done

htar -cvf $hdir/rtofs.\$PDY/hycom.tar -L $tmpdir/hycom_var.\$PDY.list
date

# other stuff in rtofs 
# ncoda for zulema
cd \$COMOUT/ncoda
htar -cvf $hdir/rtofs.\$PDY/rtofs.da.tar logs/*/*\${PDYm1}* hycom_var/restart/*\${PDYm1}* hycom_var/restart/*\${PDYm2}_000[1-9]* hycom_var/restart/*\${PDYm2}_00[1-3]* glbl_var/restart/*\${PDYm1}*

# archv_1_inc for zulema
cd \$COMOUT
htar -cvf $hdir/rtofs.\$PDY/rtofs_archv_1_inc.tar rtofs_glo.archv_1_inc.* rtofs_glo.incupd.*

# forcing for analysis and forecast-1
cd \$COMOUT
htar -cvf $hdir/rtofs.\$PDY/rtofs.forcing.tar rtofs_glo.anal.t00z* rtofs_glo.fcst1.t00z*

# dump files
cd \$COMOUT/dump
htar -cvf $hdir/rtofs.\$PDY/dump_\${PDY}.tar *



eofnh

cd $tmpdir
qsub ./ncoda_archive_hycom_var.$nday.$$

}

# build the ncoda (non hycom_var) archival script
function build_archive_ncoda_therest
{
  nday=$1
  echo building ncoda therest archive job for $nday
  tmpdir=/lfs/h2/emc/ptmp/$LOGNAME/archive
  mkdir -p $tmpdir

cat << eofn > $tmpdir/ncoda_archive_therest.$nday.$$
#!/bin/bash
#PBS -N jncoda_archiv_thereste
#PBS -j oe
#PBS -A RTOFS-DEV
#PBS -l place=vscatter,select=1:ncpus=1
#PBS -q dev_transfer
#PBS -l walltime=06:00:00
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

PDY=$nday
PDYm1=\`\$NDATE -24 \${PDY}00\`
COMOUT=$bdir/rtofs.\$PDY

echo PDY \$PDY
echo COMOUT \$COMOUT

hsi mkd $hdir/rtofs.\$PDY

cd \$COMOUT/ncoda

date
htar -cvf $hdir/rtofs.\$PDY/glbl.tar glbl_var/restart/*
date
htar -cvf $hdir/rtofs.\$PDY/nhem.tar nhem_var/restart/*
date
htar -cvf $hdir/rtofs.\$PDY/shem.tar shem_var/restart/*
date
htar -cvf $hdir/rtofs.\$PDY/ocnqc.tar ocnqc/*
date
htar -cvf $hdir/rtofs.\$PDY/logs.tar logs/*
date

eofn

cd $tmpdir
qsub ./ncoda_archive_therest.$nday.$$

}

#end of functions
#############################################################################################################

#input - if no command line input then look back starting with yesterday
pdy=`$NDATE -24 | cut -c1-8`
checkall=N
if [ $# -eq 1 ]
then
  pdy=$1
  checkall=N
fi

#wcoss1
#bdir=`compath.py rtofs/$envir`

#wcoss2
bdir=$(compath.py $envir/com/rtofs/$version)

#rtofs 
if [[ $rtofs == 'Y' ]]
then
  if [[ $checkall == 'Y' ]]
  then
    for rdir in `ls $bdir`
    do
      sys=`echo $rdir | cut -d. -f1`
      dat=`echo $rdir | cut -d. -f2`
      if [[ $sys == rtofs ]]
      then
        if [[ $dat -le $pdy ]]
        then
          if [ -s $bdir/$rdir/prtofs_global_forecast_grib2_post_d04.${dat}12.dbnlog ]
          then
            echo check if rtofs for $rdir should be archived...
            hstattot=0
            for idx in rtofs.ab.tar.idx rtofs.ncgrb.tar.idx rtofs.ncoda.tar.idx rtofs.restart.tar.idx forcing.tar.idx
            do
              hfile=`hsi -P ls $hdir/rtofs.$dat/$idx`
              hstat=$? 
              if [ $hstat -gt 0 ]
              then
                hstattot=$hstat
                break
              fi
            done
            if [ $hstattot -ne 0 ]
            then
              build_archive_rtofs_ab $dat
              build_archive_rtofs $dat
            fi
          fi
        fi
      fi
    done
  else
    build_archive_rtofs_ab $pdy
    build_archive_rtofs $pdy
  fi
fi

#ncoda
if [[ $ncoda == 'Y' ]]
then
  if [[ $checkall == 'Y' ]]
  then
    for r3dir in `ls -d $bdir/*/ncoda`
    do
      r2dir=`dirname $r3dir`
      rdir=`basename $r2dir`
      sys=`echo $rdir | cut -d. -f1`
      dat=`echo $rdir | cut -d. -f2`
      if [[ $sys == rtofs ]]
      then
        if [[ $dat -le $pdy ]]
        then
          echo check file $bdir/$rdir/ncoda/logs/hycom_var/hycom_var.*.out
          if [ -s $bdir/$rdir/ncoda/logs/hycom_var/hycom_var.*.out ]
          then
            echo check if ncoda for $rdir should be archived...
            hstattot=0
            for idx in glbl.tar.idx hycom.tar.idx logs.tar.idx nhem.tar.idx ocnqc.tar.idx shem.tar.idx
            do
              hfile=`hsi.ksh -P ls $hdir/rtofs.$dat/$idx`
              hstat=$?
              if [ $hstat -gt 0 ]
              then
                hstattot=$hstat
                break
              fi
            done
            if [ $hstattot -ne 0 ]
            then
              build_archive_ncoda_hycom_var $dat
              build_archive_ncoda_therest $dat
            fi
          fi
        fi
      fi
    done
  else
    build_archive_ncoda_hycom_var $pdy
    build_archive_ncoda_therest $pdy
  fi
fi
