# Contents:
```
-rwxr-xr-x 1 1474 Mar 30 22:06 create_all_rtofs.ic.sh
-rwxr-xr-x 1 2219 Mar 30 22:06 forhafs.sh
-rwxr-xr-x 1 3178 Mar 30 22:06 get_dcom_data_ncepobserv.sh
-rwxr-xr-x 1 3048 Mar 30 22:06 get_dcom_data.sh
-rwxr-xr-x 1 1079 Mar 30 22:06 getforcing.sh
-rwxr-xr-x 1 2878 Mar 30 22:06 get_gdas.sh
-rwxr-xr-x 1 3770 Mar 30 22:06 get_gfs.sh
-rwxr-xr-x 1 4300 Mar 30 22:06 pull_and_create_hycom_var.sh
-rwxr-xr-x 1 3375 Mar 30 22:06 pull_lotsa_stuff.sh
-rwxr-xr-x 1 3886 Mar 30 22:06 pull_rtofs_archive_and_restart.notgz.sh
-rwxr-xr-x 1 8009 Mar 30 22:06 pull_rtofs_archive_and_restart.sh
-rwxr-xr-x 1 1777 Mar 30 22:06 pull_rtofs_ncgrb.sh
```

## Create_all_rtofs.ic.sh
   - calls other scripts to pull data from HPSS to start RTOFS v2.5
   - calls are made to scripts

## Get_dcom_data.sh and get_dcom_data_ncepobserv.sh
   - these get the dcom data from HPSS.
   - two scripts for two separate hpss tarballs (get_dcom_data.sh preferred)

## get_gdas.sh and get_gfs.sh
   - these get slux data from HPSS.
   - will probably have to be modified when v17 goes operational.

## getforcing.sh
   - gets RTOFS forcing files from HPSS.
