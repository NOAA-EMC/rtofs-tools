      subroutine argo_mdb_diagn (dtg1, dtg2, data_dir, clim_dir)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  arg_mdb_diagn
c
c DESCRIPTION:  performs diagnostics on argo sss match up data base
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libphys.a
c
c PARAMETERS:
c       Name           Type     Usage             Description
c   -------------    --------   ------   ------------------------------
c
c....................MAINTENANCE SECTION................................
c
c MODULES CALLED:
c        Name                         Description
c   --------------     ---------------------------------------------
c
c METHOD:
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
      integer    N_TYP
      parameter (N_TYP = 2)
c
      integer    UNIT
      parameter (UNIT = 21)
c
      real      anm
      character clim_dir * (*)
      character data_dir * (*)
      character date * 26
      integer   day1, day2
      character dtg1 * 10
      character dtg2 * 10
      real      e
      logical   exist
      character file_dtg * 10
      character file_name * 256
      integer   hr
      integer   i, k, n
      integer   len
      integer   mon1, mon2
      character month (12) * 3
      integer   n_data
      integer   n_files
      integer   n_obs
      character sat_lbl (N_TYP) * 4
      integer   sat_typ (N_TYP)
      real      spval
      real      sss_std
      integer   status
      integer   year1, year2
c
c     ..allocatable match up data arrays
c
      real,     allocatable :: arg_clm (:)
      real,     allocatable :: arg_lat (:)
      real,     allocatable :: arg_lon (:)
      real,     allocatable :: arg_lvl (:)
      real,     allocatable :: arg_sss (:)
      real,     allocatable :: arg_sst (:)
      real,     allocatable :: clm_std (:)
      real,     allocatable :: scl_sss (:)
      real,     allocatable :: sss_dst (:)
      real,     allocatable :: sss_err( :)
      real,     allocatable :: sss_lat (:)
      real,     allocatable :: sss_lon (:)
      integer,  allocatable :: sss_msk (:)
      real,     allocatable :: sss_sss (:)
      real,     allocatable :: sss_sst (:)
      real,     allocatable :: sss_tim (:)
      integer,  allocatable :: sss_typ (:)
c
      data      sat_typ / 174, 126 /
      data      sat_lbl / 'SMOS', 'SMAP' /
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
c     ..form date time group labels
c
      read (dtg1(1:4), '(i4)') year1
      read (dtg1(5:6), '(i2)') mon1
      read (dtg1(7:8), '(i2)') day1
      read (dtg2(1:4), '(i4)') year2
      read (dtg2(5:6), '(i2)') mon2
      read (dtg2(7:8), '(i2)') day2
      write (date, '(i2.2, 1x, a, 1x, i4 '' to '',
     *               i2.2, 1x, a, 1x, i4)')
     *       day1, month(mon1), year1, day2, month(mon2), year2
c
c     ..compute number files
c
      call dtgdif (dtg1, dtg2, hr, status)
      n_files = hr / 24 + 1     
c
c     ..set number obs
c
      n_obs = 10 000 * n_files
c
c     ..allocate mdb arrays
c
      allocate (arg_clm (n_obs))
      allocate (arg_lat (n_obs))
      allocate (arg_lon (n_obs))
      allocate (arg_lvl (n_obs))
      allocate (arg_sss (n_obs))
      allocate (arg_sst (n_obs))
      allocate (clm_std (n_obs))
      allocate (scl_sss (n_obs))
      allocate (sss_dst (n_obs))
      allocate (sss_err (n_obs))
      allocate (sss_lat (n_obs))
      allocate (sss_lon (n_obs))
      allocate (sss_msk (n_obs))
      allocate (sss_sss (n_obs))
      allocate (sss_sst (n_obs))
      allocate (sss_tim (n_obs))
      allocate (sss_typ (n_obs))
c
c     ..read match up data base
c
      n_data = 0
      do k = 1, n_files
         hr = (k-1) * 24
         call dtgmod (dtg2, -hr, file_dtg, status)      
         file_name = trim (data_dir) // '/sss/' // file_dtg // '.mdb'
         len = len_trim (file_name)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            open (UNIT, file=file_name(1:len), form='unformatted',
     *                  status='old')
            read (UNIT) n
            read (UNIT) arg_clm(n_data+1:n_data+n)
            read (UNIT) arg_lat(n_data+1:n_data+n)
            read (UNIT) arg_lon(n_data+1:n_data+n)
            read (UNIT) arg_lvl(n_data+1:n_data+n)
            read (UNIT) arg_sss(n_data+1:n_data+n)
            read (UNIT) arg_sst(n_data+1:n_data+n)
            read (UNIT) sss_dst(n_data+1:n_data+n)
            read (UNIT) sss_err(n_data+1:n_data+n)
            read (UNIT) sss_lat(n_data+1:n_data+n)
            read (UNIT) sss_lon(n_data+1:n_data+n)
            read (UNIT) sss_sss(n_data+1:n_data+n)
            read (UNIT) sss_sst(n_data+1:n_data+n)
            read (UNIT) sss_tim(n_data+1:n_data+n)
            read (UNIT) sss_typ(n_data+1:n_data+n)
            n_data = n_data + n
            write (*, '(11x, ''read file: '', a)') trim (file_name)
         endif
      enddo
      write (*, '(10x, ''number obs: '', i10)') n_data
c
c     ..retrieve climate variability, scale argo-sss differences
c
      spval = -999.
      sss_std = 2.
      sss_msk = 1
      file_dtg = dtg2(1:6) // '1500'
      call rd_hycom_clim ('SSS', 'std', clim_dir, file_dtg, n_obs,
     *                    n_data, sss_lat, sss_lon, sss_msk, 
     *                    clm_std, .false., spval)
      do i = 1, n_data
         anm = sss_sss(i) - arg_sss(i)
         e = anm / clm_std(i)
         if (abs (e) .gt. sss_std) then
            if (e .gt. 0.) then
               anm = clm_std(i) * (sss_std + tanh (e - sss_std))
            else
               anm = clm_std(i) * (tanh (sss_std + e) - sss_std)
            endif
            scl_sss(i) = anm + arg_sss(i)
         else
            scl_sss(i) = sss_sss(i)
         endif
      enddo
c
c     ..loop over satellite data types, plot bias models
c
      do k = 1, N_TYP
c        call plt_sss_bias (date, n_obs, n_data, arg_sss, sss_sss, 
c    *                      sss_typ, sat_typ(k), sat_lbl(k))
         call plt_sss_sctr (date, n_obs, n_data, arg_clm, arg_sss,
     *                      scl_sss, sss_sss, sss_typ, sat_typ(k),
     *                      sat_lbl(k))
      enddo
c
c     ..clean up
c
      deallocate (arg_clm, arg_lat, arg_lon, arg_lvl, arg_sss, arg_sst)
      deallocate (clm_std, scl_sss)
      deallocate (sss_dst, sss_err, sss_lat, sss_lon, sss_msk, sss_sss)
      deallocate (sss_sst, sss_tim, sss_typ)
c
      return
      end
