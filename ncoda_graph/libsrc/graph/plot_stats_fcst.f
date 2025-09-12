      subroutine plot_stats_fcst (out_dir, dtg, dtg1, nest, n_lon,
     *                            n_lat, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_stats_fcst
c
c DESCRIPTION:  driver for display of NCODA 3DVAR verification
c               statistic time series for forecast errors 
c               beyond the update cycle interval
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..set max number forecast periods
c
      integer    N_FCST
      parameter (N_FCST = 8)
c     parameter (N_FCST = 7)
c
c     ..vertification stats time array dimension
c
      integer    N_TAUS
      parameter (N_TAUS = 360)
c
c     ..work file unit number
c
      integer    UNIT
      parameter (UNIT = 20)
c
c     ..missing (special) value
c
      real       ZDUM
      parameter (ZDUM = -999.)
c
      character dtg * 10
      character dtg1 * 10
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, k, m, n
      integer   it, kt
      integer   jday
      integer   ke, ks
      integer   len, len_dir
      character lvl_lbl (2) * 7
      character lvl_typ * 3
      integer   nest
      integer   nf, nt
      integer   n_lat, n_lon
      integer   n_var
      character out_dir * (*)
      integer   src
      integer   status
      integer   strt
      integer   tau
      character var_lbl (2) * 12
c
c     ..allocatable arrays
c
      character,allocatable :: anl_dtg (:) * 10
      real,     allocatable :: anl_time (:)
      real,     allocatable :: bias_fcst (:,:)
      real,     allocatable :: bias_fcst_lvl (:,:)
      real,     allocatable :: cnt (:,:)
      real,     allocatable :: cnt_lvl (:,:)
      integer,  allocatable :: ndx (:)
      real,     allocatable :: rms_fcst (:,:)
      real,     allocatable :: rms_fcst_lvl (:,:)
      real,     allocatable :: stats (:,:,:,:)
      real,     allocatable :: stats_lvl (:,:,:,:)
      real,     allocatable :: stats_time (:)
c
c...............................executable..............................
c
c     ..loop over source files (T&S, ice)
c
      do src = 1, 2
c
c     ..check for forecast stats file
c
      if (src .eq. 1) then
         fld_name = 'ocnobs'
         var_lbl(1) = 'Temperature '
         var_lbl(2) = 'Salinity    '
         lvl_lbl(1) = 'Surface'
         lvl_lbl(2) = '       '
      else if (src .eq. 2) then
         fld_name = 'icecov'
         var_lbl(1) = 'Ice Coverage'
         var_lbl(2) = '            '
         lvl_lbl(1) = '       '
         lvl_lbl(2) = '       '
      endif
      file_typ = 'staterr'
      fluid = 'o'
      lvl_typ = 'sfc'
      tau = 0
      call cr_fname (out_dir, dtg, nest, n_lon, n_lat, file_typ,
     *               fld_name, fluid, lvl_typ, tau, file_name,
     *               len)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) then
         len_dir = len_trim (out_dir) + 1
         write (*, '(''*** WARNING (plot_stats_fcst): missing '',
     *               ''forecast stats file "'', a ''"'')')
     *          file_name(len_dir:len)
         return
      endif
c
c     ..read header record
c
      open (UNIT, file=file_name(1:len), status='old',
     *            access='sequential', form='unformatted')
      read (UNIT) nf, n_var, nt
      if (nf .ne. N_FCST) then
         write (*, '(''*** WARNING (plot_stats_fcst): number '',
     *               ''forecasts differ "'', 2i10, ''"'')')
     *               nf, N_FCST
          return
      endif
      if (nt .ne. N_TAUS) then
         write (*, '(''*** WARNING (plot_stats_fcst): number '',
     *               ''taus differ "'', 2i10 ''"'')')
     *               nt, N_TAUS
         return
      endif
c
c     ..allocate stats arrays
c
      allocate (stats (3, N_FCST, n_var, N_TAUS))
      allocate (stats_lvl (3, N_FCST, n_var, N_TAUS))
      allocate (stats_time (N_TAUS))
c
c     ..read stats
c
      read (UNIT) stats_time
      read (UNIT) stats
      read (UNIT) stats_lvl
      close (UNIT)
c
c     ..allocate plot variables
c
      allocate (anl_dtg (N_TAUS))
      allocate (anl_time (N_TAUS))
      allocate (bias_fcst (N_TAUS, N_FCST))
      allocate (bias_fcst_lvl (N_TAUS, N_FCST))
      allocate (cnt (N_TAUS, N_FCST))
      allocate (cnt_lvl (N_TAUS, N_FCST))
      allocate (ndx (N_TAUS))
      allocate (rms_fcst (N_TAUS, N_FCST))
      allocate (rms_fcst_lvl (N_TAUS, N_FCST))
c
c     ..create expected time series (old to young)
c
      ks = 1
      ke = N_TAUS
      do k = 1, N_TAUS
         m = N_TAUS - k + 1
         tau = (m-1) * 24
         call dtgmod (dtg, -tau, anl_dtg(k), status)
         call dtg_time (anl_dtg(k), anl_time(k), jday)
         if (dtg1 .eq. anl_dtg(k)) ks = k
      enddo
c
c     ..create indexed time series
c
      ndx = 0
      do i = ks, ke
         it = int (anl_time(i) * 10.)
         do k = 1, N_TAUS
            kt = int (stats_time(k) * 10.)
            if (kt .eq. it) ndx(i) = k
         enddo
      enddo
c
c     ..loop over variables
c
      do n = 1, n_var
c
c     ..initialize plot vectors
c
      do k = 1, N_FCST
         do i = 1, N_TAUS
            bias_fcst(i,k) = ZDUM
            bias_fcst_lvl(i,k) = ZDUM
            rms_fcst(i,k) = ZDUM
            rms_fcst_lvl(i,k) = ZDUM
            cnt(i,k) = 0.
            cnt_lvl(i,k) = 0.
         enddo
      enddo
c
c     ..save stats in plot arrays
c
      do i = ks, ke
         if (ndx(i) .gt. 0) then
            do k = 1, N_FCST 
               bias_fcst(i,k) = stats(1,k,n,ndx(i))
               rms_fcst(i,k)  = stats(2,k,n,ndx(i))
               cnt(i,k)       = stats(3,k,n,ndx(i))
               bias_fcst_lvl(i,k) = stats_lvl(1,k,n,ndx(i))
               rms_fcst_lvl(i,k)  = stats_lvl(2,k,n,ndx(i))
               cnt_lvl(i,k)       = stats_lvl(3,k,n,ndx(i))
c****************************
c     if (src .eq. 1 .and. n .eq. 2) then
c     write (91,'(2i5,2x,a,2x,2f10.3)')
c    * i,k,anl_dtg(i),rms_fcst(i,k),rms_fcst_lvl(i,k)
c     endif
c**************************
            enddo
         endif
      enddo
c********************************
      if (src .eq. 1) then
      do i = ks, ke
      if (ndx(i) .gt. 0) then
      if (anl_dtg(i) .eq. '2022041900') then
      do k = 1, N_FCST
      bias_fcst_lvl(i,k) = 0.5 * (bias_fcst_lvl(i-1,k) +
     *                            bias_fcst_lvl(i+1,k))
      rms_fcst_lvl(i,k) = 0.5 * (rms_fcst_lvl(i-1,k) +
     *                           rms_fcst_lvl(i+1,k))
      enddo
      endif
      endif
      enddo
      endif
c
      if (src .eq. 1 .and. n .eq. 2) then
      do i = ks, ke
      if (ndx(i) .gt. 0) then
      if (anl_dtg(i) .eq. '2022071500') then
      do k = 1, N_FCST
      rms_fcst_lvl(i,k) = 0.5 * (rms_fcst_lvl(i-1,k) +
     *                           rms_fcst_lvl(i+1,k))
      enddo
      endif
      endif
      enddo
      endif
c********************************
c
c     ..plot surface statistics
c
      strt = 0
      do i = ke, ks, -1
         if (cnt(i,1) .gt. 0.) strt = i
      enddo
      if (strt .eq. 0) cycle
c
      call stats_fcst (src, lvl_lbl(1), var_lbl(n), N_FCST,
     *                 N_TAUS, strt, ks, ke, anl_dtg, anl_time, 
     *                 bias_fcst, rms_fcst, cnt, n, fno)
c
      if (src .eq. 1) then
c
c        ..plot statistics at depth
c      
         strt = 0
         do i = ke, ks, -1
            if (cnt_lvl(i,1) .gt. 0.) strt = i
         enddo
         if (strt .eq. 0) cycle
c
c        ..plot verification time series
c
         call stats_fcst (src, lvl_lbl(2), var_lbl(n), N_FCST,
     *                    N_TAUS, strt, ks, ke, anl_dtg, anl_time,
     *                    bias_fcst_lvl, rms_fcst_lvl, cnt_lvl,
     *                    n, fno)
      endif
      enddo
c
c     ..clean up
c
      deallocate (anl_dtg, anl_time, bias_fcst, bias_fcst_lvl)
      deallocate (cnt, cnt_lvl, ndx, rms_fcst, rms_fcst_lvl)
      deallocate (stats, stats_lvl, stats_time)
      enddo
c
      return
      end
