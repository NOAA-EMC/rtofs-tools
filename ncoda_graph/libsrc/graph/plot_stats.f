      subroutine plot_stats (out_dir, dtg, dtg1, nest, n_lon,
     *                       n_lat, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_stats
c
c DESCRIPTION:  driver for display of NCODA 3DVAR verification
c               statistic time series
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..vertification stats array dimensions
c
      integer    N_TAUS
      parameter (N_TAUS = 360)
c
      integer    N_STATS
      parameter (N_STATS = 5)
c
      integer    N_TYPS
      parameter (N_TYPS = 300)
c
      integer    N_VARS
      parameter (N_VARS = 10)
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
      include 'coda_types.h'
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
      integer   ks, ke
      integer   jday
      integer   len, len_dir
      character lvl_typ * 3
      integer   nest
      integer   n_lat, n_lon
      character out_dir * (*)
      integer   status
      integer   strt
      integer   tau
      character title * 25
      integer   var
c
c     ..allocatable arrays
c
      character,allocatable :: anl_dtg (:) * 10
      real,     allocatable :: anl_time (:)
      real,     allocatable :: bias_anl (:)
      real,     allocatable :: bias_fcst (:)
      real,     allocatable :: cnt (:)
      integer,  allocatable :: ndx (:)
      real,     allocatable :: rms_anl (:)
      real,     allocatable :: rms_fcst (:)
      real,     allocatable :: stats (:,:,:,:)
      real,     allocatable :: stats_time (:)
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (anl_dtg (N_TAUS))
      allocate (anl_time (N_TAUS))
      allocate (bias_anl (N_TAUS))
      allocate (bias_fcst (N_TAUS))
      allocate (cnt (N_TAUS))
      allocate (ndx (N_TAUS))
      allocate (rms_anl (N_TAUS))
      allocate (rms_fcst (N_TAUS))
      allocate (stats (0:N_TYPS, N_STATS, N_VARS, N_TAUS))
      allocate (stats_time (N_TAUS))
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
c     ..loop over parameters
c
      do n = 1, 2
c
c     ..build verification file name, check for existence
c
      if (n .eq. 1) then
         fld_name = 'icecov'
      else if (n .eq. 2) then
         fld_name = 'ocnobs'
      endif 
      file_typ = 'statdat'
      fluid = 'o'
      lvl_typ = 'sfc'
      tau = 0
      call cr_fname (out_dir, dtg, nest, n_lon, n_lat, file_typ,
     *               fld_name, fluid, lvl_typ, tau, file_name,
     *               len)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) then
         write (*, '(''      restart missing: '', a)')
     *          file_name(len_dir:len)
         cycle
      endif
      len_dir = len_trim (out_dir) + 1
      write (*, '(''       restart found: '', a)')
     *       file_name(len_dir:len)
c
c     ..open file, read stats array
c
      open (UNIT, file=file_name(1:len), status='old',
     *            form='unformatted')
      read (UNIT) stats_time
      read (UNIT) stats
      close (UNIT)
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
c     ..loop over variables and observing systems
c
      do var = 1, 2    
      do k = 1, N_TYPS
c
c        ..skip extended and expanded data types
c
         if (k .eq.  50) cycle
         if (k .eq.  51) cycle
         if (k .eq. 141) cycle
         if (k .eq. 142) cycle
         if (k .eq. 143) cycle
         if (k .eq. 144) cycle
c
c        ..initialize plot vectors
c
         do i = 1, N_TAUS
            bias_anl(i) = ZDUM
            bias_fcst(i) = ZDUM
            cnt(i) = 0.
            rms_anl(i) = ZDUM
            rms_fcst(i) = ZDUM
         enddo
c
c        ..save stats in plot arrays
c
         do i = ks, ke
            if (ndx(i) .gt. 0) then
               if (stats(k,5,var,ndx(i)) .gt. 0.) then
                  bias_fcst(i) = stats(k,1,var,ndx(i))
                  bias_anl(i)  = stats(k,2,var,ndx(i))
                  rms_fcst(i)  = stats(k,3,var,ndx(i))
                  rms_anl(i)   = stats(k,4,var,ndx(i))
                  cnt(i)       = stats(k,5,var,ndx(i))
               endif
            endif
         enddo
c
c        ..find starting dtg (ending dtg is argument dtg)
c
         strt = 0
         do i = ke, ks, -1
            if (cnt(i) .gt. 0.) strt = i
         enddo
c
c        ..plot analysis verification time series
c
         if (strt .gt. 0) then
            title = adjustl (data_lbl(k))
            call stats_ser (n, var, N_TAUS, strt, ks, ke, anl_dtg,
     *                      anl_time, bias_anl, bias_fcst, rms_anl,
     *                      rms_fcst, cnt, k, title, fno)
         endif
      enddo
      enddo
      enddo
c
c     ..clean up
c
      deallocate (anl_dtg, anl_time, bias_anl, bias_fcst, cnt)
      deallocate (ndx, rms_anl, rms_fcst, stats, stats_time)
c
      return
      end
