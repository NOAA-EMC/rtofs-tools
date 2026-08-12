      subroutine plot_err (out_dir, dtg, nest, n_lon, n_lat, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_err
c
c DESCRIPTION:  driver for display of NCODA 3DVAR forecast error
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
      integer    N_PARMS
      parameter (N_PARMS = 2)
c
      integer    N_TAUS
      parameter (N_TAUS = 360)
c
      integer    N_STATS
      parameter (N_STATS = 3)
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
c     ..local array dimension
c
      integer   n_fcst
      integer   n_var
c
      character dtg * 10
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j, k, m
      integer   np, nv
      integer   len
      character lvl_typ * 3
      integer   nest
      integer   n_lat, n_lon
      character out_dir * (*)
      character parm (N_PARMS) * 3
      integer   strt
      integer   tau
      character time * 10
      integer   upd
      character var_lbl (N_PARMS) * 16
      real      x
c
c     ..allocatable arrays
c
      character,allocatable :: anl_dtg (:) * 10
      real,     allocatable :: anl_time (:)
      real,     allocatable :: bias (:,:)
      real,     allocatable :: cnt (:,:)
      real,     allocatable :: rms (:,:)
      real,     allocatable :: stats (:,:,:,:)
      real,     allocatable :: stats_time (:)
c
      data      parm / 'ICE', 'MVO' /
c
c...............................executable..............................
c
c     ..initialize
c
      fluid = 'o'
      lvl_typ = 'sfc'
c
c     ..loop over parameters
c
      do np = 1, N_PARMS
c
c     ..set field parameters and variable names
c
      if (parm(np) .eq. 'ICE') then
         fld_name = 'icecov'
         var_lbl(1) = 'Sea Ice Coverage'
      else if (parm(np) .eq. 'MVO') then
         fld_name = 'ocnobs'
         var_lbl(1) = 'Temperature     '
         var_lbl(2) = 'Salinity        '
      endif 
c
c     ..build verification file name, check for existence
c
      file_typ = 'staterr'
      tau = 0
      call cr_fname (out_dir, dtg, nest, n_lon, n_lat, file_typ,
     *               fld_name, fluid, lvl_typ, tau, file_name,
     *               len)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) cycle
c
c     ..open file, read header
c
      open (UNIT, file=file_name(1:len), status='old',
     *            form='unformatted')
      read (UNIT) n_fcst, n_var, upd
c
c     ..allocate arrays
c
      allocate (anl_dtg (N_TAUS))
      allocate (anl_time (N_TAUS))
      allocate (bias (N_TAUS, n_fcst))
      allocate (cnt (N_TAUS, n_fcst))
      allocate (rms (N_TAUS, n_fcst))
      allocate (stats (N_STATS, n_fcst, n_var, N_TAUS))
      allocate (stats_time (N_TAUS))
c
c     ..read stats vectors
c
      read (UNIT) stats_time
      read (UNIT) stats
      close (UNIT)
c
c     ..loop over variables
c
      do nv = 1, n_var
c
c        ..initialize plot vectors
c
         do j = 1, n_fcst
            do i = 1, N_TAUS
               anl_dtg(i) = '          '
               anl_time(i) = ZDUM
               bias(i,j) = ZDUM
               cnt(i,j) = 0.
               rms(i,j) = ZDUM
            enddo
         enddo
c
c        ..save stats in plot arrays
c
         do i = 1, N_TAUS
            call time_dtg (stats_time(i), anl_dtg(i))
            anl_time(i) = stats_time(i)
            do j = 1, n_fcst
               bias(i,j) = stats(1,j,k,i)
               rms(i,j) = stats(2,j,k,i)
               cnt(i,j) = stats(3,j,k,i)
            enddo
         enddo
c
c        ..reorder time variable sequence (old to young)
c
         m = N_TAUS / 2
         do j = 1, m
            i = N_TAUS - j + 1
c
            x = anl_time(j)
            anl_time(j) = anl_time(i)
            anl_time(i) = x
c
            time = anl_dtg(j)
            anl_dtg(j) = anl_dtg(i)
            anl_dtg(i) = time
         enddo
c
c        ..reorder stat variable sequence (old to young)
c
         do k = 1, n_fcst
            do j = 1, m
               i = N_TAUS - j + 1
c
               x = bias(j,k)
               bias(j,k) = bias(i,k)
               bias(i,k) = x
c
               x = rms(j,k)
               rms(j,k) = rms(i,k)
               rms(i,k) = x
c 
               x = cnt(j,k)
               cnt(j,k) = cnt(i,k)
               cnt(i,k) = x
            enddo
         enddo
c
c        ..find starting and ending dtgs
c
         strt = 0
         do i = N_TAUS, 1, -1
            if (cnt(i,1) .gt. 0.) strt = i
         enddo
c
c        ..plot analysis verification time series
c
         if (maxval (cnt) .gt. 0.) then
            call err_ser (parm(np), n_fcst, N_TAUS, strt, anl_dtg,
     *                    anl_time, upd, bias, rms, cnt, nv,
     *                    var_lbl(nv), fno)
         endif
      enddo
c
c     ..clean up
c
      deallocate (anl_dtg, anl_time, bias, cnt, rms, stats)
      deallocate (stats_time)
c
      enddo
c
      return
      end
