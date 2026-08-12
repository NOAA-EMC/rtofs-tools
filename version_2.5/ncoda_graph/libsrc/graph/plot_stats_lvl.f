      subroutine plot_stats_lvl (out_dir, dtg, dtg1, nest, n_lon,
     *                           n_lat, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_stats_lvl
c
c DESCRIPTION:  driver for display of NCODA 3DVAR verification
c               statistic time series by analysis level
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
      integer    N_LVL
      parameter (N_LVL = 32)
c
      integer    N_TAUS
      parameter (N_TAUS = 360)
c
      integer    N_STATS
      parameter (N_STATS = 5)
c     parameter (N_STATS = 10)
c
      integer    N_TYPS
      parameter (N_TYPS = 300)
c
      integer    N_VARS
      parameter (N_VARS = 10)
c     parameter (N_VARS = 2)
c
c     ..number observing systems to plot
c
      integer    N_SYS
      parameter (N_SYS = 8)
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
      integer   i, j, k, m, n
      integer   it, kt
      integer   ks, ke
      integer   jday
      integer   len
      character lvl_typ * 3
      integer   nest
      integer   n_lat, n_lon
      character out_dir * (*)
      integer   status
      integer   strt
      integer   sys (N_SYS, N_VARS)
      integer   tau
      character title * 21
      character var_lbl (N_VARS) * 20
      character var_units (N_VARS) * 5
      real      z_lvl (N_LVL+1)
c
c     ..allocatable arrays
c
      character,allocatable :: anl_dtg (:) * 10
      real,     allocatable :: anl_time (:)
      real,     allocatable :: bias_anl (:,:)
      real,     allocatable :: bias_fcst (:,:)
      real,     allocatable :: cnt (:,:)
      integer,  allocatable :: ndx (:)
      real,     allocatable :: rms_anl (:,:)
      real,     allocatable :: rms_fcst (:,:)
      real,     allocatable :: stats_lvl (:,:,:,:,:)
      real,     allocatable :: stats_time (:)
c
c     ..define variable labels and units
c
      data var_lbl / 'Temperature         ',
     *               'Salinity            ',
     *               'Geopotential        ',
     *               'U Velocity          ',
     *               'V Velocity          ',
     *               'Ice Coverage        ',
     *               'Ice Temperature     ',
     *               'Ice Thickness       ',
     *               '(blank)             ',
     *               '(blank)             ' /
c
      data var_units / '(C)  ',
     *                 '(PSU)',
     *                 '(m)  ',
     *                 '(m/s)',
     *                 '(m/s)',
     *                 '(%)  ',
     *                 '(C)  ',
     *                 '(m)  ',
     *                 'null ',
     *                 'null ' /
c
c     ..observing systems
c
      data      sys   / 0,   1,   4,  19,  36, 102, 133, 188,
     *                  0,  37,  49,  52, 103, 134, 189,  -1,
     *                  0,  19,  36,  -1,  -1,  -1,  -1,  -1,
     *                  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *                  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *                  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *                  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *                  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *                  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *                  0,  -1,  -1,  -1,  -1,  -1,  -1,  -1 /
c
c     ..define standard levels
c
      data z_lvl /  0.,    5.,   10.,   15.,   20.,   30.,   40.,
     *             50.,   75.,  100.,  125.,  150.,  175.,  200.,
     *            250.,  300.,  350.,  400.,  500.,  600.,  700.,
     *            800.,  900., 1000., 1100., 1200., 1300., 1400., 
     *           1500., 1750., 2000., 2250., 2500. /
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (anl_dtg (N_TAUS))
      allocate (anl_time (N_TAUS))
      allocate (bias_anl (N_TAUS, N_LVL))
      allocate (bias_fcst (N_TAUS, N_LVL))
      allocate (cnt (N_TAUS, N_LVL))
      allocate (ndx (N_TAUS))
      allocate (rms_anl (N_TAUS, N_LVL))
      allocate (rms_fcst (N_TAUS, N_LVL))
      allocate (stats_lvl (0:N_TYPS, N_LVL, N_STATS, N_VARS, N_TAUS))
      allocate (stats_time (N_TAUS))
c
c     ..set field parameters and variable names
c
      fld_name = 'ocnobs'
      file_typ = 'statdat'
      fluid = 'o'
      lvl_typ = 'sfc'
      tau = 0
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
c     ..build verification file name, check for existence
c
      call cr_fname (out_dir, dtg, nest, n_lon, n_lat, file_typ,
     *               fld_name, fluid, lvl_typ, tau, file_name,
     *               len)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) return
c
c     ..open file, read stats array
c
      open (UNIT, file=file_name(1:len), status='old',
     *            form='unformatted')
      read (UNIT) stats_time
      read (UNIT) 
      read (UNIT) stats_lvl
      close (UNIT)
c
c     ..create indexed time series
c
      ndx = 0
      do i = 1, N_TAUS
         it = int (anl_time(i) * 10.)
         do k = 1, N_TAUS
            kt = int (stats_time(k) * 10.)
            if (kt .eq. it) ndx(i) = k
         enddo
      enddo
c
c     ..loop over analysis variables and observing systems
c
c     do n = 1, N_VARS
      do n = 1, 2
      do k = 1, N_SYS
c
         if (sys(k,n) .lt. 0) cycle
c
c        ..initialize plot vectors
c
         do j = 1, N_LVL
            do i = 1, N_TAUS
               bias_anl(i,j) = ZDUM
               bias_fcst(i,j) = ZDUM
               cnt(i,j) = 0.
               rms_anl(i,j) = ZDUM
               rms_fcst(i,j) = ZDUM
            enddo
         enddo
c
c        ..save stats in plot arrays
c
         do j = 1, N_LVL
         do i = 1, N_TAUS
            if (ndx(i) .gt. 0) then
               if (stats_lvl(sys(k,n),j,5,n,ndx(i)) .gt. 0.) then
                  bias_fcst(i,j) = stats_lvl(sys(k,n),j,1,n,ndx(i))
                  bias_anl(i,j)  = stats_lvl(sys(k,n),j,2,n,ndx(i))
                  rms_fcst(i,j)  = stats_lvl(sys(k,n),j,3,n,ndx(i))
                  rms_anl(i,j)   = stats_lvl(sys(k,n),j,4,n,ndx(i))
                  cnt(i,j)       = stats_lvl(sys(k,n),j,5,n,ndx(i))
               endif
            endif
         enddo
         enddo
c
c        ..find starting and ending dtgs
c
         strt = 0
         do i = ke, ks, -1
            if (cnt(i,1).gt.0. .or. cnt(i,2).gt.0.) strt = i
         enddo
c
c        ..plot analysis verification time series
c
         if (strt .gt. 0) then
            if (sys(k,n) .eq. 0) then
               title = var_lbl(n)   
            else
               if (n .eq. 3) then
                  if (sys(k,n) .eq. 19) then
                     title = 'Direct Geopotential  '
                  else if (sys(k,n) .eq. 36) then
                     title = 'Argo Geopotential    '
                  else
                     title = data_lbl(sys(k,n))
                  endif
               else if (n .eq. 6) then
                  if (sys(k,n) .eq. 19) then
                     title = 'Direct Layer Pressure'
                  else if (sys(k,n) .eq. 36) then
                     title = 'Argo Layer Pressure  '
                  else
                     title = data_lbl(sys(k,n))
                  endif
               else
                  title = data_lbl(sys(k,n))
               endif
            endif
            call stats_ser_lvl ('Bias', N_TAUS, N_LVL, strt, ks, ke,
     *                          anl_dtg, bias_anl, bias_fcst, z_lvl,
     *                          title, n, var_units(n), fno)
            call stats_ser_lvl ('RMSE', N_TAUS, N_LVL, strt, ks, ke,
     *                          anl_dtg, rms_anl, rms_fcst, z_lvl,
     *                          title, n, var_units(n), fno)
c           call stats_ser_lvl ('Count', N_TAUS, N_LVL, strt, ks, ke,
c    *                          anl_dtg, cnt, cnt, z_lvl, title, n, 
c    *                          var_units(n), fno)
         endif
      enddo
      enddo
c
c     ..clean up
c
      deallocate (anl_dtg, anl_time, bias_anl, bias_fcst, cnt, ndx)
      deallocate (rms_anl, rms_fcst, stats_lvl, stats_time)
c
      return
      end
