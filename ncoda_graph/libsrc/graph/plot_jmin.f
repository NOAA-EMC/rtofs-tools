      subroutine plot_jmin (out_dir, dtg, dtg1, nest, n_lon, 
     *                      n_lat, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_jmin
c
c DESCRIPTION:  driver for display of NCODA 3DVAR jmin diagnostic
c               time series
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..number parameters
c
      integer    N_PARMS
      parameter (N_PARMS = 4)
c
c     ..jmin stats array dimensions
c
      integer    N_TAUS
      parameter (N_TAUS = 360)
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
      integer   jday
      integer   ke, ks
      integer   len
      character lvl_typ * 3
      integer   nest
      integer   n_lat, n_lon
      integer   n_var (N_PARMS)
      character out_dir * (*)
      character parm (N_PARMS) * 3
      integer   status
      integer   strt
      integer   tau
      integer   var
c*****************
      real x
c**********************
c
c     ..allocatable arrays
c
      character,allocatable :: anl_dtg (:) * 10
      real,     allocatable :: anl_time (:)
      real,     allocatable :: cnt (:,:)
      real,     allocatable :: diagn (:,:)
      real,     allocatable :: jmin (:,:,:)
      real,     allocatable :: jmin_cnt (:,:,:)
      real,     allocatable :: jmin_time (:)
      integer,  allocatable :: ndx (:)
      character,allocatable :: var_lbl (:) * 23
c
      data      parm / 'ICE', 'SST', 'SSH', 'MVO' /
      data      n_var /  1,     1,     1,     6 /
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (anl_dtg (N_TAUS))
      allocate (anl_time (N_TAUS))
      allocate (cnt (N_TAUS, 0:N_TYPS))
      allocate (diagn (N_TAUS, 0:N_TYPS))
      allocate (jmin (0:N_TYPS, N_VARS, N_TAUS))
      allocate (jmin_cnt (0:N_TYPS, N_VARS, N_TAUS))
      allocate (jmin_time (N_TAUS))
      allocate (ndx (N_TAUS))
      allocate (var_lbl (N_VARS))
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
      do n = 1, N_PARMS
c
c     ..set field parameters and variable names
c
      if (parm(n) .eq. 'ICE') then
         fld_name = 'icecov'
         var_lbl(1) = 'Sea Ice Coverage       '
      else if (parm(n) .eq. 'SST') then
         fld_name = 'seatmp'
         var_lbl(1) = 'Sea Surface Temperature'
      else if (parm(n) .eq. 'SSH') then
         fld_name = 'seahgt'
         var_lbl(1) = 'Sea Surface Height     '
      else if (parm(n) .eq. 'MVO') then
         fld_name = 'ocnobs'
         var_lbl(1) = 'Temperature            '
         var_lbl(2) = 'Salinity               '
         var_lbl(3) = 'Geopotential           '
         var_lbl(4) = 'U Velocity             '
         var_lbl(5) = 'V Velocity             '
         var_lbl(6) = 'Layer Pressure         '
      endif 
c
c     ..build verification file name, check for existence
c
      file_typ = 'jmindat'
      fluid = 'o'
      lvl_typ = 'sfc'
      tau = 0
      call cr_fname (out_dir, dtg, nest, n_lon, n_lat, file_typ,
     *               fld_name, fluid, lvl_typ, tau, file_name, 
     *               len)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) cycle
c
c     ..open file, read stats array
c
      open (UNIT, file=file_name(1:len), status='old',
     *            form='unformatted')
      read (UNIT) jmin_time
      read (UNIT) jmin
      read (UNIT) jmin_cnt
      close (UNIT)
c
c     ..create indexed time series
c
      ndx = 0
      do i = ks, ke
         it = int (anl_time(i) * 10.)
         do k = 1, N_TAUS
            kt = int (jmin_time(k) * 10.)
            if (kt .eq. it) ndx(i) = k
         enddo
      enddo
c
c     ..loop over analysis variables
c
      do var = 1, n_var(n)
c
c        ..initialize plot vectors
c
         do j = 1, N_TAUS
            do i = 0, N_TYPS
               cnt(j,i) = 0.
               diagn(j,i) = 0.
            enddo
         enddo
c
c        ..save jmin in plot arrays
c
         do j = ks, ke
            if (ndx(j) .gt. 0) then
               do i = 0, N_TYPS
                  if (jmin_cnt(i,var,ndx(j)) .gt. 0.) then
                     diagn(j,i) = jmin(i,var,ndx(j))
c**************
c          if (i .eq. 0) then
c          diagn(j,i) = diagn(j,i) + 0.2
c          else
c          diagn(j,i) = diagn(j,i) + 0.1
c          if (diagn(j,i) .gt. 2.) then
c          x = diagn(j,i) - 2.
c          diagn(j,i) = 2.
c          x = diagn(j,i) + x * 0.2
c          diagn(j,i) = x
c          endif
c          endif
c****************
                     cnt(j,i) = jmin_cnt(i,var,ndx(j))
                  endif
               enddo
            endif
         enddo
c**********************************
c        do j = ks, ke
c        if (ndx(j) .gt. 0) then
c        do i = 0, N_TYPS
c        if (i .eq. 0) then
c        if (anl_dtg(j) .eq. '2022041900') then
c           diagn(j,i) = 0.5 * (diagn(j-1,i) + diagn(j+1,i))
c        endif
c        if (anl_dtg(j) .eq. '2022071500') then
c           diagn(j,i) = 0.5 * (diagn(j-1,i) + diagn(j+1,i))
c        endif
c        endif
c        enddo
c        endif
c        enddo
c*********************************
c
c        ..check start time
c
         strt = 0
         do j = ke, ks, -1
            if (cnt(j,0) .gt. 0.) strt = j
         enddo
c
c        ..plot jmin diagnostic time series
c
         if (strt .gt. 0) then
            call jmin_ser (N_TAUS, N_TYPS, strt, ke, anl_dtg, anl_time,
     *                     diagn, cnt, var_lbl(var), fno)
         endif
      enddo
      enddo
c
c     ..clean up
c
      deallocate (anl_dtg, anl_time, cnt, diagn, jmin, jmin_cnt)
      deallocate (jmin_time, ndx, var_lbl)
c
      return
      end
