      subroutine impact_hist_lat (n_obs, n_files, n_obs_file, obs_imp,
     *                            obs_lat, obs_sen, obs_typ, obs_var,
     *                            title, tmp, sal, typ_lbl, area, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  impact_hist_lat
c
c DESCRIPTION:  computes and plots histograms of data impacts
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libocnqc.a
c      
c PARAMETERS:
c     Name         Type       Usage              Description
c   ---------    --------    -------    --------------------------------
c   clim         real        input      climate at obs location
c   cut_dtg      character   input      data cut dtg
c   data         real        input      observed data (sst or ice)
c   data_typ     character   input      qc data type
c   fno          integer     input      ncar frame counter
c   glbl         real        input      global analysis at obs location
c   n_obs        integer     input      number of observations
c   regn         real        input      region analysis at obs location
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
      integer    MESH
      parameter (MESH = 5)
c
      real       LAT1
      parameter (LAT1 = -60.)
c
      integer    N_LAT
      parameter (N_LAT = 110 / MESH + 1)
c
c     ..local array dimensions
c
      integer   n_files
      integer   n_obs
c
      character area * (*)
      real      fmn, fmx
      integer   fno
      real      freq (N_LAT)
      real      g, h
      integer   i, j, k, n
      character lbl * 18
      character lbl3 * 3
      character lbl4 * 4
      character lbl5 * 5
      character lbl6 * 6
      character lbl7 * 7
      character lbl8 * 8
      integer   n_obs_file (n_files)
      real      obs_imp (n_obs, n_files)
      real      obs_lat (n_obs, n_files)
      real      obs_sen (n_obs, n_files)
      integer   obs_typ (n_obs, n_files)
      integer   obs_var (n_obs, n_files)
      real      off
      character plot_title * 256
      integer   sal
      real      siz
      character title * (*)
      integer   tmp
      integer   typ
      character typ_lbl * 7
      integer   var
      real      xi, yj
      real      xmx
      real      ymn, ymx
c
c     ..allocatable diagnostic arrays
c
      real,     allocatable :: dp (:,:)
      real,     allocatable :: ds (:,:)
      integer,  allocatable :: np (:,:)
      real,     allocatable :: xp (:,:)
      real,     allocatable :: xs (:,:)
c
c     ..set data types
c
      include 'coda_types.h'
c
c     ..set color tables
c
      include 'color_table.h'
c
c...............................executable..............................
c
c     ..allocate work arrays
c
      allocate (dp (N_LAT, 0:MX_TYPES))
      allocate (ds (N_LAT, 0:MX_TYPES))
      allocate (np (N_LAT, 0:MX_TYPES))
      allocate (xp (N_LAT, 0:MX_TYPES))
      allocate (xs (N_LAT, 0:MX_TYPES))
c
c     ..sum sensitivities and impacts by data type and latitude
c
      do k = 0, MX_TYPES
         do i = 1, N_LAT
            dp(i,k) = 0.
            ds(i,k) = 0.
            np(i,k) = 0
            xp(i,k) = 0.
            xs(i,k) = 0.
         enddo
      enddo
c
      do k = 1, n_files
      if (n_obs_file(k) .gt. 0) then
         do i = 1, n_obs_file(k)
            if (obs_var(i,k) .ne. 3) then
c********************
      if (mod(i,100) .eq. 0) then
      write (88,'(2i10,2f10.2)') k,i,obs_lat(i,k),obs_imp(i,k)
      endif
c**************************
               n = nint ((obs_lat(i,k) - LAT1) / real (MESH)) + 1
               if (n .lt. 1) n = 1
               if (n .gt. N_LAT) n = N_LAT
               xp(n,obs_typ(i,k)) = xp(n,obs_typ(i,k)) + obs_imp(i,k)
               xs(n,obs_typ(i,k)) = xs(n,obs_typ(i,k)) + obs_sen(i,k)
               np(n,obs_typ(i,k)) = np(n,obs_typ(i,k)) + 1
            endif
         enddo
      endif
      enddo
c
      do k = 0, MX_TYPES
         do i = 1, N_LAT
            if (np(i,k) .gt. 0) then
               dp(i,k) = xp(i,k) / real (np(i,k))
               ds(i,k) = xs(i,k) / real (np(i,k))
            endif
         enddo
      enddo
c
c------------------------------------------------------------------
c
c     ..set range of latitudes
c
      ymn = 0.5
      ymx = real (N_LAT) + 0.5 
c
c     ..set color scheme
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..loop over temp and salt
c
      do var = 1, 2
         if (var .eq. 1) typ = tmp
         if (var .eq. 2) typ = sal
c
c        ..load and scale data impacts
c
         do i = 1, N_LAT
            freq(i) = 0.
            if (typ .gt. 0) then
               freq(i) = dp(i,typ)
            endif
         enddo
c
         h = abs (minval (freq)) + 0.05 * abs (minval (freq))
         if (h .gt. 0.) then
            do i = 1, N_LAT
               freq(i) = freq(i) / h
               if (freq(i) .gt. 0.1) freq(i) = 0.1
            enddo
         endif
         if (var .eq. 1) then
            lbl = 'Temperature Impact'
         else
            lbl = '  Salinity Impact '
         endif
         xmx = -h
         fmn = 0.1
         fmx = -1.
         h = 0.5 * (fmn + fmx)
         g = 0.5 * (ymn + ymx)
         siz = 0.013
         off = 0.65
c
c        ..set plot window position
c
         if (var .eq. 1) then
            call set (.10, .50, .225, .775, fmn, fmx, ymn, ymx, 1)
         else if (var .eq. 2) then
            call set (.55, .95, .225, .775, fmn, fmx, ymn, ymx, 1)
         endif
c
c        ..plot histogram line plot
c
         call gslwsc (12.)
         do j = 1, N_LAT
            if (freq(j) .lt. 0.) then
               call gsplci (var + 2)
            else
               call gsplci (var + 4)
            endif
            yj = real (j)
            call frstpt (0., yj)
            call vector (freq(j), yj)
            call plotit (0, 0, 0)
         enddo
         call gsplci (1)
         call gslwsc (1.)
c
c        ..label latitude axis
c
         call plchhq (h, (ymn-2.), lbl, 0.014, 0., 0.)
         if (var .eq. 1) then
            xi = fmn - 0.2 * (fmx - fmn) 
            call plchhq (xi, g, 'Latitude', 0.014, 90., 0.)
            xi = fmn - 0.075 * (fmx - fmn)
            do j = 1, N_LAT, 2
               n = int (LAT1) + (j-1) * MESH
               yj = real (j)
               write (lbl3, '(i3)') n
               call plchhq (xi, yj, lbl3, siz, 0., 0.)
            enddo
         endif
c
c        ..label impact axis
c
         call plchhq (0., (ymn-off), '0.0', siz, 0., 0.)
         if (abs (xmx) .lt. 0.01) then
            write (lbl6, '(f6.3)') xmx
            call plchhq (fmx, (ymn-off), lbl6, siz, 0., 0.)
         else if (abs (xmx) .lt. 0.1) then
            write (lbl5, '(f5.2)') xmx
            call plchhq (fmx, (ymn-off), lbl5, siz, 0., 0.)
         else if (abs (xmx) .lt. 1.) then
            write (lbl4, '(f4.1)') xmx
            call plchhq (fmx, (ymn-off), lbl4, siz, 0., 0.)
         else if (abs (xmx) .lt. 10.) then
            write (lbl4, '(f4.1)') xmx
            call plchhq (fmx, (ymn-off), lbl4, siz, 0., 0.)
         else if (abs (xmx) .lt. 100.) then
            write (lbl5, '(f5.1)') xmx
            call plchhq (fmx, (ymn-off), lbl5, siz, 0., 0.)
         else if (abs (xmx) .lt. 1000.) then
            write (lbl6, '(f6.1)') xmx
            call plchhq (fmx, (ymn-off), lbl6, siz, 0., 0.)
         else if (abs (xmx) .lt. 10000.) then
            write (lbl7, '(f7.1)') xmx
            call plchhq (fmx, (ymn-off), lbl7, siz, 0., 0.)
         else
            write (lbl8, '(i8)') nint (xmx)
            call plchhq (fmx, (ymn-off), lbl8, siz, 0., 1.)
         endif
c
c        ..draw border around plot
c
         call line (fmn, ymn, fmx, ymn)
         call line (fmn, ymn, fmn, ymx)
         call line (fmn, ymx, fmx, ymx)
         call line (fmx, ymn, fmx, ymx)
c
c        ..draw heavy set zero line
c
         call setusv ('LW', 3000)
         call line (0., ymn, 0., ymx)
         call setusv ('LW', 1000)
      enddo
c
c     ..plot title
c
      call plchhq (0.2, (ymx+3.5), trim (title), 0.017, 0., 0.)
      plot_title = trim (area) // '    ' // typ_lbl
      call plchhq (0.2, (ymx+2.), trim (plot_title),
     *             0.017, 0., 0.)
c
c     ..advance plot frame
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a,
     *       ''  Latitude Impacts'')')
     *       fno, typ_lbl
      call frame
c
c     ..clean up
c
      deallocate (dp, ds, np, xp, xs)
c
      return
      end
