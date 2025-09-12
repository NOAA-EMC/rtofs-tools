      subroutine impact_hist_sfc (n_sat, n_typ, sat_typ, sat_nam, 
     *                            n_obs, n_data, ob_imp, ob_sen,
     *                            ob_typ, title, area, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  impact_hist_sfc
c
c DESCRIPTION:  computes and plots histograms of data impacts for
c               raw satellite sst/sss observations
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libocnqc.a
c      
c PARAMETERS:
c     Name         Type       Usage              Description
c   ---------    --------    -------    --------------------------------
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..local array dimensions
c
      integer   n_obs
      integer   n_sat
      integer   n_typ
c
      character area * (*)
      integer   color (n_sat)
      real      fmn, fmx
      integer   fno
      real      freq (20)
      real      h
      integer   i, j, k, m, n
      character knam (n_sat) * 7
      integer   kno (n_sat)
      integer   ksat
      character lbl * 15
      character lbl4 * 4
      character lbl5 * 5
      character lbl8 * 8
      integer   n_data
      real      ob_imp (n_obs)
      real      ob_sen (n_obs)
      integer   ob_typ (n_obs)
      integer   plt, sat
      character plt_title * 256
      character sat_nam (n_sat) * 7
      integer   sat_typ (n_typ, n_sat)
      real      siz
      real      spmis
      character title * (*)
      character title_plus * 96
      integer   var
      real      xi, xl, xr
      real      xmn, xmx
      real      ymn, ymx
c
c     ..allocatable diagnostic arrays
c
      real,     allocatable :: dp (:)
      real,     allocatable :: ds (:)
      integer,  allocatable :: np (:)
      real,     allocatable :: xp (:)
      real,     allocatable :: xs (:)
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
      allocate (dp (n_sat))
      allocate (ds (n_sat))
      allocate (np (n_sat))
      allocate (xp (n_sat))
      allocate (xs (n_sat))
c
c     ..loop over variables
c
      do var = 1, 2
      if (var .eq. 1) then
         ksat = 6
         do k = 1, ksat
            kno(k) = k
            knam(k) = sat_nam(kno(k))
         enddo
      else if (var .eq. 2) then
         ksat = 2
         do k = 1, ksat
            kno(k) = k + 6 
            knam(k) = sat_nam(kno(k))
         enddo
      endif
c
c     ..initialize, compute sensitivities and impacts by data type
c
      do i = 1, n_sat
         dp(i) = 0.
         ds(i) = 0.
         np(i) = 0
         xp(i) = 0.
         xs(i) = 0.
      enddo
      do i = 1, n_data
         do m = 1, n_sat
            do n = 1, n_typ
               if (ob_typ(i) .eq. sat_typ(n,m)) then
                  xp(m) = xp(m) + ob_imp(i)
                  xs(m) = xs(m) + ob_sen(i)
                  np(m) = np(m) + 1
               endif
            enddo
         enddo
      enddo
      do i = 1, n_sat
         if (np(i) .gt. 0) then
            dp(i) = xp(i) / real (np(i))
            ds(i) = xs(i) / real (np(i))
         endif
      enddo
c
c------------------------------------------------------------------
c
c     ..set color scheme
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
      color = 0
c
c     ..set range satellites 
c     
      xmn = 0.5
      xmx = real (ksat) + 0.5
c
c     ..loop over per ob and total ob impacts plots
c
      do plt = 1, 2
         freq = 0.
c
c        ..load and scale data impacts
c
         if (plt .eq. 1) then
            do i = 1, ksat
               freq(i) = dp(kno(i))
            enddo
            h = abs (minval (freq)) + 0.05 * abs (minval (freq))
            if (h .gt. 0.) then
               do i = 1, ksat
                  freq(i) = freq(i) / h
                  if (freq(i) .gt. 0.1) freq(i) = 0.1
               enddo
            endif
            lbl = 'Per Ob Impact  '
         else
            do i = 1, ksat
               freq(i) = xp(kno(i))
            enddo
            h = abs (minval (freq)) + 0.05 * abs (minval (freq))
            if (h .gt. 0.) then
               do i = 1, ksat
                  freq(i) = freq(i) / h
                  if (freq(i) .gt. 0.1) freq(i) = 0.1
               enddo
            endif
            lbl = 'Total Ob Impact'
         endif
         ymn = -h
         ymx = 0.
         fmn = -1.
         fmx = 0.1
         h = 0.5 * (fmn + fmx)
c
c        ..set plot window position
c
         if (plt .eq. 1) then
            call set (.10, .45, .325, .675, xmn, xmx, fmn, fmx, 1)
         else if (plt .eq. 2) then
            call set (.60, .95, .325, .675, xmn, xmx, fmn, fmx, 1)
         endif
c
c        ..plot histogram line plot
c
         call setusv ('LW', 1000)
         call gslwsc (36.)
         do i = 1, ksat
            color(i) = i + 2
            call gsplci (color(i))
            xi = real (i)
            call frstpt (xi, 0.)
            call vector (xi, freq(i))
            call plotit (0, 0, 0)
         enddo
         call gsplci (1)
         call gslwsc (1.)
c
c        ..label axis
c
         call setusv ('LW', 2000)
         call plchhq ((xmn-0.1*(xmx-xmn)), h, lbl, 0.014, 90., 0.)
         call gsplci (1)
         call plchhq ((xmn-0.01*(xmx-xmn)), 0., '0',
     *                .012, 0., 1.)
         if (plt .eq. 1) then    
            if (abs (ymn) .lt. 0.1) then
               write (lbl5, '(f5.2)') ymn
               call plchhq ((xmn-0.01*(xmx-xmn)), fmn, lbl5,
     *                      .012, 0., 1.)
            else if (abs (ymn) .lt. 1.) then
               write (lbl4, '(f4.1)') ymn
               call plchhq ((xmn-0.01*(xmx-xmn)), fmn, lbl4,
     *                      .012, 0., 1.)
            endif
         else if (plt .eq. 2) then
            write (lbl8, '(i8)') int (ymn / 1.e2)
            call plchhq ((xmn-0.01*(xmx-xmn)), fmn, lbl8,
     *                   .012, 0., 1.)
         endif
c     
c        ..draw border around plot
c
         call line (xmn, fmn, xmx, fmn)
         call line (xmn, fmn, xmn, fmx)
         call line (xmn, fmx, xmx, fmx)
         call line (xmx, fmn, xmx, fmx)
c
c        ..draw heavy set zero line
c
         call line (xmn, 0., xmx, 0.)
      enddo
c
c     ..put label bar below plot
c
      call gsplci (1)
      call gsfais (1)
      call lbseti ('CBL - color boxlines', 1)
      call lbseti ('CLB - color labels', 1)
      if (var .eq. 1) then
         call lblbar (0, .2, .85, .24, .30, ksat, 1., .20,
     *                color, 0, knam, ksat, 1)
      else
         call lblbar (0, .45, .61, .24, .30, ksat, 1., .20,
     *                color, 0, knam, ksat, 1)
      endif
c
c     ..plot titles
c
      if (var .eq. 1) then
         plt_title = trim (area) // '  SST Data Impacts (C)'
      else if (var .eq. 2) then
         plt_title = trim (area) // '  SSS Data Impacts (PSU)'
      endif
      call set (.1, .9, .325, .675, xmn, xmx, fmn, fmx, 1)
      siz = .019
      call title_plot (title, siz, 1, 2.5)
      call title_plot (plt_title, siz, 1, 1.1)
c
c     ..advance plot frame
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (title)
      call frame
      enddo
c
c     ..clean up
c
      deallocate (dp, ds, np, xp, xs)
c
      return
      end
