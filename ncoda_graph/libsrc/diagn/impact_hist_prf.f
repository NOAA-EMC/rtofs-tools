      subroutine impact_hist_prf (n_obs, n_files, n_obs_file, n_sys,
     *                            n_var, sys_lbl, sys_typ, obs_imp,
     *                            obs_sen, obs_typ, obs_var, title,
     *                            area, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  impact_hist_prf
c
c DESCRIPTION:  computes and plots histograms of data impacts
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libocnqc.a
c      
c PARAMETERS:
c     Name         Type       Usage              Description
c   ---------    --------    -------    --------------------------------
c   area         character   input      area name
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
c     ..local array dimensions
c
      integer   n_files
      integer   n_obs
      integer   n_sys
      integer   n_var
c
      character area * (*)
      integer   color (n_sys)
      real      fmn, fmx
      integer   fno
      real      freq (n_sys)
      real      h
      integer   i, j, k
      integer   kt, kv
      character lbl * 15
      character lbl3 * 3
      character lbl4 * 4
      character lbl5 * 5
      character lbl6 * 6
      integer   n_obs_file (n_files)
      real      obs_imp (n_obs, n_files)
      real      obs_sen (n_obs, n_files)
      integer   obs_typ (n_obs, n_files)
      integer   obs_var (n_obs, n_files)
      integer   plt
      character plt_title * 256
      real      siz
      character sys_lbl (n_sys) * 7
      integer   sys_typ (n_sys, n_var)
      character title * (*)
      integer   var
      real      xi
      real      xmn, xmx
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
      allocate (dp (0:MX_TYPES, n_var))
      allocate (ds (0:MX_TYPES, n_var))
      allocate (np (0:MX_TYPES, n_var))
      allocate (xp (0:MX_TYPES, n_var))
      allocate (xs (0:MX_TYPES, n_var))
c
c     ..impacts by data type
c
      do j = 1, n_var
         do i = 0, MX_TYPES
            dp(i,j) = 0.
            ds(i,j) = 0.
            np(i,j) = 0
            xp(i,j) = 0.
            xs(i,j) = 0.
         enddo
      enddo
c
      do k = 1, n_files
         if (n_obs_file(k) .gt. 0) then
            do i = 1, n_obs_file(k)
               if (obs_var(i,k) .le. n_var) then
                  kt = obs_typ(i,k)
                  kv = obs_var(i,k)
                  xp(kt,kv) = xp(kt,kv) + obs_imp(i,k)
                  xs(kt,kv) = xs(kt,kv) + obs_sen(i,k)
                  np(kt,kv) = np(kt,kv) + 1
               endif
            enddo
         endif
      enddo
c
      do j = 1, n_var
         do i = 0, MX_TYPES
            if (np(i,j) .gt. 0) then
               dp(i,j) = xp(i,j) / real (np(i,j))
               ds(i,j) = xs(i,j) / real (np(i,j))
            endif
         enddo    
c******************
c     dp(1,1) = dp(1,1) * 1.
c     dp(36,j) = dp(36,j) * 1.5 
c     dp(37,j) = dp(37,j) * 1.5
c     dp(20,j) = dp(20,j) * 1.
c     dp(32,j) = dp(32,j) * 1.
c     dp(133,j) = dp(133,j) * 0.2
c     dp(134,j) = dp(134,j) * 0.2
c     ds(36,j) = ds(36,j) * 1.5
c     ds(37,j) = ds(37,j) * 1.5
c*******************    
      enddo
c
c------------------------------------------------------------------
c
c     ..set range data types
c
      xmn = 0.5
      xmx = real (n_sys) + 0.5
c
c     ..set color scheme
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..loop over temp and salt, total vs. per ob
c
      do var = 1, n_var
         do plt = 1, 2
c
c        ..load and scale data impacts
c
         if (plt .eq. 1) then
            do i = 1, n_sys
               freq(i) = 0.
               if (sys_typ(i,var) .gt. 0) then
                  kt = sys_typ(i,var)
                  freq(i) = dp(kt,var)
               endif
            enddo
            h = abs (minval (freq)) + 0.05 * abs (minval (freq))
            if (h .gt. 0.) then
               do i = 1, n_sys
                  freq(i) = freq(i) / h
                  if (freq(i) .gt. 0.1) freq(i) = 0.1
               enddo
            endif
            lbl = 'Per Ob Impact  '
         else if (plt .eq. 2) then
            do i = 1, n_sys
               freq(i) = 0.
               if (sys_typ(i,var) .gt. 0) then
                  kt = sys_typ(i,var)
                  freq(i) = real (xp(kt,var))
               endif
            enddo
            h = abs (minval (freq)) + 0.05 * abs (minval (freq))
            if (h .gt. 0.) then
               do i = 1, n_sys
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
         call setusv ('LW', 1000)
c
c        ..plot histogram line plot
c
         call gslwsc (36.)
         do i = 1, n_sys
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
c        ..set thick lines
c
         call setusv ('LW', 2000)
c
c        ..label axis
c
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
            if (abs (ymn) .lt. 100) then
               write (lbl3, '(i3)') int (ymn)
               call plchhq ((xmn-0.01*(xmx-xmn)), fmn, lbl3,
     *                      .012, 0., 1.)
            else if (abs (ymn) .lt. 1000.) then
               write (lbl4, '(i4)') int (ymn)
               call plchhq ((xmn-0.01*(xmx-xmn)), fmn, lbl4,
     *                      .012, 0., 1.)
            else if (abs (ymn) .lt. 10000.) then
               write (lbl5, '(i5)') int (ymn)
               call plchhq ((xmn-0.01*(xmx-xmn)), fmn, lbl5,
     *                      .012, 0., 1.)
            else
               write (lbl6, '(i6)') int (ymn)
               call plchhq ((xmn-0.01*(xmx-xmn)), fmn, lbl6,
     *                      .012, 0., 1.)
            endif
         endif
c     
c        ..draw border around plot
c
         call line (xmn, fmn, xmx, fmn)
         call line (xmn, fmn, xmn, fmx)
         call line (xmn, fmx, xmx, fmx)
         call line (xmx, fmn, xmx, fmx)
c
c        ..draw zero line
c
         call line (xmn, 0., xmx, 0.)
c
c        ..put label bar below plot
c
         call gsplci (1)
         call gsfais (1)
         call lbseti ('CBL - color boxlines', 1)
         call lbseti ('CLB - color labels', 1)
         call lblbar (0, .14, .9, .24, .30, n_sys, 1., .20,
     *                color, 0, sys_lbl, n_sys, 1)
c
c        ..plot titles
c
         if (var .eq. 1) then
            plt_title = trim (area) // ' Temperature Impacts '
         else
            plt_title = trim (area) // ' Salinity Impacts '
         endif
         if (plt .eq. 1) then
            call set (.10, .95, .325, .675, xmn, xmx, fmn, fmx, 1)
            siz = .019
            call title_plot (title, siz, 1, 2.5)
            call title_plot (plt_title, siz, 1, 1.1)
         endif
         enddo
c
c        ..advance plot frame
c
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (plt_title)
         call frame
      enddo
c
c     ..clean up
c
      deallocate (dp, ds, np, xp, xs)
c
      return
      end
