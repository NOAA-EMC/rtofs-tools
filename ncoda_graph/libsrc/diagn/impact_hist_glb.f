      subroutine impact_hist_glb (n_obs, n_files, n_obs_file, obs_imp,
     *                            obs_sen, obs_typ, obs_var, title,
     *                            fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  impact_hist_glb
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
      integer    N_TYP
      parameter (N_TYP = 2) 
c
      integer    N_VAR
      parameter (N_VAR = 2)
c
c     ..local array dimensions
c
      integer   n_files
      integer   n_obs
c
      integer   color (N_TYP)
      real      fmn, fmx
      integer   fno
      real      freq (N_TYP)
      real      h
      integer   i, j, k
      integer   kt, kv
      character lbl * 11
      character lbl4 * 4
      character lbl5 * 5
      character lbl10 * 10
      integer   mde
      integer   n_obs_file (n_files)
      real      obs_imp (n_obs, n_files)
      real      obs_sen (n_obs, n_files)
      integer   obs_typ (n_obs, n_files)
      integer   obs_var (n_obs, n_files)
      integer   plt
      character plt_title * 256
      character sal_lbl (N_TYP) * 6
      character title * (*)
      character tmp_lbl (N_TYP) * 6
      character typ_lbl (N_TYP) * 6
      integer   var
      integer   var_typ (N_TYP, N_VAR)
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
c     ..define type codes (temperature, salinity)
c
      data var_typ / 36, 19,
     *               37, 49 /
c
c     ..define type code labels
c
      data tmp_lbl /'Argo  ', 'SSH  ' /
      data sal_lbl /'Argo  ', 'SSH  ' /
c
c...............................executable..............................
c
c     ..allocate work arrays
c
      allocate (dp (0:MX_TYPES, N_VAR))
      allocate (ds (0:MX_TYPES, N_VAR))
      allocate (np (0:MX_TYPES, N_VAR))
      allocate (xp (0:MX_TYPES, N_VAR))
      allocate (xs (0:MX_TYPES, N_VAR))
c
c     ..impacts by data type
c
      do j = 1, N_VAR
         do i = 0, MX_TYPES
            dp(i,j) = 0.
            ds(i,j) = 0.
            np(i,j) = 0
            xp(i,j) = 0.
            xs(i,j) = 0.
         enddo
      enddo
      do k = 1, n_files
         if (n_obs_file(k) .gt. 0) then
            do i = 1, n_obs_file(k)
               if (obs_var(i,k) .le. N_VAR) then
                  kt = obs_typ(i,k)
                  kv = obs_var(i,k)
                  xp(kt,kv) = xp(kt,kv) + obs_imp(i,k)
                  xs(kt,kv) = xs(kt,kv) + obs_sen(i,k)
                  np(kt,kv) = np(kt,kv) + 1
               endif
            enddo
         endif
      enddo
      do j = 1, N_VAR
         do i = 0, MX_TYPES
            if (np(i,j) .gt. 0) then
               dp(i,j) = xp(i,j) / real (np(i,j))
               ds(i,j) = xs(i,j) / real (np(i,j))
            endif
         enddo    
      enddo
c
c------------------------------------------------------------------
c
c     ..set range data types
c
      xmn = 0.5
      xmx = real (N_TYP) + 0.5
c
c     ..set color scheme
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..loop over temp and salt, total vs. per ob, impact vs. count
c
      do var = 1, N_VAR
         if (var .eq. 1) then
            typ_lbl = tmp_lbl
         else if (var .eq. 2) then
            typ_lbl = sal_lbl
         endif
         do plt = 1, 2
c
c        ..load and scale data impacts
c
         if (plt .eq. 1) then
            do i = 1, N_TYP
               freq(i) = 0.
               if (var_typ(i,var) .gt. 0) then
                  kt = var_typ(i,var)
                  freq(i) = dp(kt,var)
               endif
            enddo
            h = abs (minval (freq)) + 0.05 * abs (minval (freq))
            if (h .gt. 0.) then
               do i = 1, N_TYP
                  freq(i) = freq(i) / h
                  if (freq(i) .gt. 0.1) freq(i) = 0.1
               enddo
            endif
            ymn = -h
            ymx = 0.
         else if (plt .eq. 2) then
            do i = 1, N_TYP
               freq(i) = 0.
               if (var_typ(i,var) .gt. 0) then
                  kt = var_typ(i,var)
                  freq(i) = real (np(kt,var))
               endif
            enddo
            h = maxval (freq) + 0.05 * maxval (freq)
            if (h .gt. 0.) then
               do i = 1, N_TYP
                  freq(i) = freq(i) / h
               enddo
            endif
            ymn = 0.
            ymx = h
         endif
         if (plt .eq. 1) then
            if (var .eq. 1) then
               lbl = 'Temperature'
            else
               lbl = ' Salinity  '
            endif
            fmn = -1.
            fmx = 0.1
         else
            lbl = 'Data Count '
            fmn = 0.
            fmx = 1.
         endif
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
         call gslwsc (36.)
         do i = 1, N_TYP
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
         call plchhq (.0, h, lbl, 0.014, 90., 0.)
         call gsplci (1)
         if (plt .eq. 1) then
            call plchhq ((xmn - 0.01*(xmx-xmn)), 0., '0',
     *                   .012, 0., 1.)
            if (abs (ymn) .lt. 0.1) then
               write (lbl5, '(f5.2)') ymn
               call plchhq ((xmn - 0.01*(xmx-xmn)), fmn, lbl5,
     *                      .012, 0., 1.)
            else if (abs (ymn) .lt. 1.) then
               write (lbl4, '(f4.1)') ymn
               call plchhq ((xmn - 0.01*(xmx-xmn)), fmn, lbl4,
     *                      .012, 0., 1.)
            else
               write (lbl10, '(i10)') nint (ymn)
               call plchhq ((xmn - 0.01*(xmx-xmn)), fmn, lbl10,
     *                      .012, 0., 1.)
            endif
         else if (plt .eq. 2) then
            call plchhq ((xmn - 0.01*(xmx-xmn)), fmn, '0',
     *                   .012, 0., 1.)
            write (lbl10, '(i10)') int (ymx)
            call plchhq ((xmn - 0.01*(xmx-xmn)), fmx, lbl10,
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
         if (plt .eq. 1) then
            call setusv ('LW', 3000)
            call line (xmn, 0., xmx, 0.)
            call setusv ('LW', 1000)
         endif
c
c        ..put label bar below plot
c
         call gsplci (1)
         call gsfais (1)
         call lbseti ('CBL - color boxlines', 1)
         call lbseti ('CLB - color labels', 1)
         call lblbar (0, .44, .56, .24, .30, N_TYP, 1., .20,
     *                color, 0, typ_lbl, N_TYP, 1)
c
c        ..plot titles
c
         if (var .eq. 1) then
            plt_title = '  Temperature Impacts '
         else
            plt_title = '  Salinity Impacts '
         endif
         call plchhq (-1.3, 1.25, trim (title), 0.017, 0., 0.)
         call plchhq (-1.3, 1.15, trim (plt_title), 0.017, 0., 0.)
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
