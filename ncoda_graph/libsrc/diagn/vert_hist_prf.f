      subroutine vert_hist_prf (n_obs, n_files, n_obs_file, obs_imp,
     *                          obs_lvl, obs_sen, obs_typ, obs_var,
     *                          title, tmp, sal, typ_lbl, area, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vert_hist_prf
c
c DESCRIPTION:  computes and plots histograms of data impacts as a
c               function of vertical level for profile data types 
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
      integer    NZ
      parameter (NZ = 12)
c
c     ..local array dimensions
c
      integer   n_files
      integer   n_obs
c
      character area * (*)
      real      fmn, fmx
      integer   fno
      real      freq (NZ)
      real      g, h
      integer   i, j, k, kz
      character lbl * 18
      character lbl4 * 4
      character lbl5 * 5
      character lbl6 * 6
      character lbl7 * 7
      character lbl8 * 8
      character lvl * 11
      integer   n_obs_file (n_files)
      real      obs_imp (n_obs, n_files)
      real      obs_lvl (n_obs, n_files)
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
      real      zl (NZ), zu (NZ)
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
c     ..define verification vertical layers
c
      data zl /  0.,   20.,   50.,  100.,  200.,  300.,  400.,
     *         500.,  600.,  800., 1000., 1200. /
      data zu / 20.,   50.,  100.,  200.,  300.,  400.,  500.,
     *         600.,  800., 1000., 1200., 1500. / 
c
c...............................executable..............................
c
c     ..allocate work arrays
c
      allocate (dp (NZ, 0:MX_TYPES))
      allocate (ds (NZ, 0:MX_TYPES))
      allocate (np (NZ, 0:MX_TYPES))
      allocate (xp (NZ, 0:MX_TYPES))
      allocate (xs (NZ, 0:MX_TYPES))
c
c     ..initialize
c
      do i = 0, MX_TYPES
         do k = 1, NZ
            dp(k,i) = 0.
            ds(k,i) = 0.
            np(k,i) = 0
            xp(k,i) = 0.
            xs(k,i) = 0.
         enddo
      enddo
c
c     ..compute vertical sums
c
      do k = 1, n_files
      if (n_obs_file(k) .gt. 0) then
         do i = 1, n_obs_file(k)
            if (obs_var(i,k) .ne. 3) then
               kz = 0
               do j = 1, NZ
                  if (obs_lvl(i,k) .ge. zl(j) .and.
     *                obs_lvl(i,k) .lt. zu(j)) then
                      kz = j
                  endif
               enddo
               if (kz .ge. 1 .and. kz. le. NZ) then
                  xp(kz,obs_typ(i,k)) = xp(kz,obs_typ(i,k)) +
     *                                  obs_imp(i,k)
                  xs(kz,obs_typ(i,k)) = xs(kz,obs_typ(i,k)) +
     *                                  obs_sen(i,k)
                  np(kz,obs_typ(i,k)) = np(kz,obs_typ(i,k)) + 1
               endif
            endif
         enddo
      endif
      enddo
c
c     ..normalize
c
      do i = 0, MX_TYPES
         do k = 1, NZ
            if (np(k,i) .gt. 0) then
               dp(k,i) = xp(k,i) / real (np(k,i))
               ds(k,i) = xs(k,i) / real (np(k,i))
            endif
         enddo
      enddo
c
c------------------------------------------------------------------
c
c     ..set color scheme
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..set range levels
c
      ymx = 0.5
      ymn = real (NZ) + 0.5
c
c     ..loop temperature and salinity
c
      do var = 1, 2
         if (var .eq. 1) typ = tmp
         if (var .eq. 2) typ = sal
c
c        ..load and scale vertical data impacts
c
         do j = 1, NZ
            freq(j) = 0.
            if (typ .gt. 0) then
               freq(j) = dp(j,typ)
            endif
         enddo
c
         h = abs (minval (freq)) + 0.05 * abs (minval (freq))
         if (h .gt. 0.) then
            do j = 1, NZ
               if (freq(j) .lt. 0.) then
                  freq(j) = freq(j) / h
               else
                  if (freq(j) .gt. 0.1) freq(j) = 0.1
               endif
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
         off = 0.4
c
c        ..set plot window position
c
         if (var .eq. 1) then
            call set (.05, .40, .325, .675, fmn, fmx, ymn, ymx, 1)
         else if (var .eq. 2) then
            call set (.60, .95, .325, .675, fmn, fmx, ymn, ymx, 1)
         endif
c
c        ..plot histogram line plot
c
         call gslwsc (24.)
         do j = 1, NZ
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
         call gslwsc (1.)
         call gsplci (1)
c
c        ..label depth axis
c
         call plchhq (h, (ymn+1.3), lbl, 0.014, 0., 0.)
         if (var .eq. 1) then
            call plchhq (0.2, g, 'Depth', 0.014, 90., 0.)
            xi = -1.31
            do j = 1, NZ
               yj = real (j)
               write (lvl, '(i4, '' - '', i4)')
     *                int (zl(j)), int (zu(j))
               call plchhq (xi, yj, lvl, siz, 0., 0.)
            enddo            
         endif
c
c        ..label impact axis
c
         call plchhq (0., (ymn+off), '0.0', siz, 0., 0.)
         if (abs (xmx) .lt. 0.01) then
            write (lbl6, '(f6.3)') xmx
            call plchhq (fmx, (ymn+off), lbl6, siz, 0., 0.)
         else if (abs (xmx) .lt. 0.1) then
            write (lbl5, '(f5.2)') xmx
            call plchhq (fmx, (ymn+off), lbl5, siz, 0., 0.)
         else if (abs (xmx) .lt. 1.) then
            write (lbl4, '(f4.1)') xmx
            call plchhq (fmx, (ymn+off), lbl4, siz, 0., 0.)
         else if (abs (xmx) .lt. 10.) then
            write (lbl4, '(f4.1)') xmx
            call plchhq (fmx, (ymn+off), lbl4, siz, 0., 0.)
         else if (abs (xmx) .lt. 100.) then
            write (lbl5, '(f5.1)') xmx
            call plchhq (fmx, (ymn+off), lbl5, siz, 0., 0.)
         else if (abs (xmx) .lt. 1000.) then
            write (lbl6, '(f6.1)') xmx
            call plchhq (fmx, (ymn+off), lbl6, siz, 0., 0.)
         else if (abs (xmx) .lt. 10000.) then
            write (lbl7, '(f7.1)') xmx
            call plchhq (fmx, (ymn+off), lbl7, siz, 0., 0.)
         else
            write (lbl8, '(i8)') nint (xmx)
            call plchhq (fmx, (ymn+off), lbl8, siz, 0., 1.)
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
      call plchhq (0.5, (ymx-2.35), trim (title), 0.017, 0., 0.)
      plot_title = trim (area) // '    ' // typ_lbl
      call plchhq (0.5, (ymx-1.15), trim (plot_title),
     *             0.017, 0., 0.)
c
c     ..advance plot frame
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a,
     *       ''  Vertical Impacts'')')
     *       fno, typ_lbl
      call frame
c
c     ..clean up
c
      deallocate (dp, ds, np, xp, xs)
c
      return
      end
