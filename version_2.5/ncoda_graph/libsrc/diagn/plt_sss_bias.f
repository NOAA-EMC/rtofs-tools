      subroutine plt_sss_bias (date, n_obs, n_data, arg_sss, sss_sss, 
     *                         sss_typ, sat_typ, sat_lbl)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plt_sss_bias
c
c DESCRIPTION:  generates scatter plot of quadratic sss bias correction
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libphys.a
c
c PARAMETERS:
c       Name           Type     Usage             Description
c   -------------    --------   ------   ------------------------------
c
c....................MAINTENANCE SECTION................................
c
c MODULES CALLED:
c        Name                         Description
c   --------------     ---------------------------------------------
c
c METHOD:
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..set number regression variables (dependent and independent)
c
      integer    N_VAR
      parameter (N_VAR = 3)
      integer    N_DEP
      parameter (N_DEP = N_VAR - 1)
c
c     ..local array dimensions
c
      integer   n_data
      integer   n_obs
c
      character date * (*)
      real      del
      integer   i, j
      integer   kmn, kmx
      character lbl * 25
      real      pos (4)
      real      siz
      real      smn, smx
      real      tck
      character title * 80
      character sat_lbl * 4
      integer   sat_typ
      real      xp, yp
c
c     ..nag regression model variables
c
      real      const (3)
      integer   ifail
      integer   n
      real      res (13)
c
c     ..observation arrays
c
      real      arg_sss (n_obs)
      real      sss_sss (n_obs)
      integer   sss_typ (n_obs)
c
c     ..allocatable nag arrays
c
      real,     allocatable :: coef (:)
      real,     allocatable :: coeff (:,:)
      real,     allocatable :: cz (:,:)
      real,     allocatable :: rz (:,:)
      real,     allocatable :: rzinv (:,:)
      real,     allocatable :: sspz (:,:)
      real,     allocatable :: std (:)
      real,     allocatable :: x (:,:)
      real,     allocatable :: xbar (:)
      real,     allocatable :: wrk (:,:)
c
c     ..set color tables
c
      include 'color_table.h'
c
      data      pos / .10, .90, .10, .90 /
c
c...............................executable..............................
c
c     ..allocate nag arrays
c
      allocate (coef (n_var))
      allocate (coeff (n_var, 3))
      allocate (cz (n_var, n_var))
      allocate (rz (n_var, n_var))
      allocate (rzinv (n_var, n_var))
      allocate (sspz (n_var, n_var))
      allocate (std (n_var))
      allocate (x (n_data, n_var))
      allocate (xbar (n_var))
      allocate (wrk (n_var, n_var))
c
c     ..initialize
c
      coef = 0.
      coeff = 0.
      cz = 0.
      rz = 0.
      rzinv = 0.
      sspz = 0.
      std = 0.
      x = 0.
      xbar = 0.
      wrk = 0.
c
      const = 0.
      ifail = 0
      res = 0.
c
c     ..load nag data array
c
      n = 0
      do i = 1, n_data
         if (sss_typ(i) .eq. sat_typ) then
            n = n + 1
            x(n,1) = arg_sss(i)
            x(n,2) = arg_sss(i) * arg_sss(i)
            x(n,3) = sss_sss(i)
         endif
      enddo
c
      if (n .gt. 0) then
c
c     ..quadratic regression model
c
      call g02bde (n, n_var, x, n_data, xbar, std, sspz, n_var, rz,
     *             n_var, ifail)
      if (ifail .ne. 0) then
         write (*, '(''NAG routine g02bde failed'')')
         return
      endif
      call g02cge (n, n_var, n_dep, xbar, sspz, n_var, rz, n_var,
     *             res, coeff, n_var, const, rzinv, n_var, cz,
     *             n_var, wrk, n_var, ifail)
      if (ifail .ne. 0) then
         write (*, '(''NAG routine g02cge failed'')')
         return
      endif
c
c     ..update bias coefficient array
c   
      coef(1) = coeff(1,1)
      coef(2) = coeff(2,1)
      coef(3) = const(1)
c
c     ..report regression model
c
      write (*, '(/, ''Satellite Type: '', i3, 2x, a)') sat_typ, sat_lbl
      write (*, '(8x, ''variable     coeff      std err'',
     *                ''      t-value'')')
      write (*, '(10x, i3, 3f13.4)') (i, (coeff(i,j), j = 1, 3),
     *                                            i = 1, n_dep)
      write (*, '(8x, ''const'', 3f13.4)') (const(i), i = 1, 3)
      write (*, '(29x, ''sums of squares    df    mean square'',
     *                 ''       f-value'')')
      write (*, '(10x, ''due to regression: '', f14.4, f8.0, 2f14.4)')
     *           (res(i), i = 1, 4)
      write (*, '(10x, '' about regression: '', f14.4, f8.0, 2f14.4)')
     *           (res(i), i = 5, 7)
      write (*, '(10x, ''            total: '', f14.4, f8.0, 2f14.4)')
     *           (res(i), i = 8, 9)
      write (*, '(10x, ''     number cases: '', i14)') n
      write (*, '('' '')')
      write (*, '(10x, ''standard error of estimate: '', f8.4)') res(10)
      write (*, '(10x, ''  multiple correlation (R): '', f8.4)') res(11)
      write (*, '(10x, '' determination (R squared): '', f8.4)') res(12)
      write (*, '(10x, ''       corrected R squared: '', f8.4)') res(13)
c
c-----------------------------------------------------------------------
c
c     ..set color scheme (obs color table)
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..clip data at window boundaries
c
      call gsclip (1)
c
c     ..call set routine with min/max of sss
c
      del = 1.
      tck = 1.
      smn = 30.
      smx = 38.
      call set (pos(1), pos(2), pos(3), pos(4),
     *          smn, smx, smn, smx, 1)
c
c     ..set display color, size, and type
c
      call gspmci (4)
      call gsmksc (0.2)
      call gsmk (3)
c
c     ..mark obs position in sss space
c
      do i = 1, n_data
         if (sss_typ(i) .eq. sat_typ) then
            call gpm (1, sss_sss(i), arg_sss(i))
         endif
      enddo
      call plotit (0, 0, 0)
c
c     ..draw zero axis
c
      call gsplci (1)
      call setusv ('LW', 3000)
      call line (smn, smn, smx, smx)
      call plotit (0, 0, 0)
c
c     ..compute and plot regression line
c
      call gsplci (3)
      kmn = nint (smn)
      kmx = nint (smx)
      xp = real (kmn)
      yp = coef(3) + coef(1) * xp + coef(2) * (xp * xp)
      call frstpt (xp, yp)
      do i = kmn, kmx
         xp = real (i)
         yp = coef(3) + coef(1) * xp + coef(2) * (xp * xp)
         call vector (xp, yp)
      enddo
      call setusv ('LW', 1000)
      call plotit (0, 0, 0)
c
c     ..draw a box around data area
c
      call gsplci (1)
      call plotif (pos(1), pos(3), 0)
      call plotif (pos(2), pos(3), 1)
      call plotif (pos(2), pos(4), 1)
      call plotif (pos(1), pos(4), 1)
      call plotif (pos(1), pos(3), 1)
      call plotif (pos(1), pos(3), 2)
c
c     ..label axes
c
      del = 1.
      tck = 1.
      siz = .018
      lbl = 'Argo SSS'
      call lbl_axis (smn, smx, del, tck, lbl, 90., siz,
     *               .08, .false.)
      del = 10.
      tck = 5.
      lbl = 'Satellite SSS'
      call lbl_axis (smn, smx, del, tck, lbl,  0., siz,
     *               .05, .false.)
c
c     ..put titles above plot
c
      write (title, '(a, 5x, '' Argo vs. Satellite SSS Bias '',
     *                   ''Correction'')') trim (sat_lbl)
      call title_plot (title, .022, 1, 2.2)
      call title_plot (date,  .022, 1, 0.8)
c
c     ..advance the plot buffer
c
      call frame
      endif
c
c     ..clean up
c
      deallocate (coef, coeff, cz, rz, rzinv, sspz, std, x, xbar)
      deallocate (wrk)
c
      return
      end
   


