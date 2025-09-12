      subroutine plt_sss_sctr (date, n_obs, n_data, arg_clm, arg_sss,
     *                         scl_sss, sss_sss, sss_typ, sat_typ,
     *                         sat_lbl)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plt_sss_bias
c
c DESCRIPTION:  generates scatter plots of sss, including
c               linear regression analyses
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
c     ..local array dimensions
c
      integer   n_obs
c
      character date * (*)
      real      del
      real      dmn, dmx
      integer   i, k, m
      integer   ifail
      integer   kmn, kmx
      character lbl * 25
      integer   n
      integer   n_data
      real      pos (4)
      real      res (20)
      character sat_lbl * 4
      integer   sat_typ
      real      siz
      real      smn, smx
      real      sry
      real      tck
      character title * 80
      character tlbl * 80
      real      xp, yp
c
c     ..observation arrays
c
      real      arg_clm (n_obs)
      real      arg_sss (n_obs)
      real      scl_sss (n_obs)
      real      sss_sss (n_obs)
      integer   sss_typ (n_obs)
c
c     ..allocatable arrays
c
      real,     allocatable :: rx (:)
      real,     allocatable :: ry (:)
      real,     allocatable :: x (:)
      real,     allocatable :: y (:)
c
c     ..set color tables
c
      include 'color_table.h'
c
      data      pos / .10, .90, .10, .90 /
c
c...............................executable..............................
c
c     ..set color scheme (obs color table)
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..clip data at window boundaries
c
      call gsclip (1)
c
c     ..allocate arrays
c
      allocate (rx (n_obs))
      allocate (ry (n_obs))
      allocate (x (n_obs))
      allocate (y (n_obs))
c
      do k = 1, 3
c
c     ..set plot labels
c
      if (k .eq. 1) then
         tlbl = 'Raw Satellite SSS vs. Argo SSS Correlation       '
      else if (k .eq. 2) then
         tlbl = 'Scaled Satellite SSS vs. Argo SSS Correlation    '
      else if (k .eq. 3) then
         tlbl = 'Scaled Satellite SSS vs. Climate SSS Correlation '
      endif
c
c     ..load obs arrays
c
      n = 0
      do i = 1, n_data
         if (sss_typ(i) .eq. sat_typ) then
            n = n + 1
            if (k .eq. 1) then
               x(n) = sss_sss(i)
               y(n) = arg_sss(i)
            else if (k .eq. 2) then
               x(n) = scl_sss(i)
               y(n) = arg_sss(i)
            else if (k .eq. 3) then
               x(n) = scl_sss(i)
               y(n) = arg_clm(i)
            endif
         endif
      enddo
c
      if (n .eq. 0) cycle
c
c     ..perform linear regression
c
      res = 0.
      call g02cae (n, x, y, res, ifail)
      write (*, '(/, ''Satellite: '', a, 3x, a)') sat_lbl, trim (tlbl)
      write (*, '(5x, '' number obs: '', i10)') n        
      write (*, '(5x, ''correlation: '', f10.4)') res(5)
      write (*, '(5x, ''      slope: '', f10.4)') res(6)
      write (*, '(5x, ''  intercept: '', f10.4)') res(7)
c
c     ..compute residuals and regression bias
c
      sry = 0.
      do i = 1, n
        rx(i) = x(i)
        ry(i) = y(i) - (res(7) + res(6) * x(i))
        sry = sry + ry(i)
      enddo
      sry = sry / real (n)
      write (*, '(5x, ''   residual: '', f10.4)') sry
c
      do m = 1, 2
c
c     ..call set routine
c
      if (m .eq. 1) then
         dmn = 30.
         dmx = 38.
         smn = 30.
         smx = 38.
      else if (m .eq. 2) then
         dmn = 30.
         dmx = 38.
         smn = -5.
         smx =  5.
      endif       
      call set (pos(1), pos(2), pos(3), pos(4),
     *          dmn, dmx, smn, smx, 1)
c
c     ..set display color, size, and type
c
      call gspmci (4)
      call gsmksc (0.2)
      call gsmk (3)
c
c     ..mark obs position
c
      do i = 1, n
         if (m .eq. 1) then
            call gpm (1, x(i), y(i))
         else if (m .eq. 2) then
            call gpm (1, rx(i), ry(i))
         endif
      enddo
      call plotit (0, 0, 0)
c
c     ..draw perfect fit axis
c
      call gsplci (1)
      if (m .eq. 1) then
         call line (dmn, smn, dmx, smx)
      else
         call line (dmn, 0., dmx, 0.)
      endif
      call plotit (0, 0, 0)
c
c     ..plot regression line
c
      if (m .eq. 1) then
         call gsplci (3)
         call setusv ('LW', 2000)
         kmn = nint (dmn)
         kmx = nint (dmx)
         xp = real (kmn)
         yp = res(7) + res(6) * xp
         call frstpt (xp, yp)
         do i = kmn, kmx
            xp = real (i)
            yp = res(7) + res(6) * xp
            call vector (xp, yp)     
         enddo
         call setusv ('LW', 1000)
         call plotit (0, 0, 0)
      endif
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
      if (m .eq. 1) then
         if (k .eq. 1) then
            lbl = 'Argo SSS   '
         else if (k .eq. 2) then
            lbl = 'Argo SSS   '
         else if (k .eq. 3) then
            lbl = 'Climate SSS'
         endif
      else
         lbl = 'SSS Residual'
      endif
      call lbl_axis (smn, smx, del, tck, lbl,  90., siz,
     *               .08, .false.)
      if (k .eq. 1) then
         lbl = 'Raw Satellite SSS'
      else if (k .eq. 2) then
         lbl = 'Satellite SSS    '
      else if (k .eq. 3) then
         lbl = 'Satellite SSS    '
      endif
      call lbl_axis (dmn, dmx, del, tck, lbl, 0., siz,
     *               .05, .false.)
c
c     ..put titles above plot
c
      write (title, '(a, 5x, a)') trim (sat_lbl), trim (tlbl)
      call title_plot (title, .022, 1, 2.2)
      call title_plot (date,  .022, 1, 0.8)
c
c     ..advance the plot buffer
c
      call frame
      enddo
      enddo
c
c     ..clean up
c
      deallocate (rx, ry, x, y)
c
      return
      end
