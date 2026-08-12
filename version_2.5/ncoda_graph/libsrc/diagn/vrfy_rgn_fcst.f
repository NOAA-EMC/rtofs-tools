      subroutine vrfy_rgn_fcst (n_var, dtg, title1, title3, n_obs,
     *                          n_data, n_fcst, ob_lvl, ob_typ, 
     *                          ob_val, ob_var, ob_vfy)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vrfy_rgn_fcst
c
c DESCRIPTION:  plots vertical distributions of forecast statistics
c               by observing system for a given geographic regions
c
c PARAMETERS:
c       Name          Type        Usage            Description
c   -------------   ----------   -------   ----------------------------
c   n_obs           integer      input     number obs to process
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................END PROLOGUE.............................
c
      implicit none
c
      integer    NZ
      parameter (NZ = 40)
c
      integer    MX_AMAP
      parameter (MX_AMAP = 2 400 000)
c
      integer    MX_AI
      parameter (MX_AI = 16 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 160 000)
c
      integer    MX_CLR
      parameter (MX_CLR = 11)
c
c     ..local array dimensions
c
      integer   n_obs
      integer   n_fcst
c
      integer   clr (8)
      real      dmn, dmx
      real      dl, dt, dv, dz
      character dtg * 10
      integer   fno
      integer   i, j, k, kz, l, m, n
      character label (8) * 3
      integer   n_data
      integer   n_var
      real      omf
      character sfx * 20
      character sfx_lbl * 15
      real      size
      real      spmis
      real      spval
      character title1 * 80
      character title3 * 256
      character temp * 256
      character tmp_name * 80
      integer   typ
      real      u, v
      real      xvpl, xvpr, yvpb, yvpt
      real      ymn, ymx
      real      z_lvl (NZ)
c
c     ..obs arrays
c
      real      ob_bkg (n_obs)
      real      ob_lvl (n_obs)
      integer   ob_typ (n_obs)
      real      ob_val (n_obs)
      integer   ob_var (n_obs)
      real      ob_vfy (n_obs, n_fcst)
c
c     ..allocatable arrays
c
      real,     allocatable :: bias (:,:)
      real,     allocatable :: rms (:,:)
      real,     allocatable :: cnt (:,:)
      real,     allocatable :: zk (:)
c
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycs (:)
c
      data z_lvl /   0.,    5.,   10.,   20.,   30.,   40.,
     *              50.,  62.5,   75.,  100.,  125.,  150.,
     *             200.,  250.,  300.,  350.,  400.,  450.,
     *             500.,  550.,  600.,  700.,  800.,  900.,
     *            1000., 1100., 1200., 1300., 1400., 1500.,
     *            1600., 1700., 1800., 1900., 2000., 2100.,
     *            2250., 2500., 2750., 3000. /
c
      include 'color_table.h'
c
c...............................executable..............................
c
c     ..initialize
c
      dt = 50.
      dz = 100.
      fno = 0
      spval = -999.
      spmis = spval + 9.
      ymn = 1500.
c********************
c     ymn = 400.
c*******************
      ymx = 0.
c
      do j = 1, 8
         write (label(j), '(i3)') (j * 24)
      enddo
c
c     ..set gmeta file name, open gks
c
      write (tmp_name, '(''rgn_fcst.'', a, ''.gmeta '')') dtg
      call init_gks ('opn', tmp_name)
c
c     ..allocate stat arrays
c
      allocate (bias (NZ, 8))
      allocate (rms  (NZ, 8))
      allocate (cnt  (NZ, 8))
      allocate (zk (n_obs))
c
c     ..set depth level indicies
c
      zk = spval
      do i = 1, n_data
         do k = 2, NZ
            if (ob_lvl(i) .ge. z_lvl(k-1) .and.
     *          ob_lvl(i) .le. z_lvl(k)) then
               zk(i) = real (k-1) + (ob_lvl(i) - z_lvl(k-1)) /
     *                               (z_lvl(k) - z_lvl(k-1))
            endif
         enddo
      enddo
c
c     ..set color table
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
      do i = 1, 8
         clr(i) = i + 2
      enddo
      call setusv ('LW', 2000)
c
c     ..allocate plot arrays
c
      allocate (iai (MX_AI))
      allocate (iag (MX_AI))
      allocate (iamap (MX_AMAP))
      allocate (xcs (MX_LIN))
      allocate (ycs (MX_LIN))
c
c-----------------------------------------------------------------
c
c     ..loop over variables (temp, salt)
c
      do m = 1, n_var
c
c     ..compute forecast statistics
c
      bias = 0.
      cnt = 0.
      rms = 0.
      do i = 1, n_data
      if (ob_var(i) .eq. m) then
         if (zk(i) .gt. 0.) then
            kz = nint (zk(i))
            do j = 1, n_fcst
               if (ob_val(i)   .gt. spmis .and.
     *             ob_vfy(i,j) .gt. spmis) then
                  omf = ob_val(i) - ob_vfy(i,j)
                  bias(kz,j) = bias(kz,j) + omf
                  rms(kz,j) = rms(kz,j) + omf**2
                  cnt(kz,j) = cnt(kz,j) + 1.
               endif
            enddo
         endif
      endif
      enddo
c
c     ..normalize
c
      do j = 1, n_fcst
         do k = 1, NZ
            if (cnt(k,j) .gt. 0.) then
               bias(k,j) = bias(k,j) / cnt(k,j)
               rms(k,j) = sqrt (rms(k,j) / cnt(k,j))
            else
               bias(k,j) = spval
               rms(k,j) = spval
            endif
         enddo
      enddo
c
c     ..smooth
c
      do j = 1, n_fcst
         do k = 2, (NZ-1)
            if (cnt(k-1,j) .gt. 0. .and.
     *          cnt(k,j)   .gt. 0. .and.
     *          cnt(k+1,j) .gt. 0.) then
               bias(k,j) = 0.25 * bias(k-1,j) +
     *                     0.50 * bias(k,j) +
     *                     0.25 * bias(k+1,j)
               rms(k,j) = 0.25 * rms(k-1,j) +
     *                    0.50 * rms(k,j) +
     *                    0.25 * rms(k+1,j)
            endif
         enddo
      enddo
c
c-----------------------------------------------------------------
c
c     ..loop over plot types (bias, rms)
c
      do k = 1, 2
c
c     ..initialize plot variables
c
      if (m .eq. 1) then
         sfx = 'Argo Temperature'
         sfx_lbl = 'Temperature (C)'
         if (k .eq. 1) then
            dmn = -1.0
            dmx =  1.0
         else if (k .eq. 2) then
            dmn = 0.
            dmx = 2.0
         endif
         dl = 0.5
         dv = 0.25
      else 
         sfx = 'Argo Salinity'
         sfx_lbl = 'Salinity (PSU) '
         if (k .eq. 1) then
            dmn = -0.3
            dmx =  0.3
         else if (k .eq. 2) then
            dmn = 0.
            dmx = 0.6
         endif
         dl = 0.1
         dv = 0.1
      endif
c
c     ..set polymarker symbol and size
c
      call gsmk (3)
      call gsmksc (.3)
c
c     ..call set with min/max of variable and depth
c
      if (k .eq. 1) then
         call set (0.10, 0.45, 0.15, 0.85, dmn, dmx,
     *             ymn, ymx, 1)
      else
         call set (0.55, 0.90, 0.15, 0.85, dmn, dmx,
     *             ymn, ymx, 1)
      endif
      call gsplci (1)
      call gsfaci (1)
c
c     ..draw solid lines around chart
c
      xvpl = dmn
      xvpr = dmx
      yvpb = ymn
      yvpt = ymx
      call line (xvpl, yvpb, xvpr, yvpb)
      call line (xvpl, yvpb, xvpl, yvpt)
      call line (xvpl, yvpt, xvpr, yvpt)
      call line (xvpr, yvpb, xvpr, yvpt)
c
c     ..label depth and variable axes
c
      size = .03
      call lbl_axis (ymx, ymn, dz, dt, 'Depth', 90., size,
     *               0.535, .true.)
      if (k .eq. 1) then
         call lbl_axis (dmn, dmx, dl, dv, sfx_lbl, 0., size,
     *                  0.04, .false.)
      else
         call lbl_axis (dmn, dmx, dl, dv, sfx_lbl, 0., size,
     *                  0.04, .false.)
      endif
c
c     ..draw zero axis (bias)
c
      if (k .eq. 1) then
         call gsplci (1)
         call line (0., ymn, 0., ymx)
         call plotit (0, 0, 0)
      endif
c
c     ..plot profiles
c
      call setusv ('LW', 3000)
      if (k .eq. 1) then
         do j = 1, n_fcst
            call gsplci (clr(j))
            call frstpt (bias(1,j), z_lvl(1))
            do l = 1, 30
c           do l = 1, 17
               if (bias(l,j) .gt. spmis) then
                  call vector (bias(l,j), z_lvl(l))
               endif
            enddo
            call plotit (0, 0, 0)
         enddo
      else
         do j = 1, n_fcst
            call gsplci (clr(j))
            call frstpt (rms(1,j), z_lvl(1))
            do l = 1, 30
c           do l = 1, 17
               if (rms(l,j) .gt. spmis) then
                  call vector (rms(l,j), z_lvl(l))
               endif
            enddo
            call plotit (0, 0, 0)
         enddo
      endif
      call setusv ('LW', 2000)
c
c     ..put label bar on graph
c
      call gsplci (1)
      call gsfais (1)
      call lbseti ('CBL - color boxlines', 1)
      call lbseti ('CLB - color labels', 1)
      if (k .eq. 2) then
         call lblbar (0, .55, .90, .0, .075, 8, 1., .25,
     *                clr, 0, label, 8, 1)
      endif
      call plotit (0, 0, 0)
      call gsplci (1)
      call gsfaci (1)
c
c     ..put titles on plot
c
      call set (.1, .9, .1, .9, .1, .9, .1, .9, 1)
      write (temp, '(a, 4x, a, '' Verification '')')
     *       trim (title1), trim (sfx)
      call title_plot (temp,   .022, 1, 1.5)
      call title_plot (title3, .022, 1, 0.)
c
c     ..return for new plot type
c
      enddo
c
c     ..generate plot
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (temp)
      call frame
c
c     ..return for new variable
c
      enddo
c
c     ..close gks and clean up
c
      call setusv ('LW', 1000)
      call init_gks ('cls', tmp_name)
      deallocate (iai, iag, iamap, xcs, ycs)
      deallocate (bias, cnt, rms, zk)
c
      return
      end
