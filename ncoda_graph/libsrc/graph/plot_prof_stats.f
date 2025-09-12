      subroutine plot_prof_stats (n_src, n_sys, nz, stats, z_lvl,
     *                            n_obs, n_prf, lat, lon, typ, 
     *                            sys_lbl, sys_typ, bl, tr, dtg1,
     *                            dtg2, var_lbl, var, spval) 
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_prof_stats
c
c DESCRIPTION:  plots profiles of analysis and archive statistics
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
      integer   n_src, n_sys, nz
c
      real      bl (2)
      integer   clr (2)
      character date1 * 11
      character date2 * 11
      integer   day
      real      dmn, dmx
      real      dt, dl
      character dtg1 * 10
      character dtg2 * 10
      real      dz
      integer   i, k
      character label (2) * 4
      integer   mon
      character month (12) * 3
      integer   n_prf
      real      polon
      integer   plt
      real      size
      real      spmis
      real      spval
      integer   sys
      character sys_lbl (n_sys) * 6
      integer   sys_typ (n_sys, 2)
      character title1 * 256
      character title2 * 256
      real      tr (2)
      integer   var
      character var_lbl * 11
      real      x, y
      real      xvpl, xvpr, yvpb, yvpt
      integer   year
      real      ymn, ymx
c
c     ..obs arrays
c
      real      lat (n_obs)
      real      lon (n_obs)
      integer   typ (n_obs)
c
c     ..stat array
c
      real      stats (n_src, n_sys, nz, 3)
      real      z_lvl (nz+1)
c
c     ..external NCAR functions
c
      external  clin, cmap
c
c     ..allocatable ncar arrays
c
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycs (:)
c
c     ..ncar color tables
c
      include 'color_table.h'
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
c     ..initialize plot variables
c
      clr(1) = 3
      clr(2) = 4
      spmis = spval + 9.
      ymn = 2000.
      ymx = 0.
c
c     ..form date time group labels
c
      read (dtg2(1:4), '(i4)') year
      read (dtg1(5:6), '(i2)') mon
      read (dtg1(7:8), '(i2)') day
      write (date1, '(i2.2, 1x,a, 1x,i4)') day, month(mon), year
c
      read (dtg2(1:4), '(i4)') year
      read (dtg2(5:6), '(i2)') mon
      read (dtg2(7:8), '(i2)') day
      write (date2, '(i2.2, 1x,a, 1x,i4)') day, month(mon), year
c
c     ..allocate ncar arrays
c
      allocate (iai (MX_AI))
      allocate (iag (MX_AI))
      allocate (iamap (MX_AMAP))
      allocate (xcs (MX_LIN))
      allocate (ycs (MX_LIN))
c
c     ..set color table
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..loop over observing systems and stats
c
      do sys = 1, n_sys
      do plt = 1, 2
c
c        ..set up map background
c
         call arinam (iamap, MX_AMAP)
         call mapsti ('LA', 0)
         call mappos (.02, .38, .32, .68)
c        call mapset ('CO', bl(1), bl(2), tr(1), tr(2))
c        polon = (bl(2) + tr(2)) * 0.5
         call mapset ('CO', -75., 0., 75., 360)
         polon = 180.
         call maproj ('ME', 0., polon, 0.)
         call mapstr ('GR', 10.)
         call mplnam ('Earth..3', 1, iamap)
         call gsfais (1)
         call gsfaci (2)
         call arscam (iamap, xcs, ycs, MX_LIN, iai, iag,
     *                MX_AI, cmap)
         call gsplci (1)
         call gstxci (1)
c        call gsln (2)
c        call mapgrm (iamap, xcs, ycs, MX_LIN, iai, iag,
c    *                MX_AI, clin)
c        call gsln (1)
         call mplndr ('Earth..3', 1)
         call maplbl
c
c        ..mark profile positions
c
         call gsmk (3)
         call gsmksc (.2)
         call gspmci (4)
         do i = 1, n_prf
            if (typ(i) .eq. sys_typ(sys,var)) then
               call maptrn (lat(i), lon(i), x, y)
               call gpm (1, x, y)
            endif
         enddo
         call plotit (0, 0, 0)
         call gsplci (1)
         call gsfaci (1)
c
c        ..set polymarker symbol and size
c
         call gsmk (3)
         call gsmksc (.4)
c
         if (plt .eq. 1) then
            if (var .eq. 1) then
               dl = 0.1
               dmn = -0.3
               dmx =  0.3
            else
               dl = 0.1
               dmn = -0.3
               dmx =  0.3
            endif
         else
            dmn = 0.
            if (var .eq. 1) then
               dl = 0.5
               dmx = 1.
             else
               dl = 0.1
               dmx = 0.5
             endif
         endif
c
c        ..call set with min/max of statistics and depth
c
         call set (0.48, 0.98, 0.15, 0.85, dmn, dmx,
     *             ymn, ymx, 1)
         call gsplci (1)
         call gsfaci (1)
c
c        ..draw solid lines around chart
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
c        ..draw zero axis
c
         call gsplci (1)
         call line (0., ymn, 0., ymx)
         call plotit (0, 0, 0)
c
c        ..label depth and variable axes
c
         size = .025
         dt = 50.
         dz = 200.
         call lbl_axis (ymx, ymn, dz, dt, ' ', 90., size, 0.5, .true.)
         dt = dl * 1.
         if (var .eq. 1) then
            call lbl_axis (dmn, dmx, dl, dt, 'Temperature (C)',
     *                     0., size, 0.5, .false.)
         else
            call lbl_axis (dmn, dmx, dl, dt, 'Salinity (PSU)',
     *                     0., size, 0.5, .false.)
         endif
c
c        ..plot bias profiles
c
         call setusv ('LW', 3000)
         if (plt .eq. 1) then
            call gsplci (clr(1))
            x = stats(1,sys,1,1)
            y = z_lvl(1)
            call frstpt (x, y)
            do k = 1, nz
               x = stats(1,sys,k,1)
               y = z_lvl(k)
               if (x .gt. spmis) then
                  call vector (x, y)
               endif
            enddo
            call plotit (0, 0, 0)
c
            call gsplci (clr(2))
            x = stats(2,sys,1,1)
            y = z_lvl(1)
            call frstpt (x, y)
            do k = 1, nz
               x = stats(2,sys,k,1)
               y = z_lvl(k)
               if (x .gt. spmis) then
                  call vector (x, y)
               endif
            enddo
            call plotit (0, 0, 0)
         else if (plt .eq. 2) then
            call gsplci (clr(1))
            x = stats(1,sys,1,2)
            y = z_lvl(1)
            call frstpt (x, y)
            do k = 1, nz
               x = stats(1,sys,k,2)
               y = z_lvl(k)
               if (x .gt. spmis) then
                  call vector (x, y)
               endif
            enddo
            call plotit (0, 0, 0)
c
            call gsplci (clr(2))
            x = stats(2,sys,1,2)
            y = z_lvl(1)
            call frstpt (x, y)
            do k = 1, nz
               x = stats(2,sys,k,2)
               y = z_lvl(k)
               if (x .gt. spmis) then
                  call vector (x, y)
               endif
            enddo
            call plotit (0, 0, 0)
         endif
         call setusv ('LW', 1000)
c
c        ..put label bar on graph
c
         label(1) = 'Dan3'
         label(2) = 'Run4'
         call gsplci (1)
         call lblbar (0, .68, .78, .0, .075, 2, 1., .25,
     *                clr, 0, label, 2, 1)
c
c        ..put titles on plot
c
         call set (.1, .9, .1, .9, .1, .9, .1, .9, 1)
         if (plt .eq. 1) then
            write (title1, '(a, 1x,a, 1x,''Bias'')') 
     *             trim (sys_lbl(sys)), trim (var_lbl)
         else if (plt .eq. 2) then
            write (title1, '(a, 1x,a, 1x,''RMSE'')')
     *              trim (sys_lbl(sys)), trim (var_lbl)
         endif
         write (title2, '(a, '' to '', a)') date1, date2
         call title_plot (title1, .022, 1, 1.5)
         call title_plot (title2, .022, 1, 0.)
c
c        ..generate plot
c
         call frame
      enddo
      enddo
c
c     ..clean up
c
      deallocate (iai, iag, iamap, xcs, ycs)
c
      return
      end
