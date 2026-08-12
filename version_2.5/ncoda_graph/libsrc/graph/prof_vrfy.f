      subroutine prof_vrfy (opt, n_obs, n_lvl, n_prf, prf_age, prf_anl,
     *                      prf_bkg, prf_clm, prf_lat, prf_lon, prf_lvl,
     *                      prf_nd, prf_sgn, prf_typ, prf_val, zoom,
     *                      sys_typ, var, fno)

c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  prof_vrfy
c
c DESCRIPTION:  plot profile verification
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libocnqc.a
c      
c PARAMETERS:
c     Name         Type       Usage            Description
c   ---------    --------    -------    ----------------------------
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
      include 'coda_types.h'
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
      integer   n_lvl
      integer   n_obs
c
      real      amn, amx
      integer   color (5)
      character date * 13
      integer   day
      real      dl, dt, dz
      character dtg * 10
      character ew * 1
      integer   fno
      integer   hr
      integer   i, j, k
      character label (5) * 8
      real      lat, lat1, lat2
      real      lon, lon1, lon2
      integer   mon
      character month (12) * 3
      integer   n_prf
      integer   nd
      character ns * 1
      real      off
      character opt * 3
      real      polon
      real      prf_age (n_obs)
      real      prf_anl (n_lvl, n_obs)
      real      prf_bkg (n_lvl, n_obs)
      real      prf_clm (n_lvl, n_obs)
      real      prf_lat (n_obs)
      real      prf_lon (n_obs)
      real      prf_lvl (n_lvl, n_obs)
      integer   prf_nd  (n_obs)
      character prf_sgn (n_obs) * 7
      integer   prf_typ (n_lvl, n_obs)
      real      prf_val (n_lvl, n_obs)
      real      rgb (3, 0:MX_CLR)
      real      size
      integer   sys_typ
      character title * 80
      real      u, v
      integer   var
      real      vmn, vmx
      real      xl, xr
      real      xvpl, xvpr, yvpb, yvpt
      integer   year
      real      zoom (4)
c
c     ..allocatable profile arrays
c
      real,     allocatable :: anl (:)
      real,     allocatable :: anl_anm (:)
      real,     allocatable :: bkg (:)
      real,     allocatable :: bkg_anm (:)
      real,     allocatable :: clm (:)
      real,     allocatable :: clm_anm (:)
      real,     allocatable :: lvl (:)
      real,     allocatable :: val (:)
c
c     ..allocatable plot arrays
c
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycs (:)
c
c     ..external NCAR functions
c
      external  clin, cmap
c
c     ..set color table
c
      data ((rgb(i,j), i = 1, 3), j = 0, MX_CLR) /
     *   1.000, 1.000, 1.000,
     *   0.000, 0.000, 0.000,
     *   0.860, 0.580, 0.440,
     *   1.000, 0.000, 0.000,
     *   0.000, 1.000, 0.000,
     *   0.000, 0.000, 1.000,
     *   1.000, 1.000, 0.000,
     *   1.000, 0.000, 1.000,
     *   0.000, 1.000, 1.000,
     *   1.000, 0.500, 0.000,
     *   1.000, 0.000, 0.500,
     *   0.950, 0.950, 0.950 /
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
c     ..allocate profile arrays
c
      allocate (anl (n_lvl))
      allocate (anl_anm (n_lvl))
      allocate (bkg (n_lvl))
      allocate (bkg_anm (n_lvl))
      allocate (clm (n_lvl))
      allocate (clm_anm (n_lvl))
      allocate (lvl (n_lvl))
      allocate (val (n_lvl))
c
c     ..allocate plot arrays
c
      allocate (iai (MX_AI))
      allocate (iag (MX_AI))
      allocate (iamap (MX_AMAP))
      allocate (xcs (MX_LIN))
      allocate (ycs (MX_LIN))
c
c     ..set color scheme
c
      call gks_color (rgb, MX_CLR)
      call setusv ('LW', 2000)
c
c     ..loop over profiles
c
      do i = 1, n_prf
c        if (prf_nd(i) .lt. 5)  cycle
         if (prf_typ(1,i) .eq. sys_typ) then
c
c        ..initialize plot variables
c
         nd = 0
         do k = 1, prf_nd(i)
            if (prf_typ(k,i) .eq. 50) cycle
            if (prf_typ(k,i) .eq. 51) cycle
            nd = nd + 1
            anl(nd) = prf_anl(k,i)
            bkg(nd) = prf_bkg(k,i)
            clm(nd) = prf_clm(k,i)
            lvl(nd) = prf_lvl(k,i)
            val(nd) = prf_val(k,i)
         enddo
c
c        ..compute anomalies
c
         do k = 1, nd
            anl_anm(k) = val(k) - anl(k)
            bkg_anm(k) = val(k) - bkg(k)
            clm_anm(k) = val(k) - clm(k)
c****************
c     clm_anm(k) = -999.
c********************
         enddo
c
c        ..form date
c
         call time_dtg (prf_age(i), dtg)
         read (dtg(3:4) , '(i2)') year
         read (dtg(5:6) , '(i2)') mon
         read (dtg(7:8) , '(i2)') day
         read (dtg(9:10), '(i2)') hr
         write (date, '(i2.2, 1x, a, 1x, i2.2, 1x, i2.2, ''Z'')')
     *         day, month(mon), year, hr
c
c-----------------------------------------------------------------------
c
c        ..set profile position
c
         lat = prf_lat(i)
         lon = prf_lon(i)
         if (lon .gt. 180.) lon = lon - 360
c
c        ..check zoom settings
c
         if (zoom(1) .gt. -990.) then
            if (lat .lt. zoom(1)) cycle
            if (lat .gt. zoom(2)) cycle
            if (lon .lt. zoom(3)) cycle
            if (lon .gt. zoom(4)) cycle
         endif
c
c        ..set hemispheric indicators
c
         if (lat .ge. 0.) then
            ns = 'N'
         else
            ns = 'S'
         endif
         if (lon .ge. 0.) then
            ew = 'E'
         else
            ew = 'W'
         endif
c
c        ..set latitude/longitude range
c
         lat1 = lat - 15.
         lat2 = lat + 15.
         lon1 = lon - 15.
         lon2 = lon + 15.
         polon = (lon1 + lon2) * 0.5
c
c        ..set up map background
c
         call arinam (iamap, MX_AMAP)
         call mapsti ('LA', 0)
         call mappos (.02, .40, .52, .98)
         call mapset ('CO', lat1, lon1, lat2, lon2)
         call maproj ('CE', 0., polon, 0.)
         call mapstr ('GR', 10.)
         call mplnam ('Earth..3', 1, iamap)
         call gsfais (1)
         call gsfaci (2)
         call arscam (iamap, xcs, ycs, MX_LIN, iai, iag,
     *                MX_AI, cmap)
         call gsplci (1)
         call gstxci (1)
         call gsln (2)
         call mapgrm (iamap, xcs, ycs, MX_LIN, iai, iag,
     *                MX_AI, clin)
         call gsln (1)
         call mplndr ('Earth..3', 1)
         call maplbl
c
c        ..mark observation position
c
         call pcseti ('FN - fontcap number', 20)
         call gsplci (5)
         call maptrn (lat, lon, u, v)
         call plchhq (u, v, 'L', .017, 0., 0.)
         call pcseti ('FN - fontcap number', 0)
         call plotit (0, 0, 0)
         call gsplci (1)
         call gsfaci (1)
c
c-----------------------------------------------------------------------
c
c        ..set variable / anomaly / depth limits
c
         amn = 1.e8
         amx = -1.e8
         vmn = 1.e8
         vmx = -1.e8
         do k = 1, nd
            if (anl(k) .gt. -99.) then
               if (anl(k) .lt. vmn) vmn = anl(k)
               if (anl(k) .gt. vmx) vmx = anl(k)
            endif
            if (bkg(k) .gt. -99.) then
               if (bkg(k) .lt. vmn) vmn = bkg(k)
               if (bkg(k) .gt. vmx) vmx = bkg(k)
            endif
            if (clm(k) .gt. -99.) then
               if (clm(k) .lt. vmn) vmn = clm(k)
               if (clm(k) .gt. vmx) vmx = clm(k)
            endif
            if (val(k) .gt. -99.) then
               if (val(k) .lt. vmn) vmn = val(k)
               if (val(k) .gt. vmx) vmx = val(k)
            endif
c
            if (anl_anm(k) .gt. -99.) then
               if (anl_anm(k) .lt. amn) amn = anl_anm(k)
               if (anl_anm(k) .gt. amx) amx = anl_anm(k)
            endif
            if (bkg_anm(k) .gt. -99.) then
               if (bkg_anm(k) .lt. amn) amn = bkg_anm(k)
               if (bkg_anm(k) .gt. amx) amx = bkg_anm(k)
            endif
            if (clm_anm(k) .gt. -99.) then
               if (clm_anm(k) .lt. amn) amn = clm_anm(k)
               if (clm_anm(k) .gt. amx) amx = clm_anm(k)
            endif
         enddo
c
c        ..expand ranges slightly, form nearest integers
c
         vmn = real (nint (vmn)) - 0.5
         vmx = real (nint (vmx)) + 0.5
         amn = real (nint (amn)) - 0.5
         amx = real (nint (amx)) + 0.5
c
c-----------------------------------------------------------------------
c
c        ..call set with min/max of variable and depth
c
         call set (0.48, 0.98, 0.15, 0.85, vmn, vmx, lvl(nd), 0., 1)
         call gsplci (1)
         call gsfaci (1)
c
c        ..draw solid lines around chart
c
         xvpl = vmn
         xvpr = vmx
         yvpb = lvl(nd)
         yvpt = 0.
         call line (xvpl, yvpb, xvpr, yvpb)
         call line (xvpl, yvpb, xvpl, yvpt)
         call line (xvpl, yvpt, xvpr, yvpt)
         call line (xvpr, yvpb, xvpr, yvpt)
c
c        ..label depth and variable axes
c
         size = .025
         off = 0.09
         if (lvl(nd) .le. 50.) then
            dz = 5.
         else if (lvl(nd) .le. 100.) then
            dz = 10.
         else if (lvl(nd) .le. 400.) then
            dz = 50.
         else if (lvl(nd) .le. 1000.) then
            dz = 100.
         else if (lvl(nd) .le. 2000.) then
            dz = 200.
         else
            dz = 500.
         endif
         call lbl_axis (0., lvl(nd), dz, dz, ' ', 90., size,
     *                  off, .true.)
         if (var .eq. 1) then
            if ((vmx - vmn) .le. 12.) then
               dl = 1.
            else if ((vmx - vmn) .le. 24.) then
               dl = 2.
            else if ((vmx - vmn) .le. 48.) then
               dl = 4. 
            else
               dl = 8.
            endif
            dt = dl * 0.5
            call lbl_axis (vmn, vmx, dl, dt, 'Temperature (C)',
     *                     0., size, .042, .false.)
         else if (var .eq. 2) then
            if ((vmx - vmn) .le. 2.) then
               dl = 0.5
            else if ((vmx - vmn) .le. 4.) then
               dl = 1.
            else if ((vmx - vmn) .le. 8.) then
               dl = 2. 
            else
               dl = 4.
            endif
            dt = dl * 0.5
            call lbl_axis (vmn, vmx, dl, dt, 'Salinity (PSU)',
     *                     0., size, .042, .false.)
         endif
c
         call setusv ('LW', 3000)
c
c        ..plot profiles
c
         call gsplci (7)
         color(1) = 7
         label(1) = 'Climate '
         call frstpt (clm(1), lvl(1))
         do k = 1, nd
            if (clm(k) .gt. -99.) then
               call vector (clm(k), lvl(k))
            endif
         enddo
         call plotit (0, 0, 0)
c
         call gsplci (3)
         color(2) = 3
         label(2) = 'Model   '
         call frstpt (bkg(1), lvl(1))
         do k = 1, nd
            if (bkg(k) .gt. -99.) then
               call vector (bkg(k), lvl(k))
            endif
         enddo
         call plotit (0, 0, 0)
c
         call gsplci (4)
         color(3) = 4
         label(3) = 'Analysis'
         call frstpt (anl(1), lvl(1))
         do k = 1, nd
            if (anl(k) .gt. -99.) then
               call vector (anl(k), lvl(k))
            endif
         enddo
         call plotit (0, 0, 0)
c
         call gsplci (5)
         color(4) = 5
         label(4) = 'Obs     '
         call frstpt (val(1), lvl(1))
         do k = 1, nd
            if (val(k) .gt. -99.) then
               call vector (val(k), lvl(k))
            endif
         enddo
         call plotit (0, 0, 0)
c
         call setusv ('LW', 2000)
c
c        ..put label bar below plot
c
         call gsplci (1)
         call gsfais (1)
         call lbseti ('CBL - color boxlines', 1)
         call lbseti ('CLB - color labels', 1)
         xl = .55
         xr = .91
         call lblbar (0, xl, xr, .0, .075, 4, 1., .25, 
     *                color, 0, label, 4, 1)
         call plotit (0, 0, 0)
         call gsplci (1)
         call gsfaci (1)
c
c        ..create and plot title
c
         size = .03
         write (title, '(a)') trim (adjustl (data_lbl(prf_typ(1,i)))) 
         call title_plot (title, size, 1, 3.5)
         write (title, '(''Call Sign '', a, 5x, a)')
     *          prf_sgn(i), date
         call title_plot (title, size, 1, 2.15)
         write (title, '(f5.1, a, 5x, f6.1, a)')
     *          abs (lat), ns, abs (lon), ew
         call title_plot (title, size, 1, 1.)
c
         fno = fno + 1
         write (title, '(a, 3x, a, 3x, f5.1, 1x, f6.1)')
     *          trim (adjustl (data_lbl(prf_typ(1,i)))),
     *          prf_sgn(i), lat, lon
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title)
c
c-----------------------------------------------------------------------
c
c        ..call set with min/max of anomalies and depth
c
         call set (0.07, 0.40, 0.10, 0.48, amn, amx, lvl(nd), 0., 1)
         call gsplci (1)
         call gsfaci (1)
c
c        ..draw solid lines around chart
c
         xvpl = amn
         xvpr = amx
         yvpb = lvl(nd)
         yvpt = 0.
         call line (xvpl, yvpb, xvpr, yvpb)
         call line (xvpl, yvpb, xvpl, yvpt)
         call line (xvpl, yvpt, xvpr, yvpt)
         call line (xvpr, yvpb, xvpr, yvpt)
c
c        ..label depth and variable anomaly axes
c
         size = .03
         if ((amx - amn) .le. 3.) then
            dl = 0.5
         else if ((amx - amn) .le. 9.) then
            dl = 1.
         else if ((amx - amn) .le. 16.) then
            dl = 2.
         else
            dl = 4.
         endif
         dt = dl * 0.5
         call lbl_axis (0., lvl(nd), dz, dz, ' ', 90., size,
     *                  off, .true.)
         if (var .eq. 1) then
            call lbl_axis (amn, amx, dl, dt, 'Anomaly (C)',
     *                     0., size, .04, .false.)
         else if (var .eq. 2) then
            call lbl_axis (amn, amx, dl, dt, 'Anomaly (PSU)',
     *                     0., size, .04, .false.)
         endif
c
c        ..plot anomaly profiles
c
         call setusv ('LW', 3000)
c
         call gsplci (7)
         call frstpt (clm_anm(1), lvl(1))
         do k = 1, nd
            if (clm_anm(k) .gt. -99.) then
               call vector (clm_anm(k), lvl(k))
            endif
         enddo
         call plotit (0, 0, 0)
c
         call gsplci (4)
         call frstpt (anl_anm(1), lvl(1))
         do k = 1, nd
            if (anl_anm(k) .gt. -99.) then
               call vector (anl_anm(k), lvl(k))
            endif
         enddo
         call plotit (0, 0, 0)
c
         call gsplci (3)
         call frstpt (bkg_anm(1), lvl(1))
         do k = 1, nd
            if (bkg_anm(k) .gt. -99.) then
               call vector (bkg_anm(k), lvl(k))
            endif
         enddo
         call plotit (0, 0, 0)
c
c        ..draw line along zero anomaly
c
         call setusv ('LW', 2000)
         call gsplci (1)
         if (amn .lt. 0. .and. amx .gt. 0.) then
            call frstpt (0., 0.)
            call vector (0., lvl(nd))
         endif
c
c        ..put title on anomaly plot
c
         size = .035
         call title_plot ('Observation Anomaly', size, 1, 1.)
c
c        ..advance the plot buffer
c
         call frame
c
         endif
      enddo
c
c     ..clean up
c
      deallocate (anl, anl_anm, bkg, bkg_anm, clm, clm_anm)
      deallocate (lvl, val) 
      deallocate (iai, iag, iamap, xcs, ycs)
c
      return
      end
