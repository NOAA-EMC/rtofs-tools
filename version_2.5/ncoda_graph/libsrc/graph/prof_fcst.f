      subroutine prof_fcst (opt, n_obs, n_fcst, n_lvl, n_prf, prf_age,
     *                      prf_lat, prf_lon, prf_lvl, prf_nd, prf_sgn,
     *                      prf_typ, prf_val, prf_var, prf_vfy, zoom,
     *                      sys_typ, fno)

c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  prof_fcst
c
c DESCRIPTION:  plot profile forecast verification
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
      integer   n_fcst
      integer   n_lvl
      integer   n_obs
c
      real      amn, amx
      integer   color (8)
      character date * 13
      integer   day
      real      dl, dt, dz
      character dtg * 10
      character ew * 1
      integer   fno
      integer   hr
      integer   i, j, k
      character label (8) * 9
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
      real      prf_lat (n_obs)
      real      prf_lon (n_obs)
      real      prf_lvl (n_lvl, n_obs)
      integer   prf_nd  (n_obs)
      character prf_sgn (n_obs) * 7
      integer   prf_typ (n_lvl, n_obs)
      real      prf_val (n_lvl, n_obs)
      integer   prf_var (n_obs)
      real      prf_vfy (n_lvl, n_obs, n_fcst)
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
      real,     allocatable :: lvl (:)
      real,     allocatable :: val (:)
      real,     allocatable :: vfy (:,:)
      real,     allocatable :: vfy_anm (:,:)
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
     *   0.000, 0.000, 1.000,
     *   1.000, 0.000, 0.000,
     *   0.000, 1.000, 0.000,
     *   1.000, 1.000, 0.000,
     *   1.000, 0.000, 1.000,
     *   0.000, 1.000, 1.000,
     *   1.000, 0.500, 0.000,
     *   1.000, 0.000, 0.500,
     *   0.500, 1.000, 0.500 /
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
c     ..set variable type
c
      if (opt .eq. 'TMP') then
         var = 1
      else if (opt .eq. 'SAL') then
         var = 2
      else
         return
      endif
c
c     ..allocate profile arrays
c
      allocate (lvl (n_lvl))
      allocate (val (n_lvl))
      allocate (vfy (n_lvl, n_fcst))
      allocate (vfy_anm (n_lvl, n_fcst))
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
      do j = 1, 8
         color(j) = j+3
         write (label(j),'(i3)') j * 24
      enddo
c
c     ..loop over profiles
c
      do i = 1, n_prf
         if (prf_nd(i) .lt. 5)  cycle        
         if (prf_typ(1,i) .eq. sys_typ) then
         if (prf_var(i) .eq. var) then
c
c        ..initialize plot variables
c
         nd = prf_nd(i)
         do k = 1, nd
            lvl(k) = prf_lvl(k,i)
            val(k) = prf_val(k,i)
c           do j = 1, n_fcst
            do j = 1, 5
               vfy(k,j) = prf_vfy(k,i,j)
            enddo
         enddo
c
c        ..ensure surface values
c
         if (val(1) .lt. -99.) val(1) = val(2)
c        do j = 1, n_fcst
         do j = 1, 5
            if (vfy(2,j) .lt. -99.) vfy(2,j) = vfy(3,j)
            if (vfy(1,j) .lt. -99.) vfy(1,j) = vfy(2,j)
         enddo
c
c        ..smooth profiles
c
c        do j = 1, n_fcst
         do j = 1, 5
            do k = 2, nd-1
               vfy(k,j) = 0.25 * vfy((k-1),j) + 0.5 * vfy(k,j) +
     *                    0.25 * vfy((k+1),j)
            enddo
         enddo
c
c        ..compute forecast anomalies
c
c        do j = 1, n_fcst
         do j = 1, 5
             do k = 1, nd
               if (val(k).gt.-99. .and. vfy(k,j).gt.-99.) then
                  vfy_anm(k,j) = val(k) - vfy(k,j)
               else
                  vfy_anm(k,j) = -999.
               endif
            enddo
         enddo
c
c        ..form date
c
         call time_dtg (prf_age(i), dtg)
         read (dtg(3:4),  '(i2)') year
         read (dtg(5:6),  '(i2)') mon
         read (dtg(7:8),  '(i2)') day
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
         call gsplci (3)
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
            if (val(k) .gt. -99.) then
               if (val(k) .lt. vmn) vmn = val(k)
               if (val(k) .gt. vmx) vmx = val(k)
            endif
c           do j = 1, n_fcst
            do j = 1, 5
               if (vfy(k,j) .gt. -99.) then
                  if (vfy(k,j) .lt. vmn) vmn = vfy(k,j)
                  if (vfy(k,j) .gt. vmx) vmx = vfy(k,j)
               endif
            enddo
c
c           do j = 1, n_fcst
            do j = 1, 5
               if (vfy_anm(k,j) .gt. -99.) then
                  if (vfy_anm(k,j) .lt. amn) amn = vfy_anm(k,j)
                  if (vfy_anm(k,j) .gt. amx) amx = vfy_anm(k,j)
               endif
            enddo
         enddo
c
c        ..expand ranges slightly, form nearest integers
c
         vmn = real (nint (vmn)) - 1.
         vmx = real (nint (vmx)) + 1.
         amn = real (nint (amn - 0.5))
         amx = real (nint (amx + 0.5))
c
c-----------------------------------------------------------------------
c
c        ..call set with min/max of variable anomalies and depth
c
         call set (0.48, 0.98, 0.15, 0.85, amn, amx, lvl(nd), 0., 1)
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
c        ..label depth and variable axes
c
         size = .025
         off = 0.09
         if (lvl(nd) .le. 50.) then
            dz = 5.
         else if (lvl(nd) .le. 100.) then
            dz = 10.
         else if (lvl(nd) .le. 300.) then
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
         dt = dl * 0.7
         if (var .eq. 1) then
            call lbl_axis (amn, amx, dl, dt, 'Temperature Anomaly (C)',
     *                     0., size, .05, .false.)
         else if (var .eq. 2) then
            call lbl_axis (amn, amx, dl, dt, 'Salinity Anomaly (PSU)',
     *                     0., size, .05, .false.)
         endif
c
         call setusv ('LW', 3000)
c
c        ..plot forecast anomaly profiles
c
c        do j = 1, n_fcst
         do j = 1, 5
            call gsplci (j+3)
            write (label(j),'(i3)') j * 24
            call frstpt (vfy_anm(1,j), lvl(1))
            do k = 1, nd
               if (vfy(k,j) .gt. -99.) then
                  call vector (vfy_anm(k,j), lvl(k))
               endif
            enddo
            call plotit (0, 0, 0)
         enddo
c
         call setusv ('LW', 1000)
c
c        ..draw line along zero anomaly
c
         call gsplci (1)
         if (amn .lt. 0. .and. amx .gt. 0.) then
            call frstpt (0., 0.)
            call vector (0., lvl(nd))
         endif
c
c        ..put label bar below plot
c
         call gsplci (1)
         call gsfais (1)
         call lbseti ('CBL - color boxlines', 1)
         call lbseti ('CLB - color labels', 1)
         xl = .48
         xr = .98
         call lblbar (0, xl, xr, .0, .075, 8, 1., .25, 
     *                color, 0, label, 8, 1)
         call plotit (0, 0, 0)
         call gsplci (1)
         call gsfaci (1)
c
c        ..create and plot title
c
         size = .03
         write (title, '(a)') adjustl (data_lbl(prf_typ(1,i))) 
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
     *          prf_sgn(i), abs (lat), abs (lon)
         write (*, '(10x, ''frame'', i5, '': '', a, 2x, a)')
     *          fno, trim (title), date
c
c-----------------------------------------------------------------------
c
c        ..call set with min/max of anomalies and depth
c
         call set (0.07, 0.40, 0.10, 0.48, vmn, vmx, lvl(nd), 0., 1)
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
         size = .035
         off = 0.05
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
     *                     0., size, .04, .false.)
         else if (var .eq. 2) then
            if ((vmx - vmn) .le. 4.) then
               dl = 0.5
            else if ((vmx - vmn) .le. 8.) then
               dl = 1.
            else if ((vmx - vmn) .le. 16.) then
               dl = 2. 
            else
               dl = 4.
            endif
            dt = dl * 0.5
            call lbl_axis (vmn, vmx, dl, dt, 'Salinity (PSU)',
     *                     0., size, .04, .false.)
         endif
         call lbl_axis (0., lvl(nd), dz, dz, ' ', 90., size,
     *                  off, .true.)
c
c        ..plot profiles
c
         call setusv ('LW', 3000)
c        do j = 1, n_fcst
         do j = 1, 5
            call gsplci (j+3)
            call frstpt (vfy(1,j), lvl(1))
            do k = 1, nd
               if (vfy(k,j) .gt. -99.) then
                  call vector (vfy(k,j), lvl(k))
               endif
            enddo
            call plotit (0, 0, 0)
         enddo
c
c        ..plot observed profile
c
         call gsplci (3)
         call frstpt (val(1), lvl(1))
         do k = 1, nd
            if (val(k) .gt. -99.) then
               call vector (val(k), lvl(k))
            endif
         enddo
         call plotit (0, 0, 0)
         call setusv ('LW', 1000)
c
c        ..put title on anomaly plot
c
         size = .035
         call title_plot ('Forecast Profiles', size, 1, 1.)
c
c        ..advance the plot buffer
c
         call frame
         endif
         endif
      enddo
c
c     ..clean up
c
      deallocate (lvl, val, vfy, vfy_anm) 
      deallocate (iai, iag, iamap, xcs, ycs)
c
      return
      end
