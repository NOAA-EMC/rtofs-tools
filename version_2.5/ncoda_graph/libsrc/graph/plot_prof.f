      subroutine plot_prof (n_obs, n_lvl, n_prf, dtg, lat, lon, lvl,
     *                      nd, typ, anl, bkg, mdl, prf, sgn, sys,
     *                      fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_prof
c
c DESCRIPTION:  plot various versions of an observed profile
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
      integer    NZ
      parameter (NZ = 41)
c
c     ..local array dimensions
c
      integer   n_lvl
      integer   n_obs
c
      real      amn, amx
      real      anm_anl (n_lvl)
      real      anm_bkg (n_lvl)
      real      anm_mdl (n_lvl)
      integer   color (5)
      character dtg * 10
      real      dl, dt, dz
      character ew * 1
      integer   fno
      integer   i, j, k
      character label (5) * 9
      real      lat1, lat2
      real      lon1, lon2
      integer   n_prf
      character ns * 1
      real      off
      real      polon
      real      rgb (3, 0:MX_CLR)
      real      size
      real      spval
      integer   sys
      character title * 80
      real      u, v
      real      val_anl (n_lvl)
      real      val_bkg (n_lvl)
      real      val_lvl (n_lvl)
      real      val_mdl (n_lvl)
      real      val_prf (n_lvl)
      real      vmn, vmx
      real      xl, xr
      real      xvpl, xvpr, yvpb, yvpt
      integer   zmn, zmx
      integer   zs
c
      real      x (NZ)
      real      z (NZ)
c
c     ..profile arrays
c
      real      anl (n_lvl, n_obs)
      real      bkg (n_lvl, n_obs)
      real      lat (n_obs)
      real      lon (n_obs)
      real      lvl (n_lvl, n_obs)
      integer   nd  (n_obs)
      real      mdl (n_lvl, n_obs)
      real      prf (n_lvl, n_obs)
      character sgn (n_obs) * 7
      integer   typ (n_lvl, n_obs)
c
c     ..allocatable arrays
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
c     ..interpolation levels
c
      data      z  / 0.,    5.,    10.,   16.,
     *              28.,   44.,   60.,   76.,   92., 
     *             108.,  124.,  140.,  160.,  180.,
     *             200.,  220.,  240.,  260.,  280.,
     *             300.,  340.,  380.,  420.,  460.,
     *             500.,  560.,  620.,  680.,  740.,
     *             800.,  850., 900., 1000., 1150., 1300.,
     *            1450., 1600., 1750., 2000., 2250.,
     *            2500. /
c
c...............................executable..............................
c
c     ..allocate arrays
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
c
c     ..loop over profiles
c
      do i = 1, n_prf
      if (typ(1,i) .eq. sys) then
c
c     ..initialize plot variables
c
      do k = 1, n_lvl
         anm_anl(k) = spval
         anm_bkg(k) = spval
         anm_mdl(k) = spval
         val_anl(k) = spval
         val_bkg(k) = spval
         val_lvl(k) = spval
         val_mdl(k) = spval
         val_prf(k) = spval
      enddo
c
c     ..fill plot arrays (ignore extended levels)
c
      do k = 1, nd(i)
         val_anl(k) = anl(k,i)
         val_bkg(k) = bkg(k,i)
         val_lvl(k) = lvl(k,i)
         val_mdl(k) = mdl(k,i)
         val_prf(k) = prf(k,i)
         anm_anl(k) = prf(k,i) - anl(k,i)
         anm_bkg(k) = prf(k,i) - bkg(k,i)
         anm_mdl(k) = prf(k,i) - mdl(k,i)
      enddo
c
c     ..set latitude/longitude range
c
      lat1 = lat(i) - 20.
      lat2 = lat(i) + 20.
      lon1 = lon(i) - 20.
      lon2 = lon(i) + 20.
      polon = (lon1 + lon2) * 0.5
c
c     ..set hemispheric indicators
c
      ns = 'N'
      if (lat(i) .lt. 0.) ns = 'S'
      ew = 'E'
      if (lon(i) .lt. 0.) ew = 'W'
c
c     ..set up map background
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
     *             MX_AI, cmap)
      call gsplci (1)
      call gstxci (1)
      call gsln (2)
      call mapgrm (iamap, xcs, ycs, MX_LIN, iai, iag,
     *             MX_AI, clin)
      call gsln (1)
      call mplndr ('Earth..3', 1)
      call maplbl
c
c     ..mark observation position
c
      call pcseti ('FN - fontcap number', 20)
      call gsplci (5)
      call maptrn (lat(i), lon(i), u, v)
      call plchhq (u, v, 'L', .017, 0., 0.)
      call pcseti ('FN - fontcap number', 0)
      call plotit (0, 0, 0)
      call gsplci (1)
      call gsfaci (1)
c
c     ..set variable / anomaly / depth limits
c
      amn = 1.e8
      amx = -1.e8
      vmn = 1.e8
      vmx = -1.e8
      zmn = 1
      zmx = 33 
      do k = 1, nd(i)
         if (val_anl(k) .gt. -99.) then
            if (val_anl(k) .lt. vmn) vmn = val_anl(k)
            if (val_anl(k) .gt. vmx) vmx = val_anl(k)
         endif
         if (val_bkg(k) .gt. -99.) then
            if (val_bkg(k) .lt. vmn) vmn = val_bkg(k)
            if (val_bkg(k) .gt. vmx) vmx = val_bkg(k)
         endif
         if (val_mdl(k) .gt. -99.) then
            if (val_mdl(k) .lt. vmn) vmn = val_mdl(k)
            if (val_mdl(k) .gt. vmx) vmx = val_mdl(k)
         endif
         if (val_prf(k) .gt. -99.) then
            if (val_prf(k) .lt. vmn) vmn = val_prf(k)
            if (val_prf(k) .gt. vmx) vmx = val_prf(k)
         endif
c
         if (anm_anl(k) .gt. -99.) then
            if (anm_anl(k) .lt. amn) amn = anm_anl(k)
            if (anm_anl(k) .gt. amx) amx = anm_anl(k)
         endif
         if (anm_bkg(k) .gt. -99.) then
            if (anm_bkg(k) .lt. amn) amn = anm_bkg(k)
            if (anm_bkg(k) .gt. amx) amx = anm_bkg(k)
         endif
         if (anm_mdl(k) .gt. -99.) then
            if (anm_mdl(k) .lt. amn) amn = anm_mdl(k)
            if (anm_mdl(k) .gt. amx) amx = anm_mdl(k)
         endif
      enddo
c
c     ..expand ranges slightly, form nearest integers
c
      vmn = real (nint (vmn)) - 1.
      vmx = real (nint (vmx)) + 1.
      amn = real (nint (amn - 0.5))
      amx = real (nint (amx + 0.5))
c
c-----------------------------------------------------------------------
c
c     ..call set with min/max of variable and depth
c
      call set (0.48, 0.98, 0.15, 0.85, vmn, vmx, z(zmx), 0., 1)
      call gsplci (1)
      call gsfaci (1)
c
c     ..draw solid lines around chart
c
      xvpl = vmn
      xvpr = vmx
      yvpb = z(zmx)
      yvpt = 0.
      call line (xvpl, yvpb, xvpr, yvpb)
      call line (xvpl, yvpb, xvpl, yvpt)
      call line (xvpl, yvpt, xvpr, yvpt)
      call line (xvpr, yvpb, xvpr, yvpt)
c
c     ..label depth and variable axes
c
      size = .025
      off = 0.02
      if (z(zmx) .le. 50.) then
         dz = 5.
      else if (z(zmx) .le. 100.) then
         dz = 10.
      else if (z(zmx) .le. 300.) then
         dz = 50.
      else if (z(zmx) .le. 1000.) then
         dz = 100.
      else if (z(zmx) .le. 2000.) then
         dz = 200.
      else
         dz = 500.
      endif
      call lbl_axis (0., z(zmx), dz, dz, ' ', 90.,
     *               size, off, .true.)
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
     *               0., size, 0.045, .false.)
c
      call setusv ('LW', 3000)
c
c     ..plot profiles
c
      call gsplci (4)
      color(4) = 4
      label(4) = 'Anal'
      call prof_trp (nd(i), val_lvl, val_anl, spval, NZ, z, x)
      if (x(zmn) .gt. -99.) then
         call frstpt (x(zmn), z(zmn))
         zs = 1
      else
         call frstpt (x(2), z(2))
         zs = 2
      endif
      do k = zs, zmx
         if (x(k) .gt. -99.) then
            call vector (x(k), z(k))
         endif
      enddo
      call plotit (0, 0, 0)
c
      call gsplci (7)
      color(2) = 7
      label(2) = 'Fcst'
      call prof_trp (nd(i), val_lvl, val_bkg, spval, NZ, z, x)
      if (x(zmn) .gt. -99.) then
         call frstpt (x(zmn), z(zmn))
         zs = 1
      else
         call frstpt (x(2), z(2))
         zs = 2
      endif
      do k = zs, zmx
         if (x(k) .gt. -99.) then
            call vector (x(k), z(k))
         endif
      enddo
      call plotit (0, 0, 0)
c
      call gsplci (8)
      color(3) = 8
      label(3) = 'Arch'
      call prof_trp (nd(i), val_lvl, val_mdl, spval, NZ, z, x)
      if (x(zmn) .gt. -99.) then
         call frstpt (x(zmn), z(zmn))
         zs = 1
      else
         call frstpt (x(2), z(2))
         zs = 2
      endif
      do k = zs, zmx
         if (x(k) .gt. -99.) then
            call vector (x(k), z(k))
         endif
      enddo
      call plotit (0, 0, 0)
c
      call gsplci (5)
      color(1) = 5
      label(1) = 'Prof'
      call prof_trp (nd(i), val_lvl, val_mdl, spval, NZ, z, x)
      if (x(zmn) .gt. -99.) then
         call frstpt (x(zmn), z(zmn))
         zs = 1
      else
         call frstpt (x(2), z(2))
         zs = 2
      endif
      do k = zs, zmx
         if (x(k) .gt. -99.) then
            call vector (x(k), z(k))
         endif
      enddo
      call plotit (0, 0, 0)
c
      call setusv ('LW', 1000)
c
c     ..put label bar below plot
c
      call gsplci (1)
      call gsfais (1)
      call lbseti ('CBL - color boxlines', 1)
      call lbseti ('CLB - color labels', 1)
      xl = .56
      xr = .90
      call lblbar (0, xl, xr, .0, .075, 4, 1., .25, color,
     *             0, label, 4, 1)
      call plotit (0, 0, 0)
      call gsplci (1)
      call gsfaci (1)
c
c     ..create and plot title
c
      size = .035
      write (title, '(a, 5x, a)')
     *       adjustl (data_lbl(typ(1,i))), sgn(i)
      call title_plot (title, size, 1, 3.2)
      size = 0.030
      write (title, '(''Fcst DTG - '', a)') dtg
      call title_plot (title, size, 1, 2.5)
      write (title, '(f5.2, a, 1x, f6.2, a)')
     *       abs(lat(i)), ns, abs(lon(i)), ew
      call title_plot (title, size, 1, 1.)
c
c-----------------------------------------------------------------------
c
c     ..call set with min/max of anomalies and depth
c
      call set (0.07, 0.40, 0.10, 0.48, amn, amx,
     *                z(zmx), 0., 1)
      call gsplci (1)
      call gsfaci (1)
c
c     ..draw solid lines around chart
c
      xvpl = amn
      xvpr = amx
      yvpb = z(zmx)
      yvpt = 0.
c     zmx = nd(i)
      call line (xvpl, yvpb, xvpr, yvpb)
      call line (xvpl, yvpb, xvpl, yvpt)
      call line (xvpl, yvpt, xvpr, yvpt)
      call line (xvpr, yvpb, xvpr, yvpt)
c
c     ..label depth and variable anomaly axes
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
      call lbl_axis (0., z(zmx), dz, dz, 'Depth', 90.,
     *               size, 0.063, .true.)
      call lbl_axis (amn, amx, dl, dt, 'Anomaly (C)', 0.,
     *               size, 0.04, .false.)
c
c     ..plot anomaly profiles
c
      call setusv ('LW', 3000)
c
      call gsplci (4)
      call prof_trp (nd(i), val_lvl, anm_anl, spval, NZ, z, x)
      if (x(zmn) .gt. -99.) then
         call frstpt (x(zmn), z(zmn))
         zs = 1
      else
         call frstpt (x(2), z(2))
         zs = 2
      endif
      do k = zs, zmx
         if (x(k) .gt. -99.) then
            call vector (x(k), z(k))
         endif
      enddo
      call plotit (0, 0, 0)
c
      call gsplci (7)
      call prof_trp (nd(i), val_lvl, anm_bkg, spval, NZ, z, x)
      if (x(zmn) .gt. -99.) then
         call frstpt (x(zmn), z(zmn))
         zs = 1
      else
         call frstpt (x(2), z(2))
         zs = 2
      endif
      do k = zs, zmx
         if (x(k) .gt. -99.) then
            call vector (x(k), z(k))
         endif
      enddo
      call plotit (0, 0, 0)
c
      call gsplci (8)
      call prof_trp (nd(i), val_lvl, anm_mdl, spval, NZ, z, x)
      if (x(zmn) .gt. -99.) then
         call frstpt (x(zmn), z(zmn))
         zs = 1
      else
         call frstpt (x(2), z(2))
         zs = 2
      endif
      do k = zs, zmx
         if (x(k) .gt. -99.) then
            call vector (x(k), z(k))
         endif
      enddo
      call plotit (0, 0, 0)
c
c     ..draw line along zero anomaly
c
      call setusv ('LW', 1000)
      call gsplci (1)
      if (amn .lt. 0. .and. amx .gt. 0.) then
         call frstpt (0., 0.)
         call vector (0., z(zmx))
      endif
c
c     ..put title on anomaly plot
c
      size = .035
      call title_plot ('Observation Anomaly', size, 1, 1.)
c
c     ..advance the plot buffer
c
      fno = fno + 1
      call frame
      endif
      enddo
c
c     ..clean up
c
      deallocate (iai, iag, iamap, xcs, ycs)
c
      return
      end
