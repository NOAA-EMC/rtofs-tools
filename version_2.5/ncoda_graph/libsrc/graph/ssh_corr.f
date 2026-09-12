      subroutine ssh_corr (n_lon, n_lat, ssh, corr, xpt, ypt)
c
      implicit  none
c
c     ..local array dimensions
c
      integer   n_lon, n_lat
c
      real       cdr
      parameter (cdr = .0174532926)
c
      real       degkm
      parameter (degkm = 111.1200826)
c
      real      cc
      real      d, dh
      real      delx, dely
      real      dst
      character dtg * 10
      logical   fail
      real      fc, fn
      real      hx, hy
      integer   i, k
      integer   iref, jref
      integer   ix, jy
      integer   nest
      integer   node
      integer   n_proj
      integer   n_pts
      integer   n_sfc
      real      ob_hsc (2)
      real      ob_ssh (2)
      character path * 256
      real      rlat, rlon
      real      s
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      real      x, y
      real      xi (2), yj (2) 
      real      xpt (2), ypt (2)
c
      real      corr (n_lon * n_lat)
      real      ssh (n_lon * n_lat)
c
c     ..allocatable arrays
c
      real,     allocatable :: hsc (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (hsc (n_lon * n_lat))
      allocate (lat (n_lon * n_lat))
      allocate (lon (n_lon * n_lat))
c
c     ..read arrays
c
      dtg = '2022051500'
      nest = 1
      n_sfc = 1
      path = '/scratch2/NCEPDEV/marine/Jim.Cummings/rtofs_da/' //
     *       'ncoda_glbl/restart/'
      tau = 0
      write (*,'(''path: '', a)') trim (path)
c
      call rd_coda_file (path, dtg, nest, n_lon, n_lat, n_sfc,
     *                   'datafld', 'sclhor', 'o', tau, 'sfc',
     *                   hsc, .true., fail)
      call rd_coda_file (path, dtg, nest, n_lon, n_lat, n_sfc,
     *                   'datafld', 'grdlat', 'o', tau, 'sfc',
     *                   lat, .true., fail)
      call rd_coda_file (path, dtg, nest, n_lon, n_lat, n_sfc,
     *                   'datafld', 'grdlon', 'o', tau, 'sfc',
     *                   lon, .true., fail)
c
c     ..set obs point lon, lat and obs ssh
c 
      dh = 1. / 0.15
      xpt(1) = 147.5
      ypt(1) = 36.
      xpt(2) = 157.5
      ypt(2) = 35.
c
      n_pts = 2
      n_proj = 1
      rlat = 0.
      rlon = 180.
      iref = 1621 
      jref = 1221
      stdlt1 = 0.
      stdlt2 = 0.
      stdlon = 180.
      delx = 12355.43554688
      dely = 12355.43554688

      call ll2ij (n_proj, rlat, rlon, iref, jref, stdlt1,
     *            stdlt2, stdlon, delx, dely, ypt, xpt,
     *            n_pts, xi, yj)
c
      hsc = hsc * 1.5
c
      do k = 1, 2
         ix = nint (xi(k))
         jy = nint (yj(k))
         node = n_lon * (jy-1) + ix
         ob_hsc(k) = hsc (node)
         ob_ssh(k) = ssh (node)
         ob_hsc(k) = 1. / ob_hsc(k)
      enddo
c
      hsc = 1. / hsc
c
c     ..initialize
c
      corr = 0.
c
c     ..loop over grid
c
      do i = 1, (n_lon * n_lat)
      if (hsc(i) .gt. 0. .and. ssh(i) .gt. -990.) then
c
c        ..compute distance scaled by length scale
c
         do k = 1, 2
            y = (lat(i) + ypt(k)) * 0.5
            x = abs (lon(i) - xpt(k))
            hx = degkm * x * cos (cdr * y)
            hy = degkm * abs (lat(i) - ypt(k))
            s = sqrt (hx*hx + hy*hy) * sqrt (ob_hsc(k) * hsc(i))
c
c           ..compute SOAR correlation
c
            fn = (1. + s) * exp (-s)
c
c           ..compute height separation
c
            d = abs (ob_ssh(k) - ssh(i)) * dh
            fc = (1. + d) * exp (-d)
c
c           ..set correaltion
c
            corr(i) = corr(i) + fn * fc
            if (corr(i) .lt. 0.01) corr(i) = 0.
         enddo
      endif
      enddo
c
c     ..clean up
c
      deallocate (hsc, lat, lon)
c
      return
      end
