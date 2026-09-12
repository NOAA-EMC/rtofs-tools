      subroutine coamps_grid (igrid, reflat, reflon, iref, jref, stdlt1,
     *                        stdlt2, stdlon, delx, dely, m, n, grdlat, 
     *                        grdlon, f, hx, hy, xpos, ypos)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  coamps_grid
c
c DESCRIPTION:  to give latitude, longitude, grid constant, coriolis 
c               force and map factors in x- and y-directions for 
c               'm x n' points within a fixed grid.  all latitudes 
c               in this routine start with -90.0 at the south pole 
c               and increase northward to +90.0 at the north pole.  
c               the longitudes start with 0.0 at the greenwich
c               meridian and increase to the east, so that 90.0 
c               refers to 90.0E, 180.0 is the international dateline, 
c               and 270.0 is 90.0W. 
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libsetup.a
c
c PARAMETERS:
c     Name        Type      Usage            Description
c   ---------   --------   -------   -------------------------------
c    delx       real        input    grid spacing in x-direction
c    dely       real        input    grid spacing in y-direction
c    f          real        output   coriolis force at point
c    grdlat     real        output   latitude of points
c    grdlon     real        output   longitude of points
c    hx         real        output   map factor in x-direction
c    hy         real        output   map factor in y-direction
c    igrid      integer     input    type of grid projection:
c                                    1. mercator projection
c                                    2. lambert conformal projection
c                                    3. polar stereographic projection
c                                    4. cartesian coordinates
c                                    5. spherical projection
c    iref       integer     input    i-coordinate of reference point
c    jref       integer     input    j-coordinate of reference point
c    m          integer     input    number of points in x-direction
c    n          integer     input    number of points in y-direction
c    reflat     real        input    latitude at reference point 
c    reflon     real        input    longitude at reference point
c    stdlt1     real        input    standard latitude of grid
c    stdlt2     real        input    second standard latitude of grid 
c                                    (required for lambert conformal)
c    stdlon     real        input    standard longitude of grid
c                                    (points to the north)
c    xpos       real        output   x-position in meters from line 
c                                    extending from pole through 
c                                    standard longitue (reflat)
c    ypos       real        output   y-position in meters from the pole
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
c     ..local array dimensions
c
      integer   m, n
c
      real      angle
      real      cn1, cn2, cn3, cn4, cn5, cn6
      real      con1, con2
      real      d2r
      real      deg
      character err_msg * 256
      integer   igrid
      integer   iref
      integer   jref
      real      delx
      real      dely
      real      f (m * n)
      real      flat
      real      gcon
      real      grdlat (m * n)
      real      grdlon (m * n)
      real      hx (m * n)
      real      hy (m * n)
      integer   i, j, k
      integer   ihem
      real      ogcon
      real      omega
      real      onedeg
      real      pi, pi2, pi4
      real      r2d
      real      radius
      real      reflat
      real      reflon
      real      rih
      real      rr
      real      stdlon
      real      stdlt1
      real      stdlt2
      real      x, xi, xih, xx
      real      y, yih, yj, yy
      real      xpos (m * n)
      real      ypos (m * n)
c
c     ..allocatable arrays
c
      real,     allocatable :: distx (:,:)
      real,     allocatable :: disty (:,:)
      real,     allocatable :: grdi (:,:)
      real,     allocatable :: grdj (:,:)
c
c...............................executable..............................
c
c     ..local constants
c
      flat = 0.
      pi = 4. * atan(1.)
      pi2 = pi / 2.
      pi4 = pi / 4.
      d2r = pi / 180.
      r2d = 180. / pi
      radius = 6371229.
      omega = 4. * pi / 86400.
      onedeg = radius * 2. * pi / 360.
      xi = real (iref)
      yj = real (jref) 
c
c     ..allocate arrays
c
      allocate (distx (m, n))
      allocate (disty (m, n))
      allocate (grdi (m, n))
      allocate (grdj (m, n))
c
c     ..set computation points
c
      do j = 1, n
         do i = 1, m
            grdi(i,j) = real (i)
            grdj(i,j) = real (j)
         enddo
      enddo
c
c     ..compute distances on grid
c
      do j = 1, n   
        do i = 1, m
          distx(i,j) = (grdi(i,j) - xi) * delx
          disty(i,j) = (grdj(i,j) - yj) * dely
         enddo
      enddo
c
      if (igrid .eq. 1) then
c
c        ..mercator projection
c
         deg = abs (stdlt1) * d2r
         con1 = cos (deg)
         con2 = radius * con1
         deg = reflat * 0.5 * d2r
         rih = con2 * alog (tan (pi4 + deg))
         do j = 1, n
            do i = 1, m
               k = m * (j-1) + i
               xpos(k) = 1.
               ypos(k) = 1.
               rr = rih + (grdj(i,j) - yj) * dely
               grdlat(k) = (2. * atan (exp (rr / con2)) - pi2) * r2d
               grdlon(k) = reflon + (grdi(i,j) - xi) * r2d *
     *                     delx / con2
               if (grdlon(k) .gt. 360.) then
                  grdlon(k) = grdlon(k) - 360.
               endif
               if (grdlon(k) .lt.   0.) then
                  grdlon(k) = grdlon(k) + 360.
               endif
               deg = grdlat(k) * d2r
               f(k) = omega * sin (deg)
               hx(k) = cos (deg) / con1
               hy(k) = hx(k)
            enddo
         enddo
      else if (igrid .eq. 2 .or. igrid .eq. 3) then
c
c        ..lambert conformal or polar stereographic
c
        if (igrid .eq. 2) then
           if (stdlt1 .eq. stdlt2) then
              gcon = sin (abs (stdlt1) * d2r)
           else
              gcon = (log (sin ((90. - abs (stdlt1)) * d2r))
     *               -log (sin ((90. - abs (stdlt2)) * d2r))) /
     *               (log (tan ((90. - abs (stdlt1)) * .5 * d2r))
     *               -log (tan ((90. - abs (stdlt2)) * .5 * d2r)))
           endif
        else
           gcon = 1.
        endif
        ogcon = 1. / gcon
        ihem = nint (abs (stdlt1) / stdlt1)
        deg = (90. - abs (stdlt1)) * d2r
        cn1 = sin (deg)
        cn2 = radius * cn1 * ogcon
        deg = deg * .5
        cn3 = tan (deg)
        deg = (90. -abs (reflat)) * .5 * d2r
        cn4 = tan (deg)
        rih = cn2 * (cn4 / cn3)**gcon
        deg = (reflon - stdlon) * d2r * gcon
        xih =  rih * sin (deg)
        yih = -rih * cos (deg) * real (ihem)
        do j = 1, n
           do i = 1, m
              k = m * (j-1) + i
              x = xih + distx(i,j)
              y = yih + disty(i,j)
              xpos(k) = x * real (ihem)
              ypos(k) = y * real (ihem)
              rr = sqrt (x*x + y*y)
              grdlat(k) = r2d * (pi2 - 2. * atan (cn3 * 
     *                    (rr / cn2)**ogcon)) * real (ihem)
              xx = x
              yy = -y * real (ihem)
              if (yy .eq. 0.) then
                 if (xx .le. 0.) then
                    angle = -90.
                 else if (xx .gt. 0.) then
                    angle = 90.
                 endif
              else
                 angle = atan2 (xx,yy) * r2d
              endif
              grdlon(k) = stdlon + angle * ogcon
              deg = grdlat(k) * d2r
              f(k) = omega * sin (deg)
              if (grdlon(k) .gt. 360.) then
                 grdlon(k) = grdlon(k) - 360.
              endif
              if (grdlon(k) .lt.   0.) then
                 grdlon(k) = grdlon(k) + 360.
              endif
              deg = (90. - grdlat(k) * real (ihem)) * d2r
              cn5 = sin (deg)
              deg = deg * .5
              cn6 = tan (deg)
              if (igrid .eq. 2) then
                 hx(k) = cn5 / cn1 * (cn6 / cn3)**(-gcon)
              else
                 hx(k) = (1. + sin (abs (grdlat(k)) * d2r)) /
     *                   (1. + sin (abs (stdlt1) * d2r))
              endif
              hy(k) = hx(k)
           enddo
        enddo
      else if (igrid .eq. 4) then
c
c        ..analytic grid
c
         cn2 = delx / onedeg
         do j = 1, n
            do i = 1, m
               k = m * (j-1) + i
               xpos(k) = 1.
               ypos(k) = 1.
               grdlat(k) = (grdj(i,j) - yj) * dely / onedeg + reflat
               grdlon(k) = (grdi(i,j) - xi) * delx / onedeg + reflon
               if (grdlon(k) .gt. 360.) then
                  grdlon(k) = grdlon(k) - 360.
               endif
               if (grdlon(k) .lt.   0.) then
                  grdlon(k) = grdlon(k) + 360.
               endif
               hx(k) = 1.
               hy(k) = 1.
               f(k) = sin (flat * d2r) * omega
            enddo
         enddo
      else if (igrid .eq. 5) then
c
c        ..spherical grid
c
         do j = 1, n
            do i = 1, m
               k = m * (j-1) + i
               xpos(k) = 1.
               ypos(k) = 1.
               grdlat(k) = (grdj(i,j) - yj) * dely / onedeg + reflat
               grdlon(k) = (grdi(i,j) - xi) * delx / onedeg + reflon
               if (grdlon(k) .gt. 360.) then
                  grdlon(k) = grdlon(k) - 360.
               endif
               if (grdlon(k) .lt.   0.) then
                  grdlon(k) = grdlon(k) + 360.
               endif
               f(k) = sin (grdlat(k) * d2r) * omega
               hx(k) = cos (grdlat(k) * d2r)
               if (hx(k) .lt. 0.) hx(k) = 0.
               hy(k) = 1.
            enddo
         enddo
      else
         write (err_msg, '(''invalid grid projection "'', i2, ''"'')') 
     *          igrid
         call error_exit ('COAMPS_GRID', err_msg)
      endif
c
c     ..clean up
c
      deallocate (distx, disty, grdi, grdj)
c
      return
      end
