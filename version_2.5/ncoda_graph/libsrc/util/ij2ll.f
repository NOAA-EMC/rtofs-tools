      subroutine ij2ll (igrid, reflat, reflon, iref, jref, stdlt1, 
     *                  stdlt2, stdlon, delx, dely, grdi, grdj,
     *                  npts, grdlat, grdlon)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  ij2ll
c
c DESCRIPTION:  converts grid i,j coordinate to latitude, longitude 
c               position for different map projectons 
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c     Name        Type      Usage            Description
c   ---------   --------   -------   -------------------------------
c    delx       real        input    grid spacing in x-direction
c    dely       real        input    grid spacing in y-direction
c    grdi       real        input    grid x position of points
c    grdj       real        input    grid y position of points
c    grdlat     real        output   latitude of points
c    grdlon     real        output   longitude of points
c    igrid      integer     input    type of grid projection:
c                                    1. mercator projection
c                                    2. lambert conformal projection
c                                    3. polar stereographic projection
c                                    4. cartesian coordinates
c                                    5. spherical projection
c                                    6. global wrap mercator
c    iref       integer     input    i-coordinate of reference point
c    jref       integer     input    j-coordinate of reference point
c    npts       integer     input    number points
c    reflat     real        input    latitude at reference point 
c    reflon     real        input    longitude at reference point
c    stdlt1     real        input    standard latitude of grid
c    stdlt2     real        input    second standard latitude of grid 
c                                    (required for lambert conformal)
c    stdlon     real        input    standard longitude of grid
c                                    (points to the north)
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
c     ..local array dimension
c
      integer   npts
c
      real      angle
      real      cn1, cn2, cn3, cn4
      real      cnx, cny
      real      con1, con2, con3
      real      d2r
      real      deg
      real      gcon
      integer   igrid
      integer   iref, jref
      real      delx, dely
      character err_msg * 256
      real      grdi (npts)
      real      grdj (npts)
      real      grdlat (npts)
      real      grdlon (npts)
      integer   i
      integer   ihem
      real      ogcon
      real      onedeg
      real      pi, pi2, pi4
      real      reflat, reflon
      real      r2d
      real      radius
      real      rih, rr
      real      stdlon, stdlt1, stdlt2
      real      x, xih, xx
      real      y, yih, yy
c
c...............................executable..............................
c
c     ..local constants
c
      pi = 4. * atan (1.)
      pi2 = pi / 2.
      pi4 = pi / 4.
      d2r = pi / 180.
      r2d = 180. / pi
      radius = 6371229.
      onedeg = radius * 2. * pi / 360.
c
      if (igrid .eq. 1) then
c
c        ..mercator projection 
c
         deg = abs (stdlt1) * d2r
         con1 = cos (deg)
         con2 = radius * con1
         con3 = r2d * delx / con2
         deg = reflat * 0.5 * d2r
         rih = con2 * alog (tan (pi4 + deg))
         do i = 1, npts
            rr = rih + (grdj(i) - real (jref)) * dely
            grdlat(i) = (2. * atan (exp (rr / con2)) - pi2) * r2d
            grdlon(i) = reflon + (grdi(i) - real (iref)) * con3
            if (grdlon(i) .gt. 360.) grdlon(i) = grdlon(i) - 360.
            if (grdlon(i) .lt.   0.) grdlon(i) = grdlon(i) + 360.
         enddo
      else if (igrid .eq. 2 .or. igrid .eq. 3) then
c
c        ..lambert conformal or polar stereographic
c
         if (igrid .eq. 2) then
            if (stdlt1 .eq. stdlt2) then
               gcon = sin (abs (stdlt1) * d2r)
            else
               gcon = (log (sin ((90. - abs (stdlt1)) * d2r)) -
     *                 log (sin ((90. - abs (stdlt2)) * d2r))) /
     *                (log (tan ((90. - abs (stdlt1)) * 0.5 * d2r)) -
     *                 log (tan ((90. - abs (stdlt2)) * 0.5 * d2r)))
            endif
         else
            gcon = 1.
         endif
         ogcon = 1. / gcon
         ihem = nint (abs (stdlt1) / stdlt1)
         deg = (90. - abs (stdlt1)) * d2r
         cn1 = sin (deg)
         cn2 = radius * cn1 * ogcon
         deg = deg * 0.5
         cn3 = tan (deg)
         deg = (90. - abs (reflat)) * 0.5 * d2r
         cn4 = tan (deg)
         rih = cn2 * (cn4 / cn3)**gcon
         deg = (reflon - stdlon) * d2r * gcon
         xih = rih * sin (deg)
         yih = -rih * cos (deg) * real (ihem)
         do i = 1, npts
            x = xih + (grdi(i) - real (iref)) * delx
            y = yih + (grdj(i) - real (jref)) * dely
            rr = sqrt (x * x + y * y)
            grdlat(i) = r2d * (pi2 - 2. * atan (cn3 * 
     *                 (rr / cn2)**ogcon)) * real (ihem)
            xx = x
            yy = -y * real (ihem)
            if (yy .eq. 0.) then
               if (xx. le. 0.) then
                  angle = -90.
               else if (xx .gt. 0.) then
                  angle = 90.
               endif
            else
               angle = atan2 (xx,yy) * r2d
            endif
            grdlon(i) = stdlon + angle * ogcon
            if (grdlon(i) .gt. 360.) grdlon(i) = grdlon(i) - 360.
            if (grdlon(i) .lt.   0.) grdlon(i) = grdlon(i) + 360.
         enddo
      else if (igrid .eq. 4) then
c
c        ..analytic grid
c
         cn2 = delx / onedeg
         do i = 1, npts
            grdlat(i) = reflat + (grdj(i) - real (jref)) * cn2
            grdlon(i) = reflon + (grdi(i) - real (iref)) * cn2
            if (grdlon(i) .gt. 360.) grdlon(i) = grdlon(i) - 360.
            if (grdlon(i) .lt.   0.) grdlon(i) = grdlon(i) + 360.
         enddo
      else if (igrid .eq. 5) then
c
c        ..spherical grid
c
         cnx = delx / onedeg
         cny = dely / onedeg
         do i = 1, npts
            grdlat(i) = (grdj(i) - real (jref)) * cny + reflat
            grdlon(i) = (grdi(i) - real (iref)) * cnx + reflon
            if (grdlon(i) .gt. 360.) grdlon(i) = grdlon(i) - 360.
            if (grdlon(i) .lt.   0.) grdlon(i) = grdlon(i) + 360.
         enddo
      else if (igrid .eq. 6) then
c
c        ..global wrap mercator projection
c
         deg = abs (stdlt1) * d2r
         con1 = cos (deg)
         con2 = radius * con1
         con3 = r2d * delx / con2
         deg = reflat * 0.5 * d2r
         rih = con2 * alog (tan (pi4 + deg))
         do i = 1, npts
            rr = rih + (grdj(i) - real (jref)) * dely
            grdlat(i) = (2. * atan (exp (rr / con2)) - pi2) * r2d
            grdlon(i) = reflon + (grdi(i) - real (iref)) * con3
            if (grdlon(i) .gt. 360.) grdlon(i) = grdlon(i) - 360.
            if (grdlon(i) .lt.   0.) grdlon(i) = grdlon(i) + 360.
         enddo
      else
         write (err_msg, '(''invalid grid projection "'', i2, ''"'')') 
     *          igrid
         call error_exit ('IJ2LL', err_msg)
      endif
c
      return
      end
