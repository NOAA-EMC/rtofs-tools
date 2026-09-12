      subroutine ll2ij (igrid, reflat, reflon, iref, jref, stdlt1,
     *                  stdlt2, stdlon, delx, dely, grdlat, grdlon,
     *                  npts, grdi, grdj)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  ll2ij
c
c DESCRIPTION:  converts latitide, longitude position to grid i,j
c               coordinate for different map projectons 
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c     Name        Type      Usage            Description
c   ---------   --------   -------   -------------------------------
c    delx       real        input    grid spacing in x-direction
c    dely       real        input    grid spacing in y-direction
c    grdi       real        output   grid x position of points
c    grdj       real        output   grid y position of points
c    grdlat     real        input    latitude of points
c    grdlon     real        input    longitude of points
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
c     ..local array dimensions
c
      integer   npts
c
      real      alnfix, alon
      real      check
      real      cn1, cn2, cn3, cn4
      real      cnx, cny
      real      con1, con2, con3
      real      d2r
      real      deg
      real      delx, dely
      character err_msg * 256
      real      gcon
      real      grdi (npts)
      real      grdj (npts)
      real      grdlat (npts)
      real      grdlon (npts)
      integer   i
      integer   igrid
      integer   ihem
      integer   iref, jref
      real      ogcon
      real      onedeg
      real      pi, pi2, pi4
      real      r2d
      real      radius
      real      reflat, reflon
      real      rih, rrih
      real      rotlon
      real      stdlon, stdlt1, stdlt2
      real      x, xih
      real      y, yih
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
         deg = reflat * 0.5 * d2r
         rih = con2 * alog (tan (pi4 + deg))
         do i = 1, npts
            alon = grdlon(i) + 180. - reflon
            if (alon. lt.   0.) alon = alon + 360.
            if (alon .gt. 360.) alon = alon - 360.
            grdi(i) = real (iref) + (alon - 180.) * con2 /
     *                (r2d * delx)
            deg = grdlat(i) * d2r + pi2
            deg = deg * 0.5
            grdj(i) = real (jref) + (con2 * alog (tan (deg)) - 
     *                rih) / dely
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
          deg = (90. - grdlat(i) * real (ihem)) * 0.5 * d2r
          cn4 = tan (deg)
          rrih = cn2 * (cn4 / cn3)**gcon
          check = 180. - stdlon
          alnfix = stdlon + check
          alon = grdlon(i) + check
          if (alon .lt.   0.) alon = alon + 360.
          if (alon .gt. 360.) alon = alon - 360.
          deg = (alon - alnfix) * gcon * d2r
          x = rrih * sin (deg)
          y = -rrih * cos (deg) * real (ihem)
          grdi(i) = real (iref) + (x - xih) / delx
          grdj(i) = real (jref) + (y - yih) / dely
        enddo
      else if (igrid .eq. 4) then
c
c        ..analytic grid
c
         cnx = delx / onedeg
         cny = dely / onedeg
         do i = 1, npts
            grdi(i) = real (iref) + (grdlon(i) - reflon) / cnx
            grdj(i) = real (jref) + (grdlat(i) - reflat) / cny
         enddo
      else if (igrid .eq. 5) then
c
c        ..spherical grid
c
         cnx = delx / onedeg
         cny = dely / onedeg
         do i = 1, npts
            grdi(i) = real (iref) + (grdlon(i) - reflon) / cnx
            grdj(i) = real (jref) + (grdlat(i) - reflat) / cny
         enddo
      else if (igrid .eq. 6) then
c
c        ..global wrap mercator projection
c
         deg = abs (stdlt1) * d2r
         con1 = cos (deg)
         con2 = radius * con1
         con3 = con2 / (r2d * delx)
         deg = reflat * 0.5 * d2r
         rih = con2 * alog (tan (pi4 + deg))
         rotlon = reflon - stdlon
         if (rotlon .lt.   0.) rotlon = rotlon + 360.
         if (rotlon .gt. 360.) rotlon = rotlon - 360.
         do i = 1, npts
            alon = grdlon(i) - stdlon
            if (alon. lt.   0.) alon = alon + 360.
            if (alon .gt. 360.) alon = alon - 360.
            grdi(i) = real (iref) + (alon - rotlon) * con3
            deg = grdlat(i) * d2r + pi2
            deg = deg * 0.5
            grdj(i) = real (jref) + (con2 * alog (tan (deg)) -
     *                rih) / dely
         enddo
      else
         write (err_msg, '(''invalid grid projection "'', i2, ''"'')') 
     *          igrid
         call error_exit ('LL2IJ', err_msg)
      endif
c
      return
      end
