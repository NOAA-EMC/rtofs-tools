      subroutine map_bkg (n_proj, rlat, stdlt1, stdlt2, stdlon, bl,
     *                    br, tl, tr, pos1, pos2, pos3, pos4, iamap,
     *                    mx_amap)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_bkg
c
c DESCRIPTION:  set up map background in the NCAR 4.4 graphics system
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name          Type       Usage            Description
c   --------    ---------   -------   ---------------------------------
c    bl, br     real         input    grid bottom left, right lat, lons
c    iamap      integer      output   NCAR plot boundaries
c    mx_amap    integer      input    iamap array dimension
c    n_proj     integer      input    grid projection number
c    pos1       real         input    left frame position (0 to 1)
c    pos2       real         input    right frame position (0 to 1)
c    pos3       real         input    bottom frame position (0 to 1)
c    pos4       real         input    top frame position (0 to 1)
c    rlat       real         input    grid reference latitude
c    stdlon     real         input    grid standard longitude
c    stdlt(s)   real         input    grid standard latitudes
c    tl, tr     real         input    grid top left, right lat, lons
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
      implicit  none
c
      integer   mx_amap
c
      real      bl (2), br (2)
      real      chsz
      real      del
      character err_msg * 256
      integer   iamap (mx_amap)
      integer   n_proj
      real      ofx, ofy
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      stdlt1, stdlt2
      real      stdlon
      real      tl (2), tr (2)
c
c...............................executable..............................
c
c     ..initialize area map edge boundaries
c
      call arinam (iamap, mx_amap)
c
c     ..do not label meridians and poles
c
      call mapsti ('LA', 0)
c
c     ..set map position within plotter frame
c
      call mappos (pos1, pos2, pos3, pos4)
      call mapset ('PO', bl, br, tl, tr)
c
c     ..set map projection 
c
      if (n_proj .le. 0) then
c
c        ..spherical grid
c
         call maproj ('ME', 0., stdlon, 0.0)
      else if (n_proj .eq. 1) then
c
c        ..mercator
c
         call maproj ('ME', 0.0, stdlon, 0.0)
      else if (n_proj .eq. 2) then
c
c        ..lambert conformal
c
         call maproj ('LC', stdlt1, stdlon, stdlt2)
      else if (n_proj .eq. 3) then
c
c        ..polar stereographic
c
         call maproj ('ST', rlat, stdlon, 0.0)
      else if (n_proj .eq. 4 .or. n_proj .eq. 5) then
c
c        ..cylindrical equidistant
c
         call maproj ('CE', 0.0, stdlon, 0.0)
      else
         write (err_msg, '(''unknown projection number "'', i2,
     *                     ''"'')') n_proj
         call error_exit ('MAP_BKG', err_msg)
      endif
c
c     ..set map grid overlay spacing (degrees)
c
      if (abs (tl(1) - bl(1)) .gt. abs (tr(2) - tl(2))) then
         del = abs (tl(1) - bl(1))
      else
         del = abs (tr(2) - tl(2))
      endif
      if (del .lt. 30.) then
         del = 5.
      else if (del .lt. 60.) then
         del = 10.
      else
         del = 30.
      endif
      call mapstr ('GR', del)
c
c     ..initialize, add land sea boundaries to area map
c
      call mplnam ('Earth..3', 1, iamap)
c
c     ..set character size and off sets of lat/lon labels
c
      chsz = .011
      ofx = chsz
      ofy = 1.5 * chsz
      call gsplci (1)
c
c     ..add lat/lon labels (as if this works)
c
c     call mdlblt (pos1, pos3, pos1, pos4, -ofx,    0., chsz, 0.,  1.)
c     call mdlblt (pos2, pos3, pos2, pos4,  ofx,    0., chsz, 0., -1.)
c     call mdlbln (pos1, pos3, pos2, pos3,    0., -ofy, chsz, 0.,  0.)
c     call mdlbln (pos1, pos4, pos2, pos4,    0.,  ofy, chsz, 0.,  0.)
c
      return
      end
