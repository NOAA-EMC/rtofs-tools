      subroutine set_zoom (datao, nest, gln, glt, zoom, n_lon,
     *                     n_lat, n_lvl, global, n_proj, delx,
     *                     dely, rlat, stdlt1, stdlt2, stdlon,
     *                     upd, z_lvl, bl, br, tl, tr, i1, i2,
     *                     j1, j2, subset)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  set_zoom
c
c DESCRIPTION:  set zoom (or not) grid definitions
c      
c PARAMETERS:
c       Name          Type       Usage            Description
c   -------------   ----------   -----   -----------------------------
c   date            char         input   dtg plot label
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
c     ..local array dimensions
c
      integer   n_lon, n_lat, n_lvl
c
      real      bl(2), br(2)
      real      datao (2000)
      real      delx, dely
      character err_msg * 256
      integer   gln, glt
      logical   global
      real      glat (4), glon (4)
      real      grdi (4), grdj (4)
      integer   iref, jref
      integer   i1, i2
      integer   j1, j2
      integer   k, n
      integer   n_proj
      integer   n_pts
      integer   nest
      real      rlat, rlon
      real      stdlon
      real      stdlt1, stdlt2
      logical   subset
      real      tl(2), tr(2)
      integer   upd
      real      zoom (4)
      real      z_lvl (100)
c
c...............................executable..............................
c
c     ..decode header
c
      n = 30 + (nest-1) * 30
      delx = datao(n+4)
      dely = datao(n+5)
      n_lon = nint (datao(n+0))
      n_lat = nint (datao(n+1))
      n_lvl = nint (datao(1))
      n_proj = nint (datao(3))
      stdlt1 = datao(4)
      stdlt2 = datao(5)
      stdlon = datao(6)
      rlat = datao(7)
      rlon = datao(8) 
      iref = nint (datao(n+2))
      jref = nint (datao(n+3))
      upd = nint (datao(20))
      if (datao(22) .gt. 0.) then
         global = .true.
      else
         global = .false.
      endif
      do k = 1, 100
         z_lvl(k) = datao(k+500)
      enddo
c
c     ..check for zoomed grid subsetting 
c
      subset = .false.
      if (zoom(1) .gt. -990. .and. zoom(2) .gt. -990. .and.
     *    zoom(3) .gt. -990. .and. zoom(4) .gt. -990.) then
         if (zoom(2) .gt. zoom(1) .and. zoom(4) .gt. zoom(3)) then
            subset = .true.
         else
            write (err_msg, '(''invalid zoom namelist parameters: '',
     *             ''lat = '', 2f10.1, ''  lon = '', 2f10.1)')
     *             zoom(1), zoom(2), zoom(3), zoom(4)
            call error_exit ('SET_ZOOM', err_msg)
         endif
      endif
c
      n_pts = 4
      if (subset) then
         glat(1) = real (zoom(1))
         glon(1) = real (zoom(3))
         glat(2) = real (zoom(1))
         glon(2) = real (zoom(4))
         glat(3) = real (zoom(2))
         glon(3) = real (zoom(3))
         glat(4) = real (zoom(2))
         glon(4) = real (zoom(4))
         if (n_proj .gt. 0) then
            call ll2ij (n_proj, rlat, rlon, iref, jref, stdlt1,
     *                  stdlt2, stdlon, delx, dely, glat, glon,
     *                  n_pts, grdi, grdj)
         else
            delx = 9000.
            dely = 9000.
            iref = gln / 2 + 1
            jref = glt / 2 + 1
            rlat = 0.
            rlon = 180.
            stdlt1 = 0.
            stdlt2 = 0.
            stdlon = 180.
            call ll2ij (1, rlat, rlon, iref, jref, stdlt1, stdlt2,
     *                  stdlon, delx, dely, glat, glon, n_pts,
     *                  grdi, grdj)
         endif
         bl(1) = zoom(1)
         bl(2) = zoom(3)
         br(1) = zoom(1)
         br(2) = zoom(4)
         tl(1) = zoom(2)
         tl(2) = zoom(3)
         tr(1) = zoom(2)
         tr(2) = zoom(4)
         i1 = nint (grdi(1))
         i2 = nint (grdi(2))
         j1 = nint (grdj(1))
         j2 = nint (grdj(4))
      else
c
c        ..set full grid corner points
c 
         if (n_proj .gt. 0) then
            bl(1) = datao(n+6)
            bl(2) = datao(n+7)
            br(1) = datao(n+8)
            br(2) = datao(n+9)
            tr(1) = datao(n+10)
            tr(2) = datao(n+11)
            tl(1) = datao(n+12)
            tl(2) = datao(n+13)
            i1 = 1
            i2 = n_lon
            j1 = 1
            j2 = n_lat
         else
c
c           ..set interpolation grid corner points
c
            grdi(1) = 1.
            grdj(1) = 1.
            grdi(2) = real (gln)
            grdj(2) = 1.
            grdi(3) = real (gln)
            grdj(3) = real (glt)
            grdi(4) = 1.
            grdj(4) = real (glt)
c
            delx = 9000.
            dely = 9000.
            iref = gln / 2 + 1
            jref = glt / 2 + 1
            rlat = 0.
            rlon = 180.
            stdlt1 = 0.
            stdlt2 = 0.
            stdlon = 180.
            call ij2ll (1, rlat, rlon, iref, jref, stdlt1, stdlt2,
     *                  stdlon, delx, dely, grdi, grdj, n_pts,
     *                  glat, glon)
            bl(1) = glat(1)
            bl(2) = glon(1)
            br(1) = glat(2)
            br(2) = glon(2)
            tr(1) = glat(3)
            tr(2) = glon(3)
            tl(1) = glat(4)
            tl(2) = glon(4)
c
            if (nint (bl(2)) .eq. 360) bl(2) = 0.
            if (nint (br(2)) .eq. 0)   br(2) = 360.
            if (nint (tr(2)) .eq. 0)   tr(2) = 360.
            if (nint (tl(2)) .eq. 360) tl(2) = 0.
c
            i1 = 1
            i2 = gln
            j1 = 1
            j2 = glt
         endif
      endif
c
      return
      end
