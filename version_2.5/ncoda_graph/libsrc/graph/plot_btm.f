      subroutine plot_btm (out_dir, dtg, date, m, n, nest, depth,
     *                     mask, tau, igrid, rlat, stdlt1, stdlt2,
     *                     stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *                     btm_max, gln, glt, node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_btm
c
c DESCRIPTION:  plots grid bathymetry and open sea grid points
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
      integer    MX_AMAP
      parameter (MX_AMAP = 72 000 000)
c
c     ..local array dimensions
c
      integer   m, n
      integer   gln, glt
c
      real      bl(2), br(2)
      real      bli(2), bri(2)
      real      btm_max
      real      cint
      real      dmx, dmn
      character date * 15
      real      depth (m * n)
      logical   do_btm
      logical   do_cntr
      character dtg * 10
      logical   fail
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, l
      integer   i1, i2, j1, j2
      integer   igrid
      integer   lntr
      character lvl_typ * 3
      integer   mask (m * n)
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   nslc
      character out_dir * (*)
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval 
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      character title * 132
      real      tl(2), tr(2)
      real      tli(2), tri(2)
      real      u, v
      integer   z_lev
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: grd_lat (:)
      real,     allocatable :: grd_lon (:)
      integer,  allocatable :: iamap (:)
c
      include 'color_table.h'
c
c     ..define map position in plot frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
c     ..define global irregular grid corners
c
      data  bli / -90.,   0. /
      data  bri / -90., 360. /
      data  tli /  90.,   0. /
      data  tri /  90., 360. /
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (grd_lat (m * n))
      allocate (grd_lon (m * n))
      allocate (iamap (MX_AMAP))
c
c     ..set no bottom mask
c
      do_btm = .false.
      do i = 1, (m * n)
         btm_msk(i) = 1.
      enddo
c
c     ..set plot variables
c
      do_cntr = .false.
      cint = 0.
      dmn = 0.
      dmx = real (nint (btm_max / 60.) * 60)
c***************
      write (*,'(''btm dmx: '',f10.1)') dmx
c********************
      if (dmx .lt. 60.) dmx = 60.
      lntr = 5
      nslc = n_slc
      z_lev = 0
c
c     ..plot bathymetry field
c
      call gks_color (rgb_fld_clr, MX_FLD_CLR)
      if (igrid .lt. 0) then
         call glb_merc (depth, depth, dmn, dmx, m, n, gln,
     *                  glt, bl, br, tl, tr, i1, i2, j1, j2, 
     *                  node_eq, pos1, pos2, pos3, pos4,
     *                  nslc, iamap, MX_AMAP, cint, do_cntr, 
     *                  lntr, z_lev, spval)
      else
         call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon, bl,
     *                 br, tl, tr, pos1, pos2, pos3, pos4,
     *                 iamap, MX_AMAP)
         call contour_fld (depth, dmn, dmx, m, n, i1, i2, j1, j2,
     *                     nslc, iamap, MX_AMAP, btm_msk, cint,
     *                     do_btm, do_cntr, .true., .false.,
     *                     .false., lntr, spval)
      endif
      title = 'Bottom Depth (M)'
      call title_plot (title, .018, 1, 2.1)
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (title)
      write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
      call title_plot (title, .018, 1, 1.1)
      call frame
c
c----------------------------------------------------------------------
c     
c     ..plot masked and open sea points
c
      file_typ = 'datafld'
      fluid = 'o'
      lvl_typ = 'sfc'
      l = 1
      fld_name = 'grdlat'
      call rd_coda_file (out_dir, dtg, nest, m, n, l, file_typ,
     *                   fld_name, fluid, tau, lvl_typ, grd_lat,
     *                   .true., fail)
      fld_name = 'grdlon'
      call rd_coda_file (out_dir, dtg, nest, m, n, l, file_typ,
     *                   fld_name, fluid, tau, lvl_typ, grd_lon,
     *                   .true., fail)
      if (fail) return
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
      if (igrid .lt. 0) then
         call map_bkg (igrid, 0., stdlt1, stdlt2, 180., bli,
     *                 bri, tli, tri, pos1, pos2, pos3, pos4,
     *                 iamap, MX_AMAP)
      else
         call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon, bl,
     *                 br, tl, tr, pos1, pos2, pos3, pos4,
     *                 iamap, MX_AMAP)
      endif
c
      call gsmk (2)
      call gsmksc (0.05)
      call gspmci (3)
      do i = 1, (m * n)
         if (mask(i) .eq. 0) then
            call maptra (grd_lat(i), grd_lon(i), u, v)
            call gpm (1, u, v)
         endif
      enddo
c
      call gspmci (4)
      do i = 1, (m * n)
         if (mask(i) .gt. 0) then
            call maptra (grd_lat(i), grd_lon(i), u, v)
            call gpm (1, u, v)
         endif
      enddo
c
      title = 'Open Sea Grid Points'
      call title_plot (title, .018, 1, 1.1)
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (title)
      write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
      call title_plot (title, .019, 1, 2.1)
      call frame
c
c     ..clean up
c
      deallocate (btm_msk, grd_lat, grd_lon, iamap)
c
      return
      end
