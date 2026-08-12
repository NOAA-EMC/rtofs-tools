      subroutine map_bias (opt, m, n, fld, title1, title3, igrid,
     *                     rlat, stdlt1, stdlt2, stdlon, bl, br,
     *                     tl, tr, i1, i2, j1, j2, gln, glt,
     *                     node_eq, pln, plt, node_nh, node_sh,
     *                     fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_bias
c
c DESCRIPTION:  maps time averaged analysis increment fields
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
      parameter (MX_AMAP = 64 000 000)
c
c     ..local array dimensions
c
      integer   m, n
      integer   gln, glt
      integer   pln, plt
c
      real      bl(2), br(2)
      real      cntr_int
      real      dmn, dmx
      logical   do_btm
      logical   do_cntr
      logical   do_lbl
      real      fld (m * n)
      integer   fno
      integer   i
      integer   i1, i2
      integer   igrid
      integer   j1, j2
      character lbl * 256
      integer   lntr
      integer   n_pass
      real      node_eq ((gln * glt), 2)
      real      node_nh ((pln * plt), 2)
      real      node_sh ((pln * plt), 2)
      integer   opt
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval
      real      stdlon
      real      stdlt1, stdlt2
      character title1 * (*)
      character title2 * 256
      character title3 * (*)
      real      tl(2), tr(2)
      character var_name (4) * 19
      integer   z_lev
c
c     ..allocatable arrays
c
      real,     allocatable :: btm (:)
      real,     allocatable :: btm_msk (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: wrk (:)
c
      include 'color_table.h'
c
c     ..define map position in plot frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
c     ..set variable names
c
      data var_name / 'Surface Temperature',
     *                'Surface Salinity   ',
     *                'Ice Coverage       ',
     *                'Ice Coverage       ' /
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (btm (m * n))
      allocate (btm_msk (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n))
c
c     ..initialize
c
      do_cntr = .false.
      do_lbl = .true.
      lntr = 5
      n_pass = 2
      call gks_color (rgb_anm_clr, MX_ANM_CLR)
      z_lev = 0
c
c     ..set range of plot variables
c
      if (opt .eq. 1) then
         dmn = -1.2
         dmx =  1.2
         cntr_int = 0.6
      else if (opt .eq. 2) then
         dmn = -0.6
         dmx =  0.6
         cntr_int = 0.3
      else if (opt .eq. 3 .or. opt .eq. 4) then
         dmn = -30.
         dmx =  30.
         cntr_int = 10.
      endif
c
c     ..set dummy land and bottom masks
c
      do_btm = .false.
      do i = 1, (m * n)
         btm(i) = 0.
         btm_msk(i) = 1.
      enddo
c
c     ..extract and smooth field
c
      do i = 1, (m * n)
         wrk(i) = fld(i)
      enddo
      call smth_2d (n_pass, wrk, m, n, spval)
c
c     ..plot field
c
      if (igrid .lt. 0) then
         if (opt .eq. 1 .or. opt .eq. 2) then
            call glb_merc (wrk, btm, dmn, dmx, m, n, gln, glt,
     *                     bl, br, tl, tr, i1, i2, j1, j2,
     *                     node_eq, pos1, pos2, pos3, pos4,
     *                     n_slca, iamap, MX_AMAP, cntr_int, 
     *                     do_cntr, lntr, z_lev, spval)
         else if (opt .eq. 3) then
            call glb_polar (wrk, dmn, dmx, m, n, pln, plt,
     *                      node_nh, pos1, pos2, pos3, pos4,
     *                      n_slca, iamap, MX_AMAP, lntr,
     *                      'nh', spval)
         else if (opt .eq. 4) then
            call glb_polar (wrk, dmn, dmx, m, n, pln, plt,
     *                      node_sh, pos1, pos2, pos3, pos4,
     *                      n_slca, iamap, MX_AMAP, lntr,
     *                      'sh', spval)
         endif
      else
         call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                 bl, br, tl, tr, pos1, pos2, pos3,
     *                 pos4, iamap, MX_AMAP)
         call contour_fld (wrk, dmn, dmx, m, n, i1, i2, j1, j2,
     *                     n_slca, iamap, MX_AMAP, btm_msk,
     *                     cntr_int, do_btm, do_cntr, .true.,
     *                     do_lbl, .false., lntr, spval)
      endif
c
c     ..form plot titles
c
      write (lbl, '(a, '' Mean Error'')') trim (var_name(opt))
      title2 = trim (title1) // '     ' // trim (lbl)
      call title_plot (title2, .018, 1, 2.)
      call title_plot (title3, .018, 1, 1.)
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)') fno, trim (lbl)
      call frame
c
c     ..clean up
c
      deallocate (btm, btm_msk, iamap, wrk)
c
      return
      end
