      subroutine plot_botprs (dir_path, date, dtg, tau, nest, m, n,
     *                        depth, mask, igrid, rlat, stdlt1, stdlt2,
     *                        stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *                        gln, glt, node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_botprs
c
c DESCRIPTION:  maps a forecast bottom pressure field and color
c               fills the contours
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
      parameter (MX_AMAP = 144 000 000)
c
      integer    UNIT
      parameter (UNIT = 10)
c
c     ..local array dimensions
c
      integer   m, n
      integer   gln, glt
c
      real      bl(2), br(2)
      real      cntr_int
      character date * 15
      real      depth (m * n)
      character dir_path * (*)
      real      dmx, dmn
      logical   do_btm
      logical   do_cntr
      logical   do_lbl
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
      integer   n_nodes
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   nslc
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval 
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      character title * 132
      real      tl(2), tr(2)
      integer   z_lev
c
c     ..allocatable arrays
c
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
c...............................executable..............................
c
c     ..initialization
c
      fld_name = 'botprs'
      lvl_typ = 'sfc'
      fluid = 'o'
      file_typ = 'fcstfld'
      l = 1
      n_nodes = m * n
      tau = 24
      z_lev = 0
c
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n))
c
c     ..read field
c
      fail = .true.
      call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                   fld_name, fluid, tau, lvl_typ, wrk, 
     *                   .true., fail) 
      if (.not. fail) then
         write (title, '(''Bottom Pressure (M)'')')
         call gks_color (rgb_fld_clr, MX_FLD_CLR)
         cntr_int = 0.2
         do_cntr = .false.
         dmn = 0.
         dmx = 6000.
         do_lbl = .true.
         lntr = 6
         nslc = n_slc
c
c        ..set no bottom mask
c
         do_btm = .false.
         do i = 1, n_nodes
            if (depth(i) .gt. 1.01) then
               btm_msk(i) = -1.
            else
               btm_msk(i) = 1.
            endif
         enddo
c
c        ..set plotting mask
c
         do i = 1, n_nodes
            if (mask(i) .eq. 0) wrk(i) = spval
         enddo
c
c        ..plot field
c
         if (igrid .lt. 0) then
            call glb_merc (wrk, depth, dmn, dmx, m, n, gln, glt,
     *                     bl, br, tl, tr, i1, i2, j1, j2, node_eq,
     *                     pos1, pos2, pos3, pos4, nslc, iamap, 
     *                     MX_AMAP, cntr_int, do_cntr, lntr, z_lev,
     *                     spval)
         else
            call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                    bl, br, tl, tr, pos1, pos2, pos3,
     *                    pos4, iamap, MX_AMAP)
            call contour_fld (wrk, dmn, dmx, m, n, i1, i2, j1,
     *                        j2, nslc, iamap, MX_AMAP, btm_msk,
     *                        cntr_int, do_btm, do_cntr, .true.,
     *                        do_lbl, .false., lntr, spval)
         endif
         call title_plot (title, .018, 1, 2.1)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title)
         write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
         call title_plot (title, .018, 1, 1.)
         call frame
      endif
c
c     ..clean up
c
      deallocate (btm_msk, iamap, wrk)
c
      return
      end
