      subroutine plot_dist (out_dir, dtg, date, m, n, nest, igrid,
     *                      rlat, stdlt1, stdlt2, stdlon, bl, br,
     *                      tl, tr, i1, i2, j1, j2, gln, glt, 
     *                      node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_dist
c
c DESCRIPTION:  plots land distance field used in analysis
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
      parameter (MX_AMAP = 36 000 000)
c
c     ..local array dimensions
c
      integer   m, n
      integer   gln, glt
c
      real      bl(2), br(2)
      real      cint
      real      dmx, dmn
      character date * 15
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
      integer   nest
      real      node_eq ((3337 * 1843), 2)
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
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (btm (m * n))
      allocate (btm_msk (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n))
c
c     ..initialization
c
      file_typ = 'datafld'
      fluid = 'o'
      l = 1
      lvl_typ = 'sfc'
      tau = 0
      z_lev = 0
c
c     ..set no bottom mask
c
      do_btm = .false.
      do i = 1, (m * n)
         btm(i) = 0.
         btm_msk(i) = 1.
      enddo
c
c----------------------------------------------------------------------
c
c     ..read land distance field
c
      fld_name = 'dstlnd'
      call rd_coda_file (out_dir, dtg, nest, m, n, l, file_typ,
     *                   fld_name, fluid, tau, lvl_typ, wrk,
     *                   .true., fail)
      if (fail) return
c
c     ..set plot variables
c
      do_cntr = .false.
      dmn = 0.
      dmx = 120.
      lntr = 5
      nslc = n_slc
c
c     ..plot sea surface temperature flow field
c
      call gks_color (rgb_fld_clr, MX_FLD_CLR)
      if (igrid .lt. 0) then
         call glb_merc (wrk, btm, dmn, dmx, m, n, gln, glt, bl,
     *                  br, tl, tr, i1, i2, j1, j2, node_eq,
     *                  pos1, pos2, pos3, pos4, nslc, iamap, 
     *                  MX_AMAP, cint, do_cntr, lntr, z_lev,
     *                  spval)
      else
         call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon, bl,
     *                 br, tl, tr, pos1, pos2, pos3, pos4,
     *                 iamap, MX_AMAP)
         call contour_fld (wrk, dmn, dmx, m, n, i1, i2, j1, j2,
     *                     nslc, iamap, MX_AMAP, btm_msk, cint,
     *                     do_btm, do_cntr, .true., .false.,
     *                     .false., lntr, spval)
      endif
      title = 'Land Distance (KM)'
      call title_plot (title, .018, 1, 2.1)
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (title)
      write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
      call title_plot (title, .018, 1, 1.1)
      call frame
c
c     ..clean up
c
      deallocate (btm, btm_msk, iamap, wrk)
c
      return
      end
