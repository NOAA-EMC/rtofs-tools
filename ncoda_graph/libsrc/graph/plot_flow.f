      subroutine plot_flow (out_dir, dtg, date, m, n, nest, igrid,
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
c MODULE NAME:  plot_flow
c
c DESCRIPTION:  plots flow dependent fields used in multivariate
c               analysis
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
      integer   i, k, l
      integer   i1, i2, j1, j2
      integer   igrid
      integer   lntr
      character lvl_typ * 3
      real      node_eq ((gln * glt), 2)
      integer   nest
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
      do_cntr = .true.
      file_typ = 'flowfld'
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
c     ..loop over flow fields
c
      do k = 1, 2
      if (k .eq. 1) then
         fld_name = 'seahgt'
         title = 'Sea Surface Height Flow Field (M) '
         cint = 0.2
         dmn = -1.2
         dmx =  1.2
         lntr = 5
         call gks_color (rgb_fld_clr, MX_FLD_CLR)
         nslc = n_slc
      else if (k .eq. 2) then
         fld_name = 'seatmp'
         title = 'Sea Surface Temperature Flow Field (C) '
         cint = 1.0
         dmn = 0.
         dmx = 30.
         lntr = 4
         call gks_color (rgb_fld_clr, MX_FLD_CLR)
         nslc = n_slc
      endif
c
c     ..read and plot flow field
c
      call rd_coda_file (out_dir, dtg, nest, m, n, l, file_typ,
     *                   fld_name, fluid, tau, lvl_typ, wrk,
     *                   .true., fail)
      if (.not. fail) then
         if (igrid .lt. 0) then
            call glb_merc (wrk, btm, dmn, dmx, m, n, gln, glt, bl,
     *                     br, tl, tr, i1, i2, j1, j2, node_eq,
     *                     pos1, pos2, pos3, pos4, nslc, iamap,
     *                     MX_AMAP, cint, do_cntr, lntr, z_lev,
     *                     spval)
         else
            call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon, bl,
     *                    br, tl, tr, pos1, pos2, pos3, pos4,
     *                    iamap, MX_AMAP)
            call contour_fld (wrk, dmn, dmx, m, n, i1, i2, j1, j2,
     *                        nslc, iamap, MX_AMAP, btm_msk, cint,
     *                        do_btm, do_cntr, .true., .false.,
     *                        .false., lntr, spval)
         endif
         call title_plot (title, .018, 1, 2.1)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title)
         write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
         call title_plot (title, .018, 1, 1.1)
         call frame
      endif
      enddo
c
c     ..clean up
c
      deallocate (btm, btm_msk, iamap, wrk)
c
      return
      end
