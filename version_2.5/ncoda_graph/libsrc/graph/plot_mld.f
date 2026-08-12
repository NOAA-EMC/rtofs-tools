      subroutine plot_mld (out_dir, dtg, date, upd, m, n, btm, nest,
     *                     igrid, rlat, stdlt1, stdlt2, stdlon, bl,
     *                     br, tl, tr, i1, i2, j1, j2, gln, glt, 
     *                     node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_mld
c
c DESCRIPTION:  plots mixed layer depth field
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
c
      real      bl(2), br(2)
      real      btm (m * n)
      real      cint
      real      dmx, dmn
      character date * 15
      integer   day
      logical   do_btm
      logical   do_cntr
      character dtg * 10
      logical   fail
      character fcst_date * 13
      character file_dtg * 10
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, l
      integer   i1, i2, j1, j2
      integer   igrid
      integer   lntr
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   nslc
      character out_dir * (*)
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval 
      integer   status
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      character title1 * 132
      character title2 * 132
      real      tl(2), tr(2)
      integer   upd
      integer   year
      integer   z_lev
      integer   zulu
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: wrk (:)
c
      include 'color_table.h'
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
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
      allocate (btm_msk (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n))
c
c     ..initialization
c
      fluid = 'o'
      l = 1
      lvl_typ = 'sfc'
      z_lev = 0
c
c     ..set plot variables
c
      cint = -1.
      do_cntr = .false.
      dmn = 0.
      dmx = 90.
      lntr = 10
      nslc = n_slc
c
c     ..form forecast date time group label
c
      call dtgmod (dtg, -upd, file_dtg, status)
      read (file_dtg(3:4), '(i2)') year
      read (file_dtg(5:6), '(i2)') mon
      read (file_dtg(7:8), '(i2)') day
      read (file_dtg(9:10), '(i2)') zulu
      write (fcst_date, '(i2.2, 1x, a, 1x, i2.2, 1x, i2.2, ''Z'')')
     *       day, month(mon), year, zulu
c
c     ..set bottom mask
c
      do_btm = .false.
      do i = 1, (m * n)
         if (btm(i) .lt. 1.01) then
            btm_msk(i) = -1.
         else
            btm_msk(i) = 1.
         endif
      enddo
c
c----------------------------------------------------------------------
c
c     ..read forecast mixed layer field
c
      fld_name = 'mixlyr'
      file_typ = 'fcstfld'
      tau = upd
      title1 = 'Forecast Mixed Layer Depth (M)'
      write (title2, '(a, 2x, ''Tau '', i3.3)') fcst_date, tau
      call rd_coda_file (out_dir, file_dtg, nest, m, n, l, file_typ,
     *                   fld_name, fluid, tau, lvl_typ, wrk, .true.,
     *                   fail)
      if (fail) then
         file_typ = 'analfld'
         tau = 0
         title1 = 'Analysis Mixed Layer Depth (M)'
         write (title2, '(a, 2x, ''Tau '', i3.3)') date, tau
         call rd_coda_file (out_dir, dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, wrk,
     *                      .true., fail)
      endif
c
      if (.not. fail) then
         do i = 1, (m * n)
            if (wrk(i) .lt. 0.) wrk(i) = 0.
         enddo
c
c        ..plot mixed layer depth field
c
         call gks_color (rgb_fld_clr, MX_FLD_CLR)
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
         call title_plot (title1, .018, 1, 2.1)
         call title_plot (title2, .018, 1, 1.1)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title1)
         call frame
      endif
c
c     ..clean up
c
      deallocate (btm_msk, iamap, wrk)
c
      return
      end
