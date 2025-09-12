      subroutine plot_diurnal (dir_path, date, dtg, nest, m, n, mask,
     *                         igrid, rlat, stdlt1, stdlt2, stdlon,
     *                         bl, br, tl, tr, i1, i2, j1, j2,
     *                         gln, glt, node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_diurnal
c
c DESCRIPTION:  maps hourly (tau 24 through tau 36) sst forecast 
c               anomalies from daily mean forecast
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
      real      dmx, dmn
      character date * 15
      character dir_path * (*)
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
      integer   mask (m * n)
      integer   n_pass
      integer   nest
      real      nk
      integer   nodes
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
      real,     allocatable :: btm (:)
      real,     allocatable :: btm_msk (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: sst (:)
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
      fld_name = 'seatmp'
      file_typ = 'fcstfld'
      lvl_typ = 'sfc'
      fluid = 'o'
      l = 1
      nodes = m * n
      n_pass = 2
c
c     ..allocate arrays
c
      allocate (btm (m * n))
      allocate (btm_msk (m * n))
      allocate (iamap (MX_AMAP))
      allocate (sst (m * n))
      allocate (wrk (m * n))
c
      btm = 0.
      btm_msk = 1.
      iamap = 0
      nk = 0.
      sst = 0.
      z_lev = 0
c
c     ..compute the daily mean
c
      do k = 12, 36
         tau = k
         call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, wrk,
     *                      .true., fail)
         if (fail) then
            write (*, '(''missing sst field: dtg, tau "'', a, 2x, 
     *             i3, ''"'')') dtg, tau
            deallocate (btm_msk, iamap, sst, wrk)
            return
         endif
         do i = 1, nodes
            if (mask(i) .gt. 0) then
               sst(i) = sst(i) + wrk(i)
            endif
         enddo
         nk = nk + 1.
      enddo
c
      do i = 1, nodes
         sst(i) = sst(i) / nk
      enddo
c
c     ..set plot variables
c
      call gks_color (rgb_anm_clr, MX_ANM_CLR)
      cntr_int = 0.5
      do_cntr = .false.
      dmn = -1.5
      dmx =  1.5
      lntr = 5
      nslc = n_slca
c
c     ..compute diurnal variability
c
      do k = 12, 36
         tau = k
         call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, wrk,
     *                      .true., fail)
         do i = 1, nodes
            if (mask(i) .gt. 0) then
               wrk(i) = wrk(i) - sst(i)
            else
               wrk(i) = spval
            endif
         enddo
         call smth_2d (n_pass, wrk, m, n, spval)
c
c        ..plot field
c
         if (igrid .lt. 0) then
            call glb_merc (wrk, btm, dmn, dmx, m, n, gln, glt, bl,
     *                     br, tl, tr, i1, i2, j1, j2, node_eq, 
     *                     pos1, pos2, pos3, pos4, nslc, iamap,
     *                     MX_AMAP, cntr_int, do_cntr, lntr, 
     *                     z_lev, spval)
         else
            call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                    bl, br, tl, tr, pos1, pos2, pos3,
     *                    pos4, iamap, MX_AMAP)
            call contour_fld (wrk, dmn, dmx, m, n, i1, i2, j1,
     *                        j2, nslc, iamap, MX_AMAP, btm_msk,
     *                        cntr_int, .false., do_cntr, .true.,
     *                        .true., .false., lntr, spval)
         endif
c
c        ..plot titles
c
         write (title, '(''SST Diurnal Anomaly (C)'')')
         call title_plot (title, .018, 1, 2.1)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title)
         write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
         call title_plot (title, .018, 1, 1.)
         call frame
      enddo
c
c     ..clean up
c
      deallocate (btm, btm_msk, iamap, sst, wrk)
c
      return
      end
