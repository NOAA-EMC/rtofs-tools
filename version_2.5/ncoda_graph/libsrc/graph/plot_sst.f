      subroutine plot_sst (dir_path, date, dtg, tau, nest, m, n, depth,
     *                     mask, igrid, rlat, stdlt1, stdlt2, stdlon,
     *                     bl, br, tl, tr, i1, i2, j1, j2, sst_cnt,
     *                     sst_del, sst_min, do_sst, do_err, do_inc,
     *                     do_clm, gln, glt, node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_sst
c
c DESCRIPTION:  maps a CODA sst analysis and color fills the contours
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
      parameter (MX_AMAP = 120 000 000)
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
      real      depth (m * n)
      character dir_path * (*)
      logical   do_btm
      logical   do_clm
      logical   do_err
      logical   do_cntr
      logical   do_inc
      logical   do_lbl
      logical   do_sst
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
      integer   n_nodes
      integer   n_pass
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   nslc
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval 
      real      sst_cnt
      real      sst_del
      real      sst_min
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      integer   tau_hr
      character title * 132
      real      tl(2), tr(2)
      integer   z_lev
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: clm (:)
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
      fld_name = 'seatmp'
c     lvl_typ = 'pre'
c***********************
      lvl_typ = 'sfc'
c************************
      fluid = 'o'
      l = 1
      n_nodes = m * n
      n_pass = 2
      tau_hr = 0
      z_lev = 0
c
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n))
c
c     ..loop over analysis field types
c
      do k = 1, 5
         fail = .true.
         if (k .eq. 1 .and. do_sst) then
c
c           ..sst analysis
c
            if (tau .eq. 0) then
               file_typ = 'analfld'
            else
               file_typ = 'fcstfld'
            endif
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk,
     *                         .true., fail)
            write (title, '(''Sea Surface Temperature (C)'')')
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            cntr_int = sst_cnt
            if (cntr_int .gt. 0.) then
               do_cntr = .true.
            else
               do_cntr = .false.
            endif
            dmn = sst_min
            dmx = dmn + sst_del * real (n_slc)
            do_lbl = .true.
            lntr = 4
            nslc = n_slc
         else if (k .eq. 2 .and. tau .eq. 0 .and. do_inc) then
c
c           ..sst analyzed increment
c
            file_typ = 'analinc'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau_hr, lvl_typ, wrk,
     *                         .true., fail)
            write (title, '(''SST Analyzed Increment (C)'')')
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = 0.5
            do_cntr = .false.
            do_lbl = .true.
            dmn = -1.5
            dmx =  1.5
            lntr = 5
            nslc = n_slca
         else if (k .eq. 3 .and. tau .eq. 0 .and. do_err) then
c
c           ..sst prediction error
c
            file_typ = 'fcsterr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau_hr, lvl_typ, wrk,
     *                         .true., fail)
            write (title, '(''SST Prediction Error (C)'')')
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 0.2
            do_cntr = .false.
            do_lbl = .true.
            dmn = 0.
            dmx = 1.2
            lntr = 5
            nslc = n_slce
         else if (k .eq. 4 .and. tau .eq. 0 .and. do_err) then
c
c           ..sst analysis error
c
            file_typ = 'analerr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau_hr, lvl_typ, wrk,
     *                         .true., fail)
            do i = 1, (m * n)
               wrk(i) = wrk(i) * 100.
            enddo
            write (title, '(''SST Analysis Error Reduction (%)'')')
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 20.
            do_cntr = .true.
            do_lbl = .false.
            dmn = 0.
            dmx = 100.
            lntr = 6
            nslc = n_slce
         else if (k .eq. 5 .and. tau .eq. 0 .and. do_clm) then
c
c           ..sst climate anomaly
c
            file_typ = 'analfld'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau_hr, lvl_typ, wrk,
     *                         .true., fail)
            allocate (clm (m * n))
            file_typ = 'climfld'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau_hr, lvl_typ, clm,
     *                         .true., fail)
            do i = 1, (m * n)
               if (mask(i) .gt. 0) then
                  wrk(i) = wrk(i) - clm(i)
               else
                  wrk(i) = spval
               endif
            enddo 
            deallocate (clm)
            write (title, '(''SST Climate Anomaly (C)'')')
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = sst_cnt
            do_cntr = .false.
            do_lbl = .false.
            dmn = -3.
            dmx =  3.
            lntr = 5
            nslc = n_slca
         endif
         if (.not. fail) then
c
c           ..set no bottom mask
c
            do_btm = .false.
            do i = 1, n_nodes
               if (depth(i) .lt. 1.01) then
                  btm_msk(i) = -1.
               else
                  btm_msk(i) = 1.
               endif
            enddo
c
c           ..set plotting mask
c
            do i = 1, n_nodes
               if (mask(i) .eq. 0) wrk(i) = spval
            enddo
c           call smth_2d (n_pass, wrk, m, n, spval)
c
c           ..plot field
c
            if (igrid .lt. 0) then
               call glb_merc (wrk, depth, dmn, dmx, m, n, gln, glt,
     *                        bl, br, tl, tr, i1, i2, j1, j2, node_eq,
     *                        pos1, pos2, pos3, pos4, nslc, iamap, 
     *                        MX_AMAP, cntr_int, do_cntr, lntr, z_lev,
     *                        spval)
            else
               call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3,
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (wrk, dmn, dmx, m, n, i1, i2, j1,
     *                           j2, nslc, iamap, MX_AMAP, btm_msk,
     *                           cntr_int, do_btm, do_cntr, .true.,
     *                           do_lbl, .false., lntr, spval)
            endif
            call title_plot (title, .018, 1, 2.1)
            fno = fno + 1
            write (*, '(10x, ''frame'', i5, '': '', a)')
     *             fno, trim (title)
            write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
            call title_plot (title, .018, 1, 1.)
            call frame
         endif
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, iamap, wrk)
c
      return
      end
