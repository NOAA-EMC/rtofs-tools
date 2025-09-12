      subroutine plot_ice (dir_path, date, dtg, tau, nest, m, n, depth,
     *                     mask, igrid, rlat, stdlt1, stdlt2, stdlon,
     *                     bl, br, tl, tr, i1, i2, j1, j2, do_ice,
     *                     do_err, do_inc, pln, plt, node_nh, node_sh,
     *                     fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_ice
c
c DESCRIPTION:  maps a CODA ice analysis and color fills the contours
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
      integer    UNIT
      parameter (UNIT = 10)
c
c     ..local array dimensions
c
      integer   m, n
      integer   pln, plt
c
      real      bl(2), br(2)
      real      cntr_int
      real      dmn, dmx
      character date * 15
      real      depth (m * n)
      character dir_path * (*)
      logical   do_btm
      logical   do_err
      logical   do_ice
      logical   do_inc
      character dtg * 10
      logical   fail
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j, k, l
      integer   i1, i2, j1, j2
      integer   igrid
      integer   lntr
      character lvl_typ * 3
      integer   mask (m * n)
      integer   n_nodes
      integer   nest
      real      node_nh ((pln * plt), 2)
      real      node_sh ((pln * plt), 2)
      integer   nslc
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      integer   tau_hr
      character title1 * 132
      character title2 * 132
      real      tl(2), tr(2)
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: fld (:)
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
      cntr_int = -1.
      lvl_typ = 'sfc'
      fluid = 'o'
      l = 1
      n_nodes = m * n
      tau_hr = 0
      write (title2, '(a, 2x, ''Tau '', i3.3)') date, tau
c
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (fld (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n))
c
c     ..loop over ice analysis variables
c
      do j = 1, 3
      if (j .eq. 1) then
         fld_name = 'icecov'
      else if (j .eq. 2) then
         fld_name = 'icethk'
      else if (j .eq. 3) then
         fld_name = 'icetmp'
      endif
c
c     ..loop over analysis field types
c
      do k = 1, 4
         fail = .true.
         if (k .eq. 1 .and. do_ice) then   
c
c           ..ice analysis
c
            if (tau .eq. 0) then
               file_typ = 'analfld'
            else
               file_typ = 'fcstfld'
            endif
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, fld, 
     *                         .true., fail)
      write (*,'(''ice mn: '', f10.2)') minval (fld, mask=fld.gt.spval)
      write (*,'(''ice mx: '', f10.2)') maxval (fld)
            if (j .eq. 1) then
               write (title1, '(''Sea Ice Concentration (%)'')')
               call gks_color (rgb_ice_clr, MX_ICE_CLR)
               nslc = n_slci
               dmn = 0.
               dmx = 100.
               lntr = 1
            else if (j .eq. 2) then
               write (title1, '(''Sea Ice Thickness (M)'')')
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               nslc = n_slc
               dmn = 0.
               dmx = 3.
               lntr = 10
            else if (j .eq. 3) then
               write (title1, '(''Sea Ice Temperature (C)'')')
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               nslc = n_slc
               dmn = -25.
               dmx = 5.
               lntr = 4
            endif
         else if (k .eq. 2 .and. do_inc .and. tau .eq. 0) then
c
c           ..ice analyzed increments
c
            file_typ = 'analinc'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau_hr, lvl_typ, fld,
     *                         .true., fail)
            if (j .eq. 1) then
               write (title1, '(''Sea Ice Concentration '',
     *                          ''Increments (%)'')')
               call gks_color (rgb_ice_anm, MX_ICE_ANM)
               cntr_int = 10.
               dmn = -30.
               dmx =  30.
               lntr = 3
               nslc = n_slcia
            else if (j .eq. 2) then
               write (title1, '(''Sea Ice Thickness '',
     *                          ''Increments (M)'')')
               call gks_color (rgb_anm_clr, MX_ANM_CLR)
               nslc = n_slca
               dmn = -1.2
               dmx =  1.2
               lntr = 5
            else if (j .eq. 3) then
               write (title1, '(''Sea Ice Temperature '',
     *                          ''Increments (C)'')')
               call gks_color (rgb_anm_clr, MX_ANM_CLR)
               nslc = n_slca
               dmn = -6.
               dmx =  6.
               lntr = 5
            endif
         else if (k .eq. 3 .and. do_err .and. tau .eq. 0) then
c
c           ..ice forecast error
c
            file_typ = 'fcsterr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau_hr, lvl_typ, fld, 
     *                         .true., fail)
            if (j .eq. 1) then
               write (title1, '(''Sea Ice Concentration '',
     *                          ''Prediction Error (%)'')')
               call gks_color (rgb_ice_clr, MX_ICE_CLR)
               dmn = 0.
               dmx = 30.
               lntr = 1
               nslc = n_slci
            else if (j .eq. 2) then
               write (title1, '(''Sea Ice Thickness '',
     *                          ''Prediction Error (M)'')')
               call gks_color (rgb_err_clr, MX_ERR_CLR)
               dmn = 0.
               dmx = 3.
               lntr = 5
               nslc = n_slce
            else if (j .eq. 3) then
               write (title1, '(''Sea Ice Temperature '',
     *                          ''Prediction Error (C)'')')
               call gks_color (rgb_err_clr, MX_ERR_CLR)
               dmn = 0.
               dmx = 6.
               lntr = 5
               nslc = n_slce
            endif
         else if (k .eq. 4 .and. do_err .and. tau .eq. 0) then
c
c           ..ice analysis error
c
            file_typ = 'analerr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau_hr, lvl_typ, fld,
     *                         .true., fail)
            if (j .eq. 1) then
               write (title1, '(''Sea Ice Concentration '',
     *                          ''Analysis Error (%)'')')
            else if (j .eq. 2) then
               write (title1, '(''Sea Ice Thickness '',
     *                          ''Analysis Error (%)'')')
            else if (j .eq. 3) then
               write (title1, '(''Sea Ice Temperature '',
     *                          ''Analysis Error (%)'')')
            endif
            call gks_color (rgb_ice_clr, MX_ICE_CLR)
            do i = 1, (m * n)
               fld(i) = fld(i) * 100.
            enddo
            dmn = 0.
            dmx = 100.
            lntr = 1
            nslc = n_slci
         endif
         if (.not. fail) then
c
c           ..set bottom mask
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
c           ..mask field
c
            do i = 1, n_nodes
               if (mask(i) .eq. 0) fld(i) = spval
            enddo
c
c           ..plot field
c
            if (igrid .lt. 0) then
c******************
      write (*,'(''calling glb_polar nh'')')
c********************
               wrk = fld
               call glb_polar (wrk, dmn, dmx, m, n, pln, plt, 
     *                         node_nh, pos1, pos2, pos3, pos4,
     *                         nslc, iamap, MX_AMAP, lntr, 'nh',
     *                         spval)
               fno = fno + 1
               write (*, '(10x, ''frame'', i5, '': '', a)')
     *                fno, trim (title1)
               call title_plot (title1, .018, 1, 2.1)
               call title_plot (title2, .018, 1, 1.)
               call frame
c
c******************
      write (*,'(''calling glb_polar sh'')')
c********************
               wrk = fld
               call glb_polar (wrk, dmn, dmx, m, n, pln, plt,
     *                         node_sh, pos1, pos2, pos3, pos4,
     *                         nslc, iamap, MX_AMAP, lntr, 'sh',
     *                         spval)
               fno = fno + 1
               write (*, '(10x, ''frame'', i5, '': '', a)')
     *                fno, trim (title1)
               call title_plot (title1, .018, 1, 2.1)
               call title_plot (title2, .018, 1, 1.)
               call frame
            else
               call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3,
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (fld, dmn, dmx, m, n, i1, i2, j1, j2,
     *                           nslc, iamap, MX_AMAP, btm_msk,
     *                           cntr_int, do_btm, .false., .false.,
     *                           .false., .false., lntr, spval)
               fno = fno + 1
               write (*, '(10x, ''frame'', i5, '': '', a)')
     *                fno, trim (title1)
               call title_plot (title1, .018, 1, 2.1)
               call title_plot (title2, .018, 1, 1.)
               call frame
            endif
         endif
      enddo
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, fld, iamap, wrk)
c
      return
      end
