      subroutine plot_ssh (dir_path, clim_dir, date, dtg, tau, nest,
     *                     m, n, depth, mask, igrid, rlat, stdlt1, 
     *                     stdlt2, stdlon, bl, br, tl, tr, i1, i2,
     *                     j1, j2, ssh_mn, ssh_mx, do_ssh, do_clm, 
     *                     do_clm_anm, do_err, do_inc, ssh_opt,
     *                     gln, glt, node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_ssh
c
c DESCRIPTION:  maps a forecast ssh field and color fills the contours
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
      character clim_dir * (*)
      real      cntr_int
      character date * 15
      character date_dtg * 15
      real      depth (m * n)
      character dir_path * (*)
      real      dmx, dmn
      logical   do_btm
      logical   do_clm
      logical   do_clm_anm
      logical   do_cntr
      logical   do_err
      logical   do_inc
      logical   do_lbl
      logical   do_ssh
      character dtg * 10
      logical   fail
      character file_dtg * 10
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
      character month (12) * 3
      integer   n_nodes
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   n_pass
      integer   nslc
      real      pos1, pos2, pos3, pos4
      character plt_name * 80
      real      rlat
      real      spval 
      real      ssh_mn, ssh_mx
      integer   ssh_opt
      integer   ssh_tau
      real      stdlon
      real      stdlt1, stdlt2
      integer   status
      integer   tau
      character title1 * 132
      character title2 * 132
      real      tl(2), tr(2)
      integer   z_lev
c
      integer   year, mon, day, zulu
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      real,     allocatable :: mean (:)
      real,     allocatable :: wrk (:)
c
      include 'color_table.h'
c
c     ..define map position in plot frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
c     ..initialization
c
      fld_name = 'seahgt'
      lvl_typ = 'sfc'
      fluid = 'o'
      l = 1
      n_nodes = m * n
      n_pass = 2
      ssh_tau = 24
      z_lev = 0
c
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n))
c
c     ..set pooled satellite file name, open gks
c
      call clsgks
      write (plt_name, '(''seahgt_'', a, ''.'', a, ''.gmeta '')')
     *       lvl_typ, dtg
      fno = 0
      call init_gks ('opn', plt_name)
c
c     ..loop over field types
c
      do k = 0, 6
         fail = .true.
         if (k .eq. 0 .and. do_ssh) then
c
c           ..ssh forecast
c
            file_typ = 'fcstfld'
            call dtgmod (dtg, -ssh_tau, file_dtg, status)
            call rd_coda_file (dir_path, file_dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, ssh_tau,
     *                         lvl_typ, wrk, .true., fail)
c           call gks_color (rgb_anm_clr, MX_ANM_CLR)
c           nslc = n_slca
c           dmn = ssh_mn
c           dmx = ssh_mx
c           lntr = 5
c******************************
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            nslc = n_slc
            dmn = ssh_mn
            dmx = ssh_mx
            lntr = 10
c******************************
            cntr_int = 0.2
c           cntr_int = 0.1
            do_cntr = .true.
            do_lbl = .true.
c
            read (file_dtg(1:4),  '(i4)') year
            read (file_dtg(5:6),  '(i2)') mon
            read (file_dtg(7:8),  '(i2)') day
            read (file_dtg(9:10), '(i2)') zulu
            write (date_dtg, '(i2.2, 1x, a, 1x, i4, 1x, i2.2, ''Z'')')
     *             day, month(mon), year, zulu
            write (title1, '(''SSH Forecast (M)'')')
            write (title2, '(a, 2x, ''Tau '', i3.3)') date_dtg, ssh_tau
         else if (k .eq. 1 .and. do_ssh) then
c
c           ..ssh analysis
c
            file_typ = 'analfld'
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau,
     *                         lvl_typ, wrk, .true., fail)
            if (ssh_opt .eq. 1) then
               write (title1, '(''ADT SSH Analysis (M)'')')
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               nslc = n_slc
               dmn = ssh_mn
               dmx = ssh_mx
c******************
      write (*,'(''ssh min: '',f10.2)') minval (wrk, mask=wrk.gt.spval)
      write (*,'(''ssh max: '',f10.2)') maxval (wrk)
c*********************
            else if (ssh_opt .eq. 2) then
               write (title1, '(''SLA SSH Analysis (M)'')')
               call gks_color (rgb_anm_clr, MX_ANM_CLR)
               nslc = n_slca
               dmn = -0.9
               dmx =  0.9
c*********************
      dmn = -0.3
      dmx =  0.3
c************************
            endif
            write (title2, '(a, 2x, ''Tau '', i3.3)') date, tau
            cntr_int = 0.2
            do_cntr = .true.
            do_lbl = .true.
            lntr = 10
         else if (k .eq. 2 .and. tau .eq. 0 .and. do_inc) then
c
c           ..ssh analyzed increment
c
            file_typ = 'analinc'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk, 
     *                         .true., fail)
            if (ssh_opt .eq. 1) then
               write (title1, '(''ADT SSH Analysis Increment (M)'')')
            else if (ssh_opt .eq. 2) then
               write (title1, '(''SLA SSH Analysis Increment (M)'')')
            endif
            write (title2, '(a, 2x, ''Tau '', i3.3)') date, tau
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = 0.1
            do_cntr = .false.
            do_lbl = .true.
            dmn = -0.9
            dmx =  0.9
            lntr = 5
            nslc = n_slca
         else if (k .eq. 3 .and. tau .eq. 0 .and. do_err) then
c
c           ..ssh prediction error
c
            file_typ = 'fcsterr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk,
     *                         .true., fail)
            if (ssh_opt .eq. 1) then
               write (title1, '(''ADT SSH Prediction Error (M)'')')
            else if (ssh_opt .eq. 2) then
               write (title1, '(''SLA SSH Prediction Error (M)'')')
            endif
            write (title2, '(a, 2x, ''Tau '', i3.3)') date, tau
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 0.1
            do_cntr = .false.
            do_lbl = .true.
            dmn = 0.
            dmx = 0.3
            lntr = 5
            nslc = n_slce
         else if (k .eq. 4 .and. tau .eq. 0 .and. do_err) then
c
c           ..ssh analysis error
c
            file_typ = 'analerr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk,
     *                         .true., fail)
            do i = 1, (m * n)
               wrk(i) = wrk(i) * 100.
            enddo
            if (ssh_opt .eq. 1) then
               write (title1, '(''ADT SSH Analysis Error '',
     *                          '' Reduction (%)'')')
            else if (ssh_opt .eq. 2) then
               write (title1, '(''SLA SSH Analysis Error '',
     *                          '' Reduction (%)'')')
            endif
            write (title2, '(a, 2x, ''Tau '', i3.3)') date, tau
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
c           ..ssh climate error
c
            file_typ = 'climerr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau,
     *                         lvl_typ, wrk, .true., fail)
            write (title1, '(''SSH Climate Variability (M)'')')
            write (title2, '(a, 2x, ''Tau '', i3.3)') date, tau
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 0.01
            do_cntr = .false.
            do_lbl = .true.
            dmn = 0.
            dmx = 0.6
            lntr = 5
            nslc = n_slce
         else if (k .eq. 6 .and. do_clm_anm) then
c
c           ..ssh climate anomaly
c
            allocate (lat (m * n))
            allocate (lon (m * n))
            allocate (mean (m * n))
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         'datafld', 'grdlat', 'o', 0,
     *                         'sfc', lat, .true., fail)
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         'datafld', 'grdlon', 'o', 0,
     *                         'sfc', lon, .true., fail)
            call rd_ssh_mean (clim_dir, n_nodes, n_nodes, lat, lon,
     *                        mean)
            file_typ = 'fcstfld'
            call dtgmod (dtg, -ssh_tau, file_dtg, status)
            call rd_coda_file (dir_path, file_dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, ssh_tau,
     *                         lvl_typ, wrk, .true., fail)
            do i = 1, (m * n)
               if (mask(i) .gt. 0) then
                  wrk(i) = wrk(i) - mean(i)
               else
                  wrk(i) = spval
               endif
            enddo 
            write (title1, '(''SSH Climate Anomaly (M)'')')
            write (title2, '(a, 2x, ''Tau '', i3.3)') date, tau
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = 0.01
            do_cntr = .false.
            do_lbl = .true.
            dmn = -0.3
            dmx =  0.3
            lntr = 5
            nslc = n_slca
            deallocate (lat, lon, mean)
         endif
         if (.not. fail) then
c
c           ..smooth field
c
c           if (ssh_opt .eq. 2) then
               call smth_2d (n_pass, wrk, m, n, spval)
c           endif
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
c           ..set plotting mask
c
            do i = 1, n_nodes
               if (mask(i) .eq. 0) wrk(i) = spval
            enddo
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
     *                           cntr_int, do_btm, do_cntr, .false.,
     *                           do_lbl, .false., lntr, spval)
            endif
            call title_plot (title1, .018, 1, 2.1)
            fno = fno + 1
            write (*, '(10x, ''frame'', i5, '': '', a)')
     *             fno, trim (title1)
            call title_plot (title2, .018, 1, 1.)
            call frame
         endif
      enddo
c
c     ..close and reset gks
c
      call init_gks ('cls', plt_name)
      call opngks
c
c     ..clean up
c
      deallocate (btm_msk, iamap, wrk)
c
      return
      end
