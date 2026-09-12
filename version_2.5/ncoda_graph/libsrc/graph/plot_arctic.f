      subroutine plot_arctic (dir_path, date, dtg, tau, nest, m, n, l,
     *                        mask, igrid, pln, plt, node_nh, fno,
     *                        spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_arctic
c
c DESCRIPTION:  maps surface analysis fields on tri-polar grid
c               across the arctic
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
      integer   m, n, l
      integer   pln, plt
c
      real      dmn, dmx
      character date * 15
      character dir_path * (*)
      character dtg * 10
      logical   fail
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j, k
      integer   igrid
      integer   lntr
      character lvl_typ * 3
      integer   mask (m * n)
      integer   n_nodes
      integer   n_sfc
      integer   nest
      real      node_nh ((pln * plt), 2)
      integer   nslc
      real      pos1, pos2, pos3, pos4
      character sfc_typ * 3
      real      spval
      integer   tau
      character title1 * 132
      character title2 * 132
c
c     ..allocatable arrays
c
      integer,  allocatable :: iamap (:)
      real,     allocatable :: wrk2 (:)
      real,     allocatable :: wrk3 (:)
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
c     ..check for HYCOM global grid
c
      if (igrid .gt. 0) return
c
c     ..initialization
c
      fluid = 'o'
      lvl_typ = 'pre'
      n_nodes = m * n
      n_sfc = 1
      sfc_typ = 'sfc'
      write (title2, '(a, 2x, ''Tau '', i3.3)') date, tau
c
c     ..allocate arrays
c
      allocate (iamap (MX_AMAP))
      allocate (wrk2 (m * n))
      allocate (wrk3 (m * n * l))
c
c     ..loop over surface variables
c
      do j = 1, 3
      do k = 1, 2
      if (j .eq. 1) then
         fld_name = 'icecov' 
         if (k .eq. 1) then 
            title1 = 'Sea Ice Concentration (%)' 
            file_typ = 'analfld'
            call rd_coda_file (dir_path, dtg, nest, m, n, n_sfc,
     *                         file_typ, fld_name, fluid, tau,
     *                         sfc_typ, wrk2, .true., fail)
            dmn = 0.
            dmx = 100.
            lntr = 1
         else if (k .eq. 2) then
            title1 = 'Sea Ice Concentration Increments (%)'
            file_typ = 'analinc'
            call rd_coda_file (dir_path, dtg, nest, m, n, n_sfc,
     *                         file_typ, fld_name, fluid, tau,
     *                         sfc_typ, wrk2, .true., fail)
            dmn = -30.
            dmx =  30.
            lntr = 2
         endif
      else if (j .eq. 2) then
c
c        ..sea surface temperature
c
         fld_name = 'seatmp'
         if (k .eq. 1) then
            title1 = 'Sea Surface Temperature (C)'
            file_typ = 'analfld'
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau, 
     *                         lvl_typ, wrk3, .true., fail)
            dmn = -2.
            dmx = 13.
            lntr = 4
            do i = 1, (m * n)
               wrk2(i) = wrk3(i)
            enddo
         else if (k .eq. 2) then
            title1 = 'Sea Surface Temperature Increments (C)'
            file_typ = 'analinc'
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau,
     *                         lvl_typ, wrk3, .true., fail)
            dmn = -1.5
            dmx =  1.5
            lntr = 5
            do i = 1, (m * n)
               wrk2(i) = wrk3(i)
            enddo
         endif
      else if (j .eq. 3) then
c
c        ..sea surface salinity
c
         fld_name = 'salint'
         if (k .eq. 1) then
            title1 = 'Sea Surface Salinity (PSU)'
            file_typ = 'analfld'
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau,
     *                         lvl_typ, wrk3, .true., fail)
            dmn = 30. 
            dmx = 36.
            lntr = 10
            do i = 1, (m * n)
               wrk2(i) = wrk3(i)
            enddo
         else if (k .eq. 2) then
            title1 = 'Sea Surface Salinity Increments (PSU)'
            file_typ = 'analinc'
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau,
     *                         lvl_typ, wrk3, .true., fail)
            dmn = -0.6
            dmx =  0.6
            lntr = 5
            do i = 1, (m * n)
               wrk2(i) = wrk3(i)
            enddo
         endif
      endif
c
      if (.not. fail) then
         if (k .eq. 1) then
            if (j .eq. 1) then
               call plot_polar_data ('ICE', dir_path, date, dtg, nest,
     *                               'nh', m, n, fno)
            else if (j .eq. 2) then
               call plot_polar_data ('SST', dir_path, date, dtg, nest,
     *                               'nh',  m, n, fno)
            else if (j .eq. 3) then
               call plot_polar_data ('SSS', dir_path, date, dtg, nest,
     *                               'nh', m, n, fno)
            endif
         endif
c
c        ..mask field
c
         do i = 1, n_nodes
            if (mask(i) .eq. 0) wrk2(i) = spval
         enddo
c
c        ..set color scheme
c
         if (j .eq. 1) then
            if (k .eq. 1) then
               call gks_color (rgb_ice_clr, MX_ICE_CLR)
               nslc = n_slci
            else if (k .eq. 2) then
               call gks_color (rgb_ice_anm, MX_ICE_ANM)
               nslc = n_slcia
            endif
         else
            if (k .eq. 1) then
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               nslc = n_slc
            else if (k .eq. 2) then
               call gks_color (rgb_anm_clr, MX_ANM_CLR)
               nslc = n_slca
            endif
         endif
c
c        ..plot field
c
         call glb_polar (wrk2, dmn, dmx, m, n, pln, plt, node_nh,
     *                   pos1, pos2, pos3, pos4, nslc, iamap,
     *                   MX_AMAP, lntr, 'nh', spval)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title1)
         call title_plot (title1, .018, 1, 2.1)
         call title_plot (title2, .018, 1, 1.)
         call frame
      endif
      enddo
      enddo
c
c     ..clean up
c
      deallocate (iamap, wrk2, wrk3)
c
      return
      end
