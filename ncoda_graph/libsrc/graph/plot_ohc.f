      subroutine plot_ohc (dir_path, date, dtg, nest, m, n, igrid, 
     *                     rlat, stdlt1, stdlt2, stdlon, bl, br, tl,
     *                     tr, i1, i2, j1, j2, gln, glt, node_eq,
     *                     fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_ohc
c
c DESCRIPTION:  plots ocean heat content fields including
c               mixed layer depth
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
      real      cntr_int
      real      dmx, dmn
      character date * 15
      character dir_path * (*)
      logical   do_btm
      logical   do_cntr
      character dtg * 10
      logical   fail
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, k, l
      integer   igrid
      integer   iso (2)
      integer   i1, i2, j1, j2
      integer   lntr
      character lvl_typ * 3
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   n_pass
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
      real,     allocatable :: fld (:)
      integer,  allocatable :: iamap (:)
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
      file_typ = 'analfld'
      fluid = 'o'
      iso(1) = 20
      iso(2) = 26
      l = 1
      lvl_typ = 'sfc'
      n_pass = 2
      tau = 0
      z_lev = 0
c
c     ..allocate arrays
c
      allocate (btm (m * n))
      allocate (btm_msk (m * n))
      allocate (fld (m * n))
      allocate (iamap (MX_AMAP))
c
c     ..set no bottom mask
c
      do_btm = .false.
      do i = 1, (m * n)
         btm(i) = 0.
         btm_msk(i) = 1.
      enddo
c
c     ..loop over isotherms
c
      do i = 1, 2
      if (iso(i) .gt. 0) then
c
c     ..loop over analysis field types
c
      do k = 1, 2
         fail = .true.
         if (k .eq. 1) then
c
c           ..heat content
c
            write (fld_name, '(''ocnh'', i2.2)') iso(i)
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau,
     *                         lvl_typ, fld, .true., fail)
            write (title, '(i2, '' C Heat Content (kJ/cm**2)'')') iso(i)
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            do_cntr = .true.
            if (iso(i) .eq. 20) then
               cntr_int = 180.
               dmn = -540.
               dmx =  540.
            else if (iso(i) .eq. 26) then
               cntr_int = 60.
               dmn = -180.
               dmx =  180.
            endif
            lntr = 5
            nslc = n_slca
         else if (k .eq. 2) then
c
c           ..isotherm topography
c
            write (fld_name, '(''topo'', i2.2)') iso(i)
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau,
     *                         lvl_typ, fld, .true., fail)
            write (title, '(i2, '' C Isotherm Topography (M)'')') iso(i)
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            do_cntr = .true.
            if (iso(i) .eq. 20) then
               cntr_int = 120.
               dmn = 0.
               dmx = 360.
            else if (iso(i) .eq. 26) then
               cntr_int = 60.
               dmn = 0.
               dmx = 180.
            endif
            lntr = 5
            nslc = n_slc
         endif
         if (.not. fail) then
c
c           ..smooth field
c
            call smth_2d (n_pass, fld, m, n, spval)
c
c           ..plot field
c
            if (igrid .lt. 0) then
               call glb_merc (fld, btm, dmn, dmx, m, n, gln, glt, bl,
     *                        br, tl, tr, i1, i2, j1, j2, node_eq,
     *                        pos1, pos2, pos3, pos4, nslc, iamap,
     *                        MX_AMAP, cntr_int, do_cntr, lntr,
     *                        z_lev, spval)
            else
               call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3,
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (fld, dmn, dmx, m, n, i1, i2, j1,
     *                           j2, nslc, iamap, MX_AMAP, btm_msk,
     *                           cntr_int, do_btm, do_cntr, .true.,
     *                           .false., .false., lntr, spval)
            endif
            call title_plot (title, .019, 1, 2.1)
            fno = fno + 1
            write (*, '(10x, ''frame'', i5, '': '', a)')
     *             fno, trim (title)
            write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
            call title_plot (title, .019, 1, 1.)
            call frame
         endif
      enddo
      endif
      enddo
c
c     ..ohc mixed layer depth
c
      fld_name = 'ohcmld'
      call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                   file_typ, fld_name, fluid, tau,
     *                   lvl_typ, fld, .true., fail)
      if (.not. fail) then
         write (title, '(''OHC Mixed Layer Depth (M)'')')
         call gks_color (rgb_fld_clr, MX_FLD_CLR)
         do_cntr = .true.
         cntr_int = 20.
         dmn = 0.
         dmx = 60.
         lntr = 5
         nslc = n_slc
c
c        ..plot field
c
         if (igrid .lt. 0) then
            call glb_merc (fld, btm, dmn, dmx, m, n, gln, glt, bl, 
     *                     br, tl, tr, i1, i2, j1, j2, node_eq, 
     *                     pos1, pos2, pos3, pos4, nslc, iamap,
     *                     MX_AMAP, cntr_int, do_cntr, lntr, 
     *                     z_lev, spval)
         else
            call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                    bl, br, tl, tr, pos1, pos2, pos3,
     *                    pos4, iamap, MX_AMAP)
            call contour_fld (fld, dmn, dmx, m, n, i1, i2, j1,
     *                        j2, nslc, iamap, MX_AMAP, btm_msk,
     *                        cntr_int, do_btm, do_cntr, .true.,
     *                        .false., .false., lntr, spval)
         endif
         call title_plot (title, .019, 1, 2.1)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title)
         write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
         call title_plot (title, .019, 1, 1.)
         call frame
      endif
c
c     ..clean up
c
      deallocate (btm, btm_msk, fld, iamap)
c
      return
      end
