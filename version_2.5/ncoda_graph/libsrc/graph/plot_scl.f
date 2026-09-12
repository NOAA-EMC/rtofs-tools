      subroutine plot_scl (dir_path, date, dtg, tau, nest, m, n, l,
     *                     lvl, mask, igrid, rlat, stdlt1, stdlt2,
     *                     stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *                     gln, glt, node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_scl
c
c DESCRIPTION:  maps CODA 2D and 3D correlation length scales
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
      integer    UNIT
      parameter (UNIT = 10)
c
c     ..local array dimensions
c
      integer   m, n, l
      integer   gln, glt
c
      real      bl(2), br(2)
      real      cntr_int
      real      dmn, dmx
      character date * 15
      character dir_path * (*)
      logical   do_btm
      logical   do_cntr
      logical   do_lbl
      character dtg * 10
      character err_msg * 256
      logical   fail
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j, k
      integer   i1, i2, j1, j2
      integer   igrid
      integer   lntr
      real      lvl (100)
      character lvl_typ * 3
      integer   mask (m * n)
      integer   ndx
      integer   nest
      integer   nk
      real      node_eq ((gln * glt), 2)
      integer   nz
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
      real,     allocatable :: scl_2d (:)
      real,     allocatable :: scl_3d (:)
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
      allocate (scl_2d (m * n))
      allocate (scl_3d (m * n * l))
      allocate (wrk (m * n))
c
c     ..set dummy land and bottom masks
c
      do_btm = .false.
      do i = 1, (m * n)
         btm(i) = 0.
         btm_msk(i) = 1.
      enddo
c
c     ..initialize 
c
      file_typ = 'datafld'
      fluid = 'o'
      nk = 1
      nz = 1
      tau = 0
c
c     ..read 2D horizontal length scales
c
      fld_name = 'sclhor'
      lvl_typ = 'sfc'
      call rd_coda_file (dir_path, dtg, nest, m, n, nz, file_typ,
     *                   fld_name, fluid, tau, lvl_typ, scl_2d,
     *                   .true., fail)
      if (fail) then
         write (err_msg, '(''horizontal scale field missing: '',
     *           a)') dtg
         call error_exit ('PLOT_SCL', err_msg)
      endif
c
c     ..set 2D plotting mask
c
      do i = 1, (m * n)
         if (mask(i) .eq. 0) scl_2d(i) = spval
      enddo
c
      if (l .gt. 1) then
c
c        ..read 3D horizontal length scales
c
         fld_name = 'sclvrt'
         lvl_typ = 'pre'
         call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, scl_3d,
     *                      .true., fail)
         if (fail) then
            write (err_msg, '(''vertical scale field missing: '',
     *              a)') dtg
            call error_exit ('PLOT_SCL', err_msg)
         endif
c
c        ..set 3D plotting mask
c
         nk = 2
         do j = 1, l
            do i = 1, (m * n)
               ndx = i + (j-1) * m * n
               if (j .gt. mask(i)) scl_3d(ndx) = spval
            enddo
         enddo
      endif
c
c     ..set gks and plot environment
c
      call gks_color (rgb_fld_clr, MX_FLD_CLR)
      dmn = 0.
      do_cntr = .true.
      do_lbl = .true.
      lntr = 5
c
c     ..loop over field types (2D and 3D)
c
      do k = 1, nk
         if (k .eq. 1) nz = 1
         if (k .eq. 2) nz = l
c
c        ..loop over levels
c
         do j = 1, nz
c
c           ..extract field
c
            if (k .eq. 1) then
               do i = 1, (m * n)
                  wrk(i) = scl_2d(i)
               enddo
               cntr_int = 30. 
               dmx = 120.
            else if (k .eq. 2) then
               do i = 1, (m * n)
                  ndx = i + (j-1) * m * n
                  wrk(i) = scl_3d(ndx)
               enddo
               cntr_int = 30. 
               dmx = 120.
            endif
c
c           ..set plot title
c
            if (k .eq. 1) then
               write (title, '(''Rossby Radius (KM)'')')
            else if (k .eq. 2) then
               z_lev = int (lvl(j) + .001)
               write (title, '(''Vertical Correlation Length Scales '',
     *                         '' (M)'', 4x, i4, '' M Depth'')') z_lev
            endif
c
c           ..plot field
c
            if (igrid .lt. 0) then
               call glb_merc (wrk, btm, dmn, dmx, m, n, gln, glt, bl, 
     *                        br, tl, tr, i1, i2, j1, j2, node_eq,
     *                        pos1, pos2, pos3, pos4, n_slc, iamap,
     *                        MX_AMAP, cntr_int, do_cntr, lntr,
     *                        z_lev, spval)
            else
               call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3,
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (wrk, dmn, dmx, m, n, i1, i2, j1,
     *                           j2, n_slc, iamap, MX_AMAP, btm_msk,
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
         enddo
      enddo
c
c     ..clean up
c
      deallocate (btm, btm_msk, iamap, scl_2d, scl_3d, wrk)
c
      return
      end
