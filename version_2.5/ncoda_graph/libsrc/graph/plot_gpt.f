      subroutine plot_gpt (dir_path, date, dtg, tau, nest, m, n, l, 
     *                     depth, lvl, mask, igrid, rlat, stdlt1, 
     *                     stdlt2, stdlon, bl, br, tl, tr, i1, i2,
     *                     j1, j2, nz, z_plot, do_gpt, do_inc,
     *                     gln, glt, node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_gpt
c
c DESCRIPTION:  maps a CODA 3D geopotential analysis and color fills
c               the contours
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
      integer   nz
      integer   gln, glt
c
      real      bl (2), br (2)
      real      cntr_int
      real      dmn, dmx
      character date * 15
      real      depth (m * n)
      character dir_path * (*)
      logical   do_btm
      logical   do_cntr
      logical   do_gpt
      logical   do_inc
      character dtg * 10
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
      integer   n_nodes
      integer   ndx
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   nslc
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      character title * 132
      real      tl (2), tr (2)
      integer   z_lev
      integer   z_plot (50)
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
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (fld (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n * l))
c
c     ..initialize
c
      fld_name = 'geoptl'
      fluid = 'o'
      lvl_typ = 'pre'
      n_nodes = m * n
c
c     ..loop over field types
c
      do k = 1, 2
c
c     ..set analysis variable
c
      fail = .true.
      if (k .eq. 1 .and. do_gpt) then
         file_typ = 'analfld'
         call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, wrk,
     *                      .true., fail)
         call gks_color (rgb_fld_clr, MX_FLD_CLR)
         do_cntr = .true.
         cntr_int = 0.2
         dmn = 0.
         dmx = 2.4
         lntr = 5
         nslc = n_slc
       else if (k .eq. 2 .and. do_inc) then
         file_typ = 'analinc'
         call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, wrk,
     *                      .true., fail)
         call gks_color (rgb_anm_clr, MX_ANM_CLR)
         do_cntr = .false.
         cntr_int = 0.2
         dmn = -0.25
         dmx =  0.25
         lntr = 5
         nslc = n_slca
      endif
c
      if (.not. fail) then
c
c        ..set plotting mask
c
         do j = 1, l
            do i = 1, (m * n)
               ndx = i + (j-1) * m * n
               if (j .gt. mask(i)) wrk(ndx) = spval
            enddo
         enddo
c
c        ..loop over selected plot levels
c
         do j = 1, nz
            z_lev = int (lvl(z_plot(j)) + .001)
c
c           ..set bottom mask
c
            do_btm = .true.
            do i = 1, n_nodes
               if (depth(i) .ge. 1.01 .and.
     *             depth(i) .lt. (lvl(z_plot(j))+1.01)) then
                  btm_msk(i) = -1.
               else
                  btm_msk(i) = 1.
               endif
            enddo
c
c           ..extract field
c
            do i = 1, n_nodes
               ndx = i + (z_plot(j) - 1) * m * n
               fld(i) = wrk(ndx)
            enddo
c
c           ..set plot title
c
            if (k .eq. 1) then
               write (title, '(''Geopotential (M)'', 4x, i4,
     *                         '' M Depth'')') z_lev
            else
               write (title, '(''Geopotential Analyzed Increment '',
     *                         ''(M)'', 4x, i4, '' M Depth'')') z_lev
            endif
c
c           ..plot field
c
            if (igrid .lt. 0) then
               call glb_merc (fld, depth, dmn, dmx, m, n, gln,
     *                        glt, bl, br, tl, tr, i1, i2, j1,
     *                        j2, node_eq, pos1, pos2, pos3,
     *                        pos4, nslc, iamap, MX_AMAP, 
     *                        cntr_int, do_cntr, lntr, 
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
            call title_plot (title, .018, 1, 2.1)
            fno = fno + 1
            write (*, '(10x, ''frame'', i5, '': '', a)')
     *             fno, trim (title)
            write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
            call title_plot (title, .018, 1, 1.)
            call frame
         enddo
      endif
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, fld, iamap, wrk)
c
      return
      end
