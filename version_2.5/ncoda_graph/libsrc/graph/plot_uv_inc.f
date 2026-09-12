      subroutine plot_uv_inc (dir_path, date, dtg, nest, m, n, l,
     *                        depth, lvl, mask, igrid, rlat, stdlt1,
     *                        stdlt2, stdlon, bl, br, tl, tr, i1, i2, 
     *                        j1, j2, nz, z_plot, gln, glt, node_eq,
     *                        lyr, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_uv_inc
c
c DESCRIPTION:  maps NCODA u,v velocity increments
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
      integer   m, n, l
      integer   nz
      integer   gln, glt
c
      real      bl (2), br (2)
      real      cntr_int
      character date * 15
      real      depth (m * n)
      character dir_path * (*)
      real      dmn, dmx
      logical   do_btm
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
      logical   lyr
      integer   mask (m * n)
      integer   ndx
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   nslc
      integer   nl
      character plt_name * 80
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
      real,     allocatable :: btm_msk (:,:)
      real,     allocatable :: fld (:,:)
      real,     allocatable :: grd (:)
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
c     ..allocate arrays
c
      allocate (btm_msk (m, n))
      allocate (fld (m, n))
      allocate (grd (m * n * l))
      allocate (iamap (MX_AMAP))
c
c     ..initialize
c
      file_typ = 'analinc'
      fluid = 'o'
      if (lyr) then
         lvl_typ = 'lyr'
      else
         lvl_typ = 'pre'
      endif
c
c     ..set plot file name, open gks
c
      call clsgks
      write (plt_name, '(''uvincr_'', a, ''.'', a, ''.gmeta '')')
     *       lvl_typ, dtg
      fno = 0
      call init_gks ('opn', plt_name)
c
c     ..set plot parameters
c
      call gks_color (rgb_anm_clr, MX_ANM_CLR)
      cntr_int = 2.
      dmn = -30.
      dmx =  30.
      lntr = 5
      nslc = n_slca
      tau = 0
c
c     ..loop over analysis field types
c
      do k = 1, 2
         if (k .eq. 1) then
c
c           ..uuu velocity increments
c
            fld_name = 'uucurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd,
     *                         .true., fail)
            do i = 1, (m * n * l)
               grd(i) = grd(i) * 1.e2
            enddo
         else if (k .eq. 2) then
c
c           ..vvv velocity increments
c
            fld_name = 'vvcurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd,
     *                         .true., fail)
            do i = 1, (m * n * l)
               grd(i) = grd(i) * 1.e2
            enddo
         endif
         if (fail) cycle
c
c-----------------------------------------------------------------------
c
c        ..loop over analysis levels
c
         do nl = 1, nz
         z_lev = int (lvl(z_plot(nl)) + .001)
c
c        ..set bottom mask
c
         do_btm = .true.
         do j = 1, n
            do i = 1, m
               ndx = m * (j-1) + i
               if (depth(ndx) .ge. 1.01 .and.
     *             depth(ndx) .lt. (lvl(z_plot(nl))+1.01)) then
                  btm_msk(i,j) = -1.
               else
                  btm_msk(i,j) = 1.
               endif
            enddo
         enddo
c
c        ..set plotting mask
c
         do j = 1, l
            do i = 1, (m * n)
               ndx = i + (j-1) * m * n
               if (j .gt. mask(i)) grd(ndx) = spval
            enddo
         enddo
c
c        ..extract field
c
         do j = 1, n
            do i = 1, m
               ndx = m * (j-1) + i + (z_plot(nl)-1) * m * n
               fld(i,j) = grd(ndx)
            enddo
         enddo
c
c        ..set plot title
c
         if (k .eq. 1) then
            if (lyr) then
               write (title, '(''U Velocity Analyzed Increment '',
     *                         ''(cm/s)'', 4x, ''Layer'', i4)')
     *                         z_plot(nl)
            else
               write (title, '(''U Velocity Analyzed Increment '',
     *                         ''(cm/s)'', 4x, i4, '' M Depth'')')
     *                         z_lev
            endif
         else if (k .eq. 2) then
            if (lyr) then
               write (title, '(''V Velocity Analyzed Increment '',
     *                         ''(cm/s)'', 4x, ''Layer'', i4)')
     *                         z_plot(nl)
            else
               write (title, '(''V Velocity Analyzed Increment '',
     *                         ''(cm/s)'', 4x, i4, '' M Depth'')')
     *                         z_lev
            endif
         endif
c
c        ..plot velocity increments
c
         if (igrid .lt. 0) then
            call glb_merc (fld, depth, dmn, dmx, m, n, gln, glt,
     *                     bl, br, tl, tr, i1, i2, j1, j2, 
     *                     node_eq, pos1, pos2, pos3, pos4,
     *                     nslc, iamap, MX_AMAP, cntr_int,
     *                     .false., lntr, z_lev, spval)
         else
            call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                    bl, br, tl, tr, pos1, pos2, pos3,
     *                    pos4, iamap, MX_AMAP)
            call contour_fld (fld, dmn, dmx, m, n, i1, i2, j1,
     *                        j2, nslc, iamap, MX_AMAP, btm_msk,
     *                        cntr_int, do_btm, .false., .false.,
     *                        .false., .false., lntr, spval)
         endif
c
c        ..plot title
c
         call title_plot (title, .018, 1, 2.1)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title)
         write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
         call title_plot (title, .018, 1, 1.)
         call frame
         enddo
      enddo
c
c     ..close and reset gks
c
      call init_gks ('cls', plt_name)
      call opngks
c
c     ..clean up
c
      deallocate (btm_msk, fld, grd, iamap)
c
      return
      end
