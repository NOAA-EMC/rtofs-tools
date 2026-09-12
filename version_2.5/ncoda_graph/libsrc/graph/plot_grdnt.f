      subroutine plot_grdnt (dir_path, date, dtg, tau, nest, m, n,
     *                       l, depth, lvl, mask, igrid, delx, dely,
     *                       rlat, stdlt1, stdlt2, stdlon, bl, br,
     *                       tl, tr, i1, i2, j1, j2, nz, z_plot, 
     *                       do_sal, do_sst, do_tmp, sal_grd, 
     *                       tmp_grd, gln, glt, node_eq, fno,
     *                       spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_grdnt
c
c DESCRIPTION:  computes and maps gradient fields
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
      integer   m, n, l
      integer   gln, glt
c
      real      bl(2), br(2)
      real      cntr_int
      real      dmx, dmn
      character date * 15
      real      delx, dely
      real      depth (m * n)
      character dir_path * (*)
      logical   do_btm
      logical   do_cntr
      logical   do_sal
      logical   do_sst
      logical   do_tmp
      character dtg * 10
      real      dx, dy
      logical   fail
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j, k, kp, p
      integer   i1, i2, j1, j2
      integer   igrid
      real      lmn
      integer   lntr
      real      lvl (100)
      character lvl_typ * 3
      integer   mask (m * n)
      integer   ndx
      integer   nest
      integer   nl
      real      node_eq ((gln * glt), 2)
      integer   nodes
      integer   nslc
      integer   nz
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      sal_grd
      real      spmis
      real      spval 
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      character title * 132
      real      tl(2), tr(2)
      real      tmp_grd
      real      x, y
      integer   x1, x2, y1, y2
      integer   z_lev
      integer   z_plot (50)
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: fld (:)
      real,     allocatable :: grd (:)
      real,     allocatable :: hx (:)
      real,     allocatable :: hy (:)
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
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (fld (m * n))
      allocate (grd (m * n))
      allocate (hx (m * n))
      allocate (hy (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk2 (m * n))
      allocate (wrk3 (m * n * l))
c
c     ..initialization
c
      do_cntr = .false.
      fluid = 'o'
      nodes = m * n
      spmis = spval + 10.
      x = delx * 1.e-3
      y = dely * 1.e-3
      z_lev = 0
      do i = 1, (m * n)
         hx(i) = 1.
         hy(i) = 1.
      enddo
c
c     ..loop over analysis field types
c
      do k = 1, 3
         fail = .true.
         if (k .eq. 1 .and. do_sst) then
c
c           ..sst 
c
            fld_name = 'seatmp'
            fluid = 'o'
            if (tau .eq. 0) then
               file_typ = 'analfld'
            else
               file_typ = 'fcstfld'
            endif
            lvl_typ = 'sfc'
            nl = 1
            call rd_coda_file (dir_path, dtg, nest, m, n, nl, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk2,
     *                         .true., fail)
            if (.not. fail) then
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               cntr_int = 0.02
               dmn = 0.
               dmx = tmp_grd
               lntr = 10
               nslc = n_slc
            endif
         else if (k .eq. 2 .and. do_tmp) then
c
c           ..3D temperature 
c
            fld_name = 'seatmp'
            fluid = 'o'
            if (tau .eq. 0) then
               file_typ = 'analfld'
            else
               file_typ = 'fcstfld'
            endif
            lvl_typ = 'pre'
            nl = nz
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk3,
     *                         .true., fail)
            if (.not. fail) then
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               cntr_int = 0.02
               dmn = 0.
               dmx = tmp_grd
               lntr = 10
               nslc = n_slc
            endif
         else if (k .eq. 3 .and. do_sal) then
c
c           ..3D salinity 
c
            fld_name = 'salint'
            fluid = 'o'
            if (tau .eq. 0) then
               file_typ = 'analfld'
            else
               file_typ = 'fcstfld'
            endif
            lvl_typ = 'pre'
            nl = nz
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk3,
     *                         .true., fail)
            if (.not. fail) then
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               cntr_int = 0.01
               dmn = 0.
               dmx = sal_grd
               lntr = 20
               nslc = n_slc
            endif
         endif
         if (.not. fail) then
c
c           ..loop over plot levels
c
            do kp = 1, nl
c
c              ..extract field
c
               if (k .le. 2) then
                  do i = 1, nodes
                     fld(i) = wrk2(i)
                  enddo
                  lmn = 1.01 
               else
                  do i = 1, nodes
                     ndx = i + (z_plot(kp)-1) * m * n
                     fld(i) = wrk3(ndx)
                  enddo 
                  lmn = lvl(z_plot(kp)) + 1.01
               endif
c
c              ..set bottom mask
c
               do_btm = .false.
               do i = 1, nodes
                  if (depth(i) .lt. lmn) then
                     btm_msk(i) = -1.
                  else
                     btm_msk(i) = 1.
                  endif
               enddo
c
c              ..set plotting mask
c
               do i = 1, nodes
                  if (mask(i) .eq. 0) fld(i) = spval
               enddo
c
c              ..initialize plot array
c
               do i = 1, nodes
                  grd(i) = spval
               enddo
c
c              ..compute gradient field
c
               do j = 2, n
               do i = 2, m
                  p = m * (j-1) + i
                  if (mask(p) .gt. 0) then
                     x1 = m * (j-1) + i
                     x2 = m * (j-1) + (i-1)
                     y1 = m * (j-1) + i
                     y2 = m * (j-1-1) + i
                     if (mask(x1) .gt. 0 .and. mask(x2) .gt. 0 .and.
     *                   mask(y2) .gt. 0 .and. mask(y2) .gt. 0) then
                     if (fld(x1).gt.spmis .and. fld(x2).gt.spmis .and.
     *                   fld(y1).gt.spmis .and. fld(y2).gt.spmis) then
                        dx = abs (fld(x1) - fld(x2)) / (x * hx(p))
                        dy = abs (fld(y1) - fld(y2)) / (y * hy(p))
                        grd(p) = sqrt (dx * dy)
                     endif
                     endif
                  endif
               enddo
               enddo
c
c              ..plot field
c
               if (igrid .lt. 0) then
                  call glb_merc (grd, depth, dmn, dmx, m, n, gln, 
     *                           glt, bl, br, tl, tr, i1, i2, j1, 
     *                           j2, node_eq, pos1, pos2, pos3,
     *                           pos4, nslc, iamap, MX_AMAP, 
     *                           cntr_int, do_cntr, lntr, z_lev,
     *                           spval)
               else
                  call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                          bl, br, tl, tr, pos1, pos2, pos3,
     *                          pos4, iamap, MX_AMAP)
                  call contour_fld (grd, dmn, dmx, m, n, i1, i2, j1,
     *                              j2, nslc, iamap, MX_AMAP, btm_msk,
     *                              cntr_int, do_btm, do_cntr, .true.,
     *                              .false., .false., lntr, spval)
               endif
               if (k .eq. 1) then
                  write (title, '(''Sea Surface Temperature '',
     *                            ''Gradient (C/km)'')')
               else if (k .eq. 2) then
                  write (title, '(''Temperauture Gradient (C/km)'',
     *                   3x, i4, '' M Depth'')') nint (lvl(z_plot(kp)))
               else if (k .eq. 3) then
                  write (title, '(''Salinity Gradient (PSU/km)'',
     *                  3x, i4, '' M Depth'')') nint (lvl(z_plot(kp)))
               else
                  write (title, '(''gots no title here, boss'')')
               endif
               call title_plot (title, .018, 1, 2.1)
               fno = fno + 1
               write (*, '(10x, ''frame'', i5, '': '', a)')
     *                fno, trim (title)
               write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
               call title_plot (title, .018, 1, 1.)
               call frame
            enddo
         endif
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, fld, grd, hx, hy, iamap, wrk2, wrk3)
c
      return
      end
