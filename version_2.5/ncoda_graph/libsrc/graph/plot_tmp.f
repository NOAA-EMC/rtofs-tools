      subroutine plot_tmp (dir_path, date, dtg, tau, nest, m, n, l,
     *                     depth, lvl, mask, igrid, rlat, stdlt1,
     *                     stdlt2, stdlon, bl, br, tl, tr, i1, i2,
     *                     j1, j2, nz, z_plot, tmp_cnt, tmp_del,
     *                     tmp_min, do_tmp, do_err, do_inc, do_clm,
     *                     do_clm_err, gln, glt, node_eq, lyr, fno,
     *                     spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_tmp
c
c DESCRIPTION:  maps a CODA 3D temperature analysis and color fills
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
      parameter (MX_AMAP = 72 000 000)
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
      real      bl(2), br(2)
      real      cntr_int
      real      dmn, dmx
      character date * 15
      real      depth (m * n)
      character dir_path * (*)
      logical   do_btm
      logical   do_clm, do_clm_err
      logical   do_cntr
      logical   do_err
      logical   do_inc
      logical   do_lbl
      logical   do_tmp
      character dtg * 10
      real      dz
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
      real      node_eq ((gln * glt), 2)
      integer   n_nodes
      integer   ndx
      integer   nest
      integer   nslc
      character plt_name * 80
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval
      real      stdlon
      real      stdlt1, stdlt2
      real      sum_dz
      integer   tau
      character title * 132
      real      tl(2), tr(2)
      real      tmp_cnt
      real      tmp_del
      real      tmp_min
      integer   z_lev
      integer   z_plot (50)
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: err (:)
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
      allocate (err (m * n))
      allocate (fld (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n * l))
c
c     ..initialize
c
      fld_name = 'seatmp'
      fluid = 'o'
      lvl_typ = 'pre'
      if (lyr) lvl_typ = 'lyr'
      n_nodes = m * n
c
c     ..set pooled satellite file name, open gks
c
      call clsgks
      write (plt_name, '(''seatmp_'', a, ''.'', a, ''.gmeta '')')
     *       lvl_typ, dtg
      fno = 0
      call init_gks ('opn', plt_name)
c
c     ..loop over analysis field types
c
      do k = 1, 6
         fail = .true.
         if (k .eq. 1 .and. do_tmp) then
c
c           ..temperature
c
            if (tau .eq. 0) then
               file_typ = 'analfld'
            else
               file_typ = 'fcstfld'
            endif
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk, 
     *                         .true., fail)
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            cntr_int = tmp_cnt
            do_btm = .true.
            if (tmp_cnt .gt. 0.) then
               do_cntr = .true.
            else
               do_cntr = .false.
            endif
            do_lbl = .true.
            dmn = tmp_min
            dmx = dmn + tmp_del * real (n_slc)
            lntr = 4
            nslc = n_slc
         else if (k .eq. 2 .and. tau .eq. 0 .and. do_inc) then
c
c           ..temperature analyzed increment
c
            file_typ = 'analinc'
c************************
c     file_typ = 'meaninc'
c************************
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk,
     *                         .true., fail)
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = 0.5
            do_btm = .true.
            do_cntr = .false.
            do_lbl = .true.
            if (lyr) then
               dmn = -1.5
               dmx =  1.5
            else
               dmn = -1.5
               dmx =  1.5
            endif
            lntr = 5
            nslc = n_slca
         else if (k .eq. 3 .and. tau .eq. 0 .and. do_err) then
c
c           ..temperature prediction error
c   
            file_typ = 'fcsterr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk,
     *                         .true., fail)
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 0.2
            do_btm = .false.
            do_cntr = .false.
            do_lbl = .true.
            dmn = 0.
            dmx = 1.2
            lntr = 5
            nslc = n_slce
         else if (k .eq. 4 .and. tau .eq. 0 .and. do_err) then
c
c           ..temperature analysis error
c   
            file_typ = 'analerr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk,
     *                         .true., fail)
            wrk = wrk * 100.
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 20.
            do_btm = .true.
            do_cntr = .true.
            do_lbl = .false.
            dmn = 0.
            dmx = 100.
            lntr = 6
            nslc = n_slce
         else if (k.eq.5 .and. tau.eq.0 .and. do_err .and. do_clm) then
c
c           ..temperature climate error
c    
            file_typ = 'climerr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk,
     *                         .true., fail)
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 0.2
            do_btm = .true.
            do_cntr = .false.
            do_lbl = .true.
            dmn = 0.
            dmx = 3.
            lntr = 5
            nslc = n_slce
         else if (k.eq.6 .and. tau.eq.0 .and. do_clm_err) then
c
c           ..integral of temperature climate error
c    
            file_typ = 'climerr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, wrk,
     *                         .true., fail)
            do i = 1, (m * n)
               err(i) = 0.
               sum_dz = 0.
               do j = 2, 33
                  ndx = i + (j-1) * m * n
                  dz = (lvl(j) - lvl(j-1)) * 0.5 + 
     *                 (lvl(j+1) - lvl(j)) * 0.5
                  err(i) = err(i) + wrk(ndx) * dz
                  sum_dz = sum_dz + dz
               enddo
               err(i) = err(i) / sum_dz
            enddo
            do j = 1, l
               do i = 1, (m * n)
                  ndx = i + (j-1) * m * n
                  wrk(i) = err(i)
               enddo
            enddo
c
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 0.2
            do_btm = .true.
            do_cntr = .false.
            do_lbl = .true.
            dmn = 0.
            dmx = 4.
            lntr = 3
            nslc = n_slce
         endif
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
c           ..set land and bottom masks
c
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
               ndx = i + (z_plot(j)-1) * m * n
               fld(i) = wrk(ndx)
               if (mask(i) .eq. 0) fld(i) = spval
            enddo
c
c           ..set plot title
c
            if (k .eq. 1) then
               if (lyr) then
                  write (title, '(''Temperature (C)'', 4x,
     *                            ''Layer'', i4)') z_plot(j)
               else
                  write (title, '(''Temperature (C)'', 4x, i4,
     *                            '' M Depth'')') z_lev
               endif
            else if (k .eq. 2) then
               if (lyr) then
                  write (title, '(''Temperature Analyzed Increment '',
     *                            ''(C)'', 4x, ''Layer'', i4)')
     *                            z_plot(j)
               else
                  write (title, '(''Temperature Analyzed Increment '',
     *                            ''(C)'', 4x, i4, '' M Depth'')')
     *                            z_lev
               endif
            else if (k .eq. 3) then
               write (title, '(''Temperature Prediction Error '',
     *                         ''(C)'', 4x, i4, '' M Depth'')')
     *                         z_lev
            else if (k .eq. 4) then
               write (title, '(''Temperature Analysis Error '',
     *                         ''Reduction (%) '', 4x, i4,
     *                         '' M Depth'')') z_lev
            else if (k .eq. 5) then
               write (title, '(''Temperature Climate Error '',
     *                         ''(C)'', 4x, i4, '' M Depth'')')
     *                         z_lev
            else if (k .eq. 6) then
               write (title, '(''Integral Temperature Climate Error '',
     *                         ''(C)'', 4x, ''0-1000 M Depth'')')
            endif
c
c           ..plot field
c
            if (igrid .lt. 0) then
               call glb_merc (fld, depth, dmn, dmx, m, n, gln, 
     *                        glt, bl, br, tl, tr, i1, i2, j1,
     *                        j2, node_eq, pos1, pos2, pos3, 
     *                        pos4, nslc, iamap, MX_AMAP,
     *                        cntr_int, do_cntr, lntr, z_lev,
     *                        spval)
            else
               call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3,
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (fld, dmn, dmx, m, n, i1, i2, j1,
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
         enddo
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
      deallocate (btm_msk, err, fld, iamap, wrk)
c
      return
      end
