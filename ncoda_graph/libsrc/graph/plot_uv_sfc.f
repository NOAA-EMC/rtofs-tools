      subroutine plot_uv_sfc (dir_path, date, dtg, tau, nest, m, n,
     *                        depth, mask, igrid, rlat, stdlt1, stdlt2,
     *                        stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *                        vel_mx, vct_thn, do_vel, do_err, do_inc,
     *                        gln, glt, node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_uv_sfc
c
c DESCRIPTION:  maps NCODA surface velocity vector components overlaid
c               on the current speed and color fills the contours
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
      integer   m, n
      integer   gln, glt
c
      real      bl (2), br (2)
      real      cntr_int
      character date * 15
      real      depth (m * n)
      character dir_path * (*)
      real      dmx
      logical   do_btm
      logical   do_err
      logical   do_inc
      logical   do_vel
      character dtg * 10
      logical   fail
      character file_typ * 7
      character fld_name * 6
      real      fl, fr, fb, ft
      character fluid * 1
      integer   fno
      integer   i, j, k
      integer   i1, i2, j1, j2
      integer   igrid
      integer   l
      integer   ll
      integer   lntr
      character lvl_typ * 3
      integer   lwrk
      integer   mask (m * n)
      integer   ndx
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   nslc
      integer   ni, nj
      integer   n_pass
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spmn, spmx, sprf
      real      spval 
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      character title * 132
      real      tl (2), tr (2)
      real      ul, ur, ub, ut
      real      vel_mx
      integer   vct_thn (2)
      real      vlow
      real      vmn, vmx
      integer   z_lev
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: data (:)
      real,     allocatable :: fld_u (:,:)
      real,     allocatable :: fld_v (:,:)
      real,     allocatable :: grd_u (:)
      real,     allocatable :: grd_v (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: wrk (:)
c
c     ..functions and externals
c
      external  vvudmv
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
      allocate (data (m * n))
      allocate (fld_u (m, n))
      allocate (fld_v (m, n))
      allocate (grd_u (m * n))
      allocate (grd_v (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (2 * m * n))
c
c     ..initialize
c
      fluid = 'o'
      l = 1
      lvl_typ = 'sfc'
      ni = i2 - i1 + 1
      nj = j2 - j1 + 1
      z_lev = 0
c
c     ..loop over analysis field types
c
      do k = 1, 4
         fail = .true.
         if (k .eq. 1 .and. do_vel) then
c
c           ..u,v components
c
            if (tau .eq. 0) then
               file_typ = 'analfld'
            else
               file_typ = 'fcstfld'
            endif
            fld_name = 'uucurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd_u,
     *                         .true., fail)
            fld_name = 'vvcurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd_v,
     *                         .true., fail)
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            cntr_int = 20.
            lntr = 10 
            nslc = n_slc
            n_pass = 1
            vlow = 1.
            do i = 1, (m * n)
               grd_u(i) = grd_u(i) * 1.e2
               grd_v(i) = grd_v(i) * 1.e2
            enddo
         else if (k .eq. 2 .and. tau .eq. 0 .and. do_inc) then
c
c           ..u,v increments 
c
            file_typ = 'analinc'
            fld_name = 'uucurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd_u,
     *                         .true., fail)
            fld_name = 'vvcurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd_v,
     *                         .true., fail)
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 2.
            lntr = 6
            nslc = n_slce
            n_pass = 1
            vlow = -1.
            do i = 1, (m * n)
               grd_u(i) = grd_u(i) * 1.e2
               grd_v(i) = grd_v(i) * 1.e2
            enddo
         else if (k .eq. 3 .and. tau .eq. 0 .and. do_err) then
c
c           ..u,v velocity error
c
            file_typ = 'fcsterr'
            fld_name = 'uucurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd_u,
     *                         .true., fail)
            fld_name = 'vvcurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd_v,
     *                         .true., fail)
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 2.
            lntr = 6
            nslc = n_slce
            n_pass = 1
            do i = 1, (m * n)
               grd_u(i) = grd_u(i) * 1.e2
               grd_v(i) = grd_v(i) * 1.e2
            enddo
         else if (k .eq. 4 .and. tau .eq. 0 .and. do_err) then
c
c           ..u,v velocity analysis error
c
            file_typ = 'analerr'
            fld_name = 'uucurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd_u,
     *                         .true., fail)
            fld_name = 'vvcurr'
            call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                         fld_name, fluid, tau, lvl_typ, grd_v, 
     *                         .true., fail)
            call gks_color (rgb_err_clr, MX_ERR_CLR)
            cntr_int = 20.
            lntr = 6
            nslc = n_slce
            n_pass = 1
         endif
         if (.not. fail) then
c
c           ..set bottom mask
c
            do_btm = .true.
            do i = 1, (m * n)
               if (depth(i) .lt. 1.01) then
                  btm_msk(i) = -1.
               else
                  btm_msk(i) = 1.
               endif
            enddo
c
c           ..set plotting mask
c
            do i = 1, (m * n)
               if (mask(i) .eq. 0) then
                  grd_u(i) = spval
                  grd_v(i) = spval
               endif
            enddo
c
c           ..compute velocities
c
            do j = 1, n
               do i = 1, m
                  ndx = m * (j-1) + i
                  fld_u(i,j) = grd_u(ndx)
                  fld_v(i,j) = grd_v(ndx)
                  if (grd_u(ndx) .gt. -990. .and.
     *                grd_v(ndx) .gt. -990.) then
                     data(ndx) = sqrt (grd_u(ndx)**2 + grd_v(ndx)**2)
                  else
                     data(ndx) = 0.
                  endif
               enddo
            enddo
c
c           ..smooth field
c
            call smth_2d (n_pass, data, m, n, spval)
c
c           ..set plot title
c
            if (k .eq. 1) then
               write (title, '(''Surface Velocity (cm/s)'')') 
               spmn = 0.
               spmx = vel_mx / real (nslc)
               spmx = spmx * real (nslc)
               sprf = spmx * 0.5
            else if (k .eq. 2) then
               write (title, '(''Surface Velocity Analyzed '',
     *                         ''Increment (cm/s)'')') 
               spmn = 0.
               spmx = vel_mx * 0.5
               sprf = spmx * 0.5
            else if (k .eq. 3) then
               write (title, '(''Surface Velocity Prediction '',
     *                         ''Error (cm/s)'')')
               spmn = 0.
               spmx = (vel_mx / 2.) / real (nslc)
               if (spmx .lt. 1.) then
                  spmx = 0.5
               else
                  spmx = real (nint (spmx))
               endif
               spmx = spmx * real (nslc)
c************************
               spmx = 30.
c************************
            else if (k .eq. 4) then
               write (title, '(''Velocity Analysis Error Reduction '',
     *                         ''(%)'')')
               spmn = 0.
               spmx = 100.
            endif
c
c           ..plot velocity speed background
c
            if (igrid .lt. 0) then
               call glb_merc (data, depth, spmn, spmx, m, n, gln,
     *                        glt, bl, br, tl, tr, i1, i2, j1, j2, 
     *                        node_eq, pos1, pos2, pos3, pos4,
     *                        nslc, iamap, MX_AMAP, cntr_int,
     *                        .false., lntr, z_lev, spval)
            else
               call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3,
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (data, spmn, spmx, m, n, i1, i2, j1,
     *                           j2, nslc, iamap, MX_AMAP, btm_msk,
     *                           cntr_int, do_btm, .false., .false.,
     *                           .false., .false., lntr, spval)
            endif
            call title_plot (title, .018, 1, 2.1)
            fno = fno + 1
            write (*, '(10x, ''frame'', i5, '': '', a)')
     *             fno, trim (title)
            write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
            call title_plot (title, .018, 1, 1.)
            if (k .eq. 1 .or. k .eq. 2) then
c
c              ..overlay vector field
c
               if (vct_thn(1) .ge. 0) then
                  call getset (fl, fr, fb, ft, ul, ur, ub, ut, ll)
                  call vvseti ('MAP - mapping flag', 0)
                  call vvseti ('SET - set call flag', 0)
                  call vvseti ('TRT - transformation type', 0)
                  call vvsetr ('XC1 - lower x bound', ul)
                  call vvsetr ('XCM - upper x bound', ur)
                  call vvsetr ('YC1 - lower y bound', ub)
                  call vvsetr ('YCN - upper y bound', ut)
                  call vvseti ('SVF - special values flag', 3)
                  call vvsetr ('USV - u special value', spval)
                  call vvsetr ('VSV - v special value', spval)
                  call vvsetr ('PSV - p special value', spval)
                  call vvseti ('SPC - p special color', 1)
                  call vvseti ('VST - vector statistics', 0)
                  call vvseti ('XIN - i array increment', vct_thn(1))
                  call vvseti ('YIN - j array increment', vct_thn(2))
                  call vvinit (fld_u(i1,j1), m, fld_v(i1,j1), m, data,
     *                         m, ni, nj, wrk, lwrk)
                  call getset (fl, fr, fb, ft, ul, ur, ub, ut, ll)
                  call vvgetr ('DMX - device max vector length', dmx)
                  call vvgetr ('VMN - minimum vector', vmn)
                  call vvgetr ('VMX - maximum vector', vmx)
                  call vvsetr ('AMN - arrow minimum size', 0.007)
                  call vvsetr ('LWD - vector line width', 1.5)
                  call vvsetr ('VLC - vector low cutoff', vlow)
                  call vvsetr ('VHC - vector high cutoff value', 200.)
                  call vvsetr ('VRM - vector reference magnitude', sprf)
                  call vvsetr ('VRL - vector reference length',
     *                         2.5*dmx/(fr-fl))
                  call vvsetr ('VFR - vector fractional minimum', 0.)
                  call vvsetc ('MNT - min vector text string', ' ')
                  call vvsetc ('MXT - max vector text string', ' ')
                  call gsplci (0)
                  call vvectr (fld_u(i1,j1), fld_v(i1,j1), data, iamap,
     *                         vvudmv, wrk)
               endif
            endif
            call frame
         endif
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, data, fld_u, fld_v, grd_u, grd_v)
      deallocate (iamap, wrk)
c
      return
      end
