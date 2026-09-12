      subroutine plot_movie (dir_path, clm_path, parm, strt_dtg,
     *                       end_dtg, tau_hr, fcst, n_tau, m, n,
     *                       l, anl_lvl, z_plot, igrid, nest, rlat,
     *                       stdlt1, stdlt2, stdlon, bl, br, tl,
     *                       tr, i1, i2, j1, j2, gln, glt, pln,
     *                       plt, global, cntr, del, dmn, dmx,
     *                       spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_movie
c
c DESCRIPTION:  plots time serie of CODA pressure level analyses
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
      integer   pln, plt
c
      real      anl_lvl (100)
      real      bl(2), br(2)
      character clm_path * (*)
      real      cntr
      real      del
      real      dmn, dmx
      character dir_path * (*)
      logical   do_btm
      logical   do_cntr
      character dtg * 10
      character end_dtg * 10
      character err_msg * 256
      logical   fail
      logical   fcst
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      logical   global
      integer   hours
      integer   i, k, kz
      integer   i1, i2, j1, j2
      integer   igrid
      integer   lntr
      real      lvl
      character lvl_typ * 3
      integer   nest
      integer   nlvl
      integer   nodes
      integer   np
      integer   nsl
      integer   n_files
      integer   n_pass
      integer   n_tau
      character parm * 7
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval
      integer   status
      real      stdlon
      real      stdlt1, stdlt2
      character strt_dtg * 10
      integer   tau
      integer   tau_hr
      character title * 132
      real      tl(2), tr(2)
      integer   z_lev
      integer   z_plot
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: depth (:)
      real,     allocatable :: field (:)
      integer,  allocatable :: iamap (:)
      integer,  allocatable :: mask (:)
      real,     allocatable :: node_eq (:,:)
      real,     allocatable :: node_nh (:,:)
      real,     allocatable :: node_sh (:,:)
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
      allocate (depth (m * n))
      allocate (field (m * n))
      allocate (iamap (MX_AMAP))
      allocate (mask (m * n))
      allocate (wrk (m * n * l))
c
c     ..set global interpolation arrays
c
      allocate (node_eq ((gln * glt), 2))
      allocate (node_nh ((pln * plt), 2))
      allocate (node_sh ((pln * plt), 2))
c
c     ..retrieve bathymetry and mask fields
c
      call rd_dpth_msk (dir_path, strt_dtg, m, n, nest, depth, mask)
c
c     ..set up irregular plot grid
c
      if (global .and. igrid .lt. 0) then
         call glb_map (dir_path, clm_path, dtg, m, n, gln, glt,
     *                 node_eq, pln, plt, node_nh, node_sh)
      else
         node_eq = 0.
         node_nh = 0.
         node_sh = 0.
      endif
c
c     ..initialize
c
      fluid = 'o'
      lntr = 10
      nodes = m * n
      n_pass = 3
      if (dmx .lt. -990.) dmx = dmn + del * real (n_slc)
      if (cntr .le. 0.) then
         do_cntr = .false.
      else
         do_cntr = .true.
      endif
c
c     ..decode parameter
c
      if (parm .eq. 'sst    ') then
         if (fcst) then
            file_typ = 'fcstfld'
         else
            file_typ = 'analfld'
         endif
         fld_name = 'seatmp'
         lvl_typ = 'sfc'
         nlvl = 1
      else if (parm .eq. 'sst_err') then  
         file_typ = 'fcsterr'
         fld_name = 'seatmp'
         lvl_typ = 'sfc'
         nlvl = 1
      else if (parm .eq. 'sst_inc') then
         dmn = -3.
         dmx =  3.
         file_typ = 'analinc'
         fld_name = 'seatmp'
         fluid = 'o'
         lvl_typ = 'sfc'
         nlvl = 1
      else if (parm .eq. 'tmp    ') then  
         fld_name = 'seatmp'
         if (fcst) then
            file_typ = 'fcstfld'
         else
            file_typ = 'analfld'
         endif
         lvl_typ = 'pre'
         nlvl = l
      else if (parm .eq. 'tmp_err') then
         file_typ = 'fcsterr'
         fld_name = 'seatmp'
         lvl_typ = 'pre'
         nlvl = l
      else if (parm .eq. 'tmp_inc') then
         dmn = -9.
         dmx =  9.
         file_typ = 'analinc'
         fld_name = 'seatmp'
         lntr = 5
         lvl_typ = 'pre'
         nlvl = l
      else if (parm .eq. 'sal    ') then  
         fld_name = 'salint'
         if (fcst) then
            file_typ = 'fcstfld'
         else
            file_typ = 'analfld'
         endif
         lvl_typ = 'pre'
         nlvl = l
      else if (parm .eq. 'sal_err') then
         file_typ = 'fcsterr'
         fld_name = 'salint'
         lvl_typ = 'pre'
         nlvl = l 
      else if (parm .eq. 'sal_inc') then
         dmn = -0.3
         dmx =  0.3
         file_typ = 'analinc'
         fld_name = 'salint'
         lntr = 5
         lvl_typ = 'pre'
         nlvl = l
      else if (parm .eq. 'ssh    ') then
         fld_name = 'seahgt'
         if (fcst) then
            file_typ = 'fcstfld'
         else
            file_typ = 'analfld'
         endif
         lntr = 10
         lvl_typ = 'sfc'
         nlvl = l
      else
         write (err_msg, '(''parameter "'', a, ''" not supported'')')
     *          parm
         call error_exit ('PLOT_MOVIE', err_msg)
      endif
c
c     ..set analysis level
c
      lvl = anl_lvl(z_plot)
      z_lev = nint (lvl)
c
c     ..set bottom mask
c
      if (l .gt. 1) then
         do_btm = .true.
         do i = 1, nodes
            if (depth(i) .lt. (lvl+1.01)) then
               btm_msk(i) = -1.
            else
               btm_msk(i) = 1.
            endif
         enddo
      else
         lvl = 0.
         do_btm = .false.
         do i = 1, nodes
            if (depth(i) .lt. 1.01) then
               btm_msk(i) = -1.
            else
               btm_msk(i) = 1.
            endif
         enddo
      endif
c
c     ..compute number files to process and loop over dtgs
c
      call dtgdif (strt_dtg, end_dtg, hours, status)
      tau_hr = 24
      if (.not. fcst) then
         n_files = (hours / tau_hr) + 1
      else
         n_files = n_tau
      endif
      do np = 1, n_files
         if (.not. fcst) then
            k = (np - 1) * tau_hr
            call dtgmod (strt_dtg, k, dtg, status)
            tau = 0
         else
            dtg = strt_dtg
            tau = np * tau_hr
         endif
c
c        ..read file
c
         if (nlvl .eq. 1) then
            call rd_coda_file (dir_path, dtg, nest, m, n, nlvl,
     *                         file_typ, fld_name, fluid, tau,
     *                         lvl_typ, field, .true., fail)
            do i = 1, nodes
               if (mask(i) .eq. 0) field(i) = spval
            enddo
         else
            call rd_coda_file (dir_path, dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid,
     *                         tau, lvl_typ, wrk, .true., fail)
            do k = 1, l
               do i = 1, (m * n)
                  if (k .gt. mask(i)) then
                     kz = i + (k-1) * m * n
                     wrk(kz) = spval
                  endif
               enddo
            enddo
            do i = 1, nodes
               k = i + (z_plot-1) * m * n
               field(i) = wrk(k)
            enddo
         endif
         if (.not. fail) then
c
c           ..smooth field
c
            call smth_2d (n_pass, field, m, n, spval)
c
c           ..plot field
c
            if (parm(5:7) .eq. 'inc') then
               call gks_color (rgb_anm_clr, MX_ANM_CLR)
               nsl = n_slca
            else
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               nsl = n_slc
            endif
            if (igrid .lt. 0) then
               call glb_merc (field, depth, dmn, dmx, m, n, gln,
     *                        glt, bl, br, tl, tr, i1, i2, j1, j2, 
     *                        node_eq, pos1, pos2, pos3, pos4,
     *                        nsl, iamap, MX_AMAP, cntr, .false.,
     *                        lntr, z_lev, spval)
            else
               call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3, 
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (field, dmn, dmx, m, n, i1, i2, 
     *                           j1, j2, nsl, iamap, MX_AMAP, 
     *                           btm_msk, cntr, do_btm, do_cntr,
     *                           .false., .false., .false., lntr,
     *                           spval)
            endif
            write (title, '(a, 2x, ''Tau '', i3.3, 3x, i4,
     *                    '' M Depth'')') dtg, tau, nint (lvl)
            call title_plot (title, .018, 1, 1.)
            call frame
         endif
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, depth, field, iamap, mask, node_eq)
      deallocate (node_nh, node_sh, wrk)
c
      return
      end
