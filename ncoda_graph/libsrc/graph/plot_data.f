      subroutine plot_data (dir_path, date, dtg, nest, m, n, igrid,
     *                      rlat, stdlt1, stdlt2, stdlon, bl, br,
     *                      tl, tr, do_data, do_raw, global, zoom,
     *                      fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_data
c
c DESCRIPTION:  plots observation locations used in NCODA analysis
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
      integer   m, n
c
      real      bl(2), br(2)
      character date * 15
      character dir_path * (*)
      logical   do_data
      logical   do_raw
      character dtg * 10
      integer   fno
      logical   global
      integer   igrid
      integer   n_proj
      integer   nest
      character plt_name * 80
      real      rlat
      real      slon
      real      stdlon
      real      stdlt1, stdlt2
      logical   subset
      real      tl(2), tr(2)
      real      xl(2), xr(2)
      real      yl(2), yr(2)
      real      zoom (4)
c
c     ..allocatable arrays
c
      integer,  allocatable :: iamap (:)
c
      include 'color_table.h'
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (iamap (MX_AMAP))
c
c     ..set pooled satellite file name, open gks
c
      call clsgks
      write (plt_name, '(''obsmap_pre.'', a, ''.gmeta '')') dtg
      fno = 0
      call init_gks ('opn', plt_name)
c
c     ..check for zoom grid
c
      subset = .false.
      if (zoom(1) .gt. -990. .and. zoom(2) .gt. -990. .and.
     *    zoom(3) .gt. -990. .and. zoom(4) .gt. -990.) then
         subset = .true.
      endif
c
c     ..check for global irregular grid
c
      if (global .and. .not. subset .and. igrid .lt. 0) then
         n_proj = 1
         slon = 180.
         xl(1) = -80.
         xl(2) = 0.
         xr(1) = -80.
         xr(2) = 360.
         yl(1) = 80.
         yl(2) = 0.
         yr(1) = 80.
         yr(2) = 360.
      else
         n_proj = igrid
         slon = stdlon
         xl(1) = bl(1)
         xl(2) = bl(2)
         xr(1) = br(1)
         xr(2) = br(2)
         yl(1) = tl(1)
         yl(2) = tl(2)
         yr(1) = tr(1)
         yr(2) = tr(2)
      endif
c
c     ..raw observations
c
      if (do_raw) then
c     call map_raw_ice (dir_path, date, dtg, nest, m, n, n_proj, rlat,
c    *                  stdlt1, stdlt2, slon, xl, xr, yl, yr, iamap,
c    *                  MX_AMAP, fno)
c     call map_raw_sst (dir_path, date, dtg, nest, m, n, n_proj, rlat,
c    *                  stdlt1, stdlt2, slon, xl, xr, yl, yr, iamap,
c    *                  MX_AMAP, fno)
      call map_raw_ssh (dir_path, date, dtg, nest, m, n, n_proj, rlat, 
     *                  stdlt1, stdlt2, slon, xl, xr, yl, yr, iamap,
     *                  MX_AMAP, fno)
c     call map_raw_sss (dir_path, date, dtg, nest, m, n, n_proj, rlat,
c    *                  stdlt1, stdlt2, slon, xl, xr, yl, yr, iamap,
c    *                  MX_AMAP, fno)
c     call map_raw_prf (dir_path, date, dtg, nest, m, n, n_proj, rlat,
c    *                  stdlt1, stdlt2, slon, xl, xr, yl, yr, iamap,
c    *                  MX_AMAP, fno)
c     call map_raw_syn (dir_path, date, dtg, nest, m, n, n_proj, rlat,
c    *                  stdlt1, stdlt2, slon, xl, xr, yl, yr, iamap,
c    *                  MX_AMAP, fno)
      endif
c
c     ..innovations  
c
      if (do_data) then
      call map_obs (dir_path, date, dtg, nest, m, n, n_proj, rlat,
     *              stdlt1, stdlt2, slon, xl, xr, yl, yr, subset,
     *              iamap, MX_AMAP, fno)
      endif
c
c     ..along track velocities
c
c     call map_ssh_uv (dir_path, date, dtg, nest, m, n, n_proj, 
c    *                 rlat, stdlt1, stdlt2, slon, xl, xr, yl, 
c    *                 yr, iamap, MX_AMAP, fno)
c
c     ..close and reset gks
c
      call init_gks ('cls', plt_name)
      call opngks
c
c     ..clean up
c
      deallocate (iamap)
c
      return
      end
