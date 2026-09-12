      subroutine adj_sens_ice (title, adj_dir, dtg1, dtg2, nest, 
     *                         adj_tau, n_lon, n_lat, n_proj,
     *                         rlat, stdlt1, stdlt2, stdlon, bl,
     *                         br, tl, tr, upd, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  adj_sens_ice
c
c DESCRIPTION:  computes and plots average sea ice adjoint forecast
c               sensitivity errors. the forecast errors have been
c               calculated on the global tri-polar grid.  here the
c               that grid is interpolated to northern and southern
c               hemispheric polar stereographic grids for plotting.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name            Type       Usage            Description
c   --------      ---------   -------   -------------------------------
c    adj_tau      integer      input    adjoint forecast period
c    bl, br       real         input    grid bottom lat, lons
c    fno          integer      input    sequential frame number
c    n_proj       integer      input    grid projection number
c    out_dir      char         input    adjoint output directory
c    rlat         real         input    grid reference latitude
c    stdlon       real         input    grid standard longitude
c    stdlt(s)     real         input    grid standard latitudes
c    tl, tr       real         input    grid top lat, lons
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
      integer    N_VAR
      parameter (N_VAR = 1)
c
      integer    UNIT
      parameter (UNIT = 22)
c
c     ..local array dimensions
c
      integer   n_lat
      integer   n_lon
c
      character adj_dir * 256
      integer   adj_tau
      real      bl (2), br (2)
      character date1 * 6
      character date2 * 11
      integer   day
      character dtg * 10
      character dtg1 * 10
      character dtg2 * 10
      character dtg_end * 10
      character dtg_str * 10
      logical   fail
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   hrs
      integer   i, k
      integer   i1, i2, j1, j2
      integer   kvar
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   nest
      integer   n_files
      integer   n_proj
      integer   n_sfc
      character plot_title * 256
      real      rlat
      real      spmis
      real      spval
      real      stdlt1, stdlt2
      real      stdlon
      integer   status
      character title * (*)
      real      tl (2), tr (2)
      character tmp_name * 80  
      integer   var
      integer   upd
      real      xn
      integer   year
      real      z_lvl (100)
      integer   z_plot (20)
c
c     ..allocatable arrays
c
      real,     allocatable :: fld (:)
      real,     allocatable :: wrk (:)
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
c     ..adjust verification dtgs
c
      dtg_str = dtg1
      dtg_end = dtg2
c
c     ..form date time group labels
c
      read (dtg_str(5:6), '(i2)') mon
      read (dtg_str(7:8), '(i2)') day
      write (date1, '(i2.2, 1x, a)') day, month(mon)
c
      read (dtg_end(1:4), '(i4)') year
      read (dtg_end(5:6), '(i2)') mon
      read (dtg_end(7:8), '(i2)') day
      write (date2, '(i2.2, 1x, a, 1x, i4)') day, month(mon), year
c
c     ..set date time title
c
      if (dtg_str .eq. dtg_end) then
         plot_title = trim (title) // '     ' // date2
      else
         plot_title = trim (title) // '     ' // date1 //
     *                ' to ' // date2 
      endif
c
c     ..set adjoint file parameters
c
      fld_name = 'icecov'
      file_typ = 'sensfld'
      fluid = 'o'
      kvar = 1
      lvl_typ = 'sfc'
      n_sfc = 1
      spval = -999.
      spmis = spval + 9.
      z_lvl = 0.
      do i = 1, 20
         if (i .eq. 1) then
            z_plot(i) = i
         else
            z_plot(i) = 0
         endif
      enddo
c
c     ..set number files to process
c
      call dtgdif (dtg1, dtg2, hrs, status)
      n_files = hrs / upd + 1
c
c     ..set plot file name
c
      write (*, '(/, ''Sea Ice Adjoint Sensitivity Processing'')')
      write (tmp_name, '(''ice_sens.'', a, ''.gmeta '')') dtg2
      write (*, '(4x, ''output file name: '', a)') trim (tmp_name)
      fno = 0
      call init_gks ('opn', tmp_name)
c
c     ..allocate field arrays
c   
      allocate (fld (n_lon * n_lat))
      allocate (wrk (n_lon * n_lat))
c
c     ..loop over model variables
c
      do var = 1, N_VAR
c
c        ..initialize
c
         fld = 0.
         xn = 0.
c
c        ..read files, form summation
c
         do k = 1, n_files
            hrs = (k-1) * upd
            call dtgmod (dtg_str, hrs, dtg, status)
            call rd_coda_file (adj_dir, dtg, nest, n_lon, n_lat,
     *                         n_sfc, file_typ, fld_name, fluid,
     *                         adj_tau, lvl_typ, wrk, .true.,
     *                         fail)
            if (fail) cycle
c
            xn = xn + 1.
            do i = 1, (n_lon * n_lat)
               if (wrk(i) .gt. spmis) then
                  fld(i) = fld(i) + wrk(i)
               else
                  fld(i) = spval
               endif
            enddo
         enddo
         if (xn .lt. 1.) cycle
c
c        ..time mean
c
         do i = 1, (n_lon * n_lat)
            if (fld(i) .gt. spmis) fld(i) = fld(i) / xn
         enddo      
c
c        ..map forecast error on polar grids
c
         call map_adj_sens ('ICE', kvar, n_lon, n_lat, n_sfc, fld,
     *                      z_lvl, plot_title, n_proj, rlat, stdlt1,
     *                      stdlt2, stdlon, bl, br, tl, tr, z_plot,
     *                      fno, spval)
      enddo
c
c     ..clean up 
c
      deallocate (fld, wrk)
c
      call init_gks ('cls', tmp_name)
c
      return
      end
