      subroutine adj_sens_prf (title, adj_dir, dtg1, dtg2, nest,
     *                         adj_tau, n_lon, n_lat, n_lvl, n_proj,
     *                         rlat, stdlt1, stdlt2, stdlon, bl, br,
     *                         tl, tr, upd, z_lvl, z_plot, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  adj_sens_prf
c
c DESCRIPTION:  computes and plots average adjoint sensitivities.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name            Type       Usage            Description
c   --------      ---------   -------   ------------------------------
c    adj_tau      integer      input    adjoint forecast period
c    bl, br       real         input    grid bottom lat, lons
c    fno          integer      input    sequential frame number
c    n_proj       integer      input    grid projection number
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
      parameter (N_VAR = 2)
c
      integer    UNIT
      parameter (UNIT = 22)
c
c     ..local array dimensions
c
      integer   n_lat
      integer   n_lon
      integer   n_lvl
c
      character adj_dir * (*)
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
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   nest
      integer   n_files
      integer   n_proj
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
      call dtgmod (dtg1, -adj_tau, dtg_str, status)
      call dtgmod (dtg2, -adj_tau, dtg_end, status)
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
      file_typ = 'sensfld'
      fluid = 'o'
      lvl_typ = 'pre'
      spval = -999.
      spmis = spval + 9.
c
c     ..set number files to process
c
      call dtgdif (dtg1, dtg2, hrs, status)
      n_files = hrs / upd + 1
c
c     ..set plot file name
c
      write (*, '(/, ''Profile Adjoint Sensitivity Processing'')')
      write (tmp_name, '(''fcst_sens.'', a, ''.gmeta '')') dtg2
      write (*, '(4x, ''output file name: '', a)') trim (tmp_name)
      fno = 0
      call init_gks ('opn', tmp_name)
c
c     ..allocate field arrays
c   
      allocate (fld (n_lon * n_lat * n_lvl))
      allocate (wrk (n_lon * n_lat * n_lvl))
c
c     ..loop over model variables
c
      do var = 1, N_VAR
c
c        ..initialize
c
         fld = 0.
         wrk = 0.
         xn = 0.
c
c        ..set field label
c
         if (var .eq. 1) fld_name = 'seatmp'
         if (var .eq. 2) fld_name = 'salint'
c
c        ..read files, form summations
c
         do k = 1, n_files
            hrs = (k-1) * upd
            call dtgmod (dtg_str, hrs, dtg, status)
            call rd_coda_file (adj_dir, dtg, nest, n_lon, n_lat,
     *                         n_lvl, file_typ, fld_name, fluid,
     *                         adj_tau, lvl_typ, wrk, .true.,
     *                         fail)
            if (fail) cycle
c
            xn = xn + 1.
            do i = 1, (n_lon * n_lat * n_lvl)
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
         do i = 1, (n_lon * n_lat * n_lvl)
            if (fld(i) .gt. spmis) fld(i) = fld(i) / xn
         enddo      
c
c        ..map forecast error
c
         call map_adj_sens ('PRF', var, n_lon, n_lat, n_lvl, fld,
     *                      z_lvl, plot_title, n_proj, rlat, stdlt1,
     *                      stdlt2, stdlon, bl, br, tl, tr, z_plot,
     *                      fno, spval)
      enddo
c
c     ..clean up 
c
      deallocate (fld, wrk)
c
c     ..close gmeta file
c
      call init_gks ('cls', tmp_name)
c
      return
      end
