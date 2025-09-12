      subroutine model_bias (title1, out_dir, clm_dir, dtg1, dtg2,
     *                       n_lon, n_lat, n_lvl, msk, gln, glt,
     *                       pln, plt, nest, upd, n_proj, rlat,
     *                       stdlt1, stdlt2, stdlon, bl, br, tl,
     *                       tr, i1, i2, j1, j2, global)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  model_bias
c
c DESCRIPTION:  computes and plots average increment fields as a
c               measure of model bias.  works only on surface
c               fields because the satellite data sampling is
c               adequate.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name          Type       Usage            Description
c   --------    ---------   -------   ---------------------------------
c    bl, br     real         input    grid bottom left, right lat, lons
c    fno        integer      input    sequential frame number
c    msk        integer      input    grid mask
c    node       integer      input    irregular grid index nodes
c    n_proj     integer      input    grid projection number
c    rlat       real         input    grid reference latitude
c    stdlon     real         input    grid standard longitude
c    stdlt(s)   real         input    grid standard latitudes
c    tl, tr     real         input    grid top left, right lat, lons
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
      parameter (N_VAR = 4)
c
c     ..local array dimensions
c
      integer   n_lat
      integer   n_lon
      integer   n_lvl
      integer   n_sfc
c
      integer   gln, glt
      integer   pln, plt
c
      real      bl (2), br (2)
      character clm_dir * (*)
      character date1 * 11
      character date2 * 11
      integer   day
      character dtg * 10
      character dtg1 * 10
      character dtg2 * 10
      logical   fail
      character file_typ * 7
      character fld_name (4) * 6
      character fluid * 1
      integer   fno
      logical   global
      integer   hrs
      integer   i, k
      integer   i1, i2, j1, j2
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   msk (n_lon * n_lat)
      integer   nest
      integer   n_files
      integer   n_proj
      character out_dir * (*)
      real      rlat
      real      spmis
      real      spval
      real      stdlt1, stdlt2
      real      stdlon
      integer   status
      integer   tau
      character title1 * 80
      character title3 * 256
      real      tl (2), tr (2)  
      character tmp_name * 80
      integer   var
      integer   upd
      real      xn
      integer   year
c
c     ..allocatable arrays
c
      real,     allocatable :: fld (:)
      real,     allocatable :: node_eq (:,:)
      real,     allocatable :: node_nh (:,:)
      real,     allocatable :: node_sh (:,:)
      real,     allocatable :: sfc (:)
      real,     allocatable :: wrk (:)
c
c     ..define field names
c
      data fld_name / 'seatmp', 'salint', 'icecov', 'icecov' /
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
c     ..allocate field arrays
c   
      allocate (fld (n_lon * n_lat))
      allocate (sfc (n_lon * n_lat))
      allocate (wrk (n_lon * n_lat * n_lvl))
c
c     ..set global interpolation arrays
c
      allocate (node_eq ((gln * glt), 2))
      allocate (node_nh ((pln * plt), 2))
      allocate (node_sh ((pln * plt), 2))
c
c     call dtgmod (dtg2, -upd, dtg, status)
c
c     ..form date time group labels
c
      read (dtg1(1:4), '(i4)') year
      read (dtg1(5:6), '(i2)') mon
      read (dtg1(7:8), '(i2)') day
      write (date1, '(i2, 1x, a, 1x, i4)')
     *       day, month(mon), year
c
      read (dtg2(1:4), '(i4)') year
      read (dtg2(5:6), '(i2)') mon
      read (dtg2(7:8), '(i2)') day
      write (date2, '(i2, 1x, a, 1x, i4)')
     *       day, month(mon), year
c
c     ..set title
c
      title3 = date1 // ' to ' // date2 
c
c     ..set gmeta file name, open gks
c
      call clsgks
      write (tmp_name, '(''bias.'', a, ''.gmeta '')') dtg2
      call init_gks ('opn', tmp_name)
c
c     ..set file parameters
c
      file_typ = 'analinc'
      fluid = 'o'
      fno = 0
      lvl_typ = 'pre'
      n_sfc = 1
      spval = -999.
      spmis = spval + 9.
      tau = 0
c
c     ..set number files to process
c
      call dtgdif (dtg1, dtg2, hrs, status)
      n_files = hrs / upd + 1
c
c     ..set up irregular plot grid
c
      if (global .and. n_proj .lt. 0) then
         call glb_map (out_dir, clm_dir, dtg2, n_lon, n_lat, gln,
     *                 glt, node_eq, pln, plt, node_nh, node_sh)
      else
         node_eq = 0.
         node_nh = 0.
         node_sh = 0.
      endif
c
c     ..diagnostics
c
      write (*, '(/, ''Model Bias Processing'')')
      write (*, '(''        number files: '', i10)') n_files
c
c     ..loop over model variables
c
      do var = 1, 4
c
c        ..initialize
c
         xn = 0.
         fld = 0.
         if (var .eq. 1 .or. var .eq. 2) then
            lvl_typ = 'pre'
         else if (var .eq. 3 .or. var .eq. 4) then
            lvl_typ = 'sfc'
         endif
c
c        ..read files, form summation
c
         do k = 1, n_files
            hrs = (k-1) * upd
            call dtgmod (dtg1, hrs, dtg, status)
            if (var .eq. 1 .or. var .eq. 2) then
               call rd_coda_file (out_dir, dtg, nest, n_lon, n_lat,
     *                            n_lvl, file_typ, fld_name(var),
     *                            fluid, tau, lvl_typ, wrk, .true.,
     *                            fail)
            else
               call rd_coda_file (out_dir, dtg, nest, n_lon, n_lat,
     *                            n_sfc, file_typ, fld_name(var),
     *                            fluid, tau, lvl_typ, sfc, .true.,
     *                            fail)
               do i = 1, (n_lon * n_lat)
                  wrk(i) = sfc(i)
               enddo
            endif
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
            if (msk(i) .eq. 0) fld(i) = spval
         enddo      
c
c        ..map mean increments
c
         call map_bias (var, n_lon, n_lat, fld, title1, title3, 
     *                  n_proj, rlat, stdlt1, stdlt2, stdlon, 
     *                  bl, br, tl, tr, i1, i2, j1, j2, gln,
     *                  glt, node_eq, pln, plt, node_nh, 
     *                  node_sh, fno, spval)
      enddo
c
c     ..close gks and clean up 
c
      call init_gks ('cls', tmp_name)
      call opngks
      deallocate (fld, node_eq, node_nh, node_sh, sfc, wrk)
c
      return
      end
