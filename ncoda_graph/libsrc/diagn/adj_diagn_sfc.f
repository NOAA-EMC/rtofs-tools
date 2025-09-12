      subroutine adj_diagn_sfc (title, out_dir, adj_dir, dtg1, dtg2,
     *                          n_mdl_lon, n_mdl_lat, n_lon, n_lat,
     *                          nest, n_proj, delx, dely, iref, jref,
     *                          rlat, rlon, stdlt1, stdlt2, stdlon,
     *                          bl, br, tl, tr, adj_tau, upd, area,
     *                          fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  adj_diagn_sfc
c
c DESCRIPTION:  performs diagnostics of data impact files on raw
c               sst/sss observations (gets at individual satellite 
c               impacts)
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name          Type       Usage            Description
c   --------    ---------   -------   ---------------------------------
c    adj_tau    integer      input    adjoint forecast period
c    bl, br     real         input    grid bottom left, right lat, lons
c    fno        integer      input    sequential frame number
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
      integer    N_SAT
      parameter (N_SAT = 8) 
c
      integer    N_TYP
      parameter (N_TYP = 3)
c
c     ..define work unit number
c
      integer    UNIT
      parameter (UNIT = 20)
c
c     ..local array dimensions
c
      integer   n_data
      integer   n_obs
c
      character adj_dir * (*)
      integer   adj_tau
      real      age
      real      anm
      character area * (*)
      real      bl (2), br (2)
      integer   cls
      character date1 * 6
      character date2 * 11
      integer   day
      real      delx, dely
      character dtg * 10
      character dtg1 * 10
      character dtg2 * 10
      character dtg_end * 10
      character dtg_str * 10
      real      ebk
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name (N_SAT) * 6
      character fluid * 1
      integer   fno
      integer   hrs
      integer   i, j, k, m, n
      integer   iref, jref
      integer   len, len_adj, len_dir
      real      lvl
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   ndx
      integer   nest
      integer   n_grd
      integer   n_files
      integer   n_lat
      integer   n_lon
      integer   n_lvl
      integer   n_mdl_lat
      integer   n_mdl_lon
      integer   n_proj
      integer   n_strt
      integer   n_out
      character out_dir * (*)
      character plot_title * 256
      logical   rd_fail
      real      rlat, rlon
      character sat_nam (N_SAT) * 7
      integer   sat_typ (N_TYP, N_SAT)
      character sgn * 7
      real      spval, spmis
      real      stdlt1, stdlt2
      real      stdlon
      integer   status
      integer   tau
      character title * (*)
      real      tl (2), tr (2)
      character tmp_name * 80  
      integer   upd
      character var_name (N_SAT) * 6
      real      vfy
      real      xm, yn
      integer   year
      real      zk
c
c     ..allocatable data impact arrays
c
      real,     allocatable :: ob_imp (:)
      real,     allocatable :: ob_lat (:)
      real,     allocatable :: ob_lon (:)
      real,     allocatable :: ob_sen (:)
      integer,  allocatable :: ob_typ (:)
      real,     allocatable :: ob_xi (:)
      real,     allocatable :: ob_yj (:)
c
      character,allocatable :: file_dtg (:) * 10
c
c     ..allocatable surface sensitivity field
c
      real,     allocatable :: fld (:)
c
c     ..allocatable data arrays
c
      real,     allocatable :: bkg (:)
      real,     allocatable :: eob (:)
      logical,  allocatable :: fail (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      real,     allocatable :: sen (:)
      integer,  allocatable :: typ (:)
      real,     allocatable :: val (:)
      real,     allocatable :: xi (:)
      real,     allocatable :: yj (:)
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c     ..define types codes for sst/sss observing systems
c
      data sat_typ / 119, 123, 124,
     *               145, 166, 168,
     *               115, 116, 117,
     *               120, 121, 122,
     *                 4,   5,  -1,
     *                 3,  21,  22,
     *               174,  -1,  -1,
     *               126,  -1,  -1 /
c
c     ..define observing system labels
c
      data sat_nam / 'NOAA-20', 'NOAA-21', 'METOP-B', 'METOP-C',
     *               'Buoy   ', 'Ship   ', 'SMOS   ', 'SMAP   ' /
c
c     ..set system names
c
      data fld_name / 'jpssst', 'jpssst', 'mtpsst', 'mtpsst',
     *                'shpsst', 'shpsst', 'salint', 'salint' / 
c
c     ..set variable names
c
      data var_name / 'seatmp', 'seatmp', 'seatmp', 'seatmp',
     *                'seatmp', 'seatmp', 'salint', 'salint' /
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
c     ..set date time group title
c
      if (dtg_str .eq. dtg_end) then
         plot_title = trim (title) // '     ' // date2
      else
         plot_title = trim (title) // '     ' // date1 //
     *                ' to ' // date2 
      endif
c
c     ..set sfc file parameters
c
      file_typ = 'rawdata'
      fluid = 'o'
      len_adj = len_trim (adj_dir) + 1
      len_dir = len_trim (out_dir) + 1
      lvl_typ = 'sfc'
      n_lvl = 1
      n_strt = 1
      spval = -999.
      spmis = spval + 9.
      tau = 0
      xm = real (n_lon)
      yn = real (n_lat)
c
c     ..set number files to process
c
      call dtgdif (dtg1, dtg2, hrs, status)
      n_files = hrs / upd + 1
c
c     ..diagnostics
c
      write (*, '(/, ''SFC Adjoint Processing'')')
      write (*, '(''        number files: '', i10)') n_files
c
c     ..initialize output counter
c
      n_out = 0
c
c     ..find number obs in domain
c
      n_obs = 0
      do m = 1, N_SAT
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg1, hrs, dtg, status)
         call cr_fname (out_dir, dtg, nest, n_mdl_lon, n_mdl_lat,
     *                  file_typ, fld_name(m), fluid, lvl_typ,
     *                  tau, file_name, len)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            open (UNIT, file=file_name(1:len), status='unknown',
     *                  form='unformatted')
            read (UNIT) n_data
c*******************
      write (*, '(''     number file obs: '', 2i10, 2x, a, 2x, a)')
     * n_data, n_obs, dtg, fld_name(m)
c*********************
            allocate (lat (n_data))
            allocate (lon (n_data))
            allocate (xi  (n_data))
            allocate (yj  (n_data))
            read (UNIT) lat(1:n_data)
            read (UNIT) lon(1:n_data)
            close (UNIT)
            call ll2ij (n_proj, rlat, rlon, iref, jref, stdlt1,
     *                  stdlt2, stdlon, delx, dely, lat, lon,
     *                  n_data, xi, yj)
            do i = 1, n_data
               if (xi(i) .ge. 1. .and. xi(i) .le. xm) then
               if (yj(i) .ge. 1. .and. yj(i) .le. yn) then
                  n_obs = n_obs + 1
               endif
               endif
            enddo
            deallocate (lat, lon, xi, yj)
         endif
      enddo
      enddo
      write (*, '(''      number raw obs: '', i10)') n_obs
      if (n_obs .eq. 0) return
c
c     ..allocate data impact arrays
c   
      allocate (ob_imp (n_obs))
      allocate (ob_lat (n_obs))
      allocate (ob_lon (n_obs))
      allocate (ob_sen (n_obs))
      allocate (ob_typ (n_obs))
      allocate (ob_xi  (n_obs))
      allocate (ob_yj  (n_obs))
      allocate (file_dtg (n_files))
      allocate (fld (n_lon * n_lat))
c
c--------------------------------------------------------------
c
c     ..build data arrays
c
      do m = 1, N_SAT
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg1, hrs, dtg, status)
         file_dtg(k) = dtg
c
c        ..read raw data name
c
         call cr_fname (out_dir, dtg, nest, n_mdl_lon, n_mdl_lat,
     *                  file_typ, fld_name(m), fluid, lvl_typ,
     *                  tau, file_name, len)
         inquire (file=file_name(1:len), exist=exist)
         len_dir = len_trim (out_dir) + 1
         if (.not. exist) then
            write (*, '(''        missing file: '', a)')
     *             file_name(len_dir:len)
            cycle
         endif
         open (UNIT, file=file_name(1:len), status='unknown',
     *               form='unformatted')
         read (UNIT) n_data
         write (*, '(''       restart found: '', a, i10)')
     *          file_name(len_dir:len), n_data
c
c        ..allocate local file arrays
c     
         allocate (bkg (n_data))
         allocate (eob (n_data))
         allocate (fail (n_data))
         allocate (lat (n_data))
         allocate (lon (n_data))
         allocate (sen (n_data))
         allocate (typ (n_data))
         allocate (val (n_data))
         allocate (xi (n_data))
         allocate (yj (n_data))
c
c        ..read data vectors
c
         read (UNIT) lat(1:n_data)
         read (UNIT) lon(1:n_data)
         read (UNIT) lvl
         read (UNIT) ndx
         read (UNIT) typ(1:n_data)
         read (UNIT) val(1:n_data)
         read (UNIT) sgn
         close (UNIT)
c
c        ..project data onto adjoint grid
c
         call ll2ij (n_proj, rlat, rlon, iref, jref, stdlt1,
     *               stdlt2, stdlon, delx, dely, lat, lon,
     *               n_data, xi, yj)
c
c        ..save valid projections and satellite data types
c
         n = 0
         do i = 1, n_data
            if (xi(i) .ge. 1. .and. xi(i) .le. xm) then
            if (yj(i) .ge. 1. .and. yj(i) .le. yn) then
               do j = 1, N_TYP
                  if (typ(i) .eq. sat_typ(j,m)) then
                     n = n + 1
                     lat(n) = lat(i)
                     lon(n) = lon(i)
                     typ(n) = typ(i)
                     val(n) = val(i)
                     xi(n) = xi(i)
                     yj(n) = yj(i)
                  endif
               enddo
            endif
            endif
         enddo
         n_grd = n
         write (*, '(9x, a, '' obs: '', i10)')
     *          adjustr (sat_nam(m)), n_grd
         if (n_grd .gt. 0) then
c
c--------------------------------------------------------------
c
c        ..retrieve sensitivities at ob locations
c
         call rd_coda_file (adj_dir, dtg, nest, n_lon, n_lat, n_lvl,
     *                      'sensfld', var_name(m), 'o', adj_tau, 
     *                      'pre', fld, .true., rd_fail)
         if (.not. rd_fail) then
            write (*, '(''       restart found: '', a)')
     *                  file_name(len_dir:len)
            call fld2_trp (n_data, n_strt, n_grd, xi, yj, n_lon,
     *                     n_lat, fld, spval, sen, fail)
         else
            write (*, '(''        missing file: '', a)')
     *                  file_name(len_dir:len)
            deallocate (bkg, eob, fail, lat, lon)
            deallocate (sen, typ, val, xi, yj)
            cycle
         endif
c
c        ..retrieve background and errors at ob locations
c
         call adj_sfc_bkg (out_dir, dtg, var_name(m), adj_tau, 
     *                     n_data, n_grd, lat, lon, typ, bkg, 
     *                     eob, spval)
c
c        ..update impact data arrays
c
         do i = 1, n_grd
            if (sen(i) .gt. spmis .and. bkg(i) .gt. spmis) then
               n_out = n_out + 1
               anm = abs (val(i) - bkg(i)) / eob(i)
               ob_imp(n_out) = anm * sen(i)
               ob_lat(n_out) = lat(i)
               ob_lon(n_out) = lon(i)
               ob_sen(n_out) = sen(i)
               ob_typ(n_out) = typ(i)
               ob_xi(n_out)  = xi(i)
               ob_yj(n_out)  = yj(i)
            endif
         enddo
         write (*, '(11x, ''total obs: '', i10)') n_out
         endif
c
c        ..clean up data file arrays
c
         deallocate (bkg, eob, fail, lat, lon, sen, typ, val)
         deallocate (xi, yj)
      enddo
      enddo
c
c-----------------------------------------------------------------------
c
c     ..form maps
c
      fno = 0
      write (tmp_name, '(''sfc_map_impacts.'', a, ''.gmeta '')') dtg2
      write (*, '(4x, ''output file name: '', a)') trim (tmp_name)
      call init_gks ('opn', tmp_name)
      call map_impact_sfc (N_SAT, N_TYP, sat_typ, sat_nam, n_lon, 
     *                     n_lat, n_obs, n_out, ob_imp, ob_lat, 
     *                     ob_lon, ob_sen, ob_typ, ob_xi, ob_yj,
     *                     plot_title, n_proj, rlat, stdlt1,
     *                     stdlt2, stdlon, bl, br, tl, tr, fno)
      call init_gks ('cls', tmp_name)
c
c     ..form histograms
c
      write (tmp_name, '(''sfc_hst_impacts.'', a, ''.gmeta '')') dtg2
      write (*, '(4x, ''output file name: '', a)') trim (tmp_name)
      call init_gks ('opn', tmp_name)
      call impact_hist_sfc (N_SAT, N_TYP, sat_typ, sat_nam, n_obs, 
     *                      n_out, ob_imp, ob_sen, ob_typ, 
     *                      plot_title, area, fno)
      call init_gks ('cls', tmp_name)
c
c-----------------------------------------------------------------------
c
c     ..clean up data arrays
c
      deallocate (file_dtg, fld, ob_imp, ob_lat, ob_lon, ob_sen)
      deallocate (ob_typ, ob_xi, ob_yj)
c
      return
      end
