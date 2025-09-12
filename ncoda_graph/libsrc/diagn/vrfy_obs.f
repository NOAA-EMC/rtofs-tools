      subroutine vrfy_obs (title1, out_dir, dtg1, dtg2, nest, n_lon,
     *                     n_lat, upd, n_proj, rlat, stdlt1, stdlt2,
     *                     stdlon, bl, br, tl, tr, zoom)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vrfy_obs
c
c DESCRIPTION:  collects data for verification of observation time
c               series
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
      integer    UNIT
      parameter (UNIT = 20)
c
c     ..local array dimensions
c
      real      bl(2), br(2)
      character date1 * 11
      character date2 * 11
      integer   day
      character dtg * 10
      character dtg1 * 10
      character dtg2 * 10
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   hrs
      integer   i, k, ko, n
      integer   len, len_dir
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   n_data
      integer   n_files
      integer   n_lat
      integer   n_lon
      integer   n_obs
      integer   n_proj
      integer   n_var
      integer   nest
      character opt * 3
      character out_dir * (*)
      real      rlat
      integer   status
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      real      time
      character title1 * 80
      character title3 * 256
      real      tl(2), tr(2)
      integer   upd
      real      x, y
      integer   year
      real      zoom (4)
c
c     ..dummy variables
c
      real      clm, dmy
      real      ebk, eob
      integer   ndx
      character sgn * 7
      real      xi, yj, zk
c
c     ..allocatable global arrays
c
      real,     allocatable :: ob_anl (:)
      real,     allocatable :: ob_bkg (:)
      real,     allocatable :: ob_lat (:)
      real,     allocatable :: ob_lon (:)
      real,     allocatable :: ob_lvl (:)
      real,     allocatable :: ob_tim (:)
      integer,  allocatable :: ob_typ (:)
      real,     allocatable :: ob_val (:)
      integer,  allocatable :: ob_var (:)
c
c     ..allocatable local arrays
c
      real,     allocatable :: age (:)
      real,     allocatable :: anl (:)
      real,     allocatable :: bkg (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      real,     allocatable :: lvl (:)
      integer,  allocatable :: typ (:)
      real,     allocatable :: val (:)
      integer,  allocatable :: var (:)
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
c     ..diagnostics
c
      write (*, '(/, ''Observation Space Verification'')')
c
c     ..initialization
c
      file_typ = 'obsdata'
      fld_name = 'ocnobs'
      fluid = 'o'
      len_dir = len_trim (out_dir) + 1
      lvl_typ = 'sfc'
      n_var = 2
      tau = 0
c     call dtgmod (dtg2, -upd, dtg, status)
c
c     ..form date time group labels
c
      read (dtg1(1:4), '(i4)') year
      read (dtg1(5:6), '(i2)') mon
      read (dtg1(7:8), '(i2)') day
      write (date1, '(i2, 1x, a, 1x, i4)') day, month(mon), year
c
      read (dtg2(1:4), '(i4)') year
      read (dtg2(5:6), '(i2)') mon
      read (dtg2(7:8), '(i2)') day
      write (date2, '(i2, 1x, a, 1x, i4)') day, month(mon), year
c*********************************
      write (*,'(''dtg1: '',a,3x,a)') dtg1,date1
      write (*,'(''dtg2: '',a,3x,a)') dtg2,date2
c***********************************
c
c     ..set title
c
      title3 = date1 // ' to ' // date2
c
c     ..set number files to process
c
      call dtgdif (dtg1, dtg2, hrs, status)
      n_files = hrs / upd + 1
c**********************
      write (*,'(''hrs: '',a,2x,a,2i10)') dtg1,dtg2,hrs,n_files
c***********************8
c
c     ..count number obs
c
      n_obs = 0
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg1, hrs, dtg, status)
         call cr_fname (out_dir, dtg, nest, n_lon, n_lat,
     *                  file_typ, fld_name, fluid,
     *                  lvl_typ, tau, file_name, len)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            write (*, '(''        read restart: '', a)')
     *             file_name(len_dir:len)
            open (UNIT, file=file_name(1:len), status='unknown',
     *                  form='unformatted')
            read (UNIT) n_data
            close (UNIT)
            n_obs = n_obs + n_data
         else
            write (*, '(''     restart missing: '', a)')
     *             file_name(len_dir:len)
         endif
      enddo
c
c     ..check for data
c
      write (*, '(''          number obs: '', i10)') n_obs
      if (n_obs .eq. 0) return
c
c     ..allocate global arrays
c
      allocate (ob_anl (n_obs))
      allocate (ob_bkg (n_obs))
      allocate (ob_lat (n_obs))
      allocate (ob_lon (n_obs))
      allocate (ob_lvl (n_obs))
      allocate (ob_tim (n_obs))
      allocate (ob_typ (n_obs))
      allocate (ob_val (n_obs))
      allocate (ob_var (n_obs))
c
c     ..load data arrays
c
      ko = 0
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg1, hrs, dtg, status)
         call cr_fname (out_dir, dtg, nest, n_lon, n_lat,
     *                  file_typ, fld_name, fluid,
     *                  lvl_typ, tau, file_name, len)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            open (UNIT, file=file_name(1:len), status='unknown',
     *                  form='unformatted')
            read (UNIT) n_data, time
c
            allocate (age (n_data))
            allocate (anl (n_data))
            allocate (bkg (n_data))
            allocate (lat (n_data))
            allocate (lon (n_data))
            allocate (lvl (n_data))
            allocate (typ (n_data))
            allocate (val (n_data))
            allocate (var (n_data))
c
            read (unit) age(1:n_data)
            read (unit) lat(1:n_data)
            read (unit) lon(1:n_data)
            read (unit) lvl(1:n_data)
            read (unit) ndx
            read (unit) ebk
            read (unit) eob
            read (unit) typ(1:n_data)
            read (unit) var(1:n_data)
            read (unit) val(1:n_data)
            read (unit) anl(1:n_data)
            read (unit) bkg(1:n_data)
            read (unit) xi
            read (unit) yj
            read (unit) zk
            read (unit) sgn
            read (unit) clm
            read (unit) dmy
            close (UNIT)
c
            do i = 1, n_data
               if (zoom(1) .gt. -990.) then
                  x = lon(i)
                  y = lat(i)
                  if (x .gt. 180.) x = x - 360.
                  if (y .lt. zoom(1)) cycle
                  if (y .gt. zoom(2)) cycle
                  if (x .lt. zoom(3)) cycle
                  if (x .gt. zoom(4)) cycle
               endif
c***********************
               opt = 'prf'
c              opt = 'sfc'
               if (typ(i) .eq. 36 .or. typ(i) .eq. 37) then
c              if (typ(i) .eq. 102 .or. typ(i) .eq. 103) then
c              if (typ(i) .eq.  36 .or. typ(i) .eq.  37 .or.
c    *             typ(i) .eq. 102 .or. typ(i) .eq. 103) then
c              if (lvl(i) .lt. 1.) then
c********************
                  ko = ko + 1
                  ob_tim(ko) = age(i) + time
                  ob_lat(ko) = lat(i)
                  ob_lon(ko) = lon(i)
                  ob_lvl(ko) = lvl(i)
                  ob_typ(ko) = typ(i)
                  ob_val(ko) = val(i)
                  ob_bkg(ko) = bkg(i)
                  ob_anl(ko) = anl(i)
                  ob_var(ko) = var(i)
               endif
            enddo
c
c           ..clean up
c
            deallocate (age, anl, bkg, lat, lon, lvl, typ, val, var)
         endif
      enddo
c
c     ..set data counter
c
      n_data = ko
      write (*, '(9x, ''number data: '', i10)') n_data
      if (n_data .gt. 0) then
c
c     ..generate diagnostic plots
c
      call clsgks
      call vrfy_map (n_var, dtg, title1, title3, n_obs, n_data, ob_lat, 
     *               ob_lon, ob_var, n_proj, rlat, stdlt1, stdlt2,
     *               stdlon, bl, br, tl, tr, opt)
      call vrfy_sctr (n_var, dtg, title1, title3, n_obs, n_data, ob_var,
     *                ob_val, ob_bkg, ob_anl, opt)
c     call vrfy_time (n_var, dtg, title1, title3, n_files, dtg1, dtg2, 
c    *                upd, n_obs, n_data, ob_var, ob_val, ob_bkg,
c    *                ob_anl, ob_tim)
      if (opt .eq. 'prf') then
         call vrfy_rgn (n_var, dtg, title1, title3, n_obs, n_data,
     *                  ob_lvl, ob_typ, ob_var, ob_val, ob_bkg,
     *                  ob_anl)
      endif
c     call vrfy_vert (n_var, dtg, title1, title3, n_obs, n_data, ob_typ, 
c    *                ob_val, ob_bkg, ob_anl, ob_lvl)
      call opngks
      endif
c
c     ..clean up
c
      deallocate (ob_anl, ob_bkg, ob_lat, ob_lon, ob_lvl)
      deallocate (ob_tim, ob_typ, ob_val, ob_var)
c
      return
      end
