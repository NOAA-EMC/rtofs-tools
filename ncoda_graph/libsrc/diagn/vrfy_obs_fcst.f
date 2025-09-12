      subroutine vrfy_obs_fcst (title1, out_dir, dtg1, dtg2, nest,
     *                          n_lon, n_lat, upd, n_proj, rlat,
     *                          stdlt1, stdlt2, stdlon, bl, br,
     *                          tl, tr, zoom)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vrfy_obs_fcst
c
c DESCRIPTION:  collects data for verification of forecasts using
c               observation time series
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
      integer    MX_FCST
      parameter (MX_FCST = 8)
      integer    MX_FCST_TAU
      parameter (MX_FCST_TAU = 192)
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
      character fcst_dtg (MX_FCST) * 10
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   hrs
      integer   i, j, k, ko, n
      integer   k_fcst
      integer   len, len_dir
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   n_data
      integer   n_fcst
      integer   n_files
      integer   n_lat
      integer   n_lon
      integer   n_obs
      integer   n_proj
      integer   n_var
      integer   nest
      character out_dir * (*)
      real      rlat
      real      spval
      integer   status
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      integer   tau_hr (MX_FCST)
      real      time
      character title1 * (*)
      character title3 * 256
      real      tl(2), tr(2)
      integer   upd
      real      x, y
      integer   year
      real      zoom (4)
c
c     ..dummy variables
c
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
      real,     allocatable :: ob_vfy (:,:)
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
      real,     allocatable :: vfy (:,:)
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
      write (*, '(/, ''Observation Space Forecast Verification'')')
c
c     ..initialization
c
      file_typ = 'obsfcst'
      fld_name = 'ocnobs'
      fluid = 'o'
      len_dir = len_trim (out_dir) + 1
      lvl_typ = 'sfc'
      n_fcst = 0
      n_var = 2
      spval = -999.
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
c
c     ..set title
c
      title3 = date1 // ' to ' // date2
c
c     ..set number files to process
c
      call dtgdif (dtg1, dtg2, hrs, status)
      n_files = hrs / upd + 1
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
     *                  access='sequential',form='unformatted')
            read (UNIT) k_fcst, n_data
            close (UNIT)
            n_obs = n_obs + n_data
            n_fcst = max (n_fcst, k_fcst)
         else
            write (*, '(''     restart missing: '', a)')
     *             file_name(len_dir:len)
         endif
      enddo
c
c     ..check for data
c
      write (*, '(''    number total obs: '', i10)') n_obs
      write (*, '(''         number fcst: '', i10)') n_fcst
c
      if (n_obs .eq. 0) return
      if (n_fcst .eq. 0) return
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
      allocate (ob_vfy (n_obs, n_fcst))
c
      ob_vfy = spval
c
c     ..load global data arrays
c
      ko = 0
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg1, hrs, dtg, status)
         call cr_fname (out_dir, dtg, nest, n_lon, n_lat,
     *                  file_typ, fld_name, fluid, lvl_typ,
     *                  tau, file_name, len)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            open (UNIT, file=file_name(1:len), status='unknown',
     *                  access='sequential',form='unformatted')
            read (UNIT) k_fcst, n_data, n_var, upd, time
            read (UNIT) fcst_dtg(1:k_fcst)
            read (UNIT) tau_hr(1:k_fcst)
c
            allocate (age (n_data))
            allocate (lat (n_data))
            allocate (lon (n_data))
            allocate (lvl (n_data))
            allocate (typ (n_data))
            allocate (val (n_data))
            allocate (var (n_data))
            allocate (vfy (n_data, k_fcst))
c
            read (unit) age(1:n_data)
            read (unit) lat(1:n_data)
            read (unit) lon(1:n_data)
            read (unit) lvl(1:n_data)
            read (unit) ndx
            read (unit) var(1:n_data)
            read (unit) typ(1:n_data)
            read (unit) sgn
            read (unit) val(1:n_data)
            do j = 1, k_fcst
               read (UNIT) vfy(1:n_data,j)
            enddo
            close (UNIT)
c
c           ..save time series
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
               if (typ(i) .eq. 36 .or. typ(i) .eq. 37) then
                  ko = ko + 1
                  ob_tim(ko) = age(i) + time
                  ob_lat(ko) = lat(i)
                  ob_lon(ko) = lon(i)
                  ob_lvl(ko) = lvl(i)
                  ob_typ(ko) = typ(i)
                  ob_val(ko) = val(i)
                  ob_var(ko) = var(i)
                  do j = 1, k_fcst
                     ob_vfy(ko,j) = vfy(i,j)
                  enddo
               endif
            enddo
c
c           ..clean up
c
            deallocate (age, lat, lon, lvl, typ, val, var, vfy)
         endif
      enddo
c
c     ..update obs counter
c
      n_data = ko
      write (*, '(''     number argo obs: '', i10)') n_data
c
c     ..generate diagnostic plots
c
      call clsgks
      call vrfy_rgn_fcst (n_var, dtg, title1, title3, n_obs, n_data,
     *                    n_fcst, ob_lvl, ob_typ, ob_val, ob_var,
     *                    ob_vfy)
      call opngks
c
c     ..clean up
c
      deallocate (ob_anl, ob_bkg, ob_lat, ob_lon, ob_lvl)
      deallocate (ob_tim, ob_typ, ob_val, ob_var)
c
      return
      end
