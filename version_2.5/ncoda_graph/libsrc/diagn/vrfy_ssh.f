      subroutine vrfy_ssh (title1, data_dir, clim_dir, out_dir, dtg1,
     *                     dtg2, nest, n_lon, n_lat, msk, upd, gln,
     *                     glt, pln, plt, n_proj, rlat, stdlt1, stdlt2,
     *                     stdlon, bl, br, tl, tr, i1, i2, j1, j2)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vrfy_ssh
c
c DESCRIPTION:  collects sla observations and compares against hycom
c               sla computed from hycom forecast and mean ssh
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
      integer   n_lat
      integer   n_lon
      integer   n_lvl
c
      integer   gln, glt
      integer   pln, plt
c
      real      bl(2), br(2)
      character clim_dir * (*)
      character data_dir * (*)
      character date1 * 11
      character date2 * 11
      integer   day
      character dtg * 10
      character dtg1 * 10
      character dtg2 * 10
      logical   exist
      character file_dtg * 10
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   hrs
      integer   i, k, ko, n
      integer   i1, i2, j1, j2
      integer   io, jo
      integer   jday
      integer   len, len_dir
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   msk (n_lon, n_lat)
      integer   n_data
      integer   n_day
      integer   n_files
      integer   n_obs
      integer   n_proj
      integer   n_sfc
      integer   n_strt
      integer   nest
      integer   nodes
      character out_dir * (*)
      real      qc_lmt
      logical   rd_fail
      real      rlat
      real      spmis, spval
      integer   status
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      real      time, time1, time2
      character title1 * 80
      character title3 * 256
      real      tl(2), tr(2)
      real      topo_mn
      integer   upd
      integer   vrsn
      real      x, y
      integer   year
c
c     ..dummy variables
c
      integer   cyc, ltc, smp, ssh, trk, typ
      character rcp * 14
      character otm * 14
c
c     ..allocate arrays
c
      real,     allocatable :: grd_lat (:,:)
      real,     allocatable :: grd_lon (:,:)
      real,     allocatable :: node_eq (:,:)
      real,     allocatable :: node_nh (:,:)
      real,     allocatable :: node_sh (:,:)
      real,     allocatable :: wrk (:)
c
c     ..allocatable obs arrays
c
      real,     allocatable :: ob_age (:)
      real,     allocatable :: ob_anm (:)
      real,     allocatable :: ob_fcst (:)
      real,     allocatable :: ob_lat (:)
      real,     allocatable :: ob_lon (:)
      real,     allocatable :: ob_mean (:)
      real,     allocatable :: ob_sla (:)
      real,     allocatable :: ob_xi (:)
      real,     allocatable :: ob_yj (:)
c
c     ..allocatable global obs arrays
c
      real,     allocatable :: age (:)
      logical,  allocatable :: fail (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      integer,  allocatable :: ndx (:)
      real,     allocatable :: qc (:) 
      real,     allocatable :: sla (:)
      real,     allocatable :: topo (:)
      real,     allocatable :: val (:) 
      real,     allocatable :: xi (:)
      real,     allocatable :: yj (:)
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
      write (*, '(/, ''SSH Observation Space Verification'')')
c
c     ..initialization
c
      file_typ = 'obsdata'
      fld_name = 'ocnobs'
      fluid = 'o'
      len_dir = len_trim (data_dir)
      lvl_typ = 'sfc'
      nodes = n_lon * n_lat
      n_day = 0
      n_sfc = 1
      n_strt = 1
      qc_lmt = 4.
      spval = -999.
      spmis = spval + 9.
      tau = 0
      topo_mn = 400.
      call dtgmod (dtg2, -upd, dtg, status)
c
c     ..form date time group labels
c
      read (dtg1(1:4), '(i4)') year
      read (dtg1(5:6), '(i2)') mon
      read (dtg1(7:8), '(i2)') day
      write (date1, '(i2, 1x, a, 1x, i4)') day, month(mon), year
c
      read (dtg(1:4), '(i4)') year
      read (dtg(5:6), '(i2)') mon
      read (dtg(7:8), '(i2)') day
      write (date2, '(i2, 1x, a, 1x, i4)') day, month(mon), year
c
c     ..set title
c
      title3 = date1 // ' to ' // date2
c
c     ..set number files to process
c
      call dtgdif (dtg1, dtg2, hrs, status)
      n_files = hrs / upd
c
c     ..count number obs
c
      n_obs = 0
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg1, hrs, dtg, status)
         file_name = data_dir(1:len_dir) // '/ssh/' // dtg // '.ssh'
         len = len_trim (file_name)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            open (UNIT, file=file_name(1:len), status='unknown',
     *                  form='unformatted')
            read (UNIT) n_data
            close (UNIT)
            n_obs = n_obs + n_data
         else
            write (*, '(''    ssh file missing: '', a)')
     *             file_name(len_dir+1:len)
         endif
      enddo
c
c     ..check for data
c
      write (*, '(''    total number obs: '', i10)') n_obs
      if (n_obs .eq. 0) return
c
c     ..allocate global arrays
c
      allocate (ob_age (n_obs))
      allocate (ob_anm (n_obs))
      allocate (ob_fcst (n_obs))
      allocate (ob_lat (n_obs))
      allocate (ob_lon (n_obs))
      allocate (ob_mean (n_obs))
      allocate (ob_sla (n_obs))
      allocate (ob_xi (n_obs))
      allocate (ob_yj (n_obs))
c
c     ..load data arrays
c
      ko = 0
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg1, hrs, dtg, status)
         file_name = data_dir(1:len_dir) // '/ssh/' // dtg // '.ssh'
         len = len_trim (file_name)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            write (*, '(''       read ssh file: '', a)')
     *             file_name(len_dir+1:len)
            open (UNIT, file=file_name(1:len), status='unknown',
     *                  form='unformatted')
            read (UNIT) n_data, n_lvl, vrsn
c
            allocate (age (n_data))
            allocate (lat (n_data))
            allocate (lon (n_data))
            allocate (qc (n_data))
            allocate (sla (n_data))
c
            read (unit) age(1:n_data)
            read (unit) cyc
            read (unit) lat(1:n_data)
            read (unit) lon(1:n_data)
            read (unit) qc(1:n_data)
            read (unit) typ
            read (unit) smp
            read (unit) ssh
            read (unit) trk
            read (unit) ltc
            read (unit) otm
            read (unit) rcp
            if (vrsn .gt. 1) then
               read (unit) sla(1:n_data)
            else
               write (*,'(''ERROR: wrong data version'')')
               stop
            endif
            close (UNIT)
c
            do i = 1, n_data
               x = lon(i)
               y = lat(i)
               if (x .gt. 180.) x = x - 360.
               if (y .lt. bl(1)) cycle
               if (y .gt. tr(1)) cycle
               if (x .lt. bl(2)) cycle
               if (x .gt. tr(2)) cycle
               if (qc(i) .lt. qc_lmt) then
                  ko = ko + 1
                  ob_age(ko) = age(i)
                  ob_lat(ko) = lat(i)
                  ob_lon(ko) = lon(i)
                  ob_sla(ko) = sla(i)
               endif
            enddo
c
c           ..clean up
c
            deallocate (age, lat, lon, qc, sla)
         endif
      enddo
c
c     ..set data counter
c
      n_data = ko
      write (*, '(9x, ''number data: '', i10)') n_data
      if (n_data .eq. 0) return
c
c     ..allocate arrays
c
      allocate (grd_lat (n_lon, n_lat))
      allocate (grd_lon (n_lon, n_lat))
      allocate (node_eq ((gln * glt), 2))
      allocate (node_nh ((pln * plt), 2))
      allocate (node_sh ((pln * plt), 2))
      allocate (wrk (n_lon * n_lat))
c
      allocate (fail (n_obs))
      allocate (ndx (n_obs))
      allocate (topo (n_obs))
      allocate (val (n_obs))
      allocate (xi (n_obs))
      allocate (yj (n_obs))
c
c     ..initialize
c
      ob_anm = spval
      ob_fcst = spval
      ob_mean = spval
      ob_xi = spval
      ob_yj = spval
c
c     ..retrieve grid lat,lon and ssh mean
c
      call rd_coda_file (out_dir, dtg1, nest, n_lon, n_lat, n_sfc,
     *                   'datafld', 'grdlat', 'o', 0, 'sfc', grd_lat,
     *                   .true., rd_fail)
      call rd_coda_file (out_dir, dtg1, nest, n_lon, n_lat, n_sfc,
     *                   'datafld', 'grdlon', 'o', 0, 'sfc', grd_lon, 
     *                   .true., rd_fail)
      call rd_ssh_mean (clim_dir, n_obs, n_data, ob_lat, ob_lon,
     *                  ob_mean)
c
c     ..convert obs lat,lon to hycom i,j
c
      call irreg_ll2ij (n_lon, n_lat, grd_lat, grd_lon, n_data,
     *                  ob_lat, ob_lon, ob_xi, ob_yj)
c
c     ..retrieve smoothed bathymetry
c
      call rd_topog (clim_dir, n_obs, n_data, ob_lat, ob_lon, topo)
c
c     ..read forecasts, interpolate to obs locations
c
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg1, hrs, dtg, status)
         call dtgmod (dtg, -upd, file_dtg, status)
         call dtg_time (dtg, time, jday)
         call rd_coda_file (out_dir, file_dtg, nest, n_lon, n_lat,
     *                      n_sfc, 'fcstfld', 'seahgt', 'o', upd,
     *                      'sfc', wrk, .true., rd_fail)
         if (.not. rd_fail) then
            time1 = time - 12.
            time2 = time + 12.
            fail = .false.
            ndx = 0
            val = spval
            n = 0
            do i = 1, n_data
               if (ob_age(i).ge.time1 .and. ob_age(i).le.time2) then
                  if (topo(i) .gt. topo_mn) then
                     n = n + 1
                     ndx(n) = i
                     xi(n) = ob_xi(i)
                     yj(n) = ob_yj(i)
                  endif
               endif
            enddo
            if (n .gt. 0) then
               call fld2_trp (n_obs, n_strt, n, xi, yj, n_lon, n_lat, 
     *                        wrk, spval, val, fail)
               do i = 1, n
                  if (.not. fail(i) .and. ndx(i) .gt. 0) then
                     ob_fcst(ndx(i)) = val(i)
                  endif
               enddo
               n_day = n_day + n
            endif
         endif
      enddo
      write (*, '(''   number daily data: '', 2i10)') n_day, n_data
c
c     ..form forecast sla and model OmF
c
      do i = 1, n_data
         if (ob_fcst(i) .gt. spmis) then
             ob_fcst(i) = ob_fcst(i) - ob_mean(i)
             ob_anm(i) = ob_sla(i) - ob_fcst(i)
         else
             ob_sla(i) = spval
         endif
      enddo
c
c     ..set up irregular plot grid
c
      if (n_proj .lt. 0) then
         call glb_map (out_dir, clim_dir, dtg2, n_lon, n_lat, gln,
     *                 glt, node_eq, pln, plt, node_nh, node_sh)
      else
         node_eq = 0.
         node_nh = 0.
         node_sh = 0.
      endif
c
c     ..generate diagnostic plots
c
      call clsgks
      call vrfy_ssh_map (dtg, title1, title3, n_obs, n_data, ob_lat, 
     *                   ob_lon, topo, topo_mn,  n_proj, rlat, stdlt1,
     *                   stdlt2, stdlon, bl, br, tl, tr)
      call vrfy_ssh_rgn (dtg, title1, title3, n_lon, n_lat, msk,
     *                   n_obs, n_data, ob_anm, ob_fcst, ob_sla,
     *                   ob_xi, ob_yj, bl, br, tl, tr, i1, i2, 
     *                   j1, j2, gln, glt, node_eq, spval)
      call opngks
c
c     ..clean up
c
      deallocate (ob_age, ob_anm, ob_fcst, ob_lat, ob_lon, ob_mean)
      deallocate (ob_sla, ob_xi, ob_yj)
      deallocate (fail, ndx, topo, val, xi, yj)
      deallocate (grd_lat, grd_lon, wrk)
      deallocate (node_eq, node_nh, node_sh)
c
      return
      end
