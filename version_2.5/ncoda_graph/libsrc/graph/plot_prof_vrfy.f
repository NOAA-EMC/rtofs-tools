      subroutine plot_prof_vrfy (dir_path, dtg, nest, n_lon, n_lat,
     *                           n_lvl, zoom, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_prof_vrfy
c
c DESCRIPTION:  plot obs verification profiles
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
      integer    N_SYS
      parameter (N_SYS = 6)
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
      character dir_path * (*)
      real      dmy
      character dtg * 10
      real      ebk, eob
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j, k, n
      integer   jday
      integer   len, len_dir
      character lvl_typ * 3
      integer   n_data
      integer   n_obs
      integer   n_prf
      integer   n_sfc
      integer   nd, np
      integer   nest
      character plt_name * 80
      real      spmis, spval
      character sys_lbl (N_SYS) * 6
      integer   sys_sal (N_SYS)
      integer   sys_tmp (N_SYS)
      integer   tau
      real      time
      real      xi, yj, zk 
      real      zoom (4)
c
c     ..allocatable innovation vector arrays
c
      real,     allocatable :: ob_age (:)
      real,     allocatable :: ob_anl (:)
      real,     allocatable :: ob_bkg (:)
      real,     allocatable :: ob_clm (:)
      real,     allocatable :: ob_lat (:)
      real,     allocatable :: ob_lon (:)
      real,     allocatable :: ob_lvl (:)
      integer,  allocatable :: ob_ndx (:)
      character,allocatable :: ob_sgn (:) * 7
      integer,  allocatable :: ob_typ (:)
      real,     allocatable :: ob_val (:)
      integer,  allocatable :: ob_var (:)
c
      real,     allocatable :: age (:)
      real,     allocatable :: anl (:)
      real,     allocatable :: bkg (:)
      real,     allocatable :: clm (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      real,     allocatable :: lvl (:)
      integer,  allocatable :: ndx (:)
      character,allocatable :: sgn (:) * 7
      integer,  allocatable :: typ (:)
      real,     allocatable :: val (:)
      integer,  allocatable :: var (:)
c
c     ..allocatable profile arrays
c
      real,     allocatable :: prf_age (:)
      real,     allocatable :: prf_anl (:,:)
      real,     allocatable :: prf_bkg (:,:)
      real,     allocatable :: prf_clm (:,:)
      real,     allocatable :: prf_lat (:)
      real,     allocatable :: prf_lon (:)
      real,     allocatable :: prf_lvl (:,:)
      integer,  allocatable :: prf_nd (:)
      character,allocatable :: prf_sgn (:) * 7
      integer,  allocatable :: prf_typ (:,:)
      real,     allocatable :: prf_val (:,:)
      integer,  allocatable :: prf_var (:)
c
c     ..allocatable work arrays
c
      real,     allocatable :: wrk_age (:)
      real,     allocatable :: wrk_anl (:)
      real,     allocatable :: wrk_bkg (:)
      real,     allocatable :: wrk_clm (:)
      real,     allocatable :: wrk_lat (:)
      real,     allocatable :: wrk_lon (:)
      real,     allocatable :: wrk_lvl (:)
      character,allocatable :: wrk_sgn (:) * 7
      integer,  allocatable :: wrk_typ (:)
      real,     allocatable :: wrk_val (:)
      integer,  allocatable :: wrk_var (:)
c
      include 'color_table.h'
c
c     ..define sys labels
c
      data      sys_lbl / 'argo  ', 'glider', 'xbt   ',
     *                    'buoy  ', 'animal', 'ssh   ' /
c
c     ..define sys data types
c
      data      sys_tmp / 36, 102,   1,   4, 133,  19 /
      data      sys_sal / 37, 103,  -1,  52, 134,  49 /
c
c...............................executable..............................
c
c     ..initialization
c
      file_typ = 'obsdata'
      fluid = 'o'
      fld_name = 'ocnobs'
      fno = 0
      lvl_typ = 'sfc'
      spval = -999.
      spmis = spval + 9.
      tau = 0
c
c     ..set time reference
c
      call dtg_time (dtg, time, jday)
c
c     ..set color table
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..check for file existence
c
      call cr_fname (dir_path, dtg, nest, n_lon, n_lat, file_typ,
     *               fld_name, fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) then
         write (*, '(''file missing: '', a)') trim (file_name)
         return
      else
          len_dir = len_trim (dir_path) + 1
          write (*, '(11x, ''read file: '', a)') file_name(len_dir:len)
      endif
c
c     ..open vector file, read number obs
c
      open (UNIT, file=file_name(1:len), status='unknown',
     *            form='unformatted')
      read (UNIT) n_obs
      if (n_obs .eq. 0) then
         write (*, '(''number obs zero'')')
         return
      else
          write (*, '(10x, ''number obs: '', i10)') n_obs
      endif
c
c     ..allocate vector arrays
c
      allocate (ob_age (n_obs))
      allocate (ob_anl (n_obs))
      allocate (ob_bkg (n_obs))
      allocate (ob_clm (n_obs))
      allocate (ob_lat (n_obs))
      allocate (ob_lon (n_obs))
      allocate (ob_lvl (n_obs))
      allocate (ob_ndx (n_obs))
      allocate (ob_sgn (n_obs))
      allocate (ob_typ (n_obs))
      allocate (ob_val (n_obs))
      allocate (ob_var (n_obs))
c
      allocate (age (n_obs))
      allocate (anl (n_obs))
      allocate (bkg (n_obs))
      allocate (clm (n_obs))
      allocate (lat (n_obs))
      allocate (lon (n_obs))
      allocate (lvl (n_obs))
      allocate (ndx (n_obs))
      allocate (sgn (n_obs))
      allocate (typ (n_obs))
      allocate (val (n_obs))
      allocate (var (n_obs))
c
c     ..read vectors
c
      read (UNIT) ob_age(1:n_obs)
      read (UNIT) ob_lat(1:n_obs)
      read (UNIT) ob_lon(1:n_obs)
      read (UNIT) ob_lvl(1:n_obs)
      read (UNIT) ob_ndx(1:n_obs)
      read (UNIT) ebk 
      read (UNIT) eob
      read (UNIT) ob_typ(1:n_obs)
      read (UNIT) ob_var(1:n_obs)
      read (UNIT) ob_val(1:n_obs)
      read (UNIT) ob_anl(1:n_obs)
      read (UNIT) ob_bkg(1:n_obs)
      read (UNIT) xi
      read (UNIT) yj
      read (UNIT) zk
      read (UNIT) ob_sgn(1:n_obs)
      read (UNIT) ob_clm(1:n_obs)
      read (UNIT) dmy
      close (UNIT)
c
c     ..close gks
c
      call clsgks
c
c     ..loop over analysis variables
c
      do j = 1, 2
c
c        ..save analysis variable
c
         n_data = 0
         do i = 1, n_obs
            if (ob_var(i) .eq. j) then
               n_data = n_data + 1
               age(n_data) = ob_age(i)
               lat(n_data) = ob_lat(i)
               lon(n_data) = ob_lon(i)
               lvl(n_data) = ob_lvl(i)
               ndx(n_data) = ob_ndx(i)
               typ(n_data) = ob_typ(i)
               var(n_data) = ob_var(i)
               val(n_data) = ob_val(i)
               anl(n_data) = ob_anl(i)
               bkg(n_data) = ob_bkg(i)
               sgn(n_data) = ob_sgn(i)
               clm(n_data) = ob_clm(i)
            endif
         enddo
         if (n_data .eq. 0) cycle
         write (*, '(9x, ''number data: '', i10)') n_data
c
c        ..count number surface obs, set number prf obs
c
         n_sfc = 0
         do i = 2, n_data
            if (ndx(i) .eq. ndx(i-1)) then
               n_sfc = i - 1
               exit
            endif
         enddo
         n_prf = n_data - n_sfc + 1
         write (*, '(9x, ''number prof: '', i10)') n_prf
c
c        ..allocate work arrays
c
         allocate (wrk_age (n_lvl))
         allocate (wrk_anl (n_lvl))
         allocate (wrk_bkg (n_lvl))
         allocate (wrk_clm (n_lvl))
         allocate (wrk_lat (n_lvl))
         allocate (wrk_lon (n_lvl))
         allocate (wrk_lvl (n_lvl))
         allocate (wrk_sgn (n_lvl))
         allocate (wrk_typ (n_lvl))
         allocate (wrk_val (n_lvl))
         allocate (wrk_var (n_lvl))
c
c        ..allocate profile arrays
c
         allocate (prf_age (n_prf))
         allocate (prf_anl (n_lvl, n_prf))
         allocate (prf_bkg (n_lvl, n_prf))
         allocate (prf_clm (n_lvl, n_prf))
         allocate (prf_lat (n_prf))
         allocate (prf_lon (n_prf))
         allocate (prf_lvl (n_lvl, n_prf))
         allocate (prf_nd  (n_prf))
         allocate (prf_sgn (n_prf))
         allocate (prf_typ (n_lvl, n_prf))
         allocate (prf_val (n_lvl, n_prf))
         allocate (prf_var (n_prf))
c
c        ..initialize
c
         do i = 1, n_prf
            prf_age(i) = 0.
            prf_lat(i) = 0.
            prf_lon(i) = 0.
            prf_nd(i) = 0
            prf_sgn(i) = '       '
            prf_var(i) = 0
            do k = 1, n_lvl
               prf_anl(k,i) = spval
               prf_bkg(k,i) = spval
               prf_clm(k,i) = spval
               prf_lvl(k,i) = spval
               prf_typ(k,i) = 0
               prf_val(k,i) = spval
            enddo
         enddo
c
c        ..roll up profiles
c
         nd = 0
         np = 0
         do i = n_sfc, n_data
            if (ndx(i) .eq. ndx(i-1)) then
               nd = nd + 1
               wrk_age(nd) = age(i)
               wrk_anl(nd) = anl(i)
               wrk_bkg(nd) = bkg(i)
               wrk_clm(nd) = clm(i)
               wrk_lat(nd) = lat(i)
               wrk_lon(nd) = lon(i)
               wrk_lvl(nd) = lvl(i)
               wrk_sgn(nd) = sgn(i)
               wrk_typ(nd) = typ(i)
               wrk_val(nd) = val(i)
               wrk_var(nd) = var(i)
            else
               np = np + 1
               prf_age(np) = wrk_age(1) + time
               prf_lat(np) = wrk_lat(1)
               prf_lon(np) = wrk_lon(1)
               prf_nd(np)  = nd
               prf_sgn(np) = wrk_sgn(1)
               prf_var(np) = wrk_var(1)
               do k = 1, nd
                  prf_anl(k,np) = wrk_anl(k)
                  prf_bkg(k,np) = wrk_bkg(k)
                  prf_clm(k,np) = wrk_clm(k)
                  prf_lvl(k,np) = wrk_lvl(k)
                  prf_typ(k,np) = wrk_typ(k)
                  prf_val(k,np) = wrk_val(k)
               enddo
c
               nd = 1
               wrk_age(nd) = age(i)
               wrk_anl(nd) = anl(i)
               wrk_bkg(nd) = bkg(i)
               wrk_clm(nd) = clm(i)
               wrk_lat(nd) = lat(i)
               wrk_lon(nd) = lon(i)
               wrk_lvl(nd) = lvl(i)
               wrk_sgn(nd) = sgn(i)
               wrk_typ(nd) = typ(i)
               wrk_val(nd) = val(i)
               wrk_var(nd) = var(i)
            endif
         enddo
c
c        ..loop over observing systems
c
         do n = 1, 2
         if (j .eq. 1) then
c
c           ..temperature
c
            write (*, '(/, 4x, a, '' tmp profile verification'')')
     *             trim (sys_lbl(n))
            write (plt_name, '(a, ''_tmp.'', a, ''.gmeta '')')
     *             trim (sys_lbl(n)), dtg
            write (*, '(4x, ''output file name: '', a)') 
     *             trim (plt_name)
            fno = 0
            call init_gks ('opn', plt_name)
            call prof_vrfy ('TMP', n_prf, n_lvl, np, prf_age, prf_anl,
     *                      prf_bkg, prf_clm, prf_lat, prf_lon, prf_lvl,
     *                      prf_nd, prf_sgn, prf_typ, prf_val, zoom,
     *                      sys_tmp(n), j, fno)
            call init_gks ('cls', plt_name)
         else if (j .eq. 2) then
c
c           ..salinity
c
            write (*, '(/, 4x, a, '' sal profile verification'')')
     *             trim (sys_lbl(n))
            write (plt_name, '(a, ''_sal.'', a, ''.gmeta '')')
     *             trim (sys_lbl(n)), dtg
            write (*, '(4x, ''output file name: '', a)')
     *             trim (plt_name)
            fno = 0
            call init_gks ('opn', plt_name)
            call prof_vrfy ('SAL', n_prf, n_lvl, np, prf_age, prf_anl,
     *                      prf_bkg, prf_clm, prf_lat, prf_lon, prf_lvl,
     *                      prf_nd, prf_sgn, prf_typ, prf_val, zoom,
     *                      sys_sal(n), j, fno)
            call init_gks ('cls', plt_name)
         endif
         enddo
c
c        ..clean up
c
         deallocate (prf_age, prf_anl, prf_bkg, prf_clm, prf_lat)
         deallocate (prf_lon, prf_lvl, prf_nd, prf_sgn, prf_typ)
         deallocate (prf_val, prf_var)
         deallocate (wrk_age, wrk_anl, wrk_bkg, wrk_clm, wrk_lat)
         deallocate (wrk_lon, wrk_lvl, wrk_sgn, wrk_typ, wrk_val)
         deallocate (wrk_var)
      enddo
c
c     if (zoom(1) .gt. -990.) then
c        n = 6
c
c        ..temperature
c
c        write (*, '(/, 4x, a, '' tmp profile verification'')')
c    *          trim (sys_lbl(n))
c        write (plt_name, '(a, ''_tmp.'', a, ''.gmeta '')')
c    *          trim (sys_lbl(n)), dtg
c        write (*, '(4x, ''output file name: '', a)') trim (plt_name)
c        fno = 0
c        call init_gks ('opn', plt_name)
c        call prof_vrfy ('TMP', n_obs, n_lvl, np, prf_age, prf_anl,
c    *                   prf_bkg, prf_clm, prf_lat, prf_lon, prf_lvl,
c    *                   prf_nd, prf_sgn, prf_typ, prf_val, prf_var,
c    *                   zoom, sys_tmp(n), fno)
c        call init_gks ('cls', plt_name)
c
c        ..salinity
c
c        write (*, '(/, 4x, a, '' sal profile verification'')')
c    *          trim (sys_lbl(n))
c        write (plt_name, '(a, ''_sal.'', a, ''.gmeta '')')
c    *          trim (sys_lbl(n)), dtg
c        write (*, '(4x, ''output file name: '', a)') trim (plt_name)
c        fno = 0
c        call init_gks ('opn', plt_name)
c        call prof_vrfy ('SAL', n_obs, n_lvl, np, prf_age, prf_anl,
c    *                   prf_bkg, prf_clm, prf_lat, prf_lon, prf_lvl,,
c    *                   prf_nd, prf_sgn, prf_typ, prf_val, prf_var,
c    *                   zoom, sys_sal(n), fno)
c        call init_gks ('cls', plt_name)
c     endif
      call opngks
c
c     ..clean up
c
      deallocate (age, anl, bkg, clm, lat, lon, lvl, sgn, typ)
      deallocate (val, var)
      deallocate (ob_age, ob_anl, ob_bkg, ob_clm, ob_lat, ob_lon)
      deallocate (ob_lvl, ob_sgn, ob_typ, ob_val, ob_var)
c
      return
      end
