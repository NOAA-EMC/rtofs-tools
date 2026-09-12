      subroutine plot_prof_fcst (dir_path, dtg, nest, n_lon, n_lat,
     *                           n_lvl, zoom, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_prof_fcst
c
c DESCRIPTION:  plot obs forecast profiles
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
c     ..define number of forecast taus into the past (8 days)
c
      integer    MX_FCST
      parameter (MX_FCST = 8)
      integer    MX_FCST_TAU
      parameter (MX_FCST_TAU = 192)
c
      integer    N_SYS
      parameter (N_SYS = 6)
c
      integer    UNIT
      parameter (UNIT = 20)
c
c     ..local array dimensions
c
      integer   n_fcst
      integer   n_lat
      integer   n_lon
      integer   n_lvl
      integer   n_var
c
      character dir_path * (*)
      character dtg * 10
      logical   exist
      character fcst_dtg (MX_FCST) * 10
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j, k, n
      integer   jday
      integer   len, len_dir
      character lvl_typ * 3
      integer   mx_ndx
      integer   n_obs
      integer   n_prf
      integer   n_sfc
      integer   nd, np
      integer   nest
      real      spmis, spval
      character sys_lbl (N_SYS) * 6
      integer   sys_sal (N_SYS)
      integer   sys_tmp (N_SYS)
      integer   tau
      integer   tau_hr (MX_FCST)
      real      time
      character tmp_name * 80
      integer   upd
      real      zoom (4)
c
c     ..dummy forecast vector variables
c
      real      anl, bkg
      real      ebk, eob
      real      xi, yj, zk 
c
c     ..allocatable innovation vector arrays
c
      real,     allocatable :: age (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      real,     allocatable :: lvl (:)
      integer,  allocatable :: ndx (:)
      character,allocatable :: sgn (:) * 7
      integer,  allocatable :: typ (:)
      real,     allocatable :: val (:)
      integer,  allocatable :: var (:)
      real,     allocatable :: vfy (:,:)
c
c     ..allocatable profile arrays
c
      real,     allocatable :: prf_age (:)
      real,     allocatable :: prf_lat (:)
      real,     allocatable :: prf_lon (:)
      real,     allocatable :: prf_lvl (:,:)
      integer,  allocatable :: prf_nd (:)
      real,     allocatable :: prf_sal (:,:)
      real,     allocatable :: prf_sal_vfy (:,:,:)
      character,allocatable :: prf_sgn (:) * 7
      real,     allocatable :: prf_tmp (:,:)
      real,     allocatable :: prf_tmp_vfy (:,:,:)
      integer,  allocatable :: prf_typ (:,:)
      integer,  allocatable :: prf_var (:)
c
c     ..allocatable work arrays
c
      real,     allocatable :: wrk_age (:)
      real,     allocatable :: wrk_lat (:)
      real,     allocatable :: wrk_lon (:)
      real,     allocatable :: wrk_lvl (:)
      character,allocatable :: wrk_sgn (:) * 7
      integer,  allocatable :: wrk_typ (:)
      real,     allocatable :: wrk_val (:)
      integer,  allocatable :: wrk_var (:)
      real,     allocatable :: wrk_vfy (:,:)
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
      file_typ = 'obsfcst'
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
c     ..open forecast vector file, read number obs
c
      open (UNIT, file=file_name(1:len), status='unknown',
     *            access='sequential', form='unformatted')
      read (UNIT) n_fcst, n_obs, n_var, upd, time
      read (UNIT) fcst_dtg(1:n_fcst)
      read (UNIT) tau_hr(1:n_fcst)

      if (n_obs .eq. 0) then
         write (*, '(''number obs zero'')')
         return
      else
          write (*, '(10x, ''number obs: '', i10)') n_obs
          write (*, '(9x,  ''number fcst: '', i10)') n_fcst
          write (*, '(10x, ''number var: '', i10)') n_var
          write (*, '(8x,  ''update cycle: '', i10)') upd
      endif        
c
c     ..allocate vector arrays
c
      allocate (age (n_obs))
      allocate (lat (n_obs))
      allocate (lon (n_obs))
      allocate (lvl (n_obs))
      allocate (ndx (n_obs))
      allocate (sgn (n_obs))
      allocate (typ (n_obs))
      allocate (val (n_obs))
      allocate (var (n_obs))
      allocate (vfy (n_obs, n_fcst))
c
c     ..read vectors
c
      read (UNIT) age(1:n_obs)
      read (UNIT) lat(1:n_obs)
      read (UNIT) lon(1:n_obs)
      read (UNIT) lvl(1:n_obs)
      read (UNIT) ndx(1:n_obs)
      read (UNIT) var(1:n_obs)
      read (UNIT) typ(1:n_obs)
      read (UNIT) sgn(1:n_obs)
      read (UNIT) val(1:n_obs)
      do j = 1, n_fcst
         read (UNIT) vfy(1:n_obs,j)
      enddo
      close (UNIT)
c
c     ..count number surface obs, set number prf obs
c
      mx_ndx = ndx(n_obs)
      n_sfc = 0
      do i = 2, n_obs
         if (ndx(i) .eq. ndx(i-1)) then
            n_sfc = i - 1
            exit
         endif
      enddo
      n_prf = mx_ndx - n_sfc + 1
c
c     ..allocate work arrays
c
      allocate (wrk_age (n_lvl))
      allocate (wrk_lat (n_lvl))
      allocate (wrk_lon (n_lvl))
      allocate (wrk_lvl (n_lvl))
      allocate (wrk_sgn (n_lvl))
      allocate (wrk_typ (n_lvl))
      allocate (wrk_val (n_lvl))
      allocate (wrk_var (n_lvl))
      allocate (wrk_vfy (n_lvl, n_fcst))
c
c     ..allocate profile arrays
c
      allocate (prf_age (n_obs))
      allocate (prf_lat (n_obs))
      allocate (prf_lon (n_obs))
      allocate (prf_lvl (n_lvl, n_obs))
      allocate (prf_nd  (n_obs))
      allocate (prf_sal (n_lvl, n_obs))
      allocate (prf_sal_vfy (n_lvl, n_obs, n_fcst))
      allocate (prf_sgn (n_obs))
      allocate (prf_tmp (n_lvl, n_obs))
      allocate (prf_tmp_vfy (n_lvl, n_obs, n_fcst))
      allocate (prf_typ (n_lvl, n_obs))
      allocate (prf_var (n_obs))
c
c     ..initialize
c
      prf_nd = 0
      prf_typ = 0
      prf_sal = spval
      prf_sal_vfy = spval
      prf_tmp = spval
      prf_tmp_vfy = spval
c
c     ..roll up profiles
c
      nd = 0
      np = 0
      do i = n_sfc, n_obs
         if (ndx(i) .eq. ndx(i-1)) then
            nd = nd + 1
            wrk_age(nd) = age(i)
            wrk_lat(nd) = lat(i)
            wrk_lon(nd) = lon(i)
            wrk_lvl(nd) = lvl(i)
            wrk_sgn(nd) = sgn(i)
            wrk_typ(nd) = typ(i)
            wrk_val(nd) = val(i)
            wrk_var(nd) = var(i)
            do j = 1, n_fcst
               wrk_vfy(nd,j) = vfy(i,j)
            enddo
         else
            np = np + 1
            prf_age(np) = wrk_age(1) + time
            prf_lat(np) = wrk_lat(1)
            prf_lon(np) = wrk_lon(1)
            prf_nd(np)  = nd
            prf_sgn(np) = wrk_sgn(1)
            prf_var(np) = wrk_var(1)
            do k = 1, nd
               prf_lvl(k,np) = wrk_lvl(k)
               prf_typ(k,np) = wrk_typ(k)
               if (prf_var(np) .eq. 1) then
                  prf_tmp(k,np) = wrk_val(k)
                  do j = 1, n_fcst
                     prf_tmp_vfy(k,np,j) = wrk_vfy(k,j)
                  enddo
               else if (prf_var(np) .eq. 2) then
                  prf_sal(k,np) = wrk_val(k)
                  do j = 1, n_fcst
                     prf_sal_vfy(k,np,j) = wrk_vfy(k,j)
                  enddo
               endif
            enddo
c
            nd = 1
            wrk_age(nd) = age(i)
            wrk_lat(nd) = lat(i)
            wrk_lon(nd) = lon(i)
            wrk_lvl(nd) = lvl(i)
            wrk_sgn(nd) = sgn(i)
            wrk_typ(nd) = typ(i)
            wrk_val(nd) = val(i)
            wrk_var(nd) = var(i)
            do j = 1, n_fcst
               wrk_vfy(nd,j) = vfy(i,j)
            enddo
         endif
      enddo
      write (*,'(5x, ''number profiles: '',i10)') np
c
c     ..clean up
c
      deallocate (age, lat, lon, lvl, ndx, sgn, typ, val, var, vfy)
      deallocate (wrk_age, wrk_lat, wrk_lon, wrk_lvl, wrk_sgn)
      deallocate (wrk_typ, wrk_val, wrk_var, wrk_vfy)
c
c     ..loop over observing systems
c
      call clsgks
c     do n = 1, N_SYS
      do n = 1, 3
c
c     ..temperature
c
      write (*, '(/, 4x, a, '' tmp fcst profile verification'')')
     *       trim (sys_lbl(n))
      write (tmp_name, '(a, ''_tmp_fcst.'', a, ''.gmeta '')')
     *       trim (sys_lbl(n)), dtg
      write (*, '(4x, ''output file name: '', a)') trim (tmp_name)
      fno = 0
      call init_gks ('opn', tmp_name)
      call prof_fcst ('TMP', n_obs, n_fcst, n_lvl, np, prf_age,
     *                prf_lat, prf_lon, prf_lvl, prf_nd, prf_sgn,
     *                prf_typ, prf_tmp, prf_var, prf_tmp_vfy, zoom,
     *                sys_tmp(n), fno)
      call init_gks ('cls', tmp_name)
c
c     ..salinity
c
      write (*, '(/, 4x, a, '' sal fcst profile verification'')')
     *       trim (sys_lbl(n))
      write (tmp_name, '(a, ''_sal_fcst.'', a, ''.gmeta '')')
     *       trim (sys_lbl(n)), dtg
      write (*, '(4x, ''output file name: '', a)') trim (tmp_name)
      fno = 0
      call init_gks ('opn', tmp_name)
      call prof_fcst ('SAL', n_obs, n_fcst, n_lvl, np, prf_age,
     *                prf_lat, prf_lon, prf_lvl, prf_nd, prf_sgn,
     *                prf_typ, prf_sal, prf_var, prf_sal_vfy, zoom,
     *                sys_sal(n), fno)
      call init_gks ('cls', tmp_name)
      enddo
      call opngks
c
c     ..clean up
c
      deallocate (prf_age, prf_lat, prf_lon, prf_lvl, prf_nd, prf_sal)
      deallocate (prf_sal_vfy, prf_sgn, prf_tmp, prf_tmp_vfy, prf_typ)
      deallocate (prf_var)
c
      return
      end
