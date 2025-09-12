      subroutine adj_diagn_ice (title, adj_dir, dtg1, dtg2, nest,
     *                          n_lon, n_lat, upd, adj_tau, n_proj,
     *                          rlat, stdlt1, stdlt2, stdlon, bl,
     *                          br, tl, tr, area, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  adj_diagn_ice
c
c DESCRIPTION:  performs diagnostics on sea ice data impact output
c               files. diagnostics are plotted for northern and
c               southern hemispheres
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name            Type       Usage            Description
c   --------      ---------   -------   ------------------------------
c    adj_tau      integer      input    adjoint forecast period
c    bl, br       real         input    grid bottom left, right lat, lons
c    fno          integer      input    sequential frame counter
c    n_lon        integer      input    number grid longitudes
c    n_lat        integer      input    number grid latitudes
c    n_proj       integer      input    grid projection number
c    out_dir      char         input    adjoint output directory
c    rlat         real         input    grid reference latitude
c    stdlon       real         input    grid standard longitude
c    stdlt(s)     real         input    grid standard latitudes
c    tl, tr       real         input    grid top left, right lat, lons
c    upd          integer      input    analysis update cycle
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
c     ..define number observing systems and variables to plot
c
      integer    N_ICE_SYS
      parameter (N_ICE_SYS = 1)
      integer    N_VAR
      parameter (N_VAR = 1)
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
      character area * (*)
      real      bl (2), br (2)
      character date1 * 6
      character date2 * 11
      integer   day
      character dtg * 10
      character dtg1 * 10
      character dtg2 * 10
      character dtg_end * 10
      character dtg_str * 10
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   hrs
      integer   i, k, n
      integer   len
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   nest
      integer   n_files
      integer   n_lat
      integer   n_lon
      integer   n_proj
      character plot_title * 256
      real      rlat
      real      stdlt1, stdlt2
      real      stdlon
      integer   status
      character title * (*)
      real      tl (2), tr (2) 
      character tmp_name * 80 
      integer   upd
      integer   year
c
c     ..dummy variable
c
      real      anm
      real      err
      character sgn * 7
c
c     ..allocatable total data arrays
c
      real,     allocatable :: obs_imp (:,:)
      real,     allocatable :: obs_lat (:,:)
      real,     allocatable :: obs_lon (:,:)
      real,     allocatable :: obs_lvl (:,:)
      real,     allocatable :: obs_sen (:,:)
      integer,  allocatable :: obs_typ (:,:)
      integer,  allocatable :: obs_var (:,:)
      real,     allocatable :: obs_xi (:,:)
      real,     allocatable :: obs_yj (:,:)
c
      integer,  allocatable :: n_obs_file (:)
c
c     ..allocatable data file arrays
c
      real,     allocatable :: imp (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      real,     allocatable :: lvl (:)
      real,     allocatable :: sen (:)
      integer,  allocatable :: typ (:)
      integer,  allocatable :: var (:)
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
c     ..set date time group title
c
      if (dtg_str .eq. dtg_end) then
         plot_title = trim (title) // '     ' // date2
      else
         plot_title = trim (title) // '     ' // date1 //
     *                ' to ' // date2 
      endif
c
c     ..set adjoint hemspheric file parameters
c
      fld_name = 'seaice'
      file_typ = 'obssens'
      fluid = 'o'
      lvl_typ = 'sfc'
c
c     ..set number files to process
c
      call dtgdif (dtg_str, dtg_end, hrs, status)
      n_files = hrs / upd + 1
c
c     ..find max number obs per file
c
      n_obs = 0
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg_str, hrs, dtg, status)
         call cr_fname (adj_dir, dtg, nest, n_lon, n_lat, file_typ,
     *                  fld_name, fluid, lvl_typ, adj_tau, file_name,
     *                  len)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            open (UNIT, file=file_name(1:len), status='unknown',
     *                  form='unformatted')
            read (UNIT) n_data
            if (n_data .gt. n_obs) n_obs = n_data
            close (UNIT)
         else
            write (*, '(''file missing: '', a)') trim (file_name)
         endif
      enddo
      if (n_obs .eq. 0) return
c
c     ..allocate total data arrays
c   
      allocate (obs_imp (n_obs, n_files))
      allocate (obs_lat (n_obs, n_files))
      allocate (obs_lon (n_obs, n_files))
      allocate (obs_lvl (n_obs, n_files))
      allocate (obs_sen (n_obs, n_files))
      allocate (obs_typ (n_obs, n_files))
      allocate (obs_var (n_obs, n_files))
      allocate (obs_xi (n_obs, n_files))
      allocate (obs_yj (n_obs, n_files))
c
      allocate (n_obs_file (n_files))
c
c     ..initialize
c
      do k = 1, n_files
         n_obs_file(k) = 0
      enddo
c
c--------------------------------------------------------------
c
c     ..build data arrays
c
      do k = 1, n_files
         hrs = (k-1) * upd
         call dtgmod (dtg_str, hrs, dtg, status)
c
c        ..build file name
c
         call cr_fname (adj_dir, dtg, nest, n_lon, n_lat, file_typ,
     *                  fld_name, fluid, lvl_typ, adj_tau, 
     *                  file_name, len)
c
c        ..check for file existence
c
         inquire (file=file_name(1:len), exist=exist)
         if (.not. exist) cycle
         open (UNIT, file=file_name(1:len), status='unknown',
     *               form='unformatted')
c
c        ..read number obs
c
         read (UNIT) n_data
         if (n_data .eq. 0) then
            close (UNIT)
            cycle
         endif
c
c        ..allocate file arrays
c     
         allocate (imp (n_data))
         allocate (lat (n_data))
         allocate (lon (n_data))
         allocate (lvl (n_data))
         allocate (sen (n_data))
         allocate (typ (n_data))
         allocate (var (n_data))
         allocate (xi (n_data))
         allocate (yj (n_data))
c
c        ..read data vectors
c
         read (UNIT) err
         read (UNIT) anm
         read (UNIT) err
         read (UNIT) imp(1:n_data)
         read (UNIT) lat(1:n_data)
         read (UNIT) lon(1:n_data)
         read (UNIT) lvl(1:n_data)
         read (UNIT) sen(1:n_data)
         read (UNIT) typ(1:n_data)
         read (UNIT) var(1:n_data)
         read (UNIT) xi(1:n_data)
         read (UNIT) yj(1:n_data)
         read (UNIT) err
         read (UNIT) sgn
         close (UNIT)
c
c        ..update total data arrays
c
         n = 0
         do i = 1, n_data
            n = n + 1
            obs_imp(n,k) = imp(i)
            obs_lat(n,k) = lat(i)
            obs_lon(n,k) = lon(i)
            obs_lvl(n,k) = lvl(i)
            obs_sen(n,k) = sen(i)
            obs_typ(n,k) = typ(i)
            obs_var(n,k) = var(i)
            obs_xi(n,k) = xi(i)
            obs_yj(n,k) = yj(i)
         enddo
         n_obs_file(k) = n
c
c        ..clean up file arrays
c
         deallocate (imp, lat, lon, lvl, sen, typ)
         deallocate (var, xi, yj)
      enddo
c
c-----------------------------------------------------------------------
c
c     ..sea ice data impacts
c
      write (*, '(/, ''Sea Ice Impacts'')')
      write (tmp_name, '(''ice_impacts.'', a, ''.gmeta '')') dtg2
      write (*, '(4x, ''output file name: '', a)') trim (tmp_name)
      fno = 0
      call init_gks ('opn', tmp_name)
c
c     ..sea ice data impacts
c
      call map_impact_ice (n_lon, n_lat, n_obs, n_files, 
     *                     n_obs_file, obs_imp, obs_lat,
     *                     obs_lon, obs_sen, obs_typ, 
     *                     obs_xi, obs_yj, plot_title,
     *                     n_proj, rlat, stdlt1, stdlt2, 
     *                     stdlon, bl, br, tl, tr, fno)
c
      call init_gks ('cls', tmp_name)
c
c     ..clean up
c
      deallocate (n_obs_file, obs_imp, obs_lat, obs_lon, obs_lvl)
      deallocate (obs_sen, obs_typ, obs_var, obs_xi, obs_yj)
c
      return
      end
