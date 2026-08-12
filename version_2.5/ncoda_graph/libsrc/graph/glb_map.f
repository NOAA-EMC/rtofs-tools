      subroutine glb_map (out_dir, clm_dir, dtg, n_lon, n_lat, gln,
     *                    glt, node_eq, pln, plt, node_nh, node_sh)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  glb_map
c
c DESCRIPTION:  sets up conversion from HYCOM and MOM irregular 
c               tri-polar grids to mercator and polar stereographic
c               map backgrounds for plotting.  the polar projections
c               are used for plotting variables over the poles.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name         Type       Usage            Description
c   --------    ---------   -------   ---------------------------------
c    clm_dir     character  input     climate file directory path
c    dtg         character  input     analysis date time group
c    gln, glt    integer    input     mercator grid dimensions
c    n_lat       integer    input     model number latitudes
c    n_lon       integer    input     model number longitudes
c    node_eq     real       output    index for mercator projection
c    node_nh     real       output    index for nhem polar projection
c    node_sh     real       output    index for shem polar projection
c    pln, plt    integer    input     polar grid dimensions
c    out_dir     char       input     directory path
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
c     ..define global HYCOM grid
c
      integer    m_hyc, n_hyc
      parameter (m_hyc = 4500)
      parameter (n_hyc = 3298)
c
c     ..define global MOM grid
c
      integer    m_mom, n_mom
      parameter (m_mom = 4500)
      parameter (n_mom = 3297)
c
c     ..define mercator plot grid
c
      integer    gln, glt
c
c     ..define polar stereographic plot grids
c
      integer    pln, plt
c
      integer    UNIT
      parameter (UNIT = 21)
c
      character clm_dir * (*)
      real      delx, dely
      character dtg * 10
      character err_msg * 256
      logical   exist
      character file_name * 256
      integer   io_err
      integer   iref, jref
      integer   len
      integer   m, n
      character model * 5
      integer   n_in
      integer   n_lat, n_lon
      real      node_eq ((gln * glt), 2)
      real      node_nh ((pln * plt), 2)
      real      node_sh ((pln * plt), 2)
      integer   n_proj
      character out_dir * (*)
      real      rlat, rlon
      character sfx * 16
      logical   skip_eq, skip_nh, skip_sh
      real      stdlt1, stdlt2
      real      slon
c
c     ..allocatable arrays
c
      real,     allocatable :: dmy (:)
      real,     allocatable :: grd_lat (:)
      real,     allocatable :: grd_lon (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
c
c...............................executable..............................
c
c     ..set model
c
      if (n_lat .eq. n_hyc) then
         model = 'HYCOM'
         m = m_hyc
         n = n_hyc
      else if (n_lat .eq. n_mom) then
         model = 'MOM  '
         m = m_mom
         n = n_mom
      else
         write (*, '(''*** unknown global model: '', 2i6)')
     *          n_lon, n_lat
         return
      endif
c
c     ..check for node files
c
      skip_eq = .true.
      skip_nh = .true.
      skip_sh = .true.
c
      if (model .eq. 'HYCOM') then
         sfx = '/HYCOM.node_eq'
      else if (model(1:3) .eq. 'MOM') then
         sfx = '/MOM.node_eq'
      endif
      file_name = trim (clm_dir) // trim (sfx)
      len = len_trim (file_name)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='unknown',
     *               access='sequential', form='unformatted')
         read (UNIT) node_eq(:,1)
         read (UNIT) node_eq(:,2)
         close (UNIT)
         write (*,'(7x, ''restart found: '', a)') trim (sfx)
      else
         skip_eq = .false.
      endif
c
      if (model .eq. 'HYCOM') then
         sfx = '/HYCOM.node_nh'
      else if (model(1:3) .eq. 'MOM') then
         sfx = '/MOM.node_nh'
      endif
      file_name = trim (clm_dir) // trim (sfx)
      len = len_trim (file_name)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='unknown',
     *               access='sequential', form='unformatted')
         read (UNIT) node_nh(:,1)
         read (UNIT) node_nh(:,2)
         close (UNIT)
         write (*,'(7x, ''restart found: '', a)') trim (sfx)
      else
         skip_nh = .false.
      endif
c
      if (model .eq. 'HYCOM') then
         sfx = '/HYCOM.node_sh'
      else if (model(1:3) .eq. 'MOM') then
         sfx = '/MOM.node_sh'
      endif
      file_name = trim (clm_dir) // trim (sfx)
      len = len_trim (file_name)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='unknown',
     *               access='sequential', form='unformatted')
         read (UNIT) node_sh(:,1)
         read (UNIT) node_sh(:,2)
         close (UNIT)
         write (*,'(7x, ''restart found: '', a)') trim (sfx)
      else
         skip_sh = .false.
      endif
c
c     return, coefficients found
c
      if (skip_eq .and. skip_nh .and. skip_sh) return
c
c---------------------------------------------------------------------
c
c     ..allocate arrays
c
      allocate (grd_lat (m * n))
      allocate (grd_lon (m * n))
c
c     ..read grid positions
c
      call cr_fname (out_dir, dtg, 1, m, n, 'datafld', 'grdlat', 'o',
     *               'sfc', 0, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then 
         open (UNIT, file=file_name(1:len), status='unknown',
     *               access='stream', form='unformatted')
         read (UNIT) grd_lat
         close (UNIT)
      else
         write (err_msg, '(a, '' grid lat file missing: '', a)')
     *          trim (model), file_name(1:len)
         call error_exit ('GLB_MAP', err_msg)
      endif
c
      call cr_fname (out_dir, dtg, 1, m, n, 'datafld', 'grdlon', 'o',
     *               'sfc', 0, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then 
         open (UNIT, file=file_name(1:len), status='unknown',
     *               access='stream', form='unformatted')
         read (UNIT) grd_lon
         close (UNIT)
      else
         write (err_msg, '(a, '' grid lon file missing: '', a)')
     *          trim (model), file_name(1:len)
         call error_exit ('GLB_MAP', err_msg)
      endif
c
c-------------------------------------------------------------------------
c
c     ..set mercator plot projection parameters
c
      n_proj = 1    
      delx = 9000.
      dely = 9000.
      iref = gln / 2 + 1
      jref = glt / 2 + 1
      rlat = 0.
      rlon = 180.
      slon = 180.
      stdlt1 = 0.
      stdlt2 = 0.
c
c     ..allocate arrays
c
      allocate (dmy (gln * glt))
      allocate (lat (gln * glt))
      allocate (lon (gln * glt))
c
c     ..set number points
c
      n_in = gln * glt
c
c     ..compute mercator grid coordinates
c
      call coamps_grid (n_proj, rlat, rlon, iref, jref, stdlt1,
     *                  stdlt2, slon, delx, dely, gln, glt,
     *                  lat, lon, dmy, dmy, dmy, dmy, dmy)
c
c     ..compute HYCOM grid indices
c
      call irreg_ll2ij (m, n, grd_lat, grd_lon, n_in, lat, lon,
     *                  node_eq(1,1), node_eq(1,2))
c
c     ..save interpolation coefficients
c
      if (model .eq. 'HYCOM') then
         sfx = '/HYCOM.node_eq'
      else if (model(1:3) .eq. 'MOM') then
         sfx = '/MOM.node_eq'
      endif
      file_name = trim (clm_dir) // trim (sfx)
      len = len_trim (file_name)
      open (UNIT, file=file_name(1:len), status='unknown',
     *            access='sequential', form='unformatted')
      write (UNIT, iostat=io_err) node_eq(:,1)
      if (io_err .gt. 0) then
         write (*, '(i6, '' io error writing: '', a)')
     *          io_err, trim (file_name)
      endif
      write (UNIT, iostat=io_err) node_eq(:,2)
      if (io_err .gt. 0) then
         write (*, '(i6, '' io error writing: '', a)')
     *          io_err, trim (file_name)
      endif
      close (UNIT)      
c
c     ..clean up
c
      deallocate (dmy, lat, lon)
c
c-------------------------------------------------------------------------
c
c     ..set nhem polar stereographic plot projection parameters
c
      n_proj = 3    
      delx = 9000.
      dely = 9000.
      iref = pln / 2
      jref = plt / 2
      rlat = 90.
      rlon = 0.
      slon = 300.
      stdlt1 = 60.
      stdlt2 = 60.
c
c     ..allocate arrays
c
      allocate (dmy (pln * plt))
      allocate (lat (pln * plt))
      allocate (lon (pln * plt))
c
c     ..set number points
c
      n_in = pln * plt
c
c     ..compute polar grid coordinates
c
      call coamps_grid (n_proj, rlat, rlon, iref, jref, stdlt1,
     *                  stdlt2, slon, delx, dely, pln, plt,
     *                  lat, lon, dmy, dmy, dmy, dmy, dmy)
c
c     ..compute HYCOM grid indices
c
      call irreg_ll2ij (m, n, grd_lat, grd_lon, n_in, lat, lon,
     *                  node_nh(1,1), node_nh(1,2))
c
c     ..save interpolation coefficients
c
      if (model .eq. 'HYCOM') then
         sfx = '/HYCOM.node_nh'
      else if (model(1:3) .eq. 'MOM') then
         sfx = '/MOM.node_nh'
      endif
      file_name = trim (clm_dir) // trim (sfx)
      len = len_trim (file_name)
      open (UNIT, file=file_name(1:len), status='unknown',
     *            access='sequential', form='unformatted')
      write (UNIT, iostat=io_err) node_nh(:,1)
      if (io_err .gt. 0) then
         write (*, '(i6, '' io error writing: '', a)')
     *          io_err, trim (file_name)
      endif
      write (UNIT, iostat=io_err) node_nh(:,2)
      if (io_err .gt. 0) then
         write (*, '(i6, '' io error writing: '', a)')
     *          io_err, trim (file_name)
      endif
      close (UNIT)
c
c     ..clean up
c
      deallocate (dmy, lat, lon)
c
c-------------------------------------------------------------------------
c
c     ..set shem polar stereographic plot projection parameters
c
      rlat = -90.
      stdlt1 = -60.
      stdlt2 = -60.
c
c     ..allocate arrays
c
      allocate (dmy (pln * plt))
      allocate (lat (pln * plt))
      allocate (lon (pln * plt))
c
c     ..set number points
c
      n_in = pln * plt
c
c     ..compute polar grid coordinates
c
      call coamps_grid (n_proj, rlat, rlon, iref, jref, stdlt1,
     *                  stdlt2, slon, delx, dely, pln, plt,
     *                  lat, lon, dmy, dmy, dmy, dmy, dmy)
c
c     ..compute HYCOM grid indices
c
      call irreg_ll2ij (m, n, grd_lat, grd_lon, n_in, lat, lon,
     *                  node_sh(1,1), node_sh(1,2))
c
c     ..save interpolation coefficients
c
      if (model .eq. 'HYCOM') then
         sfx = '/HYCOM.node_sh'
      else if (model(1:3) .eq. 'MOM') then
         sfx = '/MOM.node_sh'
      endif
      file_name = trim (clm_dir) // trim (sfx)
      len = len_trim (file_name)
      open (UNIT, file=file_name(1:len), status='unknown',
     *            access='sequential', form='unformatted')
      write (UNIT, iostat=io_err) node_sh(:,1)
      if (io_err .gt. 0) then
         write (*, '(i6, '' io error writing: '', a)')
     *          io_err, trim (file_name)
      endif
      write (UNIT, iostat=io_err) node_sh(:,2)
      if (io_err .gt. 0) then
         write (*, '(i6, '' io error writing: '', a)')
     *          io_err, trim (file_name)
      endif
      close (UNIT)
c
c     ..clean up
c
      deallocate (dmy, grd_lat, grd_lon, lat, lon)
c
      return
      end
