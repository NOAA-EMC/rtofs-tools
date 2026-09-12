      program ncoda_diagn
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  ncoda_diagn
c
c DESCRIPTION:  perform diagnostics on time series of NCODA 
c               analysis, adjoint and match up outputs 
c
c RESTRICTIONS:    none
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c....................MAINTENANCE SECTION................................
c
c MODULES CALLED:
c        Name                    Description
c   --------------     -----------------------------------------
c   GETARG             retrieve command line argument
c
c LOCAL VARIABLES AND STRUCTURES:
c     Name            Type                 Description
c   ---------       --------    ----------------------------------
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
c     ..define work unit number
c
      integer    UNIT
      parameter (UNIT = 20)
c
      character adj_dir * 256
      character arg * 10
      real      bl (2), br (2)
      character clim_dir * 256
      character data_dir * 256
      real      datao (2000)
      real      delx, dely
      character dtg1 * 10
      character dtg2 * 10
      character err_msg * 256
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   gln, glt
      logical   global
      logical   got_adj, got_clm, got_data, got_hdr, got_out
      integer   i1, i2, j1, j2
      integer   iref, jref
      integer   ix, jy
      integer   k, m, n
      integer   len
      character lvl_typ * 3
      integer   n_arg
      integer   n_days
      integer   n_lat
      integer   n_lon
      integer   n_lvl
      integer   n_mdl_lat
      integer   n_mdl_lon
      integer   n_proj 
      integer   ndx
      character out_dir * 256
      integer   pln, plt
      real      rlat, rlon
      logical   subset
      integer   status
      real      stdlt1, stdlt2
      real      stdlon
      integer   tau
      real      tl (2), tr (2)  
      integer   upd
      real      z_lvl (100)
c
c     ..functions
c
      integer   IARGC
c
c     ..allocatable arrays
c
      real,     allocatable :: depth (:)
      integer,  allocatable :: msk (:)
c
c-----------------------------------------------------------------------
c
c NCODA ocean diagnostic namelist (odiagnl)
c
c PARAMETERS:
c       Name          Type                 Description
c   -------------   --------    ---------------------------------------
c   adj_tau         integer     adjoint forecast period to process
c   area            character   geographic area name
c   do_adj          logical     (true) perform adjoint diagnostics
c   do_adj_sens     logical     (true) calculate time averaged adjoint
c                               sensitivity error fields
c   do_adj_sfc      logical     (true) perform adjoint diagnostics on
c                               raw surface-only observations
c   do_bias         logical     (true) perform bias estimate from
c                               analyzed increment fields
c   do_point        logical     analyze time series at grid point
c   do_ssh_vrfy     logical     (true) verify ssh forecasts
c   do_sss_mdb      logical     analyze argo sss match up data base
c   do_vrfy         logical     (true) time series of OmF and OmA
c   nest            integer     grid nest number
c   title           character   run title for plots
c   zoom            real        lat,lon box for zoomed subset
c   z_plot          integer     indicies of vertical levels to plot
c
c     ..declarations
c
      integer   adj_tau
      character area * 64
      logical   do_adj
      logical   do_adj_sens
      logical   do_adj_sfc
      logical   do_bias
      logical   do_point
      logical   do_ssh_vrfy
      logical   do_sss_mdb
      logical   do_vrfy
      integer   nest
      character title * 80
      real      zoom (4)
      integer   z_plot (20)
c   
c     ..default values
c
      data      adj_tau     / 48 /
      data      area        / '                    ' /
      data      do_adj      / .false. /
      data      do_adj_sens / .false. /
      data      do_adj_sfc  / .false. /
      data      do_bias     / .false. /
      data      do_point    / .false. /
      data      do_ssh_vrfy / .false. /
      data      do_sss_mdb  / .false. /
      data      do_vrfy     / .false. /
      data      nest        / 1 /
      data      title       / 'RTOFS v3.2 ' /
      data      zoom        / -999., -999., -999., -999. /
      data      z_plot      / 20 * 0  /
c
c     ..namelist statement
c
      namelist /odiagnl/ adj_tau, do_adj, do_adj_sens, do_adj_sfc,
     *                   do_bias, do_point, do_ssh_vrfy, do_sss_mdb,
     *                   do_vrfy, nest, area, title, zoom, z_plot
c
c...............................executable..............................
c
c     ..count number command line arguments
c
      n_arg = IARGC ()
c
c     ..diagnostics
c
      if (n_arg .eq. 0) then
         write (*, '(/, ''NCODA_DIAGN NCEP v3.20 command line '',
     *                  ''arguments:'')')
         write (*, '(5x, ''ncoda_diagn {dtg1} {n_days}'')')
         write (*, '(5x, ''{dtg1} = starting date time group '',
     *                   ''(yyyymmddhh)'')')
         write (*, '(5x, ''{n_days} = number days to go forward from '',
     *                   ''starting dtg'')')
         write (*, '(/, ''NCODA_DIAGN namelist (odiagnl) variables:'')')
         write (*, '(5x, ''&odiagnl'')')
         write (*, '(7x, ''adj_tau     = adjoint forecast period to '',
     *                   ''process'')')
         write (*, '(7x, ''area        = geographic area name'')')
         write (*, '(7x, ''do_adj      = (true) perform adjoint '',
     *                   ''diagnostics'')')
         write (*, '(7x, ''do_adj_sens = (true) calculate time '',
     *                   ''averaged adjoint sensitivity fields'')')
         write (*, '(7x, ''do_adj_sfc  = (true) perform adjoint '',
     *                    ''diagnostics on surface observations'')')
         write (*, '(7x, ''do_bias     = (true) form bias estimates '',
     *                   ''from analyzed increment fields'')')
         write (*, '(7x, ''do_point    = (true) analyze grid point '',
     *                   ''time series'')')
         write (*, '(7x, ''do_ssh_vrfy = (true) verify ssh '',
     *                   ''forecasts'')')
         write (*, '(7x, ''do_sss_mdb  = (true) argo satellite / ''
     *                   ''sss match up data base'')')
         write (*, '(7x, ''do_vrfy     = (true) time series of error '',
     *                   ''statistics in observation space'')')
         write (*, '(7x, ''nest        = grid nest number'')')
         write (*, '(7x, ''title       = run title for plots'')')
         write (*, '(7x, ''zoom        = lat,lon box for zoomed '',
     *                   ''subset'')')
         write (*, '(7x, ''z_plot      = indicies of vertical error '',
     *                   ''levels to plot'')')
         write (*, '(5x, ''&end'')')
         write (*, '(/, ''NCODA_DIAGN environmental variables:'')')
         write (*, '(5x, ''OCN_OUTPUT_DIR = directory path to '',
     *                   ''restart files'')')
         write (*, '(5x, ''OCN_CLIM_DIR   = directory path to '',
     *                   ''ocean climate files'')')
         write (*, '(5x, ''OCN_DATA_DIR   = directory path to '',
     *                   ''ocean obs QC files'')')
         write (*, '(5x, ''OCN_ADJ_DIR    = directory path to '',
     *                   ''adjoint data files'')')
         stop
      endif
c
c     ..retrieve starting DTG argument
c
      if (n_arg .gt. 0) then
         call GETARG (1, arg)
         dtg1 = arg(1:10) 
      else
         write (err_msg, '(''no DTG argument specified'')')
         call error_exit ('NCODA_DIAGN', err_msg)
      endif
c
c     ..get number days to process, convert to dtg
c
      if (n_arg .gt. 1) then
         call GETARG (2, arg)
         read (arg, '(i3)') n_days
         if (n_days .gt. 1) then
            n_days = (n_days - 1) * 24
            call dtgmod (dtg1, n_days, dtg2, status)
         else
            dtg2 = dtg1
         endif
      else
         dtg2 = dtg1
      endif
c
c     ..retrieve restart directory path
c
      call GETENV ('OCN_OUTPUT_DIR', out_dir)
      len = len_trim (out_dir)
      if (len .eq. 0) then
         write (*, '(''**** WARNING: missing OCN_OUTPUT_DIR '',
     *               ''environmental variable'')')
         got_out = .false.
      else
         got_out = .true.
      endif
c
c     ..retrieve ocean climate file directory path
c
      call GETENV ('OCN_CLIM_DIR', clim_dir)
      len = len_trim (clim_dir)
      if (len .eq. 0) then
         write (*, '(''**** WARNING: missing OCN_CLIM_DIR '',
     *               ''environmental variable'')')
         got_clm = .false.
      else
         got_clm = .true.
      endif
c
c     ..retrieve ocean data directory path
c
      call GETENV ('OCN_DATA_DIR', data_dir)
      len = len_trim (data_dir)
      if (len .eq. 0) then
         write (*, '(''**** WARNING: missing OCN_DATA_DIR '',
     *               ''environmental variable'')')
         got_data = .false.
      else
         got_data = .true.
      endif
c
c     ..retrieve adjoint directory path
c
      call GETENV ('OCN_ADJ_DIR', adj_dir)
      len = len_trim (adj_dir)
      if (len .eq. 0) then
         write (*, '(''**** WARNING: missing OCN_ADJ_DIR '',
     *               ''environmental variable'')')
         got_adj = .false.
      else
         got_adj = .true.
      endif
c
c     ..check for ncoda diagnostics namelist
c
      inquire (file='odiagnl', exist=exist)
      if (exist) then
         open (9, file='odiagnl', status='old', form='formatted')
         read (9, odiagnl)
         close (9)
      else
         write (*, '(''**** WARNING: odiagnl namelist '',
     *               ''file missing'')')
      endif
c
c     ..process grid point coordinates
c
      if (do_point) then
         if (n_arg .gt. 2) then
            call GETARG (3, arg)
            read (arg, '(i10)') ix
         else
            write (err_msg, '(''grid point "ix" missing'')')
            call error_exit ('NCODA_DIAGN', err_msg)
         endif
         if (n_arg .gt. 3) then
            call GETARG (4, arg)
            read (arg, '(i10)') jy
         else
            write (err_msg, '(''grid point "jy" missing'')')
            call error_exit ('NCODA_DIAGN', err_msg)
         endif
      else
         ix = -999
         jy = -999
      endif
c
c     ..read datao record
c
      m = 2000
      n = 1
      tau = 0
      file_typ = 'infofld'
      fld_name = 'datahd'
      fluid = 'o'
      lvl_typ = 'hdr'
      call cr_fname (out_dir, dtg1, n, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='old',
     *               access='stream', form='unformatted')
         read (UNIT) datao
         close (UNIT)
         got_hdr = .true.
      else
         write (*, '(''**** WARNING: datahd file missing "'',
     *               a, ''"'')') trim (file_name)
         datao = 0.
         got_hdr = .false.
      endif
c
c     ..set global interpolation array dimensions
c
      gln = 4449
      glt = 2457
      pln = 900
      plt = 900
c
c     ..set zoom/plot parameters
c
      if (got_hdr) then
         call set_zoom (datao, nest, gln, glt, zoom, n_lon, n_lat,
     *                  n_lvl, global, n_proj, delx, dely, rlat,
     *                  stdlt1, stdlt2, stdlon, upd, z_lvl, bl,
     *                  br, tl, tr, i1, i2, j1, j2, subset)
         n_mdl_lon = n_lon
         n_mdl_lat = n_lat
      endif
c
c     ..diagnostics
c
      write (*, '(/, ''======================================='',
     *            /, ''==== RTOFS Observation Diagnostics ===='',
     *            /, ''========== NCEP Version 3.20 =========='',
     *            /, ''======================================='')')
      if (got_out) then
         write (*, '(''  analysis directory: '', a)') trim (out_dir)
      endif
      if (got_data) then
         write (*, '(''      data directory: '', a)') trim (data_dir)
      endif
      if (got_clm) then
         write (*, '(''   climate directory: '', a)') trim (clim_dir)
      endif
      if (got_adj) then
         write (*, '(''   adjoint directory: '', a)') trim (adj_dir)
      endif
      write (*, '(''        starting dtg: '', a)') dtg1
      write (*, '(''          ending dtg: '', a)') dtg2
      write (*, '(''           area name: '', a)') trim (area)
      if (got_hdr) then
         write (*, '(''        update cycle: '', i10)') upd
         write (*, '(''    adjoint forecast: '', i10)') adj_tau
         write (*, '(''     grid dimensions: '', 3i10)') n_lon, n_lat,
     *                                                   n_lvl
         write (*, '(''           grid mesh: '', 2f10.1)') delx, dely
         write (*, '(''     grid projection: '', i10)') n_proj
         write (*, '(''             ref lat: '', f10.2)') rlat
         write (*, '(''            stnd lon: '', f10.2)') stdlon
         write (*, '(''     stnd lat1, lat2: '', 2f10.2)') stdlt1,stdlt2
         if (n_proj .lt. 0) then
            write (*, '(''   hycom plot arrays: '', 4i10)') 
     *             gln, glt, pln, plt
         endif
         write (*, '(''          grid point: '', 2i10)') ix, jy
         write (*, '(''         zoom subset: '', l10)') subset
         write (*, '(''     i,j coordinates: '', 4i10)') i1, i2, j1, j2
         write (*, '(''         bottom left: '', 2f10.1)') bl(1), bl(2)
         write (*, '(''        bottom right: '', 2f10.1)') br(1), br(2)
         write (*, '(''            top left: '', 2f10.1)') tl(1), tl(2)
         write (*, '(''           top right: '', 2f10.1)') tr(1), tr(2)
         if (n_lvl .gt. 1) then
            write (*, '(''       vertical grid: '')')
            write (*, '(4(i10, '')'', f10.2))') 
     *            (k, z_lvl(k), k = 1, n_lvl)
         endif
      else
         n_lon = 1
         n_lat = 1
         upd = 24
      endif
c
c     ..allocate and retrieve depth and mask arrays
c
      allocate (depth (n_lon * n_lat))
      allocate (msk (n_lon * n_lat))
      if (area(1:6) .eq. 'Global') then
         depth = 0.
         msk = 100
c*********************************
      write (*,'(''here: '',2i10)') n_lon,n_lat
c*****************************
      else
         call rd_dpth_msk (out_dir, dtg1, n_lon, n_lat, nest,
     *                     depth, msk)
      endif
c
c-----------------------------------------------------------------------
c
      fno = 0
c
c     ..perform adjoint diagnostics options
c
      if (got_adj) then
         if (area(1:6) .ne. 'Global') then
c
c        ..read adjoint datao header record
c
         m = 2000
         n = 1
         call cr_fname (adj_dir, dtg1, n, m, n, 'infofld', 'datahd',
     *                  'o', 'hdr', 0, file_name, len)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            open (UNIT, file=file_name(1:len), status='old',
     *                  access='stream', form='unformatted')
            read (UNIT) datao
            close (UNIT)
         else
            write (err_msg, '(a, '' datahd file missing'')') dtg1
            write (*, '(''fn: '', a)') trim (file_name)
            call error_exit ('NCODA_DIAGN', err_msg)
         endif
c
c        ..decode adjoint grid definition
c
         n_lon = nint (datao(30+0))
         n_lat = nint (datao(30+1))
         n_proj = nint (datao(3))
         iref = nint (datao(30+2))
         jref = nint (datao(30+3))
         rlat = datao(7)
         rlon = datao(8)
         delx = datao(9)
         dely = datao(10)
         stdlon = datao(6)
         stdlt1 = datao(4)
         stdlt2 = datao(5)
         bl(1) = datao(30+6)
         bl(2) = datao(30+7)
         br(1) = datao(30+8)
         br(2) = datao(30+9)
         tr(1) = datao(30+10)
         tr(2) = datao(30+11)
         tl(1) = datao(30+12)
         tl(2) = datao(30+13)
c
         write (*, '(/, ''Adjoint Grid Header'')')
         write (*, '(''          projection: '', i10)') n_proj
         write (*, '(''          dimensions: '', 2i10)') n_lon, n_lat
         write (*, '(''         bottom left: '', 2f10.1)') bl(1), bl(2)
         write (*, '(''        bottom right: '', 2f10.1)') br(1), br(2)
         write (*, '(''            top left: '', 2f10.1)') tl(1), tl(2)
         write (*, '(''           top right: '', 2f10.1)') tr(1), tr(2)
         endif
c
c******************
      write (*,'(''area: '',a)') trim(area)
      write (*,'(''do_adj: '',l5)') do_adj
c*****************
         if (do_adj) then
c
c           ..adjoint diagnostics
c
            if (area(1:6) .eq. 'Global') then
      write (*,'(''calling glb'')')
               call adj_diagn_glb (title, adj_dir, dtg1, dtg2, nest,
     *                             upd, adj_tau, fno)
            else if (n_proj .eq. 3) then
               call adj_diagn_ice (title, adj_dir, dtg1, dtg2, nest,
     *                             n_lon, n_lat, upd, adj_tau, n_proj,
     *                             rlat, stdlt1, stdlt2, stdlon, bl,
     *                             br, tl, tr, area, fno)
            else
               call adj_diagn_prf (title, adj_dir, dtg1, dtg2, nest, 
     *                             n_lon, n_lat, upd, adj_tau, n_proj,
     *                             rlat, stdlt1, stdlt2, stdlon, bl,
     *                             br, tl, tr, area, fno)
            endif
         endif
c
         if (do_adj_sfc .and. got_out) then
c
c           ..adjoint sfc data
c     
            call adj_diagn_sfc (title, out_dir, adj_dir, dtg1, dtg2,
     *                          n_mdl_lon, n_mdl_lat, n_lon, n_lat,
     *                          nest, n_proj, delx, dely, iref, jref,
     *                          rlat, rlon, stdlt1, stdlt2, stdlon,
     *                          bl, br, tl, tr, adj_tau, upd, area,
     *                          fno)
         endif
c
         if (do_adj_sens .and. got_out) then
c
c           ..adjoint sensitivity
c
            if (n_proj .eq. 3) then
               call adj_sens_ice (title, adj_dir, dtg1, dtg2, nest,
     *                            adj_tau, n_lon, n_lat, n_proj,
     *                            rlat, stdlt1, stdlt2, stdlon,
     *                            bl, br, tl, tr, upd, fno)
            else
               call adj_sens_prf (title, adj_dir, dtg1, dtg2, nest,
     *                            adj_tau, n_lon, n_lat, n_lvl, 
     *                            n_proj, rlat, stdlt1, stdlt2, 
     *                            stdlon, bl, br, tl, tr, upd, 
     *                            z_lvl, z_plot, fno)
            endif
         endif
      endif
c
c     ..open gks
c
      call opngks
c
c***************************
      write (*,'(''do_bias: '',4l5)') do_bias,got_out,got_clm,got_hdr
c************************
      if (do_bias .and. got_out .and. got_clm .and. got_hdr) then
c
c        ..forecast bias (surface fields only)
c
         call model_bias (title, out_dir, clim_dir, dtg1, dtg2, n_lon,
     *                    n_lat, n_lvl, msk, gln, glt, pln, plt, nest,
     *                    upd, n_proj, rlat, stdlt1, stdlt2, stdlon,
     *                    bl, br, tl, tr, i1, i2, j1, j2, global)
      endif
c
      if (do_sss_mdb .and. got_data .and. got_clm) then
c
c        ..argo sss and satellite sss match up data
c
         call argo_mdb_diagn (dtg1, dtg2, data_dir, clim_dir)
      endif
c
      if (do_vrfy .and. got_out .and. got_hdr) then
c
c        ..observation space error statistics
c
         call vrfy_obs (title, out_dir, dtg1, dtg2, nest, n_lon,
     *                  n_lat, upd, n_proj, rlat, stdlt1, stdlt2,
     *                  stdlon, bl, br, tl, tr, zoom)
         call vrfy_obs_fcst (title, out_dir, dtg1, dtg2, nest, n_lon,
     *                       n_lat, upd, n_proj, rlat, stdlt1, stdlt2,
     *                       stdlon, bl, br, tl, tr, zoom)
      endif
c
      if (do_ssh_vrfy .and. got_data .and. got_clm .and. 
     *    got_out .and. got_hdr) then
c
c        ..observation space error statistics
c
         call vrfy_ssh (title, data_dir, clim_dir, out_dir, dtg1,
     *                  dtg2, nest, n_lon, n_lat, msk, upd, gln, 
     *                  glt, pln, plt, n_proj, rlat, stdlt1, stdlt2,
     *                  stdlon, bl, br, tl, tr, i1, i2, j1, j2)
      endif
c
      if (do_point .and. got_out) then
c
c        ..model point time series
c
         ndx = n_lon * (jy-1) + ix
         call model_point (title, out_dir, dtg1, dtg2, n_lon, n_lat,
     *                     n_lvl, ix, jy, msk(ndx), z_lvl, fno)
      endif
c
c     ..close gks
c
      call clsgks
c
c     ..clean up
c
      deallocate (depth, msk)
c
      stop
      end
