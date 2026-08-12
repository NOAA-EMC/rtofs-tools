      program ncoda_map
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  ncoda_map
c
c DESCRIPTION:  maps a NCODA variational analysis -
c               requires NCAR graphics
c
c RESTRICTIONS: none
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c....................MAINTENANCE SECTION................................
c
c MODULES CALLED:
c        Name                    Description
c   --------------     -----------------------------------------
c   clsgks             close gks workstation
c   error_exit         standard error processing 
c   opngks             open gks workstation
c   plot_fld           driver routine for plotting fields
c   GETARG             retrieve command line argument
c   GETENV             retrieve environmental variable
c
c LOCAL VARIABLES AND STRUCTURES:
c     Name            Type                 Description
c   ---------       --------    ----------------------------------
c   dtg             char        date time group (YYYYMMDDHH)
c   err_msg         char        standard error processing message
c   outp_dir        char        restart file directory path
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
c     ..define special value
c
      real       SPVAL
      parameter (SPVAL = -999.)
c
      character arg * 80
      real      bl (2), br (2)
      character clm_dir * 256
      real      datao (2000)
      character date * 15
      character date_dtg * 15
      integer   day
      real      delx, dely
      character dtg * 10
      character err_msg * 256
      logical   exist
      character file_dtg * 10
      character file_name * 256
      character file_type * 7
      character fld_name * 6
      character fluid * 1
      integer   gln, glt
      logical   global
      integer   i1, i2, j1, j2
      integer   j
      integer   igrid
      integer   len
      character lvl_type * 3
      integer   m, n, l
      integer   mon
      character month (12) * 3
      integer   n_arg
      character out_dir * 256
      integer   pln, plt
      real      rlat
      integer   ssh_opt
      integer   status
      real      stdlt1, stdlt2
      real      stdlon
      logical   subset
      integer   tau
      integer   tau_hr
      real      tl (2), tr (2)
      integer   upd
      integer   year
      real      zlvl (100)
      integer   zulu
c
c     ..local save variables
c
      save      datao
c
c     ..functions
c
      integer   IARGC
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
      include 'omapnl.h'
c
c     ..diagnostics
c
      n_arg = IARGC ()
      if (n_arg .eq. 0) then
         write (*, '(/, ''NCODA_MAP NCEP v3.20 command line ''
     *                  ''arguments:'')')
         write (*, '(5x, ''ncoda_map {anl_dtg} [tau]'')')
         write (*, '(5x, ''{anl_dtg} = analysis date time group '',
     *                   ''(yyyymmddhh)'')')
         write (*, '(5x, ''[tau]     = forecast period (hours)'')')
         write (*, '(/, 5x, ''NOTES: 1. {anl_dtg} is required'')')
         write (*, '(5x,    ''       2. [tau] is optional, default '',
     *                   ''is 0 (analysis)'')')
         write (*, '(/, ''NCODA_MAP environmental variables:'')')
         write (*, '(5x, ''OCN_OUTPUT_DIR = directory path to '',
     *                   ''analysis restart files'')')
         write (*, '(5x, ''OCN_CLIM_DIR   = directory path to '',
     *                   ''ocean climate files'')')
         write (*, '(5x, ''OCN_ENSM_DIR   = directory path to '',
     *                   ''ensemble member files'')')
         write (*, '(5x, ''OCN_ADJ_DIR    = directory path to '',
     *                   ''adjoint sensitivity files'')')
         write (*, '(/, ''NCODA_MAP namelist variables:'')')
         write (*, '(5x, ''&omapnl'')')
         write (*, '(7x, ''adj_mode      = adjoint mode (impact or '',
     *                   ''target)'')')
         write (*, '(7x, ''adj_tau       = adjoint forecast period '',
     *                   ''to plot'')')
         write (*, '(7x, ''btm_max       = max bottom depth to ''
     *                   ''plot'')')
         write (*, '(7x, ''do_arctic     = (true) plot arctic '',
     *                   ''fields'')')
         write (*, '(7x, ''do_antarctic  = (true) plot antarctic '',
     *                   ''fields'')')
         write (*, '(7x, ''do_arch       = (true) plot HYCOM '',
     *                   ''initial conditions'')')
         write (*, '(7x, ''do_btm        = (true) plot grid '',
     *                   ''bathymetry and open sea nodes'')')
         write (*, '(7x, ''do_ch         = (true) plot Cooper-Haines '',
     *                   ''profiles'')')
         write (*, '(7x, ''do_clm        = (true) plot climate '',
     *                   ''error'')')
         write (*, '(7x, ''do_clm_err    = (true) plot integral '',
     *                   ''climate error'')')
         write (*, '(7x, ''do_data       = (true) plot observation '',
     *                   ''locations'')')
         write (*, '(7x, ''do_data_raw   = (true) plot raw '',
     *                   ''observation locations'')')
         write (*, '(7x, ''do_fcst_diff  = (true) plot forecast '',
     *                   ''differences'')')
         write (*, '(7x, ''do_dist       = (true) plot land '',
     *                   ''distance field'')')
         write (*, '(7x, ''do_diurn      = (true) plot sst diurnal '',
     *                   ''anomalies'')')
         write (*, '(7x, ''do_ensm       = (true) plot ensemble '',
     *                   ''increment fields'')')
         write (*, '(7x, ''do_gpt        = (true) plot geopotential'')')
         write (*, '(7x, ''do_gpt_inc    = (true) plot geopotential '',
     *                   ''analysis increments'')')
         write (*, '(7x, ''do_grdnt      = (true) plot gradient '',
     *                   ''field for selected variables'')')
         write (*, '(7x, ''do_mdl_bias   = (true) plot model bias '',
     *                   ''correction fields'')')
         write (*, '(7x, ''do_ice        = (true) plot sea ice'')')
         write (*, '(7x, ''do_ice_err    = (true) plot sea ice '',
     *                   ''prediction error'')')
         write (*, '(7x, ''do_ice_inc    = (true) plot sea ice '',
     *                   ''analysis increments'')')
         write (*, '(7x, ''do_jmin       = (true) plot jmin '',
     *                   ''diagnostic time series'')')
         write (*, '(7x, ''do_lyp_inc    = (true) plot layer '',
     *                   ''pressure increments'')')
         write (*, '(7x, ''do_mld        = (true) plot mixed layer '',
     *                   ''depth field'')')
         write (*, '(7x, ''do_ohc        = (true) plot ocean heat '',
     *                   ''content fields'')')
         write (*, '(7x, ''do_prf_fcst   = (true) plot forecast '',
     *                   ''profiles'')')
         write (*, '(7x, ''do_prf_vfy    = (true) plot verification '',
     *                   ''profiles'')')
         write (*, '(7x, ''do_prs        = (true) plot bottom '',
     *                   ''pressure'')')
         write (*, '(7x, ''do_sal        = (true) plot salinity'')')
         write (*, '(7x, ''do_sal_err    = (true) plot salinity '',
     *                   ''prediction error'')')
         write (*, '(7x, ''do_sal_inc    = (true) plot salinity '',
     *                   ''analysis increments'')')
         write (*, '(7x, ''do_scl        = (true) plot length '',
     *                   ''scales'')')
         write (*, '(7x, ''do_sens       = (true) plot obs '',
     *                    ''sensitivities'')')
         write (*, '(7x, ''do_sfc_vel    = (true) plot surface '',
     *                   ''velocity'')')
         write (*, '(7x, ''do_sla        = (true) plot along-track '',
     *                   ''SLA'')')
         write (*, '(7x, ''do_ssh        = (true) plot SSH'')')
         write (*, '(7x, ''do_ssh_err    = (true) plot SSH '',
     *                   ''prediction error'')')
         write (*, '(7x, ''do_ssh_inc    = (true) plot SSH '',
     *                   ''analysis increments'')')
         write (*, '(7x, ''do_sss        = (true) plot SSS'')')
         write (*, '(7x, ''do_sss_err    = (true) plot SSS '',
     *                   ''prediction error'')')
         write (*, '(7x, ''do_sss_inc    = (true) plot SSS '',
     *                   ''analysis increments'')')
         write (*, '(7x, ''do_sst        = (true) plot SST'')')
         write (*, '(7x, ''do_sst_clm    = (true) plot SST '',
     *                   ''climate anomaly'')')
         write (*, '(7x, ''do_sst_err    = (true) plot SST '',
     *                   ''prediction error'')')
         write (*, '(7x, ''do_sst_inc    = (true) plot SST '',
     *                   ''analysis increments'')')
         write (*, '(7x, ''do_stats      = (true) plot 2D STATS '',
     *                   ''time series'')')
         write (*, '(7x, ''do_stats_lvl  = (true) plot 3D STATS '',
     *                   ''time series'')')
         write (*, '(7x, ''do_stats_fcst = (true) plot forecast '',
     *                   ''STATS time series'')')
         write (*, '(7x, ''do_tmp        = (true) plot 3D '',
     *                   ''temperature'')')
         write (*, '(7x, ''do_tmp_err    = (true) plot 3D '',
     *                   ''temperature prediction error'')')
         write (*, '(7x, ''do_tmp_inc    = (true) plot 3D '',
     *                   ''temperature analysis increments'')')
         write (*, '(7x, ''do_vel        = (true) plot velocity'')')
         write (*, '(7x, ''do_vel_err    = (true) plot velocity '',
     *                   ''prediction error'')')
         write (*, '(7x, ''do_vel_inc    = (true) plot velocity '',
     *                   ''analysis increments'')')
         write (*, '(7x, ''do_vfy        = (true) plot innovation '',
     *                   ''scatter plots'')')
         write (*, '(7x, ''do_vol        = (true) plot analysis '',
     *                   ''volume definitons'')')
         write (*, '(7x, ''do_vol_act    = (true) plot observation '',
     *                   ''volume interactions'')')
         write (*, '(7x, ''dtg1          = starting dtg for time '',
     *                   ''series plots (do_stats_*)'')')
         write (*, '(7x, ''nest          = nest number to plot'')')
         write (*, '(7x, ''n_plot        = number levels to plot'')')
         write (*, '(7x, ''z_plot        = indicies of vertical '',
     *                   ''levels to plot'')')
         write (*, '(7x, ''plot_lyr      = plot layer fields'')')
         write (*, '(7x, ''sal_cnt       = salinity contour '',
     *                   ''interval'')')
         write (*, '(7x, ''sal_del       = salinity color slicing '',
     *                   ''interval'')')
         write (*, '(7x, ''sal_grd       = maximum salinity '',
     *                   ''gradient'')')
         write (*, '(7x, ''sal_min       = minimum salinity for '',
     *                   ''color slicing'')')
         write (*, '(7x, ''ssh_del       = minimum change in SSH '',
     *                   ''for CH plots'')')
         write (*, '(7x, ''sss_cnt       = sss contour interval'')')
         write (*, '(7x, ''sss_del       = sss color slicing '',
     *                   ''interval'')')
         write (*, '(7x, ''sss_min       = minimum sss for color '',
     *                   ''slicing'')')
         write (*, '(7x, ''sst_cnt       = sst contour interval'')')
         write (*, '(7x, ''sst_del       = sst color slicing '',
     *                   ''interval'')')
         write (*, '(7x, ''sst_min       = minimum sst for color '',
     *                   ''slicing'')')
         write (*, '(7x, ''tmp_cnt       = temperature contour '',
     *                   ''interval'')')
         write (*, '(7x, ''tmp_del       = temperature color '',
     *                   ''slicing interval'')')
         write (*, '(7x, ''tmp_grd       = maximum temperature '',
     *                    ''gradient'')')
         write (*, '(7x, ''tmp_min       = minimum temperature for '',
     *                   ''color slicing'')')
         write (*, '(7x, ''vel_max       = velocity maximum at each '',
     *                   ''level'')')
         write (*, '(7x, ''vel_thn       = skip every (i,j) vector '',
     *                   ''in velocity overlay'')')
         write (*, '(7x, ''zoom          = lat,lon box for '',
     *                   ''subsetting grid, '')')
         write (*, '(7x, ''                bottom lat, top lat, '',
     *                   ''left lon, right lon'')')
         write (*, '(5x, ''&end'')')
         write (*, '(/, 5x, ''NOTES: 1. set contour intervals to '',
     *                      ''zero or a negative'')')
         write (*, '(5x, ''          value to suppress drawing '',
     *                   ''contour lines'')')
         write (*, '(5x, ''       2. set vel_thn to negative values '',
     *                   ''to suppress'')')
         write (*, '(5x, ''          vector overlays'')')
         write (*, '(5x, ''       3. "do_*" variable default values '',
     *                   ''are false'')')
         write (*, '(5x, ''          (program does nothing)'')')
         write (*, '(5x, ''       4. there are 60 colors, think '',
     *                   ''about that when you'')')
         write (*, '(5x, ''          set the *_del and *_min '',
     *                   ''namelist variables'')')
         stop
      endif
c
c     ..retrieve analysis dtg argument
c
      if (n_arg .gt. 0) then
         call GETARG (1, arg)
         dtg = arg(1:10)
      else
         write (err_msg, '(''no DTG argument specified'')')
         call error_exit ('NCODA_MAP', err_msg)
      endif
c
c     ..retrieve forecast tau argument (optional)
c
      if (n_arg .gt. 1) then
         call GETARG (2, arg)
         read (arg(1:3), '(i3)') tau_hr
      else
         tau_hr = 0
      endif
      call dtgmod (dtg, -tau_hr, file_dtg, status)
c     file_dtg = dtg
c
c     ..retrieve restart file directory path
c
      call GETENV ('OCN_OUTPUT_DIR', out_dir)
      len = len_trim (out_dir)
      if (len .eq. 0) then
         write (err_msg, '(''missing OCN_OUTPUT_DIR '',
     *                     ''environmental variable'')')
         call error_exit ('NCODA_MAP', err_msg)
      endif
c
c     ..retrieve ocean climate file directory path
c
      call GETENV ('OCN_CLIM_DIR', clm_dir)
      len = len_trim (clm_dir)
      if (len .eq. 0) then
         write (*, '(''WARNING: missing OCN_CLIM_DIR environmental '',
     *               ''variable'')')
         clm_dir = '/scratch2/NCEPDEV/marine/Jim.Cummings/codaclim'
      endif
c
c     ..check for ncoda ocean map namelist
c
      inquire (file='omapnl', exist=exist)
      if (exist) then
         open (9, file='omapnl', status='old', form='formatted')
         read (9, omapnl)
         close (9)
      else
         write (err_msg, '(''omapnl namelist file missing'')')
         call error_exit ('NCODA_MAP', err_msg)
      endif
c
c-------------------------------------------------------------------
c
c     ..form analysis and forecast date time group label
c
      read (dtg(1:4),  '(i4)') year
      read (dtg(5:6),  '(i2)') mon
      read (dtg(7:8),  '(i2)') day
      read (dtg(9:10), '(i2)') zulu
      write (date_dtg, '(i2.2, 1x, a, 1x, i4, 1x, i2.2, ''Z'')')
     *       day, month(mon), year, zulu
c
      read (file_dtg(1:4),  '(i4)') year
      read (file_dtg(5:6),  '(i2)') mon
      read (file_dtg(7:8),  '(i2)') day
      read (file_dtg(9:10), '(i2)') zulu
      write (date, '(i2.2, 1x, a, 1x, i4, 1x, i2.2, ''Z'')')
     *       day, month(mon), year, zulu
c
c     ..read datao header record
c
      fluid = 'o'
      m = 2000
      n = 1
      tau = 0
      file_type = 'infofld'
      fld_name = 'datahd'
      lvl_type = 'hdr'
      call cr_fname (out_dir, dtg, n, m, n, file_type, fld_name,
     *               fluid, lvl_type, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (10, file=file_name(1:len), status='old',
     *             access='stream', form='unformatted')
         read (10) datao
         close (10)
      else
         write (err_msg, '(a, '' datahd file missing'')') dtg
         write (*, '(''fn: '', a)') trim (file_name)
         call error_exit ('NCODA_MAP', err_msg)
      endif
c
c     ..set tripolar global and polar interpolation array dimensions
c
      gln = 4449
      glt = 2457
      pln = 900
      plt = 900
c
c     ..read header record, set zoom/plot parameters
c
      call set_zoom (datao, nest, gln, glt, zoom, m, n, l, global, 
     *               igrid, delx, dely, rlat, stdlt1, stdlt2, 
     *               stdlon, upd, zlvl, bl, br, tl, tr, i1, i2,
     *               j1, j2, subset)
c
c     ..set ssh assimilation option
c
      ssh_opt = nint (datao(23))
c*****************
      ssh_opt = 1
c*****************
c
c     ..diagnostics
c
      write (*, '(/, ''===================================='',
     *            /, ''======= NCODA - Map Analysis ======='',
     *            /, ''========= NCEP Version 3.20 ========'',
     *            /, ''===================================='')')
      write (*, '(''       analysis date: '', a)') date_dtg
      write (*, '(''       forecast date: '', a)') date
      write (*, '(''  analysis directory: '', a)') trim (out_dir)
      write (*, '(''   climate directory: '', a)') trim (clm_dir)
      write (*, '(''    grid nest number: '', i10)') nest
      write (*, '(''     forecast period: '', i10)') tau_hr
      write (*, '(''        forecast dtg: '', a)') file_dtg       
      write (*, '(''        update cycle: '', i10)') upd
      if (ssh_opt .eq. 1) then
         write (*, '(''          ssh option: '', 7x, ''ADT'')')
      else if (ssh_opt .eq. 2) then
         write (*, '(''          ssh option: '', 7x, ''SLA'')')
      endif
      write (*, '(''     grid dimensions: '', 3i10)') m, n, l
      write (*, '(''           grid mesh: '', 2f10.1)') delx, dely
      write (*, '(''     grid projection: '', i10)') igrid
      write (*, '(''         global grid: '', l10)') global
      write (*, '(''             ref lat: '', f10.2)') rlat
      write (*, '(''            stnd lon: '', f10.2)') stdlon
      write (*, '(''     stnd lat1, lat2: '', 2f10.2)') stdlt1,stdlt2
      if (igrid .lt. 0) then
         write (*, '(''   layer plot arrays: '', 4i10)') 
     *          gln, glt, pln, plt
      endif
      write (*, '(''  subset coordinates: '', l10)') subset
      write (*, '(''     i,j coordinates: '', 4i10)') i1, i2, j1, j2
      write (*, '(''         bottom left: '', 2f10.1)') bl(1), bl(2)
      write (*, '(''        bottom right: '', 2f10.1)') br(1), br(2)
      write (*, '(''            top left: '', 2f10.1)') tl(1), tl(2)
      write (*, '(''           top right: '', 2f10.1)') tr(1), tr(2)
      write (*, '(''       special value: '', f10.1)') SPVAL
      if (dtg1(1:1) .ne. '0') then
         write (*, '(''           start dtg: '', a)') dtg1
      endif
      write (*, '(''       vertical grid: '')')
      if (l .gt. 0) then
         write (*, '(4(i10, '')'', f10.2))') (j, zlvl(j), j = 1, l)
      endif
      write (*, '('' '')')
c
c     ..open gks
c
      call opngks
c
c     ..plot fields
c
      call plot_flds (out_dir, clm_dir, date, date_dtg, dtg, file_dtg,
     *                tau_hr, upd, m, n, l, zlvl, delx, dely, igrid, 
     *                rlat, stdlt1, stdlt2, stdlon, bl, br, tl, tr,
     *                i1, i2, j1, j2, gln, glt, pln, plt, global,
     *                subset, ssh_opt, SPVAL)
c
c     ..close gks
c
      call clsgks
c
      stop
      end
