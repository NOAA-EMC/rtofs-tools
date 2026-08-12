      program ncoda_movie
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  ncoda_movie
c
c DESCRIPTION:  maps a time series of NCODA analyses
c
c RESTRICTIONS:    None
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
c   plot_movie         plot movie fields
c   GETARG             retrieve command line argument
c   GETENV             retrieve environmental variable
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
      integer    UNIT
      parameter (UNIT = 60)
c
      real      anl_lvl (100)
      character arg * 80
      real      bl (2), br (2)
      character clm_dir * 256
      real      datao (2000)
      real      delx, dely
      character end_dtg * 10
      character err_msg * 256
      logical   exist
      logical   fcst
      character file_name * 256
      character file_type * 7
      character fld_name * 6
      character fluid * 1
      integer   gln, glt
      logical   global
      integer   i1, i2, j1, j2
      integer   igrid
      integer   len
      character lvl_type * 3
      integer   m, n, l
      integer   n_arg
      integer   n_tau
      character out_dir * 256
      integer   pln, plt
      real      rlat
      real      stdlt1, stdlt2
      real      stdlon
      character strt_dtg * 10
      logical   subset
      integer   tau
      integer   tau_hr
      real      tl (2), tr (2)
      integer   upd
c
c     ..functions
c
      integer   IARGC
c
c-----------------------------------------------------------------------
c
c COAMPS ncoda ocean movie namelist
c
c PARAMETERS:
c       Name          Type                 Description
c   -------------   --------    -------------------------------------
c   cntr            real        contour interval
c   del             real        color slicing interval
c   dmn, dmx        real        min/max color slicing
c   nest            integer     nest number to use
c   parm            character   parameter to plot
c                               sst, ssh, temp, terr, tinc,
c                               salt, serr, sinc, geop, velc
c   z_plot          integer     depth index to plot
c   zoom            real        lat,lon box for zoomed grid
c
c     ..movie namelist declarations
c
      real      cntr
      real      del
      real      dmn, dmx
      integer   nest
      character parm * 7
      integer   z_plot
      real      zoom (4)
c
c     ..default values for movie namelist
c
      data      cntr   / 1. /
      data      del    / 0.5 /
      data      dmn    / 0. /
      data      dmx    / -999. /
      data      nest   / 1 /
      data      parm   / 'sst    ' /
      data      z_plot / 1 /
      data      zoom   / -999., -999., -999., -999. /
c
c     ..grid namelist statement
c
      namelist /omovienl/ cntr, del, dmn, dmx, nest, parm, z_plot, zoom
c
c...............................executable..............................
c
c     ..count number command line arguments
c
      n_arg = IARGC ()
      if (n_arg .eq. 0) then
         write (*, '(/, ''NCODA_MOVIE NCEP v3.20 command line '',
     *                  ''arguments:'')')
         write (*, '(5x, ''ncoda_movie {strt_dtg} {end_dtg} '',
     *                   ''{tau} {n_tau}'')')
         write (*, '(5x, ''{strt_dtg} = starting date time group '',
     *                   ''(yyyymmddhh)'')')
         write (*, '(5x, ''{end_dtg}  = ending date time group '',
     *                   ''(yyyymmddhh)'')')
         write (*, '(5x, ''{tau}      = forecast (or update) '',
     *                   ''period (hours)'')')
         write (*, '(5x, ''{n_tau}    = number forecast periods'')')
         write (*, '(/, ''NCODA_MOVIE environmental variables:'')')
         write (*, '(5x, ''OCN_OUTPUT_DIR = directory path to '',
     *                   ''analysis restart files'')')
         write (*, '(5x, ''OCN_CLIM_DIR = directory path to '',
     *                   ''ocean climate files'')')
         write (*, '(/, ''NCODA_MOVIE namelist variables:'')')
         write (*, '(5x, ''&omovienl'')')
         write (*, '(7x, ''del    = color slicing interval'')')
         write (*, '(7x, ''cntr   = contour interval'')')
         write (*, '(7x, ''dmn    = minimum color slicing'')')
         write (*, '(7x, ''dmx    = maximum color slicing'')')
         write (*, '(7x, ''z_plot = indicies of vertical level '',
     *                   ''to plot'')')
         write (*, '(7x, ''nest   = grid nest number'')')
         write (*, '(7x, ''parm   = plot parameters (ssh, sst '',
     *                   ''sst_err, sst_inc, tmp, tmp_err,'')')
         write (*, '(7x, ''                         '',
     *                   '' tmp_inc, sal, sal_err, sal_inc)'')')
         write (*, '(7x, ''zoom   = lat,lon box for subsetting grid'')')
         write (*, '(7x, ''         bottom lat, top lat, left lon, '',
     *                   ''right lon'')')
         write (*, '(5x, ''&end'')')
         write (*, '(/, 5x, ''NOTES: 1. if strt_dtg equals end_dtg, '',
     *                   ''then tau is forecast period'')')
         write (*, '(5x, ''       2. if strt_dtg does not equal '',
     *                   ''end_dtg, then tau is update cycle'')')
         write (*, '(5x, ''       3. if strt_dtg equals end_dtg, '',
     *                   ''then n_tau is the number of'')')
         write (*, '(5x, ''          forecast periods to plot'')')
         write (*, '(5x, ''       4. set cntr to zero in omovienl '',
     *                   ''to suppress drawing contours'')')
         stop
      endif
c
c     ..retrieve analysis dtg arguments
c
      if (n_arg .gt. 0) then
         call GETARG (1, arg)
         strt_dtg = arg(1:10)
      else
         write (err_msg, '(''no starting DTG argument specified'')')
         call error_exit ('NCODA_MOVIE', err_msg)
      endif
      if (n_arg .gt. 1) then
         call GETARG (2, arg)
         end_dtg = arg(1:10)
      else
         write (err_msg, '(''no ending DTG argument specified'')')
         call error_exit ('NCODA_MOVIE', err_msg)
      endif
c
c     ..check for forecast time series
c
      if (strt_dtg .eq. end_dtg) then
         fcst = .true.
      else
         fcst = .false.
      endif
c
      if (fcst) then
c
c        ..retrieve forecast tau argument
c
         if (n_arg .gt. 2) then
            call GETARG (3, arg)
            read (arg(1:3), '(i3)') tau_hr
         else
            write (err_msg, '(''no TAU argument specified'')')
            call error_exit ('NCODA_MOVIE', err_msg)
         endif
c
c        ..retrieve number forecast periods to plot
c
         if (n_arg .gt. 3) then
            call GETARG (4, arg)
            read (arg(1:3), '(i3)') n_tau
         else
            n_tau = 72
            write (*, '(''*** WARNING : number forecast periods '',
     *                  ''not specified'')')
            write (*, '(''              using default = '',
     *             i3)') n_tau
         endif
      endif
c
c     ..check for ocean movie namelist
c
      inquire (file='omovienl', exist=exist)
      if (exist) then
         open (9, file='omovienl', status='old', form='formatted')
         read (9, omovienl)
         close (9)
      else
         write (*, '(/, ''***** WARNING (NCODA_MOVIE): omovienl '',
     *                  ''namelist file missing'')')
         write (*, '(''                              running '',
     *               '' with default settings'')')
      endif
c
c     ..retrieve file root directory
c
      call GETENV ('OCN_OUTPUT_DIR', out_dir)
      len = len_trim (out_dir)
      if (len .eq. 0) then
         write (err_msg, '(''WARNING: missing OCN_OUTPUT_DIR '',
     *                     ''environmental variable'')')
         call error_exit ('NCODA_MOVIE', err_msg)
      endif
c
c     ..retrieve ocean climate file directory path
c
      call GETENV ('OCN_CLIM_DIR', clm_dir)
      len = len_trim (clm_dir)
      if (len .eq. 0) then
         write (*, '(''WARNING: missing OCN_CLIM '',
     *               ''environmental variable'')')
         clm_dir = '/scratch2/NCEPDEV/marine/Jim.Cummings/codaclim'
      endif
c
c     ..read datao header record 
c
      file_type = 'infofld'
      fld_name = 'datahd'
      fluid = 'o'
      lvl_type = 'hdr'
      m = 2000
      n = 1
      tau = 0
      call cr_fname (out_dir, strt_dtg, n, m, n, file_type, fld_name,
     *               fluid, lvl_type, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='old',
     *               access='stream', form='unformatted')
         read (UNIT) datao
         close (UNIT)
      else
         write (err_msg, '(a, '' datahd file missing'')') strt_dtg
         call error_exit ('NCODA_MOVIE', err_msg)
      endif
c
c     ..set hycom global interpolation array dimensions
c
      gln = 4449
      glt = 2457
      pln = 900
      plt = 900
c
c     ..set zoom/plot parameters
c
      call set_zoom (datao, nest, gln, glt, zoom, m, n, l, global, 
     *               igrid, delx, dely, rlat, stdlt1, stdlt2, 
     *               stdlon, upd, anl_lvl, bl, br, tl, tr, i1,
     *               i2, j1, j2, subset)
c
c     ..diagnostics
c
      write (*, '(/, ''==================================='',
     *            /, ''======== NCODA - Movie Maker ======'',
     *            /, ''========= NCEP Version 3.20 ======='',
     *            /, ''==================================='')')
      write (*, '(''        starting dtg: '', a)') strt_dtg
      write (*, '(''          ending dtg: '', a)') end_dtg
      write (*, '(''           parameter: '', 7x, a)') parm
      write (*, '(''      analysis level: '', f10.1)') anl_lvl(z_plot)
      write (*, '(''  analysis directory: '', a)') trim (out_dir)
      write (*, '(''   climate directory: '', a)') trim (clm_dir)
      write (*, '(''    grid nest number: '', i10)') nest
      write (*, '(''     forecast series: '', l10)') fcst
      if (fcst) then
         write (*, '(''     forecast period: '', i10)') tau_hr
         write (*, '(''    number forecasts: '', i10)') n_tau
      endif
      write (*, '(''     grid dimensions: '', 3i10)') m, n, l
      write (*, '(''           grid mesh: '', 2f10.1)') delx, dely
      write (*, '(''     grid projection: '', i10)') igrid
      write (*, '(''             ref lat: '', f10.2)') rlat
      write (*, '(''            stnd lon: '', f10.2)') stdlon
      write (*, '(''     stnd lat1, lat2: '', 2f10.2)') stdlt1,stdlt2
      if (igrid .lt. 0) then
         write (*, '(''   hycom plot arrays: '', 4i10)') 
     *          gln, glt, pln, plt
      endif
      write (*, '(''         subset grid: '', l10)') subset
      write (*, '(''     i,j coordinates: '', 4i10)') i1, i2, j1, j2
      write (*, '(''         bottom left: '', 2f10.1)') bl(1), bl(2)
      write (*, '(''        bottom right: '', 2f10.1)') br(1), br(2)
      write (*, '(''            top left: '', 2f10.1)') tl(1), tl(2)
      write (*, '(''           top right: '', 2f10.1)') tr(1), tr(2)
      write (*, '(''       special value: '', f10.1)') SPVAL
      write (*, '('' '')')
c
c     ..open gks
c
      call opngks
c
c     ..plot fields
c
      call plot_movie (out_dir, clm_dir, parm, strt_dtg, end_dtg, 
     *                 tau_hr, fcst, n_tau, m, n, l, anl_lvl, 
     *                 z_plot, igrid, nest, rlat, stdlt1, stdlt2,
     *                 stdlon, bl, br, tl, tr, i1, i2, j1, j2, 
     *                 gln, glt, pln, plt, global, cntr, del, 
     *                 dmn, dmx, SPVAL)
c
c     ..close gks
c
      call clsgks
c
      stop
      end
