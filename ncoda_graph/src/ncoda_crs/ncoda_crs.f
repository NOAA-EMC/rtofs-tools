      program ncoda_crs
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  ncoda_crs
c
c DESCRIPTION:  plots cross sections from a ncoda ocean analysis
c               supports ensemble cross sections
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
c   plot_sfc           plot surface analysis field
c   GETARG             retrieve command line argument
c   GETENV             retrieve environmental variable
c
c LOCAL VARIABLES AND STRUCTURES:
c     Name            Type                 Description
c   ---------       --------    ----------------------------------
c   dtg             char        date time group (YYYYMMDDHH)
c   err_msg         char        standard error processing message
c   outp_dir        char        output file root directory path
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
      real      datao (2000)
      character date * 15
      character date_bk * 15
      integer   day
      real      delx, dely
      character dtg * 10
      integer   end_dpth (20)
      real      end_lat (20), end_lon (20)
      character ensm_dir * 256
      character err_msg * 256
      logical   exist
      character file_dtg * 10
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      logical   global
      integer   igrid
      integer   iref, jref
      integer   j, k
      integer   len
      integer   len_dir
      character lvl_typ * 3
      integer   m, n, l
      integer   mon
      character month (12) * 3
      character mrun * 80
      integer   n_arg
      integer   n_xsect
      character outp_dir * 256
      real      reflat, reflon
      real      start_lat (20), start_lon (20)
      integer   status
      real      stdlt1, stdlt2
      real      stdlon
      integer   tau, tau_hr
      real      tl (2), tr (2)
      real      xl, xr
      integer   year
      real      zlvl (100)
      integer   zulu
c
c     ..functions
c
      character dtgchk * 10
      integer   IARGC
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c-----------------------------------------------------------------------
c
c ncoda ocean cross namelist
c
c PARAMETERS:
c       Name          Type                 Description
c   -------------   --------    ----------------------------------------
c   n_mem           integer     number ensemble members
c   nest            integer     grid nest number
c   plt_typ         character   plot increments, full fields, error
c                               fields, correlation fields, or
c                               ensemble perturbation fields
c
c     ..ncoda ocean cross namelist declarations
c
      integer   n_mem
      integer   nest
      logical   plt_lyr
      character plt_typ * 5
c
c     ..default values for cross section namelist
c
      data      n_mem   / 8 /
      data      nest    / 1 /
      data      plt_lyr / .false. /
      data      plt_typ / 'full ' /
c
c     ..cross section namelist statement
c
      namelist /ocrsnl/ n_mem, nest, plt_lyr, plt_typ
c
c...............................executable..............................
c
c     ..count number command line arguments
c
      n_arg = IARGC ()
      if (n_arg .eq. 0) then
         write (*, '(/, ''NCODA_CRS NCEP v3.20 command line '',
     *                  ''arguments:'')')
         write (*, '(5x, ''ncoda_crs {dtg} {tau}'')')
         write (*, '(5x, ''{dtg} = date time group (yyyymmddhh) '',
     *                      ''cross section'')')
         write (*, '(5x, ''{tau} = forecast period (hours) '',
     *                   ''cross section'')')
         write (*, '(5x, ''NOTES: 1. {dtg} is required'')')
         write (*, '(5x, ''       2. {tau} is optional, '',
     *                   ''if not specified, default tau is 0'')')
         write (*, '(/, ''NCODA_CRS namelist (ocrsnl) variables:'')')
         write (*, '(5x, ''&ocrsnl'')')
         write (*, '(7x, ''n_mem   = number ensemble members '',
     *                   ''(default is 8)'')')
         write (*, '(7x, ''nest    = grid nest number '',
     *                   ''(default is 1)'')')
         write (*, '(7x, ''plt_lyr = (true) plot layer fields'')')
         write (*, '(7x, ''          (default is false)'')')
         write (*, '(7x, ''plt_typ = "full" full valued fields'')')
         write (*, '(7x, ''          "incr" increment fields'')')
         write (*, '(7x, ''          "error" forecast error fields'')')
         write (*, '(7x, ''          "vcorr" vertical correlations'')')
         write (*, '(7x, ''          "ensm" ensemble perturbations'')')
         write (*, '(7x, ''          (default is full)'')')
         write (*, '(5x, ''&end'')')
         write (*, '(/, ''NCODA_CRS environmental variables:'')')
         write (*, '(5x, ''OCN_OUTPUT_DIR = directory path to '',
     *                   ''restart files'')')
         write (*, '(5x, ''OCN_ENSM_DIR = directory path to '',
     *                   ''ensemble files (optional)'')')
         stop
      endif
c
c     ..retrieve dtg argument
c
      call GETARG (1, arg)
      dtg = arg(1:10)
c
c     ..check for valid dtg
c
      if (index (dtgchk(dtg), '*') .ne. 0) then
         write (err_msg, '(''invalid DTG1 argument specified "'',
     *                     a, ''"'')') dtg
         call error_exit ('NCODA_CRS', err_msg)
      endif
c
c     ..check for forecast period argument
c
      tau_hr = 0
      if (n_arg .gt. 1) then
         call GETARG (2, arg)
         read (arg(1:3), '(i3)') tau_hr
      endif
c
c     ..check for ncoda ocean crs namelist
c
      inquire (file='ocrsnl', exist=exist)
      if (exist) then
         open (9, file='ocrsnl', status='old', form='formatted')
         read (9, ocrsnl)
         close (9)
      else
         write (*, '(/, ''***** WARNING (NCODA_CRS): "ocrsnl" '',
     *                  ''namelist file missing'')')
         write (*, '(''                           using '',
     *               ''default settings'')')
      endif
c
c     ..retrieve file root directory
c
      call GETENV ('OCN_OUTPUT_DIR', outp_dir)
      len_dir = len_trim (outp_dir)
      if (len_dir .eq. 0) then
         write (err_msg, '(''missing OCN_OUTPUT_DIR environmental '',
     *                     ''variable'')')
         call error_exit ('NCODA_CRS', err_msg)
      endif
c
c     ..retrieve ensemble file root directory
c
      if (plt_typ(1:4) .eq. 'ensm') then
         call GETENV ('OCN_ENSM_DIR', ensm_dir)
         len_dir = len_trim (ensm_dir)
         if (len_dir .eq. 0) then
            write (err_msg, '(''missing OCN_ENSM_DIR environmental '',
     *                        ''variable'')')
            call error_exit ('NCODA_CRS', err_msg)
         endif
      endif
c
c-----------------------------------------------------------------------
c
c     ..form date time group labels
c
      read (dtg(1:4),  '(i4)') year
      read (dtg(5:6),  '(i2)') mon
      read (dtg(7:8),  '(i2)') day
      read (dtg(9:10), '(i2)') zulu
      write (date, '(i2.2, 1x, a, 1x, i4, 1x, i2.2, ''Z'')')
     *       day, month(mon), year, zulu
      if (plt_typ .eq. 'vcorr') then
         if (tau_hr .gt. 0) then
            call dtgmod (dtg, -tau_hr, file_dtg, status)
            read (file_dtg(1:4),  '(i4)') year
            read (file_dtg(5:6),  '(i2)') mon
            read (file_dtg(7:8),  '(i2)') day
            read (file_dtg(9:10), '(i2)') zulu
            write (date_bk, '(i2.2, 1x, a, 1x, i4, 1x, i2.2, ''Z'')')
     *             day, month(mon), year, zulu
         else
            date_bk = date
         endif
      else
         date_bk = date
      endif
c
c     ..read datao record
c
      fluid = 'o'
      m = 2000
      n = 1
      k = 1
      tau = 0
      file_typ = 'infofld'
      fld_name = 'datahd'
      lvl_typ = 'hdr'
      call cr_fname (outp_dir, dtg, k, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (10, file=file_name(1:len), status='old',
     *             access='stream', form='unformatted')
         read (10) datao
         close (10)
      else
         write (err_msg, '(''datahd file missing, dtg = '', a)') dtg
         call error_exit ('NCODA_CRS', err_msg)
      endif
      len = len_trim (outp_dir)
c
c     ..decode grid information
c
      k = 30 + (nest-1) * 30
c
c     ..set grid mesh
c
      delx = datao(k+4)
      dely = datao(k+5)
c
c     ..set grid geometry
c
      m = nint (datao(k+0))
      n = nint (datao(k+1))
      l = nint (datao(1))
      igrid = nint (datao(3))
      iref = nint (datao(k+2))
      jref = nint (datao(k+3))
      stdlt1 = datao(4)
      stdlt2 = datao(5)
      stdlon = datao(6)
      reflat = datao(7)
      reflon = datao(8)
c
c     ..set grid corner points
c
      bl(1) = datao(k+6)
      bl(2) = datao(k+7)
      br(1) = datao(k+8)
      br(2) = datao(k+9)
      tr(1) = datao(k+10)
      tr(2) = datao(k+11)
      tl(1) = datao(k+12)
      tl(2) = datao(k+13)
c
c     ..set global grid flag
c
      if (datao(22) .gt. 0.) then
         global = .true.
      else
         global = .false.
      endif
c
c     ..check number levels
c
      if (l .lt. 5) then
         write (err_msg, '(''too few vertical levels "'', i1,
     *                     ''" for cross sections'')') l
         call error_exit ('NCODA_CRS', err_msg)
      endif
c
c     ..set vertical grid 
c
      do k = 1, l
         zlvl(k) = datao(k+500)
      enddo
c
c     ..initialize section definitions
c
      do k = 1, 20
         start_lat(k) = 0.
         end_lat(k) = 0.
         start_lon(k) = 0.
         end_lon(k) = 0.
         end_dpth(k) = 0
      enddo
c
c     ..prompt user to define great circle path(s)
c
      write (*, '(/, ''Analysis Domain: '', 2f10.2)') bl(1), tr(1)
      if (bl(2) .gt. 180.) then
         xl = bl(2) - 360.
      else
         xl = bl(2)
      endif
      if (tr(2) .gt. 180.) then
         xr = tr(2) - 360.
      else
         xr = tr(2)
      endif
      write (*, '(17x, 2f10.2)') xl, xr
      if (global) then
         mrun = 'Global HYCOM   '
c        mrun = 'Global MOM     '
      else
         mrun = 'Regional HYCOM '
      endif
      write (*, '(/, ''Enter number cross sections to plot'')')
      read (*, *) n_xsect
      if (n_xsect .gt. 20) n_xsect = 20
      do k = 1, n_xsect
         write (*, '(/, ''Enter cross section starting lat, lon'')')
         read (*, *) start_lat(k), start_lon(k)
         write (*, '(''Enter cross section ending lat, lon'')')
         read (*, *) end_lat(k), end_lon(k)
         write (*, '(''Enter cross section depth (m)'')')
         read (*, *) end_dpth(k)
      enddo
c
      write (*, '(/, ''===================================='',
     *            /, ''====== NCODA - Cross Section ======='',
     *            /, ''======== NCEP Version 3.20 ========='',
     *            /, ''===================================='')')
      write (*, '(''  analysis directory: '', a)') trim (outp_dir)
      if (plt_typ .eq. 'ensm') then
         write (*, '(''  ensemble directory: '', a)') trim (ensm_dir)
      endif
      write (*, '(''           model run: '', a)') trim (mrun)
      write (*, '(''         plot layers: '', l10)') plt_lyr
      write (*, '(''           plot type: '', 5x,a)') plt_typ
      write (*, '(''    grid nest number: '', i10)') nest
      write (*, '(''     grid dimensions: '', 3i10)') m, n, l
      write (*, '(''           grid mesh: '', 2f10.1)') delx, dely
      write (*, '(''     grid projection: '', i10)') igrid
      write (*, '(''    center longitude: '', f10.1)') stdlon
      write (*, '(''      i, j reference: '', 2i10)') iref, jref
      write (*, '(''        ref lat, lon: '', 2f10.2)') reflat,reflon
      write (*, '(''     stnd lat1, lat2: '', 2f10.2)') stdlt1,stdlt2
      write (*, '(''         bottom left: '', 2f10.1)') bl(1), bl(2)
      write (*, '(''        bottom right: '', 2f10.1)') br(1), br(2)
      write (*, '(''            top left: '', 2f10.1)') tl(1), tl(2)
      write (*, '(''           top right: '', 2f10.1)') tr(1), tr(2)
      write (*, '(''       analysis date: '', a)') date
      write (*, '(''     forecast period: '', i10)') tau_hr
      if (plt_typ .eq. 'ensm') then
         if (l .gt. 0) then
            write (*, '(4(i10, '')'', f10.2))') (j, zlvl(j), j = 1, l)
         endif
      endif
      write (*, '(/, ''     number sections: '', i10)') n_xsect
      if (n_xsect .gt. 0) then
         do k = 1, n_xsect
            write (*, '(''   starting lat, lon: '', 2f10.1)')
     *             start_lat(k), start_lon(k)
            write (*, '(''     ending lat, lon: '', 2f10.1)')
     *             end_lat(k), end_lon(k)
            write (*, '(''       maximum depth: '', i10)') end_dpth(k)
         enddo
      else
         write (err_msg, '(''no cross sections defined'')')
         call error_exit ('NCODA_CRS', err_msg)
      endif
c
c     ..open gks
c
      call opngks
c
c     ..plot cross sections
c
      if (plt_typ(1:4) .eq. 'ensm') then
         call plot_xsect_ensm (outp_dir, ensm_dir, date, dtg, m, n, l,
     *                         n_mem, igrid, nest, delx, dely, iref,
     *                         jref, reflat, reflon, stdlt1, stdlt2,
     *                         stdlon, n_xsect, start_lat, start_lon,
     *                         end_lat, end_lon, end_dpth, zlvl,
     *                         plt_lyr, SPVAL)
      else
         call plot_xsect (outp_dir, date, date_bk, dtg, tau_hr,
     *                    plt_typ, mrun, m, n, l, igrid, nest,
     *                    delx, dely, iref, jref, reflat, reflon,
     *                    stdlt1, stdlt2, stdlon, n_xsect, start_lat,
     *                    start_lon, end_lat, end_lon, end_dpth,
     *                    plt_lyr, SPVAL)
      endif
c
c     ..close gks
c
      call clsgks
c
      stop
      end
