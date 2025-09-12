      subroutine plot_vol (dir_path, date, dtg, nest, n_lon, n_lat,
     *                     mask, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_vol
c
c DESCRIPTION:  routine to plot the analysis volumes for each
c               analysis variable with the observation data
c               overlaid.  the analysis domain and the 
c               observations are defined in (i,j) space so 
c               the plot is not so pretty.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libgraphics.a
c
c PARAMETERS:
c     Name         Type       Usage            Description
c   ---------    --------    -------    ----------------------------
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c MAKEFILE:   ...ops/ocn/otis/src/sub/Makefile
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
c
      character date * 15
      character dir_path * (*)
      character dtg * 10
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, k
      integer   len, len_dir
      character lvl_typ * 3
      integer   mask (n_lon * n_lat)
      integer   nest
      integer   n_data
      integer   n_obs_vol
      integer   tau
      character title * 132
c******************
      character path * 256
c***************
c
c     ..allocatable arrays
c
      real,     allocatable :: obs_xi (:)
      real,     allocatable :: obs_yj (:)
      real,     allocatable :: obs_x1 (:)
      real,     allocatable :: obs_x2 (:)
      real,     allocatable :: obs_y1 (:)
      real,     allocatable :: obs_y2 (:)
c
c     ..set color tables
c
      include 'color_table.h'
c
c...............................executable..............................
c
c     ..initialization
c
      file_typ = 'voldata'
c*********************
      path = '/scratch2/NCEPDEV/marine/Jim.Cummings/err_test/work'
      len_dir = len_trim (path) + 1
c****************************
c     len_dir = len_trim (dir_path) + 1
      tau = 0
c
c     ..loop over analysis variables
c
      do k = 7, 7
         if (k .eq. 1) then
            title = 'Sea Ice Analysis Volumes'
            fld_name = 'icecov'
            fluid = 'o'
            lvl_typ = 'sfc'
         else if (k .eq. 2) then
            title = 'Sea Surface Temperature Observation Volumes'
            fld_name = 'seatmp'
            fluid = 'o'
            lvl_typ = 'sfc'
         else if (k .eq. 3) then
            title = 'Sea Surface Height Observation Volumes'
            fld_name = 'seahgt'
            fluid = 'o'
            lvl_typ = 'sfc' 
         else if (k .eq. 4) then
            title = 'Significant Wave Height Observation Volumes'
            fld_name = 'sigwht'
            fluid = 'w'
            lvl_typ = 'sfc'
         else if (k .eq. 5) then
            title = 'Multivariate Observation Volumes'
            fld_name = 'seatmp'
            fluid = 'o'
            lvl_typ = 'pre'
         else if (k .eq. 6) then
            title = 'Surface Velocity Observation Volumes'
            fld_name = 'ocnvel'
            fluid = 'o'
            lvl_typ = 'sfc' 
         else if (k .eq. 7) then
            title = 'Error Analysis Volumes'
            fld_name = 'erranl'
            fluid = 'o'
            lvl_typ = 'sfc'
         endif
c
c        ..create volume data file name, check for existence
c
c        call cr_fname (dir_path, dtg, nest, n_lon, n_lat, file_typ,
c    *                  fld_name, fluid, lvl_typ, tau, file_name,
c    *                  len)
c**************************************
         call cr_fname (path, dtg, nest, n_lon, n_lat, file_typ,
     *                  fld_name, fluid, lvl_typ, tau, file_name,
     *                  len)
c**************************************
         inquire (file=file_name(1:len), exist=exist)
      write (*,'(''fn: '',a)') trim (file_name)
         if (exist) then
            write (*, '(''         volume data: '', a)')
     *             file_name(len_dir:len)
            open (UNIT, file=file_name(1:len), status='unknown',
     *                  form='unformatted')
            read (UNIT) n_obs_vol, n_data
            if (n_data .gt. 0) then
c
c              ..allocate observation volume arrays
c
               allocate (obs_xi (n_data))
               allocate (obs_yj (n_data))
               allocate (obs_x1 (n_obs_vol))
               allocate (obs_x2 (n_obs_vol))
               allocate (obs_y1 (n_obs_vol))
               allocate (obs_y2 (n_obs_vol))
c
c              ..read observation locations and volume definitions
c
               read (UNIT) obs_xi(1:n_data)
               read (UNIT) obs_yj(1:n_data)
               read (UNIT) obs_x1(1:n_obs_vol)
               read (UNIT) obs_x2(1:n_obs_vol)
               read (UNIT) obs_y1(1:n_obs_vol)
               read (UNIT) obs_y2(1:n_obs_vol)
c
               close (UNIT)
c
c              ..map observation volume locations
c      
               call gks_color (rgb_obs_clr, MX_OBS_CLR)
               call map_volumes (n_lon, n_lat, mask, n_obs_vol,
     *                           obs_x1, obs_x2, obs_y1, obs_y2,
     *                           n_data, obs_xi, obs_yj)
               call title_plot (title, .018, 1, 1.)
               fno = fno + 1
               write (*, '(10x, ''frame'', i5, '': '', a)')
     *                fno, trim (title)
               write (title, '(a)') date
               call title_plot (title, .018, 1, 0.)
               call frame
c
c              ..clean up
c
               deallocate (obs_xi, obs_yj, obs_x1, obs_x2)
               deallocate (obs_y1, obs_y2)
            else
               write (*, '(22x, ''no data case'')')
            endif
         endif
      enddo
c
      return
      end
