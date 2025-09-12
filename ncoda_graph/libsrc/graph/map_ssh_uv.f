      subroutine map_ssh_uv (dir_path, date, dtg, nest, n_lon, n_lat,
     *                       igrid, rlat, stdlt1, stdlt2, stdlon, bl,
     *                       br, tl, tr, iamap, mx_amap, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_ssh_uv
c
c DESCRIPTION:  routine to plot cross-track velocity vectors derived
c               from along-track altimeter measurements     
c      
c PARAMETERS:
c     Name         Type       Usage            Description
c   ---------    --------    -------    ----------------------------
c   n_obs        integer     input      number of observations
c   lat          real        input      observation latitudes
c   lon          real        input      observation longitudes
c   typ          integer     input      observation data types
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
      integer    MX_AI
      parameter (MX_AI = 20 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 80 000)
c
      integer    UNIT
      parameter (UNIT = 22)
c
c     ..local array dimensions
c
      integer   mx_amap
      integer   n_obs
c
      real      bl (2), br (2)
      real      c
      character date * 15
      character dir_path * (*)
      character dtg * 10
      logical   exist
      character file_name * 256
      integer   fno
      integer   i, k, n
      integer   iai (MX_AI)
      integer   iag (MX_AI)
      integer   iamap (mx_amap)
      integer   igrid
      integer   len, len_dir
      integer   n_lon, n_lat
      integer   n_seg
      integer   nest
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      stdlon
      real      stdlt1, stdlt2
      character title * 132
      real      tl (2), tr (2)
      real      u, v
      real      x, y
      real      xcs (MX_LIN)
      real      ycs (MX_LIN)
      integer   zero
c
c     ..dummy raw obs vector variable
c
      character obs_sgn * 7
c
c     ..external NCAR functions
c
      external  colram
c
c     ..allocatable arrays
c
      real,     allocatable :: obs_lat (:)
      real,     allocatable :: obs_lon (:)
      integer,  allocatable :: obs_seg (:)
      integer,  allocatable :: obs_smp (:)
      real,     allocatable :: obs_uuu (:)
      real,     allocatable :: obs_vvv (:)
c
c     ..set color tables
c
      include 'color_table.h'
c
c     ..define map position in plot frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
      data  zero / 0 /
c
c...............................executable..............................
c
c     ..check for and open innovation file
c
      call cr_fname (dir_path, dtg, nest, n_lon, n_lat, 'rawdata',
     *               'seahuv', 'o', 'sfc', zero, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) return
      len_dir = len_trim (dir_path) + 1
      write (*, '(''       restart found: '', a)')
     *       file_name(len_dir:len)
      open (UNIT, file=file_name(1:len), status='unknown',
     *            form='unformatted')
      read (UNIT) n_obs
c
c     ..allocate arrays
c
      allocate (obs_lat (n_obs))
      allocate (obs_lon (n_obs))
      allocate (obs_seg (n_obs))
      allocate (obs_smp (n_obs))
      allocate (obs_uuu (n_obs))
      allocate (obs_vvv (n_obs))
c
c     ..read raw obs data vectors
c
      read (UNIT) obs_lat(1:n_obs)
      read (UNIT) obs_lon(1:n_obs)
      read (UNIT) obs_uuu(1:n_obs)
      read (UNIT) obs_seg(1:n_obs)
      read (UNIT) obs_smp(1:n_obs)
      read (UNIT) obs_vvv(1:n_obs)
      read (UNIT) obs_sgn
      close (UNIT)
c
c     ..set map background
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
      call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon, bl, br, tl, tr,
     *              pos1, pos2, pos3, pos4, iamap, mx_amap)
c
c     ..color map land areas
c
      call gsfais (1)
      call gsfaci (2)
      call arscam (iamap, xcs, ycs, MX_LIN, iai, iag, MX_AI, colram)
c
c     ..draw continent outlines
c
      call mplndr ('Earth..3', 1)
      call maplbl
c
c     ..mark velocity sampling points
c
      call gsmk (2)
      call gsmksc (0.1)
      call gspmci (4)
      do i = 1, n_obs
         call maptra (obs_lat(i), obs_lon(i), u, v)
         call gpm (1, u, v)
      enddo
      call plotit (0, 0, 0)
c
c     ..set scalar for velocity overlay
c
      if ((tl(1) - bl(1)) .gt. 90.) then
         c = 4.5
      else if ((tl(1) - bl(1)) .gt. 60.) then
         c = 3.5
      else if ((tl(1) - bl(1)) .gt. 30.) then
         c = 2.5
      else if ((tl(1) - bl(1)) .gt. 10.) then
         c = 1.5
      else
         c = 1.
      endif 
c
c     ..set max number segments
c
      n_seg = maxval (obs_seg)
c
c     ..overlay velocities along segments
c
      call gsmk (2)
      call gsmksc (0.2)
      call gspmci (5)
      do k = 1, n_seg
         n = 0
         do i = 1, n_obs
            if (obs_seg(i) .eq. k) then
               x = obs_lon(i) + c * obs_uuu(i)
               y = obs_lat(i) + c * obs_vvv(i)
               call mapit (y, x, n)
               n = 1
            endif
         enddo
         call mapiq
      enddo
c
c     ..plot title
c
      write (title, '(''SSH Velocity Observations'', a)') date
      call title_plot (title, .018, 1, 1.8)
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)') fno, trim (title)
c
c     ..advance plot frame
c
      call frame
c
c     ..clean up
c
      deallocate (obs_lat, obs_lon, obs_seg, obs_smp, obs_uuu, obs_vvv)
c
      return
      end
