      subroutine plot_chobs (dir_path, dtg, date, nest, n_lon, n_lat,
     *                       subset, bl, tr, ssh_del, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_chobs
c
c DESCRIPTION:  plots Cooper-Haines synthetic profiles
c
c PARAMETERS:
c       Name          Type       Usage            Description
c   -------------   ----------   -----   -----------------------------
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..set NCAR map array dimensions
c
      integer    MX_AMAP
      parameter (MX_AMAP = 64 000 000)
c
      integer    MX_AI
      parameter (MX_AI = 1 280 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 1 280 000)
c
      integer    UNIT
      parameter (UNIT = 20)
c
c     ..local array dimensions
c
      integer   nc
      integer   nd
c
      real      bl (2)
      integer   color (3)
      character date * 15
      real      del
      character dir_path * (*)
      character dtg * 10
      logical   exist
      character file_name * 256
      integer   fno
      integer   i, j, k
      integer   io_err
      character label (3) * 5
      real      lat, lon
      real      lat1, lat2, lon1, lon2
      real      lb (2)
      integer   len, len_dir
      real      mld
      integer   n_lat
      integer   n_lon
      integer   nest
      real      off
      real      rt (2)
      real      siz
      real      spmis
      real      ssh_anm
      real      ssh_btm
      real      ssh_del
      logical   subset
      real      stdlon
      real      tck
      character title * 132
      real      tr (2)
      real      wt
      real      x, y
      real      xmn, xmx, ymn, ymx
      real      xvpl, xvpr, yvpb, yvpt
      real      zmx
c
c     ..ncar arrays
c
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycs (:)
c
c     ..observation arrays
c
      real,     allocatable :: lvl (:)
      real,     allocatable :: sal_clm (:)
      real,     allocatable :: sal_fcst (:)
      real,     allocatable :: sal_syn (:)
      real,     allocatable :: tmp_clm (:)
      real,     allocatable :: tmp_fcst (:)
      real,     allocatable :: tmp_syn (:)
c
c     ..external NCAR functions
c
      external  clin, cmap
c
c     ..set color tables
c
      include 'color_table.h'
c
c...............................executable..............................
c
c     ..open CH profile file
c
      call cr_fname (dir_path, dtg, nest, n_lon, n_lat, 'datafld',
     *               'chprof', 'o', 'sfc', 0, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      len_dir = len_trim (dir_path) + 1
      if (.not. exist) then
         write (*, '(''     restart missing: '', a)')
     *          file_name(len_dir:len)
         return
      endif
      write (*, '(''       restart found: '', a)')
     *       file_name(len_dir:len)
      open (UNIT, file=file_name(1:len), status='old',
     *            form='formatted')
c
c     ..set zoom parameters
c
      if (subset) then
         lb(1) = bl(1)
         lb(2) = bl(2)
         rt(1) = tr(1)
         rt(2) = tr(2)
      else
         lb(1) =  -80.
         lb(2) = -180.
         rt(1) =   80.
         rt(2) =  180.
      endif
c
c     ..set color table and line size
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
      call setusv ('LW', 2000)
c
c     ..allocate ncar arrays
c
      allocate (iai (MX_AI))
      allocate (iag (MX_AI))
      allocate (iamap (MX_AMAP))
      allocate (xcs (MX_LIN))
      allocate (ycs (MX_LIN))
c
      do
      read (UNIT, *, iostat=io_err) nd, nc, lat, lon, ssh_anm,
     *                              ssh_btm, mld, wt
      if (io_err .gt. 0) then
         write (*, '(''ch file ioerr :'', i7)') io_err
         exit
      else if (io_err .lt. 0) then
         exit
      else
c
c        ..allocate arrays
c
         allocate (lvl (nd))
         allocate (sal_clm (nd))
         allocate (sal_fcst (nd))
         allocate (sal_syn (nd))
         allocate (tmp_clm (nd))
         allocate (tmp_fcst (nd))
         allocate (tmp_syn (nd))
         spmis = -990.
c
c        ..read profile
c
         do i = 1, nd
            read (UNIT, *) lvl(i), tmp_fcst(i), tmp_syn(i),
     *                     tmp_clm(i), sal_fcst(i), sal_syn(i),
     *                     sal_clm(i)
         enddo
c
c        ..check for obs on grid and ssh threshold
c
         if (lat .ge. lb(1) .and. lat .le. rt(1)) then
         if (lon .ge. lb(2) .and. lon .le. rt(2)) then
         if (abs (ssh_anm) .gt. ssh_del) then
         if (lon .gt. 180.) lon = lon - 360.
         zmx = 1200.
c
c        ..loop over temperature and salinity plots
c
         do j = 1, 2
c
c        ..set plot space
c
         if (j .eq. 1) then
            xvpl = 0.11
            xvpr = 0.48
            yvpb = 0.15
            yvpt = 0.75
            xmn =  9999.
            xmx = -9999.
            do i = 1, nd
               if (tmp_clm(i)  .lt. xmn) xmn = tmp_clm(i)
               if (tmp_clm(i)  .gt. xmx) xmx = tmp_clm(i)
               if (tmp_fcst(i) .lt. xmn) xmn = tmp_fcst(i)
               if (tmp_fcst(i) .gt. xmx) xmx = tmp_fcst(i)
               if (tmp_syn(i)  .lt. xmn) xmn = tmp_syn(i)
               if (tmp_syn(i)  .gt. xmx) xmx = tmp_syn(i)
            enddo
            xmn = real (int (xmn)) - 0.5
            xmx = real (int (xmx)) + 1.0
         else if (j .eq. 2) then
            xvpl = 0.61
            xvpr = 0.98
            yvpb = 0.15
            yvpt = 0.75
            xmn =  9999.
            xmx = -9999.
            do i = 1, nd
               if (sal_clm(i)  .lt. xmn) xmn = sal_clm(i)
               if (sal_clm(i)  .gt. xmx) xmx = sal_clm(i)
               if (sal_fcst(i) .lt. xmn) xmn = sal_fcst(i)
               if (sal_fcst(i) .gt. xmx) xmx = sal_fcst(i)
               if (sal_syn(i)  .lt. xmn) xmn = sal_syn(i)
               if (sal_syn(i)  .gt. xmx) xmx = sal_syn(i)
            enddo
            xmn = real (int (xmn)) - 0.5
            xmx = real (int (xmx)) + 1.0
         endif
c
c        ..set background colors
c
         call gsplci (1)
         call gspmci (1)
         call gstxci (1)
         call gsfaci (1)
c
c        ..set plot window
c
         ymn = min (lvl(nd), zmx)
c        ymn = lvl(nd)
         ymx = 0.
         call set (xvpl, xvpr, yvpb, yvpt, xmn, xmx, ymn, ymx, 1)
c
c        ..draw line around chart
c
         call line (xmn, ymn, xmx, ymn)
         call line (xmn, ymn, xmn, ymx)
         call line (xmn, ymx, xmx, ymx)
         call line (xmx, ymn, xmx, ymx)
         call plotit (0, 0, 0)
c
c        ..label axes
c
         siz = .035
         if (ymn .le. 300.) then
            del = 50.
         else if (ymn .le. 1000.) then
            del = 100.
         else if (ymn .le. 2000.) then
            del = 200.
         else
            del = 500.
         endif
         tck = del
         off = 0.10
         call lbl_axis (0., ymn, del, tck, 'Depth (m)', 90.,
     *                  siz, off, .true.)
c
         if (j .eq. 1) then
            del = 5.
            tck = 1.
            off = 0.05
            call lbl_axis (xmn, xmx, del, tck, 'Temperature (C)',
     *                     0., siz, off, .false.)
         else if (j .eq. 2) then
            del = 1.
            tck = 0.5
            off = 0.05
            call lbl_axis (xmn, xmx, del, tck, 'Salinity (PSU)',
     *                     0., siz, off, .false.)
         endif
c
c        ..plot profiles
c
         call setusv ('LW', 3000)
         if (j .eq. 1) then
c
            call gsplci (5)
            call frstpt (tmp_clm(1), lvl(1))
            do k = 1, nc
            if (lvl(k) .le. ymn) then
               if (tmp_clm(k) .gt. -99.) then
                  call vector (tmp_clm(k), lvl(k))
               else
                  call frstpt (tmp_clm(k), lvl(k))
               endif
            endif
            enddo
            call plotit (0, 0, 0)
c
            call gsplci (4)
            call frstpt (tmp_fcst(1), lvl(1))
            do k = 1, nd
            if (lvl(k) .le. ymn) then
               if (tmp_fcst(k) .gt. spmis) then
                  call vector (tmp_fcst(k), lvl(k))
               else
                  call frstpt (tmp_fcst(k), lvl(k))
               endif
            endif
            enddo
            call plotit (0, 0, 0)
c
            call gsplci (3)
            call frstpt (tmp_syn(1), lvl(1))
            do k = 1, nc
            if (lvl(k) .le. ymn) then
               if (tmp_syn(k) .gt. -99.) then
                  call vector (tmp_syn(k), lvl(k))
               else
                  call frstpt (tmp_syn(k), lvl(k))
               endif
            endif
            enddo
            call plotit (0, 0, 0)
         else if (j .eq. 2) then
c
            call gsplci (5)
            call frstpt (sal_clm(1), lvl(1))
            do k = 1, nc
            if (lvl(k) .le. ymn) then
               if (sal_clm(k) .gt. -99.) then
                  call vector (sal_clm(k), lvl(k))
               else
                  call frstpt (sal_clm(k), lvl(k))
               endif
            endif
            enddo
            call plotit (0, 0, 0)
c
            call gsplci (4)
            call frstpt (sal_fcst(1), lvl(1))
            do k = 1, nd
            if (lvl(k) .le. ymn) then
               if (sal_fcst(k) .gt. spmis) then
                  call vector (sal_fcst(k), lvl(k))
               else
                  call frstpt (sal_fcst(k), lvl(k))
               endif
            endif
            enddo
            call plotit (0, 0, 0)
c
            call gsplci (3)
            call frstpt (sal_syn(1), lvl(1))
            do k = 1, nc
            if (lvl(k) .le. ymn) then
               if (sal_syn(k) .gt. -99.) then
                  call vector (sal_syn(k), lvl(k))
               else
                  call frstpt (sal_syn(k), lvl(k))
               endif
            endif
            enddo
            call plotit (0, 0, 0)
         endif
         call setusv ('LW', 2000)
         enddo
c
c        ..plot title and label bar on plot
c
         call set (.42, .98, .15, .75, .42, .98, .15, .75, 1)
         siz = .016
         x = (.28 + .98) * 0.5
         call gsplci (1)
         call gstxci (1)
         write (title, '(a, 5x, ''SSH Inv ='', f6.2)')
     *         date, (ssh_anm * 100.)
         len = len_trim (title)
         call plchhq (x, .92, title(1:len), siz, 0., 0.)
         siz = .016
         write (title, '(''Lat'', f6.1, 2x, ''Lon'', f7.1, 5x,
     *          ''Clm Wt = '', f4.2)') lat, lon, wt
         len = len_trim (title)
         call plchhq (x, .82, title(1:len), siz, 0., 0.)
c
         call gsplci (1)
         call gsfais (1)
         call lbseti ('CBL - color boxlines', 1)
         call lbseti ('CLB - color labels', 1)
         label(1) = 'Model'
         label(2) = 'Syn  '
         label(3) = 'Clim '
         color(1) = 4
         color(2) = 3
         color(3) = 5
         call lblbar (0, .43, .67, 0., .075, 3, 1., 0.25,
     *                color, 0, label, 3, 1)
         call plotit (0, 0, 0)
c
c----------------------------------------------------------
c
c        ..set map background
c
         lat1 = lat - 15.
         lat2 = lat + 15.
         lon1 = lon - 20.
         lon2 = lon + 20.
         stdlon = (lon1 + lon2) * 0.5
         call arinam (iamap, mx_amap)
         call mapsti ('LA', 0)
         call mapsti ('EL', 0)
         call mappos (.02, .30, .77, .98)
         call maproj ('ME', 0.0, stdlon, 0.0)
         call mapset ('CO', lat1, lon1, lat2, lon2)
         call mplnam ('Earth..3', 1, iamap)
         call gsfais (1)
         call gsfaci (2)
         call arscam (iamap, xcs, ycs, MX_LIN, iai, iag,
     *                MX_AI, cmap)
         call mplndr ('Earth..3', 1)
         call maplbl
c
c        ..mark obs location
c
         call pcseti ('FN - fontcap number', 20)
         call gsplci (4)
         call maptrn (lat, lon, x, y)
         call plchhq (x, y, 'L', .012, 0., 0.)
         call pcseti ('FN - fontcap number', 0)
         call plotit (0, 0, 0)
         call gsplci (1)
         call gsfaci (1)
c
c        ..advance the plot buffer
c
         fno = fno + 1
         write (title, '(''CH Prof  SSH anm = '', f6.2, 3f10.2)')
     *         (ssh_anm * 100.), lat, lon, wt
         write (*, '(10x, ''frame'', i6, '': '', a)')
     *          fno, trim (title)
         call frame
         endif
         endif
         endif
c
c        ..clean up
c
         deallocate (lvl)
         deallocate (sal_clm, sal_fcst, sal_syn)
         deallocate (tmp_clm, tmp_fcst, tmp_syn)
      endif
      enddo
c
c     ..clean up
c
      call setusv ('LW', 1000)
      deallocate (iai, iag, iamap, xcs, ycs)
c
c     ..close file
c
      close (UNIT)
c
      return
      end
