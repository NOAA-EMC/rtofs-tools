      subroutine plot_slatrk (dir_path, dtg, date, nest, n_lon,
     *                        n_lat, zoom, bl, tr, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_slatrk
c
c DESCRIPTION:  plots altimeter and model along track SLA corrections
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
      integer    N_SAT
      parameter (N_SAT = 8)
c
      integer    MX_PTS
      parameter (MX_PTS = 5 000)
c
      integer    UNIT
      parameter (UNIT = 21)
c
c     ..local array dimensions
c
      integer   n
c
      real      avg
      real      bl (2)
      integer   color (3)
      character date * 15
      real      del
      character dir_path * (*)
      real      dlat
      character dtg * 10
      logical   exist
      character file_name * 256
      integer   fno
      integer   i, j, k, k1, m
      integer   io_err
      character label (3) * 5
      real      lat1, lat2
      real      lb (2)
      character lbl1*1, lbl2*2, lbl3*3
      integer   len, len_dir
      real      lon1, lon2
      integer   n_lat
      integer   n_lbl
      integer   n_lon
      integer   n_pts
      integer   nest
      integer   ns
      real      off
      real      rt (2)
      integer   sat
      character sat_lbl (N_SAT) * 3
      integer   sat_typ (N_SAT)
      real      siz
      integer   skp, skp_hlf
      integer   smn, smx
      real      stdlon
      real      tck
      character title * 132
      character tmp_name * 80
      real      tr (2)
      integer   trk
      real      x, y
      real      xmn, xmx, ymn, ymx
      real      xpt, ypt
      real      xvpl, xvpr, yvpb, yvpt
      real      yel, ysl
      logical   zoom
c****************
      logical ok
c***************
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
      real,     allocatable :: bkg (:)
      real,     allocatable :: inv (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      integer,  allocatable :: ndx (:)
      integer,  allocatable :: smp (:)
      real,     allocatable :: ssh (:)
c
c     ..external NCAR functions
c
      external  clin, cmap
c
c     ..set color tables and data labels
c
      include 'color_table.h'
      include 'coda_types.h'
c
c     ..set satelite data types and file labels
c
      data sat_lbl / 's6a', 's6b', 'js3', 'cy2', 'alt',
     *               's3a', 's3b', 'swt' /
      data sat_typ /   63,    64,   193,   167,   179,
     *                197,   199,   125  /
c
c...............................executable..............................
c
c     ..allocate ncar arrays
c
      allocate (iai (MX_AI))
      allocate (iag (MX_AI))
      allocate (iamap (MX_AMAP))
      allocate (xcs (MX_LIN))
      allocate (ycs (MX_LIN))
c
c     ..allocate track arrays
c
      allocate (bkg (MX_PTS))
      allocate (inv (MX_PTS))
      allocate (lat (MX_PTS))
      allocate (lon (MX_PTS))
      allocate (ndx (MX_PTS))
      allocate (smp (MX_PTS))
      allocate (ssh (MX_PTS))
c
c     ..set zoom parameters
c
      if (zoom) then
         lb(1) = bl(1)
         lb(2) = bl(2)
         if (lb(2) .lt. 0.) lb(2) = lb(2) + 360.
         rt(1) = tr(1)
         rt(2) = tr(2)
         if (rt(2) .lt. 0.) rt(2) = rt(2) + 360.
      else
         lb(1) = -90.
         lb(2) =   0.
         rt(1) =  90.
         rt(2) = 360.
      endif
c
c     ..close gks
c
      call clsgks
c
      if (zoom) then
c
c        ..set pooled satellite file name, open gks
c
         write (tmp_name, '(''ssh_trk.'', a, ''.gmeta '')') dtg
         fno = 0
         call init_gks ('opn', tmp_name)
         call gks_color (rgb_obs_clr, MX_OBS_CLR)
      endif
c
c     ..loop over satellites
c
      do ns = 1, N_SAT
c
c     ..initialize sample array
c
      smp = 0
c
      if (.not. zoom) then
c
c        ..set satellite file name, open gks
c
         write (tmp_name, '(a, ''_trk.'', a, ''.gmeta '')') 
     *          trim (sat_lbl(ns)), dtg
         fno = 0
         call init_gks ('opn', tmp_name)
         call gks_color (rgb_obs_clr, MX_OBS_CLR)
      endif
c
c     ..open track file
c
      call cr_fname (dir_path, dtg, nest, n_lon, n_lat, 'datafld',
     *               'slatrk', 'o', 'sfc', 0, file_name, len)
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
      do
      read (UNIT, *, iostat=io_err) sat, trk, n, avg
      if (io_err .gt. 0) then
         write (*, '(''track file ioerr :'', i7)') io_err
         exit
      else if (io_err .lt. 0) then
         exit
      else
         read (UNIT, *) lat1, lat2, lon1, lon2
c
c        ..read altimeter track data
c          bkg: model forecast SSH 
c          ssh: ADT SSH
c          inv: ADT SSH minus model SSH (corrected)
c
c        ..convert to uncorrected SSH
c          bkg(i) = bkg(i) - avg
c          inv(i) = inv(i) + avg
c
         do i = 1, n
            read (UNIT, *) lat(i), lon(i), smp(i), bkg(i),
     *                     ssh(i), inv(i)
         enddo
c
c        ..select along track samples
c
         if (zoom) then
            n_pts = 0
            do i = 1, n
               if (lat(i) .ge. lb(1) .and. lat(i) .le. rt(1)) then
               if (lon(i) .gt. lb(2) .and. lon(i) .le. rt(2)) then
                  n_pts = n_pts + 1
                  bkg(n_pts) = bkg(i)
                  inv(n_pts) = inv(i)
                  lat(n_pts) = lat(i)
                  lon(n_pts) = lon(i)
                  smp(n_pts) = smp(i)
                  ssh(n_pts) = ssh(i)
               endif
               endif
            enddo
         else
            n_pts = n
         endif
         if (n_pts .le. 5) then
            cycle
         endif
c
c        ..check satellite number
c
c      write (*,'(''sat, sat_typ: '',2i10)') sat,sat_typ(ns)
         if (sat .eq. sat_typ(ns)) then
c      write (*,'(''plotting'')')
c
c        ..sort by sample number
c
         call sort_vctr (n_pts, smp, ndx)
c
c        ..set symbol size and color
c
         call gsmksc (0.3)
         call gsmk (3)
c
c        ..set plot space
c
         xvpl = 0.11
         xvpr = 0.89
         yvpb = 0.07
         yvpt = 0.66
c
         smn = 999999999
         smx = 0
         do i = 1, n_pts
            if (smp(i) .lt. smn) smn = smp(i)
            if (smp(i) .gt. smx) smx = smp(i)
         enddo
c
         xmn = real (smn)
         xmx = real (smx)
         ymn = -150.
         ymx =  150.
         call set (xvpl, xvpr, yvpb, yvpt, xmn, xmx, ymn, ymx, 1)
         call setusv ('LW', 2000)
c
c        ..loop over variables
c
         do m = 1, 3
            if (m .eq. 1) then
               call gspmci (4)
               call gsplci (4)
            else if (m .eq. 2) then
               call gspmci (3)
               call gsplci (3)
            else if (m .eq. 3) then
               call gspmci (5)
               call gsplci (5)
            endif
c
c           ..mark obs positions
c
            do i = 2, (n_pts-1)
               k = ndx(i)
               k1 = ndx(i-1)
               x = real (smp(k))
               if (m .eq. 1) then
                  y = ssh(k) * 100.
               else if (m . eq. 2) then
                  y = bkg(k) * 100.
               else if (m .eq. 3) then
                  y = inv(k) * 100.
               endif
               if (i .gt. 2) then
                  if (abs ((smp(k) - smp(k1))) .gt. 1) then
                     call frstpt (x, y)
                  endif
               else
                  call frstpt (x, y)
               endif
               call vector (x, y)
               call gpm (1, x, y)
            enddo
            call plotit (0, 0, 0)
         enddo
c
c        ..set labels
c
         n_lbl = 3
         color(3) = 4
         label(3) = 'Altim'
         color(2) = 3
         label(2) = 'Model'
         color(1) = 5
         label(1) = 'Innov'
c
c        ..draw zero line
c
         call gsplci (1)
         call gspmci (1)
         call gstxci (1)
         call gsfaci (1)
         call line (xmn, 0., xmx, 0.)
         call plotit (0, 0, 0)
c
c        ..draw line around section chart
c
         call line (xmn, ymn, xmx, ymn)
         call line (xmn, ymn, xmn, ymx)
         call line (xmn, ymx, xmx, ymx)
         call line (xmx, ymn, xmx, ymx)
         call plotit (0, 0, 0)
c
c        ..label ssh axis
c
         siz = .017
         del = 50.
         tck = 10.
         off = 0.10
         call lbl_axis (ymn, ymx, del, tck, 'SSH (cm)', 90., siz,
     *                  off, .false.)
c
c        ..label latitude axis (tick marks and title)
c
         siz = .014
         dlat = abs (lat(ndx(n_pts)) - lat(ndx(1))) 
         if (dlat .gt. 90.) then
            skp = 20
            skp_hlf = 10 
         else if (dlat .gt. 45.) then
            skp = 10
            skp_hlf = 5
         else if (dlat .gt. 22.5) then
            skp = 5
            skp_hlf = 5
         else if (dlat .gt. 11.25) then
            skp = 2
            skp_hlf = 2
         else
            skp = 1
            skp_hlf = 1
         endif
         do j = -90, 90, skp
            do i = 1, n_pts
               k = ndx(i)
               if (int (lat(k)) .eq. j) then
                  xpt = real (smp(k))
                  ypt = ymn - (ymx - ymn) * .04
                  if (j .le. -10) then
                     write (lbl3, '(i3)') j
                     call plchhq (xpt, ypt, lbl3, siz, 0., 0.)
                  else if (j .lt. 0) then
                    write (lbl2, '(i2)') j
                     call plchhq (xpt, ypt, lbl2, siz, 0., 0.)
                  else if (j .ge. 10) then
                     write (lbl2, '(i2)') j
                     call plchhq (xpt, ypt, lbl2, siz, 0., 0.)
                  else
                     write (lbl1, '(i1)') j
                     call plchhq (xpt, ypt, lbl1, siz, 0., 0.)
                  endif
                  exit
                endif
             enddo
         enddo
c
         do j = -90, 90, skp_hlf
            do i = 1, n_pts
               k = ndx(i)
               if (int (lat(k)) .eq. j) then
                  xpt = real (smp(k))
                  ysl = ymn
                  yel = ymn - (ymx - ymn) * .015
                  call line (xpt, ysl, xpt, yel)
                  exit
                endif
             enddo
         enddo
c
         xpt = (xmn + xmx) * 0.5
         ypt = ymn - (ymx - ymn) * .085
         call plchhq (xpt, ypt, 'Latitude', siz, 0., 0.)
c
c        ..put label bar on plot
c
         call gsplci (1)
         call gsfais (1)
         call lbseti ('CBL - color boxlines', 1)
         call lbseti ('CLB - color labels', 1)
         call lblbar (1, .90, 1.0, .275, .455, n_lbl, 0.2, 1.,
     *                color, 0, label, n_lbl, 1)
c
c        ..plot titles
c
         call set (.42, .89, .70, .98, .42, .89, .70, .98, 1)
         siz = .018
         x = (.42 + .89) * 0.5       
         write (title, '(a, 4x, ''Track '', i4)')
     *          trim (adjustl (data_lbl(sat))), trk
         len = len_trim (title)
         call plchhq (x, 0.93, title(1:len), siz, 0., 0.)
         write (title, '(''ADT SSH Innovations'')')
         len = len_trim (title)
         call plchhq (x, .86, title(1:len), siz, 0., 0.)
         write (title, '(a)') date
         len = len_trim (title)
         call plchhq (x, .79, title(1:len), siz, 0., 0.)
c
c----------------------------------------------------------
c
c        ..set map background
c
         lat1 = max (lb(1), -80.)
         lat2 = min (rt(1),  80.)
         lon1 = lb(2)
         lon2 = rt(2)
         stdlon = (lon1 + lon2) * 0.5
         call arinam (iamap, mx_amap)
         call mapsti ('LA', 0)
         call mapsti ('EL', 0)
         call mappos (.02, .38, .72, .95)
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
         if (zoom) then
            call mapsti ('GR', 10)
         else
            call mapsti ('GR', 30)
         endif
         call gsplci (1)
         call gstxci (1)
         call gsln (2)
         call mapgrm (iamap, xcs, ycs, MX_LIN, iai, iag,
     *                MX_AI, clin)
         call gsln (1)
c
c        ..overlay altimeter track
c
         call gsplci (4)
         call gspmci (4)
         call gsmksc (0.2)
         call gsmk (3)
         do i = 1, n_pts
            call maptrn (lat(i), lon(i), x, y)
            call gpm (1, x, y)
         enddo
         call plotit (0, 0, 0)
c
c        ..advance the plot buffer
c
         fno = fno + 1
         write (title, '(a, 2x, ''Trk '', i4, 2x, a)')
     *          trim (adjustl (data_lbl(sat))), trk, date
         write (*, '(10x, ''frame'', i5, '': '', a, i10, f10.2)')
     *          fno, trim (title), n, (avg * 100.)
         call frame
c
         endif
      endif         
c
c     ..return for new track
c
      enddo
c
c     ..close file
c
      close (UNIT)
      if (.not. zoom) then
         call init_gks ('cls', tmp_name)
      endif
c
c     ..return for new satellite
c
      enddo
c
c     ..close gks
c
      if (zoom) then
         call init_gks ('cls', tmp_name)
      endif
c
c     ..reset gks
c
      call opngks
c
c     ..clean up
c
      deallocate (iai, iag, iamap, xcs, ycs)
      deallocate (bkg, inv, lat, lon, ndx, smp, ssh)
c
      return
      end
      subroutine sort_vctr (n, vctr, ndx)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  sort_vctr
c
c DESCRIPTION:  sort a vector into ascending order by straight
c               insertion.  outpts a list of the corresponding
c               rearrangement.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c     Name      Type       Usage             Description
c   --------   -------    -------    ---------------------------------
c   vctr       integer    input      values to be sorted
c   ndx        integer    output     sorted order of input values
c   n          integer    input      number items to sort
c
c....................MAINTENANCE SECTION................................
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..local array dimension
c
      integer   n
c
      integer   a
      integer   ndx (n)
      integer   i, j, k
      integer   vctr (n)
      integer   wrk (n)
c
c...............................executable..............................
c
c     ..set vector indices, make working copy of input vector
c
      do i = 1, n
         ndx(i) = i
         wrk(i) = vctr(i)
      enddo
c
c     ..pick out each element in turn
c
      do j = 2, n
         a = wrk(j)
         k = ndx(j)
         do i = (j-1), 1, -1
            if (wrk(i) .le. a) go to 10
            wrk(i+1) = wrk(i)
            ndx(i+1) = ndx(i)
         enddo
         i = 0
   10    continue
c
c        ..insert sorted element 
c
         wrk(i+1) = a
         ndx(i+1) = k
      enddo
c
      return
      end
