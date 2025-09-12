      subroutine map_impact_ice (n_lon, n_lat, n_obs, n_files,
     *                           n_obs_file, obs_imp, obs_lat,
     *                           obs_lon, obs_sen, obs_typ,
     *                           obs_xi, obs_yj, title1, igrid,
     *                           rlat, stdlt1, stdlt2, stdlon,
     *                           bl, br, tl, tr, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_impact_ice
c
c DESCRIPTION:  routine to plot the ice observation data impact
c               distributionis in the analysis area
c      
c PARAMETERS:
c     Name         Type       Usage            Description
c   ---------    --------    -------    ----------------------------
c   n_obs        integer     input      number of observations
c   obs_lat      real        input      observation latitudes
c   obs_lon      real        input      observation longitudes
c   obs_typ      integer     input      observation data types
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
      integer    MX_AMAP
      parameter (MX_AMAP = 36 000 000)
c
      integer    MX_AI
      parameter (MX_AI = 20 000)
c
      integer    MX_CNT
      parameter (MX_CNT = 64 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 80 000)
c
      integer    N_TYP
      parameter (N_TYP = 1)
c
c     ..local array dimensions
c
      integer   n_files
      integer   n_lat
      integer   n_lon
      integer   n_obs
c
      integer   ben
      real      bl(2), br(2)
      real      del, dmn, dmx
      integer   fno
      integer   i, k, n
      integer   igrid
      integer   ix, jy
      character lbl * 20
      integer   len
      integer   lntr
      integer   mx_obs
      integer   n_ben (2)
      integer   n_obs_file (n_files)
      real      obs_imp (n_obs, n_files)
      real      obs_lat (n_obs, n_files)
      real      obs_lon (n_obs, n_files)
      real      obs_sen (n_obs, n_files)
      integer   obs_typ (n_obs, n_files)
      real      obs_xi (n_obs, n_files)
      real      obs_yj (n_obs, n_files)
      integer   plt_typ (N_TYP)
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      stdlon
      real      stdlt1, stdlt2
      character title1 * (*)
      character title2 * 256
      real      tl(2), tr(2)
      integer   typ
      real      u, v
c
c     ..allocatable data arrays
c
      integer,  allocatable :: clr (:)
      real,     allocatable :: imp (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      real,     allocatable :: sen (:)
      real,     allocatable :: xi (:)
      real,     allocatable :: yj (:)
c
      integer,  allocatable :: ice_clr (:)
      real,     allocatable :: ice_cnt (:)
      real,     allocatable :: ice_imp (:)
      real,     allocatable :: ice_lat (:)
      real,     allocatable :: ice_lon (:)
      real,     allocatable :: ice_sen (:)
c
c     ..allocatable plot arrays
c
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: iamap (:)
      integer,  allocatable :: lblndx (:)
      character,allocatable :: llbs (:) * 10
      real,     allocatable :: slc (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycs (:)
c
c     ..external NCAR functions
c
      external  clin, colram
c
c     ..set data types
c
      include 'coda_types.h'
c
c     ..set color tables
c
      include 'color_table.h'
c
c     ..define data type(s)
c
      data  plt_typ / 78 /
c
c     ..define plot position in frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
c...............................executable..............................
c
c     ..allocate plot arrays
c
      allocate (iai (MX_AI))
      allocate (iag (MX_AI))
      allocate (iamap (MX_AMAP))
      allocate (lblndx (MX_CNT))
      allocate (llbs (MX_CNT))
      allocate (slc (MX_CNT))
      allocate (xcs (MX_LIN))
      allocate (ycs (MX_LIN))
c
c     ..get max number of obs
c
      mx_obs = 0
      do k = 1, n_files
         mx_obs = mx_obs + n_obs_file(k)
      enddo
c
c     ..allocate data arrays
c
      allocate (clr (mx_obs))
      allocate (imp (mx_obs))
      allocate (lat (mx_obs))
      allocate (lon (mx_obs))
      allocate (sen (mx_obs))
      allocate (xi (mx_obs))
      allocate (yj (mx_obs))
c
      allocate (ice_clr (n_lon * n_lat))
      allocate (ice_cnt (n_lon * n_lat))
      allocate (ice_imp (n_lon * n_lat))
      allocate (ice_lat (n_lon * n_lat))
      allocate (ice_lon (n_lon * n_lat))
      allocate (ice_sen (n_lon * n_lat))
c
c     ..loop over data types
c
      do typ = 1, N_TYP 
c
c     ..loop over beneficial (non)
c
      do ben = 1, 2
c
c     ..extract ice data type
c
      n = 0
      do k = 1, n_files
      if (n_obs_file(k) .gt. 0) then
         do i = 1, n_obs_file(k)
            if (obs_typ(i,k) .eq. plt_typ(typ)) then
               n = n + 1
               imp(n) = obs_imp(i,k)
               lat(n) = obs_lat(i,k)
               lon(n) = obs_lon(i,k)
               sen(n) = obs_sen(i,k)
               xi(n) = obs_xi(i,k)
               yj(n) = obs_yj(i,k)
            endif
         enddo
      endif
      enddo
      if (n .eq. 0) cycle
c
c     ..initialize
c
      do i = 1, (n_lon * n_lat)
         ice_clr(i) = 0.
         ice_cnt(i) = 0.
         ice_imp(i) = 0.
         ice_lat(i) = 0.
         ice_lon(i) = 0.
         ice_sen(i) = 0.
      enddo
c
c     ..set color slicing
c
      dmn = -0.2
      dmx =  0.2
      del = (dmx - dmn) / real (n_slcm)
      lntr = 5
c
c     ..bin data on grid
c
      do i = 1, n
         ix = nint (xi(i))
         jy = nint (yj(i))
         k = n_lon * (jy-1) + ix
         ice_cnt(k) = ice_cnt(k) + 1.
         ice_imp(k) = ice_imp(k) + imp(i)
         ice_lat(k) = ice_lat(k) + lat(i)
         ice_lon(k) = ice_lon(k) + lon(i)
      enddo
c
c     ..bin average
c
      do k = 1, (n_lon * n_lat)
         if (ice_cnt(k) .gt. 0.) then
            ice_imp(k) = ice_imp(k) / ice_cnt(k)
            ice_lat(k) = ice_lat(k) / ice_cnt(k)
            ice_lon(k) = ice_lon(k) / ice_cnt(k)
            ice_sen(k) = ice_sen(k) / ice_cnt(k)
         endif 
      enddo
c
c     ..diagnostics
c
      if (typ .eq. 1) then
         lbl = 'Sea Ice Coverage    '
      else if (typ .eq. 2) then
         lbl = 'Sea Ice Thickness   '
      else if (typ .eq. 3) then
         lbl = 'Sea Ice Temperature ' 
      endif
c
c     ..set color table and map background
c
      call gks_color (rgb_adj_clr, MX_ADJ_CLR)
      call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon, bl, br, tl,
     *              tr, pos1, pos2, pos3, pos4, iamap, mx_amap)
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
c     ..color slice data impacts
c
      do i = 1, (n_lon * n_lat)
         ice_clr(i) = nint ((ice_imp(i) - dmn) / del) + 4
         if (ice_clr(i) .lt. 5) ice_clr(i) = 5
         if (ice_clr(i) .gt. MX_ADJ_CLR) ice_clr(i) = MX_ADJ_CLR
         if (ice_imp(i) .gt. 0.) then
            if (ice_clr(i) .le. 14) ice_clr(i) = 15
         endif
      enddo
c
c     ..mark color sliced data impacts
c
      call gsmk (2)
      call gsmksc (0.1)
      n_ben(ben) = 0
      if (ben .eq. 1) then
      do i = 1, (n_lon * n_lat)
         if (ice_cnt(i) .gt. 0.) then
         if (ice_imp(i) .le. 0.) then
            call maptra (ice_lat(i), ice_lon(i), u, v)
            call gspmci (ice_clr(i))
            call gpm (1, u, v)
            call plotit (0, 0, 0)
            n_ben(ben) = n_ben(ben) + 1
         endif
         endif
      enddo
      else if (ben .eq. 2) then
      do i = 1, (n_lon * n_lat)
         if (ice_cnt(i) .gt. 0.) then
         if (ice_imp(i) .gt. 0.) then
            call maptra (ice_lat(i), ice_lon(i), u, v)
            call gspmci (ice_clr(i))
            call gpm (1, u, v)
            call plotit (0, 0, 0)
            n_ben(ben) = n_ben(ben) + 1
         endif
         endif
      enddo
      endif
c
c     ..set label bar labels
c
      do i = 1, MX_CNT
         lblndx(i) = i + 4
         llbs(i) = '          '
      enddo
      do i = 1, (n_slcm+1), lntr
         del = dmn + real (i-1) * (dmx-dmn) / real (n_slcm)
         write (llbs(i), '(f4.1)') del
         llbs(i) = adjustl (llbs(i))
      enddo
      len = len_trim (llbs(1))
      llbs(1) = '<' // llbs(1)(1:len)
      len = len_trim (llbs(n_slcm+1))
      llbs(n_slcm+1) = llbs(n_slcm+1)(1:len) // '>'
c
c     ..plot label bars below map
c
      call lbseti ('CBL - color of box lines', 1)
      call lblbar (0, .1, .9, .0, .09, n_slcm, 1., 0.25,
     *             lblndx, 0, llbs, (n_slcm+1), 1)
c
c     ..put titles on plot
c
      call title_plot (title1, .022, 1, 2.5)
      if (ben .eq. 1) then
         write (title2, '(a, ''    Beneficial Data Impacts'')')
     *          trim (lbl)
      else if (ben .eq. 2) then
         write (title2, '(a, ''    Non-Beneficial Data Impacts'')')
     *          trim (lbl)
      endif
      call title_plot (title2, .022, 1, 0.8)
c
c     ..advance plot frame
c
      fno = fno + 1
      if (ben .eq. 1) then
         write (*, '(10x, ''frame'', i5, '': Sea Ice Coverage '',
     *          ''Beneficial'')') fno
      else
         write (*, '(10x, ''frame'', i5, '': Sea Ice Coverage '',
     *          ''Non-Beneficial'')') fno
      endif
      call frame
      enddo
      enddo
c
c     ..clean up
c
      deallocate (clr, imp, lat, lon, sen, xi, yj)
      deallocate (iai, iag, iamap, lblndx, llbs, slc, xcs, ycs)
      deallocate (ice_cnt, ice_clr, ice_imp, ice_lat, ice_lon)
      deallocate (ice_sen)
c
      return
      end
