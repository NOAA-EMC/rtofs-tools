      subroutine map_impact_prf (n_lon, n_lat, n_obs, n_files,
     *                           n_obs_file, obs_imp, obs_lat,
     *                           obs_lon, obs_sen, obs_typ,
     *                           obs_var, obs_xi, obs_yj,
     *                           title1, igrid, rlat, stdlt1,
     *                           stdlt2, stdlon, bl, br, tl,
     *                           tr, typ, var, typ_lbl,
     *                           var_lbl, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_impact_prf
c
c DESCRIPTION:  routine to plot the observation data impact distribution
c               in the analysis area
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
      integer   mx_clr
      integer   mx_obs
      integer   n_ben (2)
      integer   n_data (n_files)
      integer   n_obs_file (n_files)
      integer   nslc
      real      obs_imp (n_obs, n_files)
      real      obs_lat (n_obs, n_files)
      real      obs_lon (n_obs, n_files)
      real      obs_sen (n_obs, n_files)
      integer   obs_typ (n_obs, n_files)
      integer   obs_var (n_obs, n_files)
      real      obs_xi (n_obs, n_files)
      real      obs_yj (n_obs, n_files)
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      siz
      real      stdlon
      real      stdlt1, stdlt2
      character title1 * (*)
      character title2 * 256
      real      tl(2), tr(2)
      integer   typ
      character typ_lbl * 7
      real      u, v
      integer   var
      character var_lbl * 4
c
c     ..allocatable data arrays
c
      real,     allocatable :: imp (:,:)
      real,     allocatable :: lat (:,:)
      real,     allocatable :: lon (:,:)
      real,     allocatable :: sen (:,:)
      real,     allocatable :: xi (:,:)
      real,     allocatable :: yj (:,:)
      real,     allocatable :: prf_cnt (:)
      integer,  allocatable :: prf_clr (:)
      real,     allocatable :: prf_imp (:)
      real,     allocatable :: prf_lat (:)
      real,     allocatable :: prf_lon (:)
      real,     allocatable :: prf_sen (:)
      real,     allocatable :: sum_imp (:)
      real,     allocatable :: sum_sen (:)
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
      allocate (imp (n_obs, n_files))
      allocate (lat (n_obs, n_files))
      allocate (lon (n_obs, n_files))
      allocate (sen (n_obs, n_files))
      allocate (xi (n_obs, n_files))
      allocate (yj (n_obs, n_files))
c
      allocate (prf_clr (n_lon * n_lat))
      allocate (prf_cnt (n_lon * n_lat))
      allocate (prf_imp (n_lon * n_lat))
      allocate (prf_lat (n_lon * n_lat))
      allocate (prf_lon (n_lon * n_lat))
      allocate (prf_sen (n_lon * n_lat))
      allocate (sum_imp (n_lon * n_lat))
      allocate (sum_sen (n_lon * n_lat))
c
c     ..extract variable and data types
c
      do k = 1, n_files
      n_data(k) = 0
      if (n_obs_file(k) .gt. 0) then
         n = 0
         do i = 1, n_obs_file(k)
            if (obs_typ(i,k) .eq. typ .and. 
     *          obs_var(i,k) .eq. var) then
               n = n + 1
               imp(n,k) = obs_imp(i,k)
               lat(n,k) = obs_lat(i,k)
               lon(n,k) = obs_lon(i,k)
               sen(n,k) = obs_sen(i,k)
               xi(n,k) = obs_xi(i,k)
               yj(n,k) = obs_yj(i,k)
            endif
         enddo
         n_data(k) = n
      endif
      enddo
c
c     ..diagnostics
c
      if (typ .gt. 0) then
         lbl = data_lbl(typ)
         lbl = trim (adjustl (lbl))
      else
         lbl = 'Undefined'
      endif
c
c     ..loop over beneficial and non-beneficial 
c
      do ben = 1, 2
c
c     ..set adjoint impact color table and map background
c
      call gks_color (rgb_adj_clr, MX_ADJ_CLR)
      mx_clr = MX_ADJ_CLR
      nslc = n_slcm
      call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon, bl, br, tl,
     *              tr, pos1, pos2, pos3, pos4, iamap, mx_amap)
c
c     ..color land areas brown 
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
c     ..set color slicing 
c
      dmn = -0.2
      dmx =  0.2
      lntr = 5
      del = (dmx - dmn) / real (nslc)
c
c     ..loop over files
c
      do k = 1, n_files
c
c        ..initialize
c
         do i = 1, (n_lon * n_lat)
            prf_clr(i) = 0.
            prf_cnt(i) = 0.
            prf_imp(i) = 0.
            prf_lat(i) = 0.
            prf_lon(i) = 0.
            prf_sen(i) = 0.
            sum_imp(i) = 0.
            sum_sen(i) = 0.
         enddo
c
c        ..bin data on grid
c
         if (n_data(k) .gt. 0) then
            do i = 1, n_data(k)
               ix = nint (xi(i,k))
               jy = nint (yj(i,k))
               n = n_lon * (jy-1) + ix
               prf_cnt(n) = prf_cnt(n) + 1.
               sum_imp(n) = sum_imp(n) + imp(i,k)
               prf_lat(n) = prf_lat(n) + lat(i,k)
               prf_lon(n) = prf_lon(n) + lon(i,k)
               sum_sen(n) = sum_sen(n) + sen(i,k)
            enddo
         endif
c
c        ..bin average locations and impacts
c  
         do n = 1, (n_lon * n_lat)
            if (prf_cnt(n) .gt. 0.) then
               prf_imp(n) = sum_imp(n) / prf_cnt(n) * 10.
               prf_lat(n) = prf_lat(n) / prf_cnt(n)
               prf_lon(n) = prf_lon(n) / prf_cnt(n)
               prf_sen(n) = sum_sen(n) / prf_cnt(n)
            endif 
         enddo
c
c        ..color slice data impacts
c
         do n = 1, (n_lon * n_lat)
            if (prf_cnt(n) .gt. 0.) then
               prf_clr(n) = nint ((prf_imp(n) - dmn) / del) + 4
               if (prf_clr(n) .lt. 5) prf_clr(n) = 5
               if (prf_clr(n) .gt. mx_clr) prf_clr(n) = mx_clr
               if (prf_imp(n) .gt. 0.) then
                  if (prf_clr(n) .le. 14) prf_clr(n) = 15
               endif
            endif
         enddo
c
c        ..mark color sliced impacts
c
         call pcseti ('FN - fontcap number', 20)
         if (typ .eq. 19 .or. typ .eq. 49) then
            siz = 0.005
         else if (typ .eq. 79 .or. typ .eq. 173) then
            siz = 0.0025
         else
            siz = 0.01
         endif
c
c        ..beneficial/non-beneficial
c
         n_ben(ben) = 0
         if (ben .eq. 1) then
            do n = 1, (n_lon * n_lat)
               if (prf_cnt(n) .gt. 0.) then
c              if (prf_imp(n) .le. 0.) then
                  call maptra (prf_lat(n), prf_lon(n), u, v)
                  call gsplci (prf_clr(n))
                  call plchhq (u, v, 'L', siz, 0., 0.)
                  call plotit (0, 0, 0)
                  n_ben(ben) = n_ben(ben) + 1
c              endif
               endif
            enddo
         else if (ben .eq. 2) then
            do n = 1, (n_lon * n_lat)
               if (prf_cnt(n) .gt. 0.) then
               if (prf_imp(n) .gt. 0.) then
                  call maptra (prf_lat(n), prf_lon(n), u, v)
                  call gsplci (prf_clr(n))
                  call plchhq (u, v, 'L', siz, 0., 0.)
                  call plotit (0, 0, 0)
                  n_ben(ben) = n_ben(ben) + 1
               endif
               endif
            enddo
         endif
         call pcseti ('FN - fontcap number', 0)
      enddo
c
c     ..set label bar labels
c
      do i = 1, MX_CNT
         lblndx(i) = i + 4
         llbs(i) = '          '
      enddo
      do i = 1, (nslc+1), lntr
         del = dmn + real (i-1) * (dmx-dmn) / real (nslc)
         write (llbs(i), '(f4.1)') del
         llbs(i) = adjustl (llbs(i))
      enddo
      len = len_trim (llbs(1))
      llbs(1) = '<' // llbs(1)(1:len)
      len = len_trim (llbs(nslc+1))
      llbs(nslc+1) = llbs(nslc+1)(1:len) // '>'
c
c     ..plot label bars below map
c
      call lbseti ('CBL - color of box lines', 1)
      call lblbar (0, .1, .9, .0, .09, nslc, 1., 0.25,
     *             lblndx, 0, llbs, (nslc+1), 1)
c
c     ..put titles on plot
c
      call title_plot (title1, .022, 1, 2.2)
      if (ben .eq. 1) then
         write (title2, '(a, '' Beneficial Data Impacts'')') lbl
      else
         write (title2, '(a, '' Non-Beneficial Data Impacts'')') lbl
      endif
      call title_plot (title2, .022, 1, 0.8)
c
c     ..advance plot frame
c
      fno = fno + 1
      if (ben .eq. 1) then
         write (*, '(10x, ''frame'', i5, '': '', a, 2x, a,
     *          ''  Beneficial'')') fno, typ_lbl, var_lbl
      else
         write (*, '(10x, ''frame'', i5, '': '', a, 2x, a,
     *          ''  Non-Beneficial'')') fno, typ_lbl, var_lbl
      endif
      call frame
c
      enddo
c
c     ..clean up
c
      deallocate (imp, lat, lon, sen, xi, yj)
      deallocate (iai, iag, iamap, lblndx, llbs, slc, xcs, ycs)
      deallocate (prf_clr, prf_cnt, prf_imp, prf_lat, prf_lon)
      deallocate (prf_sen, sum_imp, sum_sen)
c
      return
      end
