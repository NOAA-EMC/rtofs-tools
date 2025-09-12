      subroutine map_impact_sfc (n_sat, n_typ, sat_typ, sat_nam, 
     *                           n_lon, n_lat, n_obs, n_data,
     *                           ob_imp, ob_lat, ob_lon, ob_sen,
     *                           ob_typ, ob_xi, ob_yj, title1, 
     *                           igrid, rlat, stdlt1, stdlt2, 
     *                           stdlon, bl, br, tl, tr, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_impact_sfc
c
c DESCRIPTION:  routine to plot the ditribution of sfc data impact
c               by satellite in the analysis area
c      
c PARAMETERS:
c     Name         Type       Usage            Description
c   ---------    --------    -------    ----------------------------
c   n_obs        integer     input      number of observations
c   ob_lat       real        input      observation latitudes
c   ob_lon       real        input      observation longitudes
c   ob_typ       integer     input      observation data types
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
      integer   n_lat
      integer   n_lon
      integer   n_obs
      integer   n_sat
      integer   n_typ
c
      real      bl(2), br(2)
      real      del, dmn, dmx
      integer   fno
      integer   i, k, n
      integer   igrid
      integer   ix, jy
      integer   len
      integer   lntr
      integer   mx_obs
      integer   mx_nodes
      integer   n_data
      integer   nodes
      real      ob_imp (n_obs)
      real      ob_lat (n_obs)
      real      ob_lon (n_obs)
      real      ob_sen (n_obs)
      integer   ob_typ (n_obs)
      real      ob_xi (n_obs)
      real      ob_yj (n_obs)
      real      pos1, pos2, pos3, pos4
      real      rlat
      integer   sat
      character sat_nam (n_sat) * 7
      integer   sat_typ (n_typ, n_sat)
      real      spmis
      real      stdlon
      real      stdlt1, stdlt2
      character title1 * (*)
      character title2 * 80
      real      tl(2), tr(2)
      integer   typ
      real      u, v
c
c     ..allocatable data arrays
c
      real,     allocatable :: imp (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      real,     allocatable :: sen (:)
      real,     allocatable :: xi (:)
      real,     allocatable :: yj (:)
      real,     allocatable :: sfc_cnt (:)
      integer,  allocatable :: sfc_clr (:)
      real,     allocatable :: sfc_imp (:)
      real,     allocatable :: sfc_lat (:)
      real,     allocatable :: sfc_lon (:)
      real,     allocatable :: sfc_sen (:)
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
c     ..get max number of obs/nodes
c
      mx_obs = n_obs
      mx_nodes = n_lon * n_lat
      nodes = max (mx_nodes, mx_obs)
c*****************
      write (*,'(''obs,nodes,max: '',3i10)') mx_obs,mx_nodes,nodes
c*******************
c
c     ..set missing value indicator
c
      spmis = -990.
c
c     ..allocate data arrays
c
      allocate (imp (n_obs))
      allocate (lat (n_obs))
      allocate (lon (n_obs))
      allocate (sen (n_obs))
      allocate (xi  (n_obs))
      allocate (yj  (n_obs))
c
      allocate (sfc_clr (nodes))
      allocate (sfc_cnt (nodes))
      allocate (sfc_imp (nodes))
      allocate (sfc_lat (nodes))
      allocate (sfc_lon (nodes))
      allocate (sfc_sen (nodes))
c
c     ..set color table
c
      call gks_color (rgb_adj_clr, MX_ADJ_CLR)
c
c     ..diagnostic
c
      write (*, '(/, ''Map Raw Satellite Surface Data Impacts'')')
c
c     ..loop over observing systems
c
      do sat = 1, n_sat
c
c     ..extract sat data types
c
      n = 0
      do i = 1, n_data
         do typ = 1, n_typ 
            if (ob_typ(i) .eq. sat_typ(typ,sat)) then
               n = n + 1
               imp(n) = ob_imp(i)
               lat(n) = ob_lat(i)
               lon(n) = ob_lon(i)
               sen(n) = ob_sen(i)
               xi(n)  = ob_xi(i)
               yj(n)  = ob_yj(i)
            endif
         enddo
      enddo
      if (n .eq. 0) cycle
c
c     ..initialize
c
      do i = 1, nodes
         sfc_clr(i) = 0.
         sfc_cnt(i) = 0.
         sfc_imp(i) = 0.
         sfc_lat(i) = 0.
         sfc_lon(i) = 0.
         sfc_sen(i) = 0.
      enddo
c
c     ..set color slicing
c
      dmn = -0.6
      dmx =  0.6
      del = (dmx - dmn) / real (n_slcm)
      lntr = 5
c
c     ..keep it raw or bin data on grid
c
      if (sat .eq. 5 .or. sat .eq. 6) then
         do i = 1, n
            sfc_cnt(i) = 1
            sfc_imp(i) = imp(i)
            sfc_lat(i) = lat(i)
            sfc_lon(i) = lon(i)
            sfc_sen(i) = sen(i)
         enddo
      else
         do i = 1, n
            ix = nint (xi(i))
            jy = nint (yj(i))
            k = n_lon * (jy-1) + ix
            sfc_cnt(k) = sfc_cnt(k) + 1.
            sfc_imp(k) = sfc_imp(k) + imp(i)
            sfc_lat(k) = sfc_lat(k) + lat(i)
            sfc_lon(k) = sfc_lon(k) + lon(i)
         enddo
c
c        ..bin average
c
         do k = 1, (n_lon * n_lat)
            if (sfc_cnt(k) .gt. 0.) then
               sfc_imp(k) = sfc_imp(k) / sfc_cnt(k)
               sfc_lat(k) = sfc_lat(k) / sfc_cnt(k)
               sfc_lon(k) = sfc_lon(k) / sfc_cnt(k)
               sfc_sen(k) = sfc_sen(k) / sfc_cnt(k)
            endif 
         enddo
      endif
c
c     ..diagnostics
c
      write (*, '(''          processing: '', a)') sat_nam(sat)
      write (*, '(''          number obs: '', i10)') n
c
c     ..set map background
c
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
      if (sat .eq. 5 .or. sat .eq. 6) then
         do k = 1, n
            if (sfc_cnt(k) .gt. 0.) then
               sfc_clr(k) = nint ((sfc_imp(k) - dmn) / del) + 4
               if (sfc_clr(k) .lt. 5) sfc_clr(k) = 5
               if (sfc_clr(k) .gt. MX_ADJ_CLR) then
                  sfc_clr(k) = MX_ADJ_CLR
               endif
            endif
         enddo
      else
         do k = 1, (n_lon * n_lat)
            if (sfc_cnt(k) .gt. 0.) then
               sfc_clr(k) = nint ((sfc_imp(k) - dmn) / del) + 4
               if (sfc_clr(k) .lt. 5) sfc_clr(k) = 5
               if (sfc_clr(k) .gt. MX_ADJ_CLR) then
                  sfc_clr(k) = MX_ADJ_CLR
               endif
            endif 
         enddo
      endif
c
c     ..mark color sliced bin averaged grid positions
c
      if (sat .eq. 5 .or. sat .eq. 6) then
         call pcseti ('FN - fontcap number', 20)
         do k = 1, n
            if (sfc_cnt(k) .gt. 0.) then
               call maptra (sfc_lat(k), sfc_lon(k), u, v)
               call gsplci (sfc_clr(k))
               call plchhq (u, v, 'L', .01, 0., 0.)
               call plotit (0, 0, 0)
            endif
         enddo
         call pcseti ('FN - fontcap number', 0)
      else
         call gsmk (2)
         call gsmksc (0.1)
         do k = 1, (n_lon * n_lat)
            if (sfc_cnt(k) .gt. 0.) then
               call maptra (sfc_lat(k), sfc_lon(k), u, v)
               call gspmci (sfc_clr(k))
               call gpm (1, u, v)
               call plotit (0, 0, 0)
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
      call title_plot (title1, .022, 1, 2.2)
      if (sat_nam(sat)(1:4) .eq. 'SMOS' .or.  
     *    sat_nam(sat)(1:4) .eq. 'SMAP') then
         write (title2, '(a, '' SSS Data Impacts (PSU)'')')
     *          trim (sat_nam(sat))
      else
         write (title2, '(a, '' SST Data Impacts (C)'')')
     *          trim (sat_nam(sat))
      endif 
      call title_plot (title2, .022, 1, 0.8)
c
c     ..advance plot frame
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)') fno, trim (title2)
      call frame
      enddo
c
c     ..clean up
c
      deallocate (imp, lat, lon, sen, xi, yj)
      deallocate (iai, iag, iamap, lblndx, llbs, slc, xcs, ycs)
      deallocate (sfc_clr, sfc_cnt, sfc_imp, sfc_lat, sfc_lon)
      deallocate (sfc_sen)
c
      return
      end
