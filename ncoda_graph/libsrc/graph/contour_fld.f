      subroutine contour_fld (data, dmn, dmx, n_lon, n_lat, i1, i2,
     *                        j1, j2, n_slc, iamap, mx_amap, btm_msk,
     *                        cntr_int, do_btm, do_cntr, do_clbl,
     *                        do_blbl, do_anm, lntr, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  contour_fld
c
c DESCRIPTION:  this routine contours a gridded field masked by land
c               and color filled. a label bar is produced at the bottom
c               of the plot. the origin of the gridded field is the
c               lower left corner with columns (longitudes) increasing
c               faster than rows (latitudes).
c
c               a call to this routine must have been preceeded by a
c               call to the routine "MAP_BKG" (where SET has been
c               called by EZMAP).
c
c PARAMETERS:
c     Name        Type    Usage          Description
c   --------     ------   -----   -----------------------------------
c   btm_msk      real     input   scaled bathymetry at grid nodes
c   cntr_int     real     input   contour interval of data array
c   data         real     input   contour array at analysis grid nodes
c   dmn          real     input   color slice data maximum
c   dmx          real     input   color slice data minimum
c   do_anm       logical  input   (true) suppress 0 contour line
c   do_blbl      logical  input   (true) insert range on label bar
c   do_btm       logical  input   (true) apply bottom masking
c   do_clbl      logical  input   (true) label contour lines
c   do_cntr      logical  input   (true) overlay contour lines
c   lntr         integer  input   label interval
c   iamap        integer  input   area map (defined in "PLOT_BKG")
c   mx_amap      integer  input   area map dimension in calling routine
c   n_lon        integer  input   number of longitudes in data array
c   n_slc        integer  input   number of color contour intervals
c   spval        real     input   special value
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
      parameter (MX_AI = 1 280 000)
c
      integer    MX_CNT
      parameter (MX_CNT = 64 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 1 280 000)
c
      integer    MX_WL
      parameter (MX_WL = 1 280 000)
c
c     ..local array dimension
c
      integer   mx_amap
      integer   n_lon, n_lat
c
      real      btm_msk (n_lon, n_lat)
      real      cntr_int
      real      cntr_lvl
      real      data (n_lon, n_lat)
      real      dmn, dmx
      real      delta
      logical   do_anm
      logical   do_blbl
      logical   do_btm
      logical   do_clbl
      logical   do_cntr
      real      fl, fr, fb, ft
      integer   i, j, k, m, n
      integer   i1, i2, j1, j2
      integer   iamap (mx_amap)
      integer   len
      integer   ll
      integer   lntr
      integer   n_slc
      real      spval
      real      ul, ur, ub, ut
      real      work (n_lon, n_lat)
      real      upt, vpt, xpt(2), ypt(2)
c
c     ..allocatable arrays
c
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iaia (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: igia (:)
      integer,  allocatable :: iwork (:)
      integer,  allocatable :: lblndx (:)
      character,allocatable :: llbs (:) * 10
      real,     allocatable :: rwork (:)
      real,     allocatable :: slc (:)
      real,     allocatable :: xcra (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycra (:)
      real,     allocatable :: ycs (:)
c************************
c     real,     allocatable :: corr (:,:)
c*************************
c
      external  colram
      external  colrcl
      external  clin
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (iai (MX_AI))
      allocate (iaia (MX_WL))
      allocate (iag (MX_AI))
      allocate (igia (MX_WL))
      allocate (iwork (MX_WL))
      allocate (lblndx (MX_CNT))
      allocate (llbs (MX_CNT))
      allocate (rwork (MX_WL))
      allocate (slc (MX_CNT))
      allocate (xcra (MX_WL))
      allocate (xcs (MX_LIN))
      allocate (ycra (MX_WL))
      allocate (ycs (MX_LIN))
c**************************
c     allocate (corr (n_lon, n_lat))
c     call ssh_corr (n_lon, n_lat, data, corr, xpt, ypt)
c***********************************
c
c     ..set solid fill
c
      call gsfais (1)
c
c     ..set contour mapping parameters
c
      call getset (fl, fr, fb, ft, ul, ur, ub, ut, ll)
      call cpseti ('SET - do not do set call', 0)
      call cpseti ('MAP - mapping flag', 0)
      call cpsetr ('XC1 - x coordinate at imin', ul)
      call cpsetr ('XCM - x coordinate at imax', ur)
      call cpsetr ('YC1 - y coordinate at jmin', ub)
      call cpsetr ('YCN - y coordinate at jmax', ut)
c
c     ..set plot parameters
c
      call cpsetc ('CFT - constant field label', ' ')
      call cpsetc ('ILT - information label text string', ' ')
      call cpseti ('GIC - group identifier for contour lines', 3)
      call cpsetr ('T2D - tension on 2D splines', 0.)
      call cpsetc ('HLT', ' ')
      call cpseti ('NOF - numeric ommission flag', 7)
      call cpsetr ('SPV - special value', spval)
c
c     ..set color slice indices
c
c***********************
c     dmx = 1.
c     dmn = 0.
c     write (*,'(''n_slc: '', i10)') n_slc
c***********************
      delta = (dmx - dmn) / real (n_slc)
      do i = 1, n_slc
         slc(i) = real (i) * delta + dmn
      enddo
c
c     ..set color fill slices
c
      call cpseti ('CLS - contour level selector', 0)
      call cpseti ('NCL - number of contour levels', (n_slc-1))
      do i = 1, (n_slc-1)
         call cpseti ('PAI - parameter array index', i)
         call cpsetr ('CLV - contour level', slc(i))
         call cpseti ('CLU - contour level use', 1)
         call cpseti ('AIB - area identifier below level', i)
         call cpseti ('AIA - area identifier above level', (i+1))
      enddo
c
c     ..set array indices (total or subsetted)
c
      m = i2 - i1 + 1
      n = j2 - j1 + 1
c
c     ..put analysis contour lines into area map
c
      call cprect (data(i1,j1), n_lon, m, n, rwork, MX_WL,
     *            iwork, MX_WL)
      call cpclam (data(i1,j1), rwork, iwork, iamap)
      if (do_btm) then
c
c        ..put contour line into area map describing bottom outline -
c          use group identifier 4 to distinguish from NCAR default of
c          group 3 for contour lines - contour only the zero line -
c          turn on smoothing of mask line
c          (the data have been scaled to vary from -1 to 1).
c
         call cpseti ('GIC - group identifier for contour lines', 4)
         call cpseti ('CLS - contour level selector', -1)
         call cpsetr ('CIS - contour interval specifier', 0.)
         call cpsetr ('T2D - tension on 2D splines', 2.5)
         call cprect (btm_msk(i1,j1), n_lon, m, n, rwork, MX_WL,
     *                iwork, MX_WL)
         call cpclam (btm_msk(i1,j1), rwork, iwork, iamap)
      endif
c
c     ..color the map 
c
      call arscam (iamap, xcra, ycra, MX_WL, iaia, igia,
     *             MX_WL, colram)
c
c     ..loop over contour intervals setting label bar labels
c
      do i = 1, MX_CNT
         lblndx(i) = i + 4
         llbs(i) = '          '
      enddo
      do i = 1, (n_slc+1), lntr
         delta = dmn + real (i-1) * (dmx-dmn) / real (n_slc)
         call cpsetr ('ZDV - z data value', delta)
         call cpgetc ('ZDV - z data value', llbs(i))
         if (do_anm) then
            if (abs (delta) .lt. .001) llbs(i) = '0'
         endif
      enddo
      if (do_blbl) then
         len = len_trim (llbs(1))
         llbs(1) = '<' // llbs(1)(1:len)
         len = len_trim (llbs(n_slc+1))
         llbs(n_slc+1) = llbs(n_slc+1)(1:len) // '>'
      endif
c
c     ..plot label bars below map - set the color of the box lines
c       to the foreground default - note that the number of labels
c       is one more than the number of boxes; this forces the first
c       label with the beginning of the bar, the last label with the
c       end of the bar, and the labels in between with the divisions
c       between the boxes
c
      call lbseti ('CBL - color of box lines', 1)
      call lblbar (0, .1, .9, .0, .09, n_slc, 1., 0.25,
     *             lblndx, 0, llbs, (n_slc+1), 1)
c
c     ..set line and text colors to foreground
c
      call gsplci (1)
      call gstxci (1)
c
      if (do_cntr) then
c
c        ..reset parameters for constant contour interval
c
         call cpsetr ('CIS - contour interval specifier', cntr_int)
         call cpseti ('CLS - contour level selector', 1)
         call cpseti ('LIS - label interval specifier', 2)
         call cpsetr ('CWM - character width multiplier', 1.)
         call cpsetc ('ILT - information label text string', ' ')
         call cpsetr ('T2D - tension on 2D splines', 0.)
c
c        ..set line labelling scheme
c
         call cpseti ('LLP - line label positioning', 3)
         call cpseti ('LLO - line label orientation', 1)
         call cpsetr ('LLS - line label size', .011)
         call cpsetr ('LLW - line label white space', .006)
c
         if (do_anm) then
c
c           ..suppress plotting 0 contour line (as if this works)
c
            call cpgeti ('NCL - number of contour levels', k)
            do i = 1, k
               call cpseti ('PAI - parameter array index', i)
               call cpgetr ('CLV - contour level', cntr_lvl)
               if (abs (cntr_lvl) .lt. 0.001) then
                  call cpseti ('CLU - contour level use', 0)
               endif
            enddo
         endif
c
c        ..mark data areas below bottom as missing
c
         do j = 1, n_lat
            do i = 1, n_lon
               if (btm_msk(i,j) .lt. 0.) then
                  work(i,j) = spval
               else
                  work(i,j) = data(i,j)
               endif
            enddo
         enddo
c
c        ..draw and label contour lines
c
         call cprect (work(i1,j1), n_lon, m, n, rwork, MX_WL,
     *                iwork, MX_WL)
         call cpcldm (work(i1,j1), rwork, iwork, iamap, colrcl)
         call cpcldr (work(i1,j1), rwork, iwork)
         if (do_clbl) then
            call cplbdr (work(i1,j1), rwork, iwork)
         endif
      endif
c
c     ..add lines of longitudes and latitudes masked by land fill
c
      call gsln (2)
      call mapgrm (iamap, xcs, ycs, MX_LIN, iai, iag, MX_AI, clin)
      call gsln (1)
c
c     ..draw continent outlines and map lables
c
      call gsplci (1)
      call mplndr ('Earth..3', 1) 
      call maplbl
c
c     ..mark point
c
      do i = 1, 2
      call maptra (ypt(i), xpt(i), upt, vpt)
      call gsplci (0)
      call plchhq (upt, vpt, 'X', .012, 0., 0.)
      call plotit (0, 0, 0)
      enddo
c
c     ..clean up
c
      deallocate (iai, iaia, iag, igia, iwork, lblndx, llbs)
      deallocate (rwork, slc, xcra, xcs, ycra, ycs)
c*********************
c     deallocate (corr)
c***********************
c
      return
      end
