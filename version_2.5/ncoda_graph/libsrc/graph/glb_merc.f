      subroutine glb_merc (data, depth, dmn, dmx, m, n, n_lon,
     *                     n_lat, bl, br, tl, tr, i1, i2, j1,
     *                     j2, node_eq, pos1, pos2, pos3, pos4, 
     *                     n_slc, iamap, mx_amap, cntr_int, 
     *                     do_cntr, lntr, z_lev, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  glb_merc
c
c DESCRIPTION:  this routine contours a mercator projection of an
c               irregular gridded field.  the projection is limited
c               from 75S to 75N at 9 km grid resolution. the code  
c               supports zooming into the global grid and plotting
c               subareas.
c      
c PARAMETERS:
c     Name        Type    Usage          Description
c   --------     ------   -----   -----------------------------------
c   bl, br       real     input   bottom left, right lat, lon
c   cntr_int     real     input   contour interval of data array
c   data         real     input   contour array global grid
c   depth        real     input   bottom depth global grid
c   dmn          real     input   color slice data maximum
c   dmx          real     input   color slice data minimum
c   do_cntr      logical  input   (true) overlay contour lines
c   i1, i2       integer  input   zoomed i coordinates
c   j1, j2       integer  input   zoomed j coordinates
c   lntr         integer  input   label interval
c   iamap        integer  input   ncar area map 
c   mx_amap      integer  input   area map dimension in calling routine
c   m, n         integer  input   number of lon,lat in model array
c   n_lon, n_lat integer  input   global mercator grid projection
c   node_eq      real     input   plot array indicies
c   n_slc        integer  input   number of color contour intervals
c   pos*         real     input   position of grid in plot space
c   spval        real     input   special value
c   tl, tr       real     input   top left, right lat, lon
c   z_lev        integer  input   nominal z-level of grid layer
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
c     ..local array dimensions
c
      integer   m, n
      integer   mx_amap
      integer   n_lon, n_lat
c
      real      bl (2), br (2)
      real      cntr_int
      real      data (m * n)
      real      depth (m * n)
      real      dmn, dmx
      real      delta
      logical   do_cntr
      real      fl, fr, fb, ft
      integer   i, j
      integer   i1, i2, j1, j2
      integer   iamap (mx_amap)
      integer   ll
      integer   lntr
      integer   ms, ns
      integer   n_lvl
      integer   n_pass
      integer   n_pts
      integer   n_slc
      integer   n_strt
      real      node_eq ((n_lon * n_lat), 2)
      real      pos1, pos2, pos3, pos4
      real      spval
      real      tl (2), tr (2)
      real      ul, ur, ub, ut
      integer   z_lev
c
c     ..allocatable arrays
c
      real,     allocatable :: btm (:,:)
      real,     allocatable :: btm_msk (:,:)
      logical,  allocatable :: fail (:)
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iaia (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: igia (:)
      integer,  allocatable :: iwork (:)
      integer,  allocatable :: lblndx (:)
      character,allocatable :: llbs (:) * 10
      real,     allocatable :: rwork (:)
      real,     allocatable :: slc (:)
      real,     allocatable :: work (:,:)
      real,     allocatable :: xcra (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycra (:)
      real,     allocatable :: ycs (:)
c
      external  colram
      external  colrcl
      external  clin
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (btm (n_lon, n_lat))
      allocate (btm_msk (n_lon, n_lat))
      allocate (fail (n_lon * n_lat))
      allocate (iai (MX_AI))
      allocate (iaia (MX_WL))
      allocate (iag (MX_AI))
      allocate (igia (MX_WL))
      allocate (iwork (MX_WL))
      allocate (lblndx (MX_CNT))
      allocate (llbs (MX_CNT))
      allocate (rwork (MX_WL))
      allocate (slc (MX_CNT))
      allocate (work (n_lon, n_lat))
      allocate (xcra (MX_WL))
      allocate (xcs (MX_LIN))
      allocate (ycra (MX_WL))
      allocate (ycs (MX_LIN))
c
c     ..initialize area map edge boundaries
c
      call arinam (iamap, mx_amap)
c
c     ..do not label meridians and poles
c
      call mapsti ('LA', 0)
c
c     ..set map position within plotter frame
c
      call mappos (pos1, pos2, pos3, pos4)
      call mapset ('PO', bl, br, tl, tr)
c
c     ..set map projection 
c
      call maproj ('ME', 0.0, 180., 0.0)
c
c     ..set map grid overlay spacing (degrees)
c
      if (i1 .gt. 1) then
         call mapstr ('GR', 10.)
      else
         call mapstr ('GR', 30.)
      endif
c
c     ..initialize, add land sea boundaries to area map
c
      call mplnam ('Earth..3', 1, iamap)
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
      delta = (dmx - dmn) / real (n_slc)
      do i = 1, n_slc
         slc(i) = real (i) * delta + dmn
      enddo
c
c     ..set color fill slices
c
      call cpseti ('CLS - contour level selector', 0)
      call cpseti ('NCL - number of contour levels', (n_slc - 1))
      do i = 1, (n_slc - 1)
         call cpseti ('PAI - parameter array index', i)
         call cpsetr ('CLV - contour level', slc(i))
         call cpseti ('CLU - contour level use', 1)
         call cpseti ('AIB - area identifier below level', i)
         call cpseti ('AIA - area identifier above level', (i + 1))
      enddo
c
c     ..initialize plot arrays
c
      n_pts = n_lon * n_lat
      n_strt = 1
      call fld2_trp (n_pts, n_strt, n_pts, node_eq(1,1), node_eq(1,2),
     *               m, n, data, spval, work, fail)
      call fld2_trp (n_pts, n_strt, n_pts, node_eq(1,1), node_eq(1,2),
     *               m, n, depth, spval, btm, fail)
c
c     ..set bottom mask
c
      do j = 1, n_lat
         do i = 1, n_lon
            if (btm(i,j) .lt. (real (z_lev))) then
                btm_msk(i,j) = -1.
            else
                btm_msk(i,j) = 1.
            endif
         enddo
      enddo
c
c     ..extend into coast
c
      if (z_lev .gt. 0) then
         n_lvl = 1
         call coda_xtnd (n_lon, n_lat, n_lvl, work, spval)
      endif
      do j = 1, n_lat
         do i = 1, n_lon
            if (btm(i,j) .lt. 0.) work(i,j) = spval
         enddo
      enddo
c
c     ..set array indices (total or subsetted)
c
      ms = i2 - i1 + 1
      ns = j2 - j1 + 1
c
c     ..put field contour lines into area map
c
      call cprect (work(i1,j1), n_lon, ms, ns, rwork, MX_WL,
     *             iwork, MX_WL)
      call cpclam (work(i1,j1), rwork, iwork, iamap)
c
c     ..put contour line into area map describing bottom outline -
c       use group identifier 4 to distinguish from NCAR default of
c       group 3 for contour lines - contour only the zero line -
c       turn on smoothing of mask line
c       (the data have been scaled to vary from -1 to 1).
c
      call cpseti ('GIC - group identifier for contour lines', 4)
      call cpseti ('CLS - contour level selector', -1)
      call cpsetr ('CIS - contour interval specifier', 0.)
      call cpsetr ('T2D - tension on 2D splines', 2.5)
      call cprect (btm_msk(i1,j1), n_lon, ms, ns, rwork, MX_WL,
     *             iwork, MX_WL)
      call cpclam (btm_msk(i1,j1), rwork, iwork, iamap)
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
      do i = 1, (n_slc + 1), lntr
         delta = dmn + real (i - 1) * (dmx - dmn) / real (n_slc)
         call cpsetr ('ZDV - z data value', delta)
         call cpgetc ('ZDV - z data value', llbs(i))
         if (abs (delta) .lt. .001) llbs(i) = '0'
      enddo
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
     *             lblndx, 0, llbs, (n_slc + 1), 1)
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
c        ..smooth the contour line map
c
         n_pass = 3
         call smth_2d (n_pass, work, n_lon, n_lat, spval)
c
c        ..mark data areas below bottom as missing
c
         do j = 1, n_lat
            do i = 1, n_lon
               if (btm_msk(i,j) .lt. 0.) work(i,j) = spval
            enddo
         enddo
c
c        ..draw and label contour lines
c
         call cprect (work(i1,j1), n_lon, ms, ns, rwork, MX_WL,
     *                iwork, MX_WL)
         call cpcldm (work(i1,j1), rwork, iwork, iamap, colrcl)
         call cpcldr (work(i1,j1), rwork, iwork)
         call cplbdr (work(i1,j1), rwork, iwork)
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
c     ..clean up
c
      deallocate (btm, btm_msk, fail, iai, iaia, iag, igia, iwork)
      deallocate (lblndx, llbs, rwork, slc, work, xcra, xcs, ycra)
      deallocate (ycs)
c
      return
      end
