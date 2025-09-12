      subroutine glb_polar (data, dmn, dmx, m, n, n_lon, n_lat,
     *                      node, pos1, pos2, pos3, pos4, n_slc,
     *                      iamap, mx_amap, lntr, opt, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  glb_polar
c
c DESCRIPTION:  this routine contours a polar sterographic projection 
c               of an irregular gridded field.
c      
c PARAMETERS:
c     Name        Type    Usage          Description
c   --------     ------   -----   -----------------------------------
c   data         real     input   contour array at model grid nodes
c   dmn          real     input   color slice data maximum
c   dmx          real     input   color slice data minimum
c   lntr         integer  input   label interval
c   iamap        integer  input   area map work array
c   mx_amap      integer  input   area map dimension
c   m, n         integer  input   number of lon,lat in data array
c   node         real     input   plot array indicies
c   n_slc        integer  input   number of color contour intervals
c   opt          char     input   hemisphere option
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
c     ..local array dimensions
c
      integer   m, n
      integer   mx_amap
      integer   n_lon, n_lat
c
      real      data (m * n)
      real      dmn, dmx
      real      delta
      real      fl, fr, fb, ft
      integer   i
      integer   iamap (mx_amap)
      integer   ll
      integer   lntr
      integer   n_pts
      integer   n_slc
      integer   n_strt
      real      nh_bl (2), nh_br (2)
      real      nh_tl (2), nh_tr (2)
      real      node ((n_lon * n_lat), 2)
      character opt * 2
      real      pos1, pos2, pos3, pos4
      real      sh_bl (2), sh_br (2)
      real      sh_tl (2), sh_tr (2)
      real      spval
      real      ul, ur, ub, ut
c
c     ..allocatable arrays
c
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
      real,     allocatable :: work (:)
      real,     allocatable :: xcra (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycra (:)
      real,     allocatable :: ycs (:)
c
      external  colram
      external  colrcl
      external  clin
c
c     ..polar stereographic projection grid corners
c
      data      nh_bl / 38.65, 255.00 /
      data      nh_br / 38.60, 164.94 /
      data      nh_tl / 38.60, 345.06 /
      data      nh_tr / 38.55,  75.00 /
c
      data      sh_bl / -38.65, 165.00 /
      data      sh_br / -38.60, 255.06 /
      data      sh_tl / -38.60,  74.94 /
      data      sh_tr / -38.55, 345.00 /
c
c...............................executable..............................
c
c     ..allocate arrays
c
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
      allocate (work (n_lon * n_lat))
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
      if (opt .eq. 'nh') then
         call mapset ('PO', nh_bl, nh_br, nh_tl, nh_tr)
      else
         call mapset ('PO', sh_bl, sh_br, sh_tl, sh_tr)
      endif
c
c     ..set map projection 
c
      if (opt .eq. 'nh') then
         call maproj ('ST', 90., 300., 0.0)
      else
         call maproj ('ST', -90., 300., 0.0)
      endif
c
c     ..set map grid overlay spacing (degrees)
c
      call mapstr ('GR', 20.)
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
c     ..initialize plot array
c
      n_pts = n_lon * n_lat
      n_strt = 1
      call fld2_trp (n_pts, n_strt, n_pts, node(1,1), node(1,2),
     *               m, n, data, spval, work, fail)
c
c     ..put analysis contour lines into area map
c
      call cprect (work, n_lon, n_lon, n_lat, rwork, MX_WL,
     *             iwork, MX_WL)
      call cpclam (work, rwork, iwork, iamap)
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
         delta = dmn + real (i-1) * (dmx - dmn) / real (n_slc)
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
     *             lblndx, 0, llbs, (n_slc+1), 1)
c
c     ..set line and text colors to foreground
c
      call gsplci (1)
      call gstxci (1)
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
      deallocate (fail, iai, iaia, iag, igia, iwork, lblndx, llbs)
      deallocate (rwork, slc, work, xcra, xcs, ycra, ycs)
c
      return
      end
