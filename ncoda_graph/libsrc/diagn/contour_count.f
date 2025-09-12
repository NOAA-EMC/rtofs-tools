      subroutine contour_count (m, n, cnt, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  contour_count
c
c DESCRIPTION:  this routine contours a data count array
c
c PARAMETERS:
c     Name      Type    Usage          Description
c   --------   ------   -----   -----------------------------------
c   cnt        real     input   data count array
c   m          integer  input   number x array nodes
c   n          integer  input   number y array nodes
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
      parameter (MX_AMAP = 128 000 000)
c
      integer    MX_CNT
      parameter (MX_CNT = 12 000)
c
      integer    MX_WL
      parameter (MX_WL = 48 000)
c
c     ..local array dimensions
c
      integer   m, n
c
      real      cnt (m, n)
      real      delta
      real      dmx
      real      fl, fr, fb, ft
      integer   i, j
      integer   ll
      integer   n_pass
      real      spval
      real      ul, ur, ub, ut
c
      include 'color_table.h'
c
c     ..allocatable arrays
c
      integer,  allocatable :: iaia (:)
      integer,  allocatable :: iamap (:)
      integer,  allocatable :: igia (:)
      integer,  allocatable :: iwork (:)
      real,     allocatable :: rwork (:)
      real,     allocatable :: slc (:)
      real,     allocatable :: xcra (:)
      real,     allocatable :: ycra (:)
c
c     ..NCAR external routines
c
      external  crsram
      external  crsrcl
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (iaia (MX_WL))
      allocate (iamap (MX_AMAP))
      allocate (igia (MX_WL))
      allocate (iwork (MX_WL))
      allocate (rwork (MX_WL))
      allocate (slc (MX_CNT))
      allocate (xcra (MX_WL))
      allocate (ycra (MX_WL))
c
c     ..mask and smooth count array
c
      do j = 1, n
         do i = 1, m
            if (cnt(i,j) .lt. 1.) cnt(i,j) = spval
         enddo
      enddo
      n_pass = 4
      call smth_2d (n_pass, cnt, m, n, spval)
c
c     ..set color scheme
c
      call gks_color (rgb_fld_clr, MX_FLD_CLR)
c
c     ..set solid fill
c
      call gsfais (1)
c
c     ..initialize area map
c
      call arinam (iamap, MX_AMAP)
c
c     ..let conpack pick contour intervals - use "n_slc-1" contour
c       levels splitting the range of data into "n_slc" equal bands,
c       one for each of the "n_slc" colors available (default
c       foreground (1), background (0), and land fill (2) are not used
c       in the contour color fill process)
c
      call cpsetr ('CIS - contour interval specifier', 0.)
      call cpseti ('CLS - contour level selector', -(n_slc-1))
c
c     ..set contour mapping parameters - do not do set call
c
      call getset (fl, fr, fb, ft, ul, ur, ub, ut, ll)
      call cpseti ('SET - do not do set call', 0)
      call cpseti ('MAP - mapping flag', 0)
      call cpsetr ('XC1 - x coordinate at imin', ul)
      call cpsetr ('XCM - x coordinate at imax', ur)
      call cpsetr ('YC1 - y coordinate at jmin', ub)
      call cpsetr ('YCN - y coordinate at jmax', ut)
      call cpseti ('GIC - group identifier for contour lines', 3)
      call cpsetc ('HLT', ' ')
      call cpseti ('NOF - numeric ommission flag', 7)
      call cpsetr ('SPV - special value', spval)
      call cpsetr ('T2D - contour smoothing parameter', 0.)
c
c     ..set color slice indices
c
      dmx = maxval (cnt)
      delta = dmx / real (n_slc)
      do i = 1, n_slc
         slc(i) = real (i) * delta
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
c     ..initialize drawing of contour plot
c
      call cprect (cnt, m, m, n, rwork, MX_WL, iwork, MX_WL)
      call cpclam (cnt, rwork, iwork, iamap)
c
c     ..color the map
c
      call arscam (iamap, xcra, ycra, MX_WL, iaia, igia,
     *             MX_WL, crsram)
c
c     ..clean up
c
      deallocate (iaia, iamap, igia, iwork, rwork)
      deallocate (slc, xcra, ycra)
c
      return
      end


