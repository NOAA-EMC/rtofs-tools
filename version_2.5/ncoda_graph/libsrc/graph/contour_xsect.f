      subroutine contour_xsect (xsect_data, xsect_mask, MX_HORIZ,
     *                          MX_VERT, n_slice, cntr_int, do_mask,
     *                          dmn, dmx, lntr, do_lbl, do_anm,
     *                          spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  contour_xsect
c
c DESCRIPTION:  this routine contours a CODA cross section, bottom
c               masked and color filled.  a label bar is produced at
c               the side of the plot.  the origin of the gridded field
c               is the lower left corner with columns (distance)
c               increasing faster than rows (depths).
c
c PARAMETERS:
c     Name      Type    Usage          Description
c   --------   ------   -----   -----------------------------------
c   cntr_int   real     input   contour interval of data array
c   dmn        real     input   color slice data maximum
c   dmx        real     input   color slice data minimum
c   do_anm     logical  input   (true) suppress 0 contour line
c   do_lbl     logical  input   (false) blank label bar labels
c   do_mask    logical  input   (true) apply bottom mask
c   lntr       integer  input   label interval
c   MX_HORIZ   integer  input   number section grid nodes
c   MX_VERT    integer  input   number depth grid nodes
c   n_slice    integer  input   number of color contour intervals
c   spval      real     input   missing value indicator
c   xsect_data real     input   vertical cross section array
c   xsect_mask real     input   bottom depth cross section mask
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
      integer    MX_CNT
      parameter (MX_CNT = 16 000)
c
      integer    MX_WL
      parameter (MX_WL = 240 000)
c
c     ..local array dimensions
c
      integer   MX_HORIZ
      integer   MX_VERT
c
      real      cntr_int
      real      cntr_lvl
      real      dmn, dmx
      real      delta
      logical   do_anm
      logical   do_lbl
      logical   do_mask
      integer   i, j
      integer   len
      integer   lntr
      integer   n_slice
      real      spval
      real      xsect_data (MX_HORIZ, MX_VERT)
      real      xsect_mask (MX_HORIZ, MX_VERT)
c
c     ..allocatable arrays
c
      integer,  allocatable :: iaia (:)
      integer,  allocatable :: iamap (:)
      integer,  allocatable :: igia (:)
      integer,  allocatable :: iwork (:)
      integer,  allocatable :: lblndx (:)
      character,allocatable :: llbs (:) * 10
      real,     allocatable :: rwork (:)
      real,     allocatable :: slice (:)
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
      allocate (lblndx (MX_CNT))
      allocate (llbs (MX_CNT))
      allocate (rwork (MX_WL))
      allocate (slice (MX_CNT))
      allocate (xcra (MX_WL))
      allocate (ycra (MX_WL))
c
c     ..set solid fill
c
      call gsfais (1)
c
c     ..initialize area map
c
      call arinam (iamap, MX_AMAP)
c
c     ..let conpack pick contour intervals - use "n_slice-1" contour
c       levels splitting the range of data into "n_slice" equal bands,
c       one for each of the "n_slice" colors available (default
c       foreground (1), background (0), and land fill (2) are not used
c       in the contour color fill process)
c
      call cpsetr ('CIS - contour interval specifier', 0.)
      call cpseti ('CLS - contour level selector', -(n_slice-1))
c
c     ..set contour mapping parameters - do not do set call
c
      call cpseti ('SET - do not do set call', 0)
      call cpseti ('MAP - mapping flag', 0)
      call cpsetr ('XC1 - left boundary', .11)
      call cpsetr ('XCM - right boundary', .89)
      call cpsetr ('YC1 - bottom boundary', .07)
      call cpsetr ('YCN - top boundary', .66)
      call cpseti ('GIC - group identifier for contour lines', 3)
      call cpsetc ('HLT', ' ')
      call cpseti ('NOF - numeric ommission flag', 7)
      call cpsetr ('SPV - special value', spval)
      call cpsetr ('T2D - contour smoothing parameter', 0.)
c
c     ..set color slice indices
c
      delta = (dmx - dmn) / real (n_slice)
      do i = 1, n_slice
         slice(i) = real (i) * delta + dmn
      enddo
c
c     ..set color fill slices
c
      call cpseti ('CLS - contour level selector', 0)
      call cpseti ('NCL - number of contour levels', (n_slice - 1))
      do i = 1, (n_slice - 1)
         call cpseti ('PAI - parameter array index', i)
         call cpsetr ('CLV - contour level', slice(i))
         call cpseti ('CLU - contour level use', 1)
         call cpseti ('AIB - area identifier below level', i)
         call cpseti ('AIA - area identifier above level', (i + 1))
       enddo
c
c     ..initialize drawing of contour plot
c
      call cprect (xsect_data, MX_HORIZ, MX_HORIZ, MX_VERT,
     *             rwork, MX_WL, iwork, MX_WL)
      call cpclam (xsect_data, rwork, iwork, iamap)
c
c     ..loop over contour intervals setting label bar labels
c
      do i = 1, MX_CNT
         llbs(i) = '          '
         lblndx(i) = i + 4
      enddo
      do i = 1, (n_slice + 1), lntr
         delta = dmn + real (i - 1) * (dmx - dmn) / real (n_slice)
         call cpsetr ('ZDV - z data value', delta)
         call cpgetc ('ZDV - z data value', llbs(i))
      enddo
      if (.not. do_lbl) then
         if (dmn .ne. 0.) then
            len = len_trim (llbs(1))
            llbs(1) = '<' // llbs(1)(1:len)
         endif
         len = len_trim (llbs(n_slice+1))
         llbs(n_slice+1) = llbs(n_slice+1)(1:len) // '>'
      endif
c
c     ..plot label bars to right of section - set color of box lines
c       to the foreground default - note that the number of labels
c       is one more than the number of boxes; this forces the first
c       label with the beginning of the bar, the last label with the
c       end of the bar, and the labels in between with the divisions
c       between the boxes
c
      call lbseti ('CBL - color of box lines', 1)
      call lblbar (1, .9, 1.0, .07, .66, n_slice, .25, 1.0,
     *             lblndx, 0, llbs, (n_slice+1), 1)
c
      if (do_mask) then
c
c        ..put contour line into area map describing bottom outline -
c          use group identifier 4 to distinguish from NCAR default of
c          group 3 for contour lines - contour only the zero line -
c          smooth the contour slightly to avoid boxey look of grid mesh
c
         call cpseti ('GIC - group identifier for contour lines', 4)
         call cpseti ('CLS - contour level selector', -1)
         call cpsetr ('CIS - contour interval specifier', 0.)
         call cprect (xsect_mask, MX_HORIZ, MX_HORIZ, MX_VERT,
     *                rwork, MX_WL, iwork, MX_WL)
         call cpsetr ('T2D - contour smoothing parameter', 8.)
         call cpclam (xsect_mask, rwork, iwork, iamap)
      endif
c
c     ..color the map
c
      call arscam (iamap, xcra, ycra, MX_WL, iaia, igia,
     *             MX_WL, crsram)
c
c     ..overlay contours
c
      call gsplci (1)
      call gstxci (1)
      call cpsetr ('CIS - contour interval specifier', cntr_int)
      call cpseti ('CLS - contour level selector', 1)
      call cpseti ('LIS - label interval specifier', 2)
      call cpsetr ('CWM - character width multiplier', 1.)
      call cpsetc ('ILT - information label text string', ' ')
      call cpsetr ('T2D - contour smoothing parameter', 0.)
      call cpseti ('LLP - line label positioning', 3)
      call cpseti ('LLO - line label orientation', 1)
      call cpsetr ('LLS - line label size', .015)
      call cpsetr ('LLW - line label white space', .0075)
c
c     ..mark data areas below bottom as missing
c
      do j = 1, MX_VERT
         do i = 1, MX_HORIZ
            if (xsect_mask(i,j) .lt. 0.) xsect_data(i,j) = spval
         enddo
      enddo
c
      if (do_anm) then
c
c        ..suppress plotting 0 contour line
c
         call cpgeti ('NCL - number of contour levels', j)
         do i = 1, j
            call cpseti ('PAI - parameter array index', i)
            call cpgetr ('CLV - contour level', cntr_lvl)
            if ( abs (cntr_lvl) .lt. 0.001) then
                call cpseti ('CLU - contour level use', 0)
            endif
         enddo
      endif
c
c     ..draw and label contour lines
c
c**************************
c     read (81) xsect_data
c     n_slice = 16
c     dmn = 21.
c     dmx = 29.
c     delta = (dmx - dmn) / real (n_slice)
c     write (*,'(''read 81: '', i5,3f8.2)') n_slice,dmn,dmx,delta
c     do i = 1, n_slice
c        slice(i) = real (i) * delta + dmn
c     enddo
c     call cpseti ('CLS - contour level selector', 0)
c     call cpseti ('NCL - number of contour levels', (n_slice - 1))
c     do i = 1, (n_slice - 1)
c        call cpseti ('PAI - parameter array index', i)
c        call cpsetr ('CLV - contour level', slice(i))
c        call cpseti ('CLU - contour level use', 1)
c        call cpseti ('AIB - area identifier below level', i)
c        call cpseti ('AIA - area identifier above level', (i + 1))
c      enddo
c*************************
      call cprect (xsect_data, MX_HORIZ, MX_HORIZ, MX_VERT,
     *             rwork, MX_WL, iwork, MX_WL)
      call cpcldm (xsect_data, rwork, iwork, iamap, crsrcl)
      call cplbdr (xsect_data, rwork, iwork)
c
c     ..clean up
c
      deallocate (iaia, iamap, igia, iwork, lblndx, llbs, rwork)
      deallocate (slice, xcra, ycra)
c
      return
      end
