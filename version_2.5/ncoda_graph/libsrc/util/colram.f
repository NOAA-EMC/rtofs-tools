      subroutine colram (xcra, ycra, ncra, iaia, igia, naia)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  colram
c
c DESCRIPTION:  This routine colors an area from an area map
c      
c PARAMETERS:
c       Name          Type       Usage            Description
c   -------------   ----------   -----   -----------------------------
c   iaia            integer      input   area identifiers
c   igia            integer      input   associated group identifiers
c   naia            integer      input   number group identifers
c   ncra            integer      input   number polygon coordinates
c   xcra            real         input   x coordinates polygon points
c   ycra            real         input   y coordinates polygon points
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
      integer   i
      integer   iaia (*)
      integer   iai1
      integer   iai3
      integer   iai4
      integer   iai5
      integer   igia (*)
      integer   naia
      integer   ncra
      real      xcra (*)
      real      ycra (*)
c
c     ..functions
c
      integer   mdipan
c
c...............................executable..............................
c
c     ..initialize area identifiers to missing
c
      iai1 = -1
      iai3 = -1
      iai4 = -1
      iai5 = -1
c
c     ..find the area group identifier - 
c       group 1 defines land/sea boundary
c       group 3 defines contour interval
c       group 4 defines bottom depth boundary
c       group 5 defines ice edge boundary
c
      do i = 1, naia
         if (igia(i) .eq. 1) iai1 = iaia(i)
         if (igia(i) .eq. 3) iai3 = iaia(i)
         if (igia(i) .eq. 4) iai4 = iaia(i)
         if (igia(i) .eq. 5) iai5 = iaia(i)
      enddo
c
c     ..process land as solid fill (map-fill index 2) and open water
c       as solid fill contours with user defined color index scheme
c       below bottom areas are color filled with a silty gray
c       over ice areas are color filled with white
c
      if (mdipan (iai1, 'Land') .ne. 0) then
         call gsfaci(2)
         call gfa ((ncra-1), xcra, ycra)
      else if (iai5 .eq. 1) then
         call gsfaci(2)
         call gfa ((ncra-1), xcra, ycra)
      else if (iai4 .eq. 1) then
         call gsfaci(3)
         call gfa ((ncra-1), xcra, ycra)
      else if (iai5 .eq. 1) then
         call gsfaci(4)
         call gfa ((ncra-1), xcra, ycra)
      else if (iai3 .gt. 0) then
         call gsfaci (iai3 + 4)
         call gfa ((ncra - 1), xcra, ycra)
      endif
c
      return
      end
