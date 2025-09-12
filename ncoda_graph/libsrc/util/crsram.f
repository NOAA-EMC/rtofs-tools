      subroutine crsram (xcra, ycra, ncra, iaia, igia, naia)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  crsram
c
c DESCRIPTION:  colors an area from an area map
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
c..............................END PROLOGUE.............................
c
      implicit  none
c
      integer   i
      integer   iaia (*)
      integer   iai3
      integer   iai4
      integer   igia (*)
      integer   naia
      integer   ncra
      real      xcra (*)
      real      ycra (*)
c
c...............................executable..............................
c
c     ..initialize area identifiers to missing
c
      iai3 = -1
      iai4 = -1
c
c     ..find the area group identifier - 
c       group 3 defines contour interval
c       group 4 defines below/above the bottom
c
      do i = 1, naia
         if (igia(i) .eq. 3) iai3 = iaia(i)
         if (igia(i) .eq. 4) iai4 = iaia(i)
      enddo
c
c     ..process below bottom as solid fill (index 3) and
c       open water as solid fill contours with user defined
c       color index scheme
c
      if (iai4 .eq. 1) then
         call gsfaci (3)
         call gfa ((ncra-1), xcra, ycra)
      else if (iai3 .gt. 0) then
         call gsfaci (iai3 + 4)
         call gfa ((ncra - 1), xcra, ycra)
      endif
c
      return
      end
