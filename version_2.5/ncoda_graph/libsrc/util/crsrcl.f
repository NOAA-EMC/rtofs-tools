      subroutine crsrcl (xcra, ycra, ncra, iaia, igia, naia)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  crsrcl
c
c DESCRIPTION:  this routine draws a contour line
c      
c PARAMETERS:
c       Name          Type       Usage            Description
c   -------------   ----------   -----   -----------------------------
c   iaia            integer      input   area identifiers
c   igia            integer      input   associated group identifiers
c   naia            integer      input   number area identifers
c   ncra            integer      input   number polygon coordinate
c   xcra            real         input   x coordinates polygon points
c   ycra            real         input   y coordinates polygon points
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
      integer   i
      integer   iai3
      integer   iai4
      integer   iaia (*)
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
c       group 1 defines plot boundary
c       group 3 defines contour interval
c       group 4 defines below/above the bottom
c
      do i = 1, naia
         if (igia(i) .eq. 3) iai3 = iaia(i)
         if (igia(i) .eq. 4) iai4 = iaia(i)
      enddo
c
c     ..draw contour lines in open sea areas using prescribed
c       color (last call to gsplci)
c
      if (iai4 .ne. 1) then
         if (iai3 .gt. 0) then
            call gpl (ncra, xcra, ycra)
         endif
      endif
c
      return
      end
