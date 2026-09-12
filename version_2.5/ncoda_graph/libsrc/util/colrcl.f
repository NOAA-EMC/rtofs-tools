      subroutine colrcl (xcra, ycra, ncra, iaia, igia, naia)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  colrcl
c
c DESCRIPTION:  This routine draws a contour line masked by land
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
      integer   iai1
      integer   iai3
      integer   iaia (*)
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
c
c     ..find the area group identifier -
c       group 1 defines land/sea boundary
c       group 3 defines contour interval
c
      do i = 1, naia
         if (igia(i) .eq. 1) iai1 = iaia(i)
         if (igia(i) .eq. 3) iai3 = iaia(i)
      enddo
c
c     ..draw contour lines in open sea areas using prescribed
c       color (last call to gsplci)
c
      if (mdipan (iai1, 'Land') .eq. 0) then
         if (iai3 .gt. 0) then
            call gpl (ncra, xcra, ycra)
         endif
      endif
c
      return
      end
