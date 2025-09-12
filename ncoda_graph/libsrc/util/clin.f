      subroutine clin (xcra, ycra, ncra, iaia, igia, naia)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  clin
c
c DESCRIPTION:  routine to draw a map grid overlay in open sea areas
c
c PARAMETERS:
c      Name          Type       Usage            Description
c   ----------    ----------   -------   ----------------------------
c   iaia          integer      input     area identifiers
c   igia          integer      input     associated group identifiers
c   naia          integer      input     number area identifers
c   ncra          integer      input     number polygon coordinate
c   xcra          real         input     x coordinates polygon points
c   ycra          real         input     y coordinates polygon points
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
      integer   iaia (*)
      integer   igia (*)
      integer   isea
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
c     ..draw grid over ocean areas
c
      isea = mdipan (iaia(1), 'Land')
      if (isea .eq. 0) then
         call gpl (ncra, xcra, ycra)
      endif
c
      return
      end
