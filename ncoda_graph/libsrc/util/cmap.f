      subroutine cmap (xcra, ycra, ncra, iaia, igia, naia)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  cmap
c
c DESCRIPTION:  Routine to fill map areas over land
c
c PARAMETERS:
c      Name         Type        Usage            Description
c   ----------    ---------    -------   ----------------------------
c   iaia          integer      input     area identifiers
c   igia          integer      input     associated group identifiers
c   naia          integer      input     number area identifers
c   ncra          integer      input     number polygon coordinate
c   xcra          real         input     x coordinates polygon points
c   ycra          real         input     y coordinates polygon points
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
      integer   iaia (*)
      integer   igia (*)
      integer   land
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
c     ..fill land only
c
      land = mdipan (iaia(1), 'Land')
      if (land .ne. 0) then
         call gfa ((ncra - 1), xcra, ycra)
      endif
c
      return
      end
