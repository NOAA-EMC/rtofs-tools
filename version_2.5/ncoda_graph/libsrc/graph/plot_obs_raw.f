      subroutine plot_obs_raw (title, igrid, rlat, stdlt1, stdlt2,
     *                         stdlon, bl, br, tl, tr, iamap,
     *                         mx_amap, pos)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_obs_raw
c
c DESCRIPTION:  sets up map background for plotting raw observations
c      
c PARAMETERS:
c     Name         Type       Usage            Description
c   ---------    --------    -------    ----------------------------
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
      parameter (MX_AI = 20 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 80 000)
c
c     ..local array dimensions
c
      integer   mx_amap
c
      real      bl(2), br(2)
      integer   iai (MX_AI)
      integer   iag (MX_AI)
      integer   iamap (mx_amap)
      integer   igrid
      real      pos (4)
      real      rlat
      real      stdlon
      real      stdlt1, stdlt2
      character title * (*)
      real      tl(2), tr(2)
      real      xcs (MX_LIN)
      real      ycs (MX_LIN)
c
c     ..external NCAR functions
c
      external  clin, colram
c
c...............................executable..............................
c
c     ..set map background
c
      call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon, bl, br, tl, tr,
     *              pos(1), pos(2), pos(3), pos(4), iamap, mx_amap)
c
c     ..color map land areas 
c
      call gsfais (1)
      call gsfaci (2)
      call arscam (iamap, xcs, ycs, MX_LIN, iai, iag, MX_AI, colram)
c
c     ..draw continent outlines
c
      call mplndr ('Earth..3', 1)
      call maplbl
c
c     ..plot title
c
      call title_plot (title, .03, 1, 1.)
c
      return
      end
