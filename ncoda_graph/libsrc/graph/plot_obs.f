      subroutine plot_obs (n_obs, n_data, obs_lon, obs_lat, obs_clr,
     *                     obs_siz, n_typ, typ_clr, typ_lbl, lft, 
     *                     rgt, title, igrid, rlat, stdlt1, stdlt2,
     *                     stdlon, bl, br, tl, tr, iamap, mx_amap,
     *                     fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_obs
c
c DESCRIPTION:  routine to plot the distribution of observation
c               innovations in the analysis area
c      
c PARAMETERS:
c     Name         Type       Usage            Description
c   ---------    --------    -------    ----------------------------
c   n_obs        integer     input      number of observations
c   obs_clr      integer     input      observation color
c   obs_lat      real        input      observation latitudes
c   obs_lon      real        input      observation longitudes
c   obs_typ      integer     input      observation data types
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
      integer    MX_TYP
      parameter (MX_TYP = 13)
c
c     ..local array dimensions
c
      integer   mx_amap
      integer   n_obs
c
      real      bl(2), br(2)
      integer   fno
      integer   i
      integer   iai (MX_AI)
      integer   iag (MX_AI)
      integer   iamap (mx_amap)
      integer   igrid
      real      lft
      integer   n_data
      integer   n_typ
      integer   obs_clr (n_obs)
      real      obs_lat (n_obs)
      real      obs_lon (n_obs)
      real      obs_siz (n_obs)
      real      pos1, pos2, pos3, pos4
      real      rgt
      real      rlat
      real      stdlon
      real      stdlt1, stdlt2
      character title * (*)
      real      tl(2), tr(2)
      integer   typ_clr (MX_TYP)
      character typ_lbl (MX_TYP) * 13
      real      u, v
      real      xcs (MX_LIN)
      real      ycs (MX_LIN)
c
c     ..external NCAR functions
c
      external  clin, colram
c
c     ..set color tables
c
      include 'color_table.h'
c
c     ..define plot position in frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
c...............................executable..............................
c
c     ..set color table and map background
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
      call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon, bl, br, tl,
     *              tr, pos1, pos2, pos3, pos4, iamap, mx_amap)
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
c     ..mark obs positions
c
      call pcseti ('FN - fontcap number', 20)
      do i = 1, n_data
         call maptra (obs_lat(i), obs_lon(i), u, v)
         if (u .lt. 1.e10) then
            call gsplci (obs_clr(i))
            call plchhq (u, v, 'L', obs_siz(i), 0., 0.)
            call plotit (0, 0, 0)
         endif
      enddo
      call pcseti ('FN - fontcap number', 0)
c
      if (n_typ .gt. 0) then
c
c        ..put a label bar on plot
c
         call gsfais (1)
         call lbseti ('CBL - color of box lines', 1)
         call lbseti ('CLB - color of labels', 1)
         call lblbar (0, lft, rgt, .0, .08, n_typ, 1., .25,
     *                typ_clr, 0, typ_lbl, n_typ, 1)
         call plotit (0, 0, 0)
      endif
c
c     ..put title on plot
c
      call title_plot (title, .02, 1, 1.)
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)') fno, trim (title)
      call frame
c
      return
      end
