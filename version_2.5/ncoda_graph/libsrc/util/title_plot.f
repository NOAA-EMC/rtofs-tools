      subroutine title_plot (title, siz, ic, stack)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  title_plot
c
c DESCRIPTION:  plot a title card above the user's plot window
c
c PARAMETERS:
c      Name         Type        Usage             Description
c   ----------   ----------    -------    ----------------------------
c    ic           integer       input     color index for plot title
c    siz          real          input     character size
c    stack        real          input     relative position of title
c    title        character     input     title card
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
      real      dbos
      integer   ic
      integer   iqua
      integer   len
      integer   ll
      real      siz
      real      stack
      real      szfs
      character title * (*)
      real      xvpl, xvpr, xwdl, xwdr
      real      ypos
      real      yvpb, yvpt, ywdb, ywdt
c
c...............................executable..............................
c
c     ..set color index to input value
c
      call gsplci (ic)
c
c     ..determine string length of title card
c
      len = len_trim (title)
c
c     ..retrieve current set of set parameters
c
      call getset (xvpl, xvpr, yvpb, yvpt, xwdl, xwdr, ywdb,
     *             ywdt, ll)
c
c     ..scale size of title to fit within plot window
c
      szfs = siz * (xvpr - xvpl)
c
c     ..call set to achieve 0 to 1 plotter coordinates
c
      call set (0., 1., 0., 1., 0., 1., 0., 1., 1)
c
c     ..retrieve current value of quality flag
c
      call pcgeti ('QU - quality flag', iqua)
c
c     ..set quality flag to high quality complex character set
c
      call pcseti ('QU - quality flag', 0)
c
c     ..retrieve text extent parameters for positioning title string
c
      call pcseti ('TE - text extent computation flag', 1)
      call plchhq (.5, .5, title(1:len), szfs, 360., 0.)
      call pcgetr ('DB - distance to bottom of string', dbos)
c
c     ..plot title using high quality character set
c
      ypos = yvpt + (szfs + dbos) * stack
      call plchhq (((xvpl + xvpr) * 0.5), ypos, title(1:len),
     *             szfs, 0., 0.)
c
c     ..reset quality flag to input value
c
      call pcseti ('QU - quality flag', iqua)
c
c     ..reset set parameters to what was set before
c
      call set (xvpl, xvpr, yvpb, yvpt, xwdl, xwdr, ywdb, ywdt, ll)
c
c     ..reset color index to default foreground
c
      call gsplci (1)
c
      return
      end
