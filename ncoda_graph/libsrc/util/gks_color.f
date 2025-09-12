      subroutine gks_color (rgb, n_clrs)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  gks_color
c
c DESCRIPTION:  routine to initialize gks and set up the color
c               index scheme.  the red-green-blue tuples are 
c               input and under user control
c
c PARAMETERS:
c    Name      Type      Usage            Description
c   ------    ------    -------    ----------------------------
c    rgb      real      input      red, green, blue tuples for
c                                  each color index in the range
c                                  0 to 1 for NCAR.  The zeroth
c                                  element is used as the
c                                  background and the 1st element
c                                  is used as the foreground.
c    n_clrs   integer   input      number of colors in the color
c                                  index table
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
      integer   iasf (13)
      integer   n_clrs
      real      rgb (3,0:n_clrs)
c
      data iasf / 13 * 1 /
c
c...............................executable..............................
c
c     ..prevent clipping at window boundaries
c
      call gsclip (0)
c
c     ..initialize source flags
c
      call gsasf (iasf)
c
c     ..set up color indices
c
      do i = 0, n_clrs
         call gscr (1, i, rgb(1,i), rgb(2,i), rgb(3,i))
      enddo
c
      return
      end
