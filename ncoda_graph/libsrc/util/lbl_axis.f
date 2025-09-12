      subroutine lbl_axis (dmn, dmx, del, tck, axis_lbl, orient,
     *                     siz, off, flip)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c DESCRIPTION:  label an axis in a NCAR plot
c
c PARAMETERS:
c      Name        Type        Usage             Description
c   ----------    -------     -------    ------------------------------
c    axis_lbl      char        input     axis label character string
c    del           real        input     label interval
c    dmn           real        input     minimum value to plot
c    dmx           real        input     maximum value to plot
c    flip          logical     input     (true) reverse order of labels 
c    orient        real        input     axis orientation (0 or 90)
c    siz           real        input     character size
c    tck           real        input     tick mark interval
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
      character axis_lbl * (*)
      real      cn
      real      d
      real      del
      real      dlft, drht
      real      dmn, dmx
      logical   flip
      integer   id
      integer   j
      character lbl1*1, lbl2*2, lbl3*3, lbl4*4, lbl5*5, lbl6*6
      integer   len
      integer   ll
      integer   nd, nt
      real      off
      real      orient
      real      siz, szfs
      real      tck
      real      xel, xsl, yel, ysl
      real      xmid, ymid
      real      xpt, ypt
      real      xvpl, xvpr, xwdl, xwdr
      real      yvpb, yvpt, ywdb, ywdt
c
c...............................executable..............................
c
c     ..retrieve current set of set paramters
c
      call getset (xvpl, xvpr, yvpb, yvpt, xwdl, xwdr, ywdb, ywdt, ll)
c
c     ..scale size of titles to fit within plot window
c
      szfs = siz * (xvpr - xvpl)
c
c     ..call set to achieve 0 to 1 plotter coordinates
c
      call set (0., 1., 0., 1., 0., 1., 0., 1., 1)
c
c     ..initialize text extent computation flag
c
      call pcseti ('TE - text extent computation flag', 1)
c
c     ..plot the axis title
c
      len = len_trim (axis_lbl)
      if (int (orient) .gt. 0) then
         xpt = xvpl - off
         ymid = (yvpb + yvpt) * 0.5
         call plchhq (xpt, ymid, axis_lbl(1:len), szfs, 90., 0.)
      else
         xmid = (xvpl + xvpr) * 0.5
         ypt = yvpb - off
         call plchhq (xmid, ypt, axis_lbl(1:len), szfs, 0., 0.)
      endif
c
c     ..set tick mark sizes for axis labels
c
      if (int (orient) .gt. 0) then
         xsl = xvpl
         xel = xvpl - (xvpr - xvpl) * .01
      else
         ysl = yvpb
         yel = yvpb - (yvpt - yvpb) * .01
      endif
c
c     ..determine number labels to plot and starting label
c     
      if (del .gt. 0.) then
         nd = nint ((dmx - dmn) / del) + 1
      else
         nd = 0
      endif
      if (tck .gt. 0.) then
         nt = nint ((dmx - dmn) / tck) + 1
      else
         nt = 0
      endif
c
c     ..determine off-axis position of axis labels
c     
      call plchhq (.5, .5, '1', szfs, 360., 0.)
      call pcgetr ('DL - distance to left of string', dlft)
      call pcgetr ('DR - distance to rght of string', drht)
      d = drht + dlft
      if (int (orient) .gt. 0) then
         xpt = xvpl - szfs - d
         cn = 1.
      else
         ypt = yvpb - szfs - d
         cn = 0.
      endif
c     
c     ..place tick marks
c
      if (nt .gt. 0) then
         d = dmn - tck
         do j = 1, nt
            d = d + tck
            if (nint (d) .le. nint (dmx)) then
               if (int (orient) .gt. 0) then
                  if (flip) then
                     ypt = yvpt - ((yvpt-yvpb) * ((d-dmn) / (dmx-dmn)))
                  else
                     ypt = yvpb + ((yvpt-yvpb) * ((d-dmn) / (dmx-dmn)))
                  endif
                  call line (xsl, ypt, xel, ypt)
               else 
                  xpt = xvpl + ((xvpr-xvpl) * ((d-dmn) / (dmx-dmn)))
                  call line (xpt, ysl, xpt, yel)
               endif 
            endif
         enddo
      endif
c
c     ..label the axis
c
      if (nd .gt. 0) then
         d = dmn - del 
         do j = 1, nd
            d = d + del
            if (nint (d) .le. nint (dmx)) then
               if (int (orient) .gt. 0) then
                  if (flip) then
                     ypt = yvpt - ((yvpt-yvpb) * ((d-dmn) / (dmx-dmn)))
                  else
                     ypt = yvpb + ((yvpt-yvpb) * ((d-dmn) / (dmx-dmn)))
                  endif
               else
                  xpt = xvpl + ((xvpr-xvpl) * ((d-dmn) / (dmx-dmn)))
               endif
               if (mod (del, 1.) .eq. 0.) then
                  id = nint (d)
                  if (id .ge. 0 .and. id .lt. 10) then
                     write (lbl1, '(i1)') id
                     call plchhq (xpt, ypt, lbl1, szfs, 0., cn)
                  else if ((id .gt. -10 .and. id .lt. 0) .or.
     *                     (id .ge.  10 .and. id .lt. 100)) then
                     write (lbl2, '(i2)') id
                     call plchhq (xpt, ypt, lbl2, szfs, 0., cn)
                  else if ((id .gt. -100 .and. id .lt. 0) .or.
     *                     (id .ge.  100 .and. id .lt. 1000)) then
                     write (lbl3, '(i3)') id
                     call plchhq (xpt, ypt, lbl3, szfs, 0., cn)
                 else
                     write (lbl4, '(i4)') id
                     call plchhq (xpt, ypt, lbl4, szfs, 0., cn)
                  endif
               else
                  if (d .ge. 0. .and. d .lt. 10.) then
                     write (lbl3, '(f3.1)') d
                     call plchhq (xpt, ypt, lbl3, szfs, 0., cn)
                  else if ((d .gt. -10. .and. d .lt. 0.) .or.
     *                     (d .ge.  10. .and. d .lt. 100.)) then
                     write (lbl4, '(f4.1)') d
                     call plchhq (xpt, ypt, lbl4, szfs, 0., cn)
                  else if ((d .gt. -100. .and. d .lt. 0.) .or.
     *                     (d .ge.  100. .and. d .lt. 1000.)) then
                     write (lbl5, '(f5.1)') d
                     call plchhq (xpt, ypt, lbl5, szfs, 0., cn)
                 else
                     write (lbl6, '(f6.1)') d
                     call plchhq (xpt, ypt, lbl6, szfs, 0., cn)
                  endif
               endif
            endif
         enddo
      endif
c
c     ..reset set parameters
c
      call set (xvpl, xvpr, yvpb, yvpt, xwdl, xwdr, ywdb, ywdt, ll)
c
      return
      end
