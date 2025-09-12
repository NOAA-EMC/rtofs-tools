      subroutine lbl_xsect (start_lat, start_lon, end_lat, end_lon,
     *                      dist_path, siz)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c DESCRIPTION: label a cross section axis in a NCAR plot
c
c PARAMETERS:
c      Name         Type        Usage            Description
c   ----------     -------     -------    ---------------------------
c    siz            real        input     character size
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
      real      dbos
      real      dist_path
      real      end_lat
      real      end_lon
      character hemi * 1
      character lbl4 * 4
      character lbl5 * 5
      character lbl6 * 6
      character lbl8 * 8
      integer   ll
      real      siz
      real      start_lat
      real      start_lon
      real      szfs
      real      xpt
      real      xvpl, xvpr, xwdl, xwdr
      real      ypt
      real      yvpb, yvpt, ywdb, ywdt
c
c...............................executable..............................
c
c     ..set color index to foreground
c
      call gsplci (1)
c
c     ..retrieve current set of set paramters
c
      call getset (xvpl, xvpr, yvpb, yvpt, xwdl, xwdr, ywdb,
     *             ywdt, ll)
c
c     ..scale size of characters to fit within plot window
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
c     ..determine distance to bottom of strings
c
      call plchhq (.5, .5, '5', szfs, 360., 0.)
      call pcgetr ('DB - distance to bottom of string', dbos)
c
c     ..label the start latitude and longitude
c
      xpt = xvpl
      ypt = yvpb - szfs - dbos
      if (start_lat .lt. 0.) then
         hemi = 'S'
      else
         hemi = 'N'
      endif
      if (abs (start_lat) .ge. 0. .and. abs (start_lat) .lt. 10.) then
         write (lbl4, '(f3.1, a)') abs (start_lat), hemi
         call plchhq (xpt, ypt, lbl4, szfs, 0., -1.)
      else if (abs (start_lat) .ge. 10.) then
         write (lbl5, '(f4.1, a)') abs (start_lat), hemi
         call plchhq (xpt, ypt, lbl5, szfs, 0., -1.)
      endif
c
      xpt = xvpl
      ypt = yvpb - szfs - 4. * dbos
      if (start_lon .ge. 0. .and. start_lon .le. 180.) then
         hemi = 'E'
      else if (start_lon .lt. 0. .or. start_lon .gt. 180.) then
         hemi = 'W'
      else
         hemi = '?'
      endif
      if (abs (start_lon) .ge. 0. .and. abs (start_lon) .lt. 10.) then
         write (lbl4, '(f3.1, a)') abs (start_lon), hemi
         call plchhq (xpt, ypt, lbl4, szfs, 0., -1.)
      else if (abs (start_lon) .ge. 10. .and.
     *         abs (start_lon) .lt. 100.) then
         write (lbl5, '(f4.1, a)') abs (start_lon), hemi
         call plchhq (xpt, ypt, lbl5, szfs, 0., -1.)
      else if (abs (start_lon) .ge. 100.) then
         write (lbl6, '(f5.1, a)') abs (start_lon), hemi
         call plchhq (xpt, ypt, lbl6, szfs, 0., -1.)
      endif
c
c     ..label the end latitude and longitude
c
      xpt = xvpr
      ypt = yvpb - szfs - dbos
      if (end_lat .lt. 0.) then
         hemi = 'S'
      else
         hemi = 'N'
      endif
      write (lbl5, '(f4.1, a)') abs (end_lat), hemi
      call plchhq (xpt, ypt, lbl5, szfs, 0., 1.)
c
      xpt = xvpr
      ypt = yvpb - szfs - 4. * dbos
      if (end_lon .ge. 0. .and. end_lon .le. 180.) then
         hemi = 'E'
      else if (end_lon .lt. 0. .or. end_lon .gt. 180.) then
         hemi = 'W'
      else
         hemi = '?'
      endif
      write (lbl6, '(f5.1, a)') abs (end_lon), hemi
      call plchhq (xpt, ypt, lbl6, szfs, 0., 1.)
c
c     ..label distance along great circle path
c
      xpt = (xvpl + xvpr) * 0.5
      ypt = yvpb - szfs - 2. * dbos
      write (lbl8, '(i5, '' km'')') nint (dist_path)
      call plchhq (xpt, ypt, lbl8, szfs, 0., 0.)
c
c     ..restore set parameters to what was set before
c
      call set (xvpl, xvpr, yvpb, yvpt, xwdl, xwdr, ywdb, ywdt, ll)
c
      return
      end
