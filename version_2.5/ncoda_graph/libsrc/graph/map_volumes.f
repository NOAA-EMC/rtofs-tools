      subroutine map_volumes (n_lon, n_lat, mask, n_vol, vol_x1, vol_x2,
     *                        vol_y1, vol_y2, n_data, obs_xi, obs_yj)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_volumes
c
c DESCRIPTION:  routine to plot the analysis volumes overlaid on
c               the observation distribution in the analysis area.
c               the analysis area is defined in (i,j) space 
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libgraphics.a
c
c PARAMETERS:
c     Name         Type       Usage            Description
c   ---------    --------    -------    -----------------------------
c   mask         integer     input      grid mask
c   n_data       integer     input      number observations
c   n_lat        integer     input      number grid latitudes
c   n_lon        integer     input      number grid longitudes
c   n_vol        integer     input      number analysis volumes
c   obs_xi       real        input      observation latitude index
c   obs_yj       real        input      observation longitude index
c   vol_x1       real        input      volume left corner
c   vol_x2       real        input      volume right corner
c   vol_y1       real        input      volume bottom corner
c   vol_y2       real        input      volume top corner
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c MAKEFILE:   ...ops/ocn/otis/src/sub/Makefile
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..local array dimensions
c
      integer   n_data
      integer   n_lat
      integer   n_lon
      integer   n_vol
c
      real      f
      integer   i, j
      integer   mask (n_lon, n_lat)
      real      obs_xi (n_data)
      real      obs_yj (n_data)
      real      pos1, pos2, pos3, pos4
      real      r
      real      vol_x1 (n_vol)
      real      vol_x2 (n_vol)
      real      vol_y1 (n_vol)
      real      vol_y2 (n_vol)
      real      xcen, ycen
      real      xi, yj
      real      xmn, xmx
      real      ymn, ymx
c
c...............................executable..............................
c
c     ..set expanded plot background
c
      if (n_lon .gt. n_lat) then
         r = (real (n_lat) / real (n_lon)) * 0.75
         pos1 = 0.1
         pos2 = 0.9
         pos3 = 0.5 - r * (pos2 - pos1)
         pos4 = 0.5 + r * (pos2 - pos1)
         pos3 = max (pos3, 0.1)
         pos4 = min (pos4, 0.9)
      else
         r = (real (n_lon) / real (n_lat)) * 0.75
         pos3 = 0.1
         pos4 = 0.9
         pos1 = 0.5 - r * (pos4 - pos3)
         pos2 = 0.5 + r * (pos4 - pos3)
         pos1 = max (pos1, 0.1)
         pos2 = min (pos2, 0.9)
      endif
c
c     ..set reasonable aspect ratio
c
      f = 0.1
      xmn = 1. - f * real (n_lon)
      xmx = real (n_lon) + f * real (n_lon)
      ymn = 1. - f * real (n_lat)
      ymx = real (n_lat) + f * real (n_lat)
      call set (pos1, pos2, pos3, pos4, xmn, xmx, ymn, ymx, 1)
c
c     ..set plot size scalar
c
      if (n_lon .gt. 1200) then
         r = 0.5
      else
         r = 1.
      endif
c
c     ..mark grid mask points (brown)
c
      call gspmci (2)
      call gsmk (2)
      f = 0.1
      call gsmksc (f)
      do j = 1, n_lat
         yj = real (j)
         do i = 1, n_lon
            xi = real (i)
            if (mask(i,j) .eq. 0) then
               call gpm (1, xi, yj)
            endif
         enddo
      enddo
      call plotit (0, 0, 0)      
c
c     ..mark observation positions (black)
c
      call gspmci (1)
      call gsmk (1)
      f = 0.05
      call gsmksc (f)
c     do i = 1, n_data
c        call gpm (1, obs_xi(i), obs_yj(i))
c     enddo
      call plotit (0, 0, 0)
c
c     ..draw volume boundaries (blue)
c
      call gsplci (4)
      call setusv ('LW', 2000)
      f = r * 0.013
      do i = 1, n_vol
         call frstpt (vol_x1(i), vol_y1(i))
         call vector (vol_x2(i), vol_y1(i))
         call vector (vol_x2(i), vol_y2(i))
         call vector (vol_x1(i), vol_y2(i))
         call vector (vol_x1(i), vol_y1(i))
         call frstpt (vol_x1(i), vol_y1(i))
      enddo
      call plotit (0, 0, 0)
      call setusv ('LW', 1000)
c
c     ..mark volume centers (green)
c
      call gsplci (5)
      f = 0.005
      do i = 1, n_vol
         xcen = (vol_x1(i) + vol_x2(i)) * 0.5
         ycen = (vol_y1(i) + vol_y2(i)) * 0.5
         call plchhq (xcen, ycen, 'X', f, 0., 0.)
      enddo
      call plotit (0, 0, 0)
c
c     ..draw solid black lines around actual grid dimenions
c
      call gsplci (1)
      xmn = 1.
      xmx = real (n_lon)
      ymn = 1.
      ymx = real (n_lat)
      call line (xmn, ymn, xmx, ymn)
      call line (xmx, ymn, xmx, ymx)
      call line (xmx, ymx, xmn, ymx)
      call line (xmn, ymx, xmn, ymn)
      call plotit (0, 0, 0)
c
      return
      end
