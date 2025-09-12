      subroutine great_circle (start_lat, start_lon, end_lat, end_lon,
     *                         n_pts, lat_path, lon_path, dist_path)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c 
c MODULE NAME:  great_circle
c 
c DESCRIPTION:  this routine computes evenly-spaced latitude/longitude
c               points along a great circle.
c 
c PARAMETERS:
c       Name          Type     Usage            Description
c   -------------   --------   -----    -----------------------------
c    dist_path       real      output   distance in km along track
c    end_lat         real      input    end latitude of track
c    end_lon         real      input    end longitude of track
c    lat_path        real      output   latitudes of extraction track
c    lon_path        real      output   longitudes of extraction track
c    n_pts           integer   input    number of points along track
c    start_lat       real      input    start latitude of track
c    start_lon       real      input    start longitude of track
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
      implicit none
c
      real       deg_rad
      parameter (deg_rad = .017453293)
c
      real       e_rad
      parameter (e_rad = 6371.221)
c
      real       pi2
      parameter (pi2 = 1.570796327)
c
c     ..local array dimensions
c
      integer   n_pts
c
c     ..variables
c
      real      cosd
      real      cosr
      real      cost
      real      cost2
      real      dist_path
      real      distr
      real      dlon
      real      end_lat
      real      end_lon
      integer   i
      real      lat_path (n_pts)
      real      lon_path (n_pts)
      real      rlat1
      real      rlat2
      real      sind
      real      sinl
      real      sinr
      real      start_lat
      real      start_lon
      real      xinc
      real      xstep
c
c...............................executable..............................
c
c     ..set start/stop points in output arrays
c
      lat_path(1) = start_lat
      lon_path(1) = start_lon
      lat_path(n_pts) = end_lat
      lon_path(n_pts) = end_lon
c
c     ..convert longitudes from -180,180 range to 0,360 range
c
      if (lon_path(1) .lt. 0.) then
         start_lon = 360. + lon_path(1)
      else
         start_lon = lon_path(1)
      endif
      if (lon_path(n_pts) .lt. 0.) then
         end_lon = 360. + lon_path(n_pts)
      else
         end_lon = lon_path(n_pts)
      endif
c
c     ..convert to radians
c
      rlat1 = start_lat * deg_rad
      rlat2 = end_lat * deg_rad
      dlon = (start_lon - end_lon) * deg_rad
c    
c     ..compute angle between starting and ending points
c
      cosr = sin (rlat1) * sin (rlat2) + 
     *       cos (rlat1) * cos (rlat2) * cos (dlon)
      if (abs (cosr) .ge. 1.) then
         write (*, '(/, ''*** ERROR (GREAT_CIRCLE): invalid angle '',
     *                  ''between start and end points'')')
         return
      endif
c
c     ..compute distance along great circle track
c
      distr = acos (cosr)
      dist_path = distr * e_rad
c
c     ..compute variables needed for calculations
c
      rlat1 = pi2 - rlat1
      rlat2 = pi2 - rlat2
      dlon = -dlon
      sinr = sqrt (1. - cosr**2)
      sind = sin (rlat2) * sin (dlon) / sinr
      cosd = sqrt (1. - sind**2)
      cost = cosr * cos (rlat1) + sinr * sin (rlat1) * cosd
      cost2 = cosr * cos (rlat1) - sinr * sin (rlat1) * cosd
      if (abs (cost  - cos (rlat2)) .gt.
     *    abs (cost2 - cos (rlat2))) then
         cosd = -cosd
      endif
c
c     ..find arc length of step in radians
c
      xinc = distr / real (n_pts - 1)
c
c     ..compute the lats/lons of the points along the great circle
c
      do i = 2, (n_pts - 1)
         xstep = xinc * real (i - 1)
         cost = cos (xstep) * cos (rlat1) + 
     *          sin (xstep) * sin (rlat1) * cosd
         if (abs (cost) .ge. 1.) then
            cost = sign (1. - 1.e-13, cost)
         endif
         sinl = sind * sin (xstep) / sqrt (1. - cost**2)
c
c        ..convert from radians back to degrees
c
         lat_path(i) = 90. - acos (cost) / deg_rad
         lon_path(i) = asin (sinl) / deg_rad + start_lon
         lon_path(i) = amod ((lon_path(i) + 540.), 360.) - 180.
      enddo
c
c     ..reset start longitudes
c
      start_lon = lon_path(1)
      end_lon = lon_path(n_pts)
c
      return
      end
