      subroutine fld2_trp (n_obs, n_strt, n_data, xi, yj, n_lon,
     *                     n_lat, fld, zdum, value, fail)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  fld2_trp
c
c DESCRIPTION:   
c     Interpolates a two dimensional gridded field to an arbitray point.
c     A central difference interpolation is used for interior points
c     with data at all 16 surrounding grid nodes (i.e. no land points).
c     A bilinear interpolation is used near the grid boundary if the
c     surrounding points have data.  Otherwise, no interpolation is
c     performed and missing values are returned with the "fail" flag
c     set to true.  The point (xi,yj) is checked for being on the
c     grid.  Global grids are assumed to be cyclic on input. 
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c      
c PARAMETERS:
c     Name       Type      Usage             Description
c   --------    -------    ------    --------------------------------
c   fail        logical    output    (false) successful interpolation
c   fld         real       input     gridded fields array
c   n_data      integer    input     number observation to process
c   n_lon       integer    input     number i grid nodes in grid
c   n_lat       integer    input     number j grid nodes in grid
c   n_obs       integer    input     max number observations
c   n_strt      integer    input     starting observation number
c   value       real       output    interpolated values
c   xi          real       input     i grid coordinate of point
c   yj          real       input     j grid coordinate of point
c   zdum        real       input     missing value indicator
c
c....................MAINTENANCE SECTION................................
c
c LOCAL VARIABLES AND STRUCTURES:
c     Name       Type               Description
c   --------    -------    ------------------------------------
c   cntdif      logical    central difference interpolator flag
c   bilnr       logical    bilinear interpolator flag
c   value       real       interpolated observation
c   r, s        real       fractional parts of (xi, yj)
c   toprt       logical    point on top or right grid border
c
c METHOD:
c     The position of the point within the grid is checked.
c     If (xi,yj) >= (2,2) and (xi,yj) <= (n_lon-1,n_lat-1), 
c     it is an interior point and the central difference 
c     interpolation is selected. If xi = n_lon and/or 
c     yj = n_lat, then a linear interpolation of the border
c     points is selected.  Otherwise, a bilinear interpolation
c     is selected. Perform the interpolation, set fail flag
c     to false.  If no interpolation is performed, set return
c     values to missing values and set fail flags to true.
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
      integer   n_lat
      integer   n_lon
      integer   n_obs
c 
      logical   bilnr
      logical   cntdif
      real      del1, del2, del3
      real      f, f1, f2, f3
      logical   fail (n_obs)
      real      fld (n_lon, n_lat)
      real      fm1
      real      fr (4)
      integer   i, j
      integer   ix, jy
      integer   i1, i2
      integer   j1, j2
      integer   m, m1, m2
      integer   n, n1, n2
      integer   n_data
      integer   n_strt
      real      r, r1, r2, r3
      real      s, s1, s2, s3
      real      third
      logical   toprt
      real      u
      real      wt
      real      xi (n_obs)
      real      yj (n_obs)
      real      value (n_obs)
      real      zdum
      real      zmis
c 
c...............................executable..............................
c
c     ..initialize
c
      m1 = 1
      m2 = n_lon
      n1 = 1
      n2 = n_lat
      zmis = zdum + 9.
      third = 1. / 3.
c
c     ..loop over data
c
      do n = n_strt, n_data
c
c     ..ensure position is on the grid
c
      if (xi(n) .ge. real (m1) .and. xi(n) .le. real (m2) .and.
     *    yj(n) .ge. real (n1) .and. yj(n) .le. real (n2)) then
c
c     ..set grid node offsets
c
      ix = int (xi(n))
      jy = int (yj(n))
      r = xi(n) - real (ix)
      s = yj(n) - real (jy)
c 
c     ..check the position of the point within the grid
c 
      if (ix .gt. m1 .and. ix .lt. (m2-1) .and.
     *    jy .gt. n1 .and. jy .lt. (n2-1)) then
c 
c        ..interior zone
c 
         cntdif = .true.
         bilnr = .true.
         toprt = .false.
      else
         cntdif = .false.
         if (ix .lt. m2 .and. jy .lt. n2) then
            bilnr = .true.
            toprt = .false.
         else
            bilnr = .false.
            toprt = .true.
         endif
      endif
c
c     ..determine data availability
c
      if (toprt) then
c
c        ..top and/or right edge
c
         if (ix .eq. m2 .and. jy .eq. n2) then
            i1 = ix
            i2 = ix
            j1 = jy
            j2 = jy
            wt = 0.
         else if (ix .eq. m2) then
            i1 = ix
            i2 = ix
            j1 = jy
            j2 = jy + 1
            wt = s
         else if (jy .eq. n2) then
            i1 = ix
            i2 = ix + 1
            j1 = jy
            j2 = jy
            wt = r
         endif
c
c        ..check for valid data at grid nodes
c
         if (fld(i1,j1) .lt. zmis .or.
     *       fld(i2,j2) .lt. zmis) then
            toprt = .false.
         endif
      else
         if (cntdif) then
c
c           ..check surrounding 16 points
c
            i1 = ix - 1
            i2 = ix + 2
            j1 = jy - 1
            j2 = jy + 2
         else
c
c           ..check surrounding 4 points
c
            i1 = ix
            i2 = ix + 1
            j1 = jy
            j2 = jy + 1
         endif
c
         do j = j1, j2
            do i = i1, i2
               if (fld(i,j) .lt. zmis) then
                  cntdif = .false.
                  if ((i .eq. ix .or. i .eq. ix+1) .and.
     *                (j .eq. jy .or. j .eq. jy+1)) then
c
c                    ..one of the surrounding 4 points is missing
c
                     bilnr = .false.
                  endif
               endif
            enddo
         enddo
      endif
c
c-----------------------------------------------------------------------
c 
      if (cntdif) then
c
c        ..perform central difference interpolation
c
         r1 = r - 0.5 
         r2 = r * (r - 1.) * 0.5 
         r3 = r1 * r2 * third
c 
         s1 = s - 0.5 
         s2 = s * (s - 1.) * 0.5 
         s3 = s1 * s2 * third
c 
c        ..interpolate 4 rows (j-1,j,j+1,j+2) to xi and store
c          in fr(1) through fr(4)
c 
         m = 0 
         do j = jy - 1, jy + 2 
            m = m + 1
            fm1  = fld(ix-1,j)
            f    = fld(ix,j)
            f1   = fld(ix+1,j)
            f2   = fld(ix+2,j)
            u    = (f + f1) * 0.5
            del1 = f1 - f
            del2 = (f2 - f1 + fm1 - f) * 0.5
            del3 = f2 - f1 -  2. * del1  + f - fm1
            fr(m) = u + r1 * del1 + r2 * del2 + r3 * del3
         enddo
c 
c        ..interpolate the fr column to yj
c 
         u    = (fr(2) + fr(3)) * 0.5
         del1 =  fr(3) - fr(2)
         del2 = (fr(4) - fr(3) + fr(1) - fr(2)) * 0.5
         del3 =  fr(4) - fr(3) - 2.*del1 + fr(2) - fr(1)
c
         value(n) = u + s1 * del1 + s2 * del2 + s3 * del3
         fail(n) = .false.
      else if (bilnr) then
c 
c        ..perform bilinear interpolation
c 
         f  = fld(ix,jy)
         f1 = fld(ix+1,jy)
         f2 = fld(ix,jy+1)
         f3 = fld(ix+1,jy+1)
c
         value(n) = (1. - s) * ((1. - r) * f + r * f1) +
     *               s * ((1. - r) * f2 + r * f3)
         fail(n) = .false.
      else if (toprt) then
c
c        ..perform linear interpolation
c
         value(n) = (1. - wt) * fld(i1,j1) +
     *                    wt  * fld(i2,j2)
         fail(n) = .false.
      else 
c 
c        ..no interpolation possible, return missing values
c 
         value(n) = zdum
         fail(n) = .true.
      endif
c
      else
c
c     ..position is off grid, set missing and failure flags
c
      value(n) = zdum
      fail(n) = .true.
      endif
c
      enddo
c
      return
      end
