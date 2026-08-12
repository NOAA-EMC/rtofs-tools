      subroutine fld3_trp (n_obs, n_strt, n_data, xi, yj, zk, n_lon,
     *                     n_lat, n_lvl, fld, zdum, value, fail)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  fld3_trp
c
c DESCRIPTION:   
c     Interpolates a three dimensional gridded field to an arbitray
c     point.  A central difference interpolation is used for interior
c     points with data at all 64 surrounding grid nodes (i.e. no land
c     points). A bilinear interpolation is used near the grid boundary
c     if the surrounding points have data. Otherwise, no interpolation
c     is performed and missing values are returned with the "fail"
c     flag set to true.  The point (xi,yj,zk) is checked for being on
c     the grid.  Global grids are assumed to be cyclic on input.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c      
c PARAMETERS:
c     Name       Type      Usage             Description
c   --------    -------    ------    --------------------------------
c   fail        logical    output    (false) successful interpolation
c   fld         real       input     gridded field array
c   n_data      integer    input     number observations to process
c   n_lon       integer    input     number i grid nodes in grid
c   n_lat       integer    input     number j grid nodes in grid
c   n_lvl       integer    input     number k grid levels in grid
c   n_obs       integer    input     max number observations
c   n_strt      integer    input     starting observation number
c   value       real       output    interpolated values
c   xi          real       input     i grid coordinate of point
c   yj          real       input     j grid coordinate of point
c   zk          real       input     k grid coordinate of point
c   zdum        real       input     missing value indicator
c
c....................MAINTENANCE SECTION................................
c
c LOCAL VARIABLES AND STRUCTURES:
c     Name       Type                Description
c   --------    -------    ------------------------------------
c   cntdif      logical    central difference interpolator flag
c   bilnr       logical    bilinear interpolator flag
c   value       real       interpolated observation
c   r, s, t     real       fractional parts of (xi, yj, zk)
c   toprt       logical    point on top or right grid border
c
c METHOD:
c     The position of the point within the grid is checked.
c     If (xi,yj) >= (2,2) and (xi,yj) <= (n_lon-1,n_lat-1), 
c     and (zk) >= (1) and (zk) <= (n_lvl-1), it is an
c     interior point and the central difference interpolation
c     is selected. If xi = n_lon and/or yj = n_lat, then a
c     linear interpolation of the border points is selected.
c     Otherwise, a bilinear interpolation is selected. Perform
c     the interpolation, set fail flag to false.  If no
c     interpolation is performed, set return values to missing
c     and set fail flags to true.
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
      integer   n_lvl
      integer   n_obs
c 
      logical   bilnr
      logical   cntdif
      real      del1, del2, del3
      real      f, f1, f2, f3
      logical   fail (n_obs)
      real      fld (n_lon, n_lat, n_lvl)
      real      fm1
      real      fr (n_lvl, 4)
      real      fs (n_lvl)
      integer   i, i1, i2
      integer   ix, jy, kz
      integer   j, j1, j2
      integer   k, k1, k2
      integer   l1, l2
      integer   m, m1, m2
      integer   n, n1, n2
      integer   n_data
      integer   n_strt
      real      r, r1, r2, r3
      real      s, s1, s2, s3
      real      t, t1, t2, t3
      real      third
      logical   toprt
      real      u
      real      xi (n_obs)
      real      yj (n_obs)
      real      zk (n_obs)
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
      l1 = 1
      l2 = n_lvl
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
     *    yj(n) .ge. real (n1) .and. yj(n) .le. real (n2) .and.
     *    zk(n) .ge. real (l1) .and. zk(n) .le. real (l2)) then
c
c     ..set grid node offsets
c
      ix = int (xi(n))
      jy = int (yj(n))
      kz = int (zk(n))
      r = xi(n) - real (ix)
      s = yj(n) - real (jy)
      t = zk(n) - real (kz)
c 
c     ..check the position of the point within the grid
c 
      if (ix .gt. m1 .and. ix .lt. (m2-1) .and.
     *    jy .gt. n1 .and. jy .lt. (n2-1) .and.
     *    kz .gt. l1 .and. kz .lt. (l2-1)) then
c 
c        ..interior zone
c 
         cntdif = .true.
         bilnr = .true.
         toprt = .false.
      else
         cntdif = .false.
         if (ix .lt. m2 .and. jy .lt. n2 .and. kz .lt. l2) then
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
            if (fld(ix,jy,kz)     .lt. zmis) toprt = .false.
         else if (ix .eq. m2) then
            if (fld(ix,jy,kz)     .lt. zmis) toprt = .false.
            if (fld(ix,jy+1,kz)   .lt. zmis) toprt = .false.
         else if (jy .eq. n2) then
            if (fld(ix,jy,kz)     .lt. zmis) toprt = .false.
            if (fld(ix+1,jy,kz)   .lt. zmis) toprt = .false.
         else
            if (fld(ix,jy,kz)     .lt. zmis) toprt = .false.
            if (fld(ix+1,jy,kz)   .lt. zmis) toprt = .false.
            if (fld(ix,jy+1,kz)   .lt. zmis) toprt = .false.
            if (fld(ix+1,jy+1,kz) .lt. zmis) toprt = .false.
         endif
      else
         if (cntdif) then
c
c           ..check surrounding 64 points
c
            i1 = ix - 1
            i2 = ix + 2
            j1 = jy - 1
            j2 = jy + 2
            k1 = kz - 1
            k2 = kz + 2
         else
c
c           ..check surrounding 8 points
c
            i1 = ix
            i2 = ix + 1
            j1 = jy
            j2 = jy + 1
            k1 = kz
            k2 = kz + 1
         endif
c
         do k = k1, k2
            do j = j1, j2
               do i = i1, i2
                  if (fld(i,j,k) .lt. zmis) then
                     cntdif = .false.
                     if ((i .eq. ix .or. i .eq. ix+1) .and.
     *                   (j .eq. jy .or. j .eq. jy+1) .and.
     *                   (k .eq. kz .or. k .eq. kz+1)) then
c
c                       ..one of the surrounding 8 points is missing
c
                        bilnr = .false.
                     endif
                  endif
               enddo
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
         t1 = t - 0.5
         t2 = t * (t - 1.) * 0.5
         t3 = t1 * t2 * third
c 
c        ..interpolate 4 rows (j-1,j,j+1,j+2) to xi and store
c          in fr(1) through fr(4)
c 
         m = 0 
         do j = jy - 1, jy + 2 
            m = m + 1
            do k = kz - 1, kz + 2
               fm1  = fld(ix-1,j,k)
               f    = fld(ix,j,k)
               f1   = fld(ix+1,j,k)
               f2   = fld(ix+2,j,k)
               u    = (f + f1) * 0.5
               del1 = f1 - f
               del2 = (f2 - f1 + fm1 - f) * 0.5
               del3 = f2 - f1 -  2. * del1  + f - fm1
               fr(k,m) = u + r1 * del1 + r2 * del2 + r3 * del3
            enddo
         enddo
c 
c        ..interpolate the fr column to yj
c 
         m = 0
         do k = kz - 1, kz + 2
            m = m + 1
            u    = (fr(k,2) + fr(k,3)) * 0.5
            del1 =  fr(k,3) - fr(k,2)
            del2 = (fr(k,4) - fr(k,3) + fr(k,1) - fr(k,2)) * 0.5
            del3 =  fr(k,4) - fr(k,3) - 2.*del1 + fr(k,2) - fr(k,1)
            fs(m) = u + s1 * del1 + s2 * del2 + s3 * del3
         enddo
c
c        ..interpolate in the vertical
c
         u    = (fs(2) + fs(3)) * 0.5
         del1 =  fs(3) - fs(2)
         del2 = (fs(4) - fs(3) + fs(1) - fs(2)) * 0.5
         del3 =  fs(4) - fs(3) - 2.*del1 + fs(2) - fs(1)
c
         value(n) = u + t1 * del1 + t2 * del2 + t3 * del3
         fail(n) = .false.
      else if (bilnr) then
c 
c        ..perform bilinear interpolation
c 
         m = 0
         do k = kz, kz + 1
            m = m + 1
            f  = fld(ix,jy,k)
            f1 = fld(ix+1,jy,k)
            f2 = fld(ix,jy+1,k)
            f3 = fld(ix+1,jy+1,k)
            fs(m) = (1. - s) * ((1. - r) * f + r * f1) +
     *               s * ((1. - r) * f2 + r * f3)
         enddo
c
         value(n) = (1. - t) * fs(1) + t * fs(2)
         fail(n) = .false.
      else if (toprt) then
c
c        ..perform linear interpolation at a grid boundary
c
         if (kz .eq. l2) then
            if (ix .eq. m2 .and. jy .eq. n2) then
               value(n) = fld(ix,jy,kz)
            else if (ix .eq. m2) then
               value(n) = (1. - s) * fld(ix,jy,kz) +
     *                          s  * fld(ix,jy+1,kz)
            else if (jy .eq. n2) then
               value(n) = (1. - r) * fld(ix,jy,kz) +
     *                          r  * fld(ix+1,jy,kz)
            else
               f  = fld(ix,jy,kz)
               f1 = fld(ix+1,jy,kz)
               f2 = fld(ix,jy+1,kz)
               f3 = fld(ix+1,jy+1,kz)
               value(n) = (1. - s) * ((1. - r) *  f + r * f1) +
     *                          s  * ((1. - r) * f2 + r * f3)
            endif
         else
            if (ix .eq. m2 .and. jy .eq. n2) then
               value(n) = (1.-t) * fld(ix,jy,kz) + t * fld(ix,jy,kz+1)
            else if (ix .eq. m2) then
               m = 0
               do k = kz, kz + 1
                  m = m + 1
                  fs(m) = (1. - s) * fld(ix,jy,k) +
     *                          s  * fld(ix,jy+1,k)
               enddo
               value(n) = (1. - t) * fs(1) + t * fs(2)
            else if (jy .eq. n2) then
               m = 0
               do k = kz, kz + 1
                  m = m + 1
                  fs(m) = (1. - r) * fld(ix,jy,k) +
     *                          r  * fld(ix+1,jy,k)
               enddo
               value(n) = (1. - t) * fs(1) + t * fs(2)
            else
               m = 0
               do k = kz, kz + 1
                  m = m + 1
                  f  = fld(ix,jy,k)
                  f1 = fld(ix+1,jy,k)
                  f2 = fld(ix,jy+1,k)
                  f3 = fld(ix+1,jy+1,k)
                  fs(m) = (1. - s) * ((1. - r) *  f + r * f1) +
     *                          s  * ((1. - r) * f2 + r * f3)
               enddo
               value(n) = (1. - t) * fs(1) + t * fs(2)
            endif
         endif
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
