      subroutine prof_trp (nin, zin, xin, missing, nout, zout, xout)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  prof_trp
c
c DESCRIPTION:
c   Interpolates the input profile, (zin, xin), from the input levels,
c   zin, to the output levels, zout, using linear interpolation
c   between depths.  The interpolated values are returned in xout.
c
c   Missing values in the profiles are designated by "missing".
c   Missing values in the input profile are not interpolated.
c
c   Assumptions:
c      1) NO missing values in the depth arrays (zin, zout)
c      2) Both depth arrays (zin, zout) are monotonically increasing
c      3) There are no "missing" input values embedded among the
c         "good" values.  There CAN be missing values at the top or
c         bottom of the profile.
c      3) Output level 1 is greater than or equal to input level 1
c            (i.e. zout(1) >= zin(1)
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libocnqc.a
c      
c PARAMETERS:
c    NAME      TYPE      USAGE              DESCRIPTION
c   ------   ---------   -----     ----------------------------
c   nin       integer    input     number of observed depths
c   zin       real       input     input depths
c   xin       real       input     input parameter values
c   missing   real       input     missing value indicator
c   nout      integer    input     number of output depths
c   zout      real       input     output depth array
c   xout      real       output    output parameter values
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c   The following pseudo-code describes the processing in detail. In
c   summary the steps are:
c   1) find last input level with non-missing data
c   2) find the last "interpolatable" output level
c          (i.e. deepest depth such that depth <= last input level
c                with data)
c   3) for each "interpolatable" output level, find the appropriate
c      input level and interpolate
c
c   An "epsilon" value is used in all floating point comparisons that
c   may suffer from numerical round-off problems.
c
c MAKEFILE:   ...ops/ocn/coda/src/sub/Makefile
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................end prologue.............................
c
      implicit none
c
      real       epsilon
      parameter (epsilon = 1.e-4)
c
      integer   first_in
      integer   first_out
      integer   i
      integer   i_minus1
      integer   j
      integer   k
      integer   last_in
      integer   last_out
      real      missing
      integer   nin
      integer   nout
      real      xin (nin)
      real      xout (nout)
      real      zin (nin)
      real      zout (nout)
c
c...............................executable..............................
c
c     ..initialize output
c
      do i = 1, nout
         xout(i) = missing
      enddo
c
c     ..no depths to process
c
      if (nin .lt. 1  .or.  nout .lt. 1) then
         return
      endif
c
c     ..find the first index of the input profile with valid data
c
      first_in = nin + 1
      do i = 1, nin
         if (abs(xin(i) - missing) .ge. epsilon) then
            first_in = i
            go to 10
         endif
      enddo
   10 continue
c
c     ..find the last index of the input profile with valid data
c
      last_in = 0
      do i = nin, 1, -1
         if (abs(xin(i) - missing) .ge. epsilon) then
            last_in = i
            go to 20
         endif
      enddo
   20 continue
c
c     ..check for valid values
c
      if (first_in .gt. last_in) then
c
c        ..no valid input values - xout is already filled with missing
c
         return
c
      else if (first_in .eq. last_in) then
c
c        ..only one valid input value - set value at output depth
c
         do i = 1, nout
            if (abs(zout(i) - zin(first_in)) .lt. epsilon) then
               xout(i) = xin(first_in)
            endif
         enddo
c
         return
      endif
c
c     ..find the first output depth >= first input depth with data
c     .. - first output index that can be interpolated
c
      first_out = nout + 1
      do i = 1, nout
         if (zout(i) .ge. (zin(first_in) - epsilon)) then
            first_out = i
            go to 30
         endif
      enddo
   30 continue
c
c     ..find the last output depth <= last input depth with data
c     .. - last output index that can be interpolated in xout
c
      last_out = 0
      do i = nout, 1, -1
         if (zout(i) .le. (zin(last_in) + epsilon)) then
            last_out = i
c           last_out = i + 1
            go to 40
         endif
      enddo
   40 continue
c
c     ..check for "interpolatable" depths
c
      if (first_out .gt. last_out) then
         return
      endif
c
c     ..interpolate
c
      do j = first_out, last_out
c
c        ..find the closest input depth >= this output depth
c
         i = 0
         do k = first_in + 1, last_in
            if  (zin(k) .ge. (zout(j) - epsilon)) then
               i = k
               exit
            endif
         enddo
c
         if (i == 0) cycle
c
c        ..interpolate between (i - 1) and (i)
c
         i_minus1 = i - 1
c
         if (zin(i) .eq. zin(i_minus1)) then
            xout(j) = xin(i)
c
         else
            xout(j) = xin(i_minus1) + (xin(i) - xin(i_minus1)) *
     *         (zout(j) - zin(i_minus1)) / (zin(i) - zin(i_minus1))
         endif
      enddo
c
      return
      end
