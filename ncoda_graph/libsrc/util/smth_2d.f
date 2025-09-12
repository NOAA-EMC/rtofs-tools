      subroutine smth_2d (n_pass, fld, n_lon, n_lat, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  smth_2d
c
c DESCRIPTION:  smooths a two dimensional field using a simple
c               weighting scheme avoiding grid nodes specified
c               as missing values.  global grids are assumed
c               to be cyclic on input.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c     Name         Type     Usage           Description
c   ---------    --------   -----    ------------------------------
c   fld          real       input    array to smooth
c   n_lat        integer    input    number grid latitudes
c   n_lon        integer    input    number grid longitudes
c   n_pass       integer    input    number passes through smoother
c   spval        real       input    missing value
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..local array dimensions
c
      integer   n_lat
      integer   n_lon
c
      real      fld (n_lon * n_lat)
      integer   i, j, m, n
      integer   ii, jj
      integer   n_pass
      real      spmis
      real      spval
      real      s, w
      real      wt (-2:2, -2:2)
c
c     ..allocatable arrays
c
      real,     allocatable :: dat (:,:)
      real,     allocatable :: wrk (:,:)
c
c     ..define filter weights
c
      data wt / 0.0, 0.5, 1.0, 0.5, 0.0,
     *          0.5, 1.0, 2.0, 1.0, 0.5,
     *          1.0, 2.0, 4.0, 2.0, 1.0,
     *          0.5, 1.0, 2.0, 1.0, 0.5,
     *          0.0, 0.5, 1.0, 0.5, 0.0 /
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (dat (n_lon, n_lat))
      allocate (wrk (n_lon, n_lat))
c
c     ..initialize work array
c
      spmis = spval + 9.
      do j = 1, n_lat
         do i = 1, n_lon
            m = n_lon * (j-1) + i
            dat(i,j) = fld(m)
         enddo
      enddo
c
c     ..loop over smoother passes
c
      do n = 1, n_pass
c
c        ..smooth data field storing results in work array
c
         do j = 1, n_lat
         do i = 1, n_lon
            if (dat(i,j) .gt. spmis) then
c
c              ..compute smoothed value
c
               s = 0.
               w = 0.
               do jj = max (-2, (2-j)), min (2, (n_lat-j))
               do ii = max (-2, (2-i)), min (2, (n_lon-i))
                  if (dat(i+ii,j+jj) .gt. spmis) then
                     s = s + wt(ii,jj) * dat(i+ii,j+jj)
                     w = w + wt(ii,jj)
                  endif
               enddo
               enddo
c
c              ..update data array
c
               if (w .gt. 0.) then
                  wrk(i,j) = s / w
               else
                  wrk(i,j) = dat(i,j)
               endif
            else
               wrk(i,j) = dat(i,j)
           endif
         enddo
         enddo
c
c        ..transfer work array to data array
c
         do j = 1, n_lat
            do i = 1, n_lon
               dat(i,j) = wrk(i,j)
            enddo
         enddo
      enddo
c
c     ..restore output array
c
      do j = 1, n_lat
         do i = 1, n_lon
            m = n_lon * (j-1) + i
            fld(m) = dat(i,j)
         enddo
      enddo
c
c     ..clean up
c
      deallocate (dat, wrk)
c
      return
      end
