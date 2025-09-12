      subroutine coda_xtnd (nx, ny, nz, fld, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  coda_xtnd
c
c DESCRIPTION:  performs fill procedure to extend masked fields into
c               the bottom and over land.  this is done to ensure no
c               missing values in the forecast model grids when input
c               into the analysis.  
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c      Name       Type       Usage             Description
c   ----------   ------     -------   ------------------------------
c   fld          real       in/out    masked/extended 3D field
c   nx           integer    input     number grid longitudes
c   ny           integer    input     number grid latitudes
c   nz           integer    input     number grid levels
c   spval        real       input     special (missing) value
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
c     ..set number iterations
c
      integer    MX_IT
      parameter (MX_IT = 10)
c
c     ..set number co-located grid locations (N_WT x N_WT)
c       must be an odd number
c
      integer    N_WT
      parameter (N_WT = 5)
c
c     ..set minimum number valid locations for extension
c
      integer    N_MIN
      parameter (N_MIN = 3)
c
c     ..local array dimensions
c
      integer   nx, ny, nz
c
      real      fld (nx, ny, nz)
      integer   i, j, k, l, m, n
      integer   ii, iw, jj, jw
      real      s, w
      real      spmis
      real      spval
      real      wrk (nx, ny)
      real      wt (N_WT, N_WT)
c
c...............................executable..............................
c
c     ..set weight array
c
      m = N_WT / 2
      do jw = 1, N_WT
         if (jw .le. m) then
            jj = jw
         else
            jj = N_WT - jw + 1
         endif
         do iw = 1, N_WT
            if (iw .le. m) then
               ii = iw
            else
               ii = N_WT - iw + 1
            endif
            k = ii + jj - 1
            wt(iw,jw) = real (k) / real (N_WT)
         enddo
      enddo
c
c     ..set spmis value
c
      spmis = spval + 9.
c
c     ..loop over levels
c
      do k = 1, nz
c
c        ..iterate over grid
c
         do l = 1, MX_IT
            do j = 1, ny
               do i = 1, nx
                  wrk(i,j) = fld(i,j,k)
               enddo
            enddo
c
c           ..loop over grid points with missing values
c
            do j = 1, ny
               do i = 1, nx
                  if (fld(i,j,k) .lt. spmis) then
c
c                    ..loop over averaging locations
c
                     n = 0
                     s = 0.
                     w = 0.
                     jw = N_WT - (min(j+m,ny) - max(1,j-m) + 1)
                     do jj = max(1,j-m), min(j+m,ny)
                        jw = jw + 1
                        iw = N_WT - (min(i+m,nx) - max(1,i-m) + 1)
                        do ii = max(1,i-m), min(i+m,nx)
                           iw = iw + 1
                           if (fld(ii,jj,k) .gt. spmis) then
                               n = n + 1
                               s = s + wt(iw,jw) * fld(ii,jj,k)
                               w = w + wt(iw,jw)
                           endif
                        enddo
                     enddo
c
c                    ..average result if adequate number of locations
c
                     if (n .gt. N_MIN) then
                        wrk(i,j) = s / w
                     endif
                  endif
               enddo
            enddo
c
c           ..update field with extended values
c
            do j = 1, ny
               do i = 1, nx
                  fld(i,j,k) = wrk(i,j)
               enddo
            enddo
         enddo
      enddo
c
      return
      end
