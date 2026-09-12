      subroutine rd_topog (clim_dir, n_obs, n_data, lat, lon, val)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  rd_topog
c
c DESCRIPTION:  checks for and opens MODAS 2.1 smoothed bathymetry.
c               the smoothed bottom depth values are interpolated
c               to the obs locations and used to guide the use of
c               altimeter SSH data in shallow water. 
c
c PARAMETERS:
c       Name        Type      Usage             Description
c   -----------    --------   ------   -------------------------------
c   clim_dir       character  input    climate file directory path
c   lat, lon       real       input    obs locations
c   n_data         integer    input    number obs locations
c   n_obs          integer    input    max number obs
c   val            real       output   interpolated values
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c MAKEFILE:   ...ops/ocn/coda/src/sub/Makefile
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c 
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..define data file starting lat,lon and grid mesh
c
      real       LAT1
      parameter (LAT1 = -90.)
      real       LON1
      parameter (LON1 = 0.)
      real       MESH
      parameter (MESH = 0.25)
c
c     ..define data file dimensions
c
      integer    NX
      parameter (NX = 1440)
      integer    NY
      parameter (NY = 721)
c
      integer    UNIT
      parameter (UNIT = 20)
c
      real       ZDUM
      parameter (ZDUM = -999.)
c
c     ..local array dimension
c
      integer   n_obs
c
      character clim_dir * (*)
      character err_msg * 256
      logical   exist
      character file_name * 256
      integer   i
      real      lat (n_obs)
      integer   len
      integer   len_dir
      real      lon (n_obs)
      integer   n_data
      integer   n_skip
      integer   n_strt
      real      val (n_obs)
      real      xlon
      real      ylat
c
c     ..allocatable arrays
c
      logical,  allocatable :: fail (:)
      real,     allocatable :: wrk (:)
      real,     allocatable :: xi (:)
      real,     allocatable :: yj (:)
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (fail (n_obs))
      allocate (wrk (NX * NY))
      allocate (xi (n_obs))
      allocate (yj (n_obs))
c
c     ..build topo file name, check for existence, read database
c
      len_dir = len_trim (clim_dir)
      file_name = clim_dir(1:len_dir) // '/MODAS.topo'
      len = len_trim (file_name)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), form='unformatted',
     *               access='sequential', status='old')
         read (UNIT) wrk
         close (UNIT)
         do i = 1, (NX * NY)
            wrk(i) = abs (wrk(i))
         enddo
      else
         write (*, '(''MODAS smoothed bathymetry file "'', a,
     *          ''" missing'')') file_name(1:len)
         stop
      endif
c
c     ..form observation lat/lon offsets from grid origin
c
      do i = 1, n_data
         if (lon(i) .lt. 0.) then
            xlon = lon(i) + 360.
         else if (lon(i) .gt. 360.) then
            xlon = lon(i) - 360.
         else
            xlon = lon(i)
         endif
         ylat = lat(i)
         xi(i) = (xlon - LON1) / MESH + 1.
         yj(i) = (ylat - LAT1) / MESH + 1.
      enddo
c***************************************************
c     KLUDGE until data files are corrected
      do i = 1, n_data
         if (xi(i) .lt. 1.) xi(i) = 1.
         if (xi(i) .gt. real (NX)) xi(i) = real (NX)
      enddo
c***************************************************
c
c     ..spatial interpolation
c
      n_strt = 1
      call fld2_trp (n_obs, n_strt, n_data, xi, yj, NX, NY,
     *               wrk, ZDUM, val, fail)
c
c     ..check for missing, out of range values
c
      n_skip = 0
      do i = 1, n_data
         if (fail(i) .or. val(i) .lt. 0.) then
            n_skip = n_skip + 1
            val(i) = 0.
         endif
      enddo
c
c     ..diagnostics
c
      write (*, '(14x, ''file name: '', a)') trim (file_name)
      write (*, '(''     retrieval failures: '', i10)') n_skip
c
c     ..clean up
c
      deallocate (fail, wrk, xi, yj)
c
      return
      end
