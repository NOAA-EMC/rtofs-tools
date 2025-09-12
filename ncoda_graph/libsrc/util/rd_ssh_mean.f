      subroutine rd_ssh_mean (clim_dir, n_obs, n_data, lat, lon, mean)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  rd_ssh_mean
c
c DESCRIPTION:  reads HYCOM ssh mean and spatially interpolates mean
c               to the analysis field (or random obs locations)
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libocnqc.a
c      
c PARAMETERS:
c       Name         Type        Usage           Description
c   ------------    --------    -------   ---------------------------
c   clim_dir        char        input     data base file directory path
c   lat             real        input     grid (obs) latitudes
c   lon             real        input     grid (obs) longitudes
c   mean            real        output    HYCOM ssh mean
c   n_data          integer     input     number obs / grid nodes
c   n_obs           integer     input     max number obs / grid nodes
c
c....................MAINTENANCE SECTION................................
c
c MODULES CALLED:
c      Name                    Description
c   ----------     -------------------------------------
c   error_exit     standard error processing
c
c..............................END PROLOGUE.............................
c
      implicit none
c
      integer    UNIT
      parameter (UNIT = 20)
c
c     ..set number file latitudes, longitudes and nodes
c
      integer    N_LAT
      parameter (N_LAT = 1441)
c
      integer    N_LON
      parameter (N_LON = 2881)
c
      integer    N_NODES
      parameter (N_NODES = N_LAT * N_LON)
c
c     ..set starting base latitude and longitude
c
      real       LAT1
      parameter (LAT1 = -90.)
c
      real       LON1
      parameter (LON1 = 0.)
c
c     ..set file grid mesh attributes
c
      real       X_MESH, Y_MESH
      parameter (X_MESH = 0.125)
      parameter (Y_MESH = 0.125)
c
c     ..define missing value parameter
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
      real      lon (n_obs)
      integer   len_file
      real      mean (n_obs)
      integer   n_data
      integer   n_fail
      integer   n_strt
      real      x_lon
      real      y_lat
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
      allocate (wrk (N_NODES))
      allocate (xi (n_obs))
      allocate (yj (n_obs))
c
c     ..build ssh mean file name
c
      file_name = trim (clim_dir) // '/HYCOM.ssh_mean'
      len_file = len_trim (file_name)
c
c     ..check for file existence, read field
c
      inquire (file=file_name(1:len_file), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len_file), status='old',
     *               form='unformatted')
         read (UNIT) wrk
         close (UNIT)
      else
         write (err_msg, '(''HYCOM ssh mean file "'', a,
     *                     ''" missing'')') trim (file_name)
         call error_exit ('RD_SSH_MEAN', err_msg)
      endif
c
c     ..calculate grid positions
c
      do i = 1, n_data
         if (lon(i) .lt. 0.) then
            x_lon = lon(i) + 360.
         else if (lon(i) .gt. 360.) then
            x_lon = lon(i) - 360.
         else
            x_lon = lon(i)
         endif
         y_lat = lat(i)
         xi(i) = (x_lon - LON1) / X_MESH + 1.
         yj(i) = (y_lat - LAT1) / Y_MESH + 1.
      enddo
c
c     ..spatial interpolation
c
      n_strt = 1
      call fld2_trp (n_obs, n_strt, n_data, xi, yj, N_LON, N_LAT,
     *               wrk, ZDUM, mean, fail)
c
c     ..check for interpolation failures
c
      n_fail = 0
      do i = 1, n_data
         if (fail(i)) then
            mean(i) = 0.
            n_fail = n_fail + 1
         endif
      enddo
c
c     ..diagnostics
c
      write (*, '(14x, ''file name: '', a)') trim (file_name)
      write (*, '(''     retrieval failures: '', i10)') n_fail
c
c     ..clean up
c
      deallocate (fail, wrk, xi, yj)
c
      return
      end
