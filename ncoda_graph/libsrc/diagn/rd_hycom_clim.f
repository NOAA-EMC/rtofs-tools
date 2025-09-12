      subroutine rd_hycom_clim (prm, opt, clm_dir, dtg, n_obs, n_data,
     *                          lat, lon, msk, val, rpt, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  rd_hycom_clim
c
c DESCRIPTION:  retrieves HYCOM surface climatology fields for
c               bracketing months and spatially interpolates
c               values to the input locations
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c      
c PARAMETERS:
c       Name         Type        Usage           Description
c   ------------    --------    -------   ---------------------------
c   clm_dir         char        input     climate directory path
c   dtg             char        input     analysis date time group
c   lat             real        input     output latitudes
c   lon             real        input     output longitudes
c   msk             integer     input     mask field
c   n_data          integer     input     number data / grid nodes
c   n_obs           integer     input     max number data / grid nodes
c   opt             char        input     processing option (sfc, std)
c   prm             char        input     processing variable
c   rpt             logical     input     (true) report results
c   spval           real        input     missing value
c   val             real        output    climate at output locations
c
c....................MAINTENANCE SECTION................................
c
c MODULES CALLED:
c      Name                    Description
c   ----------     -------------------------------------
c   error_exit     standard error processing
c
c METHOD:   HYCOM surface climatology files consists of monthly 1/12
c           degree resolution fields.  The bracketing months are read
c           and time interpolated to the day of the year and then 
c           spatially interpolated to the input locations. The origin
c           of the HYCOM climate grid is 90S, 0E with 4321 longitudes
c           (0E to 360W) and 2161 latitudes (90S to 90N).
c
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................END PROLOGUE.............................
c
      implicit none
c
c     ..surface climate file dimensions
c
      integer    N_CLM_LON, N_CLM_LAT
      parameter (N_CLM_LON = 4321)
      parameter (N_CLM_LAT = 2161)
c
c     ..surface climate file resolution
c
      real       CLM_MSH
      parameter (CLM_MSH = 1. / 12.)
c
c     ..surface climate file origin
c
      real       CLM_LON1, CLM_LAT1
      parameter (CLM_LON1 =   0.)
      parameter (CLM_LAT1 = -90.)
c
c     ..std offset parameter
c
      real       OFFSET
      parameter (OFFSET = 1.e-4)
c
      integer    UNIT
      parameter (UNIT = 22)
c
c     ..local array dimension
c
      integer   n_obs
c
      character clm_dir * (*)
      character dtg * 10
      character err_msg * 256
      logical   exist
      character file_name * 256
      real      lat (n_obs)
      real      lon (n_obs)
      integer   i
      integer   jday
      integer   len, len_dir
      integer   mon, mon1, mon2
      integer   mon_day (0:13)
      integer   msk (n_obs)
      integer   n_clm_nodes
      integer   n_data
      integer   n_strt
      character opt * (*)
      character prm * (*)
      integer   reclen
      logical   rpt
      character sfx * 64
      real      spval
      real      time
      real      val (n_obs)
      real      wt
c
c     ..allocatable arrays
c
      real,     allocatable :: clm (:)
      real,     allocatable :: clm1 (:)
      real,     allocatable :: clm2 (:)
      logical,  allocatable :: fail (:)
      real,     allocatable :: xi (:)
      real,     allocatable :: yj (:)
c
c     ..define mid points of months in day of the year format
c
      data      mon_day / -15,  15,  46,  75, 106, 136, 167,
     *                    196, 227, 258, 288, 319, 349, 380 /
c
c...............................executable..............................
c
c     ..determine month of the year
c
      read (dtg(5:6), '(i2)') mon
      if (mon .lt. 1 .or. mon .gt. 12) then
         write (err_msg, '(''invalid month in dtg "'', a, ''"'')') dtg
         call error_exit ('RD_HYCOM_CLIM', err_msg)
      endif
c
c     ..open climate file
c
      write (sfx, '(''HYCOM_'', a, ''.'', a)') trim (prm), trim (opt)
      file_name = trim (clm_dir) // '/' // trim (sfx)
      len = len_trim (file_name)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) then
         write (err_msg, '(''file missing: '', a)') trim (file_name)
         call error_exit ('RD_HYCOM_CLIM', err_msg)
      endif
c
      reclen = N_CLM_LON * N_CLM_LAT * 4
      open (UNIT, file=file_name(1:len), status='unknown',
     *            access='direct', form='unformatted',
     *            recl=reclen)
c
c     ..allocate arrays
c
      n_clm_nodes = N_CLM_LON * N_CLM_LAT
      allocate (clm  (N_CLM_LON * N_CLM_LAT))
      allocate (clm1 (N_CLM_LON * N_CLM_LAT))
      allocate (clm2 (N_CLM_LON * N_CLM_LAT))
      allocate (fail (n_obs))
      allocate (xi (n_obs))
      allocate (yj (n_obs))
c
c     ..compute day of the year
c
      call dtg_time (dtg, time, jday)
c
c     ..determine time interpolation weights
c
      if (jday .gt. mon_day(mon)) then
         mon1 = mon
         mon2 = mon + 1
      else
         mon1 = mon - 1
         mon2 = mon
      endif
      wt = real (mon_day(mon2) - jday) /
     *     real (mon_day(mon2) - mon_day(mon1))
c
c     ..ensure valid months for file access
c
      if (mon1 .lt. 1)  mon1 = 12
      if (mon2 .gt. 12) mon2 = 1
c
c     ..diagnostics
c
      if (rpt) then
         len_dir = len_trim (clm_dir)
         if (opt(1:3) .eq. 'sfc') then
            write (*, '(/, ''Retrieving HYCOM Climatology'')')
         else if (opt(1:3) .eq. 'std') then
            write (*, '(/, ''Retrieving HYCOM Climate '',
     *                     ''Variability'')')
         endif
         write (*, '(6x, ''   directory path: '', a)')
     *          clm_dir(1:len_dir)
         write (*, '(6x, ''bracketing months: '', 2i10)')
     *          mon1, mon2
         write (*, '(6x, ''    month weights: '', 2f10.2)')
     *          wt, (1. - wt)
         write (*, '(6x, '' climate variable: '', 7x, a)')
     *          trim (prm)
         write (*, '(6x, ''   climate option: '', 7x, a)')
     *          trim (opt)
      endif
c
c     ..read bracketing months
c
      read (UNIT, rec=mon1) clm1
      read (UNIT, rec=mon2) clm2
      close (UNIT)
c
c     ..time interpolate monthly fields
c
      do i = 1, n_clm_nodes
         clm(i) = wt * clm1(i) + (1. - wt) * clm2(i)
      enddo
c
c     ..form observation lat/lon offsets from HYCOM grid
c
      do i = 1, n_data
         if (msk(i) .gt. 0) then
            if (lon(i) .lt. 0.) then
               xi(i) = (lon(i)+360. - CLM_LON1) / CLM_MSH + 1.
            else if (lon(i) .gt. 360.) then
               xi(i) = (lon(i)-360. - CLM_LON1) / CLM_MSH + 1.
            else
               xi(i) = (lon(i) - CLM_LON1) / CLM_MSH + 1.
            endif
            yj(i) = (lat(i) - CLM_LAT1) / CLM_MSH + 1.
            if (yj(i) .lt. 1.) yj(i) = 1.
         else
            xi(i) = -1.
            yj(i) = -1.
         endif
      enddo
c
c     ..spatial interpolation of time interpolated climate values
c
      n_strt = 1
      call fld2_trp (n_obs, n_strt, n_data, xi, yj, N_CLM_LON,
     *               N_CLM_LAT, clm, spval, val, fail)
c
c     ..set valid values
c
      if (opt(1:3) .eq. 'sfc') then
         do i = 1, n_data
            if (msk(i) .gt. 0 .and. fail(i)) val(i) = 0.
         enddo
      else if (opt(1:3) .eq. 'std') then
         do i = 1, n_data
            if (val(i) .lt. 0.) then
               val(i) = OFFSET
            else
               val(i) = val(i) + OFFSET
            endif
         enddo
      endif 
c
c     ..clean up
c
      deallocate (clm, clm1, clm2, fail, xi, yj)
c
      return
      end
