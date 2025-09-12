      subroutine dtg_time (dtg, time, jday)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  dtg_time
c
c DESCRIPTION:  converts the date time group to the number of
c               hours since the year 1992
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c      Name            Type       Usage            Description
c   ----------      ----------   -------    ----------------------------
c   dtg              character    input     date time group (yymmddhh)
c   jday             integer      output    day of the year
c   time             real         output    hours since Jan 1, 1992
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
      implicit  none
c
      character dtg * 10 
      integer   day
      integer   hour
      integer   jday
      integer   month
      integer   month_day (12)
      real      time
      integer   year
c 
c     ..day of year to month table
c 
      data month_day /
     *     0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/ 
c
c...............................executable..............................
c 
c     ..decode dtg to year, month, day, hour
c 
      read (dtg, '(i4, 3i2)') year, month, day, hour
c 
c     ..compute day of the year
c 
      jday = month_day (month) + day
      if (mod (year,4) .eq. 0 .and. month .gt. 2) then
         jday = jday + 1
      endif
c 
c     ..compute binary time
c 
      year = year - 1992
      day = (year * 365) + ((year + 3) / 4) + (jday - 1)
      time = real (day * 24 + hour)
c 
      return
      end
