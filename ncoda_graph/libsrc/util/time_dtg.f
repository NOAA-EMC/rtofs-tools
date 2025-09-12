      subroutine time_dtg (time, dtg)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  time_dtg
c
c DESCRIPTION:  converts the analysis time in the form of the number
c               of hours since 1992 to a date time group
c
c PARAMETERS:
c      Name            Type       Usage            Description
c   ----------      ----------   -------    --------------------------
c   dtg              character    output    date time group (yymmddhh)
c   time             real         input     hours since Jan 1, 1992
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
      integer   i
      integer   i_time
      integer   jday_month (13)
      integer   month
      integer   month_day (13)
      integer   n_feb29
      integer   n_days
      integer   n_leap
      real      time
      integer   year
c 
c     ..day of year to month table
c 
      data jday_month /
     *     0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 366/ 
c
c...............................executable..............................
c 
c     ..transfer month days to working storage
c
      do i = 1, 13
         month_day(i) = jday_month(i)
      enddo
c
c     ..compute year and hour of the day
c
      i_time = nint (time)
      n_days = i_time / 24
      hour = i_time - n_days * 24
      n_feb29 = (n_days + 1401) / 1461
      year = (n_days - n_feb29) / 365
c
c     ..set day of the year
c
      n_leap = (year + 3) / 4
      day = n_days - (year * 365 + n_leap) + 1
c
c     ..check for current year being leap year
c
      if (mod (year,4) .eq. 0) then
c
c        ..bump month days beyond february by one day
c
         do i = 3, 13
            month_day(i) = month_day(i) + 1
         enddo
      endif
c
c     ..find month of the year
c
      do i = 2, 13
         if (day .gt. month_day(i-1) .and.
     *       day .le. month_day(i)) then
            month = i - 1
         endif
      enddo
c
c     ..set day of the month and the year
c
      day = day - month_day(month)
      year = year + 1992
c
c     ..form date time group return argument
c
      write (dtg, '(i4, 3i2.2)') year, month, day, hour
c 
      return
      end
