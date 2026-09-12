       subroutine stats_ser (parm, var, n_taus, n, ks, ke, anl_dtg,
     *                       anl_time, bias_anl, bias_fcst, rms_anl,
     *                       rms_fcst, cnt, sys, var_lbl, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  stats_ser
c
c DESCRIPTION:  plots analysis verification time series
c
c PARAMETERS:
c       Name         Type        Usage            Description
c   ------------    ---------    ------    ---------------------------
c   anl_dtg         character    input     analysis date time groups
c   anl_time        real         input     analysis continuous time
c   bias_anl        real         input     analysis bias stats
c   bias_fcst       real         input     forecast bias stats
c   cnt             real         input     obs data counts
c   domain          character    input     regional domain name
c   ivar            integer      input     analysis variable number
c   ks, ke          integer      input     start/stop times
c   n               integer      input     starting time periods
c   rms_anl         real         input     analysis rms stats
c   rms_fcst        real         input     forecast rms stats
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
c     ..set number colors
c
      integer    MX_CLR
      parameter (MX_CLR = 5)
c
c     ..local array dimension
c
      integer   n_taus
c
      character anl_dtg (n_taus) * 10
      real      anl_time (n_taus)
      real      bias_anl (n_taus)
      real      bias_fcst (n_taus)
      integer   color (2)
      real      cnt (n_taus)
      integer   dash_patrn
      character date * 6
      character date_beg * 6
      character date_end * 6
      integer   day
      real      del
      integer   fno
      real      half
      integer   i, j
      integer   iasf (13)
      integer   ks, ke
      character label (2) * 12
      character lbl9 * 9
      integer   len
      integer   mon
      character month (12) * 3
      integer   n
      integer   parm
      real      rgb (3, 0:MX_CLR)
      real      rms_anl (n_taus)
      real      rms_fcst (n_taus)
      integer   sys
      real      tck
      character time1 * 4, time2 * 4, time3 * 4
      character title * 256
      character unit * 7
      integer   var
      character var_lbl * (*)
      character watch * 2
      real      xmn, xmx
      real      ymn, ymx
c
c     ..functions
c
      integer   ishift
c
c     ..set months of the year
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /
c
c     ..set color table
c
      data ((rgb(i,j), i = 1, 3), j = 0, MX_CLR) /
     *   1.000, 1.000, 1.000,
     *   0.000, 0.000, 0.000,
     *   1.000, 0.000, 0.000,
     *   0.000, 1.000, 0.000,
     *   0.000, 0.000, 1.000,
     *   1.000, 0.000, 1.000 /
c
      data iasf / 13 * 1 /
c
c...............................executable..............................
c
c     ..form beginning and ending date labels
c
      read (anl_dtg(n)(5:8), '(2i2)') mon, day
      write (date_beg, '(i2, 1x, a)') day, month(mon)
      date_beg = adjustl (date_beg)
      read (anl_dtg(ke)(5:8), '(2i2)') mon, day
      write (date_end, '(i2, 1x, a)') day, month(mon)
      date_end = adjustr (date_end)
c
c     ..set color scheme
c
      call gsclip (0)
      call gsasf (iasf)
      do i = 0, MX_CLR
         call gscr (1, i, rgb(1,i), rgb(2,i), rgb(3,i))
      enddo
      call setusv ('LW', 2000)
c
c     ..set dashed line pattern
c
      dash_patrn = ishift (30840, 1)
      call dashdb (dash_patrn)
c
c     ..build dtg time check variables for labels
c
      watch = anl_dtg(n)(9:10)
      if (ke .le. 60) then
         time1 = '10' // watch
         time2 = '20' // watch
         time3 = '30' // watch
      else
         time1 = '01' // watch
         time2 = 'XX' // watch
         time3 = 'XX' // watch
      endif
c
c-----------------------------------------------------------------------
c
c     ..call set with min/max of time (x) and bias (y)
c
      xmn = anl_time(n)
      xmx = anl_time(ke)
      if (parm .eq. 1 .and. var .eq. 1) then
c        ..sea ice
         unit = '(%)    '
         ymn = -10.
         ymx =  10.
         del = 5.
         tck = 5.
      else if (parm .eq. 2 .and. var .eq. 1) then
c        ..temperature
         unit = '(C)    '
         ymn = -1.
         ymx =  1.
         del = 0.5
         tck = 0.5
      else if (parm .eq. 2 .and. var .eq. 2) then
c        ..salinity
         unit = '(PSU)  '
         ymn = -1.0
         ymx =  1.0
         del = 0.5
         tck = 0.25
      endif
      half = ymx * 0.5
      call set (.10, .90, .45, .65, xmn, xmx, ymn, ymx, 1)
c
c     ..label bias axis
c
      call gsplci (1)
      title = 'Mean Bias ' // unit
      len = len_trim (title)
      call lbl_axis (ymn, ymx, del, tck, title(1:len), 90.,
     *               .014, .08, .false.)
c
c     ..draw border around plot
c
      call line (xmn, ymn, xmx, ymn)
      call line (xmn, ymn, xmn, ymx)
      call line (xmn, ymx, xmx, ymx)
      call line (xmx, ymn, xmx, ymx)
c
c     ..mark analysis bias
c
      call gsplci (4)
      color(1) = 4
      label(1) = 'Residuals'
      call setusv ('LW', 3000)
      if (cnt(n) .gt. 0.) then
         call frstpt (anl_time(n), bias_anl(n))
      else
         call frstpt (anl_time(n), 0.)
      endif
      do i = n, ke
         if (cnt(i) .gt. 0.) then
            call vector (anl_time(i), bias_anl(i))
            call plchhq (anl_time(i), bias_anl(i), '+', .01, 0., 0.)
         else
            if (i .lt. ke) then
               if (cnt(i+1) .gt. 0.) then
                  call frstpt (anl_time(i+1), bias_anl(i+1))
               endif
            endif
         endif
      enddo
      call setusv ('LW', 2000)
      call plotit (0, 0, 0)
c
c     ..mark forecast bias
c
      call gsplci (2)
      color(2) = 2
      label(2) = 'Innovations'
      call setusv ('LW', 3000)
      if (cnt(n) .gt. 0.) then
         call frstpt (anl_time(n), bias_fcst(n))
      else
         call frstpt (anl_time(n), 0.)
      endif
      do i = n, ke
         if (cnt(i) .gt. 0.) then
            call vector (anl_time(i), bias_fcst(i))
            call plchhq (anl_time(i), bias_fcst(i), 'o', .01, 0., 0.)
         else
            if (i .lt. ke) then
               if (cnt(i+1) .gt. 0.) then
                  call frstpt (anl_time(i+1), bias_fcst(i+1))
               endif
            endif
         endif
      enddo
      call setusv ('LW', 2000)
      call plotit (0, 0, 0)
c
c     ..put label bar below plot
c
      call gsplci (1)
      call gsfais (1)
      call lbseti ('CBL - color boxlines', 1)
      call lbseti ('CLB - color labels', 1)
      call lblbar (0, .65, .90, .33, .40, 2, 1., .20,
     *             color, 0, label, 2, 1)
c
c     ..mark beginning and ending time periods
c
      call gsplci (1)
      call line (xmn, ymn, xmn, (ymn-0.05*(ymx-ymn)))
      call plchhq (xmn, (ymn-0.12*(ymx-ymn)), date_beg,
     *             .011, 0., 0.)
      call line (xmx, ymn, xmx, (ymn-0.05*(ymx-ymn)))
      call plchhq (xmx, (ymn-0.12*(ymx-ymn)), date_end,
     *             .011, 0., 0.)
      do i = (n + 1), (ke - 1)
        call line (anl_time(i), ymn, anl_time(i),
     *            (ymn-0.025*(ymx-ymn)))
      enddo
      do i = (n+5), (ke-5)
         if (anl_dtg(i)(7:10) .eq. time1 .or.
     *       anl_dtg(i)(7:10) .eq. time2 .or.
     *       anl_dtg(i)(7:10) .eq. time3) then
            read (anl_dtg(i)(5:8), '(2i2)') mon, day
            write (date, '(i2, 1x, a)') day, month(mon)
            call plchhq (anl_time(i), (ymn-0.12*(ymx-ymn)),
     *                   date, .011, 0., 0.)
            call line (anl_time(i), ymn, anl_time(i),
     *                (ymn-0.05*(ymx-ymn)))
         endif
      enddo
c
c     ..mark dashed (and solid) bias lines
c
      call lined (xmn, -half, xmx, -half)
      call line  (xmn,   0.0, xmx,  0.0)
      call lined (xmn,  half, xmx,  half)
      call plotit (0, 0, 0)
c
c-----------------------------------------------------------------------
c
c     ..call set with min/max of time (x) and rms (y)
c
      xmn = anl_time(n)
      xmx = anl_time(ke)
      ymn = 0.
      if (parm .eq. 1 .and. var .eq. 1) then
c        ..sea ice
         unit = '(%)    '
         ymx =  20.
         del = 5.
         tck = 5.
      else if (parm .eq. 2 .and. var .eq. 1) then
c        ..temperature
         unit = '(C)    '
         ymx =  2.
         del = 0.5
         tck = 0.5
      else if (parm .eq. 2 .and. var .eq. 2) then
c        ..salinity
         unit = '(PSU)  '
         ymx =  1.0
         del = 0.5
         tck = 0.25
      endif
      half = ymx * 0.5
      call set (.10, .90, .70, .90, xmn, xmx, ymn, ymx, 1)
c
c     ..label rms axis
c
      call gsplci (1)
      title = 'RMS Error ' // unit
      len = len_trim (title)
      call lbl_axis (ymn, ymx, del, tck, title(1:len), 90.,
     *               .014, .08, .false.)
c
c     ..draw border around plot
c
      call line (xmn, ymn, xmx, ymn)
      call line (xmn, ymn, xmn, ymx)
      call line (xmn, ymx, xmx, ymx)
      call line (xmx, ymn, xmx, ymx)
c
c     ..mark analysis rms
c
      call gsplci (4)
      call setusv ('LW', 3000)
      if (cnt(n) .gt. 0.) then
         call frstpt (anl_time(n), rms_anl(n))
      else
         call frstpt (anl_time(n), 0.)
      endif
      do i = n, ke
         if (cnt(i) .gt. 0.) then
            call vector (anl_time(i), rms_anl(i))
            call plchhq (anl_time(i), rms_anl(i), '+', .01, 0., 0.)
         else
            if (i .lt. ke) then
               if (cnt(i+1) .gt. 0.) then
                  call frstpt (anl_time(i+1), rms_anl(i+1))
               endif
            endif
         endif
      enddo
      call setusv ('LW', 2000)
      call plotit (0, 0, 0)
c
c     ..mark forecast rms
c
      call gsplci (2)
      call setusv ('LW', 3000)
      if (cnt(n) .gt. 0.) then
         call frstpt (anl_time(n), rms_fcst(n))
      else
         call frstpt (anl_time(n), 0.)
      endif
      do i = n, ke
         if (cnt(i) .gt. 0.) then
            call vector (anl_time(i), rms_fcst(i))
            call plchhq (anl_time(i), rms_fcst(i), 'o', .01, 0., 0.)
         else
            if (i .lt. ke) then
               if (cnt(i+1) .gt. 0.) then
                  call frstpt (anl_time(i+1), rms_fcst(i+1))
               endif
            endif
         endif
      enddo
      call setusv ('LW', 2000)
      call plotit (0, 0, 0)
c
c     ..mark selected rms lines as dashed 
c
      call gsplci (1)
      call lined (xmn, half, xmx, half)
c
      call line (xmn, ymn, xmn, (ymn-0.05*(ymx-ymn)))
      call line (xmx, ymn, xmx, (ymn-0.05*(ymx-ymn)))
      do i = (n + 1), (ke - 1)
        call line (anl_time(i), ymn, anl_time(i),
     *            (ymn-0.025*(ymx-ymn)))
      enddo
      do i = (n + 5), (ke - 5)
         if (anl_dtg(i)(7:10) .eq. time1 .or.
     *       anl_dtg(i)(7:10) .eq. time2 .or.
     *       anl_dtg(i)(7:10) .eq. time3) then
            call line (anl_time(i), ymn, anl_time(i),
     *                (ymn-0.05*(ymx-ymn)))
         endif
      enddo
c
c     ..put title above plot
c
      var_lbl = adjustl (var_lbl)
      len = len_trim (var_lbl)
      title = trim (var_lbl(1:len)) // ' Verification'
      call title_plot (title, .022, 1, 1.0)
      call plotit (0, 0, 0)
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', i3x, 2x, a)')
     *       fno, sys, trim (title)
c
c-----------------------------------------------------------------------
c
c     ..call set with min/max of time (x) and data counts (y)
c
      xmn = anl_time(n)
      xmx = anl_time(ke)
      ymn = 0.
      ymx = maxval (cnt)
      ymx = real (int (ymx / 1000.) + 1) * 1000. 
      if (ymx .gt. 1000.) ymx = ymx + 1000.
      call set (.10, .90, .10, .30, xmn, xmx, ymn, ymx, 1)
c
c     ..label data count axis
c
      call gsplci (1)
      write (lbl9, '(i9)') nint (ymn)
      call plchhq ((xmn - 0.011*(xmx-xmn)), ymn, lbl9,
     *             .011, 0., 1.)
      write (lbl9, '(i9)') nint (ymx)
      call plchhq ((xmn - 0.011*(xmx-xmn)), ymx, lbl9,
     *             .011, 0., 1.)
      half = (ymx + ymn) * 0.5
      call lined (xmn, half, xmx, half)
      call plotit (0, 0, 0)
c
c     ..mark data count values
c
      call gsplci (3)
      call setusv ('LW', 3000)
      if (cnt(n) .gt. 0.) then
         call frstpt (anl_time(n), cnt(n))
      else
         call frstpt (anl_time(n), 0.)
      endif
      do i = n, ke
         if (cnt(i) .gt. 0.) then
            call vector (anl_time(i), cnt(i))
            call plchhq (anl_time(i), cnt(i), 'o', .01, 0., 0.)
         else
            call vector (anl_time(i), 0.)
            call plchhq (anl_time(i), 0., 'o', .01, 0., 0.)
         endif
      enddo
      call setusv ('LW', 2000)
      call plotit (0, 0, 0)
c
c     ..draw border around plot
c
      call gsplci (1)
      call line (xmn, ymn, xmx, ymn)
      call line (xmn, ymn, xmn, ymx)
      call line (xmn, ymx, xmx, ymx)
      call line (xmx, ymn, xmx, ymx)
c
c     ..mark beginning and ending time periods
c
      call line (xmn, ymn, xmn, (ymn-0.05*(ymx-ymn)))
      call plchhq (xmn, (ymn-0.12*(ymx-ymn)), date_beg,
     *             .011, 0., 0.)
      call line (xmx, ymn, xmx, (ymn-0.05*(ymx-ymn)))
      call plchhq (xmx, (ymn-0.12*(ymx-ymn)), date_end,
     *             .011, 0., 0.)
      do i = (n + 1), (ke - 1)
         call line (anl_time(i), ymn, anl_time(i),
     *             (ymn-0.025*(ymx-ymn)))
      enddo
      do i = (n + 5), (ke - 5)
         if (anl_dtg(i)(7:10) .eq. time1 .or.
     *       anl_dtg(i)(7:10) .eq. time2 .or.
     *       anl_dtg(i)(7:10) .eq. time3) then
            read (anl_dtg(i)(5:8), '(2i2)') mon, day
            write (date, '(i2, 1x, a)') day, month(mon)
            call plchhq (anl_time(i), (ymn-0.12*(ymx-ymn)),
     *                   date, .011, 0., 0.)
            call line (anl_time(i), ymn, anl_time(i),
     *                (ymn-0.05*(ymx-ymn)))
         endif
      enddo
      call plotit (0, 0, 0)
c
c     ..label plot
c
      title = 'Data Counts'
      len = len_trim (title)
      call plchhq (xmn, (ymx+.10*(ymx-ymn)), title(1:len),
     *             .014, 0., -1.)
      call plotit (0, 0, 0)
c
c     ..advance the plot buffer
c
      call frame
c
      return
      end
