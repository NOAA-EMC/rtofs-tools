      subroutine err_ser (parm, n_fcst, n_tau, n, anl_dtg, anl_time,
     *                    upd, bias, rms, cnt, var, var_lbl, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  err_ser
c
c DESCRIPTION:  plots forecast error time series
c
c PARAMETERS:
c       Name         Type        Usage            Description
c   ------------    ---------    ------    ---------------------------
c   anl_dtg         character    input     analysis date time groups
c   anl_time        real         input     analysis continuous time
c   bias            real         input     analysis bias stats
c   cnt             real         input     obs data counts
c   domain          character    input     regional domain name
c   n               integer      input     starting time periods
c   n_fcst          integer      input     number forecasts
c   n_tau           integer      input     number analysis times
c   parm            character    input     analysis parameter
c   rms             real         input     analysis rms stats
c   upd             integer      input     update cycle interval
c   var             integer      input     variable number
c   var_lbl         character    input     variable name
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
      parameter (MX_CLR = 13)
c
c     ..local array dimension
c
      integer   n_fcst
      integer   n_tau
c
      character anl_dtg (n_tau) * 10
      real      anl_time (n_tau)
      real      bias (n_tau, n_fcst)
      integer   color (n_fcst)
      real      cnt (n_tau, n_fcst)
      integer   dash_patrn
      character date * 6
      character date_beg * 9
      character date_end * 9
      integer   day
      real      del
      integer   fno
      real      half
      integer   i, j, k
      integer   iasf (13)
      character label (n_fcst) * 3
      character lbl6 * 6
      real      left
      integer   len
      integer   lntr
      integer   mon
      character month (12) * 3
      integer   n
      integer   nf
      character parm * (*)
      real      rgb (3, 0:MX_CLR)
      real      rms (n_tau, n_fcst)
      real      tck
      character time1 * 4, time2 * 4, time3 * 4
      character title * 256
      character unit * 5
      integer   upd
      integer   var
      character var_lbl * (*)
      character watch * 2
      real      xmn, xmx
      real      ymn, ymx
      integer   year
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
     *   1.000, 0.000, 1.000,
     *   0.000, 0.467, 1.000,
     *   0.137, 1.000, 0.859,
     *   0.576, 1.000, 0.420,
     *   0.890, 1.000, 0.106,
     *   1.000, 0.733, 0.000,
     *   1.000, 0.294, 0.000,
     *   0.980, 0.000, 0.000,
     *   0.569, 0.000, 0.000 /
c
      data iasf / 13 * 1 /
c
c...............................executable..............................
c
c     ..form beginning and ending date labels
c
      read (anl_dtg(n)(3:8), '(3i2)') year, mon, day
      write (date_beg, '(i2, 1x, a, 1x, i2.2)') day, month(mon), year
      date_beg = adjustl (date_beg)
      read (anl_dtg(n_tau)(3:8), '(3i2)') year, mon, day
      write (date_end, '(i2, 1x, a, 1x, i2.2)') day, month(mon), year
c
c     ..set color scheme
c
      call gsclip (0)
      call gsasf (iasf)
      do i = 0, MX_CLR
         call gscr (1, i, rgb(1,i), rgb(2,i), rgb(3,i))
      enddo
c
c     ..set dashed line pattern
c
      dash_patrn = ishift (30840, 1)
      call dashdb (dash_patrn)
c
c     ..build dtg time check variables for labels
c
      watch = anl_dtg(n)(9:10)
      if (n_tau .le. 60) then
         time1 = '10' // watch
         time2 = '20' // watch
         time3 = '30' // watch
      else
         time1 = '01' // watch
         time2 = 'XX' // watch
         time3 = 'XX' // watch
      endif
c
c     ..set max number forecasts
c
      nf = 168 / upd + 1
      if (nf .gt. 8) then
         left = 0.2 
      else
         left = 0.5
      endif
c
c-----------------------------------------------------------------------
c
c     ..call set with min/max of time (x) and bias (y)
c
      xmn = anl_time(n)
      xmx = anl_time(n_tau)
      if (parm .eq. 'ICE') then
         unit = '(%)  '
         ymn = -2.
         ymx =  2.
         del = 1.
         tck = 0.5
      else if (parm .eq. 'MVO') then
         if (var .eq. 1) then
            unit = '(C)  '
            ymn = -2.
            ymx =  2.
            del = 1.
            tck = 1.
         else if (var .eq. 2) then
            unit = '(PSU)'
            ymn = -0.2
            ymx =  0.2
            del = 0.1
            tck = 0.1
         endif
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
c     ..mark forecast bias
c
      do k = 1, n_fcst
         call gsplci (k+5)
         color(k) = k+5
         call setusv ('LW', 3000)
         if (cnt(n,k) .gt. 0.) then
            call frstpt (anl_time(n), bias(n,k))
         else
            call frstpt (anl_time(n), 0.)
         endif
         do i = n, n_tau
            if (cnt(i,k) .gt. 0.) then
               call vector (anl_time(i), bias(i,k))
               call plchhq (anl_time(i), bias(i,k), 'o', .01, 0., 0.)
            else
               if (i .lt. n_tau) then
                  if (cnt(i+1,k) .gt. 0.) then
                     call frstpt (anl_time(i+1), bias(i+1,k))
                  endif
               endif
            endif
         enddo
         call setusv ('LW', 1000)
         call plotit (0, 0, 0)
      enddo
c
c     ..put label bar below plot
c
      lntr = 1 
      do k = 1, n_fcst
         label(k) = '   '
      enddo
      do k = 1, n_fcst, lntr
         write (label(k), '(i3)') (k-1) * upd
      enddo
      call gsplci (1)
      call gsfais (1)
      call lbseti ('CBL - color boxlines', 1)
      call lbseti ('CLB - color labels', 1)
      call lblbar (0, left, .90, .33, .40, nf, 1., .20,
     *             color, 0, label, nf, 1)
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
      do i = (n + 1), (n_tau - 1)
        call line (anl_time(i), ymn, anl_time(i),
     *            (ymn-0.025*(ymx-ymn)))
      enddo
      do i = (n+5), (n_tau-5)
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
      xmx = anl_time(n_tau)
      ymn = 0.
      if (parm .eq. 'ICE') then
         unit = '(%)  '
         ymx = 20.
         del = 5.
         tck = 5.
      else if (parm .eq. 'MVO') then
         if (var .eq. 1) then
            unit = '(C)  '
            ymx = 2.
            del = 1.
            tck = 0.5
         else if (var .eq. 2) then
            unit = '(PSU)'
            ymx = 1.
            del = 0.5
            tck = 0.25
         endif
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
c     ..mark forecast rms
c
      do k = 1, n_fcst
         call gsplci (k+5)
         color(k) = k+5
         call setusv ('LW', 3000)
         if (cnt(n,k) .gt. 0.) then
            call frstpt (anl_time(n), rms(n,k))
         else
            call frstpt (anl_time(n), 0.)
         endif
         do i = n, n_tau
            if (cnt(i,k) .gt. 0.) then
               call vector (anl_time(i), rms(i,k))
               call plchhq (anl_time(i), rms(i,k), '+', .01, 0., 0.)
            else
               if (i .lt. n_tau) then
                  if (cnt(i+1,k) .gt. 0.) then
                     call frstpt (anl_time(i+1), rms(i+1,k))
                  endif
               endif
            endif
         enddo
         call setusv ('LW', 1000)
         call plotit (0, 0, 0)
      enddo
c
c     ..mark selected rms lines as dashed 
c
      call gsplci (1)
      call lined (xmn, half, xmx, half)
c
      call line (xmn, ymn, xmn, (ymn-0.05*(ymx-ymn)))
      call line (xmx, ymn, xmx, (ymn-0.05*(ymx-ymn)))
      do i = (n + 1), (n_tau - 1)
        call line (anl_time(i), ymn, anl_time(i),
     *            (ymn-0.025*(ymx-ymn)))
      enddo
      do i = (n + 5), (n_tau - 5)
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
      len = len_trim (var_lbl)
      title = var_lbl(1:len) // ' Forecast Verification'
      call title_plot (title, .022, 1, 1.0)
      call plotit (0, 0, 0)
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (title)
c
c-----------------------------------------------------------------------
c
c     ..call set with min/max of time (x) and data counts (y)
c
      xmn = anl_time(n)
      xmx = anl_time(n_tau)
      ymn = 0.
      ymx = maxval (cnt)
      ymx = real (int (ymx / 1000.) + 1) * 1000. 
      call set (.10, .90, .10, .30, xmn, xmx, ymn, ymx, 1)
c
c     ..label data count axis
c
      call gsplci (1)
      write (lbl6, '(i6)') nint (ymn)
      call plchhq ((xmn - 0.011*(xmx-xmn)), ymn, lbl6,
     *             .011, 0., 1.)
      write (lbl6, '(i6)') nint (ymx)
      call plchhq ((xmn - 0.011*(xmx-xmn)), ymx, lbl6,
     *             .011, 0., 1.)
      half = (ymx + ymn) * 0.5
      call lined (xmn, half, xmx, half)
c
c     ..mark data count values
c
      call gsplci (3)
      call setusv ('LW', 3000)
      if (cnt(n,1) .gt. 0.) then
         call frstpt (anl_time(n), cnt(n,1))
      else
         call frstpt (anl_time(n), 0.)
      endif
      do i = n, n_tau
         if (cnt(i,1) .gt. 0.) then
            call vector (anl_time(i), cnt(i,1))
            call plchhq (anl_time(i), cnt(i,1), 'o', .01, 0., 0.)
         else
            call vector (anl_time(i), 0.)
            call plchhq (anl_time(i), 0., 'o', .01, 0., 0.)
         endif
      enddo
      call setusv ('LW', 1000)
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
      do i = (n + 1), (n_tau - 1)
         call line (anl_time(i), ymn, anl_time(i),
     *             (ymn-0.025*(ymx-ymn)))
      enddo
      do i = (n + 5), (n_tau - 5)
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
c     ..label plot
c
      title = 'Data Counts'
      len = len_trim (title)
      call plchhq (xmn, (ymx+.10*(ymx-ymn)), title(1:len),
     *             .014, 0., -1.)
c
c     ..advance the plot buffer
c
      call frame
c
      return
      end
