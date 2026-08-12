      subroutine stats_fcst (src, dpth_lbl, var_lbl, N_FCST, N_TAUS,
     *                       n, ks, ke, anl_dtg, anl_time, bias_fcst,
     *                       rms_fcst, cnt, var, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  stats_ser
c
c DESCRIPTION:  plots forecast error verification time series for
c               different forecast periods
c
c PARAMETERS:
c       Name         Type        Usage            Description
c   ------------    ---------    ------    ---------------------------
c   anl_dtg         character    input     analysis date time groups
c   anl_time        real         input     analysis continuous time
c   bias_fcst       real         input     forecast bias stats
c   cnt             real         input     obs data cnts
c   n               integer      input     starting time period
c   rms_fcst        real         input     forecast rms stats
c   src             integer      input     file source
c   var             integer      input     variable index
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
      parameter (MX_CLR = 9)         
c
c     ..local array dimension
c
      integer   n_fcst
      integer   n_taus
c
      character anl_dtg (n_taus) * 10
      real      anl_time (n_taus)
      real      bias_fcst (n_taus, n_fcst)
      integer   color (n_fcst)
      real      cnt (n_taus, n_fcst)
      integer   dash_patrn
      character date * 6
      character date_beg * 6
      character date_end * 6
      integer   day
      real      del
      character dpth_lbl * (*)
      integer   fno
      real      half
      integer   i, j, k, n
      integer   iasf (13)
      integer   ke, ks
      character label (MX_CLR) * 3
      integer   len
      integer   mon
      character month (12) * 3
      real      rgb (3, 0:MX_CLR)
      real      rms_fcst (n_taus, n_fcst)
      integer   src
      real      tck
      character time1 * 4, time2 * 4, time3 * 4
      character title * 256
      character unit * 7
      integer   upd
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
     *   1.000, 0.000, 1.000,
     *   0.000, 1.000, 1.000,
     *   1.000, 1.000, 0.000,
     *   0.500, 0.500, 1.000,
     *   1.000, 1.000, 0.500 /
c
      data iasf / 13 * 1 /
c
c...............................executable..............................
c
c     ..form beginning and ending date labels
c
      read (anl_dtg(n)(5:8), '(2i2)') mon, day
      write (date_beg, '(i2, 1x, a)') day, month(mon)
      read (anl_dtg(ke)(5:8), '(2i2)') mon, day
      write (date_end, '(i2, 1x, a)') day, month(mon)
c
c     ..build dtg time check variables for labels
c
      watch = anl_dtg(n)(9:10)
      if (n_taus .le. 60) then
         time1 = '10' // watch
         time2 = '20' // watch
         time3 = '30' // watch
      else
         time1 = '01' // watch
         time2 = 'XX' // watch
         time3 = 'XX' // watch
      endif
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
c     ..set line width
c
      call setusv ('LW', 2000)
c
c-----------------------------------------------------------------------
c
c     ..call set with min/max of time (x) and bias (y)
c
      xmn = anl_time(n)
      xmx = anl_time(ke)
      if (src .eq. 1) then
         if (var .eq. 1) then
            unit = '(C)    '
            if (dpth_lbl(1:4) .eq. 'Surf') then
               ymn = -0.4
               ymx =  0.4
               del = 0.2
               tck = 0.1
            else
               ymn = -0.2
               ymx =  0.2
               del = 0.1
               tck = 0.05
            endif
         else if (var .eq. 2) then
            unit = '(PSU)  '
            if (dpth_lbl(1:4) .eq. 'Surf') then
               ymn = -0.1
               ymx =  0.1
               del = 0.1
               tck = 0.05
            else
               ymn = -0.1
               ymx =  0.1 
               del = 0.1
               tck = 0.05
            endif
         endif
      else if (src .eq. 2) then
         if (var .eq. 1) then
            unit = '%      '
            ymn = -5.
            ymx =  5.
            del = 5.
            tck = 1.
         endif
      else
         return   
      endif
      half = ymx * 0.5
      call set (.10, .90, .20, .50, xmn, xmx, ymn, ymx, 1)
c
c     ..mark dashed (and solid) bias lines
c
      call gsplci (1)
      call lined (xmn, -half, xmx, -half)
      call line  (xmn,   0.0, xmx,  0.0)
      call lined (xmn,  half, xmx,  half)
      call plotit (0, 0, 0)
c
c     ..mark forecast bias
c
      do k = 1, n_fcst
         call gsplci (k+1)
         upd = k + 1
         color(k) = upd
         upd = k * 24
         write (label(k), '(i3)') upd
         call setusv ('LW', 3000)
         if (cnt(n,k) .gt. 0.) then
            call frstpt (anl_time(n), bias_fcst(n,k))
         else
            call frstpt (anl_time(n), 0.)
         endif
         do i = n, ke
            if (cnt(i,k) .gt. 0.) then
               call vector (anl_time(i), bias_fcst(i,k))
c              call plchhq (anl_time(i), bias_fcst(i,k), 'o',
c    *                      .01, 0., 0.)
            else
               if (i .lt. ke) then
                  if (cnt(i+1,k) .gt. 0.) then
                     call frstpt (anl_time(i+1), bias_fcst(i+1,k))
                  endif
               endif
            endif
         enddo
         call setusv ('LW', 2000)
         call plotit (0, 0, 0)
      enddo
c
c     ..label bias axis
c
      call gsplci (1)
      title = 'Mean Bias ' // unit
      len = len_trim (title)
      call lbl_axis (ymn, ymx, del, tck, title(1:len), 90.,
     *               .014, .08, .false.)
c
c     ..put label bar below plot
c
      call gsplci (1)
      call gsfais (1)
      call lbseti ('CBL - color boxlines', 1)
      call lbseti ('CLB - color labels', 1)
      call lblbar (0, .45, .90, .05, .12, n_fcst, 1., .20,
     *             color, 0, label, n_fcst, 1)
c
c     ..draw border around plot
c
      call line (xmn, ymn, xmx, ymn)
      call line (xmn, ymn, xmn, ymx)
      call line (xmn, ymx, xmx, ymx)
      call line (xmx, ymn, xmx, ymx)
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
      do i = (n+1), (ke-1)
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
      call plotit (0, 0, 0)
c
c-----------------------------------------------------------------------
c
c     ..call set with min/max of time (x) and rms (y)
c
      xmn = anl_time(n)
      xmx = anl_time(ke)
      ymn = 0.
      if (src .eq. 1) then
         if (var .eq. 1) then
            unit = '(C)    '
            if (dpth_lbl(1:4) .eq. 'Surf') then
               ymx = 2.
               del = 1.
               tck = 0.5
            else
               ymx = 1.
               del = 0.5
               tck = 0.25
            endif
         else if (var .eq. 2) then
            unit = '(PSU)  '
            if (dpth_lbl(1:4) .eq. 'Surf') then
               ymx = 0.4
               del = 0.2
               tck = 0.1
            else
               ymx = 0.2
               del = 0.1
               tck = 0.05
            endif
         endif
      else if (src .eq. 2) then
         if (var .eq. 1) then
            unit = '%      '
            ymx = 15.
            del = 5.
            tck = 1.
         endif
      else
         return
      endif
      half = ymx * 0.5
      call set (.10, .90, .60, .90, xmn, xmx, ymn, ymx, 1)
c
c     ..mark dashed (and solid) bias lines
c
      call gsplci (1)
      call lined (xmn,  half, xmx,  half)
      call plotit (0, 0, 0)
c
c     ..mark forecast rms
c
      do k = 1, n_fcst
         call gsplci (k+1)
         call setusv ('LW', 3000)
         if (cnt(n,k) .gt. 0.) then
            call frstpt (anl_time(n), rms_fcst(n,k))
         else
            call frstpt (anl_time(n), 0.)
         endif
         do i = n, ke
            if (cnt(i,k) .gt. 0.) then
               call vector (anl_time(i), rms_fcst(i,k))
c              call plchhq (anl_time(i), rms_fcst(i,k), 'o',
c    *                      .01, 0., 0.)
            else
               if (i .lt. ke) then
                  if (cnt(i+1,k) .gt. 0.) then
                     call frstpt (anl_time(i+1), rms_fcst(i+1,k))
                  endif
               endif
            endif
         enddo
         call setusv ('LW', 2000)
         call plotit (0, 0, 0)
      enddo
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
      call line (xmn, ymn, xmn, (ymn-0.05*(ymx-ymn)))
      call line (xmx, ymn, xmx, (ymn-0.05*(ymx-ymn)))
      do i = (n+1), (ke-1)
        call line (anl_time(i), ymn, anl_time(i),
     *            (ymn-0.025*(ymx-ymn)))
      enddo
      do i = (n+5), (ke-5)
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
      len = len_trim (dpth_lbl)
      if (len .gt. 0) then
         title = trim (dpth_lbl) // ' ' // trim (var_lbl) // 
     *          ' Forecast Error'
      else
         title = trim (var_lbl) // ' Forecast Error'
      endif
      call title_plot (title, .022, 1, 1.0)
      call plotit (0, 0, 0)
      call setusv ('LW', 1000)
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (title)
c
c-----------------------------------------------------------------------
c
c     ..advance the plot buffer
c
      call frame
c
      return
      end
