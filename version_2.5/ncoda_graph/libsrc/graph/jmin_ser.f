      subroutine jmin_ser (n_taus, n_typs, ks, ke, anl_dtg, anl_time,
     *                     diagn, cnt, var_lbl, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  jmin_ser
c
c DESCRIPTION:  plots jmin diagnostic time series
c
c PARAMETERS:
c       Name         Type        Usage            Description
c   ------------    ---------    ------    ---------------------------
c   anl_dtg         character    input     analysis date time groups
c   anl_time        real         input     analysis continuous time
c   cnt             real         input     obs data counts
c   diagn           real         input     obs jmin diagnostic
c   domain          character    input     regional domain name
c   n               integer      input     starting time periods
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
c     ..local array dimension
c
      integer   n_taus
      integer   n_typs
c
      character anl_dtg (n_taus) * 10
      real      anl_time (n_taus)
      integer   clr
      real      cnt (n_taus, 0:n_typs)
      integer   dash_patrn
      character date * 6
      character date_beg * 6
      character date_end * 6
      integer   day
      real      del
      real      diagn (n_taus, 0:n_typs)
      integer   fno
      real      half
      integer   i, k
      integer   iasf (13)
      integer   ks, ke
      character lbl1*1, lbl8*8
      integer   len
      integer   mon
      character month (12) * 3
      real      tck
      character time1 * 4, time2 * 4, time3 * 4
      character title * 256
      character var_lbl * (*)
      character watch * 2
      real      xmn, xmx
      real      ymn, ymx
c*****************
      real x
c****************
c
c     ..functions
c
      integer   ishift
c
      include 'coda_types.h'
      include 'color_table.h'
c
c     ..set label bar variables
c
      character clr_lbl (0:MX_TYPES) * 20
      integer   color (0:MX_TYPES)
      integer   n_clr
c
c     ..set months of the year
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /
c
      data iasf / 13 * 1 /
c
c...............................executable..............................
c
c     ..form beginning and ending date labels
c
      read (anl_dtg(ks)(5:8), '(2i2)') mon, day
      write (date_beg, '(i2, 1x, a)') day, month(mon)
      read (anl_dtg(ke)(5:8), '(2i2)') mon, day
      write (date_end, '(i2, 1x, a)') day, month(mon)
c
c     ..set color scheme
c
      call gsclip (0)
      call gsasf (iasf)
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
      call setusv ('LW', 2000)
c
c     ..set dashed line pattern
c
      dash_patrn = ishift (30840, 1)
      call dashdb (dash_patrn)
c
c     ..build dtg time check variables for labels
c
      k = ke - ks
      watch = anl_dtg(ks)(9:10)
      if (k .le. 60) then
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
c     ..overall jmin
c
      xmn = anl_time(ks)
      xmx = anl_time(ke)
      ymn = 0.
      ymx = 2.
      del = 1.
      tck = 0.5
      half = ymx * 0.5
      call set (.10, .90, .70, .90, xmn, xmx, ymn, ymx, 1)
c
c     ..label jmin axis
c
      call gsplci (1)
      title = 'Overall Jmin'
      len = len_trim (title)
      call lbl_axis (ymn, ymx, del, tck, title(1:len), 90.,
     *               .015, .06, .false.)
c
c     ..draw border around plot
c
      call line (xmn, ymn, xmx, ymn)
      call line (xmn, ymn, xmn, ymx)
      call line (xmn, ymx, xmx, ymx)
      call line (xmx, ymn, xmx, ymx)
c
c     ..mark jmin values
c
      call gsplci (1)
      call setusv ('LW', 2000)
      if (cnt(ks,0) .gt. 0.) then
         call frstpt (anl_time(ks), diagn(ks,0))
      else
         call frstpt (anl_time(ks), 0.)
      endif
      do i = ks, ke
         if (cnt(i,0) .gt. 0.) then
            call vector (anl_time(i), diagn(i,0))
            call plchhq (anl_time(i), diagn(i,0), '+', .01, 0., 0.)
         else
            if (i .lt. ke) then
               if (cnt(i+1,0) .gt. 0.) then
                  call frstpt (anl_time(i+1), diagn(i+1,0))
               endif
            endif
         endif
      enddo
      call setusv ('LW', 2000)
      call plotit (0, 0, 0)
c
c     ..mark beginning and ending time periods
c
      call gsplci (1)
      call line (xmn, ymn, xmn, (ymn-0.05*(ymx-ymn)))
      call line (xmx, ymn, xmx, (ymn-0.05*(ymx-ymn)))
      do i = (ks+1), (ke-1)
        call line (anl_time(i), ymn, anl_time(i),
     *            (ymn-0.025*(ymx-ymn)))
      enddo
c
c     ..mark expected jmin
c
      call line (xmn, 1., xmx, 1.)
      call plotit (0, 0, 0)
c
c     ..put title above plot
c
      len = len_trim (var_lbl)
      title = var_lbl(1:len) // ' Jmin Diagnostics'
      call title_plot (title, .022, 1, 1.0)
      call plotit (0, 0, 0)
c
c     ..update frame number
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (title)
c
c-----------------------------------------------------------------------
c
c     ..observing system jmins
c     
      xmn = anl_time(ks)
      xmx = anl_time(ke)
      ymn = 0.
      ymx = 2.
      del = 1.
      tck = 0.5
      half = ymx * 0.5
      call set (.10, .90, .45, .65, xmn, xmx, ymn, ymx, 1)
c
c     ..label jmin axis
c     
      call gsplci (1)
      title = 'Data Type Jmin'
      len = len_trim (title)
      call lbl_axis (ymn, ymx, del, tck, title(1:len), 90.,
     *               .015, .06, .false.)
c
c     ..draw border around plot
c
      call line (xmn, ymn, xmx, ymn)
      call line (xmn, ymn, xmn, ymx)
      call line (xmn, ymx, xmx, ymx)
      call line (xmx, ymn, xmx, ymx)
c
c     ..mark jmin values
c
      clr = 2
      color = 0
      n_clr = 0
      color(n_clr) = 1
      clr_lbl(n_clr) = adjustl (data_lbl(0))
      call setusv ('LW', 2000)
      do k = 1, n_typs
         if (k .eq.  50 .or. k .eq.  51) cycle
         if (k .eq. 141 .or. k .eq. 142) cycle
         if (k .eq. 143 .or. k .eq. 144) cycle
         if (k .eq. 177 .or. k. eq. 178) cycle
         if (cnt(ks,k) .gt. 0.) clr = clr + 1
         if (clr .gt. MX_OBS_CLR) clr = MX_OBS_CLR 
         call gsplci (clr)
         if (cnt(ks,k) .gt. 0.) then
            n_clr = n_clr + 1
            color(n_clr) = clr
            clr_lbl(n_clr) = adjustl (data_lbl(k))
         endif            
         if (cnt(ks,k) .gt. 0.) then
            call frstpt (anl_time(ks), diagn(ks,k)) 
         else
            call frstpt (anl_time(ks), 0.)
         endif
         do i = ks, ke
            if (cnt(i,k) .gt. 0.) then
               call vector (anl_time(i), diagn(i,k))
               call plchhq (anl_time(i), diagn(i,k), 'x', .01, 0., 0.)
            else
               if (i .lt. ke) then
                  if (cnt(i+1,k) .gt. 0.) then
                     call frstpt (anl_time(i+1), diagn(i+1,k))
                  endif
               endif 
            endif
         enddo
         call plotit (0, 0, 0)
      enddo
      call setusv ('LW', 2000)
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
      do i = (ks+1), (ke-1)
        call line (anl_time(i), ymn, anl_time(i),
     *            (ymn-0.025*(ymx-ymn)))
      enddo
      do i = (ks+5), (ke-5)
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
c     ..mark expected jmin
c
      call line (xmn, 1., xmx, 1.)
      call plotit (0, 0, 0)
c
c-----------------------------------------------------------------------
c
c     ..data counts
c
      xmn = anl_time(ks)
      xmx = anl_time(ke)
      ymn = 0.
      ymx = maxval (cnt)
      ymx = real (int (ymx / 1000.) + 1) * 1000. 
      call set (.10, .90, .10, .30, xmn, xmx, ymn, ymx, 1)
c
c     ..label data count axis
c
      call gsplci (1)
      write (lbl1, '(i1)') nint (ymn)
      call plchhq ((xmn - 0.011*(xmx-xmn)), ymn, lbl1,
     *             .011, 0., 1.)
      write (lbl8, '(i8)') nint (ymx)
      call plchhq ((xmn - 0.011*(xmx-xmn)), ymx, lbl8,
     *             .011, 0., 1.)
      half = (ymx + ymn) * 0.5
      call lined (xmn, half, xmx, half)
c
c     ..mark data count values
c
      clr = 2
      call setusv ('LW', 2000)
      do k = 0, n_typs
         if (k .eq.  50 .or. k .eq.  51) cycle
         if (k .eq. 141 .or. k .eq. 142) cycle
         if (k .eq. 143 .or. k .eq. 144) cycle
         if (k .eq. 172 .or. k. eq. 178) cycle
         if (k .eq. 183 .or. k .eq. 184) cycle
         if (k .eq. 0) then
            call gsplci (1)
         else
            if (cnt(ks,k) .gt. 0.) clr = clr + 1
            if (clr .gt. MX_OBS_CLR) clr = MX_OBS_CLR
            call gsplci (clr)
         endif
         if (cnt(ks,k) .gt. 0.) then
            call frstpt (anl_time(ks), cnt(ks,k))
         else
            call frstpt (anl_time(ks), 0.)
         endif
         do i = ks, ke
            call vector (anl_time(i), cnt(i,k))
            call plchhq (anl_time(i), cnt(i,k), 'o', .01, 0., 0.)
         enddo
         call plotit (0, 0, 0)
      enddo
      call setusv ('LW', 2000)
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
      do i = (ks+1), (ke-1)
         call line (anl_time(i), ymn, anl_time(i),
     *             (ymn-0.025*(ymx-ymn)))
      enddo
      do i = (ks+5), (ke-5)
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
c-----------------------------------------------------------------------
c
c     ..create color table label bar 
c
      call lbseti ('CBL - color of box lines', 1)
      call lblbar (1, .3, .7, .3, .7, (n_clr+1), .10, 1.0,
     *             color, 0, clr_lbl, (n_clr+1), 1)
      call frame
c
      return
      end
