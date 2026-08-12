      subroutine vrfy_time (n_var, dtg, title1, title3, n_files, dtg1,
     *                      dtg2, upd, n_obs, n_data, var, val, bkg,
     *                      anl, tim)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vrfy_time
c
c DESCRIPTION:  plots time series of OmF and OmA statistics by
c               observing system for a specified time period
c
c PARAMETERS:
c       Name          Type        Usage            Description
c   -------------   ----------   -------   ----------------------------
c   n_obs           integer      input     number obs to process
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
c     ..local array dimensions
c
      integer   n_obs
      integer   n_var
c
      character dat_sfx * 20
      real      del
      real      dmn, dmx
      character dtg * 10
      character dtg1 * 10
      character dtg2 * 10
      integer   fno
      integer   hrs
      integer   i, k, m
      integer   ix, jy
      integer   jday
      character lbl * 21
      integer   len
      integer   n_data
      integer   n_files
      integer   nx, ny
      real      oma, omf
      real      pos (4, 2)
      integer   skip
      real      spmis
      real      spval
      integer   status
      real      tck
      character temp * 132
      character time * 10
      character title1 * 80
      character title3 * 256
      character tmp_name * 80
      character unit * 6
      integer   upd
      integer   var_no
      real      x
      real      xi, yj
      real      xmn, xmx
c
c     ..obs arrays
c
      real      anl (n_obs)
      real      bkg (n_obs)
      real      tim (n_obs)
      real      val (n_obs)
      integer   var (n_obs)
c
c     ..allocatable arrays
c
      real,     allocatable :: cnt (:,:)
c
      data      pos / .10, .95, .50, .85,
     *                .10, .95, .10, .45 /
c
      include 'color_table.h'
c
c...............................executable..............................
c
c     ..initialize
c
      fno = 0
c
c     ..set gmeta file name, open gks
c
      write (tmp_name, '(''time.'', a, ''.gmeta '')') dtg
      call init_gks ('opn', tmp_name)
c
c     ..loop over variables
c
      do k = 1, 2
      if (k .eq. 1) then
         dat_sfx = 'Temperature'
         dmn = -3.
         dmx =  3.
         del = 1.
         tck = 0.5
         unit = '(C)'
      else
         dat_sfx = 'Salinity'
         dmn = -2.
         dmx =  2.
         del = 1.
         tck = 0.5
         unit = '(PSU)'
      endif
c
      spval = -999.
      spmis = spval + 9.
      skip = n_files / 30 + 1
      call dtg_time (dtg1, xmn, jday)
      call dtg_time (dtg2, xmx, jday)
      xmn = xmn - real (upd / 2)
      xmx = xmx + real (upd / 2)
c
c     ..set color table
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..set polymarker symbol and size
c
c     ..allocate contour array
c
      nx = 400
      ny = 200
      allocate (cnt (nx, ny))
c
c     ..loop over plot types
c
      do m = 1, 2
c
c        ..call set routine with min/max of data
c
         call set (pos(1,m), pos(2,m), pos(3,m), pos(4,m),
     *             xmn, xmx, dmn, dmx, 1)
c
c        ..mark valid obs positions in obs vs predicted
c          space, compute mean bias and rms statistics
c
         cnt = 0.
         do i = 1, n_data
            if (var(i) .eq. k) then
               if (m .eq. 1) then
                  if (val(i) .gt. spmis .and.
     *                bkg(i) .gt. spmis) then
                     omf = val(i) - bkg(i)
                     xi = (tim(i) - xmn) / (xmx - xmn)
                     yj = (omf - dmn) / (dmx - dmn)
                     ix = nint (xi * real (nx))
                     jy = nint (yj * real (ny))
                     if (ix .ge. 1 .and. ix .le. nx) then
                     if (jy .ge. 1 .and. jy .le. ny) then
                        cnt(ix,jy) = cnt(ix,jy) + 1.
                     endif
                     endif
                  endif
               else if (m .eq. 2) then
                  if (val(i) .gt. spmis .and.
     *                anl(i) .gt. spmis) then
                     oma = val(i) - anl(i)
                     xi = (tim(i) - xmn) / (xmx - xmn)
                     yj = (oma - dmn) / (dmx - dmn)
                     ix = nint (xi * real (nx))
                     jy = nint (yj * real (ny))
                     if (ix .ge. 1 .and. ix .le. nx) then
                     if (jy .ge. 1 .and. jy .le. ny) then
                        cnt(ix,jy) = cnt(ix,jy) + 1.
                     endif
                     endif
                  endif
               endif
            endif
         enddo
c
c        ..contour array
c
         call contour_count (nx, ny, cnt, spval)
c
c        ..set line drawing color to black (default foreground)
c
         call gsplci (1)
c
c        ..draw line along zero anomaly
c
         call frstpt (xmn, 0.)
         call vector (xmx, 0.)
c
c        ..draw a box around data area
c
         call plotif (pos(1,m), pos(3,m), 0)
         call plotif (pos(2,m), pos(3,m), 1)
         call plotif (pos(2,m), pos(4,m), 1)
         call plotif (pos(1,m), pos(4,m), 1)
         call plotif (pos(1,m), pos(3,m), 1)
         call plotif (pos(1,m), pos(3,m), 2)
c
c        ..label time axis
c
         call gsplci (1)
         do i = 1, n_files, skip
            hrs = (i-1) * upd
            call dtgmod (dtg1, hrs, time, status)
            call dtg_time (time, x, jday)
            call line (x, dmn, x, (dmn-0.025*(dmx-dmn)))
            if (m .eq. 2) then
               if (time(9:10) .eq. '00') then
                  call plchhq (x, (dmn-0.06*(dmx-dmn)), time(7:8),
     *                         .011, 0., 0.)
               else if (time(9:10) .eq. '12') then
                  call plchhq (x, (dmn-0.06*(dmx-dmn)), time(7:8),
     *                         .011, 0., 0.)
               endif
            endif
         enddo
         if (m .eq. 2) then
            x = (xmx + xmn) * 0.5
            call plchhq (x, (dmn-0.15*(dmx-dmn)), 'Time (days)',
     *                   .014, 0., 0.)
         endif
c
c        ..label oma and omf axes
c
         if (m .eq. 1) then
            lbl = 'Obs - Forecast ' // unit
         else
            lbl = 'Obs - Analysis ' // unit
         endif
         len = len_trim (lbl)
         call lbl_axis (dmn, dmx, del, tck, lbl(1:len), 90.,
     *                  .014, .08, .false.)
      enddo
c
c     ..put titles on plot
c
      call set (.1, .9, .1, .9, .1, .9, .1, .9, 1)
      write (temp, '(a, 4x, ''Argo Innovation Verification '', a)')
     *       trim (title1), trim (dat_sfx)
      call title_plot (temp,   .019, 1, 2.)
      call title_plot (title3, .019, 1, 0.)
c
c     ..generate plot
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (temp)
      call frame
      deallocate (cnt)
c
c     ..return for new variable
c
      enddo
c
c     ..close gks
c
      call init_gks ('cls', tmp_name)
c
      return
      end
