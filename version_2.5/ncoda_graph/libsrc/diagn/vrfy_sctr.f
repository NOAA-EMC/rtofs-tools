      subroutine vrfy_sctr (n_var, dtg, title1, title3, n_obs, n_data,
     *                      var, val, bkg, anl, opt)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vrfy_sctr
c
c DESCRIPTION:  plot obs vs. forecast and obs vs. analysis scatter plots
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
c
      real      bias
      character dat_sfx * 20
      real      del
      real      dmn, dmx
      character dtg * 10
      integer   fno
      integer   i, k, m, n
      integer   ix, jy
      character lbl (2) * 10
      integer   len
      integer   n_data
      integer   n_var
      integer   nx, ny
      character opt * 3
      real      pos (4, 2)
      real      rms
      real      scl
      real      siz
      real      spmis
      real      spval
      real      tck
      character temp * 132
      character title1 * 80
      character title2 * 80
      character title3 * 256
      character tmp_name * 80
      real      xi, yj
      real      x_pos, y_pos
c
c     ..obs arrays
c
      real      anl (n_obs)
      real      bkg (n_obs)
      real      val (n_obs)
      integer   var (n_obs)
c
c     ..allocatable arrays
c
      real,     allocatable :: cnt (:,:)
c
      data      lbl / 'Background', '  Analysis' /
      data      pos / .12, .50, .30, .68,
     *                .59, .97, .30, .68 /
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
      write (tmp_name, '(''sctr.'', a, ''.gmeta '')') dtg
      call init_gks ('opn', tmp_name)
c
c     ..loop over variables
c
      do k = 1, 2
c
c     ..set analysis type for plot title
c
      if (k .eq. 1) then
         dat_sfx = 'Temperature'
         scl = 1.
         tck = 1.
      else
         dat_sfx = 'Salinity'
         scl = 1.
         tck = 1.
      endif
c
      spval = -999.
      spmis = spval + 9.
c
c     ..set min max of data values
c
      dmn =  1.e16
      dmx = -1.e16
      n = 0
      do i = 1, n_data
         if (var(i) .eq. k) then
            if (anl(i) .gt. spmis) then
               if (anl(i) .lt. dmn) dmn = anl(i)
               if (anl(i) .gt. dmx) dmx = anl(i)
            endif
            if (bkg(i) .gt. spmis) then
               if (bkg(i) .lt. dmn) dmn = bkg(i)
               if (bkg(i) .gt. dmx) dmx = bkg(i)
            endif
            if (val(i) .gt. spmis) then
               if (val(i) .lt. dmn) dmn = val(i)
               if (val(i) .gt. dmx) dmx = val(i)
            endif
         endif
      enddo
c
c     ..scale data range to "nice" values
c
      if (mod (scl, 1.) .ne. 0.) then
         if (real (nint (dmn)) .lt. dmn) then
            dmn = real (nint (dmn))
         else
            dmn = real (nint (dmn)) - scl
         endif
      else
         if (dmn .lt. 0.) then
            dmn = real (int (dmn)) - scl
         else
            dmn = real (int (dmn))
         endif
      endif
      if (mod (scl, 1.) .ne. 0.) then
         if (real (nint (dmx)) .gt. dmx) then
            dmx = real (nint (dmx))
         else
            dmx = real (nint (dmx)) + scl
         endif
      else
         if (dmx .lt. 0.) then
            dmx = real (int (dmx))
         else
            dmx = real (int (dmx)) + scl
         endif
      endif
      if ((dmx - dmn) .lt. scl) dmx = dmn + scl
c
c     ..ensure reasonable min/max data range
c
      if (k .eq. 1) then
         if (dmn .lt. -2.) dmn = -2.
         if (dmx .gt. 40.) dmx = 40.
         if ((dmx - dmn) .lt. 3.) then
            del = 1.
         else if ((dmx - dmn) .lt. 18.) then
            del = 2.
         else
            del = 4.
         endif
      else
         if (dmn .lt. 32.) dmn = 32.
         if (dmx .gt. 38.) dmx = 38.
         if ((dmx - dmn) .lt. 3.) then
            del = 0.5
         else if ((dmx - dmn) .lt. 6.) then
            del = 1.
         else if ((dmx - dmn) .lt. 18.) then
            del = 2.
         else
            del = 4.
         endif
      endif
c
c     ..allocate contour array
c
      nx = 200
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
     *             dmn, dmx, dmn, dmx, 1)
c
c        ..initialize counts and rms and mean bias estimators
c
         cnt = 0.
         bias = 0.
         rms = 0.
         n = 0
c
c        ..form contour array in obs vs predicted space,
c          compute mean bias and rms statistics
c
         do i = 1, n_data
            if (var(i) .eq. k) then
               if (m .eq. 1) then
                  if (val(i) .gt. spmis .and.
     *                bkg(i) .gt. spmis) then
                     xi = (val(i) - dmn) / (dmx - dmn)
                     yj = (bkg(i) - dmn) / (dmx - dmn)
                     ix = nint (xi * real (nx))
                     jy = nint (yj * real (ny))
                     if (ix .ge. 1 .and. ix .le. nx) then
                     if (jy .ge. 1 .and. jy. le. ny) then
                        cnt(ix,jy) = cnt(ix,jy) + 1.
                     endif
                     endif
                     bias = bias + (val(i) - bkg(i))
                     rms = rms + (val(i) - bkg(i)) ** 2
                     n = n + 1
                  endif
               else if (m .eq. 2) then
                  if (val(i) .gt. spmis .and.
     *                anl(i) .gt. spmis) then
                     xi = (val(i) - dmn) / (dmx - dmn)
                     yj = (anl(i) - dmn) / (dmx - dmn)
                     ix = nint (xi * real (nx))
                     jy = nint (yj * real (ny))
                     if (ix .ge. 1 .and. ix .le. nx) then
                     if (jy .ge. 1 .and. jy. le. ny) then
                        cnt(ix,jy) = cnt(ix,jy) + 1.
                     endif
                     endif
                     bias = bias + (val(i) - anl(i))
                     rms = rms + (val(i) - anl(i)) ** 2
                     n = n + 1
                  endif
               endif
            endif
         enddo
         if (n .eq. 0) cycle
c
c        ..contour array
c
         call contour_count (nx, ny, cnt, spval)
c
c        ..compute mean bias and rms
c
         bias = bias / real (n)
         rms = sqrt (rms / real (n))
c
c        ..set color table
c
         call gks_color (rgb_obs_clr, MX_OBS_CLR)
         call setusv ('LW', 2000)
c
c        ..set line drawing color to black (default foreground)
c
         call gsplci (1)
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
c        ..set label character size
c
         siz = .025 * (pos(2,m) - pos(1,m))
c
c        ..mark rms estimate
c
         x_pos = dmx - (dmx - dmn) * 0.25
         y_pos = dmn + (dmx - dmn) * 0.12
         write (temp, '('' RMS = '', f6.2)') rms
         len = len_trim (temp)
         call plchhq (x_pos, y_pos, temp(1:len),
     *                siz, 0., 0.)
c
c        ..mark mean bias estimate
c
         x_pos = dmx - (dmx - dmn) * 0.25
         y_pos = dmn + (dmx - dmn) * 0.05
         write (temp, '(''Bias = '', f6.2)') bias
         len = len_trim (temp)
         call plchhq (x_pos, y_pos, temp(1:len),
     *                siz, 0., 0.)
c
c        ..label observed data type axis
c
         siz = .03
         temp = 'Observed ' // dat_sfx
         call lbl_axis (dmn, dmx, del, tck, temp, 0.,
     *                  siz, .05, .false.)
c
c        ..label predicted data type axis
c
         siz = .03
         temp = lbl(m) // ' ' // dat_sfx
         call lbl_axis (dmn, dmx, del, tck, temp, 90.,
     *                  siz, .06, .false.)
      enddo
c
c     ..put titles on plot
c
      call set (.12, .97, .30, .68, dmn, dmx, dmn, dmx, 1)
      if (opt .eq. 'prf') then
         write (title2, '(''Argo '', a)') trim (dat_sfx)
      else if (opt .eq. 'sfc') then
         write (title2, '(''Surface '', a)') trim (dat_sfx)
      endif
      siz = .019
      call title_plot (title1, siz, 1, 4.0)
      call title_plot (title2, siz, 1, 2.15)
      call title_plot (title3, siz, 1, 1.)
c
c     ..generate plot
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (temp)
      call frame
      call setusv ('LW', 1000)
c
c     ..clean up
c
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
