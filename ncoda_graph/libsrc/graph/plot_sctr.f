      subroutine plot_sctr (opt, n_obs, date, unit, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_sctr
c
c DESCRIPTION:
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
      include 'coda_types.h'
c
c     ..local array dimensions
c
      integer   n_obs
c
      real      age
      character albl * 132
      real      bias
      integer   clr (2)
      character date * 15
      real      del
      real      dmn, dmx
      real      ebk, eob
      integer   fno
      integer   i, k, m, n
      integer   kvar (10)
      character lbl (2) * 10
      real      lat, lon
      integer   len
      real      lvl
      integer   ndx
      integer   npts
      integer   nvar
      character opt * 3
      real      pos (4, 2)
      real      rms
      real      scl
      character sgn * 7
      real      siz
      real      tck
      character temp * 132
      character ttl_lbl (10) * 26
      integer   unit
      character var_lbl (10) * 13
      real      xi, yj, zk
      real      x_pos, y_pos
c
c     ..allocatable arrays
c
      real,     allocatable :: anl (:)
      real,     allocatable :: bkg (:)
      integer,  allocatable :: typ (:)
      real,     allocatable :: val (:)
      integer,  allocatable :: var (:)
c
      data      clr / 4, 3 /
      data      lbl / 'Background', '  Analysis' /
      data      pos / .12, .50, .30, .68,
     *                .59, .97, .30, .68 /
c
c...............................executable..............................
c
c     ..initialize
c
      kvar = 0
      var_lbl = '             '
      ttl_lbl = '                          '
c
c     ..allocate arrays
c
      allocate (anl (n_obs))
      allocate (bkg (n_obs))
      allocate (typ (n_obs))
      allocate (val (n_obs))
      allocate (var (n_obs))
c
c     ..read innovation vector; save verification data
c
      read (unit) age
      read (unit) lat
      read (unit) lon
      read (unit) lvl
      read (unit) ndx
      read (unit) ebk 
      read (unit) eob
      read (unit) typ(1:n_obs)
      read (unit) var(1:n_obs)
      read (unit) val(1:n_obs)
      read (unit) anl(1:n_obs)
      read (unit) bkg(1:n_obs)
      read (unit) xi
      read (unit) yj
      read (unit) zk
      read (unit) sgn
c************************
      if (opt .eq. 'ice') then
      do i = 1, n_obs
      if (val(i) .gt. 0.) then
      write (88,'(3i10,3f10.2)')
     * i,typ(i),var(i),val(i),anl(i),bkg(i)
      endif
      enddo
      endif
c************************
c
c     ..set analysis variables
c
      if (opt .eq. 'ice') then
         nvar = 1
         var_lbl(1) = 'Ice Cover    '
         ttl_lbl(1) = 'Sea Ice Coverage          '
         del = 10.
         scl = 1.
         tck = 5.
         kvar(1) = 1
      else if (opt .eq. 'sst') then
         nvar = 1
         var_lbl(1) = 'Temperature  '
         ttl_lbl(1) = 'Sea Surface Temperature   '
         scl = 1.
         tck = 1.
         kvar(1) = 1
      else if (opt .eq. 'sss') then
         nvar = 1
         var_lbl(1) = 'Salinity  '
         ttl_lbl(1) = 'Sea Surface Salinity      '
         scl = 1.
         tck = 1.
         kvar(1) = 1
      else if (opt .eq. 'ssh') then
         nvar = 1
         var_lbl(1) = 'SSH Anomaly  '
         ttl_lbl(1) = 'Sea Surface Height        '
         del = 0.5
         scl = 0.5
         tck = 0.1
         kvar(1) = 1
      else if (opt .eq. 'mvo') then
         nvar = 4
         var_lbl(1) = 'Temperature  '
         var_lbl(2) = 'Salinity     '
         var_lbl(3) = 'U Velocity   '
         var_lbl(4) = 'V Velocity   '
         ttl_lbl(1) = 'Temperature               '
         ttl_lbl(2) = 'Salinity                  '
         ttl_lbl(3) = 'U Velocity                '
         ttl_lbl(4) = 'V Velocity                '
         scl = 1.
         tck = 1.
         kvar(1) = 1
         kvar(2) = 2
         kvar(3) = 4
         kvar(4) = 5
      else if (opt .eq. 'vel') then
         nvar = 2
         var_lbl(1) = 'U Velocity   '
         var_lbl(2) = 'V Velocity   '
         ttl_lbl(1) = 'U Velocity                '
         ttl_lbl(2) = 'V Velocity                '
         del = 10.
         scl = 10.
         tck = 5.
         kvar(1) = 4
         kvar(2) = 5
         do i = 1, n_obs
            anl(i) = anl(i) * 100.
            bkg(i) = bkg(i) * 100.
            val(i) = val(i) * 100.
         enddo
      else
         return
      endif
c
c     ..loop over analysis variables
c
      do n = 1, nvar
c
c     ..loop over data types
c
      do k = 1, MX_TYPES
c
c        ..set min max of data values
c
         dmn =  1.e16
         dmx = -1.e16
         npts = 0
         do i = 1, n_obs
            if (kvar(n) .eq. var(i) .and. k .eq. typ(i)) then
               if (anl(i) .lt. dmn) dmn = anl(i)
               if (bkg(i) .lt. dmn) dmn = bkg(i)
               if (val(i) .lt. dmn) dmn = val(i)
               if (anl(i) .gt. dmx) dmx = anl(i)
               if (bkg(i) .gt. dmx) dmx = bkg(i)
               if (val(i) .gt. dmx) dmx = val(i)
               npts = npts + 1
            endif
         enddo
         if (npts .eq. 0) cycle
c
c        ..scale data range to "nice" values
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
c        ..ensure reasonable min/max data range
c
         if (opt .eq. 'ice') then
            if (dmn .lt.   0.) dmn = 0.
            if (dmx .gt. 100.) dmx = 100.
         else if (opt .eq. 'sst') then
            if (dmn .lt. -2.) dmn = -2.
            if (dmx .gt. 42.) dmx = 42.
            if ((dmx - dmn) .lt. 3.) then
               del = 1.
            else if ((dmx - dmn) .lt. 18.) then
               del = 2.
             else
               del = 4.
            endif
         else if (opt .eq. 'mvo') then
            if (n .eq. 1) then
               if (dmn .lt. -2.) dmn = -2.
               if (dmx .gt. 42.) dmx = 42.
               if ((dmx - dmn) .lt. 3.) then
                  del = 1.
               else if ((dmx - dmn) .lt. 18.) then
                  del = 2.
               else
                  del = 4.
               endif
            else if (n .eq. 2) then
               if (dmn .lt.  0.) dmn =  0.
               if (dmx .gt. 42.) dmx = 42.
               if ((dmx - dmn) .lt. 3.) then
                  del = 1.
               else if ((dmx - dmn) .lt. 18.) then
                  del = 2.
               else
                  del = 4.
               endif
            endif
         else if (opt .eq. 'vel') then
            if (dmx .gt. 30. .or. dmn .lt. -30.) then
               dmn = -30.
               dmx =  30.
            else
               dmx = max (dmx, abs (dmn))
               dmn = -dmx
            endif
         endif
c
c        ..set polymarker symbol and size
c
         call gsmk (3)
         call gsmksc (.4)
c
c        ..loop over plot types
c
         do m = 1, 2
c
c           ..call set routine with min/max of sst data
c
            call set (pos(1,m), pos(2,m), pos(3,m), pos(4,m),
     *                dmn, dmx, dmn, dmx, 1)
c
c           ..set display color
c
            call gspmci (clr(m))
c
c           ..initialize rms and mean bias estimators
c
            bias = 0.
            rms  = 0.
            npts = 0
c
c           ..mark valid obs positions in obs vs predicted
c             space, compute mean bias and rms statistics
c
            do i = 1, n_obs
               if (anl(i) .gt. -990. .and.
     *             bkg(i) .gt. -990. .and.
     *             val(i) .gt. -990.) then
                  if (kvar(n) .eq. var(i) .and. k .eq. typ(i)) then
                     if (m .eq. 1) then
                        call gpm (1, val(i), bkg(i))
                        bias = bias + (val(i) - bkg(i))
                        rms = rms + (val(i) - bkg(i)) ** 2
                     else if (m .eq. 2) then
                        call gpm (1, val(i), anl(i))
                        bias = bias + (val(i) - anl(i))
                        rms = rms + (val(i) - anl(i)) ** 2
                     endif
                     npts = npts + 1
                  endif
               endif
            enddo
c
c           ..compute mean bias and rms
c
            bias = bias / real (npts)
            rms = sqrt (rms / real (npts))
c
c           ..flush plot buffer
c
            call plotit (0, 0, 0)
c
c           ..set line drawing color to black (default foreground)
c
            call gsplci (1)
c
c           ..draw line along perfect fit
c
            call frstpt (dmn, dmn)
            call vector (dmx, dmx)
c
c           ..draw a box around data area
c
            call plotif (pos(1,m), pos(3,m), 0)
            call plotif (pos(2,m), pos(3,m), 1)
            call plotif (pos(2,m), pos(4,m), 1)
            call plotif (pos(1,m), pos(4,m), 1)
            call plotif (pos(1,m), pos(3,m), 1)
            call plotif (pos(1,m), pos(3,m), 2)
c
c           ..set label character size
c
            siz = .025 * (pos(2,m) - pos(1,m))
c
c           ..mark rms estimate
c
            x_pos = dmx - (dmx - dmn) * 0.25
            y_pos = dmn + (dmx - dmn) * 0.19
            write (albl, '('' RMS = '', f6.2)') rms
            len = len_trim (albl)
            call plchhq (x_pos, y_pos, albl(1:len),
     *                   siz, 0., 0.)
c
c           ..mark mean bias estimate
c
            x_pos = dmx - (dmx - dmn) * 0.25
            y_pos = dmn + (dmx - dmn) * 0.12
            write (albl, '(''Bias = '', f6.2)') bias
            len = len_trim (albl)
            call plchhq (x_pos, y_pos, albl(1:len),
     *                   siz, 0., 0.)
c
c           ..mark obs counter
c
            x_pos = dmx - (dmx - dmn) * 0.25
            y_pos = dmn + (dmx - dmn) * 0.05
            write (albl, '(''    N ='', i7)') npts
            len = len_trim (albl)
            call plchhq (x_pos, y_pos, albl(1:len),
     *                   siz, 0., 0.)
c
c           ..mark analysis date and grid mesh
c
            x_pos = dmn + (dmx - dmn) * 0.25
            y_pos = dmx - (dmx - dmn) * 0.05
            len = len_trim (date)
            call plchhq (x_pos, y_pos, date(1:len),
     *                   siz, 0., 0.)
c
c           ..label observed data type axis
c
            siz = .03
            albl = adjustl (data_lbl(k))
            call lbl_axis (dmn, dmx, del, tck, albl, 0.,
     *                     siz, .05, .false.)
c
c           ..label predicted data type axis
c
            siz = .03
            temp = lbl(m) // ' ' // var_lbl(n)
            albl = adjustl (temp)
            call lbl_axis (dmn, dmx, del, tck, albl, 90.,
     *                    siz, .06, .false.)
         enddo
c
c        ..put title on plot
c
         call set (.12, .97, .30, .68, dmn, dmx, dmn, dmx, 1)
         write (albl, '(''Verification - '', a)')
     *                  trim (ttl_lbl(n))
         siz = .019
         call title_plot (albl, siz, 1, 1.5)
c
c        ..generate plot
c
         fno = fno + 1
         albl = trim (var_lbl(n)) // ' ' // 
     *          adjustl (data_lbl(k))
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (albl)
         call frame
      enddo
      enddo
c
c     ..clean up
c
      deallocate (anl, bkg, typ, val, var)
c
      return
      end
