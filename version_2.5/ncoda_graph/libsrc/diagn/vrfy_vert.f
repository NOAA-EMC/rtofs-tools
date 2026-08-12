      subroutine vrfy_vert (n_var, dtg, title1, title3, n_obs, n_data,
     *                      typ, val, bkg, anl, lvl)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vrfy_vert
c
c DESCRIPTION:  plots vertical distributions of OmF and OmA statistics
c               for selected observing systems
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
      integer    N_TYP
      parameter (N_TYP = 2)
c
      include 'coda_types.h'
c
c     ..local array dimensions
c
      integer   n_obs
      integer   n_var
c
      integer   clr (2)
      real      del
      real      dmn, dmx
      character dtg * 10
      integer   fno
      integer   i, j, k, m, n
      character lbl * 21
      integer   len
      integer   n_data
      integer   plt_typ (N_TYP)
      real      pos (4, 2)
      integer   sal_typ (N_TYP)
      real      spmis
      real      tck
      character temp * 132
      character title1 * 80
      character title3 * 256
      character tmp_name * 80
      integer   tmp_typ (N_TYP)
      character unit * 6
      real      x, y
      real      ymn, ymx
c
c     ..obs arrays
c
      real      anl (n_obs)
      real      bkg (n_obs)
      real      lvl (n_obs)
      integer   typ (n_obs)
      real      val (n_obs)
c
c     ..specify temperature and salinity observing systems
c
      data      tmp_typ / 36, 102 /
      data      sal_typ / 37, 103 /
c
      data      clr / 4, 3 /
      data      pos / .12, .50, .20, .78,
     *                .59, .97, .20, .78 /
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
      write (tmp_name, '(''vert.'', a, ''.gmeta '')') dtg
      call init_gks ('opn', tmp_name)
c
c     ..loop over variables
c
      do k = 1, n_var
c
c     ..initialize plot variables
c
      if (k .eq. 1) then
         dmn = -20.
         dmx =  20.
         del = 10.
         tck = 5.
         unit = '(C)'
         do i = 1, N_TYP
            plt_typ(i) = tmp_typ(i)
         enddo
      else 
         dmn = -6.
         dmx =  6.
         del = 2.
         tck = 1.
         unit = '(PSU)'
         do i = 1, N_TYP
            plt_typ(i) = sal_typ(i)
         enddo
      endif
c
      spmis = -990.
      ymn = 0.
      ymx = 1000.
c
c     ..set color table
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..loop over data types
c
      do j = 1, N_TYP
c
c        ..set polymarker symbol and size
c
         call gsmk (3)
         call gsmksc (.4)
c
c        ..check for obs this data type
c
         n = 0
         do i = 1, n_data
            if (typ(i) .eq. plt_typ(j)) n = n + 1
         enddo
         if (n .eq. 0) cycle
c
c        ..loop over plot types
c
         do m = 1, 2
c
c           ..call set routine with min/max of data
c
            call set (pos(1,m), pos(2,m), pos(3,m), pos(4,m),
     *                dmn, dmx, ymx, ymn, 1)
c
c           ..set display color
c
            call gspmci (clr(m))
c
c           ..mark obs positions
c
            do i = 1, n_data
               if (lvl(i) .le. ymx) then
                  if (typ(i) .eq. plt_typ(j)) then
                     if (m .eq. 1) then
                        if (val(i) .gt. spmis .and.
     *                      bkg(i) .gt. spmis) then
                           x = val(i) - bkg(i)
                           y = ymx - lvl(i)
                           call gpm (1, x, y)
                        endif
                     else if (m .eq. 2) then
                        if (val(i) .gt. spmis .and.
     *                      anl(i) .gt. spmis) then
                           x = val(i) - anl(i)
                           y = ymx - lvl(i)
                           call gpm (1, x, y)
                        endif
                     endif
                  endif
               endif
            enddo
c
c           ..flush plot buffer
c
            call plotit (0, 0, 0)
c
c           ..set line drawing color to black (default foreground)
c
            call gsplci (1)
c
c           ..draw line along zero anomaly
c
            call frstpt (0., ymn)
            call vector (0., ymx)
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
c           ..label oma and omf axes
c
            if (m .eq. 1) then
               lbl = 'Obs - Forecast ' // unit
            else
               lbl = 'Obs - Analysis ' // unit
            endif
            len = len_trim (lbl)
            call lbl_axis (dmn, dmx, del, tck, lbl(1:len), 0.,
     *                     .031, .045, .false.)
c
c           ..label vertical axis
c
            if (m .eq. 1) then
               lbl = 'Depth (m)'
               len = len_trim (lbl)
               call lbl_axis (ymn, ymx, 200., 100., lbl(1:len), 90.,
     *                        .031, .08, .true.)
            endif
         enddo 
c
c        ..put titles on plot
c
         call set (.12, .97, .20, .78, .12, .97, .20, .78, 1)
         write (temp, '(a, 4x, ''Innovation Verification '', a)')
     *          trim (title1), adjustl (data_lbl(plt_typ(j)))
         call title_plot (temp,   .019, 1, 2.5)
         call title_plot (title3, .019, 1, 1.)
c
c        ..generate plot
c
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (temp)
         call frame
      enddo
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
