      subroutine model_point (run_title, out_dir, dtg1, dtg2, n_lon,
     *                        n_lat, n_lvl, ix, jy, msk, z_lvl, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  model_point
c
c DESCRIPTION:  plots time series of analysis or model forecast at a
c               specified grid point.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name          Type       Usage            Description
c   --------    ---------   -------   ---------------------------------
c    dtg1       character    input    starting dtg
c    dtg2       character    input    ending dtg
c    fno        integer      input    sequential frame number
c    ix, jy     integer      input    grid point coordinates
c    msk        integer      input    grid mask
c    n_lon      integer      input    number grid longitudes
c    n_lat      integer      input    number grid latitude
c    n_lvl      integer      input    number grid levels
c    out_dir    character    input    directory path
c    z_lvl      real         input    analysis grid levels
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
      integer    N_TRP
      parameter (N_TRP = 301)
c
      integer    N_VAR
      parameter (N_VAR = 5)
c
c     ..local array dimensions
c
      integer   n_lat
      integer   n_lon
      integer   n_lvl
c
      real      cntr
      character date1 * 11
      character date2 * 11
      integer   day
      real      del, tck
      real      dmn, dmx
      character dtg * 10
      character dtg1 * 10
      character dtg2 * 10
      logical   fail
      character file_dtg * 10
      character file_typ * 7
      character fld_name (N_VAR) * 6
      character fluid * 1
      integer   fno
      integer   hrs
      integer   i, k, m, n
      integer   ix, jy
      integer   lntr
      character lvl_typ * 3
      integer   mon
      character month (12) * 3
      integer   msk
      integer   nest
      integer   n_clrs
      integer   n_files
      character out_dir * (*)
      real      pos (4)
      character run_title * 80
      real      siz
      real      spmis
      real      spval
      integer   status
      integer   tau
      character title * 256
      integer   upd
      character var_lbl * 18
      real      xmn, xmx, ymn, ymx
      integer   year 
      real      z_lvl (100)
      real      zmn, zmx
c
c     ..allocatable arrays
c
      real,     allocatable :: data (:,:)
      real,     allocatable :: fld (:,:,:)
      character,allocatable :: lbl (:) * 2
      real,     allocatable :: prs (:,:,:)
      real,     allocatable :: xin (:)
      real,     allocatable :: xout (:)
      real,     allocatable :: zin (:)
      real,     allocatable :: zout (:)
c
      include 'color_table.h'
c
c     ..define field names
c
      data fld_name / 'seatmp', 'salint', 'uucurr', 'vvcurr',
     *                'lyrprs' /
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c     ..set plot position
c
      data      pos / .1, .9, .1, .9 /
c
c...............................executable..............................
c
c     ..form date time group labels
c
      read (dtg1(1:4), '(i4)') year
      read (dtg1(5:6), '(i2)') mon
      read (dtg1(7:8), '(i2)') day
      write (date1, '(i2, 1x, a, 1x, i4)')
     *       day, month(mon), year
c
      read (dtg2(1:4), '(i4)') year
      read (dtg2(5:6), '(i2)') mon
      read (dtg2(7:8), '(i2)') day
      write (date2, '(i2, 1x, a, 1x, i4)')
     *       day, month(mon), year
c
c     ..set file parameters
c
      tau = 0
c     tau = 24
      if (tau .gt. 0) then
         file_typ = 'fcstfld'
      else
         file_typ = 'analinc'
      endif
      fluid = 'o'
      nest = 1
      spval = -999.
      spmis = spval + 9.
      upd = 24
      zmn = 0.
      zmx = z_lvl(msk)
c****************
      zmx = 1200.
c****************
c
c     ..set number files to process
c
      call dtgdif (dtg1, dtg2, hrs, status)
      n_files = hrs / upd + 1
c
c     ..allocate arrays
c   
      allocate (data (n_files, N_TRP))
      allocate (fld (n_lon, n_lat, n_lvl))
      allocate (lbl  (n_files))
      allocate (prs (n_lon, n_lat, n_lvl))
      allocate (xin  (n_lvl))
      allocate (xout (N_TRP))
      allocate (zin  (n_lvl))
      allocate (zout (N_TRP))
c
      del = zmx / real (N_TRP)
      do k = 1, N_TRP
         zout(k) =  real (k-1) * del
      enddo
c
c     ..diagnostics
c
      write (*, '(/, ''Model Point Processing'')')
      write (*, '(''        number files: '', i10)') n_files
      write (*, '(''          mask level: '', i10)') msk
      write (*, '(''           max depth: '', f10.2)') zmx
c
c     ..loop over variables
c
c     do n = 1, N_VAR
      do n = 5, 5
c
c        ..initialize
c
         data = 0.
         if (n .eq. 1) then
            if (tau .gt. 0) then
               dmn = 10.
               dmx = 28.
            else
               dmn = -1.5
               dmx =  1.5
            endif
            lntr = 10
c           lntr = 5
            lvl_typ = 'pre'
            var_lbl = 'Temperature (C)  '
            cntr = 1.
         else if (n .eq. 2) then
            if (tau .gt. 0) then
               dmn = 34.
               dmx = 34.9
            else
               dmn = -0.3
               dmx =  0.3
            endif
c           lntr = 10
            lntr = 5
            lvl_typ = 'pre'
            var_lbl = 'Salinity (PSU)   '
            cntr = 0.1
         else if (n .eq. 3) then
            if (tau .gt. 0) then
               dmn = 0.
               dmx = 30.
            else
               dmn = -30.
               dmx =  30.
            endif
c           lntr = 10
            lntr = 5
            lvl_typ = 'pre'
            var_lbl = 'U Velocity (cm/s)'
            cntr = 10.
         else if (n .eq. 4) then
            if (tau .gt. 0) then
               dmn = 0.
               dmx = 30.
            else
               dmn = -30.
               dmx =  30.
            endif
c           lntr = 10
            lntr = 5
            lvl_typ = 'pre'
            var_lbl = 'V Velocity (cm/s)'
            cntr = 10.
         else if (n .eq. 5) then
            if (tau .eq. 0) then
               dmn = -150.
               dmx = 150.
            else
               dmn = -150.
               dmx =  150.
            endif
            lntr = 5
            lvl_typ = 'lyr'
            var_lbl = 'Layer Pressure (m)'
         endif         
         xmn = 1.
         xmx = real (n_files)
         ymn = 1.
         ymx = real (N_TRP)
c
c        ..loop over files
c
         do i = 1, n_files
            hrs = (i-1) * upd
            call dtgmod (dtg1, hrs, dtg, status)
            lbl(i) = dtg(7:8)
            if (tau .gt. 0) then
               call dtgmod (dtg, -tau, file_dtg, status)
            else
               file_dtg = dtg
            endif
            if (n .eq. 5) then
               call rd_coda_file (out_dir, file_dtg, nest, n_lon,
     *                            n_lat, n_lvl, file_typ,
     *                            fld_name(n), fluid, tau, 
     *                            lvl_typ, fld, .true., fail)
               call rd_coda_file (out_dir, file_dtg, nest, n_lon,
     *                            n_lat, n_lvl, 'fcstfld',
     *                            fld_name(n), fluid, upd, 
     *                            lvl_typ, prs, .true., fail)
            else
               call rd_coda_file (out_dir, file_dtg, nest, n_lon,
     *                            n_lat, n_lvl, file_typ,
     *                            fld_name(n), fluid, tau, 
     *                            lvl_typ, fld, .true., fail)
            endif            
            if (fail) cycle
c
c           ..save point data
c
            do k = 1, msk
               if (n .eq. 3 .or. n .eq. 4) then
                  xin(k) = fld(ix,jy,k) * 100.
                  zin(k) = z_lvl(k)
               else if (n .eq. 5) then
                  xin(k) = fld(ix,jy,k)
                  zin(k) = prs(ix,jy,k) / 9806.
               else
                  xin(k) = fld(ix,jy,k)
                  zin(k) = z_lvl(k)
               endif
            enddo
            call prof_trp (msk, zin, xin, spval,
     *                     N_TRP, zout, xout)
            do k = 1, N_TRP
               m = N_TRP - k + 1
               data(i,m) = xout(k)
            enddo
         enddo      
c
c        ..plot cross section
c
         if (tau .gt. 0) then
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            n_clrs = n_slc
         else
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            n_clrs = n_slca
         endif
         call set (pos(1), pos(2), pos(3), pos(4),
     *             xmn, xmx, ymn, ymx, 1)
         call contour_point (n_files, N_TRP, data, dmn, dmx, 
     *                       n_clrs, cntr, lntr, spval)
c
         call gsplci (1)
         call plotif (pos(1), pos(3), 0)
         call plotif (pos(2), pos(3), 1)
         call plotif (pos(2), pos(4), 1)
         call plotif (pos(1), pos(4), 1)
         call plotif (pos(1), pos(3), 1)
         call plotif (pos(1), pos(3), 2)
c
         siz = .01
         do i = 1, n_files
            call plchhq (real(i), (ymn-0.03*(ymx-ymn)), lbl(i),
     *                   siz, 0., 0.)
            call line (real(i), ymn, real(i), (ymn-0.011*(ymx-ymn)))
         enddo
         call plchhq (((xmn+xmx)*0.5), (ymn-.055*(ymx-ymn)),
     *                'Time (days)', .012, 0., 0.)
c
         siz = .013
         del = 100.
         tck = 50.
         call lbl_axis (zmn, zmx, del, tck, 'Depth (m)', 90.,
     *                  siz, .07, .true.)
c         
         write (title, '(a, 3x, ''Tau '', i2, 3x, i4, 2x, i4)')
     *          var_lbl, tau, ix, jy
         call title_plot (title, .018, 1, 2.1)
         title = trim (run_title) // '   ' // date1 // ' to ' // date2
         call title_plot (title, .018, 1, 1.)
         call frame
      enddo
c
c     ..clean up 
c
      deallocate (data, fld, lbl, prs, xin, xout, zin, zout)
c
      return
      end
