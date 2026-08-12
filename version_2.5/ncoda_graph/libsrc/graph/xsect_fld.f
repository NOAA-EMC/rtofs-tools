      subroutine xsect_fld (dir_path, dtg, file_dtg, file_typ, plt_typ,
     *                      nest, tau, m, n, l, k, mask, grd, cntr_int,
     *                      dmn, dmx, do_anm, do_lbl, lntr, n_clrs,
     *                      title, lyr, fail, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  xsect_fld
c
c DESCRIPTION:  returns 3D field for vertical cross section plots
c
c PARAMETERS:
c       Name          Type       Usage            Description
c   -------------   ----------   -----   -----------------------------
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..local array dimensions
c
      integer   m, n, l
c
      real      cntr_int
      character dir_path * (*)
      real      dmn, dmx
      logical   do_anm
      logical   do_lbl
      character dtg * 10
      logical   fail
      character file_dtg * 10
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      real      grd (m * n * l)
      integer   hour
      integer   i, j, k
      integer   lntr
      character lvl_typ * 3
      logical   lyr
      integer   mask (m * n)
      integer   ndx
      integer   nest
      integer   n_clrs
      character plt_typ * 5
      character rd_dtg * 10
      real      spmis
      real      spval
      integer   tau
      character title * (*)
      character typ_file * 7
c****************
      real spv, svan, zero
      integer ix,jy
c***************
c
      include 'color_table.h'
c
c     ..allocatable arrays
c
      real,     allocatable :: wrk (:)
c********************
      real,     allocatable :: sal (:)
      real,     allocatable :: tmp (:)
c******************
c
c...............................executable..............................
c
c     ..initialize
c
      fluid = 'o'
      if (lyr) then
         lvl_typ = 'lyr'
      else
         lvl_typ = 'pre'
      endif
      spmis = spval + 9.
      if (tau .gt. 0) then
         rd_dtg = file_dtg
      else
         rd_dtg = dtg
      endif
      zero = 0.
c
      fail = .true.
      if (k .eq. 1) then
c
c        ..temperature (or vertical correlations)
c
         if (plt_typ .eq. 'vcorr') then
            fld_name = 'sclvrt'
            hour = 0
            typ_file = 'datafld'
            rd_dtg = dtg
         else
            fld_name = 'seatmp'
            hour = tau
            typ_file = file_typ
            rd_dtg = file_dtg
         endif
         call rd_coda_file (dir_path, rd_dtg, nest, m, n, l, typ_file,
     *                      fld_name, fluid, hour, lvl_typ, grd,
     *                      .true., fail)
         if (plt_typ .eq. 'incr ') then
            write (title, '(''Temperature Increment (C)'')')
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = 0.5
            do_anm = .true.
            do_lbl = .false.
            dmn = -1.5
            dmx =  1.5
            lntr = 5
            n_clrs = n_slca
         else if (plt_typ .eq. 'vcorr') then
            write (title, '(''Vertical Correlation Length '',
     *                       ''Scales (M)'')')
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            cntr_int = 20.
            do_anm = .false.
            do_lbl = .false.
            dmn = 0.
            dmx = 360.
c********************
      dmx = 300.
c********************
            lntr = 10
            n_clrs = n_slc
         else
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            n_clrs = n_slc
            if (plt_typ .eq. 'error') then
               write (title, '(''Temperature Error (C)'')')
               cntr_int = 0.2
               do_anm = .false.
               do_lbl = .false.
               dmn = 0.
               dmx = 1.2
               lntr = 10 
            else
               write (title, '(''Temperature (C)'')')
               cntr_int = 1.
               do_anm = .false.
               do_lbl = .false.
               dmn = 0.
               dmx = dmn + 0.5 * real (n_clrs)
c     dmn = 0.
c     dmx = 6.
               lntr = 10
            endif
         endif
      else if (k .eq. 2) then
c
c        ..salinity
c
         fld_name = 'salint'
         call rd_coda_file (dir_path, rd_dtg, nest, m, n, l,
     *                      file_typ, fld_name, fluid, tau,
     *                      lvl_typ, grd, .true., fail)
         if (plt_typ .eq. 'incr ') then
            write (title, '(''Salinity Increment (PSU)'')')
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = 0.1
            do_anm = .true.
            do_lbl = .false.
            dmn = -.3
            dmx = .3
            lntr = 5
            n_clrs = n_slca
         else
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            n_clrs = n_slc
            if (plt_typ .eq. 'error') then
               write (title, '(''Salinity Error (PSU)'')')
               cntr_int = 0.01
               do_anm = .false.
               do_lbl = .false.
               dmn = 0.
               dmx = 0.3
               lntr = 10
            else
               write (title, '(''Salinity (PSU)'')')
               cntr_int = 0.2
               do_anm = .false.
               do_lbl = .false.
               dmn = 32.
               dmx = dmn + 0.1 * real (n_clrs)
c*****************
      dmn = 33.
      dmx = 36.
c*****************
               lntr = 10
            endif
         endif
      else if (k .eq. 3 .and. tau .eq. 0) then
c
c        ..geopotential
c
         fld_name = 'geoptl'
         call rd_coda_file (dir_path, rd_dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, grd, 
     *                      .true., fail)
         if (plt_typ .eq. 'full ') then
            write (title, '(''Geopotential (M)'')')
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            cntr_int = 0.2
            do_anm = .false.
            do_lbl = .false.
            dmn = 0.
            dmx = 2.4
            lntr = 5
            n_clrs = n_slc
         else if (plt_typ .eq. 'incr ') then
            write (title, '(''Geopotential Increment (M)'')')
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = 0.05
            do_anm = .true.
            do_lbl = .false.
            dmn = -.15
            dmx =  .15
            lntr = 5
            n_clrs = n_slca
         endif
      else if (k .eq. 4) then
c
c        ..velocity
c
         allocate (wrk (m * n * l))
         fld_name = 'uucurr'
         call rd_coda_file (dir_path, rd_dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, grd,
     *                      .true., fail)
         fld_name = 'vvcurr'
         call rd_coda_file (dir_path, rd_dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, wrk,
     *                      .true., fail)
         do i = 1, (m * n * l)
            if (grd(i) .gt. spmis .and. wrk(i) .gt. spmis) then
               grd(i) = sqrt (grd(i)**2 + wrk(i)**2) * 100.
            else
               grd(i) = spval
            endif
         enddo
         deallocate (wrk)
         do_anm = .false.
         do_lbl = .false.
         if (plt_typ .eq. 'incr ') then
            write (title, '(''Velocity Increment (cm/s)'')')
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = 5.
            dmn = -15.
            dmx =  15.
            lntr = 5
            n_clrs = n_slca
         else 
            call gks_color (rgb_fld_clr, MX_FLD_CLR)
            n_clrs = n_slc
            if (plt_typ .eq. 'error') then
               write (title, '(''Velocity Error (cm/s)'')')
               cntr_int = 5.
               dmn = 0.
               dmx = 30.
               lntr = 10
            else
               write (title, '(''Velocity (cm/s)'')')
               cntr_int = 10.
               dmn = 0.
               dmx = dmn + 1. * real (n_clrs)
               lntr = 10
            endif
         endif 
      else if (k .eq. 5 .and. tau .eq. 0) then
c
c        ..layer pressure
c
         fld_name = 'lyrprs'
         call rd_coda_file (dir_path, rd_dtg, nest, m, n, l, file_typ,
     *                      fld_name, fluid, tau, lvl_typ, grd, 
     *                      .true., fail)
         if (plt_typ .eq. 'incr ') then
            write (title, '(''Pressure Increment (db)'')')
            call gks_color (rgb_anm_clr, MX_ANM_CLR)
            cntr_int = 20.
            do_anm = .false.
            do_lbl = .true.
            dmn = -150.
            dmx =  150.
            lntr = 5
            n_clrs = n_slca
         endif
      else if (k .eq. 6 .and. tau .eq. 0) then
c
c        ..climate density
c
         allocate (sal (m * n * l))
         allocate (tmp (m * n * l))
         file_typ = 'climfld'
         fld_name = 'seatmp'
         call rd_coda_file (dir_path, file_dtg, nest, m, n, l,
     *                      file_typ, fld_name, fluid, tau, 
     *                      lvl_typ, tmp, .true., fail)
         write (*,'(''tmp fail: '', l5)') fail
         fld_name = 'salint'
         call rd_coda_file (dir_path, file_dtg, nest, m, n, l,
     *                      file_typ, fld_name, fluid, tau,
     *                      lvl_typ, sal, .true., fail)
         write (*,'(''sal_fail: '', l5)') fail
         do i = 1, (m * n * l)
            if (tmp(i) .gt. spmis .and. sal(i) .gt. spmis) then
               spv = svan (sal(i), tmp(i), zero, grd(i))
            else
               grd(i) = spval
            endif
         enddo
         deallocate (sal, tmp)
         write (title, '(''Density (C)'')')
         call gks_color (rgb_fld_clr, MX_FLD_CLR)
         n_clrs = n_slc
         cntr_int = 0.2
         do_anm = .false.
         do_lbl = .false.
         dmn = 23.
         dmx = dmn + 0.1 * real (n_clrs)
         lntr = 10
      endif
c
c     ..mask plot grid
c
      if (.not. fail) then
         do j = 1, l
            do i = 1, (m * n)
               if (j .gt. mask(i)) then
                  ndx = i + (j-1) * m * n
                  grd(ndx) = spval
               endif
            enddo
         enddo
      endif
c
      return
      end
