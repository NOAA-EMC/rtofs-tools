      subroutine plot_diff (dir_path, dtg, nest, m, n, l, depth, lvl,
     *                      mask, bl, br, tl, tr, i1, i2, j1, j2, nz,
     *                      z_plot, gln, glt, node_eq, tau, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_diff
c
c DESCRIPTION:  maps differences between forecasts separated by the
c               update cycle interval
c      
c PARAMETERS:
c       Name          Type       Usage            Description
c   -------------   ----------   -----   -----------------------------
c   date            char         input   dtg plot label
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
      integer    MX_AMAP
      parameter (MX_AMAP = 96 000 000)
c
c     ..local array dimensions
c
      integer   m, n, l
      integer   nz
      integer   gln, glt
c
      character arc_path * 256
      real      bl(2), br(2)
      real      cntr_int
      real      dmn, dmx
      character date1 * 11 
      character date2 * 6
      integer   day
      real      depth (m * n)
      character dir_path * (*)
      character dtg * 10
      logical   fail1, fail2
      character file_dtg1 * 10
      character file_dtg2 * 10
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j, k
      integer   i1, i2, j1, j2
      integer   len
      integer   lntr
      real      lvl (100)
      character lvl_typ * 3
      integer   mask (m * n)
      integer   mon
      character month (12) * 3
      real      node_eq ((gln * glt), 2)
      integer   ndx
      integer   nest
      real      pos1, pos2, pos3, pos4
      real      spval
      integer   status
      integer   tau, tau1, tau2
      character title * 132
      real      tl(2), tr(2)
      integer   year
      integer   z_lev
      integer   z_plot (50)
c
c     ..allocatable arrays
c
      real,     allocatable :: anm (:)
      real,     allocatable :: fld (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: wrk1 (:)
      real,     allocatable :: wrk2 (:)
c
      include 'color_table.h'
c
c     ..define map position in plot frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
c     ..define month labels
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (anm (m * n * l))
      allocate (fld (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk1 (m * n * l))
      allocate (wrk2 (m * n * l))
c
c     ..initialize
c
      cntr_int = 0.
      file_typ = 'fcstfld'
      fluid = 'o'
      lntr = 5
      lvl_typ = 'pre'
      call gks_color (rgb_anm_clr, MX_ANM_CLR)
c
c     ..create forecast dtgs and date labels
c
      if (tau .gt. 24) then
c
c        ..retrieve hycom archive directory path
c
         call GETENV ('OCN_ARCHV_DIR', arc_path)
c***************************
      arc_path = '/scratch2/NCEPDEV/marine/Jim.Cummings/rtofs_da/' //
     *           'ncoda_run3/archv'
c*************************
         len = len_trim (arc_path)
         if (len .eq. 0) then
            write (*, '(''missing OCN_ARCHV_DIR path '',
     *                  ''environmental variable'')')
            return
         endif
         write (*, '(''   archive directory: '', a)') trim (arc_path)
         read (dtg(5:6), '(i2)') mon
         read (dtg(7:8), '(i2)') day
         write (date1, '(i2.2, 1x, a)') day, month(mon)
         tau1 = tau
         tau2 = tau - 24
      else
         tau1 = tau * 2
         tau2 = tau
         call dtgmod (dtg, -tau1, file_dtg1, status)
         call dtgmod (dtg, -tau2, file_dtg2, status)
         read (file_dtg2(1:4), '(i4)') year
         read (file_dtg2(5:6), '(i2)') mon
         read (file_dtg2(7:8), '(i2)') day
         write (date1, '(i2.2, 1x, a, 1x, i4)')
     *          day, month(mon), year
         read (dtg(5:6), '(i2)') mon
         read (dtg(7:8), '(i2)') day
         write (date2, '(i2.2, 1x, a)') day, month(mon)
      endif
c
c     ..loop over model variables
c
      do k = 1, 2
         if (k .eq. 1) then
c
c           ..temperature
c
            fld_name = 'seatmp'
            dmn = -1.5
            dmx =  1.5
         else if (k .eq. 2) then
c
c           ..salinity
c
            fld_name = 'salint'
            dmn = -0.6
            dmx =  0.6
         else if (k .eq. 3) then
c
c           ..u velocity
c
            fld_name = 'uucurr'
            dmn = -30.
            dmx =  30.   
         else if (k .eq. 4) then
c
c           ..v velocity
c
            fld_name = 'vvcurr'
            dmn = -30.
            dmx =  30.   
         endif
c
         anm = 0.
         wrk1 = 0.
         wrk2 = 0.
c
c        ..read fields
c
         if (tau .gt. 24) then
            call rd_archv_file (dtg, arc_path, m, n, l, fld_name, 
     *                          tau1, wrk2, spval, fail1)
            call rd_archv_file (dtg, arc_path, m, n, l, fld_name,
     *                          tau2, wrk1, spval, fail2)
         else
            call rd_coda_file (dir_path, file_dtg1, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau, 
     *                         lvl_typ, wrk1, .true., fail1)
            call rd_coda_file (dir_path, file_dtg2, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau, 
     *                         lvl_typ, wrk2, .true., fail2)
         endif
         if (fail1 .or. fail2) cycle
c
c        ..compute masked differences
c
         do j = 1, l
            do i = 1, (m * n)
               ndx = i + (j-1) * m * n
               if (j .gt. mask(i)) then
                  anm(ndx) = 0.
               else
                  anm(ndx) = wrk2(ndx) - wrk1(ndx)
               endif
            enddo
         enddo
c
c        ..loop over selected plot levels
c
         do j = 1, nz
            z_lev = int (lvl(z_plot(j)) + .001)
c
c           ..extract field
c
            do i = 1, (m * n)
               ndx = i + (z_plot(j)-1) * m * n
               fld(i) = anm(ndx)
               if (k .eq. 3 .or. k .eq. 4) then
                  fld(i) = fld(i) * 100.
               endif
            enddo
c
c           ..set plot title
c
            if (k .eq. 1) then
            write (title, '(''Temperature Forecast Difference (C)'',
     *                      4x, ''Layer'', i3)') z_plot(j)
            else if (k .eq. 2) then
            write (title, '(''Salinity Forecast Difference (PSU)'',
     *                      4x, ''Layer'', i3)') z_plot(j)
            else if (k .eq. 3) then
            write (title, '(''U Velocity Forecast Difference (cm/s)'',
     *                      4x, ''Layer'', i3)') z_plot(j)
            else if (k .eq. 4) then
            write (title, '(''V Velocity Forecast Difference (cm/s)'',
     *                      4x, ''Layer'', i3)') z_plot(j)
            endif
c
c           ..plot field
c
            call glb_merc (fld, depth, dmn, dmx, m, n, gln, 
     *                     glt, bl, br, tl, tr, i1, i2, j1,
     *                     j2, node_eq, pos1, pos2, pos3, 
     *                     pos4, n_slca, iamap, MX_AMAP,
     *                     cntr_int, .false., lntr, z_lev,
     *                     spval)
c
c           ..add title plots
c
            call title_plot (title, .018, 1, 2.1)
            fno = fno + 1
            write (*, '(10x, ''frame'', i5, '': '', a)')
     *             fno, trim (title)
            if (tau .gt. 24) then
            write (title, '(a, 4x, ''tau '', i3, '' - '', i3)') 
     *             date1, tau2, tau1
            else
            write (title, '(a, '' - '', a)') date2, date1
            endif
            call title_plot (title, .018, 1, 1.)
            call frame
         enddo
      enddo
c
c     ..clean up
c
      deallocate (anm, fld, iamap, wrk1, wrk2)
c
      return
      end
      subroutine rd_archv_file (dtg, arc_path, n_lon, n_lat, n_lvl,
     *                          fld_name, fcst_tau, fld, spval,
     *                          fail)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  rd_archv_file
c
c DESCRIPTION:  reads hycom forecast archv fields for a certain 
c               forecast period
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libsetup.a
c
c PARAMETERS:
c       Name        Type      Usage            Description
c   -----------   --------   -------   -------------------------------
c   arc_path      char       input     hycom archv directory path
c   dtg           char       input     analysis date time group
c   fcst_tau      integer    input     forecast tau to read
c   fld_name      char       input     archive file variable
c   n_lon         integer    input     number x grid positions
c   n_lat         integer    input     number y grid positions
c   n_lvl         integer    input     number analysis z positions
c   spval         real       input     special (missing) value
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c   file names: archv.2017_217_12.a (DEV - THEIA)
c               archv.2017111412_2017111512.a (NAVO - GOFS3.1)
c   directory path: same as archs* and arche* files
c
c   archv* file content: 12 surface fields, then 5 model fields each layer
c
c       surface                         layers
c     1. montg1        13. u-vel       23. u-vel       33. u-vel
c     2. srfhgt        14. v_vel       24. v-vel       34. v-vel
c     3. steric        15. thknss      25. thknss      35. thknss
c     4. surflx        16. temp        26. temp        36. temp
c     5. salflx        17. salin       27. salin       37. salin
c     6. bl_dpth  
c     7. mix_dpth      18. u-vel       28. u-vel       38. u-vel
c     8. covice        19. v_vel       29. v-vel       39. v-vel
c     9. thkice        20. thknss      30. thknss      40. thknss
c    10. temice        21. temp        31. temp        41. temp
c    11. u_btrop       22. salin       32. salin       42. salin
c    12. v_btrop                         ...
c
c RECORD OF CHANGES:
c   Initial Installation - April 1994 -- Cummings, J.
c
c..............................END PROLOGUE.............................
c
      implicit none
c
      integer    UNIT
      parameter (UNIT = 22)
c
c     ..local array dimensions
c
      integer   n_lat
      integer   n_lon
      integer   n_lvl      
c
      character arc_path * (*)
      character ctau2 * 2
      character ctau3 * 3
      character dtg * 10
      logical   exist
      logical   fail
      integer   fcst_tau
      character file_name * 256
      character fld_name * 6
      integer   i, j, k
      integer   irec
      integer   len
      integer   ndx
      integer   n_fld  
      integer   n_skip
      integer   reclen
      character sfx * 64
      real      spval
c
c     ..forecast fld
c
      real      fld (n_lon, n_lat, n_lvl)
c
c     ..allocatable array
c
      real,     allocatable :: wrk (:,:)
c
c...............................executable.............................
c
c     ..initialize hycom field parameters
c
      n_fld = 5
      n_skip = 12
      if (fld_name .eq. 'seatmp') then
         ndx = 4
      else if (fld_name .eq. 'salint') then
         ndx = 5
      else
         write (*, '('' fld_name: "'', a, ''" not supported'')')
     *          fld_name
         return
      endif
c
c     ..hycom archv forecast file
c
      if (fcst_tau .gt. 99) then
         write (ctau3, '(i3.3)') fcst_tau
         sfx = '/rtofs.' // dtg(1:8) // '/rtofs_glo.t' //
     *         dtg(9:10) // 'z.f' // ctau3 // '.archv.a'
      else
         write (ctau2, '(i2.2)') fcst_tau
         sfx = '/rtofs.' // dtg(1:8) // '/rtofs_glo.t' //
     *         dtg(9:10) // 'z.f' // ctau2 // '.archv.a'
      endif
c
      file_name = trim (arc_path) // trim (sfx)
      len = len_trim (file_name)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) then
         write (*, '(''     missing archv file: '', a)') trim (sfx)
         fail = .true.
         return
      else
         write (*, '(''        read archv file: '', a)') trim (sfx)
         fail = .false.
      endif
      reclen = (((n_lon * n_lat + 4095) / 4096) * 4096) * 4
      open (UNIT, file=file_name(1:len), form='unformatted',
     *            access='direct', recl=reclen, status='old')
c
c     ..read select variable
c
      allocate (wrk (n_lon, n_lat))
      do k = 1, n_lvl
         irec = (k-1) * n_fld + ndx + n_skip
         read (UNIT, rec=irec) wrk
         do j = 1, n_lat
            do i = 1, n_lon
               fld(i,j,k) = wrk(i,j)
            enddo
         enddo
      enddo
      deallocate (wrk)
      close (UNIT)
c
c     ..apply mask
c 
      do j = 1, n_lat
         do i = 1, n_lon
            do k = 1, n_lvl
               if (fld(i,j,k) .gt. 1.e30) fld(i,j,k) = spval
            enddo
         enddo
      enddo
c
      return
      end


