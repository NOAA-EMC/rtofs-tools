      subroutine plot_ensm (dir_path, date, dtg, nest, m, n, l, 
     *                      depth, z_lvl, igrid, rlat, stdlt1, 
     *                      stdlt2, stdlon, bl, br, tl, tr, i1,
     *                      i2, j1, j2, nz, z_plot, gln, glt, 
     *                      node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_ensm
c
c DESCRIPTION:  maps ncoda_et perturbation fields for each member
c               levels plotted are under user control
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
      parameter (MX_AMAP = 64 000 000)
c
      integer    N_MEM
      parameter (N_MEM = 8)
c
      integer    N_VAR
      parameter (N_VAR = 4)
c
      integer    UNIT
      parameter (UNIT = 10)
c
c     ..local array dimensions
c
      integer   m, n, l
      integer   nz
      integer   gln, glt
c
      real      bl(2), br(2)
      real      cntr_int (4)
      real      depth (m * n)
      real      dmn (4), dmx (4)
      character date * 15
      character dir_path * (*)
      logical   do_btm
      logical   do_cntr
      logical   do_lbl
      character dtg * 10
      character err_msg * 256
      logical   exist   
      character fld_lbl (N_VAR) * 17
      character file_name * 256
      character fld_name (N_VAR) * 6
      integer   fno
      integer   i, k
      integer   i1, i2, j1, j2
      integer   igrid
      integer   len, len_dir
      integer   lntr
      character lvl * 3
      integer   ndx
      integer   nest
      integer   nm, nv
      real      node_eq ((gln * glt), 2)
      real      pos1, pos2, pos3, pos4
      real      rlat
      character sfx * 64
      real      spval
      real      stdlon
      real      stdlt1, stdlt2
      character title * 132
      real      tl(2), tr(2)
      character typ * 3
      integer   z_lev
      real      z_lvl (100)
      integer   z_plot (50)
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: fld (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: wrk (:)
c
      include 'color_table.h'
c
c     ..define map position in plot frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
c...............................executable..............................
c
c     ..retrieve ensemble directory path
c
      call GETENV ('OCN_ENSM_DIR', dir_path)
      len = len_trim (dir_path)
      if (len .eq. 0) then
         write (err_msg, '(''missing OCN_ENSM_DIR environmental '',
     *                     ''variable'')')
         call error_exit ('PLOT_ENSM', err_msg)
      endif
c
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (fld (m * n * l))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n))
c
c     ..initialize variable attributes
c
      fld_lbl(1) = 'Temperature (C) '
      fld_name(1) = 'seatmp'
      dmn(1) = -1.5
      dmx(1) =  1.5
      cntr_int(1) = 0.5
c
      fld_lbl(2) = 'Salinity (PSU)  '
      fld_name(2) = 'salint'
      dmn(2) = -0.6
      dmx(2) =  0.6
c**************************
c     dmn(2) = -0.3
c     dmx(2) =  0.3
c**********************
      cntr_int(2) = 0.2
c
      fld_lbl(3) = 'U Velocity (cm/s)'
      fld_name(3) = 'uucurr'
      dmn(3) = -75.
      dmx(3) =  75.
      cntr_int(3) = 25.
c
      fld_lbl(4) = 'V Velocity (cm/s)'
      fld_name(4) = 'vvcurr'
      dmn(4) = -75.
      dmx(4) =  75.
      cntr_int(4) = 25.
c
      fno = 0
      len_dir = len_trim (dir_path) + 1
      typ = 'inc'
      lvl = 'pre'
c
c     ..set gks and plot environment
c
      call gks_color (rgb_anm_clr, MX_ANM_CLR)
      do_btm = .true.
      do_cntr = .false.
      do_lbl = .true.
      lntr = 5
c
c     ..loop over analysis variables
c
c     do nv = 1, n_var
      do nv = 1, 2
c
c     ..loop over ensmeble members
c
      do nm = 1, N_MEM
c
c        ..check for and read increment field
c
         write (sfx, '(a, ''_'', a, ''_'', a, ''_mem'', i3.3,
     *                 ''.'', a)')
     *                 fld_name(nv), typ, lvl, nm, dtg
         file_name = trim (dir_path) // '/' // trim (sfx)
         len = len_trim (file_name)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) then
            write (*, '(''        reading file: '', a)')
     *          file_name(len_dir:len)
            open (UNIT, file=file_name(1:len), status='old',
     *                  access='stream', form='unformatted')
            read (UNIT) fld
            close (UNIT)
         else   
            cycle
         endif  
c***********************************************
      write (*,'(''dmn: '', f10.2)') minval (fld, mask=fld.gt.spval)
      write (*,'(''dmx: '', f10.2)') maxval (fld)
c***********************************************
c
c        ..scale velocities from m/s to cm/s
c
         if (nv .eq. 3 .or. nv .eq. 4) then
            do i = 1, (m * n * l)
               fld(i) = fld(i) * 100.
            enddo
         endif
c
c        ..loop over selected plot levels
c
         do k = 1, nz
c
c           ..set bottom mask
c
            do i = 1, (m * n)
               if (depth(i) .lt. (z_lvl(z_plot(k))+1.01)) then
                  btm_msk(i) = -1.
               else
                  btm_msk(i) = 1.
               endif
            enddo
c
c           ..extract field
c
            do i = 1, (m * n)
               ndx = i + (z_plot(k)-1) * m * n
               wrk(i) = fld(ndx)
            enddo
c
c           ..set plot title
c
            z_lev = int (z_lvl(z_plot(k)) + .001)
            write (title, '(a, 3x, ''Member '', i3, 3x, i4,
     *                      '' M Depth'')') fld_lbl(nv), nm,
     *                      z_lev
c
c           ..plot field
c
            if (igrid .lt. 0) then
               call glb_merc (wrk, depth, dmn(nv), dmx(nv),
     *                        m, n, gln, glt, bl, br, tl, tr,
     *                        i1, i2, j1, j2, node_eq, pos1,
     *                        pos2, pos3, pos4, n_slca, iamap,
     *                        MX_AMAP, cntr_int(nv), do_cntr, 
     *                        lntr, z_lev, spval)
            else
               call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3,
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (wrk, dmn(nv), dmx(nv), m, n, i1,
     *                           i2, j1, j2, n_slca, iamap, MX_AMAP,
     *                           btm_msk, cntr_int(nv), do_btm, 
     *                           do_cntr, .true., do_lbl, .false.,
     *                           lntr, spval)
            endif
            call title_plot (title, .018, 1, 2.1)
            fno = fno + 1
            write (*, '(10x, ''frame'', i5, '': '', a)')
     *             fno, trim (title)
            write (title, '(a)') date
            call title_plot (title, .018, 1, 1.)
            call frame
         enddo
      enddo
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, fld, iamap, wrk)
c
      return
      end
