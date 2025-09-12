      subroutine plot_sens (date, dtg, adj_tau, z_lvl, l, n_plot,
     *                      z_plot, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_sens
c
c DESCRIPTION:  maps adjoint sensitivty (or forecast error) fields
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
c     ..set number variables
c
      integer    N_VAR
      parameter (N_VAR = 4)
c
      integer    UNIT
      parameter (UNIT = 22)
c
c     ..local array dimensions
c
      integer   m, n, l
c
      character adj_dir * 256
      character adj_dtg * 10
      integer   adj_tau
      real      bl(2), br(2)
      real      cntr_int
      character date * 15
      real      dmn, dmx
      logical   do_cntr
      logical   do_lbl
      character dtg * 10
      character err_msg * 256
      logical   exist
      logical   fail
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j, k
      integer   i1, i2, j1, j2
      character lbl * 12
      integer   len, len_adj
      integer   lntr
      character lvl_typ * 3
      integer   n_lvl
      integer   n_nodes
      integer   n_pass
      integer   n_plot
      integer   n_proj
      integer   ndx
      integer   nest
      integer   nslc
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval
      integer   status
      real      stdlon
      real      stdlt1, stdlt2
      character title1 * 132
      character title2 * 132
      real      tl(2), tr(2)
      integer   z_lev
      real      z_lvl (100)
      integer   z_plot (50)
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: datao (:)
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
c     ..get adjoint data directory path
c
      call GETENV ('OCN_ADJ_DIR', adj_dir)
      len = len_trim (adj_dir)
      if (len .eq. 0) then
         write (err_msg, '(''OCN_ADJ_DIR missing'')')
         call error_exit ('PLOT_SENS', err_msg)
      else
         write (*, '(''   adjoint directory: '', a)') trim (adj_dir)
         len_adj = len_trim (adj_dir) + 1
      endif
c
c     ..read datao header record
c
      m = 2000
      n = 1
      allocate (datao (2000))
      call cr_fname (adj_dir, dtg, n, m, n, 'infofld', 'datahd',
     *               'o', 'hdr', 0, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='old',
     *             access='stream', form='unformatted')
         read (UNIT) datao
         close (UNIT)
         write (*, '(''       restart found: '', a)')
     *          file_name(len_adj:len)
      else
         write (err_msg, '(a, '' datahd file missing'')') dtg
         write (*, '(''fn: '', a)') trim (file_name)
         call error_exit ('PLOT_SENS', err_msg)
      endif
c
c     ..initialize grid definition
c
      n_proj = nint (datao(3))
      m = nint (datao(30+0))
      n = nint (datao(30+1))
      rlat = datao(7)
      stdlon = datao(6)
      stdlt1 = datao(4)
      stdlt2 = datao(5)
      bl(1) = datao(30+6)
      bl(2) = datao(30+7)
      br(1) = datao(30+8)
      br(2) = datao(30+9)
      tr(1) = datao(30+10)
      tr(2) = datao(30+11)
      tl(1) = datao(30+12)
      tl(2) = datao(30+13)
c
c     ..set adjoint base dtg
c
      call dtgmod (dtg, -adj_tau, adj_dtg, status)
c
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (fld (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n * l))
c
c     ..initialize plot variables
c
      btm_msk = 0.
      cntr_int = -1.
      do_cntr = .false.
      do_lbl = .true.
      fluid = 'o'
      lvl_typ = 'pre'
      nest = 1
      n_nodes = m * n
      n_pass = 1
      file_typ = 'sensfld'
      call gks_color (rgb_anm_clr, MX_ANM_CLR)
      nslc = n_slca
      lntr = 5
c
c     ..loop over analysis field types
c
      do k = 1, N_VAR
         if (k .eq. 1) then
c
c           ..temperature
c
            fld_name = 'seatmp'
            lvl_typ = 'pre'
            n_lvl = l
            dmn = -1.2
            dmx =  1.2
            lbl = 'Temperature'
            write (title2, '(i2, '' hr Forecast Error '',
     *             ''Sensitivity (C)'')') adj_tau
         else if (k .eq. 2) then
c
c           ..salinity
c
            fld_name = 'salint'
            lvl_typ = 'pre'
            n_lvl = l
            dmn = -0.6
            dmx =  0.6
            lbl = 'Salinity'
            write (title2, '(i2, '' hr Forecast Error '',
     *             ''Sensitivity (PSU)'')') adj_tau
         else if (k .eq. 3) then
c
c           ..ice coverage northern hemisphere
c
            fld_name = 'icecov'
            lvl_typ = 'inh'
            n_lvl = 1
            dmn = -15.
            dmx =  15.
            lbl = 'Ice Coverage'
            write (title2, '(i2, '' hr Forecast Error '',
     *             ''Sensitivity (%)'')') adj_tau
         else if (k .eq. 4) then
c
c           ..ice coverage southern hemisphere
c
            fld_name = 'icecov'
            lvl_typ = 'ish'
            n_lvl = 1
            dmn = -15.
            dmx =  15.
            lbl = 'Ice Coverage'
            write (title2, '(i2, '' hr Forecast Error '',
     *             ''Sensitivity (%)'')') adj_tau
         endif
c
c        ..read field
c
         call rd_coda_file (adj_dir, adj_dtg, nest, m, n, n_lvl,
     *                      file_typ, fld_name, fluid, adj_tau,
     *                      lvl_typ, wrk, .true., fail)
         if (.not. fail) then
c
c           ..loop over selected plot levels
c
            do j = 1, n_plot
               z_lev = int (z_lvl(z_plot(j)) + .001)
c
c              ..extract and smooth field
c
               do i = 1, n_nodes
                  ndx = i + (z_plot(j)-1) * m * n
                  fld(i) = wrk(ndx)
               enddo
               call smth_2d (n_pass, fld, m, n, spval)
c
c              ..set plot title
c
               write (title1, '(a, 5x, a, 5x, i4, '' M Depth'')')
     *                trim (lbl), date, z_lev
c
c              ..plot field
c
               call map_bkg (n_proj, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3,
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (fld, dmn, dmx, m, n, 1, m, 1, n, 
     *                           nslc, iamap, MX_AMAP, btm_msk,
     *                           cntr_int, .false., do_cntr, .true.,
     *                           do_lbl, .false., lntr, spval)
c
               call title_plot (title1, .018, 1, 2.05)
               call title_plot (title2, .018, 1, 1.05)
               fno = fno + 1
               write (*, '(10x, ''frame'', i5, '': '', a)')
     *                fno, trim (title1)
               call frame
            enddo
         endif
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, datao, fld, iamap, wrk)
c
      return
      end
