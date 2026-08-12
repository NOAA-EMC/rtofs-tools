      subroutine plot_polar_data (var_prm, dir_path, date, dtg, nest,
     *                            opt, m, n, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_polar_data
c
c DESCRIPTION:  plots arctic and antarctic basin observation locations
c               used in NCODA analysis
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
      integer    MX_AI
      parameter (MX_AI = 20 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 80 000)
c
      integer    UNIT
      parameter (UNIT = 22)
c
c     ..local array dimensions
c
      integer   m, n
      integer   n_obs
c
      real      bl(2), br(2)
      integer   clr
      character date * 15
      character dir_path * (*)
      character dtg * 10
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i
      integer   len, len_dir
      character lvl_typ * 3
      integer   n_data
      integer   n_proj
      integer   nest
      character opt * 2
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      siz
      real      slon
      real      stdlt1, stdlt2
      integer   tau
      character title * 80
      real      tl(2), tr(2)
      real      u, v
      character var_prm * 3
c
c     ..dummy innovation vector variables
c
      real      obs_age
      real      obs_anl
      real      obs_bkg
      real      obs_ebk
      real      obs_eob
      real      obs_lvl
      integer   obs_ndx
      character obs_sgn * 7
      real      obs_val
      integer   obs_var
      real      obs_xi
      real      obs_yj
      real      obs_zk
c
c     ..allocatable obs arrays
c
      real,     allocatable :: obs_lat (:)
      real,     allocatable :: obs_lon (:)
      integer,  allocatable :: obs_typ (:)
      real,     allocatable :: xln (:)
      real,     allocatable :: ylt (:)
c
c     ..allocatable ncar arrays
c
      integer,  allocatable :: iamap (:)
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iag (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycs (:)
c
c     ..external NCAR functions
c
      external  clin, colram
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
c     ..set field parameters
c
      if (var_prm .eq. 'ICE') then
         fld_name = 'icecov'
      else 
         fld_name = 'ocnobs'
      endif
      clr = 4 
      file_typ = 'obsdata'
      fluid = 'o'
      lvl_typ = 'sfc'
      siz = 0.001
      tau = 0
      len_dir = len_trim (dir_path) + 1
c
c     ..check for and open innovation file
c
      call cr_fname (dir_path, dtg, nest, m, n, file_typ,
     *               fld_name, fluid, lvl_typ, tau, 
     *               file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) return
      write (*, '(''       restart found: '', a)')
     *       file_name(len_dir:len)
      open (UNIT, file=file_name(1:len), status='unknown',
     *            form='unformatted')
      read (UNIT) n_obs
c
c     ..allocate obs arrays
c
      allocate (obs_lat (n_obs))
      allocate (obs_lon (n_obs))
      allocate (obs_typ (n_obs))
      allocate (xln (n_obs))
      allocate (ylt (n_obs))
c
c     ..read innovation vector: save lat, lon, type
c
      read (UNIT) obs_age
      read (UNIT) obs_lat(1:n_obs)
      read (UNIT) obs_lon(1:n_obs)
      read (UNIT) obs_lvl
      read (UNIT) obs_ndx
      read (UNIT) obs_ebk
      read (UNIT) obs_eob
      read (UNIT) obs_typ(1:n_obs)
      read (UNIT) obs_var
      read (UNIT) obs_val
      read (UNIT) obs_anl
      read (UNIT) obs_bkg
      read (UNIT) obs_xi
      read (UNIT) obs_yj
      read (UNIT) obs_zk
      read (UNIT) obs_sgn
      close (UNIT)
c
c     ..allocate ncar arrays
c
      allocate (iamap (MX_AMAP))
      allocate (iai (MX_AI))
      allocate (iag (MX_AI))
      allocate (xcs (MX_LIN))
      allocate (ycs (MX_LIN))
c
c     ..set color scheme
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..set map backgroud
c
      if (opt .eq. 'nh') then
         bl(1) = 38.654
         bl(2) = 255.000
         br(1) = 38.604
         br(2) = 345.064
         tl(1) = 38.604
         tl(2) = 164.936
         tr(1) = 38.554
         tr(2) = 75.000
         n_proj = 3
         rlat = 90.
         slon = 300.
         stdlt1 = 60.
         stdlt2 = 60.
      else if (opt .eq. 'sh') then
         bl(1) = -38.654
         bl(2) = 165.000
         br(1) = -38.604
         br(2) = 255.064
         tl(1) = -38.604
         tl(2) = 74.936
         tr(1) = -38.554
         tr(2) = 345.000
         n_proj = 3
         rlat = -90.
         slon = 300.
         stdlt1 = -60.
         stdlt2 = -60.
      else
         write (*, '(''unknown option: '', a)') opt
         return
      endif
      call map_bkg (n_proj, rlat, stdlt1, stdlt2, slon, bl, br, tl,
     *              tr, pos1, pos2, pos3, pos4, iamap, mx_amap)
c
c     ..color map land areas 
c
      call gsfais (1)
      call gsfaci (2)
      call arscam (iamap, xcs, ycs, MX_LIN, iai, iag, MX_AI, colram)
c
c     ..draw continent outlines
c
      call mplndr ('Earth..3', 1)
      call maplbl
c
c     ..plot satellite obs
c
      if (var_prm .eq. 'ICE') then
         n_data = 0
         do i = 1, n_obs
            if (obs_typ(i) .eq. 78) then
               n_data = n_data + 1
               xln(n_data) = obs_lon(i)
               ylt(n_data) = obs_lat(i)
            endif
         enddo
         if (n_data .gt. 0) then
            call pcseti ('FN - fontcap number', 20)
            do i = 1, n_data
               call maptra (ylt(i), xln(i), u, v)
               if (u .lt. 1.e10) then
                  call gsplci (clr)
                  call plchhq (u, v, 'L', siz, 0., 0.)
                  call plotit (0, 0, 0)
               endif
            enddo
            call pcseti ('FN - fontcap number', 0)
            write (title, '(''Sea Ice Observations'', 2x, a)') date
        endif
c
c     ..SST
c
      else if (var_prm .eq. 'SST') then
         n_data = 0
         do i = 1, n_obs
            if (obs_typ(i) .eq. 79) then
               n_data = n_data + 1
               xln(n_data) = obs_lon(i)
               ylt(n_data) = obs_lat(i)
            endif
         enddo
         if (n_data .gt. 0) then
            call pcseti ('FN - fontcap number', 20)
            do i = 1, n_data
               call maptra (ylt(i), xln(i), u, v)
               if (u .lt. 1.e10) then
                  call gsplci (clr)
                  call plchhq (u, v, 'L', siz, 0., 0.)
                  call plotit (0, 0, 0)
               endif
            enddo
            call pcseti ('FN - fontcap number', 0)
            write (title, '(''SST Observations'', 2x, a)') date
         endif
c
c     ..SSS
c
      else if (var_prm .eq. 'SSS') then
         n_data = 0
         do i = 1, n_obs
            if (obs_typ(i) .eq. 173) then
               n_data = n_data + 1
               xln(n_data) = obs_lon(i)
               ylt(n_data) = obs_lat(i)
            endif
         enddo
         if (n_data .gt. 0) then
            call pcseti ('FN - fontcap number', 20)
            do i = 1, n_data
               call maptra (ylt(i), xln(i), u, v)
               if (u .lt. 1.e10) then
                  call gsplci (clr)
                  call plchhq (u, v, 'L', siz, 0., 0.)
                  call plotit (0, 0, 0)
               endif
            enddo
            call pcseti ('FN - fontcap number', 0)
            write (title, '(''SSS Observations'', 2x, a)') date
         endif
      endif
c
c     ..put title on plot
c
      call title_plot (title, .02, 1, 1.)
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)') fno, trim (title)
      call frame
c
c     ..clean up
c
      deallocate (iamap, iai, iag, xcs, ycs)
      deallocate (obs_lat, obs_lon, obs_typ, xln, ylt)
c
      return
      end
