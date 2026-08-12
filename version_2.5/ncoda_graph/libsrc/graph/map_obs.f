      subroutine map_obs (dir_path, date, dtg, nest, n_lon, n_lat, 
     *                    igrid, rlat, stdlt1, stdlt2, stdlon, bl,
     *                    br, tl, tr, zoom, iamap, mx_amap, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_obs
c
c DESCRIPTION:  routine to plot the observation distribution in the
c               analysis area.  mulitple plots are made for the
c               different observing systems.
c      
c PARAMETERS:
c     Name         Type       Usage            Description
c   ---------    --------    -------    ----------------------------
c   n_obs        integer     input      number of observations
c   lat          real        input      observation latitudes
c   lon          real        input      observation longitudes
c   typ          integer     input      observation data types
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
      integer    MX_TYP
      parameter (MX_TYP = 13)
c
      integer    N_VAR
      parameter (N_VAR = 7)
c
      integer    UNIT
      parameter (UNIT = 22)
c
c     ..local array dimensions
c
      integer   mx_amap
c
      real      bl(2), br(2)
      character date * 15
      character dir_path * (*)
      character dtg * 10
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, k, n
      integer   iamap (mx_amap)
      integer   igrid
      integer   len, len_dir
      real      lft
      character lvl_typ * 3
      integer   n_lon, n_lat
      integer   n_obs
      integer   n_typ
      integer   nest
      real      rgt
      real      rlat
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      character title * 132
      real      tl(2), tr(2)
      integer   typ_clr (MX_TYP)
      character typ_lbl (MX_TYP) * 13
      character var_prm (N_VAR) * 3
      logical   zoom
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
c     ..allocatable arrays
c
      integer,  allocatable :: clr (:)
      real,     allocatable :: obs_lat (:)
      real,     allocatable :: obs_lon (:)
      integer,  allocatable :: obs_typ (:)
      real,     allocatable :: siz (:)
      real,     allocatable :: xln (:)
      real,     allocatable :: ylt (:)
c
c     ..set analysis variable names
c
      data      var_prm / 'ICE', 'SST', 'SSS', 'SSH',
     *                    'TMP', 'SAL', 'VEL' /
c
c...............................executable..............................
c
      do k = 1, N_VAR
c
c     ..set field parameters
c
      if (var_prm(k) .eq. 'ICE') then
         fld_name = 'icecov'
      else if (var_prm(k) .eq. 'SST') then
         fld_name = 'seatmp'
      else if (var_prm(k) .eq. 'SSS') then
         fld_name = 'salint'
      else if (var_prm(k) .eq. 'SSH') then
         fld_name = 'seahgt'
      else if (var_prm(k) .eq. 'TMP') then
         fld_name = 'ocnobs'
      else if (var_prm(k) .eq. 'SAL') then
         fld_name = 'ocnobs'
      else if (var_prm(k) .eq. 'VEL') then
         fld_name = 'ocnobs'
      else
         cycle
      endif 
      file_typ = 'obsdata'
      fluid = 'o'
      lvl_typ = 'sfc'
      tau = 0
      len_dir = len_trim (dir_path) + 1
c
c     ..check for and open innovation file
c
      call cr_fname (dir_path, dtg, nest, n_lon, n_lat, file_typ,
     *               fld_name, fluid, lvl_typ, tau, file_name,
     *               len)
      inquire (file=file_name(1:len), exist=exist)
      if (.not. exist) cycle
      write (*, '(''       restart found: '', a)')
     *       file_name(len_dir:len)
      open (UNIT, file=file_name(1:len), status='unknown',
     *            form='unformatted')
      read (UNIT) n_obs
c
c     ..allocate arrays
c
      allocate (clr (n_obs))
      allocate (obs_lat (n_obs))
      allocate (obs_lon (n_obs))
      allocate (obs_typ (n_obs))
      allocate (siz (n_obs))
      allocate (xln (n_obs))
      allocate (ylt (n_obs))
c
c     ..read innovation vector; save lat, lon, type
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
c     ..ICE
c
      if (var_prm(k) .eq. 'ICE' .and. .not. zoom) then
         n = 0
         do i = 1, n_obs
            if (obs_typ(i) .eq. 78) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 4
               siz(n) = 0.001
            else if (obs_typ(i) .eq. 11) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 3
               siz(n) = 0.001
            else if (obs_typ(i) .eq. 83) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 5
               siz(n) = 0.001
            else
               write (*, '(''MAP_OBS: unknown ICE type: '', i10)')
     *                obs_typ(i)
            endif
         enddo
         if (n .gt. 0) then
            n_typ = 2
            typ_lbl(1) = 'Sea Ice'
            typ_clr(1) = 4
            typ_lbl(2) = 'Suppl Ice'
            typ_clr(2) = 3
            typ_lbl(3) = 'Shore Ice'
            typ_clr(3) = 5
            lft = 0.4
            rgt = 0.6
            write (title, '(''Sea Ice Observations'', 2x, a)') date
            call plot_obs (n_obs, n, xln, ylt, clr, siz, n_typ,
     *                     typ_clr, typ_lbl, lft, rgt, title,
     *                     igrid, rlat, stdlt1, stdlt2, stdlon,
     *                     bl, br, tl, tr, iamap, mx_amap, fno)
        endif
      endif
c
c     ..SST
c
      if (var_prm(k) .eq. 'SST') then
         n = 0
         do i = 1, n_obs
            if (obs_typ(i) .eq. 79) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 4
               siz(n) = 0.001
            else if (obs_typ(i) .eq. 114) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 3
               siz(n) = 0.001
            else if (obs_typ(i) .eq. 41) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 6
               siz(n) = 0.001
            else
               write (*, '(''MAP_OBS: unknown SST type: '', i10)')
     *                obs_typ(i)
            endif
         enddo
         if (n .gt. 0) then
            n_typ = 3
            typ_lbl(1) = 'SST'
            typ_clr(1) = 4
            typ_lbl(2) = 'Ice Temp' 
            typ_clr(2) = 3
            typ_lbl(3) = 'Suppl SST'
            typ_clr(3) = 6
            lft = 0.3
            rgt = 0.7
            write (title, '(''SST Observations'', 2x, a)') date
            call plot_obs (n_obs, n, xln, ylt, clr, siz, n_typ,
     *                     typ_clr, typ_lbl, lft, rgt, title, 
     *                     igrid, rlat, stdlt1, stdlt2, stdlon,
     *                     bl, br, tl, tr, iamap, mx_amap, fno)
         endif
      endif
c
c     ..SSS
c
      if (var_prm(k) .eq. 'SSS') then
         n = 0
         do i = 1, n_obs
            if (obs_typ(i) .eq. 173) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 4
               siz(n) = 0.001
            else if (obs_typ(i) .eq. 127) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 6
               siz(n) = 0.001
            else
               write (*, '(''MAP_OBS: unknown SSS type: '', i10)')
     *                obs_typ(i)
            endif
         enddo
         if (n .gt. 0) then
            n_typ = 2
            typ_lbl(1) = 'SSS'
            typ_clr(1) = 4
            typ_lbl(2) = 'Suppl SSS'
            typ_clr(2) = 6
            lft = 0.4
            rgt = 0.6
            write (title, '(''SSS Observations'', 2x, a)') date
            call plot_obs (n_obs, n, xln, ylt, clr, siz, n_typ,
     *                     typ_clr, typ_lbl, lft, rgt, title,
     *                     igrid, rlat, stdlt1, stdlt2, stdlon, 
     *                     bl, br, tl, tr, iamap, mx_amap, fno)
         endif
      endif
c
c     ..SSH
c     
      if (var_prm(k) .eq. 'SSH') then
         n = 0 
         do i = 1, n_obs
            if (obs_typ(i) .eq. 81) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 4
               siz(n) = 0.001
            else
               write (*, '(''MAP_OBS: unknown SSH type: '', i10)')
     *                obs_typ(i)
            endif
         enddo
         if (n .gt. 0) then
            n_typ = 0
            typ_lbl(1) = 'SSH'
            typ_clr(1) = 4
            lft = 0.47
            rgt = 0.53
            write (title, '(''SSH Observations'', 2x, a)') date
            call plot_obs (n_obs, n, xln, ylt, clr, siz, n_typ,
     *                     typ_clr, typ_lbl, lft, rgt, title,
     *                     igrid, rlat, stdlt1, stdlt2, stdlon,
     *                     bl, br, tl, tr, iamap, mx_amap, fno)
         endif
      endif
c
c     ..temperature
c
      if (var_prm(k) .eq. 'TMP') then
         n = 0
         do i = 1, n_obs
            if (obs_typ(i) .eq. 1 .or. obs_typ(i) .eq. 186) then
c     ..XBT/XCTD
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 3
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 20) then
c     ..TESAC
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 7
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 36) then
c     ..Argo
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 4
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 188) then
c     ..Alamo
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 10
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 4 .or. obs_typ(i) .eq. 208) then
c     ..fixed buoy / ocean site
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 6
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 5) then
c     ..drifting buoy
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 12
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 102) then
c     ..glider
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 8
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 133) then
c     ..animal
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 11
               siz(n) = 0.01
            else if (obs_typ(i) .eq.  15 .or. obs_typ(i) .eq.  19 .or.
     *               obs_typ(i) .eq. 171) then
c     ..synthetics
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 9
               if (zoom) then
                  siz(n) = 0.005
               else
                  siz(n) = 0.0005
               endif
            else if (obs_typ(i) .eq. 79) then
c     ..SST
               n = n + 1
               xln(n) = obs_lon(i) 
               ylt(n) = obs_lat(i)
               clr(n) = 5
               if (zoom) then
                  siz(n) = 0.002
               else
                  siz(n) = 0.0005
               endif
            else if (obs_typ(i) .eq.   0 .or. obs_typ(i) .eq.  32 .or.
     *               obs_typ(i) .eq.  33 .or. obs_typ(i) .eq.  37 .or.
     *               obs_typ(i) .eq.  49 .or. obs_typ(i) .eq.  50 .or.
     *               obs_typ(i) .eq.  51 .or. obs_typ(i) .eq.  52 .or.
     *               obs_typ(i) .eq.  54 .or. obs_typ(i) .eq.  65 .or.
     *               obs_typ(i) .eq.  87 .or. obs_typ(i) .eq.  88 .or.
     *               obs_typ(i) .eq. 103 .or. obs_typ(i) .eq. 106 .or.
     *               obs_typ(i) .eq. 127 .or. obs_typ(i) .eq. 134 .or.
     *               obs_typ(i) .eq. 141 .or. obs_typ(i) .eq. 142 .or.
     *               obs_typ(i) .eq. 143 .or. obs_typ(i) .eq. 144 .or.
     *               obs_typ(i) .eq. 173 .or. obs_typ(i) .eq. 183 .or.
     *               obs_typ(i) .eq. 184 .or. obs_typ(i) .eq. 187 .or.
     *               obs_typ(i) .eq. 189 .or. obs_typ(i) .eq. 209) then
c     ..salinity or velocity or fake data
               cycle
            else
               write (*, '(''MAP_OBS: unknown TMP type: '', i10,
     *                2f10.2)') obs_typ(i), obs_lat(i), obs_lon(i)
            endif
         enddo
         if (n .gt. 0) then
            n_typ = 10
            typ_lbl(1) = 'XBT/XCTD'
            typ_clr(1) = 3
            typ_lbl(2) = 'TESAC'
            typ_clr(2) = 7
            typ_lbl(3) = 'Argo'
            typ_clr(3) = 4
            typ_lbl(4) = 'Alamo'
            typ_clr(4) = 10
            typ_lbl(5) = 'Fixed'
            typ_clr(5) = 6
            typ_lbl(6) = 'Drifter'
            typ_clr(6) = 12
            typ_lbl(7) = 'Glider'
            typ_clr(7) = 8
            typ_lbl(8) = 'Animal'
            typ_clr(8) = 11
            typ_lbl(9) = 'SSH'
            typ_clr(9) = 9
            typ_lbl(10) = 'SST'
            typ_clr(10) = 5
            lft = 0.01
            rgt = 0.99
            write (title, '(''Temperature Observations'', 2x, a)')
     *             date
            call plot_obs (n_obs, n, xln, ylt, clr, siz, n_typ, 
     *                     typ_clr, typ_lbl, lft, rgt, title,
     *                     igrid, rlat, stdlt1, stdlt2, stdlon,
     *                     bl, br, tl, tr, iamap, mx_amap, fno)
         endif
      endif
c
c     ..salinity
c
      if (var_prm(k) .eq. 'SAL') then
         n = 0
         do i = 1, n_obs
            if (obs_typ(i) .eq. 187) then
c     ..XCTD
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 3
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 32) then
c     ..TESAC
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 7
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 37) then
c     ..Argo
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 4
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 189) then
c     ..Alamo
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 10
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 52 .or. obs_typ(i) .eq. 209) then
c     ..fixed buoy / ocean site
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 6
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 54) then
c     ..drifting buoy
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 12
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 103) then
c     ..glider
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 8
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 134) then
c     ..animal
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 11
               siz(n) = 0.01
            else if (obs_typ(i) .eq.  65) then
c     ..model salinity
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 9
               siz(n) = 0.01
            else if (obs_typ(i) .eq.  33 .or. obs_typ(i) .eq.  49) then
c     ..derived from T/S
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 9
               if (zoom) then
                  siz(n) = 0.005
               else
                  siz(n) = 0.0005
               endif
            else if (obs_typ(i) .eq.  173) then
c     ..sss
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 5
               if (zoom) then
                  siz(n) = 0.002
               else
                  siz(n) = 0.0005
               endif
            else if (obs_typ(i) .eq.   0 .or. obs_typ(i) .eq.   1 .or.
     *               obs_typ(i) .eq.   4 .or. obs_typ(i) .eq.   5 .or.
     *               obs_typ(i) .eq.  19 .or. obs_typ(i) .eq.  20 .or.
     *               obs_typ(i) .eq.  36 .or. obs_typ(i) .eq.  50 .or.
     *               obs_typ(i) .eq.  51 .or. obs_typ(i) .eq.  79 .or.
     *               obs_typ(i) .eq.  87 .or. obs_typ(i) .eq.  88 .or.
     *               obs_typ(i) .eq. 102 .or. obs_typ(i) .eq. 106 .or.
     *               obs_typ(i) .eq. 127 .or. obs_typ(i) .eq. 133 .or.
     *               obs_typ(i) .eq. 141 .or. obs_typ(i) .eq. 142 .or.
     *               obs_typ(i) .eq. 143 .or. obs_typ(i) .eq. 144 .or.
     *               obs_typ(i) .eq. 183 .or. obs_typ(i) .eq. 184 .or.
     *               obs_typ(i) .eq. 186 .or. obs_typ(i) .eq. 188 .or.
     *               obs_typ(i) .eq. 208) then
c     ..temperature or velocity or fake data
               cycle
            else
               write (*, '(''MAP_OBS: unknown SAL type: '', i10)')
     *                obs_typ(i)
            endif
         enddo
         if (n .gt. 0) then
            n_typ = 10
            typ_lbl(1) = 'XCTD'
            typ_clr(1) = 3
            typ_lbl(2) = 'TESAC'
            typ_clr(2) = 7
            typ_lbl(3) = 'Argo'
            typ_clr(3) = 4
            typ_lbl(4) = 'Alamo'
            typ_clr(4) = 10
            typ_lbl(5) = 'Fixed'
            typ_clr(5) = 6
            typ_lbl(6) = 'Drifter'
            typ_clr(6) = 12
            typ_lbl(7) = 'Glider'
            typ_clr(7) = 8
            typ_lbl(8) = 'Animal'
            typ_clr(8) = 11
            typ_lbl(9) = 'SSH'
            typ_clr(9) = 9
            typ_lbl(10) = 'SSS'
            typ_clr(10) = 5
            lft = 0.01
            rgt = 0.99
            write (title, '(''Salinity Observations'', 2x, a)')
     *             date
            call plot_obs (n_obs, n, xln, ylt, clr, siz, n_typ, 
     *                     typ_clr, typ_lbl, lft, rgt, title,
     *                     igrid, rlat, stdlt1, stdlt2, stdlon,
     *                     bl, br, tl, tr, iamap, mx_amap, fno)
         endif
      endif
c     
c     ..VELOCITY
c
      if (var_prm(k) .eq. 'VEL') then
         n = 0  
         do i = 1, n_obs
            if (obs_typ(i) .eq. 105) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 4
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 85) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 3
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 87) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 5
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 89) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 7
               siz(n) = 0.01
            else if (obs_typ(i) .eq. 161) then
               n = n + 1
               xln(n) = obs_lon(i)
               ylt(n) = obs_lat(i)
               clr(n) = 6
               siz(n) = 0.01
            else
               cycle
c              write (*, '(''MAP_OBS: unknown VEL type: '', i10)')
c    *                obs_typ(i)
            endif
         enddo
         if (n .gt. 0) then
            n_typ = 5
            typ_lbl(1) = 'ADCP'
            typ_clr(1) = 4
            typ_lbl(2) = 'HF Radar'
            typ_clr(2) = 3
            typ_lbl(3) = 'Drifter'
            typ_clr(3) = 5
            typ_lbl(4) = 'Glider'
            typ_clr(4) = 7
            typ_lbl(5) = 'Altimeter'
            typ_clr(5) = 6
            lft = 0.2
            rgt = 0.8
            write (title, '(''Velocity Observations'', 2x, a)') date
            call plot_obs (n_obs, n, xln, ylt, clr, siz, n_typ,
     *                     typ_clr, typ_lbl, lft, rgt, title, 
     *                     igrid, rlat, stdlt1, stdlt2, stdlon,
     *                     bl, br, tl, tr, iamap, mx_amap, fno)
         endif
      endif
c
c     ..clean up
c
      deallocate (clr, obs_lat, obs_lon, obs_typ, siz, xln, ylt)
c
      enddo
c
      return
      end
