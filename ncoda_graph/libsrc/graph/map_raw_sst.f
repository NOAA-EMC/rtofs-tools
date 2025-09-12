      subroutine map_raw_sst (dir_path, date, dtg, nest, n_lon, n_lat,
     *                        igrid, rlat, stdlt1, stdlt2, stdlon, bl,
     *                        br, tl, tr, iamap, mx_amap, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_raw_sst
c
c DESCRIPTION:  routine to plot raw observation distributions in
c               the analysis area.  
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
      integer    N_CLR
      parameter (N_CLR = 72)
c
      integer    N_SST_DAT
      parameter (N_SST_DAT = 11)
      integer    N_SST_TYP
      parameter (N_SST_TYP = 12)
c
      integer    UNIT
      parameter (UNIT = 22)
c
c     ..local array dimensions
c
      integer   mx_amap
c
      real      bl (2), br (2)
      character date * 15
      character dir_path * (*)
      character dtg * 10
      logical   exist
      integer   clr
      logical   fail
      character file_name * 256
      character fld_name * 6
      integer   fno
      integer   i, j, jj, k, kk, m, n
      integer   iamap (mx_amap)
      integer   igrid
      integer   len, len_dir
      integer   mrk
      integer   n_lat, n_lon
      integer   n_obs
      integer   n_page
      integer   nest
      real      obs_lvl
      integer   obs_ndx
      character obs_sgn * 7
      real      obs_val
      real      pos (4, 4)
      real      rlat
      real      siz
      real      stdlon
      real      stdlt1, stdlt2
      character title * 132
      real      tl (2), tr (2)
      real      u, v
      integer   zero
c
c     ..data type arrays
c
      character sst_dat (N_SST_DAT) * 9
      character sst_lbl (N_SST_DAT) * 3
      integer   sst_typ (N_SST_TYP, N_SST_DAT) 
c
c     ..color arrays
c
      integer   clr1 (N_CLR)
      integer   clr2 (N_CLR)
      integer   clr3 (N_CLR)
      integer   clr4 (N_CLR)
c
c     ..allocatable arrays
c
      real,     allocatable :: obs_lat (:)
      real,     allocatable :: obs_lon (:)
      integer,  allocatable :: obs_typ (:)
c
c     ..set color tables
c
      include 'color_table.h'
c
c     ..set SST data and type codes
c
      data sst_dat / 'AMSR     ', 'GOES     ', 'HIMAWARI ', 
     *               'NOAA     ', 'METOP    ', 'MSG      ',
     *               'NPP      ', 'SLSTR    ', 'Ship     ',
     *               'Buoy     ', 'Ice Sfc  ' /
      data sst_lbl / 'ams', 'goe', 'him', 'jps', 'mtp', 'msg',
     *               'npp', 'sls', 'shp', 'shp', 'ice' /
      data sst_typ / 
c     ..AMSR
     *               77, 169,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,
c     ..GOES
     *               12,  13,  14,  59,  60,  67,  68,  76, 183,
     *              184, 185, 206,
c     ..HIMAWARI
     *              131, 132, 181, 182, 204, 205,  -1,  -1,  -1,
     *               -1,  -1,  -1,
c     ..NOAA-VIIRS
     *              119, 123, 124, 145, 166, 168,  -1,  -1,  -1,
     *               -1,  -1,  -1,
c     ..METOP
     *              110, 115, 120, 111, 116, 121, 112, 117, 122,
     *               -1,  -1,  -1,
c     ..MSG
     *               99, 100, 207, 171, 172, 180,  -1,  -1,  -1,
     *               -1,  -1,  -1,
c     ..NPP-VIIRS
     *              153, 154, 155,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,
c     ..SLSTR
     *              151, 152, 165, 194,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,
c     ..Ship
     *                3,  21,  22,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,
c     ..Buoy
     *                4,   5,  23,  84,  -1,  -1,  -1,  -1,  -1, 
     *               -1,  -1,  -1,
c     ..Ice Sfc Temp
     *              146, 147, 148, 150, 156, 190, 191,  -1,  -1,
     *               -1,  -1,  -1 /
c
c     ..set color type codes (clr1=day, clr2=night, clr3=rlx day)
c
      data clr1   /   1,   2,   3,   4,   8,  12,  15,  17,  19,
     *               20,  24,  36,  43,  47,  53,  55,  56,  59,
     *               61,  63,  64,  65,  66,  67,  68,  69,  70,
     *               74,  80,  85,  89,  93,  94,  97,  99, 102,
     *              107, 110, 113, 115, 118, 120, 123, 128, 131,
     *              133, 135, 138, 139, 145, 147, 151, 152, 153,
     *              157, 159, 161, 163, 165, 166, 167, 169, 170, 
     *              171, 175, 179, 180, 181, 183, 190, 192, 197 /
      data clr2   /   5,   6,   9,  13,  18,  21,  24,  25,  44,
     *               48,  57,  60,  62,  68,  71,  72,  75,  76,
     *               77,  86,  90,  91,  92,  93,  95,  98, 100,
     *              104, 108, 111, 114, 116, 121, 124, 125, 127, 
     *              129, 132, 136, 140, 145, 148, 152, 154, 158,
     *              160, 162, 164, 166, 171, 172, 174, 176, 182,
     *              184, 186, 188, 191, 194, 199,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1 /
      data clr3   /   7,  10,  14,  22,  23,  26,  45,  58,  76,
     *               87,  96, 109, 112, 117, 119, 122, 126, 130,
     *              150, 155, 168, 180, 185, 196, 204, 205, 206,
     *              207, 208,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1 /
      data clr4   /  29,  84, 156, 185,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
     *               -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1 /
c
c     ..define positions in plot frame
c
      data pos    / 0.05, 0.45, 0.50, 0.90,
     *              0.50, 0.90, 0.50, 0.90,
     *              0.05, 0.45, 0.05, 0.45,
     *              0.50, 0.90, 0.05, 0.45 /
c
c     ..set forecast period
c
      data zero    / 0 /
c
c...............................executable..............................
c
c     ..check sst raw data files
c
      fail = .true.
      do j = 1, N_SST_DAT
         fld_name = sst_lbl(j) // 'sst'
         call cr_fname (dir_path, dtg, nest, n_lon, n_lat,
     *                  'rawdata', fld_name, 'o', 'sfc',
     *                  zero, file_name, len)
         inquire (file=file_name(1:len), exist=exist)
         if (exist) fail = .false.
      enddo
      if (fail) return
c
c     ..sst
c
      if (mod (N_SST_DAT, 4) .eq. 0) then
         n_page = N_SST_DAT / 4
      else
         n_page = N_SST_DAT / 4 + 1
      endif
      len_dir = len_trim (dir_path) + 1
      do kk = 1, n_page
         k = 4 * (kk-1)
         call gks_color (rgb_obs_clr, MX_OBS_CLR)
         do jj = 1, 4
            j = k + jj
            if (j .le. N_SST_DAT) then
               fld_name = sst_lbl(j) // 'sst'
            else
               fld_name = 'dmysst'
            endif
            call cr_fname (dir_path, dtg, nest, n_lon, n_lat, 
     *                     'rawdata', fld_name, 'o', 'sfc',
     *                     zero, file_name, len)
            inquire (file=file_name(1:len), exist=exist)
            if (exist) then
               write (*, '(''       restart found: '', a)')
     *                file_name(len_dir:len)
               open (UNIT, file=file_name(1:len), status='unknown',
     *                     form='unformatted')
               read (UNIT) n_obs
               allocate (obs_lat (n_obs))
               allocate (obs_lon (n_obs))
               allocate (obs_typ (n_obs))
               read (UNIT) obs_lat(1:n_obs)
               read (UNIT) obs_lon(1:n_obs)
               read (UNIT) obs_lvl
               read (UNIT) obs_ndx
               read (UNIT) obs_typ(1:n_obs)
               read (UNIT) obs_val
               read (UNIT) obs_sgn
               close (UNIT)
            else
               write (*, '(''        file missing: '', a)')
     *                file_name(len_dir:len)
               n_obs = 0
            endif
            if (j .le. N_SST_DAT) then
               call pcseti ('FN - fontcap number', 0)
               call plot_obs_raw (sst_dat(j), igrid, rlat, stdlt1,
     *                            stdlt2, stdlon, bl, br, tl, tr,
     *                            iamap, mx_amap, pos(1,jj))
               do n = 1, N_SST_TYP
               if (n_obs .gt. 0) then
                  do i = 1, n_obs
                     if (obs_typ(i) .eq. sst_typ(n,j)) then
                        clr = 1
                        do m = 1, N_CLR
                           if (obs_typ(i) .eq. clr1(m)) clr = 4
                           if (obs_typ(i) .eq. clr2(m)) clr = 5
                           if (obs_typ(i) .eq. clr3(m)) clr = 6
                           if (obs_typ(i) .eq. clr4(m)) clr = 7
                        enddo
                        mrk = 2
                        if (sst_dat(j)(1:4) .eq. 'Ship' .or.
     *                      sst_dat(j)(1:4) .eq. 'Buoy') then
                           siz = 0.8
                        else
                           siz = 0.1
                        endif
                        call maptra (obs_lat(i), obs_lon(i), u, v)
                        call gsmk   (mrk)
                        call gsmksc (siz)
                        call gspmci (clr)
                        call gpm (1, u, v)
                        call plotit (0, 0, 0)
                     endif
                  enddo  
               endif  
               enddo
            endif
            if (n_obs .gt. 0) then
               deallocate (obs_lat, obs_lon, obs_typ)
            endif
         enddo
         call pcseti ('FN - fontcap number', 0)
         write (title, '(''SST Observations'', 2x, a)') date
         call set (.05, .90, .05, .90, .05, .90, .05, .90, 1)
         call title_plot (title, .02, 1, 1.8)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title)
         call frame
      enddo
c
      return
      end
