      subroutine map_raw_sss (dir_path, date, dtg, nest, n_lon, n_lat,
     *                        igrid, rlat, stdlt1, stdlt2, stdlon, bl,
     *                        br, tl, tr, iamap, mx_amap, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_raw_sss
c
c DESCRIPTION:  routine to plot raw sss observation distributions in
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
      parameter (N_CLR = 1)
c
      integer    N_SSS_DAT
      parameter (N_SSS_DAT = 2)
      integer    N_SSS_TYP
      parameter (N_SSS_TYP = 1)
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
      character file_name * 256
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
      character sss_dat (N_SSS_DAT) * 4
      integer   sss_typ (N_SSS_TYP, N_SSS_DAT) 
c
c     ..color arrays
c
      integer   clr1 (N_CLR)
      integer   clr2 (N_CLR)
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
c     ..set SSS data and type codes
c
      data sss_dat / 'SMOS', 'SMAP' /
      data sss_typ / 
c     ..SMOS
     *              174,
c     ..SMAP
     *              126 /
c
c     ..set color type codes
c
      data clr1   / 174 /
      data clr2   / 126 /
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
c     ..sea surface salinity
c
      call cr_fname (dir_path, dtg, nest, n_lon, n_lat, 'rawdata',
     *               'salint', 'o', 'sfc', zero, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      len_dir = len_trim (dir_path) + 1
      if (exist) then
         write (*, '(''       restart found: '', a)')
     *          file_name(len_dir:len)
         open (UNIT, file=file_name(1:len), status='unknown',
     *               form='unformatted')
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
     *          file_name(len_dir:len)
         return
      endif
c
      if (mod (N_SSS_DAT, 4) .eq. 0) then
         n_page = N_SSS_DAT / 4
      else
         n_page = N_SSS_DAT / 4 + 1
      endif
      do kk = 1, n_page
         k = 4 * (kk-1)
         call gks_color (rgb_obs_clr, MX_OBS_CLR)
         do jj = 1, 4
            j = k + jj
            if (j .le. N_SSS_DAT) then
               call pcseti ('FN - fontcap number', 0)
               call plot_obs_raw (sss_dat(j), igrid, rlat, stdlt1,
     *                            stdlt2, stdlon, bl, br, tl, tr,
     *                            iamap, mx_amap, pos(1,jj))
               do n = 1, N_SSS_TYP
                  do i = 1, n_obs
                     if (obs_typ(i) .eq. sss_typ(n,j)) then
                        clr = 1
                        do m = 1, N_CLR
                           if (obs_typ(i) .eq. clr1(m)) clr = 4
                           if (obs_typ(i) .eq. clr2(m)) clr = 5
                        enddo
                        mrk = 2
                        siz = 0.1
                        call maptra (obs_lat(i), obs_lon(i), u, v)
                        call gsmk   (mrk)
                        call gsmksc (siz)
                        call gspmci (clr)
                        call gpm (1, u, v)
                        call plotit (0, 0, 0)
                     endif
                  enddo    
               enddo
            endif
         enddo
         call pcseti ('FN - fontcap number', 0)
         write (title, '(''SSS Observations'', 2x, a)') date
         call set (.05, .90, .05, .90, .05, .90, .05, .90, 1)
         call title_plot (title, .02, 1, 1.8)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title)
         call frame
      enddo
c
c     ..clean up
c
      deallocate (obs_lat, obs_lon, obs_typ)
c
      return
      end
