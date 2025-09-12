      subroutine vrfy_ssh_map (dtg, title1, title3, n_obs, n_data,
     *                         lat, lon, topo, topo_mn, n_proj,
     *                         rlat, stdlt1, stdlt2, stdlon, bl,
     *                         br, tl, tr)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vrfy_ssh_map
c
c DESCRIPTION:  plot locations of ssh observing systems 
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
      integer    MX_AMAP
      parameter (MX_AMAP = 36 000 000)
c
      integer    MX_AI
      parameter (MX_AI = 20 000)
c
      integer    MX_CNT
      parameter (MX_CNT = 64 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 80 000)
c
c     ..local array dimensions
c
      integer   n_obs
      integer   n_var
c
      real      bl(2), br(2)
      character dtg * 10
      integer   fno
      integer   i, k
      real      lat (n_obs)
      real      lon (n_obs)
      integer   n_data
      integer   n_proj
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      siz
      real      stdlon
      real      stdlt1, stdlt2
      character title1 * 80
      character title2 * 256
      character title3 * 256
      character tmp_name * 80
      real      topo (n_obs)
      real      topo_mn
      real      tl(2), tr(2)
      real      u, v
      integer   var_no
c
c     ..allocatable plot arrays
c
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycs (:)
c
c     ..external NCAR functions
c
      external  clin, colram
c
c     ..define plot position in frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
      include 'color_table.h'
c
c...............................executable..............................
c
c     ..allocate plot arrays
c
      allocate (iai (MX_AI))
      allocate (iag (MX_AI))
      allocate (iamap (MX_AMAP))
      allocate (xcs (MX_LIN))
      allocate (ycs (MX_LIN))
c
c     ..initialize
c
      fno = 0
c
c     ..set gmeta file name, open gks
c
      write (tmp_name, '(''ssh_maps.'', a, ''.gmeta '')') dtg
      call init_gks ('opn', tmp_name)
c
c     ..set gks colors
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..set map background
c
      call map_bkg (n_proj, rlat, stdlt1, stdlt2, stdlon, bl, br, tl,
     *              tr, pos1, pos2, pos3, pos4, iamap, mx_amap)
c
c     ..color map land areas
c
      call gsfais (1)
      call gsfaci (2)
      call arscam (iamap, xcs, ycs, MX_LIN, iai, iag, MX_AI, colram)
c
c     ..add lines of longitudes and latitudes masked by land fill
c
      call gsplci (1)
      call gstxci (1)
      call gsln (2)
      call mapgrm (iamap, xcs, ycs, MX_LIN, iai, iag, MX_AI, clin)
      call gsln (1)
c
c     ..draw continent outlines
c
      call mplndr ('Earth..3', 1)
      call maplbl
c
c     ..mark obs positions
c
      call gsmk (2)
      call gsmksc (.03)
      call gspmci (4)
      do i = 1, n_data
         if (topo(i) .gt. topo_mn) then
            call maptrn (lat(i), lon(i), u, v)
            call gpm (1, u, v)
         endif
      enddo
      call plotit (0, 0, 0)
c
c     ..put title on plot
c
      call gsplci (1)
      title2 = trim (title1) // '    SSH'
      call title_plot (title2, .02, 1, 2.5)
      call title_plot (title3, .02, 1, 1.)
c
c     ..generate plot
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': SSH Obs'')') fno
      call frame
c
c     ..close gks and clean up
c
      call init_gks ('cls', tmp_name)
      deallocate (iai, iag, iamap, xcs, ycs)
c
      return
      end
