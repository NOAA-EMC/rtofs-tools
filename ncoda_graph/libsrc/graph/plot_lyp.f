      subroutine plot_lyp (dir_path, date, dtg, tau, nest, m, n, l,
     *                     depth, lvl, igrid, rlat, stdlt1, stdlt2, 
     *                     stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *                     nz, z_plot, gln, glt, node_eq, fno, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_lyp
c
c DESCRIPTION:  maps a CODA 3D layer pressure increment field and
c               color fills the contours
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
      parameter (MX_AMAP = 36 000 000)
c
c     ..local array dimensions
c
      integer   m, n, l
      integer   nz
      integer   gln, glt
c
      real      bl (2), br (2)
      real      cntr_int
      real      dmn, dmx
      character date * 15
      real      depth (m * n)
      character dir_path * (*)
      logical   do_btm
      character dtg * 10
      logical   fail
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   i, j
      integer   i1, i2, j1, j2
      integer   igrid
      integer   lntr
      real      lvl (100)
      character lvl_typ * 3
      integer   n_nodes
      integer   n_pass
      integer   ndx
      integer   nest
      real      node_eq ((gln * glt), 2)
      integer   nslc
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval
      real      stdlon
      real      stdlt1, stdlt2
      integer   tau
      character title * 132
      real      tl (2), tr (2)
      integer   z_lev
      integer   z_plot (50)
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: field (:)
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
c     ..read analysis variables
c
      fld_name = 'lyrprs'
      lvl_typ = 'lyr'
      fluid = 'o'
      n_nodes = m * n
      n_pass = 1
c
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (field (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n * l))
c
c     ..loop over analysis field types
c
      fail = .true.
c
c     ..layer pressure increments
c
      file_typ = 'analinc'
      call rd_coda_file (dir_path, dtg, nest, m, n, l, file_typ,
     *                   fld_name, fluid, tau, lvl_typ, wrk,
     *                   .true., fail)
      call gks_color (rgb_anm_clr, MX_ANM_CLR)
      cntr_int = 10.
c     dmn = -150.
c     dmx =  150.
      lntr = 5        
      nslc = n_slca
c*************************
c     do i = 1, (m * n * l)
c     if (wrk(i) .gt. -990.) wrk(i) = wrk(i) * 9806.
c     enddo
      dmn = minval (wrk, mask=wrk.gt.spval)
      dmx = maxval (wrk)
      write (*, '(''dmn,dmx: '', 2f12.2)') dmn, dmx
c     dmn = -30.
c     dmx =  30.
      dmn = -75.
      dmx =  75.
c*************************
      if (.not. fail) then
c
c        ..loop over selected plot levels
c
         do j = 1, nz
            z_lev = int (lvl(z_plot(j)) + .001)
c
c           ..set bottom mask
c
            do_btm = .true.
            do i = 1, n_nodes
               if (depth(i) .lt. (lvl(z_plot(j))+1.01)) then
                  btm_msk(i) = -1.
               else
                  btm_msk(i) = 1.
               endif
            enddo
c
c           ..set plot title
c
            write (title, '(''Layer Pressure Increment (m)'',
     *                      4x, ''Layer '', i4)') z_plot(j)
c
c           ..extract and plot field
c
            do i = 1, n_nodes
               ndx = i + (z_plot(j)-1) * m * n
               field(i) = wrk(ndx)
            enddo
            call smth_2d (n_pass, field, m, n, spval)
            if (igrid .lt. 0) then
               call glb_merc (field, depth, dmn, dmx, m, n, gln,
     *                        glt, bl, br, tl, tr, i1, i2, j1, j2, 
     *                        node_eq, pos1, pos2, pos3, pos4,
     *                        nslc, iamap, MX_AMAP, cntr_int, 
     *                        .false., lntr, z_lev, spval)
            else
               call map_bkg (igrid, rlat, stdlt1, stdlt2, stdlon,
     *                       bl, br, tl, tr, pos1, pos2, pos3,
     *                       pos4, iamap, MX_AMAP)
               call contour_fld (field, dmn, dmx, m, n, i1, i2, j1,
     *                           j2, nslc, iamap, MX_AMAP, btm_msk,
     *                           cntr_int, do_btm, .false., .false.,
     *                           .false., .true., lntr, spval)
            endif
            call title_plot (title, .018, 1, 2.1)
            fno = fno + 1
            write (*, '(10x, ''frame'', i5, '': '', a)')
     *             fno, trim (title)
            write (title, '(a, 2x, ''Tau '', i3.3)') date, tau
            call title_plot (title, .018, 1, 1.)
            call frame
         enddo
      endif
c
c     ..clean up
c
      deallocate (btm_msk, field, iamap, wrk)
c
      return
      end
