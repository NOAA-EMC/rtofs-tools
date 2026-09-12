      subroutine map_adj_sens (domain, opt, m, n, l, fld, z_lvl,
     *                         title, n_proj, rlat, stdlt1, stdlt2,
     *                         stdlon, bl, br, tl, tr, z_plot, fno,
     *                         spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  map_adj_sens
c
c DESCRIPTION:  maps time averaged adjoint sensitivty (or forecast error)
c               fields
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
c     ..local array dimensions
c
      integer   m, n, l
c
      real      bl(2), br(2)
      real      cntr_int
      real      dmn, dmx
      character domain * 3
      logical   do_cntr
      logical   do_lbl
      real      fld (m * n * l)
      integer   fno
      integer   i, j
      integer   i1, i2
      integer   j1, j2
      integer   lntr
      integer   n_pass
      integer   n_proj
      integer   ndx
      integer   nz
      integer   opt
      real      pos1, pos2, pos3, pos4
      real      rlat
      real      spval
      real      stdlon
      real      stdlt1, stdlt2
      character title * (*)
      character title_lbl * 256
      real      tl(2), tr(2)
      character var_name * 16
      integer   z_lev
      real      z_lvl (100)
      integer   z_plot (20)
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
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
c     ..count number levels to plot
c
      nz = 0
      do i = 1, 20
         if (z_plot(i) .gt. 0) nz = nz + 1
      enddo
      if (nz .eq. 0) return
c
c     ..allocate arrays
c
      allocate (btm_msk (m * n))
      allocate (iamap (MX_AMAP))
      allocate (wrk (m * n))
c
c     ..initialize
c
      do_cntr = .false.
      do_lbl = .true.
      i1 = 1
      i2 = m
      j1 = 1
      j2 = n
      n_pass = 2
      call gks_color (rgb_anm_clr, MX_ANM_CLR)
c
c     ..set range of plot variables
c
      if (domain .eq. 'PRF') then
         if (opt .eq. 1) then
            dmn = -0.3
            dmx =  0.3
            lntr = 5
            var_name = 'Temperature (C) '
         else if (opt .eq. 2) then
            dmn = -0.15
            dmx =  0.15
            lntr = 5
            var_name = 'Salinity (PSU)  ' 
         endif
      else if (domain .eq. 'ICE') then
         dmn = -9.
         dmx =  9. 
         lntr = 5
         var_name = 'Ice Coverage (%)'
      endif
c
c     ..set dummy land and bottom masks
c
      do i = 1, (m * n)
         btm_msk(i) = 1.
      enddo
c
c     ..loop over selected plot levels
c
      do j = 1, nz
         z_lev = int (z_lvl(z_plot(j)) + .001)
c
c        ..extract and smooth field
c
         do i = 1, (m * n)
            ndx = i + (z_plot(j)-1) * m * n
            wrk(i) = fld(ndx)
         enddo
         call smth_2d (n_pass, wrk, m, n, spval)
c
c        ..plot field
c
         call map_bkg (n_proj, rlat, stdlt1, stdlt2, stdlon,
     *                 bl, br, tl, tr, pos1, pos2, pos3,
     *                 pos4, iamap, MX_AMAP)
         call contour_fld (wrk, dmn, dmx, m, n, i1, i2, j1, j2,
     *                     n_slca, iamap, MX_AMAP, btm_msk,
     *                     cntr_int, .false., do_cntr, .true.,
     *                     do_lbl, .false., lntr, spval)
c
c        ..form plot titles
c
         call title_plot (title, .02, 1, 3.1)
         write (title_lbl, '(a, 3x, ''Forecast Error Sensitivity'',
     *          3x, i4, '' M'')') trim (var_name), z_lev
         call title_plot (title_lbl, .02, 1, 1.)
         fno = fno + 1
         write (*, '(10x, ''frame'', i5, '': '', a)')
     *          fno, trim (title_lbl)
         call frame
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, iamap, wrk)
c
      return
      end
