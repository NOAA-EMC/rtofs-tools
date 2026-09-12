      subroutine vrfy_ssh_rgn (dtg, title1, title3, n_lon, n_lat, 
     *                         msk, n_obs, n_data, ob_anm, ob_fcst, 
     *                         ob_sla, ob_xi, ob_yj, bl, br, tl, tr,
     *                         i1, i2, j1, j2, gln, glt, node_eq, 
     *                         spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  vrfy_ssh_rgn
c
c DESCRIPTION:  verifies SSH forecast against SLA observations
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
      parameter (MX_AMAP = 2 400 000)
c
      integer    MX_AI
      parameter (MX_AI = 16 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 160 000)
c
      integer    N_WT
      parameter (N_WT = 2)
c     parameter (N_WT = 4)
c
c     ..local array dimensions
c
      integer   n_lon
      integer   n_lat
      integer   n_obs
c
      integer   gln, glt
c
      real      bl(2), br(2)
      real      cntr_int
      real      dat
      real      dmn, dmx
      logical   do_btm
      logical   do_cntr
      logical   do_lbl
      character dtg * 10
      integer   fno
      integer   i, j, k
      integer   ii, jj
      integer   i1, i2, j1, j2
      integer   ix, jy
      integer   lntr
      integer   msk (n_lon, n_lat)
      integer   n_data
      integer   n_pass
      real      node_eq ((gln * glt), 2)
      real      pos1, pos2, pos3, pos4
      character sfx * 30
      real      size
      real      spmis
      real      spval
      character title1 * 80
      character title2 * 256
      character title3 * 256
      real      tl(2), tr(2)
      character tmp_name * 80
      real      xvpl, xvpr, yvpb, yvpt
      real      wt (-N_WT:N_WT, -N_WT:N_WT)
      integer   z_lev
c
c     ..obs arrays
c
      real      ob_anm (n_obs)
      real      ob_fcst (n_obs)
      real      ob_sla (n_obs)
      real      ob_xi (n_obs)
      real      ob_yj (n_obs)
c
c     ..allocatable arrays
c
      real,     allocatable :: btm (:)
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: wgt (:,:)
      real,     allocatable :: wrk (:,:)
c
c     ..allocatable ncar arrays
c
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: ycs (:)
c
      include 'color_table.h'
c
c     ..define map position in plot frame
c
      data  pos1 / 0.05 /, pos2 / 0.95 /,
     *      pos3 / 0.10 /, pos4 / 0.90 /
c
c     ..define filter weights
c
      data wt / 0.5, 1.0, 1.0, 1.0, 0.5,
     *          1.0, 2.0, 2.0, 2.0, 1.0,
     *          1.0, 2.0, 4.0, 2.0, 1.0,
     *          1.0, 2.0, 2.0, 2.0, 1.0,
     *          0.5, 1.0, 1.0, 1.0, 0.5 /
c     data wt / 0.0, 1.0, 2.0, 3.0, 4.0, 3.0, 2.0, 1.0, 0.0,
c    *          1.0, 2.0, 3.0, 4.0, 5.0, 4.0, 3.0, 2.0, 1.0,
c    *          2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 
c    *          3.0, 4.0, 5.0, 6.0, 7.0, 6.0, 5.0, 4.0, 3.0,
c    *          4.0, 5.0, 6.0, 7.0, 8.0, 7.0, 6.0, 5.0, 4.0, 
c    *          3.0, 4.0, 5.0, 6.0, 7.0, 6.0, 5.0, 4.0, 3.0, 
c    *          2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 
c    *          1.0, 2.0, 3.0, 4.0, 5.0, 4.0, 3.0, 2.0, 1.0, 
c    *          0.0, 1.0, 2.0, 3.0, 4.0, 3.0, 2.0, 1.0, 0.0 /
c
c...............................executable..............................
c
c     ..set gmeta file name, open gks
c
      write (tmp_name, '(''ssh_fcst.'', a, ''.gmeta '')') dtg
      call init_gks ('opn', tmp_name)
c
c     ..allocate arrays
c
      allocate (btm (n_lon * n_lat))
      allocate (btm_msk (n_lon * n_lat))
      allocate (wgt (n_lon, n_lat))
      allocate (wrk (n_lon, n_lat))
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
      dmn = -0.6
      dmx =  0.6
      do_cntr = .false.
      do_lbl = .true.
      lntr = 5
      n_pass = 2
      fno = 0
      spmis = spval + 9.
      z_lev = 0
c
c     ..set dummy land and bottom masks
c
      do_btm = .false.
      do i = 1, (n_lon * n_lat)
         btm(i) = 0.
         btm_msk(i) = 1.
      enddo
c
c     ..set color table
c
      call gks_color (rgb_anm_clr, MX_ANM_CLR)
c
c-----------------------------------------------------------------
c
c     ..loop over plot types (fcst, sla, anom)
c
      do k = 1, 3
c
      wgt = 0.
      wrk = 0.
      do i = 1, n_data
c
c        ..set grid node reference
c
         ix = nint (ob_xi(i))
         jy = nint (ob_yj(i))
         if (k .eq. 1) then
            dat = ob_fcst(i)
            sfx = 'HYCOM SLA' 
         else if (k .eq. 2) then
            dat = ob_sla(i)
            sfx = 'Altimeter SLA' 
         else if (k .eq. 3) then
            dat = ob_anm(i)
            sfx = 'SLA Forecast Error' 
         endif
         if (dat .lt. spmis) cycle
c
c        ..extrapolate
c
         do jj = max (-N_WT, (N_WT-jy)), min (N_WT, (n_lat-jy))
         do ii = max (-N_WT, (N_WT-ix)), min (N_WT, (n_lon-ix))
            wrk(ix+ii,jy+jj) = wrk(ix+ii,jy+jj) + wt(ii,jj) * dat
            wgt(ix+ii,jy+jj) = wgt(ix+ii,jy+jj) + wt(ii,jj)
         enddo
         enddo
      enddo
c
c     ..update array
c
      do j = 1, n_lat
         do i = 1, n_lon
            if (msk(i,j) .gt. 0) then
               if (wgt(i,j) .gt. 0.) then
                  wrk(i,j) = wrk(i,j) / wgt(i,j)
               else
                  wrk(i,j) = 0.
               endif
            else
               wrk(i,j) = 0.
            endif
         enddo
      enddo
c
c     ..smooth field
c
      call smth_2d (n_pass, wrk, n_lon, n_lat, spval)
c
c     ..plot field
c
      call glb_merc (wrk, btm, dmn, dmx, n_lon, n_lat, gln, glt,
     *               bl, br, tl, tr, i1, i2, j1, j2, node_eq,
     *               pos1, pos2, pos3, pos4, n_slca, iamap,
     *               MX_AMAP, cntr_int, do_cntr, lntr, z_lev,
     *               spval)
c
c     ..put titles on plot
c
      title2 = trim (title1) // '     ' // trim (sfx)
      call title_plot (title2, .018, 1, 2.5)
      call title_plot (title3, .018, 1, 1.)
c
c     ..generate plot, return for new plot
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)')
     *       fno, trim (title2)
      call frame
      enddo
c
c     ..close gks and clean up
c
      call init_gks ('cls', tmp_name)
      deallocate (iai, iag, iamap, xcs, ycs)
      deallocate (btm, btm_msk, wgt, wrk)
c
      return
      end
