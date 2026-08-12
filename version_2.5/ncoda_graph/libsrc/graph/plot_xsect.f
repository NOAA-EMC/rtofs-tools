      subroutine plot_xsect (dir_path, date, date_bk, dtg, tau,
     *                       plt_typ, mrun, m, n, l, igrid,
     *                       nest, delx, dely, iref, jref, reflat,
     *                       reflon, stdlt1, stdlt2, stdlon,
     *                       n_xsect, slat, slon, elat, elon,
     *                       edpth, lyr, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_xsect
c
c DESCRIPTION:  plots CODA cross sections of full valed fields, analysis
c               increments or forecast errors
c
c PARAMETERS:
c       Name          Type       Usage            Description
c   -------------   ----------   -----   -----------------------------
c   date            char         input   dtg plot label
c
c..............................END PROLOGUE.............................
c
      implicit  none
c
c     ..set NCAR map array dimensions
c
      integer    MX_AMAP
      parameter (MX_AMAP = 36 000 000)
c
      integer    MX_AI
      parameter (MX_AI = 2 000)
c
      integer    MX_LIN
      parameter (MX_LIN = 20 000)
c
c     ..set dimensions of cross section 
c
      integer    MX_HORIZ
      parameter (MX_HORIZ = 600)
c
      integer    MX_VERT
      parameter (MX_VERT = 200)
c
      integer    MX_XSECT
      parameter (MX_XSECT = 20)
c
c     ..local array dimensions
c
      integer   m, n, l
c
      real      cntr_int
      character date * 15
      character date_bk * 15
      real      del, delta
      real      delx, dely, delz
      character dir_path * (*)
      real      dmn, dmx
      logical   do_anm
      logical   do_lbl
      logical   do_mask
      character dtg * 10
      integer   edpth (20)
      real      elat (20), elon (20)
      logical   fail
      character file_dtg * 10
      character file_typ * 7
      integer   i, j, k, k1, k2
      integer   igrid
      integer   iref, jref
      integer   ix, jy
      integer   kx, kv
      integer   len
      integer   lntr
      logical   lyr
      real      map_lon
      real      mbl (2, 20), mbr (2, 20)
      character mrun * (*)
      real      mtl (2, 20), mtr (2, 20)
      integer   ndx
      integer   nv
      integer   nest
      integer   nodes
      integer   n_clrs
      integer   n_pass
      integer   n_proj
      integer   n_strt
      integer   n_xsect
      character plt_typ * 5
      real      reflat, reflon
      integer   row_swap
      real      siz
      real      stdlon
      real      stdlt1, stdlt2
      real      slat (20), slon (20)
      real      spval
      integer   status
      integer   tau
      character title1 * 132
      character title2 * 132
      real      x
      real      xvpl, xvpr, yvpb, yvpt
      real      zlvl (100)
c
c     ..allocatable arrays
c
      real,     allocatable :: btm_msk (:)
      real,     allocatable :: depth (:)
      logical,  allocatable :: ftrp2_fail (:)
      logical,  allocatable :: ftrp3_fail (:)
      real,     allocatable :: grd (:)
      integer,  allocatable :: iai (:)
      integer,  allocatable :: iag (:)
      integer,  allocatable :: iamap (:)
      real,     allocatable :: lat (:)
      real,     allocatable :: lon (:)
      real,     allocatable :: lvl (:,:,:)
      integer,  allocatable :: mask (:)
      real,     allocatable :: obs_xi (:)
      real,     allocatable :: obs_yj (:)
      real,     allocatable :: obs_zk (:)
      real,     allocatable :: pdepth (:,:)
      real,     allocatable :: pdist (:)
      real,     allocatable :: plat (:,:)
      real,     allocatable :: plon (:,:)
      real,     allocatable :: pxi (:,:)
      real,     allocatable :: pyj (:,:)
      real,     allocatable :: pzk (:,:,:)
      real,     allocatable :: xcs (:)
      real,     allocatable :: xsect_data (:)
      real,     allocatable :: xsect_mask (:)
      real,     allocatable :: ycs (:)
c
c     ..external NCAR functions
c
      external  clin, cmap
c
c...............................executable..............................
c
c     ..initialize
c
      if (tau .eq. 0) then
         if (plt_typ .eq. 'incr ') then
            file_typ = 'analinc'
         else if (plt_typ .eq. 'error') then
            file_typ = 'fcsterr'
         else
            file_typ = 'analfld'
         endif
         file_dtg = dtg
      else
         file_typ = 'fcstfld'
         call dtgmod (dtg, -tau, file_dtg, status)
      endif
c
      delta = 20.
      n_pass = 2
c
c     ..set number plot variables
c
      if (plt_typ .eq. 'vcorr') then
         nv = 1
      else
         nv = 5
      endif
c
c     ..allocate arrays
c
      allocate (btm_msk (MX_HORIZ))
      allocate (depth (m * n))
      allocate (ftrp2_fail (MX_HORIZ))
      allocate (ftrp3_fail (MX_HORIZ * MX_VERT))
      allocate (grd (m * n * l))
      allocate (iai (MX_AI))
      allocate (iag (MX_AI))
      allocate (iamap (MX_AMAP))
      allocate (lat (m * n))
      allocate (lon (m * n))
      allocate (lvl (m, n, l))
      allocate (mask (m * n))
      allocate (obs_xi (MX_HORIZ * MX_VERT))
      allocate (obs_yj (MX_HORIZ * MX_VERT))
      allocate (obs_zk (MX_HORIZ * MX_VERT))
      allocate (pdepth (MX_VERT, MX_XSECT))
      allocate (pdist (MX_XSECT))
      allocate (plat (MX_HORIZ, MX_XSECT))
      allocate (plon (MX_HORIZ, MX_XSECT))
      allocate (pxi (MX_HORIZ, MX_XSECT))
      allocate (pyj (MX_HORIZ, MX_XSECT))
      allocate (pzk (MX_HORIZ, MX_VERT, MX_XSECT))
      allocate (xcs (MX_LIN))
      allocate (xsect_data (MX_HORIZ * MX_VERT))
      allocate (xsect_mask (MX_HORIZ * MX_VERT))
      allocate (ycs (MX_LIN))
c
c     ..retrieve bathymetry and mask arrays
c
      call rd_dpth_msk (dir_path, dtg, m, n, nest, depth, mask)
c
c     ..retrieve lat, lon, lvl arrays
c
      call rd_ll_lvl (dir_path, dtg, lyr, m, n, l, nest,
     *                lat, lon, lvl)
c
c     ..set cross section interpolation points
c
      do kx = 1, n_xsect
c
c        ..set map boundaries
c
         if (slat(kx). le. elat(kx)) then
            if (slon(kx) .le. elon(kx)) then
               mbl(1,kx) = slat(kx) - delta * 0.5
               mbl(2,kx) = slon(kx) - delta
               mbr(1,kx) = slat(kx) - delta * 0.5
               mbr(2,kx) = elon(kx) + delta
               mtl(1,kx) = elat(kx) + delta * 0.5
               mtl(2,kx) = slon(kx) - delta
               mtr(1,kx) = elat(kx) + delta * 0.5
               mtr(2,kx) = elon(kx) + delta
            else if (slon(kx) .gt. elon(kx)) then
               mbl(1,kx) = slat(kx) - delta * 0.5
               mbl(2,kx) = elon(kx) - delta
               mbr(1,kx) = slat(kx) - delta * 0.5
               mbr(2,kx) = slon(kx) + delta
               mtl(1,kx) = elat(kx) + delta * 0.5
               mtl(2,kx) = elon(kx) - delta
               mtr(1,kx) = elat(kx) + delta * 0.5
               mtr(2,kx) = slon(kx) + delta
            endif
         else if (slat(kx) .gt. elat(kx)) then
            if (slon(kx) .le. elon(kx)) then
               mbl(1,kx) = elat(kx) - delta * 0.5
               mbl(2,kx) = slon(kx) - delta
               mbr(1,kx) = elat(kx) - delta * 0.5
               mbr(2,kx) = elon(kx) + delta
               mtl(1,kx) = slat(kx) + delta * 0.5 
               mtl(2,kx) = slon(kx) - delta
               mtr(1,kx) = slat(kx) + delta * 0.5
               mtr(2,kx) = elon(kx) + delta
            else if (slon(kx) .gt. elon(kx)) then
               mbl(1,kx) = elat(kx) - delta * 0.5
               mbl(2,kx) = elon(kx) - delta
               mbr(1,kx) = elat(kx) - delta * 0.5
               mbr(2,kx) = slon(kx) + delta
               mtl(1,kx) = slat(kx) + delta * 0.5
               mtl(2,kx) = elon(kx) - delta
               mtr(1,kx) = slat(kx) + delta * 0.5
               mtr(2,kx) = slon(kx) + delta
            endif
         endif
         mbl(1,kx) = max (mbl(1,kx), -90.)
         mbr(1,kx) = max (mbr(1,kx), -90.)
         mtl(1,kx) = min (mtl(1,kx),  90.)
         mtr(1,kx) = min (mtr(1,kx),  90.)
c
c        ..form depth interpolation array
c
         delz = real (edpth(kx)) / real (MX_VERT-1)
         do j = 1, MX_VERT
            pdepth(j,kx) = real (j-1) * delz
         enddo
c
c        ..compute great circle path positions
c
         call great_circle (slat(kx), slon(kx), elat(kx), elon(kx),
     *                      MX_HORIZ, plat(1,kx), plon(1,kx),
     *                      pdist(kx))
c
c        ..compute interpolation locations (i,j,k)
c
c        do i = 1, MX_HORIZ
c           if (plon(i,kx) .lt. 0.) plon(i,kx) = plon(i,kx) + 360.
c        enddo
         if (igrid .gt. 0) then
            call ll2ij (igrid, reflat, reflon, iref, jref, stdlt1, 
     *                  stdlt2, stdlon, delx, dely, plat(1,kx),
     *                  plon(1,kx), MX_HORIZ, pxi(1,kx), pyj(1,kx))
         else if (igrid .lt. 0) then
            call irreg_ll2ij (m, n, lat, lon, MX_HORIZ, plat(1,kx), 
     *                        plon(1,kx), pxi(1,kx), pyj(1,kx))
         endif
c        do i = 1, MX_HORIZ
c           if (plon(i,kx) .gt. 180.) plon(i,kx) = plon(i,kx) - 360.
c        enddo
         do i = 1, MX_HORIZ
            ix = nint (pxi(i,kx))
            jy = nint (pyj(i,kx))
            do k = 1, l
               zlvl(k) = lvl (ix,jy,k)
            enddo
            do j = 1, MX_VERT
               do k = 2, l
                  if (pdepth(j,kx) .ge. zlvl(k-1) .and.
     *                pdepth(j,kx) .le. zlvl(k)) then
                     pzk(i,j,kx) = real (k-1) + 
     *                            (pdepth(j,kx) - zlvl(k-1)) /
     *                                 (zlvl(k) - zlvl(k-1))
                  endif
               enddo
            enddo
         enddo
      enddo
c
c     ..loop over analysis variables
c
      do kv = 1, nv
c
c        ..retrieve analysis grid
c
         call xsect_fld (dir_path, dtg, file_dtg, file_typ, plt_typ,
     *                   nest, tau, m, n, l, kv, mask, grd, cntr_int,
     *                   dmn, dmx, do_anm, do_lbl, lntr, n_clrs, 
     *                   title1, lyr, fail, spval)
         if (.not. fail) then
c
c           ..loop over cross sections
c
            do kx = 1, n_xsect 
c
c              ..build interpolation list
c
               nodes = MX_HORIZ * MX_VERT
               do j = 1, MX_VERT
                  do i = 1, MX_HORIZ
                     ndx = (j-1) * MX_HORIZ + i
                     obs_xi(ndx) = pxi(i,kx)
                     obs_yj(ndx) = pyj(i,kx)
                     obs_zk(ndx) = pzk(i,j,kx)
                  enddo
               enddo
c
c              ..interpolate bottom depths to cross section
c
               n_strt = 1
               call fld2_trp (MX_HORIZ, n_strt, MX_HORIZ, pxi(1,kx),
     *                        pyj(1,kx), m, n, depth, spval, btm_msk,
     *                        ftrp2_fail)
c
c              ..build bottom depth mask overlay field
c
               do_mask = .false.
               do j = 1, MX_VERT
                  do i = 1, MX_HORIZ
                     ndx = (j-1) * MX_HORIZ + i
                     if (btm_msk(i) .lt. pdepth(j,kx)) then
                        xsect_mask(ndx) = -1.
                        do_mask = .true.
                     else
                        xsect_mask(ndx) = 1.
                     endif
                     xsect_data(ndx) = spval
                  enddo
               enddo
c
c              ..interpolate grid to cross section locations
c
               n_strt = 1
               call fld3_trp (nodes, n_strt, nodes, obs_xi, obs_yj,
     *                        obs_zk, m, n, l, grd, spval, xsect_data,
     *                        ftrp3_fail)
c
c              ..swap array rows for vertical sections
c 
               row_swap = MX_VERT / 2
               do j = 1, row_swap
                  do i = 1, MX_HORIZ 
                     ndx = MX_VERT - j + 1
                     k1 = (j-1)   * MX_HORIZ + i
                     k2 = (ndx-1) * MX_HORIZ + i
                     x = xsect_data(k1)
                     xsect_data(k1) = xsect_data(k2)
                     xsect_data(k2) = x
                     x = xsect_mask(k1)
                     xsect_mask(k1) = xsect_mask(k2)
                     xsect_mask(k2) = x
                  enddo
               enddo
c
c              ..smooth cross section
c
c              call smth_2d (n_pass, xsect_data, MX_HORIZ, MX_VERT,
c    *                       spval)
c
c              ..contour cross section
c
               xvpl = 0.11
               xvpr = 0.89
               yvpb = 0.07
               yvpt = 0.66
               call set (xvpl, xvpr, yvpb, yvpt,
     *                   xvpl, xvpr, yvpb, yvpt, 1)
               call setusv ('LW', 1000)
               call contour_xsect (xsect_data, xsect_mask, MX_HORIZ,
     *                             MX_VERT, n_clrs, cntr_int, do_mask,
     *                             dmn, dmx, lntr, do_lbl, do_anm,
     *                             spval)
c
c              ..draw line around section chart
c
               call gsplci (1)
               call line (xvpl, yvpb, xvpr, yvpb)
               call line (xvpl, yvpb, xvpl, yvpt)
               call line (xvpl, yvpt, xvpr, yvpt)
               call line (xvpr, yvpb, xvpr, yvpt)
c
c              ..label axes
c
               siz = .017
               call gsplci (1)
               if (pdepth(MX_VERT,kx) .le. 300.) then
                  del = 50.
               else if (pdepth(MX_VERT,kx) .le. 1000.) then
                  del = 100.
               else if (pdepth(MX_VERT,kx) .le. 2000.) then
                  del = 200.
               else
                  del = 500.
               endif
               call lbl_axis (pdepth(1,kx), pdepth(MX_VERT,kx), del,
     *                        del, 'Depth (M)', 90., siz, .09,
     *                        .true.)
               call lbl_xsect (slat(kx), slon(kx), elat(kx),
     *                         elon(kx), pdist(kx), siz)
c
c              ..plot titles
c
               call set (.42, .89, .70, .98, .42, .89, .70, .98, 1)
               call gsplci (1)
               siz = .018
               x = (.42 + .89) * 0.5
               len = len_trim (title1)
               call plchhq (x, .75, title1(1:len), siz, 0., 0.)
               len = len_trim (mrun)
               if (len .eq. 0) then
                  mrun = 'NCODA'
               endif
               if (plt_typ .eq. 'vcorr') then
                  write (title2, '(a, ''   Tau 000'')') trim (mrun)
               else
                  write (title2, '(a, ''   Tau '', i3.3)')
     *                   trim (mrun), tau
               endif
               len = len_trim (title2)
               call plchhq (x, .93, title2(1:len), siz, 0., 0.)
               if (plt_typ .eq. 'vcorr') then
                  write (title2, '(a)') date_bk
               else
                  write (title2, '(a)') date
               endif
               len = len_trim (title2)
               call plchhq (x, .84, title2(1:len), siz, 0., 0.)
c
c              ..plot map background and color fill land (if any)
c
               if (igrid .lt. 0) then
                  n_proj = 1
                  map_lon = ((mbl(2,kx) + mbr(2,kx)) * 0.5 + 
     *                       (mtl(2,kx) + mtr(2,kx)) * 0.5) * 0.5
               else
                  n_proj = igrid
                  map_lon = stdlon
               endif
               call map_bkg (n_proj, reflat, stdlt1, stdlt2, map_lon,
     *                       mbl(1,kx), mbr(1,kx), mtl(1,kx), 
     *                       mtr(1,kx), .02, .40, .75, .98,
     *                       iamap, MX_AMAP)
               call gsfais (1)
               call gsfaci (2)
               call arscam (iamap, xcs, ycs, MX_LIN, iai, iag,
     *                      MX_AI, cmap)
               call gsplci (1)
               call gstxci (1)
               call gsln (2)
               call mapgrm (iamap, xcs, ycs, MX_LIN, iai, iag,
     *                      MX_AI, clin)
               call gsln (1)
c
c              ..overlay great circle path
c
               call setusv ('LW', 3000)
               call gsplci (6)
               call mapfst (plat(1,kx), plon(1,kx))
               do i = 1, MX_HORIZ
                  call mapvec (plat(i,kx), plon(i,kx))
               enddo
               call mapit (plat(MX_HORIZ,kx), plon(MX_HORIZ,kx), 1)
c
c              ..advance the plot buffer
c
               call frame
            enddo
         endif
      enddo
c
c     ..clean up
c
      deallocate (btm_msk, depth, ftrp2_fail, ftrp3_fail, grd, iai)
      deallocate (iag, iamap, lat, lon, lvl, mask, obs_xi, obs_yj)
      deallocate (obs_zk, pdepth, pdist, plat, plon, pxi, pyj, pzk)
      deallocate (xcs, xsect_data, xsect_mask, ycs)
c
      return
      end
