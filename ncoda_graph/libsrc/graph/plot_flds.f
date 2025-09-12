      subroutine plot_flds (dir_path, clm_path, date, date_dtg, dtg,
     *                      file_dtg, tau, upd, m, n, l, lvls, delx,
     *                      dely, igrid, rlat, stdlt1, stdlt2, stdlon,
     *                      bl, br, tl, tr, i1, i2, j1, j2, gln, glt,
     *                      pln, plt, global, subset, ssh_opt, spval)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_flds
c
c DESCRIPTION:  maps NCODA analysis fields and plots diagnostics
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
c     ..local array dimensions
c
      integer   m, n, l
      integer   gln, glt
      integer   pln, plt
c
      real      bl(2), br(2)
      character clm_path * (*)
      character date * 15
      character date_dtg * 15
      real      delx, dely
      character dir_path * (*)
      character dtg * 10
      character file_dtg * 10
      integer   fno
      logical   global
      integer   i1, i2, j1, j2
      integer   igrid
      real      lvls (100)
      real      rlat
      real      spval
      integer   ssh_opt
      real      stdlon
      real      stdlt1, stdlt2
      logical   subset
      integer   tau
      real      tl(2), tr(2)
      integer   upd
c
c     ..allocatable arrays
c
      real,     allocatable :: depth (:)
      integer,  allocatable :: msk (:)
      real,     allocatable :: node_eq (:,:)
      real,     allocatable :: node_nh (:,:)
      real,     allocatable :: node_sh (:,:)
c
c...............................executable..............................
c
      include 'omapnl.h'
c
c     ..read ocean map namelist
c
      open (9, file='omapnl', status='old', form='formatted')
      read (9, omapnl)
      close (9)
c
c     ..allocate mask/depth arrays
c
      allocate (depth (m * n))
      allocate (msk (m * n))
c
c     ..allocate interpolation arrays
c
      allocate (node_eq ((gln * glt), 2))
      allocate (node_nh ((pln * plt), 2))
      allocate (node_sh ((pln * plt), 2))
c
c     ..initialize frame counter
c
      fno = 0
c
c     ..turn off clipping
c
      call gsclip (0)
c
c     ..retrieve bathymetry and mask fields
c
      call rd_dpth_msk (dir_path, dtg, m, n, nest, depth, msk)
c
c     ..set up irregular grid interpolation indices
c
      if (global .and. igrid .lt. 0) then
         call glb_map (dir_path, clm_path, dtg, m, n, gln, glt,
     *                 node_eq, pln, plt, node_nh, node_sh)
      else
         node_eq = 0.
         node_nh = 0.
         node_sh = 0.
      endif
c
c     ..data locatons
c
      if (do_data .or. do_data_raw) then
      call plot_data (dir_path, date, dtg, nest, m, n, igrid, 
     *                rlat, stdlt1, stdlt2, stdlon, bl, br, 
     *                tl, tr, do_data, do_data_raw, global,
     *                zoom, fno)
      endif
c
c     ..surface fields
c
      if (do_ice .or. do_ice_inc .or. do_ice_err) then
      call plot_ice (dir_path, date, file_dtg, tau, nest, m, n,
     *               depth, msk, igrid, rlat, stdlt1, stdlt2,
     *               stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *               do_ice, do_ice_err, do_ice_inc, pln, plt,
     *               node_nh, node_sh, fno, spval)
      endif
      if (do_sst .or. do_sst_inc .or. do_sst_clm .or. do_sst_err) then
      call plot_sst (dir_path, date, dtg, tau, nest, m, n, depth,
     *               msk, igrid, rlat, stdlt1, stdlt2, stdlon,
     *               bl, br, tl, tr, i1, i2, j1, j2, sst_cnt,
     *               sst_del, sst_min, do_sst, do_sst_err,
     *               do_sst_inc, do_sst_clm, gln, glt, node_eq,
     *               fno, spval)
      endif
      if (do_ssh .or. do_ssh_inc .or. do_ssh_err .or.
     *    do_ssh_clm_anm) then
      call plot_ssh (dir_path, clm_path, date, dtg, tau, nest,
     *               m, n, depth, msk, igrid, rlat, stdlt1, 
     *               stdlt2, stdlon, bl, br, tl, tr, i1, i2,
     *               j1, j2, ssh_min, ssh_max, do_ssh, do_ssh_clm, 
     *               do_ssh_clm_anm, do_ssh_err, do_ssh_inc, 
     *               ssh_opt, gln, glt, node_eq, fno, spval)
      endif
      if (do_sss .or. do_sss_inc .or. do_sss_err) then
      call plot_sss (dir_path, date, dtg, tau, nest, m, n,
     *               depth, msk, igrid, rlat, stdlt1, stdlt2,
     *               stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *               sss_cnt, sss_del, sss_min, do_sss,
     *               do_sss_err, do_sss_inc, gln, glt,
     *               node_eq, fno, spval)
      endif
      if (do_sfc_vel) then
      call plot_uv_sfc (dir_path, date, dtg, tau, nest, m, n,
     *                  depth, msk, igrid, rlat, stdlt1, stdlt2,
     *                  stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *                  vel_max, vel_thn, do_sfc_vel, do_vel_err,
     *                  do_vel_inc, gln, glt, node_eq, fno, spval)
      endif
c
      if (l .gt. 1) then
c
c        ..subsurface fields
c
         if (do_tmp .or. do_tmp_inc .or. do_tmp_err .or.
     *       do_clm_err) then
         call plot_tmp (dir_path, date, file_dtg, tau, nest, m, n, l,
     *                  depth, lvls, msk, igrid, rlat, stdlt1, stdlt2,
     *                  stdlon, bl, br, tl, tr, i1, i2, j1, j2, n_plot,
     *                  z_plot, tmp_cnt, tmp_del, tmp_min, do_tmp,
     *                  do_tmp_err, do_tmp_inc, do_clm, do_clm_err,
     *                  gln, glt, node_eq, plot_lyr, fno, spval)
         endif
         if (do_sal .or. do_sal_inc .or. do_sal_err) then
         call plot_sal (dir_path, date, file_dtg, tau, nest, m, n,
     *                  l, depth, lvls, msk, igrid, rlat, stdlt1,
     *                  stdlt2, stdlon, bl, br, tl, tr, i1, i2,
     *                  j1, j2, n_plot, z_plot, sal_cnt, sal_del,
     *                  sal_min, do_sal, do_sal_err, do_sal_inc,
     *                  do_clm, gln, glt, node_eq, plot_lyr, fno,
     *                  spval)
         endif
         if (do_gpt .or. do_gpt_inc) then
         call plot_gpt (dir_path, date, dtg, tau, nest, m, n, l,
     *                  depth, lvls, msk, igrid, rlat, stdlt1,
     *                  stdlt2, stdlon, bl, br, tl, tr, i1, i2,
     *                  j1, j2, n_plot, z_plot, do_gpt, do_gpt_inc, 
     *                  gln, glt, node_eq, fno, spval)
         endif
         if (do_mdl_bias) then
         call plot_mdl_bias (dir_path, date, dtg, tau, nest, m, n,
     *                       l, depth, lvls, msk, igrid, rlat,
     *                       stdlt1, stdlt2, stdlon, bl, br, tl,
     *                       tr, i1, i2, j1, j2, n_plot, z_plot,
     *                       gln, glt, node_eq, fno, spval)
         endif
         if (do_vel .or. do_vel_err) then
         call plot_uv (dir_path, date, file_dtg, tau, nest, m, n,
     *                 l, depth, lvls, msk, igrid, rlat, stdlt1,
     *                 stdlt2, stdlon, bl, br, tl, tr, i1, i2,
     *                 j1, j2, n_plot, z_plot, vel_max, vel_thn,
     *                 do_vel, do_vel_err, gln, glt, node_eq,
     *                 plot_lyr, fno, spval)
         endif
         if (do_vel_inc) then
         call plot_uv_inc (dir_path, date, file_dtg, nest, m, n, l,
     *                     depth, lvls, msk, igrid, rlat, stdlt1,
     *                     stdlt2, stdlon, bl, br, tl, tr, i1, i2,
     *                     j1, j2, n_plot, z_plot, gln, glt, node_eq,
     *                     plot_lyr, fno, spval)
         endif
         if (do_lyp_inc .and. tau .eq. 0) then
         call plot_lyp (dir_path, date, dtg, tau, nest, m, n,
     *                  l, depth, lvls, igrid, rlat, stdlt1,
     *                  stdlt2, stdlon, bl, br, tl, tr, i1, 
     *                  i2, j1, j2, n_plot, z_plot, gln, glt, 
     *                  node_eq, fno, spval)
         endif
         if (do_fcst_diff .and. tau .gt. 0) then
         call plot_diff (dir_path, dtg, nest, m, n, l, depth,
     *                   lvls, msk, bl, br, tl, tr, i1, i2,
     *                   j1, j2, n_plot, z_plot, gln, glt,
     *                   node_eq, tau, fno, spval)
         endif
         if (do_ensm) then
         call plot_ensm (dir_path, date, dtg, nest, m, n, l,
     *                   depth, lvls, igrid, rlat, stdlt1, 
     *                   stdlt2, stdlon, bl, br, tl, tr, i1,
     *                   i2, j1, j2, n_plot, z_plot, gln, glt,
     *                   node_eq, fno, spval)
         endif
         if (do_ensm_std) then
         call plot_ensm_std (dir_path, date, dtg, nest, m, n, l,
     *                       depth, lvls, igrid, rlat, stdlt1, 
     *                       stdlt2, stdlon, bl, br, tl, tr, i1,
     *                       i2, j1, j2, n_plot, z_plot, gln,
     *                       glt, node_eq, fno, spval)
         endif
         if (do_arctic) then
         call plot_arctic (dir_path, date, file_dtg, tau, nest, 
     *                     m, n, l, msk, igrid, pln, plt, node_nh,
     *                     fno, spval)
         endif
         if (do_antarctic) then
         call plot_antarctic (dir_path, date, file_dtg, tau, nest,
     *                        m, n, l, msk, igrid, pln, plt, node_sh,
     *                        fno, spval)
         endif
         if (do_arch .and. tau .eq. 0) then
         call plot_arch (dir_path, date, dtg, tau, nest, m, n, l,
     *                   depth, lvls, msk, igrid, rlat, stdlt1,
     *                   stdlt2, stdlon, bl, br, tl, tr, i1, i2,
     *                   j1, j2, n_plot, z_plot, gln, glt, node_eq,
     *                   fno, spval)
         endif
      endif
c
c     ..diagnostics
c
      if (do_diurn) then
      call plot_diurnal (dir_path, date, dtg, nest, m, n, msk,
     *                   igrid, rlat, stdlt1, stdlt2, stdlon,
     *                   bl, br, tl, tr, i1, i2, j1, j2, gln,
     *                   glt, node_eq, fno, spval)
      endif
      if (do_sens) then
      call plot_sens (date, dtg, adj_tau, lvls, l, n_plot, z_plot,
     *                fno, spval)
      endif
      if (do_flow .and. tau .eq. 0) then
      call plot_flow (dir_path, dtg, date, m, n, nest, igrid, rlat,
     *                stdlt1, stdlt2, stdlon, bl, br, tl, tr, i1,
     *                i2, j1, j2, gln, glt, node_eq, fno, spval)
      endif
      if (do_grdnt) then
      call plot_grdnt (dir_path, date, dtg, tau, nest, m, n, l,
     *                 depth, lvls, msk, igrid, delx, dely, rlat,
     *                 stdlt1, stdlt2, stdlon, bl, br, tl, tr, i1,
     *                 i2, j1, j2, n_plot, z_plot, do_sal, do_sst,
     *                 do_tmp, sal_grd, tmp_grd, gln, glt, node_eq, 
     *                 fno, spval)
      endif
      if (do_mld) then
      call plot_mld (dir_path, file_dtg, date, upd, m, n, depth,
     *               nest, igrid, rlat, stdlt1, stdlt2, stdlon,
     *               bl, br, tl, tr, i1, i2, j1, j2, gln, glt,
     *               node_eq, fno, spval)
      endif
      if (do_scl) then
      call plot_scl (dir_path, date, dtg, tau, nest, m, n, l,
     *               lvls, msk, igrid, rlat, stdlt1, stdlt2,
     *               stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *               gln, glt, node_eq, fno, spval) 
      endif
      if (do_dist .and. tau .eq. 0) then
      call plot_dist (dir_path, dtg, date, m, n, nest, igrid,
     *                rlat, stdlt1, stdlt2, stdlon, bl, br, tl,
     *                tr, i1, i2, j1, j2, gln, glt, node_eq,
     *                fno, spval)
      endif
      if (do_ohc .and. tau .eq. 0) then
      call plot_ohc (dir_path, date, dtg, nest, m, n, igrid,
     *               rlat, stdlt1, stdlt2, stdlon, bl, br, tl,
     *               tr, i1, i2, j1, j2, gln, glt, node_eq,
     *               fno, spval)
      endif
      if (do_btm .and. tau .eq. 0) then
      call plot_btm (dir_path, dtg, date, m, n, nest, depth, msk,
     *               tau, igrid, rlat, stdlt1, stdlt2, stdlon,
     *               bl, br, tl, tr, i1, i2, j1, j2, btm_max,
     *               gln, glt, node_eq, fno, spval)
      endif
      if (do_prs) then
      call plot_botprs (dir_path, date, dtg, tau, nest, m, n,
     *                  depth, msk, igrid, rlat, stdlt1, stdlt2,
     *                  stdlon, bl, br, tl, tr, i1, i2, j1, j2,
     *                  gln, glt, node_eq, fno, spval)
      endif
      if (do_sla) then
      call plot_slatrk (dir_path, dtg, date_dtg, nest, m, n,
     *                  subset, bl, tr, fno)
      endif
      if (do_ch) then
      call plot_chobs (dir_path, dtg, date_dtg, nest, m, n, subset,
     *                 bl, tr, ssh_del, fno)
      endif
c
c     ..verification
c
      if (do_stats .and. tau .eq. 0) then
      call plot_stats (dir_path, dtg, dtg1, nest, m, n, fno)
      endif
      if (do_stats_lvl .and. tau .eq. 0) then
      call plot_stats_lvl (dir_path, dtg, dtg1, nest, m, n, fno)
      endif
      if (do_stats_fcst .and. tau .eq. 0) then
      call plot_stats_fcst (dir_path, dtg, dtg1, nest, m, n, fno)
      endif
      if (do_fcst_err .and. tau .eq. 0) then
      call plot_err (dir_path, dtg, nest, m, n, fno)
      endif
      if (do_jmin .and. tau .eq. 0) then
      call plot_jmin (dir_path, dtg, dtg1, nest, m, n, fno)
      endif
      if (do_vfy .and. tau .eq. 0) then
      call plot_vrfy (dir_path, date, dtg, nest, m, n, fno)
      endif
      if (do_prf_vfy .and. tau .eq. 0) then
      call plot_prof_vrfy (dir_path, dtg, nest, m, n, l, zoom, fno)
      endif
      if (do_prf_fcst .and. tau .eq. 0) then
      call plot_prof_fcst (dir_path, dtg, nest, m, n, l, zoom, fno)
      endif
c
c     ..analysis monitoring
c
      if (do_vol .and. tau .eq. 0) then
         if (l .eq. 1) then
            call plot_vol (dir_path, date, dtg, nest, m, n, 
     *                     msk, fno)
         else
            call plot_vol (dir_path, date, dtg, nest, m, n,
     *                     msk, fno)
         endif
      endif
c
c     ..clean up
c
      deallocate (depth, msk, node_eq, node_nh, node_sh)
c
      return
      end
