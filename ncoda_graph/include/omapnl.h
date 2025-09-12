c
c NCODA ocean map namelist
c
c  CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c PARAMETERS:
c       Name          Type                 Description
c   -------------   --------    ---------------------------------------
c   adj_tau         integer     adjoint forecast to plot
c   btm_max         real        maximum bottom depth to plot
c   do_arch         logical     (true) plot HYCOM initial conditions
c   do_arctic       logical     (true) plot arctic sfc fields
c   do_antarctic    logical     (true) plot antarctic sfc fields
c   do_btm          logical     (true) plot grid bathymetry
c   do_ch           logical     (true) plot cooper-haines profiles
c   do_clm          logical     (true) plot climate error
c   do_clm_err      logical     (true) plot integral of climate error
c   do_data         logical     (true) plot data distributions
c   do_data_raw     logical     (true) plot raw data distributions
c   do_fcst_diff    logical     (true) plot forecast differences
c   do_dist         logical     (true) plot land distance field
c   do_diurn        logical     (true) plot sst diurnal anomalies
c   do_ensm         logical     (true) plot ensemble increment fields
c   do_ensm_std     logical     (true) plot ensemble std dev fields
c   do_fcst_err     logical     (treu) plot forecast error time series
c   do_flow         logical     (true) plot covariance flow field
c   do_gpt          logical     (true) plot geopotential
c   do_gpt_inc      logical     (true) plot geopotential increments
c   do_grdnt        logical     (true) plot gradient fields
c   do_mdl_bias     logical     (treu) plot bias correction fields
c   do_ice          logical     (true) plot sea ice
c   do_ice_err      logical     (true) plot sea ice errors
c   do_ice_inc      logical     (true) plot sea ice increments
c   do_jmin         logical     (true) plot jmin time series
c   do_lyp_inc      logical     (true) plot layer pressure increments
c   do_mld          logical     (true) plot mixed layer depth
c   do_ohc          logical     (true) plot ocean heat content
c   do_prf_fcst     logical     (true) plot forecast profiles
c   do_prf_vfy      logical     (true) plot vertification profiles
c   do_prs          logical     (true) plot bottom pressure
c   do_sal          logical     (true) plot salinity
c   do_sal_err      logical     (true) plot salinity errors
c   do_sal_inc      logical     (true) plot salinity increments
c   do_scl          logical     (true) plot length scales
c   do_sens         logical     (true) plot obs sensitivities
c   do_sfc_vel      logical     (true) plot surface velocity
c   do_sla          logical     (true) plot along track sla
c   do_ssh          logical     (true) plot SSH
c   do_ssh_clm      logical     (true) plot SSH climate errors
c   do_ssh_clm_anm  logical     (true) plot SSH climate errors
c   do_ssh_err      logical     (true) plot SSH forecast errors
c   do_ssh_inc      logical     (true) plot SSH increments
c   do_sss          logical     (true) plot SSS
c   do_sss_err      logical     (true) plot SSS errors
c   do_sss_inc      logical     (true) plot SSS increments
c   do_sst          logical     (true) plot SST
c   do_sst_err      logical     (true) plot SST errors
c   do_sst_inc      logical     (true) plot SST increments
c   do_stats        logical     (true) plot stats time series
c   do_stats_fcst   logical     (true) plot fcst stats time series
c   do_stats_lvl    logical     (true) plot 2D fcst stats time series
c   do_tmp          logical     (true) plot temperature
c   do_tmp_err      logical     (true) plot temperature errors
c   do_tmp_inc      logical     (true) plot temperature increments
c   do_vel          logical     (true) plot velocity
c   do_vel_err      logical     (true) plot velocity errors
c   do_vel_inc      logical     (true) plot velocity increments
c   do_vfy          logical     (true) plot innovation scatter plots
c   do_vol          logical     (true) plot volume definitions
c   dtg1            character   start time for time series plots
c   nest            integer     nest number to plot
c   n_plot          integer     number levels to plot
c   plot_lyr        logical     plot layer fields
c   sal_cnt         real        salinity contour interval
c   sal_del         real        salinity color slicing interval
c   sal_grd         real        maximum salinity gradient
c   sal_min         real        minimum salinity for color slicing
c   ssh_del         real        minimum change in SSH for CH plot
c   ssh_min         real        minimum ssh contour
c   ssh_max         real        maximum ssh contour
c   sss_cnt         real        sss contour interval
c   sss_del         real        sss color slicing interval
c   sss_min         real        minimum sss for color slicing
c   sst_cnt         real        sst contour interval
c   sst_del         real        sst color slicing interval
c   sst_min         real        minimum sst for color slicing
c   tmp_cnt         real        temperature contour interval
c   tmp_del         real        temperature color slicing interval
c   tmp_grd         real        maximum temperature gradient
c   tmp_min         real        minimum temperature for color slicing
c   vel_max         real        velocity maximum at each level
c   vel_thn         integer     skip every (i,j) vector in overlay
c   z_plot          integer     indicies of vertical levels to plot
c   zoom            real        lat,lon corners for zoomed grid subset
c
c     ..declarations
c
      integer   adj_tau
      real      btm_max
      logical   do_arch
      logical   do_arctic, do_antarctic
      logical   do_btm, do_ch, do_clm, do_clm_err
      logical   do_data, do_data_raw
      logical   do_dist, do_diurn
      logical   do_ensm, do_ensm_std
      logical   do_fcst_diff, do_fcst_err
      logical   do_flow, do_grdnt
      logical   do_gpt, do_gpt_inc
      logical   do_mdl_bias
      logical   do_ice, do_ice_err, do_ice_inc
      logical   do_jmin
      logical   do_lyp_inc
      logical   do_mld
      logical   do_ohc
      logical   do_vfy, do_prf_fcst, do_prf_vfy
      logical   do_prs
      logical   do_sal, do_sal_err, do_sal_inc
      logical   do_scl
      logical   do_sens
      logical   do_sfc_vel
      logical   do_sla
      logical   do_ssh, do_ssh_clm, do_ssh_clm_anm, do_ssh_err
      logical   do_ssh_inc
      logical   do_sss, do_sss_err, do_sss_inc
      logical   do_sst, do_sst_clm, do_sst_err, do_sst_inc
      logical   do_stats, do_stats_fcst, do_stats_lvl
      logical   do_tmp, do_tmp_err, do_tmp_inc
      logical   do_vel, do_vel_err, do_vel_inc
      logical   do_vol
      character dtg1 * 10
      integer   nest
      integer   n_plot
      logical   plot_lyr
      real      sal_cnt, sal_del, sal_grd, sal_min
      real      ssh_del, ssh_min, ssh_max
      real      sss_cnt, sss_del, sss_min
      real      sst_cnt, sst_del, sst_min
      real      tmp_cnt, tmp_del, tmp_grd, tmp_min
      real      vel_max
      integer   vel_thn (2)
      integer   z_plot (50)
      real      zoom (4)
c
c     ..default values
c
      data      adj_tau       / 0 /
      data      btm_max       / 2400. /
      data      do_antarctic  / .false. /
      data      do_arctic     / .false. /
      data      do_arch       / .false. /
      data      do_btm        / .false. /
      data      do_ch         / .false. /
      data      do_clm        / .false. /
      data      do_clm_err    / .false. /
      data      do_data       / .false. /
      data      do_data_raw   / .false. /
      data      do_dist       / .false. /
      data      do_diurn      / .false. /
      data      do_ensm       / .false. /
      data      do_ensm_std   / .false. /
      data      do_fcst_diff  / .false. /
      data      do_fcst_err   / .false. /
      data      do_flow       / .false. /
      data      do_gpt        / .false. /
      data      do_gpt_inc    / .false. /
      data      do_grdnt      / .false. /
      data      do_ice        / .false. /
      data      do_ice_err    / .false. /
      data      do_ice_inc    / .false. /
      data      do_jmin       / .false. /
      data      do_lyp_inc    / .false. /
      data      do_mdl_bias   / .false. /
      data      do_mld        / .false. /
      data      do_ohc        / .false. /
      data      do_prf_fcst   / .false. /
      data      do_prf_vfy    / .false. /
      data      do_prs        / .false. /
      data      do_sal        / .false. /
      data      do_sal_err    / .false. /
      data      do_sal_inc    / .false. /
      data      do_scl        / .false. /
      data      do_sens       / .false. /
      data      do_sfc_vel    / .false. /
      data      do_sla        / .false. /
      data      do_ssh        / .false. /
      data      do_ssh_clm    / .false. /
      data      do_ssh_clm_anm / .false. /
      data      do_ssh_err    / .false. /
      data      do_ssh_inc    / .false. /
      data      do_sss        / .false. /
      data      do_sss_err    / .false. /
      data      do_sss_inc    / .false. /
      data      do_sst        / .false. /
      data      do_sst_clm    / .false. /
      data      do_sst_err    / .false. /
      data      do_sst_inc    / .false. /
      data      do_stats      / .false. /
      data      do_stats_fcst / .false. /
      data      do_stats_lvl  / .false. /
      data      do_tmp        / .false. /
      data      do_tmp_err    / .false. /
      data      do_tmp_inc    / .false. /
      data      do_vel        / .false. /
      data      do_vel_err    / .false. /
      data      do_vel_inc    / .false. /
      data      do_vfy        / .false. /
      data      do_vol        / .false. /
      data      dtg1          / '1990010100' /
      data      nest          / 1 /
      data      n_plot        / 8 /
      data      plot_lyr      / .false. /
      data      sal_cnt       / -1. /
      data      sal_grd       / 0.018 /
      data      sal_del       / 0.1 /
      data      sal_min       / 31. /
      data      ssh_del       / 0. /
      data      ssh_min       / -1.2 /
      data      ssh_max       / 1.2 /
      data      sss_cnt       / -1. /
      data      sss_del       / 0.1 /
      data      sss_min       / 32. /
      data      sst_cnt       / -1. /
      data      sst_del       / 0.5 /
      data      sst_min       / 0. /
      data      tmp_cnt       / -1. /
      data      tmp_del       / 0.5 /
      data      tmp_grd       / 0.018 /
      data      tmp_min       / 0. /
      data      vel_max       / 90. /
      data      vel_thn       / -1, -1 /
      data      z_plot        / 1, 5, 7, 9, 12, 14, 15, 18, 42*0 /
      data      zoom          / -999., -999., -999., -999. /
c
c     ..namelist statement
c
      namelist /omapnl/ n_plot, z_plot, adj_tau, btm_max,
     *                  nest, zoom, plot_lyr,
     *                  do_arch,
     *                  do_antarctic, do_arctic,
     *                  do_btm, do_ch, do_clm, do_clm_err, 
     *                  do_data, do_data_raw,
     *                  do_dist, do_diurn,
     *                  do_ensm, do_ensm_std,
     *                  do_fcst_diff, do_fcst_err, do_flow, 
     *                  do_gpt, do_gpt_inc, do_grdnt,
     *                  do_ice, do_ice_err, do_ice_inc,
     *                  do_jmin, do_lyp_inc, do_mdl_bias,
     *                  do_mld, do_ohc, do_prs, do_scl,
     *                  do_sal, do_sal_err, do_sal_inc,
     *                  sal_cnt, sal_del, sal_grd, sal_min,
     *                  do_sens, do_sfc_vel, do_sla,
     *                  do_ssh, do_ssh_clm, do_ssh_clm_anm,
     *                  do_ssh_err, do_ssh_inc,
     *                  ssh_del, ssh_min, ssh_max,
     *                  do_sss, do_sss, do_sss_err, do_sss_inc, 
     *                  sss_cnt, sss_del, sss_min,
     *                  do_sst, do_sst_err, do_sst_clm, do_sst_inc,
     *                  sst_cnt, sst_del, sst_min,
     *                  do_stats, do_stats_fcst, do_stats_lvl,
     *                  do_tmp, do_tmp_err, do_tmp_inc, 
     *                  tmp_cnt, tmp_del, tmp_grd, tmp_min,
     *                  do_vel, do_vel_err, do_vel_inc, do_vfy, 
     *                  vel_max, vel_thn,
     *                  do_prf_fcst, do_prf_vfy, do_vol,
     *                  dtg1
c

