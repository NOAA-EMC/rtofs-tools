      subroutine stats_ser_lvl (opt, n_taus, n_lvl, n, ks, ke,
     *                          dtg, anl, fcst, z_lvl, var_lbl,
     *                          var, unit, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  stats_ser_lvl
c
c DESCRIPTION:  plots analysis vertical verification time series
c
c PARAMETERS:
c       Name         Type        Usage            Description
c   ------------    ---------    ------    ---------------------------
c   dtg             character    input     analysis date time groups
c   anl             real         input     analysis stats
c   fcst            real         input     forecast stats
c   domain          character    input     regional domain name
c   n               integer      input     starting time periods
c   var             integer      input     variable number
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
c     ..set cross section dimensions
c
      integer    MX_HRZ
      parameter (MX_HRZ = 360)
c
      integer    MX_VRT
      parameter (MX_VRT = 300)
c
c     ..set plot array dimensions
c
      integer    MX_AMAP
      parameter (MX_AMAP = 8 000 000)
c
      integer    MX_CNT
      parameter (MX_CNT = 16 000)
c
      integer    MX_WL
      parameter (MX_WL = 80 000)
c
c     ..plot upper 1500 m
c
      integer    NZ
c     parameter (NZ = 21)
      parameter (NZ = 26)
c     
c     ..set missing value
c
      real       ZDUM 
      parameter (ZDUM = -999.)
c
c     ..local array dimension
c
      integer   n_taus
      integer   n_lvl
c
      real      anl (n_taus, n_lvl)
      real      cntr_int
      character date * 6
      character date_beg * 6
      character date_end * 6
      integer   day
      real      del
      real      dmn, dmx
      character dtg (n_taus) * 10
      real      dz
      real      fcst (n_taus, n_lvl)
      integer   fno
      integer   i, j, k, m
      integer   idm
      integer   ks, ke
      integer   lntr
      integer   mon
      character month (12) * 3
      integer   n
      integer   nx
      integer   n_clrs
      integer   n_chrt
      integer   n_pass
      character opt * (*)
      real      pos (4, 2)
      integer   row_swap
      real      siz
      character time1 * 4, time2 * 4, time3 * 4
      character title * 256
      character unit * (*)
      integer   var
      character var_lbl * (*)
      character watch * 2
      real      x
      real      z_lvl (n_lvl+1)
c
c     ..allocatable arrays
c
      integer,  allocatable :: color_ndx (:)
      character,allocatable :: color_lbl (:) * 10
      integer,  allocatable :: iaia (:)
      integer,  allocatable :: iamap (:)
      integer,  allocatable :: igia (:)
      integer,  allocatable :: iwork (:)
      real,     allocatable :: prf (:)
      real,     allocatable :: prf_int (:)
      real,     allocatable :: rwork (:)
      real,     allocatable :: slc (:)
      real,     allocatable :: xcra (:)
      real,     allocatable :: xsect (:,:)
      real,     allocatable :: ycra (:)
      real,     allocatable :: z_int (:)
      real,     allocatable :: z_vrt (:)
c
c     ..NCAR external routines
c
      external  crsram
c
      include 'color_table.h'
c
c     ..define set positions in plot frame
c
      data ((pos(i,j), i = 1, 4), j = 1, 2) /
     *   0.10, 0.90, 0.55, 0.85,
     *   0.10, 0.90, 0.15, 0.45 /
c
c     ..set months of the year
c
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /
c
c...............................executable..............................
c
c     ..allocate arrays
c
      allocate (color_ndx (MX_CNT))
      allocate (color_lbl (MX_CNT))
      allocate (iaia (MX_WL))
      allocate (iamap (MX_AMAP))
      allocate (igia (MX_WL))
      allocate (iwork (MX_WL))
      allocate (prf (NZ))
      allocate (prf_int (MX_VRT))
      allocate (rwork (MX_WL))
      allocate (slc (MX_HRZ))
      allocate (xcra (MX_WL))
      allocate (xsect (MX_HRZ, MX_VRT))
      allocate (ycra (MX_WL))
      allocate (z_int (MX_VRT))
      allocate (z_vrt (NZ))
c
c     ..form beginning and ending date labels
c
      read (dtg(n)(5:8), '(2i2)') mon, day
      write (date_beg, '(i2, 1x, a)') day, month(mon)
      date_beg = adjustl (date_beg)
      read (dtg(ke)(5:8), '(2i2)') mon, day
      write (date_end, '(i2, 1x, a)') day, month(mon)
      date_end = adjustr (date_end)
c
c     ..form depth interpolation array
c
      do i = 1, NZ
         z_vrt(i) = z_lvl(i)
      enddo
      dz = (z_vrt(NZ) - z_vrt(1)) / real (MX_VRT-1)
      do i = 1, MX_VRT
         z_int(i) = z_vrt(1) + real (i-1) * dz
      enddo
c
c     ..build dtg time check variables for labels
c
      watch = dtg(n)(9:10)
      if (ke .le. 60) then
         time1 = '10' // watch
         time2 = '20' // watch
         time3 = '30' // watch
      else
         time1 = '01' // watch
         time2 = 'XX' // watch
         time3 = 'XX' // watch
      endif
c
c     ..set number horizontal positions and passes
c
      nx = ke - n + 1 
      n_pass = 2
      if (opt .eq. 'Count') then
         n_chrt = 1
      else
         n_chrt = 2
      endif
      call setusv ('LW', 2000)
c
c-----------------------------------------------------------------------
c
c     ..loop over cross section plots
c
      do m = 1, n_chrt
c
c        ..initialize cross section array
c
         do j = 1, MX_VRT
            do i = 1, MX_HRZ
               xsect(i,j) = ZDUM
            enddo
         enddo
c
c        ..set plot array
c
         do i = n, ke
            if (m .eq. 1) then
               do j = 1, NZ
                  prf(j) = fcst(i,j)
                  if (prf(j) .lt. -990.) prf(j) = 0.
               enddo
            else if (m .eq. 2) then
               do j = 1, NZ
                  prf(j) = anl(i,j)
                  if (prf(j) .lt. -990.) prf(j) = 0.
               enddo
            endif
            call prof_trp (NZ, z_vrt, prf, ZDUM,
     *                     MX_VRT, z_int, prf_int)
            k = i - n + 1
            do j = 1, MX_VRT
               xsect(k,j) = prf_int(j)
            enddo
         enddo
c
c        ..swap array rows for vertical sections
c
         row_swap = MX_VRT / 2
         do j = 1, row_swap
            do i = 1, MX_HRZ
               k = MX_VRT - j + 1
               x = xsect(i,j)
               xsect(i,j) = xsect(i,k)
               xsect(i,k) = x
            enddo
         enddo
c
c        ..smooth cross section field
c
         call smth_2d (n_pass, xsect, MX_HRZ, MX_VRT, ZDUM)
c
c        ..set color slicing
c
         call setusv ('LW', 2000)
         if (m .eq. 1) then
            if (opt .eq. 'Bias') then
               call gks_color (rgb_anm_clr, MX_ANM_CLR)
               n_clrs = n_slca
               lntr = 5 
               if (var .eq. 6) then
                  dmx =  90.
                  dmn = -90.
                  cntr_int = 30.
               else
                  if (var .eq. 1) then
c                    dmx =  1.0
c                    dmn = -1.0
c                    cntr_int = 0.2
                     dmx =  1.2
                     dmn = -1.2
      dmx = 0.6
      dmn = -0.6
                     cntr_int = 0.1
                  else if (var .eq. 2) then
c                    dmx =  0.15
c                    dmn = -0.15
c                    cntr_int = 0.05
                     dmx =  0.30
                     dmn = -0.30
                     cntr_int = 0.1
                  else if (var .eq. 3) then
                     dmx =  0.03
                     dmn = -0.03
                     cntr_int = 0.01
                  else
                     dmx =  0.15
                     dmn = -0.15
                     cntr_int = 0.1
                  endif
               endif
            else if (opt .eq. 'RMSE') then
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               n_clrs = n_slc 
               lntr = 20
               if (var .eq. 6) then
                  dmx = 120.
                  dmn = 0.
                  cntr_int = 20.
               else
                  dmn = 0.
                  if (var .eq. 1) then
                     dmx = 1.5
      dmx = 1.2
                     cntr_int = 0.2
                  else if (var .eq. 2) then
                     dmx = 0.6
                     cntr_int = 0.2
                  else if (var .eq. 3) then
                     dmx = 0.9
                     cntr_int = 0.03
                  else
                     dmx = 0.3
                     cntr_int = 0.1
                  endif
               endif
            else if (opt .eq. 'Count') then
               call gks_color (rgb_fld_clr, MX_FLD_CLR)
               n_clrs = n_slc
               lntr = 30
               dmn = 0.
               dmx = maxval (xsect) * 0.5
               if (dmx .gt. 10000.) then
                  idm = nint (((dmx * 1.5) / 10000.))
                  dmx = real (idm) * 10000.  
               else if (dmx .gt. 1000.) then
                  idm = nint (((dmx * 1.5) / 1000.))
                  dmx = real (idm) * 1000. 
               else if (dmx .gt. 100.) then
                  idm = nint (((dmx * 1.5) / 100.))
                  dmx = real (idm) * 100. 
               else
                  dmx = 100. 
               endif
               cntr_int = real (int (dmx) / 2)
            endif
         endif
c
c        ..set color slices
c
         del = (dmx - dmn) / real (n_clrs)
         do i = 1, (n_clrs+1)
            slc(i) = real (i-1) * del + dmn
         enddo
c
c        ..set position of cross section in plot frame
c
         call cpseti ('SET - do not do set call', 0)
         call cpseti ('MAP - mapping flag', 0)
         call set (pos(1,m), pos(2,m), pos(3,m), pos(4,m),
     *             pos(1,m), pos(2,m), pos(3,m), pos(4,m), 1)
         call cpsetr ('XC1 - left boundary', pos(1,m))
         call cpsetr ('XCM - right boundary', pos(2,m))
         call cpsetr ('YC1 - bottom boundary', pos(3,m))
         call cpsetr ('YCN - top boundary', pos(4,m))
c
c        ..set plot parameters
c
         call cpsetc ('CFT - constant field label', ' ')
         call cpsetc ('ILT - information label text string', ' ')
         call cpsetr ('SPV - special value', ZDUM)
         call cpsetc ('HLT', ' ')
         call cpseti ('NOF - numeric ommission flag', 7)
c
c        ..color fill
c
         call gsfais (1)
         call arinam (iamap, MX_AMAP)
         call cpseti ('GIC - group identifier for contour lines', 3)
         call cpseti ('CLS - contour level selector', 0)
         call cpseti ('NCL - number of contour levels', (n_clrs-1))
         call cpsetr ('T2D - contour smoothing parameter', 0.)
         do i = 1, (n_clrs-1)
            call cpseti ('PAI - parameter array index', i)
            call cpsetr ('CLV - contour level', slc(i+1))
            call cpseti ('CLU - contour level use', 1)
            call cpseti ('AIB - area identifier below level', i)
            call cpseti ('AIA - area identifier above level', (i+1))
         enddo
         call cprect (xsect, MX_HRZ, nx, MX_VRT, rwork,
     *                MX_WL, iwork, MX_WL)
         call cpclam (xsect, rwork, iwork, iamap)
         call arscam (iamap, xcra, ycra, MX_WL, iaia, igia,
     *                MX_WL, crsram)
c
c        ..set parameters for constant contour interval
c
         call cpsetr ('CIS - contour interval specifier', cntr_int)
         call cpseti ('CLS - contour level selection flag', 1)
         call cpseti ('LIS - label interval specifier', 1)
         call cpsetr ('T2D - contour smoothing parameter', 2.5)
c
c        ..set line labelling scheme
c
         call cpseti ('LLP - line label positioning', 3)
         call cpseti ('LLO - line label orientation', 0)
         call cpsetr ('LLS - line label size', .015)
         call cpseti ('LLB - line label box flag', 2)
         call cpseti ('LBC - label box color index', 0)
         call cpsetr ('LLW - line label white space', .005)
c
c        ..draw and label contour lines
c
         call gsplci (1)
         call gstxci (1)
         call cprect (xsect, MX_HRZ, nx, MX_VRT, rwork,
     *                MX_WL, iwork, MX_WL)
         call cpcldr (xsect, rwork, iwork)
c        call cplbdr (xsect, rwork, iwork)
c
c        ..draw border around contour plot
c
         call line (pos(1,m), pos(3,m), pos(2,m), pos(3,m))
         call line (pos(1,m), pos(3,m), pos(1,m), pos(4,m))
         call line (pos(1,m), pos(4,m), pos(2,m), pos(4,m))
         call line (pos(2,m), pos(3,m), pos(2,m), pos(4,m))
c
c        ..label axes
c
         siz = .014
         call gsplci (1)
         del = 200.
         call lbl_axis (z_vrt(1), z_vrt(NZ), del, del,
     *                  'Depth (M)', 90., siz, .09, .true.)
c
c        ..plot title
c
         if (opt .eq. 'Bias' .or. opt .eq. 'RMSE') then
            if (m .eq. 1) then
               title = 'Innovations'
            else if (m .eq. 2) then
               title = 'Residuals'
            endif
            call title_plot (title, .017, 1, 1.0)
         endif
c
c        ..put label bar to the right of the plot
c
         if (m .eq. 1) then
            do i = 1, MX_CNT
               color_ndx(i) = i + 4
               color_lbl(i) = '          '
            enddo
            call gsfais (1)
            call lbseti ('CBL - color boxlines', 1)
            call lbseti ('CLB - color labels', 1)
            if (opt .eq. 'Bias') then
               do i = 1, (n_clrs+1), lntr
                  write (color_lbl(i), '(f8.2)') slc(i)
                  color_lbl(i) = adjustl (color_lbl(i))
               enddo
               call lblbar (1, .91, .99, .30, .70, n_clrs, 0.25, 1.0,
     *                      color_ndx, 0, color_lbl, (n_clrs+1), 1)
            else if (opt .eq. 'RMSE') then
               do i = 1, (n_clrs+1), lntr
                  write (color_lbl(i), '(f8.2)') slc(i)
                  color_lbl(i) = adjustl (color_lbl(i))
               enddo
               call lblbar (1, .91, .99, .30, .70, n_clrs, 0.25, 1.0,
     *                      color_ndx, 0, color_lbl, (n_clrs+1), 1)
            else if (opt .eq. 'Count') then
               do i = 1, (n_clrs+1), lntr
                  write (color_lbl(i), '(i8)') nint (slc(i))
                  color_lbl(i) = adjustl (color_lbl(i))
               enddo
               call lblbar (0, .20, .80, .44, .49, n_clrs, 1., 0.25,
     *                      color_ndx, 0, color_lbl, (n_clrs+1), 1)
            endif
c
c           ..mark time periods
c
            call gsplci (1)
            call line (pos(1,m), pos(3,m), pos(1,m),
     *                (pos(3,m)-0.04*(pos(4,m)-pos(3,m))))
            call line (pos(2,m), pos(3,m), pos(2,m),
     *                (pos(3,m)-0.04*(pos(4,m)-pos(3,m))))
            do i = 2, (nx-1)
               x = (pos(2,m)-pos(1,m))*(real(i)-1.)/
     *             (real(nx)-1.)+pos(1,m)
               call line (x, pos(3,m), x,
     *                   (pos(3,m)-0.02*(pos(4,m)-pos(3,m))))
            enddo
         endif
      enddo
c
c     ..mark time periods 
c
      m = m - 1
      call gsplci (1)
      call line (pos(1,m), pos(3,m), pos(1,m),
     *          (pos(3,m)-0.04*(pos(4,m)-pos(3,m))))
      call plchhq (pos(1,m), (pos(3,m)-0.1*(pos(4,m)-pos(3,m))),
     *             date_beg, .011, 0., 0.)
      call line (pos(2,m), pos(3,m), pos(2,m),
     *          (pos(3,m)-0.04*(pos(4,m)-pos(3,m))))
      call plchhq (pos(2,m), (pos(3,m)-0.1*(pos(4,m)-pos(3,m))),
     *             date_end, .011, 0., 0.)
      do i = 2, (nx-1)
         x = (pos(2,m)-pos(1,m))*(real(i)-1.)/
     *       (real(nx)-1.)+pos(1,m)
         call line (x, pos(3,m), x,
     *             (pos(3,m)-0.02*(pos(4,m)-pos(3,m))))
      enddo
      do i = (n+5), (ke-5)
         if (dtg(i)(7:10) .eq. time1 .or.
     *       dtg(i)(7:10) .eq. time2 .or.
     *       dtg(i)(7:10) .eq. time3) then
            read (dtg(i)(5:8), '(2i2)') mon, day
            write (date, '(i2, 1x, a)') day, month(mon)
            x = (pos(2,m)-pos(1,m))*((real(i-n)-1.)/
     *          (real(nx)-1.))+pos(1,m)
            call plchhq (x, (pos(3,m)-0.1*(pos(4,m)-pos(3,m))),
     *                   date, .011, 0., 0.)
            call line (x, pos(3,m), x,
     *                (pos(3,m)-0.04*(pos(4,m)-pos(3,m))))
         endif
      enddo
c
c     ..put title above chart
c
      call set (.1, .9, .1, .9, .1, .9, .1, .9, 1)
      var_lbl = adjustl (var_lbl)
      if (opt .eq. 'Count') then
         title = trim (var_lbl) // ' Data Counts' 
      else
         title = trim (var_lbl) // ' ' // opt //
     *           ' Verification ' // unit
      endif
      call title_plot (title, .022, 1, 1.0)
      call plotit (0, 0, 0)
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': Vertical '', a)')
     *       fno, trim (title)
c
c     ..advance the plot buffer
c
      call frame
c
c     ..clean up
c
      call setusv ('LW', 1000)
      deallocate (color_ndx, color_lbl, iaia, iamap, igia, iwork)
      deallocate (prf, prf_int, rwork, slc, xcra, xsect, ycra)
      deallocate (z_int, z_vrt)
c
      return
      end
