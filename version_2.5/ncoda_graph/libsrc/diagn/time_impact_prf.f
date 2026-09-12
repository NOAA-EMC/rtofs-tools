      subroutine time_impact_prf (n_obs, n_files, n_obs_file, obs_imp,
     *                            obs_sen, obs_typ, obs_var, file_dtg,
     *                            title, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  time_impact_prf
c
c DESCRIPTION:  plots data impact time series
c
c PARAMETERS:
c       Name         Type        Usage            Description
c   ------------    ---------    ------    ---------------------------
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
      integer    N_TYP
      parameter (N_TYP = 11)
c
      integer    N_VAR
      parameter (N_VAR = 2)
c
c     ..local array dimensions
c     
      integer   n_files
      integer   n_obs
c
      character chk * 2
      integer   color (N_TYP)
      character day * 2
      real      del
      character file_dtg (n_files) * 10
      integer   fno
      integer   i, k
      character lbl * 22
      integer   lntr
      integer   n_obs_file (n_files)
      real      obs_imp (n_obs, n_files)
      real      obs_sen (n_obs, n_files)
      integer   obs_typ (n_obs, n_files)
      integer   obs_var (n_obs, n_files)
      real      tck
      integer   typ
      character typ_lbl (N_TYP) * 6
      character title * (*)
      integer   var
      integer   var_typ (N_TYP, N_VAR)
      real      xmn, xmx
      real      ymn, ymx
c
c     ..allocatable data arrays
c
      real,     allocatable :: cnt (:,:)
      real,     allocatable :: imp (:,:)
      real,     allocatable :: sen (:,:)
      real,     allocatable :: tim (:,:)
c
c     ..set color tables
c
      include 'color_table.h'
c
c     ..define types codes (temperature, salintiy)
c
c     data var_typ /  1, 36,  4,  5, 20, 171, 133, 79, 186, 188, 102,
c    *               -1, 37, 52, 54, 32, 172, 134, -1, 187, 189, 103 /
      data var_typ /  1, 36,  4,  5, 20,  19, 133, 79, 186, 188, 102,
     *               -1, 37, 52, 54, 32,  49, 134, -1, 187, 189, 103 /
c
c     ..define type code labels
c
      data typ_lbl /'XBT   ', 'Argo  ', 'Fixed ', 'Drift ',
     *              'TESAC ', 'Altim ', 'Animal', 'SST   ',
     *              'XCTD  ', 'Alamo ', 'Glider' /
c
c...............................executable..............................
c
c     ..set color scheme
c
      call gks_color (rgb_obs_clr, MX_OBS_CLR)
c
c     ..allocate data arrays
c
      allocate (cnt (n_files, N_TYP))
      allocate (imp (n_files, N_TYP))
      allocate (sen (n_files, N_TYP))
      allocate (tim (n_files, N_TYP))
c
c     ..loop over variables (temperature, salintiy)
c
      write (*, '(/, ''Time Series Profile Impacts'')')
      do var = 1, N_VAR
c
c     ..loop over data types
c
      do typ = 1, N_TYP
c
c        ..extract variable and data type and form daily sums
c
         do k = 1, n_files
            cnt(k,typ) = 0.
            imp(k,typ) = 0.
            sen(k,typ) = 0.
            tim(k,typ) = real (k)
            if (n_obs_file(k) .gt. 0) then
               do i = 1, n_obs_file(k)
                  if (obs_typ(i,k) .eq. var_typ(typ,var) .and.
     *                obs_var(i,k) .eq. var) then
                     cnt(k,typ) = cnt(k,typ) + 1.
                     imp(k,typ) = imp(k,typ) + obs_imp(i,k) * 10.
                     sen(k,typ) = sen(k,typ) + obs_sen(i,k)
                  endif
               enddo
            endif
         enddo
c
c        ..form daily means
c
         do k = 1, n_files
            if (cnt(k,typ) .gt. 1.) then
               imp(k,typ) = imp(k,typ) / cnt(k,typ)
               sen(k,typ) = sen(k,typ) / cnt(k,typ)
            endif
         enddo
      enddo
c
c----------------------------------------------------------------------
c
c     ..call set with min/max of time and impact
c
      xmn = 1.
      xmx = real (n_files)
      ymn = minval (imp)
      ymx = maxval (imp)
      if (var .eq. 1) then
         lbl = 'Temperature Impact (C)'
         ymn = -2.
         ymx = 1.
         del = 1.
         tck = .5
         call set (.10, .90, .60, .90, xmn, xmx, ymn, ymx, 1)
      else
         lbl = 'Salinity Impact (PSU) '
         ymn = -1.
         ymx =  .5
         del = .5
         tck = .25
         call set (.10, .90, .20, .50, xmn, xmx, ymn, ymx, 1)
      endif
c
c     ..label impact axis
c
      call gsplci (1)
      call lbl_axis (ymn, ymx, del, tck, lbl, 90., .016, .09, .false.)
c
c     ..draw border around plot
c
      call line (xmn, ymn, xmx, ymn)
      call line (xmn, ymn, xmn, ymx)
      call line (xmn, ymx, xmx, ymx)
      call line (xmx, ymn, xmx, ymx)
c
c     ..mark variable impact
c
      call gsclip (1) 
      call setusv ('LW', 3000)
      do typ = 1, N_TYP
         call gsplci (typ + 2)
         color (typ) = typ + 2
         if (cnt(1,typ) .gt. 0.) then
            call frstpt (tim(1,typ), imp(1,typ))
         else
            call frstpt (tim(1,typ), 0.)
         endif
         do k = 1, n_files
            if (cnt(k,typ) .gt. 0.) then
               call vector (tim(k,typ), imp(k,typ))
               call plchhq (tim(k,typ), imp(k,typ), '+', .01, 0., 0.)
            else
               if (i .lt. n_files) then
                  if (cnt(k+1,typ) .gt. 0.) then
                     call frstpt (tim(k+1,typ), imp(k+1,typ))
                  endif
               endif
            endif
         enddo
         call plotit (0, 0, 0)
      enddo
      call setusv ('LW', 1000)
      call gsclip (0) 
c
c     ..put label bar below plot
c
      call gsplci (1)
      call gsfais (1)
      call lbseti ('CBL - color boxlines', 1)
      call lbseti ('CLB - color labels', 1)
      call lblbar (0, .01, .99, .08, .14, N_TYP, 1., .20,
     *             color, 0, typ_lbl, N_TYP, 1)
c
c     ..mark time periods
c
      call gsplci (1)
      chk = file_dtg(1)(9:10)
      do k = 1, n_files
         call line (tim(k,1), ymn, tim(k,1), (ymn-0.025*(ymx-ymn)))
      enddo
      chk = file_dtg(1)(9:10)
      if (n_files .gt. 31) then
         lntr = 4
      else
         lntr = 2
      endif
      do k = 1, n_files, lntr
         if (file_dtg(k)(9:10) .eq. chk) then
            day = file_dtg(k)(7:8)
            call plchhq (tim(k,1), (ymn-0.06*(ymx-ymn)), day, .011,
     *                   0., 0.)
         endif
      enddo
c
c     ..mark zero line
c
      call setusv ('LW', 3000)
      call line (xmn, 0.0, xmx, 0.0)
      call setusv ('LW', 1000)
c
c     ..plot title
c
      if (var .eq. 1) call title_plot (title, .022, 1, 1.0)
      call plotit (0, 0, 0)
      enddo
c
c     ..advance the plot buffer
c
      fno = fno + 1
      write (*, '(10x, ''frame'', i5, '': '', a)') fno, trim (title)
      call frame
c
c     ..clean up
c
      deallocate (cnt, imp, sen, tim) 
c
      return
      end
