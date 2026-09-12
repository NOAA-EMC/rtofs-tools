      subroutine plot_vrfy (dir_path, date, dtg, nest, m, n, fno)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  plot_vrfy
c
c DESCRIPTION:  plot obs verification scatter plots
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
      integer    UNIT
      parameter (UNIT = 10)
c
c     ..local array dimensions
c
      character date * 15
      character dir_path * (*)
      character dtg * 10
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   fno
      integer   len, len_dir
      character lvl_typ * 3
      integer   m, n
      integer   n_obs
      integer   nest
      integer   tau
c
      include 'color_table.h'
c
c...............................executable..............................
c
c     ..initialization
c
      file_typ = 'obsdata'
      fluid = 'o'
      len_dir = len_trim (dir_path) + 1
      lvl_typ = 'sfc'
      tau = 0
c
c     ..ice observations
c
      fld_name = 'icecov'
      call cr_fname (dir_path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         write (*, '(11x, ''read file: '', a)')
     *          trim (file_name(len_dir:len))
         open (UNIT, file=file_name(1:len), status='unknown',
     *               form='unformatted')
         read (UNIT) n_obs
         if (n_obs .gt. 0) then
            call gks_color (rgb_obs_clr, MX_OBS_CLR)
            call plot_sctr ('ice', n_obs, date, UNIT, fno)
         endif
         close (UNIT)
      else
         write (*, '(8x, ''file missing: '', a)')
     *          trim (file_name(len_dir:len))
      endif
c
c     ..sst observations
c
      fld_name = 'seatmp'
      call cr_fname (dir_path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         write (*, '(11x, ''read file: '', a)') 
     *          trim (file_name(len_dir:len))
         open (UNIT, file=file_name(1:len), status='unknown',
     *               form='unformatted')
         read (UNIT) n_obs
         if (n_obs .gt. 0) then
            call gks_color (rgb_obs_clr, MX_OBS_CLR)
            call plot_sctr ('sst', n_obs, date, UNIT, fno)
         endif
         close (UNIT)
      else
         write (*, '(8x, ''file missing: '', a)')
     *          trim (file_name(len_dir:len))
      endif
c
c     ..sss observations
c
      fld_name = 'salint'
      call cr_fname (dir_path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         write (*, '(11x, ''read file: '', a)')
     *          trim (file_name(len_dir:len))
         open (UNIT, file=file_name(1:len), status='unknown',
     *               form='unformatted')
         read (UNIT) n_obs
         if (n_obs .gt. 0) then
            call gks_color (rgb_obs_clr, MX_OBS_CLR)
            call plot_sctr ('sss', n_obs, date, UNIT, fno)
         endif
         close (UNIT)
      else
         write (*, '(8x, ''file missing: '', a)')
     *          trim (file_name(len_dir:len))
      endif
c
c     ..ssh observations
c
      fld_name = 'seahgt'
      call cr_fname (dir_path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         write (*, '(11x, ''read file: '', a)') 
     *          trim (file_name(len_dir:len))
         open (UNIT, file=file_name(1:len), status='unknown',
     *               form='unformatted')
         read (UNIT) n_obs
         if (n_obs .gt. 0) then
            call gks_color (rgb_obs_clr, MX_OBS_CLR)
            call plot_sctr ('ssh', n_obs, date, UNIT, fno)
         endif
         close (UNIT)
      else
         write (*, '(8x, ''file missing: '', a)')
     *          trim (file_name(len_dir:len))
      endif
c
c     ..surface velocity observations
c
      fld_name = 'ocnvel'
      call cr_fname (dir_path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         write (*, '(11x, ''read file: '', a)') 
     *          trim (file_name(len_dir:len))
         open (UNIT, file=file_name(1:len), status='unknown',
     *               form='unformatted')
         read (UNIT) n_obs
         if (n_obs .gt. 0) then
            call gks_color (rgb_obs_clr, MX_OBS_CLR)
            call plot_sctr ('uuu', n_obs, date, UNIT, fno)
         endif
         close (UNIT)
      else
         write (*, '(8x, ''file missing: '', a)')
     *          trim (file_name(len_dir:len))
      endif
c
c     ..multivariate observations
c
      fld_name = 'ocnobs'
      call cr_fname (dir_path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         write (*, '(11x, ''read file: '', a)') 
     *          trim (file_name(len_dir:len))
         open (UNIT, file=file_name(1:len), status='unknown',
     *               form='unformatted')
         read (UNIT) n_obs
         if (n_obs .gt. 0) then
            call gks_color (rgb_obs_clr, MX_OBS_CLR)
            call plot_sctr ('mvo', n_obs, date, UNIT, fno)
         endif
         close (UNIT)
      else
         write (*, '(8x, ''file missing: '', a)')
     *          trim (file_name(len_dir:len))
      endif
c
      return
      end
