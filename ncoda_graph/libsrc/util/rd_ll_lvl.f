      subroutine rd_ll_lvl (path, dtg, lyr,  m, n, l, nest,
     *                      lat, lon, lvl)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  rd_ll_lvl
c
c DESCRIPTION:  reads lat, lon, and level files for given nest
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
      parameter (UNIT = 60)
c
c     ..local array dimensions
c
      integer   m, n, l
c
      character dtg * 10
      character err_msg * 256
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      real      lat (m * n)
      real      lon (m * n)
      integer   len, len_dir
      real      lvl (m * n * l)
      character lvl_typ * 3
      logical   lyr
      integer   nest
      character path * (*)
      integer   tau
c**********************
      integer i
      character file_dtg* 10
      integer status
c**********************
c
c...............................executable..............................
c
c     ..initialize
c
      file_typ = 'datafld'
      fluid = 'o'
      len_dir = len_trim (path) + 1
      tau = 0
c
c     ..retrieve grid latitudes
c
      fld_name = 'grdlat'
      lvl_typ = 'sfc'
      call cr_fname (path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='old',
     *               access='stream', form='unformatted')
         read (UNIT) lat
         close (UNIT)
         write (*, '(''       restart found: '', a)')
     *          file_name(len_dir:len)
      else
         write (err_msg, '(''missing grid lat file "'', a, ''"'')')
     *          file_name(len_dir:len)
         call error_exit ('RD_LL_LVL', err_msg)
      endif
c
c     ..retrieve grid longitudes
c
      fld_name = 'grdlon'
      lvl_typ = 'sfc'
      call cr_fname (path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='old',
     *               access='stream', form='unformatted')
         read (UNIT) lon
         close (UNIT)
         write (*, '(''       restart found: '', a)')
     *          file_name(len_dir:len)
      else
         write (err_msg, '(''missing grid lon file "'', a, ''"'')')
     *          file_name(len_dir:len)
         call error_exit ('RD_LL_LVL', err_msg)
      endif
c
c     ..retrieve grid levels or grid layers
c
      fld_name = 'grdlvl'
      lvl_typ = 'pre'
      if (lyr) then
         call dtgmod (dtg, -24, file_dtg, status)
         call cr_fname (path, file_dtg, nest, m, n, 'fcstfld',
     *                  'lyrprs', fluid, 'lyr', 24, file_name,
     *                  len)
      else
         call cr_fname (path, dtg, nest, m, n, file_typ, fld_name,
     *                  fluid, lvl_typ, tau, file_name, len)
      endif
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='old',
     *               access='stream', form='unformatted')
         read (UNIT) lvl
         close (UNIT)
         write (*, '(''       restart found: '', a)')
     *          file_name(len_dir:len)
      else
         write (err_msg, '(''missing grid level file "'', a, ''"'')')
     *          file_name(len_dir:len)
         call error_exit ('RD_LL_LVL', err_msg)
      endif
c
      return
      end
