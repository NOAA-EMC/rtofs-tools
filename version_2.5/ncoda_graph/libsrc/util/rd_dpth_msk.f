      subroutine rd_dpth_msk (dir_path, dtg, m, n, nest, depth, mask)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  rd_dpth_msk
c
c DESCRIPTION:  reads bathymetry and mask files for given nest
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
      integer   m, n
c
      real      depth (m * n)
      character dir_path * (*)
      character dtg * 10
      character err_msg * 256
      logical   exist
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   i
      integer   len, len_dir
      character lvl_typ * 3
      integer   mask (m * n)
      integer   nest
      integer   tau_hr
c
c...............................executable..............................
c
c     ..initialize
c
      fluid = 'o'
      len_dir = len_trim (dir_path) + 1
      lvl_typ = 'sfc'
      tau_hr = 0
c
c     ..retrieve bathymetry
c
      file_typ = 'datafld'
      fld_name = 'depths'
      call cr_fname (dir_path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau_hr, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='old',
     *               access='stream', form='unformatted')
         read (UNIT) depth
         close (UNIT)
         write (*, '(''       restart found: '', a)')
     *          file_name(len_dir:len)
      else
         write (err_msg, '(''missing bottom depth file "'', a, ''"'')')
     *          file_name(len_dir:len)
         call error_exit ('RD_DEPTH', err_msg)
      endif
c
c     ..retrieve mask
c
      file_typ = 'datafld'
      fld_name = 'maskls'
      call cr_fname (dir_path, dtg, nest, m, n, file_typ, fld_name,
     *               fluid, lvl_typ, tau_hr, file_name, len)
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='old',
     *               access='stream', form='unformatted')
         read (UNIT) mask
         close (UNIT)
         write (*, '(''       restart found: '', a)')
     *          file_name(len_dir:len)
      else
         write (*, '(''     restart missing: '', a)')
     *          file_name(len_dir:len)
         do i = 1, (m * n)
            mask(i) = 100
         enddo
      endif
c
      return
      end
