      subroutine rd_coda_file (out_dir, file_dtg, nest, m, n, l,
     *                         file_typ, fld_name, fluid, tau,
     *                         lvl_typ, field, diag, fail)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  rd_coda_file
c
c DESCRIPTION:  reads CODA restart field
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c       Name          Type       Usage            Description
c   -------------   ---------   ------   -------------------------------
c   diag            logical     input    (true) diagnostics
c   field           real        output   forecast (analysis) field
c   file_dtg        character   input    analysis date time group
c   file_typ        character   input    file type indicator
c   fld_name        character   input    field name indicator
c   fluid           character   input    model type (a=atm, o=ocn)
c   lvl_typ         character   input    level type indicator
c   m               integer     input    number x grid positions
c   n               integer     input    number y grid positions
c   l               integer     input    number z grid positions
c   nest            integer     input    nested grid number
c   out_dir         character   input    data directory to search
c   upd_cycle       integer     input    analysis update cycle (hours)
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
      logical   diag
      logical   exist
      logical   fail
      real      field (m * n * l)
      character file_dtg * 10
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   len, len_dir
      character lvl_typ * 3
      integer   nest
      character out_dir * (*)
      integer   tau
c
c...............................executable..............................
c
c     ..build file name
c
      call cr_fname (out_dir, file_dtg, nest, m, n, file_typ,
     *               fld_name, fluid, lvl_typ, tau, file_name,
     *               len)
c
c     ..check for file existence, read field
c
      len_dir = len_trim (out_dir) + 1
      inquire (file=file_name(1:len), exist=exist)
      if (exist) then
         open (UNIT, file=file_name(1:len), status='old',
     *               access='stream', form='unformatted')
         read (UNIT) field
         close (UNIT)
         fail = .false.
         if (diag) then
            write (*, '(''       restart found: '', a)')
     *             file_name(len_dir:len)
         endif
      else
         fail = .true.
         if (diag) then
            write (*, '(''     restart missing: '', a)')
     *             file_name(len_dir:len)
         endif
      endif
c
      return
      end
