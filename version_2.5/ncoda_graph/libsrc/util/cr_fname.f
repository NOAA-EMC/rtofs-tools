      subroutine cr_fname (out_dir, file_dtg, nest, m, n, file_typ,
     *                     fld_name, fluid, lvl_typ, tau, file_name,
     *                     len)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  cr_fname
c
c DESCRIPTION:  creates CODA restart field file name
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c       Name          Type       Usage            Description
c   -------------   ---------   ------   -------------------------------
c   file_dtg        character   input    analysis date time group
c   file_name       character   output   field name
c   file_typ        character   input    file type indicator
c   fld_name        character   input    field name indicator
c   fluid           character   input    model type (a=atm, o=ocn)
c   len             integer     output   field name length
c   lvl_typ         character   input    level type indicator
c   m               integer     input    number x grid positions
c   n               integer     input    number y grid positions
c   nest            integer     input    nested grid number
c   out_dir         character   input    output file data directory
c   tau             integer     input    analysis/forecast hour
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
      character file_dtg * 10
      character file_name * 256
      character file_typ * 7
      character fld_name * 6
      character fluid * 1
      integer   len
      integer   len_dir
      integer   len_sfx
      character lvl_typ * 3
      integer   m, n
      integer   nest
      character out_dir * (*)
      character sfx * 64
      integer   tau
c
c...............................executable..............................
c
c     ..create file name
c
      write (sfx, 100) fld_name, lvl_typ, nest, fluid, m, n, file_dtg, 
     *                 tau, file_typ
c
c     ..append file name to directory path
c
      len_dir = len_trim (out_dir)
      len_sfx = len_trim (sfx)
      file_name = out_dir(1:len_dir) // '/' // sfx(1:len_sfx)
      len = len_trim (file_name)
c
c-----------------------------------------------------------------------
c
  100 format (a6, '_', a3, '_', i1, a1, i4.4, 'x', i4.4, '_', 
     *        a10, '_', i4.4, '_', a7)
c
      return
      end
