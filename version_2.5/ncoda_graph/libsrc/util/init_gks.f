      subroutine init_gks (opt, name)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  init_gks
c
c DESCRIPTION:  calls low level gks rotines for opening and closing
c               of gks workstations used in ncar graphics  
c
c               option exists to rename gmeta file
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libocnqc.a
c      
c PARAMETERS:
c       Name        Type       Usage            Description
c   ----------    --------    -------   ----------------------------
c   opt            char        input    processing option
c                                       'opn' open workstation
c                                       'cls' close workstation
c   name           char        input    gmeta output file name
c
c....................MAINTENANCE SECTION................................
c
c METHOD:
c
c..............................END PROLOGUE.............................
c
      implicit none
c
      character dmy * 80
      character err_msg * 256
      character name * 80
      character opt * 3
c
c...............................executable..............................
c
      if (opt .eq. 'opn') then
c
c        ..open gks
c
         call gopks (6, 0)
         call gesc (-1391, 1, name, 1, 1, dmy)
         call gopwk (1, 2, 1)
         call gacwk (1)
      else if (opt .eq. 'cls') then
c
c        ..close gks
c
         call gdawk (1)
         call gclwk (1)
         call gclks
      else
         write (err_msg, '(''unknown gks processing option: "'', 
     *                     a, ''"'')') opt
         call error_exit ('INIT_GKS', err_msg)
      endif
c
      return
      end
