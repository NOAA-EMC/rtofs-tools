      subroutine error_exit (routine, message)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  error_exit
c
c DESCRIPTION:  prints a fatal error message and terminates the program.
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libcoda.a
c
c PARAMETERS:
c    Name          Type       Usage            Description
c   -------     ----------    ------    ---------------------------
c   routine     char * (*)    input     name of routine
c   message     char * (*)    input     user supplied error message
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
      integer   ln
      character message * (*)
      character routine * (*)
c
c..............................executable...............................
c
c     ..determine message string length
c
      ln = len_trim (message)
c
      write (*, '(//, ''*** FATAL ERROR ('', a, '') ***'')') routine
      write (*, '(/, a)') message(1:ln)
      write (*, '(/, ''*** PROGRAM TERMINATED ***'', /)')
c
c     ..exit with non zero completion code
c
      call exit (15)
      end
