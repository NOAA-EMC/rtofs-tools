      subroutine set_uplow (opt, word)
c
c.............................START PROLOGUE............................
c
c CONFIGURATION IDENTIFICATION:
c      $HeadURL$
c      @(#)$Id$
c
c MODULE NAME:  set_low
c
c DESCRIPTION:  convert an upper case character string to a lower
c               case character string and vice versa (opt)
c
c LIBRARIES OF RESIDENCE:   ...ops/lib/libocnqc.a
c      
c PARAMETERS:
c      Name          Type       Usage            Description
c   ----------     --------    -------   ------------------------------
c   opt            character   input     processing option: "low", "up"
c   word           character   in/out    string variable
c
c....................MAINTENANCE SECTION................................
c
c METHOD:  assumes english language and ascii character tables
c
c..............................END PROLOGUE.............................
c
      implicit none
c
      integer   i, ic
      integer   len
      character opt * 3
      character word * (*)
c
c     ..functions
c
      integer   ichar
c
c...............................executable..............................
c
c     ..branch to processing option
c
      len = len_trim (word)
      if (len .eq. 0) return
c
      if (opt .eq. 'low') then
         do i = 1, len
            ic = ichar (word(i:i))
            if (ic >= 65 .and. ic <= 90) word(i:i) = char(ic+32)
         enddo
      else if (opt .eq. 'up ') then
         do i = 1, len
            ic = ichar (word(i:i))
            if (ic >= 97 .and. ic <= 122) word(i:i) = char(ic-32)
         enddo
      endif
c
      return
      end      

