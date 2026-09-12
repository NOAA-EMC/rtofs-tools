      SUBROUTINE DTGOPS ( CDTG, ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGOPS 
C
C  DESCRIPTION:
C
C     DTGOPS returns the date-time group (YYYYMMDDHH) which is one
C     of the following: 
C
C        1) (NOT YET IMPLEMENTED) Current operational DTG.
C        2) + or - offset to current operational DTG
C        3) User supplied DTG 
C
C  USAGE (CALLING SEQUENCE):
C
C        CALL DTGOPS ( CDTG, ISTAT )
C
C     If there is no CRDATE environmental variable, 
C       -(NOT YET IMPLEMENTED)
C        if the file dtg exists, the date-time returned is 
C        the operational watch date;
C       -if the dtg file does not exist, the date-time returned 
C        is a DTG constructed from the Fortran library routine
C	 GMTIME.
C
C     IF there is a CRDATE environmental variable,
C       -if the variable contains a DTG, that DTG will be returned.
C	-if the variable contains CURRENTDTG, the date-time returned
C        is the same as if no CRDATE variable existed.
C	-if the variable contains CURRENTDTG with an offset, the 
C	   date-time returned is the watch or system DTG
C	   (depending on existence of watch DTG file) +/- the
C	   offset hours.
C 
C       To return + or - offset to current DTG:
C         CRDATE is set equal to:
C           CURRENTDTGsnnn
C              where  s = + or -
C                   nnn = offset
C	
C         EXAMPLES:  CURRENTDTG		(This is equivalent to not
C					 using a CRDATE variable.)
C	             CURRENTDTG-12
C                    CURRENTDTG+3
C		     CURRENTDTG+120
C
C       To establish the CRDATE environmental variable, execute one
C	of the following commands:
C
C         COMMANDS			   EXAMPLES
C 	  --------			   --------	
C	  setenv CRDATE DTG		   setenv CRDATE 1991051712
C	  setenv CRDATE CURRENTDTG+/-nnn   setenv CRDATE CURRENTDTG+06
C
C	  When your process and its child processes complete, 
C	    CRDATE will go away.  However, you can destroy it sooner
C	    by doing an unsetenv CRDATE.
C
C 	  See OUTPUT FILES entry for note about error messages.	
C
C  INPUT PARAMETERS:  None.
C
C  OUTPUT PARAMETERS:  
C
C    CDTG 	C*10	DTG in format YYYYMMDDHH
C    ISTAT	INT	Error return variable.  
C			  If =  0, no error.
C			  If = -1, invalid DTG in CRDATE entry.
C			  If = -2, invalid offset in CRDATE entry.
C			     The problem may be with the sign. 	
C
C  INPUT ENVIRONMENTAL VARIABLE
C
C     CRDATE	  Optional environmental variable equal to either
C			- a DTG in col 1-10 or  
C		        - the keyword CURRENTDTG in col 1-10 and an 
C			  optional offset (+/-nnn) in col 11-14.      
C				Ex:  CURRENTDTG+06
C			             1990110412 		
C		        - In the C Shell, you set the variable by
C			  using the setenv command:
C		  	     
C			  % setenv CRDATE CURRENTDTG-120.
C
C  OUTPUT FILES:  std err for informative messages.
C    Note: std err defaults to std out so if you want these messages
C    elsewhere, you'll have to redirect std err at execution time:
C
C		proga 2>msg.file
C
C  CALLS:
C
C    DTGCHK     Character function to validate a DTG.
C    DTGMOD	Subroutine to return a DTG that is the sum of a DTG
C		  and an offset in hours.
C    GETENV 	Get value of environmental variable.
C    GMTIME	Subroutine that returns ZULU date/time in a 9-word
C		  array.
C    PATH 	Subroutine that returns the full pathname of the watch
C		  DTG file, DTG. (NOT YET AVAILABLE. THE CALL IS
C		  COMMENTED OUT.)
C    TIME	Integer function that returns the seconds since 00Z,
C		  Jan. 1, 1970.  That time is the input argument to
C		  GMTIME.
C
C  EXAMPLES:
C
C    1)  Program code: 
C
C     	 CHARACTER NEWDTG*10
C	 INTEGER ISTAT
C
C        CALL DTGOPS ( NEWDTG, ISTAT )
C	 IF ( ISTAT .NE. 0 ) THEN
C	    Error...
C	 ELSE
C	    Okay...
C	 ENDIF
C	 	...
C
C    2)  Results for various conditions:
C 
C        a)  Get current watch DTG, no CRDATE variable.
C  
C	     If it is the 12Z watch on 4 March 1991, 
C            NEWDTG will contain 1991030412.  ISTAT will equal 0.
C	     
C        b)  Get current watch DTG.  You have a CRDATE variable 
C	     that equals CURRENTDTG (no offset value or +/- 0).
C
C            If it is the 12Z watch on 4 March 1991, NEWDTG will contain
C	     1991030412.  ISTAT will equal 0.
C
C        c)  User provides a DTG in the CRDATE variable.
C	     The variable equals 1991030407.
C
C	     ISTAT will equal 0.  NEWDTG will contain 1991030407.
C
C        d)  User provides a DTG in the CRDATE variable.
C	     The variable equals 1093035507, an invalid DTG.
C
C	     ISTAT will equal -1.  NEWDTG will contain 1093035507.
C
C        e)  User has set the CRDATE variable to CURRENTDTG+24
C
C            If it is the 12Z watch on 4 March 1991, NEWDTG will 
C	     contain 1991030512.  ISTAT will equal 0. 
C
C  ERROR CONDITIONS:
C
C     Invalid user-provided DTG or offset value. Return a negative
C       ISTAT.
C     Error opening the watch DTG file. Abort.
C
C  ADDITIONAL COMMENTS:
C
C     See OUTPUT FILES entry for note about output messages.  
C
C.........MAINTENANCE SECTION..............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    BADDTG     C*10    Asterisk-filled DTG that is returned to user if,
C			  because of an error, DTGOPS couldn't make a 
C			  valid DTG.
C    DIGIT(3)   LOGICAL One variable for each of the 3 numeric
C			  characters in the offset (see LOFFST and
C			  OFFSET). If TRUE, then the particular
C			  character is a digit. (OFFSET contains
C			  4 characters, i.e. a sign and 3 digits.)
C			  Used to validate the offset value.
C    DTGPATH    C*40    Full pathname of watch DTG file, DTG.  Returned
C			  by subroutine PATH.  	
C    ERRDTG	C*10	Error variable returned by DTGCHK.		
C    IARRAY(9)	INT	Holds date/time information returned by GMTIME.
C    ITIME	INT	Holds system time returned by TIME. 
C    LOFFST	INT	The offset to be applied to the current DTG.
C			  Provided by the user in the CRDATE variable.
C    NEWDTG     C*10	Holds CRDATE variable's DTG for call to DTGCHK.
C		          Holds today's date.  Used to build DTGOPS.
C    OFFSET     C*4     Character version of LOFFST. It will be used
C			  to validate the offset and figure out
C			  whether the offset is a 1, 2, or 3-digit
C			  number.
C    THERE      LOGICAL True if the DTG watch file, DTG, exists,
C			  otherwise false.  Returned by INQUIRE command.
C    VARNAME	C*6     Name of the CRDATE variable, i.e. CRDATE.
C    VARVAL	C*20	Input value from the CRDATE environmental
C			  variable.     	
C
C  METHOD:
C   
C    1.  If there is a CRDATE environmental variable:
C        a.  Access the value of the environment variable, CRDATE, by 
C            calling GETENV.  If it contains a non-blank value, it will
C            be equal to a DTG or "CURRENTDTG" and an optional offset.
C        b.  If a DTG:
C	        Validate the DTG.
C               If okay, set the return DTG, CDTG = user DTG and return.
C	        If not okay, set status, and return.
C            If CURRENTDTG and offset
C   	        Get offset and apply it later to the watch or system DTG
C        c.  Validate user-supplied DTG.
C    2.  If there was no CRDATE DTG, but there is a watch DTG file (NOT
C	   YET IMPLEMENTED):
C	 a.  Read the DTG out of the file.
C	 b.  Apply the offset to the watch DTG if the user provided one,
C	     using DTGMOD.
C        c.  Return
C    3.  If the only date/time info available is the system date/time:
C	 a.  Call TIME to get system seconds.
C 	 b.  Call GMTIME to get ZULU time (2-digit year).
C     	 c.  Put a DTG together from these parts.
C        d.  Apply the offset to the constructed DTG,
C            if the user provided one, using DTGMOD.
C        e.  Return.
C    
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      LOGICAL THERE, DIGIT(3)
      CHARACTER VARNAME*6, VARVAL*20, OFFSET*4, DTGPATH*40
      CHARACTER*10 NEWDTG, CDTG, ERRDTG, DTGCHK, BADDTG
C
      INTEGER TIME, ITIME, IARRAY(9), ISTAT, LOFFST, IERR, I, L	 

      DATA NEWDTG / '0000000000' /
      DATA VARNAME / 'CRDATE' /
      DATA DIGIT / 3 * .TRUE. /
C
      ISTAT = 0
      LOFFST = 0
      VARVAL = ' '

************************************************************************
*         Access the value of the environment variable, CRDATE, by
*	  calling GETENV.  If it is non-blank, it will be equal to
*	  a DTG or "CURRENTDTG" and an offset.
*	  If a DTG
*	     Validate the DTG.
*	     If OK, set the return DTG, CDTG = user DTG and return.
*	     If not OK, set status, and return.
*	  If CURRENTDTG and offset
*	     Get offset and apply it later to the system date/time.
************************************************************************

      CALL GETENV ( VARNAME, VARVAL )
      IF ( VARVAL .NE. ' ' ) THEN 
	 WRITE(0,*) 'There is a CRDATE environmental parameter. ',
     *            'Its contents = ', VARVAL, '.'               

************************************************************************
*            Find out if the user wants the operational DTG plus
*	       an offset or is providing his own DTG.
************************************************************************

	 IF ( VARVAL(1:10) .EQ. 'CURRENTDTG' ) THEN

************************************************************************
*            Operational DTG plus offset.  Validate offset.
************************************************************************
*	     If the offset (sign and offset, cols 11-14) is blank,
*	     set it to +0 and skip the validation.
************************************************************************

	    IF ( VARVAL(11:14) .EQ. '    ' ) THEN
	       VARVAL(11:14) = '+0  '
	       GO TO 50
	    END IF

************************************************************************
*            Set the logical DIGIT array variables to TRUE if the
*	     corresponding value is a digit.
************************************************************************

            DO 10 I=12,14
	       L = I - 11
	       IF ( VARVAL(I:I) .LT. '0' .OR.
     *		    VARVAL(I:I) .GT. '9' ) DIGIT(L) = .FALSE.
   10	    CONTINUE

************************************************************************
*	     We can have one digit followed by two blanks or
*		         two digits followed by one blank or
*			 all digits.
*	  
*            If the first character is not a digit, the offset
*	     is no good.
************************************************************************
		
	    IF ( .NOT. DIGIT(1) ) GO TO 20 

************************************************************************
*	     The first character is a digit. If the other two
*	     are digits, it's a good offset.
************************************************************************

	    IF ( DIGIT(2) .AND. DIGIT(3) ) GO TO 50

************************************************************************
*  	     If the first character is a digit and the other
*	     two are blanks, the offset is ok. 	       	  	
************************************************************************

	    IF ( VARVAL(13:14) .EQ. '  ' ) GO TO 50

************************************************************************
*	     If the first two characters are digits and the
*	     last is a blank, ok. Otherwise, it's a bad offset.
************************************************************************

	    IF ( DIGIT(2) .AND. (VARVAL(14:14) .EQ. ' ') ) GO TO 50
C 		  
   20       ISTAT = -2
            CDTG = BADDTG
	    GO TO 5000

************************************************************************
*		Offset is good. If its sign is okay, save it.
************************************************************************
		
   50	    OFFSET = VARVAL(11:14)
	    IF ( OFFSET(1:1) .NE. '+' .AND. OFFSET(1:1) .NE. '-' ) THEN 
	       ISTAT = -2
	       CDTG = BADDTG
	       GO TO 5000	
C
	    ELSE IF ( OFFSET(3:3) .EQ. ' ' ) THEN
	       READ ( OFFSET(1:2), '(I2)' ) LOFFST
C	    
            ELSE IF ( OFFSET(4:4) .EQ. ' ' ) THEN 
	          READ ( OFFSET(1:3), '(I3)' ) LOFFST
C
            ELSE 
         	  READ ( OFFSET(1:4), '(I4)' ) LOFFST
C
	    END IF
         ELSE
   
***********************************************************************
*           Validate user-provided DTG.         
***********************************************************************
                
	    ERRDTG = DTGCHK ( VARVAL(1:10) )
	    IF ( ERRDTG .NE. ' ' ) THEN
	       ISTAT = -1   
 	    END IF
C
	    CDTG = VARVAL(1:10)
 	    GO TO 5000
C
   	 END IF

*********************************************************************** 
*        The following END IF marks the end of processing the 
*        CRDATE value.  The next section executes if there is no
*	 CRDATE variable set or if the CRDATE variable is set to
*	 CURRENTDTG+/-nnn option. 
*********************************************************************** 

      END IF

*********************************************************************** 
*     	 Put together a DTG--
*
*        Get system time, which is in seconds and is input to
*	   GMTIME (GMT).
*        GMTIME returns 9 integer values in the following order:
*	   sec, min, hour, day, mo, last two digits of year,
*	   day of week, day of year, and daylight savings flag.
*********************************************************************** 

      CALL DATE_AND_TIME ( VALUES=IARRAY )
      WRITE (NEWDTG(1:8), '(I4.4, 2I2.2)' ) IARRAY(1), IARRAY(2),
     *                                      IARRAY(3)
C
      IF (IARRAY(4).NE.0) THEN  ! OFFSET FROM GMT, PUT NEWDTG AT GMT
        IARRAY(4)=-INT(IARRAY(4)/60)
        CALL DTGMOD(NEWDTG,IARRAY(4),NEWDTG,ISTAT)
      ENDIF
C
      IF ( IARRAY(5) .GE. 12 ) THEN
           NEWDTG(9:10) = '12'
      ELSE
           NEWDTG(9:10) = '00'
      ENDIF

***********************************************************************
*        If there is a value in the offset variable, call DTGMOD
*        to apply it to system time.
********************************************************************** 

  100 IF ( LOFFST .EQ. 0 ) THEN
	 CDTG = NEWDTG
      ELSE
	 CALL DTGMOD ( NEWDTG, LOFFST, CDTG, ISTAT )
      END IF	 
C       
 5000 IF ( VARVAL .NE. ' ' ) WRITE(0,*) 'The DTG is ', CDTG, '.'
      RETURN
C
 1    FORMAT ( A, I4, A ) 
      END
