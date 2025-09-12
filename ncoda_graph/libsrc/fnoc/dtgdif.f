      SUBROUTINE DTGDIF ( NDTG, MDTG, IHRS, ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGDIF
C
C  DESCRIPTION:
C
C     Given two DTGs, return the difference in hours (=MDTG-NDTG).
C
C  USAGE (CALLING SEQUENCE):
C
C     CALL DTGDIF ( NDTG, MDTG, IHRS, ISTAT )
C
C  INPUT PARAMETERS:
C
C     NDTG	C*10	A DTG of format YYYYMMDDHH.
C     MDTG	C*10    A DTG of format YYYYMMDDHH.
C
C     Note that NDTG can be greater than MDTG. The result will be a
C     negative difference.
C
C  OUTPUT PARAMETERS:
C
C     IHRS	INT	Difference in hours (MDTG-NDTG)
C     ISTAT	INT	Status variable.
C			  ISTAT =  0, ok
C			  ISTAT = -1, invalid DTG.
C
C  CALLS:
C
C     DTGNUM		Get integer year, month, day, hour, hours
C			  into the year, days into the year from DTG.
C
C  RESTRICTIONS:
C
C     DTGDIF handles DTGs in the range 1800 through 2799.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C     IDIFF		The difference between the two DTG's years.
C     IDUM		A dummy variable to use for the call to
C			  DTGNUM.  We don't need most of the
C			  values returned by DTGNUM.
C     IHOURS(4)		Total hours in the year.  IHOURS(1) is for
C			  leap years, and the other three are for
C			  non-leap years.
C     IHRYRA/B		Number of hours in each DTG's year. They start 
C		  	  out containing the values of MDTG and NDTG,
C			  respectively, but the values may be swapped
C			  later on.	
C     ITEMP		Temporary variable used for swapping the 
C			  IYEARA/B and IHRYRA/B values.     
C     IYEARA/B		Integer year for each DTG. They start out
C		  	  containing the values of MDTG and NDTG,
C			  respectively, but the values may be swapped
C			  later on.
C     JSTAT	  	Error return variable from DTGNUM. If JSTAT =
C			  =0, ok.
C     LEAPCT		Counts the number of leap years between IYEARA
C			  and IYEARB.
C     LEAPYR		A logical variable that is true if the year is
C			  a leap year.		
C     SWAP        	A logical variable that is .TRUE. if the
C			  values of the IHRYR and IYEAR variables
C			  are swapped. 
C			
C  METHOD:
C
C     1.  Call DTGNUM to get the integer years and hours into the
C	  year for both input DTGs.
C     2.  If the two years are different:
C	  a.  If NDTG < MDTG
C	      1)  Set swap flag on
C	      2)  Put MDTG's value in NDTG and vice versa.  			
C         b.  END IF
C         c.  Figure out whether this is a leap year or not and
C	      set IDX, the leap year subscript.
C	  d.  Add the remaining hours of MDTG to the hours of NDTG
C             and put the result in IHRS.
C	  e.  If there is more than one year between the two DTGs,
C	      add in those years' hours.		
C	  f.  If the two values were swapped, change the sign of
C	      IHRS.
C     3.  If the two years are the same, use the difference between
C	  their hours-of-the-year.
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER*10 NDTG, MDTG      
C
      INTEGER IDIFF, IHOURS(4), IHRYRA, IHRYRB, IYEARA, 
     *        IYEARB, LEAPCT, ITEMP, IDX, IREM, I, J, K, MOD,
     *        IDUM1, IDUM2, IDUM3, IDUM4, IHRS, ISTAT, JSTAT
C
      LOGICAL SWAP, LEAPYR
C
      DATA IHOURS / 8784, 3*8760 /
C
      ISTAT = 0
      SWAP = .FALSE.
C
      CALL DTGNUM ( MDTG, IYEARA, IDUM1, IDUM2, IDUM3, IDUM4, IHRYRA,
     *              JSTAT )
      IF ( JSTAT .NE. 0 ) THEN
	 ISTAT = -1
         GO TO 5000
      END IF
C 
      CALL DTGNUM ( NDTG, IYEARB, IDUM1, IDUM2, IDUM3, IDUM4, IHRYRB,
     *              JSTAT ) 
      IF ( JSTAT .NE. 0 ) THEN
	 ISTAT = -1
         GO TO 5000
      END IF
C
      IF ( IYEARA .NE. IYEARB ) THEN
         IF ( IYEARA .LT. IYEARB ) THEN

**********************************************************************
*        The years are unequal.
*        Year A is less than year B.  Swap values so year A is
*          greater than year B.  Then use the "greater than"
*          section to do the calculation and change the sign of
*          the output hours.
**********************************************************************

            SWAP = .TRUE.
            ITEMP  = IYEARA
	    IYEARA = IYEARB
  	    IYEARB = ITEMP
c	 
            ITEMP  = IHRYRA
    	    IHRYRA = IHRYRB
	    IHRYRB = ITEMP
         END IF

**********************************************************************
*		Section that calulates the difference for
*		  years that are not the same.
*		Set leap year subscript. (If a century is not a leap
*		  year, we set it unequal to 1, arbitrarily choosing
*		  2.)  
* 		Get the hours remaining in year B by subtracting
*		  its hours-into-the-year from the total hours 
*		  in the year.
*	        Add them to the other year's hours, then change 
*	          the sign of the result if necessary.    
**********************************************************************

	 IDX = MOD (IYEARB, 4) + 1

	 IF ( MOD ( IYEARB, 100 ) . EQ. 0 ) THEN
	    IF ( MOD ( IYEARB, 400 ) .NE. 0 ) THEN
	       IDX = 2
 	    END IF
   	 ELSEIF (MOD ( IYEARB, 4 ) .EQ. 0 ) THEN
	    IDX = 1
	 END IF
C
         IREM = IHOURS(IDX) - IHRYRB
         IHRS = IREM + IHRYRA

**********************************************************************
*           The above logic will handle dates from YY010100 through
*	      123123 of the next year.  The next section handles cases
*             where the time difference is greater than that.
**********************************************************************

         IDIFF = IYEARA - IYEARB
         IF ( IDIFF .GT. 1 ) THEN
	    
**********************************************************************
*	    Count the leap years in the set of years between the two
*	    given years.
**********************************************************************
            
            LEAPCT = 0
	    J = IYEARB
	    K = IYEARA
C 		
	    DO 10 I=J+1,K-1
               LEAPYR = .FALSE.
	       IF ( MOD ( I, 100 ) . EQ. 0 ) THEN
	          IF ( MOD ( I, 400 ) .EQ. 0 ) THEN
	             LEAPYR = .TRUE.
 	          END IF
	       ELSEIF (MOD ( I, 4 ) .EQ. 0 ) THEN
	          LEAPYR = .TRUE.
	       END IF
C
	       IF ( LEAPYR ) LEAPCT = LEAPCT + 1
   10 	    CONTINUE

**********************************************************************
*	    Add the additional years' hours to the total hours (IHRS).
*	    First, add leap hours in one year (IHOURS(1)) multiplied
*	      by the number of leap years (LEAPCT).
*	    Then, add non-leap hours in one year (IHOURS(2)) multiplied
*	      by the number of years between the given years 
*	      (IDIFF-1) less the number of leap years (LEAPCT). 	
**********************************************************************
            
            IHRS = IHRS + (IHOURS(1) * LEAPCT)
	    IHRS = IHRS + (IHOURS(2) * (IDIFF-1 - LEAPCT))
	 END IF
C		  	  	   
         IF ( SWAP ) IHRS = -IHRS

      ELSE
**********************************************************************
*		Section that calculates the difference when the
*		two years are the same.
**********************************************************************

         IHRS = IHRYRA - IHRYRB
      END IF
C
 5000 RETURN
      END      
