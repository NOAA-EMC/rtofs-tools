      REAL FUNCTION F06BLE(A,B,FAIL)
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C     .. Scalar Arguments ..
      REAL                 A, B
      LOGICAL              FAIL
C     ..
C
C  F06BLE returns the value div given by
C
C     div = ( a/b                 if a/b does not overflow,
C           (
C           ( 0.0                 if a .eq. 0.0,
C           (
C           ( sign( a/b )*flmax   if a .ne. 0.0  and a/b would overflow,
C
C  where  flmax  is a large value, via the function name. In addition if
C  a/b would overflow then  fail is returned as true, otherwise  fail is
C  returned as false.
C
C  Note that when  a and b  are both zero, fail is returned as true, but
C  div  is returned as  0.0. In all other cases of overflow  div is such
C  that  abs( div ) = flmax.
C
C  When  b = 0  then  sign( a/b )  is taken as  sign( a ).
C
C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
C
C  -- Written on 26-October-1982.
C     Sven Hammarling, Nag Central Office.
C
C
C     .. Parameters ..
      REAL                 ONE, ZERO
      PARAMETER            (ONE=1.0E+0,ZERO=0.0E+0)
C     .. Local Scalars ..
      REAL                 ABSB, DIV, FLMAX, FLMIN
      LOGICAL              FIRST
C     .. External Functions ..
      REAL                 X02AME
      EXTERNAL             X02AME
C     .. Intrinsic Functions ..
      INTRINSIC            ABS, SIGN
C     .. Save statement ..
      SAVE                 FIRST, FLMIN, FLMAX
C     .. Data statements ..
      DATA                 FIRST/.TRUE./
C     ..
C     .. Executable Statements ..
      IF (A.EQ.ZERO) THEN
         DIV = ZERO
         IF (B.EQ.ZERO) THEN
            FAIL = .TRUE.
         ELSE
            FAIL = .FALSE.
         END IF
      ELSE
C
         IF (FIRST) THEN
            FIRST = .FALSE.
            FLMIN = X02AME()
            FLMAX = 1/FLMIN
         END IF
C
         IF (B.EQ.ZERO) THEN
            DIV = SIGN(FLMAX,A)
            FAIL = .TRUE.
         ELSE
            ABSB = ABS(B)
            IF (ABSB.GE.ONE) THEN
               FAIL = .FALSE.
               IF (ABS(A).GE.ABSB*FLMIN) THEN
                  DIV = A/B
               ELSE
                  DIV = ZERO
               END IF
            ELSE
               IF (ABS(A).LE.ABSB*FLMAX) THEN
                  FAIL = .FALSE.
                  DIV = A/B
               ELSE
                  FAIL = .TRUE.
                  DIV = FLMAX
                  IF (((A.LT.ZERO) .AND. (B.GT.ZERO)) .OR. ((A.GT.ZERO)
     *                 .AND. (B.LT.ZERO))) DIV = -DIV
               END IF
            END IF
         END IF
      END IF
C
      F06BLE = DIV
      RETURN
C
C     End of F06BLE. ( SDIV )
C
      END
