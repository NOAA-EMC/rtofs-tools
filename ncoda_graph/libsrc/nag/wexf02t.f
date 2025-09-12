      SUBROUTINE WEXF02(NX,NY,ALPHA,X,INCX,Y,INCY,TOL,ZETA)
C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
C     MARK 14A REVISED. IER-687 (DEC 1989).
C
C  WEXF02  generates  details  of  a  Householder reflection  such  that
C
C     P*(   x   ) = (   0  ),   P'*P = I.
C       ( alpha )   ( beta ),
C       (   y   )   (   0  )
C
C  P is given in the form
C
C     P = I - (   w  )*( w'  zeta  z' ),
C             ( zeta )
C             (   z  )
C
C  where  w is an nx element vector, z is an ny element vector, and zeta
C  is a scalar that satisfies
C
C     1.0 .le. zeta .le. sqrt( 2.0 ).
C
C  zeta is returned in ZETA unless the vector v given by
C
C     v = ( x )
C         ( y )
C
C  is such that
C
C     max( abs( v( i ) ) ) .le. max( tol, eps*abs( alpha ) ),
C
C  where  eps  is the  relative machine precision  and  tol  is the user
C  supplied tolerance  TOL, in which case  ZETA  is returned as  0.0 and
C  P  can be taken to be the unit matrix.
C
C  beta  is  overwritten on  alpha,  w  is overwritten on  x  and  z  is
C  overwritten on  y.
C
C  The  routine  may be  called  with  either  or  both  nx = 0, ny = 0.
C
C
C  Nag Fortran 77 O( n ) basic linear algebra routine.
C
C  -- Written on 17-November-1987.
C     Sven Hammarling, Nag Central Office.
C
C
C     .. Parameters ..
      REAL              ONE, ZERO
      PARAMETER         (ONE=1.0E+0,ZERO=0.0E+0)
C     .. Scalar Arguments ..
      REAL              ALPHA, TOL, ZETA
      INTEGER           INCX, INCY, NX, NY
C     .. Array Arguments ..
      REAL              X(*), Y(*)
C     .. Local Scalars ..
      REAL              BETA, EPS, SCALE, SSQ
      LOGICAL           FIRST
C     .. External Functions ..
      REAL              X02AJE
      EXTERNAL          X02AJE
C     .. External Subroutines ..
      EXTERNAL          F06FJE, F06FRE, SSCAL
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, SQRT
C     .. Save statement ..
      SAVE              EPS, FIRST
C     .. Data statements ..
      DATA              FIRST/.TRUE./
C     .. Executable Statements ..
      IF (NX.LT.1) THEN
         CALL F06FRE(NY,ALPHA,Y,INCY,TOL,ZETA)
      ELSE IF (NY.LT.1) THEN
         CALL F06FRE(NX,ALPHA,X,INCX,TOL,ZETA)
      ELSE
C
         IF (FIRST) THEN
            FIRST = .FALSE.
            EPS = X02AJE()
         END IF
C
         SSQ = ONE
         SCALE = ZERO
         CALL F06FJE(NX,X,INCX,SCALE,SSQ)
         CALL F06FJE(NY,Y,INCY,SCALE,SSQ)
C
C        Treat  cases  where   SCALE = zero,   SCALE is negligible   and
C        ALPHA = zero  specially.  Note that
C
C           SCALE = max( abs( v( i ) ) ).
C
         IF ((SCALE.EQ.ZERO) .OR. (SCALE.LE.MAX(TOL,EPS*ABS(ALPHA))))
     *       THEN
            ZETA = ZERO
         ELSE IF (ALPHA.EQ.ZERO) THEN
            ZETA = ONE
            ALPHA = SCALE*SQRT(SSQ)
            CALL SSCAL(NX,-1/ALPHA,X,INCX)
            CALL SSCAL(NY,-1/ALPHA,Y,INCY)
         ELSE
            IF (SCALE.LT.ABS(ALPHA)) THEN
               BETA = ABS(ALPHA)*SQRT(1+SSQ*(SCALE/ALPHA)**2)
            ELSE
               BETA = SCALE*SQRT(SSQ+(ALPHA/SCALE)**2)
            END IF
            ZETA = SQRT((BETA+ABS(ALPHA))/BETA)
            IF (ALPHA.GT.ZERO) BETA = -BETA
            CALL SSCAL(NX,-1/(ZETA*BETA),X,INCX)
            CALL SSCAL(NY,-1/(ZETA*BETA),Y,INCY)
            ALPHA = BETA
         END IF
      END IF
C
      RETURN
C
C     End of WEXF02. ( SGRFG2 )
C
      END
