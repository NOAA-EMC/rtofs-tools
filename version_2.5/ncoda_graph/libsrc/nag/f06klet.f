      INTEGER FUNCTION F06KLE(N,X,INCX,TOL)
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C     .. Scalar Arguments ..
      REAL                    TOL
      INTEGER                 INCX, N
C     .. Array Arguments ..
      REAL                    X(*)
C     ..
C
C  F06KLE finds the first element of the n element vector x for which
C
C     abs( x( k ) ).le.tol*max( abs( x( 1 ) ), ..., abs( x( k - 1 ) ) )
C
C  and returns the value ( k - 1 ) in the function name F06KLE. If no
C  such k exists then F06KLE is returned as n.
C
C  If tol is supplied as less than zero then the value epsmch, where
C  epsmch is the relative machine precision, is used in place of tol.
C
C
C  Nag Fortran 77 O( n ) basic linear algebra routine.
C
C  -- Written on 27-February-1986.
C     Sven Hammarling, Nag Central Office.
C
C     .. Parameters ..
      REAL                    ZERO
      PARAMETER               (ZERO=0.0E+0)
C     .. Local Scalars ..
      REAL                    TL, XMAX
      INTEGER                 IX, K
C     .. External Functions ..
      REAL                    X02AJE
      EXTERNAL                X02AJE
C     .. Intrinsic Functions ..
      INTRINSIC               ABS, MAX
C     ..
C     .. Executable Statements ..
      K = 0
      IF (N.GE.1) THEN
         IX = 1
         IF (TOL.LT.ZERO) THEN
            TL = X02AJE()
         ELSE
            TL = TOL
         END IF
         XMAX = ABS(X(IX))
C
C+       WHILE( K.LT.N )LOOP
   10    IF (K.LT.N) THEN
            IF (ABS(X(IX)).LE.TL*XMAX) GO TO 20
            XMAX = MAX(XMAX,ABS(X(IX)))
            K = K + 1
            IX = IX + INCX
            GO TO 10
         END IF
C+       END WHILE
C
      END IF
C
   20 F06KLE = K
      RETURN
C
C     End of F06KLE. ( ISRANK )
C
      END
