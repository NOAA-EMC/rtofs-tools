      REAL FUNCTION F06EAE(N,X,INCX,Y,INCY)
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C     .. Entry Points ..
      REAL                 SDOT
      ENTRY                SDOT(N,X,INCX,Y,INCY)
C     .. Scalar Arguments ..
      INTEGER              INCX, INCY, N
C     .. Array Arguments ..
      REAL                 X(*), Y(*)
C     ..
C
C  F06EAE returns the value
C
C     F06EAE = x'y
C
C
C  Nag Fortran 77 version of the Blas routine SDOT.
C  Nag Fortran 77 O( n ) basic linear algebra routine.
C
C  -- Written on 21-September-1982.
C     Sven Hammarling, Nag Central Office.
C
C
C     .. Parameters ..
      REAL                 ZERO
      PARAMETER            (ZERO=0.0E+0)
C     .. Local Scalars ..
      REAL                 SUM
      INTEGER              I, IX, IY
C     ..
C     .. Executable Statements ..
      SUM = ZERO
      IF (N.GT.0) THEN
         IF ((INCX.EQ.INCY) .AND. (INCX.GT.0)) THEN
            DO 10 IX = 1, 1 + (N-1)*INCX, INCX
               SUM = SUM + X(IX)*Y(IX)
   10       CONTINUE
         ELSE
            IF (INCY.GE.0) THEN
               IY = 1
            ELSE
               IY = 1 - (N-1)*INCY
            END IF
            IF (INCX.GT.0) THEN
               DO 20 IX = 1, 1 + (N-1)*INCX, INCX
                  SUM = SUM + X(IX)*Y(IY)
                  IY = IY + INCY
   20          CONTINUE
            ELSE
               IX = 1 - (N-1)*INCX
               DO 30 I = 1, N
                  SUM = SUM + X(IX)*Y(IY)
                  IX = IX + INCX
                  IY = IY + INCY
   30          CONTINUE
            END IF
         END IF
      END IF
C
      F06EAE = SUM
      RETURN
C
C     End of F06EAE. ( SDOT )
C
      END
