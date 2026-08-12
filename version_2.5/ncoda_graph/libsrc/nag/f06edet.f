      SUBROUTINE F06EDE(N,ALPHA,X,INCX)
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C     .. Entry Points ..
      ENTRY             SSCAL(N,ALPHA,X,INCX)
C     .. Scalar Arguments ..
      REAL              ALPHA
      INTEGER           INCX, N
C     .. Array Arguments ..
      REAL              X(*)
C     ..
C
C  F06EDE performs the operation
C
C     x := alpha*x
C
C
C  Nag Fortran 77 version of the Blas routine SSCAL.
C  Nag Fortran 77 O( n ) basic linear algebra routine.
C
C  -- Written on 26-November-1982.
C     Sven Hammarling, Nag Central Office.
C
C
C     .. Parameters ..
      REAL              ONE, ZERO
      PARAMETER         (ONE=1.0E+0,ZERO=0.0E+0)
C     .. Local Scalars ..
      INTEGER           IX
C     ..
C     .. Executable Statements ..
      IF (N.GT.0) THEN
         IF (ALPHA.EQ.ZERO) THEN
            DO 10 IX = 1, 1 + (N-1)*INCX, INCX
               X(IX) = ZERO
   10       CONTINUE
         ELSE IF (ALPHA.EQ.(-ONE)) THEN
            DO 20 IX = 1, 1 + (N-1)*INCX, INCX
               X(IX) = -X(IX)
   20       CONTINUE
         ELSE IF (ALPHA.NE.ONE) THEN
            DO 30 IX = 1, 1 + (N-1)*INCX, INCX
               X(IX) = ALPHA*X(IX)
   30       CONTINUE
         END IF
      END IF
C
      RETURN
C
C     End of F06EDE. ( SSCAL )
C
      END
