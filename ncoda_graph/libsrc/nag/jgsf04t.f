      REAL FUNCTION JGSF04(N,A,NRA)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (UPPNRM)
C
C     JGSF04 RETURNS THE EUCLIDEAN NORM OF THE N*N UPPER TRIANGULAR
C     MATRIX A.
C
C     NRA MUST BE THE ACTUAL ROW DIMENSION OF A AS DECLARED IN THE
C     CALLING PROGRAM AND MUST BE AT LEAST N.
C
C     ONLY THE UPPER TRIANGULAR PART OF A IS REFERENCED.
C
C     .. Scalar Arguments ..
      INTEGER              N, NRA
C     .. Array Arguments ..
      REAL                 A(NRA,N)
C     .. Local Scalars ..
      REAL                 BIG, SCALE, SMALL, SUMSQ, TINY
      INTEGER              J
      LOGICAL              UNDFLW
C     .. External Functions ..
      REAL                 JGUF04, X02AME
      LOGICAL              X02DAE
      EXTERNAL             JGUF04, X02AME, X02DAE
C     .. External Subroutines ..
      EXTERNAL             JGTF04
C     .. Intrinsic Functions ..
      INTRINSIC            SQRT
C     .. Executable Statements ..
      SMALL = X02AME()
      TINY = SQRT(SMALL)
      BIG = 1.0/SMALL
      UNDFLW = X02DAE(0.0)
C
      SCALE = 0.0
      SUMSQ = 1.0
C
      DO 20 J = 1, N
C
         CALL JGTF04(J,A(1,J),SCALE,SUMSQ,TINY,UNDFLW)
C
   20 CONTINUE
C
      JGSF04 = JGUF04(SCALE,SUMSQ,BIG)
C
      RETURN
      END
