      REAL FUNCTION BAZS14(A,X,GETP,EPS,UNDFL)
C     MARK 14 RELEASE. NAG COPYRIGHT 1989.
C     .. Parameters ..
      REAL                 TWO, ONE, HALF, ZERO
      PARAMETER            (TWO=2.0E0,ONE=1.0E0,HALF=0.5E0,ZERO=0.0E0)
      REAL                 RT2PI
      PARAMETER            (RT2PI=2.5066282746310005024E0)
C     .. Scalar Arguments ..
      REAL                 A, EPS, UNDFL, X
      LOGICAL              GETP
C     .. Local Scalars ..
      REAL                 DIF, ETA, U, V, Y
      INTEGER              IFAIL, S
C     .. External Functions ..
      REAL                 BAXS14, BAYS14, S15ADE
      EXTERNAL             BAXS14, BAYS14, S15ADE
C     .. Intrinsic Functions ..
      INTRINSIC            EXP, SQRT
C     .. Executable Statements ..
C
      IF (GETP) THEN
         S = -ONE
      ELSE
         S = ONE
      END IF
      DIF = (X-A)/A
      Y = A*BAXS14(DIF)
      IF (Y.LT.ZERO) Y = ZERO
      ETA = SQRT(TWO*Y/A)
      V = SQRT(Y)
      IF (X.LT.A) THEN
         ETA = -ETA
         V = -V
      END IF
      IFAIL = 0
      U = HALF*S15ADE(S*V,IFAIL)
      IF (-Y.GE.UNDFL) THEN
         V = S*EXP(-Y)*BAYS14(ETA,A,EPS)/(RT2PI*SQRT(A))
      ELSE
C        exp(-Y) underflows.
         V = ZERO
      END IF
      BAZS14 = U + V
      RETURN
      END
