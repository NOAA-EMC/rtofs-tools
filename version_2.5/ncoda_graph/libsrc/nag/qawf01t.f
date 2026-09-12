      SUBROUTINE QAWF01(N,X,XMUL,Y,UNDFLW)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (ROWOP2)
C
C     QAWF01 RETURNS THE N ELEMENT VECTOR Z GIVEN BY
C
C     Z = X - XMUL*Y ,
C
C     WHERE XMUL IS A REAL VALUE AND X AND Y ARE N ELEMENT
C     VECTORS.
C
C     Z IS OVERWRITTEN ON X.
C
C     N MUST BE AT LEAST 1.
C
C     UNDFLW MUST BE THE VALUE RETURNED BY X02DAE
C
C     .. Scalar Arguments ..
      REAL              XMUL
      INTEGER           N
      LOGICAL           UNDFLW
C     .. Array Arguments ..
      REAL              X(N), Y(N)
C     .. Local Scalars ..
      REAL              AMUL, W
      INTEGER           I
C     .. External Functions ..
      REAL              X02AME
      EXTERNAL          X02AME
C     .. Intrinsic Functions ..
      INTRINSIC         ABS
C     .. Executable Statements ..
      IF (XMUL.EQ.0.0) RETURN
C
      IF (UNDFLW) GO TO 60
C
   20 DO 40 I = 1, N
         X(I) = X(I) - XMUL*Y(I)
   40 CONTINUE
C
      RETURN
C
   60 AMUL = ABS(XMUL)
      IF (AMUL.GE.1.0) GO TO 20
      W = X02AME()/AMUL
C
      DO 80 I = 1, N
         IF (ABS(Y(I)).LT.W) GO TO 80
         X(I) = X(I) - XMUL*Y(I)
   80 CONTINUE
C
      RETURN
      END
