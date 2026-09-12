      SUBROUTINE G02BDE(N,M,X,IX,XBAR,STD,SSPZ,ISSPZ,RZ,IRZ,IFAIL)
C     MARK 4 RELEASE NAG COPYRIGHT 1974.
C     MARK 4.5 REVISED
C     MARK 10 REVISED. IER-380 (JUN 1982).
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     NAG SUBROUTINE G02BDE
C     WRITTEN 17. 7.73 BY PAUL GRIFFITHS (OXFORD UNIVERSITY)
C
C     COMPUTES MEANS AND STANDARD DEVIATIONS OF VARIABLES SUMS OF
C     SQUARES AND CROSS-PRODUCTS ABOUT ZERO AND CORRELATION-LIKE
C     COEFFICIENTS FOR A SET OF DATA IN THE ARRAY X.
C
C     USES NAG ERROR ROUTINE P01AAE
C
C
C     ABOVE DATA STATEMENT MAY BE MACHINE-DEPENDENT -- DEPENDS ON
C     NUMBER OF CHARACTERS WHICH CAN BE STORED IN A REAL VARIABLE
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='G02BDE')
C     .. Scalar Arguments ..
      INTEGER           IFAIL, IRZ, ISSPZ, IX, M, N
C     .. Array Arguments ..
      REAL              RZ(IRZ,M), SSPZ(ISSPZ,M), STD(M), X(IX,M),
     *                  XBAR(M)
C     .. Local Scalars ..
      REAL              FN, S, XM, XN
      INTEGER           I, IERROR, J, JM, K
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABE
      EXTERNAL          P01ABE
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT, REAL
C     .. Executable Statements ..
      IERROR = 0
      IF (IX.LT.N .OR. ISSPZ.LT.M .OR. IRZ.LT.M) IERROR = 3
      IF (M.LT.2) IERROR = 2
      IF (N.LT.2) IERROR = 1
      IF (IERROR) 20, 20, 260
   20 FN = REAL(N)
      DO 240 J = 1, M
         S = 0.0
         DO 40 I = 1, N
            S = S + X(I,J)
   40    CONTINUE
         XM = S/FN
         XBAR(J) = XM
         S = 0.0
         DO 60 I = 1, N
            XN = X(I,J) - XM
            S = S + XN*XN
   60    CONTINUE
         SSPZ(J,J) = S + XM*XM*FN
         IF (S) 80, 80, 100
   80    STD(J) = 0.0
         RZ(J,J) = 0.0
         IF (SSPZ(J,J).GT.0.0) RZ(J,J) = 1.0
         GO TO 120
  100    STD(J) = SQRT(S/(FN-1.0))
         RZ(J,J) = 1.0
  120    IF (J.EQ.1) GO TO 240
         JM = J - 1
         DO 220 K = 1, JM
            S = 0.0
            DO 140 I = 1, N
               S = S + X(I,J)*X(I,K)
  140       CONTINUE
            SSPZ(J,K) = S
            SSPZ(K,J) = S
            S = SSPZ(J,J)*SSPZ(K,K)
            IF (S) 160, 160, 180
  160       RZ(J,K) = 0.0
            GO TO 200
  180       RZ(J,K) = SSPZ(J,K)/SQRT(S)
  200       RZ(K,J) = RZ(J,K)
  220    CONTINUE
  240 CONTINUE
      IFAIL = 0
      RETURN
  260 IFAIL = P01ABE(IFAIL,IERROR,SRNAME,0,P01REC)
      RETURN
      END
