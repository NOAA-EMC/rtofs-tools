      SUBROUTINE X03AAE(A,ISIZEA,B,ISIZEB,N,ISTEPA,ISTEPB,C1,C2,D1,D2,
     *                  SW,IFAIL)
C     NAG COPYRIGHT 1975
C     MARK 4.5 RELEASE
C     MARK 6 REVISED
C     MARK 10 REVISED. IER-382 (JUN 1982).
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     CALCULATES THE VALUE OF A SCALAR PRODUCT USING BASIC
C     OR ADDITIONAL PRECISION AND ADDS IT TO A BASIC OR ADDITIONAL
C     PRECISION INITIAL VALUE.
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='X03AAE')
C     .. Scalar Arguments ..
      REAL              C1, C2, D1, D2
      INTEGER           IFAIL, ISIZEA, ISIZEB, ISTEPA, ISTEPB, N
      LOGICAL           SW
C     .. Array Arguments ..
      REAL              A(ISIZEA), B(ISIZEB)
C     .. Local Scalars ..
      DOUBLE PRECISION  SUM
      REAL              X
      INTEGER           I, IERR, IS, IT
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABE
      EXTERNAL          P01ABE
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, REAL
C     .. Executable Statements ..
      IERR = 0
      IF (ISTEPA.LE.0 .OR. ISTEPB.LE.0) IERR = 1
      IF (ISIZEA.LE.(N-1)*ISTEPA .OR. ISIZEB.LE.(N-1)*ISTEPB) IERR = 2
      IF (IERR.EQ.0) GO TO 20
      IFAIL = P01ABE(IFAIL,IERR,SRNAME,0,P01REC)
      RETURN
   20 IS = 1 - ISTEPA
      IT = 1 - ISTEPB
      IF (SW) GO TO 80
      X = 0.0
      IF (N.LT.1) GO TO 60
      DO 40 I = 1, N
         IS = IS + ISTEPA
         IT = IT + ISTEPB
         X = X + A(IS)*B(IT)
   40 CONTINUE
   60 D1 = X + (C1+C2)
      D2 = 0.0
      IFAIL = 0
      RETURN
   80 SUM = 0.0D0
      IF (N.LT.1) GO TO 120
      DO 100 I = 1, N
         IS = IS + ISTEPA
         IT = IT + ISTEPB
         SUM = SUM + DBLE(A(IS))*B(IT)
  100 CONTINUE
  120 SUM = SUM + (DBLE(C1)+C2)
C     D1 = SUM + SUM - DBLE(REAL(SUM))
C     THE LAST STATEMENT ASSUMES THAT THE MACHINE SIMPLY
C     TRUNCATES WHEN ASSIGNING A DOUBLE PRECISION QUANTITY
C     TO A SINGLE PRECISION VARIABLE. IF INSTEAD THE MACHINE
C     ROUNDS, REPLACE THE LAST STATEMENT BY
      D1 = SUM
      D2 = SUM - D1
      IFAIL = 0
      RETURN
      END
