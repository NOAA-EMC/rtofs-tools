      REAL FUNCTION BAYS14(ETA,A,EPS)
C     MARK 14 RELEASE. NAG COPYRIGHT 1989.
C     .. Parameters ..
      REAL                 ONE
      PARAMETER            (ONE=1.0)
      INTEGER              NTERMS
      PARAMETER            (NTERMS=26)
C     .. Scalar Arguments ..
      REAL                 A, EPS, ETA
C     .. Local Scalars ..
      REAL                 S, T, Y
      INTEGER              I, M
C     .. Local Arrays ..
      REAL                 BM(0:NTERMS-1), FM(0:NTERMS)
C     .. Intrinsic Functions ..
      INTRINSIC            ABS
C     .. Data statements ..
      DATA                 (FM(I),I=0,18)/1.0000000000000000000E+00,
     *                     -3.3333333333333333333E-01,
     *                     8.3333333333333333333E-02,
     *                     -1.4814814814814814815E-02,
     *                     1.1574074074074074074E-03,
     *                     3.5273368606701940035E-04,
     *                     -1.7875514403292181070E-04,
     *                     3.9192631785224377817E-05,
     *                     -2.1854485106799921615E-06,
     *                     -1.8540622107151599607E-06,
     *                     8.2967113409530860050E-07,
     *                     -1.7665952736826079304E-07,
     *                     6.7078535434014985804E-09,
     *                     1.0261809784240308043E-08,
     *                     -4.3820360184533531866E-09,
     *                     9.1476995822367902342E-10,
     *                     -2.5514193994946249767E-11,
     *                     -5.8307721325504250675E-11,
     *                     2.4361948020667416244E-11/
      DATA                 (FM(I),I=19,26)/-5.0276692801141755891E-12,
     *                     1.1004392031956134771E-13,
     *                     3.3717632624009853788E-13,
     *                     -1.3923887224181620659E-13,
     *                     2.8534893807047443204E-14,
     *                     -5.1391118342425726190E-16,
     *                     -1.9752288294349442835E-15,
     *                     8.0995211567045613341E-16/
C     .. Executable Statements ..
C     When A .ge. 20.0, NTERMS = 26 is sufficient for approximately
C     18 decimal place accuracy.
      BM(NTERMS-1) = FM(NTERMS)
      BM(NTERMS-2) = FM(NTERMS-1)
      DO 20 M = NTERMS - 1, 2, -1
         BM(M-2) = FM(M-1) + M*BM(M)/A
   20 CONTINUE
C
      S = BM(0)
      Y = ETA
      M = 1
C
   40 T = BM(M)*Y
      S = S + T
      M = M + 1
      Y = Y*ETA
      IF (ABS(T/S).GE.EPS .AND. M.LT.NTERMS) GO TO 40
C
      BAYS14 = S/(ONE+BM(1)/A)
      RETURN
      END
