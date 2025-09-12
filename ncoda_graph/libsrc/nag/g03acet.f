      SUBROUTINE G03ACE(WEIGHT,N,M,X,LDX,ISX,NX,ING,NG,WT,NIG,CVM,LDCVM,
     *                  E,LDE,NCV,CVX,LDCVX,TOL,IRANKX,WK,IWK,IFAIL)
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='G03ACE')
C     .. Scalar Arguments ..
      REAL              TOL
      INTEGER           IFAIL, IRANKX, IWK, LDCVM, LDCVX, LDE, LDX, M,
     *                  N, NCV, NG, NX
      CHARACTER         WEIGHT
C     .. Array Arguments ..
      REAL              CVM(LDCVM,NX), CVX(LDCVX,NG-1), E(LDE,6),
     *                  WK(IWK), WT(*), X(LDX,M)
      INTEGER           ING(N), ISX(M), NIG(NG)
C     .. Local Scalars ..
      REAL              A, B, COND, E2, EPS, R, RDF, RN1, RN2, SCALE,
     *                  WSUM, WTOL
      INTEGER           I, IERROR, IFAULT, IRN1, IXPG, J, K, KK, MINDIM,
     *                  MINWK, MINXG, NDV, NN1, NREC, NWK, NWT, NXN
      LOGICAL           SVDX
C     .. Local Arrays ..
      REAL              WKSP1(1,1), WKSP2(1,1), WKSP3(1,1)
      CHARACTER*80      P01REC(2)
C     .. External Functions ..
      REAL              WDZF02, X02AJE
      INTEGER           F06KLE, P01ABE
      EXTERNAL          WDZF02, X02AJE, F06KLE, P01ABE
C     .. External Subroutines ..
      EXTERNAL          F01QCE, F01QDE, F02WEE, F02WUE, F06DBE, F06FBE,
     *                  F06FCE, AAZG03, ACZG03, SCOPY, SGEMV, SSCAL,
     *                  STRSV
C     .. Intrinsic Functions ..
      INTRINSIC         MAX, REAL, SQRT
C     .. Executable Statements ..
      NREC = 1
      IERROR = 1
      IXPG = NX + NG
      IF (NX.GE.NG-1) THEN
         NWK = (NX+1)*NX + (NX-1)*5
         MINXG = NG - 1
      ELSE
         NWK = (NX-1)*5 + (NG-1)*NX
         MINXG = NX
      END IF
      NN1 = N*NX
      NXN = N*NX + NX
      MINWK = MAX(N,NWK) + NN1
      IF (NX.LT.1) THEN
         WRITE (P01REC(1),FMT=99999) NX
      ELSE IF (NG.LT.2) THEN
         WRITE (P01REC(1),FMT=99998) NG
      ELSE IF (M.LT.NX) THEN
         WRITE (P01REC(1),FMT=99997) M, NX
      ELSE IF (N.LT.IXPG) THEN
         NREC = 2
         WRITE (P01REC,FMT=99996) N, IXPG
      ELSE IF (LDX.LT.N) THEN
         WRITE (P01REC(1),FMT=99995) LDX, N
      ELSE IF (LDCVX.LT.NX) THEN
         WRITE (P01REC(1),FMT=99994) LDCVX, NX
      ELSE IF (LDCVM.LT.NG) THEN
         WRITE (P01REC(1),FMT=99987) LDCVM, NG
      ELSE IF (LDE.LT.MINXG) THEN
         NREC = 2
         WRITE (P01REC,FMT=99993) LDE, MINXG
      ELSE IF (IWK.LT.MINWK) THEN
         NREC = 2
         WRITE (P01REC,FMT=99992) MINWK, IWK
      ELSE IF (WEIGHT.NE.'W' .AND. WEIGHT.NE.'w' .AND. WEIGHT.NE.
     *         'U' .AND. WEIGHT.NE.'u') THEN
         WRITE (P01REC(1),FMT=99990) WEIGHT
      ELSE IF (TOL.LT.0.0) THEN
         WRITE (P01REC(1),FMT=99984) TOL
      ELSE
         IERROR = 0
      END IF
      IF (IERROR.EQ.0) THEN
         EPS = X02AJE()
         IF (TOL.LT.EPS) THEN
            WTOL = SQRT(EPS)
         ELSE
            WTOL = TOL
         END IF
         CALL F06DBE(NG,0,NIG,1)
         DO 20 I = 1, N
            IF (ING(I).LE.0 .OR. ING(I).GT.NG) THEN
               IF (WEIGHT.EQ.'U' .OR. WEIGHT.EQ.'u') GO TO 420
               IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
                  IF (WT(I).NE.0.0) GO TO 420
               END IF
            ELSE
               NIG(ING(I)) = NIG(ING(I)) + 1
            END IF
   20    CONTINUE
         K = 0
         DO 40 I = 1, M
            IF (ISX(I).GT.0) K = K + 1
   40    CONTINUE
         IF (K.NE.NX) THEN
            IERROR = 4
            WRITE (P01REC(1),FMT=99989) K, NX
         ELSE
C
C           CHECK WEIGHTS
C
            IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
               WSUM = 0.0
               NWT = (NX-1)*N
               DO 60 I = 1, N
                  IF (WT(I).LT.0.0) GO TO 80
                  IF (WT(I).GT.0.0) THEN
                     WSUM = WSUM + WT(I)
                     WK(NWT+I) = SQRT(WT(I))
                  ELSE
                     WK(NWT+I) = 0.0
                  END IF
   60          CONTINUE
               GO TO 100
   80          IERROR = 2
               WRITE (P01REC(1),FMT=99986) I
               GO TO 440
  100          CONTINUE
               KK = (WSUM+EPS)
            ELSE
               KK = N
               WSUM = REAL(N)
            END IF
            CALL AAZG03('U',WEIGHT,N,X,LDX,M,ISX,NX,WT,WSUM,WK,N,WKSP1,
     *                  WK(NN1+1))
C
C           CALCULATE GROUP MEANS FOR QX
C
            CALL F01QCE(N,NX,WK,N,CVX,IFAULT)
            K = 0
            DO 140 I = 1, NG
               IF (NIG(I).NE.0) THEN
                  K = K + 1
                  DO 120 J = 1, N
                     IF (ING(J).EQ.I) THEN
                        WK(NN1+J) = 1.0
                     ELSE
                        WK(NN1+J) = 0.0
                     END IF
  120             CONTINUE
                  IFAULT = 1
                  CALL F01QDE('T','S',N,NX,WK,N,CVX,1,WK(NN1+1),N,WKSP1,
     *                        IFAULT)
                  CALL SCOPY(NX,WK(NN1+1),1,CVM(I,1),LDCVM)
               ELSE
                  CALL F06FBE(NX,0.0,CVM(I,1),LDCVM)
               END IF
  140       CONTINUE
            NDV = K - 1
            IF (NDV.LE.0) THEN
               IERROR = 7
               WRITE (P01REC(1),FMT=99983)
               GO TO 440
            ELSE IF (KK-(NDV+NX).LE.0) THEN
               IERROR = 7
               NREC = 2
               WRITE (P01REC,FMT=99981)
               GO TO 440
            ELSE
               RDF = SQRT(WSUM-REAL(K))
            END IF
            COND = WDZF02(NX,WK,N,WK(NN1+1))
            IF (COND*WTOL.GT.1.0) THEN
               SVDX = .TRUE.
               IFAULT = 1
               CALL F02WUE(NX,WK,N,0,WKSP1,1,.TRUE.,WK(NXN+1),NX,
     *                     WK(NN1+1),.TRUE.,WK(NXN+NX*NX+1),IFAULT)
               IF (IFAULT.GT.0) THEN
                  IERROR = 5
                  WRITE (P01REC(1),FMT=99991)
                  GO TO 440
C
               ELSE
                  IRANKX = F06KLE(NX,WK(NN1+1),1,WTOL)
                  IF (IRANKX.LE.0) THEN
                     IERROR = 8
                     WRITE (P01REC(1),FMT=99982)
                     GO TO 440
                  END IF
                  DO 160 I = 1, IRANKX
                     WK(NN1+I) = 1.0/WK(NN1+I)
  160             CONTINUE
                  DO 180 I = 1, NX
                     CALL F06FCE(IRANKX,WK(NN1+1),1,WK((I-1)*N+1),1)
  180             CONTINUE
                  DO 200 I = 1, NG
                     IF (NIG(I).NE.0) THEN
                        CALL SCOPY(NX,CVM(I,1),LDCVM,WK(NN1+1),1)
                        CALL SGEMV('T',NX,IRANKX,1.0,WK(NXN+1),NX,
     *                             WK(NN1+1),1,0.0,CVM(I,1),LDCVM)
                     END IF
  200             CONTINUE
               END IF
            ELSE
               SVDX = .FALSE.
               IRANKX = NX
            END IF
            K = 0
  220       CONTINUE
            K = K + 1
            IRN1 = NIG(K)
            IF (IRN1.EQ.0) GO TO 220
            RN1 = REAL(IRN1)
            CALL SCOPY(IRANKX,CVM(K,1),LDCVM,WK(NN1+1),1)
            KK = 0
            DO 260 I = K + 1, NG
               IF (NIG(I).NE.0) THEN
                  KK = KK + 1
                  RN2 = REAL(NIG(I))
                  R = RN1/RN2
                  A = 1.0/SQRT((1.0+R)*RN1)
                  B = -R*A
                  DO 240 J = 1, IRANKX
                     CVX(J,KK) = WK(NN1+J)*A + CVM(I,J)*B
                     WK(NN1+J) = WK(NN1+J) + CVM(I,J)
  240             CONTINUE
                  RN1 = RN1 + RN2
               END IF
  260       CONTINUE
            IFAULT = 1
            IF (IRANKX.GE.NDV) THEN
               MINDIM = NDV
               CALL F02WEE(IRANKX,NDV,CVX,LDCVX,0,WKSP1,1,.TRUE.,WKSP2,
     *                     1,E(1,1),.FALSE.,WKSP3,1,WK(NN1+1),IFAULT)
               IF (IFAULT.GT.0) THEN
                  IERROR = 5
                  WRITE (P01REC(1),FMT=99991)
                  GO TO 440
C
               END IF
            ELSE
               MINDIM = IRANKX
               DO 280 I = 1, NDV
                  CALL SCOPY(IRANKX,CVX(1,I),1,WK(NN1+(I-1)*IRANKX+1),1)
  280          CONTINUE
               CALL F02WEE(IRANKX,NDV,WK(NN1+1),IRANKX,0,WKSP1,1,.TRUE.,
     *                     CVX,LDCVX,E(1,1),.FALSE.,WKSP2,1,
     *                     WK(NN1+NDV*NX+1),IFAULT)
               IF (IFAULT.GT.0) THEN
                  IERROR = 5
                  WRITE (P01REC(1),FMT=99991)
                  GO TO 440
C
               END IF
            END IF
            NCV = F06KLE(MINDIM,E(1,1),1,WTOL)
            DO 300 I = 1, NCV
               E2 = 1.0 - E(I,1)*E(I,1)
               IF (E2.LE.0.0) GO TO 320
               SCALE = RDF/SQRT(E2)
               CALL SSCAL(IRANKX,SCALE,CVX(1,I),1)
  300       CONTINUE
            GO TO 340
  320       IERROR = 6
            WRITE (P01REC,FMT=99985)
            NCV = 1
  340       CONTINUE
            DO 360 I = 1, NG
               IF (NIG(I).NE.0) THEN
                  SCALE = 1.0/REAL(NIG(I))
                  CALL SCOPY(IRANKX,CVM(I,1),LDCVM,WK(NN1+1),1)
                  CALL SGEMV('T',IRANKX,NCV,SCALE,CVX,LDCVX,WK(NN1+1),1,
     *                       0.0,CVM(I,1),LDCVM)
               END IF
  360       CONTINUE
            IF (SVDX) THEN
               DO 380 I = 1, NCV
                  CALL SCOPY(IRANKX,CVX(1,I),1,WK(NN1+1),1)
                  CALL SGEMV('T',IRANKX,NX,1.0,WK,N,WK(NN1+1),1,0.0,
     *                       CVX(1,I),1)
  380          CONTINUE
            ELSE
               DO 400 I = 1, NCV
                  CALL STRSV('U','N','N',NX,WK,N,CVX(1,I),1)
  400          CONTINUE
            END IF
C
C           CALCULATE TEST STATISTICS
C
            CALL ACZG03(E,LDE,WSUM,NCV,IRANKX,NDV,IFAULT)
         END IF
         GO TO 440
C
  420    IERROR = 3
         WRITE (P01REC(1),FMT=99988) I
      END IF
  440 IFAIL = P01ABE(IFAIL,IERROR,SRNAME,NREC,P01REC)
C
99999 FORMAT (' ** On entry NX .lt. 1: NX = ',I16)
99998 FORMAT (' ** On entry NG .lt. 2: NG = ',I16)
99997 FORMAT (' ** On entry M .lt. NX: M = ',I16,' NX = ',I16)
99996 FORMAT (' ** On entry N .lt. NX+NG: N = ',I16,/'                ',
     *       '       NX+NG = ',I16)
99995 FORMAT (' ** On entry LDX .lt. N: LDX = ',I16,' N = ',I16)
99994 FORMAT (' ** On entry LDCVX .lt. NX: LDCVX = ',I16,' NX = ',I16)
99993 FORMAT (' ** On entry LDE .lt. MIN(NX,NG-1): LDE = ',I16,/'     ',
     *       '                      MIN(NX,NG-1) = ',I16)
99992 FORMAT (' ** On entry IWK is too small, min value = ',I16,/'    ',
     *       '                                 IWK = ',I16)
99991 FORMAT (' ** An SVD has failed to converge')
99990 FORMAT (' ** On entry WEIGHT is not valid: WEIGHT = ',A1)
99989 FORMAT (' ** On entry there are ',I16,' X vars instead of ',I16)
99988 FORMAT (' ** On entry ',I16,' th value of ING not valid')
99987 FORMAT (' ** On entry LDCVM .lt. NG: LDCVM = ',I16,' NG = ',I16)
99986 FORMAT (' ** On entry the ',I16,' th value of WT .lt. 0.0')
99985 FORMAT (' ** Canonical correlation equal to 1.0')
99984 FORMAT (' ** On entry TOL .lt. 0.0: TOL = ',E13.5)
99983 FORMAT (' ** Less than 2 groups have non-zero membership')
99982 FORMAT (' ** The rank of X is 0')
99981 FORMAT (' ** The effective number of observations is less than',/
     *     '    the effective number of groups plus number of variables'
     *       )
      END
