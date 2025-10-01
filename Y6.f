c*****************************************************************************
c
c      Manifold junction routine
c
c*****************************************************************************
c
       SUBROUTINE MJZYXX(RW,IW,NRK2,IERR)
c
c      Finds junction states for RK2 step NRK2 using data in the manifold
c      work array RW. The angle index must be in IW(IoI) at call.
c------------------------------------------------------------------------------
c      Duct 1 state 2 is the nominal input, duct 2 state 3 the nominal output.
c
c      Input:
c        RW    real work array for the manifold
c        I     angle index
c
c      Output:
c        IERR  0 if ok, otherwise >0.
c-----------------------------------------------------------------------------
c      Data in IW at call:
c        I         current angle index at which Y and Z are known
c
c      Data in RW at call:
c        Y(I,2)    entropy parameter P/rho^k at the nominal inlet
c        Y(I,3)    entropy parameter P/rho^k at the nominal outlet
c        Z(I,2)    acoustic parameter Z+ = 2c/(k-1) + V at the nominal inlet
c        Z(I,3)    acoustic parameter Z- = 2c/(k-1) - V at the nominal outlet
c        AD(1)     flow area of duct 1
c        AD(2)     flow area of duct 2
c        AV(2)     restricted area at state 2
c        AV(3)     restricted area at state 3
c
c      Input/output data for junction in RW at call and return
c        P0J(I)    stagnation pressure (trial at call)
c        YJ(I)     stagnation entropy parameter P/rho^k (trial at call)
c
c      Data in RW on return (averaged with previous cycles)
c        C(I,2)    sound speed at state 2
c        C(I,3)    sound speed at state 3
c        P(I,2)    pressure at state 2
c        P(I,3)    pressure at state 3
c        V(I,2)    velocity at state 2
c        V(I,3)    velocity at state 3
c        Y(I,2)    entropy parameter P/rho^k at the nominal inlet
c        Y(I,3)    entropy parameter P/rho^k at the nominal outlet
c-----------------------------------------------------------------------------
       DIMENSION   CD(2),PD(2),VD(2),YD(2)
       LOGICAL     BEST
c-----------------------------------------------------------------------------
       PARAMETER           (NMPTR = 196)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 2904)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoKM1 ,MPTR(2))
       EQUIVALENCE (IoRK  ,MPTR(11))
       EQUIVALENCE (IoRKM1,MPTR(13))
       EQUIVALENCE (IoAMR ,MPTR(40))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoY   ,MPTR(46))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoZ   ,MPTR(49))
       EQUIVALENCE (IoP0J ,MPTR(50))
       EQUIVALENCE (IoYJ  ,MPTR(51))
       EQUIVALENCE (IoI   ,MPTR(91))
c------------------------------------------------------------------------------
c    diagnostic monitor
       LOGICAL OUT
       COMMON /MONMXX/IUMONM,MONM,OUT
c------------------------------------------------------------------------------
c    error controls
       DATA    KTRMAX,ERROK /20,1E-4/
c-----------------------------------------------------------------------------
c    get angle index
       I = IW(IoI)
       IM1 = IM1XX(I)
c    set indices
       IX2 = I + 720*2
       IX3 = I + 720*3
       IM1 = IM1XX(I)
c    set first trial
       PJ = RW(IoP0J+IM1)
       YJ = RW(IoYJ+IM1)
c    set error maxima
       AX = MAX(RW(IoAMR+2),RW(IoAMR+3))
       CX = CPYXX(RW,PJ,YJ)
       RHOX = RCYXX(RW,CX,YJ)
       EMBOK = ERROK*AX*RHOX*CX
       EEBOK = EMBOK*CX*RW(IoRKM1)
       EMBNR2 = 10*EMBOK
       XOK = ERROK
c
c    restart point
2      KTR = 0
       X = 1
       XBEST = 1/ERROK
       EMBEST = EMBOK*XOK
       EEBEST = EEBOK*XOK
       BEST = .FALSE.
       KTRBST = 0
       IF (OUT) WRITE (IUMONM,6) I,EMBOK,EEBOK,XOK
6      FORMAT (/' MJPYXX Call for I =',I4/
     ;          ' EMBOK,EEBOK,XOK',3E14.6)
c    set first trial as previous junction state
c
c ** loop point
c
10     KTR = KTR + 1
       IF (OUT) WRITE (IUMONM,11) KTR,KTRBST,PJ,YJ
11     FORMAT (/' JERRXX try',I3,'; Best try ',I2,';  PJ,YJ',2(1PE14.6))
       CALL JERRXX(RW,IW,NRK2,PJ,YJ,
     ;             CD,PD,VD,YD,EMB,EEB,FTMAX,ETMAX,IERR)
       IF (IERR.NE.0)  RETURN
       IF (OUT) WRITE (IUMONM,12) KTR,YD,EMB,EEB,VD,CD
12     FORMAT (' J',I2,': YD,EMB,EEB/VD,CD',4(1PE14.6)/4E14.6)
c
c    check for null flow
       IF ((VD(1).EQ.0).AND.(VD(2).EQ.0)) THEN
           IF ((ABS(EMB).LT.EMBOK).AND.(ABS(EEB).LT.EEBOK)) GOTO 70
           ENDIF
c
c    check for rerun of accepted best case
       IF (BEST) GOTO 70
c
c    check convergence
       IF ((ABS(EMB).LT.EMBOK).AND.(ABS(EEB).LE.EEBOK)) GOTO 70
c
c    check iteration count
       IF (KTR.GT.KTRMAX) THEN
           IF (OUT) WRITE (IUMONM,18)
18         FORMAT (' Not convergent')
           IERRX = 3
           GOTO 90
           ENDIF
c
c    PJ perturbation at fixed YJ (in direction to improve mass balance)
       DPJ = 0.01*PJ
       IF (EMB.GT.0) DPJ = - DPJ
       PJP = PJ + DPJ
       IF (OUT) WRITE (IUMONM,22) KTR,PJP,YJ
22     FORMAT (' PJ pert',I3,' PJP,YJ',2(1PE14.6))
       CALL JERRXX(RW,IW,NRK2,PJP,YJ,
     ;             CD,PD,VD,YD,EMBP,EEBP,FTMAXP,ETMAXP,IERR)
       IF (IERR.NE.0)  RETURN
       IF (OUT) WRITE (IUMONM,24) YD,EMBP,EEBP,VD,CD
24     FORMAT (' PJ: YD,EMBP,EEBP/VD,CD',4(1PE14.6)/4E14.6)
c
c    NR derivatives
       DEMDPJ = (EMBP - EMB)/DPJ
       DEEDPJ = (EEBP - EEB)/DPJ
       IF (OUT) WRITE (IUMONM,26) DEMDPJ,DEEDPJ
26     FORMAT (' DEMDPJ,DEEDPJ',2(1PE14.6))
c
c    use NR on PJ until mass approximately balances
       IF (ABS(EMB).GT.EMBNR2) GOTO 40
c
c    YJ perturbation at fixed PJ (in direction to improve energy balance)
       DYJ = 0.01*YJ
       IF (EEB.GT.0) DYJ = - DYJ
       YJP = YJ + DYJ
       IF (OUT) WRITE (IUMONM,32) KTR,PJ,YJP
32     FORMAT (' YJ pert',I3,' PJ,YJP',2(1PE14.6))
       CALL JERRXX(RW,IW,NRK2,PJ,YJP,
     ;             CD,PD,VD,YD,EMBY,EEBY,FTMAXP,ETMAXP,IERR)
       IF (IERR.NE.0)  RETURN
       IF (OUT) WRITE (IUMONM,34) YD,EMBY,EEBY,VD,CD
34     FORMAT (' YJ: YD,EMBP,EEBP/VD,CD',4(1PE14.6)/4E14.6)
c
c    NR derivatives
       DEMDYJ = (EMBY - EMB)/DYJ
       DEEDYJ = (EEBY - EEB)/DYJ
       IF (OUT) WRITE (IUMONM,35) DEMDYJ,DEEDYJ
35     FORMAT (' DEMDYJ,DEEDYJ',2(1PE14.6))
c    dual Newton-Rhapson adjustment
       DET = DEMDPJ*DEEDYJ - DEEDPJ*DEMDYJ
       IF (DET.EQ.0) THEN
c        balances are insensitive to the junction state; increase P
           IF (OUT) WRITE (IUMONM,37)
37         FORMAT (' Junction P too small')
           PJ = 1.2*PJ
           GOTO 10
           ENDIF
       DPJ = ( - EMB*DEEDYJ + EEB*DEMDYJ)/DET
       DYJ = ( - EEB*DEMDPJ + EMB*DEEDPJ)/DET
       GOTO 50
c
c    Newton-Rhapson on PJ
40     IF (DEMDPJ.EQ.0)  THEN
           IF (OUT) WRITE (IUMONM,42)
42         FORMAT (' Singular NR on PJ; increasing PJ')
           PJ = 1.5*PJ
           GOTO 10
           ENDIF
c    set the changes
       DPJ = - EMB/DEMDPJ
       DYJ = 0
c
c    change handling for either NR
50     IF (OUT) WRITE (IUMONM,52) DPJ,DYJ
52     FORMAT (' NR changes: DPJ,DYJ=',2(1pe14.6))
c
c    check YJ change
       X = ABS(DYJ/YJ)
       IF (X.GT.0.2)  THEN
           IF (ABS(EMB).GT.EMBOK)  THEN
c                drop back to NR on PJ
                   IF (OUT) WRITE (IUMONM,53)
53                 FORMAT (' Excessive YJ change; using NR on PJ.')
                   GOTO 40
               ELSE
c                limit the changes
                   FAC = 0.2/X
                   DPJ = FAC*DPJ
                   DYJ = FAC*DYJ
                   IF (OUT) WRITE (IUMONM,54) DPJ,DYJ
54                 FORMAT ('  Reducing excessive YJ change: '//
     ;                     'DPJ,DYJ=',2(1pe14.6))
               ENDIF
           ENDIF
c
c    check PJ change
       X = ABS(DPJ/PJ)
       IF (X.GT.0.2)  THEN
c        limit the changes
           FAC = 0.2/X
           DPJ = FAC*DPJ
           DYJ = FAC*DYJ
           IF (OUT) WRITE (IUMONM,56) DPJ,DYJ
56         FORMAT ('   Reducing excessive PJ change: DPJ,DYJ='
     ;             ,2(1pe14.6))
           ENDIF
c
c    check for best try
       IF (X.LT.XBEST) THEN
c        save the best try
           KTRBST = KTR
           XBEST = X
           EMBEST = EMB
           EEBEST = EEB
           PJBEST = PJ
           YJBEST = YJ
           ENDIF
c
c    make the changes
       PJ = PJ + DPJ
       YJ = YJ + DYJ
c    try again
       GOTO 10
c
c * converged
c
c    load the junction state
70     RW(IoP0J+I) = PJ
       RW(IoYJ+I) = YJ
c    load the duct states
       RW(IoC+IX2) = CD(1)
       RW(IoP+IX2) = PD(1)
       RW(IoV+IX2) = VD(1)
       RW(IoY+IX2) = YD(1)
       RW(IoC+IX3) = CD(2)
       RW(IoP+IX3) = PD(2)
       RW(IoV+IX3) = VD(2)
       RW(IoY+IX3) = YD(2)
c    ok return
       IERR = 0
       RETURN
c
c    error
90     IF (XBEST.LT.0.01) THEN
c        accept the best try
           IF (MONM.GT.0) WRITE (IUMONM,92) KTRBST
92         FORMAT (' Redoing acceptable best try',I3)
           PJ = PJBEST
           YJ = YJBEST
           BEST = .TRUE.
           GOTO 10
           ENDIF
c    check for diagnostics
       IF (.NOT.OUT)  THEN
c        rerun with diagnostics
           OUT = .TRUE.
           PJ = RW(IoP0J+IM1)
           YJ = RW(IoYJ+IM1)
           GOTO 2
           ENDIF
c    failure exit
       IERR = IERRX
       CLOSE (IUMONM)
       OPEN (IUMONM,FILE='MANTEMP.MON')
c    instruct user
       CALL WARNZZ('#','#The manifold junction routine '//
     ;   'failed.  File MANIFOLD.MON '//
     ;   'contains data needed to diagnose and fix the problem.  '//
     ;   'If your input data seem ok, please quit ESP now and save '//
     ;   'this file and your setup file '//
     ;   'in a separate directory (so that they will '//
     ;   'not be overwritten by your next ESP run) and contact# #'//
     ;  '    wcr@thermo.stanford.edu# #'//
     ;  'to arrange for transfer of these files so that we can '//
     ;  'make ESP work better.  Do not email these files!##')
       RETURN
       END
c*****************************************************************************

