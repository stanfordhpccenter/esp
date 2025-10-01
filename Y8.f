c******************************************************************************
c
c      Manifold valve routine
c
c******************************************************************************
c
       SUBROUTINE FDRPXX(RW,PIN,Y,Z,PC,YC,AD,AV,CFI,CFO,FEGR,TREGR,
     ;                   CD,PD,VD,YD,VVA,FLOW,IERR)
c
c      Determines the flow from a duct through a restriction to a plenum
c      given the duct variables Y,Z and the cylinder pressure PC and entropy
c      parameter Y=P/rho^k. Handles flow in either direction and choking. The
c      restriction can be a valve, a throttle, or junction blockage. The
c      plenum can be the cylinder, the junction, or the atmosphere.
c------------------------------------------------------------------------------
c      This was written as an intake valve program. Therefore, below the
c      restriction is called the "valve" and the plenum the "cylinder".
c
c      The analysis assumes PIN = .TRUE. and changes signs at exit if .FALSE.
c------------------------------------------------------------------------------
c      Model:
c
c      Inflow to the plenum is isentropic to an area = AV*CFI, where the
c      pressure is PC. Outflow from the plenum is isentropic to an area
c      AV*CFO, where the pressure is PV. The pressure recovery is based
c      in a momentum analysis
c
c      If the flow at the restriction is choked, the flow pressure PV
c      can be greater than the back pressure (PC or PD).
c
c      The analysis allows for EGR, which is a set fraction of the flow
c      into the cylinder and enters just upstream of the restriction at a
c      specified temperature. Alteration of the gas specific heats by the
c      recirculated flow is neglected. Specific heats are assumed constant.
c
c      Should be called with AV < AD. If not, the duct area is treated as
c      the valve area.
c
c      With backflow from the valve into the duct, the model assumes that the
c      pressure in the duct is the valve lip pressure (no pressure recovery).
c      The analysis carefully takes into account the entropy discontinuity
c      between that of the massin the duct and that of the gas coming out of
c      the cylinder into the duct.  This is a non-trivial gasdunamic analysis.
c      FDRPT.F is the test program that has been used to test this routine
c      and confirm that it s correct under all circumstances.
c
c      The Z parameter must be corrected for the entropy change between launch
c      of the characteristic and its reception at the valve. This correction
c      depends on the entropy at the valve, which in the case of outlow from
c      the valve is part of the solution. Hence the correction for entropy
c      change along the characteristic is handled as part of the valve flow
c      solution.
c------------------------------------------------------------------------------
c      Input:
c        RW    real work array for the manifold
c        PIN   if .TRUE.,  Z=[2/(k-1)]c+V, V is positive into the plenum
c        PIN   if .FALSE., Z=[2/(k-1)]c-V, V is positive out of the plenum
c        Y     P/rho^k, the entropy parameter, at launch of Z
c        Z     acoustic parameter, m/s
c        PC    cylinder pressure, Pa
c        YC    Y in cylinder
c        AD    duct flow area, m^2
c        AV    valve (restriction) flow area, m^2
c        CFI   valve flow coefficient for flow into cylinder
c        CFO   valve flow coefficient for flow out of cylinder
c        FEGR  fraction of cylinder flow recirculated into the manifold
c        TREGR ratio of stagnation T of the EGR to that of the duct flow
c              (input the value from the last time step)
c
c      Output:
c        CD    sound speed in duct, m/s
c        PD    pressure in duct, Pa
c        VD    velocity in duct, m/s
c        YD    entropy parameter Y at the duct (=Y for inflow)
c        FLOW  mass flow rate into the plenum, kg/s
c        VVA   absolute value of valve velocity, m/s
c
c      IERR = 0 if ok, >0 if problems
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoKM1 ,MPTR(2))
       EQUIVALENCE (IoKP1 ,MPTR(3))
       EQUIVALENCE (IoKMKP,MPTR(4))
       EQUIVALENCE (IoKM12,MPTR(6))
       EQUIVALENCE (IoKM3M,MPTR(7))
       EQUIVALENCE (IoKKM1,MPTR(8))
       EQUIVALENCE (IoKPO2,MPTR(10))
       EQUIVALENCE (IoRK  ,MPTR(11))
       EQUIVALENCE (IoR2K ,MPTR(12))
       EQUIVALENCE (IoRKM1,MPTR(13))
       EQUIVALENCE (IoTK  ,MPTR(15))
       EQUIVALENCE (IoTRKM,MPTR(16))
       EQUIVALENCE (IoTRKP,MPTR(17))
       EQUIVALENCE (IoHKPM,MPTR(22))
       EQUIVALENCE (IoHKMK,MPTR(23))
       EQUIVALENCE (IoC0CS,MPTR(24))
       EQUIVALENCE (IoPRPC,MPTR(27))
       EQUIVALENCE (IoCRCC,MPTR(28))
       EQUIVALENCE (IoRCHM,MPTR(29))
       EQUIVALENCE (IoSQTK,MPTR(30))
       EQUIVALENCE (IoK2M1,MPTR(34))
       EQUIVALENCE (IoHKP1,MPTR(35))
       EQUIVALENCE (IoSFMC,MPTR(36))
       EQUIVALENCE (IoCMD2,MPTR(37))
c------------------------------------------------------------------------------
       REAL    MZ,MD,ME,MV,MDX,MMVX,MV2,MD2,ME2,MDC,MX,MXN,MEN,
     ;         MDZC,MDZC2,MVZC,MVZC2,MDVC,MDVC2,MZVC2,
     ;         MEMIN,MEMAX,MVMIN,MVMAX,MDLAST,MVLAST,
     ;         MXMIN,MXMAX
       LOGICAL PIN,MINED,MAXED,VCHOKE,ZCHOKE
c------------------------------------------------------------------------------
c    diagnostic monitor
       LOGICAL OUT
       COMMON /MONMXX/IUMONM,MON,OUT
c------------------------------------------------------------------------------
       DATA  KTRMAX,ERRH,DMOK /20,1.E-5,1E-5/
c------------------------------------------------------------------------------
c     restart point and diagnostic header
2      IF (OUT) WRITE (IUMONM,3)  PIN,Y,Z,PC,YC,YD,
     ;                            AD,AV,CFI,CFO,FEGR,TREGR
3      FORMAT (/' FDRPXX call for:  PIN = ',L2/
     ;   '  Y,Z,PC,YC,YD/AD,AV,CFI,CFO/FEGR,TREGR'/5(1PE14.6)/4E14.6/
     ;      2e14.6)
c
c    check for valve closed
       IF (AV.LE.0) GOTO 80
c
c    check for all FEGR
       IF (FEGR.GE.1)  THEN
           WRITE (IUMONM,4)
4          FORMAT (' Excessive EGR')
           CALL WARNZZ('@','@Excessive EGR; run cancelled.@@')
           IERR = 1
           RETURN
           ENDIF
c
c    determine the flow direction
       BETA = Z/CPYXX(RW,PC,Y)
       GAMMA = RW(IoTRKM)
       IF (OUT) WRITE (IUMONM,8) BETA,GAMMA
8      FORMAT (' BETA,GAMMA',2(1PE14.6))
       IF (BETA.GT.1.00001*GAMMA) THEN
c            inflow
               GOTO 20
           ELSEIF (BETA.LT.0.99999*GAMMA)  THEN
c            outflow
               GOTO 50
           ELSE
c            no flow
               GOTO 80
           ENDIF
c
c ** inflow
c
c    check valve area
20     IF (FEGR.GT.0)  THEN
               PHI = (1 - FEGR) + TREGR*FEGR
               C0VOC0 = SQRT(PHI)
               TERM = AD*C0VOC0/AV
           ELSE
               C0VOC0 = 1
               TERM = AD/AV
               PHI = 1
           ENDIF
       IF (TERM.LT.1) THEN
c            reduce effective valve area so valve chokes first
               AVX = AV*TERM
               TERM = 1
           ELSE
c            valve area ok
               AVX = AV
           ENDIF
       IF (OUT) WRITE (IUMONM,21) AD,AV,AVX
21     FORMAT (' AD,AV,AVX=',3(1PE14.6))
c    get parameters
       CPSI = (1 - FEGR)/PHI
       CPSI2 = CPSI*CPSI
       ZETA = AD/(AVX*CFI)
c
c    set to compute ME for MV = 1 to check for choking
       MV =  1
       TV = RW(IoHKP1)
       STV = RW(IoC0CS)
       MODE = 1
c    trial value based on small ME approximation with MV=1
       ME = RW(IoRCHM)/ZETA
       IF (ME.GT.0.5) ME = 0.5
c    set limits
       MEMAX = 1
       MEMIN = 0
c
c    prepare to iterate
28     MINED = .FALSE.
       MAXED = .FALSE.
       KTR = 0
       DME = 1
c
c - Loop point for NR iterations
c
30     KTR = KTR + 1
       ME2 = ME*ME
       TE = 1 + RW(IoKM12)*ME2
       STE = SQRT(TE)
c
c    compute MD for trial ME
       PSI2 = CPSI2*ME2*TE
       TERM = 2*RW(IoKM1)*PSI2
       IF (TERM.LT.0.01)  THEN
               TERM2 = TERM*TERM
               QN = 0.5*TERM - 0.125*TERM2 + 0.0625*TERM*TERM2
           ELSE
               QN = SQRT(1 + TERM) - 1
           ENDIF
       IF (QN.LT.0)  THEN
           WRITE (IUMONM,31) QN
31         FORMAT (' FDRPXX error; QN =',E14.6)
           GOTO 900
           ENDIF
       MD2 = QN*RW(IoRKM1)
       MD = SQRT(MD2)
       TD = 1 + RW(IoKM12)*MD2
       STD = SQRT(TD)
c
       IF (MODE.EQ.2)  THEN
c        compute MV for trial MD and ME
           TERM = BETA/(MD + GAMMA)
           MV2 = (TERM*TERM*TE - 1)*RW(IoTRKM)
c        if zero, set for no flow
           IF (MV2.LE.0) GOTO 80
           MV = SQRT(MV2)
           TV = 1 + RW(IoKM12)*MV2
           STV = SQRT(TV)
           ENDIF

c    compute H
       H = ZETA*(ME/MV)*(TV/TE)**RW(IoHKPM)
       IF (OUT) WRITE (IUMONM,34) KTR,MD,ME,MV,H
34     FORMAT (' KTR,MD,ME,MV,H',I3,3F10.6,1PE14.6)
       HERR = H - 1
c    check convergence
       IF (((ABS(HERR).LT.ERRH).OR.(ABS(MEMAX-MEMIN).LT.DMOK)).OR.
     ;      (ABS(DME).LT.DMOK))  GOTO 40

c    check count
       IF (KTR.GT.KTRMAX) GOTO 900
c
c    compute H derivatives
       DHDME = H*(1/ME - RW(IoHKPM)*RW(IoKM1)*ME/TE)
       DMDDME = CPSI*(STE + RW(IoKM12)*ME2/STE)/
     ;               (STD + RW(IoKM12)*MD2/STD)
       IF (MODE.EQ.2)  THEN
               DHDMV = H*( - 1/MV + RW(IoHKPM)*RW(IoKM1)*MV/TV)
               DMVDME = (TV/MV)*(- DMDDME*STE/(BETA*STV*RW(IoKM12))
     ;                     + ME/TE)
               DHDMET = DHDME + DHDMV*DMVDME
           ELSE
               DHDMET = DHDME
           ENDIF
c    reset limits
       IF (DHDMET.GT.0)  THEN
               IF (HERR.LT.0)  THEN
                       MEMIN = ME
                       HMIN = H
                       MINED = .TRUE.
                   ELSE
                       MEMAX = ME
                       HMAX = H
                       MAXED = .TRUE.
                   ENDIF
           ELSE
               IF (HERR.LT.0) THEN
                       MEMAX = ME
                       HMAX = H
                       MAXED = .TRUE.
                   ELSE
                       MEMIN = ME
                       HMIN = H
                       MINED = .TRUE.
                   ENDIF
           ENDIF
c    compute change
       DME = - HERR/DHDMET
       MEN = ME + DME
c    limit change
       IF (MAXED.AND.MINED)  THEN
c            solution bracketed
               IF (ABS(DME).GT.0.5*(MEMAX - MEMIN)) THEN
c                oscillating; split bracket
                   MEN = 0.5*(MEMAX + MEMIN)
                   ENDIF
           ELSE
c            not yet bracketed; limit change
               IF (ABS(DME).GT.0.2) THEN
                   DME = 0.2*DME/ABS(DME)
                   MEN = ME + DME
                   ENDIF
c            stay in bounds
               IF (MEN.GE.MEMAX)  THEN
                       MEN = 0.5*(ME + MEMAX)
                   ELSEIF (MEN.LE.MEMIN)  THEN
                       MEN = 0.5*(ME + MEMIN)
                   ENDIF
           ENDIF
       ME = MEN
       GOTO 30
c
c    H converged
40     IF (MODE.EQ.1)  THEN
c        check for choking
           BETAC = (MD + GAMMA)*STV/STE
           IF (BETA.LT.BETAC) THEN
c            not choked; set to find solution
               MEMAX = ME
               MEMIN = 0
               MODE = 2
c            trial value based on small M approximation
               BZ2 = BETA*ZETA*ZETA
               TERM = (BETA - GAMMA)*BZ2*RW(IoKM1)/CPSI2
               IF (TERM.LT.0.001)  THEN
                       QN = CPSI*0.5*TERM
                   ELSE
                       QN = CPSI*(SQRT(1+TERM) - 1)
                   ENDIF
               ME = QN/(RW(IoKM12)*BZ2)
               IF (ME.GT.0.5) ME = 0.5
               GOTO 28
               ENDIF
           ENDIF
c
c    converged on solution
       CD = Z/(MD + GAMMA)
       VD = MD*CD
c
c    final inflow calculations
       YD = Y
       PD = PCYXX(RW,CD,YD)
       RHOD = RW(IoK)*PD/(CD*CD)
       FLOW = AD*RHOD*VD/(1 - FEGR)
       VVA = MV*CD*C0VOC0*SQRT(TD/TV)
       GOTO 90
c
c ** outflow; EGR is assumed to be outflow when the main flow is outflow
c
c      State V is at the valve area AV
c      State D is in the duct between the valve and entropy discontinuity
c      State Z is in the duct where Z and Y are specified.
c
c    set effective valve area if duct smaller
50     AVX = MIN(AD,AV)
       ALPHA = AD/((1 - FEGR)*CFO*AVX)
       IF (OUT) WRITE (IUMONM,51) ALPHA,AD,AV,AVX
51     FORMAT (' ALPHA,AD,AV,AVX=',4(1PE14.6))
c
c    set stagnation sound speed
       C0 = CPYXX(RW,PC,YC)
c    calculate constants
       YCOYZP = (YC/Y)**RW(IoR2K)
       XT = Z/C0
       XT2 = XT*XT
       ALPHA2 = ALPHA*ALPHA
       B1 = ALPHA
       B2 = RW(IoK)
       B3= RW(IoK)/(1 - FEGR)
       C1 = RW(IoTRKM)/(YCOYZP*ALPHA**RW(IoHKMK))
c    assume Z not choled
       ZCHOKE = .FALSE.
c
c *  re-entry point if Z is found to be choked
c
c    set to compute MD for MV = -1 to check for choking
58     MV = - 1
       KTR  = - 1
       VCHOKE = .FALSE.
c    set limits
       MXMAX = 0
       MXMIN = - 1
c    prepare to iterate
       MINED = .FALSE.
       MAXED = .FALSE.
       DMX = 1
c
c  - loop point for NR iteration
c
60     KTR = KTR + 1
c
       IF (.NOT.VCHOKE)  THEN
c        calculate MD for trial MV from the momentum balance with PB = PV
           MV2 = MV*MV
           TV = 1 + RW(IoKM12)*MV2
           STV = SQRT(TV)
           PHI = (B1 + B3*MV2)/(MV*STV)
           PHI2 = PHI*PHI
           EPS = PHI2*RW(IoHKMK)*RW(IoRK) - 1
c        quadratic equation solution
           IF (ABS(EPS).LT.0.05) THEN
c                series expansion near the singularity
                   MD2 = RW(IoCMD2+1)
                   EPSN = 1
                   DO 61 N=2,6
                       EPSN = EPSN*EPS
                       MD2 = MD2 + RW(IoCMD2+N)*EPSN
61                     CONTINUE
                   TERM = - 1
               ELSE
c                full evaluation with accurate square root
                   RPHI2 = 1/PHI2
                   TERM =2*RW(IoKP1)*RPHI2
                   IF (TERM.GT.1.0) THEN
                       IF (TERM.GT.1.00001)  THEN
                           IF (OUT) WRITE (IUMONM,62) MV,TERM
62                         FORMAT (' Error: MV,TERM',2(1PE14.6))
                           IERR = 1
                           GOTO 900
                           ENDIF
                       TERM = 1
                       ENDIF
                   IF (TERM.LT.0.1) THEN
                           TERM2 = TERM*TERM
                           QN = RW(IoTK)*RPHI2
     ;                      - 0.5*TERM - 0.125*TERM2 - 0.0625*TERM2*TERM
     ;                      -  0.03906*TERM2*TERM2
                       ELSE
                           QN = RW(IoTK)*RPHI2 - 1 + SQRT(1 - TERM)

                       ENDIF
                   MD2 = QN/(2*(RW(IoKM12) - B2*B2*RPHI2))
               ENDIF
           IF (MD2.LE.0) THEN
               IF (OUT) WRITE (IUMONM,63) MV,MD2
63             FORMAT (' Error: MV,MD2',2(1PE14.6))
               IERR = 1
               GOTO 900
               ENDIF
           MD = - SQRT(MD2)
           TD = 1 + RW(IoKM12)*MD2
           STD = SQRT(TD)
           ENDIF
c
c    compute test function
       IF (ZCHOKE)  THEN
c            from MZ= -1
               H = - MD*YCOYZP*((ALPHA*MD/MV)**RW(IoHKMK))
     ;                        *(STV/STD)**RW(IoKPO2)
           ELSE
c            from the acoustic equation
               H1 = XT*STD/MD
               H2 = -C1*((MV/MD)**RW(IoHKMK))*((STD/STV)**RW(IoKPO2))/MD
               H = H1 + H2
           ENDIF
       HERR = H - 1
c    diagnostic output
       IF (OUT) WRITE (IUMONM,65) KTR,MD,MV,H
65     FORMAT (' KTR,MD,MV,H=',I3,3(1PE14.6))
c
c    branch on task
       IF (KTR.EQ.0)  THEN
c        calculated MD for MV=-1
           IF ( ((.NOT.ZCHOKE).AND.(H.GE.1)).OR.
     ;           (ZCHOKE.AND.(H.LE.1)) ) THEN
c            choked at V
               VCHOKE = .TRUE.
               IF (MD2.GE.1)  THEN
c                also choked at D; revise MD (PB<PV)
                   MD = - 1
                   TD = RW(IoHKP1)
                   STD = RW(IoC0CS)
                   GOTO 68
                   ENDIF
c            V choked but D not choked; go iterate for MD at MV=-1
               GOTO 66
               ENDIF
c        V not choked; set trial MV
           IF (ZCHOKE)  THEN
                   MV = - ALPHA/YCOYZP
               ELSE
                   MV = ALPHA*(XT - RW(IoTRKM)/YCOYZP)
               ENDIF
           IF (MV.LT.-0.5) MV = - 0.5
           GOTO 60
           ENDIF
c
c    check convergence of H
66     IF (((ABS(HERR).LT.ERRH).OR.(ABS(MXMAX-MXMIN).LT.DMOK)).OR.
     ;   (ABS(DMX).LT.DMOK))  THEN
           IF (ABS(MV).LT.1E-5)  GOTO 80
           IF (ZCHOKE)  GOTO 70
c        converged with Z not choked
           GOTO 68
           ENDIF
c
c    check iteration count
       IF (KTR.GT.KTRMAX)  THEN
           IF (OUT)  WRITE (IUMONM,67)
67         FORMAT (' FDRPXX outflow iteration not convergent')
           GOTO 900
           ENDIF
c
c    N-R iteration
       IF (.NOT.VCHOKE)  THEN
c        calculate dMD/dMV from the momentum balance
           U1 = B1 + B3*MV2
           W1 = U1*STD
           U2 = B2*MD + 1/MD
           W2 = U2*MV*STV
           CDMV = W1*2*B3*MV/U1 - W2*(1/MV + RW(IoKM12)*MV/TV)
           CDMD = W1*RW(IoKM12)*MD/TD - W2*(B2 - 1/MD2)/U2
           DMDDMV = - CDMV/CDMD
           ENDIF
       IF (ZCHOKE)  THEN
c            iterating to find MD and MV for MZ = -1 (no acoustic condition)
               DHDMD = H*(1/MD + RW(IoHKMK)/MD
     ;                     - RW(IoKPO2)*RW(IoKM12)*MD/TD)
               IF (.NOT.VCHOKE)  THEN
                   DHDMV = H*( - RW(IoHKMK)/MV
     ;                     + RW(IoKPO2)*RW(IoKM12)*MV/TV)
                   ENDIF
           ELSE
c            iterating to find MD and MV to satisfy the acoustic condition
               DHDMD = H1*(RW(IoKM12)*MD/TD - 1/MD)
     ;           + H2*((- RW(IoHKMK) - 1)/MD
     ;                + RW(IoKPO2)*RW(IoKM12)*MD/TD)
               IF (.NOT.VCHOKE) THEN
                   DHDMV = H2*(RW(IoHKMK)/MV
     ;                    - RW(IoKPO2)*RW(IoKM12)*MV/TV)
                   ENDIF
           ENDIF
       HERR = H - 1
c    branch on mode
       IF (VCHOKE)  THEN
c            choked at V; adjusting MD keeping MV=-1
               MX = MD
               DHDMX = DHDMD
           ELSE
c            not choked at V; adjusting MV
               MX = MV
               DHDMX = DHDMV +  DHDMD*DMDDMV
           ENDIF
c    reset limits
       IF (DHDMX.GT.0)  THEN
               IF (HERR.LT.0)  THEN
                       MXMIN = MX
                       HMIN = H
                       MINED = .TRUE.
                   ELSE
                       MXMAX = MX
                       HMAX = H
                       MAXED = .TRUE.
                   ENDIF
           ELSE
               IF (HERR.LT.0) THEN
                       MXMAX = MX
                       HMAX = H
                       MAXED = .TRUE.
                   ELSE
                       MXMIN = MX
                       HMIN = H
                       MINED = .TRUE.
                   ENDIF
           ENDIF
c    compute change
       DMX = - HERR/DHDMX
       MXN = MX + DMX
c    limit change
       IF (MAXED.AND.MINED)  THEN
c            solution bracketed
               IF (ABS(DMX).GT.0.5*(MXMAX - MXMIN)) THEN
c                oscillating; split bracket
                   MXN = 0.5*(MXMAX + MXMIN)
                   ENDIF
           ELSE
c            not yet bracketed; limit change
               IF (ABS(DMX).GT.0.2) THEN
                   DMX = 0.2*DMX/ABS(DMX)
                   MXN = MX + DMX
                   ENDIF
c            stay in bounds
               IF (MXN.GE.MXMAX)  THEN
                       MXN = 0.5*(MX + MXMAX)
                   ELSEIF (MXN.LE.MXMIN)  THEN
                       MXN = 0.5*(MX + MXMIN)
                   ENDIF
           ENDIF
c    load the new value
       IF (VCHOKE)  THEN
c            set new trial MD
               MD = MXN
           ELSE
c            set new trial MV
               MV = MXN
           ENDIF
       GOTO 60

c    MD and MV are ok; check Z
68     MZ = MD*YCOYZP*((ALPHA*MD/MV)**RW(IoHKMK))*(STV/STD)**RW(IoKPO2)
       IF (MZ.LT.-1)  THEN
c        choked at Z; set to find MV and MD for MZ=-1
           ZCHOKE = .TRUE.
           GOTO 58
           ENDIF
c
c    final outflow calculations
70     CD = C0/STD
       VD = MD*CD
       CD2 = CD*CD
       PV = PC/TV**RW(IoKKM1)
       PD = PV*STV*MV/(ALPHA*STD*MD)
       YD = YC2PXX(RW,CD2,PD)
       RHOD = RW(IoK)*PD/CD2
       FLOW = AD*RHOD*VD/(1 - FEGR)
       VVA = ABS(MV)*C0/STV
       GOTO 90
c
c    no flow
80     VD = 0
       MD = 0
       MV = 0
       CD = Z*RW(IoKM12)
       YD = Y
       PD = PCYXX(RW,CD,YD)
       FLOW = 0
       VVA = 0
c
c    normal exit
90     IERR = 0

c    check flow direction
       IF (.NOT.PIN)  THEN
c        set postive out
           VD = - VD
           FLOW = - FLOW
           MD = - MD
           MV = - MV
           ENDIF
       RETURN
c
c *  error exit
c
c    check for rerun
900    IF (.NOT.OUT)  THEN
c        rerun with diagnostic output
           OUT = .TRUE.
           GOTO 2
           ENDIF
c
c    write the restart file
       OPEN (10,FILE='FINDFLOW.BAD',FORM='UNFORMATTED')
       WRITE (10) PIN,Y,Z,PC,YC,AD,AV,CFI,CFO,FEGR,TREGR,
     ;       (RW(I),I=1,70)
       CLOSE (10)
c    complete the monitor file and repoen a dummy
       WRITE (IUMONM,902)
902    FORMAT (' Test file FINDFLOW.BAD written')
       CLOSE (IUMONM)
       OPEN (IUMONM,FILE='MANTEMP.MON')
c    instruct user
       CALL WARNZZ('#','#The manifold entry/exit flow routine '//
     ;   'failed to converge.  Files MANIFOLD.MON and FINDFLOW.BAD '//
     ;   'contain data needed to diagnose and fix the problem.  '//
     ;   'If your input data seem ok, please quit ESP now and save '//
     ;   'these files and your setup file '//
     ;   'in a separate directory (so that they will '//
     ;   'not be overwritten by your next ESP run) and contact# #'//
     ;  '    wcr@thermo.stanford.edu# #'//
     ;  'to arrange for transfer of these files so that we can '//
     ;  'make ESP work better.  Do not email these files!##')
       IERR = 1
       RETURN
c
       END
c******************************************************************************
c
       SUBROUTINE CENDXX(RW,Y,Z,CD,PD,VD,YD)
c
c      Determines the duct state at a closed end.
c
c      If Z=[2/(k-1)]c+V then V is positive into the closure.
c      If Z=[2/(k-1)]c-V then V is positive out of the closure.
c------------------------------------------------------------------------------
c      Input:
c        RW    real work array for the manifold
c        Y     P/rho^k, the entropy parameter at the duct end
c        Z     acoustic parameter, m/s
c
c      Output:
c        CD    sound speed in duct, m/s
c        PD    pressure in duct, Pa
c        VD    velocity in duct, m/s  (zero at closed end)
c        YD    entropy parameter in the duct at the closed end
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoKM12,MPTR(6))
       EQUIVALENCE (IoR2K ,MPTR(12))
c------------------------------------------------------------------------------
       VD = 0
       CD = Z*RW(IoKM12)
       PD = PCYXX(RW,CD,YD)
       YD = Y
       RETURN
       END
c******************************************************************************

