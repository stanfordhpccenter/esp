c******************************************************************************
c
c      Single cycle analysis subroutine
c
c******************************************************************************
c
       SUBROUTINE  CYCLVV(IERR)
c
c      Main routine for analysis of a cycle
c      Returns IERR = 0 if completed, 1 if some error
c-----------------------------------------------------------------------------
c     The following are used from RW explicitly by this subroutine:
c          HEX     total enthalpy of mixed exhaust gas this cycle, J
c          HMS     specific enthalpy of mixed charge, J/kg
c          MPE     mass of products in exhaust gas this cycle, kg
c          MRE     mass of reactants in exhaust gas this cycle, kg
c          PC      pressure in cylinder, Pa
c          PI      inlet pressure, Pa
c          QC      heat transferred to coolant thus far this cycle, J
c          TC      temperature of cylinder gas (unburned during burn), K
c          TE      temperature of mixed exhaust gas, K
c          VC      cylinder volume, m**3
c          VELT    turbulence velocity, m/s
c          WP      total work done on piston thus far this cycle, J
c-----------------------------------------------------------------------------
c     The following must be in IW at call:
c          IANG    current crank angle, degrees (0 - 719)
c          IANR    angle from compression start, degrees
c          IAIC    IANG at close of intake (start of cycle analysis)
c          IAEO    IANG at exhaust open (start of gas exchange stage)
c          IAIG    crank degrees after compression TDC where ignition occurs
c          IIGR    angle from compression start to ignition, degrees
c          IEOR    angle from compression start to gas exchange, degrees
c          KFOP    firing code 1 firing, 2 motoring
c          LSTG    stage indicator
c                1 compression
c                2 burn
c                3 expansion
c                4 gas exchange
c-----------------------------------------------------------------------------
c      The following are available in IW on return:
c          NPEC    number of polytropic exponent points for compression
c          NPEE    number of polytropic exponent points for expansion
c          JAMP    angle at maximum pressure, degrees
c          JAEB    angle at end of burn, degrees
c          JTEX    exhaust temperature, K
c-----------------------------------------------------------------------------
c      The following are available in RW on return:
c          PECS    sum of polytropic exponent contributions for compression
c          PEES    sum of polytropic exponent contributions for expansion
c          PMAX    cycle peak pressure, Pa
c          ERRT    start-finish temperature error, K
c          ERRP    start-finish pressure error, atm.
c          ERRV    start-finish turbulence velocity error, % of turb. velocity
c          ERRM    cycle mass balance error/mass in
c          ERRE    cycle energy balance error/work
c-----------------------------------------------------------------------------
       REAL        KT,MR,MP,MC,MU,MUE,MPE,MRE,MIN,MEX,MDIA,NVOL
c-----------------------------------------------------------------------------
c    manifold variables duplicated in cylinder array for output
       REAL        MIM(4),MEM(4)
       DIMENSION   PIM(4),VIM(4),PEM(4),VEM(4)
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION   IW(50),RW(400)
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
c-----------------------------------------------------------------------------
c    cylinder monitor
       COMMON /MONCVV/IUMONC,MONC
c-----------------------------------------------------------------------------
c    manifold monitor
       LOGICAL OUT,test
       COMMON /MONMXX/IUMONM,MONM,OUT
c-----------------------------------------------------------------------------
c    manifold common blocks
       PARAMETER   (NMPTR = 96)
       PARAMETER   (NDRW = 21024)
       PARAMETER   (NDIW = 2904)
       DIMENSION   MPTR(NMPTR)
       DIMENSION   RWI(NDRW)
       DIMENSION   IWI(NDIW)
       DIMENSION   RWE(NDRW)
       DIMENSION   IWE(NDIW)
       COMMON /MPTRXX/ MPTR
       COMMON /RWIXX/  RWI
       COMMON /IWIXX/  IWI
       COMMON /RWEXX/  RWE
       COMMON /IWEXX/  IWE
c------------------------------------------------------------------------------
c    Cycle stage subroutines:
       EXTERNAL    ADVBVV,ADVCVV,ADVEVV,ADVGVV,
     ;             ALGBVV,ALGCVV,ALGEVV,ALGGVV,
     ;             CPYBVV,CPYCVV,CPYEVV,CPYGVV,
     ;             DDTBVV,DDTCVV,DDTEVV,DDTGVV
c-----------------------------------------------------------------------------
c    Storage in the cylinder work arrays:
       EQUIVALENCE (UC,  RW(1))
       EQUIVALENCE (MP,  RW(2))
       EQUIVALENCE (MR,  RW(3))
       EQUIVALENCE (KT,  RW(4))
       EQUIVALENCE (QC,  RW(5))
       EQUIVALENCE (WP,  RW(6))
       EQUIVALENCE (MU  ,RW(7))
       EQUIVALENCE (TU  ,RW(9))
       EQUIVALENCE (PU  ,RW(10))
       EQUIVALENCE (MIN ,RW(11))
       EQUIVALENCE (HIN ,RW(12))
       EQUIVALENCE (MEX ,RW(16))
       EQUIVALENCE (HEX ,RW(17))
       EQUIVALENCE (MPE ,RW(18))
       EQUIVALENCE (MRE ,RW(19))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (VELT,RW(106))
       EQUIVALENCE (XPC ,RW(107))
       EQUIVALENCE (FIN ,RW(110))
       EQUIVALENCE (FEX ,RW(111))
       EQUIVALENCE (DMHD,RW(120))
       EQUIVALENCE (AFLE,RW(122))
       EQUIVALENCE (AFLI,RW(123))
       EQUIVALENCE (TE  ,RW(129))
       EQUIVALENCE (MUE ,RW(135))
       EQUIVALENCE (TU0 ,RW(139))
       EQUIVALENCE (PU0 ,RW(140))
       EQUIVALENCE (P0IV,RW(147))
       EQUIVALENCE (T0IV,RW(148))
       EQUIVALENCE (P0EV,RW(149))
       EQUIVALENCE (T0EV,RW(150))
       EQUIVALENCE (HMS ,RW(164))
       EQUIVALENCE (RHOC,RW(169))
       EQUIVALENCE (YE  ,RW(180))
       EQUIVALENCE (AMEV,RW(184))
       EQUIVALENCE (AMIV,RW(185))
       EQUIVALENCE (PE  ,RW(204))
       EQUIVALENCE (PI  ,RW(205))
       EQUIVALENCE (TI  ,RW(206))
       EQUIVALENCE (FEGR,RW(208))
       EQUIVALENCE (MDIA,RW(210))
       EQUIVALENCE (CDI ,RW(221))
       EQUIVALENCE (CDIB,RW(222))
       EQUIVALENCE (CDE, RW(223))
       EQUIVALENCE (CDEB,RW(224))
       EQUIVALENCE (PECS,RW(241))
       EQUIVALENCE (PEES,RW(242))
       EQUIVALENCE (PMAX,RW(243))
       EQUIVALENCE (ERRT,RW(286))
       EQUIVALENCE (ERRP,RW(287))
       EQUIVALENCE (ERRV,RW(288))
       EQUIVALENCE (ERRM,RW(289))
       EQUIVALENCE (ERRE,RW(290))
       EQUIVALENCE (NVOL,RW(293))
       EQUIVALENCE (PIM ,RW(321))
       EQUIVALENCE (VIM ,RW(325))
       EQUIVALENCE (PEM ,RW(331))
       EQUIVALENCE (VEM ,RW(335))
       EQUIVALENCE (MIM ,RW(341))
       EQUIVALENCE (MEM ,RW(345))
       EQUIVALENCE (YI  ,RW(349))
       EQUIVALENCE (CI  ,RW(350))
c-----------------------------------------------------------------------------
c    Storage in the cylinder integer work array IW
       EQUIVALENCE (IANG,IW(1))
       EQUIVALENCE (IAIO,IW(2))
       EQUIVALENCE (IAIC,IW(3))
       EQUIVALENCE (IAEO,IW(4))
       EQUIVALENCE (IAEC,IW(5))
       EQUIVALENCE (KFOP,IW(6))
       EQUIVALENCE (LSTG,IW(7))
       EQUIVALENCE (LEFL,IW(8))
       EQUIVALENCE (LIFL,IW(9))
       EQUIVALENCE (IAIG,IW(11))
       EQUIVALENCE (IANR,IW(30))
       EQUIVALENCE (IIGR,IW(31))
       EQUIVALENCE (IEOR,IW(32))
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
       EQUIVALENCE (NPEC,IW(41))
       EQUIVALENCE (NPEE,IW(42))
       EQUIVALENCE (JAMP,IW(43))
       EQUIVALENCE (JAEB,IW(44))
       EQUIVALENCE (JTEX,IW(45))
       EQUIVALENCE (NCYC,IW(46))
c-----------------------------------------------------------------------------
c    Storage in the manifold work arrays
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoY   ,MPTR(46))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoZ   ,MPTR(49))
       EQUIVALENCE (IoJUMP,MPTR(81))
       EQUIVALENCE (IoLAG ,MPTR(82))
       EQUIVALENCE (IoNCYE,MPTR(86))
       EQUIVALENCE (IoI   ,MPTR(91))
c-----------------------------------------------------------------------------
c    phase-averaged variables and weightings
       DIMENSION   PCAV(720),RHOCAV(720)
       COMMON      /AVCDVV/PCAV,RHOCAV,FCAVO,FCAVN,NCAV
c------------------------------------------------------------------------------
c    volume for indicator diagram
       DIMENSION   VCYL(720)
       COMMON      /VCYLVV/VCYL
c------------------------------------------------------------------------------
c    5-line plot options
       DIMENSION KOP5(6)
       COMMON/KOP5VV/KOP5,KOP5C
c------------------------------------------------------------------------------
c    test monitors
       MONC = 0
       MONM = 0
c
c    initialize for compression polytropic exponent computation
       NPEC = 0
       PECS = 0
c    set large burn-over angle to force **** in motoring display
       JAEB = 32000
c
c    initialize peak pressure
       PMAX = 0
c    save starting values for error determination
       PC0 = PC
       TC0 = TC
       VELT0 = VELT
c    initialize cycle quantities
       WP = 0
       QC = 0
       HIN = 0
       MIN = 0
       MEX = 0
       MPE = 0
       MRE = 0
       HEX = 0
c    clear all integration storage from the last cycle
       DO 3 I=21,100
           RW(I) = 0
3          CONTINUE
c
c    set cycle cylinder state averaging factors (should match manifold)
c
       FCAVN = MAX(1/(1 + 0.05*NCAV),0.5)
       FCAVO = 1 - FCAVN
c    increment cycle count
       NCAV = NCAV + 1
c
c    set counters
       KTR = 0
       KTRM = 0
c
c    The manifold index I is 1-720 with 1 at TDC compression.
c    The engine relative angle IANR is the angle from compression start.
c    The engine analysis index IANG is 0-719 with 0 at TDC compression.
c    IANG+1 corresponds to the manifold index I, also used for the array
c    storage for cylinder data.
c
c    Angles are initialized in SET0VV
c
       IF (MONC.GT.0)  WRITE (IUMONC,4)
4      FORMAT(/' IANG',6X,'PC',10X,'VC',10X,'TC',10X,'MC',
     ;            9X,'FIN',9X,'FEX'/' Starting compression')

c
c ** loop point after advance; all cylinder and manifold variables known
c
c    record volume for indicator diagram
6      IF (NCYC.EQ.0) VCYL(IANG+1) = VC
c
c    check for 5-Line plots
       IF (KOP5C.NE.0)  THEN
           IF (4*(IANG/4).EQ.IANG)  THEN
               CALL MP5SVV(IERR)
               IF (IERR.NE.0)  RETURN
               ENDIF
           ENDIF
c
c    branch on stage
       GOTO (10,20,30,40), LSTG
c
c -- compression stage
c
c    check for ignition
10     IF ((IANR.GE.IIGR).AND.(KFOP.EQ.1))  THEN
c        at ignition point; set for burn stage
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               IF (4*(IANG/4).EQ.IANG)  THEN
                   CALL MP5SVV(IERR)
                   IF (IERR.NE.0)  RETURN
                   ENDIF
               ENDIF
           CALL SETBVV(IERR)
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               CALL MP5SVV(IERR)
               IF (IERR.NE.0)  RETURN
               ENDIF
           IF (IERR.NE.0)  GOTO 900
           IF (MONC.GT.0) WRITE (IUMONC,*) ' Starting burn'
           GOTO 6
           ENDIF
c    check for gas exchange stage
       IF (IANR.GE.IEOR)  THEN
c        direct set for gas exchange
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               IF (4*(IANG/4).EQ.IANG)  THEN
                   CALL MP5SVV(IERR)
                   IF (IERR.NE.0)  RETURN
                   ENDIF
               ENDIF
           CALL SETGVV
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               CALL MP5SVV(IERR)
               IF (IERR.NE.0)  RETURN
               ENDIF
           IF (MONC.GT.0) WRITE (IUMONC,*) ' Starting gas exchange'
           GOTO 6
           ENDIF
c    get derivatives
       CALL DDTCVV(IERR)
       IF (IERR.NE.0)  GOTO 900
c    save initial pressure and volume for polytropic exponent calculation
       P1 = PC
       V1 = VC
       GOTO 50
c
c -- burn stage
c
c    check for end of burn
20     IF ((MU.LE.MUE).OR.(IANR.GE.IEOR)) THEN
c        burn over; set for expansion
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               IF (4*(IANG/4).EQ.IANG)  THEN
                   CALL MP5SVV(IERR)
                   IF (IERR.NE.0)  RETURN
                   ENDIF
               ENDIF
           CALL SETEVV
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               CALL MP5SVV(IERR)
               IF (IERR.NE.0)  RETURN
               ENDIF
           IF (MONC.GT.0) WRITE (IUMONC,*) ' Starting expansion'
c        initialize for expansion polytropic exponent computation
           NPEE = 0
           PEES = 0
c        save burn end angle
           JAEB = IANG
           GOTO 6
           ENDIF
c     get derivatives
       CALL DDTBVV(IERR)
       IF (IERR.NE.0)  GOTO 900
       GOTO 50
c
c --  expansion stage
c
c    check for exhaust valve open
30     IF (IANR.GE.IEOR)  THEN
c        exhaust open; set for gas exchange stage
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               IF (4*(IANG/4).EQ.IANG)  THEN
                   CALL MP5SVV(IERR)
                   IF (IERR.NE.0)  RETURN
                   ENDIF
               ENDIF
           CALL SETGVV
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               CALL MP5SVV(IERR)
               IF (IERR.NE.0)  RETURN
               ENDIF
             LSTG = 4
           IF (MONC.GT.0) WRITE (IUMONC,*) ' Starting gas exchange'
                 GOTO 6
                 ENDIF
c     get derivatives
       CALL DDTEVV(IERR)

       IF (IERR.NE.0)  GOTO 900
c    save initial pressure and volume for polytropic exponent calculation
       P1 = PC
       V1 = VC
       GOTO 50
c
c -- gas exchange stage

c    check exhaust valve
40     IF (IANG.EQ.IAEC)  THEN
c        close exhaust
           LEFL = 0
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               IF (4*(IANG/4).EQ.IANG)  THEN
                   CALL MP5SVV(IERR)
                   IF (IERR.NE.0)  RETURN
                   ENDIF
               ENDIF
           ENDIF
c    check for intake open
       IF (IANG.EQ.IAIO)  THEN
c        open intake
           LIFL = - 1
c         5-Line plots
           IF (KOP5C.NE.0)  THEN
               IF (4*(IANG/4).EQ.IANG)  THEN
                   CALL MP5SVV(IERR)
                   IF (IERR.NE.0)  RETURN
                   ENDIF
               ENDIF
           ENDIF
c    get derivatives (flows known)
       CALL DDTGVV(IERR)
       IF (IERR.NE.0)  GOTO 900

c    save initial pressure and volume for polytropic exponent calculation
       P1 = PC
       V1 = VC
       GOTO 50
C
c *  all stages come here
c
c    look for peak pressure
50     IF (PC.GT.PMAX)  THEN
           PMAX = PC
           JAMP = IANG
           ENDIF
c
c -- work completed at this crank angle
c
c    monitor output
        IF (MONC.GT.0)  WRITE (IUMONC,82) IANG,PC,VC,TC,MC,FIN,FEX
82      FORMAT(I5,6(1PE12.4))
c
c    check for end of cycle
       IF ((IANR.EQ.0).AND.(KTR.GT.0)) THEN
c        compute mixed exhaust state
           XPE = MPE/MEX
           HES = HEX/MEX
           CALL GTHMVV(XPE,HES,TE)
           CPE = CPMVV(XPE,TE)
           GAME = CPE/CVMVV(XPE,TE)
           RHOE = PE/PVMVV(XPE,TE)
c        set integer exhaust temperature
           JTEX = TE
c        compute error measures
           ERRT = (TC - TC0)/TC
           ERRP = (PC - PC0)/PC
           ERRV = (VELT - VELT0)/VELT
           ERRM = (MEX - MIN)/MIN
           ERRE = (WP + HEX + QC - HIN)/WP
c        compute volumetric efficiency
           NVOL = MIN/MDIA
           IF (KIMC.EQ.2) THEN
c            complete intake manifold mass flows and balance error
               CALL EMBCXX(RWI)
               ENDIF
           IF (KEMC.EQ.2) THEN
c            complete exhaust manifold mass flows and balance error
               CALL EMBCXX(RWE)
c            recalculate exhaust manifold k(T)-dependent constants
               CALL SKCEXX(TE)
c            reset exit entropy
               YE = PE/RHOE**RWE(IoK)
               ENDIF
c        set for compression stage, next cycle
           CALL SETCVV
           IF (MONC.GT.0) WRITE(IUMONC,*) ' End of cycle'

c        increment the completed cycle counter
           NCYC = NCYC + 1
c        exit
           RETURN
           ENDIF
c
c    increment counter
       KTR = KTR + 1
c
c  * RK2 advance of cylinder and manifolds
c
c    branch on cycle stage
100    GOTO (110,120,130,140), LSTG
c
c    compression stage
110    CALL ODEVV(ADVCVV,ALGCVV,CPYCVV,DDTCVV,IERR)
       IF (IERR.NE.0)  GOTO 900
c
c    contribution to polytropic exponent average
       DV = VC - V1
       IF (DV.NE.0)  THEN
           PECS = PECS - (V1 + VC)*(PC - P1)/((PC + P1)*DV)
           NPEC = NPEC + 1
           ENDIF
       GOTO 200
c
c    burn stage
120    TU0 = TU
       PU0 = PU
       CALL ODEVV(ADVBVV,ALGBVV,CPYBVV,DDTBVV,IERR)
       IF (IERR.NE.0)  GOTO 900
       GOTO 200
c
c    expansion stage
130    CALL ODEVV(ADVEVV,ALGEVV,CPYEVV,DDTEVV,IERR)
       IF (IERR.NE.0)  GOTO 900
c    contribution to polytropic exponent average
       DV = VC - V1
       IF (DV.NE.0)  THEN
           PEES = PEES - (V1 + VC)*(PC - P1)/((PC + P1)*DV)
           NPEE = NPEE + 1
           ENDIF
       GOTO 200
c
c    gas exchange stage
140    CALL ODEVV(ADVGVV,ALGGVV,CPYGVV,DDTGVV,IERR)
       IF (IERR.NE.0)  GOTO 900
       GOTO 200
c
c   Advance complete
c
200    IF (KIMC.EQ.2)  THEN
c        make the feeder cylinder-periodic
           CALL CPERXX(RWI,IWI)
c
c        Mach numbers
           DO 205 N=1,4
               IX = IWI(IoI) + 720*N
               MIM(N) = RWI(IoV+IX)/RWI(IoC+IX)
205            CONTINUE

c         debugging diagnostics
C          WRITE(*,208) IWI(IoI),
C    ;          (IWI(IoJUMP+IWI(IoI)+720*N),N=1,4),
C    ;          (IWI(IoLAG+N),N=1,4),MIM

           IF (MONM.GT.0)  WRITE(IUMONM,208) IWI(IoI),
     ;          (IWI(IoJUMP+IWI(IoI)+720*N),N=1,4),
     ;          (IWI(IoLAG+N),N=1,4),MIM
208        FORMAT (I4,' J',4I5,' L',4I5,' M',4F7.4)


           ENDIF
c
       IF (KEMC.EQ.2)  THEN
c        make the collector cylinder-periodic
           CALL CPERXX(RWE,IWE)
c        Mach numbers
           DO 215 N=1,4
               IX = IWE(IoI) + 720*N
               MEM(N) = RWE(IoV+IX)/RWE(IoC+IX)
215            CONTINUE


c        debugging diagnostics
C          WRITE(*,218) IWE(IoI),
C    ;          (IWE(IoJUMP+IWE(IoI)+720*N),N=1,4),
C    ;          (IWE(IoLAG+N),N=1,4),MEM
c
c        diagnostics
           IF (MONM.GT.0)  WRITE(IUMONM,218) IWE(IoI),
     ;          (IWE(IoJUMP+IWE(IoI)+720*N),N=1,4),
     ;          (IWE(IoLAG+N),N=1,4),MEM
218        FORMAT (I4,' J',4I5,' L',4I5,' M',4F7.4)
           ENDIF
c
       GOTO 6
c
c ** fatal failure
900    write (*,*), 'failure at iang',iang


       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  AMYZXX(NRK2,IERR)
c
c      Advances manifold Y and Z for RK2 step NRK2
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION   IW(50),RW(400)
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
c-----------------------------------------------------------------------------
c    manifold common blocks
       PARAMETER   (NMPTR = 96)
       PARAMETER   (NDRW = 21024)
       PARAMETER   (NDIW = 2904)
       DIMENSION   MPTR(NMPTR)
       DIMENSION   RWI(NDRW)
       DIMENSION   IWI(NDIW)
       DIMENSION   RWE(NDRW)
       DIMENSION   IWE(NDIW)
       COMMON /MPTRXX/ MPTR
       COMMON /RWIXX/  RWI
       COMMON /IWIXX/  IWI
       COMMON /RWEXX/  RWE
       COMMON /IWEXX/  IWE
c------------------------------------------------------------------------------
c    cylinder work array data
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
c------------------------------------------------------------------------------
c    intake
       IF (KIMC.EQ.2)  THEN
c        advance intake manifold angle, Y and Z
           CALL ADYZXX(RWI,IWI,NRK2,IERR)
           IF (IERR.NE.0) RETURN
           ENDIF
c
c    exhaust
       IF (KEMC.EQ.2)  THEN
c        advance exhaust manifold angle, Y and Z
           CALL ADYZXX(RWE,IWE,NRK2,IERR)
           IF (IERR.NE.0)  RETURN
           ENDIF
c
       IERR = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ADMSXX(NRK2,IERR)
c
c      Performs RK2 step NRK2 to advance the manifold states. Must be called
c      after Y and Z have been advanced.
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION   IW(50),RW(400)
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
c-----------------------------------------------------------------------------
c    manifold monitor
       LOGICAL OUT,test
       COMMON /MONMXX/IUMONM,MONM,OUT
c-----------------------------------------------------------------------------
c    manifold common blocks
       PARAMETER   (NMPTR = 96)
       PARAMETER   (NDRW = 21024)
       PARAMETER   (NDIW = 2904)
       DIMENSION   MPTR(NMPTR)
       DIMENSION   RWI(NDRW)
       DIMENSION   IWI(NDIW)
       DIMENSION   RWE(NDRW)
       DIMENSION   IWE(NDIW)
       COMMON /MPTRXX/ MPTR
       COMMON /RWIXX/  RWI
       COMMON /IWIXX/  IWI
       COMMON /RWEXX/  RWE
       COMMON /IWEXX/  IWE
c------------------------------------------------------------------------------
c    cylinder work array data
       EQUIVALENCE (YE  ,RW(180))
       EQUIVALENCE (PE  ,RW(204))
       EQUIVALENCE (PI  ,RW(205))
       EQUIVALENCE (YI  ,RW(349))
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
c------------------------------------------------------------------------------
c    manifold work array data
       EQUIVALENCE (IoAMR ,MPTR(40))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoY   ,MPTR(46))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoZ   ,MPTR(49))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoJUMP,MPTR(81))
       EQUIVALENCE (IoLAG ,MPTR(82))
       EQUIVALENCE (IoI   ,MPTR(91))
c-----------------------------------------------------------------------------
       REAL        MIM(4),MEM(4)
c-----------------------------------------------------------------------------
c  ** Intake manifold
c
       IF (KIMC.EQ.2)  THEN
c
c        inlet from atmosphere state 1
           IX = IWI(IoI) + 720
           XCFI = 1
           XCFO = 1
           XFEGR = 0
           XTREGR = 1
c        get state
           CALL FDRPXX(RWI,.FALSE.,RWI(IoY+IX),RWI(IoZ+IX),PI,YI,
     ;          RWI(IoAD+1),RWI(IoAMR+1),XCFI,XCFO,XFEGR,XTREGR,
     ;          RWI(IoC+IX),RWI(IoP+IX),RWI(IoV+IX),RWI(IoY+IX),
     ;          VVA,FLOW,IERR)
           IF (IERR.NE.0)  RETURN
c
c       exit state 4 was determined by FMSCVV
c
c        solve the junction for states 2 and 3
           CALL MJZYXX(RWI,IWI,NRK2,IERR)
           IF (IERR.GT.0) RETURN
c
c        advance acoustic delays
           CALL ADTDXX(RWI,IWI,NRK2)
c
c        increment flow sums
           IF (NRK2.EQ.2)  CALL IMFSXX(RWI,IWI)
c
           ENDIF
c
c ** Exhaust manifold
c
       IF (KEMC.EQ.2)  THEN
c
c        exhaust to atmosphere state 4
           IX = IWE(IoI) + 720*4
           XCFI = 1
           XCFO = 1
           XFEGR = 0
           XTREGR = 1
c        get state
           CALL FDRPXX(RWE,.TRUE.,RWE(IoY+IX),RWE(IoZ+IX),PE,YE,
     ;          RWE(IoAD+2),RWE(IoAMR+4),XCFI,XCFO,XFEGR,XTREGR,
     ;          RWE(IoC+IX),RWE(IoP+IX),RWE(IoV+IX),RWE(IoY+IX),
     ;          VVA,FLOW,IERR)
           IF (IERR.NE.0)  RETURN
c
c       entrance state 1 was determined by FMSCVV
c
c        solve the junction for states 2 and 3
           CALL MJZYXX(RWE,IWE,NRK2,IERR)
           IF (IERR.GT.0) RETURN
c
c        advance acoustic delays
           CALL ADTDXX(RWE,IWE,NRK2)
c
c        increment flow sums
           IF (NRK2.EQ.2)  CALL IMFSXX(RWE,IWE)
c
           ENDIF
c
c ** Manifold calculations completed
c
       IERR = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE AVPRVV(IANG,NRK2)
c
c      Computes the phase-average PC and RHOC at IANG for use in RK2 step NRK2
c------------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION   IW(50),RW(400)
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
c-----------------------------------------------------------------------------
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (RHOC,RW(169))
c-----------------------------------------------------------------------------
c    phase-averaged variables and weightings
       DIMENSION   PCAV(720),RHOCAV(720)
       COMMON      /AVCDVV/PCAV,RHOCAV,FCAVO,FCAVN,NCAV
c------------------------------------------------------------------------------
       IANGP = IANG + 1
       PC = FCAVO*PCAV(IANGP) + FCAVN*PC
       RHOC = FCAVO*RHOCAV(IANGP) + FCAVN*RHOC
       IF (NRK2.EQ.2)  THEN
c        reset averages
           PCAV(IANGP) = PC
           RHOCAV(IANGP) = RHOC
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CSUMVV
c
c      Computes summary output for last cycle
c------------------------------------------------------------------------------
c      The following must be in RW at call:
c          DISP    displacement; (piston area)*stroke, m**3
c          FEGR    exhaust gas recirculation fraction
c          MIN     mass inducted this cycle, kg
c          NVOL    mass of flow intake/mass at mixed charge density
c          PECS    sum of polytropic exponent contributions for compression
c          PEES    sum of polytropic exponent contributions for expansion
c          PMAX    peax pressure
c          QC      heat transferred to coolant this cycle, J
c          WP      total work done on piston this cycle, J
c------------------------------------------------------------------------------
c      The following must be in IW at call:
c          IAIG    ignition angle (gt 720 for motoring)
c          NPEC    number of polytropic exponent points for compression
c          NPEE    number of polytropic exponent points for expansion
c          JAMP    angle at maximum pressure, degrees
c          JAMP    crank angle at maximum pressure, degrees
c          JAEB    crank angle at end of burn, degrees
c          JTEX    mixed exhaust temperature at exit, K
c------------------------------------------------------------------------------
c      The following are available in RW on return:
c          IMEP    indicated work out/displacement, atm.
c          WPMF    indicated work out/mass of fuel
c          QPIW    heat transfer/indicated work
c          PEXC    polytropic exponent for compression
c          PEXE    polytropic exponent for expansion
c          PMAT    peak pressure, atm.
c------------------------------------------------------------------------------
c      The following are available in RW on return:
c          IAMP    crank angle at maximum pressure, degrees
c          IAEB    crank angle at end of burn, degrees
c          ITEX    mixed exhaust temperature at exit, K
c------------------------------------------------------------------------------
       REAL        IMEP,ISFC,MIN
c------------------------------------------------------------------------------
c    Properties data
       PARAMETER    (NSMAX = 50)
       DIMENSION    RMOLS(NSMAX)
       COMMON       /PROPVV/ PP,PR,RFT,RMOLS
c------------------------------------------------------------------------------
c    Work arrays
       DIMENSION IW(50),RW(400)
c------------------------------------------------------------------------------
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
c------------------------------------------------------------------------------
c  Locations in arrays
       EQUIVALENCE (QC  ,RW(5))
       EQUIVALENCE (WP  ,RW(6))
       EQUIVALENCE (MIN ,RW(11))
       EQUIVALENCE (DISP,RW(193))
       EQUIVALENCE (FEGR,RW(208))
       EQUIVALENCE (PECS,RW(241))
       EQUIVALENCE (PEES,RW(242))
       EQUIVALENCE (PMAX,RW(243))
       EQUIVALENCE (IMEP,RW(291))
       EQUIVALENCE (ISFC,RW(292))
       EQUIVALENCE (QPIW,RW(294))
       EQUIVALENCE (PEXC,RW(295))
       EQUIVALENCE (PEXE,RW(296))
       EQUIVALENCE (PMAT,RW(297))
       EQUIVALENCE (IAIG,IW(11))
       EQUIVALENCE (NPEC,IW(41))
       EQUIVALENCE (NPEE,IW(42))
       EQUIVALENCE (JAMP,IW(43))
       EQUIVALENCE (JAEB,IW(44))
       EQUIVALENCE (JTEX,IW(45))
       EQUIVALENCE (IAMP,IW(47))
       EQUIVALENCE (IAEB,IW(48))
       EQUIVALENCE (ITEX,IW(49))
c------------------------------------------------------------------------------
c    compute polytropic exponents
       IF (NPEC.NE.0)  THEN
               PEXC = PECS/NPEC
           ELSE
               PEXC = 0
           ENDIF
       IF (NPEE.NE.0)  THEN
               PEXE = PEES/NPEE
           ELSE
               PEXE = 0
           ENDIF
c    peak pressure
       PMAT = PMAX/101325.
c    saved integers
       IAMP = JAMP
       IAEB = JAEB
       ITEX = JTEX
c    other parameters
       QPIW = QC/WP
       FLOW = MIN*(1 - FEGR)
       IF (IAIG.LE.720)  THEN
               ISFC = FLOW*RFT/WP
           ELSE
               ISFC = 0
           ENDIF
       IMEP = WP/DISP
       RETURN
       END
c******************************************************************************
