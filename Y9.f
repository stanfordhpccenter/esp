******************************`***********************************************
c
c      Intake/exhaust initialization routines
c
c******************************************************************************
c
       SUBROUTINE SKCIXX(T)
c
c      Determines k(T)-dependent constants for the intake manfold calculation
c------------------------------------------------------------------------------
c    manifold work array
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RWI(NDRW)
       COMMON  /MPTRXX/    MPTR
       COMMON  /RWIXX/     RWI
c-----------------------------------------------------------------------------
c    pointers for the manifold work array
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoMBF ,MPTR(39))
       EQUIVALENCE (IoAMR ,MPTR(40))
       EQUIVALENCE (IoLD  ,MPTR(64))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoCF  ,MPTR(71))
       EQUIVALENCE (IoR   ,MPTR(80))
c------------------------------------------------------------------------------
c    engine work array
       PARAMETER (NDIMRW = 400)
       PARAMETER (NDIMIW = 50)
       DIMENSION RW(NDIMRW),IW(NDIMIW)
       COMMON /RWVV/ RW
       COMMON /IWVV/ IW
c------------------------------------------------------------------------------
       EQUIVALENCE (KIMC,IW(39))
c------------------------------------------------------------------------------
       DATA PI/3.1415928/
c------------------------------------------------------------------------------
c    determine specific heats at T
       CP = CPRVV(T)
       CV = CVRVV(T)
c    set k (=gamma)
       RWI(IoK) = CP/CV
c    check for intake manifold
       IF (KIMC.NE.2)  RETURN
c    gas constant
       RWI(IoR) = CP - CV
c    k-dependent constants
       CALL SETKXX(RWI)
c    get blockage factors
       RWI(IoMBF+1) = RW(301)
       RWI(IoMBF+2) = RW(302)
       RWI(IoMBF+3) = RW(303)
       RWI(IoMBF+4) = 0
c    get diameters and compute flow areass
       DIF = RW(308)
       DIR = RW(309)
       RWI(IoAD+1) = 0.25*PI*DIF**2
       RWI(IoAD+2) = 0.25*PI*DIR**2
c    compute blocakge areas
       RWI(IoAMR+1) = RWI(IoAD+1)*(1 - RWI(IoMBF+1))
       RWI(IoAMR+2) = RWI(IoAD+1)*(1 - RWI(IoMBF+2))
       RWI(IoAMR+3) = RWI(IoAD+2)*(1 - RWI(IoMBF+3))
       RWI(IoAMR+4) = RWI(IoAD+2)
c    get friction coefficients
       RWI(IoCF+1) = RW(304)
       RWI(IoCF+2) = RW(305)
c    get duct lengths
       RWI(IoLD+1) = RW(306)
       RWI(IoLD+2) = RW(307)
c    flow friction loss constants
       CALL SCFLXX(RWI)
       RETURN
       END
c*****************************************************************************
c
       SUBROUTINE SKCEXX(T)
c
c      Determines k(T)-dependent constants for the exhaust manfold calculation
c------------------------------------------------------------------------------
c    manifold work array
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RWE(NDRW)
       COMMON  /MPTRXX/    MPTR
       COMMON  /RWEXX/     RWE
c-----------------------------------------------------------------------------
c    pointers for the manifold work array
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoMBF ,MPTR(39))
       EQUIVALENCE (IoAMR ,MPTR(40))
       EQUIVALENCE (IoLD  ,MPTR(64))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoCF  ,MPTR(71))
       EQUIVALENCE (IoR   ,MPTR(80))
c------------------------------------------------------------------------------
c    engine work array
       PARAMETER (NDIMRW = 400)
       PARAMETER (NDIMIW = 50)
       DIMENSION RW(NDIMRW),IW(NDIMIW)
       COMMON /RWVV/ RW
       COMMON /IWVV/ IW
c------------------------------------------------------------------------------
       EQUIVALENCE (KEMC,IW(40))
       EQUIVALENCE (YE  ,RW(180))
       EQUIVALENCE (PE  ,RW(204))
c------------------------------------------------------------------------------
       DATA PI/3.1415928/
c------------------------------------------------------------------------------
c    determine specific heats at T
       CP = CPPVV(T)
       CV = CVPVV(T)
c    set k (=gamma)
       RWE(IoK) = CP/CV
c    check for exhaust manifold
       IF (KEMC.NE.2)  RETURN
c    gas constant
       RWE(IoR) = CP - CV
c    k-dependent constants
       CALL SETKXX(RWE)
c    get blockge factors
       RWE(IoMBF+1) = 0
       RWE(IoMBF+2) = RW(311)
       RWE(IoMBF+3) = RW(312)
       RWE(IoMBF+4) = RW(313)
c    get diameters and compute flow areass
       DER = RW(318)
       DEC = RW(319)
       RWE(IoAD+1) = 0.25*PI*DER**2
       RWE(IoAD+2) = 0.25*PI*DEC**2
c    compute blocakge areas
       RWE(IoAMR+1) = RWE(IoAD+1)
       RWE(IoAMR+2) = RWE(IoAD+1)*(1 - RWE(IoMBF+2))
       RWE(IoAMR+3) = RWE(IoAD+2)*(1 - RWE(IoMBF+3))
       RWE(IoAMR+4) = RWE(IoAD+2)*(1 - RWE(IoMBF+4))
c    get friction coefficients
       RWE(IoCF+1) = RW(314)
       RWE(IoCF+2) = RW(315)
c    get duct lengths
       RWE(IoLD+1) = RW(316)
       RWE(IoLD+2) = RW(317)
c    flow friction loss constants
       CALL SCFLXX(RWE)
       RETURN
       END
c*****************************************************************************
c
       SUBROUTINE SIMDXX(RW,IW,MAN,T0,P0)
c
c      Initializes the data for manifold MAN with stagnant fluid at T0, P0.
c      MAN = 1 for intake, MAN = 2 for exhaust.
c------------------------------------------------------------------------------
c    manifold work arrays
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 2904)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoTRKM,MPTR(16))
c------------------------------------------------------------------------------
       EQUIVALENCE (IoFTDO,MPTR(41))
       EQUIVALENCE (IoFTDN,MPTR(42))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoY   ,MPTR(46))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoW   ,MPTR(48))
       EQUIVALENCE (IoZ   ,MPTR(49))
       EQUIVALENCE (IoP0J ,MPTR(50))
       EQUIVALENCE (IoYJ  ,MPTR(51))
       EQUIVALENCE (IoDZF ,MPTR(55))
       EQUIVALENCE (IoCL  ,MPTR(56))
       EQUIVALENCE (IoVL  ,MPTR(57))
       EQUIVALENCE (IoYL  ,MPTR(58))
       EQUIVALENCE (IoG   ,MPTR(59))
       EQUIVALENCE (IoWCYC,MPTR(60))
       EQUIVALENCE (IoF   ,MPTR(61))
       EQUIVALENCE (IoDZC ,MPTR(63))
       EQUIVALENCE (IoLD  ,MPTR(64))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoTDP ,MPTR(66))
       EQUIVALENCE (IoTDM ,MPTR(67))
       EQUIVALENCE (IoTDPA,MPTR(68))
       EQUIVALENCE (IoTDMA,MPTR(69))
       EQUIVALENCE (IoDWC ,MPTR(70))
       EQUIVALENCE (IoCF  ,MPTR(71))
       EQUIVALENCE (IoCFCB,MPTR(72))
       EQUIVALENCE (IoCFCT,MPTR(73))
       EQUIVALENCE (IoCFA ,MPTR(74))
       EQUIVALENCE (IoFTEO,MPTR(75))
       EQUIVALENCE (IoFTEN,MPTR(76))
       EQUIVALENCE (IoYPAV,MPTR(77))
       EQUIVALENCE (IoZPAV,MPTR(78))
       EQUIVALENCE (IoJUMP,MPTR(81))
       EQUIVALENCE (IoLAG ,MPTR(82))
       EQUIVALENCE (IoJTAU,MPTR(84))
       EQUIVALENCE (IoJCAD,MPTR(85))
       EQUIVALENCE (IoNCYE,MPTR(86))
       EQUIVALENCE (IoNCYD,MPTR(87))
       EQUIVALENCE (IoNCYL,MPTR(88))
       EQUIVALENCE (IoMAN ,MPTR(90))
       EQUIVALENCE (IoI   ,MPTR(91))
       EQUIVALENCE (IoIV  ,MPTR(92))
       EQUIVALENCE (IoIP  ,MPTR(93))
       EQUIVALENCE (IoDZCO,MPTR(94))
       EQUIVALENCE (IoFAPO,MPTR(95))
       EQUIVALENCE (IoFAPN,MPTR(96))
c-----------------------------------------------------------------------------
c    engine work array (here called IWC)
       PARAMETER (NDIWC = 50)
       DIMENSION IWC(NDIWC)
       COMMON /IWVV/ IWC
       EQUIVALENCE (KIMC,IWC(39))
       EQUIVALENCE (KEMC,IWC(40))
c------------------------------------------------------------------------------
c    get starting index
       IBEX = IW(IoI)
c
c    branch on manifold:
c        set k(T)-dependent constants (loads duct lengths and areas)
c        get Pv
c        set number of runners
       IF (MAN.EQ.1) THEN
c            intake manifold
               IF (KIMC.EQ.2)  THEN
                       IW(IoMAN) = 1
                       IW(IoNCYL) = IWC(28)
                       CALL SKCIXX(T0)
                       PV0 = PVRVV(T0)
                   ELSE
                       IW(IoNCYL) = 0
                   ENDIF
           ELSE
c            exhaust manifold
               IF (KEMC.EQ.2)  THEN
                       IW(IoMAN) = 2
                       IW(IoNCYL) = IWC(29)
                       CALL SKCEXX(T0)
                       PV0 = PVPVV(T0)
                   ELSE
                       IW(IoNCYL) = 0
                   ENDIF
           ENDIF
c
c    check for no manifold
       IF (IW(IoNCYL).EQ.0)  RETURN
c
c    crank angle degrees between cylinders
       IW(IoJTAU) = 720/IW(IoNCYL)
c
c    stagnation conditions
       RHO0 = P0/PV0
       Y0 = P0/(RHO0**RW(IoK))
       C0S = RW(IoK)*P0/RHO0
       C0 = SQRT(C0S)
       Z0 = RW(IoTRKM)*C0
c
c    initialize duct parameters
       DO 45 J=1,2
c        identify end states
           N1 = 2*(J - 1) + 1
           N2 = N1 + 1
c        acoustic time delays
           RW(IoTDP+J) = RW(IoLD+J)/C0
           RW(IoTDM+J) = RW(IoTDP+J)
           RW(IoTDPA+J) = RW(IoTDP+J)
           RW(IoTDMA+J) = RW(IoTDM+J)
c        mass/area in duct
           W0 = RHO0*RW(IoLD+J)
c        W correction
           RW(IoDWC+J) = 0
c        data for each angle
           DO 43 I=1,720
c            end states
               DO 41 N=N1,N2
                   IX = I + 720*N
                   RW(IoC+IX) = C0
                   RW(IoV+IX) = 0
                   RW(IoY+IX) = Y0
                   RW(IoP+IX) = P0
                   RW(IoZ+IX) = Z0
                   IW(IoJUMP+IX) = 0
                   RW(IoW+IX) = 0
41                 CONTINUE
43             CONTINUE
           RW(IoW+IBEX+720*N1) = W0
45         CONTINUE
        DO 47 I=1,720
           RW(IoP0J+I) = P0
           RW(IoYJ+I) = Y0
           RW(IoYPAV+I) = Y0
           RW(IoZPAV+I) = Z0
47         CONTINUE
       DO 49 N=1,4
           RW(IoDZC+N) = 0
           RW(IoCL+N) = C0
           RW(IoVL+N) = 0
           RW(IoYL+N) = Y0
           RW(IoG+N) =  0
           RW(IoF+N) =  0
           RW(IoDZF+N) = 0
           IW(IoLAG+N) = 0
49         CONTINUE
       RW(IoDZCO) = 0
       IBEXM1 = IM1XX(IBEX)
       IW(IoIV+1) = IBEXM1
       IW(IoIV+3) = IBEXM1
       IW(IoIV+2) = IBEX
       IW(IoIV+4) = IBEX
       IW(IoIP+1) = IBEX
       IW(IoIP+3) = IBEX
       IW(IoIP+2) = IBEXM1
       IW(IoIP+4) = IBEXM1
c
c    counter initialization
       IW(IoNCYE) = 0
       IW(IoNCYD) = 0
       IW(IoJCAD) = IW(IoJTAU)

c    phase-averaging factors
       RW(IoFTEN) = 0.5
       RW(IoFTEO) = 1 - RW(IoFTEN)
       RW(IoFTDN) = 1.0/IW(IoNCYL)
       RW(IoFTDO) = 1 - RW(IoFTDN)
       RW(IoFAPN) = 1.0/IW(IoNCYL)
       RW(IoFAPO) = 1 - RW(IoFAPN)
c
       RETURN
       END
c*****************************************************************************
c
       SUBROUTINE MERRXX
c
c      Message to user about manifold error
c------------------------------------------------------------------------------
c    diagnostic monitor
       LOGICAL OUT
       COMMON /MONMXX/IUMONM,MON,OUT
c----------------------------------------------------------------------------
c    preserve the monitor file
       CLOSE (IUMONM)
       OPEN (IUMONM,FILE='MANTEMP.MON')
c    insruct user
       CALL WARNZZ('#','#An error occurred in the manifold '//
     ;   'calculation. File MANIFOLD.MON '//
     ;   'contains data needed to diagnose and fix the problem.  '//
     ;   'If your input data seem ok, please quit ESP now and save '//
     ;   'this file and your setup file in a separate directory '//
     ;   '(so that they will '//
     ;   'not be overwritten by your next ESP run) and contact# #'//
     ;   '    wcr@thermo.stanford.edu# #'//
     ;   'to arrange for transfer of these files so that we can '//
     ;   'make ESP work better.  Do not email these files!##')
       RETURN
       END
c*****************************************************************************

