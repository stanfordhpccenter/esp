c************************************************************************
c
c      Calculation at T,P
c
c******************************************************************************
c
       SUBROUTINE SJTP(NAMAX,NSMAX,NIW,NRW,NSW,
     ;         ATOM,CHEM,IW,RW,SW,KINIT)
c
c      Calculates  properties at given T,P.
c------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c @        NAMAX       maximum number of atoms
c @        NSMAX       maximum number of species
c @        NIW         dimension  work array IW
c @        NRW         dimension of work array IR
c @        NSW         dimension of work array IS
c @        ATOM(I)     CHARACTER*2 name of Ith atom
c @        CHEM(J)     CHARACTER*8 name of Jth species
c          IW(I)       integer work array
c          RW(I)       REAL*8 work array
c          SW(I)       REAL*4 work array
c @        KINIT       initialization control
c                     1 full initialization
c                     2 start with the last values if T,P are
c                       sufficiently close to their previous values
c                     3 start with the last values
c                     4 use mols and element potentials but recalculate
c                         mol fractions (final mixed phase calculation)
c
c      Variables in the integer work array IW:
c #        IB(K)       Kth independent atom
c #        KERR        error flag
c @        KFRZ      0 not frozen or constrained
c                    1 constrained equilibrium, only specified total mols
c                    2 constrained equilibrium, general linear constraints
c                    3 constrained equilibrium at specified T,P,H
c                   -1 composition frozen, phases at same T
c                   -2 composition frozen, phases at different T
c #        KTRE        SJEQLB pass counter
c @        NA          number of atom types
c #        NB          number of bases
c @        NP          number of phases
c @        NS          number of species
c @        NSP(M)      number of species in phase M
c
c      Variables in the real work array RW:
c @        CVCJ        Joules/cal
c @        DCS(J)      density of the Jth species, g/cm*3  (0 for gas)
c @        DHF0(J)     enthalpy of form. of Jth species at 298.15K, kcal/m
c #        ELAM(K)     element potential for kth independent atom
c #        H           mixture enthalpy, J/kg
c @        P           pressure, Pa
c #        PATM        Pa/atm
c #        PMOL(M)     mols of the Mth phase
c #        S           mixture entropy, J/kg-K
c #        SMOL(J)     mols of Jth species
c @        T           temperature, K
c @        TP(M)       temperature of Mth phase, K
c #        U           mixture internal energy, J/kg
c #        V           mixture volume m^3/kg
c #        WM          mixture molal mass, g/mol
c @        WMS(J)      molal mass of Jth species, g/mol
c #        X(J)        phase mol fraction of jth species
c #        XM(J)       mixture mol fraction of jth species
c #        YM(J)       mixture mass fraction of the Jth species
c-----------------------------------------------------------------------
       IMPLICIT    REAL*8      (A-H,O-Z)
       REAL*4      SW
       CHARACTER*2 ATOM
       CHARACTER*8 CHEM
c----------------------------------------------------------------------------
       DIMENSION   ATOM(NAMAX),CHEM(NAMAX),IW(NIW),RW(NRW),SW(NSW),
     ;   IEPTR(80)
c----------------------------------------------------------------------------
       COMMON /SJEPTR/ IEPTR
       COMMON /SJTPTR/
     ;   IoKFRZ,IoCVCJ,IoPATM,IoRGAS,IoP,IoT,IoH,IoS,IoU,IoV,
     ;   IoWM,IoTP,IoDCS,IoDHF0,IoWMS,IoHMH0,IoS0,IoWMP,IoXM,IoYM
       EQUIVALENCE (IoIB,IEPTR(9)),(IoKERR,IEPTR(1)),
     ;   (IoKTRE,IEPTR(3)),(IoKUMO,IEPTR(4)),(IoNA,IEPTR(5)),
     ;   (IoNB,IEPTR(6)),(IoNP,IEPTR(7)),(IoNS,IEPTR(8)),
     ;   (IoNSP,IEPTR(30)),(IoELAM,IEPTR(39)),(IoHUGE,IEPTR(32)),
     ;   (IoPMOL,IEPTR(62)),(IoSMOL,IEPTR(74)),(IoX,IEPTR(77))
c----------------------------------------------------------------------------
       IF (KCALL.EQ.0)  THEN
           TOLD = 0
           POLD = 0
           DTMAX = 50
           PRMAX = .5
           KCALL = 1
           ENDIF
       ZERO = 0
       TINY = 1/RW(IoHUGE)
c
c    get parameters
       NA = IW(IoNA)
       NP = IW(IoNP)
       NS = IW(IoNS)
c
c   set counter
      IW(IoKTRE) = 0
c
c -- compute constants
c
       RLP = RW(IoRGAS)*DLOG(RW(IoP)/RW(IoPATM))
c
c -- get species properties
c
c    check for different phase temperatures
       L = - 2
       IF (IW(IoKFRZ).EQ.L)  THEN
c            different temperatures; set a non-displayable temperature
               RW(IoT) = RW(IoHUGE)
           ELSE
c            set phase temperatures = T
               DO 3 M=1,NP
                   RW(IoTP+M) = RW(IoT)
3                  CONTINUE
           ENDIF
c
       CALL SJTPRP(NSMAX,NIW,NRW,NSW,CHEM,IW,RW,SW)
c
c -- check for frozen composition
c
       IF (IW(IoKFRZ).LT.0) THEN
c        frozen; get phase mols and mol fractions
           CALL SJIPMX(NSMAX,NIW,NRW,CHEM,IW,RW)
           NB = NA
           IW(IoNB) = NB
           DO 9 K=1,NB
c            set basis atoms
               IW(IoIB+K) = K
c            set to show stars at printout
               RW(IoELAM+K) = RW(IoHUGE)
9              CONTINUE
           GOTO 60
           ENDIF
c
c  -- check for final mixed-phase calculation
c
       IF (KINIT.EQ.4)  THEN
c        recalculate phase mols and mol fractions from mols
           CALL SJIPMX(NSMAX,NIW,NRW,CHEM,IW,RW)
           GOTO 60
           ENDIF
c
c -- equilibrium computation
c
       GOTO (10,20,30)  KINIT
c
c    set to initialize equilibrium solution
10     KIN = 1
       GOTO 50
c
c   decide if close enough to skip initialization
20     IF (DABS(RW(IoT)-TOLD).GT.DTMAX)  GOTO 10
       IF (DABS((RW(IoP)-POLD)/RW(IoP)).GT.PRMAX)  GOTO 10
c
c    initialize with the last phase mols and mol fractions
30     K = 1
c    estimate element potentials
       CALL SJIESL(NSMAX,NIW,NRW,CHEM,IW,RW,K,DUMMY,IDUMMY)
c    rare species problem probable if error
       IF (IW(IoKERR).NE.0)  THEN
           IW(IoKERR) = 4
           RETURN
           ENDIF
       KIN = 0
c
c    make the equilibrium calculation
50     CALL SJEQLB(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW,KIN)
       IF (IW(IoKERR).NE.0)  RETURN
c
c -- compute the mols, mixture mass fractions and mol fractions
c      and the molal masses of each phase
c
60     TMASS = 0
       TMOL = 0
       J2 = 0
       DO 63 M=1,NP
           J1 = J2 + 1
           J2 = J2 + IW(IoNSP+M)
           PMASS = 0
           DO 61 J=J1,J2
               RW(IoSMOL+J) = RW(IoX+J)*RW(IoPMOL+M)
               RW(IoYM+J) = RW(IoSMOL+J)*RW(IoWMS+J)
               PMASS = PMASS + RW(IoYM+J)
61             CONTINUE
           TMOL = TMOL + RW(IoPMOL+M)
           TMASS = TMASS + PMASS
           IF (RW(IoPMOL+M).GT.ZERO)  THEN
                   RW(IoWMP+M) = PMASS/RW(IoPMOL+M)
               ELSE
                   RW(IoWMP+M) = 0
               ENDIF
63         CONTINUE
       IF ((TMASS.LE.ZERO).OR.(TMOL.LE.ZERO))  THEN
           IW(IoKERR) = 1
           RETURN
           ENDIF
       DO 65 J=1,NS
           RW(IoYM+J) = RW(IoYM+J)/TMASS
           RW(IoXM+J) = RW(IoSMOL+J)/TMOL
65         CONTINUE
       RW(IoWM) = TMASS/TMOL
c
c -- mixture property calculation
c
       RW(IoH) = 0
       RW(IoS) = 0
       RW(IoV) = 0
       J2 = 0
       DO 79 M=1,NP
           J1 = J2 + 1
           J2 = J2 + IW(IoNSP+M)
           DO 77 J=J1,J2
c            get H and S at T and 1 atm
               HX = RW(IoHMH0+J) + RW(IoDHF0+J)
               SX = RW(IoS0+J)
c            correct entropy for mol fraction
               IF (RW(IoX+J).GT.TINY)
     ;           SX = SX - RW(IoRGAS)*DLOG(RW(IoX+J))
c            check species type
               IF (RW(IoDCS+J).EQ.ZERO)  THEN
c                    gas; correct entropy for pressure
                       SX = SX - RLP
c                    calculate partial molal volume,  m**3/mol
                       VX = RW(IoRGAS)*RW(IoCVCJ)*RW(IoTP+M)/RW(IoP)
                   ELSE
c                    condensed species; correct enthalpy for pressure
                      HX = HX +
     ;                 (RW(IoP) - RW(IoPATM))*1.0E-9*RW(IoWMS+J)/
     ;                                    (RW(IoDCS+J)*RW(IoCVCJ))
c                     calculate partial molal volume,  m**3/mol
                      VX = RW(IoWMS+J)*1.0E-6/RW(IoDCS+J)
                   ENDIF
               RW(IoH) = RW(IoH) + RW(IoSMOL+J)*HX
               RW(IoS) = RW(IoS) + RW(IoSMOL+J)*SX
               RW(IoV) = RW(IoV) + RW(IoSMOL+J)*VX
77             CONTINUE
79         CONTINUE
c
c    here H is in kcal, S is in cal, and V is in m**3
c    convert to J/kg, J/kg-K, and m**3/kg, the basic SI units
c
       RW(IoH) = RW(IoH)*RW(IoCVCJ)*1.E6/TMASS
       RW(IoS) = RW(IoS)*RW(IoCVCJ)*1.E3/TMASS
       RW(IoV) = RW(IoV)*1000./TMASS
c
c    compute U, J/kg
       RW(IoU) = RW(IoH) - RW(IoP)*RW(IoV)
c
c    save last  T and P
       TOLD = RW(IoT)
       POLD = RW(IoP)
c
c    exit
       IW(IoKERR) = 0
       RETURN
c
       END
c**************************************************************************
