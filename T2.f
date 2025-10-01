c******************************************************************************
c
c      Determines temperature dependent properties
c
c******************************************************************************
c
       SUBROUTINE SJTPRP(NSMAX,NIW,NRW,NSW,CHEM,IW,RW,SW)
c
c      Determines the temperature-dependent properties of the species.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c @        NSMAX       maximum number of species (dimension of CHEM)
c @        NIW         dimension of work array IW
c @        NRW         dimension of work array RW
c @        NSW         dimension of work array SW
c @        CHEM(J)     CHARACTER*8 name of Jth species
c          IW(I)       integer work array
c          RW(I)       REAL*8 work array
c @        SW(I)       REAL*4 work array holding species data file
c
c     Variables in the integer work array IW:
c @        KFRZ        constraint control = 3 for TPH specification
c @        JFS(J)   J  if the Jth species is the JFth file species
c                   0  if the Jth species is not in the data file
c @        NP          number of phases
c @        NSP(M)      number of species in Mth phase
c
c      Variables in the real work array RW:
c @        CVCJ        J/cal
c @        DCS(J)      density of condensed species, g/cm**3;  0 for gas
c @        DHF0(J)     enth. of form. at 298.15 K of Jth species (kcal/mol)
c #        G(J)        g(T,P)/RT for the Jth species
c #        HMH0(J)     enthalpy H-H(298.15) at 1 atm. for Jth species, kcal/mol
c @        P           system pressure, Pa (N/m**2)
c @        PATM        Pa, atm
c @        RGAS        gas constant, cal/mol-K
c #        S0(J)       entropy at (T,1 atm) for the Jth species, cal/mol-K
c @        TP(M)       temperature of the Mth phase, K
c @        WMS(J)      molal mass of the Jth species, g/mol.
c
c      Variables used only internally:
c          HX          enthalpy at (T,P), kcal/mol
c          RT          Rgas*T
c          RLP         Rgas*ln(P/Patm)
c          SX          entropy at (T,P), cal/mol-K
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
       REAL*4      SW
       CHARACTER*8 CHEM
c------------------------------------------------------------------------------
       DIMENSION   CHEM(NSMAX),IW(NIW),RW(NRW),SW(NSW),
     ;   IEPTR(80),ISPTR(10)
c------------------------------------------------------------------------------
c    pointers
       COMMON /SJEPTR/ IEPTR
       COMMON /SJSPTR/ ISPTR
       COMMON /SJTPTR/
     ;   IoKFRZ,IoCVCJ,IoPATM,IoRGAS,IoP,IoT,IoH,IoS,IoU,IoV,
     ;   IoWM,IoTP,IoDCS,IoDHF0,IoWMS,IoHMH0,IoS0,IoWMP,IoXM,IoYM
       EQUIVALENCE (IoNP,IEPTR(7)),(IoNSP,IEPTR(30)),
     ;   (IoG,IEPTR(57)),(IoJFS,ISPTR(6)),(IoALPH,IEPTR(33))
c------------------------------------------------------------------------------
       ZERO = 0
c
c    get parameters
       NP = IW(IoNP)
c
c     compute term
       RLP = RW(IoRGAS)*DLOG(RW(IoP)/RW(IoPATM))
c
c     check for TPH specification
       HFAC = 1.
       IF (IW(IoKFRZ).EQ.3) HFAC = HFAC - RW(IoALPH)
       HFAC = HFAC*1000.
c
       J2 = 0
       DO 29 M=1,NP
           J1 = J2 + 1
           J2 = J2 + IW(IoNSP+M)
           RT = RW(IoRGAS)*RW(IoTP+M)
           DO 19 J=J1,J2
c            get thermodynamic properties of the species
               IF (IW(IoJFS+J).LT.0)  THEN
                       CALL WARNZZ('@','@Program error; '//
     ;                   'nonfile species. Inform program author.@@')
                   ELSE
c                file species
                       CALL SJTIHS(NSMAX,NSW,SW,IW(IoJFS+J),
     ;                             RW(IoTP+M),RW(IoHMH0+J),RW(IoS0+J))
                   ENDIF
c
c            total H and correct H and S to mixture P for g/RT calculation
               HX = RW(IoHMH0+J) + RW(IoDHF0+J)
               SX = RW(IoS0+J)
               IF (RW(IoDCS+J).EQ.ZERO)  THEN
c                    gas species; correct entropy to mixture P
                       SX = SX - RLP
                   ELSE
c                    condensed species; correct enthalpy to mixture P
                       HX = HX +
     ;                      (RW(IoP) - RW(IoPATM))*1.0E-9*RW(IoWMS+J)/
     ;                                    (RW(IoDCS+J)*RW(IoCVCJ))
                   ENDIF
c            calculate g(T,P)/RT  (effective value for TPH specified)
               RW(IoG+J) = (HX*HFAC - RW(IoTP+M)*SX)/RT
19             CONTINUE
29         CONTINUE
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SJTIHS(NSMAX,NSW,SW,JF,T,HMH0J,S0J)
c
c      Interpolates in STANJAN data file to get temperature-dependent
c      properties for the JFth file species at T.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c @        NSMAX       nmaximum number of species in system or file
c @        NSW         dimension of work array SW
c @        SW(I)       REAL*4 work array
c @        JF          species index in the data file
c @        T           temperature, K
c #        HMH0J       enthalpy of the Jth species above 298.15 K, kcal/mol
c #        S0J         entropy of the Jth species, cal/mol-K
c
c      Variables in the work array SW:
c        Note:  T(L) = JANNAF tabulation temperature (200,298.15,300,...,6000)
c @        HMH0F(L,J)  H-H(298.15) for Jth file species at T(L)
c @        S0F(L,J)    S0 for Jth file species at T(L)
c
c      Variables used only internally
c #        C(L)        four-point interpolation coefficients
c          KTRL        call control
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8    (A-H,O-Z)
       REAL*4      SW
c------------------------------------------------------------------------------
c    Work array
       DIMENSION   SW(NSW)
c    Interpolation coefficients
       DIMENSION   C(4)
c------------------------------------------------------------------------------
c    JANNAF temperatures
       DATA    T200,T298,T6000/200.D0,298.15D0,6000.D0/
c    last temperature called
       DATA    KTRL /0/
c-----------------------------------------------------------------------------
c    set pointer base
           IH = 3*NSMAX + 60*(JF - 1)
           IS = IH + 60*NSMAX
c    select interpolation formula
       IF (T.GT.T6000)  THEN
c        two-point extrapolation above 6000 K
           TERM = (T - 6000.)/100.
           HMH0J = SW(IH+60) + TERM*(SW(IH+60) - SW(IH+59))
           S0J = SW(IS+60) + TERM*(SW(IS+60) - SW(IS+59))
           RETURN
           ENDIF
       IF (T.LT.T200)  THEN
c        extrapolation below 200 K
           TERM = (200. - T)/100.
           HMH0J = SW(IH+1) - TERM*(SW(IH+3) - SW(IH+1))
           S0J = SW(IS+1)*T/200.
           RETURN
           ENDIF
       IF (T.EQ.T298)  THEN
c        set precise values at 298.15 K
           HMH0J = SW(IH+2)
           S0J = SW(IS+2)
           RETURN
           ENDIF
c    four-point interpolation between 200 K and 6000 K
       IF (KTRL.EQ.0)  THEN
           TOLD = 0
           KTRL = 1
           ENDIF
       IF (T.NE.TOLD)  THEN
c        determine interpolation pivot index in the 60 entry table
           LP = T/100.0
           IF (LP.LT.3)    LP = 3
           IF (LP.GT.58)   LP = 58
           LT1 = LP - 1
           IF (LT1.EQ.2)  LT1 = 1
c        get interpolation coefficients
           TERM = (T - 100.*LP)/100.
           TERM1 = TERM - 1.
           TERM2 = TERM - 2.
           C(1) = - TERM*(TERM - 1.)*(TERM - 2.)/6.
           C(2) =  (TERM*TERM - 1.)*(TERM - 2.)/2.
           C(3) = - TERM*(TERM + 1.)*(TERM - 2.)/2.
           C(4) =  TERM*(TERM*TERM -1.)/6.
           TOLD = T
           ENDIF
       HMH0J = 0.D0
       S0J = 0.D0
       LT = LT1
       DO 9  L=1,4
           HMH0J = HMH0J + C(L)*SW(IH+LT)
           S0J = S0J + C(L)*SW(IS+LT)
           LT = LT + 1
           IF (LT.EQ.2)  LT = 3
9          CONTINUE
       RETURN
       END
c******************************************************************************
