c*****************************************************************************
c
c      Initialization routine
c
c******************************************************************************
c
       SUBROUTINE SJINIT(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW)
c
c      Initialization routine
c------------------------------------------------------------------------------
c      Identifies the linearly independent atoms in the allowed species
c      set and distributes the atom populations to a set of system
c      base species, which are expected to be the dominant species.
c      This is accomplished by an approximate Gibbs function minimization
c      in which the ln(X) term is neglected, giving rise to a linear
c      minimization problem solved by the standard simplex process (SJISMP).
c      There will be one base species for each linearly independent atom.
c      A specification of impossible populations is identified by SJISMP,
c      in which case the initialization is terminated.
c
c      The simplex multipliers emerging at the end of this process are
c      element potentials corresponding to the prescribed mol fractions of
c      the base species species. Using these potentials, the mol fractions
c      estimated for all other species are less than unity. Thus, they form
c      a first guess for the element potentials.
c
c      The main analysis loop begins with a set of trial mols from which
c      the mol fractions and phase mols are calculated (SJIPMX).
c
c      The element potentials are then estimated (SJIESL) with iteration
c      until a consistent set has been found for the given basis set.
c
c      The conditioned population equations are examined in SJIESL to see if
c      any base is dominated by another species not a balancer. If there
c      is dominance, then a base change is made (SJISMP) and the element
c      potentials are reestimated using SJIESL.
c
c      Once a consistent set of basis species and potentials have been found,
c      the mol fractions of balancing species are estimated, and atoms are
c      redistributed to seek these target mol fractions (SJISRD).
c
c      These processes are repeated until sufficient consistency is obtained.
c------------------------------------------------------------------------------
c      On return:
c
c          Successful initialization:
c
c              KERR = 0
c
c          Problems encountered:
c
c              KERR = 1 program problems (singularities)
c                     2 unable to initialize
c                     3 impossible populations
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c @        NAMAX       maximum number of atoms
c @        NSMAX       maximum number of species
c @        NIW         dimension of work array IW
c @        NRW         dimension of work array IR
c @        ATOM(I)     CHARACTER*2 name of Ith atom
c @        CHEM(J)     CHARACTER*8 name of Jth species
c          IW(I)       integer work array
c          RW(I)       REAL*8 work array
c
c      Variables in the integer work array IW:
c #        IB(K) = I   if the Ith atom is the Kth independent system atom
c #        JB(K) = J   if the Jth system species is the Kth base species
c -        JBA(K) = J  JB(K) after last redistribution
c -        JBB(K) = J  JB(K) for the best basis
c -        JBO(K) = J  JB(K) before redistribution
c #        KB(J)       basis control (SJISMP)
c @                 -1 if the Jth species is a base freed for SJISRD call
c                    0 if the Jth species is not a base
c                    1 if the Jth species is a base
c                    2 if the Jth species is a balancing species
c                    4 if the Jth species is excluded
c          KBA(J)      KB(J) after last redistribution
c -        KBB(J)      KB(J) for the best basis
c -        KBO(J)      KB(J) before the last distribution
c #        KERR        error flag
c @        KMON        monitor control
c @        KUMO        output unit for monitor
c @        N(I,J)      number of Ith atoms in Jth system species
c @        NA          number of atom types
c #        NB          number of base species (independent atoms)
c @        NP          number of phases
c @        NS          number of species
c @        NSP(M)      number of species in the Mth phase

c      Variables in the real work array RW:
c -        ELMB(K)     ELAM(K) for best basis
c #        ELAM(K)     element potential for the Kth independent system atom
c @        G(J)        g(T,P)/RT  for the Jth species
c @        HUGE        large machine number
c @        PA(I)       population of the Ith atoms
c #        PC(K)       Kth conditioned population
c #        PMOL(M)     mols of Mth phase
c -        SMLA(J)     SMOLS(J) after last redistribution
c -        SMLB(J)     SMOLS(J) for best basis
c -        SMLO(J)     SMOLS(J) for before redistribution
c #        SMOL(J)     mols of Jth species
c #        X(J)        mols fraction of the Jth species (in its phase)
c -        XO(J)       X(J) of last distribution
c
c      Variables used only internally:
c          DOM         maximum dominance of a base by a secondary species
c          DOMB        DOM for best basis
c          KCCPC       conditioning matrix calculation control
c          KEX30       exit control
c          KRED        redistribution control (0=don't, 1=maybe)
c          KTRB        basis change iteration counter
c          KTRR        redistribution counter
c          XRMIN       fractional change in X for initiation convergence
c          ZERO        0 in calculation precision
c------------------------------------------------------------------------------
       IMPLICIT        REAL*8  (A-H,O-Z)
       CHARACTER*2     ATOM,ELECT
       CHARACTER*8     CHEM
       LOGICAL*2       SJICKR
c------------------------------------------------------------------------------
       DIMENSION   ATOM(NAMAX),CHEM(NSMAX),IW(NIW),RW(NRW)
c------------------------------------------------------------------------------
c    pointers
       COMMON /SJEPTR/
     ;   IoKERR,IoKMON,IoKTRE,IoKUMO,IoNA,IoNB,IoNP,IoNS,IoIB,IoIBO,
     ;   IoJB,IoJBAL,IoJBA,IoJBB,IoJBO,IoJBX,IoJS2,IoKB,IoKB2,IoKBA,
     ;   IoKBB,IoKBO,IoKPC,IoKPCX,IoLB2,IoMPA,IoMPJ,IoN,LoN,IoNSP,
     ;   IoFRND,IoHUGE,IoR1,IoR2,IoR3,IoA,LoA,IoB,LoB,IoBBAL,
     ;   IoCM,LoCM,IoD,LoD,IoDC,LoDC,IoDPML,IoDLAM,IoDLAY,IoE,
     ;   LoE,IoEEQN,IoELAM,IoELMA,IoELMB,IoF,IoG,IoHA,IoHC,IoPA,
     ;   IoPC,IoPMOL,IoQ,LoQ,IoQC,LoQC,IoRC,LoRC,IoRL,IoRP,
     ;   IoSMOA,IoSMOB,IoSMOO,IoSMOL,IoSMUL,IoW,IoX,IoXO,IoY,IoZ
c------------------------------------------------------------------------------
c    Electron designation
       DATA ELECT /'e-'/
c------------------------------------------------------------------------------
c    set comparison constants in calculation precision
       ZERO = 0
       ONE = 1
c
c    get parameters
       NA = IW(IoNA)
       NP = IW(IoNP)
       NS = IW(IoNS)
       KMON = IW(IoKMON)
       KUMO = IW(IoKUMO)
c
c    monitor
       IF (KMON.GT.1)  WRITE (KUMO,1)
1      FORMAT (/' Initialization:')
c
c    clear X and allow all species
       DO 3 J=1,NS
           RW(IoX+J) = 0
           IW(IoKB+J) = 0
3          CONTINUE
c
c    check for zero atom populations and exclude such species
       DO 9 I=1,NA
           IF (RW(IoPA+I).EQ.ZERO)  THEN
               IF (ATOM(I).NE.ELECT)  THEN
                   IF (KMON.GT.1)  WRITE (KUMO,4) ATOM(I)
4                  FORMAT (/' Eliminating species containing ',
     ;                       'absent ',A,' atoms:')
                   DO 7 J=1,NS
                       IF (IW(IoN+I+LoN*J).NE.0)  THEN
                           IF (KMON.GT.1)  WRITE (KUMO,6) CHEM(J)
6                          FORMAT (5X,A)
                           IW(IoKB+J) = 4
                           ENDIF
7                      CONTINUE
                   ENDIF
               ENDIF
9          CONTINUE
c
c ** simplex initialization
c
c    simplex distribution
10     L = 0
       CALL SJISMP(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW,L)
       IF (IW(IoKERR).NE.0)  RETURN
c    check for no basis for anything
       NB = IW(IoNB)
       IF (NB.EQ.0)  THEN
           IW(IoKERR) = 3
           RETURN
           ENDIF
c
c    initialize counters and controls
       KTRR = 0
       KEX30 = 1
       KCCPC = 0
c
c ** loop point after redistribution
c
c    set rebasing counter
20     KTRB = 0
       DOMB = RW(IoHUGE)
c
c ** loop point after rebasing
c
30     KTRB = KTRB + 1
c
c    calculate the conditioning matrix and conditioned populations
       CALL SJICPC(NIW,NRW,IW,RW,KCCPC)
       IF (IW(IoKERR).NE.0)  RETURN
       KCCPC = 1
c
c    calculate phase mols and mol fractions
       CALL SJIPMX(NSMAX,NIW,NRW,CHEM,IW,RW)
c
c    select exit
       GOTO (32,60,90), KEX30
c
c    estimate ELAMs and dominance
32     L = 0
       CALL SJIESL(NSMAX,NIW,NRW,CHEM,IW,RW,L,DOM,JW)
c
c    check for species exclusion
       IF (IW(IoKERR).EQ.4)  GOTO 10
c    check for ESTEP error
       IF (IW(IoKERR).NE.0)  RETURN
c
c    check for needed rebasing
       IF (JW.NE.0)  THEN
c        check vs best basis
           IF (DOM.LT.DOMB)  THEN
c            save base as best
               DO 33 J=1,NS
                   IW(IoKBB+J) = IW(IoKB+J)
                   RW(IoSMOB+J) = RW(IoSMOL+J)
33                 CONTINUE
               DO 35 K=1,NB
                   IW(IoJBB+K) = IW(IoJB+K)
                   RW(IoELMB+K) = RW(IoELAM+K)
35                 CONTINUE
               DOMB = DOM
               ENDIF
c        check rebasing count
           IF (KTRB.GT.NB)  THEN
c            excessive rebasing attempts; use best
               GOTO 50
               ENDIF
c        install JW as a base
           L = JW + 10
           CALL SJISMP(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW,L)
           IF (IW(IoKERR).NE.0)  RETURN
c        check for repeat of the best basis
           IF (KTRB.GT.1)  THEN
               DO 37 J=1,NS
                   L = IW(IoKBB+J)
                   IF (L.LT.0) L = - L
                   IF ((IW(IoKB+J).EQ.1).AND.(L.NE.1))  GOTO 40
37                 CONTINUE
c            rebasing cycling
               GOTO 50
               ENDIF
c
c       try the new basis
40         KEX30 = 1
           GOTO 30
c
c       reinstall the best basis
50         DO 51 J=1,NS
               IW(IoKB+J) = IW(IoKBB+J)
               RW(IoSMOL+J) = RW(IoSMOB+J)
51             CONTINUE
           DO 53 K=1,NB
               RW(IoELAM+K) = RW(IoELMB+K)
               IW(IoJB+K) = IW(IoJBB+K)
53             CONTINUE
           DOM = DOMB
           KEX30 = 2
           GOTO 30
c
           ENDIF
c
c ** redistribution check
c
c    set controls
60     KRED = 0
       KEX30 = 1
c
c    set targets for balancers
       DO 65 J=1,NS
           IF (IW(IoKB+J).EQ.2)  THEN
c        set redistribution to balancer
c            flag possible redistribution
               IF (KRED.EQ.0)  KRED = 1
c            estimate mol fraction
               SUM = - RW(IoG+J)
               DO 63 K=1,NB
                   I = IW(IoIB+K)
                   SUM = SUM + RW(IoELAM+K)*IW(IoN+I+LoN*J)
63                 CONTINUE
                RW(IoX+J) = SJUEXP(SUM)
c            check for valid value
               IF (RW(IoX+J).GE.ONE)  THEN
c                abort redistribution
                   IF (KMON.GT.1)  WRITE (KUMO,64) CHEM(J)
64                 FORMAT (/' Redistribution to x>1 for ',A,
     ;                      ' not allowed; attempting solution.')
                   GOTO 90
                   ENDIF
               ENDIF
65         CONTINUE
c
c    check for no targets
       IF (KRED.EQ.0)  THEN
           IF (KMON.GT.1)  WRITE (KUMO,66)
66         FORMAT (/' Initializer distribution converged.')
           GOTO 90
           ENDIF
c
c    check for repeating redistribution
       IF (KTRR.GT.0)  THEN
c        check repeat of the last distribution
       IF (SJICKR(NIW,NRW,IW,RW))  THEN
c            repeating
               IF (KMON.GT.1)  WRITE (KUMO,66)
c            check for rebasing since the last redistribution
               IF (KTRB.NE.1)  THEN
c                rebased on this pass; restore last redistribution
                   DO 67 J=1,NS
                       IW(IoKB+J) = IW(IoKBA+J)
                       RW(IoSMOL+J) = RW(IoSMOA+J)
67                     CONTINUE
                   DO 69 K=1,NB
                       IW(IoJB+K) = IW(IoJBA+K)
                       RW(IoELAM+K) = RW(IoELMA+K)
69                     CONTINUE
c                go restore phase mols, conditioning
                   KEX30 = 3
                   GOTO 30
                   ENDIF
c            go exit
               GOTO 90
               ENDIF
           ENDIF
c
c ** redistribution
c
c    save distribution
70     DO 71 J=1,NS
           RW(IoXO+J) = RW(IoX+J)
           IW(IoKBO+J) = IW(IoKB+J)
           RW(IoSMOO+J) = RW(IoSMOL+J)
71         CONTINUE
       DO 73 K=1,NB
           IW(IoJBO+K) = IW(IoJB+K)
73         CONTINUE
c
c    monitor
       IF (KMON.GT.1)  THEN
           DO 75 J=1,NS
               IF (IW(IoKB+J).EQ.2) WRITE (KUMO,74) RW(IoX+J),CHEM(J)
74                 FORMAT (/' Redistributing atoms seeking',
     ;                      ' mol fraction =',E16.8,' for ',A)
75             CONTINUE
           ENDIF
c
c    redistribute
       CALL SJISRD(NAMAX,NIW,NRW,IW,RW)
c    check for failed redistribution
       IF (IW(IoKERR).NE.0) THEN
c        attempt solution with distribution before attempted redistribution
           DO 81 J=1,NS
               RW(IoSMOL+J) = RW(IoSMOO+J)
               IW(IoKB+J) = IW(IoKBO+J)
81             CONTINUE
           DO 83 K=1,NB
               IW(IoJB+K) = IW(IoJBO+K)
83             CONTINUE
           IF (KMON.GT.1)  WRITE (KUMO,84)
84         FORMAT (/' Unable to redistribute; attempting solution.')
           GOTO 90
           ENDIF
c
c    check count
       KTRR = KTRR + 1
       IF (KTRR.GT.NB*NP)  THEN
           IF (KMON.GT.1)  WRITE (KUMO,86)
86         FORMAT (/' Excessive iterations in initializer;',
     ;              ' attempting solution.')
           GOTO 90
           ENDIF
c
c    save redistribution
       DO 87 K=1,NB
           IW(IoJBA+K) = IW(IoJB+K)
           RW(IoELMA+K) = RW(IoELAM+K)
87         CONTINUE
       DO 89 J=1,NS
           IW(IoKBA+J) = IW(IoKB+J)
           RW(IoSMOA+J) = RW(IoSMOL+J)
89         CONTINUE
c
c    go try new distribution
       GOTO 20
c
c ** exit
c
c    clear flags
90     DO 91 J=1,NS
           IoKBJ = IoKB+J
           IF (IW(IoKBJ).LT.0)  IW(IoKBJ) = - IW(IoKBJ)
           IF (IW(IoKBJ).EQ.2)  IW(IoKBJ) = 0
91         CONTINUE
c
c    monitor
       If (KMON.GT.1)  THEN
           WRITE (KUMO,92)
92         FORMAT (/' Independent species used as basis set:')
           DO 95 K=1,NB
               J = IW(IoJB+K)
               WRITE (KUMO,94) CHEM(J)
94             FORMAT (5X,A)
95             CONTINUE
           ENDIF
c
c    normal return
       IW(IoKERR) = 0
       RETURN
c
       END
c******************************************************************************
c
       LOGICAL*2 FUNCTION SJICKR(NIW,NRW,IW,RW)
c
c      Tests to see if species, bases, and targets are the same as
c      for previous case KBO,XO to within XRMIN < X/XO < 1/XRMIN
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c
c @        NIW         dimension of work array IW
c @        NRW         dimension of work array IR
c          IW(I)       integer work array
c          RW(I)       REAL*8 work array
c
c      Variables in the integer work array IW:
c @        JB(K) = J   if the Jth species is the Kth base species
c @        KB(J)       basis control
c                   -1 if the Jth species is a base freed for SJISRD call
c                    0 if the Jth species is not a base
c                    1 if the Jth species is a base
c                    2 if the Jth species is a balancing species
c                    4 if the Jth species is excluded
c @        KBO(J)      old KB(J)
c @        KMON        monitor control
c @        KUMO        output unit for monitor
c @        NS          number of species
c
c      Variables in the real work array RW:
c @        X(J)        mol fraction of Jth species (here targets)
c @        XO(J)       old X(J)
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
c------------------------------------------------------------------------------
       DIMENSION   IW(NIW),RW(NRW),IEPTR(80)
c------------------------------------------------------------------------------
c   pointers
       COMMON /SJEPTR/ IEPTR
       EQUIVALENCE (IoJB,IEPTR(11)),(IoKB,IEPTR(18)),(IoKBO,IEPTR(22)),
     ;   (IoKMON,IEPTR(2)),(IoKUMO,IEPTR(4)),(IoNS,IEPTR(8)),
     ;   (IoX,IEPTR(77)),(IoXO,IEPTR(78))
c------------------------------------------------------------------------------
c    get parameters
       KMON = IW(IoKMON)
       KUMO = IW(IoKUMO)
       NS = IW(IoNS)
c
c    set range constants in calculation precision
       XRMIN = 0.8
       XRMAX = 1.2
       ZERO = 0
c
c    compare
       DO 9 J=1,NS
c       is species included this time (KB = -1,1,2 ne 0,4)?
           L = IW(IoKB+J)
           M = IW(IoKBO+J)
           IF ((L.NE.0).AND.(L.NE.4))  THEN
c                included; must repeat if omitted before (KBO=0,4)
                   IF ((M.EQ.0).OR.(M.EQ.4))  GOTO 70
               ELSE
c                not included; must repeat if included before (KBO=-1,1,2)
                   IF ((M.NE.0).AND.(M.NE.4))  GOTO 70
               ENDIF
c        check for target (balancing species)
           IF (L.EQ.2)  THEN
               IF (RW(IoX+J).EQ.ZERO)  GOTO 70
c            check target for essential repeat
               TERM = DABS(RW(IoXO+J)/RW(IoX+J))
               IF ((TERM.LT.XRMIN).OR.(TERM.GT.XRMAX)) GOTO 70
               ENDIF
9          CONTINUE
c    same
       SJICKR = .TRUE.
       RETURN
c
c    not same
70     SJICKR = .FALSE.
       RETURN
c
       END
c******************************************************************************
c
       SUBROUTINE SJIPMX(NSMAX,NIW,NRW,CHEM,IW,RW)
c
c      Calculates phase mols and mol fractions from a set of mols.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c @        NSMAX       maximum number of species
c @        NIW         dimension of work array IW
c @        NRW         dimension of work array IR
c @        CHEM(J)     CHARACTER*8 name of Jth species
c          IW(I)       integer work array
c          RW(I)       REAL*8 work array
c
c      Variables in the integer work array IW:
c @        KMON        monitor control
c @        KUMO        output unit for monitor
c @        NP          number of phases
c @        NS          number of species
c @        NSP(M)      number of species in Mth phase
c
c      Variables in the real work array RW:
c #        PMOL(M)     total mols in the Mth phase
c @        SMOL(J)     number of mols of the Jth species
c #        X(J)        mol fraction of the Jth species (in its phase)
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
       CHARACTER*8 CHEM
c------------------------------------------------------------------------------
       DIMENSION   CHEM(NSMAX),IW(NIW),RW(NRW),IEPTR(80)
c------------------------------------------------------------------------------
c    pointers
       COMMON /SJEPTR/IEPTR
       EQUIVALENCE (IoKMON,IEPTR(2)),(IoKUMO,IEPTR(4)),(IoNP,IEPTR(7)),
     ;   (IoNS,IEPTR(8)),(IoNSP,IEPTR(30)),
     ;   (IoPMOL,IEPTR(62)),(IoSMOL,IEPTR(74)),(IoX,IEPTR(77))
c------------------------------------------------------------------------------
c    comparison constant in machine precision
       DATA   ZERO/0.D0/
c------------------------------------------------------------------------------
c    get parameters
       KMON = IW(IoKMON)
       KUMO = IW(IoKUMO)
       NP = IW(IoNP)
       NS = IW(IoNS)
c
c    analyze all phases
       J2=0
       DO 15 M=1,NP
c        analyze phase M
           J1 = J2 + 1
           J2 = J2 + IW(IoNSP+M)
c        compute total mols
           IoPMOM = IoPMOL + M
           RW(IoPMOM) = 0
           DO 11 J = J1,J2
               RW(IoPMOM) = RW(IoPMOM) + RW(IoSMOL+J)
11             CONTINUE
c        compute mol fractions
           IF (RW(IoPMOM).NE.ZERO)  THEN
                   DO 13 J=J1,J2
                       RW(IoX+J) = RW(IoSMOL+J)/RW(IoPMOM)
13                     CONTINUE
               ELSE
                   DO 14 J=J1,J2
                       RW(IoX+J) = 0
14                     CONTINUE
               ENDIF
15         CONTINUE
c
c    monitor
       IF (KMON.GT.1)  THEN
           WRITE (KUMO,20)
20         FORMAT (/' Estimated distribution:')
           WRITE (KUMO,21) (M,RW(IoPMOL+M),M=1,NP)
21         FORMAT ('   Phase ',I2,' mols =',1PE12.5)
           DO 29 J1=1,NS,6
               J2 = J1 + 5
               IF (J2.GT.NS)  J2 = NS
               WRITE (KUMO,22) (CHEM(J),J=J1,J2)
22             FORMAT  (6X,6(4X,A8))
               WRITE (KUMO,23) (RW(IoSMOL+J),J=J1,J2)
23             FORMAT  ('   mols:',6(1PE12.5))
               WRITE (KUMO,24) (RW(IoX+J),J=J1,J2)
24             FORMAT  ('      X:',6E12.5)
29             CONTINUE
           ENDIF
c
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SJICPC(NIW,NRW,IW,RW,KOP)
c
c      Computes the conditioning matrix and conditioned populations
c------------------------------------------------------------------------------
c      The conditioning matrix satisfies
c
c          sum{CM(M,K)*N(I(K),J(L))} = delta(M,L)
c            independent atoms K
c------------------------------------------------------------------------------
c
c      For KOP = 0  makes the calculation and saves JB(K) in JBX(K)
c
c      For KOP > 0  only makes calculation if JB() ne JBX()
c
c      On return:
c
c          Successful:
c              CM and PC loaded
c              KERR = 0
c
c          Problems:
c              KERR = 1
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c @        NIW         dimension of work array IW
c @        NRW         dimension of work array IR
c          IW(I)       integer work array
c          RW(I)       REAL*8 work array
c @        KOP         option control (see above)
c
c      Variables in the integer work array IW:
c @        IB(K) = I   if the Ith atom is the Kth independent atom
c @        JB(K) = J   if the Jth species is the Kth base species
c @#       JBX(K)      JB of the previous call
c #        KERR        error flag
c @        N(I,J)      number if Ith atoms in the Jth species
c @        NB          number of independent atoms
c
c      Variables in the real work array RW:
c -        A(I,J)      work array
c #        CM(I,J)     conditioning matrix
c @        PA(I)       population of I atoms
c #        PC(K)       Kth conditioned population
c -        W(I)        work array
c
c      Variables used only internally:
c @#       NBX         NB at last call
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
c------------------------------------------------------------------------------
       DIMENSION   IW(NIW),RW(NRW),IEPTR(80)
c------------------------------------------------------------------------------
c    pointers
       COMMON /SJEPTR/ IEPTR
       EQUIVALENCE (IoIB,IEPTR(9)),(IoJB,IEPTR(11)),(IoJBX,IEPTR(16)),
     ;  (IoKERR,IEPTR(1)),
     ;  (IoN,IEPTR(28)),(LoN,IEPTR(29)),(IoNB,IEPTR(6)),
     ;  (IoA,IEPTR(36)),(LoA,IEPTR(37)),
     ;  (IoCM,IEPTR(41)),(LoCM,IEPTR(42)),
     ;  (IoPA,IEPTR(60)),(IoPC,IEPTR(61)),(IoW,IEPTR(76))
c------------------------------------------------------------------------------
c    get parameters
       NB = IW(IoNB)
c
c    check option
       IF (KOP.GT.0)  THEN
           DO 5 K=1,NB
               IF (IW(IoJB+K).NE.IW(IoJBX+K))  GOTO 10
5              CONTINUE
c        redundant computation
           IW(IoKERR) = 0
           RETURN
           ENDIF
c
c -- Calculate C
c
10      NBX = NB
        DO 59 M=1,NB
c        set up the matrix and rhs
           DO 39 L=1,NB
c            identify the base species
               J = IW(IoJB+L)
c            save base species
               IW(IoJBX+L) = J
c            right hand side
               IF (L.EQ.M)  THEN
                       RW(IoW+L) = 1
                   ELSE
                       RW(IoW+L) = 0
                   ENDIF
c            matrix
               DO 29 K=1,NB
c                identify the atom
                   I = IW(IoIB+K)
c                set the coefficient
                   RW(IoA+L+LoA*K) = IW(IoN+I+LoN*J)
29                 CONTINUE
39             CONTINUE
c
c        solve for CM(M,K) for K=1,NB
           CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),NB,IW(IoKERR))
           IF (IW(IoKERR).NE.0)  RETURN
c
c        set the Mth row of CM
           DO 49 K=1,NB
               RW(IoCM+M+LoCM*K) = RW(IoW+K)
49             CONTINUE
59         CONTINUE
c
c    compute conditioned populations
       DO 69 K=1,NB
           IoPCK = IoPC + K
           RW(IoPCK) = 0
           TERMX = 0
           DO 67 L=1,NB
               IL = IW(IoIB+L)
               TERM = RW(IoCM+K+LoCM*L)*RW(IoPA+IL)
               CALL SJUMAX(TERM,TERMX)
               RW(IoPCK) = RW(IoPCK) + TERM
67             CONTINUE
           CALL SJURND(RW(IoPCK),TERMX)
69         CONTINUE
c
c -- Normal return
c
       IW(IoKERR) = 0
       RETURN
       END
c******************************************************************************
