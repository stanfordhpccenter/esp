c******************************************************************************
c
c      Initial element potential estimates
c
c******************************************************************************
c
       SUBROUTINE SJIESL(NSMAX,NIW,NRW,CHEM,IW,RW,
     ;                   KOP,DOM,JW)
c
c      Estimates the element potentials ELAM and dominance DOM by species
c      JW.  JW is returned zero if no base is dominated.
c------------------------------------------------------------------------------
c  * For KOP = 0:
c
c    For each base K:
c
c      If PC(K) ne 0 for a base species, the equation used is:
c
c          sum{ELAM(I)*N(I,J)} = G(J) + ln X(J)
c
c      where J denotes the Kth base species.
c
c      If PC(K) = 0 , then an equation relating the ELAMs is (approximately)
c
c          X(JB) + B*X(JBZ) = 0
c
c      where JBZ is the balancing species. This produces the equation used:
c
c     - G(J) + sum{N(I,J)*ELAM(I)} = ln(-B) - G(JBZ) + sum{N(I,JBZ)*ELAM(I)}
c
c      This equation is used if the base and its balancer are in the same
c      phase, or in different phases that are both populated.
c
c      If the base phase is empty, the equation used is X(JB) = 1.
c
c      If a base with non-zero conditioned population is dominated by
c      another species in its equation with a negative coefficient, then
c      the base is treated as if it had a zero conditioned population.
c
c      The calculation is iterated to check balancer selection.
c------------------------------------------------------------------------------
c  * For KOP = 1:
c
c      The mol fraction of each base species is used in the ELAM estimates.
c------------------------------------------------------------------------------
c      On return:
c
c        If solution found:
c
c          KERR = 0
c
c        If there are solution singularities:
c          KERR = 1
c          ELAMs unchanged
c
c        If unbalanceable species are bases:
c          KERR = 4
c          KB(J) = 4 for bases to be removed
c          ELAMs unchanged
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
c @        KOP         option control
c #        DOM         largest dominance (see SJIJBZ)
c #        JW          worst dominating species (0 if none)
c
c      Variables in the integer work array IW:
c @        IB(K) = I   if the Kth base atom is the Ith atom
c @        JB(K) = J   if the Kth base species is the Jth species
c #        JBAL(K)     JBZ for the Kth base species
c @        KB(J)       basis control
c                    0 if the Jth species is not a base
c                   -1 if the Jth species is a base freed for SJISRD call
c                    1 if the Jth species is a base
c  #                 2 if the Jth species is a balancing species
c  #                 4 if the Jth species is excluded
c #        KERR        return error flag (0 ok, >0 problems)
c @        KMON        monitor control
c @        KUMO        output unit for monitor
c @        NB          number of basis species
c @        N(I,J)      number of Ith atoms in the Jth species
c @        NS          number of species
c @        MPJ(J)      phase of species J
c
c      Variables in the real work array RW:
c -        A(K)        work array
c #        BBAL(K)     BZ for Kth base species
c @        CM(K,L)     conditioning matrix
c @        G(J)        g(T,P)/RT for the Jth species
c @        HUGE        largest machine number
c #        ELAM(K)      element potential for the Kth base atom
c @        MOLS(J)     mols of the Jth species
c @        PC(K)       Kth conditioned population
c @        PMOL(M)     mols in phase M
c -        W(K,L)      work array
c @        X(J)        mol fraction of Jth species
c
c      Variables used only internally:
c          B           balacing ceofficient
c          BZ          balancing coefficient
c          JBZ         balancing species
c          KBAB        bad base flag
c          KGO         exit check flag
c          KTRL        iteration counter
c          MJ          phase of the Jth species
c          MJBZ        balancer phase
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
       CHARACTER*8 CHEM
c------------------------------------------------------------------------------
       DIMENSION   CHEM(NSMAX),IW(NIW),RW(NRW)
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
c    set constants
        ZERO = 0
c
c    get parameters
        NB = IW(IoNB)
        NS = IW(IoNS)
        KUMO = IW(IoKUMO)
        KMON = IW(IoKMON)
c
c  ** reentry point after exchange of base and balancer
c
c    set false balancers to trigger first run
2      DO 5 K=1,NB
           IW(IoJBAL+K) = -1
5          CONTINUE
c
c    set counter
       KTRL = 0
       KBAD = 0
c
c  **  balancer iteration loop point
c
10     KTRL = KTRL + 1
c
       IF (KOP.EQ.0)  THEN
c        initialize dominance
           DOM = - RW(IoHUGE)
           JW= 0
c        set up balancers and check for run
           KGO = 0
           DO 13 K=1,NB
c            get balancer and dominator
                CALL SJIJBZ(NIW,NRW,IW,RW,
     ;                      K,JBZ,RW(IoBBAL+K),JBS,DOMX)
c            check for populated base
               IF (RW(IoPC+K).NE.ZERO)  THEN
c                    check for dominance by balancer
                       IF ((JBS.NE.0).AND.(JBS.EQ.JBZ))  THEN
c                            treat as if zero conditioned population
                               JBALX = JBZ
                               JBS = 0
                           ELSE
c                            treat with specified mol fraction
                               JBALX = 0
                           ENDIF
                   ELSE
c                    set balancer
                       JBALX = JBZ
c                    check for no balancer
                       IF (JBZ.EQ.0)  THEN
c                        set flag
                           J = IW(IoJB+K)
                           IW(IoKB+J) = 4
                           KBAD = 1
                           IF (KMON.GT.1)  WRITE (KUMO,12) CHEM(J)
12                         FORMAT (/'  Absent species: ',A)
                           ENDIF
                   ENDIF
c            compare to current dominance
               IF (JBS.NE.JBZ)  THEN
                   IF (DOMX.GT.DOM)  THEN
c                    this is the new worst case
                       DOM = DOMX
                       IF (DOM.GT.ZERO)  JW = JBS
                       ENDIF
                   ENDIF
c            set controls
               IF (JBALX.NE.IW(IoJBAL+K))  KGO = 1
               IW(IoJBAL+K) = JBALX
13             CONTINUE
c
c        check for excluded species
           IF (KBAD.NE.0)  THEN
               IW(IoKERR) = 4
               RETURN
               ENDIF
c
c        check for converged solution
           IF ((KGO.EQ.0).OR.(KTRL.GT.NB)) GOTO 90
           ENDIF
c
c    clear flags
       DO 15 J=1,NS
           IoKBJ = IoKB + J
           IF (IW(IoKBJ).LT.0)  IW(IoKBJ) = - IW(IoKBJ)
           IF (IW(IoKBJ).EQ.2)  IW(IoKBJ) = 0
15         CONTINUE
c
c    form the rhs and matrix
       DO 39 K=1,NB
c
           IoWK = IoW + K
c        identify base species and its phase
           J = IW(IoJB+K)
           MJ = IW(IoMPJ+J)
c
c        determine appropriate equation for this base
           IF ((KOP.EQ.1).OR.(IW(IoJBAL+K).EQ.0)) THEN
c                set to produce the base mol fraction
                   RW(IoWK) = RW(IoG+J)
                   IF (RW(IoX+J).GT.ZERO)  RW(IoWK) =
     ;                 RW(IoWK) + DLOG(RW(IoX+J))
                   DO 17 L=1,NB
                       I = IW(IoIB+L)
                       RW(IoA+K+LoA*L) = IW(IoN+I+LoN*J)
17                     CONTINUE
c                go monitor
                   GOTO 39
c
               ELSE
c
c                identify balancer and its phase
                   JBZ = IW(IoJBAL+K)
                   BZ = RW(IoBBAL+K)
                   MJBZ = IW(IoMPJ+JBZ)
c
c                check for a balance within the phase
                   IF (MJBZ.EQ.MJ)  THEN
c                    set for balance equation within the phase
                       RW(IoW+K) = RW(IoG+J) - RW(IoG+JBZ) + DLOG(-BZ)
                       DO 21 L=1,NB
                           I = IW(IoIB+L)
                           RW(IoA+K+LoA*L) = IW(IoN+I+LoN*J)
     ;                       - IW(IoN+I+LoN*JBZ)
21                         CONTINUE
c                    go set balancer commitment
                       GOTO 30
                       ENDIF
c
c                the base and balancer are in different phases
c                 check for zero mols in the base phase
                   IF (RW(IoPMOL+MJ).EQ.ZERO)  THEN
c                   the base phase is empty; set X = 1 for the base
                       RW(IoW+K) = RW(IoG+J)
                       DO 23 L=1,NB
                           I = IW(IoIB+L)
                           RW(IoA+K+LoA*L) = IW(IoN+I+LoN*J)
23                         CONTINUE
c                    go set balancer commitment
                       GOTO 30
                       ENDIF
c
c                the base phase is populated
c                 check for zero mols in the balancer phase
                   IF (RW(IoPMOL+MJBZ).EQ.ZERO)  THEN
c                   the balancer phase is empty
c                    replace the base by the balancer
c                     (the balancer will then be in a populated
c                     phase and can have a target X<1 for redistribution)
                       IW(IoJB+K) = JBZ
                       IW(IoKB+JBZ) = 1
                       IW(IoKB+J) = 0
c                    recalculate the conditioning matrix and
c                     conditioned populations
                       L = 0
                       CALL SJICPC(NIW,NRW,IW,RW,L)
c                    go redo for the new base set
                       GOTO 2
                       ENDIF
c
c                balance between populated phases
                   B = BZ*RW(IoPMOL+MJBZ)/RW(IoPMOL+MJ)
                   RW(IoW+K) = RW(IoG+J) - RW(IoG+JBZ)  + DLOG(-B)
                   DO 27 L=1,NB
                       I = IW(IoIB+L)
                       RW(IoA+K+LoA*L) = IW(IoN+I+LoN*J) -
     ;                   IW(IoN+I+LoN*JBZ)
27                     CONTINUE
c                go set balancer commitment
                   GOTO 30
c
c                balancer commitment; check for previous commitment
30                 IF (IW(IoKB+JBZ).NE.2)  THEN
c                    tag the balancer
                       IW(IoKB+JBZ) = 2
c                    free the base
                       IW(IoKB+J) = -1
                       ENDIF
c
c            end of equation selection
               ENDIF
c
39         CONTINUE
c
c    solve to get the element potentials into ELAM
       CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),NB,IW(IoKERR))
       IF (IW(IoKERR).NE.0)  RETURN
c
c    put solution into ELAM
       DO 43 K=1,NB
           RW(IoELAM+K) = RW(IoW+K)
43         CONTINUE
c
c    check for iteration
       IF (KOP.EQ.0)  GOTO 10
c
c    normal return
90     IW(IoKERR) = 0
       RETURN
c
       END
c******************************************************************************
c
       SUBROUTINE  SJIJBZ(NIW,NRW,IW,RW,L,JBZ,BZ,JBS,DOM)
c
c
c      For given ELAM and PMOL, finds the principal balancing species
c      JBZ associated with the Lth base species, returning coefficient in BZ.
c      Also finds the dominator JBS and its dominance DOM.
c------------------------------------------------------------------------------
c      The atomic constraints are
c
c        sum{PMOL(M)*sum{N(I,J)*X(J)}} = PA(I)      I = i,...,NA
c                all gas species
c
c      The conditioning operation removes all base species but one from
c      each of the conditioned equations:
c
c        sum{PMOL(M)*[X(JB(L)) +  sum{sum{CM(L,K)*N(IB(K),J)*X(J)}}] = PC(L)
c         M sum over phases
c                                   J sum over non-base species in phase M
c                                       K sum over base species in phase M
c      where
c
c          PC(M) = sum{CM(L,K)*PA(IB(K))}
c                   K sum over base species in phase M
c
c      If PC(M) le 0, the mol fraction X must be balanced by terms provided
c      by secondary species.  The balance requires a negative coefficient
c
c          BZ = sum{CM(L,K)*N(IB(K),J)}
c                K sum over base species in phase M
c
c      The secondary species J having a negative coefficient and the
c      smallest G(J) is identified as the base balancing species JBZ(L).
c      The coefficient is included in the selection.
c
c      The balance used in the element potential estimation is
c
c      PMOL(MPJ(JB(L)))*X(JB(L)) + PMOL(MPJ(JBZB(L)))*BZ(L)*X(JBZ(L)) = 0
c
c      If no principal secondary species is identified, JBZ is returned 0.
c
c      The dominance is the ln of the ratio of the dominator term in the
c      conditioned population equation to the base term. It will be negative
c      for an undominated base.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c @        NIW         dimension of work array IW
c @        NRW         dimension of work array IR
c          IW(I)       integer work array
c          RW(I)       REAL*8 work array
c @        L           base index
c #        JBZ         secondary species balancing base L (0 if none)
c #        BZ          balancing coefficient
c #        JBS         secondary species dominating base L (0 if none)
c #        DOM         dominance of base by dominator
c
c      Variables in the integer work array IW:
c @        IB(K) = I   if the Ith atom is the Kth independent atom
c @        JB(K) = J   if the Jth species is the Kth base species
c @        KB(J)       basis control
c                    0 if the Jth species is not a base
c                   -1 if the Jth species is a base freed for SIMP2 call
c                    1 if the Jth species is a base
c                    2 if the Jth species is a balancing species
c                    4 if the Jth species is excluded
c @        MPJ(J) = M  if species J is in phase M
c @        N(I,J)      number of the Ith atoms in the Jth species
c @        NB          number of bases
c @        NS          number of species
c
c      Variables in the real work array RW:
c @        CM(M,K)     conditioning matrix
c @        ELAM(K)     estimated element potential of the Kth independent atom
c @        FRND        roundoff number
c @        G(J)        g(T,P)/RT for the Jth species
c @        HUGE        largest machine number
c @        PMOL(M)     mols of phase M
c
c      Variables used only internally:
c          BESTZ       balancer test function
c          BESTS       dominator test function
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
c------------------------------------------------------------------------------
       DIMENSION   IW(NIW),RW(NRW),IEPTR(80)
c------------------------------------------------------------------------------
c    pointers
       COMMON /SJEPTR/ IEPTR
       EQUIVALENCE (IoIB,IEPTR(9)),(IoJB,IEPTR(11)),(IoKB,IEPTR(18)),
     ;   (IoMPJ,IEPTR(27)),
     ;   (IoN,IEPTR(28)),(LoN,IEPTR(29)),(IoNB,IEPTR(6)),
     ;   (IoNS,IEPTR(8)),(IoCM,IEPTR(41)),(LoCM,IEPTR(42)),
     ;   (IoELAM,IEPTR(53)),(IoFRND,IEPTR(31)),(IoG,IEPTR(57)),
     ;   (IoHUGE,IEPTR(32)),(IoPMOL,IEPTR(62))
c------------------------------------------------------------------------------
c    set constants
        ZERO = 0
        ONE = 1
c
c    recover parameters
        NB = IW(IoNB)
        NS = IW(IoNS)
c
c    get species and phase
       JL = IW(IoJB+L)
       ML = IW(IoMPJ+JL)
c
c    initialize
       JBZ = 0
       BZ = 0
       JBS = 0
       BESTZ = - RW(IoHUGE)
       BESTS = - RW(IoHUGE)
c
c    examine all species
       DO 19 J=1,NS
c     check for an included non-base species
           IF ((IW(IoKB+J).EQ.0).OR.(IW(IoKB+J).EQ.2))  THEN
c          non-base species; compute the conditioned coefficient
               SUM = 0
               TERMX = 0
               DO 11 K=1,NB
                   I = IW(IoIB+K)
                   TERM = RW(IoCM+L+LoCM*K)*IW(IoN+I+LoN*J)
                   SUM = SUM + TERM
                   CALL SJUMAX(TERM,TERMX)
11                 CONTINUE
               CALL SJURND(SUM,TERMX)
               IF (SUM.EQ.ZERO)  GOTO 19
c            get phase of test species
               MJ = IW(IoMPJ+J)
c            initiate the exponent argument
               TEST = - RW(IoG+J) + DLOG(DABS(SUM))
c            incorporate phase information if known
               IF ((RW(IoPMOL+MJ).NE.ZERO).AND.(RW(IoPMOL+ML).NE.ZERO))
     ;           TEST = TEST + DLOG(RW(IoPMOL+MJ)/RW(IoPMOL+ML))
c            incorporate the ELAM terms
               DO 13 K1=1,NB
                   I1 = IW(IoIB+K1)
                   TEST = TEST + RW(IoELAM+K1)*IW(IoN+I1+LoN*J)
13                 CONTINUE
c            check sum
               IF (SUM.LT.ZERO)  THEN
c                potential balancer;  check vs current balancer
                   IF (TEST.GT.BESTZ)  THEN
c                    this is a better choice
                           BESTZ = TEST
                           JBZ = J
                           BZ = SUM
                           ENDIF
                    ENDIF
c            no domination decision if phase empty
               IF (RW(IoPMOL+MJ).NE.ZERO)  THEN
c                check vs current dominator
                   IF (TEST.GT.BESTS)  THEN
c                    this is a better choice
                       BESTS = TEST
                       JBS = J
                       ENDIF
                   ENDIF
               ENDIF
19         CONTINUE
c
c    check for dominator
       IF (JBS.NE.0)  THEN
c            add in the base contributions to dominance
               DOM =  BESTS + RW(IoG+JL)
               DO 23 K1=1,NB
                   I1 = IW(IoIB+K1)
                   DOM = DOM - RW(IoELAM+K1)*IW(IoN+I1+LoN*JL)
23                 CONTINUE
               CALL SJURND(DOM,ONE)
               IF (DOM.LE.ZERO)  JBS = 0
           ELSE
c            set large negative dominance
               DOM = - RW(IoHUGE)
           ENDIF
c
       RETURN
       END
c******************************************************************************
