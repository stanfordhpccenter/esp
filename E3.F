c******************************************************************************
c
c      Simplex initializer
c
c******************************************************************************
c
       SUBROUTINE  SJISMP(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW,KOP)
c
c      Simplex initializer;  also used in rebasing
c------------------------------------------------------------------------------
c      Estimates the initial mol distribution by minimizing
c
c              sum{F(J)*SMOL(J)}
c                real species
c
c      subject to the constraints
c
c              sum{N(I,J)*SMOL(J)} = PA(I)     for all independent atoms
c                 real species
c
c          SMOL(J)  ge 0    all species
c
c      For KOP = 0:    (first initializer call)
c
c          start with false species distribution
c          F(J) = G(J)
c          SLAM(K) returned as simplex multiplier SMUL(K)
c            (element potential assuming X(J) = 1 for each base)
c
c      For KOP = 1:    (rebasing during phase adjustment in SJEQEP)
c
c          start with false species distribution
c          F(J) = G(J) - ln PMOL(phase of J)
c            (this gives better basing with large phase mols differences)
c
c      For KOP = 10 + JN:   (rebasing during initialization)
c
c          start with current bases and mols
c          install JN as a base and then exit
c
c      Works only with the linearly independent atoms,  1,...,K,...,NB.
c
c      At call:
c
c              NB = number of atom types
c              IB(I) = I (all atoms assumed independent)
c              KB(J) = 0 real species not excluded
c                      4 excluded species
c              PA(I) = system population of the Ith atom
c
c      On return:
c
c              KERR = 1 if the problem is singular
c              KERR = 3 if the populations are impossible
c
c                      or
c
c              KERR = 0 (populations possible)
c              NB = number of independent atoms
c              IB(K) = Kth independent atom  1 <= k <= nb
c                      dependent atoms for K > NB
c              JB(K) = Kth system base
c              KB(J) = 1 for base species
c              SMOL(J) set for the extremizing distribution
c
c      Two round approach:
c
c          The first round distributes atoms from false species to the
c          real species by minimizing the moles of false species subject
c          to the constraints:
c
c              Minimize   sum{abs(SMOL(J)} (constraints as above)
c                          false species
c
c          False species J = NS + K is a monatomic species consisting of
c          one IB(K) atom.
c
c          If false species with SMOL ne 0 remain at the end of round 1,
c          the input populations were impossible (error return).
c
c          If a false species J remains in the basis set, but with
c          SMOL(J) =0, then the corresponding atom is not independent.
c          NB and IB(K) are adjusted accordingly before round 2.
c
c          The second round performs the approximate Gibbs minimization.
c
c      Special consideration for electrons:
c
c          The electron population will be negative if the system carries a
c          positive charge (a negative amount of electrons).  Therefore, it
c          may be necessary to work with negative SMOL of false species in
c          round 1.  The program is designed with this in mind.
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
c #        IB(K) = I   if the Ith atom is the Kth independent atom
c -        IBO(K)      saved IB
c #        JB(K) = J   if the Jth species is the Kth base species
c #        KB(J) = 0   species J is not a base
c                  1   species J is a base
c                  2   species J may not be used as a base
c                  4   species J is excluded
c #        KERR        error flag (see above)
c @        KOP         run control (see above)
c @        KMON        monitor control
c @        KUMO        output unit for monitor
c -        LB2(K)      temporary storage
c @        MPJ(J)      phase of species J
c @        N(I,J)      number of Ith atoms in a species-J molecule
c @        NA          number of atom types in the system
c #        NB          number of bases (independent atoms)
c @        NP          number of phases allowed
c @        NS          total number of species
c
c      Variables in the real work array RW:
c -        A(I,J)      work matrix
c #        ELAM(K)     element potential for Kth independent atom
c -        F(J)        minimizing function; modified G(J)
c @        FRND        roundoff number
c @        G(J)        g(T,P)/RT for the Jth species
c @        HUGE        largest machine number
c @        PA(I)       population of the Ith atom type
c @        PC(K)       Kth conditioned population
c @        PMOL(M)     mols in the Mth phase
c -        SMOL(J)     mols of Jth species or false species
c -        SMUL(K)     simplex Lagrange multiplier for the Kth independent atom
c -        W(I)        work vector
c
c      Variables used only internally:
c          SJ          simplex descent strength
c          DS          path length in MOLS space
c          DSJ         path length for elimination of Jth species
c          JE          species to be eliminated
c          JN          species to become a base
c          KPASS       pass index
c          KPMAX       maximum number of passes
c          KROUND      round index
c          NT          total number of species involved in the simplex round
c          PMMIN       minimum phase mols
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
       CHARACTER*2 ATOM
       CHARACTER*8 CHEM
c------------------------------------------------------------------------------
       DIMENSION   ATOM(NAMAX),CHEM(NSMAX),IW(NIW),RW(NRW)
c------------------------------------------------------------------------------
c   pointers
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
c    extract constants
       NA = IW(IoNA)
       NP = IW(IoNP)
       NS = IW(IoNS)
       KMON = IW(IoKMON)
       KUMO = IW(IoKUMO)
       ZERO = 0
c
c    check option
c
       IF (KOP.GT.10)  THEN
c        set for prescribed rebasing
           NB = IW(IoNB)
           DO 1 J=1,NS
               RW(IoSMOL+J) = 0
               IoKBJ= IoKB + J
               IF (IW(IoKBJ).EQ.2)  IW(IoKBJ) = 0
               IF (IW(IoKBJ).LT.0)  IW(IoKBJ) = 1
1              CONTINUE
           DO 3 K=1,NB
               J = IW(IoJB+K)
               RW(IoSMOL+J) = RW(IoPC+K)
3              CONTINUE
           JN = KOP - 10
           GOTO 50
           ENDIF
c
       PMMIN = RW(IoHUGE)
       IF (KOP.EQ.1)  THEN
c        find the smallest populated phase mols
           DO 7 M=1,NP
               IF ((RW(IoPMOL+M).GT.ZERO).AND.(RW(IoPMOL+M).LT.PMMIN))
     ;             PMMIN = RW(IoPMOL+M)
7              CONTINUE
           ENDIF
c
c    set to start with false species
       KROUND = 1
       NT = NA + NS
c    allow all atoms
       NB = NA
       DO 9 I=1,NA
           IW(IoIB+I) = I
9          CONTINUE
c    set false species as the basis
       DO 11 K = 1,NB
           J = NS + K
           I = IW(IoIB+K)
           RW(IoSMOL+J) = RW(IoPA+I)
           IW(IoJB+K) = J
           IW(IoKB+J) = 1
11         CONTINUE
       DO 13 J = 1,NS
           IF (IW(IoKB+J).NE.4)  IW(IoKB+J) = 0
           RW(IoSMOL+J) = 0
           IoFJ = IoF + J
           RW(IoFJ) = RW(IoG+J)
c        check for rebasing call from SJESEP
           IF (KOP.EQ.1)  THEN
c           add phase correction
               M = IW(IoMPJ+J)
               IF (RW(IoPMOL+M).GT.ZERO)  THEN
                       RW(IoFJ) = RW(IoFJ) - DLOG(RW(IoPMOL+M))
                   ELSE
                       RW(IoFJ) = RW(IoFJ) - DLOG(PMMIN)
                   ENDIF
               ENDIF
13         CONTINUE
c
       KPMAX = 2*NT
       KPASS = 0
c
c -- Round loop point
c
c -- Simplex loop point; Determine the simplex multipliers.
c
20     KPASS = KPASS + 1
c
c    check pass count
       IF (KPASS.GT.KPMAX)  THEN
           IF (KMON.GT.0)  WRITE (KUMO,26)
26         FORMAT (/' Too many passes in SJISMP')
           IW(IoKERR) = 1
           RETURN
           ENDIF
c
c    set up the matrix and rhs
       DO 29 K=1,NB
           J = IW(IoJB+K)
c
c        right hand sides are loaded in SMUL(K)
           IF (KROUND.EQ.1)  THEN
c                round 1; false species distribution
                   IF (J.GT.NS)  THEN
c                        false species
                           IF (RW(IoSMOL+J).GE.ZERO)  THEN
                                   RW(IoSMUL+K) = 1
                               ELSE
                                   RW(IoSMUL+K) = - 1
                               ENDIF
                       ELSE
c                        real species
                           RW(IoSMUL+K) = 0
                       ENDIF
               ELSE
c                round 2; approximate Gibbs minimization
                   RW(IoSMUL+K) = RW(IoF+J)
               ENDIF
c
c        matrix coefficients A(K,L)
           DO 27 L=1,NB
               I = IW(IoIB+L)
               IF (J.GT.NS)  THEN
c                    false species
                       IF (L.EQ.J-NS)  THEN
                               RW(IoA+K+LoA*L) = 1
                           ELSE
                               RW(IoA+K+LoA*L) = 0
                           ENDIF
                   ELSE
c                    real species
                       RW(IoA+K+LoA*L) = IW(IoN+I+LoN*J)
                   ENDIF
27             CONTINUE
29         CONTINUE
c
c    solve to put the multipliers in SMUL
       CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoSMUL+1),NB,IW(IoKERR))
       IF (IW(IoKERR).NE.0)  RETURN
c
c -- select the new basis member JN
c
       SMAX = 0
       JN = 0
       DO 49 J=1,NT
           IF (IW(IoKB+J).EQ.0)  THEN
c            species J is a trial new base
               IF (KROUND.EQ.1)  THEN
c                    round 1; false species distribution
                       IF (J.GT.NS)  THEN
c                            trial base is false species
                               SJ = - 1
                           ELSE
c                            trial base is a real species
                               SJ = 0
                           ENDIF
                   ELSE
c                    round 2; approximate Gibbs minimization
                       SJ = - RW(IoF+J)
                   ENDIF
               TERMX = DABS(SJ)
c
c            add in the simplex multiplier terms
               DO 39 K=1,NB
                   I = IW(IoIB+K)
                   IF (J.GT.NS)  THEN
c                        trial base is a false species
                           IF (K.EQ.J-NS)  THEN
                                   AKL = 1
                               ELSE
                                   AKL = 0
                               ENDIF
                       ELSE
c                        trial base is a real species
                           AKL = IW(IoN+I+LoN*J)
                       ENDIF
                   TERM = RW(IoSMUL+K)*AKL
                   SJ = SJ + TERM
                   CALL SJUMAX(TERM,TERMX)
39                 CONTINUE
               CALL SJURND(SJ,TERMX)
               IF (SJ.GT.SMAX)  THEN
c                a better candidate found
                   SMAX = SJ
                   JN = J
                   ENDIF
               ENDIF
49         CONTINUE
c
c -- Check for completion (JN=0)
c
       IF (JN.EQ.0)  GOTO 100
c
c -- Determine the directions of change as JN becomes a basis member.
c
c          W(K) = dSMOL(Kth old basis)/dSMOL(new basis)
c
c    load matrix in A and rhs in W
50     DO  79  K=1,NB
           I = IW(IoIB+K)
c
c        load the RHS
           IF (JN.GT.NS)  THEN
c                new base is a false species
                   IF (K.EQ.JN-NS)  THEN
                           RW(IoW+K) = - 1
                       ELSE
                           RW(IoW+K) = 0
                       ENDIF
               ELSE
c                new base is a real species
                   RW(IoW+K) = - IW(IoN+I+LoN*JN)
               ENDIF
c
c        load the matrix
           DO 69 L=1,NB
               J = IW(IoJB+L)
               IF (J.GT.NS) THEN
c                    false species
                       IF (K.EQ.J-NS)  THEN
                               RW(IoA+K+LoA*L) = 1
                           ELSE
                               RW(IoA+K+LoA*L) = 0
                           ENDIF
                   ELSE
c                    real species
                       RW(IoA+K+LoA*L) = IW(IoN+I+LoN*J)
                   ENDIF
69             CONTINUE
c
79         CONTINUE
c
c    solve to put the directions in V
       CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),NB,IW(IoKERR))
       IF (IW(IoKERR).NE.0)  RETURN
c
c -- Determine which of the old bases JE will be eliminated
c
       JE = 0
       DS = RW(IoHUGE)
       DO  89  K=1,NB
           J = IW(IoJB+K)
           IF (((RW(IoW+K).LT.ZERO).AND.(RW(IoSMOL+J).GE.ZERO)).OR.
     ;         ((RW(IoW+K).GT.ZERO).AND.(RW(IoSMOL+J).LT.ZERO)))  THEN
c            the mols of J move towards zero on the path
               DSJ = - RW(IoSMOL+J)/RW(IoW+K)
               IF (DABS(DSJ-DS).LE.RW(IoFRND)*DS)  DSJ = DS
               IF ((DSJ.LT.DS).OR.((DSJ.EQ.DS).AND.(J.GT.NS)))  THEN
c                base J is a better replacement candidate
                   DS = DSJ
                   JE = J
                   KE = K
                   ENDIF
               ENDIF
89         CONTINUE
c
c -- check for no elimination
c
       IF (JE.EQ.0)  GOTO 100
c
c -- Make the changes
c
c    modify the old basis set
       DO 91 K=1,NB
           J = IW(IoJB+K)
           IoSMOJ = IoSMOL + J
           TERM = RW(IoSMOJ)
           RW(IoSMOJ) = TERM + RW(IoW+K)*DS
           CALL SJURND(RW(IoSMOJ),TERM)
91         CONTINUE
       RW(IoSMOL+JE) = 0
c
c    modify the new base
       RW(IoSMOL+JN) = DS
       IW(IoKB+JN) = 1
       IW(IoJB+KE) = JN
c
c    remove old base from base list
       IW(IoKB+JE) = 0
c
c    check for rebasing
       IF (KOP.GT.10)  THEN
           IW(IoKERR) = 0
           RETURN
           ENDIF
c
c -- check for end of false species distribution
c
       IF (KROUND.EQ.1)  THEN
           DO 93 K=1,NB
               IF (IW(IoJB+K).GT.NS)  GOTO 20
93             CONTINUE
c
           GOTO 100
           ENDIF
c
c ---- end of a simplex pass
c
       GOTO 20
c
c ** end of simplex process
c
c    branch on round
100    IF (KROUND.EQ.1)  THEN

c -- end of false species distribution
c
c        identify the basis species for the system
           K = 0
           DO 105 L=1,NB
               J = IW(IoJB+L)
                   IF (J.LE.NS)  THEN
c                        real species is a base
                           K = K + 1
                           IW(IoJB+K) = J
                       ELSE
c                        false species remains; ok if zero moles
                           IF (RW(IoSMOL+J).NE.ZERO)  THEN
c                              impossible populations
                               IW(IoKERR) = 3
                               RETURN
                               ENDIF
                       ENDIF
105               CONTINUE
c
c        identify the independent atoms
           K = 0
           DO 109 L=1,NA
               IW(IoLB2+L) = IW(IoIB+L)
               J = NS + L
               IF (IW(IoKB+J).EQ.0)  THEN
                   K = K + 1
                   IW(IoIB+K) = IW(IoIB+L)
                   ENDIF
109            CONTINUE
c
c        set the revised basis size
           NB = K
           IW(IoNB) = NB
c
c        set the dependent atoms
           K = NB
           IF (NB.LT.NA)  THEN
               DO 119 L = 1,NA
                   J = NS + L
                   IF (IW(IoKB+J).NE.0)  THEN
                       K = K + 1
                       IW(IoIB+K) = IW(IoLB2+L)
                       ENDIF
119                CONTINUE
               ENDIF
c
c        go do the approximate Gibbs minimization
           KROUND = 2
           NT = NS
           KPASS = 0
           GOTO 20
c
       ENDIF
c
c -- end of round 2
c
c    check option
       IF (KOP.NE.1)  THEN
c        set simplex multiplier as ELAM estimate
           DO 129 K=1,NB
               RW(IoELAM+K) = RW(IoSMUL+K)
129            CONTINUE
           ENDIF
c
c    normal return
       IW(IoKERR) = 0
       RETURN
c
       END
c******************************************************************************
