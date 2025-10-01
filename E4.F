c******************************************************************************
c
c      Simlexspecies redistribution
c
c******************************************************************************
c
       SUBROUTINE SJISRD(NAMAX,NIW,NRW,IW,RW)
c
c      Simplex species redistribution.
c------------------------------------------------------------------------------
c      Species group notation:
c
c          smf denotes the species for which a target mol fraction
c          is specified.  These are balancing species.  The only other
c          allowed species are the basis species identified in SJISMP.
c
c          bo denotes basis species from SJISMP.
c
c          bx denotes detargeted balancing species.
c
c      SJISRD loads up the smf species as close to their target
c      mol fractions as possible by maximizing
c
c              sum{SMOL(J)}    sum over smf species
c
c          subject to the constraints
c
c              sum{N(I,J)*SMOL(J)} = PA(I)     for all independent atoms
c
c              SMOL(J)  ge  0    all bo&bx species
c
c              [X(J)*PMOL(phase of J) -  SMOL(J)]  ge  0   for smf species
c
c              X(J) is the target mol fraction for the smf species J.
c
c          Works only with the linearly independent atoms,  1,...,K,...,NB.
c
c          Only allocates atoms to the smf and bo&bx species.
c------------------------------------------------------------------------------
c          The simplex process is carried out using variables Y(J)
c          defined by
c
c           for bo and bx species:
c
c              Y(J) = SMOL(J)
c
c              Y(J) = - SMOL(J) + X(J)*PMOL(phase of J) smf species
c
c           for smf species:
c
c              Y(Jsmf)= - SMOL(Jsmf)
c                        + X(Jsmf)*[sum{SMOL(smf species in the phase)}
c                                + sum{Y(bo&bx species in the phase)}]
c
c          This is a system of simultaneous linear algebraic equations
c          relating the NSMF  values of SMOL(smf species) to NSMF
c          values of Y(smf species) and to the Y(bo&bx species):
c
c           - SMOL(Jsmf) + X(Jsmf)*sum{SMOL(smf species in the phase)}
c                  =  Y(Jsmf) - X(Jsmf)*sum{Y(bo&bx species in the phase)}
c
c           or, using the summation convention,
c
c           A(I,K)*SMOL(JSMF(K)) = Y(JSMF(I))
c                     + B(I,L)*Y(JBO(L))
c
c          where JSMF(K) denotes the J of the Kth smf species, and JBO(L)
c          denotes the J of the Lth bo&bx species.
c
c          The inversion of this gives
c
c          SMOL(JSMF(I)) = Q(I,K)*[Y(JSMF(K)) + B(K,L)*Y(JBO(L))]
c
c          Hence, the function to be minimized in the Simplex process is
c
c          - sum{Q(I,K)*[B(K,L)*Y(JBO(L)) + Y(JSMF(K))]} all smf species.
c
c          which we recast as
c
c            sum{F(M))*Y(M)} = min       M = 1,...,NS2
c
c          The atomic constraints become
c
c            N(I,JSMF(K))*Q(K,L)*[Y(JSMF(L)) + B(L,M)*Y(JBO(M))]
c
c               + N(I,JBO(K))*Y(JBO(K)) = PA(I)   I=IB(K), K=1,...,NB
c
c          which we recast as
c
c            sum{R(I,L)*Y(L)} = PA(I)     I = IB(K), K=1,NB
c
c          plus the constraints
c
c             Y(M) ge 0  smf and bo species.
c
c          For numerical precision we multiply by the conditioning
c          matrix CM(K,L) and instead work with the conditioned R and
c          the conditioned populations:
c
c            sum{RC(K,L)*Y(L)} = PC(K)      K=1,NB
c
c          The smf species denoted by JSMF above are carried as the first
c          species in JS2, followed by the bo, then the bx, species, then the
c          false species used during initialization.
c
c          At call there are no bx species (all balancers are given targets).
c          At the conclusion of the maximization described above, any smf
c          species in a phase with no mols is made a bx species, and a
c          second run is then made.  This allows the phase to become populated
c          while maintaining the smf constraints in populated phases.
c
c          At the conclusion, the system base control KB reset to SJISRD bases.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c @        NAMAX       maximum number of atoms
c @        NIW         dimension of work array IW
c @        NRW         dimension of work array IR
c          IW(I)       integer work array
c          RW(I)       REAL*8 work array
c
c      Variables in the integer work array IW:
c @        IB(K) = I   if the Ith atom is the Kth independent system atom
c @        JB(K) = J   if the Jth species is the Kth basis
c -        JS2(K) = J  if the Jth species is the Kth SJISRD species
c @        KB(J)       run-start basis control for species J:
c                   -3 for bx species not a SJISRD base at run start
c                   -2 for smf species not a SJISRD base at run start
c                   -1 for bo species not a SJISRD base at run start
c                    1 for bo species a SJISRD base at run sratr
c                    2 for smf species a SJISRD base at run start
c                    3 for bx species a SJISRD base at run start
c                    4 for excluded species
c                    0 for all other species
c @-       KB2(K)      basis control for the Kth SJISRD species
c                   -3 SJISRD y(K) is a bx species not in use as a y-base
c                   -2 SJISRD y(K) is a smf species not in use as a y-base
c                   -1 SJISRD y(K) is a bo species not in use as a y-base
c                    1 SJISRD y(K) is a bo species in use as a y-base
c                    2 SJISRD y(K) is a smf species in use as a y-base
c                    3 SJISRD y(k) is a bx species in use as a y-base
c @        KMON        monitor control
c @        KUMO        output unit for monitor
c -        LB2(K) = L  if the Lth SJISRD y is the Kth SJISRD basis
c @        MPJ(J) = M  if species J is in phase M
c @        N(I,J)      number of Ith atoms in Jth-species molecule
c @        NB          number of independent bases
c @        NP          number of phases
c @        NS          number of species
c
c      Variables in the real work array RW:
c -        A(I,J)      work array
c -        B(K,L)      bo species influence array (see above)
c @        CM(K,M)     conditioning matrix
c -        EEQN(K)     relative error in the Kth change equation
c -        F(K)        minimizing coefficient; see above
c @        FRND        roundoff number
c @        HUGE        largest machine number
c @#       PC(K)       conditioned population of Kth basis atoms
c @#       PMOL(M)     mols in phase M
c -        Q(I,J)      matrix inverse (y influence on mols)
c -        RC(I,L)     effective N(I,L) of y pseudospecies (conditioned R)
c @#       SMOL(J)     mols of the Jth species
c -        SMUL(I)     simplex lagrange multiplier for the Ith variable
c -        W(I)        work vector
c @        X(J)        target mol fractions (J for smf species)
c -        Y(L)        Lth element of the solution vector
c
c      Variables used only internally:
c -        NBOBX        total number of bo + bx species
c -        KPASS        pass counter
c -        KPMAX        maximum allowed KPASS
c -        KRUN         run-through counter
c -        NS2          number of SJISRD species
c -        NSMF         number of smf species
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
c------------------------------------------------------------------------------
       DIMENSION   IW(NIW),RW(NRW)
c------------------------------------------------------------------------------
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
c    integers for compares
       DATA   M1,M2,M3/-1,-2,-3/
c------------------------------------------------------------------------------
c    constants
       FERRF = 10*RW(IoFRND)
       ZERO = 0
c
c    get parameters
       NB = IW(IoNB)
       NP = IW(IoNP)
       NS = IW(IoNS)
       KMON = IW(IoKMON)
       KUMO = IW(IoKUMO)
c
c    set control
       KRUN = 1
c
c    set up JSMF and JS2 for smf species
1      NSMF = 0
       DO 2 J=1,NS
           L = IW(IoKB+J)
           IF ((L.EQ.2).OR.(L.EQ.M2))  THEN
               NSMF = NSMF + 1
               IW(IoJS2+NSMF) = J
               IW(IoKB2+NSMF) = L
               ENDIF
2          CONTINUE
c
c    set NS2, JS2, and KB2 for bo species
       NS2 = NSMF
       DO 3 K=1,NB
           J = IW(IoJB+K)
           NS2 = NS2 + 1
           IW(IoJS2+NS2) = J
           IW(IoKB2+NS2) = IW(IoKB+J)
3          CONTINUE
c    set NS2, JS2, and KB2 for bx species
       DO 4 J=1,NS
           L = IW(IoKB+J)
           IF ((L.EQ.3).OR.(L.EQ.M3))  THEN
               NS2 = NS2 + 1
               IW(IoJS2+NS2) = J
               IW(IoKB2+NS2) = L
               ENDIF
4          CONTINUE
       NBOBX = NS2 - NSMF
c
c    check for too many specified species
       IF (NSMF.GT.2*NAMAX) THEN
           IW(IoKERR) = 1
           RETURN
           ENDIF
c
c  set up the A array in B
       DO 9 K=1,NSMF
           JK = IW(IoJS2+K)
           MJK = IW(IoMPJ+JK)
           DO 8 L=1,NSMF
               JL = IW(IoJS2+L)
               MJL = IW(IoMPJ+JL)
               IoBKL = IoB + K + LoB*L
               IF (MJK.EQ.MJL)  THEN
                       RW(IoBKL) = RW(IoX+JK)
                   ELSE
                       RW(IoBKL) = 0
                   ENDIF
8              CONTINUE
           IoBKK = IoB + K + LoB*K
           RW(IoBKK) = RW(IoBKK) - 1
9          CONTINUE
c
c        compute the matrix Q
       DO 19 K=1,NSMF
c        load the matrix and rhs
           DO 15 L=1,NSMF
               RW(IoW+L) = 0
               DO 13 M=1,NSMF
                   RW(IoA+L+LoA*M) = RW(IoB+L+LoB*M)
13                 CONTINUE
15             CONTINUE
           RW(IoW+K) = 1
c        solve
           CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),NSMF,IW(IoKERR))
           IF (IW(IoKERR).NE.0)  RETURN
c        transfer the column
           DO 17 L =1,NSMF
               RW(IoQ+L+LoQ*K) = RW(IoW+L)
17             CONTINUE
19         CONTINUE
c
c    compute the B array
       IF (NBOBX.GT.0)  THEN
           DO 25 K=1,NSMF
               JK = IW(IoJS2+K)
               MJK = IW(IoMPJ+JK)
               DO 23 L=1,NBOBX
                   LX = NSMF + L
                   JL = IW(IoJS2+LX)
                   MJL = IW(IoMPJ+JL)
                   IF (MJK.EQ.MJL)  THEN
                           RW(IoB+K+LoB*L) = - RW(IoX+JK)
                       ELSE
                           RW(IoB+K+LoB*L) = 0
                       ENDIF
23                 CONTINUE
25             CONTINUE
           ENDIF
c
c    compute the F vector
       DO 39 K=1,NS2
           IoFK = IoF+K
           RW(IoFK) = 0
           IF (K.LE.NSMF)  THEN
c                smf species
                   DO 31 I=1,NSMF
                       RW(IoFK) = RW(IoFK) - RW(IoQ+I+LoQ*K)
31                     CONTINUE
               ELSE
c                bo&bx species
                   L = K - NSMF
                   DO 35 I=1,NSMF
                       DO 33 M=1,NSMF
                          RW(IoFK) = RW(IoFK) -
     ;                      RW(IoQ+I+LoQ*M)*RW(IoB+M+LoB*L)
33                        CONTINUE
35                     CONTINUE
               ENDIF
39         CONTINUE
c
c    compute the conditioned R array
       DO 59 K=1,NB
           DO 57 L=1,NS2
               JL = IW(IoJS2+L)
               IoRCKL = IoRC + K + LoRC*L
               RW(IoRCKL) = 0
c            branch on species L type
               IF (L.LE.NSMF)  THEN
c                    smf species
                       DO 45 K1=1,NSMF
                           J1 = IW(IoJS2+K1)
c                        check for a contribution
                           IF ((IW(IoKB+J1).NE.1)
     ;                      .OR.(IW(IoJB+K).EQ.J1)) THEN
                               IF (IW(IoKB+J1).EQ.1)  THEN
c                                    species J1 is the Kth base
                                       SUM1 = 1
                                   ELSE
c                                    species J1 is not a base
                                       SUM1 = 0
                                       DO 43 K2=1,NB
                                         I2 = IW(IoIB+K2)
                                         SUM1 = SUM1 +
     ;                                    RW(IoCM+K+LoCM*K2)*
     ;                                      IW(IoN+I2+LoN*J1)
43                                       CONTINUE
                                   ENDIF
                               RW(IoRCKL) = RW(IoRCKL)
     ;                           + SUM1*RW(IoQ+K1+LoQ*L)
                               ENDIF
45                         CONTINUE
                   ELSE
c                    bo&bx species
                       LX = L - NSMF
                       DO 55 K1=1,NSMF
                           SUM1 = 0
                           DO 53 K2=1,NSMF
                               J2 = IW(IoJS2+K2)
                               IF ((IW(IoKB+J2).NE.1)
     ;                          .OR.(IW(IoJB+K).EQ.J2))  THEN
c                               check for a contribution
                                 IF (IW(IoKB+J2).EQ.1)  THEN
c                                     species J2 is the Kth base
                                       SUM2 = 1
                                    ELSE
c                                     species J2 is not a base
                                       SUM2 = 0
                                       DO 51 K3=1,NB
                                         I3 = IW(IoIB+K3)
                                         SUM2 = SUM2 +
     ;                                    RW(IoCM+K+LoCM*K3)*
     ;                                      IW(IoN+I3+LoN*J2)
51                                       CONTINUE
                                    ENDIF
                                 SUM1 = SUM1 + SUM2*RW(IoQ+K2+LoQ*K1)
                                 ENDIF
53                             CONTINUE
                           RW(IoRCKL) = RW(IoRCKL) +
     ;                       SUM1*RW(IoB+K1+LoB*LX)
55                         CONTINUE
c
c                    check type of L species
                       IF (IW(IoKB+JL).EQ.1)  THEN
c                            L is a base; add 1 if its the Kth base
                               IF (L-NSMF.EQ.K)  RW(IoRCKL) =
     ;                          RW(IoRCKL) + 1
                           ELSE
c                            L is a bx species
                               DO 56 K1=1,NB
                                   I1 = IW(IoIB+K1)
                                   RW(IoRCKL) = RW(IoRCKL)
     ;                            + RW(IoCM+K+LoCM*K1)*
     ;                                IW(IoN+I1+LoN*JL)
56                                 CONTINUE
                           ENDIF
                   ENDIF
57             CONTINUE
59         CONTINUE
c
c -- set initial basis
c
c    clear all mols
       DO 61 J=1,NS
           RW(IoSMOL+J) = 0
61         CONTINUE
c    set basis mols = conditioned populations
       DO 63 K=1,NB
           J = IW(IoJB+K)
           RW(IoSMOL+J) = RW(IoPC+K)
63         CONTINUE
c
c    set Y
70     DO 71 K=1,NSMF
           J = IW(IoJS2+K)
           M = IW(IoMPJ+J)
           RW(IoY+K) = - RW(IoSMOL+J) + RW(IoX+J)*RW(IoPMOL+M)
71         CONTINUE
       DO 73 L=1,NBOBX
           K = NSMF + L
           J = IW(IoJS2+K)
           RW(IoY+K) = RW(IoSMOL+J)
73         CONTINUE
c
c    set basis pointer
       K = 0
       DO 75 L=1,NS2
           IF (IW(IoKB2+L).GT.0) THEN
               K = K + 1
               IW(IoLB2+K) = L
               ENDIF
75         CONTINUE
c
c    initializations
       KPASS = 0
       KPMAX = 2*NS2
c
c -- Simplex loop point; Determine the simplex multipliers.
c
100    KPASS = KPASS + 1
c
c    check pass count
       IF (KPASS.GT.KPMAX)  THEN
           IF (KMON.GT.0)  WRITE (KUMO,106)
106        FORMAT (/' Too many passes in SJISRD')
           IW(IoKERR) = 1
           RETURN
           ENDIF
c
c    set up the matrix and rhs
       DO 129 K=1,NB
           LK = IW(IoLB2+K)
           RW(IoSMUL+K) = RW(IoF+LK)
           DO 119 L=1,NB
               RW(IoA+K+LoA*L) = RW(IoRC+L+LoRC*LK)
119            CONTINUE
129        CONTINUE
c
c      solve to put the multipliers in SMUL
       CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoSMUL+1),NB,IW(IoKERR))
       IF (IW(IoKERR).NE.0)  RETURN
c
c ---- select the new basis member LN
c
       SMAX = 0
       LN = 0
       DO 169 L=1,NS2
           IF (IW(IoKB2+L).LT.0)  THEN
c            SJISRD species L is a trial base base
               KBN = - IW(IoKB2+L)
               SL = - RW(IoF+L)
               TERMX = DABS(SL)
c
c            add in the Simplex multiplier terms
156            DO 167 K=1,NB
                   AKL = RW(IoRC+K+LoRC*L)
                   TERM = RW(IoSMUL+K)*AKL
                   SL = SL + TERM
                   CALL SJUMAX(TERM,TERMX)
167                CONTINUE
               CALL SJURND(SL,TERMX)
               IF (SL.GT.SMAX)  THEN
c                a better candidate found
                   SMAX = SL
                   LN = L
                   ENDIF
               ENDIF
c
169        CONTINUE
c
c -- Check for completion (LN=0)
c
       IF (LN.EQ.0)  GOTO 400
c
c -- Determine the directions of change as LN becomes a basis member.
c
c          W(K) = dY(Kth old basis)/dY(new basis)
c
       KBN = - IW(IoKB2+LN)
c
c    load matrix in A and rhs in W
       DO  179  K=1,NB
           RW(IoW+K) = - RW(IoRC+K+LoRC*LN)
176        DO 177 L=1,NB
               LL = IW(IoLB2+L)
               RW(IoA+K+LoA*L) = RW(IoRC+K+LoRC*LL)
177            CONTINUE
179        CONTINUE
c
c      solve to put the directions in W
       CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),NB,IW(IoKERR))
       IF (IW(IoKERR).NE.0)  RETURN
c
c -- Determine which of the old bases KE will be eliminated
c
       LE = 0
       DS = RW(IoHUGE)
       DO 183 K=1,NB
           LK = IW(IoLB2+K)
           IoWK = IoW + K
           IoYLK = IoY + LK
           IF (((RW(IoWK).LT.ZERO).AND.(RW(IoYLK).GE.ZERO)).OR.
     ;         ((RW(IoWK).GT.ZERO).AND.(RW(IoYLK).LT.ZERO)))  THEN
c            the y of LK move towards zero on the path
               DSL = - RW(IoYLK)/RW(IoWK)
               IF (DABS(DSL-DS).LE.RW(IoFRND)*DS)  DSL = DS
               IF ((DSL.LT.DS).OR.((DSL.EQ.DS).AND.(LK.GT.NS2)))  THEN
c                base LK is a better replacement candidate
                   DS = DSL
                   LE = LK
                   KE = K
                   ENDIF
               ENDIF
183        CONTINUE
c
c -- check for elimination
c
       IF (LE.EQ.0)  GOTO 400
c
c -- solve for the new bases (rather than for changes, for better accuracy)
c
c    remove the old base
       RW(IoY+LE) = 0
       IW(IoKB2+LE) = - IW(IoKB2+LE)
c
c    set the new base
       RW(IoY+LN) = DS
       IW(IoKB2+LN) = - IW(IoKB2+LN)
       IW(IoLB2+KE) = LN
c
c    set up the equation system
       DO 209 K=1,NB
c        load rhs
           RW(IoW+K) = RW(IoPC+K)
c        load matrix
           DO 205 M=1,NB
               LM = IW(IoLB2+M)
               RW(IoA+K+LoA*M) = RW(IoRC+K+LoRC*LM)
205            CONTINUE
209        CONTINUE
c    solve
       CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),NB,IW(IoKERR))
c    check for failure
       IF (IW(IoKERR).NE.0)  RETURN
c    trim and load the solution
       DO 229 K=1,NB
           LK = IW(IoLB2+K)
           IoWK = IoW + K
           IoYLK = IoY + LK
c        zero drop fixup
           IF (((RW(IoWK).LT.ZERO).AND.(RW(IoYLK).GE.ZERO)).OR.
     ;     ((RW(IoWK).GT.ZERO).AND.(RW(IoYLK).LT.ZERO))) RW(IoWK) = 0
           RW(IoYLK) = RW(IoWK)
229        CONTINUE
c
c    check for accuracy of constraint satisfaction
       KFIX = 0
       DO 249 K=1,NB
c        compute equation error in SUM
           SUM = RW(IoPC+K)
           TERMX = DABS(SUM)
           DO 245 L=1,NB
               LL = IW(IoLB2+L)
               AX = RW(IoRC+K+LoRC*LL)
               TERM = AX*RW(IoY+LL)
               CALL SJUMAX(TERM,TERMX)
               SUM = SUM - TERM
245            CONTINUE
c        check relative errors
           IF (TERMX.NE.ZERO)  SUM = DABS(SUM/TERMX)
c        save error
           RW(IoEEQN+K) = SUM
c        check error vs limits
           IF (SUM.GT.FERRF)  KFIX = 1
249        CONTINUE
c
c    skip refinement if not necessary
       IF (KFIX.EQ.0)  GOTO 300
c
c -- Solution refinement
c
c    Solve for the populations of the other bases using the new base value
c    in a shortened set of conditional equations. The most accurately
c    solved equation (KE) is dropped.
c
c    find the most accurately solved equation
250    SUM = RW(IoHUGE)
       KE = 0
       DO 251 K=1,NB
           IF (RW(IoEEQN+K).LT.SUM)  THEN
               SUM = RW(IoEEQN+K)
               KE = K
               ENDIF
251        CONTINUE
c
c    check for inability to fix
       IF (KE.EQ.0)  THEN
           GOTO 300
           ENDIF
c
c    set up the reduced equation system
       K1 = 0
       DO 269 K=1,NB
           IF (K.NE.KE)  THEN
c            load rhs
               K1 = K1 + 1
               RW(IoW+K1) = RW(IoPC+K) -
     ;                  RW(IoRC+K+LoRC*LN)*RW(IoY+LN)
c            load matrix
               M1 = 0
               DO 267 M=1,NB
                   LM = IW(IoLB2+M)
                   IF (LM.NE.LN)  THEN
                       M1 = M1 + 1
                       RW(IoA+K1+LoA*M1) = RW(IoRC+K+LoRC*LM)
                       ENDIF
267                CONTINUE
               ENDIF
269        CONTINUE
c    solve
       CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),K1,IW(IoKERR))
c    check for failure
        IF (IW(IoKERR).NE.0)  THEN
c        try another equation
           RW(IoEEQN+KE) = RW(IoHUGE)
           GOTO 250
           ENDIF
c
c    transfer the solution
       K1 = 0
       DO 279 K=1,NB
           LK = IW(IoLB2+K)
           IF (LK.NE.LN)  THEN
               K1 = K1 + 1
               RW(IoY+LK) = RW(IoW+K1)
               ENDIF
279        CONTINUE
c
c -- end of pass
c
300   GOTO 100
c
c *** end of run
c
c   compute the mols of smf species
400    DO 409 K=1,NSMF
           J = IW(IoJS2+K)
           RW(IoSMOL+J) = 0
           TERMX = 0
           DO 407 L=1,NSMF
               TERM = RW(IoQ+K+LoQ*L)*RW(IoY+L)
               CALL SJUMAX(TERM,TERMX)
               RW(IoSMOL+J) = RW(IoSMOL+J) + TERM
               SUM = 0
               DO 405 M=1,NB
                   MX = NSMF + M
                   SUM = SUM + RW(IoB+L+LoB*M)*RW(IoY+MX)
405                CONTINUE
               TERM = RW(IoQ+K+LoQ*L)*SUM
               RW(IoSMOL+J) = RW(IoSMOL+J) + TERM
               CALL SJUMAX(TERM,TERMX)
407            CONTINUE
           CALL SJURND(RW(IoSMOL+J),TERMX)
409        CONTINUE
c
c    set mols of bo&bx species
       DO 411 K=1,NBOBX
           KX = NSMF + K
           J = IW(IoJS2+KX)
           L = NSMF + K
           RW(IoSMOL+J) = RW(IoY+L)
411        CONTINUE
c
c    compute phase mols
       DO 413 M=1,NP
           RW(IoPMOL+M) = 0
413        CONTINUE
       DO 415 K=1,NS2
           J = IW(IoJS2+K)
           M = IW(IoMPJ+J)
           RW(IoPMOL+M) = RW(IoPMOL+M) + RW(IoSMOL+J)
415        CONTINUE
c
c    check run
       IF (KRUN.EQ.1)  THEN
c       check for balancing species in an absent phase
           KSMF = 0
           DO 429 K=1,NSMF
               JK = IW(IoJS2+K)
               IF (RW(IoSMOL+JK).EQ.ZERO)  THEN
c                    balancer has zero mols; check phase
                       DO 427 L=1,NS2
                           JL = IW(IoJS2+L)
                           IF (IW(IoMPJ+JL).EQ.IW(IoMPJ+JK))  THEN
                               IF (RW(IoSMOL+JL).NE.ZERO)  THEN
c                                phase populated ; keep as smf
                                   KSMF = 1
                                   GOTO 429
                                   ENDIF
                               ENDIF
427                        CONTINUE
c                    balancer is in an empty phase; set as bx
                       IF (IW(IoKB2+K).LT.0)  THEN
c                            species is not a current SJISRD base
                               IW(IoKB2+K) = -3
                           ELSE
c                            species is a current SJISRD base
                               IW(IoKB2+K) = 3
                           ENDIF
                       KRUN = 2
                   ELSE
c                    balancer is smf species in a populated phase
                       KSMF = 1
                   ENDIF
429            CONTINUE
c
c        go do a second run if needed and feasible
           IF ((KRUN.EQ.2).AND.(KSMF.EQ.1))  THEN
c            set run-start basis controls
               DO 439 K=1,NS2
                   J = IW(IoJS2+K)
                   IW(IoKB+J) = IW(IoKB2+K)
439                CONTINUE
               GOTO 1
               ENDIF
           ENDIF
c
c ** exit
c
c    reset system basis as SJISRD basis
       NB = 0
       DO 459 L=1,NS2
           J = IW(IoJS2+L)
           IF (IW(IoKB2+L).GT.0)  THEN
c                the species is a system base
                   NB = NB + 1
                   IW(IoJB+NB) = J
                   IW(IoKB+J) = 1
               ELSE
c                the species is not a systems base
                   IW(IoKB+J) = 0
               ENDIF
459        CONTINUE
c
c    exit
       IW(IoKERR) = 0
       RETURN
c
       END
c******************************************************************************
