c******************************************************************************
c
c      Equilbrium solver
c
c******************************************************************************
c
       SUBROUTINE SJEQLB(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW,KINIT)
c
c      Equilibrium solution by element potentials (and initialization).
c------------------------------------------------------------------------------
c      Solves for the element potentials and phase mols that meet the
c      atomic constraints.  Uses the dual problem for
c
c           W = sum{PMOL(M)*[Z(M)-1]} - sum{ELAM(K)*PA(IB(K)}
c
c      The atomic constraints are satisfied when W is a minimum with
c      respect to variations in ELAMs at fixed PMOLs.  For any such
c      state the Z constraints are satisfied when this W = W* = Y is
c      maximized with respect to PMOLs.  This forms the basis for the
c      steepest descent/ascent algorithms used below. Newton-Raphson
c      iterations are used near the extremum states.
c
c      The basic element potential mol fraction generator
c
c             X(J) = exp[-G(J) + sum{ELAM(K)*N(K,J)}]
c
c      is used to calculate the phase mol fractions.
c
c      Works in three modes:
c
c          Mode 1: Steepest descent in ELAM space at fixed PMOLs
c                  followed by steepest ascent in PMOL space
c
c          Mode 2: Newton-Raphson iteration on ELAMs at fixed PMOLs
c                  followed by steepest ascent in PMOL space
c
c          Mode 3: Newton-Raphson iteration on ELAMs and PMOLs
c
c          Problems:
c
c              KERR = 1 solution failed due to singularity
c              KERR = 2 solution failed to converge
c
c      Initialization:
c
c          If KINIT = 1  at call, the initializer is called to set up
c          the problem and estimate the element potentials and phase mols.
c
c          If KINIT = 0 at call, the current values are used to start.
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
c @        KINIT       initialization control (initialize if 1)
c
c      Variables in the integer work array IW:
c #        IB(K) = I   if the Kth independent atom is the Ith system atom
c #        JB(K) = J   if the Kth base is the Jth species
c #        KB(J)       0 if Jth species is not a base
c                      1 if the Jth species is a base
c                      4 if the Jth species is excluded
c #        KERR        error flag
c @        KMON        monitor control
c -        KPC(M)      0 if phase not active, 1 if active
c -        KPCX(M)     KPC for provisional new phase distribution
c #        KTRE        main pass counter
c @        KUMO        output unit for monitor
c #        MPA(L) = M  if Mth phase is the Lth active phase
c #        MPJ(J) = M  if the Jth species is in the Mth phase
c @        N(I,J)      number of Ith atoms in the Jth species
c @        NA          number of atom types in the system
c #        NB          number of independent atoms (basis species)
c @        NP          number of allowed phases
c @        NS          number of species
c @        NSP(M)      number of species in the Mth phase
c
c      Variables in the real work array RW:
c @        ALPH        Lagrange multiplier for H when (T,P,H) specified
c -        A(K,L)      work matrix
c #        CM(K,M)     conditioning matrix
c -        D(K,M)      sum{N(IB(K),J)*X(J)} for Mth phase
c -        DC(K,M)     conditioned D vector for Mth phase
c -        DLAM(K)     change in ELAM(K) when PMOLs held constant
c -        DLAY(K)     change in ELAM(K) associated with PMOLs changes
c -        DPML(M)     change in PMOL(M)
c -        E(K,M)      satisfies sum{QC(K,L)*E(L,M)} = - DC(K,M)
c #        ELAM(K)     element potential for the Kth independent atom
c @        FRND        roundoff factor
c @        G(J)        g(T,P)/RT for the Jth species
c -        HA(K)       sum{PMOL(M)*D(K,M)} - PA(IB(K)) (zero for solution)
c -        HC(K)       conditioned H vector
c @        PA(I)       population of Ith atom
c #        PC(K)       conditioned  populations SUM{CM(K,L)*PA(IB(L))}
c #        PMOL(M)     mols of Mth phase
c -        Q(K,L)      sum{PMOL(M)*SUM{(Mth phase) N(IB(K),J)*N(IB(L),J)*X(J)}}
c -        QC(K,L)     conditioned Q matrix
c -        RL(K)       direction cosines for steepest descent in ELAM space
c -        RP(M)       direction cosines for steepest descent in PMOL space
c -        SMOL(J)     Jth species mols  (plus false species in SJISMP)
c                  (FINAL VALUES NOT COMPUTED BY SJEQLB!)
c -        W(K)        work vector
c #        X(J)        mol fraction of the Jth species
c -        Z(M)        sum{X(J)} over species in Mth phase
c
c      Variables used only internally:
c          AEZM        maximum error in Z(M) - 1 for an active phase
c          AEZMD       value of AEZM above which a mode downgrade is made
c          AEZMU       value of AEZM below which a mode upgrade is attempted
c          AEZMF       factor for error tolerance in mode 1
c          BETA2       sum{HA(K)*HA(K)} = Beta-squared (sum over bases)
c          BETA        -sqrt[BETA2] = dW/ds for steepest descent
c          CALL1       logical key; first or subsequent call
c          CLIP        Lambda clipping indicator
c          DLAMAX      maximum change allowed in ELAMs for Newton-Raphson
c          DS          path length in ELAM space
c          DSUP        DS below which upgrade from more 1 is attempted
c          DWDS        dW/ds on steepest descent path
c          DYDS        dW*/ds* on steepest ascent path
c          ERRH        allowed fractional error in any H
c          ERRHT       maximum tolerant  ERRH
c          ERRZ        allowed fractional error in Z
c          FDW         damping factor to suppress W valley oscillations
c          FDY         damping factor to suppress Y valley oscillations
c          FDPMA       maximum PMOL fractional change in W* ascent
c          FDPMC       fraction of PMOL change used for illegal phase demise
c          FDPMI3      maximum PMOL fractional (of PMMAX) increase in mode 3
c          FDPMD3      maximum PMOL fractional (of PMOL) decrease in mode 3
c          FDPMV       fraction of illegal vanishing change accepted
c          FDPYR       fraction for reduction of a large phase seeking absence
c          FDPYV       fraction of largest PMOL below which a phase may vanish
c          FDPY        maximum fraction of max PMOL for phase activation
c          KTRH        number of times the LAM changes have been halved
c          KTRM        number of passes made in the current mode
c          KTRMAX      maximum KTRE allowed
c          KTRY        Y (mols adjustment) descent counter
c          KUP         mode upgrade control
c          MODE        mode number
c          MS          matrix size in phase mols adjustment
c          PMMAX       maximum PMOL
c          REHM        maximum relative error found for any HA(K)
c          REHCM       maximum relative error found for any HC(K)
c          REHMD       value of REHM above which a mode downgrade is made
c          REHMU       value of REHM below which a mode upgrade is attempted
c          WF          sum{PMOL(M)*[Z(M)-1]} - sum{ELAM(K)*PA(IB(K)}
c          WP          WF value on previous pass
c          Y           WF at the minimum in ELAM at fixed PMOL; also called W*
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
       CHARACTER*2 ATOM
       CHARACTER*8 CHEM
       LOGICAL     CLIP
c------------------------------------------------------------------------------
       DIMENSION   ATOM(NAMAX),CHEM(NSMAX),IW(NIW),RW(NRW)
c------------------------------------------------------------------------------
c    pointers
       COMMON /SJEPTR/
     ;   IoKERR,IoKMON,IoKTRE,IoKUMO,IoNA,IoNB,IoNP,IoNS,IoIB,IoIBO,
     ;   IoJB,IoJBAL,IoJBA,IoJBB,IoJBO,IoJBX,IoJS2,IoKB,IoKB2,IoKBA,
     ;   IoKBB,IoKBO,IoKPC,IoKPCX,IoLB2,IoMPA,IoMPJ,IoN,LoN,IoNSP,
     ;   IoFRND,IoHUGE,IoALPH,IoR2,IoR3,IoA,LoA,IoB,LoB,IoBBAL,
     ;   IoCM,LoCM,IoD,LoD,IoDC,LoDC,IoDPML,IoDLAM,IoDLAY,IoE,
     ;   LoE,IoEEQN,IoELAM,IoELMA,IoELMB,IoF,IoG,IoHA,IoHC,IoPA,
     ;   IoPC,IoPMOL,IoQ,LoQ,IoQC,LoQC,IoRC,LoRC,IoRL,IoRP,
     ;   IoSMOA,IoSMOB,IoSMOO,IoSMOL,IoSMUL,IoW,IoX,IoXO,IoY,IoZ
c------------------------------------------------------------------------------
c    iteration limit
       DATA KTRMAX/200/
c------------------------------------------------------------------------------
c    error parameters for mode change
       AEZMD = 0.3
       AEZMU = 0.2
       REHMD = 0.4
       REHMU = 0.3
       DSUP  = 0.3
c    tolerant error parameters
       AEZMF = 1.E-2
       ERRHT = 1.0E-3
c    final convergence error limits
       ERRZ = 1.0E-10
       ERRH = 1.E-8
c    change limits
       DLAMAX = 2.0
       FDPMA = 0.5
       FDPMI3 = 0.3
       FDPMD3 = 0.3
c    W* ascent change limits
       FDPY = 0.3
       FDPYR = 0.8
       FDPYV = 0.02
       FDPMV = 0.8
       FDPMC = 0.9
c    constants
       ONE = 1.0
       ZERO = 0
c------------------------------------------------------------------------------
c    get parameters
       NA = IW(IoNA)
       NP = IW(IoNP)
       NS = IW(IoNS)
       KMON = IW(IoKMON)
       KUMO = IW(IoKUMO)
c
c    monitor
       IF (KMON.GT.1)  THEN
           WRITE (KUMO,1) RW(IoALPH)
1          FORMAT (/' Equilibrium solution monitor:'//
     ;              '   Atom      mols',10x,'alpha=',e15.7)
           WRITE (KUMO,2) (ATOM(I),RW(IoPA+I),I=1,NA)
2          FORMAT (4X,A,1PE14.4)
           WRITE (KUMO,3) (ATOM(I),I=1,NA)
3          FORMAT (/' Species      g/RT  ',20(1X,A2))
           DO 5 J=1,NS
              WRITE (KUMO,4) CHEM(J),RW(IoG+J),(IW(IoN+I+LoN*J),I=1,NA)
4             FORMAT (1X,A8,F10.3,(20I3))
5             CONTINUE
           ENDIF
c
c    initialization
       IF (KINIT.NE.0) THEN
c            set species phase cross-reference
               J2 = 0
               DO 7 M=1,NP
                   J1 = J2 + 1
                   J2 = J2 + IW(IoNSP+M)
                   DO 6 J=J1,J2
                       IW(IoMPJ+J) = M
6                      CONTINUE
7                  CONTINUE
c            estimate phase mols and element potentials
               CALL SJINIT(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW)
               IF (IW(IoKERR).NE.0)  RETURN
c            load NB
               NB = IW(IoNB)
           ELSE
c            estimate element potentials using current mol fractions
               K = 1
               CALL SJIESL(NSMAX,NIW,NRW,CHEM,IW,RW,K,DUMMY,IDUMMY)
               IF (IW(IoKERR).NE.0)  RETURN
           ENDIF
c
c ** initialize counters and controls for the element potential solution
c
c    set counters
       IW(IoKTRE) = 0
       KTRM = 0
       KTRH = 0
       KTRY = 0
c    set to start in mode 3
       MODE = 3
       KUP = 0
c    set initial damping for steepest descent
       FDW = 1
       FDY = 1
c    set active phases
       DO 9 M=1,NP
           IF (RW(IoPMOL+M).NE.ZERO)  THEN
                    IW(IoKPC+M) = 1
                ELSE
                    IW(IoKPC+M) = 0
                ENDIF
9          CONTINUE
c
c **** Loop point for ELAM and PMOL adjustments  *****************
c
c    check for convergence failure
10     IF (IW(IoKTRE).GT.KTRMAX) THEN
           IW(IoKERR) = 2
           RETURN
           ENDIF
c
c    increment counters
       IW(IoKTRE) = IW(IoKTRE) + 1
       KTRM = KTRM + 1
c
c ** compute mol fractions and Z  for each phase and W
c
c    initialize
       AEZM = 0
       MAEZM = 0
       WF = 0
c    phase contributions
       J2 = 0
       DO 19  M=1,NP
           J1 = J2 + 1
           J2 = J2 + IW(IoNSP+M)
           IoZM = IoZ + M
           RW(IoZM) = 0
c        species contributions
           DO 17 J=J1,J2
c            check for inclusion
               IF (IW(IoKB+J).NE.4)  THEN
                   SUM = - RW(IoG+J)
c                element potential contributions
                   DO 15 K=1,NB
                       I = IW(IoIB+K)
                       SUM = SUM + RW(IoELAM+K)*IW(IoN+I+LoN*J)
15                     CONTINUE
                   RW(IoX+J) = SJUEXP(SUM)
                   RW(IoZM) = RW(IoZM) + RW(IoX+J)
                   ENDIF
17             CONTINUE
           WF = WF + RW(IoPMOL+M)*RW(IoZM)
c
c        compute Z error
           AEZ = RW(IoZM) - 1
c        check vs maximum
           IF (IW(IoKPC+M).NE.0)  THEN
c                 the phase is active
                   IF (DABS(AEZ).GT.AEZM)  THEN
                       AEZM = DABS(AEZ)
                       MAEZM = M
                       ENDIF
               ELSE
c                 if Z ge 1 the phase should be active
                   IF (AEZ.GE.ZERO)  AEZM = AEZ
               ENDIF
c
19         CONTINUE
c
c ** calculate  D,DC, HA, HC, and QC and evaluate the relative errors
c
c    initialize
       REHM = 0
       REHCM = 0
c
       DO 29 K =1,NB
c        population term in WF
           IK = IW(IoIB+K)
           WF = WF - RW(IoELAM+K)*RW(IoPA+IK)
c        initialize
           IoHAK = IoHA + K
           IoHCK = IoHC + K
           IoPAIK = IoPA+IK
           IoPCK = IoPC + K
           RW(IoHAK) = - RW(IoPAIK)
           RW(IoHCK) = - RW(IoPCK)
           TERMX = DABS(RW(IoPAIK))
           TERMCX = DABS(RW(IoPCK))
           DO 21 L=1,NB
               RW(IoQC+K+LoQC*L) = 0
21             CONTINUE
c        phase contributions
           J2 = 0
           DSUM = 0
           DO 27 M=1,NP
               J1 = J2 + 1
               J2 = J2 + IW(IoNSP+M)
               IoDKM = IoD + K + LoD*M
               IoDCKM = IoDC + K + LoDC*M
               RW(IoDKM) = 0
               RW(IoDCKM) = 0
               TDM = 0
               TDCM = 0
c            species contributions
               DO 25 J=J1,J2
c                check for inclusion
                   IF (IW(IoKB+J).NE.4)  THEN
c                    contributions to D and H and major terms in H
                       TERM = IW(IoN+IK+LoN*J)*RW(IoX+J)
                       CALL SJUMAX(TERM,TDM)
                       RW(IoDKM) = RW(IoDKM) + TERM
c                    check for active phase
                       IF (IW(IoKPC+M).NE.0)  THEN
                           TERM = TERM*RW(IoPMOL+M)
                           CALL SJUMAX(TERM,TERMX)
                           RW(IoHAK) = RW(IoHAK) + TERM
                           ENDIF
c                    contributions to conditioned D, H, and Q
                       IF ((IW(IoKB+J).NE.1).OR.(IW(IoJB+K).EQ.J)) THEN
                           IF (IW(IoKB+J).EQ.1)  THEN
c                                species J is the Kth base species
                                   TERM = 1
                               ELSE
c                                species J is not a base
                                   TERM = 0
                                   DO 23 L=1,NB
                                       IL = IW(IoIB+L)
                                       TERM = TERM + RW(IoCM+K+LoCM*L)*
     ;                                               IW(IoN+IL+LoN*J)
23                                     CONTINUE
                               ENDIF
                           TERM = TERM*RW(IoX+J)
                           CALL SJUMAX(TERM,TDCM)
                           RW(IoDCKM) = RW(IoDCKM) + TERM
                           IF (IW(IoKPC+M).NE.0)  THEN
                               TERM = TERM*RW(IoPMOL+M)
                               CALL SJUMAX(TERM,TERMCX)
                               RW(IoHCK) = RW(IoHCK) + TERM
                               DO 24 L=1,NB
                                   IL = IW(IoIB+L)
                                   IoQCKL = IoQC + K + LoQC*L
                                   RW(IoQCKL) = RW(IoQCKL) +
     ;                                 TERM*IW(IoN+IL+LoN*J)
24                                 CONTINUE
                               ENDIF
                           ENDIF
                       ENDIF
25                 CONTINUE
               CALL SJURND(RW(IoDKM),TDM)
               CALL SJURND(RW(IoDCKM),TDCM)
27             CONTINUE
c
c        compute relative errors in HA(K) and compare vs maximum
           IF (TERMX.NE.ZERO)  THEN
                   REH = DABS(RW(IoHAK))/TERMX
               ELSE
                   REH = 0
               ENDIF
           CALL SJUMAX(REH,REHM)
c
c        compute relative errors in HC(K) and compare vs maximum
           IF (TERMCX.NE.ZERO)  THEN
                   REHC = DABS(RW(IoHCK))/TERMCX
               ELSE
                   REHC = 0
               ENDIF
           CALL SJUMAX(REHC,REHCM)
c
29         CONTINUE
c
c ** monitor
c
c    instructional monitor
       IF (KMON.GT.1)  THEN
           WRITE (KUMO,60) IW(IoKTRE),WF
60         FORMAT (/' Equilibrium solution pass ',I3,
     ;              ';   dual function W =',1PE20.12)
           WRITE (KUMO,61) (M,RW(IoPMOL+M),RW(IoZ+M),M=1,NP)
61         FORMAT  ('   phase ',I2,' mols =',1PE15.8,
     ;              ';  mol fraction sum Z =',E20.12)
           DO 63 K=1,NB
               I = IW(IoIB+K)
               WRITE (KUMO,62) ATOM(I),RW(IoELAM+K),RW(IoHA+K)
62             FORMAT  ('   element potential for ',A2,' =',1PE20.12,
     ;                  '; population error =',E11.3)
63             CONTINUE
           WRITE (KUMO,64) REHCM
64         FORMAT ('   maximum conditioned population relative error =',
     ;             1PE12.4)
           DO 69 J1=1,NS,6
               J2 = J1 + 5
               IF (J2.GT.NS) J2 = NS
               WRITE (KUMO,65) (CHEM(J),J=J1,J2)
65             FORMAT  (6X,6(4X,A8))
               WRITE (KUMO,67) (RW(IoX+J),J=J1,J2)
67             FORMAT  ('    X:',6E12.5)
69             CONTINUE
           ENDIF
c
c **** initial convergence check (superaccurate convergence)
c
       IF ((AEZM.LT.RW(IoFRND)).AND.(REHCM.LT.RW(IoFRND))) GOTO 800
c
c **** mode selection/revision ********
c
c    set error limit for constraint convergence
c    (allow more error if Zs are not close)
       ERRHX  = ERRH + AEZM*AEZMF
       IF (ERRHX.GT.ERRHT)  ERRHX = ERRHT
c
c    branch on current mode
       GOTO (110,120,130),  MODE

c
c ** mode 1:  Steepest descent of W in ELAM space at fixed PMOL
c             followed by steepest descent of Y in PMOL space
c
110    IF (KTRM.GT.1)  THEN
c        previous pass also in mode 1; check for W increase
           IF (WF.GT.WP)  THEN
c           WF increased:  check mode trial count
               IF (KTRH.GT.10)  THEN
c                not getting anywhere:  give up
                   IW(IoKERR) = 2
                   RETURN
                   ENDIF
c            cut ELAM changes in half and try again
               DO 117 K=1,NB
                   IoDLAK = IoDLAM + K
                   RW(IoDLAK) = 0.5*RW(IoDLAK)
                   RW(IoELAM+K) = RW(IoELAM+K) - RW(IoDLAK)
117                CONTINUE
               KTRH = KTRH + 1
               KUP = 2
               IF (KMON.GT.1)  WRITE (KUMO,118)
118            FORMAT (' W overshoot;',
     ;                 '  halving element potential changes.')
               GOTO 10
               ENDIF
           ENDIF
       KTRH = 0
c
c    check for upgrade to mode 2 or 3
       IF (KTRM.GT.KUP)  THEN
           IF (DS.LT.DSUP)  THEN
               IF (AEZM.LT.AEZMU)  THEN
                       MODE = 3
                   ELSE
                       MODE = 2
                   ENDIF
               KTRM = 1
               KUP = 0
               ENDIF
           ENDIF
       GOTO 200
c
c **  mode 2:  Newton-Raphson iteration for LAM followed by
c               steepest descent of Y in PMOL space
c
120    IF (KTRM.GT.1)  THEN
c        previous pass was in mode 2; W should have decreased
           IF (WF.GT.WP)  THEN
c            W increased: check numerical limit
               IF (REHM.LE.ERRH)  GOTO 126
c            use last good values in mode 1
               DO 121 K=1,NB
                   RW(IoELAM+K) = RW(IoELAM+K) - RW(IoDLAM+K)
121                CONTINUE
c            set for a fresh descent
               MODE = 1
               KTRM = 0
               KUP = 2
               IF (KMON.GT.1) WRITE (KUMO,122)
122            FORMAT (' W increased: going back to last good point.')
               GOTO 10
               ENDIF
           ENDIF
c
c    check for upgrade to mode 3
126    IF (KTRM.GT.KUP)  THEN
           IF (((REHM.LT.REHMU).AND.(AEZM.LT.AEZMU)).OR.
     ;        (REHM.LT.ERRHX))  THEN
c            set to try mode 3
               MODE = 3
               KTRM = 1
               KUP = 0
               GOTO 200
               ENDIF
           ENDIF
c
c    check for change to mode 1
       IF (REHM.GT.REHMD)  THEN
c        not close enough for mode 2; set for mode 1
           MODE = 1
           KTRM = 1
           KUP = 1
           GOTO 200
           ENDIF
c
c    continue in mode 2
       GOTO 200
c
c ** mode 3:   Newton-Raphson in ELAM and PMOL
c
c    check for errors too large for mode 3
130    IF ((AEZM.GT.AEZMD).OR.(REHM.GT.REHMD))  THEN
c        stay with mode 3 if only phase mols need adjustment
           IF (REHM.LT.ERRH)  GOTO 200
c        errors large; go to mode 2
           MODE = 2
           KTRM = 0
           KTRY = 0
           KUP = 2
           GOTO 120
           ENDIF
c
c **** calculations after a good pass
c
c    save WF from this pass
200    WP = WF
c
c    HA convergence test for mode 1 or 2
250    IF (MODE.NE.3)  THEN
c      if HA error is less than adjusted error bound ERRHX, go adjust PMOL
           IF (REHM.LT.ERRHX)  GOTO 600
           ENDIF
c
c    branch on mode
       GOTO (400,500,700),  MODE
c
c **** LAM adjustments in mode 1
c
c    valley check
400    IF (KTRM.GT.1)  THEN
c        compute dW/ds at the new point on the old path
           DWDSN = 0
           DO 407 K=1,NB
               DWDSN = DWDSN + RW(IoRL+K)*RW(IoHA+K)
407            CONTINUE
c        check for a valley
           IF (DWDS*DWDSN.LT.ZERO)  THEN
c            interpolate in the valley
               TERM =  - DABS(DWDSN/(DWDSN - DWDS))
               DO 409 K=1,NB
                   RW(IoELAM+K) = RW(IoELAM+K) + TERM*RW(IoDLAM+K)
409                CONTINUE
c            monitor
               IF (KMON.GT.1)  WRITE (KUMO,410)
410            FORMAT (' W valley interpolation')
c            set to take a fresh descent
               KTRM = 0
               GOTO 10
               ENDIF
           ENDIF
c
c    determine the dW/ds = beta
       BETA2 = 0
       DO 419 K=1,NB
           BETA2 = BETA2 + RW(IoHA+K)*RW(IoHA+K)
419        CONTINUE
c
c    we are at the minimum W point if BETA2 = 0
       IF (BETA2.EQ.ZERO)  GOTO 600
       DWDS = - DSQRT(BETA2)
c
c    determine sum{Q(K,L)*HA(K)*HA(L)}/BETA2
       RBETA = 1/DWDS
       QHSOB2 = 0
c    phase contributions
       J2 = 0
       DO 429 M=1,NP
           J1 = J2 + 1
           J2 = J2 + IW(IoNSP+M)
c        check for active phase
           IF (IW(IoKPC+M).NE.0)  THEN
c            species contributions
               PSUM = 0
               DO 427 J=J1,J2
c               check for inclusion
                   IF (IW(IoKB+J).NE.4)  THEN
                       SUM = 0
                       DO 425 K=1,NB
                           I = IW(IoIB+K)
                           SUM  = SUM + IW(IoN+I+LoN*J)*RW(IoHA+K)
425                        CONTINUE
c                    dividing by Beta here helps prevent overflow
                       SUM = SUM*RBETA
                       PSUM = PSUM + SUM*SUM*RW(IoX+J)
                       ENDIF
427                CONTINUE
               QHSOB2 = QHSOB2 + PSUM*RW(IoPMOL+M)
               ENDIF
429        CONTINUE
c
c    estimate ds to the minimum point
       IF (QHSOB2.LE.ZERO)  THEN
c        something is wrong;  quit
           IW(IoKERR) = 1
           RETURN
           ENDIF
       DS = - FDW*DWDS/QHSOB2
c
c    limit changes
       IF (DS.GT.DLAMAX)  THEN
           DS = DLAMAX
           ENDIF
c
c    set direction cosines, changes, and new values
       FDW = 1
       DO 439 K=1,NB
           RLX = RW(IoHA+K)*RBETA
c        check for oscillation
           IF (KTRM.GT.1) THEN
               IF (RLX*RW(IoRL+K).LT.ZERO)  THEN
c                oscillating; reset damping factor
                   FDWX = DABS(1 - 0.9*DABS(RLX))
                   IF (FDWX.LT.FDW)  FDW = FDWX
                   ENDIF
               ENDIF
           RW(IoRL+K) = RLX
           RW(IoDLAM+K) = RLX*DS
           RW(IoELAM+K) = RW(IoELAM+K) + RW(IoDLAM+K)
439        CONTINUE
c
c    monitor
       IF (KMON.GT.1)  THEN
           WRITE (KUMO,442)
442        FORMAT (' Element potentials adjusted by',
     ;             ' steepest descent in W')
           ENDIF
c
       GOTO 10
c
c **** LAM adjustments in mode 2
c
c    calculate the LAM changes
500    DO  529 K=1,NB
c        load the rhs = - (conditioned H vector) in W
           RW(IoW+K) = - RW(IoHC+K)
c        load the conditioned Q matrix in A
           DO 527 L=1,NB
               RW(IoA+K+LoA*L) = RW(IoQC+K+LoQC*L)
527            CONTINUE
529        CONTINUE
c    singularity check/repair
       CALL SJECKA(NSMAX,NIW,NRW,CHEM,IW,RW,NB)
c    solve to get DLAM
       CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),NB,IW(IoKERR))
c    check for trouble
       IF (IW(IoKERR).NE.0)  THEN
c        singular system; go try mode 1
           IF (KMON.GT.1)  WRITE (KUMO,530)
530        FORMAT (' Singular matrix in Newton-Raphson',
     ;             ' adjustment of element potentials')
           MODE = 1
           KUP = 2
           KTRM = 0
           GOTO 250
           ENDIF
c
c    check for excessive changes
       DO 539 K=1,NB
           IF (DABS(RW(IoW+K)).GT.DLAMAX)  THEN
c            changes too large
               IF (KMON.GT.1)  THEN
                   WRITE (KUMO,532)
532                FORMAT (' Attempted Newton-Raphson for',
     ;               ' element potentials; changes too large.'/
     ;               ' Continuing steepest descent.')
                   ENDIF
c            restart in mode 1
               MODE = 1
               KTRM = 0
               KUP = 1
               GOTO 10
               ENDIF
539        CONTINUE
c
c    make the changes
       DO 549 K=1,NB
           RW(IoDLAM+K) = RW(IoW+K)
           RW(IoELAM+K) = RW(IoELAM+K) + RW(IoDLAM+K)
549        CONTINUE
c
       IF (KMON.GT.1)  WRITE (KUMO,552)
552    FORMAT (' Element potentials adjusted by Newton-Raphson')
c
c      go do another pass
       GOTO 10
c
c      Y denotes W* =  Wmin, the maximum of which we seek in phase mols space
c
c    advance pass counter
600    KTRY = KTRY + 1
c
c    ridge interpolation check
       IF (KTRY.GT.1)  THEN
c        compute dW*/ds* at the new point on the old path
           DYDSN = 0
           DO 601 M=1,NP
               IF (IW(IoKPC+M).NE.0)
     ;            DYDSN = DYDSN + (RW(IoZ+M) - 1)*RW(IoRP+M)
601            CONTINUE
c        check for a ridge
           IF (DYDSN*DYDS.LT.ZERO)  THEN
c            interpolate for the ridge
               TERM = - DABS(DYDSN/(DYDSN - DYDS))
               DO 605 M=1,NP
                   RW(IoPMOL+M) = RW(IoPMOL+M) + TERM*RW(IoDPML+M)
605                CONTINUE
               DO 607 K=1,NB
                   RW(IoELAM+K) = RW(IoELAM+K) + TERM*RW(IoDLAY+K)
607                CONTINUE
c            monitor
               IF (KMON.GT.1)  WRITE (KUMO,608)
608            FORMAT (' W* ridge interpolation')
c            set for a fresh ascent in W*
               KTRY = 0
               KTRM = 0
               GOTO 10
               ENDIF
           ENDIF
c
c    calculate the E vectors for each phase
       DO 617 M=1,NP
           DO 612 K=1,NB
c            load the rhs =  - (conditioned D) in W
               RW(IoW+K) = - RW(IoDC+K+LoDC*M)
c            load the conditioned Q matrix in A
               DO 611 L=1,NB
                   RW(IoA+K+LoA*L) = RW(IoQC+K+LoQC*L)
611                CONTINUE
612            CONTINUE
c        singularity check/repair
           CALL SJECKA(NSMAX,NIW,NRW,CHEM,IW,RW,NB)
c        solve to get E
           CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),NB,IW(IoKERR))
c        check for problems
           IF (IW(IoKERR).NE.0)  THEN
c            set E zero and keep trying
               DO 613 K=1,NB
                   RW(IoW+K) = 0
613                CONTINUE
               ENDIF
c        load the E vector
           DO 615 K=1,NB
               RW(IoE+K+LoE*M) = RW(IoW+K)
615            CONTINUE
617        CONTINUE
c
c    activate all phases and get maximum phase mols
       PMMAX = 0
       DO 619 M=1,NP
           CALL SJUMAX(RW(IoPMOL+M),PMMAX)
           IW(IoKPC+M) = 1
619        CONTINUE

c    compute the cosine coefficients in W
620    SUM = 0
       DO 629 M=1,NP
           IoWM = IoW+M
c        check for active phase
           IF (IW(IoKPC+M).NE.0)  THEN
               RW(IoWM) = (RW(IoZ+M) - 1)
c            check for an empty phase with W < 0
               IF ((RW(IoPMOL+M).EQ.ZERO).AND.(RW(IoWM).LT.ZERO)) THEN
c                set this phase inactive and start over
                   IW(IoKPC+M) = 0
                   GOTO 620
                   ENDIF
               SUM = SUM + RW(IoWM)*RW(IoWM)
               ENDIF
629        CONTINUE
c
c    compute dY/ds on path of steepest ascent
       DYDS =  DSQRT(SUM)
       IF (DYDS.EQ.ZERO)  THEN
c        check for convergence
           IF (REHCM.LT.ERRH) GOTO 800
           IW(IoKERR) = 1
           RETURN
           ENDIF
c
c    compute direction cosines and select damping factor FDYF
       RDYDS = 1/DYDS
       FDYF = 1
       DO 631 M=1,NP
           IF (IW(IoKPC+M).NE.0)  THEN
c                active phase
                   RX = RW(IoW+M)*RDYDS
c                set damping
                   IF (KTRY.GT.1)  THEN
                       IF (RX*RW(IoRP+M).LT.ZERO)  THEN
                           FDYFX = 1 - 0.8*DABS(RX)
                           IF (FDYFX.LT.FDYF)  FDYF = FDYFX
                           ENDIF
                       ENDIF
                   RW(IoRP+M) = RX
               ELSE
c                inactive phase
                   RW(IoRP+M) = 0
               ENDIF
631        CONTINUE
c
c  set damping coefficient FDY
       IF (FDYF.EQ.ONE)  THEN
c            reduce the damping if no longer needed
               IF (FDY.LT.ONE) THEN
                   FDY = 1.4*FDY
                   IF (FDY.GT.ONE)  FDY = 1
                   ENDIF
           ELSE
               IF (KTRY.EQ.1)  THEN
c                    set undamped at the start of an ascent
                       FDY = 1
                   ELSE
c                    increase damping if more is needed
                       FDY = FDY*FDYF
                   ENDIF
           ENDIF
c
c    estimate the distance to the maximum point
       D2YDS2 = 0
       DO 639 L=1,NP
c        check for active phase
           IF (IW(IoKPC+L).NE.0)  THEN
c            phase L active
               SUM = 0
               DO 637 M=1,NP
c                check for active phase
                   IF (IW(IoKPC+M).NE.0)  THEN
c                    Mth phase active:  compute A(L,M)
                       ALM = 0
                       DO 635 K=1,NB
                           ALM = ALM + RW(IoD+K+LoD*L)*RW(IoE+K+LoE*M)
635                        CONTINUE
                       SUM = SUM + ALM*RW(IoRP+M)
                       ENDIF
637                CONTINUE
               D2YDS2 = D2YDS2 + SUM*RW(IoRP+L)
               ENDIF
639        CONTINUE
c
c    check for completion
       IF (D2YDS2.GE.ZERO)  THEN
c            set a reasonable change
               DSP = PMMAX
           ELSE
c            set distance as (damped) estimate to maximum point
               DSP = - FDY*DYDS/D2YDS2
           ENDIF
       DSP0 = DSP
c
c    limit phase mols change
       TERMY = FDPY*PMMAX
       DO 641 M=1,NP
           IF (IW(IoKPC+M).NE.0)  THEN
c            active phase
               IoPMOM = IoPMOL + M
               TERM = RW(IoRP+M)*DSP
               IF ((RW(IoPMOM)+TERM).LE.RW(IoFRND)*RW(IoPMOM))  THEN
c                    phase attempting to vanish
                       IF (RW(IoPMOM).LT.FDPYV*PMMAX)  THEN
c                            size step to make PMOL vanish
                               DSP = - RW(IoPMOM)/RW(IoRP+M)
                           ELSE
c                            size step to reduce PMOL by a factor
                               DSP = - FDPYR*RW(IoPMOM)/RW(IoRP+M)
                           ENDIF
                   ELSE
c                    phase not attempting to vanish
                       IF (RW(IoPMOM).NE.ZERO)  THEN
c                            existing phase
                               TERMX = FDPMA*RW(IoPMOM)
                               IF (DABS(TERM).GT.TERMX)  THEN
                                  DSP = DSP*DABS(TERMX/TERM)
                                  ENDIF
                           ELSE
c                            non-existing phase
                               IF (DABS(TERM).GT.TERMY)  THEN
                                  DSP = DSP*DABS(TERMY/TERM)
                                  ENDIF
                           ENDIF
                   ENDIF
               ENDIF
641        CONTINUE
c
c    check for completion
       IF (DSP.EQ.ZERO)  GOTO 800
c
c    make the phase mol changes and prepare for possible rebasing
       KPMC = 0
       KRED = 0
       DO 649 M=1,NP
           IW(IoKPCX+M) = IW(IoKPC+M)
           IoPMOM = IoPMOL + M
           IoDPMM = IoDPML + M
           IF (IW(IoKPC+M).NE.0)  THEN
c               active phase
c                 set the change
                   RW(IoDPMM) = RW(IoRP+M)*DSP
c                 no termination on phase activation
                   IF (RW(IoPMOM).EQ.ZERO)  THEN
                       KPMC = 1
                       KRED = 1
                       ENDIF
c                 make the change with roundoff control
                   RW(IoPMOM) = RW(IoPMOM) + RW(IoDPMM)
                   IF (RW(IoPMOM).LT.RW(IoFRND)*DABS(RW(IoDPMM)))
     ;              THEN
                       RW(IoPMOM) = 0
                       KPMC = 1
                       KRED = 1
                       IW(IoKPCX+M) = 0
                       ENDIF
c                 relative error assessment
                   IF (IW(IoKPCX+M).NE.ZERO)  THEN
c                       be tolerant with sparse phases
                          TERM = ERRH*(10.*DLOG(PMMAX/RW(IoPMOM)) + 1)
                          IF (DABS(RW(IoDPMM)).GT.TERM*RW(IoPMOM))
     ;                        KPMC = 1
                       ELSE
c                        can not terminate on a phase deactivation
                           KPMC = 1
                       ENDIF
               ELSE
c               inactive phase
                   RW(IoDPMM) = 0
               ENDIF
649        CONTINUE
c
c    check for phase activation/demise
       IF (KRED.NE.0)  THEN
c        check and rebase if ok
           CALL SJERB(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW)
           IF (IW(IoKERR).NE.0)  THEN
c            unacceptable changes; reduce and go
               TERM = 1 - FDPMC
               DO 657 M=1,NP
                   IoPMOM = IoPMOL + M
                   IoDPMM = IoDPML + M
                   RW(IoPMOM) = RW(IoPMOM) - TERM*RW(IoDPMM)
                   RW(IoDPMM) = FDPMC*RW(IoDPMM)
657                CONTINUE
               ENDIF
           ENDIF
c
c    estimate the associated changes in ELAMs
       DLX = 0
       DO 665 K = 1,NB
           IoDLYK = IoDLAY + K
           RW(IoDLYK) = 0
           DO 663 M=1,NP
               IF (IW(IoKPC+M).EQ.0)  GOTO 663
               RW(IoDLYK) = RW(IoDLYK) + RW(IoE+K+LoE*M)*RW(IoDPML+M)
663            CONTINUE
           CALL SJUMAX(RW(IoDLYK),DLX)
665        CONTINUE
c
c    make the ELAM changes if not excessive
       IF (DLX.LT.DLAMAX)  THEN
               DO 667 K=1,NB
                   RW(IoELAM+K) = RW(IoELAM+K) + RW(IoDLAY+K)
667                CONTINUE
           ELSE
               DO 669 K=1,NB
                   RW(IoDLAY+K) = 0
669                CONTINUE
           ENDIF
c
c    reset active phase indicator
       DO 679 M=1,NP
           IW(IoKPC+M) = IW(IoKPCX+M)
679        CONTINUE
c
c    monitor
       IF (KMON.GT.1)  THEN
           WRITE (KUMO,680)
680        FORMAT (' phase mols adjusted by steepest ascent in W*')
           ENDIF
c
c    convergence check
       IF ((REHCM.LT.ERRH).AND.(AEZM.LT.ERRZ))  THEN
           IF ((DLX.LT.ERRH).AND.(KPMC.EQ.0))  GOTO 800
           ENDIF
c
c    start a new W descent
       KTRM = 0
       KUP = 0
       GOTO 10
c
c **** ELAM and PMOL adjustments in mode 3
c
c    if constraints accurately satisfied but phasemols not, go adjust in Mode 2
700    IF (REHCM.LT.RW(IoFRND))  THEN
           MODE = 2
           KUP = 0
           KTRY = 0
           GOTO 600
           ENDIF
c
c    set up the active phase list and get maximum phase mols
       NPA = 0
       PMMAX = 0
       DO 701 M=1,NP
           IF ((RW(IoPMOL+M).GT.ZERO).OR.(RW(IoZ+M).GE.ONE))  THEN
c                set phase active
                   IW(IoKPC+M) = 1
                   NPA = NPA + 1
                   IW(IoMPA+NPA) = M
                   CALL SJUMAX(RW(IoPMOL+M),PMMAX)
               ELSE
c                set phase inactive
                   IW(IoKPC+M) = 0
               ENDIF
701        CONTINUE
c
c    set matrix size for solution for ELAM and PMOL
       MS = NB + NPA
c    load the matrix
       DO 707 K=1,NB
c        load the rhs for rows 1-NB
           RW(IoW+K) = - RW(IoHC+K)
c        load the last NPA columns and rows
           DO 703 M1=1,NPA
               L = NB + M1
               M = IW(IoMPA+M1)
               RW(IoA+K+LoA*L) = RW(IoDC+K+LoDC*M)
               RW(IoA+L+LoA*K) = RW(IoD+K+LoD*M)
703            CONTINUE
c        load the conditioned Q matrix in the first NB columns (and rows)
           DO 705 L=1,NB
               RW(IoA+K+LoA*L) = RW(IoQC+K+LoQC*L)
705            CONTINUE
707        CONTINUE
c    load the last NPA x NPA block and rhs
       DO  709 M1=1,NPA
           L = NB + M1
           M = IW(IoMPA+M1)
           RW(IoW+L) = 1 - RW(IoZ+M)
           DO 708 M2=1,NPA
               K = NB + M2
               RW(IoA+K+LoA*L) = 0
708            CONTINUE
709        CONTINUE
c
c    singularity check/repair
       CALL SJECKA(NSMAX,NIW,NRW,CHEM,IW,RW,MS)
c
c    solve to get changes in ELAMs and PMOL
       CALL SJULES(LoA,RW(IoA+1+LoA),RW(IoW+1),MS,IW(IoKERR))
c    check for problems
       IF (IW(IoKERR).NE.0)  THEN
c        singular matrix; try mode 2
           IF (KMON.GT.1)  WRITE (KUMO,710)
710        FORMAT (' Singular matrix in full Newton-Raphson')
           MODE = 2
           KUP = 2
           KTRM = 0
           GOTO 250
           ENDIF
c
c -- check for excessive change
       DLX = 0
       CLIP = .FALSE.
c    limit the maximum LAM changes
       DO 713 K=1,NB
           IF (DABS(RW(IoW+K)).GT.DLAMAX)  THEN
               RW(IoW+K) = DLAMAX*RW(IoW+K)/DABS(RW(IoW+K))
               CLIP = .TRUE.
               ENDIF
           CALL SJUMAX(RW(IoW+K),DLX)
713        CONTINUE
c
c    check phase changes
       DO 733 M1=1,NPA
           K = NB + M1
           M = IW(IoMPA+M1)
           IoWK = IoW + K
           IF (RW(IoPMOL+M).EQ.ZERO)  THEN
c                phase does not exist; permit no decrease
                   IF (RW(IoWK).LT.ZERO)  GOTO 750
c                allow a reasonable increase
                   TEST = FDPMI3*PMMAX
               ELSE
c                phase exists
                   IF (RW(IoWK).GT.ZERO) THEN
c                        allow a large increase
                           TEST = FDPMI3*PMMAX
                       ELSE
c                        allow a reasonable decrease
                           TEST = FDPMD3*RW(IoPMOL+M)
                       ENDIF
               ENDIF
           IF (DABS(RW(IoWK)).GE.TEST)  GOTO 750
733        CONTINUE
c
c -- make the changes and check for phase convergence
       DPMX = 0
       DO 741 K=1,NB
           RW(IoDLAY+K) = RW(IoW+K)
           RW(IoELAM+K) = RW(IoELAM+K) + RW(IoDLAY+K)
741        CONTINUE
       K = NB
       KPMC = 0
       DO 747 M=1,NP
           IoPMOM = IoPMOL + M
           IoDPMM = IoDPML + M
           IF (IW(IoKPC+M).NE.0)  THEN
c                active phase
                   K = K + 1
                   RW(IoDPMM) = RW(IoW+K)
c                prevent termination on phase activation
                   IF (RW(IoPMOM).EQ.ZERO)  KPMC = 1
c                make the change
                   RW(IoPMOM) = RW(IoPMOM) + RW(IoDPMM)
c                relative error assessment; tolerant with sparse phases
                   IF (RW(IoPMOM).NE.ZERO)  THEN
                          TERM = ERRH*(10.*DLOG(PMMAX/RW(IoPMOM)) + 1)
                          IF (DABS(RW(IoDPMM)).GT.TERM*RW(IoPMOM))
     ;                         KPMC = 1
                       ELSE
c                        hopefully never get here
                           KPMC = 1
                       ENDIF
               ELSE
c                inactive phase
                   RW(IoDPMM) = 0
               ENDIF
747        CONTINUE
c
       IF (KMON.GT.1)  THEN
           WRITE (KUMO,748)
748        FORMAT (' Element potentials and phase mols adjusted by',
     ;         ' Newton-Raphson')
           IF (CLIP) WRITE (KUMO,749)
749        FORMAT (' with limited element potential changes')
           ENDIF
c
c *** convergence check ***
c    must have small errors
       IF ((REHCM.LT.ERRH).AND.(AEZM.LT.ERRZ))  THEN
c        and be making small changes
           IF ((DLX.LT.ERRH).AND.(KPMC.EQ.0))  GOTO 800
           ENDIF
c
c    continue the iteration as if a fresh W* ascent
       KTRY = 0
       GOTO 10
c
c -- changes too large; reset for mode 2 and a fresh W* ascent
750    MODE = 2
       KTRM = 0
       KTRY = 0
       KUP = 2
       IF (KMON.GT.1)  THEN
           WRITE (KUMO,752)
752        FORMAT (' Attempted full Newton-Raphson; changes excessive')
           ENDIF
       GOTO 250
c
c ---- exit calculations
c
c    normal return
800    IW(IoKERR) = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SJECKA(NSMAX,NIW,NRW,CHEM,IW,RW,MS)
c
c      Checks the first NB rows of matrix A of dimension MS used in SJEQLB
c      to see if it is singular (because of an absent base).  Replaces
c      bad rows with the condition that the base mol fraction not change.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in the argument list:
c @        NSMAX       maximum number of species
c @        NIW         dimension of work array IW
c @        NRW         dimension of work array IR
c          IW(I)       integer work array
c          RW(I)       REAL*8 work array
c @        CHEM(J)     CHARACTER*8 name of Jth species
c @        MS          matrix size (row count)
c
c      Variables in the integer work array IW:
c @        IB(K) = I   if the Kth independent atom is the Ith system atom
c @        JB(K) = J   if the Kth base is the Jth species
c @        N(I,J)      number of Ith atoms in Jth molecule
c @        NB          number of bases
c
c      Variables in the real work array RW:
c @#       A(K,L)      work matrix
c @#       W(K)        work vector
c
c      Variables used only internally:
c          KCHK        check control
c------------------------------------------------------------------------------
       IMPLICIT        REAL*8  (A-H,O-Z)
       CHARACTER*8     CHEM
c------------------------------------------------------------------------------
       DIMENSION   CHEM(NSMAX),IW(NIW),RW(NRW),IEPTR(80)
c------------------------------------------------------------------------------
       COMMON /SJEPTR/ IEPTR
       EQUIVALENCE (IoIB,IEPTR(9)),(IoJB,IEPTR(11)),
     ;   (IoNB,IEPTR(6)),(IoN,IEPTR(28)), (LoN,IEPTR(29)),
     ;   (IoA,IEPTR(36)),(LoA,IEPTR(37)),(IoW,IEPTR(76))
c------------------------------------------------------------------------------
c    set comparison constant
       ZERO = 0
c
c    get parameters
       NB = IW(IoNB)
c
c    check the matrix
       DO 99 K=1,NB
           KCHK = 0
           DO 91 L=1,MS
               IF (RW(IoA+K+LoA*L).NE.ZERO)  KCHK = 1
91             CONTINUE
           IF (KCHK.EQ.0)  THEN
c            replace the row to prevent singularities
c            this treatment holds X(JB(K)) fixed in ELAM adjustments
               J = IW(IoJB+K)
               RW(IoW+K) = 0
               DO 95 L=1,NB
                   I = IW(IoIB+L)
                   RW(IoA+K+LoA*L) = IW(IoN+I+LoN*J)
95                 CONTINUE
               IF (MS.GT.NB)  THEN
                   L1 = NB + 1
                   DO 97 L=L1,MS
                       RW(IoA+K+LoA*L) = 0
97                     CONTINUE
                   ENDIF
               ENDIF
99         CONTINUE
       RETURN
       END
c******************************************************************************
