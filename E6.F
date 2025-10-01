c******************************************************************************
c
c      Rebasing
c
c******************************************************************************
c
       SUBROUTINE SJERB(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW)
c
c      Rebases the system following a change in the active phases.
c      Checks for permissibility of the demise of phase(s).
c------------------------------------------------------------------------------
c      At call:
c
c          KPCX contains the proposed active phase list
c
c      On return:
c
c          If successful, KERR = 0 and the base system is reset.
c
c          KERR = 1 if the phase change is unfeasible.
c------------------------------------------------------------------------------
c      Sets up a provisional system excluding the species in the absent
c      phase(s). Uses SJISMP to find the base set for this system.  If the
c      populations are impossible in this subset of species, or if the
c      number of bases are changed, the rebasing is not allowed.
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
c          RW(I)       real work array
c
c      Variables in the integer work array IW:
c @#       IB(K) = I   if the Ith atom is the Kth independent system atom
c -        IBO(K) = I  saved IB
c @#       JB(K) = J   if the Jth system species is the Kth base species
c -        JBO(K)      saved JB
c @#       KB(J)       basis control
c -        KBO(J)      saved KB
c #        KERR        error flag
c @        KPCX(K)     trial phase control; 0 if phase empty, 1 if populated
c @        N(I,J)      number of Ith atoms in Jth species
c @        NA          number of atom types
c @        NB          number of base species
c @        NP          number of phases
c @        NS          number of species
c @        NSP(M)      number of species in Mth phase
c
c      Variables used only internally:
c -        NBO         NB for current system
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
       CHARACTER*2 ATOM
       CHARACTER*8 CHEM
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
c    get parameters
       NP = IW(IoNP)
       NS = IW(IoNS)
c
c    save the present base parameters and load trials
       NBO = IW(IoNB)
       DO 9 K=1,NBO
           IW(IoIBO+K) = IW(IoIB+K)
           IW(IoJBO+K) = IW(IoJB+K)
9          CONTINUE
       DO 19 J=1,NS
           IW(IoKBO+J) = IW(IoKB+J)
19         CONTINUE
c
c    set basis control for rebasing
       J2 = 0
       DO 39 M=1,NP
           J1 =J2 + 1
           J2 = J2 + IW(IoNSP+M)
           DO 29 J=J1,J2
               IF (IW(IoKPCX+M).NE.0)  THEN
c                   phase will remain allow all non-excluded species
                       IF (IW(IoKB+J).NE.4)  IW(IoKB+J) = 0
                   ELSE
c                    phase will be absent
                       IW(IoKB+J) = 4
                   ENDIF
29             CONTINUE
39         CONTINUE
c
c    obtain the provisional base (include phase considerations)
       K = 1
       CALL SJISMP(NAMAX,NSMAX,NIW,NRW,ATOM,CHEM,IW,RW,K)
       IF (IW(IoKERR).NE.0)  RETURN
       NB = IW(IoNB)
c
c    check for illegal rebasing
       IF ((IW(IoKERR).NE.0).OR.(IW(IoNB).NE.NBO))  THEN
c        rebasing failed; restore old bases
           IW(IoNB) = NBO
           DO 43 K=1,NB
               IW(IoIB+K) = IW(IoIBO+K)
               IW(IoJB+K) = IW(IoJBO+K)
43             CONTINUE
           DO 49 J=1,NS
               IW(IoKB+J) = IW(IoKBO+J)
49             CONTINUE
           IW(IoKERR) = 1
           RETURN
           ENDIF
c
c    set the base controls
50     DO 59 J=1,NS
c        remove false exclusion flags
           IF ((IW(IoKB+J).EQ.4).AND.(IW(IoKBO+J).NE.4)) IW(IoKB+J) = 0
59         CONTINUE
c
c    calculate conditioning matrix and conditioned populations
       L = 0
       CALL SJICPC(NIW,NRW,IW,RW,L)
       RETURN
       END
c******************************************************************************
