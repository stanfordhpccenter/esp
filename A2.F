c******************************************************************************
c
c      Stanjan pointer setup routines
c
c******************************************************************************
c
       SUBROUTINE SJSPTS(NAMAX,NPMAX,NSMAX,NIW,NRW,NSW,RW)
c
c      Sets SJSET pointers and checks specified work array dimensions.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in argument list
c
c          NAMAX       maximum number of atom types
c          NPMAX       maximum number of phases
c          NSMAX       maximum number of species
c          NIW         dimension of work array IW
c          NRW         dimension of work array RW
c          NSW         dimension of work array SW
c          RW(I)       REAL*8 work array
c------------------------------------------------------------------------------
c    Targets:
c      NW = max{2*NA,NA+NP}
c      NIW = 22 + 14*NA + 4*NP + 8*NS + 2*NA*NS
c      NRW = 24 + 16*NA + 12*NA*NA + 3*NP*NA + 6*NP +
c    ;       18*NS + NW*NW + NW
c      NSW = 123*NS
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8      (A-H,O-Z)
c------------------------------------------------------------------------------
       DIMENSION   RW(NRW)
c------------------------------------------------------------------------------
c  10 additional pointers required by SJSET
       COMMON /SJSPTR/
     ;   IoKNFS,IoKSME,IoKUFL,IoNAF,IoNSF,IoJFS,IoNF,LoNF,IoITHL,IoITHM
c------------------------------------------------------------------------------
c   set pointers required by SJROP, SJTP and SJEQLB
       NIWZ = NIW - 5 - 3*NSMAX - NAMAX*NSMAX
       NRWZ = NRW
       CALL SJRPTS(NAMAX,NPMAX,NSMAX,NIWZ,NRWZ,NSW,RW)
       IoKNFS = NIWZ + 1
       IoKSME = IoKNFS + 1
       IoKUFL = IoKSME + 1
       IoNAF = IoKUFL + 1
       IoNSF = IoNAF + 1
       IoJFS = IoNSF
       LoNF = NAMAX
       IoNF = IoJFS + NSMAX - LoNF
       IoITHL = IoNF + NAMAX*NSMAX + LoNF
       IoITHM = IoITHL + NSMAX
       NIWX = IoITHM + NSMAX
c    check
       IF (NIWX.NE.NIW) THEN
           GOTO 90
c        diagnostics
C           WRITE (*,1) NAMAX,NPMAX,NSMAX
C1          FORMAT (/' SJSET dimensioning error for NAMAX =',I3,
C     ;              '  NPMAX =',I3,'  NSMAX =',I3)
C           WRITE (*,2) NIWX
C2          FORMAT (/'  NIWORK should be',I6)
C           STOP
           ENDIF
c    SW pointers are not explicit
       NSWX = 123*NSMAX
       IF (NSWX.NE.NSW)  THEN
           GOTO 90
c        diagnostics
C           WRITE (KU,1) NAMAX,NPMAX,NSMAX
C           WRITE (KU,4) NSWX
C4          FORMAT (/'  NSWORK should be ',I6)
C           STOP
           ENDIF
c
c    ok return
       RETURN
c
c    error
90     CALL WARNZZ('@','@SJS pointer error. Unable to '//
     ;     'build ESP. Refer to program author.@@')
       STOP
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SJRPTS(NAMAX,NPMAX,NSMAX,NIW,NRW,NSW,RW)
c
c      Sets SJROP pointers and checks specified work array dimensions.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in argument list
c
c          NAMAX       maximum number of atom types
c          NPMAX       maximum number of phases
c          NSMAX       maximum number of species
c          NIW         dimension of work array IW
c          NRW         dimension of work array RW
c          RW(I)       REAL*8 work array
c          KU          output unit for error message
c------------------------------------------------------------------------------
c    Targets:
c      NW = max{2*NA,NA+NP}
c      NIW = 17 + 14*NA + 4*NP + 5*NS + NA*NS
c      NRW = 24 + 16*NA + 12*NA*NA + 3*NA*NP + 6*NP +
c    ;      18*NS + NW*NW + NW
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8      (A-H,O-Z)
c------------------------------------------------------------------------------
       DIMENSION   RW(NRW)
c------------------------------------------------------------------------------
c  20 additional pointers required by SJROP
       COMMON/ SJRPTR/
     ;   IoKDET,IoKRCT,IoKSND,IoKTRI,IoKTRT,IoI2,IoI3,IoNCON,IoTE,IoC,
     ;   IoCDET,IoH1,IoP1,IoT1,IoV1,IoR5,IoR6,IoSMFA,IoSMFB,IoSMOZ
c------------------------------------------------------------------------------
c   set pointers required by SJTP and SJEQLB
       NIWZ = NIW - 8
       NRWZ = NRW - 9 - 3*NSMAX
       CALL SJTPTS(NAMAX,NPMAX,NSMAX,NIWZ,NRWZ,NSW,RW)
c  ** IW pointers
       IoKDET = NIWZ + 1
       IoKRCT = IoKDET + 1
       IoKSND = IoKRCT + 1
       IoKTRI = IoKSND + 1
       IoKTRT = IoKTRI + 1
       IoI2 = IoKTRT + 1
       IoI3 = IoI2 + 1
       IoNCON = IoI3 + 1
       NIWX = IoNCON
c    check
       IF (NIWX.NE.NIW) THEN
           GOTO 90
c        diagnostics
C           WRITE (KU,1) NAMAX,NPMAX,NSMAX
C1          FORMAT (/' SJROP dimensioning error for NAMAX =',I3,
C     ;              '  NPMAX =',I3,'  NSMAX =',I3)
C           WRITE (KU,2) NIWX
C2          FORMAT (/'  NIWORK error: NIWX = ',I6)
C           STOP
           ENDIF
c  ** RW pointers
       IoTE = NRWZ + 1
       IoC = IoTE + 1
       IoCDET = IoC + 1
       IoH1 = IoCDET + 1
       IoP1 = IoH1 + 1
       IoT1 = IoP1 + 1
       IoV1 = IoT1 + 1
       IoR5 = IoV1 + 1
       IoR6 = IoR5 + 1
       IoSMOZ = IoR6
       IoSMFA = IoSMOZ + NSMAX
       IoSMFB = IoSMFA + NSMAX
       NRWX = IoSMFB + NSMAX
c    check
       IF (NRWX.NE.NRW) THEN
           GOTO 90
c        diagnostics
C           WRITE (KU,1) NAMAX,NPMAX,NSMAX
C           WRITE (KU,4) NRWX
C4          FORMAT (/'  NRWORK should be = ',I6)
C           STOP
           ENDIF
c    ok return
       RETURN
c
c    error
90     CALL WARNZZ('@','@SJR pointer error. Unable to '//
     ;     'build ESP. Refer to program author.@@')
       STOP
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SJTPTS(NAMAX,NPMAX,NSMAX,NIW,NRW,NSW,RW)
c
c      Sets SJTP pointers and checks specified work array dimensions.
c      Loads physical constants and conversion factors.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in argument list
c
c          NAMAX       maximum number of atom types
c          NPMAX       maximum number of phases
c          NSMAX       maximum number of species
c          NIW         dimension of work array IW
c          NRW         dimension of work array RW
c          RW(I)       REAL*8 work array
c          KU          output unit for error message
c
c      Variables in work array RW:
c          CVCJ        J/cal
c          PATM        Pa/atm
c          RGAS        gas constant, cal/mol-K
c------------------------------------------------------------------------------
c    Targets:
c      NW = max{2*NA,NA+NP}
c      NIW = 9 + 14*NA + 4*NP + 5*NS + NA*NS
c      NRW = 15 + 16*NA + 12*NA*NA + 3*NA*NP + 6*NP +
c    ;      15*NS + NW*NW + NW
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8      (A-H,O-Z)
c------------------------------------------------------------------------------
       DIMENSION   RW(NRW)
c------------------------------------------------------------------------------
c  20 additional pointers required by  SJTP
       COMMON /SJTPTR/
     ;   IoKFRZ,IoCVCJ,IoPATM,IoRGAS,IoP,IoT,IoH,IoS,IoU,IoV,
     ;   IoWM,IoTP,IoDCS,IoDHF0,IoWMS,IoHMH0,IoS0,IoWMP,IoXM,IoYM
c------------------------------------------------------------------------------
c    Physical constants
       CVCJ = 4.184
       PATM = 101325.
       RGAS = 1.987165D0
c------------------------------------------------------------------------------
c   set pointers required by SJEQLB
       NIWZ = NIW - 1
       NRWZ = NRW - 10 - 2*NPMAX - 7*NSMAX
       CALL SJEPTS(NAMAX,NPMAX,NSMAX,NIWZ,NRWZ,NSW,RW)
c  ** IW pointers
       IoKFRZ = NIWZ + 1
c    check IW dimension
       NIWX =  IoKFRZ
       IF (NIWX.NE.NIW) THEN
           WRITE (*,1) NAMAX,NPMAX,NSMAX
1          FORMAT (/' SJTP dimensioning error for NAMAX =',I3,
     ;              '  NPMAX =',I3,'  NSMAX =',I3)
           WRITE (*,2) NIWX
2          FORMAT (/'  NIWORK error; NIWX =',I6)
           STOP
           ENDIF
C ** RW pointers
       IoCVCJ = NRWZ + 1
       IoPATM = IoCVCJ + 1
       IoRGAS = IoPATM + 1
       IoP = IoRGAS + 1
       IoT = IoP + 1
       IoH = IoT + 1
       IoS = IoH + 1
       IoU = IoS + 1
       IoV = IoU + 1
       IoWM = IoV + 1
       IoTP = IoWM
       IoDCS = IoTP + NPMAX
       IoDHF0 = IoDCS + NSMAX
       IoWMS = IoDHF0 + NSMAX
       IoHMH0 = IoWMS + NSMAX
       IoS0 = IoHMH0 + NSMAX
       IoWMP = IoS0 + NSMAX
       IoXM = IoWMP + NPMAX
       IoYM = IoXM + NSMAX
c    check RW dimension
       NRWX = IoYM + NSMAX
       IF (NRWX.NE.NRW) THEN
               GOTO 90
c            diagnostics
C               WRITE (KU,1) NAMAX,NPMAX,NSMAX
C               WRITE (KU,4) NRWX
C4              FORMAT (/'  NRWORK error; NRWX =',I6)
C               STOP
           ELSE
c            load constants
               RW(IoCVCJ) = CVCJ
               RW(IoPATM) = PATM
               RW(IoRGAS) = RGAS
           ENDIF
c    ok return
       RETURN
c
c    error
90     CALL WARNZZ('@','@SJT pointer error. Unable to '//
     ;     'build ESP. Refer to program author.@@')
       STOP
       END
c******************************************************************************
c
       SUBROUTINE SJEPTS(NAMAX,NPMAX,NSMAX,NIW,NRW,NSW,RW)
c
c      Sets SJEQLB pointers and checks specified work array dimensions.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c      Variables in argument list
c
c          NAMAX       maximum number of atom types
c          NPMAX       maximum number of phases
c          NSMAX       maximum number of species
c          NIW         dimension of work array IW
c          NRW         dimension of work array RW
c          RW(I)       REAL*8 work array
c          KU          output unit for error message
c------------------------------------------------------------------------------
c    Targets:
c      NW = max{2*NA,NA+NP}
c      NIW = 8 + 14*NA + 4*NP + 5*NS + NA*NS
c      NRW = 5 + 16*NA + 12*NA*NA + 3*NA*NP + 4*NP +
c    ;       8*NS + NW*NW + NW
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8      (A-H,O-Z)
c------------------------------------------------------------------------------
       DIMENSION   RW(NRW)
c------------------------------------------------------------------------------
c 80 pointers required by SJEQLB
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
c    round-off factor in computation precision (1.D-12 for REAL*8)
       FRND = 1.E-12
c------------------------------------------------------------------------------
c    length combinations
       IF (NPMAX.GT.NAMAX)  THEN
               NW = NAMAX + NPMAX
           ELSE
               NW = NAMAX + NAMAX
           ENDIF
       NAMAX2 = 2*NAMAX
c ** IW pointers:
       IoKERR = 1
       IoKMON = IoKERR + 1
       IoKTRE = IoKMON + 1
       IoKUMO = IoKTRE + 1
       IoNA = IoKUMO + 1
       IoNB = IoNA + 1
       IoNP = IoNB + 1
       IoNS = IoNP + 1
       IoIB = IoNS
       IoIBO = IoIB + NAMAX
       IoJB = IoIBO + NAMAX
       IoJBAL = IoJB + NAMAX
       IoJBA = IoJBAL + NAMAX
       IoJBB = IoJBA + NAMAX
       IoJBO = IoJBB + NAMAX
       IoJBX = IoJBO + NAMAX
       IoJS2 = IoJBX + NAMAX
       IoKB = IoJS2 + 2*NAMAX
       IoKB2 = IoKB + NSMAX + NAMAX
       IoKBA = IoKB2 + 2*NAMAX
       IoKBB = IoKBA + NSMAX
       IoKBO = IoKBB + NSMAX
       IoKPC = IoKBO + NSMAX
       IoKPCX = IoKPC + NPMAX
       IoLB2 = IoKPCX + NPMAX
       IoMPA = IoLB2 + NAMAX
       IoMPJ = IoMPA + NPMAX
       LoN = NAMAX
       IoN = IoMPJ + NSMAX - LoN
       IoNSP = IoN + NAMAX*NSMAX + LoN
c    check IW dimension
       NIWX =  IoNSP + NPMAX
       IF (NIWX.NE.NIW) THEN
           GOTO 90
c        diagnostics
C           WRITE (KU,1) NAMAX,NPMAX,NSMAX
C1          FORMAT (/' SJEQLB dimensioning error for NAMAX =',I3,
C     ;              '  NPMAX =',I3,'  NSMAX =',I3)
C           WRITE (KU,2) NIWX
C2          FORMAT (/'  NIWORK error; NIWX =',I6)
C           STOP
           ENDIF
c
c ** RW pointers:
       IoFRND = 1
       IoHUGE = IoFRND + 1
       IoR1 = IoHUGE + 1
       IoR2 = IoR1 + 1
       IoR3 = IoR2 + 1
       LoA = NW
       IoA = IoR3 - LoA
       LoB = NAMAX2
       IoB = IoA + NW*NW + LoA - LoB
       IoBBAL = IoB + NAMAX2*NAMAX2 + LoB
       LoCM = NAMAX
       IoCM = IoBBAL + NAMAX - LoCM
       LoD = NAMAX
       IoD = IoCM + NAMAX*NAMAX + LoCM - LoD
       LoDC = NAMAX
       IoDC = IoD + NAMAX*NPMAX + LoD - LoDC
       IoDPML = IoDC + NAMAX*NPMAX + LoDC
       IoDLAM = IoDPML + NPMAX
       IoDLAY = IoDLAM + NAMAX
       LoE = NAMAX
       IoE = IoDLAY + NAMAX - LoE
       IoEEQN = IoE + NAMAX*NPMAX + LoE
       IoELAM = IoEEQN + NAMAX
       IoELMA = IoELAM + NAMAX
       IoELMB = IoELMA + NAMAX
       IoF = IoELMB + NAMAX
       IoG = IoF + NSMAX
       IoHA = IoG + NSMAX
       IoHC = IoHA + NAMAX
       IoPA = IoHC + NAMAX
       IoPC = IoPA + NAMAX
       IoPMOL = IoPC + NAMAX
       LoQ = NAMAX2
       IoQ = IoPMOL + NPMAX - LoQ
       LoQC = NAMAX
       IoQC = IoQ + NAMAX2*NAMAX2 + LoQ - LoQC
       LoRC = 2*NAMAX
       IoRC = IoQC + NAMAX*NAMAX + LoQC - LoRC
       IoRL = IoRC + 2*NAMAX*NAMAX + LoRC
       IoRP = IoRL + NAMAX
       IoSMOA = IoRP + NPMAX
       IoSMOB = IoSMOA + NSMAX
       IoSMOO = IoSMOB + NSMAX
       IoSMOL = IoSMOO + NSMAX
       IoSMUL = IoSMOL + NSMAX + NAMAX
       IoW = IoSMUL + NAMAX
       IoX = IoW + NW
       IoXO = IoX + NSMAX
       IoY = IoXO + NSMAX
       IoZ = IoY + NAMAX2
       NRWX = IoZ + NPMAX
c    check RW dimension
       IF (NRWX.NE.NRW) THEN
               GOTO 90
c        diagnostics
C           WRITE (KU,1) NAMAX,NPMAX,NSMAX
C           WRITE (KU,4) NRWX
C4          FORMAT (/'  NRWORK error; NRWX =',I6)
C           STOP
       ENDIF
c
c    ok return
       RETURN
c
90     CALL WARNZZ('@','@SJE pointer error. Unable to '//
     ;     'build ESP. Refer to program author.@@')
       STOP
       END
c******************************************************************************
