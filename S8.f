c******************************************************************************
c
c      Routines for reading old unformatted files. This must be compiled
c      with special compiler switches.
c
c      For Digital Visual Fortran, compiler settings for this file should be
c      for PowerPC 4.0 i/o format and library compatibility, unformatted file
c      conversion = none.
c
c******************************************************************************
c
       SUBROUTINE OUFOVV(IU,FNAME,IERR)
c
c      Opens unformatted file FNAME on unit IU. Returns IERR=0 if OK, else 1.
c------------------------------------------------------------------------------
       CHARACTER*32  FNAME
c------------------------------------------------------------------------------
       OPEN (IU,FILE=FNAME,FORM='UNFORMATTED',ERR=10)
       IERR = 0
       RETURN
10     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE RSEOVV(IUF,IERR)
c
c      Reads V1 or V1.5 Microsoft compiled .ESS file opened on unit IUF
c      and loads for V2.
c------------------------------------------------------------------------------
c    designators
       CHARACTER*48    NEGM,NEOP,NEMP,NAMB,NICC,NECC,NFLM,NTBM,NVVC,
     ;                 NPCC,NPDS,NFGM,
     ;                 NEGR,NFGT,NFPM,NHTM,NIMM,NEMM,NVRA,NRUN,
     ;                 NXXX,NOLDS,UNSET,NGPM,NIVP,NEVP,NPMP
       CHARACTER*1     NGPMX(48),STRING(80),CU
       CHARACTER*32    NAMEF
c------------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION   IW(50),RW(400)
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
c    old work array
       INTEGER*2   IW2(50)
c-----------------------------------------------------------------------------
       EQUIVALENCE (KIVP,IW(18))
       EQUIVALENCE (KEVP,IW(19))
c    Note: in V1 Microsoft-compiled the valve program type 2 was user, but
c          in V2 type 3 is user.
c-----------------------------------------------------------------------------
c    run setup
       COMMON /NRUNVV/ NRUN
c    engine geometry model
       COMMON /NEGMVV/ NEGM
c    operating parameters
       COMMON /NEOPVV/ NEOP
c    model parameters
       COMMON /NEMPVV/ NEMP
c    ambient conditions
       COMMON /NAMBVV/ NAMB
c    intake valve cosine-flat-cosine
       COMMON /NICCVV /NICC
c    exhaust valve cosine-flat-cosine
       COMMON /NECCVV /NECC
c    valve reference areas
       COMMON /NVRAVV /NVRA
c    valve control
       COMMON /NVVCVV /NVVC
c    EGR
       COMMON /NEGRVV /NEGR
c    piston/cylinder conventional crankshaft
       COMMON /NPCCVV/ NPCC
c    piston/cylinder dual stroke
       COMMON /NPDSVV/ NPDS
c    flow model
       COMMON /NFLMVV /NFLM
c    turbulence model
       COMMON /NTBMVV /NTBM
c    flame geometry table
       COMMON /NFGTVV /NFGT
c    flame propagation model
       COMMON /NFPMVV /NFPM
c    gas properties model
       COMMON /NGPMVV /NGPM
c    heat transfer model
       COMMON /NHTMVV /NHTM
c    intake manifold model
       COMMON /NIMMVV /NIMM
c    exhaust manifold model
       COMMON /NEMMVV /NEMM
c    inlet valve profile name
       COMMON /NIVPVV/NIVP
c    exhaust valve profile name
       COMMON /NEVPVV/NEVP
c-----------------------------------------------------------------------------
c    user valve programs
       DIMENSION IVDI(48),FVDI(48)
       DIMENSION IVDE(48),FVDE(48)
       COMMON  /UVIDVV/ NDI,FMXI,IVDI,FVDI
       COMMON  /UVEDVV/ NDE,FMXE,IVDE,FVDE
c-----------------------------------------------------------------------------
       DATA  NOLDS/'Loaded from old-style setup file                '/
c----------------------------------------------------------------------------
c    unset designators for WCR's interface system
       IU = -22222
       RU = -1.1111111E+11
       CU = CHAR(239)
c
c     construct an unset string
        DO I=1,48
           STRING(I) = CU
           ENDDO
        CALL ASETVV(CU,UNSET)
c
c *   Read the old file
c
c    assume style 1
       KESS = 1
c
c    set work array length
10     IF (KESS.EQ.1)  THEN
c            style 1 with 300 RW fields
               NDRW = 300
               DO I=301,400
                   RW(I) = RU
                   ENDDO
           ELSE
c            style 2 with 400 RW fields
               NDRW = 400
           ENDIF
c
c    read the work arrays
        READ(IUF,END=800,ERR=800) IW2,(RW(I),I=1,NDRW)
c    convert integers
       DO I=1,50
           IW(I) = IW2(I)
           ENDDO
c    read the properties data
       CALL RTPOVV(IUF,IERR)
       IF (IERR.NE.0) GOTO 800
c    read the flame geometry table
       CALL RFGOVV(IUF,IERR)
       IF (IERR.NE.0) GOTO 800
c    read the names of parameters and property sets
       IF (KESS.EQ.1)  THEN
c            style 1
               READ (IUF,END=800,ERR=800)  NEGM,NEOP,
     ;         NEVP,NFGM,NFLM,NFPM,NGPMX,NHTM,NIVP,NPMP,NTBM
c           check for user-specified inlet valve program
              IF (KIVP.EQ.2)  THEN
                  CALL RVPOVV(IUF,NIVP,NDI,FMXI,IVDI,FVDI,IERR)
                  IF (IERR.NE.0) GOTO 800
c               change code for V2+
                  KIVP = 4
                  ENDIF
c           check for user-specified exhaust valve program
              IF (KEVP.EQ.2)  THEN
                  CALL RVPOVV(IUF,NEVP,NDE,FMAE,IVDE,FVDE,IERR)
                  IF (IERR.NE.0) GOTO 800
c               change code for V2+
                  KEVP = 4
                  ENDIF
c            set defaults for style 1 given options in style 2
               IW(6) = 1
               DO I=17,20
                   IW(I) = 1
                   ENDDO
               RW(217) = 0
c            set null data not in style 1 files
               DO I=24,29
                   IW(I) = IU
                   ENDDO
c            check ignition
               IF ((IW(11).LT.0).OR.(IW(11).GT.710)) THEN
c                set for motoring
                   IW(11) = IU
                   IW(6) = 2
                   ENDIF
c            set for loaded table
               IW(17) = 2
c            set for no setup save
               IW(35) = 2
c            set for loaded flame geometry table not saved
               IW(36) = 2
               IW(37) = 1
               IW(38) = 2
c            set for no manifold
               IW(39) = 1
               IW(40) = 1
c            convert NGPMX
                CALL ACONVV(NGPMX,NGPM)
c            set new designators
                NRUN = NOLDS
                NEMP = NOLDS
                NAMB = NOLDS
                NICC = UNSET
                NECC = UNSET
                NVRA = NOLDS
                NVVC = NOLDS
                NEGR = NOLDS
                NPCC = NOLDS
                NPDS = UNSET
                NFGT = NFGM
                NIMM = UNSET
                NEMM = UNSET
c            convert NGPMX
                CALL ACONVV(NGPMX,NGPM)
           ELSE
c            style 2
               READ (IUF,END=800,ERR=800)  NEGM,NEOP,
     ;         NEVP,NFGM,NFLM,NFPM,NGPM,NHTM,NIVP,NPMP,NTBM,NIMM,NEMM
c            set for no setup save
               IW(35) = 2
c            set for flame geometry table not saved
               IW(36) = 2
c            set new designators
               NRUN = NOLDS
               NEMP = NOLDS
               NAMB = NOLDS
               NICC = UNSET
               NECC = UNSET
               NVRA = NOLDS
               NVVC = NOLDS
               NEGR = NOLDS
               NPCC = NOLDS
               NPDS = UNSET
               NFGT = NFGM
               NIMM = UNSET
               NEMM = UNSET
           ENDIF
c    check ignition
       IF ((IW(11).LT.0).OR.(IW(11).GT.719)) THEN
c        set for motoring
           IW(11) = IU
           IW(6) = 2
           ENDIF
c    check old designators
       CALL NFIXVV(NEGM)
       CALL NFIXVV(NEOP)
       CALL NFIXVV(NGPM)
       CALL NFIXVV(NHTM)
       CALL NFIXVV(NTBM)
       CALL NFIXVV(NFLM)
       CALL NFIXVV(NFPM)

c    wrap it up
       CLOSE(IUF)
       IERR = 0
       RETURN
c
c   file read failure
800    IF (KESS.EQ.1) THEN
c        try style 2
           KESS = 2
           REWIND(IUF)
           GOTO 10
           ENDIF
c    unresolveable error
       CLOSE(IUF)
       IERR = 1
       RETURN
c
       END
c******************************************************************************
c
       SUBROUTINE OMUFVV(NAME,IU,IERR)
c
c      Opens Microsoft-compiled unformatted file NAME on unit IU
c------------------------------------------------------------------------------
       CHARACTER*32    NAME
c------------------------------------------------------------------------------
       OPEN(IU,FILE=NAME,FORM='UNFORMATTED',ERR=90)
         IERR=0
         RETURN
c    opening error
90     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RTPOVV(IU,IERR)
c
c      Reads old V1 thermodynamic property file opened on unit IU. File left
c      open. Sets up for interpolations.
c
c      Returns IERR = 0 if ok, 1 if no file read.
c------------------------------------------------------------------------------
       CHARACTER*8     CHEM
       CHARACTER*48    NGPM
c------------------------------------------------------------------------------
c    Array dimensions
       PARAMETER (IMAX = 48)
       PARAMETER (NSMAX = 50)
c------------------------------------------------------------------------------
       DIMENSION   CHEM(NSMAX),RMOLS(NSMAX)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX)
c------------------------------------------------------------------------------
c    Properties file data
       COMMON  /NGPMVV/ NGPM
       COMMON  /PROCVV/ CHEM
       COMMON  /PRONVV/ NF,NO,NP
       COMMON  /PRODVV/ HP,HR,PVP,PVR
       COMMON  /PROPVV/ PP,PR,RFT,RMOLS
c------------------------------------------------------------------------------
       INTEGER*2        NF2,NO2,NP2
c------------------------------------------------------------------------------
c   read the file
      READ (IU,ERR=20,END=20)  NGPM,RFT,NF2,NO2,PR
c   check name
       CALL NFIXVV(NGPM)
c    convert integers
       NF = NF2
       NO = NO2
       NR = NF + NO
       READ (IU,ERR=20,END=20)  (CHEM(J),J=1,NR),(RMOLS(J),J=1,NR)
       READ (IU,ERR=20,END=20)  (HR(J),J=1,IMAX),(PVR(J),J=1,IMAX)
       READ (IU,ERR=20,END=20)  NP2,PP
       NP = NP2
       J1 = NR + 1
       J2 = NR + NP
       READ (IU,ERR=20,END=20)  (CHEM(J),J=J1,J2),
     ;             (HP(J),J=1,IMAX),(PVP(J),J=1,IMAX)
       IERR = 0
       RETURN
c
c    error reading file
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RFGOVV(IU,IERR)
c
c      Reads old V1 flame geometry table file opened on unit IU. Leaves open.
c      Returns IERR = 0 read, 1 if not.
c------------------------------------------------------------------------------
       CHARACTER*48    NFGT
       DIMENSION       FAHB(11),RFAP(11)
c------------------------------------------------------------------------------
c    Flame geometry table
       COMMON /FGMTVV/ FAHB,RFAP
       COMMON /NFGTVV/ NFGT
c------------------------------------------------------------------------------
c    read the file
       READ (IU,ERR=20,END=20) NFGT,(FAHB(I),I=1,11),(RFAP(I),I=1,11)
       CALL NFIXVV(NFGT)
       IERR = 0
       RETURN
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RVPOVV(IU,NUVP,ND,FMAX,IVD,FVD,IERR)
c
c      Reads an old-style valve program from opened unit IU.
c      Returns IERR = 0 ok, 1 if not.
c------------------------------------------------------------------------------
c      Nomeclature:
c          NUVP    user valve program name
c          ND      number of data points
c          FMAX    maximum value of relative lift
c          IVD     list of specified angles from valv open angle, deg (1,ND)
c          FVD     list of corresponding lifts (arbitrary units)
c------------------------------------------------------------------------------
       CHARACTER*48    NUVP
       INTEGER*2       ND2,IVD2(48)
       DIMENSION       FVD(48),IVD(48)
c------------------------------------------------------------------------------
c    read the file
       READ (IU,ERR=20,END=20) NUVP,ND2,FMAX
       ND = ND2
       READ (IU,ERR=20,END=20) (IVD2(I),I=1,ND),(FVD(I),I=1,ND)
       CALL NFIXVV(NUVP)
c    convert integers
       DO I=1,ND
           IVD(I) = IVD2(I)
           ENDDO
       IERR = 0
       RETURN
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE NFIXVV(NAME)
c
c      Replaces blank designator
c------------------------------------------------------------------------------
       CHARACTER*48    NAME,BLANK,NONE
       DATA BLANK/'                                                '/
       DATA NONE /'Undesignated data from old style ESP file       '/
c------------------------------------------------------------------------------
       IF (NAME.EQ.BLANK) NAME = NONE
       RETURN
       END
c******************************************************************************

