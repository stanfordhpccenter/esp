c******************************************************************************
c
c      Setup file routines
c
c******************************************************************************
c
       SUBROUTINE RSETVV(IU,IERR)
c
c      Reads v2+setup file opened on unit IU.
c      Returns IERR = 0 if ok, 1 if not.
c------------------------------------------------------------------------------
c    designators
       CHARACTER*48 NRUN,NEGM,NEOP,NEMP,NAMB,
     ;              NICC,NECC,NVRA,NVVC,NEGR,NPCC,NPDS,NFLM,NTBM,
     ;              NFGT,NFPM,NGPM,NHTM,NIMM,NEMM,
     ;              NIVP,NEVP
c------------------------------------------------------------------------------
c    Work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
       EQUIVALENCE (KIVP,IW(18))
       EQUIVALENCE (KEVP,IW(19))
c------------------------------------------------------------------------------
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
c    inlet valve program name
       COMMON /NIVPVV/NIVP
c    exhaust valve program name
       COMMON /NEVPVV/NEVP
c-----------------------------------------------------------------------------
c    user valve programs
       DIMENSION AVDI(48),FVDI(48)
       DIMENSION AVDE(48),FVDE(48)
       COMMON  /UVIDVV/ NDI,FMXI,AVDI,FVDI
       COMMON  /UVEDVV/ NDE,FMXE,AVDE,FVDE
c------------------------------------------------------------------------------
c    read the work arrays
       READ(IU,END=90,ERR=90) IW,RW
c    read the properties data
       CALL RTPFVV(IU,IERR)
       IF (IERR.NE.0) GOTO 90
c    read the flame geometry table
       CALL RFGTVV(IU,IERR)
       IF (IERR.NE.0) GOTO 90
c    read the designators
       READ (IU,END=90,ERR=90)  NRUN,NEGM,NEOP,NEMP,NAMB,
     ;              NICC,NECC,NVRA,NVVC,NEGR,NPCC,NPDS,NFLM,NTBM,
     ;              NFGT,NFPM,NGPM,NHTM,NIMM,NEMM
c    check for user-specified inlet valve program
       IF (KIVP.EQ.4)  THEN
           CALL RVPTVV(IU,NIVP,NDI,FMXI,AVDI,FVDI,IERR)
           IF (IERR.NE.0) GOTO 90
           ENDIF
c    check for user-specified exhaust valve program
       IF (KEVP.EQ.4)  THEN
           CALL RVPTVV(IU,NEVP,NDE,FMXE,AVDE,FVDE,IERR)
           IF (IERR.NE.0) GOTO 90
           ENDIF
c     set for no setup save
       IW(35) = 2
c     set for flame geometry table not saved
       IW(36) = 2
c     OK exit with V2 style read
       CLOSE(IU)
       IERR = 0
       RETURN
c
c    error reading V2-style file
90     CLOSE(IU)
       IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE WSETVV(IU,IERR)
c
c      Writes V2 setup file to unit IU; Returns IERR=0 if ok, 1 otherewise.
c------------------------------------------------------------------------------
c    designators
       CHARACTER*48 NRUN,NEGM,NEOP,NEMP,NAMB,
     ;              NICC,NECC,NVRA,NVVC,NEGR,NPCC,NPDS,NFLM,NTBM,
     ;              NFGT,NFPM,NGPM,NHTM,NIMM,NEMM,
     ;              NIVP,NEVP
c------------------------------------------------------------------------------
c    Work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
       EQUIVALENCE (KIVP,IW(18))
       EQUIVALENCE (KEVP,IW(19))
c------------------------------------------------------------------------------
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
c    inlet valve program name
       COMMON /NIVPVV/NIVP
c    exhaust valve program name
       COMMON /NEVPVV/NEVP
c-----------------------------------------------------------------------------
c    user valve programs
       DIMENSION AVDI(48),FVDI(48)
       DIMENSION AVDE(48),FVDE(48)
       COMMON  /UVIDVV/ NDI,FMXI,AVDI,FVDI
       COMMON  /UVEDVV/ NDE,FMXE,AVDE,FVDE
c-----------------------------------------------------------------------------
c    write the work arrays
       WRITE(IU,ERR=80) IW,RW
c    write the properties data
       CALL WTPFVV(IU,IERR)
       IF (IERR.NE.0) GOTO 80
c    write the flame geometry table
       CALL WFGTVV(IU,IERR)
       IF (IERR.NE.0) GOTO 80
c    write the designators
       WRITE (IU,ERR=80)  NRUN,NEGM,NEOP,NEMP,NAMB,
     ;       NICC,NECC,NVRA,NVVC,NEGR,NPCC,NPDS,NFLM,NTBM,
     ;       NFGT,NFPM,NGPM,NHTM,NIMM,NEMM
c    check for user-specified inlet valve program
       IF (KIVP.EQ.4)  THEN
           CALL WVPTVV(IU,NIVP,NDI,FMXI,AVDI,FVDI,IERR)
           IF (IERR.NE.0) GOTO 80
           ENDIF
c    check for user-specified exhaust valve program
       IF (KEVP.EQ.4)  THEN
           CALL WVPTVV(IU,NEVP,NDE,FMXE,AVDE,FVDE,IERR)
           IF (IERR.NE.0) GOTO 80
           ENDIF
       CLOSE(IU)
       RETURN
80     CALL WARNZZ('@','@Error writing setup file.@@')
       CLOSE(IU)
       IERR = 1
       RETURN
       END
c******************************************************************************
c
        SUBROUTINE ACONVV(A,B)
c
c       Converts 48 bytes A to 48-byte string B
c------------------------------------------------------------------------------
        CHARACTER*48 A,B
c------------------------------------------------------------------------------
        B = A
        RETURN
        END
c******************************************************************************
c
        SUBROUTINE ASETVV(A,B)
c
c       Loads 48 bytes of A with B
c------------------------------------------------------------------------------
        CHARACTER*1 A,B(48)
c------------------------------------------------------------------------------
        DO I=1,48
           B(I) = A
           ENDDO
        RETURN
        END
c******************************************************************************

