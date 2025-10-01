c******************************************************************************
c
c      ESP main program
c
c******************************************************************************
c
c      Files to be compiled and linked to build ESP:
c
c      Setup routines
c          S0.F      ESP main program
c          S1.F      model setup routines
c          S2.F      run task setup routines
c          S3.F      quantity identifiers
c          S4.F      gas properties i/o routines
c          S5.F      setup file routines
c          S6.F      flame geometry file routines
c          S7.F      help routines
c          S8.F      reading old-style files (special compiler switches)
c          S9.F      user valve program file routines
c
c      Cycle computation routines
c          C1.F      compression stage routines
c          C2.F      burn stage routines
c          C3.F      expansion stage routines
c          C4.F      gas exchange stage routines
c          C5.F      integration routine
c          C6.F      single cycle analysis routine
c          C7.F      properties routines
c          C8.F      flow rate routine
c          C9.F      setup routines for stage changes
c          C10.F     geometry routines (valves and piston)
c          C11.F     Matlab routines for cylinder
c
c      Output routines
c          D1.F      output display routines
c
c      Manifold routines
c          Y1.F      junction support routines
c          Y2.F      position control routines
c          Y3.F      pointer setup
c          Y4.F      functions
c          Y5.F      junction support
c          Y6.F      junction routine
c          Y7.F      duct routines
c          Y8.F      valve-plenum routine
c          Y9.F      intake-exhaust initialization routines
c         Y10.F      manifold MATLAB plots
c
c        Interface programs    * files depend on the interface used
c
c          IB.F      subroutines called by ESP
c        * I1.F      main interface subroutines
c        * I2.F      interface support routines
c
c        INTWRKZZ.H  interface include file
c
c******************************************************************************
c    general setup work arrays
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
c------------------------------------------------------------------------------
c    application-specific subroutines called by GETZZ
       EXTERNAL    SETMVV,ACTMVV,SETRVV,ACTRVV,HELPVV
c------------------------------------------------------------------------------
       LOGICAL YESZZ,OK
c------------------------------------------------------------------------------
c    ESP work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
c------------------------------------------------------------------------------
c    ESP work array equivalences
       EQUIVALENCE (IRPM,IW(10))
       EQUIVALENCE (KSSU,IW(35))
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
       EQUIVALENCE (NCYC,IW(46))
c------------------------------------------------------------------------------
c    phase-averaged variables and weightings
       DIMENSION   PCAV(720),RHOCAV(720)
       COMMON      /AVCDVV/PCAV,RHOCAV,FCAVO,FCAVN,NCAV
c-----------------------------------------------------------------------------
c    manifold work arrays
       PARAMETER   (NMPTR = 96)
       PARAMETER   (NDRW = 21024)
       PARAMETER   (NDIW = 2904)
       DIMENSION   MPTR(NMPTR)
       DIMENSION   RWI(NDRW)
       DIMENSION   IWI(NDIW)
       DIMENSION   RWE(NDRW)
       DIMENSION   IWE(NDIW)
       COMMON /MPTRXX/ MPTR
       COMMON /RWIXX/  RWI
       COMMON /IWIXX/  IWI
       COMMON /RWEXX/  RWE
       COMMON /IWEXX/  IWE
       EQUIVALENCE (IoNCYL,MPTR(88))
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
       EQUIVALENCE (IAIO,IW(2))
       EQUIVALENCE (IAIC,IW(3))
       EQUIVALENCE (IAEO,IW(4))
       EQUIVALENCE (IAEC,IW(5))
c------------------------------------------------------------------------------
c    manifold diagnostic monitor
c        IUMONM  unit for monitor output
c        MONC    monitor level; 0 none, 1 low level, 2 high level
c        OUT     failure diagnostic monitor; activated by failure
       LOGICAL OUT
       COMMON /MONMXX/IUMONM,MONM,OUT
c------------------------------------------------------------------------------
c    cylinder diagnostic monitor
c        IUMONC  unit for monitor output
c        MONC    monitor level; 0 none ; 1 one-line state summary
       COMMON /MONCVV/IUMONC,MONC
c------------------------------------------------------------------------------
c    Setup file name
       CHARACTER*32  SUFILE
       COMMON /SUFVV/SUFILE
c------------------------------------------------------------------------------
c    Gas properties file name
       CHARACTER*32  PMFILE
       COMMON /PMFVV/PMFILE
c------------------------------------------------------------------------------
c    Inlet valve program file name
       CHARACTER*32  IVFILE
       COMMON /IVFVV/IVFILE
c------------------------------------------------------------------------------
c    Exhaust valve program file name
       CHARACTER*32  EVFILE
       COMMON /EVFVV/EVFILE
c------------------------------------------------------------------------------
c    Flame geometry file name
       CHARACTER*32  FGFILE
       COMMON /FGFVV/FGFILE
c------------------------------------------------------------------------------
c    name of Matlab plot file for indicator
       CHARACTER*32    MPFIND
       COMMON  /MPFCVV/MPFIND
c------------------------------------------------------------------------------
c    name of Matlab plot file for intake manifold
       CHARACTER*32    MPFIM
       COMMON  /MPFIVV/MPFIM
c------------------------------------------------------------------------------
c    name of Matlab plot file for exhaust manifold
       CHARACTER*32    MPFEM
       COMMON  /MPFEVV/MPFEM
c------------------------------------------------------------------------------
c    name of Output file
       CHARACTER*32    OFILE
       COMMON  /OFLEVV/OFIlE
c------------------------------------------------------------------------------
c    run setup
       CHARACTER*48    NMLD
       COMMON /NRUNVV/ NMLD
c------------------------------------------------------------------------------
c    run output control parameters
       DIMENSION   KPLOT(3)
       COMMON     /RUNCVV/ NRUN,NOPR,KPLOT,NOPM
c------------------------------------------------------------------------------
       CHARACTER*48    NIVP,NEVP
c    inlet valve program name
       COMMON /NIVPVV/NIVP
c    exhaust valve program name
       COMMON /NEVPVV/NEVP
c------------------------------------------------------------------------------
c    user valve programs
       DIMENSION AVDI(48),FVDI(48)
       DIMENSION AVDE(48),FVDE(48)
       COMMON  /UVIDVV/ NDI,FMXI,AVDI,FVDI
       COMMON  /UVEDVV/ NDE,FMXE,AVDE,FVDE
c------------------------------------------------------------------------------
c    5-line plot options
       DIMENSION KOP5(6)
       COMMON/KOP5VV/KOP5,KOP5C
c------------------------------------------------------------------------------
c    5-line plot codes
       DIMENSION IDPV(5,6)
       COMMON /IDPVVV/IDPV
c------------------------------------------------------------------------------
c    5-line plot scratch files
       CHARACTER*32 MX5T(6)
       DATA MX5T  /'MX5TMPF1.TMP                    ',
     ;             'MX5TMPF2.TMP                    ',
     ;             'MX5TMPF3.TMP                    ',
     ;             'MX5TMPF4.TMP                    ',
     ;             'MX5TMPF5.TMP                    ',
     ;             'MX5TMPF6.TMP                    '/
c------------------------------------------------------------------------------
c    5-line plot Matlab files
       CHARACTER*32 MPFL(6)
       COMMON /MPFLVV/ MPFL
c------------------------------------------------------------------------------
c    version
       CHARACTER*8     VERS
       COMMON /VERSVV/ VERS
c------------------------------------------------------------------------------
c    line length for non-graphical interface message box
       COMMON /MESBZZ/ LMESS
c------------------------------------------------------------------------------
c    file names for monitor files
       CHARACTER*32    CMONF,MMONF
c==============================================================================
c    set line width for messages
       LMESS = 78
c------------------------------------------------------------------------------
c    set version
       VERS = 'V2.20   '
c------------------------------------------------------------------------------
c    set terminal logical unit for output
       IUMON = 6
c------------------------------------------------------------------------------
c    set work array dimensions
       ID(2) = NCMAXZ
       ID(3) = NIMAXZ
       ID(4) = NRMAXZ
c------------------------------------------------------------------------------
c    load unset indicators (be consistent with SETZZ in file IB.F and
c     RSOVV in file S8.F.
       CD(1) = CHAR(239)
       ID(1) = -22222
       RD(1) = -1.1111111E11
c------------------------------------------------------------------------------
c    set null plot options
       DO I=1,3
           KPLOT(I) = 0
           ENDDO
       DO I=1,5
           KOP5(I)=0
           ENDDO
c------------------------------------------------------------------------------
c    set 5-line plot variable identities
       IDPV(1,1) = 114
       IDPV(2,1) = 9
       IDPV(3,1) = 129
       IDPV(4,1) = 206
       IDPV(5,1) = 105
       IDPV(1,2) = 136
       IDPV(2,2) =   7
       IDPV(3,2) =   3
       IDPV(4,2) =   2
       IDPV(5,2) = 102
       IDPV(1,3) = 120
       IDPV(2,3) = 23
       IDPV(3,3) = 22
       IDPV(4,3) = 110
       IDPV(5,3) = 111
       IDPV(1,4) = 149
       IDPV(2,4) = 147
       IDPV(3,4) = 204
       IDPV(4,4) = 205
       IDPV(5,4) = 103
       IDPV(1,5) = 25
       IDPV(2,5) = 26
       IDPV(3,5) = 21
       IDPV(4,5) = 32
       IDPV(5,5) = 37
       IDPV(1,6) = 144
       IDPV(2,6) = 143
       IDPV(3,6) = 117
       IDPV(4,6) = 118
       IDPV(5,6) = 106
c------------------------------------------------------------------------------
c    set output window scroll (platform/compiler dependent)
       CALL SOWSZZ
c
c    opening header
       CALL HVERVV
c
c    briefing
1      IF (YESZZ('Would you like a briefing on ESP?')) THEN
2          CALL HINSVV
3          IF (YESZZ('Are you ready to try using ESP?'))  GOTO 4
           IF (YESZZ('Do you want to read the briefing again?')) GOTO 2
           IF (YESZZ('Do you want to quit ESP?'))  STOP
           GOTO 3
           ENDIF
c
c    manifold diagnostic monitor
4      IUMONM = 8
       MONM = 0
       OUT = .FALSE.
       MMONF = 'MANIFOLD.MON'
       CALL OPFLVV(IUMONM,MMONF,.FALSE.,.TRUE.,OK)
       IF (.NOT.OK)  GOTO 960
c
c    cylinder diagnostic monitor
       IUMONC = 9
       MONC = 0
       CMONF = 'CYLINDER.MON'
       CALL OPFLVV(IUMONC,CMONF,.FALSE.,.TRUE.,OK)
       IF (.NOT.OK)  GOTO 960
c
c    set the default setup file
       SUFILE = 'SETUP.ESS'
c
c    clear output window
       CALL CLOWZZ

c * read the setup file
c
c    remove previous valve programs
10     NDI = 0
       NDE = 0
c
c    clear prompt window
       CALL CLPWZZ
c
c    do the read
       IERR = 1
       CALL OPFLVV(10,SUFILE,.TRUE.,.FALSE.,OK)
       IF(.NOT.OK)  THEN
           IERR = 1
           GOTO 18
           ENDIF
       CALL RSETVV(10,IERR)
c    file closed
       IF (IERR.NE.0) THEN
c        try old file format
           CALL OMUFVV(SUFILE,10,IERR)
           IF (IERR.EQ.0) THEN
                   CALL RSEOVV(10,IERR)
c                file closed
                   IF (IERR.EQ.0)  CALL FMESVV(2,'@','#',
     ;                'Setup file # was ESP V1 format.  '//
     ;                'Save to convert to V2 format.@',SUFILE)
               ELSE
c                neither format works
                   CALL FMESVV(2,'@','#',
     ;                'Setup file # corrupted.@',SUFILE)
               ENDIF
           ENDIF
c    set data condition
18     IF (IERR.EQ.0)  THEN
c            use setup data
               NIVM = 1
               CALL FMESVV(1,'@','#','Setup file # loaded.@',SUFILE)
           ELSE
c            no data
               NIVM = 0
           ENDIF
c
c    get model setup changes
20     CALL GETZZ(NIVM,SETMVV,HELPVV,ACTMVV,NOPA)
c
c    action checks
       IF (NOPA.EQ.2) THEN
c        use another setup file
           CALL GFNEZZ('ESS',SUFILE)
           INQUIRE (FILE=SUFILE,EXIST=OK)
           IF (OK)  GOTO 10
           CALL FMESVV(2,'@','#','Setup file # not found.@',SUFILE)
           NIVM = 1
           GOTO 20
           ENDIF
       IF (NOPA.EQ.3)  THEN
c        completly new setup
           NIVM = 0
           GOTO 20
           ENDIF
       IF (NOPA.EQ.4)  THEN
c        confirm quit (redundant to action)
           IF (YESZZ('Do you really want to quit ESP?')) CALL QUITVV
c        no quit
           NIVM = 1
           GOTO 20
           ENDIF
c
c *  submitting the data; make checks
c
c    check stroke-rod relationship
       IF ((IW(20).EQ.1).AND.(RW(216).LE.0.5*RW(213))) THEN
           CALL WARNZZ('@','@The connecting rod must be longer '//
     ;                 'than half the stroke.@@')
           NIVM = 1
           GOTO 20
           ENDIF
c
c    check intake close angle > open angle
       IF (IW(3).LE.IW(2)) THEN
           CALL WARNZZ('@','@The crank angle at intake valve close '//
     ;         'must be greater than the angle at intake open.@@')
           NIVM = 1
           GOTO 20
           ENDIF
c
c    check exhaust close angle > open angle
       IF (IW(5).LE.IW(4)) THEN
           CALL WARNZZ('@','@The crank angle at exhaust valve close '//
     ;         'must be greater than the angle at exhaust open.@@')
           NIVM = 1
           GOTO 20
           ENDIF
c
c    check cos-flat-cos intake angles
       IF (IW(18).EQ.2)  THEN
           IF ((IW(24)+IW(25)).GT.IW(3)-IW(2))  THEN
               CALL WARNZZ('@','@The intake valve is not open for '//
     ;             'enough angle to accomodate the rise and close.@@')
               NIVM = 1
               GOTO 20
               ENDIF
           ENDIF
c
c    check cos-flat-cos exhaust angles
       IF (IW(19).EQ.2)  THEN
           IF ((IW(26)+IW(27)).GT.(IW(5)-IW(4)))  THEN
               CALL WARNZZ('@','@The exhaust valve is not open for '//
     ;             'enough angle to accomodate the rise and close.@@')
               NIVM = 1
               GOTO 20
               ENDIF
           ENDIF
c
c    check ignition timing
       IF (IW(6).EQ.1)  THEN
           IF ((IW(11).LE.IW(3)).AND.(IW(11).GE.IW(4))) THEN
               CALL WARNZZ('@','@Improper ignition timing; '//
     ;                         'valves are open.@@')
               NIVM = 1
               GOTO 20
               ENDIF
           ENDIF
c
c    check combustion efficiency and ignition mass
       IF (RW(234).LE.RW(231)) THEN
           CALL WARNZZ('@','@The combustion efficiency must be '//
     ;         'greater than the mass fraction ignited.@@')
           NIVM = 1
           GOTO 20
           ENDIF
c
c    check for flame geometry file write
       IF(IW(36).EQ.1) THEN
c        load flame geometry table file
           CALL OPFLVV(10,FGFILE,.FALSE.,.FALSE.,OK)
           IF (.NOT.OK) GOTO 920
           CALL WFGTVV(10,IERR)
           CLOSE(10)
           IF (IERR.NE.0)  THEN
               CALL FMESVV(2,'@','#',
     ;             'Error saving flame geometry file #.@',FGFILE)
               NIVM = 1
               GOTO 20
               ENDIF
           CALL FMESVV(1,'@','#','Flame geometry file # saved.@',FGFILE)
c        set for no save next time
           IW(36) = 2
           ENDIF
c
c * file reads
c
c    check for properties file read
       IF (IW(17).EQ.1)  THEN
c        load properties file
           IERR = 1
           CALL OPFLVV(10,PMFILE,.TRUE.,.FALSE.,OK)
           IF (.NOT.OK)  GOTO 22
           CALL RTPFVV(10,IERR)
           CLOSE(10)
22         IF (IERR.NE.0)  THEN
c            try old style read
               CALL OUFOVV(10,PMFILE,IERR)
               IF (IERR.EQ.0) THEN
                       CALL RTPOVV(10,IERR)
                       CLOSE(10)
                       CALL FMESVV(2,'@','#',
     ;                   'Properties file # was ESP V1 format. '//
     ;                   'Save to convert to V2 format.@',PMFILE)
                   ELSE
c                    error
                       CALL FMESVV(2,'@','#',
     ;                   'Properties file # corrupted.@',PMFILE)
                       NIVM = 1
                       GOTO 20
                   ENDIF
               ENDIF
c        set properties loaded indicator
           IW(17) = 2
           ENDIF
c
c    check for flame geometry table file read
       IF (IW(38).EQ.1)  THEN
c        read flame geometry file
           IERR = 1
           CALL OPFLVV(10,FGFILE,.TRUE.,.FALSE.,OK)
           IF (.NOT.OK)  GOTO 24
           CALL RFGTVV(10,IERR)
           CLOSE(10)
24         IF (IERR.NE.0)  THEN
c            try V1 form
               CALL OUFOVV(10,FGFILE,IERR)
               IF (IERR.EQ.0)  CALL RFGOVV(10,IERR)
               CLOSE(10)
               IF (IERR.EQ.0)  THEN
                       CALL FMESVV(2,'@','#',
     ;                  'Flame geometry file # was ESP V1 format. '//
     ;                  'Save to convert to V2 format.@',FGFILE)
                   ELSE
c                    error
                        CALL FMESVV(2,'@','#',
     ;                  'Flame geometry file # corupted.@',FGFILE)
                        NSETM = 1
                        GOTO 10
                   ENDIF
               ENDIF
c        set flame geometry loaded indicator
           IW(38) = 2
           ENDIF
c
c    check for intake valve program read
        IF (IW(18).EQ.3)  THEN
           IERR = 1
           CALL OPFLVV(10,IVFILE,.TRUE.,.FALSE.,OK)
           IF (.NOT.OK) GOTO 26
           CALL RVPTVV(10,NIVP,NDI,FMXI,AVDI,FVDI,IERR)
           CLOSE(10)
26         IF (IERR.NE.0)  THEN
c            try old format
               CALL OUFOVV(10,IVFILE,IERR)
               IF (IERR.EQ.0) CALL RVPOVV(10,NIVP,
     ;                     NDI,FMXI,AVDI,FVDI,IERR)
               CLOSE(10)
               IF (IERR.EQ.0)  THEN
                       CALL FMESVV(2,'@','#',
     ;                     'Valve program # file was ESP V1 format.  '//
     ;                     'Save to convert to V2 format.@',IVFILE)
                   ELSE
c                    error
                       CALL FMESVV(2,'@','#',
     ;                   'Valve program file # corrupted.@',IVFILE)
                       NIVM = 1
                       GOTO 20
                   ENDIF
               ENDIF
c        set for loaded valve program
           IW(18) = 4
           ENDIF
c
c    check for exhaust valve program read
        IF (IW(19).EQ.3)  THEN
           IERR = 1
           CALL OPFLVV(10,EVFILE,.TRUE.,.FALSE.,OK)
           IF (.NOT.OK) GOTO 28
           CALL RVPTVV(10,NEVP,NDE,FMXE,AVDE,FVDE,IERR)
           CLOSE(10)
28         IF (IERR.NE.0)  THEN
c            try old format
               CALL OUFOVV(10,EVFILE,IERR)
               IF (IERR.EQ.0) CALL RVPOVV(10,NEVP,
     ;                     NDE,FMXE,AVDE,FVDE,IERR)
               CLOSE(10)
               IF (IERR.EQ.0) THEN
                       CALL FMESVV(2,'@','#',
     ;                 'Valve program # file was ESP V1 format.  '//
     ;                 'Save to convert to V2 format.@',EVFILE)
                   ELSE
c                    error
                       CALL FMESVV(2,'@','#',
     ;                   'Valve program file # corrupted.@',EVFILE)
                       NIVM = 1
                       GOTO 20
                   ENDIF
               ENDIF
c        set for loaded valve program
           IW(19) = 4
           ENDIF
c
c    check for setup file save
       IF (KSSU.EQ.1) THEN
c        load setup file
           CALL OPFLVV(10,SUFILE,.FALSE.,.FALSE.,OK)
           IF (.NOT.OK)  GOTO 920
           CALL WSETVV(10,IERR)
           CLOSE (10)
           IF (IERR.NE.0)  THEN
               CALL FMESVV(2,'@','#',
     ;                   'Error writing setup file #.@',SUFILE)
               NIVM = 1
               GOTO 20
               ENDIF
           CALL FMESVV(1,'@','#','Setup file # saved.@',SUFILE)
           ENDIF
c
c    setup for run
30     NIVR = 0
       CALL SET0VV
       NCYCLE = 0
c
c    task selection - set option unset
       NOPR = ID(1)
c    assume no 5-line plots
40     KOP5C = 0
c    get option
       CALL GETZZ(NIVR,SETRVV,HELPVV,ACTRVV,NOPAR)
c    check actions
       IF (NOPAR.EQ.2)  THEN
c        return to engine setup
           NIVM = 1
           GOTO 20
           ENDIF
c    execute tasks
       IF (NCYCLE.EQ.0) THEN
               NOPRS = 3
               NOPRQ = 4
           ELSE
               NOPRS = 6
               NOPRQ = 7
           ENDIF
c
c    check for write model to file
       IF (NOPR.EQ.1)  THEN
           CALL OPFLVV(15,OFILE,.FALSE.,.TRUE.,OK)
           IF (.NOT.OK)  GOTO 940
           CALL DMODVV(15)
           CALL DGPDVV(15)
           CLOSE(15)
           CALL FMESVV(1,'@','#','Model file # saved.@',OFILE)
           NIVR = 1
           GOTO 40
           ENDIF
c
c    check for run to check convergence
       IF (NOPR.EQ.2)  THEN
           IF ((KIMC.EQ.2).OR.(KEMC.EQ.2)) THEN
               CALL MESSZZ('@','@Working; read this while you wait!@@')
               CALL HCONVV
               ENDIF
c        make the run
           DO N=1,NRUN
               NCYCLE = NCYCLE + 1
               CALL CYCLVV(IERR)
               IF (IERR.NE.0)  THEN
                   NIVM = 1
                   GOTO 20
                   ENDIF
c            write convergence data
               CALL DCCLVV(0)
               CALL DMCSVV(0)
               ENDDO
           CALL PAUSZZ(0)
           NIVR = 1
           GOTO 40
           ENDIF
c
       IF (NOPR.EQ.3)  THEN
c        check plot file names for duplicates
           DO I=1,6
               IF (KOP5(I).NE.0) THEN
                   DO J=I+1,6
                       IF (KOP5(J).NE.0) THEN
                           IF (MPFL(I).EQ.MPFL(J)) THEN
                               CALL FMESVV(2,'@','#',
     ;                            'Duplicate plot file #; '//
     ;                            'task not executed.@',MPFL(I))
                                   NIVR = 1
                                   GOTO 40
                                   ENDIF
                               ENDIF
                       ENDDO
                   ENDIF
                ENDDO
c        open the scratch files
           DO I=1,6
               IF (KOP5(I).NE.0) THEN
                   CALL OPFLVV(20+I,MX5T(I),.FALSE.,.FALSE.,OK)
                   IF (.NOT.OK)  GOTO 940
                   KOP5C = KOP5C + 1
                   ENDIF
               ENDDO
c        run the cycle and write the scratch files
           NCYCLE = NCYCLE + 1
           CALL CYCLVV(IERR)
           IF (IERR.NE.0)  THEN
               NIVR = 1
               GOTO 40
               ENDIF
c        write the plotfiles
           DO I=1,6
               IF (KOP5(I).NE.0)  THEN
                   CALL OPFLVV(30+I,MPFL(I),.FALSE.,.TRUE.,OK)
                   IF (.NOT.OK)  GOTO 940
                   CALL MPL5VV(20+I,30+I,IDPV(1,I))
                   CALL FMESVV(1,'@','#',
     ;                'Plot file # written.@',MPFL(I))
                   ENDIF
               ENDDO
           NIVR = 1
           GOTO 40
           ENDIF
c
       IF (NCYCLE.GT.0) THEN
c        check for output
           IF (NOPR.EQ.4)  THEN
c            check for indicator plot
               IF (KPLOT(1).EQ.1)  THEN
c                open Matlab file
                   IUML = 14
                   CALL OPFLVV(IUML,MPFIND,.FALSE.,.TRUE.,OK)
                   IF (.NOT.OK)  GOTO 940
                   CALL PINDVV(IUML)
                   CLOSE(IUML)
                   CALL FMESVV(1,'@','#',
     ;                'Indicator plot file # written.@',MPFIND)
                   ENDIF
c
c            intake manifold plots
               IF (KIMC.EQ.2)  THEN
                   IF (KPLOT(2).EQ.1) THEN
c                    open Matlab file
                       IUML = 14
                       CALL OPFLVV(IUML,MPFIM,.FALSE.,.TRUE.,OK)
                       IF (.NOT.OK)  GOTO 940
                       CALL MANPXX(RWI,IWI,IUML)
                       CLOSE (IUML)
                       CALL FMESVV(1,'@','#',
     ;                   'Intake manifold plot file # written.@',MPFIM)
                       ENDIF
                   ENDIF
c
c            exhaust manifold plots
               IF (KEMC.EQ.2) THEN
                   IF ( ((KIMC.EQ.1).AND.(KPLOT(2).EQ.1)).OR.
     ;                  ((KIMC.EQ.2).AND.(KPLOT(3).EQ.1)) ) THEN
c                    open the file
                       CALL OPFLVV(IUML,MPFEM,.FALSE.,.TRUE.,OK)
                       IF (.NOT.OK)  GOTO 940
                       CALL MANPXX(RWE,IWE,IUML)
                       CLOSE (IUML)
                       CALL FMESVV(1,'@','#',
     ;                   'Exhaust manifold plot file # written.@',MPFEM)
                       ENDIF
                   ENDIF
c            end of plot output
               NIVR = 1
               GOTO 40
               ENDIF
c
c        check for summary data display
           IF (NOPR.EQ.5)  THEN
               CALL CSUMVV
               CALL DSUMVV(0)
               CALL PAUSZZ(0)
               NIVR = 1
               GOTO 40
               ENDIF
c
c        check for write output to file
           IF (NOPR.EQ.6)  THEN
               CALL CSUMVV
               CALL OPFLVV(15,OFILE,.FALSE.,.TRUE.,OK)
               IF (.NOT.OK)  GOTO 940
c            run header
               CALL DHDRVV(15)
c            operating conditions
               CALL DOPCVV(15)
c            convergence
               CALL DCCSVV(15)
               CALL DMCSVV(15)
c            performance summary
               CALL DSUMVV(15)
c            model designator
               CALL DMOHVV(15)
               CLOSE(15)
               CALL FMESVV(1,'@','#','Output file # written.@',OFILE)
               NIVR = 1
               GOTO 40
               ENDIF
c
           ENDIF
c
c    check for return to engine setup
       IF (NOPR.EQ.NOPRS)  THEN
           NIVM = 1
           GOTO 20
           ENDIF
c
c    check for quit
       IF (NOPR.EQ.NOPRQ)  CALL QUITVV
c
c    error
       CALL ERRFZZ('@','@Option error@@')
       STOP
c
c    file open errors
920    NIVM = 1
       GOTO 20
940    NIVR = 1
       GOTO 40
c    ubable to open monitor files
960    CALL WARNZZ('@','@Terminating execution; shut down other '//
     ;     'ESP running and try again.@@')
       STOP
c
       END
c******************************************************************************
c
       SUBROUTINE OPFLVV(IU,NAME,KE,KF,OK)
c
c      Opens file NAME on unit IU and returns OK=.TRUE. or writes problem
c      message.
c
c       KE  .TRUE. if file must exist
c       KF  .TRUE. if file is formatted
c-----------------------------------------------------------------------------
       LOGICAL         KE,KF,OK
       CHARACTER*32    NAME
c-----------------------------------------------------------------------------
c    check for existence
       IF (KE)  THEN
           INQUIRE (FILE=NAME,EXIST=OK)
           IF (.NOT.OK)  THEN
               CALL FMESVV(2,'@','#','File # not found@',NAME)
               RETURN
               ENDIF
           ENDIF
c    open the file
       IF (KF) THEN
               OPEN(IU,FILE=NAME,ERR=90)
           ELSE
               OPEN(IU,FILE=NAME,FORM='UNFORMATTED',ERR=90)
           ENDIF
       OK = .TRUE.
       RETURN
c    file open error
90     CALL FMESVV(2,'@','#','File # not opened. Probable '//
     ;     'cause; in use by other application or by ESP '//
     ;     'elsewhere.@',NAME)
       OK = .FALSE.
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE FMESVV(K,ESC,INS,STRING,NAME)
c
c      Issues message (K=1) or warning (K=2) ESC-terminated STRING with
c      file name inserted where INS is found.
c-----------------------------------------------------------------------------
       PARAMETER   (LNAME=32)
       PARAMETER   (LMAX=256)
       CHARACTER*1 ESC,INS,STRING(*),NAME(32),TEXT(LMAX)
c-----------------------------------------------------------------------------
c    find end of name
       DO I=1,LNAME
           IF (NAME(I).EQ.' ') THEN
               N = I - 1
               GOTO 8
               ENDIF
           ENDDO
c    no blanks
       N = LNAME
c    set to scan the string
8      LS = 0
       LT = 1
       TEXT(LT) = ESC
c    continue the scan
10     LS = LS + 1
       IF (STRING(LS).EQ.INS)  THEN
c        insert the name
           DO I=1,N
               IF (LT.EQ.LMAX-2)  GOTO 40
               LT = LT + 1
               TEXT(LT) = NAME(I)
               ENDDO
           GOTO 10
           ENDIF
       IF (STRING(LS).EQ.ESC)  GOTO 40
       IF (LT.EQ.LMAX-2)  GOTO 40
       LT = LT + 1
       TEXT(LT) = STRING(LS)
       GOTO 10
c    end the string
40     TEXT(LT+1) = ESC
       TEXT(LT+2) = ESC
c    send the message
       IF (K.EQ.1)  THEN
               CALL MESSZZ(ESC,TEXT)
               CALL PAUSZZ(0)
           ELSE
               CALL WARNZZ(ESC,TEXT)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE QUITVV
c
c      ESP close routine
c-----------------------------------------------------------------------------
       LOGICAL OUT,YESZZ
       COMMON /MONMXX/IUMONM,MONM,OUT
c------------------------------------------------------------------------------
       COMMON /MONCVV/IUMONC,MONC
c------------------------------------------------------------------------------
c    confirm quit
       IF (YESZZ('Do you really want to quit ESP?')) THEN
           CLOSE(IUMONM)
           CLOSE(IUMONC)
           STOP
           ENDIF
c    no quit
       RETURN
       END
c******************************************************************************
