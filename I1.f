c*****************************************************************************
c
c      This file contains the primary routines for the non-graphcal interface.
c      Supporting routines are in file I2.F.
c
c      Routines common to all interfaces are in file IB.F.
c
c******************************************************************************
c
       SUBROUTINE MESSZZ(ESC,STRING)
c
c      Writes STRING to the standard output, breaking lines at blanks or ESC.
c      Two ESCs mark the end of STRING.
c      A blank line is written first.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(*)
c------------------------------------------------------------------------------
c    line space
       WRITE (*,8)
8      FORMAT (1X)
c    output
       CALL DOUTZZ(ESC,STRING)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DOUTZZ(ESC,STRING)
c
c      Writes STRING to the output window, breaking lines at ESC.
c      Two ESCs mark the end of STRING.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(*),A(80),BLANK
       LOGICAL         LESC,LOADB
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
c------------------------------------------------------------------------------
c    initialize
       IS = 0
       IL = 0
       ISB = 0
       ILB = 0
       LESC = .FALSE.
c    set to load blanks
       LOADB = .TRUE.
c
c    loop point for parsing string
10     IS = IS + 1
c    check for ESC
       IF (STRING(IS).EQ.ESC)  THEN
c        set to load blanks
           LOADB = .TRUE.
c        check for last also ESC
           IF (LESC)  THEN
c            end of STRING
               RETURN
               ENDIF
c        end of line encountered
           IF (IL.GT.0) THEN
c                write the current line
                   WRITE (*,12) (A(I),I=1,IL)
12                 FORMAT (1X,79A1)
c                reset to line start
                   IL = 0
               ELSEIF (IS.EQ.1)  THEN
c                first line blank
                   WRITE (*,*) ' '
               ENDIF
c        record last ESC
           LESC = .TRUE.
           GOTO 10
           ENDIF
c    not ESC
       LESC = .FALSE.
c    check for blank
       IF (STRING(IS).EQ.BLANK) THEN
c        record location of last blank
           ISB = IS
           ILB = IL + 1
           ENDIF
       IF (IL.EQ.LMAX)  THEN
c        the line is full; check for a blank
           IF (ILB.NE.0)  THEN
c            retreat to last blank
               IL = ILB - 1
               IS = ISB
c            set to ignore blanks in next line
               LOADB = .FALSE.
               ENDIF
c        write the line
           WRITE (*,12) (A(I),I=1,IL)
           IL = 0
           GOTO 10
           ENDIF
c    check character
       IF (STRING(IS).NE.BLANK)  THEN
c        set to load blanks
           LOADB = .TRUE.
           ENDIF
       IF (LOADB)  THEN
c        load the character
           IL = IL + 1
           A(IL) = STRING(IS)
           ENDIF
       GOTO 10
       END
c******************************************************************************
c
       SUBROUTINE PAUSZZ(K)
c
c      Waits for user confirmation of a message
c      If K=1, calls CLOWZZ after confirmation to clear output window
c         K=2  is a special GUI signal not used in this interface.
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*32    LINE
c------------------------------------------------------------------------------
c    blank line
       WRITE (*,*) ' '
       ML = 0
       CALL LOADZZ(' Press enter to continue',24,LINE,ML)
       CALL POSTZZ(LINE,ML,.TRUE.)
       READ (*,*)
       IF (K.GE.1) CALL CLOWZZ
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE WARNZZ(ESC,STRING)
c
c      Writes STRING to the warning box, breaking lines at blanks or ESC.


c      Two ESCs mark the end of STRING. Then pauses for confirmation
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,STRING(*)
c------------------------------------------------------------------------------
       CALL MESSZZ(ESC,STRING)
       CALL PAUSZZ(0)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  GETZZ(NIV,SETUP,HELP,ACTION,NOPA)
c
c      Creates setup, solicits changes, provides help as requested, and
c      loads the application with the values to be used on the next run.
c      Loads a new setup in a file or saves the current setup on request.
c      Executes soft application shutdown on request.
c
c      If NIV=0   no values loaded at call
c             1   application's values loaded at call
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE  'INTWRKZZ.H'
       EQUIVALENCE  (ID(9),NACT)
       EQUIVALENCE  (ID(10),JKTGOD)
c------------------------------------------------------------------------------
c    application-specific setup routine
       EXTERNAL    SETUP
c    application-specific help routine
       EXTERNAL    HELP
c    application-specific action rorutine
       EXTERNAL    ACTION
c------------------------------------------------------------------------------
       LOGICAL OK
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*32    LINE
c------------------------------------------------------------------------------
       NIVX = NIV
c    build the setup work arrays
1      IF (NIVX.EQ.0)  THEN
               CALL SETZZ(0)
           ELSEIF (NIVX.EQ.1)  THEN
               CALL SETZZ(1)
           ELSE
               CALL ERRFZZ('@','GETZZ error; bad NOP.@@')
           ENDIF
       CALL SETUP
c
c    check the setup construction
       CALL SETZZ(2)
       CALL SETUP
c
c    get changes, providing help when requested
10     CALL GCZZ(HELP)
c    check actions
       IF (NACT.EQ.0) THEN
c       no actions; submit
           NOPA = 1
           GOTO 20
           ENDIF
c    clear prompt window
       CALL CLPWZZ
c    display actions
       CALL DGHZZ(0,JKTGOD)
       WRITE (*,12)
12     FORMAT('   0 Resume editing data/choices')
       DO I=1,NACT
           CALL DACTZZ(I)
           ENDDO
c    build and issue prompt
       ML = 0
       CALL LOADZZ(' Enter desired action > ',24,LINE,ML)
       CALL POSTZZ(LINE,ML,.TRUE.)
c    get choice
       I = NOPZZ(NACT)
c    branch on action
       IF (I.EQ.0) THEN
c        continue editing
           GOTO 10
           ENDIF
c    process action selction
       CALL ACTION(I,NOPA)
c    continue editing
       IF (NOPA.EQ.0) THEN
c            continue editing
               GOTO 10
           ELSEIF (NOPA.EQ.1)  THEN
c            go check data and submit
               GOTO 20
           ELSEIF (NOPA.GT.1)  THEN
c            abort data editing and return
               RETURN
           ELSE
c            interfaced error stop
               CALL STOPZZ
           ENDIF
c
c    check data and submit
20     CALL CHKSZZ(OK)
       IF (.NOT.OK)  THEN
           NOPA = 1
           GOTO 10
           ENDIF
c    recover new data
       CALL SETZZ(3)
       CALL SETUP
       RETURN
       END
c******************************************************************************
c
       LOGICAL FUNCTION YESZZ(QUERY)
c
c      Queries and returns .TRUE. if response is YES, .FALSE. if no.
c      QUERY must end with a ?
c------------------------------------------------------------------------------
       CHARACTER*1    QUERY(*),A
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96 LINE
c------------------------------------------------------------------------------
c    find the length
       DO I=1,78
           IF (QUERY(I).EQ.'?')  GOTO 10
           ENDDO
       CALL ERRFZZ('@','YESZZ error; QUERY too long or ? missing.@@')
c
c    blank line
10     WRITE (*,12)
12     FORMAT(/' ')
c    load the question
       ML = 0
       CALL LOADZZ(' ',1,LINE,ML)
       CALL LOADZZ(QUERY,I,LINE,ML)
       CALL LOADZZ(' ',1,LINE,ML)
14     FORMAT (A)
c    write the question
       CALL POSTZZ(LINE,ML,.TRUE.)
c    get the response
       READ (*,21) A
21     FORMAT (A)
       IF ((A.EQ.'Y').OR.(A.EQ.'y'))  THEN
           YESZZ = .TRUE.
           RETURN
           ENDIF
       IF ((A.EQ.'N').OR.(A.EQ.'n'))  THEN
           YESZZ = .FALSE.
           RETURN
           ENDIF
c    improper answer
       WRITE (*,22)
22     FORMAT (' Answer y or n!')
       GOTO 10
       END
c******************************************************************************
c
       SUBROUTINE GFNEZZ(EXT,NAME)
c
c      Gets file name with a given extent
c------------------------------------------------------------------------------
       PARAMETER       (NDNAME=32)
       CHARACTER*1     NAME(NDNAME),EXT(3)
       LOGICAL         ENTRY,EXTENT
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*48    LINE
c------------------------------------------------------------------------------
c    assemble prompt
10     ML = 0
       CALL LOADZZ(' Enter file name with extent .',30,LINE,ML)
       CALL LOADZZ(EXT,3,LINE,ML)
       CALL LOADZZ(' > ',3,LINE,ML)
c    issue prompt after blank lime
       WRITE (*,*) ' '
       CALL POSTZZ(LINE,ML,.TRUE.)
c   get file name
       READ (*,13,ERR=10) NAME
13     FORMAT (80A1)
c    scan entry and convert to upper case
       ENTRY = .FALSE.
       EXTENT = .FALSE.
       LEND = NDNAME
       LDOT = 0
       DO L=1,NDNAME
c        look for extent
           IF (NAME(L).EQ.'.') THEN
c            check for previous extent
               IF (EXTENT)  THEN
                   WRITE (*,14)
14                 FORMAT(/' Only one dot allowed')
                   GOTO 10
                   ENDIF
               LDOT = L
               EXTENT =.TRUE.
               ENTRY = .TRUE.
               ENDIF
c        look for blank
           IF (NAME(L).EQ.' ') THEN
c            blank
               IF (LEND.EQ.NDNAME) LEND = L - 1
               ENDIF
           IF ((L.GT.LEND).AND.(NAME(L).NE.' ')) THEN
               WRITE (*,16)
16             FORMAT (/' Blanks not permitted infile names')
               GOTO 10
               ENDIF
c        nonblank
           IF (NAME(L).NE.' ')  ENTRY = .TRUE.
c        capitalize
           IC = ICHAR(NAME(L))
           IF ((IC.GT.96).AND.(IC.LT.123)) IC = IC - 32
           NAME(L) = CHAR(IC)
           ENDDO
c    check for entry
       IF (.NOT.ENTRY)  THEN
c        abort
           DO I=1,NDNAME
c            set blank
               NAME(I) = ' '
               ENDDO
           RETURN
           ENDIF
c    check name length
       IF (LEND.GT.NDNAME-4)  THEN
           WRITE (*,22) EXT
22         FORMAT (/' Name too long.',3A1)
           GOTO 10
           ENDIF
       IF (EXTENT.AND.(LEND-LDOT.GT.3))  THEN
           WRITE (*,24) EXT
24         FORMAT (/' Extent too long.',3A1)
           GOTO 10
           ENDIF
c    check extent
       DO L=1,3
           IF ((.NOT.EXTENT).OR.(NAME(LDOT+L).NE.EXT(L)))  THEN
               WRITE (*,26) EXT
26             FORMAT (/' The extent must be .',3A1)
               GOTO 10
               ENDIF
           ENDDO
c     ok
       RETURN
       END
c******************************************************************************

