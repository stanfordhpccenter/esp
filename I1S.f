c**********************************************************************
c
       SUBROUTINE MESSZZ(ESC,STRING)
c
c      Displays <STRING> in the "Messages" area of the GUI, breaking
c      lines at blanks or ESC. Two ESCs mark the end of <STRING>.
c------------------------------------------------------------------------------
c SCK: 07/05/00
       CHARACTER*1     ESC,STRING(2048),A(80),BLANK
       CHARACTER*9     LEADER
       CHARACTER*3     ENDER
       LOGICAL         LESC
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' messA { ','} '/
c------------------------------------------------------------------------------
c    initialize
       IS = 0
       IL = 0
       ISB = 0
       ILB = 0
       LESC = .FALSE.
c
c    call TCL/TK routine
       WRITE(KUOUT,13)LEADER
c    line space
       WRITE (KUOUT,8)
8      FORMAT (1X)
c
c    loop point for parsing string
10     IS = IS + 1
c    check for ESC
       IF (STRING(IS).EQ.ESC)  THEN
c        check for last also ESC
           IF (LESC)  THEN
              WRITE(KUOUT,13)ENDER
              CALL FLUSH(KUOUT)
c            end of STRING
               RETURN
           ENDIF
c        end of line encountered
           IF (IL.GT.0) THEN
c            write the current line
               WRITE (KUOUT,12) (A(I),I=1,IL)
12             FORMAT (1X,80A1)
13             FORMAT (A)
c            reset to line start
               IL = 0
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
           ENDIF
c        write the line
           WRITE (KUOUT,12) (A(I),I=1,IL)
           IL = 0
           GOTO 10
       ENDIF
c    load the character
       IL = IL + 1
       A(IL) = STRING(IS)
       GOTO 10
       END
c**********************************************************************

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
       CHARACTER*9     LEADER
       CHARACTER*3     ENDER
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' messO {','} '/
c------------------------------------------------------------------------------
c    initialize

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
c    call TCL/TK routine
       WRITE(KUOUT,14)LEADER
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
              WRITE(KUOUT,13)ENDER
              CALL FLUSH(KUOUT)
               RETURN
               ENDIF
c        end of line encountered
           IF (IL.GT.0) THEN
c                write the current line
                   WRITE (KUOUT,12) (A(I),I=1,IL)
12                 FORMAT (1X,79A1)
13                 FORMAT (A)
14                 FORMAT (A,$)
c                reset to line start
                   IL = 0
               ELSEIF (IS.EQ.1)  THEN
c                first line blank
                   WRITE (KUOUT,*) ' '
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
           WRITE (KUOUT,12) (A(I),I=1,IL)
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


c**********************************************************************
c
       SUBROUTINE DOUTZZ_OLD(ESC,STRING)
c
c      Displays <STRING> in the "Messages" area of the GUI, breaking
c      lines at blanks or ESC. Two ESCs mark the end of <STRING>.
c------------------------------------------------------------------------------
c SCK: 07/05/00
       CHARACTER*1     ESC,STRING(2048),A(80),BLANK
       CHARACTER*9     LEADER
       CHARACTER*3     ENDER
       LOGICAL         LESC
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' messO { ','} '/
c------------------------------------------------------------------------------
c    initialize
       IS = 0
       IL = 0
       ISB = 0
       ILB = 0
       LESC = .FALSE.
c
c    call TCL/TK routine
       WRITE(KUOUT,13)LEADER
c    line space
c       WRITE (KUOUT,8)
8      FORMAT (1X)
c
c    loop point for parsing string
10     IS = IS + 1
c    check for ESC
       IF (STRING(IS).EQ.ESC)  THEN
c        check for last also ESC
           IF (LESC)  THEN
              WRITE(KUOUT,13)ENDER
              CALL FLUSH(KUOUT)
c            end of STRING
               RETURN
           ENDIF
c        end of line encountered
           IF (IL.GT.0) THEN
c            write the current line
               WRITE (KUOUT,12) (A(I),I=1,IL)
12             FORMAT (1X,80A1)
13             FORMAT (A)
c            reset to line start
               IL = 0
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
           ENDIF
c        write the line
           WRITE (KUOUT,12) (A(I),I=1,IL)
           IL = 0
           GOTO 10
       ENDIF
c    load the character
       IL = IL + 1
       A(IL) = STRING(IS)
       GOTO 10
       END
c**********************************************************************


c**********************************************************************
c
       SUBROUTINE MESBKK
c
c      Displays <STRING> in the "Messages" area of the GUI, breaking
c      lines at blanks or ESC. Two ESCs mark the end of <STRING>.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(2048),A(80),BLANK
       CHARACTER*8     LEADER
       CHARACTER*2     ENDER
       LOGICAL         LESC
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' messF {','} '/
c------------------------------------------------------------------------------
c    initialize
       WRITE(KUOUT,9)LEADER
c    line space
       WRITE (KUOUT,8)
8      FORMAT (1X)
9      FORMAT (A)
       CALL FLUSH(KUOUT)
       RETURN
       END
c**********************************************************************

c**********************************************************************
c
       SUBROUTINE MESEKK
c
c      Displays <STRING> in the "Messages" area of the GUI, breaking
c      lines at blanks or ESC. Two ESCs mark the end of <STRING>.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(2048),A(80),BLANK
       CHARACTER*8     LEADER
       CHARACTER*2     ENDER
       LOGICAL         LESC
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' messF {','} '/
c------------------------------------------------------------------------------
c    initialize
       WRITE(KUOUT,9)ENDER
c    line space
       WRITE (KUOUT,8)
8      FORMAT (1X)
9      FORMAT (A)
       CALL FLUSH(KUOUT)
       RETURN
       END
c**********************************************************************


c**********************************************************************
c
       SUBROUTINE PAUSZZ(K)
c
c      Waits for user confirmation of a message
c----------------------------------------------------------------------
       CHARACTER*9  LEADER
       CHARACTER*3  ENDER
       INTEGER K
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' Pauszz {',' } '/
c----------------------------------------------------------------------
       WRITE (KUOUT,2)LEADER
           IF (K.EQ.2) WRITE(KUOUT,*) "clear"
       WRITE (KUOUT,2) ENDER
       CALL FLUSH(KUOUT)
2      FORMAT (A)
       READ (KUIN,*)
       CALL POPSKK
         WRITE (KUOUT,2)'OK'
       CALL POPFKK
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
c      If NIV=0   no values loaded
c             1   application's values loaded
c-----------------------------------------------------------------------
c    setup data storage
c       COMMON /SETIZZ/ ID(128)
c       EQUIVALENCE (ID(9),NACT)
       INCLUDE  'INTWRKZZ.H'
       EQUIVALENCE  (ID(9),NACT)
       EQUIVALENCE  (ID(10),JKTGOD)
c    application-specific setup routine
       EXTERNAL    SETUP
c    application-specific help routine
       EXTERNAL    HELP
c    application-specific setup file load rorutine
c       EXTERNAL    LOAD
c    application-specific setup file save routine
c       EXTERNAL    SAVE
c    application-specific quit routine
c       EXTERNAL    QUIT
c   application-specific action routine
       EXTERNAL    ACTION
c-----------------------------------------------------------------------
       LOGICAL OK
c-----------------------------------------------------------------------
c    IGI to JKT translation
       DIMENSION I2JKT(2048)
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c-----------------------------------------------------------------------
       NIVX = NIV
c    build the setup work arrays

1          IF (NIVX.EQ.0)  THEN
               CALL SETZZ(0)
           ELSEIF (NIVX.EQ.1)  THEN
               CALL SETZZ(1)
           ELSE
               CALL ERRFZZ('@','GETZZ error; bad NOP.@@')
           ENDIF
2      CALL SETUP
c
c    check the setup
       CALL SETZZ(2)
       CALL SETUP
c       IF(NOP.EQ.0)THEN
c         create cross-reference table
           CALL STBLKK
c     pass unset default values to GUI
           CALL SUNKK
c    display all objects in GUI
10          CALL DABKK
c       ENDIF
c
c    listen to GUI for incoming changes
15       CALL GCKK(HELP)
         IF (NACT.EQ.0) GOTO 20
c    get changes, providing help when requested
           I = NOPZZ(NACT)
c    branch on command
      IF (I.EQ.0)  THEN
c            get more changes
               GOTO 15
      ENDIF
c    process action
       CALL ACTION(I,NOPA)
c       CALL MESBKK
c         WRITE(*,*)'in getzz nopa=', NOPA
c       CALL MESEKK
       IF (NOPA.EQ.0) THEN
c         call to action manager
         CALL CHTABKK
         GOTO 10
       ELSEIF (NOPA.EQ.1) THEN
c          CALL CHTABKK
         GOTO 20
       ELSEIF (NOPA.GT.1) THEN
c         call to action manager
          CALL CHTABKK
         RETURN
       ELSE
         CALL STOPZZ
       ENDIF

c       IF (NOPA.LT.2) THEN
c         CALL CHTABKK
c         GOTO 1
c       ENDIF
c       IF (NOPA.EQ.2) GOTO 10
c                 CALL MESBKK
c                          WRITE(*,*)'In GCKK NOPA= ',NOPA
c                 CALL MESEKK

c    check data and submit
20     OK = .FALSE.
       CALL CHKSZZ(OK)
c                 CALL MESBKK
c                          WRITE(*,*)'In GCKK line 20, OK= ',OK
c                 CALL MESEKK

       IF (.NOT.OK)  THEN
c SCK 07/03/00: commented out to prevent loading of unsets
           NOPA = 1
           GOTO 15
       ENDIF
c    recover new data
c         call to action manager
       CALL CHTABKK
c       CALL MESBKK
c       WRITE(*,*)'back from CHRABKK'
c       CALL MESEKK
       CALL SETZZ(3)
c       CALL MESBKK
c       WRITE(*,*)'back from SETZZ(3)'
c       CALL MESEKK
       CALL SETUP
c       CALL MESBKK
c       WRITE(*,*)'back from SETUP'
c       CALL MESEKK
       RETURN
       END

c***********************************************************************


c**********************************************************************
c
       SUBROUTINE POPSKK
c
c      Writes STRING to the standard output, breaking lines at blanks or ESC.
c      Two ESCs mark the end of STRING.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(512),A(512),BLANK
       CHARACTER*11     LEADER
       CHARACTER*2     ENDER
       LOGICAL         LESC
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' set_warn "','" '/
c------------------------------------------------------------------------------
c    initialize
       IS = 0
       IL = 0
       ISB = 0
       ILB = 0
       LESC = .FALSE.
c
c    call TCL/TK routine
       WRITE(KUOUT,13)LEADER
13     FORMAT (A)
       CALL FLUSH(KUOUT)
       RETURN
       END
c**********************************************************************



c**********************************************************************
c
       SUBROUTINE POPFKK
c
c      Writes STRING to the standard output, breaking lines at blanks or ESC.
c      Two ESCs mark the end of STRING.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(512),A(512),BLANK
       CHARACTER*11     LEADER
       CHARACTER*2     ENDER
       LOGICAL         LESC
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' set_warn "','" '/
c------------------------------------------------------------------------------
c    initialize
       IS = 0
       IL = 0
       ISB = 0
       ILB = 0
       LESC = .FALSE.
c
c    call TCL/TK routine
       WRITE(KUOUT,13)ENDER
13     FORMAT (A)
       CALL FLUSH(KUOUT)
       RETURN
       END
c**********************************************************************


c**********************************************************************
c
       SUBROUTINE ENACTS
c
c      Writes STRING to the standard output, breaking lines at blanks or ESC.
c      Two ESCs mark the end of STRING.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(512),A(512),BLANK
       CHARACTER*17     LEADER
       CHARACTER*2     ENDER
       LOGICAL         LESC
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' enable_actions  ','  '/
c------------------------------------------------------------------------------
c    initialize
       IS = 0
       IL = 0
       ISB = 0
       ILB = 0
       LESC = .FALSE.
c
c    call TCL/TK routine
       WRITE(KUOUT,13)LEADER
13     FORMAT (A)
       CALL FLUSH(KUOUT)
       RETURN
       END
c**********************************************************************

c**********************************************************************
c
       SUBROUTINE DISACTS
c
c      Writes STRING to the standard output, breaking lines at blanks or ESC.
c      Two ESCs mark the end of STRING.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(512),A(512),BLANK
       CHARACTER*18    LEADER,LEADED
       CHARACTER*2     ENDER
       LOGICAL         LESC,OK
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' disable_actions  ','  '/
       DATA  LEADED /' disable_daction  '/
c------------------------------------------------------------------------------
c    initialize
       IS = 0
       IL = 0
       ISB = 0
       ILB = 0
       LESC = .FALSE.
c
c    call TCL/TK routine
       OK = .FALSE.
       CALL CHKLKK(OK)
       IF (OK) THEN
         WRITE(KUOUT,13)LEADER
       ELSE
         WRITE(KUOUT,13)LEADED
       ENDIF
13     FORMAT (A)
       CALL FLUSH(KUOUT)
       RETURN
       END
c************

c******************************************************************************
c
       SUBROUTINE WARNZZ(ESC,STRING)
c
c      Displays <STRING> in the "Messages" area of the GUI, breaking
c      lines at blanks or ESC. Two ESCs mark the end of <STRING>.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(*),A(512),BLANK
       CHARACTER*9     LEADER
       CHARACTER*2     ENDER
       LOGICAL         LESC
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' ErrorA {','} '/
c------------------------------------------------------------------------------
c    initialize
       IS = 0
       IL = 0
       ISB = 0
       ILB = 0
       LESC = .FALSE.
c
c    call TCL/TK routine
       WRITE(KUOUT,13)LEADER
c    line space
       WRITE (KUOUT,8)
8      FORMAT (1X)
c
c    loop point for parsing string
10     IS = IS + 1
c    check for ESC
       IF (STRING(IS).EQ.ESC)  THEN
c        check for last also ESC
           IF (LESC)  THEN
              WRITE(KUOUT,13)ENDER
              CALL FLUSH(KUOUT)
              CALL PAUSZZ(0)
c            end of STRING
               RETURN
           ENDIF
c        end of line encountered
           IF (IL.GT.0) THEN
c            write the current line
               WRITE (KUOUT,12) (A(I),I=1,IL)
12             FORMAT (1X,80A)
13             FORMAT (A)
c            reset to line start
               IL = 0
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
           ENDIF
c        write the line
           WRITE (KUOUT,12) (A(I),I=1,IL)
           IL = 0
           GOTO 10
       ENDIF
c    load the character
       IL = IL + 1
       A(IL) = STRING(IS)
       GOTO 10
c
       END
c***********************************************************************


c******************************************************************************
c
       SUBROUTINE TELLZZ(ESC,STRING)
c
c      Displays <STRING> in popup notice area in the GUI, breaking
c      lines at blanks or ESC. Two ESCs mark the end of <STRING>.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(*),A(512),BLANK
       CHARACTER*9     LEADER
       CHARACTER*2     ENDER
       LOGICAL         LESC
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' TellIt {','} '/
c------------------------------------------------------------------------------
c    initialize
       IS = 0
       IL = 0
       ISB = 0
       ILB = 0
       LESC = .FALSE.
c
c    call TCL/TK routine
       WRITE(KUOUT,13)LEADER
c    line space
       WRITE (KUOUT,8)
8      FORMAT (1X)
c
c    loop point for parsing string
10     IS = IS + 1
c    check for ESC
       IF (STRING(IS).EQ.ESC)  THEN
c        check for last also ESC
           IF (LESC)  THEN
              WRITE(KUOUT,13)ENDER
              CALL FLUSH(KUOUT)
c            end of STRING
               RETURN
           ENDIF
c        end of line encountered
           IF (IL.GT.0) THEN
c            write the current line
               WRITE (KUOUT,12) (A(I),I=1,IL)
12             FORMAT (1X,80A)
13             FORMAT (A)
c            reset to line start
               IL = 0
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
           ENDIF
c        write the line
           WRITE (KUOUT,12) (A(I),I=1,IL)
           IL = 0
           GOTO 10
       ENDIF
c    load the character
       IL = IL + 1
       A(IL) = STRING(IS)
       GOTO 10
c
       END
c***********************************************************************

c************************************************************************
c
       LOGICAL FUNCTION YESZZ(QUERY)
c
c      Queries and returns .TRUE. if response is YES, .FALSE. if no.
c      QUERY must end with a ?
c------------------------------------------------------------------------------
       CHARACTER*1    QUERY(*),A
       CHARACTER*7     LEADER
       CHARACTER*2     ENDER
       CHARACTER*1  FORM(6)
       CHARACTER*3  IBEG,IEND
c------------------------------------------------------------------------------
       INCLUDE 'INTWRKZZ.H'

       DATA  LEADER,ENDER/' AskQ {','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/

c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
        INCLUDE 'SCKIOZZ.H'
c       COMMON /IOKK/   KUOUT, KUIN
c       KUOUT = 6
c       KUIN  = 5
c------------------------------------------------------------------------------
c    find the length
       DO 9 I=1,78
           IF (QUERY(I).EQ.'?')  GOTO 10
9          CONTINUE
       CALL ERRFZZ('@','YESZZ error; QUERY too long or ? missing.@@')
c
c    display the question
10     WRITE (KUOUT,14)LEADER
         CALL CFCZZ(I+2,FORM(2))
13       FORMAT(A2)
14       FORMAT (A15)
c         WRITE (KUOUT,13)IBEG
            WRITE (KUOUT,FORM) (QUERY(J),J=1,I)
c         WRITE (KUOUT,13)IEND
       WRITE (KUOUT,14)ENDER
       CALL FLUSH(KUOUT)
c    get the response
       READ (KUIN,21) A
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
c       WRITE (KUOUT,22)
c       CALL FLUSH(KUOUT)
22     CALL WARNZZ('@',' Answer y or n!@@')
       GOTO 10
       END

c******************************************************************************
c******************************************************************************
c
       SUBROUTINE GFNEZZ(EXT,NAME)
c
c      Gets file name with a given extent
c------------------------------------------------------------------------------
       PARAMETER    (NDNAME=32)
       CHARACTER*1  NAME(NDNAME),EXT(3)

       CHARACTER*11 LEADER
       CHARACTER*2  ENDER
       CHARACTER*1  FORM(6)
       CHARACTER*3  IBEG,IEND
c------------------------------------------------------------------------------
       INCLUDE 'INTWRKZZ.H'

       DATA  LEADER,ENDER/' Get_File {','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/

c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
        INCLUDE 'SCKIOZZ.H'
c       COMMON /IOKK/   KUOUT, KUIN
c       KUOUT = 6
c       KUIN  = 5

c------------------------------------------------------------------------------
c   get file name
9      FORMAT(A)

10     WRITE(KUOUT,9)LEADER
       WRITE(KUOUT,9)IBEG
       WRITE (KUOUT,12) EXT
12     FORMAT (3A1)
       WRITE(KUOUT,9)IEND
       WRITE(KUOUT,9)ENDER
       CALL FLUSH(KUOUT)
       READ (KUIN,13,ERR=10) NAME
13     FORMAT (80A1)
c    convert to upper case
       LEND = NDNAME
       DO L=1,NDNAME
c        look for extent
           IF (NAME(L).EQ.'.') LDOT = L
c        look for blank
           IF (NAME(L).EQ.' ') THEN
c            blank
               IF (LEND.EQ.NDNAME) LEND = L - 1
               ENDIF
           IF ((L.GT.LEND).AND.(NAME(L).NE.' ')) THEN
14             CALL WARNZZ('@',' Blanks not permitted.@@')
               GOTO 10
               ENDIF
c        capitalize
           IC = ICHAR(NAME(L))
           IF ((IC.GT.96).AND.(IC.LT.123)) IC = IC - 32
           NAME(L) = CHAR(IC)
           ENDDO
c    check name length
20     IF (LEND.GT.NDNAME-4)  THEN
           CALL WARNZZ('@',' Name too long.@@')
           GOTO 10
           ENDIF
       IF (LEND-LDOT.GT.3)  THEN
           CALL WARNZZ('@',' Extent too long.@@')
           GOTO 10
           ENDIF
c    check extent
       DO L=1,3
           IF (NAME(LDOT+L).NE.EXT(L))  THEN
               CALL MESBKK
                 WRITE (KUOUT,26) EXT
26               FORMAT (/' The extent must be .',3A1)
               CALL MESEKK
               GOTO 10
               ENDIF
           ENDDO
c     ok
       RETURN
       END
c******************************************************************************


c******************************************************************************
c
       SUBROUTINE HELP_not(K)
c
c      Test help routine
c------------------------------------------------------------------------------
c       IF (K.EQ.1) THEN
c               CALL MESSZZ('@',
c     ;        'Engines have pistons and cylinders.@@')
c           ELSEIF (K.EQ.2)  THEN
c               CALL MESSZZ('@',
c     ;        'Aspark is measured from compression TDC.@@')
c           ELSEIF (K.EQ.3)  THEN
c               CALL MESSZZ('@',
c     ;        'See your MATLAB manual.@@')
c          ENDIF
c         CALL HVP1VV(K)
c         CALL HELPVV(K)
       RETURN
       END
c******************************************************************************
c


c**********************************************************************
c
       SUBROUTINE PNDNKK(PRCNT)
c
c      Waits for user confirmation of a message
c----------------------------------------------------------------------
       CHARACTER*11  LEADER
       CHARACTER*2   ENDER,IBEG,IEND
       REAL          PRCNT
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' PaneDown {','} '/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
       WRITE (KUOUT,2)LEADER
          WRITE (KUOUT,*)IBEG
            WRITE(KUOUT,*) PRCNT
          WRITE (KUOUT,*)IEND
       WRITE (KUOUT,2)ENDER
       CALL FLUSH(KUOUT)
2      FORMAT (A)
3      FORMAT (F8.4)
       CALL FLUSH(KUOUT)
       RETURN
       END
c******************************************************************************

c**********************************************************************
c
       SUBROUTINE CLRNKK
c
c      Waits for user confirmation of a message
c----------------------------------------------------------------------
       CHARACTER*10  LEADER
       CHARACTER*1   ENDER,IBEG,IEND
       REAL          PRCNT
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' ClearOff ',' '/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
       WRITE (KUOUT,2)LEADER
       WRITE (KUOUT,2)ENDER
       CALL FLUSH(KUOUT)
2      FORMAT (A)
3      FORMAT (F8.4)
       CALL FLUSH(KUOUT)
       RETURN
       END
c******************************************************************************


c**********************************************************************
c
       SUBROUTINE CLRYKK
c
c      Waits for user confirmation of a message
c----------------------------------------------------------------------
       CHARACTER*9  LEADER
       CHARACTER*1   ENDER,IBEG,IEND
       REAL          PRCNT
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' ClearOn ',' '/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
c       WRITE (KUOUT,2)LEADER
c       WRITE (KUOUT,2)ENDER
       CALL FLUSH(KUOUT)
2      FORMAT (A)
3      FORMAT (F8.4)
       CALL FLUSH(KUOUT)
       RETURN
       END
c******************************************************************************



c**********************************************************************
c
       SUBROUTINE CLOWZZ
c
c      Waits for user confirmation of a message
c----------------------------------------------------------------------
       CHARACTER*12  LEADER
       CHARACTER*1   ENDER,IBEG,IEND
       REAL          PRCNT
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' Mess_clear ',' '/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
       WRITE (KUOUT,2)LEADER
       WRITE (KUOUT,2)ENDER
       CALL FLUSH(KUOUT)
2      FORMAT (A)
3      FORMAT (F8.4)
       CALL FLUSH(KUOUT)
       RETURN
       END
c******************************************************************************

c**********************************************************************
c
       SUBROUTINE BLMSZZ(K)
c
c      Begin Long Message Sequence
c          Turns Pause off so that a long a message
c          can be assembled piece by piece.
c----------------------------------------------------------------------
       CHARACTER*10  LEADER
       CHARACTER*1   ENDER,IBEG,IEND
       REAL          PRCNT
       INTEGER       K
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' PauseOff ',' '/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
       CALL PNDNKK(0.80)
       WRITE (KUOUT,2)LEADER
       WRITE (KUOUT,2)ENDER
       CALL FLUSH(KUOUT)
2      FORMAT (A)
3      FORMAT (F8.4)
       CALL FLUSH(KUOUT)
       CALL CLRNKK
       RETURN
       END
c******************************************************************************


c**********************************************************************
c
       SUBROUTINE ELMSZZ(K)
c
c      End Long Message Sequence
c          Turns Pause on after a long message
c----------------------------------------------------------------------
       CHARACTER*9  LEADER
       CHARACTER*1   ENDER,IBEG,IEND
       REAL          PRCNT
       INTEGER       K
c------------------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       INCLUDE 'SCKIOZZ.H'
c       DATA  BLANK /' '/
       DATA  LEADER,ENDER/' PauseOn ',' '/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
       WRITE (KUOUT,2)LEADER
       WRITE (KUOUT,2)ENDER
       CALL FLUSH(KUOUT)
2      FORMAT (A)
3      FORMAT (F8.4)
       CALL FLUSH(KUOUT)
       CALL CLRYKK
       CALL PAUSZZ(0)
       CALL PNDNKK(0.40)
       RETURN
       END
c******************************************************************************
c******************************************************************************
c
c      Non-graphical intrface routines likely to be platform-specific
c
c******************************************************************************
c
       SUBROUTINE SOWSZZ
c
c      Sets output window scroll
c------------------------------------------------------------------------------
c    no action
       RETURN
       END
c------------------------------------------------------------------------------
c******************************************************************************
c
       SUBROUTINE CLPWZZ
c
c      Clears the prompt window
c------------------------------------------------------------------------------
c    no action
       RETURN
       END
c******************************************************************************

c*****************************************************************************
c
       SUBROUTINE LOADZZ(A,N,B,M)
c
c      Loads string A of length N to string B starting at B(M+1).
c      Returns M as last load point. Used in building internal files.
c------------------------------------------------------------------------------
       CHARACTER*1 A(N),B(*)
c------------------------------------------------------------------------------
       DO I=1,N
           B(I+M) = A(I)
           ENDDO
       M = M + N
       RETURN
       END
c******************************************************************************











