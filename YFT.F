c*****************************************************************************
c
c      Valve flow routine test
c
c******************************************************************************
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 2896)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
c    diagnostic monitor
       LOGICAL OUT,PIN
       COMMON /MONMXX/IUMONM,MONM,OUT
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
       LMAX = 78
c------------------------------------------------------------------------------
c    write the restart file
       OPEN (10,FILE='FINDFLOW.BAD',FORM='UNFORMATTED')
       READ (10) PIN,Y,Z,PC,YC,AD,AV,CFI,CFO,FEGR,TREGR,
     ;       (RW(I),I=1,70)
       CLOSE (10)
       IUMONM = 9
c
c    open monitor file
       OPEN (IUMONM,FILE='TEST')
       MONM = 1
       OUT = .TRUE.
c
c     set the pointers
       CALL SPTRXX
c
c     call the routine
       CALL FDRPXX(RW,PIN,Y,Z,PC,YC,AD,AV,CFI,CFO,FEGR,TREGR,
     ;                   CD,PD,VD,YD,VVA,FLOW,IERR)
c
       CLOSE(IUMONM)
       STOP
       END
c*****************************************************************************
c
       SUBROUTINE MERRXX
c
c      Message to user about manifold error
c------------------------------------------------------------------------------
c    diagnostic monitor
       LOGICAL OUT
       COMMON /MONMXX/IUMONM,MON,OUT
c----------------------------------------------------------------------------
c    preserve the monitor file
       CLOSE (IUMONM)
       OPEN (IUMONM,FILE='MANTEMP.MON')
c    insruct user
       CALL WARNZZ('#','#An error occurred in the manifold '//
     ;   'calculation. File MANIFOLD.MON '//
     ;   'contains data needed to diagnose and fix the problem.  '//
     ;   'If your input data seem ok, please quit ESP now and save '//
     ;   'this file and your setup file in a separate directory '//
     ;   '(so that they will '//
     ;   'not be overwritten by your next ESP run) and contact# #'//
     ;   '    wcr@thermo.stanford.edu# #'//
     ;   'to arrange for transfer of these files so that we can '//
     ;   'make ESP work better.  Do not email these files!##')
       RETURN
       END
c*****************************************************************************
c
       SUBROUTINE MESSZZ(ESC,STRING)
c
c      Writes STRING to the standard output, breaking lines at blanks or ESC.
c      Two ESCs mark the end of STRING.
c------------------------------------------------------------------------------
       CHARACTER*1     ESC,STRING(512),A(80),BLANK
       LOGICAL         LESC
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
c
c    line space
       WRITE (*,8)
8      FORMAT (1X)
c
c    loop point for parsing string
10     IS = IS + 1
c    check for ESC
       IF (STRING(IS).EQ.ESC)  THEN
c        check for last also ESC
           IF (LESC)  THEN
c            end of STRING
               RETURN
               ENDIF
c        end of line encountered
           IF (IL.GT.0) THEN
c            write the current line
               WRITE (*,12) (A(I),I=1,IL)
12             FORMAT (1X,80A1)
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
           WRITE (*,12) (A(I),I=1,IL)
           IL = 0
           GOTO 10
           ENDIF
c    load the character
       IL = IL + 1
       A(IL) = STRING(IS)
       GOTO 10
       END
c******************************************************************************
c
       SUBROUTINE PAUSZZ
c
c      Waits for user confirmation of a message
c------------------------------------------------------------------------------
       WRITE (*,2)
2      FORMAT (/' Press enter to continue',$)
       READ (*,*)
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
       CALL PAUSZZ
       RETURN
       END
c******************************************************************************

