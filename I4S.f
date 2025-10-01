c**********************************************************************
c      FILE: I4S.f
c      This file contains subroutines used in the graphical Tcl/Tk
c      interface.
c      The primary routines for the non-graphical interface are
c      in I1.FOR.


c**********************************************************************
       FUNCTION NCHILD(JKTP)
c
c      RETURNS number of children given the pointer of parent
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    get parent type
       KTP = ID(JKTP)
c               WRITE(*,*)'JKTP= ',JKTP,'KTP= ',KTP,
c     ;          'ID(JKTP)= ',ID(JKTP),'ID(KTP)= ',ID(KTP)
c    branch by parent
       IF (KTP.EQ.1)  THEN
c            items
               NCHILD = ID(JKTP + 8)
           ELSEIF (KTP.EQ.2)  THEN
c            values
               NCHILD = ID(JKTP + 8)
           ELSEIF (KTP.EQ.3)  THEN
c            exclusive options
               NCHILD = ID(JKTP + 8)
           ELSEIF (KTP.EQ.4)  THEN
c            inclusive options
               NCHILD = ID(JKTP + 8)
           ELSEIF (KTP.EQ.5)  THEN
c            table
              IF (ID(JKTP + 8).GT.0)THEN
                 NCHILD = 1
              ELSE
                 NCHILD = 0
              ENDIF
           ELSEIF (KTP.EQ.6)  THEN
c            help options
               NCHILD = ID(JKTP + 5)
           ELSEIF (KTP.EQ.20)  THEN
c            option
               NCHILD = ID(JKTP + 6)
c            childless parent
           ELSEIF  ((KTP.EQ.10).OR.(KTP.EQ.11).OR.
     ;             (KTP.EQ.12).OR.(KTP.EQ.13).OR.(KTP.EQ.14).OR.
     ;             (KTP.EQ.21)) THEN
               NCHILD=0
           ELSE
              CALL ERRFZZ('@','NCHILD error; illegal parent.@@')
           ENDIF
       RETURN
       END
c**********************************************************************
c**********************************************************************
       FUNCTION NHELP(JKTP)
c
c      RETURNS number of help items given the pointer of parent
c----------------------------------------------------------------------
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    get parent type
       KTP = ID(JKTP)
c               WRITE(*,*)'JKTP= ',JKTP,'KTP= ',KTP,
c     ;          'ID(JKTP)= ',ID(JKTP),'ID(KTP)= ',ID(KTP)
c    branch by parent
       IF (KTP.LE.5)  THEN
c            group
               IF(ID(JKTP+5).GT.0) THEN
                  NHELP = 1
               ELSE
                  NHELP = 0
               ENDIF
           ELSEIF (KTP.EQ.6)  THEN
c            help box
               NHELP = ID(JKTP + 5)
           ELSEIF (KTP.GT.6) THEN
c            helpless item
               NHELP=0
           ELSE
              CALL ERRFZZ('@','NCHILD error; illegal parent.@@')
           ENDIF
       RETURN
       END
c**********************************************************************



c**********************************************************************
       SUBROUTINE STBLKK
c   Creates lookup table referring GUI index to pointers
       INCLUDE 'INTWRKZZ.H'
c----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, ITOT, IHTOT
c----------------------------------------------------------------------
c    initialize at top level
       JKT = ID(10)
       I = 0
       I2JKT(1) = JKT
       ITOT = 1
       IPJ  = 1
c   loop point
10     I = I + 1
         JKT = I2JKT(I)
c         write(*,*)I,JKT
         IF (I.GT.ITOT) GOTO 20
c        exceeded total # of objects
         DO J = 1,NCHILD(JKT)
            IPJ = ITOT +J
            I2JKT(IPJ) = ID(JCPZZ(JKT,J))
c            write(*,*)I,J,IPJ,JKT,JCPZZ(JKT,J),ID(JCPZZ(JKT,J))
c            PAUSE
         ENDDO
c        add children of current parent to total
         ITOT = ITOT + NCHILD(JKT)
       GOTO 10
c        done with semigods, now do help genealogy; re-initialize
c  SCK N=0 instead of N=1
c  changed 10/26/00 works with ESP, ESPCAM
c  to put help at zero level.
c
20     N = 0
       IH = 0
       IHTOT = 0
c      loop point for scanning through all semigods
25     N = N + 1
           IF (N.GT.ITOT) GOTO 40
           JKTN = I2JKT(N)
           IF (NHELP(JKTN).EQ.0) THEN
c              item with no help, skip to next semigod
              GOTO 25
           ENDIF
c          have found a semigod with help
           IH = IH + 1
           IHTOT = IHTOT + 1
           I2JKT(ITOT+IH) = ID(JHPKK(JKTN,1))
c          loop point for scanning children of a help item
35        JKT = I2JKT(ITOT+IH)
          DO 30 J = 1, NHELP(JKT)
             IHPJ = IHTOT + J
             I2JKT(ITOT+IHPJ) = ID(JHPKK(JKT,J))
30        ENDDO
          IHTOT = IHTOT + NHELP(JKT)
          IF (IH.GE.IHTOT) THEN
c           have exhausted all help items in this tree; go check
c           the help tree of the next semigod
            GOTO 25
          ENDIF
          IH = IH + 1
          GOTO 35
c
40     RETURN
       END

c**********************************************************************
       FUNCTION JKT2I(JKT)
c
c     Returns GUI index given JKT index
c
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       DIMENSION    I2JKT(NIMAXZ)
       COMMON      /TBLKK/  I2JKT, ITOT, NTHLP
c----------------------------------------------------------------------
       DO I=1,ITOT+NTHLP
         IF(I2JKT(I).EQ.JKT)THEN
            JKT2I=I
            RETURN
         ENDIF
       ENDDO
       CALL ERRFZZ('@','JKT2I error; illegal index.@@')
       RETURN
       END
c----------------------------------------------------------------------
c    check for god, who has no parent

c**********************************************************************
      SUBROUTINE DABKK
c
c    Displays all items in GUI by running through the IGI index,
c    determining type and calling appropriate support routines.
c    Tcl/Tk version.
c----------------------------------------------------------------------
c   Data storage
       INCLUDE 'INTWRKZZ.H'
       CHARACTER*1    HEADER
c----------------------------------------------------------------------
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
c    IGI to JKT translation
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize at top
c          display header for GUI selection area
       CALL INFORM('@','Data input area@@')

       DO 100 I = 1,NTOBJ
c         get type and branch
          JKT  = I2JKT(I)
          KT   = ID(JKT)
          JKTP = ID(JKT+1)
          IF (KT.EQ.1) THEN
c           zip box
              CALL DZBKK(I,JKT)
           ELSEIF (KT.EQ.2) THEN
c            values  box
             CALL  DVBKK(I,JKT)
           ELSEIF (KT.EQ.3) THEN
c            exclusive options
             CALL DEOBKK(I,JKT)
c             write(*,*)'exbox',I
           ELSEIF (KT.EQ.4) THEN
c            inclusive options
c              GOTO 100
             CALL DIOBKK(I,JKT)
c             write(*,*)'inbox',I
           ELSEIF  (KT.EQ.5) THEN
c             table object
             CALL DTBLKK(I,JKT)
c              CALL DTBHKK(JKT)
           ELSEIF (KT.EQ.6) THEN
c            help options
c             CALL DHBKK(I,JKT)
           ELSEIF (KT.EQ.10) THEN
c            group designation
c             write(*,*)'calling design'
c             CALL DGDKK(JKT)
           ELSEIF (KT.EQ.11) THEN
c            character value object
              CALL DCVKK(I,JKT)
           ELSEIF (KT.EQ.12) THEN
              IF(ID(JKTP).EQ.20) THEN
                CALL SETIVKK(I,JKT)
              ELSE
                CALL DIVKK(I,JKT)
              ENDIF
           ELSEIF (KT.EQ.13) THEN
c             real value object
              IF(ID(JKTP).EQ.20) THEN
                CALL SETRVKK(I,JKT)
              ELSE
                CALL DRVKK(I,JKT)
              ENDIF
           ELSEIF (KT.EQ.14) THEN
c             file-name value object
               IF(ID(JKTP).EQ.20) THEN
                CALL SETFVKK(I,JKT)
              ELSE
                CALL DFVKK(I,JKT)
              ENDIF
           ELSEIF (KT.EQ.20) THEN
c             option object
              CALL DOVKK(I,JKT)
c              STOP 'return from eoption'
c              write(*,*)'opbox',I
              GOTO 100
           ELSEIF (KT.EQ.21) THEN
c              CALL  DHOKK(I,JKT)
              GOTO 100
           ELSE
c              error
              CALL ERRFZZ('@','DZBKK error; illegal parent.@@')
           ENDIF
100      ENDDO
c    Done with GOD and children
c    Now turn to angel (helpers) genealogy

       DO 200 I=NTOBJ+1,NTOBJ+NTHLP
          JKT = I2JKT(I)
          IF(ID(JKT).EQ.6) THEN
            CALL DHBKK(I,JKT)
          ELSEIF(ID(JKT).EQ.21)THEN
            CALL  DHOKK(I,JKT)
          ENDIF
200    ENDDO
c    Done with angels; now do action buttons.
       DO 300 IA=1,ID(9)
          CALL DBTNKK(IA)
300    ENDDO
       RETURN
       END

c***********************************************************************
       SUBROUTINE DZBKK(I,JKT)
c
c    Constructs the zip-boxes in GUI.
c    Tcl/Tk version.
c-----------------------------------------------------------------------
c    Data storage.
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flshing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c  GUI calls
       CHARACTER*14 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c----------------------------------------------------------------------
       DATA LEADER,ENDER /'zbox_create { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
       EQUIVALENCE (ID(10),JKTGOD)
c----------------------------------------------------------------------
c    initialize parameters assuming zip-box (items group object)
             JKTP = ID(JKT+1)
             IGI  = ID(JKT+2)
             KS   = ID(JKT+3)
             JKTD = ID(JKT+4)
             JKTH = ID(JKT+5)
             JCH  = ID(JKT+6)
             NCH  = ID(JKT+7)
             NIS  = ID(JKT+8)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c             write(*,*)JKT,JKTP,IOF
c          initialize Tcl/Tk routine
              WRITE(KUOUT,10)LEADER
c             sent box title
                CALL CFCZZ(NCH+2,FORM(2))
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCH-1),J=1,NCH)
                WRITE(KUOUT,12)IEND
c             sent item GUI index
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c             sent perent GUI index
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c             sent status
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)KS
                WRITE(KUOUT,12)IEND
c             sent number of children
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)NIS
                WRITE(KUOUT,12)IEND
c              sent GUI index for each child
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,13)(JKT2I(ID(JKT+8+NISCNT)),NISCNT=1,NIS)
                WRITE(KUOUT,12)IEND
c   close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c   create designation button
              CALL DGDKK(JKTD)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
13     FORMAT(10I3)
c  finish up
       RETURN
       END


************************************************************************
       SUBROUTINE DVBKK(I,JKT)
c
c    Constructs the value-boxes in GUI.
c    Tcl/Tk version.
c-----------------------------------------------------------------------
c    Data storage.
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       CHARACTER*14 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
c   Tcl/TK calls
       DATA LEADER,ENDER /'vbox_create { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming values-box (items group object)
             JKTP = ID(JKT+1)
             IGI  = ID(JKT+2)
             KS   = ID(JKT+3)
             JKTD = ID(JKT+4)
             JKTH = ID(JKT+5)
             JCH  = ID(JKT+6)
             NCH  = ID(JKT+7)
             NVS  = ID(JKT+8)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c          initialize Tcl/Tk call
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCH+2,FORM(2))
c             sent value box header
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCH-1),J=1,NCH)
                WRITE(KUOUT,12)IEND
c             sent item GUI index
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c             sent parent GUI index
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c             sent status
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)KS
                WRITE(KUOUT,12)IEND
c             sent number of values
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)NVS
                WRITE(KUOUT,12)IEND
c             sent GUI index for each child
                WRITE(KUOUT,12)IBEG
                   WRITE(KUOUT,13)(JKT2I(ID(JKT+8+NVSCNT)),NVSCNT=1,NVS)
                 WRITE(KUOUT,12)IEND
c           close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c           display group designation button
              CALL DGDKK(JKTD)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
13     FORMAT(I3)
c  finish up
       RETURN
       END


c***********************************************************************
       SUBROUTINE DOBKK(I,JKT)
c
c    Constructs the option boxes  in GUI.
c----------------------------------------------------------------------
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c  i/o units (explicit declaration needed for flshing)
       COMMON /IOKK/ KUOUT, KUIN
       CHARACTER*14 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c----------------------------------------------------------------------
       DATA LEADER,ENDER /'box_create { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming zip-box (items group object)
             JKTP = ID(JKT+1)
             IGI  = ID(JKT+2)
             KS   = ID(JKT+3)
             JCS  = ID(JKT+4)
             NCS  = ID(JKT+5)
             NVS  = ID(JKT+6)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCH+2,FORM(2))
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCH-1),J=1,NCH)
                WRITE(KUOUT,12)IEND
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
              CALL DGDKK(JKTD)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END

c***********************************************************************
       SUBROUTINE DEOBKK(I,JKT)
c
c    Constructs the exclusive option boxes in GUI.
c    Tcl/Tk version.
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
       CHARACTER*14 LEADER
       CHARACTER*2  ENDER
       CHARACTER*2  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
c   Tcl/Tk calls
       DATA LEADER,ENDER /'eobox_create { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c-----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c-----------------------------------------------------------------------
c    initialize parameters assuming exclusive-options-box
             JKTP = ID(JKT+1)
             IGI  = ID(JKT+2)
             KS   = ID(JKT+3)
             JKTD = ID(JKT+4)
             JKTH = ID(JKT+5)
             JCH  = ID(JKT+6)
             NCH  = ID(JKT+7)
             NIS  = ID(JKT+8)
             NOP  = ID(JKT+9)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c          initialize Tcl/Tk routine
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCH+2,FORM(2))
c             sent header for exclusive options box
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCH-1),J=1,NCH)
                WRITE(KUOUT,12)IEND
c             sent GUI index for item
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c             sent GUI index for parent
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c             sent chosen option
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,*)NOP
                WRITE(KUOUT,12)IEND
c           close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
               CALL FLUSH(KUOUT)
c           display group designation button
c              if(ID(ID(JKT+9+NOP)).EQ.5)then
c                CALL MESBKK
c                  write(*,*)'TABLE EXL DES',JKT,ID(JKT+9+NOP+4)
c                CALL MESEKK
c              endif
c                CALL MESBKK
c                WRITE(KUOUT,12)IBEG
c                  WRITE(KUOUT,FORM)(CD(J+JCH-1),J=1,NCH)
c                WRITE(KUOUT,12)IEND
c                  write(*,*)'EXL SEL',JKT,NOP,ID(ID(JKT+9+NOP)),
c     ;             ID(ID(JKT+9+NOP)+4),ID(ID(ID(JKT+9+NOP)+4)),
c     ;             ID(ID(ID(JKT+9+NOP)+4)+3),JKTD,ID(JKTD+1),
c     ;             ID(JKT+9+NOP)
c                CALL MESEKK
               IF(NOP.NE.ID(1))THEN
                 CALL DGDKK(ID(JKT+9+NOP))
               ELSE
                CALL DGDKK(JKTD)
               ENDIF
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END

c**********************************************************************
      SUBROUTINE INFORM (ESC,STRING)
c**********************************************************************
c
c   TCL/TK interface routines
c
c**********************************************************************
c
c      SUBROUTINE MESSAGE (ESC,STRING)
c
c      Sends a message to TCL
c      ESC is the ASCI escape character terminating the message STRING
c      having maximum length 2000 characters.
c
c      TCL/TK displays the message in the information box. No action is
c      taken. The FORTRAN code should follow the message with an
c      appropriate action request.
c
c      Use \n for new line.
c______________________________________________________________________
       PARAMETER (NDIM=2000)
       CHARACTER*1 ESC,STRING(*)
       CHARACTER*14 LEADER
       CHARACTER*2  ENDER
c______________________________________________________________________
       COMMON /IOKK/ KUOUT, KUIN
c______________________________________________________________________
       DATA    LEADER,ENDER/' action_info "','" '/
c______________________________________________________________________
c    find the end of the string
       DO 9 I=1,NDIM
           IF (STRING(I).EQ.ESC)  THEN
               N = I - 1
               GOTO 10
               ENDIF
9          CONTINUE
       N = NDIM
c    send the data
10     WRITE (KUOUT,12) LEADER,(STRING(I),I=1,N),ENDER
12     FORMAT (2000A)
c    flush the pipe
       CALL FLUSH(KUOUT)
       RETURN
       END
c**********************************************************************


c**********************************************************************
       SUBROUTINE DHBKK(I,JKT)
c
c    Constructs the help-boxes in GUI.
c    Tcl/Tk version.
c----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c----------------------------------------------------------------------
c  Tcl/Tk calls
       CHARACTER*14 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c----------------------------------------------------------------------
       DATA LEADER,ENDER /'hbox_create { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
c  IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming help-box (items group object)
             IF(JKT.EQ.0)RETURN
             JKTP = ID(JKT+1)
             IGI  = ID(JKT+2)
             JCH  = ID(JKT+3)
             NCH  = ID(JKT+4)
             NHO  = ID(JKT+5)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c          initialize Tcl/Tk routine
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCH+2,FORM(2))
c             sent header for help box
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCH-1),J=1,NCH)
                WRITE(KUOUT,12)IEND
c             sent GUI index for box
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c             sent GUI index for parent
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c            close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c    formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END

c***********************************************************************
       SUBROUTINE DIVKK(I,JKT)
c
c    Adds integer entry value fields
c    Tcl/Tk version
c-----------------------------------------------------------------------
c    DATA storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flshing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c  Tcl/Tk calls
       CHARACTER*13 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6),FORMI(4)
c-----------------------------------------------------------------------
       DATA LEADER,ENDER /'ientry_add { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA FORMI /'(',2*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c  IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming integer value item
             JKTP  = ID(JKT+1)
             IGI   = ID(JKT+2)
             KS    = ID(JKT+3)
             JCN   = ID(JKT+4)
             NCN   = ID(JKT+5)
             KR    = ID(JKT+6)
             IV    = ID(JKT+7)
             IVMIN = ID(JKT+8)
             IVMAX = ID(JKT+9)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
c              CALL INFORM('@','Select an option')
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCN+2,FORM(2))
c 0. title
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCN-1),J=1,NCN)
                WRITE(KUOUT,12)IEND
c 1. igi
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c 2. parent igi
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c 3. status
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)KR
                WRITE(KUOUT,12)IEND
c 4. min val
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,*)IVMIN
                WRITE(KUOUT,12)IEND
c 5. max val
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,*)IVMAX
                WRITE(KUOUT,12)IEND
c 6. value
                IF (IV.NE.ID(1))THEN
                   WRITE(KUOUT,12)IBEG
                      CALL CFIZZ(IV,FORMI(2))
                      WRITE(KUOUT,FORMI)IV
                   WRITE(KUOUT,12)IEND
                ELSE
                   WRITE(KUOUT,12)IBEG
                      WRITE(KUOUT,*)""
                   WRITE(KUOUT,12)IEND
                ENDIF
c          close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END


c***********************************************************************
       SUBROUTINE DCVKK(I,JKT)
c
c    Adds character entry value fields
c    Tcl/Tk version
c-----------------------------------------------------------------------
c     Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flshing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c  Tcl/Tk calls
       CHARACTER*13 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c----------------------------------------------------------------------
       DATA LEADER,ENDER /'centry_add { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming character value item
             JKTP  = ID(JKT+1)
             IGI   = ID(JKT+2)
             KS    = ID(JKT+3)
             JCN   = ID(JKT+4)
             NCN   = ID(JKT+5)
             JCS   = JCN + NCN
             NCS   = ID(JKT+6)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
c              CALL INFORM('@','Select an option')
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCN+2,FORM(2))
c 0. title
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCN-1),J=1,NCN)
                WRITE(KUOUT,12)IEND
c 1. igi
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c 2. parent igi
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c 3. value
                IF(KS.GT.0)THEN
                  IF(CD(JCS).NE.CD(1).AND.CD(JCS).NE.CHAR(0))THEN
                   WRITE(KUOUT,12)IBEG
                     WRITE(KUOUT,*)(CD(J+JCS-1),J=1,NCS)
                   WRITE(KUOUT,12)IEND
                  ELSE
                   WRITE(KUOUT,12)IBEG
                     WRITE(KUOUT,*)""
                   WRITE(KUOUT,12)IEND
                  ENDIF
                ENDIF
c   close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END
c***********************************************************************
       SUBROUTINE DRVKK(I,JKT)
c    Adds real value entry fields
c    Tcl/Tk version
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
       CHARACTER*13 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6),FORMR(10)
c-----------------------------------------------------------------------
c   Tcl/Tk calls
       DATA LEADER,ENDER /'rentry_add { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA FORMR /'(',8*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming real value item
             JKTP  = ID(JKT+1)
             IGI   = ID(JKT+2)
             KS    = ID(JKT+3)
             JCN   = ID(JKT+4)
             NCN   = ID(JKT+5)
             KR    = ID(JKT+6)
             JRV   = ID(JKT+7)
             RVMIN = ID(JKT+8)
             RVMAX = ID(JKT+9)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
c              CALL INFORM('@','Select an option')
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCN+2,FORM(2))
c 0. title
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCN-1),J=1,NCN)
                WRITE(KUOUT,12)IEND
c 1. igi
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c 2. parent igi
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c 3. value
                IF(RD(JRV).NE.RD(1))THEN
                  WRITE(KUOUT,12)IBEG
                    CALL CFRZZ(RD(JRV),FORMR(2))
                    WRITE(KUOUT,FORMR)RD(JRV)
                  WRITE(KUOUT,12)IEND
                ELSE
                  WRITE(KUOUT,12)IBEG
                    WRITE(KUOUT,12)' '
                  WRITE(KUOUT,12)IEND
                ENDIF
c           close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I4)
12     FORMAT(A2)
13     FORMAT(F20.6)
14     FORMAT(A50)
c  finish up
       RETURN
       END

c***********************************************************************
       SUBROUTINE SETRVKK(I,JKT)
c    Sets real value entry fields
c    Tcl/Tk version
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
       CHARACTER*13 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6),FORMR(10)
c-----------------------------------------------------------------------
c   Tcl/Tk calls
       DATA LEADER,ENDER /'xvalue_set { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA FORMR /'(',8*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming real value item
                JKTP  = ID(JKT+1)
                IGI   = ID(JKT+2)
                KS    = ID(JKT+3)
                JCN   = ID(JKT+4)
                NCN   = ID(JKT+5)
                KR    = ID(JKT+6)
                JRV   = ID(JKT+7)
                RVMIN = ID(JKT+8)
                RVMAX = ID(JKT+9)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
c              CALL INFORM('@','Select an option')
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCN+2,FORM(2))
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
                IF (RD(JRV).NE.RD(1))THEN
                   WRITE(KUOUT,12)IBEG
                     CALL CFRZZ(RD(JRV),FORMR(2))
                     WRITE(KUOUT,FORMR)RD(JRV)
                   WRITE(KUOUT,12)IEND
                ELSE
                   WRITE(KUOUT,12)IBEG
                     WRITE(KUOUT,*)""
                   WRITE(KUOUT,12)IEND
                ENDIF
c           close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END

c***********************************************************************
       SUBROUTINE SETIVKK(I,JKT)
c    Sets integer value entry fields
c    Tcl/Tk version
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
       CHARACTER*13 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6),FORMI(4)
c-----------------------------------------------------------------------
c   Tcl/Tk calls
       DATA LEADER,ENDER /'xvalue_set { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA FORMI /'(',2*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming real value item
                JKTP  = ID(JKT+1)
                IGI   = ID(JKT+2)
                KS    = ID(JKT+3)
                JCN   = ID(JKT+4)
                NCN   = ID(JKT+5)
                KR    = ID(JKT+6)
                IV    = ID(JKT+7)
                IVMIN = ID(JKT+8)
                IVMAX = ID(JKT+9)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
c              CALL INFORM('@','Select an option')
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCN+2,FORM(2))
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
                IF (IV.NE.ID(1))THEN
                   WRITE(KUOUT,12)IBEG
                     CALL CFIZZ(IV,FORMI(2))
                     WRITE(KUOUT,FORMI)IV
                   WRITE(KUOUT,12)IEND
                ELSE
                   WRITE(KUOUT,12)IBEG
                     WRITE(KUOUT,*)""
                   WRITE(KUOUT,12)IEND
                ENDIF
c           close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END
c***********************************************************************
       SUBROUTINE SETFVKK(I,JKT)
c    Sets integer value entry fields
c    Tcl/Tk version
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
       CHARACTER*13 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
c   Tcl/Tk calls
       DATA LEADER,ENDER /'fvalue_set { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming file value item
                JKTP  = ID(JKT+1)
                IGI   = ID(JKT+2)
                KS    = ID(JKT+3)
                JCN   = ID(JKT+4)
                NCN   = ID(JKT+5)
                NCF   = ID(JKT+6)
                JCF   = JCN + NCN
                KRW   = ID(JKT+7)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
c              CALL INFORM('@','Select an option')
c              CALL MESBKK
c                 write(*,*)'HI'
c              CALL MESEKK
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCN+2,FORM(2))
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
                IF(KS.GT.0)THEN
                   IF(CD(JCF).NE.CD(1).AND.CD(JCF).NE.CHAR(0))THEN
                      WRITE(KUOUT,12)IBEG
                          WRITE(KUOUT,*)(CD(J+JCF-1),J=1,NCF)
                      WRITE(KUOUT,12)IEND
                   ELSEIF(CD(JCN).NE.CD(1).AND.CD(JCN).NE.CHAR(0))THEN
                      WRITE(KUOUT,12)IBEG
                         WRITE(KUOUT,*)(CD(J+JCN-1),J=1,NCN)
                      WRITE(KUOUT,12)IBEG
                   ELSE
                      WRITE(KUOUT,12)IBEG
                          WRITE(KUOUT,*)""
                      WRITE(KUOUT,12)IEND
                   ENDIF
                ELSE
                   IF(CD(JCN).NE.CD(1).AND.CD(JCN).NE.CHAR(0))THEN
                      WRITE(KUOUT,12)IBEG
                         WRITE(KUOUT,*)(CD(J+JCN-1),J=1,NCN)
                      WRITE(KUOUT,12)IBEG
                   ELSE
                      WRITE(KUOUT,12)IBEG
                          WRITE(KUOUT,*)""
                      WRITE(KUOUT,12)IEND
                   ENDIF
                ENDIF
c           close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END

c***********************************************************************
       SUBROUTINE DFVKK(I,JKT)
c    Adds file name  fields
c    Tcl/Tk version
c-----------------------------------------------------------------------
c   data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c   GUI interaction calls
       CHARACTER*13 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c----------------------------------------------------------------------
       DATA LEADER,ENDER /'fentry_add { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c----------------------------------------------------------------------
c    IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming file name item
             JKTP  = ID(JKT+1)
             IGI   = ID(JKT+2)
             KS    = ID(JKT+3)
             JCN   = ID(JKT+4)
             NCN   = ID(JKT+5)
             JCF   = JCN + NCN
             NCF   = ID(JKT+6)
c  set TCL index if parent is god set TCL index to zero
c             CALL MESBKK
c                  write(*,*)'Entering DFVKK'
c             CALL MESEKK
              IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
              WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCN+2,FORM(2))
c 0. title
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCN-1),J=1,NCN)
                WRITE(KUOUT,12)IEND
c 1. igi
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c 2. parent igi
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND

c 3. value
                IF(CD(JCF).NE.CD(1).AND.CD(JCF).NE.CHAR(0))THEN
                 WRITE(KUOUT,12)IBEG
                   WRITE(KUOUT,*)(CD(J+JCF-1),J=1,NCF)
                 WRITE(KUOUT,12)IEND
                ELSE
                 WRITE(KUOUT,12)IBEG
                   WRITE(KUOUT,*)""
                 WRITE(KUOUT,12)IEND
                ENDIF
c  close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END


c**********************************************************************
       SUBROUTINE DOVKK(I,JKT)
c
c    Adds option items (both exclusive and inclusive)
c    Tcl/Tk version
c----------------------------------------------------------------------
c    DATA storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
       CHARACTER*14 ELEADER,ILEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
c   Tcl/Tk calls
       DATA ELEADER       /'eoption_add { '/
       DATA ILEADER       /'ioption_add { '/
       DATA ENDER /'} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c-----------------------------------------------------------------------
c    initialize parameters assuming e/i option
             JKTP  = ID(JKT+1)
             IGI   = ID(JKT+2)
             KS    = ID(JKT+3)
             JCS  =  ID(JKT+4)
             NCS   = ID(JKT+5)
             NVS   = ID(JKT+6)
             JKTV1 = ID(JKT+7)
             JKTV2 = ID(JKT+8)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
c              CALL INFORM('@','Select an option')
c           choose exclusive or inclusive
                IF(ID(JKTP).EQ.3)THEN
                   WRITE(KUOUT,10)ELEADER
                ELSEIF(ID(JKTP).EQ.4)THEN
                   WRITE(KUOUT,10)ILEADER
                ENDIF
                CALL CFCZZ(NCS+2,FORM(2))
c
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCS-1),J=1,NCS)
                WRITE(KUOUT,12)IEND
c
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)NVS
                WRITE(KUOUT,12)IEND
c
                if(NVS.GT.0)THEN
                WRITE(KUOUT,12)IBEG
                     DO J = 1, NVS
                      WRITE(KUOUT,11) JKT2I(ID(JKT+6+J))
                     ENDDO
                WRITE(KUOUT,12)IEND
                   WRITE(KUOUT,12)IBEG
                     DO J = 1, NVS
                         WRITE(KUOUT,12) ""
                     ENDDO
                   WRITE(KUOUT,12)IEND
               ENDIF
cc           close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END

c**********************************************************************
       SUBROUTINE DIOBKK(I,JKT)
c
c    Constructs the inclusive-ption boxes  in GUI.
c    Tcl/Tk version
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
       CHARACTER*14 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
c   Tcl/Tk calls
       DATA LEADER,ENDER /'iobox_create { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming inclusive-options group
             JKTP   = ID(JKT+1)
             IGI    = ID(JKT+2)
             KS     = ID(JKT+3)
             JKTD   = ID(JKT+4)
             JKTH   = ID(JKT+5)
             JCH    = ID(JKT+6)
             NCH    = ID(JKT+7)
             NOS    = ID(JKT+8)
             NOPMAX = ID(JKT+9)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  determine if all items the same
       DO IX=1,NOS
c        get object data
           JKTO = ID(JKT+9+NOS+IX)
           KTO = ID(JKTO)
           IF ((KTO.NE.20).OR.(ID(JKTO+6).NE.0))  GOTO 8
c        option with no parameters
           IF (IX.EQ.1)  THEN
                   NCS = ID(JKTO+5)
               ELSE
                   IF (ID(JKTO+5).NE.NCS)  GOTO 8
               ENDIF
           ENDDO
c    ojects all the same length
       NLINE = 79/(NCS+6)
       GOTO 9
c
c    objects not same
8      NLINE = 1
c          initilize  Tcl/Tk routine
9               WRITE(KUOUT,10)LEADER
                CALL CFCZZ(NCH+2,FORM(2))
c            sent header for inclusive options box
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCH-1),J=1,NCH)
                WRITE(KUOUT,12)IEND
c            sent GUI index for box
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
c            sent GUI index for parent
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c            sent GUI index for option-child
                WRITE(KUOUT,12)IBEG
                  DO NI = 1, NOS
                       WRITE(KUOUT,11)JKT2I(ID(JKT+9+NOS+NI))
                       WRITE(KUOUT,11)ID(JKT+9+NI)
                  ENDDO
                WRITE(KUOUT,12)IEND
c              sent GUI number of columns
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)NLINE
                WRITE(KUOUT,12)IEND
c          close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c          display designation button
              CALL DGDKK(JKTD)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END

c***********************************************************************
       SUBROUTINE DHOKK(I,JKT)
c
c    Adds help item in GUI
c    Tcl/Tk version
c-----------------------------------------------------------------------
c     Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
       CHARACTER*14 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
c   Tcl/Tk calls
       DATA LEADER,ENDER /'helpo_add { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c    initialize parameters assuming zip-box (items group object)
             IF(JKT.EQ.0)RETURN
             JKTP   = ID(JKT+1)
             IGI    = ID(JKT+2)
             JCA    = ID(JKT+3)
             NCA    = ID(JKT+4)
             KHELP  = ID(JKT+5)
c  set TCL index if parent is god set TCL index to zero
c  header for Tcl/Tk routine
              WRITE(KUOUT,10)LEADER
c              write(*,*)'before form',JKTP,IGI,JCA,NCA,KHELP
c                CALL CFCZZ(NCA,FORM(2))
c               write(*,*)'after form'

                WRITE(KUOUT,12)IBEG
                   WRITE (KUOUT,*)(CD(J),J=JCA,JCA+NCA-1)
2                  FORMAT (I4,2X,80A1)
                WRITE(KUOUT,12)IEND
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)I
                WRITE(KUOUT,12)IEND
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)JKT2I(JKTP)
                WRITE(KUOUT,12)IEND
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)KHELP
                WRITE(KUOUT,12)IEND
c          close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END

c***********************************************************************
       SUBROUTINE DTBLKK(I,JKT)
c
c    Displays table-box in GUI
c    Tcl/Tk version
c      Table display calls:
c          DTBLKK: sets up column headers
c          DTABKK: loads actual values into cells
c          CTABKK: configures table (column width etc.)
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c  Tcl/Tk calls
       CHARACTER*14 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
       DATA LEADER,ENDER /'tbox_create { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c  IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
       COMMON  /TBLKK/  I2JKT, NTOBJ, NTHLP
       COMMON  /TBLINF/ NJSC(5,100), NJCC(5,100)
c----------------------------------------------------------------------
c    initialize parameters assuming tablebox (table object)
             JKTP   = ID(JKT+1)
             IGI    = ID(JKT+2)
             KS     = ID(JKT+3)
             JKTD   = ID(JKT+4)
             JKTH   = ID(JKT+5)
             JCH    = ID(JKT+6)
             NCH    = ID(JKT+7)
             NRW    = ID(JKT+9)
             NFC    = ID(JKT+10)
             NVC    = ID(JKT+11)
c  construct table and column headers
             CALL DTBHKK(JKT)
c  set TCL index if parent is god set TCL index to zero
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c          start Tcl/tk call
              CALL  CFCZZ(14,FORM(2))
              WRITE(KUOUT,10)LEADER
c          sent item index
               WRITE(KUOUT,12)IBEG
                 WRITE(KUOUT,11)I
               WRITE(KUOUT,12)IEND
c          sent parent index
               WRITE(KUOUT,12)IBEG
                 WRITE(KUOUT,11)IOFP
               WRITE(KUOUT,12)IEND
c          sent table header
               CALL  CFCZZ(NCH+2,FORM(2))
               WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,FORM)(CD(J+JCH-1),J=1,NCH)
               WRITE(KUOUT,12)IEND
c          sent number of rows
               WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)NRW
               WRITE(KUOUT,12)IEND
c          sent number of frozen columns
               WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)NFC
               WRITE(KUOUT,12)IEND
c          sent number of variable columns
               WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)NVC
               WRITE(KUOUT,12)IEND
c          sent column headers
                DO N=1,NFC+NVC
                  WRITE(KUOUT,12)IBEG
                  DO II=1,3
                     CALL CFCZZ(NJCC(II,N)+2,FORM(2))
                     WRITE(KUOUT,FORM)
     ;                (CD(NJSC(II,N)+JJ-1),JJ=1,NJCC(II,N))
                  ENDDO
                  WRITE(KUOUT,12)IEND
                ENDDO
c          close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c          load values into table cells
              CALL DTABKK(JKT)
c          configure column width, restrictions
              CALL CTABKK(JKT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END

c***********************************************************************
       SUBROUTINE DGDKK(JKT)
c    Constructs the group-designation in GUI.
c    Tcl/Tk version
c    IMPORTANT: The EO case is handled differently. In that case
c                JKT is the selected option index not the designator
c                one. For everything else JKT is the designator index.
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(10),JKTGOD)

c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c  Tcl/Tk calls
       CHARACTER*11 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
       DATA LEADER,ENDER /'g_design { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c   initialize parameters assuming group-designation
             JKTP = ID(JKT+1)
             IGI  = ID(JKT+2)
             KS   = ID(JKT+3)
             JCD  = ID(JKT+4)
             NCD  = ID(JKT+5)
c  set TCL index if parent is god set TCL index to zero
             IF(JKT.EQ.0)RETURN
             IF(JKTP.EQ.0)THEN
                IOFP = 0
             ELSE
                IOFP = JKT2I(JKTP)
             ENDIF
c  header for Tcl/Tk routine
              WRITE(KUOUT,10)LEADER
c                CALL CFCZZ(NCD+2,FORM(2))
                WRITE(KUOUT,12)IBEG
                IF(KS.GT.0)THEN
                  IF(ID(JKTP).EQ.3)THEN
                     CALL DEODKK(JKT)
                  ELSE
c                     write(*,*)'printing'
                   IF((CD(JCD).NE.CD(1)).AND.(CD(JCD+2).NE.CHAR(0)))
     ;                   THEN
                         WRITE(KUOUT,*)(CD(J+JCD-1),J=1,NCD)
                     ELSE
                         WRITE(KUOUT,*)' '
                     ENDIF
                  ENDIF
                ENDIF
                WRITE(KUOUT,10)IEND
c---
c----
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IOFP
                WRITE(KUOUT,12)IEND
c          close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       IF((KS.GT.1).AND.(ID(JKTP).EQ.3).AND.(JKTP.NE.JKTGOD)) THEN
c                   CALL CHKSKK(ID(JKTP+1),OK)
       ENDIF
       RETURN
       END
c***********************************************************************

c***********************************************************************
       SUBROUTINE SUNKK
c
c    Trasmits default unset values to GUI.
c    Tcl/Tk version
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c  Tcl/Tk calls
       CHARACTER*13 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
       DATA LEADER,ENDER /'def_unsets { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c-----------------------------------------------------------------------
       WRITE(KUOUT,10) LEADER
         WRITE(KUOUT,10)IBEG
            WRITE(KUOUT,11)RD(1)
         WRITE(KUOUT,10)IEND
         WRITE(KUOUT,10)IBEG
            WRITE(KUOUT,12)ID(1)
         WRITE(KUOUT,10)IEND
         WRITE(KUOUT,10)IBEG
            WRITE(KUOUT,10)CD(1)
         WRITE(KUOUT,10)IEND
c    close Tcl/Tk call
       WRITE(KUOUT,10) ENDER
       CALL FLUSH(KUOUT)
c   fomrats
10     FORMAT(A15)
11     FORMAT(E14.8)
12     FORMAT(I20)
c   finish up
       RETURN
       END

c***********************************************************************
       SUBROUTINE CH2NKK(alpha,ntype,kint,vreal,nerr,npreset)
c
c    Converts input string to real or integer. Used as a guard against
c    flaky compiler handling of "read" scenarios.
c-----------------------------------------------------------------------
c
c  Input:  alpha -- Alphnumeric string to be convertred to real number
c
c  Return: vreal -- Real number corresponding to "alpha"
c          kint  -- Integer number corresponding to "alpha"
c          ntype -- Type ( 0=real, 1=integer)
c          nerr  -- Error code  0 = succesfull conversion
c                               1 = alphanumeric characters in input
c                               2 = multiple numbers entered
c                               3 = null string entered
c                               4 = exponent (number) is too large
c                               5 = exceeded ansi signed integer
c                               6 = exponent with no exponent digit
c                               7 = last digit is +/- sign.
c
c  On return value stored in "vreal" or "kint" depending on the ntype
c  of the input (real vs. integer)
c
c-----------------------------------------------------------------------
c
c       double precision vreal,rdec
       real    vreal,rdec
       integer k1,k2,kdec,kexp,ntype
       integer mspace,mplus,mminus,mexp,mdexp,mdec
       integer I,k,nbase
       integer jexp,jdec,jint
       character*80 alpha
c
c-------Initialization
       nerr=0
       mchar=0
       mspace=0
       mplus=0
       mminus=0
       mexp=0
       mdexp=0
       mdec=0
       kdec=0
       kexp=0
       kminus=0
       kplus=0
       klen=80
       maxe=99
       rdec=0.0
c
c       print*,'Enter a number:'
c       read(*,'(A80)') alpha
c       print*,'...read',alpha
c
c-------Strip leading "spaces"
c       CALL MESBKK
c            write(*,*)'ENTERING CH2KK with APLHA= ',ALPHA
c       CALL MESEKK
       k1=0
 100   k1=k1+1
       if (alpha(k1:k1).ne.' ') goto 110
       if (k1.lt.klen-1) goto 100
       nerr=3
       goto 5000
 110   continue
c
c-------Strip trailing "spaces"
       k2=klen+1
 200   k2=k2-1
       if (alpha(k2:k2).ne.' ') goto 210
       if (k2.gt.1) goto 200
 210   continue
c
c-------Check for special characters
       do 300 k=k1,k2
c
         if(alpha(k:k).eq.' ') mspace=mspace+1
         if(alpha(k:k).eq.'+') then
            mplus=mplus+1
            if(kplus.eq.0) kplus=k
         end if
         if(alpha(k:k).eq.'-') then
            mminus=mminus+1
            if(kminus.eq.0) kminus=k
         end if
         if(alpha(k:k).eq.'e'.or.alpha(k:k).eq.'E') then
            mexp=mexp+1
            if(kexp.eq.0) kexp=k
         end if
         if(alpha(k:k).eq.'d'.or.alpha(k:k).eq.'D') then
            mdexp=mdexp+1
            if(kexp.eq.0) kexp=k
         end if
         if(alpha(k:k).eq.'.') then
            mdec=mdec+1
            if(kdec.eq.0) kdec=k
         end if
c
c------Characters that should not appear at all
c      first get ascii code
         I=ichar(alpha(k:k))
         if(I.LT.48.OR.I.GT.57) then
c        we have something other than 0 to 9
           if(I.NE.43.AND.I.NE.45) then
c          and it is not "+" or "-"
             if(I.NE.68.AND.I.NE.69) then
c            and it is not "D" or "E"
               if(I.NE.100.AND.I.NE.101) then
c              and it is not "d" or "e"
                 if (I.NE.46) then
c                and it is not "."
                    if (I.NE.32) then
c                   and it is not space
                      mchar=mchar+1
                    end if
                 end if
               end if
             end if
           end if
         end if
 300   continue
c
      if(mchar.gt.0)          nerr=1
      if(mdec.gt.1)           nerr=1
      if((mplus+mminus).gt.2) nerr=1
      if((mexp+mdexp).gt.1)   nerr=1
      if(kexp.eq.k2)          nerr=6
      if(alpha(k2:k2).eq.'+') nerr=7
      if(alpha(k2:k2).eq.'-') nerr=7
      if(mspace.gt.0)         nerr=2
c      if(mspace.gt.0)then
c      if( (mspace.eq.1).and.( (alpha(k1:k1).eq.'+')
c     ;    .or.(alpha(k1:k1).eq.'-')).and.(alpha(k1+1:k1+1).eq.' '))then
c          alpha(k1+1:k1+1) = alpha(k1:k1)
c          alpha(k1:k1)     = ' '
c      else
c               nerr=2
c      endif
c      endif
c
      if(nerr.gt.0) goto 5000
c
c------Resolve cases with single +/- sign
      if ((mplus+mminus).eq.1) then
        if (alpha(k1:k1).ne.'+'.and.alpha(k1:k1).ne.'-') then
           if ((mexp+mdexp).eq.0) then
              nerr=1
           elseif ((mexp+mdexp).gt.0) then
              if (alpha(kexp+1:kexp+1).ne.'+') then
                 if (alpha(kexp+1:kexp+1).ne.'-') nerr=1
              end if
           end if
        end if
      endif
c------Resolve cases with multiple +/- signs
      if ((mplus+mminus).eq.2) then
        if (alpha(k1:k1).ne.'+'.and.alpha(k1:k1).ne.'-') nerr=1
        if ((mexp+mdexp).eq.0) then
           nerr=1
        else if ((mexp+mdexp).gt.0) then
           if (alpha(kexp+1:kexp+1).ne.'+') then
             if (alpha(kexp+1:kexp+1).ne.'-') nerr=1
           end if
        end if
      end if
c
c------Step#1: Resolve Exponent First----------------------------------
      jexp=0
      if((mexp+mdexp).eq.0) goto 420
c
      L2=k2
      L1=kexp+1
      if (alpha(kexp+1:kexp+1).eq.'-') L1=kexp+2
      if (alpha(kexp+1:kexp+1).eq.'+') L1=kexp+2
c
      nbase=1
      L=L2+1
 400  L=L-1
      I=ichar(alpha(L:L))-48
      jexp=jexp+I*nbase
      nbase=nbase*10
      if(L.gt.L1) goto 400
c
c-0----Check size of exponent
      if(jexp.gt.maxe) then
       nerr=4
       goto 5000
      end if
c
      if(alpha(kexp+1:kexp+1).eq.'-') jexp=-jexp
 420  continue
c
c------Step#2: Resolve the Integer part
      jint=0
      if (alpha(k1:k1).eq.'.') goto 520
      L1=k1
      if(alpha(k1:k1).eq.'-'.or.alpha(k1:k1).eq.'+') L1=k1+1
c
      if(mdec.gt.0) then
        L2=kdec-1
      else
        L2=k2
        if((mexp+mdexp).eq.1) L2=kexp-1
      end if
c
      nbase=1
      L=L2+1
c     enforce ANSI max for signed integer
c     (exact is 2,147,438,647 here use 2,100,000,000 to allow for
c      compiler variability)
      if(mdec.eq.0)then
         if(L2.gt.10)then
            nerr=5
            goto 5000
         endif
         if(L2.eq.10)then
            if((ichar(alpha(k1:k1))-48).gt.2)then
               nerr=5
               goto 5000
            endif
            if((ichar(alpha(k1:k1))-48).eq.2)then
               if((ichar(alpha(k1+1:k1+1))-48).gt.1)then
                 nerr=5
                 goto 5000
               endif
               if(((ichar(alpha(k1+1:k1+1))-48).eq.1)
     ;               .and.(ichar(alpha(k1+2:k1+2))-48).gt.0) then
                     nerr=5
                     goto 5000
               endif
            endif
         endif
      endif
c
 500  L=L-1
      I=ichar(alpha(L:L))-48
      jint=jint+I*nbase
      nbase=nbase*10
      if(L.gt.L1) goto 500
      if(alpha(k1:k1).eq.'-') jint=-jint
 520  continue
c
c------Step#3: Resolve the decimal part
      jdec=0
      if(mdec.eq.0) goto 620
      L1=kdec+1
      L2=k2
c      CALL MESBKK
c          WRITE(6,*)'In CHK3NKK Setp3 L1, L2= ', L1, l2
c      CALL MESEKK
      if((mexp+mdexp).gt.0) L2=kexp-1
      if(L2.lt.L1) goto 620
c
      jj=0
      nbase=1
      L=L2+1
 600  L=L-1
      jj=jj+1
      I=ichar(alpha(L:L))-48
      jdec=jdec+I*nbase
      nbase=nbase*10
      if(L.gt.L1) goto 600
c      rdec=dble(jdec)*(10.d00**(-jj))
       rdec = float(jdec)*(10.0**(-jj))
 620  continue
c
c------Final Step: Construct the Number--------------------------------
      if(npreset.eq.2)then
         if(mdec.eq.0) then
c
c--------Number is integer
          ntype=1
          vreal=0
          kint=jint*(10.0**jexp)
         else
c
c--------Number is real
          ntype=0
          kint=0
          if((jint.gt.0).or.((jint.eq.0).and.alpha(k1:k1).ne.'-')) then
c            vreal=(dble(jint)+rdec)*(10.0d00**jexp)
             vreal = (float(jint)+rdec)*(10.0**jexp)
          else
c            vreal=(dble(jint)-rdec)*(10.0d00**jexp)
             vreal = (float(jint)-rdec)*(10.0**jexp)
          endif
        endif
      elseif(npreset.eq.0)then
        ntype=0
        kint=0
        if((jint.gt.0).or.((jint.eq.0).and.alpha(k1:k1).ne.'-')) then
c          vreal=(dble(jint)+rdec)*(10.0d00**jexp)
           vreal = (float(jint)+rdec)*(10.0**jexp)
        else
c          vreal=(dble(jint)-rdec)*(10.0d00**jexp)
           vreal = (float(jint)-rdec)*(10.0**jexp)
        endif
      endif
      return
c   error handling
 5000 continue
c       if(nerr.eq.1)call warnzz('@','CH2NKK error:'//
c     ;                          ' alphanumeric characters in input@@')
c       if(nerr.eq.2)call errfzz('@','CH2NKK error:'//
c     ;                          ' multiple numbers in input@@')
c       if(nerr.eq.3)call errfzz('@','CH2NKK error:'//
c     ;                          ' null string in input@@')
c       if(nerr.eq.4)call errfzz('@','CH2NKK error:'//
c     ;                          ' exponent is too large @@')
c       if(nerr.eq.4)call errfzz('@','CH2NKK error:'//
c     ;                          ' exceeded ANSI signed integer@@')
c    finish up
      return
      end


c***********************************************************************
       SUBROUTINE DEODKK(JKT)
c
c      Displays the option chosen as the designator for exclusive
c      options group with pointer JKT.
c      Tcl/Tk version
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       CHARACTER*1 C,FORM(128)
       CHARACTER*1 FORMC(7),FORMI(5),FORMR(11)
       DATA FORMC /'(',4*' ','$',')'/
       DATA FORMI /'(',2*' ','$',')'/
       DATA FORMR /'(',8*' ','$',')'/
c-----------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.LE.5)  THEN
c        option is a group
           CALL DGHZZ(0,JKT)
           RETURN
       ENDIF
c    check for option
       IF (KT.NE.20) CALL ERRFZZ('@','DOPZZ error; not option.@@')
c    determine status of the option
       JKTP = ID(JKT+1)
       KTP = ID(JKTP)
       IF (KTP.EQ.3)  THEN
c            parent is exclusive option group
               NOPC = ID(JKTP+9)
               JKTC = ID(JKTP+9+NOPC)
               IF (JKT.EQ.JKTC) THEN
                       NOPC = 1
               ELSE
                       NOPC = 0
               ENDIF
       ELSEIF (KTP.EQ.4)  THEN
c            parent is inclusive option group
               IF (I.EQ.0)  CALL ERRFZZ('@','DOPZZ error; '//
     ;                          'improper designator.@@')
               NOPC = ID(JKTP+9+I)
       ELSE
c            error
               CALL ERRFZZ('@','DOPZZ error; improper parent.@@')
       ENDIF
c    get parameters
       JCS = ID(JKT+4)
       NCS = ID(JKT+5)
c    start format to add characters
       FORM(1) = '('
       FORM(2) = ''''
c            designator
       FORM(3) = ' '
       FORM(4) = ' '
       LS = 4
       NV = 0
       DO 49 L=1,NCS
           C = CD(JCS+L-1)
           IF (C.EQ.'#')  THEN
c                value encountered
                   NV = NV + 1
c                get pointer to value
                   JKTV = ID(JKT+6+NV)
                   IF (NOPC.EQ.0)  THEN
c                      option not chosen; load name
                           JCN = ID(JKTV+4)
                           NCN = ID(JKTV+5)
                           DO 9 N=1,NCN
                               LS = LS + 1
                               FORM(LS) = CD(JCN+N-1)
9                          CONTINUE
                   ELSE
c                        option chosen; end the format
                           LS = LS + 1
                           FORM(LS) = ''''
                           LS = LS + 1
                           FORM(LS) = '$'
                           LS = LS + 1
                           FORM(LS) = ')'
                           WRITE (KUOUT,FORM)
c                        load the value
                           KTV = ID(JKTV)
c                        branch on type
                           IF (KTV.EQ.11) THEN
c                                character variable
                                   JCS = ID(JKTV+4)
                                   NCS = ID(JKTV+5)
c                                construct the format
                                   CALL CFCZZ(NCS,FORMC(2))
c                                display
                                   WRITE (KUOUT,FORMC)
     ;                                (CD(LZ),LZ=JCS,JCS+NCS-1)
                           ELSEIF (KTV.EQ.12) THEN
c                                   integer variable
                                      IV = ID(JKTV+7)
c                                   construct the format
                                      CALL CFIZZ(IV,FORMI(2))
c                                   display
                                      WRITE (KUOUT,FORMI) IV
                           ELSEIF (KTV.EQ.13) THEN
c                                   real variable
                                      JRV = ID(JKTV+7)
                                      RV = RD(JRV)
c                                   construct the format
                                      CALL CFRZZ(RV,FORMR(2))
c                                   display
                                      WRITE (KUOUT,FORMR) RV
                           ELSEIF (KTV.EQ.14) THEN
c                                   file name variable
                                      JCV = ID(JKTV+4) + ID(JKTV+5)
                                      NCV = ID(JKTV+6)
c                                construct the format
                                   CALL CFCZZ(NCV,FORMC(2))
c                                display
                                   WRITE (KUOUT,FORMC)
     ;                                (CD(LZ),LZ=JCV,JCV+NCV-1)
                           ELSE
c                                error
                                   CALL ERRFZZ('@','DOPZZ error;'//
     ;                                         ' not value.@@')
                           ENDIF
c                        start a continuation character string
                           FORM(1) = '('
                           FORM(2) = ''''
                           LS = 2
                   ENDIF
           ELSE
c                load the character
                   LS = LS + 1
                   FORM(LS) = C
           ENDIF
49     CONTINUE
c
       LS = LS + 1
       FORM(LS) = ' '
       LS = LS + 1
       FORM(LS) = ''''
       LS = LS + 1
       FORM(LS) = ')'
       WRITE (KUOUT,FORM)
c     flush pipe. (Closing call for Tcl/Tk in DEODKK)
       CALL FLUSH(KUOUT)
       RETURN
       END
c***********************************************************************
c
       FUNCTION JHPKK(JKTP,NHP)
c
c      Returns ID index of the pointer to the NCPth child of the parent
c      having pointer JKTP, 0 if the child does not exist.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c    get parent type
       KTP = ID(JKTP)
c    branch by parent
       IF ((KTP.LE.5).OR.(JKTP.EQ.ID(10)))  THEN
c            group
               NHO = 1
               JHPKK = JKTP + 5
       ELSEIF (KTP.EQ.6)  THEN
c            help box
               NHO = ID(JKTP+5)
               JHPKK = JKTP + 5 + NHP
       ELSEIF (KTP.GT.6)  THEN
c            helpless item
               NHO   = 0
               JHPKK = 0
       ELSE
               JHPKK = 0
       ENDIF
       IF ((NHP.LT.1).OR.(NHP.GT.NHO)) JHPKK = 0
       RETURN
       END
c***********************************************************************
c
c***********************************************************************
       SUBROUTINE DBTNKK(IX)
c    Constructs action button in GUI
c-----------------------------------------------------------------------
c    Data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flshing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c  Tcl/Tk calls
       CHARACTER*11 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
       DATA LEADER,ENDER /'butn_add { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION    NCHDIS(NCMAXZ)
       DIMENSION    I2JKT(NIMAXZ)
c----------------------------------------------------------------------
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c----------------------------------------------------------------------
c   initialize parameters for action items
             I = (IX-1)*3
             IGI  = ID(11+I)
             JCS  = ID(11+I+1)
             NCS  = ID(11+I+2)
c          initialize Tcl/Tk routine
              WRITE(KUOUT,10)LEADER
c             sent title for button
                WRITE(KUOUT,12)IBEG
                     WRITE(KUOUT,*)(CD(J+JCS-1),J=1,NCS)
                WRITE(KUOUT,12)IEND
c             sent action item number
                WRITE(KUOUT,12)IBEG
                  WRITE(KUOUT,11)IX
                WRITE(KUOUT,12)IEND
c           close Tcl/Tk call
              WRITE(KUOUT,10)ENDER
              CALL FLUSH(KUOUT)
c  formats
10     FORMAT(A15)
11     FORMAT(I3)
12     FORMAT(A2)
c  finish up
       RETURN
       END
c**********************************************************************

