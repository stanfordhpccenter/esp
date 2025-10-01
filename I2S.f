c**********************************************************************
c
       SUBROUTINE   DTBHKK(JKT)
c
c      Construct table header and columns headers for table with
c      pointer JKT.
c      Tcl/TK version
c----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       PARAMETER   (ITBLNHL = 5)
       PARAMETER   (ITBLNCL = 100)
c----------------------------------------------------------------------
       COMMON   /TBLINF/ NJSC(ITBLNHL,ITBLNCL), NJCC(ITBLNHL,ITBLNCL)
c----------------------------------------------------------------------
       LOGICAL  OUT
c----------------------------------------------------------------------
c    maximum table width
       PARAMETER (NWMAX=78)
       CHARACTER*1 ESC,C,D(NWMAX)
c----------------------------------------------------------------------
c    get parameters (based on WCR code of 04/01/00)
       NRW = ID(JKT+9)
       NFC = ID(JKT+10)
       NVC = ID(JKT+11)
       JRV = ID(JKT+12+NFC+2*NVC)
c    get ESC
       ESC = CD(ID(JKT+6)+ID(JKT+7))
c    get index of table column headers string
       JCCHS = ID(JKT+6) + ID(JKT+7) + 1
c
c *  construct column headers line by line
c
c    initialize display line number
       LND = 1
c
c *  loop point for scan of column headers string for the current
c    display line
c
c    initialize index at start of column headers string
2      JC = JCCHS
c    assume no data for this line
       OUT = .FALSE.
c    load blanks to allow space for row index
       DO 3 LD=1,3
           D(LD) = ' '
3      CONTINUE
       LD = 3
c    get data for each column
       DO 29 J=1,NFC+NVC
c        get width of column J
           NCWJ = ID(JKT+11+J)
c        leave 2 characters minimum space between columns
           NDISP = NCWJ - 2
c        total table width after this column
           LDTOT = LD + NCWJ
c        check display limit
           IF (LDTOT.GT.NWMAX)  CALL ERRFZZ('@','DTBHZZ error; '//
     ;                             'column header to long.@@')
c        set to scan line 1 in this column header
           LNX = 1
c        postion of first character in this line for this column
4          JC1 = JC
c
c        initialize last blank for full line
           JCLB = JC1 + NDISP - 1
           LELB = NDISP
c        find ESC
           DO 9 LX=1,NDISP+1
               C = CD(JC)
c            look for mandatory line ender
               IF (C.EQ.ESC)  THEN
                   LE = LX
                   GOTO 10
               ENDIF
c            check for line overflow
               IF (LX.GT.NDISP)    THEN
c                see if lines break at blank
                   IF (C.EQ.' ')  THEN
                       LE = LX
                       GOTO 10
                   ENDIF
c                end line at last blank (or full line if no blanks)
                   JC = JCLB
                   LE = LELB
                   GOTO 10
               ENDIF
c            check for update of last blank
               IF (C.EQ.' ') THEN
                       JCLB = JC
                       LELB = LX
               ELSE
c                    indicate characters on the line
                       IF (LNX.EQ.LND) OUT = .TRUE.
               ENDIF
c            set to look at next character
               JC = JC + 1
9          CONTINUE
c        error
           CALL ERRFZZ('@','DTBHZZ error; column head '//
     ;                 'too long or not ESC terminated.@@')
c        line end found; check for display
10         IF (LNX.EQ.LND)  THEN
c            get count of characters to display
               NCDJ = LE - 1
               NJSC(LNX,J) = JC1
               NJCC(LNX,J) = NCDJ
c            determine number of leading and trailing blanks
               NLB = (NCWJ - NDISP)/2
               NTB = NCWJ - NLB - NCDJ
c            load the leading blanks
               DO 13 L=1,NLB
                   LD = LD + 1
                   D(LD) = ' '
13             CONTINUE
c            load the text
               JX = JC1
               DO 15 L=1,NCDJ
                   LD = LD + 1
                   D(LD) = CD(JX)
                   JX = JX + 1
15             CONTINUE
c            load the trailing blanks
               DO 17 L=1,NTB
                   LD = LD + 1
                   D(LD) = ' '
17             CONTINUE
           ENDIF
c        advance to next character
           JC = JC + 1
c        check for end of column data
           IF (CD(JC).EQ.ESC)  THEN
c            this column's data exhausted
               IF (LNX.LT.LND) THEN
c                current line absent this column; load blanks
                   DO 27 L=1,NCWJ
                       LD = LD + 1
                       D(LD) = ' '
27                 CONTINUE
               ENDIF
c            advance to next character
               JC = JC + 1
               GOTO 29
           ENDIF
c        set to scan next column header line
           LNX = LNX + 1
c        go scan next line of this column header data
           GOTO 4
c        column complete

29     CONTINUE

c
c    all columns done for this line
       IF (LND.EQ.1)  THEN
c        draw top line
c           WRITE (*,32) ('_',L=1,LDTOT)
32         FORMAT (5X,75A1)
       ENDIF

c    check for display
       IF (OUT) THEN
c        set for next column header line
           LND = LND + 1
c        go work on next line
           GOTO 2
       ENDIF
c    completed all headers
c           CALL DGDKK(JKT)
       RETURN
       END
c**********************************************************************

c**********************************************************************
c
       SUBROUTINE   DTABKK(JKT)
c
c      Display table having pointer JKT with line index on left.
c-----------------------------------------------------------------------
       INCLUDE 'INTWRKZZ.H'
c    setup data storage
c       CHARACTER*1  CD
       CHARACTER*12 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
c    i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       DATA LEADER,ENDER /'tbox_vadd { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
       DIMENSION I2JKT(NIMAXZ)
       COMMON /TBLKK/  I2JKT, ITOT, NTHLP
       EQUIVALENCE (RD(1),RU)
c-----------------------------------------------------------------------
c    get parameters
       KS = ID(JKT+3)
       NRW = ID(JKT+9)
       NFC = ID(JKT+10)
       NVC = ID(JKT+11)
       JRV = ID(JKT+12+NFC+2*NVC)
c       JRV = ID(JKT+12+NFC+NVC)
       NTC = NFC + NVC
c    get display width
       LDTOT = 3
c    step through cols
       DO 9 J=1,NTC
c        get width of column J
           NCWJ = ID(JKT+11+J)
           LDTOT = LDTOT + NCWJ
9      CONTINUE
c    write the data
c      step through rows
       DO 49 I=1,NRW
c        load index and trailing space
           LD = 5
           DO 39 J=1,NTC
c    initialize TCL call for each table cell
               WRITE(KUOUT,21)LEADER
               WRITE(KUOUT,47) IBEG
                   WRITE(KUOUT,48)JKT2I(JKT)
               WRITE(KUOUT,47) IEND
c            get width of column J
               NCWJ = ID(JKT+11+J)
c            load the number
               JV = JRV + I - 1 + NRW*(J-1)
c            write row and column indices
               WRITE(KUOUT,47) IBEG
                   WRITE(KUOUT,48)I
               WRITE(KUOUT,47) IEND
               WRITE(KUOUT,47) IBEG
                   WRITE(KUOUT,48)J
               WRITE(KUOUT,47) IEND
               IF (RD(JV).EQ.RU)  THEN
c            check data
c                    unset; write blanks
                       WRITE(KUOUT,47) IBEG
                           WRITE (KUOUT,22) ' '
21                         FORMAT (A15)
22                         FORMAT (A)
                       WRITE(KUOUT,47) IEND
                   ELSE
c                    set; write number
                       WRITE(KUOUT,47) IBEG
                          WRITE (KUOUT,26) RD(JV)
                       WRITE(KUOUT,47) IEND
26                     FORMAT (1X,1PE14.6)
                       LD = LD + 14
c                complete TCL call for current cell
                   ENDIF
                   WRITE(KUOUT,21)ENDER
                   CALL FLUSH(KUOUT)
39             CONTINUE
c        end the line
47         FORMAT (A3)
48         FORMAT (I3)
49         CONTINUE
c           CALL MESBKK
c                WRITE(*,*)'Table:', ID(JKT+4),ID(ID(JKT+4)+3)
c           CALL MESEKK

           CALL DGDKK(ID(JKT+4))
       RETURN
       END
c**********************************************************************


c**********************************************************************
c
       SUBROUTINE CTABKK(JKT)
c
c      Configures table having pointer JKT.
c-----------------------------------------------------------------------
c    setup data storage

       INCLUDE 'INTWRKZZ.H'
       CHARACTER*12 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c----------------------------------------------------------------------
       DATA LEADER,ENDER /'tbox_conf { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
       DIMENSION I2JKT(NIMAXZ)
       COMMON /TBLKK/  I2JKT, ITOT, NTHLP
       EQUIVALENCE (RD(1),RU)
c-----------------------------------------------------------------------
c    make sure is a table
       IF(ID(JKT).NE.5) GOTO 90
c    get parameters
       KS = ID(JKT+3)
       NRW = ID(JKT+9)
       NFC = ID(JKT+10)
       NVC = ID(JKT+11)
       JRV = ID(JKT+12+NFC+2*NVC)
       NTC = NFC + NVC
c    write the data
c      step through rows
           DO 39 J=1,NTC
c    initialize TCL call for each table cell
               WRITE(KUOUT,46)LEADER
               WRITE(KUOUT,47) IBEG
                   WRITE(KUOUT,48)JKT2I(JKT)
               WRITE(KUOUT,47) IEND
c            get width of column J
               NCWJ = ID(JKT+11+J)
c            get restriction of column J
c               IF (J.GT.NFC) THEN
                 NCRJ = ID(JKT+11+NTC+J -NFC )
                 CMINJ = RD(JRV + NRW*NTC - 1 + J - NFC)
                 CMAXJ = RD(JRV + NRW*NTC - 1 + NVC + J -NFC )
c               ELSE
c                 NCRJ = 0
c                 CMINJ = RD(JRV + NRW*NFC - 1 + J)
c                 CMAXJ = RD(JRV + NVC + NRW*NFC - 1 + J)
c               ENDIF
c            write column index, width and restriction
               WRITE(KUOUT,47) IBEG
                   WRITE(KUOUT,48)J
               WRITE(KUOUT,47) IEND
               WRITE(KUOUT,47) IBEG
                   WRITE(KUOUT,48)NCWJ
               WRITE(KUOUT,47) IEND
               WRITE(KUOUT,47) IBEG
                   WRITE(KUOUT,48)NCRJ
               WRITE(KUOUT,47) IEND
               WRITE(KUOUT,47) IBEG
                   WRITE(KUOUT,*)CMINJ
               WRITE(KUOUT,47) IEND
               WRITE(KUOUT,47) IBEG
                   WRITE(KUOUT,*)CMAXJ
               WRITE(KUOUT,47) IEND
c           close Tcl/Tk call
               WRITE(KUOUT,46)ENDER
               CALL FLUSH(KUOUT)
39         CONTINUE
c        formats
46         FORMAT  (A15)
47         FORMAT (A3)
48         FORMAT (I3)
c     finish up
       RETURN
90     CALL ERRFZZ('@','CTABKK error; item not a table.@@')
       RETURN
       END
c**********************************************************************

c***********************************************************************
c
       SUBROUTINE GCKK(HELP)
c
c      Get changes.
c      Tcl/Tk version
c-----------------------------------------------------------------------
c      This routine enters a loop listening to the GUI for incoming
c      requests to process changes.
c      Items with special treatment: Designators and Actions and Help.
c      Differences from command-line version:
c        JKTX = 0 in all cases since the user is not restricted to
c        complete a selected group option at selection time.
c        Help is processed locally. A routine for processing Help groups
c        is available, but not active.
c        Designators and actions
c
c-----------------------------------------------------------------------
c    applications-specific help routine
       EXTERNAL    HELP
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       CHARACTER*80 ALPHA
       LOGICAL OK
c-----------------------------------------------------------------------
c    GUI index to JKT index lookup array
       DIMENSION I2JKT(NIMAXZ)
c-----------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c-----------------------------------------------------------------------
       COMMON /TBLKK/  I2JKT, ITOT, NTHLP
       EQUIVALENCE     (ID(10),JKTGOD)
c-----------------------------------------------------------------------
c    get response from GUI.
c    response can be one of the following:
c         integer index for item
c         string "DESIGN" for designation processing
c         string "action" for action processing
c --loop point----------------------------------------------------------
10     READ(KUIN,11,ERR=90) ALPHA
c       WRITE(55,*)ALPHA
c                 CALL MESBKK
c                        WRITE(*,*)'On entering GCKK APLHA=',ALPHA
c                 CALL MESEKK
11     FORMAT(A)
       IF(ALPHA.EQ.'DESIGN')THEN
c
c   the request is for designation
           READ(KUIN,11,ERR=90)ALPHA
           CALL CH2NKK(alpha,ntype,kint,vreal,nerr,2)
           IF((NERR.NE.0).OR.(NTYPE.NE.1))GOTO 90
           JKT = I2JKT(KINT)
           CALL GGDKK(JKT)

c SCK 07/06/00
c           CALL CHKGKK(OK)
            CALL CHKLKK(OK)
           CALL DISACTS
c                          CALL MESBKK
c                                 WRITE(*,*)'CALLING CKGDKK in GCKK --1'
c                          CALL MESEKK
           CALL CKGDKK(OK)
c                          CALL MESBKK
c                                 WRITE(*,*)'Result --1 OK= ',OK
c                          CALL MESEKK
           IF (OK) THEN
c                          CALL MESBKK
c                                 WRITE(*,*)'IN GCKK JKTGOD+4 is ',ID(JKTGOD+4),ID(ID(JKTGOD+4)+3)
c                          CALL MESEKK

                                  IF (  (ID(JKTGOD+4).GT.0).AND.(ID(ID(JKTGOD+4)+3).EQ.0) ) THEN
                                  ELSE
                  CALL ENACTS
                                  ENDIF
           ENDIF
c           IF (OK) RETURN
           GOTO 10
       ELSEIF(ALPHA.EQ.'CHKDES')THEN
           CALL CKGDKK(OK)
           GOTO 10
       ELSEIF(ALPHA.EQ.'action')THEN
c   the request is for action
           RETURN
       ENDIF
c   the request is integer for item index IGI:
c   translate string to integer; lookup corresponding JKT
       CALL CH2NKK(alpha,ntype,kint,vreal,nerr,2)
       IF((NERR.NE.0).OR.(NTYPE.NE.1))GOTO 90
       IGI = KINT
       JKT = I2JKT(IGI)
       JKTX = JKT
c    actions at the current level
15     KT = ID(JKT)
       IF (KT.EQ.1)  THEN
c            items group
c               CALL SFIGZZ(JKT,JKTX)
           ELSEIF (KT.EQ.2) THEN
c            values group
c                CALL GCVGZZ(JKT,JKTX)
           ELSEIF (KT.EQ.3) THEN
c            exclusive options group
               CALL GCEOKK(JKT,JKTX)
              JKTX=0
           ELSEIF (KT.EQ.4) THEN
c            inclusive options group
               CALL GCIOKK(JKT,JKTX)
               JKTX=0
           ELSEIF (KT.EQ.5) THEN
c            table
               CALL GTBCKK(JKT,JKTX)
               JKTX=0
           ELSEIF (KT.EQ.6)  THEN
c            help options group (not used)
c               CALL GHOPKK(JKT,JKTX,HELP)
               JKTX = 0
           ELSEIF (KT.EQ.10)  THEN
c            group designator  (not used)
               CALL GGDKK(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.11)  THEN
c            character value
               CALL GCVKK(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.12)  THEN
c            integer value
               CALL GIVKK(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.13)  THEN
c            real value
               CALL GRVKK(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.14)  THEN
c            file name
               CALL GFNKK(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.21)  THEN
c            help option (help request process at this level)
               KHELP = ID(JKT+5)
c               CALL MESBKK
c                   write(*,*)'calling help with khelp=',KHELP
c               CALL MESEKK
               CALL HELP(KHELP)
               JKTX = 0
           ELSE
c            error
               CALL ERRFZZ('@','GCKK error; improper object.@@')
       ENDIF
c
c    check for redirection
       IF (JKTX.NE.0) THEN
c        go to specified object
           JKT = JKTX
           GOTO 15
           ENDIF
c
c    quit if at top level
       IF (JKT.EQ.JKTGOD) THEN
        OK = .FALSE.
c  SCK: 07/06/00
c        CALL CHKGZZ(OK)
c       CALL MESBKK
c                         write(*,*)'IN GCKK CALIING CKGDKK --2'
c                 CALL MESEKK
        CALL CHKLKK(OK)
        CALL CKGDKK(OK)
c       CALL MESBKK
c                         write(*,*)'Result--2 is',OK
c                 CALL MESEKK

        IF (OK) CALL ENACTS
c
        GOTO 10
       ENDIF
c
c    go to parent
       JKT = ID(JKT+1)
       GOTO 10
c
90     CALL ERRFZZ ('@','GCKK error: Expecting integer, try again.@@')
       END
c***********************************************************************

c***********************************************************************
c
       SUBROUTINE GTBCKK(JKT,JKTX)
c
c      Gets data for cell I,J of table having pointer JKT.
c      Tcl/Tk version.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c    define i/o units (explicit decleration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
c-----------------------------------------------------------------------
       PARAMETER (NVMAX=10)
       DIMENSION V(NVMAX)
       CHARACTER FORM(6)
c       CHARACTER*1  CD
       CHARACTER*80 ALPHA
c-----------------------------------------------------------------------
       LOGICAL OK
       DATA FORM /'(',4*' ',')'/
c-----------------------------------------------------------------------
c    get parameters
       JKTP = ID(JKT+1)
       JCH  = ID(JKT+6)
       NCH  = ID(JKT+7)
       NRW  = ID(JKT+9)
       NFC  = ID(JKT+10)
       NVC  = ID(JKT+11)
       JRV  = ID(JKT+12+NFC+2*NVC)
c    check column count
       IF (NVC.GT.NVMAX)  THEN
c        error
           CALL ERRFZZ('@','GTBCKK error; too many table columns.@@')
       ENDIF
c    get the data
       READ(*,*,ERR=20) IROW,JCOL
       JCOL = JCOL - NFC
c    read string containing value
       READ(*,5,ERR=22) ALPHA
5      FORMAT(A50)
c       CALL MESBKK
c             WRITE(6,*)'In GTBCKK ALPHA =', ALPHA
c       CALL MESEKK
c  SCK: 12/18/00
c       IF(ALPHA.EQ.'UNSET')THEN
c           CALL POPSKK
c              WRITE(KUOUT,*)'OK'
c           CALL POPFKK
c           JV = JRV - 1 + NFC*NRW + IROW + NRW*(JCOL-1)
c           RD(JV) = RD(1)
c           CALL DISGDKK(JKT)
c           CALL DISGDKK(JKTP)
c           CALL SGDUZZ(JKT)
c           RETURN
c       ENDIF
c     convert string to real
       CALL CH2NKK(alpha,ntype,kint,vreal,nerr,0)
c        CALL MESBKK
c             WRITE(6,*)'In GTBCKK alpha,ntype,kint,vreal,nerr =',
c     ;                  alpha,ntype,kint,vreal,nerr
c       CALL MESEKK

       IF((NERR.NE.0).OR.(NTYPE.NE.0))GOTO 22
c     no error; grab value
       VALUE=VREAL
       IF (VALUE.EQ.RD(1))THEN
c    set parent unset
           ID(JKT+3) = 0
           JV = JRV - 1 + NFC*NRW + IROW + NRW*(JCOL-1)
           RD(JV) = VALUE
           RETURN
       ENDIF
c    check the data for column conditions
           KR = ID(JKT+11+NFC+NVC+JCOL)
c        get restriction bits
           KR8 = KR/8
           KR4 = (KR - 8*KR8)/4
           KR2 = (KR - 8*KR4 - 4*KR4)/2
           KR1 = KR - 8*KR4 - 4*KR4 - 2*KR2
c        check restrictions
           IF (KR8.GT.0)  THEN
c            check maximum
               VMAX = RD(JRV-1+(NFC+NVC)*NRW+NVC+JCOL)
               IF (VALUE.GT.VMAX)  THEN
                   CALL POPSKK
                     WRITE (KUOUT,11) JCOL+NFC
                     CALL  CFCZZ(NCH+2,FORM(2))
                     WRITE(KUOUT,*)'    of the ',(CD(JJ+JCH-1),
     ;                             JJ=1,NCH)
                     WRITE (KUOUT,9) VMAX
11                   FORMAT (' The maximum value in column ',I2)
9                    FORMAT (' is ', 1PE14.6)
                     WRITE (KUOUT,30)
                   CALL POPFKK
                   GOTO 10
               ENDIF
           ENDIF
           IF (KR4.GT.0) THEN
c           check minimum
               VMIN = RD(JRV-1+(NFC+NVC)*NRW+JCOL)
               IF (VALUE.LT.VMIN)  THEN
                  CALL POPSKK
                   WRITE (KUOUT,12) JCOL+NFC
                   WRITE(KUOUT,*)'    of the ',(CD(JJ+JCH-1),JJ=1,NCH)
                   WRITE (KUOUT,9)ALPHA,VALUE, VMIN
12                 FORMAT (' The minimum value in column ',I2)
                   WRITE (KUOUT,30)
                   CALL POPFKK
                   GOTO 10
               ENDIF
           ENDIF
           IF (KR1.GT.0) THEN
c           sign restricted
               IF (KR2.GT.0) THEN
c                    negative definite
                       IF (VALUE.GE.0)  THEN
                         CALL POPSKK
                           WRITE (KUOUT,13)  JCOL+NFC
13                         FORMAT(' The value in column ',I2)
                           WRITE(KUOUT,*)'    of the ',
     ;                                   (CD(JJ+JCH-1),JJ=1,NCH)
                           WRITE(KUOUT,*)' must be < 0'
                           WRITE (KUOUT,30)
                         CALL POPFKK
                         GOTO 10
                       ENDIF
               ELSE
c                    positive definite
                       IF (VALUE.LE.0)  THEN
                         CALL POPSKK
                           WRITE (*,14) JCOL+NFC
14                         FORMAT(' The value in column ',I2)
                           WRITE(KUOUT,*)'    of the ',
     ;                                   (CD(JJ+JCH-1),JJ=1,NCH)
                           WRITE(KUOUT,*)' must be > 0'
                           WRITE (KUOUT,30)
                         CALL POPFKK
                         GOTO 10
                       ENDIF
               ENDIF
           ENDIF
19         CONTINUE
c      value acceptable; inform GUI
       CALL POPSKK
         WRITE(KUOUT,*)'OK'
       CALL POPFKK
       JV = JRV - 1 + NFC*NRW + IROW + NRW*(JCOL-1)
       RD(JV) = VALUE
29     CONTINUE
c     unset group designation
c       CALL DISGDKK(JKT)
c    set parent unset before checking all siblings
           ID(JKT+3) =0
           ID(JKTP+3) = 0
           CALL SGDUZZ(JKT)
c     check if siblings hold values
c     and enable designations appropriately
c       CALL MESBKK
c         write(*,*)'In GTBCKK calling CHKTKK'
c       CALL MESEKK
       CALL CHKTKK(JKT,OK)
c       CALL MESBKK
c         write(*,*)'CHKTKK result is: ',OK
c       CALL MESEKK

       IF (OK) THEN
c       CALL MESBKK
c         write(*,*)'In GTBCKK calling SAKSKK due to OK'
c       CALL MESEKK
           CALL SAKSKK(JKT)
       ENDIF
       RETURN
c
c    entry error
20     CALL POPSKK
         WRITE (KUOUT,*) 'Illegal table cell specification'
         WRITE (KUOUT,*)'    in the ',(CD(JJ+JCH-1),JJ=1,NCH)

       CALL POPFKK
       GOTO 10
22     CALL POPSKK
         WRITE (KUOUT,*)'    Error in the ',(CD(JJ+JCH-1),JJ=1,NCH)
         WRITE (KUOUT,*) 'Expecting real number.'
         IF(NERR.EQ.1) WRITE(KUOUT,*)
     ;      'Aplhanumeric characters not allowed.'
         IF(NERR.EQ.2) WRITE(KUOUT,*)
     ;      'Blanks not allowed.'
         WRITE (KUOUT,30)
       CALL POPFKK
       GOTO 10
30     FORMAT (' Please redo the cell highlighted in red.')
       GOTO 10
10     RETURN
       END
c***********************************************************************

c***********************************************************************
c
       SUBROUTINE SAKSKK(JKT)
c
c      Routine enable designation for group with pointer JKT.
c      Actions taken herein:
c          1. Register item status as set.
c          2. Turn designation into editable green.
c          3. Call recursive check of groups antecedents.
c      Tcl/Tk version
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c
       CHARACTER*11 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)

       DIMENSION I2JKT(NIMAXZ)
c-----------------------------------------------------------------------
c    define i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/   KUOUT, KUIN
       COMMON /TBLKK/  I2JKT, ITOT, NTHLP

       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
c
       DATA LEADER,ENDER /'g_enable { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/

c-----------------------------------------------------------------------
       LOGICAL OK
c-----------------------------------------------------------------------
c    set current item as set
       ID(JKT+3) = 2
c     enable designation
       WRITE(KUOUT,5)LEADER
         WRITE(KUOUT,5)IBEG
          WRITE(KUOUT,6)JKT2I(JKT)
         WRITE(KUOUT,5)IEND
      WRITE(KUOUT,5)ENDER
      CALL FLUSH(KUOUT)
5     FORMAT(A15)
6     FORMAT(I3)
c
c       CALL MESBKK
c            write(*,*)'Calling CHKSKK in SAKSKK'
c       CALL MESEKK
c     recursively check antecedents
      CALL CHKSKK(JKT,OK)
      RETURN
      END


c***********************************************************************
c
       SUBROUTINE CHKTKK(JKT,OK)
c
c      Checks the table having pointer JKT. Returns OK = .TRUE.
c      if all values are loaded, otherwise .FALSE.
c      Tcl/Tk version
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'

       EQUIVALENCE (RD(1),RU)
c-----------------------------------------------------------------------
       LOGICAL OK
c-----------------------------------------------------------------------
c    get parameters
       NRW = ID(JKT+9)
       NFC = ID(JKT+10)
       NVC = ID(JKT+11)
       JKTA = ID(JKT+8)
       JRV = ID(JKT+12+NFC+2*NVC)
c    check the non-frozen matrix values
       DO 9 I=1,NRW
           DO 7 J=1,NVC
               LAIJ = JRV + I - 1 + (J + NFC - 1)*NRW
c            see if value is unset
               IF (RD(LAIJ).EQ.RU)  THEN
c                value unset; set table unset
                   ID(JKT+3) = 0
                   OK =.FALSE.
                   RETURN
               ENDIF
7          CONTINUE
9      CONTINUE
c     check if attached object is set
       IF ((JKTA.LE.0).OR.(ID(JKTA+3).GT.0)) THEN
         OK = .TRUE.
       ELSE
         OK =.FALSE.
       ENDIF
       RETURN
       END
c***********************************************************************


c***********************************************************************
c
       SUBROUTINE CHKSKK(JKT,OK)
c
c      Used by Tcl/Tk version.
c      Called when item with pointer JKT is set to recursively check
c      parents to see if they should now be updated to set status.
c      Values in unspecified options need not be set.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
c-----------------------------------------------------------------------
       LOGICAL OK
c-----------------------------------------------------------------------
       DIMENSION I2JKT(NIMAXZ)
       COMMON   /TBLKK/  I2JKT, NTOBJ, NTHLP
c    get type
       JKTX = JKT
c       CALL MESBKK
c             WRITE(6,*)'ENTERING CHKSKK for',JKT2I(JKT)
c       CALL MESEKK
c    loop point
10     JKTP = ID(JKTX+1)
       JKTDDD = 0
       IF(JKTX.NE.JKTGOD)THEN
c       CALL MESBKK
c             WRITE(6,*)'looping in CHKSKK for',JKT2I(JKTP)
c       CALL MESEKK
       ENDIF

c    handle the GOD case
      IF (JKT2I(JKTX).EQ.1)THEN
c SCK: 07/06/00
c         CALL CHKGKK(OK)

c         global completeness check
          CALL CHKSZZ(OK)
c               CALL MESBKK
c                    WRITE(*,*)'OK After calling CHKSZZ',OK
c              CALL MESEKK
c         global designation check
          CALL CKGDKK(OK)
c               CALL MESBKK
c                    WRITE(*,*)'OK After calling CKGDKK',OK
c               CALL MESEKK
c         if both data and designators are set
c         then if not EOG enable green designation
          IF (OK) THEN
             IF (ID(JKTX).NE.3)THEN
c                CALL MESBKK
c                     WRITE(*,*)'JKTX NE 3',JKT2I(JKTX)
c                CALL MESEKK
                CALL ENGDKK(JKTX,JKTX)
             ELSE
c               if GOD is EOG then its designation should
c               be taken care of. Only need to return to
c               normal state (no green).
c                CALL MESBKK
c                     WRITE(*,*)'JKTX EQ 3',JKT2I(JKTX)
c                CALL MESEKK
               CALL DGDKK(JKTX+9)
               CALL DSEDBKK(JKTX)
               CALL ENACTS
             ENDIF
c     commented out by SCK 12/16/00
c     does not seem to do anything
c             CALL CKGDKK(OK)
          ELSE
c            either data or designation is unset
c    set parent unset
             ID(JKTX+3) = 0
             CALL DISACTS
          ENDIF
c          CALL MESBKK
c                WRITE(*,*)'Returing from CHKSKK --1'
c          CALL MESEKK
          RETURN
      ENDIF

c SCK: 07/05/00 - moved down to handle the GOD case
       KTP  = ID(JKTP)
c    if parent is exclusive options group
       IF  (KTP.EQ.3)  THEN
c         get selected option
           NOPI = ID(JKTP+9)
c        if nothing is selected, we have an error
c        since we came from set child
           IF (NOPI.EQ.IU)  GOTO 90
c        get option pointer
           JKTI = ID(JKTP+9+NOPI)
c        if option is not the child we came from
c        we have an error since we're processing
c        exlcusive options
           IF (JKTX.NE.JKTI) GOTO 90
c        if we've come this far the parent is set
c         exclusive options group does not have a
c         pre-installed designation button.
c         Create designation.
           IF((ID(JKTX).LE.5).AND.(ID(ID(JKTX+4)+3).LT.2))THEN
                             IF(ID(JKTX+3).EQ.0) THEN
                                   CALL DGDKK(JKTX)
                                   GOTO 90
                                        ENDIF
               CALL ENGDKK(JKTP,JKTP)
           ELSE
c              CALL MESBKK
c                   WRITE(6,*)ID(JKTX),ID(JKTX+4),ID(ID(JKTX+4)+3)
c              CALL MESEKK
           ENDIF
           ID(JKTP+3)=2
           CALL DGDKK(JKTX)
           JKTX = JKTP
           GOTO 10
c    parent is inclusive option
       ELSEIF (KTP.EQ.4)  THEN
c       get option count
           NOS = ID(JKTP+8)
c       find the option and check status
           DO 11 L=1,NOS
              JKTL = ID(JKTP+9+NOS+L)
              IF (JKTX.EQ.JKTL)  THEN
c       go no further if the option is selected
                 IF (ID(JKTP+9+L).NE.1)  THEN
                ENDIF
c                 IF (ID(JKTP+9+L).EQ.1)  THEN
c                   ID(JKTP+3)=2
c
c                enable designation button (pre-installed for inclusive)
                  IF( (ID(JKTX).LE.5).AND.(ID(ID(JKTX+4)+3).LT.2) )THEN
                                          CALL ENGDKK(JKTP,JKTX)
                  ELSEIF ( (ID(JKTX).LE.5).AND.(ID(JKTX+4).EQ.0) )THEN

                             CALL ENGDKK(JKTP,JKTP)
c                      CALL MESBKK
c                        WRITE(6,*)'Calling ENGDKK of IO in CHKSKK ',
c     ;                              JKTX,JKTP
c                      CALL MESEKK

                    ELSE
                      CALL ENGDKK(JKTP,ID(JKT+1))
                   ENDIF
                   JKTX = JKTP
                   GOTO 10
c                 ENDIF
              ENDIF
11         CONTINUE
       ELSEIF (KTP.EQ.5) THEN
            JKTA = ID(JKTP+8)
            IF (JKTX.NE.JKTA) THEN
             CALL ERRFZZ('@','CHKSKK for Table attached object.@@')
             GOTO 90
            ENDIF
c    set parent unset before checking all siblings
            ID(JKTP+3) = 0
            CALL CHKTKK(JKTP,OK)
            IF (OK) THEN
               CALL SAKSKK(JKTP)
            ENDIF
            RETURN
       ENDIF
c    parent has normal children; check each child for set
       NOCH = NCHILD(JKTP)
       OK = .TRUE.
12     DO I=1,NOCH
c         CALL MESBKK
c              WRITE(6,*)'Looping child',I,'of ',JKT2I(JKTP)
c         CALL MESEKK
          JKTC = JKTCZZ(JKTP,I)
          IF (JKTC.LT.0) THEN
            CALL ERRFZZ('@','CHKSKK error; undefined child.@@')
          ENDIF
c    go process if no children
         IF (JKTC.EQ.0)  THEN
           CALL ERRFZZ('@',
     ;                 'CHKSKK error: childess parent not expected@@')
         ENDIF
c    set for first child
         IF (ID(JKTC+3).EQ.0)THEN
           OK=.FALSE.
         ENDIF
         IF((ID(JKTC).LE.5).AND.(ID(ID(JKTC+4)+3).EQ.0))THEN
           IF ((ID(JKTC).NE.3).AND.(ID(JKTC+3).GT.0)) JKTDDD  = JKTC
           IF (ID(JKTC).EQ.3) THEN
              NOPI = ID(JKTC+9)
              IF (ID(JKTC+9+NOPI).GT.5) JKTDDD = JKTDDD
              IF (ID(ID(JKTC+9+NOPI)).LE.5) JKTDDD = JKTC
c             CALL MESBKK
c                 WRITE(*,*)'Exclusive child in CHKSKK ', JKT2I(JKTC),
c     ;                  ID(ID(JKTC+4)+3),ID(JKTC+3),NOPI,ID(JKTC+9+NOPI)
c            CALL MESEKK

           ENDIF
c           CALL MESBKK
c                WRITE(6,*)'Setting JKTDDD in CHKSKK=',JKT2I(JKTC),
c     ;           JKT2I(JKTP),JKT2I(JKT),ID(JKTP),ID(JKTC+4)
c           CALL MESEKK
         ENDIF
       ENDDO
c
c         write(*,*)'came this far'
         IF (OK) THEN
           ID(JKTP+3)=2
           IF(JKTDDD.EQ.0) JKTDDD = JKTP
           CALL ENGDKK(JKTP,JKTDDD)
c           CALL MESBKK
c                WRITE(6,*)'Calling ENGDKK in CHKSSKK for IGI=',
c     ;          JKT2I(JKTP),JKT2I(JKTDDD),JKT2I(JKT),ID(JKTP),ID(JKTP+4)
c           CALL MESEKK
           JKTX = JKTP
           GOTO 10
         ELSE
           ID(JKTP+3) = 0
c    set parent unset
           ID(JKTP+3) = 0
c 10/26/00
           CALL DISACTS
c           write(*,*)'hi aft'
           RETURN
         ENDIF
c    incomplete data
90     ID(JKTP+3) = 0
c       CALL WARNZZ('@',' Request denied: needed data are still '//
c     ;                        'unspecified!@@')
       OK = .FALSE.
       RETURN
       END
c***********************************************************************


c***********************************************************************
c
       SUBROUTINE ENGDKK(JKT,JKTX)
c
c      Enables the DESIGNATOR button in GUI for object with pointer JKT.
c        IMPORTANT: ID(JKT+4) = 0 means group is not designated.
c                   ID(ID(JKT+4)+3) = 0 group is designated, but
c                   currently its designator is unset.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'

c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c    IGI to JKT translation
       DIMENSION I2JKT(NIMAXZ)
       COMMON /TBLKK/  I2JKT, ITOT, NTHLP
       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
c-----------------------------------------------------------------------
c    Tcl/Tk calls
       CHARACTER*11 LEADER,LEADEM
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
       DATA LEADER,ENDER /'g_enable { ','} '/
       DATA LEADEM  /'g_enabla { '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
       LOGICAL OK
c-----------------------------------------------------------------------
c    set current item
c          CALL MESBKK
c             write(*,*)'entering ENGDKK with JKT, JKTX=', JKT, JKTX
c         CALL MESEKK
c     designation belongs to a E.O.G.
      IF(ID(JKT).EQ.3)THEN

         WRITE(KUOUT,5)LEADER
              WRITE(KUOUT,5)IBEG
                   WRITE(KUOUT,6)JKT2I(JKT)
              WRITE(KUOUT,5)IEND
              WRITE(KUOUT,5)IBEG
                   WRITE(KUOUT,6)JKT2I(JKTX)
              WRITE(KUOUT,5)IEND
          WRITE(KUOUT,5)ENDER
          CALL FLUSH(KUOUT)
      RETURN
      ENDIF
c    set current item
c    SCK: check if this needed here and if yes
c         why it does occur before E.O.G. ???
      ID(JKT+3) = 2
c     if the designation is for GOD and
c     set there's nothing to do
      IF((JKT.EQ.JKTGOD).AND.(ID(ID(JKT+4)+3).EQ.2)) RETURN
c     if the designation call is for GOD, but GOD has no
c     designation defined again nothing to do other than
c     enable action buttons.
      IF((JKT.EQ.JKTGOD).AND.(ID(JKT+4).EQ.0)) THEN
         CALL ENACTS
         RETURN
      ENDIF
c     if have reached this point it means the designator parent
c     is not an EOB; if it is the GOD it has to have a designator
c     defined that is pending (KS not 2).
      IF(JKT.EQ.JKTX)THEN
c        JKT=JKTX means ready to make green
         JKTD = ID(JKT+4)
c         CALL MESBKK
c             write(*,*)'calling DISACTS in ENGDKK'
c         CALL MESEKK
c        put warning on actions since we have to get new
c        designation for this group
         CALL DISACTS

         IF ((JKTD.GT.0)) THEN
c           do this if designator is defined
c  set as unset since we demand new designation
c  06/23/00
c        ID(JKTD+3)=0
c    enable designation button in GUI
             WRITE(KUOUT,5)LEADER
                 WRITE(KUOUT,5)IBEG
                     WRITE(KUOUT,6)JKT2I(JKT)
                WRITE(KUOUT,5)IEND
                WRITE(KUOUT,5)IBEG
                     WRITE(KUOUT,6)JKT2I(JKTX)
               WRITE(KUOUT,5)IEND
             WRITE(KUOUT,5)ENDER
             CALL FLUSH(KUOUT)
c      IF (JKT.NE.JKTGOD) CALL WRNDKK(ID(JKT+1))
c        CALL WRNDKK(JKT)
         ENDIF
      ELSE
c        this is the JKT .ne. JKTX case
c        means maroon designation and message
             ID(ID(JKT+4)+3)=0
             WRITE(KUOUT,5)LEADEM
                 WRITE(KUOUT,5)IBEG
                     WRITE(KUOUT,6)JKT2I(JKT)
                 WRITE(KUOUT,5)IEND
                 WRITE(KUOUT,5)IBEG
                     WRITE(KUOUT,6)JKT2I(JKTX)
                 WRITE(KUOUT,5)IEND
             WRITE(KUOUT,5)ENDER
             CALL FLUSH(KUOUT)

      ENDIF
c    formats
5     FORMAT(A15)
6     FORMAT(I3)
      RETURN
      END
c***********************************************************************


c***********************************************************************
c
       SUBROUTINE CHTABKK
c     Currently clears Tab (to be modified to change Tab)
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
c-----------------------------------------------------------------------
c    IGI to JKT translation
       DIMENSION I2JKT(NIMAXZ)
       COMMON /TBLKK/  I2JKT, ITOT, NTHLP
c-----------------------------------------------------------------------
c    Tcl/Tk calls
       CHARACTER*15 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
       DATA LEADER,ENDER /'action_manager ',' '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       LOGICAL OK
c-----------------------------------------------------------------------
          WRITE(KUOUT,5)LEADER
          CALL FLUSH(KUOUT)
c    formats
5      FORMAT(A17)
6      FORMAT(I3)
c    finish up
       RETURN
       END

c***********************************************************************
c
       SUBROUTINE DSGDBKK(JKT)
c     Disables the "designation" field in Tcl/Tk GUI and removes
c     green halo around the group's  arrow button for object
c     with pointer JKT. Called after new designation has been
c     set or a pending group option has been unselected.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
c-----------------------------------------------------------------------
c    IGI to JKT translation
       DIMENSION I2JKT(NIMAXZ)
       COMMON /TBLKK/  I2JKT, ITOT, NTHLP
c-----------------------------------------------------------------------
c    Tcl/Tk calls
       CHARACTER*11 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
       DATA LEADER,ENDER /'g_dsableb { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       LOGICAL OK
c-----------------------------------------------------------------------
c    group designation index
       JKTD = ID(JKT+4)
       JKTP = ID(JKT+1)
       IF (JKTD.GT.0) THEN
c        disable designation button in GUI
          WRITE(KUOUT,5)LEADER
            WRITE(KUOUT,5)IBEG
              WRITE(KUOUT,6)JKT2I(JKT)
            WRITE(KUOUT,5)IEND
          WRITE(KUOUT,5)ENDER
          CALL FLUSH(KUOUT)
c SCK: 0705/00
          IF(JKTP.GT.0)THEN
85        IF (ID(JKTP).EQ.3) THEN
             CALL DSEDBKK(JKTP)
             JKTP = ID(JKTP+1)
c SCK: 0705/00
             IF(JKTP.EQ.0) RETURN
             GOTO 85
          ENDIF
          ENDIF
       ENDIF
c    formats
5      FORMAT(A15)
6      FORMAT(I3)
c    finish up
       RETURN
       END

c***********************************************************************
c
       SUBROUTINE DSEDBKK(JKT)
c     Disables the "designator" button in Tcl/Tk GUI for E.O.G
c     object with pointer JKT. Called after new designation has been
c     set.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
c-----------------------------------------------------------------------
c    IGI to JKT translation
       DIMENSION I2JKT(NIMAXZ)
       COMMON /TBLKK/  I2JKT, ITOT, NTHLP
c-----------------------------------------------------------------------
c    Tcl/Tk calls
       CHARACTER*11 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
       DATA LEADER,ENDER /'g_dsableb { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       LOGICAL OK
c-----------------------------------------------------------------------
c    group designation index
       JKTD = ID(JKT+4)
       JKTP = ID(JKT+1)
c                 CALL MESBKK
c                  WRITE(*,*)'In DSEDBKK JKTD JKTP=',JKTD,JKTP
c                 CALL MESEKK
c       IF (JKTD.GT.0) THEN
c        disable designation button in GUI
          WRITE(KUOUT,5)LEADER
            WRITE(KUOUT,5)IBEG
              WRITE(KUOUT,6)JKT2I(JKT)
            WRITE(KUOUT,5)IEND
          WRITE(KUOUT,5)ENDER
          CALL FLUSH(KUOUT)
c         ENDIF
c    formats
5      FORMAT(A15)
6      FORMAT(I3)
c    finish up
       RETURN
       END
c***********************************************************************


c***********************************************************************
c
       SUBROUTINE DIOIKK(JKT,IX)
c     Diselects option IX in inclusive options group with pointer JKT.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c   Tcl/Tk calls
       CHARACTER*11 LEADER
       CHARACTER*2  ENDER
       CHARACTER*3  IBEG,IEND
       CHARACTER*1  FORM(6)
c-----------------------------------------------------------------------
c   IGI to JKT translation
       DIMENSION I2JKT(NIMAXZ)
c
       COMMON /TBLKK/  I2JKT, ITOT, NTHLP
c
       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
c
       DATA LEADER,ENDER /'io_desel { ','} '/
       DATA FORM /'(',4*' ',')'/
       DATA IBEG,IEND /' "','" '/
c-----------------------------------------------------------------------
       LOGICAL OK
c-----------------------------------------------------------------------
c
          WRITE(KUOUT,5)LEADER
            WRITE(KUOUT,5)IBEG
              WRITE(KUOUT,6)JKT2I(JKT)
            WRITE(KUOUT,5)IEND
            WRITE(KUOUT,5)IBEG
              WRITE(KUOUT,6)IX
            WRITE(KUOUT,5)IEND
          WRITE(KUOUT,5)ENDER
          CALL FLUSH(KUOUT)
5     FORMAT(A15)
6     FORMAT(I3)
      RETURN
      END



c***********************************************************************
c
       SUBROUTINE CKGDKK(OK)
c
c      Returns OK = .TRUE. if all designators are specified, .FALSE. if not.
c      Values in unspecified options need not be set.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
       DIMENSION    I2JKT(NIMAXZ)
c-----------------------------------------------------------------------
       LOGICAL OK
       COMMON  /TBLKK/  I2JKT, ITOT, NTHLP
c    Tcl/Tk i/o
       INCLUDE 'SCKIOZZ.H'
c-----------------------------------------------------------------------
c    initialize going down from god
c----------------------------------------------------------------------
      OK = .TRUE.
c SCK: 07/05/00 - changed lower bound to 1
c       to handle GOD case
      DO I= ITOT,2,-1
             JKT=I2JKT(I)
             JKTP=ID(JKT+1)
             KS=ID(JKT+3)
             IF((ID(JKT).NE.3).AND.(ID(JKT).LE.5)) THEN
                JKTD = ID(JKT+4)
c SCK: 07/05/00
                IF(JKTD.GT.0)THEN
                    KSD=ID(JKTD+3)
                    IF(ID(JKTD).EQ.10)THEN
                       IF((KSD.EQ.0).AND.(KS.GT.1)) THEN
                         OK = .FALSE.
                       ENDIF
c                     GOTO 90
                    ENDIF
                ENDIF
             ENDIF
      ENDDO
c       OK = .TRUE.
      RETURN
c
90     OK = .FALSE.
c       CALL MESBKK
c           WRITE(KUOUT,*)'Provide all designators',I,JKT,ID(JKT)
c       CALL MESEKK
       RETURN
       END
c***********************************************************************


c***********************************************************************
c
       SUBROUTINE GRVKK(JKT)
c
c      Gets new real value having pointer JKT.
c      Tcl/Tk version.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       CHARACTER*1 FORM(22),FORMAX(25),FORMIN(25)
       CHARACTER*80 ALPHA
       LOGICAL OK
c    (' Enter ',nnA1,': '$)
       DATA FORM/ '(','''',' ','E','n','t','e','r',' ','''',',',
     ;             'n','n','A','1',',','''',':',' ','''','$',')'/
c    (' Maximum is ',nnnnnnnn)
       DATA FORMAX/ '(','''',' ','M','a','x','i','m','u','m',' ',
     ;       'i','s',' ','''',',',8*'n',')'/
c    (' Minimum is ',nnnnnnnn)
       DATA FORMIN/ '(','''',' ','M','i','n','i','m','u','m',' ',
     ;       'i','s',' ','''',',',8*'n',')'/
c-----------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.13)
     ;     CALL ERRFZZ('@','GRVKK error; not real variable.@@')
c    get parameters
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
       KR = ID(JKT+6)
       JRV = ID(JKT+7)
       RVMIN = RD(JRV+1)
       RVMAX = RD(JRV+2)
c    read string sent by GUI
       READ(*,5,ERR=90) ALPHA
5      FORMAT(A)
c SCK 12/18/00
c       IF(ALPHA.EQ.'UNSET')THEN
c           CALL DISGDKK(ID(JKT+1))
c           RD(ID(JKT+7)) = RD(1)
c           ID(JKT+3) = 0
c           CALL DISACTS
c           RETURN
c       ENDIF
c    convert string to real
       CALL CH2NKK(alpha,ntype,kint,vreal,nerr,0)
       IF((NERR.NE.0))GOTO 90
c       IF((NERR.EQ.0).AND.(NTYPE.EQ.1))VREAL=KINT*1.0
c    no errors, grab real value
       RV=VREAL
c    get restriction bits
       KR8 = KR/8
       KR4 = (KR - 8*KR8)/4
       KR2 = (KR - 8*KR8 - 4*KR4)/2
       KR1 = KR - 8*KR8 - 4*KR4 - 2*KR2
c    check restrictions
       IF (KR8.GT.0)  THEN
c         check maximum
            IF (RV.GT.RVMAX)  THEN
                CALL POPSKK
                  CALL CFRZZ(RVMAX,FORMAX(17))
                  WRITE (KUOUT,FORMAX) RVMAX
                CALL POPFKK
                GOTO 10
            ENDIF
       ENDIF
       IF (KR4.GT.0)  THEN
c        check minimum
           IF (RV.LT.RVMIN)  THEN
               CALL POPSKK
                 CALL CFRZZ(RVMIN,FORMIN(17))
                 WRITE (KUOUT,FORMIN) RVMIN
               CALL POPFKK
               GOTO 10
           ENDIF
       ENDIF
       IF (KR1.GT.0)  THEN
c        sign restricted
           IF (KR2.GT.0) THEN
c                negative definite
                   IF (RV.GE.0)  THEN
                       CALL POPSKK
                         WRITE (KUOUT,*) 'Must be <0'
                       CALL POPFKK
                         GOTO 10
                   ENDIF
           ELSE
c                positive definite
                   IF (RV.LE.0)  THEN
                       CALL POPSKK
                         WRITE (KUOUT,*) 'Must be >0'
                       CALL POPFKK
                       GOTO 10
                   ENDIF
           ENDIF
       ENDIF
c    value acceptable; inform GUI
       CALL POPSKK
         WRITE(KUOUT,*)'OK'
       CALL POPFKK
c     record value
       RD(JRV) = RV
c    register the status change
       ID(JKT+3) = 2
c    clear antecedent designators
       JKTP = ID(JKT+1)
       CALL SGDUZZ(JKTP)
c    set parent unset before checking all siblings
       ID(JKTP+3) = 0
c    check if siblings are set
c    enable group designations accordingly
c       CALL MESBKK
c             write(*,*)'Calling CHKSKK in GRVKK'
c        CALL MESEKK

       CALL CHKSKK(JKT,OK)
       RETURN
c
c    read error; popup message
90     CALL POPSKK
         WRITE(KUOUT,92)
       CALL POPFKK
92     FORMAT('GRVKK error: expecting real number; try again: ')
c    record status as unset
10     ID(JKT+3) = 0
       RETURN
       END

c***********************************************************************
c
       SUBROUTINE GIVKK(JKT)
c
c      Gets new integer value having pointer JKT.
c      Tck/Tk version.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c   i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       CHARACTER*1 FORM(22),FORMAX(19),FORMIN(19)
       CHARACTER*80 ALPHA
       LOGICAL OK
c    (' Enter ',nnA1,': '$)
       DATA FORM/ '(','''',' ','E','n','t','e','r',' ','''',',',
     ;             'n','n','A','1',',','''',':',' ','''','$',')'/
c    (' Maximum is ',xx)
       DATA FORMAX/ '(','''',' ','M','a','x','i','m','u','m',' ',
     ;     'i','s',' ','''',',',2*'x',')'/
c    (' Minimum is ',xx)
       DATA FORMIN/ '(','''',' ','M','i','n','i','m','u','m',' ',
     ;     'i','s',' ','''',',',2*'x',')'/
c-----------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.12)
     ;    CALL ERRFZZ('@','GIVKK error; not integer variable.@@')
c    get parameters
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
       KR  = ID(JKT+6)
       IVMIN = ID(JKT+8)
       IVMAX = ID(JKT+9)
c     read string sent by GUI
       READ(KUIN,5,ERR=90) ALPHA
5      FORMAT(A50)
c SCK 12/18/00
c       IF(ALPHA.EQ.'UNSET')THEN
c           CALL DISGDKK(ID(JKT+1))
c           ID(JKT+7) = ID(1)
c           ID(JKT+3) = 0
c           CALL DISACTS
c           RETURN
c       ENDIF
c     convert string to integer
       CALL CH2NKK(alpha,ntype,kint,vreal,nerr,2)
       IF((NERR.NE.0).OR.(NTYPE.NE.1))GOTO 90
c     no error, grab integer value
       IV=KINT
c    get restriction bits
       KR8 = KR/8
       KR4 = (KR - 8*KR8)/4
       KR2 = (KR - 8*KR8 - 4*KR4)/2
       KR1 = KR - 8*KR8 - 4*KR4 - 2*KR2
c    check restrictions
       IF (KR8.GT.0)  THEN
c        check maximum
           IF (IV.GT.IVMAX) THEN
               CALL POPSKK
                 CALL CFIZZ(IVMAX,FORMAX(17))
                 WRITE (KUOUT,FORMAX) IVMAX
               CALL POPFKK
               GOTO 10
           ENDIF
       ENDIF
       IF (KR4.GT.0)  THEN
c        check minimum
           IF (IV.LT.IVMIN)  THEN
               CALL POPSKK
                 CALL CFIZZ(IVMIN,FORMIN(17))
                 WRITE (KUOUT,FORMIN) IVMIN
               CALL POPFKK
               GOTO 10
           ENDIF
       ENDIF
       IF (KR1.GT.0)  THEN
c        sign restricted
           IF (KR2.GT.0) THEN
c                negative definite
                   IF (IV.GE.0)  THEN
                       CALL POPSKK
                         WRITE (KUOUT,*) 'Must be <0'
                       CALL POPFKK
                       GOTO 10
                   ENDIF
           ELSE
c                positive definite
                   IF (IV.LE.0)  THEN
                       CALL POPSKK
                         WRITE (KUOUT,*) 'Must be >0'
                       CALL POPFKK
                       GOTO 10
                   ENDIF
           ENDIF
       ENDIF
c    value acceptable; inform GUI
       CALL POPSKK
         WRITE(KUOUT,*)'OK'
       CALL POPFKK
c    record value
       ID(JKT+7) = IV
c    register the status change
       ID(JKT+3) = 2
c    clear antecedent designators
       JKTP = ID(JKT+1)
       CALL SGDUZZ(JKTP)
c    set parent unset before checking all siblings
        ID(JKTP+3) = 0
c    check if siblings are set
c    enable group designations accordingly
c       CALL MESBKK
c                                write(*,*)'Calling CHKSKK in GIVKK'
c                 CALL MESEKK

       CALL CHKSKK(JKT,OK)
       RETURN
c
c    read error; popup message
90     CALL POPSKK
         WRITE (KUOUT,92)
       CALL POPFKK
92     FORMAT (' GIVKK error: expecting integer; try again ')
c    register status as unset
       ID(JKT+3) = 0
10     RETURN
       END
c***********************************************************************

c***********************************************************************
c
       SUBROUTINE GCVKK(JKT)
c
c      Gets new character value having pointer JKT.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c    i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       CHARACTER*1 FORM(31),FORMS(9),FORMR(6)
       CHARACTER*80 ALPHA
       LOGICAL OK
c    (' Enter ',nnA1,' under stars')
       DATA FORM/ '(','''',' ','E','n','t','e','r',' ','''',',',
     ;             'n','n','A','1',',','''',
     ;            ' ','u','n','d','e','r',' ','s','t','a','r','s',
     ;            '''',')'/
c    (1X,nnA1)
       DATA FORMS /'(','1','X',',','n','n','A','1',')'/
c    (nnA1)
       DATA FORMR /'(','n','n','A','1',')'/
c-----------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.11)
     ;     CALL ERRFZZ('@','GCVKK error; not character variable.@@')
c    get parameters
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
       JCS = JCN + NCN
       NCS = ID(JKT+6)
c    read string sent by GUI
       READ(*,5,ERR=10)ALPHA
5      FORMAT(A)
c SCK: 12/18/00
c       IF(ALPHA.EQ.'UNSET')THEN
c           CALL DISGDKK(ID(JKT+1))
c           DO J=JCS,JCS+NCS-1
c             CD(J)=CD(1)
c           ENDDO
c           RETURN
c       ENDIF
c    new string; get it.
       DO J=JCS,JCS+NCS-1
           CD(J)=ALPHA(J+1-JCS:J+1-JCS)
c           CALL MESBKK
c              write(*,*)ALPHA(J+1-JCS:J+1-JCS),'::'
c           CALL MESEKK
       ENDDO
c    register the status change; inform GUI
       ID(JKT+3) = 2
       CALL POPSKK
         WRITE(KUOUT,*)'OK'
       CALL POPFKK
c    clear antecedant designators
       JKTP = ID(JKT+1)
       CALL SGDUZZ(JKTP)
c    set parent unset before checking all siblings
       ID(JKTP+3) = 0
c    check if siblings are set and
c    enable group designations accordingly
c       CALL MESBKK
c          write(*,*)'Calling CHKSKK in GCVKK'
c       CALL MESEKK

       CALL CHKSKK(JKT,OK)
       RETURN
c
c    read error
10     CALL POPSKK
         WRITE (*,12)
       CALL POPFKK
12     FORMAT ('GCVKK error: Character read error; try again: ')
       RETURN
       END
c***********************************************************************


c***********************************************************************
c
       SUBROUTINE GFNKK_OLD(JKT)
c
c      Gets new file name having pointer JKT.
c      Tcl/Tk version.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       CHARACTER*1     STRING(64),EXT(3)
       CHARACTER*64    NAME
       LOGICAL OK, FOUND
c-----------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.14)
     ;     CALL ERRFZZ('@','GFNKK error; not file name variable.@@')
c    get parameters
       NCN = ID(JKT+5)
       NCF = ID(JKT+6)
       JCF = ID(JKT+4) + NCN
       KRW = ID(JKT+7)
c   get extent
       JCE = JCF + NCF
       DO 9 I=1,3
           EXT(I) = CD(JCE+I-1)
9      CONTINUE
       READ (KUIN,13,ERR=90) NAME
c       write(*,*)NAME
13     FORMAT (A)
c    load NCF characters to string
       CALL LSTRZZ(NAME,NCF,STRING)
c    find and check extent without regard to case
       DO 29 I=1,NCF
           IDOT = NCF + 1 - I
           IF (STRING(IDOT).EQ.'.')  THEN
               IF (IDOT+4.GT.NCF)  GOTO 90
               DO 23 L=1,3
c                get ASCII integer equivalents
                   IC = ICHAR(STRING(IDOT+L))
                   IE = ICHAR(EXT(L))
c                convert to upper
                   IF (IC.GT.96)   IC = IC - 32
                   IF (IE.GT.96)   IE = IE - 32
                   IF (IC.NE.IE)  GOTO 30
23             CONTINUE
c            extent ok; check file
               INQUIRE(FILE=NAME,EXIST=FOUND)
               IF (FOUND)  THEN
               CALL POPSKK
                   WRITE(KUOUT,*)'OK'
               CALL POPFKK
c                    load file upper case
                       DO 25 L=1,NCF
                           IC = ICHAR(STRING(L))
                           IF ((IC.GT.96).AND.(IC.LT.123)) IC = IC - 32
                           CD(JCF+L-1) = CHAR(IC)
25                     CONTINUE
c                    register the status change
                       ID(JKT+3) = 2
c                    clear antecedent designators
                       JKTP = ID(JKT+1)
                       CALL SGDUZZ(JKTP)
c    set parent unset before checking all siblings
                       ID(JKTP+3)  = 0
c                    check if siblings hold values
c                    enable group designations are appropriate
                       CALL CHKSKK(JKT,OK)
                       RETURN
                   ELSE
c                    not found; inform GUI
                       CALL POPSKK
                         WRITE (KUOUT,24) (STRING(L),L=1,NCF)
24                       FORMAT (' Unable to find file ',80A1)
                         WRITE (KUOUT,92)
                       CALL POPFKK
                       GOTO 90
                   ENDIF
               ENDIF
29         CONTINUE
c
c    improper extent
30     CALL POPSKK
         WRITE (KUOUT,32) EXT
32       FORMAT (' The file extent must be .',3A1)
         WRITE (KUOUT,92)
       CALL POPFKK
       RETURN
c
c    entry error
90     CALL POPSKK
         WRITE (KUOUT,92)
92       FORMAT (' Cancelling file name input.')
       CALL POPFKK
       RETURN
       END
c***********************************************************************
c******************************************************************************
c
       SUBROUTINE GFNKK(JKT)
c
c      Gets file name having pointer JKT. If cancelled, no changes made.
c      Tcl/Tk version.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       PARAMETER       (NDNAME=32)
       CHARACTER*32     NAME
       CHARACTER*32     MESSAGE
       CHARACTER*1      EXT(3)
c------------------------------------------------------------------------------
       LOGICAL FOUND,YESZZ,EXTOK,OK
c------------------------------------------------------------------------------
c    Tcl/Tk i/o
       INCLUDE 'SCKIOZZ.H'
c    check call
       KT = ID(JKT)
       IF (KT.NE.14)
     ;     CALL ERRFZZ('@','GFNZZ error; not file name variable.@@')
c    get parameters
       NCN = ID(JKT+5)
       NCF = ID(JKT+6)
       JCF = ID(JKT+4) + NCN
       KRW = ID(JKT+7)
c   get extent
       JCE = JCF + NCF
       DO I=1,3
           EXT(I) = CD(JCE+I-1)
           ENDDO
c    get name with extent
10    READ (KUIN,13,ERR=90) NAME
13    FORMAT (A)
      CALL CFNEKK(EXT,NAME,MESSAGE,EXTOK)
      IF(.NOT.EXTOK)GOTO 92
c    check file
       INQUIRE(FILE=NAME,EXIST=FOUND)
c    check for file read
       IF ((KRW.EQ.1).AND.(.NOT.FOUND)) THEN
c       file does not exist
           IF (YESZZ('File not found. Do you want another?')) THEN
                   GOTO 25
               ELSE
                   GOTO 30
               ENDIF
           ENDIF
c    check for file write
       IF ((KRW.EQ.2).AND.FOUND) THEN
c       file already exists
           IF (YESZZ('File exists. Do you want to replace it?')) THEN
                   GOTO 20
               ELSE
                   IF (YESZZ('Do you want another?')) THEN
                           GOTO 25
                       ELSE
                           GOTO 30
                       ENDIF
               ENDIF
           ENDIF
c    load name
20     CALL LSTRZZ(NAME,NCF,CD(JCF))
c20       DO  III=1,NCF
c           CD(III) = NAME(III:III)
c        ENDDO
c        CALL MESBKK
c            write(*,*)(CD(I),I=1,NCF)
c        CALL MESEKK
c    register the status change
       ID(JKT+3) = 2
c    clear antecedent designators
       JKTP = ID(JKT+1)
       CALL POPSKK
           WRITE(KUOUT,*)'OK'
       CALL POPFKK
       CALL SGDUZZ(JKTP)
c    set parent unset before checking all siblings
       ID(JKTP+3) = 0
c    check if siblings hold values
c    enable group designations are appropriate
       CALL CHKSKK(JKT,OK)
       RETURN
c   will read again; GUI treats as new try
25     ID(JKT+3) = 0
        CALL TELLZZ('@','Please type new filename.@@')
       CALL POPSKK
         WRITE (KUOUT,*)'PASS'
       CALL POPFKK

       RETURN

c   aborted; set value unset
30     ID(JKT+3) = 0
c    entry error
90     CALL TELLZZ('@','Cancelling file name input.@'//
     ;                 'Try again or choose another option.@@')
       CALL POPSKK
         WRITE (KUOUT,*)'PASS'
       CALL POPFKK
       RETURN

92     ID(JKT+3) = 0
       CALL POPSKK
         WRITE (KUOUT,*)MESSAGE
       CALL POPFKK
       RETURN
       END
c******************************************************************************
c

       SUBROUTINE  CFNEKK(EXT,NAME,MESSAGE,EXTOK)

       PARAMETER   (NDNAME=32)
       CHARACTER*1 NAME(NDNAME),EXT(3)
       CHARACTER*32  MESSAGE
       LOGICAL EXTOK
c------------------------------------------------------------------------------
c    Tcl/Tk i/o
       INCLUDE 'SCKIOZZ.H'

c------------------------------------------------------------------------------
c    convert to upper case
       LDOT = 0
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
              WRITE (UNIT=MESSAGE,FMT=10)
10            FORMAT ('Blanks not permitted.')
              GOTO 92
           ENDIF
c        capitalize
           IC = ICHAR(NAME(L))
           IF ((IC.GT.96).AND.(IC.LT.123)) IC = IC - 32
           NAME(L) = CHAR(IC)
      ENDDO
c    check name length
      IF (LEND.GT.NDNAME-4)  THEN
            WRITE (UNIT=MESSAGE,FMT=20)
20          FORMAT ('Name too long.')
            GOTO 92
       ENDIF
       IF (LDOT.EQ.0)  THEN
            WRITE (UNIT=MESSAGE,FMT=40)EXT
           GOTO 92
       ENDIF
       IF (LEND-LDOT.GT.3)  THEN
            WRITE (UNIT=MESSAGE,FMT=30)
30          FORMAT ('Extension too long.')
           GOTO 92
       ENDIF
c    check extent
       DO L=1,3
           IF (NAME(LDOT+L).NE.EXT(L))  THEN
              WRITE (UNIT=MESSAGE,FMT=40)EXT
40            FORMAT ('The extent must be .',3A1)
              GOTO 92
           ENDIF
       ENDDO

c     ok
90    EXTOK=.TRUE.
      RETURN
c     not ok
92      EXTOK=.FALSE.

       RETURN
       END
c**********************************************************************

c**********************************************************************
c
       SUBROUTINE  GCEOKK(JKT,JKTX)
c
c    Gets changes to exclusive options having pointer JKT.
c    Tcl/Tk version.
c    Currently returns JKTX=0
c-----------------------------------------------------------------------
c    Differences to command line version to be noted:
c      1. In the Tcl/Tk GUI, the Help options for each group are
c         available through buttons displayed at the bottom of the
c         group box. The call to this routine is made only after the
c         user has selected a new option (in the case of valued options
c         clicked on the option's submit). Therefore access to Help
c         options through this routine is not needed.
c      2. Also the case of I=0 (i.e. cancellation) is not needed since
c         this routine is only called once a change has been made. The
c         user of the graphical interface can always move out of the
c         group w/o submitting anything to the code.
c      3. When the selected option is a group, the only action taken by
c         this routine is to update ID(JKT+9), ID(JKT+3) and ID(JKT+4).
c         The group is tested for completeness in order to set designators.
c         Any further action in the selected group are processed by return
c         to the level of GCKK because in a GUI, the user is not
c         restricted as to when to complete the chosen group.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flshing)
       COMMON /IOKK/ KUOUT, KUIN
       LOGICAL OK
       EQUIVALENCE (IU ,ID(1))
       EQUIVALENCE (JKTGOD ,ID(10))
c-----------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.3)
     ;     CALL ERRFZZ('@','GCEOKK error; not exclusive options.@@')
c    get parameters
       NOS  = ID(JKT+8)
       NOPC = ID(JKT+9)
c    set number of options
       IX = NOS
c    get option to change
c       write(*,*)'calling NOPKK in exclusive'
       I = NOPKK(IX)
c    (check for none is currently not used in the GUI version)
c
c    save selected option and status in case of abort getting #
       NOPCX = ID(JKT+9)
c    set NOPC
       ID(JKT+9) = I
c    get pointer to option
       JKTX = ID(JKT+9+I)
c    check for group option
       IF (ID(JKTX).LE.5)  THEN
c        point to group's designator
          ID(JKT+4) = ID(JKTX+4)
          ID(JKT+3) = ID(JKTX+3)
          CALL SGDUZZ(ID(JKTX+1))
c         set unset before recursive completeness test
          IF((ID(JKTX).LE.5).OR.(ID(JKTX).EQ.20))THEN
c            CALL MESBKK
c            WRITE(*,*) '--------------------------'
c               write(*,*) JKT, ID(JKT+3), ID(JKT+4), ID(ID(JKT+4)+3)
c            WRITE(*,*) '--------------------------'
c           CALL MESEKK
c             if selected option is a group loaded by
c             application as set and it is first time
c             we select, then enable its designation
              IF( (ID(JKTX+3).EQ.1).AND.(ID(JKTX).LE.5).
     ;            AND.(ID(ID(JKTX+4)+3).EQ.0) ) CALL ENGDKK(JKTX,JKTX)
c             recursive completeness check
              CALL CHKSKK(JKTX,OK)
              CALL DSGDBKK(ID(JKTX+1))
          ENDIF
          IF ((NOPCX.GT.0).AND.ID(ID(JKT+9+NOPCX)).LE.5)  THEN
c               if the previous choice was a group with pending
c               designator, clear green arrow halo.
               CALL DSGDBKK(ID(JKT+9+NOPCX))
          ENDIF
          RETURN
      ENDIF
c    selected option not a group
c    inform GUI that selection ok so far
       CALL POPSKK
         WRITE (KUOUT,*)'OK'
       CALL POPFKK
c    check for # variables
       NVS = ID(JKTX+6)
       IF (NVS.GT.0)  THEN
c        get new values
           DO 29 IV=1,NVS
               JKTV = ID(JKTX+6+IV)
               CALL GVKK(JKTV)
c            check that value is ok
               IF (ID(JKTV+3).EQ.0)  THEN
c                value bad; restore to previous option and abort
                   ID(JKT+9) = NOPCX
                   RETURN
               ENDIF
29         CONTINUE
       ENDIF
c    everything is ok
c    set option status
       ID(JKTX+3) = 2
c SCK: 07/03/00: commented out (did not make sense)
cv       ID(JKT+4) = JKTX
c    check for completness up the family tree
c    needed if group or non-valued option
c    valued options checked in their routines.
c
c    clear designator and antecedents
       CALL SGDUZZ(JKTX)
       IF((ID(JKTX).LE.5).OR.(ID(JKTX).EQ.20))THEN
c       CALL MESBKK
c            write(*,*)'Calling CHKSKK in GCEOKK'
c       CALL MESEKK

       CALL CHKSKK(JKTX,OK)
       CALL DSGDBKK(ID(JKTX+1))
       ENDIF
c     this needed as latest change to enable button (?)
c       IF(OK) CALL CKGDKK(OK)
c SCK: 07/05/00
       IF(NOPCX.EQ.IU) RETURN
       IF(ID(JKT+9+NOPCX).EQ.0)RETURN
       IF ((NOPCX.GT.0).AND.ID(ID(JKT+9+NOPCX)).LE.5)  THEN
c        if the previous choice was a group with pending
c        designator, clear green arrow halo.
           CALL DSGDBKK(ID(JKT+9+NOPCX))
       ENDIF
       RETURN
       END
c***********************************************************************
c
c
c***********************************************************************
c
       INTEGER FUNCTION NOPKK(NOPMAX)
c
c      Gets user integer (option) number in the range 0-NOPMAX.
c      Tcl/Tk version.
c-----------------------------------------------------------------------
c      Differences to command line version:
c        Unlike the command line version <NOPZZ>, this routine assumes a
c        single pass processing. "out of range" & "non integer" errors
c        should not occur since we are processing information sumbitted
c        by the GUI and not directly by the user. This type of error in
c        the GUI version if it occurs is most probably fatal and hence
c        we should just print identifying information and abort.
c-----------------------------------------------------------------------
       CHARACTER*80 ALPHA
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flshing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       READ(*,5,ERR=10) ALPHA
5      FORMAT(A)
c
       CALL CH2NKK(alpha,ntype,kint,vreal,nerr,2)
       IF((NERR.NE.0).OR.(NTYPE.NE.1))GOTO 10
       I=KINT
       IF ((I.LT.0).OR.(I.GT.NOPMAX)) THEN
           CALL ERRFZZ('@',' NOPKK error: Integer out of range.@@')
           RETURN
       ENDIF
       NOPKK = I
       RETURN
10     CALL ERRFZZ('@','NOPKK error: Expecting integer; try again.@@')
       RETURN
       END
c***********************************************************************
c
c***********************************************************************
c
       SUBROUTINE GVKK(JKT)
c
c      Gets value of object having pointer JKT.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
c    get type
       KT = ID(JKT)
c    get the new value
       IF (KT.EQ.10)  THEN
c          group designation handled
c          separetely for GUI version
c               CALL GGDKK(JKT)
           ELSEIF (KT.EQ.11)  THEN
c            character variable
               CALL GCVKK(JKT)
           ELSEIF (KT.EQ.12)  THEN
c            integer variable
               CALL GIVKK(JKT)
           ELSEIF (KT.EQ.13)  THEN
c            real variable
               CALL GRVKK(JKT)
           ELSEIF (KT.EQ.14)  THEN
c            file name
               CALL GFNKK(JKT)
           ELSE
c            error
               CALL POPSKK
                 WRITE(KUOUT,*)'GVKK error; not variable.'
               CALL POPFKK
           ENDIF
       RETURN
       END



c***********************************************************************
c
       SUBROUTINE  GCIOKK(JKT,JKTX)
c
c    Gets changes to inclusive options having pointer JKT.
c    Tcl/Tk version
c    Currently returns JKTX= 0.
c-----------------------------------------------------------------------
c    Differences to command line version:
c      1.  With a GUI this routine is called only when the user selects
c          or deselects an option. Therefore, this is a single pass call
c          (no looping listenning for changes) and there is no check for
c          "I=0".
c      2.  The Tcl/Tk GUI informs explicitly this routine whether a
c          the chosen option is to be selected or deselected. The
c          reason is that if the request is to "select" additional
c          information is expected on the Tcl/Tk side and must be put
c          in the communication pipe.
c      3.  Help is for the inclusive-options group is accessed
c          independently of option selection and is not processed here.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
       LOGICAL OK, VALS
c-----------------------------------------------------------------------
       CHARACTER*1 FORM(17)
       CHARACTER*80 ALPHA
c    ('(',nn,' max.)')
       DATA FORM/'(','''','(','''',',','n','n',',',
     ;           '''',' ','m','a','x','.',')','''',')'/
c-----------------------------------------------------------------------
c    check call
c       write(*,*)'Entering GCIOKK'
       KT = ID(JKT)
       IF (KT.NE.4)
     ;     CALL ERRFZZ('@','GCIOKK error; not inclusive options.@@')
c    get parameters
       NOS = ID(JKT+8)
       NOPMAX = ID(JKT+9)
c    loop point
1     CONTINUE
c    reset parameters
       NSEL = 0
       VALS = .FALSE.
       DO 9 I=1,NOS
           JKTO = ID(JKT+9+NOS+I)
c        count selected items
           NOPCI = ID(JKT+9+I)
           IF (NOPCI.EQ.1) THEN
c            selected option, add to count
               NSEL = NSEL + 1
           ENDIF
9      CONTINUE
c    get option to change
       IX = NOS
       I = NOPKK(IX)
c    select or deselect info
       READ(*,5,ERR=90)ALPHA
5      FORMAT(A)
c    Note: check for none & help (present in command line
c          version not necessary for GUI).
c
c    get pointer to option
       JKTX = ID(JKT+9+NOS+I)
c    check for group option
       IF (ID(JKTX).LE.4) THEN
           IF (ALPHA.EQ.'DESELECT') THEN
              ID(JKT+9+I) = 0
           ELSEIF (ALPHA.EQ.'SELECT') THEN
               ID(JKT+9+I) = 1
           ENDIF
           RETURN
       ENDIF
c    check status
       NOPCI = ID(JKT+9+I)
c       IF (NOPCI.EQ.1)  THEN
c            deselect option
c             write(*,*)'Deselecting'
16     IF (ALPHA.EQ.'DESELECT') THEN
            ID(JKT+9+I) = 0
       ELSEIF (ALPHA.EQ.'SELECT') THEN
c              write(*,*)'Selecting'
c            check if ok to add option
               IF (NSEL.GE.NOPMAX)  THEN
                   IF (ID(JKT+9+I).EQ.0) THEN
                      CALL POPSKK
                           WRITE (KUOUT,18)
18                         FORMAT('Maximum number already selected')
                      CALL POPFKK
                      CALL DIOIKK(JKT,I)
                      RETURN
                    ENDIF
               ELSE
               ENDIF
c           check for # variables
              NVS = ID(JKTX+6)
              IF (NVS.GT.0)  THEN
                   CALL POPSKK
                     WRITE (KUOUT,*)'OK'
                   CALL POPFKK
c               get new values
                  DO 29 IV=1,NVS

                      JKTV = ID(JKTX+6+IV)
                      CALL GVKK(JKTV)
c                    check value and abort if not set
                        IF (ID(JKTV+3).EQ.0)  THEN
                          RETURN
                        ENDIF
29                CONTINUE
              ELSE
                   CALL POPSKK
                     WRITE (KUOUT,*)'OK'
                   CALL POPFKK

              ENDIF
c           change status
              ID(JKT+9+I) = 1
       ENDIF
c    set status
       ID(JKT+3) = 2
c    clear designator and antecedants
       CALL SGDUZZ(JKT)
c    now check the family tree for completion
c       CALL MESBKK
c                                write(*,*)'Calling CHKSKK in GCIOKK'
c                 CALL MESEKK
       CALL CHKSKK(JKTX,OK)
c    we're done for this option

        RETURN
c     error handling
90      CALL POPSKK
            WRITE(KUOUT,*)'APLHA error in GCIOKK'
        CALL POPFKK
       END
c***********************************************************************
c
c***********************************************************************
c
       SUBROUTINE GGDKK(JKTX)
c
c      Gets new group designation for group having pointer JKT.
c      Tcl/Tk version.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       CHARACTER*80 ALPHA
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
       EQUIVALENCE     (ID(10),JKTGOD)
       LOGICAL OK
c-----------------------------------------------------------------------
c      write(*,*)'entering GGDKK'
c    check call
1      READ(*,5,ERR=10)ALPHA
5      FORMAT(A)

       JKT = ID(JKTX+4)
c       IF(JKT.EQ.0)RETURN
       KT  = ID(JKT)
c       write(55,*)'ALPHA=', ALPHA,' JKT=',JKT,' KT=',KT
       IF (KT.NE.10)
     ;     CALL ERRFZZ('@','GGDKK error; not group designation.@@')
c    check that it is unset
       KS = ID(JKT+3)
c        write(55,*)'KS=',KS
       IF (KS.NE.0)  RETURN
c    get parameters
       JCD = ID(JKT+4)
       NCD = ID(JKT+5)
c    identify parent
       JKTP = ID(JKT+1)
c    verify that parent is set
       IF (ID(JKTP+3).EQ.0) RETURN
       KTP = ID(JKTP)
       IF (KTP.GT.5)
     ;     CALL ERRFZZ('@','GGDKK error; parent not group.@@')
c    get parent header data
       JCH = ID(JKTP+6)
       NCH = ID(JKTP+7)
c SCK: 12/18/00
c       IF(ALPHA.EQ.'UNSET')THEN
c           CALL DISGDKK(ID(JKT+1))
c           DO J=JCD,JCD+NCD-1
c             CD(J)=CD(1)
c           ENDDO
c           RETURN
c       ENDIF
       DO J=JCD,JCD+NCD-1
           CD(J)=ALPHA(J+1-JCD:J+1-JCD)
c       READ(*,FORMR,ERR=10)   (CD(J),J=JCS,JCS+NCS-1)
       ENDDO
c       write(*,*)'continuing in GGDKK',(CD(J),J=JCD,JCD+NCD-1)
c    register the status changes
       ID(JKT+3) = 2
       ID(JKTP+3) = 3
c    clear ancestor designations
       IF (JKTP.NE.JKTGOD)  THEN
           JKTPP= ID(JKTP+1)
           CALL SGDUZZ(JKTPP)
c           CALL ENGDKK(JKTPP)
c                  write(*,*)'continuing in GGDKK'
       ENDIF
c        write(*,*)'continuing in GGDKK'
c       CALL DISGDKK(JKT)
c       CALL DGDRKK(JKT)
        CALL DGDKK(JKT)
c the following block takes care of
c updating designators in exclusive option
c groups
       IF (JKTP.NE.JKTGOD) THEN
          JKTPX = JKTP
8         JKTPPX= ID(JKTPX+1)
c          write(55,*)'LOOPING in 8',JKTPX,JKTPPX
          IF (ID(JKTPPX).EQ.3) THEN
              CALL DGDKK(JKTPX)
              JKTPX= ID(JKTPX+1)
c SCK: 07/05/00
              IF(JKTPX.NE.JKTGOD)GOTO 8
          ELSE
              CALL CHKSKK(JKTPX,OK)
          ENDIF

       ENDIF
c
       CALL DSGDBKK(JKTP)
       CALL CHKSKK(JKTP,OK)
c
       RETURN
c
c    read error
10     CALL POPSKK
         WRITE (KUOUT,12)
       CALL POPFKK
12     FORMAT (' Character read error; try again: ')
       END
c*********

c***********************************************************************
c
       SUBROUTINE  GHOPKK(JKT,JKTX,HELP)
c
c      Gets help option from help options group having pointer JKT.
c      If a help options group is selected, returns the group pointer
c      in JKTX. Otherwise returns JKTX = 0.
c-----------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c-----------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.6)
     ;     CALL ERRFZZ('@','GHOPKK error; not help options.@@')
c    get parameters
       NOH = ID(JKT+5)
c    get option to change
12     CONTINUE
       I = NOPKK(NOH)
c    check for none
       IF (I.EQ.0)  THEN
           JKTX = 0
           RETURN
       ENDIF
c    check option
       JKTO = ID(JKT+5+I)
       IF (ID(JKTO).EQ.6)  THEN
c        option group selected
           JKTX = JKTO
           RETURN
       ENDIF
c    get pointer to help option
       JKTH = ID(JKT+5+I)
       KHELP = ID(JKTH+5)
       CALL HELP(KHELP)
c    we're done
       RETURN
       END
c***********************************************************************


c***********************************************************************
c
       INTEGER FUNCTION NOPZZ(NOPMAX)
c
c      Gets user integer (option) number in the range 0-NOPMAX.
c-----------------------------------------------------------------------
c  i/o units (explicit declaration needed for flushing)
       COMMON /IOKK/ KUOUT, KUIN
c-----------------------------------------------------------------------
1      READ (*,*,ERR=10) I
       IF ((I.LT.0).OR.(I.GT.NOPMAX)) THEN
          CALL ERRFZZ('@','NOPZZ error: integer out of range;'//
     ;                    ' try again!@@')
          GOTO 1
       ENDIF
       NOPZZ = I
       RETURN
10     CALL ERRFZZ ('@','NOPZZ error: expecting integer; try again! @@')
       GOTO 1
       END
c******************************************************************************
c
       SUBROUTINE LSTRZZ(A,N,B)
c
c      Loads string A of length N to string B
c------------------------------------------------------------------------------
       CHARACTER*1 A(*),B(*)
c------------------------------------------------------------------------------
       DO 9 I=1,N
           B(I) = A(I)
9          CONTINUE
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE LI2CZZ(I,STRING)
c
c      Formats a 2-digit positive integer and loads 2 bytes to STRING
c------------------------------------------------------------------------------
       CHARACTER*1 STRING(2)
       CHARACTER*1 NUM(10)
       DATA    NUM    /'0','1','2','3','4','5','6','7','8','9'/
c------------------------------------------------------------------------------
       IF ((I.GE.0).AND.(I.LT.100)) THEN
               IF (I.LT.10)  THEN
                       STRING(1) = ' '
                       STRING(2) = NUM(I+1)
                   ELSE
                       IX = I/10
                       STRING(1) = NUM(IX + 1)
                       STRING(2) = NUM(I - 10*IX + 1)
                   ENDIF
           ELSE
c            error
               CALL ERRFZZ('@','LI2CZZ error.@@')
           ENDIF
           RETURN
           END
c******************************************************************************
c
       SUBROUTINE CFCZZ(NC,FORM)
c
c      Creates format for character variable of length NC. Loads 4-byte format
c      to FORM.
c------------------------------------------------------------------------------
       CHARACTER*1     FORM(4),FORMC(4)
       INTEGER NC
c    nnA1
       DATA FORMC  /'n','n','A','1'/
c------------------------------------------------------------------------------
       DO 9 I=1,4
           FORM(I) = FORMC(I)
9          CONTINUE
       CALL LI2CZZ(NC,FORM(1))
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CFIZZ(I,FORM)
c
c      Creates format for integer variable I. Loads 2-byte format to FORM.
c------------------------------------------------------------------------------
       CHARACTER*1     FORM(*),CN(7)
       INTEGER I, N
       DATA    CN/'1','2','3','4','5','6','7'/
c------------------------------------------------------------------------------
       IF (IABS(I).GE.10000)  THEN
               N = 5
           ELSEIF (IABS(I).GE.1000)  THEN
               N = 4
           ELSEIF (IABS(I).GE.100)  THEN
               N = 3
           ELSEIF (IABS(I).GE.10)  THEN
               N = 2
           ELSE
               N = 1
           ENDIF
c    space for sign
       IF (I.LT.0) N = N + 1
c    set character
       FORM(1) = 'I'
       FORM(2) = CN(N)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CFRZZ(V,FORM)
c
c      Creates format for 4-digit display of a real variable V. Uses F format
c      10000 > V >= .001, otherwise E format. Loads 8-bye format to FORM.
c------------------------------------------------------------------------------
       CHARACTER*1     FORM(8),FORME(8),FORMF(8)
       CHARACTER*1     CN(10)
       INTEGER I,N2,N4
c------------------------------------------------------------------------------
       DATA    FORME /'1','P','E','1','n','.','3',' '/
       DATA    FORMF /'F','n','.','n',4*' '/
       DATA    CN    /'0','1','2','3','4','5','6','7','8','9'/
c------------------------------------------------------------------------------
c    branch on range
       IF ((ABS(V).GE.10000.).OR.(ABS(V).LT.0.001)) THEN
               IF (V.EQ.0.)  THEN
c                    set for F2.0
                       DO 2 I=1,8
                           FORM(I) = FORMF(I)
2                          CONTINUE
                       FORM(2) = CN(3)
                       FORM(4) = CN(1)
                   ELSE
c                    exponential format
                       DO 3 I=1,8
                           FORM(I) = FORME(I)
3                          CONTINUE
                       IF (V.GT.0.) THEN
c                           1PE09.3
                               FORM(4) = CN(1)
                               FORM(5) = CN(10)
                           ELSE
c                           1PE10.3
                               FORM(4) = CN(2)
                               FORM(5) = CN(1)
                           ENDIF
                   ENDIF
           ELSE
c            fixed point formats
               DO 5 I=1,8
                   FORM(I) = FORMF(I)
5                  CONTINUE
               IF (ABS(V).GE.1000.) THEN
c                    F5.0
                       N2 = 5
                       N4 = 0
                   ELSEIF(ABS(V).GE.100.) THEN
c                    F5.1
                       N2 = 5
                       N4 = 1
                   ELSEIF(ABS(V).GE.10.) THEN
c                    F5.2
                       N2 = 5
                       N4 = 2
                   ELSEIF(ABS(V).GE.1.) THEN
c                    F5.3
                       N2 = 5
                       N4 = 3
                   ELSEIF(ABS(V).GE.0.1) THEN
c                    F5.4
                       N2 = 5
                       N4 = 4
                   ELSEIF(ABS(V).GE.0.01) THEN
c                    F6.5
                       N2 = 6
                       N4 = 5
                   ELSEIF(ABS(V).GE.0.001) THEN
c                    F7.6
                       N2 = 7
                       N4 = 6
                   ENDIF
c            space for sign
               IF (V.LT.0) N2 = N2 + 1
               FORM(2) = CN(N2+1)
               FORM(4) = CN(N4+1)
           ENDIF
       RETURN
       END
c*****************************************************************************

c******************************************************************************
c
       SUBROUTINE  DGDZZ(JKT)
c
c      Displays the group designator having pointer JKT if set
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       CHARACTER*1  FORM(11)
       DATA FORM  /'(','''',' ',' ','''',',','n','n','A','1',')'/
c------------------------------------------------------------------------------
       JCD = ID(JKT+4)
       NCD = ID(JKT+5)
       IF(CD(JCD).NE.CHAR(0))THEN
         CALL CFCZZ(NCD,FORM(7))
         WRITE (*,*) (CD(L),L=JCD,JCD+NCD-1)
       ENDIF
       RETURN
       END
cc****************************************************************************
c
       SUBROUTINE  DGHZZ(I,JKT)
c
c      Displays the header of group having pointer JKT, following I, followed
c      by the group designation if set.
c
c      If I=0, I is not displayed and a blank line is first written.
c
c      If the group is an option, it is displayed accordingly.
c
c      If the group is an exclusive option, the selected option is displayed
c      as the designator.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       CHARACTER*1  FORM(7),C
       DATA FORM  /'(',4*' ','$',')'/
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF ((KT.LT.1).AND.(KT.GT.6))  THEN
           CALL ERRFZZ('@','DGHZZ error.@@')
       ENDIF
c    branch on type
       IF ((KT.EQ.6).OR.(KT.EQ.21))  THEN
c            help group or option; get parameters
               JCH = ID(JKT+3)
               NCH = ID(JKT+4)
       ELSE
c            not help group; get parameters
               JCH = ID(JKT+6)
               NCH = ID(JKT+7)
       ENDIF
c    check parent for option group
       JKTP = ID(JKT+1)
       KTP = ID(JKTP)
       IF (KTP.EQ.3)  THEN
c            parent is exclusive option group
               NOPC = ID(JKTP+9)
               IF (NOPC.EQ.I)  THEN
c                    option is selected
                       NOP = 2
                ELSE
                       NOP = 1
                ENDIF
        ELSEIF (KTP.EQ.4) THEN
c            parent is inclusive option group
               NOPI = ID(JKTP+9+I)
               IF (NOPI.EQ.1)  THEN
c                    option is selected
                       NOP = 2
               ELSE
                       NOP = 1
               ENDIF
        ELSE
c            parent is not an option group
               NOP = 0
        ENDIF
c    display
       IF (I.EQ.0)  THEN
c            srart display without integer as header for children
             WRITE (*,12)
12           FORMAT(1X,$)
       ELSE
c            start display with integer
            IF (NOP.EQ.0)  THEN
c                    not option
             WRITE (*,14) I
14           FORMAT(I4,'  ',$)
            ELSE
c              option
               IF (NOP.EQ.2) THEN
c              selected option
                    C = '*'
               ELSEIF (NOP.EQ.1)  THEN
c              unselected option
                    C = ' '
               ELSE
                    CALL ERRFZZ('@','DGHZZ option error.@@')
               ENDIF
               WRITE (*,16) I,C
16             FORMAT(I4,A1,' ',$)
             ENDIF
         ENDIF
c    format header
       CALL CFCZZ(NCH,FORM(2))
c    add header
c      WRITE (*,FORM) (CD(L),L=JCH,JCH+NCH-1)
c    get designator pointer
       JKTD = ID(JKT+4)
c    check for designator
       IF (JKTD.NE.0) THEN
c        check that designator is ready
           IF(ID(JKTD+3).GT.0) THEN
c            check designator type
               KTD = ID(JKTD)
c            check for option
               IF (KTD.EQ.20)  THEN
c                close with option
                   CALL DOPZZ(0,JKTD)
                   RETURN
               ENDIF
c            check for group
               IF (KTD.EQ.10) THEN
c                close with group designator
                   CALL DGDZZ(JKTD)
                   RETURN
               ENDIF
           ENDIF
       ENDIF
c    no designator; close without
       WRITE (*,*) ' '
       RETURN
       END
c******************************************************************************

c******************************************************************************
c
       SUBROUTINE DOPZZ(I,JKT)
c
c      Displays the option having pointer JKT, following I.  If the option
c      is selected (NOPC=1), the I is followed by a * and any # values
c      are displayed. Otherwise the # names are displayed.
c
c      If I=0 the option is displayed without the I and * as a designator
c      (only used for exclusive options).
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       CHARACTER*1 C,FORM(128)
       CHARACTER*1 FORMC(7),FORMI(5),FORMR(11)
       DATA FORMC /'(',4*' ','$',')'/
       DATA FORMI /'(',2*' ','$',')'/
       DATA FORMR /'(',8*' ','$',')'/
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.LE.5)  THEN
c        option is a group
           CALL DGHZZ(I,JKT)
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
       IF (I.GT.0)  THEN
c           write integer
               WRITE (*,2) I
2              FORMAT (I4,$)
C  SCK modification
               IF (NOPC.EQ.1)  THEN
c                    item is selecgted
                       FORM(3) = '*'
                   ELSE
                       FORM(3) = ' '
                   ENDIF
           ELSE
c            designator
               FORM(3) = ':'
           ENDIF
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
9                              CONTINUE
                       ELSE
c                        option chosen; end the format
                           LS = LS + 1
                           FORM(LS) = ''''
                           LS = LS + 1
                           FORM(LS) = '$'
                           LS = LS + 1
                           FORM(LS) = ')'
                           WRITE (*,FORM)
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
                                   WRITE (*,FORMC)
     ;                                (CD(LZ),LZ=JCS,JCS+NCS-1)
                               ELSEIF (KTV.EQ.12) THEN
c                                   integer variable
                                      IV = ID(JKTV+7)
c                                   construct the format
                                      CALL CFIZZ(IV,FORMI(2))
c                                   display
                                      WRITE (*,FORMI) IV
                               ELSEIF (KTV.EQ.13) THEN
c                                   real variable
                                      JRV = ID(JKTV+7)
                                      RV = RD(JRV)
c                                   construct the format
                                      CALL CFRZZ(RV,FORMR(2))
c                                   display
                                      WRITE (*,FORMR) RV
                               ELSEIF (KTV.EQ.14) THEN
c                                   file name variable
                                      JCV = ID(JKTV+4) + ID(JKTV+5)
                                      NCV = ID(JKTV+6)
c                                construct the format
                                   CALL CFCZZ(NCV,FORMC(2))
c                                display
                                   WRITE (*,FORMC)
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
49         CONTINUE
c    flush
       LS = LS + 1
       FORM(LS) = ' '
       LS = LS + 1
       FORM(LS) = ''''
       LS = LS + 1
       FORM(LS) = ')'
       WRITE (*,FORM)
       RETURN
       END
c****************************************************************************
c
c******************************************************************************
c
       SUBROUTINE CHKLKK(OK)
c
c      Returns OK = .TRUE. if all data are specified, .FALSE. if not.
c      Values in unspecified options need not be set.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
c------------------------------------------------------------------------------
       CHARACTER*128   LINE
       LOGICAL OK
c------------------------------------------------------------------------------
c    initialize going down from god
       JKT = JKTGOD
c
c    going down generations
10     KT = ID(JKT)
C      write (*,*) '10:  jkt,jktp,kt  ',jkt,id(jkt+1),kt
C      write (9,*) '10:  jkt,jktp,kt  ',jkt,id(jkt+1),kt
c    skip processing of objects without values
       IF ((KT.EQ.6).OR.(KT.EQ.21))  GOTO 20
c    skip values checks in unselected options
       IF (JKT.NE.JKTGOD) THEN
c        get parent
           JKTP = ID(JKT+1)
           KTP = ID(JKTP)
           IF (KTP.EQ.3)  THEN
c                parent is exclusive option; get selected option
                   NOPI = ID(JKTP+9)
c                if nothing is selected, go no further
                   IF (NOPI.EQ.IU)  THEN

                       JCN = ID(JKTP+6)
                       NCN = ID(JKTP+7)
c                       WRITE (LINE,12)
c     ;                   (CD(L),L=JCN,JCN+NCN-1),'@','@'
c12                     FORMAT(' @Unset option: ',80A1)
                       GOTO 90
                   ENDIF
c                get option pointer
                   JKTI = ID(JKTP+9+NOPI)
c                if option is not selected, don't check values
                   IF (JKT.NE.JKTI) GOTO 20
           ELSEIF (KTP.EQ.4)  THEN
c                parent is inclusive option; get option count
                   NOS = ID(JKTP+8)
c                find the option and check status
                   DO 15 L=1,NOS
                       JKTL = ID(JKTP+9+NOS+L)
                       IF (JKT.EQ.JKTL)  THEN
c                        go no further if the option is selected
                           IF (ID(JKTP+9+L).NE.1)  GOTO 20
                           GOTO 16
                       ENDIF
15                     CONTINUE
           ENDIF
       ENDIF
c    check for first child
16     JKTC = JKTCZZ(JKT,1)
       IF (JKTC.LT.0)  CALL ERRFZZ('@','CHKSZZ error; '//
     ;                     'undefined child.@@')
c    go process if no children
       IF (JKTC.EQ.0)  GOTO 40
c    set for first child
       JKT = JKTC
       GOTO 10
c
c    go to younger sibling
20     JKTS = JKTSZZ(JKT)
C      write (*,*) '20:  jkt,jktp,jkts',jkt,id(jkt+1),jkts
C      write (9,*) '20:  jkt,jktp,jkts',jkt,id(jkt+1),jkts
c    if no younger sibling, go up to parent
       IF (JKTS.EQ.0) GOTO 30
c    set for younger sibling
       JKT = JKTS
       GOTO 10

c    go up to parent
30     IF (JKT.EQ.JKTGOD)  THEN
c        scan completed
           OK = .TRUE.
           RETURN
           ENDIF
c    set for parent
       JKT = ID(JKT+1)
c    go for parent's sibling
       GOTO 20

c    end of the line; check object
40     KS = ID(JKT+3)
C      write (*,*) '40:  jkt,jktp,kt  ',jkt,id(jkt+1),kt
C      write (9,*) '40:  jkt,jktp,kt  ',jkt,id(jkt+1),kt
       IF (KS.EQ.0)  THEN
           JCN = ID(JKT+4)
           NCN = ID(JKT+5)
c           WRITE (LINE,42) (CD(L),L=JCN,JCN+NCN-1),'@','@'
c42         FORMAT(' @Unset values INCLUDE ',80A1)
           GOTO 90
           ENDIF
c    check for table
       IF (ID(JKT).EQ.5)  THEN
           CALL CHKTZZ(JKT,OK)
           IF (.NOT.OK) THEN
               JCN = ID(JKT+6)
               NCN = ID(JKT+7)
               WRITE (LINE,52) (CD(L),L=JCN,JCN+NCN-1),'@','@'
52             FORMAT (' @Incomplete table: ',80A1)
               GOTO 90
               ENDIF
           ENDIF
c    go to younger sibling
       GOTO 20
c
c    incomplete data
90     OK = .FALSE.
       RETURN
       END
c******************************************************************************
c

