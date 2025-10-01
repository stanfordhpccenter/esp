c*****************************************************************************
c
c      This file contains subroutines used in the non-graphical interface.
c      The primary routines for the non-graphical interface are in I1.FOR.
c
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
c
       SUBROUTINE BLMSZZ(N)
c
c    Start of a long message series. Use unique N for each call. GUI writer
c    can then adapt accordingly.
c------------------------------------------------------------------------------
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ELMSZZ(N)
c
c    End of a long message series. Use unique N for each call. GUI writer
c    can then adapt accordingly.
c------------------------------------------------------------------------------
       RETURN
       END
c******************************************************************************
c
       INTEGER FUNCTION NOPZZ(NOPMAX)
c
c      Gets user integer (option) number in the range 0-NOPMAX.
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96 LINE
c------------------------------------------------------------------------------
1      READ (*,2,ERR=10) I
2      FORMAT (I10)
       IF ((I.LT.0).OR.(I.GT.NOPMAX)) THEN
c        out of range; construct prompt
           ML = 0
           CALL LOADZZ(' Integer out of range; try again. > ',36,
     ;      LINE,ML)
c        write prompt
           CALL POSTZZ(LINE,ML,.TRUE.)
           GOTO 1
           ENDIF
       NOPZZ = I
       RETURN
c    error; construct prompt
10     ML = 0
       CALL LOADZZ(' Expecting integer; try again. > ',33,LINE,ML)
       CALL POSTZZ(LINE,ML,.TRUE.)
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
       DO I=1,N
           B(I) = A(I)
           ENDDO
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
       SUBROUTINE CFCZZ(N,FORM)
c
c      Creates a format for reading a charactr string of length N
c------------------------------------------------------------------------------
       CHARACTER*1 FORMC(6),FORM(6)
c    (nnA1)
       DATA FORMC /'(','n','n','A','1',')'/
c------------------------------------------------------------------------------
       DO I=1,6
           FORM(I) = FORMC(I)
           ENDDO
       CALL LI2CZZ(N,FORM(2))
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CFIZZ(I,FORM,N)
c
c      Creates format for integer variable I. Loads 4-byte format to FORM.
c      Returns field width N.
c------------------------------------------------------------------------------
       CHARACTER*1     FORM(4),CN(7)
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
       FORM(1)='('
       FORM(2) = 'I'
       FORM(3) = CN(N)
       FORM(4)=')'
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CFRZZ(V,FORM,NW)
c
c      Creates format for 4-digit display of a real variable V. Uses F format
c      10000 > V >= .001, otherwise E format. Loads 10-bye format to FORM.
c      Returns field width NW.
c------------------------------------------------------------------------------
       CHARACTER*1     FORM(10),FORME(10),FORMF(10)
       CHARACTER*1     CN(10)
c------------------------------------------------------------------------------
       DATA    FORME /'(','1','P','E','1','n','.','3',' ',')'/
       DATA    FORMF /'(','F','n','.','n',4*' ',')'/
       DATA    CN    /'0','1','2','3','4','5','6','7','8','9'/

c------------------------------------------------------------------------------
c    branch on range
       IF ((ABS(V).GE.10000.).OR.(ABS(V).LT.0.001)) THEN
               IF (V.EQ.0.)  THEN
c                    set for F2.0
                       DO 2 I=1,10
                           FORM(I) = FORMF(I)
2                          CONTINUE
                       FORM(3) = CN(3)
                       FORM(5) = CN(1)
                       NW= 2
                   ELSE
c                    exponential format
                       DO 3 I=1,10
                           FORM(I) = FORME(I)
3                          CONTINUE
                       IF (V.GT.0.) THEN
c                           1PE09.3
                               FORM(5) = CN(1)
                               FORM(6) = CN(10)
                               NW = 9
                           ELSE
c                           1PE10.3
                               FORM(5) = CN(2)
                               FORM(6) = CN(1)
                               NW =10
                           ENDIF
                   ENDIF
           ELSE
c            fixed point formats
               DO 5 I=1,10
                   FORM(I) = FORMF(I)
5                  CONTINUE
               IF (ABS(V).GE.1000.) THEN
c                    F5.0
                       NW = 5
                       ND = 0
                   ELSEIF(ABS(V).GE.100.) THEN
c                    F5.1
                       NW = 5
                       ND = 1
                   ELSEIF(ABS(V).GE.10.) THEN
c                    F5.2
                       NW = 5
                       ND = 2
                   ELSEIF(ABS(V).GE.1.) THEN
c                    F5.3
                       NW = 5
                       ND = 3
                   ELSEIF(ABS(V).GE.0.1) THEN
c                    F5.4
                       NW = 5
                       ND = 4
                   ELSEIF(ABS(V).GE.0.01) THEN
c                    F6.5
                       NW = 6
                       ND = 5
                   ELSEIF(ABS(V).GE.0.001) THEN
c                    F7.6
                       NW = 7
                       ND = 6
                   ENDIF
c            space for sign
               IF (V.LT.0) NW = NW + 1
               FORM(3) = CN(NW+1)
               FORM(5) = CN(ND+1)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DACTZZ(I)
c
c      Displays action I with I
c------------------------------------------------------------------------------
c   setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE     (ID(9),NACT)
c------------------------------------------------------------------------------
c    check call
       IF ((I.LT.1).OR.(I.GT.NACT)) THEN
           CALL ERRFZZ('@','DACTZZ error; illegal action.@@')
           ENDIF
c    get data
       JAO = 11 + 3*(I-1)
       JCA = ID(JAO+1)
       NCA = ID(JAO+2)
c    display
       WRITE (*,2) I,(CD(J),J=JCA,JCA+NCA-1)
2      FORMAT (I4,1X,70A1)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DCVZZ(I,JKT)
c
c      Displays the character variable having pointer JKT, following I.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*64    LINE
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.11) CALL ERRFZZ('@','DCVZZ error.@@')
c    get parameters
       KS = ID(JKT+3)
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
       NCS = ID(JKT+6)
c    load the index
       WRITE (LINE,2) I
2      FORMAT(I4,1X)
       ML = 5
c    load the variable name
       CALL LOADZZ(CD(JCN),NCN,LINE,ML)
c    load connector
       CALL LOADZZ(' = ',3,LINE,ML)
c    load the variable value if set
       IF (KS.GT.0)  THEN
           JCS = JCN + NCN
           CALL LOADZZ(CD(JCS),NCS,LINE,ML)
           ENDIF
c    display
       CALL POSTZZ(LINE,ML,.FALSE.)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DIVZZ(I,JKT)
c
c      Displays the integer variable having pointer JKT, following I.
c-----------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    intger variableformat
       CHARACTER*4 FORMI
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
       CHARACTER*16    TEMP
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.12) CALL ERRFZZ('@','DIVZZ error.@@')
c    get parameters
       KS = ID(JKT+3)
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
c    load the index
       WRITE (LINE,2) I
2      FORMAT(I4,1X)
       ML = 5
c    load the variable name
       CALL LOADZZ(CD(JCN),NCN,LINE,ML)
c    load connector
       CALL LOADZZ(' = ',3,LINE,ML)
c    load the variable value if set
       IF (KS.GT.0)  THEN
c        get value
           IV = ID(JKT+7)
c        construct format
           CALL CFIZZ(IV,FORMI,NW)
c        load
           WRITE (TEMP,FORMI) IV
           CALL LOADZZ(TEMP,NW,LINE,ML)
           ENDIF
c    display
       CALL POSTZZ(LINE,ML,.FALSE.)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DRVZZ(I,JKT)
c
c      Displays the real variable having pointer JKT, following I.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    real variable format
       CHARACTER*10 FORMR
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
       CHARACTER*16    TEMP
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.13) CALL ERRFZZ('@','DRVZZ error.@@')
c    get parameters
       KS = ID(JKT+3)
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
c    load the index
       WRITE (LINE,2) I
2      FORMAT(I4,1X)
       ML = 5
c    load the variable name
       CALL LOADZZ(CD(JCN),NCN,LINE,ML)
c    load connector
       CALL LOADZZ(' = ',3,LINE,ML)
c    load the variable value if set
       IF (KS.GT.0)  THEN
c        get value
           JRV = ID(JKT+7)
           RV = RD(JRV)
c        construct format
           CALL CFRZZ(RV,FORMR,NW)
c        load
           WRITE (TEMP,FORMR) RV
           CALL LOADZZ(TEMP,NW,LINE,ML)
           ENDIF
c    display
       CALL POSTZZ(LINE,ML,.FALSE.)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DFNZZ(I,JKT)
c
c      Displays the file name variable having pointer JKT, following I.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96    LINE
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.14) CALL ERRFZZ('@','DFNZZ error.@@')
c    get parameters
       KS = ID(JKT+3)
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
c    load the index
       WRITE (LINE,2) I
2      FORMAT(I4,1X)
       ML = 5
c    load the variable name
       CALL LOADZZ(CD(JCN),NCN,LINE,ML)
c    load connector
       CALL LOADZZ(' = ',3,LINE,ML)
c    load the variable value if set
       IF (KS.GT.0)  THEN
c        get pointer and length
           JCF = JCN + NCN
           NCF = ID(JKT+6)
           CALL LOADZZ(CD(JCF),NCF,LINE,ML)
           ENDIF
c    display
       CALL POSTZZ(LINE,ML,.FALSE.)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DTBHZZ(JKT)
c
c      Displays table header and columns header for table having pointer JKT.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       LOGICAL     OUT
c------------------------------------------------------------------------------
c    maximum table width
       PARAMETER (NWMAX=78)
       CHARACTER*1 ESC,C,D(NWMAX)
c------------------------------------------------------------------------------
c    display table header
       CALL DGHZZ(0,JKT)
c    get parameters
       JCH = ID(JKT+6)
       NCH = ID(JKT+7)
       NRW = ID(JKT+9)
       NFC = ID(JKT+10)
       NVC = ID(JKT+11)
       JRV = ID(JKT+12+NFC+NVC)
c    get ESC
       ESC = CD(JCH+NCH)
c    get index of table column headers string
       JCCHS = JCH + NCH + 1
c
c *  construct column headers line by line
c
c    initialize display line number
       LND = 1
c
c *  loop point for scan of column headers string for the current display line
c
c    initialize index at start of column headers string
2      JC = JCCHS
c    assume no data for this line
       OUT = .FALSE.
c    load blanks to allow space for row index
       DO LD=1,3
           D(LD) = ' '
           ENDDO
       LD = 3
c    get data for each column
       DO J=1,NFC+NVC
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
c        initialize last blank for full line
           JCLB = JC1 + NDISP - 1
           LELB = NDISP
c        find ESC
           DO LX=1,NDISP+1
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
               ENDDO
c        error
           CALL ERRFZZ('@','DTBHZZ error; column head '//
     ;                 'too long or not ESC terminated.@@')
c        line end found; check for display
10         IF (LNX.EQ.LND)  THEN
c            get count of characters to display
               NCDJ = LE - 1
c            determine number of leading and trailing blanks
               NLB = (NCWJ - NDISP)/2
               NTB = NCWJ - NLB - NCDJ
c            load the leading blanks
               DO L=1,NLB
                   LD = LD + 1
                   D(LD) = ' '
                   ENDDO
c            load the text
               JX = JC1
               DO L=1,NCDJ
                   LD = LD + 1
                   D(LD) = CD(JX)
                   JX = JX + 1
                   ENDDO
c            load the trailing blanks
               DO L=1,NTB
                   LD = LD + 1
                   D(LD) = ' '
                   ENDDO
               ENDIF
c        advance to next character
           JC = JC + 1
c        check for end of column data
           IF (CD(JC).EQ.ESC)  THEN
c            this column's data exhausted
               IF (LNX.LT.LND) THEN
c                current line absent this column; load blanks
                   DO L=1,NCWJ
                       LD = LD + 1
                       D(LD) = ' '
                       ENDDO
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
29         ENDDO
c
c    all columns done for this line
       IF (LND.EQ.1)  THEN
c        draw top line
           WRITE (*,32) ('-',L=1,LDTOT)
32         FORMAT (5X,75A1)
           ENDIF
c    check for display
       IF (OUT) THEN
c        display the line
           WRITE (*,32) (D(L),L=1,LD)
c        set for next column header line
           LND = LND + 1
c        go work on next line
           GOTO 2
           ENDIF
c    draw bottom line
       WRITE (*,32) ('-',L=1,LDTOT)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DTABZZ(JKT)
c
c      Display table having pointer JKT with line index on left.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (RD(1),RU)
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
       CHARACTER*16    TEMP
c------------------------------------------------------------------------------
c    display headers
       CALL DTBHZZ(JKT)
c    get parameters
       KS = ID(JKT+3)
       NRW = ID(JKT+9)
       NFC = ID(JKT+10)
       NVC = ID(JKT+11)
       JRV = ID(JKT+12+NFC+2*NVC)
       NTC = NFC + NVC
c    get display width
       LDTOT = 3
       DO J=1,NTC
c        get width of column J
           NCWJ = ID(JKT+11+J)
           LDTOT = LDTOT + NCWJ
           ENDDO
c    write the data
       DO I=1,NRW
c        load index and trailing space
           WRITE (LINE,12) I
12         FORMAT (I8,2X)
           ML = 10
c        initialize displayed character count
           LD = 5
           DO J=1,NTC
c            get width of column J
               NCWJ = ID(JKT+11+J)
c            load the number
               JV = JRV + I - 1 + NRW*(J-1)
c            check data
               IF (RD(JV).EQ.RU)  THEN
c                    unset; load blanks
                       DO L=1,NCWJ
                           CALL LOADZZ(' ',1,LINE,ML)
                           ENDDO
                   ELSE
c                    set; load number
                       WRITE (TEMP,26) RD(JV)
26                     FORMAT(1X,1PE14.6)
                       CALL LOADZZ(TEMP,15,LINE,ML)
                       LD = LD + 14
c                    load the trailing blanks
                       NTB = NCWJ - 14
                       DO L=1,NTB
                           CALL LOADZZ(' ',1,LINE,ML)
                           ENDDO
                   ENDIF
               ENDDO
c        write the line
           CALL POSTZZ(LINE,ML,.FALSE.)
           ENDDO
c    draw bottom line
       WRITE (*,52) ('-',L=1,LDTOT)
52     FORMAT (5X,75A1)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DOBJZZ(I,JKT)
c
c      Displays the header or value of the object having pointer JKT,
c      following I.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    get type code
       KT = ID(JKT)
       IF (KT.LE.6)  THEN
c            group
               CALL DGHZZ(I,JKT)
           ELSEIF (KT.EQ.11)  THEN
c            character variable
               CALL DCVZZ(I,JKT)
           ELSEIF (KT.EQ.12)  THEN
c            integer variable
               CALL DIVZZ(I,JKT)
           ELSEIF (KT.EQ.13)  THEN
c            real variable
               CALL DRVZZ(I,JKT)
           ELSEIF (KT.EQ.14)  THEN
c            file name
               CALL DFNZZ(I,JKT)
           ELSEIF (KT.EQ.20)  THEN
c            option
               CALL DOPZZ(I,JKT)
           ELSEIF (KT.EQ.21)  THEN
c            help option
               CALL DHOZZ(I,JKT)
           ELSE
c            error
               CALL ERRFZZ('@','DOBJZZ error.@@')
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DOPZZ(I,JKT)
c
c      Displays the option having pointer JKT, following I>0.  If the option
c      is selected (NOPC=1), the I is followed by a * and any # values
c      are displayed. Otherwise the # names are displayed.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96 LINE
c------------------------------------------------------------------------------
c    check call
       IF (I.LE.0) CALL ERRFZZ('@','@DOPZZ call with I=0@@')
c    check for group
       KT = ID(JKT)
       IF (KT.LE.5)  THEN
c        option is a group
           CALL DGHZZ(I,JKT)
           RETURN
           ENDIF
c    not a group
       ML = 0
       CALL LOPZZ(I,JKT,LINE,ML)
       CALL POSTZZ(LINE,ML,.FALSE.)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE LOPZZ(I,JKT,LINE,ML)
c
c      Loads the option having pointer JKT, following I, to LINE(ML+1).
c      If I=0 the option is loaded without the I and *, as a designator
c      (used in headers for exclusive options).
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(1),IU)
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
       CHARACTER*72    TEMP
c------------------------------------------------------------------------------
       CHARACTER*1 C
c------------------------------------------------------------------------------
c    variable formats
       CHARACTER*6     FORMC
       CHARACTER*4     FORMI
       CHARACTER*10    FORMR
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
c    determine status of the option
       JKTP = ID(JKT+1)
       KTP = ID(JKTP)
       IF (KTP.EQ.3)  THEN
c            parent is exclusive option group
               NOPCP = ID(JKTP+9)
               IF (NOPCP.EQ.IU) THEN
c                    nothing selected
                       NOPC = 0
                   ELSE
c                    check selection
                       JKTC = ID(JKTP+9+NOPCP)
                       IF (JKT.EQ.JKTC) THEN
c                            this is seleted option
                               NOPC = 1
                           ELSE
c                            not selected option
                               NOPC = 0
                           ENDIF
                   ENDIF
           ELSEIF (KTP.EQ.4)  THEN
c            parent is inclusive option group
               IF (I.EQ.0)  CALL ERRFZZ('@','DOPZZ error; '//
     ;                          'improper designator.@@')
               NOPC = ID(JKTP+9+I)
               IF (NOPC.LT.0) NOPC = 0
           ELSE
c            error
               CALL ERRFZZ('@','DOPZZ error; improper parent.@@')
           ENDIF
c    load the precursor
       IF (I.GT.0)  THEN
c           write integer
               WRITE (TEMP,2) I
2              FORMAT(I4)
               CALL LOADZZ(TEMP,4,LINE,ML)
               IF (NOPC.EQ.1)  THEN
c                    item is selected
                       CALL LOADZZ('*',1,LINE,ML)
                   ELSE
c                    item not selected
                       CALL LOADZZ(' ',1,LINE,ML)
                   ENDIF
           ELSE
c            designator
               CALL LOADZZ(': ',2,LINE,ML)
           ENDIF
c    check for group as option
       IF (KT.LE.5)  THEN
c        option is a group
           JCH = ID(JKT+6)
           NCH = ID(JKT+7)
           CALL LOADZZ(CD(JCH),NCH,LINE,ML)
c        check for valid designator
           JKTD = ID(JKT+4)
           IF (JKTD.NE.0)  THEN
               KSD = ID(JKTD+3)
               IF (KSD.GT.0)  THEN
c                add designator
                   JCSD = ID(JKTD+4)
                   NCSD = ID(JKTD+5)
                   CALL LOADZZ(': ',2,LINE,ML)
                   CALL LOADZZ(CD(JCSD),NCSD,LINE,ML)
                   ENDIF
               ENDIF
           RETURN
           ENDIF
c    check for option
       IF (KT.NE.20)   CALL ERRFZZ('@','@LOPZZ error; not option@@')
c    get parameters
       JCS = ID(JKT+4)
       NCS = ID(JKT+5)
c    initialize
       NV = 0
c    scan the option and load
       DO L=1,NCS
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
                           CALL LOADZZ(CD(JCN),NCN,LINE,ML)
                       ELSE
c                        option chosen; load the value
                           KTV = ID(JKTV)
c                        branch on type
                           IF (KTV.EQ.11) THEN
c                                character variable
                                   JCS = ID(JKTV+4)
                                   NCS = ID(JKTV+5)
                                   CALL LOADZZ(CD(JCS),NCS,LINE,ML)
                               ELSEIF (KTV.EQ.12) THEN
c                                    integer variable
                                       IV = ID(JKTV+7)
c                                    construct the format
                                       CALL CFIZZ(IV,FORMI,NW)
c                                    load
                                       WRITE (TEMP,FORMI) IV
                                       CALL LOADZZ(TEMP,NW,LINE,ML)
                               ELSEIF (KTV.EQ.13) THEN
c                                    real variable
                                       JRV = ID(JKTV+7)
                                       RV = RD(JRV)
c                                    construct the format
                                       CALL CFRZZ(RV,FORMR,NW)
c                                    load
                                       WRITE (TEMP,FORMR) RV
                                       CALL LOADZZ(TEMP,NW,LINE,ML)
                               ELSEIF (KTV.EQ.14) THEN
c                                    file name variable
                                       JCV = ID(JKTV+4) + ID(JKTV+5)
                                       NCV = ID(JKTV+6)
c                                    load
                                       CALL LOADZZ(CD(JCV),NCV,LINE,ML)
                               ELSE
c                                error
                                   CALL ERRFZZ('@','DOPZZ error;'//
     ;                                         ' not value.@@')
                               ENDIF
                       ENDIF
               ELSE
c                load the character
                   CALL LOADZZ(C,1,LINE,ML)
               ENDIF
           ENDDO
       RETURN
       END
c****************************************************************************
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
       EQUIVALENCE     (ID(10),JKTGOD)
c------------------------------------------------------------------------------
       CHARACTER*1     C
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96    LINE
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF ((KT.LT.1).AND.(KT.GT.6))  THEN
           CALL ERRFZZ('@','DGHZZ error.@@')
           ENDIF
c    branch on the group type
       IF ((KT.EQ.6).OR.(KT.EQ.21))  THEN
c            help group or option; get parameters
               JCH = ID(JKT+3)
               NCH = ID(JKT+4)
           ELSE
c            not help group; get parameters
               JCH = ID(JKT+6)
               NCH = ID(JKT+7)
           ENDIF
c    check group for god
       IF (JKT.EQ.JKTGOD)  THEN
c        code group not option
           NOP = 0
           GOTO 10
           ENDIF
c    check group's parent for option group
       JKTP = ID(JKT+1)
       KTP = ID(JKTP)
       IF (KTP.EQ.3)  THEN
c            parent is exclusive option group
               NOPC = ID(JKTP+9)
               IF (NOPC.EQ.I)  THEN
c                    code option selected
                       NOP = 2
                   ELSE
c                    code option not selected
                       NOP = 1
                   ENDIF
           ELSEIF (KTP.EQ.4) THEN
c            parent is inclusive option group
               NOS = ID(JKTP+8)
               NOP = 1
               DO L=1,NOS
                   IF (ID(JKTP+9+L).EQ.1)  THEN
c                    option L is selected; is it the group?
                       IF (JKT.EQ.ID(JKTP+9+NOS+L))  THEN
c                        code group selected
                           NOP = 2
                           GOTO 10
                           ENDIF
                       ENDIF
                   ENDDO
           ELSE
c            parent is not an option group; code group not option
               NOP = 0
           ENDIF
c
c    load precursor
10     IF (I.EQ.0)  THEN
c            blank line
               WRITE (*,*) ' '
c            start display without integer as header for children
               WRITE (LINE,12)
12             FORMAT (1X)
               ML = 1
           ELSE
c            start display with integer
               IF (NOP.EQ.0)  THEN
c                    not option
                       C = ' '
                   ELSEIF (NOP.EQ.1)  THEN
c                     unselected option
                       C = ' '
                   ELSEIF (NOP.EQ.2) THEN
c                     selected option
                       C = '*'
                   ELSE
                       CALL ERRFZZ('@','DGHZZ option error.@@')
                   ENDIF
               WRITE (LINE,14) I,C
14             FORMAT(I4,A)
               ML = 5
           ENDIF
c    load header
       CALL LOADZZ(CD(JCH),NCH,LINE,ML)
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
c                load option
                   CALL LOPZZ(0,JKTD,LINE,ML)
                   ENDIF
c            check for group
               IF (KTD.EQ.10) THEN
c                load group designator
                   CALL LGDZZ(JKTD,LINE,ML)
                   ENDIF
               ENDIF
           ENDIF
c    display
       CALL POSTZZ(LINE,ML,.FALSE.)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE LGDZZ(JKTD,LINE,ML)
c
c      Loads the group designator having pointer JKTD to LINE(ML+1).
c      Returns ML as index of last load point.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
c------------------------------------------------------------------------------
       CALL LOADZZ(': ',2,LINE,ML)
       JCD = ID(JKTD+4)
       NCD = ID(JKTD+5)
       CALL LOADZZ(CD(JCD),NCD,LINE,ML)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DHOZZ(I,JKT)
c
c      Displays the help option having pointer JKT, following I.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96    LINE
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.21) CALL ERRFZZ('@','DHOZZ error; not option.@@')
       WRITE (LINE,2) I
2      FORMAT (I4,1X)
       ML = 5
c    get parameters
       JCS = ID(JKT+3)
       NCS = ID(JKT+4)
       CALL LOADZZ(CD(JCS),NCS,LINE,ML)
       CALL POSTZZ(LINE,ML,.FALSE.)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  SFIGZZ(JKT,JKTX)
c
c      Selects item from the item group having pointer JKT, returning the item
c      pointer in JKTX. Returns JKTX=0 if none selected.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*80    LINE
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.1) CALL ERRFZZ('@','DIGZZ error.@@')
c    display the header
       CALL DGHZZ(0,JKT)
c    get parameters
       NIS = ID(JKT+8)
c    display the objects
       DO I=1,NIS
           JKTI = ID(JKT+8+I)
           CALL DOBJZZ(I,JKTI)
           ENDDO
c    add help if present
       JKTH = ID(JKT+5)
       IF (JKTH.GT.0)  THEN
c            display object
               IX = NIS + 1
               CALL DOBJZZ(IX,JKTH)
           ELSE
c            no help
               IX = NIS
           ENDIF
       ML=0
       CALL LOADZZ(' Enter item number to inspect or change '//
     ;    '(0 if none) > ',54,LINE,ML)
       CALL POSTZZ(LINE,ML,.TRUE.)
c    get response
       I = NOPZZ(IX)
c    check for none
       IF (I.EQ.0)  THEN
               JKTX = 0
c            check status of items
               DO 19 I=1,NIS
                   JKTI = ID(JKT+8+I)
                    IF (ID(JKTI+3).EQ.0)  THEN
c                    return with incomplete data
                       RETURN
                       ENDIF
19                 CONTINUE
c            all items complete; set status of items group
               ID(JKT+3) = 2
c            check for items group designation
               JKTD = ID(JKT+4)
               IF (JKTD.NE.0)  THEN
c                get items group designation
                   CALL GGDZZ(JKTD)
                   ENDIF
           ELSE
c           set redirection
               IF (I.LE.NIS)  THEN
c                    item
                       JKTX = ID(JKT+8+I)
                   ELSE
c                     help
                       JKTX = JKTH
                   ENDIF
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE GGDZZ(JKT)
c
c      Gets new group designation having pointer JKT.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE     (ID(10),JKTGOD)
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*32    LINE
c    character format
       CHARACTER*6     FORMC
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.10)
     ;     CALL ERRFZZ('@','GGDZZ error; not group designation.@@')
c    check that it is unset
       KS = ID(JKT+3)
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
     ;     CALL ERRFZZ('@','GGDZZ error; parent not group.@@')
c    get parent header data
       JCH = ID(JKTP+6)
       NCH = ID(JKTP+7)
c    display
1      WRITE (*,2) (CD(L),L=JCH,JCH+NCH-1),'.'
2      FORMAT (/' New data completed for ',60A1)
       WRITE(*,4) ('*',J=1,NCD)
4      FORMAT (' Enter your designation of this data set ',
     ;            'under the stars:'/1X,80A1)
       ML = 0
       CALL LOADZZ(' ',1,LINE,ML)
       CALL POSTZZ(LINE,ML,.TRUE.)
c    create read format
       CALL CFCZZ(NCD,FORMC)
c    get response
       READ(*,FORMC,ERR=10)   (CD(J),J=JCD,JCD+NCD-1)
8      FORMAT (80A1)
c    register the status changes
       ID(JKT+3) = 2
c    clear ancestor designations
       IF (JKTP.NE.JKTGOD)  THEN
           JKTPP= ID(JKTP+1)
           CALL SGDUZZ(JKTPP)
           ENDIF
       RETURN
c
c    read error
10     WRITE (*,12)
12     FORMAT (/' Character read error; try again.')
       GOTO 1
       END
c******************************************************************************
c
       SUBROUTINE GCVZZ(JKT)
c
c      Gets new character value having pointer JKT.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       CHARACTER*1 FORMC(6)
c    (nnA1)
       DATA FORMC /'(','n','n','A','1',')'/
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
c------------------------------------------------------------------------------
       CHARACTER*1     INPUT(80)
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.11)
     ;     CALL ERRFZZ('@','GCVZZ error; not character variable.@@')
c    get parameters
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
       JCS = JCN + NCN
       NCS = ID(JKT+6)
c    load prompt
1      ML = 0
       CALL LOADZZ(' Enter ',7,LINE,ML)
       CALL LOADZZ(CD(JCN),NCN,LINE,ML)
       CALL LOADZZ(' under the stars',16,LINE,ML)
c    issue the prompt
       CALL POSTZZ(LINE,ML,.FALSE.)
       WRITE (*,4) ('*',N=1,NCS)
4      FORMAT (1X,80A1)
       CALL POSTZZ(' ',1,.TRUE.)
c    set read format
       CALL CFCZZ(NCS,FORMC)
c    get value
       READ (*,FORMC,END=20)  (INPUT(I),I=1,NCS)
c    check for blanks
       DO I=1,NCS
           IF (INPUT(I).NE.' ')  GOTO 10
           ENDDO
c    exit if no data
       RETURN
c    accept value
10     DO I=1,NCS
           CD(JCS+I-1) = INPUT(I)
           ENDDO
c    register the status change
       ID(JKT+3) = 2
c    clear antecedant designators
       JKTP = ID(JKT+1)
       CALL SGDUZZ(JKTP)
       RETURN
c
c    read error
20     WRITE (*,*) ' Character variable entry error; try again.'
       GOTO 1
       END
c******************************************************************************
c
       LOGICAL FUNCTION REALZZ(STRING,VALUE)
c
c      Scans STRING to check for real number.  Returns .TRUE. and VALUE
c      if set, otherwise .FALSE. with no change in VALUE.
c----------------------------------------------------------------------
       PARAMETER (IMAX=16)
       CHARACTER*1  STRING(IMAX),DIGIT(10)
       LOGICAL NUMOK,EXOK
c----------------------------------------------------------------------
c    maximum exponent
       DATA EXMAX/30/
c----------------------------------------------------------------------
c    digits
       DATA    DIGIT /'0','1','2','3','4','5','6','7','8','9'/
c----------------------------------------------------------------------
c    scan state control
c      KSTATE = 0  scan has not yet found a non-blank; seeking sign
c      KSTATE = 1  collecting digits before decimal point
c      KSTATE = 2  collecting digits after decimal point
c      KSTATE = 3  seeking exponent sign
c      KSTATE = 4  collecting digits in exponent
c      KSTATE = 5  number completed
c----------------------------------------------------------------------
c    initializations
       KSTATE = 0
       SIGN = 1
       ESIGN = 1
       EX = 0
       EXOK = .TRUE.
       NUMOK = .FALSE.
       SUM = 0
c    scan the string
       DO I=1,IMAX
c        check for blank
           IF (STRING(I).EQ.' ')  THEN
                   IF (KSTATE.GT.0)  THEN
c                    set state complete
                       KSTATE = 5
                       ENDIF
               ELSEIF (KSTATE.EQ.5) THEN
c                error: non-blank after number complete
                   GOTO 90
c            check for +
               ELSEIF (STRING(I).EQ.'+') THEN
                   IF (KSTATE.EQ.0)  THEN
c                        set collecting before decimal point
                           KSTATE = 1
                       ELSEIF (KSTATE.EQ.3) THEN
c                        set collecting exponent
                           KSTATE = 4
                           EXOK = .FALSE.
                       ELSE
c                    error; improperly placed +
                           GOTO 90
                       ENDIF
c            check for -
               ELSEIF (STRING(I).EQ.'-') THEN
                   IF (KSTATE.EQ.0)  THEN
c                        set for collecting negative before decimal point
                           SIGN = - 1
                           KSTATE = 1
                       ELSEIF (KSTATE.EQ.3) THEN
c                        set for collecting negative exponent
                           ESIGN = - 1
                           KSTATE = 4
c                        set exponent incomplete
                           EXOK = .FALSE.
                       ELSE
c                    error; improperly placed -
                           GOTO 90
                       ENDIF
c            check for .
               ELSEIF (STRING(I).EQ.'.')  THEN
                   IF (KSTATE.GT.1)  THEN
c                    error; second decimal point
                       GOTO 90
                       ENDIF
c                set state after decimal point
                   KSTATE = 2
c                initialize factor
                   FACTOR = 1
c            check for exponent
               ELSEIF ((STRING(I).EQ.'e').OR.(STRING(I).EQ.'E')) THEN
c                check state
                   IF (KSTATE.LE.2) THEN
c                        set state seeking exponent sign
                           KSTATE = 3
c                        set exponent incomplete
                           EXOK = .FALSE.
c                        check for nothing before e
                           IF (SUM.EQ.0)  SUM = 1
                       ELSE
c                        error; second e
                           GOTO 90
                       ENDIF
               ELSE
c                check digits
                   DO L=1,10
                       IF (STRING(I).EQ.DIGIT(L))  THEN
                           NUMOK = .TRUE.
                           IF (KSTATE.LE.1)  THEN
c                                add contribution to sum before decimal
                                   SUM = 10*SUM + L - 1
                                   KSTATE = 1
                               ELSEIF (KSTATE.EQ.2) THEN
c                                add contribution after decimal
                                   FACTOR = 0.1*FACTOR
                                   SUM = SUM + FACTOR*(L - 1)
                               ELSEIF (KSTATE.LE.4)  THEN
c                                set for collecting exponent digits
                                   KSTATE = 4
c                                add contribution to exponent
                                   EX = 10*EX + L - 1
c                                check for exponent overflow
                                   IF (EX.GT.EXMAX) GOTO 90
c                                exponent now ok
                                   EXOK = .TRUE.
                               ENDIF
c                        digit found and processed
                           GOTO 12
                           ENDIF
c                    continue digit scan
                       ENDDO
c                 error; illegal character
                   GOTO 90
12             ENDIF
           ENDDO
c    scan completed; check for completed exponent
       IF (EXOK.AND.NUMOK) THEN
c        compute the value
           VALUE = SIGN*SUM*10**(ESIGN*EX)
           REALZZ = .TRUE.
           RETURN
           ENDIF
c    error
90     REALZZ = .FALSE.
       RETURN
       END
c**********************************************************************
c
       LOGICAL FUNCTION INTZZ(STRING,IVALUE)
c
c      Scans STRING to check for integer number.  Returns .TRUE. and IVALUE
c      if set, otherwise .FALSE. with no change in IVALUE.
c----------------------------------------------------------------------
       PARAMETER (IMAX=16)
       CHARACTER*1  STRING(IMAX),DIGIT(10)
       LOGICAL NUMOK
c----------------------------------------------------------------------
c    digits
       DATA    DIGIT /'0','1','2','3','4','5','6','7','8','9'/
c----------------------------------------------------------------------
c    scan state control
c      KSTATE = 0  scan has not yet found a non-blank; seeking sign
c      KSTATE = 1  collecting digits before decimal point
c      KSTATE = 2  collecting digits before decimal point
c----------------------------------------------------------------------
c    initializations
       KSTATE = 0
       ISIGN = 1
       NUMOK = .FALSE.
       ISUM = 0
c    scan the string
       DO I=1,IMAX
c        check for blank
           IF (STRING(I).EQ.' ')  THEN
                   IF (KSTATE.GT.0)  THEN
c                    set state complete
                       KSTATE = 2
                       ENDIF
               ELSEIF (KSTATE.EQ.2) THEN
c                error: non-blank after number complete
                   GOTO 90
c            check for +
               ELSEIF (STRING(I).EQ.'+') THEN
                   IF (KSTATE.EQ.0)  THEN
c                        set for collecting digits
                           KSTATE = 1
                       ELSE
c                    error; improperly placed +
                           GOTO 90
                       ENDIF
c            check for -
               ELSEIF (STRING(I).EQ.'-') THEN
                   IF (KSTATE.EQ.0)  THEN
c                        set for collecting digits
                           ISIGN = - 1
                           KSTATE = 1
                       ELSE
c                    error; improperly placed -
                           GOTO 90
                       ENDIF
               ELSE
c                check digits
                   DO L=1,10
                       IF (STRING(I).EQ.DIGIT(L))  THEN
                           NUMOK = .TRUE.
                           IF (KSTATE.LE.1)  THEN
c                                add contribution to sum
                                   ISUM = 10*ISUM + L - 1
                                   KSTATE = 1
                               ENDIF
c                        digit found and processed
                           GOTO 12
                           ENDIF
c                    continue digit scan
                       ENDDO
c                 error; illegal character
                   GOTO 90
12             ENDIF
           ENDDO
c    scan completed; check for completed exponent
       IF (NUMOK) THEN
c        compute the value
           IVALUE = ISIGN*ISUM
           INTZZ = .TRUE.
           RETURN
           ENDIF
c    error
90     INTZZ = .FALSE.
       RETURN
       END
c**********************************************************************
c
       SUBROUTINE GIVZZ(JKT)
c
c      Gets new integer value having pointer JKT.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    integer format
       CHARACTER*4 FORMI
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
       CHARACTER*32    TEMP
c------------------------------------------------------------------------------
c    string read
       CHARACTER*16    STRING
       LOGICAL         INTZZ
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.12)
     ;     CALL ERRFZZ('@','GIVZZ error; not integer variable.@@')
c    get parameters
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
       KR = ID(JKT+6)
       IVMIN = ID(JKT+8)
       IVMAX = ID(JKT+9)
c    construct the prompt
1      ML = 0
       CALL LOADZZ(' Enter ',7,LINE,ML)
c    load name
       CALL LOADZZ(CD(JCN),NCN,LINE,ML)
       CALL LOADZZ(' > ',3,LINE,ML)
c    issue the prompt
       CALL POSTZZ(LINE,ML,.TRUE.)
c    read value
       READ(*,2)  STRING
2      FORMAT(A)
       IF (.NOT.INTZZ(STRING,IV)) THEN
c        read error
           WRITE (*,*) ' Expecting integer; try again.'
           GOTO 1
           ENDIF
c    get restriction bits
       KR8 = KR/8
       KR4 = (KR - 8*KR8)/4
       KR2 = (KR - 8*KR8 - 4*KR4)/2
       KR1 = KR - 8*KR8 - 4*KR4 - 2*KR2
c    check restrictions
       IF (KR8.GT.0)  THEN
c        check maximum
           IF (IV.GT.IVMAX)  THEN
               CALL CFIZZ(IVMAX,FORMI,NW)
               ML = 0
               CALL LOADZZ(' The maximum is ',16,LINE,ML)
               CALL CFIZZ(IVMAX,FORMI,NW)
               WRITE (TEMP,FORMI) IVMAX
               CALL LOADZZ(TEMP,NW,LINE,ML)
               CALL POSTZZ(LINE,ML,.FALSE.)
               GOTO 1
               ENDIF
           ENDIF
       IF (KR4.GT.0)  THEN
c        check minimum
           IF (IV.LT.IVMIN)  THEN
               CALL CFIZZ(IVMIN,FORMI,NW)
               ML = 0
               CALL LOADZZ(' The minimum is ',16,LINE,ML)
               CALL CFIZZ(IVMIN,FORMI,NW)
               WRITE (TEMP,FORMI) IVMIN
               CALL LOADZZ(TEMP,NW,LINE,ML)
               CALL POSTZZ(LINE,ML,.FALSE.)
               GOTO 1
               ENDIF
           ENDIF
       IF (KR1.GT.0)  THEN
c        sign restricted
           IF (KR2.GT.0) THEN
c                negative definite
                   IF (IV.GE.0)  THEN
                       WRITE (*,*) ' Must be <0'
                       GOTO 1
                       ENDIF
               ELSE
c                positive definite
                   IF (IV.LE.0)  THEN
                       WRITE (*,*) ' Must be >0'
                       GOTO 1
                       ENDIF
               ENDIF
           ENDIF
c    value acceptable
       ID(JKT+7) = IV
c    register the status change
       ID(JKT+3) = 2
c    clear antecedent designators
       JKTP = ID(JKT+1)
       CALL SGDUZZ(JKTP)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE GRVZZ(JKT)
c
c      Gets new real value having pointer JKT.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    real format
       CHARACTER*10 FORMR
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
       CHARACTER*32    TEMP
c------------------------------------------------------------------------------
c    string read
       CHARACTER*16    STRING
       LOGICAL         REALZZ
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.13)
     ;     CALL ERRFZZ('@','GRVZZ error; not real variable.@@')
c    get parameters
       JCN = ID(JKT+4)
       NCN = ID(JKT+5)
       KR = ID(JKT+6)
       JRV = ID(JKT+7)
       RVMIN = RD(JRV+1)
       RVMAX = RD(JRV+2)
c    construct the prompt
1      ML = 0
       CALL LOADZZ(' Enter ',7,LINE,ML)
       CALL LOADZZ(CD(JCN),NCN,LINE,ML)
       CALL LOADZZ(' > ',3,LINE,ML)
c    issue the prompt
       CALL POSTZZ(LINE,ML,.TRUE.)
c    read
       READ(*,2)  STRING
2      FORMAT(A)
       IF (.NOT.REALZZ(STRING,RV))  THEN
c        read error
           WRITE(*,*) ' Expecting number; try again.'
           GOTO 1
           ENDIF
c    get restriction bits
       KR8 = KR/8
       KR4 = (KR - 8*KR8)/4
       KR2 = (KR - 8*KR8 - 4*KR4)/2
       KR1 = KR - 8*KR8 - 4*KR4 - 2*KR2
c    check restrictions
       IF (KR8.GT.0)  THEN
c        check maximum
           IF (RV.GT.RVMAX)  THEN
               ML = 0
               CALL LOADZZ(' The maximum is ',16,LINE,ML)
               CALL CFRZZ(RVMAX,FORMR,NW)
               WRITE (TEMP,FORMR) RVMAX
               CALL LOADZZ(TEMP,NW,LINE,ML)
               CALL POSTZZ(LINE,ML,.FALSE.)
               GOTO 1
               ENDIF
           ENDIF
       IF (KR4.GT.0)  THEN
c        check minimum
           IF (RV.LT.RVMIN)  THEN
               CALL LOADZZ(' The maximum is ',16,LINE,ML)
               CALL CFRZZ(RVMIN,FORMR,NW)
               WRITE (TEMP,FORMR) RVMIN
               CALL LOADZZ(TEMP,NW,LINE,ML)
               CALL POSTZZ(LINE,ML,.FALSE.)
               GOTO 1
               ENDIF
           ENDIF
       IF (KR1.GT.0)  THEN
c        sign restricted
           IF (KR2.GT.0) THEN
c                negative definite
                   IF (RV.GE.0)  THEN
                       WRITE (*,*) ' Must be <0'
                       GOTO 1
                       ENDIF
               ELSE
c                positive definite
                   IF (RV.LE.0)  THEN
                       WRITE (*,*) ' Must be >0'
                       GOTO 1
                       ENDIF
               ENDIF
           ENDIF
c    value acceptable
       RD(JRV) = RV
c    register the status change
       ID(JKT+3) = 2
c    clear antecedent designators
       JKTP = ID(JKT+1)
       CALL SGDUZZ(JKTP)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE GFNZZ(JKT)
c
c      Gets file name having pointer JKT. If cancelled, no changes made.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       CHARACTER*32        NAME,ABORT
       CHARACTER*1         EXT(3)
       DATA    ABORT/'                               '/
c------------------------------------------------------------------------------
       LOGICAL FOUND,YESZZ
c------------------------------------------------------------------------------
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
10     CALL GFNEZZ(EXT,NAME)
       IF (NAME.EQ.ABORT)  GOTO 30
c    check file
       INQUIRE(FILE=NAME,EXIST=FOUND)
c    check for file read
       IF ((KRW.EQ.1).AND.(.NOT.FOUND)) THEN
c       file does not exist
           IF (YESZZ('File not found. Do you want another?')) THEN
                   GOTO 10
               ELSE
                   GOTO 30
               ENDIF
           ENDIF
c    check for file write
       IF ((KRW.EQ.2).AND.FOUND) THEN
c       file already exists
           IF (YESZZ('File exists Do you want to replace it?')) THEN
                   GOTO 20
               ELSE
                   IF (YESZZ('Do you want another?')) THEN
                           GOTO 10
                       ELSE
                           GOTO 30
                       ENDIF
               ENDIF
           ENDIF
c    load name
20     CALL LSTRZZ(NAME,NCF,CD(JCF))
c    register the status change
       ID(JKT+3) = 2
c    clear antecedent designators
       JKTP = ID(JKT+1)
       CALL SGDUZZ(JKTP)
       RETURN
c   aborted; set value unset
30     ID(JKT+3) = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE GTBLZZ(I,JKT)
c
c      Gets data for line I of table having pointer JKT.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       PARAMETER (NVMAX=10)
       DIMENSION V(NVMAX)
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
       CHARACTER*16    TEMP
c------------------------------------------------------------------------------
c    get parameters
       NRW = ID(JKT+9)
       NFC = ID(JKT+10)
       NVC = ID(JKT+11)
       JRV = ID(JKT+12+NFC+2*NVC)
c    check column count
       IF (NVC.GT.NVMAX)  THEN
           WRITE (*,1)
1          FORMAT (' Too many table columns.')
           CALL PAUSZZ(0)
           CALL STOPZZ
           ENDIF
c    load index and trailing space
2      WRITE (LINE,4) I
4      FORMAT(I8,2X)
       ML = 10
c    do the frozen columns
       DO J=1,NFC
c        get width of column J
           NCWJ = ID(JKT+11+J)
c        load the number
           JV = JRV + I - 1 + NRW*(J-1)
           WRITE (TEMP,6) RD(JV)
6          FORMAT(1PE14.6)
           CALL LOADZZ(TEMP,14,LINE,ML)
c        loade the trailing blanks
           NTB = NCWJ - 14
           DO L=1,NTB
               CALL LOADZZ(' ',1,LINE,ML)
               ENDDO
           ENDDO
c   load pointer
       CALL LOADZZ(' > ',3,LINE,ML)
c    write the prompt
       CALL POSTZZ(LINE,ML,.TRUE.)
c    get the data
10     READ(*,*,ERR=20) (V(J),J=1,NVC)
c    check the data for column conditions
       DO J=1,NVC
           KR = ID(JKT+11+NFC+NVC+J)
c        get restriction bits
           KR8 = KR/8
           KR4 = (KR - 8*KR8)/4
           KR2 = (KR - 8*KR4 - 4*KR4)/2
           KR1 = KR - 8*KR4 - 4*KR4 - 2*KR2
c        check restrictions
           IF (KR8.GT.0)  THEN
c            check maximum
               VMAX = RD(JRV-1+(NFC+NVC)*NRW+NVC+J)
               IF (V(J).GT.VMAX)  THEN
                   WRITE (*,11) J+NFC,VMAX
11                 FORMAT (' The maximum value in column ',I2,' is',
     ;                     1PE14.6)
                           WRITE (*,30)
                   GOTO 2
                   ENDIF
              ENDIF
          IF (KR4.GT.0)  THEN
c           check minimum
               VMIN = RD(JRV-1+(NFC+NVC)*NRW+J)
               IF (V(J).LT.VMIN)  THEN
                   WRITE (*,12) J+NFC,VMIN
12                 FORMAT (' The minimum value in column ',I2,' is',
     ;                     1PE14.6)
                           WRITE (*,30)
                   GOTO 2
                   ENDIF
              ENDIF
           IF (KR1.GT.0)  THEN
c           sign restricted
               IF (KR2.GT.0) THEN
c                    negative definite
                       IF (V(J).GE.0)  THEN
                           WRITE (*,13)  J+NFC
13                         FORMAT(' The value in column ',I2,
     ;                            ' must be < 0')
                           WRITE (*,30)
                           GOTO 2
                           ENDIF
                   ELSE
c                    positive definite
                       IF (V(J).LE.0)  THEN
                           WRITE (*,14) J+NFC
14                         FORMAT(' The value in column ',I2,
     ;                            ' must be > 0')
                           WRITE (*,30)
                           GOTO 2
                           ENDIF
                   ENDIF
               ENDIF
           ENDDO
c    column values acceptable
       DO J=1,NVC
           JV = JRV - 1 + NFC*NRW + I + NRW*(J-1)
           RD(JV) = V(J)
           ENDDO
       RETURN
c
c    entry error
20     WRITE (*,*) ' Entry error; try again.'
       GOTO 10
30     FORMAT (' Please redo the line.')

       END
c******************************************************************************
c
       SUBROUTINE GFTBZZ(JKT)
c
c      Gets user input to full table having pointer JKT.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    initial instructions
       WRITE (*,2)
2      FORMAT (/' Enter the table data line-by-line;',
     ;          ' columns need not be aligned.'/
     ;          ' You can correct entry errors after the',
     ;          ' full table is loaded.'/)
c    display header
       CALL DTBHZZ(JKT)
c    get parameters
       NRW = ID(JKT+9)
c    get data line by line
       DO 9 I=1,NRW
           CALL GTBLZZ(I,JKT)
9          CONTINUE
c    done; set status
       ID(JKT+3) = 2
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE GCTBZZ(JKT,JKTX)
c
c      Gets changes to table having pointer JKT. Returns JKTX = JKTH if
c      help is requested, otherwise JKTX = 0.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE     (ID(10),JKTGOD)
       EQUIVALENCE     (RD(1),RU)
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96    LINE
c------------------------------------------------------------------------------
c    get parameters                       `
       JKTP = ID(JKT+1)
       JKTD = ID(JKT+4)
       JKTH = ID(JKT+5)
       JKTA = ID(JKT+8)
       NRW = ID(JKT+9)
       NFC = ID(JKT+10)
       NVC = ID(JKT+11)
       JRV = ID(JKT+12+NFC+2*NVC)
c
c    display the table
2      CALL CLPWZZ
       CALL DTABZZ(JKT)
3      WRITE (*,4)
4      FORMAT (/' Option:')
       IF (ID(JKT+3).NE.0)  THEN
c           table is loaded
              WRITE(*,5)
5             FORMAT ('   I  (integer) change row I')
              NALL = NRW + 1
           ELSE
c           table is empty
              NALL = 1
           ENDIF
       WRITE (*,6) NALL
6      FORMAT (I4,' change entire table')
       NMAX = NALL
c    check for table attachment
       IF (JKTA.NE.0)  THEN
           NAT = NMAX + 1
           NMAX = NAT
           CALL DOBJZZ(NAT,JKTA)
           ENDIF
c    check for help availability
       IF (JKTH.NE.0)  THEN
           NHELP = NMAX + 1
           NMAX = NHELP
           CALL DGHZZ(NHELP,JKTH)
           ENDIF
c    construct the prompt
       ML = 0
       CALL LOADZZ(' Enter item to change (0 for none) > ',37,LINE,ML)
c    write the prompt
       CALL POSTZZ(LINE,ML,.TRUE.)
c    get the response
       I = NOPZZ(NMAX)
       IF (I.EQ.NALL)  THEN
c            get full table
               CALL GFTBZZ(JKT)
c            set table as full and new
               ID(JKT+3) = 2
           ELSEIF (I.EQ.0)  THEN
c            check that array is complete
               DO 15 IX=1,NRW
                   DO 13 JX=1,NVC
c                    check for unset element
                       JV = JRV + IX - 1 + NRW*(JX-1)
                       IF (RD(JV).EQ.RU)  THEN
c                        unset element; set table as unset
                           ID(JKT+3) = 0
                           JKTX = 0
                           RETURN
                           ENDIF
13                     CONTINUE
15                 CONTINUE
c            get designation
               IF (JKTD.NE.0)  CALL GGDZZ(JKTD)
c            set to go on
               JKTX = 0
               RETURN
           ELSEIF (((I.GT.0).AND.(I.LT.NALL)).AND.
     ;                   (ID(JKT+3).GT.0))  THEN
c            get row I
               WRITE (*,18)
18             FORMAT (' Enter the missing columns;',
     ;                 ' alignment not required.')
               CALL GTBLZZ(I,JKT)
               ID(JKT+3) = 2
           ELSEIF (I.EQ.NAT) THEN
c            set to gO process attachment
               JKTX = JKTA
               RETURN
           ELSEIF (I.EQ.NHELP) THEN
c            get help
               JKTX = JKTH
               RETURN
           ENDIF
c    clear affected designations
       CALL SGDUZZ(JKT)
       GOTO 2
       END
c******************************************************************************
c
       SUBROUTINE GVZZ(JKT)
c
c      Gets value of object having pointer JKT.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    get type
       KT = ID(JKT)
c    get the new value
       IF (KT.EQ.10)  THEN
c            group designation
               CALL GGDZZ(JKT)
           ELSEIF (KT.EQ.11)  THEN
c            character variable
               CALL GCVZZ(JKT)
           ELSEIF (KT.EQ.12)  THEN
c            integer variable
               CALL GIVZZ(JKT)
           ELSEIF (KT.EQ.13)  THEN
c            real variable
               CALL GRVZZ(JKT)
           ELSEIF (KT.EQ.14)  THEN
c            file name
               CALL GFNZZ(JKT)
           ELSE
c            error
               CALL ERRFZZ('@','GVZZ error; not variable.@@')
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  GCVGZZ(JKT,JKTX)
c
c      Gets changes to values group having pointer JKT. Returns JKTX = JKTH
c      if help is required, otherwise JKTX = 0.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96    LINE
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.2) CALL ERRFZZ('@','GCVGZZ error; not values group.@@')
c    display the header
1      CALL CLPWZZ
       CALL DGHZZ(0,JKT)
c    get parameters
       NVS = ID(JKT+8)
c    display the objects and construct list
       DO 9 I=1,NVS
           JKTI = ID(JKT+8+I)
           CALL DOBJZZ(I,JKTI)
9          CONTINUE
c    add help if present
       JKTH = ID(JKT+5)
       IF (JKTH.GT.0)  THEN
c            display object
               IX = NVS + 1
               CALL DOBJZZ(IX,JKTH)
           ELSE
c            no help
               IX = NVS
           ENDIF
c    construct prompt
       ML = 0
       CALL LOADZZ(' Enter item number to change (0 if none) > ',43,
     ;          LINE,ML)
c    issue prompt
       CALL POSTZZ(LINE,ML,.TRUE.)
c    get response
       I = NOPZZ(IX)
c    check for none
       IF (I.EQ.0)  THEN
           JKTX = 0
c        check if all values are set
           DO 19 I=1,NVS
               JKTI = ID(JKT+8+I)
c            get the next unset value
               IF (ID(JKTI+3).EQ.0)  THEN
c                return with incomplete data
                   RETURN
                   ENDIF
19             CONTINUE
c        check for values group designator
           JKTD = ID(JKT+4)
c        set status
           ID(JKT+3) = 2
c            get values group designator
           IF (JKTD.NE.0)  THEN
               CALL GGDZZ(JKTD)
               ENDIF
           RETURN
           ENDIF
       IF (I.GT.NVS)  THEN
c        help
           JKTX = JKTH
           RETURN
           ENDIF
c    get the new value
20     JKTI = ID(JKT+8+I)
       CALL GVZZ(JKTI)
c    go around again
       GOTO 1
       END
c******************************************************************************
c
       SUBROUTINE  GCEOZZ(JKT,JKTX)
c
c      Gets changes to exclusive options having pointer JKT. Returns JKTX
c      as pointer to another group or help if selected, otherwise JKTX = 0.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(1),IU)
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96    LINE
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.3)
     ;     CALL ERRFZZ('@','GCEOZZ error; not exclusive options.@@')
c    display the header
1      CALL CLPWZZ
       CALL DGHZZ(0,JKT)
       WRITE (*,3)
3      FORMAT (4X,'* denotes selected option')
c    get parameters
       NOS = ID(JKT+8)
       NOPC = ID(JKT+9)
c    display the objects and construct list
       DO I=1,NOS
           JKTO = ID(JKT+9+I)
           CALL DOPZZ(I,JKTO)
           ENDDO
c    add help if present
       JKTH = ID(JKT+5)
       IF (JKTH.GT.0)  THEN
c            display object
               IX = NOS + 1
               CALL DOBJZZ(IX,JKTH)
           ELSE
c            no help
               IX = NOS
           ENDIF
c    create prompt
       ML = 0
       CALL LOADZZ(' Enter desired option (0 if selection is ok) > ',47,
     ;     LINE,ML)
c    issue prompt
       CALL POSTZZ(LINE,ML,.TRUE.)
c    get response
       I = NOPZZ(IX)
c    check for none
       IF (I.EQ.0)  THEN
           JKTX = 0
c        check for a selection
           IF ((NOPC.EQ.0).OR.(NOPC.EQ.IU))  THEN
c            return with incomplete data
               ID(JKT+3) = 0
               RETURN
               ENDIF
c        set status as status of option
           JKTO = ID(JKT+9+NOPC)
           ID(JKT+3) = ID(JKTO+3)
           RETURN
           ENDIF
c    check for help
       IF (I.GT.NOS)  THEN
           JKTX = JKTH
           RETURN
           ENDIF
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
c        set main group's status to that of the option group
           ID(JKT+3) = ID(JKTX+3)
           RETURN
           ENDIF
c    check for # variables
       NVS = ID(JKTX+6)
       IF (NVS.GT.0)  THEN
c        get new values
           DO 29 IV=1,NVS
               JKTV = ID(JKTX+6+IV)
               CALL GVZZ(JKTV)
c            check that value is ok
               IF (ID(JKTV+3).EQ.0)  THEN
c                value bad; restore and abort
                   ID(JKT+9) = NOPCX
                   GOTO 1
                   ENDIF
29             CONTINUE
           ENDIF
c    set option status
       ID(JKTX+3) = 2
c    clear designator and antecedents
       CALL SGDUZZ(JKTX)
c    load pointer to designator
       ID(JKT+4) = JKTX
c    go around again
       GOTO 1
       END
c******************************************************************************
c
       SUBROUTINE  GCIOZZ(JKT,JKTX)
c
c      Gets changes to inclusive options having pointer JKT. Returns JKTX
c      as pointer to another group or help if selected, otherwise JKTX = 0.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       LOGICAL VALS,SAME
c------------------------------------------------------------------------------
       CHARACTER*1 FORM(17)
c    ('(',nn,' max.)')
       DATA FORM/'(','''','(','''',',','n','n',',',
     ;           '''',' ','m','a','x','.',')','''',')'/
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*96    LINE
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.4)
     ;     CALL ERRFZZ('@','GCIOZZ error; not inclusive options.@@')
c    get parameters
       NOS = ID(JKT+8)
       NOPMAX = ID(JKT+9)
c    display the header
1      CALL CLPWZZ
       CALL DGHZZ(0,JKT)
       WRITE (*,2) NOPMAX
2      FORMAT (4X,'* denotes selected options (maximum ',I2,')')
c
c    see if objects are options of the same length without parameters
       SAME = .FALSE.
       DO I=1,NOS
c        get object data
           JKTO = ID(JKT+9+NOS+I)
           KTO = ID(JKTO)
           IF ((KTO.NE.20).OR.(ID(JKTO+6).NE.0))  GOTO 8
c        option with no parameters
           IF (I.EQ.1)  THEN
                   NCS = ID(JKTO+5)
               ELSE
                   IF (ID(JKTO+5).NE.NCS)  GOTO 8
               ENDIF
           ENDDO
c    ojects all the same length
       NLINE = 79/(NCS+6)
       GOTO 10
c
c    objects not same
8      NLINE = 1
c
c    display the objects and construct list
10     NSEL = 0
       VALS = .FALSE.
       ML = 0
       KTR = 0
       DO I=1,NOS
c        construct indicator and option
           JKTO = ID(JKT+9+NOS+I)
           CALL LOPZZ(I,JKTO,LINE,ML)
           IF (NLINE.GT.1)  CALL LOADZZ('   ',1,LINE,ML)
           KTR = KTR + 1
           IF ((KTR.EQ.NLINE).OR.(I.EQ.NOS)) THEN
               CALL POSTZZ(LINE,ML,.FALSE.)
               KTR = 0
               ML = 0
               ENDIF
c        count selected items
           NOPCI = ID(JKT+9+I)
           IF (NOPCI.EQ.1) THEN
c            selected option
               NSEL = NSEL + 1
c            check for values in a selected item
               IF (ID(JKTO).LE.4)  THEN
                       VALS = .TRUE.
                   ELSEIF (ID(JKTO).EQ.20)  THEN
                       IF (ID(JKTO+5).GT.0)  VALS = .TRUE.
                   ELSE
                       CALL ERRFZZ('@','GCIOZZ error; '//
     ;                              'improper option.@@')
                   ENDIF
               ENDIF
           ENDDO
c    add help if present
       JKTH = ID(JKT+5)
       IF (JKTH.GT.0)  THEN
c            display object
               IX = NOS + 1
               CALL DOBJZZ(IX,JKTH)
           ELSE
c            no help
               IX = NOS
           ENDIF
c    construct prompt
       ML = 0
       CALL LOADZZ(' Enter option to add/remove',27,LINE,ML)
       ML = 27
       IF (VALS) THEN
           CALL LOADZZ('/change values',14,LINE,ML)
           ENDIF
       CALL LOADZZ(' (0 if none) > ',15,LINE,ML)
c    write prompt
       CALL POSTZZ(LINE,ML,.TRUE.)
c    get option to change
       I = NOPZZ(IX)
c    check for none
       IF (I.EQ.0)  THEN
           JKTX = 0
           IF (ID(JKT+3).EQ.0) ID(JKT+3) = 2
c        check for incomplete data in selected group options
           DO 15 I=1,NOS
               NOPCI = ID(JKT+9+I)
               JKTO = ID(JKT+9+NOS+I)
               IF (NOPCI.EQ.1) THEN
                   JKTO = ID(JKT+9+NOS+I)
c                reset status as incomplete if group data is incomplete
                   IF (ID(JKTO+3).EQ.0) ID(JKT+3) = 0
                   ENDIF
15             CONTINUE
c        check for group designation
           JKTD = ID(JKT+4)
           IF (JKTD.NE.0)  THEN
c            get group designation
               CALL GGDZZ(JKTD)
               ENDIF
           RETURN
           ENDIF
c    check for help
       IF (I.GT.NOS) THEN
           JKTX = JKTH
           RETURN
           ENDIF
c    check status of this option
       NOPCI = ID(JKT+9+I)
       IF (NOPCI.EQ.1)  THEN
c            deselect option
               ID(JKT+9+I) = 0
c            go around again
               GOTO 1
           ELSE
c            check if ok to add option
               IF (NSEL.GE.NOPMAX)  THEN
                   WRITE (*,18)
18                 FORMAT(/' Maximum number already selected')
                   GOTO 1
                   ENDIF
c            select option
               ID(JKT+9+I) = 1
           ENDIF
c    get pointer to option
       JKTX = ID(JKT+9+NOS+I)
c    check for group option
       IF (ID(JKTX).LE.5)  THEN
c        set new status of the option group
           ID(JKT+3) = 2
c        clear designator and antecedants
           CALL SGDUZZ(JKT)
c        go process the group
           RETURN
           ENDIF
c    check for # variables
       NVS = ID(JKTX+6)
       IF (NVS.GT.0)  THEN
c        get new values
           DO 29 IV=1,NVS
               JKTV = ID(JKTX+6+IV)
               CALL GVZZ(JKTV)
c             check value and abort if not set
               IF (ID(JKTV+3).EQ.0)  THEN
                   ID (JKT+9+I) = NOPCI
                   GOTO 1
                   ENDIF
29             CONTINUE
           ENDIF
c    set new status of the selected option
       ID(JKTX+3) = 2
c    set new status of the option group
       ID(JKT+3) = 2
c    clear designator and antecedants
       CALL SGDUZZ(JKT)
c    go around again
       GOTO 1
       END
c******************************************************************************
c
       SUBROUTINE  GHOPZZ(JKT,JKTX,HELP)
c
c      Gets help option from help options group having pointer JKT.
c      If a help options group is selected, returns the group pointer in JKTX.
c      Otherwise returns JKTX = 0.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       EXTERNAL    HELP
c------------------------------------------------------------------------------
c    external file
       CHARACTER*96 LINE
c------------------------------------------------------------------------------
c    check call
       KT = ID(JKT)
       IF (KT.NE.6)
     ;     CALL ERRFZZ('@','GHOPZZ error; not help options.@@')
c    display the header
1      CALL DGHZZ(0,JKT)
c    get parameters
       NOH = ID(JKT+5)
c    display the objects and construct list
       DO 9 I=1,NOH
           JKTO = ID(JKT+5+I)
           KTO = ID(JKTO)
           IF (KTO.EQ.21)  THEN
c                help option
                   CALL DHOZZ(I,JKTO)
               ELSEIF (KTO.EQ.6)  THEN
c                help option group
                   CALL DGHZZ(I,JKTO)
               ENDIF
9          CONTINUE
c    construct prompt
       ML = 0
       CALL LOADZZ(' Enter the desired help number (0 if none) > ',45,
     ;     LINE,ML)
c    write prompt
       CALL POSTZZ(LINE,ML,.TRUE.)
c    get choice
       I = NOPZZ(NOH)
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
c    go around again
       GOTO 1
       END
c******************************************************************************
c
       SUBROUTINE GCZZ(HELP)
c
c      Gets changes, providing help when requested
c------------------------------------------------------------------------------
c    applications-specific help routine
       EXTERNAL    HELP
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE     (ID(10),JKTGOD)
c------------------------------------------------------------------------------
c    initialize with the top-level
       JKT = JKTGOD
c
c    actions at the current level
10     KT = ID(JKT)
c    branch on current object type
       IF (KT.EQ.1)  THEN
c            items group
               CALL CLPWZZ
               CALL SFIGZZ(JKT,JKTX)
           ELSEIF (KT.EQ.2) THEN
c            values group
               CALL CLPWZZ
               CALL GCVGZZ(JKT,JKTX)
           ELSEIF (KT.EQ.3) THEN
c            exclusive options group
               CALL CLPWZZ
               CALL GCEOZZ(JKT,JKTX)
           ELSEIF (KT.EQ.4) THEN
c            inclusive options group
               CALL CLPWZZ
               CALL GCIOZZ(JKT,JKTX)
           ELSEIF (KT.EQ.5) THEN
c            table
               CALL CLPWZZ
               CALL GCTBZZ(JKT,JKTX)
           ELSEIF (KT.EQ.6)  THEN
c            help options group
               CALL CLPWZZ
               CALL GHOPZZ(JKT,JKTX,HELP)
           ELSEIF (KT.EQ.10)  THEN
c            group designator
               CALL GGDZZ(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.11)  THEN
c            character value
               CALL GCVZZ(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.12)  THEN
c            integer value
               CALL GIVZZ(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.13)  THEN
c            real value
               CALL GRVZZ(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.14)  THEN
c            file name
               CALL GFNZZ(JKT)
               JKTX = 0
           ELSEIF (KT.EQ.21)  THEN
c            help option
               KHELP = ID(JKT+5)
               CALL HELP(KHELP)
               JKTX = 0
           ELSE
c            error
               CALL ERRFZZ('@','GCZZ error; improper object.@@')
               CALL STOPZZ
           ENDIF
c
c    check for redirection
       IF (JKTX.NE.0) THEN
c        go to specified object
           JKT = JKTX
           GOTO 10
           ENDIF
c
c    quit if at top level
       IF (JKT.EQ.JKTGOD) RETURN
c
c    go to parent
       JKT = ID(JKT+1)
       GOTO 10
       END
c******************************************************************************

