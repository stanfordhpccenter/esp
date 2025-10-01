c******************************************************************************
c
c      ESPCAM  valve program prepartion for ESP
c
c------------------------------------------------------------------------------
       PARAMETER (IMAX=48)
c------------------------------------------------------------------------------
c    general setup work arrays
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
c------------------------------------------------------------------------------
c    application-specific subroutines called by GETZZ
       EXTERNAL SVP1VV,HVP1VV,AVP1VV
       EXTERNAL SVP2VV,AVP2VV
c------------------------------------------------------------------------------
c    task control
       COMMON /TASKVV/ NTASK
c------------------------------------------------------------------------------
c    old and new file data
       CHARACTER*32    FILEO,FILEN
       CHARACTER*48    NAMEO,NAMEN
       DIMENSION       ADO(IMAX),FDO(IMAX)
       DIMENSION       ADN(IMAX),FDN(IMAX)
       COMMON /VPDOVV/ NAMEO,FMAXO,NDO,ADO,FDO
       COMMON /VPDNVV/ NAMEN,FMAXN,NDN,ADN,FDN
       COMMON /FILEVV /FILEO,FILEN
c------------------------------------------------------------------------------
c    output file controls
       CHARACTER*32    MPFILE,PRFILE,VPFILE
       DIMENSION NOPO(3)
       COMMON /VPOFVV/MPFILE,PRFILE,VPFILE,NOPO
c------------------------------------------------------------------------------
c    data from table
       DIMENSION AV(2*IMAX)
       COMMON /NVPTVV/AV
c------------------------------------------------------------------------------
       LOGICAL OK
c------------------------------------------------------------------------------
c    line length for non-graphical interface message box
       COMMON /MESBZZ/ LMESS
       LMESS = 78
c------------------------------------------------------------------------------
c    load unset indicators (be consistent with SETZZ in file IB.F
       CD(1) = CHAR(239)
       ID(1) = -22222
       RD(1) = -1.1111111E11
c------------------------------------------------------------------------------
c   opening header
       CALL MESSZZ('#','#ESPCAM v1.03  Creates valve program file '//
     ;             'for ESP.# #'//
     ;             'Address all enquiries to '//
     ;             ' wcr@thermo.stanford.edu.##')
       CALL HVPCVV
       CALL PAUSZZ(1)
c
10     NIV1 = 0
c    main menu
       CALL GETZZ(NIV1,SVP1VV,HVP1VV,AVP1VV,NOPA1)

c    check for edit of existing file
       IF (NTASK.EQ.1) THEN
c        get file
           CALL OPFLVV(10,FILEO,.TRUE.,.FALSE.,OK)
           IF (.NOT.OK)  THEN
               CALL FMESVV(1,'@','#','Valve program file # '//
     ;         'not found.@',FILEO)
               GOTO 10
               ENDIF
           CALL  RVPDVV(10,IMAX,NAMEO,NDO,FMAXO,ADO,FDO,IERR)
           CLOSE(10)
c        check for file error
           IF (IERR.NE.0) THEN
c            try V1 format
               CALL OPUOVV(10,FILEO,IERR)
               IF (IERR.NE.0)  THEN
                   CALL FMESVV(1,'@','#','Valve program file # '//
     ;             'not found.@',FILEO)
                   GOTO 10
                   ENDIF
               CALL RVPOVV(10,IMAX,NAMEO,NDO,FMAXO,ADO,FDO,IERR)
               IF (IERR.NE.0)  THEN
                   CALL FMESVV(2,'@','#','Valve program file # '//
     ;             'corrupted.@',FILEO)
                   GOTO 10
                   ENDIF
               CALL FMESVV(1,'@','#','Valve program file # was in '//
     ;             'ESP v1 format. Save to ESP v2+ format.@',FILEO)
               CLOSE(10)
               ENDIF


c        load the old file to new
           NAMEN = NAMEO
           NDN = NDO
           DO I=1,NDO
               AV(I) = ADO(I)
               AV(I+NDN) = FDO(I)
               ENDDO
           NIV2 = 1
           GOTO 20
           ENDIF
c
c    new table input
       NIV2 = 0
c
c    get or correct data
20     CALL GETZZ(NIV2,SVP2VV,HVP1VV,AVP2VV,NOPA2)

c *  data checks
c
c    check first angle
       IF (AV(1).NE.0)  THEN
           CALL WARNZZ('@','@Error; first angle must be zero@@')
           NIV2 = 1
           GOTO 20
           ENDIF
c
c    check angles for monotonicity
       DO I=2,NDN
           IF (AV(I).LE.AV(I-1))  THEN
               CALL WARNZZ('@','@Error;  angles must increase@@')
               NIV2= 1
               GOTO 20
               ENDIF
           ENDDO
c    lifts were checked for >=0 in table construction
c
c    scan and load the table
       FMAXN = 0
       DO  I=1,NDN
           ADN(I) = AV(I)
           FDN(I) = AV(I+NDN)
           IF (FDN(I).GT.FMAXN) FMAXN = FDN(I)
           ENDDO
c    matlab file
       IF (NOPO(1).EQ.1)  THEN
           CALL OPFLVV(10,MPFILE,.FALSE.,.TRUE.,OK)
           IF (OK)  THEN
                   CALL VPMPVV(10,NAMEN,NDN,ADN,FDN)
                   CALL FMESVV(1,'@','#','Plot file # written.@',
     ;                         MPFILE)
               ELSE
                   CALL FMESVV(2,'@','#','Plot file # not written.@',
     ;                         MPFILE)
               ENDIF
           CLOSE(10)
           ENDIF
c    print file
       IF (NOPO(2).EQ.1)  THEN
           CALL OPFLVV(10,PRFILE,.FALSE.,.TRUE.,OK)
           IF (OK)  THEN
                   CALL DVPTVV(10,0,NAMEN,NDN,ADN,FDN)
                   CALL FMESVV(1,'@','#','Print file # written.@',
     ;                         PRFILE)
               ELSE
                   CALL FMESVV(2,'@','#','Print file # not written.@',
     ;                         PRFILE)
               ENDIF
           CLOSE(10)
           ENDIF
       IF (NOPO(3).EQ.1)  THEN
c        .ESV file
           CALL OPFLVV(10,VPFILE,.FALSE.,.FALSE.,OK)
           IF (OK)  THEN
                   CALL WVPDVV(10,IMAX,NAMEN,NDN,FMAXN,ADN,FDN,IERR)
                   IF (IERR.EQ.0)  THEN
                           CALL FMESVV(1,'@','#',
     ;                      'Valve program file # written.@',VPFILE)
                       ELSE
                           CALL FMESVV(2,'@','#',
     ;                      'Error writing valve program  file #  . '//
     ;                         'File missing or corrupted.@',VPFILE)
                       ENDIF
               ELSE
                   IF (IERR.NE.0)  CALL FMESVV(1,'@','#',
     ;                 'Valve program file # not written.@',VPFILE)
               ENDIF
               CLOSE(10)
           ENDIF
c    next program
       GOTO 10
c
       END
c******************************************************************************
c
       SUBROUTINE SVP1VV
c
c      Setup for getting valve program task
c------------------------------------------------------------------------------
       PARAMETER (IMAX=48)
c------------------------------------------------------------------------------
c    old and new file data
       CHARACTER*32    FILEO,FILEN
       CHARACTER*48    NAMEO,NAMEN
       DIMENSION       ADO(IMAX),FDO(IMAX)
       DIMENSION       ADN(IMAX),FDN(IMAX)
       COMMON /VPDOVV/ NAMEO,FMAXO,NDO,ADO,FDO
       COMMON /VPDNVV/ NAMEN,FMAXN,NDN,ADN,FDN
       COMMON /FILEVV /FILEO,FILEN
c------------------------------------------------------------------------------
       COMMON /TASKVV/ NTASK
c------------------------------------------------------------------------------
c    exit actions
       CALL AOZZ('@','Do task@')
       CALL AOZZ('@','Quit ESPCAM@')
c    task options and data
       CALL BEOGZZ(0,0,'@','Task@',2,NTASK,J0)
         CAll HOZZ(J0,0,'@','Help@',51,JX)
         CALL ODZZ(J0,1,'@','Edit #@',1,J0E)
           CALL FNZZ(J0E,1,'.ESV file',9,32,'ESV',1,FILEO,JX)
         CALL ODZZ(J0,2,'@','Create new program with # points@',1,J0N)
           CALL IVZZ(J0N,1,'N',1,12,2,IMAX,NDN,JX)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE AVP1VV(I,NOPA)
c
c      Setup ACTIONS
c
c      I=1 do task
c        2 Quit
c------------------------------------------------------------------------------
c    Setup file name
       CHARACTER*32  SUFILE
       COMMON /SUFVV/SUFILE
c------------------------------------------------------------------------------
       LOGICAL OK,YESZZ
c------------------------------------------------------------------------------
       IF (I.EQ.1)  THEN
c        set to check data and exit
           NOPA = 1
           RETURN
           ENDIF
       IF (I.EQ.2) THEN
c        confirm quit
           IF (YESZZ('Do you really want to quit ESPCAM?')) STOP
c        no quit
           NOPA = 0
           RETURN
           ENDIF
c    improper request
c      CALL ERRFZZ('@','@Illegal action request.@@')
       NOPA = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  HVP1VV(K)
c
c      Responds to interface's requests for help code K.
c------------------------------------------------------------------------------
c    clear output window
       CALL CLOWZZ
c    provide the help
       IF (K.EQ.51)  THEN
               CALL HVPCVV
           ELSEIF (K.EQ.52)  THEN
               CALL HVPCVV
           ELSEIF (K.EQ.53)  THEN
               CALL HVPOVV
           ELSE
               CALL MESSZZ('@','Error; help missing.@@')
           ENDIF
       CALL PAUSZZ(1)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HVPCVV
c
c      Help in valve program construction
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;     '@Before using ESPCAM, prepare a table of the '//
     ;     'valve lift (arbitrary units) as a function of '//
     ;     'crank angle (arbitrary units) past the angle at '//
     ;     'which the valve opens.  The opening and closing '//
     ;     'angles will be set as an operating parameter in ESP.  '//
     ;     'ESP will stretch your cam curve to fit these angles. '//
     ;     'In ESPCAM you enter the number of points in this '//
     ;     'table (48 max), the angles and lifts, the name of '//
     ;     'your valve program, and the .ESV save file name.  '//
     ;     'The table must contain at least 2 points.  '//
     ;     'The first point must be at zero angle. ' //
     ;     'Angles and lifts are floating '//
     ;     'point numbers used with linear interpolations between.@ @'//
     ;     'A smooth cam should have zero lift at the first and last '//
     ;     'points.  You can make a step up at opening by setting '//
     ;     'non-zero lift at the first point and a step down '//
     ;     'at the end by non-zero lift at the last point.@ @'//
     ;     'You can edit an existing .ESV file as long as you '//
     ;     'do not change the number of data points, and save the '//
     ;     'new program in the same or a different .ESV file.  '//
     ;     'ESPCAM can generate a Matlab plot file for your valve '//
     ;     'program and/or write the valve program to a print file.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HVPOVV
c
c      Help on output
c------------------------------------------------------------------------------
       CALL MESSZZ('@','@Write a Matlab .M file to make a plot using '//
     ;    'Matlab.  Write a .PRN file to use for printing your '//
     ;    'valve program.  Save an .ESV file to use in ESP.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE STOPZZ
c
c      Interface error stop routine
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(2),NC)
       EQUIVALENCE(ID(3),NI)
       EQUIVALENCE(ID(4),NR)
c------------------------------------------------------------------------------
       OPEN (19,FILE='WORKDUMP.INT')
c    work array dump
       WRITE (19,82) (ID(I),I=1,NI)
82     FORMAT (' ID: ',15I5)
       WRITE (19,83) (RD(I),I=1,NR)
83     FORMAT (' RD: ',15F5.2)
       WRITE (19,84) (CD(I),I=1,NC)
84     FORMAT (' CD: ',75A1)
       CLOSE(19)
       STOP
       END
c******************************************************************************
c
       SUBROUTINE SVP2VV
c
c      Setup for getting new program data
c------------------------------------------------------------------------------
       PARAMETER (IMAX=48)
c------------------------------------------------------------------------------
c    new file data
       CHARACTER*32    FILEN
       CHARACTER*48    NAMEN
       DIMENSION       ADN(IMAX),FDN(IMAX)
       COMMON /VPDNVV/ NAMEN,FMAXN,NDN,ADN,FDN
c------------------------------------------------------------------------------
c    data from table
       DIMENSION AV(2*IMAX)
       COMMON /NVPTVV/AV
c------------------------------------------------------------------------------
c    frozen table columns referenced but not used
       DIMENSION AF(1)
c------------------------------------------------------------------------------
c    output file controls
       CHARACTER*32    MPFILE,PRFILE,VPFILE
       DIMENSION NOPO(3)
       COMMON /VPOFVV/MPFILE,PRFILE,VPFILE,NOPO
c------------------------------------------------------------------------------
c    data restrictions
       DIMENSION KRC(2),NCW(2),VMINC(2),VMAXC(2)
       DATA KRC /12,4/
       DATA VMINC /0.,0./
       DATA VMAXC /720.,0./
       DATA NCW /22,22/
c------------------------------------------------------------------------------
c    plot task designator
       CHARACTER*32    FILEOP
c------------------------------------------------------------------------------
c    set no output options
       DO I=1,3
           NOPO(I) = 0
           ENDDO
c    exit actions
       CALL AOZZ('@','Save indicated files@')
       CALL AOZZ('@','Return to main menu@')
       CALL AOZZ('@','Quit ESPCAM@')
       CALL BIGZZ(0,0,'@','Valve program construction/output@',2,J0)
         CALL HOZZ(J0,0,'@','Help@',52,JX)
c       table
         CALL BTABZZ(J0,1,'@','Valve program@',NDN,0,2,NCW,
     ;    'Degrees past open@@'//
     ;    'Relative lift@@',
     ;     KRC,VMINC,VMAXC,AF,AV,KSO,JT)
           CALL GDZZ(JT,48,NAMEN,JX)
           CAll HOZZ(JT,0,'@','Help@',52,JX)
c       output options
         CALL BIOGZZ(J0,2,'@','Output file options@',
     ;              3,3,NOPO,J0O)
           CALL HOZZ(J0O,0,'@','Help@',53,JX)
           CALL GDZZ(J0O,32,FILEOP,JX)
             CALL ODZZ(J0O,1,'@','Make #@',1,J0OM)



               CALL FNZZ(J0OM,1,'Matlab .M plot file',19,32,'M  ',
     ;                  2,MPFILE,JX)
             CALL ODZZ(J0O,2,'@','Write to #@',1,J0OP)
               CALL FNZZ(J0OP,1,'.PRN print File',15,32,'PRN',
     ;                  2,PRFILE,JX)
             CALL ODZZ(J0O,3,'@','Save in #@',1,J0OV)
               CALL FNZZ(J0OV,1,'.ESV file',9,32,'ESV',
     ;                  2,VPFILE,JX)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE AVP2VV(I,NOPA)
c
c      Setup ACTIONS
c
c      I=1 save file
c        2 return to main menu
c        3 Quit
c------------------------------------------------------------------------------
c    Setup file name
       CHARACTER*32  SUFILE
       COMMON /SUFVV/SUFILE
c------------------------------------------------------------------------------
       LOGICAL OK,YESZZ
c------------------------------------------------------------------------------
       IF (I.EQ.1)  THEN
c        set to check data and exit
           NOPA = 1
           RETURN
           ENDIF
       IF (I.EQ.2)  THEN
c        set to check data and exit
           NOPA = 2
           RETURN
           ENDIF
       IF (I.EQ.3) THEN
c        confirm quit
           IF (YESZZ('Do you really want to quit ESPCAM?')) STOP
c        no quit
           NOPA = 0
           RETURN
           ENDIF
c    improper request
c      CALL ERRFZZ('@','@Illegal action request.@@')
       NOPA = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RVPDVV(IU,NDMAX,NAME,ND,FMAX,AD,FD,IERR)
c
c      Reads a new-style valve program file from opened unit IU
c      Checks that the array size ND <= NDMAX
c      Returns IERR=0 of ok, otherwise 1
c------------------------------------------------------------------------------
       DIMENSION AD(NDMAX),FD(NDMAX)
c------------------------------------------------------------------------------
       CHARACTER*48    NAME
c------------------------------------------------------------------------------
c    read name and size
       READ (IU,ERR=20,END=20) NAME,ND,FMAX
c    check size
       IF (ND.GT.NDMAX)  THEN
           CALL WARNZZ('@','@Valve program file too large@@')
           GOTO 20
           ENDIF
c    read the file
       READ (10,END=20,ERR=20) (AD(I),I=1,ND),(FD(I),I=1,ND)
       IERR = 0
       RETURN
c    file error
20     IERR=1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  WVPDVV(IU,NDMAX,NAME,ND,FMAX,AD,FD,IERR)
c
c      Writes a new-style unformatted valve program file to opened unit IU.
c      Returns IERR=0 of ok, otherwise 1.
c------------------------------------------------------------------------------
       DIMENSION AD(NDMAX),FD(NDMAX)
c------------------------------------------------------------------------------
       CHARACTER*48    NAME
c------------------------------------------------------------------------------
c    write file
       WRITE (IU,ERR=20) NAME,ND,FMAX
       WRITE (IU,ERR=20) (AD(I),I=1,ND),(FD(I),I=1,ND)
       IERR = 0
       RETURN
c    error writing file
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE VPMPVV(IU,NAME,ND,AD,FD)
c
c      Writes a Matlab valve program plot file to open unit IU
c------------------------------------------------------------------------------
c    new file data
       CHARACTER*48    NAME
       DIMENSION       AD(ND),FD(ND)
c------------------------------------------------------------------------------
       WRITE (IU,1)
1      FORMAT (' clear'/' figure'/' subplot(1,1,1)')
       WRITE (IU,4) AD(1),FD(1)
c
4      FORMAT (' A = [ ',2(1PE12.4),'    ;')
       WRITE (IU,5) (AD(I),FD(I),I=2,ND-1)
5      FORMAT ('       ',2(1PE12.4),'    ;')
       WRITE (IU,6) AD(ND),FD(ND),NAME
6      FORMAT ('       ',2(1PE12.4),'  ] ;'/
     ;   'plot (A(:,1),A(:,2))'/
     ;   'xlabel (''Crank angle degrees past valve open'')'/
     ;   'ylabel (''Relative lift '')'/
     ;   'title  ('' ',A,' '')' )
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  DVPTVV(IU,L,PROG,ND,AD,FD)
c
c      Displays a valve program table on unit IU.
c      No action for IU=0.
c
c      L=1 for intake, 2 for exhaust; 0 unspecified.
c------------------------------------------------------------------------------
       CHARACTER*48    PROG
       DIMENSION       AD(ND),FD(ND)
c------------------------------------------------------------------------------
c    no write for IU=0
       IF (IU.EQ.0)  RETURN
       IF (L.EQ.1)  THEN
               WRITE(IU,1) PROG
1              FORMAT (/' Intake valve program: ',A)
           ELSEIF (L.EQ.2)  THEN
               WRITE(IU,2) PROG
2              FORMAT (/' Exhaust valve program: ',A)
           ELSE
               WRITE (IU,3) PROG
3              FORMAT (/' Valve program: ',A)
           ENDIF
       WRITE (IU,4)
4      FORMAT(
     ; ' * Angles are in arbitrary units measured from valve opening.'/
     ; '   The curve is stretched to fit the open and close angles.'/
     ; ' * Lifts are in arbitrary units; max flow area set separately.'/
     ; ' * Linear interpolation is used between tabulated points.'/
     ; ' ============================================================='/
     ; '  angle  lift     angle  lift     angle  lift     angle  lift'/
     ; ' -------------------------------------------------------------')
       WRITE (IU,11) (AD(J),FD(J),J=1,ND)
11     FORMAT (4(F7.1,F7.3,2X))
c     write table ender
       WRITE (IU,12)
12     FORMAT(
     ; ' -------------------------------------------------------------')
       RETURN
       END
c*****************************************************************************
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
