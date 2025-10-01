c******************************************************************************
c
c      Model setup routines
c
c******************************************************************************
c
       SUBROUTINE SETRVV
c
c      ESPJAN reactant setup
c------------------------------------------------------------------------------
c    Work array dimensioning
       INCLUDE 'ESPJWORK.H'
c------------------------------------------------------------------------------
c    Stanjan data work arrays
       CHARACTER*8 CHEM(NSMAX2)
       COMMON /CHEMVV/CHEM
c------------------------------------------------------------------------------
c    Stanjan work arrays referenced by pointers
       INTEGER IW(NIWORK)
       COMMON /SJIW/IW
c------------------------------------------------------------------------------
c    Stanjan work array pointers
       DIMENSION ISPTR(10)
       COMMON /SJSPTR/ ISPTR
       EQUIVALENCE (IoNSF,ISPTR(5))
c------------------------------------------------------------------------------
c    reactant selection
       DIMENSION   NOPR(NSMAX)
       COMMON /NOPRVV/NOPR
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*16  LINE
c------------------------------------------------------------------------------
       NSF = IW(IoNSF)
       CALL AOZZ('@','Proceed to fuel identification@')
       CALL AOZZ('@','Use another .SUD data file@')
       CALL AOZZ('@','Quit ESPJAN@')
c
c    species group
       CALL BIOGZZ(0,0,'@','Select reactant species@',NSF,NSF,NOPR,J0)
           CALL HOZZ(J0,0,'@','Help@',2,JX)
           DO J=1,NSF
c            construct species options
               ML = 0
               CALL LOADZZ(CHEM(NSMAX+J),8,LINE,ML)
               CALL LOADZZ('@',1,LINE,ML)
               CALL ODZZ(J0,J,'@',LINE,0,JX)
               ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ACTRVV(I,NOPA)
c
c      Setup ACTIONS
c------------------------------------------------------------------------------
       IF (I.EQ.3)  THEN
c        verify quit
           CALL QUITVV
c        if here no quit
           NOPA = 0
           RETURN
           ENDIF
       NOPA = I
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SETFVV
c
c      ESPJAN fuel identification
c------------------------------------------------------------------------------
c    Work array dimensioning
       INCLUDE 'ESPJWORK.H'
c------------------------------------------------------------------------------
c    Stanjan data work arrays
       CHARACTER*8 CHEM(NSMAX2)
       COMMON /CHEMVV/CHEM
c--------------------------------\---------------------------------------------
c    Stanjan work arrays referenced by pointers
       INTEGER IW(NIWORK)
       COMMON /SJIW/IW
c------------------------------------------------------------------------------
c    Stanjan work array pointers
       DIMENSION IEPTR(80)
       COMMON /SJEPTR/ IEPTR
c------------------------------------------------------------------------------
c    pointers
       EQUIVALENCE (IoNS ,IEPTR(8))
c------------------------------------------------------------------------------
c    name of reactant moles
       CHARACTER*48   NRMOLE
       COMMON /NRMLVV/NRMOLE
c------------------------------------------------------------------------------
       DIMENSION NPF(NSMAX)
       COMMON  /NOPFVV/NOPF
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96  LINE
c------------------------------------------------------------------------------
       CALL AOZZ('@','Proceed to reactant moles specification@')
       CALL AOZZ('@','Return to reactant species selection@')
       CALL AOZZ('@','Quit ESPJAN@')
c
c     set pointers to data
       IoDHFF = NSMAX
       IoWMF = 2*NSMAX
       NS = IW(IoNS)
       CALL BIOGZZ(0,0,'@','Identify fuel species@',NS,NS,NOPF,J0)
           CALL HOZZ(J0,0,'@','Help@',3,JX)
           DO JS=1,NS
c            construct species option list
               ML = 0
               CALL LOADZZ(CHEM(JS),8,LINE,ML)
               CALL LOADZZ('@',1,LINE,ML)
               CALL ODZZ(J0,JS,'@',LINE,0,JX)
               ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ACTFVV(I,NOPA)
c
c      Setup ACTIONS
c------------------------------------------------------------------------------
       IF (I.EQ.3)  THEN
c        verify quit
           CALL QUITVV
c        if here no quit
           NOPA = 0
           RETURN
           ENDIF
       NOPA = I
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SETMVV
c
c      ESPJAN reactant moles setup
c------------------------------------------------------------------------------
c    Work array dimensioning
       INCLUDE 'ESPJWORK.H'
c------------------------------------------------------------------------------
c    Stanjan data work arrays
       CHARACTER*8 CHEM(NSMAX2)
       COMMON /CHEMVV/CHEM
c-----------------------------------------------------------------------------
c    Stanjan work arrays referenced by pointers
       INTEGER IW(NIWORK)
       REAL*4  SW(NSWORK)
       COMMON /SJIW/IW
       COMMON /SJSW/SW
c------------------------------------------------------------------------------
c    Stanjan work array pointers
       DIMENSION IEPTR(80)
       DIMENSION ISPTR(10)
       COMMON /SJEPTR/ IEPTR
       COMMON /SJSPTR/ ISPTR
c------------------------------------------------------------------------------
c    pointers
       EQUIVALENCE (IoNS ,IEPTR(8))
       EQUIVALENCE (IoJFS,ISPTR(6))
       EQUIVALENCE (IoNSF,ISPTR(5))
c------------------------------------------------------------------------------
c    name of reactant moles
       CHARACTER*48   NRMOLE
       COMMON /NRMLVV/NRMOLE
c------------------------------------------------------------------------------
c    .ESJ properties file data
       REAL*4   PP,PR,RFT,RMOLS(NSMAX)
       COMMON  /PROPVV/ PP,PR,RFT,RMOLS
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96  LINE
       CHARACTER*16  TEMP
c------------------------------------------------------------------------------
       CALL AOZZ('@','Proceed to product species selection@')
       CALL AOZZ('@','Return to fuel specification@')
       CALL AOZZ('@','Return to reactant species selection@')
       CALL AOZZ('@','Quit ESPJAN@')
c
c    set pointers to data
       NSF = IW(IoNSF)
       IoDHFF = NSMAX
       IoWMF = 2*NSMAX
       NS = IW(IoNS)
       CALL BIGZZ(0,0,'@','Set reactant moles@',NS,J0)
           CALL HOZZ(J0,0,'@','Help@',4,JX)
c        moles of reactants
           DO JS=1,IW(IoNS)
c            find system species JS in data file
               JF = IW(IoJFS+JS)
c            construct species and data prompt
               ML = 0
               CALL LOADZZ(CHEM(JS),8,LINE,ML)
               CALL LOADZZ(' (M = ',5,LINE,ML)
               WRITE (TEMP,2) SW(IoWMF+JF)
2              FORMAT(F6.2)
               CALL LOADZZ(TEMP,6,LINE,ML)
               CALL LOADZZ(' kg/kmole, Hf =',15,LINE,ML)
               WRITE (TEMP,3) SW(IoDHFF+JF)
3              FORMAT(F7.2)
               CALL LOADZZ(TEMP,7,LINE,ML)
               CALL LOADZZ(' kcal/mole) moles',17,LINE,ML)
               CALL RVZZ(J0,JS,LINE,ML,1,0.,0.,RMOLS(JS),JX)
               ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ACTMVV(I,NOPA)
c
c      Setup ACTIONS
c------------------------------------------------------------------------------
       IF (I.EQ.4)  THEN
c        verify quit
           CALL QUITVV
c        if here no quit
           NOPA = 0
           RETURN
           ENDIF
       NOPA = I
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SETPVV
c
c      ESPJAN product setup
c------------------------------------------------------------------------------
c    Work array dimensioning
       INCLUDE 'ESPJWORK.H'
c------------------------------------------------------------------------------
c    Stanjan data work arrays
       CHARACTER*8 CHEM(NSMAX2)
       COMMON /CHEMVV/CHEM
c------------------------------------------------------------------------------
c    Stanjan work arrays referenced by pointers
       INTEGER IW(NIWORK)
       COMMON /SJIW/IW
c------------------------------------------------------------------------------
c    Stanjan work array pointers
       DIMENSION ISPTR(10)
       COMMON /SJSPTR/ ISPTR
       EQUIVALENCE (IoNSF,ISPTR(5))
c------------------------------------------------------------------------------
c    product selection
       DIMENSION   NOPP(NSMAX)
       COMMON /NOPPVV/NOPP
c------------------------------------------------------------------------------
c    internal file
       CHARACTER*16  LINE
c------------------------------------------------------------------------------
       NSF = IW(IoNSF)
       CALL AOZZ('@','Proceed to final data input@')
       CALL AOZZ('@','Return to reactant selection@')
       CALL AOZZ('@','Use another .SUD data file@')
       CALL AOZZ('@','Quit ESPJAN@')
c
c    species group
       CALL BIOGZZ(0,0,'@','Select product species@',NSF,NSF,NOPP,J0)
           CALL HOZZ(J0,0,'@','Help@',5,JX)
           DO J=1,NSF
c            construct species options
               ML = 0
               CALL LOADZZ(CHEM(NSMAX+J),8,LINE,ML)
               CALL LOADZZ('@',1,LINE,ML)
               CALL ODZZ(J0,J,'@',LINE,0,JX)
               ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ACTPVV(I,NOPA)
c
c      Setup ACTIONS
c------------------------------------------------------------------------------
       IF (I.EQ.4)  THEN
c        verify quit
           CALL QUITVV
c        if here no quit
           NOPA = 0
           RETURN
           ENDIF
       NOPA = I
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SETXVV
c
c      ESPJAN final setup
c------------------------------------------------------------------------------
c    Work array dimensioning
       INCLUDE 'ESPJWORK.H'
c------------------------------------------------------------------------------
c    Stanjan work arrays referenced by pointers
       INTEGER IW(NIWORK)
       COMMON /SJIW/IW
c------------------------------------------------------------------------------
c    Stanjan work array pointers
       DIMENSION ISPTR(10)
       COMMON /SJSPTR/ ISPTR
       EQUIVALENCE (IoNSF,ISPTR(5))
c------------------------------------------------------------------------------
c    .ESJ properties file data
       REAL*4   PP,PR,RFT,RMOLS(NSMAX)
       COMMON  /PROPVV/ PP,PR,RFT,RMOLS
       CHARACTER*48    NGPM
       COMMON  /NGPMVV/ NGPM
c------------------------------------------------------------------------------
c    data file name
       CHARACTER*32    DFILE
       COMMON /DFLEVV/ DFILE
c------------------------------------------------------------------------------
c    output file name
       CHARACTER*32    OFILE
       COMMON /OFLEVV/ OFILE,NOUT
c------------------------------------------------------------------------------
       NSF = IW(IoNSF)
       CALL AOZZ('@','Calculate the properties@')
       CALL AOZZ('@','Return to setup@')
       CALL AOZZ('@','Quit ESPJAN@')
c
       CALL BIGZZ(0,0,'@','Final data@',5,J0)
           CALL HOZZ(J0,0,'@','Help@',6,JX)
           CALL RVZZ(J0,1,
     ;        'Pressure for reactant properties evaluation, atm.',49,
     ;              1,0.,0.,PR,JX)
           CALL RVZZ(J0,2,
     ;        'Pressure for products properties evaluation, atm.',49,
     ;              1,0.,0.,PP,JX)
           CALL CVZZ(J0,3,'Properties set name',19,48,NGPM,JX)
           CALL FNZZ(J0,4,'.ESJ file',9,32,'ESJ',2,DFILE,JX)
           CALL BEOGZZ(J0,5,'@','Print file@',2,NOUT,J0P)
               CALL ODZZ(J0P,1,'@','None@',0,JX)
               CALL ODZZ(J0P,2,'@',
     ;           'Write properties to #@',1,J0PW)
                   CALL FNZZ(J0PW,1,'.OUT file',9,32,'OUT',2,OFILE,JX)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ACTXVV(I,NOPA)
c
c      Setup ACTIONS
c------------------------------------------------------------------------------
       IF (I.EQ.3)  THEN
c        verify quit
           CALL QUITVV
c        if here no quit
           NOPA = 0
           RETURN
           ENDIF
       NOPA = I
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  HELPVV(K)
c
c      Responds to interface's requests for help code K.
c------------------------------------------------------------------------------
c    clear output window
       CALL CLOWZZ
c    provide the help
       IF (K.EQ.2)  THEN
c            reactants
               CALL HRCTVV
           ELSEIF (K.EQ.3)  THEN
               CALL HRCTVV
           ELSEIF (K.EQ.4)  THEN
               CALL HRCTVV
           ELSEIF (K.EQ.5)  THEN
               CALL HPRDVV
           ELSEIF (K.EQ.6)  THEN
               CALL HPSEVV
               CALL HNMEVV
           ELSE
               CALL MESSZZ('#','#Program error; help missing.  '//
     ;                    'Report to wcr@thermo.stanford.edu##')
           ENDIF
       CALL PAUSZZ(1)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ENAMVV(ESC,NAME,NAMES)
c
c      Forms an ESC-terminated string in NAMES from NAME
c-----------------------------------------------------------------------------
       CHARACTER*1 ESC,NAME(48),NAMES(64),LEAD(9)
       DATA LEAD/'S','p','e','c','i','a','l',':',' '/
c-----------------------------------------------------------------------------
c    remove trailing blanks
       DO L=1,48
           N = 48 - L
           IF (NAME(N).NE.' ')  GOTO 10
           ENDDO
c    load the lead
10     DO I=1,9
           NAMES(I) = LEAD(I)
           ENDDO
c    load the name
       DO I=1,N
           NAMES(I+9) = NAME(I)
           ENDDO
c    load the final ESC
       NAMES(N+10) = ESC
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CHMNVV(CHEM,N)
c
c      Finds the length N of chemical code CHEM
c-----------------------------------------------------------------------------
       CHARACTER*1 CHEM(8)
c-----------------------------------------------------------------------------
c    remove trailing blanks
       DO L=1,8
           N = 9 - L
           IF (CHEM(N).NE.' ')  RETURN
           ENDDO
       RETURN
       END
c******************************************************************************
