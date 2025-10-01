c******************************************************************************
c
c      Run task setup routines
c
c******************************************************************************
c
       SUBROUTINE SETRVV
c
c      ESP run output setup
c------------------------------------------------------------------------------
c    ESP work arrays:
       DIMENSION IW(50)
       COMMON /IWVV/IW
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
       EQUIVALENCE (NCYC,IW(46))
c------------------------------------------------------------------------------
c    run output control parameters
       DIMENSION   KPLOT(3)
       COMMON     /RUNCVV/ NRUN,NOPR,KPLOT,NOPM
c------------------------------------------------------------------------------
c    5-line plot options
       DIMENSION KOP5(6)
       COMMON/KOP5VV/KOP5,KOP5C
c------------------------------------------------------------------------------
c    name of complete output plot setup vs. crankangle
       CHARACTER*32    LOPV
       COMMON  /LOPVVV/LOPV
c------------------------------------------------------------------------------
c    name of output plot setup for indicator
       CHARACTER*32    LOPC
       COMMON  /LOPCVV/LOPC
c------------------------------------------------------------------------------
c    name of output plot setup for variables
       CHARACTER*32    LOPA
       COMMON  /LOPAVV/LOPA
c------------------------------------------------------------------------------
c    name of ouptut file for tabulated data
       CHARACTER*32    TOUT
       COMMON  /TOUTVV/TOUT
c------------------------------------------------------------------------------
c    name of Matlab plot file for indicator
       CHARACTER*32    MPFIND
       COMMON  /MPFCVV/MPFIND
c------------------------------------------------------------------------------
c    5-line plot Matlab files
       CHARACTER*32 MPFL(6)
       COMMON /MPFLVV/ MPFL
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
c    indicator scales
       COMMON   /PVIMVV/PIMAX,VIMAX,NOPIS
c------------------------------------------------------------------------------
c    action items
       CALL AOZZ('@','Execute selected run task@')
       CALL AOZZ('@','Return to model/operation setup@')
       CALL AOZZ('@','Quit ESP@')
c
c    main run options
c      determine total number of task options
         NTOTOT = 3
         IF (NCYC.GT.0) NTOTOT = 6
       CALL BEOGZZ(0,0,'@','Run task Option@',NTOTOT,NOPR,J0)
         CAll HOZZ(J0,0,'@','Help@',30,JX)
         NTO = 1
         CALL ODZZ(J0,NTO,'@','Write model parameters to #@',1,J0M)
           CALL FNZZ(J0M,1,'Output File',
     ;                     11,32,'ESM',2,OFILE,JX)
         NTO = NTO + 1
         CALL ODZZ(J0,NTO,'@','Run # Cycles without output '//
     ;                      'to check convergence@',1,J0C)
           CALL IVZZ(J0C,1,'N',1,4,1,IX,NRUN,JX)
         NTO = NTO + 1
         CALL BIOGZZ(J0,NTO,'@','Run one cycle and plot variables@',
     ;               6,6,KOP5,J05)
           CALL GDZZ(J05,32,LOPA,JX)
           CALL HOZZ(J05,0,'@','Help@',40,JX)
           CALL ODZZ(J05,1,'@','Temperatures in #@',1,J05T)
               CALL FNZZ(J05T,1,'Matlab file',
     ;                         11,32,'M  ',2,MPFL(1),JX)
           CALL ODZZ(J05,2,'@','Masses in #@',1,J05M)
               CALL FNZZ(J05M,1,'Matlab file',
     ;                         11,32,'M  ',2,MPFL(2),JX)
           CALL ODZZ(J05,3,'@','Flow rates in #@',1,J05F)
               CALL FNZZ(J05F,1,'Matlab file',
     ;                         11,32,'M  ',2,MPFL(3),JX)
           CALL ODZZ(J05,4,'@','Pressures in #@',1,J05P)
               CALL FNZZ(J05P,1,'Matlab file',
     ;                         11,32,'M  ',2,MPFL(4),JX)
           CALL ODZZ(J05,5,'@','Energy rates in #@',1,J05R)
               CALL FNZZ(J05R,1,'Matlab file',
     ;                         11,32,'M  ',2,MPFL(5),JX)
           CALL ODZZ(J05,6,'@','Velocities in #@',1,J05V)
               CALL FNZZ(J05V,1,'Matlab file',
     ;                         11,32,'M  ',2,MPFL(6),JX)
c
         IF (NCYC.GT.0) THEN
c
c         plots from previous cycle
           NTO = NTO + 1
c         determine number of plot options
           NPOTOT = 1
           IF (KIMC.EQ.2) NPOTOT = NPOTOT + 1
           IF (KEMC.EQ.2) NPOTOT = NPOTOT + 1
c
c         main plot option header
           CALL BIOGZZ(J0,NTO,'@','Plot output from last cycle@',
     ;               NPOTOT,NPOTOT,KPLOT,J0O)
             CALL GDZZ(J0O,32,LOPV,JX)
             CALL HOZZ(J0O,0,'@','Help@',13,JX)
c
c           indicator option
             NPO = 1
             CALL BIGZZ(J0O,NPO,'@','Indicator (P-V) Diagram @',
     ;                   2,J0OD)
               CALL FNZZ(J0OD,1,'Matlab file for Indicator Diagram',
     ;                         33,32,'M  ',2,MPFIND,JX)
               CALL BEOGZZ(J0OD,2,'@','Plot scale option@',
     ;                         2,NOPIS,J0ODS)
                 CALL ODZZ(J0ODS,1,'@','Automatic P and V ranges@',
     ;                     0,JX)
                 CALL ODZZ(J0ODS,2,'@','P from 0 to #(MPa),'//
     ;             ' V from 0 to #(cm^3)@',2,J0ODSP)
                   CALL RVZZ(J0ODSP,1,'Pmax',4,1,X,X,PIMAX,JX)
                   CALL RVZZ(J0ODSP,2,'Vmax',4,1,X,X,VIMAX,JX)
c
c        check for intake manifold inclusion
           IF (KIMC.EQ.2)  THEN
             NPO = NPO + 1
             CALL ODZZ(J0O,NPO,'@','Intake manifold P and V plots '//
     ;       'in #@',1,J0OI)
               CALL FNZZ(J0OI,1,'Matlab file',
     ;                         11,32,'M  ',2,MPFIM,JX)
             ENDIF
c
c        check for exhaust manifold inclusion
           IF (KEMC.EQ.2)  THEN
             NPO = NPO + 1
             CALL ODZZ(J0O,NPO,'@','Exhaust manifold P and V plots '//
     ;       'in #@',1,J0OE)
               CALL FNZZ(J0OE,1,'Matlab file',
     ;                         11,32,'M  ',2,MPFEM,JX)
             ENDIF
c
c        tabular output
           NTO = NTO + 1
           CALL ODZZ(J0,NTO,'@','Display performance data '//
     ;         'for last cycle@',0,JX)
           NTO = NTO + 1
           CALL ODZZ(J0,NTO,'@','Write '//
     ;               'last cycle data to #@',1,J0F)
               CALL FNZZ(J0F,1,'Output File',
     ;                         11,32,'ESR',2,OFILE,JX)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ODRVV(JKTP,NCP,I)
c
c      Defines option for plotting real variable RW(I) as the NCPth child of
c      the option group having pointer JKTP.
c------------------------------------------------------------------------------
       CHARACTER*1 NAME(61)
c-----------------------------------------------------------------------------
       CALL GRVNVV(I,NAME,NCN)
       NAME(NCN+1) = '@'
       CALL ODZZ(JKTP,NCP,'@',NAME,0,JX)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  ACTRVV(I,NOPA)
c
c      Run actions
c        1 Execute tasks
c        2 Return to model/operation stup
c        3 Quit
c------------------------------------------------------------------------------
       IF (I.EQ.1)  THEN
c            set to check data and execute
               NOPA = 1
           ELSEIF (I.EQ.2) THEN
c            abort and return to model setup
               NOPA = 2
           ELSEIF (I.EQ.3) THEN
c            quit
               CALL QUITVV
c            no quit if here
               NOPA = 0
           ELSE
c            interfaced error stop
               CALL STOPZZ
           ENDIF
       RETURN
       END
c******************************************************************************
