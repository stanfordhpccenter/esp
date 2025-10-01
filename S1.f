c******************************************************************************
c
c      Model setup routines
c
c******************************************************************************
c
       SUBROUTINE SETMVV
c
c      ESP model setup
c------------------------------------------------------------------------------
c    ESP work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
c------------------------------------------------------------------------------
c    Setup file name
       CHARACTER*32  SUFILE
       COMMON /SUFVV/SUFILE
c------------------------------------------------------------------------------
c    Gas properties file name
       CHARACTER*32  PMFILE
       COMMON /PMFVV/PMFILE
c------------------------------------------------------------------------------
c    Flame geometry file name
       CHARACTER*32  FGFILE
       COMMON /FGFVV/FGFILE
c------------------------------------------------------------------------------
c    Inlet valve program file name
       CHARACTER*32  IVFILE
       COMMON /IVFVV/IVFILE
c------------------------------------------------------------------------------
c    Exhaust valve program file name
       CHARACTER*32  EVFILE
       COMMON /EVFVV/EVFILE
c------------------------------------------------------------------------------
c    Special name
       CHARACTER*1 NAME(64)
c------------------------------------------------------------------------------
c    designators
       CHARACTER*48    NEGM,NEOP,NEMP,NAMB,NICC,NECC,NFLM,NTBM,NVVC,
     ;                 NPCC,NPDS,NEVP,NIVP,
     ;                 NEGR,NFGT,NFPM,NHTM,NIMM,NEMM,NVRA,NRUN

       CHARACTER*48    NGPM
c      CHARACTER*1     NGPM(48)
c------------------------------------------------------------------------------
c    run setup
       COMMON /NRUNVV/ NRUN
c    engine geometry model
       COMMON /NEGMVV/ NEGM
c    valve reference areas
       COMMON /NVRAVV /NVRA
c    piston/cylinder conventional crankshaft
       COMMON /NPCCVV/ NPCC
c    piston/cylinder dual stroke
       COMMON /NPDSVV/ NPDS
c    operating parameters
       COMMON /NEOPVV/ NEOP
c    ambient conditions
       COMMON /NAMBVV/ NAMB
c    valve control
       COMMON /NVVCVV /NVVC
c    EGR
       COMMON /NEGRVV /NEGR
c    model parameters
       COMMON /NEMPVV/ NEMP
c    intake valve cosine-flat-cosine
       COMMON /NICCVV /NICC
c    exhaust valve cosine-flat-cosine
       COMMON /NECCVV /NECC
c    inlet valve program name
       COMMON /NIVPVV/NIVP
c    exhaust valve program name
       COMMON /NEVPVV/NEVP
c    flow model
       COMMON /NFLMVV /NFLM
c    turbulence model
       COMMON /NTBMVV /NTBM
c    flame geometry table
       COMMON /NFGTVV /NFGT
c    flame propagation model
       COMMON /NFPMVV /NFPM
c    gas properties model
       COMMON /NGPMVV /NGPM
c    heat transfer model
       COMMON /NHTMVV /NHTM
c    intake manifold model
       COMMON /NIMMVV /NIMM
c    exhaust manifold model
       COMMON /NEMMVV /NEMM
c-----------------------------------------------------------------------------
c    Flame geometry table
       DIMENSION NCWFGT(3)
       DIMENSION KRFG(2),VMINFG(2),VMAXFG(2)
       DIMENSION FGTABR(11),FGTABE(11,2)
       COMMON  /FGMTVV /FGTABE
c------------------------------------------------------------------------------
c    user valve programs
       DIMENSION IVDI(48),FVDI(48)
       DIMENSION IVDE(48),FVDE(48)
       COMMON  /UVIDVV/ NDI,FMXI,IVDI,FVDI
       COMMON  /UVEDVV/ NDE,FMXE,IVDE,FVDE
c-----------------------------------------------------------------------------
c    Flame geometry table data
       DATA    NCWFGT/22,22,22/
       DATA    KRFG/12,4/
       DATA    VMINFG/0.,0./
       DATA    VMAXFG/1.,0./
       DATA    FGTABR/0.,.1,.2,.3,.4,.5,.6,.7,.8,.9,1./
c------------------------------------------------------------------------------
       CALL AOZZ('@','Run with this setup@')
       CALL AOZZ('@','Use another setup file@')
       CALL AOZZ('@','Create totally new setup@')
       CALL AOZZ('@','Quit ESP@')
c
       CALL BIGZZ(0,0,'@','Run Setup@',4,J0)
         CALL GDZZ(J0,48,NRUN,JX)
         CALL HOZZ(J0,0,'@','Help@',11,JX)
c
c       operating parameters
         CALL BIGZZ(J0,1,'@','Operating Parameters@',5,J0O)
           CALL GDZZ(J0O,48,NEOP,JX)
c
           CALL IVZZVV(J0O,1,10,1,0,0,IW(10),JX)
c
           CALL BEOGZZ(J0O,2,'@','Firing@',2,IW(6),J0OF)
             CALL ODZZ(J0OF,1,'@','Ignition at # degrees '//
     ;                            'after TDC compression@',1,J0OFI)
               CALL IVZZ(J0OFI,1,'IAIG',4,12,0,719,IW(11),JX)
             CALL ODZZ(J0OF,2,'@','Motoring@',0,JX)
c
           CALL BVGZZ(J0O,3,'@','Ambient Conditions@',3,J0OA)
             CALL GDZZ(J0OA,48,NAMB,JX)
             CALL RVZZVV(J0OA,1,202,1,0.,0.,RW(202),JX)
             CALL RVZZVV(J0OA,2,203,1,0.,0.,RW(203),JX)
             CALL IVZZVV(J0OA,3,12,1,0,0,IW(12),JX)
c
           CALL BVGZZ(J0O,4,'@','EGR@',2,J0OR)
             CALL GDZZ(J0OR,48,NEGR,JX)
             CALL IVZZVV(J0OR,1,13,12,0,90,IW(13),JX)
             CALL IVZZVV(J0OR,2,14,1,0,0,IW(14),JX)
c
           CALL BVGZZ(J0O,5,'@','Valve Control@',6,J0OV)
             CALL GDZZ(J0OV,48,NVVC,JX)
             CALL HOZZ(J0OV,0,'@','Help@',18,JX)
             DO I=2,5
               CALL IVZZVV(J0OV,(I-1),I,12,0,719,IW(I),JX)
               ENDDO
             DO I=15,16
               CALL IVZZVV(J0OV,(I-10),I,1,0,0,IW(I),JX)
               ENDDO
c
c       engine geometry
         CALL BIGZZ(J0,2,'@','Engine Geometry@',4,J0E)
           CALL GDZZ(J0E,48,NEGM,JX)
           CALL BHOGZZ(J0E,0,'@','Help@',2,J0EH)
             CALL HOZZ(J0EH,1,'@','Valve program help@',3,JX)
             CALL HOZZ(J0EH,2,'@','Piston program help@',2,JX)
c
c        inlet valve program
           IF (NDI.GT.0)  THEN
c                loaded setup includes a valve program
                   CALL ENAMVV('@',NIVP,NAME)
                   NIVPOP = 4
               ELSE
c                loaded setup does not include a valve program table
                   NIVPOP = 3
               ENDIF
           CALL BEOGZZ(J0E,1,'@','Intake Valve Program@',NIVPOP,
     ;                  IW(18),J0EI)
             CALL HOZZ(J0EI,0,'@','Help@',3,JX)
             CALL ODZZ(J0EI,1,'@','Cosine@',0,JX)
             CALL BVGZZ(J0EI,2,'@','Cosine-constant-cosine@',2,J0EIF)
               CALL GDZZ(J0EIF,48,NICC,JX)
               CALL HOZZ(J0EIF,0,'@','Help@',3,JX)
               DO I=24,25
                 CALL IVZZVV(J0EIF,I-23,I,12,4,90,IW(I),JX)
                 ENDDO
             CALL ODZZ(J0EI,3,'@','Use #@',1,J0EIF)
               CALL FNZZ(J0EIF,1,'Valve Program File',18,32,'ESV',
     ;                  1,IVFILE,JX)
             IF (NIVPOP.EQ.4)  CALL ODZZ(J0EI,4,'@',NAME,0,JX)
c
c        exhaust valve program
           IF (NDE.GT.0)  THEN
c                loaded setup includes a valve program
                   CALL ENAMVV('@',NEVP,NAME)
                   NEVPOP = 4
               ELSE
c                loaded setup does not include a valve program
                   NEVPOP = 3
               ENDIF
           CALL BEOGZZ(J0E,2,'@','Exhaust Valve Program@',NEVPOP,
     ;                 IW(19),J0EE)
             CALL HOZZ(J0EE,0,'@','Help@',3,JX)
             CALL ODZZ(J0EE,1,'@','Cosine@',0,JX)
             CALL BVGZZ(J0EE,2,'@','Cosine-constant-cosine@',2,J0EEF)
               CALL GDZZ(J0EEF,48,NECC,JX)
               CALL HOZZ(J0EEF,0,'@','Help@',3,JX)
               DO I=26,27
                 CALL IVZZVV(J0EEF,I-25,I,12,4,90,IW(I),JX)
                 ENDDO
             CALL ODZZ(J0EE,3,'@','Use #@',1,J0EEF)
               CALL FNZZ(J0EEF,1,'Valve Program File',18,32,'ESV',
     ;                  1,EVFILE,JX)
             IF (NEVPOP.EQ.4)  CALL ODZZ(J0EE,4,'@',NAME,0,JX)
c
           CALL BVGZZ(J0E,3,'@','Valve Reference Areas@',2,J0EV)
             CALL GDZZ(J0EV,48,NVRA,JX)
             CALL HOZZ(J0EV,0,'@','Help@',18,JX)
             DO I=218,219
               CALL RVZZVV(J0EV,I-217,I,1,0.,0.,RW(I),JX)
               ENDDO
c
           CALL BIGZZ(J0E,4,'@','Piston/Cylinder@',4,J0EP)
             CALL GDZZ(J0EP,48,NPCC,JX)
             CALL HOZZ(J0EP,0,'@','Help@',2,JX)
               DO I=211,213
                 CALL RVZZVV(J0EP,(I-210),I,1,0.,0.,RW(I),JX)
                 ENDDO
             CALL BEOGZZ(J0EP,4,'@','Piston Program@',2,IW(20),J0EPP)
               CALL ODZZ(J0EPP,1,'@','Conventional; '//
     ;             'connecting rod length # m@',1,J0EPPC)
                 CALL RVZZ(J0EPPC,1,'Lrod',4,1,0.,0.,RW(216),JX)
               CALL ODZZ(J0EPP,2,'@','Dual stroke sinusoid; '//
     ;             'expansion stroke # m@',1,J0EPPD)
                 CALL RVZZ(J0EPPD,1,'Sexp',4,1,0.,0.,RW(217),JX)
c
c        model parameters
           CALL BIGZZ(J0,3,'@','Model Parameters@',8,J0M)
             CALL GDZZ(J0M,48,NEMP,J0MG)

c          gas properties
             IF (IW(17).EQ.2) THEN
c                  set option load for loaded properties data
                     CALL ENAMVV('@',NGPM,NAME)
                     NGPOP = 2
                 ELSE
c                  read file only
                     NGPOP = 1
                 ENDIF
             CALL BEOGZZ(J0M,1,'@','Gas Properties@',NGPOP,IW(17),J0MG)
               CALL HOZZ(J0MG,0,'@','Help@',10,JX)
               CALL ODZZ(J0MG,1,'@','Use #@',1,J0MGF)
                 CALL FNZZ(J0MGF,1,'Properties File',15,32,'ESJ',
     ;                     1,PMFILE,JX)
               IF (NGPOP.EQ.2) CALL ODZZ(J0MG,2,'@',NAME,0,JX)
c
             CALL BVGZZ(J0M,2,'@','Valve Flow Model@',4,J0MV)
               CALL GDZZ(J0MV,48,NFLM,JX)
               CALL HOZZ(J0MV,0,'@','Help@',5,JX)
               DO I=221,224
                 CALL RVZZVV(J0MV,I-220,I,9,0.,1.,RW(I),JX)
                 ENDDO
c
             CALL BVGZZ(J0M,3,'@','Heat Transfer Model@',13,J0MQ)
               CALL GDZZ(J0MQ,48,NHTM,JX)
               CALL HOZZ(J0MQ,0,'@','Help@',4,JX)
               DO I=251,260
                 CALL RVZZVV(J0MQ,I-250,I,4,0.,0.,RW(I),JX)
                 ENDDO
               DO I=21,23
                 CALL IVZZVV(J0MQ,I-10,I,1,0,0,IW(I),JX)
                 ENDDO
c
             CALL BVGZZ(J0M,4,'@','Turbulence Model@',10,J0MT)
               CALL GDZZ(J0MT,48,NTBM,JX)
               CALL HOZZ(J0MT,0,'@','Help@',6,JX)
               DO I=271,280
                 CALL RVZZVV(J0MT,I-270,I,1,0.,0.,RW(I),JX)
                 ENDDO
c
             CALL BEOGZZ(J0M,5,'@','Flame Geometry Model@',
     ;                   3,IW(38),J0MF)
               CALL BHOGZZ(J0MF,0,'@','Help@',2,J0MFH)
                 CALL HOZZ(J0MFH,1,'@','Flame Geometry Model@',7,JX)
                 CALL HOZZ(J0MFH,2,'@','Flame Geometry Table@',8,JX)
               CALL ODZZ(J0MF,1,'@','Use #@',1,J0MFL)
                 CALL FNZZ(J0MFL,1,'Flame Geometry Table File',25,
     ;                        32,'ESF',1,FGFILE,JX)
               CALL BTABZZ(J0MF,2,'@','Flame Geometry Table@',
     ;             11,1,2,NCWFGT,
     ;            'fraction of volume containing burned gas@@'//
     ;            'fraction of heat transfer area in burned zone '//
     ;                 'at TDC@@'//
     ;            'ratio of projected flame area at TDC to bore area@@',
     ;             KRFG,VMINFG,VMAXFG,FGTABR,FGTABE,IW(37),J0MFT)
                 CALL GDZZ(J0MFT,48,NFGT,JX)
                 CALL HOZZ(J0MFT,0,'@','Help@',8,JX)
                 CALL BEOGZZ(J0MFT,0,'@','Table save option@',
     ;                           2,IW(36),J0MFTS)
                   CALL ODZZ(J0MFTS,1,'@','Save table in #@',1,J0MFTX)
                     CALL FNZZ(J0MFTX,1,'File',4,
     ;                        32,'ESF',2,FGFILE,JX)
                   CALL ODZZ(J0MFTS,2,'@','No table save@',0,JX)
               CALL ODZZ(J0MF,3,'@','Cylindrical burn@',0,JX)
c
             CALL BVGZZ(J0M,6,'@','Flame Propagation Model@',4,J0MP)
               CALL GDZZ(J0MP,48,NFPM,JX)
               CALL HOZZ(J0MP,0,'@','Help@',9,JX)
               CALL RVZZVV(J0MP,1,231,9,0.,1.0,RW(231),JX)
               CALL RVZZVV(J0MP,2,232,1,0.,0.,RW(232),JX)
               CALL RVZZVV(J0MP,3,233,1,0.,0.,RW(233),JX)
               CALL RVZZVV(J0MP,4,234,9,0.,1.,RW(234),JX)
c
             CALL BEOGZZ(J0M,7,'@','Intake Manifold Model@',
     ;                           2,IW(39),J0MI)
               CALL HOZZ(J0MI,0,'@','Help@',16,JX)
               CALL ODZZ(J0MI,1,'@','No manifold@',0,JX)
               CALL BVGZZ(J0MI,2,'@','Manifold included@',10,J0MIV)
                 CALL GDZZ(J0MIV,48,NIMM,JX)
                 DO I=301,303
                   CALL RVZZVV(J0MIV,I-300,I,12,0.,0.95,RW(I),JX)
                   ENDDO
                 DO I=304,309
                   CALL RVZZVV(J0MIV,I-300,I,1,0.,0.,RW(I),JX)
                   ENDDO
                 CALL IVZZVV(J0MIV,10,28,12,1,6,IW(28),JX)
c
             CALL BEOGZZ(J0M,8,'@','Exhaust Manifold Model@',
     ;                           2,IW(40),J0ME)
               CALL HOZZ(J0ME,0,'@','Help@',17,JX)
               CALL ODZZ(J0ME,1,'@','No manifold@',0,JX)
               CALL BVGZZ(J0ME,2,'@','Manifold included@',10,J0MEV)
                 CALL GDZZ(J0MEV,48,NEMM,JX)
                 DO I=311,313
                   CALL RVZZVV(J0MEV,I-310,I,12,0.,0.95,RW(I),JX)
                   ENDDO
                 DO I=314,319
                   CALL RVZZVV(J0MEV,I-310,I,1,0.,0.,RW(I),JX)
                    ENDDO
                 CALL IVZZVV(J0MEV,10,29,12,1,6,IW(29),JX)
           CALL BEOGZZ(J0,4,'@','Setup file save option@',
     ;                           2,IW(35),J0S)
             CALL ODZZ(J0S,1,'@','Save setup in #@',1,J0SF)
               CALL FNZZ(J0SF,1,'File',4,
     ;                        32,'ESS',2,SUFILE,JX)
             CALL ODZZ(J0S,2,'@','No setup save@',0,JX)
c
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ACTMVV(I,NOPA)
c
c      Setup ACTIONS
c
c      I=1 Run with this data
c        2 Use another setup file
c        3 Create totally new setup
c        4 Quit ESP
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
       SUBROUTINE GIVNVV(I,NAME,NCN)
c
c      Removes trailing blanks from the name of integer variable I, returns
c      the shortened NAME and the number of characters NCN.
c------------------------------------------------------------------------------
       CHARACTER*1 NAME(60),CODE(4)
c-----------------------------------------------------------------------------
c    ESP work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
c------------------------------------------------------------------------------
c    get the name
       CALL IWIDVV(I,CODE,NAME)
c    remove trailing blanks
       DO 9 L=1,60
           NCN = 61 - L
           IF (NAME(NCN).NE.' ') RETURN
9          CONTINUE
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE GRVNVV(I,NAME,NCN)
c
c      Removes trailing blanks from the name of real variable I, returns
c      the shortened NAME and the number of characters NCN.
c------------------------------------------------------------------------------
       CHARACTER*1 NAME(60),CODE(4)
c-----------------------------------------------------------------------------
c    ESP work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
c------------------------------------------------------------------------------
c    get the name
       CALL RWIDVV(I,CODE,NAME)
c    remove trailing blanks
       DO 9 L=1,60
           NCN = 61 - L
           IF (NAME(NCN).NE.' ') RETURN
9          CONTINUE
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
       IF (K.EQ.1)  THEN
c            overview
               CALL HOVRVV
           ELSEIF (K.EQ.2)  THEN
c            piston program
               CALL HPISVV
           ELSEIF (K.EQ.3)  THEN
c            valve program
               CALL HVALVV
           ELSEIF (K.EQ.4)  THEN
c            heat transfer model
               CALL HHTMVV
           ELSEIF (K.EQ.5)  THEN
c            valve flow model
               CALL HFLOVV
           ELSEIF (K.EQ.6)  THEN
c            turbulence model
               CALL HTRBVV
           ELSEIF (K.EQ.7)  THEN
c            flame geometry model
               CALL HFGMVV
           ELSEIF (K.EQ.8)  THEN
c            flame geometry table
               CALL HFGTVV
           ELSEIF (K.EQ.9)  THEN
c            flame propagation model
               CALL HFPMVV
           ELSEIF (K.EQ.10)  THEN
c            thermodynamic properties model
               CALL HPROVV
           ELSEIF (K.EQ.11)  THEN
c            setup help
               CALL HSETVV
               CALL HINTVV
           ELSEIF (K.EQ.12)  THEN
c            run help
               CALL HRUNVV
           ELSEIF (K.EQ.13)  THEN
c            matlab help
               CALL HMLPVV
           ELSEIF (K.EQ.14)  THEN
c            files help
               CALL HFILVV
           ELSEIF (K.EQ.15)  THEN
c            tuning the model
               CALL HTUNVV
           ELSEIF (K.EQ.16)  THEN
c            intake manifold model
               CALL HIMMVV
           ELSEIF (K.EQ.17)  THEN
c            exhaust manifold model
               CALL HEMMVV
           ELSEIF (K.EQ.18)  THEN
c            valve control
               CALL HVVCVV
           ELSEIF (K.EQ.20)  THEN
c            instruction on entire model
               CALL HINSVV
           ELSEIF (K.EQ.30)  THEN
c            convergence
               CALL HCONVV
           ELSEIF (K.EQ.40)  THEN
c            5-line plots
               CALL HPL5VV
               CALL HPRSVV
           ELSE
               CALL MESSZZ('#','#Program error; help missing.  '//
     ;                    'Report to wcr@thermo.stanford.edu##')
           ENDIF
       CALL PAUSZZ(1)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE RVZZVV(JKTP,NCP,IRW,KR,RVMIN,RVMAX,RV,JKT)
c
c      Loads ESP real work array variable number IRW
c-----------------------------------------------------------------------------
       CHARACTER*1 NAME(60)
c-----------------------------------------------------------------------------
       CALL GRVNVV(IRW,NAME,NCN)
       CALL RVZZ(JKTP,NCP,NAME,NCN,KR,RVMIN,RVMAX,RV,JKT)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE IVZZVV(JKTP,NCP,IIW,KR,IVMIN,IVMAX,IV,JKT)
c
c      Loads ESP integer work array variable number IIW
c-----------------------------------------------------------------------------
       CHARACTER*1 NAME(60)
c-----------------------------------------------------------------------------
       CALL GIVNVV(IIW,NAME,NCN)
       CALL IVZZ(JKTP,NCP,NAME,NCN,KR,IVMIN,IVMAX,IV,JKT)
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
82     FORMAT (' ID: ',10I7)
       WRITE (19,83) (RD(I),I=1,NR)
83     FORMAT (' RD: ',5(1PE14.6))
       WRITE (19,84) (CD(I),I=1,NC)
84     FORMAT (' CD: ',70A1)
       CLOSE(19)
       CALL QUITVV
       END
c******************************************************************************
c
       SUBROUTINE ENAMVV(ESC,NAME,NAMES)
c
c      Forms an RSC-terminated string in NAMES from NAME
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
