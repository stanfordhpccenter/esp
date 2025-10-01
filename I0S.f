c******************************************************************************
c
c      This file contains a test setup for an arbitrary interface.
c
c      Other files to be compiled and linked with this I0.FOR file:
c
c          I1.FOR  required interface routines MESSZZ, PAUSZZ, and GETZZ
c          I2.FOR  support routines for I1.FOR
c          IB.FOR  general interface library routines
c
c******************************************************************************
c    setup data storage
       PARAMETER   (NCMAX = 4096)
       PARAMETER   (NIMAX = 2048)
       PARAMETER   (NRMAX = 256)
       CHARACTER*1 CD
       COMMON /SETCZZ/ CD(NCMAX)
       COMMON /SETIZZ/ ID(NIMAX)
       COMMON /SETRZZ/ RD(NRMAX)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
c------------------------------------------------------------------------------
c    application-specific subroutines called by GETZZ
       EXTERNAL    SETUP,HELP,LOAD,SAVE,QUIT,ACTION
c------------------------------------------------------------------------------
c    test data
       DIMENSION NOPI(3),I2JKT(2048)
       DIMENSION NCWFGT(3)
       CHARACTER*30   SGDES,EGDES,OGDES,PODES,TABDES
       CHARACTER*8    ENAME
       CHARACTER*32   PFILE
       COMMON /DEMO/BORE,PMAX,VMAX,NCYL,NOPF,NOPI,ISPARK,LV1,LV2,
     ; NCWFGT, ENAME,PFILE,SGDES,EGDES,OGDES,PODES,TABDES
       DIMENSION FGTABR(11),FGTABE(11,2)
       COMMON  /FGTAB /FGTABR,FGTABE
c------------------------------------------------------------------------------
c    i/o units
       COMMON /IOKK/ KUOUT, KUIN
c------------------------------------------------------------------------------
c    line length for message box
       COMMON /MESBZZ/ LMAX
c------------------------------------------------------------------------------
c    GUI indexing
       COMMON /TBLKK/  I2JKT, NTOBJ, NTHLP
c------------------------------------------------------------------------------
       KUOUT = 6
       KUIN  = 5
       LMAX = 40
c------------------------------------------------------------------------------
       ID(2) = NCMAX
       ID(3) = NIMAX
       ID(4) = NRMAX
c    file
       OPEN(9,FILE='test')
c    message test
       CALL MESSZZ('@','This is test line 1@'//
     ;                 'and this is the last line.@@')

c    build arrays and get changes with no values loaded
       DO 3 J=1,3
           NCWFGT(J) = 20 + J
3      CONTINUE
c     Load default values
       DO 5 I=1,11
           FGTABE(I,1) = 0.2*(I-1)
           FGTABE(I,2) = 0.3*(I-1)
5      CONTINUE
       DO 7 I=1,11
           FGTABR(I) = 0.1*(I-1)
7      CONTINUE
       CALL GETZZ(0,SETUP,HELP,ACTION)
c       CALL STBLKK
       write(11,*)'NTOBJ=',NTOBJ,'NTHLP=',NTHLP
       DO II=1,NTOBJ+NTHLP
         write(11,*)II,I2JKT(II),ID(I2JKT(II)),ID(I2JKT(II)+1)
       ENDDO
c
c    work array dump
       WRITE (9,82) (ID(I),I=1,NI)
       WRITE (9,83) (RD(I),I=1,NR)
       WRITE (9,84) (CD(I),I=1,NC)


c
c    display/write the new data
       CALL MESBKK
       write (KUOUT,*)  ' '
       WRITE (KUOUT,*) 'TABDES = ',TABDES
       WRITE (KUOUT,*) 'SGDES = ',SGDES
       WRITE (KUOUT,*) 'EGDES = ',EGDES
       WRITE (KUOUT,*) 'PODES = ',PODES
       WRITE (KUOUT,*) 'PFILE = ',PFILE
       WRITE (KUOUT,*) 'ENAME = ',ENAME
       WRITE (KUOUT,*) 'NCYL = ',NCYL
       WRITE (KUOUT,*) 'BORE = ',BORE
       WRITE (KUOUT,*) 'NOPF = ',NOPF
       WRITE (KUOUT,*) 'ISPARK = ',ISPARK
       WRITE (KUOUT,*) 'NOPI = ',NOPI
       WRITE (KUOUT,*) 'PMAX = ',PMAX
       WRITE (KUOUT,*) 'VMAX = ',VMAX
       CALL MESEKK

c
       WRITE (9,*) 'TABDES = ',TABDES
       WRITE (9,*) 'SGDES = ',SGDES
       WRITE (9,*) 'EGDES = ',EGDES
       WRITE (9,*) 'PODES = ',PODES
       WRITE (9,*) 'PFILE = ',PFILE
       WRITE (9,*) 'ENAME = ',ENAME
       WRITE (9,*) 'NCYL = ',NCYL
       WRITE (9,*) 'BORE = ',BORE
       WRITE (9,*) 'NOPF = ',NOPF
       WRITE (9,*) 'ISPARK = ',ISPARK
       WRITE (9,*) 'NOPI = ',NOPI
       WRITE (9,*) 'PMAX = ',PMAX
       WRITE (9,*) 'VMAX = ',VMAX
c
c     Load default values
       SGDES = 'Test setup 1        '
       EGDES = 'Engine geometry 1   '
       PODES = 'Usual plots         '
       TABDES = 'Weird flame table   '
       PFILE = 'PROPS.ESJ'
       ENAME = 'GMI6    '
       NCYL = 2
       BORE = .03
       NOPF = 1
       NOPM = 2
       ISPARK = 420
       NOPI(1) = 1
       NOPI(2) = 1
       NOPI(3) = 0
       PMAX = 5.E7
       VMAX = .02
       LV1 = 33
       LV2 = 44
       LVV1 = 11
       LVV2 = 22


c        display the new data
           CALL MESBKK
           write (KUOUT,*)  ' '
           WRITE (KUOUT,*) 'TABDES = ',TABDES
           WRITE (KUOUT,*) 'SGDES = ',SGDES
           WRITE (KUOUT,*) 'EGDES = ',EGDES
           WRITE (KUOUT,*) 'PODES = ',PODES
           WRITE (KUOUT,*) 'PFILE = ',PFILE
           WRITE (KUOUT,*) 'ENAME = ',ENAME
           WRITE (KUOUT,*) 'NCYL = ',NCYL
           WRITE (KUOUT,*) 'BORE = ',BORE
           WRITE (KUOUT,*) 'NOPF = ',NOPF
           WRITE (KUOUT,*) 'NOPM = ',NOPM
           WRITE (KUOUT,*) 'ISPARK = ',ISPARK
           WRITE (KUOUT,*) 'NOPI = ',NOPI
           WRITE (KUOUT,*) 'PMAX = ',PMAX
           WRITE (KUOUT,*) 'VMAX = ',VMAX
           CALL MESEKK
c
c    build arrays and get changes
       CALL GETZZ(1,SETUP,HELP,ACTION)
c
c    display the new data
       CALL MESBKK
       write (KUOUT,*)  ' '
       WRITE (KUOUT,*) 'TABDES = ',TABDES
       WRITE (KUOUT,*) 'SGDES = ',SGDES
       WRITE (KUOUT,*) 'PFILE = ',PFILE
       WRITE (KUOUT,*) 'ENAME = ',ENAME
       WRITE (KUOUT,*) 'NCYL = ',NCYL
       WRITE (KUOUT,*) 'BORE = ',BORE
       WRITE (KUOUT,*) 'NOPF = ',NOPF
       WRITE (KUOUT,*) 'NOPM = ',NOPM
       WRITE (KUOUT,*) 'ISPARK = ',ISPARK
       WRITE (KUOUT,*) 'NOPI = ',NOPI
       WRITE (KUOUT,*) 'PMAX = ',PMAX
       WRITE (KUOUT,*) 'VMAX = ',VMAX
       CALL MESEKK
c
c    work array dump
       WRITE (9,82) (ID(I),I=1,NI)
82     FORMAT (' ID: ',15I5)
       WRITE (9,83) (RD(I),I=1,NR)
83     FORMAT (' RD: ',15F5.2)
       WRITE (9,84) (CD(I),I=1,NC)
84     FORMAT (' CD: ',75A1)
c
       CLOSE(9)
       END

c******************************************************************************
c
       SUBROUTINE SETUP
c
c      Test setup routine
c------------------------------------------------------------------------------
       PARAMETER   (NCMAX = 4096)
       PARAMETER   (NIMAX = 2048)
       PARAMETER   (NRMAX = 256)
       CHARACTER*1 CD
       COMMON /SETCZZ/ CD(NCMAX)
       COMMON /SETIZZ/ ID(NIMAX)
       COMMON /SETRZZ/ RD(NRMAX)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
c------------------------------------------------------------------------------
c    test data
       DIMENSION NOPI(3)
       DIMENSION NCWFGT(3)
       CHARACTER*30   SGDES,EGDES,OGDES,PODES,TABDES
       CHARACTER*8    ENAME
       CHARACTER*32   PFILE
       COMMON /DEMO/BORE,PMAX,VMAX,NCYL,NOPF,NOPI,ISPARK,LV1,LV2,
     ; NCWFGT, ENAME,PFILE,SGDES,EGDES,OGDES,PODES,TABDES
       DIMENSION FGTABR(11),FGTABE(11,2)
       COMMON  /FGTAB /FGTABR,FGTABE
c------------------------------------------------------------------------------
c    flame table controls
       DIMENSION   KRC(2),VMINCT(2),VMAXCT(2)
       DATA        KRC   /12,4/
       DATA        VMINCT/0.,0./
       DATA        VMAXCT/1.,0./
c------------------------------------------------------------------------------
c    character widths for the flame geometry table

       CALL AOZZ('@','Special@')

       CALL BIGZZ(0,0,'@','Interface test@',2,JGOD)
         CALL BIGZZ(JGOD,1,'@','Setup data@',3,JKT0)
           CALL GDZZ(JKT0,30,SGDES,JKT0D)
           CALL BVGZZ(JKT0,1,'@','Engine model@',4,JKT1)
              CALL GDZZ(JKT1,30,EGDES,JKT1D)
              CALL HOZZ(JKT1,0,'@','Engine help@',1,JKT1H)
              CALL CVZZ(JKT1,1,'engine name',11,8,ENAME,JV1)
              CALL IVZZ(JKT1,2,'cylinders',9,12,1,6,NCYL,JV2)
              CALL RVZZ(JKT1,3,'bore (m)',8,1,RX,RX,BORE,JV3)
              CALL FNZZ(JKT1,4,'properties file',15,32,'ESJ',PFILE,JV4)
           CALL BEOGZZ(JKT0,2,'@','Firing options@',4,NOPF,JKTEO)
              CALL HOZZ(JKTEO,0,'@','Firing help@',2,JKH2)
              CALL ODZZ(JKTEO,1,'@','Motoring@',0,JEO1)
              CALL ODZZ(JKTEO,2,'@','Spark at A = # deg.@',1,JEO2)
                 CALL IVZZ(JEO2,1,'Aignition',9,12,0,720,ISPARK,J3OV1)
c sck
              CALL BIGZZ(JKTEO,3,'@','Option values@',3,JKTEOV)
                 CALL IVZZ(JKTEOV,1,'KEGS',4,0,0,0,LV1,JKVO2)
                 CALL IVZZ(JKTEOV,2,'PEGS',4,0,0,0,LV2,JKVO2)
                      CALL BIGZZ(JKTEOV,3,'@','Sub values@',2,JKTEOVV)
                        CALL IVZZ(JKTEOVV,1,'KEX',4,0,0,0,LVV1,JKVOV2)
                        CALL IVZZ(JKTEOVV,2,'PEX',4,0,0,0,LVV2,JKVOV2)

c sck

              CALL BEOGZZ(JKTEO,4,'@','Sub-options@',2,NOPM,JKTEOM)
                 CALL ODZZ(JKTEOM,1,'@','More@',0,JEOM1)
                 CALL ODZZ(JKTEOM,2,'@','Less@',0,JEOM2)
c sck
           CALL BIOGZZ(JKT0,3,'@','Plot options@',4,2,NOPI,JKTIO)
              CALL HOZZ(JKTIO,0,'@','Plot help@',2,JKH2)
              CALL GDZZ(JKTIO,30,PODES,JKTIOD)
              CALL ODZZ(JKTIO,1,'@','Indicator diagram@',0,JIO1)
              CALL ODZZ(JKTIO,2,'@',
     ;             'P axis maximum P = # (Pa) and at L = # (l)@',2,JIO2)
                 CALL RVZZ(JIO2,1,'Pmax',4,1,X,X,PMAX,JIO2V1)
                 CALL RVZZ(JIO2,2,'Pmax',4,1,X,X,PMAX,JIO2V2)
              CALL ODZZ(JKTIO,3,'@',
     ;                 'V axis maximum V = # (m^3)@',1,JIO3)
                 CALL RVZZ(JIO3,1,'Vmax',4,1,X,X,VMAX,JIO3V1)
c sck
            CALL BEOGZZ(JKTIO,4,'@','Subplot options@',2,NOPIX,JKTIOX)
              CALL HOZZ(JKTIOX,0,'@','Plot help@',2,JKH2X)
C              CALL GDZZ(JKTIOX,20,PODES,JKTIOXD)
              CALL ODZZ(JKTIOX,1,'@','Indicator diagram@',0,JIOX1)
              CALL ODZZ(JKTIOX,2,'@','Diagramed indicator@',0,JIOX2)

           CALL BHOGZZ(JKT0,0,'@','Help@',4,JKTH)
               CALL HOZZ(JKTH,1,'@','Engine model@',1,JKH1)
               CALL HOZZ(JKTH,2,'@','Firing model@',2,JKH2)
               CALL HOZZ(JKTH,3,'@','MATLAB plots@',3,JKH3)
c sck
               CALL BHOGZZ(JKTH,4,'@','More Help@',3,JKTHX)
                   CALL HOZZ(JKTHX,1,'@','Firing more@',2,JKHX1)
                   CALL HOZZ(JKTHX,2,'@','MATLAB more@',3,JKHX2)
c sck
                    CALL BHOGZZ(JKTHX,3,'@','More Help@',2,JKTHXX)
                        CALL HOZZ(JKTHXX,1,'@','Firing more@',2,JKHXX1)
                        CALL HOZZ(JKTHXX,2,'@','MATLAB more@',3,JKHXX2)
c
          CALL BTABZZ(JGOD,2,'@','Flame geometry table@',
     ;             11,1,2,NCWFGT,
     ;            'fraction of volume containing burned gas@@'//
     ;            'fraction of heat transfer area in burned zone '//
     ;                 'at TDC@@'//
     ;            'ratio of projected flame area at TDC to bore area@@',
     ;             K,KRC,VMINCT,VMAXCT,FGTABR,FGTABE,JKTAB)
            CALL GDZZ(JKTAB,30,TABDES,JKTTD)
            CALL HOZZ(JKTAB,0,'@','Help@',2,JKH2)
       RETURN


c    work array dump
80     WRITE (9,82) (ID(I),I=1,NI)
82     FORMAT (' ID: ',15I5)
       WRITE (9,83) (RD(I),I=1,NR)
83     FORMAT (' RD: ',15F5.2)
       WRITE (9,84) (CD(I),I=1,NC)
84     FORMAT (' CD: ',75A1)
c

       END
c******************************************************************************
c
       SUBROUTINE HELP(K)
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
c       CALL PAUSZZ
        CALL HVP1VV(K)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SAVE(K)
c
c      Test save routine
c------------------------------------------------------------------------------
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE LOAD(K)
c
c      Test load routine
c------------------------------------------------------------------------------
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE QUIT
c
c      Test quit routine
c------------------------------------------------------------------------------
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE STOPZZ
c
c      Error stop routine
c------------------------------------------------------------------------------
c    setup data storage
       PARAMETER   (NCMAX = 4096)
       PARAMETER   (NIMAX = 2048)
       PARAMETER   (NRMAX = 128)
       CHARACTER*1 CD
       COMMON /SETCZZ/ CD(128)
       COMMON /SETIZZ/ ID(128)
       COMMON /SETRZZ/ RD(128)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
c------------------------------------------------------------------------------
c    work array dump
       WRITE (19,*) (ID(I),I=1,NI)
82     FORMAT (' ID: ',15I5)
       WRITE (19,*) (RD(I),I=1,NR)
83     FORMAT (' RD: ',15F5.2)
       WRITE (19,*) (CD(I),I=1,NC)
84     FORMAT (' CD: ',75A1)
       CLOSE(9)
       STOP
       END
c******************************************************************************

c******************************************************************************
c
       SUBROUTINE ACTION(K)
c
c      Test action routine
c------------------------------------------------------------------------------
       CALL MESSZZ('@',' Special action @@')
       RETURN
       END
c******************************************************************************
c










