c******************************************************************************
c      Display routines
c
c******************************************************************************
c
       SUBROUTINE DRWVVV(IU,I1,I2)
c
c      Displays block of RW values I=I1,I2 on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CHARACTER*4     CODE
       CHARACTER*60    NAME
c------------------------------------------------------------------------------
c    Work array
       DIMENSION RW(400)
       COMMON /RWVV/ RW
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       DATA  RUNSET/ -1.1111111E11/
c------------------------------------------------------------------------------
       DO I=I1,I2
           CALL RWIDVV(I,CODE,NAME)
           IF (RW(I).EQ.RUNSET)  THEN
                   IF (IU.EQ.0)  THEN
                           WRITE (LINE,2) NAME
2                          FORMAT (3X,A,' >> ','none@@')
                           CALL DOUTZZ('@',LINE)
                       ELSE
                           WRITE (IU,3) NAME
3                          FORMAT(4X,A,' >> ','none')
                       ENDIF
               ELSE
                   IF (IU.EQ.0)  THEN
                           WRITE (LINE,4) NAME,RW(I)
4                          FORMAT (3X,A,' >> ',1PE10.3,'@@')
                           CALL DOUTZZ('@',LINE)
                       ELSE
                           WRITE (IU,5) NAME,RW(I)
5                          FORMAT (4X,A,' >> ',1PE10.3)
                       ENDIF
               ENDIF
           ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DIWVVV(IU,I1,I2)
c
c      Displays a block of IW values I=I1,I2 and names on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CHARACTER*4     CODE
       CHARACTER*60    NAME
c------------------------------------------------------------------------------
       DIMENSION IW(50)
       COMMON /IWVV/ IW
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       DATA IUNSET/ -22222/
c------------------------------------------------------------------------------
       DO I=I1,I2
           CALL IWIDVV(I,CODE,NAME)
           IF (IW(I).EQ.IUNSET)  THEN
                   IF (IU.EQ.0)  THEN
                           WRITE (LINE,2) NAME
2                          FORMAT (3X,A,' >> ','none@@')
                           CALL DOUTZZ('@',LINE)
                       ELSE
                           WRITE (IU,3) NAME
3                          FORMAT(4X,A,' >> ','none')
                       ENDIF
               ELSE
                   IF (IU.EQ.0)  THEN
                           WRITE (LINE,4) NAME,IW(I)
4                          FORMAT (3X,A,' >> ',6X,I4,'@@')
                           CALL DOUTZZ('@',LINE)
                       ELSE
                           WRITE (IU,5) NAME,IW(I)
5                          FORMAT (4X,A,' >> ',6X,I4)
                       ENDIF
               ENDIF
           ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DHDRVV(IU)
c
c      Displays run header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CHARACTER*8     RUN,VERS
c------------------------------------------------------------------------------
       COMMON /VERSVV/ VERS
       COMMON /RUNVV/ RUN
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) VERS,RUN
2              FORMAT ('@ESP   ',A,'  Run ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) VERS,RUN
3              FORMAT (/' ESP   ',A,'   Run ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DCCLVV(IU)
c
c      Displays cylinder convergence single-line summary on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       REAL    NVOL
c------------------------------------------------------------------------------
c    Work arrays
       DIMENSION IW(50)
       DIMENSION RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/ RW
       EQUIVALENCE (NCYC,IW(46))
       EQUIVALENCE (ERRT,RW(286))
       EQUIVALENCE (ERRP,RW(287))
       EQUIVALENCE (ERRV,RW(288))
       EQUIVALENCE (ERRM,RW(289))
       EQUIVALENCE (ERRE,RW(290))
       EQUIVALENCE (NVOL,RW(293))
c------------------------------------------------------------------------------
       CHARACTER*90 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NCYC,NVOL,
     ;         ABS(ERRT),ABS(ERRP),ABS(ERRV),ABS(ERRM),ABS(ERRE)
2              FORMAT ('@Cycle',I4,' Nvol=',F5.3,' errors: t=',
     ;         1PE7.1,' p=',E7.1,' v=',E7.1,' m=',E7.1,' e='E7.1,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NCYC,NVOL,
     ;         ABS(ERRT),ABS(ERRP),ABS(ERRV),ABS(ERRM),ABS(ERRE)
3              FORMAT (/' Cycle',I4,' Nvol=',F5.3,' errors: t=',
     ;         1PE7.1,' p=',E7.1,' v=',E7.1,' m=',E7.1,' e='E7.1)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DCCSVV(IU)
c
c      Displays cylinder convergence summary on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------

       DIMENSION IW(50)
       COMMON /IWVV/IW
       EQUIVALENCE (NCYC,IW(46))
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NCYC
2              FORMAT('@Convergence error summary for cycle ',I3,
     ;                 ':@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NCYC
3              FORMAT(/' Convergence error summary for cycle ',I3,
     ;                 ':')
           ENDIF
       CALL DRWVVV(IU,286,290)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  DMCSVV(IU)
c
c      Displays manifold convergence summary on unit IU.
c      If IU=0, calls DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
c    ESP work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
c------------------------------------------------------------------------------
c    ESP work array equivalences
       REAL        MIN,MEX
       EQUIVALENCE (MIN ,RW(11))
       EQUIVALENCE (MEX ,RW(16))
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
c------------------------------------------------------------------------------
c    manifold work arrays
       PARAMETER   (NMPTR = 96)
       PARAMETER   (NDRW = 21024)
       DIMENSION   MPTR(NMPTR)
       DIMENSION   RWI(NDRW)
       DIMENSION   RWE(NDRW)
       COMMON /MPTRXX/ MPTR
       COMMON /RWIXX/  RWI
       COMMON /RWEXX/  RWE
       EQUIVALENCE (IoF   ,MPTR(61))
c------------------------------------------------------------------------------
       CHARACTER*90  LINE
c------------------------------------------------------------------------------
       IF (KIMC.EQ.2)  THEN
c        intake manifold
           IF (IU.EQ.0)  THEN
                   WRITE (LINE,2) (RWI(IoF+L),L=1,4),MIN
2                  FORMAT ('Intake  F1-F4,Fin (kg) ',5(1pe11.3),
     ;                     '@@')
                   CALL DOUTZZ('@',LINE)
               ELSE
                   WRITE (IU,3) (RWI(IoF+L),L=1,4),MIN
3                  FORMAT (' Intake  F1-F4,Fin (kg) ',5(1pe11.3))
               ENDIF
           ENDIF
c
       IF (KEMC.EQ.2)  THEN
c        exhaust manifold
           IF (IU.EQ.0)  THEN
                   WRITE (LINE,4) MEX,(RWE(IoF+L),L=1,4)
4                  FORMAT ('Exhaust Fex,F1=F4 (kg) ',5(1pe11.3),
     ;                     '@@')
                   CALL DOUTZZ('@',LINE)
               ELSE
                   WRITE (IU,5) MEX,(RWE(IoF+L),L=1,4)
5                  FORMAT (' Exhaust Fex,F1=F4 (kg) ',5(1pe11.3))
               ENDIF
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DSUMVV(IU)
c
c      Displays performance summary output on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               CALL DOUTZZ('@','@Cycle performance summary:@@')
           ELSE
               WRITE (IU,4)
4              FORMAT (/' Cycle performance summary:')
           ENDIF
       CALL DRWVVV(IU,291,297)
       CALL DIWVVV(IU,47,49)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DOPHVV(IU)
c
c      Displays operating conditions header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
c    operating condition name
       CHARACTER*48    NEOP
       COMMON /NEOPVV/ NEOP
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NEOP
2              FORMAT ('@Operating conditions: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NEOP
3              FORMAT (/' Operating conditions: ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DOPCVV(IU)
c
c      Displays operating conditons on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
c    Work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       CALL DOPHVV(IU)
       CALL DIWVVV(IU,2,5)
       CALL DIWVVV(IU,10,16)
       CALL DRWVVV(IU,202,203)
       CALL DRWVVV(IU,206,208)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DEGHVV(IU)
c
c      Displays engine geometry header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
c    Engine geometry name
       CHARACTER*48    NEGM
       COMMON /NEGMVV/ NEGM
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NEGM
2              FORMAT ('@Engine geometry: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NEGM
3              FORMAT (/' Engine geometry: ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DEGMVV(IU)
c
c      Displays engine geometry model on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
c    Work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
       EQUIVALENCE (KPPR,IW(20))
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       CALL DEGHVV(IU)
       CALL DRWVVV(IU,211,213)
       CALL DRWVVV(IU,216,216)
       IF (KPPR.EQ.2) CALL DRWVVV(IU,217,217)
       CALL DRWVVV(IU,206,208)
       CALL DRWVVV(IU,218,219)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DMOHVV(IU)
c
c      Displays the model header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
c    model parameters
       CHARACTER*48    NEMP
       COMMON /NEMPVV/ NEMP
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NEMP
2              FORMAT('@Engine model: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NEMP
3              FORMAT (/' Engine model: ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DMEHVV(IU)
c
c      Displays the model element headers on unit IU (abbreviated model).
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
c    header
       CALL DMOHVV(IU)
c    engine geometry
       CALL DEGHVV(IU)
c    gas properties header
       CALL DGPHVV(IU)
c    flow
       CALL DFLHVV(IU)
c    heat transfer
       CALL DHTHVV(IU)
c    turbulence
       CALL DTBHVV(IU)
c    flame geometry
       CALL DFGHVV(IU)
c    flame propagation
       CALL DFPHVV(IU)
c    inlet manifold
       CALL DIMHVV(IU)
c    exuaust manifold
       CALL DEMHVV(IU)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DMODVV(IU)
c
c      Displays the model on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CHARACTER*48    NFLM,NHTM,NTBM,NFPM,NIMM,NEMM
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
c    ESP work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
c------------------------------------------------------------------------------
c    valve program names
       CHARACTER*48    NIVP,NEVP
       COMMON /NIVPVV/NIVP
       COMMON /NEVPVV/NEVP
c------------------------------------------------------------------------------
c    user valve programs
       DIMENSION IVDI(48),FVDI(48)
       DIMENSION IVDE(48),FVDE(48)
       COMMON  /UVIDVV/ NDI,FMXI,IVDI,FVDI
       COMMON  /UVEDVV/ NDE,FMXE,IVDE,FVDE
c-----------------------------------------------------------------------------
c    header
       CALL DMOHVV(IU)
c    engine geometry
       CALL DEGMVV(IU)
c    gas properties header
       CALL DGPHVV(IU)
c    flow
       CALL DFLMVV(IU)
c    heat transfer
       CALL DHTMVV(IU)
c    turbulence
       CALL DTBMVV(IU)
c    flame geometry
       CALL DFGMVV(IU)
c    flame propagation
       CALL DFPMVV(IU)
c    inlet manifold
       CALL DIMMVV(IU)
c    exhaust manifold
       CALL DEMMVV(IU)
c    intake valve profile
       IF (IW(18).EQ.4)  CALL DVPTVV(IU,1,NIVP,NDI,IVDI,FVDI)
       IF (IW(19).EQ.4)  CALL DVPTVV(IU,2,NEVP,NDE,IVDE,FVDE)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DFLHVV(IU)
c
c      Displays the flow model header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CHARACTER*48    NFLM
       COMMON /NFLMVV /NFLM
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NFLM
2              FORMAT ('@Flow model: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NFLM
3              FORMAT (/' Flow model: ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DFLMVV(IU)
c
c      Displays the flow model on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CALL DFLHVV(IU)
       CALL DRWVVV(IU,221,224)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DHTHVV(IU)
c
c      Displays the heat transfer model header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CHARACTER*48    NHTM
       COMMON /NHTMVV /NHTM
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NFLM
2              FORMAT ('@Heat transfer model: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NFLM
3              FORMAT (/' Heat transfer model: ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DHTMVV(IU)
c
c      Displays the heat transfer model on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CALL DHTHVV(IU)
       CALL DRWVVV(IU,251,260)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DTBHVV(IU)
c
c      Displays the turbulence model header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CHARACTER*48    NTBM
       COMMON /NTBMVV /NTBM
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NTBM
2              FORMAT ('@Turbulence model: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NTBM
3              FORMAT (/' Turbulence model: ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DTBMVV(IU)
c
c      Displays the turbulence model on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CALL DTBHVV(IU)
       CALL DRWVVV(IU,271,280)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DFPHVV(IU)
c
c      Displays the flame propagation model header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CHARACTER*48    NFPM
       COMMON /NFPMVV /NFPM
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NFPM
2              FORMAT ('@Flame propagation model: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NFPM
3              FORMAT (/' Flame propagation model: ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DFPMVV(IU)
c
c      Displays the flame propagation model on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CALL DFPHVV(IU)
       CALL DRWVVV(IU,231,234)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DFGHVV(IU)
c
c      Displays the flame geometry model header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       CHARACTER*48    NFGM
       COMMON /NFGMVV /NFGM
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2) NFGM
2              FORMAT ('@Flame geometry model: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NFGM
3              FORMAT (/' Flame geometry model: ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DFGMVV(IU)
c
c      Displays the flame geometry model on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
c    Work arrays
       DIMENSION IW(50)
       COMMON  /IWVV/  IW
       EQUIVALENCE (KFGM,IW(38))
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       CALL DFGHVV(IU)
       IF (KFGM.EQ.3)  THEN
               IF (IU.EQ.0)  THEN
                       WRITE (LINE,2)
2                      FORMAT('@','   Centered cylindrical burn@@')
                       CALL DOUTZZ('@',LINE)
                   ELSE
                       WRITE (IU,3)
3                      FORMAT('    Centered cylindrical burn')
                   ENDIF
           ELSE
c            flame geometry table
               CALL DFGTVV(IU)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DFGTVV(IU)
c
c      Displays the flame geometry table on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       DIMENSION   FAHB(11),RFAP(11)
c------------------------------------------------------------------------------
c    Flame geometry table
       COMMON /FGMTVV/FAHB,RFAP
c------------------------------------------------------------------------------
       CHARACTER*48    NFGT
       COMMON /NFGTVV/NFGT
c------------------------------------------------------------------------------
       CHARACTER*500 STRING
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
         WRITE(STRING,2) NFGT
  2      FORMAT ('@Flame geometry table: ',A,'@'/
     ; '@-----------------------------------------------------------@ '/
     ; '@    fraction of      fraction of heat    ratio of projected@ '/
     ; '@ volume containing   transfer area in    flame area at TDC@ '/
     ; '@     burned gas      burned zone at TDC   to bore area@ '/
     ; '@ ----------------------------------------------------------@@')
         CALL DOUTZZ('@',STRING)
c
        ELSE
c
         WRITE(IU,3) NFGT
3        FORMAT (/' Flame geometry table: ',A/
     ; ' -------------------------------------------------------------'/
     ; '     fraction of       fraction of heat     ratio of projected'/
     ; '  volume containing    transfer area in     flame area at TDC'/
     ; '     burned gas       burned zone at TDC      to bore area'/
     ; ' -------------------------------------------------------------')
         ENDIF
c
       DO I=1,11
           X = 0.1*(I - 1)
           IF (IU.EQ.0)  THEN
                   WRITE (STRING,4) X,FAHB(I),RFAP(I)
4                  FORMAT ('@',F11.1,F23.3,F20.3,'@@')
                   CALL DOUTZZ('@',STRING)
               ELSE
                   WRITE (IU,5) X,FAHB(I),RFAP(I)
5                  FORMAT (F12.1,F23.3,F20.3)
               ENDIF
           ENDDO

       IF (IU.EQ.0) THEN
            CALL DOUTZZ('@',
     ; '------------------------------------------------------------@@')
         ELSE
            WRITE (IU,12)
12          FORMAT(
     ; ' ------------------------------------------------------------')
         ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DIMHVV(IU)
c
c      Displays the intake manifold model header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       DIMENSION IW(50)
       COMMON /IWVV/IW
       EQUIVALENCE (KIMC,IW(39))
c------------------------------------------------------------------------------
       CHARACTER*48   NIMM
       COMMON /NIMMVV /NIMM
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (KIMC.EQ.1) THEN
               IF (IU.EQ.0)  THEN
                       WRITE (LINE,2)
2                      FORMAT ('@','@No intake manifold@@')
                       CALL DOUTZZ('@',LINE)
                   ELSE
                       WRITE (IU,3)
3                      FORMAT (/' No intake manifold')
                   ENDIF
           ELSE
               IF (IU.EQ.0)  THEN
                       WRITE (LINE,4) NIMM
4                      FORMAT ('@','@Intake manifold model: ',A,'@@')
                       CALL DOUTZZ('@',LINE)
                   ELSE
                       WRITE (IU,5) NIMM
5                      FORMAT (/' Intake manifold model: ',A)
                   ENDIF
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DIMMVV(IU)
c
c      Displays the intake manifold model on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       DIMENSION IW(50)
       COMMON /IWVV/IW
       EQUIVALENCE (KIMC,IW(39))
c------------------------------------------------------------------------------
       CALL DIMHVV(IU)
       IF (KIMC.EQ.2) THEN
           CALL DRWVVV(IU,301,309)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DEMHVV(IU)
c
c      Displays the exhaust manifold model header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       DIMENSION IW(50)
       COMMON /IWVV/IW
       EQUIVALENCE (KEMC,IW(40))
c------------------------------------------------------------------------------
       CHARACTER*48   NEMM
       COMMON /NEMMVV /NEMM
c------------------------------------------------------------------------------
       CHARACTER*80 LINE
c------------------------------------------------------------------------------
       IF (KEMC.EQ.1) THEN
               IF (IU.EQ.0)  THEN
                       WRITE (LINE,2)
2                      FORMAT ('@','@No exhaust manifold@@')
                       CALL DOUTZZ('@',LINE)
                   ELSE
                       WRITE (IU,3)
3                      FORMAT (/' No exhaust manifold')
                   ENDIF
           ELSE
               IF (IU.EQ.0)  THEN
                       WRITE (LINE,4) NEMM
4                      FORMAT ('@','@Exhaust manifold model: ',A,'@@')
                       CALL DOUTZZ('@',LINE)
                   ELSE
                       WRITE (IU,5) NEMM
5                      FORMAT (/' Exhaust manifold model: ',A)
                   ENDIF
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE DEMMVV(IU)
c
c      Displays the exhaust manifold model on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
       DIMENSION IW(50)
       COMMON /IWVV/IW
       EQUIVALENCE (KEMC,IW(40))
c------------------------------------------------------------------------------
       CALL DEMHVV(IU)
       IF (KEMC.EQ.2)  THEN
           CALL DRWVVV(IU,311,319)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  DGPHVV(IU)
c
c      Displays the gas properties header on unit IU.
c      If IU=0, CALLS DOUTZZ to send data to the output window.
c------------------------------------------------------------------------------
c    Properties file data
       CHARACTER*48    NGPM
       COMMON  /NGPMVV/NGPM
c------------------------------------------------------------------------------
       CHARACTER*80  LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2)  NGPM
2              FORMAT ('@Gas properties model: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NGPM
3              FORMAT (/' Gas properties model: ',A)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  DGPDVV(IU)
c
c      Displays the gas property data on unit IU.
c      No action for IU=0.
c------------------------------------------------------------------------------
       CHARACTER*8     CHEM
       CHARACTER*48    NGPM
c------------------------------------------------------------------------------
c    Array dimension
       PARAMETER (IMAX = 48)
       PARAMETER (NSMAX = 50)
c------------------------------------------------------------------------------
       DIMENSION   CHEM(NSMAX),RMOLS(NSMAX)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
c------------------------------------------------------------------------------
c    Properties file data
       COMMON  /NGPMVV/ NGPM
       COMMON  /PROCVV/ CHEM
       COMMON  /PRONVV/ NF,NO,NP
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
       COMMON  /PROPVV/ PP,PR,RFT,RMOLS
c------------------------------------------------------------------------------
c    no write for IU=0
       IF (IU.EQ.0)  RETURN
c    header
       WRITE (IU,2) NGPM,PR
2      FORMAT (/' Gas properties model: ',A//
     ;  '   Reactants;  properties evaluated at',F6.2,' atm.'/
     ;  '      Fuel         relative mols')
c    fuels
       J1 = 1
       J2 = NF
       WRITE (IU,4) (CHEM(J),RMOLS(J),J=J1,J2)
4      FORMAT(8X,A8,1PE14.3)
c    oxidizer
       IF (NO.GT.0)  THEN
           WRITE (IU,6)
6          FORMAT ('    Oxidizer')
           J1 = J2 + 1
           J2 = J2 + NO
           WRITE (IU,4) (CHEM(J),RMOLS(J),J=J1,J2)
           ENDIF
c    products
       WRITE (IU,8) PP
8      FORMAT ('   Products;   properties evaluated at',F6.2,' atm.')
       J1 = J2 + 1
       J2 = J2 + NP
       WRITE (IU,10) (CHEM(J),J=J1,J2)
10     FORMAT (8X,A8)
       WRITE (IU,12) RFT
12     FORMAT ('   Fuel mass/total mass = ',E13.5)
c    tables
       WRITE (IU,14)
14     FORMAT (/'   T, K  reactant properties     product properties'/
     ;        '          h, J/kg     Pv, J/kg    h, J/kg     Pv, J/kg')
       L = NF + NO + NP + 10
       DO I=1,48
           IT = 100*(I + 1)
           WRITE (IU,16) IT,HR(I),PVR(I),HP(I),PVP(I)
16         FORMAT (I7,4(1PE12.4))
           ENDDO
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

