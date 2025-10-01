c******************************************************************************
c
c      Cylinder data Matlab routines
c
c******************************************************************************
c
       SUBROUTINE PINDVV(IU)
c
c      Writes a Matlab indicator diagram file to unit IU
c------------------------------------------------------------------------------
c    phase-averaged variables and weightings
       DIMENSION   PCAV(720),RHOCAV(720)
       COMMON      /AVCDVV/PCAV,RHOCAV,FCAVO,FCAVN,NCAV
c------------------------------------------------------------------------------
c    volume for indicator diagram
       DIMENSION   VCYL(720)
       COMMON      /VCYLVV/VCYL
c------------------------------------------------------------------------------
c    indicator scales
       COMMON   /PVIMVV/PIMAX,VIMAX,NOPIS
c------------------------------------------------------------------------------
c    ESP cylinder work arrays:
       DIMENSION IWC(50),RWC(400)
       COMMON /IWVV/IWC
       COMMON /RWVV/RWC
       EQUIVALENCE (IRPM,IWC(10))
c------------------------------------------------------------------------------
c    run setup
       CHARACTER*48    NRUN,NRUNX
       COMMON /NRUNVV/ NRUN
c------------------------------------------------------------------------------
       WRITE (IU,1)
1      FORMAT (' clear'/' figure'/' subplot(1,1,1)')
       WRITE (IU,4) VCYL(4)*1E6,PCAV(1)*1E-6
c
       CALL APOSVV(48,NRUN,NRUNX)
4      FORMAT (' A = [ ',2(1PE12.4),'    ;')
       WRITE (IU,5) (VCYL(I)*1E6,PCAV(I)*1E-6,I=2,720)
5      FORMAT ('       ',2(1PE12.4),'    ;')
       WRITE (IU,6) VCYL(1)*1E6,PCAV(1)*1E-6,NRUNX,IRPM
6      FORMAT ('       ',2(1PE12.4),'  ] ;'/
     ;   'plot (A(:,1),A(:,2))'/
     ;   'xlabel (''Volume, cm^3'')'/
     ;   'ylabel ('' P, MPa'')'/
     ;   'title  ('' ',A,I8,' RPM '')' )
       IF (NOPIS.EQ.2)  THEN
               WRITE (IU,12) VIMAX,PIMAX
12             FORMAT(' axis ( [ 0 ',1PE12.2,' 0 ',E12.2,' ] )')
           ELSE
               WRITE (IU,13)
13             FORMAT (' axis auto')
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE MPL5VV (IUS,IU,ID)
c
c      Generates Matlab 5-line plot file from scratch file
c------------------------------------------------------------------------------
c    Nomenclature
c      IUS     open unformatted scratch file unit containing tabulated data
c      IU      open formatted Matlab file unit
c      ID(K)   index of the Kth line in the real work array (K=1,5)
c------------------------------------------------------------------------------
       DIMENSION ID(5),Y(5)
c------------------------------------------------------------------------------
c    quantity code and name
       CHARACTER*4     CODE
       CHARACTER*60    NAME,NAMEX
c-----------------------------------------------------------------------------
c    run designator
       CHARACTER*48    NRUN,NRUNX
       COMMON /NRUNVV/ NRUN
c------------------------------------------------------------------------------
c    color code for the five lines
       CHARACTER*1 C(5)
       DATA C/'r','g','b','c','k'/
c------------------------------------------------------------------------------
c    initializations
       YMAX = 0
       YMIN = 0
       KPASS = 0
c    rest scratch file
       REWIND(IUS)
       CALL APOSVV(48,NRUN,NRUNX)
c
c    read and process the scratch file
10     READ (IUS,END=18,ERR=90) IA,(Y(I),I=1,5)
       IF (IA.EQ.0)  THEN
           IF (KPASS.EQ.0) KPASS = 1
           ENDIF
       IF (KPASS.EQ.0)  GOTO 10
c    processing from the scratch file
       IF (IA.EQ.0)  THEN
c            at TDC
               IF (KPASS.EQ.1) THEN
c                    write the first matlab line
                       WRITE (IU,11)
11                     FORMAT (' clear'/' figure'/' subplot(1,1,1)')
                       WRITE (IU,12,ERR=90) 0,(Y(I),I=1,5)
12                     FORMAT (' A = [ ',I5,5(1PE14.6), ' ; ')
                   ELSEIF (KPASS.EQ.2)  THEN
c                    write the last matlab line
                       WRITE (IU,14,ERR=90) 720,(Y(I),I=1,5)
14                     FORMAT (7X,I5,5(1PE14.6), '] ; ')
                       GOTO 20
                   ENDIF
           ELSE
c            write intermediate Matlab lines
               WRITE (IU,16,ERR=90) IA,(Y(I),I=1,5)
16             FORMAT (7X,I5,5(1PE14.6), ' ; ')
           ENDIF
c    update YMAX and YMIN
       DO I=1,5
           YMAX = MAX(YMAX,Y(I))
           YMIN = MIN(YMIN,Y(I))
           ENDDO
       GOTO 10
c    end of scratch file
18     REWIND (IUS)
       KPASS = 2
       GOTO 10
c
c    scratch file processed: empty and close scratch file
20     REWIND(IUS)
       CLOSE(IUS)
c
c    ledger height above YMAX
       H = 5*0.07*(YMAX - YMIN)
       YTOP = YMAX + H
c    plot a high point to obtain clear ledger zone
       WRITE (IU,22,ERR=90) YTOP
22     FORMAT('plot(0,',1PE12.4,',''k'' )' /
     ;        'hold on')
c    spacing of ledger lines
       S = H/6
c    build the legend commands
       DO I=1,5
c        set legend plot point
           YL = YTOP - I*S
c        get variable name
           CALL RWIDVV(ID(I),CODE,NAME)
c        write legend for variable
           CALL APOSVV(60,NAME,NAMEX)
           WRITE (IU,24,ERR=90) YL,YL,C(I),YL,NAMEX
24         FORMAT ('L= [20 ',1PE12.4' ;'/
     ;             '     80 ',E12.4, ']  ;'/
     ;             'plot (L(:,1),L(:,2),'' ',A,' '')'/
     ;             'text (100,',E12.4,','' ',A,' '')')
c        plot the line
           WRITE(IU,26,ERR=90) I+1,C(I)
26         FORMAT('plot(A(:,1),A(:,',I1,'),'' ',A,' '')')
           ENDDO
c
c    write x-axis label and run designator title
       WRITE (IU,28,ERR=90) NRUNX
28     FORMAT (
     ; 'xlabel ('' crank angle after TDC compression, degrees'' )'/
     ; 'title  ('' ',A,' '' )' /
     ; 'hold off')
c    close Matlab file
       CLOSE(IU)
       RETURN
c
c    error reading scratch file
90     CALL WARNZZ('@','@Error building Matlab file@@')
       REWIND(IUS)
       CLOSE(IUS)
       CLOSE(IU)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE MP5SVV(IERR)
c
c      Writes data for 5-line plots to open scratch files
c      Returns IERR=0 if ok, otherwise 1.
c------------------------------------------------------------------------------
c    ESP work arrays:
       DIMENSION IW(50),RW(400)
       COMMON /IWVV/IW
       COMMON /RWVV/RW
       EQUIVALENCE (IANG,IW(1))
c------------------------------------------------------------------------------
c    5-line plot codes
       DIMENSION IDPV(5,6)
       COMMON /IDPVVV/IDPV
c------------------------------------------------------------------------------
c    5-line plot options
       DIMENSION KOP5(6)
       COMMON/KOP5VV/KOP5,KOP5C
c------------------------------------------------------------------------------
c    check for no plots
       IF (KOP5C.EQ.0)  RETURN
c    write the plots
       DO I=1,6
           IF (KOP5(I).EQ.1) THEN
               WRITE (20+I,ERR=90) IANG,(RW(IDPV(K,I)),K=1,5)
               ENDIF
           ENDDO
       IERR = 0
       RETURN
c    error
90     IERR = 1
       CALL WARNZZ('@','@Error writing Matlab files@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  APOSVV(N,A,B)

c      Replaces apostrophies in string A by by ` in string B
c------------------------------------------------------------------------------
       CHARACTER*1 A(N),B(N)
c------------------------------------------------------------------------------
       DO I=1,N
           IF (A(I).EQ.'''')  THEN
                   B(I)='`'
               ELSE
                   B(I) = A(I)
               ENDIF
           ENDDO
       RETURN
       END
c******************************************************************************

