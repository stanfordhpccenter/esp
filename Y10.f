c******************************************************************************
c
c      MATLAB routines
c
c******************************************************************************
c
       SUBROUTINE SVMPXX(RW,VMIN,VMAX)
c
c    Finds minimum and maximum manifold velocity for Matlab plot scales
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoV   ,MPTR(45))
c------------------------------------------------------------------------------
       VMAX = 0
       VMIN = 1E20
       DO 9 I=1,720
           DO 7 N=1,4
               V = RW(IoV+I+720*N)
               IF (V.GT.VMAX) VMAX = V
               IF (V.LT.VMIN) VMIN = V
7              CONTINUE
9          CONTINUE

       I = 0
10     V = I*10
       IF (V.LT.VMAX) THEN
           I = I + 1
           GOTO 10
           ENDIF
       VMAX = V
       I = 0
20     V = I*10
       IF (V.GT.VMIN) THEN
           I = I - 1
           GOTO 20
           ENDIF
       VMIN = V
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SPMPXX(RW,PMIN,PMAX)
c
c    Finds minimum and maximum manifold pressure for Matlap plot scales
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoP   ,MPTR(47))
c------------------------------------------------------------------------------
       PMAX = 0
       PMIN = 1E20
       DO 9 I=1,720
           DO 7 N=1,4
               P = RW(IoP+I+720*N)
               IF (P.GT.PMAX) PMAX = P
               IF (P.LT.PMIN) PMIN = P
7              CONTINUE
9          CONTINUE
       I = 11
10     P = I*1.E4
       IF (P.LT.PMAX) THEN
           I = I + 1
           GOTO 10
           ENDIF
       PMAX = P
       I = 10
20     P = I*1E4
       IF (P.GT.PMIN) THEN
           I = I - 1
           GOTO 20
           ENDIF
       PMIN = P
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE PLOTXX(RW,IW,K,N,MODEL,IRPM,IUML,SMIN,SMAX)
c
c      Loads MATLAB .M file open on unit IUML with plot data from RW for a
c      manifold variable denoted by K at point N. K=1 for V, K=2 for P.
c      MODEL is a 48-character model descriptor that heads the plot.
c      SMAX and SMIN are the maximum and minimum scale values.
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 2904)
c------------------------------------------------------------------------------
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoNCYE,MPTR(86))
       EQUIVALENCE (IoNCYL,MPTR(88))
       EQUIVALENCE (IoMAN ,MPTR(90))
c-----------------------------------------------------------------------------
       CHARACTER*48 MODEL,MODELX
       CHARACTER*1  C(2)
       CHARACTER*4  UNITS(2)
       CHARACTER*8  MAN(2)
       DATA    C /'V','P'/
       DATA    UNITS /'m/s ','MPa '/
       DATA    MAN/'intake ','exhaust'/
c-----------------------------------------------------------------------------
       CALL APOSVV(48,MODEL,MODELX)
c    identify the manifold
       M = IW(IoMAN)
c    scale factor
       IF (K.EQ.1)  THEN
               S = 1
           ELSE
               S=1E-6
           ENDIF
c    set data pointer
       IF (K.EQ.1)  THEN
               IX = IoV + 720*N
           ELSEIF (K.EQ.2) THEN
               IX = IoP + 720*N
           ELSE
               RETURN
           ENDIF

c   write first MATLAB data line
       WRITE (IUML,2) 0,RW(IX+1)*S
2      FORMAT (1X,'A=[',I4,1PE11.3,' ;')
c    middle MATLAB data lines
       WRITE (IUML,4) (I-1,RW(IX+I)*S,I=6,720,5)
4      FORMAT (4x,i4,1PE11.3,' ;')
c   last MATLAB data line
       WRITE (IUML,6) 720,RW(IX+720)*S
6      FORMAT (4x,I4,1PE11.3,' ; ]; ')
c   instructions
      WRITE (IUML,8) C(K),N,UNITS(K),SMIN,SMAX
8     FORMAT (' plot (A(:,1),A(:,2))'/
     ;        ' ylabel ('' ',A1,I1,1X,A4,' '') '/
     ;        ' axis ( [ 0  720 ',1PE12.2,E12.2,' ] )')
c
       IF ((K.EQ.1).AND.(N.EQ.1)) WRITE (IUML,12)
     ;             MODELX,MAN(M),IRPM,IW(IoNCYE)
12     FORMAT(' title ('' ',40X,A48,2X,A8,I8,
     ;        ' RPM   cycle',I3,' '') ')
c
       IF (N.EQ.4) WRITE (IUML,14)
14     FORMAT (' xlabel (''crank angle degrees from TDC'') ')
       RETURN
       END
c*****************************************************************************
c
       SUBROUTINE MANPXX(RW,IW,IUML)
c
c      Writes the manifold Matlap plots to unit IUML
c-----------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 2904)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
c    ESP cylinder work arrays:
       DIMENSION IWC(50),RWC(400)
       COMMON /IWVV/IWC
       COMMON /RWVV/RWC
c------------------------------------------------------------------------------
       EQUIVALENCE (IRPM,IWC(10))
c------------------------------------------------------------------------------
c    run setup
       CHARACTER*48    NMLD
       COMMON /NRUNVV/ NMLD
c------------------------------------------------------------------------------
       WRITE (IUML,1)
1      FORMAT (' clear'/
     ;         ' scrsz = get(0,''ScreenSize'');'/
     ;  ' figure (''Position'', ',
     ;  '[scrsz(3)*0.25 1 scrsz(3)*0.6 scrsz(4)*0.9])')
c
c    velocity plots
       CALL SVMPXX(RW,VMIN,VMAX)
       DO N=1,4
           WRITE (IUML,2) 1+2*(N-1)
2          FORMAT (' subplot(4,2,',I1,')')
           CALL PLOTXX(RW,IW,1,N,NMLD,IRPM,IUML,VMIN,VMAX)
           ENDDO
c
c    pressure plots
       CALL SPMPXX(RW,PMIN,PMAX)
       DO N=1,4
           WRITE (IUML,4) 2+2*(N-1)
4          FORMAT (' subplot(4,2,',I1,')')
           CALL PLOTXX(RW,IW,2,N,NMLD,IRPM,IUML,PMIN*1E-6,PMAX*1E-6)
           ENDDO
c      WRITE (IUML,8)
8      FORMAT (' orient tall')
       RETURN
       END
c******************************************************************************

