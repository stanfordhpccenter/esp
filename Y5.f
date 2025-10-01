c******************************************************************************
c
c      Manifold junction support
c
c******************************************************************************
c
       SUBROUTINE JERRXX(RW,IW,NRK2,PJ,YJ,
     ;                   CD,PD,VD,YD,EMB,EEB,FTMAX,ETMAX,IERR)
c
c      Calculates junction mass and energy balance errors for given PJ,YJ.
c-----------------------------------------------------------------------------
c      Duct 1 state 2 is the nominal input, duct 2 state 3 the nominal output.
c
c      Flows to/from other runners are computed using the current input trial
c      junction state (PJ,YJ) and Y,Z values in the runners at the junction
c      taken from the computed runner at the appropriate phase.
c
c      Input:
c        RW    real work array for the manifold
c        IW    integer work array
c        PJ    junction pressure
c        YJ    junction entropy parameter P/rho^k
c
c      Output:
c        CD(J) sound speed for duct J=1,2
c        PD(J) pressure for duct J=1,2
c        VD(J) velocity for duct J=1,2
c        YD(J) entropy parameter for duct J=1,2
c        ERRMB mass balance error
c        ERREB energy balance error
c        FTMAX maximum term in the mass flow balance
c        ETMAX maximum term in the energy balance
c        IERR  0 if ok, otherwise >0.
c-----------------------------------------------------------------------------
c      Data in IW at call:
c        I         angle index where Y,Z knowm
c
c      Data in RW at call:
c        Y(I,2)    entropy parameter P/rho^k at the nominal inlet
c        Y(I,3)    entropy parameter P/rho^k at the nominal outlet
c        Z(I,2)    acoustic parameter Z+ = 2c/(k-1) + V at the nominal inlet
c        Z(I,3)    acoustic parameter Z- = 2c/(k-1) - V at the nominal outlet
c        AD(1)     flow area of duct 1
c        AD(2)     flow area of duct 2
c        AV(2)     restricted area at state 2
c        AV(3)     restricted area at state 3
c-----------------------------------------------------------------------------
       DIMENSION   CD(2),PD(2),VD(2),YD(2)
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
       EQUIVALENCE (IoKM1K,MPTR(9))
       EQUIVALENCE (IoRKM1,MPTR(13))
       EQUIVALENCE (IoAMR ,MPTR(40))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoY   ,MPTR(46))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoZ   ,MPTR(49))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoYPAV,MPTR(77))
       EQUIVALENCE (IoZPAV,MPTR(78))
       EQUIVALENCE (IoJTAU,MPTR(84))
       EQUIVALENCE (IoNCYL,MPTR(88))
       EQUIVALENCE (IoMAN ,MPTR(90))
       EQUIVALENCE (IoI   ,MPTR(91))
       EQUIVALENCE (IoDZCO,MPTR(94))
c-----------------------------------------------------------------------------
c    diagnostic monitor
       LOGICAL OUT,PINR,PINF
       COMMON /MONMXX/IUMONM,MONM,OUT
c------------------------------------------------------------------------------
c    identify manifold and angle
       M = IW(IoMAN)
       I = IW(IoI)
c
       IF (OUT) WRITE (IUMONM,2) M,I,PJ,YJ
2      FORMAT(' JERRXX: M,I,PJ,JY:',I2,I4,2(1PE14.6))
c
c    set parameters for the manifold
       IF (M.EQ.1)  THEN
c            intake
               JR = 2
               JF = 1
               NF = 2
               NR = 3
               KS = 1
               PINR = .FALSE.
               PINF = .TRUE.
           ELSE
c            exhaust
               JR = 1
               JF = 2
               NR = 2
               NF = 3
               KS = - 1
               PINR = .TRUE.
               PINF = .FALSE.
           ENDIF
c    coefficients are unity (blockage handles losses)
       CFI = 1
       CFO = 1
       FEGR = 0
       TREGR = 1
c    initialize balances
       EMB = 0
       EEB = 0
       FTMAX = 0
       ETMAX = 0
c    calculate runners using incident Y and Z values
       IR1X = I + 720*NR
       DO 19 N=1,IW(IoNCYL)
           IF (N.EQ.1) THEN
c                current values in the computed runner (at I-1/2 or I)
                   ZR = RW(IoZ+IR1X)
                   YR = RW(IoY+IR1X)
               ELSE
c                phase-averaged values for other runners; Z corrected
                   IR = IJPXX(I,IW(IoJTAU)*(N-1))
                   IF (NRK2.EQ.1)  THEN
c                        get phase averages at I-1/2
                           IRM = IM1XX(IR)
                           YR = EXP( 0.5*(LOG(RW(IoYPAV+IRM))
     ;                                  + LOG(RW(IoYPAV+IR))) )
                           ZR = 0.5*( RW(IoZPAV+IRM) + RW(IoZPAV+IR) )
                       ELSE
c                        phase averages at I
                           YR = RW(IoYPAV+I)
                           ZR = RW(IoZPAV+I)
                       ENDIF
c                correction for mass balance
                   ZR = ZR + KS*RW(IoDZCO)
               ENDIF
           CALL FDRPXX(RW,PINR,YR,ZR,PJ,YJ,
     ;        RW(IoAD+JR),RW(IoAMR+NR),CFI,CFO,FEGR,TREGR,
     ;        CDX,PDX,VDX,YDX,VVA,FR,IERR)
           IF (IERR.NE.0)  RETURN
c        energy flow rate
           ER = FR*(RW(IoRKM1)*CDX*CDX + 0.5*VDX*VDX)
c        reselect largest terms
           FTMAX = MAX(ABS(FR),FTMAX)
           ETMAX = MAX(ABS(ER),ETMAX)
c        accumulate balance errors
           EMB = EMB + KS*FR
           EEB = EEB + KS*ER
c        load the current runner state
           IF (N.EQ.1)  THEN
               CD(JR) = CDX
               PD(JR) = PDX
               VD(JR) = VDX
               YD(JR) = YDX
               ENDIF
19         CONTINUE
c
c    calculate feeder/collector
       IFX = I + 720*NF
       CALL FDRPXX(RW,PINF,RW(IoY+IFX),RW(IoZ+IFX),PJ,YJ,
     ;   RW(IoAD+JF),RW(IoAMR+NF),CFI,CFO,FEGR,TREGR,
     ;   CD(JF),PD(JF),VD(JF),YD(JF),VVA,FF,IERR)
       IF (IERR.NE.0)  RETURN
c    energy flow rate
       EF =  FF*(RW(IoRKM1)*CD(JF)*CD(JF) + 0.5*VD(JF)*VD(JF))
c    reselect largest terms
       FTMAX = MAX(ABS(FF),FTMAX)
       ETMAX = MAX(ABS(EF),ETMAX)
c    accumulate balance errors
       EMB = EMB - KS*FF
       EEB = EEB - KS*EF
c    normal return
       IERR = 0
       RETURN
       END
c*****************************************************************************

