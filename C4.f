c******************************************************************************
c
c      Subroutines for gas exchange phase
c
c******************************************************************************
c
       SUBROUTINE  FMSCVV(IERR)
c
c      Calculates flow rates through valves and manifold states at valves
c      if a manifold is present.
c
c      Returns IERR = 0 if ok, otherwise a fatal error.
c-----------------------------------------------------------------------------
c      The following quantities must be loaded in RW at call:
c          AFLE    exhaust valve area open area, m**2
c          AFLI    intake valve area open area, m**2
c          CDE     discharge coefficient for exhaust valve
c          CDEB    discharge coefficient for exhaust valve backflow
c          CDI     discharge coefficient for intake valve
c          CDIB    discharge coefficient for intake valve backflow
c          FEGR    exhaust gas recirculation fraction
c          GAMB    Cp/Cv for backflow gas
c          GAMC    Cp/Cv for gas in cylinder (unburned during burn)
c          GAME    Cp/Cv for mixed exhaust gas
c          GAMM    Cp/Cv for mixed charge
c          MC      total mass in cylinder, kg
c          PC      pressure in cylinder, Pa
c          PE      exhaust ambient pressure, Pa
c          PI      intake ambient pressure, PA
c          RHOB    density of backflow gas gas, kg/m**3
c          RHOC    density of gas in cylinder (unburned during burn), kg/m**3
c          RHOE    density of mixed exhaust gas, kg/m**3
c          RHOM    density of mixed charge, kg/m**3
c
c      If manifolds are present, the Y and Z values at the duct point just
c      outside of the valve and EGR (4 for intake, 1 for exhaust) must be
c      loaded in their work arrays (RWI and RWE).
c-----------------------------------------------------------------------------
c      The following quantities must be loaded in IW at call:
c          LEFL    0 if exhaust valve closed, otherwise not
c          LIFL    0 if intake valve is closed, otherwise not
c-----------------------------------------------------------------------------
c      On return the following quantities are available in IW:
c          LEFL    0 if exhaust valve closed
c                  1 if exhaust outflow
c                  2 if exhaust backflow
c          LIFL    0 if intake valve is closed
c                  1 mixed charge inflow
c                  2 backflow gas inflow
c                  3 backflow
c-----------------------------------------------------------------------------
c    The following quantities are returned in RW:
c          FIN     mass inflow rate through intake valve, kg/s
c          FEX     mass outflow rate through exhaust valve, kg/s
c          P0EV    stagnation pressure downstream of exhaust valve, Pa
c          P0IV    stagnation pressure upstream of intake valve, Pa
c          T0EV    stagnation temperature downstream of exhaust valve, K
c          T0IV    stagnation temperature upstream of intake valve, K
c          VELI    velocity magnitude through intake valve, m/s
c          VELE    velocity magnitude through exhaust valve, m/s
c
c      If manifolds are present, the duct state (P,C,V,Y) outside of the
c      valve and EGR (point 4 for intake, 1 for exhaust) is also set.
c-----------------------------------------------------------------------------
c      Quantities used only in this subroutine include:
c          FLO     mass flow rate, kg/s
c          VEL     velocity, m/s
c-----------------------------------------------------------------------------
       REAL        KT,MC,MIB,MI4,ME1
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION   IW(50),RW(400)
       COMMON  /IWVV/  IW
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (ANG ,RW(101))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (VELT,RW(106))
       EQUIVALENCE (FIN ,RW(110))
       EQUIVALENCE (FEX ,RW(111))
       EQUIVALENCE (AFLE,RW(122))
       EQUIVALENCE (AFLI,RW(123))
       EQUIVALENCE (AHTC,RW(124))
       EQUIVALENCE (MIB ,RW(126))
       EQUIVALENCE (TE  ,RW(129))
       EQUIVALENCE (VELI,RW(145))
       EQUIVALENCE (VELE,RW(146))
       EQUIVALENCE (P0IV,RW(147))
       EQUIVALENCE (T0IV,RW(148))
       EQUIVALENCE (P0EV,RW(149))
       EQUIVALENCE (T0EV,RW(150))
       EQUIVALENCE (GAMM,RW(158))
       EQUIVALENCE (GAMB,RW(159))
       EQUIVALENCE (GAMC,RW(157))
       EQUIVALENCE (GAME,RW(160))
       EQUIVALENCE (RHOC,RW(169))
       EQUIVALENCE (RHOM,RW(170))
       EQUIVALENCE (RHOB,RW(171))
       EQUIVALENCE (RHOE,RW(172))
       EQUIVALENCE (DT1 ,RW(199))
       EQUIVALENCE (PE  ,RW(204))
       EQUIVALENCE (PI  ,RW(205))
       EQUIVALENCE (TI  ,RW(206))
       EQUIVALENCE (TEGR,RW(207))
       EQUIVALENCE (FEGR,RW(208))
       EQUIVALENCE (TM  ,RW(209))
       EQUIVALENCE (CDI ,RW(221))
       EQUIVALENCE (CDIB,RW(222))
       EQUIVALENCE (CDE ,RW(223))
       EQUIVALENCE (CDEB,RW(224))
       EQUIVALENCE (TRRI,RW(329))
c-----------------------------------------------------------------------------
c    Variable locations in IW:
       EQUIVALENCE (IANG,IW(1))
       EQUIVALENCE (LEFL,IW(8))
       EQUIVALENCE (LIFL,IW(9))
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
c-----------------------------------------------------------------------------
c    manifold work arrays
       PARAMETER   (NMPTR = 96)
       PARAMETER   (NDRW = 21024)
       PARAMETER   (NDIW = 2904)
       DIMENSION   MPTR(NMPTR)
       DIMENSION   RWI(NDRW)
       DIMENSION   RWE(NDRW)
       DIMENSION   IWI(NDIW)
       DIMENSION   IWE(NDIW)
       COMMON /MPTRXX/MPTR
       COMMON /RWIXX/RWI
       COMMON /RWEXX/RWE
       COMMON /IWIXX/IWI
       COMMON /IWEXX/IWE
c------------------------------------------------------------------------------
c    pointers for the manifold work arrays
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoKM1 ,MPTR(2))
       EQUIVALENCE (IoKM12,MPTR(6))
       EQUIVALENCE (IoKM1K,MPTR(9))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoY   ,MPTR(46))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoW   ,MPTR(48))
       EQUIVALENCE (IoZ   ,MPTR(49))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoR   ,MPTR(80))
       EQUIVALENCE (IoNCYL,MPTR(88))
       EQUIVALENCE (IoI   ,MPTR(91))
c-----------------------------------------------------------------------------
c    cylinder diagnostic monitor
       COMMON /MONCVV/IUMONC,MONC
c------------------------------------------------------------------------------
c    manifold diagnostic monitor
       LOGICAL OUT
       COMMON /MONMXX/IUMONM,MONM,OUT
c-----------------------------------------------------------------------------


c -- intake flow
c
       IF (LIFL.NE.0)  THEN
c            infow: determine state outside valve
               IF (MIB.GT.0)  THEN
c                    backflow gas at intake
                       RHOI = RHOB
                       GAMI = GAMB
                       LIFL = 2
                   ELSE
c                    fresh charge at intake
                       RHOI = RHOM
                       GAMI = GAMM
                       LIFL = 1
                   ENDIF
c            branch on manifolding
               IF (KIMC.EQ.1) THEN
c                    no intake manifold
                       P0IV = PI
                       T0IV = TI
c                    branch on flow direction
                       IF (PI.GE.PC)  THEN
c                           flow in
                              CALL FLOWVV(AFLI,CDI,RHOI,GAMI,PI,PC,
     ;                                    VELI,FIN)
                           ELSE
c                           flow out
                              LIFL = 3
                              CALL FLOWVV(AFLI,CDIB,RHOC,GAMC,PC,PI,
     ;                                    VELI,FLO)
                              FIN = - FLO
                           ENDIF
                   ELSE
c                    manifold; set pointer to state 4
                       IX = IWI(IoI) + 720*4
c                    get cylinder entropy parameter
                       YC = PC/RHOC**RWI(IoK)
c                    get flow
                       CALL FDRPXX(RWI,.TRUE.,RWI(IoY+IX),RWI(IoZ+IX),
     ;                  PC,YC,RWI(IoAD+2),AFLI,CDI,CDIB,FEGR,TRRI,
     ;                  RWI(IoC+IX),RWI(IoP+IX),RWI(IoV+IX),RWI(IoY+IX),
     ;                  VELI,FIN,IERR)
                       IF (IERR.NE.0)  GOTO 90
c                    get inflow stagnation conditions
                       TI4 = RWI(IoC+IX)**2/(RWI(IoK)*RWI(IoR))
                       MI4 = RWI(IoV+IX)/RWI(IoC+IX)
                       TERM = 1 + RWI(IoKM12)*MI4*MI4
                       P0IV = RWI(IoP+IX)*TERM**RWI(IoKM1K)
                       IF (FIN.GT.0)  THEN
                               T0I4 = TI4*TERM
                               T0IV = (1 - FEGR)*T0I4 + FEGR*TEGR
c                            check for update of TRRI
                               IF (FEGR.GT.0)  TRRI = TEGR/T0I4
                           ELSE
c                            outflow
                               LIFL = 3
                               T0IV = TC
                           ENDIF
                   ENDIF
c            check for clipping
               TERM = 1
               IF ((P0IV.GE.PC).AND.(LIFL.NE.3))  THEN
c                    isothermal filling to equalize pressures
                       FINMAX = (P0IV/PC - 1)*MC/DT1
                       IF (FIN.GT.FINMAX)  TERM = FINMAX/FIN
                   ELSEIF ((P0IV.LE.PC).AND.(LIFL.EQ.3))  THEN
c                    isentropic blowdown to equalize pressures
                       FINMIN = - (1 - (P0IV/PC)**(1/GAMC))*MC/DT1
                       IF (FIN.LT.FINMIN)  TERM = FINMIN/FIN
                   ENDIF
               IF (TERM.NE.1) THEN
c                clip
                   FIN = TERM*FIN
                   VELI = VELI*TERM
                   IF (KIMC.EQ.2) RWI(IoV+IX) = RWI(IoV+IX)*TERM
                   ENDIF
           ELSE
c            intake valve closed
               FIN = 0
               VELI = 0
               IF (KIMC.EQ.2)  THEN
c                    get intake manifold state 4 with valve closed
                       IX = IWI(IoI) + 720*4
                       CALL CENDXX(RWI,RWI(IoY+IX),RWI(IoZ+IX),
     ;                  RWI(IoC+IX),RWI(IoP+IX),RWI(IoV+IX),RWI(IoY+IX))
                       P0IV = RWI(IoP+IX)
                       T0IV = (RWI(IoC+IX)**2)/(RWI(IoK)*RWI(IoR))
                   ELSE
                       P0IV = PI
                       T0IV = TI
                   ENDIF
           ENDIF
c
c -- exhaust flow
c
       IF (LEFL.NE.0.) THEN
c            exhaust valve open
               IF (KEMC.EQ.1) THEN
c                    no exhaust manifold
                       P0EV = PE
                       T0EV = TC
                       IF (PC.GE.PE)  THEN
c                            flow from cylinder
                               CALL FLOWVV(AFLE,CDE,RHOC,GAMC,PC,PE,
     ;                                       VELE,FEX)
                               LEFL = 1
                           ELSE
c                            backflow into cylinder
                               CALL FLOWVV(AFLE,CDEB,RHOE,GAME,PE,PC,
     ;                                       VELE,FLO)
                               FEX = - FLO
                               LEFL = 2
                           ENDIF
                   ELSE
c                    manifold; set pointer to state 1
                       IX = IWE(IoI)+720
c                    get cylinder entropy parameter
                       YC = PC/RHOC**RWE(IoK)
c                    set for EGR outflow
                       FEGRE = - FEGR
                       TRRIE = 1
c                    get flow
                       CALL FDRPXX(RWE,.FALSE.,RWE(IoY+IX),RWE(IoZ+IX),
     ;                  PC,YC,RWE(IoAD+1),AFLE,CDE,CDEB,FEGRE,TRRIE,
     ;                  RWE(IoC+IX),RWE(IoP+IX),RWE(IoV+IX),RWE(IoY+IX),
     ;                  VELE,FEX,IERR)
                       IF (IERR.NE.0)  GOTO 90
c                    calculate stagnation conditions at valve exit
                       ME1 = RWE(IoV+IX)/RWE(IoC+IX)
                       TERM = 1 + RWE(IoKM12)*ME1*ME1
                       P0EV = RWE(IoP+IX)*TERM**RWE(IoKM1K)
                       IF (FEX.GT.0)  THEN
c                            outflow
                               LEFL = 1
                               T0EV = TC
                           ELSE
c                            inflow
                               LEFL= 2
                               TE1 = RWE(IoC+IX)**2/(RWE(IoK)*RWE(IoR))
                               T0EV= TE1*TERM
                           ENDIF
                   ENDIF
c            check for clipping
               TERM = 1
               IF ((PC.GE.P0EV).AND.(LEFL.EQ.1))  THEN
c                    isentropic blowdown to equalize pressures
                       FEXMAX = (1- (P0EV/PC)**(1/GAMC))*MC/DT1
                       IF (FEX.GT.FEXMAX) TERM = FEXMAX/FEX
                   ELSEIF ((PC.LE.P0EV).AND.(LEFL.EQ.2))  THEN
c                    isothermal filling to equalize pressures
                       FEXMIN = - (P0EV/PC - 1)*MC/DT1
                       IF (FEX.LE.FEXMIN) TERM = FEXMIN/FEX
                   ENDIF
               IF (TERM.NE.1)  THEN
c                clip
                   FEX = FEX*TERM
                   VELE = VELE*TERM
                   IF (KEMC.EQ.2)  RWE(IoV+IX) = RWE(IoV+IX)*TERM
                   ENDIF
           ELSE
c            exhaust valve closed
               FEX = 0
               VELE = 0
               IF (KEMC.EQ.2)  THEN
c                    get exhaust manifold state 1 with valve closed
                       IX = IWE(IoI)+720
                       CALL CENDXX(RWE,RWE(IoY+IX),RWE(IoZ+IX),
     ;                  RWE(IoC+IX),RWE(IoP+IX),RWE(IoV+IX),RWE(IoY+IX))
                       P0EV = RWE(IoP+IX)
                       T0EV = (RWE(IoC+IX)**2)/(RWE(IoK)*RWI(IoR))
                   ELSE
                       P0EV = PE
                       T0EV = TE
                   ENDIF
           ENDIF
90     RETURN
       END
c******************************************************************************
c
       SUBROUTINE  DDTGVV(IERR)
c
c      Derivative calculations during gas exchange
c      IERR = 0 if ok, otherwise a fatal error
c-----------------------------------------------------------------------------
c      The following quantities must be loaded in RW at call:
c          AHTE    heat transfer area for exhaust valve flow, m**2
c          AHTI    heat transfer area for inlet valve flow, m**2
c          AHTC    heat transfer area in cylinder, m**2
c          APIS    bore are, m**2
c          DCKT    coefficient of KT in TKE equation, 1/s
c          CPC     specific heat Cp of gas in cylinder, J/kg-K
c          CPB     specific heat of backflow gas, J/kg-K
c          CPE     specific heat of mixed exhaust gas, J/kg-K
c          CPM     specific heat of mixed charge, J/kg-K
c          DVDT    dV/dt, m**3/s
c          FEGR    exhaust gas recirculation fraction
C          FEX     flow rate out the exhaust valve, kg/s
C          FIN     flow rate in the intake valve, kg/s
c          FKEB    exhaust backflow turb. kinetic energy/mean flow kin. energy
c          FKEI    inlet turbulent kinetic energy/mean flow kinetic energy
c          FRAB    fraction of reactants burned (combustion efficiency)
c          FTDG    factor in turbulence dissipation model during gas exchange
c          FTPG    factor in turbulence production model during gas exchange
c          GAMB    Cp/Cv for backflow gas
c          GAMC    Cp/Cv for gas in cylinder (unburned during burn)
c          GAME    Cp/Cv for mixed exhaust gas
c          GAMM    Cp/Cv for mixed charge
c          HB      total enthalpy of intake backflow gas, J
c          HE      total enthalpy of mixed exhausted gas this cycle, J
c          HBS     specific enthalpy of inlet backflow gas, J/kg
c          HCS     specific enthalpy of gas in cylinder, J/kg
c          HES     specific enthalpy of mixed exhaust exhaust, J/kg
c          HMS     specific enthalpy of mixed charge, J/kg
c          KT      kinetic energy of large-scale turbulence in cylinder, J/kg
c          MC      total mass in cylinder, kg
c          PC      pressure in cylinder, Pa
c          QC      heat transferred to coolant this cycle, J
c          RHOB    density of backflow gas gas, kg/m**3
c          RHOC    density of gas in cylinder (unburned during burn), kg/m**3
c          RHOE    density of mixed exhaust gas, kg/m**3
c          RHOM    density of mixed charge, kg/m**3
c          STEV    Stanton number for heat transfer from exhaust valve flow
c          STGE    Stanton number for in-cylinder heat trans. during gas exch.
c          STIV    Stanton number for heat transfer from inlet valve flow
c          TB      temperature of backflow gas, K
c          TC      temperature of cylinder gas, K
c          TE      mixed temperature of gas exhausted this cycle, K
c          TEGR    exhaust gas recirculation temperature, K
c          TI      inlet ambient temperature , K
c          TM      mixed inlet temperature , K
c          T0IV    stagnation temperature upstream of intake valve, K
c          TRRI    T0egr/T0runner at intake valve before EGR
c          TW      wall temperature (for convective heat transfer), K
c          UC      total internal energy of gas in cylinder, J
c          UCS     specific internal energy of gas in cylinder, J/kg
c          VC      cylinder volume, m**3
c          VELT    velocity of large-scale turbulence, m/s
c          VPIS    piston velcity, m/s
c          WP      total work done on piston this cycle, J
c          XPB     fraction of backflow gas that is products
c          XPC     fraction of cylinder gas that is products
c          XPE     fraction of exhaust gas that is products
c
c      If manifolds are present, the Y and Z values at the duct point just
c      outside of the valve and EGR (4 for intake, 1 for exhaust) must be
c      loaded in their work arrays (RWI and RWE).
c-----------------------------------------------------------------------------
c      The following quantities must be loaded in IW at call:
c          LEFL    0 if exhaust valve closed
c                  1 if exhaust outflow
c                  2 if exhaust backflow
c          LIFL    0 if intake valve is closed
c                  1 mixed charge inflow
c                  2 backflow gas inflow
c                  3 backflow
c-----------------------------------------------------------------------------
c    The following quantities are returned in RW:
c
c      Derivatives of variables advanced by differential equations:
c          DCKT    coefficient of KT in KT equation, 1/s
c          DMEX    dMEX/dt, kg/s
c          DMIN    dMIN/dt, kg/s
c          DHBD    dHB/dt, J/s
c          DHEX    dHEX/dt, J/s
c          DHIN    dHIN/dt, J/s
c          DKTD    dKT/dt, J/kg-s
c          DMPD    dMPD/dt, kg/s
c          DMPD    dMPD/dt, kg/s
c          DMPE    dMPE/dt, kg/s
c          DMRB    dMRB/dt, kg/s
c          DMRC    dMRC/dt, kg/s
c          DMRE    dMRE/dt, kg/s
c          DUCD    dUC/dt, J/s
c          FIN     mass inflow rate through intake valve, kg/s
c          FEX     mass outflow rate through exhaust valve, kg/s
c          P0EV    stagnation pressure downstream of exhaust valve, Pa
c          P0IV    stagnation pressure upstream of intake valve, Pa
c          T0EV    stagnation temperature downstream of exhaust valve, K
c          T0IV    stagnation temperature upstream of intake valve, K
c          QDOT    heat transfer rate to coolant, J/s
c          WDOT    power output to piston, J/s
c
c      If manifolds are present, the duct state (P,C,V,Y) outside of the
c      valve and EGR (point 4 for intake, 1 for exhaust) is also set.
c-----------------------------------------------------------------------------
c      Quantities used only in this subroutine include:
c          QDOTV   heat transfer rate in a valve, J/s
c-----------------------------------------------------------------------------
       REAL        KT,MC,MIB,MPE,MRE,MIN,MEX
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION   IW(50),RW(400)
       COMMON  /IWVV/  IW
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (KT  ,RW(4))
       EQUIVALENCE (MIN ,RW(11))
       EQUIVALENCE (MEX ,RW(16))
       EQUIVALENCE (MPE ,RW(18))
       EQUIVALENCE (MRE ,RW(19))
       EQUIVALENCE (DUCD,RW(21))
       EQUIVALENCE (DMPD,RW(22))
       EQUIVALENCE (DMRD,RW(23))
       EQUIVALENCE (DKTD,RW(24))
       EQUIVALENCE (QDOT,RW(25))
       EQUIVALENCE (WDOT,RW(26))
       EQUIVALENCE (DMIN,RW(31))
       EQUIVALENCE (DHIN,RW(32))
       EQUIVALENCE (DHBD,RW(33))
       EQUIVALENCE (DMPB,RW(34))
       EQUIVALENCE (DMRB,RW(35))
       EQUIVALENCE (DMEX,RW(36))
       EQUIVALENCE (DHEX,RW(37))
       EQUIVALENCE (DMPE,RW(38))
       EQUIVALENCE (DMRE,RW(39))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (VELT,RW(106))
       EQUIVALENCE (XPC ,RW(107))
       EQUIVALENCE (FIN ,RW(110))
       EQUIVALENCE (FEX ,RW(111))
       EQUIVALENCE (DVDT,RW(121))
       EQUIVALENCE (AFLE,RW(122))
       EQUIVALENCE (AFLI,RW(123))
       EQUIVALENCE (AHTC,RW(124))
       EQUIVALENCE (MIB ,RW(126))
       EQUIVALENCE (XPB ,RW(127))
       EQUIVALENCE (TB  ,RW(128))
       EQUIVALENCE (TE  ,RW(129))
       EQUIVALENCE (XPE ,RW(130))
       EQUIVALENCE (DCKT,RW(141))
       EQUIVALENCE (VELI,RW(145))
       EQUIVALENCE (VELE,RW(146))
       EQUIVALENCE (P0IV,RW(147))
       EQUIVALENCE (T0IV,RW(148))
       EQUIVALENCE (P0EV,RW(149))
       EQUIVALENCE (T0EV,RW(150))
       EQUIVALENCE (CPC ,RW(151))
       EQUIVALENCE (CPM ,RW(152))
       EQUIVALENCE (CPB ,RW(153))
       EQUIVALENCE (CPE ,RW(154))
       EQUIVALENCE (GAMM,RW(158))
       EQUIVALENCE (GAMB,RW(159))
       EQUIVALENCE (GAMC,RW(157))
       EQUIVALENCE (GAME,RW(160))
       EQUIVALENCE (HCS ,RW(163))
       EQUIVALENCE (HMS ,RW(164))
       EQUIVALENCE (HBS ,RW(165))
       EQUIVALENCE (HES ,RW(166))
       EQUIVALENCE (RHOC,RW(169))
       EQUIVALENCE (RHOM,RW(170))
       EQUIVALENCE (RHOB,RW(171))
       EQUIVALENCE (RHOE,RW(172))
       EQUIVALENCE (APIS,RW(181))
       EQUIVALENCE (AHTE,RW(182))
       EQUIVALENCE (AHTI,RW(183))
       EQUIVALENCE (DT1 ,RW(199))
       EQUIVALENCE (VPIS,RW(200))
       EQUIVALENCE (PE  ,RW(204))
       EQUIVALENCE (PI  ,RW(205))
       EQUIVALENCE (TI  ,RW(206))
       EQUIVALENCE (TEGR,RW(207))
       EQUIVALENCE (FEGR,RW(208))
       EQUIVALENCE (TM  ,RW(209))
       EQUIVALENCE (CDI ,RW(221))
       EQUIVALENCE (CDIB,RW(222))
       EQUIVALENCE (CDE ,RW(223))
       EQUIVALENCE (CDEB,RW(224))
       EQUIVALENCE (FRAB,RW(234))
       EQUIVALENCE (XPM ,RW(240))
       EQUIVALENCE (STGE,RW(255))
       EQUIVALENCE (STIV,RW(256))
       EQUIVALENCE (STEV,RW(257))
       EQUIVALENCE (TEV ,RW(268))
       EQUIVALENCE (TIV ,RW(269))
       EQUIVALENCE (TW  ,RW(270))
       EQUIVALENCE (FKEI,RW(271))
       EQUIVALENCE (FKEE,RW(272))
       EQUIVALENCE (FTDG,RW(276))
       EQUIVALENCE (FTPG,RW(280))
       EQUIVALENCE (TRRI,RW(329))
c-----------------------------------------------------------------------------
c    Variable locations in IW:
       EQUIVALENCE (IANG,IW(1))
       EQUIVALENCE (LEFL,IW(8))
       EQUIVALENCE (LIFL,IW(9))
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
c-----------------------------------------------------------------------------
c    cylinder diagnostic monitor
       COMMON /MONCVV/IUMONC,MONC
c------------------------------------------------------------------------------
c    initialize for summations
       DMPD = 0
       DMRD = 0
       DMPE = 0
       DMRE = 0
       DMPB = 0
       DMRB = 0

c    mass in and out derivatives from precalculated flows
       DMIN = FIN
       DMEX = FEX
c
c -- piston power
c
       WDOT = PC*DVDT
c
c    initialize dU/dt (energy balance) with the piston power contribution
       DUCD = - WDOT
c
c -- large-scale turbulence kinetic energy - explicit and implicit terms
c
       DKTD = FTPG*ABS(VPIS*VPIS*VPIS)*AHTC/VC
       DCKT = 0.666667*DVDT/VC + FTDG*VELT/(VC*0.333333)
c
c -- in-cylinder heat transfer rate
c
       QDOT = STGE*VELT*RHOC*CPC*AHTC*(TC - TW)
c    maximum rate to reach wall temperature
       QDOTM = MC*CPC*(TC - TW)/(GAMC*DT1)
c    heat transfer clipping
       IF (ABS(QDOT).GT.ABS(QDOTM)) QDOT = QDOTM
c    cylinder heat transfer contribution to the energy balance
       DUCD = DUCD - QDOT
c
c -- intake flow
c
c    contributions to balance equations
       IF (LIFL.NE.0)  THEN
           IF (LIFL.EQ.1)  THEN
c                    mixed charge intake; set null backflow mass input
                       DMPB = 0
                       DMRB = 0
c                    get mixed charge products content (assumes XPE = FRAB)
                       TERM = FEGR/(1 - FEGR)
                       XPM = FRAB*TERM/(TERM + 1)
c                    mix EGR gas and fresh charge for enthalpy
                        HMS = (HRVV(T0IV)
     ;                        + TERM*HMVV(FRAB,TEGR))/(1 + TERM)
c                    get mixed charge temperature from enthalpy
                       CALL GTHMVV(XPM,HMS,TM)
                       CPM = CPMVV(XPM,TM)
                       GAMM = CPM/CVMVV(XPM,TM)
                       RHOM = P0IV/PVMVV(XPM,TM)
c                    mass balance contributions
                       DMPD = DMPD + FIN*XPM
                       DMRD = DMRD + FIN*(1 - XPM)
c                    energy inflow rate (boundary upstream of valve)
                       DHIN = FIN*HMS
c                    heat transfer rate to valve area
                       QDOTV = STIV*AHTI*VELI*RHOM*CPM*(TM - TIV)
c                    maximum rate
                       QDOTVM = FIN*CPM*(TM - TIV)
c                    heat transfer rate clipping
                       IF (ABS(QDOTV).GT.ABS(QDOTVM))  QDOTV = QDOTVM
c                    kinetic enery balance contribution
                       DKTD = DKTD + 0.5*VELI*VELI*FKEI*FIN/MC
               ELSEIF (LIFL.EQ.2)  THEN
c                    backflow gas intake; backflow slug mass balances
                       DMPB = - FIN*XPB
                       DMRB = - FIN*(1 - XPB)
c                    cylinder mass balance contributions
                       DMPD = DMPD - DMPB
                       DMRD = DMRD - DMRB
c                    energy inflow rate upstream of valve
                       DHIN = FIN*HBS
c                    backflow slug energy balance
                       DHBD = - DHIN
c                    heat transfer rate to valve area
                       QDOTV = STIV*AHTI*VELI*RHOB*CPB*(TB - TIV)
c                    maximum rate
                       QDOTVM = FIN*CPB*(TB - TIV)
c                    heat transfer rate clipping
                       IF (ABS(QDOTV).GT.ABS(QDOTVM))  QDOTV = QDOTVM
c                    kinetic enery balance contribution
                       DKTD = DKTD + 0.5*VELI*VELI*FKEI*FIN/MC
               ELSEIF (LIFL.EQ.3)  THEN
c                backflow; balance on backflow gas
                   DMPB =  - FIN*XPC
                   DMRB =  - FIN*(1 - XPC)
c                mass balance contributions (boundary upstream of valve)
                   DMPD = DMPD - DMPB
                   DMRD = DMRD - DMRB
c                heat transfer rate to valve area
                   QDOTV = STIV*AHTI*VELI*RHOC*CPC*(TC - TIV)
c                maximum rate
                   QDOTVM = - FIN*CPC*(TC - TIV)
c                heat transfer rate clipping
                   IF (ABS(QDOTV).GT.ABS(QDOTVM))  QDOTV = QDOTVM
c                effect of heat transfer to valve flow for outflow
                   DHBD = - DHIN - QDOTV
c                energy inflow rate upstream of valve
                   DHIN = FIN*HCS + QDOTV
c                turbulence kinetic energy balance contribution
                   DKTD = DKTD + KT*FIN/MC
               ENDIF
c        heat transfer rate contribution
           QDOT = QDOT + QDOTV
c        energy balance contribution (boundary upstream of valve)
           DUCD = DUCD + DHIN - QDOTV
           ENDIF
c
c -- exhaust flow
c
c    contributions to balance equations
       IF (LEFL.NE.0) THEN
           IF (LEFL.EQ.1)  THEN
c                flow from cylinder; exit slug mass balance
                   DMPE = FEX*XPC
                   DMRE = FEX*(1 - XPC)
c                heat transfer rate to valve area
                   QDOTV = STEV*AHTE*VELE*RHOC*CPC*(TC - TEV)
c                maximum rate
                   QDOTVM = FEX*CPC*(TC - TEV)
c                heat transfer rate clipping
                   IF (ABS(QDOTV).GT.ABS(QDOTVM)) QDOTV = QDOTVM
c                energy flow rate downstream of valve (EB on valve)
                   DHEX = FEX*HCS - QDOTV
c                kinetic energy balance contribution
                   DKTD = DKTD - KT*FEX/MC
               ELSEIF (LEFL.EQ.2)  THEN
c                backflow from exhaust; mass balances on exhaust slug
                   DMPE = FEX*XPE
                   DMRE = FEX*(1 - XPE)
c                energy flow rate upstream of valve
                   DHEX = FEX*HES
c                kinetic energy balance contribution
                   DKTD = DKTD - 0.5*VELE*VELE*FKEE*FEX/MC
c                heat transfer rate to valve area
                   QDOTV = STEV*AHTE*VELE*RHOE*CPE*(TE - TEV)
c                maximum rate
                   QDOTVM = - FEX*CPE*(TE - TEV)
c                heat transfer rate clipping
                   IF (ABS(QDOTV).GT.ABS(QDOTVM)) QDOTV = QDOTVM
               ENDIF
c        heat transfer rate contribution
           QDOT = QDOT + QDOTV
c        energy balance contribution
           DUCD = DUCD - QDOTV - DHEX
c        contributions to the cylinder mass balance
           DMPD = DMPD - DMPE
           DMRD = DMRD - DMRE
           ENDIF
c
c    final term in kinetic energy
       DCKT = DCKT + (DMPD + DMRD)/MC
       IERR = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ADVGVV(DT,Y,YN,IERR)
c
c      Advances differential variables during gas exchange from Y() to YN()
c      using time step DT and derivatives in the work array.
c
c      If backflow gas would be depleted, advance is not made unless IERR = -1
c      at call, in which case DT is returned as the time step to depletion.
c----------------------------------------------------------------------------
c    Arguments:
c      Loaded at call:
c          DT      time step, s
c          Y(I)    vector of initial values of solution variables
c      Returned:
c          YN(I)     vector of advanced solution variables
c          IERR = 0  advance made for DT
c                -1  advanced to depletion of backflow gas and DT returned
c                    as the advancement step
c----------------------------------------------------------------------------
c      Differential variable list in Y(I) and YN(I):
c   I
c   1      UC      internal energy in cylinder,J
c   2      MP      total product mass in cylinder, kg
c   3      MR      total reactant mass in cylinder, kg
c   4      KT      turbulent kinetic energy , J/kg
c   5      QC      heat transferred from gas in cylinder this cycle, J
c   6      WP      total work done on piston this cycle, J
c  11      MIN     mass in through intake valve this cycle,kg
c  12      HIN     total enthalpy in through intake this cycle, J
c  13      HB      total enthalpy of intake backflow gas, J
c  14      MPB     mass of products in intake backflow gas, kg
c  15      MRB     mass of reactants in intake backflow gas, kg
c  16      MEX     mass out through exhaust valve this cycle,kg
c  17      HEX     total enthalpy of mixed exhausted gas this cycle, J
c  18      MPE     mass of products in exhaust this cycle, kg
c  19      MRE     mass of reactants in exhaust this cycle, kg
c      Derivatives are in RW(I+20)
c-----------------------------------------------------------------------------
c      The following quantities must be loaded in RW at call:
c          DMPB    dMPB/dt, kg/s
c          DMRB    dMRB/dt, kg/s
c          MC      gas mass in cylinder, kg
c-----------------------------------------------------------------------------
c      The following quantities must be loaded in IW at call:
c          LEFL    0 exhaust closed
c                  1 exhaust open
c          LIFL    0 if intake valve is closed
c                  1 mixed charge inflow
c                  2 backflow gas inflow
c                  3 backflow
c-----------------------------------------------------------------------------
c      Variables used only in this subroutine:
c          DMBDT   dMB/dT, kg/s
c          MIBX    backflow mass, kg
c-----------------------------------------------------------------------------
       REAL        MC,MIBX
       DIMENSION   IW(50),RW(400),Y(20),YN(20),F(20)
       COMMON  /IWVV/  IW
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (F(1),RW(21))
       EQUIVALENCE (DMPB,RW(34))
       EQUIVALENCE (DMRB,RW(35))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (DCKT,RW(141))
c-----------------------------------------------------------------------------
c    Variable locations in IW:
       EQUIVALENCE (LEFL,IW(8))
       EQUIVALENCE (LIFL,IW(9))
c-----------------------------------------------------------------------------
c    cylinder diagnostic monitor
       COMMON /MONCVV/IUMONC,MONC
c------------------------------------------------------------------------------
c    check for backflow gas inflow
       MIBX = Y(14) + Y(15)
       IF (LIFL.EQ.2)  THEN
c            check for backflow gas depletion
               DMBDT = DMPB + DMRB
               IF ((- DMBDT*DT).GE.0.9999*MIBX)  THEN
c                    backflow gas will be depleted
                       IF (MONC.GT.0) WRITE (IUMONC,2)
2                      FORMAT(' Inlet backflow discontinuity')
c                    reset time step
                       DT = - MIBX/DMBDT
c                    set flag
                       IERR = - 1
                   ELSE
c                    not yet depleted
                       IERR = 0
                   ENDIF
           ELSE
c            no backflow
               IERR = 0
           ENDIF
c
c    advance cylinder variables
       DO I=1,6
           IF (I.EQ.4)  THEN
c                semi-implicit for KE
                   TERM = 0.5*DT*DCKT
                   YN(I) = (Y(I)*(1 - TERM) + F(I)*DT)/(1 + TERM)
               ELSE
c                explicit
                   YN(I) = Y(I) + F(I)*DT
               ENDIF
           ENDDO
c
c    clipping of masses and kinetic energy
       DO I=2,4
           YNX = YN(I)
           YN(I) = MAX(YN(I),0.5*Y(I))
           YN(I) = MIN(YN(I),2.0*Y(I))
           IF (YN(I).NE.YNX) THEN
               YNA = 0.5*(YN(I) + Y(I))
c            clipped; average successive values if oscillating
               IF ((YN(I)-YNA)*(Y(I)-YNA).LT.0) YN(I) = YNA
               ENDIF
           ENDDO
c
c    check for backflow gas
       IF (LIFL.EQ.2)  THEN
c        advance backflow variables
           DO I=13,15
               IF (IERR.LT.0)  THEN
c                   depletion
                       YN(I) = 0
                   ELSE
c                    not yet depleted
                       YN(I) = Y(I) + F(I)*DT
                   ENDIF
               ENDDO
           ENDIF
c    advance inflow energy and mass
       IF (LIFL.GT.0)  THEN
           DO I=11,12
                YN(I) = Y(I) + F(I)*DT
                ENDDO
           ENDIF
c
c    check for exhaust flow
       IF (LEFL.GT.0)  THEN
c        advance exhaust variables
           DO I=16,19
               YN(I) = Y(I) + F(I)*DT
               ENDDO
           ENDIF
c
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CPYGVV(Y,YN)
c
c      Copies differential variables for gas exchange stage from Y to YN
c-----------------------------------------------------------------------------
       DIMENSION   Y(20),YN(20)
c-----------------------------------------------------------------------------
       DO I=1,6
           YN(I) = Y(I)
           ENDDO
       DO I=11,19
           YN(I) = Y(I)
           ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ALGGVV
c
c      Advances algebraic variables during gas exchange
c-----------------------------------------------------------------------------
c      Flow into the cylinder from the exhaust is assumed to occur at the
c      cycle-averaged exhaust state computed at the end of the previous
c      cycle. On each cycle the mass and energy flows out the exhaust
c      are integrated over the cycle and then mixed to determine the
c      next cycle-average exhaust state.
c-----------------------------------------------------------------------------
c      The following quantities must be loaded in RW at call:
c          HB      total enthalpy of intake backflow gas, J
c          HE      total enthalpy of mixed exhausted gas this cycle, J
c          KE      kinetic energy of large-scale turbulence in cylinder, J/kg
c          MPB     mass of products in intake backflow gas, kg
c          MPC     mass of products in cylinder, kg
c          MPE     mass of products in exhaust this cycle, kg
c          MRB     mass of reactants in intake backflow gas, kg
c          MRC     mass of reactants in cylinder, kg
c          MRE     mass of reactants in exhaust this cycle, kg
c          TEV     exhaust valve heat transfer surface temperature, K
c          TIV     inlet valve heat transfer surface temperature, K
c          UC      total internal energy of gas in cylinder, J
c          VC      cylinder volume, m**3
c-----------------------------------------------------------------------------
c      The following quantities must be loaded in IW at call:
c          LEFL    0 if exhaust valve closed
c                  1 if exhaust outflow
c                  2 if exhaust backflow
c          LIFL    0 if intake valve is closed
c                  1 mixed charge inflow
c                  2 backflow gas inflow
c                  3 backflow
c-----------------------------------------------------------------------------
c      The following quantities are returned in RW:
c          CPB     specific heat Cp of backflow gas, J/kg-K
c          CPC     specific heat Cp of gas in cylinder, J/kg-K
c          GAMB    Cp/Cv for backflow gas
c          GAMC    Cp/Cv for gas in cylinder
c          HBS     specific enthalpy of inlet backflow gas, J/kg
c          HCS     specific enthalpy of gas in cylinder, J/kg
c          MC      gass mass in cylinder, kg
c          MIB     mass of backflow gas in inlet manifold, kg
c          PC      pressure in cylinder, Pa
c          RHOB    density of backflow gas, kg/m**3
c          RHOC    density of gas in cylinder, kg/m**3
c          TB      temperature of backflow gas, K
c          TC      temperature of cylinder gas, K
c          UCS     specific internal energy of gas in cylinder, J/kg
c          VELT    velocity of large-scale turbulence, m/s
c          XPB     product mass fraction in backflow gas
c          XPC     product mass fraction in cylinder
c-----------------------------------------------------------------------------
       REAL        KT,MC,MIB,MP,MPB,MR,MRB
       DIMENSION   IW(50),RW(400)
       COMMON  /IWVV/  IW
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (UC  ,RW(1))
       EQUIVALENCE (MP  ,RW(2))
       EQUIVALENCE (MR  ,RW(3))
       EQUIVALENCE (KT  ,RW(4))
       EQUIVALENCE (TU  ,RW(9))
       EQUIVALENCE (HB  ,RW(13))
       EQUIVALENCE (MPB ,RW(14))
       EQUIVALENCE (MRB ,RW(15))
       EQUIVALENCE (HEX ,RW(17))
       EQUIVALENCE (MPE ,RW(18))
       EQUIVALENCE (MRE ,RW(19))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (VELT,RW(106))
       EQUIVALENCE (XPC ,RW(107))
       EQUIVALENCE (FIN ,RW(110))
       EQUIVALENCE (FEX ,RW(111))
       EQUIVALENCE (TH  ,RW(114))
       EQUIVALENCE (VELH,RW(116))
       EQUIVALENCE (VELU,RW(117))
       EQUIVALENCE (MIB ,RW(126))
       EQUIVALENCE (XPB ,RW(127))
       EQUIVALENCE (TB  ,RW(128))
       EQUIVALENCE (VI10,RW(143))
       EQUIVALENCE (VE10,RW(144))
       EQUIVALENCE (VELI,RW(145))
       EQUIVALENCE (VELE,RW(146))
       EQUIVALENCE (CPB ,RW(153))
       EQUIVALENCE (CPC ,RW(151))
       EQUIVALENCE (GAMC,RW(157))
       EQUIVALENCE (GAMB,RW(159))
       EQUIVALENCE (HCS ,RW(163))
       EQUIVALENCE (HBS ,RW(165))
       EQUIVALENCE (HES ,RW(166))
       EQUIVALENCE (RHOC,RW(169))
       EQUIVALENCE (RHOB,RW(171))
       EQUIVALENCE (UCS ,RW(175))
       EQUIVALENCE (PI  ,RW(205))
       EQUIVALENCE (TEV ,RW(268))
       EQUIVALENCE (TIV ,RW(269))
c-----------------------------------------------------------------------------
c    Variable locations in IW:
       EQUIVALENCE (IANG,IW(1))
       EQUIVALENCE (LEFL,IW(8))
       EQUIVALENCE (LIFL,IW(9))
c-----------------------------------------------------------------------------
       IANG = IW(1)
       FIN = RW(110)
       FEX = RW(111)
c    mass distribution in cylinder
       MC = MP + MR
       XPC = MP/MC
c    calculate cylinder gas temperature from internal energy
       UCS = UC/MC
       CALL GTUMVV(XPC,UCS,TC)
c    calculate density from mass and volume
       RHOC = MC/VC
c    calculate the pressure from temperature and density
       TERM = PVMVV(XPC,TC)
       PC = TERM*RHOC
c    get cylinder gas properties
       CPC = CPMVV(XPC,TC)
       GAMC = CPC/CVMVV(XPC,TC)
       HCS = HMVV(XPC,TC)
c   calculate turbulence velocity
       VELT = SQRT(2*KT)
c    calculate backflow mass
       MIB = MPB + MRB
c    check for no more backflow gas
       IF (LIFL.EQ.2)  THEN
           IF (MIB.EQ.0.) HB = 0
           ENDIF
c    check for intake backflow
       IF (LIFL.EQ.3)  THEN
c        check for existing backflow mass
           IF (MIB.GT.0.)  THEN
c            remix the backflow
               XPB = MPB/MIB
c            calculate backflow gas temperature from enthalpy
               HBS = HB/MIB
               CALL GTHMVV(XPB,HBS,TB)
c            get backflow gas properties
               CPB = CPMVV(XPB,TB)
               GAMB = CPB/CVMVV(XPB,TB)
               RHOB = PI/PVMVV(XPB,TB)
               ENDIF
           ENDIF
c    other variables
       TH = TC
       TU = TC
c    flow velocities/10
       VI10 = VELI/10
       IF (FIN.LT.0) VI10 = - VI10
       VE10 = VELE/10
       IF (FEX.LT.0) VE10 = - VE10
c    other turbulence velocities
       VELH = VELT
       VELU = VELT
       RETURN
       END
c******************************************************************************

