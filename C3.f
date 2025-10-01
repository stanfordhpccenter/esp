c******************************************************************************
c
c      Subroutines for expansion stage
c
c******************************************************************************
c
       SUBROUTINE  DDTEVV(IERR)
c
c      Derivative calculations during expansion stage
c-----------------------------------------------------------------------------
c    The following quantities must be loaded in RW at call:
c
c          AHTC    heat transfer area in cylinder, m**2
c          CPC     specific heat of gas in cylinder, J/kg-K
c          DVDT    dV/dt, m**3/s
c          FTDE    factor in turbulence dissipation model during expansion
c          FTPE    factor in turbulence production model during expansion
c          GAMC    Cp/Cv for cylinder gas
c          KT      kinetic energy of large-scale turbulence in cylinder, J/kg
c          PC      pressure in cylinder, Pa
c          RHOC    density of gas in cylinder, kg/m**3
c          STE     Stanton number during expansion
c          TC      temperature of gas in cylinder, K
c          TW      temperature of wall, K
c          UC      internal energy of gas in cylinder, J
c          VC      cylinder volume, m**3
c          VELT    velocity of large-scale turbulence, m/s
c          VPIS    piston velocity, m/s
c-----------------------------------------------------------------------------
c    The following quantities are returned in RW:
c
c      Derivatives of variables advanced by differential equations:
c          DCKT    coefficient of KT in KT equation, 1/s
c          DKTD    dKT/dt, J/kg-s
c          DMPD    dMP/dt = 0
c          DMRD    dMR/dt = 0
c          DUCD    dUC/dt, J/s
c          QDOT    heat transfer rate from cylinder gas, J/s
c          WDOT    power output to piston, J/s
c-----------------------------------------------------------------------------
       REAL        KT,MC
       DIMENSION   RW(400)
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (KT  ,RW(4))
       EQUIVALENCE (DUCD,RW(21))
       EQUIVALENCE (DMPD,RW(22))
       EQUIVALENCE (DMRD,RW(23))
       EQUIVALENCE (DKTD,RW(24))
       EQUIVALENCE (QDOT,RW(25))
       EQUIVALENCE (WDOT,RW(26))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (VELT,RW(106))
       EQUIVALENCE (DVDT,RW(121))
       EQUIVALENCE (AHTC,RW(124))
       EQUIVALENCE (DCKT,RW(141))
       EQUIVALENCE (CPC ,RW(151))
       EQUIVALENCE (GAMC,RW(157))
       EQUIVALENCE (RHOC,RW(169))
       EQUIVALENCE (DT1 ,RW(199))
       EQUIVALENCE (VPIS,RW(200))
       EQUIVALENCE (STE ,RW(254))
       EQUIVALENCE (TW  ,RW(270))
       EQUIVALENCE (FTDE,RW(275))
       EQUIVALENCE (FTPE,RW(279))
c-----------------------------------------------------------------------------
c    heat transfer rate
       QDOT = STE*VELT*RHOC*CPC*AHTC*(TC - TW)
c    maximum rate to reach wall temperature
       QDOTM = MC*CPC*(TC - TW)/(GAMC*DT1)
c    heat transfer clipping
       IF (ABS(QDOT).GT.ABS(QDOTM)) QDOT = QDOTM
       WDOT = PC*DVDT
c    energy balance
       DUCD = - QDOT - WDOT
c    kinetic energy balance terms for semi-implicit advance
       DKDT = FTPE*ABS(VPIS*VPIS*VPIS)*AHTC/VC
       DCKT = 0.666667*DVDT/VC + FTDE*VELT/(VC**0.333333)
c    mass balances
       DMPD = 0
       DMRD = 0
c    exit
       RETURN
       IERR = 0
       END
c******************************************************************************
c
       SUBROUTINE ADVEVV(DT,Y,YN,IERR)
c
c      Advances differential variables for expansion stage from Y() to YN()
c      using time step DT and derivatives in the work array.
c----------------------------------------------------------------------------
c    Arguments:
c      Loaded at call:
c          DT      time step, s
c          Y(I)    vector of initial values of solution variables
c      Returned:
c          YN(I)     vector of advanced solution variables
c          IERR = 0  advance ok
c----------------------------------------------------------------------------
c    Differential variable list in Y(I) and YN(I):
c   I
c   1      UC      internal energy in cylinder,J
c   2      MP      total product mass in cylinder, kg
c   3      MR      total reactant mass in cylinder, kg
c   4      KT      turbulent kinetic energy , J/kg
c   5      QC      heat transferred from gas in cylinder this cycle, J
c   6      WP      total work done on piston this cycle, J
c    Derivatives are in RW(I+20)
c----------------------------------------------------------------------------
       DIMENSION   RW(400),Y(20),YN(20),F(20)
       COMMON  /RWVV/  RW
c----------------------------------------------------------------------------
c   Variable locations in arrays:
       EQUIVALENCE (F(1),RW(21))
       EQUIVALENCE (DCKT,RW(141))
c-----------------------------------------------------------------------------
c    advance variables
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
c    kinetic energy clipping
       YN(4) = MAX(YN(4),0.5*Y(4))
       YN(4) = MIN(YN(4),2.0*Y(4))
c    normal return
       IERR = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CPYEVV(Y,YN)
c
c      Copies differential variables for expansion stage from Y to YN
c-----------------------------------------------------------------------------
       DIMENSION   Y(20),YN(20)
c-----------------------------------------------------------------------------
       DO I=1,6
           YN(I) = Y(I)
           ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ALGEVV
c
c      Advances algebraic variables for expansion stage
c-----------------------------------------------------------------------------
c    The following quantities must be loaded in RW at call:
c
c          UC      total internal energy of gas in cylinder, J
c          MP      reactant mass in cylinder, kg
c          MR      reactant mass in cylinder, kg
c          KT      kinetic energy of large-scale turbulence in cylinder, J/kg
c          VC      cylinder volume, m**3
c-----------------------------------------------------------------------------
c    The following quantities are returned in RW:
c          CPC     specific heat of gas in cylinder, J/kg
c          GAMC    Cp/Cv for gas in cylinder
c          PC      pressure in cylinder, Pa
c          RHOC    density of gas in cylinder, kg/m**3
c          TC      temperature of gas in cylinder, K
c          UCS     specific internal energy of cylinder gas, J/kg
c          VELT    velocity of large-scale turbulence, m/s
c          XPC     mass fraction of products in cylinder
c-----------------------------------------------------------------------------
       REAL        KT,MC,MP,MR
       DIMENSION   RW(400)
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (UC  ,RW(1))
       EQUIVALENCE (MP  ,RW(2))
       EQUIVALENCE (MR  ,RW(3))
       EQUIVALENCE (KT  ,RW(4))
       EQUIVALENCE (TU  ,RW(9))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (VELT,RW(106))
       EQUIVALENCE (XPC ,RW(107))
       EQUIVALENCE (TH  ,RW(114))
       EQUIVALENCE (VELH,RW(116))
       EQUIVALENCE (VELU,RW(117))
       EQUIVALENCE (CPC ,RW(151))
       EQUIVALENCE (GAMC,RW(157))
       EQUIVALENCE (RHOC,RW(169))
       EQUIVALENCE (UCS ,RW(175))
c-----------------------------------------------------------------------------
c    get total mass
       MC = MR + MP
c    product mass fraction
       XPC = MP/MC
c    calculate temperature from internal energy
       UCS = UC/MC
       CALL GTUMVV(XPC,UCS,TC)
c    set other temperatures
       TH = TC
       TU = TC
c    calculate density from mass and volume
       RHOC = MC/VC
c    calculate the pressure from temperature and density
       PC = PVMVV(XPC,TC)*RHOC
c    get specific heat and gamma
       CPC = CPMVV(XPC,TC)
       GAMC = CPC/CVMVV(XPC,TC)
c    calculate turbulence velocity
       VELT = SQRT(2*KT)
c    set other velocities
       VELH = 0
       VELU = 0
       RETURN
       END
c******************************************************************************

