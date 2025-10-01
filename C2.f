c******************************************************************************
c
c      Subroutines for burn stage
c
c******************************************************************************
c
       SUBROUTINE  DDTBVV(IERR)
c
c      Derivative calculations during burning
c-----------------------------------------------------------------------------
c    The following quantities must be loaded in RW at call:
c
c          AFLA    projected flame area, m**2
c          AHTH    heat transfer area for burned gas in cylinder, m**2
c          AHTU    heat transfer area for unburned gas in cylinder, m**2
c          CPH     specific heat Cp of burned gas in cylinder, J/kg-K
c          CPU     specific heat Cp of unburned gas in cylinder, J/kg-K
c          DVDT    dV/dt, m**3/s
c          FTDB    factor in turbulence dissipation model during burn
c          FTPB    factor in turbulence production model during burn
c          GAFH    (gamma - 1)/gamma for burned gas
c          GAFU    (gamma - 1)/gamma for unburned gas
c          HH      total enthalpy of burned gas, J
c          HHS     specific enthalpy of burned gas, J/kg
c          HU      total enthalpy of unburned gas, J
c          HUS     specific enthalpy of unburned gas, J/kg
c          KH      turbulence kinetic energy of burned gas, J/kg
c          KU      turbulence kinetic energy of unburned gas, J/kg
c          MH      mass of burned gas in cylinder, kg
c          MU      unburned mass, kg
c          MUE     residual unburned mass at end of combustion, kg
c          PC      pressure in cylinder, Pa
c          PVH     PV of burned gas, J/kg
c          PVU     PV of unburned gas, J/kg
c          RHOH    density of burned gas, kg/m**3
c          RHOU    density of unburned gas, kg/m**3
c          STBH    Stanton number during burn for burned gas
c          STBU    Stanton number during burn for unburned gas
c          TH      temperature of burned gas, K
c          TU      temperature of unburned gas, K
c          TW      temperature of wall, K
c          VC      cylinder volume, m**3
c          VH      burned volume, m**3
c          VU      unburned volume, m**3
c          VELH    turbulence velocity in burned zone, m/s
c          VELU    turbulence velocity in unburned zone, m/s
c          VFLA    flame speed, m/s
c          VPIS    piston speed, m/s
c          XPU     product mass fraction in unburned gas
c-----------------------------------------------------------------------------
c    The following quantities are returned in RW:
c
c      Derivatives of variables advanced by differential equations:
c          DCKT    coefficient of KT in KT equation, 1/s
c          DCKU    coefficient of KU in KU equation, 1/s
c          DUCD    dUC/dt, J/s
c          DMPD    dMP/dt, J/s
c          DMRD    dMR/dt, J/kg-s
c          DKHD    dKH/dt, J/kg-s
c          DHUD    dHU/dt, kg/s
c          DMUD    dMU/dt, kg/s
c          DKUD    dKU/dt, J/kg-s
c          QDOT    heat transfer rate from cylinder gas, J/s
c          WDOT    power output to piston, J/s
c-----------------------------------------------------------------------------
c      Variables used only in this subroutine include:
c          QHDOT   heat transfer rate from burned gas, J/s
c          QUDOT   heat transfer rate from unburned gas, J/s
c          DPDT    dP/dt, Pa/s
c          DHHD    dHH/dt, J/s
c-----------------------------------------------------------------------------
       REAL        KH,KU,MH,MU,MUE
c-----------------------------------------------------------------------------
       DIMENSION   RW(400)
c-----------------------------------------------------------------------------
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (KH  ,RW(4))
       EQUIVALENCE (MU  ,RW(7))
       EQUIVALENCE (KU  ,RW(8))
       EQUIVALENCE (TU  ,RW(9))
       EQUIVALENCE (PU  ,RW(10))
       EQUIVALENCE (DUCD,RW(21))
       EQUIVALENCE (DMPD,RW(22))
       EQUIVALENCE (DMRD,RW(23))
       EQUIVALENCE (DKHD,RW(24))
       EQUIVALENCE (QDOT,RW(25))
       EQUIVALENCE (WDOT,RW(26))
       EQUIVALENCE (DMUD,RW(27))
       EQUIVALENCE (DKUD,RW(28))
       EQUIVALENCE (DSSU,RW(29))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (TH  ,RW(114))
       EQUIVALENCE (VELH,RW(116))
       EQUIVALENCE (VELU,RW(117))
       EQUIVALENCE (VFLA,RW(118))
       EQUIVALENCE (AFLA,RW(119))
       EQUIVALENCE (DMHD,RW(120))
       EQUIVALENCE (DVDT,RW(121))
       EQUIVALENCE (AHTC,RW(124))
       EQUIVALENCE (AHTH,RW(131))
       EQUIVALENCE (AHTU,RW(132))
       EQUIVALENCE (VH  ,RW(133))
       EQUIVALENCE (VU  ,RW(134))
       EQUIVALENCE (MUE ,RW(135))
       EQUIVALENCE (MH  ,RW(136))
       EQUIVALENCE (XPU ,RW(138))
       EQUIVALENCE (DCKT,RW(141))
       EQUIVALENCE (DCKU,RW(142))
       EQUIVALENCE (CPH ,RW(155))
       EQUIVALENCE (CPU ,RW(156))
       EQUIVALENCE (GAMH,RW(161))
       EQUIVALENCE (GAMU,RW(162))
       EQUIVALENCE (HHS ,RW(167))
       EQUIVALENCE (HUS ,RW(168))
       EQUIVALENCE (RHOH,RW(173))
       EQUIVALENCE (RHOU,RW(174))
       EQUIVALENCE (PVH ,RW(176))
       EQUIVALENCE (PVU ,RW(177))
       EQUIVALENCE (GAFH,RW(178))
       EQUIVALENCE (GAFU,RW(179))
       EQUIVALENCE (DT1 ,RW(199))
       EQUIVALENCE (VPIS,RW(200))
       EQUIVALENCE (STBU,RW(252))
       EQUIVALENCE (STBH,RW(253))
       EQUIVALENCE (TW  ,RW(270))
       EQUIVALENCE (FTDB,RW(274))
       EQUIVALENCE (FTPB,RW(278))
c------------------------------------------------------------------------------
c    check for unburned mass
       IF (MU.GT.MUE)  THEN
c            unburned gas remains; get burn rate
               DMHD = AFLA*RHOU*VFLA
c            unburned zone heat transfer
               QUDOT = STBU*VELU*RHOU*CPU*AHTU*(TU - TW)
c            maximum rate to reach wall temperature
               QUDOTM = MU*CPU*(TU - TW)/(GAMU*DT1)
c            heat transfer clipping
               IF (ABS(QUDOT).GT.ABS(QUDOTM)) THEN
                   QUDOT = QUDOTM
                   ENDIF
           ELSE
c            unburned gas is depleted
               DMHD = 0
               QUDOT = 0
           ENDIF
c    mass balances
       DMPD = DMHD*(1 - XPU)
       DMRD = - DMPD
       DMUD = - DMHD
c    hot region heat transfer
       QHDOT = STBH*VELH*RHOH*CPH*AHTH*(TH - TW)
c    maximum rate to reach wall temperature
       QHDOTM = MH*CPH*(TH - TW)/(GAMH*DT1)
c    heat transfer clipping
       IF (ABS(QHDOT).GT.ABS(QHDOTM)) THEN
           QHDOT = QHDOTM
           ENDIF
c    total heat transfer rate
       QDOT = QUDOT + QHDOT
c    power
       WDOT = PC*DVDT
c    overall energy balance
       DUCD = - WDOT - QDOT
c    specific entropy change rate for unburned gas
       DSSU = - QUDOT/(MU*TU)
c
c  *  turbulence model
c
c    dP/dt
       DPDT = ( - PC*DVDT + DMHD*(GAFH*(HUS - HHS) + PVH - PVU)
     ;        - GAFU*QUDOT - GAFH*QHDOT)/(VC - GAFU*VU - GAFH*VH)
c    burned zone energy balance
       DHHD = VH*DPDT - QHDOT + DMHD*HUS
c    unburned zone energy balance
       DHUD = VU*DPDT - QUDOT - DMHD*HUS
c    production terms
       TERM =  FTPB*ABS(VPIS*VPIS*VPIS)/VC
       PRODU = TERM*AHTC
       PRODH = PRODH
c   logarithgmic derivatives of T and P for RDT terms
       DLPDT = DPDT/PC
       IF (MU.GT.0)  THEN
               DLTUDT = (DHUD - HUS*DMUD)/(MU*TU*CPU)
           ELSE
               DLTUDT = 0
           ENDIF
       DLTHDT = (DHHD - HHS*DMHD)/(MH*TH*CPH)
c    RDT terms
       RDTHC =  0.66667*(DLTHDT - DLPDT)
       RDTUC =  0.66667*(DLTUDT - DLPDT)
c    dissipation terms
       TERM = FTDB/VC**0.333333
       DISHC = TERM*VELH
       DISUC = TERM*VELU
c    semi-implicit coefficients
       DCKT =  RDTHC + DISHC
       DCKU =  RDTUC + DISUC
c    explicit kinetic energy change rates
       DKHD = PRODH + DMHD*(KU - KH)/MH
       DKUD = PRODU
       IERR = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ADVBVV(DT,Y,YN,IERR)
c
c      Advances differential variables during burn stage from Y() to YN()
c      using time step DT and derivatives in the work array.
c----------------------------------------------------------------------------
c    Arguments:
c      Loaded at call:
c          DT      time step, s
c          Y(I)    vector of initial values of solution variables
c      Returned:
c          YN(I)     vector of advanced solution variables
c          IERR = 0  advance made for DT
c          IERR = -1 unburned gas will be depleted; advanced to discontinuity
c                    and DT returned as the advancement step
c----------------------------------------------------------------------------
c    Differential variable list in Y(I) and YN(I):
c   I
c   1      UC      internal energy in cylinder,J
c   2      MP      total product mass in cylinder, kg
c   3      MR      total reactant mass in cylinder, kg
c   4      KH      turbulent kinetic energy of burned gas, J/kg
c   5      QC      heat transferred from gas in cylinder this cycle, J
c   6      WP      total work done on piston this cycle, J
c   7      MU      mass of burned gas in cylinder, kg
c   8      KU      turbulent kinetic energy of burned gas, J/kg
c    Derivatives are in RW(I+20)
c----------------------------------------------------------------------------
c    The following quantities must be loaded in RW at call:
c          DSSU    d(unburned specific entropy)/dt, J/(kg-K-s)
c          DMUD    dMU/dt, J/s
c          MC      mass in cylinder, kg
c          MUE     residual unburned mass, kg
c----------------------------------------------------------------------------
c      Variables used in this subroutine only:
c          DMH     mass burned this time-step, kg
c----------------------------------------------------------------------------
       REAL        MC,MH,MU,MUE
c----------------------------------------------------------------------------
       DIMENSION   F(20),RW(400),Y(20),YN(20)
c----------------------------------------------------------------------------
       COMMON  /RWVV/  RW
c----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (MU  ,RW(7))
       EQUIVALENCE (TU  ,RW(9))
       EQUIVALENCE (PU  ,RW(10))
       EQUIVALENCE (F(1),RW(21))
       EQUIVALENCE (DMUD,RW(27))
       EQUIVALENCE (DSSU,RW(29))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (MUE ,RW(135))
       EQUIVALENCE (MH  ,RW(136))
       EQUIVALENCE (XPU ,RW(138))
       EQUIVALENCE (DCKT,RW(141))
       EQUIVALENCE (DCKU,RW(142))
c-----------------------------------------------------------------------------
c    cylinder diagnostic monitor
       COMMON /MONCVV/IUMONC,MONC
c------------------------------------------------------------------------------
c    advance the regular differential variables
       DO I=1,8
           IF (I.EQ.4) THEN
c                semi-implicit for KH
                   TERM = 0.5*DT*DCKT
                   YN(I) = (Y(I)*(1 - TERM) + F(I)*DT)/(1 + TERM)
               ELSEIF (I.EQ.8) THEN
c                semi-implicit for KU
                   TERM = 0.5*DT*DCKU
                   YN(I) = (Y(I)*(1 - TERM) + F(I)*DT)/(1 + TERM)
               ELSE
c                implicit advance
                   YN(I) = Y(I) + F(I)*DT
                   ENDIF
           ENDDO
c
c    clipping for end of burn
       IF (YN(7).LE.MUE)  THEN
           YN(7) = MUE
           YN(3) = MUE*(1 - XPU)
           YN(2) = MC - YN(3)
c        set MU=MUE for RK2 step 2
           Y(7) = YN(7)
           ENDIF
c
c    clipping for kinetic energy
       YN(4) = MAX(YN(4),0.5*Y(4))
       YN(4) = MIN(YN(4),2.0*Y(4))
       YN(8) = MAX(YN(8),0.5*Y(8))
       YN(8) = MIN(YN(8),2.0*Y(8))
c
c    special algebraic advance for PC, TH, and TU and other quantities
       MH = MC - YN(7)
       TU0 = Y(9)
       PU0 = Y(10)
       DSS0 = DSSU*DT
       CALL BURNVV(DSS0,IERR)
       YN(9) = TU
       YN(10) = PU
c
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CPYBVV(Y,YN)
c
c      Copies differential variables for burn stage from Y to YN
c-----------------------------------------------------------------------------
       DIMENSION   Y(20),YN(20)
c-----------------------------------------------------------------------------
       DO I=1,10
           YN(I) = Y(I)
           ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ALGBVV
c
c      Advances algebraic variables for burn stage
c-----------------------------------------------------------------------------
c    The following quantities must be loaded in RW at call:
c
c          AHTC    total heat transfer area in cylinder, m**2
c          APIS    piston bore area, m**2
c          FVFL    ratio of flame speed to turb. velocity
c          HH      total enthalpy of burned gas, J
c          HU      total enthalpy of unburned gas, J
c          KH      turbulence kinetic energy of burned gas, J/kg
c          KU      turbulence kinetic energy of burned gas, J/kg
c          MP      mass of products in cylinder, kg
c          MR      mass of reactants in cylinder, kg
c          MU      unburned mass in cylinder, kg
c          MUE     minimum unburned mass in cylinder, kg
c          VC      cylinder volume, m**3
c          VCLR    clearance volume, m**3
c          VLAM    laminar flame speed, m/s
c          XPU     mass fraction of products in unburned gas
c-----------------------------------------------------------------------------
c    The following quantities are returned in RW:
c
c          AFLA    projected flame area, m**2
c          AHTH    heat transfer area for burned gas in cylinder, m**2
c          AHTU    heat transfer area for unburned gas in cylinder, m**2
c          CPH     specific heat Cp of burned gas in cylinder, J/kg-K
c          CPU     specific heat Cp of unburned gas in cylinder, J/kg-K
c          GAFH    (gamma - 1)/gamma for burned gas
c          GAFU    (gamma - 1)/gamma for unburned gas
c          GAMH    gamma = Cp/Cv for burned gas
c          GAMU    gamma = Cp/Cv for unburned gas
c          HHS     specific enthalpy of burned gas, J/kg
c          HUS     specific enthalpy of unburned gas, J/kg
c          MH      burned gas mass in cylinder
c          MC      total mass in cylinder
c          PC      pressure in cylinder, Pa
c          PVH     PV of burned gas, J/kg
c          PVU     PV of unburned gas, J/kg
c          RHOH    density of burned gas, kg/m**3
c          RHOU    density of unburned gas, kg/m**3
c          TH      temperature of burned gas, K
c          TU      temperature of unburned gas, K
c          VH      burned volume, m**3
c          VU      unburned volume, m**3
c          VELH    turbulence velocity of burned gas, m/s
c          VELU    turbulence velocity of unburned gas, m/s
c          VFLA    flame speed, m/s
c          XPC     fraction of mass in cylinder that is products
c-----------------------------------------------------------------------------
c      Quantities used only in this subroutine incude:
c          FAHH    fraction of heat transfer area exposed to burned gas
c          RAFP    projected flame area/piston area
c          VHS     ratio of burned volume to total volume
c-----------------------------------------------------------------------------
       REAL        KH,KU,MC,MH,MP,MR,MU,MUE,MFH
c-----------------------------------------------------------------------------
       DIMENSION   RW(400),IW(50)
c-----------------------------------------------------------------------------
       COMMON  /RWVV/  RW
       COMMON  /IWVV/  IW
c-----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (UC  ,RW(1))
       EQUIVALENCE (MP  ,RW(2))
       EQUIVALENCE (MR  ,RW(3))
       EQUIVALENCE (KH  ,RW(4))
       EQUIVALENCE (MU  ,RW(7))
       EQUIVALENCE (KU  ,RW(8))
       EQUIVALENCE (TU  ,RW(9))
       EQUIVALENCE (PU  ,RW(10))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (VELT,RW(106))
       EQUIVALENCE (XPC ,RW(107))
       EQUIVALENCE (VFH ,RW(112))
       EQUIVALENCE (MFH ,RW(113))
       EQUIVALENCE (TH  ,RW(114))
       EQUIVALENCE (VELH,RW(116))
       EQUIVALENCE (VFLA,RW(118))
       EQUIVALENCE (AFLA,RW(119))
       EQUIVALENCE (AHTC,RW(124))
       EQUIVALENCE (AHTH,RW(131))
       EQUIVALENCE (AHTU,RW(132))
       EQUIVALENCE (VH  ,RW(133))
       EQUIVALENCE (VU  ,RW(134))
       EQUIVALENCE (MUE ,RW(135))
       EQUIVALENCE (MH  ,RW(136))
       EQUIVALENCE (XPU ,RW(138))
       EQUIVALENCE (CPH ,RW(155))
       EQUIVALENCE (CPU ,RW(156))
       EQUIVALENCE (GAMH,RW(161))
       EQUIVALENCE (GAMU,RW(162))
       EQUIVALENCE (HHS ,RW(167))
       EQUIVALENCE (HUS ,RW(168))
       EQUIVALENCE (RHOC,RW(169))
       EQUIVALENCE (RHOH,RW(173))
       EQUIVALENCE (RHOU,RW(174))
       EQUIVALENCE (PVH ,RW(176))
       EQUIVALENCE (PVU ,RW(177))
       EQUIVALENCE (GAFH,RW(178))
       EQUIVALENCE (GAFU,RW(179))
       EQUIVALENCE (APIS,RW(181))
       EQUIVALENCE (THOR,RW(189))
       EQUIVALENCE (VCLR,RW(190))
       EQUIVALENCE (BORE,RW(211))
       EQUIVALENCE (VLAM,RW(232))
       EQUIVALENCE (FVFL,RW(233))
c-----------------------------------------------------------------------------
c    Variable locations in IW:
        EQUIVALENCE (KFGM,IW(38))
c-----------------------------------------------------------------------------
c    masses
       MC = MP + MR
       XPC = MP/MC
       MH = MC - MU
       MFH = MH/MC
c    mixed density
       RHOC = MC/VC
c    get flame area parameters
       VHS = VH/VC
       IF (KFGM.EQ.3) THEN
c            cylindrical burn approximations for smooth behavior
                   FAHH = VHS
                   RFAP = SQRT(VHS)*THOR
               ELSE
c                flame geometry table
                   CALL FMGCVV(VHS,FAHH,RFAP)
               ENDIF
c    flame area stretched by volume ratio
       AFLA = RFAP*APIS*VC/VCLR
c    heat transfer ratios
       AHTH = FAHH*AHTC
       AHTU = AHTC - AHTH
c    calculate turbulence velociies
       VH2 = 2*KH
       VU2 = 2*KU
       VELH = SQRT(VH2)
       VELU = SQRT(VU2)
c    mass-averaged turbulence velocity
       VELT = SQRT((MU*VU2 + MH*VH2)/MC)
c    mass-averaged internal energy and temperature
        UCS = UC/MC
        XMU = MP/MC
        CALL GTUMVV(XMU,UCS,TC)
c    calculate flame speed
       IF (MU.GT.MUE)  THEN
               VFLA = FVFL*VELU + VLAM
           ELSE
               VFLA = 0
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE BURNVV(DSS0,IERR)
c
c      Calculates pressure and burned and unburned-gas temperatures given the
c      mass, unburned composition, total volume, and internal energy.
c-----------------------------------------------------------------------------
c      Input
c          DSS0    unburned specific entropy increase from last instant
c-----------------------------------------------------------------------------
c      The following quantities must be loaded in RW at call:
c          MU      unburned mass in cylinder, kg
c          VC      cylinder volume, m**3
c          UC      total internal energy in cylinder, J
c          XPU     mass fraction of products in unburned gas
c          TU0     unburned gas temperature at last instant, K
c          PU0     pressure at last instant, Pa
c-----------------------------------------------------------------------------
c      The following quantities are returned in RW:
c          TH      burned gas temperature
c          TU      unburned gas temperature
c          PU      unburned gas pressure  (=PC)
c          CPH     specific heat Cp of burned gas in cylinder, J/kg-K
c          CPU     specific heat Cp of unburned gas in cylinder, J/kg-K
c          GAFH    (gamma - 1)/gamma for burned gas
c          GAFU    (gamma - 1)/gamma for unburned gas
c          GAMH    gamma = Cp/Cv for burned gas
c          GAMU    gamma = Cp/Cv for unburned gas
c          HHS     specific enthalpy of burned gas, J/kg
c          HUS     specific enthalpy of unburned gas, J/kg
c          PC      pressure in cylinder, Pa
c          PVH     PV of burned gas, J/kg
c          PVU     PV of unburned gas, J/kg
c          RHOH    density of burned gas, kg/m**3
c          RHOU    density of unburned gas, kg/m**3
c          TH      temperature of burned gas, K
c          TU      temperature of unburned gas, K
c          VH      burned volume, m**3
c          VU      unburned volume, m**3
c          XPC     fraction of mass in cylinder that is products
c-----------------------------------------------------------------------------
c      Quantities used only in this subroutine incude:
c          VHS     ratio of burned volume to total volume
c-----------------------------------------------------------------------------
       REAL        MH,MU,MC
c-----------------------------------------------------------------------------
       DIMENSION   RW(400),IW(50)
c-----------------------------------------------------------------------------
       COMMON  /RWVV/  RW
       COMMON  /IWVV/  IW
c-----------------------------------------------------------------------------
c    Variable locations in RW:
       EQUIVALENCE (UC  ,RW(1))
       EQUIVALENCE (MU  ,RW(7))
       EQUIVALENCE (TU  ,RW(9))
       EQUIVALENCE (PU  ,RW(10))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TH  ,RW(114))
       EQUIVALENCE (VH  ,RW(133))
       EQUIVALENCE (VU  ,RW(134))
       EQUIVALENCE (MH  ,RW(136))
       EQUIVALENCE (XPU ,RW(138))
       EQUIVALENCE (TU0 ,RW(139))
       EQUIVALENCE (PU0 ,RW(140))
       EQUIVALENCE (CPU ,RW(156))
       EQUIVALENCE (GAMH,RW(161))
       EQUIVALENCE (GAMU,RW(162))
       EQUIVALENCE (HHS ,RW(167))
       EQUIVALENCE (HUS ,RW(168))
       EQUIVALENCE (RHOH,RW(173))
       EQUIVALENCE (RHOU,RW(174))
       EQUIVALENCE (PVH ,RW(176))
       EQUIVALENCE (PVU ,RW(177))
       EQUIVALENCE (GAFH,RW(178))
       EQUIVALENCE (GAFU,RW(179))
c-----------------------------------------------------------------------------
       DATA    ERR,KTRMAX /0.1, 20/
c-----------------------------------------------------------------------------
c    set counter
1      KTR = 0
c
c -- iteration loop point
c
10     KTR = KTR + 1
c
c    calculate gas properties for trial temperatures
       UUS = UMVV(XPU,TU)
       PVU = PVMVV(XPU,TU)
       CVU = CVMVV(XPU,TU)
       IF (KTR.EQ.1)  THEN
c        ideal PC*VC based on last-instant values
           RU = PVU/TU
           PV0 = VC*PU0*EXP(-DSS0/RU)
           ENDIF
       GAMU = (CVU + RU)/CVU
       POWER = GAMU/(GAMU - 1)
       UHS = UPVV(TH)
       PVH = PVPVV(TH)
       CVH = CVPVV(TH)
c    set up simultaneous equations for temperature perturbations
       Y1 = UC - MH*UHS - MU*UUS
       A11 = MH*CVH
       A12 = MU*CVU
       PVX = PV0*(TU/TU0)**POWER
       Y2 = PVX - MH*PVH - MU*PVU
       A21 = MH*PVH/TH
       A22 = MU*PVU/TU - PVX*POWER/TU
       DET = A11*A22 - A21*A12
       IF (DET.EQ.0.)  THEN
           CALL WARNZZ('@','@Calculation failed; '//
     ;                     'burn iteration singularity.@@')
           IERR= 1
           RETURN
           ENDIF
c    calculate temperature perturbations
       X1 = (Y1*A22 - Y2*A12)/DET
       X2 = (A11*Y2 - A21*Y1)/DET
c    check convergence
       IF ((ABS(X1).LT.ERR).AND.(ABS(X2).LT.ERR)) GOTO 50
c    check iteration count
       IF (KTR.GT.KTRMAX)  THEN
c        not converging
           CALL WARNZZ('@','@Calculation failed; '//
     ;          'burn iteration not convergent.@@')
           IERR = 1
           RETURN
           ENDIF
c    limit the changes
       F1 = ABS(X1)/TH
       IF (F1.GT.0.2) THEN
               F1 = 0.2/F1
           ELSE
               F1 = 1
           ENDIF
       F2 = ABS(X2)/TU
       IF (F2.GT.0.2) THEN
               F2 = 0.2/F2
           ELSE
               F2 = 1
           ENDIF
       F = MIN(F1,F2)
c    correct temperatures
       THN = TH + X1*F
       TUN = TU + X2*F
       IF (KTR.GT.KTRMAX/2) THEN
c           iteration oscillation control
               TH = 0.5*(THN +TH)
               TU = 0.5*(TUN +TU)
           ELSE
               TH = THN
               TU = TUN
           ENDIF
c    go try again
       GOTO 10
c
c    converged; calculate the pressure
50     PC = (MH*PVH + MU*PVU)/VC
       PU = PC
c    calculate the volumes
       PVMH = MH*PVH
       PVMU = MU*PVU
       VH = VC*PVMH/(PVMH + PVMU)
       VU = VC - VH
c    other algebraic properties
       GAMH = (CVH + PVH/TH)/CVH
       RHOH = PC/PVH
       RHOU = PC/PVU
       HHS = UHS + PVH
       HUS = UUS + PVH
       CPH = CVH/GAMH
       CPU = CVU/GAMU
       GAFH = (GAMH - 1)/GAMH
       GAFU = (GAMU - 1)/GAMU
c    OK exit
       IERR = 0
       RETURN
       END
c***************************************************************************
