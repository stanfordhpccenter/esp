c******************************************************************************
c
c      Setup routines for stage changes
c
c******************************************************************************
c
       SUBROUTINE SET0VV
c
c      Set up to begin a calculation
c------------------------------------------------------------------------------
c
c       At call the following must be in RW:
c
c          AVER    reference flow area of exhaust valve (full open), m^2
c          AVIR    reference flow area of intake valve (full open), m^2
c          BORE    cylinder bore, m
c          FAHE    (effective heat trans. area for exhaust flow)/(bore area)
c          FAHH    (heat transfer area above piston at TDC)/(bore area)
c          FAHI    (effective heat trans. area for inlet flow)/(bore area)
c          FRAB    fraction of reactants burned (combustion efficiency)
c          PEAT    exhaust manifold presssure, atmospheres
c          PIAT    inlet manifold pressure, atmospheres
c          STRK    piston compression stroke, m
c          VCR     volume compression ratio
c-----------------------------------------------------------------------------
c       At call the following must be in IW:
c          IAEO    IANG at exhaust open (start of gas exchange stage)
c          IAIC    IANG at intake close (end of gas exchange stage)
c          IAIG    crank degrees after compression TDC where ignition occurs
c          IEGR    percent of exhaust recirculated to intake (EGR)
c          IEVF    maximum exhaust valve flow area as percent of reference area
c          IIVF    maximum inlet valve flow area as percent of reference area
c          IRPM    crankshaft revolutions per minute
c          ITCW    temperature of liner/piston/head heat transfer area, K
c          ITER    EGR return temperature, K
c          ITEV    temperature of exhaust valve flow heat transfer area, K
c          ITI     fresh charge intake temperature, K
c          ITIV    temperature of intake valve flow heat transfer area, K
c          KFOP    firing control (1 firing at IANG, 2 motoring)
c          KIMC    inlet manifold control (1 none, 2 included)
c          KEMC    exhaust manifold control (1 none, 2 included)
c-----------------------------------------------------------------------------
c       On return the following quantities are in RW:
c          ANG     crank angle, degrees
c          AHAC    heat transfer area exclusive of stroked cylinder, m^2
c          AHTE    heat transfer area for exhaust valve flow, m^2
c          AHTI    heat transfer area for inlet valve flow, m^2
c          AMEV    maximum flow area of exhaust valve, m^2
c          AMIV    maximum flow area of intake valve, m^2
c          APIS    piston area, m**2
c          CPB     specific heat of backflow gas, J/kg-K
c          CPE     specific heat of exhaust gas, J/kg-K
c          CPM     specific heat of mixed charge, J/kg-K
c          DT1     time step for one crank angle degree, sec
c          DISP    displacement; (piston area)*stroke, m**3
c          FEGR    exhaust gas recirculation fraction
c          GAMB    Cp/Cv for backflow gas
c          GAME    Cp/Cv for exhaust gas
c          GAMM    Cp/Cv for mixed charge
c          HB      enthalpy of intake backflow gas, J
c          HBS     specific enthalpy of backflow gas, J/kg
c          HCLR    clearance height, m
c          HES     specific enthalpy of exhaust gas, J/kg
c          HMS     specific enthalpy of mixed charge, J/kg
c          KT      turbulence kinetic energy, J/kg
c          MC      total gas mass in cylinder, kg
c          MDIA    mass displaced at inlet ambient density, kg
c          MPB     mass of products in intake backflow gas, kg
c          MP      mass of products in cylinder, kg
c          MRB     mass of reactants in intake backflow gas, kg
c          MR      mass of reactants in cylinder, kg
c          P0EV    stagnation pressure at exhaust valve after EGR, Pa
c          P0IV    stagnation pressure at intake valve before EGR, Pa
c          PC      pressure in cylinder, Pa
c          PE      exhaust ambient pressure, Pa
c          PI      inlet ambient pressure, Pa
c          RHOB    density of backflow gas, kg/m**3
c          RHOE    density of exhaust gas, kg/m**3
c          RHOM    density of mixed charge, kg/m**3
c          T0EV    stagnation temperature at exhaust valve after EGR, K
c          T0IV    stagnation temperature at intake valve before EGR, K
c          TB      backflow gas temperature, K
c          TC      temperature of cylinder gas (unburned during burn), K
c          TE      dummy value of exhaust temperature, K
c          TEV     temperature of exhaust valve heat transfer area, K
c          TEGR    exhaust gas recirculation temperature, K
c          THOR    2*clearance height/bore radius
c          TI      inlet fresh charge temperature, K
c          TIV     temperature of inlet valve heat transfer area, K
c          TM      mixed charge temperature, K
c          TRRI    T0egr/T0runner at intake valve before EGR
c          TW      wall temperature, K
c          UC      total internal energy of gas in cylinder, J
c          VC      cylinder volume, m**3
c          VCLR    clearance volume, m**3
c          XPB     fraction of backflow gas that is products
c          XPC     fraction of cylinder gas that is products
c          XPE     fraction of exhaust gas that is products
c          XPM     fraction of mixed charge gas that is products
c          ZCHT    bore*pi*stroke, m**2
c          ZDXP    stroke*RPM*2*pi/60, rad-m/s
c-----------------------------------------------------------------------------
c       On return the following quantities are in IW:
c          IANG    current crank angle, degrees (0 - 720)
c          IANR    angle from compression start, degrees
c          IEOR    angle from compression start to gas exchange, degrees
c          IIGR    angle from compression start to ignition, degrees
c          LSTG    stage indicator
c-----------------------------------------------------------------------------
       REAL        KT,MC,MDIA,MP,MPB,MR,MRB
c-----------------------------------------------------------------------------
       DIMENSION   IW(50),RW(400)
c-----------------------------------------------------------------------------
       COMMON  /IWVV/ IW
       COMMON  /RWVV/ RW
c-----------------------------------------------------------------------------
c    Locations in the cylinder real work array
       EQUIVALENCE (UC  ,RW(1))
       EQUIVALENCE (MP  ,RW(2))
       EQUIVALENCE (MR  ,RW(3))
       EQUIVALENCE (KT  ,RW(4))
       EQUIVALENCE (MU  ,RW(7))
       EQUIVALENCE (KU  ,RW(8))
       EQUIVALENCE (TU  ,RW(9))
       EQUIVALENCE (PU  ,RW(10))
       EQUIVALENCE (HB  ,RW(13))
       EQUIVALENCE (MPB ,RW(14))
       EQUIVALENCE (MRB ,RW(15))
       EQUIVALENCE (ANG ,RW(101))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (VELT,RW(106))
       EQUIVALENCE (XPC ,RW(107))
       EQUIVALENCE (FIN ,RW(110))
       EQUIVALENCE (FEX ,RW(111))
       EQUIVALENCE (TB  ,RW(128))
       EQUIVALENCE (XPB ,RW(127))
       EQUIVALENCE (TE  ,RW(129))
       EQUIVALENCE (XPE ,RW(130))
       EQUIVALENCE (VELI,RW(145))
       EQUIVALENCE (VELE,RW(146))
       EQUIVALENCE (P0IV,RW(147))
       EQUIVALENCE (T0IV,RW(148))
       EQUIVALENCE (P0EV,RW(149))
       EQUIVALENCE (T0EV,RW(150))
       EQUIVALENCE (CPM ,RW(152))
       EQUIVALENCE (CPB ,RW(153))
       EQUIVALENCE (CPE ,RW(154))
       EQUIVALENCE (GAMM,RW(158))
       EQUIVALENCE (GAMB,RW(159))
       EQUIVALENCE (GAME,RW(160))
       EQUIVALENCE (HMS ,RW(164))
       EQUIVALENCE (HBS ,RW(165))
       EQUIVALENCE (HES ,RW(166))
       EQUIVALENCE (RHOC,RW(169))
       EQUIVALENCE (RHOM,RW(170))
       EQUIVALENCE (RHOB,RW(171))
       EQUIVALENCE (RHOE,RW(172))
       EQUIVALENCE (YE  ,RW(180))
       EQUIVALENCE (APIS,RW(181))
       EQUIVALENCE (AHTE,RW(182))
       EQUIVALENCE (AHTI,RW(183))
       EQUIVALENCE (AMEV,RW(184))
       EQUIVALENCE (AMIV,RW(185))
       EQUIVALENCE (AHAC,RW(186))
       EQUIVALENCE (THOR,RW(189))
       EQUIVALENCE (VCLR,RW(190))
       EQUIVALENCE (ZCHT,RW(191))
       EQUIVALENCE (ZDXP,RW(192))
       EQUIVALENCE (DISP,RW(193))
       EQUIVALENCE (DT1 ,RW(199))
       EQUIVALENCE (PIAT,RW(202))
       EQUIVALENCE (PEAT,RW(203))
       EQUIVALENCE (PE  ,RW(204))
       EQUIVALENCE (PI  ,RW(205))
       EQUIVALENCE (TI  ,RW(206))
       EQUIVALENCE (TEGR,RW(207))
       EQUIVALENCE (FEGR,RW(208))
       EQUIVALENCE (TM  ,RW(209))
       EQUIVALENCE (MDIA,RW(210))
       EQUIVALENCE (BORE,RW(211))
       EQUIVALENCE (VCR ,RW(212))
       EQUIVALENCE (STRK,RW(213))
       EQUIVALENCE (AVIR,RW(218))
       EQUIVALENCE (AVER,RW(219))
       EQUIVALENCE (FRAB,RW(234))
       EQUIVALENCE (XPM ,RW(240))
       EQUIVALENCE (FAHH,RW(258))
       EQUIVALENCE (FAHI,RW(259))
       EQUIVALENCE (FAHE,RW(260))
       EQUIVALENCE (TEV ,RW(268))
       EQUIVALENCE (TIV ,RW(269))
       EQUIVALENCE (TW  ,RW(270))
       EQUIVALENCE (TRRI,RW(329))
       EQUIVALENCE (YI  ,RW(349))
       EQUIVALENCE (CI  ,RW(350))
       EQUIVALENCE (RHOI,RW(351))
c-----------------------------------------------------------------------------
c    Locations in the cylinder integer work array
       EQUIVALENCE (IANG,IW(1))
       EQUIVALENCE (IAIC,IW(3))
       EQUIVALENCE (IAEO,IW(4))
       EQUIVALENCE (KFOP,IW(6))
       EQUIVALENCE (LSTG,IW(7))
       EQUIVALENCE (IRPM,IW(10))
       EQUIVALENCE (IAIG,IW(11))
       EQUIVALENCE (ITI ,IW(12))
       EQUIVALENCE (IEGR,IW(13))
       EQUIVALENCE (ITER,IW(14))
       EQUIVALENCE (IIVF,IW(15))
       EQUIVALENCE (IEVF,IW(16))
       EQUIVALENCE (ITCW,IW(21))
       EQUIVALENCE (ITIV,IW(22))
       EQUIVALENCE (ITEV,IW(23))
       EQUIVALENCE (IANR,IW(30))
       EQUIVALENCE (IIGR,IW(31))
       EQUIVALENCE (IEOR,IW(32))
       EQUIVALENCE (IUIP,IW(33))
       EQUIVALENCE (IUEP,IW(34))
       EQUIVALENCE (KIMC,IW(39))
       EQUIVALENCE (KEMC,IW(40))
       EQUIVALENCE (NCYC,IW(46))
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
c    Locations in the manifold work arrays
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoR   ,MPTR(80))
       EQUIVALENCE (IoNCYL,MPTR(88))
       EQUIVALENCE (IoIBEG,MPTR(89))
       EQUIVALENCE (IoI   ,MPTR(91))
c------------------------------------------------------------------------------
c    phase-averaged variables and weightings
       DIMENSION   PCAV(720),RHOCAV(720)
       COMMON      /AVCDVV/PCAV,RHOCAV,FCAVO,FCAVN,NCAV
c------------------------------------------------------------------------------
c    Manifold time step
       COMMON /DTXX/ DT
c------------------------------------------------------------------------------
       DATA  PICON /3.1412859E0/
c-----------------------------------------------------------------------------
c -- conversions
c
c    set REAL values
       TEGR = ITER
       TI = ITI
       TIV = ITIV
       TEV = ITEV
       TW = ITCW
       FEGR = 0.01*IEGR
c
c    convert pressures
       PI = 101325.*PIAT
       PE = 101325.*PEAT
c
c  -  set reference angles
c
       IANR = 0
c    ignition reference to cycle analysis start
       IF (KFOP.EQ.1)  THEN
           IF (IAIG.GT.IAIC) THEN
c                ignition at higher crank angle before TDC compression
                   IIGR = IAIG - IAIC
               ELSE
c                ignition at lower crank angle after TDC compression
                   IIGR = IAIG - IAIC + 720
               ENDIF
           ENDIF
c
c    gas exchange reference to cycle analysis start
       IF (IAEO.GT.IAIC) THEN
c            exhaust opens at higher crank angle before TDC compression
               IEOR = IAEO - IAIC
           ELSE
c            exhaust opens at lower crank angle after DC compression
               IEOR = IAEO - IAIC + 720
           ENDIF
c
c -- compute fixed geometrical quantities
c
c    piston bore area
       APIS = PICON*BORE*BORE/4
c    compression displacement
       DISP = APIS*STRK
c    clearance volume
       VCLR = DISP/(VCR - 1)
c    clearance parameter for cylindrical burn 2h/r
       THOR = 4*VCLR/(APIS*BORE)
c    valve heat transfer areas
       AHTE = FAHE*APIS
       AHTI = FAHI*APIS
c    maximum valve openings
       AMEV = AVER*0.01*IEVF
       AMIV = AVIR*0.01*IIVF
c    heat transfer area exclusive of stroked cylinder
       AHAC = (FAHH + 1)*APIS
c    multiplier coefficients
       ZCHT = PICON*BORE*STRK
       ZDXP = STRK*IRPM*PICON/30.
c    displacement mass
       RHOI = PI/PVRVV(TI)
       MDIA = RHOI*DISP
c
c    time step
       RPM = IRPM
       DT1 = 1.0/(RPM*6)
c
c -- calculate inlet mixture composition
c
c    get mixed charge products content (XPE = FRAB)
       TERM = FEGR/(1 - FEGR)
       XPM = FRAB*TERM/(TERM + 1)
c    mix EGR gas and fresh charge to determine mixed charge enthalpy
       HMS = (HRVV(TI) + TERM*HMVV(FRAB,TEGR))/(1 + TERM)
c    get mixed charge temperature and properties
       TM = TI
       CALL GTHMVV(XPM,HMS,TM)
       CPM = CPMVV(XPM,TM)
       GAMM = CPM/CVMVV(XPM,TM)
       RHOM = PI/PVMVV(XPM,TM)
c
c    initialize with no backflow
       HB = 0
       MPB = 0
       MRB = 0
c
c    initialize exhaust properties
       TE = TM
       IF (KFOP.EQ.1)  THEN
c            burning
               XPE = FRAB
           ELSE
c            motoring
               XPE = 0
           ENDIF
       HES = HMVV(XPE,TE)
       RHOE = PE/PVMVV(XPE,TE)
       CPE = CPMVV(XPE,TE)
       GAME = CPE/CVMVV(XPE,TE)
c
c -- set manifold data
c
c    set manifold pointers
       CALL SPTRXX
c    set manifold time step
       DT = DT1
c
c -- set conditions at start of compression
c
c    set angles
       IANG = IAIC
       ANG = IANG
c    initial manifold angles
       IANGP = IANG + 1
       IWI(IoIBEG) = IANGP
       IWE(IoIBEG) = IANGP
       IWI(IoI) = IANGP
       IWE(IoI) = IANGP
c
c    initialize intake manifold
       IF (KIMC.EQ.2)  THEN
           CALL SIMDXX(RWI,IWI,1,TM,PI)
           YI = PI/RHOI**RWI(IoK)
           CI = CPYXX(RWI,PI,YI)
           IF (FEGR.GT.0)  THEN
                   TRRI = TEGR/TM
               ELSE
                   TRRI = 1
               ENDIF
           ENDIF
c
c    initialize exhaust manifold
       IF (KEMC.EQ.2)  THEN
           TE = TM
           CALL SIMDXX(RWE,IWE,2,TE,PE)
           RHOE = PE/(RWE(IoR)*TE)
           YE = PE/RHOE**RWE(IoK)
           ENDIF
c
c    set phase averages
       DO I=1,720
           PCAV(I) = PI
           RHOCAV(I) = RHOI
           ENDDO
c
c    get initial angle geometry
       CALL GEOMVV
c
c    set initial masses
       MC = VC*RHOM
       MP  = MC*XPM
       MR  = MC*(1 - XPM)
c    set initial internal energy
       UC = UMVV(XPM,TM)*MC
c    set initial kinetic energy proportional to piston speed
       KT = 0.1*(2*STRK*IRPM/30.)**2

           if (kt.lt.0) write (*,*) 'c9: 1',kt

       VELT = SQRT(2*KT)
c
c    set TC needed for trial in ALGCVV called by SETCVV
       TC = TM
c
c    get algebraic variables and set for compression stage
       CALL SETCVV
c
c * all variables needed for compression calculation now known
c
c    set cycle count
       NCAV = 0
       NCYC = 0
c
c    set valve lookup indices
       IUIP = 1
       IUEP = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  SETBVV(IERR)
c
c      Set up for burn stage
c-----------------------------------------------------------------------------
c      Arguments:
c          IERR    return error flag
c                0 setup ok
c                1 ignition iteration not convergent
c                2 ignition calculation singular (program error)
c-----------------------------------------------------------------------------
c    The following quantities must be loaded in RW at call:
c          CPC     specific heat Cp of unburned gas in cylinder, J/kg-K
c          FMIG    fraction of mass ignited
c          FRAB    fraction of reactants burned (combustion efficiency)
c          MC      mass in cylinder, kg
c          MRC     reactant mass in cylinder, kg
c          PC      pressure in cylinder, Pa
c          RHOC    density of unburned gas, kg/m**3
c          TC      temperature of unburned gas, K
c          VC      cylinder volume, m**3
c          UC      total internal energy of burned gas, J
c          XPC     mass fraction of products in cylinder
c-----------------------------------------------------------------------------
c    The following quantities are returned in RW:
c          AFLA    projected flame area, m**2
c          AHTH    heat transfer area for burned gas in cylinder, m**2
c          AHTU    heat transfer area for unburned gas in cylinder, m**2
c          CPC     specific heat Cp of unburned gas in cylinder, J/kg
c          CPH     specific heat Cp of burned gas in cylinder, J/kg
c          GAFH    (gamma - 1)/gamma for burned gas
c          GAFU    (gamma - 1)/gamma for unburned gas  (gamma = Cp/Cv)
c          HH      enthalpy of burned has, J
c          HHS     specific enthalpy of burned gas, J/kg
c          HU      enthalpy of unburned has, J
c          HUS     specific enthalpy of unburned gas, J/kg
c          KT      turbulence kinetic energy of burned gas, J/kg
c          KU      turbulence kinetic energy of unburned gas, J/kg
c          MH      burned mass, kg
c          MU      unburned mass, kg
c          MUE     residual unburned mass at end of combustion, kg
c          PC      pressure in cylinder, Pa
c          PVH     PV of burned gas, J/kg
c          PVU     PV of unburned gas, J/kg
c          RHOH    density of burned gas, kg/m**3
c          RHOU    density of unburned gas, kg/m**3
c          TC      temperature of unburned gas, K
c          TH      temperature of burned gas, K
c          VH      burned volume, m**3
c          VU      unburned volume, m**3
c          VFLA    flame speed, m/s
c          XPU     mass fraction of products in the unburned gas
c-----------------------------------------------------------------------------
c    The following quantities are returned in IW:
c          LSTG    stage indicator
c-----------------------------------------------------------------------------
c      The following are used only in this subroutine:
c          KTR     iteration counter
c          KTRMAX  iteration limit
c          TAFV    adiabatic flame temperature at constant volume, K
c          TAFP    adiabatic flame temperature at constant pressure, K
c          PI      initial pressure, Pa
c          TI      initial temperature, K
c          CVH     specific heat Cv of burned gas, J/kg
c          CVU     specific heat Cv of unburned gas, J/kg
c          UHS     specific internal energy of burned gas, J/kg
c          UUS     specific internal energy of unburned gas, J/kg
c          PVU     Pv of unburned gas, J/kg
c-----------------------------------------------------------------------------
       REAL        KT,KU,MC,MH,MP,MR,MU,MUE
c-----------------------------------------------------------------------------
       DIMENSION   IW(50),RW(400)
c-----------------------------------------------------------------------------
       COMMON  /IWVV/  IW
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
c    Locations in the work arrays:
       EQUIVALENCE (MP  ,RW(2))
       EQUIVALENCE (MR  ,RW(3))
       EQUIVALENCE (KT  ,RW(4))
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
       EQUIVALENCE (VELH,RW(116))
       EQUIVALENCE (VELU,RW(117))
       EQUIVALENCE (TH  ,RW(114))
       EQUIVALENCE (MUE ,RW(135))
       EQUIVALENCE (MH  ,RW(136))
       EQUIVALENCE (XPU ,RW(138))
       EQUIVALENCE (TU0 ,RW(139))
       EQUIVALENCE (PU0 ,RW(140))
       EQUIVALENCE (FMIG,RW(231))
       EQUIVALENCE (FRAB,RW(234))
       EQUIVALENCE (LSTG,IW(7))
c-----------------------------------------------------------------------------
c    ignition iteration controls
       DATA    ERR,KTRMAX/0.1,40/
c-----------------------------------------------------------------------------
c    set stage
       LSTG = 2
c    determine residual unburned products at end of burn
       IF (FMIG.GT.1) THEN
               MUE = 0
           ELSEIF (FMIG.LE.0) THEN
c            no ignition, no burn
               MUE = MR
           ELSE
               MUE = (1 - FRAB)*MR
               MUE = MAX(MUE,0.)
           ENDIF
c    pre-ignition values
       XPU = XPC
       KU = KT
       KH = KT
       VELH = VELT
       VELU = VELT
       PU0 = PC
       TU0 = TC
c    post-ignition values
       MH = FMIG*MR/(1 - XPC)
       MU = MC - MH
       MU = MAX(MU,0.)
       MH = MC - MU
       MP = MH + XPC*MU
       MR = MC - MP
       MR = MAX(MR,0.)
       MP = MC - MR
       IF (MH.GT.0)  THEN
c            trial value
               TH = 2200
           ELSE
c            no ignition
               TH = TC
           ENDIF
       TU = TC
c    calculate burned and unburned states for instant ignition
       CALL BURNVV(0.,IERR)
       IF (IERR.NE.0)  RETURN
c    get algebraic variables
       CALL ALGBVV
c    OK exit
       IERR = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  SETEVV
c
c      Set up for expansion stage
c-----------------------------------------------------------------------------
c    The following quantities must be loaded in RW at call:
c          UC      total internal energy of cylinder gas, J
c          MP      mass of products in cylinder, kg
c          MR      mass of reactants in cylinder, kg
c          KT      turbulence kinetic energy of unburned gas, J/kg
c          MU      mass of unburned gas, kg
c          KU      turbulence kinetic energy of burned gas, J/kg
c          TC      temperature of unburned gas, K
c          TH      temperature of burned gas, K
c          XPU     fraction of unburned mass that is products
c-----------------------------------------------------------------------------
c    The following quantities are returned in RW:
c          KT      turbulence kinetic energy of mixed gas, J/kg
c-----------------------------------------------------------------------------
c    The following quantities are returned in IW:
c          LSTG    stage indicator
c-----------------------------------------------------------------------------
       REAL        KT,KU,MC,MH,MU
       DIMENSION   IW(50),RW(400)
       COMMON  /IWVV/ IW
       COMMON  /RWVV/ RW
c-----------------------------------------------------------------------------
c    Locations in the arrays:
       EQUIVALENCE (KT  ,RW(4))
       EQUIVALENCE (MU  ,RW(7))
       EQUIVALENCE (KU  ,RW(8))
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (VELT,RW(106))
       EQUIVALENCE (VFLH,RW(116))
       EQUIVALENCE (VFLU,RW(117))
       EQUIVALENCE (VFLA,RW(118))
       EQUIVALENCE (DMHD,RW(120))
       EQUIVALENCE (MH  ,RW(136))
       EQUIVALENCE (LSTG,IW(7))
c-----------------------------------------------------------------------------
c    set stage
       LSTG = 3
c    average the kinetic energy
       KT = (MU*KT + MH*KU)/MC

       if (kt.lt.0) write (*,*)' c9:2', kt

       VELT = SQRT(2*KT)
c    equalize
       VELU = VELT
       VELH = VELT
c    terminate burn
       VFLA = 0
       DMHD = 0
       MH = 0
       MU = 0
c    get algebraic variables
       CALL ALGEVV
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  SETGVV
c
c      Set up for gas exchange stage
c-----------------------------------------------------------------------------
c    The following quantities are returned in IW:
c          LSTG    stage indicator
c-----------------------------------------------------------------------------
       DIMENSION   IW(50),RW(400)
       COMMON  /IWVV/  IW
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
c    Locations in the arrays:
       EQUIVALENCE (LSTG,IW(7))
       EQUIVALENCE (LEFL,IW(8))
       EQUIVALENCE (NCYC,IW(46))
c-----------------------------------------------------------------------------
c    set stage
       LSTG = 4
c    set exhaust open
       LEFL = - 1
c    get algebraic variables
       CALL ALGGVV
c    on first cycle reinitalize exhaust manifold
       IF (NCYC.EQ.0)  CALL IEXMVV
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  SETCVV
c
c      Set up for compression stage
c-----------------------------------------------------------------------------
c    The following quantities are returned in IW:
c          LSTG    stage indicator
c-----------------------------------------------------------------------------
       DIMENSION   IW(50),RW(400)
       COMMON  /IWVV/  IW
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
       EQUIVALENCE (FIN ,RW(110))
       EQUIVALENCE (FEX ,RW(111))
       EQUIVALENCE (VI10,RW(143))
       EQUIVALENCE (VE10,RW(144))
       EQUIVALENCE (VELI,RW(145))
       EQUIVALENCE (VELE,RW(146))
       EQUIVALENCE (LSTG,IW(7))
       EQUIVALENCE (LEFL,IW(8))
       EQUIVALENCE (LIFL,IW(9))
c-----------------------------------------------------------------------------
c    set stage
       LSTG = 1
c    set valves closed
       LEFL = 0
       LIFL = 0
c    get algebraic variables
       CALL ALGCVV
c    set no flows or velocitis
       FIN = 0
       FEX = 0
       VELI = 0
       VELE = 0
       VI10 = 0
       VE10 = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE IEXMVV
c
c      Estimates exhaust temperature and reinitializes exhaust manifold.
c      Call on first opening of exhaust valve.
c-----------------------------------------------------------------------------
       REAL    MA,MB,MC,MX
c-----------------------------------------------------------------------------
       DIMENSION   IW(50),RW(400)
c-----------------------------------------------------------------------------
       COMMON  /IWVV/ IW
       COMMON  /RWVV/ RW
c-----------------------------------------------------------------------------
c    Locations in the cylinder real work array
       EQUIVALENCE (MC  ,RW(102))
       EQUIVALENCE (PC  ,RW(103))
       EQUIVALENCE (TC  ,RW(105))
       EQUIVALENCE (TE  ,RW(129))
       EQUIVALENCE (GAMC,RW(157))
       EQUIVALENCE (RHOE,RW(172))
       EQUIVALENCE (YE  ,RW(180))
       EQUIVALENCE (PE  ,RW(204))
       EQUIVALENCE (VCR ,RW(212))
c-----------------------------------------------------------------------------
c    Locations in the cylinder integer work array
       EQUIVALENCE (KEMC,IW(40))
c-----------------------------------------------------------------------------
c    manifold work arrays
       PARAMETER   (NMPTR = 96)
       PARAMETER   (NDRW = 21024)
       PARAMETER   (NDIW = 2904)
       DIMENSION   MPTR(NMPTR)
       DIMENSION   RWE(NDRW)
       DIMENSION   IWE(NDIW)
       COMMON /RWEXX/RWE
       COMMON /IWEXX/IWE
       COMMON /MPTRXX/MPTR
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoR   ,MPTR(80))
       EQUIVALENCE (IoNCYE,MPTR(86))
c------------------------------------------------------------------------------
       IF (KEMC.EQ.2)  THEN
c        estimated temperature in cylinder after blowdown
           TA = TC*(PE/PC)**((GAMC-1)/GAMC)
c        mass in cylinder after blowdown
           MA = MC*PE*TA/(PC*TC)
c        blowdown mass
           MB = MC - MA
c        mass expelled at TA,PE
           MX = MA/VCR
c        estimated mixed exhaust temperature
           TE = ( (MC*TC - MA*TA)/GAMC + MX*TA )/(MB + MX)
c        reinitialize the manifold
           CALL SIMDXX(RWE,IWE,2,TE,PE)
c        override reset cycle counter
           IWE(IoNCYE) = 1
c        reset RHOE and YE
           RHOE = PE/(RWE(IoR)*TE)
           YE = PE/RHOE**RWE(IoK)
           ENDIF
       RETURN
       END
c******************************************************************************

