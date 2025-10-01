c*****************************************************************************
c
c      Manifold pointer documentation and setup routines
c
c******************************************************************************
c
       SUBROUTINE  SPTRXX
c
c      Sets pointers to variables in the manifold work arrays
c============================================================================
c      Explanation of pointers
c
c      In order to enable the reference to a large block of data with
c      a single subroutine argument, work arrays are used. Each manifold
c      has a real work array RW and an integer work array IW.
c
c      Quantities in the work arrays are indexed by pointers in the
c      common block /MPTRXX/. Pointers have names IoWXYX, where Io stands
c      for "index of" and WXYX is the four-character (max) variable name.
c      Thus, WXYZ is referred to as RW(IoWXYZ). This makes the code easy
c      to read.
c
c      Scalar pointers are direct. The pointer for an array points to the
c      position just before the first array element. Then the Ith element
c      of a vector WXYZ is referred to as RW(IoWXYZ+I). The (I,J) element
c      of an KxN matrix WXYZ is referred to as RW(IoWXYZ+I+K*J).  Hence,
c      for a scalar the pointer is to its location in the array.  For a
c      vector the pointer is to one place before the first element of the
c      vector. For an KxN matrix the pointer is to a place K+1 before
c      the first element.
c
c      The intake and exhaust manifolds have the same number of pointers,
c      so those in MPTRXX apply either.
c
c      The intake manfold is MANifold 1
c          Point 1: start of feeder downstream of ambient plenum
c          Point 2: discharge of feeder into junction
c          Point 3: entrance to runner from junction
c          Point 4: discharge from runner to cylinder (before EGR)
c
c      The intake exhaust manifold is MANifold 2
c          Point 1: intake from cylinder (after EGR)
c          Point 2: discharge of runner into junction
c          Point 3: entrance to collector from junctiom
c          Point 4: discharge from collector to ambient
c============================================================================
c      The layout of the data in the work arrays is described below.
c
c  ** Real work array RW  (RWI or RWE)
c    location  symbol          quantity
c------------------------------------------------------------------------------
c  k-dependent constants
c      1       K               k, specific heat ratio (1.4 for air)
c      2       KM1             k - 1
c      3       KP1             k + 1
c      4       KMKP            (k - 1)/(k + 1)
c      5       KPKM            (k + 1)/(k - 1)
c      6       KM12            (k - 1)/2
c      7       KM3M            (k - 1)/(3 - k)
c      8       KKM1            k/(k - 1)
c      9       KM1K            (k - 1)/k
c     10       KPO2            (k+1)/2k
c     11       RK              1/k
c     12       R2K             1/2k
c     13       RKM1            1/(k - 1)
c     14       RKKM            k^(-1/(k-1))
c     15       TK              2k
c     16       TRKM            2/(k - 1)
c     17       TRKP            2/(k + 1)
c     18       TOKS            [2/(k - 1)]^2
c     19       TKOM            2*k/(k-1)
c     20       FRKM            4/(k - 1)
c     21       SKMS            16/(k - 1)^2
c     22       HKPM            (k+1)/(2(k-1))
c     23       HKMK            (k-1)/(2k)
c     24       C0CS            sqrt{(k + 1)/2}
c     25       XMAX            sqrt{2/(k + 1)}[(k + 1)/(k - 1)]
c     26       XMIN            sqrt{2/(k + 1)}[(3 - k)/(k - 1)]
c     27       PRPC            1/[(k + 1)/2]^{(k/(k-1)} = P_*/P_0 (s)
c     28       CRCC            1/sqrt((k+1)/2) =  C*/C0
c     29       RCHM            1/( (k+1)/2)**((k+1)/2(k-1)) )
c     30       SQTK            sqrt(k)
c     31       PHMX            1/((k+1)/(k-1))**((k+1)/(k-1))
c     32       SCKF            1/(k(k-1)) (for entropy friction correction)
c     33       TKOP            2k/(k+1)
c     34       K2M1            k^2-1
c     35       HKP1            (k+1)/2
c     36       SFMC            sqrt{2/(3-k)}
c     37       CMD(1)          coefficient in expansion for valve outflow MD
c     38       CMD(2)           "
c     39       CMD(3)           "
c     40       CMD(4)           "
c     41       CMD(5)           "
c     42       CMD(6)           "
c------------------------------------------------------------------------------
c     43-49    reserved
c------------------------------------------------------------------------------
c  constants involving the manifold blockage at N=1,4  (4 not used)
c     50       reserved
c     51-54    MBF(N)           blockage fraction
c     55-58    AMR(N)           restriction flow area
c------------------------------------------------------------------------------
c   averaging weights for the acoustic time delays
c     59       FTDO             delay-time averaging weight for old D cycles
c     60       FTDN             delay-time averaging weight for new D cycles
c     61       FTEO             delay-time averaging weight for old E cycles
c     62       FTEN             delay-time averaging weight for new E cycles
c------------------------------------------------------------------------------
c   63-70      reserved
c------------------------------------------------------------------------------
c  array data at angle I at point N, i=1,720   N=1,4
c    71-2950    C(I,N)         sound speed
c  2951-5830    V(I,N)         velocity in the nominal direction
c  5831-8710    Y(I,N)         entropy parameter P/rho^k
c  8711-11590   P(I,N)         pressure
c 11591-14470   W(I,N)         mass/area that has passed point J since t=0
c 14471-17350   Z(I,N)         phase-averaged arriving acoustic parameter
c------------------------------------------------------------------------------
c  array data at angle I=1,720
c 17351-18070   P0J(I)         junction stagnation pressure
c 18071-18790   YJ(I)          junction stagnation entropy parameter P/rho^k
c------------------------------------------------------------------------------
c  array data at angle I=1,720
c 18791-19510    YPAV(I)       phase-averaged Y, used in junction analysis
c 19511-20230    ZPAC(I)       phase-averaged Z, used in junction analysis
c 20231-20950    reserved
c------------------------------------------------------------------------------
c  data for Z correction at point N
c 20951-20954    DZF(N)        friction correction to Z
c------------------------------------------------------------------------------
c  data for acoustic characteristic arriving at point N=1,4
c 20955-20958    CL(N)         speed of sound at launch
c 20959-20962    VL(N)         velocity at launch
c 20963-20966    YL(N)         entropy measure at launch
c------------------------------------------------------------------------------
c  data for point N=1,4 at the current time
c 20967-20970    G(N)          mass velocity rho*V
c 20971-20974    WCYC(N)       mass/area past point N for last 720 degrees
c 20975-20978    F(N)          total mass flow past N for current cycle (~/DT)
c 20979-20982    RSUM(N)       accumulating cycle density sum
c 20983-20986    DZC(N)        Z correction for mass unbalance
c------------------------------------------------------------------------------
c  data for for duct J=1,2   variables at the current time
c 20987-20988    LD(J)         duct length
c 20989-20990    AD(J)         flow area
c 20991-20992    TDP(J)        time delay for Z+ characteristics
c 20993-20994    TDM(J)        time delay for Z- characteristics
c 20995-20996    TDPA(J)       cycle average TDP at start of cycle
c 20997-20998    TDMA(J)       cycle average TDM at start of cycle
c 20999-21000    DWC(J)        W correction for unbalanced flows
c------------------------------------------------------------------------------
c  constants involving the friction factor and k in duct J=1,2
c 21001-21002    CF(J)         friction factor
c 21003-21004    CFCB(J)       convection backflow coefficient
c 21005-21006    CFCT(J)       convection throughtflow coefficient
c 21007-21008    CFA(J)        acoustic chracteristic friction coefficient
c------------------------------------------------------------------------------
c  other scalars
c    21009       FAPO          weight to old data in phase averages
c    21010       FAPN          weight to new data in phase averages
c    21011       DZCO          Z  correction for non-cumputed runners
c    21012       FOUT          total junct. flow to other cyls. current cycle
c    21013       EMB           manifold mass unbalance, current cycle
c    21014       R             gas constant (SI units)
c------------------------------------------------------------------------------
c 21015-21024   reserved
c==============================================================================
c  ** Integer work array IW  (IWI or IWE)
c    location  symbol          quantity
c------------------------------------------------------------------------------
c       1-2880   JUMP(I,N)     jump at angle I=1,720 for point N=1,4
c    2881-2884   LAG(N)        lag at point N=1,4
c    2885-2888                 reserved
c         2889   JTAU          degrees per cylinder cycle (720/Ncyl)
c         2890   JCAD          degrees into current feeder/collector D cycle
c         2891   NCYE          engine cycle number
c         2892   NCYD          feeder/collector duct cycle number
c         2893   NCYL          number of cylinders on the manifold
c         2894   IBEG          index at start of cycle calculation
c         2895   MAN           manfold type; 1=intake, 2=exhaust
c         2896   I             current angle index (1-720) for Y and Z
c    2897-2900   IV(N)         index of min Win point
c    2901-2904   IP(N)         index of max Win point
c============================================================================
c   code for memory reservations and pointer locations
c
       PARAMETER           (NMPTR = 96)
c------------------------------------------------------------------------------
       DIMENSION           MPTR(NMPTR)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
c    Pointers for the work arrays
       EQUIVALENCE (IoCMD2,MPTR(37))
C      EQUIVALENCE (,MPTR(38))
       EQUIVALENCE (IoMBF ,MPTR(39))
       EQUIVALENCE (IoAMR ,MPTR(40))
       EQUIVALENCE (IoFTDO,MPTR(41))
       EQUIVALENCE (IoFTDN,MPTR(42))
C      EQUIVALENCE (IoZPAV,MPTR(43))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoY   ,MPTR(46))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoW   ,MPTR(48))
       EQUIVALENCE (IoZ   ,MPTR(49))
       EQUIVALENCE (IoP0J ,MPTR(50))
       EQUIVALENCE (IoYJ  ,MPTR(51))
       EQUIVALENCE (IoFI  ,MPTR(52))
       EQUIVALENCE (IoQI  ,MPTR(53))
       EQUIVALENCE (IoFO  ,MPTR(54))
       EQUIVALENCE (IoDZF ,MPTR(55))
       EQUIVALENCE (IoCL  ,MPTR(56))
       EQUIVALENCE (IoVL  ,MPTR(57))
       EQUIVALENCE (IoYL  ,MPTR(58))
       EQUIVALENCE (IoG   ,MPTR(59))
       EQUIVALENCE (IoWCYC,MPTR(60))
       EQUIVALENCE (IoF   ,MPTR(61))
       EQUIVALENCE (IoRSUM,MPTR(62))
       EQUIVALENCE (IoDZC ,MPTR(63))
       EQUIVALENCE (IoLD  ,MPTR(64))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoTDP ,MPTR(66))
       EQUIVALENCE (IoTDM ,MPTR(67))
       EQUIVALENCE (IoTDPA,MPTR(68))
       EQUIVALENCE (IoTDMA,MPTR(69))
       EQUIVALENCE (IoDWC ,MPTR(70))
       EQUIVALENCE (IoCF  ,MPTR(71))
       EQUIVALENCE (IoCFCB,MPTR(72))
       EQUIVALENCE (IoCFCT,MPTR(73))
       EQUIVALENCE (IoCFA ,MPTR(74))
       EQUIVALENCE (IoFTEO,MPTR(75))
       EQUIVALENCE (IoFTEN,MPTR(76))
       EQUIVALENCE (IoYPAV,MPTR(77))
       EQUIVALENCE (IoZPAV,MPTR(78))
       EQUIVALENCE (IoEMB ,MPTR(79))
       EQUIVALENCE (IoR   ,MPTR(80))
       EQUIVALENCE (IoJUMP,MPTR(81))
       EQUIVALENCE (IoLAG ,MPTR(82))
       EQUIVALENCE (IoFOUT,MPTR(83))
       EQUIVALENCE (IoJTAU,MPTR(84))
       EQUIVALENCE (IoJCAD,MPTR(85))
       EQUIVALENCE (IoNCYE,MPTR(86))
       EQUIVALENCE (IoNCYD,MPTR(87))
       EQUIVALENCE (IoNCYL,MPTR(88))
       EQUIVALENCE (IoIBEG,MPTR(89))
       EQUIVALENCE (IoMAN ,MPTR(90))
       EQUIVALENCE (IoI   ,MPTR(91))
       EQUIVALENCE (IoIV  ,MPTR(92))
       EQUIVALENCE (IoIP  ,MPTR(93))
       EQUIVALENCE (IoDZCO,MPTR(94))
       EQUIVALENCE (IoFAPO,MPTR(95))
       EQUIVALENCE (IoFAPN,MPTR(96))
c-----------------------------------------------------------------------------
c ** real work array
c
c    set all pointers null
       DO 3 I=1,NMPTR
           MPTR(I) = 0
3          CONTINUE
c    k-dependent scalar pointers
       DO 9 I=1,36
           MPTR(I) = I
9          CONTINUE
c    MD coefficients
       IoCMD2 = 36
c    vector pointers
       IoMBF  = 50
       IoAMR  = 54
c    more scalar pointers
       IoFTDO = 59
       IoFTDN = 60
       IoFTEO = 61
       IoFTEN = 62
c    matrix pointers 720x4
       IoC    = 71 - 721
       IoV    = 2951 - 721
       IoY    = 5831 - 721
       IoP    = 8711 - 721
       IoW    = 11591 - 721
       IoZ    = 14471 - 721
c    vector pointers
       IoP0J  = 17350
       IoYJ   = 18070
       IoYPAV = 18790
       IoZPAV = 19510
       IoDZF  = 20950
       IoCL   = 20954
       IoVL   = 20958
       IoYL   = 20962
       IoG    = 20966
       IoWCYC = 20970
       IoF    = 20974
       IoRSUM = 20978
       IoDZC  = 20982
       IoLD   = 20986
       IoAD   = 20988
       IoTDP  = 20990
       IoTDM  = 20992
       IoTDPA = 20994
       IoTDMA = 20996
       IoDWC  = 20998
c    k-dependent vector pointers 2
       IoCF   = 21000
       IoCFCB = 21002
       IoCFCT = 21004
       IoCFA  = 21006
c    scalars
       IoFAPO = 21009
       IoFAPN = 21010
       IoDZCO = 21011
       IoFOUT = 21012
       IoEMB  = 21013
       IoR    = 21014
c
c ** integer work array
       IoJUMP = 1 - 721
       IoLAG  = 2880
       IoIP   = 2884
       IoIV   = 2886
       IoJTAU = 2889
       IoJCAD = 2890
       IoNCYE = 2891
       IoNCYD = 2892
       IoNCYL = 2893
       IoIBEG = 2894
       IoMAN  = 2895
       IoI    = 2896
       IoIV   = 2896
       IoIP   = 2900
c
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  SETKXX(RW)
c
c      Loads work array RW with k-dependent constants used repeatedly.
c      K must be loaded in RW(IoK) at call.
c------------------------------------------------------------------------------
c      K               k, specific heat ratio
c      KM1             k - 1
c      KP1             k + 1
c      KMKP            (k - 1)/(k + 1)
c      KPKM            (k + 1)/(k - 1)
c      KM12            (k - 1)/2
c      KM3M            (k - 1)/(3 - k)
c      KKM1            k/(k - 1)
c      KM1K            (k - 1)/k
c      KPO2            (k+1)/2k
c      RK              1/k
c      R2K             1/2k
c      RKM1            1/(k - 1)
c      RKKM            k^(-1/(k-1))
c      TK              2k
c      TRKM            2/(k - 1)
c      TRKP            2/(k + 1)
c      TOKS            [2/(k - 1)]^2
c      TKOM            2*k/(k-1)
c      FRKM            4/(k - 1)
c      SKMS            16/(k - 1)^2
c      HKPM            (k+1)/(2(k-1))
c      HKMK            (k-1)/(2k)
c      C0CS            sqrt{(k + 1)/2}
c      XMAX            sqrt{2/(k + 1)}[(k + 1)/(k - 1)]
c      XMIN            sqrt{2/(k + 1)}[(3 - k)/(k - 1)]
c      PRPC            1/[(k + 1)/2]^{(k/(k-1)} = P_*/P_0 (s)
c      CRCC            1/sqrt((k+1)/2) =  C*/C0
c      RCHM            1/( (k+1)/2)**((k+1)/2(k-1)) )
c      SQTK            sqrt(k)
c      PHMX            1/((k+1)/(k-1))**((k+1)/(k-1))
c      SCKF            1/(k(k-1))  (entropy correction K factor)
c      TKOP            2k/(k+1)
c      K2M1            k^2-1
c      HKP1            (k+1)/2
c      SFMC            sqrt{2/(3-k)}
c      CMD2(N)         coefficients in expansion for MD2
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoKM1 ,MPTR(2))
       EQUIVALENCE (IoKP1 ,MPTR(3))
       EQUIVALENCE (IoKMKP,MPTR(4))
       EQUIVALENCE (IoKPKM,MPTR(5))
       EQUIVALENCE (IoKM12,MPTR(6))
       EQUIVALENCE (IoKM3M,MPTR(7))
       EQUIVALENCE (IoKKM1,MPTR(8))
       EQUIVALENCE (IoKM1K,MPTR(9))
       EQUIVALENCE (IoKPO2,MPTR(10))
       EQUIVALENCE (IoRK  ,MPTR(11))
       EQUIVALENCE (IoR2K ,MPTR(12))
       EQUIVALENCE (IoRKM1,MPTR(13))
       EQUIVALENCE (IoRKKM,MPTR(14))
       EQUIVALENCE (IoTK  ,MPTR(15))
       EQUIVALENCE (IoTRKM,MPTR(16))
       EQUIVALENCE (IoTRKP,MPTR(17))
       EQUIVALENCE (IoTOKS,MPTR(18))
       EQUIVALENCE (IoTKOM,MPTR(19))
       EQUIVALENCE (IoFRKM,MPTR(20))
       EQUIVALENCE (IoSKMS,MPTR(21))
       EQUIVALENCE (IoHKPM,MPTR(22))
       EQUIVALENCE (IoHKMK,MPTR(23))
       EQUIVALENCE (IoC0CS,MPTR(24))
       EQUIVALENCE (IoXMAX,MPTR(25))
       EQUIVALENCE (IoXMIN,MPTR(26))
       EQUIVALENCE (IoPRPC,MPTR(27))
       EQUIVALENCE (IoCRCC,MPTR(28))
       EQUIVALENCE (IoRCHM,MPTR(29))
       EQUIVALENCE (IoSQTK,MPTR(30))
       EQUIVALENCE (IoPHMX,MPTR(31))
       EQUIVALENCE (IoSCKF,MPTR(32))
       EQUIVALENCE (IoTKOP,MPTR(33))
       EQUIVALENCE (IoK2M1,MPTR(34))
       EQUIVALENCE (IoHKP1,MPTR(35))
       EQUIVALENCE (IoSFMC,MPTR(36))
       EQUIVALENCE (IoCMD2,MPTR(37))
c------------------------------------------------------------------------------
       REAL    K,K2
c------------------------------------------------------------------------------
c    diagnostic monitor
       LOGICAL OUT
       COMMON /MONMXX/IUMONM,MON,OUT
c------------------------------------------------------------------------------
c    compute the constants
       K = RW(IoK)
       RW(IoTK) = 2*K
       RW(IoKM1) = K - 1
       RW(IoKP1) = K + 1
       RW(IoKMKP) = RW(IoKM1)/RW(IoKP1)
       RW(IoKPKM) = RW(IoKP1)/RW(IoKM1)
       RW(IoKM12) = RW(IoKM1)/2
       RW(IoKM3M) = RW(IoKM1)/(3 - K)
       RW(IoKKM1) = K/RW(IoKM1)
       RW(IoKM1K) = 1/RW(IoKKM1)
       RW(IoKPO2) = RW(IoKP1)/RW(IoTK)
       RW(IoRK) = 1/K
       RW(IoR2K) = 1/RW(IoTK)
       RW(IoRKM1) = 1/RW(IoKM1)
       RW(IoRKKM) = 1/(K**RW(IoRKM1))
       RW(IoTRKM) = 2*RW(IoRKM1)
       RW(IoTRKP) = 2/RW(IoKP1)
       RW(IoTOKS) = RW(IoTRKM)*RW(IoTRKM)
       RW(IoTKOM) = 2*RW(IoKKM1)
       RW(IoSKMS) = 16*RW(IoRKM1)*RW(IoRKM1)
       RW(IoFRKM) = 4*RW(IoRKM1)
       RW(IoHKPM) = RW(IoKPKM)/2
       RW(IoHKMK) = RW(IoKM1K)/2
       RW(IoC0CS) = SQRT(RW(IoKP1)/2)
       RW(IoCRCC) = 1/RW(IoC0CS)
       RW(IoXMAX) = RW(IoCRCC)*RW(IoKPKM)
       RW(IoXMIN) = RW(IoCRCC)/RW(IoKM3M)
       RW(IoPRPC) = RW(IoTRKP)**RW(IoKKM1)
       RW(IoRCHM) = RW(IoTRKP)**RW(IoHKPM)
       RW(IoSQTK) = SQRT(K)
       RW(IoPHMX) = 1/RW(IoKPKM)**RW(IoKPKM)
       RW(IoSCKF) = 1/(K*(K-1))
       RW(IoTKOP) = 1/RW(IoKPO2)
       RW(IoK2M1) = RW(IoK)**2 - 1
       RW(IoHKP1) = RW(IoKP1)/2
       RW(IoSFMC) = SQRT(2/(3 - RW(IoK)))
c
c    coefficients in expansion for MD2 about the quadratic singular point
       COEFR = RW(IoRKM1)*RW(IoRK)
       K2 = K*K
       OPKS = 1 + K2
       OPKS2 = OPKS*OPKS
       TERM = K2 - OPKS2/4
c    series developed by Mathematica and checked numerically for k=1.4
       RW(IoCMD2+1) = RW(IoHKMK)
       RW(IoCMD2+2) = COEFR*TERM/2
       RW(IoCMD2+3) = COEFR*(- OPKS*TERM)/4.
       RW(IoCMD2+4) = COEFR*(-0.5*K2*TERM + 0.625*OPKS2*TERM)/4.
       RW(IoCMD2+5) = COEFR*(0.5*K2*OPKS*TERM
     ;         -0.875*OPKS*(-0.5*K2*TERM
     ;         + 0.625*OPKS2*TERM))/5.
       RW(IoCMD2+6) = COEFR*(-0.75*K2*(-0.5*K2*TERM
     ;                                +0.625*OPKS2*TERM)
     ;             -0.9*OPKS*(0.5*K2*OPKS*TERM
     ;             -0.875*OPKS*(-0.5*K2*TERM
     ;             +0.625*OPKS2*TERM)))/6.
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SCFLXX(RW)
c
c      Loads work array RW with flow constants used repeatedly.
c      CF(J) is the friction factor list and must be loaded in
c      RW(IoCF+JN) J=1,24 at call (after SETKXX).
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoSCKF,MPTR(32))
       EQUIVALENCE (IoMBF ,MPTR(39))
       EQUIVALENCE (IoAMR ,MPTR(40))
       EQUIVALENCE (IoLD  ,MPTR(64))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoCF  ,MPTR(71))
       EQUIVALENCE (IoCFCB,MPTR(72))
       EQUIVALENCE (IoCFCT,MPTR(73))
       EQUIVALENCE (IoCFA ,MPTR(74))
c------------------------------------------------------------------------------
       DATA PI/ 3.14159/
c------------------------------------------------------------------------------
c    friction
       DO 9 J=1,2
           BOA = PI*SQRT(4*RW(IoAD+J)/PI)/RW(IoAD+J)
           TERM = 0.5*RW(IoCF+J)*BOA*RW(IoLD+J)/2
           RW(IoCFCB+J) = RW(IoSCKF)*0.5*RW(IoCF+J)*BOA/32
           RW(IoCFCT+J) = RW(IoSCKF)*TERM
           RW(IoCFA+J) = TERM
9          CONTINUE
       RETURN
       END
c******************************************************************************

