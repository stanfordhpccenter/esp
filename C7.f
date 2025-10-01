c******************************************************************************
c
c      Properties routines
c
c******************************************************************************
c      The enthalpy and Pv product for the reactants and products are
c      assumed to be functions of temperature only.  The data are stored in
c      tables in COMMON block /PRODVV/:
c
c       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c
c          HP(I)   enthalpy of products at Ith temperature, J/kg
c          HR(I)   enthalpy of reactants at Ith temperature, J/kg
c          PVP(I)  Pv of products at Ith temperature, J/kg
c          PVR(I)  Pv of reactants at Ith temperature, J/kg
c          UP(I)   internal energy of products at Ith temperature, J/kg
c          UR(I)   internal energy of reactants at Ith temperature, J/kg
c
c      The temperatures are 200,300,400,...,4900 K; T(I) = 100 + 100*I
c
c      The energies and enthalpies are adjusted to make the internal energy
c      of the products zero at 300K.
c******************************************************************************
c
       FUNCTION VINTVV(D,T)
c
c      Interpolates in property data array D at temperature T, returning value.
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   D(IMAX)
c-----------------------------------------------------------------------------
c    determine lower entry
       I = T/100 - 1
       IF (I.LT.1)  I = 1
       IF (I.GE.IMAX) I = IMAX - 1
c    interpolate
       TI =  100 + 100*I
       VINTVV = D(I) + (D(I+1) - D(I))*(T - TI)/100
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE TINTVV(D,T,V)
c
c      Interpolates in property data array D for the value V to find the
c      temperature T.  At call T is the estimated temperature.
c      dD/dT > 0 is assumed.
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   D(IMAX)
c-----------------------------------------------------------------------------
c    estimate lower entry
       I = T/100 - 1
       IF (I.LT.1)  I = 1
       IF (I.GE.IMAX) I = IMAX - 1
c    find lower entry
10     IF (V.LT.D(I))  THEN
           IF (I.EQ.1)  GOTO 20
           I = I - 1
           GOTO 10
           ENDIF
       IF (V.GT.D(I+1)) THEN
           IF (I+1.EQ.IMAX)  GOTO 20
           I = I + 1
           GOTO 10
           ENDIF
c    interpolate to find T
20     TI =  100 + 100*I
       T = TI + 100*(V - D(I))/(D(I+1) - D(I))
       RETURN
       END
c******************************************************************************
c
       FUNCTION DERTVV(D,T)
c
c      Uses property array D to return the value of dD/dT at temperature T.
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   D(IMAX)
c-----------------------------------------------------------------------------
c    determine lower entry
       I = T/100 - 1
       IF (I.LT.2)  THEN
c        use central difference at second point
           DERTVV = (D(3) - D(1))/200
           RETURN
           ENDIF
       IF (I.GE.IMAX-1) THEN
c        use central difference at penultimate point
           DERTVV = (D(IMAX) - D(IMAX-2))/200
           RETURN
           ENDIF
c   interpolate between central differences at I and I+1
       DIF1 = D(I+1) - D(I-1)
       DIF2 = D(I+2) - D(I)
c   note: TI = 100 + 100*I  so F = (T - TI)/100 = T/100 - I - 1
       DERTVV = (DIF1 + (DIF2 - DIF1)*(T/100 - I - 1))/200
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE TIN2VV(D1,D2,F1,T,V)
c
c      Interpolates in property data arrays D1 and D2 to find the temperature
c      T at which   F1*D1 + (1-F1)*D2   has the value V.
c      At call T is the estimated temperature.  dD/dT > 0 is assumed.
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   D1(IMAX),D2(IMAX)
c-----------------------------------------------------------------------------
c    estimate lower entry
       I = T/100 - 1
       IF (I.LT.1)  I = 1
       IF (I.GE.IMAX) I = IMAX - 1
c    find lower entry
       F2 = 1 - F1
10     VI = F1*D1(I) + F2*D2(I)
       IF (V.LT.VI)  THEN
           IF (I.EQ.1)  GOTO 12
           I = I - 1
           GOTO 10
           ENDIF
12     VIP = F1*D1(I+1) + F2*D2(I+1)
       IF (V.GT.VIP) THEN
           IF (I+1.EQ.IMAX)  GOTO 20
           I = I + 1
           GOTO 10
           ENDIF
c    interpolate to find T
20     VI = F1*D1(I) + F2*D2(I)
       TI =  100 + 100*I
       T = TI + 100*(V - VI)/(VIP - VI)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  UPVV(T)
c
c      Returns product internal energy (J/kg)  at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
c    reference shift
       UPVV = VINTVV(UP,T) - UP(2)
C      UPVV = VINTVV(UP,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  URVV(T)
c
c      Returns reactant internal energy (J/kg)  at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
c    reference shift
       URVV = VINTVV(UR,T) - UP(2)
C      URVV = VINTVV(UR,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  HPVV(T)
c
c      Returns product enthalpy (J/kg)  at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
c    reference shift
       HPVV = VINTVV(HP,T) - UP(2)
C      HPVV = VINTVV(HP,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  HRVV(T)
c
c      Returns reactant enthalpy (J/kg)  at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
c    reference shift
       HRVV = VINTVV(HR,T) - UP(2)
C      HRVV = VINTVV(HR,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  PVPVV(T)
c
c      Returns value of Pv (J/kg) for product at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       PVPVV = VINTVV(PVP,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  PVRVV(T)
c
c      Returns value of Pv (J/kg) for reactants at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       PVRVV = VINTVV(PVR,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  CPPVV(T)
c
c      Returns product specific heat Cp (J/kg-K)  at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       CPPVV = DERTVV(HP,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  CPRVV(T)
c
c      Returns reactant specific heat Cp (J/kg-K)  at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       CPRVV = DERTVV(HR,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  CVPVV(T)
c
c      Returns product specific heat Cv (J/kg-K)  at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       CVPVV = DERTVV(UP,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  CVRVV(T)
c
c      Returns reactant specific heat Cv (J/kg-K)  at T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       CVRVV = DERTVV(UR,T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  UMVV(XP,T)
c
c      Returns internal energy (J/kg) of mixture with product mass fraction XP
c      at temperature T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       UMVV = XP*UPVV(T) + (1 - XP)*URVV(T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  HMVV(XP,T)
c
c      Returns enthalpy (J/kg) of mixture with product mass fraction XP
c      at temperature T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       HMVV = XP*HPVV(T) + (1 - XP)*HRVV(T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  PVMVV(XP,T)
c
c      Returns Pv product (J/kg) of mixture with product mass fraction XP
c      at temperature T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       PVMVV = XP*PVPVV(T) + (1 - XP)*PVRVV(T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  CPMVV(XP,T)
c
c      Returns specific heat Cp (J/kg) of mixture with product mass fraction XP
c      at temperature T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       CPMVV = XP*CPPVV(T) + (1 - XP)*CPRVV(T)
       RETURN
       END
c******************************************************************************
c
       FUNCTION  CVMVV(XP,T)
c
c      Returns specific heat Cv (J/kg) of mixture with product mass fraction XP
c      at temperature T (K).
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
       CVMVV = XP*CVPVV(T) + (1 - XP)*CVRVV(T)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  GTUPVV(U,T)
c
c      Gets the temperature T (K) for products having internal energy U, J/kg
c      At call T is a trial value.
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
c    reference shift
       UX = U + UP(2)
C      UX = U
       CALL TINTVV(UP,T,UX)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  GTUMVV(XP,UM,T)
c
c      Gets the temperature T (K) corresponding to a mixture of products
c      and reactants with product mass fraction XP and mixture enthalpy
c      UM (J/kg).  At call T is a trial value.
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
c    reference shift
       UMX = UM + UP(2)
C      UMX = UM
       CALL TIN2VV(UP,UR,XP,T,UMX)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  GTHPVV(H,T)
c
c      Gets the temperature T (K) for products having enthalpy H,J/kg.
c      At call T is a trial value.
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
c    reference shift
       HX = H + UP(2)
C      HX = H
       CALL TINTVV(HP,T,HX)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  GTHMVV(XP,HM,T)
c
c      Gets the temperature T (K) corresponding to a mixture of products
c      and reactants with product mass fraction XP and mixture enthalpy
c      HM (J/kg).  At call T is a trial value.
c-----------------------------------------------------------------------------
       PARAMETER   (IMAX = 48)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
c-----------------------------------------------------------------------------
c    reference shift
       HMX = HM + UP(2)
C      HMX = HM
       CALL TIN2VV(HP,HR,XP,T,HMX)
       RETURN
       END
c******************************************************************************

