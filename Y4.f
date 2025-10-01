c******************************************************************************
c
c      Manifold function routines
c
c******************************************************************************
c
       FUNCTION YZXPXX(RW,Z,X,P0)
c
c      Returns entropy measure Y for specified Z,X,P0.
c      RW must be loaded at call.
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoKM1 ,MPTR(2))
       EQUIVALENCE (IoRK  ,MPTR(11))
c------------------------------------------------------------------------------
       C0 = Z/X
       YZXPXX = ((C0*C0*RW(IoRK))**RW(IoK))/(P0**RW(IoKM1))
       RETURN
       END
c*****************************************************************************
c
       FUNCTION PZXYXX(RW,Z,X,Y)
c
c      Returns the stagnation pressure P0 for specified Z,X,Y.
c      RW must be loaded at call.
c------------------------------------------------------------------------------
       PARAMETER           (NDRW = 21024)
       DIMENSION           RW(NDRW)
c------------------------------------------------------------------------------
       C0 = Z/X
       PZXYXX = PCYXX(RW,C0,Y)
       RETURN
       END
c*****************************************************************************
c
       FUNCTION PCYXX(RW,C,Y)
c
c      Returns the pressure  specified C,Y
c      RW must be loaded at call.
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoKKM1,MPTR(8))
       EQUIVALENCE (IoRK  ,MPTR(11))
       EQUIVALENCE (IoRKM1,MPTR(13))
c------------------------------------------------------------------------------
       PCYXX = ((C*C*RW(IoRK))**RW(IoKKM1))/Y**RW(IoRKM1)
       RETURN
       END
c******************************************************************************
c
       FUNCTION CPYXX(RW,P,Y)
c
c      Returns the sound speed C for specified P,Y
c      RW must be loaded at call.
c-----------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoR2K ,MPTR(12))
       EQUIVALENCE (IoHKMK,MPTR(23))
       EQUIVALENCE (IoSQTK,MPTR(30))
c------------------------------------------------------------------------------
       CPYXX = RW(IoSQTK)*(P**RW(IoHKMK))*(Y**RW(IoR2K))
       RETURN
       END
c******************************************************************************
c
       FUNCTION YC2PXX(RW,CS,P)
c
c      Returns the Y for specified C^2,P
c      RWK must be loaded at call.
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoKM1 ,MPTR(2))
c------------------------------------------------------------------------------
       YC2PXX = (CS/RW(IoK))**RW(IoK)/P**RW(IoKM1)
       RETURN
       END
c******************************************************************************
c
       FUNCTION RCYXX(RW,C,Y)
c
c      Returns the density for specified C,Y
c      RW must be loaded at call.
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoRK  ,MPTR(11))
       EQUIVALENCE (IoRKM1,MPTR(13))
c------------------------------------------------------------------------------
       RCYXX = (C*C*RW(IoRK)/Y)**RW(IoRKM1)
       RETURN
       END
c******************************************************************************
c
       FUNCTION RCPXX(RW,C,P)
c
c      Returns the density for specified C,P
c      RW must be loaded at call.
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoK   ,MPTR(1))
c------------------------------------------------------------------------------
       RCPXX = RW(IoK)*P/(C*C)
       RETURN
       END
c******************************************************************************

