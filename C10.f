c******************************************************************************
c
c      Geometry subroutines
c
c******************************************************************************
c
       SUBROUTINE AVPOVV(IO,IC,APO,IDW)
c
c      Returns valve angle past open and dwell angle, degrees.
c      Returns FPO = 0 if valve closed and IDW not set.
c-----------------------------------------------------------------------------
c      In RW at call:
c          ANG crank angle (degrees)
c      Loaded at call:
c          IO  IANG at open
c          IC  IANG at close
c      Available on return:
c          APO angle past open, degrees
c          IDW total dwell angle, degrees
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION     RW(400)
       COMMON /RWVV/ RW
       EQUIVALENCE (RW(101), ANG)
c-------------------------------------------------------------------------------
c    determine angle after open
       AC = IC
       AO = IO
       IF (AO.LE.AC)  THEN
c            valve dwell in 0-720 degrees
               IF ((ANG.GT.AO).AND.(ANG.LT.AC))  THEN
c                    valve is open
                       APO = ANG - IO
                       IDW = IC - IO
                   ELSE
c                    valve is closed
                       APO = 0
                       RETURN
                   ENDIF
           ELSE
c            valve dwell straddles 0-720 degrees
               IF ((ANG.GE.AC).AND.(ANG.LE.AO))  THEN
c                    valve is closed
                       APO = 0
                   ELSE
c                    valve is open
                       IF (ANG.LT.AO)  THEN
c                            valve opened last 720 degree cycle
                               APO = ANG + 720 - IO
                           ELSE
c                            valve opened this 720 degree cycle
                               APO = ANG - IO
                           ENDIF
                       IDW = IC + 720 - IO
                   ENDIF
           ENDIF
       RETURN
       END
c******************************************************************************
c
       FUNCTION  CCAMVV(APO,IDW)
c
c      Cosine valve subroutine; returns fraction of full-open area.
c-----------------------------------------------------------------------------
c      Arguments loaded at call:
c          APO     degrees past open
c          IDW     degrees of dwell
c      Assumed area: 0.5*[1 - cos(2*pi*APO/IDW)]
c-----------------------------------------------------------------------------
       DATA PI/3.1415928E0/
c-----------------------------------------------------------------------------
       ADW = IDW
       IF ((APO.LE.0).OR.(APO.GE.ADW)) THEN
               CCAMVV = 0
           ELSE
               CCAMVV = 0.5*(1 - COS(2.0*PI*APO/ADW))
           ENDIF
       RETURN
       END
c******************************************************************************
c
       FUNCTION  CFCPVV(APO,IDW,IRO,IRC)
c
c      Cos-flat-cos valve subroutine; returns fraction of full-open area.
c-----------------------------------------------------------------------------
c      Arguments loaded at call:
c          APO     degrees past open
c          IDW     degrees of dwell
c          IRO     degrees required to open
c          IRC     degrees required to close
c      Assumed area fraction:
c           0.5*[1 - cos(pi*APO/IRO)]             O le APO le IRO
c           1                                     IRO le APO le IDW-IRC
c           0.5*[1 + cos(pi*(IDW-IRC-APO)/IRC)]   IDW-IRC le APO le IDW
c-----------------------------------------------------------------------------
       DATA PI/3.1415928E0/
c-----------------------------------------------------------------------------
           ADW = ADW
           ARO = ARO
           ARC = ARC
       IF ((APO.LE.0).OR.(APO.GE.ADW)) THEN
               CFCPVV = 0
           ELSEIF ((APO.GT.0).AND.(APO.LT.ARO))  THEN
               CFCPVV = 0.5*(1 - COS(PI*APO/ARO))
           ELSEIF ((APO.GE.ARO).AND.(APO.LT.(ADW-ARC)))  THEN
               CFCPVV = 1
           ELSE
               CFCPVV = 0.5*(1 + COS(PI*(ADW-ARC-APO)/ARC))
           ENDIF
       RETURN
       END
c******************************************************************************
c
       FUNCTION FVINVV()
c
c      Returns intake valve area fraction for the current crank angle.
c      Selects appropriate subroutine depending on valve program.
c-----------------------------------------------------------------------------
c      Arguments:
c          IO    IANG at open
c          IC    IANG at close
c-----------------------------------------------------------------------------
c      Loaded in RW at call:
c          ANG   crank angle (degrees)
c      Loaded in IW at call:
c          KIVP  intake valve program; 1 cosine, 2 cos-flat-cos, 4 user
c          IAIO  angle at intake close, degrees
c          IAIC  angle at intake open, degrees
c          IRIO  angle required for intake to open, degrees
c          IRIC  angle required for intake to close, degrees
c          LIFL  zero if valve closed, otherwise not
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION     IW(50),RW(400)
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
       EQUIVALENCE (IAIO,IW(2))
       EQUIVALENCE (IAIC,IW(3))
       EQUIVALENCE (LIFL,IW(9))
       EQUIVALENCE (KIVP,IW(18))
       EQUIVALENCE (IRIO,IW(24))
       EQUIVALENCE (IRIC,IW(25))
       EQUIVALENCE (IUIP,IW(33))
       EQUIVALENCE (ANG ,RW(101))
c-----------------------------------------------------------------------------
c    user valve program
       DIMENSION AVDI(48),FVDI(48)
       COMMON  /UVIDVV/ NDI,FMXI,AVDI,FVDI
c-----------------------------------------------------------------------------
c    check for valve closed
       IF (LIFL.EQ.0)  THEN
           FVINVV = 0
           RETURN
           ENDIF
c    determine open angle and dwell
       CALL AVPOVV(IAIO,IAIC,APO,IDW)
c    branch on program
       IF (KIVP.EQ.1)  THEN
c            cosine cam
               FVINVV = CCAMVV(APO,IDW)
           ELSEIF (KIVP.EQ.2)  THEN
c            cos-flat-cos cam
               FVINVV = CFCPVV(APO,IDW,IRIO,IRIC)
           ELSEIF (KIVP.EQ.4) THEN
c            user table
               CALL INTPVV(ANG,APO,IDW,NDI,AVDI,IUIP,CM,CP)
               FVINVV = (CM*FVDI(IUIP) + CP*FVDI(IUIP+1))/FMXI
           ELSE
c            error; shut off the flow
               FVINVV = 0
           ENDIF
       RETURN
       END
c******************************************************************************
c
       FUNCTION FVEXVV()
c
c      Returns exhaust valve area fraction for the current crank angle.
c      Selects appropriate subroutine depending on valve program.
c-----------------------------------------------------------------------------
c      Arguments:
c          IO    IANG at open
c          IC    IANG at close
c-----------------------------------------------------------------------------
c      Loaded in RW at call:
c          ANG   crank angle (degrees)
c      Loaded in IW at call:
c          KIVP  exhaust valve program; 1 cosine, 2 cos-flat-cos, 4 user
c          IAEO  angle at intake close, degrees
c          IAEC  angle at intake open, degrees
c          IREO  angle required for intake to open, degrees
c          IREC  angle required for intake to close, degrees
c          LEFL  0 if valve closed, otherwise not
c-----------------------------------------------------------------------------
c     Arguments:
c          IA    current crank angle, degrees (0 - 720)
c          IO    IANG at open
c          IC    IANG at close
c-----------------------------------------------------------------------------
c      Loaded in RW at call:
c          ANG   crank angle (degrees)
c      Loaded in IW at call:
c          KEVP  exhaust valve program; 1 cosine, 2 cos-flat-cos, 4 user
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION     IW(50),RW(400)
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
       EQUIVALENCE (IAEO,IW(4))
       EQUIVALENCE (IAEC,IW(5))
       EQUIVALENCE (LEFL,IW(8))
       EQUIVALENCE (KEVP,IW(18))
       EQUIVALENCE (IREO,IW(26))
       EQUIVALENCE (IREC,IW(27))
       EQUIVALENCE (IUEP,IW(34))
       EQUIVALENCE (ANG ,RW(101))
c-----------------------------------------------------------------------------
c    user valve program
       DIMENSION AVDE(48),FVDE(48)
       COMMON  /UVEDVV/ NDE,FMXE,AVDE,FVDE
c------------------------------------------------------------------------------
c    check for valve closed
       IF (LEFL.EQ.0)  THEN
           FVEXVV = 0
           RETURN
           ENDIF
c    determine angle past open and dwell
       CALL AVPOVV(IAEO,IAEC,APO,IDW)
c    branch on program
       IF (KEVP.EQ.1)  THEN
c            cosine cam
               FVEXVV = CCAMVV(APO,IDW)
           ELSEIF (KEVP.EQ.2)  THEN
c            cos-flat-cos program
               FVEXVV = CFCPVV(APO,IDW,IREO,IREC)
           ELSEIF (KEVP.EQ.4)  THEN
c            user table
               CALL INTPVV(ANG,APO,IDW,NDI,AVDE,IUEP,CM,CP)
               FVEXVV = (CM*FVDE(IUEP) + CP*FVDE(IUEP+1))/FMXE
           ELSE
c            error; shut off the flow
               FVEXVV = 0
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  INTPVV(A,APO,IDW,ND,AVD,IM,CM,CP)
c
c      Determines interpolation factors in valve program table
c-----------------------------------------------------------------------------
c      Nomenclature
c          APO     absolute crank angle degrees past open
c          IDW     absolute dwell, degrees
c          ND      number ot data points in table
c          AVD(K)  relative angle past open at Kth data point
c          IM      data point just before A
c          CM      weight of point at K=IM in linear interpolation
c          CP      wight of point at K=IM+1 in linear interpolation
c-----------------------------------------------------------------------------
       DIMENSION AVD(ND)
c-----------------------------------------------------------------------------
c    determine relative angle (to shift and stretch profile)
       AR = APO*AVD(ND)/IDW
c    check that AR > AVD(IM)
10     IF (AR.LT.AVD(IM)) THEN
           IF (IM.EQ.1) THEN
               CM = 1
               CP = 0
               RETURN
               ENDIF
           IM = IM-1
           GOTO 10
           ENDIF
c    find point above
20     IF (AR.GT.AVD(IM+1)) THEN
           IF (IM.EQ.ND)  THEN
               CM = 0
               CP = 1
               RETURN
               ENDIF
           IM = IM + 1
           GOTO 20
           ENDIF
c    angle now bracketed by IM and IM+1
       CM = (AVD(IM+1) - AR)/(AVD(IM+1) - AVD(IM))
       CP =  1 - CM
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SPISVV(F,DFDA)
c
c      Piston displacement and velocity for standard crankshaft.
c-----------------------------------------------------------------------------
c      In RW at call:
c          ANG     crank angle (degrees)
c          R       connecting rod length/stroke (called RRTS in RW)
c      Arguments returned:
c          F       (displacement from tdc)/stroke
c          DFDA    dF/dA, 1/radians
c-----------------------------------------------------------------------------
c      Nomenclature:
c          A       crank angle, radians
c          B       angle opposite connecting rod (at piston), radians
c          XM      (distance to tdc)/stroke
c          COSA    cosine(A)
c          COSB    cosine(B)
c          SINA    sin(AR)
c          SINBB   sin(BR)
c
c      Formulae:
c          XM = R + 0.5
c          F = XM - 0.5*cos(A) - R*cos(B)
c          R*sin(B) = 0.5*sin(A)
c          dF/dA = 0.5*sin(A) + R*sin(B)*dB/dA
c                 = 0.5*sin(A) + 0.5*sin(A)*0.5*cos(A)/(R*cos(B))
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION     IW(50),RW(400)
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
       EQUIVALENCE (ANG ,RW(101))
       EQUIVALENCE (R   ,RW(194))
c-----------------------------------------------------------------------------
       DATA PI/3.1415928E0/
c-----------------------------------------------------------------------------
       A = (PI*ANG)/180
       COSA = COS(A)
       SINA = SIN(A)
       SINB = 0.5*SINA/R
       COSB = SQRT(1 - SINB*SINB)
       F = R + 0.5 - 0.5*COSA - R*COSB
       DFDA = 0.5*SINA*(1 + 0.5*COSA/(R*COSB))
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE PIS2VV(F,DFDA)
c
c      Piston displacement and velocity for dual-stroke sinusoid.
c-----------------------------------------------------------------------------
c      In RW at call:
c          ANG     crank angle (degrees)
c          R       ratio of expansion stroke/compression stroke (RRTS in RW)
c      Arguments returned:
c          F       (displacement from tdc)/compression stroke
c          DFDA    dF/dA, 1/radians
c-----------------------------------------------------------------------------
c      Nomenclature:
c          A       crank angle, radians
c
c      Formulae:
c          F = 0.5*R*(1 - cos(A))      0 - 360 deg
c          F = 0.5*(1 - cos(A))      360 - 720 deg
c-----------------------------------------------------------------------------
c    cylinder work arrays
       DIMENSION     IW(50),RW(400)
       COMMON /IWVV/ IW
       COMMON /RWVV/ RW
       EQUIVALENCE (ANG ,RW(101))
       EQUIVALENCE (R   ,RW(194))
c-----------------------------------------------------------------------------
       DATA PI/3.1415928E0/
c-----------------------------------------------------------------------------
       A = (PI*ANG)/180
       COSA = COS(A)
       SINA = SIN(A)
c    assume compression stroke
       F = 0.5*(1 - COS(A))
       DFDA = 0.5*SIN(A)
         IA = A
       IF (IA.GT.360)  RETURN
c    expansion stroke
       F = F*R
       DFDA = DFDA*R
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE PISTVV(F,DFDA)
c
c      Calculates piston displacement and angular velocity/stroke.
c      Selects appropriate subroutine depending on piston program.
c-----------------------------------------------------------------------------
c      Loaded in RE at call:
c          RRTS    piston program parameter (rod/stroke for conventional)
c      Loaded in IW at call:
c          IANG    current crank angle, degrees (0 - 719)
c          KPPR    piston program control; 1 standard, 2 dual-stroke
c-----------------------------------------------------------------------------
c      Arguments returned:
c          F       (displacement from tdc)/stroke
c          DFDA    dF/dA, 1/radians
c-----------------------------------------------------------------------------
       DIMENSION IW(50),RW(400)
       COMMON /IWVV /IW
       COMMON /RWVV/ RW
c-----------------------------------------------------------------------------
       EQUIVALENCE (RRTS,RW(194))
       EQUIVALENCE (IANG,IW(1))
       EQUIVALENCE (KPPR,IW(20))
c-----------------------------------------------------------------------------
       IF (KPPR.EQ.1)  THEN
c            standard piston program
               CALL SPISVV(F,DFDA)
           ELSEIF (KPPR.EQ.2) THEN
c            dual-stroke sinusoid
               CALL PIS2VV(F,DFDA)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  GEOMVV
c
c      Calculates flow and heat transfer areas at current crank angle
c-----------------------------------------------------------------------------
c    Loaded in RW at call:
c          AHAC    head heat transfer area above piston at TDC, m**2
c          AMEV    full-open exhaust valve flow area, m**2
c          AMIV    full-open inlet valve flow area, m**2
c          ANG     crank angle from TDC compression, degrees
c          APIS    piston area, m**2
c          DISP    (bore area)*stroke, m**3
c          RRTS    piston program parameter (rod/stroke for conventional)
c          VCLR    clearance volume, m**3
c          ZCHT    bore*pi*stroke, m**2
c          ZDXP    stroke*RPM*2*pi/60, rad-m/s
c-----------------------------------------------------------------------------
c    Loaded in IW at call:
c          IANG    current crank angle, degrees (0 - 720)
c          IAIO    IANG at intake open
c          IAIC    IANG at intake close (end of gas exchange stage)
c          IAEO    IANG at exhaust open (start of gas exchange stage)
c          IAEC    IANG at exhaust close
c-----------------------------------------------------------------------------
c    Available in RW on return:
c          AFLE    exhaust valve area open area, m**2
c          AFLI    intake valve area open area, m**2
c          AHTC    heat transfer area in cylinder, m**2
c          DVDT    dV/dt, m**3/s
c          VC      cylinder volume, m**3
c          VPIS    piston velocity, m/s
c-----------------------------------------------------------------------------
c    Available in IW on return:
c          LEFL    0 exhaust closed
c                  1 exhaust open
c          LIFL    0 intake valve is closed
c                  1 intake valve open
c-----------------------------------------------------------------------------
       EXTERNAL FVINVV,FVEXVV
c-----------------------------------------------------------------------------
       DIMENSION   IW(50),RW(400)
c-----------------------------------------------------------------------------
       COMMON  /IWVV/  IW
       COMMON  /RWVV/  RW
c-----------------------------------------------------------------------------
       EQUIVALENCE (VC  ,RW(104))
       EQUIVALENCE (DVDT,RW(121))
       EQUIVALENCE (AFLE,RW(122))
       EQUIVALENCE (AFLI,RW(123))
       EQUIVALENCE (AHTC,RW(124))
       EQUIVALENCE (APIS,RW(181))
       EQUIVALENCE (AMEV,RW(184))
       EQUIVALENCE (AMIV,RW(185))
       EQUIVALENCE (AHAC,RW(186))
       EQUIVALENCE (VCLR,RW(190))
       EQUIVALENCE (ZCHT,RW(191))
       EQUIVALENCE (ZDXP,RW(192))
       EQUIVALENCE (DISP,RW(193))
       EQUIVALENCE (VPIS,RW(200))
       EQUIVALENCE (IANG,IW(1))
       EQUIVALENCE (IAIO,IW(2))
       EQUIVALENCE (IAIC,IW(3))
       EQUIVALENCE (IAEO,IW(4))
       EQUIVALENCE (IAEC,IW(5))
       EQUIVALENCE (LEFL,IW(8))
       EQUIVALENCE (LIFL,IW(9))
c-----------------------------------------------------------------------------
c    calculate intake valve flow area
       IF (LIFL.NE.0)  THEN
               FV =  FVINVV()
               AFLI = AMIV*FV
           ELSE
               AFLI = 0
           ENDIF
c    calculate exhaust valve flow area
       IF (LEFL.NE.0)  THEN
               FV =  FVEXVV()
               AFLE = AMEV*FV
           ELSE
               AFLE = 0
           ENDIF
c    get piston positon and angular velocity
       CALL PISTVV(F,DFDA)
       VC = VCLR + DISP*F
c    compute piston velocity
       VPIS = ZDXP*DFDA
c    compute rate of change of volume
       DVDT = APIS*VPIS
c    compute heat transfer area
       AHTC = AHAC + ZCHT*F
       RETURN
       END
c******************************************************************************

