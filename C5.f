c******************************************************************************
c
c      Integration subroutine
c
c******************************************************************************
c
       SUBROUTINE ODEVV(ADVAN,ALGEB,COPYV,DERIV,IERR)
c
c      Advances the differential variables one time step integration of the
c      engine model ordinary differential equations.  The geometrical variables
c      and algebraic variables are also advanced.
c------------------------------------------------------------------------------
c      SUBROUTINEs in the arguments (for the proper cycle stage):
c
c          ADVAN(DT,Y,YN,IERR)     advances diff vars. from Y to YN at step DT
c          ALGEB                   calculates algebraic vars. from diff. vars.
c          COPYV(Y,YN)             copies differential vars. from Y to YN
c          DERIV(IERR)             calculates derivatives using known vars.
c------------------------------------------------------------------------------
c      Uses second-order RK. If a discontinuity is encountered the step is
c      made in two first-order steps. The total time step DT corresponds
c      to one crank-angle degree.
c------------------------------------------------------------------------------
c      Valve flows and derivatives must all be known at call
c------------------------------------------------------------------------------
c      Notation
c          ANG     crank angle relative to compression TDC, degrees (REAL)
c          DT      time step, s
c          IANG    crank angle, degrees (INTEGER)
c          IANR    crank angle relative to intake close, degrees (INTEGER)
c          IRPM    revs/min
c------------------------------------------------------------------------------
       DIMENSION   IW(50),RW(400),Y(20),YOLD(20),YPART(20)
       COMMON  /IWVV/  IW
       COMMON  /RWVV/  RW
c------------------------------------------------------------------------------
       EXTERNAL    ALGEB,COPYV,DERIV,ADVAN
c------------------------------------------------------------------------------
       EQUIVALENCE (Y(1)    ,RW(1))
       EQUIVALENCE (YOLD(1) ,RW(41))
       EQUIVALENCE (YPART(1),RW(61))
       EQUIVALENCE (ANG     ,RW(101))
       EQUIVALENCE (DT1     ,RW(199))
       EQUIVALENCE (IANG,IW(1))
       EQUIVALENCE (IANR,IW(30))
       EQUIVALENCE (LSTG,IW(7))
       EQUIVALENCE (IRPM,IW(10))
c------------------------------------------------------------------------------
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
c    cylinder diagnostic monitor
       COMMON /MONCVV/IUMONC,MONC
c------------------------------------------------------------------------------
c    save starting angle
       IANG0 = IANG
       IANR0 = IANR
c
c -- first RK step
c
c    save cylinder variables at starting point
       CALL COPYV(Y,YOLD)
c    advance cylinder differential variables from start to 1/2 point
       DTX = DT1/2
       CALL ADVAN(DTX,YOLD,YPART,IERR)
       IF (IERR.GT.0)  RETURN
c    check for discontinuity
       IF (IERR.LT.0)  THEN
c        advanced to discontinuity; set angle at discontinuity
           ANG = IANG + DTX/DT1
c        determine remaining time
           DTX = DT1/2 - DTX
c        get geometry at the discontinuity
           CALL GEOMVV
c        use partially-advanced values for derivative estimation
           CALL COPYV(YPART,Y)
c        get algebraic cylinder variables at discontinuity
           CALL ALGEB
c        get cylinder derivatives at discontinuity
           CALL DERIV(IERR)
           IF (IERR.NE.0)  RETURN
c        complete the advance to the half point
           CALL ADVAN(DTX,Y,YPART,IERR)
           ENDIF
       IF (IERR.NE.0) RETURN
c    advance manifold Y and Z to the half point
       CALL AMYZXX(1,IERR)
       IF (IERR.NE.0) RETURN

c    get manifold states at the half point
       CALL ADMSXX(1,IERR)
       IF (IERR.NE.0) RETURN
c
c -- second RK step
c
c    set angle at the half point
       ANG = IANG + 0.5
c    get engine geometry at the half point
       CALL GEOMVV
c    use partially-advanced values for cylinder derivative estimation
       CALL COPYV(YPART,Y)
c    get algebraic cylinder variables at the half point
       CALL ALGEB
c    phase-average PC and RHOC at the half point for use in half-point flows
       CALL ADANVV(IANG,1,IANGH)
       CALL AVPRVV(IANGH,1)
c    get valve flows at the half-point
       CALL FMSCVV(IERR)
       IF (IERR.NE.0) RETURN
c    advance manifold Y and Z to the new point
       CALL AMYZXX(2,IERR)
       IF (IERR.NE.0) RETURN
c    get manifold states at the new point
       CALL ADMSXX(2,IERR)
       IF (IERR.NE.0) RETURN
c    get cylinder derivatives at the half point
       CALL DERIV(IERR)
       IF (IERR.NE.0)  RETURN
c    advance differential variables from the old point using new derivatives
       DTX = DT1
       CALL ADVAN(DTX,YOLD,Y,IERR)
c    check for discontinuity
       IF (IERR.LT.0)  THEN
c        advanced to discontinuity; set angle at discontinuity
           ANG = IANG + DTX/DT1
c        determine remaining time
           DTX = DT1 - DTX
c        get algebraic variables at the discontinuity (Y all set)
           CALL ALGEB
c        get derivatives at the discontiniuty
           CALL DERIV(IERR)
           IF (IERR.NE.0)  RETURN
c        complete the advance to the new point
           CALL COPYV(Y,YPART)
           CALL ADVAN(DTX,YPART,Y,IERR)
           ENDIF
c    check for error
       IF (IERR.NE.0)  RETURN
c    advance angle to new point
       CALL ADANVV(IANG,1,IANG)
       CALL ADANVV(IANR,1,IANR)
       ANG = IANG
c    get geometry at the new point
       CALL GEOMVV
c    get cylinder algebraic variables at new point
       CALL ALGEB
c    set phase-average PC and RHOC at the new point
       CALL AVPRVV(IANG,2)
c    get valve flows at the new point
       CALL FMSCVV(IERR)
       IF (IERR.NE.0) RETURN
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ADANVV(IA,IAD,IAN)
c
c      Advances angle from IA by IAD, putting result in IAN.  The angle
c      is maintained in the range 0-719 degrees.
c-------------------------------------------------------------------------------
       IAN = IA + IAD
       IF (IAN.GT.719) IAN = IAN - 720
       RETURN
       END
c******************************************************************************

