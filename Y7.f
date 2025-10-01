c****************************************************************************
c
c      Manifold duct routines
c
c******************************************************************************
c
       SUBROUTINE ADYZXX(RW,IW,NRK2,IERR)
c
c      Performs step 1 or step 2 of the RK2 advance of manifold ducts.
c
c      Step 1:
c        Advances Y and Z one-half crank angle degree, to be used in step 1
c        of the flow calculation. The restores the W record, jumps, and lags
c        to their original conditions for use in the second step of the RK2.
c
c      Step 2:
c        Advances Y and Z one full crank angle degree using the flows based
c        on the 1/2 state after RK2 step 1.
c
c      RW and IW are the work arrays for the manifold.
c      NRK2 is the RK2 step number.
c      IERR is returned with the error number (0 if ok).
c-----------------------------------------------------------------------------
c      The intake manfold is MAN 1
c          Point 1: start of feeder downstream of ambient plenum
c          Point 2: discharge of feeder into junction
c          Point 3: entrance to runner from junction
c          Point 4: discharge from runner to cylinder (before EGR)
c          Duct 1:  feeder
c          Duct 2:  runner
c
c      The intake exhaust manifold is MAN 2
c          Point 1: intake from cylinder (after EGR)
c          Point 2: discharge of runner into junction
c          Point 3: entrance to collector from junctiom
c          Point 4: discharge from collector to ambient
c          Duct 1:  runner
c          Duct 2:  collector
c-----------------------------------------------------------------------------
c      This subroutine tracks the mass entering and leaving each duct, for
c      the purpose of evaluating the entropy of mass leaving based on the
c      entropy it had when it entered.  In order to account for slight mass
c      unbalances over a cycle due to the analytical and numerical
c      approximations, the mass flow rates at the junction end of each duct,
c      computed from the approximate characteristic analysis, are augmented
c      by factors based on the unbalance over the previous cycles.
c
c      Entropy discontinuities are handled by interpolation on P and C.
c      P is continuous, but C is not, and therefore the interpolated
c      Y value is calculated from the interpolated P and C to maintain
c      consistency in smoothing the discontinuity.
c-----------------------------------------------------------------------------
c        variables indexed by angle index I=(degrees-1) and state index N
c          V(I,N)      velocity (positive in direction of increasing N)
c          C(I,N)      sound speed
c          P(I,N)      pressure
c          Y(I,N)      entropy parameter P/rho^k
c          W(I,N)      total mass/area that has past point N since t=0
c          Z(I,N)      phase-averaged acoustic parameter Z arriving at N
c                        for N=1 and N=3, [2/(k-1)]c - V
c                        for N=2 and N=4, [2/(k-1)]c + V
c          CV(N)       C-V for N=1 and N=3 at the - charactersitic launch time
c                      C+V for N=2 and N=4 at the - charactersitic launch time
c
c        averaged delay time at start of period
c          TDPA(J)     average TDP(J)
c          TDMA(J)     average TDM(J)
c
c        factors to correct for slight (numerical) mass unbalances
c          GF(J)       cycle-average corrector for G at inlet to duct J
c
c        pointers/variables for convection analysis (FP = first place)
c          JUMP(I,N)   if <0,  back jump to FP where W(I+JUMP,N) > W(I,N)
c                      if >0,  forward jump to FP where W(I+JUMP,N) < W(I,N)
c                      if 0,   unset; mass still in duct, not otherwise
c                      Since there can be rare points having both positive
c                      and negative jumps, JUMP(I,N) is a code that can
c                      produce both. See subroutine JUMPXX and function
c                      JUMCXX in this file for the code.
c
c          LAG(N)      back jump to FP where W(I-ILAG,NO) = W(I,N)-;
c                        Here N is analysis end state, NO is other end state.
c          G(N)        current mass flow rate/area at state N
c          WCYC(N)     total mass/area passed over one cycle
c
c          IV(N)       index of V point (minimum Win)
c          IP(N)       index of P point (maximum Win)
c
c        duct constants and variables
c          LD(J)       length
c          AD(J)       flow area
c          TDP(J)      time delay for + acoustic Z = [2/(k-1)] + V
c          TDM(J)      time delay for - acoustic Z = [2/(k-1)] - V
c
c        duct friction coefficients for characteristics in duct J
c          CFCB(J)     convection backflow
c          CFCF(J)     convection from opposite end
c          CFA(J)      acoustic characteristics
c
c        launch data for acoustic characteristic arriving at N
c          CL(N)       sound speed
c          VL(N)       velocity
c          YL(N)       entropy parameter (not saved or used)
c
c        terms for acoustic characteristics arriving at state N
c          DZF(N)      friction correction to Z
c
c        Z corrections for cycle mass balance
c          RSUM(N)     density sum for cycle at point N
c          DZC(J)      increment to Z in duct J
c------------------------------------------------------------------------------
c      The duct that handles the total flow for all cylinders on the
c      manifold is made periodic at the cylinder frequency.
c
c      FI,QI,FO, and Z are averaged linearly. Y is averaged logarithmically
c      since the entropy is proportional to log(Y).
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 2904)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
c------------------------------------------------------------------------------
       LOGICAL BEFORE,AFTER
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoKM1 ,MPTR(2))
       EQUIVALENCE (IoKPKM,MPTR(5))
       EQUIVALENCE (IoR2K ,MPTR(12))
       EQUIVALENCE (IoTRKM,MPTR(16))
       EQUIVALENCE (IoHKMK,MPTR(23))
       EQUIVALENCE (IoSQTK,MPTR(30))
       EQUIVALENCE (IoSCKF,MPTR(32))
       EQUIVALENCE (IoZCF ,MPTR(37))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoY   ,MPTR(46))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoW   ,MPTR(48))
       EQUIVALENCE (IoZ   ,MPTR(49))
       EQUIVALENCE (IoDZF ,MPTR(55))
       EQUIVALENCE (IoCL  ,MPTR(56))
       EQUIVALENCE (IoVL  ,MPTR(57))
       EQUIVALENCE (IoYL  ,MPTR(58))
       EQUIVALENCE (IoG   ,MPTR(59))
       EQUIVALENCE (IoWCYC,MPTR(60))
       EQUIVALENCE (IoF   ,MPTR(61))
       EQUIVALENCE (IoRSUM,MPTR(62))
       EQUIVALENCE (IoDZC ,MPTR(63))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoTDP ,MPTR(66))
       EQUIVALENCE (IoTDM ,MPTR(67))
       EQUIVALENCE (IoDWC ,MPTR(70))
       EQUIVALENCE (IoCFCB,MPTR(72))
       EQUIVALENCE (IoCFCT,MPTR(73))
       EQUIVALENCE (IoYPAV,MPTR(77))
       EQUIVALENCE (IoZPAV,MPTR(78))
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
c    values computed before averaging
       DIMENSION Y(4),Z(4),RHO(4)
c-----------------------------------------------------------------------------
c    cylinder work array
       REAL          MINC
       DIMENSION     RWC(400)
       COMMON /RWVV/ RWC
       EQUIVALENCE (MINC,RWC(10))
c-----------------------------------------------------------------------------
c    arrays used locally
       DIMENSION WLAST(4),CLAST(4),VLAST(4),PLAST(4),KFLO(4)
c-----------------------------------------------------------------------------
c    data for RK2 step2 reset
       DIMENSION WQ(4,2),YQ(4,2),ZQ(4,2),VQ(4,2),PQ(4,2),CQ(4,2),
     ;   FQ(4,2),
     ;   JUMPQ(4,2),LAGQ(4,2),IVQ(4,2),IPQ(4,2),JUMPBQ(4,2),IJBQ(4,2)
       COMMON /RK2AXX/WQ,YQ,ZQ,VQ,PQ,CQ,FQ,JUMPQ,LAGQ,IVQ,IPQ,JUMPBQ,IJBQ
c-----------------------------------------------------------------------------
       LOGICAL MORE,ESE(4)
c-----------------------------------------------------------------------------
c    time step
       COMMON /DTXX/DT
c------------------------------------------------------------------------------
c    diagnostic monitor
       LOGICAL OUT
       COMMON /MONMXX/IUMONM,MONM,OUT
c------------------------------------------------------------------------------
c    Roundoff error factors
       DATA ROERR/1E-7/
c------------------------------------------------------------------------------
c    code for positive flow direction at state N; 1 in, -1 out
       DATA    KFLO/1,-1,1,-1/
c------------------------------------------------------------------------------
c    manifold number
       M = IW(IoMAN)
c    get current and start angles
       IBEG = IW(IoIBEG)
c    check RK2 step
       IF (NRK2.EQ.1)  THEN
c        get index to current angle
           I = IW(IoI)
c          first RK2 step
               DTX = DT/2
c            check for start of cycle
               IF (I.EQ.IBEG)  THEN
c                initialize sums for cycle
                  RW(IoFOUT) = 0
                   DO 9 N=1,4
                       RW(IoRSUM+N) = 0
                       RW(IoF+N) = 0
9                      CONTINUE
c                advance engine cycle count
                   IW(IoNCYE) = IW(IoNCYE) + 1
                   ENDIF
c            check for reset of cylinder cycle counter
               IF (IW(IoJCAD).EQ.IW(IoJTAU))  THEN
c                reset cylinder cycle counter
                   IW(IoJCAD) = 0
c                advance cylinder cycle count
                   IW(IoNCYD) = IW(IoNCYD+1)
                   ENDIF
c            increment counter
               IW(IoJCAD) = IW(IoJCAD) + 1
c            get index to next engine cycle angle
               IP1 = IP1XX(I)
c            save data at IP1 on last cycle to restore for RK2 step 2
               DO 27 N=1,4
                   IP1X = IP1 + 720*N
                   WQ(N,M) = RW(IoW+IP1X)
                   YQ(N,M) = RW(IoY+IP1X)
                   ZQ(N,M) = RW(IoZ+IP1X)
                   VQ(N,M) = RW(IoV+IP1X)
                   PQ(N,M) = RW(IoP+IP1X)
                   CQ(N,M) = RW(IoC+IP1X)
                   FQ(N,M) = RW(IoF+N)
                   JUMPQ(N,M) = IW(IoJUMP+IP1X)
                   LAGQ(N,M) = IW(IoLAG+N)
                   IVQ(N,M) = IW(IoIV+N)
                   IPQ(N,M) = IW(IoIP+N)
c                load null recycle jumps/locations
                   JUMPBQ(N,M) = 0

                   IJBQ(N,M) = 0
27                 CONTINUE
           ELSE
c            second RK2 step
               DTX = DT
c            restore angle to start of step 1
               IP1 = IW(IoI)
               I = IM1XX(IP1)
               IW(IoI) = I
c            restore values at IP1 to last-cycle values
               DO 37 N=1,4
                   IP1X = IP1 + 720*N
                   RW(IoW+IP1X) = WQ(N,M)
                   RW(IoY+IP1X) = YQ(N,M)
                   RW(IoZ+IP1X) = ZQ(N,M)
                   RW(IoV+IP1X) = VQ(N,M)
                   RW(IoP+IP1X) = PQ(N,M)
                   RW(IoC+IP1X) = CQ(N,M)
                   RW(IoF+N) = FQ(N,M)
                   IW(IoJUMP+IP1X) = JUMPQ(N,M)
                   IW(IoLAG+N) = LAGQ(N,M)
                   IW(IoIV+N) = IVQ(N,M)
                   IW(IoIP+N) = IPQ(N,M)
c                load any recycle jumps
                   IF (IJBQ(N,M).NE.0)
     ;                IW(IoJUMP+IJBQ(N,M)+720*N) = JUMPBQ(N,M)
37                 CONTINUE
           ENDIF
c
c    get mass fluxes from known flow rates and advance W
       DO 109 J=1,2
c        identify duct end states
           N1 = 2*(J - 1) + 1
           N2 = N1 + 1
           DO 107 N=N1,N2
               KS = KFLO(N)
               IX = I + 720*N
               IP1X = IP1 + 720*N
c            calculate mass flux
               CX = RW(IoC+IX)
               VX = RW(IoV+IX)
               RHO(N) = RCYXX(RW,CX,RW(IoY+IX))
               RW(IoG+N) = RHO(N)*VX
c            add to current cycle sums
               RW(IoF+N) = RW(IoF+N) + RW(IoG+N)
c            determine new cycle flow
               WNEW = RW(IoW+IX) + RW(IoG+N)*DTX
c            check for cycle flow reversal (a start-up problem)
               IF (WNEW.LE.RW(IoW+IP1X))  THEN
c            adjust the back history for zero reversal
                   RW(IoW+IP1X) = WNEW
                   ENDIF
c            save data at IP1 on last cycle for gap interpolation
               WLAST(N) = RW(IoW+IP1X)
               VLAST(N) = RW(IoV+IP1X)
               CLAST(N) = RW(IoC+IP1X)
               PLAST(N) = RW(IoP+IP1X)
c            record the new flow
               RW(IoW+IP1X) = WNEW
c            calculate the cycle flow
               RW(IoWCYC+N) = WNEW - WLAST(N)
c            check for change of V point (min Win)
               IV = IW(IoIV+N)
               IF (IV.EQ.IP1) THEN
c                IV was at beginning of current cycle; check jump
                   CALL JUMPXX(IW(IoJUMP+IV+720*N),JB,JF)
                   IF (JF.GT.0)  THEN
c                        jump ahead to next ok point
                           IW(IoIV+N) = IJPXX(IV,JF)
                       ELSE
c                        step ahead to next point
                           IW(IoIV+N) = IP1XX(IP1)
                       ENDIF
                   ENDIF
c            check the new point vs V
               WV = RW(IoW+IV+720*N)
               IF (KS*RW(IoW+IP1X).LT.KS*WV) THEN
c                 new point is V point
                   IW(IoIV+N) = IP1
                   ENDIF
c
c            check for change of P point (max Win)
               IP = IW(IoIP+N)
               IF (IP.EQ.IP1) THEN
c                IP was at beginning of current cycle; check jump
                   CALL JUMPXX(IW(IoJUMP+IP+720*N),JB,JF)
                   IF (JF.GT.0)  THEN
c                        jump ahead to next ok point
                           IW(IoIP+N) = IJPXX(IP,JF)
                       ELSE
c                        step ahead to next point
                           IW(IoIP+N) = IP1XX(IP1)
                       ENDIF
                   ENDIF
c            check the new point vs P
               WP = RW(IoW+IP+720*N)
               IF (KS*RW(IoW+IP1X).GT.KS*WP) THEN
c                 new point is P point
                   IW(IoIP+N) = IP1
                   ENDIF
c
c            see if the fluid at N entered at N
               IF (KS.EQ.1)  THEN
                       ESE(N) = .TRUE.
                   ELSE
                       IF (IV.EQ.IP1) THEN
                               ESE(N) = .TRUE.
                           ELSE
                               ESE(N) = .FALSE.
                           ENDIF
                   ENDIF
c
107            CONTINUE
c
109        CONTINUE
c
c    update of density sums used in Z correction
       IF (NRK2.EQ.1)  THEN
           DO 111 N=1,4
               RW(IoRSUM+N) = RW(IoRSUM+N) + RHO(N)
111            CONTINUE
           ENDIF
c
c  *  determine jump and Y for fluid that exited at NA having entered at NA
c
       DO 129 J=1,2
c        identify duct end states
           N1 = 2*(J - 1) + 1
           N2 = N1 + 1
c
c        set to get analyze N1
           NA = N1
           NO = N2
           MORE = .TRUE.
c
c      * duct loop point
c
120        KS = KFLO(NA)
           IP1XA = IP1+720*NA
           IP1XO = IP1+720*NO
           IXA = I + 720*NA
           IF (KS*RW(IoG+NA).GE.0)  THEN
c            inflow to duct at NA; set null jump
               IW(IoJUMP+IP1XA) = 0
c            set previous Y as trial for state determination
               Y(NA) = RW(IoY+IXA)
               GOTO 128
               ENDIF
c        outflow from duct at NA; set test mass/area
           W = RW(IoW+IP1XA)
c
c        branch on fluid entry point
           IF (.NOT.ESE(NA)) THEN
c            fluid entered at NO; set null jump
               IW(IoJUMP+IP1XA) = 0
               GOTO 128
               ENDIF
c        fluid entered at NA; initialize JUMP for search
           CALL JUMPXX(IW(IoJUMP+IXA),JB,JF)
           JUMP = JB - 1
c        set logicals
           BEFORE = .FALSE.
           AFTER = .FALSE.
           KTR = 0
c
c      * loop point in search for the before point
c
c        check JUMP for out of cycle
122        IF (KTR.GT.800)  THEN
               WRITE (IUMONM,*) ' Excessive iteration 1'
               GOTO 900
               ENDIF
           KTR = KTR + 1
c        check for backjump
           IF (JUMP.LE.-720) goto 126
c        check for forward jump
           IF (JUMP.GT.0)  THEN
c            error
               WRITE (IUMONM,123) IP1,NA,JUMP
123            FORMAT (' Forward jump: IP1,NA,JUMP=',3I4)
               GOTO 900
               ENDIF
c        get test point index
           IB = IJPXX(IP1,JUMP)
c        get jump at the test point
           CALL JUMPXX(IW(IoJUMP+IB+720*NA),JB,JF)
c        check test point
           IF (KS*RW(IoW+IB+720*NA).GE.KS*W)  THEN
c            test point is not before
               IF (JB.LT.0) THEN
c                    leap back
                       JUMP = JUMP + JB
                   ELSE
c                    creep back
                       JUMP = JUMP - 1
                   ENDIF
               GOTO 122
           ENDIF
c        before point found; set backward jump at IP1
          IW(IoJUMP+IP1XA) = JUMP
c        get old jump at IB
           JUMPB = IW(IoJUMP+IB+720*NA)
           CALL JUMPXX(JUMPB,JB,JF)
c        save old jump and location if on RK2 step 1
           IF (NRK2.EQ.1) THEN
               JUMPBQ(NA,M) = JUMPB
               IJBQ(NA,M) = IB
               ENDIF
c        load the new forward jump, retaining any existing back jump
           IW(IoJUMP+IB+720*NA) = JUMCXX(JB,-JUMP)
c        determine index IZ of point (ok or not) right after IB
           IZ = IP1XX(IB)
           IZX = IZ + 720*NA
           IBX = IB + 720*NA
c        compute interpolation coefficients
           WZ = RW(IoW+IZX)
           WB = RW(IoW+IBX)
           DW = WZ - WB
c        check for error
           IF (KS*DW.LE.0) THEN
c                check for real error
                   IF (ABS(DW).GT.ROERR*MAX(ABS(WB),ABS(WZ))) THEN
c                    real error
                       WRITE (IUMONM,124) IP1,NA
124                    FORMAT (' KS*DW < 0: IP1,NA=',2I3)
                       GOTO 900
                       ENDIF
c                within roundoff
                   HZ = 0.5
                   HB = 0.5
               ELSE
                   HZ = (W - WB)/DW
                   HB = 1 - HZ
               ENDIF
c        interpolation for entry entropy
           Y(NA) = HB*RW(IoY+IBX) + HZ*RW(IoY+IZX)
c         check for error
           IF (Y(NA).LE.0)  THEN
               WRITE (IUMONM,125)  I,J,NA
125            FORMAT (' Y < 0: I,J,NA=',3I6)
               GOTO 900
               ENDIF
c        friction change
           VY = RW(IoV+IXA)
           VX = HB*RW(IoV+IBX) + HZ*RW(IoV+IZX)
           DV = ABS(VX-VY)
           DELT = (- JB - HZ)*DTX
           CY = RW(IoC+IXA)
           CX = HB*RW(IoC+IBX) + HZ*RW(IoC+IZX)
           CAV = 0.5*(CX + CY)
           ARG = RW(IoCFCB+J)*DV*DV*DV*DELT/(CAV*CAV)
           Y(NA) = Y(NA)*EXP(ARG)
           GOTO 128
c
c        excessive backjump; use value at last cycle
126        Y(NA) = RW(IoY+IP1XA)
c        record the jump
           IW(IoJUMP+IP1XA) = - 720
c
c        check for both ends done
128        IF (MORE) THEN
c            set to analyze N2
               NA = N2
               NO = N1
               MORE = .FALSE.
               GOTO 120
               ENDIF
129        CONTINUE
c
c  *  determine lags and Y for any fluid that exited at NA having entered at NO
c
c      The following are used in searching the data for NO to find the W
c      set at NA:
c
c          LAG   is the absolute lag
c          LAGR  is the lag within 0-720 for the adjusted test W
c          NCLAG is the number of full cycles lagged
c
c          W     is the target W
c          WR    is the target W adjusted to 0-720 LAGR
c          W0    is the W for NO at IP1 (current)
c          WLAST is W at IP1 on the last cycle
c          WV    is the minimum W recorded I=1-720
c          WM    is the minimum W for the full cycle (WV or WLAST)
c
c          NCYCLE is such that   WV le WR lt W0
c
       DO 149 J=1,2
c        identify duct end states
           N1 = 2*(J - 1) + 1
           N2 = N1 + 1
c
c        set to analyze N1
           NA = N1
           NO = N2
           MORE = .TRUE.
c
c      * duct loop point
c
130        IP1XA = IP1+720*NA
           IP1XO = IP1+720*NO
           IXA = I + 720*NA
c        check flow direction at NA
           KS = KFLO(NA)
           IF (KS*RW(IoG+NA).GE.0)  THEN
c            inflow to duct at NA; set null lag and skip
               IW(IoLAG+NA) = 0
               GOTO 148
               ENDIF
c        outflow at NA; skip if flow entered at NA
           IF (ESE(NA))  GOTO 148
c        the fluid entered at NO; check KS
           KS = KFLO(NO)
           IF (KS.NE.1)  THEN
               WRITE (IUMONM,131)  IP1,NA
131            FORMAT (' Massive backflow; IP1,NA=',2I6)
               GOTO 900
               ENDIF
c        target W set at NA
           W = RW(IoW+IP1XA)
c        check data for NO
           W0 = RW(IoW+IP1XO)
           IF (W.GE.W0)  THEN
               IF (MONM.GT.0) WRITE (IUMONM,132) IP1,NA
132            FORMAT (' W > W0; IP1,NA=',2I6)
               GOTO 146
               ENDIF
c        get minimum W for the cycle
           IV = IW(IoIV+NO)
           IVX = IV + 720*NO
           WV = RW(IoW+IVX)
           IF (WLAST(NO).LE.WV)  THEN
                   WM = WLAST(NO)
               ELSE
                   WM = WV
               ENDIF
c        determine cycle lag
           IF (W.GE.WM) THEN
c                in current cycle
                   NCLAG = 0
                   WR = W
               ELSE
c                in a fictitious backcycle; determine cycle lag
                   DWC = RW(IoWCYC+NO)
                   IF (DWC.LE.0)  THEN
c                    reflux backcycle at NO (a start-up problem only)
                       WRITE (IUMONM,133) IW(IoNCYE),NRK2,M,I,NA,NO,DWC
133                    FORMAT (' Reflux: ',
     ;                ' NCYE,NRK2,M,I,NA,NO,DWC=',6I4,1PE12.4)
                       GOTO 146
                       ENDIF
c                compute cycles+
                   X =  (WM - W)/DWC
c                guard against integer lag overflow
                   IF (X.GT.29800) THEN
                       IF (MONM.GT.0) WRITE (IUMONM,134) X
134                    FORMAT(' Huge LAG; X=',1PE12.4)
                       GOTO 146
                       ENDIF
                   NX =  X
                   NCLAG  = 1 + NX
                   WR = W + NCLAG*DWC
c                reflux adjustment
                   IF (WR.GE.W0)  WR = WR - DWC
c                roundoff protection
                   IF (WR.LT.WM) WR = WM
                   IF (WR.GE.W0) WR = 0.999999*W0
               ENDIF
c
c        initialize for search
           KTR = 0
           LAGR = 0
c
c      * search loop point; searching back starting at IP1
c
136        IF (KTR.GT.722)  THEN
               WRITE (IUMONM,137) NA,NO,WLAST(NO),WM,WV,WR,W0
137            FORMAT(' Excessive lag search'//
     ;           '  NA,NO/WLAST(NO),WM,WV,WR,W0',2I3/5(1PE14.6))
               GOTO 900
               ENDIF
           KTR = KTR + 1
c
c        check for cycle start
           IF (LAGR.GE.720) THEN
c            if here it should be in the gap
               IF ((WM.LT.WV).AND.((WR.GE.WM).AND.(WR.LE.WV))) THEN
c                set to interpolate in the gap
                   WB = WLAST(NO)
                   CB = CLAST(NO)
                   VB = VLAST(NO)
                   PB = PLAST(NO)
                   IZ = IV
                   LAGR = 720
                   IW(IoLAG+NA) = LAGR + 720*NCLAG
                   GOTO 142
                   ENDIF
               WRITE (IUMONM,139) IP1,NA,NO,NCLAG,RW(IoWCYC+NO)
139            FORMAT (' Gap error: IP1,NA,NO,NCLAG,WCYCO=',4I6,1PE14.6)
               GOTO 900
               ENDIF
c
c        get index of test point
           IB = IJPXX(IP1,-LAGR)
           IBX = IB+720*NO
c        test the point
           IF (WR.LE.RW(IoW+IBX)) THEN
c            test point is after
               IF (IB.NE.IV)  THEN
c                    get jump at test point
                       CALL JUMPXX(IW(IoJUMP+IBX),JB,JF)
c                    increase lag seeking a before
                       IF (JB.LT.0)  THEN
c                            leap back
                               LAGR = LAGR - JB
                           ELSE
c                            creep back
                               LAGR = LAGR + 1
                           ENDIF
                       GOTO 136
                   ELSE
c                    set to interpolate in the gap
                       WB = WLAST(NO)
                       CB = CLAST(NO)
                       VB = VLAST(NO)
                       PB = PLAST(NO)
                       IZ = IV
                       LAGR = 720
                       IW(IoLAG+NA) = LAGR + 720*NCLAG
                       GOTO 142
                   ENDIF
               ENDIF
c        point IB is the interpolation base; record the lag
140        IW(IoLAG+NA) = LAGR + 720*NCLAG
c        set base values
           WB = RW(IoW+IBX)
           CB = RW(IoC+IBX)
           VB = RW(IoV+IBX)
           PB = RW(IoP+IBX)
c        get index IZ of next point after IB
           IZ = IP1XX(IB)
c
c        jump in here from gap
142        IZX = IZ + 720*NO
           WZ = RW(IoW+IZX)
           DW = WZ - WB
c        check for error
           IF (DW.LE.0)  THEN
c                check for real error
                   IF (ABS(DW).GT.ROERR*MAX(ABS(WB),ABS(WZ))) THEN
c                    real error
                       WRITE (IUMONM,143) IP1,NA
143                    FORMAT (' DW < 0: IP1,NA=',2I3)
                       GOTO 900
                       ENDIF
c                within roundoff
                   HZ = 0.5
                   HB = 0.5
               ELSE
                   HZ = (WR-WB)/DW
                   HB = 1 - HZ
               ENDIF
c        interpolate for V, C, and P at launch
           VX = HB*VB + HZ*RW(IoV+IZX)
           CX = HB*CB + HZ*RW(IoC+IZX)
           PX = HB*PB + HZ*RW(IoP+IZX)
c        construct Y at launch
           Y(NA) = YC2PXX(RW,CX*CX,PX)
c        make friction correction to exit entropy
           VY = RW(IoV+IXA)
           CY = RW(IoC+IXA)
           CAV = 0.5*(CX + CY)
           Y(NA) = Y(NA)*EXP(RW(IoCFCT+J)
     ;               *(VX*VX + VY*VY)/(CAV*CAV))
           GOTO 148
c
c        handling of W errors in startup
146        Y(NA) = RW(IoY+IP1XO)
           IW(IoLAG+NA) = 719
c        check for both ends done
148        IF (MORE) THEN
c            set to analyze N2
               NA = N2
               NO = N1
               MORE = .FALSE.
               GOTO 130
               ENDIF
c
c        Y work completed for duct J
149        CONTINUE
c
c  *  determine Z values at duct ends
c
       DO 159 J=1,2
c        identify duct end states
           N1 = 2*(J - 1) + 1
           N2 = N1 + 1
c
c        get launch data for - acoustic characteristic
           IF (NRK2.EQ.1)  THEN
c                extra half-step delay from IP1
                   TDPX = RW(IoTDP+J) + DTX
                   TDMX = RW(IoTDm+J) + DTX
               ELSE
                   TDPX = RW(IoTDP+J)
                   TDMX = RW(IoTDM+J)
               ENDIF
           CALL GINTXX(IP1,TDMX,IA1,IA2,CIA1,CIA2,IERR)
           IF (IERR.NE.0) THEN
               WRITE (IUMONM,*) ' Z- interpolation error'
               GOTO 900
               ENDIF
           V2X = CIA1*RW(IoV+IA1+720*N2)
     ;         + CIA2*RW(IoV+IA2+720*N2)
           C2X = CIA1*RW(IoC+IA1+720*N2)
     ;         + CIA2*RW(IoC+IA2+720*N2)
           P2X = CIA1*RW(IoP+IA1+720*N2)
     ;         + CIA2*RW(IoP+IA2+720*N2)
c        save launch data for characteristic arriving at N1
           RW(IoVL+N1) = V2X
           RW(IoCL+N1) = C2X
c        entropy correction (model)
           DZS = RW(IoTRKM)*(CPYXX(RW,P2X,Y(N1)) - C2X)
c        Z- corrected for friction and entropy change
           Z(N1) = RW(IoTRKM)*C2X - V2X + RW(IoDZF+N1) + DZS
c        corrrection for mass unbalance
           Z(N1) = Z(N1) + RW(IoDZC+N1)
c
c        get launch data for + acoustic characteristic
           CALL GINTXX(IP1,TDPX,IA1,IA2,CIA1,CIA2,IERR)
           IF (IERR.NE.0) THEN
               WRITE (IUMONM,*) ' Z+ interpolation error'
               GOTO 900
               ENDIF
           V1X = CIA1*RW(IoV+IA1+720*N1)
     ;         + CIA2*RW(IoV+IA2+720*N1)
           C1X = CIA1*RW(IoC+IA1+720*N1)
     ;         + CIA2*RW(IoC+IA2+720*N1)
           P1X = CIA1*RW(IoP+IA1+720*N1)
     ;         + CIA2*RW(IoP+IA2+720*N1)
c        save launch data for characteristic arriving at N2
           RW(IoVL+N2) = V1X
           RW(IoCL+N2) = C1X
c        entropy correction (model)
           DZS = RW(IoTRKM)*(CPYXX(RW,P1X,Y(N2)) - C1X)
c        Z+  corrected for friction and entropy change
           Z(N2) = RW(IoTRKM)*C1X + V1X + RW(IoDZF+N2) + DZS
c        correction for  mass balance
           Z(N2) = Z(N2) + RW(IoDZC+N2)
c
c        load the Y and Z values for the duct (half-point or full point)
           DO 153 N=N1,N2
               IP1X = IP1 + 720*N
               RW(IoY+IP1X) = Y(N)
               RW(IoZ+IP1X) = Z(N)
153            CONTINUE
c
c        check for update of correction factors for the next cycle
           IF ((IP1.EQ.IBEG).AND.(NRK2.EQ.2)) THEN
c            compute Z correction factor for the next cycle
               DZC = (RW(IoF+N1) - RW(IoF+N2))/RW(IoRSUM+N1)
               RW(IoDZC+N1) = RW(IoDZC+N1) + DZC
               RW(IoDZC+N2) = 0
c            inflow excess this cycle
               RW(IoDWC+J) = (RW(IoF+N1) - RW(IoF+N2))*DTX
c            shift in W at N1 and N2 to retain accuracy in difference
               WX = RW(IoW+IBEG+720*N2)
c            modify the W record
               DO 157 IZ =1,720
                   DO 155 N=N1,N2
                       IX = IoW + IZ + 720*N
c                    accuracy shift
                       RW(IX) = RW(IX) - WX
c                    purge extra inflow
                       IF (N.EQ.N2)  RW(IX) = RW(IX)+RW(IoDWC+J)
155                   CONTINUE
157               CONTINUE
               ENDIF
c        work completed for the duct
159        CONTINUE
c
c    update phase average Y and Z for the runners
       IF ((IW(IoNCYL).GT.1).AND.(NRK2.EQ.2))  THEN
c        set manifold states
           IF (M.EQ.1)  THEN
                   JF = 1
                   JR = 2
                   NF = 1
                   NR = 3
                   ND = 4
               ELSE
                   JF = 2
                   JR = 1
                   NF = 4
                   NR = 2
                   ND = 1
               ENDIF
c        compute new phase averages
           YPAV = EXP( RW(IoFAPO)*LOG(RW(IoYPAV+IP1))
     ;               + RW(IoFAPN)*LOG(Y(NR)) )
           ZPAV = RW(IoFAPO)*RW(IoZPAV+IP1)
     ;                     + RW(IoFAPN)*Z(NR)
c        load new values at all phases
           DO 167 L=1,IW(IoNCYL)
               ID = IJPXX(IP1,L*IW(IoJTAU))
               RW(IoYPAV+ID) = YPAV
               RW(IoZPAV+ID) = ZPAV
167            CONTINUE
c
c        check for update of Z correction for other runners
           IF ( (IP1.EQ.IBEG).AND.(IW(IoNCYE).GT.5) ) THEN
c            Z correction to uncomputed runners to improve mass balance
               DZCO = (RW(IoF+NF)*RW(IoAD+JF)
     ;                 - RW(IoF+ND)*RW(IoAD+JR)*IW(IoNCYL))/
     ;                   (RW(IoRSUM+NR)*RW(IoAD+JR)*(IW(IoNCYL) - 1))
               RW(IoDZCO) = RW(IoDZCO) + 0.2*DZCO/IW(IoNCYL)
               ENDIF
c
           ENDIF
c
c    advance angle
       IW(IoI) = IP1
c
c    ok return
       IERR = 0
       RETURN
c
c  *  fatal error occurred; write diagnostic data
c
900    WRITE (IUMONM,902) IERR,I,IW(IoNCYE)
902    FORMAT (/' Dump following ADYZ error',I2,' at I=',I3,
     ;           ' on cycle ',I3)
       DO 903 IX=IP1,1,-1
           WRITE (IUMONM,906) IX,(IW(IoJUMP+IX+720*N),N=1,4),
     ;           (RW(IoW+IX+720*N),N=1,4)
903        CONTINUE
       IP2 = IP1XX(IP1)
       DO 904 IX=720,IP2,-1
           WRITE(IUMONM,906) IX,(IW(IoJUMP+IX+720*N),N=1,4),
     ;           (RW(IoW+IX+720*N),N=1,4)
904        CONTINUE
       WRITE(IUMONM,906) IP1,(-22222,N=1,4),(WLAST(N),N=1,4)
906    FORMAT (I4,' JW',4I5,4(1PE13.6))
       CLOSE (IUMONM)
       OPEN (IUMONM,FILE='MANTEMP.MON')
c    instruct user
       CALL WARNZZ('#','#The manifold flow routine '//
     ;   'failed.  File MANIFOLD.MON '//
     ;   'contains data needed to diagnose and fix the problem.  '//
     ;   'If your input data seem ok, please quit ESP now and save '//
     ;   'this file and your setup file '//
     ;   'in a separate directory (so that they will '//
     ;   'not be overwritten by your next ESP run) and contact# #'//
     ;  '    wcr@thermo.stanford.edu# #'//
     ;  'to arrange for transfer of these files so that we can '//
     ;  'make ESP work better.  Do not email these files!##')
       IERR = 1
       RETURN
       END
c*****************************************************************************
c
       FUNCTION JUMCXX(JB,JF)
c
c      Encodes the forward and backward jumps JF>0 and JB<0
c------------------------------------------------------------------------------
       IF (JB.EQ.0)  THEN
               JUMCXX = JF
           ELSEIF (JF.EQ.0) THEN
               JUMCXX = JB
           ELSE
               JUMCXX = 1440*JF - JB
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE JUMPXX(JUMC,JB,JF)
c
c      Decodes jump code JUMC, returning the forward and backward jumps JF>0
c      and JB<0.
c------------------------------------------------------------------------------
       IF (JUMC.GT.720)  THEN
               JF = JUMC/1440
               JB =  1440*JF - JUMC
           ELSEIF (JUMC.LT.0) THEN
               JB = JUMC
               JF = 0
           ELSE
               JB = 0
               JF = JUMC
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE ADTDXX(RW,IW,NRK2)
c
c      Adavnces the acoustic time delays for the RK2 process and calculates
c      friction corrections to Z.
c
c      Step 1:
c        Advances from I to I + 1/2
c
c      Step 2:
c        Advances DT from  I to I+1
c
c      Must be called after the duct states have been advanced.

c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 3004)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoK   ,MPTR(1))
       EQUIVALENCE (IoKM1 ,MPTR(2))
       EQUIVALENCE (IoKPKM,MPTR(5))
       EQUIVALENCE (IoTRKM,MPTR(16))
       EQUIVALENCE (IoSCKF,MPTR(32))
       EQUIVALENCE (IoFTDO,MPTR(41))
       EQUIVALENCE (IoFTDN,MPTR(42))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoY   ,MPTR(46))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoZ   ,MPTR(49))
       EQUIVALENCE (IoDZF ,MPTR(55))
       EQUIVALENCE (IoCL  ,MPTR(56))
       EQUIVALENCE (IoVL  ,MPTR(57))
       EQUIVALENCE (IoLD  ,MPTR(64))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoTDP ,MPTR(66))
       EQUIVALENCE (IoTDM ,MPTR(67))
       EQUIVALENCE (IoTDPA,MPTR(68))
       EQUIVALENCE (IoTDMA,MPTR(69))
       EQUIVALENCE (IoCFCB,MPTR(72))
       EQUIVALENCE (IoCFCT,MPTR(73))
       EQUIVALENCE (IoCFA ,MPTR(74))
       EQUIVALENCE (IoFTEO,MPTR(75))
       EQUIVALENCE (IoFTEN,MPTR(76))
       EQUIVALENCE (IoJCAD,MPTR(85))
       EQUIVALENCE (IoIBEG,MPTR(89))
       EQUIVALENCE (IoMAN ,MPTR(90))
       EQUIVALENCE (IoI   ,MPTR(91))
c-----------------------------------------------------------------------------
c    time step
       COMMON /DTXX/DT
c-----------------------------------------------------------------------------
c    rounding down for time delays
       DATA  RDN /0.999/
c------------------------------------------------------------------------------
c    RK2 reset data
       DIMENSION   TDMQ(2,2),TDPQ(2,2)
       COMMON /RK2TXX/TDMQ,TDPQ
c------------------------------------------------------------------------------
c    get manifold
       M = IW(IoMAN)
c    get new angle index
       I = IW(IoI)
c
c    do both ducts
       DO 185 J=1,2
c        identify duct end states
           N1 = 2*(J - 1) + 1
           N2 = N1 + 1
c
c    check RK2 step
       IF (NRK2.EQ.1)   THEN
c            step 1
               DTX = DT/2
c            check for reset of time delays at start of period
               IF (J.EQ.M)  THEN
c                    cylinder averaging
                       IF (IW(IoJCAD).EQ.1) THEN
c                        average time delays and reset duct cycle average
                           RW(IoTDMA+J) = RW(IoFTDO)*RW(IoTDMA+J)
     ;                                  + RW(IoFTDN)*RW(IoTDM+J)
                           RW(IoTDPA+J) = RW(IoFTDO)*RW(IoTDPA+J)
     ;                                  + RW(IoFTDN)*RW(IoTDP+J)
                           RW(IoTDP+J) = RW(IoTDPA+J)
                           RW(IoTDM+J) = RW(IoTDMA+J)
                           ENDIF
                   ELSE
                       IF (I.EQ.IW(IoIBEG)) THEN
c                        reset time delays to engine cycle average
                           RW(IoTDMA+J) = RW(IoFTEO)*RW(IoTDMA+J)
     ;                                  + RW(IoFTEN)*RW(IoTDM+J)
                           RW(IoTDPA+J) = RW(IoFTEO)*RW(IoTDPA+J)
     ;                                  + RW(IoFTEN)*RW(IoTDP+J)
                           RW(IoTDP+J) = RW(IoTDPA+J)
                           RW(IoTDM+J) = RW(IoTDMA+J)
                           ENDIF
                   ENDIF
c            save old delays to reset for step 2
               TDMQ(J,M) = RW(IoTDM+J)
               TDPQ(J,M) = RW(IoTDP+J)
           ELSE
c            step2
               DTX = DT
c            recover old delays
               RW(IoTDM+J) = TDMQ(J,M)
               RW(IoTDP+J) = TDPQ(J,M)
           ENDIF
c
c  *   advance - acoustic time delay and calculate friction term
c
c        launch data
           V1 = RW(IoVL+N1)
           C1 = RW(IoCL+N1)
           CMV1 = C1 - V1
c        arrival data
           IX = I+720*N1
           V2 = RW(IoV+IX)
           C2 = RW(IoC+IX)
           Z2 = RW(IoZ+IX)
           CMV2 = C2 - V2
c        max time delay and friction
           TDMAX = RW(IoTDM+J) + DTX
           DZFMAX = (RW(IoTRKM) + 1)*C2 - Z2
c        check headwind
           IF ((CMV1.LE.0).OR.(CMV2.LE.0)) THEN
c                sonic headwind
                   RW(IoTDM+J) = RDN*TDMAX
                   RW(IoDZF+N1) = DZFMAX
                   DTMDT = 1
               ELSE
c                subsonic headwind
                   TERM = 0.5*RW(IoLD+J)*(1/CMV1 + 1/CMV2)
                   DTMDT = (1 - RW(IoTDM+J)/TERM)
                   RW(IoTDM+J) = RW(IoTDM+J) + DTX*DTMDT
                   IF (RW(IoTDM+J).GT.TDMAX) RW(IoTDM+J) = TDMAX
                   IF (RW(IoTDM+J).LT.DTX) RW(IoTDM+J) = DTX
                   TERM1 = V1*ABS(V1)*(RW(IoKM1)*V1/C1 + 1)
                   TERM2 = V2*ABS(V2)*(RW(IoKM1)*V2/C2 + 1)
                   CAV = 0.5*(C1 + C2)
                   RW(IoDZF+N1) =
     ;                     RW(IoCFA+J)*(TERM1 + TERM2)*RW(IoTDM+J)
                   IF (ABS(RW(IoDZF+N1)).GT.ABS(DZFMAX))
     ;                   RW(IoDZF+N1) = DZFMAX
               ENDIF
c
c   *   advance + acoustic time delay and calculate friction term
c
c        launch data
           V1 = RW(IoVL+N2)
           C1 = RW(IoCL+N2)
           CPV1 = C1 + V1
c        arrival data
           IX = I+720*N2
           V2 = RW(IoV+IX)
           C2 = RW(IoC+IX)
           Z2 = RW(IoZ+IX)
           CPV2 = C2 + V2
c        max time delay and friction
           TDMAX = RW(IoTDP+J) + DTX
           DZFMAX = (RW(IoTRKM) - 1)*C2 - Z2
c        check headwind
           IF ((CPV1.LE.0).OR.(CPV2.LE.0)) THEN
c                sonic headwind
                   RW(IoTDP+J) = RDN*TDMAX
                   RW(IoDZF+N2) = DZFMAX
                   DTPDT = 1
               ELSE
c                subsonic headwind
                   TERM = 0.5*RW(IoLD+J)*(1/CPV1 + 1/CPV2)
                   DTPDT = 1 - RW(IoTDP+J)/TERM
                   RW(IoTDP+J) = RW(IoTDP+J) + DTX*DTPDT
                   IF (RW(IoTDP+J).GT.TDMAX) RW(IoTDP+J) = TDMAX
                   IF (RW(IoTDP+J).LT.DTX) RW(IoTDP+J) = DTX
                   CAV = 0.5*(C1 + C2)
                   TERM1 = V1*ABS(V1)*(RW(IoKM1)*V1/C1 - 1)
                   TERM2 = V1*ABS(V2)*(RW(IoKM1)*V2/C2 - 1)
                   RW(IoDZF+N2) =
     ;                     RW(IoCFA+J)*(TERM1 + TERM2)*RW(IoTDP+J)
                   IF (ABS(RW(IoDZF+N2)).GT.ABS(DZFMAX))
     ;                     RW(IoDZF+N2) = DZFMAX
               ENDIF
185        CONTINUE
c
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE IMFSXX(RW,IW)
c
c      Increments FOUT after advance of states.
c      At call, everything must be known at angle index IW(IoI).
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 3004)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
       EQUIVALENCE (IoFI  ,MPTR(52))
       EQUIVALENCE (IoFO  ,MPTR(54))
       EQUIVALENCE (IoFOUT,MPTR(83))
       EQUIVALENCE (IoI   ,MPTR(91))
c------------------------------------------------------------------------------
c        get advanced angle
           I = IW(IoI)
c        increment cycle outflow
           RW(IoFOUT) = RW(IoFOUT) + (RW(IoFO+I) - RW(IoFI+I))
           RETURN
           END
c******************************************************************************
c
       SUBROUTINE EMBCXX(RW)
c
c      Converts the manifold flux sums to flow rates at end of a cycle.
c      Returns the overall manifold mass balance error EMB.
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
       EQUIVALENCE (IoF   ,MPTR(61))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoFOUT,MPTR(83))
       EQUIVALENCE (IoEMB ,MPTR(79))
c------------------------------------------------------------------------------
c    time step
       COMMON /DTXX/DT
c-----------------------------------------------------------------------------
c    output calculations
       RW(IoFOUT) = RW(IoFOUT)*DT
       DO 9 J=1,2
c            identify duct end states
               N1 = 2*(J - 1) + 1
               N2 = N1 + 1
c            convert sums to mass flow rates
               DO 7 N=N1,N2
                   RW(IoF+N) = RW(IoF+N)*RW(IoAD+J)*DT
7                  CONTINUE
9              CONTINUE
       RW(IoEMB) = RW(IoFOUT) + RW(IoF+4) - RW(IoF+1)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CPERXX(RW,IW)
c
c      Phase-averages C,V,and P and makes them cylinder-periodic in the
c      feeder/collector for the current angle.
c------------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 2904)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
c------------------------------------------------------------------------------
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoJTAU,MPTR(84))
       EQUIVALENCE (IoNCYL,MPTR(88))
       EQUIVALENCE (IoMAN ,MPTR(90))
       EQUIVALENCE (IoI   ,MPTR(91))
c------------------------------------------------------------------------------
c    skip for single cylinder
       IF (IW(IoNCYL).EQ.1)  RETURN
c    get current index
       I = IW(IoI)
c    set the states
       IF (IW(IoMAN).EQ.1)  THEN
c            intake feeder
               N1 = 1
               N2 = 2
           ELSE
c            exhaust collector
               N1 = 3
               N2 = 4
           ENDIF
c    enforce the periodicity
       DO 29 N=N1,N2
           IX = I + 720*N
c        compute phase average
           CSUM = 0
           VSUM = 0
           PSUM = 0
           DO 9 J=1,IW(IoNCYL)
               ID = IJPXX(I,J*IW(IoJTAU))
               IDX = ID + 720*N
               CSUM = CSUM + RW(IoC+IDX)
               VSUM = VSUM + RW(IoV+IDX)
               PSUM = PSUM + RW(IoP+IDX)
9              CONTINUE
           CX = CSUM/IW(IoNCYL)
           VX = VSUM/IW(IoNCYL)
           PX = PSUM/IW(IoNCYL)
c        make cylinder-periodic
           DO 19 J=1,IW(IoNCYL)
               ID = IJPXX(I,J*IW(IoJTAU))
               IDX = ID + 720*N
               RW(IoC+IDX) = CX
               RW(IoV+IDX) = VX
               RW(IoP+IDX) = PX
19             CONTINUE
29         CONTINUE
       RETURN
       END
c******************************************************************************

