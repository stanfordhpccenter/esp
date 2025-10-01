c******************************************************************************
c
c      Manifold junction support routines
c
c******************************************************************************
c
       SUBROUTINE OTHRXX(RW,IW,NRK2)
c
c      Computes other runner flows at angle RIW(IoI)
c----------------------------------------------------------------------------
       PARAMETER           (NMPTR = 96)
       PARAMETER           (NDRW = 21024)
       PARAMETER           (NDIW = 2904)
       DIMENSION           MPTR(NMPTR)
       DIMENSION           RW(NDRW)
       DIMENSION           IW(NDIW)
       COMMON  /MPTRXX/    MPTR
c------------------------------------------------------------------------------
       EQUIVALENCE (IoKM1K,MPTR(9))
       EQUIVALENCE (IoRKM1,MPTR(13))
       EQUIVALENCE (IoC   ,MPTR(44))
       EQUIVALENCE (IoV   ,MPTR(45))
       EQUIVALENCE (IoP   ,MPTR(47))
       EQUIVALENCE (IoP0J ,MPTR(50))
       EQUIVALENCE (IoFI  ,MPTR(52))
       EQUIVALENCE (IoQI  ,MPTR(53))
       EQUIVALENCE (IoFO  ,MPTR(54))
       EQUIVALENCE (IoAD  ,MPTR(65))
       EQUIVALENCE (IoFAVO,MPTR(75))
       EQUIVALENCE (IoFAVN,MPTR(76))
       EQUIVALENCE (IoJTAU,MPTR(84))
       EQUIVALENCE (IoNCYE,MPTR(86))
       EQUIVALENCE (IoNCYL,MPTR(88))
       EQUIVALENCE (IoMAN ,MPTR(90))
       EQUIVALENCE (IoI   ,MPTR(91))
c-----------------------------------------------------------------------------
c    RK2 restoration data
       DIMENSION FIH(2),QIH(2),FOH(2)
       COMMON  /RK2OXX/FIH,QIH,FOH
c-----------------------------------------------------------------------------
       I = IW(IoI)
       M = IW(IoMAN)
c        check for multicylinder
           IF (IW(IoNCYL).GT.1) THEN
c                multi-cylinder
                   if (M.EQ.1) THEN
c                        intake
                           JR = 2
                           NR = 3
                           KS = 1
                       ELSE
c                        exhaust
                           JR = 1
                           NR = 2
                           KS = -1
                       ENDIF
c                compute flows for other cylinders
                   FIS = 0
                   QIS = 0
                   FOS = 0
                   DO 9 J=1,IW(IoNCYL)-1
                       ID = IJPXX(I,-IW(IoJTAU)*J)
                       IX = ID+720*NR
                       VX = RW(IoV+IX)
                       CX = RW(IoC+IX)
                       PX = RW(IoP+IX)
                       P0JX = RW(IoP0J+ID)
                       FO = KS*RW(IoAD+JR)*RCPXX(RW,CX,PX)*VX
                       IF (FO.LT.0)  THEN
c                            inflow to junction
                               H0X = RW(IoRKM1)*CX*CX + 0.5*VX*VX
                               FIS = FIS - FO




C                              QIS = QIS - FO*H0X/P0JX**RW(IoKM1K)

                               QIS = QIS - FO*H0X




                           ELSE
c                            outflow from junction
                               FOS =  FOS + FO
                           ENDIF
9                      CONTINUE


                   IF (NRK2.EQ.1)  THEN
c                        save averages for RK2 step 2 restoration
                           FIH(M) = RW(IoFI+I)
                           QIH(M) = RW(IoQI+I)
                           FOH(M) = RW(IoFO+I)
c                        get averages at previous angle
                           IM1 = IM1XX(I)
                           FIP = RW(IoFI+IM1)
                           QIP = RW(IoQI+IM1)
                           FOP = RW(IoFO+IM1)
c                        interpolate to the half angle
                           FIS = 0.5*(FIS+FIP)
                           QIS = 0.5*(QIS+QIP)
                           FOS = 0.5*(FOS+FOP)
                       ELSE
c                        restore values
                           RW(IoFI+I) = FIH(M)
                           RW(IoQI+I) = QIH(M)
                           RW(IoFO+I) = FOH(M)
                       ENDIF
c                average with previous engine cycles
                   RW(IoFI+I) = RW(IoFAVO)*RW(IoFI+I) + RW(IoFAVN)*FIS
                   RW(IoQI+I) = RW(IoFAVO)*RW(IoQI+I) + RW(IoFAVN)*QIS
                   RW(IoFO+I) = RW(IoFAVO)*RW(IoFO+I) + RW(IoFAVN)*FOS
               ELSE
c                single cylinder
                   RW(IoFI+I) = 0
                   RW(IoQI+I) = 0
                   RW(IoFO+I) = 0
               ENDIF
           RETURN
           END
c*****************************************************************************

