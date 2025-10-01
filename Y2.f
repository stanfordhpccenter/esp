c******************************************************************************
c
c      Manifold subroutines for position control
c
c******************************************************************************
c
       INTEGER FUNCTION IP1XX(I)
c
c      Returns next index (recycles at 720)
c------------------------------------------------------------------------------
       IF (I.EQ.720)  THEN
               IP1XX = 1
           ELSE
               IP1XX = I + 1
           ENDIF
       RETURN
       END
c******************************************************************************
c
       INTEGER FUNCTION IM1XX(I)
c
c      Returns previous index (recycles at 1)
c------------------------------------------------------------------------------
       IF (I.EQ.1)  THEN
               IM1XX = 720
           ELSE
               IM1XX = I - 1
           ENDIF
       RETURN
       END
c******************************************************************************
c
       INTEGER FUNCTION IJPXX(I,J)
c
c      Returns index of I jumped by J; returns in range 1 - 720
c------------------------------------------------------------------------------
       IJPXX = I + J
2      IF (IJPXX.LT.1)  THEN
               IJPXX = IJPXX + 720
               GOTO 2
           ELSEIF (IJPXX.GT.720)  THEN
               IJPXX = IJPXX - 720
               GOTO 2
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE GINTXX(I,TD,IA1,IA2,CIA1,CIA2,IERR)
c
c      Determines interpolation location and interpolation factors
c
c      I           current index in 720 entry table
c      TD          time delay
c      IA1, IA2    indices bounding the retarded interpolation point
c      CIA1, CIA2  coefficients of the tabulated values in the interpolation
c                  formula  Q(DELAY) = CIA1*Q(IA1) + CIA2*Q(IA2)
c
c      DT time difference per tabulation point (crank angle)
c
c      Terminates if delay is negative
c------------------------------------------------------------------------------
c    time step
       COMMON /DTXX/ DT
c------------------------------------------------------------------------------
c    diagnostic monitor
       LOGICAL OUT
       COMMON /MONMXX/IUMONM,MONM,OUT
c------------------------------------------------------------------------------
c    real constants common to both manifolds
       IF (TD.LT.0)  THEN
           WRITE (IUMONM,3) I
3          FORMAT (' Negative time delay for I=',I4)
           CALL WARNZZ('@','@Manifold error; negative time delay. '//
     ;                     'run terminated.@@')
           IERR = 1
           RETURN
           ENDIF
c    index delay (floating)
       DELAY = TD/DT
c    delay to point after
       IDELAY = DELAY
c    indices to points after and before
       IA2 = IJPXX(I,-IDELAY)
       IA1 = IM1XX(IA2)
c    factors
       CIA1 = DELAY - IDELAY
       CIA2 = 1 - CIA1
       IERR = 0
       RETURN
       END
c******************************************************************************

