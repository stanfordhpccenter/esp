c******************************************************************************
c
c      Flow rate routines
c
c******************************************************************************
c
       SUBROUTINE FLOWVV(A,CD,D0,GAM,P0,PB,V,FLOW)
c
c      Calculates mass flow rate through a valve when no manifold
c------------------------------------------------------------------------------
c      Arguments loaded at call:
c          A       discharge area, m**2
c          CD      discharge coefficient (actual flow/isentropic flow)
c          D0      stagnation density, kg/m**3
c          GAM     specific heat ratio Cp/Cv
c          P0      stagnation pressure, Pa
c          PB      back pressure, PA
c      Arguments returned:
c          V       discharge velocity (isentropic core flow), m/s
c          FLOW    mass flow rate, kg/s
c------------------------------------------------------------------------------
c      Variables used in this subroutine
c          C1      (GAM - 1)/GAM
c          E       (P0 - PE)/P0 ( when small)
c          PE      exit plane pressure, Pa
c          TERM    1 - (PE/P0)**C1
c------------------------------------------------------------------------------
c    check flow regime
       C1 = (GAM - 1)/GAM
       PC = P0/(((GAM+1)/2)**(1/C1))
       IF (PB.GT.PC)  THEN
c            subcritical flow
               PE = PB
           ELSE
c            critical flow
               PE = PC
           ENDIF
c    calculate the driving pressure term
       PR = PE/P0
       IF (PR.LT.0.97)  THEN
               TERM = 1 - PR**C1
           ELSE
c            series expansion for accuracy at small flow rates
               E = (P0 - PE)/P0
               TERM = C1*E*(1 - 0.5*(C1-1)*E*(1 - (C1-2)*E/3))
           ENDIF
c    compute isentropic velocity
       TERM =  2*P0*TERM/(D0*C1)

       if (term.lt.0) write (*,*) 'c8:1 term,c1,e,p0 ',term,c1,e,p0

       V = SQRT(TERM)
c    compute mass flow rate
       FLOW = CD*A*V*D0*PR**(1/GAM)
       RETURN
       END
c******************************************************************************

