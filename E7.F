c******************************************************************************
c
c      Untilty routines
c
c******************************************************************************
c
       DOUBLE PRECISION FUNCTION SJUEXP(X)
c
c      Special DEXP routine.
c      A call must be made before use with X = HUGE to set HUGE.
c------------------------------------------------------------------------------
c      Nomenclature:
c          HUGE        nearly-largest machine number
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
c------------------------------------------------------------------------------
c    Nearly-largest REAL*8 number:
       PARAMETER (HUGEN = 1.D100)
       COMMON /SJHUGE/HUGE,EAMIN,EAMAX
c------------------------------------------------------------------------------
       IF (X.GT.EAMAX) THEN
           SJUEXP = HUGE
           RETURN
           ENDIF
       IF (X.LT.EAMIN)  THEN
           SJUEXP = 0
           RETURN
           ENDIF
       SJUEXP = DEXP(X)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SJULES(NDIM,A,V,N,IERR)
c
c          Double precision linear algebraic equation solver.
c
c          Solves A(I,J)*X(J) = Y(I)  I = 1,...,N
c
c          FROUND is a round-off test factor assuming at least 15 digit accuracy
c
c          Uses Gaussian elimination with row normalization and selection.
c
c      At entry:
c          NDIM is the dimension of the A and V arrays
c          Y is in V
c
c      On return:
c
c          Solution ok:
c              IERR = 0
c              X is in V
c              A is destroyed
c
c          Singular matrix or bad dimensioning:
c              IERR = 1
c              A and V are destroyed
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
c-----------------------------------------------------------------------------
       DIMENSION   A(NDIM,NDIM),V(NDIM)
c-----------------------------------------------------------------------------
       DATA     FROUND/1.D-15/
       DATA     ZERO/0.D0/
c-----------------------------------------------------------------------------
c -- Check dimensions
c
       IF (N.GT.NDIM)  THEN
c        dimensioning error: treat as program error in EQUIL calls
           IERR = 1
           RETURN
           ENDIF
c
c -- form the lower triagonal system
c
       IF (N.GT.1)  THEN
           NM1 = N - 1
           DO 199 K=1,NM1
c            eliminate the last column in the upper M x M matrix
               M = N - K + 1
               MM1 = M - 1
c
c            normalize each row on its largest emement
c
               DO 119 I=1,M
                   C = 0
                   DO 111 J=1,M
                       CX = DABS(A(I,J))
                       IF (CX.GT.C)  C = CX
111                    CONTINUE
c                check for singular matrix
                   IF (C.EQ.ZERO)  THEN
                       IERR = 1
                       RETURN
                       ENDIF
                   C = 1/C
                   DO 113 J=1,M
                       A(I,J) = A(I,J)*C
113                     CONTINUE
                   V(I) = V(I)*C
119                CONTINUE
c            find the best row IX to eliminate in column M
               C = 0
               DO 121 I=1,M
                   CX=DABS(A(I,M))
                   IF (CX.GT.C)  THEN
                       C = CX
                       IX = I
                       ENDIF
121                CONTINUE
c            check for singular matrix
               IF (C.EQ.ZERO)  THEN
                   IERR = 1
                   RETURN
                   ENDIF
               IF (M.NE.IX)  THEN
c                  switch rows M and IX
                   C = V(M)
                   V(M) = V(IX)
                   V(IX) = C
                   DO 123 J=1,M
                       C = A(M,J)
                       A(M,J) = A(IX,J)
                       A(IX,J) = C
123                    CONTINUE
                   ENDIF
c
c          eliminate last column using the lowest row in the M x M matrix
c
c            check for singular matrix
               IF (A(M,M).EQ.ZERO)  THEN
                   IERR = 1
                   RETURN
                   ENDIF
c            column loop
               DO 139 I=1,MM1
c                check for column entry
                   IF (A(I,M).NE.ZERO)  THEN
c                    eliminate
                       C = A(I,M)/A(M,M)
                       D = V(I) - C*V(M)
                       IF (DABS(D).LT.FROUND*DABS(V(I)))  D = 0
                       V(I) = D
                       DO 131 J=1,MM1
                           D = A(I,J) - C*A(M,J)
                           IF (DABS(D).LT.FROUND*DABS(A(I,J)))  D = 0
                           A(I,J) = D
131                        CONTINUE
                       ENDIF
139                CONTINUE
199            CONTINUE
           ENDIF
c
c -- compute the back solution
c
c    check for singular matrix
       IF (A(1,1).EQ.ZERO) THEN
           IERR = 1
           RETURN
           ENDIF
c    calculate X(1)
       V(1) =  V(1)/A(1,1)
       IF (N.GT.1)  THEN
           DO 229 I=2,N
c            calculate X(I)
               IM1 = I - 1
               C = V(I)
               TERMB = DABS(C)
               DO 219 J=1,IM1
                   TERM = A(I,J)*V(J)
                   C = C - TERM
                   IF (DABS(TERM).GT.TERMB)  TERMB = DABS(TERM)
219                CONTINUE
               IF (DABS(C).LT.FROUND*TERMB)  C = 0
               V(I) = C/A(I,I)
229            CONTINUE
           ENDIF
c
c -- normal exit
c
       IERR = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SJUMAX(A,B)
c
c      Sets B = max{|A|,B}
c-----------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
c-----------------------------------------------------------------------------
       IF (DABS(A).GT.B)  B = DABS(A)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SJURND(A,B)
c
c      Rounds A to zero if |A|< FRND*B.
c-----------------------------------------------------------------------------
       IMPLICIT    REAL*8  (A-H,O-Z)
c-----------------------------------------------------------------------------
       DATA    FRND/1.D-13/
       IF (DABS(A).LT.FRND*B)  A = 0
       RETURN
       END
c******************************************************************************
