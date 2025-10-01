c******************************************************************************
c
c      Chemical and atom name processing routines
c
c******************************************************************************
c
       SUBROUTINE SJSCHA(NAMAX,ATOMX,ATOM,I1,I2,I)
c
c      Checks to see if ATOMX is in the list ATOM(I),I=I1,I2.
c      Returns I if found, I = 0 if not.
c-----------------------------------------------------------------------------
       CHARACTER*2 ATOMX,ATOM
c------------------------------------------------------------------------------
       DIMENSION   ATOM(NAMAX)
c-----------------------------------------------------------------------------
       IF (I2.GE.I1)  THEN
           DO 9 I=I1,I2
               IF (ATOMX.EQ.ATOM(I))  RETURN
9              CONTINUE
           ENDIF
       I = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SJSCHC(NSMAX,CHEMX,CHEM,J1,J2,J)
c
c      Checks to see if CHEM is in the list (CHEM(J),J1,J2).
c      Returns J if found, J=0 otherwise.
c-----------------------------------------------------------------------------
       CHARACTER*8 CHEMX,CHEM
c------------------------------------------------------------------------------
       DIMENSION   CHEM(NSMAX)
c-----------------------------------------------------------------------------
       IF (J2.GE.J1)  THEN
           DO J=J1,J2
               IF (CHEMX.EQ.CHEM(J))  RETURN
               ENDDO
           ENDIF
       J = 0
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  SJSCVC(CHEMI,CHEM)
c
c      Converts chemical name CHEMI (a string of up to 8 bytes),
c      to CHEM, which may be regarded externally as a CHARACTER*8 string.
c      CHEMI is ended after its first blank or comma.
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8      (A-H,O-Z)
       CHARACTER*1 BLANK,CHEM(8),CHEMI(8),COMMA
c------------------------------------------------------------------------------
       DATA        BLANK,COMMA/' ',','/
c------------------------------------------------------------------------------
c    load with blanks
       DO I=1,8
           CHEM(I) = BLANK
           ENDDO
c    transfer characters
       DO I=1,8
           IF ((CHEMI(I).EQ.BLANK).OR.(CHEMI(I).EQ.COMMA))  RETURN
           CHEM(I) = CHEMI(I)
           ENDDO
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  SJSCVA(ATOMI,ATOM)
c
c      Converts atom name ATOMI (a string of up to 2 bytes),
c      to ATOM, which may be regarded externally as a CHARACTER*2 string.
c      ATOM is ended after its first blank.
c------------------------------------------------------------------------------
       IMPLICIT    REAL*8      (A-H,O-Z)
       CHARACTER*1 BLANK,ATOM(2),ATOMI(2)
c------------------------------------------------------------------------------
       DATA        BLANK/' '/
c------------------------------------------------------------------------------
c    load with blanks
       DO 2 I=1,2
           ATOM(I) = BLANK
2          CONTINUE
c    transfer characters
       DO 4 I=1,2
           IF (ATOMI(I).EQ.BLANK)  RETURN
           ATOM(I) = ATOMI(I)
4          CONTINUE
       RETURN
       END
c******************************************************************************
