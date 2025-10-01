c******************************************************************************
c
c      Routines for reading old unformatted Stanjan .SUD or formatted .DAT
c      data files created by DOS JANFILE compiled by the Microsoft compiler.
c      This file must be compiled using special compiler switches.
c
c      For Digital Visual Fortran, compiler settings for this file should be
c      for PowerPC 4.0 i/o format and library compatibility, unformatted file
c      conversion = none.
c
c******************************************************************************
c
       SUBROUTINE OPFOVV(IU,NAME,KE,KF,OK)
c
c      Opens file NAME on unit IU and returns OK=.TRUE. or writes problem
c      message.
c
c       KE  .TRUE. if file must exist
c       KF  .TRUE. if file is formatted
c-----------------------------------------------------------------------------
       LOGICAL         KE,KF,OK
       CHARACTER*32    NAME
c-----------------------------------------------------------------------------
c    check for existence
       IF (KE)  THEN
           INQUIRE (FILE=NAME,EXIST=OK)
           IF (.NOT.OK)  THEN
               CALL FMESVV(2,'@','#','File # not found@',NAME)
               RETURN
               ENDIF
           ENDIF
c    open the file
       IF (KF) THEN
               OPEN(IU,FILE=NAME,ERR=90)
           ELSE
               OPEN(IU,FILE=NAME,FORM='UNFORMATTED',ERR=90)
           ENDIF
       OK = .TRUE.
       RETURN
c    file open error
90     CALL FMESVV(2,'@','#','File # not opened. Probable '//
     ;     'cause; in use by other application.@',NAME)
       OK = .FALSE.
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RUSOVV(IU,NAMAX2,NSMAX2,NIW,NSW,
     ;                      ATOM,CHEM,IW,SW,IERR)
c
c      Loads the unformatted species data file from opened unit IU).

c      Returns NSF = 0 if the file is bad or empty.
c      Loads a maximum subset file if the file is too big.
c
c      Returns IERR=0 of ok, 1 of bad.
c------------------------------------------------------------------------------
c      Nomenclature:
c
c          ATOMF is in work array ATOM after the NAMAX system atom names
c          CHEMF is in work array CHEM after the NSMAX system species names
c
c      Variables in the argument list:
c @        NAMAX2      2*NAMAX dimension of ATOM
c @        NSMAX2      2*NSMAX dimension of CHEM
c @        NIW         dimension of work array IW
c @        NSW         dimension of work array SW
c @        ATOM(I)     CHARACTER*2 name of Ith atom
c @        CHEM(J)     CHARACTER*8 name of Jth species
c          IW(I)       integer work array
c          SW(I)       REAL*4 work array
c @        KSUD        0 if file is formatted, 1 if unformatted
c
c      Data in the CHARACTER*2 array ATOM:
c #        ATOMF(I)    name of the Ith atom type in the file
c
c      Data in the CHARACTER*8 array CHEM:
c #        CHEMF(J)    chemical name of the Jth file species
c
c      Data in the integer array IW:
c #        ITHF(J)     min T/100 for valid Jth file species data
c #        ITHF(J)     max T/100 for valid Jth file species data
c @        KUFL        unit assigned for data file
c @        KUMO        logical unit for monitor
c #        NF(I,J)     number of Ith file atoms in the Jth file species
c #        NAF         number of atoms in the file species
c #        NSF         number of species in the file
c
c      Data in the REAL*4 work array SW:
c #        DCSF(J)     density parameter for the Jth file species
c                          0 for gas species
c                          density of condensed species (g/cm**3)
c #        DHFF(J)     enthalpy of formation at 298.15 K of Jth file species
c #        HMHF(L,J)   enthalpy above 298.15K of the Jth file species at the
c                          Lth tabulated temperature (cal/mol-K)
c #        S0F(L,J)    absolute entropy of the Jth file species at the
c #        WMF(J)      molal mass of the Jth file species (g/mol)
c                          Lth tabulated temperature, (kcal/mol)
c
c      Variables used only internally:
c -        ATOMR(K)    name of the Kth-read atom of a species
c -        NAR(K)      number of the Kth-read atoms for a species
c -        REF         reference date for data (not retained here)
c------------------------------------------------------------------------------
C       IMPLICIT        REAL*8      (A-H,O-Z)
       REAL*4          SW
       CHARACTER*2     ATOM,ATOMR
       CHARACTER*8     CHEM,REF,CHEMX
c    integer*2 variables used in the original microsoft-compiled STANJAN
       INTEGER*2       NAR2,ITHL2,ITHM2
c------------------------------------------------------------------------------
       DIMENSION   ATOM(NAMAX2),CHEM(NSMAX2),IW(NIW),SW(NSW),
     ;             ATOMR(6),NAR(6),NAR2(6)
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*128   LINE
       CHARACTER*16    TEMP
c------------------------------------------------------------------------------
       LOGICAL OK
c------------------------------------------------------------------------------
c    pointers
       DIMENSION       ISPTR(10)
       COMMON /SJSPTR/ ISPTR
       EQUIVALENCE (IoNAF ,ISPTR(4))
       EQUIVALENCE (IoNSF ,ISPTR(5))
       EQUIVALENCE (IoNF  ,ISPTR(7))
       EQUIVALENCE (LoNF  ,ISPTR(8))
       EQUIVALENCE (IoITHL,ISPTR(9))
       EQUIVALENCE (IoITHM,ISPTR(10))
c------------------------------------------------------------------------------
c    set for empty file
       NAF = 0
       NSF = 0
c    set maximum species and atoms
       NAMAX = NAMAX2/2
       NSMAX = NSMAX2/2
c
c    initialize molecule compositions
       DO I=1,NAMAX
           DO J=1,NSMAX
               IW(IoNF+I+LoNF*J) = 0
               ENDDO
           ENDDO
c
c    set pointers for tabular data
       IoDCSF = 0
       IoDHFF = NSMAX
       IoWMF = 2*NSMAX
c    pointers to first species load
       IoHMFJ = 3*NSMAX
       IoS0FJ = 63*NSMAX
c
c -- loop point for file read
c
c    check for too many species
20     IF (NSF.LT.NSMAX)  THEN

c        read the next species data
           J = NSF + 1
           READ (IU,END=50,ERR=70)
     ;     CHEMX,REF,SW(IoWMF+J),SW(IoDHFF+J),
     ;     ITHL2,SW(IoDCSF+J),
     ;     (NAR2(K),ATOMR(K),K=1,6),ITHM2,
     ;     (SW(IoS0FJ+L2),L2=1,60),
     ;     (SW(IoHMFJ+L2),L2=1,60)
c
c        check for duplicate species
           J1 = NSMAX + 1
           J2 = NSMAX + NSF
           CALL SJSCHC(NSMAX2,CHEMX,CHEM,J1,J2,L)
           IF (L.NE.0) THEN
               ML = 0
               CALL LOADZZ('@Duplicate species ',19,LINE,ML)
               CALL LOADZZ(CHEMX,8,LINE,ML)
               CALL LOADZZ(' not loaded.@@',14,LINE,ML)
               CALL WARNZZ('@',LINE)
               GOTO 20
               ENDIF
c
c        set up the atoms for this species
           NAFX = NAF
           DO  K=1,6
               NAR(K) = NAR2(K)
               IF (NAR(K).NE.0)  THEN
c                atom is in the molecule
c                 see if the atom is already known
                   I1 = NAMAX + 1
                   I2 = NAMAX + NAF
                   CALL SJSCHA(NAMAX2,ATOMR(K),ATOM,I1,I2,I)
                   IF (I.NE.0)  THEN
c                    atom already known
                       I = I - NAMAX
                       GOTO 28
                       ENDIF
c                atom is new
                   IF (NAF.EQ.NAMAX)  THEN
                       WRITE (TEMP,24)  NAMAX
24                     FORMAT (I2)
                       ML = 0
                       CALL LOADZZ(
     ;                 '@The file has too many atoms.  Only ',36,
     ;                      LINE,ML)
                       CALL LOADZZ(TEMP,2,LINE,ML)
                       CALL LOADZZ(
     ;                   ' can be handled by this ESPJAN version.@@',41,
     ;                     LINE,ML)
                       CALL WARNZZ('@',LINE)
c                    unload the atoms added by this species
                       NAF = NAFX
                       GOTO 40
                       ENDIF
c                increment the atom count
                   NAF = NAF + 1
c                load the atom name
                   I = NAF
                   ATOM(NAMAX+I) = ATOMR(K)
c                set the atom count
28                 IW(IoNF+I+LoNF*J) = NAR(K)
                   ENDIF
               ENDDO
c
c        accept the species and increment the count
           CHEM(NSMAX+J) = CHEMX
           IW(IoITHL+J)= ITHL2
           IW(IoITHM+J)= ITHM2
           NSF = NSF + 1
c        increment pointers for the next species
           IoHMFJ = IoHMFJ + 60
           IoS0FJ = IoS0FJ + 60
           GOTO 20
           ENDIF
c
c    out of room; look for more data
       READ (IU,END=50,ERR=70) REF
c
c    too many species
       WRITE (TEMP,26)  NSMAX
26     FORMAT (I2)
       ML = 0
       CALL LOADZZ('@The file has too many species.  '//
     ; 'Only ',39,LINE,ML)
       CALL LOADZZ(TEMP,2,LINE,ML)
       CALL LOADZZ(' can be handled by this ESPJAN version.@@',41,
     ;               LINE,ML)
       CALL WARNZZ('@',LINE)
c
c    overflow message
40     CALL WARNZZ('@','@Largest acceptable sub-set file loaded.@@')
c
c    final return
50     CLOSE(IU)
       IW(IoNAF) = NAF
       IW(IoNSF) = NSF
       IERR = 0
       RETURN
c
c    error reading data
70     CLOSE(IU)
       IERR = 1
       RETURN
c
       END
c******************************************************************************
c
       SUBROUTINE FIXSVV(CHEM)
c
c      Removes # from CHM name, replaces with *
c------------------------------------------------------------------------------
       CHARACTER*1 CHEM(8)
c------------------------------------------------------------------------------
       DO I=1,8
           IF (CHEM(I).EQ.'#')  CHEM(I) =  '*'
           ENDDO
           RETURN
       END
c******************************************************************************
