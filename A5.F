c******************************************************************************
c
c      Routines for reading/writing STANJAN species data files
c
c******************************************************************************
c
       SUBROUTINE RSUDVV(IU,SUFILE,OK)
c
c      Reads .SUD file (old or new style) SUFILE using unit IU.
c      Returns OK = .true. of ok, otherwise false.
c------------------------------------------------------------------------------
c    Work array dimensioning
       INCLUDE 'ESPJWORK.H'
c------------------------------------------------------------------------------
c    Stanjan data work arrays
       CHARACTER*2     ATOM(NAMAX3)
       CHARACTER*8     CHEM(NSMAX2)
       COMMON /ATOMVV/ATOM
       COMMON /CHEMVV/CHEM
c------------------------------------------------------------------------------
c    Stanjan work arrays referenced by pointers
       INTEGER IW(NIWORK)
       REAL*4  SW(NSWORK)
       COMMON /SJIW/IW
       COMMON /SJSW/SW
c------------------------------------------------------------------------------
c    Stanjan work array pointers
       DIMENSION ISPTR(10)
       COMMON /SJSPTR/ ISPTR
       EQUIVALENCE (IoNSF ,ISPTR(5))
c------------------------------------------------------------------------------
       CHARACTER*32    SUFILE
       LOGICAL OK
c------------------------------------------------------------------------------
c    new style read
       CALL OPFLVV(IU,SUFILE,.TRUE.,.FALSE.,OK)
       IF (.NOT.OK)  GOTO 99
       CALL RUSFVV(IU,NAMAX2,NSMAX2,NIWORK,NSWORK,ATOM,CHEM,IW,SW,IERR)
c    (file closed)
       IF (IERR.NE.0)  THEN
c        try old style read
           CALL OPFOVV(IU,SUFILE,.TRUE.,.FALSE.,OK)
           IF (.NOT.OK)  GOTO 99
           CALL RUSOVV(IU,NAMAX2,NSMAX2,NIWORK,NSWORK,
     ;                   ATOM,CHEM,IW,SW,IERR)
c        (file closed)
           ENDIF

c    report results
90      IF (IERR.EQ.0) THEN
c            check file contents
               IF (IW(IoNSF).EQ.0)  THEN
                   CALL FMESVV(2,'@','#',
     ;               'Species data file # empty.@',SUFILE)
                   OK = .FALSE.
                   RETURN
                   ENDIF
               CALL FMESVV(1,'@','#',
     ;          'Species data file # loaded.@',SUFILE)
               OK = .TRUE.
           ELSE
               CALL FMESVV(2,'@','#',
     ;          'Bad species data file # not loaded.@',
     ;          SUFILE)
               OK = .FALSE.
           ENDIF
99     RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RUSFVV(IU,NAMAX2,NSMAX2,NIW,NSW,ATOM,CHEM,IW,SW,IERR)
c
c      Loads the unformatted species data file from opened unit IU.

c      Returns NSF = 0 if the file is bad or empty.
c      Loads a maximum subset file if the file is too big.
c
c      Returns IERR=0 if ok, 1 of not.
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
       IMPLICIT        REAL*8      (A-H,O-Z)
       REAL*4          SW
       CHARACTER*2     ATOM,ATOMR
       CHARACTER*8     CHEM,REF,CHEMX
c    .SUD file intger*2 variables
       INTEGER*2       NAR2,ITHL2,ITHM2
c------------------------------------------------------------------------------
       DIMENSION   ATOM(NAMAX2),CHEM(NSMAX2),IW(NIW),SW(NSW),
     ;    IEPTR(80),ATOMR(6),NAR(6),NAR2(6)
c------------------------------------------------------------------------------
       CHARACTER*128   LINE
       CHARACTER*16    TEMP
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
c
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
       IoHMFJ = 3*NSMAX
       IoS0FJ = 63*NSMAX
c
c -- read the file
c
c    check for too many species
20     IF (NSF.LT.NSMAX)  THEN
c        read the next species data
           J = NSF + 1
           READ (IU,END=50,ERR=70)
     ;       CHEMX,REF,SW(IoWMF+J),SW(IoDHFF+J),
     ;       ITHL2,SW(IoDCSF+J),
     ;       (NAR2(K),ATOMR(K),K=1,6),ITHM2,
     ;       (SW(IoS0FJ+L),L=1,60),(SW(IoHMFJ+L),L=1,60)
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
c        accept the species
           CHEM(NSMAX+J) = CHEMX
           IW(IoITHL+J) = ITHL2
           IW(IoITHM+J) = ITHM2
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
       SUBROUTINE  WTPFVV(IU,IERR)
c
c      Writes the thermodynamic property data to file opened on unit IU.
c      Returns IERR = 0 if ok, 1 if write problems.
c------------------------------------------------------------------------------
       CHARACTER*8     CHEM
       CHARACTER*48    NGPM
c------------------------------------------------------------------------------
c    Array dimensions
       PARAMETER (IMAX = 48)
       PARAMETER (NSMAX = 50)
c------------------------------------------------------------------------------
       DIMENSION   CHEM(NSMAX),RMOLS(NSMAX)
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
c------------------------------------------------------------------------------
c    Properties file data
       COMMON  /NGPMVV/ NGPM
       COMMON  /PROCVV/ CHEM
       COMMON  /PRONVV/ NF,NO,NP
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
       COMMON  /PROPVV/ PP,PR,RFT,RMOLS
c------------------------------------------------------------------------------
c    write the file
       WRITE (IU,ERR=20)  NGPM,RFT,NF,NO,PR
       NR = NF + NO
       WRITE (IU,ERR=20)  (CHEM(J),J=1,NR),(RMOLS(J),J=1,NR)
       WRITE (IU,ERR=20)  (HR(J),J=1,IMAX),(PVR(J),J=1,IMAX)
       WRITE (IU,ERR=20)  NP,PP
       J1 = NR + 1
       J2 = NR + NP
       WRITE (IU,ERR=20)  (CHEM(J),J=J1,J2),
     ;       (HP(J),J=1,IMAX),(PVP(J),J=1,IMAX)
       IERR = 0
       RETURN
c    error writing file
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  DGPDVV(IU)
c
c      Displays the gas property data on unit IU.
c      No action for IU=0.
c------------------------------------------------------------------------------
c    Work array dimensioning
       INCLUDE 'ESPJWORK.H'
c------------------------------------------------------------------------------
c    .ESJ properties file data
       PARAMETER   (IMAX=48)
       REAL*4      HP,HR,PVP,PVR,UP,UR,PP,PR,RFT
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       CHARACTER*8 CHEM(NSMAX2)
       REAL*4   RMOLS(NSMAX)
       COMMON  /PROCVV/ CHEM
       COMMON  /PRONVV/ NFUEL,NOX,NPROD
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
       COMMON  /PROPVV/ PP,PR,RFT,RMOLS
       CHARACTER*48    NGPM
       COMMON  /NGPMVV/ NGPM
c------------------------------------------------------------------------------
c    no write for IU=0
       IF (IU.EQ.0)  RETURN
c    header
       WRITE (IU,2) NGPM,PR
2      FORMAT (/' Gas properties model: ',A//
     ;  '   Reactants:  properties evaluated at',F6.2,' atm.'/
     ;  '      Fuel         relative mols')
c    fuels
       J1 = 1
       J2 = NFUEL
       WRITE (IU,4) (CHEM(J),RMOLS(J),J=J1,J2)
4      FORMAT(8X,A8,1PE14.3)
c    oxidizer
       IF (NOX.GT.0)  THEN
           WRITE (IU,6)
6          FORMAT ('    Oxidizer')
           J1 = J2 + 1
           J2 = J2 + NOX
           WRITE (IU,4) (CHEM(J),RMOLS(J),J=J1,J2)
           ENDIF
c    products
       WRITE (IU,8) PP
8      FORMAT ('   Products:   properties evaluated at',F6.2,' atm.')
       J1 = J2 + 1
       J2 = J2 + NPROD
       WRITE (IU,10) (CHEM(J),J=J1,J2)
10     FORMAT (8X,A8)
       WRITE (IU,12) RFT
12     FORMAT ('   Fuel mass/total mass = ',E13.5)
c    tables
       WRITE (IU,14)
14     FORMAT (/'   T, K  reactant properties     product properties'/
     ;        '          h, J/kg     Pv, J/kg    h, J/kg     Pv, J/kg')
       DO I=1,48
           IT = 100*(I + 1)
           WRITE (IU,16) IT,HR(I),PVR(I),HP(I),PVP(I)
16         FORMAT (I7,4(1PE12.4))
           ENDDO
       RETURN
       END
c******************************************************************************
