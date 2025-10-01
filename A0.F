c******************************************************************************
c
c      ESPJAN main program
c
c******************************************************************************
c
c      Files to be compiled and linked to build ESP:
c
c      Setup routines
c          A0.F      ESP main program
c          A1.F      run setup routines
c          A2.F      pointer setup routines
c          A3.F      help files
c          A4.F      old-style file read (special compiler switches)
c          A5.F      new-style file read
c          A6.F      chemical and atom name processing routines
c
c        Interface programs    * files depend on the interface used
c
c          IB.F      subroutines called by ESP
c        * I1.F      main interface subroutines
c        * I2.F      interface support routines
c
c        INTWRKZZ.H  interface include file
c
c******************************************************************************
c
c      Data file structure
c
c      The enthalpy and Pv product are assumed to be functions of temperature
c      only (independent of pressure).
c
c      The thermodynamic properties of reactants and products are read from
c      a file created by GEMPROP, a program based on the STANJAN chemical
c      equilibrium program.  The file is UNFORMATTED and structured as follows:
c
c      CHARACTER*8 CHEM
c
c      First record:
c          PNAME       48-character name of the properties model
c          RFT         fuel mass/total mass
c          NF          number of species in fuel
c          NO          number of species in oxidizer
c          PR          pressure at which reactant properties were evaluated
c      Next records: for J = 1,NF+NO:
c          CHEM(J)     8-character chemical name of the Jth reactant
c      Next records: for J = 1,NF+NO:
c          RMOLS(J)    mols of the reactants (relative)
c      Next records: for I=1,48:
c          HR(I)       enthalpy of reactants at Ith temperature, J/K
c      Next records: for I=1,48:
c          PVR(I)      Pv product of reactants at the Ith temperature, J/K
c      Next record:
c          NP          number of product species
c          PP          pressure at which product species were evaluated
c      Next records: for J = NF+NO+1,NF+N0+NP:
c          CHEM(J)     8-character chemical name of the (J-NF-N0)th product
c      Next records: for I=1,48:
c          HP(I)       enthalpy of products at Ith temperature, J/K
c      Next records: for I=1,48:
c          PVP(I)      Pv product of products at the Ith temperature, J/K
c
c      The temperatures are 200,300,400,...,4900 K; T(I) = 100 + 100*I.
c      The internal energies are computed from h and Pv.
c      The data are stored in COMMON blocks.
c******************************************************************************
c
c      ESPJAN main program
c
c------------------------------------------------------------------------------
c    Work array dimensioning
       INCLUDE 'ESPJWORK.H'
c------------------------------------------------------------------------------
c    Stanjan runs double precision except as noted
       IMPLICIT    REAL*8 (A-H,O-Z)
       LOGICAL     YESZZ,OK,CFNGVV
c------------------------------------------------------------------------------
c    interface work arrays
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
c------------------------------------------------------------------------------
c    application-specific subroutines called by interface program GETZZ
       EXTERNAL    SETRVV,ACTRVV,HELPVV
       EXTERNAL    SETMVV,ACTMVV
       EXTERNAL    SETFVV,ACTFVV
       EXTERNAL    SETPVV,ACTPVV
       EXTERNAL    SETXVV,ACTXVV
c------------------------------------------------------------------------------
c    line length for non-graphical interface message box
       COMMON /MESBZZ/ LMESS
c------------------------------------------------------------------------------
c    Stanjan data work arrays
       CHARACTER*2     ATOM(NAMAX3)
       CHARACTER*8     CHEM(NSMAX2)
       COMMON /ATOMVV/ATOM
       COMMON /CHEMVV/CHEM
c------------------------------------------------------------------------------
c    Stanjan work arrays referenced by pointers
       INTEGER IW(NIWORK)
       REAL*8  RW(NRWORK)
       REAL*4  SW(NSWORK)
       COMMON /SJIW/IW
       COMMON /SJRW/RW
       COMMON /SJSW/SW
c------------------------------------------------------------------------------
c    Stanjan work array pointers
       DIMENSION IEPTR(80)
       DIMENSION ITPTR(20)
       DIMENSION IRPTR(20)
       DIMENSION ISPTR(10)
       COMMON /SJEPTR/ IEPTR
       COMMON /SJTPTR/ ITPTR
       COMMON /SJRPTR/ IRPTR
       COMMON /SJSPTR/ ISPTR
c------------------------------------------------------------------------------
c    pointers
       EQUIVALENCE (IoKERR,IEPTR(1))
       EQUIVALENCE (IoKMON,IEPTR(2))
       EQUIVALENCE (IoKUMO,IEPTR(4))
       EQUIVALENCE (IoNA  ,IEPTR(5))
       EQUIVALENCE (IoNP  ,IEPTR(7))
       EQUIVALENCE (IoNS  ,IEPTR(8))
       EQUIVALENCE (IoN   ,IEPTR(28))
       EQUIVALENCE (LoN   ,IEPTR(29))
       EQUIVALENCE (IoNSP ,IEPTR(30))
       EQUIVALENCE (IoHUGE,IEPTR(32))
       EQUIVALENCE (IoPA  ,IEPTR(60))
       EQUIVALENCE (IoSMOL,IEPTR(74))
c
       EQUIVALENCE (IoKFRZ,ITPTR(1))
       EQUIVALENCE (IoPATM,ITPTR(3))
       EQUIVALENCE (IoP   ,ITPTR(5))
       EQUIVALENCE (IoT   ,ITPTR(6))
       EQUIVALENCE (IoH   ,ITPTR(7))
       EQUIVALENCE (IoV   ,ITPTR(10))
       EQUIVALENCE (IoDHF0,ITPTR(14))
       EQUIVALENCE (IoWMS ,ITPTR(15))
c
       EQUIVALENCE (IoNAF ,ISPTR(4))
       EQUIVALENCE (IoNSF ,ISPTR(5))
       EQUIVALENCE (IoJFS ,ISPTR(6))
       EQUIVALENCE (IoNF  ,ISPTR(7))
       EQUIVALENCE (LoNF  ,ISPTR(8))
c------------------------------------------------------------------------------
c    Nearly-largest REAL*8 number:
       PARAMETER (HUGEN = 1.D100)
       COMMON /SJHUGE/HUGE,EAMIN,EAMAX
c------------------------------------------------------------------------------
c    Data file name
       CHARACTER*32    SUFILE
c------------------------------------------------------------------------------
c    Version designator
       CHARACTER*16    VERS
       COMMON /VERSVV/ VERS
c------------------------------------------------------------------------------
c    name of reactants
       CHARACTER*48    NREACT
       COMMON /NRCTVV/NREACT
c------------------------------------------------------------------------------
c    reactant inclusion
       DIMENSION   NOPR(NSMAX)
       COMMON /NOPRVV/NOPR
c------------------------------------------------------------------------------
c    fuel identification
       DIMENSION   NOPF(NSMAX)
       COMMON /NOPFVV/NOPF
c------------------------------------------------------------------------------
c    product identification
       DIMENSION   NOPP(NSMAX)
       COMMON /NOPPVV/NOPP
c------------------------------------------------------------------------------
c    data file name
       CHARACTER*32    DFILE
       COMMON /DFLEVV/ DFILE
c------------------------------------------------------------------------------
c    .ESJ properties file data
       PARAMETER   (IMAX=48)
       REAL*4      HP,HR,PVP,PVR,UP,UR,PP,PR,RFT
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX),
     ;             UP(IMAX),UR(IMAX)
       CHARACTER*8 CHEMJ(NSMAX2)
       REAL*4   RMOLS(NSMAX)
       COMMON  /PROCVV/ CHEMJ
       COMMON  /PRONVV/ NFUEL,NOX,NPROD
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
       COMMON  /PROPVV/ PP,PR,RFT,RMOLS
       CHARACTER*48    NGPM
       COMMON  /NGPMVV/ NGPM
c------------------------------------------------------------------------------
c    output file name
       CHARACTER*32    OFILE
       COMMON /OFLEVV/ OFILE,NOUT
c------------------------------------------------------------------------------
       LOGICAL ATOMZ(NAMAX)
c------------------------------------------------------------------------------
c    Stanjan monitor file
       CHARACTER*32 MONF
c------------------------------------------------------------------------------
c    internal files
       CHARACTER*96    LINE
       CHARACTER*16    TEMP
c==============================================================================
c    set output window scroll
       CALL SOWSZZ
c------------------------------------------------------------------------------
c    set line width for messages
       LMESS = 78
c------------------------------------------------------------------------------
c    set version
       VERS = 'V2.04W  '
c------------------------------------------------------------------------------
c    set INTRFACE work array dimensions
       ID(2) = NCMAXZ
       ID(3) = NIMAXZ
       ID(4) = NRMAXZ
c------------------------------------------------------------------------------
c    load unset indicators (be consistent with SETZZ in file IB.F and
c     RSOVV in file S8.F.
       CD(1) = CHAR(239)
       ID(1) = -22222
       RD(1) = -1.1111111E11
c==============================================================================
c    set shorthand work array dimensions
       NIW = NIWORK
       NRW = NRWORK
       NSW = NSWORK
c
c    set pointers and check dimensions
       CALL SJSPTS(NAMAX,NPMAX,NSMAX,NIW,NRW,NSW,RW)
c
c    set exponential argument limits
       RW(IoHUGE) = HUGEN
       EAMAX = DLOG(HUGEN)
       EAMIN = - EAMAX
c
c    set Stanjan monitor file
       MONF = 'ESPJAN.MON'

c    opening header
       CALL HVERVV
c
c    briefing
1      IF (YESZZ('Would you like a briefing on ESPJAN?')) THEN
           CALL CLOWZZ
2          CALL HINSVV
c
3          CALL CLPWZZ
           IF (YESZZ('Are you ready to try using ESPJAN?'))  GOTO 4
           IF (YESZZ('Do you want to read the briefing again?')) GOTO 2
           IF (YESZZ('Do you want to quit ESPJAN?'))  STOP
           GOTO 3
           ENDIF
c
c    set the default data file
4     SUFILE = 'ESP.SUD'
c
c * read the data
c
c    do the read; integers are INTEGER*2 in .SUD files
10     CALL CLPWZZ
       INQUIRE (FILE=SUFILE,EXIST=OK)
       IF (OK) THEN
               CALL RSUDVV(8,SUFILE,OK)
               IF (.NOT.OK)  THEN
                   NOPA = 2
                   GOTO 22
                   ENDIF
           ELSE
               CALL FMESVV(2,'@','#',
     ;           'Species data file # not found.@',SUFILE)
               NOPA = 2
               GOTO 22
           ENDIF
c
c    get file counts
       NSF = IW(IoNSF)
       NAF = IW(IoNAF)
c    check file contents
       IF (NSF.EQ.0)  THEN
           CALL FMESVV(2,'@','#',
     ;       'Species data file # empty.@',SUFILE)
           NOPA = 2
           GOTO 22
           ENDIF

c    initialize no selections
       NIVR = 0
       DO I=1,NSF
           NOPR(I) = 0
           NOPF(I) = 0
           ENDDO
       NOUT = 1

c    reactant seletion
20     DO J=1,NSF
c        clear any fuel tags
           IF (NOPR(J).EQ.2) NOPR(J) = 1
           ENDDO
       CALL GETZZ(NIVR,SETRVV,HELPVV,ACTRVV,NOPA)
22     IF (NOPA.EQ.2)  THEN
           IF (YESZZ('Do you want to quit ESPJAN?'))  GOTO 200
c        use another .SUD data file
           CALL  GFNEZZ('SUD',SUFILE)
           GOTO 10
           ENDIF
c     check that all species are gas
       DO J=1,NSF
           IF (NOPR(J).NE.0)  THEN
               IF (CFNGVV(J)) THEN
                   NOPR(J) = 0
                   NIVR = 1
                   GOTO 20
                   ENDIF
               ENDIF
           ENDDO
c
c    scan file species for inclusion in the system
       JS = 0
       DO JF=1,NSF
           IF (NOPR(JF).NE.0)  THEN
c            put the file species in the system
               JS = JS + 1
               IW(IoJFS+JS) = JF
               CHEM(JS) = CHEM(NSMAX+JF)
               ENDIF
           ENDDO
       IW(IoNS) = JS
       NS = JS
c    check for nothing
       IF (NS.EQ.0)  THEN
           CALL WARNZZ('@','@You must have some species.@@')
           NIVR = 0
           GOTO 20
           ENDIF
c
c    identify fuel species
       NIVF = 0
c    clear fuel tags in reactants
30     DO J=1,NSF
           IF (NOPR(J).EQ.2) NOPR(J) = 1
           ENDDO
c    get fuel identification
       CALL GETZZ(NIVF,SETFVV,HELPVV,ACTFVV,NOPA)
       IF (NOPA.EQ.2) THEN
           NIVR = 1
           GOTO 20
           ENDIF
c    find fuel
       NFUEL = 0
       DO JS=1,NS
           IF (NOPF(JS).EQ.1) THEN
c            tag reactant as fuel
               JFS = IW(IoJFS+JS)
               NOPR(JFS) = 2
               NFUEL = NFUEL + 1
               ENDIF
           ENDDO
c    check for nothing
       IF (NFUEL.EQ.0)  THEN
           CALL WARNZZ('@','@You must have some fuel.@@')
           NIVF = 0
           GOTO 30
           ENDIF
c    set oxidizer count
       NOX = NS - NFUEL
c    clear N(I,J)
       DO J=1,NSF
           DO I=1,NAMAX
               IW(IoN+I+LoN*J) = 0
               ENDDO
           ENDDO
c    resort to put fuels first
       JS = 0
       NA = 0
c    set to do fuel
       KNOP = 2
32     DO JF=1,NSF
           IF (NOPR(JF).EQ.KNOP)  THEN
c            put the file species in the system
               JS = JS + 1
               IW(IoJFS+JS) = JF
               CHEM(JS) = CHEM(NSMAX+JF)
               RW(IoDHF0+JS) = SW(NSMAX+JF)
               RW(IoWMS+JS) = SW(NSMAX2+JF)
c            identify atoms in this species
               DO IF=1,NAF
                   IF (IW(IoNF+IF+LoNF*JF).NE.0) THEN
c                    file atom is in the species; identify system atom
                       DO IS=1,NA
                           IF (ATOM(IS).EQ.ATOM(NAMAX+IF))  GOTO 34
                           ENDDO
c                    atom not yet in system; add it
                       NA = NA + 1
                       IS = NA
                       ATOM(IS) = ATOM(NAMAX+IF)
c                    record the compostion
34                     IW(IoN+IS+LoN*JS) = IW(IoNF+IF+LoNF*JF)
                       ENDIF
                   ENDDO
               ENDIF
           ENDDO
c    check pass
       IF (KNOP.EQ.2)  THEN
c        set to do oxidizers
           KNOP = 1
           GOTO 32
           ENDIF
c    load the counts
       IW(IoNS) = JS
       IW(IoNSP+1) = JS
       IW(IoNA) = NA
c
c    reactant moles specification
       NIVM = 0
40     CALL GETZZ(NIVM,SETMVV,HELPVV,ACTMVV,NOPA)
       IF (NOPA.EQ.2) THEN
           NIVF = 1
           GOTO 30
           ENDIF
       IF (NOPA.EQ.3) THEN
           NIVR = 1
           GOTO 20
           ENDIF
c
c    compute the atom populations
       DO I=1,NA
           RW(IoPA+I) = 0
           DO J=1,NS
               RW(IoPA+I) = RW(IoPA+I) + IW(IoN+I+LoN*J)*RMOLS(J)
               ENDDO
           ENDDO
c
c    compute fuel mass / total mass
       TOTM= 0
       FUELM= 0
       IoWMF = NSMAX2
       DO JS=1,NS
           JFS = IW(IoJFS+JS)
           RMASS = RMOLS(JS)*SW(IoWMF+JFS)
           IF (NOPR(JFS).EQ.2)  FUELM = FUELM + RMASS
           TOTM = TOTM + RMASS
           ENDDO
       RFT = FUELM/TOTM
c
c    select products
       NIVP = 0
50     CALL GETZZ(NIVP,SETPVV,HELPVV,ACTPVV,NOPA)
       IF (NOPA.EQ.2) THEN
           DO J=1,NSF
               IF (NOPR(J).EQ.2) NOPR(J) = 1
               ENDDO
           NIVR = 1
           GOTO 20
           ENDIF
       IF (NOPA.EQ.3)  THEN
c        use another .SUD data file
           CALL  GFNEZZ('SUD',SUFILE)
           INQUIRE (FILE=SUFILE,EXIST=OK)
           IF (OK) THEN
                   CALL RSUDVV(8,SUFILE,OK)
               ELSE
                   CALL FMESVV(2,'@','#',
     ;               'Species data file # not found.@',SUFILE)
               ENDIF
           NIVP = 1
           GOTO 50
           ENDIF
c     check that all product species are gas
       DO J=1,NSF
           IF (NOPP(J).NE.0)  THEN
                IF (CFNGVV(J)) THEN
                    NOPP(J) = 0
                    NIVP = 1
                    GOTO 50
                    ENDIF
               ENDIF
           ENDDO
c
c    check products for atom content
       DO IS=1,NA
           ATOMZ(IS) = .FALSE.
           ENDDO
c    scan products
       DO JF=1,NSF
           IF (NOPP(JF).NE.0)  THEN
c            species included; check atoms
               DO IF=1,NAF
                   IF (IW(IoNF+IF+LoNF*JF).NE.0)  THEN
c                    atom in species; is it in the system?
                       CALL SJSCHA(NAMAX,ATOM(NAMAX+IF),ATOM,1,NA,IS)
                       IF (IS.NE.0)  THEN
c                            atom is in the system and products
                               ATOMZ(IS) = .TRUE.
                           ELSE
c                            atom not in the system; inform user
                               ML = 0
                               CALL LOADZZ('@Species ',9,LINE,ML)
                               CALL CHMNVV(CHEM(NSMAX+JF),N)
                               CALL LOADZZ(CHEM(NSMAX+JF),N,LINE,ML)
                               CALL LOADZZ(' contains atom ',15,LINE,ML)
                               CALL LOADZZ(ATOM(NAMAX+IF),2,LINE,ML)
                               CALL LOADZZ(' not in the reactants.@@',
     ;                                    24,LINE,ML)
                               CALL WARNZZ('@',LINE)
                               NIVP = 1
                               GOTO 50
                           ENDIF
                       ENDIF
                   ENDDO
               ENDIF
           ENDDO
c    check that all reactant atoms are represented
       DO IS=1,NA
           IF (.NOT.ATOMZ(IS)) THEN
c            an atom in the reactants is not in the products
               ML = 0
               CALL LOADZZ('@The products do not contain ',29,
     ;                      LINE,ML)
               CALL LOADZZ(ATOM(IS),2,LINE,ML)
               CALL LOADZZ(' atoms provided by the reactants.@@',35,
     ;                     LINE,ML)
               CALL WARNZZ('@',LINE)
               NIVP = 1
               GOTO 50
               ENDIF
           ENDDO
c
c    final data input
       NIVX = 0
60     CALL GETZZ(NIVX,SETXVV,HELPVV,ACTXVV,NOPA)
         IF (NOPA.EQ.2)  THEN
           NIVP = 1
           GOTO 50
           ENDIF
c
c    open the data fle
       CALL OPFLVV(10,DFILE,.FALSE.,.FALSE.,OK)
       IF (.NOT.OK)  THEN
           CLOSE(10)
           NIVX = 1
           GOTO 60
           ENDIF
c
c    make the Stanjan calculation
       KPASS = 1
100    IF (KPASS.EQ.1)  THEN
c            set for reactants
               RW(IoP) = PR*RW(IoPATM)
               IW(IoKFRZ) = - 1
               IW(IoNP) = 1
               DO JS=1,NS
                   CHEMJ(JS) = CHEM(JS)
                   RW(IoSMOL+JS) = RMOLS(JS)
                   ENDDO
           ELSE
c            set for products
               RW(IoP) = PP*RW(IoPATM)
               IW(IoKFRZ) = 0
c            clear atom composition
               DO J=1,NSMAX
                   DO I=1,NAMAX
                       IW(IoN+I+LoN*J) = 0
                       ENDDO
                   ENDDO
c            keep number of rectants
               NR = NS
c            set up product species in system
               NS = 0
               JS = 0
               DO JF=1,NSF
                   IF (NOPP(JF).NE.0) THEN
c                    put the file species in the system
                       JS = JS + 1
                       IW(IoJFS+JS) = JF
                       CHEM(JS) = CHEM(NSMAX+JF)
                       RW(IoDHF0+JS) = SW(NSMAX+JF)
                       RW(IoWMS+JS) = SW(NSMAX2+JF)
c                    add to the .ESJ data file specis
                       CHEMJ(NR+JS) = CHEM(JS)
c                    identify atoms in this species
                       DO IF=1,NAF
                           IF (IW(IoNF+IF+LoNF*JF).NE.0) THEN
c                            file atom is in the species; identify
                               DO IS=1,NA
                                   IF (ATOM(IS).EQ.ATOM(NAMAX+IF))
     ;                                 GOTO 134
                                   ENDDO
c                            atom not in system
                               CALL WARNZZ('@','@Program error; '//
     ;                           ' missing atom; inform program '//
     ;                           ' author.@@')
                               GOTO 20
c                            record the compostion
134                            IW(IoN+IS+LoN*JS) =
     ;                             IW(IoNF+IF+LoNF*JF)
                               ENDIF
                           ENDDO
                       ENDIF
                   ENDDO
               NS = JS
               NPROD = NS
               IW(IoNS) = NS
               IW(IoNSP+1) = JS
           ENDIF
c
c    make the calculation
       KINIT = 1
c    diagnostic monitor; normally KMON=0
       CALL OPFLVV(3,MONF,.FALSE.,.TRUE.,OK)
       IW(IoKUMO) = 3
       IW(IoKMON) = 0
c    loop point
        DO I=1,48
           RW(IoT) = 100 + 100*I
           CALL SJTP(NAMAX,NSMAX,NIW,NRW,NSW,ATOM,CHEM,IW,RW,SW,KINIT)
c        check for impossible populations
           IF (IW(IoKERR).EQ.3)  THEN
               IF (KPASS.EQ.1) THEN
                      CALL WARNZZ('@','@Program error; impossible '//
     ;                'reactant populations.  Inform program author.@@')
                       NIVR = 1
                       GOTO 20
                   ELSE
                       CALL WARNZZ('@','@The specified products '//
     ;                 'cannot contain all the atoms in the '//
     ;                 'reactants.  Change products.@@')
                   NIVP = 1
                   GOTO 50
                 ENDIF
               ENDIF
c
c        check for error
           IF (IW(IoKERR).NE.0)  THEN
c            calculation failed
               ML = 0
               CALL LOADZZ('@The calculation FAILED for T =',31,
     ;           LINE,ML)
               WRITE (TEMP,152) RW(IoT)
152            FORMAT(F6.0)
               CALL LOADZZ(TEMP,6,LINE,ML)
               CALL LOADZZ(' K.  Probable cause is poor product '//
     ;            ' species selection.@@',57,LINE,ML)
               CALL WARNZZ('@',LINE)
               NIVP = 1
               GOTO 50
               ENDIF
c
c        save the data
           IF (KPASS.EQ.1)  THEN
c                reactants
                   HR(I) = RW(IoH)
                   PVR(I) = RW(IoP)*RW(IoV)
               ELSE
c                products
                   HP(I) = RW(IoH)
                   PVP(I) = RW(IoP)*RW(IoV)
               ENDIF
           ENDDO
c
c    end of pass
       IF (KPASS.EQ.1)  THEN
c        go do reactants
           KPASS = 2
           GOTO 100
           ENDIF
c
c    write the data file
       CALL OPFLVV(10,DFILE,.FALSE.,.FALSE.,OK)
       IF (OK)  THEN
               CALL WTPFVV(10,IERR)
               IF (IERR.NE.0)  THEN
                       CALL FMESVV(2,'@','#',
     ;                  'Error writing #.  File corrupt.@',DFILE)
                   ELSE
                       CALL FMESVV(1,'@','#',
     ;                  'ESP properties data file # written.@',DFILE)
                   ENDIF
           ELSE
               CALL FMESVV(2,'@','#',
     ;           'Error opening #.  No file saved.@',DFILE)
           ENDIF
       CLOSE(10)
c
c    check for output file write
       IF (NOUT.EQ.2)  THEN
c        write display file
           CALL OPFLVV(10,OFILE,.FALSE.,.TRUE.,OK)
           IF (OK)  THEN
                   CALL DGPDVV(10)
                   CALL FMESVV(1,'@','#',
     ;              'Display file # written.@@',OFILE)
               ELSE
                   CALL FMESVV(1,'@','#',
     ;              'File # not written.@@',OFILE)
               ENDIF
           CLOSE(10)
           ENDIF
c
       NIVR = 1
       GOTO 20
c
c    exit
200    CLOSE(3)

       STOP
       END
c******************************************************************************
c
       LOGICAL FUNCTION CFNGVV(J)
c
c      Checks that file species J is a gas.  Returns .FALSE. if gas.
c      If condensed, sends message and returns  .TRUE.
c-----------------------------------------------------------------------------
c    Work array dimensioning
       INCLUDE 'ESPJWORK.H'
c------------------------------------------------------------------------------
       CHARACTER*64    LINE
c------------------------------------------------------------------------------
c    Stanjan data work arrays
       CHARACTER*8  CHEM(NSMAX2)
       COMMON /CHEMVV/CHEM
       REAL*4  SW(NSWORK)
       COMMON /SJSW/SW
c------------------------------------------------------------------------------
       IF (SW(J).EQ.0)  THEN
c        gas species
               CFNGVV = .FALSE.
c        non-gas species
           ELSE
               CALL CHMNVV(CHEM(NSMAX+J),N)
               ML = 0
               CALL LOADZZ('@',1,LINE,ML)
               CALL LOADZZ(CHEM(NSMAX+J),N,LINE,ML)
               CALL LOADZZ(' is not a gas species and '//
     ;             'cannot be included.@@',47,LINE,ML)
               CALL WARNZZ('@',LINE)
               CFNGVV = .TRUE.
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE OPFLVV(IU,NAME,KE,KF,OK)
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
       SUBROUTINE FMESVV(K,ESC,INS,STRING,NAME)
c
c      Issues message (K=1) or warning (K=2) ESC-terminated STRING with
c      file name inserted where INS is found.
c-----------------------------------------------------------------------------
       PARAMETER   (LNAME=32)
       PARAMETER   (LMAX=256)
       CHARACTER*1 ESC,INS,STRING(*),NAME(32),TEXT(LMAX)
c-----------------------------------------------------------------------------
c    find end of name
       DO I=1,LNAME
           IF (NAME(I).EQ.' ') THEN
               N = I - 1
               GOTO 8
               ENDIF
           ENDDO
c    no blanks
       N = LNAME
c    set to scan the string
8      LS = 0
       LT = 1
       TEXT(LT) = ESC
c    continue the scan
10     LS = LS + 1
       IF (STRING(LS).EQ.INS)  THEN
c        insert the name
           DO I=1,N
               IF (LT.EQ.LMAX-2)  GOTO 40
               LT = LT + 1
               TEXT(LT) = NAME(I)
               ENDDO
           GOTO 10
           ENDIF
       IF (STRING(LS).EQ.ESC)  GOTO 40
       IF (LT.EQ.LMAX-2)  GOTO 40
       LT = LT + 1
       TEXT(LT) = STRING(LS)
       GOTO 10
c    end the string
40     TEXT(LT+1) = ESC
       TEXT(LT+2) = ESC
c    send the message
       IF (K.EQ.1)  THEN
               CALL MESSZZ(ESC,TEXT)
               CALL PAUSZZ(0)
           ELSE
               CALL WARNZZ(ESC,TEXT)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE QUITVV
c
c      ESP close routine
c-----------------------------------------------------------------------------
       LOGICAL OUT,YESZZ
c------------------------------------------------------------------------------
c    confirm quit
       IF (YESZZ('Do you really want to quit ESPJAN?')) THEN
c        close the monitor file
           CLOSE(3)
           STOP
           ENDIF
c    no quit
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE STOPZZ
c
c      Stop routine
c------------------------------------------------------------------------------
c    close the monitor file
       CLOSE(3)
       STOP
       END
c******************************************************************************
