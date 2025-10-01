c******************************************************************************
c
c      Properties data file i/o routines
c
c******************************************************************************
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
       SUBROUTINE  DGPHVV(IU)
c
c      Displays the gas properties header on unit IU.
c------------------------------------------------------------------------------
c    Properties file data
       CHARACTER*48    NGPM
       COMMON  /NGPMVV/NGPM
c------------------------------------------------------------------------------
       CHARACTER*80  LINE
c------------------------------------------------------------------------------
       IF (IU.EQ.0)  THEN
               WRITE (LINE,2)  NGPM
2              FORMAT ('@Gas properties model: ',A,'@@')
               CALL DOUTZZ('@',LINE)
           ELSE
               WRITE (IU,3) NGPM
3              FORMAT (/' Gas properties model: ',A)
           ENDIF
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
       CHARACTER*8 CHEMJ(NSMAX2)
       REAL*4   RMOLS(NSMAX)
       COMMON  /PROCVV/ CHEMJ
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
     ;  '   Reactants;  properties evaluated at',F6.2,' atm.'/
     ;  '      Fuel         relative mols')
c    fuels
       J1 = 1
       J2 = NF
       WRITE (IU,4) (CHEM(J),RMOLS(J),J=J1,J2)
4      FORMAT(8X,A8,1PE14.3)
c    oxidizer
       IF (NO.GT.0)  THEN
           WRITE (IU,6)
6          FORMAT ('    Oxidizer')
           J1 = J2 + 1
           J2 = J2 + NO
           WRITE (IU,4) (CHEM(J),RMOLS(J),J=J1,J2)
           ENDIF
c    products
       WRITE (IU,8) PP
8      FORMAT ('   Products;   properties evaluated at',F6.2,' atm.')
       J1 = J2 + 1
       J2 = J2 + NP
       WRITE (IU,10) (CHEM(J),J=J1,J2)
10     FORMAT (8X,A8)
       WRITE (IU,12) RFT
12     FORMAT ('   Fuel mass/total mass = ',E13.5)
c    tables
       WRITE (IU,14)
14     FORMAT (/'   T, K  reactant properties     product properties'/
     ;        '          h, J/kg     Pv, J/kg    h, J/kg     Pv, J/kg')
       L = NF + NO + NP + 10
       DO I=1,48
           IT = 100*(I + 1)
           WRITE (IU,16) IT,HR(I),PVR(I),HP(I),PVP(I)
16         FORMAT (I7,4(1PE12.4))
           ENDDO
       RETURN
       END
c******************************************************************************
