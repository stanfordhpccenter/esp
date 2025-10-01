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

       SUBROUTINE  RTPFVV(IU,IERR)
c
c      Reads thermodynamic property file from open unit IU. File left open.
c
c      Sets up for interpolations.
c      Returns IERR = 0 if ok, 1 if no file read.
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
c    Terminal designation
       COMMON /TERMVV/ KTERM,IUTERM
c------------------------------------------------------------------------------
c    read the file
       READ (IU,ERR=20,END=20)  NGPM,RFT,NF,NO,PR
       NR = NF + NO
c    check overflow
       IF ((NR+NP).GT.NSMAX)  THEN
           CALL WARNZZ('@','@Properties file has too many species. '//
     ;                 ' Use another .ESJ file created by ESPJAN.@@')
           IERR = 1
           RETURN
           ENDIF
       READ (IU,ERR=20,END=20)  (CHEM(J),J=1,NR),(RMOLS(J),J=1,NR)
       READ (IU,ERR=20,END=20)  (HR(J),J=1,IMAX),(PVR(J),J=1,IMAX)
       READ (IU,ERR=20,END=20)  NP,PP
       J1 = NR + 1
       J2 = NR + NP
       READ (IU,ERR=20,END=20)  (CHEM(J),J=J1,J2),
     ;             (HP(J),J=1,IMAX),(PVP(J),J=1,IMAX)
c    compute internal energy
       DO I=1,IMAX
           UP(I) = HP(I) - PVP(I)
           UR(I) = HR(I) - PVR(I)
           ENDDO
       IERR = 0
       RETURN
c
c    error reading file
20     CLOSE(IU)
       IERR = 1
       RETURN
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

