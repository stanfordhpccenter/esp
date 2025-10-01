c*****************************************************************************
c
c      Converts ASCI ESP V2 setup file to unformatted .ESS file
c
c------------------------------------------------------------------------------
c    designators
       CHARACTER*48    NEGM,NEOP,NEMP,NAMB,NICC,NECC,NFLM,NTBM,NVVC,
     ;                 NPCC,NPDS,
     ;                 NEGR,NFGT,NFPM,NHTM,NIMM,NEMM,NVRA,NRUN,
     ;                 NXXX,NOLDS,UNSET,NGPM
       CHARACTER*32    NAMEA,NAMEU
c------------------------------------------------------------------------------
c    Work arrays:
       DIMENSION IW(50),RW(400)
       COMMON  /NGPMVV/ NGPM
       COMMON  /NFGTVV/ NFGT
c-----------------------------------------------------------------------------
       DATA  NOLDS/'Loaded from old-style setup file                '/
       DATA  UNSET/'                                                '/
c----------------------------------------------------------------------------
       IUA = 10
       IUF = 12
c
1      WRITE (*,2)
2      FORMAT (' Enter ASCII file name: '$)
       READ(*,3,ERR=1)  NAMEA
3      FORMAT(A32)
       OPEN(IUA,FILE=NAMEA,ERR=1)

c
4      WRITE (*,5)
5      FORMAT (' Enter new unformattted .ESS file name: '$)
       READ(*,6,ERR=4)  NAMEU
6      FORMAT(A32)
       OPEN(IUF,FILE=NAMEU,FORM='UNFORMATTED',ERR=4)
c
c *  read the ASCI file
c
       READ (IUA,102,END=800,ERR=800) (IW(I),I=1,50)
102    FORMAT (10I7)
       READ(IUA,103,END=800,ERR=800) (RW(I),I=1,400)
103    FORMAT (5(1PE15.7))
c    read the properties data
       CALL RTPFVV(IUA,IERR)
       IF (IERR.NE.0)  GOTO 800
c    read the flame geometry table
       CALL RFGTVV(IUA,IERR)
       IF (IERR.NE.0)  GOTO 800
c    read the names of parameters and property sets
       READ (IUA,104,END=800,ERR=800)  NRUN,NEGM,NEOP,NEMP,NAMB,
     ;       NICC,NECC,NVRA,NVVC,NEGR,NPCC,NPDS,NFLM,NTBM,
     ;       NFGT,NFPM,NGPM,NHTM,NIMM,NEMM
104    FORMAT(A48)
       CLOSE(IUA)
c
c *  write the unformatted file
c
       WRITE (IUF,ERR=900) IW,RW
c    write the properties data
       CALL WTPFVV(IUF,IERR)
       IF (IERR.NE.0)  GOTO 900
c    write the flame geometry table
       CALL WFGTVV(IUF,IERR)
       IF (IERR.NE.0)  GOTO 900
c    write the names of parameters and property sets
       WRITE (IUF,ERR=900)  NRUN,NEGM,NEOP,NEMP,NAMB,
     ;      NICC,NECC,NVRA,NVVC,NEGR,NPCC,NPDS,NFLM,NTBM,
     ;      NFGT,NFPM,NGPM,NHTM,NIMM,NEMM
       WRITE (*,*) ' Conversion successful'
       CLOSE(IUF)
       STOP
c
c    error reading file
800    WRITE (*,*)  ' Bad ASCII setup file'
       CLOSE(IUA)
       STOP
c
c    error writing file
900    WRITE (*,*)  ' Error writing file'
       CLOSE(IUF)
       STOP
c
       END
c******************************************************************************
c
       SUBROUTINE  RTPFVV(IU,IERR)
c
c      Reads thermodynamic property file opened on unit IU.
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
       DIMENSION   HP(IMAX),HR(IMAX),PVP(IMAX),PVR(IMAX)
c------------------------------------------------------------------------------
c    Properties file data
       COMMON  /NGPMVV/ NGPM
       COMMON  /PROCVV/ CHEM
       COMMON  /PRONVV/ NF,NO,NP
       COMMON  /PRODVV/ HP,HR,PVP,PVR,UP,UR
       COMMON  /PROPVV/ PP,PR,RFT,RMOLS
c------------------------------------------------------------------------------
c    read the file
       READ (IU,2,END=20,ERR=20)  NGPM,RFT,NF,NO,PR
2      FORMAT (A48/1PE14.6,2I5,E14.6)
       NR = NF + NO
       READ (IU,4,END=20,ERR=20)  (CHEM(J),J=1,NR)
4      FORMAT (8(A8,1X))
       READ (IU,5,END=20,ERR=20)  (RMOLS(J),J=1,NR)
5      FORMAT (5(1PE14.6))
       READ (IU,6,END=20,ERR=20)  (HR(J),J=1,IMAX)
6      FORMAT (5(1PE14.6))
       READ (IU,7,END=20,ERR=20)  (PVR(J),J=1,IMAX)
7      FORMAT (5(1PE14.6))
       READ (IU,8,END=20,ERR=20)  NP,PP
8      FORMAT (I6,1PE14.6)
       J1 = NR + 1
       J2 = NR + NP
       READ (IU,12,END=20,ERR=20)  (CHEM(J),J=J1,J2)
12     FORMAT (8(A8,1X))
       READ (IU,13,END=20,ERR=20)  (HP(J),J=1,IMAX)
13     FORMAT (5(1PE14.6))
       READ (IU,14,END=20,ERR=20)  (PVP(J),J=1,IMAX)
14     FORMAT (5(1PE14.6))
       IERR = 0
       RETURN
c    error reading file
20     IERR = 1
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
       WRITE (IU,ERR=20)  NGPM,RFT,NF,NO,PR
       NR = NF + NO
       WRITE (IU,ERR=20)  (CHEM(J),J=1,NR),(RMOLS(J),J=1,NR)
       WRITE (IU,ERR=20)  (HR(J),J=1,IMAX),(PVR(J),J=1,IMAX)
       WRITE (IU,ERR=20)  NP,PP
       J1 = NR + 1
       J2 = NR + NP
       WRITE (IU,ERR=20)  (CHEM(J),J=J1,J2),
     ;             (HP(J),J=1,IMAX),(PVP(J),J=1,IMAX)

       IERR=0
         RETURN
c    error writing file
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RFGTVV(IU,IERR)
c
c      Reads an ASCI flame geometry table file opened on unit IU.
c      Returns IERR = 0 read, 1 if not.
c------------------------------------------------------------------------------
       CHARACTER*48    NFGT
       DIMENSION       FAHB(11),RFAP(11)
c------------------------------------------------------------------------------
c    Flame geometry table
       COMMON /FGMTVV/ FAHB,RFAP
       COMMON /NFGTVV/ NFGT
c------------------------------------------------------------------------------
c    read the file
       READ (IU,2,END=20,ERR=20) NFGT
2      FORMAT (A48)
       READ (IU,3,END=20,ERR=20) (FAHB(I),I=1,11)
3      FORMAT (5(1PE14.6))
       READ (IU,4,END=20,ERR=20) (RFAP(I),I=1,11)
4      FORMAT (5(1PE14.6))
       IERR = 0
       RETURN
c
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  WFGTVV(IU,IERR)
c
c      Writes an unformatted flame geometry table to unit IU.
c      Returns IERR = 0 written ok, 1 if not.
c------------------------------------------------------------------------------
       CHARACTER*48    NFGT
       DIMENSION       FAHB(11),RFAP(11)
c------------------------------------------------------------------------------
c    Flame geometry table
       COMMON /FGMTVV/ FAHB,RFAP
       COMMON /NFGTVV/ NFGT
c------------------------------------------------------------------------------
c    write the file
       WRITE (IU,ERR=20) NFGT
         WRITE (IU,ERR=20) (FAHB(I),I=1,11)
         WRITE (IU,ERR=20) (RFAP(I),I=1,11)
       IERR = 0
       RETURN
c
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************
