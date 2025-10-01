c*****************************************************************************
c
c      Converts ESP V2 unformatted .ESS file to ASCI
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
       COMMON  /IWVV/IW
       COMMON  /RWVV/RW
c-----------------------------------------------------------------------------
       DATA  NOLDS/'Loaded from old-style setup file                '/
       DATA  UNSET/'                                                '/
c----------------------------------------------------------------------------
       IUA = 10
       IUF = 12
c
1      WRITE (*,2)
2      FORMAT (' Enter new ASCI file name: '$)
       READ(*,3,ERR=1)  NAMEA
3      FORMAT(A32)
       OPEN(IUA,FILE=NAMEA,ERR=1)
c
4      WRITE (*,5)
5      FORMAT (' Enter old unformattted .ESS file name: '$)
       READ(*,6,ERR=4)  NAMEU
6      FORMAT(A32)
       OPEN(IUF,FILE=NAMEU,FORM='UNFORMATTED',ERR=4)
c
c *  read the .ESS file file

         pause 'reading arrays'

       READ (IUF,END=900,ERR=900) IW,RW

       pause 'work arrays read'

c    read the properties data
       CALL RTPFVV(IUF,IERR)
       IF (IERR.NE.0)  GOTO 900


       pause 'properties read'

c    read the flame geometry table
       CALL RFGTVV(IUF,IERR)
       IF (IERR.NE.0)  GOTO 900


       pause 'flame geometry read'

c    read the names of parameters and property sets
       READ (IUF,END=900,ERR=900)  NRUN,NEGM,NEOP,NEMP,NAMB,
     ;      NICC,NECC,NVRA,NVVC,NEGR,NPCC,NPDS,NFLM,NTBM,
     ;      NFGT,NFPM,NGPM,NHTM,NIMM,NEMM


       pause 'names read'

       CLOSE(IUF)
c
c       adjust unset reals
        DO I=1,400
                IF (RW(I).EQ.-1.111111E11) RW(I) = -1.1111111E11
                ENDDO
c
c *  write the ASCII
       WRITE(IUA,102,err=800) (IW(I),I=1,50)
102    FORMAT (10I7)
       WRITE(IUA,103,ERR=800) (RW(I),I=1,400)
103    FORMAT (5(1PE15.7))
c    write the properties data
       CALL WTPFVV(IUA,IERR)
       IF (IERR.NE.0)  GOTO 800
c    write the flame geometry table
       CALL WFGTVV(IUA,IERR)
       IF (IERR.NE.0)  GOTO 800
c    write the names of parameters and property sets
       WRITE (IUA,104,ERR=800)  NRUN,NEGM,NEOP,NEMP,NAMB,
     ;       NICC,NECC,NVRA,NVVC,NEGR,NPCC,NPDS,NFLM,NTBM,
     ;       NFGT,NFPM,NGPM,NHTM,NIMM,NEMM
104    FORMAT(A48)
       CLOSE(IUA)
       WRITE (*,*) ' Conversion successful'
       STOP
c    error writing file
800    WRITE (*,*)  ' Error writing file'
       CLOSE(IUF)
       CLOSE(IUA)
       STOP
c    error reading file
900    WRITE (*,*)  ' Error reading file'
       CLOSE(IUA)
       STOP
c
       END
c******************************************************************************
c
       SUBROUTINE  WTPFVV(IU,IERR)
c
c      Writes ASCII formatted thermodynamic property file opened on unit IU.
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
c    write the file
       WRITE (IU,2,ERR=20)  NGPM,RFT,NF,NO,PR
2      FORMAT (A48/1PE14.6,2I5,E14.6)
       NR = NF + NO
       WRITE  (IU,4,ERR=20)  (CHEM(J),J=1,NR)
4      FORMAT (8(A8,1X))
       WRITE (IU,5,ERR=20)  (RMOLS(J),J=1,NR)
5      FORMAT (5(1PE14.6))
       WRITE  (IU,6,ERR=20)  (HR(J),J=1,IMAX)
6      FORMAT (5(1PE14.6))
       WRITE  (IU,7,ERR=20)  (PVR(J),J=1,IMAX)
7      FORMAT (5(1PE14.6))
       WRITE  (IU,8,ERR=20)  NP,PP
8      FORMAT (I6,1PE14.6)
       J1 = NR + 1
       J2 = NR + NP
       WRITE  (IU,12,ERR=20)  (CHEM(J),J=J1,J2)
12     FORMAT (8(A8,1X))
       WRITE  (IU,13,ERR=20)  (HP(J),J=1,IMAX)
13     FORMAT (5(1PE14.6))
       WRITE  (IU,14,ERR=20)  (PVP(J),J=1,IMAX)
14     FORMAT (5(1PE14.6))
       IERR = 0
       RETURN
c    error writing file
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RTPFVV(IU,IERR)
c
c      Reads the unformatted thermodynamic property data opened on unit IU.
c      Returns IERR = 0 if ok, 1 if problems.
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
       READ (IU,ERR=20)  NGPM,RFT,NF,NO,PR
       NR = NF + NO
       READ  (IU,ERR=20,END=20)  (CHEM(J),J=1,NR),(RMOLS(J),J=1,NR)
       READ  (IU,ERR=20,END=20)  (HR(J),J=1,IMAX),(PVR(J),J=1,IMAX)
       READ  (IU,ERR=20,END=20)  NP,PP
       J1 = NR + 1
       J2 = NR + NP
       READ  (IU,ERR=20,END=20)  (CHEM(J),J=J1,J2),
     ;             (HP(J),J=1,IMAX),(PVP(J),J=1,IMAX)
       IERR=0
       RETURN
c    error writing file
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  WFGTVV(IU,IERR)
c
c      Writes an ASCI flame geometry table file opened on unit IU.
c      Returns IERR = 0 ok, 1 if not.
c------------------------------------------------------------------------------
       CHARACTER*48    NFGT
       DIMENSION       FAHB(11),RFAP(11)
c------------------------------------------------------------------------------
c    Flame geometry table
       COMMON /FGMTVV/ FAHB,RFAP
       COMMON /NFGTVV/ NFGT
c------------------------------------------------------------------------------
c    write the file
       WRITE  (IU,2,ERR=20) NFGT
2      FORMAT (A48)
       WRITE  (IU,3,ERR=20) (FAHB(I),I=1,11)
3      FORMAT (5(1PE14.6))
       WRITE  (IU,4,ERR=20) (RFAP(I),I=1,11)
4      FORMAT (5(1PE14.6))
       IERR = 0
       RETURN
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RFGTVV(IU,IERR)
c
c      Reads an unformatted flame geometry table from file open on unit IU.
c      Returns IERR = 0  ok, 1 if not.
c------------------------------------------------------------------------------
       CHARACTER*48    NFGT
       DIMENSION       FAHB(11),RFAP(11)
c------------------------------------------------------------------------------
c    Flame geometry table
       COMMON /FGMTVV/ FAHB,RFAP
       COMMON /NFGTVV/ NFGT
c------------------------------------------------------------------------------
c    read the file
       READ (IU,ERR=20,END=20) NFGT
       READ  (IU,ERR=20,END=20) (FAHB(I),I=1,11)
       READ  (IU,ERR=20,END=20) (RFAP(I),I=1,11)
       IERR = 0
       RETURN
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************

