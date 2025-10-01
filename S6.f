c******************************************************************************
c
c      Flame geometry table handling routines
c
c******************************************************************************
c      The Flame Geometry  Model specifies the following data in tabular
c      form at the Jth tabulated burned gas volume fraction.
c
c      FAHB(J)     fraction of heat transfer area exposed to burned gas
c      RFAP(J)     ratio of projected flame area to piston area
c
c      The volume fractions for the table are 0, 0.1, 0.2, ..., 1.0
c          J = 10*VB/V + 1
c
c      The data are stored in COMMON /FGAMVV/
c******************************************************************************
c
       SUBROUTINE FMGCVV(XB,FAHBI,RFAPI)
c
c      Flame model geometry calculation
c-----------------------------------------------------------------------------
c      Arguments:
c          At call:
c              XB      fraction of cylinder volume occupied by burned gas
c          On return:
c              FAHBI   fraction of heat transfer area exposed to burned gas
c              RFAPI   flame area/piston area
c-----------------------------------------------------------------------------
       DIMENSION   FAHB(11),RFAP(11)
c-----------------------------------------------------------------------------
       COMMON /FGMTVV/ FAHB,RFAP
c-----------------------------------------------------------------------------
c    determine lower entry
       J = 10*XB + 1
       IF (J.GE.11) J = 10
c    interpolate
       XJ = 0.1*(J - 1)
       TERM =  (XB - XJ)/0.1
       FAHBI = FAHB(J) + (FAHB(J+1) - FAHB(J))*TERM
       RFAPI = RFAP(J) + (RFAP(J+1) - RFAP(J))*TERM
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RFGTVV(IU,IERR)
c
c      Reads a flame geometry table file opened on unit IU.
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
       READ (IU,ERR=20,END=20) NFGT
         READ (IU,ERR=20,END=20) (FAHB(I),I=1,11)
         READ (IU,ERR=20,END=20) (RFAP(I),I=1,11)
       IERR = 0
       RETURN
c
c    error; try old style
20     CLOSE(IU)
       IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  WFGTVV(IU,IERR)
c
c      Writes a flame geometry table to a file opened on unit IU.
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
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************

