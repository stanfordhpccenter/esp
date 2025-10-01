c******************************************************************************
c
c      Valve program table handling routines
c
c******************************************************************************
c
       SUBROUTINE  RVPTVV(IU,NUVP,ND,FMAX,AVD,FVD,IERR)
c
c      Reads a valve program table from opened unit IU.
c      Returns IERR = 0 ok, 1 if not.
c------------------------------------------------------------------------------
c      Nomeclature:
c          NUVP    user valve program name
c          ND      number of data points
c          FMAX    maximum value of relative lift
c          AVD     list of ND specified angles (from valve open angle, deg)
c          FVD     list of corresponding lifts (arbitrary units)
c------------------------------------------------------------------------------
       CHARACTER*48    NUVP
       DIMENSION       AVD(48),FVD(48)
c------------------------------------------------------------------------------
c    read the file
       READ (IU,ERR=20,END=20) NUVP,ND,FMAX
       READ (IU,ERR=20,END=20) (AVD(I),I=1,ND),(FVD(I),I=1,ND)
       IERR = 0
       RETURN
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  WVPTVV(IU,NUVP,ND,FMAX,AVD,FVD,IERR)
c
c      Writes a valve program table to unformatted file opened on unit IU.
c      Returns IERR = 0 ok, 1 if not.
c------------------------------------------------------------------------------
c      Nomeclature:
c          NUVP    user valve program name
c          ND      number of data points
c          FMAX    maximum value of relative lift
c          IVD     list of specified angles from valv open angle, deg (1,ND)
c          FVD     list of corresponding lifts (arbitrary units)
c------------------------------------------------------------------------------
       CHARACTER*48    NUVP
       DIMENSION       AVD(48),FVD(48)
c------------------------------------------------------------------------------
c    write the file
       WRITE (IU,ERR=20) NUVP,ND,FMAX
       WRITE (IU,ERR=20) (AVD(I),I=1,ND),(FVD(I),I=1,ND)
       IERR = 0
       RETURN
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************

