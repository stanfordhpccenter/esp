c******************************************************************************
c
c      Compile this file for compatablity with Microsoft compiler
c
c      Using Digital Visual Fortran, set i/o compatability with power pc.
c
c******************************************************************************
c
       SUBROUTINE  RVPOVV(IU,IMAX,NUVP,ND,FMAX,AVD,FVD,IERR)
c
c      Reads an old-style valve program from opened unit IU.
c      Returns IERR = 0 ok, 1 if not.
c------------------------------------------------------------------------------
c      Nomeclature:
c          IMAX     maximum number of data points (48 in V1)
c          NUVP    user valve program name
c          ND      number of data points
c          FMAX    maximum value of relative lift
c          IVD     list of specified angles from valve open angle, deg (1,ND)
c          FVD     list of corresponding lifts (arbitrary units)
c------------------------------------------------------------------------------
       CHARACTER*48    NUVP
       INTEGER*2       ND2,IVD2(48)
       DIMENSION       AVD(IMAX),FVD(IMAX)
c------------------------------------------------------------------------------
c    read the file
       READ (IU,ERR=20,END=20) NUVP,ND2,FMAX
c    convert integer
       ND = ND2
       READ (IU,ERR=20,END=20) (IVD2(I),I=1,ND),(FVD(I),I=1,ND)
c    convert integer*2 angles to floating point
       DO I=1,ND
           AVD(I) = IVD2(I)
           ENDDO
       IERR = 0
       RETURN
c    error
20     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE OPUOVV(IU,FILEN,IERR)
c
c      Opens unformated file on unit IU. Returns IERR=0 if ok, 1 if not.
c------------------------------------------------------------------------------
       CHARACTER*32    FILEN
c------------------------------------------------------------------------------
       OPEN(IU,FILE=FILEN,FORM='UNFORMATTED',ERR=2)
       IERR = 0
       RETURN
c    error
2      IERR = 1
       RETURN
       END
c******************************************************************************
