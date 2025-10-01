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
       SUBROUTINE OUFOVV(IU,FNAME,IERR)
c
c      Opens unformatted file FNAME on unit IU. Returns IERR=0 if OK, else 1.
c------------------------------------------------------------------------------
       CHARACTER*32  FNAME
c------------------------------------------------------------------------------
       OPEN (IU,FILE=FNAME,FORM='UNFORMATTED',ERR=10)
       IERR = 0
       RETURN
10     IERR = 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  RSOVV(IU,CHEM,REF,WMF,DHFF,ITHL2,DCSF,
     ;      NAR2,ATOMR,ITHM2,S0F,HMF,IERR)
c
c      Reads one species from old style .SUD file
c------------------------------------------------------------------------------
       CHARACTER*8  CHEM,REF
       CHARACTER*2  ATOMR(6)
       REAL         S0F(60),HMF(60)
c    .SUD intgers are all *2
       INTEGER*2    NAR2(6),ITHL2,ITHM2
c------------------------------------------------------------------------------
c    read the next species data
       READ (IU,END=50,ERR=60) CHEM,REF,WMF,DHFF,ITHL2,DCSF,
     ; (NAR2(K),ATOMR(K),K=1,6),ITHM2,
     ; (S0F(L),L=1,60),
     ; (HMF(L),L=1,60)
       IERR = 0
       RETURN

c    end of processing
50     IERR = 1
       RETURN
c
c    read error
60     IERR = 2
       RETURN
       END
c******************************************************************************
