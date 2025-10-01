c******************************************************************************
c
c      ASCII file conversion to SUD
c
c******************************************************************************
c   file names
       CHARACTER*32 SSUDFILE,SDATFILE
c------------------------------------------------------------------------------
       CHARACTER*8 CHEM,REF
       CHARACTER*2 ATOMR(6)
       REAL        S0F(60),HMF(60)
c------------------------------------------------------------------------------
c    all integers are *2 in .SUD fils
       INTEGER*2   NAR2(6),ITHM2,ITHL2
c------------------------------------------------------------------------------
c    get filenames
c
1      WRITE (*,2)
2      FORMAT (/' ASCII .DAT file conversion to .SUD'/
     ;          ' Enter .DAT file name > ',$)
       READ (*,3,ERR=1) SDATFILE
3      FORMAT (A32)
c
       WRITE (*,4)
4      FORMAT (' Enter .SUD file name > ', $)
       READ (*,3,ERR=1) SSUDFILE
c
c    open the files
       OPEN (10,FILE=SDATFILE,ERR=60)
       OPEN (9,FILE=SSUDFILE,FORM='UNFORMATTED',ERR=60)
c
10     READ (10,12,END=50,ERR=70) CHEM,REF,WMF,DHFF,ITHL2,DCSF,
     ;    (NAR2(K),ATOMR(K),K=1,6),ITHM2,
     ;    (S0F(L),L=1,60),(HMF(L),L=1,60)
12      FORMAT (A,2X,A,F10.5,F10.3,I2,F8.3,6(I3,A),I2,12(/10F8.3))
c
       WRITE (*,*) 'Converting to .SUD: ', CHEM
c
c    write the file
       WRITE (9,ERR=70) CHEM,REF,WMF,DHFF,ITHL2,DCSF,
     ;  (NAR2(K),ATOMR(K),K=1,6),ITHM2,
     ;  (S0F(L),L=1,60),
     ;  (HMF(L),L=1,60)
       GOTO 10
c
50     ENDFILE(9)
       CLOSE(9)
       CLOSE(10)
       STOP
c
60     write (*,*) 'error opening .dat file'
c
70     write (*,*)' file error'
       STOP
       END
c******************************************************************************
