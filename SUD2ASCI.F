c******************************************************************************
c
c      SUD file conversion to ASCII
c
c******************************************************************************
c   file names
       CHARACTER*32 SSUDFILE,SDATFILE
c------------------------------------------------------------------------------
       CHARACTER*8 CHEM,REF
       CHARACTER*2 ATOMR(6)
       REAL        S0F(60),HMF(60)
c------------------------------------------------------------------------------
c    .SUD file integers are all *2
       INTEGER*2   NAR2(6),ITHM2,ITHL2
c------------------------------------------------------------------------------
c    get filenames
1      WRITE (*,2)
2      FORMAT (/' .SUD file conversion to ASCII .DAT file'/
     ;          ' Enter .SUD file name > ', $)
       READ (*,3,ERR=1) SSUDFILE
3      FORMAT (A32)
c
5      WRITE (*,6)
6      FORMAT (' Enter .DAT file name > ',$)
       READ (*,3,ERR=5) SDATFILE
c
c    open the files
       OPEN (9,FILE=SSUDFILE,FORM='UNFORMATTED',ERR=60)
       OPEN (10,FILE=SDATFILE,ERR=60)
c
c   read a line from the file
10     READ (9,ERR=20,END=50) CHEM,REF,WMF,DHFF,ITHL2,DCSF,
     ;  (NAR2(K),ATOMR(K),K=1,6),ITHM2,
     ;  (S0F(L),L=1,60),
     ;  (HMF(L),L=1,60)
c
c
c   write line to the  file
c
       WRITE (*,*) 'new file converted to ASCII: ',CHEM
c
       WRITE (10,12,ERR=60) CHEM,REF,WMF,DHFF,ITHL2,DCSF,
     ;    (NAR2(K),ATOMR(K),K=1,6),ITHM2,
     ;    (S0F(L),L=1,60),(HMF(L),L=1,60)
12      FORMAT (A,2X,A,F10.5,F10.3,I2,F8.3,6(I3,A),I2,12(/10F8.3))
       GOTO 10
c
c    try old style file read
20     CLOSE(9)
c    open the file old style
       CALL OUFOVV(9,SSUDFILE,IERR)
       IF (IERR.NE.0)  GOTO 60
c    read a line from the file
21     CALL RSOVV(9,CHEM,REF,WMF,DHFF,ITHL2,DCSF,
     ;       NAR2,ATOMR,ITHM2,S0F,HMF,IERR)
       IF (IERR.EQ.2)  GOTO 60
       IF (IERR.EQ.1)  GOTO 50
c
c   write file
c
       WRITE (*,*) 'old file converted to ASCII: ',CHEM
c
        WRITE (10,12,ERR=60) CHEM,REF,WMF,DHFF,ITHL2,DCSF,
     ;    (NAR2(K),ATOMR(K),K=1,6),ITHM2,
     ;    (S0F(L),L=1,60),(HMF(L),L=1,60)
       GOTO 21
c
c    close after successful conversion
50     CLOSE(9)
       ENDFILE(10)
       CLOSE(10)
       STOP
c
60     write (*,*) 'file error'
       CLOSE(10)
       STOP
       END
c******************************************************************************
