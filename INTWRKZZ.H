c    general setup work arrays
       PARAMETER   (NCMAXZ = 6144)
       PARAMETER   (NIMAXZ = 2048)
       PARAMETER   (NRMAXZ = 256)
       CHARACTER*1 CD
       COMMON /SETCZZ/ CD(NCMAXZ)
       COMMON /SETIZZ/ ID(NIMAXZ)
       COMMON /SETRZZ/ RD(NRMAXZ)
c------------------------------------------------------------------------------
