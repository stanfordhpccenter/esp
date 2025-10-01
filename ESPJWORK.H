c******************************************************************************
c
c      Work array dimensioning for STANJAN and ESPJAN
c
c******************************************************************************
c
c    Dimension parameters
c          NAMAX        maximum number of atom types
c          NSMAX        maximum number of species
c          NAMAX3  =  3*NAMAX
c          NSMAX2  =  2*NSMAX
c          NWORK   =  max(2*NAMAX,NAMAX+NPMAX)
c          NIWORK  =  22 + 14*NAMAX + 4*NPMAX + 8*NSMAX + 2*NAMAX*NSMAX
c          NRWORK  =  24 + 16*NAMAX + 12*NAMAX*NAMAX + 3*NPMAX*NAMAX + 6*NPMAX
c                    + 18*NSMAX + NWORK*NWORK + NWORK
c          NSWORK = 123*NSMAX
c.............................................................................
c   Use the following statements for 20 species, 8 atoms, 6 phases:
       PARAMETER (NAMAX = 8)
       PARAMETER (NAMAX2 = 16)
       PARAMETER (NAMAX3 = 24)
       PARAMETER (NPMAX = 6)
       PARAMETER (NSMAX = 20)
       PARAMETER (NSMAX2 = 40)
       PARAMETER (NIWORK = 638)
       PARAMETER (NRWORK = 1732)
       PARAMETER (NSWORK = 2460)
c.............................................................................
c   Use the following statements for 80 species, 8 atoms, 6 phases:
C      PARAMETER (NAMAX = 8)
C      PARAMETER (NAMAX2 = 16)
C      PARAMETER (NAMAX3 = 24)
C      PARAMETER (NPMAX = 6)
C      PARAMETER (NSMAX = 80)
C      PARAMETER (NSMAX2 = 160)
C      PARAMETER (NIWORK = 2078)
C      PARAMETER (NRWORK = 2812)
C      PARAMETER (NSWORK = 9840)
c------------------------------------------------------------------------------
c   Use the following statements for 100 species, 10 atoms, 4 phases:
C      PARAMETER (NAMAX = 10)
C      PARAMETER (NAMAX2 = 20)
C      PARAMETER (NAMAX3 = 30)
C      PARAMETER (NPMAX = 4)
C      PARAMETER (NSMAX = 100)
C      PARAMETER (NSMAX2 = 200)
C      PARAMETER (NIWORK = 2978)
C      PARAMETER (NRWORK = 3748)
C      PARAMETER (NSWORK = 12300)
c******************************************************************************

