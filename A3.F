c******************************************************************************
c
c      ESPJAN help messages
c
c******************************************************************************
c
       SUBROUTINE HVERVV
c
c      Opening header and version display
c------------------------------------------------------------------------------
c    version
       CHARACTER*8     VERS
       COMMON /VERSVV/ VERS
c------------------------------------------------------------------------------
       CHARACTER*44    HEADER
       CHARACTER*96    LINE
       DATA    HEADER/
     ;  '@ESPJAN Gas Properties Preparation for ESP  '/
c------------------------------------------------------------------------------
       ML = 0
       CALL LOADZZ(HEADER,44,LINE,ML)
       CALL LOADZZ(VERS,9,LINE,ML)
       CALL LOADZZ('@@',2,LINE,ML)
       CALL MESSZZ('@',LINE)
       CALL MESSZZ('#','Copyright (c) 1987,1997,2000 Stanford '//
     ;              'University.##')
       CALL MESSZZ('#','Developed by W.C. Reynolds for use in '//
     ;                 'the Stanford Engine Simulation Program ESP '//
     ;                 'featured in Lumley''s introductory book '//
     ;                 'on internal combustion engines, published '//
     ;                 'in 1999 by the Cambridge University Press.# #'//
     ;        'Address all queries on ESP to  wcr@thermo.stanford.edu'//
     ;        '  or to# #'//
     ;        '      Prof. W.C. Reynolds#'//
     ;        '      Department of Mechanical Engineering#'//
     ;        '      Stanford University#'//
     ;        '      Stanford, CA 94305.##')
       CALL PAUSZZ(2)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HINSVV
c
c      Briefing
c------------------------------------------------------------------------------
       CALL MESSZZ('@','@ESPJAN calculates the specific enthalpy and '//
     ;         'energy of reactants and product and prepares a data '//
     ;         'table for use by ESP. The element potential method '//
     ;         'for chemical equilibrium analysis, developed at '//
     ;         'Stanford by W.C. Reynolds and implemented in his '//
     ;         'STANJAN program, is used for this calculation.@@')
       CALL PAUSZZ(0)
       CALL HSUDVV
       CALL PAUSZZ(0)
       CALL HRCTVV
       CALL PAUSZZ(0)
       CALL HPRDVV
       CALL PAUSZZ(0)
       CALL HPSEVV
       CALL PAUSZZ(0)
       CALL HNMEVV
       CALL PAUSZZ(0)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HSUDVV
c
c      .SUD file
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;        '@ESPJAN requires a .SUD data file containing the '//
     ;        'atomic composition and thermodynamic data for every '//
     ;        'species to be included in either the reactants or '//
     ;        'products.  ESP.SUD is the default STANJAN data file '//
     ;        'loaded when you start ESPJAN.  You can edit this '//
     ;        'file using the STANJAN data file manager JANFILE, '//
     ;        'which is part of the STANJAN program package.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HRCTVV
c
c      Reactant help
c------------------------------------------------------------------------------
       CALL MESSZZ('@','@You first specify the reactant species '//
     ;     'and moles (fuel and oxidizer) for a unit reaction '//
     ;     '(the moles are only relatve).  ESP treats the reactant '//
     ;     'mixture as an ideal gas with variable specific heats.  '//
     ;     'You can mix simple hydrocarbon gases in the data file '//
     ;     'to obtain the enthalpy of formation per kg '//
     ;     'and C:H ratio of a more complex fuel.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HPRDVV
c
c      Product help
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;     '@You specify the species in the products.  ESPJAN '//
     ;     'determines the amounts of each to match the '//
     ;     'atomic populations of the reactants assuming '//
     ;     'thermodynamic equilibrium of the set of species '//
     ;     'included in the products.  It is important to include '//
     ;     'all major product species (e.g. CO2, H2O, N2, O2,) and '//
     ;     'important minor species (e.g. HO, CO, NO2,..).@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HPSEVV
c
c      Pressure help
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;     '@ESP simplifies its calculation by assuming that the '//
     ;     'product gas composition is independent of pressure.  '//
     ;     'This is an acceptable approximation for the energy '//
     ;     'analysis done by ESP.  You specify the pressures '//
     ;     'at which ESPJAN evaluates the product gas '//
     ;     'composition and reactant and product ' //
     ;     'thermodynamic properties.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HNMEVV
c
c      Name help
c------------------------------------------------------------------------------
       CALL MESSZZ('@','@You specify a name for your properties '//
     ;     'model.  This name is used in ESP. You also specify '//
     ;     'the name of the .ESJ file that ESPJAN saves for use by '//
     ;     'ESP.  When you load this file in ESP and save as a '//
     ;     '.ESS setup file, the properties data are saved in the '//
     ;     'setup file and are reloaded whenever that setup file '//
     ;     'is used by ESP.@@')
       RETURN
       END
c******************************************************************************
