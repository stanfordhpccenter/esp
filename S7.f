c******************************************************************************
c
c      Help subroutines
c
c******************************************************************************
c
       SUBROUTINE COPEVV(ESC,SOURCE,DEST)
c
c      Copies ESC-terminated string SOURCE to string DEST
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,SOURCE(*),DEST(*)
c------------------------------------------------------------------------------
       I = 1
1      IF (SOURCE(I).EQ.ESC) RETURN
       DEST(I) = SOURCE(I)
       I = I + 1
       GOTO 1
       END
c******************************************************************************
c
       SUBROUTINE COPNVV(N,SOURCE,DEST)
c
c      Copies N characters from string SOURCE to string DEST
c-----------------------------------------------------------------------------
       CHARACTER*1 SOURCE(*),DEST(*)
c-----------------------------------------------------------------------------
       DO I=1,N
           DEST(I) = SOURCE(I)
           ENDDO
       RETURN
       END
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
       CHARACTER*47     HEADER
       CHARACTER*1      STRING(47)
       DATA    HEADER/'@Stanford Engine Simulation Program V2345678@@'/
c------------------------------------------------------------------------------
       CALL COPNVV(47,HEADER,STRING)
       CALL COPNVV(8,VERS,STRING(37))
       CALL MESSZZ('@',STRING)
       CALL MESSZZ('#','Copyright (c) 1987,1997,2000 Stanford '//
     ;              'University.##')
       CALL MESSZZ('#','#Developed by W.C. Reynolds, improved in '//
     ;                 'collaboration with J.L. Lumley, and '//
     ;                 'featured in Lumley''s introductory book '//
     ;                 'on internal combustion engines, published '//
     ;                 'in 1999 by the Cambridge University Press.# #'//
     ;        'Address all queries on ESP to  wcr@thermo.stanford.edu'//
     ;        '  or to# #'//
     ;        '      Prof. W.C. Reynolds#'//
     ;        '      Department of Mechanical Engineering#'//
     ;        '      Stanford University#'//
     ;        '      Stanford, CA 94305.##')
c
       CALL PAUSZZ(2)
c
       CALL MESSZZ('@','@This is the version of '//
     ;     'ESP with the manifold model developed by me and '//
     ;     'outlined in Lumley''s book. The science and mathematics '//
     ;     'of this model are now developed, and you can test it '//
     ;     'using this version, which provides indicator plots, '//
     ;     'manifold pressure and velocity plots, plots of '//
     ;     'the informative cylinder variables, and tabular '//
     ;     'output.@ @'//
     ;     'Command-line and GUI versions '//
     ;     'function the same general way on PC, Mac, Unix, '//
     ;     'and Linux platforms.  Check our website '//
     ;     '(esp.stanford.edu) for currently available downloads.  '//
     ;     'Old versions should be discarded.@ @'//
     ;     'W.C.Reynolds, Stanford University, January 9, 2001@@')
c
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HOVRVV
c
c      Model overview
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @ESP calculates the thermodynamic performance of a '//
     ;     'homogeneous charge spark ignition engine using a '//
     ;     'zero-dimensional model (ordinary differential '//
     ;     'equations), with one in-cylinder zone '//
     ;     'during gas exchange, compression, and expansion '//
     ;     'and two zones during combustion.  It uses a '//
     ;     'one-equation (ODE) turbulence model to track the '//
     ;     'large-scale turbulent kinetic energy and uses this '//
     ;     'turbulence velocity in heat transfer and combustion '//
     ;     'models. The manifold gasdynamic model uses ODEs '//
     ;     'based on the method of characteristics and '//
     ;     'models for the acoustic time delays.@ @'//
     ;     'ESP can be used to study various valve and '//
     ;     'piston programs, various fuels and oxidixers, '//
     ;     'effects of turbulence, impact of reduced heat '//
     ;     'heat transfer, manifold tuning, spark timing, '//
     ;     'and other design options.@@')
       CALL PAUSZZ(0)
       CALL MESSZZ('@',
     ;   ' @The model is based on energy and mass balances '//
     ;     'on the various zones. There are four calculation '//
     ;     'stages:@ @'//
     ;     '  compression; begins when intake valve closes@'//
     ;     '  burn; begins at ignition@'//
     ;     '  expansion; begins at end of burn@'//
     ;     '  gas exchange; begins when exhaust valve opens.@ @'//
     ;     'You can monitor the history of any of the model '//
     ;     'variables to study the behavior of the model, '//
     ;     'which will now be explained.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HPISVV
c
c      Instruction on piston programs
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @The piston motion is described by a piston program, '//
     ;     'which gives the position from TDC and its angular '//
     ;     'rate of change.  This version of ESP offers two '//
     ;     'options: @ @'//
     ;     ' 1 Standard crankshaft@'//
     ;     '    (specified stroke and connecting rod length)@ @'//
     ;     ' 2 Dual-stroke cosine@'//
     ;     '    (specified expansion/exhaust and '//
     ;     'intake/compression strokes)@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HVALVV
c
c      Instruction on valve programs
c------------------------------------------------------------------------------
       CALL MESSZZ('#',
     ;  ' #The valve lifts are described by valve programs, '//
     ;    'which give the fraction of flow area open as a '//
     ;    'function of crank angle.  This version of ESP offers '//
     ;    'three options:# #'//
     ;    ' 1. cosine:#'//
     ;    '     A/Amax = '//
     ;    '{1-cos[2*pi*(A-Aso)/(Afc-Aso)]}/2# #'//
     ;    ' 2. cosine-flat-cosine:#'//
     ;    '     A/Amax = '//
     ;    '{1-cos[pi*(A-Aso)/(Afo-Aso)]}/2  Aso < A < Afo#'//
     ;    '     A/Amax = '//
     ;    '1                                Afo < A < Asc#'//
     ;    '     A/Amax = '//
     ;    '{1+cos[pi*(A-Asc)/(Asc-Afc)]}/2  Asc < A < Afc# #'//
     ;    ' 3. user-specified table with angles in arbitrary units#'//
     ;    '     A/Amax = F(A-Aso)  (shape stretched to fit dwell)# #'//
     ;    'Notation:  A = crank angle from TDC compression#'//
     ;    ' Aso @ start open; Afo @ full open;'//
     ;    ' Asc @ start close; Afc @ full close.##')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HVVCVV
c
c      Instruction on valve control
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;  '@The valve programs defined with the engine gegometry give '//
     ;   'the form of the valve flow area variation.  The valve '//
     ;   'control operating parameters include the starting and '//
     ;   'ending crank angles for the valve events and '//
     ;   'factors to increase or decrease the maximum '//
     ;   'flow areas from the reference values defined '//
     ;   'with the engine geometry.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HHTMVV
c
c      Instruction on the heat transfer model
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;  ' @The heat transfer rate q is computed from@ @'//
     ;    '               q = h*A*(Tg - Tw)@ @'//
     ;    'where Tg is the computed gas temperature, Tw is the '//
     ;    'specified wall temperature, h is the convective heat '//
     ;    'transfer coefficient, and A is the pertinent surface '//
     ;    'area.  Values for h are calculated from specified '//
     ;    'Stanton numbers.  Stanton '//
     ;    'numbers in the cylinder are defined by@ @'//
     ;    '               St = h/(v*rho*Cp)@ @'//
     ;    'where v is the large-scale turbulence velocity, rho '//
     ;    'is the fluid density, and Cp is the gas specific '//
     ;    'heat at constant pressure. For heat transfer from the '//
     ;    'valve flow, the Stanton number is instead based on the '//
     ;    'velocity through the valve.  The Stanton numbers '//
     ;    'can be adjusted to match overall heat transfer rates '//
     ;    'and polytropic exponents from engine experiments.@@')
       CALL PAUSZZ(0)
       CALL MESSZZ('@',
     ;   ' @The heat transfer areas exclusive of the cylinder '//
     ;     'and piston are specified in ratio to the bore area.  '//
     ;     'The effect of inlet jet impingement can be studied '//
     ;     'by varying the area for inlet valve heat transfer. '//
     ;     'Heat transfer to the head from the impinging exhaust '//
     ;     'jet can be included by enlarging the heat transfer '//
     ;     'area specified for the exhaust valve flow.  '//
     ;     'Separate wall temperatures are specified for the '//
     ;     'in-cylinder surfaces, the inlet valve heat transfer '//
     ;     'surface, and the exhaust valve heat transfer surface. '//
     ;     'This allows one to study the result of insulating '//
     ;     'these surfaces.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HFLOVV
c
c      Instruction on the valve flow model
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @The valve flow rates are calculated using specified '//
     ;     'discharge coefficients, defined as the ratio of the '//
     ;     'actual flow rate to that predicted by isentropic '//
     ;     'flow theory for the same inlet state and exit pressure.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HTRBVV
c
c      Instruction on the turbulence model
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @The turbulence model uses the equation for turbulent '//
     ;     'kinetic energy @ @'//
     ;     '               k = (1/2)*v**2@ @'//
     ;     'where v is the large-scale turbulence velocity, '//
     ;     'averaged over the volume.  The '//
     ;     'time evolution of k is governed by@ @'//
     ;     '           dk/dt = P - D + {(I - R - k*dM/dt)/M}@ @'//
     ;     'where I and R are the rates of input and removal '//
     ;     'of turbulence energy by flow '//
     ;     'through the valves, P is the rate of production of '//
     ;     'turbulence energy by mean '//
     ;     'deformation, D is the rate of dissipation of turbulence '//
     ;     'energy by eddy-eddy interactions, '//
     ;     'and M is the mass in the cylinder. @@')
           CALL PAUSZZ(0)
           CALL MESSZZ('@','@ The production and dissipation models '//
     ;     'are@ @'//
     ;     '   P = Fp*(A/V)|Vp|^3 - (2/3)*k*(1/V)*dV/dt@'//
     ;     '   D = - Fd*k*v/V^(1/3).@ @'//
     ;     'Here |Vp| is the absolute piston velocity, A is the '//
     ;     'heat transfer area, V is the cylinder volume, and '//
     ;     'Fp and Fd are model constants.  The first term in P '//
     ;     'models the production rate due to shear caused by '//
     ;     'piston motion, and the second term models the effect '//
     ;     'of compression as predicted by rapid distortion theory.  '//
     ;     'In addition to specifying Fp and Fd for each stage of '//
     ;     'the cycle, you specify the ratio of the turbulence '//
     ;     'energy input associated with inflow through the '//
     ;     'valves to the mean inflow kinetic energy.  See the '//
     ;     'default setup for approximate orders of magnitudes '//
     ;     'of Fp and Fd.  Increasing Fp or reducing Fd will '//
     ;     'increase the turbulence at TDC and hence will '//
     ;     'produce faster burning.  Fp should be larger for '//
     ;     'high squish configurations.  You can explore the '//
     ;     'influence of turbulence by trying different constants.@@')
       CALL PAUSZZ(0)
       CALL MESSZZ('@',
     ;   ' @During burn, the turbulence kinetic energy of the '//
     ;     'unburned gas is also calculated using the kinetic '//
     ;     'energy equation written following a fluid element '//
     ;     'in the zone ({} terms in k equation = 0).  An extra '//
     ;     'production term is added in each zone,@ @'//
     ;     '     (2/3)*ku*(1/r)*dr/dt@ @'//
     ;     'where ku is the kinetic energy of the gas '//
     ;     'and r is its density.  This term models the effect '//
     ;     'of rapid compression of the unburned gas, '//
     ;     'which increases the turbulence and flame speed.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HFGMVV
c
c      Instruction on the flame geometry model
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @The burn model assumes that the turbulence '//
     ;     'wrinkles the flame about the location it would have '//
     ;     'for a laminar burn. The model requires (1) '//
     ;     'the ratio of the projected flame '//
     ;     'area to the bore area and (2) the fraction of the heat '//
     ;     'transfer area behind the flame, as functions of the '//
     ;     'fraction of volume occupied by burned gas. This version ' //
     ;     'of ESP offers two options:@ @'//
     ;     ' 1 Precalculated flame geometry table@ @'//
     ;     ' 2 Cylindrical burn from center@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HFGTVV
c
c      Instruction on the flame geometry table
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;     '@The flame geometry table must give '//
     ;     'the ratio of the projected flame '//
     ;     'area to the bore area and the fraction of the heat '//
     ;     'transfer area behind the flame as functions of the '//
     ;     'fraction of volume occupied by burned gas. You first ' //
     ;     'generate these values by separate geometrical '//
     ;     'calculations, then enter them in ESP, saving as '//
     ;     'part of the .ESS setup file or in a .ESF flame geometry '//
     ;     'table file.  The calculations must be made for the '//
     ;     'combustion chamber geometry at TDC (away from TDC the '//
     ;     'tabulated flame area is increased by the ratio '//
     ;     'V/V_clearance).  The flame geometry table requires '//
     ;     'entries at burned volume fractions from 0 to 1 at '//
     ;     'intervals of 0.1. The last few entries influence the '//
     ;     'end of burn very significantly, and can be adjusted '//
     ;     'to match experimental indicator data.  The flame area '//
     ;     'at 1 should be non-zero.@ @The model can be used to '//
     ;     'explore the variation of burn time with combustion '//
     ;     'chamber geometry and spark plug location.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HFPMVV
c
c      Instruction on the flame propagation model
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @The flame propagation model assumes that the flame '//
     ;    'originates with the ignition of a specified mass '//
     ;    'fraction of the reactants  (not zero, typically 0.02).  '//
     ;    'The model also assumes that the projected flame front '//
     ;    'propagates at a speed equal to the sum of a specified '//
     ;    'laminar flame speed and a turbulent flame speed '//
     ;    'proportional to the turbulence velocity in the '//
     ;    'unburned zone, computed by the turbulence model.  '//
     ;    'The proportionality factor should be approximately 1.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HPROVV
c
c      Instruction on thermodynamic properties.
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @Thermodynamic properties of the reactants and '//
     ;     'products are required.  The model assumes that the '//
     ;     'enthalpy and molal mass are independent of pressure. '//
     ;     'These data are precalculated as functions of '//
     ;     'temperature using the companion program ESPJAN, a '//
     ;     'derivative of our chemical equilibrium program STANJAN.  '//
     ;     'When you run ESPJAN you specify the fuel, oxidizer, '//
     ;     'and allowable product species, and select the '//
     ;     'pressures at which the properties of the reactants and '//
     ;     'products are evaluated.  The errors caused by '//
     ;     'neglecting pressure dependence will be minimized if '//
     ;     'you specify pressures typical of the highest '//
     ;     'temperatures encountered by reactants and products.  '//
     ;     'ESPJAN creates .ESJ files that can be read by ESP '//
     ;     'and saved as part of the setup file.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HPRSVV
c
c      Explains properties energy shift
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;     'With mass flow to/from the cylinder, the values of the '//
     ;     'energy inflow/outflow rates and energy change rates '//
     ;     'depend on the datum state for the thermochemical data,  '//
     ;     'but the datum does not matter to the complete energy '//
     ;     'balance.  In order to make the energy rate plot easier '//
     ;     'to understand, ESP shifts the datum state to make '//
     ;     'pure products have zero internal energy at 300K.@@')
           RETURN
           END
c******************************************************************************
c
       SUBROUTINE HSETVV
c
c      Instruction on setup.
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @You begin by preparing a setup, which describes the '//
     ;     'engine, the parameters in the model elements, and the '//
     ;     'operating conditions.  You can save the setup in a file '//
     ;     'for future reload.  The setup file SETUP.ESS contains '//
     ;     'the default setup loaded when you first start ESP. '//
     ;     'You can examine or modify the setup before any run.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HRUNVV
c
c      Instruction on run.
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @You should first run some cycles to check convergence.  '//
     ;     'With no manifolds, three or four cycles are usually '//
     ;     'sufficient.  With manifolds many more (10-50) are '//
     ;     'required, especially near a resonance condition.  '//
     ;     'Hence for some applications you may find it much '//
     ;     'faster to set up without manifolds.@'//
     ;   ' @After you are satisfied with the convergence, you can '//
     ;     'run again and make plot files for any of the variables '//
     ;     'vs. crank angle, and/or make an indicator plot file '//
     ;     '(Pcyl vs. Vcyl). You can also save data in files. '//
     ;     'When you have finished running for the '//
     ;     'current setup, you can return to change the setup or '//
     ;     'exit ESP.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HMLPVV
c
c      Instruction on MATLAB plots.
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @ESP plot files are self-contained Matlab .M files that '//
     ;     'you can plot immediately using Matlab (V.5+).  '//
     ;     'Use the Matlab Set-Path control to tell Matlab where to '//
     ;     'find your .M  files. Then enter Matlab and type the file '//
     ;     'name (without the extent .M) and the plots will appear '//
     ;     'in the Matlab plot window.  Use the Matlab tools to set '//
     ;     'the font you prefer, then print. You can edit these .M '//
     ;     'files to make the plots suit your tastes.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HFILVV
c
c      Help with files
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;  ' @ESP uses several different types of files '//
     ; 'identified by their extents:@ @'//
     ; '  .ESS   setup file (unformatted)@'//
     ; '  .ESF   flame geometry file (unformatted)@'//
     ; '  .ESV   special valve lift program (unformatted)@'//
     ; '  .ESJ   property file, created by ESPJAN (unformatted)@'//
     ; '  .ESM   model output data file (ASCII formatted)@'//
     ; '  .ESR   run output data file (ASCII formatted)@'//
     ; '  .M     Matlab script file for plots (ASCII formatted)@'//
     ; '  .MON   temporary run monitor files (ok to delete)@'//
     ; '  .TMP   temporary scratch files (ok to delete)@'//
     ; '  .BAD   diagnostics for manifold convergence failure@ @'//
     ; '.ESS, .ESF, and .M files are created using ESP.  '//
     ; '.ESJ files are prepared using ESPJAN, and .ESV files are '//
     ; 'prepared using ESPCAM.@ @'//
     ; 'This version will read unformatted files prepared '//
     ; 'with the old V1+ ESP (without manifolds) and saves files '//
     ; 'in the new V2 format.@ @'//
     ; 'SETUP.ESS is the default setup file, which you can '//
     ; 'replace with your most used setup.@@')
       END
c******************************************************************************
c
       SUBROUTINE HTUNVV
c
c      Instruction on tuning the model.
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @The following steps are suggested for tuning the '//
     ;   'model to engine indicator data:@'//
     ;   '  1. Set the geometry model for the engine.@'//
     ;   '  2. Match the manifold geometry.@'//
     ;   '  3. Select an appropriate properties model for '//
     ;   'the fuel used.@'//
     ;   '  4. Adjust manifold blockages and valve '//
     ;   'coefficients to match pressures.@'//
     ;   '  5. Adjust turbulence coefficients '//
     ;   'and flame parameters to match burn.@'//
     ;   '  6. Adjust the Stanton numbers to match polytropic '//
     ;   'exponents and IMEP.@'//
     ;   '  7. Iterate as necessary to match the IMEP and '//
     ;   'indicator data.@'//
     ;   ' @The burn time is VERY sensitive to the flame geometry '//
     ;   'model, and the IMEP is VERY sensitive to burn time.  '//
     ;   'This is true in actual engines.@ @'//
     ;   'The velocity plot is useful in adjusting the turbulence '//
     ;   'model parametrs.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HIMMVV
c
c      Displays the intake manfold model help
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @The intake manifold model assumes a feeder that feeds '//
     ;     'the runners to the cylinders. You specify the length '//
     ;     'and diameter of the feeder, the length and diameter of '//
     ;     'the runner, the number of runners from the feeder, '//
     ;     'friction factors for the feeder and runner, '//
     ;     'and duct flow area blockages (throttles or eddies) at '//
     ;     'the feeder intake, feeder exit to the junction, and '//
     ;     'runner inlet from the junction.@'//
     ;   ' @Calculations take longer with a manifold, '//
     ;     'so you may prefer to run without manifolds.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HEMMVV
c
c      Displays the exhaust manfold model help
c------------------------------------------------------------------------------
       CALL MESSZZ('@',
     ;   ' @The exhaust manifold model assumes runners from '//
     ;     'the cylinders discharging into the collector '//
     ;     '(tailpipe).  You specify the length '//
     ;     'and diameter of the collector, the length and diameter '//
     ;     'of the runner, the number of runners to the collector, '//
     ;     'friction factors for the collector and runner, '//
     ;     'and flow blockages at '//
     ;     'the runner exit to the junction, the collector intake '//
     ;     'from the junction, and the collector exit.@'//
     ;   ' @Calculations take longer with a manifold, '//
     ;     'so you may prefer to run without manifolds.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HCONVV
c
c      Help for convergence
c------------------------------------------------------------------------------
       CALL MESSZZ('@','@Convergence can be checked by observing '//
     ; 'the one-line cycle summary display of the '//
     ; 'volumetric efficiency Nvol, '//
     ; 'fractional non-cyclic errors (FNCE) and '//
     ; 'fractional mass and energy balance errors:@'//
     ; '  t= |FNCE| for gas temperature@'//
     ; '  p= |FNCE| for gas pressure@'//
     ; '  v= |FNCE| for turbulence velocity@'//
     ; '  m= |cylinder mass balance error|/mass in@'//
     ; '  e= |cylinder energy balance error|/Net Work out@'//
     ; 'If you include manifolds, a line is displayed for each '//
     ; 'manifold giving the mass/cycle past the four manifold states '//
     ; 'and through the valve; see Lumley''s text for state '//
     ; 'locations. Convergence is faster and better without '//
     ; 'manifolds.@@ ')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HINTVV
c
c      Help with the interface
c------------------------------------------------------------------------------
       CALL MESSZZ('@','@ESP setup is done the same basic way '//
     ; 'on different computer platforms using command-line (CL) '//
     ; 'or graphical user interfaces (GUI). The setup data are '//
     ; 'grouped in sets that you give designators (names) so that '//
     ; 'you can identify the data when it is not fully displayed.  '//
     ; 'The main setup elements are@ @'//
     ; '  Run setup: (your designator)@'//
     ; '     Operating parameters: (your designator)@'//
     ; '     Engine Geometry: (your designator)@'//
     ; '     Model Parameters: (your designator)@'//
     ; '     Setup file save option@ @'//
     ; 'These have sub-elements, which have futher sub-elements.  '//
     ; 'With CL you work on one level at a time, moving down '//
     ; 'to reach a desired level and up a '//
     ; 'level when finished. With GUI you can randomly unzip each '//
     ; 'level to work on it and then zip it up for compactness. '//
     ; 'When you are finished with the setup you select the GUI '//
     ; 'action button or exit the top CL setup level and select the '//
     ; 'action.  The run task options work the same basic way.@@')
       CALL PAUSZZ(0)
       CALL MESSZZ('@','The sub-elements of the main setup '//
     ; 'elements are@ @'//
     ; '     Operating parameters: (your designator)@'//
     ; '        RPM@'//
     ; '        Firing timing@'//
     ; '        Ambient conditions: (your designator)@'//
     ; '        EGR specification (your designator)@'//
     ; '        Valve lift and timing (your designator)@'//
     ; '     Engine Geometry: (your designator)@'//
     ; '        Intake valve program: (your designator)@'//
     ; '        Exhaust valve program: (your designator)@'//
     ; '        Valve reference areas: (your designator)@'//
     ; '        Piston/cylinder: (your designator)@'//
     ; '     Model Parameters: (your designator)@'//
     ; '        Gas Properties: (your designator)@'//
     ; '        Valve flow: (your designator)@'//
     ; '        Heat transfer: (your designator)@'//
     ; '        Turbulence: (your designator)@'//
     ; '        Flame geometry: (your designator)@'//
     ; '        Flame propagation: (your designator)@'//
     ; '        Intake manifold: (your designator)@'//
     ; '        Exhaust manifold: (your designator)@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HINSVV
c
c      Instruction on the entire model
c------------------------------------------------------------------------------
c    clear output window
       CALL CLOWZZ
c    GUI start key
       CALL BLMSZZ(0)
c    overview
       CALL HOVRVV
       CALL PAUSZZ(0)
c    piston model
       CALL HPISVV
       CALL PAUSZZ(0)
c    valve model
       CALL HVALVV
       CALL PAUSZZ(0)
c   flow model
       CALL HFLOVV
       CALL PAUSZZ(0)
c   manifold model
       CALL HIMMVV
       CALL PAUSZZ(0)
       CALL HEMMVV
       CALL PAUSZZ(0)
c   heat transfer
       CALL HHTMVV
       CALL PAUSZZ(0)
c   turbulence model
       CALL HTRBVV
       CALL PAUSZZ(0)
c   burn model
       CALL HFGMVV
       CALL PAUSZZ(0)
       CALL HFPMVV
       CALL PAUSZZ(0)
c   properties model
       CALL HPROVV
       CALL PAUSZZ(0)
c   setup
       CALL HSETVV
       CALL PAUSZZ(0)
c   run
       CALL HRUNVV
       CALL PAUSZZ(0)
       CALL HCONVV
       CALL PAUSZZ(0)
c   Matlab plots
       CALL HMLPVV
       CALL PAUSZZ(0)
c   data reference shift
       CALL HPRSVV
       CALL PAUSZZ(0)
c   files
       CALL HFILVV
       CALL PAUSZZ(0)
c   tuning
       CALL HTUNVV
       CALL PAUSZZ(0)
c   interface
       CALL HINTVV
c   GUI end key
       CALL ELMSZZ(0)
c   exit
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE HPL5VV
c
c      Help on the variables plots
c------------------------------------------------------------------------------
       CALL MESSZZ('@','@You can make Matlab plot files of '//
     ;     'various quantities calculated for the cylinder.  '//
     ;     'Each plot contains five quantities plotted with '//
     ;     'colored lines and identified in the legend, '//
     ;     'plotted vs. crank angle for the full cycle.  '//
     ;     'The five quantities on each plot all have the same '//
     ;     'dimensions.  You specify the Matlab plot file name '//
     ;     'for each plot. The available quantities are:@ @'//
     ;     '  Burnt gas, unburnt gas, and other temperatures@'//
     ;     '  Product, reactant, and cumulative masses@'//
     ;     '  Mass flow rates and change rates@'//
     ;     '  Energy transfer rates and change rates@'//
     ;     '  Cumulative energies@'//
     ;     '  Flow and turbulence velocities@ @'//
     ;     'You can make all of these plots on the same engine '//
     ;     'cycle, and must use a different name for each plot. @ @'//
     ;     'Cumulative quantities begin from zero when the intake '//
     ;     'valve closes.@@')
           RETURN
           END
c******************************************************************************
