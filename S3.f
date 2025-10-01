c******************************************************************************
c
c      Quantity identification routines
c
c******************************************************************************
c
       SUBROUTINE RWIDVV(I,CODEI,NAMEI)
c
c      Returns 4-character code and 60-character name of RW(I).
c-----------------------------------------------------------------------------
       CHARACTER*4     CODEI,CODE(400)
       CHARACTER*60    NAMEI,NAME(400)
c-----------------------------------------------------------------------------
c
c    Initialize null (NOT ALLOWED BY SOME COMPILERS)
C      DATA CODE/400*'****'/
C      DATA NAME/400*
C    ; '                                                            '/
C
c  Alphanumeric codes and descriptions of the variables
c
c  Differential variables
       DATA CODE(1),NAME(1)/'UC  ',
     ; 'total internal energy of gas in cylinder, J                 '/
       DATA CODE(2),NAME(2)/'MP  ',
     ; 'mass of products in cylinder, kg                            '/
       DATA CODE(3),NAME(3)/'MR  ',
     ; 'mass of reactants in cylinder, kg                           '/
       DATA CODE(4),NAME(4)/'KT  ',
     ; 'turbulence kinetic energy/mass in cylinder, J/kg            '/
       DATA CODE(5),NAME(5)/'QC  ',
     ; 'heat transferred to coolant this cycle, J                   '/
       DATA CODE(6),NAME(6)/'WP  ',
     ; 'total work done on piston this cycle, J                     '/
       DATA CODE(7),NAME(7)/'MU  ',
     ; 'mass of unburned gas in cylinder, kg                        '/
       DATA CODE(8),NAME(8)/'KU  ',
     ; 'turbulende kinetic energy pr unit unburnd mass, J/kg        '/
       DATA CODE(9),NAME(9)/'TU  ',
     ; 'temperatur of unburnd gas, K                                '/
       DATA CODE(10),NAME(10)/'PU  ',
     ; 'pressure of unburned gas, J/kg                              '/
       DATA CODE(11),NAME(11)/'MIN ',
     ; 'mass in through intake valve this cycle, kg                 '/
       DATA CODE(12),NAME(12)/'HIN ',
     ; 'stagnation enthalpy input for cycle, J                      '/
       DATA CODE(13),NAME(13)/'HB  ',
     ; 'total enthalpy of intake backflow gas, J                    '/
       DATA CODE(14),NAME(14)/'MPB ',
     ; 'mass of products in intake backflow gas, kg                 '/
       DATA CODE(15),NAME(15)/'MRB ',
     ; 'mass of reactants in intake backflow gas, kg                '/
       DATA CODE(16),NAME(16)/'MEX ',
     ; 'mass out through exhaust valve this cycle, kg               '/
       DATA CODE(17),NAME(17)/'HEX ',
     ; 'total enthalpy of mixed exhausted gas this cycle, J         '/
       DATA CODE(18),NAME(18)/'MPE ',
     ; 'mass of products in exhaust gas this cycle, kg              '/
       DATA CODE(19),NAME(19)/'MRE ',
     ; 'mass of reactants in exhaust gas this cycle, kg             '/
c
c  Rates of change of differential variables
       DATA CODE(21),NAME(21)/'DUCD',
     ; 'rate of accumulation of energy (U) in cylinder, J/s         '/
       DATA CODE(22),NAME(22)/'DMPD',
     ; 'rate of accumulation of product in cylinder, kg/s           '/
       DATA CODE(23),NAME(23)/'DMRD',
     ; 'rate of accumulation of reactants in cylinder, kg/s         '/
       DATA CODE(24),NAME(24)/'DKTD',
     ; 'dK/dt, J/kg-s                                               '/
       DATA CODE(25),NAME(25)/'QDOT',
     ; 'heat transfer rate to coolant, J/s                          '/
       DATA CODE(26),NAME(26)/'WDOT',
     ; 'power output to piston, J/s                                 '/
       DATA CODE(27),NAME(27)/'DHHD',
     ; 'dHH/dt, J/s                                                 '/
       DATA CODE(28),NAME(28)/'DHUD',
     ; 'dHU/dt, J/s                                                 '/
       DATA CODE(29),NAME(29)/'DMUD',
     ; 'dMU/dt, kg/s                                                '/
       DATA CODE(30),NAME(30)/'DKUD',
     ; 'dKUD/dt, J/kg-s                                             '/
       DATA CODE(31),NAME(31)/'DMIN',
     ; 'dMIN/dt, kg/s                                               '/
       DATA CODE(32),NAME(32)/'DHIN',
     ; 'energy inflow rate through intake valve, J/s                '/
       DATA CODE(33),NAME(33)/'DHBD',
     ; 'dHB/dt, J/s                                                 '/
       DATA CODE(34),NAME(34)/'DMPB',
     ; 'dMPB/dt, kg/s                                               '/
       DATA CODE(35),NAME(35)/'DMRB',
     ; 'dMRB/dt, kg/s                                               '/
       DATA CODE(36),NAME(36)/'DMEX',
     ; 'dMEX/dt, kg/s                                               '/
       DATA CODE(37),NAME(37)/'DHEX',
     ; 'energy outflow rate through exhaust valve, J/s              '/
       DATA CODE(38),NAME(38)/'DMPE',
     ; 'dMPE/dt, kg/s                                               '/
       DATA CODE(39),NAME(39)/'DMRE',
     ; 'dMRE/dt, kg/s                                               '/
c
c  Algebraic variables determined in all phases for indicator output:
       DATA CODE(101),NAME(101)/'ANG ',
     ; 'crank angle, degrees from TDC compression                   '/
       DATA CODE(102),NAME(102)/'MC  ',
     ; 'total gas mass in cylinder, kg                              '/
       DATA CODE(103),NAME(103)/'PC  ',
     ; 'pressure in cylinder, Pa                                    '/
       DATA CODE(104),NAME(104)/'VC  ',
     ; 'cylinder volume, m^3                                        '/
       DATA CODE(105),NAME(105)/'TC  ',
     ; 'mass-averaged temperature of gas in cylinder, K             '/
       DATA CODE(106),NAME(106)/'VELT',
     ; 'mass-averaged turbulence velocity, m/s                      '/
       DATA CODE(107),NAME(107)/'XPC ',
     ; 'fraction of cylinder gas mass that is products              '/
c
       DATA CODE(110),NAME(110)/'FIN',
     ; 'mass inflow rate through intake valve, kg/s                 '/
       DATA CODE(111),NAME(111)/'FEX ',
     ; 'mass outflow rate through exhaust valve, kg/s               '/
c
       DATA CODE(112),NAME(112)/'VFH ',
     ; 'burbed gas volume fraction                                  '/
       DATA CODE(113),NAME(113)/'MFH ',
     ; 'burbed gas mass fraction                                    '/
       DATA CODE(114),NAME(114)/'TH  ',
     ; 'temperature of burned gas, K                                '/
c
       DATA CODE(116),NAME(116)/'VELH',
     ; 'turbulence velocity in the burned zone, m/s                 '/
       DATA CODE(117),NAME(117)/'VELU',
     ; 'turbulence velocity in the unburned zone, m/s               '/
       DATA CODE(118),NAME(118)/'VFLA',
     ; 'flame speed, m/s                                            '/
       DATA CODE(119),NAME(119)/'AFLA',
     ; 'projected flame area, m^2                                   '/
       DATA CODE(120),NAME(120)/'DMHD',
     ; 'burn rate, kg/s                                             '/
       DATA CODE(121),NAME(121)/'DVDT',
     ; 'dV/dt, m^3/s                                                '/
       DATA CODE(122),NAME(122)/'AFLE',
     ; 'exhaust valve area open area, m^2                           '/
       DATA CODE(123),NAME(123)/'AFLI',
     ; 'intake valve area open area, m^2                            '/
       DATA CODE(124),NAME(124)/'AHTC',
     ; 'heat transfer area in cylinder, m^2                         '/
       DATA CODE(125),NAME(125)/'MIB ',
     ; 'mass of inlet bakflow gas, kg                               '/
c
       DATA CODE(127),NAME(127)/'XPB ',
     ; 'fraction of backflow gas that is products                   '/
       DATA CODE(128),NAME(128)/'TB  ',
     ; 'temperature of backflow gas, K                              '/
       DATA CODE(129),NAME(129)/'TE  ',
     ; 'temperature of mixed exhaust, K                             '/
       DATA CODE(130),NAME(130)/'XPE ',
     ; 'fraction of exhaust gas that is products                    '/
       DATA CODE(131),NAME(131)/'AHTH',
     ; 'heat transfer area for burned gas during burn, m^2          '/
       DATA CODE(132),NAME(132)/'AHTU',
     ; 'heat transfer area for unburned gas, m^2                    '/
       DATA CODE(133),NAME(133)/'VH  ',
     ; 'burned gas volume, m^3                                      '/
       DATA CODE(134),NAME(134)/'VU  ',
     ; 'unburned volume, m^3                                        '/
       DATA CODE(135),NAME(135)/'MUE ',
     ; 'reactants remaining at end of burn, kg                      '/
       DATA CODE(136),NAME(136)/'MH  ',
     ; 'mass of burned gas, kg                                      '/


       DATA CODE(138),NAME(138)/'XPU ',
     ; 'product mass fraction in unburned gas                       '/
c
c  Flow velocities
       DATA CODE(143),NAME(143)/'VI10',
     ; '0.1 x inflow velocity through intake valve, m/s             '/
       DATA CODE(144),NAME(144)/'VE10',
     ; '0.1 x outflow velocity through exhaust valve, m/s           '/
       DATA CODE(145),NAME(145)/'VELI',
     ; 'velocity magnitude through intake valve, m/s                '/
       DATA CODE(146),NAME(146)/'VELE',
     ; 'velocity magnitude through exhaust valve, m/s               '/
c
c  Computed stagnation states outside valves
       DATA CODE(147),name(147)/'P0IV',
     ; 'stagnation pressure at intake valve before EGR, Pa          '/
       DATA CODE(148),name(148)/'T0IV',
     ; 'stagnation temperature at intake valve before EGR, K        '/
       DATA CODE(149),name(149)/'P0EV',
     ; 'stagnation pressure at exhaust valve after EGR, Pa          '/
       DATA CODE(150),name(150)/'T0EV',
     ; 'stagnation temperature at exhaust valve after EGR, K        '/
c
c  Other thermodynamic properties:
       DATA CODE(151),NAME(151)/'CPC ',
     ; 'specific heat of cylinder gas (unburned during burn), J/kg-K'/
       DATA CODE(152),NAME(152)/'CPM ',
     ; 'specific heat of mixed charge, J/kg-K                       '/
       DATA CODE(153),NAME(153)/'CPB ',
     ; 'specific heat of backflow gas, J/kg-K                       '/
       DATA CODE(154),NAME(154)/'CPE ',
     ; 'specific heat of mixed exhaust, J/kg-K                      '/
       DATA CODE(155),NAME(155)/'CPH ',
     ; 'specific heat Cp of burned gas in cylinder, J/kg            '/
       DATA CODE(156),NAME(156)/'CPU ',
     ; 'specific heat Cp of unburned gas in cylinder, J/kg          '/
       DATA CODE(157),NAME(157)/'GAMC',
     ; 'Cp/Cv for cylinder gas                                      '/
       DATA CODE(158),NAME(158)/'GAMM',
     ; 'Cp/Cv for mixed charge                                      '/
       DATA CODE(159),NAME(159)/'GAMB',
     ; 'Cp/Cv for backflow gas                                      '/
       DATA CODE(160),NAME(160)/'GAME',
     ; 'Cp/Cv for mixed exhausted gas                               '/
       DATA CODE(161),NAME(161)/'GAMH',
     ; 'Cp/Cv for burned gas                                        '/
       DATA CODE(162),NAME(162)/'GAMU',
     ; 'Cp/Cv for unburned gas                                      '/
       DATA CODE(163),NAME(163)/'HCS ',
     ; 'specific enthalpy of gas in cylinder, J/kg                  '/
       DATA CODE(164),NAME(164)/'HMS ',
     ; 'specific enthalpy of mixed charge, J/kg                     '/
       DATA CODE(165),NAME(165)/'HBS ',
     ; 'specific enthalpy of inlet backflow gas, J/kg               '/
       DATA CODE(166),NAME(166)/'HES ',
     ; 'specific enthalpy of mixed exhaust exhaust, J/kg            '/
       DATA CODE(167),NAME(167)/'HHS ',
     ; 'specific enthalpy of burned gas, J/kg                       '/
       DATA CODE(168),NAME(168)/'HUS ',
     ; 'specific enthalpy of unburned gas, J/kg                     '/
       DATA CODE(169),NAME(169)/'RHOC',
     ; 'density of cylinder gas (unburned during burn), kg/m^3      '/
       DATA CODE(170),NAME(170)/'RHOM',
     ; 'density of mixed charge, kg/m^3                             '/
       DATA CODE(171),NAME(171)/'RHOB',
     ; 'density of backflow gas gas, kg/m^3                         '/
       DATA CODE(172),NAME(172)/'RHOE',
     ; 'density of mixed exhaust gas, kg/m^3                        '/
       DATA CODE(173),NAME(173)/'RHOH',
     ; 'density of burned gas, kg/m^3                               '/
       DATA CODE(174),NAME(174)/'RHOU',
     ; 'density of burned gas, kg/m^3                               '/
       DATA CODE(175),NAME(175)/'UCS ',
     ; 'specific internal energy of gas in cylinder, J/kg           '/
       DATA CODE(176),NAME(176)/'PVH ',
     ; 'Pv for burned gas, J/kg                                     '/
       DATA CODE(177),NAME(177)/'PVU ',
     ; 'Pv for unburned gas, J/kg                                   '/
       DATA CODE(178),NAME(178)/'GAFH',
     ; '(gamma-1)/gamma for burned gas                              '/
       DATA CODE(179),NAME(179)/'GAFU',
     ; '(gamma-1)/gamma for unburned gas                            '/

c
       DATA CODE(180),NAME(180)/'YE  ',
     ; 'P/rho^k for exhaust gas manifold calculations               '/

c  Computed engine parameters:
       DATA CODE(181),NAME(181)/'APIS',
     ; 'piston bore area, m^2                                       '/
       DATA CODE(182),NAME(182)/'AHTE',
     ; 'heat transfer area for exhaust valve flow, m^2              '/
       DATA CODE(183),NAME(183)/'AHTI',
     ; 'heat transfer area for inlet valve flow, m^2                '/
       DATA CODE(184),NAME(184)/'AMEV',
     ; 'maximum flow area of exhaust valve, m^2                     '/
       DATA CODE(185),NAME(185)/'AMIV',
     ; 'maximum flow area of intake valve, m^2                      '/
       DATA CODE(186),NAME(186)/'AHAC',
     ; 'heat transfer area exclusive of stroked cylinder, m^2       '/
c
       DATA CODE(189),NAME(189)/'THOR',
     ; '2*clearance height/bore radius (cylindrical burn parameter) '/
       DATA CODE(190),NAME(190)/'VCLR',
     ; 'clearance volume, m^3                                       '/
       DATA CODE(191),NAME(191)/'ZCHT',
     ; 'bore*pi*stroke, m^2                                         '/
       DATA CODE(192),NAME(192)/'ZDXP',
     ; 'stroke*RPM*2*pi/60, rad-m/s                                 '/
       DATA CODE(193),NAME(193)/'DISP',
     ; '(piston area)*stroke, m^3                                   '/
       DATA CODE(194),NAME(194)/'RRTS',
     ; 'piston program parameter (rod/stroke for conventional)      '/
       DATA CODE(199),NAME(199)/'DT1 ',
     ; 'time step for one crank angle degree, sec                   '/
       DATA CODE(200),NAME(200)/'VPIS',
     ; 'piston speed, m/s                                           '/
c
c  Specified engine operating conditions:
       DATA CODE(202),NAME(202)/'PIAT',
     ; 'intake ambient pressure, atm.                               '/
       DATA CODE(203),NAME(203)/'PEAT',
     ; 'exhaust ambient presssure, atm.                             '/
c
c  Converted specified engine operating parameters
       DATA CODE(204),NAME(204)/'PE  ',
     ; 'exhaust ambient presssure, Pa                               '/
       DATA CODE(205),NAME(205)/'PI  ',
     ; 'intake ambient presssure, Pa                                '/
       DATA CODE(206),NAME(206)/'TI  ',
     ; 'inlet ambient temperature, K                                '/
       DATA CODE(207),NAME(207)/'TEGR',
     ; 'exhaust gas recirculation temperature, K                    '/
       DATA CODE(208),NAME(208)/'FEGR',
     ; 'exhaust gas recirculation fraction                          '/
       DATA CODE(209),NAME(209)/'TM  ',
     ; 'mixed charge temperature, K                                 '/
       DATA CODE(210),NAME(210)/'MDIA',
     ; 'mass displaced at ambient density, kg                       '/
c
c  Specified piston/cylinder geometrical parameters:
       DATA CODE(211),NAME(211)/'BORE',
     ; 'cylinder bore, m                                            '/
       DATA CODE(212),NAME(212)/'VCR ',
     ; 'volume compression ratio                                    '/
       DATA CODE(213),NAME(213)/'STRK',
     ; 'piston compression stroke, m                                '/
c
       DATA CODE(216),NAME(216)/'CROD',
     ; 'connecting rod length, m                                    '/
       DATA CODE(217),NAME(217)/'STRE',
     ; 'piston expansion stroke, m                                  '/
c
c  Specified valve parameters:
       DATA CODE(218),NAME(218)/'AVIR',
     ; 'reference flow area of intake valve (full open),  m^2       '/
       DATA CODE(219),NAME(219)/'AVER',
     ; 'reference flow area of exhaust valve (full open), m^2       '/
c
c  Specified parameters in the flow model:
       DATA CODE(221),NAME(221)/'CDI ',
     ; 'discharge coefficient for intake valve                      '/
       DATA CODE(222),NAME(222)/'CDIB',
     ; 'discharge coefficient for intake valve backflow             '/
       DATA CODE(223),NAME(223)/'CDE ',
     ; 'discharge coefficient for exhaust valve                     '/
       DATA CODE(224),NAME(224)/'CDEB',
     ; 'discharge coefficient for exhaust valve backflow            '/
c
c  Specified parameters in the burn model
       DATA CODE(231),NAME(231)/'FMIG',
     ; 'fraction of mass ignited at ignition                        '/
       DATA CODE(232),NAME(232)/'VLAM',
     ; 'laminar flame speed, m/s                                    '/
       DATA CODE(233),NAME(233)/'FVFL',
     ; 'ratio of turbulent flame speed to turbulence velocity       '/
       DATA CODE(234),NAME(234)/'FRAB',
     ; 'fraction of reactants burned (combustion efficiency)        '/
c
c  Computed parameters in the burn model
       DATA CODE(240),NAME(240)/'XPM ',
     ; 'product fraction in the mixed charge                        '/

c  Computed cycle quantities
       DATA CODE(241),NAME(241)/'PECS',
     ; 'sum of polytropic exponent contributions for compression    '/
       DATA CODE(242),NAME(242)/'PEES',
     ; 'sum of polytropic exponent contributions for expansion      '/
       DATA CODE(243),NAME(243)/'PMAX',
     ; 'peak cycle pressure, Pa                                     '/
c
c  Specified parameters in the heat transfer model:
       DATA CODE(251),NAME(251)/'STC ',
     ; 'Stanton number during compression                           '/
       DATA CODE(252),NAME(252)/'STBU',
     ; 'Stanton number during burn for unburned gas                 '/
       DATA CODE(253),NAME(253)/'STBH',
     ; 'Stanton number during burn for burned gas                   '/
       DATA CODE(254),NAME(254)/'STE ',
     ; 'Stanton number during expansion                             '/
       DATA CODE(255),NAME(255)/'STGE',
     ; 'Stanton number for in-cylinder heat trans. during gas exch. '/
       DATA CODE(256),NAME(256)/'STIV',
     ; 'Stanton number for heat transfer from inlet valve flow      '/
       DATA CODE(257),NAME(257)/'STEV',
     ; 'Stanton number for heat transfer from exhaust valve flow    '/
       DATA CODE(258),NAME(258)/'FAHH',
     ; '(heat transfer area above piston at TDC)/(bore area)        '/
       DATA CODE(259),NAME(259)/'FAHI',
     ; '(heat transfer area for intake jet flow)/(bore area)        '/
       DATA CODE(260),NAME(260)/'FAHE',
     ; '(heat transfer area for exhaust jet flow)/(bore area)       '/
c
       DATA CODE(268),NAME(268)/'TEV ',
     ; 'temperature of exhaust valve heat transfer area, K          '/
       DATA CODE(269),NAME(269)/'TIV ',
     ; 'temperature of inlet valve heat transfer area, K            '/
       DATA CODE(270),NAME(270)/'TW  ',
     ; 'wall temperature, K                                         '/
c
c  Specified parameters in the turbulence model:
       DATA CODE(271),NAME(271)/'FKEI',
     ; '(inlet flow turb. kinetic energy)/(mean flow kinetic energy)'/
       DATA CODE(272),NAME(272)/'FKEE',
     ; '(exhaust bkflow. turb. energy)/(mean bkflo. kinetic energy) '/
       DATA CODE(273),NAME(273)/'FTDC',
     ; 'factor in turbulence dissipation during compression         '/
       DATA CODE(274),NAME(274)/'FTDB',
     ; 'factor in turbulence dissipation during burn                '/
       DATA CODE(275),NAME(275)/'FTDE',
     ; 'factor in turbulence dissipation during expansion           '/
       DATA CODE(276),NAME(276)/'FTDG',
     ; 'factor in turbulence dissipation during gas exchange        '/
       DATA CODE(277),NAME(277)/'FTPC',
     ; 'factor in turbulence production during compression          '/
       DATA CODE(278),NAME(278)/'FTPB',
     ; 'factor in turbulence production during burn                 '/
       DATA CODE(279),NAME(279)/'FTPE',
     ; 'factor in turbulence production during expansion            '/
       DATA CODE(280),NAME(280)/'FTPG',
     ; 'factor in turbulence production during gas exchange         '/
c
c    Cycle convergence checks
       DATA CODE(286),NAME(286)/'ERRT',
     ; 'start-finish temperature error/temperature at intake close  '/
       DATA CODE(287),NAME(287)/'ERRP',
     ; 'start-finish pressure error/pressure at intake close        '/
       DATA CODE(288),NAME(288)/'ERRV',
     ; 'start-finish turbulence velocity error/turbulence velocity  '/
       DATA CODE(289),NAME(289)/'ERRM',
     ; 'cycle mass balance error/mass output                        '/
       DATA CODE(290),NAME(290)/'ERRE',
     ; 'cycle energy balance error/work output                      '/

c    Computed cycle parameters
       DATA CODE(291),NAME(291)/'IMEP',
     ; 'net indicated work output/displacement, Pa                  '/
       DATA CODE(292),NAME(292)/'ISFC',
     ; 'mass of fuel/indicated work output, kg/J                    '/
       DATA CODE(293),NAME(293)/'NVOL',
     ; 'flow mass/displacement mass at mixed charge density         '/
       DATA CODE(294),NAME(294)/'QPIW',
     ; 'heat transfer/net indicated work output                     '/
       DATA CODE(295),NAME(295)/'PEXC',
     ; 'average polytropic exponent for compression                 '/
       DATA CODE(296),NAME(296)/'PEXE',
     ; 'average polytropic exponent for expansion                   '/
       DATA CODE(297),NAME(297)/'PMAT',
     ; 'cycle peak pressure, atm.                                   '/
c
c    Specified parameters for the intake manifold model
       DATA CODE(301),NAME(301)/'BEIF',
     ; 'entrance blockage for intake feeder                         '/
       DATA CODE(302),NAME(302)/'BDIF',
     ; 'discharge blockage fraction for intake feeder               '/
       DATA CODE(303),NAME(303)/'BEIR',
     ; 'entrance blockage fraction for intake runner                '/
       DATA CODE(304),NAME(304)/'FIF ',
     ; 'friction factor for inlet feeder                            '/
       DATA CODE(305),NAME(305)/'FIR ',
     ; 'friction factor for inlet runner                            '/
       DATA CODE(306),NAME(306)/'LIF ',
     ; 'length of the intake feeder, m                              '/
       DATA CODE(307),NAME(307)/'LIR ',
     ; 'length of the intake runner, m                              '/
       DATA CODE(308),NAME(308)/'DIF ',
     ; 'diameter of the intake feeder, m                            '/
       DATA CODE(309),NAME(309)/'DIR ',
     ; 'diameter of the intake runner, m                            '/
c
c    Specified parameters for the intake manifold model
       DATA CODE(311),NAME(311)/'BDER',
     ; 'discharge blockage fraction for exhaust runner              '/
       DATA CODE(312),NAME(312)/'BEEC',
     ; 'entrance blockage fraction  for exhaust collector           '/
       DATA CODE(313),NAME(313)/'BDEC',
     ; 'discharge blockage fraction for exhaust collector           '/
       DATA CODE(314),NAME(314)/'FER ',
     ; 'friction factor for exhaust runner                          '/
       DATA CODE(315),NAME(315)/'FEC ',
     ; 'friction factor for exhaust collector                       '/
       DATA CODE(316),NAME(316)/'LER ',
     ; 'length of the exhaust runner, m                             '/
       DATA CODE(317),NAME(317)/'LIC ',
     ; 'length of the exhaust collector, m                          '/
       DATA CODE(318),NAME(318)/'DER ',
     ; 'diameter of the exhaust runner, m                           '/
       DATA CODE(319),NAME(319)/'DEC ',
     ; 'diameter of the exhaust collector, m                        '/
c
c      Computed manifold states
       DATA CODE(321),NAME(321)/'P1IM',
     ; ' Pressure at feeder inlet in intake manifold, Pa            '/
       DATA CODE(322),NAME(322)/'P2IM',
     ; 'Pressure at feeder exit in intake manifold, Pa              '/
       DATA CODE(323),NAME(323)/'P3IM',
     ; 'Pressure at runner inlet in intake manifold, Pa             '/
       DATA CODE(324),NAME(324)/'P4IM',
     ; 'Pressure at runner exit in intake manifold, Pa              '/
       DATA CODE(325),NAME(325)/'V1IM',
     ; 'Velocity at feeder inlet in intake manifold, Pa             '/
       DATA CODE(326),NAME(326)/'V2IM',
     ; 'Velocity at feeder exit in intake manifold, Pa              '/
       DATA CODE(327),NAME(327)/'V3IM',
     ; 'Velocity at runner inlet in intake manifold, Pa             '/
       DATA CODE(328),NAME(328)/'V4IM',
     ; 'Velocity at runner exit in intake manifold, Pa              '/
       DATA CODE(329),NAME(329)/'TRRI',
     ; 'T0egr/T0runner at intake valve before EGR                   '/
c
       DATA CODE(331),NAME(331)/'P1EM',
     ; 'Pressure at runner inlet in exhaust manifold, Pa            '/
       DATA CODE(332),NAME(332)/'P2EM',
     ; 'Pressure at runner exit in exhaust manifold, Pa             '/
       DATA CODE(333),NAME(333)/'P3EM',
     ; 'Pressure at collector inlet in exhaust manifold, Pa         '/
       DATA CODE(334),NAME(334)/'P4EM',
     ; 'Pressure at collector exit in exhaust manifold, Pa          '/
       DATA CODE(335),NAME(335)/'V1EM',
     ; 'Velocity at runner inlet in exhaust manifold, Pa            '/
       DATA CODE(336),NAME(336)/'V2EM',
     ; 'Velocity at runner exit in exhaust manifold, Pa             '/
       DATA CODE(337),NAME(337)/'V3EM',
     ; 'Velocity at collector inlet in exhaust manifold, Pa         '/
       DATA CODE(338),NAME(338)/'V4EM',
     ; 'Velocity at collector exit in exhaust manifold, Pa          '/
c
       DATA CODE(341),NAME(341)/'M1IM',
     ; 'Mach number in intake manfold at feeder inlet               '/
       DATA CODE(342),NAME(342)/'M2IM',
     ; 'Mach number in intake manifold at feeder exit               '/
       DATA CODE(343),NAME(343)/'M3IM',
     ; 'Mach number in intake manifold at runner inlet              '/
       DATA CODE(344),NAME(344)/'M4IM',
     ; 'Mach number in intake manifold at runner exit               '/
       DATA CODE(345),NAME(345)/'M1EM',
     ; 'Mach number in exhaust manifold at runner inlet             '/
       DATA CODE(346),NAME(346)/'M2EM',
     ; 'Mach number in exhaust manifold at runner exit              '/
       DATA CODE(347),NAME(347)/'M3EM',
     ; 'Mach number in exhaust manifold at collector inlet          '/
       DATA CODE(348),NAME(348)/'M4EM',
     ; 'Mach number in exhaust manifold at collector outlet         '/
c
c  Intake manifold constants
       DATA CODE(349),NAME(349)/'YI  ',
     ; 'entropy parameter P/rho^k at inlet, SI units                '/
       DATA CODE(350),NAME(350)/'CI  ',
     ; 'sound speed at inlet, m/s                                   '/
       DATA CODE(351),NAME(351)/'RHOI',
     ; 'density at inlet, kg/m^3                                    '/
c-----------------------------------------------------------------------------
       CODEI = CODE(I)
       NAMEI = NAME(I)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE IWIDVV(I,CODEI,NAMEI)
c
c      Returns 4-character code and 60-character name of IW(I).
c-----------------------------------------------------------------------------
       CHARACTER*4     CODEI,CODE(50)
       CHARACTER*60    NAMEI,NAME(50)
c-----------------------------------------------------------------------------

c    Initialize null   NOT ALLOWED BY SOME COMPILERS
C      DATA CODE/50*'****'/
C      DATA NAME/50*
C      ; '                                                            '/

c
c  Crank angles controlling the cycle:
       DATA CODE(1),NAME(1)/'IANG',
     ; 'crank angle, degrees                                        '/
       DATA CODE(2),NAME(2)/'IAIO',
     ; 'crank degrees after compression TDC where intake opens      '/
       DATA CODE(3),NAME(3)/'IAIC',
     ; 'crank degrees after compression TDC where intake closes     '/
       DATA CODE(4),NAME(4)/'IAEO',
     ; 'crank degrees after compression TDC where exhaust opens     '/
       DATA CODE(5),NAME(5)/'IAEC',
     ; 'crank degrees after compression TDC where exhaust closes    '/
c
c  firing option
       DATA CODE(6),NAME(6)/'KFOP',
     ; 'firing option (1 firing at IAIG, 2 motoring)                '/
c
c  control codes
       DATA CODE(7),NAME(7)/'LSTG',
     ; ' cycle stage indicator                                      '/
       DATA CODE(8),NAME(8)/'LEFL',
     ; 'exhaust flow state indicator                                '/
       DATA CODE(9),NAME(9)/'LIFL',
     ; 'intake flow state indicator                                 '/
c
c  operating parameters
       DATA CODE(10),NAME(10)/'IRPM ',
     ; 'revolutions per minute                                      '/
       DATA CODE(11),NAME(11)/'IAIG',
     ; 'crank deg. after comp. TDC at ignition                      '/
       DATA CODE(12),NAME(12)/'ITI ',
     ; 'ambient temperature, K                                      '/
       DATA CODE(13),NAME(13)/'IEGR',
     ; 'mass percent EGR                                            '/
       DATA CODE(14),NAME(14)/'ITER',
     ; 'EGR return temperature, K                                   '/
       DATA CODE(15),NAME(15)/'IIVF',
     ; 'max. intake valve flow area % of reference                  '/
       DATA CODE(16),NAME(16)/'IEVF',
     ; 'max. exhaust valve flow area % of reference                 '/
c
c    gas properties loading code
       DATA CODE(17),NAME(17)/'KGPM',
     ; 'gas loading code; 1 new file; 2 existing file               '/
c
c    valve and piston program codes
       DATA CODE(18),NAME(18)/'KIVP',
     ; 'intake valve program code: 1 cos, 2 cos-flat-cos, 4 user    '/
       DATA CODE(19),NAME(19)/'KEVP',
     ; 'exhaust valve program code: 1 cos, 2 cosflat-cos, 4 user    '/
       DATA CODE(20),NAME(20)/'KPPR',
     ; 'piston valve program code: 1 conven; 2 dual stroke          '/
c
c  heat transfer model parameters
       DATA CODE(21),NAME(21)/'ITCW',
     ; 'temperature of liner/piston/head heat transfer area, K      '/
       DATA CODE(22),NAME(22)/'ITIV',
     ; 'temperature of intake valve-flow heat transfer area, K      '/
       DATA CODE(23),NAME(23)/'ITEV',
     ; 'temperature of exhaust valve-flow heat transfer area, K     '/
c
c   rise/fall angles for flat top valve program
       DATA CODE(24),NAME(24)/'IRIO',
     ; 'degrees required for intake valve to open                   '/
       DATA CODE(25),NAME(25)/'IRIC',
     ; 'degrees required for intake valve to close                  '/
       DATA CODE(26),NAME(26)/'IREO',
     ; 'degrees required for intake valve to open                   '/
       DATA CODE(27),NAME(27)/'IREC',
     ; 'degrees required for intake valve to close                  '/
c
c    manifold parameters
       DATA CODE(28),NAME(28)/'NIR ',
     ; 'number of inlet runners from feeder                         '/
       DATA CODE(29),NAME(29)/'NER ',
     ; 'number of exhaust runners to collector                      '/
c
c  relative angles
       DATA CODE(30),NAME(30)/'IANR',
     ; 'current angle from start of compression                     '/
       DATA CODE(31),NAME(31)/'IIGR',
     ; 'angle from compression start to ignition                    '/
       DATA CODE(32),NAME(32)/'IEOR',
     ; 'angle from compression start to gas exchange start          '/
c
       DATA CODE(35),NAME(35)/'KSSU',
     ; 'setup file save; 1 save, 2 no save                          '/
       DATA CODE(36),NAME(36)/'KSFG',
     ; 'flame geometry table file save; 1 save, 2 no save           '/
c
       DATA CODE(39),NAME(39)/'KIMC',
     ; 'intake manifold; 1 run with none; 2 run included            '/
       DATA CODE(40),NAME(40)/'KEMC',
     ; 'exhaust manifold; 1 run with none; 2 run included           '/


c  computed cycle parameters being collected
       DATA CODE(41),NAME(41)/'NPEC',
     ; 'number of polytropic exponent points for compression        '/
       DATA CODE(42),NAME(42)/'NPEE',
     ; 'number of polytropic exponent points for expansion          '/
       DATA CODE(43),NAME(43)/'JAMP',
     ; 'crank angle at maximum pressure, degrees                    '/
       DATA CODE(44),NAME(44)/'JAEB',
     ; 'crank angle at end of burn, degrees                         '/
       DATA CODE(45),NAME(45)/'JTEX',
     ; 'mixed exhaust temperature at valve exit, K                  '/
c
c  computed cycle parameters for display
       DATA CODE(47),NAME(47)/'IAMP',
     ; 'crank angle at maximum pressure, degrees                    '/
       DATA CODE(48),NAME(48)/'IAEB',
     ; 'crank angle at end of burn, degrees                         '/
       DATA CODE(49),NAME(49)/'ITEX',
     ; 'mixed exhaust temperature at valve exit, K                  '/
c
c  flame geometry model code
       DATA CODE(50),NAME(50)/'KFGM',
     ; 'flame geometry model; 1 loaded; 2 new file; 3 cylindrical   '/
c-----------------------------------------------------------------------------
       CODEI = CODE(I)
       NAMEI = NAME(I)
       RETURN
       END
c******************************************************************************

