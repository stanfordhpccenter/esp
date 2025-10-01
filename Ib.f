c******************************************************************************
c
c      Explanation of the general interface concept and primary subroutines
c      called by the FORTRAN code.
c
c      Some changes between UNIX, IBM, and Mac are flagged below.
c
c******************************************************************************
c
c      Plan of the interface and application
c
c      The interface routines allow the same core FORTRAN programs to be used
c      with a variety of graphical or non-graphical interfaces.
c ____________________________________________________________________________
c |                             |               |                            |
c |         Interface           |  Application  |          Library           |
c |--------------------------------------------------------------------------|
c | Written by interface author |  Written by   |                            |
c | in any language linkable    |  application  |          Existing          |
c | with FORTRAN.               |  author       |                            |
c |--------------------------------------------------------------------------|
c |  Non-graphical interface in |  User files   |         Library in         |
c |  File I2.FOR | File I1.FOR  |               |         File IB.FOR        |
c |--------------------------------------------------------------------------|
c |  called by   | called by    |  called by    |   called by    | called by |
c |  interface   | application  |  interface    | application to | library/  |
c |              |              |               |  define setup  | interface |
c |              |              |  dummy names  |                |           |
c |  supporting  |   MESSZZ     |    SETUP      |     BIGZZ      |  SETZZ    |
c |   routines   |   PAUSZZ     |    HELP       |     BVGZZ      |  INCCZZ   |
c |    with      |   WARNZZ     |    ACTION     |     BEOGZZ     |  INCIZZ   |
c | distinctive  |   GETZZ      |               |     BIOGZZ     |  INCRZZ   |
c |    names     |   YESZZ      |  actual name  |     BTABZZ     |  SERRZZ   |
c |              |   GFNEZZ     |    STOPZZ     |     BHOGZZ     |  JCPZZ    |
c |              |   CLOWZZ     |               |     ODZZ       |  JKTCZZ   |
c |              |   DOUTZZ     |               |     HOZZ       |  JKTSZZ   |
c |              |              |               |     GDZZ       |  LPTRZZ   |
c |              |              |               |     CVZZ       |  ERRFZZ   |
c |              |              |               |     IVZZ       |  CHKCZZ   |
c |              |              |               |     RVZZ       |  SGUDZZ   |
c |              |              |               |     FNZZ       |  CHKSZZ   |
c |              |              |               |     AOZZ       |  CHKTZZ   |
c |              |              |               |                |  SOADZZ   |
c |              |              |               |                |  CVFLZZ   |
c ----------------------------------------------------------------------------
c
c******************************************************************************
c
c      Setup concept
c
c      Everything needed to construct the setup display, revise the data, and
c      return new values checked for certain limits is loaded into three work
c      arrays:
c
c          CD  character data
c          ID  integer data (includes pointers to the setup objects)
c          RD  real data
c
c      The work arrays are constructed by the application program using
c      subroutines that define the setup objects or return new values
c      from the work arrays to their proper locations for the application
c      program.
c
c      The first data in the work arrays provide data for the interface.
c
c          ID(1)   IU unset integer
c          ID(2)   NCMAX   dimension of CD
c          ID(3)   NIMAX   dimension of ID
c          ID(4)   NRMAX   dimension of RD
c          ID(5)   NC      number of characters (bytes) in CD
c          ID(6)   NI      number of integers in ID
c          ID(7)   NR      number of real values in RD
c          ID(8)   NGET    setup control
c          ID(9)   NACT    number of actions
c          ID(10)  JKTGOD  pointer to the god object following user actions
c
c          RD(1)   RU unset real
c
c          CD(1)   CU unset character
c
c      The action options start in ID(11).
c
c      The actions following changes in the setup are defined by action
c      options (mutually exclusive action options):
c
c                                  TCL/TK graphical format
c        Object type
c          action option           (labeled button in the action area)
c
c
c      The setup is organized using five types of group objects:
c
c                                  TCL/TK graphical format
c        Group type
c          items                   (a zip box group boxes)
c          values                  (a zip box of value boxes)
c          exclusive options       (a zip box of radio buttons)
c          inclusive options       (a zip box of check buttons)
c          table                   (a zip box containing a table)
c
c      Five types of editable values objects are used:
c
c          group designation       (string of specified langth)
c          character value         (string of specified length)
c          integer value           (integer of restricted range)
c          real value              (floating point of restricted range)
c          file name               (path/name with specified extent)
c
c      The first object defined in the setup must be one of these groups.
c      This is called the god object (god hath no parent). All of the
c      other objects must be decendants of god. The children of an items
c      group can be any type of groups or values.
c
c      Only one group designation object may appear in any group. The
c      designator of an exclusive option is the option choice.
c
c      A table group consists of the table and an optional attached object,
c      for example an exclusive options group containing the ptions to
c      save or not save the table in a file.
c
c      One type of option object is used in both exclusive and inclusive
c      option object groups. An option object may contain adjustable values
c      defined by assocated value objects (its children):
c
c          option                  (option string with associated values)
c
c      The children of an options group may be options or groups.
c
c      Help can be supplied with any object using two types of help objects:
c
c                                  TCL/TK graphical format
c          help options group      (a zip box of radio buttons)
c          help option             (option string and help code)
c
c      The elements (children) of a help options group must be help option
c      objects or other help option groups. Each option object must have a
c      unique integer code that is used to inform the help routine what help
c      is requested by the user.
c
c      The pointer to an object is the index in ID of the type code KT for the
c      object,  which is the first integer data for the object. The ID data for
c      each object contains pointers to its children. The pointer to the god
c      object is 10, meaning that its ID data begins at ID(10).
c
c      The data for these different object types is stored in the work
c      arrays in the order indicated below. Note that the ID data for all
c      editable groups is organized as follows:
c
c              KT        type code
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              JKTD      pointer to group designation
c              JKTH      pointer to help
c              ...       object-specific data
c
c      while for editable values the ID data are as follows:
c
c              KT        type code
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              ...       object-specific data
c
c        Action option object
c          ID data
c              IGI       graphical interface index
c              JCS       index of option string in CD
c              NCS       number of characters in the option string CS
c          CD data
c              ACTION    string (escape removed)
c
c      Items group object
c          ID data
c              KT        type code for begin items (1)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              JKTD      pointer to group designation
c              JKTH      pointer to help
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NIS       number of items (children) in the group
c              JKTI(1)   pointer to first child
c              ...
c              JKTI(NIS) pointer to last child
c          CD data
c              HEADER    string (escape removed)
c
c      Values group object
c          I data
c              KT        type code for begin values (2)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              JKTD      pointer to group designation
c              JKTH      pointer to help
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NVS       number of values(children) in the group
c              JKTD      pointer to group designation
c              JKTV(1)   pointer to first child
c              ...
c              JKTV(NVS) pointer to last child
c          C data
c              HEADER    string (escape removed)
c
c      Exclusive options group object
c          ID data
c              KT        type code for begin exclusive options (3)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              JKTD      pointer to group designation
c              JKTH      pointer to help
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NOS       number of options (children) in the group
c            * NOPC      chosen option number (if 0 none)
c              JKTO(1)   pointer to first child
c              ...
c              JKTO(NOS) pointer to last child
c          CD data
c              HEADER    string (escape removed)
c
c      Inclusive options group object
c          ID data
c              KT        type code for begin inclusive options (4)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              JKTD      pointer to group designation
c              JKTH      help pointer
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NOS       number of options (children) in the group
c              NOPMAX    max number of selectable options
c            * NOPC(1)   status of option 1 (0 if not chosen, 1 if chosen)
c            * ...
c            * NOPC(NOS) status of last option (0 if not chosen, 1 if chosen)
c              JKTO(1)   pointer to first child
c              ...
c              JKTO(NOS) pointer to last child
c          CD data
c              HEADER    header string (escape removed)
c
c      Table object
c          ID data
c              KT          type code for begin table (5)
c              JKTP        parent pointer
c              IGI         graphical interface index
c              KS          status of the table
c              JKTD        pointer to table designation
c              JKTH        pointer to help
c              JCH         index of header string in CD
c              NCH         number of characters in the header string
c              JKTA        pointer to attached object (e.g.save options)
c              NRW         number of rows
c              NFC         number of frozen columns
c              NVC         number of variable columns
c              NCW         character width of column 1
c              ...
c              NCW(NCL)  character width of last column
c              KRC(1)      KR restriction code for variable column 1
c              ...
c              KRC(NVC)    KR restriction code for variable column NVC
c              JRV         index in RD of first element of the array data
c          CD data
c              HEADER      header string (escape removed)
c              ESC         escape character
c              COLHDS      column header data string a la MESSZZ per column
c          RD data
c              AF(1,1)
c              AF(2,1)     matrix AF(I,J) of frozen columns, by columns
c              ...         I = row index, J =column index
c              AF(NRW,NFL)
c              AV(1,1)
c              AV(2,1)     matrix AV(I,J) of variable columns, by columns
c              ...         I = row index, J =column index
c              AV(NRW,NVL)
c              VMINC(1)    minimum value for variable column 1
c              ...
c              VMINC(NVC)  minimum value for variable column NVC
c              VMAXC(1)    maximum value for variable column 1
c              ...
c              VMAXC(NVC)  maximum value for variable column NVC
c
c        Group designation object
c           I data
c              KT        type code for designator (10)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              JCD       index of designation string in CD
c              NCD       number of characters in the designation string
c          C data
c            * GDES      group designation string
c
c        Character value object
c           I data
c              KT        type code for character variable (11)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              JCN       index of name string in CD
c              NCN       number of characters in the name string
c              NCS       number of characters in the character variable
c          C data
c              NAME      string
c            * CS        variable string
c
c        Integer value object
c          ID data
c              KT        type code for integer (12)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JCN       index of name in CD
c              NCN       number of characters in the name
c              KR        restriction code
c            * IV        value
c              IVMIN     minimum value
c              IVMAX     maximum value
c          CD data
c              NAME      string
c
c      Help options group object
c          ID data
c              KT        type code for begin help options (6)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NHO       number of options (children) in the group
c              JKTO(1)   pointer to first child
c              ...
c              JKTO(NHO) pointer to last child
c          CD data sequence
c              HEADER    string (escape removed)
c
c        Real value object
c          ID data
c              KT        type code for real variable (13)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              JCN       index of name string in CD
c              NCN       number of characters in the name string
c              KR        restriction code
c              JRV       index to the value in RD
c          RD data
c            * RV        value
c              RVMIN     minimum value
c              RVMAX     maximum value
c          CD data
c              NAME      string
c
c        File-name value object
c          ID data
c              KT        type code for file-name variable (14)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              KS        status code
c              JCN       index of file use string in CD
c              NCN       number of characters in the variable name string
c              NCF       number of characters in the file path/name string
c              KRW       read/write code; 1 read, 2 write
c          CD data
c              NAME      file-name variable string (escape removed)
c              FILE      file path/name string
c              EXT       specified extent
c
c              NVS       number of variables (children) in the option
c              JKTV(1)   pointer to first child
c              ...
c              JKTV(NVS) pointer to last child
c          CD data
c              OPTION    option string (escape removed)
c
c        Option object
c          ID data
c              KT        type code for option (20)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JCS       index of option string in CD
c              NCS       number of characters in the option string
c              NVS       number of # variables in the option
c              JKTV(1)   pointer to first variable object (child)
c              ...
c              JKTV(NVS) pointer to last variable object
c          CD data
c              CS        option string (escape removed)
c        Help option object
c          ID data
c              KT        type code for help option (21)
c              JKTP      pointer to parent
c              IGI       graphical interface index
c              JCA       index of ABOUT string in CD
c              NCA       number of characters in the string ABOUT
c              KHELP     code transmitted to application to request this help
c          CD data
c              ABOUT     string (escape removed)
c
c------------------------------------------------------------------------------
c
c     Integer or real variable restriction byte KR:
c
c         KR bit
c          1: 0  sign unrestricted
c             1  sign restricted
c          2: 0  positive definite
c             1  negative definite
c          4: 0  minimum unrestricted
c             1  minimum specified
c          8: 0  maximum unrestricted
c             1  maximum specified
c
c------------------------------------------------------------------------------
c
c    Value or option group status code KS:
c
c        KS = 0  value or choices undefined (and hence not displayed)
c           = 1  values as loaded from application
c           = 2  new values
c
c      The setup can not be sumbitted to the application if any values
c      or designators are undefined.
c------------------------------------------------------------------------------
c
c    Option definitions:
c
c      An option object is defined by its option string and value objects       in the option string. If the string contains a #, the # is to be
c      referenced in the option string.  If the string contains a #, the #
c      is to be replaced by a character, integer, real variable, or file name
c      when the option object is displayed. Multiple # may occur in any option.
c      Pointers to the # value objects are included in the ID data for the
c      option object in which they occur.
c
c      A help option object is defined by its ABOUT string. Only one help
c      option from all the help options in the entire setup may be selected
c      at any time. When the user requests help on an item, GETZZ calls the
c      application-specific subroutine HELP(KHELP), which must provide the
c      help by calls to MESSZZ.
c------------------------------------------------------------------------------
c
c    Group designation:
c
c      The group designation is displayed as an editable item when the
c      group is fully displayed. When only the group header is displayed,
c      the group designation is displayed with the header so that the user
c      can tell what setf values it contains.  When any member of the group
c      is changed, the group designation is set undefined, forcing the user
c      to give it a distinguishing name before the group display is closed
c      or the setup is used.
c
c   Group designations for exclusive options:
c
c      The designator of an exclusive option is the chosen option or its
c      designator. JKTD for the exclusive option group is the pointer to
c      its designation.  A designator can not be assigned directly to an
c      exclusive options group.
c------------------------------------------------------------------------------
c
C      SUBROUTINE SETZZ(NGET)
c
c      Initializes setup work arrays
c
c      NGET control
c
c          NGET = 0    construct setup and load no data
c                 1    construct setup and load current application data
c                 2    check setup seeking undefined children
c                 3    return new values to applcation
c------------------------------------------------------------------------------
c
c      Actions
c
c      Additional actions may be defined by the user.  These action options
c      must be the first objects in the SETUP program and are numbered in order
c      of appearance. GETZZ must call ACTION(K,NOPA) to do the action and
c      continue. NOPA determines what happens on return:
c
c        NOPA = 0 GETZZ continues editing values/choices
c               1 check and submit current values/choices before GETZZ return
c              >1 no values/choices submitted and GETZZ returns
c
c******************************************************************************
c
c      Subroutines provided by the interface writer
c
c------------------------------------------------------------------------------
c      The MESSZZ, DOUTZZ, and PAUSZZ routines are called by the application
c      to provide information to the user.  In graphical interfaces this is
c      display in an information area.
c
c      The WARNZZ routine is called by the interface or user to warn against
c      improper action. In graphical interfaces this is a pop-up box with
c      mandatory acknwledgement.
c
c      The interface routine GETZZ(NIV,SETUP,HELP,ACTION,NOPA) must create
c      the setup by call to SETUP, solicit changes and record them in the work
c      arrays, provide help as requested by calls to HELP, load the application
c      with the values to be used on the next run by a second call to SETUP,
c      and perform user-defined actions before returning to setup by calls
c      to ACTION.
c
c      SETUP, HELP, and ACTION are applications-specific subroutines provided
c      by the application programmer. These need not make any use of the
c      interface work arrays. These are dummy names,so multiple setups can
c      appear in the same application.
c
c      Only one setup can be active at any time, but an application may use
c      several different setups (with distinctive names) sequentially.
c------------------------------------------------------------------------------
C      SUBROUTINE MESSZZ(ESC,STRING)
c
c      Writes ESC-terminated header to the message window.
c      A single escape indicates a line end, a double escape the data end.
c
c      CHARACTER*1 ESC,HEADER(*)
c------------------------------------------------------------------------------
C      SUBROUTINE DOUTZZ(ESC,STRING)
c
c      Writes ESC-terminated STRING to the output data window.
c      A single escape indicates a line end, a double escape the data end.
c
c      CHARACTER*1 ESC,STRING(*)
c------------------------------------------------------------------------------
C      SUBROUTINE PAUSZZ(K)
c
c      Waits for user confirmation to continue after message.
c      if K=1, calls CLOWZZ befor return to clear output window
c------------------------------------------------------------------------------
C      SUBROUTINE GETZZ(NIV,SETUP,HELP,ACTION,NOPA)
c
c      Creates setup, solicits changes, provides help as requested, and
c      performs the desired actions.
c      loads the application with the values to be used on the next run.
c      Loads a new setup in a file or saves the current setup on request.
c      Executes soft application shutdown on request.
c
c      NIV entry code used at GETZZ call:
c        0 values are undefined initially
c        1 values currently in the application are used initially
c
c      NOPA code for action following GETZZ exit:
c        0 GETZZ continues editing values/choices
c        1 the current values/choices are submitted and GETZZ returns
c        2 no values/choices are submitted and GETZZ returns
c
c      application-specific setup routine
c        EXTERNAL    SETUP
c      application-specific help routine
c        EXTERNAL    HELP
c      application-specific action routine
c        EXTERNAL    ACTION
c
c******************************************************************************
c
c    Setup memory allocation by the application program
c
c      The application main program must reserve the required setup memory
c      and record the dimensions in ID by including the following:
C ---------------------------------------------------------------------
c      The work array dimensions must be in file INTWRKZZ,H,
c      which is incorporated by an $INCLUDE compiler directive
c      in subroutines. the contents of this file must be as follows
c      (remove comment-outs):
c
c    general setup work arrays
C      PARAMETER   (NCMAXZ = 6144)
C      PARAMETER   (NIMAXZ = 2048)
C      PARAMETER   (NRMAXZ = 256)
C      CHARACTER*1 CD
C      COMMON /SETCZZ/ CD(NCMAXZ)
C      COMMON /SETIZZ/ ID(NIMAXZ)
C      COMMON /SETRZZ/ RD(NRMAXZ)
C      EQUIVALENCE(ID(5),NC)
C      EQUIVALENCE(ID(6),NI)
C      EQUIVALENCE(ID(7),NR)
C  ---------------------------------------------------------------------
c    The main program must load the array dimensions by the following
c    commands at the start of the main program:
C
C      ID(2) = NCMAX
C      ID(3) = NIMAX
C      ID(4) = NRMAX
C
c      The the application writer will be told if the allocated memory is
c      insufficient when the code is tested. If additional memory is required,
c      the parameter values above can be changed in INTWRKZZ,H.
c******************************************************************************
c
c      The application FORTRAN code must provide four subroutines:
c
c      SETUP      defines the setup or copies new setup to the application data
c      HELP(K)    provides help requested by the Kth help option
c      ACTION(K)  performs user-defined action K > 2.
c      STOPZZ     provides soft shutdown following interface program error
c******************************************************************************
c
c      The general interface library (file IB.FOR)
c
c      SETUP defines the work arrays by calls to subroutines provided in this
c      general interface package. The arguments of these routines take the
c      following form:
c
c          JKTP    index in ID to th type code KT of the parent object
c          NCP     child number of the new object (0 for god)
c          .....   data for the new object
c          JKT     index in ID to the type code KT of the new object
c
c      The subroutines that initiate the definitions of a group are
c
c          BIGZZ(JKTP,NCP,...,JKT)   begin items group
c          BVGZZ(JKTP,NCP,...,JKT)   begin values group
c          BEOGZZ(JKTP,NCP,...,JKT)  begin exclusive options
c          BIOGZZ(JKTP,NCP,...,JKT)  begin inclusive options
c          BTABZZ(JKTP,NCP,...,JKT)  begin table
c          BHOGZZ(JKTP,NCP,...,JKT)  begin help options
c
c      Option objects are defined by subsequent calls to
c
c          ODZZ(JKTP,NCP,...,JKT)  exclusive or inclusive option definition
c          HOZZ(JKTP,NCP,...,JKT)  help option
c
c      The editable variables are defined by calls to
c
c          GDZZ(JKTP,NCP,...,JKT)    group designation
c          CVZZ(JKTP,NCP,...,JKT)    character string
c          IVZZ(JKTP,NCP,...,JKT)    integer
c          RVZZ(JKTP,NCP,...,JKT)    real (floating point number)
c          FNZZ(JKTP,NCP,...,JKT)    file-name
c
c      Special information for the interface designer:
c
c          CLOWZZ  clear output window
c==============================================================================
c      Subroutines in the general interface library called by the application
c------------------------------------------------------------------------------
c      SUBROUTINE  AOZZ(ESC,STRING)
c      User defined action. These must be the first objects defined in SETUP.
c------------------------------------------------------------------------------
C      SUBROUTINE BIGZZ(JKTP,NCP,ESC,HEADER,NIS,JKT)
c      Begin items group
c------------------------------------------------------------------------------
C      SUBROUTINE BVGZZ(JKTP,NCP,ESC,HEADER,NVS,JKT)
c      Begin values group
c------------------------------------------------------------------------------
C      SUBROUTINE BEOGZZ(JKTP,NCP,ESC,HEADER,NOS,NOPI,JKT)
c      Begin exclusive options group
c------------------------------------------------------------------------------
C      SUBROUTINE BIOGZZ(JKTP,NCP,ESC,HEADER,NOS,NOPMAX,NOPI,JKT)
c      Begin inclusive options group
c------------------------------------------------------------------------------
C      SUBROUTINE BTABZZ(JKTP,NCP,ESC,HEADER,NRW,NFC,NVC,NCW,COLHDS,
C    ;          KRC,VMINC,VMAXC,AF,AV,KSO,JKT)
c      Begin table
c------------------------------------------------------------------------------
C      SUBROUTINE BHOGZZ(JKTP,NCP,ESC,HEADER,NOH,JKT)
c      Begin help options group
c------------------------------------------------------------------------------
C      SUBROUTINE ODZZ(JKTP,NOP,ESC,STRING,NVS,JKT)
c      Option definition (one option for either type of option group).
c------------------------------------------------------------------------------
C      SUBROUTINE HOZZ(JKTP,NOP,ESC,STRING,KHELP,JKT)
c      Help option
c------------------------------------------------------------------------------
C      SUBROUTINE GDZZ(JKTP,NCD,GDES,JKT)
c      Group designation
c------------------------------------------------------------------------------
C      SUBROUTINE CVZZ(JKTP,NCP,NAME,NCN,NCS,CS,JKT)
c      Character variable
c-----------------------------------------------------------------------------
C      SUBROUTINE IVZZ(JKTP,NCP,NAME,NCN,KR,IVMIN,IVMAX,IV,JKT)
c      Integer variable
c-----------------------------------------------------------------------------
C      SUBROUTINE RVZZ(JKTP,NCP,NAME,NCN,KR,RVMIN,RVMAX,RV,JKT)
c      Real variable
c-----------------------------------------------------------------------------
c      SUBROUTINE FNZZ(JKTP,NCP,NAME,NCN,NCF,EXT,KRW,FILE,JKT)
c      File name with specified extent
c==============================================================================
c      Other general interface subroutines called by the subroutines above;
c      these will also be useful in the interface routines.
c-----------------------------------------------------------------------------
C      SUBROUTINE SETZZ(NGET)
c      Initializes setup work arrays
c------------------------------------------------------------------------------
C      SUBROUTINE INCCZZ
c      Increments NC; used only in setting up the work arrays.
c------------------------------------------------------------------------------
C      SUBROUTINE INCIZZ
c      Increments NI; used only in setting up the work arrays.
c------------------------------------------------------------------------------
C      SUBROUTINE INCRZZ
c      Increments NR; used only in setting up the work arrays.
c------------------------------------------------------------------------------
C      FUNCTION JCPZZ(JKTP,NCP)
c      Returns index of the pointer to the NCPth child of the parent having
c      pointer JKTP; used only in setting up the work arrays.
c------------------------------------------------------------------------------
C      FUNCTION JKTCZZ(JKTP,NCP)
c      Returns pointer to the NCPth child from the data of the parent having
c      pointer JKTP. Useful in scanning the setups. See CHKSZZ.
c------------------------------------------------------------------------------
C      FUNCTION JKTSCZZ(JKT)
c      Returns pointer to the next younger sibling of the object having pointer
c      JKT, 0 if none exists. Useful in scanning the setup. See CHKSZZ.
c------------------------------------------------------------------------------
C      SUBROUTINE LPTRZZ(JKTP,NCP,JKT)
c      Loads pointer JKT to the NCPth child of the parent having pointer JKTP.
c      Used only on the work array construction pass.
c------------------------------------------------------------------------------
C      SUBROUTINE ERRFZZ(ESC,ERROR)
c      Writes ESC-terminated fatal ERROR message using MESSZZ and STOPZZ.
c------------------------------------------------------------------------------
C      SUBROUTINE CHKCZZ(JKT)
c      Checks to see that children of object having pointer JKT are defined.
c      If not issues fatal error message and stops.
c------------------------------------------------------------------------------
C      SUBROUTINE CHKSZZ(OK)
c      Returns OK=.TRUE.  if the setup data is complete, otherwise .FALSE.
c------------------------------------------------------------------------------
C      SUBROUTINE CHKTZZ(JKT,OK)
c      Returns OK=.TRUE.  if the table having pointer JKT is ready to use.
c------------------------------------------------------------------------------
c      SUBROUTINE SOADZZ(JKT)
c      Transfers proper desinator to all antecedants of a selected option
c------------------------------------------------------------------------------
c      LOGICAL FUNCTION CVFLZZ(JKT)
c      Checks value having pointer JKT to see if it should be returned to app.
c
c******************************************************************************
c
c      The following routines are used by every interface
c
c******************************************************************************
c
       SUBROUTINE SETZZ(NGET)
c
c      If NGET=0,1  intializes setup work arrays
c                2  checks setup after construction
c                3  set to recover data
c------------------------------------------------------------------------------
c      Data loaded by this routine if NGET=0 or 1
c          ID(1)   IU unset integer
c          ID(5)   NC number of characters (bytes) in CD
c          ID(6)   NI number of integers in ID
c          ID(7)   NR number of real values in RD
c          ID(9)   reserved
c          RD(1)   RU unset real
c          CD(1)   CU unset character
c      Data loaded for all NGETs

c          ID(8)   NGET
c          ID(9)   NACT
c          ID(10)  JKTGOD
c      Data previously loaded by main proggram:
c          ID(2)   NCMAX   dimension of CD
c          ID(3)   NIMAX   dimension of ID
c          ID(4)   NRMAX   dimension of RD
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       CHARACTER*1 CU
       EQUIVALENCE(CD(1),CU)
       EQUIVALENCE(ID(1),IU)
       EQUIVALENCE(RD(1),RU)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
       EQUIVALENCE(ID(9),NACT)
       EQUIVALENCE(ID(10),JKTGOD)
c------------------------------------------------------------------------------
c    branch on NGET
       IF ((NGET.LT.0).OR.(NGET.GT.3)) CALL ERRFZZ('@','SETZZ error.@@')
c    load NGET
       ID(8) = NGET
c    initializations
       IF (NGET.LE.1)  THEN
c            load unset indicators
               CU = CHAR(239)
               IU = -22222
               RU = -1.1111111E11
c            initialize
               NC = 1
               NI = 10
               NR = 1
               NACT = 0
               JKTGOD = 11
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE INCCZZ
c
c      Increments NC if space permits; quits if overrun.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(2),NCMAX)
       EQUIVALENCE(ID(5),NC)
c------------------------------------------------------------------------------
       IF (NC.EQ.NCMAX)
     ;     CALL ERRFZZ('@','Insufficient character memory in setup.@@')
       NC = NC + 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE INCIZZ
c
c      Increments NI if space permits; quits if overrun.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(3),NIMAX)
       EQUIVALENCE(ID(6),NI)
c------------------------------------------------------------------------------
       IF (NI.EQ.NIMAX)
     ;     CALL ERRFZZ('@','Insufficient integer memory in setup.@@')
       NI = NI + 1
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE INCRZZ
c
c      Increments NR if space permits; quits if overrun.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(4),NRMAX)
       EQUIVALENCE(ID(7),NR)
c------------------------------------------------------------------------------
       IF (NR.EQ.NRMAX)
     ;     CALL ERRFZZ('@','Insufficient real memory in setup.@@')
       NR = NR + 1
       RETURN
       END
c******************************************************************************
c
       FUNCTION JCPZZ(JKTP,NCP)
c
c      Returns ID index of the pointer to the NCPth child of the parent having
c      pointer JKTP, 0 if the child does not exist.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
c    get parent type
       KTP = ID(JKTP)
c    branch by parent
       IF (KTP.EQ.1)  THEN
c            items
               NOC = ID(JKTP+8)
               JCPZZ = JKTP + 8 + NCP
           ELSEIF (KTP.EQ.2)  THEN
c            values
               NOC = ID(JKTP+8)
               JCPZZ = JKTP + 8 + NCP
           ELSEIF (KTP.EQ.3)  THEN
c            exclusive options
               NOC = ID(JKTP+8)
               JCPZZ = JKTP + 9 + NCP
           ELSEIF (KTP.EQ.4)  THEN
c            inclusive options
               NOC = ID(JKTP+8)
               JCPZZ = JKTP + 9 + NOC + NCP
           ELSEIF (KTP.EQ.5)  THEN
c            table
               JCPZZ = JKTP + 8
               RETURN
           ELSEIF (KTP.EQ.6)  THEN
c            help options
               NOC = ID(JKTP+5)
               JCPZZ = JKTP + 5 + NCP
           ELSEIF (KTP.EQ.20)  THEN
c            option
               NOC = ID(JKTP+6)
               JCPZZ = JKTP + 6 + NCP
           ELSE
               JCPZZ = 0
           ENDIF
       IF ((NCP.LT.1).OR.(NCP.GT.NOC)) JCPZZ = 0
       RETURN
       END
c******************************************************************************
c
       FUNCTION JKTSZZ(JKT)
c
c      Returns the pointer to the next younger sibling of the object having
c      JKT, 0 if the sibling does not exist.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(10),JKTGOD)
c------------------------------------------------------------------------------
c    check for god
       IF (JKT.EQ.JKTGOD) THEN
           JKTSZZ = 0
           RETURN
           ENDIF
c    get parent
       JKTP = ID(JKT+1)
c    get parent type
       KTP = ID(JKTP)
c    branch by parent
       IF (KTP.EQ.1)  THEN
c            items
               NOC = ID(JKTP+8)
               JCP0 = JKTP + 8
           ELSEIF (KTP.EQ.2)  THEN
c            values
               NOC = ID(JKTP+8)
               JCP0 = JKTP + 8
           ELSEIF (KTP.EQ.3)  THEN
c            exclusive options
               NOC = ID(JKTP+8)
               JCP0 = JKTP + 9
           ELSEIF (KTP.EQ.4)  THEN
c            inclusive options
               NOC = ID(JKTP+8)
               JCP0 = JKTP + 9 + NOC
           ELSEIF (KTP.EQ.6)  THEN
c            help options
               NOC = ID(JKTP+5)
               JCP0 = JKTP + 5
           ELSEIF (KTP.EQ.20)  THEN
c            option
               NOC = ID(JKTP+6)
               JCP0 = JKTP + 6
           ELSE
c            no sibings
               JKTSZZ = 0
               RETURN
           ENDIF
c    find the entry child JKT
       DO 9 L=1,NOC-1
           JKTX = ID(JCP0+L)
           IF (JKTX.EQ.JKT) THEN
c            the next child is the younger sibling
               JKTSZZ = ID(JCP0+L+1)
               RETURN
               ENDIF
9          CONTINUE
c    no younger sibling
       JKTSZZ = 0
       RETURN
       END
c******************************************************************************
c
       FUNCTION JKTCZZ(JKTP,NCP)
c
c      Returns pointer to the NCPth child from the data of the parent having
c      pointer JKTP, 0 if the child does not exist.  Not used on the work array
c      construction pass.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(10),JKTGOD)
c------------------------------------------------------------------------------
       IF (NCP.EQ.0)  THEN
c        Note:  this path is not encountered for help options
c        check parent
           IF (JKTP.EQ.0) THEN
c                the child is god
                   JKTCZZ = JKTGOD
                   RETURN
               ELSE
                   KTP = ID(JKTP)
                   IF (KTP.EQ.5) THEN
c                    child is a table attachment
                       JKTCZZ = ID(JKTP+8)
                       RETURN
                       ENDIF
               ENDIF
           CALL ERRFZZ('@','@JKTCZZ error; null child.@@')
           ENDIF
c    check for table
       KTP = ID(JKTP)
       IF (KTP.EQ.5)  THEN
c        check for table attachment
           IF (NCP.EQ.1)  THEN
                   JKTCZZ = ID(JKTP+8)
               ELSE
                   JKTCZZ = 0
               ENDIF
           RETURN
           ENDIF
c    get index of the child pointer
       JCP = JCPZZ(JKTP,NCP)
       IF (JCP.EQ.0)  THEN
               JKTCZZ = 0
           ELSE
               JKTCZZ = ID(JCP)
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE LPTRZZ(JKTP,NCP,JKT)
c
c     Loads pointer JKT to the NCPth child of the parent having pointer JKTP.
c     Used only on the work array construction pass.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(8),NGET)
       EQUIVALENCE (ID(10),JKTGOD)
c------------------------------------------------------------------------------
c    check call
       IF (NCP.EQ.0)  THEN
c         no children
           IF ((JKTP.EQ.JKTGOD).OR.(JKTP.EQ.0))  THEN
c                child is god
                   RETURN
               ELSEIF (ID(JKTP).EQ.5)  THEN
c                parent is table; load pointer to attached object
                   IF (ID(JKTP+8).NE.0) CALL ERRFZZ('@',
     ;               '@LPTRZZ error; duplicate table attachment.@@')
                   ID(JKTP+8) = JKT
                   RETURN
               ELSE
c                calling error
                   CALL ERRFZZ('@','LPTRZZ error; null child.@@')
               ENDIF
           ENDIF
c
c    get index of child pointer
       JCP = JCPZZ(JKTP,NCP)
       IF (JCP.EQ.0) THEN
           CALL ERRFZZ('@','LPTRZZ error; illegal JCPZZ call.@@')
           ENDIF
c    branch on pass
       IF ((NGET.EQ.0).OR.(NGET.EQ.1)) THEN
c            check child count vs. child number initially loaded at pointer
               IF (ID(JCP).NE.NCP)
     ;            CALL ERRFZZ('@','LPTRZZ error; illegal child.@@')
c            load pointer
               ID(JCP) = JKT
               RETURN
           ELSE
c            error
               CALL ERRFZZ('@','Illegal LPTRZZ call.@@')
           ENDIF
       END
c******************************************************************************
c
       SUBROUTINE ERRFZZ(ESC,ERROR)
c
c      Writes ESC-terminated fatal error message and calls STOPZZ to stop.
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,ERROR(*)
c------------------------------------------------------------------------------
       CALL WARNZZ(ESC,ERROR)
       CALL STOPZZ
       END
c******************************************************************************
c
       SUBROUTINE CHKCZZ(JKT)
c
c      Checks children for object having pointer JKT to verify their presence.
c      Writes ERRFZZ message and STOPZZ if missing.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
c------------------------------------------------------------------------------
       KT = ID(JKT)
       IF ((KT.EQ.1).OR.(KT.EQ.2))  THEN
c            items or values
               NOC = ID(JKT+8)
               JKTC1 = JKT+9
           ELSEIF (KT.EQ.3) THEN
c            exclusive options
               NOC = ID(JKT+8)
               JKTC1 = JKT+10
           ELSEIF (KT.EQ.4) THEN
c            inclusive options
               NOC = ID(JKT+8)
               JKTC1 = JKT + 10 + NOC
           ELSEIF (KT.EQ.5) THEN
c            table
               NOC = 0
           ELSEIF (KT.EQ.6) THEN
c            help options
               NOC = ID(JKT+5)
               JKTC1 = JKT + 6
           ELSEIF (KT.EQ.20) THEN
c            options
               NOC = ID(JKT+6)
               JKTC1 = JKT+7
           ELSE
               CALL ERRFZZ('@','CHKCZZ error.@@')
           ENDIF
c    check for children
       DO 9 I=1,NOC
           IF (ID(JKTC1+I-1).EQ.I) then
                CALL ERRFZZ('@','CHKCZZ error; child missing.@@')
               endif
9          CONTINUE
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE SGDUZZ(JKT)
c
c      For object having pointer JKT, sets group designation and all
c      antecedant group designations as unset.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(10),JKTGOD)
c------------------------------------------------------------------------------
c    initilize JKTX
       JKTX = JKT
c    check for group
2      KTX = ID(JKTX)
       IF ((KTX.LE.5).AND.(KTX.NE.3))  THEN
c        get pointer to designator
           JKTD = ID(JKTX+4)
           IF (JKTD.NE.0)  THEN
c            set designation unset
               ID(JKTD+3) = 0
               ENDIF
c        check for top level
           IF (JKTX.EQ.JKTGOD) RETURN
           ENDIF
c    advance to parent of current object
       JKTX = ID(JKTX+1)
c    check for god
       IF (JKTX.EQ.0)  RETURN
       GOTO 2
       END
c******************************************************************************
c
       SUBROUTINE CHKSZZ(OK)
c
c      Returns OK = .TRUE. if all data are specified, .FALSE. if not.
c      Values in unspecified options need not be set.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (ID(1),IU)
       EQUIVALENCE (ID(10),JKTGOD)
c------------------------------------------------------------------------------
       CHARACTER*128   LINE
       LOGICAL OK
c------------------------------------------------------------------------------
c    initialize going down from god
       JKT = JKTGOD
c
c    going down generations
10     KT = ID(JKT)
C      write (*,*) '10:  jkt,jktp,kt  ',jkt,id(jkt+1),kt
C      write (9,*) '10:  jkt,jktp,kt  ',jkt,id(jkt+1),kt
c    skip processing of objects without values
       IF ((KT.EQ.6).OR.(KT.EQ.21))  GOTO 20
c    skip values checks in unselected options
       IF (JKT.NE.JKTGOD) THEN
c        get parent
           JKTP = ID(JKT+1)
           KTP = ID(JKTP)
           IF (KTP.EQ.3)  THEN
c                parent is exclusive option; get selected option
                   NOPI = ID(JKTP+9)
c                if nothing is selected, go no further
                   IF (NOPI.EQ.IU)  THEN
                       JCN = ID(JKTP+6)
                       NCN = ID(JKTP+7)
                       WRITE (LINE,12)
     ;                   (CD(L),L=JCN,JCN+NCN-1),'@','@'
12                     FORMAT(' @Unset option: ',80A1)
                       GOTO 90
                       ENDIF
c                get option pointer
                   JKTI = ID(JKTP+9+NOPI)
c                if option is not selected, don't check values
                   IF (JKT.NE.JKTI) GOTO 20
               ELSEIF (KTP.EQ.4)  THEN
c                parent is inclusive option; get option count
                   NOS = ID(JKTP+8)
c                find the option and check status
                   DO 15 L=1,NOS
                       JKTL = ID(JKTP+9+NOS+L)
                       IF (JKT.EQ.JKTL)  THEN
c                        go no further if the option is selected
                           IF (ID(JKTP+9+L).NE.1)  GOTO 20
                           GOTO 16
                           ENDIF
15                     CONTINUE
               ENDIF
           ENDIF
c    check for first child
16     JKTC = JKTCZZ(JKT,1)
       IF (JKTC.LT.0)  CALL ERRFZZ('@','CHKSZZ error; '//
     ;                     'undefined child.@@')
c    go process if no children
       IF (JKTC.EQ.0)  GOTO 40
c    set for first child
       JKT = JKTC
       GOTO 10
c
c    go to younger sibling
20     JKTS = JKTSZZ(JKT)
C      write (*,*) '20:  jkt,jktp,jkts',jkt,id(jkt+1),jkts
C      write (9,*) '20:  jkt,jktp,jkts',jkt,id(jkt+1),jkts
c    if no younger sibling, go up to parent
       IF (JKTS.EQ.0) GOTO 30
c    set for younger sibling
       JKT = JKTS
       GOTO 10

c    go up to parent
30     IF (JKT.EQ.JKTGOD)  THEN
c        scan completed
           OK = .TRUE.
           RETURN
           ENDIF
c    set for parent
       JKT = ID(JKT+1)
c    go for parent's sibling
       GOTO 20

c    end of the line; check object
40     KS = ID(JKT+3)
C      write (*,*) '40:  jkt,jktp,kt  ',jkt,id(jkt+1),kt
C      write (9,*) '40:  jkt,jktp,kt  ',jkt,id(jkt+1),kt
       IF (KS.EQ.0)  THEN
           JCN = ID(JKT+4)
           NCN = ID(JKT+5)
           WRITE (LINE,42) (CD(L),L=JCN,JCN+NCN-1),'@','@'
42         FORMAT(' @Unset values INCLUDE ',80A1)
           GOTO 90
           ENDIF
c    check for table
       IF (ID(JKT).EQ.5)  THEN
           CALL CHKTZZ(JKT,OK)
           IF (.NOT.OK) THEN
               JCN = ID(JKT+6)
               NCN = ID(JKT+7)
               WRITE (LINE,52) (CD(L),L=JCN,JCN+NCN-1),'@','@'
52             FORMAT (' @Incomplete table: ',80A1)
               GOTO 90
               ENDIF
           ENDIF
c    go to younger sibling
       GOTO 20
c
c    incomplete data
90     CALL WARNZZ('@',LINE)
       OK = .FALSE.
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CHKTZZ(JKT,OK)
c
c      Checks the table having pointer JKT. Returns OK = .TRUE. if all values
c      are loaded, otherwise .FALSE.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE (RD(1),RU)
c------------------------------------------------------------------------------
       LOGICAL OK
c------------------------------------------------------------------------------
c    get parameters
       JKTA = ID(JKT+8)
       NRW = ID(JKT+9)
       NFC = ID(JKT+10)
       NVC = ID(JKT+11)
       JRV = ID(JKT+12+NFC+2*NVC)
c    check the non-frozen matrix values
       DO 9 I=1,NRW
           DO 7 J=1,NVC
               LAIJ = JRV + I - 1 + (J + NFC - 1)*NRW
c            see if value is unset
               IF (RD(LAIJ).EQ.RU)  THEN
c                value unset; set table unset
                   ID(JKT+3) = 0
                   OK =.FALSE.
                   RETURN
                   ENDIF
7              CONTINUE
9          CONTINUE
c    check attachment
       IF (JKTA.NE.0)  THEN
           IF (ID(JKTA+3).EQ.0) THEN
               OK = .FALSE.
               RETURN
               ENDIF
           ENDIF
       OK = .TRUE.
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  SOADZZ(JKT)
c
c      Sets option antecedent designators for object having pointer JKT
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(10),JKTGOD)
c------------------------------------------------------------------------------
c    initialize
       JKTX = JKT
c    check type
1      KTX = ID(JKTX)
c    get pointer to the designator
       IF (KTX.EQ.20)  THEN
c            option; it is the designator
               JKTDX = JKTX
           ELSEIF (KTX.LE.5)  THEN
c            group; has a designator
               JKTDX = ID(JKTX+4)
           ELSEIF (KTX.EQ.10) THEN
c            designator; advance to parent
               JKTX = ID(JKTX+1)
               GOTO 1
           ELSE
c            not an object with transferrable designator
               RETURN
           ENDIF
c    object has transferable designator; check parent
       IF (JKTX.EQ.JKTGOD)  RETURN
       JKTP = ID(JKTX+1)
       KTP = ID(JKTP)
c    check parent for exclusive option group
       IF (KTP.EQ.3)  THEN
c        parent is exclusive option; get selected item
           NOPC = ID (JKTP+9)
           IF (NOPC.LE.0)  RETURN
c        get selected option pointer
           JKTO = ID(JKTP+9+NOPC)
c        see if it is the object
           IF (JKTX.NE.JKTO)  RETURN
c        load object's designator pointer in parent's designator pointer
           ID(JKTP+4) = JKTDX
c        check for god
           IF (JKTP.EQ.0)  RETURN
c        go up one generation
           JKTX = JKTP
           GOTO 1
           ENDIF
c    end of the line for designator transfer
       RETURN
       END
c******************************************************************************
c
       LOGICAL FUNCTION  CVFLZZ(JKT)
c
c      Returns .TRUE. if the value having pointer JKT is to be loaded, .FALSE.
c      if the value is a parameter of an unselected option.
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(10),JKTGOD)
c------------------------------------------------------------------------------
c    initialize
       JKTX = JKT
c    check for god
10     IF (JKTX.EQ.JKTGOD)  THEN
c        value is to be loaded
           CVFLZZ = .TRUE.
           RETURN
           ENDIF
c   check parent
       JKTP = ID(JKTX+1)
       IF (ID(JKTP).EQ.3)  THEN
c            parent is exclusive option; get chosen option
               NOPI = ID(JKTP+9)
               IF (NOPI.LE.0)  THEN
c                nothing selected
                   CVFLZZ = .FALSE.
                   RETURN
                   ENDIF
c            check the selection
               JKTO = ID(JKTP+9+NOPI)
c            check vs object
               IF (JKTO.NE.JKTX) THEN
c                object not selected
                   CVFLZZ = .FALSE.
                   RETURN
                   ENDIF
           ELSEIF (JKTP.EQ.4) THEN
c            parent is inclusive option; scan all options
               NOS = ID(JKTP+8)
               DO I=1,NOS
                   NOPI = ID(JKTP+9+I)
                   IF (NOPI.EQ.0)  THEN
c                    option not selected; is this it?
                       JKTO = ID(JKTP+9+NOS+I)
                       IF (JKTO.EQ.JKTX)  THEN
c                        object not selected
                           CVFLZZ = .FALSE.
                           RETURN
                           ENDIF
                       ENDIF
                   ENDDO
           ENDIF
c    move up one generation
       JKTX= JKTP
       GOTO 10
       END
c******************************************************************************
c
       SUBROUTINE BIGZZ(JKTP,NCP,ESC,HEADER,NIS,JKT)
c
c      Begin items group
c------------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCP     child number of the new object
c          ESC     escape character
c          HEADER  ESC-terminated header string
c          NIS     the number of items (children) in the group
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code (1)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JKTD      designation pointer
c              JKTH      help pointer
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NIS       the number of item objects (children)
c              JKTI(1)   pointer to first item object
c              ...
c              JKTI(NIS) pointer to last item object
c          To CD in next available locations:
c              HEADER      string  (escape removed)
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,HEADER(*)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1) THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NCP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','BIGZZ error; JKT=0.@@')
c        check children on check pass
           IF (NGET.EQ.2)  CALL CHKCZZ(JKT)
           RETURN
           ENDIF
c
c    get pointer and load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 1
c    load pointer in parent's data
       CALL LPTRZZ(JKTP,NCP,JKT)
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status KS
       CALL INCIZZ
       ID(NI) = NGET
c    load null designation pointer
       CALL INCIZZ
       ID(NI) = 0
c    load null help pointer
       CALL INCIZZ
       ID(NI) = 0
c    load index of header
       CALL INCIZZ
       ID(NI) = NC + 1
c    load header
       NCH = 0
       DO 9 I=1,80
           IF (HEADER(I).EQ.ESC)  THEN
c            end found; load NCH
               CALL INCIZZ
               ID(NI) = NCH
c            load number of elements (children)
               CALL INCIZZ
               ID(NI) = NIS
c            reserve space for pointers to children
               DO 7 L=1,NIS
                   CALL INCIZZ
c                load child number for checking
                   ID(NI) = L
7                  CONTINUE
c            check for designator transfer
               IF (NGET.GT.0)  CALL SOADZZ(JKT)
               RETURN
               ENDIF
c        load the character
           CALL INCCZZ
           CD(NC) = HEADER(I)
           NCH = NCH + 1
9          CONTINUE
c    end of string not found
       CALL ERRFZZ('@','BIGZZ error; header too long or '//
     ;                 'not ESC-terminated.@@')
       END
c******************************************************************************
c
       SUBROUTINE BVGZZ(JKTP,NCP,ESC,HEADER,NVS,JKT)
c
c      Begin values group
c------------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCP     child number of the new object
c          ESC     escape character
c          HEADER  ESC-terminated header string
c          NVS     the number of values objects (children)
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code (2)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JKTD      designation pointer
c              JKTH      help pointer
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NVS       the number of values (children)
c              JKTV(1)   pointer to first value object
c              ...
c              JKTV(NVS) pointer to last value object
c          To CD in next available locations:
c              HEADER      string  (escape removed)
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,HEADER(*)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1)  THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NCP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','BVGZZ error; JKT=0.@@')
c        check children on check pass
           IF (NGET.EQ.2)  CALL CHKCZZ(JKT)
           RETURN
           ENDIF
c
c    get pointer and load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 2
c    load pointer in parent's data
       CALL LPTRZZ(JKTP,NCP,JKT)
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status KS
       CALL INCIZZ
       ID(NI) = NGET
c    load null designation pointer
       CALL INCIZZ
       ID(NI) = 0
c    load null help pointer
       CALL INCIZZ
       ID(NI) = 0
c    load JCH
       CALL INCIZZ
       ID(NI) = NC + 1
c    load header
       NCH = 0
       DO 9 I=1,80
           IF (HEADER(I).EQ.ESC)  THEN
c            end found; load NCH
               CALL INCIZZ
               ID(NI) = NCH
c            load number of elements (children)
               CALL INCIZZ
               ID(NI) = NVS
c            reserve space for pointers to children
               DO 7 L=1,NVS
                   CALL INCIZZ
c                load child number for checking
                   ID(NI) = L
7                  CONTINUE
c            check for designator transfer
               IF (NGET.GT.0)  CALL SOADZZ(JKT)
               RETURN
               ENDIF
c        load the character
           CALL INCCZZ
           CD(NC) = HEADER(I)
           NCH = NCH + 1
9          CONTINUE
c    end of string not found
       CALL ERRFZZ('@','BVGZZ error; header too long or '//
     ;                 'not ESC-terminated.@@')
       END
c******************************************************************************
c
       SUBROUTINE BEOGZZ(JKTP,NCP,ESC,HEADER,NOS,NOPI,JKT)
c
c      Begin exclusive options group
c------------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCP     child number of the new object
c          ESC     escape character
c          HEADER  ESC-terminated header string
c          NOS     number of options (children)
c      Input/output of the data being revised
c          NOPI    if NGET=0, input option number at call
c                  if NGET=1, output NOPC
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code (3)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status
c              JKTD      designation pointer
c              JKTH      help pointer
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NOS       number of options (children)
c              NOPI      chosen option number (if 0 none)
c              JKTO(1)   pointer to first option object
c              ...
c              JKTO(NOS) pointer to last option object
c          To CD in next available locations:
c              HEADER    string (escape removed)
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,HEADER(*)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(1),IU)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1) THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NCP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','BEOGZZ error; JKT=0.@@')
           IF (NGET.EQ.2)  THEN
c                check children on check pass
                   CALL CHKCZZ(JKT)
               ELSEIF (NGET.EQ.3) THEN
c                load selected option number on recovery pass
                   NOPI = ID(JKT+9)
               ENDIF
           RETURN
           ENDIF
c
c    get pointer and load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 3
c    load pointer in parent's data
       CALL LPTRZZ(JKTP,NCP,JKT)
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status KS
       CALL INCIZZ
       ID(NI) = NGET
c    load null designation pointer
       CALL INCIZZ
       ID(NI) = 0
c    load null help pointer
       CALL INCIZZ
       ID(NI) = 0
c    load JCH
       CALL INCIZZ
       ID(NI) = NC + 1
c    load HEADER string
       NCH = 0
       DO 9 I=1,80
           IF (HEADER(I).EQ.ESC)  THEN
c            end found; load NCH
               CALL INCIZZ
               ID(NI) = NCH
c            load option count NOS
               CALL INCIZZ
               ID(NI) = NOS
c            load initial option NOPI
               CALL INCIZZ
               IF (NGET.GT.0)  THEN
c                    load choice
                       ID(NI) = NOPI
                   ELSE
c                    load unset
                       ID(NI) = IU
                   ENDIF
C            reserve locations for option pointers and load numbers
               DO 7 J=1,NOS
                   CALL INCIZZ
                   ID(NI) = J
7                  CONTINUE
c            check for designator transfer
               IF (NGET.GT.0)  CALL SOADZZ(JKT)
               RETURN
               ENDIF
c        load the character
           CALL INCCZZ
           CD(NC) = HEADER(I)
           NCH = NCH + 1
9          CONTINUE
c    end of string not found
       CALL ERRFZZ('@','BEOGZZ error; header string too long or '//
     ;                 'not terminated by the escape character.@@')
       END
c******************************************************************************
c
       SUBROUTINE BIOGZZ(JKTP,NCP,ESC,HEADER,NOS,NOPMAX,NOPI,JKT)
c
c      Begin inclusive options group
c------------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCP     child number of the new object
c          ESC     escape character
c          HEADER  ESC-terminated header string
c          NOS     the number of elements in the group
c          NOPMAX  maximum number of selectable items
c      Input/output of the data being revised
c          NOPI(.) if NGET=0, input status vector of the options at call
c                  if NGET=1, output status vector NOPO(.)
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code (4)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JKTD      designation pointer
c              JKTH      help pointer
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NOS       number of options (children)
c              NOPMAX    max number of selectable options
c              NOPO(1)   status of option 1 (0 if not chosen, 1 if chosen)
c              ...
c              NOPO(NOS) status of last option (0 if not chosen, 1 if chosen)
c              JKTO(1)   pointer to first option
c              ...
c              JKTO(NOS) pointer to last option
c          To CD in next available locations:
c              HEADER  string (escape removed)
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,HEADER(*)
       DIMENSION   NOPI(NOS)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(1),IU)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1)  THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NCP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','BIOGZZ error; JKT=0.@@')
           IF (NGET.EQ.2)  THEN
c                check children on check pass
                   CALL CHKCZZ(JKT)
               ELSEIF (NGET.EQ.3)  THEN
c                load option states on data recovery pass
                   DO 3 L=1,NOS
                       NOPI(L) = ID(JKT+9+L)
3                      CONTINUE
               ENDIF
           RETURN
           ENDIF
c
c    load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 4
c    load pointer in parent's data
       CALL LPTRZZ(JKTP,NCP,JKT)
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status KS
       CALL INCIZZ
       ID(NI) = NGET
c    load null designation pointer
       CALL INCIZZ
       ID(NI) = 0
c    load null help pointer
       CALL INCIZZ
       ID(NI) = 0
c    load JCH
       CALL INCIZZ
       ID(NI) = NC + 1
c    load HEADER
       NCH = 0
       DO 9 I=1,80
           IF (HEADER(I).EQ.ESC)  THEN
c            end found; load the string length
               CALL INCIZZ
               ID(NI) = NCH
c            load option count
               CALL INCIZZ
               ID(NI) = NOS
c            load maximum options selectable
               CALL INCIZZ
               ID(NI) = NOPMAX
c            load initial option states
               DO 5 J=1,NOS
                   CALL INCIZZ
                   IF (NGET.GT.0) THEN
c                        load value
                           ID(NI) = NOPI(J)
                       ELSE
c                        load unchosen
                           ID(NI) = 0
                       ENDIF
5                  CONTINUE
c            reserve locations for option pointers and load numbers
               DO 7 J=1,NOS
                   CALL INCIZZ
                   ID(NI) = J
7                  CONTINUE
c            check for designator transfer
               IF (NGET.GT.0)  CALL SOADZZ(JKT)
               RETURN
               ENDIF
c        load the character
           CALL INCCZZ
           CD(NC) = HEADER(I)
           NCH = NCH + 1
9          CONTINUE
c    end of string not found
       CALL ERRFZZ('@','BIOGZZ error; header string too long or '//
     ;                 'not terminated by the escape character.@@')
       END
c******************************************************************************
c
       SUBROUTINE BTABZZ(JKTP,NCP,ESC,HEADER,NRW,NFC,NVC,NCW,COLHDS,
     ;          KRC,VMINC,VMAXC,AF,AV,KSO,JKT)
c
c     Begin table
c------------------------------------------------------------------------------
c      Input
c          JKTP        pointer to parent
c          NCP         child number of the new object
c          ESC         escape character
c          HEADER      ESC-terminated file header
c          NRW         number of rows in the table
c          NFC         number of frozen columns in the table
c          NVC         number of variable columns in the table
c          NCW         character width vector for the columns
c          COLHDS      ESC-terminated column head blocks a la MESSZZ arguments
c          KRC         restriction code vector for the variable columns
c          VMINC       minimum value vector for the variable columns
c          VMAXC       maximum value vector for the variable columns
c          AF          value matrix for the frozen columns, ordered by columns
c      Input/outpt
c          AV          value matrix for variable columns, ordered by columns
c      Output
c          KSO         KS output
c          JKT         pointer to object
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT         pointer for the object
c          To ID in next available locations:
c              KT          type code for begin table (5)
c              JKTP        parent pointer
c              IGI         graphical interface index
c              KS          status of the table
c              JKTD        designation pointer
c              JKTH        help pointer
c              JCH         index of header string in CD
c              NCH         number of characters in the header string
c              JKTA        attached object pointer (e.g. file save options)
c              NRW         number of rows
c              NFC         number of frozen columns
c              NVC         number of variable columns
c              NCW(1)      character width of column 1
c              ...
c              NCW(NCL)  character width of last column
c              KRC(1)      KR restriction code for variable column 1
c              ...
c              KRC(NVC)    KR restriction code for variable column NVC
c              JRV         index in RD of first element of the array data
c          To CD in next available locations:
c              HEADER      header string (escape removed)
c              ESC         escape character
c              COLHDS      column header data string a la MESSZZ per column
c          To RV in next available locations:
c              AF(1,1)
c              AF(2,1)     matrix AF(I,J) of frozen columns, by columns
c              ...         I = row index, J =column index
c              AF(NRW,NFL)
c              AV(1,1)
c              AV(2,1)     matrix AV(I,J) of variable columns, by columns
c              ...         I = row index, J =column index
c              AV(NRW,NVL)
c              VMINC(1)    minimum value for variable column 1
c              ...
c              VMINC(NVC)  minimum value for variable column NVC
c              VMAXC(1)    maximum value for variable column 1
c              ...
c              VMAXC(NVC)  maximum value for variable column NVC
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,HEADER(*),COLHDS(*)
       DIMENSION NCW(NFC+NVC),KRC(NVC),VMINC(NVC),VMAXC(NVC),
     ;           AF(NRW,NFC),AV(NRW,NVC)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
       EQUIVALENCE(ID(8),NGET)
       EQUIVALENCE(RD(1),RU)
c------------------------------------------------------------------------------
       IF (NGET.GT.1)  THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NCP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','BTABZZ error; JKT=0.@@')
           IF (NGET.EQ.3)  THEN
c            get parameters
               JRV = ID(JKT+12+NFC+2*NVC)
c            recover the non-frozen matrix values
               DO 9 I=1,NRW
                   DO 7 J=1,NVC
                       LAIJ = JRV + I - 1 + (J + NFC - 1)*NRW
                       AV(I,J) = RD(LAIJ)
7                      CONTINUE
9                  CONTINUE
c            return status
               KSO = ID(JKT+3)
               ENDIF
           RETURN
           ENDIF
c
c    load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 5
c    load pointer in parent's data
       CALL LPTRZZ(JKTP,NCP,JKT)
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status KS
       CALL INCIZZ
       ID(NI) = NGET
c    load null designation pointer
       CALL INCIZZ
       ID(NI) = 0
c    load null help pointer
       CALL INCIZZ
       ID(NI) = 0
c    load JCH
       CALL INCIZZ
       ID(NI) = NC + 1
c    load HEADER
       LS = 0
       DO 39 I=1,80
           IF (HEADER(I).EQ.ESC)  THEN
c            end found; load the string length
               CALL INCIZZ
               ID(NI) = LS
c            load null attached object pointer
               CALL INCIZZ
               ID(NI) = 0
c            load number of rows
               CALL INCIZZ
               ID(NI) = NRW
c            load number of frozen columns
               CALL INCIZZ
               ID(NI) = NFC
c            load number of variable columns
               CALL INCIZZ
               ID(NI) = NVC
c            load character widths
               DO 11 J=1,NFC+NVC
                   CALL INCIZZ
                   ID(NI) = NCW(J)
11                 CONTINUE
c            load column KR restriction codes
               DO 13 J=1,NVC
                   CALL INCIZZ
                   ID(NI) = KRC(J)
13                 CONTINUE
c            load index JRV
               CALL INCIZZ
               ID(NI) = NR + 1
c            load ESC
               CALL INCCZZ
               CD(NC) = ESC
c            load column header data
               L = 0
               DO 15 J=1,NFC+NVC
c                advance to next character
14                 L = L + 1
c                load character
                   CALL INCCZZ
                   CD(NC) = COLHDS(L)
c                check for escape
                   IF (CD(NC).EQ.ESC) THEN
c                   check previous character
                       IF (CD(NC-1).EQ.ESC) THEN
c                        at end of column head
                           GOTO 15
                           ENDIF
                       ENDIF
                   GOTO 14
15                 CONTINUE
c            load frozen matrix
               DO 23 J=1,NFC
                   DO 21 IX=1,NRW
                       CALL INCRZZ
                       RD(NR) = AF(IX,J)
21                     CONTINUE
23                 CONTINUE
c            load variable matrix
               DO 27 J=1,NVC
                   DO 25 IX=1,NRW
                       CALL INCRZZ
                       IF (NGET.EQ.1)  THEN
c                            load value
                               RD(NR) = AV(IX,J)
                           ELSE
c                            load unset
                               RD(NR) = RU
                           ENDIF
25                     CONTINUE
27                 CONTINUE
c            load minimum variable column values
               DO 31 J=1,NVC
                   CALL INCRZZ
                   RD(NR) = VMINC(J)
31                 CONTINUE
c            load maximum variable column values
               DO 33 J=1,NVC
                   CALL INCRZZ
                   RD(NR) = VMAXC(J)
33                 CONTINUE
c            check for designator transfer
               IF (NGET.GT.0)  CALL SOADZZ(JKT)
               RETURN
               ENDIF
c        load the character
           CALL INCCZZ
           CD(NC) = HEADER(I)
           LS = LS + 1
39         CONTINUE
c    end of string not found
       CALL ERRFZZ('@','BTABZZ error; header string too long or '//
     ;                 'not terminated by the escape character.@@')
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE BHOGZZ(JKTP,NCP,ESC,HEADER,NOH,JKT)
c
c      Begin help options group
c------------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCP     child number in help group or 0 if parent is other
c          ESC     escape character
c          HEADER  ESC-terminated header string
c          NOH     the number of elements in the group
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code for begin table (6)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              JCH       index of header string in CD
c              NCH       number of characters in the header string
c              NOH       number of children
c              JKTC(1)   pointer to first child
c              ...
c              JKTC(NOH) pointer to last child
c          To CD in next available locations:
c              HEADER    string (escape removed)
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,HEADER(*)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1)  THEN
c        get object pointer
           KTP = ID(JKTP)
           IF (KTP.EQ.6)  THEN
c                parent is a help option group
                   JKT = JKTCZZ(JKTP,NCP)
               ELSEIF ((KTP.GE.1).AND.(KTP.LE.5))  THEN
c                parent is a group
                   JKT = ID(JKTP+5)
               ELSE
                   CALL ERRFZZ('@','BHOGZZ check error; bad parent.@@')
               ENDIF
           IF (JKT.EQ.0) CALL ERRFZZ('@','BHOGZZ error; JKT=0.@@')
c        check children on check pass
           IF (NGET.EQ.2)  CALL CHKCZZ(JKT)
           RETURN
           ENDIF
c
c    load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 6
c    get parent type
       KTP = ID(JKTP)
       IF (KTP.EQ.6)  THEN
c            parent is a help options group
               JKTL = JKTP + 5 + NCP
c            check preload
               IF (JKTL.EQ.NCP) CALL ERRFZZ('@','BHOGZZ error; '//
     ;                         'duplicate child.@@')
           ELSE
c            parent is a group
               IF (NCP.NE.0)  CALL ERRFZZ('@','BHOGZZ error; '//
     ;                         'improper child.@@')
               JKTL = JKTP + 5
c            check parent for previous help load
               IF (ID(JKTP+5).NE.0) CALL ERRFZZ('@','BHOGZZ error; '//
     ;                         'duplicate help.@@')
           ENDIF
c    load pointer in parent's data
       ID(JKTL) = JKT
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load JCH
       CALL INCIZZ
       ID(NI) = NC + 1
c    load HEADER
       LS = 0
       DO 9 I=1,80
           IF (HEADER(I).EQ.ESC)  THEN
c            end found; load the string length
               CALL INCIZZ
               ID(NI) = LS
c            load option count
               CALL INCIZZ
               ID(NI) = NOH
c            reserve locations for option pointers and load numbers
               DO 7 J=1,NOH
                   CALL INCIZZ
                   ID(NI) = J
7                  CONTINUE
c            loading complete
               RETURN
               ENDIF
c        load the character
           CALL INCCZZ
           CD(NC) = HEADER(I)
           LS = LS + 1
9          CONTINUE
c    end of string not found
       CALL ERRFZZ('@','BHOZZ error; header string too long or '//
     ;                 'not terminated by the escape character.@@')
       END
c******************************************************************************
c
       SUBROUTINE ODZZ(JKTP,NOP,ESC,STRING,NVS,JKT)
c
c      Option definition (one option for either type of option group).
c
c      The option is defined by an ESC-terminated option STRING that contains
c      NVS variables. Where the string contains a #, the # is to be replaced
c      by a character, integer, or real variable. A pointer location for each
c      # object is reserved.
c------------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NOP     option (child) number
c          ESC     escape character
c          STRING  ESC-terminated option string
c          NVS     number of # variables (children)
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code for option (20)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JCS       index of option string in CD
c              NCS       number of characters in the option string
c              NVS       number of # variables in the option
c              JKTV(1)   pointer to first variable object (child)
c              ...
c              JKTV(NVS) pointer to last variable object
c          To CD in next available locations:
c              CS        option string (escape removed)
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,STRING(*)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1) THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NOP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','ODZZ error; JKT=0.@@')
c        check children on check pass
           IF (NGET.EQ.2) CALL CHKCZZ(JKT)
           RETURN
           ENDIF
c
c    assume loading options to get begin code and NOS
       KTP = ID(JKTP)
       NOS = ID(JKTP+8)
c    get reserved pointer location
       IF (KTP.EQ.3)  THEN
c            exclusive option
               JKTO = JKTP + 9 + NOP
           ELSEIF (KTP.EQ.4)  THEN
c            inclusive option
               JKTO = JKTP + 9 + NOS + NOP
           ELSE
c            error
               CALL ERRFZZ('@','ODZZ error; not loading options.@@')
           ENDIF
c    check NOP vs. preload at reserved location
       IF (NOP.NE.ID(JKTO)) CALL ERRFZZ('@','ODZZ error; wrong NOP.@@')
c    load pointer in parent's data
       JKT = NI + 1
       ID(JKTO) = JKT
c    load type code KT
       CALL INCIZZ
       ID(NI) = 20
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status code KS
       CALL INCIZZ
       IF (NVS.EQ.0)  THEN
c            option is set if no values
               ID(NI) = 1
           ELSE
c            option required values
               ID(NI) = NGET
           ENDIF
c    load pointer JCS to STRINGS
       CALL INCIZZ
       ID(NI) = NC + 1
c    reserve spot for NCS
       CALL INCIZZ
       JNCS = NI
c    reserve spot for NVS
       CALL INCIZZ
       JNVS = NI
c    load the character string and count variables and characters
       NCS = 0
       NVSX = 0
       DO 9 I=1,80
           IF (STRING(I).EQ.ESC) THEN
c            end of string; check count
               IF (NVSX.NE.NVS)
     ;              CALL ERRFZZ('@','ODZZ error; incorrect # count.@@')
               ID(JNCS) = NCS
               ID(JNVS) = NVS
c            check for designator transfer
               IF (NGET.GT.0)  CALL SOADZZ(JKT)
               RETURN
               ENDIF
c        load character
           CALL INCCZZ
           CD(NC) = STRING(I)
           NCS = NCS + 1
c        check for variable
           IF (STRING(I).EQ.'#')  THEN
c            increase variable count, reserve pointer location, load number
               NVSX = NVSX + 1
               CALL INCIZZ
               ID(NI) = NVSX
               ENDIF
9          CONTINUE
c    error
       CALL ERRFZZ('@','ODZZ error; option string not terminated.@@')
       END
c******************************************************************************
c
       SUBROUTINE HOZZ(JKTP,NOP,ESC,STRING,KHELP,JKT)
c
c      Help option
c------------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NOP     option (child) number
c          ESC     escape character
c          STRING  ESC-terminated option string
c          KHELP   help code
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code for help option (21)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              JCS       index of option string in CD
c              NCS       number of characters in the option string
c              KHELP     help code
c          To CD in next available locations:
c              CS        option string (escape removed)
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,STRING(*)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1) THEN
c        get object pointer
           KTP = ID(JKTP)
           IF (KTP.EQ.6)  THEN
c                parent is a help option group
                   JKT = JKTCZZ(JKTP,NOP)
               ELSEIF ((KTP.GE.1).AND.(KTP.LE.5))  THEN
c                parent is a group
                   JKT = ID(JKTP+5)
               ELSE
                   CALL ERRFZZ('@','HOZZ check error; bad parent.@@')
               ENDIF
           IF (JKT.EQ.0) CALL ERRFZZ('@','HOZZ error; JKT=0.@@')
           RETURN
           ENDIF
c
c    assume loading options to get begin code and NOS
       KTP = ID(JKTP)
       NOH = ID(JKTP+5)
c    get reserved pointer location from parent's data
       IF (KTP.EQ.6)  THEN
c            parent is a help options group
               JKTO = JKTP + 5 + NOP
c            check NOP vs. preload at reserved location
               IF (NOP.NE.ID(JKTO)) CALL ERRFZZ('@','HOZZ error; '//
     ;                               'wrong NOP.@@')
           ELSEIF(KTP.LE.5)  THEN
c            parent is another group
               JKTO = JKTP + 5
c            check for help duplicate
               IF (ID(JKTO).NE.0) CALL ERRFZZ('@','HOZZ error; '//
     ;                         'duplicate help.@@')
           ELSE
c            error
               CALL ERRFZZ('@','HOZZ error; '//
     ;                  'improper parent for help option.@@')
           ENDIF
c    load pointer in parent's data
       JKT = NI + 1
       ID(JKTO) = JKT
c    load type code
       CALL INCIZZ
       ID(NI) = 21
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    set pointer JCS to STRINGS
       CALL INCIZZ
       ID(NI) = NC + 1
c    load the character string
       NCS = 0
       DO 9 I=1,80
           IF (STRING(I).EQ.ESC) THEN
c            end of string; load NCS
               CALL INCIZZ
               ID (NI) = NCS
               CALL INCIZZ
c            load KHELP
               ID(NI) = KHELP
               RETURN
               ENDIF
c        load the character
           CALL INCCZZ
           CD(NC) = STRING(I)
           NCS = NCS + 1
9          CONTINUE
c    error
       CALL ERRFZZ('@','HOZZ error; option string not terminated.@@')
       END
c******************************************************************************
c
       SUBROUTINE GDZZ(JKTP,NCD,GDES,JKT)
c
c      Group designator
c-----------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCD     number of characters in the designator
c      Input/output of the data being revised
c          GDES    if NGET=0: undefined
c                          1: input initial designator string
c                          3: output revised designator string
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by parent object for the designation
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code for group designator (10)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status
c              JCD       index of designator string in CD
c              NCD       number of characters in the designator string
c          To CD in next available locations:
c              GDES      designator string
c------------------------------------------------------------------------------
       CHARACTER*1   GDES(NCD)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       CHARACTER*1 CU
       EQUIVALENCE(CD(1),CU)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1)  THEN
c        get object pointer
           JKT = ID(JKTP+4)
           IF (NGET.EQ.3)  THEN
c            data recovery
               JCD = ID(JKT+4)
               DO 3 L=1,NCD
                   GDES(L) = CD(JCD+L-1)
3                  CONTINUE
               ENDIF
           RETURN
           ENDIF
c
c    check that parent is a designateable group
       KTP = ID(JKTP)
       IF (((KTP.LT.1).OR.(KTP.GT.5)).OR.(KTP.EQ.3))
     ;        CALL ERRFZZ('@','GDZZ error; parent not group.@@')
c    check for previous designation object
       JKTPD = ID(JKTP+4)
       IF (JKTPD.NE.0)
     ;     CALL ERRFZZ('@','GDZZ error; previous designation.@@')
c    get pointer
       CALL INCIZZ
       JKT = NI
c    load designation pointer in parent's data
       ID(JKTP+4) = JKT
c    load type KT
       ID(JKT) = 10
c    load parent pointer JKTP
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status KS
       CALL INCIZZ
       ID(NI) = NGET
c    load index JCD of designation
       CALL INCIZZ
       ID(NI) = NC + 1
c    load number of characters NCD in the designation
       CALL INCIZZ
       ID(NI) = NCD
c    load designation GDES
       DO 9 L=1,NCD
           CALL INCCZZ
           IF (NGET.GT.0) THEN
c                load data
                   CD(NC) = GDES(L)
                   IF (CD(NC).EQ.CU)  ID(JKT+3) = 0
               ELSE
c                load unset
                   CD(NC) = CU
               ENDIF
9          CONTINUE
c    check for designator transfer
       IF (NGET.GT.0)  CALL SOADZZ(JKT)
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CVZZ(JKTP,NCP,NAME,NCN,NCS,CS,JKT)
c
c      Character variable
c-----------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCP     child number
c          NAME    character string name of the variable
c          NCN     number of characters in the name
c          NCS     number of characters in the variable
c      Input/output of the data being revised
c          CS      if NGET=0: undefined
c                          1: input initial string of the character variable
c                          3: output revised string of the character variable
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code for character variable (11)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JCN       index of name string in CD
c              NCN       number of characters in the name string
c              NCS       number of characters in the character variable
c          To CD in next available locations:
c              NAME      name string
c              CS        variable string
c------------------------------------------------------------------------------
       CHARACTER*1   NAME(NCN),CS(NCS)
c------------------------------------------------------------------------------
       LOGICAL    CVFLZZ
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       CHARACTER*1 CU
       EQUIVALENCE(CD(1),CU)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1)  THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NCP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','CVZZ error; JKT=0.@@')
           IF (NGET.EQ.3)  THEN
c            check for load
               IF (CVFLZZ(JKT)) THEN
c                data recovery
                   JCN = ID(JKT+4)
                   JCS = JCN + NCN
                   DO 3 L=1,NCS
                       CS(L) = CD(JCS+L-1)
3                      CONTINUE
                   ENDIF
               ENDIF
           RETURN
           ENDIF
c
c    get pointer and load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 11
c    load pointer in parent's data
       CALL LPTRZZ(JKTP,NCP,JKT)
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status
       CALL INCIZZ
       ID(NI) = NGET
c    load index JCN of name
       CALL INCIZZ
       ID(NI) = NC + 1
c    load number of characters NCN in the name
       CALL INCIZZ
       ID(NI) = NCN
c    load number of characters NCS in the string variable
       CALL INCIZZ
       ID(NI) = NCS
c    load NAME
       DO 9 L=1,NCN
           CALL INCCZZ
           CD(NC) = NAME(L)
9          CONTINUE
c    load current character string to CS
       DO 19 L=1,NCS
           CALL INCCZZ
           IF ((NGET.GT.0).AND.(CS(L).NE.CU)) THEN
c                load character
                   CD(NC) = CS(L)
               ELSE
c                load unset
c                  CD(NC) = CU
                   ID(JKT+3) = 0
c                set parent unset
                   ID(JKTP+3) = 0
               ENDIF
19         CONTINUE
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE IVZZ(JKTP,NCP,NAME,NCN,KR,IVMIN,IVMAX,IV,JKT)
c
c      Integer variable
c-----------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCP     child number
c          NAME    character string name of the variable
c          NCN     number of characters in the name
c          KR      resriction code for the variable
c          IVMIN   minimum value (or undefinend)
c          IVMAX   maximum value (or undefined)
c      Input/output of the data being revised
c          IV      if NGET=0: undefined
c                          1: input of the current value
c                          3: output of the revised value
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code for integer variable (12)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JCN       index of name in CD
c              NCN       number of characters in the name
c              KR        restriction code
c              IV        value
c              IVMIN     minimum value
c              IVMAX     maximum value
c          To CD in next available locations:
c              NAME      name string
c------------------------------------------------------------------------------
c         KR bit
c          1: 0  sign unrestricted
c             1  sign restricted
c          2: 0  positive definite
c             1  negative definite
c          4: 0  minimum unrestricted
c             1  minimum specified
c          8: 0  maximum unrestricted
c             1  maximum specified
c------------------------------------------------------------------------------
       CHARACTER*1   NAME(NCN)
c------------------------------------------------------------------------------
       LOGICAL    CVFLZZ
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(1),IU)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1)  THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NCP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','CIZZ error; JKT=0.@@')
c        check for data recovery
           IF (NGET.EQ.3) THEN
c            check for load
               IF (CVFLZZ(JKT))  IV = ID(JKT+7)
               ENDIF
           RETURN
           ENDIF
c
c    get pointer and load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 12
c    load pointer in parent's data
       CALL LPTRZZ(JKTP,NCP,JKT)
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status
       CALL INCIZZ
       ID(NI) = NGET
c    load index JCN to name
       CALL INCIZZ
       ID(NI) = NC + 1
c    load number of characters NCN in the name
       CALL INCIZZ
       ID(NI) = NCN
c    load restriction code KR
       CALL INCIZZ
       ID(NI) = KR
c    load IV
       CALL INCIZZ
       IF ((NGET.GT.0).AND.(IV.NE.IU))  THEN
c            load value
               ID(NI) = IV
           ELSE
c            load unset
               ID(NI) = IU
               ID(JKT+3) = 0
c            set parent unset
               ID(JKTP+3) = 0
           ENDIF
c    load minimum IVMIN
       CALL INCIZZ
       ID(NI) = IVMIN
c    load maximum IVMAX
       CALL INCIZZ
       ID(NI) = IVMAX
c    load NAME
       DO 9 I=1,NCN
           CALL INCCZZ
           CD(NC) = NAME(I)
9          CONTINUE
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE RVZZ(JKTP,NCP,NAME,NCN,KR,RVMIN,RVMAX,RV,JKT)
c
c      Real variable
c-----------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCP     child number
c          NAME    character string name of the variable
c          NCN     number of characters in the name
c          KR      resriction code for the variable
c          RVMIN   minimum value (or undefined)
c          RVMAX   maximum value (or undefined)
c      Input/output of the data being revised
c          RV      if NGET=0: undefined

c                          1: input of the current value
c                          3: output of the revised value
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code for real variable (13)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JCN       index of NAME in CD
c              NCN       number of characters in NAME
c              KR        restriction code
c              JRV       index of RV in RD
c          To RD in next available locations:
c              RV        value
c              RVMIN     minimum value
c              RVMAX     maximum value
c          To CD in next available locations:
c              NAME      name string
c------------------------------------------------------------------------------
c         KR bit
c          1: 0  sign unrestricted
c             1  sign restricted
c          2: 0  positive definite
c             1  negative definite
c          4: 0  minimum unrestricted
c             1  minimum specified
c          8: 0  maximum unrestricted
c             1  maximum specified
c------------------------------------------------------------------------------
       CHARACTER*1   NAME(NCN)
c------------------------------------------------------------------------------
       LOGICAL    CVFLZZ
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
       EQUIVALENCE(ID(8),NGET)
       EQUIVALENCE(RD(1),RU)
c------------------------------------------------------------------------------
       IF (NGET.GT.1)  THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NCP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','CRZZ error; JKT=0.@@')
c        check for data recovery
           IF (NGET.EQ.3)  THEN
c            check for load
               IF (CVFLZZ(JKT))  RV = RD(ID(JKT+7))
               ENDIF
           RETURN
           ENDIF
c
c    get pointer and load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 13
c    load pointer in parent's data
       CALL LPTRZZ(JKTP,NCP,JKT)
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status
       CALL INCIZZ
       ID(NI) = NGET
c    load index JCN to name
       CALL INCIZZ
       ID(NI) = NC + 1
c    load number of characters NCN in the name
       CALL INCIZZ
       ID(NI) = NCN
c    load restriction code KR
       CALL INCIZZ
       ID(NI) = KR
c    load index JRV
       CALL INCIZZ
       ID(NI) = NR + 1
c    load RV
       CALL INCRZZ
       IF ((NGET.GT.0).AND.(RV.NE.RU)) THEN
c            load value
               RD(NR) = RV
           ELSE
c            load unset
               RD(NR) = RU
               ID(JKT+3) = 0
c            set parent unset
               ID(JKTP+3) = 0
           ENDIF
c    load minimum RVMIN
       CALL INCRZZ
       RD(NR) = RVMIN
c    load maximum RVMAX
       CALL INCRZZ
       RD(NR) = RVMAX
c    load NAME
       DO 9 I=1,NCN
           CALL INCCZZ
           CD(NC) = NAME(I)
9          CONTINUE
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE FNZZ(JKTP,NCP,NAME,NCN,NCF,EXT,KRW,FILE,JKT)
c
c      File-name variable with specified extent
c------------------------------------------------------------------------------
c      Input
c          JKTP    parent pointer
c          NCP     child number
c          NAME    file-variable name
c          NCN     number of characters in the file-variable name
c          NCF     number of characters allowed for the file path/name
c          EXT     3-character file specified file extent
c          KRW     read/write code; 1 read, 2 write
c      Input/output of the data being revised
c          FILE    if NGET=0: undefined
c                          1: input initial file path/name string
c                          3: output revised file path/name string
c      Output
c          JKT     object pointer
c
c      Data added to the work arrays:
c          To ID in space reserved by the parent object for this child:
c              JKT       pointer for the object
c          To ID in next available locations:
c              KT        type code for file-name variable (14)
c              JKTP      parent pointer
c              IGI       graphical interface index
c              KS        status code
c              JCN       index of file use string in CD
c              NCN       number of characters in the variable name string
c              NCF       number of characters in the file path/name string
c              KRW       read/write code; 1 read, 2 write
c          To CD in next available locations:
c              NAME      file-variable name string (escape removed)
c              FILE      file path/name string
c              EXT       specified extent
c------------------------------------------------------------------------------
       CHARACTER*1   NAME(NCN),FILE(NCF),EXT(3)
c------------------------------------------------------------------------------
       LOGICAL    CVFLZZ
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       CHARACTER*1 CU
       EQUIVALENCE(CD(1),CU)
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(7),NR)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
       IF (NGET.GT.1)  THEN
c        get object pointer
           JKT = JKTCZZ(JKTP,NCP)
           IF (JKT.EQ.0) CALL ERRFZZ('@','FNZZ error; JKT=0.@@')
           IF (NGET.EQ.3)  THEN
c            check for load
               IF (CVFLZZ(JKT))  THEN
c                data recovery
                   JCF = ID(JKT+4) + NCN
                   DO 3 L=1,NCF
                       FILE(L) = CD(JCF+L-1)
3                      CONTINUE
                   ENDIF
               ENDIF
           RETURN
           ENDIF
c
c    get pointer and load type
       CALL INCIZZ
       JKT = NI
       ID(JKT) = 14
c    load pointer in parent's data
       IF (ID(JKTP).EQ.5)  THEN
c            parent is table
               ID(JKTP+8) = JKT
           ELSE
               CALL LPTRZZ(JKTP,NCP,JKT)
           ENDIF
c    load parent pointer
       CALL INCIZZ
       ID(NI) = JKTP
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load status
       CALL INCIZZ
       ID(NI) = NGET
c    load index JCN of variable name
       CALL INCIZZ
       ID(NI) = NC + 1
c    load number of characters NCN in the variable name
       CALL INCIZZ
       ID(NI) = NCN
c    load number of characters NCF in the file path/name
       CALL INCIZZ
       ID(NI) = NCF
c    load read/write control KRW
       CALL INCIZZ
       ID(NI) = KRW
c    load variable NAME
       DO 9 L=1,NCN
           CALL INCCZZ
           CD(NC) = NAME(L)
9          CONTINUE
c    load file path/name FILE
       DO 19 L=1,NCF
           CALL INCCZZ
           IF (NGET.GT.0) THEN
                   CD(NC) = FILE(L)
               ELSE
                   CD(NC) = CU
               ENDIF
19         CONTINUE
c    load extent
       DO 29 L=1,3
           CALL INCCZZ
           CD(NC) = EXT(L)
29         CONTINUE
c    check for reset of parent status
       IF (NGET.EQ.0)  THEN
c        set parent unset
           ID(JKTP+3) = 0
           ENDIF
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE  AOZZ(ESC,ACTION)
c
c      User-defined action
c------------------------------------------------------------------------------
c      Input
c          ESC     escape character
c          ACTION  ESC-terminated action string
c
c      Data added to the work arrays:
c          To ID in next available locations:
c              IGI       graphical interface index
c              JCA       index of action string in CD
c              NCA       number of characters in the action string
c          To CD in next available locations:
c              ACTION      string  (escape removed)
c------------------------------------------------------------------------------
       CHARACTER*1 ESC,ACTION(*)
c------------------------------------------------------------------------------
c    setup data storage
       INCLUDE 'INTWRKZZ.H'
       EQUIVALENCE(ID(5),NC)
       EQUIVALENCE(ID(6),NI)
       EQUIVALENCE(ID(9),NACT)
       EQUIVALENCE(ID(10),JKTGOD)
       EQUIVALENCE(ID(8),NGET)
c------------------------------------------------------------------------------
c    nothing to do after firtst load
       IF (NGET.GT.1)  RETURN
c    check for non-action objects
       IF (NI.GT.JKTGOD)  THEN
           CALL ERRFZZ('@','AOZZ error; misplaced object.@@')
           ENDIF
c    increment counts and the god pointer
       ID(9) = NACT + 1
       ID(10) = JKTGOD + 3
c    load null IGI
       CALL INCIZZ
       ID(NI) = 0
c    load string pointer
       CALL INCIZZ
       ID(NI) =  NC + 1
c    load string
       NCA = 0
       DO 9 I=1,80
           IF (ACTION(I).EQ.ESC)  THEN
c            end found; load NCA
               CALL INCIZZ
               ID(NI) = NCA
               RETURN
               ENDIF
c        load the character
           CALL INCCZZ
           CD(NC) = ACTION(I)
           NCA = NCA + 1
9          CONTINUE
c    end of string not found
       CALL ERRFZZ('@','AOZZ error; string too long or '//
     ;                 'not ESC-terminated.@@')
       END
c******************************************************************************

