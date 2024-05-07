*&---------------------------------------------------------------------*
*& Report  ZSHOW_USORG_ERP
*& Show Organizational Authorization Fields
*&---------------------------------------------------------------------*
*&
*& Use the hotspot on column 'Field' to navigate to transaction SU20 for that field
*& Use the hotspot on column 'Reference table' to navigate to transaction SE16 for that table
*& Use the hotspot on column '# roles' to show assigned roles having authorizations for the specific organizational field
*&
*&---------------------------------------------------------------------*
report ZSHOW_USORG_ERP
  no standard page heading.

constants: C_PROGRAM_VERSION(10) type C value '15.05.2018'.

* Global field to allow interactive reporting
data: LS_USORG      type USORG,
      LS_USVART     type USVART,
      LS_AUTHX      type AUTHX,
      LS_AGR_DEFINE type AGR_DEFINE,
      LS_USR02      type USR02.
data: L_ROLE_CNT    type I.
data: L_USER_CNT    type I.

types:
  begin of TS_PA,
    PERNR type PERSNO,    " PA0105 Personnel Number
    USRID type SYSID,     " PA0105 User id
    VORNA type PAD_VORNA, " PA0002 First name
    NACHN type PAD_NACHN, " PA0002 Last name
    BUKRS type BUKRS,     " PA0001 Company Code
    PERSA type PERSA,     " PA0001 Personnel Area
    BTRTL type BTRTL,     " PA0001 Personnel Subarea
    GSBER type GSBER,     " PA0001 Business Area
  end of TS_PA.
data: LS_PA type TS_PA.

*-----------------------------------------------------------------------


selection-screen begin of line.
selection-screen comment (30) T_SFIELD.
select-options SFIELD for LS_USORG-FIELD.
selection-screen end of line.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

*-----------------------------------------------------------------------

at selection-screen on value-request for SFIELD-LOW.
  perform F4_SFIELD using 'SFIELD-LOW'.

at selection-screen on value-request for SFIELD-HIGH.
  perform F4_SFIELD using 'SFIELD-HIGH'.
*
form F4_SFIELD using L_DYNPROFIELD  type HELP_INFO-DYNPROFLD.

  data: PROGNAME      type SY-REPID,
        DYNNUM        type SY-DYNNR,
        DYNPRO_VALUES type table of DYNPREAD,
        FIELD_VALUE   like line of DYNPRO_VALUES,
        FIELD_TAB     type table of DFIES  with header line,
        begin of VALUE_TAB occurs 0,
          FIELD type USORG-FIELD,
          VTEXT type USVART-VTEXT,
        end of VALUE_TAB.

  PROGNAME = SY-REPID.
  DYNNUM   = SY-DYNNR.

  select F~FIELD, T~VTEXT
    from USORG as F
    left outer join USVART as T
      on T~VARBL = F~VARBL
    into table @VALUE_TAB
    where T~LANGU = @SY-LANGU
    order by F~FIELD.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      RETFIELD        = 'FIELD'
      DYNPPROG        = PROGNAME
      DYNPNR          = DYNNUM
      DYNPROFIELD     = L_DYNPROFIELD
      VALUE_ORG       = 'S'
    tables
*     field_tab       = field_tab
      VALUE_TAB       = VALUE_TAB
    exceptions
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2.
  if SY-SUBRC <> 0.
* Implement suitable error handling here
  endif.
endform.  "F4_SFIELD

*-----------------------------------------------------------------------

initialization.
  concatenate 'Program version'(t00) c_program_version into SS_VERS
    SEPARATED BY SPACE.

  T_SFIELD = 'Org. Authorization Field'(017).

  if SFIELD[] is initial.
    select single * from USORG where FIELD = 'BUKRS' into @LS_USORG.
    if SY-SUBRC = 0.
      SFIELD-OPTION = 'EQ'.
      SFIELD-SIGN   = 'I'.
      SFIELD-LOW    = 'BUKRS'.  append SFIELD.
      SFIELD-LOW    = 'WERKS'.  append SFIELD.
      SFIELD-LOW    = 'VKORG'.  append SFIELD.
      SFIELD-LOW    = 'EKORG'.  append SFIELD.
      SFIELD-LOW    = 'PERSA'.  append SFIELD.
    endif.
  endif.

*-----------------------------------------------------------------------

start-of-selection.

* Check same authorizations as in transaction SU20
  authority-check object 'S_DEVELOP'
           id 'DEVCLASS' dummy
           id 'OBJTYPE'  field 'SUSO'
           id 'OBJNAME'  dummy
           id 'P_GROUP'  dummy
           id 'ACTVT'    field '03'.
  if SY-SUBRC <> 0.
    message E172(00) with 'SU20'.
  endif.

* List heading for main list
  new-page line-size 170.
  format reset.
  format color col_heading.
  write:
    /(10) 'Field'(001),            "usorg-field
     (14) 'Variable name'(002),    "(40) usorg-varbl
     (30) 'Data element'(003),     "authx-rollname
     (30) 'Reference table'(004),  "authx-checktable / dd04v_wa-entitytab
     (10) '# entries'(005),
     (60) 'Description'(006).      "usvart-vtext
  write:
     (10) '# roles'(007).
  format reset.

* Get organizational authorization fields
  select * from USORG into LS_USORG
    where FIELD in SFIELD.

*   Get description of variable
    clear LS_USVART.
    select single * from USVART into LS_USVART
      where VARBL = LS_USORG-VARBL
        and LANGU = SY-LANGU.

*   Get definition of authorization field
    clear LS_AUTHX.
    select single * from AUTHX into LS_AUTHX
      where FIELDNAME = LS_USORG-FIELD.

*   Get domain value table (see include LSAUT_FIELDF01 form GET_AUTH_FIELD)
    if LS_AUTHX-CHECKTABLE is initial.
      data: LS_DD04V type DD04V.
      clear LS_DD04V.
      call function 'DDIF_DTEL_GET'
        exporting
          NAME          = LS_AUTHX-ROLLNAME
*         STATE         = 'A'
*         LANGU         = ' '
        importing
*         GOTSTATE      =
          DD04V_WA      = LS_DD04V
*         TPARA_WA      =
        exceptions
          ILLEGAL_INPUT = 1
          others        = 2.
      if SY-SUBRC <> 0.
*       Implement suitable error handling here
      endif.
      LS_AUTHX-CHECKTABLE = LS_DD04V-ENTITYTAB.
    endif.

*   Count entries in reference table
    if LS_AUTHX-CHECKTABLE is not initial.
      data: L_DB_CNT type I.
      clear L_DB_CNT.
      select count( * ) from (LS_AUTHX-CHECKTABLE) into L_DB_CNT.
    endif.

*   Count assigned roles
    clear L_ROLE_CNT.
    select single count( distinct A~AGR_NAME )
      from AGR_1252 as A
      inner join AGR_USERS as B
        on B~AGR_NAME = A~AGR_NAME
      into L_ROLE_CNT
      where A~VARBL = LS_USORG-VARBL.

*   Show authorization field
    write:
      /     LS_USORG-FIELD      color col_key    hotspot,
       (14) LS_USORG-VARBL,
            LS_AUTHX-ROLLNAME,
            LS_AUTHX-CHECKTABLE color col_total  hotspot,
       (10) L_DB_CNT,
            LS_USVART-VTEXT.
    write:
       (10) L_ROLE_CNT          color col_normal hotspot.

  endselect.

*-----------------------------------------------------------------------

at line-selection.

  data: L_CURSOR(30),
        L_VALUE(30),
        L_TABLE(30),
        LS_SELTAB    type RSPARAMS,
        LT_SELTAB    type table of RSPARAMS.

  if SY-LISTI = 0. " Main list

    clear: LS_USORG.
    read current line
      field value LS_USORG-FIELD
                  LS_USORG-VARBL.
    get cursor field L_CURSOR value L_VALUE.

*   Use the hotspot on column 'Field' to navigate to transaction SU20 for that authorization field
    if L_CURSOR = 'LS_USORG-FIELD' and L_VALUE is not initial.
      data: L_FIELDNAME like  AUTHX-FIELDNAME.
      L_FIELDNAME = L_VALUE.
      call function 'SUSR_AUTF_DISPLAY_FIELD'
        exporting
          FIELDNAME    = L_FIELDNAME
*         SCROLL_STATUS       =
          SINGLE_MODE  = 'X'
*       TABLES
*         I_AUTHX      =
        exceptions
          NO_AUTHORITY = 1
          others       = 2.
      if SY-SUBRC <> 0.
*       Implement suitable error handling here
      endif.

*   Use the hotspot on column 'Reference table' to navigate to transaction SE16 for that table
    elseif L_CURSOR = 'LS_AUTHX-CHECKTABLE' and L_VALUE is not initial.
      data: L_TABLE_NAME like  AUTHX-CHECKTABLE.
      L_TABLE_NAME = L_VALUE.
      call function 'RS_TABLE_LIST_CREATE'
        exporting
          TABLE_NAME         = L_TABLE_NAME
*         ACTION             = 'ANZE'
*         WITHOUT_SUBMIT     = ' '
*         GENERATION_FORCED  =
*         NEW_SEL            =
*         NO_STRUCTURE_CHECK = ' '
*         DATA_EXIT          = ' '
*       IMPORTING
*         PROGNAME           =
*       TABLES
*         SELTAB             =
        exceptions
          TABLE_IS_STRUCTURE = 1
          TABLE_NOT_EXISTS   = 2
          DB_NOT_EXISTS      = 3
          NO_PERMISSION      = 4
          NO_CHANGE_ALLOWED  = 5
          others             = 6.
      if SY-SUBRC <> 0.
*       Implement suitable error handling here
      endif.

*   Use the hotspot on column '# roles' to show assigned roles having authorizations for the specific organizational field
    elseif L_CURSOR = 'L_ROLE_CNT'.
      perform WRITE_ROLES using LS_USORG-VARBL.

    endif.

  elseif SY-LISTI = 1. " Detail list showing role list

    clear: LS_AGR_DEFINE.
    read current line
      field value LS_AGR_DEFINE-AGR_NAME
                  LS_AGR_DEFINE-PARENT_AGR.
    get cursor field L_CURSOR value L_VALUE.

*   Use the hotspot on column 'Role name' to show the role
    if L_CURSOR = 'LS_AGR_DEFINE-AGR_NAME' and LS_AGR_DEFINE-AGR_NAME is not initial.
      call function 'PRGN_SHOW_EDIT_AGR'
        exporting
          AGR_NAME = LS_AGR_DEFINE-AGR_NAME
*         MODE     = 'A'
*         SCREEN   = '1'
*         SICHT    = ' '
*       EXCEPTIONS
*         AGR_NOT_FOUND       = 1
*         OTHERS   = 2
        .
      if SY-SUBRC <> 0.
* Implement suitable error handling here
      endif.

*   Use the hotspot on column 'Parent role' to show the role
    elseif L_CURSOR = 'LS_AGR_DEFINE-PARENT_AGR' and LS_AGR_DEFINE-PARENT_AGR is not initial.
      call function 'PRGN_SHOW_EDIT_AGR'
        exporting
          AGR_NAME = LS_AGR_DEFINE-PARENT_AGR
*         MODE     = 'A'
*         SCREEN   = '1'
*         SICHT    = ' '
*       EXCEPTIONS
*         AGR_NOT_FOUND       = 1
*         OTHERS   = 2
        .
      if SY-SUBRC <> 0.
* Implement suitable error handling here
      endif.

*   Use the hotspot on column '# users' to show assigned users for the specific role
    elseif L_CURSOR = 'L_USER_CNT'.
      perform WRITE_ROLE using LS_AGR_DEFINE-AGR_NAME.

    endif.


  elseif SY-LISTI = 2. " Detail list showing role with user list

    clear: L_TABLE.
    get cursor field L_CURSOR value L_VALUE.

*   Use the hotspot on column 'User' to show the user
    if     ( L_CURSOR = 'LS_USR02-BNAME' or L_CURSOR = 'LS_PA-USRID' ) and L_VALUE is not initial.

      data: l_bname type XUBNAME.
      l_bname = l_value.
      call function 'SUID_IDENTITY_MAINT'
        EXPORTING
          I_USERNAME         = l_bname
          I_TCODE_MODE       = 6   " 1=change, 6=display
          .

*   Use the hotspot on column 'PERNR' to show the employee
    elseif L_CURSOR = 'LS_PA-PERNR' and L_VALUE is not initial.

        SET PARAMETER ID 'PER' FIELD L_VALUE.  " Personnel number
        SET PARAMETER ID 'ITP' FIELD '0001'.   " Infotype
        SET PARAMETER ID 'SUB' FIELD ''.       " Subtype
        CALL TRANSACTION 'PA20'
          WITH AUTHORITY-CHECK
*          AND SKIP FIRST SCREEN
          .


*   Use the hotspot on other columns to Call SE16 for a specific entry
    else.

      clear: LS_SELTAB, LT_SELTAB.

      if     L_CURSOR cs 'BUKRS'.  " Company Code
        L_TABLE  =  'T001'.
      elseif L_CURSOR cs 'BWKEY'.
        L_TABLE  =  'T001K'.
      elseif L_CURSOR cs 'WERKS'.
        L_TABLE  =  'T001W'.
      elseif L_CURSOR cs 'VKORG'.
        L_TABLE  =  'TVKO'.
      elseif L_CURSOR cs 'EKORG'.
        L_TABLE  =  'T024E'.
      elseif L_CURSOR cs 'PERSA'.  " Personnel Area
        L_TABLE  =  'T500P'.
      elseif L_CURSOR cs 'LAND1'.
        L_TABLE  =  'T005'.
      elseif L_CURSOR cs 'MOLGA'.
        L_TABLE  =  'T500L'.
      elseif L_CURSOR cs 'GSBER'.  " PA0001 Business Area
        L_TABLE  =  'TGSB'.
      elseif L_CURSOR cs 'BTRTL'.  " PA0001 Personnel Subarea
        L_TABLE  =  'T001P'. " Use two selection fields: PERSA and BTRTL
        LS_SELTAB-SELNAME = 'I2'.
        LS_SELTAB-KIND    = 'S'.
        LS_SELTAB-SIGN    = 'I'.
        LS_SELTAB-OPTION  = 'EQ'.
        LS_SELTAB-LOW     = L_VALUE.
        append LS_SELTAB to LT_SELTAB.
        read current line field value LS_PA-PERSA.
        L_VALUE =  LS_PA-PERSA.
      endif.

      if L_TABLE is not initial.
        LS_SELTAB-SELNAME = 'I1'.
        LS_SELTAB-KIND    = 'S'.
        LS_SELTAB-SIGN    = 'I'.
        LS_SELTAB-OPTION  = 'EQ'.
        LS_SELTAB-LOW     = L_VALUE.
        append LS_SELTAB to LT_SELTAB.

        call function 'RS_TABLE_LIST_CREATE'
          exporting
            TABLE_NAME         = L_TABLE
*           ACTION             = 'ANZE'
*           WITHOUT_SUBMIT     = ' '
*           GENERATION_FORCED  =
*           NEW_SEL            =
*           NO_STRUCTURE_CHECK = ' '
*           DATA_EXIT          = ' '
*       IMPORTING
*           PROGNAME           =
          tables
            SELTAB             = LT_SELTAB
          exceptions
            TABLE_IS_STRUCTURE = 1
            TABLE_NOT_EXISTS   = 2
            DB_NOT_EXISTS      = 3
            NO_PERMISSION      = 4
            NO_CHANGE_ALLOWED  = 5
            others             = 6.
        if SY-SUBRC <> 0.
*       Implement suitable error handling here
        endif.
      endif.

    endif.

  endif. " sy-listi

*-----------------------------------------------------------------------

form WRITE_ROLES
  using L_VARBL like USORG-VARBL.

  new-page line-size 153.
  format reset.
  format color col_heading.
  write: / 'Assigned roles having authorization data for organizational field'(008), L_VARBL,
           at SY-LINSZ SPACE.
  write: /(30) 'Role'(009),
          (30) 'Parent role'(010),
          (10) '# users'(011),
          (80) 'Description'(006).
  format reset.

* Show assigned roles having authorization data for a specific organizational field
  data: L_AGR_TEXT   type AGR_TEXTS-TEXT.

  clear:LS_AGR_DEFINE, L_AGR_TEXT.

  select distinct A~AGR_NAME D~PARENT_AGR T~TEXT
    from AGR_1252 as A
    inner join AGR_USERS as B
      on     B~AGR_NAME = A~AGR_NAME
    left outer join AGR_DEFINE as D
      on     D~AGR_NAME = A~AGR_NAME
    left outer join AGR_TEXTS as T
      on     T~AGR_NAME = A~AGR_NAME
         and T~SPRAS    = SY-LANGU
         and T~LINE     = 0
    into (LS_AGR_DEFINE-AGR_NAME, LS_AGR_DEFINE-PARENT_AGR, L_AGR_TEXT)
    where    A~VARBL    = L_VARBL.

*   Count assigned users
    clear L_USER_CNT.
    select single count( distinct UNAME )
      from AGR_USERS
      into L_USER_CNT
      where AGR_NAME = LS_AGR_DEFINE-AGR_NAME.

*   Show roles
    write: /     LS_AGR_DEFINE-AGR_NAME      color col_key    hotspot,
                 LS_AGR_DEFINE-PARENT_AGR    color col_key    hotspot,
            (10) L_USER_CNT                  color col_normal hotspot,
                 L_AGR_TEXT.

  endselect.

endform.

*-----------------------------------------------------------------------

form WRITE_ROLE
  using L_AGR_NAME type AGR_DEFINE-AGR_NAME.

*  new-page line-size 122.
  new-page line-size 1023.
  format reset.
  format color col_heading.
  write: / 'Role'(009), L_AGR_NAME,
           at SY-LINSZ SPACE.
  format reset.

* Show organizational values of role
  format color col_heading.
  write: /(40) 'Variable name'(002),
          (40) 'Low'(012),
          (40) 'High'(013),
           at SY-LINSZ SPACE.
  format reset.
  data: LS_AGR_1252 type AGR_1252.
  select * from AGR_1252
    into LS_AGR_1252
    where AGR_NAME = L_AGR_NAME
    order by VARBL LOW HIGH.

    write: / LS_AGR_1252-VARBL,
             LS_AGR_1252-LOW,
             LS_AGR_1252-HIGH.
  endselect.

* Show assigned users
  format reset.
  format color col_heading.
  write: /(40) 'User'(014),
          (40) 'User type'(015),
          (40) 'User group'(016),
          (8)  'PERNR',     " PA0105 Personnel Number
*          (12)  'USRID',   " PA0105 User id
*          (40)  'VORNA',   " PA0002 First name
*          (40)  'NACHN',   " PA0002 Last name
          (5)  'BUKRS',     " PA0001 Company Code
          (5)  'PERSA',     " PA0001 Personnel Area
          (5)  'BTRTL',     " PA0001 Personnel Subarea
          (5)  'GSBER',     " PA0001 Business Area
          at SY-LINSZ SPACE.
  format reset.

  select distinct A~UNAME U~USTYP U~CLASS
    from AGR_USERS as A
    left outer join USR02 as U
      on U~BNAME = A~UNAME
    into (LS_USR02-BNAME, LS_USR02-USTYP, LS_USR02-CLASS)
    where A~AGR_NAME = L_AGR_NAME
    order by A~UNAME.

*   Show users
    write: /(40) LS_USR02-BNAME hotspot,
            (40) LS_USR02-USTYP,
            (40) LS_USR02-CLASS.

*   Show organizational assignment of user
    clear: LS_PA.
    select
        U~PERNR
        U~USRID
      from PA0105 as U
      into (LS_PA-PERNR, LS_PA-USRID)
      up to 1 rows
      where U~SUBTY =  '0001'    " Sub type for user name
        and U~BEGDA <= SY-DATUM  " valid today
        and U~ENDDA >= SY-DATUM  " valid today
        and U~USRTY =  '0001'    " Sub type for user name
        and U~USRID = LS_USR02-BNAME
        .

      select
          O~BUKRS    " Company Code
          O~WERKS    " Personnel Area
          O~BTRTL    " Personnel Sub Area
          O~GSBER    " Business Area
        from PA0001 as O
        into (LS_PA-BUKRS, LS_PA-PERSA, LS_PA-BTRTL, LS_PA-GSBER)
        up to 1 rows
        where O~PERNR =  LS_PA-PERNR
          and O~BEGDA <= SY-DATUM  " valid today
          and O~ENDDA >= SY-DATUM  " valid today
        .
      endselect.                                            " pa0001

      select
          P~VORNA
          P~NACHN
        from PA0002 as P
        into (LS_PA-VORNA, LS_PA-NACHN)
        up to 1 rows
        where P~PERNR =  LS_PA-PERNR
          and P~BEGDA <= SY-DATUM  " valid today
          and P~ENDDA >= SY-DATUM  " valid today
        .

      endselect.                                            " pa0002
    endselect.                                              " pa0105

    if LS_PA-PERNR is not initial.
      write:      LS_PA-PERNR  hotspot,
*            (12)  ls_PA-usrid  hotspot,
*                  LS_PA-VORNA,
*                  LS_PA-NACHN,
             (5)  LS_PA-BUKRS  hotspot,
             (5)  LS_PA-PERSA  hotspot,
             (5)  LS_PA-BTRTL  hotspot,
             (5)  LS_PA-GSBER  hotspot.
    endif.
  endselect.

endform.