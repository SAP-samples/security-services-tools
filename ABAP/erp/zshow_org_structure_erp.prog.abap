*&---------------------------------------------------------------------*
*& Report  ZSHOW_ORG_STRUCTURE_ERP
*& Show Organizational Structure
*&---------------------------------------------------------------------*
*& see note 2649596 - GDPR Technical Basic Check
*&
*& 29.05.2018 Initial version for ERP systems with SAP_BASIS 7.40 or higher
*& 18.07.2018 Downport for for ERP systems with SAP_BASIS 7.00 or higher
*&---------------------------------------------------------------------*

report ZSHOW_ORG_STRUCTURE
  line-size 255.

constants: C_PROGRAM_VERSION(10) type C value '18.07.2018'.

types:

  begin of TS_T001,                       " Company Code
    BUKRS   type T001-BUKRS,
    B_BUTXT type T001-BUTXT,
    B_LAND1 type T001-LAND1,
    B_LANDX type T005T-LANDX,
  end of TS_T001,

  begin of TS_T001W,                      " Plant
    WERKS   type T001W-WERKS,
    W_NAME1 type T001W-NAME1,
    W_LAND1 type T001W-LAND1,
    W_LANDX type T005T-LANDX,
    BWKEY   type T001K-BWKEY,

    BUKRS   type T001-BUKRS,
    B_BUTXT type T001-BUTXT,
    B_LAND1 type T001-LAND1,
    B_LANDX type T005T-LANDX,

    VKORG   type TVKO-VKORG,
    VTEXT   type TVKOT-VTEXT,

    EKORG   type T024E-EKORG,
    EKOTX   type T024E-EKOTX,
  end of TS_T001W,

  begin of TS_TVKO,                       " Sales organization
    VKORG   type TVKO-VKORG,
    VTEXT   type TVKOT-VTEXT,

    BUKRS   type TVKO-BUKRS,
    B_BUTXT type T001-BUTXT,
    B_LAND1 type T001-LAND1,
    B_LANDX type T005T-LANDX,

    WERKS   type TVKO-WERKS,
    W_NAME1 type T001W-NAME1,
    W_LAND1 type T001W-LAND1,
    W_LANDX type T005T-LANDX,
    W_BWKEY type T001K-BWKEY,
    K_BUKRS type T001-BUKRS,

    EKORG   type T024E-EKORG,
    EKOTX   type T024E-EKOTX,
  end of TS_TVKO,

  begin of TS_T024E,                      " Purchase organization
    EKORG   type T024E-EKORG,
    EKOTX   type T024E-EKOTX,

    BUKRS   type T024E-BUKRS,
    B_BUTXT type T001-BUTXT,
    B_LAND1 type T001-LAND1,
    B_LANDX type T005T-LANDX,

    WERKS   type TVKO-WERKS,
    W_NAME1 type T001W-NAME1,
    W_LAND1 type T001W-LAND1,
    W_LANDX type T005T-LANDX,
    W_BWKEY type T001K-BWKEY,
    K_BUKRS type T001-BUKRS,
  end of TS_T024E,

  begin of TS_T500P,                      " Personal area
    PERSA   type T500P-PERSA,
    P_NAME1 type T500P-NAME1,
    P_LAND1 type T001W-LAND1,
    P_LANDX type T005T-LANDX,
    MOLGA   type T500P-MOLGA,
    M_LTEXT type T500T-LTEXT,
    BUKRS   type T001-BUKRS,
    B_BUTXT type T001-BUTXT,
    B_LAND1 type T001-LAND1,
    B_LANDX type T005T-LANDX,
  end of TS_T500P.

data:
  LS_T001  type TS_T001,                  " Company Code
  LT_T001  type table of TS_T001,
  LS_T001W type TS_T001W,                 " Plant
  LT_T001W type table of TS_T001W,
  LS_TVKO  type TS_TVKO,                  " Sales organization
  LT_TVKO  type table of TS_TVKO,
  LS_T024E type TS_T024E,                 " Purchase organization
  LT_T024E type table of TS_T024E,
  LS_T500P type TS_T500P,                 " Personal area
  LT_T500P type table of TS_T500P.

data: G_AGR_NAME   type AGR_NAME,
      G_PARENT_AGR type AGR_NAME.

data: L_PART(5),
      L_ERROR(30).

*-----------------------------------------------------------------------

selection-screen begin of line.
parameters P_BUKRS as checkbox default 'X'.
selection-screen comment (30) T_BUKRS.
selection-screen end of line.

selection-screen begin of line.
parameters P_WERKS as checkbox default 'X'.
selection-screen comment (30) T_WERKS.
selection-screen end of line.

selection-screen begin of line.
parameters P_VKORG as checkbox default 'X'.
selection-screen comment (30) T_VKORG.
selection-screen end of line.

selection-screen begin of line.
parameters P_EKORG as checkbox default 'X'.
selection-screen comment (30) T_EKORG.
selection-screen end of line.

selection-screen begin of line.
parameters P_PERSA as checkbox default 'X'.
selection-screen comment (30) T_PERSA.
selection-screen end of line.

selection-screen skip.

selection-screen begin of block ROLE with frame title F_ROLE.

selection-screen begin of line.
parameters P_ROLE as checkbox.
selection-screen comment (30) F_ROLE1.
selection-screen end of line.

selection-screen begin of line.
parameters P_USED as checkbox default 'X'.
selection-screen comment (30) T_P_USED.
selection-screen end of line.

selection-screen begin of line.
parameters P_STAR as checkbox.
selection-screen comment (30) T_P_STAR.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (28) T_ROLES.
select-options ROLES for G_AGR_NAME.
selection-screen end of line.

selection-screen end of block ROLE.

selection-screen skip.

selection-screen begin of line.
selection-screen comment (30) T_EULAND.
select-options EULAND for LS_T001-B_LAND1.
selection-screen end of line.

selection-screen comment /1(60) SS_VERS.

*-----------------------------------------------------------------------

initialization.
  concatenate 'Program version'(T00) C_PROGRAM_VERSION into SS_VERS
    separated by SPACE.

  T_BUKRS    = 'Company Code'(001).
  T_WERKS    = 'Plant'(002).
  T_VKORG    = 'Sales Organization'(003).
  T_EKORG    = 'Purchase Organization'(004).
  T_PERSA    = 'Personal Area'(005).

  T_EULAND   = 'EU Countries'(006).

  F_ROLE     = 'Show roles'(014).
  F_ROLE1    = 'Show roles'(014).
  T_ROLES    = 'Role'(011).
  T_P_STAR   = 'Including roles with * values'(013).
  T_P_USED   = 'Assigned roles only'(012).

  if EULAND[] is initial.
    EULAND-OPTION = 'EQ'.
    EULAND-SIGN   = 'I'.
    EULAND-LOW    = 'AT'. append EULAND.  "Austria
    EULAND-LOW    = 'BE'. append EULAND.  "Belgium
    EULAND-LOW    = 'BG'. append EULAND.  "Bulgaria
    EULAND-LOW    = 'CY'. append EULAND.  "Cyprus
    EULAND-LOW    = 'CZ'. append EULAND.  "Czech Republic
    EULAND-LOW    = 'DE'. append EULAND.  "Germany
    EULAND-LOW    = 'DK'. append EULAND.  "Denmark
    EULAND-LOW    = 'EE'. append EULAND.  "Estonia
    EULAND-LOW    = 'ES'. append EULAND.  "Spain
    EULAND-LOW    = 'FI'. append EULAND.  "Finland
    EULAND-LOW    = 'FR'. append EULAND.  "France
    EULAND-LOW    = 'GB'. append EULAND.  "United Kingdom
    EULAND-LOW    = 'GR'. append EULAND.  "Greece
    EULAND-LOW    = 'HR'. append EULAND.  "Croatia
    EULAND-LOW    = 'HU'. append EULAND.  "Hungary
    EULAND-LOW    = 'IE'. append EULAND.  "Ireland
    EULAND-LOW    = 'IT'. append EULAND.  "Italy
    EULAND-LOW    = 'LT'. append EULAND.  "Lithuania
    EULAND-LOW    = 'LU'. append EULAND.  "Luxembourg
    EULAND-LOW    = 'LV'. append EULAND.  "Latvia
    EULAND-LOW    = 'MT'. append EULAND.  "Malta
    EULAND-LOW    = 'NL'. append EULAND.  "Netherlands
    EULAND-LOW    = 'PL'. append EULAND.  "Poland
    EULAND-LOW    = 'PT'. append EULAND.  "Portugal
    EULAND-LOW    = 'RO'. append EULAND.  "Romania
    EULAND-LOW    = 'SE'. append EULAND.  "Sweden
    EULAND-LOW    = 'SI'. append EULAND.  "Slovenia
    EULAND-LOW    = 'SK'. append EULAND.  "Slovakia
  endif.

*-----------------------------------------------------------------------

top-of-page.

  case L_PART.
    when 'BUKRS'.
      format reset.
      format color col_heading.
      write: /(5) 'T001', 'Company Code'(001),
             at SY-LINSZ SPACE.
      write: /(5) 'BUKRS',
             (30) 'BUKRS~BUTXT',
             (19) 'BUKRS~LAND1',
             at SY-LINSZ SPACE.

    when 'WERKS'.
      format reset.
      format color col_heading.
      write: /(5) 'T001W', 'Plant'(002),
             at SY-LINSZ SPACE.
      write: /(5) 'WERKS',
             (30) 'WERKS~NAME1',
             (19) 'WERKS~LAND1',
             (5)  'BWKEY',
             (5)  'BUKRS',
             (30) 'BUKRS~BUTXT',
             (19) 'BUKRS~LAND1',
             (5)  'VKORG',
             (30) 'VKORG~VTEXT',
             (5)  'EKORG',
             (30) 'EKORG~EKOTX',
             at SY-LINSZ SPACE.

    when 'VKORG'.
      new-page line-size 273.
      format reset.
      format color col_heading.
      write: /(5) 'TVKO', 'Sales Organization'(003),
             at SY-LINSZ SPACE.
      write: /(5) 'VKORG',     " TVKO
             (30) 'VKORG~VTEXT',
             (19) SPACE,
             (5)  SPACE,
             (5)  'BUKRS',                                    " direct in TVKO
             (30) 'BUKRS~BUTXT',
             (19) 'BUKRS~LAND1',

             (5)  'WERKS',     " T001W
             (30) 'WERKS~NAME1',
             (19) 'WERKS~LAND1',
             (5)  'BWKEY',     " T001W
             (5)  'BUKRS',     " T001K                       " indirect via WERKS

             (5)  'EKORG',     " T024E
             (30) 'EKORG~EKOTX',
             at SY-LINSZ SPACE.

    when 'EKORG'.
      new-page line-size 255.
      format reset.
      format color col_heading.
      write: /(5) 'T024E', 'Purchase Organization'(004),
             at SY-LINSZ SPACE.
      write: /(5) 'EKORG',
             (30) 'EKORG~EKOTX',
             (19) SPACE,
             (5)  SPACE,

             (5)  'BUKRS',                                    " direkt in T024E
             (30) 'BUKRS~BUTXT',
             (19) 'BUKRS_LAND1',

             (5)  'WERKS',
             (30) 'WERKS~NAME1',
             (19) 'WERKS~LAND1',
             (5)  'BWKEY',
             (5)  'BUKRS',                                    " indirect via WERKS

             at SY-LINSZ SPACE.

    when 'PERSA'.
      format reset.
      format color col_heading.
      write: /(5) 'T500P', 'Personal Area'(005),
             at SY-LINSZ SPACE.
      write: /(5) 'PERSA',
             (30) 'PERSA~NAME1',
             (19) 'PERSA~LAND1',
             (5)  'MOLGA',
             (19) 'MOLGA~LTEXT',
             (5)  'BUKRS',
             (30) 'BUKRS~BUTXT',
             (19) 'BUKRS~LAND1',
             at SY-LINSZ SPACE.

    when others.
      write / 'error'.

  endcase.

  if P_ROLE is not initial.
    format intensified.
    write: /7(30) 'Role'(016),
             (30) 'Parent role'(017),
             (80) 'Role name'(018).
    if P_USED is not initial.
      write: '#', 'Users'(019).
    endif.
    write at SY-LINSZ SPACE.
  endif.

*-----------------------------------------------------------------------

start-of-selection.

  if P_BUKRS = 'X'.
    call function 'VIEW_AUTHORITY_CHECK'
      exporting
        VIEW_ACTION                    = 'S'
        VIEW_NAME                      = 'T001'
      exceptions
        INVALID_ACTION                 = 1
        NO_AUTHORITY                   = 2
        NO_CLIENTINDEPENDENT_AUTHORITY = 3
        TABLE_NOT_FOUND                = 4
        NO_LINEDEPENDENT_AUTHORITY     = 5
        others                         = 6.
    if SY-SUBRC <> 0.
      clear P_BUKRS.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO
        with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
  endif.

  if P_WERKS = 'X'.
    call function 'VIEW_AUTHORITY_CHECK'
      exporting
        VIEW_ACTION                    = 'S'
        VIEW_NAME                      = 'T001W'
      exceptions
        INVALID_ACTION                 = 1
        NO_AUTHORITY                   = 2
        NO_CLIENTINDEPENDENT_AUTHORITY = 3
        TABLE_NOT_FOUND                = 4
        NO_LINEDEPENDENT_AUTHORITY     = 5
        others                         = 6.
    if SY-SUBRC <> 0.
      clear P_WERKS.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO
        with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
  endif.

  if P_VKORG = 'X'.
    call function 'VIEW_AUTHORITY_CHECK'
      exporting
        VIEW_ACTION                    = 'S'
        VIEW_NAME                      = 'TVKO'
      exceptions
        INVALID_ACTION                 = 1
        NO_AUTHORITY                   = 2
        NO_CLIENTINDEPENDENT_AUTHORITY = 3
        TABLE_NOT_FOUND                = 4
        NO_LINEDEPENDENT_AUTHORITY     = 5
        others                         = 6.
    if SY-SUBRC <> 0.
      clear P_VKORG.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO
        with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
  endif.

  if P_EKORG = 'X'.
    call function 'VIEW_AUTHORITY_CHECK'
      exporting
        VIEW_ACTION                    = 'S'
        VIEW_NAME                      = 'T024E'
      exceptions
        INVALID_ACTION                 = 1
        NO_AUTHORITY                   = 2
        NO_CLIENTINDEPENDENT_AUTHORITY = 3
        TABLE_NOT_FOUND                = 4
        NO_LINEDEPENDENT_AUTHORITY     = 5
        others                         = 6.
    if SY-SUBRC <> 0.
      clear P_EKORG.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO
        with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
  endif.

  if P_PERSA = 'X'.
    call function 'VIEW_AUTHORITY_CHECK'
      exporting
        VIEW_ACTION                    = 'S'
        VIEW_NAME                      = 'T500P'
      exceptions
        INVALID_ACTION                 = 1
        NO_AUTHORITY                   = 2
        NO_CLIENTINDEPENDENT_AUTHORITY = 3
        TABLE_NOT_FOUND                = 4
        NO_LINEDEPENDENT_AUTHORITY     = 5
        others                         = 6.
    if SY-SUBRC <> 0.
      clear P_PERSA.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO
        with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
  endif.

*-----------------------------------------------------------------------

end-of-selection.

  data:
    S_T001  type T001,      T_T001  type table of T001,                 " Company Code
    S_T001K type T001K,     T_T001K type table of T001K,                " BW Key
    S_T001W type T001W,     T_T001W type table of T001W,                " Plant
    S_TVKO  type TVKO,      T_TVKO  type table of TVKO,                 " Sales organization
    S_TVKOT type TVKOT,     T_TVKOT type table of TVKOT,                " Sales organization text
    S_T024E type T024E,     T_T024E type table of T024E,                " Purchase organization
    S_T005T type T005T,     T_T005T type table of T005T,                " Country Code
    S_T500P type T500P,     T_T500P type table of T500P,                " Personal area
    S_T500T type T500T,     T_T500T type table of T500T.                " Country Code HR

  select * from T001       into table T_T001.                           " Company Code
  select * from T001K      into table T_T001K.                          " BW Key
  select * from T001W      into table T_T001W.                          " Plant
  select * from TVKO       into table T_TVKO.                           " Sales organization
  select * from TVKOT      into table T_TVKOT  where SPRAS = SY-LANGU.  " Sales organization text
  select * from T024E      into table T_T024E.                          " Purchase organization
  select * from T005T      into table T_T005T  where SPRAS = SY-LANGU.  " Country Codes
  select * from T500P      into table T_T500P.                          " Personal area
  select * from T500T      into table T_T500T  where SPRAS = SY-LANGU.  " Country Code HR

  define MOVE_LAND1. " in: LAND1 out: LAND1 LANDX
    &2 = &1.
    read table T_T005T into S_T005T
      with key LAND1 = &1.
    if SY-SUBRC ne 0.
      clear &3.
    else.
      &3 = S_T005T-LANDX.
    endif.
  end-of-definition.

  define MOVE_BWKEY. " in: BWKEY out: BWKEY BUKRS BUTXT LAND1 LANDX
    &2 = &1.
    read table T_T001K into S_T001K
      with key BWKEY = &1.
    if SY-SUBRC ne 0.
      clear: &3, &4, &5, &6.
    else.
      MOVE_BUKRS S_T001K-BUKRS &3 &4 &5 &6.
    endif.
  end-of-definition.

  define MOVE_BUKRS. " in: BUKRS out: BUKRS BUTXT LAND1 LANDX
    &2 = &1.
    read table T_T001 into S_T001
      with key BUKRS = &1.
    if SY-SUBRC ne 0.
      clear: &3, &4, &5.
    else.
      &3 = S_T001-BUTXT.
      MOVE_LAND1 S_T001-LAND1 &4 &5.
    endif.
  end-of-definition.

  define MOVE_WERKS. " in: WERKS out: WERKS NAME1 LAND1 LANDX BWKEY BUKRS
    &2 = &1.
    read table T_T001W into S_T001W
      with key WERKS = &1.
    if SY-SUBRC ne 0.
      clear: &3, &4, &5, &6, &7.
    else.
      &3 = S_T001W-NAME1.
      MOVE_LAND1 S_T001W-LAND1 &4 &5.
      &6 = S_T001W-BWKEY.
      read table T_T001K into S_T001K
        with key BWKEY = S_T001W-BWKEY.
      if SY-SUBRC ne 0.
        clear: &7.
      else.
        &7 = S_T001K-BUKRS.
      endif.
    endif.
  end-of-definition.

  define GET_WERKS. " in: KEY WERKS/EKORG out: WERKS NAME1 LAND1 LANDX BWKEY BUKRS
    read table T_T001W into S_T001W
      with key &1 = &2.
    if SY-SUBRC ne 0.
      clear: &3, &4, &5, &6, &7, &8.
    else.
      &3 = S_T001W-WERKS.
      &4 = S_T001W-NAME1.
      MOVE_LAND1 S_T001W-LAND1 &5 &6.
      &7 = S_T001W-BWKEY.
      read table T_T001K into S_T001K
        with key BWKEY = S_T001W-BWKEY.
      if SY-SUBRC ne 0.
        clear: &8.
      else.
        &8 = S_T001K-BUKRS.
      endif.
    endif.
  end-of-definition.

  define MOVE_VKORG. " in: VKORG out: VKORG VTEXT
    &2 = &1.
    read table T_TVKOT into S_TVKOT
      with key VKORG = &1.
    if SY-SUBRC ne 0.
      clear &3.
    else.
      &3 = S_TVKOT-VTEXT.
    endif.
  end-of-definition.

  define MOVE_EKORG. " in: EKORG out: EKORG EKOTX
    &2 = &1.
    read table T_T024E into S_T024E
      with key EKORG = &1.
    if SY-SUBRC ne 0.
      clear &3.
    else.
      &3 = S_T024E-EKOTX.
    endif.
  end-of-definition.

  define MOVE_MOLGA. " in: MOLGA out: MOLGA LTEXT
    &2 = &1.
    read table T_T500T into S_T500T
      with key MOLGA = &1.
    if SY-SUBRC ne 0.
      clear &3.
    else.
      &3 = S_T500T-LTEXT.
    endif.
  end-of-definition.

*-----------------------------------------------------------------------
* Company Code T001
*-----------------------------------------------------------------------
  if P_BUKRS = 'X'.
    L_PART = 'BUKRS'.

    loop at T_T001 into S_T001.
      move       S_T001-BUKRS to LS_T001-BUKRS.
      move       S_T001-BUTXT to LS_T001-B_BUTXT.
      MOVE_LAND1 S_T001-LAND1    LS_T001-B_LAND1
                                 LS_T001-B_LANDX.

      append LS_T001 to LT_T001.
    endloop.

    format reset.
    loop at LT_T001 into LS_T001.
      write: /(5) LS_T001-BUKRS hotspot,
             (30) LS_T001-B_BUTXT.
      if LS_T001-B_LAND1 in EULAND.
        write:   LS_T001-B_LAND1  color col_total hotspot,
                 LS_T001-B_LANDX  color col_total.
      else.
        write:   LS_T001-B_LAND1  color col_normal hotspot,
                 LS_T001-B_LANDX  color col_normal.
      endif.

      perform SHOW_ROLES using '$BUKRS' LS_T001-BUKRS.

    endloop. " t001
    new-page.
  endif.

*-----------------------------------------------------------------------
* Plant T001W
*-----------------------------------------------------------------------
  if P_WERKS = 'X'.
    L_PART = 'WERKS'.

    loop at T_T001W into S_T001W.
      move       S_T001W-WERKS to LS_T001W-WERKS.
      move       S_T001W-NAME1 to LS_T001W-W_NAME1.
      MOVE_LAND1 S_T001W-LAND1    LS_T001W-W_LAND1
                                  LS_T001W-W_LANDX.
      MOVE_BWKEY S_T001W-BWKEY    LS_T001W-BWKEY
                                  LS_T001W-BUKRS
                                  LS_T001W-B_BUTXT
                                  LS_T001W-B_LAND1
                                  LS_T001W-B_LANDX.
      MOVE_VKORG S_T001W-VKORG    LS_T001W-VKORG
                                  LS_T001W-VTEXT.
      MOVE_EKORG S_T001W-EKORG    LS_T001W-EKORG
                                  LS_T001W-EKOTX.

      append LS_T001W to LT_T001W.
    endloop.

    format reset.
    loop at LT_T001W into LS_T001W.
      clear L_ERROR.

      write: /(5) LS_T001W-WERKS hotspot,
             (30) LS_T001W-W_NAME1.
      if LS_T001W-W_LAND1 in EULAND.
        write:   LS_T001W-W_LAND1  color col_total hotspot,
                 LS_T001W-W_LANDX  color col_total.
      else.
        write:   LS_T001W-W_LAND1  color col_normal hotspot,
                 LS_T001W-W_LANDX  color col_normal.
      endif.

      if LS_T001W-BWKEY is initial.
        L_ERROR = 'BWKEY is missing'(007).
        write: (5) LS_T001W-BWKEY color col_negative hotspot.
      else.
        write: (5) LS_T001W-BWKEY hotspot.
      endif.

      if LS_T001W-BUKRS is initial.
        if L_ERROR is initial. L_ERROR = 'BWKEY is missing'(007). endif.
        write: (5) LS_T001W-BUKRS color col_negative hotspot.
      else.
        write: (5) LS_T001W-BUKRS hotspot.
      endif.
      write:   (30) LS_T001W-B_BUTXT.

      if LS_T001W-W_LAND1 ne LS_T001W-B_LAND1.
        if L_ERROR is initial. L_ERROR = 'Countries do not match'(008). endif.
        write:   LS_T001W-B_LAND1  color col_negative hotspot,
                 LS_T001W-B_LANDX  color col_negative.
      elseif LS_T001W-B_LAND1 in EULAND.
        write:   LS_T001W-B_LAND1  color col_total hotspot,
                 LS_T001W-B_LANDX  color col_total.
      else.
        write:   LS_T001W-B_LAND1  color col_normal hotspot,
                 LS_T001W-B_LANDX  color col_normal.
      endif.

      write: (5)  LS_T001W-VKORG hotspot,
             (30) LS_T001W-VTEXT,
             (5)  LS_T001W-EKORG hotspot,
             (30) LS_T001W-EKOTX.

      write: L_ERROR.

      perform SHOW_ROLES using '$WERKS' LS_T001W-WERKS. " Plant
*      perform SHOW_ROLES using '$IWERK' LS_T001W-WERKS. " Maintenance planning plant
*      perform SHOW_ROLES using '$SWERK' LS_T001W-WERKS. " Maintenance plant

    endloop. " t001w
    new-page.
  endif.

*-----------------------------------------------------------------------
* Sales Organization TVKO
*-----------------------------------------------------------------------
  if P_VKORG = 'X'.
    L_PART = 'VKORG'.

    loop at T_TVKO into S_TVKO.
      MOVE_VKORG S_TVKO-VKORG     LS_TVKO-VKORG
                                  LS_TVKO-VTEXT.
      MOVE_BUKRS S_TVKO-BUKRS     LS_TVKO-BUKRS
                                  LS_TVKO-B_BUTXT
                                  LS_TVKO-B_LAND1
                                  LS_TVKO-B_LANDX.
      GET_WERKS WERKS S_TVKO-WERKS
                                  LS_TVKO-WERKS
                                  LS_TVKO-W_NAME1
                                  LS_TVKO-W_LAND1
                                  LS_TVKO-W_LANDX
                                  LS_TVKO-W_BWKEY
                                  LS_TVKO-K_BUKRS.
      MOVE_EKORG S_TVKO-EKORG     LS_TVKO-EKORG
                                  LS_TVKO-EKOTX.

      append LS_TVKO to LT_TVKO.
    endloop.

    format reset.
    loop at LT_TVKO into LS_TVKO.
      clear L_ERROR.

      write: /(5) LS_TVKO-VKORG hotspot,
             (30) LS_TVKO-VTEXT,
             (19) SPACE,
             (5)  SPACE.

      if LS_TVKO-BUKRS is initial.                               " direct in TVKO
        if L_ERROR is initial. L_ERROR = 'Company Code is missing'(009). endif.
        write: (5) LS_TVKO-BUKRS color col_negative hotspot.
      else.
        write: (5) LS_TVKO-BUKRS hotspot.
      endif.

      write: (30) LS_TVKO-B_BUTXT.

      if LS_TVKO-B_LAND1 in EULAND.
        write:   LS_TVKO-B_LAND1  color col_total hotspot,
                 LS_TVKO-B_LANDX  color col_total.
      else.
        write:   LS_TVKO-B_LAND1  color col_normal hotspot,
                 LS_TVKO-B_LANDX  color col_normal.
      endif.

      write: (5)  LS_TVKO-WERKS hotspot,
             (30) LS_TVKO-W_NAME1.
      if LS_TVKO-W_LAND1 in EULAND.
        write:   LS_TVKO-W_LAND1  color col_total hotspot,
                 LS_TVKO-W_LANDX  color col_total.
      else.
        write:   LS_TVKO-W_LAND1  color col_normal hotspot,
                 LS_TVKO-W_LANDX  color col_normal.
      endif.

      if LS_TVKO-WERKS is not initial and LS_TVKO-W_BWKEY is initial.
        L_ERROR = 'BWKEY is missing'(007).
        write: (5) LS_TVKO-W_BWKEY color col_negative hotspot.
      else.
        write: (5) LS_TVKO-W_BWKEY hotspot.
      endif.
      if LS_TVKO-K_BUKRS is not initial and LS_TVKO-K_BUKRS ne LS_TVKO-BUKRS.  " indirect via WERKS
        L_ERROR = 'Company Code is different'(010).
        write: (5) LS_TVKO-K_BUKRS color col_negative hotspot.
      else.
        write: (5) LS_TVKO-K_BUKRS hotspot.
      endif.

      write: (5)  LS_TVKO-EKORG hotspot,
             (30) LS_TVKO-EKOTX.

      write: L_ERROR.

      perform SHOW_ROLES using '$VKORG' LS_TVKO-VKORG.

    endloop. " tvko
    new-page.
  endif.

*-----------------------------------------------------------------------
* Purchase organization T024E
*-----------------------------------------------------------------------
  if P_EKORG = 'X'.
    L_PART = 'EKORG'.

    loop at T_T024E into S_T024E.
      MOVE_EKORG S_T024E-EKORG    LS_T024E-EKORG
                                  LS_T024E-EKOTX.
      MOVE_BUKRS S_T024E-BUKRS    LS_T024E-BUKRS
                                  LS_T024E-B_BUTXT
                                  LS_T024E-B_LAND1
                                  LS_T024E-B_LANDX.
      GET_WERKS EKORG S_T024E-EKORG
                                  LS_T024E-WERKS
                                  LS_T024E-W_NAME1
                                  LS_T024E-W_LAND1
                                  LS_T024E-W_LANDX
                                  LS_T024E-W_BWKEY
                                  LS_T024E-K_BUKRS.

      append LS_T024E to LT_T024E.
    endloop.

    format reset.
    loop at LT_T024E into LS_T024E.
      clear L_ERROR.

      write: /(5) LS_T024E-EKORG hotspot,
             (30) LS_T024E-EKOTX,
             (19) SPACE,
             (5)  SPACE.

      if LS_T024E-BUKRS is initial.                          " direct in E024E
        if L_ERROR is initial. L_ERROR = 'Company Code is missing'(009). endif.
        write: (5) LS_T024E-BUKRS color col_negative hotspot.
      else.
        write: (5) LS_T024E-BUKRS hotspot.
      endif.

      write: (30) LS_T024E-B_BUTXT.

      if LS_T024E-B_LAND1 in EULAND.
        write:   LS_T024E-B_LAND1  color col_total hotspot,
                 LS_T024E-B_LANDX  color col_total.
      else.
        write:   LS_T024E-B_LAND1  color col_normal hotspot,
                 LS_T024E-B_LANDX  color col_normal.
      endif.

      write: (5)  LS_T024E-WERKS hotspot,
             (30) LS_T024E-W_NAME1.
      if LS_T024E-W_LAND1 in EULAND.
        write:   LS_T024E-W_LAND1  color col_total hotspot,
                 LS_T024E-W_LANDX  color col_total.
      else.
        write:   LS_T024E-W_LAND1  color col_normal hotspot,
                 LS_T024E-W_LANDX  color col_normal.
      endif.

      if LS_T024E-WERKS is not initial and LS_T024E-W_BWKEY is initial.
        L_ERROR = 'BWKEY is missing'(007).
        write: (5) LS_T024E-W_BWKEY color col_negative hotspot.
      else.
        write: (5) LS_T024E-W_BWKEY hotspot.
      endif.
      if LS_T024E-K_BUKRS is not initial and LS_T024E-K_BUKRS ne LS_T024E-BUKRS.   " indirect via WERKS
        L_ERROR = 'Company Code is different'(010).
        write: (5) LS_T024E-K_BUKRS color col_negative hotspot.
      else.
        write: (5) LS_T024E-K_BUKRS hotspot.
      endif.

      write: L_ERROR.

      perform SHOW_ROLES using '$EKORG' LS_T024E-EKORG.

    endloop. " T024E
    new-page.
  endif.

*-----------------------------------------------------------------------
* Personal Area T500P
*-----------------------------------------------------------------------
  if P_PERSA = 'X'.
    L_PART = 'PERSA'.

    loop at T_T500P into S_T500P.
      move       S_T500P-PERSA to LS_T500P-PERSA.
      move       S_T500P-NAME1 to LS_T500P-P_NAME1.
      MOVE_LAND1 S_T500P-LAND1    LS_T500P-P_LAND1
                                  LS_T500P-P_LANDX.
      MOVE_MOLGA S_T500P-MOLGA    LS_T500P-MOLGA
                                  LS_T500P-M_LTEXT.
      MOVE_BUKRS S_T500P-BUKRS    LS_T500P-BUKRS
                                  LS_T500P-B_BUTXT
                                  LS_T500P-B_LAND1
                                  LS_T500P-B_LANDX.

      append LS_T500P to LT_T500P.
    endloop.

    format reset.
    loop at LT_T500P into LS_T500P.
      clear L_ERROR.

      write: /(5) LS_T500P-PERSA hotspot,
             (30) LS_T500P-P_NAME1.
      if LS_T500P-P_LAND1 in EULAND.
        write:   LS_T500P-P_LAND1  color col_total hotspot,
                 LS_T500P-P_LANDX  color col_total.
      else.
        write:   LS_T500P-P_LAND1  color col_normal hotspot,
                 LS_T500P-P_LANDX  color col_normal.
      endif.

      write:   (5)  LS_T500P-MOLGA hotspot,
               (19) LS_T500P-M_LTEXT.

      if LS_T500P-BUKRS is initial.
        if L_ERROR is initial. L_ERROR = 'BWKEY is missing'(007). endif.
        write: (5) LS_T500P-BUKRS color col_negative hotspot.
      else.
        write: (5) LS_T500P-BUKRS hotspot.
      endif.
      write:   (30) LS_T500P-B_BUTXT.

      if LS_T500P-P_LAND1 ne LS_T500P-B_LAND1.
        if L_ERROR is initial. L_ERROR = 'Countries do not match'(008). endif.
        write:   LS_T500P-B_LAND1  color col_negative hotspot,
                 LS_T500P-B_LANDX  color col_negative.
      elseif LS_T500P-B_LAND1 in EULAND.
        write:   LS_T500P-B_LAND1  color col_total hotspot,
                 LS_T500P-B_LANDX  color col_total.
      else.
        write:   LS_T500P-B_LAND1  color col_normal hotspot,
                 LS_T500P-B_LANDX  color col_normal.
      endif.

      write: L_ERROR.

      perform SHOW_ROLES using 'PERSA' LS_T500P-PERSA.

    endloop. " T500P
    new-page.
  endif.

*-----------------------------------------------------------------------

at line-selection.

* Call SE16 for a specif entry
  data: L_CURSOR(30),
        L_VALUE(30),
        L_TABLE(30),
        LS_SELTAB    type RSPARAMS,
        LT_SELTAB    type table of RSPARAMS.

  clear: L_TABLE.
  read current line.
  get cursor field L_CURSOR value L_VALUE.

  clear: LS_SELTAB, LT_SELTAB.

  if     L_CURSOR cs 'BUKRS'.
    L_TABLE  =  'T001'.
  elseif L_CURSOR cs 'BWKEY'.
    L_TABLE  =  'T001K'.
  elseif L_CURSOR cs 'WERKS'.
    L_TABLE  =  'T001W'.
  elseif L_CURSOR cs 'VKORG'.
    L_TABLE  =  'TVKO'.
  elseif L_CURSOR cs 'EKORG'.
    L_TABLE  =  'T024E'.
  elseif L_CURSOR cs 'PERSA'.
    L_TABLE  =  'T500P'.
  elseif L_CURSOR cs 'LAND1'.
    L_TABLE  =  'T005'.
  elseif L_CURSOR cs 'MOLGA'.
    L_TABLE  =  'T500L'.
  elseif L_CURSOR cs 'GSBER'.  " PA0001 Business Area
    L_TABLE  =  'TGSB'.
  elseif L_CURSOR cs 'BTRTL'.  " PA0001 Personnel Subarea
*    L_TABLE  =  'T001P'. " Use two selection fields: PERSA and BTRTL
*    LS_SELTAB-SELNAME = 'I2'.
*    LS_SELTAB-KIND    = 'S'.
*    LS_SELTAB-SIGN    = 'I'.
*    LS_SELTAB-OPTION  = 'EQ'.
*    LS_SELTAB-LOW     = L_VALUE.
*    append LS_SELTAB to LT_SELTAB.
*    read current line field value LS_PA-PERSA.
*    L_VALUE =  LS_PA-PERSA.
  elseif L_CURSOR = 'G_AGR_NAME'.   " Role
    clear G_AGR_NAME.
    read current line field value G_AGR_NAME.
    if G_AGR_NAME is not initial.
      call function 'PRGN_SHOW_EDIT_AGR'
        exporting
          AGR_NAME = G_AGR_NAME
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
    endif.
    exit.
  elseif L_CURSOR = 'G_PARENT_AGR'.   " Parent Role
    clear G_PARENT_AGR.
    read current line field value G_PARENT_AGR.
    if G_PARENT_AGR is not initial.
      call function 'PRGN_SHOW_EDIT_AGR'
        exporting
          AGR_NAME = G_PARENT_AGR
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
    endif.
    exit.
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
*       ACTION             = 'ANZE'
*       WITHOUT_SUBMIT     = ' '
*       GENERATION_FORCED  =
*       NEW_SEL            =
*       NO_STRUCTURE_CHECK = ' '
*       DATA_EXIT          = ' '
*       IMPORTING
*       PROGNAME           =
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

*-----------------------------------------------------------------------

form SHOW_ROLES
  using L_VARBL type AGR_1252-VARBL
        L_VALUE "type AGR_1252-LOW
        .

  types:
    begin of TS_ROLE,
      AGR_NAME   type AGR_1252-AGR_NAME,
      PARENT_AGR type AGR_DEFINE-PARENT_AGR,
      TEXT       type AGR_TEXTS-TEXT,
    end of TS_ROLE.

  data: LS_ROLE type TS_ROLE,
        LT_ROLE type table of TS_ROLE.

  types:
    begin of TS_ROLE_CNT,
      AGR_NAME   type AGR_1252-AGR_NAME,
      PARENT_AGR type AGR_DEFINE-PARENT_AGR,
      TEXT       type AGR_TEXTS-TEXT,
      CNT_USERS  type I,
    end of TS_ROLE_CNT.

  data: LS_ROLE_CNT type TS_ROLE_CNT,
        LT_ROLE_CNT type table of TS_ROLE_CNT.

  check P_ROLE is not initial.

  if L_VARBL = 'PERSA'.

    if P_STAR is initial.
      if P_USED is initial.

*       PERSA, P_STAR is initial, P_USED is initial
        select distinct R~AGR_NAME D~PARENT_AGR T~TEXT
          from AGR_1251 as R

         join AGR_DEFINE as D
           on    D~AGR_NAME = R~AGR_NAME

          left outer join AGR_TEXTS as T
            on    T~AGR_NAME = R~AGR_NAME
              and T~SPRAS    = SY-LANGU
              and T~LINE     = '00000'

*         join AGR_USERS as U
*           on    U~AGR_NAME = R~AGR_NAME

          into table LT_ROLE

          where R~AGR_NAME in ROLES
            and R~FIELD = L_VARBL
            and (   R~LOW =  L_VALUE
                 or R~LOW <= L_VALUE and R~HIGH >= L_VALUE
*                or R~LOW = '*'
                )
            and R~DELETED = SPACE

          order by R~AGR_NAME.

      else.

*       PERSA, P_STAR is initial, P_USED is not initial
        select distinct R~AGR_NAME D~PARENT_AGR T~TEXT count( distinct U~UNAME )
          from AGR_1251 as R

         join AGR_DEFINE as D
           on    D~AGR_NAME = R~AGR_NAME

          left outer join AGR_TEXTS as T
            on    T~AGR_NAME = R~AGR_NAME
              and T~SPRAS    = SY-LANGU
              and T~LINE     = '00000'

         join AGR_USERS as U
           on    U~AGR_NAME = R~AGR_NAME

          into table LT_ROLE_CNT

          where R~AGR_NAME in ROLES
            and R~FIELD = L_VARBL
            and (   R~LOW =  L_VALUE
                 or R~LOW <= L_VALUE and R~HIGH >= L_VALUE
*                or R~LOW = '*'
                )
            and R~DELETED = SPACE

          group by R~AGR_NAME D~PARENT_AGR T~TEXT

          order by R~AGR_NAME.

      endif. " P_USED

    else.

      if P_USED is initial.

*       PERSA, P_STAR is not initial, P_USED is initial
        select distinct R~AGR_NAME D~PARENT_AGR T~TEXT
          from AGR_1251 as R

         join AGR_DEFINE as D
           on    D~AGR_NAME = R~AGR_NAME

          left outer join AGR_TEXTS as T
            on    T~AGR_NAME = R~AGR_NAME
              and T~SPRAS    = SY-LANGU
              and T~LINE     = '00000'

*         join AGR_USERS as U
*           on    U~AGR_NAME = R~AGR_NAME

          into table LT_ROLE

          where R~AGR_NAME in ROLES
            and R~FIELD = L_VARBL
            and (   R~LOW =  L_VALUE
                 or R~LOW <= L_VALUE and R~HIGH >= L_VALUE
                 or R~LOW = '*'
                )
            and R~DELETED = SPACE

          order by R~AGR_NAME.

      else.

*       PERSA, P_STAR is not initial, P_USED is not initial
        select distinct R~AGR_NAME D~PARENT_AGR T~TEXT count( distinct U~UNAME )
          from AGR_1251 as R

         join AGR_DEFINE as D
           on    D~AGR_NAME = R~AGR_NAME

          left outer join AGR_TEXTS as T
            on    T~AGR_NAME = R~AGR_NAME
              and T~SPRAS    = SY-LANGU
              and T~LINE     = '00000'

          join AGR_USERS as U
            on    U~AGR_NAME = R~AGR_NAME

          into table LT_ROLE_CNT

          where R~AGR_NAME in ROLES
            and R~FIELD = L_VARBL
            and (   R~LOW =  L_VALUE
                 or R~LOW <= L_VALUE and R~HIGH >= L_VALUE
                 or R~LOW = '*'
                )
            and R~DELETED = SPACE

          group by R~AGR_NAME D~PARENT_AGR T~TEXT

          order by R~AGR_NAME.

      endif. " P_USED
    endif. " P_STAR

  else. " L_VARBL = org value

    if P_STAR is initial.
      if P_USED is initial.

*       org value, P_STAR is initial, P_USED is initial
        select distinct R~AGR_NAME D~PARENT_AGR T~TEXT
          from AGR_1252 as R

         join AGR_DEFINE as D
           on    D~AGR_NAME = R~AGR_NAME

          left outer join AGR_TEXTS as T
            on    T~AGR_NAME = R~AGR_NAME
              and T~SPRAS    = SY-LANGU
              and T~LINE     = '00000'

*         join AGR_USERS as U
*           on    U~AGR_NAME = R~AGR_NAME

          into table LT_ROLE

          where R~AGR_NAME in ROLES
            and R~VARBL = L_VARBL
            and (   R~LOW =  L_VALUE
                 or R~LOW <= L_VALUE and R~HIGH >= L_VALUE
*                or R~LOW = '*'
                )

          order by R~AGR_NAME.

      else.

*       org value, P_STAR is initial, P_USED is not initial
        select distinct R~AGR_NAME D~PARENT_AGR T~TEXT count( distinct U~UNAME )
         from AGR_1252 as R

         join AGR_DEFINE as D
           on    D~AGR_NAME = R~AGR_NAME

         left outer join AGR_TEXTS as T
           on    T~AGR_NAME = R~AGR_NAME
             and T~SPRAS    = SY-LANGU
             and T~LINE     = '00000'

         join AGR_USERS as U
           on    U~AGR_NAME = R~AGR_NAME

         into table LT_ROLE_CNT

         where R~AGR_NAME in ROLES
           and R~VARBL = L_VARBL
           and (   R~LOW =  L_VALUE
                or R~LOW <= L_VALUE and R~HIGH >= L_VALUE
*               or R~LOW = '*'
               )

         group by R~AGR_NAME D~PARENT_AGR T~TEXT

         order by R~AGR_NAME.

      endif. " P_USED

    else.
      if P_USED is initial.

*       org value, P_STAR is not initial, P_USED is initial
        select distinct R~AGR_NAME D~PARENT_AGR T~TEXT
          from AGR_1252 as R

         join AGR_DEFINE as D
           on    D~AGR_NAME = R~AGR_NAME

          left outer join AGR_TEXTS as T
            on    T~AGR_NAME = R~AGR_NAME
              and T~SPRAS    = SY-LANGU
              and T~LINE     = '00000'

*         join AGR_USERS as U
*           on    U~AGR_NAME = R~AGR_NAME

          into table LT_ROLE

          where R~AGR_NAME in ROLES
            and R~VARBL = L_VARBL
            and (   R~LOW =  L_VALUE
                 or R~LOW <= L_VALUE and R~HIGH >= L_VALUE
                 or R~LOW = '*'
                )

          order by R~AGR_NAME.

      else.

*       org value, P_STAR is not initial, P_USED is not initial
        select distinct R~AGR_NAME D~PARENT_AGR T~TEXT count( distinct U~UNAME )
         from AGR_1252 as R

         join AGR_DEFINE as D
           on    D~AGR_NAME = R~AGR_NAME

         left outer join AGR_TEXTS as T
           on    T~AGR_NAME = R~AGR_NAME
             and T~SPRAS    = SY-LANGU
             and T~LINE     = '00000'

         join AGR_USERS as U
           on    U~AGR_NAME = R~AGR_NAME

         into table LT_ROLE_CNT

         where R~AGR_NAME in ROLES
           and R~VARBL = L_VARBL
           and (   R~LOW =  L_VALUE
                or R~LOW <= L_VALUE and R~HIGH >= L_VALUE
                or R~LOW = '*'
               )

         group by R~AGR_NAME D~PARENT_AGR T~TEXT

         order by R~AGR_NAME.

      endif. " P_USED
    endif. " P_STAR

  endif. " L_VARBL

  format intensified.
  if P_USED is initial.
    loop at LT_ROLE into LS_ROLE.
      G_AGR_NAME   = LS_ROLE-AGR_NAME.
      G_PARENT_AGR = LS_ROLE-PARENT_AGR.
      write: /7 G_AGR_NAME hotspot, G_PARENT_AGR hotspot, LS_ROLE-TEXT.
    endloop.
  else.
    loop at LT_ROLE_CNT into LS_ROLE_CNT.
      G_AGR_NAME = LS_ROLE_CNT-AGR_NAME.
      G_PARENT_AGR = LS_ROLE_CNT-PARENT_AGR.
      write: /7 G_AGR_NAME hotspot, G_PARENT_AGR hotspot, LS_ROLE_CNT-TEXT.

      if LS_ROLE_CNT-CNT_USERS = 0.
      elseif LS_ROLE_CNT-CNT_USERS = 1.
        write: (6) LS_ROLE_CNT-CNT_USERS no-sign, 'user'(020).
      else.
        write: (6) LS_ROLE_CNT-CNT_USERS no-sign, 'users'(021).
      endif.

    endloop.
  endif.
  format reset.
endform.