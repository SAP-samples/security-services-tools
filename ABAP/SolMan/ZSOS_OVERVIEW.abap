*&---------------------------------------------------------------------*
*& Report  ZSOS_OVERVIEW
*& Show EWA and SOS rating overview
*&---------------------------------------------------------------------*
*&
*&
*& 24.08.2021 Initial version
*& 25.08.2021 Show SOS Check ID
*& 30.08.2021 Show count
*& 20.01.2022 Option to choose ALV layout
*&            Default selection for change datum
*&            Turn count field into integer for easy filtering (but no differentiation between empty and zero anymore)
*&            Value help for rating
*& 27.01.2022 Support for icon @0S@ 'information'
*& 01.02.2022 Correction to show the lastest SOS per system
*& 27.07.2022 Show results from EWA as well
*&---------------------------------------------------------------------*
REPORT zsos_overview
  MESSAGE-ID dsvas.

CONSTANTS: c_program_version(30) TYPE c VALUE '27.07.2022'.

*&---------------------------------------------------------------------*

* SALV types and data

TYPES:
  BEGIN OF ts_outtab,
    instno        TYPE dsvasdinstno,
    dbid          TYPE dsvasddbid,
    sessno        TYPE dsvasdsessino,
    change_date   TYPE dsvasdchangedate,
    creation_date TYPE dsvasdcreatdate,
    row           TYPE i,
    main_chapter  TYPE string,
    chapter       TYPE string,
    check         TYPE string,
    alert_id      TYPE string,                " only used for EWA
    sos_check_id  TYPE string,                " only used for SOS
    rating        TYPE string,
    rating_text   TYPE string,
    count         TYPE i,                     " only used for SOS
    check_group   TYPE dsvasdcheckgrp,
    check_id      TYPE dsvasdcheckid,
    con           TYPE dsvasdcontext,
    ins           TYPE dsvasdcontextinstance,
  END OF ts_outtab,
  tt_outtab TYPE TABLE OF ts_outtab.
DATA:
  ls_outtab TYPE ts_outtab,
  lt_outtab TYPE tt_outtab.

*&---------------------------------------------------------------------*

* Selection screen
SELECTION-SCREEN BEGIN OF BLOCK type WITH FRAME TITLE s_type.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_ewa RADIOBUTTON GROUP type USER-COMMAND type. "user command required to trigger OUTPUT event
SELECTION-SCREEN COMMENT 3(40) ss_ewa FOR FIELD s_ewa.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_sos RADIOBUTTON GROUP type DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(40) ss_sos FOR FIELD s_sos.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK type.

SELECTION-SCREEN BEGIN OF BLOCK sess WITH FRAME TITLE s_sess.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_inst FOR FIELD instno.
SELECT-OPTIONS instno  FOR ls_outtab-instno.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_dbid FOR FIELD dbid.
SELECT-OPTIONS dbid    FOR ls_outtab-dbid.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_udate FOR FIELD chgdate.
SELECT-OPTIONS chgdate FOR ls_outtab-change_date.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_cdate FOR FIELD credate.
SELECT-OPTIONS credate FOR ls_outtab-creation_date.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_sess FOR FIELD sessno.
SELECT-OPTIONS sessno  FOR ls_outtab-sessno.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK sess.

SELECTION-SCREEN BEGIN OF BLOCK chap WITH FRAME TITLE s_chap.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_main FOR FIELD main.
DATA s_main_chapter(40).
SELECT-OPTIONS main    FOR s_main_chapter LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_chap FOR FIELD chapter.
DATA s_chapter(40).
SELECT-OPTIONS chapter FOR s_chapter LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_chk FOR FIELD chk.
DATA s_check(40).
SELECT-OPTIONS chk     FOR s_check LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_ewaid FOR FIELD ewa_id MODIF ID ewa.
DATA s_alert_id TYPE i.
SELECT-OPTIONS ewa_id FOR s_alert_id MODIF ID ewa.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_sosid FOR FIELD sos_id MODIF ID sos.
DATA s_sos_check_id(20).
SELECT-OPTIONS sos_id FOR s_sos_check_id MODIF ID sos.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_chkgp FOR FIELD chkgrp.
SELECT-OPTIONS chkgrp  FOR ls_outtab-check_group.
SELECTION-SCREEN END OF LINE.

"SELECTION-SCREEN BEGIN OF LINE.
"SELECTION-SCREEN COMMENT 1(28) ss_chkid FOR FIELD chkid.
"SELECT-OPTIONS chkid   FOR ls_outtab-check_id.
"SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_rat FOR FIELD rating.
DATA s_rating_text(40).
SELECT-OPTIONS rating  FOR s_rating_text LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK chap.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_new AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(40) ss_new FOR FIELD p_new.
SELECTION-SCREEN END OF LINE.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) ps_lout FOR FIELD p_layout.
PARAMETERS       p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

*&---------------------------------------------------------------------*

* Constants to distinguish between EWA and SOS
DATA:
  c_bundle_id  TYPE dsvassessadmin-bundle_id,    " Bundle id
  c_relid      TYPE dsvasresultsgen-relid,       " Check tables
  c_sessitype  TYPE dsvasdsessitype,             " Session type
  c_grp        TYPE dsvasdcheckgrp,              " Rating overview
  c_id         TYPE dsvasdcheckid,               " Check Overview
  c_ses_gr_typ TYPE dsvasdsessgrptype,           " Authorization object D_SVAS_SES
  c_pkg_id_typ TYPE dsvasdpkgidtype,             " Authorization object D_SVAS_SES
  c_days       TYPE i.                           " Days for value help on sessions

INITIALIZATION.

  s_type   = 'Session type'(010).
  ss_ewa   = 'EarlyWatch Alert (EWA)'(011).
  ss_sos   = 'Security Optimization Service (SOS)'(012).

  s_sess   = 'Select session'(001).
  ss_inst  = 'Installation number'(002).
  ss_dbid  = 'System ID'(m09).
  ss_udate = 'Change date'(003).
  ss_cdate = 'Create date'(004).
  ss_sess  = 'Session number'(m10).

  s_chap   = 'Filter'(005).
  ss_main  = 'Main chapter'(m01).
  ss_chap  = 'Chapter'(m02).
  ss_chk   = 'Check'(m03).
  ss_ewaid = 'EWA Alert ID'(m15).
  ss_sosid = 'SOS Check ID'(m12).
  ss_chkgp = 'Check group'(006).
  "ss_chkid = 'Check ID'(007).
  ss_rat   = 'Rating'(m04).

  ps_lout     = 'Layout'(008).

  ss_new   = 'Newest result per system only'(009).

  CONCATENATE 'Program version:'(VER) c_program_version INTO ss_vers
    SEPARATED BY space.

* Default values

  GET PARAMETER ID 'DSVAS_SESSNO' FIELD sessno.

*&---------------------------------------------------------------------*

  TABLES sscrfields.

AT SELECTION-SCREEN.

  IF sscrfields-ucomm EQ 'TYPE'. "radiobutton was changed
    CLEAR: sessno, sessno[].
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'EWA'.
      IF s_ewa = 'X'.
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF screen-group1 = 'SOS'.
      IF s_sos = 'X'.
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF s_ewa = 'X'.
    c_bundle_id  = 'EW_ALERT'.  " Bundle id
    c_relid      = 'CT'.        " Check tables
    c_sessitype  = 'EA'.        " Session type
    c_grp        = 'EA_HEAD'.   " Rating overview
    c_id         = '00003'.     " 00003 Check Overview, 00018 Alert Overview and Service Summary
    c_ses_gr_typ = 'Earlywatch/Alert'.
    c_pkg_id_typ = 'EW_ALERT/EA'.
    c_days       = 30.          " Get EWA sessions which are not older than one month

  ELSEIF s_sos = 'X'.
    c_bundle_id  = 'GSS_SEC'.   " Bundle id
    c_relid      = 'CT'.        " Check tables
    c_sessitype  = 'GS'.        " Session type
    c_grp        = 'SC_FINISH'. " Rating overview
    c_id         = '00013'.     " Rating overview, get it either from check 00013 or from check 00011
    c_ses_gr_typ = 'GSS Security Service/Session GS'.
    c_pkg_id_typ = 'GSS_SEC/GS'.
    c_days       = 365.         " Get SOS sessions which are not older than one year

  ENDIF.

  CLEAR: chgdate, chgdate[].
  chgdate-option = 'GE'.
  chgdate-sign   = 'I'.
  chgdate-low    = sy-datum - c_days. " Omit old sessions
  APPEND chgdate.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON VALUE-REQUEST FOR sessno
*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sessno-low.
  PERFORM f4_sessno USING 'SESSNO-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sessno-high.
  PERFORM f4_sessno USING 'SESSNO-HIGH'.

FORM f4_sessno
  USING
    l_dynprofield  TYPE help_info-dynprofld.

  DATA:
    retfield      TYPE dfies-fieldname,
    "dynpro_values   TYPE TABLE OF dynpread,
    "field_value     LIKE LINE OF dynpro_values,
    "field_tab       TYPE TABLE OF dfies  WITH HEADER LINE,
    field_mapping TYPE STANDARD TABLE OF dselc,
    BEGIN OF value_tab OCCURS 0,
      "sessitype     TYPE dsvassessadmin-sessitype,
      sessno      TYPE dsvassessadmin-sessno,
      "bundle_id     TYPE dsvassessadmin-bundle_id,
      "bundle_versnr TYPE dsvassessadmin-bundle_versnr,
      dbid        TYPE dsvassessadmin-dbid,
      change_user TYPE dsvassessadmin-change_user,
      change_date TYPE dsvassessadmin-change_date,
      description TYPE dsvassessadmin-description,
    END OF value_tab.

  DATA(min_change_date) = sy-datum - c_days.
  SELECT
      "h~sessitype
      h~sessno h~change_user h~change_date
      "a~bundle_id a~bundle_versnr
      a~dbid a~description
    FROM dsvassessionhead AS h
    JOIN dsvassessadmin   AS a
      ON  a~sessno = h~sessno
    JOIN dsvasresultsgen  AS r       " Only sessions having results
      ON  r~relid      = c_relid     " Check tables
      AND r~sessitype  = h~sessitype " Session type
      AND r~sessno     = h~sessno
      AND r~grp        = c_grp       " Rating overview
      AND r~id         = c_id        " Check Overview
      AND r~srtf2      = 0           " first entry
    INTO CORRESPONDING FIELDS OF value_tab
    WHERE a~bundle_id   =  c_bundle_id
      AND h~sessitype   =  c_sessitype
      AND a~change_date >= min_change_date
    ORDER BY a~dbid ASCENDING a~change_date DESCENDING.
    APPEND value_tab.
  ENDSELECT.
  "SORT value_tab BY dbid ASCENDING change_date DESCENDING.

  retfield = l_dynprofield.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SESSNO' "retfield "(I do not know why this works better)
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = l_dynprofield
      value_org       = 'S'
    TABLES
*     field_tab       = field_tab
      value_tab       = value_tab
      "dynpfld_mapping = field_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM. "F4_SESSNO

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON p_layout
*----------------------------------------------------------------------*

DATA: gs_alv_lout_variant TYPE disvariant.

AT SELECTION-SCREEN ON p_layout.
  CHECK NOT p_layout IS INITIAL.
  PERFORM handle_at_selscr_on_p_layout USING p_layout sy-repid 'A'.
*
FORM handle_at_selscr_on_p_layout
   USING id_varname TYPE disvariant-variant
         id_repid   TYPE sy-repid
         id_save    TYPE c.

  DATA: ls_variant TYPE disvariant.

  ls_variant-report  = id_repid.
  ls_variant-variant = id_varname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save        = id_save
    CHANGING
      cs_variant    = ls_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.
*   Selected layout variant is not found
    MESSAGE e204(0k).
  ENDIF.

  gs_alv_lout_variant-report  = id_repid.
  gs_alv_lout_variant-variant = id_varname.

ENDFORM.                    " handle_at_selscr_on_p_layout

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.  " ( Note 890141 )
  PERFORM handle_at_selscr_f4_p_layout USING    sy-repid 'A'
                                       CHANGING p_layout.
*
FORM handle_at_selscr_f4_p_layout
  USING    id_repid   TYPE sy-repid
           id_save    TYPE c
  CHANGING ed_varname TYPE disvariant-variant.

  gs_alv_lout_variant-report = id_repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = gs_alv_lout_variant
      i_save        = id_save
    IMPORTING
      es_variant    = gs_alv_lout_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc = 0.
    ed_varname = gs_alv_lout_variant-variant.
  ELSE.
    MESSAGE s073(0k).
*   Keine Anzeigevariante(n) vorhanden
  ENDIF.

ENDFORM.                               " handle_at_selscr_f4_p_layout


*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON VALUE-REQUEST FOR rating
*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR rating-low.
  PERFORM f4_rating USING 'RATING-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR rating-high.
  PERFORM f4_rating USING 'RATING-HIGH'.

FORM f4_rating
  USING
    l_dynprofield  TYPE help_info-dynprofld.

  DATA:
    retfield      TYPE dfies-fieldname,
    "dynpro_values   TYPE TABLE OF dynpread,
    "field_value     LIKE LINE OF dynpro_values,
    "field_tab       TYPE TABLE OF dfies  WITH HEADER LINE,
    field_mapping TYPE STANDARD TABLE OF dselc,
    BEGIN OF value_tab OCCURS 0,
      rating TYPE text15,
    END OF value_tab.

  value_tab-rating = 'red'(R01).          APPEND value_tab. " @AG@
  value_tab-rating = 'yellow'(R02).       APPEND value_tab. " @AH@
  value_tab-rating = 'green'(R03).        APPEND value_tab. " @01@
  value_tab-rating = 'information'(R04).  APPEND value_tab. " @0S@
  value_tab-rating = 'not rated'(R05).    APPEND value_tab. " @BZ@
  value_tab-rating = 'unknown'(R06).      APPEND value_tab. " others

  retfield = l_dynprofield.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'RATING' "retfield "(i do not know why this works better)
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = l_dynprofield
      value_org       = 'S'
    TABLES
*     field_tab       = field_tab
      value_tab       = value_tab
      "dynpfld_mapping = field_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM. "F4_SESSNO

*&---------------------------------------------------------------------*

START-OF-SELECTION.

* Authority check for Service Package, Session and Report
  AUTHORITY-CHECK OBJECT 'D_SVAS_SES'
    ID 'SES_GR_TYP' FIELD c_ses_gr_typ
    ID 'PKG_ID_TYP' FIELD c_pkg_id_typ
    ID 'ACTVT_SES'  FIELD 'R3'                              " Display Reports
    ID 'AUTHGROUP'  FIELD 'R'.                              " Restricted Authorization
  IF sy-subrc IS NOT INITIAL.
    "message e425(dsvas). " No authorization to display a session
    MESSAGE e430(dsvas). " No authorization to display a report
  ENDIF.

* Authorization check for LMDB Object (needs repetition as soon as the system id is known)
  AUTHORITY-CHECK OBJECT 'AI_LMDB_OB'
           ID 'ACTVT'      FIELD '03'     " display
           ID 'LMDB_DOMA'  FIELD 'LDB'
           ID 'LMDB_NAMES' FIELD 'ACTIVE'
           ID 'LMDB_MTYPE' FIELD 'TECSYS'
           ID 'LMDB_STYPE' DUMMY          " system type ABAP, HANA,...
           ID 'LMDB_OBJID' DUMMY.         " System id
  IF sy-subrc IS NOT INITIAL.
    "message e073(ai_lmdb_external) with sy-uname 'display' 'ABAP'. "User &1 is not authorized for activity '&2' for techn. systems of type &3
    "message e425(dsvas). " No authorization to display a session
    MESSAGE e430(dsvas). " No authorization to display a report
  ENDIF.

  PERFORM get_sessions.
  PERFORM show_alv.

*&---------------------------------------------------------------------*

FORM get_sessions.

  " load sessions
  DATA lt_dsvassessadmin TYPE TABLE OF dsvassessadmin.
  SELECT
      "h~sessitype,
      "h~sessno,
      "h~change_user,
      "h~change_date,
      "a~bundle_id,
      "a~bundle_versnr,
      a~sessno,
      a~instno,
      a~dbid,
      a~change_date,
      a~creation_date,
      a~description
    FROM dsvassessionhead AS h
    JOIN dsvassessadmin   AS a
      ON  a~sessno = h~sessno
    JOIN dsvasresultsgen  AS r       " Only sessions having results
      ON  r~relid         =  'CT'        " Check tables
      AND r~sessitype     =  h~sessitype " Session type
      AND r~sessno        =  h~sessno    " Session number
      AND r~grp           =  @c_grp      " Rating overview
      AND r~id            =  @c_id       " Rating overview, get it either from check 00013 or from check 00011
      AND r~srtf2         =  0           " first entry
    INTO CORRESPONDING FIELDS OF TABLE @lt_dsvassessadmin
    WHERE a~bundle_id     =  @c_bundle_id
      AND h~sessitype     =  @c_sessitype
      AND a~sessno        IN @sessno
      AND a~instno        IN @instno
      AND a~dbid          IN @dbid
      AND a~change_date   IN @chgdate
      AND a~creation_date IN @credate
    ORDER BY a~dbid, a~instno, a~change_date DESCENDING  " allow filtering for latest session per system
    .

  " process sessions
  CLEAR ls_outtab.
  LOOP AT lt_dsvassessadmin INTO DATA(ls_dsvassessadmin).
    IF p_new = abap_true.
      CHECK ls_dsvassessadmin-instno      NE ls_outtab-instno
         OR ls_dsvassessadmin-dbid        NE ls_outtab-dbid
         OR ls_dsvassessadmin-change_date >  ls_outtab-change_date.
    ENDIF.

    ls_outtab-instno        = ls_dsvassessadmin-instno.
    ls_outtab-dbid          = ls_dsvassessadmin-dbid.
    ls_outtab-sessno        = ls_dsvassessadmin-sessno.
    ls_outtab-change_date   = ls_dsvassessadmin-change_date.
    ls_outtab-creation_date = ls_dsvassessadmin-creation_date.

    PERFORM get_resultsgen
      USING
        ls_dsvassessadmin.
  ENDLOOP.
ENDFORM.

FORM get_resultsgen
  USING
    ls_dsvassessadmin TYPE dsvassessadmin.

  DATA:
    ls_dsvasresultsgen TYPE dsvasresultsgen,
    lt_dsvasresultsgen TYPE TABLE OF dsvasresultsgen.

  IF s_ewa = 'X'. " Show data from check 00018 Alert Overview and Service Summary, too
    SELECT * FROM dsvasresultsgen INTO TABLE @lt_dsvasresultsgen
      WHERE relid      = @c_relid
        AND sessitype  = @c_sessitype
        AND sessno     = @ls_dsvassessadmin-sessno
        AND grp        = @c_grp
        AND id         = '00018'     " c_id = 00003 Check Overview, 00018 Alert Overview and Service Summary
        AND srtf2      = 0           " first entry
      ORDER BY relid, sessitype, sessno, grp, id, con, ins, varkey, srtf2
      .

    LOOP AT lt_dsvasresultsgen INTO ls_dsvasresultsgen.
      PERFORM get_check_table
        USING ls_dsvasresultsgen.
    ENDLOOP.
  ENDIF.

  SELECT * FROM dsvasresultsgen INTO TABLE @lt_dsvasresultsgen
    WHERE relid      = @c_relid
      AND sessitype  = @c_sessitype
      AND sessno     = @ls_dsvassessadmin-sessno
      AND grp        = @c_grp
      AND id         = @c_id
      AND srtf2      = 0           " first entry
    ORDER BY relid, sessitype, sessno, grp, id, con, ins, varkey, srtf2
    .

  LOOP AT lt_dsvasresultsgen INTO ls_dsvasresultsgen.
    PERFORM get_check_table
      USING ls_dsvasresultsgen.
  ENDLOOP.

ENDFORM.

FORM get_check_table
  USING
    ls_dsvasresultsgen TYPE dsvasresultsgen.

  DATA :
  results_key       TYPE  dsvasresultsgenkey. " key for IMPORT ... FROM DATABASE dsvasresultsgen(ct)

  " metadata table
  TYPES :
    BEGIN OF  type_check_table_meta_h_data,
      columns         TYPE dsvastable-columns,
      column_metadata TYPE dsvas_type_ct_col_meta_table,
      title           TYPE dsvastablet-text,
      action          TYPE dsvastable-action,
      fixed_columns   TYPE dsvastable-fixed_columns,
      invisible       TYPE dsvastable-invisible,
      in_report       TYPE c,
      edit_mode       TYPE dsvastable-edit_mode,
    END OF  type_check_table_meta_h_data,
    type_check_table_meta_h_table TYPE  STANDARD TABLE OF  type_check_table_meta_h_data.
  DATA :
  lt_metadata_h TYPE  type_check_table_meta_h_table.

  " data tables
  DATA :
    table_data   TYPE  dsvas_type_tables_data_table,   " contains STRING
    table_data_s TYPE  dsvas_type_tables_data_table_s. " contains CHAR120

  " Build the check key
  results_key-sessitype  =  ls_dsvasresultsgen-sessitype.
  results_key-sessno     =  ls_dsvasresultsgen-sessno.
  results_key-versnr     =  ls_dsvasresultsgen-versnr.
  results_key-grp        =  ls_dsvasresultsgen-grp.
  results_key-id         =  ls_dsvasresultsgen-id.
  results_key-con        =  ls_dsvasresultsgen-con.
  results_key-ins        =  ls_dsvasresultsgen-ins.

  IMPORT
    table_data_ext  =  table_data     " contains STRING
    table_data      =  table_data_s   " contains CHAR120
    metadata        =  lt_metadata_h  " column headings
    FROM DATABASE dsvasresultsgen(ct)
    ID  results_key
    IGNORING CONVERSION ERRORS
    ACCEPTING PADDING
    ACCEPTING TRUNCATION.

  IF sy-subrc = 0.

    IF s_ewa = 'X'.
      PERFORM process_ewa_table
        USING
          ls_dsvasresultsgen-grp
          ls_dsvasresultsgen-id
          table_data
          .

    ELSEIF s_sos = 'X'.
      PERFORM process_sos_table
        USING
          ls_dsvasresultsgen-grp
          ls_dsvasresultsgen-id
          table_data
          .

    ENDIF.

  ENDIF.
ENDFORM.

FORM process_ewa_table
  USING
    check_group TYPE dsvasdcheckgrp
    check_id    TYPE dsvasdcheckid
    table_data  TYPE dsvas_type_tables_data_table
    .

  DATA rating_tabix TYPE i.
  rating_tabix = 2.

  " get meta data for ratings (column headings, not used yet)
  "READ TABLE lt_metadata_h INTO DATA(ls_metadata_h) INDEX rating_tabix.

  " get check table for ratings
  READ TABLE table_data INTO DATA(lt_rating_table) INDEX rating_tabix.

  LOOP AT lt_rating_table INTO DATA(ls_rating_entry).
    ls_outtab-row = ls_rating_entry-row.
    CLEAR:
      "ls_outtab-main_chapter, " Keep text from previous line
      ls_outtab-chapter,
      ls_outtab-check,
      ls_outtab-alert_id,
      "ls_outtab-sos_check_id, "not used for EWA
      ls_outtab-rating,
      ls_outtab-rating_text,
      "ls_outtab-count,        "not used for EWA
      ls_outtab-check_group,
      ls_outtab-check_id,
      ls_outtab-con,
      ls_outtab-ins.

    LOOP AT ls_rating_entry-columns INTO DATA(ls_rating_column).

      IF check_id = '00018'.
        " EA_HEAD Check 00018 Table 2 Alert Overview and Service Summary
        "                         main_chapter = 'Alerts'
        "  1  Rating              rating / rating_text
        "  2  Description         check
        "  3  Category            chapter
        "  4  Area
        "  5  Context             con
        "  6  Instance            ins
        "  7  Alert ID            sos_check_id
        "  8  Check group         check_group
        "  9  Check ID            check_id
        " 10  Referenced group
        " 11  Priority
        ls_outtab-main_chapter = 'Alerts'(013).
        CASE ls_rating_column-col.
          WHEN 1. " Rating
            ls_outtab-rating       = ls_rating_column-field.
            " Get rating text
            PERFORM get_rating_text
              USING
                ls_outtab-rating
              CHANGING
                ls_outtab-rating_text.
          WHEN 2. " Description
            FIND REGEX '<.*>(.*)<.*>'     " find text '<[^>]*>([^<]*)<[^>]*>' or '<.*>(.*)<.*>'
                IN ls_rating_column-field
                SUBMATCHES ls_outtab-check.
          WHEN 3. " Category
            ls_outtab-chapter      = ls_rating_column-field.
          WHEN 4. " Area
            "concatenate ls_outtab-chapter ls_rating_column-field into ls_outtab-chapter SEPARATED BY space.
          WHEN 5. " Context
            ls_outtab-con          = ls_rating_column-field.
          WHEN 6 . " Instance
            ls_outtab-ins          = ls_rating_column-field.
          WHEN 7 . " Alert ID
            ls_outtab-alert_id     = ls_rating_column-field.
          WHEN 8. " Check group
            ls_outtab-check_group  = ls_rating_column-field.
          WHEN 9. " Check ID
            ls_outtab-check_id     = ls_rating_column-field.
          WHEN 10. " Referenced group
          WHEN 11. " Priority
            "concatenate ls_outtab-chapter ls_rating_column-field into ls_outtab-chapter SEPARATED BY space.
        ENDCASE.

      ELSEIF check_id = '00003'.
        " EA_HEAD Check 00003 Table 2 Check overview
        "  1  Topic Rating
        "  2  Topic
        "  3  Check-GRP
        "  4  Check-ID
        "  5  Context
        "  6  Instance
        "  7  Subtopic Rating
        "  8  Subtopic
        "  9  Check-GRP
        " 10  Check-ID
        " 11  Context
        " 12  Instance
        CASE ls_rating_column-col.
          WHEN 1 OR 7. " Rating
            ls_outtab-rating       = ls_rating_column-field.
            " Get rating text
            PERFORM get_rating_text
              USING
                ls_outtab-rating
              CHANGING
                ls_outtab-rating_text.
          WHEN 2. " Topic
            FIND REGEX '<.*>(.*)<.*>'     " find text '<[^>]*>([^<]*)<[^>]*>' or '<.*>(.*)<.*>'
                IN ls_rating_column-field
                SUBMATCHES ls_outtab-main_chapter.
          WHEN 8. " Sub topic
            FIND REGEX '<.*>(.*)<.*>'     " find text '<[^>]*>([^<]*)<[^>]*>' or '<.*>(.*)<.*>'
                IN ls_rating_column-field
                SUBMATCHES ls_outtab-chapter.
          WHEN 3 OR 9. " Check-GRP
            ls_outtab-check_group  = ls_rating_column-field.
          WHEN 4 OR 10. " Check-ID
            ls_outtab-check_id     = ls_rating_column-field.
          WHEN 5 OR 11. " Context
            ls_outtab-con          = ls_rating_column-field.
          WHEN 6 OR 12. " Instance
            ls_outtab-ins          = ls_rating_column-field.
        ENDCASE.
      ENDIF.

    ENDLOOP. " ls_rating_entry-columns

    " Filter
    CHECK ls_outtab-main_chapter IN main
      AND ls_outtab-chapter      IN chapter
      AND ls_outtab-check        IN chk
      AND ls_outtab-alert_id     IN ewa_id
      "AND ls_outtab-sos_check_id IN sos_id " not used for EWA
      AND ls_outtab-check_group  IN chkgrp
      "AND ls_outtab-check_id     IN chkid
      AND ls_outtab-rating_text  IN rating.

    APPEND ls_outtab TO lt_outtab.
  ENDLOOP. " lt_rating_table
ENDFORM.


FORM process_sos_table
  USING
    check_group TYPE dsvasdcheckgrp
    check_id    TYPE dsvasdcheckid
    table_data  TYPE dsvas_type_tables_data_table
    .

  " SC_FINISH Check 00011 Table 1 Rating overview
  "  1  Main Chapter
  "  3  Check
  "  2  Chapter
  "  4  Rating

  " SC_FINISH Check 00013 Table 2 Rating overview
  "  1  Main Chapter
  "  2  Chapter
  "  3  Check
  "  4  Rating
  "  5  Check grp (hidden)
  "  6  Check id (hidden)
  "  7  Context (hidden)
  "  8  Instance (hidden)

  " SC_FINISH Check 00013 Table 5 User count
  "  1  Check group
  "  2  Check id
  "  3  Count
  "  4  Rating

  DATA rating_tabix TYPE i.

  "    " Both checks SC_FINISH 00011 and 00013 contain the rating table
  IF     check_id = '00011'. " SC_FINISH -> 00006 Summary -> 00011 Rating overview (rading data in 1st check table)

    rating_tabix = 1.

  ELSEIF check_id = '00013'. " SC_FINISH -> 00013 Issues and Rating Overview (rating in 2nd check table)

    rating_tabix = 2.

    " Check SC_FINISH 00013 shows additional check tables:
    " 1: Main Chapter, Chapter, Issue
    " 5: Check group, Check id, Count, Rating
    TYPES:
      BEGIN OF ts_count,
        check_group TYPE dsvasdcheckgrp,          " Check group in DSA
        check_id    TYPE dsvasdcheckid,           " Check ID in DSA
        count       TYPE i,
        rating      TYPE string,
      END OF ts_count.
    DATA:
      ls_count TYPE ts_count,
      lt_count TYPE SORTED TABLE OF ts_count
        WITH UNIQUE KEY check_group check_id.
    READ TABLE table_data INTO DATA(lt_count_table) INDEX 5.
    LOOP AT lt_count_table INTO DATA(ls_count_entry).
      CLEAR ls_count.
      LOOP AT ls_count_entry-columns INTO DATA(ls_count_column).
        CASE ls_count_column-col.
          WHEN 1. " Check group
            ls_count-check_group = ls_count_column-field.
          WHEN 2. " Check id
            ls_count-check_id    = ls_count_column-field.
          WHEN 3. " Count
            ls_count-count       = ls_count_column-field.
          WHEN 4. " Rating
            ls_count-rating      = ls_count_column-field.
        ENDCASE.
      ENDLOOP.
      INSERT ls_count INTO TABLE lt_count.
    ENDLOOP.

  ENDIF.
  " get meta data for ratings (column headings, not used yet)
  "READ TABLE lt_metadata_h INTO DATA(ls_metadata_h) INDEX rating_tabix.

  " get check table for ratings
  READ TABLE table_data INTO DATA(lt_rating_table) INDEX rating_tabix.

  LOOP AT lt_rating_table INTO DATA(ls_rating_entry).
    ls_outtab-row = ls_rating_entry-row.
    CLEAR:
      "ls_outtab-main_chapter, " Keep text from previous line
      "ls_outtab-chapter,      " Keep text from previous line
      ls_outtab-check,
      "ls_outtab-alert_id, " not used for SOS
      ls_outtab-sos_check_id,
      ls_outtab-rating,
      ls_outtab-rating_text,
      ls_outtab-count,
      ls_outtab-check_group,
      ls_outtab-check_id,
      ls_outtab-con,
      ls_outtab-ins.

    LOOP AT ls_rating_entry-columns INTO DATA(ls_rating_column).
      CASE ls_rating_column-col.
        WHEN 1. " Main chapter (in session language)
          ls_outtab-main_chapter = ls_rating_column-field.
        WHEN 2. " Chapter (in session language)
          ls_outtab-chapter      = ls_rating_column-field.
        WHEN 3. " Check title (in session language)
          ls_outtab-check        = ls_rating_column-field.
          " Get SOS Check ID by extracting the code in (...) from title
          " Some check titles contain text in (...) as well
          FIND ALL OCCURRENCES OF '(' IN ls_rating_column-field
            MATCH COUNT DATA(count).
          IF count = 1.
            " Normal title, only the SOS check ID is in (...)
            FIND REGEX '\(([^)]*)\)'     " find 1st group
              IN ls_rating_column-field
              SUBMATCHES ls_outtab-sos_check_id.
          ELSEIF count = 2.
            " Another part of the title shows text in (...)
            FIND REGEX '\).*\(([^)]*)\)' " find 2nd group
              IN ls_rating_column-field
              SUBMATCHES ls_outtab-sos_check_id.
          ENDIF.
        WHEN 4. " Rating icon
          ls_outtab-rating       = ls_rating_column-field.
          " Get rating text
          PERFORM get_rating_text
            USING
              ls_outtab-rating
            CHANGING
              ls_outtab-rating_text.
        WHEN 5. " Check group in DSA
          ls_outtab-check_group  = ls_rating_column-field.
        WHEN 6. " Check ID in DSA
          ls_outtab-check_id     = ls_rating_column-field.
        WHEN 7. " Context
          ls_outtab-con          = ls_rating_column-field.
        WHEN 8. " Instance
          ls_outtab-ins          = ls_rating_column-field.
      ENDCASE.
    ENDLOOP. " ls_rating_entry-columns

    " Filter
    CHECK ls_outtab-main_chapter IN main
      AND ls_outtab-chapter      IN chapter
      AND ls_outtab-check        IN chk
      "AND ls_outtab-alert_id     IN ewa_id " not used for SOS
      AND ls_outtab-sos_check_id IN sos_id
      AND ls_outtab-check_group  IN chkgrp
      "AND ls_outtab-check_id     IN chkid
      AND ls_outtab-rating_text  IN rating.

    READ TABLE lt_count INTO ls_count
      WITH KEY
        check_group = ls_outtab-check_group
        check_id    = ls_outtab-check_id.
    IF sy-subrc IS INITIAL.
      ls_outtab-count = ls_count-count.
    ENDIF.

    APPEND ls_outtab TO lt_outtab.
  ENDLOOP. " lt_rating_table
ENDFORM.

FORM get_rating_text
  USING
    rating      TYPE string
  CHANGING
    rating_text TYPE string.

  CASE rating.
    WHEN '@AG@'. rating_text = 'red'(R01).
    WHEN '@AH@'. rating_text = 'yellow'(R02).
    WHEN '@01@'. rating_text = 'green'(R03).
    WHEN '@0S@'. rating_text = 'information'(R04).
    WHEN '@BZ@'. rating_text = 'not rated'(R05).
    WHEN OTHERS.
      rating_text = 'unknown'(R06).
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*

* ALV

* main data table
DATA: gr_alv_table      TYPE REF TO cl_salv_table.

CLASS lcl_handle_events DEFINITION DEFERRED.
DATA: gr_events TYPE REF TO lcl_handle_events.
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_before_salv_function FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_after_salv_function FOR EVENT after_salv_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    "importing e_salv_function

*   Get selected item
    DATA(lr_selections)   = gr_alv_table->get_selections( ).
    DATA(ls_cell)         = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    CASE e_salv_function.

      WHEN 'PICK'. " Double click
        IF ls_cell-row > 0.
          READ TABLE lt_outtab INTO DATA(ls_outtab) INDEX ls_cell-row.
          IF sy-subrc = 0.
            "...

          ENDIF.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "on_user_command

  METHOD on_before_salv_function.
    "importing e_salv_function
  ENDMETHOD.                    "on_before_salv_function

  METHOD on_after_salv_function.
    "importing e_salv_function
  ENDMETHOD.                    "on_after_salv_function

  METHOD on_double_click.
    "importing row column

    " Get selected item
    DATA(lr_selections) = gr_alv_table->get_selections( ).
    DATA(ls_cell) = lr_selections->get_current_cell( ).

    IF row > 0.
      READ TABLE lt_outtab INTO DATA(ls_outtab) INDEX row.
      IF sy-subrc = 0.

        CASE column.

          WHEN 'SESSNO'.
            SUBMIT rdsvas_session_start
              VIA SELECTION-SCREEN AND RETURN
              WITH language = sy-langu
              WITH mode     = 'R'       " read
              WITH sessno   = ls_outtab-sessno
              WITH sesstype = c_sessitype
              .

        ENDCASE.

      ENDIF.
    ENDIF.
  ENDMETHOD.                    "on_double_click

  METHOD on_link_click.
    "importing row column
  ENDMETHOD.                    "on_single_click
ENDCLASS.

FORM show_alv.

*... create ALV
  TRY.
      cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false "  false: grid, true: list
        IMPORTING
          r_salv_table = gr_alv_table
        CHANGING
          t_table      = lt_outtab ).
    CATCH cx_salv_msg
          INTO DATA(lr_exception_salv_msg).
      DATA(lv_message) = lr_exception_salv_msg->get_message( ).
      MESSAGE ID lv_message-msgid TYPE lv_message-msgty
              NUMBER lv_message-msgno
              WITH lv_message-msgv1 lv_message-msgv2
                   lv_message-msgv3 lv_message-msgv4.
  ENDTRY.

*... activate generic ALV functions
  DATA(lr_functions) = gr_alv_table->get_functions( ).
  lr_functions->set_all( abap_true ).
  lr_functions->set_graphics( if_salv_c_bool_sap=>false ).

*... set display settings
  DATA(lr_display) = gr_alv_table->get_display_settings( ).
  TRY.
      lr_display->set_list_header( sy-title ).
      "lr_display->set_list_header_size( header_size ).
      lr_display->set_striped_pattern( abap_true ).
      lr_display->set_horizontal_lines( abap_true ).
      lr_display->set_vertical_lines( abap_true ).
      lr_display->set_suppress_empty_data( abap_true ).
    CATCH cx_salv_method_not_supported.
  ENDTRY.

*... set column appearance
  DATA(lr_columns) = gr_alv_table->get_columns( ).
  lr_columns->set_optimize( abap_true ). " Optimize column width

  TRY.
*... adjust columns
      DATA: lr_column     TYPE REF TO cl_salv_column_table.

      DATA: ls_color_key  TYPE lvc_s_colo.
      ls_color_key-col  = col_key.

      lr_column ?= lr_columns->get_column( 'INSTNO' ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'DBID' ).
      lr_column->set_short_text( 'System ID'(s09) ).
      lr_column->set_medium_text( 'System ID'(m09) ).
      lr_column->set_long_text( 'System ID'(l09) ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'SESSNO' ).
      lr_column->set_short_text( 'Session'(s10) ).
      lr_column->set_medium_text( 'Session number'(m10) ).
      lr_column->set_long_text( 'Session number'(l10) ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'CHANGE_DATE' ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'CREATION_DATE' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'ROW' ).
      lr_column->set_short_text( 'Row'(s11) ).
      lr_column->set_medium_text( 'Row'(m11) ).
      lr_column->set_long_text( 'Row'(l11) ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column ?= lr_columns->get_column( 'MAIN_CHAPTER' ).
      lr_column->set_short_text( 'Main Chap.'(s01) ).
      lr_column->set_medium_text( 'Main chapter'(m01) ).
      lr_column->set_long_text( 'Main chapter'(l01) ).

      lr_column ?= lr_columns->get_column( 'CHAPTER' ).
      lr_column->set_short_text( 'Chapter'(s02) ).
      lr_column->set_medium_text( 'Chapter'(m02) ).
      lr_column->set_long_text( 'Chapter'(l02) ).

      lr_column ?= lr_columns->get_column( 'CHECK' ).
      IF s_ewa = 'X'.
        lr_column->set_short_text( 'Alert'(s16) ).
        lr_column->set_medium_text( 'Alert'(m16) ).
        lr_column->set_long_text( 'Alert'(l16) ).
      ELSEIF s_sos = 'X'.
        lr_column->set_short_text( 'Check'(s03) ).
        lr_column->set_medium_text( 'Check'(m03) ).
        lr_column->set_long_text( 'Check'(l03) ).
      ENDIF.

      lr_column ?= lr_columns->get_column( 'ALERT_ID' ).
      lr_column->set_short_text( 'Alert ID'(s15) ).
      lr_column->set_medium_text( 'EWA Alert ID'(m15) ).
      lr_column->set_long_text( 'EWA Alert ID'(l15) ).
      IF s_ewa NE 'X'. " Only used for EWA
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      ENDIF.

      lr_column ?= lr_columns->get_column( 'SOS_CHECK_ID' ).
      lr_column->set_short_text( 'SOS Check'(s12) ).
      lr_column->set_medium_text( 'SOS Check ID'(m12) ).
      lr_column->set_long_text( 'SOS Check ID'(l12) ).
      IF s_sos NE 'X'. " Only used for SOS
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      ENDIF.

      lr_column ?= lr_columns->get_column( 'RATING' ).
      lr_column->set_short_text( 'Rating'(s04) ).
      lr_column->set_medium_text( 'Rating'(m04) ).
      lr_column->set_long_text( 'Rating'(l04) ).

      lr_column ?= lr_columns->get_column( 'RATING_TEXT' ).
      lr_column->set_short_text( 'RatingText'(s13) ).
      lr_column->set_medium_text( 'Rating text'(m13) ).
      lr_column->set_long_text( 'Rating text'(l13) ).

      lr_column ?= lr_columns->get_column( 'COUNT' ).
      lr_column->set_short_text( 'Count'(s14) ).
      lr_column->set_medium_text( 'Count'(m14) ).
      lr_column->set_long_text( 'Count'(l14) ).
      lr_column->set_zero( if_salv_c_bool_sap=>false ).
      IF s_sos NE 'X'. " Only used for SOS
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      ENDIF.

      lr_column ?= lr_columns->get_column( 'CHECK_GROUP' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column ?= lr_columns->get_column( 'CHECK_ID' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column ?= lr_columns->get_column( 'CON' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column ?= lr_columns->get_column( 'INS' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found
          INTO DATA(lr_exception_salv_not_found).
      lv_message = lr_exception_salv_not_found->get_message( ).
      MESSAGE ID lv_message-msgid TYPE lv_message-msgty
              NUMBER lv_message-msgno
              WITH lv_message-msgv1 lv_message-msgv2
                   lv_message-msgv3 lv_message-msgv4.
  ENDTRY.

*... register to the events of cl_salv_table
  DATA(lr_events) = gr_alv_table->get_event( ).
  CREATE OBJECT gr_events.
  SET HANDLER gr_events->on_user_command         FOR lr_events.
  SET HANDLER gr_events->on_before_salv_function FOR lr_events.
  SET HANDLER gr_events->on_after_salv_function  FOR lr_events.
  SET HANDLER gr_events->on_double_click         FOR lr_events.
  SET HANDLER gr_events->on_link_click           FOR lr_events.

*... choose selction method
  DATA(lr_selections) = gr_alv_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

*... allow to save layout
  DATA(lr_layout) = gr_alv_table->get_layout( ).
  DATA ls_layout_key TYPE salv_s_layout_key.
  ls_layout_key-report = sy-repid.
  lr_layout->set_key( ls_layout_key ).
  lr_layout->set_initial_layout( p_layout ).
  AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
                      ID 'ACTVT' FIELD '23'.
  IF sy-subrc = 0.
    lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
  ELSE.
    lr_layout->set_save_restriction( cl_salv_layout=>restrict_user_dependant ).
  ENDIF.

*... show it
  gr_alv_table->display( ).

ENDFORM.