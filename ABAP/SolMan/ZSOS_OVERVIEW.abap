*&---------------------------------------------------------------------*
*& Report  ZSOS_OVERVIEW
*& Show SOS rating overview
*&---------------------------------------------------------------------*
*&
*&
*& 24.08.2021 Initial version
*& 25.08.2021 Show SOS Check ID
*& 30.08.2021 Show count
*&---------------------------------------------------------------------*
REPORT zsos_overview
  MESSAGE-ID dsvas.

CONSTANTS: c_program_version(30) TYPE c VALUE '30.08.2021'.

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
    main_chapter  TYPE string,                  " column 1
    chapter       TYPE string,                  " column 2
    check         TYPE string,                  " column 3
    sos_check_id  TYPE string,
    rating        TYPE string,                  " column 4 Icon for rating
    rating_text   TYPE string,                  "          Text for rating
    count         TYPE string,
    check_group   TYPE dsvasdcheckgrp,          " column 5 Check group in DSA
    check_id      TYPE dsvasdcheckid,           " column 6 Check ID in DSA
    con           TYPE dsvasdcontext,           " column 7 Context
    ins           TYPE dsvasdcontextinstance,   " column 8 Instance
  END OF ts_outtab.
DATA:
  ls_outtab TYPE ts_outtab,
  lt_outtab TYPE TABLE OF ts_outtab.

*&---------------------------------------------------------------------*

* Selection screen
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
SELECTION-SCREEN COMMENT 1(28) ss_sosid FOR FIELD sos_id.
DATA s_sos_check_id(10).
SELECT-OPTIONS sos_id FOR s_sos_check_id.
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
PARAMETERS p_new AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(40) ss_new FOR FIELD p_new.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

*&---------------------------------------------------------------------*

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

  DATA(min_change_date) = sy-datum - 180. " Skip old sessions
  SELECT
      "h~sessitype
      h~sessno h~change_user h~change_date
      "a~bundle_id a~bundle_versnr
      a~dbid a~description
    FROM dsvassessionhead AS h
    JOIN dsvassessadmin   AS a
      ON  a~sessno = h~sessno
    JOIN dsvasresultsgen  AS r       " Only sessions having results
      ON  r~relid      = 'CT'        " Check tables
      AND r~sessitype  = 'GS'        " Session
      AND r~sessno     = h~sessno
      AND r~grp        = 'SC_FINISH' " Rating overview
      AND r~id         = '00013'     " Rating overview, get it either from check 00013 or from check 00011
      AND r~srtf2      = 0           " first entry
    INTO CORRESPONDING FIELDS OF value_tab
    WHERE a~change_date >= min_change_date
      AND a~bundle_id   =  'GSS_SEC'
      AND h~sessitype   =  'GS'
    ORDER BY a~change_date DESCENDING a~dbid ASCENDING.
    APPEND value_tab.
  ENDSELECT.
  "SORT value_tab BY change_date DESCENDING dbid ASCENDING.

  retfield = l_dynprofield.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SESSNO' "retfield "(i do not know why this works better)
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

INITIALIZATION.

  s_sess   = 'Select session'.
  ss_inst  = 'Installation number'.
  ss_dbid  = 'System ID'(m09).
  ss_udate = 'Change date'.
  ss_cdate = 'Create date'.
  ss_sess  = 'Session number'(m10).

  s_chap   = 'Filter chapter'.
  ss_main  = 'Main chapter'(m01).
  ss_chap  = 'Chapter'(m02).
  ss_chk   = 'Check'(m03).
  ss_sosid = 'SOS Check ID'(m12).
  ss_chkgp = 'Check group'.
  "ss_chkid = 'Check ID'.
  ss_rat   = 'Rating'(m04).

  ss_new   = 'Newest result per system only'.

  CONCATENATE 'Program version:'(VER) c_program_version INTO ss_vers
    SEPARATED BY space.

* Default values

  CASE sy-sysid.
    WHEN 'X3A'.
      sessno = VALUE #( low = '0010000002582' ).
      APPEND sessno.
  ENDCASE.
  GET PARAMETER ID:
      'DSVAS_SESSNO'    FIELD sessno.

*&---------------------------------------------------------------------*

START-OF-SELECTION.

* Authority check for Service Package, Session and Report
  AUTHORITY-CHECK OBJECT 'D_SVAS_SES'
    ID 'SES_GR_TYP' FIELD 'GSS Security Service/Session GS'
    ID 'PKG_ID_TYP' FIELD 'GSS_SEC/GS'
    ID 'ACTVT_SES'  FIELD 'R3'                              " Display Reports
    ID 'AUTHGROUP'  FIELD 'R'.                              " Restricted Authorization
  IF sy-subrc IS NOT INITIAL.
    "message e425(dsvas). " No authorization to display a session
    MESSAGE e430(dsvas). " No authorization to display a report
  ENDIF.

* Authorization Object for LMDB Object (needs repetition as soon as the system id is known)
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
  SELECT * FROM dsvassessadmin INTO TABLE @DATA(lt_dsvassessadmin)
    WHERE bundle_id     =  'GSS_SEC'
      AND sessno        IN @sessno
      AND instno        IN @instno
      AND dbid          IN @dbid
      AND change_date   IN @chgdate
      AND creation_date IN @credate
    ORDER BY instno, dbid, change_date DESCENDING, creation_date DESCENDING.

  " process sessions
  LOOP AT lt_dsvassessadmin INTO DATA(ls_dsvassessadmin).
    IF p_new = abap_true.
      CHECK ls_dsvassessadmin-instno      NE ls_outtab-instno
         OR ls_dsvassessadmin-dbid        NE ls_outtab-dbid
         OR ls_dsvassessadmin-change_date >  ls_outtab-change_date.
    ENDIF.

    CLEAR ls_outtab.
    ls_outtab-instno        = ls_dsvassessadmin-instno.
    ls_outtab-dbid          = ls_dsvassessadmin-dbid.
    ls_outtab-sessno        = ls_dsvassessadmin-sessno.
    ls_outtab-change_date   = ls_dsvassessadmin-change_date.
    ls_outtab-creation_date = ls_dsvassessadmin-creation_date.

    PERFORM get_resultsgen
      USING
        ls_dsvassessadmin-sessno.
  ENDLOOP.
ENDFORM.

FORM get_resultsgen
  USING
    l_sessno TYPE dsvasresultsgen-sessno.

  SELECT * FROM dsvasresultsgen INTO TABLE @DATA(lt_dsvasresultsgen)
    WHERE relid      = 'CT'        " Check tables
      AND sessitype  = 'GS'        " Session
      AND sessno     =  @l_sessno
      AND grp        = 'SC_FINISH' " Rating overview, get it either from check 00011 or check 00013
      "AND id         = '00011'     " SC_FINISH -> 00006 Summary -> 00011 Rating overview (1st check table of 1)
      AND id         = '00013'     " SC_FINISH -> 00013 Issues and Rating Overview (2nd check table of 5)
      AND srtf2      = 0           " first entry
    ORDER BY relid, sessitype, sessno, grp, id, con, ins, varkey, srtf2
    .

  LOOP AT lt_dsvasresultsgen INTO DATA(ls_dsvasresultsgen).
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

    " Both checks SC_FINISH 00011 and 00013 contain the rating table
    DATA rating_tabix TYPE i.
    IF     ls_dsvasresultsgen-id = '00011'. " SC_FINISH -> 00006 Summary -> 00011 Rating overview (rading data in 1st check table)

      rating_tabix = 1.

    ELSEIF ls_dsvasresultsgen-id = '00013'. " SC_FINISH -> 00013 Issues and Rating Overview (rating in 2nd check table)

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
    READ TABLE lt_metadata_h INTO DATA(ls_metadata_h) INDEX rating_tabix.
    " get check table for ratings
    READ TABLE table_data INTO DATA(lt_rating_table) INDEX rating_tabix.

    LOOP AT lt_rating_table INTO DATA(ls_rating_entry).
      ls_outtab-row = ls_rating_entry-row.
      CLEAR:
        "ls_outtab-main_chapter, " Keep text from previous line
        "ls_outtab-chapter,      " Keep text from previous line
        ls_outtab-check,
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
            CASE ls_outtab-rating.
              WHEN '@AG@'. ls_outtab-rating_text = 'red'.
              WHEN '@AH@'. ls_outtab-rating_text = 'yellow'.
              WHEN '@01@'. ls_outtab-rating_text = 'green'.
              WHEN '@BZ@'. ls_outtab-rating_text = 'not rated'.
              WHEN OTHERS. ls_outtab-rating_text = 'unknown'.
            ENDCASE.
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

  ENDIF.
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
              WITH sesstype = 'GS'
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
      lr_column->set_long_text( 'System ID'(l09) ).
      lr_column->set_medium_text( 'System ID'(m09) ).
      lr_column->set_short_text( 'System ID'(s09) ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'SESSNO' ).
      lr_column->set_long_text( 'Session number'(l10) ).
      lr_column->set_medium_text( 'Session number'(m10) ).
      lr_column->set_short_text( 'Session'(s10) ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'CHANGE_DATE' ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'CREATION_DATE' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'ROW' ).
      lr_column->set_long_text( 'Row'(l11) ).
      lr_column->set_medium_text( 'Row'(m11) ).
      lr_column->set_short_text( 'Row'(s11) ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column ?= lr_columns->get_column( 'MAIN_CHAPTER' ).
      lr_column->set_long_text( 'Main chapter'(l01) ).
      lr_column->set_medium_text( 'Main chapter'(m01) ).
      lr_column->set_short_text( 'Main Chap.'(s01) ).

      lr_column ?= lr_columns->get_column( 'CHAPTER' ).
      lr_column->set_long_text( 'Chapter'(l02) ).
      lr_column->set_medium_text( 'Chapter'(m02) ).
      lr_column->set_short_text( 'Chapter'(s03) ).

      lr_column ?= lr_columns->get_column( 'CHECK' ).
      lr_column->set_long_text( 'Check'(l03) ).
      lr_column->set_medium_text( 'Check'(m03) ).
      lr_column->set_short_text( 'Check'(s03) ).

      lr_column ?= lr_columns->get_column( 'SOS_CHECK_ID' ).
      lr_column->set_long_text( 'SOS Check ID'(l12) ).
      lr_column->set_medium_text( 'SOS Check ID'(m12) ).
      lr_column->set_short_text( 'SOS Check'(s12) ).

      lr_column ?= lr_columns->get_column( 'RATING' ).
      lr_column->set_long_text( 'Rating'(l04) ).
      lr_column->set_medium_text( 'Rating'(m04) ).
      lr_column->set_short_text( 'Rating'(s04) ).

      lr_column ?= lr_columns->get_column( 'RATING_TEXT' ).
      lr_column->set_long_text( 'Rating text'(l13) ).
      lr_column->set_medium_text( 'Rating text'(m13) ).
      lr_column->set_short_text( 'RatingText'(s13) ).

      lr_column ?= lr_columns->get_column( 'COUNT' ).
      lr_column->set_long_text( 'Count'(l14) ).
      lr_column->set_medium_text( 'Count'(m14) ).
      lr_column->set_short_text( 'Count'(s14) ).

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
