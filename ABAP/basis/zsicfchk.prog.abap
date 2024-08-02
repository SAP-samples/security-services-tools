*&---------------------------------------------------------------------*
*& Report  ZSICFCHK
*&---------------------------------------------------------------------*
*& Show public ICF services and services with logon data
*& This is an extended version of standard report RSICFCHK
*& 23.09.2021 List changed to ALV
*&            Navigation to transaction SICF
*&            Table ICFSECPASSWD added
*&---------------------------------------------------------------------*

REPORT  zsicfchk LINE-SIZE 170.

CONSTANTS: c_program_version(30) TYPE c VALUE '23.09.2021'.

*TABLES: icfservice, icfactive, icfservloc, icfdocu, icfsecpasswd.

selection-screen begin of line.
DATA so_name TYPE icfname.
SELECTION-SCREEN COMMENT 1(30) ss_name FOR FIELD s_name.
SELECT-OPTIONS s_name FOR so_name.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
DATA so_path TYPE icfrequest_path.
SELECTION-SCREEN COMMENT 1(30) ss_path FOR FIELD s_path.
SELECT-OPTIONS s_path FOR so_path LOWER CASE.
SELECTION-SCREEN END OF LINE.

* Public Services
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_public AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(60) ss_pub FOR FIELD p_public.
SELECTION-SCREEN END OF LINE.

* Services mit Anmeldedaten
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_login  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(60) ss_login FOR FIELD p_login.
SELECTION-SCREEN END OF LINE.

* Auch inaktive Services zeigen
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_inact  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(60) ss_inact FOR FIELD p_inact.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

*----------------------------------------------------------------------

TYPES: BEGIN OF ts_icfservice,
         icf_name     LIKE icfservice-icf_name,     " Service name
         icfparguid   LIKE icfservice-icfparguid,   " Parent GUID (hidden)
         icfnodguid   LIKE icfservice-icfnodguid,   " Node GUID (hidden)
         icf_mandt    LIKE icfservice-icf_mandt,    " Client
         icf_user     LIKE icfservice-icf_user,     " User
         icf_passwd   LIKE icfservice-icf_passwd,   " Password
         oblusrflag   LIKE icfservice-oblusrflag,   " Anonymous ICF user required
         protsec      LIKE icfservice-protsec,      " Security requirement
         x509flag     LIKE icfservice-x509flag,     " X.509 Cesrtificate required
         sslflag      LIKE icfservice-sslflag,      " SSL/TLS required
         aliaslogin   LIKE icfservice-aliaslogin,   " Internet user for basic authentication
         icf_auth     LIKE icfservice-icf_auth,     " Service authorization
         orig_name    LIKE icfservice-orig_name,    " Service name
         icfactive    LIKE icfservloc-icfactive,    " Activated
         icfaliflag   TYPE string, "LIKE icfservice-icfaliflag, " Path type
         io_path_from TYPE string,                  " Path(additional field)
         icf_redir    LIKE icfservice-icf_redir,    " Redirected to
         icf_docu     TYPE icfdocu-icf_docu,        " Description
         cellcolor    TYPE lvc_t_scol,              " ALV field color (additional field)
       END OF ts_icfservice.

DATA:
  ls_icfservice        TYPE          ts_icfservice,
  it_icfservice_tmp    TYPE TABLE OF ts_icfservice,
  it_icfservice_tmp2   TYPE TABLE OF ts_icfservice,
  it_icfservice_public TYPE TABLE OF ts_icfservice.

* SICF Public Services
CONSTANTS:
  public_icf_name   TYPE icfservice-icf_name   VALUE 'PUBLIC',
  public_icfparguid TYPE icfservice-icfparguid VALUE 'DFFAEATGKMFLCDXQ04F0J7FXK', "/sap/
  public_icfnodguid TYPE icfservice-icfnodguid VALUE '0V000YHIHJTMAQZ31MI9AONBR', "/sap/public/
  rootguid          TYPE icfservice-icfnodguid VALUE 'FFFFFFFFFFFFFFFFFFFFFFFFF'.

INITIALIZATION.

  sy-title = 'SICF: Public Services and Services having logon credentials'.

  ss_name   = 'Service name'.
  ss_path   = 'Service path'.

  ss_pub    = 'Public services'.
  ss_login  = 'Services having logon credentials'.
  ss_inact  = 'also inactive services'.

  CONCATENATE 'Program version:'(VER) c_program_version INTO ss_vers
    SEPARATED BY space.

START-OF-SELECTION.

  PERFORM get_services.
  PERFORM show_services.

FORM get_services.
*----------------------------------------------------------------------*
* Public SICF Node
*----------------------------------------------------------------------*
  IF p_public = 'X'.
    SELECT icf_name
           icfparguid
           icfnodguid
           icfaliflag
           icf_mandt
           icf_user
           icf_passwd
           oblusrflag
           protsec
           x509flag
           sslflag
           aliaslogin
           icf_auth
           icf_redir
           orig_name
      FROM icfservice INTO CORRESPONDING FIELDS OF ls_icfservice
      WHERE icf_name   = public_icf_name
        AND icfparguid = public_icfparguid.

      CLEAR ls_icfservice-icfactive.
      SELECT SINGLE icfactive INTO ls_icfservice-icfactive
        FROM icfservloc
        WHERE icf_name   = ls_icfservice-icf_name
          AND icfparguid = ls_icfservice-icfparguid.
        IF ls_icfservice-icfactive = 'X' OR p_inact = 'X'.
          APPEND ls_icfservice TO it_icfservice_tmp.
        ENDIF.

      ENDSELECT.
    ENDIF.

*----------------------------------------------------------------------*
* SICF Services with logon data
*----------------------------------------------------------------------*
    IF p_login = 'X'.
      SELECT icf_name
             icfparguid
             icfnodguid
             icfaliflag
             icf_mandt
             icf_user
             icf_passwd
             oblusrflag
             protsec
             x509flag
             sslflag
             aliaslogin
             icf_auth
             icf_redir
             orig_name
        FROM icfservice INTO CORRESPONDING FIELDS OF ls_icfservice
        WHERE icf_user   NE space
          AND icf_passwd NE space.

        CLEAR ls_icfservice-icfactive.
        SELECT SINGLE icfactive INTO ls_icfservice-icfactive
          FROM icfservloc
          WHERE icf_name   = ls_icfservice-icf_name
            AND icfparguid = ls_icfservice-icfparguid.
          IF ls_icfservice-icfactive = 'X' OR p_inact = 'X'.
            APPEND ls_icfservice TO it_icfservice_tmp.
          ENDIF.

        ENDSELECT.

      SELECT s~icf_name
             s~icfparguid
             s~icfnodguid
             s~icfaliflag
             s~icf_mandt
             p~icf_user
             p~icf_passwd
             s~oblusrflag
             s~protsec
             s~x509flag
             s~sslflag
             s~aliaslogin
             s~icf_auth
             s~icf_redir
             s~orig_name
        INTO CORRESPONDING FIELDS OF ls_icfservice
        FROM icfservice   as s
        JOIN icfsecpasswd as p
          on    p~icfparguid = s~icfparguid
            AND p~icf_name   = s~icf_name
        WHERE p~icf_user   NE space
          AND p~icf_passwd NE space
          .

        CLEAR ls_icfservice-icfactive.
        SELECT SINGLE icfactive INTO ls_icfservice-icfactive
          FROM icfservloc
          WHERE icf_name   = ls_icfservice-icf_name
            AND icfparguid = ls_icfservice-icfparguid.
          IF ls_icfservice-icfactive = 'X' OR p_inact = 'X'.
            APPEND ls_icfservice TO it_icfservice_tmp.
          ENDIF.

        ENDSELECT.

      ENDIF.

*----------------------------------------------------------------------*
* Iteration (max 10) to get services on deeper levels
*----------------------------------------------------------------------*
      DO 10 TIMES.
        IF it_icfservice_tmp[] IS INITIAL.
          EXIT.
        ELSE.
          APPEND LINES OF it_icfservice_tmp TO it_icfservice_public.
        ENDIF.
        CLEAR it_icfservice_tmp2[].
        SELECT icf_name
               icfparguid
               icfnodguid
               icfaliflag
               icf_mandt
               icf_user
               icf_passwd
               oblusrflag
               protsec
               x509flag
               sslflag
               aliaslogin
               icf_auth
               icf_redir
               orig_name
       FROM icfservice INTO CORRESPONDING FIELDS OF ls_icfservice
          FOR ALL ENTRIES IN it_icfservice_tmp
          WHERE icfparguid =  it_icfservice_tmp-icfnodguid.

          CLEAR ls_icfservice-icfactive.
          SELECT SINGLE icfactive INTO ls_icfservice-icfactive
            FROM icfservloc
            WHERE icf_name   = ls_icfservice-icf_name
              AND icfparguid = ls_icfservice-icfparguid.
            IF ls_icfservice-icfactive = 'X' OR p_inact = 'X'.
              APPEND ls_icfservice TO it_icfservice_tmp2.
            ENDIF.

          ENDSELECT.
          it_icfservice_tmp[] = it_icfservice_tmp2[].
        ENDDO.

*----------------------------------------------------------------------*
* Get more data
*----------------------------------------------------------------------*

* Get path
* Variables for FORM construct_path(RSICFTREE)
        TYPES: BEGIN OF icf_outtab.
                 INCLUDE STRUCTURE icfservice AS icfs.
                 TYPES:    icf_docu  TYPE char210,
                 icfactive TYPE icfactive,
                 icfsrvgrp TYPE icfsrvgrp,
*          extflag1 TYPE c,
*          extflag2 TYPE c,
               END OF icf_outtab.

***** Programminterne Tabelle: Knoten + Daten
        TYPES: BEGIN OF icf_treetab.
                 INCLUDE TYPE icf_outtab AS icfout.
                 TYPES:    node      TYPE lvc_nkey,
                 sort_name TYPE icfaltnme,
               END OF icf_treetab.
        DATA: gl_selected_icf TYPE icf_treetab,
              error_flag      TYPE c.

        LOOP AT it_icfservice_public INTO ls_icfservice.

          " Get path

          CLEAR gl_selected_icf.
          gl_selected_icf-icf_name   = ls_icfservice-icf_name.
          gl_selected_icf-icfparguid = ls_icfservice-icfparguid.
          gl_selected_icf-icfnodguid = ls_icfservice-icfnodguid.
          PERFORM construct_path(rsicftree)
            USING gl_selected_icf
            CHANGING ls_icfservice-io_path_from error_flag.
          IF error_flag = 'X'.
            CLEAR ls_icfservice-io_path_from.
          ENDIF.
          CONCATENATE ls_icfservice-io_path_from
                      ls_icfservice-orig_name
            INTO ls_icfservice-io_path_from.

          " Check select options
          IF   ls_icfservice-icf_name     NOT IN s_name
            OR ls_icfservice-io_path_from NOT IN s_path.

            DELETE it_icfservice_public.
            CONTINUE.
          ENDIF.

          " Describe type of path

          IF ls_icfservice-icfaliflag = 'X' OR
             ls_icfservice-icfaliflag = 'V'.
            ls_icfservice-icfaliflag = 'Alias Target'.
          ELSEIF ls_icfservice-icfparguid = rootguid.
            ls_icfservice-icfaliflag = 'Default Service'.
          ELSE.
            " empty
          ENDIF.

          " Get short text

          SELECT SINGLE icf_docu INTO ls_icfservice-icf_docu
            FROM icfdocu
            WHERE icf_name   = ls_icfservice-icf_name   AND
                  icfparguid = ls_icfservice-icfparguid AND
                  icf_langu  = sy-langu.

            " Replace password hash value

            IF ls_icfservice-icf_passwd IS NOT INITIAL.
              "ls_icfservice-icf_passwd = '***'.

              APPEND VALUE lvc_s_scol(
                  fname       = 'ICF_PASSWD'
                  color-col   = 6         " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange
                  color-int   = '1'       " 1 = Intensified on, 0 = Intensified off
                  color-inv   = '0'       " 1 = text colour, 0 = background colour
                ) TO ls_icfservice-cellcolor.
            ENDIF.

            " Identify public services

            IF ls_icfservice-icf_user IS INITIAL.
              ls_icfservice-icf_user = '<public>'.

              APPEND VALUE lvc_s_scol(
                  fname       = 'ICF_USER'
                  color-col   = 6         " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange
                  color-int   = '1'       " 1 = Intensified on, 0 = Intensified off
                  color-inv   = '0'       " 1 = text colour, 0 = background colour
                ) TO ls_icfservice-cellcolor.
            ENDIF.

            " Store it
            MODIFY it_icfservice_public FROM ls_icfservice.
            CLEAR ls_icfservice.
          ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
* Show result
*----------------------------------------------------------------------*
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
        READ TABLE it_icfservice_public INTO DATA(ls_outtab) INDEX ls_cell-row.
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
    READ TABLE it_icfservice_public INTO DATA(ls_outtab) INDEX row.
    IF sy-subrc = 0.

      CASE column.

        WHEN 'ICF_NAME' OR 'ICFNODGUID' OR 'ORIG_NAME'.
          SUBMIT rsicftree
            "VIA SELECTION-SCREEN
            AND RETURN
            WITH icf_serv   = ls_outtab-icf_name
            .

      ENDCASE.

    ENDIF.
  ENDIF.
ENDMETHOD.                    "on_double_click

METHOD on_link_click.
  "importing row column
ENDMETHOD.                    "on_single_click
ENDCLASS.

FORM show_services.

  "type-pools: slis. "ALV Declarations

  DATA:
    "gr_alv_table      TYPE REF TO cl_salv_table,               " Main ALV class
    lr_functions  TYPE REF TO cl_salv_functions_list,      " Generic and Application-Specific Functions
    lr_display    TYPE REF TO cl_salv_display_settings,    " Appearance of the ALV Output
    lr_functional TYPE REF TO cl_salv_functional_settings,
    lr_sorts      TYPE REF TO cl_salv_sorts,               " All Sort Objects
    "lr_aggregations      type ref to cl_salv_aggregations,
    "lr_filters           type ref to cl_salv_filters,
    "lr_print             type ref to cl_salv_print,
    lr_columns    TYPE REF TO cl_salv_columns_table,       " All Column Objects
    lr_column     TYPE REF TO cl_salv_column_table,        " Columns in Simple, Two-Dimensional Tables
    lr_layout     TYPE REF TO cl_salv_layout.              " Settings for Layout

  "data:
  "  header              type lvc_title,
  "  header_size         type salv_de_header_size,
  "  f2code              type syucomm,
  "  buffer              type salv_de_buffer,

  TRY.
      cl_salv_table=>factory(
        EXPORTING
            list_display = abap_false "  false: grid, true: list
        IMPORTING
          r_salv_table = gr_alv_table
        CHANGING
          t_table      = it_icfservice_public
      ).

    CATCH cx_salv_msg.
      MESSAGE i001(00) WITH 'ALV Error at factory'.
  ENDTRY.

*... activate ALV generic Functions
  lr_functions = gr_alv_table->get_functions( ).
  lr_functions->set_all( abap_true ).
  lr_functions->set_graphics( if_salv_c_bool_sap=>false ).

*... set the display settings
  lr_display = gr_alv_table->get_display_settings( ).
  lr_display->set_list_header( sy-title ).
  "lr_display->set_list_header_size( header_size ).
  lr_display->set_striped_pattern( abap_true ).
  lr_display->set_horizontal_lines( abap_true ).
  lr_display->set_vertical_lines( abap_true ).

*... set the display settings for lists and hier seq. lists
  TRY.
      lr_display->set_suppress_empty_data( abap_true ).

    CATCH cx_salv_method_not_supported.
      "message i001(00) with 'ALV Error at suppress_empty_data'.
  ENDTRY.

*... set the functional settings
  lr_functional = gr_alv_table->get_functional_settings( ).
  TRY.
      lr_functional->set_sort_on_header_click( abap_true ).
      "lr_functional->set_f2_code( f2code ).
      "lr_functional->set_buffer( gs_test-settings-functional-buffer ).
    CATCH cx_salv_method_not_supported.
      MESSAGE i001(00) WITH 'ALV Error at functional settings'.
  ENDTRY.

*... sort
  TRY.
      lr_sorts = gr_alv_table->get_sorts( ).
      lr_sorts->add_sort( 'IO_PATH_FROM' ).

    CATCH cx_salv_data_error cx_salv_existing cx_salv_not_found.
      MESSAGE i001(00) WITH 'ALV Error at sort'.
  ENDTRY.

*... set column appearance
  lr_columns = gr_alv_table->get_columns( ).
  lr_columns->set_optimize( abap_true ). " Optimize column width

  TRY.

*... adjust headings

      lr_column ?= lr_columns->get_column( 'ICF_NAME' ).
      lr_column->set_key( abap_true ).

      lr_column ?= lr_columns->get_column( 'ICFALIFLAG' ).
      lr_column->set_long_text( 'Path type' ).
      lr_column->set_medium_text( 'Path type' ).
      lr_column->set_short_text( 'Path type' ).

      lr_column ?= lr_columns->get_column( 'ICF_REDIR' ).
      lr_column->set_long_text( 'Redirected to' ).
      lr_column->set_medium_text( 'Redirected to' ).
      lr_column->set_short_text( 'Redirected' ).

      lr_column ?= lr_columns->get_column( 'IO_PATH_FROM' ).
      lr_column->set_long_text( 'Path' ).
      lr_column->set_medium_text( 'Path' ).
      lr_column->set_short_text( 'Path.' ).

      lr_column ?= lr_columns->get_column( 'ICF_DOCU' ).
      lr_column->set_long_text( 'Short text' ).
      lr_column->set_medium_text( 'Short text' ).
      lr_column->set_short_text( 'Short text' ).

*... convert time stamps
*      lr_column->set_edit_mask( '==TSTMP' ).

*... color for column
      lr_column->set_color( VALUE lvc_s_colo(
          col   = 3         " 1=light blue, 2=grey, 3=yellow, 4=blue, 5=green, 6=red, 7=orange
          int   = '0'       " 1 = Intensified on, 0 = Intensified off
          inv   = '0'       " 1 = text colour, 0 = background colour
      ) ).

*... hide unimportant columns
      lr_column ?= lr_columns->get_column( 'ICFPARGUID' ).   lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'ICFNODGUID' ).   lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'ORIG_NAME' ).    lr_column->set_visible( abap_false ).

    CATCH cx_salv_not_found.
      MESSAGE i001(00) WITH 'ALV Error at columns'.
  ENDTRY.

*... set field colors
  TRY.
      lr_columns->set_color_column( 'CELLCOLOR' ).

    CATCH cx_salv_data_error.
      MESSAGE i001(00) WITH 'ALV Error at field colors'.
  ENDTRY.

*... register to the events of cl_salv_table
  DATA(lr_events) = gr_alv_table->get_event( ).
  CREATE OBJECT gr_events.
  SET HANDLER gr_events->on_user_command         FOR lr_events.
  SET HANDLER gr_events->on_before_salv_function FOR lr_events.
  SET HANDLER gr_events->on_after_salv_function  FOR lr_events.
  SET HANDLER gr_events->on_double_click         FOR lr_events.
  SET HANDLER gr_events->on_link_click           FOR lr_events.

*... choose selection method
  DATA(lr_selections) = gr_alv_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

*... Allow to save layout
  lr_layout = gr_alv_table->get_layout( ).
  lr_layout->set_key( VALUE salv_s_layout_key(
      report = sy-repid
    )
  ).
  lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

*... show it
  gr_alv_table->display( ).

ENDFORM.
