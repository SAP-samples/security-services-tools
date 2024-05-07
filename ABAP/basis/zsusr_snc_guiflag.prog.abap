*&---------------------------------------------------------------------*
*& Report ZSUSR_SNC_GUIFLAG
*& Set/unset the SNC GUIFLAG of users
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Source: https://github.com/SAP-samples/security-services-tools
*&
*& 14.09.2023 Initial version
*& 15.09.2023 Refactoring for using the ALV
*&---------------------------------------------------------------------*
REPORT zsusr_snc_guiflag.

CONSTANTS: c_program_version(30) TYPE c VALUE '15.09.2023 FBT'.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*

DATA _bname TYPE bapibname-bapibname.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_bname FOR FIELD s_bname.
SELECT-OPTIONS s_bname FOR _bname.
SELECTION-SCREEN END OF LINE.

DATA _class TYPE bapilogond-class.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_class FOR FIELD s_class.
SELECT-OPTIONS s_class FOR _class.
SELECTION-SCREEN END OF LINE.

DATA _ustyp TYPE bapilogond-ustyp.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_ustyp FOR FIELD s_ustyp.
SELECT-OPTIONS s_ustyp FOR _ustyp.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_pname AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(40) ss_pname FOR FIELD p_pname.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN COMMENT /1(80) ss_snc.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

*----------------------------------------------------------------------*

" Main result table
TYPES:
  BEGIN OF ts_result,
    bname      TYPE bapibname-bapibname,
    "firstname  TYPE bapiaddr3-firstname,
    "lastname   TYPE bapiaddr3-lastname,
    class      TYPE bapilogond-class,
    ustyp      TYPE bapilogond-ustyp,
    gltgv      TYPE bapilogond-gltgv,
    gltgb      TYPE bapilogond-gltgb,
    islocked   TYPE string, "bapislockd,
    no_user_pw TYPE bapislockd-no_user_pw,
    guiflag    TYPE bapisncu-guiflag,
    action     TYPE string,
    pname      TYPE bapisncu-pname,
    message    TYPE bapiret2-message,
  END OF ts_result,
  tt_result TYPE STANDARD TABLE OF ts_result.

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      initialization,
      start_of_selection.

  PRIVATE SECTION.

    CLASS-DATA:
      " main data table
      lt_result          TYPE tt_result.

    CLASS-DATA:
      " main ALV table
      lr_alv_table  TYPE REF TO cl_salv_table,
      " for handling the events on the main ALV table
      lr_alv_events TYPE REF TO lcl_report.

    CLASS-METHODS:

      show_result,

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

*      on_single_click for event link_click of cl_salv_events_table
*        importing row column,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.                    "lcl_report DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    CONCATENATE 'Program version from'(100) c_program_version INTO ss_vers
      SEPARATED BY space.

    ss_bname  = 'User'.
    ss_class  = 'User group'.
    ss_ustyp  = 'User type'.
    ss_pname  = 'Only users with SNC name'.

    DATA: snc_active TYPE c.
    CALL FUNCTION 'SNC_CHECK_ACTIVE'
      IMPORTING
        active = snc_active.
    IF snc_active = abap_true.
      CALL METHOD cl_suid_tools=>get_snc_insecure_information
        IMPORTING
          ev_snc_info      = DATA(snc_accept_snc_logon)
          ev_snc_info_text = DATA(snc_info_text).
      CONCATENATE snc_info_text '( snc/accept_insecure_gui =' snc_accept_snc_logon ')'
        into ss_snc SEPARATED BY space.
    ELSE.
      ss_snc = 'SNC is not active'.
    ENDIF.

  ENDMETHOD. " initialization


  METHOD start_of_selection.
    " The BAPI functions perform required authority checks

    DATA: snc_active TYPE c.
    CALL FUNCTION 'SNC_CHECK_ACTIVE'
      IMPORTING
        active = snc_active.
    IF snc_active NE abap_true.
      MESSAGE e721(00). "SNC deactivated
    ENDIF.

    DATA:
      ls_selection_range TYPE          bapiussrge,
      lt_selection_range TYPE TABLE OF bapiussrge.

    ls_selection_range-parameter = 'USERNAME'.
    LOOP AT s_bname INTO s_bname.
      MOVE-CORRESPONDING s_bname TO ls_selection_range.
      APPEND ls_selection_range TO lt_selection_range.
    ENDLOOP.

    ls_selection_range-parameter = 'LOGONDATA'.
    ls_selection_range-field     = 'CLASS'.
    LOOP AT s_class INTO s_class.
      MOVE-CORRESPONDING s_class TO ls_selection_range.
      APPEND ls_selection_range TO lt_selection_range.
    ENDLOOP.

    ls_selection_range-parameter = 'LOGONDATA'.
    ls_selection_range-field     = 'USTYP'.
    LOOP AT s_ustyp INTO s_ustyp.
      MOVE-CORRESPONDING s_ustyp TO ls_selection_range.
      APPEND ls_selection_range TO lt_selection_range.
    ENDLOOP.

    DATA:
      ls_userlist TYPE          bapiusname,
      lt_userlist TYPE TABLE OF bapiusname,
      ls_return   TYPE          bapiret2,
      lt_return   TYPE TABLE OF bapiret2.
    CALL FUNCTION 'BAPI_USER_GETLIST'
*     EXPORTING
*       MAX_ROWS              = 0
*       WITH_USERNAME         = ' '
*     IMPORTING
*       ROWS                  =
      TABLES
        selection_range = lt_selection_range
*       SELECTION_EXP   =
        userlist        = lt_userlist
        return          = lt_return.

    " Show messages
    LOOP AT lt_return INTO ls_return.
      WRITE: / ls_return-type,
               ls_return-message.
      IF ls_return-type = 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Process data.
    DATA ls_result TYPE ts_result.
    LOOP AT lt_userlist INTO ls_userlist.
      CLEAR: ls_result, ls_return, lt_return.

      "CLEAR user_snc.
      "CALL FUNCTION 'SNC_USER_GET_DATA'
      "  EXPORTING
      "    user_name = ls_userlist-username
      "  IMPORTING
      "    user_snc  = user_snc
*     "    KNAME_EXISTS  =
      "  .
      DATA logondata      TYPE bapilogond.
      "DATA address        TYPE bapiaddr3.
      DATA user_snc       TYPE bapisncu.
      DATA islocked       TYPE bapislockd.
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username  = ls_userlist-username
*         CACHE_RESULTS        = 'X'
        IMPORTING
          logondata = logondata
          "address   = address
          snc       = user_snc
          islocked  = islocked
        TABLES
          return    = lt_return.

      CHECK p_pname IS INITIAL OR user_snc-pname IS NOT INITIAL.

      ls_result-bname     = ls_userlist-username.
      "ls_result-firstname = address-firstname.
      "ls_result-lastname  = address-lastname.
      ls_result-class     = logondata-class.
      ls_result-ustyp     = logondata-ustyp.
      ls_result-gltgv     = logondata-gltgv.
      ls_result-gltgb     = logondata-gltgb.
      IF   islocked-wrng_logon = 'L'
        OR islocked-local_lock = 'L'
        OR islocked-glob_lock  = 'L'.
        ls_result-islocked = icon_locked. "'X'.
      ENDIF.
      IF islocked-no_user_pw = 'L'.
        ls_result-no_user_pw = 'X'.
      ENDIF.
      ls_result-guiflag    = user_snc-guiflag.
      ls_result-pname      = user_snc-pname.

      IF user_snc-pname IS INITIAL.
        ls_result-message = 'No SNC name'.
      ENDIF.

      APPEND ls_result TO lt_result.

    ENDLOOP.

    show_result( ).

  ENDMETHOD. " start_of_selection

  METHOD on_user_command.
*    importing e_salv_function

    " Get selected item(s)
    DATA(lr_selections)   = lr_alv_table->get_selections( ).
    DATA(ls_cell)         = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    CASE e_salv_function.

      WHEN 'PICK'. " Double click

        IF ls_cell-row > 0.
          READ TABLE lt_result INTO DATA(ls_result) INDEX ls_cell-row.
          CHECK sy-subrc = 0.

        ENDIF.

      WHEN 'SET' OR 'UNSET'.

        " do it !
        DATA:
          user_snc  TYPE bapisncu,
          sncx      TYPE bapisncux,
          ls_return TYPE          bapiret2,
          lt_return TYPE TABLE OF bapiret2.
        FIELD-SYMBOLS <fs_result> TYPE ts_result.
        LOOP AT lt_seleced_rows INTO DATA(l_row).
          READ TABLE lt_result ASSIGNING <fs_result> INDEX l_row.
          CHECK sy-subrc = 0 AND <fs_result>-bname IS NOT INITIAL AND <fs_result>-pname IS NOT INITIAL.
          CLEAR: <fs_result>-action, <fs_result>-message.

          IF     e_salv_function = 'UNSET' AND <fs_result>-guiflag = 'X'.
            <fs_result>-action = 'unset'.
            user_snc-guiflag  = abap_false.
            sncx-guiflag      = abap_true.

          ELSEIF e_salv_function = 'SET' AND <fs_result>-guiflag = ' '.
            <fs_result>-action = 'set'.
            user_snc-guiflag  = abap_true.
            sncx-guiflag      = abap_true.

          ENDIF.

          IF <fs_result>-action IS NOT INITIAL.
            CALL FUNCTION 'BAPI_USER_CHANGE'
              EXPORTING
                username = <fs_result>-bname
                snc      = user_snc
                sncx     = sncx
              TABLES
                return   = lt_return.
            LOOP AT lt_return INTO ls_return.
              IF ls_return-type = 'E'.
                <fs_result>-action = 'error'.
                CONTINUE. " stop at first error
              ENDIF.
            ENDLOOP.

            <fs_result>-guiflag = user_snc-guiflag.
            <fs_result>-message = ls_return-message.
          ENDIF.

        ENDLOOP.
        " Update ALV list
        IF sy-subrc = 0.
          lr_alv_table->refresh( refresh_mode = if_salv_c_refresh=>soft ).
        ENDIF.

    ENDCASE.

  ENDMETHOD. " on_user_command

  METHOD on_double_click.
*   importing row column

    " Get selected item(s)
    DATA(lr_selections) = lr_alv_table->get_selections( ).
    DATA(ls_cell) = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).
    FIELD-SYMBOLS <fs_result> TYPE ts_result.

    IF row > 0.
      READ TABLE lt_result ASSIGNING <fs_result> INDEX row.
      CHECK sy-subrc = 0 AND <fs_result>-bname IS NOT INITIAL.

      CASE column.

        WHEN 'BNAME'.
          CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
            EXPORTING
              tcode  = 'SU01'
            EXCEPTIONS
              ok     = 0
              not_ok = 1
              OTHERS = 2.
          IF sy-subrc = 0.
            " Does not work anymore because of the list trick which enables additional ALV functions.
            "CALL FUNCTION 'SUID_IDENTITY_MAINT'
            "  EXPORTING
            "    i_username = <fs_result>-bname.
            SET PARAMETER ID 'XUS' FIELD <fs_result>-bname.
            CALL TRANSACTION 'SU01' WITH AUTHORITY-CHECK.
          ENDIF.

        WHEN 'GUIFLAG'.
          CHECK <fs_result>-pname IS NOT INITIAL.

          DATA:
            user_snc  TYPE bapisncu,
            sncx      TYPE bapisncux,
            ls_return TYPE          bapiret2,
            lt_return TYPE TABLE OF bapiret2.

          CLEAR: <fs_result>-action, <fs_result>-message.

          IF  <fs_result>-guiflag = 'X'.
            <fs_result>-action = 'unset'.
            user_snc-guiflag  = abap_false.
            sncx-guiflag      = abap_true.

          ELSEIF <fs_result>-guiflag = ' '.
            <fs_result>-action = 'set'.
            user_snc-guiflag  = abap_true.
            sncx-guiflag      = abap_true.

          ENDIF.

          IF <fs_result>-action IS NOT INITIAL.
            CALL FUNCTION 'BAPI_USER_CHANGE'
              EXPORTING
                username = <fs_result>-bname
                snc      = user_snc
                sncx     = sncx
              TABLES
                return   = lt_return.
            LOOP AT lt_return INTO ls_return.
              IF ls_return-type = 'E'.
                <fs_result>-action = 'error'.
                CONTINUE. " stop at first error
              ENDIF.
            ENDLOOP.

            <fs_result>-guiflag = user_snc-guiflag.
            <fs_result>-message = ls_return-message.

            " Update ALV list
            lr_alv_table->refresh( refresh_mode = if_salv_c_refresh=>soft ).
          ENDIF.

      ENDCASE.

    ENDIF.

  ENDMETHOD. " on_double_click

  METHOD show_result.
    DATA:
      lr_functions_list TYPE REF TO cl_salv_functions_list,
      lr_functions      TYPE REF TO cl_salv_functions_list,      " Generic and Application-Specific Functions
      lr_display        TYPE REF TO cl_salv_display_settings,    " Appearance of the ALV Output
      lr_functional     TYPE REF TO cl_salv_functional_settings,
      lr_sorts          TYPE REF TO cl_salv_sorts,        " All Sort Objects
      "lr_aggregations      type ref to cl_salv_aggregations,
      "lr_filters           type ref to cl_salv_filters,
      "lr_print             type ref to cl_salv_print,
      lr_selections     TYPE REF TO cl_salv_selections,
      lr_events         TYPE REF TO cl_salv_events_table,
      "lr_hyperlinks TYPE REF TO cl_salv_hyperlinks,
      "lr_tooltips   TYPE REF TO cl_salv_tooltips,
      "lr_grid_header TYPE REF TO cl_salv_form_layout_grid,
      "lr_grid_footer TYPE REF TO cl_salv_form_layout_grid,
      "lr_content     TYPE REF TO cl_salv_form_element,
      lr_display_settings  TYPE REF TO cl_salv_display_settings,
      lr_columns        TYPE REF TO cl_salv_columns_table,       " All Column Objects
      lr_column         TYPE REF TO cl_salv_column_table,        " Columns in Simple, Two-Dimensional Tables
      lr_layout         TYPE REF TO cl_salv_layout,               " Settings for Layout
      ls_layout_key     TYPE salv_s_layout_key.

    "data:
    "  header              type lvc_title,
    "  header_size         type salv_de_header_size,
    "  f2code              type syucomm,
    "  buffer              type salv_de_buffer,

    DATA: lr_exception TYPE REF TO cx_salv_error,
          lv_message   TYPE bal_s_msg.

*   Suppress toolbar of list output
    cl_abap_list_layout=>suppress_toolbar( ).
*   Create an ALV table for grid display
    WRITE space. "trick to get the screen
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = cl_gui_container=>default_screen "screen0
            list_display = abap_false "  false: grid, true: list
          IMPORTING
            r_salv_table = lr_alv_table
          CHANGING
            t_table      = lt_result ).
      CATCH cx_salv_msg
            INTO lr_exception.
        lv_message = lr_exception->get_message( ).
        "MESSAGE ID lv_message-msgid TYPE lv_message-msgty
        "        NUMBER lv_message-msgno
        "        WITH lv_message-msgv1 lv_message-msgv2
        "             lv_message-msgv3 lv_message-msgv4.
    ENDTRY.

*... activate ALV generic Functions
    lr_functions = lr_alv_table->get_functions( ).
    lr_functions->set_all( abap_true ).

*... set the display settings
    lr_display = lr_alv_table->get_display_settings( ).
    TRY.
        lr_display->set_list_header( sy-title ).
        "lr_display->set_list_header( header ).
        "lr_display->set_list_header_size( header_size ).
        lr_display->set_striped_pattern( abap_true ).
        lr_display->set_horizontal_lines( abap_true ).
        lr_display->set_vertical_lines( abap_true ).
        lr_display->set_suppress_empty_data( abap_true ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

    DATA: l_repid LIKE sy-repid.
    l_repid = sy-repid.
    CALL FUNCTION 'RS_CUA_STATUS_CHECK'
      EXPORTING
        objectname       = 'SALV_TABLE_STANDARD'
        program          = l_repid
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    IF sy-subrc = 0.
      lr_alv_table->set_screen_status(
        report        = l_repid
        pfstatus      = 'SALV_TABLE_STANDARD'
        set_functions = cl_salv_table=>c_functions_default
        ).

    ENDIF.

* set ALV generic funtions of class CL_SALV_FUNCTIONS_LIST
    lr_functions_list = lr_alv_table->get_functions( ).
*  lr_functions->SET_DEFAULT( if_salv_c_bool_sap=>true ).
    lr_functions_list->set_detail( if_salv_c_bool_sap=>true ).
    lr_functions_list->set_group_export( if_salv_c_bool_sap=>true ).
    lr_functions_list->set_group_filter( if_salv_c_bool_sap=>true ).
    lr_functions_list->set_group_layout( if_salv_c_bool_sap=>true ).
    lr_functions_list->set_print( if_salv_c_bool_sap=>true ).
    lr_functions_list->set_print_preview( if_salv_c_bool_sap=>true ).
    lr_functions_list->set_group_sort( if_salv_c_bool_sap=>true ).
    lr_functions_list->set_find( if_salv_c_bool_sap=>true ).
    lr_functions_list->set_graphics( if_salv_c_bool_sap=>false ).

* Add custom functions (require a gui container)

    DATA l_icon TYPE string.
    "IF 1 = 0.
    TRY.
        l_icon = icon_positive. " ICON_ACTIVATE ICON_SET_STATE ICON_CHECKBOX
        lr_functions_list->add_function(
          name     = 'SET'
          icon     = l_icon
          text     = 'Set'
          tooltip  = 'Set GUIFLAG'
          position = if_salv_c_function_position=>right_of_salv_functions ).

        "      l_icon = icon_system_user_menu.
        "      lr_functions_list->add_function(
        "        name     = 'SU01'
        "        icon     = l_icon
        "        text     = 'Show user'
        "        tooltip  = 'Show user (current client only)'
        "        position = if_salv_c_function_position=>right_of_salv_functions ).

        l_icon = icon_negative. "ICON_DEACTIVATE ICON_STATUS_OVERVIEW ICON_DELETE ICON_SNC_INFO
        lr_functions_list->add_function(
          name     = 'UNSET'
          icon     = l_icon
          text     = 'Unset'
          tooltip  = 'Unset GUIFLAG'
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_existing cx_salv_wrong_call.
    ENDTRY.
    "ENDIF.

*... set the functional settings
    lr_functional = lr_alv_table->get_functional_settings( ).
    TRY.
        lr_functional->set_sort_on_header_click( abap_true ).
        "lr_functional->set_f2_code( f2code ).
        "lr_functional->set_buffer( gs_test-settings-functional-buffer ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

* ...Set the layout
    "lr_layout = lr_alv_table->get_layout( ).
    "ls_layout_key-report = sy-repid.
    "lr_layout->set_key( ls_layout_key ).
    "lr_layout->set_initial_layout( p_layout ).
    "AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
    "                    ID 'ACTVT' FIELD '23'.
    "IF sy-subrc = 0.
    "  lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ) . "no restictions
    "ELSE.
    "  lr_layout->set_save_restriction( cl_salv_layout=>restrict_user_dependant ) . "user dependend
    "ENDIF.

*... sort
    TRY.
        lr_sorts = lr_alv_table->get_sorts( ).
        lr_sorts->add_sort( 'BNAME' ).

      CATCH cx_salv_data_error cx_salv_existing cx_salv_not_found.
    ENDTRY.

*... set column appearance
    lr_columns = lr_alv_table->get_columns( ).
    lr_columns->set_optimize( abap_true ). " Optimize column width

*... set the color of cells
    TRY.
        lr_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

* register to the events of cl_salv_table
    lr_events = lr_alv_table->get_event( ).
    CREATE OBJECT lr_alv_events.
* register to the event USER_COMMAND
    SET HANDLER lr_alv_events->on_user_command FOR lr_events.
* register to the event DOUBLE_CLICK
    SET HANDLER lr_alv_events->on_double_click FOR lr_events.

* set selection mode
    lr_selections = lr_alv_table->get_selections( ).
    lr_selections->set_selection_mode(
    if_salv_c_selection_mode=>row_column ).

    DATA: ls_color_guiflag TYPE lvc_s_colo.
    ls_color_guiflag-col  = col_total.

    TRY.
*... convert time stamps
        "lr_column ?= lr_columns->get_column( 'STORE_LAST_UPLOAD' ).
        "lr_column->set_edit_mask( '==TSTMP' ).

*... adjust headings
        "DATA color TYPE lvc_s_colo.

        lr_column ?= lr_columns->get_column( 'BNAME' ).
        lr_column->set_key( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'ISLOCKED' ).
        lr_column->set_short_text( 'Locked' ).
        lr_column->set_medium_text( 'Locked' ).
        lr_column->set_long_text( 'Locked' ).

        lr_column ?= lr_columns->get_column( 'NO_USER_PW' ).
        lr_column->set_short_text( 'No pwd' ).
        lr_column->set_medium_text( 'No password' ).
        lr_column->set_long_text( 'No password' ).

        lr_column ?= lr_columns->get_column( 'GUIFLAG' ).
        "lr_column->set_color( ls_color_guiflag ).
        lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

        lr_column ?= lr_columns->get_column( 'ACTION' ).
        lr_column->set_short_text( 'Action' ).
        lr_column->set_medium_text( 'Action' ).
        lr_column->set_long_text( 'Action' ).

        lr_column ?= lr_columns->get_column( 'PNAME' ).

        lr_column ?= lr_columns->get_column( 'MESSAGE' ).
        lr_column->set_short_text( 'Message' ).
        lr_column->set_medium_text( 'Message' ).
        lr_column->set_long_text( 'Message' ).

      CATCH cx_salv_not_found
        INTO lr_exception.
        lv_message = lr_exception->get_message( ).
        MESSAGE ID lv_message-msgid TYPE lv_message-msgty
                NUMBER lv_message-msgno
                WITH lv_message-msgv1 lv_message-msgv2
                     lv_message-msgv3 lv_message-msgv4.
    ENDTRY.

* register to the events of cl_salv_table
    lr_events = lr_alv_table->get_event( ).
    CREATE OBJECT lr_alv_events.
* register to the event USER_COMMAND
    SET HANDLER lr_alv_events->on_user_command FOR lr_events.
* register to the event DOUBLE_CLICK
    SET HANDLER lr_alv_events->on_double_click FOR lr_events.

* set selection mode
    lr_selections = lr_alv_table->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Set Title
  lr_display_settings = lr_alv_table->get_display_settings( ).
  lr_display_settings->set_list_header( 'Select entries or click on checkbox' ).
  lr_display_settings->set_list_header_size( cl_salv_display_settings=>c_header_size_small ).
  lr_display_settings->set_no_merging( if_salv_c_bool_sap=>true ).

*... show it
    lr_alv_table->display( ).

  ENDMETHOD. " show_result

ENDCLASS.                    "lcl_report IMPLEMENTATION

*----------------------------------------------------------------------*
*      REPORT events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization( ).

START-OF-SELECTION.
  lcl_report=>start_of_selection( ).
