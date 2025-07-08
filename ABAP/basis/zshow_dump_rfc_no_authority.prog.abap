*&---------------------------------------------------------------------*
*& Report ZSHOW_DUMP_RFC_NO_AUTHORITY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshow_dump_rfc_no_authority.

CONSTANTS: c_program_version(30) TYPE c VALUE '08.07.2025 S44'.

*------------------------------------------------------------------------*
* Selection screen
*------------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) s_dfrom FOR FIELD datefrom.
  PARAMETERS datefrom TYPE sy-datum DEFAULT sy-datum.
  PARAMETERS timefrom TYPE sy-uzeit DEFAULT '000000'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) s_dto FOR FIELD dateto.
  PARAMETERS dateto   TYPE sy-datum DEFAULT sy-datum.
  PARAMETERS timeto   TYPE sy-uzeit DEFAULT '235959'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) s_mandt FOR FIELD mandt.
  SELECT-OPTIONS mandt FOR sy-mandt MODIF ID mda.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) s_uname FOR FIELD uname.
  SELECT-OPTIONS uname FOR sy-uname.
SELECTION-SCREEN END OF LINE.

* Runtime error id
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) s_errid FOR FIELD errid.
  PARAMETERS errid TYPE snapt-errid DEFAULT 'RFC_NO_AUTHORITY'.
SELECTION-SCREEN END OF LINE.

* Max count of entries
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) s_maxcnt FOR FIELD maxcnt.
  PARAMETERS maxcnt TYPE i DEFAULT 1000.
SELECTION-SCREEN END OF LINE.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) ps_lout FOR FIELD p_layout.
  PARAMETERS       p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

*---------------------------------------------------------------------*
*      Main result table
*---------------------------------------------------------------------*

" Interesting fields for runtime error RFC_NO_AUTHORITY (see Include MS380F10 Form DISPLAY_SNAP_DUMP)
TYPES: BEGIN OF ts_rfc_no_authority,
         " Key
         datum   TYPE snap-datum,     " Date
         uzeit   TYPE snap-uzeit,     " Time
         mandt   TYPE snap-mandt,     " Client
         uname   TYPE snap-uname,     " User Name
         ahost   TYPE snap-ahost,     " Application Server
         modno   TYPE snap-modno,     " Index of Work Process

         " Runtime error
         fc      TYPE snapt-errid,    " Runtime Error: RFC_NO_AUTHORITY
         cm      TYPE string,         " C Module: //bas/GIT/krn/rfc/abrfcfun.c#0
         cf      TYPE string,         " C Function: RabaxRfcNoAuthority
         re      TYPE string,         " Exception: RFC_ERROR_SYSTEM_FAILURE
         ct      TYPE string,         " (Wrong) Error description like: No RFC authorization for function module ...

         " Function
         r5      TYPE funcname,       " RFC: Function
         p3      TYPE string,         " Runtime Error Parameter: Function
         p2      TYPE string,         " Runtime Error Parameter: Function group
         ap      TYPE string,         " ABAP: Program

         " RFC Client / source system
         r1      TYPE sy-sysid,       " RFC: System
         r9      TYPE string,         " RFC: Installation Number
         s1      TYPE string,         " RFC: SAP Version
         s2      TYPE string,         " RFC: Kernel Version
         r7      TYPE string,         " RFC: IP Source Address
         r8      TYPE string,         " RFC: IP Destination
         r3      TYPE sy-mandt,       " RFC: Client
         r2      TYPE sy-uname,       " RFC: Caller User
         s8      TYPE progname,       " RFC: Calling Program
         r6      TYPE rfcdes-rfcdest, " RFC: Destination
         s3      TYPE string,         " RFC: Type
         s7      TYPE string,         " RFC: Asynchronous
         s5      TYPE string,         " RFC: Trusted Login
         s6      TYPE string,         " RFC: Login

         " RFC server / target system
         ei      TYPE string,         " Server IP Address
         p4      TYPE string,         " Runtime Error Parameter: Client
         p1      TYPE string,         " Runtime Error Parameter: User

         " Release and Kernel version
         er      TYPE string,         " Kernel Version

         " ALV
         t_color TYPE lvc_t_scol,
         "t_celltype                   type salv_t_int4_column,
         "T_HYPERLINK                  type SALV_T_INT4_COLUMN,
         "t_dropdown                   type salv_t_int4_column,

       END OF ts_rfc_no_authority,
       tt_rfc_no_authority TYPE STANDARD TABLE OF ts_rfc_no_authority.

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      initialization,

      selscreen_output,

      f4_p_layout
        CHANGING layout TYPE disvariant-variant,

      at_selscr_on_p_layout
        IMPORTING layout TYPE disvariant-variant,

      start_of_selection.

  PRIVATE SECTION.

    CLASS-DATA:
      " main data table
      lt_result          TYPE tt_rfc_no_authority.

    CLASS-DATA:
      " main ALV table
      lr_alv_table  TYPE REF TO cl_salv_table,
      " for handling the events on the main ALV table
      lr_alv_events TYPE REF TO lcl_report.

    CLASS-METHODS:

      authorization_check,

      select_snap_dumps,

      show_result,

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

*      on_single_click for event link_click of cl_salv_events_table
*        importing row column.
ENDCLASS.                    "lcl_report DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    s_dfrom  = 'From Date / Time'.
    s_dto    = 'To Date / Time'.
    s_mandt  = 'Client'.
    s_uname  = 'User'.
    s_errid  = 'Runtime error'.
    s_maxcnt = 'Max count of records'.
    ps_lout  = 'Layout'.

    CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
      SEPARATED BY space.

    authorization_check( ).

  ENDMETHOD.

  METHOD authorization_check.

    " new method does not work in all systems
    "* 2018 754: check against new auth. object S_ABAPDUMP
    "    IF cl_dump_authorization=>is_authoriz_displ_all_users( ) = abap_false.
    "      DATA(err_msg) = `No authority for cross-user ABAP Dump Analysis, see SAP Note `
    "        && cl_dump_authorization=>note_number.              "#EC NOTEXT
    "      MESSAGE err_msg TYPE 'E'.
    "    ENDIF.
    "    IF cl_dump_authorization=>is_authoriz_displ_all_clients( ) = abap_false.
    "      err_msg = `No authority for cross-client ABAP Dump Analysis, , see SAP Note `
    "          && cl_dump_authorization=>note_number.            "#EC NOTEXT
    "      MESSAGE err_msg TYPE 'E'.
    "    ENDIF.

    IF cl_st22_tools=>check_user_authorization_st22( ) <> 'X'.
      MESSAGE e172(00) WITH 'ST22'.
    ENDIF.

  ENDMETHOD.

  METHOD selscreen_output.

    " This report is specialized, therefore no change of runtime error id
    LOOP AT SCREEN.
      IF screen-name = 'ERRID'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD f4_p_layout.
    "CHANGING layout TYPE disvariant-variant.

    DATA ls_alv_variant TYPE disvariant.
    ls_alv_variant-report  = sy-repid.
    ls_alv_variant-variant = layout.

    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant    = ls_alv_variant
        i_save        = 'A'
      IMPORTING
        es_variant    = ls_alv_variant
      EXCEPTIONS
        not_found     = 1
        program_error = 2
        OTHERS        = 3.

    IF sy-subrc = 0.
      layout = ls_alv_variant-variant.
    ELSE.
      MESSAGE s073(0k). " Keine Anzeigevariante(n) vorhanden
    ENDIF.

  ENDMETHOD. " f4_p_layout

  METHOD at_selscr_on_p_layout.
    "IMPORTING layout TYPE disvariant-variant.

    DATA: ls_variant TYPE disvariant.
    ls_variant-report  = sy-repid.
    ls_variant-variant = layout.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save        = 'A'
      CHANGING
        cs_variant    = ls_variant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.

    IF sy-subrc <> 0.
      " Selected layout variant is not found
      MESSAGE e204(0k).
    ENDIF.

  ENDMETHOD. " at_selscr_on_p_layout

  METHOD start_of_selection.

    select_snap_dumps( ).

    show_result( ).

  ENDMETHOD.

  METHOD select_snap_dumps.

    DATA:
      ft                  TYPE rsdump_ft_it,
      cnt                 TYPE i,
      ls_rfc_no_authority TYPE ts_rfc_no_authority.

    IF datefrom IS INITIAL. datefrom = sy-datum. ENDIF.
    IF timefrom IS INITIAL. timefrom = '000000'. ENDIF.
    IF dateto   IS INITIAL. dateto   = sy-datum. ENDIF.
    IF timeto   IS INITIAL. timeto   = '235959'. ENDIF.

    DATA(flist_selection) = |FC{ CONV string( strlen( errid ) ) WIDTH = 3 ALPHA = IN }{ errid }%|. " Example: FC016RFC_NO_AUTHORITY%

    SELECT
        datum,
        uzeit,
        mandt,
        uname,
        ahost,
        modno
      FROM snap_beg "snap: Dumps, snap_beg: Keys only
        WHERE seqno = '000' " fist line only
          AND ( ( datum = @datefrom AND uzeit >= @timefrom ) OR datum > @datefrom )
          AND ( ( datum = @dateto   AND uzeit <= @timeto   ) OR datum < @dateto   )
          AND mandt IN @mandt
          AND uname IN @uname
          AND flist  LIKE @flist_selection " Fast search for FC016RFC_NO_AUTHORITY%
        ORDER BY
          datum DESCENDING,
          uzeit DESCENDING,
          ahost,
          uname,
          mandt,
          modno
        INTO TABLE @DATA(lt_snap)
        UP TO @maxcnt ROWS.
    .

    LOOP AT lt_snap INTO DATA(ls_snap).

      CALL FUNCTION 'RS_ST22_GET_FT'
        EXPORTING
          datum = ls_snap-datum
          uzeit = ls_snap-uzeit
          uname = ls_snap-uname
          ahost = ls_snap-ahost
          modno = ls_snap-modno
          mandt = ls_snap-mandt
        IMPORTING
          ft    = ft.

      " not needed, already selected
      "READ TABLE ft INTO DATA(ft_entry) WITH KEY id = 'FC'. "Runtime error, for example RFC_NO_AUTHORITY
      "CHECK sy-subrc = 0 AND ft_entry-value = 'RFC_NO_AUTHORITY'.

      " Limit output lines, not needed, already selected
      "add 1 to cnt.
      "IF cnt > maxcnt.
      "  EXIT.
      "ENDIF.

      CLEAR ls_rfc_no_authority.
      MOVE-CORRESPONDING ls_snap TO ls_rfc_no_authority.

      LOOP AT ft INTO DATA(ft_entry).
        CASE ft_entry-id.
          WHEN 'FC'. ls_rfc_no_authority-fc = ft_entry-value. " Runtime Errors
          WHEN 'AP'. ls_rfc_no_authority-ap = ft_entry-value. " ABAP: Program
          WHEN 'CM'. ls_rfc_no_authority-cm = ft_entry-value. " C Module
          WHEN 'CF'. ls_rfc_no_authority-cf = ft_entry-value. " C Function
          WHEN 'RE'. ls_rfc_no_authority-re = ft_entry-value. " Exception like RFC_ERROR_SYSTEM_FAILURE
          WHEN 'CT'. ls_rfc_no_authority-ct = ft_entry-value. " (Wrong) Error description

          WHEN 'R1'. ls_rfc_no_authority-r1 = ft_entry-value. " RFC: System
          WHEN 'R2'. ls_rfc_no_authority-r2 = ft_entry-value. " RFC: Caller
          WHEN 'R3'. ls_rfc_no_authority-r3 = ft_entry-value. " RFC: Client
          WHEN 'R5'. ls_rfc_no_authority-r5 = ft_entry-value. " RFC: Function
          WHEN 'R6'. ls_rfc_no_authority-r6 = ft_entry-value. " RFC: Destination
          WHEN 'R7'. ls_rfc_no_authority-r7 = ft_entry-value. " RFC: IP Source Address
          WHEN 'R8'. ls_rfc_no_authority-r8 = ft_entry-value. " RFC: IP Destination
          WHEN 'R9'. ls_rfc_no_authority-r9 = ft_entry-value. " RFC: Installation Number

          WHEN 'S1'. ls_rfc_no_authority-s1 = ft_entry-value. " RFC: SAP Version
          WHEN 'S2'. ls_rfc_no_authority-s2 = ft_entry-value. " RFC: Kernel Version
          WHEN 'S3'. ls_rfc_no_authority-s3 = ft_entry-value. " RFC: Type
          WHEN 'S5'. ls_rfc_no_authority-s5 = ft_entry-value. " RFC: Trusted Login
          WHEN 'S6'. ls_rfc_no_authority-s6 = ft_entry-value. " RFC: Login
          WHEN 'S7'. ls_rfc_no_authority-s7 = ft_entry-value. " RFC: Asynchronous
          WHEN 'S8'. ls_rfc_no_authority-s8 = ft_entry-value. " RFC: Calling Program

          WHEN 'P1'. ls_rfc_no_authority-p1 = ft_entry-value. " Runtime Error Parameter
          WHEN 'P2'. ls_rfc_no_authority-p2 = ft_entry-value. " Runtime Error Parameter
          WHEN 'P3'. ls_rfc_no_authority-p3 = ft_entry-value. " Runtime Error Parameter
          WHEN 'P4'. ls_rfc_no_authority-p4 = ft_entry-value. " Runtime Error Parameter

          WHEN 'ER'. ls_rfc_no_authority-er = ft_entry-value. " Kernel Version
          WHEN 'EI'. ls_rfc_no_authority-ei = ft_entry-value. " Server IP Address
        ENDCASE.
      ENDLOOP.
      APPEND ls_rfc_no_authority TO lt_result.
    ENDLOOP.

  ENDMETHOD.

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

          " Details
          CASE  ls_cell-columnname.
            WHEN 'UNAME'.

          ENDCASE.
        ENDIF.
    ENDCASE.

  ENDMETHOD. " on_user_command

  METHOD on_double_click.
*   importing row column

    " Get selected item(s)
    DATA(lr_selections) = lr_alv_table->get_selections( ).
    DATA(ls_cell) = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    IF row > 0.
      READ TABLE lt_result INTO DATA(ls_result) INDEX row.
      CHECK sy-subrc = 0.

      " Details
      CASE  column.

        WHEN 'DATUM' OR 'UZEIT'.
          " Ether call ST22 or report RSNAPREAD
          SUBMIT rsshowrabax AND RETURN
                  " Selection screen of ST22
                  WITH s_datum = ls_result-datum   " Date
                  WITH s_uzeit = ls_result-uzeit   " Time
                  WITH s_ahost = ls_result-ahost   " Host
                  WITH s_wpid  = ls_result-modno   " Work Process Index
                  WITH s_uname = ls_result-uname   " User
                  WITH s_mandt = ls_result-mandt   " Client
                  "WITH s_xhold ...                " To be stored
                  WITH s_errid = ls_result-fc      " Runtime error
                  "WITH s_prg ...                  " Canceled Program
                  "WITH s_except ...               " Exception
                  "WITH s_tid ...                  " Transaction ID
                  "WITH s_epprid ...
                  "WITH s_eppcid ...
                  "WITH s_eppcc ...
                  "WITH flag_exc ...               " With start text of Runtime Error
                  "WITH flag_com ...               " Corresponding Application Component
                  .

        WHEN 'UNAME'.
          CHECK ls_result-mandt = sy-mandt.
          " Show popup with user details
          "CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
          "  EXPORTING
          "    bname = ls_result-user.
          " Navigate to local SU01
          CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
            EXPORTING
              tcode  = 'SU01'
            EXCEPTIONS
              ok     = 0
              not_ok = 1
              OTHERS = 2.
          IF sy-subrc = 0.
            CALL FUNCTION 'SUID_IDENTITY_MAINT'
              EXPORTING
                i_username     = ls_result-uname
                i_tcode_mode   = 1 " 1 SU10, 6 SU01D
                i_su01_display = 'X'.
          ENDIF.

        WHEN 'P1'. " User
          CHECK ls_result-mandt = sy-mandt.
          " Show popup with user details
          "CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
          "  EXPORTING
          "    bname = ls_result-user.
          " Navigate to local SU01
          CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
            EXPORTING
              tcode  = 'SU01'
            EXCEPTIONS
              ok     = 0
              not_ok = 1
              OTHERS = 2.
          IF sy-subrc = 0.
            CALL FUNCTION 'SUID_IDENTITY_MAINT'
              EXPORTING
                i_username     = ls_result-p1
                i_tcode_mode   = 1 " 1 SU10, 6 SU01D
                i_su01_display = 'X'.
          ENDIF.

        WHEN 'R5'. " Function
          " Call ABAP Workbench (with all required authorization checks)
          CHECK ls_result-ap IS NOT INITIAL.
          "EDITOR-CALL FOR REPORT ls_result-ap DISPLAY-MODE.
          CALL FUNCTION 'RS_TOOL_ACCESS'
            EXPORTING
              operation           = 'SHOW'
              object_name         = ls_result-r5
              object_type         = 'FUNC'
*             ENCLOSING_OBJECT    =
*             POSITION            = ' '
*             DEVCLASS            =
*             INCLUDE             =
*             VERSION             = ' '
*             MONITOR_ACTIVATION  = 'X'
*             WB_MANAGER          =
*             IN_NEW_WINDOW       =
*             WITH_OBJECTLIST     = ' '
*             WITH_WORKLIST       = ' '
*           IMPORTING
*             NEW_NAME            =
*             WB_TODO_REQUEST     =
*           TABLES
*             OBJLIST             =
*           CHANGING
*             P_REQUEST           = ' '
            EXCEPTIONS
              not_executed        = 1
              invalid_object_type = 2
              OTHERS              = 3.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

        WHEN 'P3'. " Function
          " Call ABAP Workbench (with all required authorization checks)
          CHECK ls_result-ap IS NOT INITIAL.
          "EDITOR-CALL FOR REPORT ls_result-ap DISPLAY-MODE.
          CALL FUNCTION 'RS_TOOL_ACCESS'
            EXPORTING
              operation           = 'SHOW'
              object_name         = ls_result-p3
              object_type         = 'FUNC'
*             ENCLOSING_OBJECT    =
*             POSITION            = ' '
*             DEVCLASS            =
*             INCLUDE             =
*             VERSION             = ' '
*             MONITOR_ACTIVATION  = 'X'
*             WB_MANAGER          =
*             IN_NEW_WINDOW       =
*             WITH_OBJECTLIST     = ' '
*             WITH_WORKLIST       = ' '
*           IMPORTING
*             NEW_NAME            =
*             WB_TODO_REQUEST     =
*           TABLES
*             OBJLIST             =
*           CHANGING
*             P_REQUEST           = ' '
            EXCEPTIONS
              not_executed        = 1
              invalid_object_type = 2
              OTHERS              = 3.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

        WHEN 'AP'. " ABAP: Program
          " Call ABAP Workbench (with all required authorization checks)
          CHECK ls_result-ap IS NOT INITIAL.
          "EDITOR-CALL FOR REPORT ls_result-ap DISPLAY-MODE.
          CALL FUNCTION 'RS_TOOL_ACCESS'
            EXPORTING
              operation           = 'SHOW'
              object_name         = ls_result-ap
              object_type         = 'PROG'
*             ENCLOSING_OBJECT    =
*             POSITION            = ' '
*             DEVCLASS            =
*             INCLUDE             =
*             VERSION             = ' '
*             MONITOR_ACTIVATION  = 'X'
*             WB_MANAGER          =
*             IN_NEW_WINDOW       =
*             WITH_OBJECTLIST     = ' '
*             WITH_WORKLIST       = ' '
*           IMPORTING
*             NEW_NAME            =
*             WB_TODO_REQUEST     =
*           TABLES
*             OBJLIST             =
*           CHANGING
*             P_REQUEST           = ' '
            EXCEPTIONS
              not_executed        = 1
              invalid_object_type = 2
              OTHERS              = 3.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

        WHEN 'S8'. " RFC: Calling Program
          " Call ABAP Workbench (with all required authorization checks)
          CHECK ls_result-s8 IS NOT INITIAL.
          "EDITOR-CALL FOR REPORT ls_result-s8 DISPLAY-MODE.
          CALL FUNCTION 'RS_TOOL_ACCESS'
            EXPORTING
              operation           = 'SHOW'
              object_name         = ls_result-s8
              object_type         = 'PROG'
*             ENCLOSING_OBJECT    =
*             POSITION            = ' '
*             DEVCLASS            =
*             INCLUDE             =
*             VERSION             = ' '
*             MONITOR_ACTIVATION  = 'X'
*             WB_MANAGER          =
*             IN_NEW_WINDOW       =
*             WITH_OBJECTLIST     = ' '
*             WITH_WORKLIST       = ' '
*           IMPORTING
*             NEW_NAME            =
*             WB_TODO_REQUEST     =
*           TABLES
*             OBJLIST             =
*           CHANGING
*             P_REQUEST           = ' '
            EXCEPTIONS
              not_executed        = 1
              invalid_object_type = 2
              OTHERS              = 3.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.


      ENDCASE.
    ENDIF.

  ENDMETHOD. " on_double_click

  METHOD show_result.
    " Show ALV in demo mode
    "cl_demo_output=>display_data(
    "  value = lt_result
    "  name = 'Dumps about RFC_NO_AUTHORITY'
    ").

    DATA:
      lr_functions  TYPE REF TO cl_salv_functions_list,      " Generic and Application-Specific Functions
      lr_display    TYPE REF TO cl_salv_display_settings,    " Appearance of the ALV Output
      lr_functional TYPE REF TO cl_salv_functional_settings,
      lr_sorts      TYPE REF TO cl_salv_sorts,        " All Sort Objects
      "lr_aggregations      type ref to cl_salv_aggregations,
      "lr_filters           type ref to cl_salv_filters,
      "lr_print             type ref to cl_salv_print,
      lr_selections TYPE REF TO cl_salv_selections,
      lr_events     TYPE REF TO cl_salv_events_table,
      "lr_hyperlinks TYPE REF TO cl_salv_hyperlinks,
      "lr_tooltips   TYPE REF TO cl_salv_tooltips,
      "lr_grid_header TYPE REF TO cl_salv_form_layout_grid,
      "lr_grid_footer TYPE REF TO cl_salv_form_layout_grid,
      lr_columns    TYPE REF TO cl_salv_columns_table,       " All Column Objects
      lr_column     TYPE REF TO cl_salv_column_table,        " Columns in Simple, Two-Dimensional Tables
      lr_layout     TYPE REF TO cl_salv_layout,               " Settings for Layout
      ls_layout_key TYPE salv_s_layout_key.

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
            r_salv_table = lr_alv_table
          CHANGING
            t_table      = lt_result ).
      CATCH cx_salv_msg.
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
        lr_display->set_no_merging( abap_false ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

*... set the functional settings
    lr_functional = lr_alv_table->get_functional_settings( ).
    TRY.
        lr_functional->set_sort_on_header_click( abap_true ).
        "lr_functional->set_f2_code( f2code ).
        "lr_functional->set_buffer( gs_test-settings-functional-buffer ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

* ...Set the layout
    lr_layout = lr_alv_table->get_layout( ).
    ls_layout_key-report = sy-repid.
    lr_layout->set_key( ls_layout_key ).
    lr_layout->set_initial_layout( p_layout ).
    AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
                        ID 'ACTVT' FIELD '23'.
    IF sy-subrc = 0.
      lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ) . "no restictions
    ELSE.
      lr_layout->set_save_restriction( cl_salv_layout=>restrict_user_dependant ) . "user dependend
    ENDIF.

*... sort
    TRY.
        lr_sorts = lr_alv_table->get_sorts( ).
        "lr_sorts->add_sort( 'DATUM' ).
        "lr_sorts->add_sort( 'UZEIT' ).

      CATCH cx_salv_data_error cx_salv_existing cx_salv_not_found.
    ENDTRY.

*... set column appearance
    lr_columns = lr_alv_table->get_columns( ).
    lr_columns->set_optimize( abap_true ). " Optimize column width

*... set the color of columns, 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange
    DATA(color_key)      = VALUE lvc_s_colo( col = col_key ).
    DATA(color_source)   = VALUE lvc_s_colo( col = col_total ).
    DATA(color_target)   = VALUE lvc_s_colo( col = col_key ).
    DATA(color_function) = VALUE lvc_s_colo( col = col_negative ).

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

    TRY.
*... convert time stamps
        "lr_column ?= lr_columns->get_column( 'STORE_TO_DATE' ).
        "lr_column->set_edit_mask( '==TSTMP' ).

*... adjust headings
        DATA color TYPE lvc_s_colo.

        " Key

        lr_column ?= lr_columns->get_column( 'DATUM' ).
        "lr_column->set_short_text( 'Date' ).
        "lr_column->set_medium_text( 'Date' ).
        "lr_column->set_long_text( 'Date' ).
        lr_column->set_color( color_key ).

        lr_column ?= lr_columns->get_column( 'UZEIT' ).
        "lr_column->set_short_text( 'Time' ).
        "lr_column->set_medium_text( 'Time' ).
        "lr_column->set_long_text( 'Time' ).
        lr_column->set_color( color_key ).

        lr_column ?= lr_columns->get_column( 'MANDT' ).
        "lr_column->set_short_text( 'Client' ).
        "lr_column->set_medium_text( 'Client' ).
        "lr_column->set_long_text( 'Client' ).
        lr_column->set_color( color_key ).

        lr_column ?= lr_columns->get_column( 'UNAME' ).
        "lr_column->set_short_text( 'User' ).
        "lr_column->set_medium_text( 'User' ).
        "lr_column->set_long_text( 'User' ).
        lr_column->set_color( color_key ).

        lr_column ?= lr_columns->get_column( 'AHOST' ).
        "lr_column->set_short_text( 'Server' ).
        "lr_column->set_medium_text( 'Application Server' ).
        "lr_column->set_long_text( 'Application Server' ).
        lr_column->set_color( color_key ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'MODNO' ).
        "lr_column->set_short_text( 'WP' ).
        "lr_column->set_medium_text( 'Work Process' ).
        "lr_column->set_long_text( 'Index of Work Process' ).
        lr_column->set_color( color_key ).
        lr_column->set_visible( abap_false ).

        " Runtime error

        lr_column ?= lr_columns->get_column( 'FC' ). " Runtime Error
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'CM' ). " C Module
        lr_column->set_short_text( 'C Module' ).
        lr_column->set_medium_text( 'C Module' ).
        lr_column->set_long_text( 'C Module' ).
        lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'CF' ). " C Function
        lr_column->set_short_text( 'C Function' ).
        lr_column->set_medium_text( 'C Function' ).
        lr_column->set_long_text( 'C Function' ).
        lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'RE' ). " Exception like RFC_ERROR_SYSTEM_FAILURE
        lr_column->set_short_text( 'Exception' ).
        lr_column->set_medium_text( 'Exception' ).
        lr_column->set_long_text( 'Exception' ).
        lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'CT' ). " (Wrong) Error description
        lr_column->set_short_text( 'Error' ).
        lr_column->set_medium_text( 'Error description' ).
        lr_column->set_long_text( 'Error description' ).
        lr_column->set_technical( abap_true ).

        " Function

        lr_column ?= lr_columns->get_column( 'R5' ). " RFC: Function
        lr_column->set_short_text( 'Function' ).
        lr_column->set_medium_text( 'Function' ).
        lr_column->set_long_text( 'Function' ).
        lr_column->set_color( color_function ).

        lr_column ?= lr_columns->get_column( 'P3' ). " Runtime Error Parameter: Function
        lr_column->set_short_text( 'Function' ).
        lr_column->set_medium_text( 'Function' ).
        lr_column->set_long_text( 'Function' ).
        lr_column->set_color( color_function ).
        lr_column->set_technical( abap_true ).


        lr_column ?= lr_columns->get_column( 'P2' ). " Runtime Error Parameter: Function group
        lr_column->set_short_text( 'Func Group' ).
        lr_column->set_medium_text( 'Function group' ).
        lr_column->set_long_text( 'Function group' ).
        lr_column->set_color( color_function ).

        lr_column ?= lr_columns->get_column( 'AP' ). " ABAP: Program
        lr_column->set_short_text( 'Program' ).
        lr_column->set_medium_text( 'Program' ).
        lr_column->set_long_text( 'Program' ).
        lr_column->set_color( color_function ).
        lr_column->set_visible( abap_false ).

        " RFC Client / source system

        lr_column ?= lr_columns->get_column( 'R1' ). " RFC: System
        lr_column->set_short_text( 'System' ).
        lr_column->set_medium_text( 'Source system' ).
        lr_column->set_long_text( 'Source system' ).
        lr_column->set_color( color_source ).

        lr_column ?= lr_columns->get_column( 'R9' ). " RFC: Installation Number
        lr_column->set_short_text( 'Inst No' ).
        lr_column->set_medium_text( 'Installation Number' ).
        lr_column->set_long_text( 'Installation Number' ).
        lr_column->set_color( color_source ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'S1' ). " RFC: SAP Version
        lr_column->set_short_text( 'Release' ).
        lr_column->set_medium_text( 'Release' ).
        lr_column->set_long_text( 'Release' ).
        lr_column->set_color( color_source ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'S2' ). " RFC: Kernel Version
        lr_column->set_short_text( 'Kernel' ).
        lr_column->set_medium_text( 'Kernel Version' ).
        lr_column->set_long_text( 'Kernel Version' ).
        lr_column->set_color( color_source ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'R7' ). " RFC: IP Source Address
        lr_column->set_short_text( 'Source IP ' ).
        lr_column->set_medium_text( 'Source IP Address' ).
        lr_column->set_long_text( 'Source IP Address' ).
        lr_column->set_color( color_source ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'R8' ). " RFC: IP Destination
        lr_column->set_short_text( 'Source Sys' ).
        lr_column->set_medium_text( 'Source System' ).
        lr_column->set_long_text( 'Source System' ).
        lr_column->set_color( color_source ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'R3' ). " RFC: Client
        lr_column->set_short_text( 'Client' ).
        lr_column->set_medium_text( 'Source client' ).
        lr_column->set_long_text( 'Source client' ).
        lr_column->set_color( color_source ).

        lr_column ?= lr_columns->get_column( 'R2' ). " RFC: Caller User
        lr_column->set_color( color_source ).

        lr_column ?= lr_columns->get_column( 'S8' ). " RFC: Calling Program
        lr_column->set_color( color_source ).

        lr_column ?= lr_columns->get_column( 'R6' ). " RFC: Destination
        lr_column->set_color( color_source ).

        lr_column ?= lr_columns->get_column( 'S3' ). " RFC: Type
        lr_column->set_short_text( 'RFC Type' ).
        lr_column->set_medium_text( 'RFC Type' ).
        lr_column->set_long_text( 'RFC Type' ).
        lr_column->set_color( color_source ).
        lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'S7' ). " RFC: Asynchronous
        lr_column->set_short_text( 'Async.' ).
        lr_column->set_medium_text( 'Asynchronous' ).
        lr_column->set_long_text( 'Asynchronous' ).
        lr_column->set_color( color_source ).

        lr_column ?= lr_columns->get_column( 'S5' ). " RFC: Trusted Login
        lr_column->set_short_text( 'TrustLogin' ).
        lr_column->set_medium_text( 'Trusted Login' ).
        lr_column->set_long_text( 'Trusted Login' ).
        lr_column->set_color( color_source ).
        lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'S6' ). " RFC: Login
        lr_column->set_short_text( 'Login' ).
        lr_column->set_medium_text( 'Login' ).
        lr_column->set_long_text( 'Login' ).
        lr_column->set_color( color_source ).
        lr_column->set_technical( abap_true ).

        " RFC Server / target system

        lr_column ?= lr_columns->get_column( 'EI' ). " Server IP Address
        lr_column->set_short_text( 'Server IP' ).
        lr_column->set_medium_text( 'Server IP Address' ).
        lr_column->set_long_text( 'Server IP Address' ).
        lr_column->set_color( color_target ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'P4' ). " Runtime Error Parameter: Client
        lr_column->set_short_text( 'Client' ).
        lr_column->set_medium_text( 'Client' ).
        lr_column->set_long_text( 'Client' ).
        lr_column->set_color( color_target ).
        lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'P1' ). " Runtime Error Parameter: User
        lr_column->set_short_text( 'User' ).
        lr_column->set_medium_text( 'User' ).
        lr_column->set_long_text( 'User' ).
        lr_column->set_color( color_target ).
        lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'ER' ). " Kernel Version
        lr_column->set_short_text( 'Kernel' ).
        lr_column->set_medium_text( 'Kernel Version' ).
        lr_column->set_long_text( 'Kernel Version' ).
        lr_column->set_color( color_target ).
        lr_column->set_technical( abap_true ).

      CATCH cx_salv_not_found.
    ENDTRY.

*... Set title
    " Show dumps for runtime error RFC_NO_AUTHORITY

*... show it
    lr_alv_table->display( ).

  ENDMETHOD.

ENDCLASS.                    "lcl_report IMPLEMENTATION

*----------------------------------------------------------------------*
*      REPORT events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_report=>selscreen_output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  lcl_report=>f4_p_layout( CHANGING layout = p_layout ).

AT SELECTION-SCREEN ON p_layout.
  CHECK p_layout IS NOT INITIAL.
  lcl_report=>at_selscr_on_p_layout( p_layout ).

START-OF-SELECTION.
  lcl_report=>start_of_selection( ).
