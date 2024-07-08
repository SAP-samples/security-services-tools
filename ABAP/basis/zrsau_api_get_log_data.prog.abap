*&---------------------------------------------------------------------*
*& Report ZRSAU_API_GET_LOG_DATA
*& Show usage of RFC function RSAU_API_GET_LOG_DATA
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Published on: https://github.com/SAP-samples/security-services-tools
*&
*& The report requires SAP_BASIS 7.50 as well as note 3054326 - API for remote reading of audit logs as of 7.50
*&
*& 12.03.2024 Initial version
*& 08.07.2024 Improved robustness for older releases or support packages
*&---------------------------------------------------------------------*

* Fields of the result table:

* SID        ABAP System Field: Name of SAP System
* INSTANCE   Application Server Instance
* SAL_DATE   Date
* SAL_TIME   Time
* SLGMAND    Client
* AREA       System Log: Group of 36 System Log Messages [Hide field]
* SUBID      System log: Third character of message name [Hide field]
* MSG        System log message Identifier
* SLGUSER    Character field of length 40 ['User']
**USERALIAS	 Internet user alias (new field, not available in all releases)
**SMTP_ADDR  E-Mail Address (new field, not available in all releases)
**CLASS      User Group (new field, not available in all releases)
* COUNTER    2 Byte Signed Integer ['Counter']
* SLGLTRM2   Character field of length 40 ['Terminal']
* TERM_IPV6  IPv6 - Terminal IP Address
* SLGTC      Transaction Code
* SLGREPNA   Program Name
* SUBCLASID  System log/audit: subgroup classification indicators [Hide field]
* TXSUBCLSID Audit Class
* SEVERITY   System log/audit: message importance [Hide field]
* SEVERITY_S Severity -> ['Severity'] [Hide field]
* TXSEVERITY Message Severity
* FILE_NO    SysLog: File number [Hide field]
* TASKTYPE   System log: SAP process name [Hide field]
* TASKNO     Security Audit Log: Work Process Number [Hide field]
* SAL_DATA   Security Audit Log: Text Part of Formatted Audit Log Message
* X_STRING   Name of an Icon [Hide field]
* PARAM1     Variable Data for Message ['Variable 1']
* PARAM2     Variable Data for Message ['Variable 2']
* PARAM3     Variable Data for Message ['Variable 3']
* PARAMX     Variable Data for Message ['Variable 4']
* EPP        User UUID [Hide field]
* SRC        Data Record Source [Hide field]
* MFD_FLAG   ['MFD flag'] [Hide field]
* ALERT_FLAG ['Alert flag'] [Hide field]
* SLGDATTIM  System log time stamp ['Time stamp']
* LOG_TSTMP  UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun) ['Time stamp UTC'] [Hide field]
* CTAB       Color Attributes for ALV Lists  [Hide field]

REPORT zrsau_api_get_log_data.

CONSTANTS c_program_version(30) TYPE c VALUE '08.07.2024 S44'.

* Selection screen

SELECTION-SCREEN BEGIN OF BLOCK rfc WITH FRAME TITLE t_rfc.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) t_dest FOR FIELD rfcdest.
    PARAMETERS rfcdest     TYPE rfcdest    DEFAULT 'NONE'.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK rfc.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE t_sel.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) t_from FOR FIELD dat_from.
    PARAMETERS dat_from    TYPE datum      DEFAULT sy-datum.
    PARAMETERS tim_from    TYPE syst_uzeit DEFAULT '000000'.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) t_to FOR FIELD dat_to.
    PARAMETERS dat_to      TYPE datum      DEFAULT sy-datum.
    PARAMETERS tim_to      TYPE syst_uzeit DEFAULT sy-uzeit.
  SELECTION-SCREEN END OF LINE.

  DATA rslgno TYPE rslgno.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(30) t_msg FOR FIELD r_msg.
    SELECT-OPTIONS r_msg   FOR rslgno.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(30) t_mandt FOR FIELD r_mandt.
    SELECT-OPTIONS r_mandt FOR sy-mandt.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(30) t_user FOR FIELD r_user.
    SELECT-OPTIONS r_user  FOR sy-uname.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(30) t_tcd FOR FIELD r_tcd.
    SELECT-OPTIONS r_tcd   FOR sy-tcode.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(30) t_reps FOR FIELD r_reps.
    SELECT-OPTIONS r_reps  FOR sy-cprog.
  SELECTION-SCREEN END OF LINE.

  DATA rsdsselop_ TYPE rsdsselop_.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(30) t_inst FOR FIELD r_inst.
    SELECT-OPTIONS r_inst  FOR rsdsselop_.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS short AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 3(30) t_short FOR FIELD short.
SELECTION-SCREEN END OF LINE.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(35) t_layout FOR FIELD layout.
  PARAMETERS       layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      initialization,

      at_selection_screen_f4_layout
        CHANGING
          layout TYPE disvariant-variant,

      start_of_selection
        IMPORTING
          rfcdest       TYPE rfcdest
          dat_from      TYPE datum
          tim_from      TYPE syst_uzeit
          dat_to        TYPE datum
          tim_to        TYPE syst_uzeit
          it_r_msg      TYPE rsau_sel_opt_t_evts    OPTIONAL "ranges table
          it_r_mandt    TYPE aut_t_mandt_range      OPTIONAL "ranges table
          it_r_user     TYPE susr_t_range_4_xubname OPTIONAL "ranges table
          it_r_tcd      TYPE aut_t_tcode_range      OPTIONAL "ranges table
          it_r_reps     TYPE aut_t_progn_range      OPTIONAL "ranges table
          it_r_instance TYPE rsdsselopt_t           OPTIONAL "ranges table
        .

  PRIVATE SECTION.

    CLASS-DATA:
      et_log     TYPE rsau_t_result,
      "et_log_utc TYPE rsau_t_result_utc,
      et_fstat   TYPE rsau_t_file_info,
      et_return  TYPE bapiret2_t.

ENDCLASS.                    "lcl_report DEFINITION

*---------------------------------------------------------------------*
*      CLASS lcl_alv DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_alv DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      show_result
        IMPORTING
          et_fstat  TYPE rsau_t_file_info
          et_return TYPE bapiret2_t
        CHANGING
          et_log    TYPE rsau_t_result.

  PRIVATE SECTION.

    CLASS-DATA:

      r_alv_table      TYPE REF TO cl_salv_table.

ENDCLASS.                    "cl_alv DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    sy-title = 'Show usage of RFC function RSAU_API_GET_LOG_DATA'(tit).

    t_rfc    = 'Remote system'.
    t_dest   = 'RFC Destination'.

    t_sel    = 'Selection'.
    t_msg    = 'Event (Audit message)'.
    t_from   = 'Date/time from'.
    t_to     = 'Date/time to'.
    t_mandt  = 'Client'.
    t_user   = 'User'.
    t_tcd    = 'Transaction'.
    t_reps   = 'Program'.
    t_inst   = 'Application server instance'.

    t_short  = 'Short list (hide some fields)'.
    t_layout = 'Layout'.

    CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
       SEPARATED BY space.

  ENDMETHOD. " initialization

  METHOD at_selection_screen_f4_layout.

    DATA: gs_alv_lout_variant TYPE disvariant.

    gs_alv_lout_variant-report = sy-repid.

    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant         = gs_alv_lout_variant
        i_save             = 'A'
        i_display_via_grid = 'X'
      IMPORTING
        es_variant         = gs_alv_lout_variant
      EXCEPTIONS
        not_found          = 1
        program_error      = 2
        OTHERS             = 3.

    IF sy-subrc = 0.
      layout = gs_alv_lout_variant-variant.
    ELSE.
      MESSAGE s073(0k).
*   Keine Anzeigevariante(n) vorhanden
    ENDIF.

  ENDMETHOD. " at_selection_screen_on_layout

  METHOD start_of_selection.

    DATA:
      is_interval TYPE rsau_sel_intv,
      mess        TYPE text80.

    " Transfer selection
    is_interval-dat_from = dat_from.
    is_interval-tim_from = tim_from.
    is_interval-dat_to   = dat_to.
    is_interval-tim_to   = tim_to.

    CALL FUNCTION 'RSAU_API_GET_LOG_DATA'
      DESTINATION rfcdest " Remote destination
      EXPORTING
        is_interval           = is_interval    " Time interval to read
        it_r_msg              = it_r_msg       " Selection option: Event selection
        it_r_mandt            = it_r_mandt     " Selection option: Client
        it_r_user             = it_r_user      " Selection option: User
        it_r_tcd              = it_r_tcd       " Selection option: Transaction
        it_r_reps             = it_r_reps      " Selection option: Program
        it_r_instance         = it_r_instance  " Selection option: Application instance
      IMPORTING
        et_log                = et_log         " Log data (system time stamp)
        "et_log_utc            = et_log_utc     " Log data (UTC time stamp), NOT USED YET
        et_fstat              = et_fstat       " Statistics information for data sources
        et_return             = et_return      " Application messages
      EXCEPTIONS
        system_failure        = 1 MESSAGE mess
        communication_failure = 2 MESSAGE mess
        OTHERS                = 3.

    " Verify RFC error
    IF sy-subrc IS NOT INITIAL.
      " Show error message
      MESSAGE mess TYPE 'E'.
    ENDIF.

    " Verify message table
    READ TABLE et_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      " Show error message
      " ...
    ENDIF.

    lcl_alv=>show_result(
      EXPORTING
        et_fstat  = et_fstat
        et_return = et_return
      CHANGING
        et_log    = et_log
    ).

  ENDMETHOD. " start_of_selection

ENDCLASS.                    "lcl_report IMPLEMENTATION

*---------------------------------------------------------------------*
*      CLASS lcl_alv IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.

  METHOD show_result.

    " references to ALV objects
    DATA:
      lr_functions_list   TYPE REF TO cl_salv_functions_list,
      "lr_functions           TYPE REF TO cl_salv_functions,
      lr_selections       TYPE REF TO cl_salv_selections,
      lr_columns          TYPE REF TO cl_salv_columns_table,
      lr_column           TYPE REF TO cl_salv_column_list, "or cl_salv_column_table,
      "lr_sorts               TYPE REF TO cl_salv_sorts.
      "lr_events              TYPE REF TO cl_salv_events_table,
      "lr_functional_settings TYPE REF TO cl_salv_functional_settings,
      "lr_hyperlinks          TYPE REF TO cl_salv_hyperlinks,
      "lr_tooltips            TYPE REF TO cl_salv_tooltips,
      lr_layout           TYPE REF TO cl_salv_layout,
      ls_key              TYPE salv_s_layout_key,
      "lr_content             TYPE REF TO cl_salv_form_element,
      "lr_grid_header         TYPE REF TO cl_salv_form_layout_grid,
      lr_grid_footer      TYPE REF TO cl_salv_form_layout_grid,
      lr_display_settings TYPE REF TO cl_salv_display_settings,
      lr_exception        TYPE REF TO cx_salv_error,
      lv_message          TYPE bal_s_msg.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = r_alv_table
          CHANGING
            t_table      = et_log ).

      CATCH cx_salv_msg
            INTO lr_exception.
        lv_message = lr_exception->get_message( ).
        MESSAGE ID lv_message-msgid TYPE lv_message-msgty
                NUMBER lv_message-msgno
                WITH lv_message-msgv1 lv_message-msgv2
                     lv_message-msgv3 lv_message-msgv4.
    ENDTRY.

    "Set the default ALV functions
    lr_functions_list = r_alv_table->get_functions( ).
    "lr_functions_list->set_detail( abap_true ).
    "lr_functions_list->set_default( abap_true ).
    lr_functions_list->set_all( abap_true ).

    " Set the layout
    lr_layout = r_alv_table->get_layout( ).
    ls_key-report = sy-repid.
    lr_layout->set_key( ls_key ).
    lr_layout->set_initial_layout( layout ).  " Let's use the global variable here.
    AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
                        ID 'ACTVT' FIELD '23'.
    IF sy-subrc = 0.
      lr_layout->set_save_restriction( 3 ). "no restictions
    ELSE.
      lr_layout->set_save_restriction( 2 ). "user dependend
    ENDIF.

    " Selection mode: single cell
    lr_selections   = r_alv_table->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

    lr_columns = r_alv_table->get_columns( ).

    " Set the columns visible
    lr_columns->set_optimize( if_salv_c_bool_sap=>true ).

    TRY.
        " Adjust fields
* Methods of class CL_SALV_COLUMN
* SET_SHORT_TEXT          Set Short Column Title (10 chars)
* SET_MEDIUM_TEXT         Set Medium Column Title (20 chars)
* SET_LONG_TEXT           Set Long Column Title (40 chars)
* SET_TOOLTIP             Set Tooltip for Column Title (40 chars)
* SET_ROW                 Set Row for Multirow Display
*
* SET_ALIGNMENT           Set Alignment of Column
* SET_CURRENCY            Set Currency for Whole Column
* SET_CURRENCY_COLUMN     Set Currency Column
* SET_DDIC_REFERENCE      Set DDIC Reference
* SET_DECIMALS_COLUMN     Set Decimal Column for Number of Decimal Places
* SET_DECIMALS            Set Number of Decimal Places for Whole Column
* SET_EDIT_MASK           Set Conversion Routine
* SET_F1_ROLLNAME         Set F1 Data Element
* SET_LEADING_ZERO        Set Leading Zeroes for Output
* SET_LOWERCASE           Specify Whether Lowercase Letters Converted
* SET_OPTIMIZED           Set Optimize Column
* SET_OUTPUT_LENGTH       Set Output Length (CHAR)
* SET_QUANTITY            Set Unit of Measurement for Whole Column
* SET_QUANTITY_COLUMN     Set Column for Units of Measurement
* SET_ROUND               Set Rounding for Whole Column
* SET_ROUND_COLUMN        Set Rounding Column
* SET_SIGN                Specify Whether Sign Displayed in Output
* SET_TECHNICAL           Set Column as Technical Column
* SET_VISIBLE             Set Column to Visible
* SET_ZERO                Specify Empty Cell Display

* Additional methods of class CL_SALV_COLUMN_LIST / .._TABLE
* SET_COLOR               Set Column Color
* SET_ICON                Set Column as Icon Column
* SET_KEY                 Set Column as Key Column
*
* SET_CELL_TYPE           Set Cell Type
* SET_DROPDOWN_ENTRY      Set Handle for Dropdown
* SET_HYPERLINK_ENTRY     Set Handle for Dropdown
* SET_F4                  Set Column with F1 Help
* SET_F4_CHECKTABLE       Set Check Table for F4 Help
* SET_KEY_PRESENCE_REQUIRED Set Key Columns as Always Visible
* SET_SYMBOL              Set Column as Symbol Column
* SET_TEXT_COLUMN         Set Text Column

        lr_column ?= lr_columns->get_column( 'SLGUSER' ).
        lr_column->set_short_text( 'User' ).
        lr_column->set_medium_text( 'User' ).
        lr_column->set_long_text( 'User' ).

        lr_column ?= lr_columns->get_column( 'COUNTER' ).
        lr_column->set_short_text( 'Number' ).
        lr_column->set_medium_text( 'Event number' ).
        lr_column->set_long_text( 'Event number inside a second' ).

        lr_column ?= lr_columns->get_column( 'SLGLTRM2' ).
        lr_column->set_short_text( 'Terminal' ).
        lr_column->set_medium_text( 'Terminal name' ).
        lr_column->set_long_text( 'Terminal name' ).

        lr_column ?= lr_columns->get_column( 'SEVERITY_S' ).
        lr_column->set_short_text( 'Critical.' ).
        lr_column->set_medium_text( 'Criticality' ).
        lr_column->set_long_text( 'Criticality' ).

        lr_column ?= lr_columns->get_column( 'PARAM1' ).
        lr_column->set_short_text( 'Variable 1' ).
        lr_column->set_medium_text( 'Variable 1' ).
        lr_column->set_long_text( 'Variable 1' ).

        lr_column ?= lr_columns->get_column( 'PARAM2' ).
        lr_column->set_short_text( 'Variable 2' ).
        lr_column->set_medium_text( 'Variable 2' ).
        lr_column->set_long_text( 'Variable 2' ).

        lr_column ?= lr_columns->get_column( 'PARAM3' ).
        lr_column->set_short_text( 'Variable 3' ).
        lr_column->set_medium_text( 'Variable 3' ).
        lr_column->set_long_text( 'Variable 3' ).

        lr_column ?= lr_columns->get_column( 'PARAMX' ).
        lr_column->set_short_text( 'Variable X' ).
        lr_column->set_medium_text( 'Other variables' ).
        lr_column->set_long_text( 'Other variables' ).

        "lr_column ?= lr_columns->get_column( 'MFD_FLAG' ).
        "lr_column->set_short_text( 'Deletion' ).
        "lr_column->set_medium_text( 'Marked for deletion' ).
        "lr_column->set_long_text( 'Marked for deletion' ).

        "lr_column ?= lr_columns->get_column( 'ALERT_FLAG' ).
        "lr_column->set_short_text( 'Alert flag' ).
        "lr_column->set_medium_text( 'Alert flag' ).
        "lr_column->set_long_text( 'Alert flag' ).

        "lr_column ?= lr_columns->get_column( 'SLGDATTIM' ).
        "lr_column->set_short_text( 'Time stamp' ).
        "lr_column->set_medium_text( 'Time stamp' ).
        "lr_column->set_long_text( 'Time stamp' ).

        "lr_column ?= lr_columns->get_column( 'LOG_TSTMP' ).
        "lr_column->set_short_text( 'Time UTC' ).
        "lr_column->set_medium_text( 'Time stamp UTC' ).
        "lr_column->set_long_text( 'Time stamp UTC' ).

        " Hide some fields if the user likes to see a short list
        IF short = 'X' AND layout IS INITIAL. " Let's use the global variable here.

          lr_column ?= lr_columns->get_column( 'AREA' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'SUBID' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'SUBCLASID' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'SEVERITY' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'SEVERITY_S' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'FILE_NO' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'TASKTYPE' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'TASKNO' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'X_STRING' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          "lr_column ?= lr_columns->get_column( 'EPP' ).
          "lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'SRC' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          "lr_column ?= lr_columns->get_column( 'MFD_FLAG' ).
          "lr_column->set_visible( if_salv_c_bool_sap=>false ).

          "lr_column ?= lr_columns->get_column( 'ALERT_FLAG' ).
          "lr_column->set_visible( if_salv_c_bool_sap=>false ).

          "lr_column ?= lr_columns->get_column( 'SLGDATTIM' ).
          "lr_column->set_visible( if_salv_c_bool_sap=>false ).

          "lr_column ?= lr_columns->get_column( 'LOG_TSTMP' ).
          "lr_column->set_visible( if_salv_c_bool_sap=>false ).

        ENDIF.

      CATCH cx_salv_not_found
        INTO lr_exception.
        lv_message = lr_exception->get_message( ).
        MESSAGE ID lv_message-msgid TYPE lv_message-msgty
                NUMBER lv_message-msgno
                WITH lv_message-msgv1 lv_message-msgv2
                     lv_message-msgv3 lv_message-msgv4.
    ENDTRY.

    " Additional fields depending on the release
    TRY.
        lr_column ?= lr_columns->get_column( 'MFD_FLAG' ).
        lr_column->set_short_text( 'Deletion' ).
        lr_column->set_medium_text( 'Marked for deletion' ).
        lr_column->set_long_text( 'Marked for deletion' ).

        lr_column ?= lr_columns->get_column( 'ALERT_FLAG' ).
        lr_column->set_short_text( 'Alert flag' ).
        lr_column->set_medium_text( 'Alert flag' ).
        lr_column->set_long_text( 'Alert flag' ).

        lr_column ?= lr_columns->get_column( 'SLGDATTIM' ).
        lr_column->set_short_text( 'Time stamp' ).
        lr_column->set_medium_text( 'Time stamp' ).
        lr_column->set_long_text( 'Time stamp' ).

        lr_column ?= lr_columns->get_column( 'LOG_TSTMP' ).
        lr_column->set_short_text( 'Time UTC' ).
        lr_column->set_medium_text( 'Time stamp UTC' ).
        lr_column->set_long_text( 'Time stamp UTC' ).

        " Hide some fields if the user likes to see a short list
        IF short = 'X' AND layout IS INITIAL. " Let's use the global variable here.

          lr_column ?= lr_columns->get_column( 'EPP' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'MFD_FLAG' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'ALERT_FLAG' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).

          "lr_column ?= lr_columns->get_column( 'SLGDATTIM' ).
          "lr_column->set_visible( if_salv_c_bool_sap=>false ).

          lr_column ?= lr_columns->get_column( 'LOG_TSTMP' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        ENDIF.

      CATCH cx_salv_not_found
        INTO lr_exception.
        " ok
    ENDTRY.

    " Set the color of cells
    TRY.
        lr_columns->set_color_column( 'CTAB' ). " Does not work: The color field has to have type LVC_T_SCOL but not RSAU_T_COLOR
      CATCH
        cx_salv_data_error    " column not in data table
        cx_salv_invalid_input " invalid input type, requires is type LVC_T_SCOL
        .
        TRY.
            lr_column ?= lr_columns->get_column( 'CTAB' ).
            lr_column->set_technical( if_salv_c_bool_sap=>true ). " Hide color field
          CATCH cx_salv_not_found
           INTO lr_exception.
            lv_message = lr_exception->get_message( ).
            MESSAGE ID lv_message-msgid TYPE lv_message-msgty
                    NUMBER lv_message-msgno
                    WITH lv_message-msgv1 lv_message-msgv2
                         lv_message-msgv3 lv_message-msgv4.
        ENDTRY.
    ENDTRY.

    " Footer (not visible on gui container)
    DATA l_line TYPE i.
    CREATE OBJECT lr_grid_footer.
    l_line = 1.

    " RFC destination
    lr_grid_footer->create_text(
         row    = l_line
         column = 1
         text   = 'RFC Destination:' ).
    lr_grid_footer->create_text(
         row    = l_line
         column = 2
         text   = rfcdest ). " Let's use the global variable here.
    ADD 1 TO l_line.

    " Log
    LOOP AT et_return INTO DATA(ls_return).
      lr_grid_footer->create_text(
           row    = l_line
           column = 1
           text   = ls_return-type ).
      lr_grid_footer->create_text(
           row    = l_line
           column = 2
           text   = ls_return-message ).
      ADD 1 TO l_line.
    ENDLOOP.

    " Program version
    lr_grid_footer->create_text(
         row    = l_line
         column = 1
         text   = 'Program version:'(ver) ).
    lr_grid_footer->create_text(
         row    = l_line
         column = 2
         text   = c_program_version ).
    ADD 1 TO l_line.

    r_alv_table->set_end_of_list( lr_grid_footer ).

    " Set Title
    lr_display_settings = r_alv_table->get_display_settings( ).
    lr_display_settings->set_list_header( 'Show usage of RFC function RSAU_API_GET_LOG_DATA' ). "sy-title
    lr_display_settings->set_list_header_size(
      cl_salv_display_settings=>c_header_size_small ).
    lr_display_settings->set_no_merging( if_salv_c_bool_sap=>true ).

    " Display the table
    r_alv_table->display( ).

  ENDMETHOD. " show_result

ENDCLASS.                    "cl_alv IMPLEMENTATION


*----------------------------------------------------------------------*
*      REPORT events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR r_msg-low.
  CALL METHOD cl_sal_f4=>f4_for_tsl1d
    EXPORTING
      iv_repid     = sy-repid
      iv_dynnr     = '1000'
      iv_fieldname = 'R_MSG-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR r_msg-high.
  CALL METHOD cl_sal_f4=>f4_for_tsl1d
    EXPORTING
      iv_repid     = sy-repid
      iv_dynnr     = '1000'
      iv_fieldname = 'R_MSG-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR r_inst-low.
  CALL METHOD cl_sal_f4=>f4_for_sel_server
    EXPORTING
      iv_repid     = sy-repid
      iv_dynnr     = '1000'
      iv_fieldname = 'R_INST-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR r_inst-high.
  CALL METHOD cl_sal_f4=>f4_for_sel_server
    EXPORTING
      iv_repid     = sy-repid
      iv_dynnr     = '1000'
      iv_fieldname = 'R_INST-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR layout.
  "  call method cl_sal_f4=>f4_for_vari  " works well, too
  "    exporting
  "      iv_repid     = sy-repid
  "      iv_dynnr     = '1000'
  "      iv_fieldname = 'LAYOUT'
  "      iv_alv_clas  = ' '.
  lcl_report=>at_selection_screen_f4_layout(
    CHANGING layout = layout
  ).

START-OF-SELECTION.
  lcl_report=>start_of_selection(
    EXPORTING
          rfcdest       = rfcdest
          dat_from      = dat_from
          tim_from      = tim_from
          dat_to        = dat_to
          tim_to        = tim_to
          it_r_msg      = r_msg[]
          it_r_mandt    = r_mandt[]
          it_r_user     = r_user[]
          it_r_tcd      = r_tcd[]
          it_r_reps     = r_reps[]
          it_r_instance = r_inst[]
  ).
