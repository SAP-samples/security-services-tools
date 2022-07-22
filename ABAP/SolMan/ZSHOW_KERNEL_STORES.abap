*&---------------------------------------------------------------------*
*& Report  ZSHOW_KERNEL_STORES
*&
*&---------------------------------------------------------------------*
*&
*& Show ABAP release, Kernel patch level and version of the CommonCryptoLib
*& using the configuration stores SAP_KERNEL and CRYPTOLIB
*&
*& The report uses following functions to access CCDB data:
*& DIAGST_GET_TECH_SYSTEMS  Get list of systems
*& DIAGST_GET_STORES        Get directory of configuration store instances
*&                          (You can search for STORE_NAME with patterns as well.)
*& DIAGST_TABLE_SNAPSHOT    Read data from configuration store instance
*&
*& If you want to access the same data externally, e.g. for your SIEM application
*& you can use following remote enabled functions instead:
*& DIAGST_GET_TECH_SYSTEMS_RFC
*& DIAGST_GET_STORES_RFC
*& DIAGST_TABLE_SNAPSHOT_RFC
*&
*& Required authorizations to access system data:
*&   AI_DIAGE2E with ACTVT=03
*&   AI_LMDB_OB with LMDB_MTYPE=TECSYST	and LMDB_STYPE=ABAP
*&   AI_LMDB_OB with LMDB_MTYPE=HOST
*& Required authorizations to access configuration store CRYPTOLIB:
*&   AI_CCDB_SC with CONT_AUTH=SECURITY	and ACTVT=03
*&---------------------------------------------------------------------*
REPORT zshow_kernel_stores
  "MESSAGE-ID CL_DIAGST_MESSAGE
  .

CONSTANTS: c_program_version(30) TYPE c VALUE '15.06.2021'.

DATA sel_store_dir TYPE sdiagst_store_dir.

* System name
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) ss_sid FOR FIELD p_sid.
SELECT-OPTIONS p_sid   FOR sel_store_dir-long_sid.
SELECTION-SCREEN END OF LINE.

* Store status
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) ss_state FOR FIELD p_state.
SELECT-OPTIONS p_state FOR sel_store_dir-store_main_state_type." DEFAULT 'G'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

*----------------------------------------------------------------------

TYPES:
  BEGIN OF ts_outtab,
    " Assumption: Match entries from different stores based on install_number and landscape_id

    install_number        TYPE sdiagst_store_dir-install_number,

    long_sid              TYPE diagls_tech_syst_long_sid,    "sdiagst_store_dir-long_sid,
    sid                   TYPE diagls_technical_system_sid,  "sdiagst_store_dir-sid,
    tech_system_type      TYPE diagls_technical_system_type, "sdiagst_store_dir-tech_system_type,
    tech_system_id        TYPE diagls_id,                    "sdiagst_store_dir-tech_system_id,
    landscape_id          TYPE diagls_id,                    "sdiagst_store_dir-landscape_id,
    host_full             TYPE diagls_host_full_name,        "sdiagst_store_dir-host_full,
    host                  TYPE diagls_host_name,             "sdiagst_store_dir-host,
    host_id               TYPE diagls_id,                    "sdiagst_store_dir-host_id,
    physical_host         TYPE diagls_host_name,             "sdiagst_store_dir-physical_host,
    instance_type         TYPE diagls_instance_type,         "sdiagst_store_dir-instance_type,
    instance              TYPE diagls_instance_name,         "sdiagst_store_dir-instance,

    " Source store: we show the status of the first found store only which is usually store SAP_KERNEL
    compv_name            TYPE sdiagst_store_dir-compv_name,

    " Source store: SAP_KERNEL
    kern_rel              TYPE string,                               " 722_EXT_REL
    kern_patchlevel       TYPE string,                               " 1000
    kern_comp_time        TYPE string,                               " Jun  7 2020 15:44:10
    kern_comp_date        TYPE sy-datum,

    " Source store: CRYPTOLIB
    ccl_version           TYPE sdiagst_trow_elem-fieldvalue,         " 8.5.33 May 26 2020
    ccl_vers              TYPE string,
    ccl_date              TYPE sy-datum,

    " Source store: we show the status of the first found store only which is usually store SAP_KERNEL
    store_id              TYPE sdiagst_store_dir-store_id,
    store_last_upload     TYPE sdiagst_store_dir-store_last_upload,
    store_state           TYPE sdiagst_store_dir-store_state,           " CMPL = ok
    store_main_state_type TYPE sdiagst_store_dir-store_main_state_type, " (G)reen, (Y)ello, (R)ed, (N)ot relevant
    store_main_state      TYPE sdiagst_store_dir-store_main_state,
    store_outdated_day    TYPE sdiagst_store_dir-store_outdated_day,
  END OF ts_outtab,
  tt_outtab TYPE TABLE OF ts_outtab.

DATA:
  lt_outtab TYPE tt_outtab,
  ls_outtab TYPE ts_outtab.

*----------------------------------------------------------------------

INITIALIZATION.
  sy-title = 'Show Configuration Stores SAP_KERNEL and CRYPTOLIB'.

  ss_sid   = 'System (long sid)'.
  ss_state = 'Config. store status (G/Y/R)'.

  CONCATENATE 'Program version:'(VER) c_program_version INTO ss_vers
    SEPARATED BY space.

*----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sid-low.
  PERFORM f4_sid USING 'P_SID-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sid-high.
  PERFORM f4_sid USING 'P_SID-HIGH'.

*
FORM f4_sid USING l_dynprofield  TYPE help_info-dynprofld.

  DATA: "dynpro_values TYPE TABLE OF dynpread,
    "field_value   LIKE LINE OF dynpro_values,
    "field_tab     TYPE TABLE OF dfies  WITH HEADER LINE,
    BEGIN OF value_tab OCCURS 0,
      long_sid       TYPE diagls_tech_syst_long_sid,    "sdiagst_store_dir-long_sid,
      "sid                   TYPE diagls_technical_system_sid,  "sdiagst_store_dir-sid,
      "tech_system_id        TYPE diagls_id,                    "sdiagst_store_dir-tech_system_id,
      install_number TYPE diagls_tech_syst_install_nbr,
      itadmin_role   TYPE diagls_itadmin_role,
    END OF value_tab.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.

  DATA:
    lt_technical_systems TYPE  tt_diagst_tech_syst,
    rc                   TYPE  i,
    rc_text              TYPE  natxt.

  CALL FUNCTION 'DIAGST_GET_TECH_SYSTEMS'
    EXPORTING
      namespace         = 'ACTIVE'
*     LONG_SID          =
      tech_type         = 'ABAP'
*     INSTALL_NUMBER    =
*     TECH_SYST_ID      =
*     DIAG_RELEVANT     = 'X'
*     STATS_FROM        =
*     STATS_TO          =
*     DISPLAY           = ' '                        " Only useful if the function is manually executed by transaction SE37.
                                                     " Setting this parameter to ‘X’ will display the result.
*     CALLING_APPL      = ' '
    IMPORTING
      technical_systems = lt_technical_systems
*     STATS             =
      rc                = rc
      rc_text           = rc_text.

  LOOP AT lt_technical_systems INTO DATA(ls_technical_systems).
    MOVE-CORRESPONDING ls_technical_systems TO value_tab.
    APPEND value_tab.
  ENDLOOP.
  SORT value_tab BY long_sid.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LONG_SID'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = l_dynprofield
      value_org       = 'S'
    TABLES
*     field_tab       = field_tab
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM. "F4_SID

*----------------------------------------------------------------------

START-OF-SELECTION.

  PERFORM get_kernel_version.
  PERFORM add_ccl_version.
  PERFORM show_result.

FORM get_kernel_version.

  DATA:
    lt_store_dir_tech    TYPE  tt_diagst_store_dir_tech,
    lt_store_dir         TYPE  tt_diagst_store_dir,
    lt_fieldlist         TYPE  tt_diagst_table_store_fields,
    lt_snapshot          TYPE  tt_diagst_trows,
    rc                   TYPE  i,
    rc_text              TYPE  natxt.

  CALL FUNCTION 'DIAGST_GET_STORES'
    EXPORTING

      " The “System Filter” parameters allow to get all Stores of a system or technical system.
      " Some combinations of the four parameters are not allowed.
      " The function will return an error code in such a case.
*     SID                   = ' '
*     INSTALL_NUMBER        = ' '
*     LONG_SID              = ' '
*     TECH_SYSTEM_TYPE      = 'ABAP'                     "(only together with LONG_SID)

      " Store key fields
      group_namespace       = 'ACTIVE'                   "(optional)
      group_landscape_class = 'CL_DIAGLS_ABAP_INSTANCE'  "(optional)
*     GROUP_LANDSCAPE_ID    = ' '
*     GROUP_COMP_ID         = ' '
      group_source          = 'ABAP'                     "(optional)
      group_name            = 'INSTANCE'                 "(optional)
      store_category        = 'SOFTWARE'                 "(optional)
      store_type            = 'PROPERTY'                 "(optional)
*     STORE_FULLPATH        = ' '
      store_name            = 'SAP_KERNEL'

      " Special filters
      store_mainalias       = 'ABAP-SOFTWARE'            "(optional)
      store_subalias        = 'SAP-KERNEL'               "(optional)
*     STORE_TPL_ID          = ' '
*     HAS_ELEMENT_FROM      =                            " date range
*     HAS_ELEMENT_TO        =                            " date range
*     ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*     CASE_INSENSITIVE      = ' '
*     PATTERN_SEARCH        = 'X'                        " Allow pattern search for SEARCH_STRING
*     SEARCH_STRING         =
*     ONLY_RELEVANT         = 'X'
*     PROTECTED             = 'A'                        " (N)ot, (Y)es, (A)ll

      " Others
*     DISPLAY               = ' '                        " Only useful if the function is manually executed by transaction SE37.
                                                         " Setting this parameter to ‘X’ will display the result.
*     CALLING_APPL          = ' '

    IMPORTING
*     STORE_DIR_TECH        = lt_STORE_DIR_TECH          "(efficient, reduced structure)
      store_dir             = lt_store_dir               "(not recommended anymore)
*     STORE_DIR_MI          =                            "(SAP internal usage only)
*     STORE_STATS           =                            " History regarding the changes of elements (configuration items).
*     PARAMETER             =                            "(SAP internal usage only)
      rc                    = rc
      rc_text               = rc_text.

  IF rc IS NOT INITIAL.
    MESSAGE e001(00) WITH rc_text.
  ENDIF.

  LOOP AT lt_store_dir INTO data(ls_store_dir)
    WHERE long_sid              IN p_sid
      AND store_main_state_type IN p_state
    .

    CLEAR ls_outtab.
    MOVE-CORRESPONDING ls_store_dir TO ls_outtab.

    IF ls_outtab-host_full IS INITIAL.
      ls_outtab-host_full = ls_outtab-host. " host, host_id, physical_host
    ENDIF.

    CALL FUNCTION 'DIAGST_TABLE_SNAPSHOT'
      EXPORTING
        store_id                    = ls_store_dir-store_id
*       TIMESTAMP                   =                        " if not specified the latest available snapshot is returned
*       CALLING_APPL                = ' '
      IMPORTING
        fieldlist                   = lt_fieldlist
*       SNAPSHOT_VALID_FROM         =
*       SNAPSHOT_VALID_TO_CONFIRMED =
*       SNAPSHOT_VALID_TO           =
        snapshot                    = lt_snapshot            " The content of the requested snapshot in ABAP DDIC type format
*       SNAPSHOT_TR                 =
*       SNAPSHOT_ITSAM              =                        " The content of the requested snapshot in XML-based format
        rc                          = rc                     " 3: Permission denied, Content Authorization missing
                                                             " 4: Store not existing
                                                             " 8: Error
        rc_text                     = rc_text.

    LOOP AT lt_snapshot INTO data(lt_snapshot_elem).
      READ TABLE lt_snapshot_elem INTO data(ls_snapshot_element1) INDEX 1. " PARAMETER
      READ TABLE lt_snapshot_elem INTO data(ls_snapshot_element2) INDEX 2. " VALUE
      check ls_snapshot_element1-fieldname = 'PARAMETER'.
      check ls_snapshot_element2-fieldname = 'VALUE'.
      CASE ls_snapshot_element1-fieldvalue.

        WHEN 'KERN_COMP_ON'.      " Linux GNU SLES-11 x86_64 cc4.3.4 use-pr190909
          " not used yet

        WHEN 'KERN_COMP_TIME'.    " Jun  7 2020 15:44:10
          ls_outtab-kern_comp_time  = ls_snapshot_element2-fieldvalue.
          PERFORM convert_comp_time USING ls_outtab-kern_comp_time CHANGING ls_outtab-kern_comp_date.

        WHEN 'KERN_DBLIB'.        " SQLDBC 7.9.8.040
          " not used yet

        WHEN 'KERN_PATCHLEVEL'.   " 1000
          ls_outtab-kern_patchlevel = ls_snapshot_element2-fieldvalue.

        WHEN 'KERN_REL'.          " 722_EXT_REL
          ls_outtab-kern_rel        = ls_snapshot_element2-fieldvalue.

        WHEN 'PLATFORM-ID'.       " 390
          " not used yet

      ENDCASE.
    ENDLOOP.

    APPEND ls_outtab TO lt_outtab.

  ENDLOOP. " lt_STORE_DIR

ENDFORM.

FORM add_ccl_version.

  DATA:
    lt_store_dir_tech    TYPE  tt_diagst_store_dir_tech,
    lt_store_dir         TYPE  tt_diagst_store_dir,
    lt_fieldlist         TYPE  tt_diagst_table_store_fields,
    lt_snapshot          TYPE  tt_diagst_trows,
    rc                   TYPE  i,
    rc_text              TYPE  natxt.

  FIELD-SYMBOLS:
    <fs_outtab>          TYPE ts_outtab.

  CALL FUNCTION 'DIAGST_GET_STORES'
    EXPORTING

      " The “System Filter” parameters allow to get all Stores of a system or technical system.
      " Some combinations of the four parameters are not allowed.
      " The function will return an error code in such a case.
*     SID                   = ' '
*     INSTALL_NUMBER        = ' '
*     LONG_SID              = ' '
*     TECH_SYSTEM_TYPE      = 'ABAP'                     "(only together with LONG_SID)

      " Store key fields
      group_namespace       = 'ACTIVE'                   "(optional)
      group_landscape_class = 'CL_DIAGLS_ABAP_INSTANCE'  "(optional)
*     GROUP_LANDSCAPE_ID    = ' '
*     GROUP_COMP_ID         = ' '
      group_source          = 'ABAP'                     "(optional)
      group_name            = 'INSTANCE'                 "(optional)
      store_category        = 'CONFIG'                   "(optional)
      store_type            = 'PROPERTY'                 "(optional)
*     STORE_FULLPATH        = ' '
      store_name            = 'CRYPTOLIB'

      " Special filters
      store_mainalias       = 'SECURITY'                 "(optional)
      store_subalias        = 'CRYPTOLIB'                "(optional)
*     STORE_TPL_ID          = ' '
*     HAS_ELEMENT_FROM      =                            " date range
*     HAS_ELEMENT_TO        =                            " date range
*     ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*     CASE_INSENSITIVE      = ' '
*     PATTERN_SEARCH        = 'X'                        " Allow pattern search for SEARCH_STRING
*     SEARCH_STRING         =
*     ONLY_RELEVANT         = 'X'
*     PROTECTED             = 'A'                        " (N)ot, (Y)es, (A)ll

      " Others
*     DISPLAY               = ' '                        " Only useful if the function is manually executed by transaction SE37.
                                                         " Setting this parameter to ‘X’ will display the result.
*     CALLING_APPL          = ' '

    IMPORTING
*     STORE_DIR_TECH        = lt_STORE_DIR_TECH          "(efficient, reduced structure)
      store_dir             = lt_store_dir               "(not recommended anymore)
*     STORE_DIR_MI          =                            "(SAP internal usage only)
*     STORE_STATS           =                            " History regarding the changes of elements (configuration items).
*     PARAMETER             =                            "(SAP internal usage only)
      rc                    = rc
      rc_text               = rc_text.

  IF rc IS NOT INITIAL.
    MESSAGE e001(00) WITH rc_text.
  ENDIF.

  LOOP AT lt_store_dir INTO data(ls_store_dir)
    WHERE long_sid              IN p_sid
      AND store_main_state_type IN p_state
    .

    CLEAR ls_outtab.
    MOVE-CORRESPONDING ls_store_dir TO ls_outtab.

    IF ls_outtab-host_full IS INITIAL.
      ls_outtab-host_full = ls_outtab-host. " host, host_id, physical_host
    ENDIF.

    CALL FUNCTION 'DIAGST_TABLE_SNAPSHOT'
      EXPORTING
        store_id                    = ls_store_dir-store_id
*       TIMESTAMP                   =                        " if not specified the latest available snapshot is returned
*       CALLING_APPL                = ' '
      IMPORTING
        fieldlist                   = lt_fieldlist
*       SNAPSHOT_VALID_FROM         =
*       SNAPSHOT_VALID_TO_CONFIRMED =
*       SNAPSHOT_VALID_TO           =
        snapshot                    = lt_snapshot            " The content of the requested snapshot in ABAP DDIC type format
*       SNAPSHOT_TR                 =
*       SNAPSHOT_ITSAM              =                        " The content of the requested snapshot in XML-based format
        rc                          = rc                     " 3: Permission denied, Content Authorization missing
                                                             " 4: Store not existing
                                                             " 8: Error
        rc_text                     = rc_text.

*   We know that we get exactly one entry like this:
    " FIELDPOS  F FIELDNAME                      FIELDVALUE
    "        1  K PARAMETER                      CCL
    "        2  D VALUE                          8.5.33 May 26 2020
    "
    LOOP AT lt_snapshot INTO data(lt_snapshot_elem).
      READ TABLE lt_snapshot_elem INTO data(ls_snapshot_element1) INDEX 1. " PARAMETER
      READ TABLE lt_snapshot_elem INTO data(ls_snapshot_element2) INDEX 2. " VALUE
      check ls_snapshot_element1-fieldname  = 'PARAMETER'.
      check ls_snapshot_element1-fieldvalue = 'CCL'.
      check ls_snapshot_element2-fieldname  = 'VALUE'.
      ls_outtab-ccl_version = ls_snapshot_element2-fieldvalue.
    ENDLOOP.

*   Merge result
    READ TABLE lt_outtab ASSIGNING <fs_outtab>
      WITH KEY
        install_number = ls_outtab-install_number
        landscape_id   = ls_outtab-landscape_id
        "long_sid       = ls_outtab-long_sid
        "sid            = ls_outtab-sid
        "tech_system_id = ls_outtab-tech_system_id
        "host_full      = ls_outtab-host_full
        "instance_type  = ls_outtab-instance_type
        "instance       = ls_outtab-instance
        .
    IF sy-subrc = 0.

      <fs_outtab>-ccl_version = ls_outtab-ccl_version.
      SPLIT <fs_outtab>-ccl_version AT space INTO <fs_outtab>-ccl_vers DATA(comp_date).
      PERFORM convert_comp_time USING comp_date CHANGING <fs_outtab>-ccl_date.

    ELSE.
      APPEND ls_outtab TO lt_outtab.
    ENDIF.

  ENDLOOP. " lt_STORE_DIR

ENDFORM.

FORM show_result.
  DATA:
    lr_table      TYPE REF TO cl_salv_table,               " Main ALV class
    lr_functions  TYPE REF TO cl_salv_functions_list,      " Generic and Application-Specific Functions
    lr_display    TYPE REF TO cl_salv_display_settings,    " Appearance of the ALV Output
    lr_functional TYPE REF TO cl_salv_functional_settings,
    lr_sorts      TYPE REF TO cl_salv_sorts,        " All Sort Objects
    "lr_aggregations      type ref to cl_salv_aggregations,
    "lr_filters           type ref to cl_salv_filters,
    "lr_print             type ref to cl_salv_print,
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
          r_salv_table = lr_table
        CHANGING
          t_table      = lt_outtab ).
    CATCH cx_salv_msg.
  ENDTRY.

*... activate ALV generic Functions
  lr_functions = lr_table->get_functions( ).
  lr_functions->set_all( abap_true ).

*... set the display settings
  lr_display = lr_table->get_display_settings( ).
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

*... set the functional settings
  lr_functional = lr_table->get_functional_settings( ).
  TRY.
      lr_functional->set_sort_on_header_click( abap_true ).
      "lr_functional->set_f2_code( f2code ).
      "lr_functional->set_buffer( gs_test-settings-functional-buffer ).
    CATCH cx_salv_method_not_supported.
  ENDTRY.

*... sort
  TRY.
      lr_sorts = lr_table->get_sorts( ).
      lr_sorts->add_sort( 'INSTALL_NUMBER' ).
      lr_sorts->add_sort( 'LONG_SID' ).
      lr_sorts->add_sort( 'SID' ).
      lr_sorts->add_sort( 'HOST_FULL' ).

    CATCH cx_salv_data_error cx_salv_existing cx_salv_not_found.
  ENDTRY.

*... set column appearance
  lr_columns = lr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ). " Optimize column width

  TRY.
*... convert time stamps
      lr_column ?= lr_columns->get_column( 'STORE_LAST_UPLOAD' ).
      lr_column->set_edit_mask( '==TSTMP' ).

*... adjust headings
      DATA color TYPE lvc_s_colo.

      lr_column ?= lr_columns->get_column( 'TECH_SYSTEM_ID' ).
      lr_column->set_long_text( 'Technical system ID' ).
      lr_column->set_medium_text( 'Technical system ID' ).
      lr_column->set_short_text( 'Tech. sys.' ).

      lr_column ?= lr_columns->get_column( 'LANDSCAPE_ID' ).
      lr_column->set_long_text( 'Landscape ID' ).
      lr_column->set_medium_text( 'Landscape ID' ).
      lr_column->set_short_text( 'Landscape' ).

      lr_column ?= lr_columns->get_column( 'HOST_ID' ).
      lr_column->set_long_text( 'Host ID' ).
      lr_column->set_medium_text( 'Host ID' ).
      lr_column->set_short_text( 'Host ID' ).

      color-col = 4. " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange

      lr_column ?= lr_columns->get_column( 'COMPV_NAME' ).
      lr_column->set_long_text( 'ABAP release' ).   "max. 40 characters
      lr_column->set_medium_text( 'ABAP release' ). "max. 20 characters
      lr_column->set_short_text( 'ABAP rel.' ).     "max. 10 characters
      lr_column->set_color( color ).

      color-col = 2. " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange

      lr_column ?= lr_columns->get_column( 'KERN_REL' ).
      lr_column->set_long_text( 'Kernel release' ).
      lr_column->set_medium_text( 'Kernel release' ).
      lr_column->set_short_text( 'Kernel rel' ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'KERN_PATCHLEVEL' ).
      lr_column->set_long_text( 'Kernel patch level' ).
      lr_column->set_medium_text( 'Kernel patch' ).
      lr_column->set_short_text( 'patch' ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'KERN_COMP_TIME' ).
      lr_column->set_long_text( 'Kernel compilation time' ).
      lr_column->set_medium_text( 'Kernel compilation' ).
      lr_column->set_short_text( 'Comp.time' ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'KERN_COMP_DATE' ).
      lr_column->set_long_text( 'Kernel compilation date' ).
      lr_column->set_medium_text( 'Kernel compilation' ).
      lr_column->set_short_text( 'Comp.date' ).
      lr_column->set_color( color ).

      color-col = 7. " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange

      lr_column ?= lr_columns->get_column( 'CCL_VERSION' ).
      lr_column->set_long_text( 'CommonCryptoLib version' ).
      lr_column->set_medium_text( 'CCL Version' ).
      lr_column->set_short_text( 'CCL Vers.' ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'CCL_VERS' ).
      lr_column->set_long_text( 'CommonCryptoLib version' ).
      lr_column->set_medium_text( 'CCL Version' ). "
      lr_column->set_short_text( 'CCL Vers.' ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'CCL_DATE' ).
      lr_column->set_long_text( 'CommonCryptoLib compilation date' ).
      lr_column->set_medium_text( 'CCL Date' ).
      lr_column->set_short_text( 'CCL Date' ).
      lr_column->set_color( color ).

*... hide unimportant columns
      lr_column ?= lr_columns->get_column( 'LONG_SID' ).                lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'SID' ).                     lr_column->set_visible( abap_true ).

      lr_column ?= lr_columns->get_column( 'TECH_SYSTEM_TYPE' ).        lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'TECH_SYSTEM_ID' ).          lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'LANDSCAPE_ID' ).            lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'STORE_ID' ).                lr_column->set_visible( abap_false ).

      lr_column ?= lr_columns->get_column( 'HOST_FULL' ).               lr_column->set_visible( abap_true ).
      lr_column ?= lr_columns->get_column( 'HOST' ).                    lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'HOST_ID' ).                 lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'PHYSICAL_HOST' ).           lr_column->set_visible( abap_false ).

      lr_column ?= lr_columns->get_column( 'KERN_COMP_TIME' ).          lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'KERN_COMP_DATE' ).          lr_column->set_visible( abap_true ).

      lr_column ?= lr_columns->get_column( 'CCL_VERSION' ).             lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'CCL_VERS' ).                lr_column->set_visible( abap_true ).
      lr_column ?= lr_columns->get_column( 'CCL_DATE' ).                lr_column->set_visible( abap_true ).

      lr_column ?= lr_columns->get_column( 'STORE_ID' ).                lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'STORE_LAST_UPLOAD' ).       lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'STORE_STATE' ).             lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'STORE_MAIN_STATE_TYPE' ).   lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'STORE_MAIN_STATE' ).        lr_column->set_visible( abap_true ).
      lr_column ?= lr_columns->get_column( 'STORE_OUTDATED_DAY' ).      lr_column->set_visible( abap_false ).

    CATCH cx_salv_not_found.
  ENDTRY.

*       Allow to save layout
  lr_layout = lr_table->get_layout( ).
  ls_layout_key-report = sy-repid.
  lr_layout->set_key( ls_layout_key ).
  lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

*... show it
  lr_table->display( ).

ENDFORM.

* Convert text like 'Dec  7 2020' into a date field
FORM convert_comp_time
  USING
    comp_time TYPE string
  CHANGING
    comp_date TYPE sy-datum.

  DATA:
    text     TYPE string,
    day(2)   TYPE n,
    month(2) TYPE n,
    year(4)  TYPE n.

  text = comp_time.
  CONDENSE text.
  SPLIT text AT space INTO DATA(month_c) DATA(day_c) DATA(year_c) DATA(time_c).
  day = day_c.
  CASE month_c.
    WHEN 'Jan'. month = 1.
    WHEN 'Feb'. month = 2.
    WHEN 'Mar'. month = 3.
    WHEN 'Apr'. month = 4.
    WHEN 'May'. month = 5.
    WHEN 'Jun'. month = 6.
    WHEN 'Jul'. month = 7.
    WHEN 'Aug'. month = 8.
    WHEN 'Sep'. month = 9.
    WHEN 'Oct'. month = 10.
    WHEN 'Nov'. month = 11.
    WHEN 'Dec'. month = 12.
  ENDCASE.
  year = year_c.
  comp_date = |{ year }{ month }{ day }|.
ENDFORM.
