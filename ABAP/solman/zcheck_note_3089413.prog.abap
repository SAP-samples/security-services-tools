*&---------------------------------------------------------------------*
*& Report  ZCHECK_NOTE_3089413
*& Check implementation status of note 3089413 for connected ABAP systems
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Source: https://github.com/SAP-samples/security-services-tools
*&
*& 08.09.2023 SLIN corrections
*& 06.07.2023 Typo in text corrected
*& 29.06.2023 Updated Kernel prerequisites as described in note 3224161
*&            Updated Note prerequisites for note 3287611 v9
*& 28.03.2023 New check about generic authorizations for S_RFCACL (configuration in CCDB needed)
*& 13.03.2023 Updated note 3287611, new note 3304520
*& 27.02.2023 Check version of installed notes, small corrections
*& 16.02.2023 Show count of migrated trusted destinations per trust relation
*&            ABAPLINT corrections, optimized performance
*& 15.02.2023 Show more details about destinations
*& 14.02.2023 A double click on a count of destinations shows a popup with the details
*& 13.02.2023 Refactoring to use local methods instead of forms
*& 07.02.2023 Show trusted systems without any data in RFCSYSACL
*&            Show mutual trust relations
*& 06.02.2023 New result field to indicate explicit selftrust defined in SMT1
*&            A double click on a count of trusted systems shows a popup with the details
*& 02.02.2023 Check destinations, too
*& 02.02.2023 Initial version
*&---------------------------------------------------------------------*
REPORT zcheck_note_3089413.

CONSTANTS c_program_version(30) TYPE c VALUE '08.09.2023 FBT'.

TYPE-POOLS: icon, col, sym.

* System name
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) ss_sid FOR FIELD p_sid.
SELECT-OPTIONS p_sid   FOR ('DIAGLS_TECH_SYST_LONG_SID').
SELECTION-SCREEN END OF LINE.

* Check Kernel
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS       p_kern AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(80) ps_kern FOR FIELD p_kern.
SELECTION-SCREEN END OF LINE.

* Check ABAP
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS       p_abap AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(80) ps_abap FOR FIELD p_abap.
SELECTION-SCREEN END OF LINE.

* Check trusted relations
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS       p_trust AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(80) ps_trust FOR FIELD p_trust.
SELECTION-SCREEN END OF LINE.

* Check trusted destinations
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS       p_dest AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(80) ps_dest FOR FIELD p_dest.
SELECTION-SCREEN END OF LINE.
* Show specific type only if data found
DATA p_dest_3 TYPE abap_bool.
DATA p_dest_h TYPE abap_bool.
DATA p_dest_w TYPE abap_bool.

* Check user authorizations for S_RFCACL
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS       p_auth AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(80) ps_auth FOR FIELD p_auth.
SELECTION-SCREEN END OF LINE.

* Store status
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) ss_state FOR FIELD p_state.
SELECT-OPTIONS p_state FOR ('DIAGST_MAIN_STATE_TYPE') VISIBLE LENGTH 1." DEFAULT 'G'.
SELECTION-SCREEN END OF LINE.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) ps_lout FOR FIELD p_layout.
PARAMETERS       p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

*---------------------------------------------------------------------*

" Main result table
TYPES:
  BEGIN OF ts_result,
    " Assumption: Match entries from different stores based on install_number and landscape_id

    install_number               TYPE sdiagst_store_dir-install_number,

    long_sid                     TYPE diagls_tech_syst_long_sid,    "sdiagst_store_dir-long_sid,
    sid                          TYPE diagls_technical_system_sid,  "sdiagst_store_dir-sid,
    tech_system_type             TYPE diagls_technical_system_type, "sdiagst_store_dir-tech_system_type,
    tech_system_id               TYPE diagls_id,                    "sdiagst_store_dir-tech_system_id,
    landscape_id                 TYPE diagls_id,                    "sdiagst_store_dir-landscape_id,
    host_full                    TYPE diagls_host_full_name,        "sdiagst_store_dir-host_full,
    host                         TYPE diagls_host_name,             "sdiagst_store_dir-host,
    host_id                      TYPE diagls_id,                    "sdiagst_store_dir-host_id,
    physical_host                TYPE diagls_host_name,             "sdiagst_store_dir-physical_host,
    instance_type                TYPE diagls_instance_type,         "sdiagst_store_dir-instance_type,
    instance                     TYPE diagls_instance_name,         "sdiagst_store_dir-instance,

    " Source store: SAP_KERNEL
    kern_rel                     TYPE string,                               " 722_EXT_REL
    kern_patchlevel              TYPE string,                               " 1000
    kern_comp_time               TYPE string,                               " Jun  7 2020 15:44:10
    kern_comp_date               TYPE sy-datum,

    validate_kernel              TYPE string,

    " Source store: ABAP_COMP_SPLEVEL
    compv_name                   TYPE sdiagst_store_dir-compv_name,         " Software Component Version
    abap_release                 TYPE string,                               " 754
    abap_sp                      TYPE string,                               " 0032

    validate_abap                TYPE string,

    " Source store: ABAP_NOTES
    note_3089413                 TYPE string,
    note_3089413_prstatus        TYPE cwbprstat,
    note_3287611                 TYPE string,
    note_3287611_prstatus        TYPE cwbprstat,
    note_3304520                 TYPE string,
    note_3304520_prstatus        TYPE cwbprstat,

    " Source store: RFCSYSACL
    trustsy_cnt_all              TYPE i,
    no_data_cnt                  TYPE i,
    mutual_trust_cnt             TYPE i,
    trustsy_cnt_tcd              TYPE i,
    trustsy_cnt_3                TYPE i,
    trustsy_cnt_2                TYPE i,
    trustsy_cnt_1                TYPE i,
    explicit_selftrust           TYPE string,

    " Source store: ABAP_INSTANMCE_PAHI
    rfc_selftrust                TYPE string,
    rfc_allowoldticket4tt        TYPE string,
    rfc_sendinstnr4tt            TYPE string,

    " Source store: RFCDES
    dest_3_cnt_all               TYPE i,
    dest_3_cnt_trusted           TYPE i,
    dest_3_cnt_trusted_migrated  TYPE i,
    dest_3_cnt_trusted_no_instnr TYPE i,
    dest_3_cnt_trusted_no_sysid  TYPE i,
    dest_3_cnt_trusted_snc       TYPE i,

    dest_h_cnt_all               TYPE i,
    dest_h_cnt_trusted           TYPE i,
    dest_h_cnt_trusted_migrated  TYPE i,
    dest_h_cnt_trusted_no_instnr TYPE i,
    dest_h_cnt_trusted_no_sysid  TYPE i,
    dest_h_cnt_trusted_tls       TYPE i,

    dest_w_cnt_all               TYPE i,
    dest_w_cnt_trusted           TYPE i,
    dest_w_cnt_trusted_migrated  TYPE i,
    dest_w_cnt_trusted_no_instnr TYPE i,
    dest_w_cnt_trusted_no_sysid  TYPE i,
    dest_w_cnt_trusted_tls       TYPE i,

    " Source store: AUTH_COMB_CHECK_USER (users having generic authorizations for S_RFCACL)
    s_rfcacl_status              TYPE string,
    s_rfcacl_active_users        TYPE i,
    s_rfcacl_inactive_users      TYPE i,

    " Source store: we show the status of the first found store only which is usually store SAP_KERNEL
    store_name                   TYPE sdiagst_store_dir-store_name,
    store_id                     TYPE sdiagst_store_dir-store_id,
    store_last_upload            TYPE sdiagst_store_dir-store_last_upload,
    store_state                  TYPE sdiagst_store_dir-store_state,           " CMPL = ok
    store_main_state_type        TYPE sdiagst_store_dir-store_main_state_type, " (G)reen, (Y)ello, (R)ed, (N)ot relevant
    store_main_state             TYPE sdiagst_store_dir-store_main_state,
    store_outdated_day           TYPE sdiagst_store_dir-store_outdated_day,

    t_color                      TYPE lvc_t_scol,
    "t_celltype                   type salv_t_int4_column,
    "T_HYPERLINK                  type SALV_T_INT4_COLUMN,
    "t_dropdown                   type salv_t_int4_column,
  END OF ts_result,
  tt_result TYPE STANDARD TABLE OF ts_result.

" Popup showing trusted systems
TYPES:
  BEGIN OF ts_rfcsysacl_data,
    rfcsysid         TYPE rfcssysid,  " Trusted system
    tlicense_nr      TYPE slic_inst,  " Installation number of trusted system

    rfctrustsy       TYPE rfcssysid,  " Trusting system (=current system)
    llicense_nr      TYPE slic_inst,  " Installation number of trusting system (=current system), only available in higher versions

    rfcslopt         TYPE rfcslopt,   " Options respective version
    no_data          TYPE string,    " No data found for trusted system
    mutual_trust     TYPE string,    " Mutual trust relation

    rfcdest          TYPE rfcdest,    " Destination to trusted system
    rfccredest       TYPE rfcdest,    " Destination, only available in higher versions
    rfcregdest       TYPE rfcdest,    " Destination, only available in higher versions

    rfcsnc           TYPE rfcsnc,     " SNC respective TLS
    rfcseckey        TYPE rfcticket,  " Security key (empty or '(stored)'), only available in higher versions
    rfctcdchk        TYPE rfctcdchk,  " Tcode check

    trusted_dest_cnt TYPE i,          " Count of migrated trusted destinations in trusted system

    t_color          TYPE lvc_t_scol,
  END OF ts_rfcsysacl_data,
  tt_rfcsysacl_data TYPE STANDARD TABLE OF ts_rfcsysacl_data WITH KEY rfcsysid tlicense_nr,

  BEGIN OF ts_trusted_system,
    rfctrustsy     TYPE rfcssysid,  " Trusting system (=current system)
    llicense_nr    TYPE slic_inst,  " Installation number of trusting system
    rfcsysacl_data TYPE tt_rfcsysacl_data,
  END OF ts_trusted_system,
  tt_trusted_systems TYPE STANDARD TABLE OF ts_trusted_system WITH KEY rfctrustsy llicense_nr.

" Popup showing destinations
TYPES:
  BEGIN OF ts_destination_data,
    rfcdest       TYPE rfcdest,
    rfctype       TYPE rfctype_d,

    trusted(1),                    " Flag for Trusted RFC
    rfcslogin     TYPE rfcslogin,   " Logon Procedure
    serversysid   TYPE rfcsysid,    " System ID of target system
    serverinstnr  TYPE slic_inst,   " Installation number of target system
    check_trusted TYPE string,     " Check trusted relation in trusting system

    rfchost       TYPE rfchost_ext, " Name of Target Host
    rfcservice    TYPE rfcservice,  " Service used (TCP service, SAP System number)

    rfcsysid      TYPE rfcsysid,    " System ID
    rfcclient     TYPE rfcclient,   " Explicit logon client
    rfcsameusr    TYPE rfcsameusr,  " Current User
    rfcuser       TYPE rfcuser,     " Explicit user ID
    rfcauth       TYPE rfcauth,     " Explicit password

    rfcsnc        TYPE rfcsnc,      " RFC Secure Network Communication (HTTP SSL)
    sslapplic     TYPE ssfapplssl,  " SSL Client Identity
    noccert       TYPE char1,       " No client cert

    t_color       TYPE lvc_t_scol,
  END OF ts_destination_data,
  tt_destination_data TYPE STANDARD TABLE OF ts_destination_data WITH KEY rfcdest,

  BEGIN OF ts_destination,
    sid              TYPE diagls_technical_system_sid,  "sdiagst_store_dir-sid,
    install_number   TYPE sdiagst_store_dir-install_number,
    destination_data TYPE tt_destination_data,
  END OF ts_destination,
  tt_destinations TYPE STANDARD TABLE OF ts_destination WITH KEY sid install_number.

" Popup showing users
TYPES:
  BEGIN OF ts_user_data,
    client     TYPE usr02-mandt,
    rc         TYPE string,
    user       TYPE usr02-bname,
    locked(1),
    invalid(1),
    user_type  TYPE string,
    user_group TYPE usr02-class,
  END OF ts_user_data,
  tt_user_data TYPE STANDARD TABLE OF ts_user_data WITH KEY client user,

  BEGIN OF ts_critical_user,
    sid            TYPE diagls_technical_system_sid,  "sdiagst_store_dir-sid,
    install_number TYPE sdiagst_store_dir-install_number,
    user_data      TYPE tt_user_data,
  END OF ts_critical_user,
  tt_critical_users TYPE STANDARD TABLE OF ts_critical_user WITH KEY sid install_number.

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      initialization,

      f4_s_sid,

      f4_p_layout
        CHANGING layout TYPE disvariant-variant,

      at_selscr_on_p_layout
        IMPORTING layout TYPE disvariant-variant,

      start_of_selection.

  PRIVATE SECTION.

    CLASS-DATA:
      " main data table
      lt_result          TYPE tt_result,
      " details about trust relations
      lt_trusted_systems TYPE tt_trusted_systems,
      " details about destinations
      lt_destinations    TYPE tt_destinations,
      " details about users
      lt_critical_users  TYPE tt_critical_users.

    CLASS-DATA:
      " main ALV table
      lr_alv_table  TYPE REF TO cl_salv_table,
      " for handling the events on the main ALV table
      lr_alv_events TYPE REF TO lcl_report.

    CLASS-METHODS:

      get_sap_kernel,

      " Convert text like 'Dec  7 2020' into a date field
      convert_comp_time
        IMPORTING comp_time        TYPE string
        RETURNING VALUE(comp_date) TYPE sy-datum,

      get_abap_comp_splevel,

      get_abap_notes,

      get_rfcsysacl,

      get_rfcdes,

      get_abap_instance_pahi,

      get_authorizations,

      validate_kernel,

      validate_abap,

      validate_mutual_trust,

      count_dest_per_trust,

      validate_trust_for_dest,

      validate_authorizations,

      show_result,

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

*      on_single_click for event link_click of cl_salv_events_table
*        importing row column,

      show_trusted_systems
        IMPORTING
          column      TYPE salv_de_column
          rfctrustsy  TYPE ts_result-sid
          llicense_nr TYPE ts_result-install_number,

      show_destinations
        IMPORTING
          column         TYPE salv_de_column
          sid            TYPE ts_result-sid
          install_number TYPE ts_result-install_number,

      show_critical_users
        IMPORTING
          column         TYPE salv_de_column
          sid            TYPE ts_result-sid
          install_number TYPE ts_result-install_number.

ENDCLASS.                    "lcl_report DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    sy-title = 'Check implementation status of note 3089413 for connected ABAP systems'(TIT).

    ss_sid   = 'System'.
    ss_state = 'Config. store status (G/Y/R)'.

    ps_kern  = 'Check Kernel'.
    ps_abap  = 'Check Support Package and Notes'.
    ps_trust = 'Check Trusted Relations (in server systems)'.
    ps_dest  = 'Check Trusted Destinations (in client systems)'.
    ps_auth  = 'Check critical authorizations for S_RFCACL (configuration needed)'.

    ps_lout  = 'Layout'.

    CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
       SEPARATED BY space.

  ENDMETHOD. " initialization

  METHOD f4_s_sid.

    TYPES:
      BEGIN OF ts_f4_value,
        long_sid       TYPE diagls_tech_syst_long_sid,    "sdiagst_store_dir-long_sid,
        "sid                   TYPE diagls_technical_system_sid,  "sdiagst_store_dir-sid,
        "tech_system_id        TYPE diagls_id,                    "sdiagst_store_dir-tech_system_id,
        install_number TYPE diagls_tech_syst_install_nbr,
        itadmin_role   TYPE diagls_itadmin_role,
      END OF ts_f4_value.

    DATA:
      f4_value     TYPE          ts_f4_value,
      f4_value_tab TYPE TABLE OF ts_f4_value.

    DATA:
      lt_technical_systems TYPE  tt_diagst_tech_syst,
      rc                   TYPE  i,
      rc_text              TYPE  natxt.

    CALL FUNCTION 'DIAGST_GET_TECH_SYSTEMS'
      EXPORTING
        namespace         = 'ACTIVE'
*       LONG_SID          =
        tech_type         = 'ABAP'
*       INSTALL_NUMBER    =
*       TECH_SYST_ID      =
*       DIAG_RELEVANT     = 'X'
*       STATS_FROM        =
*       STATS_TO          =
*       DISPLAY           = ' '                        " Only useful if the function is manually executed by transaction SE37.
        " Setting this parameter to 'X' will display the result.
*       CALLING_APPL      = ' '
      IMPORTING
        technical_systems = lt_technical_systems
*       STATS             =
        rc                = rc
        rc_text           = rc_text.

    LOOP AT lt_technical_systems INTO DATA(ls_technical_systems).
      MOVE-CORRESPONDING ls_technical_systems TO f4_value.
      APPEND f4_value TO f4_value_tab.
    ENDLOOP.
    SORT f4_value_tab BY long_sid.

    DATA(progname) = sy-repid.
    DATA(dynnum)   = sy-dynnr.
    DATA field TYPE dynfnam.
    DATA stepl TYPE sy-stepl.
    GET CURSOR FIELD field LINE stepl.
    DATA return_tab TYPE TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'LONG_SID'
        dynpprog        = progname
        dynpnr          = dynnum
        dynprofield     = field
        stepl           = stepl
        value_org       = 'S'
      TABLES
*       field_tab       = field_tab
        value_tab       = f4_value_tab
        return_tab      = return_tab " surprisingly required to get lower case values
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.

  ENDMETHOD. " f4_s_sid

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

    get_sap_kernel( ).         " Kernel version
    get_abap_comp_splevel( ).  " Support Package version of SAP_BASIS
    get_abap_notes( ).         " Notes 3089413 and 3287611
    get_rfcsysacl( ).          " Trusting relations
    get_rfcdes( ).             " Trusted desinations
    get_abap_instance_pahi( ). " rfc/selftrust
    get_authorizations( ).     " Authorizations for S_RFCACL

    validate_kernel( ).
    validate_abap( ).
    count_dest_per_trust( ).
    validate_trust_for_dest( ).
    validate_mutual_trust( ).
    validate_authorizations( ).

    show_result( ).

  ENDMETHOD. " start_of_selection

  METHOD get_sap_kernel.
    CHECK p_kern = 'X'.

    " Same as in report ZSHOW_KERNEL_STORES but one one entry per system

    DATA:
      lt_store_dir_tech TYPE  tt_diagst_store_dir_tech,
      lt_store_dir      TYPE  tt_diagst_store_dir,
      lt_fieldlist      TYPE  tt_diagst_table_store_fields,
      lt_snapshot       TYPE  tt_diagst_trows,
      rc                TYPE  i,
      rc_text           TYPE  natxt.

    DATA tabix TYPE i.

    CALL FUNCTION 'DIAGST_GET_STORES'
      EXPORTING
        " The "System Filter" parameters allow to get all Stores of a system or technical system.
        " Some combinations of the four parameters are not allowed.
        " The function will return an error code in such a case.
*       SID                   = ' '
*       INSTALL_NUMBER        = ' '
*       LONG_SID              = ' '
*       TECH_SYSTEM_TYPE      = 'ABAP'                     "(only together with LONG_SID)
        " Store key fields
        group_namespace       = 'ACTIVE'                   "(optional)
        group_landscape_class = 'CL_DIAGLS_ABAP_INSTANCE'  "(optional)
*       GROUP_LANDSCAPE_ID    = ' '
*       GROUP_COMP_ID         = ' '
        group_source          = 'ABAP'                     "(optional)
        group_name            = 'INSTANCE'                 "(optional)
        store_category        = 'SOFTWARE'                 "(optional)
        store_type            = 'PROPERTY'                 "(optional)
*       STORE_FULLPATH        = ' '
        store_name            = 'SAP_KERNEL'
        " Special filters
        store_mainalias       = 'ABAP-SOFTWARE'            "(optional)
        store_subalias        = 'SAP-KERNEL'               "(optional)
*       STORE_TPL_ID          = ' '
*       HAS_ELEMENT_FROM      =                            " date range
*       HAS_ELEMENT_TO        =                            " date range
*       ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*       CASE_INSENSITIVE      = ' '
*       PATTERN_SEARCH        = 'X'                        " Allow pattern search for SEARCH_STRING
*       SEARCH_STRING         =
*       ONLY_RELEVANT         = 'X'
*       PROTECTED             = 'A'                        " (N)ot, (Y)es, (A)ll
        " Others
*       DISPLAY               = ' '                        " Only useful if the function is manually executed by transaction SE37.
        " Setting this parameter to 'X' will display the result.
*       CALLING_APPL          = ' '
      IMPORTING
*       STORE_DIR_TECH        = lt_STORE_DIR_TECH          "(efficient, reduced structure)
        store_dir             = lt_store_dir               "(not recommended anymore)
*       STORE_DIR_MI          =                            "(SAP internal usage only)
*       STORE_STATS           =                            " History regarding the changes of elements (configuration items).
*       PARAMETER             =                            "(SAP internal usage only)
        rc                    = rc
        rc_text               = rc_text.

    IF rc IS NOT INITIAL.
      MESSAGE e001(00) WITH rc_text ##MG_MISSING.
    ENDIF.

    LOOP AT lt_store_dir INTO DATA(ls_store_dir)
      WHERE long_sid              IN p_sid
        AND store_main_state_type IN p_state
      .

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          install_number = ls_store_dir-install_number
          long_sid       = ls_store_dir-long_sid
          sid            = ls_store_dir-sid
          .
      IF sy-subrc = 0.
        tabix = sy-tabix.
        IF ls_store_dir-instance_type NE 'CENTRAL'.
          CONTINUE.
        ENDIF.
        MOVE-CORRESPONDING ls_store_dir TO ls_result.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_store_dir TO ls_result.
      ENDIF.

      IF ls_result-host_full IS INITIAL.
        ls_result-host_full = ls_result-host. " host, host_id, physical_host
      ENDIF.

      CALL FUNCTION 'DIAGST_TABLE_SNAPSHOT'
        EXPORTING
          store_id  = ls_store_dir-store_id
*         TIMESTAMP =                        " if not specified the latest available snapshot is returned
*         CALLING_APPL                = ' '
        IMPORTING
          fieldlist = lt_fieldlist
*         SNAPSHOT_VALID_FROM         =
*         SNAPSHOT_VALID_TO_CONFIRMED =
*         SNAPSHOT_VALID_TO           =
          snapshot  = lt_snapshot            " The content of the requested snapshot in ABAP DDIC type format
*         SNAPSHOT_TR                 =
*         SNAPSHOT_ITSAM              =                        " The content of the requested snapshot in XML-based format
          rc        = rc                     " 3: Permission denied, Content Authorization missing
          " 4: Store not existing
          " 8: Error
          rc_text   = rc_text.

      LOOP AT lt_snapshot INTO DATA(lt_snapshot_elem).
        READ TABLE lt_snapshot_elem INTO DATA(ls_parameter) INDEX 1.
        CHECK ls_parameter-fieldname = 'PARAMETER'.

        READ TABLE lt_snapshot_elem INTO DATA(ls_value)     INDEX 2.
        CHECK ls_value-fieldname = 'VALUE'.

        CASE ls_parameter-fieldvalue.

          WHEN 'KERN_COMP_ON'.      " Linux GNU SLES-11 x86_64 cc4.3.4 use-pr190909
            " not used yet

          WHEN 'KERN_COMP_TIME'.    " Jun  7 2020 15:44:10
            ls_result-kern_comp_time  = ls_value-fieldvalue.
            ls_result-kern_comp_date  = convert_comp_time( ls_result-kern_comp_time ).

          WHEN 'KERN_DBLIB'.        " SQLDBC 7.9.8.040
            " not used yet

          WHEN 'KERN_PATCHLEVEL'.   " 1000
            ls_result-kern_patchlevel = ls_value-fieldvalue.

          WHEN 'KERN_REL'.          " 722_EXT_REL
            ls_result-kern_rel        = ls_value-fieldvalue.

          WHEN 'PLATFORM-ID'.       " 390
            " not used yet

        ENDCASE.

      ENDLOOP.

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_STORE_DIR

  ENDMETHOD. " get_SAP_KERNEL

* Convert text like 'Dec  7 2020' into a date field
  METHOD convert_comp_time.
    "IMPORTING comp_time TYPE string
    "RETURNING VALUE(comp_date) TYPE sy-datum

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
  ENDMETHOD. " convert_comp_time

  METHOD get_abap_comp_splevel.
    CHECK p_abap = 'X'.

    DATA:
      lt_store_dir_tech TYPE  tt_diagst_store_dir_tech,
      lt_store_dir      TYPE  tt_diagst_store_dir,
      lt_fieldlist      TYPE  tt_diagst_table_store_fields,
      lt_snapshot       TYPE  tt_diagst_trows,
      rc                TYPE  i,
      rc_text           TYPE  natxt.

    DATA tabix TYPE i.

    " Using a SEARCH_STRING for SAP_BASIS should not speed up processing, as this component exists always
    CALL FUNCTION 'DIAGST_GET_STORES'
      EXPORTING
        " The "System Filter" parameters allow to get all Stores of a system or technical system.
        " Some combinations of the four parameters are not allowed.
        " The function will return an error code in such a case.
*       SID                   = ' '
*       INSTALL_NUMBER        = ' '
*       LONG_SID              = ' '
*       TECH_SYSTEM_TYPE      = 'ABAP'                     "(only together with LONG_SID)
        " Store key fields
        group_namespace       = 'ACTIVE'                   "(optional)
        group_landscape_class = 'CL_DIAGLS_ABAP_TECH_SYST' "(optional)
*       GROUP_LANDSCAPE_ID    = ' '
*       GROUP_COMP_ID         = ' '
        group_source          = 'ABAP'                     "(optional)
        group_name            = 'ABAP-SOFTWARE'            "(optional)
        store_category        = 'SOFTWARE'                 "(optional)
        store_type            = 'TABLE'                    "(optional)
*       STORE_FULLPATH        = ' '
        store_name            = 'ABAP_COMP_SPLEVEL'
        " Special filters
        store_mainalias       = 'ABAP-SOFTWARE'            "(optional)
        store_subalias        = 'SUPPORT-PACKAGE-LEVEL'    "(optional)
*       STORE_TPL_ID          = ' '
*       HAS_ELEMENT_FROM      =                            " date range
*       HAS_ELEMENT_TO        =                            " date range
*       ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*       CASE_INSENSITIVE      = ' '
        "PATTERN_SEARCH        = ' '                        " Allow pattern search for SEARCH_STRING
        "SEARCH_STRING         = 'SAP_BASIS'
*       ONLY_RELEVANT         = 'X'
*       PROTECTED             = 'A'                        " (N)ot, (Y)es, (A)ll
        " Others
*       DISPLAY               = ' '                        " Only useful if the function is manually executed by transaction SE37.
        " Setting this parameter to 'X' will display the result.
*       CALLING_APPL          = ' '
      IMPORTING
*       STORE_DIR_TECH        = lt_STORE_DIR_TECH          "(efficient, reduced structure)
        store_dir             = lt_store_dir               "(not recommended anymore)
*       STORE_DIR_MI          =                            "(SAP internal usage only)
*       STORE_STATS           =                            " History regarding the changes of elements (configuration items).
*       PARAMETER             =                            "(SAP internal usage only)
        rc                    = rc
        rc_text               = rc_text.

    IF rc IS NOT INITIAL.
      MESSAGE e001(00) WITH rc_text ##MG_MISSING.
    ENDIF.

    LOOP AT lt_store_dir INTO DATA(ls_store_dir)
      WHERE long_sid              IN p_sid
        AND store_main_state_type IN p_state
      .

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          install_number = ls_store_dir-install_number
          long_sid       = ls_store_dir-long_sid
          sid            = ls_store_dir-sid
          .
      IF sy-subrc = 0.
        tabix = sy-tabix.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_store_dir TO ls_result.
      ENDIF.

      CALL FUNCTION 'DIAGST_TABLE_SNAPSHOT'
        EXPORTING
          store_id  = ls_store_dir-store_id
*         TIMESTAMP =                        " if not specified the latest available snapshot is returned
*         CALLING_APPL                = ' '
        IMPORTING
          fieldlist = lt_fieldlist
*         SNAPSHOT_VALID_FROM         =
*         SNAPSHOT_VALID_TO_CONFIRMED =
*         SNAPSHOT_VALID_TO           =
          snapshot  = lt_snapshot            " The content of the requested snapshot in ABAP DDIC type format
*         SNAPSHOT_TR                 =
*         SNAPSHOT_ITSAM              =                        " The content of the requested snapshot in XML-based format
          rc        = rc                     " 3: Permission denied, Content Authorization missing
          " 4: Store not existing
          " 8: Error
          rc_text   = rc_text.

      LOOP AT lt_snapshot INTO DATA(lt_snapshot_elem).
        READ TABLE lt_snapshot_elem INTO DATA(ls_component)  INDEX 1.
        CHECK ls_component-fieldname = 'COMPONENT'.
        CHECK ls_component-fieldvalue = 'SAP_BASIS'.

        READ TABLE lt_snapshot_elem INTO DATA(ls_release)    INDEX 2.
        CHECK ls_release-fieldname = 'RELEASE'.
        ls_result-abap_release = ls_release-fieldvalue.

        READ TABLE lt_snapshot_elem INTO DATA(ls_extrelease) INDEX 3.
        CHECK ls_extrelease-fieldname = 'EXTRELEASE'.
        ls_result-abap_sp      = ls_extrelease-fieldvalue.
      ENDLOOP.

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_STORE_DIR

  ENDMETHOD. " get_ABAP_COMP_SPLEVEL

  METHOD get_abap_notes.
    CHECK p_abap = 'X'.

    DATA:
      lt_store_dir_tech TYPE  tt_diagst_store_dir_tech,
      lt_store_dir      TYPE  tt_diagst_store_dir,
      lt_fieldlist      TYPE  tt_diagst_table_store_fields,
      lt_snapshot       TYPE  tt_diagst_trows,
      rc                TYPE  i,
      rc_text           TYPE  natxt.

    DATA tabix TYPE i.

    " Maybe it' faster to call it twice including a SEARCH_STRING for both note numbers.
    CALL FUNCTION 'DIAGST_GET_STORES'
      EXPORTING
        " The "System Filter" parameters allow to get all Stores of a system or technical system.
        " Some combinations of the four parameters are not allowed.
        " The function will return an error code in such a case.
*       SID                   = ' '
*       INSTALL_NUMBER        = ' '
*       LONG_SID              = ' '
*       TECH_SYSTEM_TYPE      = 'ABAP'                     "(only together with LONG_SID)
        " Store key fields
        group_namespace       = 'ACTIVE'                   "(optional)
        group_landscape_class = 'CL_DIAGLS_ABAP_TECH_SYST' "(optional)
*       GROUP_LANDSCAPE_ID    = ' '
*       GROUP_COMP_ID         = ' '
        group_source          = 'ABAP'                     "(optional)
        group_name            = 'ABAP-SOFTWARE'            "(optional)
        store_category        = 'SOFTWARE'                 "(optional)
        store_type            = 'TABLE'                    "(optional)
*       STORE_FULLPATH        = ' '
        store_name            = 'ABAP_NOTES'
        " Special filters
        store_mainalias       = 'ABAP-SOFTWARE'            "(optional)
        store_subalias        = 'ABAP-NOTES'               "(optional)
*       STORE_TPL_ID          = ' '
*       HAS_ELEMENT_FROM      =                            " date range
*       HAS_ELEMENT_TO        =                            " date range
*       ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*       CASE_INSENSITIVE      = ' '
*       PATTERN_SEARCH        = 'X'                        " Allow pattern search for SEARCH_STRING
*       SEARCH_STRING         =
*       ONLY_RELEVANT         = 'X'
*       PROTECTED             = 'A'                        " (N)ot, (Y)es, (A)ll
        " Others
*       DISPLAY               = ' '                        " Only useful if the function is manually executed by transaction SE37.
        " Setting this parameter to 'X' will display the result.
*       CALLING_APPL          = ' '
      IMPORTING
*       STORE_DIR_TECH        = lt_STORE_DIR_TECH          "(efficient, reduced structure)
        store_dir             = lt_store_dir               "(not recommended anymore)
*       STORE_DIR_MI          =                            "(SAP internal usage only)
*       STORE_STATS           =                            " History regarding the changes of elements (configuration items).
*       PARAMETER             =                            "(SAP internal usage only)
        rc                    = rc
        rc_text               = rc_text.

    IF rc IS NOT INITIAL.
      MESSAGE e001(00) WITH rc_text ##MG_MISSING.
    ENDIF.

    LOOP AT lt_store_dir INTO DATA(ls_store_dir)
      WHERE long_sid              IN p_sid
        AND store_main_state_type IN p_state
      .

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          install_number = ls_store_dir-install_number
          long_sid       = ls_store_dir-long_sid
          sid            = ls_store_dir-sid
          .
      IF sy-subrc = 0.
        tabix = sy-tabix.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_store_dir TO ls_result.
      ENDIF.

      CALL FUNCTION 'DIAGST_TABLE_SNAPSHOT'
        EXPORTING
          store_id  = ls_store_dir-store_id
*         TIMESTAMP =                        " if not specified the latest available snapshot is returned
*         CALLING_APPL                = ' '
        IMPORTING
          fieldlist = lt_fieldlist
*         SNAPSHOT_VALID_FROM         =
*         SNAPSHOT_VALID_TO_CONFIRMED =
*         SNAPSHOT_VALID_TO           =
          snapshot  = lt_snapshot            " The content of the requested snapshot in ABAP DDIC type format
*         SNAPSHOT_TR                 =
*         SNAPSHOT_ITSAM              =                        " The content of the requested snapshot in XML-based format
          rc        = rc                     " 3: Permission denied, Content Authorization missing
          " 4: Store not existing
          " 8: Error
          rc_text   = rc_text.

      LOOP AT lt_snapshot INTO DATA(lt_snapshot_elem).
        READ TABLE lt_snapshot_elem INTO DATA(ls_note)      INDEX 1. "
        CHECK ls_note-fieldname = 'NOTE'.
        CHECK ls_note-fieldvalue = '0003089413'
           OR ls_note-fieldvalue = '0003287611'
           OR ls_note-fieldvalue = '0003304520'
           .

        READ TABLE lt_snapshot_elem INTO DATA(ls_version)   INDEX 2. "
        CHECK ls_version-fieldname = 'VERSION'.

        "READ TABLE lt_snapshot_elem INTO data(ls_TEXT)      INDEX 3. "
        "check ls_TEXT-fieldname = 'TEXT'.

        READ TABLE lt_snapshot_elem INTO DATA(ls_prstatust) INDEX 4. "
        CHECK ls_prstatust-fieldname = 'PRSTATUST'.

        READ TABLE lt_snapshot_elem INTO DATA(ls_prstatus)  INDEX 5. "
        CHECK ls_prstatus-fieldname = 'PRSTATUS'.

        DATA(status) = ls_prstatust-fieldvalue && ` version ` && ls_version-fieldvalue.
        CASE ls_note-fieldvalue.
          WHEN '0003089413'.
            ls_result-note_3089413 = status.
            ls_result-note_3089413_prstatus = ls_prstatus-fieldvalue.
          WHEN '0003287611'.
            ls_result-note_3287611 = status.
            ls_result-note_3287611_prstatus = ls_prstatus-fieldvalue.
          WHEN '0003304520'.
            ls_result-note_3304520 = status.
            ls_result-note_3304520_prstatus = ls_prstatus-fieldvalue.
        ENDCASE.

      ENDLOOP.

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_STORE_DIR

  ENDMETHOD. " get_ABAP_NOTES

  METHOD get_rfcsysacl.
    CHECK p_trust = 'X'.

    DATA:
      lt_store_dir_tech TYPE  tt_diagst_store_dir_tech,
      lt_store_dir      TYPE  tt_diagst_store_dir,
      lt_fieldlist      TYPE  tt_diagst_table_store_fields,
      lt_snapshot       TYPE  tt_diagst_trows,
      rc                TYPE  i,
      rc_text           TYPE  natxt.

    DATA tabix TYPE i.

    CALL FUNCTION 'DIAGST_GET_STORES'
      EXPORTING
        " The "System Filter" parameters allow to get all Stores of a system or technical system.
        " Some combinations of the four parameters are not allowed.
        " The function will return an error code in such a case.
*       SID                   = ' '
*       INSTALL_NUMBER        = ' '
*       LONG_SID              = ' '
*       TECH_SYSTEM_TYPE      = 'ABAP'                     "(only together with LONG_SID)
        " Store key fields
        group_namespace       = 'ACTIVE'                   "(optional)
        group_landscape_class = 'CL_DIAGLS_ABAP_TECH_SYST' "(optional)
*       GROUP_LANDSCAPE_ID    = ' '
*       GROUP_COMP_ID         = ' '
        group_source          = 'ABAP'                     "(optional)
        group_name            = 'ABAP-SECURITY'            "(optional)
        store_category        = 'CONFIG'                   "(optional)
        store_type            = 'TABLE'                    "(optional)
*       STORE_FULLPATH        = ' '
        store_name            = 'RFCSYSACL'
        " Special filters
        store_mainalias       = 'SECURITY'                 "(optional)
        store_subalias        = 'TRUSTED RFC'              "(optional)
*       STORE_TPL_ID          = ' '
*       HAS_ELEMENT_FROM      =                            " date range
*       HAS_ELEMENT_TO        =                            " date range
*       ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*       CASE_INSENSITIVE      = ' '
*       PATTERN_SEARCH        = 'X'                        " Allow pattern search for SEARCH_STRING
*       SEARCH_STRING         =
*       ONLY_RELEVANT         = 'X'
*       PROTECTED             = 'A'                        " (N)ot, (Y)es, (A)ll
        " Others
*       DISPLAY               = ' '                        " Only useful if the function is manually executed by transaction SE37.
        " Setting this parameter to 'X' will display the result.
*       CALLING_APPL          = ' '
      IMPORTING
*       STORE_DIR_TECH        = lt_STORE_DIR_TECH          "(efficient, reduced structure)
        store_dir             = lt_store_dir               "(not recommended anymore)
*       STORE_DIR_MI          =                            "(SAP internal usage only)
*       STORE_STATS           =                            " History regarding the changes of elements (configuration items).
*       PARAMETER             =                            "(SAP internal usage only)
        rc                    = rc
        rc_text               = rc_text.

    IF rc IS NOT INITIAL.
      MESSAGE e001(00) WITH rc_text ##MG_MISSING.
    ENDIF.

    LOOP AT lt_store_dir INTO DATA(ls_store_dir)
      WHERE long_sid              IN p_sid
        AND store_main_state_type IN p_state
      .

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          install_number = ls_store_dir-install_number
          long_sid       = ls_store_dir-long_sid
          sid            = ls_store_dir-sid
          .
      IF sy-subrc = 0.
        tabix = sy-tabix.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_store_dir TO ls_result.
      ENDIF.

      CALL FUNCTION 'DIAGST_TABLE_SNAPSHOT'
        EXPORTING
          store_id  = ls_store_dir-store_id
*         TIMESTAMP =                        " if not specified the latest available snapshot is returned
*         CALLING_APPL                = ' '
        IMPORTING
          fieldlist = lt_fieldlist
*         SNAPSHOT_VALID_FROM         =
*         SNAPSHOT_VALID_TO_CONFIRMED =
*         SNAPSHOT_VALID_TO           =
          snapshot  = lt_snapshot            " The content of the requested snapshot in ABAP DDIC type format
*         SNAPSHOT_TR                 =
*         SNAPSHOT_ITSAM              =                        " The content of the requested snapshot in XML-based format
          rc        = rc                     " 3: Permission denied, Content Authorization missing
          " 4: Store not existing
          " 8: Error
          rc_text   = rc_text.

      " Store RFCSYSACL data
      DATA ls_trusted_system  TYPE ts_trusted_system.
      CLEAR ls_trusted_system.
      ls_trusted_system-rfctrustsy  = ls_store_dir-sid.
      ls_trusted_system-llicense_nr = ls_store_dir-install_number.

      LOOP AT lt_snapshot INTO DATA(lt_snapshot_elem).

        " Store RFCSYSACL data
        DATA ls_rfcsysacl_data TYPE ts_rfcsysacl_data.
        CLEAR ls_rfcsysacl_data.

        LOOP AT lt_snapshot_elem INTO DATA(snapshot_elem).
          IF snapshot_elem-fieldvalue = '<CCDB NULL>'.
            CLEAR snapshot_elem-fieldvalue.
          ENDIF.

          CASE snapshot_elem-fieldname.
            WHEN 'RFCSYSID'.    ls_rfcsysacl_data-rfcsysid    = snapshot_elem-fieldvalue. " 1
            WHEN 'TLICENSE_NR'. ls_rfcsysacl_data-tlicense_nr = snapshot_elem-fieldvalue. " 2
            WHEN 'RFCTRUSTSY'.  ls_rfcsysacl_data-rfctrustsy  = snapshot_elem-fieldvalue. " 3
            WHEN 'RFCDEST'.     ls_rfcsysacl_data-rfcdest     = snapshot_elem-fieldvalue. " 4
            WHEN 'RFCTCDCHK'.   ls_rfcsysacl_data-rfctcdchk   = snapshot_elem-fieldvalue. " 5
            WHEN 'RFCSNC'.      ls_rfcsysacl_data-rfcsnc      = snapshot_elem-fieldvalue. " 6
            WHEN 'RFCSLOPT'.    ls_rfcsysacl_data-rfcslopt    = snapshot_elem-fieldvalue. " 7
              " only available in higher versions
            WHEN 'RFCCREDEST'.  ls_rfcsysacl_data-rfccredest  = snapshot_elem-fieldvalue. " 8
            WHEN 'RFCREGDEST'.  ls_rfcsysacl_data-rfcregdest  = snapshot_elem-fieldvalue. " 9
            WHEN 'LLICENSE_NR'. ls_rfcsysacl_data-llicense_nr = snapshot_elem-fieldvalue. " 10
            WHEN 'RFCSECKEY'.   ls_rfcsysacl_data-rfcseckey   = snapshot_elem-fieldvalue. " 11
          ENDCASE.
        ENDLOOP.

        " Add installation number
        IF ls_rfcsysacl_data-llicense_nr IS INITIAL.
          ls_rfcsysacl_data-llicense_nr = ls_trusted_system-llicense_nr.
        ENDIF.

        " Store RFCSYSACL data
        APPEND ls_rfcsysacl_data TO ls_trusted_system-rfcsysacl_data.

        ADD 1 TO ls_result-trustsy_cnt_all.

        IF ls_rfcsysacl_data-rfctcdchk IS NOT INITIAL.
          ADD 1 TO ls_result-trustsy_cnt_tcd.
        ENDIF.

        " Get version
        DATA version(1).
        version = ls_rfcsysacl_data-rfcslopt. " get first char of the string
        CASE version.
          WHEN '3'. ADD 1 TO ls_result-trustsy_cnt_3.
          WHEN '2'. ADD 1 TO ls_result-trustsy_cnt_2.
          WHEN ' '. ADD 1 TO ls_result-trustsy_cnt_1.
        ENDCASE.

        " Identify selftrust
        IF ls_rfcsysacl_data-rfcsysid = ls_rfcsysacl_data-rfctrustsy.
          IF ls_rfcsysacl_data-llicense_nr IS NOT INITIAL AND ls_rfcsysacl_data-llicense_nr = ls_rfcsysacl_data-tlicense_nr.
            ls_result-explicit_selftrust = 'Explicit selftrust'.
          ELSE.
            ls_result-explicit_selftrust = 'explicit selftrust'. " no check for installation number
          ENDIF.
        ENDIF.

      ENDLOOP.

      " Store trusted systems
      APPEND ls_trusted_system TO lt_trusted_systems.

      " Store result
      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_STORE_DIR

  ENDMETHOD. " get_RFCSYSACL

  METHOD get_rfcdes.
    CHECK p_dest = 'X'.

    DATA:
      lt_store_dir_tech TYPE  tt_diagst_store_dir_tech,
      lt_store_dir      TYPE  tt_diagst_store_dir,
      lt_fieldlist      TYPE  tt_diagst_table_store_fields,
      lt_snapshot       TYPE  tt_diagst_trows,
      rc                TYPE  i,
      rc_text           TYPE  natxt.

    DATA tabix TYPE i.

    CALL FUNCTION 'DIAGST_GET_STORES'
      EXPORTING
        " The "System Filter" parameters allow to get all Stores of a system or technical system.
        " Some combinations of the four parameters are not allowed.
        " The function will return an error code in such a case.
*       SID                   = ' '
*       INSTALL_NUMBER        = ' '
*       LONG_SID              = ' '
*       TECH_SYSTEM_TYPE      = 'ABAP'                     "(only together with LONG_SID)
        " Store key fields
        group_namespace       = 'ACTIVE'                   "(optional)
        group_landscape_class = 'CL_DIAGLS_ABAP_TECH_SYST' "(optional)
*       GROUP_LANDSCAPE_ID    = ' '
*       GROUP_COMP_ID         = ' '
        group_source          = 'ABAP'                     "(optional)
        group_name            = 'RFC-DESTINATIONS'         "(optional)
        store_category        = 'CONFIG'                   "(optional)
        store_type            = 'TABLE'                    "(optional)
*       STORE_FULLPATH        = ' '
        store_name            = 'RFCDES'
        " Special filters
        store_mainalias       = 'RFC-DESTINATIONS'         "(optional)
        store_subalias        = 'RFCDES'                   "(optional)
*       STORE_TPL_ID          = ' '
*       HAS_ELEMENT_FROM      =                            " date range
*       HAS_ELEMENT_TO        =                            " date range
*       ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*       CASE_INSENSITIVE      = ' '
*       PATTERN_SEARCH        = 'X'                        " Allow pattern search for SEARCH_STRING
*       SEARCH_STRING         =
*       ONLY_RELEVANT         = 'X'
*       PROTECTED             = 'A'                        " (N)ot, (Y)es, (A)ll
        " Others
*       DISPLAY               = ' '                        " Only useful if the function is manually executed by transaction SE37.
        " Setting this parameter to 'X' will display the result.
*       CALLING_APPL          = ' '
      IMPORTING
*       STORE_DIR_TECH        = lt_STORE_DIR_TECH          "(efficient, reduced structure)
        store_dir             = lt_store_dir               "(not recommended anymore)
*       STORE_DIR_MI          =                            "(SAP internal usage only)
*       STORE_STATS           =                            " History regarding the changes of elements (configuration items).
*       PARAMETER             =                            "(SAP internal usage only)
        rc                    = rc
        rc_text               = rc_text.

    IF rc IS NOT INITIAL.
      MESSAGE e001(00) WITH rc_text ##MG_MISSING.
    ENDIF.

    LOOP AT lt_store_dir INTO DATA(ls_store_dir)
      WHERE long_sid              IN p_sid
        AND store_main_state_type IN p_state
        .

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          install_number = ls_store_dir-install_number
          long_sid       = ls_store_dir-long_sid
          sid            = ls_store_dir-sid
          .
      IF sy-subrc = 0.
        tabix = sy-tabix.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_store_dir TO ls_result.
      ENDIF.

      " Store destination data
      DATA ls_destination  TYPE ts_destination.
      CLEAR ls_destination.
      ls_destination-sid            = ls_store_dir-sid.
      ls_destination-install_number = ls_store_dir-install_number.

      CALL FUNCTION 'DIAGST_TABLE_SNAPSHOT'
        EXPORTING
          store_id  = ls_store_dir-store_id
*         TIMESTAMP =                        " if not specified the latest available snapshot is returned
*         CALLING_APPL                = ' '
        IMPORTING
          fieldlist = lt_fieldlist
*         SNAPSHOT_VALID_FROM         =
*         SNAPSHOT_VALID_TO_CONFIRMED =
*         SNAPSHOT_VALID_TO           =
          snapshot  = lt_snapshot            " The content of the requested snapshot in ABAP DDIC type format
*         SNAPSHOT_TR                 =
*         SNAPSHOT_ITSAM              =                        " The content of the requested snapshot in XML-based format
          rc        = rc                     " 3: Permission denied, Content Authorization missing
          " 4: Store not existing
          " 8: Error
          rc_text   = rc_text.

      LOOP AT lt_snapshot INTO DATA(lt_snapshot_elem).
        READ TABLE lt_snapshot_elem INTO DATA(ls_rfcdest)       INDEX 1.
        CHECK ls_rfcdest-fieldname = 'RFCDEST'.

        READ TABLE lt_snapshot_elem INTO DATA(ls_rfctype)       INDEX 2.
        CHECK ls_rfctype-fieldname = 'RFCTYPE'.
        CHECK ls_rfctype-fieldvalue = '3' OR  ls_rfctype-fieldvalue = 'H' OR ls_rfctype-fieldvalue =  'W'.

        READ TABLE lt_snapshot_elem INTO DATA(ls_rfcoptions)    INDEX 3.
        CHECK ls_rfcoptions-fieldname = 'RFCOPTIONS'.

        " Store destination data
        DATA ls_destination_data TYPE ts_destination_data.
        CLEAR ls_destination_data.
        ls_destination_data-rfcdest = ls_rfcdest-fieldvalue.
        ls_destination_data-rfctype = ls_rfctype-fieldvalue.

        " Interpret tokens similar to function RFCDES2RFCDISPLAY (multiple FIND REGEX are slower that SPLIT+LOOP+CASE)
*        FIND REGEX 'Q=(Y),' IN ls_rfcoptions-fieldvalue              " Trusted
*          SUBMATCHES ls_destination_data-trusted.
*        IF ls_destination_data-trusted = 'Y'.
*          ls_destination_data-trusted = 'X'. " show checkbox instead of value
*        ENDIF.
*        FIND REGEX 'Q=([^,]*),'       IN ls_rfcoptions-fieldvalue    " Logon Procedure (Y = Trusted)
*          SUBMATCHES ls_destination_data-rfcslogin.
*
*        FIND REGEX '\[=([^,]{3}),'    IN ls_rfcoptions-fieldvalue    " System ID for trusted connection
*          SUBMATCHES ls_destination_data-serversysid.
*
*        FIND REGEX '\^=([^,]{1,10}),' IN ls_rfcoptions-fieldvalue    " Installation number for trusted connection
*          SUBMATCHES ls_destination_data-serverinstnr.
*
*
*        FIND REGEX 'H=([^,]*),'       IN ls_rfcoptions-fieldvalue    " Name of Target Host
*          SUBMATCHES ls_destination_data-rfchost.
*
*        FIND REGEX 'S=([^,]*),'       IN ls_rfcoptions-fieldvalue    " Service used (TCP service, SAP System number)
*          SUBMATCHES ls_destination_data-rfcservice.
*
*
*        FIND REGEX 'I=([^,]*),'       IN ls_rfcoptions-fieldvalue    " System ID
*          SUBMATCHES ls_destination_data-rfcsysid.
*
*        FIND REGEX 'M=([^,]*),'       IN ls_rfcoptions-fieldvalue    " Explicit logon client
*          SUBMATCHES ls_destination_data-rfcclient.
*
*        FIND REGEX 'U=([^,]*),'       IN ls_rfcoptions-fieldvalue    " Explicit user
*          SUBMATCHES ls_destination_data-rfcuser.
*        CONSTANTS logon_screen_token(8) VALUE '%_LOG01%'.
*        IF ls_destination_data-rfcuser = logon_screen_token.
*          ls_destination_data-rfcuser = 'logon screen'.
*        ENDIF.
*
*        FIND REGEX 'u=(Y),'           IN ls_rfcoptions-fieldvalue    " Same user flag
*          SUBMATCHES ls_destination_data-rfcsameusr.
*        IF ls_destination_data-rfcsameusr = 'Y'.
*          ls_destination_data-rfcsameusr = 'X'. " show checkbox instead of value
*
*          ls_destination_data-rfcuser = 'same user'.
*        ENDIF.
*
*        FIND REGEX 'v=([^,]*),'       IN ls_rfcoptions-fieldvalue    " Explicit password
*          SUBMATCHES ls_destination_data-rfcauth.
*        FIND REGEX 'V=([^,]*),'       IN ls_rfcoptions-fieldvalue    " Explicit password
*          SUBMATCHES ls_destination_data-rfcauth.
*        FIND REGEX 'P=([^,]*),'       IN ls_rfcoptions-fieldvalue    " Explicit password
*          SUBMATCHES ls_destination_data-rfcauth.
*        CONSTANTS sec_storage_token(5) VALUE '%_PWD'.
*        IF ls_destination_data-rfcauth = sec_storage_token.
*          ls_destination_data-rfcauth = 'stored password'.
*        ENDIF.
*
*        FIND REGEX 's=(Y),'           IN ls_rfcoptions-fieldvalue    " SNC/TLS
*          SUBMATCHES ls_destination_data-rfcsnc.
*        IF ls_destination_data-rfcsnc = 'Y'.
*          ls_destination_data-rfcsnc = 'X'. " show checkbox instead of value
*        ENDIF.
*        FIND REGEX 't=([^,]*),'       IN ls_rfcoptions-fieldvalue    " SSL Client Identity
*          SUBMATCHES ls_destination_data-sslapplic.
*        FIND REGEX '/=([^,]*),'       IN ls_rfcoptions-fieldvalue    " No client cert
*          SUBMATCHES ls_destination_data-noccert.
        SPLIT ls_rfcoptions-fieldvalue AT ',' INTO TABLE DATA(tokens).
        LOOP AT tokens INTO DATA(token).
          SPLIT token AT '=' INTO DATA(key) DATA(value).
          CASE key.

            WHEN 'Q'.
              MOVE value TO ls_destination_data-rfcslogin.     " Logon Procedure (Y = Trusted)
              IF value = 'Y'.
                ls_destination_data-trusted = 'X'.             " show checkbox instead of value
              ENDIF.
            WHEN '['. MOVE value TO ls_destination_data-serversysid.   " System ID for trusted connection
            WHEN '^'. MOVE value TO ls_destination_data-serverinstnr.  " Installation number for trusted connection
            WHEN 'H'. MOVE value TO ls_destination_data-rfchost.       " Name of Target Host
            WHEN 'S'. MOVE value TO ls_destination_data-rfcservice.    " Service used (TCP service, SAP System number)
            WHEN 'I'. MOVE value TO ls_destination_data-rfcsysid.      " System ID
            WHEN 'M'. MOVE value TO ls_destination_data-rfcclient.     " Explicit logon client

            WHEN 'U'.
              MOVE value TO ls_destination_data-rfcuser.       " Explicit user
              CONSTANTS logon_screen_token(8) VALUE '%_LOG01%'.
              IF ls_destination_data-rfcuser = logon_screen_token.
                ls_destination_data-rfcuser = 'logon screen'.
              ENDIF.

            WHEN 'u'.
              MOVE value TO ls_destination_data-rfcsameusr.    " Same user flag
              IF ls_destination_data-rfcsameusr = 'Y'.
                ls_destination_data-rfcsameusr = 'X'.          " show checkbox instead of value
                ls_destination_data-rfcuser = 'same user'.
              ENDIF.
            WHEN 'v' OR 'V' OR 'P'.
              MOVE value TO ls_destination_data-rfcauth.       " Explicit password
              CONSTANTS sec_storage_token(5) VALUE '%_PWD'.
              IF ls_destination_data-rfcauth = sec_storage_token.
                ls_destination_data-rfcauth = 'stored password'.
              ENDIF.

            WHEN 's'.
              MOVE value TO ls_destination_data-rfcsnc.        " SNC/TLS
              IF ls_destination_data-rfcsnc = 'Y'.
                ls_destination_data-rfcsnc = 'X'.              " show checkbox instead of value
              ENDIF.
            WHEN 't'. MOVE value TO ls_destination_data-sslapplic.     " SSL Client Identity
            WHEN '/'. MOVE value TO ls_destination_data-noccert.       " No client cert
          ENDCASE.
        ENDLOOP.

        APPEND ls_destination_data TO ls_destination-destination_data.

        " Update count
        CASE ls_destination_data-rfctype.

          WHEN '3'. " RFC destinations
            p_dest_3 = 'X'.
            ADD 1 TO ls_result-dest_3_cnt_all.                         " All destinations

            IF ls_destination_data-trusted IS NOT INITIAL.             " Trusted destination
              ADD 1 TO ls_result-dest_3_cnt_trusted.

              IF ls_destination_data-serversysid IS NOT INITIAL.
                IF ls_destination_data-serverinstnr IS NOT INITIAL.
                  " System ID and installation number are available
                  ADD 1 TO ls_result-dest_3_cnt_trusted_migrated.
                ELSE.
                  " Installation number is missing
                  ADD 1 TO ls_result-dest_3_cnt_trusted_no_instnr.
                ENDIF.
              ELSE.
                " System ID is missing
                ADD 1 TO ls_result-dest_3_cnt_trusted_no_sysid.
              ENDIF.

              IF ls_destination_data-rfcsnc IS NOT INITIAL.
                ADD 1 TO ls_result-dest_3_cnt_trusted_snc.
              ENDIF.
            ENDIF.

          WHEN 'H'. " http destinations
            p_dest_h = 'X'.
            ADD 1 TO ls_result-dest_h_cnt_all.                             " All destinations

            IF ls_destination_data-trusted IS NOT INITIAL.             " Trusted destination
              ADD 1 TO ls_result-dest_h_cnt_trusted.

              IF ls_destination_data-serversysid IS NOT INITIAL.
                IF ls_destination_data-serverinstnr IS NOT INITIAL.
                  " System ID and installation number are available
                  ADD 1 TO ls_result-dest_h_cnt_trusted_migrated.
                ELSE.
                  " Installation number is missing
                  ADD 1 TO ls_result-dest_h_cnt_trusted_no_instnr.
                ENDIF.
              ELSE.
                " System ID is missing
                ADD 1 TO ls_result-dest_h_cnt_trusted_no_sysid.
              ENDIF.

              IF ls_destination_data-rfcsnc IS NOT INITIAL.
                ADD 1 TO ls_result-dest_h_cnt_trusted_tls.
              ENDIF.
            ENDIF.

          WHEN 'W'. " web RFC destinations
            p_dest_w = 'X'.
            ADD 1 TO ls_result-dest_w_cnt_all.                             " All destinations

            IF ls_destination_data-trusted IS NOT INITIAL.             " Trusted destination
              ADD 1 TO ls_result-dest_w_cnt_trusted.

              IF ls_destination_data-serversysid IS NOT INITIAL.
                IF ls_destination_data-serverinstnr IS NOT INITIAL.
                  " System ID and installation number are available
                  ADD 1 TO ls_result-dest_w_cnt_trusted_migrated.
                ELSE.
                  " Installation number is missing
                  ADD 1 TO ls_result-dest_w_cnt_trusted_no_instnr.
                ENDIF.
              ELSE.
                " System ID is missing
                ADD 1 TO ls_result-dest_w_cnt_trusted_no_sysid.
              ENDIF.

              IF ls_destination_data-rfcsnc IS NOT INITIAL.
                ADD 1 TO ls_result-dest_w_cnt_trusted_tls.
              ENDIF.
            ENDIF.

        ENDCASE.

      ENDLOOP.

      " Store destination data
      APPEND ls_destination TO lt_destinations.

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_STORE_DIR

  ENDMETHOD. " get_RFCDES

  METHOD get_abap_instance_pahi.
    CHECK p_trust = 'X' OR p_dest = 'X'.

    " Same as in report ZSHOW_KERNEL_STORES but one one entry per system

    DATA:
      lt_store_dir_tech TYPE  tt_diagst_store_dir_tech,
      lt_store_dir      TYPE  tt_diagst_store_dir,
      lt_fieldlist      TYPE  tt_diagst_table_store_fields,
      lt_snapshot       TYPE  tt_diagst_trows,
      rc                TYPE  i,
      rc_text           TYPE  natxt.

    DATA tabix TYPE i.

    CALL FUNCTION 'DIAGST_GET_STORES'
      EXPORTING
        " The "System Filter" parameters allow to get all Stores of a system or technical system.
        " Some combinations of the four parameters are not allowed.
        " The function will return an error code in such a case.
*       SID                   = ' '
*       INSTALL_NUMBER        = ' '
*       LONG_SID              = ' '
*       TECH_SYSTEM_TYPE      = 'ABAP'                     "(only together with LONG_SID)
        " Store key fields
        group_namespace       = 'ACTIVE'                   "(optional)
        group_landscape_class = 'CL_DIAGLS_ABAP_INSTANCE'  "(optional)
*       GROUP_LANDSCAPE_ID    = ' '
*       GROUP_COMP_ID         = ' '
        group_source          = 'ABAP'                     "(optional)
        group_name            = 'INSTANCE'                 "(optional)
        store_category        = 'CONFIG'                   "(optional)
        store_type            = 'PROPERTY'                 "(optional)
*       STORE_FULLPATH        = ' '
        store_name            = 'ABAP_INSTANCE_PAHI'
        " Special filters
        store_mainalias       = 'ABAP-PARAMETER'           "(optional)
        store_subalias        = 'PAHI'                     "(optional)
*       STORE_TPL_ID          = ' '
*       HAS_ELEMENT_FROM      =                            " date range
*       HAS_ELEMENT_TO        =                            " date range
*       ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*       CASE_INSENSITIVE      = ' '
*       PATTERN_SEARCH        = 'X'                        " Allow pattern search for SEARCH_STRING
*       SEARCH_STRING         =
*       ONLY_RELEVANT         = 'X'
*       PROTECTED             = 'A'                        " (N)ot, (Y)es, (A)ll
        " Others
*       DISPLAY               = ' '                        " Only useful if the function is manually executed by transaction SE37.
        " Setting this parameter to 'X' will display the result.
*       CALLING_APPL          = ' '
      IMPORTING
*       STORE_DIR_TECH        = lt_STORE_DIR_TECH          "(efficient, reduced structure)
        store_dir             = lt_store_dir               "(not recommended anymore)
*       STORE_DIR_MI          =                            "(SAP internal usage only)
*       STORE_STATS           =                            " History regarding the changes of elements (configuration items).
*       PARAMETER             =                            "(SAP internal usage only)
        rc                    = rc
        rc_text               = rc_text.

    IF rc IS NOT INITIAL.
      MESSAGE e001(00) WITH rc_text ##MG_MISSING.
    ENDIF.

    LOOP AT lt_store_dir INTO DATA(ls_store_dir)
      WHERE long_sid              IN p_sid
        AND store_main_state_type IN p_state
      .

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          install_number = ls_store_dir-install_number
          long_sid       = ls_store_dir-long_sid
          sid            = ls_store_dir-sid
          .
      IF sy-subrc = 0.
        tabix = sy-tabix.
        IF ls_store_dir-instance_type NE 'CENTRAL'.
          CONTINUE.
        ENDIF.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_store_dir TO ls_result.
      ENDIF.

      IF ls_result-host_full IS INITIAL.
        ls_result-host_full = ls_result-host. " host, host_id, physical_host
      ENDIF.

      CALL FUNCTION 'DIAGST_TABLE_SNAPSHOT'
        EXPORTING
          store_id  = ls_store_dir-store_id
*         TIMESTAMP =                        " if not specified the latest available snapshot is returned
*         CALLING_APPL                = ' '
        IMPORTING
          fieldlist = lt_fieldlist
*         SNAPSHOT_VALID_FROM         =
*         SNAPSHOT_VALID_TO_CONFIRMED =
*         SNAPSHOT_VALID_TO           =
          snapshot  = lt_snapshot            " The content of the requested snapshot in ABAP DDIC type format
*         SNAPSHOT_TR                 =
*         SNAPSHOT_ITSAM              =                        " The content of the requested snapshot in XML-based format
          rc        = rc                     " 3: Permission denied, Content Authorization missing
          " 4: Store not existing
          " 8: Error
          rc_text   = rc_text.

      LOOP AT lt_snapshot INTO DATA(lt_snapshot_elem).
        READ TABLE lt_snapshot_elem INTO DATA(ls_parameter) INDEX 1.
        CHECK ls_parameter-fieldname = 'PARAMETER'.

        READ TABLE lt_snapshot_elem INTO DATA(ls_value)     INDEX 2.
        CHECK ls_value-fieldname = 'VALUE'.

        CASE ls_parameter-fieldvalue.
          WHEN 'rfc/selftrust'.         ls_result-rfc_selftrust         = ls_value-fieldvalue.
          WHEN 'rfc/allowoldticket4tt'. ls_result-rfc_allowoldticket4tt = ls_value-fieldvalue.
          WHEN 'rfc/sendInstNr4tt'.     ls_result-rfc_sendinstnr4tt     = ls_value-fieldvalue.
        ENDCASE.

      ENDLOOP.

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_STORE_DIR

  ENDMETHOD. " get_ABAP_INSTANCE_PAHI

  METHOD get_authorizations.
    CHECK p_auth = 'X'.

* Load data from configuration store AUTH_COMB_CHECK_USER about authorization object S_RFCACL
* A similar check could use store AUTH_COMB_CHECK_ROLE
* Prerequisite: The active customizing for these stores in transaction CCDB has to contain entries for combination id S_RFCACL:
* Combination id, Authorization id, Group, Object,  Field name, From, To, And/Or
* S_RFCACL        S_RFCACL          STAR   S_RFCACL RFC_SYSID   #*        OR
* S_RFCACL        S_RFCACL          STAR   S_RFCACL RFC_CLIENT  #*        OR
* S_RFCACL        S_RFCACL          STAR   S_RFCACL RFC_EQUSER  #*        OR

    DATA:
      lt_store_dir_tech TYPE  tt_diagst_store_dir_tech,
      lt_store_dir      TYPE  tt_diagst_store_dir,
      lt_fieldlist      TYPE  tt_diagst_table_store_fields,
      lt_snapshot       TYPE  tt_diagst_trows,
      rc                TYPE  i,
      rc_text           TYPE  natxt.

    DATA tabix TYPE i.

    CALL FUNCTION 'DIAGST_GET_STORES'
      EXPORTING
        " The "System Filter" parameters allow to get all Stores of a system or technical system.
        " Some combinations of the four parameters are not allowed.
        " The function will return an error code in such a case.
*       SID                   = ' '
*       INSTALL_NUMBER        = ' '
*       LONG_SID              = ' '
*       TECH_SYSTEM_TYPE      = 'ABAP'                     "(only together with LONG_SID)
        " Store key fields
        group_namespace       = 'ACTIVE'                   "(optional)
        group_landscape_class = 'CL_DIAGLS_ABAP_CLIENT'    "(optional)
*       GROUP_LANDSCAPE_ID    = ' '
*       GROUP_COMP_ID         = ' '
        group_source          = 'ABAP'                     "(optional)
        group_name            = 'USER-AUTHORIZATION'       "(optional)
        store_category        = 'CONFIG'                   "(optional)
        store_type            = 'TABLE'                    "(optional)
*       STORE_FULLPATH        = ' '
        store_name            = 'AUTH_COMB_CHECK_USER'
        " Special filters
        store_mainalias       = 'AUTHORITY'                 "(optional)
        store_subalias        = 'USERS'              "(optional)
*       STORE_TPL_ID          = ' '
*       HAS_ELEMENT_FROM      =                            " date range
*       HAS_ELEMENT_TO        =                            " date range
*       ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*       CASE_INSENSITIVE      = ' '
*       PATTERN_SEARCH        = 'X'                        " Allow pattern search for SEARCH_STRING
*       SEARCH_STRING         =
*       ONLY_RELEVANT         = 'X'
*       PROTECTED             = 'A'                        " (N)ot, (Y)es, (A)ll
        " Others
*       DISPLAY               = ' '                        " Only useful if the function is manually executed by transaction SE37.
        " Setting this parameter to 'X' will display the result.
*       CALLING_APPL          = ' '
      IMPORTING
*       STORE_DIR_TECH        = lt_STORE_DIR_TECH          "(efficient, reduced structure)
        store_dir             = lt_store_dir               "(not recommended anymore)
*       STORE_DIR_MI          =                            "(SAP internal usage only)
*       STORE_STATS           =                            " History regarding the changes of elements (configuration items).
*       PARAMETER             =                            "(SAP internal usage only)
        rc                    = rc
        rc_text               = rc_text.

    IF rc IS NOT INITIAL.
      MESSAGE e001(00) WITH rc_text ##MG_MISSING.
    ENDIF.

    " We might get multiple stores per system, sort by system, system type, client
    SORT lt_store_dir BY landscape_id.

    LOOP AT lt_store_dir INTO DATA(ls_store_dir)
      WHERE long_sid              IN p_sid
        AND store_main_state_type IN p_state
        .

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          install_number = ls_store_dir-install_number
          long_sid       = ls_store_dir-long_sid
          sid            = ls_store_dir-sid
          .
      IF sy-subrc = 0.
        tabix = sy-tabix.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_store_dir TO ls_result.
      ENDIF.

* Structure: LS_STORE_DIR
* LONG_SID             FBT
* TECH_SYSTEM_TYPE     ABAP
* SID                  FBT
* INSTALL_NUMBER       0012345678
* NAMESPACE            active
* LANDSCAPE_CLASS      CL_DIAGLS_ABAP_CLIENT
* LANDSCAPE_ID         FBT~ABAP~200
* GROUP_SOURCE         ABAP
* GROUP_NAME           USER-AUTHORIZATION
* TECH_SYSTEM_ID       FBT~ABAP
* STORE_CUST_TTYPE     TT_DIAGSTC_AC_USER
* STORE_CUST_TTYPE_ID  001

      " Get the client
      SPLIT ls_store_dir-landscape_id AT '~' INTO
        DATA(sysid) DATA(systype) DATA(client).

      " Check if the customizing contains rules about S_RFCACL to avoid unneccessaary data access
      " see report ZSHOW_CCDB_CUSTOMIZING
      " Table DIAGSTC for STORE_CUST_TTYPE and STORE_CUST_TTYPE_ID contains the customizing
      DATA:
        lv_cust_ttype_desc TYPE  diagst_cust_ttype_desc,
        lv_is_default      TYPE  diagst_boolean,
        lv_last_change_ts  TYPE  timestampl,
        lr_cust_data       TYPE  REF TO data,
        l_customizing_xml  TYPE  xstring.
      FIELD-SYMBOLS: <cust_table> TYPE ANY TABLE.

      DATA exc TYPE REF TO cx_diagst_retrieve_exception.
      TRY.
      CALL FUNCTION 'DIAGST_GET_CUST_TTYPE'
        EXPORTING
          lscp_vk                      = 1 " ls_diagstc-lscp_vk
          cust_ttype                   = ls_store_dir-store_cust_ttype
          cust_ttype_id                = ls_store_dir-store_cust_ttype_id
        IMPORTING
          cust_ttype_desc              = lv_cust_ttype_desc
          is_default                   = lv_is_default
          last_change                  = lv_last_change_ts
          r_cust_data                  = lr_cust_data
          .
        CATCH cx_diagst_retrieve_exception INTO exc.
          MESSAGE exc->get_text( ) TYPE 'I'.
          ls_result-s_rfcacl_status = 'Error with store customizing'.
      ENDTRY.
      IF exc is initial.
        ASSIGN lr_cust_data->* TO <cust_table>.  " <cust_table> gets type ls_DIAGSTC-CUST_TTYPE
        IF <cust_table> IS INITIAL.
          ls_result-s_rfcacl_status = 'No store customizing'.
        ELSE.

          " Inspect content
          DATA(s_rfcacl_found) = abap_false.
          LOOP AT <cust_table> ASSIGNING FIELD-SYMBOL(<cust_line>).
            ASSIGN COMPONENT 1 OF STRUCTURE <cust_line> TO FIELD-SYMBOL(<comp>).
            IF <comp> = 'S_RFCACL'.
              s_rfcacl_found = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP. " <CUST_TABLE>

          IF s_rfcacl_found IS INITIAL.
            ls_result-s_rfcacl_status = 'No customizing about S_RFCACL'.
          ELSE.
            ls_result-s_rfcacl_status = lv_cust_ttype_desc && ` (` && ls_store_dir-store_cust_ttype_id && `)`.

            CALL FUNCTION 'DIAGST_TABLE_SNAPSHOT'
              EXPORTING
                store_id  = ls_store_dir-store_id
*               TIMESTAMP =             " if not specified the latest available snapshot is returned
*               CALLING_APPL                = ' '
              IMPORTING
                fieldlist = lt_fieldlist
*               SNAPSHOT_VALID_FROM         =
*               SNAPSHOT_VALID_TO_CONFIRMED =
*               SNAPSHOT_VALID_TO           =
                snapshot  = lt_snapshot " The content of the requested snapshot in ABAP DDIC type format
*               SNAPSHOT_TR                 =
*               SNAPSHOT_ITSAM              =             " The content of the requested snapshot in XML-based format
                rc        = rc          " 3: Permission denied, Content Authorization missing
                " 4: Store not existing
                " 8: Error
                rc_text   = rc_text.

            " Store critical user data
            FIELD-SYMBOLS <fs_critical_user> TYPE ts_critical_user.

            IF   <fs_critical_user> IS NOT ASSIGNED
              OR <fs_critical_user>-sid NE ls_store_dir-sid
              OR <fs_critical_user>-install_number NE ls_store_dir-install_number.

              " New entry in case of a different system
              APPEND INITIAL LINE TO lt_critical_users ASSIGNING <fs_critical_user>.
              <fs_critical_user>-sid            = ls_store_dir-sid.
              <fs_critical_user>-install_number = ls_store_dir-install_number.
            ENDIF.

            LOOP AT lt_snapshot INTO DATA(lt_snapshot_elem).
              READ TABLE lt_snapshot_elem INTO DATA(ls_comb_id)    INDEX 1.
              CHECK ls_comb_id-fieldname  = 'COMB_ID'
                AND ls_comb_id-fieldvalue = 'S_RFCACL'.

              READ TABLE lt_snapshot_elem INTO DATA(ls_rc)         INDEX 2.
              CHECK ls_rc-fieldname = 'RC'.

              READ TABLE lt_snapshot_elem INTO DATA(ls_user)       INDEX 3.
              CHECK ls_user-fieldname = 'USER'.

              READ TABLE lt_snapshot_elem INTO DATA(ls_locked)     INDEX 4.
              CHECK ls_locked-fieldname = 'LOCKED'.

              READ TABLE lt_snapshot_elem INTO DATA(ls_invalid)    INDEX 5.
              CHECK ls_invalid-fieldname = 'INVALID'.

              READ TABLE lt_snapshot_elem INTO DATA(ls_user_type)  INDEX 6.
              CHECK ls_user_type-fieldname = 'USER_TYPE'.

              READ TABLE lt_snapshot_elem INTO DATA(ls_user_group) INDEX 7.
              CHECK ls_user_group-fieldname = 'USER_GROUP'.

              " Store critical user data
              APPEND INITIAL LINE TO <fs_critical_user>-user_data ASSIGNING FIELD-SYMBOL(<fs_user_data>).
              <fs_user_data>-client     = client.
              <fs_user_data>-rc         = ls_rc-fieldvalue.
              <fs_user_data>-user       = ls_user-fieldvalue.
              <fs_user_data>-locked     = ls_locked-fieldvalue.
              <fs_user_data>-invalid    = ls_invalid-fieldvalue.
              <fs_user_data>-user_type  = ls_user_type-fieldvalue.
              <fs_user_data>-user_group = ls_user_group-fieldvalue.

              IF ls_rc-fieldvalue IS INITIAL.
                " User has critical authorization
                IF    ls_locked-fieldvalue IS INITIAL
                  AND ls_invalid-fieldvalue IS INITIAL.

                  ADD 1 TO ls_result-s_rfcacl_active_users.
                ELSE.
                  ADD 1 TO ls_result-s_rfcacl_inactive_users.
                ENDIF.
              ELSE.
                " RC = NONE, no user has the critical authoization
                IF ls_user-fieldvalue IS INITIAL.
                  <fs_user_data>-user = '<no user>'.
                ENDIF.
              ENDIF.

            ENDLOOP.

          ENDIF.
        ENDIF.
      ENDIF.

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_STORE_DIR

  ENDMETHOD. " get_authorizations

  METHOD validate_kernel.
    CHECK p_kern = 'X'.

* Minimum Kernel
* The solution works only if both the client systems as well as the server systems of a trusting/trusted connection runs on a suitable Kernel version:

    DATA:
      rel   TYPE i,
      patch TYPE i.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).

      IF <fs_result>-kern_rel IS INITIAL OR <fs_result>-kern_patchlevel IS INITIAL.
        <fs_result>-validate_kernel = 'Unknown Kernel'.
        APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_normal ) TO <fs_result>-t_color.

      ELSE.

        rel   = <fs_result>-kern_rel(3).
        patch = <fs_result>-kern_patchlevel.

        " Prerequisites fulfilled according to note 3224161 (version from 02.06.2023)
        IF     rel = 722 AND patch >= 1300 " to match note 3318850 - Improper authentication vulnerability in SAP NetWeaver AS ABAP and ABAP Platform
          OR   rel = 753 AND patch >= 1213 " to match note 3318850 - Improper authentication vulnerability in SAP NetWeaver AS ABAP and ABAP Platform
          OR   rel = 754 AND patch >= 120
          OR   rel = 777 AND patch >= 556  " to match note 3342409 - Trusted/Trusting http call with different clients on client and server side is not working
          OR   rel = 785 AND patch >= 251  " to match note 3342409 - Trusted/Trusting http call with different clients on client and server side is not working
          OR   rel = 789 AND patch >= 123  " to match note 3342409 - Trusted/Trusting http call with different clients on client and server side is not working
          OR   rel = 790 AND patch >= 34   " to match note 3283951 - Local BGRFC and TQRFC issues with UCON, S_RFC, Scope
          OR   rel = 791 AND patch >= 27   " to match note 3324768 - TT-Call with Load-Balancing from resetted session fails
          OR   rel = 792 AND patch >= 10   " to match note 3324768 - TT-Call with Load-Balancing from resetted session fails
          or   rel > 792
          .
          <fs_result>-validate_kernel = 'ok'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_positive ) TO <fs_result>-t_color. " Green

        " Prerequisites not fulfilled according to note 3224161 (version from 02.06.2023)

        ELSEIF rel >= 700 and rel <= 721.
          <fs_result>-validate_kernel = `Not Supported. Use AKK Kernel 722 instead`.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_negative ) TO <fs_result>-t_color. " Red

        ELSEIF rel = 722 AND patch < 1300. " to match note 3324768 - TT-Call with Load-Balancing from resetted session fails
          <fs_result>-validate_kernel = 'Kernel 722 patch 1300 required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_result>-t_color. " Yellow

        ELSEIF rel >= 740 and rel <= 752.
          <fs_result>-validate_kernel = `Not Supported. Use  AKK Kernel 753 / 754 instead`.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_negative ) TO <fs_result>-t_color. " Red

        ELSEIF rel = 753 AND patch < 1213. " to match note 3342409 - Trusted/Trusting http call with different clients on client and server side is not working
          <fs_result>-validate_kernel = 'Kernel 753 patch 1213 required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_result>-t_color. " Yellow

        ELSEIF rel = 754 AND patch < 120. " to match note 3324768 - TT-Call with Load-Balancing from resetted session fails
          <fs_result>-validate_kernel = 'Kernel 754 patch 120 required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_result>-t_color. " Yellow

        ELSEIF rel = 777 AND patch < 556. " to match note 3342409 - Trusted/Trusting http call with different clients on client and server side is not working
          <fs_result>-validate_kernel = 'Kernel 777 patch 556 required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_result>-t_color. " Yellow

        ELSEIF rel = 781 or ( rel >= 786 and rel <= 788 ).
          <fs_result>-validate_kernel = `Not Supported. Use  AKK Kernel 7.85 / 7.89 instead`.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_negative ) TO <fs_result>-t_color. " Red

        ELSEIF rel = 785 AND patch < 251. " to match note 3342409 - Trusted/Trusting http call with different clients on client and server side is not working
          <fs_result>-validate_kernel = 'Kernel 785 patch 251 required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_result>-t_color. " Yellow

        ELSEIF rel = 789 AND patch < 123. " to match note 3342409 - Trusted/Trusting http call with different clients on client and server side is not working
          <fs_result>-validate_kernel = 'Kernel 789 patch 123 required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_result>-t_color. " Yellow

        ELSEIF rel = 790 AND patch < 34. " to match note 3283951 - Local BGRFC and TQRFC issues with UCON, S_RFC, Scope
          <fs_result>-validate_kernel = 'Kernel 790 patch 34 required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_result>-t_color. " Yellow

        ELSEIF rel = 791 AND patch < 27. " to match note 3324768 - TT-Call with Load-Balancing from resetted session fails
          <fs_result>-validate_kernel = 'Kernel 791 patch 27 required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_result>-t_color. " Yellow

        ELSEIF rel = 792 AND patch < 10. " to match note 3324768 - TT-Call with Load-Balancing from resetted session fails
          <fs_result>-validate_kernel = 'Kernel 792 patch 10 required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_result>-t_color. " Yellow

        ELSE.

          <fs_result>-validate_kernel = 'Release update required'.
          APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_negative ) TO <fs_result>-t_color. " Red

        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD. " validate_kernel

  METHOD validate_abap.
    CHECK p_abap = 'X' OR p_trust = 'X' OR p_dest = 'X'.

* Minimum SAP_BASIS for SNOTE
* You only can implement the notes 3089413, 3287611, and 3304520 using transaction SNOTE if the system runs on a suitable ABAP version:
*                          solved         solved                solved
*                minimum   Note 3089413   Note 3287611          Note 3304520
* SAP_BASIS 700  SP 35     SP 41          v1 SP 41
* SAP_BASIS 701  SP 20     SP 26          v1 SP 26
* SAP_BASIS 702  SP 20     SP 26          v1 SP 26
* SAP_BASIS 731  SP 19     SP 33          v1 SP 33 -> v9 SP 34  v2 SP 33
* SAP_BASIS 740  SP 16     SP 30          v1 SP 30              v2 SP 30
* SAP_BASIS 750  SP 12     SP 26          v4 SP 27 -> v9 SP 28
* SAP_BASIS 751  SP 7      SP 16          v4 SP 17
* SAP_BASIS 752  SP 1      SP 12          v4 SP 13
* SAP_BASIS 753            SP 10          v8 SP 11
* SAP_BASIS 754            SP 8           v8 SP 9
* SAP_BASIS 755            SP 6           v8 SP 7
* SAP_BASIS 756            SP 4           v8 SP 5
* SAP_BASIS 757            SP 2           v8 SP 3

    DATA:
      rel TYPE i,
      sp  TYPE i.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).

      " Validate release and SP
      IF <fs_result>-abap_release IS INITIAL OR <fs_result>-abap_sp IS INITIAL.
        <fs_result>-validate_abap = 'Unknown ABAP version'.
        APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_normal ) TO <fs_result>-t_color.

      ELSE.
        rel   = <fs_result>-abap_release.
        sp    = <fs_result>-abap_sp.

        " Old SP
        IF     rel < 700
          OR   rel = 700 AND sp < 35
          OR   rel = 701 AND sp < 20
          OR   rel = 702 AND sp < 20
          OR   rel = 731 AND sp < 19
          OR   rel = 740 AND sp < 16
          OR   rel = 750 AND sp < 12
          OR   rel = 751 AND sp < 7
          OR   rel = 752 AND sp < 1
          .
          <fs_result>-validate_abap = 'ABAP SP required'.
          APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_negative ) TO <fs_result>-t_color.

        ELSEIF rel = 700 AND sp >= 41 " New SP is installed
          OR   rel = 701 AND sp >= 26
          OR   rel = 702 AND sp >= 26
          OR   rel = 731 AND sp >= 34
          OR   rel = 740 AND sp >= 30
          OR   rel = 750 AND sp >= 28
          OR   rel = 751 AND sp >= 17
          OR   rel = 752 AND sp >= 13
          OR   rel = 753 AND sp >= 11
          OR   rel = 754 AND sp >= 9
          OR   rel = 755 AND sp >= 7
          OR   rel = 756 AND sp >= 5
          OR   rel = 757 AND sp >= 3
          OR   rel > 757
          .
          <fs_result>-validate_abap = 'ok'.
          APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_positive ) TO <fs_result>-t_color.

          "<fs_result>-note_3089413 = 'ok'.
          "APPEND VALUE #( fname = 'NOTE_3089413'  color-col = col_positive ) TO <fs_result>-t_color.

          "<fs_result>-note_3287611 = 'ok'.
          "APPEND VALUE #( fname = 'NOTE_3287611'  color-col = col_positive ) TO <fs_result>-t_color.

        ELSE. " Notes are required

          " Check note 3089413 - Capture-replay vulnerability in SAP NetWeaver AS for ABAP and ABAP Platform
          DATA(note_3089413_ok) = abap_true.
          IF     rel = 700 AND sp < 41
            OR   rel = 701 AND sp < 26
            OR   rel = 702 AND sp < 26
            OR   rel = 731 AND sp < 33
            OR   rel = 740 AND sp < 30
            OR   rel = 750 AND sp < 26
            OR   rel = 751 AND sp < 16
            OR   rel = 752 AND sp < 12
            OR   rel = 753 AND sp < 10
            OR   rel = 754 AND sp < 8
            OR   rel = 755 AND sp < 6
            OR   rel = 756 AND sp < 4
            OR   rel = 757 AND sp < 2
            .
            " Note 3089413 is required
            CONSTANTS note_3089413_min_version(4) VALUE '0011'.
            note_3089413_ok = abap_false.

            FIND REGEX '(\d{4})' IN <fs_result>-note_3089413 SUBMATCHES DATA(note_3089413_version).
            " E Completely implemented
            " V Obsolete version implemented
            "   Undefined Implementation State
            " N Can be implemented
            " U Incompletely implemented
            " - Cannot be implemented
            " O Obsolete
            CASE <fs_result>-note_3089413_prstatus.
              WHEN 'E'.
                IF note_3089413_version >= note_3089413_min_version.
                  if <fs_result>-note_3089413 is initial.
                    <fs_result>-note_3089413 = `Version ` && note_3089413_version && ' implemented'.
                  endif.
                  APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_positive ) TO <fs_result>-t_color.
                  note_3089413_ok = abap_true.
                ELSE.
                  <fs_result>-note_3089413 = `Old version ` && note_3089413_version && ' implemented'.
                  APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_total ) TO <fs_result>-t_color.
                ENDIF.
              WHEN 'V'.
                if <fs_result>-note_3089413 is initial.
                  <fs_result>-note_3089413 = `Old version ` && note_3089413_version && ' implemented'.
                endif.
                APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_total ) TO <fs_result>-t_color.
              WHEN ' ' OR 'N' OR 'U'.
                if <fs_result>-note_3089413 is initial.
                  <fs_result>-note_3089413 = `required`.
                endif.
                APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_negative ) TO <fs_result>-t_color.
              WHEN '-' OR 'O'.
                if <fs_result>-note_3089413 is initial.
                  <fs_result>-note_3089413 = `Strange status (` && <fs_result>-note_3089413_prstatus && `)`.
                endif.
                APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_negative ) TO <fs_result>-t_color.
              WHEN OTHERS.
                if <fs_result>-note_3089413 is initial.
                  <fs_result>-note_3089413 = `Strange status (` && <fs_result>-note_3089413_prstatus && `)`.
                endif.
                APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_negative ) TO <fs_result>-t_color.
            ENDCASE.
          ENDIF.

          " Check note 3287611 - Bugfixes in Trusted/Trusting Migration
          DATA(note_3287611_ok) = abap_true.
          IF     rel = 700 AND sp < 41 " v1
            OR   rel = 701 AND sp < 26 " v1
            OR   rel = 702 AND sp < 26 " v1
            OR   rel = 731 AND sp < 34 " v1 SP 33, v9 SP 34
            OR   rel = 740 AND sp < 30 " v1
            OR   rel = 750 AND sp < 28 " v4 SP 27, v9 SP 28
            OR   rel = 751 AND sp < 17 " v4
            OR   rel = 752 AND sp < 13 " v4
            OR   rel = 753 AND sp < 11 " v8
            OR   rel = 754 AND sp < 9  " v8
            OR   rel = 755 AND sp < 7  " v8
            OR   rel = 756 AND sp < 5  " v8
            OR   rel = 757 AND sp < 3  " v8
            .
            " Note 3287611 is required
            CONSTANTS note_3287611_min_version(4) VALUE '0009'.
            note_3287611_ok = abap_false.

            FIND REGEX '(\d{4})' IN <fs_result>-note_3287611 SUBMATCHES DATA(note_3287611_version).
            " E Completely implemented
            " V Obsolete version implemented
            "   Undefined Implementation State
            " N Can be implemented
            " U Incompletely implemented
            " - Cannot be implemented
            " O Obsolete
            CASE <fs_result>-note_3287611_prstatus.
              WHEN 'E'.
                IF note_3287611_version >= note_3287611_min_version.
                  <fs_result>-note_3287611 = `Version ` && note_3287611_version && ' implemented'.
                  APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_positive ) TO <fs_result>-t_color.
                  note_3287611_ok = abap_true.
                ELSE.
                  <fs_result>-note_3287611 = `Old version ` && note_3287611_version && ' implemented'.
                  APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_total ) TO <fs_result>-t_color.
                ENDIF.
              WHEN 'V'.
                <fs_result>-note_3287611 = `Old version ` && note_3287611_version && ' implemented'.
                APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_total ) TO <fs_result>-t_color.
              WHEN ' ' OR 'N' OR 'U'.
                <fs_result>-note_3287611 = `required`.
                APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_negative ) TO <fs_result>-t_color.
              WHEN '-' OR 'O'.
                <fs_result>-note_3287611 = `Strange status (` && <fs_result>-note_3287611_prstatus && `)`.
                APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_negative ) TO <fs_result>-t_color.
              WHEN OTHERS.
                <fs_result>-note_3287611 = `Strange status (` && <fs_result>-note_3287611_prstatus && `)`.
                APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_negative ) TO <fs_result>-t_color.
            ENDCASE.
          ENDIF.

          " Check note 3304520 - Trusted Trusting: Note 3089413 implementation fails due to incorrect TCI package validity
          DATA(note_3304520_ok) = abap_true.
          IF     rel = 731 AND sp < 33
            OR   rel = 740 AND sp < 30
            .
            " Note 3304520 is required
            CONSTANTS note_3304520_min_version(4) VALUE '0002'.
            note_3304520_ok = abap_false.

            FIND REGEX '(\d{4})' IN <fs_result>-note_3304520 SUBMATCHES DATA(note_3304520_version).
            " E Completely implemented
            " V Obsolete version implemented
            "   Undefined Implementation State
            " N Can be implemented
            " U Incompletely implemented
            " - Cannot be implemented
            " O Obsolete
            CASE <fs_result>-note_3304520_prstatus.
              WHEN 'E'.
                IF note_3304520_version >= note_3304520_min_version.
                  <fs_result>-note_3304520 = `Version ` && note_3304520_version && ' implemented'.
                  APPEND VALUE #( fname = 'NOTE_3304520' color-col = col_positive ) TO <fs_result>-t_color.
                  note_3304520_ok = abap_true.
                ELSE.
                  <fs_result>-note_3304520 = `Old version ` && note_3304520_version && ' implemented'.
                  APPEND VALUE #( fname = 'NOTE_3304520' color-col = col_total ) TO <fs_result>-t_color.
                ENDIF.
              WHEN 'V'.
                <fs_result>-note_3304520 = `Old version ` && note_3304520_version && ' implemented'.
                APPEND VALUE #( fname = 'NOTE_3304520' color-col = col_total ) TO <fs_result>-t_color.
              WHEN ' ' OR 'N' OR 'U'.
                <fs_result>-note_3304520 = `required`.
                APPEND VALUE #( fname = 'NOTE_3304520' color-col = col_negative ) TO <fs_result>-t_color.
              WHEN '-' OR 'O'.
                <fs_result>-note_3304520 = `Strange status (` && <fs_result>-note_3304520_prstatus && `)`.
                APPEND VALUE #( fname = 'NOTE_3304520' color-col = col_negative ) TO <fs_result>-t_color.
              WHEN OTHERS.
                <fs_result>-note_3304520 = `Strange status (` && <fs_result>-note_3304520_prstatus && `)`.
                APPEND VALUE #( fname = 'NOTE_3304520' color-col = col_negative ) TO <fs_result>-t_color.
            ENDCASE.
          ENDIF.

          " Adjust overall status
          IF    note_3089413_ok = abap_true
            AND note_3287611_ok = abap_true
            AND note_3304520_ok = abap_true
            .
            <fs_result>-validate_abap = 'Notes are installed'.
            APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_positive ) TO <fs_result>-t_color.
          ELSE.
            <fs_result>-validate_abap = 'Note(s) are required'.
            APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_total ) TO <fs_result>-t_color.
          ENDIF.

        ENDIF. " Check SP and notes

      ENDIF. " Unknown ABAP version


      " Validate trusted systems
      IF <fs_result>-trustsy_cnt_3 > 0.
        APPEND VALUE #( fname = 'TRUSTSY_CNT_3' color-col = col_positive ) TO <fs_result>-t_color.
      ENDIF.
      IF <fs_result>-trustsy_cnt_2 > 0.
        APPEND VALUE #( fname = 'TRUSTSY_CNT_2' color-col = col_negative ) TO <fs_result>-t_color.
      ENDIF.
      IF <fs_result>-trustsy_cnt_1 > 0.
        APPEND VALUE #( fname = 'TRUSTSY_CNT_1' color-col = col_negative ) TO <fs_result>-t_color.
      ENDIF.

      " Validate TCD flag
      IF <fs_result>-trustsy_cnt_tcd > 0.
        APPEND VALUE #( fname = 'TRUSTSY_CNT_TCD' color-col = col_positive ) TO <fs_result>-t_color.
      ENDIF.

      " Validate rfc/selftrust
      CASE <fs_result>-rfc_selftrust.
        WHEN '0'. APPEND VALUE #( fname = 'RFC_SELFTRUST' color-col = col_positive ) TO <fs_result>-t_color.
        WHEN '1'. APPEND VALUE #( fname = 'RFC_SELFTRUST' color-col = col_total )    TO <fs_result>-t_color.
      ENDCASE.

      " Validate trusted destinations
      IF <fs_result>-dest_3_cnt_trusted_migrated > 0.
        APPEND VALUE #( fname = 'DEST_3_CNT_TRUSTED_MIGRATED' color-col = col_positive ) TO <fs_result>-t_color.
      ENDIF.
      IF <fs_result>-dest_3_cnt_trusted_no_instnr > 0.
        APPEND VALUE #( fname = 'DEST_3_CNT_TRUSTED_NO_INSTNR' color-col = col_negative ) TO <fs_result>-t_color.
      ENDIF.
      IF <fs_result>-dest_3_cnt_trusted_no_sysid > 0.
        APPEND VALUE #( fname = 'DEST_3_CNT_TRUSTED_NO_SYSID' color-col = col_negative ) TO <fs_result>-t_color.
      ENDIF.

      IF <fs_result>-dest_h_cnt_trusted_migrated > 0.
        APPEND VALUE #( fname = 'DEST_H_CNT_TRUSTED_MIGRATED' color-col = col_positive ) TO <fs_result>-t_color.
      ENDIF.
      IF <fs_result>-dest_h_cnt_trusted_no_instnr > 0.
        APPEND VALUE #( fname = 'DEST_H_CNT_TRUSTED_NO_INSTNR' color-col = col_negative ) TO <fs_result>-t_color.
      ENDIF.
      IF <fs_result>-dest_h_cnt_trusted_no_sysid > 0.
        APPEND VALUE #( fname = 'DEST_H_CNT_TRUSTED_NO_SYSID' color-col = col_negative ) TO <fs_result>-t_color.
      ENDIF.

      IF <fs_result>-dest_w_cnt_trusted_migrated > 0.
        APPEND VALUE #( fname = 'DEST_W_CNT_TRUSTED_MIGRATED' color-col = col_positive ) TO <fs_result>-t_color.
      ENDIF.
      IF <fs_result>-dest_w_cnt_trusted_no_instnr > 0.
        APPEND VALUE #( fname = 'DEST_W_CNT_TRUSTED_NO_INSTNR' color-col = col_negative ) TO <fs_result>-t_color.
      ENDIF.
      IF <fs_result>-dest_w_cnt_trusted_no_sysid > 0.
        APPEND VALUE #( fname = 'DEST_3_CNT_TRUSTED_NO_SYSID' color-col = col_negative ) TO <fs_result>-t_color.
      ENDIF.

    ENDLOOP.
  ENDMETHOD. " validate_ABAP

  METHOD validate_mutual_trust.
    CHECK p_trust = 'X'.

    FIELD-SYMBOLS:
      <fs_trusted_system>   TYPE ts_trusted_system,
      <fs_rfcsysacl_data>   TYPE ts_rfcsysacl_data,
      <fs_trusted_system_2> TYPE ts_trusted_system,
      <fs_rfcsysacl_data_2> TYPE ts_rfcsysacl_data,
      <fs_result>           TYPE ts_result.

*  " Fast but only possible if LLICENSE_NR is available
*  " For all trusting systems
*  loop at lt_TRUSTED_SYSTEMS ASSIGNING <fs_TRUSTED_SYSTEM>.
*
*    " Get trusted systems
*    loop at <fs_TRUSTED_SYSTEM>-RFCSYSACL_data ASSIGNING <fs_RFCSYSACL_data>
*      where RFCSYSID    ne <fs_TRUSTED_SYSTEM>-RFCTRUSTSY   " Ignore selftrust
*         "or TLICENSE_NR ne <fs_TRUSTED_SYSTEM>-LLICENSE_NR
*         .
*
*      " Get data of these trusted systems
*      read table lt_TRUSTED_SYSTEMS ASSIGNING <fs_TRUSTED_SYSTEM_2>
*        WITH TABLE KEY
*          RFCTRUSTSY  = <fs_RFCSYSACL_data>-RFCSYSID
*          LLICENSE_NR = <fs_RFCSYSACL_data>-TLICENSE_NR
*          .
*      if sy-subrc = 0.
*
*        " Check for mutual trust
*        read table <fs_TRUSTED_SYSTEM_2>-RFCSYSACL_data ASSIGNING <fs_RFCSYSACL_data_2>
*          WITH TABLE KEY
*            RFCSYSID     = <fs_RFCSYSACL_data>-RFCTRUSTSY
*            TLICENSE_NR  = <fs_RFCSYSACL_data>-LLICENSE_NR
*            .
*        if sy-subrc = 0
*          "and <fs_RFCSYSACL_data>-RFCSYSID       = <fs_RFCSYSACL_data_2>-RFCTRUSTSY
*          "and <fs_RFCSYSACL_data>-TLICENSE_NR    = <fs_RFCSYSACL_data_2>-LLICENSE_NR
*          "and <fs_RFCSYSACL_data_2>-RFCSYSID     = <fs_RFCSYSACL_data>-RFCTRUSTSY
*          "and <fs_RFCSYSACL_data_2>-TLICENSE_NR  = <fs_RFCSYSACL_data>-LLICENSE_NR
*          .
*          " Store mutual trust
*          "...
*        endif.
*
*      endif.
*
*    endloop.
*  endloop.

    " Slow, and maybe inaccurate ignoring LLICENSE_NR
    " For all trusting systems
    LOOP AT lt_trusted_systems ASSIGNING <fs_trusted_system>.

      " Get trusted systems
      LOOP AT <fs_trusted_system>-rfcsysacl_data ASSIGNING <fs_rfcsysacl_data>
        WHERE rfcsysid    NE <fs_trusted_system>-rfctrustsy   " Ignore selftrust
           "or TLICENSE_NR ne <fs_TRUSTED_SYSTEM>-LLICENSE_NR
           .

        LOOP AT lt_trusted_systems ASSIGNING <fs_trusted_system_2>
          WHERE rfctrustsy  = <fs_rfcsysacl_data>-rfcsysid
            "and LLICENSE_NR = <fs_RFCSYSACL_data>-TLICENSE_NR
            .

          LOOP AT <fs_trusted_system_2>-rfcsysacl_data ASSIGNING <fs_rfcsysacl_data_2>
            WHERE rfcsysid     = <fs_rfcsysacl_data>-rfctrustsy
              "and TLICENSE_NR  = <fs_RFCSYSACL_data>-LLICENSE_NR
              .

            " Store mutual trust
            READ TABLE lt_result ASSIGNING <fs_result>
              WITH KEY
                sid            = <fs_rfcsysacl_data>-rfctrustsy
                install_number = <fs_rfcsysacl_data>-llicense_nr.
            IF sy-subrc = 0.
              ADD 1 TO <fs_result>-mutual_trust_cnt.
              IF <fs_result>-mutual_trust_cnt = 1.
                APPEND VALUE #( fname = 'MUTUAL_TRUST_CNT' color-col = col_total ) TO <fs_result>-t_color.
              ENDIF.
            ENDIF.

            <fs_rfcsysacl_data>-mutual_trust   = 'mutual'.
            <fs_rfcsysacl_data_2>-mutual_trust = 'mutual'.

            "write: /(3) <fs_RFCSYSACL_data>-RFCSYSID,     <fs_RFCSYSACL_data>-TLICENSE_NR,   (3) <fs_RFCSYSACL_data>-RFCTRUSTSY,   <fs_RFCSYSACL_data>-LLICENSE_NR.   " <fs_RFCSYSACL_data>
            "write:  (3) <fs_RFCSYSACL_data_2>-RFCTRUSTSY, <fs_RFCSYSACL_data_2>-LLICENSE_NR, (3) <fs_RFCSYSACL_data_2>-RFCSYSID,   <fs_RFCSYSACL_data_2>-TLICENSE_NR. " <fs_RFCSYSACL_data_2>

          ENDLOOP.
        ENDLOOP.

        IF sy-subrc IS NOT INITIAL                      " No data of trusted system found?
          AND <fs_rfcsysacl_data>-rfcsysid IN p_sid.    " But check this only of data should be available

          " Store mutual trust
          READ TABLE lt_result ASSIGNING <fs_result>
            WITH KEY
              sid            = <fs_rfcsysacl_data>-rfctrustsy
              install_number = <fs_rfcsysacl_data>-llicense_nr.
          IF sy-subrc = 0.
            ADD 1 TO <fs_result>-no_data_cnt.
            IF <fs_result>-mutual_trust_cnt = 1.
              APPEND VALUE #( fname = 'MUTUAL_TRUST_CNT' color-col = col_total ) TO <fs_result>-t_color.
            ENDIF.
          ENDIF.

          <fs_rfcsysacl_data>-no_data = 'no data'.

        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD. " validate_mutual_trust

  METHOD count_dest_per_trust.
    CHECK p_trust IS NOT INITIAL AND p_dest IS NOT INITIAL.

    " Count migrated destinations in trusted systems which point to a trusting system
    LOOP AT lt_trusted_systems ASSIGNING FIELD-SYMBOL(<fs_trusted_system>).
      LOOP AT <fs_trusted_system>-rfcsysacl_data ASSIGNING FIELD-SYMBOL(<fs_rfcsysacl_data>).

        READ TABLE lt_destinations ASSIGNING FIELD-SYMBOL(<fs_destination>)
          WITH TABLE KEY
            sid            = <fs_rfcsysacl_data>-rfcsysid
            install_number = <fs_rfcsysacl_data>-tlicense_nr.
        IF sy-subrc IS INITIAL.
          LOOP AT <fs_destination>-destination_data ASSIGNING FIELD-SYMBOL(<fs_destination_data>)
            WHERE trusted IS NOT INITIAL
              AND serversysid  = <fs_trusted_system>-rfctrustsy
              AND serverinstnr = <fs_trusted_system>-llicense_nr.

            ADD 1 TO <fs_rfcsysacl_data>-trusted_dest_cnt.

            IF <fs_rfcsysacl_data>-trusted_dest_cnt = 1.
              APPEND VALUE #( fname = 'TRUSTED_DEST_CNT' color-col = col_positive ) TO <fs_rfcsysacl_data>-t_color.
            ENDIF.

          ENDLOOP.
        ELSE.
          " No data available for trusting system
        ENDIF.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD. " count_dest_per_trust

  METHOD validate_trust_for_dest.
    CHECK p_trust IS NOT INITIAL AND p_dest IS NOT INITIAL.

    " Check migrated trusted destinations if there exist a trust relation in the target system for the calling system
    LOOP AT lt_destinations ASSIGNING FIELD-SYMBOL(<fs_destination>).
      LOOP AT <fs_destination>-destination_data ASSIGNING FIELD-SYMBOL(<fs_destination_data>)
        WHERE trusted      IS NOT INITIAL
          AND serversysid  IS NOT INITIAL
          AND serverinstnr IS NOT INITIAL.

        " Get data from trusting system
        READ TABLE lt_trusted_systems ASSIGNING FIELD-SYMBOL(<fs_trusted_system>)
          WITH TABLE KEY
            rfctrustsy  = <fs_destination_data>-serversysid
            llicense_nr = <fs_destination_data>-serverinstnr.
        IF sy-subrc IS INITIAL.
          " Is the current system a trusted system?
          READ TABLE <fs_trusted_system>-rfcsysacl_data ASSIGNING FIELD-SYMBOL(<fs_rfcsysacl_data>)
            WITH TABLE KEY
              rfcsysid    = <fs_destination>-sid
              tlicense_nr = <fs_destination>-install_number.
          IF sy-subrc IS INITIAL.
            CASE <fs_rfcsysacl_data>-rfcslopt.
              WHEN space. <fs_destination_data>-check_trusted = `very old`.
              WHEN '2'.   <fs_destination_data>-check_trusted = `old`.
              WHEN '3'.   <fs_destination_data>-check_trusted = `migrated`.
            ENDCASE.
          ELSE.
            <fs_destination_data>-check_trusted = 'missing'.
          ENDIF.
        ELSE.
          " No data available for trusting system
        ENDIF.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD. " validate_trust_for_dest

  METHOD validate_authorizations.
    CHECK p_auth = 'X'.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).

      IF <fs_result>-s_rfcacl_active_users > 0.
        APPEND VALUE #( fname = 'S_RFCACL_ACTIVE_USERS' color-col = col_negative ) TO <fs_result>-t_color.
      ELSE.
        " No user or missing data?
        READ TABLE lt_critical_users ASSIGNING FIELD-SYMBOL(<fs_critical_users>)
          WITH TABLE KEY
            sid            = <fs_result>-sid
            install_number = <fs_result>-install_number.
        IF sy-subrc = 0.
          DATA rc TYPE string.
          CLEAR rc.
          LOOP AT <fs_critical_users>-user_data ASSIGNING FIELD-SYMBOL(<fs_user_data>).
            IF rc NE 'user' AND <fs_user_data>-rc = 'NONE'.
              rc = 'NONE'. " keep green
            ELSE.
              rc = 'user'. " no color
            ENDIF.
          ENDLOOP.
          IF rc = 'NONE'.
            APPEND VALUE #( fname = 'S_RFCACL_ACTIVE_USERS' color-col = col_positive ) TO <fs_result>-t_color.
          ENDIF.
        ENDIF.
      ENDIF.

      IF <fs_result>-s_rfcacl_inactive_users > 0.
        APPEND VALUE #( fname = 'S_RFCACL_INACTIVE_USERS' color-col = col_total ) TO <fs_result>-t_color.
      ENDIF.

    ENDLOOP.

  ENDMETHOD. " validate_authorizations

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

          " Show trusted systems
          IF   ls_cell-columnname(12) = 'TRUSTSY_CNT_'
            OR ls_cell-columnname = 'MUTUAL_TRUST_CNT'
            OR ls_cell-columnname = 'NO_DATA_CNT'.

            show_trusted_systems(
              column      = ls_cell-columnname
              rfctrustsy  = ls_result-sid
              llicense_nr = ls_result-install_number
            ).

            " Show destinations
          ELSEIF ls_cell-columnname(11) = 'DEST_3_CNT_'
            OR ls_cell-columnname(11) = 'DEST_H_CNT_'
            OR ls_cell-columnname(11) = 'DEST_W_CNT_'.

            show_destinations(
              column         = ls_cell-columnname
              sid            = ls_result-sid
              install_number = ls_result-install_number
            ).

            " Show critical users
          ELSEIF ls_cell-columnname = 'S_RFCACL_ACTIVE_USERS'
            OR ls_cell-columnname = 'S_RFCACL_INACTIVE_USERS'.

            show_critical_users(
              column         = ls_cell-columnname
              sid            = ls_result-sid
              install_number = ls_result-install_number
            ).

          ENDIF.
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

      " Show trusted systems
      IF   column(12) = 'TRUSTSY_CNT_'
        OR column = 'MUTUAL_TRUST_CNT'
        OR column = 'NO_DATA_CNT'.

        show_trusted_systems(
          column      = column
          rfctrustsy  = ls_result-sid
          llicense_nr = ls_result-install_number
        ).

        " Show destinations
      ELSEIF column(11) = 'DEST_3_CNT_'
        OR column(11) = 'DEST_H_CNT_'
        OR column(11) = 'DEST_W_CNT_'.

        show_destinations(
          column         = column
          sid            = ls_result-sid
          install_number = ls_result-install_number
        ).

        " Show critical users
      ELSEIF column = 'S_RFCACL_ACTIVE_USERS'
        OR column = 'S_RFCACL_INACTIVE_USERS'.

        show_critical_users(
          column         = ls_cell-columnname
          sid            = ls_result-sid
          install_number = ls_result-install_number
        ).

      ENDIF.
    ENDIF.

  ENDMETHOD. " on_double_click

  METHOD show_result.
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
        lr_sorts->add_sort( 'INSTALL_NUMBER' ).
        lr_sorts->add_sort( 'LONG_SID' ).
        lr_sorts->add_sort( 'SID' ).

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

        " Kernel

        lr_column ?= lr_columns->get_column( 'KERN_REL' ).
        lr_column->set_long_text( 'Kernel release' ).
        lr_column->set_medium_text( 'Kernel release' ).
        lr_column->set_short_text( 'Kernel rel' ).

        lr_column ?= lr_columns->get_column( 'KERN_PATCHLEVEL' ).
        lr_column->set_long_text( 'Kernel patch level' ).
        lr_column->set_medium_text( 'Kernel patch' ).
        lr_column->set_short_text( 'patch' ).

        lr_column ?= lr_columns->get_column( 'KERN_COMP_TIME' ).
        lr_column->set_long_text( 'Kernel compilation time' ).
        lr_column->set_medium_text( 'Kernel compilation' ).
        lr_column->set_short_text( 'Comp.time' ).

        lr_column ?= lr_columns->get_column( 'KERN_COMP_DATE' ).
        lr_column->set_long_text( 'Kernel compilation date' ).
        lr_column->set_medium_text( 'Kernel compilation' ).
        lr_column->set_short_text( 'Comp.date' ).

        lr_column ?= lr_columns->get_column( 'VALIDATE_KERNEL' ).
        lr_column->set_long_text( 'Validate Kernel' ).
        lr_column->set_medium_text( 'Validate Kernel' ).
        lr_column->set_short_text( 'Kernel?' ).

        " ABAP

        lr_column ?= lr_columns->get_column( 'COMPV_NAME' ).
        lr_column->set_long_text( 'Software Component Version' ).   "max. 40 characters
        lr_column->set_medium_text( 'SW Comp. Version' ). "max. 20 characters
        lr_column->set_short_text( 'SWCompVers' ).     "max. 10 characters

        lr_column ?= lr_columns->get_column( 'ABAP_RELEASE' ).
        lr_column->set_long_text( 'ABAP release' ).
        lr_column->set_medium_text( 'ABAP release' ).
        lr_column->set_short_text( 'ABAP rel.' ).

        lr_column ?= lr_columns->get_column( 'ABAP_SP' ).
        lr_column->set_long_text( 'ABAP Support Package' ).
        lr_column->set_medium_text( 'ABAP Support Package' ).
        lr_column->set_short_text( 'ABAP SP' ).

        lr_column ?= lr_columns->get_column( 'VALIDATE_ABAP' ).
        lr_column->set_long_text( 'Validate ABAP' ).
        lr_column->set_medium_text( 'Validate ABAP' ).
        lr_column->set_short_text( 'ABAP?' ).

        " Notes

        lr_column ?= lr_columns->get_column( 'NOTE_3089413' ).
        lr_column->set_long_text( 'Note 3089413' ).
        lr_column->set_medium_text( 'Note 3089413' ).
        lr_column->set_short_text( 'N. 3089413' ).

        lr_column ?= lr_columns->get_column( 'NOTE_3089413_PRSTATUS' ).
        lr_column->set_long_text( 'Note 3089413' ).
        lr_column->set_medium_text( 'Note 3089413' ).
        lr_column->set_short_text( 'N. 3089413' ).

        lr_column ?= lr_columns->get_column( 'NOTE_3287611' ).
        lr_column->set_long_text( 'Note 3287611' ).
        lr_column->set_medium_text( 'Note 3287611' ).
        lr_column->set_short_text( 'N. 3287611' ).

        lr_column ?= lr_columns->get_column( 'NOTE_3287611_PRSTATUS' ).
        lr_column->set_long_text( 'Note 3287611' ).
        lr_column->set_medium_text( 'Note 3287611' ).
        lr_column->set_short_text( 'N. 3287611' ).

        lr_column ?= lr_columns->get_column( 'NOTE_3304520' ).
        lr_column->set_long_text( 'Note 3304520' ).
        lr_column->set_medium_text( 'Note 3304520' ).
        lr_column->set_short_text( 'N. 3304520' ).

        lr_column ?= lr_columns->get_column( 'NOTE_3304520_PRSTATUS' ).
        lr_column->set_long_text( 'Note 3304520' ).
        lr_column->set_medium_text( 'Note 3304520' ).
        lr_column->set_short_text( 'N. 3304520' ).

        " Trusted systems

        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_ALL' ).
        lr_column->set_long_text( 'All trusted systems' ).
        lr_column->set_medium_text( 'All trusted systems' ).
        lr_column->set_short_text( 'Trusted' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'NO_DATA_CNT' ).
        lr_column->set_long_text( 'No data of trusted system found' ).
        lr_column->set_medium_text( 'No data found' ).
        lr_column->set_short_text( 'No data' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'MUTUAL_TRUST_CNT' ).
        lr_column->set_long_text( 'Mutual trust relations' ).
        lr_column->set_medium_text( 'Mutual trust' ).
        lr_column->set_short_text( 'Mutual' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_TCD' ).
        lr_column->set_long_text( 'Tcode active for trusted systems' ).
        lr_column->set_medium_text( 'Tcode active' ).
        lr_column->set_short_text( 'TCD active' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_3' ).
        lr_column->set_long_text( 'Migrated trusted systems' ).
        lr_column->set_medium_text( 'Migrated systems' ).
        lr_column->set_short_text( 'Migrated' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_2' ).
        lr_column->set_long_text( 'Old trusted systems' ).
        lr_column->set_medium_text( 'Old trusted systems' ).
        lr_column->set_short_text( 'Old' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_1' ).
        lr_column->set_long_text( 'Very old trusted systems' ).
        lr_column->set_medium_text( 'Very old trusted sys' ).
        lr_column->set_short_text( 'Very old' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'EXPLICIT_SELFTRUST' ).
        lr_column->set_long_text( 'Explicit selftrust defined in SMT1' ).
        lr_column->set_medium_text( 'Explicit selftrust' ).
        lr_column->set_short_text( 'Explicit' ).

        " Profile parameter

        lr_column ?= lr_columns->get_column( 'RFC_ALLOWOLDTICKET4TT' ).
        lr_column->set_long_text( 'Allow old ticket' ).
        lr_column->set_medium_text( 'Allow old ticket' ).
        lr_column->set_short_text( 'Allow old' ).

        lr_column ?= lr_columns->get_column( 'RFC_SELFTRUST' ).
        lr_column->set_long_text( 'RFC selftrust by profile parameter' ).
        lr_column->set_medium_text( 'RFC selftrust by par' ).
        lr_column->set_short_text( 'Selftrust' ).

        lr_column ?= lr_columns->get_column( 'RFC_SENDINSTNR4TT' ).
        lr_column->set_long_text( 'Send installation number' ).
        lr_column->set_medium_text( 'Send installation nr' ).
        lr_column->set_short_text( 'SendInstNr' ).

        " Type 3 Destinations
        color-col = 2. " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange

        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_ALL' ).
        lr_column->set_long_text( 'RFC Destinations (Type 3)' ).
        lr_column->set_medium_text( 'RFC Destinations' ).
        lr_column->set_short_text( 'RFC Dest.' ).
        lr_column->set_zero( abap_false  ).
        lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED'  ).
        lr_column->set_long_text( 'Trusted RFC Destinations' ).
        lr_column->set_medium_text( 'Trusted RFC Dest.' ).
        lr_column->set_short_text( 'Trust.RFC' ).
        lr_column->set_zero( abap_false  ).
        lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_MIGRATED' ).
        lr_column->set_long_text( 'Migrated Trusted RFC Destinations' ).
        lr_column->set_medium_text( 'Migrated Trusted' ).
        lr_column->set_short_text( 'Migrated' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_NO_INSTNR' ).
        lr_column->set_long_text( 'No Installation Number in Trusted Dest.' ).
        lr_column->set_medium_text( 'No Installation Nr' ).
        lr_column->set_short_text( 'No Inst Nr' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_NO_SYSID' ).
        lr_column->set_long_text( 'No System ID in Trusted RFC Destinations' ).
        lr_column->set_medium_text( 'No System ID' ).
        lr_column->set_short_text( 'No Sys. ID' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_SNC' ).
        lr_column->set_long_text( 'SNC for Trusted RFC Destinations' ).
        lr_column->set_medium_text( 'SNC Trusted Dest' ).
        lr_column->set_short_text( 'SNC Trust.' ).
        lr_column->set_zero( abap_false  ).

        " Type H Destinations

        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_ALL' ).
        lr_column->set_long_text( 'HTTP Destinations (Type H)' ).
        lr_column->set_medium_text( 'HTTP Destinations' ).
        lr_column->set_short_text( 'HTTP Dest.' ).
        lr_column->set_zero( abap_false  ).
        lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED'  ).
        lr_column->set_long_text( 'Trusted HTTP Destinations' ).
        lr_column->set_medium_text( 'Trusted HTTPS Dest.' ).
        lr_column->set_short_text( 'Trust.HTTP' ).
        lr_column->set_zero( abap_false  ).
        lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_MIGRATED' ).
        lr_column->set_long_text( 'Migrated Trusted HTTP Destinations' ).
        lr_column->set_medium_text( 'Migrated Trusted' ).
        lr_column->set_short_text( 'Migrated' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_NO_INSTNR' ).
        lr_column->set_long_text( 'No Installation Number in Trusted Dest.' ).
        lr_column->set_medium_text( 'No Installation Nr' ).
        lr_column->set_short_text( 'No Inst Nr' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_NO_SYSID' ).
        lr_column->set_long_text( 'No System ID in Trusted HTTP Dest.' ).
        lr_column->set_medium_text( 'No System ID' ).
        lr_column->set_short_text( 'No Sys. ID' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_TLS' ).
        lr_column->set_long_text( 'TLS for Trusted HTTP Destinations' ).
        lr_column->set_medium_text( 'TLS Trusted Dest.' ).
        lr_column->set_short_text( 'TLS Trust.' ).
        lr_column->set_zero( abap_false  ).

        " Type W Destinations

        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_ALL' ).
        lr_column->set_long_text( 'WebRFC Destinations (Type W)' ).
        lr_column->set_medium_text( 'WebRFC Destinations' ).
        lr_column->set_short_text( 'Web Dest.' ).
        lr_column->set_zero( abap_false  ).
        lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED'  ).
        lr_column->set_long_text( 'Trusted WebRFC Destinations' ).
        lr_column->set_medium_text( 'Trusted WebRFC Dest.' ).
        lr_column->set_short_text( 'Trust Web' ).
        lr_column->set_zero( abap_false  ).
        lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_MIGRATED' ).
        lr_column->set_long_text( 'Migrated Trusted WebRFC Destinations' ).
        lr_column->set_medium_text( 'Migrated Trusted' ).
        lr_column->set_short_text( 'Migrated' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_NO_INSTNR' ).
        lr_column->set_long_text( 'No Installation Number in Trusted Dest.' ).
        lr_column->set_medium_text( 'No Installation Nr' ).
        lr_column->set_short_text( 'No Inst Nr' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_NO_SYSID' ).
        lr_column->set_long_text( 'No System ID in Trusted WebRFC Dest.' ).
        lr_column->set_medium_text( 'No System ID' ).
        lr_column->set_short_text( 'No Sys. ID' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_TLS' ).
        lr_column->set_long_text( 'TLS for Trusted WebRFC Destinations' ).
        lr_column->set_medium_text( 'TLS Trusted Dest.' ).
        lr_column->set_short_text( 'TLS Trust.' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'S_RFCACL_STATUS' ).
        lr_column->set_long_text(   'Customizing for S_RFCACL' ).
        lr_column->set_medium_text( 'Customizing S_RFCACL' ).
        lr_column->set_short_text(  'S_RFCACL' ).

        lr_column ?= lr_columns->get_column( 'S_RFCACL_ACTIVE_USERS' ).
        lr_column->set_long_text(   'Active users with critical S_RFCACL' ).
        lr_column->set_medium_text( 'Act. users S_RFCACL' ).
        lr_column->set_short_text(  'Act. users' ).
        lr_column->set_zero( abap_false  ).

        lr_column ?= lr_columns->get_column( 'S_RFCACL_INACTIVE_USERS' ).
        lr_column->set_long_text(   'Inactive users with critical S_RFCACL' ).
        lr_column->set_medium_text( 'Inact.users S_RFCACL' ).
        lr_column->set_short_text(  'Inact.user' ).
        lr_column->set_zero( abap_false  ).

*... hide unimportant columns
        lr_column ?= lr_columns->get_column( 'LONG_SID' ).                lr_column->set_visible( abap_true ).
        lr_column ?= lr_columns->get_column( 'SID' ).                     lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'TECH_SYSTEM_TYPE' ).        lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'TECH_SYSTEM_ID' ).          lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'LANDSCAPE_ID' ).            lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'HOST_FULL' ).               lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'HOST' ).                    lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'HOST_ID' ).                 lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'PHYSICAL_HOST' ).           lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'INSTANCE_TYPE' ).           lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'INSTANCE' ).                lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'COMPV_NAME' ).              lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'KERN_COMP_TIME' ).          lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'KERN_COMP_DATE' ).          lr_column->set_visible( abap_true ).

        lr_column ?= lr_columns->get_column( 'NOTE_3089413_PRSTATUS' ).   lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'NOTE_3287611_PRSTATUS' ).   lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'NOTE_3304520_PRSTATUS' ).   lr_column->set_technical( abap_true ).

        " Housekeeping stuff
        lr_column ?= lr_columns->get_column( 'STORE_NAME' ).              lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'STORE_ID' ).                lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'STORE_LAST_UPLOAD' ).       lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'STORE_STATE' ).             lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'STORE_MAIN_STATE_TYPE' ).   lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'STORE_MAIN_STATE' ).        lr_column->set_visible( abap_true ).
        lr_column ?= lr_columns->get_column( 'STORE_OUTDATED_DAY' ).      lr_column->set_visible( abap_false ).

        IF p_kern IS INITIAL.
          lr_column ?= lr_columns->get_column( 'KERN_REL' ).              lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'KERN_PATCHLEVEL' ).       lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'KERN_COMP_TIME' ).        lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'KERN_COMP_DATE' ).        lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'VALIDATE_KERNEL' ).       lr_column->set_technical( abap_true ).
        ENDIF.

        IF p_abap IS INITIAL.
          lr_column ?= lr_columns->get_column( 'ABAP_RELEASE' ).          lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'ABAP_SP' ).               lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'VALIDATE_ABAP' ).         lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'NOTE_3089413' ).          lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'NOTE_3089413_PRSTATUS' ). lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'NOTE_3287611' ).          lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'NOTE_3287611_PRSTATUS' ). lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'NOTE_3304520' ).          lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'NOTE_3304520_PRSTATUS' ). lr_column->set_technical( abap_true ).
        ENDIF.

        IF p_trust IS INITIAL.
          lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_ALL' ).       lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'NO_DATA_CNT' ).           lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'MUTUAL_TRUST_CNT' ).      lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_TCD' ).       lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_3' ).         lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_2' ).         lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_1' ).         lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'EXPLICIT_SELFTRUST' ).    lr_column->set_technical( abap_true ).

          lr_column ?= lr_columns->get_column( 'RFC_SELFTRUST' ).         lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'RFC_SENDINSTNR4TT' ).     lr_column->set_technical( abap_true ).
        ENDIF.

        IF p_dest IS INITIAL OR p_dest_3 IS INITIAL.
          lr_column ?= lr_columns->get_column( 'DEST_3_CNT_ALL' ).               lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED' ).           lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_MIGRATED' ).  lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_NO_INSTNR' ). lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_NO_SYSID' ).  lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_SNC' ).       lr_column->set_technical( abap_true ).
        ENDIF.

        IF p_dest IS INITIAL OR p_dest_h IS INITIAL.
          lr_column ?= lr_columns->get_column( 'DEST_H_CNT_ALL' ).               lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED' ).           lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_MIGRATED' ).  lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_NO_INSTNR' ). lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_NO_SYSID' ).  lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_TLS' ).       lr_column->set_technical( abap_true ).
        ENDIF.

        IF p_dest IS INITIAL OR p_dest_w IS INITIAL.
          lr_column ?= lr_columns->get_column( 'DEST_W_CNT_ALL' ).               lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED' ).           lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_MIGRATED' ).  lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_NO_INSTNR' ). lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_NO_SYSID' ).  lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_TLS' ).       lr_column->set_technical( abap_true ).
        ENDIF.

        IF p_auth IS INITIAL.
          lr_column ?= lr_columns->get_column( 'S_RFCACL_STATUS' ).              lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'S_RFCACL_ACTIVE_USERS' ).        lr_column->set_technical( abap_true ).
          lr_column ?= lr_columns->get_column( 'S_RFCACL_INACTIVE_USERS' ).      lr_column->set_technical( abap_true ).
        ENDIF.

        IF p_trust IS INITIAL AND p_dest IS INITIAL.
          lr_column ?= lr_columns->get_column( 'RFC_ALLOWOLDTICKET4TT' ). lr_column->set_technical( abap_true ).
        ENDIF.

      CATCH cx_salv_not_found.
    ENDTRY.

*... show it
    lr_alv_table->display( ).

  ENDMETHOD. " show_result

  METHOD show_trusted_systems.
    "IMPORTING
    "  column      type SALV_DE_COLUMN
    "  RFCTRUSTSY  type ts_result-sid
    "  LLICENSE_NR type ts_result-install_number

    " Show trusted systems
    CHECK column(12) = 'TRUSTSY_CNT_'
       OR column = 'MUTUAL_TRUST_CNT'
       OR column = 'NO_DATA_CNT'.

    DATA ls_trusted_system  TYPE ts_trusted_system.
    READ TABLE lt_trusted_systems ASSIGNING FIELD-SYMBOL(<fs_trusted_system>)
      WITH TABLE KEY
        rfctrustsy  = rfctrustsy
        llicense_nr = llicense_nr.
    CHECK sy-subrc = 0.

    DATA:
      ls_rfcsysacl_data TYPE ts_rfcsysacl_data,
      lt_rfcsysacl_data TYPE tt_rfcsysacl_data.

    " ALV
    DATA:
      lr_table TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lr_table
          CHANGING
            t_table      = lt_rfcsysacl_data ).
      CATCH cx_salv_msg.
    ENDTRY.

*... activate ALV generic Functions
    DATA(lr_functions) = lr_table->get_functions( ).
    lr_functions->set_all( abap_true ).

*... set the display settings
    DATA(lr_display) = lr_table->get_display_settings( ).
    TRY.
        "lr_display->set_list_header( sy-title ).
        "lr_display->set_list_header_size( CL_SALV_DISPLAY_SETTINGS=>C_HEADER_SIZE_LARGE ).
        lr_display->set_striped_pattern( abap_true ).
        lr_display->set_horizontal_lines( abap_true ).
        lr_display->set_vertical_lines( abap_true ).
        lr_display->set_suppress_empty_data( abap_true ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

*... set the functional settings
    DATA(lr_functional) = lr_table->get_functional_settings( ).
    TRY.
        lr_functional->set_sort_on_header_click( abap_true ).
        "lr_functional->set_f2_code( f2code ).
        "lr_functional->set_buffer( gs_test-settings-functional-buffer ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

* ...Set the layout
    "data(lr_layout) = lr_table->get_layout( ).
    "ls_layout_key-report = sy-repid.
    "lr_layout->set_key( ls_layout_key ).
    "lr_layout->set_initial_layout( P_LAYOUT ).
    "authority-check object 'S_ALV_LAYO'
    "                    id 'ACTVT' field '23'.
    "if sy-subrc = 0.
    "  lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ) . "no restictions
    "else.
    "  lr_layout->set_save_restriction( cl_salv_layout=>restrict_user_dependant ) . "user dependend
    "endif.

*... sort

*... set column appearance
    DATA(lr_columns) = lr_table->get_columns( ).
    lr_columns->set_optimize( abap_true ). " Optimize column width

*... set the color of cells
    TRY.
        lr_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

    " Copy relevant data
    CASE column.
      WHEN 'TRUSTSY_CNT_ALL'. " All trusted systems
        lr_display->set_list_header( `All trusted systems of system ` && rfctrustsy ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'NO_DATA_CNT'.   " No data for trusted system found
        lr_display->set_list_header( `No data for trusted system found of system ` && rfctrustsy ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE no_data IS NOT INITIAL.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'MUTUAL_TRUST_CNT'.   " Mutual trust relations
        lr_display->set_list_header( `Mutual trust relations with system ` && rfctrustsy ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE mutual_trust IS NOT INITIAL.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'TRUSTSY_CNT_TCD'. " Tcode active for trusted systems
        lr_display->set_list_header( `Tcode active for trusted systems of system ` && rfctrustsy ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE rfctcdchk = 'X'.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'TRUSTSY_CNT_3'.   " Migrated trusted systems
        lr_display->set_list_header( `Migrated trusted systems of system ` && rfctrustsy ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE rfcslopt(1) = '3'.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'TRUSTSY_CNT_2'.   " Old trusted systems
        lr_display->set_list_header( `Old trusted systems of system ` && rfctrustsy ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE rfcslopt(1) = '2'.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'TRUSTSY_CNT_1'.   " Very old trusted systems
        lr_display->set_list_header( `Very old trusted systems of system ` && rfctrustsy ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE rfcslopt(1) = ' '.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

    ENDCASE.

    " Set color
    LOOP AT lt_rfcsysacl_data ASSIGNING FIELD-SYMBOL(<fs_rfcsysacl_data>).
      IF <fs_rfcsysacl_data>-mutual_trust IS NOT INITIAL.
        APPEND VALUE #( fname = 'MUTUAL_TRUST' color-col = col_total ) TO <fs_rfcsysacl_data>-t_color.
      ENDIF.

      IF <fs_rfcsysacl_data>-rfctcdchk IS NOT INITIAL.
        APPEND VALUE #( fname = 'RFCTCDCHK' color-col = col_positive ) TO <fs_rfcsysacl_data>-t_color.
      ENDIF.

      IF <fs_rfcsysacl_data>-rfcslopt(1) = '3'.
        APPEND VALUE #( fname = 'RFCSLOPT' color-col = col_positive ) TO <fs_rfcsysacl_data>-t_color.
      ELSE.
        APPEND VALUE #( fname = 'RFCSLOPT' color-col = col_negative ) TO <fs_rfcsysacl_data>-t_color.
      ENDIF.
    ENDLOOP.

    TRY.
        DATA lr_column TYPE REF TO cl_salv_column_table.        " Columns in Simple, Two-Dimensional Tables

        lr_column ?= lr_columns->get_column( 'RFCSYSID' ).
        lr_column->set_long_text( 'Trusted system' ).
        lr_column->set_medium_text( 'Trusted system' ).
        lr_column->set_short_text( 'Trusted' ).
        lr_column->set_key( abap_true ).

        lr_column ?= lr_columns->get_column( 'RFCTRUSTSY' ).
        lr_column->set_long_text( 'Trusting system' ).
        lr_column->set_medium_text( 'Trusting system' ).
        lr_column->set_short_text( 'Trusting' ).
        lr_column->set_key( abap_true ).

        lr_column ?= lr_columns->get_column( 'RFCSLOPT' ).
        lr_column->set_long_text( 'Version: 3=migrated, 2=old,  =very old' ).
        lr_column->set_medium_text( 'Version' ).
        lr_column->set_short_text( 'Version' ).

        lr_column ?= lr_columns->get_column( 'NO_DATA' ).
        lr_column->set_long_text( 'No data of trusted system found' ).
        lr_column->set_medium_text( 'No data found' ).
        lr_column->set_short_text( 'No data' ).

        lr_column ?= lr_columns->get_column( 'MUTUAL_TRUST' ).
        lr_column->set_long_text( 'Mutual trust relation' ).
        lr_column->set_medium_text( 'Mutual trust' ).
        lr_column->set_short_text( 'Mutual' ).

        lr_column ?= lr_columns->get_column( 'RFCDEST' ).
        lr_column->set_long_text( 'Destination to trusted system' ).
        lr_column->set_medium_text( 'Dest. to trusted sys' ).
        lr_column->set_short_text( 'Dest.Trust' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'TRUSTED_DEST_CNT' ).
        lr_column->set_long_text( 'Count of migrated trusted destinations' ).
        lr_column->set_medium_text( 'Trusted destinations' ).
        lr_column->set_short_text( 'TrustDest.' ).
        lr_column->set_zero( abap_false  ).

        "lr_column ?= lr_columns->get_column( 'TLICENSE_NR' ).     lr_column->set_technical( abap_true ).
        "lr_column ?= lr_columns->get_column( 'LLICENSE_NR' ).     lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'RFCCREDEST' ).      lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'RFCREGDEST' ).      lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'RFCSNC' ).          lr_column->set_technical( abap_true ).
        "lr_column ?= lr_columns->get_column( 'RFCSECKEY' ).       lr_column->set_technical( abap_true ).

      CATCH cx_salv_not_found.
    ENDTRY.

    " Show it
    lr_table->display( ).

  ENDMETHOD. " show_trusted_systems

  METHOD show_destinations.
    "IMPORTING
    "  column      TYPE salv_de_column
    "  sid         TYPE ts_result-sid
    "  install_number TYPE ts_result-install_number

    " Show trusted systems
    CHECK column(11) = 'DEST_3_CNT_'
       OR column(11) = 'DEST_H_CNT_'
       OR column(11) = 'DEST_W_CNT_'.

    DATA ls_destination  TYPE ts_destination.
    READ TABLE lt_destinations ASSIGNING FIELD-SYMBOL(<fs_destination>)
      WITH TABLE KEY
        sid            = sid
        install_number = install_number.
    CHECK sy-subrc = 0.

    DATA:
      ls_destination_data TYPE ts_destination_data,
      lt_destination_data TYPE tt_destination_data.

    " ALV
    DATA:
      lr_table TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lr_table
          CHANGING
            t_table      = lt_destination_data ).
      CATCH cx_salv_msg.
    ENDTRY.

*... activate ALV generic Functions
    DATA(lr_functions) = lr_table->get_functions( ).
    lr_functions->set_all( abap_true ).

*... set the display settings
    DATA(lr_display) = lr_table->get_display_settings( ).
    TRY.
        "lr_display->set_list_header( sy-title ).
        "lr_display->set_list_header_size( CL_SALV_DISPLAY_SETTINGS=>C_HEADER_SIZE_LARGE ).
        lr_display->set_striped_pattern( abap_true ).
        lr_display->set_horizontal_lines( abap_true ).
        lr_display->set_vertical_lines( abap_true ).
        lr_display->set_suppress_empty_data( abap_true ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

*... set the functional settings
    DATA(lr_functional) = lr_table->get_functional_settings( ).
    TRY.
        lr_functional->set_sort_on_header_click( abap_true ).
        "lr_functional->set_f2_code( f2code ).
        "lr_functional->set_buffer( gs_test-settings-functional-buffer ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

* ...Set the layout
    "data(lr_layout) = lr_table->get_layout( ).
    "ls_layout_key-report = sy-repid.
    "lr_layout->set_key( ls_layout_key ).
    "lr_layout->set_initial_layout( P_LAYOUT ).
    "authority-check object 'S_ALV_LAYO'
    "                    id 'ACTVT' field '23'.
    "if sy-subrc = 0.
    "  lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ) . "no restictions
    "else.
    "  lr_layout->set_save_restriction( cl_salv_layout=>restrict_user_dependant ) . "user dependend
    "endif.

*... sort

*... set column appearance
    DATA(lr_columns) = lr_table->get_columns( ).
    lr_columns->set_optimize( abap_true ). " Optimize column width

*... set the color of cells
    TRY.
        lr_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

    " Copy relevant data
    CASE column.
      WHEN 'DEST_3_CNT_ALL'.
        lr_display->set_list_header( `All RFC destinations (type 3) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED'.
        lr_display->set_list_header( `Trusted RFC destinations (type 3) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED_MIGRATED'.
        lr_display->set_list_header( `Migrated RFC destinations (type 3) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED_NO_INSTNR'.
        lr_display->set_list_header( `Missing installation number in RFC destinations (type 3) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED_NO_SYSID'.
        lr_display->set_list_header( `Missing system id in RFC destinations (type 3) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED_SNC'.
        lr_display->set_list_header( `Encrypted trusted RFC destinations (type 3) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted IS NOT INITIAL
            AND rfcsnc IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.


      WHEN 'DEST_H_CNT_ALL'.
        lr_display->set_list_header( `All http destinations (type H) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED'.
        lr_display->set_list_header( `Trusted http destinations (type H) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED_MIGRATED'.
        lr_display->set_list_header( `Migrated http destinations (type H) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED_NO_INSTNR'.
        lr_display->set_list_header( `Missing installation number in http destinations (type H) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED_NO_SYSID'.
        lr_display->set_list_header( `Missing system id in http destinations (type H) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED_TLS'.
        lr_display->set_list_header( `Encrypted trusted http destinations (type H) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted IS NOT INITIAL
            AND rfcsnc IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.


      WHEN 'DEST_W_CNT_ALL'.
        lr_display->set_list_header( `All WebRFC destinations (type W) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED'.
        lr_display->set_list_header( `Trusted WebRFC destinations (type W) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED_MIGRATED'.
        lr_display->set_list_header( `Migrated WebRFC destinations (type W) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED_NO_INSTNR'.
        lr_display->set_list_header( `Missing installation number in WebRFC destinations (type W) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED_NO_SYSID'.
        lr_display->set_list_header( `Missing system id in WebRFC destinations (type W) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED_TLS'.
        lr_display->set_list_header( `Encrypted trusted WebRFC destinations (type W) of system ` && sid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted IS NOT INITIAL
            AND rfcsnc IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

    ENDCASE.

    " Set color
    LOOP AT lt_destination_data ASSIGNING FIELD-SYMBOL(<fs_destination_data>)
      WHERE trusted IS NOT INITIAL.

      IF <fs_destination_data>-serversysid IS NOT INITIAL.
        APPEND VALUE #( fname = 'SERVERSYSID' color-col = col_positive ) TO <fs_destination_data>-t_color.
      ELSE.
        APPEND VALUE #( fname = 'SERVERSYSID' color-col = col_negative ) TO <fs_destination_data>-t_color.
      ENDIF.

      IF <fs_destination_data>-serverinstnr IS NOT INITIAL.
        APPEND VALUE #( fname = 'SERVERINSTNR' color-col = col_positive ) TO <fs_destination_data>-t_color.
      ELSE.
        APPEND VALUE #( fname = 'SERVERINSTNR' color-col = col_negative ) TO <fs_destination_data>-t_color.
      ENDIF.

      CASE <fs_destination_data>-check_trusted.
        WHEN 'missing'.   APPEND VALUE #( fname = 'CHECK_TRUSTED' color-col = col_negative ) TO <fs_destination_data>-t_color.
        WHEN 'very old'
          OR 'old'.       APPEND VALUE #( fname = 'CHECK_TRUSTED' color-col = col_total )    TO <fs_destination_data>-t_color.
        WHEN 'migrated'.  APPEND VALUE #( fname = 'CHECK_TRUSTED' color-col = col_positive ) TO <fs_destination_data>-t_color.
      ENDCASE.
    ENDLOOP.

    TRY.
        DATA lr_column TYPE REF TO cl_salv_column_table.        " Columns in Simple, Two-Dimensional Tables

        lr_column ?= lr_columns->get_column( 'RFCDEST' ).
        lr_column->set_long_text( 'Destination' ).
        lr_column->set_medium_text( 'Destination' ).
        lr_column->set_short_text( 'Dest.' ).
        lr_column->set_key( abap_true ).

        lr_column ?= lr_columns->get_column( 'RFCTYPE' ).

        lr_column ?= lr_columns->get_column( 'TRUSTED' ).
        lr_column->set_long_text( 'Trusted destination' ).
        lr_column->set_medium_text( 'Trusted dest.' ).
        lr_column->set_short_text( 'Trusted' ).
        lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

        lr_column ?= lr_columns->get_column( 'RFCSLOGIN' ).
        lr_column->set_long_text( 'Logon Procedure (A, B, Y)' ).
        lr_column->set_medium_text( 'Logon Procedure' ).
        lr_column->set_short_text( 'LogonProc.' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'SERVERSYSID' ).
        lr_column->set_long_text( 'System ID' ).
        lr_column->set_medium_text( 'System ID' ).
        lr_column->set_short_text( 'System ID' ).

        lr_column ?= lr_columns->get_column( 'SERVERINSTNR' ).
        lr_column->set_long_text( 'Installation number' ).
        lr_column->set_medium_text( 'Installation nr' ).
        lr_column->set_short_text( 'Inst. nr' ).

        lr_column ?= lr_columns->get_column( 'CHECK_TRUSTED' ).
        lr_column->set_long_text( 'Check trusted relation in trusting sys.' ).
        lr_column->set_medium_text( 'Check trusted rel.' ).
        lr_column->set_short_text( 'CheckTrust' ).
        IF p_trust IS INITIAL.
          lr_column->set_technical( abap_true ).
        ENDIF.

        lr_column ?= lr_columns->get_column( 'RFCHOST' ).

        lr_column ?= lr_columns->get_column( 'RFCSERVICE' ).

        lr_column ?= lr_columns->get_column( 'RFCSYSID' ).

        lr_column ?= lr_columns->get_column( 'RFCCLIENT' ).

        lr_column ?= lr_columns->get_column( 'RFCSAMEUSR' ).
        lr_column->set_long_text( 'Same user' ).
        lr_column->set_medium_text( 'Same user' ).
        lr_column->set_short_text( 'Same user' ).
        lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

        lr_column ?= lr_columns->get_column( 'RFCUSER' ).

        lr_column ?= lr_columns->get_column( 'RFCAUTH' ).

        lr_column ?= lr_columns->get_column( 'RFCSNC' ).
        lr_column->set_long_text( 'Encrypted using SNC/TLS' ).
        lr_column->set_medium_text( 'Encrypted (SNC/TLS)' ).
        lr_column->set_short_text( 'Encrypted' ).
        lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

        lr_column ?= lr_columns->get_column( 'SSLAPPLIC' ).

        lr_column ?= lr_columns->get_column( 'NOCCERT' ).
        lr_column->set_long_text( 'Send no client certificate' ).
        lr_column->set_medium_text( 'No client cert.' ).
        lr_column->set_short_text( 'No Cl.Cert' ).

        " Hide TLS columns for type 3 destinations
        IF column+5(1) = '3'. " 'DEST_3_CNT_ALL'

          lr_column ?= lr_columns->get_column( 'SSLAPPLIC' ).
          lr_column->set_visible( abap_false ).

          lr_column ?= lr_columns->get_column( 'NOCCERT' ).
          lr_column->set_visible( abap_false ).

        ENDIF.

      CATCH cx_salv_not_found.
    ENDTRY.

    " Show it
    lr_table->display( ).

  ENDMETHOD. " show_destinations

  METHOD show_critical_users.
    "IMPORTING
    "  column      TYPE salv_de_column
    "  sid         TYPE ts_result-sid
    "  install_number TYPE ts_result-install_number

    " Show trusted systems
    CHECK column = 'S_RFCACL_ACTIVE_USERS'
       OR column = 'S_RFCACL_INACTIVE_USERS'.

    DATA ls_critical_user  TYPE ts_critical_user.
    READ TABLE lt_critical_users ASSIGNING FIELD-SYMBOL(<fs_critical_users>)
      WITH TABLE KEY
        sid            = sid
        install_number = install_number.
    CHECK sy-subrc = 0.

    DATA:
      ls_user_data TYPE ts_user_data,
      lt_user_data TYPE tt_user_data.

    " ALV
    DATA:
      lr_table TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lr_table
          CHANGING
            t_table      = lt_user_data ).
      CATCH cx_salv_msg.
    ENDTRY.

*... activate ALV generic Functions
    DATA(lr_functions) = lr_table->get_functions( ).
    lr_functions->set_all( abap_true ).

*... set the display settings
    DATA(lr_display) = lr_table->get_display_settings( ).
    TRY.
        "lr_display->set_list_header( sy-title ).
        "lr_display->set_list_header_size( CL_SALV_DISPLAY_SETTINGS=>C_HEADER_SIZE_LARGE ).
        lr_display->set_striped_pattern( abap_true ).
        lr_display->set_horizontal_lines( abap_true ).
        lr_display->set_vertical_lines( abap_true ).
        lr_display->set_suppress_empty_data( abap_true ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

*... set the functional settings
    DATA(lr_functional) = lr_table->get_functional_settings( ).
    TRY.
        lr_functional->set_sort_on_header_click( abap_true ).
        "lr_functional->set_f2_code( f2code ).
        "lr_functional->set_buffer( gs_test-settings-functional-buffer ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

* ...Set the layout
    "data(lr_layout) = lr_table->get_layout( ).
    "ls_layout_key-report = sy-repid.
    "lr_layout->set_key( ls_layout_key ).
    "lr_layout->set_initial_layout( P_LAYOUT ).
    "authority-check object 'S_ALV_LAYO'
    "                    id 'ACTVT' field '23'.
    "if sy-subrc = 0.
    "  lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ) . "no restictions
    "else.
    "  lr_layout->set_save_restriction( cl_salv_layout=>restrict_user_dependant ) . "user dependend
    "endif.

*... sort

*... set column appearance
    DATA(lr_columns) = lr_table->get_columns( ).
    lr_columns->set_optimize( abap_true ). " Optimize column width

*... set the color of cells
    TRY.
        lr_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

    " Copy relevant data
    CASE column.
      WHEN 'S_RFCACL_ACTIVE_USERS'.
        lr_display->set_list_header( `Active users having critical auth. for S_RFCACL in system ` && sid ).

        LOOP AT <fs_critical_users>-user_data INTO ls_user_data
          WHERE locked IS INITIAL
            AND invalid IS INITIAL.
          APPEND ls_user_data TO lt_user_data.
        ENDLOOP.

      WHEN 'S_RFCACL_INACTIVE_USERS'.
        lr_display->set_list_header( `Inactive users having critical auth. for S_RFCACL in system ` && sid ).

        LOOP AT <fs_critical_users>-user_data INTO ls_user_data
          WHERE locked IS NOT INITIAL
             OR invalid IS NOT INITIAL.
          APPEND ls_user_data TO lt_user_data.
        ENDLOOP.
    ENDCASE.

    " Set color

    TRY.
        DATA lr_column TYPE REF TO cl_salv_column_table.        " Columns in Simple, Two-Dimensional Tables

        lr_column ?= lr_columns->get_column( 'CLIENT' ).
        "lr_column->set_long_text( 'Client' ).
        "lr_column->set_medium_text( 'Client' ).
        "lr_column->set_short_text( 'Client' ).
        lr_column->set_key( abap_true ).

        lr_column ?= lr_columns->get_column( 'RC' ).
        lr_column->set_long_text( 'Status per client' ).
        lr_column->set_medium_text( 'Status per client' ).
        lr_column->set_short_text( 'Status' ).
        lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'USER' ).
        "lr_column->set_long_text( 'User' ).
        "lr_column->set_medium_text( 'User' ).
        "lr_column->set_short_text( 'User' ).
        lr_column->set_key( abap_true ).

        lr_column ?= lr_columns->get_column( 'LOCKED' ).
        lr_column->set_long_text( 'Locked' ).
        lr_column->set_medium_text( 'Locked' ).
        lr_column->set_short_text( 'Locked' ).

        lr_column ?= lr_columns->get_column( 'INVALID' ).
        lr_column->set_long_text( 'Invalid' ).
        lr_column->set_medium_text( 'Invalid' ).
        lr_column->set_short_text( 'Invalid' ).

        lr_column ?= lr_columns->get_column( 'USER_TYPE' ).
        lr_column->set_long_text( 'User type' ).
        lr_column->set_medium_text( 'User type' ).
        lr_column->set_short_text( 'User type' ).

        lr_column ?= lr_columns->get_column( 'USER_GROUP' ).

      CATCH cx_salv_not_found.
    ENDTRY.

    " Show it
    lr_table->display( ).

  ENDMETHOD. " show_critical_users

ENDCLASS.                    "lcl_report IMPLEMENTATION

*----------------------------------------------------------------------*
*      REPORT events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sid-low.
  lcl_report=>f4_s_sid( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sid-high.
  lcl_report=>f4_s_sid( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  lcl_report=>f4_p_layout( CHANGING layout = p_layout ).

AT SELECTION-SCREEN ON p_layout.
  CHECK p_layout IS NOT INITIAL.
  lcl_report=>at_selscr_on_p_layout( p_layout ).

START-OF-SELECTION.
  lcl_report=>start_of_selection( ).
