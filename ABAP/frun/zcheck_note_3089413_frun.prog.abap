*&---------------------------------------------------------------------*
*& Report  ZCHECK_NOTE_3089413_FRUN
*& Check implementation status of note 3089413 for connected ABAP systems
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Source: https://github.com/SAP-samples/security-services-tools
*&
*& 10.07.2023 Updated Kernel prerequisites as described in note 3224161
*&            Updated Note prerequisites for note 3287611 v9
*& 13.03.2023 Updated note 3287611, new note 3304520
*& 27.02.2023 Initial version based on the similar report for the SAP Solution Manager
*&            No API is not used but direct DB access,therefore further changes are recommended
*&            Reason: Function CCDB_GET_STORES work for a single system name only
*&---------------------------------------------------------------------*
REPORT zcheck_note_3089413_frun.

CONSTANTS c_program_version(30) TYPE c VALUE '10.07.2023 FQ4'.

TYPE-POOLS: icon, col, sym.

* System name
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ss_sid FOR FIELD p_EXTSID.
  SELECT-OPTIONS p_EXTSID   FOR ('LMDB_ESID').        " Extended System ID
SELECTION-SCREEN END OF LINE.

* Check Kernel
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS       p_kern AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 3(33) ps_kern FOR FIELD p_kern.
SELECTION-SCREEN END OF LINE.

* Check ABAP
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS       p_abap AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 3(33) ps_abap FOR FIELD p_abap.
SELECTION-SCREEN END OF LINE.

* Check trusted relations
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS       p_trust AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 3(33) ps_trust FOR FIELD p_trust.
SELECTION-SCREEN END OF LINE.

* Check trusted destinations
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS       p_dest AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 3(33) ps_dest FOR FIELD p_dest.
SELECTION-SCREEN END OF LINE.
* Show specific type only if data found
DATA p_dest_3 TYPE abap_bool.
DATA p_dest_h TYPE abap_bool.
DATA p_dest_w TYPE abap_bool.

* Store status
"SELECTION-SCREEN BEGIN OF LINE.
"SELECTION-SCREEN COMMENT 1(30) ss_state FOR FIELD p_state.
"SELECT-OPTIONS p_state FOR sel_store_dir-store_main_state_type." DEFAULT 'G'.
"SELECTION-SCREEN END OF LINE.

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

    " Key
    extsid                       TYPE lmdb_esid, " from CCDB_ISCI-EXTSID,
    sid                          TYPE lmdb_esid,
    install_number               TYPE lmdb_installation_no,

    " Source store: SAP_KERNEL
    kern_rel                     TYPE string,                               " 722_EXT_REL
    kern_patchlevel              TYPE string,                               " 1000
    kern_comp_time               TYPE string,                               " Jun  7 2020 15:44:10
    kern_comp_date               TYPE sy-datum,

    validate_kernel              TYPE string,

    " Source store: ABAP_COMP_SPLEVEL
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

    " Source store: we show the status of the first found store only which is usually store SAP_KERNEL
    store_name                   TYPE ccdb_store-store_name,
    store_id                     TYPE ccdb_store-store_id,
    store_to_date                TYPE ccdb_store-store_to_date,

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
    extsid           TYPE lmdb_esid,
    sid              TYPE diagls_technical_system_sid,  "sdiagst_store_dir-sid,
    install_number   TYPE diagls_tech_syst_install_nbr,
    destination_data TYPE tt_destination_data,
  END OF ts_destination,
  tt_destinations TYPE STANDARD TABLE OF ts_destination WITH KEY sid install_number,

  " Mapping between extended SID and SID with installation number
  BEGIN OF ts_EXTSID_data,
    extsid         TYPE lmdb_esid,
    rt_cim_guid    TYPE cof_rt_tree_p-rt_cim_guid,
    hier_id        TYPE cof_rt_tree_p-hier_id,
    "system_type     TYPE lmdb_system_type,
    sid            TYPE lmdb_esid,
    install_number TYPE lmdb_installation_no,
  END OF ts_EXTSID_data,
  tt_EXTSID_DATA TYPE STANDARD TABLE OF ts_EXTSID_data WITH KEY extsid.

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      initialization,

      f4_EXTSID,

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
      " Mapping between extended SID and SID with installation number
      lt_EXTSID_data     TYPE tt_EXTSID_data.

    CLASS-DATA:
      " main ALV table
      lr_alv_table  TYPE REF TO cl_salv_table,
      " for handling the events on the main ALV table
      lr_alv_events TYPE REF TO lcl_report.

    CLASS-METHODS:

      get_EXTSID_data,

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

      validate_kernel,

      validate_abap,

      validate_mutual_trust,

      count_dest_per_trust,

      validate_trust_for_dest,

      show_result,

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

*      on_single_click for event link_click of cl_salv_events_table
*        importing row column,

      show_trusted_systems
        IMPORTING
          column         TYPE salv_de_column
          extsid         TYPE ts_result-extsid
          rfctrustsy     TYPE ts_result-sid
          llicense_nr    TYPE ts_result-install_number,

      show_destinations
        IMPORTING
          column         TYPE salv_de_column
          extsid         TYPE ts_result-extsid
          sid            TYPE ts_result-sid
          install_number TYPE ts_result-install_number.

ENDCLASS.                    "lcl_report DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    sy-title = 'Check implementation status of note 3089413 for connected ABAP systems'(tit).

    ss_sid   = 'System'.
    "ss_state = 'Config. store status (G/Y/R)'.

    ps_kern  = 'Check Kernel'.
    ps_abap  = 'Check Support Package and Notes'.
    ps_trust = 'Check Trusted Relations'.
    ps_dest  = 'Check Trusted Destinations'.

    ps_lout  = 'Layout'.

    CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
       SEPARATED BY space.

  ENDMETHOD. " initialization

  METHOD f4_EXTSID.

    TYPES:
      BEGIN OF ts_f4_value,
        extsid TYPE ccdb_isci-extsid,
      END OF ts_f4_value.

    DATA:
      f4_value     TYPE          ts_f4_value,
      f4_value_tab TYPE TABLE OF ts_f4_value.

    DATA:
      rc      TYPE  i,
      rc_text TYPE  natxt.

    SELECT DISTINCT extsid FROM ccdb_isci INTO TABLE f4_value_tab
      ORDER BY extsid.

    DATA(progname) = sy-repid.
    DATA(dynnum)   = sy-dynnr.
    DATA field TYPE dynfnam.
    DATA stepl TYPE sy-stepl.
    GET CURSOR FIELD field LINE stepl.
    DATA return_tab TYPE TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'EXTSID'
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

  ENDMETHOD. " f4_EXTSID

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

    get_EXTSID_data( ).        " Mapping between extended SID and SID with installation number

    get_sap_kernel( ).         " Kernel version
    get_abap_comp_splevel( ).  " Support Package version of SAP_BASIS
    get_abap_notes( ).         " Notes 3089413 and 3287611
    get_rfcsysacl( ).          " Trusting relations
    get_rfcdes( ).             " Trusted desinations
    get_abap_instance_pahi( ). " rfc/selftrust

    validate_kernel( ).
    validate_abap( ).
    count_dest_per_trust( ).
    validate_trust_for_dest( ).
    validate_mutual_trust( ).

    show_result( ).

  ENDMETHOD. " start_of_selection

  METHOD get_EXTSID_data.

    " Option 1: Class cl_lmdb_filterbar_dpc_util->get_technical_Systems()
    " Option 2: Table COF_RT_TREE_P (only usable for CSA data)

    DATA ls_extsid_data TYPE ts_extsid_data.

    " Get GUID and HIER_ID of ABAP systems
    SELECT
        rt_cim_guid,
        hier_id
      FROM cof_rt_tree_p
      INTO TABLE @DATA(lt_GUIDs)
      WHERE name  = 'SYSTEM_TYPE'
        AND value = 'ABAP'.

    " Get data
    SELECT *
      FROM cof_rt_tree_p
      INTO @DATA(ls_cof_rt_tree_p)
      FOR ALL ENTRIES IN @lt_GUIDs
      WHERE rt_cim_guid = @lt_GUIDs-rt_cim_guid
        AND hier_id     = @lt_GUIDs-hier_id
        AND (   name = 'ESID'
             OR name = 'TS_INSTALL_NUMBER'
             OR name = 'TS_SID' )
      ORDER BY PRIMARY KEY. " Important to the the correct order of CASE events per system

      CASE ls_cof_rt_tree_p-name.
        WHEN 'ESID'.
          CLEAR ls_extsid_data.
          ls_extsid_data-extsid         = ls_cof_rt_tree_p-value.
          ls_extsid_data-rt_cim_guid    = ls_cof_rt_tree_p-rt_cim_guid.
          ls_extsid_data-hier_id        = ls_cof_rt_tree_p-hier_id.
        WHEN 'TS_INSTALL_NUMBER'.
          ls_extsid_data-install_number = ls_cof_rt_tree_p-value.
        WHEN 'TS_SID'.
          ls_extsid_data-sid            = ls_cof_rt_tree_p-value.
          APPEND ls_extsid_data TO lt_extsid_data.
      ENDCASE.
    ENDSELECT.
    SORT lt_extsid_data BY extsid.

  ENDMETHOD. " get_EXTSID_data

  METHOD get_sap_kernel.
    CHECK p_kern = 'X'.

    DATA tabix TYPE sy-tabix.

    " Get stores
    " Function CCDB_GET_STORES work for a single system name only
*    CALL FUNCTION 'CCDB_GET_STORES'
*      EXPORTING
**       RT_CIM_GUID               =
**       RT_CIM_NSPA               =
**       EXTSID                    IN @p_extsid   " System name
*        SYSTEM_TYPE               = 'ABAP'       " System type
**       STORE_CIM_GUID            =
*        SCI_ID                    = 'S00001'     " Store id
**       STORE_NAME_EXT            = '*'
*        STORE_NAME                = 'SAP_KERNEL' " Store name
**       CALLER                    =
**       DISPLAY                   =
**     IMPORTING
**       CCDB_DIRV                 =              " Store directory
**       CCDB_DIRV_ATTR            =              " Attribute host name
**       CCDB_DIRV_CXML            =
**       CCDB_DIRV_ATTR_CXML       =
**       RC                        =
**       RC_STRING                 =
*          .
    SELECT *
      FROM ccdb_rt_dirv_dbv                " CCDB: Root object - Directory
      INTO TABLE @DATA(lt_storetab)
     WHERE rt_type          = 'TS'         " Root Type
       AND extsid           IN @p_extsid   " System name
       AND system_type      = 'ABAP'       " System type
       AND sci_id           = 'S00001'     " Store id
       AND store_name       = 'SAP_KERNEL' " Store name
     ORDER BY extsid .
    CHECK sy-subrc IS INITIAL.

    " Use table name from first store
    READ TABLE lt_storetab INTO DATA(ls_storetab) INDEX 1.
    CHECK sy-subrc IS INITIAL.

    " Create data table
    DATA(ddic_tablename) = ls_storetab-ddic_tablename.
    DATA ref_tab TYPE REF TO data.
    CREATE DATA ref_tab TYPE STANDARD TABLE OF (ddic_tablename).
    FIELD-SYMBOLS <fs_table> TYPE ANY TABLE.
    ASSIGN ref_tab->* TO <fs_table>.

    LOOP AT lt_storetab INTO ls_storetab
      WHERE ddic_tablename = ddic_tablename.

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          extsid = ls_storetab-extsid.
      IF sy-subrc = 0.
        tabix = sy-tabix.
        "MOVE-CORRESPONDING ls_storetab TO ls_result.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_storetab TO ls_result. " EXTSID, STORE_NAME, STORE_ID, STORE_TO_DATE
        READ TABLE lt_extsid_data INTO DATA(ls_extsid_data)
          WITH TABLE KEY
            extsid = ls_storetab-extsid.
        IF sy-subrc = 0.
          ls_result-sid            = ls_extsid_data-sid.
          ls_result-install_number = ls_extsid_data-install_number.
        ENDIF.
      ENDIF.

      " Get data
      SELECT *
        FROM (ddic_tablename) " CCDB_DATA_001
        WHERE cd_store_id = @ls_storetab-store_id
          and CD_HIST_DATE_TO is initial             " ignore old change documents
        ORDER BY PRIMARY KEY
        INTO TABLE @<fs_table>.

      LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<entry>).

        " Table CCDB_DATA_001
        ASSIGN COMPONENT 'NAME'  OF STRUCTURE <entry> TO FIELD-SYMBOL(<name>).
        ASSIGN COMPONENT 'VALUE' OF STRUCTURE <entry> TO FIELD-SYMBOL(<value>).

        " Copy data
        CASE <name>.

          WHEN 'KERN_COMP_ON'.      " Linux GNU SLES-11 x86_64 cc4.3.4 use-pr190909
            " not used yet

          WHEN 'KERN_COMP_TIME'.    " Jun  7 2020 15:44:10
            ls_result-kern_comp_time  = <value>.
            ls_result-kern_comp_date  = convert_comp_time( ls_result-kern_comp_time ).

          WHEN 'KERN_DBLIB'.        " SQLDBC 7.9.8.040
            " not used yet

          WHEN 'KERN_PATCHLEVEL'.   " 1000
            ls_result-kern_patchlevel = <value>.

          WHEN 'KERN_REL'.          " 722_EXT_REL
            ls_result-kern_rel        = <value>.

          WHEN 'PLATFORM-ID'.       " 390
            " not used yet

        ENDCASE.

      ENDLOOP. " <fs_table>

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_storetab

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

    DATA tabix TYPE i.

    " Get stores
    SELECT *
      FROM ccdb_rt_dirv_dbv                " CCDB: Root object - Directory
      INTO TABLE @DATA(lt_storetab)
     WHERE rt_type          = 'TS'         " Root Type
       AND extsid           IN @p_extsid   " System name
       AND system_type      = 'ABAP'       " System type
       AND sci_id           = 'S00120'     " Store id
       AND store_name       = 'COMP_LEVEL' " Store name
     ORDER BY extsid .
    CHECK sy-subrc IS INITIAL.

    " Use table name from first store
    READ TABLE lt_storetab INTO DATA(ls_storetab) INDEX 1.
    CHECK sy-subrc IS INITIAL.

    " Create data table
    DATA(ddic_tablename) = ls_storetab-ddic_tablename.
    DATA ref_tab TYPE REF TO data.
    CREATE DATA ref_tab TYPE STANDARD TABLE OF (ddic_tablename).
    FIELD-SYMBOLS <fs_table> TYPE ANY TABLE.
    ASSIGN ref_tab->* TO <fs_table>.

    LOOP AT lt_storetab INTO ls_storetab
      WHERE ddic_tablename = ddic_tablename.

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          extsid = ls_storetab-extsid.
      IF sy-subrc = 0.
        tabix = sy-tabix.
        "MOVE-CORRESPONDING ls_storetab TO ls_result.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_storetab TO ls_result. " EXTSID, STORE_NAME, STORE_ID, STORE_TO_DATE
        READ TABLE lt_extsid_data INTO DATA(ls_extsid_data)
          WITH TABLE KEY
            extsid = ls_storetab-extsid.
        IF sy-subrc = 0.
          ls_result-sid            = ls_extsid_data-sid.
          ls_result-install_number = ls_extsid_data-install_number.
        ENDIF.
      ENDIF.

      " Get data
      SELECT *
        FROM (ddic_tablename) " CCDB_DATA_110
        WHERE cd_store_id = @ls_storetab-store_id
          and CD_HIST_DATE_TO is initial             " ignore old change documents
          AND component = 'SAP_BASIS'
        ORDER BY PRIMARY KEY
        INTO TABLE @<fs_table>.

      LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<entry>).

        " Table CCDB_DATA_110
        ASSIGN COMPONENT 'COMPONENT' OF STRUCTURE <entry> TO FIELD-SYMBOL(<component>).
        ASSIGN COMPONENT 'VERSION'   OF STRUCTURE <entry> TO FIELD-SYMBOL(<release>).
        ASSIGN COMPONENT 'SP'        OF STRUCTURE <entry> TO FIELD-SYMBOL(<sp>).

        " Copy data
        ls_result-abap_release = <release>.
        ls_result-abap_sp      = <sp>.

      ENDLOOP. " <fs_table>

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_storetab

  ENDMETHOD. " get_ABAP_COMP_SPLEVEL

  METHOD get_abap_notes.
    CHECK p_abap = 'X'.

    DATA tabix TYPE i.

    " Get stores
    SELECT *
      FROM ccdb_rt_dirv_dbv                " CCDB: Root object - Directory
      INTO TABLE @DATA(lt_storetab)
     WHERE rt_type          = 'TS'         " Root Type
       AND extsid           IN @p_extsid   " System name
       AND system_type      = 'ABAP'       " System type
       AND sci_id           = 'S00028'     " Store id
       AND store_name       = 'ABAP_NOTES' " Store name
     ORDER BY extsid .
    CHECK sy-subrc IS INITIAL.

    " Use table name from first store
    READ TABLE lt_storetab INTO DATA(ls_storetab) INDEX 1.
    CHECK sy-subrc IS INITIAL.

    " Create data table
    DATA(ddic_tablename) = ls_storetab-ddic_tablename.
    DATA ref_tab TYPE REF TO data.
    CREATE DATA ref_tab TYPE STANDARD TABLE OF (ddic_tablename).
    FIELD-SYMBOLS <fs_table> TYPE ANY TABLE.
    ASSIGN ref_tab->* TO <fs_table>.

    LOOP AT lt_storetab INTO ls_storetab
      WHERE ddic_tablename = ddic_tablename.

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          extsid = ls_storetab-extsid.
      IF sy-subrc = 0.
        tabix = sy-tabix.
        "MOVE-CORRESPONDING ls_storetab TO ls_result.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_storetab TO ls_result. " EXTSID, STORE_NAME, STORE_ID, STORE_TO_DATE
        READ TABLE lt_extsid_data INTO DATA(ls_extsid_data)
          WITH TABLE KEY
            extsid = ls_storetab-extsid.
        IF sy-subrc = 0.
          ls_result-sid            = ls_extsid_data-sid.
          ls_result-install_number = ls_extsid_data-install_number.
        ENDIF.
      ENDIF.

      " Get data
      SELECT *
        FROM (ddic_tablename) " CCDB_DATA_112
        WHERE cd_store_id = @ls_storetab-store_id
          and CD_HIST_DATE_TO is initial             " ignore old change documents
          AND (   note = '0003089413'
               OR note = '0003287611'
               OR note = '0003304520' )
        ORDER BY PRIMARY KEY
        INTO TABLE @<fs_table>.

      LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<entry>).

        " Table CCDB_DATA_112
        ASSIGN COMPONENT 'NOTE'      OF STRUCTURE <entry> TO FIELD-SYMBOL(<note>).
        ASSIGN COMPONENT 'VERSION'   OF STRUCTURE <entry> TO FIELD-SYMBOL(<version>).
        ASSIGN COMPONENT 'TEXT'      OF STRUCTURE <entry> TO FIELD-SYMBOL(<text>).
        ASSIGN COMPONENT 'PRSTATUS'  OF STRUCTURE <entry> TO FIELD-SYMBOL(<prstatus>).
        ASSIGN COMPONENT 'PRSTATUST' OF STRUCTURE <entry> TO FIELD-SYMBOL(<prstatust>).

        " Copy data
        DATA(status) = <prstatust> && ` version ` && <version>.
        CASE <note>.
          WHEN '0003089413'.
            ls_result-note_3089413 = status.
            ls_result-note_3089413_prstatus = <prstatus>.
          WHEN '0003287611'.
            ls_result-note_3287611 = status.
            ls_result-note_3287611_prstatus = <prstatus>.
          WHEN '0003304520'.
            ls_result-note_3304520 = status.
            ls_result-note_3304520_prstatus = <prstatus>.
        ENDCASE.

      ENDLOOP. " <fs_table>

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_storetab

  ENDMETHOD. " get_ABAP_NOTES

  METHOD get_rfcsysacl.
    CHECK p_trust = 'X'.

    DATA tabix TYPE i.

    " Get stores
    SELECT *
      FROM ccdb_rt_dirv_dbv                " CCDB: Root object - Directory
      INTO TABLE @DATA(lt_storetab)
     WHERE rt_type          = 'TS'         " Root Type
       AND extsid           IN @p_extsid   " System name
       AND system_type      = 'ABAP'       " System type
       AND sci_id           = 'S00048'     " Store id
       AND store_name       = 'RFCSYSACL'  " Store name
     ORDER BY extsid .
    CHECK sy-subrc IS INITIAL.

    " Use table name from first store
    READ TABLE lt_storetab INTO DATA(ls_storetab) INDEX 1.
    CHECK sy-subrc IS INITIAL.

    " Create data table
    DATA(ddic_tablename) = ls_storetab-ddic_tablename.
    DATA ref_tab TYPE REF TO data.
    CREATE DATA ref_tab TYPE STANDARD TABLE OF (ddic_tablename).
    FIELD-SYMBOLS <fs_table> TYPE ANY TABLE.
    ASSIGN ref_tab->* TO <fs_table>.

    LOOP AT lt_storetab INTO ls_storetab
      WHERE ddic_tablename = ddic_tablename.

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          extsid = ls_storetab-extsid.
      IF sy-subrc = 0.
        tabix = sy-tabix.
        "MOVE-CORRESPONDING ls_storetab TO ls_result.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_storetab TO ls_result. " EXTSID, STORE_NAME, STORE_ID, STORE_TO_DATE
        READ TABLE lt_extsid_data INTO DATA(ls_extsid_data)
          WITH TABLE KEY
            extsid = ls_storetab-extsid.
        IF sy-subrc = 0.
          ls_result-sid            = ls_extsid_data-sid.
          ls_result-install_number = ls_extsid_data-install_number.
        ENDIF.
      ENDIF.

      " Get data
      SELECT *
        FROM (ddic_tablename) " CCDB_DATA_127
        WHERE cd_store_id = @ls_storetab-store_id
          and CD_HIST_DATE_TO is initial             " ignore old change documents
        ORDER BY PRIMARY KEY
        INTO TABLE @<fs_table>.

      DATA ls_trusted_system  TYPE ts_trusted_system.
      CLEAR ls_trusted_system.

      LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<entry>).

        " Table CCDB_DATA_127
        ASSIGN COMPONENT 'RFCSYSID'    OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfcsysid>).
        ASSIGN COMPONENT 'TLICENSE_NR' OF STRUCTURE <entry> TO FIELD-SYMBOL(<tlicense_nr>).
        ASSIGN COMPONENT 'RFCTRUSTSY'  OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfctrustsy>).
        ASSIGN COMPONENT 'RFCDEST'     OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfcdest>).
        ASSIGN COMPONENT 'RFCTCDCHK'   OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfctcdchk>).
        ASSIGN COMPONENT 'RFCSNC'      OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfcsnc>).
        ASSIGN COMPONENT 'RFCSLOPT'    OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfcslopt>).
        ASSIGN COMPONENT 'RFCCREDEST'  OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfccredest>).
        ASSIGN COMPONENT 'RFCREGDEST'  OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfcregdest>).
        ASSIGN COMPONENT 'LLICENSE_NR' OF STRUCTURE <entry> TO FIELD-SYMBOL(<llicense_nr>).
        ASSIGN COMPONENT 'RFCSECKEY'   OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfcseckey>).

        " Copy data

        " Store RRFCSYSACL data
        if ls_trusted_system-rfctrustsy is initial.
          ls_trusted_system-rfctrustsy  = <rfctrustsy>.
          ls_trusted_system-llicense_nr = <llicense_nr>.
          " Add installation number
          IF ls_trusted_system-llicense_nr IS INITIAL.
            ls_trusted_system-llicense_nr = ls_result-install_number.
          ENDIF.
        endif.

        " Store RRFCSYSACL data
        DATA ls_rfcsysacl_data TYPE ts_rfcsysacl_data.
        CLEAR ls_rfcsysacl_data.
        ls_rfcsysacl_data-rfcsysid    = <rfcsysid>.
        ls_rfcsysacl_data-tlicense_nr = <tlicense_nr>.
        ls_rfcsysacl_data-rfctrustsy  = <rfctrustsy>.
        ls_rfcsysacl_data-rfcdest     = <rfcdest>.
        ls_rfcsysacl_data-rfctcdchk   = <rfctcdchk>.
        ls_rfcsysacl_data-rfcsnc      = <rfcsnc>.
        ls_rfcsysacl_data-rfcslopt    = <rfcslopt>.
        " only available in higher versions
        ls_rfcsysacl_data-rfccredest  = <rfccredest>.
        ls_rfcsysacl_data-rfcregdest  = <rfcregdest>.
        ls_rfcsysacl_data-llicense_nr = <llicense_nr>.
        ls_rfcsysacl_data-rfcseckey   = <rfcseckey>.

        " Add installation number
        IF ls_rfcsysacl_data-llicense_nr IS INITIAL.
          ls_rfcsysacl_data-llicense_nr = ls_result-install_number.
        ENDIF.

        " Store RRFCSYSACL data
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

      ENDLOOP. " <fs_table>

      " Store trusted systems
      APPEND ls_trusted_system TO lt_trusted_systems.

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_storetab

  ENDMETHOD. " get_RFCSYSACL

  METHOD get_rfcdes.
    CHECK p_dest = 'X'.

    DATA tabix TYPE i.

    " Get stores
    SELECT *
      FROM ccdb_rt_dirv_dbv                " CCDB: Root object - Directory
      INTO TABLE @DATA(lt_storetab)
     WHERE extsid           IN @p_extsid   " System name
       AND system_type      = 'ABAP'       " System type
       AND sci_id           = 'S00035'     " Store id
       AND store_name       = 'RFCDES'     " Store name
     ORDER BY extsid .
    CHECK sy-subrc IS INITIAL.

    " Use table name from first store
    READ TABLE lt_storetab INTO DATA(ls_storetab) INDEX 1.
    CHECK sy-subrc IS INITIAL.

    " Create data table
    DATA(ddic_tablename) = ls_storetab-ddic_tablename.
    DATA ref_tab TYPE REF TO data.
    CREATE DATA ref_tab TYPE STANDARD TABLE OF (ddic_tablename).
    FIELD-SYMBOLS <fs_table> TYPE ANY TABLE.
    ASSIGN ref_tab->* TO <fs_table>.

    LOOP AT lt_storetab INTO ls_storetab
      WHERE ddic_tablename = ddic_tablename.

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          extsid = ls_storetab-extsid.
      IF sy-subrc = 0.
        tabix = sy-tabix.
        "MOVE-CORRESPONDING ls_storetab TO ls_result.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_storetab TO ls_result. " EXTSID, STORE_NAME, STORE_ID, STORE_TO_DATE
        READ TABLE lt_extsid_data INTO DATA(ls_extsid_data)
          WITH TABLE KEY
            extsid = ls_storetab-extsid.
        IF sy-subrc = 0.
          ls_result-sid            = ls_extsid_data-sid.
          ls_result-install_number = ls_extsid_data-install_number.
        ENDIF.
      ENDIF.

      " Get data
      SELECT *
        FROM (ddic_tablename) " CCDB_DATA_115
        WHERE cd_store_id = @ls_storetab-store_id
          and CD_HIST_DATE_TO is initial             " ignore old change documents
          AND (   rfctype = '3'
               OR rfctype = 'H'
               OR rfctype = 'W' )
        ORDER BY PRIMARY KEY
        INTO TABLE @<fs_table>.

      DATA ls_destination  TYPE ts_destination.
      CLEAR ls_destination.

      LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<entry>).

        " Table CCDB_DATA_115
        ASSIGN COMPONENT 'RFCDEST'    OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfcdest>).
        ASSIGN COMPONENT 'RFCTYPE'    OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfctype>).
        ASSIGN COMPONENT 'RFCOPTIONS' OF STRUCTURE <entry> TO FIELD-SYMBOL(<rfcoptions>).

        " Copy data

        " Store destination data
        if ls_destination-sid is initial.
          ls_destination-sid            = ls_result-sid.
          ls_destination-install_number = ls_result-install_number.
        endif.

        " Store destination data
        DATA ls_destination_data TYPE ts_destination_data.
        CLEAR ls_destination_data.
        ls_destination_data-rfcdest = <rfcdest>.
        ls_destination_data-rfctype = <rfctype>.

        " Interpret tokens similar to function RFCDES2RFCDISPLAY (multiple FIND REGEX are slower that SPLIT+LOOP+CASE)
*        FIND REGEX 'Q=(Y),' IN <RFCOPTIONS>              " Trusted
*          SUBMATCHES ls_destination_data-trusted.
*        IF ls_destination_data-trusted = 'Y'.
*          ls_destination_data-trusted = 'X'. " show checkbox instead of value
*        ENDIF.
*        FIND REGEX 'Q=([^,]*),'       IN <RFCOPTIONS>    " Logon Procedure (Y = Trusted)
*          SUBMATCHES ls_destination_data-rfcslogin.
*
*        FIND REGEX '\[=([^,]{3}),'    IN <RFCOPTIONS>    " System ID for trusted connection
*          SUBMATCHES ls_destination_data-serversysid.
*
*        FIND REGEX '\^=([^,]{1,10}),' IN <RFCOPTIONS>    " Installation number for trusted connection
*          SUBMATCHES ls_destination_data-serverinstnr.
*
*
*        FIND REGEX 'H=([^,]*),'       IN <RFCOPTIONS>    " Name of Target Host
*          SUBMATCHES ls_destination_data-rfchost.
*
*        FIND REGEX 'S=([^,]*),'       IN <RFCOPTIONS>    " Service used (TCP service, SAP System number)
*          SUBMATCHES ls_destination_data-rfcservice.
*
*
*        FIND REGEX 'I=([^,]*),'       IN <RFCOPTIONS>    " System ID
*          SUBMATCHES ls_destination_data-rfcsysid.
*
*        FIND REGEX 'M=([^,]*),'       IN <RFCOPTIONS>    " Explicit logon client
*          SUBMATCHES ls_destination_data-rfcclient.
*
*        FIND REGEX 'U=([^,]*),'       IN <RFCOPTIONS>    " Explicit user
*          SUBMATCHES ls_destination_data-rfcuser.
*        CONSTANTS logon_screen_token(8) VALUE '%_LOG01%'.
*        IF ls_destination_data-rfcuser = logon_screen_token.
*          ls_destination_data-rfcuser = 'logon screen'.
*        ENDIF.
*
*        FIND REGEX 'u=(Y),'           IN <RFCOPTIONS>    " Same user flag
*          SUBMATCHES ls_destination_data-rfcsameusr.
*        IF ls_destination_data-rfcsameusr = 'Y'.
*          ls_destination_data-rfcsameusr = 'X'. " show checkbox instead of value
*
*          ls_destination_data-rfcuser = 'same user'.
*        ENDIF.
*
*        FIND REGEX 'v=([^,]*),'       IN <RFCOPTIONS>    " Explicit password
*          SUBMATCHES ls_destination_data-rfcauth.
*        FIND REGEX 'V=([^,]*),'       IN <RFCOPTIONS>    " Explicit password
*          SUBMATCHES ls_destination_data-rfcauth.
*        FIND REGEX 'P=([^,]*),'       IN <RFCOPTIONS>    " Explicit password
*          SUBMATCHES ls_destination_data-rfcauth.
*        CONSTANTS sec_storage_token(5) VALUE '%_PWD'.
*        IF ls_destination_data-rfcauth = sec_storage_token.
*          ls_destination_data-rfcauth = 'stored password'.
*        ENDIF.
*
*        FIND REGEX 's=(Y),'           IN <RFCOPTIONS>    " SNC/TLS
*          SUBMATCHES ls_destination_data-rfcsnc.
*        IF ls_destination_data-rfcsnc = 'Y'.
*          ls_destination_data-rfcsnc = 'X'. " show checkbox instead of value
*        ENDIF.
*        FIND REGEX 't=([^,]*),'       IN <RFCOPTIONS>    " SSL Client Identity
*          SUBMATCHES ls_destination_data-sslapplic.
*        FIND REGEX '/=([^,]*),'       IN <RFCOPTIONS>    " No client cert
*          SUBMATCHES ls_destination_data-noccert.
        SPLIT <rfcoptions> AT ',' INTO TABLE DATA(tokens).
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
        ENDLOOP. " tokens

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

              IF ls_destination_data-rfcsnc IS NOT INITIAL
                and ls_destination_data-rfcsnc ne 'N'.
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

              IF ls_destination_data-rfcsnc IS NOT INITIAL
                and ls_destination_data-rfcsnc ne 'N'.
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

              IF ls_destination_data-rfcsnc IS NOT INITIAL
                and ls_destination_data-rfcsnc ne 'N'.
                ADD 1 TO ls_result-dest_w_cnt_trusted_tls.
              ENDIF.
            ENDIF.

        ENDCASE.

      ENDLOOP. " <fs_table>

      " Store destination data
      APPEND ls_destination TO lt_destinations.

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_storetab

  ENDMETHOD. " get_RFCDES

  METHOD get_abap_instance_pahi.
    CHECK p_trust = 'X' OR p_dest = 'X'.

    DATA tabix TYPE i.

    " Get stores
    SELECT *
      FROM ccdb_rt_dirv_dbv                " CCDB: Root object - Directory
      INTO TABLE @DATA(lt_storetab)
     WHERE extsid           IN @p_extsid   " System name
       AND system_type      = 'ABAP'       " System type
       AND sci_id           = 'S00007'     " Store id
       AND store_name       = 'ABAP_INSTANCE_PAHI'     " Store name
     ORDER BY extsid .
    CHECK sy-subrc IS INITIAL.

    " Use table name from first store
    READ TABLE lt_storetab INTO DATA(ls_storetab) INDEX 1.
    CHECK sy-subrc IS INITIAL.

    " Create data table
    DATA(ddic_tablename) = ls_storetab-ddic_tablename.
    DATA ref_tab TYPE REF TO data.
    CREATE DATA ref_tab TYPE STANDARD TABLE OF (ddic_tablename).
    FIELD-SYMBOLS <fs_table> TYPE ANY TABLE.
    ASSIGN ref_tab->* TO <fs_table>.

    LOOP AT lt_storetab INTO ls_storetab
      WHERE ddic_tablename = ddic_tablename.

      " Do we already have an entry for this system?
      READ TABLE lt_result INTO DATA(ls_result)
        WITH KEY
          extsid = ls_storetab-extsid.
      IF sy-subrc = 0.
        tabix = sy-tabix.
        "MOVE-CORRESPONDING ls_storetab TO ls_result.
      ELSE.
        tabix = -1.
        CLEAR ls_result.
        MOVE-CORRESPONDING ls_storetab TO ls_result. " EXTSID, STORE_NAME, STORE_ID, STORE_TO_DATE
        READ TABLE lt_extsid_data INTO DATA(ls_extsid_data)
          WITH TABLE KEY
            extsid = ls_storetab-extsid.
        IF sy-subrc = 0.
          ls_result-sid            = ls_extsid_data-sid.
          ls_result-install_number = ls_extsid_data-install_number.
        ENDIF.
      ENDIF.

      " Get data
      SELECT *
        FROM (ddic_tablename) " CCDB_DATA_001
        WHERE cd_store_id = @ls_storetab-store_id
          and CD_HIST_DATE_TO is initial             " ignore old change documents
          AND (   name = 'rfc/selftrust'
               OR name = 'rfc/allowoldticket4tt'
               OR name = 'rfc/sendInstNr4tt' )
        ORDER BY PRIMARY KEY
        INTO TABLE @<fs_table>.

      LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<entry>).

        " Table CCDB_DATA_001
        ASSIGN COMPONENT 'NAME'    OF STRUCTURE <entry> TO FIELD-SYMBOL(<name>).
        ASSIGN COMPONENT 'VALUE'   OF STRUCTURE <entry> TO FIELD-SYMBOL(<value>).

        " Copy data
        CASE <name>.
          WHEN 'rfc/selftrust'.         ls_result-rfc_selftrust         = <value>.
          WHEN 'rfc/allowoldticket4tt'. ls_result-rfc_allowoldticket4tt = <value>.
          WHEN 'rfc/sendInstNr4tt'.     ls_result-rfc_sendinstnr4tt     = <value>.
        ENDCASE.

      ENDLOOP. " <fs_table>

      IF tabix > 0.
        MODIFY lt_result FROM ls_result INDEX tabix.
      ELSE.
        APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP. " lt_storetab

  ENDMETHOD. " get_ABAP_INSTANCE_PAHI

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
        IF <fs_result>-kern_rel(3) CO '0123456789'.
          rel   = <fs_result>-kern_rel(3).
        ELSE.
          rel = 0.
        ENDIF.
        IF <fs_result>-kern_patchlevel CO '0123456789'.
          patch = <fs_result>-kern_patchlevel.
        ELSE.
          patch = 0.
        ENDIF.

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
          AND <fs_rfcsysacl_data>-rfcsysid IN p_EXTSID.    " But check this only of data should be available

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

    " Check migrated trusted destinations if there exist a trust relation in the target system for the clling system
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
              extsid      = ls_result-extsid
              rfctrustsy  = ls_result-sid
              llicense_nr = ls_result-install_number
            ).

            " Show destinations
          ELSEIF ls_cell-columnname(11) = 'DEST_3_CNT_'
            OR ls_cell-columnname(11) = 'DEST_H_CNT_'
            OR ls_cell-columnname(11) = 'DEST_W_CNT_'.

            show_destinations(
              column         = ls_cell-columnname
              extsid         = ls_result-extsid
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
          extsid      = ls_result-extsid
          rfctrustsy  = ls_result-sid
          llicense_nr = ls_result-install_number
        ).

        " Show destinations
      ELSEIF column(11) = 'DEST_3_CNT_'
        OR column(11) = 'DEST_H_CNT_'
        OR column(11) = 'DEST_W_CNT_'.

        show_destinations(
          column         = column
          extsid         = ls_result-extsid
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
        lr_sorts->add_sort( 'EXTSID' ).
        "lr_sorts->add_sort( 'INSTALL_NUMBER' ).
        "lr_sorts->add_sort( 'SID' ).

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
        lr_column ?= lr_columns->get_column( 'STORE_TO_DATE' ).
        lr_column->set_edit_mask( '==TSTMP' ).

*... adjust headings
        DATA color TYPE lvc_s_colo.

        " Key

        lr_column ?= lr_columns->get_column( 'EXTSID' ).
        lr_column->set_long_text( 'Extended system id' ).
        lr_column->set_medium_text( 'Extended system id' ).
        lr_column->set_short_text( 'Ext.Sys.Id' ).

        lr_column ?= lr_columns->get_column( 'SID' ).
        lr_column->set_long_text( 'System id' ).
        lr_column->set_medium_text( 'System id' ).
        lr_column->set_short_text( 'Sys.Id' ).

        lr_column ?= lr_columns->get_column( 'INSTALL_NUMBER' ).
        lr_column->set_long_text( 'Installation number' ).
        lr_column->set_medium_text( 'Installation number' ).
        lr_column->set_short_text( 'Inst.Nr.' ).

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

        lr_column ?= lr_columns->get_column( 'STORE_NAME' ).
        lr_column->set_long_text( 'ABAP release' ).   "max. 40 characters
        lr_column->set_medium_text( 'ABAP release' ). "max. 20 characters
        lr_column->set_short_text( 'ABAP rel.' ).     "max. 10 characters

*... hide unimportant columns

        lr_column ?= lr_columns->get_column( 'KERN_COMP_TIME' ).          lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'KERN_COMP_DATE' ).          lr_column->set_visible( abap_true ).

        lr_column ?= lr_columns->get_column( 'NOTE_3089413_PRSTATUS' ).   lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'NOTE_3287611_PRSTATUS' ).   lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'NOTE_3304520_PRSTATUS' ).   lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'STORE_NAME' ).              lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'STORE_ID' ).                lr_column->set_visible( abap_false ).
        lr_column ?= lr_columns->get_column( 'STORE_TO_DATE' ).       lr_column->set_visible( abap_false ).

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
    "  extsid         TYPE ts_result-extsid
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
        lr_display->set_list_header( `All trusted systems of system ` && extsid ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'NO_DATA_CNT'.   " No data for trusted system found
        lr_display->set_list_header( `No data for trusted system found of system ` && extsid ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE no_data IS NOT INITIAL.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'MUTUAL_TRUST_CNT'.   " Mutual trust relations
        lr_display->set_list_header( `Mutual trust relations with system ` && extsid ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE mutual_trust IS NOT INITIAL.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'TRUSTSY_CNT_TCD'. " Tcode active for trusted systems
        lr_display->set_list_header( `Tcode active for trusted systems of system ` && extsid ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE rfctcdchk = 'X'.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'TRUSTSY_CNT_3'.   " Migrated trusted systems
        lr_display->set_list_header( `Migrated trusted systems of system ` && extsid ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE rfcslopt(1) = '3'.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'TRUSTSY_CNT_2'.   " Old trusted systems
        lr_display->set_list_header( `Old trusted systems of system ` && extsid ).

        LOOP AT <fs_trusted_system>-rfcsysacl_data INTO ls_rfcsysacl_data
          WHERE rfcslopt(1) = '2'.
          APPEND ls_rfcsysacl_data TO lt_rfcsysacl_data.
        ENDLOOP.

      WHEN 'TRUSTSY_CNT_1'.   " Very old trusted systems
        lr_display->set_list_header( `Very old trusted systems of system ` && extsid ).

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
    "  column         TYPE salv_de_column
    "  extsid         TYPE ts_result-extsid
    "  sid            TYPE ts_result-sid
    "  install_number TYPE ts_result-install_number

    " Show trusted systems
    CHECK column(11) = 'DEST_3_CNT_'
       OR column(11) = 'DEST_H_CNT_'
       OR column(11) = 'DEST_W_CNT_'.

    DATA ls_destination  TYPE ts_destination.
    READ TABLE lt_destinations ASSIGNING FIELD-SYMBOL(<fs_destination>)
      WITH TABLE KEY
        sid            = SID
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
        lr_display->set_list_header( `All RFC destinations (type 3) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED'.
        lr_display->set_list_header( `Trusted RFC destinations (type 3) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED_MIGRATED'.
        lr_display->set_list_header( `Migrated RFC destinations (type 3) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED_NO_INSTNR'.
        lr_display->set_list_header( `Missing installation number in RFC destinations (type 3) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED_NO_SYSID'.
        lr_display->set_list_header( `Missing system id in RFC destinations (type 3) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_3_CNT_TRUSTED_SNC'.
        lr_display->set_list_header( `Encrypted trusted RFC destinations (type 3) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = '3'
            AND trusted IS NOT INITIAL
            AND rfcsnc IS NOT INITIAL
            AND rfcsnc NE 'N'.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.


      WHEN 'DEST_H_CNT_ALL'.
        lr_display->set_list_header( `All http destinations (type H) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED'.
        lr_display->set_list_header( `Trusted http destinations (type H) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED_MIGRATED'.
        lr_display->set_list_header( `Migrated http destinations (type H) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED_NO_INSTNR'.
        lr_display->set_list_header( `Missing installation number in http destinations (type H) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED_NO_SYSID'.
        lr_display->set_list_header( `Missing system id in http destinations (type H) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_H_CNT_TRUSTED_TLS'.
        lr_display->set_list_header( `Encrypted trusted http destinations (type H) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'H'
            AND trusted IS NOT INITIAL
            AND rfcsnc IS NOT INITIAL
            AND rfcsnc NE 'N'.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.


      WHEN 'DEST_W_CNT_ALL'.
        lr_display->set_list_header( `All WebRFC destinations (type W) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED'.
        lr_display->set_list_header( `Trusted WebRFC destinations (type W) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED_MIGRATED'.
        lr_display->set_list_header( `Migrated WebRFC destinations (type W) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS NOT INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED_NO_INSTNR'.
        lr_display->set_list_header( `Missing installation number in WebRFC destinations (type W) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS NOT INITIAL
            AND serverinstnr IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED_NO_SYSID'.
        lr_display->set_list_header( `Missing system id in WebRFC destinations (type W) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted      IS NOT INITIAL
            AND serversysid  IS INITIAL.
          APPEND ls_destination_data TO lt_destination_data.
        ENDLOOP.

      WHEN 'DEST_W_CNT_TRUSTED_TLS'.
        lr_display->set_list_header( `Encrypted trusted WebRFC destinations (type W) of system ` && extsid ).

        LOOP AT <fs_destination>-destination_data INTO ls_destination_data
          WHERE rfctype = 'W'
            AND trusted IS NOT INITIAL
            AND rfcsnc IS NOT INITIAL
            AND rfcsnc NE 'N'.
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

ENDCLASS.                    "lcl_report IMPLEMENTATION

*----------------------------------------------------------------------*
*      REPORT events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_EXTSID-low.
  lcl_report=>f4_EXTSID( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_EXTSID-high.
  lcl_report=>f4_EXTSID( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  lcl_report=>f4_p_layout( CHANGING layout = p_layout ).

AT SELECTION-SCREEN ON p_layout.
  CHECK p_layout IS NOT INITIAL.
  lcl_report=>at_selscr_on_p_layout( p_layout ).

START-OF-SELECTION.
  lcl_report=>start_of_selection( ).
