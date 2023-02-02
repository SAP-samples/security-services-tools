*&---------------------------------------------------------------------*
*& Report  ZCHECK_NOTE_3089413
*& Check implementation status of note 3089413 for connected ABAP systems
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Source: https://github.com/SAP-samples/security-services-tools
*&
*& 02.02.2023 Check destinations, too
*& 02.02.2023 Initial version
*&---------------------------------------------------------------------*
REPORT ZCHECK_NOTE_3089413.

CONSTANTS: c_program_version(30) TYPE c VALUE '02.02.2023 FBT'.

type-pools: ICON, COL, SYM.

DATA sel_store_dir TYPE sdiagst_store_dir.

* System name
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) ss_sid FOR FIELD p_sid.
SELECT-OPTIONS p_sid   FOR sel_store_dir-long_sid.
SELECTION-SCREEN END OF LINE.

* Check Kernel
selection-screen begin of line.
parameters       P_KERN as checkbox default 'X'.
selection-screen comment 3(33) PS_KERN for field P_KERN.
selection-screen end of line.

* Check ABAP
selection-screen begin of line.
parameters       P_ABAP as checkbox default 'X'.
selection-screen comment 3(33) PS_ABAP for field P_ABAP.
selection-screen end of line.

* Check trusted relations
selection-screen begin of line.
parameters       P_TRUST as checkbox default 'X'.
selection-screen comment 3(33) PS_TRUST for field P_TRUST.
selection-screen end of line.

* Check trusted destinations
selection-screen begin of line.
parameters       P_DEST as checkbox default 'X'.
selection-screen comment 3(33) PS_DEST for field P_DEST.
selection-screen end of line.
* Show specific type only if data found
data P_DEST_3 type abap_bool.
data P_DEST_H type abap_bool.
data P_DEST_W type abap_bool.

* Store status
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) ss_state FOR FIELD p_state.
SELECT-OPTIONS p_state FOR sel_store_dir-store_main_state_type." DEFAULT 'G'.
SELECTION-SCREEN END OF LINE.

* Layout of ALV output
selection-screen begin of line.
selection-screen comment 1(33) PS_LOUT for field P_LAYOUT.
parameters       P_LAYOUT type DISVARIANT-VARIANT.
selection-screen end of line.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

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

    validate_kernel       TYPE string,

    " Source store: ABAP_COMP_SPLEVEL
    ABAP_RELEASE          TYPE string,                               " 754
    ABAP_SP               TYPE string,                               " 0032

    validate_ABAP         TYPE string,

    " Source store: ABAP_NOTES
    NOTE_3089413          TYPE string,
    NOTE_3089413_PRSTATUS TYPE CWBPRSTAT,
    NOTE_3287611          TYPE string,
    NOTE_3287611_PRSTATUS TYPE CWBPRSTAT,

    " Source store: RFCSYSACL
    TRUSTSY_cnt_all       TYPE i,
    TRUSTSY_CNT_TCD       TYPE i,
    TRUSTSY_cnt_3         TYPE i,
    TRUSTSY_cnt_2         TYPE i,
    TRUSTSY_cnt_1         TYPE i,

    " Source store: ABAP_INSTANMCE_PAHI
    rfc_allowoldticket4tt TYPE string,
    RFC_SELFTRUST         TYPE string,
    rfc_sendInstNr4tt     TYPE string,

    " Source store: RFCDES
    DEST_3_cnt_all               TYPE i,
    DEST_3_cnt_trusted           TYPE i,
    DEST_3_cnt_trusted_migrated  TYPE i,
    DEST_3_cnt_trusted_no_instnr TYPE i,
    DEST_3_cnt_trusted_no_sysid  TYPE i,
    DEST_3_cnt_trusted_snc       TYPE i,

    DEST_H_cnt_all               TYPE i,
    DEST_H_cnt_trusted           TYPE i,
    DEST_H_cnt_trusted_migrated  TYPE i,
    DEST_H_cnt_trusted_no_instnr TYPE i,
    DEST_H_cnt_trusted_no_sysid  TYPE i,
    DEST_H_cnt_trusted_tls       TYPE i,

    DEST_W_cnt_all               TYPE i,
    DEST_W_cnt_trusted           TYPE i,
    DEST_W_cnt_trusted_migrated  TYPE i,
    DEST_W_cnt_trusted_no_instnr TYPE i,
    DEST_W_cnt_trusted_no_sysid  TYPE i,
    DEST_W_cnt_trusted_tls       TYPE i,

    " Source store: we show the status of the first found store only which is usually store SAP_KERNEL
    store_id              TYPE sdiagst_store_dir-store_id,
    store_last_upload     TYPE sdiagst_store_dir-store_last_upload,
    store_state           TYPE sdiagst_store_dir-store_state,           " CMPL = ok
    store_main_state_type TYPE sdiagst_store_dir-store_main_state_type, " (G)reen, (Y)ello, (R)ed, (N)ot relevant
    store_main_state      TYPE sdiagst_store_dir-store_main_state,
    store_outdated_day    TYPE sdiagst_store_dir-store_outdated_day,

    t_color               type lvc_t_scol,
*   t_celltype             type salv_t_int4_column,
*   T_HYPERLINK            type SALV_T_INT4_COLUMN,
*   t_dropdown             type salv_t_int4_column,
  END OF ts_outtab,
  tt_outtab TYPE TABLE OF ts_outtab.

DATA:
  lt_outtab TYPE tt_outtab,
  ls_outtab TYPE ts_outtab.

data: GS_ALV_LOUT_VARIANT type DISVARIANT.

*----------------------------------------------------------------------

INITIALIZATION.
  sy-title = 'Check implementation status of note 3089413 for connected ABAP systems'(TIT).

  ss_sid   = 'System (long sid)'.
  ss_state = 'Config. store status (G/Y/R)'.

  PS_KERN  = 'Check Kernel'.
  PS_ABAP  = 'Check Support Package and Notes'.
  PS_TRUST = 'Check Trusted Relations'.
  PS_DEST  = 'Check Trusted Destinations'.

  PS_LOUT     = 'Layout'(t18).

  CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
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

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON p_layout
*----------------------------------------------------------------------*

at selection-screen on P_LAYOUT.
  check not P_LAYOUT is initial.
  perform HANDLE_AT_SELSCR_ON_P_LAYOUT using P_LAYOUT SY-REPID 'A'.
*
form HANDLE_AT_SELSCR_ON_P_LAYOUT
   using ID_VARNAME type DISVARIANT-VARIANT
         ID_REPID   type SY-REPID
         ID_SAVE    type C.

  data: LS_VARIANT type DISVARIANT.

  LS_VARIANT-REPORT  = ID_REPID.
  LS_VARIANT-VARIANT = ID_VARNAME.

  call function 'REUSE_ALV_VARIANT_EXISTENCE'
    exporting
      I_SAVE        = ID_SAVE
    changing
      CS_VARIANT    = LS_VARIANT
    exceptions
      WRONG_INPUT   = 1
      NOT_FOUND     = 2
      PROGRAM_ERROR = 3
      others        = 4.

  if SY-SUBRC <> 0.
*   Selected layout variant is not found
    message E204(0K).
  endif.

  GS_ALV_LOUT_VARIANT-REPORT  = ID_REPID.
  GS_ALV_LOUT_VARIANT-VARIANT = ID_VARNAME.

endform.                    " handle_at_selscr_on_p_layout

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout
*----------------------------------------------------------------------*
at selection-screen on value-request for P_LAYOUT.  " ( Note 890141 )
  perform HANDLE_AT_SELSCR_F4_P_LAYOUT using    SY-REPID 'A'
                                       changing P_LAYOUT.
*
form HANDLE_AT_SELSCR_F4_P_LAYOUT
  using    ID_REPID   type SY-REPID
           ID_SAVE    type C
  changing ED_VARNAME type DISVARIANT-VARIANT.

  GS_ALV_LOUT_VARIANT-REPORT = ID_REPID.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      IS_VARIANT    = GS_ALV_LOUT_VARIANT
      I_SAVE        = ID_SAVE
    importing
      ES_VARIANT    = GS_ALV_LOUT_VARIANT
    exceptions
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      others        = 3.

  if SY-SUBRC = 0.
    ED_VARNAME = GS_ALV_LOUT_VARIANT-VARIANT.
  else.
    message S073(0K).
*   Keine Anzeigevariante(n) vorhanden
  endif.

endform.                               " handle_at_selscr_f4_p_layout

*----------------------------------------------------------------------

START-OF-SELECTION.

  PERFORM get_SAP_KERNEL.         " Kernel version
  PERFORM get_ABAP_COMP_SPLEVEL.  " Support Package version of SAP_BASIS
  PERFORM get_ABAP_NOTES.         " Notes 3089413 and 3287611
  PERFORM get_RFCSYSACL.          " Trusting relations
  PERFORM get_RFCDES.             " Trusted desinations
  PERFORM get_ABAP_INSTANCE_PAHI. " rfc/selftrust

  PERFORM validate_kernel.
  PERFORM validate_ABAP.

  PERFORM show_result.


FORM get_SAP_KERNEL.
  check P_KERN = 'X'.

  " Same as in report ZSHOW_KERNEL_STORES but one one entry per system

  DATA:
    lt_store_dir_tech    TYPE  tt_diagst_store_dir_tech,
    lt_store_dir         TYPE  tt_diagst_store_dir,
    lt_fieldlist         TYPE  tt_diagst_table_store_fields,
    lt_snapshot          TYPE  tt_diagst_trows,
    rc                   TYPE  i,
    rc_text              TYPE  natxt.

  data: tabix type i.

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

    " Do we already have an entry for this system?
    READ table lt_outtab into ls_outtab
      with key
        install_number = ls_store_dir-install_number
        long_sid       = ls_store_dir-long_sid
        sid            = ls_store_dir-sid
        .
    if sy-subrc = 0.
      tabix = sy-tabix.
      if ls_store_dir-instance_type ne 'CENTRAL'.
        continue.
      endif.
      MOVE-CORRESPONDING ls_store_dir TO ls_outtab.
    else.
      tabix = -1.
      CLEAR ls_outtab.
      MOVE-CORRESPONDING ls_store_dir TO ls_outtab.
    endif.

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
      READ TABLE lt_snapshot_elem INTO data(ls_PARAMETER) INDEX 1.
      check ls_PARAMETER-fieldname = 'PARAMETER'.

      READ TABLE lt_snapshot_elem INTO data(ls_VALUE)     INDEX 2.
      check ls_VALUE-fieldname = 'VALUE'.

      CASE ls_PARAMETER-fieldvalue.

        WHEN 'KERN_COMP_ON'.      " Linux GNU SLES-11 x86_64 cc4.3.4 use-pr190909
          " not used yet

        WHEN 'KERN_COMP_TIME'.    " Jun  7 2020 15:44:10
          ls_outtab-kern_comp_time  = ls_VALUE-fieldvalue.
          PERFORM convert_comp_time USING ls_outtab-kern_comp_time CHANGING ls_outtab-kern_comp_date.

        WHEN 'KERN_DBLIB'.        " SQLDBC 7.9.8.040
          " not used yet

        WHEN 'KERN_PATCHLEVEL'.   " 1000
          ls_outtab-kern_patchlevel = ls_VALUE-fieldvalue.

        WHEN 'KERN_REL'.          " 722_EXT_REL
          ls_outtab-kern_rel        = ls_VALUE-fieldvalue.

        WHEN 'PLATFORM-ID'.       " 390
          " not used yet

      ENDCASE.
    ENDLOOP.

    if tabix > 0.
      MODIFY lt_outtab from ls_outtab INDEX tabix.
    else.
      APPEND ls_outtab TO lt_outtab.
    endif.

  ENDLOOP. " lt_STORE_DIR

ENDFORM. " get_SAP_KERNEL


FORM get_ABAP_COMP_SPLEVEL.
  check P_ABAP = 'X'.

  DATA:
    lt_store_dir_tech    TYPE  tt_diagst_store_dir_tech,
    lt_store_dir         TYPE  tt_diagst_store_dir,
    lt_fieldlist         TYPE  tt_diagst_table_store_fields,
    lt_snapshot          TYPE  tt_diagst_trows,
    rc                   TYPE  i,
    rc_text              TYPE  natxt.

  data: tabix type i.

  " Using a SEARCH_STRING for SAP_BASIS should not speed up processing, as this component exists always
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
      group_landscape_class = 'CL_DIAGLS_ABAP_TECH_SYST' "(optional)
*     GROUP_LANDSCAPE_ID    = ' '
*     GROUP_COMP_ID         = ' '
      group_source          = 'ABAP'                     "(optional)
      group_name            = 'ABAP-SOFTWARE'            "(optional)
      store_category        = 'SOFTWARE'                 "(optional)
      store_type            = 'TABLE'                    "(optional)
*     STORE_FULLPATH        = ' '
      store_name            = 'ABAP_COMP_SPLEVEL'

      " Special filters
      store_mainalias       = 'ABAP-SOFTWARE'            "(optional)
      store_subalias        = 'SUPPORT-PACKAGE-LEVEL'    "(optional)
*     STORE_TPL_ID          = ' '
*     HAS_ELEMENT_FROM      =                            " date range
*     HAS_ELEMENT_TO        =                            " date range
*     ELEMENT_FILTER        = 'C'                        " (C)hange, (I)nitial, (A)ll
*     CASE_INSENSITIVE      = ' '
      "PATTERN_SEARCH        = ' '                        " Allow pattern search for SEARCH_STRING
      "SEARCH_STRING         = 'SAP_BASIS'
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

    " Do we already have an entry for this system?
    READ table lt_outtab into ls_outtab
      with key
        install_number = ls_store_dir-install_number
        long_sid       = ls_store_dir-long_sid
        sid            = ls_store_dir-sid
        .
    if sy-subrc = 0.
      tabix = sy-tabix.
    else.
      tabix = -1.
      CLEAR ls_outtab.
      MOVE-CORRESPONDING ls_store_dir TO ls_outtab.
    endif.

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
      READ TABLE lt_snapshot_elem INTO data(ls_COMPONENT)  INDEX 1.
      check ls_COMPONENT-fieldname = 'COMPONENT'.
      check ls_COMPONENT-fieldvalue = 'SAP_BASIS'.

      READ TABLE lt_snapshot_elem INTO data(ls_RELEASE)    INDEX 2.
      check ls_RELEASE-fieldname = 'RELEASE'.
      ls_outtab-ABAP_RELEASE = ls_RELEASE-fieldvalue.

      READ TABLE lt_snapshot_elem INTO data(ls_EXTRELEASE) INDEX 3.
      check ls_EXTRELEASE-fieldname = 'EXTRELEASE'.
      ls_outtab-ABAP_SP      = ls_EXTRELEASE-fieldvalue.
    ENDLOOP.

    if tabix > 0.
      MODIFY lt_outtab from ls_outtab INDEX tabix.
    else.
      APPEND ls_outtab TO lt_outtab.
    endif.

  ENDLOOP. " lt_STORE_DIR

ENDFORM. " get_ABAP_COMP_SPLEVEL


FORM get_ABAP_NOTES.
  check P_ABAP = 'X'.

  DATA:
    lt_store_dir_tech    TYPE  tt_diagst_store_dir_tech,
    lt_store_dir         TYPE  tt_diagst_store_dir,
    lt_fieldlist         TYPE  tt_diagst_table_store_fields,
    lt_snapshot          TYPE  tt_diagst_trows,
    rc                   TYPE  i,
    rc_text              TYPE  natxt.

  data: tabix type i.

  " Maybe it' faster to call it twice including a SEARCH_STRING for both note numbers.
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
      group_landscape_class = 'CL_DIAGLS_ABAP_TECH_SYST' "(optional)
*     GROUP_LANDSCAPE_ID    = ' '
*     GROUP_COMP_ID         = ' '
      group_source          = 'ABAP'                     "(optional)
      group_name            = 'ABAP-SOFTWARE'            "(optional)
      store_category        = 'SOFTWARE'                 "(optional)
      store_type            = 'TABLE'                    "(optional)
*     STORE_FULLPATH        = ' '
      store_name            = 'ABAP_NOTES'

      " Special filters
      store_mainalias       = 'ABAP-SOFTWARE'            "(optional)
      store_subalias        = 'ABAP-NOTES'               "(optional)
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

    " Do we already have an entry for this system?
    READ table lt_outtab into ls_outtab
      with key
        install_number = ls_store_dir-install_number
        long_sid       = ls_store_dir-long_sid
        sid            = ls_store_dir-sid
        .
    if sy-subrc = 0.
      tabix = sy-tabix.
    else.
      tabix = -1.
      CLEAR ls_outtab.
      MOVE-CORRESPONDING ls_store_dir TO ls_outtab.
    endif.

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
      READ TABLE lt_snapshot_elem INTO data(ls_NOTE)      INDEX 1. "
      check ls_NOTE-fieldname = 'NOTE'.
      check ls_NOTE-fieldvalue = '0003089413'
         or ls_NOTE-fieldvalue = '0003287611'.

      READ TABLE lt_snapshot_elem INTO data(ls_VERSION)   INDEX 2. "
      check ls_VERSION-fieldname = 'VERSION'.

      "READ TABLE lt_snapshot_elem INTO data(ls_TEXT)      INDEX 3. "
      "check ls_TEXT-fieldname = 'TEXT'.

      READ TABLE lt_snapshot_elem INTO data(ls_PRSTATUST) INDEX 4. "
      check ls_PRSTATUST-fieldname = 'PRSTATUST'.

      READ TABLE lt_snapshot_elem INTO data(ls_PRSTATUS)  INDEX 5. "
      check ls_PRSTATUS-fieldname = 'PRSTATUS'.

      data(status) = ls_PRSTATUST-fieldvalue && ` version ` && ls_VERSION-fieldvalue.
      case ls_NOTE-fieldvalue.
        when '0003089413'.
          ls_outtab-NOTE_3089413 = status.
          ls_outtab-NOTE_3089413_PRSTATUS = ls_PRSTATUS-fieldvalue.
        when '0003287611'.
          ls_outtab-NOTE_3287611 = status.
          ls_outtab-NOTE_3287611_PRSTATUS = ls_PRSTATUS-fieldvalue.
      endcase.

    ENDLOOP.

    if tabix > 0.
      MODIFY lt_outtab from ls_outtab INDEX tabix.
    else.
      APPEND ls_outtab TO lt_outtab.
    endif.

  ENDLOOP. " lt_STORE_DIR

ENDFORM. " get_ABAP_NOTES


FORM get_RFCSYSACL.
  check P_TRUST = 'X'.

  DATA:
    lt_store_dir_tech    TYPE  tt_diagst_store_dir_tech,
    lt_store_dir         TYPE  tt_diagst_store_dir,
    lt_fieldlist         TYPE  tt_diagst_table_store_fields,
    lt_snapshot          TYPE  tt_diagst_trows,
    rc                   TYPE  i,
    rc_text              TYPE  natxt.

  data: tabix type i.

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
      group_landscape_class = 'CL_DIAGLS_ABAP_TECH_SYST' "(optional)
*     GROUP_LANDSCAPE_ID    = ' '
*     GROUP_COMP_ID         = ' '
      group_source          = 'ABAP'                     "(optional)
      group_name            = 'ABAP-SECURITY'            "(optional)
      store_category        = 'CONFIG'                   "(optional)
      store_type            = 'TABLE'                    "(optional)
*     STORE_FULLPATH        = ' '
      store_name            = 'RFCSYSACL'

      " Special filters
      store_mainalias       = 'SECURITY'                 "(optional)
      store_subalias        = 'TRUSTED RFC'              "(optional)
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

    " Do we already have an entry for this system?
    READ table lt_outtab into ls_outtab
      with key
        install_number = ls_store_dir-install_number
        long_sid       = ls_store_dir-long_sid
        sid            = ls_store_dir-sid
        .
    if sy-subrc = 0.
      tabix = sy-tabix.
    else.
      tabix = -1.
      CLEAR ls_outtab.
      MOVE-CORRESPONDING ls_store_dir TO ls_outtab.
    endif.

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
      "READ TABLE lt_snapshot_elem INTO data(ls_RFCSYSID)      INDEX 1.
      "check ls_RFCSYSID-fieldname = 'RFCSYSID'.

      "READ TABLE lt_snapshot_elem INTO data(ls_TLICENCE_NR)   INDEX 2.
      "check ls_TLICENCE_NR-fieldname = 'TLICENSE_NR'.

      "READ TABLE lt_snapshot_elem INTO data(ls_RFCTRUSTSY)    INDEX 3.
      "check ls_RFCTRUSTSY-fieldname = 'RFCTRUSTSY'.

      "READ TABLE lt_snapshot_elem INTO data(ls_RFCDEST)       INDEX 4.
      "check ls_RFCDEST-fieldname = 'RFCDEST'.

      READ TABLE lt_snapshot_elem INTO data(ls_RFCTCDCHK)     INDEX 5.
      check ls_RFCTCDCHK-fieldname = 'RFCTCDCHK'.

      "READ TABLE lt_snapshot_elem INTO data(ls_RFCSNC)        INDEX 6.
      "check ls_RFCSNC-fieldname = 'RFCSNC'.

      READ TABLE lt_snapshot_elem INTO data(ls_RFCSLOPT)      INDEX 7.
      check ls_RFCSLOPT-fieldname = 'RFCSLOPT'.

      " The following fields are only available in higher versions:

      "READ TABLE lt_snapshot_elem INTO data(ls_RFCCREDEST)    INDEX 8.
      "check ls_RFCCREDEST-fieldname = 'RFCCREDEST'.

      "READ TABLE lt_snapshot_elem INTO data(ls_RFCREGDEST)    INDEX 9.
      "check ls_RFCREGDEST-fieldname = 'RFCREGDEST'.

      "READ TABLE lt_snapshot_elem INTO data(ls_LLICENSE_NR)   INDEX 10.
      "check ls_LLICENSE_NR-fieldname = 'LLICENSE_NR'.

      "READ TABLE lt_snapshot_elem INTO data(ls_RFCSECKEY)     INDEX 11.
      "check ls_RFCSECKEY-fieldname = 'RFCSECKEY'.

      add 1 to ls_outtab-TRUSTSY_cnt_all.

      if ls_RFCTCDCHK-fieldvalue is not initial.
        add 1 to ls_outtab-TRUSTSY_cnt_TCD.
      endif.

      data version(1).
      version = ls_RFCSLOPT-fieldvalue. " get first char of the string
      case version.
        when '3'. add 1 to ls_outtab-TRUSTSY_cnt_3.
        when '2'. add 1 to ls_outtab-TRUSTSY_cnt_2.
        when ' '. add 1 to ls_outtab-TRUSTSY_cnt_1.
      endcase.
    ENDLOOP.

    if tabix > 0.
      MODIFY lt_outtab from ls_outtab INDEX tabix.
    else.
      APPEND ls_outtab TO lt_outtab.
    endif.

  ENDLOOP. " lt_STORE_DIR

ENDFORM. " get_RFCSYSACL


FORM get_RFCDES.
  check P_DEST = 'X'.

  DATA:
    lt_store_dir_tech    TYPE  tt_diagst_store_dir_tech,
    lt_store_dir         TYPE  tt_diagst_store_dir,
    lt_fieldlist         TYPE  tt_diagst_table_store_fields,
    lt_snapshot          TYPE  tt_diagst_trows,
    rc                   TYPE  i,
    rc_text              TYPE  natxt.

  data: tabix type i.

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
      group_landscape_class = 'CL_DIAGLS_ABAP_TECH_SYST' "(optional)
*     GROUP_LANDSCAPE_ID    = ' '
*     GROUP_COMP_ID         = ' '
      group_source          = 'ABAP'                     "(optional)
      group_name            = 'RFC-DESTINATIONS'         "(optional)
      store_category        = 'CONFIG'                   "(optional)
      store_type            = 'TABLE'                    "(optional)
*     STORE_FULLPATH        = ' '
      store_name            = 'RFCDES'

      " Special filters
      store_mainalias       = 'RFC-DESTINATIONS'         "(optional)
      store_subalias        = 'RFCDES'                   "(optional)
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

    " Do we already have an entry for this system?
    READ table lt_outtab into ls_outtab
      with key
        install_number = ls_store_dir-install_number
        long_sid       = ls_store_dir-long_sid
        sid            = ls_store_dir-sid
        .
    if sy-subrc = 0.
      tabix = sy-tabix.
    else.
      tabix = -1.
      CLEAR ls_outtab.
      MOVE-CORRESPONDING ls_store_dir TO ls_outtab.
    endif.

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
      READ TABLE lt_snapshot_elem INTO data(ls_RFCDEST)       INDEX 1.
      check ls_RFCDEST-fieldname = 'RFCDEST'.

      READ TABLE lt_snapshot_elem INTO data(ls_RFCTYPE)       INDEX 2.
      check ls_RFCTYPE-fieldname = 'RFCTYPE'.
      check ls_RFCTYPE-fieldvalue = '3' or  ls_RFCTYPE-fieldvalue = 'H' or ls_RFCTYPE-fieldvalue =  'W'.

      READ TABLE lt_snapshot_elem INTO data(ls_RFCOPTIONS)    INDEX 3.
      check ls_RFCOPTIONS-fieldname = 'RFCOPTIONS'.

      case ls_RFCTYPE-fieldvalue.

        when '3'. " RFC destinations
          P_DEST_3 = 'X'.
          add 1 to ls_outtab-DEST_3_cnt_all.                             " All destinations

          if ls_RFCOPTIONS-fieldvalue cs ',Q=Y,'.                        " Trusted destination
            add 1 to ls_outtab-DEST_3_cnt_trusted.

            find regex ',\[=[^,]{3},'    in ls_RFCOPTIONS-fieldvalue.    " System ID
            if sy-subrc = 0.
              find regex ',\^=[^,]{1,10},' in ls_RFCOPTIONS-fieldvalue.  " Installation number
              if sy-subrc = 0.
                " System ID and installation number are available
                add 1 to ls_outtab-DEST_3_cnt_trusted_migrated.
              else.
                " Installation number is missing
                add 1 to ls_outtab-DEST_3_cnt_trusted_no_instnr.
              endif.
            else.
              " System ID is missing
              add 1 to ls_outtab-DEST_3_cnt_trusted_no_sysid.
            endif.

            if ls_RFCOPTIONS-fieldvalue cs ',s=Y,'.                      " SNC
              add 1 to ls_outtab-DEST_3_cnt_trusted_snc.
            endif.
          endif.

        when 'H'. " http destinations
          P_DEST_H = 'X'.
          add 1 to ls_outtab-DEST_H_cnt_all.                             " All destinations

          if ls_RFCOPTIONS-fieldvalue cs ',Q=Y,'.                        " Trusted destination
            add 1 to ls_outtab-DEST_H_cnt_trusted.

            find regex ',\[=[^,]{3},'    in ls_RFCOPTIONS-fieldvalue.    " System ID
            if sy-subrc = 0.
              find regex ',\^=[^,]{1,10},' in ls_RFCOPTIONS-fieldvalue.  " Installation number
              if sy-subrc = 0.
                " System ID and installation number are available
                add 1 to ls_outtab-DEST_H_cnt_trusted_migrated.
              else.
                " Installation number is missing
                add 1 to ls_outtab-DEST_H_cnt_trusted_no_instnr.
              endif.
            else.
              " System ID is missing
              add 1 to ls_outtab-DEST_H_cnt_trusted_no_sysid.
            endif.

            if ls_RFCOPTIONS-fieldvalue cs ',s=Y,'.                      " TLS
              add 1 to ls_outtab-DEST_H_cnt_trusted_tls.
            endif.
          endif.

        when 'W'. " web RFC destinations
          P_DEST_W = 'X'.
          add 1 to ls_outtab-DEST_W_cnt_all.                             " All destinations

          if ls_RFCOPTIONS-fieldvalue cs ',Q=Y,'.                        " Trusted destination
            add 1 to ls_outtab-DEST_W_cnt_trusted.

            find regex ',\[=[^,]{3},'    in ls_RFCOPTIONS-fieldvalue.    " System ID
            if sy-subrc = 0.
              find regex ',\^=[^,]{1,10},' in ls_RFCOPTIONS-fieldvalue.  " Installation number
              if sy-subrc = 0.
                " System ID and installation number are available
                add 1 to ls_outtab-DEST_W_cnt_trusted_migrated.
              else.
                " Installation number is missing
                add 1 to ls_outtab-DEST_W_cnt_trusted_no_instnr.
              endif.
            else.
              " System ID is missing
              add 1 to ls_outtab-DEST_W_cnt_trusted_no_sysid.
            endif.

            if ls_RFCOPTIONS-fieldvalue cs ',s=Y,'.                      " TLS
              add 1 to ls_outtab-DEST_W_cnt_trusted_tls.
            endif.
          endif.

      endcase.

    ENDLOOP.

    if tabix > 0.
      MODIFY lt_outtab from ls_outtab INDEX tabix.
    else.
      APPEND ls_outtab TO lt_outtab.
    endif.

  ENDLOOP. " lt_STORE_DIR

ENDFORM. " get_RFCDES


FORM get_ABAP_INSTANCE_PAHI.
  check P_TRUST = 'X' or P_DEST = 'X'.

  " Same as in report ZSHOW_KERNEL_STORES but one one entry per system

  DATA:
    lt_store_dir_tech    TYPE  tt_diagst_store_dir_tech,
    lt_store_dir         TYPE  tt_diagst_store_dir,
    lt_fieldlist         TYPE  tt_diagst_table_store_fields,
    lt_snapshot          TYPE  tt_diagst_trows,
    rc                   TYPE  i,
    rc_text              TYPE  natxt.

  data: tabix type i.

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
      store_name            = 'ABAP_INSTANCE_PAHI'

      " Special filters
      store_mainalias       = 'ABAP-PARAMETER'           "(optional)
      store_subalias        = 'PAHI'                     "(optional)
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

    " Do we already have an entry for this system?
    READ table lt_outtab into ls_outtab
      with key
        install_number = ls_store_dir-install_number
        long_sid       = ls_store_dir-long_sid
        sid            = ls_store_dir-sid
        .
    if sy-subrc = 0.
      tabix = sy-tabix.
      if ls_store_dir-instance_type ne 'CENTRAL'.
        continue.
      endif.
    else.
      tabix = -1.
      CLEAR ls_outtab.
      MOVE-CORRESPONDING ls_store_dir TO ls_outtab.
    endif.

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
      READ TABLE lt_snapshot_elem INTO data(ls_PARAMETER) INDEX 1.
      check ls_PARAMETER-fieldname = 'PARAMETER'.

      READ TABLE lt_snapshot_elem INTO data(ls_VALUE)     INDEX 2.
      check ls_VALUE-fieldname = 'VALUE'.

      case ls_PARAMETER-fieldvalue.
        when 'rfc/selftrust'.         ls_outtab-rfc_selftrust         = ls_VALUE-fieldvalue.
        when 'rfc/allowoldticket4tt'. ls_outtab-rfc_allowoldticket4tt = ls_VALUE-fieldvalue.
        when 'rfc/sendInstNr4tt'.     ls_outtab-rfc_sendInstNr4tt     = ls_VALUE-fieldvalue.
      endcase.
    ENDLOOP.

    if tabix > 0.
      MODIFY lt_outtab from ls_outtab INDEX tabix.
    else.
      APPEND ls_outtab TO lt_outtab.
    endif.

  ENDLOOP. " lt_STORE_DIR

ENDFORM. " get_ABAP_INSTANCE_PAHI


FORM validate_kernel.
  check P_KERN = 'X'.

* Minimum Kernel
* The solution works only if both the client systems as well as the server systems of a trusting/trusted connection runs on a suitable Kernel version:
* 7.22       1214
* 7.49       Not Supported. Use 753 / 754 instead
* 7.53       (1028) 1036
* 7.54       18
* 7.77       (500) 516
* 7.81       (251) 300
* 7.85       (116, 130)  214
* 7.88       21
* 7.89       10

  data:
    rel   type i,
    patch type i.

  loop at lt_outtab ASSIGNING FIELD-SYMBOL(<fs_outtab>).

    if <fs_outtab>-kern_rel is initial or <fs_outtab>-kern_patchlevel is INITIAL.
      <fs_outtab>-validate_kernel = 'Unknown Kernel'.
      APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_normal ) TO <fs_outtab>-t_color.

    else.
      rel   = <fs_outtab>-kern_rel(3).
      patch = ls_outtab-kern_patchlevel.

      if     rel = 722 and patch < 1214
        or   rel = 753 and patch < 1036
        or   rel = 754 and patch < 18
        or   rel = 777 and patch < 516
        or   rel = 781 and patch < 300
        or   rel = 785 and patch < 214
        or   rel = 788 and patch < 21
        or   rel = 789 and patch < 10
        .
        <fs_outtab>-validate_kernel = 'Kernel patch required'.
        APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_total ) TO <fs_outtab>-t_color.

      elseif rel < 722
          or rel > 722 and rel < 753
          .
        <fs_outtab>-validate_kernel = `Release update required`.
        APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_negative ) TO <fs_outtab>-t_color.

      else.

        <fs_outtab>-validate_kernel = 'ok'.
        APPEND VALUE #( fname = 'VALIDATE_KERNEL' color-col = col_positive ) TO <fs_outtab>-t_color.

      endif.
    endif.
  endloop.
ENDFORM. " validate_kernel


FORM validate_ABAP.
  check P_ABAP = 'X' or P_TRUST = 'X' or P_DEST = 'X'.

* Minimum SAP_BASIS for SNOTE
* You only can implement the notes 3089413 and 3287611 using transaction SNOTE if the system runs on a suitable ABAP version:
*                minimum   Note 3089413 solved   Note 3287611 solved
* SAP_BASIS 700  SP 35     SP 41
* SAP_BASIS 701  SP 20     SP 26
* SAP_BASIS 702  SP 20     SP 26
* SAP_BASIS 731  SP 19     SP 33
* SAP_BASIS 740  SP 16     SP 30
* SAP_BASIS 750  SP 12     SP 26                 SP 27
* SAP_BASIS 751  SP 7      SP 16                 SP 17
* SAP_BASIS 752  SP 1      SP 12
* SAP_BASIS 753            SP 10
* SAP_BASIS 754            SP 8
* SAP_BASIS 755            SP 6
* SAP_BASIS 756            SP 4
* SAP_BASIS 757            SP 2

  data:
    rel   type i,
    SP    type i.

  loop at lt_outtab assigning FIELD-SYMBOL(<fs_outtab>).

    " Validate release and SP
    if <fs_outtab>-ABAP_RELEASE is initial or <fs_outtab>-ABAP_SP is INITIAL.
      <fs_outtab>-validate_ABAP = 'Unknown ABAP version'.
      APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_normal ) TO <fs_outtab>-t_color.

    else.
      rel   = <fs_outtab>-ABAP_RELEASE.
      SP    = <fs_outtab>-ABAP_SP.

      if     rel < 700
        or   rel = 700 and SP < 35
        or   rel = 701 and SP < 20
        or   rel = 702 and SP < 20
        or   rel = 731 and SP < 19
        or   rel = 740 and SP < 16
        or   rel = 750 and SP < 12
        or   rel = 751 and SP < 7
        or   rel = 752 and SP < 1
        .
        <fs_outtab>-validate_ABAP = 'ABAP SP required'.
        APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_negative ) TO <fs_outtab>-t_color.

      elseif rel = 700 and SP < 41
        or   rel = 701 and SP < 26
        or   rel = 702 and SP < 26
        or   rel = 731 and SP < 33
        or   rel = 740 and SP < 30
        or   rel = 750 and SP < 26
        or   rel = 751 and SP < 16
        or   rel = 752 and SP < 12
        or   rel = 753 and SP < 10
        or   rel = 754 and SP < 8
        or   rel = 755 and SP < 6
        or   rel = 756 and SP < 4
        or   rel = 757 and SP < 2
        or   rel > 757
        .
        <fs_outtab>-validate_ABAP = 'Note required'.
        APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_total ) TO <fs_outtab>-t_color.

        if <fs_outtab>-NOTE_3089413 is initial.
           <fs_outtab>-NOTE_3089413 = 'required'.
        endif.
        if <fs_outtab>-NOTE_3287611 is initial.
           <fs_outtab>-NOTE_3287611 = 'required'.
        endif.

      elseif rel = 750 and SP < 27
        or   rel = 751 and SP < 17
        .
        <fs_outtab>-validate_ABAP = 'Note required'.
        APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_total ) TO <fs_outtab>-t_color.

        if <fs_outtab>-NOTE_3089413 is initial.
           <fs_outtab>-NOTE_3089413 = 'ok'.
        endif.
        if <fs_outtab>-NOTE_3287611 is initial.
           <fs_outtab>-NOTE_3287611 = 'required'.
        endif.

      else.
        <fs_outtab>-validate_ABAP = 'ok'.
        APPEND VALUE #( fname = 'VALIDATE_ABAP' color-col = col_positive ) TO <fs_outtab>-t_color.

        if <fs_outtab>-NOTE_3089413 is initial.
           <fs_outtab>-NOTE_3089413 = 'ok'.
        endif.
        if <fs_outtab>-NOTE_3287611 is initial.
           <fs_outtab>-NOTE_3287611 = 'ok'.
        endif.

      endif.
    endif.

    " Validate notes
    "   Undefined Implementation State
    " -	Cannot be implemented
    " E	Completely implemented
    " N	Can be implemented
    " O	Obsolete
    " U	Incompletely implemented
    " V	Obsolete version implemented
    case <fs_outtab>-NOTE_3089413_PRSTATUS.
      when 'E' or '-'.
        APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_positive ) TO <fs_outtab>-t_color.
      when 'N' or 'O' or 'U' or 'V'.
        APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_negative ) TO <fs_outtab>-t_color.
      when others.
        if     <fs_outtab>-NOTE_3089413 = 'ok'.
          APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_positive ) TO <fs_outtab>-t_color.
        elseif <fs_outtab>-NOTE_3089413 = 'required'.
          APPEND VALUE #( fname = 'NOTE_3089413' color-col = col_negative ) TO <fs_outtab>-t_color.
        endif.
    endcase.
    case <fs_outtab>-NOTE_3287611_PRSTATUS.
      when 'E' or '-'.
        APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_positive ) TO <fs_outtab>-t_color.
      when 'N' or 'O' or 'U' or 'V'.
        APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_negative ) TO <fs_outtab>-t_color.
      when others.
        if     <fs_outtab>-NOTE_3287611 = 'ok'.
          APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_positive ) TO <fs_outtab>-t_color.
        elseif <fs_outtab>-NOTE_3287611 = 'required'.
          APPEND VALUE #( fname = 'NOTE_3287611' color-col = col_negative ) TO <fs_outtab>-t_color.
        endif.
    endcase.

    " Validate trusted systems
    if <fs_outtab>-TRUSTSY_cnt_3 > 0.
      APPEND VALUE #( fname = 'TRUSTSY_CNT_3' color-col = col_positive ) TO <fs_outtab>-t_color.
    endif.
    if <fs_outtab>-TRUSTSY_cnt_2 > 0.
      APPEND VALUE #( fname = 'TRUSTSY_CNT_2' color-col = col_negative ) TO <fs_outtab>-t_color.
    endif.
    if <fs_outtab>-TRUSTSY_cnt_1 > 0.
      APPEND VALUE #( fname = 'TRUSTSY_CNT_1' color-col = col_negative ) TO <fs_outtab>-t_color.
    endif.

    " Validate TCD flag
    if <fs_outtab>-TRUSTSY_CNT_TCD > 0.
      APPEND VALUE #( fname = 'TRUSTSY_CNT_TCD' color-col = col_positive ) TO <fs_outtab>-t_color.
    endif.

    " Validate rfc/selftrust
    if <fs_outtab>-rfc_selftrust = '0'.
      APPEND VALUE #( fname = 'RFC_SELFTRUST' color-col = col_positive ) TO <fs_outtab>-t_color.
    elseif <fs_outtab>-rfc_selftrust = '1'.
      APPEND VALUE #( fname = 'RFC_SELFTRUST' color-col = col_total ) TO <fs_outtab>-t_color.
    endif.

    " Validate trusted destinations
    if <fs_outtab>-DEST_3_CNT_TRUSTED_MIGRATED > 0.
      APPEND VALUE #( fname = 'DEST_3_CNT_TRUSTED_MIGRATED' color-col = col_positive ) TO <fs_outtab>-t_color.
    endif.
    if <fs_outtab>-DEST_3_CNT_TRUSTED_NO_INSTNR > 0.
      APPEND VALUE #( fname = 'DEST_3_CNT_TRUSTED_NO_INSTNR' color-col = col_negative ) TO <fs_outtab>-t_color.
    endif.
    if <fs_outtab>-DEST_3_CNT_TRUSTED_NO_SYSID > 0.
      APPEND VALUE #( fname = 'DEST_3_CNT_TRUSTED_NO_SYSID' color-col = col_negative ) TO <fs_outtab>-t_color.
    endif.

    if <fs_outtab>-DEST_H_CNT_TRUSTED_MIGRATED > 0.
      APPEND VALUE #( fname = 'DEST_H_CNT_TRUSTED_MIGRATED' color-col = col_positive ) TO <fs_outtab>-t_color.
    endif.
    if <fs_outtab>-DEST_H_CNT_TRUSTED_NO_INSTNR > 0.
      APPEND VALUE #( fname = 'DEST_H_CNT_TRUSTED_NO_INSTNR' color-col = col_negative ) TO <fs_outtab>-t_color.
    endif.
    if <fs_outtab>-DEST_H_CNT_TRUSTED_NO_SYSID > 0.
      APPEND VALUE #( fname = 'DEST_H_CNT_TRUSTED_NO_SYSID' color-col = col_negative ) TO <fs_outtab>-t_color.
    endif.

    if <fs_outtab>-DEST_W_CNT_TRUSTED_MIGRATED > 0.
      APPEND VALUE #( fname = 'DEST_W_CNT_TRUSTED_MIGRATED' color-col = col_positive ) TO <fs_outtab>-t_color.
    endif.
    if <fs_outtab>-DEST_W_CNT_TRUSTED_NO_INSTNR > 0.
      APPEND VALUE #( fname = 'DEST_W_CNT_TRUSTED_NO_INSTNR' color-col = col_negative ) TO <fs_outtab>-t_color.
    endif.
    if <fs_outtab>-DEST_W_CNT_TRUSTED_NO_SYSID > 0.
      APPEND VALUE #( fname = 'DEST_3_CNT_TRUSTED_NO_SYSID' color-col = col_negative ) TO <fs_outtab>-t_color.
    endif.

  endloop.
ENDFORM. " validate_ABAP


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

* ...Set the layout
  lr_layout = lr_table->get_layout( ).
  ls_layout_key-report = sy-repid.
  lr_layout->set_key( ls_layout_key ).
  lr_layout->set_initial_layout( P_LAYOUT ).
  authority-check object 'S_ALV_LAYO'
                      id 'ACTVT' field '23'.
  if sy-subrc = 0.
    lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ) . "no restictions
  else.
    lr_layout->set_save_restriction( cl_salv_layout=>restrict_user_dependant ) . "user dependend
  endif.

*... sort
  TRY.
      lr_sorts = lr_table->get_sorts( ).
      lr_sorts->add_sort( 'INSTALL_NUMBER' ).
      lr_sorts->add_sort( 'LONG_SID' ).
      lr_sorts->add_sort( 'SID' ).

    CATCH cx_salv_data_error cx_salv_existing cx_salv_not_found.
  ENDTRY.

*... set column appearance
  lr_columns = lr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ). " Optimize column width

  TRY.
      lr_columns->set_color_column( 'T_COLOR' ).
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
  ENDTRY.

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

      lr_column ?= lr_columns->get_column( 'COMPV_NAME' ).
      lr_column->set_long_text( 'ABAP release' ).   "max. 40 characters
      lr_column->set_medium_text( 'ABAP release' ). "max. 20 characters
      lr_column->set_short_text( 'ABAP rel.' ).     "max. 10 characters

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

      " Trusted systems

      lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_ALL' ).
      lr_column->set_long_text( 'All trusted systems' ).
      lr_column->set_medium_text( 'All trusted systems' ).
      lr_column->set_short_text( 'Trusted' ).
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

      " Profile parameter

      lr_column ?= lr_columns->get_column( 'RFC_ALLOWOLDTICKET4TT' ).
      lr_column->set_long_text( 'Allow old ticket' ).
      lr_column->set_medium_text( 'Allow old ticket' ).
      lr_column->set_short_text( 'Allow old' ).

      lr_column ?= lr_columns->get_column( 'RFC_SELFTRUST' ).
      lr_column->set_long_text( 'RFC selftrust' ).
      lr_column->set_medium_text( 'RFC selftrust' ).
      lr_column->set_short_text( 'Selftrust' ).

      lr_column ?= lr_columns->get_column( 'RFC_SENDINSTNR4TT' ).
      lr_column->set_long_text( 'Send installation number' ).
      lr_column->set_medium_text( 'Send installation nr' ).
      lr_column->set_short_text( 'SendInstNr' ).

      " Type 3 Destinations
      color-col = 2. " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange

      lr_column ?= lr_columns->get_column( 'DEST_3_CNT_ALL' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'RFC Destinations (Type 3)' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'RFC Destinations' ).
                                 "1234567890
      lr_column->set_short_text( 'RFC Dest.' ).
      lr_column->set_zero( abap_false  ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED'  ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'Trusted RFC Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'Trusted RFC Dest.' ).
                                 "1234567890
      lr_column->set_short_text( 'Trust.RFC' ).
      lr_column->set_zero( abap_false  ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_MIGRATED' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'Migrated Trusted RFC Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'Migrated Trusted' ).
                                 "1234567890
      lr_column->set_short_text( 'Migrated' ).
      lr_column->set_zero( abap_false  ).

      lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_NO_INSTNR' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'No Installation Number in Trusted Dest.' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'No Installation Nr' ).
                                 "1234567890
      lr_column->set_short_text( 'No Inst Nr' ).
      lr_column->set_zero( abap_false  ).

      lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_NO_SYSID' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'No System ID in Trusted RFC Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'No System ID' ).
                                 "1234567890
      lr_column->set_short_text( 'No Sys. ID' ).
      lr_column->set_zero( abap_false  ).

      lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_SNC' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'SNC for Trusted RFC Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'SNC Trusted Dest' ).
                                 "1234567890
      lr_column->set_short_text( 'SNC Trust.' ).
      lr_column->set_zero( abap_false  ).

      " Type H Destinations

      lr_column ?= lr_columns->get_column( 'DEST_H_CNT_ALL' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'HTTP Destinations (Type H)' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'HTTP Destinations' ).
                                 "1234567890
      lr_column->set_short_text( 'HTTP Dest.' ).
      lr_column->set_zero( abap_false  ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED'  ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'Trusted HTTP Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'Trusted HTTPS Dest.' ).
                                 "1234567890
      lr_column->set_short_text( 'Trust.HTTP' ).
      lr_column->set_zero( abap_false  ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_MIGRATED' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'Migrated Trusted HTTP Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'Migrated Trusted' ).
                                 "1234567890
      lr_column->set_short_text( 'Migrated' ).
      lr_column->set_zero( abap_false  ).

      lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_NO_INSTNR' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'No Installation Number in Trusted Dest.' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'No Installation Nr' ).
                                 "1234567890
      lr_column->set_short_text( 'No Inst Nr' ).
      lr_column->set_zero( abap_false  ).

      lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_NO_SYSID' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'No System ID in Trusted HTTP Dest.' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'No System ID' ).
                                 "1234567890
      lr_column->set_short_text( 'No Sys. ID' ).
      lr_column->set_zero( abap_false  ).

      lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_TLS' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'TLS for Trusted HTTP Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'TLS Trusted Dest.' ).
                                 "1234567890
      lr_column->set_short_text( 'TLS Trust.' ).
      lr_column->set_zero( abap_false  ).

      " Type W Destinations

      lr_column ?= lr_columns->get_column( 'DEST_W_CNT_ALL' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'WebRFC Destinations (Type W)' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'WebRFC Destinations' ).
                                 "1234567890
      lr_column->set_short_text( 'Web Dest.' ).
      lr_column->set_zero( abap_false  ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED'  ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'Trusted WebRFC Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'Trusted WebRFC Dest.' ).
                                 "1234567890
      lr_column->set_short_text( 'Trust Web' ).
      lr_column->set_zero( abap_false  ).
      lr_column->set_color( color ).

      lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_MIGRATED' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'Migrated Trusted WebRFC Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'Migrated Trusted' ).
                                 "1234567890
      lr_column->set_short_text( 'Migrated' ).
      lr_column->set_zero( abap_false  ).

      lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_NO_INSTNR' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'No Installation Number in Trusted Dest.' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'No Installation Nr' ).
                                 "1234567890
      lr_column->set_short_text( 'No Inst Nr' ).
      lr_column->set_zero( abap_false  ).

      lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_NO_SYSID' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'No System ID in Trusted WebRFC Dest.' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'No System ID' ).
                                 "1234567890
      lr_column->set_short_text( 'No Sys. ID' ).
      lr_column->set_zero( abap_false  ).

      lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_TLS' ).
                                "1234567890123456789012345678901234567890
      lr_column->set_long_text( 'TLS for Trusted WebRFC Destinations' ).
                                  "12345678901234567890
      lr_column->set_medium_text( 'TLS Trusted Dest.' ).
                                 "1234567890
      lr_column->set_short_text( 'TLS Trust.' ).
      lr_column->set_zero( abap_false  ).


*... hide unimportant columns
      lr_column ?= lr_columns->get_column( 'LONG_SID' ).                lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'SID' ).                     lr_column->set_visible( abap_true ).

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

      lr_column ?= lr_columns->get_column( 'STORE_ID' ).                lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'STORE_LAST_UPLOAD' ).       lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'STORE_STATE' ).             lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'STORE_MAIN_STATE_TYPE' ).   lr_column->set_visible( abap_false ).
      lr_column ?= lr_columns->get_column( 'STORE_MAIN_STATE' ).        lr_column->set_visible( abap_true ).
      lr_column ?= lr_columns->get_column( 'STORE_OUTDATED_DAY' ).      lr_column->set_visible( abap_false ).

      if P_KERN is initial.
        lr_column ?= lr_columns->get_column( 'KERN_REL' ).              lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'KERN_PATCHLEVEL' ).       lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'KERN_COMP_TIME' ).        lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'KERN_COMP_DATE' ).        lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'VALIDATE_KERNEL' ).       lr_column->set_technical( abap_true ).
      endif.

      if P_ABAP is initial.
        lr_column ?= lr_columns->get_column( 'ABAP_RELEASE' ).          lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'ABAP_SP' ).               lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'VALIDATE_ABAP' ).         lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'NOTE_3089413' ).          lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'NOTE_3089413_PRSTATUS' ). lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'NOTE_3287611' ).          lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'NOTE_3287611_PRSTATUS' ). lr_column->set_technical( abap_true ).
      endif.

      if P_TRUST is initial.
        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_ALL' ).       lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_TCD' ).       lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_3' ).         lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_2' ).         lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'TRUSTSY_CNT_1' ).         lr_column->set_technical( abap_true ).

        lr_column ?= lr_columns->get_column( 'RFC_SELFTRUST' ).         lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'RFC_SENDINSTNR4TT' ).     lr_column->set_technical( abap_true ).
      endif.

      if P_DEST is initial or P_DEST_3 is initial.
        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_ALL' ).               lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED' ).           lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_MIGRATED' ).  lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_NO_INSTNR' ). lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_NO_SYSID' ).  lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_3_CNT_TRUSTED_SNC' ).       lr_column->set_technical( abap_true ).
      endif.

      if P_DEST is initial or P_DEST_H is initial.
        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_ALL' ).               lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED' ).           lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_MIGRATED' ).  lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_NO_INSTNR' ). lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_NO_SYSID' ).  lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_H_CNT_TRUSTED_TLS' ).       lr_column->set_technical( abap_true ).
      endif.

      if P_DEST is initial or P_DEST_W is initial.
        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_ALL' ).               lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED' ).           lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_MIGRATED' ).  lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_NO_INSTNR' ). lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_NO_SYSID' ).  lr_column->set_technical( abap_true ).
        lr_column ?= lr_columns->get_column( 'DEST_W_CNT_TRUSTED_TLS' ).       lr_column->set_technical( abap_true ).
      endif.

      if P_TRUST is initial and P_DEST is initial.
        lr_column ?= lr_columns->get_column( 'RFC_ALLOWOLDTICKET4TT' ). lr_column->set_technical( abap_true ).
      endif.

    CATCH cx_salv_not_found.
  ENDTRY.

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
