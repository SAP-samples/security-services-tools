*&-.-------------------------------------------------------------------*
*& Report ZSHOW_UCON_RFC_DATA
*& Show extended UCON data for RFC
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Published on: https://github.com/SAP-samples/security-services-tools
*&
*& 26.04.2024 Initial version
*& 29.04.2024 Value help for function, and user
*&            Additional columns to mark called functions, function groups, packages, and software components
*&            Freeze function column while scrolling horizontally
*& 30.04.2024 Show blocklist package from view V_RFCBL_SERVER as of 7.50
*&            Store radio button selection in (hidden) user parameter
*&            Show authorizations of users in extended view
*& 06.05.2024 Small performance optimization
*&            Hide selection for client and user in case of the simple view
*&            Value help for client and user group
*&            Show authorizations in red if there are any * authorizations
*&            Navigation to function, function group or package
*& 08.05.2024 Selection for source of statistical data
*&            Selection for called/uncalled function groups, packages, software components
*&            Interactive functions: display/change, set phase
*& 15.05.2024 Interactive functions: assign to CA, assign to SNC CA, remove from CA
*& 27.05.2024 Interactive functions: show called user in current client
*&                                   show popup about authorizations
*& 28.05.2024 Optimization for showing authorizationn data
*& 13.06.2024 Optimization for progress indicator
*&            Show error if function is not RFC enabled anymore if function does not exist anymore
*&            Use report RS_UCON_CLEAN_RFC_STATE to repair these entries
*&            Show status of switched packages
*&            Performance optimization concerning view V_RFCBL_SERVER as of 7.50
*& 14.06.2024 Performance optimization for reading function data
*&---------------------------------------------------------------------*
REPORT zshow_ucon_rfc_data.

CONSTANTS c_program_version(30) TYPE c VALUE '14.06.2024 S41'.

* Selection screen
TABLES sscrfields.
SELECTION-SCREEN: FUNCTION KEY 1. "UCON Cockpit

" System
SELECTION-SCREEN BEGIN OF BLOCK system WITH FRAME TITLE tit_sys.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (30) t_system FOR FIELD p_system.
    TYPES tf_system TYPE c LENGTH 15.
    PARAMETERS p_system TYPE tf_system AS LISTBOX VISIBLE LENGTH 22 USER-COMMAND p_system.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK system.

" Standard view for functions / Enhanced view incl. destinations and users
SELECTION-SCREEN BEGIN OF BLOCK cols WITH FRAME TITLE tit_col.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS p_simp  RADIOBUTTON GROUP cols USER-COMMAND cols.
    SELECTION-SCREEN COMMENT (28) t_simp FOR FIELD p_simp.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS p_comp  RADIOBUTTON GROUP cols.
    SELECTION-SCREEN COMMENT (40) t_comp FOR FIELD p_comp.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK cols.

" RFC Function modules
SELECTION-SCREEN BEGIN OF BLOCK fumo WITH FRAME TITLE tit_fum.
  " Function name
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_fumo FOR FIELD so_fumo.
    DATA funcname TYPE tfdir-funcname.
    SELECT-OPTIONS so_fumo FOR funcname.
  SELECTION-SCREEN END OF LINE.
  " Function group
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_area FOR FIELD so_area.
    DATA area TYPE enlfdir-area.
    SELECT-OPTIONS so_area FOR area.
  SELECTION-SCREEN END OF LINE.
  " Package
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_devc FOR FIELD so_devc.
    DATA devclass TYPE tadir-devclass.
    SELECT-OPTIONS so_devc FOR devclass.
  SELECTION-SCREEN END OF LINE.
  " Software component
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_unit FOR FIELD so_unit.
    DATA dlvunit TYPE tdevc-dlvunit.
    SELECT-OPTIONS so_unit FOR dlvunit.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_switch AS CHECKBOX MODIF ID swt.
    SELECTION-SCREEN COMMENT (60) t_switch FOR FIELD p_switch MODIF ID swt.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_bl_srv AS CHECKBOX MODIF ID blk.
    SELECTION-SCREEN COMMENT (60) t_bl_srv FOR FIELD p_bl_srv MODIF ID blk.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK fumo.

" Called users
SELECTION-SCREEN BEGIN OF BLOCK user WITH FRAME TITLE tit_usr.
  " Client
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_mandt FOR FIELD so_mandt MODIF ID usr.
    DATA mandt TYPE usr02-mandt.
    SELECT-OPTIONS so_mandt FOR mandt DEFAULT sy-mandt MODIF ID usr.
  SELECTION-SCREEN END OF LINE.
  " User
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_bname FOR FIELD so_bname MODIF ID usr.
    DATA bname TYPE usr02-bname.
    SELECT-OPTIONS so_bname FOR bname MODIF ID usr.
  SELECTION-SCREEN END OF LINE.
  " User type
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_ustyp FOR FIELD so_ustyp MODIF ID usr.
    DATA ustyp TYPE usr02-ustyp.
    SELECT-OPTIONS so_ustyp FOR ustyp MODIF ID usr.
  SELECTION-SCREEN END OF LINE.
  " User group
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_class FOR FIELD so_class MODIF ID usr.
    DATA class TYPE usr02-class.
    SELECT-OPTIONS so_class FOR class MODIF ID usr.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK user.

" Called / uncalled / all
SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE tit_mde.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_mon RADIOBUTTON GROUP stat.                 " Called functions
    SELECTION-SCREEN COMMENT (28) t_mon FOR FIELD p_mon.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS: p_nmon  RADIOBUTTON GROUP stat.               " Not called functions
    SELECTION-SCREEN COMMENT (28) t_nmon FOR FIELD p_nmon.
    SELECTION-SCREEN POSITION 64.
    PARAMETERS: p_all RADIOBUTTON GROUP stat.                 " All functions
    SELECTION-SCREEN COMMENT (28) t_all FOR FIELD p_all.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_mona RADIOBUTTON GROUP stat.                " Called function groups
    SELECTION-SCREEN COMMENT (28) t_mona FOR FIELD p_mona.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS: p_nmona  RADIOBUTTON GROUP stat.              " Not called function groups
    SELECTION-SCREEN COMMENT (28) t_nmona FOR FIELD p_nmona.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_mond RADIOBUTTON GROUP stat.                " Called packages
    SELECTION-SCREEN COMMENT (28) t_mond FOR FIELD p_mond.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS: p_nmond  RADIOBUTTON GROUP stat.              " Not called packages
    SELECTION-SCREEN COMMENT (28) t_nmond FOR FIELD p_nmond.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_mons RADIOBUTTON GROUP stat.                " Called software components
    SELECTION-SCREEN COMMENT (28) t_mons FOR FIELD p_mons.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS: p_nmons  RADIOBUTTON GROUP stat.              " Not called software components
    SELECTION-SCREEN COMMENT (28) t_nmons FOR FIELD p_nmons.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK mode.

" Assigned / not assigned / all
SELECTION-SCREEN BEGIN OF BLOCK assig WITH FRAME TITLE tit_asg.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_assi RADIOBUTTON GROUP assi.
    SELECTION-SCREEN COMMENT (28) t_assi FOR FIELD p_assi.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS: p_unas  RADIOBUTTON GROUP assi.
    SELECTION-SCREEN COMMENT (28) t_unas FOR FIELD p_unas.
    SELECTION-SCREEN POSITION 64.
    PARAMETERS: p_both RADIOBUTTON GROUP assi.
    SELECTION-SCREEN COMMENT (28) t_both FOR FIELD p_both.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_assi_s RADIOBUTTON GROUP assi.
    SELECTION-SCREEN COMMENT (28) t_assi_s FOR FIELD p_assi_s.
    SELECTION-SCREEN POSITION 32.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK assig.

" Phase logging / evaluation / final
" Phase expired logging / evaluation / all
SELECTION-SCREEN BEGIN OF BLOCK choice WITH FRAME TITLE tit_chc.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_log RADIOBUTTON GROUP grob.
    SELECTION-SCREEN COMMENT (28) t_log FOR FIELD p_log.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS: p_eval  RADIOBUTTON GROUP grob.
    SELECTION-SCREEN COMMENT (28) t_eval FOR FIELD p_eval.
    SELECTION-SCREEN POSITION 64.
    PARAMETERS: p_act RADIOBUTTON GROUP grob.
    SELECTION-SCREEN COMMENT (28) t_act FOR FIELD p_act.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_log_e RADIOBUTTON GROUP grob.
    SELECTION-SCREEN COMMENT (28) t_log_e FOR FIELD p_log_e.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS: p_eval_e RADIOBUTTON GROUP grob.
    SELECTION-SCREEN COMMENT (28) t_eval_e FOR FIELD p_eval_e.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_all_p RADIOBUTTON GROUP grob.
    SELECTION-SCREEN COMMENT (28) t_all_p FOR FIELD p_all_p.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK choice.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) t_layout FOR FIELD layout.
  PARAMETERS       layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

TYPES:
  BEGIN OF ts_ucon_phase_tool_fields_ext,                  "* Enhanced list
    funcname               TYPE tfdir-funcname,            "* CHAR 30  Name of Function Module

    fmode                  TYPE tfdir-fmode,               " additional field
    func_text              TYPE tftit-stext,               " additional field
    func_called            TYPE char1,                     " additional field
    area                   TYPE enlfdir-area,              " additional field
    area_text              TYPE tlibt-areat,               " additional field
    area_called            TYPE char1,                     " additional field
    devclass               TYPE tadir-devclass,            " additional field
    devclass_text          TYPE tdevct-ctext,              " additional field
    devclass_called        TYPE char1,                     " additional field
    switch_id              TYPE sfw_package-switch_id,     " additional field
    switch_name            TYPE sfw_switcht-name32,        " additional field
    switch_state           TYPE string, "sfw_switchpos,    " additional field
    dlvunit                TYPE tdevc-dlvunit,             " additional field
    dlvunit_called         TYPE char1,                     " additional field
    blpackage              TYPE devclass,                  " additional field, field v_rfcbl_server-blpackage exists as of 7.50

    actual_phase           TYPE  uconrfcphase,             "* CHAR 1   Phase of an RFC Function module in CA fill process
    phasetext              TYPE string, "c LENGTH 10,

    "spau_relevant          TYPE  uconspaurelevant,        "  CHAR 1   RFC Service should apear in SPAU
    "duration_days          TYPE  int4,                    "  INT4 10  4 Byte Signed Integer
    end_phase              TYPE  as4date,                  "* DATS 8   Last Changed On
    "secure_by_default      TYPE  sap_bool,                "* CHAR 1   Boolean Variable (X=True, Space=False)
    "origin_sid             TYPE  sysysid,                 "  CHAR 8   Name of SAP System
    id                     TYPE  uconservid,               "* CHAR 30  Unified Connectivity Service Assembly  ID
    "version                TYPE  uconstate,               "  CHAR 1   State of Object
    "rfcfuncname            TYPE  rs38l_fnam,              "  CHAR 30  Name of Function Module
    "called_rfm             TYPE  funcname,                "  CHAR 30  Function name

    called_sid             TYPE  sysysid,                  "* CHAR 8   Name of SAP System
    called_installation_nr TYPE  uconinstnr,               "* CHAR 10  Installation Number
    called_client          TYPE  symandt,                  "* CLNT 3   Client ID
    called_user            TYPE  syuname,                  "* CHAR 12  User Name

    ustyp                  TYPE  usr02-ustyp,              " additional field
    class                  TYPE  usr02-class,              " additional field
    authorizations         TYPE  string,                   " additional field

    "executive_instance     TYPE  uconexecutiveinstance,   "  CHAR 40  Executive ABAP instance which performs the RFC Call
    "called_vh              TYPE  uconvirthostname,        "* CHAR 30  Name of virt. hosts
    "ucon_phase             TYPE  c LENGTH 1,              "  CHAR 1   UCON RFC Phase (L =Logging; E=Evaluation; A=Active)
    "external_connector     TYPE  ucon_external_connector, "  CHAR 1   External Connector, which calls into an ABAP system
    rejected_rfc_call      TYPE  uconrfc_reject_call,      "* CHAR 1   Rejected RFC Call ('X' = Rejected; space not Rejected)

    caller_sid             TYPE  sysysid,                  "* CHAR 8   Name of SAP System
    caller_instance        TYPE  uconpartnerinstance,      "* CHAR 40  Partner Instance
    caller_client          TYPE  uconclnt,                 "* CHAR 3   Caller Client
    caller_user            TYPE  uconuserid,               "  CHAR 12  Caller User Name
    "caller_program         TYPE  progname,                "  CHAR 40  ABAP Program Name
    caller_destination     TYPE  rfcdest,                  "* CHAR 32  Logical destination (specified in function call)

    "last_root_context_id   TYPE  uconrootcntxid,          "  CHAR 35  Root Context ID
    "last_connection_id     TYPE  uconconnectionid,        "  CHAR 35  ID of Connection Type

    firstcall_timestamp    TYPE  salv_tstmp, "timestamp,   "* DEC  15  UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
    previouscall_timestamp TYPE  salv_tstmp, "timestamp,   "* DEC  15  UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
    lastcall_timestamp     TYPE  salv_tstmp, "timestamp,   "* DEC  15  UTC Time Stamp in Short Form (YYYYMMDDhhmmss)

    counter_same_system    TYPE  i,                        "* INT4 10  Counter for external RFC from same system & different client
    counter_other_system   TYPE  i,                        "* INT4 10  Counter for external  RFC call from other systems
    counter                TYPE  i,                        "  INT4 10

    "nosnc                  TYPE  sap_bool,                "  CHAR 1   Boolean Variable (X=True, Space=False)
    "snc                    TYPE  sap_bool,                "  CHAR 1   Boolean Variable (X=True, Space=False)
    "counter_same_l         TYPE  i,                       "  INT8 19  Counter for calls from same system but other client
    "counter_other_l        TYPE  i,                       "  INT8 19  Counter for calls from external system

    ctab                   TYPE lvc_t_scol,                "  Color field for ALV cells
  END OF ts_ucon_phase_tool_fields_ext,
  tt_ucon_phase_tool_fields_ext TYPE TABLE OF ts_ucon_phase_tool_fields_ext.

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_para,
        list                   TYPE char6,
        block_srv              TYPE char6,
        call_status            TYPE char6,
        communication_assembly TYPE char6,
        phase                  TYPE char6,
        layout                 TYPE disvariant-variant,
        switch                 TYPE char6,
      END OF ts_para,
      tt_sel_area     TYPE RANGE OF enlfdir-area,
      tt_sel_devclass TYPE RANGE OF tadir-devclass,
      tt_sel_dlvunit  TYPE RANGE OF tdevc-dlvunit,
      tt_sel_mandt    TYPE RANGE OF usr02-mandt,
      tt_sel_bname    TYPE RANGE OF usr02-bname,
      tt_sel_ustyp    TYPE RANGE OF usr02-ustyp,
      tt_sel_class    TYPE RANGE OF usr02-class.

    CLASS-METHODS:

      initialization,

      at_selection_screen_output,

      at_selection_screen_f4_fumo,

      at_selection_screen_f4_mandt,

      at_selection_screen_f4_bname,

      at_selection_screen_f4_class,

      at_selection_screen_f4_layout
        CHANGING
          layout TYPE disvariant-variant,

      at_selection_screen,

      start_of_selection
        IMPORTING
          p_system         TYPE tf_system " System and installation number

          p_simp           TYPE abap_bool " Simple list
          p_comp           TYPE abap_bool " Enhanced list

          sel_function     TYPE ucon_func_list_range "ranges tables
          sel_area         TYPE tt_sel_area
          sel_devclass     TYPE tt_sel_devclass
          sel_dlvunit      TYPE tt_sel_dlvunit
          switch           TYPE abap_bool
          blocklist_server TYPE abap_bool

          sel_mandt        TYPE tt_sel_mandt
          sel_bname        TYPE tt_sel_bname
          sel_ustyp        TYPE tt_sel_ustyp
          sel_class        TYPE tt_sel_class

          p_mon            TYPE abap_bool " Called Function Modules
          p_nmon           TYPE abap_bool " Uncalled Function Modules
          p_all            TYPE abap_bool " All Function Modules
          p_mona           TYPE abap_bool " Called Function Groups
          p_nmona          TYPE abap_bool " Uncalled Function Groups
          p_mond           TYPE abap_bool " Called Packages
          p_nmond          TYPE abap_bool " Uncalled Packages
          p_mons           TYPE abap_bool " Called Software Components
          p_nmons          TYPE abap_bool " Uncalled Software Components

          p_assi           TYPE abap_bool " RFMs Assigned to Default CA
          p_unas           TYPE abap_bool " Unassigned RFMs
          p_both           TYPE abap_bool " Assigned and Unassigned RFMs
          p_assi_s         TYPE abap_bool " Assigned to SNC CA

          p_log            TYPE abap_bool " RFMs in Logging Phase
          p_eval           TYPE abap_bool " RFMs in Evaluation Phase
          p_act            TYPE abap_bool " RFMs in Final Phase
          p_log_e          TYPE abap_bool " Expired RFMs in Logg. Phase
          p_eval_e         TYPE abap_bool " Expired RFMs in Eval. Phase
          p_all_p          TYPE abap_bool " All Phases
        .

  PROTECTED SECTION. " used by class lcl_alv, too

    " Collect user data
    TYPES:
      BEGIN OF ts_user,
        mandt       TYPE usr02-mandt,
        bname       TYPE usr02-bname,
        ustyp       TYPE usr02-ustyp,
        class       TYPE usr02-class,
        auth_values TYPE STANDARD TABLE OF usvalues WITH DEFAULT KEY,
      END OF ts_user,
      tt_user TYPE SORTED TABLE OF ts_user WITH UNIQUE KEY mandt bname.

    CLASS-DATA:
      lt_data TYPE tt_ucon_phase_tool_fields_ext,
      lt_user TYPE tt_user.

  PRIVATE SECTION.

    CONSTANTS:

      usr_parid            TYPE usr05-parid VALUE 'ZSHOW_UCON_RFC_DATA',

      c_v_rfcbl_server(30) VALUE 'V_RFCBL_SERVER'. "View V_RFCBL_SERVER exists as of 7.50

    CLASS-DATA:

      sysid           TYPE sy-sysid,
      installation_nr TYPE uconinstnr,

      auth_change     TYPE abap_bool.

    CLASS-METHODS:

      authority_check,

      " Extend the UCON date with additional information
      extend_data
        IMPORTING
          lt_called_rfm_list TYPE ucon_phase_tool_fields_tt
          sel_function       TYPE ucon_func_list_range
          sel_area           TYPE tt_sel_area
          sel_devclass       TYPE tt_sel_devclass
          sel_dlvunit        TYPE tt_sel_dlvunit
          switch             TYPE abap_bool
          blocklist_server   TYPE abap_bool
          sel_mandt          TYPE tt_sel_mandt  OPTIONAL
          sel_bname          TYPE tt_sel_bname  OPTIONAL
          sel_ustyp          TYPE tt_sel_ustyp  OPTIONAL
          sel_class          TYPE tt_sel_class  OPTIONAL
        ,

      " Show progress indicator
      sapgui_progress_indicator
        IMPORTING
          tabix TYPE i
          total TYPE i
          text  TYPE string
        .

ENDCLASS.                    "lcl_report DEFINITION

*---------------------------------------------------------------------*
*      CLASS lcl_alv DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM lcl_report.

  PUBLIC SECTION.

    CLASS-METHODS:

      show_result
        IMPORTING
          auth_change TYPE abap_bool
          ext_list    TYPE abap_bool, " Enhanced list

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

  PRIVATE SECTION.

    CLASS-DATA:

      r_alv_table   TYPE REF TO cl_salv_table,
      lr_alv_events TYPE REF TO lcl_alv.

    CLASS-METHODS:

      set_phase
        IMPORTING
          seleced_rows           TYPE salv_t_row
          salv_function          TYPE salv_de_function
        RETURNING
          VALUE(changed_entries) TYPE i,

      set_ca
        IMPORTING
          seleced_rows           TYPE salv_t_row
          salv_function          TYPE salv_de_function
        RETURNING
          VALUE(changed_entries) TYPE i,

      show_function               IMPORTING funcname TYPE tfdir-funcname,
      show_function_group         IMPORTING area     TYPE enlfdir-area,
      show_package                IMPORTING devclass TYPE tadir-devclass,
      show_communication_assembly IMPORTING id       TYPE uconservid,

      show_user                   IMPORTING user     TYPE usr02-bname, " Only in current client

      show_authorizations
        IMPORTING
          client         TYPE usr02-mandt
          user           TYPE usr02-bname
          authorizations TYPE string.

ENDCLASS.                    "lcl_alv DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    authority_check( ).

    sy-title = 'Show extended UCON RFC data'(tit).

    DATA functxt TYPE smp_dyntxt.

    functxt-icon_id   = icon_wd_application.
    functxt-quickinfo = 'UCON Cockpit'.
    functxt-icon_text = 'UCON'.
    sscrfields-functxt_01 = functxt.

    tit_sys  = 'Use Statistics from System'.
    t_system = 'System'.

    tit_col  = 'Displayed Fields'.
    t_simp   = 'Standard view'.
    t_comp   = 'Enhanced view incl. called users'.

    tit_fum  = 'Remote Function Modules (RFMs)'.
    t_fumo   = 'Selection Range for RFMs'.
    t_area   = 'Function group'.
    t_devc   = 'Package'.
    t_unit   = 'Software component'.
    t_switch = 'Only functions of switched components'.
    t_bl_srv = 'Only functions of blocklist V_RFC_BL_SERVER'.

    tit_usr  = 'Called Users'.
    t_mandt  = 'Client'.
    t_bname  = 'User'.
    t_ustyp  = 'User type'.
    t_class  = 'User group'.

    tit_mde  = 'Filter Criteria Based on Call Statistics'.
    t_mon    = 'Called Function Modules'.
    t_nmon   = 'Uncalled Function Modules'.
    t_all    = 'All Function Modules'.
    t_mona   = 'Called Function Groups'.
    t_nmona  = 'Uncalled Function Groups'.
    t_mond   = 'Called Packages'.
    t_nmond  = 'Uncalled Packages'.
    t_mons   = 'Called Software Components'.
    t_nmons  = 'Uncalled Software Components'.

    tit_asg  = 'CA Assignment'.
    t_assi   = 'RFMs Assigned to Default CA'.
    t_unas   = 'Unassigned RFMs'.
    t_both   = 'Assigned and Unassigned RFMs'.
    t_assi_s = 'RFMs Assigned to SNC CA'.

    tit_chc  = 'Filter by Phase'.
    t_log    = 'RFMs in Logging Phase'.
    t_eval   = 'RFMs in Evaluation Phase'.
    t_act    = 'RFMs in Final Phase'.
    t_log_e  = 'Expired RFMs in Logg. Phase'.
    t_eval_e = 'Expired RFMs in Eval. Phase'.
    t_all_p  = 'All Phases'.

    t_layout = 'Layout'.

    CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
       SEPARATED BY space.


    " Get (hidden) user parameter
    DATA par_value TYPE ts_para.
    GET PARAMETER ID usr_parid FIELD par_value.

    CASE par_value-list.
      WHEN 'SIMP'.   p_simp   = 'X'. " Simple list
      WHEN 'COMP'.   p_comp   = 'X'. " Enhanced list
    ENDCASE.

    p_switch = par_value-switch.
    p_bl_srv = par_value-block_srv.

    CASE par_value-call_status.
      WHEN 'MON'.    p_mon    = 'X'. " Called Function Modules
      WHEN 'NMON'.   p_nmon   = 'X'. " Uncalled Function Modules
      WHEN 'ALL'.    p_all    = 'X'. " All Function Modules
      WHEN 'MONA'.   p_mona   = 'X'. " Called Function Groups
      WHEN 'NMONA'.  p_nmona  = 'X'. " Uncalled Function Groups
      WHEN 'MOND'.   p_mond   = 'X'. " Called Packages
      WHEN 'NMOND'.  p_nmond  = 'X'. " Uncalled Packages
      WHEN 'MONS'.   p_mons   = 'X'. " Called Software Components
      WHEN 'NMONS'.  p_nmons  = 'X'. " Uncalled Software Components
    ENDCASE.

    CASE par_value-communication_assembly.
      WHEN 'ASSI'.   p_assi   = 'X'. " RFMs Assigned to Default CA
      WHEN 'UNAS'.   p_unas   = 'X'. " Unassigned RFMs
      WHEN 'BOTH'.   p_both   = 'X'. " Assigned and Unassigned RFMs
      WHEN 'ASSI_S'. p_assi_s = 'X'. " Assigned to SNC CA
    ENDCASE.

    CASE par_value-phase.
      WHEN 'LOG'.    p_log    = 'X'. " RFMs in Logging Phase
      WHEN 'EVAL'.   p_eval   = 'X'. " RFMs in Evaluation Phase
      WHEN 'ACT'.    p_act    = 'X'. " RFMs in Final Phase
      WHEN 'LOG_E'.  p_log_e  = 'X'. " Expired RFMs in Logg. Phase
      WHEN 'EVAL_E'. p_eval_e = 'X'. " Expired RFMs in Eval. Phase
      WHEN 'ALL_P'.  p_all_p  = 'X'. " All Phases
    ENDCASE.

    layout = par_value-layout. " Let's use the global variable here.


    "Fill drop down box for field SYSTEM
    DATA:
      lt_system       TYPE vrm_values,
      ls_system       TYPE vrm_value,
      sysid           TYPE sy-sysid,
      installation_nr TYPE uconinstnr.
    CLEAR lt_system[].
    SELECT DISTINCT
        called_sid
        called_installation_nr
      FROM uconrfmcalleratt
      INTO (sysid, installation_nr).
      CONCATENATE sysid '-' installation_nr INTO ls_system-key.
      ls_system-text = ls_system-key.
      APPEND ls_system TO lt_system.
    ENDSELECT.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'P_SYSTEM'
        values = lt_system.
    "set default value for system
    CALL FUNCTION 'SLIC_GET_LICENCE_NUMBER'
      IMPORTING
        license_number = installation_nr.
    CONCATENATE sy-sysid '-' installation_nr INTO p_system.

  ENDMETHOD. " initialization

  METHOD authority_check.

    " Authority check for display
    " Class cl_ucon_api_auth_check method if_ucon_api_auth_check~check_auth does not exist in 7.40
    AUTHORITY-CHECK OBJECT 'S_UCON_ADM'
       ID 'UCON_TYPE' DUMMY " Field is not used yet
       ID 'UCON_NAME' DUMMY " Field is not used yet
       ID 'ACTVT' FIELD '03'.
    IF sy-subrc <> 0.
      LOOP AT SCREEN.
        screen-active = 0.
        MODIFY SCREEN.
      ENDLOOP.
      MESSAGE e219(s_ucon_lm). " Missing authorization to start Unified Connectivity Tools.
      LEAVE TO SCREEN 0.
    ENDIF.

    " Authority check for change
    AUTHORITY-CHECK OBJECT 'S_UCON_ADM'
       ID 'UCON_TYPE' DUMMY " Field is not used yet
       ID 'UCON_NAME' DUMMY " Field is not used yet
       ID 'ACTVT' FIELD '02'.
    IF sy-subrc = 0.
      auth_change = abap_true.
    ENDIF.

  ENDMETHOD. " authority_check

  METHOD at_selection_screen_output.
    " Blocklist packages exist as of SAP_BAIS 7.50
    IF sy-saprl < 750.
      LOOP AT SCREEN INTO DATA(ls_screen).
        IF ls_screen-group1 = 'BLK'.
          ls_screen-active = '0'.
        ENDIF.
        MODIFY SCREEN FROM ls_screen.
      ENDLOOP.
    ENDIF.

    " Hide client and user fields in case of the simple output list
    LOOP AT SCREEN INTO ls_screen.
      IF ls_screen-group1 = 'USR'.
        IF p_comp IS INITIAL.
          ls_screen-active = '0'.
        ELSE.
          ls_screen-active = '1'.
        ENDIF.
        MODIFY SCREEN FROM ls_screen.
      ENDIF.
    ENDLOOP.

  ENDMETHOD. " at_selection_screen_output

  METHOD at_selection_screen_f4_fumo.

    TYPES:
      BEGIN OF ts_f4_function,
        funcname     TYPE uconrfcstatehead-funcname,
        actual_phase TYPE uconrfcstatehead-actual_phase,
      END OF ts_f4_function,
      tt_f4_function TYPE STANDARD TABLE OF ts_f4_function.

    STATICS:
      "f4_value     TYPE ts_f4_function,
      f4_value_tab TYPE tt_f4_function.

    IF f4_value_tab IS INITIAL.
      SELECT DISTINCT
          funcname,
          actual_phase
        FROM uconrfcstatehead
        ORDER BY
          funcname
        INTO TABLE @f4_value_tab.
    ENDIF.

    DATA(progname) = sy-repid.
    DATA(dynnum)   = sy-dynnr.
    DATA field TYPE dynfnam.
    DATA stepl TYPE sy-stepl.
    GET CURSOR FIELD field LINE stepl.
    DATA return_tab TYPE TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'FUNCNAME'
        dynpprog        = progname
        dynpnr          = dynnum
        dynprofield     = field
        stepl           = stepl
        value_org       = 'S'
      TABLES
*       field_tab       = field_tab
        value_tab       = f4_value_tab
        "return_tab      = return_tab " surprisingly required to get lower case values
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.

  ENDMETHOD. " at_selection_screen_f4_fumo

  METHOD at_selection_screen_f4_mandt.

    TYPES:
      BEGIN OF ts_f4_mandt,
        mandt TYPE uconrfmcalleratt-called_client,
      END OF ts_f4_mandt,
      tt_f4_mandt TYPE STANDARD TABLE OF ts_f4_mandt.

    STATICS:
      "f4_value     TYPE ts_f4_mandtd,
      f4_value_tab TYPE tt_f4_mandt.

    IF f4_value_tab IS INITIAL.
      SELECT DISTINCT
          called_client
        FROM uconrfmcalleratt
        ORDER BY
          called_client
        INTO TABLE @f4_value_tab.
    ENDIF.

    DATA(progname) = sy-repid.
    DATA(dynnum)   = sy-dynnr.
    DATA field TYPE dynfnam.
    DATA stepl TYPE sy-stepl.
    GET CURSOR FIELD field LINE stepl.
    DATA return_tab TYPE TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CALLED_MANDT'
        dynpprog        = progname
        dynpnr          = dynnum
        dynprofield     = field
        stepl           = stepl
        value_org       = 'S'
      TABLES
*       field_tab       = field_tab
        value_tab       = f4_value_tab
        "return_tab      = return_tab " surprisingly required to get lower case values
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.

  ENDMETHOD. " at_selection_screen_f4_mandt

  METHOD at_selection_screen_f4_bname.

    TYPES:
      BEGIN OF ts_f4_bname,
        mandt TYPE uconrfmcalleratt-called_client,
        bname TYPE uconrfmcalleratt-called_user,
      END OF ts_f4_bname,
      tt_f4_bname TYPE STANDARD TABLE OF ts_f4_bname.

    STATICS:
      "f4_value     TYPE ts_f4_bname,
      f4_value_tab TYPE tt_f4_bname.

    IF f4_value_tab IS INITIAL.
      SELECT DISTINCT
          called_client,
          called_user
        FROM uconrfmcalleratt
        ORDER BY
          called_client,
          called_user
        INTO TABLE @f4_value_tab.
    ENDIF.

    DATA(progname) = sy-repid.
    DATA(dynnum)   = sy-dynnr.
    DATA field TYPE dynfnam.
    DATA stepl TYPE sy-stepl.
    GET CURSOR FIELD field LINE stepl.
    DATA return_tab TYPE TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CALLED_USER'
        dynpprog        = progname
        dynpnr          = dynnum
        dynprofield     = field
        stepl           = stepl
        value_org       = 'S'
      TABLES
*       field_tab       = field_tab
        value_tab       = f4_value_tab
        "return_tab      = return_tab " surprisingly required to get lower case values
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.

  ENDMETHOD. " at_selection_screen_f4_bname

  METHOD at_selection_screen_f4_class.

    TYPES:
      BEGIN OF ts_f4_class,
        mandt TYPE usr02-mandt,
        class TYPE usr02-class,
      END OF ts_f4_class,
      tt_f4_class TYPE STANDARD TABLE OF ts_f4_class.

    STATICS:
      "f4_value     TYPE ts_f4_class,
      f4_value_tab TYPE tt_f4_class.

    IF f4_value_tab IS INITIAL.
      "SELECT DISTINCT
      "    mandt,
      "    class
      "  FROM usr02 CLIENT SPECIFIED
      "  ORDER BY
      "    mandt,
      "    class
      "  INTO TABLE @f4_value_tab.
      SELECT DISTINCT
          uconrfmcalleratt~called_client,
          usr02~class
        FROM uconrfmcalleratt
        JOIN usr02
          ON    usr02~mandt = uconrfmcalleratt~called_client
            AND usr02~bname = uconrfmcalleratt~called_user
            "AND usr02~class IS NOT INITIAL
        CLIENT SPECIFIED
        ORDER BY
          uconrfmcalleratt~called_client,
          usr02~class
        INTO TABLE @f4_value_tab.
    ENDIF.

    DATA(progname) = sy-repid.
    DATA(dynnum)   = sy-dynnr.
    DATA field TYPE dynfnam.
    DATA stepl TYPE sy-stepl.
    GET CURSOR FIELD field LINE stepl.
    DATA return_tab TYPE TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CALLED_USER'
        dynpprog        = progname
        dynpnr          = dynnum
        dynprofield     = field
        stepl           = stepl
        value_org       = 'S'
      TABLES
*       field_tab       = field_tab
        value_tab       = f4_value_tab
        "return_tab      = return_tab " surprisingly required to get lower case values
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.

  ENDMETHOD. " at_selection_screen_f4_bname

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

  METHOD at_selection_screen.

    CASE sscrfields-ucomm.

      WHEN 'FC01'.
        " UCON Cockpit
        TRY.
            CALL TRANSACTION 'UCONCOCKPIT' WITH AUTHORITY-CHECK.
          CATCH cx_sy_authorization_error.
            MESSAGE e219(s_ucon_lm). " Missing authorization to start Unified Connectivity Tools.
        ENDTRY.

      WHEN 'COLS'.
        " see AT SELECTION SCREEN OUTPUT

    ENDCASE.

  ENDMETHOD. " at_selection_screen

  METHOD start_of_selection.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = 'Get data'.

    " Store (hidden) user parameter
    DATA par_value TYPE ts_para.
    IF p_simp   = 'X'. par_value-list = 'SIMP'. ENDIF. " Simple list
    IF p_comp   = 'X'. par_value-list = 'COMP'. ENDIF. " Enhanced list

    par_value-switch     = p_switch.
    par_value-block_srv  = p_bl_srv.

    IF p_mon    = 'X'. par_value-call_status = 'MON'. ENDIF. " Called Function Modules
    IF p_nmon   = 'X'. par_value-call_status = 'NMON'. ENDIF. " Uncalled Function Modules
    IF p_all    = 'X'. par_value-call_status = 'ALL'. ENDIF. " All Function Modules
    IF p_mona   = 'X'. par_value-call_status = 'MONA'. ENDIF. " Called Function Groups
    IF p_nmona  = 'X'. par_value-call_status = 'NMONA'. ENDIF. " Uncalled Function Groups
    IF p_mond   = 'X'. par_value-call_status = 'MOND'. ENDIF. " Called Packages
    IF p_nmond  = 'X'. par_value-call_status = 'NMOND'. ENDIF. " Uncalled Packages
    IF p_mons   = 'X'. par_value-call_status = 'MONS'. ENDIF. " Called Software Components
    IF p_nmons  = 'X'. par_value-call_status = 'NMONS'. ENDIF. " Uncalled Software Components

    IF p_assi   = 'X'. par_value-communication_assembly = 'ASSI'.   ENDIF. " RFMs Assigned to Default CA
    IF p_unas   = 'X'. par_value-communication_assembly = 'UNAS'.   ENDIF. " Unassigned RFMs
    IF p_both   = 'X'. par_value-communication_assembly = 'BOTH'.   ENDIF. " Assigned and Unassigned RFMs
    IF p_assi_s = 'X'. par_value-communication_assembly = 'ASSI_S'. ENDIF. " Assigned to SNC CA

    IF p_log    = 'X'. par_value-phase = 'LOG'.    ENDIF. " RFMs in Logging Phase
    IF p_eval   = 'X'. par_value-phase = 'EVAL'.   ENDIF. " RFMs in Evaluation Phase
    IF p_act    = 'X'. par_value-phase = 'ACT'.    ENDIF. " RFMs in Final Phase
    IF p_log_e  = 'X'. par_value-phase = 'LOG_E'.  ENDIF. " Expired RFMs in Logg. Phase
    IF p_eval_e = 'X'. par_value-phase = 'EVAL_E'. ENDIF. " Expired RFMs in Eval. Phase
    IF p_all_p  = 'X'. par_value-phase = 'ALL_P'.  ENDIF. " All Phases

    par_value-layout = layout.

    " Set user parameter for current session
    SET PARAMETER ID usr_parid FIELD par_value.
    " Set user parameter permamently
    "MODIFY usr05 FROM @(
    "  VALUE #(
    "    bname = sy-uname
    "    parid = usr_parid
    "    parva = par_value )
    "  ).
    DATA:
      lt_usr05     TYPE suid_tt_usr05,
      ls_timestamp TYPE cl_identity=>ty_timestamp.
    lt_usr05 = VALUE #( (
      bname = sy-uname
      parid = usr_parid
      parva = par_value
    ) ).
    GET TIME STAMP FIELD ls_timestamp-timestamp.
    CALL FUNCTION 'SUID_IDENTITY_SAVE_TO_DB'
      EXPORTING
        it_usr05_update = lt_usr05
        is_timestamp    = ls_timestamp.

    " Own system and installation number
    sysid = sy-sysid.
    CALL FUNCTION 'SLIC_GET_LICENCE_NUMBER'
      IMPORTING
        license_number = installation_nr.

    " Selected system and installation number
    sysid = p_system(3).
    installation_nr = p_system+4.
    CHECK sysid IS NOT INITIAL AND installation_nr IS NOT INITIAL.

    " Prepare selection parameters

    DATA:
      cols               TYPE if_ucon_phase_store_utility=>tt_columns,

      call_mode          TYPE c,
      " 'C'  p_mon       Called Function Modules
      " 'U'  p_nmon      Uncalled Function Modules
      " space            all

      assign_mode        TYPE c,
      " 'A'  p_assi      RFMs Assigned to Default CA
      " 'U'  p_unas      Unassigned RFMs
      " 'B'
      " space            all
      " or
      " 'S'  p_assi_s    SNC CA

      phase_mode         TYPE ucon_filter_value,
      " 'LG' p_log       RFMs in Logging Phase
      " 'LE' p_log_e     Expired RFMs in Logg. Phase
      " 'EV' p_eval      RFMs in Evaluation Phase
      " 'EE' p_eval_e    Expired RFMs in Evaluation Phase
      " 'AC' p_act       RFMs in Final Phase
      " space            all

      aggregate_vh       TYPE sap_bool, "OPTIONAL
      snc_only           TYPE sap_bool, "OPTIONAL
      lt_called_rfm_list TYPE ucon_phase_tool_fields_tt,
      result_limit       TYPE abap_bool.

    REFRESH cols.
    IF p_comp = abap_false.
      " Standard view
      cl_ucon_phase_store_utility=>if_ucon_phase_store_utility~get_columns_simple_select(
        IMPORTING
         e_columns = cols ).
    ELSEIF p_comp = abap_true.
      " Enhanced view
      cl_ucon_phase_store_utility=>if_ucon_phase_store_utility~get_columns_complex_select(
        IMPORTING
          e_columns = cols ).
      "INSERT `UCONRFMCALLERATT~CALLER_USER` INTO TABLE cols. "not possible because of whitelist check
    ENDIF.
    " Remove unimportant fields
    LOOP AT cols INTO DATA(column).
      CASE column.
        WHEN 'UCONRFCSTATEHEAD~SECURE_BY_DEFAULT'
          OR 'UCONRFMCALLERATT~CALLED_VH'.
          DELETE cols.
      ENDCASE.
    ENDLOOP.

    " Called / uncalled / all
    IF p_mon = abap_true.       " Called functions
      call_mode = 'C'.
    ELSEIF p_nmon = abap_true.  " Not called functions
      call_mode = 'U'.
    ELSE.
      CLEAR call_mode.
    ENDIF.

    " Assigned to default CA / assigned to SNC CA / not assigned / all
    IF p_assi = abap_true.
      assign_mode = 'A'.
    ELSEIF p_assi_s = abap_true.
      assign_mode = 'S'.
    ELSEIF p_unas = abap_true.
      assign_mode = 'U'.
    ELSE.
      CLEAR assign_mode.
    ENDIF.

    " Phase logging / logging expired / evaluation / evaluation expired / final
    IF p_log IS NOT INITIAL.
      phase_mode = 'LG'.
    ENDIF.
    IF p_eval IS NOT INITIAL.
      phase_mode = 'EV'.
    ENDIF.
    IF p_act IS NOT INITIAL.
      phase_mode = 'AC'.
    ENDIF.
    IF p_log_e IS NOT INITIAL.
      phase_mode = 'LE'.
    ENDIF.
    IF p_eval_e IS NOT INITIAL.
      phase_mode = 'EE'.
    ENDIF.
*  IF p_eval_d IS NOT INITIAL.
*    phase_mode = 'ED'.
*  ENDIF.
*    IF p_assi IS NOT INITIAL and lv_filter is not initial.
*      phase_mode = 'AS'.
*    ENDIF.
*    IF p_unas IS NOT INITIAL and lv_filter is not initial.
*      phase_mode = 'US'.
*    ENDIF.
    IF p_all_p IS NOT INITIAL.
      CLEAR phase_mode.
    ENDIF.

    " Optimize selection for function groups respective package (does not work as the SQL statement become too big.)
    "if sel_function is initial and sel_area is not initial.
    "  data sel_func type line of ucon_func_list_range.
    "  sel_func-option = 'EQ'.
    "  sel_func-sign   = 'I'.
    "  select tfdir~FUNCNAME
    "    from ENLFDIR
    "    join tfdir
    "      on tfdir~funcname = ENLFDIR~funcname
    "      and tfdir~fmode is not initial
    "    into @sel_func-low.
    "    append sel_func to sel_function.
    "  endselect.
    "endif.

    cl_ucon_phase_tool=>get_data(
      EXPORTING
        i_funclist             = sel_function
        i_columns              = cols
        i_call_mode            = call_mode       " 'C' = called,   'U' = uncalled,   space = all
        i_assign_mode          = assign_mode     " 'A' = assigned, 'U' = unassigned, space = all
        i_phase_mode           = phase_mode
        i_sysid                = sysid
        i_installation_nr      = installation_nr
        "i_aggregate_vh         = aggregate_vh   " only in simple mode
        "i_snc_only             = snc_only       " only in simple mode
      IMPORTING
        e_called_rfm_list      = lt_called_rfm_list
        e_result_limit_reached = result_limit ).

    IF result_limit = abap_true.
      CALL FUNCTION 'POPUP_DISPLAY_TEXT'
        EXPORTING
          "LANGUAGE     = SY-LANGU
          popup_title  = 'Search Result incomplete'(inc)
          start_column = 10
          start_row    = 3
          text_object  = 'UCONPHTL_INCOMPL_RESULT'
        EXCEPTIONS
          OTHERS       = 1.
      .
    ENDIF.

    " Extend the result table
    IF p_simp = 'X'.
      extend_data(
        EXPORTING
          lt_called_rfm_list = lt_called_rfm_list
          sel_function = sel_function
          sel_area     = sel_area
          sel_devclass = sel_devclass
          sel_dlvunit  = sel_dlvunit
          switch       = switch
          blocklist_server = blocklist_server
          "sel_mandt    = sel_mandt
          "sel_bname    = sel_bname
          "sel_ustyp    = sel_ustyp
          "sel_class    = sel_class
        ).
    ELSE.
      extend_data(
        EXPORTING
          lt_called_rfm_list = lt_called_rfm_list
          sel_function = sel_function
          sel_area     = sel_area
          sel_devclass = sel_devclass
          sel_dlvunit  = sel_dlvunit
          switch       = switch
          blocklist_server = blocklist_server
          sel_mandt    = sel_mandt
          sel_bname    = sel_bname
          sel_ustyp    = sel_ustyp
          sel_class    = sel_class
        ).
    ENDIF.
    FREE lt_called_rfm_list.

    " Show result
    lcl_alv=>show_result(
      EXPORTING
        auth_change = auth_change
        ext_list    = p_comp
    ).

  ENDMETHOD. " start_of_selection

  METHOD extend_data.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 20
        text       = 'Load additional data'.

    " Add marker if function group, package or software component has some called functions
    TYPES:
      BEGIN OF ts_fugr,
        area TYPE enlfdir-area,
      END OF ts_fugr,
      tt_fugr TYPE SORTED TABLE OF ts_fugr WITH UNIQUE KEY area,

      BEGIN OF ts_devclass,
        devclass TYPE tdevc-devclass,
      END OF ts_devclass,
      tt_devclass TYPE SORTED TABLE OF ts_devclass WITH UNIQUE KEY devclass,

      BEGIN OF ts_dlvunit,
        dlvunit TYPE tdevc-dlvunit,
      END OF ts_dlvunit,
      tt_dlvunit TYPE SORTED TABLE OF ts_dlvunit WITH UNIQUE KEY dlvunit.

    DATA:
      lt_fugr     TYPE tt_fugr,
      lt_devclass TYPE tt_devclass,
      lt_dlvunit  TYPE tt_dlvunit.

    " Get called function groups
    SELECT DISTINCT
        enlfdir~area   " Function group
      FROM tfdir                       " Function
      JOIN enlfdir                     " Function group
        ON enlfdir~funcname = tfdir~funcname
      WHERE tfdir~fmode NE ' '         " RFC mode
        AND EXISTS ( SELECT * FROM uconrfmcalleratt
                       WHERE uconrfmcalleratt~called_rfm = tfdir~funcname
                         AND uconrfmcalleratt~called_sid = @sysid
                         AND uconrfmcalleratt~called_installation_nr = @installation_nr )
      ORDER BY
        enlfdir~area
      INTO TABLE @lt_fugr.

    " Get called packages
    SELECT DISTINCT
        tadir~devclass   " Package
      FROM tfdir                       " Function
      JOIN enlfdir                     " Function group
        ON enlfdir~funcname = tfdir~funcname
      JOIN tadir                       " Package
        ON    tadir~pgmid    = 'R3TR'
          AND tadir~object   = 'FUGR'
          AND tadir~obj_name = enlfdir~area
      WHERE tfdir~fmode NE ' '         " RFC mode
        AND EXISTS ( SELECT * FROM uconrfmcalleratt
                       WHERE uconrfmcalleratt~called_rfm = tfdir~funcname
                         AND uconrfmcalleratt~called_sid = @sysid
                         AND uconrfmcalleratt~called_installation_nr = @installation_nr )
      ORDER BY
        tadir~devclass
      INTO TABLE @lt_devclass.

    " Get called software components
    SELECT DISTINCT
        tdevc~dlvunit                  " Software component
      FROM tfdir                       " Function
      JOIN enlfdir                     " Function group
        ON enlfdir~funcname = tfdir~funcname
      JOIN tadir                       " Package
        ON    tadir~pgmid    = 'R3TR'
          AND tadir~object   = 'FUGR'
          AND tadir~obj_name = enlfdir~area
      JOIN tdevc                       " Software component
        ON tdevc~devclass = tadir~devclass
      WHERE tfdir~fmode NE ' '         " RFC mode
        AND EXISTS ( SELECT * FROM uconrfmcalleratt
                       WHERE uconrfmcalleratt~called_rfm = tfdir~funcname
                         AND uconrfmcalleratt~called_sid = @sysid
                         AND uconrfmcalleratt~called_installation_nr = @installation_nr )
      ORDER BY
        tdevc~dlvunit
      INTO TABLE @lt_dlvunit.


    " Get active switches
    cl_abap_switch=>get_enabled_switches( IMPORTING p_enabled_switches = DATA(enabled_switches) ).


    " Add Blacklist for blocked Function Modules
    TYPES:
      BEGIN OF ts_rfcbl_server,
        rfm_name  TYPE funcname,
        blpackage TYPE devclass,
      END OF ts_rfcbl_server.
    DATA lt_rfcbl_server TYPE SORTED TABLE OF ts_rfcbl_server WITH UNIQUE KEY rfm_name blpackage.
    IF sy-saprl >= 750.
      SELECT DISTINCT
          rfm_name,
          blpackage
        FROM (c_v_rfcbl_server)      " Blacklist for blocked Function Modules
        WHERE rfm_name IN @sel_function
          AND version = 'A'
        INTO TABLE @lt_rfcbl_server.
    ENDIF.


    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 30
        text       = 'Load additional data'.

    " Collect function data
    TYPES:
      BEGIN OF ts_function,
        funcname      TYPE tfdir-funcname,
        fmode         TYPE tfdir-fmode,
        func_text     TYPE tftit-stext,
        area          TYPE enlfdir-area,
        area_text     TYPE tlibt-areat,
        devclass      TYPE tadir-devclass,
        devclass_text TYPE tdevct-ctext,
        switch_id     TYPE sfw_package-switch_id,
        switch_name   TYPE sfw_switcht-name32,
        switch_state  TYPE sfw_switchpos,
        dlvunit       TYPE tdevc-dlvunit,
        blpackage     TYPE devclass, " Field v_rfcbl_server-blpackage exists as of 7.50
      END OF ts_function,
      tt_function TYPE SORTED TABLE OF ts_function WITH UNIQUE KEY funcname.

    DATA:
      ls_function TYPE ts_function,
      lt_function TYPE tt_function.

    SELECT
        tfdir~funcname,        " Function
        tfdir~fmode,           " RFC mode
        tftit~stext,           " Function text
        enlfdir~area,          " Function group
        tlibt~areat,           " Function group text
        tadir~devclass,        " Package
        tdevct~ctext,          " Package text
        sfw_package~switch_id, " Switch framework ID
        sfw_switcht~name32,    " Switch framework name
        ' ',                   " Switch state (no select possible)
        tdevc~dlvunit,         " Delivery unit
        ' ' " v_rfcbl_server~blpackage    " Blocklist view V_RFC_BL_SERVER exists as of SAP_BASIS 7.50
      FROM tfdir                       " Function
      LEFT OUTER JOIN tftit            " Function text
        ON    tftit~funcname = tfdir~funcname
          AND spras = @sy-langu
      JOIN enlfdir                     " Function group
        ON    enlfdir~funcname = tfdir~funcname
      LEFT OUTER JOIN tlibt            " Function group text
        ON    tlibt~area = enlfdir~area
          AND tlibt~spras = @sy-langu
      JOIN tadir                       " Package
        ON    pgmid    = 'R3TR'
          AND object   = 'FUGR'
          AND obj_name = enlfdir~area
      LEFT OUTER JOIN tdevct           " Package text
        ON    tdevct~devclass = tadir~devclass
          AND tdevct~spras = @sy-langu
      LEFT OUTER JOIN sfw_package      " Switch framework ID
        ON    sfw_package~devclass = tadir~devclass
          AND sfw_package~version  = 'A'
      LEFT OUTER JOIN sfw_switcht      " Switch framework name
        ON    sfw_switcht~switch_id = sfw_package~switch_id
          AND sfw_switcht~spras = @sy-langu
      JOIN tdevc                       " Delivery unit
        ON    tdevc~devclass = tadir~devclass
"          LEFT OUTER JOIN v_rfcbl_server   " Blocklist view V_RFC_BL_SERVER
"            ON    v_rfcbl_server~rfm_name = tfdir~funcname
"              AND v_rfcbl_server~blpackage = 'A'
      WHERE tfdir~funcname IN @sel_function
        AND enlfdir~area   IN @sel_area
        AND tadir~devclass IN @sel_devclass
        AND tdevc~dlvunit  IN @sel_dlvunit
      INTO TABLE @lt_function.

    LOOP AT lt_function ASSIGNING FIELD-SYMBOL(<ls_function>).
      " Add switch id. The simple approach as part of the select statement is not sufficent for a hierarchy of packages
      IF <ls_function>-switch_id IS INITIAL AND <ls_function>-devclass IS NOT INITIAL.
        " Simple
        <ls_function>-switch_id = cl_switch=>sw_devclass( <ls_function>-devclass ).
        " Additional data about business functions etc.
*            CALL METHOD cl_sfw_helper=>get_switch_by_package
*              EXPORTING
*                package_name              =
*              IMPORTING
*                switch_id                 =
*                business_functions        =
*                ea_functions              =
*                business_function_sets    =
*                package_hierarchy         =
*              EXCEPTIONS
*                package_does_not_exist    = 1
*                package_is_not_switchable = 2
*                internal_error            = 3
*                others                    = 4
*                    .
*            IF sy-subrc <> 0.
*             Implement suitable error handling here
*            ENDIF.

        IF <ls_function>-switch_id IS NOT INITIAL.
          " Get switch text
          SELECT SINGLE name32
            FROM sfw_switcht      " Switch framework name
            WHERE switch_id = @<ls_function>-switch_id
              AND spras     = @sy-langu
            INTO @<ls_function>-switch_name.
        ENDIF.
      ENDIF.

      IF <ls_function>-switch_id IS NOT INITIAL.
        " Get switch state
        READ TABLE enabled_switches INTO DATA(enabled_switch)
          WITH KEY switch_id = <ls_function>-switch_id.
        IF sy-subrc = 0.
          <ls_function>-switch_state = enabled_switch-state. " T on, S stand-by, F off
        ELSE.
          <ls_function>-switch_state = '-'.
        ENDIF.
      ENDIF.

      " Add Blacklist for blocked Function Modules
      IF sy-saprl >= 750.
        " slow
        "SELECT SINGLE blpackage
        "  FROM (c_v_rfcbl_server)      " Blacklist for blocked Function Modules
        "  WHERE rfm_name = @<ls_function>-funcname
        "    AND version = 'A'
        "  INTO @ls_function-blpackage.
        " fast
        READ TABLE lt_rfcbl_server INTO DATA(ls_rfcbl_server)
          WITH KEY rfm_name = <ls_function>-funcname.
        IF sy-subrc = 0.
          <ls_function>-blpackage = ls_rfcbl_server-blpackage.
        ENDIF.
      ENDIF.
    ENDLOOP.


    " ALV Data
    DATA:
      ls_data TYPE ts_ucon_phase_tool_fields_ext.

    " Copy data to ALV data
    LOOP AT lt_called_rfm_list INTO DATA(ls_called_rfm_list)
      WHERE called_client IN sel_mandt
        AND called_user   IN sel_bname.

      sapgui_progress_indicator(
       EXPORTING
         tabix = sy-tabix
         total = lines( lt_called_rfm_list )
         text  = 'Extend data'
      ).

      CLEAR ls_data.
      MOVE-CORRESPONDING ls_called_rfm_list TO ls_data.

      " Get additional function data
      CLEAR ls_function.
      READ TABLE lt_function INTO ls_function WITH KEY funcname = ls_data-funcname.
*      IF sy-subrc NE 0.
*        DATA(function_tabix) = sy-tabix.
*        SELECT SINGLE
*            tfdir~funcname,        " Function
*            tfdir~fmode,           " RFC mode
*            tftit~stext,           " Function text
*            enlfdir~area,          " Function group
*            tlibt~areat,           " Function group text
*            tadir~devclass,        " Package
*            tdevct~ctext,          " Package text
*            sfw_package~switch_id, " Switch framework ID
*            sfw_switcht~name32,    " Switch framework name
*            ' ',                   " Switch state (no select possible)
*            tdevc~dlvunit,         " Delivery unit
*            ' ' " v_rfcbl_server~blpackage    " Blocklist view V_RFC_BL_SERVER exists as of SAP_BASIS 7.50
*          FROM tfdir                       " Function
*          LEFT OUTER JOIN tftit            " Function text
*            ON    tftit~funcname = tfdir~funcname
*              AND spras = @sy-langu
*          JOIN enlfdir                     " Function group
*            ON    enlfdir~funcname = tfdir~funcname
*          LEFT OUTER JOIN tlibt            " Function group text
*            ON    tlibt~area = enlfdir~area
*              AND tlibt~spras = @sy-langu
*          JOIN tadir                       " Package
*            ON    pgmid    = 'R3TR'
*              AND object   = 'FUGR'
*              AND obj_name = enlfdir~area
*          LEFT OUTER JOIN tdevct           " Package text
*            ON    tdevct~devclass = tadir~devclass
*              AND tdevct~spras = @sy-langu
*          LEFT OUTER JOIN sfw_package      " Switch framework ID
*            ON    sfw_package~devclass = tadir~devclass
*              AND sfw_package~version  = 'A'
*          LEFT OUTER JOIN sfw_switcht      " Switch framework name
*            ON    sfw_switcht~switch_id = sfw_package~switch_id
*              AND sfw_switcht~spras = @sy-langu
*          JOIN tdevc                       " Delivery unit
*            ON    tdevc~devclass = tadir~devclass
*"          LEFT OUTER JOIN v_rfcbl_server   " Blocklist view V_RFC_BL_SERVER
*"            ON    v_rfcbl_server~rfm_name = tfdir~funcname
*"              AND v_rfcbl_server~blpackage = 'A'
*          WHERE tfdir~funcname = @ls_data-funcname
*          INTO @ls_function.
*        IF sy-subrc = 0.
*
*          " Add switch id. The simple approach as part of the select statement is not sufficent for a hierarchy of packages
*          IF ls_function-switch_id IS INITIAL AND ls_function-devclass IS NOT INITIAL.
*            " Simple
*            ls_function-switch_id = cl_switch=>sw_devclass( ls_function-devclass ).
*            " Additional data about business functions etc.
**            CALL METHOD cl_sfw_helper=>get_switch_by_package
**              EXPORTING
**                package_name              =
**              IMPORTING
**                switch_id                 =
**                business_functions        =
**                ea_functions              =
**                business_function_sets    =
**                package_hierarchy         =
**              EXCEPTIONS
**                package_does_not_exist    = 1
**                package_is_not_switchable = 2
**                internal_error            = 3
**                others                    = 4
**                    .
**            IF sy-subrc <> 0.
**             Implement suitable error handling here
**            ENDIF.
*
*            IF ls_function-switch_id IS NOT INITIAL.
*              " Get switch text
*              SELECT SINGLE name32
*                FROM sfw_switcht      " Switch framework name
*                WHERE switch_id = @ls_function-switch_id
*                  AND spras     = @sy-langu
*                INTO @ls_function-switch_name.
*            ENDIF.
*          ENDIF.
*
*          IF ls_function-switch_id IS NOT INITIAL.
*            " Get switch state
*            READ TABLE enabled_switches INTO DATA(enabled_switch)
*              WITH KEY switch_id = ls_function-switch_id.
*            IF sy-subrc = 0.
*              ls_function-switch_state = enabled_switch-state. " T on, S stand-by, F off
*            ELSE.
*              ls_function-switch_state = '-'.
*            ENDIF.
*          ENDIF.
*
*          " Add Blacklist for blocked Function Modules
*          IF sy-saprl >= 750.
*            " slow
*            "SELECT SINGLE blpackage
*            "  FROM (c_v_rfcbl_server)      " Blacklist for blocked Function Modules
*            "  WHERE rfm_name = @ls_data-funcname
*            "    AND version = 'A'
*            "  INTO @ls_function-blpackage.
*            " fast
*            READ TABLE lt_rfcbl_server INTO DATA(ls_rfcbl_server)
*              WITH TABLE KEY rfm_name = ls_data-funcname.
*            IF sy-subrc = 0.
*              ls_function-blpackage = ls_rfcbl_server-blpackage.
*            ENDIF.
*          ENDIF.
*
*          INSERT ls_function INTO lt_function INDEX function_tabix.
*        ENDIF.
*      ENDIF.

      " Check select options for function group, package and software component
      CHECK ls_function-area     IN sel_area.
      CHECK ls_function-devclass IN sel_devclass.
      CHECK ls_function-dlvunit  IN sel_dlvunit.

      " Select functions of switched components only
      CHECK switch IS INITIAL OR ls_function-switch_id IS NOT INITIAL.

      " Select functions of the blocklist view V_RFC_BL_SERVER only
      CHECK blocklist_server IS INITIAL OR ls_function-blpackage IS NOT INITIAL.

      ls_data-fmode         = ls_function-fmode.
      ls_data-func_text     = ls_function-func_text.

      ls_data-area          = ls_function-area.
      ls_data-area_text     = ls_function-area_text.

      ls_data-devclass      = ls_function-devclass.
      ls_data-devclass_text = ls_function-devclass_text.

      ls_data-switch_id     = ls_function-switch_id.
      ls_data-switch_name   = ls_function-switch_name.
      CASE ls_function-switch_state.
        WHEN 'T'.    ls_data-switch_state = 'active'.
        WHEN 'S'.
          ls_data-switch_state = 'stand-by'.
          APPEND VALUE #( fname = 'SWITCH_STATE' color-col = col_total )    TO ls_data-ctab.
        WHEN 'F'.
          ls_data-switch_state = 'off'.
          APPEND VALUE #( fname = 'SWITCH_STATE' color-col = col_negative ) TO ls_data-ctab.
        WHEN '-'.
          ls_data-switch_state = 'Off'.
          APPEND VALUE #( fname = 'SWITCH_STATE' color-col = col_negative ) TO ls_data-ctab.
        WHEN OTHERS. ls_function-switch_state = ls_function-switch_state.
      ENDCASE.

      ls_data-dlvunit       = ls_function-dlvunit.

      ls_data-blpackage     = ls_function-blpackage.

      " Add marker if function is called
      IF ls_data-counter > 0.
        ls_data-func_called = 'X'. "sym_filled_circle. "'called'.
      ELSE.
        ls_data-func_called = ' '. "sym_circle.
      ENDIF.

      " Add marker if function group has some called functions
      READ TABLE lt_fugr WITH TABLE KEY area = ls_data-area TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_data-area_called = 'X'. "sym_filled_circle. "'called'.
      ELSE.
        ls_data-area_called = ' '. "sym_circle.
      ENDIF.

      " Add marker if package has some called functions
      READ TABLE lt_devclass WITH TABLE KEY devclass = ls_data-devclass TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_data-devclass_called = 'X'. "sym_filled_circle. "'called'.
      ELSE.
        ls_data-devclass_called = ' '. "sym_circle.
      ENDIF.

      " Add marker if software component has some called functions
      READ TABLE lt_dlvunit WITH TABLE KEY dlvunit = ls_data-dlvunit TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_data-dlvunit_called = 'X'. "sym_filled_circle. "'called'.
      ELSE.
        ls_data-dlvunit_called = ' '. "sym_circle.
      ENDIF.

      IF p_mon = 'X'. " Called functions
        CHECK ls_data-func_called = 'X'.
      ENDIF.
      IF p_nmon = 'X'. " Not called functions
        CHECK ls_data-func_called = ' '.
      ENDIF.
      IF p_mona = 'X'. " Called function groups
        CHECK ls_data-area_called = 'X'.
      ENDIF.
      IF p_nmona = 'X'. " Not called function groups
        CHECK ls_data-area_called = ' '.
      ENDIF.
      IF p_mond = 'X'. " Called packages
        CHECK ls_data-devclass_called = 'X'.
      ENDIF.
      IF p_nmond = 'X'. " Not called packages
        CHECK ls_data-devclass_called = ' '.
      ENDIF.
      IF p_mons = 'X'. " Called software components
        CHECK ls_data-dlvunit_called = 'X'.
      ENDIF.
      IF p_nmons = 'X'. " Not called software components
        CHECK ls_data-dlvunit_called = ' '.
      ENDIF.

      " Set color of function
      " Variant A (this is currently used):
      "   blocked -> red
      "   candidate for blocking -> yellow
      "   callable -> green
      " Variant B (maybe we can use it in another recommendation column):
      "   blocked but called -> red
      "   callable but not called -> yellow
      "   callable and called -> green
      "   blocked and not called -> neutral
      IF ls_data-id IS INITIAL.
        IF ls_data-actual_phase = 'A'.
          APPEND VALUE #( fname = 'FUNCNAME' color-col = col_negative ) TO ls_data-ctab. " red: blocked
        ELSE.
          APPEND VALUE #( fname = 'FUNCNAME' color-col = col_total )    TO ls_data-ctab. " yellow: candidate for blocking
        ENDIF.
      ELSE.
        APPEND VALUE #( fname = 'FUNCNAME' color-col = col_positive )   TO ls_data-ctab. " green: callable
      ENDIF.

      " Show error if function is not RFC enabled anymore
      " Use report RS_UCON_CLEAN_RFC_STATE to repair these entries
      IF NOT ( ls_data-fmode = 'R' OR ls_data-fmode = 'X' OR ls_data-fmode = 'K').
        APPEND VALUE #( fname = 'FMODE'     color-col = col_negative ) TO ls_data-ctab.
      ENDIF.
      " Show error if function does not exist anymore
      " Use report RS_UCON_CLEAN_RFC_STATE to repair these entries
      IF ls_data-area IS INITIAL.
        APPEND VALUE #( fname = 'AREA'      color-col = col_negative ) TO ls_data-ctab.
        APPEND VALUE #( fname = 'AREA_TEXT' color-col = col_negative ) TO ls_data-ctab.
      ENDIF.

      " Add text of phase
      CASE ls_data-actual_phase.
        WHEN 'L'.
          ls_data-phasetext = 'Logging'.
        WHEN 'E'.
          ls_data-phasetext = 'Evaluation'.
        WHEN 'A'.
          IF ls_data-id IS INITIAL.
            ls_data-phasetext = 'Final blocked'.
          else.
            ls_data-phasetext = 'Final allowed'.
          endif.
      ENDCASE.

      " Get additional user data: user type, user group, and authorizations for S_RFC (only possible for the current system)
      IF ls_data-called_sid = sy-sysid AND ls_data-called_client IS NOT INITIAL AND ls_data-called_user IS NOT INITIAL.
        DATA ls_user TYPE ts_user.
        CLEAR ls_user.
        " Do we know this user already?
        READ TABLE lt_user INTO ls_user
          WITH KEY
            mandt = ls_data-called_client
            bname = ls_data-called_user.
        IF sy-subrc NE 0.
          " New user, add it to the table
          DATA(tabix) = sy-tabix.
          SELECT SINGLE mandt, bname, ustyp, class
            FROM usr02 CLIENT SPECIFIED
            WHERE mandt = @ls_data-called_client
              AND bname = @ls_data-called_user
            INTO CORRESPONDING FIELDS OF @ls_user.
          IF sy-subrc = 0. " Yes, the user exists in current system

            " Do we like to see more data for this user?
            IF    ls_user-ustyp IN sel_ustyp
              AND ls_user-class IN sel_class.

              " Get authorization data for S_RFC
              "IF ls_user-mandt = sy-mandt.
              "  CALL FUNCTION 'GET_AUTH_VALUES' " Works only for current client
              "    EXPORTING
              "      object1           = 'S_RFC'
              "      user              = ls_user-bname
              "      "OPTIMIZE          =
              "    TABLES
              "      values            = ls_user-auth_values
              "    EXCEPTIONS
              "      user_doesnt_exist = 1
              "      OTHERS            = 2.
              "  IF sy-subrc <> 0.
              "    " Implement suitable error handling here
              "  ENDIF.
              "ENDIF.
              CALL FUNCTION 'SUSR_USER_AUTH_FOR_OBJ_GET'
                EXPORTING
*                 NEW_BUFFERING       = 3
                  mandant             = ls_user-mandt
                  user_name           = ls_user-bname
                  sel_object          = 'S_RFC'
*                 NO_TRACE            =
*                 OPTIMIZE            =
*                 RESPECT_DISABLEMNT_4_AUTH_CHK       =
*                 SACF_SCENARIO       = ' '
*             IMPORTING
*                 FULLY_AUTHORIZED    =
                TABLES
                  values              = ls_user-auth_values
*                 IT_FILTERS          =
                EXCEPTIONS
                  user_name_not_exist = 1
                  not_authorized      = 2
                  internal_error      = 3
                  OTHERS              = 4.
              IF sy-subrc <> 0.
                " Implement suitable error handling here
                CLEAR ls_user-auth_values.
              ENDIF.
              SORT ls_user-auth_values BY objct auth field von bis.

            ENDIF.

            " Store the user
            INSERT ls_user INTO lt_user INDEX tabix.
          ENDIF.
        ENDIF.

        " Do we like to see this user?
        CHECK ls_user-ustyp IN sel_ustyp.
        CHECK ls_user-class IN sel_class.

        " Copy user data
        ls_data-ustyp = ls_user-ustyp.
        ls_data-class = ls_user-class.

        " Copy authorization data
        " Let's assume that ACTVT=16 does not need to get verified and that RFC_TYPE has only one of the values *, FUGR or FUNC
        CLEAR ls_data-authorizations.
        DATA auth_star TYPE abap_bool. " Flag to indicate that this user has * authorizations
        CLEAR auth_star.
        DATA:
          ls_auth     TYPE usvalues,
          ls_rfc_name TYPE usvalues.
        LOOP AT ls_user-auth_values INTO ls_auth
          WHERE field = 'RFC_TYPE'.

          CASE ls_auth-von.
            WHEN '*'.
              " Full authorization
              LOOP AT ls_user-auth_values INTO ls_rfc_name
                WHERE auth  = ls_auth-auth
                  AND field = 'RFC_NAME'
                  AND von   = '*'.
                CONCATENATE
                  ls_data-authorizations
                  ls_rfc_name-auth
                  "ls_auth-von " '*'
                  ls_rfc_name-von
                  ','
                  INTO ls_data-authorizations SEPARATED BY space.
              ENDLOOP.
              auth_star = 'X'.

            WHEN 'FUGR'.
              " Authorization match to function group
              LOOP AT ls_user-auth_values INTO ls_rfc_name
                WHERE auth  = ls_auth-auth
                  AND field = 'RFC_NAME'
                  AND von   = ls_data-area.
                CONCATENATE
                  ls_data-authorizations
                  ls_rfc_name-auth
                  ls_auth-von " 'FUGR'
                  ls_rfc_name-von
                  ','
                  INTO ls_data-authorizations SEPARATED BY space.
              ENDLOOP.
              " Generic authorization for function group
              " ... tbd...
              " Authorization range
              LOOP AT ls_user-auth_values INTO ls_rfc_name
               WHERE auth  = ls_auth-auth
                 AND field = 'RFC_NAME'
                 AND ( von <= ls_data-area AND bis >= ls_data-area ).
                CONCATENATE
                  ls_data-authorizations
                  ls_rfc_name-auth
                  ls_auth-von " 'FUGR'
                  ls_rfc_name-von
                  '-'
                  ls_rfc_name-bis
                  ','
                  INTO ls_data-authorizations SEPARATED BY space.
              ENDLOOP.

            WHEN 'FUNC'.
              " Authorization match to function name
              LOOP AT ls_user-auth_values INTO ls_rfc_name
                WHERE auth  = ls_auth-auth
                  AND field = 'RFC_NAME'
                  AND von   = ls_data-funcname.
                CONCATENATE
                  ls_data-authorizations
                  ls_rfc_name-auth
                  ls_auth-von " 'FUNC'
                  ls_rfc_name-von
                  ','
                  INTO ls_data-authorizations SEPARATED BY space.
              ENDLOOP.
              " Generic authorization for function name
              " ... tbd...
              " Authorization range
              LOOP AT ls_user-auth_values INTO ls_rfc_name
               WHERE auth  = ls_auth-auth
                 AND field = 'RFC_NAME'
                 AND ( von <= ls_data-funcname AND bis >= ls_data-funcname ).
                CONCATENATE
                  ls_data-authorizations
                  ls_rfc_name-auth
                  ls_auth-von " 'FUNC'
                  ls_rfc_name-von
                  '-'
                  ls_rfc_name-bis
                  ','
                  INTO ls_data-authorizations SEPARATED BY space.
              ENDLOOP.

            WHEN OTHERS.
              CONCATENATE ls_data-authorizations 'Strange data' ls_auth-von ',' INTO ls_data-authorizations SEPARATED BY space.
          ENDCASE.

        ENDLOOP.

        " Set color if any * authorizations have been found
        IF auth_star = 'X'.
          APPEND VALUE #( fname = 'AUTHORIZATIONS' color-col = col_negative ) TO ls_data-ctab.
        ENDIF.

      ENDIF. " Get additional user data

      " Store data
      APPEND ls_data TO lt_data.

    ENDLOOP.


  ENDMETHOD. " extend_data.


  METHOD sapgui_progress_indicator.
    DATA    percentage          TYPE i.
    STATICS prevoius_percentage TYPE i.

    percentage = tabix * 60 / total + 40.
    IF percentage > prevoius_percentage.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = percentage
          text       = text.
      prevoius_percentage = percentage.
    ENDIF.
  ENDMETHOD.

ENDCLASS.                    "lcl_report IMPLEMENTATION

*---------------------------------------------------------------------*
*      CLASS lcl_alv IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.

  METHOD show_result.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 100
        text       = 'Show result'.

    " references to ALV objects
    DATA:
      lr_functions_list   TYPE REF TO cl_salv_functions_list,
      "lr_functions           TYPE REF TO cl_salv_functions,
      lr_selections       TYPE REF TO cl_salv_selections,
      lr_columns          TYPE REF TO cl_salv_columns_table,
      lr_column           TYPE REF TO cl_salv_column_list, "or cl_salv_column_table,
      lr_sorts            TYPE REF TO cl_salv_sorts,
      lr_aggregations     TYPE REF TO cl_salv_aggregations,
      lr_events           TYPE REF TO cl_salv_events_table,
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
          EXPORTING
            r_container  = cl_gui_container=>default_screen "screen0
          IMPORTING
            r_salv_table = r_alv_table
          CHANGING
            t_table      = lt_data ).

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
    "lr_functions_list->set_group_export( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_group_filter( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_group_layout( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_print( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_print_preview( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_group_sort( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_find( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_graphics( if_salv_c_bool_sap=>false ).
    lr_functions_list->set_all( abap_true ).



    " Additional function for ALV toolbar
    " requires r_container = cl_gui_container=>default_screen

    " Suppress toolbar of list output
    cl_abap_list_layout=>suppress_toolbar( ).
    WRITE space. "trick to get the screen

    TRY.
        DATA l_icon TYPE string.

        l_icon = icon_toggle_display_change.
        lr_functions_list->add_function(
          name     = 'DISPCHG'
          icon     = l_icon
          text     = ''
          tooltip  = 'Display/Change'
          position = if_salv_c_function_position=>right_of_salv_functions ).
        IF auth_change IS INITIAL.
          lr_functions_list->enable_function( name = 'DISPCHG' boolean = if_salv_c_bool_sap=>false ).
        ENDIF.

        l_icon = icon_set_state.
        lr_functions_list->add_function(
          name     = 'LOGGING'
          icon     = l_icon
          text     = 'Logging'
          tooltip  = 'Set to Logging Phase'
          position = if_salv_c_function_position=>right_of_salv_functions ).
        lr_functions_list->set_function( name = 'LOGGING' boolean = if_salv_c_bool_sap=>false ).

        l_icon = icon_set_state.
        lr_functions_list->add_function(
          name     = 'EVALUATION'
          icon     = l_icon
          text     = 'Evaluation'
          tooltip  = 'Set to Evaluation Phase'
          position = if_salv_c_function_position=>right_of_salv_functions ).
        lr_functions_list->set_function( name = 'EVALUATION' boolean = if_salv_c_bool_sap=>false ).

        l_icon = icon_set_state.
        lr_functions_list->add_function(
          name     = 'FINAL'
          icon     = l_icon
          text     = 'Final'
          tooltip  = 'Set to Final Phase'
          position = if_salv_c_function_position=>right_of_salv_functions ).
        lr_functions_list->set_function( name = 'FINAL' boolean = if_salv_c_bool_sap=>false ).

        l_icon = icon_assign.
        lr_functions_list->add_function(
          name     = 'ASSIGN'
          icon     = l_icon
          text     = 'Assign to Default CA'
          tooltip  = 'Assign to Default CA'
          position = if_salv_c_function_position=>right_of_salv_functions ).
        lr_functions_list->set_function( name = 'ASSIGN' boolean = if_salv_c_bool_sap=>false ).
        "lr_functions_list->enable_function( name = 'ASSIGN' boolean = if_salv_c_bool_sap=>false ). " not implemented yet

        l_icon = icon_assign.
        lr_functions_list->add_function(
          name     = 'ASSIGN_SNC'
          icon     = l_icon
          text     = 'Assign to CA for SNC'
          tooltip  = 'Assign to CA for SNC'
          position = if_salv_c_function_position=>right_of_salv_functions ).
        lr_functions_list->set_function( name = 'ASSIGN_SNC' boolean = if_salv_c_bool_sap=>false ).
        "lr_functions_list->enable_function( name = 'ASSIGN_SNC' boolean = if_salv_c_bool_sap=>false ). " not implemented yet

        l_icon = icon_unassign.
        lr_functions_list->add_function(
          name     = 'REMOVE'
          icon     = l_icon
          text     = 'Remove from CA'
          tooltip  = 'Remove from CA'
          position = if_salv_c_function_position=>right_of_salv_functions ).
        lr_functions_list->set_function( name = 'REMOVE' boolean = if_salv_c_bool_sap=>false ).
        "lr_functions_list->enable_function( name = 'REMOVE' boolean = if_salv_c_bool_sap=>false ). " not implemented yet

      CATCH cx_salv_not_found cx_salv_existing cx_salv_wrong_call.
    ENDTRY.


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

    " register to the events of cl_salv_table
    lr_events = r_alv_table->get_event( ).
    CREATE OBJECT lr_alv_events.
    " register to the event USER_COMMAND
    SET HANDLER lr_alv_events->on_user_command FOR lr_events.
    " register to the event DOUBLE_CLICK
    SET HANDLER lr_alv_events->on_double_click FOR lr_events.

    " Selection mode: single cell
    lr_selections   = r_alv_table->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ). " or if_salv_c_selection_mode=>row_column

    " Sort columns (Example)
*    TRY.
*        lr_sorts = r_alv_table->get_sorts( ).
*        lr_sorts->add_sort( columnname = 'DLVUNIT'
*                            position   = 1
*                            sequence   = if_salv_c_sort=>sort_down
*                            subtotal   = abap_true ).
*        lr_sorts->add_sort( columnname = 'DEVCLASS'
*                            position   = 2
*                            sequence   = if_salv_c_sort=>sort_down
*                            subtotal   = abap_true ).
*        lr_sorts->add_sort( columnname = 'AREA'
*                            position   = 3
*                            sequence   = if_salv_c_sort=>sort_down
*                            subtotal   = abap_true ).
*      CATCH cx_salv_not_found
*            cx_salv_existing
*            cx_salv_data_error.
*    ENDTRY.

    " Aggregation columns (Example)
    TRY.
        lr_aggregations = r_alv_table->get_aggregations( ).
        lr_aggregations->add_aggregation( columnname = 'COUNTER_SAME_SYSTEM'  aggregation = if_salv_c_aggregation=>total ).
        lr_aggregations->add_aggregation( columnname = 'COUNTER_OTHER_SYSTEM' aggregation = if_salv_c_aggregation=>total ).
        lr_aggregations->add_aggregation( columnname = 'COUNTER'              aggregation = if_salv_c_aggregation=>total ).
      CATCH cx_salv_not_found
            cx_salv_existing
            cx_salv_data_error.
    ENDTRY.

    " Get column definitions
    lr_columns = r_alv_table->get_columns( ).

    " Set the column width
    lr_columns->set_optimize( if_salv_c_bool_sap=>true ).
    lr_columns->set_key_fixation( if_salv_c_bool_sap=>true ).

    " Set field descriptions and attributes
    DATA ls_color_called  TYPE lvc_s_colo.
    ls_color_called-col = col_heading.
    DATA ls_color_caller  TYPE lvc_s_colo.
    ls_color_caller-col = col_group. " col_group: light red

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
* SET_CELL_TYPE           Set Cell Type (Text, Checkbox, Pushbutton, Dropdown, Link, Hotspot)
* SET_DROPDOWN_ENTRY      Set Handle for Dropdown
* SET_HYPERLINK_ENTRY     Set Handle for Dropdown
* SET_F4                  Set Column with F1 Help
* SET_F4_CHECKTABLE       Set Check Table for F4 Help
* SET_KEY_PRESENCE_REQUIRED Set Key Columns as Always Visible
* SET_SYMBOL              Set Column as Symbol Column
* SET_TEXT_COLUMN         Set Text Column

        lr_column ?= lr_columns->get_column( 'FUNCNAME' ).
        lr_column->set_key( if_salv_c_bool_sap=>true ).
        "lr_column->set_cell_type( ). " dropdown, link, hotspot, checkbox_hotspot

        lr_column ?= lr_columns->get_column( 'FMODE' ).
        lr_column->set_long_text( 'RFC Mode' ).
        lr_column->set_medium_text( 'RFC Mode' ).
        lr_column->set_short_text( 'RFC Mode' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'FUNC_TEXT' ).
        lr_column->set_long_text( 'Function text' ).
        lr_column->set_medium_text( 'Function text' ).
        lr_column->set_short_text( 'Function' ).

        lr_column ?= lr_columns->get_column( 'FUNC_CALLED' ).
        lr_column->set_long_text( 'Called function' ).
        lr_column->set_medium_text( 'Called function' ).
        lr_column->set_short_text( 'CalledFunc' ).
        "lr_column->set_symbol( if_salv_c_bool_sap=>true ).
        lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

        lr_column ?= lr_columns->get_column( 'AREA' ).
        "lr_column->set_key( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'AREA_TEXT' ).
        lr_column->set_long_text( 'Function group text' ).
        lr_column->set_medium_text( 'Function group text' ).
        lr_column->set_short_text( 'Func.Group' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'AREA_CALLED' ).
        lr_column->set_long_text( 'Function group with called functions' ).
        lr_column->set_medium_text( 'Called funct. group' ).
        lr_column->set_short_text( 'CalledGrp' ).
        "lr_column->set_symbol( if_salv_c_bool_sap=>true ).
        lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

        lr_column ?= lr_columns->get_column( 'DEVCLASS' ).
        "lr_column->set_key( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'DEVCLASS_TEXT' ).
        lr_column->set_long_text( 'Package text' ).
        lr_column->set_medium_text( 'Package' ).
        lr_column->set_short_text( 'Package' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'DEVCLASS_CALLED' ).
        lr_column->set_long_text( 'Package with called functions' ).
        lr_column->set_medium_text( 'Called package' ).
        lr_column->set_short_text( 'CalledPack' ).
        "lr_column->set_symbol( if_salv_c_bool_sap=>true ).
        lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).


        lr_column ?= lr_columns->get_column( 'SWITCH_ID' ).
        "lr_column->set_symbol( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'SWITCH_NAME' ).
        lr_column->set_long_text( 'Switch name' ).
        lr_column->set_medium_text( 'Switch name' ).
        lr_column->set_short_text( 'SwitchName' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'SWITCH_STATE' ).
        lr_column->set_long_text( 'Switch state' ).
        lr_column->set_medium_text( 'Switch state' ).
        lr_column->set_short_text( 'SwitchStat' ).


        lr_column ?= lr_columns->get_column( 'DLVUNIT' ).
        "lr_column->set_key( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'DLVUNIT_CALLED' ).
        lr_column->set_long_text( 'Called software component' ).
        lr_column->set_medium_text( 'Called SW component' ).
        lr_column->set_short_text( 'CalledSW' ).
        "lr_column->set_symbol( if_salv_c_bool_sap=>true ).
        lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

        lr_column ?= lr_columns->get_column( 'BLPACKAGE' ).
        lr_column->set_long_text( 'Blocklist package' ).
        lr_column->set_medium_text( 'Blocklist package' ).
        lr_column->set_short_text( 'Blocklist' ).
        IF sy-saprl < 750.
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
        ENDIF.


        lr_column ?= lr_columns->get_column( 'ACTUAL_PHASE' ).
        lr_column->set_long_text( 'UCON Phase' ).
        lr_column->set_medium_text( 'UCON Phase' ).
        lr_column->set_short_text( 'UCON Phase' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'PHASETEXT' ).
        lr_column->set_long_text( 'UCON Phase' ).
        lr_column->set_medium_text( 'UCON Phase' ).
        lr_column->set_short_text( 'UCON Phase' ).

        lr_column ?= lr_columns->get_column( 'END_PHASE' ).
        lr_column->set_long_text( 'Expiration date' ).
        lr_column->set_medium_text( 'Expiration date' ).
        lr_column->set_short_text( 'Expiration' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'ID' ).


        lr_column ?= lr_columns->get_column( 'CALLED_SID' ).
        lr_column->set_long_text( 'Called system' ).
        lr_column->set_medium_text( 'Called system' ).
        lr_column->set_short_text( 'System' ).
        lr_column->set_color( ls_color_called ).

        lr_column ?= lr_columns->get_column( 'CALLED_INSTALLATION_NR' ).
        lr_column->set_long_text( 'Called installation' ).
        lr_column->set_medium_text( 'Called installation' ).
        lr_column->set_short_text( 'Inst. Nr.' ).
        lr_column->set_color( ls_color_called ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'CALLED_CLIENT' ).
        lr_column->set_long_text( 'Called client' ).
        lr_column->set_medium_text( 'Called client' ).
        lr_column->set_short_text( 'Client' ).
        lr_column->set_color( ls_color_called ).

        lr_column ?= lr_columns->get_column( 'CALLED_USER' ).
        lr_column->set_long_text( 'Called user' ).
        lr_column->set_medium_text( 'Called user' ).
        lr_column->set_short_text( 'User' ).
        lr_column->set_color( ls_color_called ).

        lr_column ?= lr_columns->get_column( 'USTYP' ).
        lr_column->set_color( ls_color_called ).

        lr_column ?= lr_columns->get_column( 'CLASS' ).
        lr_column->set_color( ls_color_called ).

        lr_column ?= lr_columns->get_column( 'AUTHORIZATIONS' ).
        lr_column->set_long_text( 'Authorizations for S_RFC for called user' ).
        lr_column->set_medium_text( 'Authorizations S_RFC' ).
        lr_column->set_short_text( 'Auth.S_RFC' ).
        lr_column->set_color( ls_color_called ).


        lr_column ?= lr_columns->get_column( 'REJECTED_RFC_CALL' ).


        lr_column ?= lr_columns->get_column( 'CALLER_SID' ).
        lr_column->set_long_text( 'Calling system' ).
        lr_column->set_medium_text( 'Calling system' ).
        lr_column->set_short_text( 'C-System' ).
        lr_column->set_color( ls_color_caller ).

        lr_column ?= lr_columns->get_column( 'CALLER_INSTANCE' ).
        lr_column->set_long_text( 'Calling installation' ).
        lr_column->set_medium_text( 'Calling installation' ).
        lr_column->set_short_text( 'C-Inst. Nr' ).
        lr_column->set_color( ls_color_caller ).

        lr_column ?= lr_columns->get_column( 'CALLER_CLIENT' ).
        lr_column->set_long_text( 'Calling client' ).
        lr_column->set_medium_text( 'Calling client' ).
        lr_column->set_short_text( 'C-Client' ).
        lr_column->set_color( ls_color_caller ).

        lr_column ?= lr_columns->get_column( 'CALLER_USER' ).
        lr_column->set_long_text( 'Calling user' ).
        lr_column->set_medium_text( 'Calling user' ).
        lr_column->set_short_text( 'C-User' ).
        lr_column->set_color( ls_color_caller ).

        lr_column ?= lr_columns->get_column( 'CALLER_DESTINATION' ).
        lr_column->set_color( ls_color_caller ).


        lr_column ?= lr_columns->get_column( 'FIRSTCALL_TIMESTAMP' ).
        lr_column->set_long_text( 'First call' ).
        lr_column->set_medium_text( 'First call' ).
        lr_column->set_short_text( 'First call' ).

        lr_column ?= lr_columns->get_column( 'PREVIOUSCALL_TIMESTAMP' ).
        lr_column->set_long_text( 'Previous call' ).
        lr_column->set_medium_text( 'Previous call' ).
        lr_column->set_short_text( 'Prev. call' ).

        lr_column ?= lr_columns->get_column( 'LASTCALL_TIMESTAMP' ).
        lr_column->set_long_text( 'Last call' ).
        lr_column->set_medium_text( 'Last call' ).
        lr_column->set_short_text( 'Last call' ).

        lr_column ?= lr_columns->get_column( 'COUNTER_SAME_SYSTEM' ).
        lr_column->set_long_text( 'Counter same system' ).
        lr_column->set_medium_text( 'Counter same system' ).
        lr_column->set_short_text( 'Same sys' ).

        lr_column ?= lr_columns->get_column( 'COUNTER_OTHER_SYSTEM' ).
        lr_column->set_long_text( 'Counter other system' ).
        lr_column->set_medium_text( 'Counter other system' ).
        lr_column->set_short_text( 'Other sys' ).

        lr_column ?= lr_columns->get_column( 'COUNTER' ).
        lr_column->set_long_text( 'Counter' ).
        lr_column->set_medium_text( 'Counter' ).
        lr_column->set_short_text( 'Counter' ).

        " Hide empty fields in case of the simple list
        IF ext_list IS INITIAL.
          lr_column ?= lr_columns->get_column( 'CALLED_CLIENT' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
          lr_column ?= lr_columns->get_column( 'CALLED_USER' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
          lr_column ?= lr_columns->get_column( 'USTYP' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
          lr_column ?= lr_columns->get_column( 'CLASS' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
          lr_column ?= lr_columns->get_column( 'AUTHORIZATIONS' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).

          lr_column ?= lr_columns->get_column( 'REJECTED_RFC_CALL' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).

          lr_column ?= lr_columns->get_column( 'CALLER_SID' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
          lr_column ?= lr_columns->get_column( 'CALLER_INSTANCE' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
          lr_column ?= lr_columns->get_column( 'CALLER_CLIENT' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
          lr_column ?= lr_columns->get_column( 'CALLER_USER' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
          lr_column ?= lr_columns->get_column( 'CALLER_DESTINATION' ).
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
        ENDIF.

      CATCH cx_salv_not_found
        INTO lr_exception.
        lv_message = lr_exception->get_message( ).
        MESSAGE ID lv_message-msgid TYPE lv_message-msgty
                NUMBER lv_message-msgno
                WITH lv_message-msgv1 lv_message-msgv2
                     lv_message-msgv3 lv_message-msgv4.
    ENDTRY.

    " Set the color of cells
    TRY.
        lr_columns->set_color_column( 'CTAB' ). " The color field has to have type LVC_T_SCOL
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

*    " Footer (not visible on gui container)
*    DATA l_line TYPE i.
*    CREATE OBJECT lr_grid_footer.
*    l_line = 1.
*
*    " Program version
*    lr_grid_footer->create_text(
*         row    = l_line
*         column = 1
*         text   = 'Program version:'(ver) ).
*    lr_grid_footer->create_text(
*         row    = l_line
*         column = 2
*         text   = c_program_version ).
*    ADD 1 TO l_line.
*
*    r_alv_table->set_end_of_list( lr_grid_footer ).

    " Set Title
    lr_display_settings = r_alv_table->get_display_settings( ).
    "lr_display_settings->set_list_header( 'Show extended UCON RFC data' ). "sy-title
    lr_display_settings->set_list_header( |Program version: { c_program_version }| ).
    lr_display_settings->set_list_header_size(
      cl_salv_display_settings=>c_header_size_small ).
    lr_display_settings->set_no_merging( if_salv_c_bool_sap=>true ).


    " Display the table
    r_alv_table->display( ).

  ENDMETHOD. " show_result

  METHOD on_user_command.
    " importing e_salv_function

    " Get selected item
    DATA(lr_selections) = r_alv_table->get_selections( ).
    DATA(ls_cell) = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    DATA lcx_ucon_not_active TYPE REF TO cx_ucon_not_active.

    " see UCON_PHASE_TOOL form PAI.
    CASE e_salv_function.

      WHEN 'DISPCHG'.
        " Toogle between display and change
        DATA(lr_functions_list) = r_alv_table->get_functions( ).
        IF lr_functions_list->is_visible( 'LOGGING' ).
          DATA(change_mode) = if_salv_c_bool_sap=>false.
          CALL FUNCTION 'DEQUEUE_E_UCON_PHTL_EDIT'.
        ELSE.
          AUTHORITY-CHECK OBJECT 'S_UCON_ADM'
                   ID 'UCON_TYPE' DUMMY
                   ID 'UCON_NAME' DUMMY
                   ID 'ACTVT' FIELD '02'.
          IF sy-subrc <> 0.
            MESSAGE e217(s_ucon_lm).
            RETURN.
          ENDIF.
          CALL FUNCTION 'ENQUEUE_E_UCON_PHTL_EDIT'
            EXPORTING
              mode_uconrfcstatehead = 'E'
              _scope                = '2'
              _wait                 = space
              _collect              = ' '
            EXCEPTIONS
              foreign_lock          = 1
              system_failure        = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            change_mode = if_salv_c_bool_sap=>true.
          ENDIF.
        ENDIF.
        TRY.
            lr_functions_list->set_function( name = 'LOGGING'    boolean = change_mode ).
            lr_functions_list->set_function( name = 'EVALUATION' boolean = change_mode ).
            lr_functions_list->set_function( name = 'FINAL'      boolean = change_mode ).
            lr_functions_list->set_function( name = 'ASSIGN'     boolean = change_mode ).
            lr_functions_list->set_function( name = 'ASSIGN_SNC' boolean = change_mode ).
            lr_functions_list->set_function( name = 'REMOVE'     boolean = change_mode ).
            " Refresh Toolbar not needed
            "r_alv_table->refresh( ).
          CATCH cx_salv_not_found cx_salv_wrong_call.
        ENDTRY.

        " Set phase
      WHEN 'LOGGING' OR 'EVALUATION' OR 'FINAL'.
        DATA(changed_entries) = set_phase(
          EXPORTING
            seleced_rows  = lt_seleced_rows
            salv_function = e_salv_function
          ).
        IF changed_entries IS NOT INITIAL.
          " Refresh ALV list
          r_alv_table->refresh( ).
        ENDIF.

        " Set/unset CA assignment
      WHEN 'ASSIGN' OR 'ASSIGN_SNC' OR 'REMOVE'.
        changed_entries = set_ca(
          EXPORTING
            seleced_rows  = lt_seleced_rows
            salv_function = e_salv_function
          ).
        IF changed_entries IS NOT INITIAL.
          " Refresh ALV list
          r_alv_table->refresh( ).
        ENDIF.

    ENDCASE.

  ENDMETHOD. " on_user_command

  METHOD on_double_click.
    " importing row column

    " Get selected item
    DATA(lr_selections) = r_alv_table->get_selections( ).
    DATA(ls_cell) = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    CHECK row > 0.

    READ TABLE lt_data INTO DATA(ls_data) INDEX row.
    CHECK sy-subrc = 0.

    CASE column.

      WHEN 'FUNCNAME' OR 'FMODE' OR 'FUNC_TEXT' OR 'FUNC_CALLED'.
        show_function( ls_data-funcname ).

      WHEN 'AREA' OR 'AREA_TEXT' OR 'AREA_CALLED'.
        show_function_group( ls_data-area ).

      WHEN 'DEVCLASS' OR 'DEVCLASS_TEXT' OR 'DEVCLASS_CALLED'.
        show_package( ls_data-devclass ).

      WHEN 'ID'.
        show_communication_assembly( ls_data-id ).

      WHEN 'CALLED_USER'.
        CHECK ls_data-called_client = sy-mandt.
        show_user( ls_data-called_user ).

      WHEN 'AUTHORIZATIONS'.
        show_authorizations(
          client         = ls_data-called_client
          user           = ls_data-called_user
          authorizations = ls_data-authorizations ).

    ENDCASE.

  ENDMETHOD. " on_double_click


  METHOD set_phase.

    CLEAR changed_entries.

    DATA new_phase TYPE uconrfcphase.
    CASE salv_function.
      WHEN 'LOGGING'.    new_phase = 'L'.
      WHEN 'EVALUATION'. new_phase = 'E'.
      WHEN 'FINAL'.      new_phase = 'A'. "Should we ask for a confirmation?
    ENDCASE.

    LOOP AT seleced_rows INTO DATA(index).
      READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<line>) INDEX index.
      IF sy-subrc = 0.
        DATA(state_obj) = cl_ucon_state_factory=>get_ucon_state_object(
                               iv_function_name = <line>-funcname ).
        DATA(current_phase) = state_obj->get_current_phase( ).
        IF current_phase <> new_phase.
          state_obj->set_phase( iv_phase = new_phase ).
          state_obj->set_date( iv_start_phase_date = sy-datum ).
          state_obj->get_date(
            IMPORTING
              "ev_date_start_phase = sy-datum    ##WRITE_OK  " Current Date of Application Server
              ev_date_end_phase   = <line>-end_phase
              "ev_phase_length     = <line>-duration_days    " Length of phase in days
          ).
          <line>-actual_phase = new_phase.
          CASE new_phase.
            WHEN 'L'.
              <line>-phasetext = 'Logging (changed)'.
            WHEN 'E'.
              <line>-phasetext = 'Evaluation (changed)'.
            WHEN 'A'.
              <line>-phasetext = 'Final (changed)'.
          ENDCASE.
          ADD 1 TO changed_entries.
        ENDIF.
      ENDIF.
    ENDLOOP.
    MESSAGE s159(s_unified_con) WITH |UCON Logging Phase: { changed_entries } changed entries|.
    IF changed_entries IS NOT INITIAL.
      " Save state-object
      TRY.
          cl_ucon_state_factory=>save_all(
            "iv_run_dark =
            iv_transport_requested = cl_ucon_setup=>is_transport_requested( )
            "iv_no_commit =
            "iv_avoid_cd =
          ).
        CATCH cx_ucon_api_state cx_ucon_base INTO DATA(cx).
          MESSAGE cx TYPE 'E'.
      ENDTRY.
    ENDIF.

  ENDMETHOD. " set_phase

  METHOD set_ca.

    CLEAR changed_entries.

    DATA:
      lc_default_ca TYPE uconcaid VALUE 'DEFAULT_CA',
      lc_snc_ca     TYPE uconcaid VALUE 'SNC_CA'.
    TRY.
        cl_ucon_setup=>get_default_object_names(
          EXPORTING
            client_independent_only = abap_true
          IMPORTING
            default_ca_name = lc_default_ca  ).
        cl_ucon_setup=>get_snc_ca_name(
         EXPORTING
           check_for_existence = abap_true
         IMPORTING
           snc_ca_name = lc_snc_ca  ).
      CATCH cx_ucon_not_active INTO DATA(lcx_ucon_not_active).
        DATA(l_err_text) = lcx_ucon_not_active->get_text( ).
        MESSAGE e000(s_unified_con) WITH l_err_text.
    ENDTRY.

    DATA:
      selected_data TYPE ucon_phase_tool_fields_tt,
      new_line      TYPE ucon_phase_tool_fields_ext,
      lt_ca_list    TYPE TABLE OF ucon_ca_list.

    CASE salv_function.
      WHEN 'ASSIGN' OR 'ASSIGN_SNC'.

        " Copy selected entries
        LOOP AT seleced_rows INTO DATA(index).
          READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<line>) INDEX index.
          IF sy-subrc = 0.
            new_line-called_rfm = <line>-funcname.
            new_line-id = <line>-id.
            MOVE-CORRESPONDING <line> TO new_line.
            APPEND new_line TO selected_data.

            " Update ALV
            CASE salv_function.
              WHEN 'ASSIGN'.     <line>-id = lc_default_ca.
              WHEN 'ASSIGN_SNC'. <line>-id = lc_snc_ca.
            ENDCASE.
          ENDIF.
        ENDLOOP.

        CALL METHOD cl_ucon_phase_tool=>filter_blacklist_rfms
          CHANGING
            lt_display_data = selected_data.

        CHECK selected_data IS NOT INITIAL.

        CLEAR lt_ca_list.

        CASE salv_function.

          WHEN 'ASSIGN'.
            IF lc_snc_ca IS NOT INITIAL.
              CALL METHOD cl_ucon_phase_tool=>remove_rfm_from_ca
                EXPORTING
                  remove_ca           = lc_snc_ca
                CHANGING
                  chg_ca_list         = lt_ca_list
                  chg_called_rfm_list = selected_data.
            ENDIF.

            CALL METHOD cl_ucon_phase_tool=>set_rfm_to_ca
              EXPORTING
                i_new_ca            = lc_default_ca
              CHANGING
                chg_ca_list         = lt_ca_list
                chg_called_rfm_list = selected_data.

          WHEN 'ASSIGN_SNC'.
            CALL METHOD cl_ucon_phase_tool=>move_rfm_to_ca
              EXPORTING
                i_new_ca            = lc_snc_ca
                i_remove_ca         = lc_default_ca
              CHANGING
                chg_ca_list         = lt_ca_list
                chg_called_rfm_list = selected_data.
            CALL METHOD cl_ucon_phase_tool=>set_rfm_to_ca
              EXPORTING
                i_new_ca            = lc_snc_ca
              CHANGING
                chg_ca_list         = lt_ca_list
                chg_called_rfm_list = selected_data.

        ENDCASE.
        " 134(S_UNIFIED_CON)  Add &1 object(s) to Communication Assembly &2

      WHEN 'REMOVE'.
        " Copy selected entries
        LOOP AT seleced_rows INTO index.
          READ TABLE lt_data ASSIGNING <line> INDEX index.
          IF sy-subrc = 0.
            CHECK <line>-id CP '*DEFAULT_CA' OR <line>-id CP '*SNC_CA'. " Touch default CAs only

            new_line-called_rfm = <line>-funcname.
            new_line-id = <line>-id.
            MOVE-CORRESPONDING <line> TO new_line.
            APPEND new_line TO selected_data.

            " Update ALV
            <line>-id = ''.
          ENDIF.
        ENDLOOP.

        CHECK selected_data IS NOT INITIAL.

        CLEAR lt_ca_list.
        CALL METHOD cl_ucon_phase_tool=>remove_rfm_from_ca
          EXPORTING
            remove_ca           = lc_default_ca
          CHANGING
            chg_called_rfm_list = selected_data
            chg_ca_list         = lt_ca_list.
        IF lc_snc_ca IS NOT INITIAL.
          CALL METHOD cl_ucon_phase_tool=>remove_rfm_from_ca
            EXPORTING
              remove_ca           = lc_snc_ca
            CHANGING
              chg_called_rfm_list = selected_data
              chg_ca_list         = lt_ca_list.
        ENDIF.

    ENDCASE.

    DESCRIBE TABLE lt_ca_list LINES changed_entries.
    IF changed_entries IS NOT INITIAL.
      DATA lcx_error TYPE REF TO cx_root.
      TRY.
          "cl_ucon_setup=>init( ). "init setup again and reread data from DB.
          " Save data. If no CAs have been touched only state objects are updated
          "CALL METHOD cl_ucon_phase_tool=>save
          "  EXPORTING
          "    i_com_assembly_t = lt_ca_list.
          LOOP AT lt_ca_list INTO DATA(wa_com_assembly).
            TRY.
                wa_com_assembly-ca_object->activate( ).
                wa_com_assembly-ca_object->save( ).

                " For unknown reasons we have to dequeue the CA after saving.
                CALL FUNCTION 'DEQUEUE_E_UCON_SA'
                  EXPORTING
                    mode_uconservashead = 'X'  "   "Art der Kurzzeitsperre: 'X' (exclusiv) oder 'E' (kumulativ)
                    id                  = wa_com_assembly-ca_id
                    "X_OBJTYPE           = ' '
                    "X_OBJNAME           = ' '
                    "_SCOPE              = '3'
                    "_SYNCHRON           = ' '
                    "_COLLECT            = ' '
                  .
              CATCH cx_ucon_api_com_assembly INTO DATA(cx).
                MESSAGE cx TYPE 'E'.
            ENDTRY.
          ENDLOOP.

        CATCH cx_root INTO lcx_error.
          MESSAGE lcx_error TYPE 'W'.
      ENDTRY.
    ENDIF.

  ENDMETHOD. " set_ca

  METHOD show_function.
    CHECK funcname IS NOT INITIAL.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = funcname
        object_type         = 'FUNC'
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE w215(s_ucon_lm).
    ENDIF.
  ENDMETHOD. " show_function

  METHOD show_function_group.
    CHECK area IS NOT INITIAL.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = area
        object_type         = 'FUGR'
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE w215(s_ucon_lm).
    ENDIF.
  ENDMETHOD. " show_function_group

  METHOD show_package.
    CHECK devclass IS NOT INITIAL.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = devclass
        object_type         = 'DEVC'
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE w215(s_ucon_lm).
    ENDIF.
  ENDMETHOD. " show_package

  METHOD show_communication_assembly.
    CHECK id IS NOT INITIAL.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = id
        object_type         = 'UCSA'
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE w215(s_ucon_lm).
    ENDIF.
  ENDMETHOD. " show_communication_assembly

  METHOD show_user.
    " Does not work as it reuses the current screen
    "CALL FUNCTION 'SUID_IDENTITY_MAINT'
    "  EXPORTING
    "    I_USERNAME           = user
    "    "I_TCODE_MODE         = 1 " 1: Single maintenance SU01, 6: Display mode SU01D
    "    I_SU01_DISPLAY       = abap_true
    .
    " same as above
    "CALL FUNCTION 'SUSR_USER_MAINT_WITH_DIALOG'
    "  EXPORTING
    "    "MAINT_FOR_OWN_USER_ONLY       =
    "    DISPLAY_ONLY                  = 'X'
    "    USER_TO_DISPLAY               = user
    "     "DO_NOT_USE                    = ' '
    "   EXCEPTIONS
    "     ERROR_WRITING_TO_DB           = 1
    "     OTHERS                        = 2
    "           .

    DATA bdcdata TYPE TABLE OF bdcdata WITH EMPTY KEY.
    bdcdata = VALUE #(
      " Show user
      "( program = 'SAPLSUU5'     dynpro = '0050'  dynbegin = 'X' )         " old screen
      ( program = 'SAPLSUID_MAINTENANCE'  dynpro = '1050'  dynbegin = 'X' ) " current screen
      "( fnam    = 'USR02-BNAME'  fval   = user    )
      ( fnam    = 'SUID_ST_BNAME-BNAME'  fval   = user    )
      ( fnam    = 'BDC_OKCODE'   fval   = '=SHOW' ) " SHOW, CHAN

      " End transaction after going back
      "( program = 'SAPLSUU5'     dynpro = '0050'  dynbegin = 'X' )         " old screen
      ( program = 'SAPLSUID_MAINTENANCE'  dynpro = '1050'  dynbegin = 'X' ) " current screen
      ( fnam    = 'BDC_OKCODE'   fval   = '=BACK' )
    ).

    DATA(opt) = VALUE ctu_params( dismode = 'E'    " Display of screens only if an error occurs
                                  defsize = 'X' ). " Standard size of screens

    SET PARAMETER ID 'XUS' FIELD user.

    TRY.
        CALL TRANSACTION 'SU01' WITH AUTHORITY-CHECK
            USING bdcdata
            OPTIONS FROM opt.
      CATCH cx_sy_authorization_error.
    ENDTRY.

  ENDMETHOD. " show_user

  METHOD show_authorizations.

    READ TABLE lt_user INTO DATA(ls_user)
      WITH TABLE KEY
        mandt = client
        bname = user.
    CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'CRM_SURVEY_EDITOR_LONGTEXT'
*     IMPORTING
*       GROUP              =
*       INCLUDE            =
*       NAMESPACE          =
*       STR_AREA           =
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
      " Show multiple long lines in a textedit control
      DATA longtext TYPE string.
      longtext = |Authorizations for user { ls_user-mandt } { ls_user-bname } (Type { ls_user-ustyp }, group { ls_user-class })|.

      "LOOP AT ls_user-auth_values INTO DATA(auth_value).
      "  CONCATENATE
      "    auth_value-objct
      "    auth_value-auth
      "    auth_value-field
      "    auth_value-von
      "    auth_value-bis
      "    INTO DATA(s) SEPARATED BY space.
      "  CONCATENATE
      "    longtext
      "    s
      "    INTO longtext
      "    SEPARATED BY cl_abap_char_utilities=>newline.
      "ENDLOOP.

      CONCATENATE
        longtext
        authorizations
        INTO longtext
        SEPARATED BY cl_abap_char_utilities=>newline.

      CALL FUNCTION 'CRM_SURVEY_EDITOR_LONGTEXT'
        EXPORTING
*         MAX_LENGTH     = 0
          read_only      = 'X'
        CHANGING
          longtext       = longtext
        EXCEPTIONS
          user_cancelled = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
*  Implement suitable error handling here
      ENDIF.

    ELSE.

      " Show multiple long lines on a list popup
      DATA:
        titlebar(80),
        line_size    TYPE i,
        list_tab     TYPE TABLE OF trtab,
        line         TYPE          trtab.

      CONCATENATE
        'Authorizations for user'
        user
        INTO titlebar
        SEPARATED BY space.
      line_size = 60.

      SPLIT authorizations AT ' ,' INTO TABLE list_tab.

      CALL FUNCTION 'LAW_SHOW_POPUP_WITH_TEXT'
        EXPORTING
          titelbar         = titlebar
*         HEADER_LINES     =
*         SHOW_CANCEL_BUTTON           = ' '
          line_size        = line_size
*         SHOW_BUTTON_YES_TO_ALL       = ' '
*       IMPORTING
*         YES_TO_ALL       =
        TABLES
          list_tab         = list_tab
        EXCEPTIONS
          action_cancelled = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
*   Implement suitable error handling here
      ENDIF.

    ENDIF.

  ENDMETHOD. " show_authorizations

ENDCLASS.                    "cl_alv IMPLEMENTATION

*----------------------------------------------------------------------*
*      REPORT events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_report=>at_selection_screen_output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_fumo-low.
  lcl_report=>at_selection_screen_f4_fumo( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_fumo-high.
  lcl_report=>at_selection_screen_f4_fumo(  ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_mandt-low.
  lcl_report=>at_selection_screen_f4_mandt( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_mandt-high.
  lcl_report=>at_selection_screen_f4_mandt(  ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_bname-low.
  lcl_report=>at_selection_screen_f4_bname( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_bname-high.
  lcl_report=>at_selection_screen_f4_bname(  ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_class-low.
  lcl_report=>at_selection_screen_f4_class( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_class-high.
  lcl_report=>at_selection_screen_f4_class(  ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR layout.
  lcl_report=>at_selection_screen_f4_layout(
    CHANGING layout = layout
  ).

AT SELECTION-SCREEN.
  lcl_report=>at_selection_screen( ).

START-OF-SELECTION.
  lcl_report=>start_of_selection(
    EXPORTING
      p_system     = p_system   " System and installation number

      p_simp       = p_simp     " Simple list
      p_comp       = p_comp     " Enhanced list

      sel_function = so_fumo[]  " Functions
      sel_area     = so_area[]  " Function groups
      sel_devclass = so_devc[]  " Packages
      sel_dlvunit  = so_unit[]  " Software component
      switch       = p_switch   " Switched components
      blocklist_server = p_bl_srv

      sel_mandt    = so_mandt[] " Clients
      sel_bname    = so_bname[] " Users
      sel_ustyp    = so_ustyp[] " User types
      sel_class    = so_class[] " User groups

      p_mon        = p_mon      " Called Function Modules
      p_nmon       = p_nmon     " Uncalled Function Modules
      p_all        = p_all      " All Function Modules
      p_mona       = p_mona     " Called Function Groups
      p_nmona      = p_nmona    " Uncalled Function Groups
      p_mond       = p_mond     " Called Packages
      p_nmond      = p_nmond    " Uncalled Packages
      p_mons       = p_mons     " Called Software Components
      p_nmons      = p_nmons    " Uncalled Software Components

      p_assi       = p_assi     " RFMs Assigned to Default CA
      p_unas       = p_unas     " Unassigned RFMs
      p_both       = p_both     " Assigned and Unassigned RFMs
      p_assi_s     = p_assi_s   " Assigned to SNC CA

      p_log        = p_log      " RFMs in Logging Phase
      p_eval       = p_eval     " RFMs in Evaluation Phase
      p_act        = p_act      " RFMs in Final Phase
      p_log_e      = p_log_e    " Expired RFMs in Logg. Phase
      p_eval_e     = p_eval_e   " Expired RFMs in Eval. Phase
      p_all_p      = p_all_p    " All Phases
          ).
