*&---------------------------------------------------------------------*
*& Report ZSHOW_UCON_RFC_DATA
*& Show extended UCON data for RFC
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Published on: https://github.com/SAP-samples/security-services-tools
*&
*& 26.04.2024 Initial version
*&---------------------------------------------------------------------*
REPORT zshow_ucon_rfc_data.

CONSTANTS c_program_version(30) TYPE c VALUE '26.04.2024 S41'.

* Selection screen

" Standard view for functions / Enhanced view incl. destinations and users
SELECTION-SCREEN BEGIN OF BLOCK cols WITH FRAME TITLE tit_col.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS p_simp  RADIOBUTTON GROUP cols.
    SELECTION-SCREEN COMMENT (28) t_simp FOR FIELD p_simp.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS p_comp  RADIOBUTTON GROUP cols.
    SELECTION-SCREEN COMMENT (28) t_comp FOR FIELD p_comp.
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
SELECTION-SCREEN END   OF BLOCK fumo.

" Called users
SELECTION-SCREEN BEGIN OF BLOCK user WITH FRAME TITLE tit_usr.
  " User
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_bname FOR FIELD so_bname.
    DATA bname TYPE usr02-bname.
    SELECT-OPTIONS so_bname FOR bname.
  SELECTION-SCREEN END OF LINE.
  " User type
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_ustyp FOR FIELD so_ustyp.
    DATA ustyp TYPE usr02-ustyp.
    SELECT-OPTIONS so_ustyp FOR ustyp.
  SELECTION-SCREEN END OF LINE.
  " User group
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(28) t_class FOR FIELD so_class.
    DATA class TYPE usr02-class.
    SELECT-OPTIONS so_class FOR class.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK user.

" Called / uncalled / all
SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE tit_mde.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_mon RADIOBUTTON GROUP stat DEFAULT 'X'.
    SELECTION-SCREEN COMMENT (28) t_mon FOR FIELD p_mon.
    SELECTION-SCREEN POSITION 32.
    PARAMETERS: p_nmon  RADIOBUTTON GROUP stat.
    SELECTION-SCREEN COMMENT (28) t_nmon FOR FIELD p_nmon.
    SELECTION-SCREEN POSITION 64.
    PARAMETERS: p_all RADIOBUTTON GROUP stat.
    SELECTION-SCREEN COMMENT (28) t_all FOR FIELD p_all.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK mode.

" Assigned / not assigned / all
SELECTION-SCREEN BEGIN OF BLOCK assig WITH FRAME TITLE tit_asg.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_assi RADIOBUTTON GROUP assi DEFAULT 'X'.
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
    PARAMETERS: p_log RADIOBUTTON GROUP grob DEFAULT 'X'.
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

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_ucon_phase_tool_fields_ext,                  "* Enhanced list
        funcname               TYPE tfdir-funcname,              "* CHAR 30  Name of Function Module

        fmode                  TYPE tfdir-fmode,               " additional field
        func_text              TYPE tftit-stext,               " additional field
        area                   TYPE enlfdir-area,              " additional field
        area_text              TYPE tlibt-areat,               " additional field
        devclass               TYPE tadir-devclass,            " additional field
        devclass_text          TYPE tdevct-ctext,              " additional field
        dlvunit                TYPE tdevc-dlvunit,             " additional field

        actual_phase           TYPE  uconrfcphase,             "* CHAR 1   Phase of an RFC Function module in CA fill process
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

    TYPES:
      tt_sel_area     TYPE RANGE OF enlfdir-area,
      tt_sel_devclass TYPE RANGE OF tadir-devclass,
      tt_sel_dlvunit  TYPE RANGE OF tdevc-dlvunit,
      tt_sel_bname    TYPE RANGE OF usr02-bname,
      tt_sel_ustyp    TYPE RANGE OF usr02-ustyp,
      tt_sel_class    TYPE RANGE OF usr02-class.

    CLASS-METHODS:

      initialization,

      at_selection_screen_f4_layout
        CHANGING
          layout TYPE disvariant-variant,

      start_of_selection
        IMPORTING
          ext_list     TYPE abap_bool " Enhanced list

          sel_function TYPE ucon_func_list_range "ranges tables
          sel_area     TYPE tt_sel_area
          sel_devclass TYPE tt_sel_devclass
          sel_dlvunit  TYPE tt_sel_dlvunit

          sel_bname    TYPE tt_sel_bname
          sel_ustyp    TYPE tt_sel_ustyp
          sel_class    TYPE tt_sel_class

          p_mon        TYPE abap_bool " Called Function Modules
          p_nmon       TYPE abap_bool " Uncalled Function Modules
          p_all        TYPE abap_bool " All Function Modules

          p_assi       TYPE abap_bool " RFMs Assigned to Default CA
          p_unas       TYPE abap_bool " Unassigned RFMs
          p_both       TYPE abap_bool " Assigned and Unassigned RFMs
          p_assi_s     TYPE abap_bool " Assigned to SNC CA

          p_log        TYPE abap_bool " RFMs in Logging Phase
          p_eval       TYPE abap_bool " RFMs in Evaluation Phase
          p_act        TYPE abap_bool " RFMs in Final Phase
          p_log_e      TYPE abap_bool " Expired RFMs in Logg. Phase
          p_eval_e     TYPE abap_bool " Expired RFMs in Eval. Phase
          p_all_p      TYPE abap_bool " All Phases
        .

  PRIVATE SECTION.

    CLASS-METHODS:

      " Extend the UCON date with additional information
      extend_data
        IMPORTING
          lt_called_rfm_list TYPE ucon_phase_tool_fields_tt
          sel_area           TYPE tt_sel_area
          sel_devclass       TYPE tt_sel_devclass
          sel_dlvunit        TYPE tt_sel_dlvunit
          sel_bname          TYPE tt_sel_bname
          sel_ustyp          TYPE tt_sel_ustyp
          sel_class          TYPE tt_sel_class
        EXPORTING
          lt_data            TYPE tt_ucon_phase_tool_fields_ext
        .


ENDCLASS.                    "lcl_report DEFINITION

*---------------------------------------------------------------------*
*      CLASS lcl_alv DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_alv DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      show_result
        IMPORTING
          ext_list TYPE abap_bool " Enhanced list
        CHANGING
          et_data  TYPE ANY TABLE. " see lcl_report type tt_ucon_phase_tool_fields_ext

  PRIVATE SECTION.

    CLASS-DATA:

      r_alv_table      TYPE REF TO cl_salv_table.

ENDCLASS.                    "cl_alv DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    AUTHORITY-CHECK OBJECT 'S_UCON_ADM'
       ID 'UCON_TYPE' DUMMY
       ID 'UCON_NAME' DUMMY
       ID 'ACTVT' FIELD '03'.
    IF sy-subrc <> 0.

      LOOP AT SCREEN.
        screen-active = 0.
        MODIFY SCREEN.
      ENDLOOP.

      MESSAGE e219(s_ucon_lm).
      LEAVE TO SCREEN 0.
    ENDIF.

    sy-title = 'Show extended UCON RFC data'(tit).

    tit_col  = 'Displayed Fields'.
    t_simp   = 'Standard View'.
    t_comp   = 'Enhanced View'.

    tit_fum  = 'Remote Function Modules (RFMs)'.
    t_fumo   = 'Selection Range for RFMs'.
    t_area   = 'Function group'.
    t_devc   = 'Package'.
    t_unit   = 'Software component'.

    tit_usr  = 'Called Users'.
    t_bname  = 'User'.
    t_ustyp  = 'User type'.
    t_class  = 'User group'.

    tit_mde  = 'Filter Criteria Based on Call Statistics'.
    t_mon    = 'Called Function Modules'.
    t_nmon   = 'Uncalled Function Modules'.
    t_all    = 'All Function Modules'.

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

      sysid              TYPE sysysid,
      installation_nr    TYPE uconinstnr,
      aggregate_vh       TYPE sap_bool, "OPTIONAL
      snc_only           TYPE sap_bool, "OPTIONAL
      lt_called_rfm_list TYPE ucon_phase_tool_fields_tt,
      result_limit       TYPE abap_bool.

    " Prepare selection parametes

    REFRESH cols.
    IF ext_list = abap_false.
      " Standard view
      cl_ucon_phase_store_utility=>if_ucon_phase_store_utility~get_columns_simple_select(
        IMPORTING
         e_columns = cols ).
    ELSEIF ext_list = abap_true.
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

    " set default value for system
    sysid = sy-sysid.
    CALL FUNCTION 'SLIC_GET_LICENCE_NUMBER'
      IMPORTING
        license_number = installation_nr.

    " Called / uncalled / all
    IF p_mon = abap_true.
      call_mode = 'C'.
    ELSEIF p_nmon = abap_true.
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

    " Optimise selection for function groups respective package (does not work as the SQL statement become too big.)
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
    extend_data(
      EXPORTING
        lt_called_rfm_list = lt_called_rfm_list
        sel_area     = sel_area
        sel_devclass = sel_devclass
        sel_dlvunit  = sel_dlvunit
        sel_bname    = sel_bname
        sel_ustyp    = sel_ustyp
        sel_class    = sel_class
      IMPORTING
        lt_data = DATA(lt_data)
      ).
    FREE lt_called_rfm_list.

    " Show result
    lcl_alv=>show_result(
      EXPORTING
        ext_list   = ext_list
      CHANGING
        et_data    = lt_data
    ).

  ENDMETHOD. " start_of_selection

  METHOD extend_data.

    " User data from USR02
    TYPES:
      BEGIN OF ts_user,
        mandt TYPE usr02-mandt,
        bname TYPE usr02-bname,
        ustyp TYPE usr02-ustyp,
        class TYPE usr02-class,
      END OF ts_user,
      tt_user TYPE SORTED TABLE OF ts_user WITH UNIQUE KEY mandt bname.

    DATA:
      ls_user TYPE ts_user,
      lt_user TYPE tt_user.

    " Function data
    TYPES:
      BEGIN OF ts_function,
        funcname      TYPE tfdir-funcname,
        fmode         TYPE tfdir-fmode,
        func_text     TYPE tftit-stext,
        area          TYPE enlfdir-area,
        area_text     TYPE tlibt-areat,
        devclass      TYPE tadir-devclass,
        devclass_text TYPE tdevct-ctext,
        dlvunit       TYPE tdevc-dlvunit,
      END OF ts_function,
      tt_function TYPE SORTED TABLE OF ts_function WITH UNIQUE KEY funcname.

    DATA:
      ls_function TYPE ts_function,
      lt_function TYPE tt_function.

    DATA:
      ls_data TYPE ts_ucon_phase_tool_fields_ext.

    LOOP AT lt_called_rfm_list INTO DATA(ls_called_rfm_list)
      WHERE called_user IN sel_bname.

      CLEAR ls_data.
      MOVE-CORRESPONDING ls_called_rfm_list TO ls_data.

      " Get function data
      CLEAR ls_function.
      READ TABLE lt_function INTO ls_function WITH KEY funcname = ls_data-funcname.
      IF sy-subrc NE 0.
        SELECT SINGLE
            tfdir~funcname, " Function
            tfdir~fmode,    " RFC mode
            tftit~stext,    " Function text
            enlfdir~area,   " Function group
            tlibt~areat,    " Function group text
            tadir~devclass, " Package
            tdevct~ctext,   " Package text
            tdevc~dlvunit   " Delivery unit
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
          JOIN tdevc                       " Delivery unit
            ON    tdevc~devclass = tadir~devclass
          WHERE tfdir~funcname = @ls_data-funcname
          INTO @ls_function.
        IF sy-subrc = 0.
          INSERT ls_function INTO lt_function INDEX sy-tabix.
        ENDIF.
      ENDIF.
      CHECK ls_function-area     IN sel_area.
      CHECK ls_function-devclass IN sel_devclass.
      CHECK ls_function-dlvunit  IN sel_dlvunit.
      ls_data-fmode         = ls_function-fmode.
      ls_data-func_text     = ls_function-func_text.
      ls_data-area          = ls_function-area.
      ls_data-area_text     = ls_function-area_text.
      ls_data-devclass      = ls_function-devclass.
      ls_data-devclass_text = ls_function-devclass_text.
      ls_data-dlvunit       = ls_function-dlvunit.

      " Get user data (user type and user group)
      IF ls_data-called_sid = sy-sysid AND ls_data-called_client IS NOT INITIAL AND ls_data-called_user IS NOT INITIAL.
        CLEAR ls_user.
        READ TABLE lt_user INTO ls_user WITH KEY mandt = ls_data-called_client bname = ls_data-called_user.
        IF sy-subrc NE 0.
          SELECT SINGLE mandt, bname, ustyp, class
            FROM usr02 CLIENT SPECIFIED
            WHERE mandt = @ls_data-called_client
              AND bname = @ls_data-called_user
            INTO @ls_user.
          IF sy-subrc = 0.
            INSERT ls_user INTO lt_user INDEX sy-tabix.
          ENDIF.
        ENDIF.
        CHECK ls_user-ustyp IN sel_ustyp.
        CHECK ls_user-class IN sel_class.
        ls_data-ustyp = ls_user-ustyp.
        ls_data-class = ls_user-class.
      ENDIF.

      " Set color of function
      IF ls_data-id IS INITIAL.
        IF ls_data-actual_phase = 'A'.
          ls_data-ctab = VALUE #( ( fname = 'FUNCNAME' color-col = col_negative ) ). " red: blocked
        ELSE.
          ls_data-ctab = VALUE #( ( fname = 'FUNCNAME' color-col = col_total ) ).    " yellow: candidate for blocking
        ENDIF.
      ELSE.
        ls_data-ctab   = VALUE #( ( fname = 'FUNCNAME' color-col = col_positive ) ).   " green: callable
      ENDIF.

      APPEND ls_data TO lt_data.

    ENDLOOP.

  ENDMETHOD. " extend_data.

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
      lr_sorts            TYPE REF TO cl_salv_sorts,
      lr_aggregations     TYPE REF TO cl_salv_aggregations,
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
            t_table      = et_data ).

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
        lr_column->set_cell_type( ).

        lr_column ?= lr_columns->get_column( 'FMODE' ).
        lr_column->set_long_text( 'RFC Mode' ).
        lr_column->set_medium_text( 'RFC Mode' ).
        lr_column->set_short_text( 'RFC Mode' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'FUNC_TEXT' ).
        lr_column->set_long_text( 'Function text' ).
        lr_column->set_medium_text( 'Function text' ).
        lr_column->set_short_text( 'Function' ).

        lr_column ?= lr_columns->get_column( 'AREA' ).

        lr_column ?= lr_columns->get_column( 'AREA_TEXT' ).
        lr_column->set_long_text( 'Function group text' ).
        lr_column->set_medium_text( 'Function group text' ).
        lr_column->set_short_text( 'Func.Group' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'DEVCLASS' ).

        lr_column ?= lr_columns->get_column( 'DEVCLASS_TEXT' ).
        lr_column->set_long_text( 'Package text' ).
        lr_column->set_medium_text( 'Package' ).
        lr_column->set_short_text( 'Package' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'DLVUNIT' ).


        lr_column ?= lr_columns->get_column( 'ACTUAL_PHASE' ).
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

    " Footer (not visible on gui container)
    DATA l_line TYPE i.
    CREATE OBJECT lr_grid_footer.
    l_line = 1.

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
    lr_display_settings->set_list_header( 'Show extended UCON RFC data' ). "sy-title
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR layout.
  lcl_report=>at_selection_screen_f4_layout(
    CHANGING layout = layout
  ).

START-OF-SELECTION.
  lcl_report=>start_of_selection(
    EXPORTING
      ext_list     = p_comp     " Enhanced list

      sel_function = so_fumo[]  " Functions
      sel_area     = so_area[]  " Function groups
      sel_devclass = so_devc[]  " Packages
      sel_dlvunit  = so_unit[]  " Software component

      sel_bname    = so_bname[] " Users
      sel_ustyp    = so_ustyp[] " User types
      sel_class    = so_class[] " User groups

      p_mon        = p_mon      " Called Function Modules
      p_nmon       = p_nmon     " Uncalled Function Modules
      p_all        = p_all      " All Function Modules

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
