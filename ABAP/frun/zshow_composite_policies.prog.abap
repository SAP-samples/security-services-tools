*&---------------------------------------------------------------------*
*& Report ZSHOW_COMPOSITE_POLICIES
*&---------------------------------------------------------------------*
*&
*& Show composite policies
*&
*&---------------------------------------------------------------------*
REPORT zshow_composite_policies.

CONSTANTS c_program_version(30) TYPE c VALUE '27.04.2023 FQ4'.

TABLES sscrfields.

SELECTION-SCREEN: FUNCTION KEY 1. " Policy Management
SELECTION-SCREEN: FUNCTION KEY 2. " Composite Policy Management

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ss_tsys FOR FIELD s_tsys.
  SELECT-OPTIONS s_tsys FOR ('COVA_TARSYSID').
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS s_root AS CHECKBOX DEFAULT abap_true.
  SELECTION-SCREEN COMMENT 3(30) ss_root FOR FIELD s_root.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

TYPES:
  BEGIN OF ts_pol_id,
    pol_id TYPE cova_tarsysid,
  END OF ts_pol_id.

DATA g_tarsysid TYPE cova_tarsysid. " Global field for HIDE statement

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      initialization,

      at_selection_screen,

      start_of_selection,

      line_selection.

  PRIVATE SECTION.

    CLASS-DATA:

      lt_cova_cpolh TYPE TABLE OF cova_cpolh,
      lt_cova_cpol  TYPE TABLE OF cova_cpol,
      lt_pol_id     TYPE TABLE OF ts_pol_id.

    CLASS-METHODS:

      write_composite_policies,

      write_composite_policy
        IMPORTING
          pol_id      TYPE cova_cpolh-pol_id
          pol_version TYPE cova_cpolh-pol_version
          level       TYPE i.

ENDCLASS.                    "lcl_report DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    sy-title = 'Show composite policies'(tit).

    DATA functxt TYPE smp_dyntxt.
    functxt-icon_id   = icon_wd_application.
    functxt-quickinfo = 'Configuration & Security Analysis - CSA Policy Management'.
    functxt-icon_text = 'Policy Management'.
    sscrfields-functxt_01 = functxt.

    functxt-icon_id   = icon_wd_application.
    functxt-quickinfo = 'Configuration & Security Analysis - CSA Composite Policy Management'.
    functxt-icon_text = 'Composite Policy Management'.
    sscrfields-functxt_02 = functxt.

    ss_tsys   = 'Composite policy'.
    ss_root   = 'Root policies only'.

    CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
       SEPARATED BY space.

  ENDMETHOD. " initialization

  METHOD at_selection_screen.

    CASE sscrfields-ucomm.

      WHEN 'FC01'.
        " CSA Policy Management
        " https://ldcifq4.wdf.sap.corp:44390/sap/bc/ui5_ui5/sap/csa_policy_mgm/index.html?sap-client=200&sap-language=EN&#/PolicyMaint

        DATA(lo_start_url) = NEW /ui2/cl_start_url( '/sap/bc/ui5_ui5/sap/csa_policy_mgm/index.html' ).

        lo_start_url->add_sap_params( ). " add sap params "sy-mandt", "sap-language"
        lo_start_url->add_url_param( '#/PolicyMaint' ).

        " start in browser
        lo_start_url->start_browser( ).

      WHEN 'FC02'.
        " CSA Policy Management
        " https://ldcifq4.wdf.sap.corp:44390/sap/bc/ui5_ui5/sap/csa_policy_mgm/index.html?sap-client=200&sap-language=EN&#/CompositePol

        lo_start_url = NEW /ui2/cl_start_url( '/sap/bc/ui5_ui5/sap/csa_policy_mgm/index.html' ).

        lo_start_url->add_sap_params( ). " add sap params "sy-mandt", "sap-language"
        lo_start_url->add_url_param( '#/CompositePol' ).

        " start in browser
        lo_start_url->start_browser( ).

    ENDCASE.

  ENDMETHOD. "at_selection_screen

  METHOD start_of_selection.

    " Get composite policies
    SELECT * FROM cova_cpolh
      INTO TABLE @lt_cova_cpolh
      ORDER BY PRIMARY KEY.

    " Get childs of composite policies
    SELECT * FROM cova_cpol
      INTO TABLE @lt_cova_cpol
      ORDER BY PRIMARY KEY.

    write_composite_policies( ).

  ENDMETHOD. "start_of_selection

  METHOD line_selection.
    CLEAR g_tarsysid.
    READ CURRENT LINE.
    CHECK g_tarsysid IS NOT INITIAL.

    MESSAGE s000(01) WITH `Show policy ` && g_tarsysid && ` in new browser window`.

    " https://ldcifq4.wdf.sap.corp:44390/sap/bc/ui5_ui5/sap/csa_policy_mgm/index.html?sap-client=200&sap-language=EN&#/PolicyDefinition/BL22_CRITAU-A/0000

    DATA(lo_start_url) = NEW /ui2/cl_start_url( `/sap/bc/ui5_ui5/sap/csa_policy_mgm/index.html` ).

    lo_start_url->add_sap_params( ). " add sap params "sy-mandt", "sap-language"
    lo_start_url->add_url_param( `#/PolicyDefinition/` && g_tarsysid && `/0000` ).

    " start in browser
    lo_start_url->start_browser( ).
  ENDMETHOD. " line_selection


  METHOD write_composite_policies.

    FORMAT RESET.

    LOOP AT lt_cova_cpolh INTO DATA(ls_cova_cpolh)
      WHERE pol_id IN s_tsys.

      " Check for root entries
      IF s_root = abap_true.
        READ TABLE lt_cova_cpol TRANSPORTING NO FIELDS
          WITH KEY
            child_pol_id      = ls_cova_cpolh-pol_id
            child_pol_version = ls_cova_cpolh-pol_version.
        CHECK sy-subrc NE 0.
      ENDIF.

      " Clear recursion protection
      CLEAR lt_pol_id.

      " Show composite policy
      write_composite_policy(
        EXPORTING
          pol_id      = ls_cova_cpolh-pol_id
          pol_version = ls_cova_cpolh-pol_version
          level       = 0 " initial level
      ).

    ENDLOOP.

  ENDMETHOD. " write_composite_policies

  METHOD write_composite_policy.
    "IMPORTING
    "  pol_id      TYPE cova_cpolh-pol_id
    "  pol_version TYPE cova_cpolh-pol_version
    "  level       TYPE i.

    " Recursion protection
    READ TABLE lt_pol_id TRANSPORTING NO FIELDS
      WITH KEY pol_id = pol_id.
    IF sy-subrc = 0.
      EXIT.
    ENDIF.
    APPEND pol_id TO lt_pol_id.

    DATA(indent) = level * 2.

    " Get description
    READ TABLE lt_cova_cpolh INTO DATA(ls_cova_cpolh)
          WITH KEY
            pol_id      = pol_id
            pol_version = pol_version.
    IF sy-subrc NE 0.
      CLEAR ls_cova_cpolh.
    ENDIF.

    NEW-LINE.
    WRITE: AT indent pol_id          COLOR COL_KEY,
           50 ls_cova_cpolh-pol_desc COLOR COL_NORMAL.

    indent = indent + 2. " Shift single policies to the right
    LOOP AT lt_cova_cpol INTO DATA(ls_cova_cpol)
      WHERE pol_id      = pol_id
        AND pol_version = pol_version.

      CASE ls_cova_cpol-child_pol_type.

        WHEN 'S'. " Single policy

          SELECT SINGLE tardesc
            FROM cova_tarsysv_dbv
            INTO @DATA(tardesc)
            WHERE tarsysid   = @ls_cova_cpol-child_pol_id
              AND tarversion = '0000' " current version (>= 9000 for historic versions)
              .
          IF sy-subrc NE 0.
            CLEAR tardesc.
          ENDIF.

          g_tarsysid = ls_cova_cpol-child_pol_id.

          NEW-LINE.
          WRITE:
            AT indent ls_cova_cpol-child_pol_id HOTSPOT,
            50        tardesc.
          HIDE g_tarsysid.

        WHEN 'C'. " Composite policy
          IF level > 10.
            WRITE: / 'nesting error' COLOR COL_NEGATIVE.
            EXIT.
          ENDIF.

          DATA(next_level) = level + 1.
          write_composite_policy(
            EXPORTING
              pol_id = ls_cova_cpol-child_pol_id
              pol_version = ls_cova_cpol-child_pol_version
              level = next_level
          ).

        WHEN OTHERS.
          WRITE: / 'unknown type' COLOR COL_NEGATIVE.
          EXIT.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD. " write_composite_policy

ENDCLASS.                    "lcl_report IMPLEMENTATION


*----------------------------------------------------------------------*
*      REPORT events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization( ).

AT SELECTION-SCREEN.
  lcl_report=>at_selection_screen( ).

START-OF-SELECTION.
  lcl_report=>start_of_selection( ).

AT LINE-SELECTION.
  lcl_report=>line_selection( ).
