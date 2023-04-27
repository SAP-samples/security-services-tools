*&---------------------------------------------------------------------*
*& Report ZSHOW_TARGET_SYSTEM
*&---------------------------------------------------------------------*
*&
*& Show CSA target systems (policies)
*&
*& 09.03.2023 Initial version
*& 27.04.2023 Added button to call policy CSA management
*&---------------------------------------------------------------------*
REPORT zshow_target_system.

CONSTANTS c_program_version(30) TYPE c VALUE '27.04.2023 FQ4'.

TABLES sscrfields.

SELECTION-SCREEN: FUNCTION KEY 1. " Policy Management

* Target systems
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ss_tsys FOR FIELD s_tsys.
  SELECT-OPTIONS s_tsys FOR ('COVA_TARSYSID').
SELECTION-SCREEN END OF LINE.

* Target system description
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ss_tdesc FOR FIELD s_tdesc.
  SELECT-OPTIONS s_tdesc FOR ('COVA_TARDESC').
SELECTION-SCREEN END OF LINE.

* Show Details
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS p_item AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 3(30) ss_item FOR FIELD p_item.
SELECTION-SCREEN END OF LINE.

* Show XML
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS p_xml AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 3(30) ss_xml FOR FIELD p_xml.
SELECTION-SCREEN END OF LINE.

" Show SQL
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS p_sql AS CHECKBOX DEFAULT ' '.
  SELECTION-SCREEN COMMENT 3(30) ss_sql FOR FIELD p_sql.
SELECTION-SCREEN END OF LINE.

" Line size
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) ss_lsize FOR FIELD p_lsize.
  PARAMETERS p_lsize TYPE i DEFAULT 255.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_vers.


DATA g_tarsysid TYPE cova_tarsysid. " Global field for HIDE statement

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      initialization,

      at_selection_screen,

      f4_tsys,

      start_of_selection,

      line_selection.

  PRIVATE SECTION.

    CONSTANTS:

      version TYPE cova_tarversion VALUE '0000'.

ENDCLASS.                    "lcl_report DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    sy-title = 'Show target systems (policies)'(tit).

    DATA functxt TYPE smp_dyntxt.
    functxt-icon_id   = icon_wd_application.
    functxt-quickinfo = 'Configuration & Security Analysis - CSA Policy Management'.
    functxt-icon_text = 'Policy Management'.
    sscrfields-functxt_01 = functxt.

    ss_tsys   = 'Target system'.
    ss_tdesc  = 'Target system description'.
    ss_item   = 'Show Details'.
    ss_xml    = 'Show XML'.
    ss_sql    = 'Show SQL'.
    ss_lsize  = 'Line size of list'.

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

    ENDCASE.

  ENDMETHOD. "at_selection_screen

  METHOD f4_tsys.

    TYPES:
      BEGIN OF ts_f4_value,
        tarsysid   TYPE cova_tarsysid,
        tardesc    TYPE cova_tardesc,
        taruserxml TYPE cova_taruser,
      END OF ts_f4_value.

    DATA:
      f4_value     TYPE          ts_f4_value,
      f4_value_tab TYPE TABLE OF ts_f4_value.

    " Get data
    SELECT
        tarsysid,
        tardesc,
        taruserxml
      FROM cova_tarsysv_dbv     " View COVA_TARSYSV_DBV = merged tables COVA_TARSYSTEMS and COVA_TARSYSTEMS_SAP
      INTO TABLE @f4_value_tab
      WHERE tarversion = '0000' " current version (>= 9000 for historic versions)
      ORDER BY tarsysid.

    " Show value help
    DATA(progname) = sy-repid.
    DATA(dynnum)   = sy-dynnr.
    DATA field TYPE dynfnam.
    DATA stepl TYPE sy-stepl.
    GET CURSOR FIELD field LINE stepl.
    DATA return_tab TYPE TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TARSYSID'
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

  ENDMETHOD. " f4_TSYS

  METHOD start_of_selection.

    " Set line size
    IF p_lsize < 80.   p_lsize = 80.   ENDIF.
    IF p_lsize > 1023. p_lsize = 1023. ENDIF.
    NEW-PAGE LINE-SIZE p_lsize.

    FORMAT RESET.

    " Get target systems
    SELECT
        tarsysid,
        "tarversion,
        tardesc,
        tarsysxml,
        tarsyssql,
        taruserxml,
        tardatexml,
        tartimexml,
        tarusersql,
        tardatesql,
        tartimesql
        "tarversiondesc
      FROM cova_tarsysv_dbv             " View COVA_TARSYSV_DBV = merged tables COVA_TARSYSTEMS and COVA_TARSYSTEMS_SAP
      INTO TABLE @DATA(lt_cova_tarsysv)
      WHERE tarsysid   IN @s_tsys
        AND tardesc    IN @s_tdesc
        AND tarversion =  @version        " current version (>= 9000 for historic versions)
      ORDER BY tarsysid.

    " Get composite policies
    SELECT
        p~pol_id,
        p~child_pol_id,
        d~pol_desc
      FROM cova_cpol AS p
      LEFT JOIN cova_cpolh AS d
        ON  d~pol_id      = p~pol_id
        AND d~pol_version = p~pol_version
      WHERE p~child_pol_id      IN @s_tsys
        AND p~child_pol_version =  @version
        "AND p~pol_id            IN @s_polid
        AND p~pol_version       = @version
      ORDER BY p~pol_id, p~child_pol_id
      INTO TABLE @DATA(lt_cova_cpol)
      .

    LOOP AT lt_cova_tarsysv INTO DATA(ls_cova_tarsysv).
      g_tarsysid = ls_cova_tarsysv-tarsysid.

      " Authorization check
      DATA(l_rc) = cl_cova_target_sys_auth_check=>display(
        EXPORTING
          im_target_system  = ls_cova_tarsysv-tarsysid
          im_xml_user       = ls_cova_tarsysv-taruserxml
          im_chk_custnw     = abap_true ).
      IF l_rc NE 0.
        MESSAGE s022(cl_cova_wd_message). " You are not authorized
        CONTINUE.
      ENDIF.

      " Show header
      FORMAT COLOR COL_HEADING.
      DATA(len) = sy-linsz - 20 - 1 - 12 - 1 - 10 - 1 - 8 - 1.
      WRITE:
        /        ls_cova_tarsysv-tarsysid HOTSPOT,   " (20) Target system
                 ls_cova_tarsysv-taruserxml,         " (12) User
                 ls_cova_tarsysv-tardatexml,         " (10) Date
                 ls_cova_tarsysv-tartimexml,         " (8)  Time
        AT (len) ls_cova_tarsysv-tardesc.            "      Description
      HIDE g_tarsysid.
      FORMAT RESET.

      IF p_item = 'X' OR p_xml = 'X'.

        " Format XML
        cl_cova_util=>xstr_to_str(
          EXPORTING
            im_xstring = ls_cova_tarsysv-tarsysxml
          IMPORTING
            re_string  = DATA(xml_string)
            ex_rc      = DATA(rc)
            ex_msg     = DATA(msg)
            ).
        IF rc IS NOT INITIAL.
          WRITE / msg COLOR COL_NEGATIVE.
          CONTINUE.
        ENDIF.

      ENDIF.

      IF p_item = 'X'.

        FORMAT COLOR COL_NORMAL.

        " Show SQL Info
        DATA(sql_len) = strlen( ls_cova_tarsysv-tarsyssql ).
        WRITE:
          /(8) 'SQL Size:'.
        IF sql_len > 0.
          WRITE (11) sql_len.
        ELSE.
          WRITE (11) sql_len COLOR COL_NEGATIVE.
        ENDIF.
        IF        ls_cova_tarsysv-tardatesql > ls_cova_tarsysv-tardatexml
          OR (    ls_cova_tarsysv-tardatesql = ls_cova_tarsysv-tardatexml
              AND ls_cova_tarsysv-tartimesql >= ls_cova_tarsysv-tartimexml ).
          WRITE:
            ls_cova_tarsysv-tarusersql,
            ls_cova_tarsysv-tardatesql,
            ls_cova_tarsysv-tartimesql.
        ELSE.
          WRITE:
            ls_cova_tarsysv-tarusersql,
            ls_cova_tarsysv-tardatesql COLOR COL_NEGATIVE,
            ls_cova_tarsysv-tartimesql COLOR COL_NEGATIVE.
        ENDIF.
        WRITE:
          AT sy-linsz space.
        HIDE g_tarsysid.

        " Show composite profiles
        LOOP AT lt_cova_cpol INTO DATA(ls_cova_cpol)
          WHERE child_pol_id = ls_cova_tarsysv-tarsysid.

          WRITE:
            /(20) 'Composite policy:',
            (32)  ls_cova_cpol-pol_id,
            AT (len) ls_cova_cpol-pol_desc,
            AT sy-linsz space.
          HIDE g_tarsysid.
        ENDLOOP.
        IF sy-subrc IS NOT INITIAL.
          WRITE:
            /(20) 'Composite policy:',
            (32)  '<none>' COLOR COL_TOTAL,
            AT sy-linsz space.
          HIDE g_tarsysid.
        ENDIF.

        " Show Config stores (via some weird method calls instead of copying the code about call transformation targsystdef)
        DATA ls_target_system TYPE cl_cova_transform=>ts_target_system.
        DATA lr_cova_transform TYPE REF TO cl_cova_transform.
        IF lr_cova_transform IS NOT BOUND.
          CREATE OBJECT lr_cova_transform.
        ENDIF.
        lr_cova_transform->get_multisql(
          EXPORTING
            im_xml_string = xml_string
          IMPORTING
            ex_multisql = DATA(ex_multisql)
            ex_rc = DATA(ex_rc)
            ex_msg = DATA(ex_msg)
        ).
        IF ex_rc IS INITIAL.
          ls_target_system = lr_cova_transform->get_policy_as_abap( ).

          "sort ls_target_system-configstores by name.
          len = sy-linsz - 20 - 1 - 60 - 1 - 20 - 1.
          LOOP AT ls_target_system-configstores INTO DATA(ls_configstore).
            LOOP AT ls_configstore-checkitems INTO DATA(ls_checkitem).

              WRITE:
                /(20)       'Check item:',
                (60)        ls_configstore-name, " type char 250 (60 seems to be sufficent)
                (20)        ls_checkitem-id,     " type char 20
                AT (len)    ls_checkitem-desc,   " type char 250
                AT sy-linsz space.
              HIDE g_tarsysid.

              if ls_checkitem-JOIN_ITEMS is not initial.
                loop at ls_checkitem-JOIN_ITEMS into data(ls_JOIN_ITEM).
                  write:
                    /       '      join:',
                    (60)   ls_JOIN_ITEM-name under ls_configstore-name,
                    AT sy-linsz space.
                  HIDE g_tarsysid.
                endloop.
              endif.

            ENDLOOP.
          ENDLOOP.

        ENDIF.


        FORMAT RESET.
        SKIP.

      ENDIF.

      IF p_xml = 'X'.

        " Replace CR/LF and TAB
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN xml_string WITH cl_abap_char_utilities=>newline.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN xml_string WITH `    `.

        " Show formatted XML
        SPLIT xml_string AT cl_abap_char_utilities=>newline INTO TABLE DATA(xml_tab).
        LOOP AT xml_tab INTO DATA(line).
          DO.
            IF strlen( line ) > 0.
              WRITE / line FRAMES OFF.
              HIDE g_tarsysid.
              SHIFT line BY sy-linsz PLACES.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.
        ENDLOOP.

        SKIP.

      ENDIF. " p_xml = 'X'

      IF p_sql = 'X'.

        " Generate SQL
        "data lr_transform type ref to cl_cova_transform.
        "create object lr_transform.
        "lr_transform->CREATE_TARGET_SQL(
        "  EXPORTING
        "    im_xml_string = xml_string
        "  IMPORTING
        "    ex_sql_string = data(sql_string)
        "    ex_rc         = rc
        "    ex_msg        = msg
        "    ).

        " Format SQL by adding line breaks
        DATA(sql_string) = ls_cova_tarsysv-tarsyssql.
        "cl_cova_util=>cutting_sql_stmnt( changing ch_stmnt = sql_string ).

        " Let's try it by our own
        DATA(nl) = cl_abap_char_utilities=>newline.
        REPLACE ALL OCCURRENCES OF `/*`  IN sql_string WITH         nl && `/*`.
        REPLACE ALL OCCURRENCES OF `*/(` IN sql_string WITH `*/` && nl && `(`.
        REPLACE ALL OCCURRENCES OF REGEX ` ((with|select|from|inner join|left join|minus|union all|where|WHERE|and|AND|or|OR) )` IN sql_string WITH nl && `$1`.
        REPLACE ALL OCCURRENCES OF `,`   IN sql_string WITH `,`  && nl.
        "REPLACE ALL OCCURRENCES OF `(`  IN sql_string WITH `(`  && nl.
        REPLACE ALL OCCURRENCES OF `)`   IN sql_string WITH         nl && `)`.

        " Show formatted SQL
        SPLIT sql_string AT cl_abap_char_utilities=>newline INTO TABLE DATA(sql_tab).
        DATA(quote)   = 0. " Count of quotes
        DATA(pos)     = 1. " Position of idention
        DATA(printed) = 0. " Length of already printed text in the line
        LOOP AT sql_tab INTO line.

          IF quote MOD 2 = 0 OR strlen( line ) > sy-linsz - pos - printed.
            " Matching quotes or long line
            NEW-LINE.
            WRITE AT pos line FRAMES OFF NO-GAP INTENSIFIED.
            HIDE g_tarsysid.
            printed = strlen( line ).
            DATA(offset) = sy-linsz - pos.

            " More lines
            DO.
              IF strlen( line ) > offset.
                NEW-LINE.
                WRITE AT pos line+offset FRAMES OFF NO-GAP INTENSIFIED.
                HIDE g_tarsysid.
                printed = strlen( line+offset ).
                offset = offset + sy-linsz - pos.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.

          ELSE.
            " Uneven quotes
            WRITE line FRAMES OFF NO-GAP INTENSIFIED. " Continue the line
            printed = printed + strlen( line ).
          ENDIF.

          " Count quotes
          FIND ALL OCCURRENCES OF `'` IN line MATCH COUNT DATA(q).
          ADD q TO quote.

          " calculate indention for next line
          FIND ALL OCCURRENCES OF '(' IN line MATCH COUNT DATA(open).
          FIND ALL OCCURRENCES OF ')' IN line MATCH COUNT DATA(close).
          pos = pos + 2 * open - 2 * close.
          IF pos < 1.  pos = 1.  ENDIF.
          IF pos > 41. pos = 41. ENDIF. " max indention 20 levels
        ENDLOOP.

        SKIP.

      ENDIF. " p_sql = 'X'

    ENDLOOP.

  ENDMETHOD. " start_of_selection

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

ENDCLASS.                    "lcl_report IMPLEMENTATION

*----------------------------------------------------------------------*
*      REPORT events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization( ).

AT SELECTION-SCREEN.
  lcl_report=>at_selection_screen( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tsys-low.
  lcl_report=>f4_tsys( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tsys-high.
  lcl_report=>f4_tsys( ).

START-OF-SELECTION.
  lcl_report=>start_of_selection( ).

AT LINE-SELECTION.
  lcl_report=>line_selection( ).
