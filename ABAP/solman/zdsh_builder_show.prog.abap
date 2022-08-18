*&---------------------------------------------------------------------*
*& Report  ZDSH_BUILDER_SHOW
*& Edit Dashboard Builder definition
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*&
*& 16.01.2018 Initial version
*& 16.08.2018 Variant SEC_BASELINE extended
*& 19.10.2020 Adjusted data type for formal parameter of form routine
*& 02.02.2021 Interactive option to move item to another parent id (inactive, see comment ### - use with extreme caution!)
*& 04.02.2021 New option to show keys only
*& 12.02.2021 Rearrange columns to show texts in one line
*& 19.02.2021 Add some texts
*& to do:
*& Check authorization for SM_DSHO not only for dashboard but for category, too.
*&---------------------------------------------------------------------*
REPORT zdsh_builder_edit
  LINE-SIZE 1023.

CONSTANTS: c_program_version(10) TYPE c VALUE '19.02.2020'.

* Dashboard item                                              Transport key                 Element type  Table
* Dashboard category: Security Baseline                       R3TR  DSHE  YCP54M5B614VV681  CAT           AGS_DSH_CAT
*   Dashboard: Security Baseline ABAP                         R3TR  DSHE  ZC60Q0QER8T6511Q  DSH           AGS_DSH_HEADER
*     Group: System Configuration                             R3TR  DSHE  YZ587XO6J7YEP28O  KPI           AGS_KPI_CONF
*       Tile: Password Policy                                 R3TR  DSHE  YQ3R6Y8U5M83DA6A  KPI           AGS_KPI_CONF
*         Drilldown view: Password Policy (Overview)          R3TR  DSHE  YP95L70GT14B441K  DRV           AGS_KPI_DRILLDOW
*         Drilldown view: Password Policy (Details)           R3TR  DSHE  ZQ9PFT0SW08EY1W9  DRV           AGS_KPI_DRILLDOW
*       Tile: Standard Users                                  R3TR  DSHE  YK7S0802GCZ4852G  KPI           AGS_KPI_CONF
*         Drilldown view: Standard Users (Overview)           R3TR  DSHE  YB907K0L6068696O  DRV           AGS_KPI_DRILLDOW
*         Drilldown view: Standard Users (User)               R3TR  DSHE  YG64J8H63OZQVN1A  DRV           AGS_KPI_DRILLDOW
*         Drilldown view: Standard Users (Profile Parameter)  R3TR  DSHE  YXO19I67SW3J45FI  DRV           AGS_KPI_DRILLDOW
* ...

DATA:

* All

  ls_tadir            TYPE tadir,            "(X) Directory of Repository Objects

  ls_ags_dsh_elt_dir  TYPE ags_dsh_elt_dir,  "(X) Dashboard element directory
  lt_ags_dsh_elt_dir  TYPE TABLE OF ags_dsh_elt_dir,  "(X) Dashboard element directory

* Category

  ls_ags_dsh_cat      TYPE ags_dsh_cat,      "(X) solman dashboard category
  ls_ags_dsh_cat_txt  TYPE ags_dsh_cat_txt,  "(X) text for dashboard category

* Dashboard

  ls_ags_dsh_header   TYPE ags_dsh_header,   "(X) dashboard header table, incl. CATEGORY
  ls_ags_dsh_head_txt TYPE ags_dsh_head_txt, "(X) dashboard header text table
  ls_ags_dsh_gflt_hd  TYPE ags_dsh_gflt_hd,  "(X) dashboard global filter header

* Group (KPI_KIND = KPG) / KPI Item

  ls_ags_kpi_conf     TYPE ags_kpi_conf,     "(X) dashboard KPI defintion, incl. PARENT_ID of dashboard
  ls_ags_kpi_conf_txt TYPE ags_kpi_conf_txt, "(X) dashboard KPI defintion text table, incl. PARENT_ID of dashboard
  ls_ags_dsh_kfg_conf TYPE ags_dsh_kfg_conf, "(X) kpi data source setting key figures for columns
  ls_ags_dsh_flt_conf TYPE ags_dsh_flt_conf, "(X) kpi data source setting filter
  ls_ags_dsh_ds_conf  TYPE ags_dsh_ds_conf,  "(X) data sources for tile/drilldown chart configuration
  ls_dsh_kpi_thres_hd TYPE dsh_kpi_thres_hd, "(X) kpi threshold defintion

* Drilldown

  ls_ags_kpi_drilldow TYPE ags_kpi_drilldow, "(X) KPI drilldown definition, incl. PARENT_ID of KPI item
  ls_ags_dsh_dril_txt TYPE ags_dsh_dril_txt, "(X) text table for AGS_DSH_DRIL_TXT, incl. PARENT_ID of KPI item
  ls_ags_dsh_dim_conf TYPE ags_dsh_dim_conf, "(X) rows: selected characteristics
*  LS_AGS_DSH_FLT_CONF type AGS_DSH_FLT_CONF, "(X) kpi data source setting filter

* Other tables (not used by Security Baseline Template)

  ls_ags_dsh_condit   TYPE ags_dsh_condit,   "condition setting

  ls_ags_dsh_ds_flt   TYPE ags_dsh_ds_flt,   "data source level predefined filters(like monid/sid/client)

  ls_ags_dsh_gflt_lst TYPE ags_dsh_gflt_lst, "global filter lastset table
  ls_ags_dsh_gflt_map TYPE ags_dsh_gflt_map, "global filter mapping table
  ls_ags_dsh_gflt_opr TYPE ags_dsh_gflt_opr, "global filter available operator(EQ;NE;BT)

  ls_ags_kpi_to_appl  TYPE ags_kpi_to_appl,  "jump to url for drilldown table

  ls_dsh_kpi_threstxt TYPE dsh_kpi_threstxt, "kpi threshold defintion text table
  ls_ags_dsh_thre_dim TYPE ags_dsh_thre_dim, "threshold setting: characteristic values (header)

  ls_ags_monty_conf   TYPE ags_monty_conf,   "BPA key figure monetary value configuration

  ls_ags_std_kpi_info TYPE ags_std_kpi_info, "dashboard KPI defintion

  ls_dsh_semantic_set TYPE dsh_semantic_set, "semantics setting for dashboard charactertics
  ls_dsh_sematic_set2 TYPE dsh_sematic_set2, "semantics setting for dashboard charactertics

  ls_dsh_time_flt_rol TYPE dsh_time_flt_rol  "kpi data source setting time filter rolling pattern
  .


DATA:
  posid   TYPE i VALUE 19,  " Line position of ID = lenName + 1
  posdata TYPE i VALUE 39,  " Line position of data = posID + 16 + 1 + 2 + 1
  postext TYPE i VALUE 110. " Line position of text

*&---------------------------------------------------------------------*

TABLES sscrfields.
SELECTION-SCREEN: FUNCTION KEY 1. "Dashboard Builder

SELECTION-SCREEN BEGIN OF BLOCK dsh WITH FRAME TITLE text001.

* Categories DEFTP = CAT
DATA s_catid LIKE ls_ags_dsh_elt_dir-eltuid.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_catid FOR FIELD scatid.
SELECT-OPTIONS scatid  FOR s_catid MATCHCODE OBJECT catid_hlp
  DEFAULT 'YCP54M5B614VV681' "Security Baseline
  .
SELECTION-SCREEN END OF LINE.

* Dashboards DEFTP = DSH
DATA s_dshid LIKE ls_ags_dsh_elt_dir-eltuid.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_dshid FOR FIELD sdshid.
SELECT-OPTIONS sdshid  FOR s_catid MATCHCODE OBJECT dshid_hlp.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS pkeys AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 5(28) ss_keys FOR FIELD pkeys.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK dsh.

SELECTION-SCREEN BEGIN OF BLOCK tech WITH FRAME TITLE text002.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) ss_ptech FOR FIELD ptech.
PARAMETERS ptech AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

* Items
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_id FOR FIELD seltuid.
SELECT-OPTIONS seltuid FOR ls_ags_dsh_elt_dir-eltuid.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK tech.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(60) ss_vers.

*&---------------------------------------------------------------------*

INITIALIZATION.

  DATA: functxt TYPE smp_dyntxt.

  functxt-icon_id   = icon_wd_application.
  functxt-quickinfo = 'Dashboard Builder'(001).
  functxt-icon_text = 'Builder'(002).
  sscrfields-functxt_01 = functxt.

  text001   = 'Dashboard Builder'(001).

  ss_catid  = 'Dashboard Category'(003).
* 'YCP54M5B614VV681' "Security Baseline

  ss_dshid  = 'Dashboard'(004).
* 'ZC60Q0QER8T6511Q' "Security Baseline ABAP
* 'YXXL1O2PPW5N18FS' "Security Baseline Critical Authorizations
* 'YF69KB598Z3R1DXW' "Security Baseline JAVA
* 'YG9ZNW0KGG6U57E0' "Security Notes ABAP with Configuration

  text002   = 'Technical view'(005).
  ss_id     = 'Item'(006).
  ss_ptech  = 'Technical view'(005).

  ss_keys   = 'Show keys only'.

  CONCATENATE 'Program version:'(VER) c_program_version INTO ss_vers
    SEPARATED BY space.

  CONCATENATE 'Show Dashboard Definition'(010)
              '(' 'Version'(023) c_program_version ')'
    INTO sy-title SEPARATED BY space.

*&---------------------------------------------------------------------*

AT SELECTION-SCREEN.


  CASE sscrfields-ucomm.

    WHEN 'FC01'.
*     Dashboard Builder
      DATA: lv_url TYPE string.
*      call function 'AGS_DSH_GET_BUILDER_URL'
*        IMPORTING
*          EV_URL        = lv_URL
*          .
*data: lv_url type string.
      CALL METHOD cl_wd_utilities=>construct_wd_url
        EXPORTING
          application_name    = 'AGS_DSH_ISLAND_TILES'  " Application
*         IN_HOST             =     " Requested Host if Not Own
*         IN_PORT             =     " Requested Port Number
*         IN_PROTOCOL         =     " Requested Log
*         IN_PARAMETERS       =     " Additional parameters
*         IN_CLIENT           =     " Requested Client ('csf', 'XmlClient', ...)
*         IN_FORWARD_ACCESSIBILITY_FLAG = ABAP_TRUE    " Forward the Accessibility Flag if Required
          in_forward_language = abap_true    " Forwarding Logon Language
          in_forward_client   = abap_true    " Forwarding Client
*         IN_FORWARD_THEME    =     " Forwarding of sap-theme parameter
*         NAMESPACE           = 'sap'    " Namespace
*         IN_SERVER           =     " HTTP Framework (iHTTP) HTTP Server
        IMPORTING
*         OUT_HOST            =     " Host Name
*         OUT_PORT            =     " Port Number
*         OUT_PROTOCOL        =     " Log
*         OUT_LOCAL_URL       =     " URL (Relative to the Current Server)
          out_absolute_url    = lv_url  " Absolute URL (Incl. Log, Host, Port)
*         VH_SWITCH           =
        .


      PERFORM call_url
        USING lv_url.

  ENDCASE.


FORM call_url
  USING lv_url         TYPE string.

* URL generation see report /UI2/START_URL
* <https>://<server>:<port>/sap/bc/ui5_ui5/ui2/ushell/shells/abap/FioriLaunchpad.html<action>?sap-client=<client>

**********************************************************************
* create entries in table HTTPURLLOC for the below application string.
* these entries have to point to the reverse proxy (e.g. SAP WebDispatcher).
**********************************************************************

* single sign-on
  CONSTANTS lc_icf_url TYPE string VALUE '/sap/public/myssocntl'. "#EC SYNTCHAR
  DATA lv_sso_active   TYPE abap_bool.
  CALL METHOD cl_icf_tree=>if_icf_tree~service_from_url
    EXPORTING
      url                   = lc_icf_url
      hostnumber            = 0
      authority_check       = abap_false
    IMPORTING
      icfactive             = lv_sso_active
    EXCEPTIONS
      wrong_application     = 1
      no_application        = 2
      not_allow_application = 3
      wrong_url             = 4
      no_authority          = 4
      OTHERS                = 5.
  IF sy-subrc NE 0.
    lv_sso_active = abap_false.
  ENDIF.
*    if lv_sso_active eq abap_false.
*      message e027(bsp_wd) with lc_icf_url.
*    endif.

  DATA lv_urlc TYPE c LENGTH 1024.
  lv_urlc = lv_url.

* start browser with single sign-on
  IF lv_sso_active = abap_true.
    DATA lv_container TYPE REF TO cl_gui_container.         "#EC NEEDED
    DATA lv_viewer    TYPE REF TO cl_gui_html_viewer.

    CREATE OBJECT lv_viewer
      EXPORTING
        parent             = lv_container
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE 'E'.
    ENDIF.

    CALL METHOD lv_viewer->enable_sapsso
      EXPORTING
        enabled    = abap_true
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE 'E'.
    ENDIF.

    CALL METHOD lv_viewer->detach_url_in_browser
      EXPORTING
        url        = lv_urlc
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE 'E'.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2
        OTHERS            = 3.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE 'E'.
    ENDIF.

* start browser without single-sign-on
  ELSE.
    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url                    = lv_urlc
*       WINDOW_NAME            = ' '
        new_window             = abap_true
      EXCEPTIONS
        frontend_not_supported = 1
        frontend_error         = 2
        prog_not_found         = 3
        no_batch               = 4
        unspecified_error      = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM. " CALL_URL

*&---------------------------------------------------------------------*

* Table AGS_DSH_ELT_DIR
* ELTUID  Key
* DEFTP   Type
* MAPNAME Short text

AT SELECTION-SCREEN ON VALUE-REQUEST FOR seltuid-low.
  PERFORM f4_seltuid USING 'SELTUID'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR seltuid-high.
  PERFORM f4_seltuid USING 'SELTUID'.

*
FORM f4_seltuid USING l_dynprofield  TYPE help_info-dynprofld.

  DATA: progname      TYPE sy-repid,
        dynnum        TYPE sy-dynnr,
        dynpro_values TYPE TABLE OF dynpread,
        field_value   LIKE LINE OF dynpro_values,
        field_tab     TYPE TABLE OF dfies  WITH HEADER LINE.

  STATICS:
        value_tab     LIKE ags_dsh_elt_dir OCCURS 0.

  DATA: ls_system_info   TYPE ags_sr_s_lmdb_system.

  progname = sy-repid.
  dynnum   = sy-dynnr.

  IF value_tab[] IS INITIAL.
    SELECT * FROM ags_dsh_elt_dir
      INTO TABLE value_tab
      WHERE deftp NE 'CAT'
        AND deftp NE 'DSH'
      ORDER BY deftp eltuid.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      ddic_structure  = 'AGS_DSH_ELT_DIR'
      retfield        = 'ELTUID'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = l_dynprofield
      value_org       = 'S'
*     MULTIPLE_CHOICE = 'X'
    TABLES
*     field_tab       = field_tab
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                                                    "F4_SELDEFT

*&---------------------------------------------------------------------*

START-OF-SELECTION.
  CONCATENATE 'Show Dashboard Definition'(010)
              '(' 'Version'(023) c_program_version ')'
    INTO sy-title SEPARATED BY space.

* Check authority (simplified)
  AUTHORITY-CHECK OBJECT 'SM_DSHO'
           ID 'ACTVT' FIELD '03'
           ID 'DSHID' DUMMY
           ID 'CATID' DUMMY.
  IF sy-subrc <> 0.
*  Missing authority &1: ACTVT = &2 / DSHID = &3 / CATID = &4
    MESSAGE e002(dsh_builder) WITH 'SM_DSHO' '03 (display)' '' ''.
  ENDIF.

* Get DDIC texts for KPI rendering type
  DATA: ls_kpi_rendering_type_text TYPE dd07v,
        lt_kpi_rendering_type_text TYPE TABLE OF dd07v.
  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      name          = 'DSH_RENDERING_TYPE'
*     LANGU         = SY-LANGU
      texts_only    = 'X'
    TABLES
      dd07v_tab     = lt_kpi_rendering_type_text
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

* Get DDIC texts for drilldown rendering type
  DATA: ls_drill_rendering_type_text TYPE dd07v,
        lt_drill_rendering_type_text TYPE TABLE OF dd07v.
  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      name          = 'DSH_DRILL_RENDER_TYPE'
*     LANGU         = SY-LANGU
      texts_only    = 'X'
    TABLES
      dd07v_tab     = lt_drill_rendering_type_text
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



  FORMAT RESET.
* Dashboard element directory

  IF ptech IS INITIAL.
    PERFORM write_dashboards.
  ENDIF.
  CHECK ptech = 'X'.

* 1. Category
  IF scatid[] IS NOT INITIAL.
    SELECT * FROM ags_dsh_elt_dir INTO TABLE lt_ags_dsh_elt_dir
      WHERE eltuid IN scatid
        AND deftp = 'CAT'.
    LOOP AT lt_ags_dsh_elt_dir INTO ls_ags_dsh_elt_dir.
      PERFORM write_element.
    ENDLOOP.
    IF sy-subrc = 0.
      SKIP.
    ENDIF.
  ENDIF.

* 2. Dashboard
  IF sdshid[] IS NOT INITIAL.
    SELECT * FROM ags_dsh_elt_dir INTO TABLE lt_ags_dsh_elt_dir
      WHERE eltuid IN sdshid
        AND deftp = 'DSH'.
    LOOP AT lt_ags_dsh_elt_dir INTO ls_ags_dsh_elt_dir.
      PERFORM write_element.
    ENDLOOP.
    IF sy-subrc = 0.
      SKIP.
    ENDIF.
  ENDIF.

* Other items
  IF seltuid[] IS NOT INITIAL.
    SELECT * FROM ags_dsh_elt_dir INTO TABLE lt_ags_dsh_elt_dir
      WHERE eltuid IN seltuid.

*   3. Group
*   Groups are KPIs having an entry in field AGS_KPI_CONF-KPI_ID with AGS_KPI_CONF-KPI_KIND = 'KGP'
*   4.1 KPI
    LOOP AT lt_ags_dsh_elt_dir INTO ls_ags_dsh_elt_dir
      WHERE deftp = 'KPI'.
      DATA: ls_ags_kpi_conf_temp TYPE ags_kpi_conf.
      SELECT SINGLE * FROM ags_kpi_conf INTO ls_ags_kpi_conf_temp
        WHERE kpi_id   = ls_ags_dsh_elt_dir-eltuid
          AND kpi_kind = 'KGP'
        .
      IF sy-subrc = 0.
        PERFORM write_element.
        WRITE: / 'Group'(007).
        DELETE lt_ags_dsh_elt_dir.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0.
      SKIP.
    ENDIF.

*   4.1 KPI
    LOOP AT lt_ags_dsh_elt_dir INTO ls_ags_dsh_elt_dir
      WHERE deftp = 'KPI'.
      PERFORM write_element.
    ENDLOOP.
    IF sy-subrc = 0.
      SKIP.
    ENDIF.

*   4.2 KPI Child
    LOOP AT lt_ags_dsh_elt_dir INTO ls_ags_dsh_elt_dir
      WHERE deftp = 'CHD'.
      PERFORM write_element.
    ENDLOOP.
    IF sy-subrc = 0.
      SKIP.
    ENDIF.

*   4.3 KPI Drilldown view
    LOOP AT lt_ags_dsh_elt_dir INTO ls_ags_dsh_elt_dir
      WHERE deftp = 'DRV'.
      PERFORM write_element.
    ENDLOOP.
    IF sy-subrc = 0.
      SKIP.
    ENDIF.

  ENDIF.

*&---------------------------------------------------------------------*

FORM write_element.

  CHECK ptech = 'X'.

*----------------------------------------------------------------------

* Directory of Repository Objects
  DATA: ls_tadir TYPE tadir.
  CLEAR ls_tadir.
  SELECT SINGLE * FROM tadir INTO ls_tadir
    WHERE pgmid    = 'R3TR'
      AND object   = 'DSHE'
      AND obj_name =  ls_ags_dsh_elt_dir-eltuid.

  FORMAT COLOR COL_HEADING.
  WRITE: /            'Dashboard element'(008),
           AT posid   ls_ags_dsh_elt_dir-eltuid INTENSIFIED,
           AT posdata ls_ags_dsh_elt_dir-deftp.
  CASE ls_ags_dsh_elt_dir-deftp.
    WHEN 'CAT'.  WRITE (15) 'Category'(009).
    WHEN 'DSH'.  WRITE (15) 'Dashboard'(004).
    WHEN 'KPI'.  WRITE (15) 'Key perf. index'(011).
    WHEN 'CHD'.  WRITE (15) 'Child KPI'(012).
    WHEN 'DRV'.  WRITE (15) 'Drilldown view'(013).
    WHEN OTHERS. WRITE (15) ls_ags_dsh_elt_dir-deftp COLOR COL_NEGATIVE.
  ENDCASE.
  WRITE:    ls_ags_dsh_elt_dir-mapname,
            (2) space. " no icon here

  WRITE:    ls_tadir-devclass,
            ls_tadir-author.

  WRITE: AT sy-linsz space.
  FORMAT RESET.

*----------------------------------------------------------------------

* solman dashboard category
  SELECT * FROM ags_dsh_cat INTO ls_ags_dsh_cat
    WHERE id = ls_ags_dsh_elt_dir-eltuid.

    WRITE: /            'AGS_DSH_CAT',
           AT posid(16) ls_ags_dsh_cat-namespace,
           (2)          space, " no icon here
           AT posdata   ls_ags_dsh_cat-created_by,
                        ls_ags_dsh_cat-created_at DD/MM/YYYY.

*   text for dashboard category
    SELECT SINGLE * FROM ags_dsh_cat_txt INTO ls_ags_dsh_cat_txt
      WHERE id = ls_ags_dsh_cat-id
        AND lang = sy-langu.
    IF sy-subrc NE 0 AND sy-langu NE 'E'.
      SELECT SINGLE * FROM ags_dsh_cat_txt INTO ls_ags_dsh_cat_txt
        WHERE id   = ls_ags_dsh_cat-id
          AND lang = 'E'.
    ENDIF.
    IF sy-subrc = 0.
      WRITE: /        'AGS_DSH_CAT_TXT',
             AT posid ls_ags_dsh_cat_txt-name COLOR COL_NORMAL.
    ENDIF.

  ENDSELECT.

*----------------------------------------------------------------------

* dashboard header table
  SELECT * FROM ags_dsh_header INTO ls_ags_dsh_header
    WHERE id = ls_ags_dsh_elt_dir-eltuid
    ORDER BY pos.

    WRITE: /          'AGS_DSH_HEADER',
           AT posid   ls_ags_dsh_header-category,
           (2)        space, " no icon here
           AT posdata
*          (30)       LS_AGS_DSH_HEADER-LOGO,
*                     LS_AGS_DSH_HEADER-NAMESPACE,
*                     LS_AGS_DSH_HEADER-CREATED_BY,
*                     LS_AGS_DSH_HEADER-CREATED_AT dd/mm/yyyy,
                      ls_ags_dsh_header-last_modified_by,
                      ls_ags_dsh_header-last_modified_at DD/MM/YYYY,
*                     LS_AGS_DSH_HEADER-LAST_MODIFIED_TS,
*                     LS_AGS_DSH_HEADER-AUTO_REFRESH,
*                     LS_AGS_DSH_HEADER-TIME_INTERVAL,
                      ls_ags_dsh_header-pos.

*   dashboard header text table
    SELECT SINGLE * FROM ags_dsh_head_txt INTO ls_ags_dsh_head_txt
      WHERE id = ls_ags_dsh_header-id
        AND lang = sy-langu.
    IF sy-subrc NE 0 AND sy-langu NE 'E'.
      SELECT SINGLE * FROM ags_dsh_head_txt INTO ls_ags_dsh_head_txt
        WHERE id   = ls_ags_dsh_header-id
          AND lang = 'E'.
    ENDIF.
    IF sy-subrc = 0.
      WRITE: /              'AGS_DSH_HEAD_TXT',
             AT posid       ls_ags_dsh_head_txt-name COLOR COL_NORMAL,
                            ls_ags_dsh_head_txt-description COLOR COL_NORMAL.
    ENDIF.

*   dashboard global filter header
    SELECT * FROM ags_dsh_gflt_hd INTO ls_ags_dsh_gflt_hd
      WHERE dsh_id = ls_ags_dsh_header-id.
      WRITE: /              'AGS_DSH_GFLT_HD',
             AT posid       ls_ags_dsh_gflt_hd-gflt_id INTENSIFIED,
             (2)            space, " no icon here
             AT posdata(30) ls_ags_dsh_gflt_hd-primary_ds,
                            ls_ags_dsh_gflt_hd-primary_ds_type,
                            ls_ags_dsh_gflt_hd-filter_field,
                            "LS_AGS_DSH_GFLT_HD-pos,
                            ls_ags_dsh_gflt_hd-sign,
                            ls_ags_dsh_gflt_hd-operator,
                            ls_ags_dsh_gflt_hd-low,
                            ls_ags_dsh_gflt_hd-high.
    ENDSELECT.

  ENDSELECT.

*----------------------------------------------------------------------

* dashboard KPI defintion
  SELECT * FROM ags_kpi_conf INTO ls_ags_kpi_conf
    WHERE parent_id = ls_ags_dsh_elt_dir-eltuid
    ORDER BY pos.

*   dashboard KPI defintion text table
    CLEAR ls_ags_kpi_conf_txt.
    SELECT SINGLE * FROM ags_kpi_conf_txt INTO ls_ags_kpi_conf_txt
      WHERE parent_id = ls_ags_kpi_conf-parent_id
        AND kpi_id    = ls_ags_kpi_conf-kpi_id
        AND lang = sy-langu.
    IF sy-subrc NE 0 AND sy-langu NE 'E'.
      SELECT SINGLE * FROM ags_kpi_conf_txt INTO ls_ags_kpi_conf_txt
        WHERE parent_id = ls_ags_kpi_conf-parent_id
          AND kpi_id    = ls_ags_kpi_conf-kpi_id
          AND lang      = 'E'.
    ENDIF.

    WRITE: /          'AGS_KPI_CONF',
           AT posid   ls_ags_kpi_conf-kpi_id INTENSIFIED,
                      "ls_ags_kpi_conf-parent_id COLOR COL_POSITIVE,
*###
                      '@3N@' AS ICON HOTSPOT, "3N     ICON_INSERT_RELATION
           AT posdata ls_ags_kpi_conf-kpi_kind,
                      ls_ags_kpi_conf-pos,
                      ls_ags_kpi_conf-layout_type,
                      ls_ags_kpi_conf-rendering_type,
                      ls_ags_kpi_conf-ds_type,
              (30)    ls_ags_kpi_conf-ds_source,
                      "LS_AGS_KPI_CONF-DS_LAST_CHANGED_AT,
                      ls_ags_kpi_conf-ds_source_setting,
                      ls_ags_kpi_conf-drilldown_target_template,
                      ls_ags_kpi_conf-drilldown_target_id,
              (30)    ls_ags_kpi_conf-url,
                      ls_ags_kpi_conf-smart_init_vis,
                      ls_ags_kpi_conf-smart_init_chart.
    HIDE: ls_ags_kpi_conf-parent_id,
          ls_ags_kpi_conf-pos,
          ls_ags_kpi_conf_txt-kpi_name.

    IF ls_ags_kpi_conf_txt-kpi_name IS NOT INITIAL.
      WRITE: /        'AGS_KPI_CONF_TXT',
             AT posid "LS_AGS_KPI_CONF_TXT-KPI_ID intensified,
                      ls_ags_kpi_conf_txt-kpi_name COLOR COL_NORMAL,
                      ls_ags_kpi_conf_txt-kpi_description COLOR COL_NORMAL,
                      ls_ags_kpi_conf_txt-kpi_goal,
                      ls_ags_kpi_conf_txt-kpi_units.
    ENDIF.

*   Data source
    SELECT * FROM ags_dsh_ds_conf INTO ls_ags_dsh_ds_conf
      WHERE ds_setting_id = ls_ags_kpi_conf-kpi_id
      ORDER BY pos.

      WRITE: /          'AGS_KPI_CONF',
             "AT posid  "ls_ags_dsh_ds_conf-ds_setting_id INTENSIFIED,
                        "ls_AGS_DSH_DS_CONF-GUID,
             AT posdata ls_ags_dsh_ds_conf-pos,
                        ls_ags_dsh_ds_conf-ds_type,
                (30)    ls_ags_dsh_ds_conf-ds_name.
    ENDSELECT.

*   Threshold
    SELECT * FROM dsh_kpi_thres_hd INTO ls_dsh_kpi_thres_hd
      WHERE kpi_id = ls_ags_kpi_conf-kpi_id.

      WRITE: /          'DSH_KPI_THRES_HD',
             "AT posid   "ls_ags_dsh_ds_conf-threshold_id INTENSIFIED,
             AT posdata(15) ls_dsh_kpi_thres_hd-kfg_id,
             (15)       ls_dsh_kpi_thres_hd-kfg_name,
                        ls_dsh_kpi_thres_hd-sign,
                        ls_dsh_kpi_thres_hd-zoption,
             (10)       ls_dsh_kpi_thres_hd-low,
             (10)       ls_dsh_kpi_thres_hd-high,
                        ls_dsh_kpi_thres_hd-rating.
    ENDSELECT.

  ENDSELECT.

*----------------------------------------------------------------------

* KPI drilldown definition
  SELECT * FROM ags_kpi_drilldow INTO ls_ags_kpi_drilldow
    WHERE parent_id = ls_ags_dsh_elt_dir-eltuid
    ORDER BY pos.

    WRITE: /          'AGS_KPI_DRILLDOW',
           AT posid   ls_ags_kpi_drilldow-kpi_id INTENSIFIED,
           (2)        space, " no icon here
           AT posdata ls_ags_kpi_drilldow-pos,
                      ls_ags_kpi_drilldow-ds_type,
           (30)       ls_ags_kpi_drilldow-ds_source,
                      "LS_AGS_KPI_DRILLDOW-DS_LAST_CHANGED_AT,
                      ls_ags_kpi_drilldow-ds_source_setting,
                      ls_ags_kpi_drilldow-render_type,
                      ls_ags_kpi_drilldow-disable_visual_switch,
                      ls_ags_kpi_drilldow-jump_bics,
                      ls_ags_kpi_drilldow-target_url_type,
                      ls_ags_kpi_drilldow-appl_target_url,
                      ls_ags_kpi_drilldow-drilldown_type,
                      ls_ags_kpi_drilldow-jump_type.

*   text table for AGS_DSH_DRIL_TXT
    SELECT SINGLE * FROM ags_dsh_dril_txt INTO ls_ags_dsh_dril_txt
      WHERE parent_id = ls_ags_kpi_drilldow-parent_id
        AND kpi_id    = ls_ags_kpi_drilldow-kpi_id
        AND lang      = sy-langu.
    IF sy-subrc NE 0 AND sy-langu NE 'E'.
      SELECT SINGLE * FROM ags_dsh_dril_txt INTO ls_ags_dsh_dril_txt
        WHERE parent_id = ls_ags_kpi_drilldow-parent_id
          AND kpi_id    = ls_ags_kpi_drilldow-kpi_id
          AND lang      = sy-langu.
    ENDIF.
    IF sy-subrc = 0.
      WRITE: /        'AGS_DSH_DRIL_TXT',
             AT posid "LS_AGS_DSH_DRIL_TXT-PARENT_ID intensified,
                      "LS_AGS_DSH_DRIL_TXT-KPI_ID intensified,
                      ls_ags_dsh_dril_txt-drilldown_name COLOR COL_NORMAL.
    ENDIF.

*   Data source
    SELECT * FROM ags_dsh_ds_conf INTO ls_ags_dsh_ds_conf
      WHERE ds_setting_id = ls_ags_kpi_drilldow-kpi_id
      ORDER BY pos.

      WRITE: /          'AGS_KPI_CONF',
             AT posid   ls_ags_dsh_ds_conf-ds_setting_id INTENSIFIED,
                        "ls_AGS_DSH_DS_CONF-GUID,
             AT posdata ls_ags_dsh_ds_conf-pos,
                        ls_ags_dsh_ds_conf-ds_type,
                (30)    ls_ags_dsh_ds_conf-ds_name.
    ENDSELECT.

  ENDSELECT.

*----------------------------------------------------------------------

* rows: selected characteristics
  SELECT * FROM ags_dsh_dim_conf INTO ls_ags_dsh_dim_conf
    WHERE id = ls_ags_dsh_elt_dir-eltuid
    ORDER BY pos.

    WRITE: /          'AGS_DSH_DIM_CONF',
           AT posid   ls_ags_dsh_dim_conf-dim_id INTENSIFIED,
           (2)        space, " no icon here
           AT posdata "LS_AGS_DSH_DIM_CONF-DIM_NAME color col_group,
                      ls_ags_dsh_dim_conf-pos,
                      ls_ags_dsh_dim_conf-sort_type,
                      ls_ags_dsh_dim_conf-sort_direction,
                      ls_ags_dsh_dim_conf-kfg_member .
  ENDSELECT.

* kpi data source setting filter
  SELECT * FROM ags_dsh_flt_conf INTO ls_ags_dsh_flt_conf
    WHERE id = ls_ags_dsh_elt_dir-eltuid
    ORDER BY pos.

    WRITE: /          'AGS_DSH_FLT_CONF',
           AT posid   "LS_AGS_DSH_FLT_CONF-ID,
                      ls_ags_dsh_flt_conf-dim_id INTENSIFIED,
           (2)        space, " no icon here
           AT posdata ls_ags_dsh_flt_conf-serialno INTENSIFIED,
                      "LS_AGS_DSH_FLT_CONF-DIM_NAME color col_group,
                      ls_ags_dsh_flt_conf-kind,
           (30)       ls_ags_dsh_flt_conf-dim_text,
                      ls_ags_dsh_flt_conf-sign,
                      ls_ags_dsh_flt_conf-operator,
           (60)       ls_ags_dsh_flt_conf-low,
           (60)       ls_ags_dsh_flt_conf-low_text,
           (60)       ls_ags_dsh_flt_conf-high,
           (60)       ls_ags_dsh_flt_conf-high_text,
           (60)       ls_ags_dsh_flt_conf-text,
                      ls_ags_dsh_flt_conf-time_relevant,
                      ls_ags_dsh_flt_conf-time_rolling_id,
                      ls_ags_dsh_flt_conf-pos,
                      ls_ags_dsh_flt_conf-ref_kfg.
  ENDSELECT.

* kpi data source setting key figures for columns
  SELECT * FROM ags_dsh_kfg_conf INTO ls_ags_dsh_kfg_conf
    WHERE id = ls_ags_dsh_elt_dir-eltuid
    ORDER BY pos.

    WRITE: /          'AGS_DSH_KFG_CONF',
           AT posid   ls_ags_dsh_kfg_conf-kfg_id INTENSIFIED,
           (2)        space, " no icon here
           AT posdata
                      "LS_AGS_DSH_KFG_CONF-KFG_NAME color col_group,
                      ls_ags_dsh_kfg_conf-pos,
                      ls_ags_dsh_kfg_conf-sort_type,
                      ls_ags_dsh_kfg_conf-sort_direction,
                      ls_ags_dsh_kfg_conf-kfg_member.
  ENDSELECT.

ENDFORM.

*----------------------------------------------------------------------

FORM write_dashboards.

  DATA: ls_ags_dsh_header TYPE ags_dsh_header,
        lt_ags_dsh_header TYPE TABLE OF ags_dsh_header.

* Dashboards
  SELECT * FROM ags_dsh_header INTO TABLE lt_ags_dsh_header
    WHERE id       IN sdshid
      AND category IN scatid
    ORDER BY category pos.
  LOOP AT lt_ags_dsh_header INTO ls_ags_dsh_header.
    PERFORM write_dashboard
      USING ls_ags_dsh_header-id.
    ULINE.
  ENDLOOP.

ENDFORM.


FORM write_dashboard
    USING l_dashboard TYPE sysuuid_16.

  DATA: ls_tadir            TYPE tadir,

        ls_ags_dsh_cat      TYPE ags_dsh_cat,
        ls_ags_dsh_cat_txt  TYPE ags_dsh_cat_txt,

        ls_ags_dsh_header   TYPE ags_dsh_header,
        ls_ags_dsh_head_txt TYPE ags_dsh_head_txt,
        lt_ags_dsh_gflt_hd  TYPE TABLE OF ags_dsh_gflt_hd,
        ls_ags_dsh_gflt_hd  TYPE ags_dsh_gflt_hd,

        ls_ags_kpi_conf     TYPE ags_kpi_conf,
        lt_ags_kpi_conf     TYPE TABLE OF ags_kpi_conf.

* Dashboard
  SELECT SINGLE * FROM ags_dsh_header     INTO ls_ags_dsh_header
    WHERE id  = l_dashboard.
  SELECT SINGLE * FROM ags_dsh_head_txt   INTO ls_ags_dsh_head_txt
    WHERE id    = l_dashboard
      AND lang  = sy-langu.
  IF sy-subrc NE 0 AND sy-langu NE 'E'.
    SELECT SINGLE * FROM ags_dsh_head_txt   INTO ls_ags_dsh_head_txt
      WHERE id    = l_dashboard
        AND lang  = 'E'.
  ENDIF.

* Category
  SELECT SINGLE * FROM ags_dsh_cat INTO ls_ags_dsh_cat
    WHERE id = ls_ags_dsh_header-category.
  SELECT SINGLE * FROM ags_dsh_cat_txt INTO ls_ags_dsh_cat_txt
      WHERE id   = ls_ags_dsh_header-category
        AND lang = sy-langu.
  IF sy-subrc NE 0 AND sy-langu NE 'E'.
    SELECT SINGLE * FROM ags_dsh_cat_txt INTO ls_ags_dsh_cat_txt
        WHERE id   = ls_ags_dsh_header-category
          AND lang = 'E'.
  ENDIF.

  FORMAT RESET.
* Category    COL_HEADING
* Dashboard   COL_KEY
* Group       COL_GROUP
* KPI         COL_TOTAL
* Drilldown   COL_NORMAL

* Category
  CLEAR ls_tadir.
  SELECT SINGLE * FROM tadir INTO ls_tadir
    WHERE pgmid    = 'R3TR'
      AND object   = 'DSHE'
      AND obj_name =  ls_ags_dsh_cat-id.

  FORMAT COLOR COL_HEADING.
  WRITE: /(16)          'Category'(009),
         AT posid       ls_ags_dsh_cat-id,
         (2)            space, " no icon here
         AT posdata(18) ls_tadir-devclass,
                        "ls_tadir-author,
                        "ls_ags_dsh_cat-created_by,
                        "ls_ags_dsh_cat-created_at DD/MM/YYYY,
         AT postext     ls_ags_dsh_cat_txt-name,
         AT sy-linsz    space.

* ---

* Dashboard
  CLEAR ls_tadir.
  SELECT SINGLE * FROM tadir INTO ls_tadir
    WHERE pgmid    = 'R3TR'
      AND object   = 'DSHE'
      AND obj_name =  ls_ags_dsh_header-id.

  FORMAT COLOR COL_KEY.
  WRITE: /(16)          'Dashboard'(004),
         AT posid       ls_ags_dsh_header-id,
         (2)            space, " no icon here
         AT posdata(18) ls_tadir-devclass,
                        "ls_tadir-author,
                        "ls_ags_dsh_header-created_by,
                        "ls_ags_dsh_header-created_at DD/MM/YYYY,
         AT postext     ls_ags_dsh_head_txt-name,
         (50)           ls_ags_dsh_head_txt-description,
         AT sy-linsz    space.
  FORMAT RESET.
*  IF ls_ags_dsh_head_txt-description IS NOT INITIAL.
*    WRITE: / ls_ags_dsh_head_txt-description UNDER ls_ags_dsh_head_txt-name.
*  ENDIF.

  IF pkeys IS INITIAL.

*   Global filtes of Dashboard
    SELECT * FROM ags_dsh_gflt_hd INTO TABLE lt_ags_dsh_gflt_hd
      WHERE dsh_id = ls_ags_dsh_header-id
      ORDER BY pos.
    LOOP AT lt_ags_dsh_gflt_hd INTO ls_ags_dsh_gflt_hd.
      WRITE: /(16)           'Global Filter'(014),
              AT posid(16)   space,  " no id
                             "LS_AGS_DSH_GFLT_HD-GFLT_ID,
              AT posdata(18) ls_ags_dsh_gflt_hd-primary_ds_type,
                             ls_ags_dsh_gflt_hd-primary_ds.

*     Data Source
      IF ls_ags_dsh_gflt_hd-primary_ds_type = 'FM'.
        DATA: ls_tftit TYPE tftit.
        SELECT SINGLE * FROM tftit INTO ls_tftit
          WHERE spras    = sy-langu
            AND funcname = ls_ags_dsh_gflt_hd-primary_ds.
        IF sy-subrc NE 0 AND sy-langu NE 'E'.
          SELECT SINGLE * FROM tftit INTO ls_tftit
            WHERE spras    = 'E'
              AND funcname = ls_ags_dsh_gflt_hd-primary_ds.
        ENDIF.
        WRITE: AT postext    ls_tftit-stext.
      ENDIF.

      WRITE: /(16)           'Field'(015),
              AT posid(16)   space, " no id
              AT posdata(18) ls_ags_dsh_gflt_hd-filter_field,
                             ls_ags_dsh_gflt_hd-sign,
                             ls_ags_dsh_gflt_hd-operator,
                             ls_ags_dsh_gflt_hd-low,
                             ls_ags_dsh_gflt_hd-high.
    ENDLOOP.

  ENDIF.

* ---

* Groups and KPIs
  SELECT * FROM ags_kpi_conf INTO TABLE lt_ags_kpi_conf
    WHERE parent_id = ls_ags_dsh_header-id
    ORDER BY pos.
  LOOP AT lt_ags_kpi_conf INTO ls_ags_kpi_conf.

    PERFORM write_kpi
      USING ls_ags_kpi_conf-parent_id
            ls_ags_kpi_conf-kpi_id.

  ENDLOOP. " lt_AGS_KPI_CONF

ENDFORM. " WRITE_DASHBOARD


FORM write_kpi
  USING l_parent_id  TYPE sysuuid_16
        l_kpi_id     TYPE sysuuid_16.

  DATA: ls_tadir            TYPE tadir,

        "ls_ags_kpi_conf     TYPE ags_kpi_conf, "global field for HIDE
        lt_ags_kpi_conf     TYPE TABLE OF ags_kpi_conf,
        "ls_ags_kpi_conf_txt TYPE ags_kpi_conf_txt, "global field for HIDE

        ls_ags_dsh_ds_conf  TYPE ags_dsh_ds_conf,
        lt_ags_dsh_ds_conf  TYPE TABLE OF ags_dsh_ds_conf,

        ls_dsh_kpi_thres_hd TYPE dsh_kpi_thres_hd,
        lt_dsh_kpi_thres_hd TYPE TABLE OF dsh_kpi_thres_hd,

        ls_ags_dsh_flt_conf TYPE ags_dsh_flt_conf,
        lt_ags_dsh_flt_conf TYPE TABLE OF ags_dsh_flt_conf,

        ls_ags_dsh_kfg_conf TYPE ags_dsh_kfg_conf,
        lt_ags_dsh_kfg_conf TYPE TABLE OF ags_dsh_kfg_conf,

        ls_ags_dsh_dim_conf TYPE ags_dsh_dim_conf,
        lt_ags_dsh_dim_conf TYPE TABLE OF ags_dsh_dim_conf.

* Object calalog entry
  CLEAR ls_tadir.
  SELECT SINGLE * FROM tadir INTO ls_tadir
    WHERE pgmid    = 'R3TR'
      AND object   = 'DSHE'
      AND obj_name =  l_kpi_id.

* KPI definition
  CLEAR ls_ags_kpi_conf.
  SELECT SINGLE * FROM ags_kpi_conf INTO ls_ags_kpi_conf
    WHERE parent_id = l_parent_id
      AND kpi_id    = l_kpi_id.

  CLEAR ls_ags_kpi_conf_txt.
  SELECT SINGLE * FROM ags_kpi_conf_txt INTO ls_ags_kpi_conf_txt
    WHERE parent_id = l_parent_id
      AND kpi_id    = l_kpi_id
      AND lang      = sy-langu.
  IF sy-subrc NE 0 AND sy-langu NE 'E'.
    SELECT SINGLE * FROM ags_kpi_conf_txt INTO ls_ags_kpi_conf_txt
      WHERE parent_id = l_parent_id
        AND kpi_id    = l_kpi_id
        AND lang      = 'E'.
  ENDIF.

* Data source
  CLEAR ls_ags_dsh_ds_conf.
  SELECT * FROM ags_dsh_ds_conf INTO TABLE lt_ags_dsh_ds_conf
    WHERE ds_setting_id = l_kpi_id.

  CASE ls_ags_kpi_conf-kpi_kind.
    WHEN 'STD'.
      FORMAT COLOR COL_KEY.
      WRITE: /(16) 'Standard'.

    WHEN 'KGP'.
      FORMAT COLOR COL_GROUP.
      WRITE: /(16) 'Group'(007).

    WHEN 'CUS'.
      FORMAT COLOR COL_TOTAL.
      WRITE: /(16) 'Key perf. index'(011).

*     Layout type
      DATA layout_type(4).
      CASE ls_ags_kpi_conf-layout_type.
        WHEN 'S'.    layout_type = '1X1'.
        WHEN 'L'.    layout_type = '1X2'.
        WHEN 'XL'.   layout_type = '2X2'.
        WHEN 'XXL'.  layout_type = '3X3'.
        WHEN 'FS'.   layout_type = 'Full'.
        WHEN OTHERS. layout_type = ls_ags_kpi_conf-layout_type.
      ENDCASE.

*     KPI Rendering type
      DATA rendering_type TYPE val_text.
      rendering_type = ls_ags_kpi_conf-rendering_type.
      READ TABLE lt_kpi_rendering_type_text INTO ls_kpi_rendering_type_text
        WITH KEY domvalue_l = ls_ags_kpi_conf-rendering_type.
      IF sy-subrc = 0.
        rendering_type = ls_kpi_rendering_type_text-ddtext.
      ENDIF.

    WHEN OTHERS.
      FORMAT COLOR COL_TOTAL.
      WRITE: /(16) ls_ags_kpi_conf-kpi_kind COLOR COL_NEGATIVE.
  ENDCASE.

  WRITE: "/(16)         '...',
         AT posid       ls_ags_kpi_conf-kpi_id,
                        "ls_ags_kpi_conf-parent_id COLOR COL_POSITIVE,
*###
                        '@3N@' AS ICON HOTSPOT, "3N     ICON_INSERT_RELATION
         AT posdata(18) ls_tadir-devclass,
         "               ls_tadir-author,
         "(12)           ls_AGS_KPI_CONF-CREATED_BY,
         "(10)           space, "ls_AGS_KPI_CONF-CREATED_AT dd/mm/yyyy,
                        layout_type,
         (28)           rendering_type,
         AT postext(50) ls_ags_kpi_conf_txt-kpi_name,
         (50)           ls_ags_kpi_conf_txt-kpi_description,
         (50)           ls_ags_kpi_conf_txt-kpi_goal,
         (10)           ls_ags_kpi_conf_txt-kpi_units,
         AT sy-linsz    space.
  HIDE: ls_ags_kpi_conf-parent_id,
        ls_ags_kpi_conf-pos,
        ls_ags_kpi_conf_txt-kpi_name.
  FORMAT RESET.

  IF ls_ags_kpi_conf-kpi_kind = 'CUS'.

    IF pkeys IS INITIAL.

*     Data Source
      LOOP AT lt_ags_dsh_ds_conf INTO ls_ags_dsh_ds_conf.
        PERFORM write_data_source
          USING ls_ags_dsh_ds_conf-ds_type
                ls_ags_dsh_ds_conf-ds_name.
      ENDLOOP.
      IF sy-subrc NE 0 AND ls_ags_kpi_conf-ds_type IS NOT INITIAL.
        PERFORM write_data_source
          USING ls_ags_kpi_conf-ds_type
                ls_ags_kpi_conf-ds_source.
      ENDIF.

*     Filters
      SELECT * FROM ags_dsh_flt_conf INTO TABLE lt_ags_dsh_flt_conf
        WHERE id = ls_ags_kpi_conf-kpi_id
        ORDER BY pos.
      LOOP AT lt_ags_dsh_flt_conf INTO ls_ags_dsh_flt_conf.
        WRITE: /(16)           'Filter'(019),
                AT posid(16)   space. " no id
        IF ls_ags_dsh_flt_conf-dim_id = 'AGR_ON_SYSTEM'.
          WRITE: AT posdata(18) ls_ags_dsh_flt_conf-dim_id INTENSIFIED ON.
        ELSE.
          WRITE: AT posdata(18) ls_ags_dsh_flt_conf-dim_id.
        ENDIF.
        WRITE:                 ls_ags_dsh_flt_conf-sign,
                               ls_ags_dsh_flt_conf-operator,
                               ls_ags_dsh_flt_conf-low,
                               "ls_AGS_DSH_GFLT_HD-high,
                AT postext     ls_ags_dsh_flt_conf-dim_text.
      ENDLOOP.

*     Row dimensions
      SELECT * FROM ags_dsh_dim_conf INTO TABLE lt_ags_dsh_dim_conf
        WHERE id = ls_ags_kpi_conf-kpi_id
        ORDER BY pos.
      LOOP AT lt_ags_dsh_dim_conf INTO ls_ags_dsh_dim_conf.
        WRITE: /(16)        'Row dimension'(021),
               AT posid(16) space, " no id
               AT posdata(18) ls_ags_dsh_dim_conf-dim_id,
                            "ls_ags_dsh_dim_conf-sort_type,
                            ls_ags_dsh_dim_conf-sort_direction.
      ENDLOOP.

*     Columns
      SELECT * FROM ags_dsh_kfg_conf INTO TABLE lt_ags_dsh_kfg_conf
        WHERE id = ls_ags_kpi_conf-kpi_id
        ORDER BY pos.

      LOOP AT lt_ags_dsh_kfg_conf INTO ls_ags_dsh_kfg_conf.
        WRITE: /(16)          'Column'(018),
               AT posid(16)   space, " no id
               AT posdata(18) ls_ags_dsh_kfg_conf-kfg_id,
                              "LS_AGS_DSH_KFG_CONF-POS,
                              ls_ags_dsh_kfg_conf-sort_type,
                              ls_ags_dsh_kfg_conf-sort_direction,
                              ls_ags_dsh_kfg_conf-kfg_member.
*            at postext     LS_AGS_DSH_KFG_CONF-KFG_NAME.
      ENDLOOP.

*   Threshold
      SELECT * FROM dsh_kpi_thres_hd INTO ls_dsh_kpi_thres_hd
        WHERE kpi_id = ls_ags_kpi_conf-kpi_id.

        WRITE: /(16)          'Threshold',
               "AT posid   "ls_ags_dsh_ds_conf-threshold_id INTENSIFIED,
               "AT posdata(18) ls_dsh_kpi_thres_hd-kfg_ID,
               AT posdata(18) ls_dsh_kpi_thres_hd-kfg_name,
                              ls_dsh_kpi_thres_hd-sign,
                              ls_dsh_kpi_thres_hd-zoption,
               (10)           ls_dsh_kpi_thres_hd-low,
               (10)           ls_dsh_kpi_thres_hd-high,
                              ls_dsh_kpi_thres_hd-rating.
      ENDSELECT.

    ENDIF.

*   Drilldown
    PERFORM write_drilldown
      USING ls_ags_kpi_conf-kpi_id.

  ENDIF.
ENDFORM. "


FORM write_drilldown
  USING l_kpi_id TYPE sysuuid_16.

  DATA: ls_tadir            TYPE tadir,

        lt_ags_kpi_drilldow TYPE TABLE OF ags_kpi_drilldow,
        ls_ags_kpi_drilldow TYPE ags_kpi_drilldow,
        ls_ags_dsh_dril_txt TYPE ags_dsh_dril_txt,

        ls_ags_dsh_ds_conf  TYPE ags_dsh_ds_conf,
        lt_ags_dsh_ds_conf  TYPE TABLE OF ags_dsh_ds_conf,

        ls_ags_dsh_flt_conf TYPE ags_dsh_flt_conf,
        lt_ags_dsh_flt_conf TYPE TABLE OF ags_dsh_flt_conf,

        ls_ags_dsh_dim_conf TYPE ags_dsh_dim_conf,
        lt_ags_dsh_dim_conf TYPE TABLE OF ags_dsh_dim_conf,

        ls_ags_dsh_kfg_conf TYPE ags_dsh_kfg_conf,
        lt_ags_dsh_kfg_conf TYPE TABLE OF ags_dsh_kfg_conf.

  CLEAR ls_tadir.
  SELECT SINGLE * FROM tadir INTO ls_tadir
    WHERE pgmid    = 'R3TR'
      AND object   = 'DSHE'
      AND obj_name =  l_kpi_id.

  SELECT * FROM ags_kpi_drilldow INTO TABLE lt_ags_kpi_drilldow
    WHERE parent_id = l_kpi_id
    ORDER BY pos.
  LOOP AT lt_ags_kpi_drilldow INTO ls_ags_kpi_drilldow.

    CLEAR ls_ags_dsh_dril_txt.
    SELECT SINGLE * FROM ags_dsh_dril_txt INTO ls_ags_dsh_dril_txt
      WHERE parent_id = ls_ags_kpi_drilldow-parent_id
        AND kpi_id = ls_ags_kpi_drilldow-kpi_id
        AND lang   = sy-langu.
    IF sy-subrc NE 0 AND sy-langu NE 'E'.
      SELECT SINGLE * FROM ags_dsh_dril_txt INTO ls_ags_dsh_dril_txt
        WHERE parent_id = ls_ags_kpi_drilldow-parent_id
          AND kpi_id = ls_ags_kpi_drilldow-kpi_id
          AND lang   = 'E'.
    ENDIF.

*   Data source
    CLEAR ls_ags_dsh_ds_conf.
    SELECT * FROM ags_dsh_ds_conf INTO TABLE lt_ags_dsh_ds_conf
      WHERE ds_setting_id = ls_ags_kpi_drilldow-kpi_id.


* Drillwown rendering type
    DATA rendering_type TYPE val_text.
    rendering_type = ls_ags_kpi_drilldow-render_type.
    READ TABLE lt_drill_rendering_type_text INTO ls_drill_rendering_type_text
      WITH KEY domvalue_l = ls_ags_kpi_drilldow-render_type.
    IF sy-subrc = 0.
      rendering_type = ls_drill_rendering_type_text-ddtext.
    ENDIF.

    FORMAT COLOR COL_NORMAL.
    WRITE: /(16)           'Drilldown'(020),
           AT posid        ls_ags_kpi_drilldow-kpi_id,
                           "ls_ags_kpi_drilldow-parent_id COLOR COL_POSITIVE INPUT,
            (2)            space, " no icon here
           AT posdata(18)  ls_tadir-devclass,
                           "ls_tadir-author,
                           "ls_ags_kpi_drilldow-DS_LAST_CHANGED_AT,
           (28)            rendering_type,
           AT postext     ls_ags_dsh_dril_txt-drilldown_name,
           AT sy-linsz     space.
    "HIDE ls_ags_kpi_drilldow-parent_id.
    FORMAT RESET.

    IF pkeys IS INITIAL.

*     Data Source
      LOOP AT lt_ags_dsh_ds_conf INTO ls_ags_dsh_ds_conf.
        PERFORM write_data_source
          USING ls_ags_dsh_ds_conf-ds_type
                ls_ags_dsh_ds_conf-ds_name.
      ENDLOOP.
      IF sy-subrc NE 0 AND ls_ags_kpi_drilldow-ds_type IS NOT INITIAL.
        PERFORM write_data_source
          USING ls_ags_kpi_drilldow-ds_type
                ls_ags_kpi_drilldow-ds_source.
      ENDIF.

*     Filters
      SELECT * FROM ags_dsh_flt_conf INTO TABLE lt_ags_dsh_flt_conf
        WHERE id = ls_ags_kpi_drilldow-kpi_id
        ORDER BY pos.
      LOOP AT lt_ags_dsh_flt_conf INTO ls_ags_dsh_flt_conf.
        WRITE: /(16)          'Filter'(019),
               AT posid(16)   space. " no id
        IF ls_ags_dsh_flt_conf-dim_id = 'AGR_ON_SYSTEM'.
          WRITE: AT posdata(18) ls_ags_dsh_flt_conf-dim_id INTENSIFIED ON.
        ELSE.
          WRITE: AT posdata(18) ls_ags_dsh_flt_conf-dim_id.
        ENDIF.
        WRITE:                ls_ags_dsh_flt_conf-sign,
                              ls_ags_dsh_flt_conf-operator,
                              ls_ags_dsh_flt_conf-low,
                              "ls_AGS_DSH_GFLT_HD-high,
               AT postext     ls_ags_dsh_flt_conf-dim_text.
      ENDLOOP. " LT_AGS_DSH_FLT_CONF

*     Row dimensions
      SELECT * FROM ags_dsh_dim_conf INTO TABLE lt_ags_dsh_dim_conf
        WHERE id = ls_ags_kpi_drilldow-kpi_id
        ORDER BY pos.
      LOOP AT lt_ags_dsh_dim_conf INTO ls_ags_dsh_dim_conf.
        WRITE: /(16)        'Row dimension'(021),
               AT posid(16) space, " no id
               AT posdata(18) ls_ags_dsh_dim_conf-dim_id,
                            "ls_ags_dsh_dim_conf-sort_type,
                            ls_ags_dsh_dim_conf-sort_direction.
      ENDLOOP.

*     Columns
      SELECT * FROM ags_dsh_kfg_conf INTO TABLE lt_ags_dsh_kfg_conf
        WHERE id = ls_ags_kpi_drilldow-kpi_id
        ORDER BY pos.

      LOOP AT lt_ags_dsh_kfg_conf INTO ls_ags_dsh_kfg_conf.
        WRITE: /(16)          'Column'(018),
               AT posid(16)   space, " no id
               AT posdata(18) ls_ags_dsh_kfg_conf-kfg_id,
                              "LS_AGS_DSH_KFG_CONF-POS,
                              ls_ags_dsh_kfg_conf-sort_type,
                              ls_ags_dsh_kfg_conf-sort_direction,
                              ls_ags_dsh_kfg_conf-kfg_member.
*            at postext     LS_AGS_DSH_KFG_CONF-KFG_NAME.
      ENDLOOP.

*   Threshold
      SELECT * FROM dsh_kpi_thres_hd INTO ls_dsh_kpi_thres_hd
        WHERE kpi_id = ls_ags_kpi_drilldow-kpi_id.

        WRITE: /(16)          'Threshold',
               "AT posid   "ls_ags_dsh_ds_conf-threshold_id INTENSIFIED,
               "AT posdata(18) ls_dsh_kpi_thres_hd-kfg_ID,
               AT posdata(18) ls_dsh_kpi_thres_hd-kfg_name,
                              ls_dsh_kpi_thres_hd-sign,
                              ls_dsh_kpi_thres_hd-zoption,
               (10)           ls_dsh_kpi_thres_hd-low,
               (10)           ls_dsh_kpi_thres_hd-high,
                              ls_dsh_kpi_thres_hd-rating.
      ENDSELECT.

    ENDIF.

  ENDLOOP. " LT_AGS_KPI_DRILLDOW

ENDFORM. " WRITE_DRILLDOWN


FORM write_data_source
  USING l_ds_type    TYPE ags_kpi_conf-ds_type   "AGS_DSH_DS_TYPE
        l_ds_source  TYPE ags_kpi_conf-ds_source "DS_NAME_OLD_ST
        .

  WRITE: /(16)           'Data Source'(022),
          AT posid(16)   space, " no id
          AT posdata(18) l_ds_type,
                         l_ds_source.

  IF l_ds_type = 'FM'.
    DATA: ls_tftit TYPE tftit.
    SELECT SINGLE * FROM tftit INTO ls_tftit
      WHERE spras    = sy-langu
        AND funcname = l_ds_source.
    IF sy-subrc NE 0 AND sy-langu NE 'E'.
      SELECT SINGLE * FROM tftit INTO ls_tftit
        WHERE spras    = 'E'
          AND funcname = l_ds_source.
    ENDIF.

    WRITE: AT postext    ls_tftit-stext.
  ENDIF.
ENDFORM. " WRITE_DATA_SOURCE

AT LINE-SELECTION.
  PERFORM at_line_selection.
FORM at_line_selection.
  CLEAR: ls_ags_kpi_conf,
         ls_ags_kpi_conf_txt.
  READ CURRENT LINE FIELD VALUE
    ls_ags_kpi_conf-parent_id
    ls_ags_kpi_conf-kpi_id
    ls_ags_kpi_conf-pos
    ls_ags_kpi_conf_txt-kpi_name
    .
  CHECK ls_ags_kpi_conf-parent_id IS NOT INITIAL.

  DATA:
    ls_ags_kpi_conf_original     TYPE ags_kpi_conf,
    ls_ags_kpi_conf_new          TYPE ags_kpi_conf,
    ls_ags_dsh_header_parent     TYPE ags_dsh_header,

    ls_ags_kpi_conf_txt_original TYPE ags_kpi_conf_txt,
    lt_ags_kpi_conf_txt_original TYPE TABLE OF ags_kpi_conf_txt,

    ls_ags_kpi_conf_txt_new      TYPE ags_kpi_conf_txt,
    lt_ags_kpi_conf_txt_new      TYPE TABLE OF ags_kpi_conf_txt.

* Get original header entry
  SELECT SINGLE * FROM ags_kpi_conf INTO ls_ags_kpi_conf_original
    WHERE parent_id = ls_ags_kpi_conf-parent_id
      AND kpi_id    = ls_ags_kpi_conf-kpi_id.
  IF sy-subrc NE 0.
    "No original header entry
    BREAK-POINT.
    EXIT.
  ENDIF.

* Get original text entries
  SELECT * FROM ags_kpi_conf_txt INTO TABLE lt_ags_kpi_conf_txt_original
    WHERE parent_id = ls_ags_kpi_conf-parent_id
      AND kpi_id    = ls_ags_kpi_conf-kpi_id.
  IF sy-subrc NE 0.
    "No original text entries
    BREAK-POINT.
    EXIT.
  ENDIF.

* Check authority for original dashboard
  AUTHORITY-CHECK OBJECT 'SM_DSHO'
           ID 'ACTVT' FIELD '02'
           ID 'DSHID' FIELD ls_ags_kpi_conf-parent_id
           ID 'CATID' DUMMY. "Simplification: Let's ignore the (original and new) categoy
  IF sy-subrc <> 0.
*  Missing authority &1: ACTVT = &2 / DSHID = &3 / CATID = &4
    MESSAGE e002(dsh_builder) WITH 'SM_DSHO' '02 (change)' ls_ags_kpi_conf-parent_id 'ignored'.
  ENDIF.

  DATA:
    ls_fields    TYPE sval,
    lt_fields    TYPE TABLE OF sval,
    l_returncode TYPE c.

  ls_fields-tabname    = 'AGS_KPI_CONF_TXT'.
  ls_fields-fieldname	 = 'KPI_NAME'.
  ls_fields-value      =  ls_ags_kpi_conf_txt-kpi_name.
  ls_fields-field_attr = '02'. "display
  "ls_fields-FIELD_OBL   = 'X'.
  ls_fields-fieldtext	 = 'Name'.
  "ls_fields-NOVALUEHLP = 'S'. " only usevol for function POPUP_GET_VALUES_USER_HELP)
  APPEND ls_fields TO lt_fields.

  ls_fields-tabname    = 'AGS_KPI_CONF'.
  ls_fields-fieldname	 = 'KPI_ID'.
  ls_fields-value      = ls_ags_kpi_conf-kpi_id.
  ls_fields-field_attr = '02'. "display
  "ls_fields-FIELD_OBL   = 'X'.
  ls_fields-fieldtext	 = 'KPI-ID'.
  "ls_fields-NOVALUEHLP = 'S'. " only usevol for function POPUP_GET_VALUES_USER_HELP)
  APPEND ls_fields TO lt_fields.

  ls_fields-tabname    = 'AGS_KPI_CONF'.
  ls_fields-fieldname	 = 'PARENT_ID'.
  ls_fields-value      = ls_ags_kpi_conf-parent_id.  "Searchhelp dshid_hlp
  ls_fields-field_attr = '  '. "input
  ls_fields-field_obl	 = 'X'.
  ls_fields-fieldtext	 = 'Parent-ID'.
  "ls_fields-NOVALUEHLP = 'S'. " only usevol for function POPUP_GET_VALUES_USER_HELP)
  APPEND ls_fields TO lt_fields.


  ls_fields-tabname    = 'AGS_KPI_CONF'.
  ls_fields-fieldname	 = 'POS'.
  ls_fields-value      = ls_ags_kpi_conf-pos.
  ls_fields-field_attr = '  '. "input
  ls_fields-field_obl	 = 'X'.
  ls_fields-fieldtext	 = 'Position'.
  "ls_fields-NOVALUEHLP = 'S'. " only usevol for function POPUP_GET_VALUES_USER_HELP)
  APPEND ls_fields TO lt_fields.

  "  CALL FUNCTION 'POPUP_GET_VALUES'
  "    EXPORTING
  "      popup_title     = 'Move entry to another Dashboard'
  "*     NO_VALUE_CHECK  = ' '
  "*     START_COLUMN    = '5'
  "*     START_ROW       = '5'
  "    IMPORTING
  "      returncode      = l_returncode
  "    TABLES
  "      fields          = lt_fields
  "    EXCEPTIONS
  "      error_in_fields = 1
  "      OTHERS          = 2.
  DATA: progname TYPE sy-repid.
  progname = sy-repid.
  CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
    EXPORTING
      popup_title     = 'Move entry to another Dashboard'
      programname     = progname                 " Input validation
      formname        = 'DSHID_INPUT_CHECK'
      f1_programname  = progname                 " Input Help
      f1_formname     = 'DSHID_F1_HELP'
      f4_programname  = progname                 " Value Help
      f4_formname     = 'DSHID_F4_HELP'
*     START_COLUMN    = '5'
*     START_ROW       = '5'
*     NO_CHECK_FOR_FIXED_VALUES       = ' '
    IMPORTING
      returncode      = l_returncode
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF sy-subrc <> 0 OR l_returncode IS NOT INITIAL.
* Implement suitable error handling here
    EXIT.
  ENDIF.

* Move header entry
  ls_ags_kpi_conf_new = ls_ags_kpi_conf_original.

  READ TABLE lt_fields INTO ls_fields
    WITH KEY tabname    = 'AGS_KPI_CONF'
             fieldname  = 'PARENT_ID'.
  IF sy-subrc NE 0.
    BREAK-POINT.
    EXIT.
  ENDIF.
  ls_ags_kpi_conf_new-parent_id = ls_fields-value.

  READ TABLE lt_fields INTO ls_fields
    WITH KEY tabname    = 'AGS_KPI_CONF'
             fieldname  = 'POS'.
  IF sy-subrc NE 0.
    BREAK-POINT.
    EXIT.
  ENDIF.
  ls_ags_kpi_conf_new-pos       = ls_fields-value.

* Check new parent entry
  SELECT SINGLE * FROM ags_dsh_header INTO ls_ags_dsh_header_parent
    WHERE id        = ls_ags_kpi_conf_new-parent_id.
  IF sy-subrc NE 0.
    "No parent entry
    BREAK-POINT.
    EXIT.
  ENDIF.

* Check new position
*...

* Move text entries
  LOOP AT lt_ags_kpi_conf_txt_original INTO ls_ags_kpi_conf_txt_new.
    ls_ags_kpi_conf_txt_new-parent_id = ls_ags_kpi_conf_new-parent_id.
    APPEND ls_ags_kpi_conf_txt_new TO lt_ags_kpi_conf_txt_new.
  ENDLOOP.

* Check authority for new dashboard
  AUTHORITY-CHECK OBJECT 'SM_DSHO'
           ID 'ACTVT' FIELD '02'
           ID 'DSHID' FIELD ls_ags_kpi_conf_new-parent_id
           ID 'CATID' DUMMY. "Simplification: Let's ignore the (original and new) categoy
  IF sy-subrc <> 0.
*  Missing authority &1: ACTVT = &2 / DSHID = &3 / CATID = &4
    MESSAGE e002(dsh_builder) WITH 'SM_DSHO' '02 (change)' ls_ags_kpi_conf_new-parent_id 'ignored'.
  ENDIF.

* Do it!
*###  EXIT.

  IF ls_ags_kpi_conf_original-parent_id = ls_ags_kpi_conf_new-parent_id.
*   Move to another position
    MODIFY ags_kpi_conf FROM ls_ags_kpi_conf_new.
  ELSE.
*   Move to another parent
    INSERT ags_kpi_conf FROM ls_ags_kpi_conf_new.
    DELETE ags_kpi_conf FROM ls_ags_kpi_conf_original.

    INSERT ags_kpi_conf_txt FROM TABLE lt_ags_kpi_conf_txt_new.
    DELETE ags_kpi_conf_txt FROM TABLE lt_ags_kpi_conf_txt_original.
  ENDIF.

  MESSAGE s044(dsh_builder) WITH 'Item moved' ls_ags_kpi_conf_original-kpi_id 'sucess'.

ENDFORM.


FORM dshid_input_check
  TABLES
    fields STRUCTURE sval
  CHANGING
    error  STRUCTURE svale
    .

  READ TABLE fields WITH KEY fieldname = 'PARENT_ID'.

  DATA: ls_ags_dsh_elt_dir TYPE ags_dsh_elt_dir.
  SELECT SINGLE * FROM ags_dsh_elt_dir
    INTO ls_ags_dsh_elt_dir
    WHERE deftp EQ 'DSH'
      AND eltuid = fields-value.
  IF sy-subrc NE 0.
    "message e045(DSH_BUILDER). "This dashboard doesn't exist!
    error-msgid      = 'DSH_BUILDER'.
    error-msgty      = 'E'.
    error-msgno      = '045'.
    "ERROR-MSGV1      =
    "ERROR-MSGV2      =
    "ERROR-MSGV3      =
    "ERROR-MSGV4      =
    error-errortab   = 'AGS_KPI_CONF'.
    error-errorfield = 'PARENT_ID'.
  ELSE.
    CLEAR error.
  ENDIF.
ENDFORM.

FORM dshid_f1_help
  USING
    tabname
    fieldname
    .

  MESSAGE i030(dsh_builder) WITH 'Select dashboard'.

ENDFORM.

FORM dshid_f4_help
  USING
    tabname
    fieldname
    display
  CHANGING
    returncode
    value
    .

  DATA: "progname      TYPE sy-repid,
        "dynnum        TYPE sy-dynnr,
        "dynpro_values TYPE TABLE OF dynpread,
        "field_value   LIKE LINE OF dynpro_values,
        "field_tab     TYPE TABLE OF dfies      WITH HEADER LINE,
        return_tab    TYPE TABLE OF ddshretval WITH HEADER LINE.

  STATICS:
        value_tab     LIKE ags_dsh_elt_dir OCCURS 0.

  DATA: ls_system_info   TYPE ags_sr_s_lmdb_system.

  "progname = sy-repid.
  "dynnum   = sy-dynnr.

  IF value_tab[] IS INITIAL.
    SELECT * FROM ags_dsh_elt_dir
      INTO TABLE value_tab
      WHERE deftp EQ 'DSH'
      ORDER BY deftp eltuid.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      ddic_structure  = 'AGS_DSH_ELT_DIR'
      retfield        = 'ELTUID'
*     PVALKEY         = ' '
*     DYNPPROG        = ' '
*     DYNPNR          = ' '
*     DYNPROFIELD     = ' '
*     STEPL           = 0
*     WINDOW_TITLE    =
*     VALUE           = ' '
      value_org       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     CALLBACK_METHOD =
*     MARK_TAB        =
*   IMPORTING
*     USER_RESET      =
    TABLES
      value_tab       = value_tab
*     FIELD_TAB       =
      return_tab      = return_tab
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR: returncode, value.
  READ TABLE return_tab INDEX 1.
  returncode = sy-subrc.
  value = return_tab-fieldval.
ENDFORM.
