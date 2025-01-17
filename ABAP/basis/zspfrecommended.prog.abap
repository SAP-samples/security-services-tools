*&---------------------------------------------------------------------*
*& Report rspfrecommended
*&---------------------------------------------------------------------*
*& based on report RSPFRECOMMENDED on SAP_BASIS 7.58 downported to 7.40
*& Author: Frank Buchholz, SAP CoE Security Services
*& Published on: https://github.com/SAP-samples/security-services-tools
*& 02.11.2022 Initial version
*& 03.11.2022 Authorization check added
*& 07.11.2022 Popup to show values
*& 18.04.2023 Change recommendation for rdisp/gui_auto_logout from 1H to 3600
*&            Show multiple long lines in a textedit control
*& 19.04.2023 Change notes link to me.sap.com
*& 15.11.2023 S/4HANA 2023
*& 23.11.2023 Add a VERSION column
*& 17.01.2025 Update according to ECS note 3250501 version 27 from 17.01.2025
*&---------------------------------------------------------------------*

REPORT rspfrecommended NO STANDARD PAGE HEADING MESSAGE-ID pf.

CONSTANTS: c_program_version(30) TYPE c VALUE '17.01.2025 FBT'.

TYPE-POOLS: slis.

TYPES:
  spfl_note_number TYPE string,
  BEGIN OF spfl_recommended_value,
    name    TYPE string, " Profile parameter name
    table   TYPE string, " Customizing table like PRGN_CUST
    field   TYPE string, " Field in customizing table
    value   TYPE string,
    note    TYPE string,
    version TYPE instswprod-version,
  END OF spfl_recommended_value,
  spfl_recommended_value_t TYPE TABLE OF spfl_recommended_value.

TYPES: BEGIN OF ts_outtab,
         name        TYPE spfl_parameter_name,
         actual      TYPE spfl_parameter_value,
         recommended TYPE spfl_parameter_value,
         result      TYPE char4,
         default     TYPE spfl_parameter_value,
         note        TYPE string, "spfl_note_number,
         version     TYPE instswprod-version,
         color       TYPE slis_t_specialcol_alv,
         profile     TYPE pfl_profilename,
       END OF ts_outtab,
       tt_outtab TYPE STANDARD TABLE OF ts_outtab.

DATA: gt_outtab   TYPE tt_outtab.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(25) ss_para FOR FIELD para.
  SELECT-OPTIONS para FOR ('spfl_parameter_name').
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

INITIALIZATION.

  ss_para = 'Parameter'.

  CONCATENATE 'Program version from'(100) c_program_version INTO ss_vers
    SEPARATED BY space.

START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT 'S_RZL_ADM'
    ID 'ACTVT' FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE e002. "You have no display authorization for CCMS tools
  ENDIF.

  sy-title = 'Show recommended profile parameter values'(tit).
  PERFORM execute.

*---------------------------------------------------------------------
*       FORM EXECUTE
*---------------------------------------------------------------------

FORM execute.

  DATA: ls_layout   TYPE slis_layout_alv,
        lt_fieldcat TYPE slis_t_fieldcat_alv,
        lt_sort     TYPE slis_t_sortinfo_alv.

  ls_layout-no_input          = 'X'.
  ls_layout-zebra             = 'X'.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-coltab_fieldname = 'color'.
  lt_sort = VALUE #( ( fieldname = 'NAME' ) ).

  PERFORM create_table TABLES lt_fieldcat gt_outtab.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program          = sy-repid
      i_callback_user_command     = 'CALLBACK_USER_COMMAND'
      it_fieldcat                 = lt_fieldcat
      is_layout                   = ls_layout
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      it_sort                     = lt_sort
    TABLES
      t_outtab                    = gt_outtab
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.

ENDFORM.                     "EXECUTE

DEFINE set_fieldcat.
  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname    = &1.
  ls_fieldcat-rollname     = &2.
  ls_fieldcat-reptext_ddic =
  ls_fieldcat-seltext_s    =
  ls_fieldcat-seltext_m    =
  ls_fieldcat-seltext_l    = &3.
  ls_fieldcat-lowercase    = abap_true.
  ls_fieldcat-key          = &4.
  ls_fieldcat-key_sel      = &4.
  ls_fieldcat-no_out       = &5.
" ls_fieldcat-outputlen    = &5.
  APPEND ls_fieldcat TO it_fieldcat.

END-OF-DEFINITION.


*---------------------------------------------------------------------
*       FORM CREATE_TABLE
*---------------------------------------------------------------------

FORM create_table TABLES it_fieldcat TYPE slis_t_fieldcat_alv
                         it_outtab   TYPE tt_outtab.

  DATA: ls_fieldcat                   TYPE slis_fieldcat_alv,
        lt_all_recommended_values     TYPE spfl_recommended_value_t,
        ls_recommended_value          TYPE spfl_recommended_value,
        ls_recommended_value_no_space TYPE spfl_recommended_value,
        ls_actual_value_no_space      TYPE ts_outtab,
        ls_default_value_no_space     TYPE ts_outtab,
        ls_outtab                     TYPE ts_outtab,
        lv_rc                         TYPE i,
        lv_origin                     TYPE i,
        xcolor                        TYPE slis_specialcol_alv.

  "Retrieve recommended parameters
  TRY.
      CALL METHOD ('CL_SPFL_PROFILE_PARAMETER')=>('GET_ALL_RECOMMENDED_VALUES')
        IMPORTING
          list = lt_all_recommended_values
        RECEIVING
          rc   = lv_rc.
    CATCH cx_sy_dyn_call_error.
      " ignore any error
  ENDTRY.
  IF lv_rc <> 0.
    MESSAGE w160 DISPLAY LIKE 'E'.
    LEAVE PROGRAM.
  ENDIF.

  PERFORM add_security_parameters CHANGING lt_all_recommended_values.

  CLEAR: it_outtab.
  LOOP AT lt_all_recommended_values INTO ls_recommended_value ##INTO_OK.
    CHECK ls_recommended_value-name IN para.
    CLEAR: ls_outtab.
    ls_outtab-name        = ls_recommended_value-name.
    ls_outtab-recommended = ls_recommended_value-value.
    ls_outtab-note        = ls_recommended_value-note.
    ls_outtab-version     = ls_recommended_value-version.

    "Retrieve parameter value
    CALL METHOD cl_spfl_profile_parameter=>get_value
      EXPORTING
        name  = ls_recommended_value-name
      IMPORTING
        value = ls_outtab-actual
      RECEIVING
        rc    = lv_rc.
    IF lv_rc <> 0.
      ls_outtab-actual = 'Value Undefined'(010).
    ENDIF.

    "Retrieve default parameter value
    CALL METHOD cl_spfl_profile_parameter=>get_default_value
      EXPORTING
        name  = ls_recommended_value-name
      IMPORTING
        value = ls_outtab-default
      RECEIVING
        rc    = lv_rc.
    IF lv_rc <> 0.
      ls_outtab-default = 'Value Undefined'(010).
    ENDIF.

    "Retrieve origin of a profile parameter
    TRY.
        CALL METHOD ('CL_SPFL_PROFILE_PARAMETER')=>('GET_ORIGIN')
          EXPORTING
            name   = ls_recommended_value-name
          IMPORTING
            origin = lv_origin
          RECEIVING
            rc     = lv_rc.
        IF lv_rc <> 0.
          ls_outtab-profile = 'Value Undefined'(010).
        ENDIF.
      CATCH cx_sy_dyn_call_error.
        " ignore any error
        DATA: par_name    TYPE text60,
              par_value   TYPE text60,
              par_value11 TYPE text60,
              par_value21 TYPE text60,
              par_value31 TYPE text60,
              par_value32 TYPE text60,
              par_value33 TYPE text60,
              par_value34 TYPE text60,
              par_value44 TYPE text60.
        par_name = ls_recommended_value-name.
        CALL 'C_SAPGPARAM'
          ID 'NAME'    FIELD par_name
          ID 'VALUE11' FIELD par_value11  "static initialization
          ID 'VALUE21' FIELD par_value21  "<11> and DEFAULT.PFL
          ID 'VALUE31' FIELD par_value31  "<21> and pf=..-file
          ID 'VALUE32' FIELD par_value32  "<31> and argv, env
          ID 'VALUE33' FIELD par_value33  "<32> and $$ and $(..) substitution
          ID 'VALUE34' FIELD par_value34  "<33> and filename generation
          ID 'VALUE44' FIELD par_value44. "<44> shared memory
        IF ls_outtab-actual = par_value44.
          lv_origin = 3.  " Dynamic Switching
          IF ls_outtab-actual = par_value31.
            lv_origin = 2.  " Instance Profile
            IF ls_outtab-actual = par_value21.
              lv_origin = 1.  " Default Profile
              IF ls_outtab-actual = par_value11.
                lv_origin = 0.  " Kernel Default
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDTRY.

    IF lv_origin EQ 0. "cl_spfl_profile_parameter=>orig_krn. "kernel
      ls_outtab-profile = 'Kernel Default'(014).
    ELSEIF lv_origin EQ 1. "cl_spfl_profile_parameter=>orig_default_pfl. "default profile
      ls_outtab-profile = 'Default Profile'(015).
    ELSEIF lv_origin EQ 2. "cl_spfl_profile_parameter=>orig_inst_pfl. "instance profile
      ls_outtab-profile = 'Instance Profile'(016).
    ELSEIF lv_origin EQ 3. "cl_spfl_profile_parameter=>orig_dyn_change. "dynamic change
      ls_outtab-profile = 'Dynamic Switching'(017).
    ENDIF.

    ls_actual_value_no_space-actual = ls_outtab-actual.
    CONDENSE ls_actual_value_no_space-actual NO-GAPS.

    ls_default_value_no_space-actual = ls_outtab-default.
    CONDENSE ls_default_value_no_space-actual NO-GAPS.

    ls_recommended_value_no_space-value = ls_recommended_value-value.
    CONDENSE ls_recommended_value_no_space-value NO-GAPS.

    "icons and colours definitions
    xcolor-color-int = '0'.
    xcolor-color-inv = '0'.
    IF ls_actual_value_no_space-actual = ls_recommended_value_no_space-value.
      xcolor-color-col = '5'.
      ls_outtab-result = icon_checked.
    ELSEIF ls_actual_value_no_space-actual = ls_default_value_no_space-actual.
      xcolor-color-col = ''.
      ls_outtab-result = icon_action_success.
    ELSE.
      xcolor-color-col = ''.
      ls_outtab-result = icon_compare.
    ENDIF.
    APPEND xcolor TO ls_outtab-color.

    APPEND ls_outtab TO it_outtab.

    " Split long values into additional line (ALV shows 128 chars only)
    DATA l TYPE i VALUE 128.
    IF   strlen( ls_outtab-actual ) > l
      OR strlen( ls_outtab-default ) > l
      OR strlen( ls_outtab-recommended ) > l.
      IF strlen( ls_outtab-actual ) > l.
        ls_outtab-actual = ls_outtab-actual+l.
      ELSE.
        CLEAR ls_outtab-actual.
      ENDIF.
      IF strlen( ls_outtab-default ) > l.
        ls_outtab-default = ls_outtab-default+l.
      ELSE.
        CLEAR ls_outtab-default.
      ENDIF.
      IF strlen( ls_outtab-recommended ) > l.
        ls_outtab-recommended = ls_outtab-recommended+l.
      ELSE.
        CLEAR ls_outtab-actual.
      ENDIF.
      CLEAR: ls_outtab-result, ls_outtab-profile, ls_outtab-note.
      APPEND ls_outtab TO it_outtab.
    ENDIF.
  ENDLOOP.

  CLEAR: it_fieldcat.
  set_fieldcat: 'NAME'        'SPFL_PARAMETER_NAME'  'Parameter Name'(001)    abap_true  '',  " 50,
                'RESULT'      'CHAR4'                'Result'(006)            abap_false '', " 20,
                'ACTUAL'      'SPFL_PARAMETER_VALUE' 'Actual Value'(002)      abap_false '', " 20,
                'RECOMMENDED' 'SPFL_PARAMETER_VALUE' 'Recommended Value'(003) abap_false '', " 20,
                'DEFAULT'     'SPFL_PARAMETER_VALUE' 'Default Value'(005)     abap_false 'X', " 20,
                'PROFILE'     'PFL_PROFILENAME'      'Profile'(007)           abap_false '', " 20,
                'NOTE'        'SPFL_NOTE_NUMBER'     'Related Note'(004)      abap_false '', " 20,
                'VERSION'     'SC_VERSION'           'Version'(008)           abap_false ''. " 20

ENDFORM. "create_table


*---------------------------------------------------------------------
*       FORM CALLBACK_USER_COMMAND
*---------------------------------------------------------------------

FORM callback_user_command USING r_ucomm     LIKE sy-ucomm
                                 rs_selfield TYPE slis_selfield.

  DATA: ls_outtab TYPE ts_outtab.

  READ TABLE gt_outtab INDEX rs_selfield-tabindex INTO ls_outtab.
  CHECK sy-subrc = 0.

  IF     r_ucomm               = '&IC1' "pick
    AND  rs_selfield-fieldname = 'NAME'.

    PERFORM call_rz11 USING ls_outtab-name.

  ELSEIF r_ucomm               = '&IC1' "pick
    AND  rs_selfield-fieldname = 'NOTE'.

    PERFORM show_note USING ls_outtab-note.

  ELSEIF r_ucomm               = '&IC1' "pick
    AND (   rs_selfield-fieldname = 'ACTUAL'
         OR rs_selfield-fieldname = 'RECOMMENDED'
         OR rs_selfield-fieldname = 'DEFAULT' )
    AND ls_outtab-result IS NOT INITIAL.

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
      CONCATENATE
        'Profile parameter'(par)
        ls_outtab-name
        space
        'Actual Value'(002)
        ls_outtab-actual
        space
        'Recommended Value'(003)
        ls_outtab-recommended
        space
        'Default Value'(005)
        ls_outtab-default
        space
        'Profile'(007)
        ls_outtab-profile
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

      CONCATENATE 'Profile parameter'(par) ls_outtab-name INTO titlebar SEPARATED BY space.

      line_size = strlen( ls_outtab-name ).
      IF line_size < strlen( ls_outtab-actual ).      line_size = strlen( ls_outtab-actual ).      ENDIF.
      IF line_size < strlen( ls_outtab-recommended ). line_size = strlen( ls_outtab-recommended ). ENDIF.
      IF line_size < strlen( ls_outtab-default ).     line_size = strlen( ls_outtab-default ).     ENDIF.

      CLEAR list_tab[].
      line = 'Profile parameter'(par). APPEND line TO list_tab.
      line = ls_outtab-name.           APPEND line TO list_tab.
      line = space.                    APPEND line TO list_tab.
      line = 'Actual Value'(002).      APPEND line TO list_tab.
      line = ls_outtab-actual.         APPEND line TO list_tab.
      line = space.                    APPEND line TO list_tab.
      line = 'Recommended Value'(003). APPEND line TO list_tab.
      line = ls_outtab-recommended.    APPEND line TO list_tab.
      line = space.                    APPEND line TO list_tab.
      line = 'Default Value'(005).     APPEND line TO list_tab.
      line = ls_outtab-default.        APPEND line TO list_tab.

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

  ENDIF.

  CLEAR sy-subrc.

ENDFORM. "CALLBACK_USER_COMMAND


*---------------------------------------------------------------------
*       FORM call_rz11
*---------------------------------------------------------------------

FORM call_rz11 USING parameter_name TYPE spfl_parameter_name.

  DATA: BEGIN OF bdcdata OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA: END OF bdcdata.

  CHECK parameter_name IS NOT INITIAL.

  CLEAR bdcdata. REFRESH bdcdata.
  bdcdata-program  = 'RSPFLDOC'.
  bdcdata-dynpro   = '1000'.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.

  CLEAR bdcdata-dynbegin.
  bdcdata-fnam = 'TPFYSTRUCT-NAME'.
  bdcdata-fval = parameter_name.
  APPEND bdcdata.

  TRY.
      CALL TRANSACTION 'RZ11' WITH AUTHORITY-CHECK
        USING bdcdata
        MODE 'E'
        UPDATE 'S'.
    CATCH cx_sy_authorization_error.
      RETURN.
  ENDTRY.

ENDFORM. "call_rz11


*---------------------------------------------------------------------
*       FORM show_note
*---------------------------------------------------------------------

FORM show_note USING note TYPE spfl_note_number.

  DATA: l_url TYPE char80.

  CHECK note IS NOT INITIAL.

  CONCATENATE 'https://me.sap.com/notes/' note INTO l_url.

  CALL FUNCTION 'CALL_BROWSER'
    EXPORTING
      url                    = l_url
    EXCEPTIONS
      frontend_not_supported = 1
      frontend_error         = 2
      prog_not_found         = 3
      no_batch               = 4
      unspecified_error      = 5
      OTHERS                 = 6.

ENDFORM. "show_note


*---------------------------------------------------------------------
*       FORM html_top_of_page
*---------------------------------------------------------------------

FORM html_top_of_page USING top TYPE REF TO cl_dd_document.

  DATA: ls_text TYPE sdydo_text_element,
        l_grid  TYPE REF TO cl_gui_alv_grid,
        f(14)   TYPE c VALUE 'SET_ROW_HEIGHT'.

  CALL METHOD top->add_icon EXPORTING sap_icon = 'ICON_CHECKED'.
  CALL METHOD top->add_text EXPORTING text = 'Parameter matches recommended configuration'(011).
  CALL METHOD top->new_line( ).
  CALL METHOD top->add_icon EXPORTING sap_icon = 'ICON_ACTION_SUCCESS'.
  CALL METHOD top->add_text EXPORTING text = 'Parameter is set to an upgrade-compatible default value, check if recommended value can be set'(012).
  CALL METHOD top->new_line( ).
  CALL METHOD top->add_icon EXPORTING sap_icon = 'ICON_COMPARE'.
  CALL METHOD top->add_text EXPORTING text = 'Parameter does not match recommended or default value, read parameter documentation for details'(013).

  CALL METHOD top->new_line( ).
  CALL METHOD top->add_text EXPORTING text = 'Long values are continued in an additional line'(val).
  CALL METHOD top->new_line( ).
  CALL METHOD top->add_text EXPORTING text = 'Program version'(ver) && `: ` && c_program_version.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_grid.

  CALL METHOD l_grid->parent->parent->(f)
    EXPORTING
      id     = 1
      height = 6.

ENDFORM. "html_top_of_page


FORM add_security_parameters CHANGING lt_all_recommended_values TYPE spfl_recommended_value_t.
  DATA ls_recommended_value TYPE spfl_recommended_value.

  DEFINE add_value. " version name value note
    ls_recommended_value-version = &1.
    ls_recommended_value-name    = &2.
    ls_recommended_value-value   = &3.
    ls_recommended_value-note    = &4.

    READ TABLE lt_all_recommended_values WITH KEY name = ls_recommended_value-name TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      "check if the recommended value match
    ELSE.
      " add recommended value
      APPEND ls_recommended_value TO lt_all_recommended_values.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE add_table. " version name value note
    ls_recommended_value-version = &1.
    ls_recommended_value-table   = &2.
    ls_recommended_value-field   = &3.
    ls_recommended_value-value   = &4.
    ls_recommended_value-note    = &5.

    APPEND ls_recommended_value TO lt_all_recommended_values.
  END-OF-DEFINITION.

  DATA:
    kernel_release TYPE  sysaprl,
    kernel_patch   TYPE  sychar05.
  CALL FUNCTION 'OCS_GET_KERNEL_VERSION'
    IMPORTING
      ev_krelease = kernel_release
      ev_klevel   = kernel_patch.

  DATA:
    lt_swproducts TYPE  tt_instswprod,
    ls_swproduct  TYPE  instswprod.
  CALL FUNCTION 'OCS_GET_INSTALLED_SWPRODUCTS'
*   EXPORTING
*     IV_BUFFERED              = 'X'
    IMPORTING
      et_swproducts  = lt_swproducts
*     ET_SWPROD_SPSTACK        =
*     ET_SWPROD_SPS_EP         =
*     ET_SWFEATURES  =
*     ET_INCL_SWFEATURES       =
*     ET_TECH_USAGES =
    EXCEPTIONS
      internal_error = 1
      OTHERS         = 2.
  READ TABLE lt_swproducts INTO ls_swproduct
    WITH KEY "ID = '073555000100900003877'
             name = 'ABAP PLATFORM'.

  " S/4HANA 1909
  add_value '1909' 'auth/check/calltransaction'                  '3'    '515130'.
  add_value '1909' 'auth/object_disabling_active'                'N'    '2926224'.
  add_value '1909' 'auth/rfc_authority_check'                    '6'    '2216306'.
  add_value '1909' 'gw/reg_no_conn_info'                         '255'  '2776748'.
  add_value '1909' 'gw/rem_start'                                'DISABLED'   '2776748'.
  add_value '1909' 'icf/set_HTTPonly_flag_on_cookies'            '0'    '1277022'.
  "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678
  add_value '1909' 'icm/HTTP/logging_0'                          'PREFIX=/,LOGFILE=http_%y_%m.log,MAXFILES=2,MAXSIZEKB=50000,SWITCHTF=month, LOGFORMAT=%t %a %u1 \"%r\" %s %b %Lms %{Host}i %w1 %w2'   '2788140'.
  add_value '1909' 'icm/HTTP/logging_client_0'                   'PREFIX=/,LOGFILE=http_client_%y_%m.log,MAXFILES=2,MAXSIZEKB=50000,SWITCHTF=month, LOGFORMAT=%t %a %u1 \"%r\" %s %b %Lms %{Host}i'   '2788140'.
  add_value '1909' 'icm/security_log'                            'LOGFILE=dev_icm_sec_%y_%m,LEVEL=3,MAXFILES=2,MAXSIZEKB=50000,SWITCHTF=month'   '2788140'.
  add_value '1909' 'login/disable_cpic'                          '1'    '2926224'.
  add_value '1909' 'login/password_downwards_compatibility'      '0'    '1023437'.
  add_value '1909' 'login/password_hash_algorithm'               'encoding=RFC2307, algorithm=iSSHA-512, iterations=15000, saltsize=256'   '2140269'.
  add_value '1909' 'ms/HTTP/logging_0'                           'PREFIX=/,LOGFILE=$(DIR_LOGGING)/ms-http-%y-%m-%d.log%o,MAXFILES=7,MAXSIZEKB=10000,SWITCHTF=day,LOGFORMAT=%t %a %u %r %s %b %{Host}i'   '2794817'.
  add_value '1909' 'ms/http_logging'                             '1'    '2794817'.
  add_value '1909' 'rdisp/gui_auto_logout'                       '3600'   ''.
  add_value '1909' 'rdisp/vbdelete'                              '0'    '2441606'.
  add_value '1909' 'rfc/callback_security_method'                '3'    '2678501'.
  add_value '1909' 'rfc/reject_expired_passwd'                   '1'    '2579165'.
  add_value '1909' 'wdisp/add_xforwardedfor_header'              'TRUE' '2788140'.

  " S/4HANA 2020
  add_value '2020' 'login/show_detailed_errors'                  '0'    '2001962'.
  add_value '2020' 'login/password_compliance_to_current_policy' '1'    '862989'.
  add_value '2020' 'login/password_max_idle_initial'             '7'    '862989'.
  add_value '2020' 'login/password_max_idle_productive'          '180'  '862989'.
  add_value '2020' 'icf/reject_expired_passwd'                   '1'    '2579165'.
  add_value '2020' 'system/secure_communication'                 'ON'   '2040644'.

  " S/4HANA 2021
  add_value '2021' 'rec/client'                                  'ALL'  '3093760'.

  " S/4HANA 2022
  IF kernel_release >= 789.
    add_value '2022' 'gw/acl_mode_proxy'                         '1'    '3224889'. " as of Kernel 7.89
  ENDIF.
  add_value '2022' 'login/ticket_only_by_https'                  '1'    '1531399'.
  add_value '2022' 'ssl/ciphersuites'                            '545:PFS:HIGH::EC_X25519:EC_P256:EC_HIGH' '3198351'.
  IF ls_swproduct-version >= 2022.
    add_value '2022' 'rfc/log/active'                            '1'    ''.        " as of S/4HANA 2022
    add_value '2022' 'icf/log/active'                            '1'    ''.        " as of S/4HANA 2022
  ENDIF.

  " S/4HANA 2023
  add_value '2023' 'rfc/allowoldticket4tt'                       'no'    '3157268'. " changed from '0' to 'no' on 17.01.2025
  "add_table '2023' 'PRGN_CUST' 'BNAME_RESTRICT'                  'XXX'  '1731549'.
  "add_table '2023' 'PRGN_CUST' 'US_ASGM_TRANSPORT'               'NO'   '1723881'.
  "add_table '2023' 'PRGN_CUST' 'USER_REL_IMPORT'                 'NO'   '571276'.

  "ECS note 3250501 version 27 from 17.01.2025
  add_value 'ECS 2025' 'abap/ext_debugging_possible'             '2'     '2077333'.
  add_value 'ECS 2025' 'dbs/dba/ccms_maintenance'                '1'     ''.
  add_value 'ECS 2025' 'dbs/dba/ccms_security_level'             '1'     ''.
  add_value 'ECS 2025' 'rdisp/TRACE_HIDE_SEC_DATA'               'ON'    '2012562'.
  add_value 'ECS 2025' 'sapgui/user_scripting'                   'FALSE' '2715519'.
  add_value 'ECS 2025' 'snc/permit_insecure_start'               '0'     ''.

ENDFORM.
