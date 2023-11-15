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
*&---------------------------------------------------------------------*

report rspfrecommended no standard page heading message-id pf.

constants: C_PROGRAM_VERSION(30) type C value '15.11.2023 FBT'.

type-pools: slis.

types:
  spfl_note_number type string,
  begin of SPFL_RECOMMENDED_VALUE,
    NAME  type string,
    VALUE type string,
    NOTE  type string,
  end of SPFL_RECOMMENDED_VALUE,
  SPFL_RECOMMENDED_VALUE_T type table of SPFL_RECOMMENDED_VALUE.

types: begin of ts_outtab,
         name         type spfl_parameter_name,
         actual       type spfl_parameter_value,
         recommended  type spfl_parameter_value,
         result       type char4,
         default      type spfl_parameter_value,
         note         type string, "spfl_note_number,
         color        type slis_t_specialcol_alv,
         profile      type pfl_profilename,
       end of ts_outtab,
       tt_outtab type standard table of ts_outtab.

data: gt_outtab   type tt_outtab.

START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT 'S_RZL_ADM'
    ID 'ACTVT' FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE E002. "You have no display authorization for CCMS tools
  ENDIF.

  sy-title = 'Show recommended profile parameter values'(TIT).
  perform execute.

*---------------------------------------------------------------------
*       FORM EXECUTE
*---------------------------------------------------------------------

form execute.

  data: ls_layout   type slis_layout_alv,
        lt_fieldcat type slis_t_fieldcat_alv,
        lt_sort     type SLIS_T_SORTINFO_ALV.

  ls_layout-no_input          = 'X'.
  ls_layout-zebra             = 'X'.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-coltab_fieldname = 'color'.
  lt_sort = value #( ( fieldname = 'NAME' ) ).

  perform create_table tables lt_fieldcat gt_outtab.

  call function 'REUSE_ALV_GRID_DISPLAY'
                exporting   i_callback_program          = sy-repid
                            i_callback_user_command     = 'CALLBACK_USER_COMMAND'
                            it_fieldcat                 = lt_fieldcat
                            is_layout                   = ls_layout
                            i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
                            it_sort                     = lt_sort
                tables      t_outtab                    = gt_outtab
                exceptions  program_error               = 1
                            others                      = 2.

endform.                     "EXECUTE

define set_fieldcat.
  clear: ls_fieldcat.
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
  append ls_fieldcat to it_fieldcat.

end-of-definition.


*---------------------------------------------------------------------
*       FORM CREATE_TABLE
*---------------------------------------------------------------------

form create_table tables it_fieldcat type slis_t_fieldcat_alv
                         it_outtab   type tt_outtab.

  data: ls_fieldcat                   type slis_fieldcat_alv,
        lt_all_recommended_values     type spfl_recommended_value_t,
        ls_recommended_value          type spfl_recommended_value,
        ls_recommended_value_no_space type spfl_recommended_value,
        ls_actual_value_no_space      type ts_outtab,
        ls_default_value_no_space     type ts_outtab,
        ls_outtab                     type ts_outtab,
        lv_rc                         type i,
        lv_origin                     type i,
        xcolor                        type slis_specialcol_alv.

  "Retrieve recommended parameters
  TRY.
  call method ('CL_SPFL_PROFILE_PARAMETER')=>('GET_ALL_RECOMMENDED_VALUES')
              importing list = lt_all_recommended_values
              receiving rc   = lv_rc.
  CATCH cx_sy_dyn_call_error.
    " ignore any error
  ENDTRY.
  if lv_rc <> 0.
    message w160 display like 'E'.
    leave program.
  endif.

  perform add_security_parameters CHANGING lt_all_recommended_values.

  clear: it_outtab.
  loop at lt_all_recommended_values into ls_recommended_value ##INTO_OK.
    clear: ls_outtab.
    ls_outtab-name        = ls_recommended_value-name.
    ls_outtab-recommended = ls_recommended_value-value.
    ls_outtab-note        = ls_recommended_value-note.

    "Retrieve parameter value
    call method cl_spfl_profile_parameter=>get_value
                exporting name  = ls_recommended_value-name
                importing value = ls_outtab-actual
                receiving rc    = lv_rc.
    if lv_rc <> 0.
      ls_outtab-actual = 'Value Undefined'(010).
    endif.

    "Retrieve default parameter value
    call method cl_spfl_profile_parameter=>get_default_value
                exporting name  = ls_recommended_value-name
                importing value = ls_outtab-default
                receiving rc    = lv_rc.
    if lv_rc <> 0.
      ls_outtab-default = 'Value Undefined'(010).
    endif.

    "Retrieve origin of a profile parameter
    TRY.
    call method ('CL_SPFL_PROFILE_PARAMETER')=>('GET_ORIGIN')
                exporting name   = ls_recommended_value-name
                importing origin = lv_origin
                receiving rc     = lv_rc.
    if lv_rc <> 0.
      ls_outtab-profile = 'Value Undefined'(010).
    endif.
    CATCH cx_sy_dyn_call_error.
      " ignore any error
      DATA: PAR_NAME    type text60,
            PAR_VALUE   type text60,
            PAR_VALUE11 type text60,
            PAR_VALUE21 type text60,
            PAR_VALUE31 type text60,
            PAR_VALUE32 type text60,
            PAR_VALUE33 type text60,
            PAR_VALUE34 type text60,
            PAR_VALUE44 type text60.
      PAR_NAME = ls_recommended_value-name.
      CALL 'C_SAPGPARAM'
        ID 'NAME'    FIELD PAR_NAME
        ID 'VALUE11' FIELD PAR_VALUE11  "static initialization
        ID 'VALUE21' FIELD PAR_VALUE21  "<11> and DEFAULT.PFL
        ID 'VALUE31' FIELD PAR_VALUE31  "<21> and pf=..-file
        ID 'VALUE32' FIELD PAR_VALUE32  "<31> and argv, env
        ID 'VALUE33' FIELD PAR_VALUE33  "<32> and $$ and $(..) substitution
        ID 'VALUE34' FIELD PAR_VALUE34  "<33> and filename generation
        ID 'VALUE44' FIELD PAR_VALUE44. "<44> shared memory
      if ls_outtab-actual = PAR_VALUE44. lv_origin = 3.  " Dynamic Switching
        if ls_outtab-actual = PAR_VALUE31. lv_origin = 2.  " Instance Profile
          if ls_outtab-actual = PAR_VALUE21. lv_origin = 1.  " Default Profile
            if ls_outtab-actual = PAR_VALUE11. lv_origin = 0.  " Kernel Default
            endif.
          endif.
        endif.
      endif.
    ENDTRY.

    if lv_origin eq 0. "cl_spfl_profile_parameter=>orig_krn. "kernel
      ls_outtab-profile = 'Kernel Default'(014).
    elseif lv_origin eq 1. "cl_spfl_profile_parameter=>orig_default_pfl. "default profile
      ls_outtab-profile = 'Default Profile'(015).
    elseif lv_origin eq 2. "cl_spfl_profile_parameter=>orig_inst_pfl. "instance profile
      ls_outtab-profile = 'Instance Profile'(016).
    elseif lv_origin eq 3. "cl_spfl_profile_parameter=>orig_dyn_change. "dynamic change
      ls_outtab-profile = 'Dynamic Switching'(017).
    endif.

    ls_actual_value_no_space-actual = ls_outtab-actual.
    condense ls_actual_value_no_space-actual NO-GAPS.

    ls_default_value_no_space-actual = ls_outtab-default.
    condense ls_default_value_no_space-actual NO-GAPS.

    ls_recommended_value_no_space-value = ls_recommended_value-value.
    condense ls_recommended_value_no_space-value NO-GAPS.

    "icons and colours definitions
    xcolor-color-int = '0'.
    xcolor-color-inv = '0'.
    if ls_actual_value_no_space-actual = ls_recommended_value_no_space-value.
      xcolor-color-col = '5'.
      ls_outtab-result = ICON_CHECKED.
    elseif ls_actual_value_no_space-actual = ls_default_value_no_space-actual.
      xcolor-color-col = ''.
      ls_outtab-result = ICON_ACTION_SUCCESS.
    else.
      xcolor-color-col = ''.
      ls_outtab-result = ICON_COMPARE.
    endif.
    append xcolor to ls_outtab-color.

    append ls_outtab to it_outtab.

    " Split long values into additional line (ALV shows 128 chars only)
    data L type i value 128.
    if   strlen( ls_outtab-actual ) > L
      or strlen( ls_outtab-default ) > L
      or strlen( ls_outtab-recommended ) > L.
      if strlen( ls_outtab-actual ) > L.
        ls_outtab-actual = ls_outtab-actual+l.
      else.
        clear ls_outtab-actual.
      endif.
      if strlen( ls_outtab-default ) > L.
        ls_outtab-default = ls_outtab-default+l.
      else.
        clear ls_outtab-default.
      endif.
      if strlen( ls_outtab-recommended ) > L.
        ls_outtab-recommended = ls_outtab-recommended+l.
      else.
        clear ls_outtab-actual.
      endif.
      clear: ls_outtab-result, ls_outtab-profile, ls_outtab-note.
      append ls_outtab to it_outtab.
    endif.
  endloop.

  clear: it_fieldcat.
  set_fieldcat: 'NAME'        'SPFL_PARAMETER_NAME'  'Parameter Name'(001)    abap_true  '',  " 50,
                'RESULT'      'CHAR4'                'Result'(006)            abap_false '', " 20,
                'ACTUAL'      'SPFL_PARAMETER_VALUE' 'Actual Value'(002)      abap_false '', " 20,
                'RECOMMENDED' 'SPFL_PARAMETER_VALUE' 'Recommended Value'(003) abap_false '', " 20,
                'DEFAULT'     'SPFL_PARAMETER_VALUE' 'Default Value'(005)     abap_false 'X', " 20,
                'PROFILE'     'pfl_profilename'      'Profile'(007)           abap_false '', " 20,
                'NOTE'        'SPFL_NOTE_NUMBER'     'Related Note'(004)      abap_false ''. " 20,

endform. "create_table


*---------------------------------------------------------------------
*       FORM CALLBACK_USER_COMMAND
*---------------------------------------------------------------------

form CALLBACK_USER_COMMAND using r_ucomm     LIKE sy-ucomm
                                 rs_selfield TYPE slis_selfield.

  data: ls_outtab type ts_outtab.

  read table gt_outtab index rs_selfield-tabindex into ls_outtab.
  check sy-subrc = 0.

  if     R_UCOMM               = '&IC1' "pick
    and  rs_selfield-fieldname = 'NAME'.

    perform call_rz11 using ls_outtab-name.

  elseif R_UCOMM               = '&IC1' "pick
    and  rs_selfield-fieldname = 'NOTE'.

    perform show_note using ls_outtab-note.

  elseif R_UCOMM               = '&IC1' "pick
    and (   rs_selfield-fieldname = 'ACTUAL'
         or rs_selfield-fieldname = 'RECOMMENDED'
         or rs_selfield-fieldname = 'DEFAULT' )
    and ls_outtab-result is not initial.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname                 = 'CRM_SURVEY_EDITOR_LONGTEXT'
*     IMPORTING
*       GROUP                    =
*       INCLUDE                  =
*       NAMESPACE                =
*       STR_AREA                 =
      EXCEPTIONS
        FUNCTION_NOT_EXIST       = 1
        OTHERS                   = 2
              .
    IF sy-subrc = 0.

     " Show multiple long lines in a textedit control
     data LONGTEXT type string.
     CONCATENATE
       'Profile parameter'(par)
       ls_outtab-name
       space
       'Actual Value'(002)
       ls_outtab-ACTUAL
       space
       'Recommended Value'(003)
       ls_outtab-RECOMMENDED
       space
       'Default Value'(005)
       ls_outtab-DEFAULT
       space
       'Profile'(007)
       ls_outtab-PROFILE
       into LONGTEXT
       SEPARATED BY CL_ABAP_CHAR_UTILITIES=>NEWLINE.


     call function 'CRM_SURVEY_EDITOR_LONGTEXT'
       EXPORTING
*        MAX_LENGTH           = 0
         READ_ONLY            = 'X'
       changing
         LONGTEXT             = LONGTEXT
       EXCEPTIONS
         USER_CANCELLED       = 1
         OTHERS               = 2
               .
     if SY-SUBRC <> 0.
*  Implement suitable error handling here
     endif.

    else.

      " Show multiple long lines on a list popup
      data:
        titlebar(80),
        line_size type i,
        list_tab type table of TRTAB,
        line     type          TRTAB.

      concatenate 'Profile parameter'(par) ls_outtab-name into titlebar SEPARATED BY space.

      line_size = strlen( ls_outtab-name ).
      if line_size < strlen( ls_outtab-ACTUAL ).      line_size = strlen( ls_outtab-ACTUAL ).      endif.
      if line_size < strlen( ls_outtab-RECOMMENDED ). line_size = strlen( ls_outtab-RECOMMENDED ). endif.
      if line_size < strlen( ls_outtab-DEFAULT ).     line_size = strlen( ls_outtab-DEFAULT ).     endif.

      clear list_tab[].
      line = 'Profile parameter'(par). append line to list_tab.
      line = ls_outtab-name.           append line to list_tab.
      line = space.                    append line to list_tab.
      line = 'Actual Value'(002).      append line to list_tab.
      line = ls_outtab-ACTUAL.         append line to list_tab.
      line = space.                    append line to list_tab.
      line = 'Recommended Value'(003). append line to list_tab.
      line = ls_outtab-RECOMMENDED.    append line to list_tab.
      line = space.                    append line to list_tab.
      line = 'Default Value'(005).     append line to list_tab.
      line = ls_outtab-DEFAULT.        append line to list_tab.

      call function 'LAW_SHOW_POPUP_WITH_TEXT'
        exporting
          TITELBAR                     = titlebar
*         HEADER_LINES                 =
*         SHOW_CANCEL_BUTTON           = ' '
          LINE_SIZE                    = line_size
*         SHOW_BUTTON_YES_TO_ALL       = ' '
*       IMPORTING
*         YES_TO_ALL                   =
        TABLES
          LIST_TAB                     = list_tab
        EXCEPTIONS
          ACTION_CANCELLED             = 1
          OTHERS                       = 2
                .
      if SY-SUBRC <> 0.
*   Implement suitable error handling here
      endif.

    endif.

  endif.

  clear sy-subrc.

endform. "CALLBACK_USER_COMMAND


*---------------------------------------------------------------------
*       FORM call_rz11
*---------------------------------------------------------------------

form call_rz11 using parameter_name type spfl_parameter_name.

  data: begin of bdcdata occurs 0.
        include structure bdcdata.
  data: end of bdcdata.

  check parameter_name is not initial.

  clear bdcdata. refresh bdcdata.
  bdcdata-program  = 'RSPFLDOC'.
  bdcdata-dynpro   = '1000'.
  bdcdata-dynbegin = 'X'.
  append bdcdata.

  clear bdcdata-dynbegin.
  bdcdata-fnam = 'TPFYSTRUCT-NAME'.
  bdcdata-fval = parameter_name.
  append bdcdata.

  try.
    call transaction 'RZ11' with authority-check
      using bdcdata
      mode 'E'
      update 'S'.
    catch cx_sy_authorization_error.
      return.
  endtry.

endform. "call_rz11


*---------------------------------------------------------------------
*       FORM show_note
*---------------------------------------------------------------------

form show_note using note type spfl_note_number.

  data: l_url type char80.

  check note is not initial.

  concatenate 'https://me.sap.com/notes/' note into l_url.

  call function 'CALL_BROWSER'
    exporting
      url                     = l_url
    exceptions
      frontend_not_supported  = 1
      frontend_error          = 2
      prog_not_found          = 3
      no_batch                = 4
      unspecified_error       = 5
      others                  = 6.

endform. "show_note


*---------------------------------------------------------------------
*       FORM html_top_of_page
*---------------------------------------------------------------------

form html_top_of_page using top type ref to cl_dd_document.

  data: ls_text type sdydo_text_element,
        l_grid  type ref to cl_gui_alv_grid,
        f(14)   type c value 'SET_ROW_HEIGHT'.

  call method top->ADD_icon exporting sap_icon = 'ICON_CHECKED'.
  call method top->ADD_text exporting text = 'Parameter matches recommended configuration'(011).
  call method top->NEW_LINE( ).
  call method top->ADD_icon exporting sap_icon = 'ICON_ACTION_SUCCESS'.
  call method top->ADD_text exporting text = 'Parameter is set to an upgrade-compatible default value, check if recommended value can be set'(012).
  call method top->NEW_LINE( ).
  call method top->ADD_icon exporting sap_icon = 'ICON_COMPARE'.
  call method top->ADD_text exporting text = 'Parameter does not match recommended or default value, read parameter documentation for details'(013).

  call method top->NEW_LINE( ).
  call method top->ADD_text exporting text = 'Long values are continued in an additional line'(VAL).
  call method top->NEW_LINE( ).
  call method top->ADD_text exporting text = 'Program version'(VER) && `: ` && C_PROGRAM_VERSION.

  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
                importing e_grid = l_grid.

  call method l_grid->parent->parent->(f)
              exporting id = 1
                        height = 6.

endform. "html_top_of_page


form add_security_parameters CHANGING lt_all_recommended_values type spfl_recommended_value_t.
  data ls_recommended_value type spfl_recommended_value.

  define add_value. " name value note
    ls_recommended_value-NAME  = &1.
    ls_recommended_value-VALUE = &2.
    ls_recommended_value-NOTE  = &3.

    read table lt_all_recommended_values with key name = ls_recommended_value-NAME TRANSPORTING NO FIELDS.
    if sy-subrc is initial.
      "check if the recommended value match
    else.
      " add recommended value
      append ls_recommended_value to lt_all_recommended_values.
    endif.
  end-of-definition.

  data:
    kernel_release TYPE  SYSAPRL,
    kernel_patch   TYPE  SYCHAR05.
  CALL FUNCTION 'OCS_GET_KERNEL_VERSION'
    IMPORTING
      EV_KRELEASE       = kernel_release
      EV_KLEVEL         = kernel_patch.

  data:
    LT_SWPRODUCTS TYPE  TT_INSTSWPROD,
    ls_SWPRODUCT  type  INSTSWPROD.
  CALL FUNCTION 'OCS_GET_INSTALLED_SWPRODUCTS'
*   EXPORTING
*     IV_BUFFERED              = 'X'
    IMPORTING
      ET_SWPRODUCTS            = LT_SWPRODUCTS
*     ET_SWPROD_SPSTACK        =
*     ET_SWPROD_SPS_EP         =
*     ET_SWFEATURES            =
*     ET_INCL_SWFEATURES       =
*     ET_TECH_USAGES           =
    EXCEPTIONS
      INTERNAL_ERROR           = 1
      OTHERS                   = 2
            .
  read table LT_SWPRODUCTS into ls_SWPRODUCT
    WITH key "ID = '073555000100900003877'
             NAME = 'ABAP PLATFORM'.

  " S/4HANA 1909
  add_value 'auth/check/calltransaction'                  '3'    '515130'.
  add_value 'auth/object_disabling_active'                'N'    '2926224'.
  add_value 'auth/rfc_authority_check'                    '6'    '2216306'.
  add_value 'gw/reg_no_conn_info'                         '255'  '2776748'.
  add_value 'gw/rem_start'                                'DISABLED'   '2776748'.
  add_value 'icf/set_HTTPonly_flag_on_cookies'            '0'    '1277022'.
                                                          "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678
  add_value 'icm/HTTP/logging_0'                          'PREFIX=/,LOGFILE=http_%y_%m.log,MAXFILES=2,MAXSIZEKB=50000,SWITCHTF=month, LOGFORMAT=%t %a %u1 \"%r\" %s %b %Lms %{Host}i %w1 %w2'   '2788140'.
  add_value 'icm/HTTP/logging_client_0'                   'PREFIX=/,LOGFILE=http_client_%y_%m.log,MAXFILES=2,MAXSIZEKB=50000,SWITCHTF=month, LOGFORMAT=%t %a %u1 \"%r\" %s %b %Lms %{Host}i'   '2788140'.
  add_value 'icm/security_log'                            'LOGFILE=dev_icm_sec_%y_%m,LEVEL=3,MAXFILES=2,MAXSIZEKB=50000,SWITCHTF=month'   '2788140'.
  add_value 'login/disable_cpic'                          '1'    '2926224'.
  add_value 'login/password_downwards_compatibility'      '0'    '1023437'.
  add_value 'login/password_hash_algorithm'               'encoding=RFC2307, algorithm=iSSHA-512, iterations=15000, saltsize=256'   '2140269'.
  add_value 'ms/HTTP/logging_0'                           'PREFIX=/,LOGFILE=$(DIR_LOGGING)/ms-http-%y-%m-%d.log%o,MAXFILES=7,MAXSIZEKB=10000,SWITCHTF=day,LOGFORMAT=%t %a %u %r %s %b %{Host}i'   '2794817'.
  add_value 'ms/http_logging'                             '1'    '2794817'.
  add_value 'rdisp/gui_auto_logout'                       '3600'   ''.
  add_value 'rdisp/vbdelete'                              '0'    '2441606'.
  add_value 'rfc/callback_security_method'                '3'    '2678501'.
  add_value 'rfc/reject_expired_passwd'                   '1'    '2579165'.
  add_value 'wdisp/add_xforwardedfor_header'              'TRUE' '2788140'.

  " S/4HANA 2020
  add_value 'login/show_detailed_errors'                  '0'    '2001962'.
  add_value 'login/password_compliance_to_current_policy' '1'    '862989'.
  add_value 'login/password_max_idle_initial'             '7'    '862989'.
  add_value 'login/password_max_idle_productive'          '180'  '862989'.
  add_value 'icf/reject_expired_passwd'                   '1'    '2579165'.
  add_value 'system/secure_communication'                 'ON'   '2040644'.

  " S/4HANA 2021
  add_value 'rec/client'                                  'ALL'  '3093760'.

  " S/4HANA 2022
  if kernel_release >= 789.
    add_value 'gw/acl_mode_proxy'                         '1'    '3224889'. " as of Kernel 7.89
  endif.
  add_value 'login/ticket_only_by_https'                  '1'    '1531399'.
  add_value 'ssl/ciphersuites'                            '545:PFS:HIGH::EC_X25519:EC_P256:EC_HIGH' '3198351'.
  if ls_SWPRODUCT-VERSION >= 2022.
    add_value 'rfc/log/active'                            '1'    ''.        " as of S/4HANA 2022
    add_value 'icf/log/active'                            '1'    ''.        " as of S/4HANA 2022
  endif.

  " S/4HANA 2023
  add_value 'rfc/allowoldticket4tt'                       '0'    '3157268'.
endform.
