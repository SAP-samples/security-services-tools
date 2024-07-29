*&---------------------------------------------------------------------*
*& Report ZSHOW_SECPOL
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Published on: https://github.com/SAP-samples/security-services-tools
*&
*& 07.10.2022 Initial version
*& 19.10.2022 Selection mode: single cell
*& 05.02.2024 Extension to 40 columns
*& 29.07.2024 Replace CALL 'C_SAPGPARAM' with CL_SPFL_PROFILE_PARAMETER (note 3334028)
*&---------------------------------------------------------------------*
REPORT zshow_secpol.

CONSTANTS: c_program_version(30) TYPE c VALUE '29.07.2024 S44'.

" see class CL_SECURITY_POLICY with methods like GET_ATTRIBUTE_VALUE_LIST

TABLES:
  sec_policy_attr,  " List of Available Security Policy Attributes
  sec_policy_cust,  " Configuration of Security Policies
  sec_policy_custt, " Configuration of Security Policies (Texts)
  sec_policy_rt.    " Security Policy (Runtime Details, Kernel Use)

TYPES:
  BEGIN OF ts_result,
    type                       TYPE security_policy_attrib_type,
    attribute_type             TYPE dd07v-ddtext,
    name                       TYPE security_policy_attrib_key,
    ddic_dataelement           TYPE dataelem,
    default_value	             TYPE sec_policy_attrib_default_val,

    policy01                   TYPE sec_policy_attrib_default_val,
    policy02                   TYPE sec_policy_attrib_default_val,
    policy03                   TYPE sec_policy_attrib_default_val,
    policy04                   TYPE sec_policy_attrib_default_val,
    policy05                   TYPE sec_policy_attrib_default_val,
    policy06                   TYPE sec_policy_attrib_default_val,
    policy07                   TYPE sec_policy_attrib_default_val,
    policy08                   TYPE sec_policy_attrib_default_val,
    policy09                   TYPE sec_policy_attrib_default_val,
    policy10                   TYPE sec_policy_attrib_default_val,
    policy11                   TYPE sec_policy_attrib_default_val,
    policy12                   TYPE sec_policy_attrib_default_val,
    policy13                   TYPE sec_policy_attrib_default_val,
    policy14                   TYPE sec_policy_attrib_default_val,
    policy15                   TYPE sec_policy_attrib_default_val,
    policy16                   TYPE sec_policy_attrib_default_val,
    policy17                   TYPE sec_policy_attrib_default_val,
    policy18                   TYPE sec_policy_attrib_default_val,
    policy19                   TYPE sec_policy_attrib_default_val,
    policy20                   TYPE sec_policy_attrib_default_val,
    policy21                   TYPE sec_policy_attrib_default_val,
    policy22                   TYPE sec_policy_attrib_default_val,
    policy23                   TYPE sec_policy_attrib_default_val,
    policy24                   TYPE sec_policy_attrib_default_val,
    policy25                   TYPE sec_policy_attrib_default_val,
    policy26                   TYPE sec_policy_attrib_default_val,
    policy27                   TYPE sec_policy_attrib_default_val,
    policy28                   TYPE sec_policy_attrib_default_val,
    policy29                   TYPE sec_policy_attrib_default_val,
    policy30                   TYPE sec_policy_attrib_default_val,
    policy31                   TYPE sec_policy_attrib_default_val,
    policy32                   TYPE sec_policy_attrib_default_val,
    policy33                   TYPE sec_policy_attrib_default_val,
    policy34                   TYPE sec_policy_attrib_default_val,
    policy35                   TYPE sec_policy_attrib_default_val,
    policy36                   TYPE sec_policy_attrib_default_val,
    policy37                   TYPE sec_policy_attrib_default_val,
    policy38                   TYPE sec_policy_attrib_default_val,
    policy39                   TYPE sec_policy_attrib_default_val,
    policy40                   TYPE sec_policy_attrib_default_val,

    description                TYPE dd04v-scrtext_l,

    redeemed_profile_parameter TYPE pfeparname,
    profile_parameter_value(6), " type SEC_POLICY_ATTRIB_DEFAULT_VAL,
    kernel_default_value(6),    " type SEC_POLICY_ATTRIB_DEFAULT_VAL,

    t_color                    TYPE lvc_t_scol, " ALV color of row
  END OF ts_result.

DATA:
  ls_result TYPE          ts_result,
  lt_result TYPE TABLE OF ts_result.

DATA:
  max_columns        TYPE i VALUE 40,
  lt_sec_policy_cust TYPE TABLE OF sec_policy_cust.

*------------------------------------------------------------------------*
* Selection screen
*------------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(25) s_secpol FOR FIELD secpol.
SELECT-OPTIONS secpol FOR sec_policy_cust-name.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

*----------------------------------------------------------------------*

INITIALIZATION.

  s_secpol = 'Security policy (max 40)'.

  CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
    SEPARATED BY space.

*----------------------------------------------------------------------*

START-OF-SELECTION.

  " Simplified check (let's omit individual checks per policy)
  AUTHORITY-CHECK OBJECT 'S_SECPOL'
   ID 'ACTVT'      FIELD '03'
   ID 'POLICYNAME' DUMMY.
  IF sy-subrc <> 0.
    MESSAGE e018(security_policy).
  ENDIF.

  CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
    EXPORTING
      view_action                    = 'S'
      view_name                      = 'V_SEC_POLICY_CUS'
*     NO_WARNING_FOR_CLIENTINDEP     = ' '
*     CHECK_ACTION_ALTERNATIVE       =
*     CHECK_MAINFLAG                 = ABAP_FALSE
*   IMPORTING
*     GRANTED_ACTVT                  =
*     MAINFLAG                       =
*   CHANGING
*     ORG_CRIT_INST                  =
    EXCEPTIONS
      invalid_action                 = 1
      no_authority                   = 2
      no_clientindependent_authority = 3
      table_not_found                = 4
      no_linedependent_authority     = 5
      OTHERS                         = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM load_data.
  PERFORM show_result.

FORM load_data.

* ALV Color of entry
  DATA: lt_color TYPE lvc_t_scol,
        ls_color TYPE lvc_s_scol.

  " Get texts for attribute types
  DATA:
    ls_dd07v       TYPE          dd07v,
    lt_dd07v_tab_a TYPE TABLE OF dd07v,
    lt_dd07v_tab_n TYPE TABLE OF dd07v.
  CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      domain_name   = 'SECURITY_POLICY_ATTRIB_TYPE'
      get_state     = 'A  '
*     LANGU         = SY-LANGU
*     PRID          = 0
*     WITHTEXT      = 'X'
*   IMPORTING
*     DD01V_WA_A    =
*     DD01V_WA_N    =
*     GOT_STATE     =
    TABLES
      dd07v_tab_a   = lt_dd07v_tab_a
      dd07v_tab_n   = lt_dd07v_tab_n
    EXCEPTIONS
      illegal_value = 1
      op_failure    = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  " Read attribute data
  SELECT * FROM sec_policy_attr
    INTO CORRESPONDING FIELDS OF TABLE lt_result
    ORDER BY type name.

  LOOP AT lt_result INTO ls_result.
    " Get attribute type text
    READ TABLE lt_dd07v_tab_a INTO ls_dd07v INDEX ls_result-type.
    ls_result-attribute_type = ls_dd07v-ddtext.

    " Get description
    DATA l_dd04v TYPE dd04v.
    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = ls_result-ddic_dataelement
*       STATE         = 'A'
        langu         = sy-langu
      IMPORTING
*       GOTSTATE      =
        dd04v_wa      = l_dd04v
*       TPARA_WA      =
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0.
      ls_result-description = l_dd04v-scrtext_l.
    ENDIF.

    " Get parameter
    IF ls_result-redeemed_profile_parameter IS NOT INITIAL.
      " Use offical API for reading profile parameter values according to note 3334028
      DATA:
        name TYPE spfl_parameter_name,
        val  TYPE spfl_parameter_value.

      " Current profile parameter value
      "CALL 'C_SAPGPARAM'
      "  ID 'NAME'    FIELD ls_result-redeemed_profile_parameter
      "  ID 'VALUE'   FIELD ls_result-profile_parameter_value
      "  .

      name = ls_result-redeemed_profile_parameter.
      CLEAR val.
      CALL METHOD cl_spfl_profile_parameter=>get_value
        EXPORTING
          "server_name =
          name  = name
        IMPORTING
          value = val
        RECEIVING
          rc    = DATA(rc).
      "CONCATENATE ls_result-profile_parameter_value val into ls_result-profile_parameter_value SEPARATED BY '|'.
      ls_result-profile_parameter_value = val.

      " Kernel default value
      "CALL 'C_SAPGPARAM'
      "   ID 'NAME'    FIELD ls_result-redeemed_profile_parameter
      "   ID 'VALUE11' FIELD ls_result-kernel_default_value "static initialization
      "   "ID 'VALUE21' FIELD PAR_VALUE21 "<11> and DEFAULT.PFL
      "   "ID 'VALUE31' FIELD PAR_VALUE31 "<21> and pf=..-file
      "   "ID 'VALUE32' FIELD PAR_VALUE32 "<31> and argv, env
      "   "ID 'VALUE33' FIELD PAR_VALUE33 "<32> and $$ and $(..) substitution
      "   "ID 'VALUE34' FIELD PAR_VALUE34 "<33> and filename generation
      "   "ID 'VALUE44' FIELD PAR_VALUE44 "<44> shared memory
      "   .

      CLEAR val.
      CALL METHOD cl_spfl_profile_parameter=>get_default_value
        EXPORTING
          name  = name
        IMPORTING
          value = val
        RECEIVING
          rc    = rc.
      "CONCATENATE ls_result-kernel_default_value val into ls_result-kernel_default_value SEPARATED BY '|'.
      ls_result-kernel_default_value = val.
    ENDIF.

    CLEAR: ls_color, lt_color.
    ls_color-fname = 'PROFILE_PARAMETER_VALUE'.
    IF ls_result-profile_parameter_value = ls_result-kernel_default_value.
      ls_color-color-col = col_normal.
    ELSE.
      ls_color-color-col = col_negative.
    ENDIF.

    APPEND ls_color TO lt_color.
    ls_result-t_color = lt_color.

    " Store it
    MODIFY lt_result FROM ls_result.
  ENDLOOP.

  " Get security policy data
  DATA:
    ls_sec_policy_cust TYPE sec_policy_cust,
    "lt_sec_policy_cust type table of sec_policy_cust,
    ls_sec_policy_rt   TYPE sec_policy_rt,
    lt_sec_policy_rt   TYPE TABLE OF sec_policy_rt,
    policy_nr(2)       TYPE n,
    value              TYPE sec_policy_attrib_default_val,
    tabix              TYPE i.

  SELECT * FROM sec_policy_cust INTO TABLE lt_sec_policy_cust
    UP TO max_columns ROWS
    WHERE name IN secpol
    ORDER BY name.
  SELECT * FROM sec_policy_rt INTO TABLE lt_sec_policy_rt.

  LOOP AT lt_result INTO ls_result.
    tabix = sy-tabix.

    LOOP AT lt_sec_policy_cust INTO ls_sec_policy_cust.
      policy_nr = sy-tabix.

      CLEAR: ls_color.
      ls_color-fname = 'POLICY__'.
      ls_color-fname+6(2) = policy_nr.

      READ TABLE lt_sec_policy_rt INTO ls_sec_policy_rt
        WITH KEY
          name       = ls_sec_policy_cust-name
          attrib_key = ls_result-name.
      IF sy-subrc IS INITIAL.
        value = ls_sec_policy_rt-attrib_value.
        ls_color-color-col = col_total.
      ELSE.
        value = ls_result-default_value.
        ls_color-color-col = col_normal.
      ENDIF.

      CASE policy_nr.
        WHEN  1. ls_result-policy01 = value.
        WHEN  2. ls_result-policy02 = value.
        WHEN  3. ls_result-policy03 = value.
        WHEN  4. ls_result-policy04 = value.
        WHEN  5. ls_result-policy05 = value.
        WHEN  6. ls_result-policy06 = value.
        WHEN  7. ls_result-policy07 = value.
        WHEN  8. ls_result-policy08 = value.
        WHEN  9. ls_result-policy09 = value.
        WHEN 10. ls_result-policy10 = value.
        WHEN 11. ls_result-policy11 = value.
        WHEN 12. ls_result-policy12 = value.
        WHEN 13. ls_result-policy13 = value.
        WHEN 14. ls_result-policy14 = value.
        WHEN 15. ls_result-policy15 = value.
        WHEN 16. ls_result-policy16 = value.
        WHEN 17. ls_result-policy17 = value.
        WHEN 18. ls_result-policy18 = value.
        WHEN 19. ls_result-policy19 = value.
        WHEN 20. ls_result-policy20 = value.
        WHEN 21. ls_result-policy21 = value.
        WHEN 22. ls_result-policy22 = value.
        WHEN 23. ls_result-policy23 = value.
        WHEN 24. ls_result-policy24 = value.
        WHEN 25. ls_result-policy25 = value.
        WHEN 26. ls_result-policy26 = value.
        WHEN 27. ls_result-policy27 = value.
        WHEN 28. ls_result-policy28 = value.
        WHEN 29. ls_result-policy29 = value.
        WHEN 30. ls_result-policy30 = value.
        WHEN 31. ls_result-policy31 = value.
        WHEN 32. ls_result-policy32 = value.
        WHEN 33. ls_result-policy33 = value.
        WHEN 34. ls_result-policy34 = value.
        WHEN 35. ls_result-policy35 = value.
        WHEN 36. ls_result-policy36 = value.
        WHEN 37. ls_result-policy37 = value.
        WHEN 38. ls_result-policy38 = value.
        WHEN 39. ls_result-policy39 = value.
        WHEN 40. ls_result-policy40 = value.
      ENDCASE.

      APPEND ls_color TO ls_result-t_color.

    ENDLOOP.

    MODIFY lt_result FROM ls_result INDEX tabix.
  ENDLOOP.

  " Add count of assigned users
  CLEAR ls_result.
  ls_result-attribute_type = 'Usage'.
  ls_result-name           = 'Count of users'.
  LOOP AT lt_sec_policy_cust INTO ls_sec_policy_cust.
    policy_nr = sy-tabix.

    SELECT COUNT(*) FROM usr02 INTO value
      WHERE security_policy = ls_sec_policy_cust-name.

    CASE policy_nr.
      WHEN  1. ls_result-policy01 = value.
      WHEN  2. ls_result-policy02 = value.
      WHEN  3. ls_result-policy03 = value.
      WHEN  4. ls_result-policy04 = value.
      WHEN  5. ls_result-policy05 = value.
      WHEN  6. ls_result-policy06 = value.
      WHEN  7. ls_result-policy07 = value.
      WHEN  8. ls_result-policy08 = value.
      WHEN  9. ls_result-policy09 = value.
      WHEN 10. ls_result-policy10 = value.
      WHEN 11. ls_result-policy11 = value.
      WHEN 12. ls_result-policy12 = value.
      WHEN 13. ls_result-policy13 = value.
      WHEN 14. ls_result-policy14 = value.
      WHEN 15. ls_result-policy15 = value.
      WHEN 16. ls_result-policy16 = value.
      WHEN 17. ls_result-policy17 = value.
      WHEN 18. ls_result-policy18 = value.
      WHEN 19. ls_result-policy19 = value.
      WHEN 20. ls_result-policy20 = value.
      WHEN 21. ls_result-policy21 = value.
      WHEN 22. ls_result-policy22 = value.
      WHEN 23. ls_result-policy23 = value.
      WHEN 24. ls_result-policy24 = value.
      WHEN 25. ls_result-policy25 = value.
      WHEN 26. ls_result-policy26 = value.
      WHEN 27. ls_result-policy27 = value.
      WHEN 28. ls_result-policy28 = value.
      WHEN 29. ls_result-policy29 = value.
      WHEN 30. ls_result-policy30 = value.
      WHEN 31. ls_result-policy31 = value.
      WHEN 32. ls_result-policy32 = value.
      WHEN 33. ls_result-policy33 = value.
      WHEN 34. ls_result-policy34 = value.
      WHEN 35. ls_result-policy35 = value.
      WHEN 36. ls_result-policy36 = value.
      WHEN 37. ls_result-policy37 = value.
      WHEN 38. ls_result-policy38 = value.
      WHEN 39. ls_result-policy39 = value.
      WHEN 40. ls_result-policy40 = value.
    ENDCASE.

  ENDLOOP.
  APPEND ls_result TO lt_result.

ENDFORM.

*------------------------------------------------------------------------*
* Show result
*------------------------------------------------------------------------*

*---------------------------------------------------------------------*
*      CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.

  PUBLIC SECTION.

    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

*      on_single_click for event link_click of cl_salv_events_table
*        importing row column.

  PRIVATE SECTION.
    DATA: dialogbox_status TYPE c.  "'X': does exist, SPACE: does not ex.

ENDCLASS.                    "lcl_handle_events DEFINITION

* main data table
DATA: gr_alv_table      TYPE REF TO cl_salv_table.

* for handling the events of cl_salv_table
DATA: gr_alv_events     TYPE REF TO lcl_handle_events.

*----------------------------------------------------------------------*
*      CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
* implement the events for handling the events of cl_salv_table
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
*    importing e_salv_function

    DATA: ls_result       TYPE ts_result,
          lr_selections   TYPE REF TO cl_salv_selections,
          ls_cell         TYPE salv_s_cell,
          lt_seleced_rows TYPE salv_t_row,
          l_row           TYPE i.

    " Get selected item
    lr_selections   = gr_alv_table->get_selections( ).
    ls_cell         = lr_selections->get_current_cell( ).
    lt_seleced_rows = lr_selections->get_selected_rows( ).

    CASE e_salv_function.

      WHEN 'PICK'. " Double click
        IF ls_cell-row > 0.
          CLEAR ls_result.
          READ TABLE lt_result INTO ls_result INDEX ls_cell-row.
          IF sy-subrc = 0.
            "...
          ENDIF.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "on_user_command

  METHOD on_double_click.
*   importing row column

    DATA: ls_result    TYPE        ts_result.
    DATA: lr_selections TYPE REF TO cl_salv_selections,
          ls_cell       TYPE        salv_s_cell.

*   Get selected item
    lr_selections = gr_alv_table->get_selections( ).
    ls_cell = lr_selections->get_current_cell( ).

    " Call view custer for security policy
    IF ls_cell-columnname(6) = 'POLICY'.
      DATA:
        policy_nr(2)       TYPE n,
        ls_sec_policy_cust TYPE sec_policy_cust.
      policy_nr = ls_cell-columnname+6(2).
      READ TABLE lt_sec_policy_cust INTO ls_sec_policy_cust INDEX policy_nr.
      IF sy-subrc IS INITIAL.
        PERFORM call_view_cluster USING ls_sec_policy_cust-name.
      ENDIF.
    ENDIF.

    " Call RZ11
    IF   ls_cell-columnname = 'REDEEMED_PROFILE_PARAMETER'
      OR ls_cell-columnname = 'PROFILE_PARAMETER_VALUE'
      OR ls_cell-columnname = 'KERNEL_DEFAULT_VALUE'.

      IF ls_cell-row > 0.
        CLEAR ls_result.
        READ TABLE lt_result INTO ls_result INDEX ls_cell-row.
        IF sy-subrc = 0 AND ls_result-redeemed_profile_parameter IS NOT INITIAL.
          PERFORM call_rz11 USING ls_result-redeemed_profile_parameter.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "on_double_click

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION


FORM show_result.

* reference to ALV objects
  DATA: lr_functions_list      TYPE REF TO cl_salv_functions_list,
*        lr_functions           TYPE REF TO cl_salv_functions,
        lr_selections          TYPE REF TO cl_salv_selections,
        lr_columns             TYPE REF TO cl_salv_columns_table,
*        lr_column              TYPE REF TO cl_salv_column_table,
*        lr_sorts               TYPE REF TO cl_salv_sorts.
        lr_events              TYPE REF TO cl_salv_events_table,
        lr_functional_settings TYPE REF TO cl_salv_functional_settings,
        lr_hyperlinks          TYPE REF TO cl_salv_hyperlinks,
        lr_tooltips            TYPE REF TO cl_salv_tooltips,
        lr_layout              TYPE REF TO cl_salv_layout,
        ls_key                 TYPE salv_s_layout_key,
*        lr_content             TYPE REF TO cl_salv_form_element,
        lr_grid_header         TYPE REF TO cl_salv_form_layout_grid,
        lr_grid_footer         TYPE REF TO cl_salv_form_layout_grid,
        lr_display_settings    TYPE REF TO cl_salv_display_settings.

  DATA: lr_exception TYPE REF TO cx_salv_error,
        lv_message   TYPE bal_s_msg.

  DATA: lr_column TYPE REF TO cl_salv_column_list,
        lr_sorts  TYPE REF TO cl_salv_sorts.

* Color of column
  DATA: ls_color_key     TYPE lvc_s_colo,
        ls_color_comment TYPE lvc_s_colo.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_alv_table
        CHANGING
          t_table      = lt_result ).

    CATCH cx_salv_msg
          INTO lr_exception.
      lv_message = lr_exception->get_message( ).
      MESSAGE ID lv_message-msgid TYPE lv_message-msgty
              NUMBER lv_message-msgno
              WITH lv_message-msgv1 lv_message-msgv2
                   lv_message-msgv3 lv_message-msgv4.
  ENDTRY.

* Set the default ALV functions
  lr_functions_list = gr_alv_table->get_functions( ).
  "lr_functions_list->set_detail( abap_true ).
  "lr_functions_list->set_default( abap_true ).
  lr_functions_list->set_all( abap_true ).

  " Selection mode: single cell
  lr_selections   = gr_alv_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

* Set the columns visible
  lr_columns = gr_alv_table->get_columns( ).
  lr_columns->set_optimize( if_salv_c_bool_sap=>true ).

* Set the fields description and field attributes

  ls_color_key-col      = col_key.
  ls_color_comment-col  = col_normal.

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
* SET_CELL_TYPE           Set Cell Type
* SET_DROPDOWN_ENTRY      Set Handle for Dropdown
* SET_HYPERLINK_ENTRY     Set Handle for Dropdown
* SET_F4                  Set Column with F1 Help
* SET_F4_CHECKTABLE       Set Check Table for F4 Help
* SET_KEY_PRESENCE_REQUIRED Set Key Columns as Always Visible
* SET_SYMBOL              Set Column as Symbol Column
* SET_TEXT_COLUMN         Set Text Column

  TRY.
      " Adjust fields
      "                            12345678901234567890
      " lr_column->set_zero( if_salv_c_bool_sap=>false ).
      " lr_column->set_short_text( 'Status' ).
      " lr_column->set_medium_text( 'User status' ).
      " lr_column->set_long_text( 'User status' ).

      lr_column ?= lr_columns->get_column( 'NAME' ).
      lr_column->set_key( if_salv_c_bool_sap=>true ).
      lr_column->set_color( ls_color_key ).

      lr_column ?= lr_columns->get_column( 'TYPE' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).

      lr_column ?= lr_columns->get_column( 'ATTRIBUTE_TYPE' ).
      lr_column->set_short_text( 'Type' ).
      lr_column->set_medium_text( 'Attribute type' ).
      lr_column->set_long_text( 'Attribute type' ).

      lr_column ?= lr_columns->get_column( 'DDIC_DATAELEMENT' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).

      lr_column ?= lr_columns->get_column( 'DEFAULT_VALUE' ).

      DATA:
        ls_sec_policy_cust TYPE sec_policy_cust,
        policy_nr(2)       TYPE n,
        column_name        TYPE lvc_fname VALUE 'POLICY__'.
      DO max_columns TIMES.
        policy_nr = sy-index.
        column_name+6(2) = policy_nr.
        lr_column ?= lr_columns->get_column( column_name ).
        READ TABLE lt_sec_policy_cust INTO ls_sec_policy_cust INDEX sy-index.
        IF sy-subrc IS INITIAL.
          "data tooltip TYPE LVC_TIP. "char 40
          "lr_column->SET_TOOLTIP( ls_sec_policy_cust-name ).
          lr_column->set_short_text( ls_sec_policy_cust-name(10) ).
          lr_column->set_medium_text( ls_sec_policy_cust-name(20) ).
          lr_column->set_long_text( ls_sec_policy_cust-name(40) ).
        ELSE.
          lr_column->set_technical( if_salv_c_bool_sap=>true ).
        ENDIF.
      ENDDO.

      lr_column ?= lr_columns->get_column( 'REDEEMED_PROFILE_PARAMETER' ).

      lr_column ?= lr_columns->get_column( 'PROFILE_PARAMETER_VALUE' ).
      lr_column->set_short_text( 'Par. Val.' ).
      lr_column->set_medium_text( 'Profile par. value' ).
      lr_column->set_long_text( 'Current profile parameter value' ).

      lr_column ?= lr_columns->get_column( 'KERNEL_DEFAULT_VALUE' ).
      lr_column->set_short_text( 'Kernel' ).
      lr_column->set_medium_text( 'Kernel default' ).
      lr_column->set_long_text( 'Kernel default value' ).

      lr_column ?= lr_columns->get_column( 'DESCRIPTION' ).
      lr_column->set_short_text( 'Text' ).
      lr_column->set_medium_text( 'Description' ).
      lr_column->set_long_text( 'Description' ).

    CATCH cx_salv_not_found
      INTO lr_exception.
      lv_message = lr_exception->get_message( ).
      MESSAGE ID lv_message-msgid TYPE lv_message-msgty
              NUMBER lv_message-msgno
              WITH lv_message-msgv1 lv_message-msgv2
                   lv_message-msgv3 lv_message-msgv4.
  ENDTRY.

* Set the color of cells
  TRY.
      lr_columns->set_color_column( 'T_COLOR' ).
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
  ENDTRY.

* register to the events of cl_salv_table
  lr_events = gr_alv_table->get_event( ).
  CREATE OBJECT gr_alv_events.
* register to the event USER_COMMAND
  SET HANDLER gr_alv_events->on_user_command FOR lr_events.
* register to the event DOUBLE_CLICK
  SET HANDLER gr_alv_events->on_double_click FOR lr_events.

* footer
  DATA l_line TYPE i.
  CREATE OBJECT lr_grid_footer.
  l_line = 1.

* Program version (not visible on gui container)
  lr_grid_footer->create_text(
       row    = l_line
       column = 1
       text   = 'Program version:'(ver) ).
  lr_grid_footer->create_text(
       row    = l_line
       column = 2
       text   = c_program_version ).
  ADD 1 TO l_line.

  gr_alv_table->set_end_of_list( lr_grid_footer ).

* Set Title
  lr_display_settings = gr_alv_table->get_display_settings( ).
  lr_display_settings->set_list_header( 'Show security policies' ). "sy-title
  lr_display_settings->set_list_header_size(
    cl_salv_display_settings=>c_header_size_small ).
  lr_display_settings->set_no_merging( if_salv_c_bool_sap=>true ).

* display the table
  gr_alv_table->display( ).

ENDFORM.

FORM call_view_cluster
  USING policy_name TYPE security_policy_name.

  CHECK policy_name IS NOT INITIAL.

  " Selection for view cluster
  DATA:
    ls_dba_sellist TYPE          vimsellist,
    lt_dba_sellist TYPE TABLE OF vimsellist.
  CLEAR ls_dba_sellist.
  ls_dba_sellist-viewfield = 'NAME'.
  ls_dba_sellist-operator  = 'EQ'.
  ls_dba_sellist-value     = policy_name.
  "ls_DBA_SELLIST-AND_OR    = 'AND'.
  APPEND ls_dba_sellist TO lt_dba_sellist.

  DATA:
    ls_dba_sellist_cluster TYPE LINE OF  vclty_sellist_table,
    lt_dba_sellist_cluster TYPE          vclty_sellist_table.
  ls_dba_sellist_cluster-object = 'V_SEC_POLICY_CUS'. "first_object
  ls_dba_sellist_cluster-sellist[] = lt_dba_sellist[].
  APPEND ls_dba_sellist_cluster TO lt_dba_sellist_cluster.

  CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
    EXPORTING
      viewcluster_name             = 'SEC_POLICY_VC'
      start_object                 = 'V_SEC_POLICY_RT' "dependend object
      maintenance_action           = 'S'
*     READ_KIND                    = ' '
*     SHOW_SELECTION_POPUP         = ' '
*     CORR_NUMBER                  = ' '
*     NO_WARNING_FOR_CLIENTINDEP   = ' '
*     RFC_DESTINATION              = ' '
*     SUPPRESS_WA_POPUP            = ' '
    TABLES
*     DBA_SELLIST                  = lt_DBA_SELLIST
      dba_sellist_cluster          = lt_dba_sellist_cluster
*     EXCL_CUA_FUNCT_ALL_OBJECTS   =
*     EXCL_CUA_FUNCT_CLUSTER       =
*     DPL_SELLIST_FOR_START_OBJECT =
    EXCEPTIONS
      client_reference             = 1
      foreign_lock                 = 2
      viewcluster_not_found        = 3
      viewcluster_is_inconsistent  = 4
      missing_generated_function   = 5
      no_upd_auth                  = 6
      no_show_auth                 = 7
      object_not_found             = 8
      no_tvdir_entry               = 9
      no_clientindep_auth          = 10
      invalid_action               = 11
      saving_correction_failed     = 12
      system_failure               = 13
      unknown_field_in_dba_sellist = 14
      missing_corr_number          = 15
      OTHERS                       = 16.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

FORM call_rz11
  USING profile_parameter.

  DATA: bdcdata_wa  TYPE bdcdata,
        bdcdata_tab TYPE TABLE OF bdcdata.

  CHECK profile_parameter IS NOT INITIAL.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = 'RZ11'
    EXCEPTIONS
      ok     = 0
      not_ok = 2
      OTHERS = 3.
  IF sy-subrc <> 0.
    MESSAGE e172(00) WITH 'RZ11'.
  ENDIF.

  CLEAR bdcdata_tab. REFRESH bdcdata_tab.
  bdcdata_wa-program  = 'RSPFLDOC'.
  bdcdata_wa-dynpro   = '1000'.
  bdcdata_wa-dynbegin = 'X'.
  APPEND bdcdata_wa TO bdcdata_tab.

  CLEAR bdcdata_wa-dynbegin.
  bdcdata_wa-fnam = 'TPFYSTRUCT-NAME'.
  bdcdata_wa-fval = profile_parameter.
  APPEND bdcdata_wa TO bdcdata_tab.

  SET PARAMETER ID 'SBMTRP' FIELD abap_true.

  CALL TRANSACTION 'RZ11' WITH AUTHORITY-CHECK
    USING bdcdata_tab
    MODE 'E'
    UPDATE 'S'.

  SET PARAMETER ID 'SBMTRP' FIELD abap_false.

ENDFORM.
