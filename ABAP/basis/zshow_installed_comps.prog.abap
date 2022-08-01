*&---------------------------------------------------------------------*
*& Report  ZSHOW_INSTALLED_COMPS
*& Show installed software components and verify the age of the support packages
*&---------------------------------------------------------------------*
REPORT zshow_installed_comps.

TYPES:
  BEGIN OF ts_result,
    component  TYPE comp_props-component,
    release    TYPE comp_props-release,
    sp         TYPE comp_props-sp,
    sp_level   TYPE comp_props-sp_level,
    imple_date TYPE pat03-imple_date,
    deliv_date TYPE pat03-deliv_date,
    age_month  TYPE i,
    desc_text  TYPE comp_props-desc_text,
    t_color    TYPE lvc_t_scol,
  END OF ts_result.
DATA:
  ls_result TYPE ts_result,
  lt_result TYPE TABLE OF ts_result.

START-OF-SELECTION.

  PERFORM load_data.
  PERFORM display_list.

FORM load_data.

*DATA IV_PATCHABLE_ONLY TYPE SPAM_CVERS-COMP_TYPE.
*DATA IV_ACTIVE_ONLY    TYPE SPAM_CVERS-COMP_TYPE.
*DATA IV_BUFFERED       TYPE CHAR1.
*DATA IV_LANGUAGE       TYPE LANGU.
*DATA TT_COMPTAB        TYPE STANDARD TABLE OF SPAM_CVERS.
  DATA et_components     TYPE STANDARD TABLE OF comp_props.
  DATA ls_components     TYPE comp_props.
*DATA ET_COMPLAYER      TYPE STANDARD TABLE OF SPAM_CLAYR.
*DATA ET_CVERS_SUB      TYPE STANDARD TABLE OF CVERS_SUB.
*DATA ET_CPK            TYPE STANDARD TABLE OF SPAM_CPK.

  CALL FUNCTION 'OCS_GET_INSTALLED_COMPS'
    EXPORTING
      iv_patchable_only = 'X'
      iv_active_only    = 'X'
*     IV_BUFFERED       = 'X'
*     IV_LANGUAGE       = SY-LANGU
    TABLES
*     TT_COMPTAB        = TT_COMPTAB
      et_components     = et_components
*     ET_COMPLAYER      = ET_COMPLAYER
*     ET_CVERS_SUB      = ET_CVERS_SUB
*     ET_CPK            = ET_CPK
    EXCEPTIONS
      no_release_found  = 1
      wrong_release     = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    RETURN.
  ENDIF.

  "sort ET_COMPONENTS by COMPONENT.

  LOOP AT et_components INTO ls_components.

    CLEAR ls_result.
    MOVE-CORRESPONDING ls_components TO ls_result.

    " Calculate age
    DATA pat03 TYPE pat03.
    CLEAR: pat03.
    SELECT SINGLE * FROM pat03 INTO pat03
      WHERE patch = ls_components-sp.
    IF pat03-deliv_date+4(2) = '  ' OR pat03-deliv_date+6(2) = '  '.
      CLEAR pat03-deliv_date. " Clean incomplete data
    ENDIF.
    ls_result-imple_date = pat03-imple_date.
    ls_result-deliv_date = pat03-deliv_date.
    IF pat03-deliv_date IS NOT INITIAL.
      ls_result-age_month = ( sy-datum - pat03-deliv_date ) / 30.
    ENDIF.

    DATA ls_color TYPE lvc_s_scol.
    CLEAR ls_color.
    IF ls_result-age_month >= 24.     " RED: Age is at least 24 month
      ls_color-fname     = 'COMPONENT'.
      ls_color-color-col = col_negative.
      APPEND ls_color TO ls_result-t_color.
      ls_color-fname     = 'AGE_MONTH'.
      ls_color-color-col = col_negative.
      APPEND ls_color TO ls_result-t_color.

    ELSEIF ls_result-age_month >= 18. " YELLOW: Age is at least 18 month
      ls_color-fname     = 'COMPONENT'.
      ls_color-color-col = col_total.
      APPEND ls_color TO ls_result-t_color.
      ls_color-fname     = 'AGE_MONTH'.
      ls_color-color-col = col_total.
      APPEND ls_color TO ls_result-t_color.

    ELSE.
      ls_color-fname     = 'COMPONENT'.
      ls_color-color-col = col_key.
      APPEND ls_color TO ls_result-t_color.

    ENDIF.

    APPEND ls_result TO lt_result.

  ENDLOOP.
ENDFORM.

DATA: gr_table   TYPE REF TO cl_salv_table.

FORM display_list .

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = lt_result ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  lr_functions = gr_table->get_functions( ).
  lr_functions->set_default( abap_true ).

  DATA: lr_columns TYPE REF TO cl_salv_columns_table.

  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).

  TRY.
      lr_columns->set_color_column( 'T_COLOR' ).
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
  ENDTRY.

  DATA: lr_column TYPE REF TO cl_salv_column.

  TRY.
      lr_column = lr_columns->get_column( 'SP_LEVEL' ).
      lr_column->set_short_text( 'SP level' ).
      lr_column->set_medium_text( 'SP level' ).
      lr_column->set_long_text( 'Support Package level' ).
      lr_column->SET_ZERO( abap_true  ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = lr_columns->get_column( 'IMPLE DATE' ).
      lr_column->set_short_text( 'Implement.' ).
      lr_column->set_medium_text( 'Implementation' ).
      lr_column->set_long_text( 'Implementation date' ).
      lr_column->SET_ZERO( abap_false  ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = lr_columns->get_column( 'DELIV_DATE' ).
      lr_column->set_short_text( 'Delivery' ).
      lr_column->set_medium_text( 'Delivery date' ).
      lr_column->set_long_text( 'Delivery date' ).
      lr_column->SET_ZERO( abap_false  ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = lr_columns->get_column( 'AGE_MONTH' ).
      lr_column->set_short_text( 'Age month' ).
      lr_column->set_medium_text( 'Age in month' ).
      lr_column->set_long_text( 'Age in month' ).
      lr_column->SET_ZERO( abap_false  ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  gr_table->display( ).

ENDFORM.
