*&---------------------------------------------------------------------*
*& Report ZRSPFPAR_DYNAMIC_CD
*&
*& The report shows the change documents for dynamic parameters
*& A double click on the parameter name calls RZ11
*&
*& The report is loosly based on report RSPFPAR
*& Tipps for RSPFPAR:
*& - you can show the metadata, too
*& - Filtering on metadata is tricky because of the upper case conversion
*&   However, you can use filter values like Y* or N* on these columns
*&
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Source: https://github.com/SAP-samples/security-services-tools
*&
*& 29.03.2023 Show all instance specific change documents
*&            Show the changing client if available depending on the release
*& 10.02.2023 Initial version
*&---------------------------------------------------------------------*
REPORT zrspfpar_dynamic_cd
  MESSAGE-ID pf.

CONSTANTS: c_program_version(30) TYPE c VALUE '29.03.2023 S41'.

* a) Syslog entry Q1 9 for the parameter_name and Q1 A for the value.
* b) History about recent changes

DATA ss_name TYPE pfeparname.
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ps_name FOR FIELD s_name.
  SELECT-OPTIONS s_name FOR ss_name LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS p_all AS CHECKBOX.
  SELECTION-SCREEN COMMENT 3(60) ps_all FOR FIELD p_all.
SELECTION-SCREEN END OF LINE.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) ps_lout FOR FIELD p_layout.
  PARAMETERS       p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      initialization,

      f4_s_name,

      f4_p_layout
        CHANGING layout TYPE disvariant-variant,

      at_selscr_on_p_layout
        IMPORTING layout TYPE disvariant-variant,

      start_of_selection.

    METHODS:

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

*      on_single_click for event link_click of cl_salv_events_table
*        importing row column.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_result,
        " header
        name               TYPE spfl_parameter_name,
        csn_component      TYPE spfl_parameter_csn_component,
        is_dynamic         TYPE spfl_is_dynamic_parameter,
        pgroup             TYPE spfl_parameter_group,
        description        TYPE spfl_parameter_description,
        restriction_values TYPE spfl_restriction_values,
        type_desc          TYPE string, " type SPFL_PARAMETER_TYPE,

        instance_name      TYPE msname2,

        first_log_entry    TYPE timestamp,
        no_of_changes      TYPE i,

        " First entry in details
        "instance_start_time type timestamp, " type c length 14, " yyyymmddhhmmss
        "original_value      type string,

        " details
        timestamp          TYPE timestamp,
        changed_cli        TYPE sy-mandt,
        changed_by         TYPE sy-uname,
        value              TYPE string,

        t_color            TYPE lvc_t_scol,
      END OF  ts_result,
      tt_result TYPE STANDARD TABLE OF ts_result.

    CLASS-METHODS:

      get_history,

      show_result,

      call_rz11
        IMPORTING parameter_name TYPE spfl_parameter_name.

    CLASS-DATA:

      hide_changed_cli type abap_bool value abap_true,

      " main data table
      ls_result      TYPE ts_result,
      lt_result      TYPE tt_result,

      " main ALV table
      lr_alv_table   TYPE REF TO cl_salv_table,

      ls_alv_variant TYPE disvariant,

      " for handling the events of cl_salv_table
      lr_alv_events  TYPE REF TO lcl_report.

ENDCLASS.                    "lcl_report DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.
*    DATA(myname) = cl_abap_syst=>get_instance_name( ).

    sy-title = `Show history of dynamic profile parameters`." && ` on ` && myname.

    ps_name = 'Dynamic profile parameter'.
    ps_all  = 'Show unchanged dynamic parameters, too'.

    ps_lout = 'Layout'.

    CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
        SEPARATED BY space.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'RZ11'
      EXCEPTIONS
        ok     = 0
        not_ok = 2
        OTHERS = 3.
    IF sy-subrc <> 0.
      MESSAGE e002. "You have no display authorization for CCMS tools
    ENDIF.

  ENDMETHOD.

  METHOD f4_s_name.

    TYPES:
      BEGIN OF ts_F4_value,
        name        TYPE spfl_parameter_name,
        description TYPE spfl_parameter_description,
      END OF ts_F4_value.

    DATA:
      F4_value TYPE ts_F4_value.

    STATICS:
      F4_value_tab TYPE TABLE OF ts_F4_value.

    STATICS:
      lt_METADATA   TYPE  spfl_parameter_metadata_list_t.

    " Get all parameters
    IF lt_METADATA IS INITIAL.
      DATA(subrc) = cl_spfl_profile_parameter=>get_all_metadata(
        IMPORTING
           metadata  = lt_METADATA ).

      LOOP AT lt_METADATA INTO DATA(ls_METADATA)
        WHERE is_dynamic IS NOT INITIAL.

        F4_value-name        = ls_METADATA-name.
        F4_value-description = ls_METADATA-description.
        APPEND F4_value TO F4_value_tab.
      ENDLOOP.
      SORT F4_value_tab BY name.
    ENDIF.

    DATA(progname) = sy-repid.
    DATA(dynnum)   = sy-dynnr.
    DATA field type dynfnam.
    DATA stepl type sy-stepl.
    GET CURSOR FIELD field LINE stepl.
    DATA return_tab TYPE TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'NAME'    " see ts_F4_value
        dynpprog        = progname
        dynpnr          = dynnum
        dynprofield     = field
        stepl           = stepl
        value_org       = 'S'
      TABLES
*       field_tab       = field_tab
        value_tab       = F4_value_tab
        return_tab      = return_tab " surprisingly required to get lower case values
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.

  METHOD f4_p_layout.
    "CHANGING layout TYPE disvariant-variant.

    ls_alv_variant-report  = sy-repid.
    ls_alv_variant-variant = layout.

    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant    = ls_alv_variant
        i_save        = 'A'
      IMPORTING
        es_variant    = ls_alv_variant
      EXCEPTIONS
        not_found     = 1
        program_error = 2
        OTHERS        = 3.

    IF sy-subrc = 0.
      layout = ls_alv_variant-variant.
    ELSE.
      MESSAGE s073(0k). " Keine Anzeigevariante(n) vorhanden
    ENDIF.

  ENDMETHOD.

  METHOD at_selscr_on_p_layout.
    "IMPORTING layout TYPE disvariant-variant.

    DATA: ls_variant TYPE disvariant.

    ls_variant-report  = sy-repid.
    ls_variant-variant = layout.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save        = 'A'
      CHANGING
        cs_variant    = ls_variant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.

    IF sy-subrc <> 0.
*   Selected layout variant is not found
      MESSAGE e204(0k).
    ENDIF.

    ls_alv_variant-report  = sy-repid.
    ls_alv_variant-variant = layout.

  ENDMETHOD.

  METHOD start_of_selection.

    get_history( ).
    show_result( ).

  ENDMETHOD.

  METHOD get_history.
    DATA:
      parameter_name TYPE spfl_parameter_name,
      lt_SUB_PARAM   TYPE spfl_parameter_list_t,
      ls_METADATA    TYPE spfl_parameter_metadata,
      ls_HISTORY     TYPE cl_spfl_profile_parameter=>ty_change_log_for_param,
      subrc          TYPE sy-subrc.

    " Get all parameters
    subrc = cl_spfl_profile_parameter=>get_all_parameter(
      IMPORTING
         parameter_sub  = lt_SUB_PARAM
         "parameter_usub =
    ).
    CHECK subrc IS INITIAL.

    " Process selected parameters
    LOOP AT lt_SUB_PARAM INTO DATA(ls_SUB_PARAM)
      WHERE name IN s_name.

      parameter_name = ls_SUB_PARAM-name.

      subrc  = cl_spfl_profile_parameter=>get_metadata(
          EXPORTING
            name = ls_SUB_PARAM-name
          IMPORTING
            metadata =  ls_METADATA ).
      CHECK subrc IS INITIAL.

      " Process dynamic parameters only
      CHECK ls_METADATA-is_dynamic = 1.

      " Get status (not used - it seems it's never the case)
      "DATA(obsolete) = cl_spfl_profile_parameter=>check_obsolete( EXPORTING name = parameter_name ).

      " Interpret parameter type
      DATA:
        type_desc TYPE string.
      CASE ls_METADATA-type.
        WHEN if_parameter_types=>string.       type_desc = 'String'.
        WHEN if_parameter_types=>int.          type_desc = 'Int'.
        WHEN if_parameter_types=>double.       type_desc = 'Double'.
        WHEN if_parameter_types=>int_range.    type_desc = 'Int range'.
        WHEN if_parameter_types=>double_range. type_desc = 'Double range'.
        WHEN if_parameter_types=>enum.         type_desc = 'Enum'.
        WHEN if_parameter_types=>bool.         type_desc = 'Boolean'.
        WHEN OTHERS.                           type_desc = ls_METADATA-type.
      ENDCASE.

      " Copy data
      CLEAR ls_result.
      ls_result-name               = parameter_name.
      ls_result-csn_component      = ls_METADATA-csn_component.
      ls_result-is_dynamic         = ls_METADATA-is_dynamic.
      ls_result-pgroup             = ls_METADATA-pgroup.
      ls_result-description        = ls_METADATA-description.
      ls_result-type_desc          = type_desc.
      ls_result-restriction_values = ls_METADATA-restriction_values.

      " Get history of a dynamic parameter (for current instance)
*      cl_spfl_profile_parameter=>get_dyn_value_change_history(
*        EXPORTING
*          i_parameter_name = parameter_name
*        IMPORTING
*          e_history        = ls_HISTORY ).
      " Get history of a dynamic parameter (all instances)
      types:
        begin of ty_log_key,
          instance_name   type msname2,
          parameter_name type pfeparname,
        end of ty_log_key.
      data: param type ty_log_key.
      SELECT
          instance_name,
          parameter_name
        FROM pfl_indx
        INTO (
          @param-instance_name,
          @param-parameter_name
        )
        WHERE
          parameter_name = @parameter_name
        ORDER BY instance_name.
      try.
        import log to ls_HISTORY
           from database pfl_indx(cl)
           id param.
          catch
            cx_sy_import_format_error
            cx_sy_import_mismatch_error
            cx_sy_conversion_codepage
            cx_sy_expimp_db_sql_error.
      endtry.

      " Process all respective changed parameters only
      CHECK p_all = 'X' OR ls_HISTORY-no_of_changes > 0 OR lines( ls_HISTORY-log ) > 0.

      " Copy data
      ls_result-first_log_entry    = ls_HISTORY-first_log_entry.
      ls_result-no_of_changes      = ls_HISTORY-no_of_changes.
      ls_result-first_log_entry    = ls_HISTORY-first_log_entry.

      ls_result-instance_name      = param-instance_name.

      " Insert original value
      ls_result-timestamp          = ls_HISTORY-instance_start_time.
      ls_result-changed_cli        = SPACE.
      ls_result-changed_by         = 'Original'.
      ls_result-value              = ls_HISTORY-original_value.
      CLEAR ls_result-t_color.

      " Get original value
      IF ( ls_HISTORY-no_of_changes = 0 OR lines( ls_HISTORY-log ) = 0 ) AND ls_HISTORY-original_value IS INITIAL.
        subrc = cl_spfl_profile_parameter=>get_value(
          EXPORTING
            server_name  = conv #( param-instance_name )
            name         = parameter_name
           IMPORTING
             value       = ls_result-value
          ).
      ENDIF.

      APPEND ls_result TO lt_result.

      LOOP AT ls_HISTORY-log INTO DATA(ls_log).
        IF sy-tabix = 1 AND ls_HISTORY-first_log_entry <> ls_log-timestamp.
          " different first log entry
        ENDIF.

        " Does field CHANGED_CLI exists?
        assign ('LS_LOG-CHANGED_CLI') to FIELD-SYMBOL(<changed_cli>).
        IF sy-subrc = 0.
          hide_changed_cli = abap_false.
        endif.

        ls_result-timestamp        = ls_log-timestamp.
        ls_result-changed_by       = ls_log-changed_by.
        IF <changed_cli> is ASSIGNED.
          ls_result-changed_cli    = <changed_cli>.
        endif.
        ls_result-value            = ls_log-new_value.

        CLEAR ls_result-t_color.
        IF ls_result-value = ls_HISTORY-original_value.
          APPEND VALUE #( fname = 'TIMESTAMP'   color-col = col_POSITIVE ) TO ls_result-t_color.
          APPEND VALUE #( fname = 'CHANGED_CLI' color-col = col_POSITIVE ) TO ls_result-t_color.
          APPEND VALUE #( fname = 'CHANGED_BY'  color-col = col_POSITIVE ) TO ls_result-t_color.
          APPEND VALUE #( fname = 'VALUE'       color-col = col_POSITIVE ) TO ls_result-t_color.
        ELSE.
          APPEND VALUE #( fname = 'TIMESTAMP'   color-col = col_TOTAL ) TO ls_result-t_color.
          APPEND VALUE #( fname = 'CHANGED_CLI' color-col = col_TOTAL ) TO ls_result-t_color.
          APPEND VALUE #( fname = 'CHANGED_BY'  color-col = col_TOTAL ) TO ls_result-t_color.
          APPEND VALUE #( fname = 'VALUE'       color-col = col_TOTAL ) TO ls_result-t_color.
        ENDIF.

        APPEND ls_result TO lt_result.
      ENDLOOP.

      ENDSELECT.
      IF sy-subrc ne 0 and p_all = 'X'.
        " No change documents, get current value of current application server
        ls_result-instance_name = cl_abap_syst=>get_instance_name( ).
        cl_spfl_profile_parameter=>get_value(
          EXPORTING
            "server_name  =
            name         = parameter_name
           IMPORTING
             value       = ls_result-value
          ).
      APPEND ls_result TO lt_result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD show_result.

    TRY.
        cl_salv_table=>factory(
            EXPORTING
              list_display = abap_false "  false: grid, true: list
          IMPORTING
            r_salv_table = lr_alv_table
          CHANGING
            t_table      = lt_result ).
      CATCH cx_salv_msg.
    ENDTRY.

*... activate ALV generic Functions
    DATA(lr_functions) = lr_alv_table->get_functions( ).
    lr_functions->set_all( abap_true ).

*... set the display settings
    DATA(lr_display) = lr_alv_table->get_display_settings( ).
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
    DATA(lr_functional) = lr_alv_table->get_functional_settings( ).
    TRY.
        lr_functional->set_sort_on_header_click( abap_true ).
        "lr_functional->set_f2_code( f2code ).
        "lr_functional->set_buffer( gs_test-settings-functional-buffer ).
      CATCH cx_salv_method_not_supported.
    ENDTRY.

* ...Set the layout
    DATA(lr_layout) = lr_alv_table->get_layout( ).
    DATA ls_layout_key TYPE salv_s_layout_key.
    ls_layout_key-report = sy-repid.
    lr_layout->set_key( ls_layout_key ).
    lr_layout->set_initial_layout( p_layout ).
    AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
                        ID 'ACTVT' FIELD '23'.
    IF sy-subrc = 0.
      lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ) . "no restictions
    ELSE.
      lr_layout->set_save_restriction( cl_salv_layout=>restrict_user_dependant ) . "user dependend
    ENDIF.

*... sort
    TRY.
        DATA(lr_sorts) = lr_alv_table->get_sorts( ).
        lr_sorts->add_sort( 'NAME' ).
        " Additional sort fields to hide replicated data
        lr_sorts->add_sort( 'CSN_COMPONENT' ).
        lr_sorts->add_sort( 'IS_DYNAMIC' ).
        lr_sorts->add_sort( 'PGROUP' ).
        lr_sorts->add_sort( 'DESCRIPTION' ).
        lr_sorts->add_sort( 'RESTRICTION_VALUES' ).
        lr_sorts->add_sort( 'TYPE_DESC' ).
        lr_sorts->add_sort( 'INSTANCE_NAME' ).
        lr_sorts->add_sort( 'FIRST_LOG_ENTRY' ).
        lr_sorts->add_sort( 'NO_OF_CANGES' ).

      CATCH cx_salv_data_error cx_salv_existing cx_salv_not_found.
    ENDTRY.

*... set column appearance
    DATA(lr_columns) = lr_alv_table->get_columns( ).
    lr_columns->set_optimize( abap_true ). " Optimize column width

*... set the color of cells
    TRY.
        lr_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

* register to the events of cl_salv_table
    DATA(lr_events) = lr_alv_table->get_event( ).
    CREATE OBJECT lr_alv_events.
* register to the event USER_COMMAND
    SET HANDLER lr_alv_events->on_user_command FOR lr_events.
* register to the event DOUBLE_CLICK
    SET HANDLER lr_alv_events->on_double_click FOR lr_events.

* set selection mode
    DATA(lr_selections) = lr_alv_table->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).


    DATA lr_column     TYPE REF TO cl_salv_column_table.        " Columns in Simple, Two-Dimensional Tables
    DATA color         TYPE lvc_s_colo.
    TRY.
        " Header
        color-col = 4. " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange

        lr_column ?= lr_columns->get_column( 'NAME' ).
        lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'CSN_COMPONENT' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'IS_DYNAMIC' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'PGROUP' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'DESCRIPTION' ).

        lr_column ?= lr_columns->get_column( 'RESTRICTION_VALUES' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'TYPE_DESC' ).
        lr_column->set_long_text( 'Profile parameter type' ).
        lr_column->set_medium_text( 'Parameter type' ).
        lr_column->set_short_text( 'Par. type' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'INSTANCE_NAME' ).
        "lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'FIRST_LOG_ENTRY' ).
        lr_column->set_long_text( 'First log entry' ).
        lr_column->set_medium_text( 'First log entry' ).
        lr_column->set_short_text( 'FirstEntry' ).
        lr_column->set_edit_mask( '==TSTMP' ). " convert time stamps
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'NO_OF_CHANGES' ).
        lr_column->set_long_text( 'Count of change records' ).
        lr_column->set_medium_text( 'Count of records' ).
        lr_column->set_short_text( 'Count' ).
        lr_column->set_visible( abap_false ).

        " Details
        color-col = 2. " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange

        lr_column ?= lr_columns->get_column( 'TIMESTAMP' ).
        lr_column->set_edit_mask( '==TSTMP' ). " convert time stamps
        "lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'CHANGED_CLI' ).
        lr_column->set_technical( hide_changed_cli ).
        "lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'CHANGED_BY' ).
        "lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'VALUE' ).
        lr_column->set_long_text( 'Value' ).
        lr_column->set_medium_text( 'Value' ).
        lr_column->set_short_text( 'Value' ).
        "lr_column->set_color( color ).


      CATCH cx_salv_not_found.
    ENDTRY.

*... show it
    lr_alv_table->display( ).

  ENDMETHOD.

  METHOD on_user_command.
*    importing e_salv_function

    " Get selected item(s)
    DATA(lr_selections)   = lr_alv_table->get_selections( ).
    DATA(ls_cell)         = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    CASE e_salv_function.

      WHEN 'PICK'. " Double click

        " Show trusted systems
        IF   ls_cell-columnname = 'NAME'.

          IF ls_cell-row > 0.

            READ TABLE lt_result INTO DATA(ls_result) INDEX ls_cell-row.
            CHECK sy-subrc = 0.

            call_RZ11( ls_result-name ).

          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "on_user_command

  METHOD on_double_click.
*   importing row column

    " Get selected item(s)
    DATA(lr_selections) = lr_alv_table->get_selections( ).
    DATA(ls_cell) = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    " Show trusted systems
    IF   column = 'NAME'.

      IF row > 0.

        READ TABLE lt_result INTO DATA(ls_result) INDEX row.
        CHECK sy-subrc = 0.

        call_RZ11( ls_result-name ).

      ENDIF.
    ENDIF.

  ENDMETHOD.                    "on_double_click

  METHOD call_rz11.
    "IMPORTING parameter_name TYPE spfl_parameter_name.

    DATA:
      ls_bdcdata TYPE          bdcdata,
      lt_bdcdata TYPE TABLE OF bdcdata.

    AUTHORITY-CHECK OBJECT 'S_TCODE'
             ID 'TCD' FIELD 'RZ11'.
    CHECK sy-subrc = 0.

    CLEAR: ls_bdcdata, lt_bdcdata.
    ls_bdcdata-program  = 'RSPFLDOC'.
    ls_bdcdata-dynpro   = '1000'.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata-dynbegin.
    ls_bdcdata-fnam = 'TPFYSTRUCT-NAME'.
    ls_bdcdata-fval = parameter_name.
    APPEND ls_bdcdata TO lt_bdcdata.
    CALL TRANSACTION 'RZ11'
                            USING lt_bdcdata
                            MODE 'E'
                            UPDATE 'S'.
  ENDMETHOD.

ENDCLASS.                    "lcl_report IMPLEMENTATION

*----------------------------------------------------------------------*
*      REPORT events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_report=>initialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-low.
  lcl_report=>f4_s_name( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-high.
  lcl_report=>f4_s_name( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  lcl_report=>f4_p_layout( CHANGING layout = p_layout ).

AT SELECTION-SCREEN ON p_layout.
  CHECK NOT p_layout IS INITIAL.
  lcl_report=>at_selscr_on_p_layout( p_layout ).

START-OF-SELECTION.
  lcl_report=>start_of_selection( ).
