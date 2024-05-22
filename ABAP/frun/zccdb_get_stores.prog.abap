*&---------------------------------------------------------------------*
*& Report ZCCDB_GET_STORES
*&---------------------------------------------------------------------*
*& Show an overview about configuration stores in FRUN
*& Author: Frank Buchholz, SAP CoE Security Services
*& https://github.com/SAP-samples/security-services-tools
*&
*& All select options have a value help. After pressing ENTER the already
*& entered values are used to restrict the value help for the current field.
*&
*& You can select stores and use function F2 or you can double click on
*& an entry to show the data of the selected stores. All selected stores
*& have to contain the same field list.
*&
*& You can add filter values to reduce the data shown. Both filter fields
*& are connected with a locical AND.
*&
*& A double click on field value DDIC_TABLENAME shows the table in SE16
*& instead of using the internal viewer.
*&
*& 27.01.2023 Initial version
*& 21.05.2024 Enhance robustness if case of no data
*&---------------------------------------------------------------------*
REPORT zccdb_get_stores.

CONSTANTS: c_program_version(30) TYPE c VALUE '21.05.2024 FQ4'.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text001.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(25) ss_sid FOR FIELD extsid.
    SELECT-OPTIONS extsid   FOR ('LMDB_ESID').        " Extended System ID
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(25) ss_type FOR FIELD systype.
    SELECT-OPTIONS systype  FOR ('LMDB_SYSTEM_TYPE').  " Technical System Type
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(25) ss_id FOR FIELD sci_id.
    SELECT-OPTIONS sci_id   FOR ('CCDB_SCI_ID').       " Store Collect Item
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(25) ss_store FOR FIELD storenam.
    SELECT-OPTIONS storenam FOR ('CCDB_STORE_NAME').   " Store Name
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK filter WITH FRAME TITLE text002.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(25) ss_val_1 FOR FIELD val_1.
    SELECT-OPTIONS val_1 FOR ('TEXT60').               " Filter value
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(25) ss_val_2 FOR FIELD val_2.
    SELECT-OPTIONS val_2 FOR ('TEXT60').               " Filter value
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS showdata AS CHECKBOX.
    SELECTION-SCREEN COMMENT 3(25) ss_data FOR FIELD showdata.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK filter.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ps_lout FOR FIELD p_layout.
  PARAMETERS       p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

*-----------------------------------------------------------------------

INITIALIZATION.

  sy-title = 'Show configuration stores and content'(tit).

  text001  = 'Store selection'(001).
  ss_sid   = 'System'(002).
  ss_type  = 'System type'(003).
  ss_id    = 'Store item'(004).
  ss_store = 'Store name'(005).

  text002  = 'Filter for configuration items or values'(006).
  ss_val_1 = 'Value'(007).
  ss_val_2 = 'and value'(008).

  ss_data  = 'Show data directly'(009).

  ps_lout  = 'Layout'(010).

  CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
    SEPARATED BY space.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR extsid-low.
  PERFORM f4_extsid USING 'EXTSID-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR extsid-high.
  PERFORM f4_extsid USING 'EXTSID-HIGH'.

FORM f4_extsid USING l_dynprofield  TYPE help_info-dynprofld.

  TYPES:
    BEGIN OF ts_value_tab,
      extsid      TYPE lmdb_esid,             " Extended System ID
      system_type TYPE lmdb_system_type,      " Technical System Type
    END OF ts_value_tab.

  STATICS:
    "dynpro_values TYPE TABLE OF dynpread,
    "field_value   LIKE LINE OF dynpro_values,
    "field_tab     TYPE TABLE OF dfies  WITH HEADER LINE,
    value_tab      TYPE STANDARD TABLE OF ts_value_tab.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.

  "IF value_tab[] IS INITIAL.
  SELECT DISTINCT extsid, system_type
    FROM ccdb_rt_dirv_dbv                     " Store Directory - Root (Internal)
    INTO TABLE @value_tab
   WHERE system_type      IN @systype    " System type
     "AND extsid           IN @extsid     " System name
     AND sci_id           IN @sci_id     " Store id
     AND store_name       IN @storenam   " Store name
   ORDER BY extsid, system_type.
  "ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'EXTSID'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = l_dynprofield
      value_org       = 'S'
    TABLES
*     field_tab       = field_tab
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR systype-low.
  PERFORM f4_systype USING 'SYSTYPE-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR systype-high.
  PERFORM f4_systype USING 'SYSTYPE-HIGH'.

FORM f4_systype USING l_dynprofield  TYPE help_info-dynprofld.

  TYPES:
    BEGIN OF ts_value_tab,
      system_type TYPE lmdb_system_type,      " Technical System Type
    END OF ts_value_tab.

  STATICS:
    value_tab      TYPE STANDARD TABLE OF ts_value_tab.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.

  "IF value_tab[] IS INITIAL.
  SELECT DISTINCT system_type
    FROM ccdb_rt_dirv_dbv                     " Store Directory - Root (Internal)
    INTO TABLE @value_tab
   WHERE extsid           IN @extsid     " System name
     "AND system_type      IN @systype    " System type
     AND sci_id           IN @sci_id     " Store id
     AND store_name       IN @storenam   " Store name
   ORDER BY system_type.
  "ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SYSTYPE'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = l_dynprofield
      value_org       = 'S'
    TABLES
*     field_tab       = field_tab
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sci_id-low.
  PERFORM f4_sci_id USING 'SCI_ID-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR sci_id-high.
  PERFORM f4_sci_id USING 'SCI_ID-HIGH'.

FORM f4_sci_id USING l_dynprofield  TYPE help_info-dynprofld.

  TYPES:
    BEGIN OF ts_value_tab,
      sci_id   TYPE ccdb_sci_id,           " Store Collect Item
      sci_desc TYPE cof_extr_desc,         " Description
    END OF ts_value_tab.

  STATICS:
    value_tab      TYPE STANDARD TABLE OF ts_value_tab.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.

  "IF value_tab[] IS INITIAL.
  SELECT DISTINCT sci_id, sci_desc
    FROM ccdb_rt_dirv_dbv                     " Store Directory - Root (Internal)
    INTO TABLE @value_tab
   WHERE extsid           IN @extsid     " System name
     AND system_type      IN @systype    " System type
     "AND sci_id           IN @sci_id     " Store id
     AND store_name       IN @storenam   " Store name
   ORDER BY sci_id.
  "ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SCI_ID'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = l_dynprofield
      value_org       = 'S'
    TABLES
*     field_tab       = field_tab
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR storenam-low.
  PERFORM f4_storenam USING 'STORENAM-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR storenam-high.
  PERFORM f4_storenam USING 'STORENAM-HIGH'.

FORM f4_storenam USING l_dynprofield  TYPE help_info-dynprofld.

  TYPES:
    BEGIN OF ts_value_tab,
      store_name TYPE ccdb_store_name,       " Store Name
      sci_id     TYPE ccdb_sci_id,           " Store Collect Item
      sci_desc   TYPE cof_extr_desc,         " Description
    END OF ts_value_tab.

  STATICS:
    value_tab      TYPE STANDARD TABLE OF ts_value_tab.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.

  "IF value_tab[] IS INITIAL.
  SELECT DISTINCT store_name, sci_id, sci_desc
    FROM ccdb_rt_dirv_dbv                     " Store Directory - Root (Internal)
    INTO TABLE @value_tab
   WHERE extsid           IN @extsid     " System name
     AND system_type      IN @systype    " System type
     AND sci_id           IN @sci_id     " Store id
     "AND store_name       IN @storenam   " Store name
   ORDER BY store_name, sci_id.
  "ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'STORE_NAME'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = l_dynprofield
      value_org       = 'S'
    TABLES
*     field_tab       = field_tab
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON p_layout
*----------------------------------------------------------------------*

DATA: gs_alv_lout_variant TYPE disvariant.

AT SELECTION-SCREEN ON p_layout.
  CHECK NOT p_layout IS INITIAL.
  PERFORM handle_at_selscr_on_p_layout USING p_layout sy-repid 'A'.
*
FORM handle_at_selscr_on_p_layout
   USING id_varname TYPE disvariant-variant
         id_repid   TYPE sy-repid
         id_save    TYPE c.

  DATA: ls_variant TYPE disvariant.

  ls_variant-report  = id_repid.
  ls_variant-variant = id_varname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save        = id_save
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

  gs_alv_lout_variant-report  = id_repid.
  gs_alv_lout_variant-variant = id_varname.

ENDFORM.                    " handle_at_selscr_on_p_layout

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.  " ( Note 890141 )
  PERFORM handle_at_selscr_f4_p_layout USING    sy-repid 'A'
                                       CHANGING p_layout.
*
FORM handle_at_selscr_f4_p_layout
  USING    id_repid   TYPE sy-repid
           id_save    TYPE c
  CHANGING ed_varname TYPE disvariant-variant.

  gs_alv_lout_variant-report = id_repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = gs_alv_lout_variant
      i_save        = id_save
    IMPORTING
      es_variant    = gs_alv_lout_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc = 0.
    ed_varname = gs_alv_lout_variant-variant.
  ELSE.
    MESSAGE s073(0k).
*   Keine Anzeigevariante(n) vorhanden
  ENDIF.

ENDFORM.                               " handle_at_selscr_f4_p_layout

*-----------------------------------------------------------------------

CLASS main DEFINITION.

  PUBLIC SECTION.

    " main report coding
    CLASS-METHODS:
      authority_check RETURNING VALUE(subrc) TYPE i,
      get_stores,
      show_stores,
      show_all_content.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_storetab.
        INCLUDE TYPE ccdb_dirv.
    TYPES:
      "t_color     TYPE lvc_t_scol,
      "t_celltype  TYPE salv_t_int4_column,
      "t_hyperlink TYPE salv_t_int4_column,
      "t_dropdown  TYPE salv_t_int4_column,
      END OF ts_storetab,
      tt_outtab TYPE STANDARD TABLE OF ts_storetab.

    CLASS-DATA:
      lr_alv_table TYPE REF TO cl_salv_table,
      lt_storetab  TYPE tt_outtab.

    " handling the events of cl_salv_table
    CLASS-METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

    "on_single_click FOR EVENT link_click OF cl_salv_events_table
    "  IMPORTING row column.

    CLASS-METHODS:
      show_content
        IMPORTING lt_seleced_rows TYPE salv_t_row.

ENDCLASS.

CLASS main IMPLEMENTATION.

  METHOD authority_check.
    " Check general CCDB-Read-API authorization
    subrc = cl_ccdb_read=>check_read_ext_auth_read( ).
    "IF subrc NE 0.
    "  MESSAGE e016(cx_ccdb_exception). " Permission denied
    "ENDIF.
  ENDMETHOD.

  METHOD get_stores.
    " based on function CCDB_GET_STORES

    " Get Data
    SELECT *
      FROM ccdb_rt_dirv_dbv                     " Store Directory - Root (Internal)
      INTO CORRESPONDING FIELDS OF TABLE @lt_storetab
     WHERE extsid           IN @extsid     " System name
       AND system_type      IN @systype    " System type
       AND sci_id           IN @sci_id     " Store id
       AND store_name       IN @storenam   " Store name
     ORDER BY extsid, system_type, sci_id
     .

    " Perform End User Fencing ?
    IF cl_ccdb_read=>check_read_ext_auth_readall( ) NE 0.
      " End User Fencing
      DATA: lt_guid         TYPE HASHED TABLE OF cl_cof_lmdb=>ts_guid32 WITH UNIQUE KEY guid.
      DATA: lt_guid_show    TYPE HASHED TABLE OF cl_cof_lmdb=>ts_guid32 WITH UNIQUE KEY guid.
      DATA: ls_guid         TYPE cl_cof_lmdb=>ts_guid32.
      DATA: li_tabix        TYPE i.

      DATA ls_storetab      TYPE ts_storetab.
      LOOP AT lt_storetab INTO ls_storetab.
        ls_guid-guid = ls_storetab-rt_cim_guid.
        INSERT ls_guid INTO TABLE lt_guid.
        IF sy-subrc NE 0.
          " filter duplicates
        ENDIF.
      ENDLOOP.

      cl_cof_lmdb=>exec_end_user_fencing(
        EXPORTING
          im_table            = lt_guid
          im_guid_column_name = 'GUID'
        IMPORTING
          ex_table            = lt_guid_show ).
      IF lt_guid_show NE lt_guid.
        LOOP AT lt_storetab INTO ls_storetab.
          li_tabix = sy-tabix.
          READ TABLE lt_guid_show TRANSPORTING NO FIELDS
            WITH KEY guid = ls_storetab-rt_cim_guid.
          IF sy-subrc NE 0.
            DELETE lt_storetab INDEX li_tabix.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD show_stores.
    " Display a table as ALV-Grid
    "CALL FUNCTION 'COF_ITAB_VIEWER'
    "  EXPORTING
    "    t_flat_table         = lt_storetab
    "    use_tech_fieldnames  = ' '
    "    hide_initial_columns = ' '.

    " Display ALV-Grid
    DATA: lr_exception TYPE REF TO cx_salv_error.
    TRY.

        " Instantiation
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false "  false: grid, true: list
          IMPORTING
            r_salv_table = lr_alv_table
          CHANGING
            t_table      = lt_storetab ).

        "... activate ALV generic Functions
        DATA(lr_functions) = lr_alv_table->get_functions( ).
        "lr_functions->set_default( abap_true ).           " Enable default ALV toolbar functions
        lr_functions->set_all( abap_true ).               " Enable all ALV toolbar functions
        "lr_functions->set_detail( if_salv_c_bool_sap=>true ).
        "lr_functions->set_group_export( if_salv_c_bool_sap=>true ).
        "lr_functions->set_group_filter( if_salv_c_bool_sap=>true ).
        "lr_functions->set_group_layout( if_salv_c_bool_sap=>true ).
        "lr_functions->set_print( if_salv_c_bool_sap=>true ).
        "lr_functions->set_print_preview( if_salv_c_bool_sap=>true ).
        "lr_functions->set_group_sort( if_salv_c_bool_sap=>true ).
        "lr_functions->set_find( if_salv_c_bool_sap=>true ).
        "lr_functions->set_graphics( if_salv_c_bool_sap=>false ).

        "... allow to save layout
        DATA(lr_layout) = lr_alv_table->get_layout( ).
        lr_layout->set_key( VALUE #( report = sy-repid ) ).               " Set layout key for identifying the particular ALV list
        "lr_layout->set_default( abap_true ).                             " Allow setting layouts as default layouts
        lr_layout->set_initial_layout( p_layout ).                        " Set initial variant
        AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
                            ID 'ACTVT' FIELD '23'.
        IF sy-subrc = 0.
          lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ). " Allow variant saving
        ELSE.
          lr_layout->set_save_restriction( cl_salv_layout=>restrict_user_dependant ). " Allow user specific variant saving
        ENDIF.

        "... set the display settings
        DATA(lr_display) = lr_alv_table->get_display_settings( ).
        lr_display->set_list_header( sy-title ).
        "lr_display->set_list_header( header ).
        "lr_display->set_list_header_size( header_size ).
        lr_display->set_striped_pattern( abap_true ).
        lr_display->set_horizontal_lines( abap_true ).
        lr_display->set_vertical_lines( abap_true ).
        "lr_display->set_suppress_empty_data( abap_true ).

        "... set the functional settings
        DATA(lr_functional) = lr_alv_table->get_functional_settings( ).
        lr_functional->set_sort_on_header_click( abap_true ).
        "lr_functional->set_f2_code( f2code ).
        "lr_functional->set_buffer( gs_test-settings-functional-buffer ).

        "... sort
        DATA(lr_sorts) = lr_alv_table->get_sorts( ).
        "lr_sorts->add_sort( 'EXTSID' ).

        "... set selection mode
        DATA(lr_selections) = lr_alv_table->get_selections( ).
        "allow single line selection
        lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ). " SINGLE, MULTIPLE, CELL, ROW_COLUMN, NONE

        "... register to the events of cl_salv_table
        DATA(lr_events) = lr_alv_table->get_event( ).
        SET HANDLER on_user_command FOR lr_events.
        SET HANDLER on_double_click FOR lr_events.

        "... set column appearance
        DATA(lr_columns) = lr_alv_table->get_columns( ).
        lr_columns->set_optimize( abap_true ).

        "... set column attributes
        DATA lr_column     TYPE REF TO cl_salv_column_table.        " Columns in Simple, Two-Dimensional Tables
        DATA color     TYPE lvc_s_colo.
        color-col = 1. " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange

        lr_column ?= lr_columns->get_column( 'STORE_ID' ).
        "lr_column->set_long_text(   'LONG_HEADER_40__________________________' ).
        "lr_column->set_medium_text( 'MEDIUM_HEADER_20____' ).
        "lr_column->set_short_text(  'SHORT_10__' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'RT_CIM_GUID' ).
        lr_column->set_long_text( 'Runtime CIM GUID' ).
        lr_column->set_medium_text( 'Runtime CIM GUID' ).
        lr_column->set_short_text( 'RT GUID' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'RT_CIM_NSPA' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'RT_TYPE' ).
        lr_column->set_key( abap_false ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'EXTSID' ).

        lr_column ?= lr_columns->get_column( 'SYSTEM_TYPE' ).

        lr_column ?= lr_columns->get_column( 'SCI_ID' ).

        lr_column ?= lr_columns->get_column( 'SCI_DESC' ).
        lr_column->set_key( abap_false ).

        lr_column ?= lr_columns->get_column( 'PROTECTED' ).
        lr_column->set_key( abap_false ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'EC_OB_DEST_ID' ).
        lr_column->set_key( abap_false ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'ASSIGNMENT_TYPE' ).
        lr_column->set_long_text( 'Assignment type' ).
        lr_column->set_medium_text( 'Assignment type' ).
        lr_column->set_short_text( 'AssignType' ).
        lr_column->set_key( abap_false ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'STORE_CIM_GUID' ).
        lr_column->set_long_text( 'Store CIM GUID' ).
        lr_column->set_medium_text( 'Store CIM GUID' ).
        lr_column->set_short_text( 'Store GUID' ).
        lr_column->set_visible( abap_false ).

        lr_column ?= lr_columns->get_column( 'STORE_CIM_CLASS' ).
        lr_column->set_key( abap_false ).

        lr_column ?= lr_columns->get_column( 'STORE_CIM_CAPT' ).
        lr_column->set_key( abap_false ).

        lr_column ?= lr_columns->get_column( 'STORE_NAME_EXT' ).
        lr_column->set_key( abap_false ).
        lr_column->set_key( abap_false ).

        lr_column ?= lr_columns->get_column( 'STORE_NAME' ).

        lr_column ?= lr_columns->get_column( 'DDIC_TABLENAME' ).
        lr_column->set_key( abap_false ).
        lr_column->set_visible( abap_true ).

        lr_column ?= lr_columns->get_column( 'STORE_FROM_DATE' ).
        lr_column->set_long_text( 'From date' ).
        lr_column->set_medium_text( 'From date' ).
        lr_column->set_short_text( 'From date' ).
        lr_column->set_edit_mask( '==TSTMP' ). " convert time stamps
        lr_column->set_key( abap_false ).

        lr_column ?= lr_columns->get_column( 'STORE_TO_DATE' ).
        lr_column->set_long_text( 'To date' ).
        lr_column->set_medium_text( 'To date' ).
        lr_column->set_short_text( 'To date' ).
        lr_column->set_edit_mask( '==TSTMP' ). " convert time stamps
        lr_column->set_key( abap_false ).

        lr_column ?= lr_columns->get_column( 'STORE_TYPE' ).
        lr_column->set_key( abap_false ).

        "... header
        DATA lr_grid_header         TYPE REF TO cl_salv_form_layout_grid.
        CREATE OBJECT lr_grid_header.
        lr_grid_header->create_text(
             row    = 1
             column = 1
             text   = 'Select multiple stores of the same type and use function F2 to show the data' ).
        lr_alv_table->set_top_of_list( lr_grid_header ).

        "... select all entries
        "if sel_all = ABAP_true.
        "  lr_selections->set_selected_rows( VALUE #( FOR i = 1 THEN i + 1 WHILE i <= lines( lt_storetab ) ( i ) ) ).
        "endif.

        "Display the ALV Grid
        lr_alv_table->display( ).

      CATCH cx_salv_msg
            cx_salv_not_found
            cx_salv_data_error
            cx_salv_existing
            INTO lr_exception.
        DATA(lv_message) = lr_exception->get_message( ).
        MESSAGE ID lv_message-msgid TYPE lv_message-msgty
                NUMBER lv_message-msgno
                WITH lv_message-msgv1 lv_message-msgv2
                     lv_message-msgv3 lv_message-msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD on_user_command.
*    importing e_salv_function

*   Get selected items
    DATA(lr_selections)   = lr_alv_table->get_selections( ).
    DATA(ls_cell)         = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    CASE e_salv_function.

      WHEN 'PICK'.
        IF lt_seleced_rows IS NOT INITIAL.
          show_content( lt_seleced_rows ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "on_user_command

  METHOD on_double_click. " F2
*   importing row column

    CASE column.
      WHEN 'DDIC_TABLENAME'.
        " Show content in transaction SE16
        READ TABLE lt_storetab INTO DATA(ls_storetab) INDEX row.

        DATA(t_sel) = VALUE rsparams_tt( (
            selname = 'I1' " Let's assume that field STORE_ID is the first field always
            kind    = 'S'  " S: Select option, P: Parameter
            sign    = 'I'  " Include
            option  = 'EQ'
            low     = ls_storetab-store_id
            high    = ''
          ) ).

        CALL FUNCTION 'RS_TABLE_LIST_CREATE'
          EXPORTING
            table_name             = ls_storetab-ddic_tablename
            "action                 = 'ANZE'
            "without_submit         = ' '
            "generation_forced      =
            "new_sel                =
            "no_structure_check     = ' '
            "data_exit              = ' '
            "IMPORTING
            "progname               =
          TABLES
            seltab                 = t_sel
          EXCEPTIONS
            table_is_structure     = 1
            table_not_exists       = 2
            db_not_exists          = 3
            no_permission          = 4
            no_change_allowed      = 5
            table_is_gtt           = 6
            cds_view_not_supported = 7
            OTHERS                 = 8.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

      WHEN OTHERS.
        " internal viewer

        DATA(lr_selections)   = lr_alv_table->get_selections( ).
        DATA(ls_cell)         = lr_selections->get_current_cell( ).
        DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

        IF lt_seleced_rows IS NOT INITIAL.
          show_content( lt_seleced_rows ).    " Multiple stores are selected
        ELSE.
          show_content( VALUE #( ( row ) ) ). " Show current store
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "on_double_click

  METHOD show_content.
    " importing lt_seleced_rows
    " used global variables: val_1 val_2

    DATA:
      first_entry  TYPE abap_bool        VALUE abap_true,
      lv_caller    TYPE char10           VALUE 'MY_APPL',
      lx_data      TYPE xstring,
      li_rc        TYPE i,
      lv_rc_string TYPE string.

    DATA: ls_fieldlist   TYPE  sccdb_fieldlist.
    DATA: lt_fieldlist   TYPE  tt_ccdb_fieldlist_sorted.  " Current field list
    DATA: lt_fieldlist_1 TYPE  tt_ccdb_fieldlist_sorted. " Field list of first store

    DATA: components TYPE        abap_component_tab,
          component  TYPE        abap_componentdescr.

    " Dynamic table for raw data
    DATA: r_dyntable TYPE REF TO cl_abap_tabledescr,
          r_dynstruc TYPE REF TO cl_abap_structdescr.

    DATA: lr_table_struc TYPE REF TO data.
    DATA: lr_table       TYPE REF TO data.

    FIELD-SYMBOLS: <table_struc> TYPE any.
    FIELD-SYMBOLS: <table>       TYPE STANDARD TABLE.

    " Dynamic table for extended result data
    DATA: r_dyntable_ext TYPE REF TO cl_abap_tabledescr,
          r_dynstruc_ext TYPE REF TO cl_abap_structdescr.

    DATA: lr_table_struc_ext TYPE REF TO data.
    DATA: lr_table_ext       TYPE REF TO data.

    FIELD-SYMBOLS: <table_struc_ext> TYPE any.
    FIELD-SYMBOLS: <table_ext>       TYPE STANDARD TABLE.

    LOOP AT lt_seleced_rows INTO DATA(row).
      READ TABLE lt_storetab INTO DATA(ls_storetab) INDEX row.

      CASE ls_storetab-store_type.
        WHEN: 'TABLE' OR 'TEXT'.

          CALL FUNCTION 'CCDB_GET_TABLE_SN' " Same function gan be used for both store types
            EXPORTING
              store_id       = ls_storetab-store_id
              caller         = lv_caller
            IMPORTING
              fieldlist      = lt_fieldlist
              " SNAPSHOT  TYPE  TT_CCDB_TROW
              snapshot_itsam = lx_data
              rc             = li_rc
              rc_string      = lv_rc_string.
          IF li_rc NE 0.
            MESSAGE lv_rc_string TYPE 'I'.
            RETURN.
          ENDIF.

          IF first_entry EQ abap_false AND lt_fieldlist_1[] NE lt_fieldlist[].
            MESSAGE 'Do not mix different store structures' TYPE 'I'.
            RETURN.
          ENDIF.
          lt_fieldlist_1[] = lt_fieldlist[].

          IF first_entry = abap_true.

            " Determine fieldlist (not neccessary)
            "CALL TRANSFORMATION id
            "  SOURCE XML lx_data
            "  RESULT fieldlist = lt_fieldlist.

            " define table structure
            LOOP AT lt_fieldlist INTO ls_fieldlist.
              "  Process Component if provided
              CLEAR component.
              component-name = ls_fieldlist-fieldname.
              component-type = cl_abap_elemdescr=>get_string( ).
              APPEND component TO components.
            ENDLOOP.

            TRY.
                " create raw table structure
                r_dynstruc = cl_abap_structdescr=>create(
                  p_components = components
                  p_strict     = space " allow e.g. '/'
                ).

                " create raw table (Standard table)
                r_dyntable = cl_abap_tabledescr=>create(
                  p_line_type  = r_dynstruc
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  "               cl_abap_tabledescr=>TABLEKIND_SORTED
                  "p_key_kind   = cl_abap_tabledescr=>KEYDEFKIND_USER
                  "p_key        = lt_key_fields
                  "p_unique     = 'X'
                ).

                "CREATE DATA lr_table_struc TYPE HANDLE r_dynstruc. " not used
                "ASSIGN lr_table_struc->* TO <table_struc>.

                CREATE DATA lr_table TYPE HANDLE r_dyntable.
                ASSIGN lr_table->* TO <table>.

              CATCH cx_sy_struct_creation cx_sy_table_creation INTO DATA(cx_dynabp_error).
                MESSAGE cx_dynabp_error->get_text( ) TYPE 'I'.
                RETURN.
            ENDTRY.

          ENDIF. " end of 1st entry

          " get raw data
          CALL TRANSFORMATION id
            SOURCE XML lx_data
            RESULT snapshot = <table>.

        WHEN: 'XML'.
          " call function 'CCDB_GET_XML_SN'...

        WHEN: 'EVENT'.
          " call function 'CCDB_GET_EVENT_TR'...

      ENDCASE.

*-----------------------------------------------------------------------

      " copy data to result table

      IF first_entry = abap_true.

        " create extended result table structure
        CLEAR: component, components.
        component-name = 'EXTSID'.
        component-type = cl_abap_elemdescr=>get_string( ). " Can we use the real type here?
        APPEND component TO components.
        component-name = 'SCI_ID'.
        component-type = cl_abap_elemdescr=>get_string( ). " Can we use the real type here?
        APPEND component TO components.
        component-name = 'STORE_NAME'.
        component-type = cl_abap_elemdescr=>get_string( ). " Can we use the real type here?
        APPEND component TO components.
        LOOP AT lt_fieldlist INTO ls_fieldlist.
          component-name = ls_fieldlist-fieldname.
          component-type = cl_abap_elemdescr=>get_string( ).
          APPEND component TO components.
        ENDLOOP.

        TRY.
            r_dynstruc_ext = cl_abap_structdescr=>create(
              p_components = components
              p_strict     = space " allow e.g. '/'
            ).

            " create result table (Standard table)
            r_dyntable_ext = cl_abap_tabledescr=>create(
              p_line_type  = r_dynstruc_ext
              p_table_kind = cl_abap_tabledescr=>tablekind_std
              "               cl_abap_tabledescr=>TABLEKIND_SORTED
              "p_key_kind   = cl_abap_tabledescr=>KEYDEFKIND_USER
              "p_key        = lt_key_fields
              "p_unique     = 'X'
            ).

          CATCH cx_sy_struct_creation cx_sy_table_creation INTO cx_dynabp_error.
            MESSAGE cx_dynabp_error->get_text( ) TYPE 'I'.
            RETURN.
        ENDTRY.

        CREATE DATA lr_table_struc_ext TYPE HANDLE r_dynstruc_ext.
        ASSIGN lr_table_struc_ext->* TO <table_struc_ext>.

        CREATE DATA lr_table_ext TYPE HANDLE r_dyntable_ext.
        ASSIGN lr_table_ext->* TO <table_ext>.

      ENDIF. " end of 1st entry
      first_entry = abap_false.

      check <table> IS ASSIGNED.
      LOOP AT <table> ASSIGNING <table_struc>.

        " Filter values
        IF val_1 IS NOT INITIAL OR val_2 IS NOT INITIAL.
          DATA: pos     TYPE i,
                found_1 TYPE abap_bool,
                found_2 TYPE abap_bool.
          FIELD-SYMBOLS: <field> TYPE any.
          pos = 1.
          found_1 = abap_false.
          found_2 = abap_false.
          DO.
            ASSIGN COMPONENT pos OF STRUCTURE <table_struc> TO <field>.
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.

            IF <field> IN val_1. " Compare field value with select option of the report
              found_1 = abap_true.
            ENDIF.
            IF <field> IN val_2. " Compare field value with select option of the report
              found_2 = abap_true.
            ENDIF.

            pos += 1.
          ENDDO.
          IF found_1 = abap_false OR found_2 = abap_false.
            CONTINUE. " Omit this entry
          ENDIF.
        ENDIF.

        " Copy extended data
        DATA: "Does there exist a smarter way to copy selected data only?
          BEGIN OF temp,
            extsid     TYPE lmdb_esid,
            sci_id     TYPE ccdb_sci_id,
            store_name TYPE ccdb_store_name,
          END OF temp.
        MOVE-CORRESPONDING ls_storetab   TO temp.
        MOVE-CORRESPONDING temp          TO <table_struc_ext>.
        " Copy data
        MOVE-CORRESPONDING <table_struc> TO <table_struc_ext>.
        APPEND <table_struc_ext> TO <table_ext>.
      ENDLOOP.
    ENDLOOP.

    IF <table_ext> IS NOT ASSIGNED.
      MESSAGE 'No data' TYPE 'I'.
      RETURN.
    ENDIF.
    IF <table_ext> IS INITIAL.
      MESSAGE 'No data' TYPE 'I'.
      RETURN.
    ENDIF.

*-----------------------------------------------------------------------

    " Display a table as ALV-Grid
    "CALL FUNCTION 'COF_ITAB_VIEWER'
    "  EXPORTING
    "    t_flat_table         = <table_ext>
    "    use_tech_fieldnames  = ' '
    "    hide_initial_columns = ' '.

    " Display ALV-Grid
    DATA:
      lr_alv_content_table TYPE REF TO cl_salv_table,
      lr_exception         TYPE REF TO cx_salv_error.

    TRY.

        " Instantiation
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false "  false: grid, true: list
          IMPORTING
            r_salv_table = lr_alv_content_table
          CHANGING
            t_table      = <table_ext> ).

        "... activate ALV generic Functions
        DATA(lr_functions) = lr_alv_content_table->get_functions( ).
        lr_functions->set_all( abap_true ).               " Enable all ALV toolbar functions

        "... set the display settings
        DATA(lr_display) = lr_alv_content_table->get_display_settings( ).
        lr_display->set_list_header( sy-title ).

        "... set column appearance
        DATA(lr_columns) = lr_alv_content_table->get_columns( ).
        lr_columns->set_optimize( abap_true ).

        "... set column attributes
        DATA lr_column     TYPE REF TO cl_salv_column_table.

        DATA ls_ddic_ref      TYPE salv_s_ddic_reference.

        DATA color     TYPE lvc_s_colo.
        color-col = 1. " 2=light blue, 3=yellow, 4=blue, 5=green, 6=red, 7=orange

        " Extended fields
        lr_column ?= lr_columns->get_column( 'EXTSID' ).
        ls_ddic_ref-table = 'CCDB_DIRV'.
        ls_ddic_ref-field = 'EXTSID'.
        lr_column->set_ddic_reference( ls_ddic_ref ).
        lr_column->set_key( abap_true ).
        "lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'SCI_ID' ).
        ls_ddic_ref-field = 'SCI_ID'.
        lr_column->set_ddic_reference( ls_ddic_ref ).
        lr_column->set_key( abap_true ).
        "lr_column->set_color( color ).

        lr_column ?= lr_columns->get_column( 'STORE_NAME' ).
        ls_ddic_ref-field = 'STORE_NAME'.
        lr_column->set_ddic_reference( ls_ddic_ref ).
        lr_column->set_key( abap_true ).
        "lr_column->set_color( color ).

        " Data fields
        LOOP AT lt_fieldlist INTO ls_fieldlist.
          DATA text TYPE text40.
          text = ls_fieldlist-fieldname.
          lr_column ?= lr_columns->get_column( ls_fieldlist-fieldname ).
          lr_column->set_long_text(   text(40) ).
          lr_column->set_medium_text( text(20) ).
          lr_column->set_short_text(  text(10) ).
          IF ls_fieldlist-fieldrole = 'K'.
            "lr_column->set_key( abap_true ).
            lr_column->set_color( color ).
          ELSE.
            "lr_column->set_key( abap_false ).
          ENDIF.
        ENDLOOP.

        "Display the ALV Grid
        lr_alv_content_table->display( ).

      CATCH cx_salv_msg
            cx_salv_not_found
            cx_salv_data_error
            cx_salv_existing
            INTO lr_exception.
        DATA(lv_message) = lr_exception->get_message( ).
        MESSAGE ID lv_message-msgid TYPE lv_message-msgty
                NUMBER lv_message-msgno
                WITH lv_message-msgv1 lv_message-msgv2
                     lv_message-msgv3 lv_message-msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD show_all_content.
    " Show the content for all stores
    show_content( VALUE #( FOR i = 1 THEN i + 1 WHILE i <= lines( lt_storetab ) ( i ) ) ).
  ENDMETHOD.

ENDCLASS.

*-----------------------------------------------------------------------

START-OF-SELECTION.

* Check general CCDB-Read-API authorization
  IF main=>authority_check( ) NE 0.
    MESSAGE e016(cx_ccdb_exception). " Permission denied
  ENDIF.

  main=>get_stores( ).

  IF showdata = abap_false.
    main=>show_stores( ).        " Show store list first
  ELSE.
    main=>show_all_content( ).   " Show data at once
  ENDIF.
