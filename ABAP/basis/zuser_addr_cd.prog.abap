*&---------------------------------------------------------------------*
*& Report ZUSER_ADDR_CD
*& Show change documents for address data of users
*& These change documents are not yet integrated into transaction SUIM report RSUSR100N.
*&---------------------------------------------------------------------*
*& Change documents with empty old and new value are omitted.
*&
*& Limitations:
*& The change documents for the corresponding business partner are slightly different.
*& Instead of reading table USR21, the report may should get the change documents for address assignments.
*&
*& Author: Frank Buchholz, SAP CoE Security Services
*& 20.01.2025 Created
*&---------------------------------------------------------------------*
REPORT zuser_addr_cd.

CONSTANTS c_program_version(30) TYPE c VALUE '20.01.2025 FBT'.

* Selection screen
TABLES sscrfields.
SELECTION-SCREEN:
FUNCTION KEY 1, "SUIM report RSUSR100N
FUNCTION KEY 2. "CD report RSSCD100

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (28) t_user FOR FIELD s_user.
  SELECT-OPTIONS s_user FOR ('XUBNAME') MATCHCODE OBJECT user_comp_addr DEFAULT sy-uname.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (28) t_cduser FOR FIELD s_cduser.
  SELECT-OPTIONS s_cduser FOR ('CDUSERNAME') MATCHCODE OBJECT user_comp.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (31) t_date FOR FIELD p_fdate.
  PARAMETERS p_fdate TYPE sy-datum.
  SELECTION-SCREEN COMMENT (7) t_space.
  SELECTION-SCREEN COMMENT (5) t_to.
  PARAMETERS p_tdate TYPE sy-datum.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN END OF LINE.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(31) t_layout FOR FIELD layout.
  PARAMETERS       layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_vers.
TYPES:
  BEGIN OF ts_usadr_cd,
    " User
    bname       TYPE usr21-bname,
    persnumber  TYPE usr21-persnumber,
    addrnumber  TYPE usr21-addrnumber,
    " Head
    objectclas  TYPE cdhdr-objectclas, "Object class
    objectid 	  TYPE cdhdr-objectid,   "Object value
    changenr    TYPE cdhdr-changenr,   "Change Number of Document
    username    TYPE cdhdr-username,   "User name of the person responsible in change document
    udate       TYPE cdhdr-udate,      "Creation Date of Change Document
    utime       TYPE cdhdr-utime,      "Time of Change
    tcode       TYPE cdhdr-tcode,      "Transaction in which a change was made
    " Pos
    tabname     TYPE cdpos-tabname,    "Table Name
    tabkey      TYPE cdpos-tabkey,     "Key of Modified Table Row
    fname       TYPE cdpos-fname,      "Field Name
    ftext       TYPE cdred-ftext,      "Field text
    value_old   TYPE cdpos-value_old,  "Old Contents of Changed Field
    value_new   TYPE cdpos-value_new,  "New Contents of Changed Field
    chngind     TYPE cdpos-chngind,    "Type of Change
    chngind_txt TYPE string,           "Type of Change (text)

    ctab        TYPE lvc_t_scol,       " Color field for ALV cells
  END OF ts_usadr_cd,
  tt_usadr_cd TYPE STANDARD TABLE OF ts_usadr_cd WITH KEY bname udate utime tabname fname.

*---------------------------------------------------------------------*
*      CLASS lcl_report DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      initialization,

      at_selection_screen_f4_layout
        CHANGING
          layout TYPE disvariant-variant,

      at_selection_screen,

      start_of_selection
      .

  PROTECTED SECTION. " used by class lcl_alv, too

    CONSTANTS:
      objectclas_adr TYPE cdhdr-objectclas VALUE 'ADRESSE3'.

    CLASS-DATA:
      lt_data TYPE tt_usadr_cd.

  PRIVATE SECTION.

    CLASS-METHODS:

      authority_check
      .

ENDCLASS.                    "lcl_report DEFINITION

*---------------------------------------------------------------------*
*      CLASS lcl_alv DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM lcl_report.

  PUBLIC SECTION.

    CLASS-METHODS:

      show_result,

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

  PRIVATE SECTION.

    CLASS-DATA:

      r_alv_table   TYPE REF TO cl_salv_table,
      lr_alv_events TYPE REF TO lcl_alv.

    CLASS-METHODS:

      show_user      IMPORTING user     TYPE usr02-bname
      .

ENDCLASS.                    "lcl_alv DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.

    authority_check( ).

    DATA functxt TYPE smp_dyntxt.

    functxt-icon_id   = icon_report.
    functxt-quickinfo = 'SUIM report RSUSR100N'.
    functxt-icon_text = 'SUIM'.
    sscrfields-functxt_01 = functxt.

    functxt-icon_id   = icon_report.
    functxt-quickinfo = 'CD report RSSCD100'.
    functxt-icon_text = 'CD'.
    sscrfields-functxt_02 = functxt.

    sy-title = 'Show change documents for address data of users'(tit).

    t_user   = 'User'(usr).
    t_cduser = 'Changed by'(mod).
    t_date   = 'Date from/to'(dat).
    t_to     = 'to'(dto).

    t_layout = 'Layout'(lot).

    CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
       SEPARATED BY space.

    " Default date range 1 year
    p_fdate = sy-datum - 365.
    p_tdate = sy-datum.

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

  METHOD at_selection_screen.

    CASE sscrfields-ucomm.

      WHEN 'FC01'.
        " SUIM report RSUSR100N
        SUBMIT rsusr100n VIA SELECTION-SCREEN AND RETURN
                 WITH user   IN s_user
                 WITH cduser IN s_cduser
                 WITH fdate  =  p_fdate
                 WITH tdate  =  p_tdate
*                WITH accnt ...
*                WITH addr ...
*                WITH addrs ...
*                WITH ALIAS ...
*                WITH alock_d ...
*                WITH alock_s ...
*                WITH arch ...
*                WITH ATTR ...
*                WITH cert_m ...
*                WITH CHANGE ...
*                WITH clic ...
*                WITH cprof ...
*                WITH crole ...
*                WITH dbms_usr ...
*                WITH extuid ...
*                WITH flock_d ...
*                WITH flock_s ...
*                WITH ftime ...
*                WITH f_cprof ...
*                WITH f_crole ...
*                WITH f_prof ...
*                WITH f_role ...
*                WITH f_sys ...
*                WITH globid ...
*                WITH GROUP ...
*                WITH license ...
*                WITH LOCK ...
*                WITH no_alv ...
*                WITH pass ...
*                WITH prof ...
*                WITH p_info ...
*                WITH p_layout ...
*                WITH p_selshw ...
*                WITH p_title ...
*                WITH refus ...
*                WITH respo ...
*                WITH role ...
*                WITH secu ...
*                WITH snc ...
*                WITH sys ...
*                WITH tech ...
*                WITH ttime ...
*                WITH tval ...
*                WITH TYPE ...
*                WITH user_blk ...
*                WITH user_crt ...
*                WITH user_del ...
                .

      WHEN 'FC02'.
        " CD report RSSCD100
        SUBMIT rsscd100 VIA SELECTION-SCREEN AND RETURN
                 WITH objekt   = objectclas_adr
                 WITH aenderer = s_cduser-low
                 WITH datum    = p_fdate
                 WITH dat_bis  = p_tdate
*                WITH callsub ...
*                WITH keyguid ...
*                WITH key_exp ...
*                WITH local_t ...
*                WITH lowercas ...
*                WITH new_disp ...
*                WITH no_plus ...
*                WITH no_star ...
*                WITH nummer ...
*                WITH objektid ...
*                WITH readarch ...
*                WITH readdb ...
*                WITH tabkey ...
*                WITH tabkeylo ...
*                WITH tabname ...
*                WITH tcode ...
*                WITH tzsource ...
*                WITH zeit ...
*                WITH zeit_bis ...
                .

    ENDCASE.

  ENDMETHOD. " at_selection_screen

  METHOD start_of_selection.
    " Change documents of user address

    " Get users (not perfect: Instead, the program may should get the change documents for address assignments)
    TYPES:
      BEGIN OF ts_usr21,
        bname      TYPE usr21-bname,
        persnumber TYPE usr21-persnumber,
        addrnumber TYPE usr21-addrnumber,
      END OF ts_usr21,
      tt_usr21 TYPE STANDARD TABLE OF ts_usr21 WITH KEY bname.

    SELECT
        bname,
        persnumber,
        addrnumber
      FROM usr21
      WHERE bname    IN @s_user
        AND idadtype =  '00'      " Only for users having an address
      ORDER BY bname
      INTO TABLE @DATA(lt_usr21).

    " Get change documents
    IF p_tdate IS INITIAL.
      p_tdate = sy-datum.
    ENDIF.
    IF p_tdate < p_fdate.
      p_tdate = p_fdate.
    ENDIF.

    DATA:
      ls_usadr_cd TYPE ts_usadr_cd.

    LOOP AT lt_usr21 INTO DATA(ls_usr21).
      CLEAR: ls_usadr_cd.

      ls_usadr_cd-bname           = ls_usr21-bname.
      ls_usadr_cd-persnumber      = ls_usr21-persnumber.
      ls_usadr_cd-addrnumber      = ls_usr21-addrnumber.
      ls_usadr_cd-objectclas      = objectclas_adr.

      " Construct selection for change documents
      ls_usadr_cd-objectid+0(4)   = 'BC01'. " persgroup
      ls_usadr_cd-objectid+4(10)  = ls_usr21-persnumber.
      ls_usadr_cd-objectid+14(10) = ls_usr21-addrnumber.

      " Get change documents
      DATA:
        it_username TYPE cdusername_range_tab,
        lt_cdpos    TYPE cdpos_tab,
        lt_cdred    TYPE cdredcd_tab,
        lt_cdshw    TYPE cdshw_tab,
        lt_cdhdr    TYPE cdhdr_tab.

*      SELECT
*          h~objectclas,  "Object class
*          "h~OBJECTID,   "Object value
*          h~changenr,    "Change Number of Document
*          h~username,    "User name of the person responsible in change document
*          h~udate,      "Creation Date of Change Document
*          h~utime,      "Time of Change
*          h~tcode,      "Transaction in which a change was made
*          p~tabname,    "Table Name
*          p~tabkey,     "Key of Modified Table Row
*          p~fname,      "Field Name
*          p~chngind,    "Type of Change
*          p~value_new,   "New Contents of Changed Field
*          p~value_old   "Old Contents of Changed Field
*        FROM cdhdr AS h
*        INNER JOIN cdpos AS p
*          ON    p~objectclas =  h~objectclas
*            AND p~objectid   =  h~objectid
*            AND p~changenr   =  h~changenr
*            AND ( p~value_new IS NOT INITIAL OR p~value_old IS NOT INITIAL ) " Omit records with empty value fields
*          WHERE h~udate      GE @p_fdate
*            AND h~udate      LE @p_tdate
*            AND h~objectclas =  @objectclas_adr
*            AND h~objectid   =  @ls_usadr_cd-objectid
*            AND h~username   IN @s_cduser
*        ORDER BY
*          udate ascending,
*          utime ascending,
*          tabname,
*          fname
*        INTO CORRESPONDING FIELDS OF @ls_usadr_cd.
*    ENDSELECT.

      clear: lt_cdpos, lt_cdred, lt_cdshw, lt_cdhdr.

      " Type conversion is required to avoid a dump
      it_username[] = s_cduser[].
      CALL FUNCTION 'CHANGEDOCUMENT_READ_ALL'
        EXPORTING
          i_objectclass              = objectclas_adr
          i_objectid                 = ls_usadr_cd-objectid
*         I_CHANGENUMBER             = ' '
          i_date_of_change           = p_fdate
*         I_TIME_OF_CHANGE           = '000000'
          i_date_until               = p_tdate
*         I_TIME_UNTIL               = '235959'
*         I_USERNAME                 = ' '
*         I_TCODE                    = ' '
*         I_TABLENAME                = ' '
*         I_TABLEKEY_SHORT           = ' '
*         I_TABLEKEY_LONG            = ' '
*         I_KEYGUID                  = ' '
*         I_KEYGUID_STR              = ' '
*         I_LOCAL_TIME               = ' '
*         I_TIME_ZONE                = 'UTC'
*         I_ARCHIVE_HANDLE           = 0
*         I_READ_ARCHIVE_IS          = ' '
*         I_READ_MAX                 = 0
*         I_NO_INITIAL_VALUES        = ' '
*         I_NOPLUS_ASWILDCARD_INOBJID       = ' '
*         I_ONLY_HEADERS             = ' '
*         I_ONLY_POSITIONS           = ' '
*         I_HOT                      = ' '
*         IT_OBJECTCLASS             =
*         IT_OBJECTID                =
*         IT_CHANGENR                =
          it_username                = it_username "s_cduser[]
*         IT_TCODE                   =
*         IT_TABLENAME               =
*         IT_TABLEKEY_SHORT          =
*         IT_TABLEKEY254             =
*         IT_KEYGUID                 =
*         IT_KEYGUID_STR             =
*         I_PREP_UNIT                = 'X'
*         I_NOSTAR_ASWILDCARD_INOBJID       = ' '
        IMPORTING
          et_cdpos                   = lt_cdpos          " Found positions from table CDPOS
*         ET_CDPOS_UID               =
*         ET_CDPOS_STR               =
          et_cdred                   = lt_cdred          " Formatted change documents with header data
*         ET_CDRED_STR               =
          et_cdshw                   = lt_cdshw          " Formatted change documents without header data
*         ET_CDREDADD                =
        CHANGING
          ct_cdhdr                   = lt_cdhdr          " Entries from table CDHDR: Read or passed (to read positions)
        EXCEPTIONS
          missing_input_objectclass  = 1
          missing_input_header       = 2
          no_position_found          = 3
          wrong_access_to_archive    = 4
          time_zone_conversion_error = 5
          read_too_many_entries      = 6
          OTHERS                     = 7.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        check sy-subrc = 0. " Just ignore it
      ENDIF.

      LOOP AT lt_cdred INTO DATA(ls_cdred).
        " Omit emtpy change documents
        check ls_cdred-f_old is not initial or ls_cdred-f_new is not initial.

        " Copy data
        MOVE-CORRESPONDING ls_cdred TO ls_usadr_cd.
        ls_usadr_cd-value_old = ls_cdred-f_old.
        ls_usadr_cd-value_new = ls_cdred-f_new.

        " Set field color
        IF ls_usadr_cd-value_old IS NOT INITIAL.
          APPEND VALUE #( fname = 'VALUE_OLD' color-col = col_negative ) TO ls_usadr_cd-ctab.
        ENDIF.
        IF ls_usadr_cd-value_new IS NOT INITIAL.
          APPEND VALUE #( fname = 'VALUE_NEW' color-col = col_positive ) TO ls_usadr_cd-ctab.
        ENDIF.

        " Get text of change indicator (better: use function DDIF_FIELDINFO_GET for CDPOS-CHNGIND)
        CASE ls_usadr_cd-chngind.
          WHEN 'I'.    ls_usadr_cd-chngind_txt = 'Insert'(ins).
          WHEN 'U'.    ls_usadr_cd-chngind_txt = 'Change'(chg).
          WHEN 'E'.    ls_usadr_cd-chngind_txt = 'Delete'(del).
          WHEN OTHERS. ls_usadr_cd-chngind_txt = ls_usadr_cd-chngind_txt.
        ENDCASE.

        APPEND ls_usadr_cd TO lt_data.

      ENDLOOP.

    ENDLOOP.

    IF lt_data IS NOT INITIAL.
*      cl_demo_output=>display_data( lt_data ).
      " Show result
      lcl_alv=>show_result( ).
    ENDIF.

  ENDMETHOD. " start_of_selection

  METHOD authority_check.

    DATA l_subrc TYPE sy-subrc.
    CALL FUNCTION 'CD_CHECK_AUTHORITY'
      EXPORTING
        objectclass = objectclas_adr
        activity    = '08'
      IMPORTING
        subrc       = l_subrc.
    IF l_subrc <> 0.
      MESSAGE s800(cd).
      LEAVE.
    ENDIF.

  ENDMETHOD. " authority_check

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
      lr_events           TYPE REF TO cl_salv_events_table,
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
*          EXPORTING
*            r_container  = cl_gui_container=>default_screen "screen0
          IMPORTING
            r_salv_table = r_alv_table
          CHANGING
            t_table      = lt_data ).

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
    "lr_functions_list->set_group_export( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_group_filter( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_group_layout( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_print( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_print_preview( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_group_sort( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_find( if_salv_c_bool_sap=>true ).
    "lr_functions_list->set_graphics( if_salv_c_bool_sap=>false ).
    lr_functions_list->set_all( abap_true ).



    " Additional function for ALV toolbar
    " requires r_container = cl_gui_container=>default_screen

    " Suppress toolbar of list output
*    cl_abap_list_layout=>suppress_toolbar( ).
*    WRITE space. "trick to get the screen

*    TRY.
*        DATA l_icon TYPE string.
*
*        l_icon = icon_toggle_display_change.
*        lr_functions_list->add_function(
*          name     = '...'
*          icon     = l_icon
*          text     = ''
*          tooltip  = '...'
*          position = if_salv_c_function_position=>right_of_salv_functions ).
*        IF auth_change IS INITIAL.
*          lr_functions_list->enable_function( name = '...' boolean = if_salv_c_bool_sap=>false ).
*        ENDIF.
*
*      CATCH cx_salv_not_found cx_salv_existing cx_salv_wrong_call.
*    ENDTRY.


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

    " register to the events of cl_salv_table
    lr_events = r_alv_table->get_event( ).
    CREATE OBJECT lr_alv_events.
    " register to the event USER_COMMAND
    SET HANDLER lr_alv_events->on_user_command FOR lr_events.
    " register to the event DOUBLE_CLICK
    SET HANDLER lr_alv_events->on_double_click FOR lr_events.

    " Selection mode: single cell
    lr_selections   = r_alv_table->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ). " or if_salv_c_selection_mode=>row_column

    " Sort columns (Example)
*  TRY.
*      lr_sorts = r_alv_table->get_sorts( ).
*      lr_sorts->add_sort( columnname = 'USER'
*                          position   = 1
*                          sequence   = if_salv_c_sort=>sort_up
*                          subtotal   = abap_true ).
*      lr_sorts->add_sort( columnname = 'UDATE'
*                          position   = 2
*                          sequence   = if_salv_c_sort=>sort_up
*                          subtotal   = abap_true ).
*      lr_sorts->add_sort( columnname = 'UTIME'
*                          position   = 3
*                          sequence   = if_salv_c_sort=>sort_up
*                          subtotal   = abap_true ).
*    CATCH cx_salv_not_found
*          cx_salv_existing
*          cx_salv_data_error.
*  ENDTRY.

    " Aggregation columns (Example)
*    TRY.
*        lr_aggregations = r_alv_table->get_aggregations( ).
*        lr_aggregations->add_aggregation( columnname = '...'  aggregation = if_salv_c_aggregation=>total ).
*      CATCH cx_salv_not_found
*            cx_salv_existing
*            cx_salv_data_error.
*    ENDTRY.

    " Get column definitions
    lr_columns = r_alv_table->get_columns( ).

    " Set the column width
    lr_columns->set_optimize( if_salv_c_bool_sap=>true ).
    lr_columns->set_key_fixation( if_salv_c_bool_sap=>true ).

    " Set field descriptions and attributes
    DATA ls_color_key  TYPE lvc_s_colo.
    ls_color_key-col = col_key.
    DATA ls_color_old  TYPE lvc_s_colo.
    ls_color_old-col = col_heading.
    DATA ls_color_new  TYPE lvc_s_colo.
    ls_color_new-col = col_total.

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

        lr_column ?= lr_columns->get_column( 'BNAME' ).
        lr_column->set_long_text( 'User' ).
        lr_column->set_medium_text( 'User' ).
        lr_column->set_short_text( 'User' ).
        lr_column->set_color( ls_color_key ).

        lr_column ?= lr_columns->get_column( 'USERNAME' ).
        lr_column->set_long_text( 'Changed by' ).
        lr_column->set_medium_text( 'Changed by' ).
        lr_column->set_short_text( 'Chng. by' ).
*       lr_column->set_color( ls_color ).

        lr_column ?= lr_columns->get_column( 'OBJECTID' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'CHANGENR' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'TABKEY' ).
        lr_column->set_visible( if_salv_c_bool_sap=>false ).

        lr_column ?= lr_columns->get_column( 'VALUE_OLD' ).
        "lr_column->set_color( ls_color_old ).

        lr_column ?= lr_columns->get_column( 'VALUE_NEW' ).
        "lr_column->set_color( ls_color_new ).

        lr_column ?= lr_columns->get_column( 'CHNGIND_TXT' ).
        lr_column->set_long_text( 'Change type' ).
        lr_column->set_medium_text( 'Change type' ).
        lr_column->set_short_text( 'Change' ).

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

    " Set sisplay settings
    lr_display_settings = r_alv_table->get_display_settings( ).
    lr_display_settings->set_list_header( 'Show change documents for address data of users'(tit) ). "sy-title
*    lr_display_settings->set_list_header( |Program version: { c_program_version }| ).
*    lr_display_settings->set_list_header_size(
*      cl_salv_display_settings=>c_header_size_small ).
    lr_display_settings->set_no_merging( if_salv_c_bool_sap=>true ).


    " Display the table
    r_alv_table->display( ).

  ENDMETHOD. " show_result

  METHOD on_user_command.
    " importing e_salv_function

    " Get selected item
    DATA(lr_selections) = r_alv_table->get_selections( ).
    DATA(ls_cell) = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    CASE e_salv_function.

      WHEN '...'.

    ENDCASE.

  ENDMETHOD. " on_user_command

  METHOD on_double_click.
    " importing row column

    " Get selected item
    DATA(lr_selections) = r_alv_table->get_selections( ).
    DATA(ls_cell) = lr_selections->get_current_cell( ).
    DATA(lt_seleced_rows) = lr_selections->get_selected_rows( ).

    CHECK row > 0.

    READ TABLE lt_data INTO DATA(ls_data) INDEX row.
    CHECK sy-subrc = 0.

    CASE column.

      WHEN 'BNAME'.
        show_user( ls_data-bname ).

    ENDCASE.

  ENDMETHOD. " on_double_click

  METHOD show_user.
    " Does not work as it reuses the current screen
    "CALL FUNCTION 'SUID_IDENTITY_MAINT'
    "  EXPORTING
    "    I_USERNAME           = user
    "    "I_TCODE_MODE         = 1 " 1: Single maintenance SU01, 6: Display mode SU01D
    "    I_SU01_DISPLAY       = abap_true
    .
    " same as above
    "CALL FUNCTION 'SUSR_USER_MAINT_WITH_DIALOG'
    "  EXPORTING
    "    "MAINT_FOR_OWN_USER_ONLY       =
    "    DISPLAY_ONLY                  = 'X'
    "    USER_TO_DISPLAY               = user
    "     "DO_NOT_USE                    = ' '
    "   EXCEPTIONS
    "     ERROR_WRITING_TO_DB           = 1
    "     OTHERS                        = 2
    "           .

    DATA bdcdata TYPE TABLE OF bdcdata WITH EMPTY KEY.
    bdcdata = VALUE #(
      " Show user
      "( program = 'SAPLSUU5'     dynpro = '0050'  dynbegin = 'X' )         " old screen
      ( program = 'SAPLSUID_MAINTENANCE'  dynpro = '1050'  dynbegin = 'X' ) " current screen
      "( fnam    = 'USR02-BNAME'  fval   = user    )
      ( fnam    = 'SUID_ST_BNAME-BNAME'  fval   = user    )
      ( fnam    = 'BDC_OKCODE'   fval   = '=SHOW' ) " SHOW, CHAN

      " End transaction after going back
      "( program = 'SAPLSUU5'     dynpro = '0050'  dynbegin = 'X' )         " old screen
      ( program = 'SAPLSUID_MAINTENANCE'  dynpro = '1050'  dynbegin = 'X' ) " current screen
      ( fnam    = 'BDC_OKCODE'   fval   = '=BACK' )
    ).

    DATA(opt) = VALUE ctu_params( dismode = 'E'    " Display of screens only if an error occurs
                                  defsize = 'X' ). " Standard size of screens

    SET PARAMETER ID 'XUS' FIELD user.

    TRY.
        CALL TRANSACTION 'SU01' WITH AUTHORITY-CHECK
            USING bdcdata
            OPTIONS FROM opt.
      CATCH cx_sy_authorization_error.
    ENDTRY.

  ENDMETHOD. " show_user

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

AT SELECTION-SCREEN.
  lcl_report=>at_selection_screen( ).

START-OF-SELECTION.
  lcl_report=>start_of_selection( ).
