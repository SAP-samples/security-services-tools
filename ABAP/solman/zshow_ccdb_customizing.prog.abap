*&---------------------------------------------------------------------*
*& Report  ZSHOW_CCDB_CUSTOMIZING
*& Author: Frank Buchholz, SAP Security Services
*&---------------------------------------------------------------------*
*& Show customizing for CCDB data collectors
*&
*& 03.12.2020 Initial version
*& 19.04.2023 Corrections for showing only systems which use a specific customizing
*& 08.09.2023 SLIN corrections
*&---------------------------------------------------------------------*
REPORT zshow_ccdb_customizing
  LINE-SIZE 1023.

CONSTANTS: c_program_version(30) TYPE c VALUE '08.09.2023 FBT'.

*Tables:
*  DIAGST_STORE,      " Header data of Configuration Stores
*  DIAGSTC,           " Header data of Store Customizing
*  DIAGSTC_AUTH,      " not used
*  DIAGSTC_TRANS,     " not used
*  DIAGSTC_PROFILE,   " Default store customizing (contins SAP_ALL)
*  DIAGSTC_ROLE,      " Default store customizing (empty)

* Type                        Store Name
* TT_DIAGSTC_AC_ROLE          AUTH_COMB_CHECK_ROLE
* TT_DIAGSTC_AC_USER          AUTH_COMB_CHECK_USER
* TT_DIAGSTC_AUTH_ROLE_USER	  AUTH_ROLE_USER
* ...

* Store groups (including system id)
DATA: ls_diagst_storegr TYPE diagst_storegr,
      lt_diagst_storegr TYPE TABLE OF diagst_storegr.

* Configuration stores
DATA: ls_diagst_store TYPE diagst_store,
      lt_diagst_store TYPE TABLE OF diagst_store.

* Store customizing
DATA: ls_diagstc TYPE diagstc,
      lt_diagstc TYPE TABLE OF diagstc.

* Configuration stores
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_store FOR FIELD s_store.
SELECT-OPTIONS s_store FOR ls_diagst_store-name DEFAULT 'AUTH*' OPTION CP SIGN I.
SELECTION-SCREEN END OF LINE.

* Configuration store types
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_ttype FOR FIELD s_ttype.
SELECT-OPTIONS s_ttype FOR ls_diagstc-cust_ttype.
SELECTION-SCREEN END OF LINE.

* Show all technical systems using the customizing
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_alls AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(60) ss_alls FOR FIELD s_alls.
SELECTION-SCREEN END OF LINE.

* Show technical keys, too
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_tech AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(60) ss_tech FOR FIELD s_tech.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(60) ss_vers.


INITIALIZATION.

  ss_store = 'Configuration store'(001).
  ss_ttype = 'Configuration store type'(002).
  ss_alls  = 'Show all technical systems using the customizing'(005).
  ss_tech  = 'Show technical keys, too'(006).

  CONCATENATE 'Program version:'(VER) c_program_version INTO ss_vers
    SEPARATED BY space.

START-OF-SELECTION.

* check authorization in general
  AUTHORITY-CHECK OBJECT 'AI_CCDB_CU'
    ID 'ACTVT' FIELD '03'.
  IF sy-subrc NE 0.
    MESSAGE e019(diag_gen_msg)
      WITH 'No authorization to display Store Customizing'(006)
           '(authorization object AI_CCDB_CU)'(007).
    RETURN.
  ENDIF.

* Get configuration stores
  SELECT * FROM diagst_store INTO TABLE lt_diagst_store
    WHERE name       IN s_store
      AND cust_ttype IN s_ttype.
  IF s_alls IS INITIAL.
    SORT lt_diagst_store BY name.
    DELETE ADJACENT DUPLICATES FROM lt_diagst_store COMPARING name.
  ELSE.
*   Get store groups
    SELECT * FROM diagst_storegr INTO TABLE lt_diagst_storegr
      FOR ALL ENTRIES IN lt_diagst_store
      WHERE store_group_id = lt_diagst_store-store_group_id.
  ENDIF.

* Get store Customizing
  SELECT lscp_vk cust_ttype cust_ttype_id cust_ttype_desc is_default last_change "XDATA
    FROM diagstc INTO TABLE lt_diagstc
    FOR ALL ENTRIES IN lt_diagst_store
    WHERE cust_ttype = lt_diagst_store-cust_ttype.


* Output
  FORMAT RESET.

  LOOP AT lt_diagstc INTO ls_diagstc.

*   Show related configuration store(s)
    LOOP AT lt_diagst_store INTO ls_diagst_store
      WHERE cust_ttype    = ls_diagstc-cust_ttype
        AND CUST_TTYPE_ID = ls_diagstc-CUST_TTYPE_ID.

      WRITE: /
        'Configuration Store'(003)         INTENSIFIED,
        (58) ls_diagst_store-name          INTENSIFIED.

      IF s_alls = 'X'.
*       Show technical system which uses this store customizing
        CLEAR ls_diagst_storegr.
        READ TABLE lt_diagst_storegr INTO ls_diagst_storegr
          WITH KEY store_group_id = ls_diagst_store-store_group_id.

        WRITE:
*          (15) ls_diagst_storegr-tech_system_id INTENSIFIED,
          (20) ls_diagst_storegr-landscape_id   INTENSIFIED.
        IF s_tech = 'X'.
          WRITE:
            ls_diagst_store-store_id            COLOR COL_KEY,
            ls_diagst_store-store_group_id      COLOR COL_KEY.
        ENDIF.
      ENDIF.

    ENDLOOP.

*   Show store customizing header
    WRITE: /
      'Customizing'(004)           COLOR COL_NORMAL,
      ls_diagstc-cust_ttype_id     COLOR COL_NORMAL,                      " Customizing TableType ID
      '-'                          COLOR COL_NORMAL,
      ls_diagstc-cust_ttype_desc   COLOR COL_NORMAL.                      " Customizing TableType ID description
    IF ls_diagstc-is_default IS INITIAL.                                  " Boolean:  'X' = True,  space = False
      WRITE (8) space              COLOR COL_TOTAL.
    ELSE.
      WRITE (8) 'default'(def)     COLOR COL_POSITIVE.
    ENDIF.
    WRITE:
      ls_diagstc-last_change       COLOR COL_NORMAL USING EDIT MASK '____-__-__ __:__:__'. " UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun)
    IF s_tech = 'X'.
      WRITE:
        ls_diagstc-lscp_vk           COLOR COL_KEY,                         " Landscape Virtual Key
        ls_diagstc-cust_ttype        COLOR COL_KEY.                         " Customizing TableType of Store
    ENDIF.

*   Show content
    DATA:
      lv_cust_ttype_desc TYPE  diagst_cust_ttype_desc,
      lv_is_default      TYPE  diagst_boolean,
      lv_last_change_ts  TYPE  timestampl,
      lr_cust_data       TYPE  REF TO data,
      l_customizing_xml  TYPE  xstring.
    FIELD-SYMBOLS: <cust_table> TYPE ANY TABLE.

    DATA exc TYPE REF TO cx_diagst_retrieve_exception.
    TRY.
      CALL FUNCTION 'DIAGST_GET_CUST_TTYPE'
        EXPORTING
          lscp_vk                      = ls_diagstc-lscp_vk
          cust_ttype                   = ls_diagstc-cust_ttype
          cust_ttype_id                = ls_diagstc-cust_ttype_id
        IMPORTING
          cust_ttype_desc              = lv_cust_ttype_desc
          is_default                   = lv_is_default
          last_change                  = lv_last_change_ts
          r_cust_data                  = lr_cust_data
          .
      CATCH cx_diagst_retrieve_exception INTO exc.
        MESSAGE exc->get_text( ) TYPE 'I'.
        WRITE: / 'Error'(err) COLOR COL_NEGATIVE.
        CONTINUE.
    ENDTRY.

    ASSIGN lr_cust_data->* TO <cust_table>.  " <cust_table> gets type ls_DIAGSTC-CUST_TTYPE
    IF <cust_table> IS INITIAL.
      WRITE: / 'empty'(emp) COLOR COL_NEGATIVE.
    ELSE.
*     Show field headers
      DATA : l_tabledescr_ref TYPE REF TO cl_abap_tabledescr,
             l_descr_ref      TYPE REF TO cl_abap_structdescr,
             ls_compdescr     TYPE abap_compdescr.
*      l_tabledescr_ref ?= cl_abap_typedescr=>describe_by_data_ref( lr_cust_data ). " both stantements work
      l_tabledescr_ref ?= cl_abap_typedescr=>describe_by_data( <cust_table> ).
      l_descr_ref ?= l_tabledescr_ref->get_table_line_type( ).
      NEW-LINE.
      LOOP AT l_descr_ref->components INTO ls_compdescr.
        DATA(length) = ls_compdescr-length / 2. " Unicode takes 2 bytes per character
        WRITE: AT (length) ls_compdescr-name COLOR COL_GROUP.
      ENDLOOP.

*     Show content
      LOOP AT <cust_table> ASSIGNING FIELD-SYMBOL(<cust_line>).
        NEW-LINE.
        DO.
*          try.
          ASSIGN COMPONENT sy-index OF STRUCTURE <cust_line> TO FIELD-SYMBOL(<comp>).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          WRITE <comp>.

*      CATCH." cx_sy_assign_empty_structure.
*        "Skip empty generic boxes, if any
*        CONTINUE.
*          endtry.
        ENDDO.
      ENDLOOP. " <CUST_TABLE>
    ENDIF.
    SKIP.

  ENDLOOP. " LT_DIAGSTC
