*&---------------------------------------------------------------------*
*& Report  ZDIAGST_GET_STORES
*& Show Configuration Stores
*&---------------------------------------------------------------------*
*&
*& Reference: Show definition of stores
*& Solution Manager Administration WorkCenter / InfraStructure / RCA content
*& Filter, e.g. by *HANA*
*& You may download the cv xml into your favorite xml editor or just the UI to browse the content.
*&
*& Special stores:
*& All trace files are defined as LOG stores and used to provided file locations to the log viewer and always empty.
*&
*& 09.12.2009 Initial Version
*& 21.08.2013 Updated
*& 11.01.2016 Show authorization for protected stores
*&            Performance optimization for simple selections
*&---------------------------------------------------------------------*

REPORT  ZDIAGST_GET_STORES
  line-SIZE 1023.

constants: C_PROGRAM_VERSION(10) type C value '11.01.2016'.

data: lt_STORE_DIR     TYPE          TT_DIAGST_STORE_DIR,
      ls_STORE_DIR     TYPE  LINE OF TT_DIAGST_STORE_DIR,
*      lt_STORE_DIR_MI  TYPE  TT_DIAGST_STORE_DIR_MI,
*      lt_STORE_STATS   TYPE  TT_DIAGST_STORE_SUM_HIST_DATE,
      l_RC             TYPE  I,
      l_RC_TEXT        TYPE  NATXT,
      l_date           TYPE  D,
      l_time           TYPE  T.

* Prototypes for SELECT-OPTIONS
data: STORE_NAME(80), "TYPE DIAGST_STORE_NAME_LONG
      FIELDVALUE(80). "TYPE DIAGST_FIELDVALUE

selection-screen begin of block SEL with frame title TEXT001.

selection-screen begin of line.
selection-screen comment 1(25) SS_SID for field S_SID.
select-options s_SID  for ls_STORE_DIR-LONG_SID.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(25) SS_TYPE for field S_TYPE.
select-options s_TYPE for ls_STORE_DIR-TECH_SYSTEM_TYPE. " default 'JAVA',
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(25) SS_NAME for field S_NAME.
select-options s_NAME for STORE_NAME LOWER CASE. "         default 'Default'. "ls_STORE_DIR-STORE_NAME.
selection-screen end of line.

selection-screen end of block SEL.

selection-screen begin of block DAT with frame title TEXT002.

selection-screen begin of line.
selection-screen comment 1(28) SS_DATA for field showdata.
parameters showdata as checkbox.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(25) SS_MARK for field S_MARK.
select-options s_MARK for FIELDVALUE LOWER CASE.
selection-screen end of line.

selection-screen end of block DAT.

selection-screen begin of line.
selection-screen comment 1(30) SS_RFC for field S_RFC.
parameters S_RFC TYPE  RFCDEST.
selection-screen end of line.

selection-screen comment 1(60) SS_VERS.

*----------------------------------------------------------------------*

initialization.

  TEXT001 = 'Store selection'.
  SS_SID  = 'System'.
  SS_TYPE = 'System type'.
  SS_NAME = 'Store name'.

  TEXT002 = 'Content'.
  SS_DATA = 'Show data'.
  SS_MARK = 'Value'.

  SS_RFC  = 'RFC Destination'.

  concatenate 'Program version:'(VER) C_PROGRAM_VERSION into SS_VERS
    separated by SPACE.



*  s_NAME-option = 'EQ'.
*  s_NAME-sign   = 'I'.
*  s_NAME-low    = 'Default'.           append s_name.
*  s_NAME-low    = 'ADSSAP'.            append s_name.
*  s_NAME-low    = 'BI_MMR'.            append s_name.
*  s_NAME-low    = 'CAF'.               append s_name.
*  s_NAME-low    = 'CAF-UM'.            append s_name.
*  s_NAME-low    = 'J2EE_COMP_SPLEVEL'. append s_name.
*  s_NAME-low    = 'LM-TOOLS'.          append s_name.
*  s_NAME-low    = 'SAP_JTECHS'.        append s_name.
*  s_NAME-low    = 'SWLIFECYCL'.        append s_name.
*  s_NAME-low    = 'UMEADMIN'.          append s_name.

  s_MARK-option = 'EQ'.
  s_MARK-sign   = 'I'.
  s_MARK-low    = 'ObjVersion'.                                append s_mark.
  s_MARK-low    = 'ume.persistence.data_source_configuration'. append s_mark.
  s_MARK-low    = 'ume.r3.connection.master.user'.             append s_mark.

*----------------------------------------------------------------------*

start-of-selection.

  data: l_cnt          type i,
        sel_SID        TYPE DIAGLS_TECHNICAL_SYSTEM_SID,
*        sel_TYPE       TYPE DIAGLS_TECHNICAL_SYSTEM_TYPE,
        sel_STORE_NAME TYPE DIAGST_STORE_NAME_LONG  .

  describe table s_sid lines l_cnt.
  if l_cnt = 1.
    read table s_sid index 1.
    if    s_sid-option = 'EQ'
      and s_sid-sign   = 'I'
      and s_sid-low    ne space
      and s_sid-high   = space.
      sel_sid =  s_sid-low.
    endif.
  endif.

*  describe table s_TYPE lines l_cnt.
*  if l_cnt = 1.
*    read table s_TYPE index 1.
*    if    s_TYPE-option = 'EQ'
*      and s_TYPE-sign   = 'I'
*      and s_TYPE-low    ne space
*      and s_TYPE-high   = space.
*      sel_TYPE =  s_TYPE-low.
*    endif.
*  endif.

  describe table s_NAME lines l_cnt.
  if l_cnt = 1.
    read table s_NAME index 1.
    if    s_NAME-option = 'EQ'
      and s_NAME-sign   = 'I'
      and s_NAME-low    ne space
      and s_NAME-high   = space.
      sel_STORE_NAME =  s_NAME-low.
    endif.
  endif.

  CALL FUNCTION 'DIAGST_GET_STORES_RFC'
    destination S_RFC
  EXPORTING
    SID                         = sel_sid
*   INSTALL_NUMBER              = ' '
*   LONG_SID                    = ' '
*    TECH_SYSTEM_TYPE            = sel_TYPE  " TECH_SYSTEM_TYPE requires LONG_SID
*   GROUP_LANDSCAPE_CLASS       = ' '
*   GROUP_LANDSCAPE_ID          = ' '
*   GROUP_COMP_ID               = ' '
*   GROUP_SOURCE                = ' '
*   GROUP_NAME                  = ' '
*   STORE_CATEGORY              = ' '
*   STORE_TYPE                  = ' '
*   STORE_FULLPATH              = ' '
    STORE_NAME                  = sel_STORE_NAME
*   STORE_MAINALIAS             = ' '
*   STORE_SUBALIAS              = ' '
*   HAS_ELEMENT_FROM            =
*   HAS_ELEMENT_TO              =
*   ELEMENT_FILTER              = 'C'
*   CASE_INSENSITIVE            = ' '
*   PATTERN_SEARCH              = 'X'
*   SEARCH_STRING               =
*   ONLY_WITH_VALID_COMPV       = 'X'
*   DISPLAY                     = ' '
*   SAVE_EXPORT_XML             = ' '
* IMPORTING
    IMPORTING
      STORE_DIR                   = lt_STORE_DIR
*    STORE_DIR_MI                = lt_STORE_DIR_MI
*    STORE_STATS                 = lt_STORE_STATS
      RC                          = l_RC
      RC_TEXT                     = l_RC_TEXT
*   EXPORT_XML                  =
            .

  if l_RC ne 0.
    format color col_negative.
    write: / l_RC, l_RC_TEXT.
    format reset.
  endif.

  sort lt_STORE_DIR by LONG_SID INSTALL_NUMBER STORE_NAME.

  format INTENSIFIED.
  write: /(8) 'SID',
         (10) 'Inst.Nr',
         (25) 'Host',
         (4)  'Type',
         (30) 'Landscape class',
         (10) 'Category',
         (15) 'Store type',
         (32) 'Store ID',
         (10) 'Date',
         (8)  'Time',
         (30) 'Authorization check', " for AI_CCDB_SC
         (20) 'Store name and path'.
  format reset.
  loop at lt_STORE_DIR into ls_STORE_DIR
    where LONG_SID         in s_SID
      and TECH_SYSTEM_TYPE in s_TYPE
      and STORE_NAME       in s_NAME.


*   Authority Check for authorization object AI_CCDB_SC for field CONT_AUTH
    data: l_AUTH_PROTECTED    TYPE DIAGST_YESNO,
          l_AUTH_SUBRC        TYPE I,
          lt_AUTH_CHECKED     TYPE TT_DIAGST_AUTH_SC,
          ls_AUTH_CHECKED     TYPE SDIAGST_AUTH_SC.
    clear: l_AUTH_PROTECTED, l_AUTH_SUBRC, lt_AUTH_CHECKED.
    CALL METHOD CL_DIAGST_STORE=>CONTENT_AUTH_CHECK_BY_NAME
      EXPORTING
        IM_NAMESPACE       = ls_STORE_DIR-NAMESPACE
        IM_LONG_SID        = ls_STORE_DIR-LONG_SID
        IM_TECH_TYPE       = ls_STORE_DIR-TECH_SYSTEM_TYPE
        IM_LANDSCAPE_CLASS = ls_STORE_DIR-LANDSCAPE_CLASS
        IM_COMP_ID         = ls_STORE_DIR-COMP_ID
        IM_GROUP_SOURCE    = ls_STORE_DIR-GROUP_SOURCE
        IM_GROUP_NAME      = ls_STORE_DIR-GROUP_NAME
        IM_STORE_CATEGORY  = ls_STORE_DIR-STORE_CATEGORY
        IM_STORE_TYPE      = ls_STORE_DIR-STORE_TYPE
        IM_STORE_PATH      = ls_STORE_DIR-STORE_FULLPATH
        IM_STORE_NAME      = ls_STORE_DIR-STORE_NAME
        IM_ACTVT           = '03'
      IMPORTING
        EX_PROTECTED       = l_AUTH_PROTECTED "Flag showing if the store is protected
        EX_SUBRC           = l_AUTH_SUBRC     "Return code of authority check
        EX_AUTH_CHECKED    = lt_AUTH_CHECKED  "Field CONT_AUTH shows which authorization
        .                                     "gets checked for authorization object AI_CCDB_SC
    clear ls_AUTH_CHECKED.
    read table lt_AUTH_CHECKED into ls_AUTH_CHECKED INDEX 1.

    write ls_STORE_DIR-STORE_TO to FIELDVALUE(14). " p(8) -> char
    l_date = FIELDVALUE(8).
    l_time = FIELDVALUE+8(6).
    WRITE: /     ls_STORE_DIR-LONG_SID         color col_key,
                 ls_STORE_DIR-INSTALL_NUMBER   color col_key,
           (25)  ls_STORE_DIR-HOST             color col_key,
           (4)   ls_STORE_DIR-TECH_SYSTEM_TYPE,
                 ls_STORE_DIR-LANDSCAPE_CLASS,
                 ls_STORE_DIR-STORE_CATEGORY,
                 ls_STORE_DIR-STORE_TYPE,
                 ls_STORE_DIR-STORE_ID.
    if l_date is initial.
      WRITE: (10)  SPACE.
    else.
      WRITE: (10)  l_date                        DD/MM/YYYY.
    endif.
    if l_time is initial.
      WRITE: (8)  SPACE.
    else.
      WRITE: (8)   l_time                        using EDIT MASK '__:__:__'.
    endif.

*   Authority Check for authorization object AI_CCDB_SC for field CONT_AUTH
    WRITE:
*          (1)   l_PROTECTED                   color col_negative INVERSE ON,
*          (2)   l_AUTH_SUBRC                  color col_negative INVERSE ON,
           (30)  ls_AUTH_CHECKED-CONT_AUTH     color col_negative INVERSE ON.

    WRITE:       ls_STORE_DIR-STORE_NAME       color col_total, "string
                 ls_STORE_DIR-STORE_FULLPATH  .                 "string

    if showdata = 'X'.
      perform showdata using ls_STORE_DIR.
      uline.
    endif.

  endloop.

*&---------------------------------------------------------------------*
*&      Form  showdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->S_STORE_DIR  text
*----------------------------------------------------------------------*
form showdata using S_STORE_DIR like ls_STORE_DIR.
* see function DSVAS_APPL_CCDB_STORE2CHECKTAB

  data: F_RC      TYPE  I,
        F_RC_TEXT TYPE  NATXT,
        F_LENGTH  TYPE  I.

  data: LT_TEXT_SNAPSHOT        type         TT_DIAGST_TL_ELEM,
        LS_TEXT_SNAPSHOT        TYPE LINE OF TT_DIAGST_TL_ELEM,
        LT_TABLE_SNAPSHOT       type         TT_DIAGST_TROWS,
        LS_TABLE_SNAPSHOT       TYPE LINE OF TT_DIAGST_TROWS,
        LT_INNER_TABLE_SNAPSHOT type         TT_DIAGST_TROW_ELEM,
        LS_INNER_TABLE_SNAPSHOT TYPE LINE OF TT_DIAGST_TROW_ELEM,
        LT_FIELDLIST            TYPE         TT_DIAGST_TABLE_STORE_FIELDS,
        LS_FIELDLIST            TYPE LINE OF TT_DIAGST_TABLE_STORE_FIELDS.

  data: T_DSVAS_CHECK_TABLE     TYPE table of DSVASTABROWDFEXT,
        LS_CHECK_TABLE          type          DSVASTABROWDFEXT.

  field-symbols: <FIELD_N> type any.

* write: / S_STORE_DIR-STORE_TO.

  clear: LT_FIELDLIST, T_DSVAS_CHECK_TABLE.
  case s_STORE_DIR-STORE_TYPE.

    when 'TEXT'.
      call function 'DIAGST_TEXT_SNAPSHOT_RFC'
        destination S_RFC
        exporting
          STORE_ID            = S_STORE_DIR-STORE_ID
*         TIMESTAMP           =
        importing
*         SNAPSHOT_VALID_FROM =
*         SNAPSHOT_VALID_TO   =
          SNAPSHOT            = LT_TEXT_SNAPSHOT
          RC                  = F_RC
          RC_TEXT             = F_RC_TEXT.
      loop at LT_TEXT_SNAPSHOT into LS_TEXT_SNAPSHOT.
*        write: / LS_TEXT_SNAPSHOT-LINE_NUMBER,
*                 LS_TEXT_SNAPSHOT-LINE.

        clear LS_CHECK_TABLE.
        LS_CHECK_TABLE-FIELD1 = LS_TEXT_SNAPSHOT-LINE_NUMBER.
        LS_CHECK_TABLE-FIELD2 = LS_TEXT_SNAPSHOT-LINE.
        append LS_CHECK_TABLE to T_DSVAS_CHECK_TABLE.

      endloop.

    when 'TABLE' or 'PROPERTY' or 'WIN_INI'.
      call function 'DIAGST_TABLE_SNAPSHOT_RFC'
        destination S_RFC
        exporting
          STORE_ID            = S_STORE_DIR-STORE_ID
*         TIMESTAMP           =
        importing
          FIELDLIST           = LT_FIELDLIST
*         SNAPSHOT_VALID_FROM =
*         SNAPSHOT_VALID_TO   =
          SNAPSHOT            = LT_TABLE_SNAPSHOT
          RC                  = F_RC
          RC_TEXT             = F_RC_TEXT.
**     Show field list
*      new-line.
*      loop at LT_FIELDLIST into LS_FIELDLIST.
*        write: "/ LS_FIELDLIST-FIELDPOS  color COL_HEADING,
*               "  LS_FIELDLIST-FIELDROLE color COL_HEADING,
*                 LS_FIELDLIST-FIELDNAME color COL_HEADING.
*      endloop.
*     Show content
      loop at LT_TABLE_SNAPSHOT into LS_TABLE_SNAPSHOT.
        LT_INNER_TABLE_SNAPSHOT = LS_TABLE_SNAPSHOT.
        new-line.
        loop at LT_INNER_TABLE_SNAPSHOT into LS_INNER_TABLE_SNAPSHOT.
          if LS_INNER_TABLE_SNAPSHOT-FIELDPOS <= 25.
*            F_LENGTH = strlen( LS_INNER_TABLE_SNAPSHOT-FIELDVALUE ).
*            if LS_INNER_TABLE_SNAPSHOT-FIELDVALUE in s_mark.
*              write: at (F_LENGTH) LS_INNER_TABLE_SNAPSHOT-FIELDVALUE color col_positive.
*            else.
*              write: at (F_LENGTH) LS_INNER_TABLE_SNAPSHOT-FIELDVALUE.
*            endif.

            assign component LS_INNER_TABLE_SNAPSHOT-FIELDPOS of structure LS_CHECK_TABLE to <FIELD_N>.
            <FIELD_N> = LS_INNER_TABLE_SNAPSHOT-FIELDVALUE.

          endif.
        endloop.
        append LS_CHECK_TABLE to T_DSVAS_CHECK_TABLE.
      endloop.

    when 'XML'.
      write: / 'Store type XML not supported'.

    when others.
      write: / 'Unknown store type', s_STORE_DIR-STORE_TYPE.

  endcase.


* Output
  data: max_field type i value 5.                           " max 25

* Calculate max length
  data: begin of max_length occurs 25,
          value type i,
        end of max_length.
  clear: max_length[].
  loop at T_DSVAS_CHECK_TABLE into LS_CHECK_TABLE.
    do max_field times.
      ASSIGN COMPONENT sy-index OF STRUCTURE LS_CHECK_TABLE to <FIELD_N>.
      read table max_length index sy-index.
      if sy-subrc ne 0.
         max_length-value = strlen( <FIELD_N> ).  append max_length.
      else.
        if max_length-value < strlen( <FIELD_N> ).
          max_length-value = strlen( <FIELD_N> ). modify max_length index sy-index.
        endif.
      endif.
    enddo.
  endloop.
* Show field list
  new-line.
  loop at LT_FIELDLIST into LS_FIELDLIST.
      read table max_length index LS_FIELDLIST-FIELDPOS.
      if sy-subrc ne 0.
         max_length-value = strlen( LS_FIELDLIST-FIELDNAME ).  append max_length.
      else.
        if max_length-value < strlen( LS_FIELDLIST-FIELDNAME ).
          max_length-value = strlen( LS_FIELDLIST-FIELDNAME ). modify max_length index LS_FIELDLIST-FIELDPOS.
        endif.
      endif.

    write: "/ LS_FIELDLIST-FIELDPOS  color COL_HEADING,
           "  LS_FIELDLIST-FIELDROLE color COL_HEADING,
            at (max_length-value) LS_FIELDLIST-FIELDNAME color COL_HEADING.
  endloop.
* Show content
  loop at T_DSVAS_CHECK_TABLE into LS_CHECK_TABLE.
    new-line.
    do max_field times.
      ASSIGN COMPONENT sy-index OF STRUCTURE LS_CHECK_TABLE to <FIELD_N>.
      read table max_length index sy-index.
      if <FIELD_N> in s_mark.
        write: at (max_length-value) <FIELD_N> color col_positive.
      else.
        write: at (max_length-value) <FIELD_N>.
      endif.
    enddo.
  endloop.

endform.                    "showdata
