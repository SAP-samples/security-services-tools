*&---------------------------------------------------------------------*
*& Report ZSUSR_SNC_GUIFLAG
*& Set/unset the SNC GUIFLAG of users
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Source: https://github.com/SAP-samples/security-services-tools
*&
*& 14.09.2023 Initial version
*&---------------------------------------------------------------------*
REPORT zsusr_snc_guiflag
  LINE-SIZE 162.

CONSTANTS: c_program_version(10) TYPE c VALUE '14.09.2023'.

DATA:
  snc_active           TYPE c,
  snc_accept_snc_logon TYPE suid_snc_info,
  suid_snc_info_text   TYPE suid_snc_info_text,
  selection_range      TYPE TABLE OF bapiussrge WITH HEADER LINE,
  userlist             TYPE TABLE OF bapiusname WITH HEADER LINE,
  return               TYPE TABLE OF bapiret2   WITH HEADER LINE,
  user_snc             TYPE          bapisncu,
  sncx                 TYPE          bapisncux,
  msg(5)               TYPE c.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_bname FOR FIELD s_bname.
SELECT-OPTIONS s_bname FOR userlist-username.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT /1(60) ss_snc.

SELECTION-SCREEN BEGIN OF BLOCK act WITH FRAME TITLE text001.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_show  RADIOBUTTON GROUP act.
SELECTION-SCREEN COMMENT 3(40) ss_show FOR FIELD s_show.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_set   RADIOBUTTON GROUP act.
SELECTION-SCREEN COMMENT 3(40) ss_set FOR FIELD s_set.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_unset RADIOBUTTON GROUP act.
SELECTION-SCREEN COMMENT 3(40) ss_unset FOR FIELD s_unset.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK act.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*

INITIALIZATION.

  CONCATENATE 'Program version from'(100) c_program_version INTO ss_vers
    SEPARATED BY space.

  ss_bname  = 'User'.

  text001   = `Action for the user specific SNC setting 'Allow password logon'`.
  ss_show   = `Show setting`.
  ss_set    = `Set 'Allow password logon' if not set`.
  ss_unset  = `Unset 'Allow password logon' if set`.

  CALL FUNCTION 'SNC_CHECK_ACTIVE'
    IMPORTING
      active = snc_active.
  IF snc_active = abap_true.
    CALL METHOD cl_suid_tools=>get_snc_insecure_information
      IMPORTING
        ev_snc_info      = snc_accept_snc_logon
        ev_snc_info_text = suid_snc_info_text.
    ss_snc = suid_snc_info_text.
  ELSE.
    ss_snc = 'SNC is not active'.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " The BAPI functions perform required authority checks

  CALL FUNCTION 'SNC_CHECK_ACTIVE'
    IMPORTING
      active = snc_active.
  IF snc_active NE abap_true.
    MESSAGE e721(00). "SNC deactivated
  ENDIF.

  CLEAR selection_range.
  selection_range-parameter = 'USERNAME'.
  LOOP AT s_bname.
    MOVE-CORRESPONDING s_bname TO selection_range.
    APPEND selection_range.
  ENDLOOP.

  CALL FUNCTION 'BAPI_USER_GETLIST'
*   EXPORTING
*     MAX_ROWS              = 0
*     WITH_USERNAME         = ' '
*   IMPORTING
*     ROWS                  =
    TABLES
      selection_range = selection_range
*     SELECTION_EXP   =
      userlist        = userlist
      return          = return.

  " Show messages
  LOOP AT return.
    WRITE: / return-type,
             return-message.
    IF return-type = 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.

  " Show and process data.
  FORMAT RESET.
  WRITE: /(12) 'User'     COLOR COL_HEADING,
           (7) 'GUIFLAG'  COLOR COL_HEADING,
          (80) 'SNC Name' COLOR COL_HEADING,
          (60) 'Message'  COLOR COL_HEADING.
  LOOP AT userlist.
    CLEAR: return, return[].

    CLEAR user_snc.
    CALL FUNCTION 'SNC_USER_GET_DATA'
      EXPORTING
        user_name = userlist-username
      IMPORTING
        user_snc  = user_snc
*       KNAME_EXISTS  =
      .

    IF user_snc-pname IS INITIAL.
      return-message = 'No SNC name'.
    ENDIF.

    IF     user_snc-guiflag = abap_true AND s_unset = abap_true." AND user_snc-pname IS NOT INITIAL.
      msg = 'unset'.
      user_snc-guiflag  = abap_false.
      sncx-guiflag      = abap_true.

    ELSEIF user_snc-guiflag = ' ' AND s_set = abap_true AND user_snc-pname IS NOT INITIAL.
      msg = 'set'.
      user_snc-guiflag  = abap_true.
      sncx-guiflag      = abap_true.

    ELSE.
      msg = user_snc-guiflag.

    ENDIF.

    IF msg = 'unset' OR msg = 'set'.
      CLEAR return.
      CALL FUNCTION 'BAPI_USER_CHANGE'
        EXPORTING
          username = userlist-username
          snc      = user_snc
          sncx     = sncx
        TABLES
          return   = return.
      LOOP AT return.
        IF return-type = 'E'.
          msg = 'error'.
          CONTINUE. " stop at first error
        ENDIF.
      ENDLOOP.
    ENDIF.

    " show it
    WRITE: /(12) userlist-username COLOR COL_KEY,
             (7) msg COLOR COL_NORMAL,
            (80) user_snc-pname,
            (60) return-message.
  ENDLOOP.

*----------------------------------------------------------------------*
* AT LINE-SELECTION
*----------------------------------------------------------------------*
AT LINE-SELECTION.
  " Call SU01
  CLEAR userlist.
  READ CURRENT LINE FIELD VALUE userlist-username.
  IF userlist-username IS NOT INITIAL.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'SU01'
      EXCEPTIONS
        ok     = 0
        not_ok = 1
        OTHERS = 2.
    IF sy-subrc = 0.
      CALL FUNCTION 'SUID_IDENTITY_MAINT'
        EXPORTING
          i_username = userlist-username.
    ENDIF.
  ENDIF.
