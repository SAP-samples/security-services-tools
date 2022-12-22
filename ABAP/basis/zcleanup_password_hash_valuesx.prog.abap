*&---------------------------------------------------------------------*
*& Report  ZCLEANUP_PASSWORD_HASH_VALUESX
*&
*&---------------------------------------------------------------------*
*& Extended version of the program to remove all weak password hash values in user master data, change documents and password history
*& Use it on your own risk!
*&
*& 20.07.2022 Initial version
*& 28.07.2022 Typos corrected
*&            Interpret BCODE and PASSCODE with code versions space, A, D, X as redundant
*&            Correction: only selected entries for USH02 and USRPWDHISTORY are deleted (not all of that selected user)
*& 24.08.2022 Correction in showing the profile parameter values on the report selection screen
*& 22.12.2022 Correction of the PWDSALTEDHASH icon for USH02
*&---------------------------------------------------------------------*

REPORT     zcleanup_password_hash_valuesx
           LINE-SIZE 132.

CONSTANTS: c_program_version(30) TYPE c VALUE '22.12.2022 OQL'.


"INCLUDE <color>.
"INCLUDE <icon>.
"INCLUDE <symbol>.

TABLES:    usr02, ush02, usrpwdhistory.

TYPES:
  BEGIN OF ts_result,
    tabname        TYPE tabname,    " Table name
    mandt          TYPE mandt,      " Client
    bname          TYPE xubname,    " User
    modda          TYPE xumoddate,  " Modification date in USH02 and USRPWDHISTORY
    modti          TYPE xumodtime,  " Modification time in USH02 and USRPWDHISTORY
    class          TYPE xuclass,    " Group
    ustyp          TYPE xuustyp,    " Type
    uflag          TYPE xuuflag,    " Lock
    gltgv          TYPE xugltgv,    " From date
    gltgb          TYPE xugltgb ,   " To date
    zbvmaster      TYPE xuzbvflag,  " CUA flag
    trdat          TYPE xuldate,    " Login date
    ltime          TYPE xultime,    " Login time
    user_status    TYPE text15,     " status 'inactive'
    codvn          TYPE xucodever2, " Code version
    xbcode         TYPE icon_d,     " BCODE
    xpasscode      TYPE icon_d,     " PASSCODE
    xpwdsaltedhash TYPE icon_d,     " PWDSALTEDHASH
    comment        TYPE text132,    " Comment
    t_color        TYPE lvc_t_scol, " ALV color of row
    timestamp      type TIMESTAMP,  " Time stamp in USRPWDHISTORY (hidden, converted to MODDA, MODTI)
  END OF ts_result.

DATA:
  ls_result        TYPE ts_result,
  lt_result        TYPE TABLE OF ts_result,
  ls_usr02         TYPE usr02,
  ls_ush02         TYPE ush02,
  ls_usrpwdhistory TYPE usrpwdhistory.

CONSTANTS:
  empty_bcode    LIKE usr02-bcode    VALUE 0,
  empty_passcode LIKE usr02-passcode VALUE 0.

*------------------------------------------------------------------------*
* Selection screen
*------------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK usr WITH FRAME TITLE text001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_mandt FOR FIELD mandt.
SELECT-OPTIONS mandt FOR usr02-mandt DEFAULT sy-mandt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_bname FOR FIELD bname.
SELECT-OPTIONS bname FOR usr02-bname.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_codvn FOR FIELD codvn.
SELECT-OPTIONS codvn FOR usr02-codvn.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_usr02 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(79) ss_usr02 FOR FIELD s_usr02.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_ush02 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(79) ss_ush02 FOR FIELD s_ush02.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_hist AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 2(79) ss_hist FOR FIELD s_hist.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK usr.

SELECTION-SCREEN BEGIN OF BLOCK lgn WITH FRAME TITLE text002.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_class FOR FIELD class.
SELECT-OPTIONS class FOR usr02-class.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_ustyp FOR FIELD ustyp.
SELECT-OPTIONS ustyp FOR usr02-ustyp.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_uflag FOR FIELD uflag.
SELECT-OPTIONS uflag FOR usr02-uflag.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_lvl1 RADIOBUTTON GROUP lvl DEFAULT 'X'.
SELECTION-SCREEN COMMENT 2(79) ss_lvl1 FOR FIELD s_lvl1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_lvl2 RADIOBUTTON GROUP lvl.
SELECTION-SCREEN COMMENT 2(79) ss_lvl2 FOR FIELD s_lvl2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_lvl3 RADIOBUTTON GROUP lvl.
SELECTION-SCREEN COMMENT 2(79) ss_lvl3 FOR FIELD s_lvl3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK lgn.

SELECTION-SCREEN BEGIN OF BLOCK par WITH FRAME TITLE text003.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(43) ss_par01.
SELECTION-SCREEN COMMENT 44(8) s_par01.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(43) ss_par02.
SELECTION-SCREEN COMMENT 44(8) s_par02.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(43) ss_par03.
SELECTION-SCREEN COMMENT 44(8) s_par03.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(43) ss_par04.
SELECTION-SCREEN COMMENT 44(8) s_par04.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(43) ss_par05.
SELECTION-SCREEN COMMENT 44(8) s_par05.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(43) ss_par06.
SELECTION-SCREEN COMMENT 44(8) s_par06.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(43) ss_par07.
SELECTION-SCREEN COMMENT 44(8) s_par07.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK par.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) ps_lout FOR FIELD p_layout.
PARAMETERS       p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

*----------------------------------------------------------------------*

DATA: gs_alv_lout_variant TYPE disvariant.

*----------------------------------------------------------------------*

INITIALIZATION.

  text001  = 'User selection'.
  ss_mandt = 'Client'.
  ss_bname = 'User'.
  ss_usr02 = 'Logon data (USR02)'.
  ss_ush02 = 'Change documents (USH02)'.
  ss_hist  = 'Password history (USRPWDHISTORY)'.

  text002  = 'Additional selection for logon data (USR02)'.
  ss_class = 'User group'.
  ss_ustyp = 'User type'.
  ss_uflag = 'Lock status'.
  ss_codvn = 'Code version of password hash'.
  ss_lvl1  = 'Show and process redundant password hashes only (safe)'.
  ss_lvl2  = 'Show and process weak password hashes of inactive users, too (critical!)'.
  ss_lvl3  = 'Show and process weak password hashes of active users, too (danger!)'.

  text003  = 'Profile parameter'.
  ss_par01 = 'login/password_downwards_compatibility'.
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD ss_par01 ID 'VALUE' FIELD s_par01.
  ss_par02 = 'login/password_compliance_to_current_policy'.
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD ss_par02 ID 'VALUE' FIELD s_par02.
  ss_par03 = 'login/min_password_lng'.
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD ss_par03 ID 'VALUE' FIELD s_par03.
  ss_par04 = 'login/min_password_lowercase'.
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD ss_par04 ID 'VALUE' FIELD s_par04.
  ss_par05 = 'login/min_password_uppercase'.
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD ss_par05 ID 'VALUE' FIELD s_par05.
  ss_par06 = 'login/min_password_digits'.
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD ss_par06 ID 'VALUE' FIELD s_par06.
  ss_par07 = 'login/min_password_specials'.
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD ss_par07 ID 'VALUE' FIELD s_par07.

  ps_lout     = 'Layout'.

  CONCATENATE 'Program version:'(VER) c_program_version INTO ss_vers
    SEPARATED BY space.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON p_layout
*----------------------------------------------------------------------*

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

*----------------------------------------------------------------------*

START-OF-SELECTION.

*------------------------------------------------------------------------*
* Check usage boundaries for cross-client operation
*------------------------------------------------------------------------*
  FORMAT RESET.

  AUTHORITY-CHECK OBJECT 'S_TABU_CLI' ID 'CLIIDMAINT' FIELD 'X'.
  IF sy-subrc <> 0.
    WRITE: / 'You do not have authorizations for cross-client operations (S_TABU_CLI)' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  DATA flag_ok TYPE abap_bool.
  flag_ok = abap_true.
  PERFORM check_table_authorization USING 'USR02' CHANGING flag_ok.
  PERFORM check_table_authorization USING 'USH02' CHANGING flag_ok.
  PERFORM check_table_authorization USING 'USRPWDHISTORY' CHANGING flag_ok.
  IF flag_ok <> abap_true.
    WRITE: / 'You do not have authorizations for table operations (S_TABU_DIS/S_TABU_NAM)' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  " AUW	Report &A started
  CALL FUNCTION 'RSLG_WRITE_SYSLOG_ENTRY'
    EXPORTING
      data_word1       = 'ZCLEANUP_PASSWORD_HASH_VALUESX'
      sl_message_area  = 'AU'
      sl_message_subid = 'W'
    EXCEPTIONS
      OTHERS           = 0. "ignore

*------------------------------------------------------------------------*
* Do it
*------------------------------------------------------------------------*

  PERFORM load_data.

  PERFORM show_result.

*------------------------------------------------------------------------*

FORM check_table_authorization USING name TYPE string CHANGING flag_ok TYPE abap_bool.
  DATA: l_name         TYPE dd25v-viewname,
        l_errormessage TYPE string.

  l_name = name.
  CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
    EXPORTING
      view_action                    = 'U'
      view_name                      = l_name
      no_warning_for_clientindep     = 'X'
    EXCEPTIONS
      invalid_action                 = 1
      no_authority                   = 2
      no_clientindependent_authority = 3
      table_not_found                = 4
      OTHERS                         = 5.
  IF sy-subrc = 2 OR sy-subrc = 3.
    l_errormessage = 'You are not authorizated to update table $TABNAME$ (S_TABU_DIS / S_TABU_NAM)'(E03).
    REPLACE '$TABNAME$' IN l_errormessage WITH name.
    WRITE: / l_errormessage.
    flag_ok = abap_false.
  ENDIF.
ENDFORM.                    "check_table_authorization

*------------------------------------------------------------------------*
* Load data
*------------------------------------------------------------------------*

FORM load_data.

* ALV Color of entry
  DATA: lt_color TYPE lvc_t_scol,
        ls_color TYPE lvc_s_scol.

  IF s_usr02 = 'X'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 25
        text       = 'Load user data'.

    CLEAR ls_result.
    ls_result-tabname = 'USR02'.
    SELECT
        u~mandt
        u~bname
        u~class
        u~ustyp
        u~uflag
        u~gltgv
        u~gltgb
        u~zbvmaster
        u~trdat
        u~ltime
        u~codvn
        u~bcode
        u~passcode
        u~pwdsaltedhash
      FROM usr02 AS u
      CLIENT SPECIFIED
      INTO CORRESPONDING FIELDS OF ls_usr02
      WHERE u~mandt IN mandt
        AND u~bname IN bname
        AND u~class IN class
        AND u~ustyp IN ustyp
        AND u~uflag IN uflag
        AND u~codvn IN codvn
        AND (   u~bcode    NE empty_bcode
             OR u~passcode NE empty_passcode )
      ORDER BY u~mandt u~bname.

      MOVE-CORRESPONDING ls_usr02 TO ls_result.

      " ICON_INCOMPLETE  ICON_FAILURE  ICON_POSITIVE  ICON_NEGATIVE  ICON_MESSAGE_ERROR ICON_MESSAGE_CRITICAL  ICON_LED_RED
      " ICON_MESSAGE_CRITICAL_SMALL  ICON_MESSAGE_WARNING_SMALL
      " ICON_ALERT  ICON_WARNING

      CLEAR: ls_result-xbcode, ls_result-xpasscode, ls_result-xpwdsaltedhash.
      IF ls_usr02-bcode         IS NOT INITIAL. ls_result-xbcode         = icon_incomplete.   ENDIF.
      IF ls_usr02-passcode      IS NOT INITIAL. ls_result-xpasscode      = icon_incomplete.   ENDIF.
      IF ls_usr02-pwdsaltedhash IS NOT INITIAL. ls_result-xpwdsaltedhash = icon_checked.      ENDIF.

      IF     ls_usr02-uflag IS NOT INITIAL
        OR ( ls_usr02-gltgv IS NOT INITIAL AND ls_usr02-gltgv > sy-datum )
        OR ( ls_usr02-gltgb IS NOT INITIAL AND ls_usr02-gltgb < sy-datum )
        OR   ls_usr02-zbvmaster IS NOT INITIAL
        "or   usr02-TRDAT is not initial " caution: active job users do not update this field
        .
        ls_result-user_status = 'inactive'.
      ELSE.
        ls_result-user_status = space.
      ENDIF.

      CLEAR: ls_color, lt_color.
      ls_color-fname = 'COMMENT'.

      ls_result-comment = space.
      CASE ls_usr02-codvn.
        WHEN 'A'. " Code Version A (Obsolete)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'B'. " BCODE Code Version B (MD5-Based, 8 Characters, Upper-Case, ASCII)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_negative.

        WHEN 'D'. " Code Version D (MD5-Based, 8 Characters, Upper-Case, UTF-8)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'E'. " Code Version E (Corrected Code Version D)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_negative.

        WHEN 'F'. " Code Version F (SHA1, 40 Characters, Case-Sensitive, UTF-8)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_negative.

        WHEN 'G'. " Code Version G = Code Vers. F + Code Vers. B (2 Hash Values)
          ls_result-comment = 'redundant and weak password hash'.
          ls_color-color-col = col_negative.

        WHEN 'H'. " PWDSALTEDHASH Code Version H (Generic Hash Procedure)
          ls_result-comment = 'ok'.
          ls_color-color-col = col_positive.

        WHEN 'I'. " Code Version I = Code Versions H + F + B (Three Hash Values)
          ls_result-comment = 'redundant and weak password hashes'.
          ls_color-color-col = col_group.

        WHEN 'X'. " Password Deactivated
          ls_result-comment = 'no password'.
          ls_color-color-col = col_positive.

        WHEN OTHERS.
          ls_result-comment = 'unknown hash version'.
          ls_color-color-col = col_total.

      ENDCASE.

      APPEND ls_color TO lt_color.
      ls_result-t_color = lt_color.

      IF   ( s_lvl1 = 'X' AND ( ls_result-codvn CA ' ADGIX' ) )
        OR ( s_lvl2 = 'X' AND ( ls_result-codvn CA ' ADGIX' OR ls_result-user_status IS NOT INITIAL ) )
        OR ( s_lvl3 = 'X' ).
        APPEND ls_result TO lt_result.
      ENDIF.
    ENDSELECT.

  ENDIF.

  IF s_ush02 = 'X'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Load change documents'.

    CLEAR ls_result.
    ls_result-tabname = 'USH02'.
    SELECT
        h~mandt
        h~bname
        h~modda
        h~modti
        h~modbe
        h~tcode
        h~repid
        h~bcode
        h~gltgv
        h~gltgb
        h~ustyp
        h~class
        h~uflag
        h~accnt
        h~passcode
        h~codvn
        h~pwdinitial
        h~pwdsaltedhash
        h~security_policy
      FROM ush02 AS h
      CLIENT SPECIFIED
      INTO CORRESPONDING FIELDS OF ls_ush02
      WHERE h~mandt IN mandt
        AND h~bname IN bname
        AND h~codvn IN codvn
        AND (   h~bcode    NE empty_bcode
             OR h~passcode NE empty_passcode )
      ORDER BY h~mandt h~bname h~modbe DESCENDING h~modti DESCENDING.

      " Do we know this user already?
      IF   ls_ush02-mandt NE ls_result-mandt
        OR ls_ush02-bname NE ls_result-bname.

        SELECT SINGLE mandt bname
          FROM usr02
          CLIENT SPECIFIED
          INTO CORRESPONDING FIELDS OF ls_result
          WHERE mandt = ls_ush02-mandt
            AND bname = ls_ush02-bname
            .
        IF sy-subrc = 0.
          ls_result-user_status = ''.
        ELSE.
          ls_result-user_status = 'deleted'.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING ls_ush02 TO ls_result.

      CLEAR: ls_result-xbcode, ls_result-xpasscode, ls_result-xpwdsaltedhash.
      IF ls_ush02-bcode         IS NOT INITIAL. ls_result-xbcode         = icon_incomplete.   ENDIF.
      IF ls_ush02-passcode      IS NOT INITIAL. ls_result-xpasscode      = icon_incomplete.   ENDIF.
      IF ls_ush02-pwdsaltedhash IS NOT INITIAL. ls_result-xpwdsaltedhash = icon_checked.      ENDIF.

      CLEAR: ls_color, lt_color.
      ls_color-fname = 'COMMENT'.

      ls_result-comment = space.
      CASE ls_ush02-codvn.
        WHEN 'A'. " Code Version A (Obsolete)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'B'. " BCODE Code Version B (MD5-Based, 8 Characters, Upper-Case, ASCII)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'D'. " Code Version D (MD5-Based, 8 Characters, Upper-Case, UTF-8)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'E'. " Code Version E (Corrected Code Version D)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'F'. " Code Version F (SHA1, 40 Characters, Case-Sensitive, UTF-8)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'G'. " Code Version G = Code Vers. F + Code Vers. B (2 Hash Values)
          ls_result-comment = 'redundant and weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'H'. " PWDSALTEDHASH Code Version H (Generic Hash Procedure)
          ls_result-comment = 'ok'.
          ls_color-color-col = col_positive.

        WHEN 'I'. " Code Version I = Code Versions H + F + B (Three Hash Values)
          ls_result-comment = 'redundant and weak password hashes'.
          ls_color-color-col = col_group.

        WHEN 'X'. " Password Deactivated
          ls_result-comment = 'no password'.
          ls_color-color-col = col_positive.

        WHEN OTHERS.
          ls_result-comment = 'unknown hash version'.
          ls_color-color-col = col_total.

      ENDCASE.

      APPEND ls_color TO lt_color.
      ls_result-t_color = lt_color.

      APPEND ls_result TO lt_result.
    ENDSELECT.

  ENDIF.

  IF s_hist = 'X'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 75
        text       = 'Load password history'.

    CLEAR ls_result.
    ls_result-tabname = 'USRPWDHISTORY'.
    SELECT
        h~mandt
        h~bname
        h~timestamp
        h~passcode
        h~bcode
        h~codvn
        h~pwdsaltedhash
      FROM usrpwdhistory AS h
      CLIENT SPECIFIED
      INTO CORRESPONDING FIELDS OF ls_usrpwdhistory
      WHERE h~mandt IN mandt
        AND h~bname IN bname
        AND h~codvn IN codvn
        AND (   h~bcode    NE empty_bcode
             OR h~passcode NE empty_passcode )
      ORDER BY h~mandt h~bname h~timestamp DESCENDING.

      MOVE-CORRESPONDING ls_usrpwdhistory TO ls_result.

      CLEAR: ls_result-xbcode, ls_result-xpasscode, ls_result-xpwdsaltedhash.
      IF ls_usrpwdhistory-bcode         IS NOT INITIAL. ls_result-xbcode         = icon_incomplete.   ENDIF.
      IF ls_usrpwdhistory-passcode      IS NOT INITIAL. ls_result-xpasscode      = icon_incomplete.   ENDIF.
      IF ls_usrpwdhistory-pwdsaltedhash IS NOT INITIAL. ls_result-xpwdsaltedhash = icon_checked.      ENDIF.

      DATA tz TYPE systzonlo.
      CONVERT TIME STAMP ls_usrpwdhistory-timestamp TIME ZONE tz INTO DATE ls_result-modda TIME ls_result-modti.

      CLEAR: ls_color, lt_color.
      ls_color-fname = 'COMMENT'.

      ls_result-comment = space.
      CASE ls_usrpwdhistory-codvn.
        WHEN 'A'. " Code Version A (Obsolete)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'B'. " BCODE Code Version B (MD5-Based, 8 Characters, Upper-Case, ASCII)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'D'. " Code Version D (MD5-Based, 8 Characters, Upper-Case, UTF-8)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'E'. " Code Version E (Corrected Code Version D)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'F'. " Code Version F (SHA1, 40 Characters, Case-Sensitive, UTF-8)
          ls_result-comment = 'weak password hash'.
          ls_color-color-col = col_total.

        WHEN 'G'. " Code Version G = Code Vers. F + Code Vers. B (2 Hash Values)
          ls_result-comment = 'weak and outdated password hash'.
          ls_color-color-col = col_total.

        WHEN 'H'. " PWDSALTEDHASH Code Version H (Generic Hash Procedure)
          ls_result-comment = 'ok'.
          ls_color-color-col = col_positive.

        WHEN 'I'. " Code Version I = Code Versions H + F + B (Three Hash Values)
          ls_result-comment = 'weak password hashes'.
          ls_color-color-col = col_group.

        WHEN 'X'. " Password Deactivated
          ls_result-comment = 'no password'.
          ls_color-color-col = col_positive.

        WHEN OTHERS.
          ls_result-comment = 'unknown hash version'.
          ls_color-color-col = col_total.

      ENDCASE.

      APPEND ls_color TO lt_color.
      ls_result-t_color = lt_color.

      APPEND ls_result TO lt_result.
    ENDSELECT.

  ENDIF.

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

      WHEN 'NOTE'.
        " Show note in browser
        CALL FUNCTION 'OCS_CALL_BROWSER_WITH_NOTE'
          EXPORTING
            iv_note = '1458262'
          EXCEPTIONS
            OTHERS  = 0. "don't care

        "      WHEN 'SU01'.
        "        " Show user (in current client only)
        "        IF ls_cell-row > 0.
        "          CLEAR ls_result.
        "          READ TABLE lt_result INTO ls_result INDEX ls_cell-row.
        "          IF    ls_result-mandt = sy-mandt
        "            AND ls_result-bname IS NOT INITIAL.

        "            CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
        "              EXPORTING
        "                tcode  = 'SU01'
        "              EXCEPTIONS
        "                ok     = 0
        "                not_ok = 2
        "                OTHERS = 3.
        "            IF sy-subrc = 0.
        "              CALL FUNCTION 'SUID_IDENTITY_MAINT'
        "                EXPORTING
        "                  i_username     = ls_result-bname
        "                  i_tcode_mode   = 1
        "                  i_su01_display = 'X'.
        "            ENDIF.
        "          ENDIF.
        "        ENDIF.

      WHEN 'REMOVE'.
        " Delete weak password hashes
        READ TABLE lt_seleced_rows INTO l_row INDEX 1.
        CHECK sy-subrc = 0.

        " Ask user for confirmation
        DATA: l_sel_cnt        TYPE i,
              l_textline1(132),
              l_textline2(132),
              l_textline3(132),
              l_answer(8).

        l_textline1  = 'Do you really want to remove weak password hashes?'.

        DESCRIBE TABLE lt_seleced_rows LINES l_sel_cnt.
        IF l_sel_cnt = 1.
          l_textline2 = '1 entry selected'.
        ELSEIF l_sel_cnt > 1.
          l_answer = l_sel_cnt.
          CONCATENATE l_answer 'entries selected'
            INTO l_textline2 SEPARATED BY space.
        ELSE.
          RETURN. "nothing selected
        ENDIF.

        CLEAR l_answer.
        CALL FUNCTION 'POPUP_TO_DECIDE' ##FM_OLDED
          EXPORTING
            defaultoption     = '2'
            textline1         = l_textline1
            textline2         = l_textline2
            textline3         = l_textline3
            text_option1      = 'Remove'
            text_option2      = 'Cancel'
            icon_text_option1 = 'ICON_DELETE'
            icon_text_option2 = 'ICON_CANCEL'
            titel             = 'Remove weak password hashes'
*           START_COLUMN      = 25
*           START_ROW         = 6
            cancel_display    = space "'X'
          IMPORTING
            answer            = l_answer.
        IF l_answer <> '1'.
          RETURN.
        ENDIF.

        " do it !
        FIELD-SYMBOLS <fs_result> TYPE ts_result.
        LOOP AT lt_seleced_rows INTO l_row.
          CLEAR ls_result.
          READ TABLE lt_result ASSIGNING <fs_result> INDEX l_row.
          CHECK sy-subrc = 0 AND <fs_result>-tabname IS NOT INITIAL.
          CASE <fs_result>-tabname.
            WHEN 'USR02'.

              IF     ( s_lvl1 = 'X' AND ( <fs_result>-codvn CA ' ADIX' ) )
                  OR ( s_lvl2 = 'X' AND ( <fs_result>-codvn CA ' ADIX' OR <fs_result>-user_status IS NOT INITIAL ) )
                  OR ( s_lvl3 = 'X' ).

                " remove BCODE and PASSCODE
                CLEAR: <fs_result>-xbcode, <fs_result>-xpasscode.
                <fs_result>-comment = 'removed'.
                UPDATE usr02 CLIENT SPECIFIED
                  SET bcode    = empty_bcode
                      ocod1    = empty_bcode
                      ocod2    = empty_bcode
                      ocod3    = empty_bcode
                      ocod4    = empty_bcode
                      ocod5    = empty_bcode
                      passcode = empty_passcode
                  WHERE mandt = <fs_result>-mandt
                    AND bname = <fs_result>-bname.

              ELSE.

                " remove BCODE only
                CLEAR: <fs_result>-xbcode.
                <fs_result>-comment = 'weak password hash removed'.
                UPDATE usr02 CLIENT SPECIFIED
                  SET bcode    = empty_bcode
                      ocod1    = empty_bcode
                      ocod2    = empty_bcode
                      ocod3    = empty_bcode
                      ocod4    = empty_bcode
                      ocod5    = empty_bcode
                      "passcode = empty_passcode
                  WHERE mandt = <fs_result>-mandt
                    AND bname = <fs_result>-bname.

              ENDIF.

            WHEN 'USH02'.

              CLEAR: <fs_result>-xbcode, <fs_result>-xpasscode.
              <fs_result>-comment = 'weak password hash removed'.
              UPDATE ush02 CLIENT SPECIFIED
                SET bcode = empty_bcode
                    passcode = empty_passcode
                WHERE mandt = <fs_result>-mandt
                  AND bname = <fs_result>-bname
                  AND modda = <fs_result>-modda
                  AND modti = <fs_result>-modti.

            WHEN 'USRPWDHISTORY'.

              CLEAR: <fs_result>-xbcode, <fs_result>-xpasscode.
              <fs_result>-comment = 'weak password hash removed'.
              UPDATE usrpwdhistory CLIENT SPECIFIED
                SET bcode = empty_bcode
                    passcode = empty_passcode
                WHERE mandt = <fs_result>-mandt
                  AND bname = <fs_result>-bname
                  AND timestamp = <fs_result>-timestamp.

          ENDCASE.
        ENDLOOP.

*       Update ALV list
        IF sy-subrc = 0.
          gr_alv_table->refresh( refresh_mode = if_salv_c_refresh=>soft ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "on_user_command

  METHOD on_double_click.
*   importing row column

    DATA: ls_result    TYPE        ts_result.
*   DATA: lr_Selections  type ref to cl_Salv_selections,
*         ls_cell        type        salv_s_cell.

*   Get selected item
*   lr_selections = gr_ALV_TABLE->get_selections( ).
*   ls_cell = lr_selections->get_current_cell( ).

    "    " Show user (in current client only)
    "    IF row > 0.
    "      CLEAR ls_result.
    "      READ TABLE lt_result INTO ls_result INDEX row.
    "      IF    ls_result-mandt = sy-mandt
    "        AND ls_result-bname IS NOT INITIAL.

    "        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    "          EXPORTING
    "            tcode  = 'SU01'
    "          EXCEPTIONS
    "            ok     = 0
    "            not_ok = 2
    "            OTHERS = 3.
    "        IF sy-subrc = 0.
    "          CALL FUNCTION 'SUID_IDENTITY_MAINT'
    "            EXPORTING
    "              i_username     = ls_result-bname
    "              i_tcode_mode   = 1
    "              i_su01_display = 'X'.
    "        ENDIF.
    "      ENDIF.
    "    ENDIF.

  ENDMETHOD.                    "on_double_click

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION


FORM show_result.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 100
      text       = 'Show result'.

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

* Toolbar der Listausgabe unterdrücken
  cl_abap_list_layout=>suppress_toolbar( ).
* Create an ALV table for grid display
  WRITE space. "trick to get the screen
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container  = cl_gui_container=>default_screen "screen0
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

  DATA: l_repid LIKE sy-repid.
  l_repid = sy-repid.
  CALL FUNCTION 'RS_CUA_STATUS_CHECK'
    EXPORTING
      objectname       = 'SALV_TABLE_STANDARD'
      program          = l_repid
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  IF sy-subrc = 0.
    gr_alv_table->set_screen_status(
      report        = l_repid
      pfstatus      = 'SALV_TABLE_STANDARD'
      set_functions = cl_salv_table=>c_functions_default
      ).

  ENDIF.

* Add custom functions (require a gui container)
  lr_functions_list = gr_alv_table->get_functions( ).

  DATA l_icon TYPE string.
  "IF 1 = 0.
  TRY.
      l_icon = icon_display_note.
      lr_functions_list->add_function(
        name     = 'NOTE'
        icon     = l_icon
        text     = 'Note 1458262'
        tooltip  = 'Show note 1458262'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      "      l_icon = icon_system_user_menu.
      "      lr_functions_list->add_function(
      "        name     = 'SU01'
      "        icon     = l_icon
      "        text     = 'Show user'
      "        tooltip  = 'Show user (current client only)'
      "        position = if_salv_c_function_position=>right_of_salv_functions ).

      l_icon = icon_delete.
      lr_functions_list->add_function(
        name     = 'REMOVE'
        icon     = l_icon
        text     = 'Remove'
        tooltip  = 'Remove selected weak password hashes'
        position = if_salv_c_function_position=>right_of_salv_functions ).
    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.
  "ENDIF.

* set ALV generic funtions of class CL_SALV_FUNCTIONS_LIST
*  lr_functions->SET_DEFAULT( if_salv_c_bool_sap=>true ).
  lr_functions_list->set_detail( if_salv_c_bool_sap=>true ).
  lr_functions_list->set_group_export( if_salv_c_bool_sap=>true ).
  lr_functions_list->set_group_filter( if_salv_c_bool_sap=>true ).
  lr_functions_list->set_group_layout( if_salv_c_bool_sap=>true ).
  lr_functions_list->set_print( if_salv_c_bool_sap=>true ).
  lr_functions_list->set_print_preview( if_salv_c_bool_sap=>true ).
  lr_functions_list->set_group_sort( if_salv_c_bool_sap=>true ).
  lr_functions_list->set_find( if_salv_c_bool_sap=>true ).
  lr_functions_list->set_graphics( if_salv_c_bool_sap=>false ).

* Set the layout
  lr_layout = gr_alv_table->get_layout( ) .
  ls_key-report = sy-repid.
  lr_layout->set_key( ls_key ).
  lr_layout->set_initial_layout( p_layout ).
  AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
                      ID 'ACTVT' FIELD '23'.
  IF sy-subrc = 0.
    lr_layout->set_save_restriction( 3 ) . "no restictions
  ELSE.
    lr_layout->set_save_restriction( 2 ) . "user dependend
  ENDIF.

* Set the columns visible
  lr_columns = gr_alv_table->get_columns( ).
  lr_columns->set_optimize( if_salv_c_bool_sap=>true ).

* Set the fields description and field attributes

  ls_color_key-col      = col_key.
  ls_color_comment-col  = col_normal.

* Sort Columns
  "  LR_SORTS = GR_ALV_TABLE->GET_SORTS( ).
  "  LR_SORTS->CLEAR( ).
  "  try.
  "      LR_SORTS->ADD_SORT(
  "        COLUMNNAME = 'TABNAME'
  "        POSITION   = 1
  "        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP ).
  "      LR_SORTS->ADD_SORT(
  "        COLUMNNAME = 'MANDT'
  "        POSITION   = 2
  "        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP ).
  "      LR_SORTS->ADD_SORT(
  "        COLUMNNAME = 'BNAME'
  "        POSITION   = 3
  "        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP ).

  "    catch CX_SALV_NOT_FOUND
  "          CX_SALV_EXISTING
  "          CX_SALV_DATA_ERROR
  "          into LR_EXCEPTION.
  "      LV_MESSAGE = LR_EXCEPTION->GET_MESSAGE( ).
  "      message id LV_MESSAGE-MSGID type LV_MESSAGE-MSGTY
  "              number LV_MESSAGE-MSGNO
  "              with LV_MESSAGE-MSGV1 LV_MESSAGE-MSGV2
  "                   LV_MESSAGE-MSGV3 LV_MESSAGE-MSGV4.
  "  endtry.

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

*     Table name
      lr_column ?= lr_columns->get_column( 'TABNAME' ).

*     Client
      lr_column ?= lr_columns->get_column( 'MANDT' ).
      "lr_column->set_key( if_salv_c_bool_sap=>true ).
      lr_column->set_color( ls_color_key ).

*     User
      lr_column ?= lr_columns->get_column( 'BNAME' ).
      "lr_column->set_key( if_salv_c_bool_sap=>true ).
      lr_column->set_color( ls_color_key ).

*     Modification time
      lr_column ?= lr_columns->get_column( 'MODTI' ).
      lr_column->set_zero( if_salv_c_bool_sap=>false ).

*     Lock
      lr_column ?= lr_columns->get_column( 'UFLAG' ).
      lr_column->set_zero( if_salv_c_bool_sap=>false ).

*     Last logon time
      lr_column ?= lr_columns->get_column( 'LTIME' ).
      lr_column->set_zero( if_salv_c_bool_sap=>false ).

*     User status
      lr_column ?= lr_columns->get_column( 'USER_STATUS' ).
      lr_column->set_short_text( 'Status' ).
      lr_column->set_medium_text( 'User status' ).
      lr_column->set_long_text( 'User status' ).

*     BCODE
      lr_column ?= lr_columns->get_column( 'XBCODE' ).
      lr_column->set_short_text( 'BCODE' ).
      lr_column->set_medium_text( 'BCODE' ).
      lr_column->set_long_text( 'BCODE' ).

*     PASSCODE
      lr_column ?= lr_columns->get_column( 'XPASSCODE' ).
      lr_column->set_short_text( 'PASSCODE' ).
      lr_column->set_medium_text( 'PASSCODE' ).
      lr_column->set_long_text( 'PASSCODE' ).

*     PWDSALTEDHASH
      lr_column ?= lr_columns->get_column( 'XPWDSALTEDHASH' ).
      lr_column->set_short_text( 'PWDSALTED' ).
      lr_column->set_medium_text( 'PWDSALTEDHASH' ).
      lr_column->set_long_text( 'PWDSALTEDHASH' ).

*     Comment
      lr_column ?= lr_columns->get_column( 'COMMENT' ).
      lr_column->set_short_text( 'Comment' ).
      lr_column->set_medium_text( 'Comment' ).
      lr_column->set_long_text( 'Comment' ).

*     Time stamp in USRPWDHISTORY (hidden, converted to MODDA, MODTI)
      lr_column ?= lr_columns->get_column( 'TIMESTAMP' ).
      lr_column->SET_TECHNICAL( if_salv_c_bool_sap=>true ).

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

* set selection mode
  lr_selections = gr_alv_table->get_selections( ).
  lr_selections->set_selection_mode(
  if_salv_c_selection_mode=>row_column ).

  DATA: l_line                 TYPE i.

* header (not visible on gui container)
  CREATE OBJECT lr_grid_header.
  l_line = 1.

  lr_grid_header->create_label( " CREATE_HEADER_INFORMATION
       row    = l_line
       column = 1
       text   = 'Profile parameters:' ).
  ADD 1 TO l_line.

  PERFORM add_parameter_to_header USING lr_grid_header 'login/password_downwards_compatibility'      CHANGING l_line.
  PERFORM add_parameter_to_header USING lr_grid_header 'login/password_compliance_to_current_policy' CHANGING l_line.
  PERFORM add_parameter_to_header USING lr_grid_header 'login/min_password_lng'                      CHANGING l_line.
  PERFORM add_parameter_to_header USING lr_grid_header 'login/min_password_lowercase'                CHANGING l_line.
  PERFORM add_parameter_to_header USING lr_grid_header 'login/min_password_uppercase'                CHANGING l_line.
  PERFORM add_parameter_to_header USING lr_grid_header 'login/min_password_digits'                   CHANGING l_line.
  PERFORM add_parameter_to_header USING lr_grid_header 'login/min_password_specials'                 CHANGING l_line.

  gr_alv_table->set_top_of_list( lr_grid_header ).

* footer
  CREATE OBJECT lr_grid_footer.
  l_line = 1.

* Program version (not visible on gui container)
  lr_grid_footer->create_text(
       row    = l_line
       column = 1
       text   = 'Program version:'(VER) ).
  lr_grid_footer->create_text(
       row    = l_line
       column = 2
       text   = c_program_version ).
  ADD 1 TO l_line.

  gr_alv_table->set_end_of_list( lr_grid_footer ).

* Set Title
  lr_display_settings = gr_alv_table->get_display_settings( ).
  lr_display_settings->set_list_header( 'Select entries to remove weak password hashes' ). "sy-title
  lr_display_settings->set_list_header_size(
    cl_salv_display_settings=>c_header_size_small ).
  lr_display_settings->set_no_merging( if_salv_c_bool_sap=>true ).

* display the table
  gr_alv_table->display( ).

ENDFORM.

FORM add_parameter_to_header
  USING
    lr_grid_header TYPE REF TO cl_salv_form_layout_grid
    par
  CHANGING
    l_line         TYPE i.

  DATA val TYPE pfeparvalu.

  CALL 'C_SAPGPARAM'
    ID 'NAME'  FIELD par
    ID 'VALUE' FIELD val.

  lr_grid_header->create_text(
       row    = l_line
       column = 1
       text   = par ).

  lr_grid_header->create_text(
       row    = l_line
       column = 2
       text   = val ).

  IF par = 'login/password_downwards_compatibility' AND val <> '0'.
    lr_grid_header->create_text(
         row    = l_line
         column = 3
         text   = 'Change the parameter to recommended value 0' ).
  ENDIF.

  ADD 1 TO l_line.
ENDFORM.
