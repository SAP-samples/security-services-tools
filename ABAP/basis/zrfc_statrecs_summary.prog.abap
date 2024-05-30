*&---------------------------------------------------------------------*
*& Report  ZRFC_STATRECS_SUMMARY
*& Show Workload Statistic of RFC calls
*&---------------------------------------------------------------------*
*& created by:
*& Frank Buchholz
*& SAP Active Global Support - Security Services
*&
*& Source:
*& https://github.com/SAP-samples/security-services-tools
*&
*& Version history
*& 05.12.2010 Initial version
*& 06.12.2010 Show user group and user type for local accounts
*&            Navigation to ABAP editor
*&            Omit program name for RFC Server records
*&            Hide unimportant columns
*& 09.12.2010 Use API to read directory and data
*& 12.12.2010 Show RFC destination options
*& 27.09.2012 Show unused client destinations
*&            Enable subtotals
*& 28.09.2012 Optimize robustness
*& 04.03.2013 Show RFC authorizations of users
*& 05.03.2013 Show available or missing RFC authorizations
*&            List view removed
*&            New ALV for main list; choose ALV layout
*& 03.05.2013 Correct bug if user has FUNC authorizations
*&            Special rule for function group SRFC depending on profile parameter auth/rfc_authority_check
*& 27.05.2013 Show profile parameter stat/rfcrec
*& 19.07.2013 Update TOTAL statistics for today using report SWNCTOTALT
*& 06.12.2013 Hint to solve an issue with form tt_convert_number_to_letter
*& 24.04.2014 Activate ALV total and sub-totals
*& 17.03.2015 Bug: Authorizations with ranges are not supported as the to-field is not used
*& 16.12.2015 Navigation to SM59 for CL and CLD
*& 20.07.2017 Support for all values of auth/rfc_authority_check
*& 26.07.2017 Show Trusted RFC flags
*&            Table RFCSYSACL List of permitted trusted systems for the current system: Systems whose calls are trusted (to be used to SV/SVD)
*&            Table RFCTRUST List of existing trusting systems: Systems who trust current system (to be used for CL/CLD)
*& 26.03.2018 Allow exclude selection for S_USER and S_DEST
*& 26.08.2022 Updated list of tast types
*&            Prepare to analyse authorizations for S_RFCACL
*& 18.01.2023 Tooltip for column Logon Procedure (trusted, basic, no user)
*& 14.09.2023 Show SNC status of outgoing destinations
*&            Show http connections, too
*& 15.09.2023 Optimization
*&---------------------------------------------------------------------*

REPORT  ZRFC_STATRECS_SUMMARY.

constants: c_program_version(14) type c value '15.09.2023 FBT'.

* see function SWNC_COLLECTOR_GET_AGGREGATES
* in include LSCSM_COLLECTORU04

TYPE-POOLS: slis.                                     "Necessary for ALV
TYPE-POOLS: icon, col.
TABLES sscrfields.

* Translate task type (if you get a syntax error with the first include, than try the second one)
*INCLUDE SAPWL_DECODE_TTYPE.
INCLUDE SAPWLSTAD_TT.

SELECTION-SCREEN: FUNCTION KEY 1. "Submit report SWNCTOTALT

* Parameters: see table SWNCMONI
selection-SCREEN BEGIN OF BLOCK bsel WITH FRAME TITLE bsel.
* component: TOTAL or instance name
selection-screen begin of line.
selection-SCREEN comment 1(31) t_COMP.
parameters: p_COMP   TYPE  SWNCHOSTNAME default 'TOTAL'.
selection-screen end of line.
* assigndsys: System id
selection-screen begin of line.
selection-SCREEN comment 1(31) t_SYS.
parameters: p_SYS    TYPE  SWNCSYSID    DEFAULT SY-SYSID.
selection-screen end of line.
* periodtype: M month, W week, D day
selection-screen begin of line.
selection-SCREEN comment 1(31) t_PERIOD.
parameters: p_PERIOD TYPE  SWNCPERITYPE default 'M'. "M / W / D
selection-screen end of line.
* periodstrt: Start date
selection-screen begin of line.
selection-SCREEN comment 1(31) t_STRT.
parameters: p_STRT   TYPE  SWNCDATUM    default sy-datum.
selection-screen end of line.
* End date
selection-screen begin of line.
selection-SCREEN comment 1(31) t_ENDT.
parameters: p_ENDT   TYPE  SWNCDATUM    default sy-datum.
selection-screen end of line.
selection-SCREEN END OF BLOCK bsel.

selection-SCREEN BEGIN OF BLOCK bdat WITH FRAME TITLE bdat.

selection-screen begin of line.
parameters: p_CL     as checkbox default ' '.
selection-SCREEN comment 3(79) t_CL.
selection-screen end of line.
selection-screen begin of line.
parameters: p_CLH    as checkbox default ' '.
selection-SCREEN comment 3(79) t_CLH.
selection-screen end of line.

selection-screen skip.

selection-screen begin of line.
parameters: p_SV     as checkbox default 'X'.
selection-SCREEN comment 3(79) t_SV.
selection-screen end of line.
selection-screen begin of line.
parameters: p_SVH    as checkbox default 'X'.
selection-SCREEN comment 3(79) t_SVH.
selection-screen end of line.

selection-screen skip.

selection-screen begin of line.
parameters: p_CLD    as checkbox default ' '.
selection-SCREEN comment 3(79) t_CLD.
selection-screen end of line.
selection-screen begin of line.
parameters: p_CLDH   as checkbox default ' '.
selection-SCREEN comment 3(79) t_CLDH.
selection-screen end of line.
selection-screen begin of line.
selection-screen POSITION 3.
parameters: p_CLDNU  as checkbox default 'X'.
selection-SCREEN comment 5(31) t_CLDNU.
selection-screen end of line.

selection-screen skip.

selection-screen begin of line.
parameters: p_SVD    as checkbox default ' '.
selection-SCREEN comment 3(79) t_SVD.
selection-screen end of line.
selection-screen begin of line.
parameters: p_SVDH   as checkbox default ' '.
selection-SCREEN comment 3(79) t_SVDH.
selection-screen end of line.

selection-SCREEN END OF BLOCK bdat.

selection-SCREEN BEGIN OF BLOCK bfil WITH FRAME TITLE bfil.
* User
selection-screen begin of line.
selection-SCREEN comment 1(28) t_USER.
SELECT-OPTIONS: s_USER   for sy-uname.
selection-screen end of line.
* RFC Destination
data: l_RFCDEST like RFCDES-RFCDEST.
selection-screen begin of line.
selection-SCREEN comment 1(28) t_DEST.
SELECT-OPTIONS: s_DEST   for l_RFCDEST.
selection-screen end of line.
* RFC Function
data: l_FUNCNAME like RS38L-NAME.
selection-screen begin of line.
selection-SCREEN comment 1(28) t_FUNC.
SELECT-OPTIONS: s_FUNC   for l_FUNCNAME.
selection-screen end of line.
* Function group
data: l_GROUP like RS38L-AREA.
selection-screen begin of line.
selection-SCREEN comment 1(28) t_GROUP.
SELECT-OPTIONS: s_GROUP   for l_GROUP.
selection-screen end of line.
selection-SCREEN END OF BLOCK bfil.

* Layout of ALV output
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) ps_lout FOR FIELD p_layout.
PARAMETERS       p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

* RFC Options
types:
  begin of ts_RFCOPTIONS,
*   RFCDEST     TYPE RFCDISPLAY-RFCDEST,   " Destination
   RFCTYPE(60)," TYPE RFCDISPLAY-RFCTYPE,   " Type text
   RFCHOST(60)," TYPE RFCDISPLAY-RFCHOST,   " Host
   RFCSERVICE  TYPE RFCDISPLAY-RFCSERVICE," Service
   RFCSYSID    TYPE RFCDISPLAY-RFCSYSID,  " Systemid (if logon group)
   PFADPRE(64)," TYPE RFCDISPLAY-PFADPRE,   " Logon group or
   RFCCLIENT   TYPE RFCDISPLAY-RFCCLIENT, " Client
   RFCUSER     TYPE RFCDISPLAY-RFCUSER,   " User  (%_LOG01% = empty)
   RFCAUTH(6),"  TYPE RFCDISPLAY-RFCAUTH, " Password available = %PWD%
   RFCSLOGIN   TYPE RFCDISPLAY-RFCSLOGIN, " Trusted = Y
   RFCSAMEUSR  TYPE RFCDISPLAY-RFCSAMEUSR," Trusted with same user
   "RFCTRUSTID_EXT type string,            " Table RFCTRUST fields RFCTRUSTID TLICENSE_NR RFCMSGSRV
   RFCSNC      TYPE RFCDISPLAY-RFCSNC,    " SNC/TLS
   SSLAPPLIC   TYPE RFCDISPLAY-SSLAPPLIC, " PSE
 end of ts_RFCOPTIONS.

* Main result table
types: begin of ts_result,
         RECORDTYPE(3),
         DATE       like sy-datum,
         TASKTYPE(20), " like SWNCINCRFCH-TASKTYPE,
         MANDT      like SWNCINCRFCH-MANDT,
         ACCOUNT    like SWNCINCRFCH-ACCOUNT,
         USTYP      like USR02-USTYP,
         CLASS      like usr02-class,
         USERID     like SWNCINCRFCH-USERID,       " RFC only
         RFC_CALLER like SWNCINCRFCH-USERID,       " RFC server only
         TARGET     like SWNCINCRFCH-TARGET,
         LOCAL_DEST like SWNCINCRFCH-LOCAL_DEST,   " RFC only
         REMOT_DEST like SWNCINCRFCH-REMOT_DEST,   " RFC only
         FUNC_NAME  like RS38L-NAME,               " RFC only
         GROUP      like RS38L-AREA,
         AUTH_S_RFC type string,                   " RFC only
         AUTH_S_RFCACL type string,                " RFC only
         PROG_NAME  like RS38L-PROGNAME,           " RFC client only
         PROTOCOL   type string,                   " SWNCWEBPROT, HTTP only
         HOST       type SWNCTXT128,               " HTTP only
         PORT       type SWNCTXT08,                " HTTP only
         PATH       type SWNCTXT250,               " HTTP only
         ENTRY_ID   like SWNCINCRFCH-ENTRY_ID,
         COUNTER    like SWNCINCRFCB-COUNTER,
         RECEIVE    like SWNCINCRFCB-RECEIVE,      " Corresponding field for HTTP: DATA_RECEIVE
         SEND       like SWNCINCRFCB-SEND,         " Corresponding field for HTTP: DATA_SEND
         EXE_TIME   like SWNCINCRFCB-EXE_TIME,     " Corresponding field for HTTP: EXECUTION_TI
         CALL_TIME  like SWNCINCRFCB-CALL_TIME,    " Corresponding field for HTTP: CALLTIME
         CALLS      like SWNCAGGRFCCLNTDEST-CALLS,
         RFCOPTIONS type ts_RFCOPTIONS,
       end of ts_result.

data: gs_result     type ts_result,
      gt_result     type table of ts_result.

*-----------------------------------------------------------------------

* Authorizations
types:
  begin of ts_FNAME,
    FROM_VALUE type c length 30,
    TO_VALUE   type c length 30,
  end of ts_FNAME,
  tt_FNAME type standard table of ts_FNAME with default key,
  begin of ts_user_auth,
    mandt type usr02-mandt,
    bname type usr02-bname,
    USTYP type usr02-USTYP,
    CLASS type usr02-CLASS,
    FUGR  type tt_FNAME,  " authorizations for S_RFC
    FUNC  type tt_FNAME,  " authorizations for S_RFC
    S_RFCACL type string, " authorizations for S_RFCACL
  end of ts_user_auth,
  tt_user_auth type sorted table of ts_user_auth
    with non-unique key mandt bname.

data: gt_user_auth type tt_user_auth,
      gs_user_auth type ts_user_auth.

* ALV
types:
  begin of ts_USER,
    mandt   type usr02-mandt,
    bname   type usr02-bname,
    USTYP   type usr02-USTYP,
    CLASS   type usr02-CLASS,
    FUGR    type RS38L-AREA,
    FUNC    type TFDIR-FUNCNAME,
    COUNTER like SWNCINCRFCB-COUNTER,
  end of ts_USER,
  TT_USER type table of ts_USER.

data: gt_USER type TT_USER,
      gs_USER type TS_USER.

*-----------------------------------------------------------------------

DATA: gs_alv_lout_variant TYPE disvariant.
data: l_stat_rfcrec(6).

*-----------------------------------------------------------------------

initialization.
  concatenate 'Program version'(t00) c_program_version into SS_VERS
    SEPARATED BY SPACE.

  DATA: functxt TYPE smp_dyntxt.
  functxt-icon_id   = ICON_TOOLS.
  functxt-quickinfo = 'Update TOTAL statistics for today'(023).
  functxt-icon_text = 'Update TOTAL'(024).
  sscrfields-functxt_01 = functxt.

  bsel     = 'Data Selection'(t01).
  t_COMP   = 'Component (TOTAL or <instance>'(t02).
  t_SYS    = 'System ID'(t03).
  t_PERIOD = 'Period (M=month, W=week, D=day)'(t04).
  t_STRT   = 'Start datum'(t05).
  t_ENDT   = 'End datum'(t06).

  bdat     = 'Data Options'(t07).
*             1234567890123456789012345678901234567890123456789012345678901234567890123456789
  t_CL     = 'CL: Show RFC functions executed by RFC client in another system'(t08).
  t_CLH    = 'CL: Show HTTP calls executed by HTTP client in another system'(t19).

  t_SV     = 'SV: Show RFC functions executing in RFC server (and show RFC authorizations)'(t09).
  t_SVH    = 'SV: Show HTTP calls executing in HTTP server'(t20).

  t_CLD    = 'CLD: Show RFC destinations for other systems called by RFC client'(t10).
  t_CLDH   = 'CLD: Show HTTP destinations for other systems called by HTTP client'(t21).
  t_CLDNU  = 'show unused destinations, too'(t11).

  t_SVD    = 'SVD: Show RFC destinations calling into RFC server'(t12).
  t_SVDH   = 'SVD: Show http destinations calling into HTTP server'(t22).

  bfil     = 'Filter Options'(t13).
  t_USER   = 'User'(t14).
  t_DEST   = 'Destination'(t15).
  t_FUNC   = 'RFC function'(t16).
  t_GROUP  = 'Function group'(t17).

  ps_lout  = 'Layout'(t18).

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN
*----------------------------------------------------------------------*

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      submit SWNCTOTALT AND RETURN.
      MESSAGE i861(s06).
  endcase.

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

*-----------------------------------------------------------------------

start-of-selection.
  perform MAIN.

*-----------------------------------------------------------------------
*  FORM MAIN
*-----------------------------------------------------------------------
FORM MAIN.

  data: lt_DIRECTORY_KEYS   TYPE TABLE OF SWNCMONIKEY,
        ls_DIRECTORY_KEYS   TYPE          SWNCMONIKEY,
*      lT_DIRECTORY_MONI   TYPE TABLE OF SWNCMONIKEYRELID,
        " RFC data
        lt_rfcclnt          TYPE          swnc_t_aggrfccli,  "wo
        ls_rfcclnt          TYPE LINE OF  swnc_t_aggrfccli,
        lt_rfcsrvr          TYPE          swnc_t_aggrfcsrv,  "wq
        ls_rfcsrvr          TYPE LINE OF  swnc_t_aggrfcsrv,
        lt_rfcclntdest      TYPE          swnc_t_aggrfcclid, "wp
        ls_rfcclntdest      TYPE LINE OF  swnc_t_aggrfcclid,
        lt_rfcsrvrdest      TYPE          swnc_t_aggrfcsrvd, "wp
        ls_rfcsrvrdest      TYPE LINE OF  swnc_t_aggrfcsrvd,
        " HTTP data
        lt_webc             TYPE          SWNC_T_AGGWEBCLNT,
        ls_webc             TYPE          SWNCAGGWEBCLNT,
        lt_webcd            TYPE          SWNC_T_AGGWEBDEST,
        ls_webcd            TYPE          SWNCAGGWEBDEST,
        lt_webs             TYPE          SWNC_T_AGGWEBCLNT,
        ls_webs             TYPE          SWNCAGGWEBCLNT,
        lt_websd            TYPE          SWNC_T_AGGWEBDEST,
        ls_websd            TYPE          SWNCAGGWEBDEST,

        division_factor     TYPE SWNCDIVFACTOR value 1000.


* Authorization to show Workload Statistics (see transaction ST03N)
  AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
    ID 'S_ADMI_FCD' FIELD 'ST0R'.
  IF sy-subrc <> 0.
    MESSAGE E150(00) with 'Workload Statistics'(001).
  ENDIF.

* Authorization check for retrieving user specific data
  AUTHORITY-CHECK OBJECT 'S_TOOLS_EX'
    ID 'AUTH' FIELD 'S_TOOLS_EX_A'.
  IF sy-subrc <> 0.
*   Warning is ok: If you do not have the authorization you will get
*   anonymized data only
    MESSAGE W008(S03).  "authorization missing
  ENDIF.

* Read Profile parameter
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'stat/rfcrec'
                     ID 'VALUE' FIELD l_stat_rfcrec.

  TRANSLATE p_PERIOD TO UPPER CASE.

* Adjust selection
  IF p_PERIOD = 'M'.
*   Set date to first day of month
    p_STRT+6(2) = '01'.
    p_ENDT+6(2) = '01'.
  ELSEIF p_PERIOD = 'W'.
*   Set date to first day of week
    DATA: n TYPE i.
    n = ( ( sy-datum + 1 - sy-fdayw ) - p_STRT ) MOD 7.
    IF n <> 0.
      p_STRT = n + p_STRT - 7.
    ELSE.
*     no change
    ENDIF.
    n = ( ( sy-datum + 1 - sy-fdayw ) - p_ENDT ) MOD 7.
    IF n <> 0.
      p_ENDT = n + p_ENDT - 7.
    ELSE.
*     no change
    ENDIF.
  ENDIF.

* Prepare include respective exclude selection for S_USER and S_DEST
  data: S_USER_SIGN like s_USER-SIGN,
        S_DEST_SIGN like s_DEST-SIGN.
  clear: S_USER_SIGN, S_DEST_SIGN.
  loop at S_USER.
    if s_USER-SIGN = 'I'.
      if     S_USER_SIGN is initial.
        S_USER_SIGN = 'I'.
      elseif S_USER_SIGN = 'E'.
        MESSAGE E150(00) with 'Do not mix positive and negative selections for users'(028).
      endif.
    elseif s_USER-SIGN = 'E'.
      if     S_USER_SIGN is initial.
        S_USER_SIGN = 'E'.
      elseif S_USER_SIGN = 'I'.
        MESSAGE E150(00) with 'Do not mix positive and negative selections for users'(028).
      endif.
    endif.
  endloop.
  loop at S_DEST.
    if s_DEST-SIGN = 'I'.
      if     S_DEST_SIGN is initial.
        S_DEST_SIGN = 'I'.
      elseif S_DEST_SIGN = 'E'.
        MESSAGE E150(00) with 'Do not mix positive and negative selections for destinations'(029).
      endif.
    elseif s_DEST-SIGN = 'E'.
      if     S_DEST_SIGN is initial.
        S_DEST_SIGN = 'E'.
      elseif S_DEST_SIGN = 'I'.
        MESSAGE E150(00) with 'Do not mix positive and negative selections for destinations'(029).
      endif.
    endif.
  endloop.

* Read directory
* GET_DIR_FROM_CLUSTER = ' ' -> DIRECTORY_KEYS
  CALL FUNCTION 'SWNC_COLLECTOR_GET_DIRECTORY'
    EXPORTING
      GET_DIR_FROM_CLUSTER = ' '
*     EXCLUDE_SUMMARY      = ' '
*     STORAGE_TYPE         = ' ' " SPACE or 'A'
    TABLES
*     DIRECTORY_FULL       =
*     DIRECTORY_SHORT      =
      DIRECTORY_KEYS       = lt_DIRECTORY_KEYS
*     DIRECTORY_MONI       = lt_DIRECTORY_MONI
    EXCEPTIONS
      NO_DATA_FOUND        = 1
      OTHERS               = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  delete lt_DIRECTORY_KEYS
    where COMPONENT  ne p_COMP
       or COMPTYPE   ne cl_swnc_collector=>collector_wload " or cl_swnc_collector=>COLLECTOR_ASTAT
       or ASSIGNDSYS ne p_SYS
       or PERIODTYPE ne p_PERIOD
       or PERIODSTRT <  p_STRT
       or PERIODSTRT >  p_ENDT.
** GET_DIR_FROM_CLUSTER = 'X' -> DIRECTORY_MONI
*  CALL FUNCTION 'SWNC_COLLECTOR_GET_DIRECTORY'
*    EXPORTING
*      GET_DIR_FROM_CLUSTER       = 'X' " takes longer
**     EXCLUDE_SUMMARY            = ' '
**     STORAGE_TYPE         = ' ' " SPACE or 'A'
*    TABLES
**     DIRECTORY_FULL             =
**     DIRECTORY_SHORT            =
**     DIRECTORY_KEYS             = lt_DIRECTORY_KEYS
*      DIRECTORY_MONI             = lt_DIRECTORY_MONI
*    EXCEPTIONS
*      NO_DATA_FOUND              = 1
*      OTHERS                     = 2
*            .
*  IF SY-SUBRC <> 0.
**   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*  delete lt_DIRECTORY_MONI
*    where COMPONENT  ne p_COMP
*       or COMPTYPE   ne cl_swnc_collector=>collector_wload
*       or ASSIGNDSYS ne p_SYS
*       or PERIODTYPE ne p_PERIOD
*       or PERIODSTRT <  p_STRT
*       or PERIODSTRT >  p_ENDT
*       or (    RELID ne 'WO'
*           and RELID ne 'WP'
*           and RELID ne 'WQ'
*           and RELID ne 'WR' ).

* Read data

  loop at lt_DIRECTORY_KEYS into ls_DIRECTORY_KEYS.
    clear gs_result.
    gs_result-date = ls_DIRECTORY_KEYS-PERIODSTRT.

*   RFC Client
*   empty fields: RFC_CALLER, CALLS
*   MANDT, ACCOUNT: calling user in RFC client (local system)
*   USERID:         called user in RFC server
*   RFC_CALLER:     empty
*   RFCUSER:        called user in RFC server (as defined in RFC dest. or "<same user>")
    if p_CL = 'X'.
      gs_result-recordtype = 'CL'.

      clear lt_rfcclnt[].
      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          COMPONENT     = ls_DIRECTORY_KEYS-COMPONENT
          ASSIGNDSYS    = ls_DIRECTORY_KEYS-ASSIGNDSYS
          PERIODTYPE    = ls_DIRECTORY_KEYS-PERIODTYPE
          PERIODSTRT    = ls_DIRECTORY_KEYS-PERIODSTRT
*         SUMMARY_ONLY  = ' '
*         STORAGE_TYPE  = 'A'
          FACTOR        = division_factor
        TABLES
          RFCCLNT       = lt_rfcclnt       "wo
*         RFCCLNTDEST   = lt_rfcclntdest   "wp
*         RFCSRVR       = lt_rfcsrvr       "wq
*         RFCSRVRDEST   = lt_rfcsrvrdest   "wr
*         WEBC          = lt_webc          " TYPE SWNCAGGWEBCLNT
*         WEBCD         = lt_webcd         " TYPE SWNCAGGWEBDEST
*         WEBS          = lt_webs          " TYPE SWNCAGGWEBCLNT
*         WEBSD         = lt_websd         " TYPE SWNCAGGWEBDEST
        EXCEPTIONS
          NO_DATA_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      loop at lt_rfcclnt into ls_rfcclnt
        where FUNC_NAME  in s_FUNC.
        if     S_USER_SIGN = 'I'.
          check ls_rfcclnt-account    in s_USER
             or ls_rfcclnt-userid     in s_USER.
        elseif S_USER_SIGN = 'E'.
          check ls_rfcclnt-account    in s_USER
            AND ls_rfcclnt-userid     in s_USER.
        endif.
        if     S_DEST_SIGN = 'I'.
          check ls_rfcclnt-TARGET     in s_DEST
             or ls_rfcclnt-LOCAL_DEST in s_DEST
             or ls_rfcclnt-REMOT_DEST in s_DEST.
        elseif S_DEST_SIGN = 'E'.
          check ls_rfcclnt-TARGET     in s_DEST
            AND ls_rfcclnt-LOCAL_DEST in s_DEST
            AND ls_rfcclnt-REMOT_DEST in s_DEST.
        endif.

        move-CORRESPONDING ls_rfcclnt to gs_result.
        PERFORM translate_tasktype using ls_rfcclnt-tasktype.
        perform get_function_group
          using    ls_rfcclnt-FUNC_NAME
          changing gs_result-group.
        check gs_result-GROUP      in s_GROUP.
        perform get_user_data
          using    ls_rfcclnt-mandt ls_rfcclnt-account
          changing gs_result-USTYP  gs_result-CLASS.
        perform get_destination_data
          using    gs_result-TARGET
          changing gs_result-rfcoptions.

        append gs_result to gt_result.
      endloop.
      free lt_rfcclnt.
    endif.

    " HTTP Client
    if p_CLH = 'X'.
      gs_result-recordtype = 'CL'.

      clear lt_webc[].
      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          COMPONENT     = ls_DIRECTORY_KEYS-COMPONENT
          ASSIGNDSYS    = ls_DIRECTORY_KEYS-ASSIGNDSYS
          PERIODTYPE    = ls_DIRECTORY_KEYS-PERIODTYPE
          PERIODSTRT    = ls_DIRECTORY_KEYS-PERIODSTRT
*         SUMMARY_ONLY  = ' '
*         STORAGE_TYPE  = 'A'
          FACTOR        = division_factor
        TABLES
*         RFCCLNT       = lt_rfcclnt       "wo
*         RFCCLNTDEST   = lt_rfcclntdest   "wp
*         RFCSRVR       = lt_rfcsrvr       "wq
*         RFCSRVRDEST   = lt_rfcsrvrdest   "wr
          WEBC          = lt_webc          " TYPE SWNCAGGWEBCLNT
*         WEBCD         = lt_webcd         " TYPE SWNCAGGWEBDEST
*         WEBS          = lt_webs          " TYPE SWNCAGGWEBCLNT
*         WEBSD         = lt_websd         " TYPE SWNCAGGWEBDEST
        EXCEPTIONS
          NO_DATA_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      loop at lt_webc into ls_webc.
        if     S_USER_SIGN = 'I'.
          check ls_webc-account    in s_USER.
        elseif S_USER_SIGN = 'E'.
          check ls_webc-account    in s_USER.
        endif.
        if     S_DEST_SIGN = 'I'.
          check ls_webc-DESTINATION in s_DEST.
        elseif S_DEST_SIGN = 'E'.
          check ls_webc-DESTINATION in s_DEST.
        endif.

        move-CORRESPONDING ls_webc to gs_result.
        gs_result-TARGET    = ls_webc-DESTINATION.
        gs_result-RECEIVE   = ls_webc-DATA_RECEIVE.
        gs_result-SEND      = ls_webc-DATA_SEND.
        gs_result-EXE_TIME  = ls_webc-EXECUTION_TI.
        gs_result-CALL_TIME = ls_webc-CALLTIME.
        PERFORM translate_tasktype using ls_webc-tasktype.
        perform translate_protocol USING gs_result-protocol.
        perform get_user_data
          using    ls_webc-mandt ls_webc-account
          changing gs_result-USTYP  gs_result-CLASS.
        perform get_destination_data
          using    gs_result-TARGET
          changing gs_result-rfcoptions.
*        if gs_result-protocol is INITIAL.
*          if gs_result-rfcoptions-rfcsnc = 'X'.
*            gs_result-protocol = 'https'.
*          else.
*            gs_result-protocol = 'http'.
*          endif.
*        endif.
*        if gs_result-host is INITIAL.
*          gs_result-host = gs_result-rfcoptions-rfchost.
*        endif.
*        if gs_result-port is INITIAL.
*          gs_result-port = gs_result-rfcoptions-rfcsysid.
*        endif.
*        if gs_result-path is INITIAL.
*          gs_result-path = gs_result-rfcoptions-PFADPRE.
*        endif.

        if gs_result-path = '@H3@'. " Don't show this as an icon.
           clear gs_result-path.
        endif.

        append gs_result to gt_result.
      endloop.
      free lt_webc.
    endif.

*   RFC Server
*   empty fields: CALLS
*   MANDT, ACCOUNT: called user in RFC server (local system)
*   USERID:         called user in RFC server (local system)
*   RFC_CALLER:     called user in RFC server (local system)
*   RFCUSER:        empty
    if p_SV = 'X'.
      gs_result-recordtype = 'SV'.

      clear lt_rfcsrvr[].
      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          COMPONENT     = ls_DIRECTORY_KEYS-COMPONENT
          ASSIGNDSYS    = ls_DIRECTORY_KEYS-ASSIGNDSYS
          PERIODTYPE    = ls_DIRECTORY_KEYS-PERIODTYPE
          PERIODSTRT    = ls_DIRECTORY_KEYS-PERIODSTRT
*         SUMMARY_ONLY  = ' '
*         STORAGE_TYPE  = 'A'
          FACTOR        = division_factor
        TABLES
*         RFCCLNT       = lt_rfcclnt       "wo
*         RFCCLNTDEST   = lt_rfcclntdest   "wp
          RFCSRVR       = lt_rfcsrvr       "wq
*         RFCSRVRDEST   = lt_rfcsrvrdest   "wr
*         WEBC          = lt_webc          " TYPE SWNCAGGWEBCLNT
*         WEBCD         = lt_webcd         " TYPE SWNCAGGWEBDEST
*         WEBS          = lt_webs          " TYPE SWNCAGGWEBCLNT
*         WEBSD         = lt_websd         " TYPE SWNCAGGWEBDEST
       EXCEPTIONS
          NO_DATA_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      loop at lt_rfcsrvr into ls_rfcsrvr
        where FUNC_NAME  in s_FUNC.
        if     S_USER_SIGN = 'I'.
          check ls_rfcsrvr-account    in s_USER
             or ls_rfcsrvr-userid     in s_USER
             or ls_rfcsrvr-rfc_caller in s_USER.
        elseif S_USER_SIGN = 'E'.
          check ls_rfcsrvr-account    in s_USER
            AND ls_rfcsrvr-userid     in s_USER
            AND ls_rfcsrvr-rfc_caller in s_USER.
        endif.
        if     S_DEST_SIGN = 'I'.
          check ls_rfcsrvr-TARGET     in s_DEST
             or ls_rfcsrvr-LOCAL_DEST in s_DEST
             or ls_rfcsrvr-REMOT_DEST in s_DEST.
        elseif S_DEST_SIGN = 'E'.
          check ls_rfcsrvr-TARGET     in s_DEST
            AND ls_rfcsrvr-LOCAL_DEST in s_DEST
            AND ls_rfcsrvr-REMOT_DEST in s_DEST.
        endif.

        move-CORRESPONDING ls_rfcsrvr to gs_result.
        clear gs_result-prog_name. "Not useful for RFC Server
        PERFORM translate_tasktype using ls_rfcsrvr-tasktype.
        perform get_function_group
          using    ls_rfcsrvr-FUNC_NAME
          changing gs_result-group.
        check gs_result-GROUP      in s_GROUP.
        perform get_user_data
          using    ls_rfcsrvr-mandt ls_rfcsrvr-account
          changing gs_result-USTYP  gs_result-CLASS.
        perform get_authorization
          using    ls_rfcsrvr-mandt ls_rfcsrvr-account
                   gs_result-FUNC_NAME gs_result-GROUP
          changing gs_result-AUTH_S_RFC
                   gs_result-AUTH_S_RFCACL.

        append gs_result to gt_result.
      endloop.
      free lt_rfcsrvr.
    endif.

    " HTTP Server
    if p_SVH = 'X'.
      gs_result-recordtype = 'SV'.

      clear lt_webs[].
      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          COMPONENT     = ls_DIRECTORY_KEYS-COMPONENT
          ASSIGNDSYS    = ls_DIRECTORY_KEYS-ASSIGNDSYS
          PERIODTYPE    = ls_DIRECTORY_KEYS-PERIODTYPE
          PERIODSTRT    = ls_DIRECTORY_KEYS-PERIODSTRT
*         SUMMARY_ONLY  = ' '
*         STORAGE_TYPE  = 'A'
          FACTOR        = division_factor
        TABLES
*         RFCCLNT       = lt_rfcclnt       "wo
*         RFCCLNTDEST   = lt_rfcclntdest   "wp
*         RFCSRVR       = lt_rfcsrvr       "wq
*         RFCSRVRDEST   = lt_rfcsrvrdest   "wr
*         WEBC          = lt_webc          " TYPE SWNCAGGWEBCLNT
*         WEBCD         = lt_webcd         " TYPE SWNCAGGWEBDEST
          WEBS          = lt_webs          " TYPE SWNCAGGWEBCLNT
*         WEBSD         = lt_websd         " TYPE SWNCAGGWEBDEST
        EXCEPTIONS
          NO_DATA_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      loop at lt_webs into ls_webs.
        if     S_USER_SIGN = 'I'.
          check ls_webs-account    in s_USER.
        elseif S_USER_SIGN = 'E'.
          check ls_webs-account    in s_USER.
        endif.
        if     S_DEST_SIGN = 'I'.
          check ls_webs-DESTINATION in s_DEST.
        elseif S_DEST_SIGN = 'E'.
          check ls_webs-DESTINATION in s_DEST.
        endif.

        move-CORRESPONDING ls_webs to gs_result.
        gs_result-TARGET    = ls_webs-DESTINATION.
        gs_result-RECEIVE   = ls_webs-DATA_RECEIVE.
        gs_result-SEND      = ls_webs-DATA_SEND.
        gs_result-EXE_TIME  = ls_webs-EXECUTION_TI.
        gs_result-CALL_TIME = ls_webs-CALLTIME.
        PERFORM translate_tasktype using ls_webs-tasktype.
        perform translate_protocol USING gs_result-protocol.
        perform get_user_data
          using    ls_webs-mandt ls_webs-account
          changing gs_result-USTYP  gs_result-CLASS.

        if gs_result-path = '@H3@'. " Don't show this as an icon.
           clear gs_result-path.
        endif.

        append gs_result to gt_result.
      endloop.
      free lt_webs.
    endif.

*   RFC Client Destinations
*   empty fields: RFC_CALLER, PROG_NAME , FUNC_NAME
*   MANDT, ACCOUNT: calling user in RFC client (local system)
*   USERID:         called user in RFC server
*   RFC_CALLER:     empty
*   RFCUSER:        called user in RFC server (as defined in RFC dest. or "<same user>")
    if p_CLD = 'X'.
      gs_result-recordtype = 'CLD'.

      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          COMPONENT     = ls_DIRECTORY_KEYS-COMPONENT
          ASSIGNDSYS    = ls_DIRECTORY_KEYS-ASSIGNDSYS
          PERIODTYPE    = ls_DIRECTORY_KEYS-PERIODTYPE
          PERIODSTRT    = ls_DIRECTORY_KEYS-PERIODSTRT
*         SUMMARY_ONLY  = ' '
*         STORAGE_TYPE  = 'A'
          FACTOR        = division_factor
        TABLES
*         RFCCLNT       = lt_rfcclnt       "wo
          RFCCLNTDEST   = lt_rfcclntdest   "wp
*         RFCSRVR       = lt_rfcsrvr       "wq
*         RFCSRVRDEST   = lt_rfcsrvrdest   "wr
*         WEBC          = lt_webc          " TYPE SWNCAGGWEBCLNT
*         WEBCD         = lt_webcd         " TYPE SWNCAGGWEBDEST
*         WEBS          = lt_webs          " TYPE SWNCAGGWEBCLNT
*         WEBSD         = lt_websd         " TYPE SWNCAGGWEBDEST
        EXCEPTIONS
          NO_DATA_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        clear lt_rfcclntdest[].
      ENDIF.

*     Add not-used RFC destinations
      if p_CLDNU = 'X'.
        clear ls_rfcclntdest.
        select RFCDEST from RFCDES into ls_rfcclntdest-TARGET
          where RFCDEST in s_DEST
            and RFCTYPE = '3'.

          read table lt_rfcclntdest transporting no fields
            with key TARGET = ls_rfcclntdest-TARGET.
          if sy-subrc is not initial.
            append ls_rfcclntdest to lt_rfcclntdest.
          endif.
        endselect.
      endif.

      loop at lt_rfcclntdest into ls_rfcclntdest.
        if     S_USER_SIGN = 'I'.
          check ls_rfcclntdest-account    in s_USER
             or ls_rfcclntdest-userid     in s_USER.
        elseif S_USER_SIGN = 'E'.
          check ls_rfcclntdest-account    in s_USER
            AND ls_rfcclntdest-userid     in s_USER.
        endif.
        if     S_DEST_SIGN = 'I'.
          check ls_rfcclntdest-TARGET     in s_DEST
             or ls_rfcclntdest-LOCAL_DEST in s_DEST
             or ls_rfcclntdest-REMOT_DEST in s_DEST.
        elseif S_DEST_SIGN = 'E'.
          check ls_rfcclntdest-TARGET     in s_DEST
            AND ls_rfcclntdest-LOCAL_DEST in s_DEST
            AND ls_rfcclntdest-REMOT_DEST in s_DEST.
        endif.

        move-CORRESPONDING ls_rfcclntdest to gs_result.
        PERFORM translate_tasktype using ls_rfcclntdest-tasktype.
        perform get_user_data
          using    ls_rfcclnt-mandt ls_rfcclntdest-account
          changing gs_result-USTYP  gs_result-CLASS.
        perform get_destination_data
          using    gs_result-TARGET
          changing gs_result-rfcoptions.
        if ls_rfcclntdest-tasktype = 0 and ls_rfcclntdest-counter = 0.
          "if gs_result-rfcoptions-RFCTRUSTID_EXT is initial.
            gs_result-tasktype = 'NOT USED'.
          "else.
          "  gs_result-tasktype = 'NOT USED/REQUIRED'.
          "endif.
        endif.

        append gs_result to gt_result.
      endloop.
      free lt_rfcclnt.
    endif.

    " HTTP Client destination
    if p_CLDH = 'X'.
      gs_result-recordtype = 'CLD'.

      clear lt_webcd[].
      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          COMPONENT     = ls_DIRECTORY_KEYS-COMPONENT
          ASSIGNDSYS    = ls_DIRECTORY_KEYS-ASSIGNDSYS
          PERIODTYPE    = ls_DIRECTORY_KEYS-PERIODTYPE
          PERIODSTRT    = ls_DIRECTORY_KEYS-PERIODSTRT
*         SUMMARY_ONLY  = ' '
*         STORAGE_TYPE  = 'A'
          FACTOR        = division_factor
        TABLES
*         RFCCLNT       = lt_rfcclnt       "wo
*         RFCCLNTDEST   = lt_rfcclntdest   "wp
*         RFCSRVR       = lt_rfcsrvr       "wq
*         RFCSRVRDEST   = lt_rfcsrvrdest   "wr
*         WEBC          = lt_webc          " TYPE SWNCAGGWEBCLNT
          WEBCD         = lt_webcd         " TYPE SWNCAGGWEBDEST
*         WEBS          = lt_webs          " TYPE SWNCAGGWEBCLNT
*         WEBSD         = lt_websd         " TYPE SWNCAGGWEBDEST
        EXCEPTIONS
          NO_DATA_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

*     Add not-used HTTP destinations
      if p_CLDNU = 'X'.
        clear ls_webcd.
        select RFCDEST from RFCDES into ls_webcd-DESTINATION
          where RFCDEST in s_DEST
            and ( RFCTYPE = 'G' or RFCTYPE = 'H' ).

          read table lt_webcd transporting no fields
            with key DESTINATION = ls_webcd-DESTINATION.
          if sy-subrc is not initial.
            append ls_webcd to lt_webcd.
          endif.
        endselect.
      endif.

      loop at lt_webcd into ls_webcd.
        if     S_USER_SIGN = 'I'.
          check ls_webcd-account    in s_USER.
        elseif S_USER_SIGN = 'E'.
          check ls_webcd-account    in s_USER.
        endif.
        if     S_DEST_SIGN = 'I'.
          check ls_webcd-DESTINATION in s_DEST.
        elseif S_DEST_SIGN = 'E'.
          check ls_webcd-DESTINATION in s_DEST.
        endif.

        move-CORRESPONDING ls_webcd to gs_result.
        gs_result-TARGET    = ls_webcd-DESTINATION.
        gs_result-RECEIVE   = ls_webcd-DATA_RECEIVE.
        gs_result-SEND      = ls_webcd-DATA_SEND.
        gs_result-EXE_TIME  = ls_webcd-EXECUTION_TI.
        gs_result-CALL_TIME = ls_webcd-CALLTIME.
        PERFORM translate_tasktype using ls_webcd-tasktype.
        perform translate_protocol USING gs_result-protocol.
        perform get_user_data
          using    ls_webcd-mandt ls_webcd-account
          changing gs_result-USTYP  gs_result-CLASS.
        perform get_destination_data
          using    gs_result-TARGET
          changing gs_result-rfcoptions.
*        if gs_result-protocol is INITIAL.
*          if gs_result-rfcoptions-rfcsnc = 'X'.
*            gs_result-protocol = 'https'.
*          else.
*            gs_result-protocol = 'http'.
*          endif.
*        endif.
*        if gs_result-host is INITIAL.
*          gs_result-host = gs_result-rfcoptions-rfchost.
*        endif.
*        if gs_result-port is INITIAL.
*          gs_result-port = gs_result-rfcoptions-rfcsysid.
*        endif.
*        if gs_result-path is INITIAL.
*          gs_result-path = gs_result-rfcoptions-PFADPRE.
*        endif.

        if gs_result-path = '@H3@'. " Don't show this as an icon.
           clear gs_result-path.
        endif.

        append gs_result to gt_result.
      endloop.
      free lt_webcd.
    endif.

*   RFC Server Destinations
*   empty fields: PROG_NAME , FUNC_NAME
*   MANDT, ACCOUNT: called user in RFC server (local system)
*   USERID:         called user in RFC server (local system)
*   RFC_CALLER:     called user in RFC server (local system)
*   RFCUSER:        empty
    if p_SVD = 'X'.
      gs_result-recordtype = 'SVD'.

      clear lt_rfcsrvrdest[].
      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          COMPONENT     = ls_DIRECTORY_KEYS-COMPONENT
          ASSIGNDSYS    = ls_DIRECTORY_KEYS-ASSIGNDSYS
          PERIODTYPE    = ls_DIRECTORY_KEYS-PERIODTYPE
          PERIODSTRT    = ls_DIRECTORY_KEYS-PERIODSTRT
*         SUMMARY_ONLY  = ' '
*         STORAGE_TYPE  = 'A'
          FACTOR        = division_factor
        TABLES
*         RFCCLNT       = lt_rfcclnt       "wo
*         RFCCLNTDEST   = lt_rfcclntdest   "wp
*         RFCSRVR       = lt_rfcsrvr       "wq
          RFCSRVRDEST   = lt_rfcsrvrdest   "wr
*         WEBC          = lt_webc          " TYPE SWNCAGGWEBCLNT
*         WEBCD         = lt_webcd         " TYPE SWNCAGGWEBDEST
*         WEBS          = lt_webs          " TYPE SWNCAGGWEBCLNT
*         WEBSD         = lt_websd         " TYPE SWNCAGGWEBDEST
        EXCEPTIONS
          NO_DATA_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      loop at lt_rfcsrvrdest into ls_rfcsrvrdest.
        if     S_USER_SIGN = 'I'.
          check ls_rfcsrvrdest-account    in s_USER
             or ls_rfcsrvrdest-userid     in s_USER
             or ls_rfcsrvrdest-rfc_caller in s_USER.
        elseif S_USER_SIGN = 'E'.
          check ls_rfcsrvrdest-account    in s_USER
            AND ls_rfcsrvrdest-userid     in s_USER
            AND ls_rfcsrvrdest-rfc_caller in s_USER.
        endif.
        if     S_DEST_SIGN = 'I'.
          check ls_rfcsrvrdest-TARGET     in s_DEST
             or ls_rfcsrvrdest-LOCAL_DEST in s_DEST
             or ls_rfcsrvrdest-REMOT_DEST in s_DEST.
        elseif S_DEST_SIGN = 'E'.
          check ls_rfcsrvrdest-TARGET     in s_DEST
            AND ls_rfcsrvrdest-LOCAL_DEST in s_DEST
            AND ls_rfcsrvrdest-REMOT_DEST in s_DEST.
        endif.

        move-CORRESPONDING ls_rfcsrvrdest to gs_result.
        PERFORM translate_tasktype using ls_rfcsrvrdest-tasktype.
        perform get_user_data
          using    ls_rfcsrvrdest-mandt ls_rfcsrvrdest-account
          changing gs_result-USTYP      gs_result-CLASS.

        append gs_result to gt_result.
      endloop.
      free lt_rfcsrvrdest.
    endif.

    " HTTP Server Destination
    if p_SVDH = 'X'.
      gs_result-recordtype = 'SVD'.

      clear lt_websd[].
      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          COMPONENT     = ls_DIRECTORY_KEYS-COMPONENT
          ASSIGNDSYS    = ls_DIRECTORY_KEYS-ASSIGNDSYS
          PERIODTYPE    = ls_DIRECTORY_KEYS-PERIODTYPE
          PERIODSTRT    = ls_DIRECTORY_KEYS-PERIODSTRT
*         SUMMARY_ONLY  = ' '
*         STORAGE_TYPE  = 'A'
          FACTOR        = division_factor
        TABLES
*         RFCCLNT       = lt_rfcclnt       "wo
*         RFCCLNTDEST   = lt_rfcclntdest   "wp
*         RFCSRVR       = lt_rfcsrvr       "wq
*         RFCSRVRDEST   = lt_rfcsrvrdest   "wr
*         WEBC          = lt_webc          " TYPE SWNCAGGWEBCLNT
*         WEBCD         = lt_webcd         " TYPE SWNCAGGWEBDEST
*         WEBS          = lt_webs          " TYPE SWNCAGGWEBCLNT
          WEBSD         = lt_websd         " TYPE SWNCAGGWEBDEST
        EXCEPTIONS
          NO_DATA_FOUND = 1
          OTHERS        = 2.
      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      loop at lt_websd into ls_websd.
        if     S_USER_SIGN = 'I'.
          check ls_websd-account    in s_USER.
        elseif S_USER_SIGN = 'E'.
          check ls_websd-account    in s_USER.
        endif.
        if     S_DEST_SIGN = 'I'.
          check ls_websd-DESTINATION in s_DEST.
        elseif S_DEST_SIGN = 'E'.
          check ls_websd-DESTINATION in s_DEST.
        endif.

        move-CORRESPONDING ls_websd to gs_result.
        gs_result-TARGET    = ls_websd-DESTINATION.
        gs_result-RECEIVE   = ls_websd-DATA_RECEIVE.
        gs_result-SEND      = ls_websd-DATA_SEND.
        gs_result-EXE_TIME  = ls_websd-EXECUTION_TI.
        gs_result-CALL_TIME = ls_websd-CALLTIME.
        PERFORM translate_tasktype using ls_websd-tasktype.
        perform translate_protocol USING gs_result-protocol.
        perform get_user_data
          using    ls_websd-mandt ls_websd-account
          changing gs_result-USTYP  gs_result-CLASS.

        if gs_result-path = '@H3@'. " Don't show this as an icon.
           clear gs_result-path.
        endif.

        append gs_result to gt_result.
      endloop.
      free lt_websd.
    endif.

  endloop. "lt_DIRECTORY_KEYS

* Output
  perform show_alv_GT_RESULT.

endform. " MAIN

*&---------------------------------------------------------------------*
*&      Form  translate_tasktype
*&---------------------------------------------------------------------*
form translate_tasktype
  USING tasktype LIKE sapwlpfnrm-tasktype.

  data: l_char(1) TYPE c.

* If you get a syntax error here replace INCLUDE SAPWL_DECODE_TTYPE.
* with INCLUDE SAPWLSTAD_TT at the beginning.
  PERFORM tt_convert_number_to_letter
    USING tasktype
          l_char.

  CASE l_char.
    WHEN '*'.    gs_result-tasktype = 'Total'(406).
    WHEN 'D'.    gs_result-tasktype = 'Dialog'(402).
    WHEN 'U'.    gs_result-tasktype = 'Update (V1)'(403).
    WHEN 'S'.    gs_result-tasktype = 'Spool'(407).
    WHEN '$'.    gs_result-tasktype = 'NBR'(415).
    WHEN 'B'.    gs_result-tasktype = 'Background'(405).
    WHEN 'E'.    gs_result-tasktype = 'Enqueue'(408).
    WHEN 'Y'.    gs_result-tasktype = 'Buffer Sync.'(409).
    WHEN 'A'.    gs_result-tasktype = 'Auto ABAP'(410).
    WHEN '7'.    gs_result-tasktype = 'Auto RFC'(416).
    WHEN '§'.    gs_result-tasktype = 'lrrfc'. "(417) paragraph sign
    WHEN '2'.    gs_result-tasktype = 'Update (V2)'(404).
    WHEN 'C'.    gs_result-tasktype = 'CPI-C'(411).
    WHEN 'R'.    gs_result-tasktype = 'RFC'(412).
    WHEN 'L'.    gs_result-tasktype = 'ALE'(413).
    WHEN 'H'.    gs_result-tasktype = 'http'(395).
    WHEN 'T'.    gs_result-tasktype = 'https'(396).
    WHEN 'N'.    gs_result-tasktype = 'nntp'(397).
    WHEN 'M'.    gs_result-tasktype = 'smtp'(398).
    WHEN 'F'.    gs_result-tasktype = 'ftp'(399).

    WHEN 'P'.    gs_result-tasktype = 'plugin'(394).
    WHEN 'G'.    gs_result-tasktype = 'auto task handler'(418).
    WHEN 'I'.    gs_result-tasktype = 'remote procedure call'(419).
    WHEN 'X'.    gs_result-tasktype = 'RFV within VMC'(420).
    WHEN 'K'.    gs_result-tasktype = 'DDLOG cleanup'(421).
    WHEN 'Z'.    gs_result-tasktype = 'delayed task handler call'(422).
    WHEN 'J'.    gs_result-tasktype = 'Auto JAVA'(423).
    WHEN 'O'.    gs_result-tasktype = 'LCOM (fast RFC)'(204).
    WHEN 'V'.    gs_result-tasktype = 'http/JSP'(424).
    WHEN 'W'.    gs_result-tasktype = 'https/JSP'(425).
    WHEN 'Q'.    gs_result-tasktype = 'Licence server'(426).
    WHEN '1'.    gs_result-tasktype = 'ESF sub records'(427).
    WHEN '8'.    gs_result-tasktype = 'WS-RFC'(390).
    WHEN '9'.    gs_result-tasktype = 'WS-http'(391).
    WHEN '?'.    gs_result-tasktype = 'unknown'(414).

    WHEN '4'.    gs_result-tasktype = 'BGRFC Scheduler'(428).
    WHEN '5'.    gs_result-tasktype = 'BGRFC Unit'(429).

    WHEN '6'.    gs_result-tasktype = 'msadm'. "(430)
    WHEN '0'.    gs_result-tasktype = 'sys_startup'. "(431)
    WHEN '#'.    gs_result-tasktype = 'apc'. "(432)
    WHEN '3'.    gs_result-tasktype = 'Auto CCMS'(433).

    WHEN OTHERS. gs_result-tasktype = 'unknown'(414).
  ENDCASE.

  concatenate l_char gs_result-tasktype
    into gs_result-tasktype SEPARATED BY SPACE.
endform.                    "translate_tasktype

*&---------------------------------------------------------------------*
*&      Form  translate_protocol
*&---------------------------------------------------------------------*
form translate_protocol
  USING protocol type string.

  case protocol.
    when 0. clear gs_result-protocol.
    when 1. gs_result-protocol = 'http'.
    when 2. gs_result-protocol = 'https'.
    when 4. gs_result-protocol = 'smtp'.
    when others.
            gs_result-protocol = protocol.
  endcase.
endform.

*&---------------------------------------------------------------------*
*&      Form  Get_function_group
*&---------------------------------------------------------------------*
Form get_function_group
  using    l_funcname "type RS38L-NAME
  changing l_group    type RS38L-AREA.

  " Only the top n most expensive RFC Client Calls are stored in the statistics record.
  " If there have been more than n calls, the remaining RFC client calls are summed up as *RFC_CLIENT_COLLECTOR*.
  " https://help.sap.com/docs/btp/technical-monitoring-cockpit-cloud-version/abap-statistics-records#rfc-client-calls
  if l_funcname = '*RFC_CLIENT_COLLECTOR*'.
    l_group = '<sum of additional calls>'(030).
    return.
  endif.

  data: l_func  type RS38L-NAME.
  l_func = l_funcname.
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      FUNCNAME           = l_func
    IMPORTING
      GROUP              = l_group
*     INCLUDE            =
*     NAMESPACE          =
*     STR_AREA           =
    EXCEPTIONS
      FUNCTION_NOT_EXIST = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    l_group = '<function is unknown>'(025).
  ENDIF.
endform.                    "Get_function_group

*&---------------------------------------------------------------------*
*&      Form  get_user_data
*&---------------------------------------------------------------------*
form get_user_data
          using    l_mandt  type usr02-mandt
                   l_userid type usr02-bname
          changing l_USTYP  type usr02-USTYP
                   l_CLASS  type usr02-CLASS.

  data: lt_S_RFC_USER_AUTH type table of USVALUES,
        ls_S_RFC_USER_AUTH type          USVALUES.

  data: lt_S_RFCACL_USER_AUTH type table of USVALUES,
        ls_S_RFCACL_USER_AUTH type          USVALUES.

  data: l_tabix      type sy-tabix,
        gs_user_auth type ts_user_auth,
        lS_FNAME     type ts_FNAME,
        lt_FNAME     type tt_FNAME.

  clear: l_USTYP, l_CLASS.
  check l_mandt is not initial and l_userid is not initial.

* Get user type and user group
  select single USTYP CLASS from usr02 client specified
    into (l_USTYP, l_CLASS)
    where mandt = l_mandt
      and bname = l_userid.

* Do we know this user already?
  read table gt_user_auth TRANSPORTING NO FIELDS
    with key mandt = l_mandt
             bname = l_userid.
  check sy-subrc ne 0.
  l_tabix = sy-tabix.

  clear gs_user_auth.
  gs_user_auth-mandt = l_mandt.
  gs_user_auth-bname = l_userid.
  gs_user_auth-USTYP = l_USTYP.
  gs_user_auth-CLASS = l_CLASS.

* Get authorizations for S_RFC
  clear lt_S_RFC_USER_AUTH[].
  CALL FUNCTION 'SUSR_USER_AUTH_FOR_OBJ_GET'
    EXPORTING
*     NEW_BUFFERING       = 3
      MANDANT             = l_mandt
      USER_NAME           = l_userid
      SEL_OBJECT          = 'S_RFC'
    TABLES
      VALUES              = lt_S_RFC_USER_AUTH
    EXCEPTIONS
      USER_NAME_NOT_EXIST = 1
      NOT_AUTHORIZED      = 2
      INTERNAL_ERROR      = 3
      OTHERS              = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  data: l_ACTVT_OK(1),
        l_RFC_TYPE(1).
  sort lt_S_RFC_USER_AUTH by OBJCT AUTH FIELD VON BIS.
  loop at lt_S_RFC_USER_AUTH into ls_S_RFC_USER_AUTH.
    at new auth.
      clear: l_ACTVT_OK, l_RFC_TYPE, lt_FNAME.
    endat.

    case ls_S_RFC_USER_AUTH-FIELD.

      when 'ACTVT'.
        if   ls_S_RFC_USER_AUTH-VON = '16'
          or ls_S_RFC_USER_AUTH-VON = '1*'
          or ls_S_RFC_USER_AUTH-VON = '*'.
          l_ACTVT_OK = 'X'.
        endif.

      when 'RFC_NAME'.
        ls_FNAME-FROM_VALUE = ls_S_RFC_USER_AUTH-VON.
        ls_FNAME-TO_VALUE   = ls_S_RFC_USER_AUTH-BIS.
        append ls_FNAME to lt_FNAME.

      when 'RFC_TYPE'.
        if     ls_S_RFC_USER_AUTH-VON = 'FUGR'.
          l_RFC_TYPE = 'G'.
        elseif ls_S_RFC_USER_AUTH-VON = 'FUNC'.
          l_RFC_TYPE = 'F'.
        elseif ls_S_RFC_USER_AUTH-VON = '*'.
          l_RFC_TYPE = '*'.
        endif.

    endcase.

    at end of auth.
      check l_ACTVT_OK = 'X'
        and l_RFC_TYPE is not initial.

      if     l_RFC_TYPE = 'F'.
        append lines of lt_fname to gs_user_auth-FUNC.
      elseif l_RFC_TYPE = 'G'.
        append lines of lt_fname to gs_user_auth-FUGR.
      elseif l_RFC_TYPE = '*'.
        append lines of lt_fname to gs_user_auth-FUNC.
        append lines of lt_fname to gs_user_auth-FUGR.
      endif.
    endat.
  endloop.

  if   gs_user_auth-FUNC is not initial
    or gs_user_auth-FUGR is not initial.
*   Reduce authorization data
    sort gs_user_auth-FUNC by from_value to_value.
    delete ADJACENT DUPLICATES FROM gs_user_auth-FUNC COMPARING ALL FIELDS.
    sort gs_user_auth-FUGR by from_value to_value.
    delete ADJACENT DUPLICATES FROM gs_user_auth-FUGR COMPARING ALL FIELDS.
  endif.

* Get authorizations for S_RFCACL
  clear lt_S_RFCACL_USER_AUTH[].
  CALL FUNCTION 'SUSR_USER_AUTH_FOR_OBJ_GET'
    EXPORTING
*     NEW_BUFFERING       = 3
      MANDANT             = l_mandt
      USER_NAME           = l_userid
      SEL_OBJECT          = 'S_RFCACL'
    TABLES
      VALUES              = lt_S_RFCACL_USER_AUTH
    EXCEPTIONS
      USER_NAME_NOT_EXIST = 1
      NOT_AUTHORIZED      = 2
      INTERNAL_ERROR      = 3
      OTHERS              = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  data: l_S_RFCACL   type string,
        l_AUTH       type string,
        l_VAL        type string,
        l_RFC_SYSID	 type RFCSSYSID,
        l_RFC_CLIENT type RFC_CLIENT,
        l_RFC_USER   type RFC_USER,
        l_RFC_EQUSER type RFC_EQUSER,
        l_RFC_TCODE  type RFC_TCODE,
        l_RFC_INFO   type RFC_INFO,
        l_ACTVT      type ACTIV_AUTH.
  sort lt_S_RFC_USER_AUTH by OBJCT AUTH FIELD VON BIS.
  loop at lt_S_RFCACL_USER_AUTH into ls_S_RFCACL_USER_AUTH.
    at new auth.
      clear: l_AUTH, l_RFC_SYSID, l_RFC_CLIENT, l_RFC_USER, l_RFC_EQUSER, l_RFC_TCODE, l_RFC_INFO, l_ACTVT.
    endat.

    if ls_S_RFCACL_USER_AUTH-BIS is initial.
      l_VAL = ls_S_RFCACL_USER_AUTH-VON.
    else.
      CONCATENATE
          ls_S_RFCACL_USER_AUTH-VON
          ls_S_RFCACL_USER_AUTH-BIS
        INTO l_VAL SEPARATED BY '-'.
    endif.

    case ls_S_RFCACL_USER_AUTH-FIELD.
      when 'RFC_SYSID'.  l_RFC_SYSID  = l_VAL.
      when 'RFC_CLIENT'. l_RFC_CLIENT = l_VAL.
      when 'RFC_USER'.   l_RFC_USER   = l_VAL.
      when 'RFC_EQUSER'. l_RFC_EQUSER = l_VAL.
      when 'RFC_TCODE'.  l_RFC_TCODE  = l_VAL.
      when 'RFC_INFO'.   l_RFC_INFO   = l_VAL.
      when 'ACTVT'.      l_ACTVT      = l_VAL.
    endcase.

    at end of auth.
      concatenate
          'ID' l_RFC_SYSID
          'CL' l_RFC_CLIENT
          'US' l_RFC_USER
          'EQ' l_RFC_EQUSER
          "l_RFC_TCODE
          "l_RFC_INFO
          l_ACTVT
        into l_AUTH SEPARATED BY space.

      if l_S_RFCACL is initial.
        l_S_RFCACL = l_AUTH.
      else.
        concatenate l_S_RFCACL l_AUTH
          INTO l_S_RFCACL SEPARATED BY ', '.
      endif.
    endat.
  endloop.
  gs_user_auth-S_RFCACL = l_S_RFCACL.

* Store  authorization data
  insert gs_user_auth into gt_user_auth index l_tabix.

endform.                    "get_user_data

form get_authorization
  using    l_mandt      type usr02-mandt
           l_userid     type usr02-bname
           l_FUNC_NAME  type RS38L-NAME
           l_GROUP      type RS38L-AREA
  changing l_auth       type ts_result-auth_S_RFC
           l_S_RFCACL   type ts_result-auth_S_RFCACL.

  data: ls_FNAME     type ts_FNAME,
        ls_FNAME2    type ts_FNAME,
        l_FNAME_STAR(1),
        l_icon(40).

  if l_GROUP = 'SRFC'.
*   check profile parameter auth/rfc_authority_check first
    data: l_rfc_authority_check.
    CALL 'C_SAPGPARAM'
      ID 'NAME' FIELD 'auth/rfc_authority_check'
      ID 'VALUE' FIELD l_rfc_authority_check.
  case l_rfc_authority_check.
    when '0' or '1' or '2' or '3' or '5' or '8'.
        l_auth = 'implicit (SRFC)'.
        return.
    when '4'.
      if l_FUNC_NAME = 'RFC_PING' or l_FUNC_NAME = 'RFC_SYSTEM_INFO'.
        l_auth = 'implicit (SRFC)'.
        return.
      endif.
    when '6'.
      if l_FUNC_NAME = 'RFC_PING'.
        l_auth = 'implicit (SRFC)'.
        return.
      endif.
    when others. "'9'
  endcase.
  endif.

  clear: l_auth.
  read table gt_user_auth into gs_user_auth
    with key mandt = l_mandt
             bname = l_userid.
  check sy-subrc = 0.

  clear l_FNAME_STAR.
* Search authorizations for function group
  loop at gs_user_auth-FUGR into ls_FNAME.
    if ls_FNAME-TO_VALUE is initial.

      if ls_FNAME-FROM_VALUE = '*'.
        l_FNAME_STAR = '*'.
      elseif l_GROUP CP ls_FNAME-FROM_VALUE.
        if l_auth is initial.
          l_auth = ls_FNAME-FROM_VALUE.
        else.
          concatenate l_auth ls_FNAME-FROM_VALUE
            into l_auth SEPARATED BY SPACE.
        endif.
      endif.

    else. " with to_value

      ls_FNAME2 = ls_FNAME.
      replace '*' in ls_FNAME2-FROM_VALUE with ' ' in CHARACTER MODE.
      replace '*' in ls_FNAME2-TO_VALUE   with ' ' in CHARACTER MODE.
      if    l_GROUP >= ls_FNAME2-FROM_VALUE
        and l_GROUP <= ls_FNAME2-TO_VALUE.
        if l_auth is initial.
          concatenate        '[' ls_FNAME-FROM_VALUE ls_FNAME-TO_VALUE ']'
            into l_auth SEPARATED BY SPACE.
        else.
          concatenate l_auth '[' ls_FNAME-FROM_VALUE ls_FNAME-TO_VALUE ']'
            into l_auth SEPARATED BY SPACE.
        endif.
      endif.

    endif.
  endloop.

* Search authorizations for function
  loop at gs_user_auth-FUNC into ls_FNAME.
    if ls_FNAME-TO_VALUE is initial.

      if ls_FNAME-FROM_VALUE = '*'.
        l_FNAME_STAR = '*'.
      elseif l_FUNC_NAME CP ls_FNAME-FROM_VALUE.
        if l_auth is initial.
          l_auth = ls_FNAME-FROM_VALUE.
        else.
          concatenate l_auth ls_FNAME-FROM_VALUE
            into l_auth SEPARATED BY SPACE.
        endif.
      endif.

    else. " with to_value

      ls_FNAME2 = ls_FNAME.
      replace '*' in ls_FNAME2-FROM_VALUE with ' ' in CHARACTER MODE.
      replace '*' in ls_FNAME2-TO_VALUE   with ' ' in CHARACTER MODE.
      if    l_FUNC_NAME >= ls_FNAME2-FROM_VALUE
        and l_FUNC_NAME <= ls_FNAME2-TO_VALUE.
        if l_auth is initial.
          concatenate        '[' ls_FNAME-FROM_VALUE ls_FNAME-TO_VALUE ']'
            into l_auth SEPARATED BY SPACE.
        else.
          concatenate l_auth '[' ls_FNAME-FROM_VALUE ls_FNAME-TO_VALUE ']'
            into l_auth SEPARATED BY SPACE.
        endif.
      endif.

    endif.
  endloop.

* Full authorizations
  if l_FNAME_STAR = '*'.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        NAME                        = 'ICON_MESSAGE_CRITICAL_SMALL'
* Other icons: ICON_MESSAGE_ERROR ICON_MESSAGE_CRITICAL
* ICON_MESSAGE_ERROR_SMALL ICON_MESSAGE_CRITICAL_SMALL
* ICON_LED_RED ICON_BREAKPOINT ICON_ALERT ICON_DEFECT
* ICON_INCOMPLETE
*       TEXT                        = ' '
        INFO                        = 'Full authorizations'
        ADD_STDINF                  = ' '
      IMPORTING
        RESULT                      = l_icon
      EXCEPTIONS
        ICON_NOT_FOUND              = 1
        OUTPUTFIELD_TOO_SHORT       = 2
        OTHERS                      = 3
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

    concatenate l_icon l_FNAME_STAR l_auth into l_auth
      SEPARATED BY SPACE.

* Missing authorizations
  elseif l_auth is initial.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        NAME                  = 'ICON_FAILURE'
*       TEXT                  = ' '
        INFO                  = 'Missing authorizations'
        ADD_STDINF            = ' '
      IMPORTING
        RESULT                = l_auth
      EXCEPTIONS
        ICON_NOT_FOUND        = 1
        OUTPUTFIELD_TOO_SHORT = 2
        OTHERS                = 3.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  endif.

  l_S_RFCACL = gs_user_auth-S_RFCACL.

endform.

*&---------------------------------------------------------------------*
*&      Form  get_destination_data
*&---------------------------------------------------------------------*

form get_destination_data
          using    l_rfcdest      "type rfcdes-rfcdest
          changing ls_RFCOPTIONS  type ts_RFCOPTIONS.

  data: ls_RFCDES     type RFCDES,
        ls_RFCDISPLAY type RFCDISPLAY.

  statics: lt_DD07V type table of DD07V.
  data:    ls_DD07V type DD07V.

  check l_rfcdest is not initial.

* Get texts of RFC type in DOMVALUE_L into DOMVALUE_H
* (not used yet)
  if lt_DD07V[] is initial.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        DOMNAME              = 'RFCTYPE'
        TEXT                 = 'X'
*     LANGU                = ' '
*     BYPASS_BUFFER        = ' '
*   IMPORTING
*     RC                   =
      TABLES
        DD07V_TAB            = lt_DD07V
      EXCEPTIONS
        WRONG_TEXTFLAG       = 1
        OTHERS               = 2
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  clear: ls_RFCOPTIONS.

* retrieve RFC destination settings:
  select single * from rfcdes into ls_rfcdes
         where rfcdest = l_rfcdest.

* Transport blob options
  CALL FUNCTION 'RFCDES2RFCDISPLAY'
    EXPORTING
      IMPORT_RFCDES           = ls_RFCDES
*     AUTHORITY_CHECK         = ' ' " Checks S_RFC_ADM activity 03
    IMPORTING
      EXPORT_RFCDISPLAY       = ls_RFCDISPLAY
    EXCEPTIONS
      FORMAT_ERROR            = 1
      AUTHORITY_NOT_AVAILABLE = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  if ls_RFCDISPLAY-RFCTYPE = 'T'.
    case ls_RFCDISPLAY-RFCHOST.
      when '%%RFCSERVER%%'. "external RFC Server
        CONCATENATE 'registerd RFC server:' ls_RFCDISPLAY-PFADPRE
          into ls_RFCDISPLAY-RFCHOST SEPARATED BY space.
        clear ls_RFCDISPLAY-PFADPRE.
      when '%%SAPGUI%%'.
        CONCATENATE 'SAPGUI:' ls_RFCDISPLAY-PFADPRE
          into ls_RFCDISPLAY-RFCHOST SEPARATED BY space.
        clear ls_RFCDISPLAY-PFADPRE.
      when space.
        ls_RFCDISPLAY-RFCHOST = ls_RFCDISPLAY-PFADPRE.
        clear ls_RFCDISPLAY-PFADPRE.
    endcase.
  elseif ls_RFCDISPLAY-RFCTYPE = 'I'.
    clear: ls_RFCDISPLAY-RFCHOST, ls_RFCDISPLAY-RFCSERVICE.
  endif.

  if ls_RFCDISPLAY-RFCUSER = '%_LOG01%'.     "Empty user field
    clear ls_RFCDISPLAY-RFCUSER.
  endif.

  if ls_RFCDISPLAY-RFCAUTH is not initial.   "Just to be sure..
    ls_RFCDISPLAY-RFCAUTH = '********'.
  endif.

  if ls_RFCDISPLAY-RFCSAMEUSR = 'X'.
    ls_RFCDISPLAY-RFCUSER = '<same user>'(002).
  endif.

  if ls_RFCDISPLAY-RFCSNC is initial.
    clear ls_RFCDISPLAY-SSLAPPLIC. " Don't show the PSE name if TLS is not active.
  endif.

  MOVE-CORRESPONDING ls_RFCDISPLAY to ls_RFCOPTIONS.

  if ls_RFCDISPLAY-RFCUSER is initial and ls_RFCDISPLAY-RFCALIAS+12 is initial.
    ls_RFCOPTIONS-RFCUSER = ls_RFCDISPLAY-RFCALIAS.
  endif.

  read table lt_DD07V into ls_DD07V
    with key DOMVALUE_L = ls_RFCDISPLAY-RFCTYPE.
  if sy-subrc = 0.
    concatenate ls_RFCDISPLAY-RFCTYPE ls_DD07V-DDTEXT
      into ls_RFCOPTIONS-RFCTYPE SEPARATED BY space.
  endif.

* Check Table RFCTRUST List of existing trusting systems: Systems who trust current system (to be used for CL/CLD)
  "data: ls_RFCTRUST type RFCTRUST.
  "select single * from RFCTRUST into ls_RFCTRUST
  "  where RFCDEST = l_rfcdest.
  "if sy-subrc = 0.
  "  concatenate ls_RFCTRUST-RFCTRUSTID ls_RFCTRUST-TLICENSE_NR ls_RFCTRUST-RFCMSGSRV
  "    into ls_RFCOPTIONS-RFCTRUSTID_EXT separated by space.
  "else. " no
  "  clear ls_RFCOPTIONS-RFCTRUSTID_EXT.
  "endif.

endform.                    "get_destination_data

*=====================================================================*
*  Show ALV
*=====================================================================*
* see demo Reports SALV_DEMO*
*---------------------------------------------------------------------*
* ALV

* Definition is later
class lcl_handle_events definition deferred.

* main data table
data: gr_ALV_TABLE_gt_RESULT  type ref to cl_salv_table.

* for handling the events of cl_salv_table
data: gr_ALV_EVENTS_gt_RESULT type ref to lcl_handle_events.


*---------------------------------------------------------------------*
*      CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
class lcl_handle_events definition.

  public section.

    methods:
      on_user_command for event added_function of cl_salv_events
        importing e_salv_function,

      on_double_click for event double_click of cl_salv_events_table
        importing row column.

*      on_single_click for event link_click of cl_salv_events_table
*        importing row column.
endclass.                    "lcl_handle_events DEFINITION

*----------------------------------------------------------------------*
*      CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
* implement the events for handling the events of cl_salv_table
*----------------------------------------------------------------------*
class lcl_handle_events implementation.

  method on_user_command.
*    importing e_salv_function

    DATA: lr_Selections  type ref to cl_salv_selections,
          ls_cell        type  salv_s_cell,
          lt_rows        type  salv_t_row,
          l_row          type  i.

*   Get selected item
    lr_selections = gr_ALV_TABLE_gt_RESULT->get_selections( ).
    ls_cell = lr_selections->get_current_cell( ).
    lt_rows = lr_selections->get_selected_rows( ).

    CASE e_salv_function.

      WHEN 'PICK'.
        READ TABLE gt_result INTO gs_result INDEX ls_cell-row.
        CHECK sy-subrc = 0.

*       Call ABAP Workbench (with all required authorization checks)
        check gs_result-prog_name IS NOT INITIAL.
        editor-call for report gs_result-prog_name DISPLAY-MODE.

    ENDCASE.

  endmethod.                    "on_user_command

  method on_double_click.
*   importing row column

    DATA: lr_Selections  type ref to cl_Salv_selections,
          ls_cell        type        salv_s_cell.

    data: lt_sel_FUGR type RANGE OF ts_USER-FUGR,
          ls_sel_FUGR like LINE OF lt_sel_FUGR,
          lt_sel_FUNC type RANGE OF ts_USER-FUNC,
          ls_sel_FUNC like LINE OF lt_sel_FUNC.

*   Get selected item
    lr_selections = gr_ALV_TABLE_gt_RESULT->get_selections( ).
    ls_cell = lr_selections->get_current_cell( ).

    READ TABLE gt_result INTO gs_result INDEX row.
    check sy-subrc = 0.

    case ls_cell-COLUMNNAME.

      when 'MANDT' or 'ACCOUNT'.
        read table gt_user_auth into gs_user_auth
          with key mandt = gs_result-mandt
                   bname = gs_result-account.
        check sy-subrc = 0 and gs_result-RECORDTYPE = 'SV'.

        clear: gt_USER.
        gs_USER-mandt = gs_user_auth-mandt.
        gs_USER-bname = gs_user_auth-bname.
        gs_USER-USTYP = gs_user_auth-USTYP.
        gs_USER-CLASS = gs_user_auth-CLASS.

        clear: gs_USER-FUGR, gs_USER-FUNC.
        loop at gs_user_auth-FUGR into gs_USER-FUGR.
          clear: lt_sel_FUGR, ls_sel_FUGR.
          if gs_USER-FUGR ca '*'.
            ls_sel_FUGR-option = 'CP'.
          else.
            ls_sel_FUGR-option = 'EQ'.
          endif.
          ls_sel_FUGR-sign   = 'I'.
          ls_sel_FUGR-low    = gs_USER-FUGR.
          append ls_sel_FUGR to lt_sel_FUGR.
          clear gs_USER-COUNTER.
          loop at GT_RESULT into gs_result
            where RECORDTYPE = 'SV'
              and mandt      = gs_user_auth-mandt
              and account    = gs_user_auth-bname
              and GROUP      in lt_sel_FUGR.

            add gs_result-counter to gs_USER-counter.
          endloop.
          append gs_USER to gt_USER.
        endloop.

        clear: gs_USER-FUGR, gs_USER-FUNC.
        loop at gs_user_auth-FUNC into gs_USER-FUNC.
          clear: lt_sel_FUNC, ls_sel_FUNC.
          if gs_USER-FUNC ca '*'.
            ls_sel_FUNC-option = 'CP'.
          else.
            ls_sel_FUNC-option = 'EQ'.
          endif.
          ls_sel_FUNC-sign   = 'I'.
          ls_sel_FUNC-low    = gs_USER-FUNC.
          append ls_sel_FUNC to lt_sel_FUNC.
          clear gs_USER-counter.
          loop at GT_RESULT into gs_result
            where RECORDTYPE = 'SV'
              and mandt      =  gs_user_auth-mandt
              and account    =  gs_user_auth-bname
              and FUNC_NAME  in lt_sel_FUNC.

            add gs_result-counter to gs_USER-counter.
          endloop.
          append gs_USER to gt_USER.
        endloop.

        perform show_alv_GT_USER.

      when 'PROG_NAME'.
*       Call ABAP Workbench (with all required authorization checks)
        check gs_result-prog_name IS NOT INITIAL.
        editor-call for report gs_result-prog_name DISPLAY-MODE.

      when 'TARGET'.
        check gs_result-RECORDTYPE = 'CL'
           or gs_result-RECORDTYPE = 'CLD'.
        check gs_result-TARGET is not initial and gs_result-TARGET ne 'NONE'.
        data: ls_rfcdes      TYPE rfcdes.
        SELECT SINGLE * FROM rfcdes INTO ls_rfcdes
          WHERE rfcdest = gs_result-TARGET.
        if  sy-subrc ne 0.
          message e069(sr) with gs_result-TARGET. " Destination & no longer exists
          exit.
        endif.
        CALL FUNCTION 'RFCDES_DISPLAY'
          EXPORTING
            rfcdes_export           = ls_rfcdes
            authority_check         = abap_true
            editable                = abap_true
*          IMPORTING
*            modified                = ld_chg_flg
*            rfcdes_import           = ls_rfcdes
          EXCEPTIONS
            authority_not_available = 1
            OTHERS                  = 2.

    endcase.

  endmethod.                    "on_double_click

endclass.                    "lcl_handle_events IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  show_alv_GT_RESULT
*&---------------------------------------------------------------------*
form show_alv_GT_RESULT.

* reference to ALV objects
  data: lr_functions_list      type ref to cl_salv_functions_list,
*        lr_functions           TYPE REF TO cl_salv_functions,
        lr_selections          type ref to cl_salv_selections,
        lr_columns             type ref to cl_salv_columns_table,
*        lr_column              TYPE REF TO cl_salv_column_table,
*        lr_sorts               TYPE REF TO cl_salv_sorts.
        lr_events              type ref to cl_salv_events_table,
        lr_functional_settings TYPE REF TO cl_salv_functional_settings,
        lr_hyperlinks          type ref to cl_salv_hyperlinks,
        lr_tooltips            TYPE REF TO cl_salv_tooltips,
        lr_layout              TYPE REF TO cl_salv_layout,
        ls_key                 TYPE salv_s_layout_key,
*        lr_content             TYPE REF TO cl_salv_form_element,

        lr_display_settings    TYPE REF TO cl_salv_display_settings.

  DATA: lr_exception           TYPE REF TO cx_salv_error,
        lv_message             type bal_s_msg.

* Create an ALV table for grid display
  try.
      cl_salv_table=>factory(
        importing
          r_salv_table = gr_ALV_TABLE_gt_RESULT
        changing
          t_table      = gt_RESULT ).

    CATCH cx_salv_msg
          INTO lr_exception.
      lv_message = lr_exception->get_message( ).
      MESSAGE ID lv_message-msgid TYPE lv_message-msgty
              NUMBER lv_message-msgno
              WITH lv_message-msgv1 lv_message-msgv2
                   lv_message-msgv3 lv_message-msgv4.
  endtry.

*  set GUI status including own function PICK and DL
*  gr_ALV_TABLE->set_screen_status(
*    pfstatus      =  'LIST_ALV'
*    report        =  l_repid
*    set_functions = gr_ALV_TABLE->c_functions_all ).

* set ALV generic funtions of class CL_SALV_FUNCTIONS_LIST
  lr_functions_list = gr_ALV_TABLE_gt_RESULT->get_functions( ).
*  lr_functions->SET_DEFAULT(            if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_DETAIL(        if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_GROUP_AGGREGATION(  if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_GROUP_EXPORT(  if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_GROUP_FILTER(  if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_GROUP_LAYOUT(  if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_PRINT(         if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_PRINT_PREVIEW( if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_GROUP_SORT(    if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_FIND(          if_salv_c_bool_sap=>true ).
  lr_functions_list->SET_GRAPHICS(      if_salv_c_bool_sap=>false ).

* Set the layout
  lr_layout = gr_ALV_TABLE_gt_RESULT->get_layout( ) .
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
  lr_columns = gr_ALV_TABLE_gt_RESULT->get_columns( ).
  lr_columns->set_optimize( if_salv_c_bool_sap=>true ).

* Set the fields description and field attributes
  perform set_columns_text_GT_RESULT using lr_columns.

* Activate total sums
  data: lr_aggregations type ref to cl_salv_aggregations.
  lr_aggregations = gr_ALV_TABLE_gt_RESULT->get_aggregations( ).
  lr_aggregations->clear( ).
  try.
      lr_aggregations->add_aggregation( columnname = 'COUNTER' ). " default:    aggregation = if_salv_c_aggregation=>total
      lr_aggregations->add_aggregation( columnname = 'CALLS' ).
      lr_aggregations->add_aggregation( columnname = 'RECEIVE' ).
      lr_aggregations->add_aggregation( columnname = 'SEND' ).
      lr_aggregations->add_aggregation( columnname = 'EXE_TIME' ).
      lr_aggregations->add_aggregation( columnname = 'CALL_TIME' ).
    catch cx_salv_not_found cx_salv_data_error cx_salv_existing.
  endtry.

* register to the events of cl_salv_table
  lr_events = gr_ALV_TABLE_gt_RESULT->get_event( ).
  CREATE OBJECT gr_ALV_EVENTS_gt_RESULT.
* register to the event USER_COMMAND
  set handler gr_ALV_EVENTS_gt_RESULT->on_user_command for lr_events.
* register to the event DOUBLE_CLICK
  set handler gr_ALV_EVENTS_gt_RESULT->on_double_click for lr_events.

* set selection mode
  lr_selections = gr_ALV_TABLE_gt_RESULT->get_selections( ).
  lr_selections->set_selection_mode(
  if_salv_c_selection_mode=>row_column ).

* Set Title
  lr_display_settings = gr_ALV_TABLE_gt_RESULT->get_display_settings( ).
  lr_display_settings->set_list_header( 'RFC Statistics' ).
  lr_display_settings->SET_LIST_HEADER_SIZE(
    cl_salv_display_settings=>c_header_size_large ).

  perform set_title_GT_RESULT.

* display the table
  gr_ALV_TABLE_gt_RESULT->display( ).

endform. "show_alv_GT_RESULT


*&---------------------------------------------------------------------*
*&      Form  set_columns_text_GT_RESULT
*&---------------------------------------------------------------------*
FORM set_columns_text_GT_RESULT
  using ir_columns type ref to cl_salv_columns_table.

  data: lr_column      type ref to cl_salv_column_list,
        lr_sorts       TYPE REF TO cl_salv_sorts.

  data: lr_exception   TYPE REF TO cx_salv_error,
        lv_message     type bal_s_msg.

  data: LS_COLOR_ACCOUNT  TYPE LVC_S_COLO,
        LS_COLOR_FUNCTION TYPE LVC_S_COLO.

  ls_COLOR_ACCOUNT-col   = col_KEY.
  ls_COLOR_FUNCTION-col  = col_TOTAL.

  try.

      lr_column ?= ir_columns->get_column( 'RECORDTYPE' ).
      lr_column->set_short_text(  'RecordType'(s01) ).
      lr_column->set_medium_text( 'Record type'(m01) ).
      lr_column->set_long_text(   'Record type'(l01) ).

      lr_column ?= ir_columns->get_column( 'DATE' ).
      lr_column->set_short_text(  'Date'(s02) ).
      lr_column->set_medium_text( 'Date'(m02) ).
      lr_column->set_long_text(   'Date'(l02) ).

      lr_column ?= ir_columns->get_column( 'TASKTYPE' ).
      lr_column->set_short_text(  'Task type'(s03) ).
      lr_column->set_medium_text( 'Task type'(m03) ).
      lr_column->set_long_text(   'Task type'(l03) ).

      lr_column ?= ir_columns->get_column( 'MANDT' ). "USR02-MANDT
      lr_column->set_short_text(  'Client'(s04) ).
      lr_column->set_medium_text( 'Client'(m04) ).
      lr_column->set_long_text(   'Client in local system'(l04) ).
      lr_column->SET_COLOR( LS_COLOR_ACCOUNT ).

      lr_column ?= ir_columns->get_column( 'ACCOUNT' ). "USR02-BNAME
      lr_column->set_short_text(  'Account'(s05) ).
      lr_column->set_medium_text( 'Account'(m05) ).
      lr_column->set_long_text(   'Account in local system'(l05) ).
      lr_column->SET_COLOR( LS_COLOR_ACCOUNT ).

      lr_column ?= ir_columns->get_column( 'USTYP' ). "USR02-USTYP
      lr_column->set_short_text(  'User type'(s06) ).
      lr_column->set_medium_text( 'User type'(m06) ).
      lr_column->set_long_text(   'User type of account in local system'(l06) ).
      lr_column->SET_COLOR( LS_COLOR_ACCOUNT ).
      if p_CL is initial and p_CLH is initial and p_SV is initial and p_SVH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'CLASS' ). "USR02-CLASS
      lr_column->set_short_text(  'User group'(s07) ).
      lr_column->set_medium_text( 'User group'(m07) ).
      lr_column->set_long_text(   'User group of account in local system'(l07) ).
      lr_column->SET_COLOR( LS_COLOR_ACCOUNT ).
      if p_CL is initial and p_CLH is initial and p_SV is initial and p_SVH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'USERID' ). "USR02-BNAME
      lr_column->set_short_text(  'Userid'(s08) ).
      lr_column->set_medium_text( 'Userid'(m08) ).
      lr_column->set_long_text(   'Userid'(l08) ).
      if p_CL is initial and p_SV is initial. " Empty for CLD, CLDH, SVDH
        lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ).
        "lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFC_CALLER' ). "USR02-BNAME
      lr_column->set_short_text(  'RFC Caller'(s09) ).
      lr_column->set_medium_text( 'RFC Caller'(m09) ).
      lr_column->set_long_text(   'RFC Caller'(l09) ).
      if p_SV is initial and p_SVD is initial. " Empty for CL, CLH, SV?, SVH, CLD, CLDH, SVDH
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'TARGET' ). "RFCDES-RFCDEST
      lr_column->set_short_text(  'Target'(s10) ).
      lr_column->set_medium_text( 'Target'(m10) ).
      lr_column->set_long_text(   'Target'(l10) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial  and p_CLDH is initial and p_SV is initial and p_SVD is initial. " Empty for SVH, SVDH
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'LOCAL_DEST' ).
      lr_column->set_short_text(  'Local Inst'(s11) ).
      lr_column->set_medium_text( 'Local instance'(m11) ).
      lr_column->set_long_text(   'Local instance'(l11) ).
      lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ). " Empty in CLH, SVH, CLDH, SVDH

      lr_column ?= ir_columns->get_column( 'REMOT_DEST' ).
      lr_column->set_short_text(  'RemoteInst'(s12) ).
      lr_column->set_medium_text( 'Remote instance'(m12) ).
      lr_column->set_long_text(   'Remote instance'(l12) ).
      if p_CL is initial and p_CLD is initial and p_SV is initial and p_SVD is initial. " Empty for CLH, SVH, CLDH, SVDH
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'FUNC_NAME' ). "RS38L-NAME
      lr_column->set_short_text(  'Function'(s13) ).
      lr_column->set_medium_text( 'RFC Function'(m13) ).
      lr_column->set_long_text(   'Function (started over RFC)'(l13) ).
      lr_column->SET_COLOR( LS_COLOR_FUNCTION ).
      if p_CL is initial and p_SV is initial. " Empty for CLH, SVH, CLD, SVD, CLDH, SVDH
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'GROUP' ). "RS38L-AREA
      lr_column->set_short_text(  'Group'(s14) ).
      lr_column->set_medium_text( 'Function group'(m14) ).
      lr_column->set_long_text(   'Function group of RFC function'(l14) ).
      lr_column->SET_COLOR( LS_COLOR_FUNCTION ).
      if p_CL is initial and p_SV is initial. " Empty for CLH, SVH, CLD, CLDH, SVD, SVDH
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'AUTH_S_RFC' ).
      lr_column->set_short_text(  'S_RFC'(s15) ).
      lr_column->set_medium_text( 'Auth. for S_RFC'(m15) ).
      lr_column->set_long_text(   'Authorizations for S_RFC'(l15) ).
      if p_SV is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'AUTH_S_RFCACL' ).
      lr_column->set_short_text(  'S_RFCACL'(s37) ).
      lr_column->set_medium_text( 'Auth. for S_RFCACL'(m37) ).
      lr_column->set_long_text(   'Authorizations for S_RFCACL'(l37) ).
      lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ). " not fully used yet
      if p_SV is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'PROG_NAME' ). "RS38L-PROGNAME
      lr_column->set_short_text(  'Program'(s16) ).
      lr_column->set_medium_text( 'Program name'(m16) ).
      lr_column->set_long_text(   'Name of calling program'(l16) ).
      if p_CL is initial. " Empty for CLH, SV, SVH, SVD, CLDH, SVD, SVDH
        lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ).
        "lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'PROTOCOL' ).
      lr_column->set_short_text(  'Protocol'(s38) ).
      lr_column->set_medium_text( 'Protocol'(m38) ).
      lr_column->set_long_text(   'Protocol'(l38) ).
      if p_CLH is initial and p_CLDH is initial and p_SVH is initial and p_SVDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'HOST' ).
      lr_column->set_short_text(  'Host'(s39) ).
      lr_column->set_medium_text( 'Host'(m39) ).
      lr_column->set_long_text(   'Host'(l39) ).
      if p_CLH is initial and p_CLDH is initial and p_SVH is initial and p_SVDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'PORT' ).
      lr_column->set_short_text(  'Port'(s40) ).
      lr_column->set_medium_text( 'Port'(m40) ).
      lr_column->set_long_text(   'Port'(l40) ).
      if p_CLH is initial and p_CLDH is initial and p_SVH is initial and p_SVDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'PATH' ).
      lr_column->set_short_text(  'Path'(s41) ).
      lr_column->set_medium_text( 'Path'(m41) ).
      lr_column->set_long_text(   'Path'(l41) ).
      lr_column->set_icon( if_salv_c_bool_sap=>false ). " Does not work ?
      if p_CLH is initial and p_CLDH is initial and p_SVH is initial. " Empty for SVDH
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'ENTRY_ID' ).
      lr_column->set_short_text(  'Entry ID'(s17) ).
      lr_column->set_medium_text( 'Entry ID'(m17) ).
      lr_column->set_long_text(   'Entry ID'(l17) ).
      lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ).

      lr_column ?= ir_columns->get_column( 'COUNTER' ).
      lr_column->set_short_text(  '# Calls'(s18) ).
      lr_column->set_medium_text( 'No of calls'(m18) ).
      lr_column->set_long_text(   'Number of RFC calls'(l18) ).

      lr_column ?= ir_columns->get_column( 'RECEIVE' ).
      lr_column->set_short_text(  'Receive'(s19) ).
      lr_column->set_medium_text( 'Received data'(m19) ).
      lr_column->set_long_text(   'Received data (Bytes)'(l19) ).
      lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ).

      lr_column ?= ir_columns->get_column( 'SEND' ).
      lr_column->set_short_text(  'Send'(s20) ).
      lr_column->set_medium_text( 'Send data'(m20) ).
      lr_column->set_long_text(   'Send data (Bytes)'(l20) ).
      lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ).

      lr_column ?= ir_columns->get_column( 'EXE_TIME' ).
      lr_column->set_short_text(  'Exe.Time'(s21) ).
      lr_column->set_medium_text( 'Execution time (s)'(m21) ).
      lr_column->set_long_text(   'Execution time (s)'(l21) ).
      lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ).

      lr_column ?= ir_columns->get_column( 'CALL_TIME' ).
      lr_column->set_short_text(  'Call time'(s22) ).
      lr_column->set_medium_text( 'Call time (s)'(m22) ).
      lr_column->set_long_text(   'Call Time (s)'(l22) ).
      lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ).

      lr_column ?= ir_columns->get_column( 'CALLS' ).
      lr_column->set_short_text(  '# Records'(s23) ).
      lr_column->set_medium_text( 'Number of recods'(m23) ).
      lr_column->set_long_text(   'Number of processed records'(l23) ).
      lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ).
      if p_CLD is initial and p_CLDH is initial and p_SVD is initial. " Empty in SVDH
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCTYPE' ). "RFCDISPLAY-RFCTYPE
      lr_column->set_short_text(  'Dest.Type'(s24) ).
      lr_column->set_medium_text( 'Destination Type'(m24) ).
      lr_column->set_long_text(   'Destination Type'(l24) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCHOST' ).
      lr_column->set_short_text(  'Host'(s25) ).
      lr_column->set_medium_text( 'Host'(m25) ).
      lr_column->set_long_text(   'Host (of destination)'(l25) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCSERVICE' ).
      lr_column->set_short_text(  'Service'(s26) ).
      lr_column->set_medium_text( 'Service'(m26) ).
      lr_column->set_long_text(   'Service'(l26) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial. " Empty for CLDH
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCSYSID' ).
      lr_column->set_short_text(  'SysID'(s27) ).
      lr_column->set_medium_text( 'System ID'(m27) ).
      lr_column->set_long_text(   'System ID'(l27) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-PFADPRE' ).
      lr_column->set_short_text(  'Path'(s28) ).
      lr_column->set_medium_text( 'Path'(m28) ).
      lr_column->set_long_text(   'Path (of destination)'(l28) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCCLIENT' ).
      lr_column->set_short_text(  'Client'(s29) ).
      lr_column->set_medium_text( 'Client'(m29) ).
      lr_column->set_long_text(   'Client'(l29) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCUSER' ).
      lr_column->set_short_text(  'RemoteUser'(s30) ).
      lr_column->set_medium_text( 'Remote User'(m30) ).
      lr_column->set_long_text(   'Remote User'(l30) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCAUTH' ).
      lr_column->set_short_text(  'Password'(s31) ).
      lr_column->set_medium_text( 'Remote Password'(m31) ).
      lr_column->set_long_text(   'Remote Password'(l31) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCSLOGIN' ).
      lr_column->set_short_text(  'Logon Proc'(s34) ).
      lr_column->set_medium_text( 'Logon Procedure'(m34) ).
      lr_column->set_long_text(   'Logon Procedure (trusted, basic, no)'(l34) ).
      lr_column->SET_TOOLTIP( 'Y=Trusted RFC, B=Basic auth., A=No user'(t34) ). " 40 char
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCSAMEUSR' ).
      lr_column->set_short_text(  'Same user'(s35) ).
      lr_column->set_medium_text( 'TrustedRFC same user'(m35) ).
      lr_column->set_long_text(   'Trusted-RFC with same user'(l35) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      "lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCTRUSTID_EXT' ).
      "lr_column->set_short_text(  'RFCTrustID'(s36) ).
      "lr_column->set_medium_text( 'Trusting system ID'(m36) ).
      "lr_column->set_long_text(   'Trusting system ID'(l36) ).
      "if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
      "  lr_column->set_technical( if_salv_c_bool_sap=>true ).
      "endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-RFCSNC' ).
      lr_column->set_short_text(  'SNC/TLS'(s42) ).
      lr_column->set_medium_text( 'SNC/TLS'(m42) ).
      lr_column->set_long_text(   'SNC/TLS'(l42) ).
      if p_CL is initial and p_CLH is initial and p_CLD is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

      lr_column ?= ir_columns->get_column( 'RFCOPTIONS-SSLAPPLIC' ).
      lr_column->set_short_text(  'PSE'(s43) ).
      lr_column->set_medium_text( 'SSL Applic. (PSE)'(m43) ).
      lr_column->set_long_text(   'SSL Application (PSE)'(l43) ).
      if p_CLH is initial and p_CLDH is initial.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      endif.

    CATCH cx_salv_not_found
      into lr_exception.
      lv_message = lr_exception->get_message( ).
      MESSAGE ID lv_message-msgid TYPE lv_message-msgty
              NUMBER lv_message-msgno
              WITH lv_message-msgv1 lv_message-msgv2
                   lv_message-msgv3 lv_message-msgv4.
  ENDTRY.

endform. "set_columns_text_GT_RESULT

*&---------------------------------------------------------------------*
*&      Form  set_title_GT_RESULT
*&---------------------------------------------------------------------*
form set_title_GT_RESULT.

  data: lr_grid TYPE REF TO cl_salv_form_layout_grid.

  data: l_line type i,
        l_text(80).

* Header

  CREATE OBJECT lr_grid.
  l_line = 1.

  lr_grid->create_label(
         row    = l_line
         column = 1
         text   = 'System'(003) ).
  CONCATENATE sy-sysid 'on'(004) sy-host 'client'(005) sy-mandt
      INTO l_text SEPARATED BY space.
  lr_grid->create_text(
         row    = l_line
         column = 2
         text   = l_text ).
  add 1 to l_line.

  lr_grid->create_label(
         row    = l_line
         column = 1
         text   = 'Selection'(006) ).
  WRITE p_COMP   TO l_text.
  case p_PERIOD.
    when 'M'.    WRITE 'month'(007) TO l_text+11(5).
    when 'W'.    WRITE 'week'(008)  TO l_text+11(5).
    when 'D'.    WRITE 'day'(009)   TO l_text+11(5).
    when others. WRITE p_PERIOD TO l_text+11(5).
  endcase.
  WRITE 'from'(010) TO l_text+17(4).
  WRITE p_STRT TO l_text+22(10) DD/MM/YYYY.
  if p_STRT < p_ENDT.
    WRITE 'to'(011)   TO l_text+33(2).
    WRITE p_ENDT TO l_text+36(10) DD/MM/YYYY.
  endif.
*  WRITE p_SYS    TO l_text+35(32).
  lr_grid->create_text(
         row    = l_line
         column = 2
         text   = l_text ).
  add 1 to l_line.

  CONCATENATE 'stat/rfcrec' '=' l_stat_rfcrec into l_text
    SEPARATED BY SPACE.
  lr_grid->create_label(
         row    = l_line
         column = 1
         text   = l_text ).
  if l_stat_rfcrec > 5.
    lr_grid->create_text(
         row    = l_line
         column = 2
         text   = 'Profile parameter stat/rfcrec is activated'(021) ).
  else.
    lr_grid->create_text(
         row    = l_line
         column = 2
         text   = 'Profile parameter stat/rfcrec is too low'(022) ).
  endif.
  add 1 to l_line.

  if p_SV = 'X' or p_SVH = 'X'.
    lr_grid->create_label(
           row    = l_line
           column = 1
           text   = 'Navigation for SV'(012) ).
    lr_grid->create_text(
           row    = l_line
           column = 2
           text   = 'Double click on account shows RFC authorizations'(013) ).
    add 1 to l_line.
  endif.

  if p_CL = 'X' or p_CLH = 'X' or p_CLD = 'X' or p_CLDH = 'X'.
    lr_grid->create_label(
           row    = l_line
           column = 1
           text   = 'Navigation for CL and CLD'(026) ).
    lr_grid->create_text(
           row    = l_line
           column = 2
           text   = 'Double click on Target shows RFC destination'(027) ).
    add 1 to l_line.
    endif.

  gr_ALV_TABLE_gt_RESULT->SET_TOP_OF_LIST( lr_grid ).

* Footer

  CREATE OBJECT lr_grid.
  l_line = 1.

  lr_grid->create_label(
         row    = l_line
         column = 1
         text   = 'Program version'(t00) ).
  lr_grid->create_text(
         row    = l_line
         column = 2
         text   = c_program_version ).
  add 1 to l_line.

  gr_ALV_TABLE_gt_RESULT->SET_END_OF_LIST( lr_grid ).

endform. "set_title_GT_RESULT






*&---------------------------------------------------------------------*
*&      Form  show_alv_GT_USER
*&---------------------------------------------------------------------*
form show_alv_GT_USER.

  DATA: l_repid      LIKE sy-repid,
        ls_fieldcat  TYPE slis_fieldcat_alv,
        lt_fieldcat  TYPE slis_t_fieldcat_alv,
        ls_layout    TYPE slis_layout_alv,
        ls_event     TYPE slis_alv_event,
        lt_events    TYPE slis_t_event,
        lt_SORT      TYPE SLIS_T_SORTINFO_ALV,
        ls_SORT      TYPE SLIS_SORTINFO_ALV,
        ls_prnt      TYPE slis_print_alv.

  l_repid = sy-repid.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MANDT'.
  ls_fieldcat-ref_tabname   = 'USR02'.
  ls_fieldcat-ref_fieldname = 'MANDT'.
  ls_fieldcat-outputlen = 3.
  ls_fieldcat-datatype  = 'CHAR'.
  ls_fieldcat-seltext_s = 'Client'(s04).
  ls_fieldcat-seltext_m = 'Client'(m04).
  ls_fieldcat-seltext_l = 'Client in local system'(l04).
  ls_fieldcat-no_out   = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BNAME'.
  ls_fieldcat-ref_tabname   = 'USR02'.
  ls_fieldcat-ref_fieldname = 'BNAME'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-datatype  = 'CHAR'.
  ls_fieldcat-seltext_s = 'Account'(s05).
  ls_fieldcat-seltext_m = 'Account'(m05).
  ls_fieldcat-seltext_l = 'Account in local system'(l05).
  ls_fieldcat-no_out   = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'USTYP'.
  ls_fieldcat-ref_tabname   = 'USR02'.
  ls_fieldcat-ref_fieldname = 'USTYP'.
  ls_fieldcat-outputlen = 1.
  ls_fieldcat-datatype  = 'CHAR'.
  ls_fieldcat-seltext_s = 'User type'(s06).
  ls_fieldcat-seltext_m = 'User type'(m06).
  ls_fieldcat-seltext_l = 'User type of account in local system'(l06).
  ls_fieldcat-no_out   = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CLASS'.
  ls_fieldcat-ref_tabname   = 'USR02'.
  ls_fieldcat-ref_fieldname = 'CLASS'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-datatype  = 'CHAR'.
  ls_fieldcat-seltext_s = 'User group'(s07).
  ls_fieldcat-seltext_m = 'User group'(m07).
  ls_fieldcat-seltext_l = 'User group of account in local system'(l07).
  ls_fieldcat-no_out   = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'FUGR'.
  ls_fieldcat-ref_tabname   = 'RS38L'.
  ls_fieldcat-ref_fieldname = 'AREA'.
  ls_fieldcat-outputlen = 26.
  ls_fieldcat-datatype  = 'CHAR'.
  ls_fieldcat-seltext_s = 'Func.group'(s32).
  ls_fieldcat-seltext_m = 'Auth. for func.group'(m32).
  ls_fieldcat-seltext_l = 'Authorization for function group'(l32).
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'FUNC'.
  ls_fieldcat-ref_tabname   = 'TFDIR'.
  ls_fieldcat-ref_fieldname = 'FUNCNAME'.
  ls_fieldcat-outputlen = 30.
  ls_fieldcat-datatype  = 'CHAR'.
  ls_fieldcat-seltext_s = 'Function'(s33).
  ls_fieldcat-seltext_m = 'Auth. for function'(m33).
  ls_fieldcat-seltext_l = 'Authorization for function'(l33).
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'COUNTER'.
  ls_fieldcat-outputlen = 24.
  ls_fieldcat-datatype  = 'DEC'.
  ls_fieldcat-no_zero   = 'X'.
  ls_fieldcat-seltext_s = '# Calls'(s18).
  ls_fieldcat-seltext_m = 'No of calls'(m18).
  ls_fieldcat-seltext_l = 'Number of RFC calls'(l18).
  APPEND ls_fieldcat TO lt_fieldcat.

***

  ls_sort-spos      = 1.
  ls_sort-fieldname = 'COUNTER'.
  ls_sort-down      = 'X'.
  append ls_sort to lt_sort.

*  ls_layout-colwidth_optimize = 'X'.
*  ls_layout-zebra             = 'X'.
*  ls_layout-coltab_fieldname  = 'COLOR'.
  ls_layout-window_titlebar   = 'RFC Authorizations'(014).

*  CLEAR ls_event.
*  ls_event-name = slis_ev_user_command.
*  ls_event-form = 'HANDLE_EVENT_USER_COMMAND'.
*  APPEND ls_event TO lt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = l_repid
*     I_CALLBACK_PF_STATUS_SET          = 'HANDLE_EVENT_PF_STATUS'
*     I_CALLBACK_USER_COMMAND           = 'HANDLE_USER_COMMAND_GT_USER'
      I_CALLBACK_TOP_OF_PAGE            = 'TOP_PG_GT_USER'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      = 'RFC Authorizations'(014)
*     I_GRID_SETTINGS                   =
      IS_LAYOUT                         = ls_layout
      IT_FIELDCAT                       = lt_fieldcat
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
      IT_SORT                           = lt_sort
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
*     I_SAVE                            = ' '
*     IS_VARIANT                        =
*     IT_EVENTS                         = lt_events
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = gt_USER
    EXCEPTIONS
      PROGRAM_ERROR                     = 1
      OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

endform.                    "show_alv_GT_USER

*&---------------------------------------------------------------------*
*&      Form  TOP_PG_GT_USER
*&---------------------------------------------------------------------*
FORM top_pg_GT_USER.

  DATA : t_header        TYPE slis_t_listheader.
  DATA : s_header        TYPE slis_listheader.
  DATA : inf             TYPE slis_listheader-info.
  DATA : list_lines      TYPE i,
         ld_linesc(10)   TYPE c.

  DESCRIBE TABLE gt_USER LINES list_lines.
  ld_linesc = list_lines.

  CLEAR: s_header.
  s_header-typ   =  'H'.
  s_header-info  =  'RFC Authorizations'(014).
  APPEND s_header TO t_header.

  CLEAR: s_header.
  s_header-typ  = 'S'.
  CONCATENATE 'Account' ':' INTO s_header-key.
  CONCATENATE 'System'(003)      sy-sysid
              'client'(005)      gs_user_auth-mandt
              'user'(015)        gs_user_auth-bname
    INTO s_header-info SEPARATED BY space.
  APPEND s_header TO t_header.

  CLEAR: s_header.
  s_header-typ  = 'S'.
  CONCATENATE 'Attributes'(016) ':' INTO s_header-key.
  CONCATENATE 'user type'(017)   gs_user_auth-USTYP
              'user group'(018)  gs_user_auth-CLASS
    INTO s_header-info SEPARATED BY space.
  APPEND s_header TO t_header.

  CLEAR: s_header.
  s_header-typ  = 'S'.
  CONCATENATE 'Description'(019) ':' INTO s_header-key.
  s_header-info = 'Show which authorizations for S_RFC have been used'(020).
  APPEND s_header TO t_header.

  CLEAR: s_header.
  s_header-typ  = 'S'.
  CONCATENATE 'stat/rfcrec' '=' l_stat_rfcrec into s_header-key
    SEPARATED BY SPACE.
  if l_stat_rfcrec > 5.
    s_header-info = 'Profile parameter stat/rfcrec is activated'(021).
  else.
    s_header-info = 'Profile parameter stat/rfcrec is too low'(022).
  endif.
  APPEND s_header TO t_header.

  CLEAR: s_header.
  s_header-typ  = 'S'.
  CONCATENATE 'Program version'(t00) ':' INTO s_header-key.
  s_header-info = c_program_version.
  APPEND s_header TO t_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header
*     i_logo             = ''
    .

ENDFORM.                    " TOP_PG_GT_USER
