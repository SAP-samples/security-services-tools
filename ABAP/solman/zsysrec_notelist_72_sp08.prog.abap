*----------------------------------------------------------------------*
* This report is used to verify results from the application System
* Recommendations and to experiment with new features.
*----------------------------------------------------------------------*

*-----------------------------------------------------------------------
* Author: Frank Buchholz, SAP CoE Security Services
* Source: https://github.com/SAP-samples/security-services-tools
* Documentation:
* https://blogs.sap.com/2011/07/18/report-zsysrecnotelist-show-results-of-system-recommendation/
* https://community.sap.com/t5/application-development-blog-posts/report-zsysrec-notelist-show-results-of-system-recommendation/ba-p/13006390
*
* 28.03.2024 Restrict the size of the header (important for backgroud processing)
* 18.04.2023 Customizing SYSREC_MAX_RFC_TIME deactivatied, was added to AGSSN on 18.11.2022
* 17.02.2023 Some ABAPlint corrections, optimized value help
* 28.02.2019 perform DISPLAY_NOTE_TEXT updated for ST-A/PI Release 01T_731, SP 1
* 06.08.2018 Separation between SAP and user status
* 17.04.2018 Prepare for License Audit notes
* 07.08.2017 Show customizing
* 27.07.2017 Call /SDF/SCSI_GET_HARDWARE_INFO instead of SCSI_GET_HARDWARE_INFO
* 28.06.2017 Show Side-Effect Notes
* 06.12.2016 Initial version
* to do:
* - Use function AGSNO_GET_SFW_COMPS for ABAP to get active switched components
* - Use favorite within selection (not after reading the data)
*
*tables:
*  AGSSR_CONFSYS    " SysRec: Technical system (system + type)
*  AGSSR_SYSNOTE    " SysRec: Notes
*  AGSSR_SYSNOTECI  " SysRec: Correction instructions of notes
*  AGSSR_SYSNOTES   " SysRec: Latest note status
*   New fields as of SolMan 7.2 SP 7 (see Class CL_AGS_SYSREC_DPC_EXT Method SYSTEMNOTECOMMEN_CREATE_ENTITY ):
*   LAST_SSTATUS
*   LAST_SUSER
*   FLAG_REVIEW
*   OLD_SSTATUS
*  AGSSR_SYSNOTEC   " SysRec: Note comments
*  AGSSR_STATUS     " SysRec: Status
*  AGSSR_NOTE       " Notes: Header
*  AGSSR_NOTECIO    " Notes: Object list
*  AGSSR_NOTECIP    " Notes: Required notes
*  AGSSR_NOTESD     " Notes: Side Effect Notes
*  AGSSR_NOTEAUDITA " Notes: License Audit C_LAW, EMC, EMD, MTI, RFC_RT SM_USMM, [...]
*-----------------------------------------------------------------------

report RSYSREC_NOTELIST
  message-id AGSNO_MESSAGE
  line-size 1023.

constants C_PROGRAM_VERSION(30) type C value '28.03.2024 FBT'.

type-pools AGSNO.

type-pools: ICON, COL, SYM.

*----------------------------------------------------------------------*

tables SSCRFIELDS.
selection-screen: function key 1, "System Recommendations
                  function key 2, "Status Customizing
                  function key 3, "Joblist
                  function key 4, "Application log
                  function key 5. "BW Query
*                  FUNCTION KEY 6. "Show configuration

* Definitions for select-options
data: SL_SYS   type AGSSR_CONFSYS-SYSTEM_NAME,
      SL_TYPE  type AGSSR_CONFSYS-SYSTEM_TYPE,
      SL_NOTE  type AGSSR_NOTE-NOTE_NUMBER,
      SL_PRIO  type AGSSR_NOTE-PRIORITY,
      SL_STAT  type AGSSR_STATUS-STATUS_ID,
      SL_THEMK type AGSNOTE_API_S_THEMK-THEMK,
      SL_COMP  type AGSNO_API_S_COMPVERS-COMPONENT.

selection-screen begin of block SYS with frame title TEXT001.

* System name
selection-screen begin of line.
selection-screen comment 1(25) SS_SYS for field S_SYS.
select-options S_SYS   for SL_SYS.
selection-screen end of line.

* System type
selection-screen begin of line.
selection-screen comment 1(25) SS_TYPE for field S_TYPE.
select-options S_TYPE  for SL_TYPE.
selection-screen end of line.

* Favorite
selection-screen begin of line.
selection-screen comment 1(28) SS_FAVO for field S_FAVO.
parameters S_FAVO as checkbox.             " System Favorite
selection-screen end of line.

selection-screen end of block SYS.

selection-screen begin of block NS with frame title TEXT002.

* Note
selection-screen begin of line.
selection-screen comment 1(25) SS_NOTE for field S_NOTE.
select-options S_NOTE  for SL_NOTE.
selection-screen end of line.

* Priority
selection-screen begin of line.
selection-screen comment 1(25) SS_PRIO for field S_PRIO.
select-options S_PRIO  for SL_PRIO.
selection-screen end of line.

* Application Component
selection-screen begin of line.
selection-screen comment 1(25) SS_THEMK for field S_THEMK.
select-options S_THEMK for SL_THEMK
                matchcode object H_AKH_COMP. " AGS_AKH_COMPONENT or H_AKH_COMP or H_AKH_COMPONENT
selection-screen end of line.

* Software Component
selection-screen begin of line.
selection-screen comment 1(25) SS_COMP for field S_COMP.
select-options S_COMP  for SL_COMP matchcode object H_CVERS.
selection-screen end of line.

* Release date from
selection-screen begin of line.
selection-screen comment 1(28) SS_FROM for field S_FROM.
parameters S_FROM type SY-DATUM.
selection-screen end of line.

* Release date to
selection-screen begin of line.
selection-screen comment 1(28) SS_TO   for field S_TO.
parameters S_TO   type SY-DATUM. " default sy-datum.
selection-screen end of line.

selection-screen end of block NS.

selection-screen begin of block SR with frame title TEXT004.

* User status
selection-screen begin of line.
selection-screen comment 1(25) SS_STAT for field S_STAT.
select-options S_STAT  for SL_STAT.
selection-screen end of line.

selection-screen end of block SR.

selection-screen begin of block NT with frame title TEXT003.

selection-screen begin of line.
selection-screen comment 1(28) SS_GRP1 for field S_GROUP1.
parameters S_GROUP1 as checkbox default 'X'. " Security notes
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(28) SS_GRP2 for field S_GROUP2.
parameters S_GROUP2 as checkbox.             " Hot News
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(28) SS_GRP3 for field S_GROUP3.
parameters S_GROUP3 as checkbox.             " Performance notes
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(28) SS_GRP4 for field S_GROUP4.
parameters S_GROUP4 as checkbox.             " Legal change notes
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(28) SS_GRP6 for field S_GROUP6.
parameters S_GROUP6 as checkbox.             " License Audit relevant notes
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(28) SS_GRP5 for field S_GROUP5.
parameters S_GROUP5 as checkbox.             " Correction notes
selection-screen end of line.

*selection-screen begin of line.
*SELECTION-SCREEN COMMENT 1(29) ss_grp6 for field s_group6.
*parameters s_group6 as checkbox.             " Java patch notes
*selection-screen end of line.

selection-screen end of block NT.

** Check RFC destination to managed system (ABAP)
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS       p_RFC AS CHECKBOX default space.
*SELECTION-SCREEN COMMENT 3(31) ps_RFC FOR FIELD p_RFC.
*SELECTION-SCREEN END OF LINE.

* Check implemented notes and Kernel via RFC
selection-screen begin of line.
selection-screen comment 1(30) PS_RFC for field P_RFC.
parameters       P_RFC as checkbox default 'X'.
selection-screen comment 36(60) PS_RFC1 for field P_RFC.
selection-screen end of line.

* Get required notes
selection-screen begin of line.
selection-screen comment 1(30) PS_REQ for field P_REQ.
parameters       P_REQ as checkbox default ' '.
selection-screen comment 36(60) PS_REQ1 for field P_REQ.
selection-screen end of line.

* Get side effect notes
selection-screen begin of line.
selection-screen comment 1(30) PS_SD for field P_SD.
parameters       P_SD as checkbox default ' '.
selection-screen comment 36(60) PS_SD1 for field P_SD.
selection-screen end of line.

* Count of month for which UPL data get loaded (2)
selection-screen begin of line.
selection-screen comment 1(30) PS_UPL for field P_UPL.
parameters       P_UPL(3) type N default 2.
selection-screen end of line.

* Layout of ALV output
selection-screen begin of line.
selection-screen comment 1(30) PS_LOUT for field P_LAYOUT.
parameters       P_LAYOUT type DISVARIANT-VARIANT.
selection-screen end of line.

* Omit system list in header
selection-screen begin of line.
selection-screen comment 1(30) PS_HDR for field P_HDR.
parameters       P_HDR as checkbox default ' '.
selection-screen end of line.

* Show configuration status
selection-screen begin of line.
selection-screen comment 1(30) PS_STAT1. " for field PS_STAT.
selection-screen pushbutton 32(40) PS_STAT user-command FC06 visible length 4.
parameters P_FC06(1) type C." no-display.
selection-screen end of line.

* Customizing table DNOC_USERCFG, Transaction SM30_DNOC_USERCFG_SR
selection-screen begin of line.
selection-screen comment 1(30) PS_DNOC1. " for field PS_DNOC.
selection-screen pushbutton 32(40) PS_DNOC user-command FC07 visible length 4.
parameters P_FC07(1) type C no-display.
selection-screen end of line.

* Customizing table AGSSR_OSDB, Transaction SM30_AGSSR_OSDB
selection-screen begin of line.
selection-screen comment 1(30) PS_OSDB1. " for field PS_OSDB.
selection-screen pushbutton 32(40) PS_OSDB user-command FC08 visible length 4.
parameters P_FC08(1) type C no-display.
selection-screen end of line.

* Show note user status and comments
selection-screen begin of line.
selection-screen comment 1(30) PS_COM1. " for field PS_COM.
selection-screen pushbutton 32(40) PS_COM user-command FC09 visible length 4.
parameters P_FC09(1) type C no-display.
selection-screen end of line.


selection-screen comment 1(60) SS_VERS.

*----------------------------------------------------------------------*

types:
* The order of the fields defines the order of columns of the ALV
  begin of TS_NOTELIST,  "based on type agsno_api_s_note
    SYSTEM_NAME            type AGSSR_CONFSYS-SYSTEM_NAME,
    SYSTEM_TYPE            type AGSSR_CONFSYS-SYSTEM_TYPE,
    SYSTEM_ROLE            type AGS_SR_S_LMDB_SYSTEM-ROLE,
    SYSTEM_ROLE_TEXT       type DD07T-DDTEXT,
    SYSTEM_PRIORITY        type AGS_SR_S_LMDB_SYSTEM-PRIORITY,
    SYSTEM_PRIORITY_TEXT   type DD07T-DDTEXT, "AGSSR_PRIORITY-PI_STEXT,

    NOTE_NUMBER            type AGSSR_NOTE-NOTE_NUMBER,
    NOTE_VERSION           type AGSSR_NOTE-NOTE_VERSION,
    SHORT_TEXT             type AGSSR_TNOTE-SHORT_TEXT,

    THEMK                  type AGSSR_NOTE-THEMK, "agsno_api_s_note-themk,
    THEMK_TEXT             type AGSSR_THEMK-TK_LTEXT, "agsno_api_s_note-themk_text,

    PRIORITY_ID            type AGSSR_NOTE-PRIORITY,
    PRIORITY               type AGSSR_PRIORITY-PI_STEXT,

    CATEGORY_ID            type AGSSR_NOTE-CATEGORY,
    CATEGORY               type AGSSR_CATEGORY-CT_STEXT,

    DISPLAY_URL            type AGSNO_API_S_NOTE-DISPLAY_URL,
    NOTEURL                type S_URL, " t_hyperlink

    RELEASE_DATE           type AGSSR_NOTE-RELEASE_DATE,

    SW_COMP_TBL            type AGSNO_API_S_NOTE-SW_COMP_TBL,

*   latest status and comment
    SSTATUS                type AGSSR_STATUS-STATUS_ID, "agsno_api_s_note-status,
    SSTATUS_TEXT           type AGSSR_STATUS-STATUS_TEXT,
*    sstatus_ltext       TYPE AGSSR_STATUS-STATUS_LTEXT,
    SUSER                  type AGSSR_SYSNOTES-LAST_SUSER,

*   latest status and comment
    STATUS                 type AGSSR_STATUS-STATUS_ID, "agsno_api_s_note-status,
    STATUS_TEXT            type AGSSR_STATUS-STATUS_TEXT,
*    status_ltext       TYPE AGSSR_STATUS-STATUS_LTEXT,
    USER                   type AGSSR_SYSNOTES-LAST_USER,
    COMMENT_CREATED_AT(19) type C, " type AGSSR_SYSNOTEC-CREATED_AT,
    LATEST_COMMENT         type AGSSR_SYSNOTEC-COMMENT_TEXT,

*   latest user status and comment
    USER_STATUS                 type AGSSR_STATUS-STATUS_ID, "agsno_api_s_note-status,
    USER_STATUS_TEXT            type AGSSR_STATUS-STATUS_TEXT,
*    user_status_ltext           TYPE AGSSR_STATUS-STATUS_LTEXT,
    USER_COMMENT_BY             type AGSSR_SYSNOTES-LAST_USER,
    USER_COMMENT_CREATED_AT(19) type C, " type AGSSR_SYSNOTEC-CREATED_AT,
    USER_LATEST_COMMENT         type AGSSR_SYSNOTEC-COMMENT_TEXT,

    PROC_STATUS            type AGS_SR_S_IMPL_NOTE-PROC_STATUS,
    IMPL_STATUS            type AGS_SR_S_IMPL_NOTE-IMPL_STATUS,  "AGSNO_API_S_NOTE-IMPL_STATUS,
    IMPL_NOTE_VERSION      type AGS_SR_S_IMPL_NOTE-NOTE_VERSION, "AGSNO_API_S_NOTE-IMPL_VERSION,

    AUTO                   type FLAG, "agsno_api_s_note-a,
    M                      type FLAG, "agsno_api_s_note-m,
    PRE                    type FLAG,
    POST                   type FLAG,
    MANUAL                 type FLAG,
    SPN                    type AGSNO_API_S_NOTE-SPN,

    IS_KERNEL              type AGSNO_API_S_NOTE-IS_KERNEL,
    KERN_REL(3)            type C, "see include LSHSYTOP - KINFOSTRUC-KERNEL_RELEASE
    KERN_PATCHLEVEL(5)     type C, "see include LSHSYTOP - KINFOSTRUC-KERNEL_PATCH_LEVEL

    IS_INDEP               type AGSSR_NOTE-IS_INDEP,

    REQUIRED_NOTES         type STRING,
    SIDEEFFECT_NOTES       type STRING,

    SEC_CATEGORY           type AGSSR_NOTE-SEC_CATEGORY,
    SEC_CATEGORY_TEXT      type AGSSR_SEC_CAT-SC_STEXT,
*    SEC_CATEGORY_LTEXT TYPE AGSSR_SEC_CAT-SC_LTEXT,

    AUDIT_ATTRIBUTE        type AGSSR_NOTEAUDITA-AUDIT_ATTRIBUTE,
    AUDIT_ATTRIBUTE_TEXT   type STRING,

    NOTE_TYPE_S            type C, " Security
    NOTE_TYPE_H            type C, " HotNews
    NOTE_TYPE_P            type C, " Performance
    NOTE_TYPE_L            type C, " Legal
    NOTE_TYPE_A            type C, " License Audit
    NOTE_TYPE_C            type C, " Correction

*   t_color           type lvc_t_scol,
*   t_celltype        type salv_t_int4_column,
    T_HYPERLINK            type SALV_T_INT4_COLUMN,
*   t_dropdown        type salv_t_int4_column,
  end of TS_NOTELIST,

  TT_NOTELIST type table of TS_NOTELIST
    with key SYSTEM_NAME SYSTEM_TYPE NOTE_NUMBER NOTE_VERSION.

* Global data table for ALV
data: GT_NOTELIST         type TT_NOTELIST.


types:
  begin of TS_URLS_FOR_NOTES,
    HANDLE  type SALV_DE_HYPERLINK_HANDLE,
    NOTENUM type AGSSR_NOTE-NOTE_NUMBER, "CWBNTSTEXT,
    NOTEURL type SERVICE_RL,
  end   of TS_URLS_FOR_NOTES,
  TT_URLS_FOR_NOTES type sorted table of TS_URLS_FOR_NOTES
                    with unique key NOTENUM.

data: LT_URLS_FOR_NOTES type TT_URLS_FOR_NOTES,
      LT_T_HYPERLINK    type SALV_T_INT4_COLUMN,
      LS_T_HYPERLINK    type SALV_S_INT4_COLUMN.


data GS_ALV_LOUT_VARIANT type DISVARIANT.

*----------------------------------------------------------------------*

initialization.

  data FUNCTXT type SMP_DYNTXT.

  FUNCTXT-ICON_ID   = ICON_WD_APPLICATION.
  FUNCTXT-QUICKINFO = 'System Recommendations'(001).
  FUNCTXT-ICON_TEXT = 'SysRec'(002).
  SSCRFIELDS-FUNCTXT_01 = FUNCTXT.

  FUNCTXT-ICON_ID   = ICON_TOOLS.
  FUNCTXT-QUICKINFO = 'Status Customizing'(003).
  FUNCTXT-ICON_TEXT = 'Status Customizing'(004).
  SSCRFIELDS-FUNCTXT_02 = FUNCTXT.

  FUNCTXT-ICON_ID   = ICON_BACKGROUND_JOB. "ICON_JOB ICON_JOB_DETAIL
  FUNCTXT-QUICKINFO = 'Job list'(034).
  FUNCTXT-ICON_TEXT = 'List'(035).
  SSCRFIELDS-FUNCTXT_03 = FUNCTXT.

  FUNCTXT-ICON_ID   = ICON_PROTOCOL. "ICON_ERROR_PROTOCOL
  FUNCTXT-QUICKINFO = 'Application log'(040).
  FUNCTXT-ICON_TEXT = 'Appl.Log'(041).
  SSCRFIELDS-FUNCTXT_04 = FUNCTXT.

  FUNCTXT-ICON_ID   = ICON_BW_REPORT_SAP. "ICON_QUERY.
  FUNCTXT-QUICKINFO = 'BW Query'(045).
  FUNCTXT-ICON_TEXT = 'BW Query'(046).
  SSCRFIELDS-FUNCTXT_05 = FUNCTXT.

*  We only have 5 selection screen function buttons

*  functxt-icon_id   = ICON_STATUS_OVERVIEW.
*  functxt-quickinfo = 'Show configuration'(043).
*  functxt-icon_text = 'Show Config'(044).
*  sscrfields-functxt_06 = functxt.

*  functxt-icon_id   = ICON_SYSTEM_SETTINGS. "or ICON_SETTINGS or ICON_PARAMETER
*  functxt-quickinfo = 'Customizing'(089).
*  functxt-icon_text = 'Customizing table DNOC_USERCFG'(090).
*  sscrfields-functxt_07 = functxt.



  TEXT001     = 'Systems'(005).
  SS_SYS      = 'System'(006).
  SS_TYPE     = 'System type'(007).
  SS_FAVO     = 'System Favorites only'(065).

  TEXT002     = 'Notes'(008).
  SS_NOTE     = 'Note'(009).
  SS_PRIO     = 'Priority'(010).
  SS_THEMK    = 'Application area'(011).
  SS_COMP     = 'Software component'(012).
  SS_FROM     = 'Released from date'(014).
  SS_TO       = 'Released to date'(015).

  TEXT004     = 'System Recommendations'(001).
  SS_STAT     = 'Status'(016).

  TEXT003     = 'Note types'(017).
  SS_GRP1     = 'Security notes'(018).
  SS_GRP2     = 'HotNews'(019).
  SS_GRP3     = 'Performance notes'(020).
  SS_GRP4     = 'Legal change notes'(021).
  SS_GRP6     = 'License Audit relevant notes'(103).
  SS_GRP5     = 'Correction notes'(023).
*  SS_GRP6     = 'Java Patch notes'(028).
  PS_RFC      = 'Check managed system via RFC'(082).
  PS_RFC1     = 'SolMan-READ destination is used'(091).
* ps_RFC      = 'Check RFC destination (ABAP)'(037).
  PS_REQ      = 'Get required notes (ABAP)'(092).
  PS_REQ1     = '(might be slow)'(093).
  PS_SD       = 'Get side effect notes (ABAP)'(094).
  PS_SD1      = '(might be slow)'(095).
  PS_UPL      = '# month to read UPL data'(042).

  PS_STAT     = ICON_STATUS_OVERVIEW. "'STAT'.
  PS_STAT1    = 'Show configuration status'(043).
  clear P_FC06.

  PS_LOUT     = 'Layout'(t18).

  PS_HDR      = 'Omit system list in header'(094).

  PS_DNOC     = ICON_SYSTEM_SETTINGS.  " ICON_SYSTEM_SETTINGS or ICON_SETTINGS or ICON_PARAMETER (see report RSTXICON)
  PS_DNOC1    = 'Customizing table DNOC_USERCFG'(090). "'Customizing'(089)
  clear P_FC07.

  PS_OSDB     = ICON_SETTINGS.         " ICON_SYSTEM_SETTINGS or ICON_SETTINGS or ICON_PARAMETER (see report RSTXICON)
  PS_OSDB1    = 'Customizing table AGSSR_OSDB'(022). "'Customizing'(089)
  clear P_FC08.

* Show note user status and comments
  PS_COM      = ICON_LIST.             " ICON_LIST or ICON_VIEW_LIST (see report RSTXICON)
  PS_COM1     = 'Show note user status and comments'.
  clear P_FC09.


  concatenate 'Program version:'(VER) C_PROGRAM_VERSION into SS_VERS
    separated by SPACE.

* SysRec: Count of month for which UPL data get loaded (default = 2)
  data: L_MONTH_CNT_C(3) type C.
*  get parameter id 'SYSREC_UPL_MONTH' field L_MONTH_CNT_C.
  L_MONTH_CNT_C = CL_AGS_SYSREC_CONFIG=>GET_UPL_CHECKED_MONTH( ).
*  if SY-SUBRC = 0.
  P_UPL = L_MONTH_CNT_C.
*  endif.

* Get System Roles
  data GT_SYSTEM_ROLES type  AGS_SR_T_KV.
  call function 'AGSNO_GET_ROLE'
    importing
      ROLE = GT_SYSTEM_ROLES.

*   Get System Priorities
  data GT_SYSTEM_PRIORITIES type  AGS_SR_T_KV.
  call function 'AGSNO_GET_PRIORITY'
    importing
      PRIORITY = GT_SYSTEM_PRIORITIES.



*----------------------------------------------------------------------*

at selection-screen.

  data: LS_BDC        type BDCDATA,
        LT_BDC        type table of BDCDATA,
        L_DATE        like SY-DATUM,
        L_DATUM_C(10).

  case SSCRFIELDS-UCOMM.

    when 'FC01'.
*     System Recommendations
      perform CALL_URL
        using '/sap/bc/ui5_ui5'                              " Application
              '/ui2/ushell/shells/abap/FioriLaunchpad.html'  " Service: Fiori Launchpad
              '#Action-UISMMySAPNotes'                       " Action: System Recommendations
              ABAP_TRUE.                                           " Add sap-client


    when 'FC02'.
*     System Recommendations Customizing
      call function 'VIEW_MAINTENANCE_CALL'
        exporting
          ACTION                       = 'U' "Update
*         CORR_NUMBER                  = '          '
*         GENERATE_MAINT_TOOL_IF_MISSING       = ' '
*         SHOW_SELECTION_POPUP         = ' '
          VIEW_NAME                    = 'AGSSR_STATUS'
          NO_WARNING_FOR_CLIENTINDEP   = 'X'
*         RFC_DESTINATION_FOR_UPGRADE  = ' '
*         CLIENT_FOR_UPGRADE           = ' '
*         VARIANT_FOR_SELECTION        = ' '
*         COMPLEX_SELCONDS_USED        = ' '
          CHECK_DDIC_MAINFLAG          = 'X'
*         SUPPRESS_WA_POPUP            = ' '
*       TABLES
*         DBA_SELLIST                  =
*         EXCL_CUA_FUNCT               =
        exceptions
          CLIENT_REFERENCE             = 1
          FOREIGN_LOCK                 = 2
          INVALID_ACTION               = 3
          NO_CLIENTINDEPENDENT_AUTH    = 4
          NO_DATABASE_FUNCTION         = 5
          NO_EDITOR_FUNCTION           = 6
          NO_SHOW_AUTH                 = 7
          NO_TVDIR_ENTRY               = 8
          NO_UPD_AUTH                  = 9
          ONLY_SHOW_ALLOWED            = 10
          SYSTEM_FAILURE               = 11
          UNKNOWN_FIELD_IN_DBA_SELLIST = 12
          VIEW_NOT_FOUND               = 13
          MAINTENANCE_PROHIBITED       = 14
          others                       = 15.
      if SY-SUBRC <> 0.
        message id SY-MSGID type SY-MSGTY number SY-MSGNO
                with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.


    when 'FC03'.
*     Show job log
*      call function 'AUTHORITY_CHECK_TCODE'
*        exporting
*          TCODE  = 'SM37'
*        exceptions
*          OK     = 0
*          NOT_OK = 2
*          others = 3.
*      if SY-SUBRC <> 0.
*        message S119(ED) with 'SM37'.
*        RETURN.
*      endif.
      clear: LS_BDC, LT_BDC.
      LS_BDC-PROGRAM  = 'SAPLBTCH'.
      LS_BDC-DYNPRO   = '2170'.
      LS_BDC-DYNBEGIN = 'X'.
      LS_BDC-FNAM     = 'BTCH2170-JOBNAME'.
      LS_BDC-FVAL     = 'SM:SYSTEM RECOMMENDATIONS'.
      append LS_BDC to LT_BDC.
      LS_BDC-DYNBEGIN = ' '.
      LS_BDC-FNAM     = 'BTCH2170-USERNAME'.
      LS_BDC-FVAL     = '*'.
      append LS_BDC to LT_BDC.
      L_DATE = SY-DATUM - 90.
      write L_DATE to L_DATUM_C dd/mm/yyyy.
      LS_BDC-FNAM     = 'BTCH2170-FROM_DATE'.
      LS_BDC-FVAL     = L_DATUM_C.
      append LS_BDC to LT_BDC.
      L_DATE = SY-DATUM + 30.
      write L_DATE to L_DATUM_C dd/mm/yyyy.
      LS_BDC-FNAM     = 'BTCH2170-TO_DATE'.
      LS_BDC-FVAL     = L_DATUM_C.
      append LS_BDC to LT_BDC.
      LS_BDC-FNAM     = 'BTCH2170-PRELIM'.   "Scheduled
      LS_BDC-FVAL     = 'X'.
      append LS_BDC to LT_BDC.
      LS_BDC-FNAM     = 'BTCH2170-SCHEDUL'.  "Released
      LS_BDC-FVAL     = 'X'.
      append LS_BDC to LT_BDC.
      LS_BDC-FNAM     = 'BTCH2170-READY'.    "Ready
      LS_BDC-FVAL     = 'X'.
      append LS_BDC to LT_BDC.
      LS_BDC-FNAM     = 'BTCH2170-RUNNING'.  "Active
      LS_BDC-FVAL     = 'X'.
      append LS_BDC to LT_BDC.
      LS_BDC-FNAM     = 'BTCH2170-FINISHED'. "Finished
      LS_BDC-FVAL     = 'X'.
      append LS_BDC to LT_BDC.
      LS_BDC-FNAM     = 'BTCH2170-ABORTED'.  "Canceled
      LS_BDC-FVAL     = 'X'.
      append LS_BDC to LT_BDC.
*      ls_bdc-FNAM     = 'OKCODE'.            "Submit
*      ls_bdc-FVAL     = 'DOIT'.
*      append ls_bdc to lt_bdc.
      try.
          call transaction 'SM37' WITH AUTHORITY-CHECK
            using LT_BDC.
        catch cx_sy_authorization_error.
          message S119(ED) with 'SM37'.
          RETURN.
      endtry.

    when 'FC04'.
*     Show application log
*      call function 'AUTHORITY_CHECK_TCODE'
*        exporting
*          TCODE  = 'SLG1'
*        exceptions
*          OK     = 0
*          NOT_OK = 2
*          others = 3.
*      if SY-SUBRC <> 0.
*        message S119(ED) with 'SLG1'.
*        RETURN.
*      endif.
      clear: LS_BDC, LT_BDC.
      LS_BDC-PROGRAM  = 'SAPLSLG3'.
      LS_BDC-DYNPRO   = '0100'.
      LS_BDC-DYNBEGIN = 'X'.
      LS_BDC-FNAM     = 'BALHDR-OBJECT'.
      LS_BDC-FVAL     = 'AGS_SR'.
      append LS_BDC to LT_BDC.
      LS_BDC-DYNBEGIN = ' '.
      L_DATE = SY-DATUM - 90.
      write L_DATE to L_DATUM_C dd/mm/yyyy.
      LS_BDC-FNAM     = 'BALHDR-ALDATE'.
      LS_BDC-FVAL     = L_DATUM_C.
      append LS_BDC to LT_BDC.
      L_DATE = SY-DATUM.
      write L_DATE to L_DATUM_C dd/mm/yyyy.
      LS_BDC-FNAM     = '*BALHDR-ALDATE'.
      LS_BDC-FVAL     = L_DATUM_C.
      append LS_BDC to LT_BDC.
*      ls_bdc-FNAM     = 'OK_CODE'.            "Submit
*      ls_bdc-FVAL     = 'SELE'.
*      append ls_bdc to lt_bdc.
      try.
          call transaction 'SLG1' WITH AUTHORITY-CHECK
            using LT_BDC.
        catch cx_sy_authorization_error.
          message S119(ED) with 'SLG1'.
          RETURN.
      endtry.

    when 'FC05'.
*     Call BW query
      data: L_T_URL type TIHTTPURLS2,
            L_S_URL type line of TIHTTPURLS2,
            L_URL   type STRING.
      clear L_T_URL.
      call function 'HTTP_GET_URL2'
*       DESTINATION 'NONE'
        exporting
          HANDLERCLASS = 'CL_RSR_WWW_HTTP'
        importing
          URLLIST      = L_T_URL
        exceptions
          others       = 0.
      read table L_T_URL into L_S_URL index 1. "1=http, 2=https
      if SY-SUBRC = 0.
*        concatenate
*          L_S_URL-PROTOCOL '://'  L_S_URL-HOST ':' L_S_URL-PORT
*          L_S_URL-URL                                             "/sap/bw/BEx
*          '?QUERY=' '0SMD_VCA2_SYS_RECOM_NOTES'
*          '&CMD=PROCESS_VARIABLES&VARIABLE_SCREEN=X'              "Show selection screen
**         '&VAR_NAME_1=' <name>                                   "Selection Variable Name
**         '&VAR_VALUE_EXT_1=' <value>                             "Selection Variable Name value
**         '&VAR_OPERATOR_1=' <value>
**         '&VAR_SIGN_1=' <value>
**         '&VAR_VALUE_LOW_EXT_1=' <value>
**         '&VAR_VALUE_HIGH_EXT_1=' <value>
**         '&SAP-LANGUAGE=' <language>
*          into L_URL.
*        call method CL_GUI_FRONTEND_SERVICES=>EXECUTE
*          exporting
*            DOCUMENT       = L_URL
*          exceptions
*            FILE_NOT_FOUND = 1
*            others         = 2.
        perform CALL_URL
          using L_S_URL-URL                                    " Application /sap/bw/BEx
                '?QUERY=0SMD_VCA2_SYS_RECOM_NOTES'             " Query
                '&CMD=PROCESS_VARIABLES&VARIABLE_SCREEN=X'     " Parameters
                ABAP_FALSE.                                    " do not add sap-client
      endif.


    when 'FC06'.
*     Show configuration status
*      PERFORM SHOW_CONFIGURATION.
*      We need the main list screen
      submit (SY-REPID)
        with P_FC06 = 'X'
        with s_SYS  in S_SYS
        with s_TYPE in s_TYPE
        and return.


    when 'FC07'.
*     System Recommendations Customizing
*      call function 'VIEW_MAINTENANCE_CALL'
*        exporting
*          ACTION                       = 'U' "Update
**         CORR_NUMBER                  = '          '
**         GENERATE_MAINT_TOOL_IF_MISSING       = ' '
**         SHOW_SELECTION_POPUP         = ' '
*          VIEW_NAME                    = 'DNOC_USERCFG'
*          NO_WARNING_FOR_CLIENTINDEP   = 'X'
**         RFC_DESTINATION_FOR_UPGRADE  = ' '
**         CLIENT_FOR_UPGRADE           = ' '
**         VARIANT_FOR_SELECTION        = ' '
**         COMPLEX_SELCONDS_USED        = ' '
*          CHECK_DDIC_MAINFLAG          = 'X'
**         SUPPRESS_WA_POPUP            = ' '
**       TABLES
**         DBA_SELLIST                  =
**         EXCL_CUA_FUNCT               =
*        exceptions
*          CLIENT_REFERENCE             = 1
*          FOREIGN_LOCK                 = 2
*          INVALID_ACTION               = 3
*          NO_CLIENTINDEPENDENT_AUTH    = 4
*          NO_DATABASE_FUNCTION         = 5
*          NO_EDITOR_FUNCTION           = 6
*          NO_SHOW_AUTH                 = 7
*          NO_TVDIR_ENTRY               = 8
*          NO_UPD_AUTH                  = 9
*          ONLY_SHOW_ALLOWED            = 10
*          SYSTEM_FAILURE               = 11
*          UNKNOWN_FIELD_IN_DBA_SELLIST = 12
*          VIEW_NOT_FOUND               = 13
*          MAINTENANCE_PROHIBITED       = 14
*          others                       = 15.
*      if SY-SUBRC <> 0.
*        message id SY-MSGID type SY-MSGTY number SY-MSGNO
*                with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      endif.

      CL_AGS_SYSREC_UTIL=>SM30_DNOC_USERCFG_SYSREC( ).


    when 'FC08'.
*     Customizing table AGSSR_OSDB, Transaction SM30_AGSSR_OSDB
      call function 'VIEW_MAINTENANCE_CALL'
        exporting
          ACTION                       = 'U' "Update
*         CORR_NUMBER                  = '          '
*         GENERATE_MAINT_TOOL_IF_MISSING       = ' '
*         SHOW_SELECTION_POPUP         = ' '
          VIEW_NAME                    = 'AGSSR_OSDB'
          NO_WARNING_FOR_CLIENTINDEP   = 'X'
*         RFC_DESTINATION_FOR_UPGRADE  = ' '
*         CLIENT_FOR_UPGRADE           = ' '
*         VARIANT_FOR_SELECTION        = ' '
*         COMPLEX_SELCONDS_USED        = ' '
          CHECK_DDIC_MAINFLAG          = 'X'
*         SUPPRESS_WA_POPUP            = ' '
*       TABLES
*         DBA_SELLIST                  =
*         EXCL_CUA_FUNCT               =
        exceptions
          CLIENT_REFERENCE             = 1
          FOREIGN_LOCK                 = 2
          INVALID_ACTION               = 3
          NO_CLIENTINDEPENDENT_AUTH    = 4
          NO_DATABASE_FUNCTION         = 5
          NO_EDITOR_FUNCTION           = 6
          NO_SHOW_AUTH                 = 7
          NO_TVDIR_ENTRY               = 8
          NO_UPD_AUTH                  = 9
          ONLY_SHOW_ALLOWED            = 10
          SYSTEM_FAILURE               = 11
          UNKNOWN_FIELD_IN_DBA_SELLIST = 12
          VIEW_NOT_FOUND               = 13
          MAINTENANCE_PROHIBITED       = 14
          others                       = 15.
      if SY-SUBRC <> 0.
        message id SY-MSGID type SY-MSGTY number SY-MSGNO
                with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.


* Show note user status and comments
    when 'FC09'.
      submit (SY-REPID)
        with P_FC09 = 'X'
        with S_SYS  in S_SYS
        with S_TYPE in S_TYPE
        with S_NOTE in S_NOTE
        with S_STAT in S_STAT
        and return.

  endcase.

form CALL_URL
  using LV_APPLICATION type STRING " Fiory Launchpad: /sap/bc/ui5_ui5
        LV_SERVICE     type STRING " Fiory Launchpad: /ui2/ushell/shells/abap/FioriLaunchpad.html
        LV_ACTION      type STRING " Fiory Launchpad: <empty>, Sysrec: '#Action-UISMMySAPNotes'
        LV_CLIENT      type ABAP_BOOL.

* URL generation see report /UI2/START_URL
* <https>://<server>:<port>/sap/bc/ui5_ui5/ui2/ushell/shells/abap/FioriLaunchpad.html<action>?sap-client=<client>

* get start url
  data LV_URL         type STRING.
  data LV_PARAM       type STRING.
  data LV_PROTOCOL    type STRING.
  data LV_HOST        type STRING.
  data LV_PORT        type STRING.
*  data LV_APPLICATION type STRING.

**********************************************************************
* create entries in table HTTPURLLOC for the below application string.
* these entries have to point to the reverse proxy (e.g. SAP WebDispatcher).
**********************************************************************

*  LV_APPLICATION = '/sap/bc/ui5_ui5'.

  CL_HTTP_SERVER=>IF_HTTP_SERVER~GET_LOCATION(
    exporting
      APPLICATION  = LV_APPLICATION
*     for_domain   = for_domain
    importing
      HOST         = LV_HOST
      PORT         = LV_PORT
      OUT_PROTOCOL = LV_PROTOCOL ).

  concatenate LV_PROTOCOL '://' LV_HOST ':' LV_PORT LV_APPLICATION into LV_URL.
  translate LV_URL to lower case.

*  concatenate LV_URL '/ui2/ushell/shells/abap/FioriLaunchpad.html' LV_ACTION into LV_URL.
  concatenate LV_URL LV_SERVICE LV_ACTION into LV_URL.

*  if access = abap_true.
*    perform add_url_param using 'sap-accessibility=X'       "#EC NOTEXT
*                          changing lv_url.
*  endif.
*  if langu is not initial.
*    concatenate 'sap-language=' langu into lv_param.        "#EC NOTEXT
*    perform add_url_param using lv_param
*                          changing lv_url.
*  endif.

  if LV_CLIENT = 'X'.
    do 1 times.
      concatenate 'sap-client=' SY-MANDT into LV_PARAM.     "#EC NOTEXT
      perform ADD_URL_PARAM using    LV_PARAM
                            changing LV_URL.
    enddo.
  endif.

** Activate External Debugging
*  if extdebug = abap_true.
*
**   Determine GUI IP address
*    data: ls_rfcsi  type rfcsi,
*          lv_gui_ip type rfcipaddr.
*    call function 'RFC_SYSTEM_INFO' destination 'SAPGUI'
*      importing
*        rfcsi_export          = ls_rfcsi
*      exceptions
*        communication_failure = 1
*        system_failure        = 2
*        others                = 3.
*    if sy-subrc eq 0.
*      lv_gui_ip = ls_rfcsi-rfcipaddr.
*    endif.
*
**   Reset external debugging for current user to reduce side effects
**   ... as a consequence in table ICFATTRIB the entries for the current user get deleted
*    call function 'HTTP_DEBUG_UPDATE'
*      exporting
*        action             = ' ' "Deactivate external debugging
*        authority_check    = 'X'
*        gui_ipaddress      = lv_gui_ip
*        gui_ipaddress_flag = ' '
*        no_message         = ' '
*        user               = sy-uname
*      exceptions
*        internal_error     = 1
*        others             = 2.
*    if sy-subrc <> 0.
*      message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*              display like 'S'.
*    endif.
*
**   Activate external debugging for the current user
**   ... as a consequence in table ICFATTRIB an entry for the current user is created
*    call function 'HTTP_DEBUG_UPDATE'
*      exporting
*        action             = 'X' "Activate external Debugging
*        authority_check    = 'X'
*        gui_ipaddress      = lv_gui_ip
*        gui_ipaddress_flag = ' '
*        no_message         = ' '
**       SYSDEBUG           = ' '
*        timeout            = '020000'
*        user               = sy-uname
*      exceptions
*        internal_error     = 1
*        others             = 2.
*    if sy-subrc <> 0.
*      message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*              display like 'S'.
*    endif.
*  endif.

* single sign-on
  constants LC_ICF_URL type STRING value '/sap/public/myssocntl'. "#EC SYNTCHAR
  data LV_SSO_ACTIVE   type ABAP_BOOL.
  call method CL_ICF_TREE=>IF_ICF_TREE~SERVICE_FROM_URL
    exporting
      URL                   = LC_ICF_URL
      HOSTNUMBER            = 0
      AUTHORITY_CHECK       = ABAP_FALSE
    importing
      ICFACTIVE             = LV_SSO_ACTIVE
    exceptions
      WRONG_APPLICATION     = 1
      NO_APPLICATION        = 2
      NOT_ALLOW_APPLICATION = 3
      WRONG_URL             = 4
      NO_AUTHORITY          = 5
      others                = 6.
  if SY-SUBRC ne 0.
    LV_SSO_ACTIVE = ABAP_FALSE.
  endif.
*    if lv_sso_active eq abap_false.
*      message e027(bsp_wd) with lc_icf_url.
*    endif.

  data LV_URLC type C length 1024.
  LV_URLC = LV_URL.

* start browser with single sign-on
  if LV_SSO_ACTIVE = ABAP_TRUE.
    data LV_CONTAINER type ref to CL_GUI_CONTAINER.         "#EC NEEDED
    data LV_VIEWER    type ref to CL_GUI_HTML_VIEWER.

    create object LV_VIEWER
      exporting
        PARENT             = LV_CONTAINER
      exceptions
        CNTL_ERROR         = 1
        CNTL_INSTALL_ERROR = 2
        DP_INSTALL_ERROR   = 3
        DP_ERROR           = 4
        others             = 5.
    if SY-SUBRC ne 0.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
              display like 'E'.
    endif.

    call method LV_VIEWER->ENABLE_SAPSSO
      exporting
        ENABLED    = ABAP_TRUE
      exceptions
        CNTL_ERROR = 1
        others     = 2.
    if SY-SUBRC ne 0.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
              display like 'E'.
    endif.

    call method LV_VIEWER->DETACH_URL_IN_BROWSER
      exporting
        URL        = LV_URLC
      exceptions
        CNTL_ERROR = 1
        others     = 2.
    if SY-SUBRC ne 0.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
              display like 'E'.
    endif.

    call method CL_GUI_CFW=>FLUSH
      exceptions
        CNTL_SYSTEM_ERROR = 1
        CNTL_ERROR        = 2
        others            = 3.
    if SY-SUBRC ne 0.
      message id SY-MSGID type SY-MSGTY number SY-MSGNO with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
              display like 'E'.
    endif.

* start browser without single-sign-on
  else.
    call function 'CALL_BROWSER'
      exporting
        URL                    = LV_URLC
*       WINDOW_NAME            = ' '
        NEW_WINDOW             = ABAP_TRUE
      exceptions
        FRONTEND_NOT_SUPPORTED = 1
        FRONTEND_ERROR         = 2
        PROG_NOT_FOUND         = 3
        NO_BATCH               = 4
        UNSPECIFIED_ERROR      = 5
        others                 = 6.
    if SY-SUBRC <> 0.
      message id SY-MSGID type 'S' number SY-MSGNO with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
              display like 'E'.
    endif.
  endif.
endform. " CALL_URL
*
form ADD_URL_PARAM using    IV_PARAM  type STRING
                   changing CV_URL    type STRING.

  if CV_URL ca '?'.
    concatenate CV_URL '&' IV_PARAM into CV_URL.            "#EC NOTEXT
  else.
    concatenate CV_URL '?' IV_PARAM into CV_URL.            "#EC NOTEXT
  endif.

endform. " add_url_param

*----------------------------------------------------------------------*
* SHOW_CONFIGURATION
*----------------------------------------------------------------------*
form SHOW_CONFIGURATION.

  format reset.

*----------------------------------------------------------------------*
* Show job
*----------------------------------------------------------------------*

* Heading
  format color col_group.
  write: / 'Scheduled background jobs (report AGSNO_RPT_COLLECT_DATA)'(050), at SY-LINSZ SPACE.
  format reset.

  data: LT_JOBLIST      type table of V_OP,
        LS_JOBLIST      type V_OP,
        SELECT_VALUES   type BTCSELECT,
        P_STATUS_CLAUSE type STRING.

* Selection see SM37 include LBTCHFXX FORM do_select_time_only
  SELECT_VALUES-FROM_DATE = SY-DATUM.
  SELECT_VALUES-FROM_TIME = '000000'.
  SELECT_VALUES-TO_DATE   = SY-DATUM + 30.
  SELECT_VALUES-TO_TIME   = '240000'.
  SELECT_VALUES-JOBNAME   = '%'. " SM:SYSTEM RECOMMENDATIONS
  SELECT_VALUES-USERNAME  = '%'.
  SELECT_VALUES-JOBCOUNT  = '%'.
  SELECT_VALUES-JOBGROUP  = '%'.
  SELECT_VALUES-ABAPNAME  = 'AGSNO_RPT_COLLECT_DATA'.
  P_STATUS_CLAUSE         = ''.
  select distinct * from V_OP into table @LT_JOBLIST
    where
        (
          (
            (
              (     SDLSTRTDT =  @SELECT_VALUES-FROM_DATE
                and SDLSTRTTM >= @SELECT_VALUES-FROM_TIME )
              or SDLSTRTDT > @SELECT_VALUES-FROM_DATE
            )
            and
            (
              (     SDLSTRTDT =  @SELECT_VALUES-TO_DATE
                and SDLSTRTTM <= @SELECT_VALUES-TO_TIME )
              or SDLSTRTDT < @SELECT_VALUES-TO_DATE
            )
          )
          or
          (
                SDLSTRTDT eq 'NO_DATE'
            and EVENTID   eq @SPACE
          )
        )
        and JOBNAME  like @SELECT_VALUES-JOBNAME escape '#'
        and SDLUNAME like @SELECT_VALUES-USERNAME
        and JOBCOUNT like @SELECT_VALUES-JOBCOUNT
        and JOBGROUP like @SELECT_VALUES-JOBGROUP
        and PROGNAME like @SELECT_VALUES-ABAPNAME
        and (P_STATUS_CLAUSE).
  loop at LT_JOBLIST into LS_JOBLIST.
    write: / LS_JOBLIST-JOBNAME   color col_key,
           /(10) 'Job ID'(069),     LS_JOBLIST-JOBCOUNT,
           /(10) 'Creator'(068),    LS_JOBLIST-SDLUNAME,
           /(10) 'periodic'(051),   LS_JOBLIST-PERIODIC,
           /(10) 'mins'(052),       LS_JOBLIST-PRDMINS,
           /(10) 'hours'(053),      LS_JOBLIST-PRDHOURS,
           /(10) 'days'(054),       LS_JOBLIST-PRDDAYS,
           /(10) 'weeks'(055),      LS_JOBLIST-PRDWEEKS,
           /(10) 'month'(056),      LS_JOBLIST-PRDMONTHS,
           /(10) 'status'(057),     LS_JOBLIST-STATUS,
           /(10) 'start'(067),      LS_JOBLIST-SDLSTRTDT dd/mm/yyyy,
                                    LS_JOBLIST-SDLSTRTTM using edit mask '__:__:__',
           /(10) 'user'(058),       LS_JOBLIST-AUTHCKNAM,
           /(10) 'client'(059),     LS_JOBLIST-AUTHCKMAN.
  endloop.
  skip.

*----------------------------------------------------------------------*
* Show Systems
*----------------------------------------------------------------------*

  format color col_group.
  write: / 'Configuration of SysRec background job'(060), at SY-LINSZ SPACE.
  format reset.

  data: LT_AGSSR_CONFSYS type table of AGSSR_CONFSYS,
        LS_AGSSR_CONFSYS type AGSSR_CONFSYS.

* Get active systems
  select * from AGSSR_CONFSYS into table @LT_AGSSR_CONFSYS
    where CHK_FLAG = @AGSNO_C_TRUE
     and  system_name in @s_SYS
      and system_type in @s_TYPE
    order by primary key.

* Get UPL availability
  data: LO_OBJ         type ref to CL_AGS_CC_SCMON_DATA,
        LO_EX_QUERY    type ref to CX_AGS_CC_QUERY_EXEC_ERROR,
        LO_EX_LMDB     type ref to CX_LMDB_UNKNOWN_SYSTEM_TYPE,
        LT_UPL_SYSTEMS type AGS_CC_UPL_DATA_SYSTEM_AV_T,
        LT_UPL_MONTH   type AGS_SCMON_DATA_MONTH_AV_T,
        LS_UPL_MONTH   type AGS_SCMON_DATA_MONTH_AV_S,
        L_LINES        type I.
  try.
      create object LO_OBJ.
    catch CX_AGS_CC_QUERY_EXEC_ERROR into LO_EX_QUERY.
      write: /(30) 'CREATE UPL' color col_negative, LO_EX_QUERY->GET_TEXT( ).
  endtry.
  if 1 = 1.
    try.
        LO_OBJ->GET_AVAILABLE_SYSTEMS( importing ET_SYSTEMS = LT_UPL_SYSTEMS ).
      catch CX_AGS_CC_QUERY_EXEC_ERROR into LO_EX_QUERY.
        write: /(30) 'GET_AVAILABLE_SYSTEMS' color col_negative, LO_EX_QUERY->GET_TEXT( ).
    endtry.
  endif.

* Show system data
  format color col_heading.
  write: /(8)  'Name'(070),
          (10) 'Type'(071),
          (24) 'Role'(072),
          (24) 'Priority'(073),
          (6)  'Active',
          (10) 'Date'(074),
          (8)  'Time'(075),
          "(32) 'RFC Destination'(076), " Might take ages in case of defect READ destinations
          (15) 'UPL available'(079),
          (15) 'Month'(080),
          (10) 'Favorite'(077),
               'Hana revision'(102).
  format reset.
  loop at LT_AGSSR_CONFSYS into LS_AGSSR_CONFSYS
    where system_name in s_SYS
      and system_type in s_TYPE.

*   Show System
    write: / LS_AGSSR_CONFSYS-SYSTEM_NAME color col_key,
             LS_AGSSR_CONFSYS-SYSTEM_TYPE color col_key.

*   Get system information
    data: LS_SYSTEM_INFO   type AGS_SR_S_LMDB_SYSTEM.
    clear LS_SYSTEM_INFO.
    try.
        call function 'AGSNO_GET_SYSTEM_INFO'
          exporting
            SYSTEM_NAME = LS_AGSSR_CONFSYS-SYSTEM_NAME
            SYSTEM_TYPE = LS_AGSSR_CONFSYS-SYSTEM_TYPE
*           ROLE        =
*           PRIORITY    =
          importing
            SYSTEM_INFO = LS_SYSTEM_INFO.

      catch CX_LMDB_UNKNOWN_SYSTEM_TYPE into LO_EX_LMDB.
        write: /(30) 'AGSNO_GET_SYSTEM_INFO' color col_negative, LO_EX_LMDB->GET_TEXT( ).
    endtry.

*   Show system role
    data: LS_KV type AGS_SR_S_KV.
    clear LS_KV.
    read table GT_SYSTEM_ROLES into LS_KV
      with key SR_KEY = LS_SYSTEM_INFO-ROLE.
    if SY-SUBRC = 0.
      write (24)  LS_KV-SR_VALUE.
    else.
      write (24)  LS_SYSTEM_INFO-ROLE.
    endif.

*   Show system priority
    clear LS_KV.
    read table GT_SYSTEM_PRIORITIES into LS_KV
      with key SR_KEY = LS_SYSTEM_INFO-PRIORITY.
    if SY-SUBRC = 0.
      write (24)  LS_KV-SR_VALUE.
    else.
      write (24)  LS_SYSTEM_INFO-PRIORITY.
    endif.

*   Show active flag
    if LS_AGSSR_CONFSYS-CHK_FLAG = 'T'.
      write (6) LS_AGSSR_CONFSYS-CHK_FLAG color col_positive.
    else.
      write (6) LS_AGSSR_CONFSYS-CHK_FLAG color col_negative.
    endif.

*   Show date of recommendations
    data: L_DATE type SY-DATUM,
          L_TIME type SY-UZEIT.
    convert time stamp LS_AGSSR_CONFSYS-LAST_CHK
        time zone SY-ZONLO
        into date L_DATE
             time L_TIME.
    write:   L_DATE dd/mm/yyyy,
             L_TIME using edit mask '__:__:__'.

*   Show RFC Destination for ABAP systems
    " Function AGSNO_GET_READ_RFC could take ages in case of not-working READ destinations
    " The wait time depends on dnoc_usercfg parameter agssn_sysrec_max_rfc_time = SYSREC_MAX_RFC_TIME
    if 1 = 0.
    data L_RFC type  RFCDEST.
    clear L_RFC.
    if LS_AGSSR_CONFSYS-SYSTEM_TYPE = 'ABAP'.
      call function 'AGSNO_GET_READ_RFC'
        exporting
          SYSTEM_NAME = LS_AGSSR_CONFSYS-SYSTEM_NAME
        importing
          RFC         = L_RFC.
      if L_RFC is initial.
        L_RFC = '<unknown>'(078).
      endif.
    endif.
    write (32) L_RFC.
    endif.

*   Show UPL availability
    if LS_AGSSR_CONFSYS-SYSTEM_TYPE = 'ABAP' and 1 = 1.
      read table LT_UPL_SYSTEMS transporting no fields
        with key SYSTEM_ID = LS_AGSSR_CONFSYS-SYSTEM_NAME.
      if SY-SUBRC = 0.
        write (15) 'UPL available'(083).
        clear: LT_UPL_MONTH, LS_UPL_MONTH.
        try.
            call method LO_OBJ->GET_AVAILABLE_MONTHS
              exporting
                IV_SYSTEM_ID = LS_AGSSR_CONFSYS-SYSTEM_NAME
              importing
                ET_MONTHS    = LT_UPL_MONTH.
          catch CX_AGS_CC_QUERY_EXEC_ERROR into LO_EX_QUERY.
            write: /(30) 'GET_AVAILABLE_MONTHS' color col_negative, LO_EX_QUERY->GET_TEXT( ).
        endtry.
        data: min_month like LS_UPL_MONTH-MONTH,
              max_month like LS_UPL_MONTH-MONTH.
        min_month = '999999'.
        max_month = '000000'.
        loop at LT_UPL_MONTH into LS_UPL_MONTH
          where SCMON = 'X' or UPL = 'X'.

          if min_month > LS_UPL_MONTH-MONTH. min_month = LS_UPL_MONTH-MONTH. endif.
          if max_month < LS_UPL_MONTH-MONTH. max_month = LS_UPL_MONTH-MONTH. endif.
        endloop.
        if min_month = '999999' or max_month = '000000'.
          write (15) 'error' color col_negative.
        else.
          write: min_month(4) no-gap, '.' no-gap, min_month+4(2) no-gap,
                 '-' no-gap,
                 max_month(4) no-gap, '.' no-gap, max_month+4(2) no-gap.
        endif.
      else.
        write: (15) SPACE, (15) SPACE.
      endif.
    else.
      write: (15) SPACE, (15) SPACE.
    endif.


*   Show Favorite
    data LS_AGSSR_SYSFAVO type AGSSR_SYSFAVO.
    clear LS_AGSSR_SYSFAVO.
    select single * from AGSSR_SYSFAVO into @LS_AGSSR_SYSFAVO
      where USER_NAME   = @SY-UNAME
        and SYSTEM_NAME = @LS_AGSSR_CONFSYS-SYSTEM_NAME
        and SYSTEM_TYPE = @LS_AGSSR_CONFSYS-SYSTEM_TYPE.
    if SY-SUBRC = 0.
      write (10) 'Favorite'(066).
    else.
      write (10) SPACE.
    endif.

*   Show system version for HANA
    if LS_AGSSR_CONFSYS-SYSTEM_TYPE = 'HANADB'.
      data: ls_SCV TYPE  AGS_SR_S_SCV,
            lt_SCV TYPE  AGS_SR_T_SCV.
      clear lt_SCV.
      CALL FUNCTION 'AGSNO_GET_SCV_SP_PL'
        EXPORTING
          SYSTEM_NAME       = LS_AGSSR_CONFSYS-SYSTEM_NAME
          SYSTEM_TYPE       = LS_AGSSR_CONFSYS-SYSTEM_TYPE
        IMPORTING
          ET_SCV            = lt_SCV
                .
      loop at lt_SCV into ls_SCV
        where name = 'HDB'.

        write: 'HDB', 'version', (5) ls_SCV-version, 'revision', ls_SCV-SPLEVEL, 'patchlevel', ls_SCV-patchlevel.
      endloop.


    endif.

  endloop.
  skip.

*----------------------------------------------------------------------*
* Show UPL execution time
*----------------------------------------------------------------------*

if 1 = 0.
  format color col_group.
  write: / 'UPL execution time'(081), at SY-LINSZ SPACE.
  format reset.

  perform SHOW_UPL_EXECUTION_TIME.
endif.

*----------------------------------------------------------------------*
* Show Customizing
*----------------------------------------------------------------------*

  types AGSSN.
  data: ls_DNOC_USERCFG  type DNOC_USERCFG,
        ls_DNOC_USERCFGT type DNOC_USERCFGT.

  format color col_group.
  write: / 'Customizing in table DNOC_USERCFG'(096), at SY-LINSZ SPACE.
  format reset.

  format color col_heading.
  write: /(32) 'Field'(097),
          (12) 'User'(098),
          (4)  'Nr'(099),
          (60) 'Value'(100),
          (60) 'Description'(101).
  format RESET.
  do.
    clear: ls_DNOC_USERCFG, ls_DNOC_USERCFGT.
    case sy-index.

      when  1. ls_DNOC_USERCFG-field =   agssn_sysrec_note_types.      " 'SYSREC_NOTE_TYPES',
               ls_DNOC_USERCFG-value =   'HSPL'.
               ls_DNOC_USERCFGT-descr =  'note types (HSPLC)'.

      when  2. ls_DNOC_USERCFG-field =   agssn_sysrec_last_monthyear. " 'SYSREC_LAST_MONTHYEAR',
               ls_DNOC_USERCFG-value =   '2009_01'.
               ls_DNOC_USERCFGT-descr =  'start month for notes selection'.

      when  3. ls_DNOC_USERCFG-field =   agssn_sysrec_status_filter.   " 'SYSREC_STATUS_FILTER',
               ls_DNOC_USERCFG-value =   'NEW,INP'.
               ls_DNOC_USERCFG-uname =   '<user>'.
               ls_DNOC_USERCFGT-descr =  'status filter'.
*     UPL
      when  4. ls_DNOC_USERCFG-field =   agssn_sysrec_upl_month.       " 'SYSREC_UPL_MONTH',
               ls_DNOC_USERCFG-value =   '2'.
               ls_DNOC_USERCFG-uname =   '<user>'.
               ls_DNOC_USERCFGT-descr =  'month for UPL'.
      when  5. ls_DNOC_USERCFG-field =   agssn_sysrec_upl_active.      " 'SYSREC_UPL_ACTIVE',
               ls_DNOC_USERCFG-uname =   '<user>'.
               ls_DNOC_USERCFG-value =   'T'.
               ls_DNOC_USERCFGT-descr =  'use UPL (T|F)'.

      when  6. ls_DNOC_USERCFG-field =   agssn_sysrec_calc_mode.       " 'SYSREC_CALC_MODE',
               ls_DNOC_USERCFGT-descr =  'calculation mode'.

      when  7. ls_DNOC_USERCFG-field =   agssn_sysrec_delta_days.      " 'SYSREC_DELTA_DAYS',
               ls_DNOC_USERCFG-value =   7.
               ls_DNOC_USERCFGT-descr =  'days'.

      "when  8. ls_DNOC_USERCFG-field =   agssn_sysrec_max_rfc_time.    " 'SYSREC_MAX_RFC_TIME', " added in AGSSN on 18.11.2022
      "         ls_DNOC_USERCFGT-descr =  'Max waiting time for RFC call to backend system'.

      when  9. ls_DNOC_USERCFG-field =   agssn_sysrec_rfc_call.        " 'SYSREC_RFC_CALL',
               ls_DNOC_USERCFGT-descr =  'RFC instead of webservice call to SAP Backbone ( |X)'.
*     display all systems at one time
      when 10. ls_DNOC_USERCFG-field =   'SYSREC_DIS_ALL'.
               ls_DNOC_USERCFGT-descr =  'display all systems on system statistics list ( |X)'.
*     BPCA
      when 11. ls_DNOC_USERCFG-field =   agssn_sysrec_bpca_user.       " 'SYSREC_BPCA_USER',
               ls_DNOC_USERCFGT-descr =  'BPCA requests from all users ( |X)'.
      when 12. ls_DNOC_USERCFG-field =   agssn_sysrec_bpca_date.       " 'SYSREC_BPCA_DATE',
               ls_DNOC_USERCFGT-descr =  'from date for BPCA requests'.
*     Charm
      when 13. ls_DNOC_USERCFG-field =   agssn_sysrec_charm_log_type.  " 'SYSREC_CHARM_LOG_TYPE',
               ls_DNOC_USERCFGT-descr =  'text Id for ChaRM log'.
      when 14. ls_DNOC_USERCFG-field =   agssn_sysrec_charm_user.      " 'SYSREC_CHARM_USER',
               ls_DNOC_USERCFGT-descr =  'ChaRM requests from all users ( |X)'.
      when 15. ls_DNOC_USERCFG-field =   agssn_sysrec_charm_date.      " 'SYSREC_CHARM_DATE',
               ls_DNOC_USERCFGT-descr =  'from date for ChaRM requests'.
*     Object list
      when 16. ls_DNOC_USERCFG-field =   agssn_sysrec_object_exp.      " 'SYSREC_OBJECT_EXP',
               ls_DNOC_USERCFG-value =   14.
               ls_DNOC_USERCFGT-descr =  'days until object list expires'.
      when 17. ls_DNOC_USERCFG-field =   agssn_sysrec_req_exp.         " 'SYSREC_REQ_EXP',
               ls_DNOC_USERCFG-value =   14.
               ls_DNOC_USERCFGT-descr =  'days until required notes expire'.
*     Side effect notes
      when 18. ls_DNOC_USERCFG-field =   agssn_sysrec_side_effect.     " 'SYSREC_SIDE_EFFECT',
               ls_DNOC_USERCFG-value =   14.
               ls_DNOC_USERCFGT-descr =  'days until side effect notes expire'.

*     when 19. ls_DNOC_USERCFG-field =   agssn_sysrec_engine_version.  " 'SYSREC_ENGINE_VERSION',
*     when 20. ls_DNOC_USERCFG-field =   agssn_sysrec_migrate_flag.    " 'SYSREC_MIGRATE_FLAG',
      when 21. ls_DNOC_USERCFG-field =   'URL_DISPLAY_NOTE'. " see method IF_EX_DNO_NOTE~DISPLAY_NOTE or GET_NOTE_URL_BY_NOTE_ID
      when 22. ls_DNOC_USERCFG-field =   'URL_SEARCH_NOTE'.
      when 23. exit.
    endcase.
    check ls_DNOC_USERCFG-field is not initial.

  select * from DNOC_USERCFG
    into CORRESPONDING FIELDS OF @ls_DNOC_USERCFG
    where FIELD = @ls_DNOC_USERCFG-field
    order by field, uname, lfdnr.

    clear ls_DNOC_USERCFGT.
    select single * from DNOC_USERCFGT into @ls_DNOC_USERCFGT
      where LANGUAGE = @sy-langu
        and field    = @ls_DNOC_USERCFGT-FIELD
        and uname    = @ls_DNOC_USERCFGT-uname
        and lfdnr    = @ls_DNOC_USERCFGT-lfdnr.
*   Show custom value
    write: /    ls_DNOC_USERCFG-field color col_total,
                ls_DNOC_USERCFG-UNAME,
                ls_DNOC_USERCFG-lfdnr,
           (60) ls_DNOC_USERCFG-value,
                ls_DNOC_USERCFGT-descr.
  endselect.
  if sy-subrc ne 0.
*   Show customized value
    write: /    ls_DNOC_USERCFG-field color col_normal,
                ls_DNOC_USERCFG-UNAME,
                ls_DNOC_USERCFG-lfdnr,
           (60) ls_DNOC_USERCFG-value,
                ls_DNOC_USERCFGT-descr.
  endif.

  enddo.
  skip.

endform. " SHOW_CONFIGURATION

form SHOW_UPL_EXECUTION_TIME.
* Show UPL runtime of following methods:
*   GET_AVAILABLE_SYSTEMS
*   GET_AVAILABLE_MONTHS
*   GET_UPL_DATA_PER_MONTHS

  data: LO_OBJ type ref to CL_AGS_CC_SCMON_DATA,
        LO_EX  type ref to CX_AGS_CC_QUERY_EXEC_ERROR.

  data: LT_SYSTEMS        type AGS_CC_UPL_DATA_SYSTEM_AV_T,
        LS_SYSTEMS        type AGS_CC_UPL_DATA_SYSTEM_AV_S,
        LT_MONTHS         type AGS_SCMON_DATA_MONTH_AV_T,
        LS_MONTHS         type AGS_SCMON_DATA_MONTH_AV_S,
        LT_R3TR_OBJECTS    type AGS_CC_UPL_R3TR_OBJECT_T,
        LS_R3TR_OBJECTS    type AGS_CC_UPL_R3TR_OBJECT_S,
        LT_UPL_MONTH_DATA type AGS_CC_UPL_DATA_MONTH_T,
        LS_UPL_MONTH_DATA type AGS_CC_UPL_DATA_MONTH_S,
        LV_FROM_MONTH     like LS_MONTHS-MONTH,
        LV_TO_MONTH       like LS_MONTHS-MONTH.

* Get run time of UPL calls
  data: T0           type I,
        T1           type I,
        TIME_CREATE  type I,
        TIME_SYSTEMS type I,
        TIME_MONTH   type I,
        TIME_DATA    type I.

  data: L_MSG(80)     type C,
        L_CNT_SYSTEMS type I,
        L_PERCENT     type I.

  format color col_heading.
  write: /(30)  'Function'(084),
          (8)   'System'(085),
          (11)  'Time'(086),
          (10)  'Month'(087),
         at  SY-LINSZ SPACE.
  format reset.

* CREATE object
  get run time field T0.
  try.
      create object LO_OBJ.
    catch CX_AGS_CC_QUERY_EXEC_ERROR into LO_EX.
      write: /(30) 'CREATE_UPL' color col_negative, LO_EX->GET_TEXT( ).
  endtry.
  get run time field T1.
  TIME_CREATE = T1 - T0.

*  write: /(30) 'Create UPL object', TIME_CREATE color col_total.

* GET_AVAILABLE_SYSTEMS
  clear: LT_SYSTEMS.
  concatenate 'Show UPL execution time'(088) 'GET_AVAILABLE_SYSTEMS'
    into L_MSG separated by SPACE.
  L_PERCENT = 1.
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      PERCENTAGE = L_PERCENT
      TEXT       = L_MSG.
  get run time field T0.
  try.
      LO_OBJ->GET_AVAILABLE_SYSTEMS( importing ET_SYSTEMS = LT_SYSTEMS ).
    catch CX_AGS_CC_QUERY_EXEC_ERROR into LO_EX.
      write: /(30) 'GET_AVAILABLE_SYSTEMS' color col_negative, LO_EX->GET_TEXT( ).
  endtry.
  get run time field T1.
  TIME_SYSTEMS = T1 - T0.

  write: /(30) 'GET_AVAILABLE_SYSTEMS', (8) SPACE, TIME_SYSTEMS color col_total.

* Execution time optimization: call only AGS_CC_GET_UPL_SYSTEMS_M
  get run time field T0.
  perform GET_AVAILABLE_SYSTEMS changing LT_SYSTEMS.
  get run time field T1.
  TIME_SYSTEMS = T1 - T0.

  write: /(30) 'AGS_CC_GET_UPL_SYSTEMS_M', (8) SPACE, TIME_SYSTEMS color col_total.

* Calculate date range based on user parameter SYSREC_UPL_MONTH
* SysRec: Count of month for which UPL data get loaded (default = 2)
  data: L_YEAR(4)        type N,
        L_MONTH(2)       type N,
        L_MONTH_CNT_C(3) type C,
        L_MONTH_CNT      type I.
*  get parameter id 'SYSREC_UPL_MONTH' field L_MONTH_CNT_C.
  L_MONTH_CNT_C = CL_AGS_SYSREC_CONFIG=>GET_UPL_CHECKED_MONTH( ).
  L_MONTH_CNT = L_MONTH_CNT_C.
  if SY-SUBRC ne 0 or L_MONTH_CNT < 1.
    L_MONTH_CNT = 2. "default: current month and previous month
  endif.
  L_YEAR   = SY-DATUM(4).
  L_MONTH  = SY-DATUM+4(2).
  LV_TO_MONTH(2)   = L_MONTH.
  LV_TO_MONTH+2(1) = '.'.
  LV_TO_MONTH+3(4) = L_YEAR.
  subtract 1 from L_MONTH_CNT.
  if L_MONTH > L_MONTH_CNT.
    L_MONTH = L_MONTH - L_MONTH_CNT.
  else.
    L_MONTH = L_MONTH + 12 - L_MONTH_CNT.
    L_YEAR  = L_YEAR - 1.
  endif.
  LV_FROM_MONTH(2)   = L_MONTH.
  LV_FROM_MONTH+2(1) = '.'.
  LV_FROM_MONTH+3(4) = L_YEAR.

* Fill objects into lt_R3TR_OBJECTS
* Field           Example             Description
* PROGRAM_ID      LIMU                Transport ID = LIMU, ...
* OBJECT_TYPE      REPS                Transport Object type
* OBJECT_NAME      CRM_FS_MASS_CHANGE  Transport Object name
* PROGRAM_ID_2    R3TR                TADIR ID = R3TR
* OBJECT_TYPE_2    PROG                TADIR Object type
* OBJECT_NAME_2    CRM_FS_MASS_CHANGE  TADIR Object name
  clear LT_R3TR_OBJECTS.
  LS_R3TR_OBJECTS-OBJECT_TYPE = 'PROG'.                 " TADIR Object type
  LS_R3TR_OBJECTS-OBJECT_NAME = 'ZSYSREC_NOTELIST_72'.  " TADIR Object name
  append LS_R3TR_OBJECTS to LT_R3TR_OBJECTS.

* Check all systems
  L_CNT_SYSTEMS = lines( LT_SYSTEMS ).
  loop at LT_SYSTEMS into LS_SYSTEMS
    where SYSTEM_ID in s_SYS.
    L_PERCENT = SY-TABIX * 100 / L_CNT_SYSTEMS.

*   GET_AVAILABLE_MONTH
* 1st call:
*   LV_INFOPROVIDER   0SM_UPLMM
*   LV_QUERY          0SM_UPLM_LOOKUP_CALMONTH
*   -> et_months
* 2nd call:
*   LV_INFOPROVIDER   0SM_UPLMM
*   LV_QUERY_ROOT     0SM_UPLMM_LOOKUP_MONTH_ROOT
*   -> lt_months_root (SCMON)
* 3rd call:
*   LV_INFOPROVIDER   0SM_UPLMM
*   LV_QUERY_NO_ROOT  0SM_UPLMM_LOOKUP_MONTH_NO_ROOT
*   -> lt_months_no_root (UPL)
* Execution time optimization: call only 0SM_UPLM_LOOKUP_CALMONTH
    clear: LT_MONTHS.
    concatenate 'Show UPL execution time'(088) 'GET_AVAILABLE_MONTH' LS_SYSTEMS-SYSTEM_ID
      into L_MSG separated by SPACE.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        PERCENTAGE = L_PERCENT
        TEXT       = L_MSG.
    get run time field T0.
    try.
        call method LO_OBJ->GET_AVAILABLE_MONTHS
          exporting
            IV_SYSTEM_ID = LS_SYSTEMS-SYSTEM_ID
          importing
            ET_MONTHS    = LT_MONTHS.
      catch CX_AGS_CC_QUERY_EXEC_ERROR into LO_EX.
        write: /(30) 'GET_AVAILABLE_MONTHS' color col_negative, LO_EX->GET_TEXT( ).
    endtry.
    get run time field T1.
    TIME_MONTH = T1 - T0.

    write: /(30) 'GET_AVAILABLE_MONTH',  LS_SYSTEMS-SYSTEM_ID, TIME_MONTH color col_total.

    sort LT_MONTHS by MONTH descending.
    loop at LT_MONTHS into LS_MONTHS to 6. " Just show 6 entries (month)
      write: LS_MONTHS-MONTH, LS_MONTHS-SCMON, LS_MONTHS-UPL.
    endloop.

    check 1 = 1.

*   GET_UPL_DATA_PER_MONTHS
    clear LT_UPL_MONTH_DATA.
    concatenate 'Show UPL execution time'(088) 'GET_UPL_DATA_PER_MONTHS' LS_SYSTEMS-SYSTEM_ID
      into L_MSG separated by SPACE.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        PERCENTAGE = L_PERCENT
        TEXT       = L_MSG.
    get run time field T0.
    try.
        call method LO_OBJ->GET_UPL_DATA_PER_MONTHS
          exporting
            SYSTEM_ID         = LS_SYSTEMS-SYSTEM_ID " System
            START_MONTH       = LV_FROM_MONTH        " Start Month (Format: MM.YYYY)
            END_MONTH         = LV_TO_MONTH          " End Month (Format: MM.YYYY)
            R3TR_OBJECTS      = LT_R3TR_OBJECTS     " Transport objects
          importing
            ET_UPL_MONTH_DATA = LT_UPL_MONTH_DATA.
      catch CX_AGS_CC_QUERY_EXEC_ERROR into LO_EX.
        write: /(30) 'GET_UPL_DATA_PER_MONTHS' color col_negative, LO_EX->GET_TEXT( ).
    endtry.
    get run time field T1.
    TIME_DATA = T1 - T0.

    write: /(30) 'GET_AVAILABLE_MONTH',  LS_SYSTEMS-SYSTEM_ID, TIME_DATA color col_total.

* ls_UPL_MONTH_DATA-SYSTEM_ID           System
* ls_UPL_MONTH_DATA-PROGRAM_ID
* ls_UPL_MONTH_DATA-PROGRAM_NAME
* ls_UPL_MONTH_DATA-OBJECT_TYPE
* ls_UPL_MONTH_DATA-OBJECT_NAME
* ls_UPL_MONTH_DATA-PROCESS_TYPE
* ls_UPL_MONTH_DATA-PROCESS_NAME
* ls_UPL_MONTH_DATA-PROCESS_CLASS
* ls_UPL_MONTH_DATA-MONTHM              Month
* ls_UPL_MONTH_DATA-OBJECT_EXECUTIONS   UPL Count of executions
    loop at LT_UPL_MONTH_DATA into LS_UPL_MONTH_DATA.
      write: / LS_UPL_MONTH_DATA-SYSTEM_ID,
               LS_UPL_MONTH_DATA-PROGRAM_ID,
               LS_UPL_MONTH_DATA-PROGRAM_NAME,
               LS_UPL_MONTH_DATA-OBJECT_TYPE,
               LS_UPL_MONTH_DATA-OBJECT_NAME,
               LS_UPL_MONTH_DATA-PROCESS_TYPE,
               LS_UPL_MONTH_DATA-PROCESS_NAME,
               LS_UPL_MONTH_DATA-PROCESS_CLASS,
               LS_UPL_MONTH_DATA-MONTHM,
               LS_UPL_MONTH_DATA-OBJECT_EXECUTIONS.
    endloop.

  endloop. "systems



endform. " SHOW_UPL_EXECUTION_TIME

form GET_AVAILABLE_SYSTEMS
  changing LT_SYS  type AGS_CC_UPL_DATA_SYSTEM_AV_T.
  clear LT_SYS.

* see class CL_AGS_CC_SCMON_DATA method GET_AVAILABLE_SYSTEMS
* Execution time optimization: call only AGS_CC_GET_UPL_SYSTEMS_M to get systems with month data only

  data:  MSG             type CHAR255,
         LF_RFC_DEST(32) type C,
         LT_SYSTEMS      type E2E_SID_TT.
*         lt_sys          TYPE ags_cc_upl_data_system_av_t.

  data: LO_EX  type ref to CX_SM_BW_ACCESS.

  try.
    LF_RFC_DEST = CL_SM_BW_ACCESS=>GET_WRITE_BW_DESTINATION( ). " Logical Destination (Specified in Function Call)
    catch CX_SM_BW_ACCESS into LO_EX.
      write: /(30) 'GET_WRITE_BW_DESTINATION' color col_negative, LO_EX->GET_TEXT( ).
      RETURN.
  endtry.

  call function 'AGS_CC_GET_UPL_SYSTEMS_M'
    destination LF_RFC_DEST
    importing
      ET_SYSTEMS            = LT_SYSTEMS
    exceptions
      NOT_SYSTEMS           = 1
      SYSTEM_FAILURE        = 2 message MSG
      COMMUNICATION_FAILURE = 3 message MSG
      others                = 4.
  if SY-SUBRC ne 0.
    write: /(30) 'AGS_CC_GET_UPL_SYSTEMS_M', (8) 'no data'(013) color col_negative, MSG.
  endif.
  append lines of LT_SYSTEMS to LT_SYS.

  sort LT_SYS ascending by SYSTEM_ID.
  delete adjacent duplicates from LT_SYS comparing SYSTEM_ID.

endform. " GET_AVAILABLE_SYSTEMS

*----------------------------------------------------------------------*

at selection-screen on value-request for S_SYS-LOW.
  perform F4_SYS.

at selection-screen on value-request for S_SYS-HIGH.
  perform F4_SYS.

*
form F4_SYS.

  TYPES:
    BEGIN OF ts_f4_value,
      SYSTEM_NAME   type SMSY_SUBSYS_NAME,
      SYSTEM_TYPE   type SMSY_SYSTYPE,
      ROLE_TEXT     type DD07V-DDTEXT, "AGS_SR_S_LMDB_SYSTEM-ROLE,
      PRIORITY_TEXT type DD07V-DDTEXT, "AGS_SR_S_LMDB_SYSTEM-PRIORITY,
      LAST_CHK_DATE type SY-DATUM, "TIMESTAMP,
      LAST_CHK_TIME type SY-UZEIT, "TIMESTAMP,
      FAVORITE      type TEXT10,
    END OF ts_f4_value.

  DATA:
    f4_value     TYPE          ts_f4_value,
    f4_value_tab TYPE TABLE OF ts_f4_value.

  data LS_SYSTEM_INFO   type AGS_SR_S_LMDB_SYSTEM.

  data LT_SYSTEM type  AGSNOTE_API_T_CHKED_SYSTEM.
  data LO_EX  type ref to CX_LMDB_UNKNOWN_SYSTEM_TYPE.
  call function 'AGSNO_API_GET_CHKED_SYSTEMS'
    importing
      ET_SYSTEM = LT_SYSTEM.
  loop at LT_SYSTEM into data(LS_SYSTEM).
    clear LS_SYSTEM_INFO.
    try.
        call function 'AGSNO_GET_SYSTEM_INFO'
          exporting
            SYSTEM_NAME = LS_SYSTEM-SYSTEM_NAME
            SYSTEM_TYPE = LS_SYSTEM-SYSTEM_TYPE
*           ROLE        =
*           PRIORITY    =
          importing
            SYSTEM_INFO = LS_SYSTEM_INFO.
      catch CX_LMDB_UNKNOWN_SYSTEM_TYPE into LO_EX.
        write: /(30) 'AGSNO_GET_SYSTEM_INFO' color col_negative, LO_EX->GET_TEXT( ).
    endtry.

    f4_VALUE-SYSTEM_NAME   = LS_SYSTEM-SYSTEM_NAME.
    f4_VALUE-SYSTEM_TYPE   = LS_SYSTEM-SYSTEM_TYPE.

    data LS_KV type AGS_SR_S_KV.
    read table GT_SYSTEM_ROLES into LS_KV
      with key SR_KEY = LS_SYSTEM_INFO-ROLE.
    if SY-SUBRC = 0.
      f4_VALUE-ROLE_TEXT     = LS_KV-SR_VALUE.
    else.
      f4_VALUE-ROLE_TEXT     = LS_SYSTEM_INFO-ROLE.
    endif.

    read table GT_SYSTEM_PRIORITIES into LS_KV
      with key SR_KEY = LS_SYSTEM_INFO-PRIORITY.
    if SY-SUBRC = 0.
      f4_VALUE-PRIORITY_TEXT = LS_KV-SR_VALUE.
    else.
      f4_VALUE-PRIORITY_TEXT = LS_SYSTEM_INFO-PRIORITY.
    endif.

    convert time stamp LS_SYSTEM-LAST_CHK
        time zone SY-ZONLO
        into date f4_VALUE-LAST_CHK_DATE
             time f4_VALUE-LAST_CHK_TIME.

    data LS_AGSSR_SYSFAVO type AGSSR_SYSFAVO.
    select single * from AGSSR_SYSFAVO into @LS_AGSSR_SYSFAVO
      where USER_NAME   = @SY-UNAME
        and SYSTEM_NAME = @LS_SYSTEM-SYSTEM_NAME
        and SYSTEM_TYPE = @LS_SYSTEM-SYSTEM_TYPE.
    if SY-SUBRC = 0.
      f4_VALUE-FAVORITE = 'Favorite'(066).
    else.
      f4_VALUE-FAVORITE = SPACE.
    endif.

    append f4_VALUE to f4_VALUE_TAB.
  endloop.
  sort f4_VALUE_TAB by SYSTEM_NAME SYSTEM_TYPE.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.
  DATA field TYPE dynfnam.
  DATA stepl TYPE sy-stepl.
  GET CURSOR FIELD field LINE stepl.
  DATA return_tab TYPE TABLE OF ddshretval.
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      RETFIELD        = 'SYSTEM_NAME'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = field
      stepl           = stepl
      value_org       = 'S'
    tables
*     field_tab       = field_tab
      VALUE_TAB       = f4_VALUE_TAB
      return_tab      = return_tab " surprisingly required to get lower case values
    exceptions
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2.
  if SY-SUBRC <> 0.
* Implement suitable error handling here
  endif.
endform. " F4_SYS

at selection-screen on value-request for S_TYPE-LOW.
  perform F4_TYPE.

at selection-screen on value-request for S_TYPE-HIGH.
  perform F4_TYPE.
*
form F4_TYPE.

  TYPES:
    BEGIN OF ts_f4_value,
      SYSTEM_TYPE type  SMSY_SYSTYPE,
    END OF ts_f4_value.

  DATA:
    f4_value     TYPE          ts_f4_value,
    f4_value_tab TYPE TABLE OF ts_f4_value.

  data LT_SYSTEM type  AGSNOTE_API_T_CHKED_SYSTEM.
  call function 'AGSNO_API_GET_CHKED_SYSTEMS'
    importing
      ET_SYSTEM = LT_SYSTEM.
  loop at LT_SYSTEM into data(LS_SYSTEM).
    f4_VALUE-SYSTEM_TYPE  = LS_SYSTEM-SYSTEM_TYPE.
    collect f4_VALUE into f4_VALUE_TAB.
  endloop.
  sort f4_VALUE_TAB by SYSTEM_TYPE.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.
  DATA field TYPE dynfnam.
  DATA stepl TYPE sy-stepl.
  GET CURSOR FIELD field LINE stepl.
  DATA return_tab TYPE TABLE OF ddshretval.
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      RETFIELD        = 'SYSTEM_TYPE'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = field
      stepl           = stepl
      value_org       = 'S'
    tables
*     field_tab       = field_tab
      VALUE_TAB       = f4_VALUE_TAB
      return_tab      = return_tab " surprisingly required to get lower case values
    exceptions
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2.
  if SY-SUBRC <> 0.
* Implement suitable error handling here
  endif.
endform. " F4_TYPE

at selection-screen on value-request for S_STAT-LOW.
  perform F4_STAT.

at selection-screen on value-request for S_STAT-HIGH.
  perform F4_STAT.
*
form F4_STAT.

  TYPES:
    BEGIN OF ts_f4_value,
      STATUS_ID    type AGSSR_STATUS-STATUS_ID,
      STATUS_TEXT  type AGSSR_STATUS-STATUS_TEXT,
      STATUS_LTEXT type AGSSR_STATUS-STATUS_LTEXT,
    END OF ts_f4_value.

  DATA:
    f4_value     TYPE          ts_f4_value,
    f4_value_tab TYPE TABLE OF ts_f4_value.

  select STATUS_ID, STATUS_TEXT, STATUS_LTEXT
    from AGSSR_STATUS
    into table @F4_VALUE_TAB
    where LANGU = @SY-LANGU
    order by primary key.
  if SY-SUBRC ne 0.
    select STATUS_ID, STATUS_TEXT, STATUS_LTEXT
      from AGSSR_STATUS
      into table @F4_VALUE_TAB
      where LANGU = 'E'       "English is secondary language
      order by primary key.
  endif.

  read table F4_VALUE_TAB TRANSPORTING NO FIELDS
   WITH KEY
     status_id = 'SAPINITSTA'.
  if sy-subrc is not initial.
    F4_VALUE-STATUS_ID    = 'SAPINITSTA'.
    F4_VALUE-STATUS_TEXT  = 'Initial user status'(030).
    F4_VALUE-STATUS_LTEXT = 'Initial user status'(030).
    insert F4_VALUE into F4_VALUE_TAB index 1.
  endif.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.
  DATA field TYPE dynfnam.
  DATA stepl TYPE sy-stepl.
  GET CURSOR FIELD field LINE stepl.
  DATA return_tab TYPE TABLE OF ddshretval.
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      RETFIELD        = 'STATUS_ID'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = field
      stepl           = stepl
      value_org       = 'S'
    tables
*     field_tab       = field_tab
      VALUE_TAB       = F4_VALUE_TAB
      return_tab      = return_tab " surprisingly required to get lower case values
    exceptions
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2.
  if SY-SUBRC <> 0.
* Implement suitable error handling here
  endif.
endform. " F4_STAT

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON p_layout
*----------------------------------------------------------------------*

at selection-screen on P_LAYOUT.
  check P_LAYOUT is not initial.
  perform HANDLE_AT_SELSCR_ON_P_LAYOUT using P_LAYOUT SY-REPID 'A'.
*
form HANDLE_AT_SELSCR_ON_P_LAYOUT
   using ID_VARNAME type DISVARIANT-VARIANT
         ID_REPID   type SY-REPID
         ID_SAVE    type C.

  data LS_VARIANT type DISVARIANT.

  LS_VARIANT-REPORT  = ID_REPID.
  LS_VARIANT-VARIANT = ID_VARNAME.

  call function 'REUSE_ALV_VARIANT_EXISTENCE'
    exporting
      I_SAVE        = ID_SAVE
    changing
      CS_VARIANT    = LS_VARIANT
    exceptions
      WRONG_INPUT   = 1
      NOT_FOUND     = 2
      PROGRAM_ERROR = 3
      others        = 4.

  if SY-SUBRC <> 0.
*   Selected layout variant is not found
    message E204(0K).
  endif.

  GS_ALV_LOUT_VARIANT-REPORT  = ID_REPID.
  GS_ALV_LOUT_VARIANT-VARIANT = ID_VARNAME.

endform.                    " handle_at_selscr_on_p_layout

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout
*----------------------------------------------------------------------*
at selection-screen on value-request for P_LAYOUT.  " ( Note 890141 )
  perform HANDLE_AT_SELSCR_F4_P_LAYOUT using    SY-REPID 'A'
                                       changing P_LAYOUT.
*
form HANDLE_AT_SELSCR_F4_P_LAYOUT
  using    ID_REPID   type SY-REPID
           ID_SAVE    type C
  changing ED_VARNAME type DISVARIANT-VARIANT.

  GS_ALV_LOUT_VARIANT-REPORT = ID_REPID.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      IS_VARIANT    = GS_ALV_LOUT_VARIANT
      I_SAVE        = ID_SAVE
    importing
      ES_VARIANT    = GS_ALV_LOUT_VARIANT
    exceptions
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      others        = 3.

  if SY-SUBRC = 0.
    ED_VARNAME = GS_ALV_LOUT_VARIANT-VARIANT.
  else.
    message S073(0K).
*   Keine Anzeigevariante(n) vorhanden
  endif.

endform.                               " handle_at_selscr_f4_p_layout

*----------------------------------------------------------------------*

start-of-selection.

* Show configuration status
  if P_FC06 = 'X'.
    perform SHOW_CONFIGURATION.
    RETURN.
  endif.

  if P_FC09 = 'X'.
    perform SHOW_STATUS_COMMENTS.
    RETURN.
  endif.

* Check authorizations (clear selection if no authorization)
  perform CHECK_AUTHORIZATIONS
    changing SY-SUBRC.
  if SY-SUBRC ne 0.
    message E050 with 'System Recommendations'(001).
  endif.

  if    S_GROUP1 is initial "agsno_c_security
    and S_GROUP2 is initial "agsno_c_hotnews
    and S_GROUP3 is initial "agsno_c_performance
    and S_GROUP4 is initial "agsno_c_legal
    and S_GROUP6 is initial "agsno_c_audit                      "###AUDIT###
    and S_GROUP5 is initial "agsno_c_correction
*    and s_group6 is initial "agsno_c_patch
    .
    message E002.
  endif.

* Count of month for which UPL data get loaded
*  set parameter id 'SYSREC_UPL_MONTH' field P_UPL.
* Maintain field SYSREC_UPL_MONTH in customizing table DNOC_USERCFG instead

* Get Settings
  data: L_REQNOTE_CHK_DATE type I.
  try.
      L_REQNOTE_CHK_DATE = CL_AGS_SYSREC_UTIL=>GET_KV( 'REQNOTE_CHK_DATE' ).
      if L_REQNOTE_CHK_DATE <= 0.
        L_REQNOTE_CHK_DATE = 7.
      endif.
    catch CX_ROOT.
      L_REQNOTE_CHK_DATE = 7.
  endtry.

*  CALL FUNCTION 'AGSNO_API_GET_CHKED_SYSTEMS'
*    IMPORTING
*      ET_SYSTEM       = LT_SYSTEM
*            .
*
*  loop at LT_SYSTEM into LS_SYSTEM
*    where SYSTEM_NAME in s_sys
*      and SYSTEM_TYPE in s_TYPE.
*
*    CALL FUNCTION 'AGSNO_API_GET_RECOMM_NOTES'
*      EXPORTING
*        IV_SYSTEM_NAME        = LS_SYSTEM-SYSTEM_NAME
*        IV_SYSTEM_TYPE        = LS_SYSTEM-SYSTEM_TYPE
*        IV_RELEASE_FROM       = S_FROM
*        IV_RELEASE_TO         = S_TO
*        IT_THEMK              = LT_THEMK
*        IT_NOTE               = LT_NOTE_NUMBERS
*        IT_GROUP              = LT_GROUP
*      IMPORTING
*        ET_NOTE               = LT_NOTE
*        ET_MSG                = LT_MSG
**        ES_SYSTEM             =
*            .
*
*  endloop.
  perform AGSNO_API_GET_RECOMM_NOTES.

* perform show_notelist.
  perform ALV_SHOW_NOTELIST.

*----------------------------------------------------------------------*

form CHECK_AUTHORIZATIONS
  changing SUBRC type SY-SUBRC.

  clear SUBRC.
  if S_GROUP1 = 'X'. "agsno_c_security
    authority-check object 'SM_FUNCS'
            id 'SM_APPL' field 'SYSTEM_REC'
            id 'SM_FUNC' field 'SECURITY'.              "#EC AUTFLD_MIS
    if SY-SUBRC ne 0.
      clear S_GROUP1.
      if SUBRC = 0. SUBRC = SY-SUBRC. endif.
    endif.
  endif.
  if S_GROUP2 = 'X'. "agsno_c_hotnews
    authority-check object 'SM_FUNCS'
            id 'SM_APPL' field 'SYSTEM_REC'
            id 'SM_FUNC' field 'HOTNEWS'.               "#EC AUTFLD_MIS
    if SY-SUBRC ne 0.
      clear S_GROUP2.
      if SUBRC = 0. SUBRC = SY-SUBRC. endif.
    endif.
  endif.
  if S_GROUP3 = 'X'. "agsno_c_performance
    authority-check object 'SM_FUNCS'
            id 'SM_APPL' field 'SYSTEM_REC'
            id 'SM_FUNC' field 'PERFORMANC'.            "#EC AUTFLD_MIS
    if SY-SUBRC ne 0.
      clear S_GROUP3.
      if SUBRC = 0. SUBRC = SY-SUBRC. endif.
    endif.
  endif.
  if S_GROUP4 = 'X'. "agsno_c_legal
    authority-check object 'SM_FUNCS'
            id 'SM_APPL' field 'SYSTEM_REC'
            id 'SM_FUNC' field 'LEGALCHANG'.            "#EC AUTFLD_MIS
    if SY-SUBRC ne 0.
      clear S_GROUP4.
      if SUBRC = 0. SUBRC = SY-SUBRC. endif.
    endif.
  endif.
  if S_GROUP6 = 'X'. "agsno_c_audit                 "###AUDIT###
    authority-check object 'SM_FUNCS'
            id 'SM_APPL' field 'SYSTEM_REC'
            id 'SM_FUNC' field 'LIC_AUDIT'.                 "#EC AUTFLD_MIS  ###AUDIT###
    if SY-SUBRC ne 0.
      clear S_GROUP6.
      if SUBRC = 0. SUBRC = SY-SUBRC. endif.
    endif.
  endif.
  if S_GROUP5 = 'X'. "agsno_c_correction
    authority-check object 'SM_FUNCS'
            id 'SM_APPL' field 'SYSTEM_REC'
            id 'SM_FUNC' field 'CORRECTION'.            "#EC AUTFLD_MIS
    if SY-SUBRC ne 0.
      clear S_GROUP5.
      if SUBRC = 0. SUBRC = SY-SUBRC. endif.
    endif.
  endif.
*  if s_group6 = 'X'. "agsno_c_patch
*    AUTHORITY-CHECK OBJECT 'SM_FUNCS'
*            ID 'SM_APPL' FIELD 'SYSTEM_REC'
*            ID 'SM_FUNC' FIELD '...'. "#EC AUTFLD_MIS
*    if sy-subrc ne 0.
*      clear S_GROUP6.
*      if subrc = 0. subrc = sy-subrc. endif.
*    endif.
*  ENDIF.

endform. " CHECK_AUTHORIZATIONS

*----------------------------------------------------------------------*

form AGSNO_API_GET_RECOMM_NOTES.

  data LS_NOTELIST         type TS_NOTELIST.

  field-symbols <LS_NOTELIST> type TS_NOTELIST.

  data: LV_NOTE_TYPS  type AGSNOTE_NOTE_TYPS,
        LV_CORR_TYPS  type AGSNOTE_CORR_TYPS,
        LV_HAS_KERNEL type AGSNOTE_FLAG,
        LV_IS_INDEP   type AGSNOTE_FLAG.

  data: LT_NOTE_TYPES type range of AGSSR_NOTE-NOTE_TYPES,
        LS_NOTE_TYPES like line of LT_NOTE_TYPES.

  data LS_URLS_FOR_NOTES type TS_URLS_FOR_NOTES.

* Prepare date range
  if S_TO is initial.
    S_TO = SY-DATUM.
  endif.

* Prepare note types (no selection = all)
  LS_NOTE_TYPES-SIGN = 'I'.
  LS_NOTE_TYPES-OPTION = 'CP'.
  if S_GROUP1 = 'X'. "agsno_c_security
    LS_NOTE_TYPES-LOW = '*S*'.
    append LS_NOTE_TYPES to LT_NOTE_TYPES.
  endif.
  if S_GROUP2 = 'X'. "agsno_c_hotnews
    LS_NOTE_TYPES-LOW = '*H*'.
    append LS_NOTE_TYPES to LT_NOTE_TYPES.
  endif.
  if S_GROUP3 = 'X'. "agsno_c_performance
    LS_NOTE_TYPES-LOW = '*P*'.
    append LS_NOTE_TYPES to LT_NOTE_TYPES.
  endif.
  if S_GROUP4 = 'X'. "agsno_c_legal
    LS_NOTE_TYPES-LOW = '*L*'.
    append LS_NOTE_TYPES to LT_NOTE_TYPES.
  endif.
  if S_GROUP6 = 'X'. "agsno_c_audit
    LS_NOTE_TYPES-LOW = '*A*'.                           "###AUDIT###
    append LS_NOTE_TYPES to LT_NOTE_TYPES.
  endif.
  if S_GROUP5 = 'X'. "agsno_c_correction
    LS_NOTE_TYPES-LOW = '*C*'.
    append LS_NOTE_TYPES to LT_NOTE_TYPES.
  endif.
*  if s_group6 = 'X'. "agsno_c_patch
*    ls_note_types-low = '<...>'.
*    APPEND ls_note_types TO lt_note_types.
*  ENDIF.

  clear: LS_NOTELIST, LV_NOTE_TYPS, LV_CORR_TYPS, LV_HAS_KERNEL, LV_IS_INDEP.
  select S~SYSTEM_NAME,
         S~SYSTEM_TYPE,
         N~NOTE_NUMBER,
         N~NOTE_VERSION,
         N~PRIORITY,
         N~CATEGORY,
         N~SEC_CATEGORY,
         N~THEMK,
         N~RELEASE_DATE,
         N~NOTE_TYPES,
         N~IS_INDEP,
         S~CORR_TYPES,
         S~HAS_KERNEL,
         S~SPNAME
         from AGSSR_SYSNOTE as S
         inner join AGSSR_NOTE as N
           on S~NOTE_NUMBER = N~NOTE_NUMBER
         into
         (@LS_NOTELIST-SYSTEM_NAME,
          @LS_NOTELIST-SYSTEM_TYPE,
          @LS_NOTELIST-NOTE_NUMBER,
          @LS_NOTELIST-NOTE_VERSION,
          @LS_NOTELIST-PRIORITY_ID,
          @LS_NOTELIST-CATEGORY_ID,
          @LS_NOTELIST-SEC_CATEGORY,
          @LS_NOTELIST-THEMK,
          @LS_NOTELIST-RELEASE_DATE,
          @LV_NOTE_TYPS,
          @LV_IS_INDEP,
          @LV_CORR_TYPS,
          @LV_HAS_KERNEL,
          @LS_NOTELIST-SPN)
      where S~SYSTEM_NAME  in @S_SYS
        and S~SYSTEM_TYPE  in @S_TYPE
        and N~NOTE_NUMBER  in @S_NOTE
        and N~PRIORITY     in @S_PRIO
        and N~THEMK        in @S_THEMK
        and N~RELEASE_DATE >= @S_FROM
        and N~RELEASE_DATE <= @S_TO
        and N~NOTE_TYPES   in @LT_NOTE_TYPES
      .

*   Check user favorite
    data LS_AGSSR_SYSFAVO type AGSSR_SYSFAVO.
    if S_FAVO = 'X'.
      select single * from AGSSR_SYSFAVO into @LS_AGSSR_SYSFAVO
        where USER_NAME   = @SY-UNAME
          and SYSTEM_NAME = @LS_NOTELIST-SYSTEM_NAME
          and SYSTEM_TYPE = @LS_NOTELIST-SYSTEM_TYPE.
      if SY-SUBRC ne 0.
        continue.
      endif.
    endif.

*   Get SysRec status of note
*   New fields as of SolMan 7.2 SP 7 (see Class CL_AGS_SYSREC_DPC_EXT Method SYSTEMNOTECOMMEN_CREATE_ENTITY ):
*   LAST_SSTATUS
*   LAST_SUSER
*   FLAG_REVIEW
*   OLD_SSTATUS
    select single LAST_SSTATUS, LAST_SUSER,
                  LAST_STATUS,  LAST_USER
      from AGSSR_SYSNOTES
      into (@LS_NOTELIST-SSTATUS, @LS_NOTELIST-SUSER,
            @LS_NOTELIST-STATUS,  @LS_NOTELIST-USER)
      where SYSTEM_NAME = @LS_NOTELIST-SYSTEM_NAME
        and SYSTEM_TYPE = @LS_NOTELIST-SYSTEM_TYPE
        and NOTE_NUMBER = @LS_NOTELIST-NOTE_NUMBER.
    if SY-SUBRC <> 0.
      LS_NOTELIST-SSTATUS =  agssn_status_new. "'NEW'.
      LS_NOTELIST-SUSER   = ''.
      LS_NOTELIST-STATUS  = agssn_status_und.  "'SAPINITSTA'.
      LS_NOTELIST-USER    = ''.
    endif.
    check LS_NOTELIST-STATUS in S_STAT.

*   Get short text of SAP status
* NEW  New
* SAPREVIEWF SAP review (agssn_status_review)
* INP  New version available
    select single STATUS_TEXT from AGSSR_STATUS into @LS_NOTELIST-SSTATUS_TEXT
      where LANGU = @SY-LANGU
        and STATUS_ID = @LS_NOTELIST-SSTATUS.
    if SY-SUBRC ne 0.
      select single STATUS_TEXT from AGSSR_STATUS into @LS_NOTELIST-SSTATUS_TEXT
       where LANGU = 'E'       "English is secondary language
         and STATUS_ID = @LS_NOTELIST-SSTATUS.
      if SY-SUBRC ne 0.
        LS_NOTELIST-SSTATUS_TEXT = LS_NOTELIST-SSTATUS.
      endif.
    endif.

*   Get short text of status
* SAPINITSTA Initial user status
* NEW  New
* INP  New version available
* IMP  To Be Implemented
* NOR  Irrelevant
* PSP  Postponed
    select single STATUS_TEXT from AGSSR_STATUS into @LS_NOTELIST-STATUS_TEXT
      where LANGU = @SY-LANGU
        and STATUS_ID = @LS_NOTELIST-STATUS.
    if SY-SUBRC ne 0.
      select single STATUS_TEXT from AGSSR_STATUS into @LS_NOTELIST-STATUS_TEXT
       where LANGU = 'E'       "English is secondary language
         and STATUS_ID = @LS_NOTELIST-STATUS.
      if SY-SUBRC ne 0.
        if LS_NOTELIST-STATUS = agssn_status_und.  "'SAPINITSTA'.
          LS_NOTELIST-STATUS_TEXT = 'Initial user status'(030).
        else.
          LS_NOTELIST-STATUS_TEXT = LS_NOTELIST-STATUS.
        endif.
      endif.
    endif.

*   Get short text of application component
    select single TK_LTEXT from AGSSR_THEMK into @LS_NOTELIST-THEMK_TEXT
      where LANGU = @SY-LANGU
        and TK_ID = @LS_NOTELIST-THEMK.
    if SY-SUBRC ne 0.
      select single TK_LTEXT from AGSSR_THEMK into @LS_NOTELIST-THEMK_TEXT
        where LANGU = 'E'       "English is secondary language
          and TK_ID = @LS_NOTELIST-THEMK.
      if SY-SUBRC ne 0.
        LS_NOTELIST-THEMK_TEXT = LS_NOTELIST-THEMK.
      endif.
    endif.

*   Get short text of security category
    select single SC_STEXT from AGSSR_SEC_CAT into @LS_NOTELIST-SEC_CATEGORY_TEXT
      where LANGU = @SY-LANGU
        and SC_ID = @LS_NOTELIST-SEC_CATEGORY.
    if SY-SUBRC ne 0.
      select single SC_STEXT from AGSSR_SEC_CAT into @LS_NOTELIST-SEC_CATEGORY_TEXT
        where LANGU = 'E'       "English is secondary language
          and SC_ID = @LS_NOTELIST-SEC_CATEGORY.
      if SY-SUBRC ne 0.
        LS_NOTELIST-SEC_CATEGORY_TEXT = LS_NOTELIST-SEC_CATEGORY.
      endif.
    endif.

*   Get technical user to distinguish technical comments from user comments
    data: l_technical_user type SYUNAME.
    CALL FUNCTION 'AGSNO_GET_SOLMAN_BTC_USER'
      IMPORTING
        EV_USER       = l_technical_user
              .

*   Get latest comment and latest user status and comment
*   LS_NOTELIST-STATUS, LS_NOTELIST-USER are already filled using table AGSSR_STATUS
    data: l_AGSSR_SYSNOTEC type AGSSR_SYSNOTEC,
          L_DATE           type SY-DATUM,
          L_DATEC(10)      type C,
          L_TIME           type SY-UZEIT,
          L_TIMEC(8)       type C,
          l_technical_comment type c.
    select * from AGSSR_SYSNOTEC
      into @l_AGSSR_SYSNOTEC
       where  SYSTEM_NAME = @LS_NOTELIST-SYSTEM_NAME
          and SYSTEM_TYPE = @LS_NOTELIST-SYSTEM_TYPE
          and NOTE_NUMBER = @LS_NOTELIST-NOTE_NUMBER
      order by CREATED_AT descending.

*     1st entry -> latest comment
      if sy-dbcnt = 1.
        convert time stamp L_AGSSR_SYSNOTEC-CREATED_AT
          time zone SY-ZONLO
          into date L_DATE
               time L_TIME.
        write L_DATE to L_DATEC dd/mm/yyyy.
        write L_TIME to L_TIMEC using edit mask '__:__:__'.
        concatenate L_DATEC L_TIMEC into LS_NOTELIST-COMMENT_CREATED_AT
          separated by SPACE.
        LS_NOTELIST-LATEST_COMMENT = l_AGSSR_SYSNOTEC-COMMENT_TEXT.

        if l_AGSSR_SYSNOTEC-CREATED_BY = l_technical_user.
          l_technical_comment = 'X'.
        else.
          l_technical_comment = space.
        endif.

      endif.

*     Older entry with different user than technical user -> latest user status and comment
      if sy-dbcnt > 1
        and l_technical_comment = 'X'
        and l_AGSSR_SYSNOTEC-CREATED_BY ne l_technical_user.

        LS_NOTELIST-USER_STATUS         = l_AGSSR_SYSNOTEC-STATUS_TO.

*   Get short text of status
    select single STATUS_TEXT from AGSSR_STATUS into @LS_NOTELIST-USER_STATUS_TEXT
      where LANGU = @SY-LANGU
        and STATUS_ID = @LS_NOTELIST-USER_STATUS.
    if SY-SUBRC ne 0.
      select single STATUS_TEXT from AGSSR_STATUS into @LS_NOTELIST-USER_STATUS_TEXT
       where LANGU = 'E'       "English is secondary language
         and STATUS_ID = @LS_NOTELIST-USER_STATUS.
      if SY-SUBRC ne 0.
        LS_NOTELIST-STATUS_TEXT = LS_NOTELIST-USER_STATUS.
      endif.
    endif.

        LS_NOTELIST-USER_COMMENT_BY     = l_AGSSR_SYSNOTEC-CREATED_BY.

        convert time stamp L_AGSSR_SYSNOTEC-CREATED_AT
          time zone SY-ZONLO
          into date L_DATE
               time L_TIME.
        write L_DATE to L_DATEC dd/mm/yyyy.
        write L_TIME to L_TIMEC using edit mask '__:__:__'.
        concatenate L_DATEC L_TIMEC into LS_NOTELIST-USER_COMMENT_CREATED_AT
          separated by SPACE.
        LS_NOTELIST-USER_LATEST_COMMENT = l_AGSSR_SYSNOTEC-COMMENT_TEXT.

        exit.
      endif.

    endselect.

*   Get system data
    data LS_SYSTEM_INFO type  AGS_SR_S_LMDB_SYSTEM.
    data LO_EX  type ref to CX_LMDB_UNKNOWN_SYSTEM_TYPE.
    if   LS_NOTELIST-SYSTEM_NAME ne LS_SYSTEM_INFO-SYSTEM_NAME
      or LS_NOTELIST-SYSTEM_TYPE ne LS_SYSTEM_INFO-SYSTEM_TYPE.
      try.
          call function 'AGSNO_GET_SYSTEM_INFO'
            exporting
              SYSTEM_NAME = LS_NOTELIST-SYSTEM_NAME
              SYSTEM_TYPE = LS_NOTELIST-SYSTEM_TYPE
*             ROLE        =
*             PRIORITY    =
            importing
              SYSTEM_INFO = LS_SYSTEM_INFO.
        catch CX_LMDB_UNKNOWN_SYSTEM_TYPE into LO_EX.
          write: /(30) 'AGSNO_GET_SYSTEM_INFO' color col_negative, LO_EX->GET_TEXT( ).
      endtry.
    endif.
    LS_NOTELIST-SYSTEM_ROLE     = LS_SYSTEM_INFO-ROLE.
    LS_NOTELIST-SYSTEM_PRIORITY = LS_SYSTEM_INFO-PRIORITY.

*   Get System Role Text
    data: LS_KV type AGS_SR_S_KV.
    read table GT_SYSTEM_ROLES into LS_KV
      with key SR_KEY = LS_NOTELIST-SYSTEM_ROLE.
    if SY-SUBRC = 0.
      LS_NOTELIST-SYSTEM_ROLE_TEXT = LS_KV-SR_VALUE.
    endif.

*   Get System Priority Text
*    data: ls_KV type AGS_SR_S_KV.
    read table GT_SYSTEM_PRIORITIES into LS_KV
      with key SR_KEY = LS_NOTELIST-SYSTEM_PRIORITY.
    if SY-SUBRC = 0.
      LS_NOTELIST-SYSTEM_PRIORITY_TEXT = LS_KV-SR_VALUE.
    endif.

*   Transform automatic correction flag
    if LV_CORR_TYPS ca 'C'. " automatic correction instruction
      LS_NOTELIST-AUTO = 'X'.
    endif.

*   Transform manual correction flags
    if LV_CORR_TYPS ca 'B'. " Pre manual
      LS_NOTELIST-PRE = 'Pre'(061).
      LS_NOTELIST-M = 'X'.
    endif.
    if LV_CORR_TYPS ca 'A'. " Post manual
      LS_NOTELIST-POST = 'Post'(062).
      LS_NOTELIST-M = 'X'.
    endif.
    if LV_CORR_TYPS ca 'M'. " Manual
      LS_NOTELIST-MANUAL = 'Manual'(063).
      LS_NOTELIST-M = 'X'.
    endif.

*   Get short text of note
    select single SHORT_TEXT
      from AGSSR_TNOTE
      into @LS_NOTELIST-SHORT_TEXT
      where LANGU       = @SY-LANGU
        and NOTE_NUMBER = @LS_NOTELIST-NOTE_NUMBER.
    if SY-SUBRC ne 0 and SY-LANGU ne 'E'.
      select single SHORT_TEXT
        from AGSSR_TNOTE
        into @LS_NOTELIST-SHORT_TEXT
        where LANGU       = 'E'
          and NOTE_NUMBER = @LS_NOTELIST-NOTE_NUMBER.
    endif.

*   Get License Audit Attributes
    if S_GROUP6 = 'X'.
      clear: LS_NOTELIST-AUDIT_ATTRIBUTE, LS_NOTELIST-AUDIT_ATTRIBUTE_TEXT.
      SELECT SINGLE AUDIT_ATTRIBUTE
        FROM AGSSR_NOTEAUDITA
        INTO @LS_NOTELIST-AUDIT_ATTRIBUTE
        WHERE note_number = @LS_NOTELIST-NOTE_NUMBER.
      if sy-subrc = 0.
*       see table AGSSR_MESUATTRI
*       C EMC     Engine Measurement Correction
*       D EMD     Engine Measurement Delivery
*       I EMI     Engine Measurement Info
*       L C_LAW   Consolidation LAW
*       R RFC_RT  RFC Result Transfer
*       S SM_USMM System Measurement USMM
*       T MTI     Measurement Tools Info
        select single LONG_TEXT
          from AGSSR_MESUATTRI
          into @LS_NOTELIST-AUDIT_ATTRIBUTE_TEXT
          where SHORT_TEXT = @LS_NOTELIST-AUDIT_ATTRIBUTE
            and LANGU      = @SY-LANGU.
        if sy-subrc ne 0.
*         Unknown value or multi value
          LS_NOTELIST-AUDIT_ATTRIBUTE_TEXT = LS_NOTELIST-AUDIT_ATTRIBUTE.
        endif.
      endif.
    endif.

*   Transform Kernel flag
    if LV_HAS_KERNEL = AGSNO_C_TRUE.
      LS_NOTELIST-IS_KERNEL = 'X'.
    endif.

*   Transform SP independent flag
    if LV_IS_INDEP = AGSNO_C_TRUE.
      LS_NOTELIST-IS_INDEP = 'X'.
    endif.

*   Transform note types (see domain AGSNOTE_NOTETYP)
    if LV_NOTE_TYPS ca 'S'.                 " Security
      LS_NOTELIST-NOTE_TYPE_S = 'S'.
    endif.
    if LV_NOTE_TYPS ca 'H'.                 " HotNews
      LS_NOTELIST-NOTE_TYPE_H = 'H'.
    endif.
    if LV_NOTE_TYPS ca 'P'.                 " Performance
      LS_NOTELIST-NOTE_TYPE_P = 'P'.
    endif.
    if LV_NOTE_TYPS ca 'L'.                 " Legal Change
      LS_NOTELIST-NOTE_TYPE_L = 'L'.
    endif.
    if LV_NOTE_TYPS ca 'A'.                 " License Audit
      LS_NOTELIST-NOTE_TYPE_A = 'A'.
    endif.
    if LV_NOTE_TYPS ca 'C'.                 " Correction
      LS_NOTELIST-NOTE_TYPE_C = 'C'.
    endif.
*   IF lv_note_typs CA '<...>'.
*     ls_notelist-note_type_<...> = 'X'.
*   ENDIF.

*   Get priority text
    LS_NOTELIST-PRIORITY = CL_AGS_SYSREC_BUFFER=>GET_PRIORITY_TEXT( LS_NOTELIST-PRIORITY_ID ).

*   Get category text
    LS_NOTELIST-CATEGORY = CL_AGS_SYSREC_BUFFER=>GET_CATEGORY_TEXT( LS_NOTELIST-CATEGORY_ID ).

*   Create link to service marketplace
    concatenate
      'https://launchpad.support.sap.com/#/notes/'
      LS_NOTELIST-NOTE_NUMBER
      into LS_NOTELIST-DISPLAY_URL.

    call function 'ICON_CREATE'
      exporting
        NAME                  = 'ICON_URL'
*       TEXT                  = ' '
        INFO                  = 'Link to note in the Support Portal'(SMP)
        ADD_STDINF            = ' '
      importing
        RESULT                = LS_NOTELIST-NOTEURL
      exceptions
        ICON_NOT_FOUND        = 1
        OUTPUTFIELD_TOO_SHORT = 2
        others                = 3.
    if SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

    read table LT_URLS_FOR_NOTES
      with key NOTENUM = LS_NOTELIST-NOTE_NUMBER
      binary search transporting no fields.
    if SY-SUBRC ne 0.
      if LS_NOTELIST-NOTE_NUMBER > 2147483647.
        LS_URLS_FOR_NOTES-HANDLE  = LS_NOTELIST-NOTE_NUMBER+1(9).
      else.
        LS_URLS_FOR_NOTES-HANDLE  = LS_NOTELIST-NOTE_NUMBER.
      endif.
      " Type conversion
      LS_URLS_FOR_NOTES-NOTENUM = LS_NOTELIST-NOTE_NUMBER.
      " https://service.sap.com/sap/support/notes/888889
      " https://launchpad.support.sap.com/#/notes/888889
      LS_URLS_FOR_NOTES-NOTEURL = LS_NOTELIST-DISPLAY_URL.
      insert LS_URLS_FOR_NOTES
        into table LT_URLS_FOR_NOTES. "sorted table
    endif.

    clear: LT_T_HYPERLINK, LS_T_HYPERLINK.
    LS_T_HYPERLINK-COLUMNNAME = 'NOTEURL'.
    LS_T_HYPERLINK-VALUE      = LS_URLS_FOR_NOTES-HANDLE.
    append LS_T_HYPERLINK to LT_T_HYPERLINK.
    LS_NOTELIST-T_HYPERLINK = LT_T_HYPERLINK.


*   Store result
    append LS_NOTELIST to GT_NOTELIST.

    clear: LS_NOTELIST, LV_NOTE_TYPS, LV_CORR_TYPS, LV_HAS_KERNEL.
  endselect.

* Get additional data (not possible in SELECT .. ENDSELECT above)
  loop at GT_NOTELIST assigning <LS_NOTELIST>.

*   Get required notes
    if p_req = 'X'.
    if <LS_NOTELIST>-SYSTEM_TYPE = 'ABAP' and <LS_NOTELIST>-AUTO = 'X'.
      perform GET_REQUIRED_NOTES
        using
          <LS_NOTELIST>-SYSTEM_NAME
          <LS_NOTELIST>-SYSTEM_TYPE
          <LS_NOTELIST>-NOTE_NUMBER
        changing
          <LS_NOTELIST>-REQUIRED_NOTES
          .
    endif.
    endif.

*   Get side effect notes
    if p_sd = 'X'.
    if <LS_NOTELIST>-SYSTEM_TYPE = 'ABAP'. " and <LS_NOTELIST>-AUTO = 'X'.
      perform GET_SIDEEFFECT_NOTES
        using
          <LS_NOTELIST>-SYSTEM_NAME
          <LS_NOTELIST>-SYSTEM_TYPE
          <LS_NOTELIST>-NOTE_NUMBER
        changing
          <LS_NOTELIST>-SIDEEFFECT_NOTES
          .
    endif.
    endif.

*   Get SNOTE implementation status of note from
    if <LS_NOTELIST>-SYSTEM_TYPE = 'ABAP'.
      perform GET_SNOTE_IMPL_STATUS
        using
          <LS_NOTELIST>-SYSTEM_NAME
          <LS_NOTELIST>-SYSTEM_TYPE
          <LS_NOTELIST>-NOTE_NUMBER
        changing
          <LS_NOTELIST>-PROC_STATUS
          <LS_NOTELIST>-IMPL_STATUS
          <LS_NOTELIST>-IMPL_NOTE_VERSION
          .
    endif.

*   Get kernel data
    perform GET_KERNEL_REL
      using
        <LS_NOTELIST>-SYSTEM_NAME
        <LS_NOTELIST>-SYSTEM_TYPE
      changing
        <LS_NOTELIST>-KERN_REL
        <LS_NOTELIST>-KERN_PATCHLEVEL
        .

*   Store result
*    modify GT_NOTELIST FROM <LS_NOTELIST>.
  endloop.


endform. " AGSNO_API_GET_RECOMM_NOTES

*----------------------------------------------------------------------*

form GET_REQUIRED_NOTES
  using
    L_SYSTEM_NAME       type AGSSR_CONFSYS-SYSTEM_NAME
    L_SYSTEM_TYPE       type AGSSR_CONFSYS-SYSTEM_TYPE
    L_NOTE_NUMBER       type AGSSR_NOTE-NOTE_NUMBER
  changing
    L_REQUIRED_NOTES    type STRING
    .

*   Get required notes for ABAP notes having automatic correction instructions
  data: LT_REQUIRED_NOTES_INPUT  type  AGSSN_T_SYSREC_REQ_OBJECT,
        LS_REQUIRED_NOTES_INPUT  type  AGSSN_S_SYSREC_REQ_OBJECT,
        LT_REQUIRED_NOTES_OUTPUT type  AGSSN_T_SYSREC_NOTECIP,
        LS_REQUIRED_NOTES_OUTPUT type  AGSSN_S_SYSREC_NOTECIP.

  clear L_REQUIRED_NOTES.

  check L_SYSTEM_TYPE = 'ABAP'.

  clear: LT_REQUIRED_NOTES_INPUT, LT_REQUIRED_NOTES_OUTPUT.
  LS_REQUIRED_NOTES_INPUT-NOTE_NUMBER = L_NOTE_NUMBER.
  append LS_REQUIRED_NOTES_INPUT to LT_REQUIRED_NOTES_INPUT.
  call function 'AGSNO_GET_REQUIRED_NOTES'
    exporting
      INPUT  = LT_REQUIRED_NOTES_INPUT
    importing
      OUTPUT = LT_REQUIRED_NOTES_OUTPUT.
  loop at LT_REQUIRED_NOTES_OUTPUT into LS_REQUIRED_NOTES_OUTPUT.
*   Ignore self references
    check LS_REQUIRED_NOTES_OUTPUT-REQ_NOTE_NUMBER ne LS_REQUIRED_NOTES_OUTPUT-NOTE_NUMBER.

*   Store result
    if L_REQUIRED_NOTES is initial.
      L_REQUIRED_NOTES = LS_REQUIRED_NOTES_OUTPUT-NOTE_NUMBER.
    else.
      concatenate L_REQUIRED_NOTES LS_REQUIRED_NOTES_OUTPUT-NOTE_NUMBER
        into L_REQUIRED_NOTES separated by SPACE.
    endif.
  endloop.

endform. " GET_REQUIRED_NOTES

*----------------------------------------------------------------------*

form GET_SIDEEFFECT_NOTES
  using
    L_SYSTEM_NAME       type AGSSR_CONFSYS-SYSTEM_NAME
    L_SYSTEM_TYPE       type AGSSR_CONFSYS-SYSTEM_TYPE
    L_NOTE_NUMBER       type AGSSR_NOTE-NOTE_NUMBER
  changing
    L_SIDEEFFECT_NOTES  type STRING
    .

*   Get side effect notes for ABAP notes having automatic correction instructions
  data: LT_SIDEEFFECT_NOTES_INPUT  type  AGSSN_T_SYSREC_REQ_OBJECT,
        LS_SIDEEFFECT_NOTES_INPUT  type  AGSSN_S_SYSREC_REQ_OBJECT,
        LT_SIDEEFFECT_NOTES_OUTPUT type  AGSSN_T_SYSREC_SIDE_EFFECT,
        LS_SIDEEFFECT_NOTES_OUTPUT type  AGSSN_S_SYSREC_SIDE_EFFECT.

  clear L_SIDEEFFECT_NOTES.

  check L_SYSTEM_TYPE = 'ABAP'.

  clear: LT_SIDEEFFECT_NOTES_INPUT, LT_SIDEEFFECT_NOTES_OUTPUT.
  LS_SIDEEFFECT_NOTES_INPUT-NOTE_NUMBER = L_NOTE_NUMBER.
  append LS_SIDEEFFECT_NOTES_INPUT to LT_SIDEEFFECT_NOTES_INPUT.
  call function 'AGSNO_GET_SIDEEFFECT_NOTES'
    exporting
      INPUT  = LT_SIDEEFFECT_NOTES_INPUT
    importing
      OUTPUT = LT_SIDEEFFECT_NOTES_OUTPUT.
  loop at LT_SIDEEFFECT_NOTES_OUTPUT into LS_SIDEEFFECT_NOTES_OUTPUT.
*   Ignore self references
    check LS_SIDEEFFECT_NOTES_OUTPUT-S_NOTE_NUMBER ne LS_SIDEEFFECT_NOTES_OUTPUT-NOTE_NUMBER.

*   Store result
    if L_SIDEEFFECT_NOTES is initial.
      L_SIDEEFFECT_NOTES = LS_SIDEEFFECT_NOTES_OUTPUT-S_NOTE_NUMBER.
    else.
      concatenate L_SIDEEFFECT_NOTES LS_SIDEEFFECT_NOTES_OUTPUT-S_NOTE_NUMBER
        into L_SIDEEFFECT_NOTES separated by SPACE.
    endif.
  endloop.

endform. " GET_SIDEEFFECT_NOTES

*----------------------------------------------------------------------*

form GET_SNOTE_IMPL_STATUS
  using
    L_SYSTEM_NAME       type AGSSR_CONFSYS-SYSTEM_NAME
    L_SYSTEM_TYPE       type AGSSR_CONFSYS-SYSTEM_TYPE
    L_NOTE_NUMBER       type AGSSR_NOTE-NOTE_NUMBER
  changing
    L_PROC_STATUS       type AGS_SR_S_IMPL_NOTE-PROC_STATUS
    L_IMPL_STATUS       type AGS_SR_S_IMPL_NOTE-IMPL_STATUS
    L_IMPL_NOTE_VERSION type AGS_SR_S_IMPL_NOTE-NOTE_VERSION
    .

  clear: L_PROC_STATUS, L_IMPL_STATUS, L_IMPL_NOTE_VERSION.

  check L_SYSTEM_TYPE = 'ABAP'.

  check P_RFC is not initial.

*   Check implemented notes via RFC
  types:
    begin of TS_SYS_IMPL_NOTE,
      SYSTEM_NAME  type AGSSR_CONFSYS-SYSTEM_NAME,
      SYSTEM_TYPE  type AGSSR_CONFSYS-SYSTEM_TYPE,
      LT_IMPL_NOTE type AGS_SR_T_IMPL_NOTE,
    end of TS_SYS_IMPL_NOTE,
    TT_SYS_IMPL_NOTE type sorted table of TS_SYS_IMPL_NOTE
      with unique key SYSTEM_NAME SYSTEM_TYPE.

  statics ST_SYS_IMPL_NOTE type TT_SYS_IMPL_NOTE.
  field-symbols <LS_SYS_IMPL_NOTE> type TS_SYS_IMPL_NOTE.
  data: LS_SYS_IMPL_NOTE type TS_SYS_IMPL_NOTE,
        LS_IMPL_NOTE     type AGS_SR_S_IMPL_NOTE.

  data l_tabix type sy-tabix.

* Try to get it from cache
  read table ST_SYS_IMPL_NOTE assigning <LS_SYS_IMPL_NOTE>
    with key SYSTEM_NAME = L_SYSTEM_NAME
             SYSTEM_TYPE = L_SYSTEM_TYPE.
  if SY-SUBRC ne 0.
    l_tabix = sy-tabix.
*   Get data via RFC
    clear: LS_SYS_IMPL_NOTE.
    LS_SYS_IMPL_NOTE-SYSTEM_NAME = L_SYSTEM_NAME.
    LS_SYS_IMPL_NOTE-SYSTEM_TYPE = L_SYSTEM_TYPE.
    call function 'AGSNO_GET_IMPL_NOTE'
      exporting
        SYSTEM_NAME = LS_SYS_IMPL_NOTE-SYSTEM_NAME
      importing
        NOTE        = LS_SYS_IMPL_NOTE-LT_IMPL_NOTE.
    insert LS_SYS_IMPL_NOTE into ST_SYS_IMPL_NOTE INDEX l_tabix.
    assign LS_SYS_IMPL_NOTE to <LS_SYS_IMPL_NOTE>.
  endif.

*     No we have data about implemented notes in <ls_sys_impl_note>
  read table <LS_SYS_IMPL_NOTE>-LT_IMPL_NOTE into LS_IMPL_NOTE
    with key NOTE_NUMBER = L_NOTE_NUMBER.
  if SY-SUBRC = 0.
    L_PROC_STATUS       = LS_IMPL_NOTE-PROC_STATUS.
    L_IMPL_STATUS       = LS_IMPL_NOTE-IMPL_STATUS.
    L_IMPL_NOTE_VERSION = LS_IMPL_NOTE-NOTE_VERSION.
  endif.

endform. " GET_SNOTE_IMPL_STATUS

*----------------------------------------------------------------------*

form GET_KERNEL_REL
  using
    L_SYSTEM_NAME       type AGSSR_CONFSYS-SYSTEM_NAME
    L_SYSTEM_TYPE       type AGSSR_CONFSYS-SYSTEM_TYPE
  changing
    L_KERN_REL          "type TS_SYS_KERN_REL-KERN_REL
    L_KERN_PATCHLEVEL   "type TS_SYS_KERN_REL-KERN_PATCHLEVEL
    .

  types:
    begin of TS_SYS_KERN_REL,
      SYSTEM_NAME        type AGSSR_CONFSYS-SYSTEM_NAME,
      SYSTEM_TYPE        type AGSSR_CONFSYS-SYSTEM_TYPE,
      KERN_REL(3)        type C, "see include LSHSYTOP - KINFOSTRUC-KERNEL_RELEASE
      KERN_PATCHLEVEL(5) type C, "see include LSHSYTOP - KINFOSTRUC-KERNEL_PATCH_LEVEL
    end of TS_SYS_KERN_REL,
    TT_SYS_KERN_REL type sorted table of TS_SYS_KERN_REL
      with unique key SYSTEM_NAME SYSTEM_TYPE.

  statics ST_SYS_KERN_REL type TT_SYS_KERN_REL.
  field-symbols <LS_SYS_KERN_REL> type TS_SYS_KERN_REL.
  data LS_SYS_KERN_REL type TS_SYS_KERN_REL.

  data l_tabix type sy-tabix.

  clear: L_KERN_REL, L_KERN_PATCHLEVEL.

* Try to get it from cache
  read table ST_SYS_KERN_REL assigning <LS_SYS_KERN_REL>
    with key SYSTEM_NAME = L_SYSTEM_NAME
             SYSTEM_TYPE = L_SYSTEM_TYPE.
  l_tabix = sy-tabix.
  if SY-SUBRC ne 0.
    do 1 times. " Trick to use CHECK instead of nested IFs

      clear: LS_SYS_KERN_REL.
      LS_SYS_KERN_REL-SYSTEM_NAME = L_SYSTEM_NAME.
      LS_SYS_KERN_REL-SYSTEM_TYPE = L_SYSTEM_TYPE.

*     Currently only ABAP is supported
      check L_SYSTEM_TYPE = 'ABAP'.

*     Get data via RFC
      check P_RFC is not initial.

*     call via SM-READ destination:
*     SCSI_GET_INSTANCE_TECH_INFO             (all application servers, result in table)
*     /SDF/SCSI_GET_HARDWARE_INFO             (current application server, result in table)

      data: L_DEST    type RFCDEST,
            L_INFO    type SMSYHWINFO,
            LT_INFO   type table of SMSYHWINFO,
            L_MSG(80) type C,
            L_TEXT    type STRING.

      call function 'AGSNO_GET_READ_RFC'
        exporting
          SYSTEM_NAME = L_SYSTEM_NAME
        importing
          RFC         = L_DEST.
      check L_DEST is not initial.

      call function '/SDF/SCSI_GET_HARDWARE_INFO'
        destination L_DEST
        tables
          INFO                  = LT_INFO
        exceptions
          SYSTEM_FAILURE        = 1 message L_MSG
          COMMUNICATION_FAILURE = 2 message L_MSG
          others                = 3.
      if SY-SUBRC ne 0.
        concatenate L_SYSTEM_NAME ':' L_MSG into L_TEXT separated by SPACE. "#EC NOTEXT
        CL_AGSNO_LOGGER=>WRITE_TEXT(
          IV_SUBOBJ = CL_AGSNO_LOGGER=>C_SUBOBJECT_CHECK
          IV_MSGTY = 'W'
          IV_MSGTXT = L_TEXT ).
      endif.
      check SY-SUBRC = 0.

      loop at LT_INFO into L_INFO.
        case L_INFO-DESCRIPTOR.
*         System -> Status: Host
          when 'operating system'.    "Operating system
          when 'machine type'.        "Machine type
*          when ''.                    "Server name
          when 'SAP system id'.       "Platform ID

*         System -> Status: Database data
          when 'database system'.     "Database System
*          when ''.                    "Release
          when 'database host'.       "Host
          when 'database owner'.      "Owner

*         System -> Status: Kernel information
          when 'kernel release'.      "Kernel release
            LS_SYS_KERN_REL-KERN_REL        = L_INFO-VALUE_1.
          when 'kernel compiled'.     "Compilation
          when 'kernel patch level'.  "Sup.Pkg lvl.
            LS_SYS_KERN_REL-KERN_PATCHLEVEL = L_INFO-VALUE_1.
          when 'ABAP load version'.   "ABAP load
          when 'CUA load version'.    "CUA load
          when 'kernel kind'.         "Mode
          when 'rsyn'.                "Rsyn file

*         System -> Status: Database information
          when 'database library'.    "DB client lib.
          when 'supported database'.  "supported DB releases
*          when ''.                    "DBSL version
*          when ''.                    "DBSL patch level

*         System -> Status: System information
          when 'IP address'.          "IP address
          when 'SAP version'.         "SAP version
          when 'valid OP system'.     "supported Operating system
          when 'OP system release'.   "OP release
*          when ''.                    "SNC name

*         other fields
          when 'database name'.       "System id
          when 'supported SAP vers'.  "supported SAP release
          when 'node name'.           "host name of ?
          when 'relinfo'.             "constant 'valid'
          when 'hot package level'.   "?
          when 'db release'.          "SAP release DB
        endcase.
      endloop.

    enddo. " Trick to use CHECK instead of nested IFs

*   Store new entry
    insert LS_SYS_KERN_REL into ST_SYS_KERN_REL index l_tabix.
    assign LS_SYS_KERN_REL to <LS_SYS_KERN_REL>.
  endif.

* Now we have it
  L_KERN_REL        = <LS_SYS_KERN_REL>-KERN_REL.
  L_KERN_PATCHLEVEL = <LS_SYS_KERN_REL>-KERN_PATCHLEVEL.

endform. " GET_KERNEL_REL

*----------------------------------------------------------------------*

form SHOW_NOTELIST.

  data LS_NOTELIST         type TS_NOTELIST.

  loop at GT_NOTELIST into LS_NOTELIST.
    write: /     LS_NOTELIST-SYSTEM_NAME  color col_group,
                 LS_NOTELIST-SYSTEM_TYPE  color col_group,

                 LS_NOTELIST-NOTE_NUMBER  color col_key,
                 LS_NOTELIST-NOTE_VERSION,
                 LS_NOTELIST-RELEASE_DATE,
                 LS_NOTELIST-SHORT_TEXT,

           /     LS_NOTELIST-THEMK,
                 LS_NOTELIST-THEMK_TEXT,

           /     LS_NOTELIST-PRIORITY_ID,
                 LS_NOTELIST-PRIORITY,

           /     LS_NOTELIST-CATEGORY_ID,
                 LS_NOTELIST-CATEGORY,

           /     LS_NOTELIST-SEC_CATEGORY,
                 LS_NOTELIST-SEC_CATEGORY_TEXT,
*                 ls_notelist-SEC_CATEGORY_LTEXT,

           /     LS_NOTELIST-DISPLAY_URL,

*                 ls_notelist-sw_comp_tbl,

           /     LS_NOTELIST-STATUS,
                 LS_NOTELIST-STATUS_TEXT,
*                 ls_notelist-status_ltext ,

                 LS_NOTELIST-USER,

           /     LS_NOTELIST-SSTATUS,
                 LS_NOTELIST-SSTATUS_TEXT,
*                 ls_notelist-Sstatus_ltext,

                 LS_NOTELIST-SUSER,

           /     LS_NOTELIST-PROC_STATUS,
                 LS_NOTELIST-IMPL_STATUS,
                 LS_NOTELIST-IMPL_NOTE_VERSION,

           /     LS_NOTELIST-AUTO,
                 LS_NOTELIST-M,
                 LS_NOTELIST-PRE,
                 LS_NOTELIST-POST,
                 LS_NOTELIST-MANUAL,
                 LS_NOTELIST-SPN,
                 LS_NOTELIST-IS_KERNEL,
                 LS_NOTELIST-IS_INDEP,
                 LS_NOTELIST-REQUIRED_NOTES,
                 LS_NOTELIST-SIDEEFFECT_NOTES,

           /     LS_NOTELIST-NOTE_TYPE_S, " Security
                 LS_NOTELIST-NOTE_TYPE_H, " HotNews
                 LS_NOTELIST-NOTE_TYPE_P, " Performance
                 LS_NOTELIST-NOTE_TYPE_L, " Legal
                 LS_NOTELIST-NOTE_TYPE_C. " Correction

  endloop.

endform. " form show_notelist.

*---------------------------------------------------------------------*
*      CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
class LCL_HANDLE_EVENTS definition.

  public section.

    methods:
      ON_USER_COMMAND for event ADDED_FUNCTION of CL_SALV_EVENTS
        importing E_SALV_FUNCTION,

      ON_DOUBLE_CLICK for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
        importing ROW COLUMN.

*      on_single_click for event link_click of cl_salv_events_table
*        importing row column.

  private section.
    data DIALOGBOX_STATUS type C.  "'X': does exist, SPACE: does not ex.

endclass.                    "lcl_handle_events DEFINITION

* main data table
data: GR_ALV_TABLE      type ref to CL_SALV_TABLE.

* for handling the events of cl_salv_table
data: GR_ALV_EVENTS     type ref to LCL_HANDLE_EVENTS.

*----------------------------------------------------------------------*
*      CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
* implement the events for handling the events of cl_salv_table
*----------------------------------------------------------------------*
class LCL_HANDLE_EVENTS implementation.

  method ON_USER_COMMAND.
*    importing e_salv_function

    data: L_PROGNAME_NXS  type SY-REPID,
          LS_NOTELIST     type TS_NOTELIST,
          LR_SELECTIONS   type ref to CL_SALV_SELECTIONS,
          LS_CELL         type SALV_S_CELL,
          LT_SELECED_ROWS type SALV_T_ROW,
          L_ROW           type I.

*   Get selected item
    LR_SELECTIONS   = GR_ALV_TABLE->GET_SELECTIONS( ).
    LS_CELL         = LR_SELECTIONS->GET_CURRENT_CELL( ).
    LT_SELECED_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).

    case E_SALV_FUNCTION.

      when 'PICK'. " Double click
        if LS_CELL-ROW > 0.
          clear LS_NOTELIST.
          read table GT_NOTELIST into LS_NOTELIST index LS_CELL-ROW.
          if SY-SUBRC = 0 and LS_NOTELIST-NOTE_NUMBER is not initial.
            perform GET_PROGNAME_NXS changing L_PROGNAME_NXS.
            if SY-SUBRC = 0 and LS_NOTELIST-NOTE_NUMBER is not initial.
              perform DISPLAY_NOTE_TEXT in program (L_PROGNAME_NXS)
                using LS_NOTELIST-NOTE_NUMBER
                      "SPACE " not valid in ST-A/PI Release 01T_731, SP 1
                .
            endif.
          endif.
        endif.

      when 'STATUS'. " Change status and add comment

* get 1st selected row
  read table LT_SELECED_ROWS into L_ROW index 1.
  check sy-subrc = 0.
  read table GT_NOTELIST into LS_NOTELIST index L_ROW.

  data: l_cursorline type i,
        l_textline1(132),
        l_textline2(132),
        l_textline3(132),
        ls_AGSSR_STATUS type AGSSR_STATUS,
        L_STATUS_TEXT   type AGSNOTE_STATUS_TEXT,
        ls_SPOPLI type SPOPLI,

        l_answer(8),
        l_index type i,
        l_sel_cnt type i.

  data: "statics: "(not possible in method)
        lt_AGSSR_STATUS type table of AGSSR_STATUS,
        lt_SPOPLI type table of SPOPLI.

* Prepare status popup
  l_cursorline = 1.
  l_textline1  = ''.
  l_textline2  = ''.
  l_textline3  = ''.

  l_sel_cnt = lines( LT_SELECED_ROWS ).
  if l_sel_cnt = 1.
    l_textline1 = '1 entry selected'(025).
    l_textline1 = 'Current status:'(026).
    concatenate LS_NOTELIST-STATUS LS_NOTELIST-STATUS_TEXT
      into l_textline2 SEPARATED BY space.
  elseif l_sel_cnt > 1.
    l_ANSWER = l_sel_cnt.
    concatenate l_answer 'entries selected'(027)
      into l_textline1 SEPARATED BY space.
  else.
    RETURN. "nothing selected
  endif.

* Get status list
  if lt_AGSSR_STATUS is initial.
    select *
      from AGSSR_STATUS
      into table  @lt_AGSSR_STATUS
      where langu = @sy-langu
      ORDER BY PRIMARY KEY.

    clear lt_SPOPLI.
    loop at lt_AGSSR_STATUS into ls_AGSSR_STATUS.
      if LS_NOTELIST-STATUS = ls_AGSSR_STATUS-STATUS_ID.
        l_cursorline = sy-tabix.
      endif.

      if ls_AGSSR_STATUS-STATUS_TEXT is initial.
        ls_AGSSR_STATUS-STATUS_TEXT = ls_AGSSR_STATUS-STATUS_ID.
        modify lt_AGSSR_STATUS from ls_AGSSR_STATUS index sy-tabix.
      endif.

      clear ls_SPOPLI.
      concatenate ls_AGSSR_STATUS-STATUS_ID ls_AGSSR_STATUS-STATUS_TEXT
        into ls_SPOPLI-VAROPTION SEPARATED BY space.
      append ls_SPOPLI to lt_SPOPLI.

    endloop.
  endif.

* Ask for new status
  clear l_answer.
  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
    EXPORTING
      CURSORLINE               = l_CURSORLINE
      MARK_FLAG                = ' ' " space: radio button
      MARK_MAX                 = 1
*     START_COL                = 0
*     START_ROW                = 0
      TEXTLINE1                = l_textline1
      TEXTLINE2                = l_textline2
      TEXTLINE3                = l_textline3
      TITEL                    = 'Change status'(028)
*     DISPLAY_ONLY             = ' '
    IMPORTING
      ANSWER                   = l_ANSWER
    TABLES
      T_SPOPLI                 = lt_SPOPLI
    EXCEPTIONS
      NOT_ENOUGH_ANSWERS       = 1
      TOO_MUCH_ANSWERS         = 2
      TOO_MUCH_MARKS           = 3
      OTHERS                   = 4
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
    RETURN. " Cancel status maintenance
  ENDIF.
  check l_ANSWER ne 'A'. " cancel
  l_index = l_answer.
  read table lt_AGSSR_STATUS into ls_AGSSR_STATUS index l_index.
  check sy-subrc = 0. " Cancel status maintenance

* Ask for comment
  data: l_COMMENT_TEXT TYPE  STRING,
        l_OKAY         TYPE  ABAP_BOOL.
  CALL FUNCTION 'POPUP_GET_STRING'
    EXPORTING
      LABEL         = 'Comment'(029)
    IMPORTING
      VALUE         = l_COMMENT_TEXT
      OKAY          = l_OKAY
            .
  check l_OKAY = 'X'. " Cancel status maintenance

* Store new current status
  data: ls_SYSTEM_NOTE TYPE  AGS_SR_S_CRM_SYSTEM_NOTE,
        lt_SYSTEM_NOTE TYPE  AGS_SR_T_CRM_SYSTEM_NOTE.

  data: L_CREATED_AT type AGSSR_SYSNOTEC-CREATED_AT,
        L_DATE       type SY-DATUM,
        L_DATEC(10)  type C,
        L_TIME       type SY-UZEIT,
        L_TIMEC(8)   type C.
  GET TIME STAMP FIELD L_CREATED_AT.
  convert time stamp L_CREATED_AT
    time zone SY-ZONLO
    into date L_DATE
         time L_TIME.
  write L_DATE to L_DATEC dd/mm/yyyy.
  write L_TIME to L_TIMEC using edit mask '__:__:__'.

  clear lt_SYSTEM_NOTE.
  loop at LT_SELECED_ROWS into L_ROW.
    read table GT_NOTELIST into LS_NOTELIST index L_ROW.
    check sy-subrc = 0.

*   Update ALV list
    LS_NOTELIST-STATUS         = ls_AGSSR_STATUS-STATUS_ID.
    LS_NOTELIST-STATUS_TEXT    = ls_AGSSR_STATUS-STATUS_TEXT.
    LS_NOTELIST-USER           = sy-uname.
    LS_NOTELIST-LATEST_COMMENT = l_COMMENT_TEXT.
    concatenate L_DATEC L_TIMEC into LS_NOTELIST-COMMENT_CREATED_AT
      separated by SPACE.
    modify GT_NOTELIST from LS_NOTELIST index L_ROW.

*   Prepare storing comment
    ls_SYSTEM_NOTE-SYSTEM_ID   = ''. "not used
    ls_SYSTEM_NOTE-SYSTEM_NAME = LS_NOTELIST-system_name.
    ls_SYSTEM_NOTE-SYSTEM_TYPE = LS_NOTELIST-system_type.
    ls_SYSTEM_NOTE-NOTE_NUMBER = LS_NOTELIST-note_number.
    append ls_SYSTEM_NOTE to lt_SYSTEM_NOTE.

*   Store new current status if neccessay
*   New fields as of SolMan 7.2 SP 7 (see Class CL_AGS_SYSREC_DPC_EXT Method SYSTEMNOTECOMMEN_CREATE_ENTITY ):
*   LAST_SSTATUS
*   LAST_SUSER
*   FLAG_REVIEW
*   OLD_SSTATUS
    DATA: ls_sysnotes TYPE agssr_sysnotes.
    select single * from agssr_sysnotes into @ls_sysnotes
      where system_name = @ls_NOTELIST-system_name
        and system_type = @ls_NOTELIST-system_type
        and note_number = @ls_NOTELIST-note_number.
    if sy-subrc = 0 and ls_sysnotes-last_status <> ls_AGSSR_STATUS-STATUS_ID.
      ls_sysnotes-last_status = ls_AGSSR_STATUS-STATUS_ID.
      ls_sysnotes-last_user   = sy-uname.
      MODIFY agssr_sysnotes FROM ls_sysnotes.
    endif.
  endloop.

*  Store comment with history of status
*  CALL FUNCTION 'AGSNO_CREATE_COMMENT'
*    EXPORTING
*      SYSTEM_NOTE        = lt_SYSTEM_NOTE
*      COMMENT_TEXT       = l_COMMENT_TEXT
*            .
  PERFORM agsno_create_comment
    using
      lt_SYSTEM_NOTE
      ls_AGSSR_STATUS-STATUS_ID
      l_COMMENT_TEXT
      L_CREATED_AT
      .

* Update ALV list
  if sy-subrc = 0.
    gr_ALV_TABLE->REFRESH( refresh_mode = if_salv_c_refresh=>soft ).
  endif.

    endcase.

  endmethod.                    "on_user_command

  method ON_DOUBLE_CLICK.
*   importing row column

    data: L_PROGNAME_NXS type        SY-REPID,
          LS_NOTELIST    type        TS_NOTELIST.
*   DATA: lr_Selections  type ref to cl_Salv_selections,
*         ls_cell        type        salv_s_cell.

*   Get selected item
*   lr_selections = gr_ALV_TABLE->get_selections( ).
*   ls_cell = lr_selections->get_current_cell( ).

    if ROW > 0.
      read table GT_NOTELIST into LS_NOTELIST index ROW.
      if SY-SUBRC = 0 and LS_NOTELIST-NOTE_NUMBER is not initial.
        perform GET_PROGNAME_NXS changing L_PROGNAME_NXS.
        perform DISPLAY_NOTE_TEXT in program (L_PROGNAME_NXS)
          using LS_NOTELIST-NOTE_NUMBER
                "SPACE " not valid in ST-A/PI Release 01T_731, SP 1
          .
      endif.
    endif.

  endmethod.                    "on_double_click

endclass.                    "lcl_handle_events IMPLEMENTATION


FORM agsno_create_comment
  using
    SYSTEM_NOTE  TYPE  AGS_SR_T_CRM_SYSTEM_NOTE
    STATUS       TYPE  AGSSR_STATUS-STATUS_ID
    COMMENT_TEXT TYPE  STRING
    CREATED_AT   type  AGSSR_SYSNOTEC-CREATED_AT
    .
*FUNCTION agsno_create_comment.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(SYSTEM_NOTE) TYPE  AGS_SR_T_CRM_SYSTEM_NOTE
*"     REFERENCE(COMMENT_TEXT) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: ls_sysnotec            TYPE agssr_sysnotec.

  LOOP AT system_note ASSIGNING field-symbol(<fs_system_note>).

    ls_sysnotec-system_name = <fs_system_note>-system_name.

    ls_sysnotec-system_type = <fs_system_note>-system_type.

    ls_sysnotec-note_number = <fs_system_note>-note_number.

    ls_sysnotec-comment_id = cl_ags_sysrec_util=>create_guid( ).

    ls_sysnotec-created_by  = sy-uname.

*    GET TIME STAMP FIELD ls_sysnotec-created_at.
    ls_sysnotec-created_at = CREATED_AT.

    DATA lt_sysnotec  TYPE TABLE OF agssr_sysnotec.
    DATA ls_sysnotect TYPE agssr_sysnotec.

    SELECT * FROM agssr_sysnotec INTO TABLE @lt_sysnotec
      WHERE system_name = @ls_sysnotec-system_name
        AND system_type = @ls_sysnotec-system_type
        AND note_number = @ls_sysnotec-note_number
      ORDER BY created_at DESCENDING.
    IF sy-subrc <> 0.
      ls_sysnotec-status_from = 'NEW'.
    ELSE.
      READ TABLE lt_sysnotec INDEX 1 INTO ls_sysnotect.
      ls_sysnotec-status_from = ls_sysnotect-status_to.
    ENDIF.

*    ls_sysnotec-status_to = ls_sysnotec-status_from.
    ls_sysnotec-status_to = STATUS.

    ls_sysnotec-comment_text = comment_text.

    INSERT agssr_sysnotec FROM @ls_sysnotec.

  ENDLOOP.

*ENDFUNCTION.
ENDFORM. " agsno_create_comment

form GET_PROGNAME_NXS
  changing L_PROGNAME_NXS type SY-REPID.

  statics LF_PROGNAME_NXS type SY-REPID.

  if LF_PROGNAME_NXS is initial.
*   Check if ST-A/PI is installed
    perform REGISTRY_GET_SUBROUTINE_PROG(RDSVAS_SSFSAIF) if found
      using 'NXS'
            'NXS_ONLINEREC_SERVICE_EXECUTE'
      changing L_PROGNAME_NXS.
  else.
    L_PROGNAME_NXS = LF_PROGNAME_NXS.
  endif.
endform.                    "get_progname_NXS

form ALV_SHOW_NOTELIST.

* reference to ALV objects
  data: LR_FUNCTIONS_LIST      type ref to CL_SALV_FUNCTIONS_LIST,
*        lr_functions           TYPE REF TO cl_salv_functions,
        LR_SELECTIONS          type ref to CL_SALV_SELECTIONS,
        LR_COLUMNS             type ref to CL_SALV_COLUMNS_TABLE,
*        lr_column              TYPE REF TO cl_salv_column_table,
*        lr_sorts               TYPE REF TO cl_salv_sorts.
        LR_EVENTS              type ref to CL_SALV_EVENTS_TABLE,
        LR_FUNCTIONAL_SETTINGS type ref to CL_SALV_FUNCTIONAL_SETTINGS,
        LR_HYPERLINKS          type ref to CL_SALV_HYPERLINKS,
        LR_TOOLTIPS            type ref to CL_SALV_TOOLTIPS,
        LR_LAYOUT              type ref to CL_SALV_LAYOUT,
        LS_KEY                 type SALV_S_LAYOUT_KEY,
*        lr_content             TYPE REF TO cl_salv_form_element,
        LR_GRID_HEADER         type ref to CL_SALV_FORM_LAYOUT_GRID,
        LR_GRID_FOOTER         type ref to CL_SALV_FORM_LAYOUT_GRID,
        LR_DISPLAY_SETTINGS    type ref to CL_SALV_DISPLAY_SETTINGS.

  data: LR_EXCEPTION type ref to CX_SALV_ERROR,
        LV_MESSAGE   type BAL_S_MSG.

  data: LR_COLUMN type ref to CL_SALV_COLUMN_LIST,
        LR_SORTS  type ref to CL_SALV_SORTS.

  data: LS_COLOR_SID  type LVC_S_COLO,
        LS_COLOR_NOTE type LVC_S_COLO.

  data LS_URLS_FOR_NOTES type TS_URLS_FOR_NOTES.

* Create an ALV table for grid display
  try.
      CL_SALV_TABLE=>FACTORY(
        importing
          R_SALV_TABLE = GR_ALV_TABLE
        changing

          T_TABLE      = GT_NOTELIST ).

    catch CX_SALV_MSG
          into LR_EXCEPTION.
      LV_MESSAGE = LR_EXCEPTION->GET_MESSAGE( ).
      message id LV_MESSAGE-MSGID type LV_MESSAGE-MSGTY
              number LV_MESSAGE-MSGNO
              with LV_MESSAGE-MSGV1 LV_MESSAGE-MSGV2
                   LV_MESSAGE-MSGV3 LV_MESSAGE-MSGV4.
  endtry.

  data L_REPID like SY-REPID.
  L_REPID = SY-REPID.
  call function 'RS_CUA_STATUS_CHECK'
    exporting
      OBJECTNAME       = 'SALV_TABLE_STANDARD'
      PROGRAM          = L_REPID
    exceptions
      OBJECT_NOT_FOUND = 1
      others           = 2.

  if SY-SUBRC = 0.
    GR_ALV_TABLE->SET_SCREEN_STATUS(
      REPORT        = L_REPID
      PFSTATUS      = 'SALV_TABLE_STANDARD'
      SET_FUNCTIONS = CL_SALV_TABLE=>C_FUNCTIONS_DEFAULT
      ).

  endif.

* set ALV generic funtions of class CL_SALV_FUNCTIONS_LIST
  LR_FUNCTIONS_LIST = GR_ALV_TABLE->GET_FUNCTIONS( ).
*  lr_functions->SET_DEFAULT( if_salv_c_bool_sap=>true ).
  LR_FUNCTIONS_LIST->SET_DETAIL( IF_SALV_C_BOOL_SAP=>TRUE ).
  LR_FUNCTIONS_LIST->SET_GROUP_EXPORT( IF_SALV_C_BOOL_SAP=>TRUE ).
  LR_FUNCTIONS_LIST->SET_GROUP_FILTER( IF_SALV_C_BOOL_SAP=>TRUE ).
  LR_FUNCTIONS_LIST->SET_GROUP_LAYOUT( IF_SALV_C_BOOL_SAP=>TRUE ).
  LR_FUNCTIONS_LIST->SET_PRINT( IF_SALV_C_BOOL_SAP=>TRUE ).
  LR_FUNCTIONS_LIST->SET_PRINT_PREVIEW( IF_SALV_C_BOOL_SAP=>TRUE ).
  LR_FUNCTIONS_LIST->SET_GROUP_SORT( IF_SALV_C_BOOL_SAP=>TRUE ).
  LR_FUNCTIONS_LIST->SET_FIND( IF_SALV_C_BOOL_SAP=>TRUE ).
  LR_FUNCTIONS_LIST->SET_GRAPHICS( IF_SALV_C_BOOL_SAP=>FALSE ).

* Set the layout
  LR_LAYOUT = GR_ALV_TABLE->GET_LAYOUT( ).
  LS_KEY-REPORT = SY-REPID.
  LR_LAYOUT->SET_KEY( LS_KEY ).
  LR_LAYOUT->SET_INITIAL_LAYOUT( P_LAYOUT ).
  authority-check object 'S_ALV_LAYO'
                      id 'ACTVT' field '23'.
  if SY-SUBRC = 0.
    LR_LAYOUT->SET_SAVE_RESTRICTION( 3 ). "no restictions
  else.
    LR_LAYOUT->SET_SAVE_RESTRICTION( 2 ). "user dependend
  endif.

* Set the columns visible
  LR_COLUMNS = GR_ALV_TABLE->GET_COLUMNS( ).
  LR_COLUMNS->SET_OPTIMIZE( IF_SALV_C_BOOL_SAP=>TRUE ).

* Set the fields description and field attributes

  LS_COLOR_SID-COL  = COL_KEY.
  LS_COLOR_NOTE-COL = COL_TOTAL.

* Sort Columns
  LR_SORTS = GR_ALV_TABLE->GET_SORTS( ).
  LR_SORTS->CLEAR( ).
  try.
      LR_SORTS->ADD_SORT(
        COLUMNNAME = 'SYSTEM_NAME'
        POSITION   = 1
        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP ).
      LR_SORTS->ADD_SORT(
        COLUMNNAME = 'SYSTEM_TYPE'
        POSITION   = 2
        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP ).
      LR_SORTS->ADD_SORT(
        COLUMNNAME = 'NOTE_NUMBER'
        POSITION   = 3
        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP ).

    catch CX_SALV_NOT_FOUND
          CX_SALV_EXISTING
          CX_SALV_DATA_ERROR
          into LR_EXCEPTION.
      LV_MESSAGE = LR_EXCEPTION->GET_MESSAGE( ).
      message id LV_MESSAGE-MSGID type LV_MESSAGE-MSGTY
              number LV_MESSAGE-MSGNO
              with LV_MESSAGE-MSGV1 LV_MESSAGE-MSGV2
                   LV_MESSAGE-MSGV3 LV_MESSAGE-MSGV4.
  endtry.

* Methods of class CL_SALV_COLUMN
* SET_SHORT_TEXT          Set Short Column Title (10 chars)
* SET_MEDIUM_TEXT         Set Medium Column Title (20 chars)
* SET_LONG_TEXT           Set Long Column Title (40 chars)
* SET_TOOLTIP             Set Tooltip for Column Title (40 chars)
* SET_ROW                 Set Row for Multirow Display
*
* SET_ALIGNMENT             Set Alignment of Column
* SET_CURRENCY            Set Currency for Whole Column
* SET_CURRENCY_COLUMN     Set Currency Column
* SET_DDIC_REFERENCE      Set DDIC Reference
* SET_DECIMALS_COLUMN   Set Decimal Column for Number of Decimal Places
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
* SET_KEY                   Set Column as Key Column
*
* SET_CELL_TYPE             Set Cell Type
* SET_DROPDOWN_ENTRY      Set Handle for Dropdown
* SET_HYPERLINK_ENTRY       Set Handle for Dropdown
* SET_F4                  Set Column with F1 Help
* SET_F4_CHECKTABLE       Set Check Table for F4 Help
* SET_KEY_PRESENCE_REQUIRED Set Key Columns as Always Visible
* SET_SYMBOL              Set Column as Symbol Column
* SET_TEXT_COLUMN           Set Text Column

  try.
*     System identification
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SYSTEM_NAME' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SID'(S01) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'System ID'(m01) ).
      LR_COLUMN->SET_LONG_TEXT( 'System identification'(l01) ).
      LR_COLUMN->SET_COLOR( LS_COLOR_SID ).
*      lr_column->set_key( if_salv_c_bool_sap=>true ).

*     System type
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SYSTEM_TYPE' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Sys-Type'(S02) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'System type'(m02) ).
      LR_COLUMN->SET_LONG_TEXT( 'System type'(l02) ).
      LR_COLUMN->SET_COLOR( LS_COLOR_SID ).

*     System Role
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SYSTEM_ROLE' ).
      LR_COLUMN->SET_SHORT_TEXT( 'ITA Role'(S42) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'IT Admin Role'(m42) ).
      LR_COLUMN->SET_LONG_TEXT( 'System Role in IT Administration'(l42) ).
      LR_COLUMN->SET_COLOR( LS_COLOR_SID ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

*     System Role Text
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SYSTEM_ROLE_TEXT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'ITA Role'(S43) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'IT Admin Role'(m43) ).
      LR_COLUMN->SET_LONG_TEXT( 'Text of System Role in IT Administration'(l43) ).
      LR_COLUMN->SET_COLOR( LS_COLOR_SID ).

*     System Priority
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SYSTEM_PRIORITY' ).
      LR_COLUMN->SET_SHORT_TEXT( 'ITA Prio'(S44) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'IT Admin Priority'(m44) ).
      LR_COLUMN->SET_LONG_TEXT( 'System Priority'(l44) ).
      LR_COLUMN->SET_COLOR( LS_COLOR_SID ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

*     System Priority Text
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SYSTEM_PRIORITY_TEXT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'ITA Prio'(S45) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'IT Admin Priority'(m45) ).
      LR_COLUMN->SET_LONG_TEXT( 'Text of System Priority in IT Admin.'(l45) ).
      LR_COLUMN->SET_COLOR( LS_COLOR_SID ).

*     Note number
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'NOTE_NUMBER' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Note'(S03) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Note'(m03) ).
      LR_COLUMN->SET_LONG_TEXT( 'Note'(l03) ).
      LR_COLUMN->SET_COLOR( LS_COLOR_NOTE ).

*     Note version
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'NOTE_VERSION' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Version'(S04) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Note version'(m04) ).
      LR_COLUMN->SET_LONG_TEXT( 'Note version'(l04) ).
      LR_COLUMN->SET_COLOR( LS_COLOR_NOTE ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

*     Note short text
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SHORT_TEXT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Short text'(S11) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Note short text'(m11) ).
      LR_COLUMN->SET_LONG_TEXT( 'Note short text'(l11) ).

*     Application area
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'THEMK' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Appl.area'(S05) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Application area'(m05) ).
      LR_COLUMN->SET_LONG_TEXT( 'Application area'(l05) ).

*     Application area text
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'THEMK_TEXT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Appl.area'(S06) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Application area'(m06) ).
      LR_COLUMN->SET_LONG_TEXT( 'Application area text'(l06) ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

*     Priority
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'PRIORITY_ID' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Priority'(S07) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Priority'(m07) ).
      LR_COLUMN->SET_LONG_TEXT( 'Priority'(l07) ).

*     Priority text
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'PRIORITY' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Priority'(S08) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Priority'(m08) ).
      LR_COLUMN->SET_LONG_TEXT( 'Priority text'(l08) ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

*     Category
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'CATEGORY_ID' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Category'(S09) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Category'(m09) ).
      LR_COLUMN->SET_LONG_TEXT( 'Category'(l09) ).

*     Category text
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'CATEGORY' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Category'(S10) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Category'(m10) ).
      LR_COLUMN->SET_LONG_TEXT( 'Category text'(l10) ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

*     Link
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'DISPLAY_URL' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Note URL'(s28) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Link to Note in SMP'(m28) ).
      LR_COLUMN->SET_LONG_TEXT( 'Link to note in the Service Marketplace'(l28) ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
*      LR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
*      LR_COLUMN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>LINK ).
*      lr_column->SET_HYPERLINK_ENTRY( handle ).

      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'NOTEURL' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Note URL'(s28) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Link to Note in SMP'(m28) ).
      LR_COLUMN->SET_LONG_TEXT( 'Link to note in the Service Marketplace'(l28) ).

*     Note release date
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'RELEASE_DATE' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Rel.date'(S12) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Note release date'(m12) ).
      LR_COLUMN->SET_LONG_TEXT( 'Note release date'(l12) ).


*     SAP-Status
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SSTATUS' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SAP Status'(s66) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'SAP SysRec Status'(m66) ).
      LR_COLUMN->SET_LONG_TEXT( 'SAP System Recommendations Status'(l66) ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

*     SAP-Status
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SSTATUS_TEXT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SAP Status'(s66) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'SAP SysRec Status'(m66) ).
      LR_COLUMN->SET_LONG_TEXT( 'SAP System Recommendations Status Text'(l66) ).

*     SAP-User
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SUSER' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SAP User'(s67) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'SAP User'(m67) ).
      LR_COLUMN->SET_LONG_TEXT( 'SAP User'(l67) ).


*     Status
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'STATUS' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SysRecStat'(S13) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'SysRec Status'(m13) ).
      LR_COLUMN->SET_LONG_TEXT( 'System Recommendations Status'(l13) ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

*     Status
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'STATUS_TEXT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SysRecStat'(S13) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'SysRec Status'(m13) ).
      LR_COLUMN->SET_LONG_TEXT( 'System Recommendations Status Text'(l13) ).

*     User
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'USER' ).
      LR_COLUMN->SET_SHORT_TEXT( 'User'(S14) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'User'(m14) ).
      LR_COLUMN->SET_LONG_TEXT( 'User'(l14) ).

*     Latest comment created at
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'COMMENT_CREATED_AT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'DateComm.'(S40) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Date of comment'(m40) ).
      LR_COLUMN->SET_LONG_TEXT( 'Date of comment'(l40) ).

*     Latest comment
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'LATEST_COMMENT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Comment'(S41) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Latest comment'(m41) ).
      LR_COLUMN->SET_LONG_TEXT( 'Latest comment'(l41) ).


*     User: Status
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'USER_STATUS' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SysRecStat'(S50) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'SysRec Status'(m50) ).
      LR_COLUMN->SET_LONG_TEXT( 'User: System recommendations Status'(l50) ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

*     User: Status
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'USER_STATUS_TEXT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SysRecStat'(S51) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'User: SysRec Status'(m51) ).
      LR_COLUMN->SET_LONG_TEXT( 'User: System recommendations Status'(l51) ).

*     User: User
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'USER_COMMENT_BY' ).
      LR_COLUMN->SET_SHORT_TEXT( 'User'(S52) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'User: User'(m52) ).
      LR_COLUMN->SET_LONG_TEXT( 'User'(l52) ).

*     User: Latest comment created at
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'USER_COMMENT_CREATED_AT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'DateComm.'(S53) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'User:Date of comment'(m53) ).
      LR_COLUMN->SET_LONG_TEXT( 'User: Date of comment'(l53) ).

*     User: Latest comment
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'USER_LATEST_COMMENT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Comment'(S54) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'User: Latest comment'(m54) ).
      LR_COLUMN->SET_LONG_TEXT( 'User: Latest comment'(l54) ).


*     Processing status from managed system
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'PROC_STATUS' ).
      LR_COLUMN->SET_SHORT_TEXT( 'ProcStatus'(S47) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Proc. status'(m47) ).
      LR_COLUMN->SET_LONG_TEXT( 'Processing status in managed system'(l47) ).
      if P_RFC is initial.
        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
      endif.

*     Implementation status from managed system
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'IMPL_STATUS' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Imp.status'(S15) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Impl. status'(m15) ).
      LR_COLUMN->SET_LONG_TEXT( 'Implementation status in managed system'(l15) ).
      if P_RFC is initial.
        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
      endif.

*     Implementad note version from managed system
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'IMPL_NOTE_VERSION' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Imp.vers.'(S16) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Impl. version'(m16) ).
      LR_COLUMN->SET_LONG_TEXT( 'Impl. note version in managed system'(l16) ).
      if P_RFC is initial.
        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
      endif.

*     Implementation status
*        lr_column ?= lr_columns->get_column( 'IMPLEMENTATION_STATUS' ).
*        lr_column->set_short_text( 'Imp.status'(S15) ).
*        lr_column->set_medium_text( 'Impl. status'(m15) ).
*        lr_column->set_long_text( 'Implementation status'(l15) ).

*     Software component version
*        lr_column ?= lr_columns->get_column( 'COMPVERS' ).
*        lr_column->set_short_text( 'SWCompVers'(S17) ).
*        lr_column->set_medium_text( 'SW component version'(m17) ).
*        lr_column->set_long_text( 'Software component version'(l17) ).
*        lr_column->SET_VISIBLE( if_salv_c_bool_sap=>false ).

*     Software Component
*        lr_column ?= lr_columns->get_column( 'COMPONENT' ).
*        lr_column->set_short_text( 'SW Comp.'(S20) ).
*        lr_column->set_medium_text( 'Software Component'(m20) ).
*        lr_column->set_long_text( 'Software Component'(l20) ).

*     Release
*        lr_column ?= lr_columns->get_column( 'SAPRELEASE' ).
*        lr_column->set_short_text( 'Release'(S21) ).
*        lr_column->set_medium_text( 'Release'(m21) ).
*        lr_column->set_long_text( 'Release'(l21) ).

*     Support Package
*        lr_column ?= lr_columns->get_column( 'CV_SPLVL' ).
*        lr_column->set_short_text( 'SP'(S18) ).
*        lr_column->set_medium_text( 'Support Package'(m18) ).
*        lr_column->set_long_text( 'Support Package'(l18) ).

*     Patch
*        lr_column ?= lr_columns->get_column( 'CV_PATCHLVL' ).
*        lr_column->set_short_text( 'Patch'(S19) ).
*        lr_column->set_medium_text( 'Patch'(m19) ).
*        lr_column->set_long_text( 'Patch'(l19) ).

*     Switch Framework: Status of Software Component
*        lr_column ?= lr_columns->get_column( 'SWF_COMPONENT_STATUS' ).
*        lr_column->set_short_text( 'SWF Status'(S36) ).
*        lr_column->set_medium_text( 'SWF Status'(m36) ).
*        lr_column->set_long_text( 'Switch Framework Status'(l36) ).

*     Automatic correction instruction
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'AUTO' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Auto.corr.'(S30) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Automatic corr.'(M30) ).
      LR_COLUMN->SET_LONG_TEXT( 'Automatic correction instruction'(L30) ).

*     Manual correction instruction
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'M' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Man.corr.'(S31) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Manual correction'(M31) ).
      LR_COLUMN->SET_LONG_TEXT( 'Manual correction instruction'(L31) ).

*     Pre. manual correction instruction
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'PRE' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Pre.corr.'(S35) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Pre. correction'(M35) ).
      LR_COLUMN->SET_LONG_TEXT( 'Prerequisite correction instruction'(L35) ).

*     Post manual correction instruction
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'POST' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Post.corr.'(S36) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Post correction'(M36) ).
      LR_COLUMN->SET_LONG_TEXT( 'Post manual correction instruction'(L36) ).

*     Manual instruction
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'MANUAL' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Man.instr.'(S37) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Manual instruction'(M37) ).
      LR_COLUMN->SET_LONG_TEXT( 'Manual instruction'(L37) ).

*     Patch
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SPN' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SP Patch'(S32) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'SP with solution'(M32) ).
      LR_COLUMN->SET_LONG_TEXT( 'Support Package with solution'(L32) ).

*     Is SP independant
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'IS_INDEP' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SP indep.'(S22) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'SP independent'(m22) ).
      LR_COLUMN->SET_LONG_TEXT( 'Is Support Package independent'(l22) ).


*     Required Notes
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'REQUIRED_NOTES' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Req.Notes'(S46) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Required Notes'(m46) ).
      LR_COLUMN->SET_LONG_TEXT( 'Required Notes'(l46) ).
      if P_REQ is initial.
        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
      endif.

*     Side effect Notes
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SIDEEFFECT_NOTES' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SideEffect'(S55) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Side effect Notes'(m55) ).
      LR_COLUMN->SET_LONG_TEXT( 'Side effect solving Notes'(l55) ).
      if P_SD is initial.
        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
      endif.


*     Kernel
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'IS_KERNEL' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Kernel'(S34) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Kernel Note'(M34) ).
      LR_COLUMN->SET_LONG_TEXT( 'Kernel Note'(L34) ).


*     License Audit attribute values
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'AUDIT_ATTRIBUTE' ).
      LR_COLUMN->SET_SHORT_TEXT( 'LicAudit'(S56) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'License Audit Attr.'(m56) ).
      LR_COLUMN->SET_LONG_TEXT( 'License Audit attribute values'(l56) ).
      if SS_GRP6 is initial.
        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
      endif.

*     License Audit attribute value texts
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'AUDIT_ATTRIBUTE_TEXT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'LicAudit'(S57) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'License Audit Attr.'(m57) ).
      LR_COLUMN->SET_LONG_TEXT( 'License Audit attribute value texts'(l57) ).
      if SS_GRP6 is initial.
        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
      endif.

*     Security Note Category
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SEC_CATEGORY' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SecCat'(S38) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Security Category'(m38) ).
      LR_COLUMN->SET_LONG_TEXT( 'Security Note Category'(l38) ).

*     Security Note Category text
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'SEC_CATEGORY_TEXT' ).
      LR_COLUMN->SET_SHORT_TEXT( 'SecCat'(S39) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Security Category'(m39) ).
      LR_COLUMN->SET_LONG_TEXT( 'Security Note Category text'(l39) ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).


*     Group: Security Note
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'NOTE_TYPE_S' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Sec. Note'(S23) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Security Note'(m23) ).
      LR_COLUMN->SET_LONG_TEXT( 'Security Note'(l23) ).

*     Group: Hot News
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'NOTE_TYPE_H' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Hot News'(S24) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Hot News'(m24) ).
      LR_COLUMN->SET_LONG_TEXT( 'Hot News'(l24) ).

*     Group: Performance Note
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'NOTE_TYPE_P' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Perf. Note'(S25) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Performance Note'(m25) ).
      LR_COLUMN->SET_LONG_TEXT( 'Performance Note'(l25) ).

*     Group: Legal Change Note
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'NOTE_TYPE_L' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Legal Note'(S26) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Legal Change Note'(m26) ).
      LR_COLUMN->SET_LONG_TEXT( 'Legal Change Note'(l26) ).

*     Group: License Audit Note
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'NOTE_TYPE_A' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Lic.Audit'(S29) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'License Audit Note'(m29) ).
      LR_COLUMN->SET_LONG_TEXT( 'License Audit relevant Note'(l29) ).

*     Group: Correction Note
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'NOTE_TYPE_C' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Corr. Note'(S27) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Correction Note'(m27) ).
      LR_COLUMN->SET_LONG_TEXT( 'Correction Note'(l27) ).
*      if S_GROUP5 is initial.
*        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
*      endif.

*     Group: Java Patch Note
*        lr_column ?= lr_columns->get_column( 'GROUP6' ).
*        lr_column->set_short_text( 'Java Note'(S33) ).
*        lr_column->set_medium_text( 'Java Patch Note'(m33) ).
*        lr_column->set_long_text( 'Java Patch Note'(l33) ) .

*     Current Kernel Release
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'KERN_REL' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Kern. Rel.'(S48) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Kernel Release'(m48) ).
      LR_COLUMN->SET_LONG_TEXT( 'Current Kernel Release'(l48) ).
      if P_RFC is initial.
        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
      endif.

*     Current Kernel Patch Level
      LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'KERN_PATCHLEVEL' ).
      LR_COLUMN->SET_SHORT_TEXT( 'Kern.Patch'(S49) ).
      LR_COLUMN->SET_MEDIUM_TEXT( 'Kernel Patch Level'(m49) ).
      LR_COLUMN->SET_LONG_TEXT( 'Current Kernel Patch Level'(l49) ).
      if P_RFC is initial.
        LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
      endif.

    catch CX_SALV_NOT_FOUND
      into LR_EXCEPTION.
      LV_MESSAGE = LR_EXCEPTION->GET_MESSAGE( ).
      message id LV_MESSAGE-MSGID type LV_MESSAGE-MSGTY
              number LV_MESSAGE-MSGNO
              with LV_MESSAGE-MSGV1 LV_MESSAGE-MSGV2
                   LV_MESSAGE-MSGV3 LV_MESSAGE-MSGV4.
  endtry.


* Activate hyperlink; set tooltips for icons
  LR_FUNCTIONAL_SETTINGS = GR_ALV_TABLE->GET_FUNCTIONAL_SETTINGS( ).

* Activate hyperlink
  try.
      LR_COLUMNS->SET_HYPERLINK_ENTRY_COLUMN( 'T_HYPERLINK' ).

    catch CX_SALV_DATA_ERROR
      into LR_EXCEPTION.
      LV_MESSAGE = LR_EXCEPTION->GET_MESSAGE( ).
      message id LV_MESSAGE-MSGID type LV_MESSAGE-MSGTY
              number LV_MESSAGE-MSGNO
              with LV_MESSAGE-MSGV1 LV_MESSAGE-MSGV2
                   LV_MESSAGE-MSGV3 LV_MESSAGE-MSGV4.
  endtry.

  LR_HYPERLINKS = LR_FUNCTIONAL_SETTINGS->GET_HYPERLINKS( ).
  loop at LT_URLS_FOR_NOTES into LS_URLS_FOR_NOTES.
    try.
        LR_HYPERLINKS->ADD_HYPERLINK(
          HANDLE    = LS_URLS_FOR_NOTES-HANDLE
          HYPERLINK = LS_URLS_FOR_NOTES-NOTEURL ).

      catch CX_SALV_EXISTING
        into LR_EXCEPTION.
        LV_MESSAGE = LR_EXCEPTION->GET_MESSAGE( ).
        message id LV_MESSAGE-MSGID type LV_MESSAGE-MSGTY
                number LV_MESSAGE-MSGNO
                with LV_MESSAGE-MSGV1 LV_MESSAGE-MSGV2
                     LV_MESSAGE-MSGV3 LV_MESSAGE-MSGV4.
    endtry.
  endloop.

* register to the events of cl_salv_table
  LR_EVENTS = GR_ALV_TABLE->GET_EVENT( ).
  create object GR_ALV_EVENTS.
* register to the event USER_COMMAND
  set handler GR_ALV_EVENTS->ON_USER_COMMAND for LR_EVENTS.
* register to the event DOUBLE_CLICK
  set handler GR_ALV_EVENTS->ON_DOUBLE_CLICK for LR_EVENTS.

* set selection mode
  LR_SELECTIONS = GR_ALV_TABLE->GET_SELECTIONS( ).
  LR_SELECTIONS->SET_SELECTION_MODE(
  IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).

  data L_LINE                 type I.

* header
  create object LR_GRID_HEADER.
  L_LINE = 1.
* System list
  data: LT_SYSTEM     type  AGSNOTE_API_T_CHKED_SYSTEM,
        LS_SYSTEM     type  AGSNOTE_API_S_CHKED_SYSTEM,
        LAST_CHK_DATE type SY-DATUM,
        LAST_CHK_TIME type SY-UZEIT,
        L_MSG(80).

  data LO_EX  type ref to CX_LMDB_UNKNOWN_SYSTEM_TYPE.

  if P_HDR is initial. " 'X': Omit system list in header
    call function 'AGSNO_API_GET_CHKED_SYSTEMS'
      importing
        ET_SYSTEM = LT_SYSTEM.

    LR_GRID_HEADER->CREATE_TEXT(
         ROW    = L_LINE
         COLUMN = 1
         TEXT   = 'Systems:'(064) ).

    loop at LT_SYSTEM into LS_SYSTEM
      where SYSTEM_NAME in S_SYS
        and SYSTEM_TYPE in S_TYPE.

      convert time stamp LS_SYSTEM-LAST_CHK
          time zone SY-ZONLO
          into date LAST_CHK_DATE
               time LAST_CHK_TIME.

      data LS_SYSTEM_INFO type  AGS_SR_S_LMDB_SYSTEM.
      try.
          call function 'AGSNO_GET_SYSTEM_INFO'
            exporting
              SYSTEM_NAME = LS_SYSTEM-SYSTEM_NAME
              SYSTEM_TYPE = LS_SYSTEM-SYSTEM_TYPE
*             ROLE        =
*             PRIORITY    =
            importing
              SYSTEM_INFO = LS_SYSTEM_INFO.
        catch CX_LMDB_UNKNOWN_SYSTEM_TYPE into LO_EX.
          write: /(30) 'AGSNO_GET_SYSTEM_INFO' color col_negative, LS_SYSTEM-SYSTEM_NAME, LS_SYSTEM-SYSTEM_TYPE, LO_EX->GET_TEXT( ).
      endtry.

      LR_GRID_HEADER->CREATE_TEXT(
           ROW    = L_LINE
           COLUMN = 2
           TEXT   = LS_SYSTEM-SYSTEM_NAME ).

      LR_GRID_HEADER->CREATE_TEXT(
           ROW    = L_LINE
           COLUMN = 3
           TEXT   = LS_SYSTEM-SYSTEM_TYPE ).

      data LS_KV type AGS_SR_S_KV.
      read table GT_SYSTEM_ROLES into LS_KV
        with key SR_KEY = LS_SYSTEM_INFO-ROLE.
      if SY-SUBRC = 0.
        LR_GRID_HEADER->CREATE_TEXT(
           ROW    = L_LINE
           COLUMN = 4
           TEXT   = LS_KV-SR_VALUE ).
      else.
        LR_GRID_HEADER->CREATE_TEXT(
           ROW    = L_LINE
           COLUMN = 4
           TEXT   = LS_SYSTEM_INFO-ROLE ).
      endif.

      read table GT_SYSTEM_PRIORITIES into LS_KV
        with key SR_KEY = LS_SYSTEM_INFO-PRIORITY.
      if SY-SUBRC = 0.
        LR_GRID_HEADER->CREATE_TEXT(
           ROW    = L_LINE
           COLUMN = 5
           TEXT   = LS_KV-SR_VALUE ).
      else.
        LR_GRID_HEADER->CREATE_TEXT(
           ROW    = L_LINE
           COLUMN = 5
           TEXT   = LS_SYSTEM_INFO-PRIORITY ).
      endif.

      clear L_MSG.
      write LAST_CHK_DATE dd/mm/yyyy to L_MSG(10).
      write LAST_CHK_TIME using edit mask '__:__:__' to L_MSG+11(8).
      LR_GRID_HEADER->CREATE_TEXT(
           ROW    = L_LINE
           COLUMN = 6
           TEXT   = L_MSG ).
      add 1 to L_LINE.
    endloop.
  endif.

* Show information about status maintenance
  if sy-batch is initial.
    lr_grid_header->create_text(
         row    = l_line
         column = 1
         text   = 'Maintain status:'(032) ).
    lr_grid_header->create_text(
         row    = l_line
         column = 2
         text   = 'Use function STATUS to maintain status and to enter comments'(024) ).
    add 1 to l_line.
  endif.

  GR_ALV_TABLE->SET_TOP_OF_LIST( LR_GRID_HEADER ).

* footer
  create object LR_GRID_FOOTER.
  L_LINE = 1.

* Program version
  LR_GRID_FOOTER->CREATE_TEXT(
       ROW    = L_LINE
       COLUMN = 1
       TEXT   = 'Program version:'(VER) ).
  LR_GRID_FOOTER->CREATE_TEXT(
       ROW    = L_LINE
       COLUMN = 2
       TEXT   = C_PROGRAM_VERSION ).
  add 1 to L_LINE.

  GR_ALV_TABLE->SET_END_OF_LIST( LR_GRID_FOOTER ).

* Set Title
  LR_DISPLAY_SETTINGS = GR_ALV_TABLE->GET_DISPLAY_SETTINGS( ).
  LR_DISPLAY_SETTINGS->SET_LIST_HEADER( SY-TITLE ).
  LR_DISPLAY_SETTINGS->SET_LIST_HEADER_SIZE( CL_SALV_DISPLAY_SETTINGS=>C_HEADER_SIZE_LARGE ).

* display the table
  GR_ALV_TABLE->DISPLAY( ).

endform. " alv_show_notelist.

form SHOW_STATUS_COMMENTS.

  data:
    LS_AGSSR_STATUS       type                 AGSSR_STATUS,
    LT_AGSSR_STATUS       type sorted table of AGSSR_STATUS    with unique key STATUS_ID,
*    LS_AGSSR_SYSNOTES     type                 AGSSR_SYSNOTES,
*    LT_AGSSR_SYSNOTES     type sorted table of AGSSR_SYSNOTES  with unique key SYSTEM_NAME SYSTEM_TYPE NOTE_NUMBER,
    LS_AGSSR_SYSNOTEC     type                 AGSSR_SYSNOTEC,
    LS_AGSSR_SYSNOTEC_OLD type                 AGSSR_SYSNOTEC,
    LT_AGSSR_SYSNOTEC     type        table of AGSSR_SYSNOTEC.

*   Show date of recommendations
    data: L_DATE type SY-DATUM,
          L_TIME type SY-UZEIT.

* More selections:
* S_FAVO
* S_PRIO
* S_THEMK
* S_COMP
* S_FROM
* S_TO
* S_GROUP1 .. S_GROUP6

* SysRec: Status
  select * from AGSSR_STATUS into table @LT_AGSSR_STATUS
    where LANGU =  @SY-LANGU
    order by primary key.

* SysRec: Latest note status
*  select * from AGSSR_SYSNOTES into table LT_AGSSR_SYSNOTES
*    where SYSTEM_NAME in S_SYS
*      and SYSTEM_TYPE in S_TYPE
*      and NOTE_NUMBER in S_NOTE
*      and LAST_STATUS in S_STAT
*    order by SYSTEM_NAME SYSTEM_TYPE NOTE_NUMBER.

* SysRec: Note comments
  select * from AGSSR_SYSNOTEC into table @LT_AGSSR_SYSNOTEC
    where SYSTEM_NAME in @S_SYS
      and SYSTEM_TYPE in @S_TYPE
      and NOTE_NUMBER in @S_NOTE
      and (   STATUS_FROM in @S_STAT    " maybe that's not very useful because the chain of status changes get's lost
           or STATUS_TO   in @S_STAT )
    order by NOTE_NUMBER, SYSTEM_NAME, CREATED_AT ascending
    .


  format reset.
  format color col_heading.
  write: /(10)  'Note',
          (8)   'System',
          (10)  'Type',
          (10)  'Old status',
          (10)  'New status',
          (12)  'User',
          (10)  'Date',
          (8)   'Time',
                'Comment',
           at sy-linsz space.
  format reset.

  loop at LT_AGSSR_SYSNOTEC into LS_AGSSR_SYSNOTEC.
    if SY-TABIX > 1.
      new-line.
    endif.

    if   LS_AGSSR_SYSNOTEC_OLD-NOTE_NUMBER  ne LS_AGSSR_SYSNOTEC-NOTE_NUMBER.
      write
         (10)   LS_AGSSR_SYSNOTEC-NOTE_NUMBER  color col_key.
    endif.

    if   LS_AGSSR_SYSNOTEC_OLD-NOTE_NUMBER  ne LS_AGSSR_SYSNOTEC-NOTE_NUMBER
      or LS_AGSSR_SYSNOTEC_OLD-SYSTEM_NAME  ne LS_AGSSR_SYSNOTEC-SYSTEM_NAME
      or LS_AGSSR_SYSNOTEC_OLD-SYSTEM_TYPE  ne LS_AGSSR_SYSNOTEC-SYSTEM_TYPE.
      write:
         12(8)  LS_AGSSR_SYSNOTEC-SYSTEM_NAME  color col_total,
         (10)   LS_AGSSR_SYSNOTEC-SYSTEM_TYPE  color col_total.

*      clear LS_AGSSR_SYSNOTES.
*      read table LT_AGSSR_SYSNOTES into LS_AGSSR_SYSNOTES
*        with key SYSTEM_NAME = LS_AGSSR_SYSNOTEC-SYSTEM_NAME
*                 SYSTEM_TYPE = LS_AGSSR_SYSNOTEC-SYSTEM_TYPE
*                 NOTE_NUMBER = LS_AGSSR_SYSNOTEC-NOTE_NUMBER.
**    clear ls_AGSSR_STATUS.
**    read table lt_AGSSR_STATUS into ls_AGSSR_STATUS
**      WITH KEY STATUS_ID = ls_AGSSR_SYSNOTES-LAST_STATUS.
*      write:
*         32  "'Current status',
*             LS_AGSSR_SYSNOTES-LAST_STATUS,
*             LS_AGSSR_SYSNOTES-LAST_USER.
**             ls_AGSSR_STATUS-STATUS_TEXT,
**             ls_AGSSR_STATUS-STATUS_LTEXT.
    endif.

*    clear ls_AGSSR_STATUS.
*    read table lt_AGSSR_STATUS into ls_AGSSR_STATUS
*      WITH KEY STATUS_ID = ls_AGSSR_SYSNOTEC-STATUS_FROM.
    write:
       32  "'Old status',
           LS_AGSSR_SYSNOTEC-STATUS_FROM,
           ls_AGSSR_STATUS-STATUS_TEXT.
*           ls_AGSSR_STATUS-STATUS_LTEXT.

*    clear ls_AGSSR_STATUS.
*    read table lt_AGSSR_STATUS into ls_AGSSR_STATUS
*      WITH KEY STATUS_ID = ls_AGSSR_SYSNOTEC-STATUS_TO.
    write:
          "'New status',
           LS_AGSSR_SYSNOTEC-STATUS_TO,
           ls_AGSSR_STATUS-STATUS_TEXT.
*           ls_AGSSR_STATUS-STATUS_LTEXT.


    write:
              LS_AGSSR_SYSNOTEC-CREATED_BY.

*    write:
*              LS_AGSSR_SYSNOTEC-CREATED_AT.
    convert time stamp LS_AGSSR_SYSNOTEC-CREATED_AT
        time zone SY-ZONLO
        into date L_DATE
             time L_TIME.
    write:   L_DATE dd/mm/yyyy,
             L_TIME using edit mask '__:__:__'.

*    write:
*               LS_AGSSR_SYSNOTEC-COMMENT_TEXT  color col_normal.
    data: lt_COMMENT_TEXTS TYPE TABLE OF string,
          l_COMMENT_TEXT   type string.
    SPLIT LS_AGSSR_SYSNOTEC-COMMENT_TEXT AT cl_abap_char_utilities=>newline INTO
          TABLE lt_COMMENT_TEXTS
          IN CHARACTER MODE.
    loop at lt_COMMENT_TEXTS into l_COMMENT_TEXT.
      if l_COMMENT_TEXT is not initial.
        if sy-tabix > 1.
          NEW-LINE.
        endif.
        write   l_COMMENT_TEXT  color col_normal
          under l_COMMENT_TEXT.
      endif.
    endloop.

    LS_AGSSR_SYSNOTEC_OLD = LS_AGSSR_SYSNOTEC.
  endloop. " lt_AGSSR_SYSNOTEC

endform. " SHOW_STATUS_COMMENTS
