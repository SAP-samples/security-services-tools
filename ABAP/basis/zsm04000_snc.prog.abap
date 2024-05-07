*&---------------------------------------------------------------------*
*& Report  ZSM04000_SNC
*& based on transaction SM04 = Report RSM04000_ALV
*& (report RSM04000_ALV_NEW might know some more data)
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Source: https://github.com/SAP-samples/security-services-tools
*&
*& 30.09.2013 Initial version
*& 12.12.2018 Additional fields SNC_MODE, SNC_PEER_NAME (if available depending on Kernel version)
*& 29.09.2021 Show ccl-Parameters, too
*&            Use more fields to find the SNC name
*& 30.09.2021 Get data from all active application servers
*& 02.08.2022 Small correction of ALV because of consistency check using Shift+Double right click
*& 11.10.2022 Correct coloring in case of snc_mode = OFF
*&            Show transaction and program always
*& 09.05.2023 Add another field 'Client IP' which shows the original IP address of the client in case of connections based on hopping from one server to another.
*&            Show host and service in case of connections of type 'Plugin HTPPS'
*&            Don't show red color in case of connection type 'System'
*&---------------------------------------------------------------------*

REPORT  zsm04000_snc
  MESSAGE-ID 14
  LINE-SIZE 1023.

CONSTANTS: c_program_version(10) TYPE c VALUE '09.05.2023'.

INCLUDE <color>.
INCLUDE tskhincl. "opcodes for ThUsrInfo
*include RSUSRCOM. "USR_TABL



* ALV

TYPE-POOLS: slis.

DATA: BEGIN OF usr_tabl_alv OCCURS 10,
        server_name      TYPE msxxlist-name.
* SM04 standard fields
        INCLUDE STRUCTURE usrinfo.
        "MANDT  Client
        "BNAME  User Name
        "TCODE  TCODE
        "TERM	Terminal ID
        "ZEIT	Dialog time in SM04
        "MASTER	Master
        "TRACE  User trace
        "EXTMODI  Task Handler: Number of External or Internal Modes
        "INTMODI  Task Handler: Number of External or Internal Modes
        "TYPE	Type of Logon
        "STAT	State of System Logon
        "PROTOCOL	Logon Protocol (if Plug-In)
        "GUIVERSION	Version of SAP GUI
        "RFC_TYPE	Type of RFC Logon
        "HOSTADDR	IP Address
        DATA:
        hostadr(15), " from uinfo-hostadr provided by function THUSRINFO
        uinfo_type                   type string,
        uinfo_subType                type string,
        uinfo_rfc_type               type string,
        uinfo_server_plugin_protocol type string,
        uinfo_server_plugin_state    type string,
        ext_type(120),
        ext_state(10),
        ext_time(8),
        ext_trace(5),
        line_col(3),
        ipaddr(30),
        selected(1),
        total_mem_mb     TYPE i,
        security_context TYPE string,
        programInfo      type string.
* SM04 -> user -> technical information
DATA: display(80),
      iaddr(80),
      snc_mode(8),
      snc_pname     TYPE snc_pname,
      snc_count(80).                   " number of snc contexts
" Each connection protected with SNC needs an SNC context. For a logon there may exisit several snc contexts:
" - if this is a Sapgui logon: one for each active session
" - if there are RFC connections with active SNC: one for each "secure" RFC connection

* ALV specific
DATA:   field_col TYPE slis_t_specialcol_alv.
DATA: END OF usr_tabl_alv.

DATA: fieldcat          TYPE slis_t_fieldcat_alv,
      layout            TYPE slis_layout_alv,
      events            TYPE slis_t_event,
      usr_info_fieldcat TYPE slis_t_fieldcat_alv,
      usr_info_layout   TYPE slis_layout_alv,
      is_variant        LIKE disvariant.

DATA: gs_alv_lout_variant TYPE disvariant.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_mandt FOR FIELD s_mandt.
SELECT-OPTIONS s_mandt  FOR usr_tabl_alv-mandt     DEFAULT sy-mandt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_bname FOR FIELD s_bname.
SELECT-OPTIONS s_bname  FOR usr_tabl_alv-bname.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_snc FOR FIELD s_snc.
SELECT-OPTIONS s_snc    FOR usr_tabl_alv-snc_count.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_type FOR FIELD s_type.
SELECT-OPTIONS s_type   FOR usr_tabl_alv-ext_type.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_rfcty FOR FIELD s_rfctyp.
SELECT-OPTIONS s_rfctyp FOR usr_tabl_alv-rfc_type.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_head AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(40) ss_head FOR FIELD s_head.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*

INITIALIZATION.

  text001   = 'Selection'(101).
  ss_mandt  = 'Client'(001).
  ss_bname  = 'User'(002).
  ss_snc    = 'SNC count'(104).
  ss_type   = 'Logon type (GUI/RFC)'(105).
  ss_rfcty  = 'RFC type (E/I)'(106).

  ss_head   = 'Show profile parameters in header'(107).

  CONCATENATE 'Program version from'(100) c_program_version INTO ss_vers
    SEPARATED BY space.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
    ID 'S_ADMI_FCD' FIELD 'PADM'.
  IF sy-subrc NE 0.
    MESSAGE e104.
  ENDIF.

* Should we display the application info?
* This means to lock all single entries of tm_adm. There might
* be bad entries holding the tm_adm mutex. To display the user
* list in such a situation, the display of the application info
* can be deactivated. To do this,
* - use menu System->User Profile->Own Data
* - select Parmeters
* - add Parameter ID SM04_CONFIGURATION with Value ' '.
*
* (to display the application info use Value 'X').
  "DATA with_appl_info(1).
  "GET PARAMETER ID 'SM04_CONFIGURATION' FIELD with_appl_info.
  "IF sy-subrc = 4.
  "  with_appl_info = 'X'.
  "  SET PARAMETER ID 'SM04_CONFIGURATION' FIELD with_appl_info.
  "ENDIF.



*DATA SERVICES        TYPE MSXXLIST-MSGTYPES.
*DATA SYSSERVICE      TYPE MSSYSSERVICE.
*DATA ACTIVE_SERVER   TYPE I.
*DATA SUBSYSTEM_AWARE TYPE I.
  DATA server_list      TYPE STANDARD TABLE OF msxxlist.
  DATA server_entry     TYPE msxxlist.
*DATA LIST_IPV6       TYPE STANDARD TABLE OF MSXXLIST_V6.

  CALL FUNCTION 'TH_SERVER_LIST'
    EXPORTING
*     SERVICES       = 255
*     SYSSERVICE     = 0
      active_server  = 1
*     SUBSYSTEM_AWARE       = 1
    TABLES
      list           = server_list
*     LIST_IPV6      = LIST_IPV6
    EXCEPTIONS
      no_server_list = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT server_list INTO server_entry.
    PERFORM build_list
      USING server_entry-name.
  ENDLOOP.
  SORT usr_tabl_alv BY mandt bname server_name.

  PERFORM build_fieldcat USING fieldcat[].
  PERFORM build_layout USING layout.
  PERFORM build_event  USING events[].

  is_variant-handle = 'USRL'.
  IF s_head = 'X'.
    DATA: callback_top_of_page(30).
    callback_top_of_page = 'CREATE_HEADER'.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name        = 'USR_TABL_ALV'
      i_callback_program      = sy-repid
*     i_callback_pf_status_set = 'SM04_SET_STATUS'
      i_callback_top_of_page  = callback_top_of_page
      i_callback_user_command = 'SM04_USER_CMD'
      i_save                  = 'A'
      it_fieldcat             = fieldcat[]
      it_events               = events[]
      is_layout               = layout
      is_variant              = is_variant
    TABLES
      t_outtab                = usr_tabl_alv.

*----------------------------------------------------------------------*
* form build_list
*----------------------------------------------------------------------*
FORM build_list
  USING server_name TYPE msxxlist-name.

  DATA: BEGIN OF usr_info OCCURS 10.
  DATA: field(40),
        VALUE(80).
  DATA: END OF usr_info.

  DATA: plugin_name     TYPE plg_name,
        plugin_host     TYPE plg_host,
        plugin_service  TYPE plg_srv,
        myname          LIKE msxxlist-name,
        dest            TYPE rfcdest.

  DATA: tmp_field_col TYPE slis_specialcol_alv.

  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'rdisp/myname'
                     ID 'VALUE' FIELD myname.

  IF server_entry-name = myname.
    CLEAR dest.
  ELSE.
    dest = server_entry-name.
  ENDIF.

* Get SM04 user list

  " Instead of C-call 'ThUsrInfo' we can use the RFC enabled functions THUSRINFO or TH_USER_LIST
  "REFRESH usr_tabl.
  "DATA: BEGIN OF usr_tabl OCCURS 10.
  "        INCLUDE STRUCTURE usrinfo.
  "      DATA: END OF usr_tabl.
  "call 'ThUsrInfo'
  "  id 'OPCODE' field opcode_list       " 2, see include TSKHINCL
  "  id 'TABUSR' field usr_tabl[].
  DATA usr_tabl TYPE STANDARD TABLE OF uinfo.
  CALL FUNCTION 'THUSRINFO'
    DESTINATION dest
    TABLES
      usr_tabl              = usr_tabl
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 3.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  DATA usr_entry LIKE LINE OF usr_tabl.
  LOOP AT usr_tabl INTO usr_entry
    WHERE mandt    IN s_mandt
      AND bname    IN s_bname
      AND rfc_type IN s_rfctyp.

    CLEAR usr_tabl_alv.
    usr_tabl_alv-server_name = server_entry-name.
    MOVE-CORRESPONDING usr_entry TO usr_tabl_alv.

    " Logon type according to data element SSI_SESSION_TYPE
    " GUI             User session of type SAP GUI front end
    " HTTP            User session of type HTTP
    " HTTPS           User session of type HTTPS
    " SMTP            User session of type SMTP
    " RFC             User session of type RFC
    " BGRFC Scheduler User session of type BGRFC
    " BGRFC unit      User session of type RFC Unit
    " APC             User session of type  ABAP Push Channel
    " APC streaming   User session to send via ABAP Push Channel
    " Daemon          User session of ABAP Daemon Framework
    " BATCH           User session with background processing
    " Enqueue         User session for the integrated enqueue server
    " Update          Use of the Update Task
    " SYSTEM          Back-end session created by the system (e.g. to execute Auto-ABAP or other internal tasks)
    " WebSocket RFC   User session of type WebSocket RFC (RFC via WebSocket connection)

    CASE usr_tabl_alv-type.
      WHEN 2. usr_tabl_alv-ext_type   = 'System'(052).
      WHEN 4. usr_tabl_alv-ext_type   = 'GUI'(035).
      WHEN 32. usr_tabl_alv-ext_type  = 'RFC'(036).
      WHEN 202.
        usr_tabl_alv-ext_type = 'Plugin &'(039).
        CALL FUNCTION 'TH_GET_PLUGIN_INFO'
          EXPORTING
            protocol = usr_tabl_alv-protocol
          IMPORTING
            name     = plugin_name
            host     = plugin_host
            service  = plugin_service
          EXCEPTIONS
            OTHERS   = 1.

        IF sy-subrc = 0.
          REPLACE '&' WITH plugin_name INTO usr_tabl_alv-ext_type.
          CONDENSE usr_tabl_alv-ext_type.
          concatenate usr_tabl_alv-ext_type plugin_host plugin_service into usr_tabl_alv-ext_type SEPARATED BY space.
        ENDIF.
      WHEN OTHERS.
        WRITE usr_tabl_alv-type TO
              usr_tabl_alv-ext_type LEFT-JUSTIFIED.
    ENDCASE.

    CHECK usr_tabl_alv-ext_type IN s_type.

    CASE usr_tabl_alv-stat.
      WHEN 2. usr_tabl_alv-ext_state = 'Connected'(037).
      WHEN 6. usr_tabl_alv-ext_state = 'Pooled'(038).
      WHEN OTHERS.
        WRITE usr_tabl_alv-stat TO
              usr_tabl_alv-ext_state LEFT-JUSTIFIED.
    ENDCASE.

    IF usr_tabl_alv-trace <> 0.
      usr_tabl_alv-line_col = 'C30'.
      usr_tabl_alv-ext_trace = 'ON'(028).
    ELSE.
      usr_tabl_alv-ext_trace = 'OFF'(027).
    ENDIF.

    WRITE usr_tabl_alv-zeit TO usr_tabl_alv-ext_time
      USING EDIT MASK '__:__:__'.

*    CALL FUNCTION 'TH_GET_SECURITY_CONTEXT_REF'
*      EXPORTING
*        tid                        = usr_tabl_alv-tid
*      IMPORTING
*        SECURITY_CONTEXT_REFERENCE = security_context_reference
*        IS_SET                     = is_sec_cont_ref_set
*      EXCEPTIONS
*        INTERNAL_ERROR             = 1
*        OTHERS                     = 2.
*    IF is_sec_cont_ref_set = 'X'.
*      TRY.
*          CALL METHOD cl_http_security_session_admin=>get_displayname_for_contextref
*            EXPORTING
*              context_ref  = security_context_reference
*            RECEIVING
*              display_name = usr_tabl_alv-security_context.
*        CATCH cx_http_security_session .
*      ENDTRY.
*    ENDIF.

*   Get SM04 -> user -> technical information

    REFRESH usr_info.

    " Instead of C-call 'ThUsrInfo' we could use the RFC enabled functions TH_SHOW_USR_DETAILS
    "call 'ThUsrInfo'
    "  id 'OPCODE'         field opcode_usr_info " 53, see include TSKHINCL
    "  id 'TID'            field usr_tabl-TID
    "  ID 'WITH_APPL_INFO' field with_appl_info
    "  id 'TABLE'          field usr_info[].
    "DATA TERM_ID   TYPE UTID.
    "DATA TECH_INFO TYPE STANDARD TABLE OF TECHINFO.
    CALL FUNCTION 'TH_SHOW_USR_DETAILS'
      DESTINATION dest
      EXPORTING
        term_id                       = usr_entry-tid
      TABLES
        tech_info                     = usr_info
      EXCEPTIONS
        th_unknown_opcode             = 1
        th_mismatch_in_info_structure = 2
        th_cant_read_tid              = 3
        th_tid_irregular_value        = 4
        th_slot_free                  = 5
        th_techinfo_not_available     = 6
        th_mismatch_in_info_strucnew  = 7
        th_mismatch_in_opcode         = 8
        th_unknown                    = 9
        communication_failure         = 12
        system_failure                = 12
        other                         = 13.
    IF sy-subrc NE 0.
      "CONTINUE.
    ENDIF.

    LOOP AT usr_info.
      CASE usr_info-field.
        WHEN 'tid'.
          " already known
        WHEN 'client'.
          " already known
        WHEN 'user'.
          " already known
        WHEN 'type'.                                " DP_LOGON_RFC           DP_LOGON_GUI
          usr_tabl_alv-uinfo_type                   = usr_info-value+9.
        WHEN 'subType'.                             " DP_LOGON_ASYNC_RFC     DP_LOGON_SUB_TYPE_UNDEF
          if usr_info-value ne 'DP_LOGON_SUB_TYPE_UNDEF'.
            usr_tabl_alv-uinfo_subType                = usr_info-value+9.
          endif.
        WHEN 'rfc_type'.                            " DP_INTERNAL_RFC        DP_RFC_TYPE_UNDEF
          if usr_info-value ne 'DP_RFC_TYPE_UNDEF'.
            usr_tabl_alv-uinfo_rfc_type               = usr_info-value+3.
          endif.
        WHEN 'server_plugin_protocol'.              " DP_PLUGIN_PROTOCOL_NONE
          if usr_info-value ne 'DP_PLUGIN_PROTOCOL_NONE'.
            usr_tabl_alv-uinfo_server_plugin_protocol = usr_info-value+19.
          endif.
        WHEN 'server_plugin_state'.                 " DP_PLUGIN_FREE
          if usr_info-value ne 'DP_PLUGIN_FREE'.
            usr_tabl_alv-uinfo_server_plugin_state    = usr_info-value+10.
          endif.
        WHEN 'term'.
          " already known
        WHEN 'SAPGUI version'.
          " already known

        WHEN 'display'.
          usr_tabl_alv-display   = usr_info-value.

        WHEN 'iaddr (gui host)' OR 'client UI net addr' OR 'client net addr' OR 'client peer net addr'.
          IF usr_info-value IS NOT INITIAL AND usr_info-value NE '0.0.0.0'.
            usr_tabl_alv-iaddr     = usr_info-value.
          ENDIF.

        WHEN 'snc_count'.
          usr_tabl_alv-snc_count = usr_info-value.

          IF   usr_tabl_alv-type = 4   "GUI
            OR usr_tabl_alv-type = 32. "RFC

            clear tmp_field_col.
            tmp_field_col-fieldname = 'SNC_COUNT'.
            tmp_field_col-color-int = 0.
            tmp_field_col-color-inv = 0.
            IF usr_tabl_alv-snc_count > 0.
              tmp_field_col-color-col = col_positive.
            ELSEIF usr_tabl_alv-type ne 2. " not for system connections.
              if usr_tabl_alv-rfc_type = 'I'.
                tmp_field_col-color-col = col_total.
              else.
                tmp_field_col-color-col = col_negative.
              endif.
            ENDIF.
            APPEND tmp_field_col TO usr_tabl_alv-field_col.
          ENDIF.

        WHEN 'snc_per_logon.sncMode' OR 'snc_base_info.mode'.
          usr_tabl_alv-snc_mode = usr_info-value.

          clear tmp_field_col.
          tmp_field_col-fieldname = 'SNC_MODE'.
          tmp_field_col-color-int = 0.
          tmp_field_col-color-inv = 0.
          IF usr_tabl_alv-snc_mode = 'ON'.
            tmp_field_col-color-col = col_positive.
            APPEND tmp_field_col TO usr_tabl_alv-field_col.
          ELSEIF usr_tabl_alv-snc_mode = 'OFF' and usr_tabl_alv-type ne 2. " not for system connections
            if usr_tabl_alv-rfc_type = 'I'.
              tmp_field_col-color-col = col_total.
            else.
              tmp_field_col-color-col = col_negative.
            endif.
            APPEND tmp_field_col TO usr_tabl_alv-field_col.
          ENDIF.

        WHEN 'snc_per_logon.peerid'.
          " ignored

        WHEN 'snc_per_logon.peeridlen'.
          " ignored

        WHEN 'snc_per_logon.snc_peer_name' OR 'snc_base_info.snc_peer_name'.
          usr_tabl_alv-snc_pname = usr_info-value.

        WHEN 'snc_per_logon.snc_peer_name_len'.
          " ignored

        WHEN 'modeinfo[0].tcode'.
          " already known

        WHEN 'modeinfo[0].programInfo'.
          usr_tabl_alv-programInfo =  usr_info-value.

      ENDCASE.
    ENDLOOP.

    CHECK usr_tabl_alv-snc_count IN s_snc.

    IF xstrlen( usr_entry-hostadr ) = 4 and usr_entry-hostadr is not initial. " The IP contains 4 hex values
      " Convert hex values to integer
      " Modern style:
      "usr_tabl_alv-hostadr = |{ CONV i( usr_entry-hostadr+0(1) ) }.{ CONV i( usr_entry-hostadr+1(1) ) }.{ CONV i( usr_entry-hostadr+2(1) ) }.{ CONV i( usr_entry-hostadr+3(1) ) }|.
      " Old style:
      data:
        h1 type x, i1 type i, c1(3),
        h2 type x, i2 type i, c2(3),
        h3 type x, i3 type i, c3(3),
        h4 type x, i4 type i, c4(3).
      h1 = usr_entry-hostadr+0(1). i1 = h1. c1 = i1.
      h2 = usr_entry-hostadr+1(1). i2 = h2. c2 = i2.
      h3 = usr_entry-hostadr+2(1). i3 = h3. c3 = i3.
      h4 = usr_entry-hostadr+3(1). i4 = h4. c4 = i4.
      concatenate c1 c2 c3 c4 into usr_tabl_alv-hostadr SEPARATED BY '.'.
      CONDENSE usr_tabl_alv-hostadr NO-GAPS.
      " Show it only it it's different
      if usr_tabl_alv-hostadr = usr_tabl_alv-iaddr.
        clear usr_tabl_alv-hostadr.
      endif.
    else.
      clear usr_tabl_alv-hostadr.
    ENDIF.

    APPEND usr_tabl_alv.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* build alv field catalog for sm04
*----------------------------------------------------------------------*
FORM build_fieldcat USING fieldcat TYPE slis_t_fieldcat_alv.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

  REFRESH fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'MANDT'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-key          = 'X'.
  ls_fieldcat-reptext_ddic = 'Client'(001).
  ls_fieldcat-outputlen    = 4.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'BNAME'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-key          = 'X'.
  ls_fieldcat-reptext_ddic = 'User'(002).
  ls_fieldcat-outputlen    = 12.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'SERVER_NAME'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-key          = ' '.
  ls_fieldcat-reptext_ddic = 'Server name'(008).
  ls_fieldcat-outputlen    = 40.
  ls_fieldcat-ref_tabname  = 'MSXXLIST'.
  ls_fieldcat-ref_fieldname = 'NAME'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'TID'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-datatype     = 'INT4'.
  ls_fieldcat-reptext_ddic = 'Session ID'(020).
  ls_fieldcat-outputlen    = 10.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'HOSTADDR'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Hostname'(009).
  ls_fieldcat-outputlen    = 12.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  ls_fieldcat-lowercase    = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'IPADDR'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'IP Address'(048).
  ls_fieldcat-outputlen    = 30.
  ls_fieldcat-ref_tabname  = ''.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'IADDR'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'IADDR'(010).
  ls_fieldcat-outputlen    = 10.
  ls_fieldcat-no_out       = ' '.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'HOSTADR'. " from uinfo-hostadr provided by function THUSRINFO
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Client IP'.
  ls_fieldcat-outputlen    = 30.
  "ls_fieldcat-ref_tabname  = 'UINFO'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'TERM'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Terminal'(003).
  ls_fieldcat-outputlen    = 15.
  ls_fieldcat-lowercase    = 'X'.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'DISPLAY'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Display'(011).
  ls_fieldcat-outputlen    = 10.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'SNC_MODE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'SNC Mode'(012).
  ls_fieldcat-outputlen    = 8.
*  ls_fieldcat-emphasize    = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'SNC_PNAME'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'SNC Name'(013).
  ls_fieldcat-outputlen    = 40.
*  ls_fieldcat-emphasize    = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'SNC_COUNT'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'SNC Count'(014). "number of snc contexts
  ls_fieldcat-outputlen    = 10.
*  ls_fieldcat-emphasize    = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'TCODE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Transaction'(004).
  ls_fieldcat-outputlen    = 20.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
"  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'PROGRAMINFO'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Program'(015).
  ls_fieldcat-outputlen    = 40.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
"  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'ZEIT'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Time'(005).
  ls_fieldcat-outputlen    = 10.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  ls_fieldcat-tech         = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'EXT_TIME'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Time'(005).
  ls_fieldcat-outputlen    = 8.
  ls_fieldcat-ref_fieldname = 'ZEIT'.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'MASTER'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Master'(016).
  ls_fieldcat-outputlen    = 12.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'TRACE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Trace'(026).
  ls_fieldcat-datatype     = 'INT1'.
  ls_fieldcat-outputlen    = 3.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'EXT_TRACE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Trace'(026).
  ls_fieldcat-outputlen    = 8.
  ls_fieldcat-ref_tabname  = 'RSM04000_TYPES'.
  ls_fieldcat-ref_fieldname = 'EXT_TRACE'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'EXTMODI'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Sess.'(006).
  ls_fieldcat-datatype     = 'INT4'.
  ls_fieldcat-outputlen    = 5.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'INTMODI'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Sess.'(007).
  ls_fieldcat-datatype     = 'INT4'.
  ls_fieldcat-outputlen    = 6.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'TYPE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Type'(033).
  ls_fieldcat-datatype     = 'INT4'.
  ls_fieldcat-outputlen    = 6.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  ls_fieldcat-tech         = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'EXT_TYPE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Type'(033).
* W.Rehm CNVT_NO_NUMBER
* use rollname instead of ref_tabname and
* ref_fieldname for F1-Help
*  LS_FIELDCAT-REF_TABNAME = 'USRINFO'.
*  LS_FIELDCAT-REF_FIELDNAME = 'TYPE'.
  ls_fieldcat-rollname     = 'UEXT_TYPE'.
  "ls_fieldcat-outputlen    = 15.
  ls_fieldcat-lowercase    = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'UINFO_TYPE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Type'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'UINFO_SUBTYPE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Subtype'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'RFC_TYPE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'RFC Type'(041).
  ls_fieldcat-outputlen    = 10.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'UINFO_RFC_TYPE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'RFC Type'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'UINFO_SERVER_PLUGIN_PROTOCOL'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'SERVER_PLUGIN_PROTOCOL'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'UINFO_SERVER_PLUGIN_STATE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'SERVER_PLUGIN_STATE'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'STAT'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Status'(034).
  ls_fieldcat-datatype     = 'INT4'.
  ls_fieldcat-outputlen    = 6.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  ls_fieldcat-tech         = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'EXT_STATE'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Status'(034).
  ls_fieldcat-outputlen    = 10.
* W.Rehm CNVT_NO_NUMBER
* use rollname instead of ref_tabname and
* ref_fieldname for F1-Help
*  LS_FIELDCAT-REF_TABNAME = 'USRINFO'.
*  LS_FIELDCAT-REF_FIELDNAME = 'STAT'.
  ls_fieldcat-rollname     = 'USTATE'.
  ls_fieldcat-lowercase    = 'X'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'PROTOCOL'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Log'(029).
  ls_fieldcat-outputlen    = 10.
  ls_fieldcat-ref_tabname = 'USRINFO'.
  ls_fieldcat-lowercase    = 'X'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'GUIVERSION'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'GUI version'(030).
  ls_fieldcat-outputlen    = 12.
  ls_fieldcat-ref_tabname  = 'USRINFO'.
  APPEND ls_fieldcat TO fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname    = 'TOTAL_MEM_MB'.
  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
  ls_fieldcat-reptext_ddic = 'Megabyte'(051).
  ls_fieldcat-outputlen    = 10.
  ls_fieldcat-ref_tabname  = 'RSM04000_TYPES'.
  ls_fieldcat-ref_fieldname = 'MBYTE'.
  ls_fieldcat-no_out       = 'X'.
  APPEND ls_fieldcat TO fieldcat.

*  CLEAR ls_fieldcat.
*  ls_fieldcat-fieldname    = 'SECURITY_CONTEXT'.
*  ls_fieldcat-tabname      = 'USR_TABL_ALV'.
*  ls_fieldcat-reptext_ddic = 'Security Context'(059).
*  ls_fieldcat-outputlen    = 20.
*  ls_fieldcat-ref_tabname  = 'RSM04000_TYPES'.
*  ls_fieldcat-ref_fieldname = 'SECURITY_CONTEXT'.
*  ls_fieldcat-lowercase    = 'X'.
*  ls_fieldcat-no_out       = 'X'.
*  APPEND ls_fieldcat TO fieldcat.

ENDFORM.

*----------------------------------------------------------------------*
* build alv layout for sm04
*----------------------------------------------------------------------*
FORM build_layout USING layout TYPE slis_layout_alv.
  layout-info_fieldname       = 'LINE_COL'.
  layout-box_fieldname        = 'SELECTED'.
  layout-f2code               = 'PMOD'.
  layout-allow_switch_to_list = 'X'.

  layout-zebra                = 'X'.
  layout-cell_merge           = 'X'.
  layout-colwidth_optimize    = 'X'.
  layout-window_titlebar      = 'Show SNC status of active users on current application server'.
  layout-detail_titlebar      = 'Show SNC status of active users on current application server'.

  layout-coltab_fieldname     = 'FIELD_COL'.
ENDFORM.

*----------------------------------------------------------------------*
* build alv events
*----------------------------------------------------------------------*
FORM build_event USING c_event TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.
  CLEAR ls_event.

  ls_event-name = 'TOP_OF_LIST'.
  ls_event-form = 'CREATE_HEADER'.
  APPEND ls_event TO c_event.

  ls_event-name = 'END_OF_LIST'.
  ls_event-form = 'CREATE_FOOTER'.
  APPEND ls_event TO c_event.
ENDFORM.

*----------------------------------------------------------------------*
* create_header
*----------------------------------------------------------------------*
FORM create_header.

  DATA: par_status           LIKE sy-index.
  DATA: par_name(60).
  DATA: par_wert(60).

*** Tabelle mit unsubstituierten Werten der Profile_parameter **********
  DATA: BEGIN OF par_usub OCCURS 0,
          status       LIKE par_status,
          pname        LIKE par_name,
          user_wert    LIKE par_wert,
          default_wert LIKE par_wert,
        END OF par_usub.

*** Tabelle mit substituierten Werten der Profile_parameter ************
  DATA: BEGIN OF par_sub OCCURS 0,
          status       LIKE par_status,
          pname        LIKE par_name,
          user_wert    LIKE par_wert,
          default_wert LIKE par_wert,
        END OF par_sub.

  DATA:
    lr_grid  TYPE REF TO cl_salv_form_layout_grid,
    lr_label TYPE REF TO cl_salv_form_label,
    lr_text  TYPE REF TO cl_salv_form_text,
    l_row    TYPE i.

  CHECK s_head = 'X'.

  CREATE OBJECT lr_grid.

  CALL 'C_SAPGALLPARAM'                                   "#EC CI_CCALL
    ID 'PAR_USUB' FIELD par_usub[]
    ID 'PAR_SUB'  FIELD par_sub[].


  ADD 1 TO l_row.
  lr_label = lr_grid->create_label(  "label or text
    row     = l_row
    column  = 1
    colspan = 2
    text    = 'Profile Parameters concerning SNC' ).

  LOOP AT par_sub
    WHERE pname CP 'snc/*'
       OR pname CP 'ccl/*'
       OR pname CS 'CRYPTOLIB'
       .
    "read table PAR_USUB index sy-tabix.
    "if sy-subrc ne 0.
    "  clear PAR_USUB.
    "endif.

    ADD 1 TO l_row.
    IF par_sub-pname = 'snc/enable'.
      lr_label = lr_grid->create_label(  "label or text
        row     = l_row
        column  = 1
        text    = par_sub-pname ).
      IF par_sub-user_wert IS NOT INITIAL.
        lr_label = lr_grid->create_label(  "label or text
          row     = l_row
          column  = 2
          text    = par_sub-user_wert ).
      ELSE.
        lr_label = lr_grid->create_label(  "label or text
          row     = l_row
          column  = 2
          text    = par_sub-default_wert ).
      ENDIF.
    ELSE.
      lr_text = lr_grid->create_text(  "label or text
        row     = l_row
        column  = 1
        text    = par_sub-pname ).
      IF par_sub-user_wert IS NOT INITIAL.
        lr_text = lr_grid->create_text(  "label or text
          row     = l_row
          column  = 2
          text    = par_sub-user_wert ).
      ELSE.
        lr_text = lr_grid->create_text(  "label or text
          row     = l_row
          column  = 2
          text    = par_sub-default_wert ).
      ENDIF.
    ENDIF.

  ENDLOOP.

  ADD 1 TO l_row.
  lr_label = lr_grid->create_label(  "label or text
    row     = l_row
    column  = 1
    colspan = 2
    text    = 'Function codes' ).
  ADD 1 TO l_row.
  lr_text = lr_grid->create_text(
    row     = l_row
    column  = 1
    text    = 'USER' ).
  lr_text = lr_grid->create_text(
    row     = l_row
    column  = 2
    text    = 'Show user details' ).
  ADD 1 TO l_row.
  lr_text = lr_grid->create_text(
    row     = l_row
    column  = 1
    text    = 'HELP' ).
  lr_text = lr_grid->create_text(
    row     = l_row
    column  = 2
    text    = 'Show help for column' ).

  CALL METHOD cl_salv_form_content=>set
    EXPORTING
      value = lr_grid.

ENDFORM.                    " create_header

*----------------------------------------------------------------------*
* create_footer
*----------------------------------------------------------------------*
FORM create_footer.

  DATA:
    lr_grid       TYPE REF TO cl_salv_form_layout_grid,
    lr_label      TYPE REF TO cl_salv_form_label,
    l_version(80).

  CREATE OBJECT lr_grid.

  lr_label = lr_grid->create_label(
    row     = 1
    column  = 1
    text    = 'Program version' ).
  lr_label = lr_grid->create_label(
    row     = 1
    column  = 2
    text    = c_program_version ).

  CALL METHOD cl_salv_form_content=>set
    EXPORTING
      value = lr_grid.

ENDFORM.                    " create_footer

*----------------------------------------------------------------------*
* handle user commands of sm04
*----------------------------------------------------------------------*
FORM sm04_user_cmd USING r_ucomm LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.
  DATA: act_index LIKE sy-tabix,
*        refresh like th_bool,
        fieldname LIKE rs_selfield-fieldname,
        selected  LIKE usr_tabl_alv-selected,
        count     TYPE i.

*  refresh = th_true.
  act_index = rs_selfield-tabindex.
  CLEAR usr_tabl_alv.
  selected = ' '.

  IF act_index > 0.
    READ TABLE usr_tabl_alv INDEX rs_selfield-tabindex.
    selected = 'X'.
  ENDIF.

  CASE r_ucomm.
*    when 'PMOD'.
*      perform modus_liste using selected
*                                usr_tabl_alv-mandt
*                                usr_tabl_alv-bname
*                                usr_tabl_alv-tid.

    WHEN 'USER'.
      PERFORM user_info USING selected
                              usr_tabl_alv-mandt
                              usr_tabl_alv-bname.

*    when 'UDELLN'.
*      perform call_delete_user using th_false.
*
*    when 'UDELLP'.
*      perform reset_session_pool using selected th_false.
*
*    when 'UDELGN'.
*      perform call_delete_user using th_true.
*
*    when 'UDELGP'.
*      perform reset_session_pool using selected th_true.
*
*    when 'TRON'.
*      perform switch_user_trace using 2.
*
*    when 'TROND'.
*      perform switch_user_trace using 3.
*
*    when 'TROF'.
*      perform switch_user_trace using 0.
*
*    when 'TRDI'.
*      perform show_user_trace using selected.
*
*    when 'TRRSALL'.
*      perform wp_reset_trace(rsmon000_alv) using th_true.
*
*    when 'TRRSWP'.
*      perform wp_reset_trace(rsmon000_alv) using th_false.
*
*    when 'GUSR'.
*      perform show_terminals.
*
*    when 'BLKL'.
*      refresh selected_usr_gen.
*      loop at usr_tabl_alv.
*        if usr_tabl_alv-selected = 'X'.
*          selected_usr_gen-tid = usr_tabl_alv-tid.
*          append selected_usr_gen.
*        endif.
*      endloop.
*      perform show_memory.
*
*    when 'MBLOCK'.
*      if block_status = 1.
*        perform mark_usr_tabl_block using act_index block_opened_index.
*        refresh = th_false.
*      else.
*        block_status = 2.
*        block_opened_index = act_index.
*        refresh = th_false.
*      endif.
*
*    when 'DINFO'.
*      perform show_detailed_usr_info using selected usr_tabl_alv-tid.

    WHEN 'HELP'.

* if column header selected, rs_selfield-value contains the field name.
*      clear fieldname.
*      condense rs_selfield-value.
*      case rs_selfield-value.
*          when text-001. fieldname = 'MANDT'.
*          when text-002. fieldname = 'BNAME'.
*          when text-003. fieldname = 'TERM'.
*          when text-004. fieldname = 'TCODE'.
*          when text-005. fieldname = 'EXT_TIME'.
*          when text-006. fieldname = 'EXTMODI'.
*          when text-007. fieldname = 'EXTMODE'.
*          when text-020. fieldname = 'TID'.
*          when text-026. fieldname = 'EXT_TRACE'.
*          when text-029. fieldname = 'PROTOCOL'.
*          when text-030. fieldname = 'GUIVERSION'.
*          when text-033. fieldname = 'EXT_TYPE'.
*          when text-034. fieldname = 'EXT_STATE'.
*          when text-041. fieldname = 'RFC_TYPE'.
*          when text-048. fieldname = 'IPADDR'.
*          when text-051. fieldname = 'TOTAL_MEM_MB'.
*      endcase.
*
** if data selected, rs_selfield-fieldname contains the field name.
*      if fieldname = space.
      fieldname = rs_selfield-fieldname.
*      endif.

      PERFORM show_help_info USING fieldname rs_selfield.

*    when 'SLIST'.
*      long_usr_list = th_false.

*    when 'LLIST'.
*      long_usr_list = th_true.
  ENDCASE.

*  if block_status > 0.
*    block_status = block_status - 1.
*  endif.

*  if refresh = th_true.
** clear set of selected users
*    refresh selected_usr_gen.
*    perform build_list.
*  endif.
  rs_selfield-refresh = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.
ENDFORM.

*----------------------------------------------------------------------*
* call user info function module
*----------------------------------------------------------------------*
FORM user_info
  USING
    selected LIKE usr_tabl_alv-selected
    mandt    LIKE usr_tabl_alv-mandt
    bname    LIKE usr_tabl_alv-bname.

  IF selected <> 'X'.
    MESSAGE s101.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
    EXPORTING
      mandt = mandt
      bname = bname.
ENDFORM.

*----------------------------------------------------------------------*
* show_help_info
*----------------------------------------------------------------------*
FORM show_help_info
  USING help_fieldname
        rs_selfield    TYPE slis_selfield.

  DATA: doku       LIKE dokhl,
        answer     LIKE rsnewleng-fcode,
        docu_class LIKE dokil-id.

  DATA: BEGIN OF loc_spar OCCURS 0.
          INCLUDE STRUCTURE spar.
        DATA: END OF loc_spar.

  CLEAR loc_spar.

  docu_class = 'DE'.
  CASE help_fieldname.
    WHEN 'MANDT'.       doku-object = 'MANDT'.
    WHEN 'BNAME'.       doku-object = 'UBNAME'.
    WHEN 'TERM'.        doku-object = 'UTERM'.
    WHEN 'TCODE'.       doku-object = 'UTCODE'.
    WHEN 'EXT_TIME'.    doku-object = 'UDTIME'.
    WHEN 'EXTMODI'.     doku-object = 'UMODE'.
    WHEN 'EXT_TYPE'.    doku-object = 'SESSION_TYPE'.   docu_class = 'DT'.
    WHEN 'TID'.         doku-object = 'UTID'.           docu_class = 'DT'.
    WHEN 'EXT_STATE'.   doku-object = 'SESSION_STATE'.  docu_class = 'DT'.
    WHEN 'EXT_TRACE'.   doku-object = 'USER_TRACE'.
    WHEN 'EXT_TERMINAL'.doku-object = 'UTERM'.
    WHEN 'SERVER'.      doku-object = 'SESSION_SERVER'. docu_class = 'DT'.
    WHEN 'ROLL'.        doku-object = 'SESSION_ROLL'.   docu_class = 'DT'.
    WHEN 'PAGE'.        doku-object = 'SESSION_PAGE'.   docu_class = 'DT'.
    WHEN 'MEMSUM'.      doku-object = 'UMEMSUMCNT'.
    WHEN 'PRIVSUM'.     doku-object = 'UMEMPRICNT'.
    WHEN 'KEY'.         doku-object = 'UKEY'.
    WHEN 'PROTOCOL'.    doku-object = 'PROTOCOL'.       docu_class = 'DT'.
    WHEN 'GUIVERSION'.  doku-object = 'GUIVERSION'.     docu_class = 'DT'.
    WHEN 'RFC_TYPE'.    doku-object = 'RFC_TYPE'.       docu_class = 'DT'.
    WHEN 'TOTAL_MEM_MB'.doku-object = 'TOTAL_MEM_MB'.   docu_class = 'DT'.
    WHEN 'EXTMODE'.     doku-object = 'EXTMODE'.        docu_class = 'DT'.
    WHEN 'INTMODE'.     doku-object = 'INTMODE'.        docu_class = 'DT'.
    WHEN 'IPADDR'.      doku-object = 'IPADDR'.         docu_class = 'DT'.
    WHEN 'SPRACHE'.     doku-object = 'SPRACHE'.        docu_class = 'DT'.
    WHEN 'LOGON_DATE'.  doku-object = 'LOGON_DATE'.     docu_class = 'DT'.
    WHEN 'LOGON_TIME'.  doku-object = 'LOGON_TIME'.     docu_class = 'DT'.
    WHEN 'IMODE_MEM'.   doku-object = 'IMODE_MEM'.      docu_class = 'DT'.
    WHEN 'SNC_PNAME'.   doku-object = 'SNC_PNAME'.      docu_class = 'DT'.
  ENDCASE.

  CHECK doku-object IS NOT INITIAL.

  CALL FUNCTION 'POPUP_DISPLAY_TEXT_USER_BUTTON'
    EXPORTING
      popup_title = help_fieldname
      text_object = doku-object
      docu_class  = docu_class
    IMPORTING
      answer      = answer
    TABLES
      parameter   = loc_spar
    EXCEPTIONS
      OTHERS      = 0.
ENDFORM.
