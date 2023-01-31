*&---------------------------------------------------------------------*
*& Report  ZSHOW_GWMON_LOG
*& Show settings, log and trace files of the RFC gateway
*&---------------------------------------------------------------------*
*& Author: Frank Buchholz, SAP CoE Security Services
*& Source: https://github.com/SAP-samples/security-services-tools
*&
*& 31.01.2023 Validate NI addresses and show network buffer
*& 27.01.2023 Initial version
*&
*& Limitation: Only daily logfiles are supported
*&---------------------------------------------------------------------*
REPORT zshow_gwmon_log
  MESSAGE-ID gw
  LINE-SIZE 255.

CONSTANTS: c_program_version(30) TYPE c VALUE '31.01.2023 FBT'.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text001.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS show_par AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(30) p_par FOR FIELD show_par.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS show_sec AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(30) p_sec FOR FIELD show_sec.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS show_reg AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(30) p_reg FOR FIELD show_reg.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS show_prx AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(30) p_prx FOR FIELD show_prx.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS show_buf AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(30) p_buf FOR FIELD show_buf.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS show_log AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(30) p_log FOR FIELD show_log.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS show_trc AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(30) p_trc FOR FIELD show_trc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK rng WITH FRAME TITLE text002.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) p_from FOR FIELD fromdate.
PARAMETERS fromdate TYPE sy-datum.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) p_to FOR FIELD todate.
PARAMETERS todate TYPE sy-datum.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK rng.

SELECTION-SCREEN COMMENT /1(60) ss_vers.

INITIALIZATION.

  text001 = 'Choose output'(000).
  p_par   = 'Show profile parameters'(001).
  p_sec   = 'Show secinfo'(002).
  p_reg   = 'Show reginfo'(003).
  p_prx   = 'Show prxinfo'(004).
  p_buf   = 'Show network buffer'(027).
  p_log   = 'Show log files'(005).
  p_trc   = 'Show trace files'(006).

  text002 = 'Date range to select log and trace entries'(007).
  p_from  = 'From date'(008).
  p_to    = 'To date'(009).

  CONCATENATE 'Program version:'(VER) c_program_version INTO ss_vers
    SEPARATED BY space.

  fromdate = sy-datum - 7.
  todate   = sy-datum.

START-OF-SELECTION.

  " Class CL_GW_MON
  " FLUSH_LOGFILE            Write Data to Log File
  " GET_ACL_FILE_DETAILS     Displays information about ACL file
  " GET_ADM                  Make Data from GW Administration Available
  " GET_CONN_ATTR            Display Information About a Specific Connection
  " GET_CONN_PARTNER         Display Information About a Specific Connection Partner
  " GET_CONN_TABLE           Display Gateway Connection Table
  " GET_CONN_TABLE_DETAILS   Display an Entry from the Connection Table
  " GET_CONV_INFO            Display Information About a Specific Conversation
  " GET_CVID_TABLE_DETAILS   Display CVID Table
  " GET_EXT_TABLE            List of External Programs
  " GET_GLOBAL               Gets the value
  " GET_NIBUFFER_DETAILS     Displays the NI buffer
  " GET_OWN_IPADR            Displays your own IP address
  " GET_PROGRAM_DETAILS      Displays program details
  " GET_REGISTRATION_INFO    Informationen About Regsitrations
  " GET_REGLOAD_TABLE        Displays the Reload table
  " GET_REG_TABLE            List of Registered RFC Server Programs
  " GET_RELINFO              Displays information about the release
  " GET_REMGW_TABLE          List of Remote Gateways
  " GET_REMGW_TABLE_DETAILS  Displays the remote system table
  " GET_RFC_ERRORTEXT        Gets the RFC error text
  " GET_SERVER_NAME          Returns name of active server
  " GET_STATISTIC_DETAILS    Displays statistical information
  " GET_SYS_TABLE            Displays Gateway System Table
  " GET_SYS_TABLE_DETAILS    Displays System Table
  " GET_SYS_TABLE_INTERNAL   Displays System Table
  " GET_TEST                 Getter for Variable mv_test
  " GET_TRACE                Getter for Variable mv_trace
  " GET_TRUSTED_IPADR        Displays the trusted IP address
  " GET_TRUST_INFO           Gets list of IP addresses and port numbers
  " READ_STATISTIC           Read Gateway Statistics
  " READ_TRACE_FILE          Read Trace File

  DATA subrc TYPE syst-subrc.
  DATA line TYPE gw_line.

  DATA lo_gwmon TYPE REF TO cl_gw_mon.
  CREATE OBJECT lo_gwmon.

  IF lo_gwmon->auth_check( ) NE 0.
    MESSAGE e032.
  ENDIF.

  DATA sim_mode TYPE string.
  subrc = lo_gwmon->get_parameter( EXPORTING iv_name = 'gw/sim_mode' ##NO_TEXT
                                   IMPORTING ev_value = sim_mode ).

  DATA acl_mode TYPE string.
  subrc = lo_gwmon->get_parameter( EXPORTING iv_name = 'gw/acl_mode' ##NO_TEXT
                                   IMPORTING ev_value = acl_mode ).

  DATA reg_no_conn_info TYPE string.
  subrc = lo_gwmon->get_parameter( EXPORTING iv_name = 'gw/reg_no_conn_info' ##NO_TEXT
                                   IMPORTING ev_value = reg_no_conn_info ).

  DATA sec_info TYPE string.
  subrc = lo_gwmon->get_parameter( EXPORTING iv_name = 'gw/sec_info' ##NO_TEXT
                                   IMPORTING ev_value = sec_info ).

  DATA reg_info TYPE string.
  subrc = lo_gwmon->get_parameter( EXPORTING iv_name = 'gw/reg_info' ##NO_TEXT
                                   IMPORTING ev_value = reg_info ).

  DATA prxy_info TYPE string.
  subrc = lo_gwmon->get_parameter( EXPORTING iv_name = 'gw/prxy_info' ##NO_TEXT
                                   IMPORTING ev_value = prxy_info ).

  DATA log_settings TYPE string.
  subrc = lo_gwmon->get_parameter( EXPORTING iv_name = 'gw/logging' ##NO_TEXT
                                   IMPORTING ev_value = log_settings ).

  DATA file_pattern TYPE string.
  PERFORM read_log_settings
    USING
      log_settings
    CHANGING
      file_pattern.

  " Get current log file name
  DATA: current_log_file TYPE string.
  subrc = lo_gwmon->get_parameter( EXPORTING iv_name = 'gw/logging_name' ##NO_TEXT
                                   IMPORTING ev_value = current_log_file ).

  FORMAT RESET.
  FORMAT COLOR COL_HEADING.
  WRITE: / 'Overview'(010), AT sy-linsz space.
  FORMAT RESET.
  WRITE: / 'System'(011)                COLOR COL_NORMAL, 30 sy-sysid,
         / 'Current date'(012)          COLOR COL_NORMAL, 30 sy-datum DD/MM/YYYY, sy-uzeit,
         / 'Selected date range'(013)   COLOR COL_NORMAL, 30 fromdate DD/MM/YYYY, '-', todate DD/MM/YYYY.

  IF show_par = 'X'.
    SKIP.
    WRITE: / 'gw/sim_mode'(014)         COLOR COL_NORMAL, 30 sim_mode.
    WRITE: / 'gw/reg_no_conn_info'(015) COLOR COL_NORMAL, 30 reg_no_conn_info.
    WRITE: / 'gw/acl_mode'(016)         COLOR COL_NORMAL, 30 acl_mode.
    WRITE: / 'gw/sec_info'(017)         COLOR COL_NORMAL, 30 sec_info.
    WRITE: / 'gw/reg_info'(018)         COLOR COL_NORMAL, 30 reg_info.
    WRITE: / 'gw/prxy_info'(019)        COLOR COL_NORMAL, 30 prxy_info.
    WRITE: / 'Log settings'(020)        COLOR COL_NORMAL, 30 log_settings.
    WRITE: / 'Log file pattern'(021)    COLOR COL_NORMAL, 30 file_pattern.
    WRITE: / 'Current log file'(022)    COLOR COL_NORMAL, 30 current_log_file.
  ENDIF.

  ULINE.

  IF show_sec = 'X'.
    " Get secinfo
    DATA secinfo TYPE tgw_buffer.
    subrc = lo_gwmon->get_sec_info( IMPORTING et_list = secinfo ).

    " Show secinfo
    FORMAT RESET.
    FORMAT COLOR COL_HEADING.
    WRITE: / 'secinfo'(023), sec_info, AT sy-linsz space.
    FORMAT RESET.
    LOOP AT secinfo INTO line.
      WRITE: / line.
      PERFORM convert_adress USING 'USER-HOST' line.
      PERFORM convert_adress USING 'HOST' line.
    ENDLOOP.
    ULINE.
  ENDIF.

  IF show_reg = 'X'.
    " Get reginfo
    DATA reginfo TYPE tgw_buffer.
    subrc = lo_gwmon->get_reg_info( IMPORTING et_list = reginfo ).

    " Show reginfo
    FORMAT RESET.
    FORMAT COLOR COL_HEADING.
    WRITE: / 'reginfo'(024), reg_info, AT sy-linsz space.
    FORMAT RESET.
    LOOP AT reginfo INTO line.
      WRITE: / line.
      PERFORM convert_adress USING 'USER-HOST' line.
      PERFORM convert_adress USING 'HOST' line.
    ENDLOOP.
    ULINE.
  ENDIF.

  IF show_prx = 'X'.
    " Get prxyinfo
    DATA prxyinfo TYPE tgw_buffer.
    subrc = lo_gwmon->get_proxy_info( IMPORTING et_list = prxyinfo ).

    " Show proxyinfo
    FORMAT RESET.
    FORMAT COLOR COL_HEADING.
    WRITE: / 'prxyinfo'(025), prxy_info, AT sy-linsz space.
    FORMAT RESET.
    LOOP AT prxyinfo INTO line.
      WRITE: / line.
      PERFORM convert_adress USING 'USER-HOST' line.
      PERFORM convert_adress USING 'HOST' line.
    ENDLOOP.
    ULINE.
  ENDIF.

  " Host buffer
  IF show_buf = 'X'.
    PERFORM rsnitest.
  ENDIF.

  " Log files
  IF show_log = 'X'.
    FORMAT RESET.
    FORMAT COLOR COL_HEADING.
    WRITE: / 'Logging'(026), AT sy-linsz space.
    FORMAT RESET.

    DATA date TYPE sy-datum.
    DATA file TYPE string.

    " Swap dates to prevent endless loop
    IF fromdate > todate.
      date     = fromdate.
      fromdate = todate.
      todate   = date.
    ENDIF.

    "PERFORM read_directory. " not used yet

    " First file
    date = fromdate.
    DO.

      IF date = sy-datum.
        lo_gwmon->flush_logfile( ).
      ENDIF.

      file = file_pattern.
      REPLACE '%y' IN file WITH date(4).   " year
      REPLACE '%m' IN file WITH date+4(2). " month
      REPLACE '%d' IN file WITH date+6(2). " day
      "REPLACE '%h' IN file WITH '\d\d'. " hour
      "REPLACE '%t' IN file WITH '\d\d'. " minute
      "REPLACE '%s' IN file WITH '\d\d'. " second

      PERFORM show_file USING file.

      " Next file
      date = date + 1.
      IF date > todate.
        EXIT.
      ENDIF.
    ENDDO.
    ULINE.
  ENDIF.

  " Trace files
  IF show_trc = 'X'.
    FORMAT RESET.
    FORMAT COLOR COL_HEADING.
    WRITE: / 'Trace'(027), AT sy-linsz space.
    FORMAT RESET.

    PERFORM show_file USING 'dev_rd.old' ##NO_TEXT.
    PERFORM show_file USING 'dev_rd' ##NO_TEXT.
  ENDIF.


FORM show_file
  USING file.

  DATA: line(255)  TYPE c.

  OPEN DATASET file IN TEXT MODE FOR INPUT ENCODING DEFAULT.
  IF sy-subrc <> 0.
    "MESSAGE s108 WITH file.
    RETURN.
  ENDIF.

  FORMAT COLOR COL_NORMAL.
  WRITE: / 'File'(028), 30 file, AT sy-linsz space.
  FORMAT RESET.

  DATA hide(1).
  DO.
    READ DATASET file INTO line.
    IF sy-subrc <> 0. EXIT. ENDIF.

    IF file = 'dev_rd' OR file = 'dev_rd.old' ##NO_TEXT.

      " Highlight timestamp in dev_rd: Wed Jan 18 05:21:13:794 2023
      DATA: monthtext(3), day(2), month(2), year(4).
      FIND REGEX  '^\w\w\w (\w\w\w) (\d{1,2}) \d\d:\d\d:\d\d:\d\d\d (\d\d\d\d)' ##NO_TEXT
        IN line SUBMATCHES monthtext day year.
      IF sy-subrc = 0.
        CASE monthtext.
          WHEN 'Jan'. month = '01' ##NO_TEXT.
          WHEN 'Feb'. month = '02' ##NO_TEXT.
          WHEN 'Mar'. month = '03' ##NO_TEXT.
          WHEN 'Apr'. month = '04' ##NO_TEXT.
          WHEN 'May'. month = '05' ##NO_TEXT.
          WHEN 'Jun'. month = '06' ##NO_TEXT.
          WHEN 'Jul'. month = '07' ##NO_TEXT.
          WHEN 'Aug'. month = '08' ##NO_TEXT.
          WHEN 'Sep'. month = '09' ##NO_TEXT.
          WHEN 'Oct'. month = '10' ##NO_TEXT.
          WHEN 'Nov'. month = '11' ##NO_TEXT.
          WHEN 'Dec'. month = '12' ##NO_TEXT.
        ENDCASE.
        DATA log_date TYPE sy-datum.
        log_date(4) = year.
        log_date+4(2) = month.
        log_date+6(2) = day.
        IF log_date >= fromdate AND log_date <= todate.
          hide = ' '.
        ELSE.
          hide = 'X'.
        ENDIF.
        IF hide = ' '.
          WRITE / line COLOR COL_TOTAL.
        ENDIF.
      ELSE.
        IF hide = ' '.
          " Highlight errors
          FIND REGEX  '^\*\*\* ERROR' IN line ##NO_TEXT.
          IF sy-subrc = 0.
            WRITE / line COLOR COL_NEGATIVE INVERSE ON.
          ELSE.
            WRITE / line.
          ENDIF.
        ENDIF.

      ENDIF.

    ELSE. " other files
      WRITE / line.
    ENDIF.
  ENDDO.

  CLOSE DATASET file.
ENDFORM.

FORM read_log_settings
  USING
    log_settings
  CHANGING
   file_pattern.

  "Details about log setting according to program RGWMON_LOGGING FORM get_actual_state

  DATA:
    BEGIN OF itab OCCURS 5,
      line(64) TYPE c,
    END OF itab,
    id(64)  TYPE c,
    val(64) TYPE c,
    len     TYPE i,
    idx     TYPE i,
    comp(1) TYPE c.

  SPLIT log_settings AT ' ' INTO TABLE itab.
  LOOP AT itab.
    SPLIT itab-line AT '=' INTO id val.
    CASE id.

      WHEN 'ACTION'.
        len = strlen( val ).
        DO len TIMES.
          idx = sy-index - 1.
          comp = val+idx.
          CASE comp.
            WHEN 'T'. "gw_log_tcpip = 'X'.
            WHEN 'C'. "gw_log_rfc = 'X'.
            WHEN 'O'. "gw_log_rfc2 = 'X'.
            WHEN 'E'. "gw_log_ext = 'X'.
            WHEN 'R'. "gw_log_reg = 'X'.
            WHEN 'S'. "gw_log_sec = 'X'.
            WHEN 's'. "gw_log_secd = 'X'.
            WHEN 'Z'. "gw_log_norule = 'X'.
            WHEN 'M'. "gw_log_mon = 'X'.
            WHEN 'P'. "gw_log_param = 'X'.
            WHEN 'X'. "gw_log_start = 'X'.
            WHEN 'V'. "gw_log_cvid = 'X'.
          ENDCASE.
        ENDDO.

      WHEN 'SWITCHTF'.
        DATA: switchft(12)     TYPE c,
              switchft_int(12) TYPE c.
        switchft_int = val.
        CASE switchft_int.
          WHEN 'hour'.
            "switchft = text-005.
          WHEN 'day'.
            "switchft = text-006.
          WHEN 'week'.
            "switchft = text-007.
          WHEN 'month'.
            "switchft = text-008.
          WHEN 'year'.
            "switchft = text-009.
          WHEN OTHERS.
            "switchft = text-004.
        ENDCASE.

      WHEN 'MAXSIZEKB'.
        "maxsizekb = val.

      WHEN 'FILEWRAP'.
        IF val = 'on'.
          "filewrapt = 'X'.
        ENDIF.

      WHEN 'LOGFILE'.
        file_pattern = val.

      WHEN OTHERS.
* ignore

    ENDCASE.
  ENDLOOP.
ENDFORM.

FORM read_directory. " not used yet

  DATA dir_home  TYPE string.
  subrc = lo_gwmon->get_parameter( EXPORTING iv_name = 'DIR_HOME' ##NO_TEXT
                                   IMPORTING ev_value = dir_home ).

  DATA dir_name     TYPE salfile-longname.
  "DATA FROMLINE     TYPE I.
  "DATA NRLINES      TYPE I.
  DATA file_tbl     TYPE STANDARD TABLE OF salfldir.
  DATA file_entry   TYPE salfldir.

  dir_name = dir_home.

  CALL FUNCTION 'RZL_READ_DIR_LOCAL'
    EXPORTING
      name               = dir_name
*     FROMLINE           = 0
*     NRLINES            = 1000
    TABLES
      file_tbl           = file_tbl
    EXCEPTIONS
      argument_error     = 1
      not_found          = 2
      no_admin_authority = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  SORT file_tbl BY name.

  DATA regex TYPE string.
  regex = |^| & |{ file_pattern }|.
  REPLACE '%y' IN regex WITH '\d\d\d\d'. " year
  REPLACE '%m' IN regex WITH '\d\d'.     " month
  REPLACE '%d' IN regex WITH '\d\d'.     " day
  REPLACE '%h' IN regex WITH '\d\d'.     " hour
  REPLACE '%t' IN regex WITH '\d\d'.     " minute
  REPLACE '%s' IN regex WITH '\d\d'.     " second

  DATA size TYPE i.
  LOOP AT file_tbl INTO file_entry.
    FIND REGEX regex IN file_entry-name.
    IF sy-subrc = 0.
      size = file_entry-size.
      WRITE: / file_entry-name, size.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM rsnitest. " based on report RSNITEST
  DATA:
    it_addrlist TYPE TABLE OF nis_nodeaddr_list,
    wa_addrlist TYPE          nis_nodeaddr_list,
    it_hostlist TYPE TABLE OF nis_hostbuf_list,
    wa_hostlist TYPE          nis_hostbuf_list,
    it_servlist TYPE TABLE OF nis_servbuf_list,
    wa_servlist TYPE          nis_servbuf_list.

  FORMAT RESET.
  FORMAT COLOR COL_HEADING.
  WRITE: / 'Network buffer'(028), AT sy-linsz space.
  FORMAT RESET.

*-----------------------------------------------------------------------
* LOCAL ADDRESS LIST
*-----------------------------------------------------------------------

  CALL FUNCTION 'NI_GET_ADDR_LIST'
    TABLES
      addrlist = it_addrlist
    EXCEPTIONS
      einval   = 1
      OTHERS   = 2.

  IF sy-subrc <> 0.
    WRITE: 'Error in function NI_GET_ADDR_LIST'(029) COLOR COL_NEGATIVE.
  ENDIF.

  IF it_addrlist IS NOT INITIAL.
    FORMAT COLOR COL_NORMAL.
    WRITE: / 'Local address list'(030), AT sy-linsz space.
    FORMAT RESET.

    LOOP AT it_addrlist INTO wa_addrlist.
      NEW-LINE.
      WRITE:
        wa_addrlist-nodeaddr.

      IF wa_addrlist-protocol = 0.
        WRITE 'unknown'.
      ELSEIF wa_addrlist-protocol = 1.
        WRITE 'UDS'.
      ELSEIF wa_addrlist-protocol = 2.
        WRITE 'IPv4'.
      ELSEIF wa_addrlist-protocol = 4.
        WRITE 'IPv6'.
      ENDIF.

      IF wa_addrlist-addrtype = 0.
        " undefined
      ELSEIF wa_addrlist-addrtype = 1.
        WRITE: '/', 'any'.
      ELSEIF wa_addrlist-addrtype = 2.
        WRITE: '/', 'loopback'.
      ELSEIF wa_addrlist-addrtype = 4.
        WRITE: '/', 'multicast'.
      ELSEIF wa_addrlist-addrtype = 8.
        WRITE: '/', 'link local'.
      ELSEIF wa_addrlist-addrtype = 16.
        WRITE: '/', 'site local'.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-----------------------------------------------------------------------
* HOST BUFFER
*-----------------------------------------------------------------------

  CALL FUNCTION 'NI_GET_BUFFER_LIST'
    TABLES
      hostlist = it_hostlist
*     SERVLIST =
    EXCEPTIONS
      einval   = 1
      eintern  = 2
      OTHERS   = 3.

  IF sy-subrc <> 0.
    WRITE: 'Error in function NI_GET_BUFFER_LIST'(031) COLOR COL_NEGATIVE.
  ENDIF.

  IF it_hostlist IS NOT INITIAL.
    FORMAT COLOR COL_NORMAL.
    WRITE: / 'Host buffer'(032), AT sy-linsz space.
    FORMAT RESET.

    LOOP AT it_hostlist INTO wa_hostlist.
      NEW-LINE.
      WRITE:
        wa_hostlist-id.

      IF wa_hostlist-status = 1.
        WRITE 'valid  '.
      ELSEIF wa_hostlist-status = 0.
        WRITE 'unknown'.
      ELSE.
        WRITE 'invalid status'.
      ENDIF.

      WRITE:
        wa_hostlist-typ,
        wa_hostlist-ipv4addr.

      IF NOT wa_hostlist-ipv6addr = '~'.
        WRITE wa_hostlist-ipv6addr.
      ENDIF.

      WRITE:
        wa_hostlist-hostname.
    ENDLOOP.
  ENDIF.

*-----------------------------------------------------------------------
* SERV BUFFER
*-----------------------------------------------------------------------

  CALL FUNCTION 'NI_GET_BUFFER_LIST'
    TABLES
*     hostlist =
      servlist = it_servlist
    EXCEPTIONS
      einval   = 1
      eintern  = 2
      OTHERS   = 3.

  IF sy-subrc <> 0.
    WRITE: 'Error in function NI_GET_BUFFER_LIST'(031) COLOR COL_NEGATIVE.
  ENDIF.

  IF it_servlist IS NOT INITIAL.
    FORMAT COLOR COL_NORMAL.
    WRITE: / 'Service buffer'(034), AT sy-linsz space.
    FORMAT RESET.

    LOOP AT it_servlist INTO wa_servlist.
      NEW-LINE.
      WRITE:
        wa_servlist-id.

      IF wa_servlist-status = 1.
        WRITE 'valid  '.
      ELSEIF wa_servlist-status = 0.
        WRITE 'unknown'.
      ELSE.
        WRITE 'invalid status'.
      ENDIF.

      IF wa_servlist-servno = 0.
        WRITE '    - '.
      ELSE.
        WRITE wa_servlist-servno.
      ENDIF.

      WRITE:
        wa_servlist-servname.
    ENDLOOP.
  ENDIF.

ENDFORM.


FORM convert_adress
    USING
      token " USER_HOST or HOST
      line. " Line from secifo, reginfo, prxyinfo

  " Convert NI adresses using functions NI_ADDR_TO_NAME (or NI_ADDR_TO_FQ_NAME?) respective NI_NAME_TO_ADDR
  " Other useful function might be NI_PORT_TO_NAME, NI_NAME_TO_PORT

  DATA value TYPE string.
  DATA result TYPE string.

  DATA nodeaddr      TYPE ni_nodeaddr.
  DATA hostname      TYPE ni_hostname.      "  63 char
*  DATA hostname_long TYPE ni_hostname_long. " 256 char
  DATA protocol      TYPE ni_proto_fam.     "   0 Undefined Protocol Family
  "   1 Local Protocol (UDS)
  "   2 Internet Protocol Version 4 (IPv4)
  "   4 Internet Protocol Version 6 (IPv6)
  DATA addrtype      TYPE ni_addr_type.     "   0 Undefined Address Type
  "   1 Any Address
  "   2 Loopback Address
  "   4 Multicast Address
  "   8 Link Local Address
  "  16 Site Local Address
  DATA local         TYPE ni_addr_local.

  " Convert IP address to hostname
  FIND REGEX  ` ` && token && `=([0-9.:/*]*)` ##NO_TEXT
    IN line SUBMATCHES value.
  IF sy-subrc = 0 AND value IS NOT INITIAL.
    IF value NA '*/'. " ignore entries with submask like /24 or .*)
      nodeaddr = value.
      CALL FUNCTION 'NI_ADDR_TO_NAME' " or NI_ADDR_TO_FQ_NAME
        EXPORTING
          nodeaddr      = nodeaddr
        IMPORTING
          hostname      = hostname
*          hostname_long = hostname_long
          protocol      = protocol
          addrtype      = addrtype
          local         = local
        EXCEPTIONS
          ehost_unknown = 1
          einval        = 2
          etoo_small    = 3
          OTHERS        = 4.
      IF sy-subrc = 0.
        result = hostname.
      ELSEIF sy-subrc = 1.
        WRITE: '#', `unknown` && ` ` && token COLOR COL_NEGATIVE.
      ELSE.
        WRITE: '#', 'Error in NI_ADDR_TO_NAME' COLOR COL_NEGATIVE.
      ENDIF.
    ENDIF.

  ELSE.

    " Convert hostname to IP address
    FIND REGEX  ` ` && token && `=([^ ]*) ` ##NO_TEXT
      IN line SUBMATCHES value.
    IF sy-subrc = 0 AND value IS NOT INITIAL.
      IF value NE 'local' AND value NE 'internal' AND value NA '*'. " Ignore some values and an submask
        hostname = value.
*        hostname_long = value.
        CALL FUNCTION 'NI_NAME_TO_ADDR'
          EXPORTING
           hostname      = hostname
*            hostname_long = hostname_long
*           PROTOCOL_RESTRICTION       = 0
          IMPORTING
            nodeaddr      = nodeaddr
            protocol      = protocol
            addrtype      = addrtype
            local         = local
          EXCEPTIONS
            ehost_unknown = 1
            einval        = 2
            OTHERS        = 3.
        IF sy-subrc = 0.
          result = nodeaddr.
        ELSEIF sy-subrc = 1.
          WRITE: '#', `unknown` && ` ` && token COLOR COL_NEGATIVE.
        ELSE.
          WRITE: '#', 'Error in NI_NAME_TO_ADDR' COLOR COL_NEGATIVE.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.

  " Show result as comment and
  IF result IS NOT INITIAL.
    WRITE: '#', token && `=` && result COLOR COL_POSITIVE.
    CASE protocol.
      WHEN 1. WRITE 'UDS'.
        "WHEN 2. WRITE 'IPv4'.
      WHEN 4. WRITE 'IPv6'.
    ENDCASE.
    CASE addrtype.
      WHEN 1. WRITE 'Any'.
      WHEN 2. WRITE 'Loopback'.
      WHEN 4. WRITE 'Multicase'.
      WHEN 8. WRITE 'Link local'.
    ENDCASE.
    IF local IS NOT INITIAL.
      WRITE 'local'.
    ENDIF.
  ENDIF.

ENDFORM.
