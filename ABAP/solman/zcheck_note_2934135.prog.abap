*&---------------------------------------------------------------------*
*& Report ZCHECK_NOTE_2934135
*& Check the implementation status of note 2934135 for connected Java systems
*&---------------------------------------------------------------------*
*& published via KBA 2953257 - Check implementation of Note 2934135 based on data from SLD
*& Source: https://github.com/SAP-samples/security-services-tools
*&---------------------------------------------------------------------*
REPORT zcheck_note_2934135
  LINE-SIZE 255.

CONSTANTS: c_program_version(30) TYPE c VALUE '28.08.2020 FA7'.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) t_sys FOR FIELD p_sys.
DATA c_sys TYPE char80.
SELECT-OPTIONS p_sys FOR c_sys.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) t_comp FOR FIELD p_comp.
DATA c_comp TYPE char80.
SELECT-OPTIONS p_comp FOR c_comp DEFAULT 'LMCTC'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_call AS CHECKBOX USER-COMMAND enter.
SELECTION-SCREEN COMMENT 3(29) t_call FOR FIELD p_call.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK srv WITH FRAME TITLE b_srv.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) t_srv FOR FIELD p_srv MODIF ID srv.
PARAMETERS p_srv TYPE string LOWER CASE DEFAULT '/CTCWebService/CTCWebServiceBean/' MODIF ID srv.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_head RADIOBUTTON GROUP meth DEFAULT 'X' MODIF ID srv.
SELECTION-SCREEN COMMENT 3(29) t_head FOR FIELD p_head MODIF ID srv.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_get RADIOBUTTON GROUP meth MODIF ID srv.
SELECTION-SCREEN COMMENT 3(29) t_get FOR FIELD p_get MODIF ID srv.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_post RADIOBUTTON GROUP meth MODIF ID srv.
SELECTION-SCREEN COMMENT 3(29) t_post FOR FIELD p_post MODIF ID srv.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_redir AS CHECKBOX DEFAULT 'X' MODIF ID srv. " follow redirect automatically
SELECTION-SCREEN COMMENT 3(29) t_redir FOR FIELD p_redir MODIF ID srv.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) t_time FOR FIELD p_time MODIF ID srv.
PARAMETERS p_time TYPE i DEFAULT 1 MODIF ID srv.  " timeout per call
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) t_appl FOR FIELD p_appl MODIF ID srv.
PARAMETERS p_appl TYPE ssfapplssl DEFAULT 'ANONYM' MATCHCODE OBJECT f4strustssl MODIF ID srv.
SELECTION-SCREEN COMMENT 50(31) t_appl2 FOR FIELD p_appl MODIF ID srv.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK srv.


SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_xlog AS CHECKBOX.           " extended log
SELECTION-SCREEN COMMENT 3(29) t_xlog FOR FIELD p_xlog.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

INITIALIZATION.
  CONCATENATE 'Program version:'(ver) c_program_version INTO ss_vers
     SEPARATED BY space.

  sy-title = 'Check Note 2934135'(TIT).

  t_sys    = 'System'(s01).
  t_comp   = 'Software Component'(s02).

  t_call   = 'Test Service'(s03).
  b_srv    = 'Service parameters'(s04).
  t_srv    = 'Service'(s05).
  t_head   = 'http method HEAD'(s06).
  t_get    = 'http method GET'(s07).
  t_post   = 'http method POST'(s08).
  t_redir  = 'resolve redirect directly'(s09).
  t_time   = 'http timeout'(s10).
  t_appl   = 'SSL Client Identity'(s11).
  t_appl2  = '(skip https calls if empty)'(s12).
  t_xlog   = 'Extended log'(s13).

AT SELECTION-SCREEN OUTPUT.

* Hide test service parameters
  IF p_call IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = 'SRV'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.


START-OF-SELECTION.
  FORMAT RESET.

  AUTHORITY-CHECK OBJECT 'S_RZL_ADM'
           ID 'ACTVT' FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE e149(00) WITH 'S_RZL_ADM'.
  ENDIF.

  IF p_call IS NOT INITIAL.
    FORMAT COLOR COL_HEADING.
    WRITE: / 'Test Service'(s03), p_srv, AT sy-linsz space.
    FORMAT RESET.
  ENDIF.

  PERFORM main.

FORM main.

  DATA wbem_cim_err        TYPE REF TO cx_wbem_cim_err.

  " Create SLD client
  TRY.
      DATA(wbem_client) = cl_lmdb_cim_factory=>get_domain_manager( )->get_domain_context( cl_lmdb_cim_domain=>ldb )->create_wbem_client( ).
    CATCH cx_wbem_cim_err INTO wbem_cim_err.
  ENDTRY.

  " get table of Java systems from SLD
  TRY.
      DATA(java_systems) = wbem_client->enumerate_instancenames( cl_cim_elementname=>create( 'SAP_J2EEEngineCluster' ) ).
    CATCH cx_wbem_cim_err INTO wbem_cim_err.
  ENDTRY.

  DATA java_system TYPE REF TO cl_cim_reference.
  LOOP AT java_systems INTO java_system.
    DATA(percent) = 100 * sy-tabix / lines( java_systems ).

*    write: / 'CIM ref string', java_system->TO_STRING( ).

*   Keys:
*     ASSOCIATION
*     Abstract
*     Aggregation
*     CIM_Namespace
*     CreationClassName
*     Exception
*     INDICATION
**    Name                        SYS on host
*     SAP_J2EEEngineCluster

*   Properties:
*     activehardwarekey           R1234567890
**    caption
*     creationclassname           SAP_J2EEEngineCluster
*     description
*     distribution
*     elementname
*     enableddefault
*     enabledstate
*     extsidname                Long SID
*     healthstate
*     identifyingdescriptions
*     installationpath
*     installdate
*     itadministrationcaption
*     itadministrationpriority  0
*     itadministrationrole      DEV
*     lastservingstatusupdate
*     licenseexpiration
*     manufacturer
**    name                      SYS.SystemHome.host
*     nameformat
*     operationalstatus
*     otherenabledstate
*     otheridentifyinginfo
*     primaryownercontact
*     primaryownername
*     requestedstate
*     roles
**    sapsystemname             SYS
*     servingstatus
*     startuptime
*     status
*     statusdescriptions
**    systemhome                host
*     systemlicensenumber       000000000123456789
*     systemnumber
*     timeoflaststatechange
**    version                 7.50.3301.458716.20200601133628

    TRY.
        DATA(name)          = java_system->get_key( cl_cim_elementname=>create( 'Name' ) )-value ##NO_TEXT.

        DATA(extsidname)    = wbem_client->get_property(
                                instanceref     = java_system
                                propertyname    = cl_cim_elementname=>create( 'ExtSIDName' ) )-value.
        DATA(caption)       = wbem_client->get_property(
                                instanceref     = java_system
                                propertyname    = cl_cim_elementname=>create( 'Caption' ) )-value ##NO_TEXT.
        DATA(version)       = wbem_client->get_property(
                                instanceref     = java_system
                                propertyname    = cl_cim_elementname=>create( 'Version' ) )-value ##NO_TEXT.
        DATA(sapsystemname) = wbem_client->get_property(
                                instanceref     = java_system
                                propertyname    = cl_cim_elementname=>create( 'SAPSystemName' ) )-value.
        DATA(systemhome)    = wbem_client->get_property(
                                instanceref     = java_system
                                propertyname    = cl_cim_elementname=>create( 'SystemHome' ) )-value.

      CATCH cx_wbem_cim_err.
    ENDTRY.

    CHECK sapsystemname IN p_sys.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = CONV string( percent )
        text       = caption.

    SKIP.
    FORMAT COLOR COL_GROUP.
    WRITE: / extsidname, sapsystemname, 'on'(001), systemhome, AT 79 space.
    FORMAT RESET.

    PERFORM show_data_supplier_status
      USING
        wbem_client
        java_system
        name.

    WRITE: / 'Version'(027), version.
    PERFORM check_software
      USING
        wbem_client
        java_system.

    PERFORM check_service
      USING
        wbem_client
        java_system.

  ENDLOOP. "java_systems

ENDFORM.

FORM show_data_supplier_status
  USING
    wbe_mclient  TYPE REF TO  if_wbem_sap_client
    java_system  TYPE REF TO  cl_cim_reference
    name         TYPE string.

  DATA wbem_cim_err        TYPE REF TO cx_wbem_cim_err.

* How to find supplier information?
* This draft code is based on report AI_LMDB_EASY_SUPPORT form OUTPUT_INSTANCE_INFO

*  DATA lo_bo_cim_inst       TYPE REF TO cl_lmdb_bo_cim_instance.

  DATA lt_node_id           TYPE lmdb_t_generic_id.
  DATA ls_node_id           TYPE lmdb_s_generic_id.

  DATA lt_refstring_suppl   TYPE lmdb_t_bo_refstring_suppl.
  DATA ls_refstring_suppl   TYPE lmdb_s_bo_refstring_suppl.

  DATA lt_string            TYPE string_table.
  DATA lv_string            TYPE string.
  DATA min_date             TYPE sy-datum.

  min_date = sy-datum - 30.

* refstring = 'sap_j2eeenginecluster.creationclassname="SAP_J2EEEngineCluster",name="FAJ.SystemHome.lddbfa7"'.
  CONCATENATE 'sap_j2eeenginecluster.creationclassname="SAP_J2EEEngineCluster",name="' name '"' INTO ls_node_id-lmdb_refstring.
  APPEND ls_node_id TO lt_node_id.

  TRY.

      DATA(lo_bo_cim_inst) = cl_lmdb_bo_cim_instance=>s_get_instance( ).
      lo_bo_cim_inst->get_refstring_suppl_by_refstr(
        EXPORTING
          it_node_id   = lt_node_id
        IMPORTING
          et_node_data = lt_refstring_suppl ).

*      DATA lt_supplier          TYPE lmdb_t_bo_suppl_name.
*      DATA ls_supplier          TYPE lmdb_s_bo_suppl_name.
*      DATA lt_message            TYPE /plmb/t_spi_msg.
*      lo_bo_cim_inst->get_supplier_by_refstring(
*        EXPORTING
*          it_node_id   = lt_node_id
*        IMPORTING
*          et_result    = lt_supplier
*          et_message   = lt_message ).

      READ TABLE lt_refstring_suppl INTO ls_refstring_suppl INDEX 1.
      IF sy-subrc = 0.
*        write: / 'Supplier LastUIModificationTime'(023),
*                 35 ls_refstring_suppl-lastuimodificationdate,
*                 ls_refstring_suppl-lastuimodificationtime,
*                 'by'(024),
*                 ls_refstring_suppl-lastuimodifiedby.

        IF ls_refstring_suppl-lastdsdate < min_date.
          FORMAT COLOR COL_TOTAL.
        ENDIF.
        WRITE: / 'Supplier LastDataSynchTime'(025),
                 35 ls_refstring_suppl-lastdsdate,
                 ls_refstring_suppl-lastdstime.
        FORMAT RESET.

*       Method get_refstring_suppl_by_refstr produces a table of strings which are prepared for web based user interface
        SPLIT ls_refstring_suppl-suppliernames AT cl_abap_char_utilities=>newline INTO TABLE lt_string.
*        WRITE / 'SupplierNames'(026).
        LOOP AT lt_string INTO lv_string.
          "WRITE: /3, lv_string.

*         replace escaped charcters
          REPLACE ALL OCCURRENCES OF '&#x20;' IN lv_string WITH '|'. " It's not possible to replace into space directly
          TRANSLATE lv_string USING '| '.
          REPLACE ALL OCCURRENCES OF '&#x21;' IN lv_string WITH '!'.
          REPLACE ALL OCCURRENCES OF '&#x22;' IN lv_string WITH '"'.
          REPLACE ALL OCCURRENCES OF '&#x28;' IN lv_string WITH '('.
          REPLACE ALL OCCURRENCES OF '&#x29;' IN lv_string WITH ')'.
          REPLACE ALL OCCURRENCES OF '&#x2f;' IN lv_string WITH '/'.
          REPLACE ALL OCCURRENCES OF '&lt;'   IN lv_string WITH '<'.
          REPLACE ALL OCCURRENCES OF '&gt;'   IN lv_string WITH '>'.
          REPLACE ALL OCCURRENCES OF '&#x3a;' IN lv_string WITH ':'.
          REPLACE ALL OCCURRENCES OF '&#x3b;' IN lv_string WITH ';'.
          REPLACE ALL OCCURRENCES OF '&#x3d;' IN lv_string WITH '='.
          REPLACE ALL OCCURRENCES OF '&#x5b;' IN lv_string WITH '['.
          REPLACE ALL OCCURRENCES OF '&#x5d;' IN lv_string WITH ']'.
          REPLACE ALL OCCURRENCES OF '&#xc4;' IN lv_string WITH 'Ä'.
          REPLACE ALL OCCURRENCES OF '&#xe4;' IN lv_string WITH 'ä'.

          DATA: part1 TYPE string,
                part2 TYPE string.
          SPLIT lv_string AT ':' INTO part1 part2.

          WRITE: /3(30) part1, " supplier name
                 34     part2. " date, time, description

        ENDLOOP.
      ENDIF.

    CATCH cx_wbem_cim_err INTO wbem_cim_err.
  ENDTRY.

  RETURN.

* The following looks more elegant, however, I do not know hot to transforkm java_system into key_instance

  DATA key_instance        TYPE REF TO if_lmdb_supplier_key_instance.
  DATA data_supplier_info  TYPE REF TO cl_lmdb_data_supplier_info.
  DATA abstract_entity     TYPE REF TO cl_lmdb_abstract_entity.

  TRY.

*     key_instance = ...

      data_supplier_info = key_instance->get_data_supplier_info( ).

      abstract_entity ?= key_instance.

      WRITE: / abstract_entity->get_caption( ), abstract_entity->get_cim_reference( )->classname->name.

      IF data_supplier_info->supplied_by_data_supplier( ) = abap_true.
        WRITE: /  'Last Update by Data Supplier',
               60 |{ data_supplier_info->last_update_by_data_supplier( ) TIMESTAMP = ISO }|.
      ENDIF.

      IF data_supplier_info->supplied_by_own_data_supplier( ) = abap_true.
        WRITE: /  'Last Update by own Data Supplier',
               60 |{ data_supplier_info->last_update_by_own_supplier( ) TIMESTAMP = ISO }|.
      ENDIF.

      IF data_supplier_info->has_manual_changes( ) = abap_true.
        WRITE: /  'Last Manual Change in LMDB by', data_supplier_info->last_manual_change_by( ),
               60 |{ data_supplier_info->last_manual_change( ) TIMESTAMP = ISO }|.
      ENDIF.
    CATCH cx_wbem_cim_err INTO wbem_cim_err.
  ENDTRY.

ENDFORM.


FORM check_software
  USING
    wbem_client  TYPE REF TO  if_wbem_sap_client
    java_system  TYPE REF TO  cl_cim_reference.

* CIM Class Diagram
* X3A http://mo-c81a86caf.mo.sap.corp:50300/webdynpro/resources/sap.com/tc~sld~wd~classbrowser/Components/com.sap.sld.wd.classbrowser.ClassBrowser/sap_installed_software.pdf

  DATA wbem_cim_err        TYPE REF TO cx_wbem_cim_err.

  " Get from SLD: Generic Application System -> Installed SW Components on Application Server -> Installed Software Component
  TRY.
      DATA(sw_component_list) = wbem_client->associatornames(
                                  objectref   = java_system                 "SAP_ApplicationSystem
                                  role        = cl_cim_elementname=>create( 'System' ) ##NO_TEXT
                                  assocclass  = cl_cim_elementname=>create( 'SAP_InstalledSWComponentOnApplicationSystem' )
                                  resultrole  = cl_cim_elementname=>create( 'Software' ) ##NO_TEXT
                                  resultclass = cl_cim_elementname=>create( 'SAP_InstalledSoftwareComponent' )
                                ).
    CATCH cx_wbem_cim_err INTO wbem_cim_err.
  ENDTRY.

  LOOP AT sw_component_list INTO DATA(sw_component).

    TRY.
        DATA(sw_comp_name)    = sw_component->get_key( cl_cim_elementname=>create( 'Name' ) )-value ##NO_TEXT.

        DATA(sw_comp_version) = wbem_client->get_property(
                                  instanceref  = sw_component
                                  propertyname = cl_cim_elementname=>create( 'Version' ) )-value ##NO_TEXT.
        " other properties: buildnumber, installdate, installtype, installationpath, primaryownercontact, primaryownername
      CATCH cx_wbem_cim_err INTO wbem_cim_err.
    ENDTRY.

    CHECK sw_comp_name IN p_comp.

    WRITE: / 'Software Component'(s02), sw_comp_name, sw_comp_version.

    " Get from SLD: Installed Software Component -> Installed Support Package Software Component -> Installed Support Package
    TRY.
        DATA(support_package_list) = wbem_client->associatornames(
                                       objectref   = sw_component                "SAP_InstalledSoftwareComponent
                                       role        = cl_cim_elementname=>create( 'Antecedent' ) ##NO_TEXT
                                       assocclass  = cl_cim_elementname=>create( 'SAP_InstalledSupportPackageSoftwareComponent' )
                                       resultrole  = cl_cim_elementname=>create( 'Dependent' ) ##NO_TEXT
                                       resultclass = cl_cim_elementname=>create( 'SAP_InstalledSupportPackage' )
                                     ).
      CATCH cx_wbem_cim_err INTO wbem_cim_err.
    ENDTRY.

    LOOP AT support_package_list INTO DATA(support_package). " some systems have multiple records: it would be sufficent to take the latest entry

      TRY.
          " Keys: Name, SoftwareElementID, Version, ...
          DATA(sp_name)           = support_package->get_key( cl_cim_elementname=>create( 'Name' ) )-value ##NO_TEXT.
          DATA(sp_version)        = support_package->get_key( cl_cim_elementname=>create( 'Version' ) )-value ##NO_TEXT.

          " Properties: BuildNumber, Caption, Description, InstallDate, InstallType, PatchLevel, ...
          DATA(patchlevel)        = wbem_client->get_property(
                                      instanceref  = support_package
                                      propertyname = cl_cim_elementname=>create( 'PatchLevel' ) )-value.

          WRITE: 'SP'(002), sp_version, 'patch'(003), patchlevel.

          IF sw_comp_name = 'LMCTC'.
            PERFORM check_note_2934135
              USING
                sw_comp_name
                sw_comp_version
                sp_version
                patchlevel.
          ENDIF.

        CATCH cx_wbem_cim_err INTO wbem_cim_err.
      ENDTRY.

    ENDLOOP. "support_package

  ENDLOOP. "sw_component

ENDFORM.

FORM check_note_2934135
  USING
    sw_comp_name    TYPE string
    sw_comp_version TYPE string
    sp_version      TYPE string
    patchlevel      TYPE string
    .

* Note 2934135 - [CVE-2020-6287] Multiple Vulnerabilities in SAP NetWeaver AS JAVA (LM Configuration Wizard)
* https://launchpad.support.sap.com/#/notes/2934135
* Version 13 from 28.07.2020
* Software Component Version  Support Package Patch Level
* LM CONFIGURATION WIZARD 7.50  SP012 000002
* LM CONFIGURATION WIZARD 7.50  SP013 000003
* LM CONFIGURATION WIZARD 7.50  SP014 000002
* LM CONFIGURATION WIZARD 7.50  SP015 000002
* LM CONFIGURATION WIZARD 7.50  SP016 000002
* LM CONFIGURATION WIZARD 7.50  SP017 000002
* LM CONFIGURATION WIZARD 7.40  SP018 000001
* LM CONFIGURATION WIZARD 7.50  SP018 000001
* LM CONFIGURATION WIZARD 7.30  SP019 000001
* LM CONFIGURATION WIZARD 7.40  SP019 000001
* LM CONFIGURATION WIZARD 7.50  SP019 000000
* LM CONFIGURATION WIZARD 7.30  SP020 000001
* LM CONFIGURATION WIZARD 7.40  SP020 000001
* LM CONFIGURATION WIZARD 7.50  SP020 000000
* LM CONFIGURATION WIZARD 7.40  SP021 000001
* LM CONFIGURATION WIZARD 7.30  SP021 000000
* LM CONFIGURATION WIZARD 7.40  SP022 000000
* LM CONFIGURATION WIZARD 7.31  SP023 000001
* LM CONFIGURATION WIZARD 7.40  SP023 000000
* LM CONFIGURATION WIZARD 7.31  SP024 000001
* LM CONFIGURATION WIZARD 7.31  SP025 000001
* LM CONFIGURATION WIZARD 7.31  SP026 000001
* LM CONFIGURATION WIZARD 7.31  SP027 000000
* LM CONFIGURATION WIZARD 7.31  SP028 000000

  CHECK sw_comp_name = 'LMCTC'.

  DATA:
    sp    TYPE i,
    patch TYPE i.

  sp    = sp_version.
  patch = patchlevel.

  IF   ( sw_comp_version < '7.30'                                   ).
    " not affected
    WRITE: 55 'System is not affected'(004) COLOR COL_POSITIVE.

  ELSEIF
       ( sw_comp_version = '7.30' AND sp =  019 AND patch >= 000001 )
    OR ( sw_comp_version = '7.30' AND sp =  020 AND patch >= 000001 )
    OR ( sw_comp_version = '7.30' AND sp >= 021                     )

    OR ( sw_comp_version = '7.31' AND sp =  023 AND patch >= 000001 )
    OR ( sw_comp_version = '7.31' AND sp =  024 AND patch >= 000001 )
    OR ( sw_comp_version = '7.31' AND sp =  025 AND patch >= 000001 )
    OR ( sw_comp_version = '7.31' AND sp =  026 AND patch >= 000001 )
    OR ( sw_comp_version = '7.31' AND sp =  027 AND patch >= 000000 )
    OR ( sw_comp_version = '7.31' AND sp >= 028                     )

    OR ( sw_comp_version = '7.40' AND sp =  018 AND patch >= 000001 )
    OR ( sw_comp_version = '7.40' AND sp =  019 AND patch >= 000001 )
    OR ( sw_comp_version = '7.40' AND sp =  020 AND patch >= 000001 )
    OR ( sw_comp_version = '7.40' AND sp =  021 AND patch >= 000001 )
    OR ( sw_comp_version = '7.40' AND sp =  022 AND patch >= 000000 )
    OR ( sw_comp_version = '7.40' AND sp >= 023                     )

    OR ( sw_comp_version = '7.50' AND sp =  012 AND patch >= 000002 )
    OR ( sw_comp_version = '7.50' AND sp =  013 AND patch >= 000003 )
    OR ( sw_comp_version = '7.50' AND sp =  014 AND patch >= 000002 )
    OR ( sw_comp_version = '7.50' AND sp =  015 AND patch >= 000002 )
    OR ( sw_comp_version = '7.50' AND sp =  016 AND patch >= 000002 )
    OR ( sw_comp_version = '7.50' AND sp =  017 AND patch >= 000002 )
    OR ( sw_comp_version = '7.50' AND sp =  018 AND patch >= 000001 )
    OR ( sw_comp_version = '7.50' AND sp =  019 AND patch >= 000000 )
    OR ( sw_comp_version = '7.50' AND sp >= 020                     )

    OR ( sw_comp_version > '7.50'                                   ).
    " ok
    WRITE: 55 'Note 2934135 is installed'(005) COLOR COL_POSITIVE.

  ELSE.
    " not ok
    WRITE: 55 'Note 2934135 is missing'(006) COLOR COL_NEGATIVE.

  ENDIF.

ENDFORM.

FORM check_service
  USING
    wbem_client  TYPE REF TO  if_wbem_sap_client
    java_system  TYPE REF TO  cl_cim_reference.

  " Get SAP AS Java Service Instance(s)
  TRY.
      DATA(scs_list) = wbem_client->associatornames(
                         objectref  = java_system
                         assocclass = cl_cim_elementname=>create( 'SAP_J2EEEngineServiceInstance' )
                       ).
    CATCH cx_wbem_cim_err.
  ENDTRY.

  " Check count of SCS
  IF lines( scs_list ) = 0.
    WRITE: /'System has no SCS'(007) COLOR COL_TOTAL.
    RETURN.
  ELSEIF lines( scs_list ) > 1.
    WRITE / |System has { lines( scs_list ) } SCS| COLOR COL_TOTAL. "(008)
  ENDIF.

  LOOP AT scs_list INTO DATA(scs).

    TRY.
        DATA(name) = java_system->get_key( cl_cim_elementname=>create( 'Name' ) )-value ##NO_TEXT.
        "WRITE: / 'SCS', name.

        DATA(hosts) = wbem_client->associators(
                        objectref   = scs
                        resultclass = cl_cim_elementname=>create( 'SAP_ComputerSystem' )
                      ).
      CATCH cx_wbem_cim_err.
    ENDTRY.

    " Check count of hosts
    IF lines( hosts ) = 0.
      WRITE: / 'SCS has no host'(009) COLOR COL_TOTAL.
      CONTINUE.
    ELSEIF lines( hosts ) > 1.
      WRITE / |SCS has { lines( hosts ) } hosts| COLOR COL_TOTAL. "(010)
    ENDIF.

    TRY.
        DATA(ports) = wbem_client->associators(
                        objectref  = scs
                        assocclass = cl_cim_elementname=>create( 'SAP_BCCentralServiceMsgHttpPort' )
                      ).
      CATCH cx_wbem_cim_err.
    ENDTRY.

    " Check count of ports
    IF lines( ports ) = 0.
      WRITE: / 'SCS has no port'(011).
      CONTINUE.
    ENDIF.

    " try all combinations of hosts and ports
    LOOP AT  hosts INTO DATA(host).
      LOOP AT ports INTO DATA(port).

        TRY.
            DATA(hostname) = host->object->get_value( cl_cim_elementname=>create( 'fqdname' ) ).
            IF hostname IS INITIAL.
              hostname     = host->object->get_value( cl_cim_elementname=>create( 'name' ) ).
            ENDIF.

            DATA(portnumber) = port->object->get_value( cl_cim_elementname=>create( 'portnumber' ) ).
            DATA(protocol)   = port->object->get_value( cl_cim_elementname=>create( 'protocol' ) ).
          CATCH cx_wbem_cim_err.
        ENDTRY.

        " Skip the call if SSL application is not set
        IF p_appl IS INITIAL AND protocol = 'https'.
          CONTINUE.
        ENDIF.

        DATA(url) = |{ protocol }://{ hostname }:{ portnumber }|.
        WRITE: / url.

        IF p_call IS NOT INITIAL.
          DATA redirect TYPE i.
          IF p_redir IS INITIAL.
            redirect = if_http_client=>co_disabled. " resolve redirects step by step
          ELSE.
            redirect = if_http_client=>co_enabled.  " resolve redirects directly
          ENDIF.
          PERFORM call_url USING url redirect.
        ENDIF.

      ENDLOOP. "ports
    ENDLOOP. "hosts

  ENDLOOP. "scs_list

ENDFORM.

FORM call_url
  USING
    url          TYPE string
    redirect     TYPE i
    .

  DATA:
    msg         TYPE string.

  GET RUN TIME FIELD DATA(t0).

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = url
      ssl_id             = p_appl
    IMPORTING
      client             = DATA(http_client)
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc IS NOT INITIAL.
    WRITE: 80 'create http client failed'(012) COLOR COL_TOTAL.
    PERFORM write_time USING t0.
    RETURN.
  ENDIF.

  cl_http_utility=>set_request_uri( request = http_client->request
                                    uri     = p_srv ).
  "http_client->request->set_header_field(
  "  EXPORTING
  "    name  = if_http_header_fields_sap=>request_uri
  "    value = p_srv
  ").

  http_client->propertytype_accept_cookie     = if_http_client=>co_enabled.
  http_client->propertytype_redirect          = redirect.

  http_client->propertytype_logon_popup       = if_http_client=>co_disabled.
  http_client->propertytype_send_sap_passport = if_http_client=>co_disabled.

  IF p_head IS NOT INITIAL.
    http_client->request->set_method( 'HEAD' ).
  ELSEIF p_post IS NOT INITIAL.
    http_client->request->set_method( if_http_request=>co_request_method_post ). "default: if_http_request=>co_request_method_get
  ENDIF.

  CALL METHOD http_client->send
    EXPORTING
      timeout                    = p_time
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5.
  IF sy-subrc IS NOT INITIAL.
    http_client->get_last_error(
      IMPORTING
        "code    = sy-subrc
        message = msg
    ).
    http_client->close( ).
    WRITE: 85 'send failed'(013) COLOR COL_TOTAL.
    PERFORM write_time USING t0.
    PERFORM write_msg USING msg.
    RETURN.
  ENDIF.

  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.
  IF sy-subrc IS NOT INITIAL.
    http_client->get_last_error(
     IMPORTING
       "code    = sy-subrc
       message = msg
   ).
    http_client->close( ).
    WRITE: 85 'receive failed'(014) COLOR COL_TOTAL.
    PERFORM write_time USING t0.
    PERFORM write_msg USING msg.
    RETURN.
  ENDIF.

  " get http status code from header (not used)
  DATA(http_code_c) = http_client->response->get_header_field( '~status_code' ).
  " get http status code
  http_client->response->get_status(
    IMPORTING
      code   = DATA(http_code)
      reason = msg
  ).

  WRITE AT 80(4) http_code LEFT-JUSTIFIED.

  CASE http_code.
    WHEN 200.    WRITE 'service is active'(015)      COLOR COL_NEGATIVE. " 200 OK
    WHEN 300 OR 301 OR 302 OR 303 OR 304 OR 305 OR 306 OR 307 OR 308.
      WRITE 'redirect'(019)               COLOR COL_TOTAL.    " 300-308 redirect
    WHEN 401.    WRITE 'Unauthorized'(016)           COLOR COL_POSITIVE. " 401 Unauthorized / authentication is required
    WHEN 403.    WRITE 'Forbidden'(017)              COLOR COL_POSITIVE. " 403 Forbidden
    WHEN 404.    WRITE 'service is not active'(018)  COLOR COL_POSITIVE. " 404 Not Found / not active
    WHEN 405.    WRITE 'Method Not Allowed'(020)     COLOR COL_TOTAL.    " 405 Method Not Allowed (i.e. GET instead of POST)
    WHEN 415.    WRITE 'Unsupported Media Type'(021) COLOR COL_TOTAL.    " 415 Unsupported Media Type
    WHEN OTHERS. WRITE 'others'(022)                 COLOR COL_TOTAL.
  ENDCASE.

  PERFORM write_time USING t0.

  " show header fields
  IF p_xlog IS NOT INITIAL.
    DATA header_fields TYPE tihttpnvp.
    http_client->response->get_header_fields( CHANGING fields = header_fields ).
    IF p_xlog = 'X'.
      LOOP AT header_fields INTO DATA(header_field).
        WRITE:/ header_field-name, 30 header_field-value.
      ENDLOOP.
      SKIP.
    ENDIF.
  ENDIF.

  " resolve redirect
  IF http_code >= 300 AND http_code <= 308.
    DATA(location) = http_client->response->get_header_field( 'location' ) ##NO_TEXT.
    REPLACE p_srv IN location WITH ''. " not neccessary but looks better
    WRITE: / location.

    "call it again (and resolve any further redirects directly)
    IF redirect = if_http_client=>co_disabled.
      PERFORM call_url USING location if_http_client=>co_enabled.
    ENDIF.
  ENDIF.

  CALL METHOD http_client->close
    EXCEPTIONS
      http_invalid_state = 1 ##SUBRC_OK
      OTHERS             = 2.

ENDFORM.

FORM write_time
  USING
    t0 TYPE i.

  GET RUN TIME FIELD DATA(t1).
  WRITE: '(' NO-GAP, CONV string( ( t1 - t0 ) / 1000000 ) NO-GAP, 'sec)'.
ENDFORM.

FORM write_msg
  USING
    msg TYPE string.

  IF p_xlog IS INITIAL.
    " show a part of the message in current line
    WRITE (140) msg.
  ELSE.
    " show all message lines
    SPLIT msg AT cl_abap_char_utilities=>newline INTO TABLE DATA(msg_lines).
    LOOP AT msg_lines INTO msg.
      WRITE: / msg.
    ENDLOOP.
    SKIP.
  ENDIF.

ENDFORM.
