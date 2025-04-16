*&---------------------------------------------------------------------*
*& Report ZTOUCH_PROJVIEW_DEP_PROGS
*&---------------------------------------------------------------------*
*& Note 3565944 - [CVE-2025-30015] Memory Corruption vulnerability in SAP NetWeaver and ABAP Platform (Application Server ABAP)
*& https://me.sap.com/notes/3565944
*& Note 3577258 - FAQ for security note 3565944
*& https://me.sap.com/notes/3577258
*&---------------------------------------------------------------------*
REPORT ztouch_projview_dep_progs.

PARAMETERS test     RADIOBUTTON GROUP fnk DEFAULT 'X'. " Just show the list
PARAMETERS touch    RADIOBUTTON GROUP fnk.             " Touch programs (delayed generation if needed)
PARAMETERS generate RADIOBUTTON GROUP fnk.             " Generate programs directly (slow!)

PARAMETERS listprog AS CHECKBOX.                       " List all relevant programs, too.

START-OF-SELECTION.

  " Authority check
  IF touch = 'X' OR generate = 'X'.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
     ID 'DEVCLASS' DUMMY
     ID 'OBJTYPE'  FIELD 'PROG'
     ID 'OBJNAME'  DUMMY
     ID 'P_GROUP'  DUMMY
     ID 'ACTVT'    FIELD '07'. " 03 display, 07, generate - we need a simple auth check for prog systems
  ELSE.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
     ID 'DEVCLASS' DUMMY
     ID 'OBJTYPE'  FIELD 'PROG'
     ID 'OBJNAME'  DUMMY
     ID 'P_GROUP'  DUMMY
     ID 'ACTVT'    FIELD '03'. " 03 display, 07, generate - we need a simple auth check for prog systems
  ENDIF.
  IF sy-subrc <> 0.
    MESSAGE e009(s3). " You are not authorized for this function
  ENDIF.

  " Prepare selection for report TOUCHTAB
  DATA params TYPE STANDARD TABLE OF rsparams.
  DATA param TYPE rsparams.
  param-kind = 'P'.
  param-selname = 'TABNAME'.
  APPEND param TO params.
  param-selname = 'GENERATE'.
  param-low = generate.
  APPEND param TO params.

  FIELD-SYMBOLS <tabname_param> TYPE rsparams.
  READ TABLE params WITH KEY selname = 'TABNAME' ASSIGNING <tabname_param>.

  FORMAT RESET.

  " Get projection views
  SELECT tabname
    FROM dd02l
    INTO TABLE @DATA(projview_names)
    WHERE tabclass = 'VIEW'
      AND viewclass = 'P'
    ORDER BY tabname.

  " Process projection views
  DATA: progcnt TYPE i.
  LOOP AT projview_names INTO DATA(projview_name).

    " Get programs related to the projection view
    DATA tabname TYPE d010tab-tabname.
    tabname = projview_name.
    SELECT
        ddic~master AS progname,
        prog~sdate                " Field SDATE will be touched to trigger regeneration
      FROM d010tab AS ddic
      JOIN trdir AS prog
        ON prog~name = ddic~master
      INTO TABLE @DATA(programs)
      WHERE tabname = @tabname
      ORDER BY ddic~master.
    DATA(dbcnt) = sy-dbcnt.
    progcnt += dbcnt.

    WRITE: / projview_name COLOR COL_GROUP, dbcnt COLOR COL_NORMAL.

    IF listprog = 'X'.
      LOOP AT programs INTO DATA(program).
        WRITE: / program-progname UNDER dbcnt.
        "WRITE program-sdate DD/MM/YYYY.
      ENDLOOP.
    ENDIF.

    " Submit report TOUCHTAB
    IF dbcnt > 0 AND ( touch = 'X' OR generate = 'X' ).
      <tabname_param>-low = projview_name.
      SUBMIT touchtab AND RETURN
        WITH SELECTION-TABLE params.
    ENDIF.
  ENDLOOP.

  " Show total number
  WRITE: / 'Total programs' COLOR COL_TOTAL, progcnt COLOR COL_TOTAL UNDER dbcnt.
