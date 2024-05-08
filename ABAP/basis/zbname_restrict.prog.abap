*&---------------------------------------------------------------------*
*& Report ZBNAME_RESTRICT
*&---------------------------------------------------------------------*
*& Check user names concerning parameter BNAME_RESTRICT in table PRGN_CUST
*& Note 1731549 https://me.sap.com/notes/1731549
*& https://help.sap.com/docs/ABAP_PLATFORM_NEW/c6e6d078ab99452db94ed7b3b7bbcccf/8a922c9d7bca45c9b29bff3c59b344df.html
*&---------------------------------------------------------------------*
REPORT zbname_restrict.

START-OF-SELECTION.

  SELECT mandt, bname FROM usr02 CLIENT SPECIFIED INTO TABLE @DATA(lt_user)
    ORDER BY PRIMARY KEY.

  DATA:
    username TYPE xubname, " required for method cl_susr_basic_tools=>check_username
    user     TYPE string,  " allows to show chinese characters
    length   TYPE i,
    length2  TYPE i,
    count    TYPE i.
  FIELD-SYMBOLS:
    <username> TYPE x.
  ASSIGN username TO <username> CASTING.

  FORMAT RESET.
  WRITE:
    /(3)   'Clt'        COLOR COL_HEADING,
    5(12)  'User'       COLOR COL_HEADING,
    18(48) 'User (hex)' COLOR COL_HEADING,
    67(7)  'Result'     COLOR COL_HEADING.
  FORMAT RESET.

  LOOP AT lt_user INTO DATA(ls_user).
    username = ls_user-bname.
    user     = ls_user-bname.
    length = strlen( username ).
    length2 = length * 2. " Unicode factor
    IF ( cl_susr_basic_tools=>check_username( username ) = abap_false ).
      WRITE:
        /  ls_user-mandt,                             " 3 char
        "5  ls_user-bname(length) COLOR COL_NORMAL,   " max 12 char, does not show chinese characters
        "5  username(length)      COLOR COL_NORMAL,   " max 12 char, does not show chinese characters
        5  user(length)           COLOR COL_NORMAL,   " max 12 char
        18 <username>(length2)    COLOR COL_NORMAL,   " max 12*4=48 char
        67 'invalid'              COLOR COL_NEGATIVE.
      ADD 1 TO count.
    ENDIF.
  ENDLOOP.
  WRITE: / count, 'users are invalid'.
