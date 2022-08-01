*&---------------------------------------------------------------------*
*& Report  ZSHOW_BG_JOB_USER
*& Show user type of background job steps
*&---------------------------------------------------------------------*

REPORT  ZSHOW_BG_JOB_USER
  line-SIZE 187.

*tables: TBTCO, " Background job
*        TBTCP, " Background job step
*        USR02. " User

* Work area
data:
  begin of wa,
    CO_JOBNAME     type TBTCO-JOBNAME,
    CO_JOBCOUNT    type TBTCO-JOBCOUNT,
    CO_SDLUNAME    type TBTCO-SDLUNAME,         " scheduling user
    CO_SDLSTRTDT   type TBTCO-SDLSTRTDT,       " planned start date
    CO_STATUS      type TBTCO-STATUS,
**    CO_AUTHCKNAM   type TBTCO-AUTHCKNAM,     " field is always empty
    CO_AUTHCKMAN   type TBTCO-AUTHCKMAN,       " client
*    CP_JOBNAME     type TBTCP-JOBNAME,
*    CP_JOBCOUNT    type TBTCP-JOBCOUNT,
    CP_STEPCOUNT   type TBTCP-STEPCOUNT,
    CP_PROGNAME    type TBTCP-PROGNAME,
    CP_VARIANT     type TBTCP-VARIANT,
    CP_AUTHCKNAM   type TBTCP-AUTHCKNAM,       " user
    US_MANDT       type USR02-MANDT,           " client
    US_BNAME       type USR02-BNAME,           " user
    US_USTYP       type USR02-USTYP,           " user type
    US_CLASS       type USR02-CLASS,           " user group
  end of wa,
  lt_result like table of wa.

selection-screen begin of block job WITH FRAME TITLE t_job.

* Job name
selection-screen begin of line.
selection-screen COMMENT 1(30) t_JOBNAM.
select-options s_JOBNAM for wa-CO_JOBNAME.
selection-screen end of line.

* Scheduling user
selection-screen begin of line.
selection-screen COMMENT 1(30) t_JUSER.
select-options s_JUSER for wa-CO_SDLUNAME.
selection-screen end of line.

* Date range
selection-screen begin of line.
selection-screen COMMENT 1(30) t_DATE.
select-options s_DATE for wa-CO_SDLSTRTDT.
selection-screen end of line.

* Job status
* scheduled = 'P'
* released  = 'S'
* suspended = 'Z'
* ready     = 'Y'
* active    = 'R'
* finished  = 'F'
* cancelled = 'A'
selection-screen begin of line.
selection-screen COMMENT 1(30) t_status.
select-options s_status for wa-CO_status no-DISPLAY. "P=scheduled
selection-screen end of line.
selection-screen begin of line.
parameters s_STAT_P as CHECKBOX default 'X'.
selection-screen COMMENT (10) t_STAT_P.
parameters s_STAT_S as CHECKBOX default ' '.
selection-screen COMMENT (10) t_STAT_S.
*parameters s_STAT_Z as CHECKBOX default ' '.
*selection-screen COMMENT (10) t_STAT_Z.
parameters s_STAT_Y as CHECKBOX default ' '.
selection-screen COMMENT (10) t_STAT_Y.
parameters s_STAT_R as CHECKBOX default ' '.
selection-screen COMMENT (10) t_STAT_R.
parameters s_STAT_F as CHECKBOX default ' '.
selection-screen COMMENT (10) t_STAT_F.
parameters s_STAT_A as CHECKBOX default ' '.
selection-screen COMMENT (10) t_STAT_A.
selection-screen end of line.

selection-screen end of block job.


selection-screen begin of block user WITH FRAME TITLE t_user.

* Client
selection-screen begin of line.
selection-screen COMMENT 1(30) t_mandt.
select-options s_mandt for wa-us_mandt.
selection-screen end of line.

* User
selection-screen begin of line.
selection-screen COMMENT 1(30) t_bname.
select-options s_bname for wa-us_bname.
selection-screen end of line.

* User type
selection-screen begin of line.
selection-screen COMMENT 1(30) t_ustyp.
select-options s_ustyp for wa-us_ustyp.
selection-screen end of line.

* User group
selection-screen begin of line.
selection-screen COMMENT 1(30) t_class.
select-options s_class for wa-us_class.
selection-screen end of line.

selection-screen end of block user.

*&---------------------------------------------------------------------*
initialization.
*             123456789012345678901234567890
  t_job    = 'Background job'.
  t_JOBNAM = 'Job name'.
  t_Juser  = 'User who schedules the job'.
  t_DATE   = 'Start date'.
  t_status = 'Job status'. " 'Job status (P/S/Z/Y/R/F/A)'.
  t_stat_P  = 'scheduled'.
  t_stat_S  = 'released'.
*  t_stat_Z  = 'suspended'.
  t_stat_Y  = 'ready'.
  t_stat_R  = 'active'.
  t_stat_F  = 'finished'.
  t_stat_A  = 'cancelled'.

  t_user   = 'User who executes the job step'.
  t_mandt  = 'Client'.
  t_bname  = 'User'.
  t_ustyp  = 'User type'.
  t_class  = 'User group'.

*&---------------------------------------------------------------------*
START-OF-SELECTION.

  clear s_STATUS.
  s_status-option = 'EQ'.
  s_status-sign   = 'I'.
  if s_stat_P is not initial. s_status-low = 'P'. append s_status. endif.
  if s_stat_S is not initial. s_status-low = 'S'. append s_status. endif.
*  if s_stat_Z is not initial. s_status-low = 'Z'. append s_status. endif.
  if s_stat_Y is not initial. s_status-low = 'Y'. append s_status. endif.
  if s_stat_R is not initial. s_status-low = 'R'. append s_status. endif.
  if s_stat_F is not initial. s_status-low = 'F'. append s_status. endif.
  if s_stat_A is not initial. s_status-low = 'A'. append s_status. endif.

* Read job
  select
    CO~JOBNAME
    CO~JOBCOUNT
    CO~SDLUNAME
    CO~SDLSTRTDT
    CO~STATUS         " P/S/Z/Y/R/F/A
**    CO~AUTHCKNAM    " field is always empty
    CO~AUTHCKMAN      " client
*    CP~JOBNAME
*    CP~JOBCOUNT
    CP~STEPCOUNT
    CP~PROGNAME
    CP~VARIANT
    CP~AUTHCKNAM      " user
    from TBTCO as CO
    LEFT OUTER JOIN TBTCP as CP
      on  CP~JOBNAME  = CO~JOBNAME
      and CP~JOBCOUNT = CO~JOBCOUNT
*      and CP~AUTHCKNAM in s_BNAME " 'in' is not supported
    into wa
    where CO~JOBNAME   in s_JOBNAM
      and CO~SDLUNAME  in s_JUSER
      and CO~SDLSTRTDT in s_DATE
      and CO~STATUS    in s_status
      and CO~AUTHCKMAN in s_MANDT
    .

*   Read user
    clear: wa-us_mandt, wa-us_bname, wa-us_ustyp, wa-us_class.
    select single mandt bname ustyp class from usr02 client SPECIFIED
      into (wa-us_mandt, wa-us_bname, wa-us_ustyp, wa-us_class)
      where MANDT    = wa-CO_AUTHCKMAN
        and BNAME    = wa-CP_AUTHCKNAM.

    check wa-CP_AUTHCKNAM in s_bname
      and wa-us_ustyp     in s_ustyp
      and wa-us_class     in s_class.

    append wa to lt_result.
  endselect.

* Show result
  format reset.
  format color col_heading.
  write:
   /(32) 'JOBNAME',
   (8)   'JOBCOUNT',
   (12)  'SDLUNAME',
   (10)  'SDLSTRTDT',
   (6)   'STATUS',
**   (12)  'AUTHCKNAM',
*   (3)   'AUTHCKMAN',
*  (32)   'JOBNAME',
*  (8)    'JOBCOUNT',
   (10)  'STEPCOUNT',
   (40)  'PROGNAME',
   (14)  'VARIANT',
*  (12)   'AUTHCKNAM',
   (5)   'MANDT',
   (12)  'BNAME',
   (15)  'USTYP',
   (12)  'CLASS'.

  format reset.
  loop at lt_result into wa.
    write:
     /     wa-CO_JOBNAME,
           wa-CO_JOBCOUNT,
           wa-CO_SDLUNAME,
           wa-CO_SDLSTRTDT DD/MM/YYYY,
     (6)   wa-CO_STATUS,
**          wa-CO_AUTHCKNAM,
*          wa-CO_AUTHCKMAN,
*          wa-CP_JOBNAME,
*          wa-CP_JOBCOUNT,
     (10) wa-CP_STEPCOUNT,
          wa-CP_PROGNAME,
          wa-CP_VARIANT,
*          wa-CP_AUTHCKNAM,
     (5)  wa-US_mandt color COL_NORMAL,
          wa-US_bname color COL_NORMAL.

    if wa-us_ustyp = 'B'.
    write:
     (15) wa-us_ustyp color COL_POSITIVE.
    else.
    write:
     (15) wa-us_ustyp color COL_NEGATIVE.
    endif.

    write:
          wa-us_class color COL_NORMAL.
  endloop.

*&---------------------------------------------------------------------*
at LINE-SELECTION.

  clear wa.
  read CURRENT LINE FIELD VALUE wa-CO_JOBNAME wa-CO_JOBCOUNT.
  check wa-CO_JOBNAME is not initial
    and wa-CO_JOBCOUNT is not initial.

  data lt_joblist type table of tbtcjob.
  perform SHOW_JOB_SM37B(SAPLBTCH)
    TABLES lt_joblist
    USING wa-CO_JOBNAME
          wa-CO_JOBCOUNT.
