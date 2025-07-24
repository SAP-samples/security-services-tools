*&---------------------------------------------------------------------*
*& Report ZSAP4ME_NOTE_SEARCH
*&---------------------------------------------------------------------*
*& Construct an URL to search for notes in the SAP Support Portal
*& Author: Frank Buchholz, SAP CoE Security Services
*& Published on: https://github.com/SAP-samples/security-services-tools
*& 20.07.2025 Initial version
*&---------------------------------------------------------------------*

REPORT zsap4me_note_search.

CONSTANTS: c_program_version(30) TYPE c VALUE '20.07.2025 S44'.

" Search term
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) ss_q.
  PARAMETERS q TYPE string LOWER CASE.
SELECTION-SCREEN END OF LINE.

" Components (Exact / Start with)
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ss_comp.
  SELECT-OPTIONS comp FOR ('AKHKOMPO') MATCHCODE OBJECT apch_akh_comp. "or H_AKH_COMP
SELECTION-SCREEN END OF LINE.

" Software Components (Exact)
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ss_scomp.
  SELECT-OPTIONS swcomp FOR ('DLVUNIT')." MATCHCODE OBJECT scprcomp. " or H_CVERS.
SELECTION-SCREEN END OF LINE.

" Released on (Free)
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(33) ss_date.
  PARAMETERS datefrom TYPE sy-datum.
  SELECTION-SCREEN COMMENT (07) ss_space.
  SELECTION-SCREEN COMMENT (05) ss_to.
  PARAMETERS dateto TYPE sy-datum.
SELECTION-SCREEN END OF LINE.

" Support Packages (greater than)
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ss_ptchg.
  SELECT-OPTIONS ptchg FOR ('PATCH')." MATCHCODE OBJECT sh_spam_patch.
SELECTION-SCREEN END OF LINE.

" Support Packages (Exact)
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) ss_patch.
  SELECT-OPTIONS patch FOR ('PATCH')." MATCHCODE OBJECT sh_spam_patch.
SELECTION-SCREEN END OF LINE.

" Priority
SELECTION-SCREEN BEGIN OF BLOCK prio WITH FRAME TITLE ss_prio.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS prio_01  AS CHECKBOX.
    SELECTION-SCREEN COMMENT 3(60) ss_prio1. " 01
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS prio_02  AS CHECKBOX.
    SELECTION-SCREEN COMMENT 3(60) ss_prio2. " 02
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS prio_03  AS CHECKBOX.
    SELECTION-SCREEN COMMENT 3(60) ss_prio3. " 03
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS prio_04  AS CHECKBOX.
    SELECTION-SCREEN COMMENT 3(60) ss_prio4. " 04
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS prio_06  AS CHECKBOX.
    SELECTION-SCREEN COMMENT 3(60) ss_prio6. "06
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK prio.

" Category
SELECTION-SCREEN BEGIN OF BLOCK catg WITH FRAME TITLE ss_catg.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS catlg AS CHECKBOX.    " O Announcement of Legal Change, T Correction of legal function, H Legal Change
    SELECTION-SCREEN COMMENT 3(60) ss_catlg.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS faq      AS CHECKBOX.  " B Consulting, K FAQ, 01 How To
    SELECTION-SCREEN COMMENT 3(60) ss_faq.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS perf     AS CHECKBOX.  " P Performance
    SELECTION-SCREEN COMMENT 3(60) ss_perf.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK catg.

" Document type
SELECTION-SCREEN BEGIN OF BLOCK type WITH FRAME TITLE ss_type.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS sapnotes AS CHECKBOX.  " SAPNOTES
    SELECTION-SCREEN COMMENT 3(60) ss_note.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS kba      AS CHECKBOX.  " KBA
    SELECTION-SCREEN COMMENT 3(60) ss_kba.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS secu     AS CHECKBOX.  " SECU
    SELECTION-SCREEN COMMENT 3(60) ss_secu.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS hotnews  AS CHECKBOX.  " HOTNEWS
    SELECTION-SCREEN COMMENT 3(60) ss_hot.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS legal    AS CHECKBOX.  " LEGAL_CHANGE
    SELECTION-SCREEN COMMENT 3(60) ss_legal.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK type.

" SAP Security Patch Day
SELECTION-SCREEN BEGIN OF BLOCK sec WITH FRAME TITLE ss_sec.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS sec_no RADIOBUTTON GROUP sec DEFAULT 'X'. " No restriction
    SELECTION-SCREEN COMMENT 3(60) ss_secno.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS sec_pday  RADIOBUTTON GROUP sec. " Patch Day SAP Security Notes
    SELECTION-SCREEN COMMENT 3(60) ss_pday.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS sec_spin RADIOBUTTON GROUP sec. " Support Package SAP Security Notes
    SELECTION-SCREEN COMMENT 3(60) ss_spin.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK sec.

" Sort
SELECTION-SCREEN BEGIN OF BLOCK sort WITH FRAME TITLE ss_sort.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS sort_rel RADIOBUTTON GROUP sort DEFAULT 'X'. " Relevance
    SELECTION-SCREEN COMMENT 3(60) ss_rel.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS sort_on  RADIOBUTTON GROUP sort. " ReleasedOn
    SELECTION-SCREEN COMMENT 3(60) ss_on.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS sort_num RADIOBUTTON GROUP sort. " Number
    SELECTION-SCREEN COMMENT 3(60) ss_num.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK sort.

SELECTION-SCREEN COMMENT 1(60) ss_vers.

INITIALIZATION.

  ss_q     = 'Search term'.

  ss_comp  = 'Components (use right *)'.

  ss_scomp = 'Software Components'.

  ss_date  = 'Released on (Free)'.
  ss_to    = 'to'.

  ss_ptchg = 'Support Packages, greater than'.
  ss_patch = 'Support Packages, equal'.

  ss_prio  = 'Priority'.
  ss_prio1 = 'HotNews / Proactive Notifications'.
  ss_prio2 = 'Correction with high priority'.
  ss_prio3 = 'Correction with medium priority'.
  ss_prio4 = 'Correction with low priority'.
  ss_prio6 = 'Recommendations / Additional Info'.

  ss_catg  = 'Category'.
  ss_catlg = 'Legal Change (announcement, correction)'.
  ss_faq   = 'Consulting, FAQ, How To'.
  ss_perf  = 'Performance'.

  ss_type  = 'Document type'.
  ss_note  = 'SAP Notes'.
  ss_kba   = 'SAP Knowlege Base Articles'.
  ss_secu  = 'SAP Security Notes'.
  ss_hot   = 'SAP HotNews'.
  ss_legal = 'SAP Legal Change Notes'.

  ss_sec   = 'SAP Security Patch Day'.
  ss_secno = 'No restriction'.
  ss_pday  = 'Patch Day SAP Security Notes'.
  ss_spin  = 'Support Package SAP Security Notes'.

  ss_sort  = 'Sort'.
  ss_rel   = 'Relevance'.
  ss_on    = 'Released On'.
  ss_num   = 'Number'.

  ss_sort  = 'Sort'.

  CONCATENATE 'Program version from'(100) c_program_version INTO ss_vers
    SEPARATED BY space.

  " Restrict SELECT-OPTIONS to allow Include-EQ only
  DATA(options) = VALUE sscr_opt_list_tab(
    (
      name       = 'EQ'
      options-eq = 'X'
    )
    (
      name       = 'EQ_CP'
      options-eq = 'X'
      options-cp = 'X'
    )
  ).
  DATA(assignment) = VALUE sscr_ass_tab(
    (
      kind    = 'S'                " A(ll), B(lock), S(elect-Option)
      name    = 'COMP'             " Screen Field Name on Selection Screen
      sg_main = 'I'                " (only) I, SPACE = both
      op_main = 'EQ_CP'            " Name of the corresponding option definition
    )
    (
      kind    = 'S'                " A(ll), B(lock), S(elect-Option)
      name    = 'SWCOMP'           " Screen Field Name on Selection Screen
      sg_main = 'I'                " (only) I, SPACE = both
      op_main = 'EQ'               " Name of the corresponding option definition
    )
    (
      kind    = 'S'                " A(ll), B(lock), S(elect-Option)
      name    = 'PTCHG'            " Screen Field Name on Selection Screen
      sg_main = 'I'                " (only) I, SPACE = both
      op_main = 'EQ'               " Name of the corresponding option definition
    )
    (
      kind    = 'S'                " A(ll), B(lock), S(elect-Option)
      name    = 'PATCH'            " Screen Field Name on Selection Screen
      sg_main = 'I'                " (only) I, SPACE = both
      op_main = 'EQ'               " Name of the corresponding option definition
    )
  ).
  DATA(restrictions) = VALUE sscr_restrict(
    opt_list_tab = options
    ass_tab      = assignment
  ).
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
*     PROGRAM                =
      restriction            = restrictions
*     DB                     = ' '
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR swcomp-low.
  PERFORM at_selection_screen_f4_swcomp
    USING 'SWCOMP-LOW'.

FORM at_selection_screen_f4_swcomp
  USING field TYPE dynfnam.

  TYPES:
    BEGIN OF ts_f4_swcomp,
      component TYPE comp_props-component,
    END OF ts_f4_swcomp,
    tt_f4_swcomp TYPE STANDARD TABLE OF ts_f4_swcomp.

  STATICS:
    f4_value     TYPE ts_f4_swcomp,
    f4_value_tab TYPE tt_f4_swcomp.

  IF f4_value_tab IS INITIAL.
    DATA tt_components TYPE TABLE OF comp_props.
    CALL FUNCTION 'OCS_GET_INSTALLED_COMPS'
*       EXPORTING
*         IV_PATCHABLE_ONLY       =
*         IV_ACTIVE_ONLY          =
*         IV_BUFFERED             = 'X'
*         IV_LANGUAGE             = SY-LANGU
      TABLES
*       TT_COMPTAB       =
        et_components    = tt_components
*       ET_COMPLAYER     =
*       ET_CVERS_SUB     =
*       ET_CPK           =
      EXCEPTIONS
        no_release_found = 1
        wrong_release    = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    LOOP AT tt_components INTO DATA(components).
      f4_value-component = components-component.
      APPEND f4_value TO f4_value_tab.
    ENDLOOP.
  ENDIF.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.
  "DATA field TYPE dynfnam.
  DATA stepl TYPE sy-stepl.
  GET CURSOR FIELD field LINE stepl.
  STATICS mark_tab TYPE ddshmarks.
  DATA return_tab TYPE TABLE OF ddshretval.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COMPONENT'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = field
      stepl           = stepl
      value_org       = 'S'
      multiple_choice = 'X'
      mark_tab        = mark_tab
    TABLES
*     field_tab       = field_tab
      value_tab       = f4_value_tab
      return_tab      = return_tab " surprisingly required to get lower case values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

  " Copy selected values to selection table
  DATA(swcomp_old) = swcomp.
  LOOP AT return_tab INTO DATA(return_value).
    " Store the list of selected items
    READ TABLE f4_value_tab INTO f4_value WITH KEY component = return_value-fieldval.
    IF sy-subrc = 0.
      DATA(tabix) = sy-tabix.
      READ TABLE mark_tab FROM tabix TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        INSERT tabix INTO mark_tab INDEX sy-tabix.
      ENDIF.
    ENDIF.

    " Add selected items to the selection
    swcomp-low    = return_value-fieldval.
    swcomp-sign   = 'I'.
    swcomp-option = 'EQ'.
    APPEND swcomp.
  ENDLOOP.
  " Ensure, that the first item is set/preserved
  swcomp = swcomp_old.
  IF swcomp IS INITIAL.
    READ TABLE return_tab INTO return_value INDEX 1.
    swcomp-low    = return_value-fieldval.
    swcomp-sign   = 'I'.
    swcomp-option = 'EQ'.
  ENDIF.

  " Refreh the selection screen
  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'.
ENDFORM.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR ptchg-low.
  PERFORM at_selection_screen_f4_ptchg
    USING 'PTCHG-LOW'.

FORM at_selection_screen_f4_ptchg
  USING field TYPE dynfnam.

  TYPES:
    BEGIN OF ts_f4_ptchg,
      component TYPE comp_props-component,
      sp        TYPE comp_props-sp,
    END OF ts_f4_ptchg,
    tt_f4_ptchg TYPE STANDARD TABLE OF ts_f4_ptchg.

  STATICS:
    f4_value     TYPE ts_f4_ptchg,
    f4_value_tab TYPE tt_f4_ptchg.

  IF f4_value_tab IS INITIAL.
    DATA tt_components TYPE TABLE OF comp_props.
    CALL FUNCTION 'OCS_GET_INSTALLED_COMPS'
*       EXPORTING
*         IV_PATCHABLE_ONLY       =
*         IV_ACTIVE_ONLY          =
*         IV_BUFFERED             = 'X'
*         IV_LANGUAGE             = SY-LANGU
      TABLES
*       TT_COMPTAB       =
        et_components    = tt_components
*       ET_COMPLAYER     =
*       ET_CVERS_SUB     =
*       ET_CPK           =
      EXCEPTIONS
        no_release_found = 1
        wrong_release    = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    LOOP AT tt_components INTO DATA(components)
      WHERE sp IS NOT INITIAL.
      f4_value-component = components-component.
      f4_value-sp        = components-sp.
      APPEND f4_value TO f4_value_tab.
    ENDLOOP.
  ENDIF.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.
  "DATA field TYPE dynfnam.
  DATA stepl TYPE sy-stepl.
  GET CURSOR FIELD field LINE stepl.
  STATICS mark_tab TYPE ddshmarks.
  DATA return_tab TYPE TABLE OF ddshretval.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SP'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = field
      stepl           = stepl
      value_org       = 'S'
      multiple_choice = 'X'
      mark_tab        = mark_tab
    TABLES
*     field_tab       = field_tab
      value_tab       = f4_value_tab
      return_tab      = return_tab " surprisingly required to get lower case values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

  " Copy selected values to selection table
  DATA(ptchg_old) = ptchg.
  LOOP AT return_tab INTO DATA(return_value).
    " Store the list of selected items
    READ TABLE f4_value_tab INTO f4_value WITH KEY sp = return_value-fieldval.
    IF sy-subrc = 0.
      DATA(tabix) = sy-tabix.
      READ TABLE mark_tab FROM tabix TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        INSERT tabix INTO mark_tab INDEX sy-tabix.
      ENDIF.
    ENDIF.

    " Add selected items to the selection
    ptchg-low    = return_value-fieldval.
    ptchg-sign   = 'I'.
    ptchg-option = 'EQ'.
    APPEND ptchg.
  ENDLOOP.
  " Ensure, that the first item is set/preserved
  ptchg = ptchg_old.
  IF ptchg IS INITIAL.
    READ TABLE return_tab INTO return_value INDEX 1.
    ptchg-low    = return_value-fieldval.
    ptchg-sign   = 'I'.
    ptchg-option = 'EQ'.
  ENDIF.

  " Refreh the selection screen
  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'.
ENDFORM.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR patch-low.
  PERFORM at_selection_screen_f4_patch
    USING 'PATCH-LOW'.

FORM at_selection_screen_f4_patch
  USING field TYPE dynfnam.

  TYPES:
    BEGIN OF ts_f4_patch,
      component TYPE comp_props-component,
      sp        TYPE comp_props-sp,
    END OF ts_f4_patch,
    tt_f4_patch TYPE STANDARD TABLE OF ts_f4_patch.

  STATICS:
    f4_value     TYPE ts_f4_patch,
    f4_value_tab TYPE tt_f4_patch.

  IF f4_value_tab IS INITIAL.
    DATA tt_components TYPE TABLE OF comp_props.
    CALL FUNCTION 'OCS_GET_INSTALLED_COMPS'
*       EXPORTING
*         IV_PATCHABLE_ONLY       =
*         IV_ACTIVE_ONLY          =
*         IV_BUFFERED             = 'X'
*         IV_LANGUAGE             = SY-LANGU
      TABLES
*       TT_COMPTAB       =
        et_components    = tt_components
*       ET_COMPLAYER     =
*       ET_CVERS_SUB     =
*       ET_CPK           =
      EXCEPTIONS
        no_release_found = 1
        wrong_release    = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    LOOP AT tt_components INTO DATA(components)
      WHERE sp IS NOT INITIAL.
      f4_value-component = components-component.
      f4_value-sp        = components-sp.
      APPEND f4_value TO f4_value_tab.
    ENDLOOP.
  ENDIF.

  DATA(progname) = sy-repid.
  DATA(dynnum)   = sy-dynnr.
  "DATA field TYPE dynfnam.
  DATA stepl TYPE sy-stepl.
  GET CURSOR FIELD field LINE stepl.
  STATICS mark_tab TYPE ddshmarks.
  DATA return_tab TYPE TABLE OF ddshretval.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SP'
      dynpprog        = progname
      dynpnr          = dynnum
      dynprofield     = field
      stepl           = stepl
      value_org       = 'S'
      multiple_choice = 'X'
      mark_tab        = mark_tab
    TABLES
*     field_tab       = field_tab
      value_tab       = f4_value_tab
      return_tab      = return_tab " surprisingly required to get lower case values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

  " Copy selected values to selection table
  DATA(patch_old) = patch.
  LOOP AT return_tab INTO DATA(return_value).
    " Store the list of selected items
    READ TABLE f4_value_tab INTO f4_value WITH KEY sp = return_value-fieldval.
    IF sy-subrc = 0.
      DATA(tabix) = sy-tabix.
      READ TABLE mark_tab FROM tabix TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        INSERT tabix INTO mark_tab INDEX sy-tabix.
      ENDIF.
    ENDIF.

    " Add selected items to the selection
    patch-low    = return_value-fieldval.
    patch-sign   = 'I'.
    patch-option = 'EQ'.
    APPEND patch.
  ENDLOOP.
  " Ensure, that the first item is set/preserved
  patch = patch_old.
  IF patch IS INITIAL.
    READ TABLE return_tab INTO return_value INDEX 1.
    patch-low    = return_value-fieldval.
    patch-sign   = 'I'.
    patch-option = 'EQ'.
  ENDIF.

  " Refreh the selection screen
  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'.
ENDFORM.

START-OF-SELECTION.

  DATA url TYPE string.

  " Example:
*   https://me.sap.com/mynotes/expertsearch/{
*X    "searchTerm":"search",
*     "fuzzyThreshold":"0.8",
*X    "componentsStartWith":["COMP_START"],
*X    "componentsExact":["COMP"],
*     "excludedComponentsExact":["COMP_EXCL"],
*     "releasedOnPreDefined":"FilterReleasedOnDefined___EQ___Last7Days",
*X    "releasedOnFree":"2025-07-13 - 2025-07-14",
*X    "softComp":["swcomp"],
*     "softCompVersion":["swcompversion"],
*X    "supportPackageGreaterThan":["sp_greater_than"],
*X    "supportPackageEqual":["sp_equal"],
*     "product":[{"key":"FilterProduct___EQ___73555000100800001701","text":"ORTEC FOR SAP S/4HANA"}],
*     "productVersion":[{"key":"FilterProductVersion___EQ___73555000100900003102","text":"ORTEC FOR SAP S/4HANA 1.0"}],
*X    "priority":["Priority___EQ___01","Priority___EQ___02","Priority___EQ___03","Priority___EQ___04","Priority___EQ___06"],
*     "category":["Category___EQ___P","Category___EQ___H"],
*X    "documentType":["FilterDocumentType___EQ___SAPNOTES","FilterDocumentType___EQ___KBA","FilterDocumentType___EQ___SECU","FilterDocumentType___EQ___HOTNEWS","FilterDocumentType___EQ___LEGAL_CHANGE"],
*X    "SAPSecurityPatchDay":"PATCHDAY" or "SPIN"
*X    "orderBy":"ReleasedOn" or "Number"
*   }

* Categories:

* Category___EQ___O  Announcement of Legal Change
* Category___EQ___T  Correction of legal function
* Category___EQ___H  Legal Change

* Category___EQ___B  Consulting
* Category___EQ___K  FAQ
* Category___EQ___01 How To

* Category___EQ___P Performance

  " Parameter for search term: "searchTerm":"search"
  IF q IS NOT INITIAL.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"searchTerm":"` && q && `search",`.
  ENDIF.

  " Filter for application components: "componentsStartWith":["COMP_START"],"componentsExact":["COMP"]
  IF comp IS NOT INITIAL.
    DATA: componentsstartwith TYPE string, componentsexact TYPE string.
    LOOP AT comp INTO DATA(comp_value).
      TRANSLATE comp_value-low TO UPPER CASE.

      IF substring(
        val = comp_value-low
        off = strlen( comp_value-low ) - 1
        len = 1
      ) = '*'.
        IF componentsstartwith IS NOT INITIAL. componentsstartwith = componentsstartwith && `,`. ENDIF.
        componentsstartwith = componentsstartwith && `"` && substring( val = comp_value-low  len = strlen( comp_value-low ) - 1 ) && `"`.
      ELSE.
        IF componentsexact IS NOT INITIAL. componentsexact = componentsexact && `,`. ENDIF.
        componentsexact = componentsexact && `"` && comp_value-low && `"`.
      ENDIF.
    ENDLOOP.
    IF componentsstartwith IS NOT INITIAL.
      IF url IS NOT INITIAL. url = url && `,`. ENDIF.
      url = url && `"componentsStartWith":[` && componentsstartwith && `]`.
    ENDIF.
    IF componentsexact IS NOT INITIAL.
      IF url IS NOT INITIAL. url = url && `,`. ENDIF.
      url = url && `"componentsExact":[` && componentsexact && `]`.
    ENDIF.
  ENDIF.

  " Filter for Released on (Free): "releasedOnFree":"2025-07-13 - 2025-07-14"
  IF datefrom IS NOT INITIAL OR dateto IS NOT INITIAL.
    IF datefrom IS INITIAL.
      datefrom = '20000101'.
    ENDIF.
    IF dateto IS INITIAL.
      dateto = sy-datum.
    ENDIF.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"releasedOnFree":` && `"` && datefrom+0(4) && `-` && datefrom+4(2) && `-` && datefrom+6(2) && ` - `
                                            && dateto+0(4)   && `-` && dateto+4(2)   && `-` && dateto+6(2)   && `"`.
  ENDIF.

  " Filter for software components: "softComp":["swcomp"]
  IF swcomp IS NOT INITIAL.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"softComp":[`.
    LOOP AT swcomp INTO DATA(swcomp_value).
      IF sy-tabix > 1. url = url && `,`. ENDIF.
      TRANSLATE swcomp_value-low TO UPPER CASE.
      url = url && `"` && swcomp_value-low && `"`.
    ENDLOOP.
    url = url && `]`.
  ENDIF.

  " Filter for support packages (greater than): "supportPackageGreaterThan":["sp_greater_than"]
  IF ptchg IS NOT INITIAL.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"supportPackageGreaterThan":[`.
    LOOP AT ptchg INTO DATA(ptchg_value).
      IF sy-tabix > 1. url = url && `,`. ENDIF.
      TRANSLATE ptchg_value-low TO UPPER CASE.
      url = url && `"` && ptchg_value-low && `"`.
    ENDLOOP.
    url = url && `]`.
  ENDIF.

  " Filter for support packages: "supportPackageEqual":["sp_equal"]
  IF patch IS NOT INITIAL.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"supportPackageEqual":[`.
    LOOP AT patch INTO DATA(patch_value).
      IF sy-tabix > 1. url = url && `,`. ENDIF.
      TRANSLATE patch_value-low TO UPPER CASE.
      url = url && `"` && patch_value-low && `"`.
    ENDLOOP.
    url = url && `]`.
  ENDIF.

  " Filter for priority: "priority":["Priority___EQ___01","Priority___EQ___02","Priority___EQ___03","Priority___EQ___04","Priority___EQ___06"]
  IF   prio_01 IS NOT INITIAL
    OR prio_02 IS NOT INITIAL
    OR prio_03 IS NOT INITIAL
    OR prio_04 IS NOT INITIAL
    OR prio_06 IS NOT INITIAL.
    DATA priority TYPE string.
    IF prio_01 IS NOT INITIAL.
      IF priority IS NOT INITIAL. priority = priority && `,`. ENDIF.
      priority = priority && `"Priority___EQ___01"`.
    ENDIF.
    IF prio_02 IS NOT INITIAL.
      IF priority IS NOT INITIAL. priority = priority && `,`. ENDIF.
      priority = priority && `"Priority___EQ___02"`.
    ENDIF.
    IF prio_03 IS NOT INITIAL.
      IF priority IS NOT INITIAL. priority = priority && `,`. ENDIF.
      priority = priority && `"Priority___EQ___03"`.
    ENDIF.
    IF prio_04 IS NOT INITIAL.
      IF priority IS NOT INITIAL. priority = priority && `,`. ENDIF.
      priority = priority && `"Priority___EQ___04"`.
    ENDIF.
    IF prio_06 IS NOT INITIAL.
      IF priority IS NOT INITIAL. priority = priority && `,`. ENDIF.
      priority = priority && `"Priority___EQ___06"`.
    ENDIF.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"priority":[` && priority && `]`.
  ENDIF.

  " Filter for category: "category":["Category___EQ___O","Category___EQ___T", ...]
  IF   catlg is not initial
    or faq   is not initial
    or perf  is not initial.
    data category type string.
    if catlg is not initial.
      if category is not initial. category = category &&  `,`. ENDIF.
      category = category && `"Category___EQ___O","Category___EQ___T","Category___EQ___H"`. " O Announcement of Legal Change, T Correction of legal function, H Legal Change
    endif.
    if faq is not initial.
      if category is not initial. category = category &&  `,`. ENDIF.
      category = category && `"Category___EQ___B","Category___EQ___K","Category___EQ___01"`. " B Consulting, K FAQ, 01 How To
    endif.
    if perf is not initial.
      if category is not initial. category = category &&  `,`. ENDIF.
      category = category && `"Category___EQ___P"`. " P Performance
    endif.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"category":[` && category && `]`.
  endif.

  " Filter for document type: "documentType":["FilterDocumentType___EQ___SAPNOTES","FilterDocumentType___EQ___KBA","FilterDocumentType___EQ___SECU","FilterDocumentType___EQ___HOTNEWS","FilterDocumentType___EQ___LEGAL_CHANGE"]
  IF   sapnotes IS NOT INITIAL
    OR kba      IS NOT INITIAL
    OR secu     IS NOT INITIAL
    OR hotnews  IS NOT INITIAL
    OR legal    IS NOT INITIAL.
    DATA doctype TYPE string.
    IF sapnotes IS NOT INITIAL.
      IF doctype IS NOT INITIAL. doctype = doctype && `,`. ENDIF.
      doctype = doctype && `"FilterDocumentType___EQ___SAPNOTES"`.
    ENDIF.
    IF kba IS NOT INITIAL.
      IF doctype IS NOT INITIAL. doctype = doctype && `,`. ENDIF.
      doctype = doctype && `"FilterDocumentType___EQ___KBA"`.
    ENDIF.
    IF secu IS NOT INITIAL.
      IF doctype IS NOT INITIAL. doctype = doctype && `,`. ENDIF.
      doctype = doctype && `"FilterDocumentType___EQ___SECU"`.
    ENDIF.
    IF hotnews IS NOT INITIAL.
      IF doctype IS NOT INITIAL. doctype = doctype && `,`. ENDIF.
      doctype = doctype && `"FilterDocumentType___EQ___HOTNEWS"`.
    ENDIF.
    IF legal IS NOT INITIAL.
      IF doctype IS NOT INITIAL. doctype = doctype && `,`. ENDIF.
      doctype = doctype && `"FilterDocumentType___EQ___LEGAL_CHANGE"`.
    ENDIF.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"documentType":[` && doctype && `]`.
  ENDIF.

  " Filter for SAP Security Patch Day: "SAPSecurityPatchDay":"PATCHDAY" or "SPIN"
  IF secu IS NOT INITIAL.
    IF     sec_no IS NOT INITIAL.
      " no filter
    ELSEIF sec_pday IS NOT INITIAL.
      IF url IS NOT INITIAL. url = url && `,`. ENDIF.
      url = url && `"SAPSecurityPatchDay":"PATCHDAY"`.
    ELSEIF sec_spin IS NOT INITIAL.
      IF url IS NOT INITIAL. url = url && `,`. ENDIF.
      url = url && `"SAPSecurityPatchDay":"SPIN"`.
    ENDIF.
  ENDIF.

  " +releaseStatus:eq~'NotRestricted'             NotRestricted CustomerRelease PilotNotes
  " +fuzzyThreshold:eq~'0.9'

  " Parameter for sort order: "orderBy":"ReleasedOn" or "Number"
  IF     sort_rel IS NOT INITIAL.
    " no filter
  ELSEIF sort_on IS NOT INITIAL.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"orderBy":"ReleasedOn"`.
  ELSEIF sort_num IS NOT INITIAL.
    IF url IS NOT INITIAL. url = url && `,`. ENDIF.
    url = url && `"orderBy":"Number"`.
  ENDIF.

  CHECK url IS NOT INITIAL.

  url = `{` && url && `}`.
  url = cl_http_utility=>escape_url( url ). " escape url:  { %7b   " %22   : %3a   [ %5b   , %2c   ] %5d   } %7d
  url = `https://me.sap.com/mynotes/expertsearch/` && url.

  " Show URL
  "WRITE / url.
  " Show URL in textedit control
  CALL FUNCTION 'CRM_SURVEY_EDITOR_LONGTEXT'
    EXPORTING
*     MAX_LENGTH     = 0
      read_only      = ' '
    CHANGING
      longtext       = url
    EXCEPTIONS
      user_cancelled = 1
      OTHERS         = 2.
  CHECK sy-subrc = 0.

  " Call URL
  DATA url_c TYPE c LENGTH 4069.
  url_c = url.
  CALL FUNCTION 'CALL_BROWSER'
    EXPORTING
      url                    = url_c
    EXCEPTIONS
      frontend_not_supported = 1
      frontend_error         = 2
      prog_not_found         = 3
      no_batch               = 4
      unspecified_error      = 5
      OTHERS                 = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid  TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.