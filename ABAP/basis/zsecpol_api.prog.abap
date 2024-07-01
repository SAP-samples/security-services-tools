*&---------------------------------------------------------------------*
*& Report ZSECPOL_API
*&---------------------------------------------------------------------*
*& Example for using the API to manage security policies (SECPOL)
*& Shown API methods:
*&   Class CL_SECURITY_POLICY
*&     list_all_available, exists, get_info, get_attribute_value,
*&     lock_all, create, modify, delete, unlock_all
*&   Class CL_SECURITY_POLICY_ATTRIBUTE
*&     list_all_available
*& There exist some more API methods in these classes.
*&---------------------------------------------------------------------*
REPORT zdefine_secpol
 LINE-SIZE 255.

CONSTANTS: c_program_version(30) TYPE c VALUE '01.07.2024 S44'.

PARAMETERS policy TYPE security_policy_name.

PARAMETERS list   RADIOBUTTON GROUP cmd.
PARAMETERS show   RADIOBUTTON GROUP cmd.
PARAMETERS create RADIOBUTTON GROUP cmd.
PARAMETERS modify RADIOBUTTON GROUP cmd.
PARAMETERS delete RADIOBUTTON GROUP cmd.

START-OF-SELECTION.
  FORMAT RESET.

  DATA:
    "policy                     TYPE security_policy_name,
    policy_list                TYPE security_policy_list,
    policy_attribute_info_list TYPE sec_policy_attribute_info_list,
    policy_exists              TYPE abap_bool,
    policy_info                TYPE security_policy_info,
    description	               TYPE security_policy_text,
    attribute_list             TYPE security_policy_attribute_list,
    only_persisted_content     TYPE abap_bool,
    discard_default_values     TYPE abap_bool,
    invalid_attribute_values   TYPE abap_bool,
    messages                   TYPE bapirettab,
    ex                         TYPE REF TO cx_security_policy.

  " API: Get list of policies and list of attributes
  IF list = 'X'.
    TRY.
        CALL METHOD cl_security_policy=>list_all_available
        "EXPORTING
        "language = SPACE
          RECEIVING
            result = policy_list.
      CATCH cx_security_policy.
    ENDTRY.
    LOOP AT policy_list INTO POLICY_info.
      WRITE: / policy_info-name,
               policy_info-created_by,
               policy_info-created_on USING EDIT MASK '____-__-__ __:__:__',
               policy_info-changed_by,
               policy_info-changed_on USING EDIT MASK '____-__-__ __:__:__',
               policy_info-description.
    ENDLOOP.

    SKIP.

    TRY.
        CALL METHOD cl_security_policy_attribute=>list_all_available
        "EXPORTING
        "language = SPACE
          RECEIVING
            result = policy_attribute_info_list.
      CATCH cx_security_policy.
    ENDTRY.
    LOOP AT policy_attribute_info_list INTO DATA(policy_attribute_info).
      WRITE: / policy_attribute_info-name,
               "policy_attribute_info-type,
               "policy_attribute_info-ddic_dataelement,
               policy_attribute_info-default_value,
               policy_attribute_info-description.
    ENDLOOP.

    RETURN.
  ENDIF.

  CHECK policy IS NOT INITIAL.

  " API: Check existance
  CALL METHOD cl_security_policy=>exists
    EXPORTING
      name   = policy
      "client =
    RECEIVING
      result = policy_exists.
  IF policy_exists = abap_false.
    MESSAGE s001(security_policy) WITH policy." Security policy &1 does not exist
  ENDIF.

  " API: Get description
  TRY.
      CALL METHOD cl_security_policy=>get_info
        EXPORTING
          name   = policy
          "language = SPACE
        RECEIVING
          result = policy_info.
    CATCH cx_security_policy INTO ex.
      "MESSAGE s001(security_policy) WITH policy." Security policy &1 does not exist
      MESSAGE s000(security_policy) WITH ex->get_text( ).
  ENDTRY.

  IF policy_exists = abap_true.

    " Show description
    FORMAT COLOR COL_HEADING.
    WRITE: / policy_info-name,
             policy_info-created_by,
             policy_info-created_on USING EDIT MASK '____-__-__ __:__:__',
             policy_info-changed_by,
             policy_info-changed_on USING EDIT MASK '____-__-__ __:__:__',
             policy_info-description.
    FORMAT RESET.

    " API: Get attributes
    TRY.
        CALL METHOD cl_security_policy=>get_attribute_value_list
          EXPORTING
            policy                 = policy
            "client                 =
            only_persisted_content = only_persisted_content
          RECEIVING
            attribute_list         = attribute_list.
      CATCH cx_security_policy INTO ex.
        MESSAGE w000(security_policy) WITH ex->get_text( ).
    ENDTRY.

    " Show attributes
    LOOP AT attribute_list INTO DATA(attribute).
      WRITE: / attribute-name COLOR COL_KEY,
               attribute-value.
    ENDLOOP.

  ENDIF.

  IF create = 'X' OR modify = 'X' OR delete = 'X'.

    " API: Lock policies for change operation
    TRY.
        CALL METHOD cl_security_policy=>lock_all.
      CATCH cx_security_policy INTO ex.
        MESSAGE e000(security_policy) WITH ex->get_text( ).
    ENDTRY.

  ENDIF.

  IF policy_exists = abap_false AND create = 'X'.

    " API: Create policy
    description = |Security policy { policy }|.
    attribute_list = VALUE #(
      ( name = 'MIN_PASSWORD_DIGITS' value = '2'  )
      ( name = 'MIN_PASSWORD_LENGTH' value = '12' )
    ).
    TRY.
        CALL METHOD cl_security_policy=>create
          EXPORTING
            policy                   = policy
            attribute_list           = attribute_list
            discard_default_values   = abap_false
            "language                 = SPACE
            description              = description
          IMPORTING
            messages                 = messages
            invalid_attribute_values = invalid_attribute_values.
      CATCH cx_security_policy INTO ex.
        MESSAGE e000(security_policy) WITH ex->get_text( ).
    ENDTRY.
    LOOP AT messages INTO DATA(message).
      WRITE: / message-id,
               message-number,
               message-message.
    ENDLOOP.
    MESSAGE s000(security_policy) WITH 'Policy created'.

  ENDIF.

  IF policy_exists = abap_true AND modify = 'X'.

    " API: Modify policy
    TRY.
        CALL METHOD cl_security_policy=>modify
          EXPORTING
            policy                   = policy
            modified_attributes      = attribute_list
            discard_default_values   = discard_default_values
          IMPORTING
            messages                 = messages
            invalid_attribute_values = invalid_attribute_values.
      CATCH cx_security_policy INTO ex.
        MESSAGE e000(security_policy) WITH ex->get_text( ).
    ENDTRY.
    MESSAGE s000(security_policy) WITH 'Policy updated'.

  ENDIF.

  IF policy_exists = abap_true AND delete = 'X'.

    " API: Delete policy
    TRY.
        CALL METHOD cl_security_policy=>delete
          EXPORTING
            policy = policy.
      CATCH cx_security_policy INTO ex.
        MESSAGE e000(security_policy) WITH ex->get_text( ).
    ENDTRY.
    MESSAGE s000(security_policy) WITH 'Policy deleted'.

  ENDIF.

  IF create = 'X' OR modify = 'X' OR delete = 'X'.

    " API: Unlock all after
    TRY.
        CALL METHOD cl_security_policy=>unlock_all.
      CATCH cx_security_policy INTO ex.
        MESSAGE e000(security_policy) WITH ex->get_text( ).
    ENDTRY.

  ENDIF.
