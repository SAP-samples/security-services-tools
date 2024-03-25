*&---------------------------------------------------------------------*
*& Report  ZDIAGCV_TSCUS_HDR
*& Author: Frank Buchholz, SAP Security Services
*&---------------------------------------------------------------------*
*& ConfigVal: Maintain Descriptions of Target Systems
*& 02.09.2022 Updated
*& 25.03.2024 Selection by description added
*&---------------------------------------------------------------------*
REPORT zdiagcv_tscus_hdr.

CONSTANTS: c_program_version(30) TYPE c VALUE '25.03.2024 FBT'.

TABLES diagcv_tscus_hdr.
types: icon.

DATA:
  BEGIN OF ls_tsys,
    ref_id      TYPE diagcv_tscus_hdr-ref_id,
    description TYPE diagcv_tscus_hdr-description,
  END OF ls_tsys,
  lt_tsys LIKE TABLE OF ls_tsys.

DATA: line_num TYPE i.

* Target systems
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_ref FOR FIELD ref_id.
SELECT-OPTIONS: ref_id FOR diagcv_tscus_hdr-ref_id MATCHCODE OBJECT h_target_system.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) ss_desc FOR FIELD desc.
SELECT-OPTIONS: desc FOR diagcv_tscus_hdr-DESCRIPTION LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(60) ss_vers.

INITIALIZATION.

  ss_ref = 'Target System'(001).
  ss_desc = 'Description'(006).

  CONCATENATE 'Program version:'(VER) c_program_version INTO ss_vers
    SEPARATED BY space.

START-OF-SELECTION.

* check authorization in general
  AUTHORITY-CHECK OBJECT 'SM_CV_TASM'
    ID 'ACTVT' FIELD '03'
    ID 'CV_TARSYS' DUMMY
    ID 'CV_TSUSER' DUMMY.
  IF sy-subrc NE 0.
    MESSAGE e019(diag_gen_msg)
      WITH 'No authorization to display Target Systems'
           '(authorization object SM_CV_TASM)'.
    RETURN.
  ENDIF.

  FORMAT RESET.
  write: / ICON_SYSTEM_SAVE as icon hotspot, 'Save'(002).
*  write:   '| or use F2 on a line to save a single entry'(003).
  uline.

  SELECT DISTINCT ref_id, description FROM diagcv_tscus_hdr
    INTO TABLE @lt_tsys
    WHERE ref_id IN @ref_id
      AND description IN @desc
    ORDER BY ref_id.

  LOOP AT  lt_tsys INTO ls_tsys.
    WRITE: / ls_tsys-ref_id.

*   check authorization
    AUTHORITY-CHECK OBJECT 'SM_CV_TASM'
      ID 'ACTVT' FIELD '02'
      ID 'CV_TARSYS' FIELD ls_tsys-ref_id
      ID 'CV_TSUSER' DUMMY.
    IF sy-subrc = 0.
      WRITE ls_tsys-description INPUT HOTSPOT.  " change
    ELSE.
      WRITE ls_tsys-description.                " read
    ENDIF.

  ENDLOOP.

AT LINE-SELECTION.

* Update all
  if sy-lisel(3) = '2L '. "click on ICON_SYSTEM_SAVE
    DO.
      CLEAR ls_tsys.
      READ LINE sy-index FIELD VALUE ls_tsys-ref_id ls_tsys-description.
      IF sy-subrc <> 0.
        EXIT.
      endif.
      IF ls_tsys-ref_id IS NOT INITIAL AND ls_tsys-description IS NOT INITIAL.
        UPDATE diagcv_tscus_hdr
          SET description = ls_tsys-description
          WHERE ref_id = ls_tsys-ref_id.
      ENDIF.
    ENDDO.
    MESSAGE s398(00) WITH 'All lines saved'(003).
    exit.
  endif.

* Update an entry
  CLEAR ls_tsys.
  READ LINE sy-lilli FIELD VALUE ls_tsys-ref_id ls_tsys-description.
  IF ls_tsys-ref_id IS NOT INITIAL AND ls_tsys-description IS NOT INITIAL.
    UPDATE diagcv_tscus_hdr
      SET description = ls_tsys-description
      WHERE ref_id = ls_tsys-ref_id.
    MESSAGE s398(00) WITH 'Saved'(004) ls_tsys-ref_id ls_tsys-description.
    exit.
  ENDIF.
  MESSAGE w398(00) WITH 'Nothing saved'(005).