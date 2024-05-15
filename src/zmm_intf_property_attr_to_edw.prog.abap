*&---------------------------------------------------------------------*
*& Report ZMM_INTF_PROPERTY_ATTR_TO_EDW
*&---------------------------------------------------------------------*
*& KDURAI, 11/12/2023
*&---------------------------------------------------------------------*
REPORT zmm_intf_property_attr_to_edw.

DATA: gv_file         TYPE string,
      gt_fcat         TYPE lvc_t_fcat,
      gs_data         TYPE string,
      gt_data         TYPE string_table,
      gv_data         TYPE string,
      gv_char500(500) TYPE c,
      gt_ccpc         TYPE TABLE OF zfi_pccenters,
      gv_ccpc         TYPE char10.

FIELD-SYMBOLS: <lfs_field> TYPE any.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : p_srv TYPE string LOWER CASE DEFAULT '/interfaces/edw/INT0049/out/'.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZPROPERTY'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  DELETE gt_fcat WHERE fieldname EQ 'MANDT'.

  SELECT * FROM zproperty                     "#EC CI_ALL_FIELDS_NEEDED
    INTO TABLE @DATA(lt_zproperty).                     "#EC CI_NOWHERE

  DATA(lv_file_name) = 'INT0049_' && sy-datum && sy-uzeit && '.TXT'.

  gv_file = p_srv && lv_file_name.

  OPEN DATASET gv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  LOOP AT gt_fcat INTO DATA(ls_fcat).
    DATA(lv_tabix) = sy-tabix.
    CONCATENATE '"' ls_fcat-fieldname '"' INTO gv_data.
    IF lv_tabix = 1.
      gs_data = gv_data.
    ELSE.
      CONCATENATE gs_data gv_data INTO gs_data SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    ENDIF.
  ENDLOOP.
  TRANSFER gs_data TO gv_file.
  APPEND gs_data TO gt_data.

  LOOP AT lt_zproperty INTO DATA(ls_zproperty).
    CLEAR: gs_data,gv_data.
    LOOP AT gt_fcat INTO ls_fcat.
      lv_tabix = sy-tabix.
      UNASSIGN <lfs_field>.
      ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE ls_zproperty TO <lfs_field>.
      IF sy-subrc IS INITIAL.
        gv_data = CONV #( <lfs_field> ).
*>> Profit Center Legacy format
        IF ls_fcat-fieldname = 'STORAGE_CBT_PC'.
          gv_ccpc = gv_data.
          CLEAR:gt_ccpc[].
          CALL FUNCTION 'ZCA_OUTPUT_PC'
            EXPORTING
              iv_pcenters = gv_ccpc
              iv_pc_flag  = 'P'
            TABLES
              et_output   = gt_ccpc.
          READ TABLE gt_ccpc INTO DATA(ls_ccpc) INDEX 1.
          IF sy-subrc IS INITIAL.
            gv_data = ls_ccpc-numbr.
            CLEAR:ls_ccpc.
          ELSE.
            gv_data = gv_ccpc.
          ENDIF.
        ENDIF.
        gv_char500 = gv_data.
        gv_data = gv_char500.
        CONCATENATE '"' gv_data '"' INTO gv_data.
        IF lv_tabix = 1.
          gs_data = gv_data.
        ELSE.
          CONCATENATE gs_data gv_data INTO gs_data SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND gs_data TO gt_data.
    TRANSFER gs_data TO gv_file.
  ENDLOOP.
  CLOSE DATASET gv_file.
  IF sy-subrc IS INITIAL.
    MESSAGE 'File created successfully.' TYPE 'I'.
    COMMIT WORK AND WAIT.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF 1 = 2.
    DATA: lt_data TYPE TABLE OF string,
          lv_file TYPE string VALUE '/interfaces/edw/INT0049/archive/'.
    OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc IS INITIAL.
      DO.
        READ DATASET lv_file INTO gs_data.
        IF sy-subrc IS NOT INITIAL.
          EXIT.
        ELSE.
          APPEND gs_data TO lt_data.
        ENDIF.
      ENDDO.
    ENDIF.
    CLOSE DATASET lv_file.
    IF sy-subrc IS INITIAL.
      MESSAGE 'File read successfully.' TYPE 'I'.
*    COMMIT WORK AND WAIT.
*   LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
