*&---------------------------------------------------------------------*
*& Report zmm_property_addon_upload
*&---------------------------------------------------------------------*
*& KDURAI, 11/12/2023
*&---------------------------------------------------------------------*
REPORT zmm_property_addon_upload.

TYPES: BEGIN OF ty_output,
         plant      TYPE zproperty_add_on-plant,
         property   TYPE zproperty_add_on-property,
         key_id     TYPE zproperty_add_on-key_id,
         code       TYPE zproperty_add_on-code,
         status     TYPE char10,
         error_text TYPE char100,
       END OF ty_output.

TYPES : BEGIN OF ty_property_add_on,
          plant        TYPE zproperty_add_on-plant,
          property     TYPE zproperty_add_on-property,
          key_id       TYPE zproperty_add_on-key_id,
          code         TYPE zproperty_add_on-code,
          value1       TYPE zproperty_add_on-value1,
          value2       TYPE zproperty_add_on-value2,
          description1 TYPE zproperty_add_on-description1,
          description2 TYPE zproperty_add_on-description2,
        END OF ty_property_add_on.

DATA : gt_property_add_on  TYPE TABLE OF ty_property_add_on,
       gw_property_add_on  TYPE ty_property_add_on,
       gt_output           TYPE TABLE OF ty_output,
       gt_zproperty_add_on TYPE TABLE OF zproperty_add_on,
       gw_zproperty_add_on TYPE zproperty_add_on.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS :p_cre RADIOBUTTON GROUP rb1,
              p_del RADIOBUTTON GROUP rb1.
  PARAMETERS : p_file TYPE string LOWER CASE,
               p_test AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM file_selection_f4.


START-OF-SELECTION.

  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) "#EC CI_ALL_FIELDS_NEEDED
     WHERE name EQ 'ZPS_ENH0014_ADD_ON_KEYID'
    ORDER BY PRIMARY KEY.
  IF sy-subrc IS INITIAL.
    SORT lt_tvarvc BY low.
  ENDIF.

  DATA lv_path TYPE string.
  lv_path = p_file.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_path
      has_field_separator     = 'X'
    CHANGING
      data_tab                = gt_property_add_on
*     isscanperformed         = SPACE
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
   MESSAGE 'Error: Unable to read the file' TYPE 'I' DISPLAY LIKE 'E'.
   LEAVE LIST-PROCESSING.
  ENDIF.

  DELETE gt_property_add_on INDEX 1.
  DATA lv_count(5) TYPE n.

  DATA(lt_property_add_on_tmp) = gt_property_add_on.
  SORT lt_property_add_on_tmp BY plant.
  DELETE ADJACENT DUPLICATES FROM lt_property_add_on_tmp
   COMPARING plant.
  IF lt_property_add_on_tmp[] IS NOT INITIAL.
    SELECT werks,name2 FROM t001w INTO TABLE @DATA(lt_t001w)
      FOR ALL ENTRIES IN @lt_property_add_on_tmp
      WHERE werks = @lt_property_add_on_tmp-plant.
    IF sy-subrc IS INITIAL.
      SORT lt_t001w BY werks.
    ENDIF.
  ENDIF.

  LOOP AT gt_property_add_on INTO gw_property_add_on.

    MOVE-CORRESPONDING gw_property_add_on TO gw_zproperty_add_on.
    APPEND INITIAL LINE TO gt_output ASSIGNING FIELD-SYMBOL(<lfs_output>).
    <lfs_output>-plant  = gw_zproperty_add_on-plant.
    <lfs_output>-key_id = gw_zproperty_add_on-key_id.
    <lfs_output>-code = gw_zproperty_add_on-code.
    READ TABLE lt_t001w INTO DATA(ls_t001w)
                        WITH KEY werks = gw_zproperty_add_on-plant
                        BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <lfs_output>-property  = gw_zproperty_add_on-property = ls_t001w-name2.
    ELSE.
      <lfs_output>-status = 'ERROR'.
      <lfs_output>-error_text = 'Plant is not exists in T001W'.
      CONTINUE.
    ENDIF.

    READ TABLE lt_tvarvc INTO DATA(ls_tvarvc)
             WITH KEY low = gw_zproperty_add_on-key_id.
    IF sy-subrc IS NOT INITIAL.
      <lfs_output>-status = 'ERROR'.
      <lfs_output>-error_text = 'Keyfield is not valid'.
      CONTINUE.
    ENDIF.

    IF line_exists( gt_zproperty_add_on[  plant    = gw_zproperty_add_on-plant
                                          property = gw_zproperty_add_on-property
                                          key_id   = gw_zproperty_add_on-key_id
                                          code     = gw_zproperty_add_on-code ] ).
      <lfs_output>-status = 'ERROR'.
      <lfs_output>-error_text = 'Duplicate key entries are existing.'.
      CONTINUE.
    ENDIF.

    <lfs_output>-status = 'SUCCESS'.


    gw_zproperty_add_on-changed_by = sy-uname.
    gw_zproperty_add_on-changed_date = sy-datum.
    gw_zproperty_add_on-changed_time = sy-uzeit.
    APPEND gw_zproperty_add_on TO gt_zproperty_add_on.
    CLEAR gw_zproperty_add_on.
  ENDLOOP.

  IF p_test IS INITIAL.
    IF p_cre IS NOT INITIAL.

      MODIFY zproperty_add_on FROM TABLE gt_zproperty_add_on.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
        MESSAGE 'Data has been processed' TYPE 'S'.
      ENDIF.

    ELSE.

      DATA(lt_zproperty_add_on) = gt_zproperty_add_on.
      CLEAR gt_zproperty_add_on.

      IF lt_zproperty_add_on IS NOT INITIAL.
        SELECT * FROM zproperty_add_on INTO TABLE gt_zproperty_add_on
          FOR ALL ENTRIES IN lt_zproperty_add_on
          WHERE plant = lt_zproperty_add_on-plant
            AND property = lt_zproperty_add_on-property
            AND key_id = lt_zproperty_add_on-key_id
            AND code = lt_zproperty_add_on-code.
        IF sy-subrc IS INITIAL.
          DELETE zproperty_add_on FROM TABLE gt_zproperty_add_on.
          IF sy-subrc EQ 0.
            COMMIT WORK AND WAIT.
            MESSAGE 'Data has been deleted' TYPE 'S'.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

  IF gt_output[] IS NOT INITIAL .
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING  t_table   = gt_output ).
        IF lo_alv IS BOUND.

          DATA(lo_func) = lo_alv->get_functions( ).
          lo_func->set_all( abap_true ).
          DATA(lo_columns) = lo_alv->get_columns( ).
          lo_columns->set_optimize( abap_true ).
          DATA(lt_cols)    = lo_columns->get( ).

          DATA: gv_short  TYPE scrtext_s,
                gv_med    TYPE scrtext_m,
                gv_long   TYPE scrtext_l,
                lo_column TYPE REF TO  cl_salv_column_list.

          LOOP AT lt_cols INTO DATA(ls_cols).
            lo_column ?= ls_cols-r_column.    "Narrow casting

            gv_short = ls_cols-columnname.
            lo_column->set_short_text( gv_short ).

            gv_med = ls_cols-columnname.
            lo_column->set_medium_text( gv_med ).

            gv_long = ls_cols-columnname.
            lo_column->set_long_text( gv_long ).
          ENDLOOP.

          lo_alv->display( ).

        ENDIF.
      CATCH cx_salv_msg.
        MESSAGE 'ALV display error.' TYPE 'I'.
    ENDTRY.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form file_selection_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM file_selection_f4 .
  DATA: gt_filetab  TYPE filetable,
        gs_filetab  TYPE file_table,
        g_fo_title  TYPE string,
        g_fo_rc     TYPE i,
        g_fo_action TYPE i.

  g_fo_title = TEXT-fo1.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = g_fo_title
*     file_filter             = '(*.csv*)' "#EC NOTEXT
    CHANGING
      file_table              = gt_filetab
      rc                      = g_fo_rc
      user_action             = g_fo_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0 AND
     g_fo_action = cl_gui_frontend_services=>action_ok.
*   get file name
    READ TABLE gt_filetab INDEX 1 INTO gs_filetab.
    IF sy-subrc = 0.
      p_file  = gs_filetab-filename.
    ENDIF.
  ENDIF.
ENDFORM.
