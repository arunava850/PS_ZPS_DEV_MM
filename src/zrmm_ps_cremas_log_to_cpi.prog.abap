*&---------------------------------------------------------------------*
*& Report ZRMM_PS_CREMAS_LOG_TO_CPI
*&---------------------------------------------------------------------*
*& Karthik, 01/07/2023
*&---------------------------------------------------------------------*
REPORT zrmm_ps_cremas_log_to_cpi.

DATA: lv_brsch TYPE lfa1-brsch.
PARAMETERS: p_date  TYPE sy-datum OBLIGATORY,
            cb_prxy AS CHECKBOX,
            cb_list AS CHECKBOX.
SELECT-OPTIONS: s_sys FOR lv_brsch NO INTERVALS.

DATA: lt_final      TYPE TABLE OF zst_cremas_idoc_log,
      ls_final      TYPE zst_cremas_idoc_log,
      lt_edids      TYPE TABLE OF edids,
      lt_edidd      TYPE TABLE OF edidd,
      ls_e1lfa1m    TYPE e1lfa1m,
      ls_e1lfb1m    TYPE e1lfb1m,
      lo_alv        TYPE REF TO cl_salv_table,
      ls_out_proxy  TYPE zmt_bp_send_proxy,
      lt_proxy_data TYPE zdt_bp_send_proxy_tab.

*--------------------------------------------------------------------*
*& START-OF-SELECTION.
*--------------------------------------------------------------------*
START-OF-SELECTION.

  SELECT docnum,status,doctyp,direct,credat,cretim FROM edidc "#EC CI_NOFIELD
    INTO TABLE @DATA(lt_idoc_data)
    WHERE credat = @p_date
      AND mestyp = 'CREMAS'
      AND direct = '2'.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'No data found..' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.


  LOOP AT lt_idoc_data INTO DATA(ls_idoc_data).

    CLEAR: ls_final.

    ls_final-idoc_num = ls_idoc_data-docnum.

    CLEAR:lt_edids,lt_edidd.

    CALL FUNCTION 'IDOC_READ_COMPLETELY'
      EXPORTING
        document_number         = ls_idoc_data-docnum
* IMPORTING
*       IDOC_CONTROL            =
*       NUMBER_OF_DATA_RECORDS  =
*       NUMBER_OF_STATUS_RECORDS       =
      TABLES
        int_edids               = lt_edids
        int_edidd               = lt_edidd
      EXCEPTIONS
        document_not_exist      = 1
        document_number_invalid = 2
        OTHERS                  = 3.
    IF sy-subrc IS INITIAL.
      SORT lt_edids BY credat DESCENDING cretim DESCENDING.
      READ TABLE lt_edids INTO DATA(ls_edids)
                           WITH KEY status = ls_idoc_data-status.
      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_edids.
      ENDIF.
      READ TABLE lt_edidd INTO DATA(ls_edidd)
                           WITH KEY segnam = 'E1LFA1M'.
      IF sy-subrc IS INITIAL.
        ls_e1lfa1m = ls_edidd-sdata.
        ls_final-lifnr = ls_e1lfa1m-lifnr.
        ls_final-brsch = ls_e1lfa1m-brsch.
      ENDIF.
      READ TABLE lt_edidd INTO ls_edidd
                           WITH KEY segnam = 'E1LFB1M'.
      IF sy-subrc IS INITIAL.
        ls_e1lfb1m = ls_edidd-sdata.
        ls_final-altkn = ls_e1lfb1m-altkn.
      ENDIF.
    ENDIF.
    CASE ls_idoc_data-status.
      WHEN '53'.
        ls_final-status_code = ls_idoc_data-status.
        ls_final-status = 'SUCCESS'.
        ls_final-status_txt = 'Idoc is posted successfully'.
      WHEN OTHERS.
        ls_final-status_code = ls_idoc_data-status.
        ls_final-status = 'FAILED'.
        IF ls_edids-stamid IS NOT INITIAL.
          MESSAGE ID ls_edids-stamid TYPE 'I' NUMBER ls_edids-stamno
                 INTO ls_final-status_txt
                 WITH ls_edids-stapa1  ls_edids-stapa2  ls_edids-stapa3  ls_edids-stapa4.
        ELSE.
          ls_final-status_txt = 'Idoc is not posted successfully'.
        ENDIF.
*        MESSAGE LS_EDIDS-STAMID(ls_edids-stamno) .
    ENDCASE.

    IF ls_e1lfa1m-brsch = 'WC'.
      ls_final-interface_name = 'INT0001'.
    ELSEIF ls_e1lfa1m-brsch NE 'WC'.
      ls_final-interface_name = 'INT0004'.
    ENDIF.
    ls_final-receiver_system = ls_e1lfa1m-brsch.

    IF s_sys[] IS NOT INITIAL
      AND ls_e1lfa1m-brsch NOT IN s_sys[].
      CONTINUE.
    ENDIF.

    IF cb_prxy IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_proxy_data ASSIGNING FIELD-SYMBOL(<lfs_proxy_data>).
      <lfs_proxy_data> = CORRESPONDING #( ls_final ).
    ENDIF.

    APPEND ls_final TO lt_final.
    CLEAR ls_final.

    CLEAR: ls_edids,ls_edidd.

  ENDLOOP.

  SORT lt_proxy_data BY lifnr brsch.
  DELETE ADJACENT DUPLICATES FROM lt_proxy_data COMPARING lifnr brsch.
  SORT lt_final BY lifnr brsch.
  DELETE ADJACENT DUPLICATES FROM lt_final COMPARING lifnr brsch.

*--------------------------------------------------------------------*
*& END-OF-SELECTION.
*--------------------------------------------------------------------*
END-OF-SELECTION.

  IF lt_proxy_data[] IS NOT INITIAL AND cb_prxy IS NOT INITIAL.
    TRY.
        DATA(lo_obj_proxy) = NEW zco_zps_bp_send_proxy( ).
        IF lo_obj_proxy IS BOUND.
          ls_out_proxy-mt_bp_send_proxy-it_data[] = lt_proxy_data[].
          lo_obj_proxy->send_bp( output = ls_out_proxy ).
          COMMIT WORK AND WAIT.
          MESSAGE 'The data send to CPI is initiated.' TYPE 'I'.
        ENDIF.
      CATCH cx_ai_system_fault INTO DATA(lo_fault).

        DATA(lv_error) = lo_fault->errortext.
        IF lv_error IS NOT INITIAL.
          MESSAGE lv_error TYPE 'I'.
        ENDIF.
        MESSAGE 'Exception occurred while transferring proxy data.' TYPE 'I'.

    ENDTRY.
  ENDIF.

  IF lt_final[] IS NOT INITIAL AND cb_list IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table   = lt_final ).
        IF lo_alv IS BOUND.
          DATA(lo_func) = lo_alv->get_functions( ).
          lo_func->set_all( abap_true ).
          DATA(lo_columns) = lo_alv->get_columns( ).
*  gr_column ?= gr_columns->get_column( text-102 ).
*  gr_column->set_visible( abap_false ).
          lo_columns->set_optimize( abap_true ).
          lo_alv->display( ).
        ENDIF.
      CATCH cx_salv_msg.
        MESSAGE 'ALV display error.' TYPE 'I'.
    ENDTRY.
  ENDIF.
