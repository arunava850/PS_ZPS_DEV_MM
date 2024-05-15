*&---------------------------------------------------------------------*
*& Include          ZXVSVU03
*&---------------------------------------------------------------------*
DATA: ls_e1lfa1m TYPE e1lfa1m.

IF message_type EQ 'CREMAS'.
  DATA(lv_lines) = lines( idoc_data[] ).
  READ TABLE idoc_data ASSIGNING FIELD-SYMBOL(<lfs_idoc>) INDEX lv_lines.
  IF sy-subrc IS INITIAL.
    CASE <lfs_idoc>-segnam.
      WHEN  'E1LFA1M'.
        ls_e1lfa1m = <lfs_idoc>-sdata.
        IF ls_e1lfa1m-lifnr IS NOT INITIAL.
          SELECT altkn FROM lfb1 INTO @ls_e1lfa1m-emnfr UP TO 1 ROWS
            WHERE lifnr EQ @ls_e1lfa1m-lifnr.
          ENDSELECT.
          IF sy-subrc IS INITIAL.
            <lfs_idoc>-sdata = ls_e1lfa1m.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDIF.
ENDIF.
