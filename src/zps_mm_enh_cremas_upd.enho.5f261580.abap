"Name: \FU:IDOC_INPUT_CREDITOR\SE:BEGIN\EI
ENHANCEMENT 0 ZPS_MM_ENH_CREMAS_UPD.
DATA: ls_e1lfa1m      TYPE e1lfa1m,
      ls_e1lfa1m_1    TYPE e1lfa1m,
      ls_e1lfb1m      TYPE e1lfb1m,
      ls_e1lfbwm      TYPE e1lfbwm,
      ls_e1lfbkm      TYPE e1lfbkm,
      lt_idoc_data    TYPE TABLE OF edidd,
      ls_docstat      TYPE bdidocstat,
      lt_bukrs        TYPE TABLE OF cepc_bukrs,
      lv_prctr        TYPE prctr,
      lv_error        TYPE c,
      lv_error_dup    TYPE c,
      lv_msg_txt      TYPE char50,
      lr_excl_company TYPE RANGE OF bukrs.
FIELD-SYMBOLS: <fs_field> TYPE any. "or your_structure

SELECT sign,opti,low,high FROM tvarvc INTO TABLE @lr_excl_company
  WHERE name = 'ZMM_CREMAS_EXCLUDE_BUKRS'.
IF sy-subrc IS NOT INITIAL.
  CLEAR lr_excl_company[].
ENDIF.

LOOP AT idoc_contrl INTO DATA(ls_idoc_ctrl) WHERE mestyp = 'CREMAS'.
  READ TABLE idoc_data ASSIGNING FIELD-SYMBOL(<lfs_idoc_data_1>)
                       WITH KEY docnum = ls_idoc_ctrl-docnum
                                segnam = 'E1LFA1M'.
  IF sy-subrc  IS INITIAL.
    DATA(ls_idoc_data) = <lfs_idoc_data_1>.
    ls_e1lfa1m_1 = ls_e1lfa1m = ls_idoc_data-sdata.
*>> BOC
    DO.
      UNASSIGN <fs_field>.
      ASSIGN COMPONENT sy-index OF STRUCTURE ls_e1lfa1m TO <fs_field>.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      IF <fs_field> IS ASSIGNED.
        IF <fs_field> IS INITIAL.
          <fs_field> = '/'.
        ENDIF.
      ENDIF.
    ENDDO.
    IF ls_e1lfa1m-brsch = '/'.
      ls_e1lfa1m-brsch = space.
    ENDIF.
*// RAMP INT0003
    IF ls_e1lfa1m-brsch NE 'WC'.
      IF ls_e1lfa1m-sperr = '/'.
        ls_e1lfa1m-sperr = space.
      ENDIF.
    ENDIF.
    <lfs_idoc_data_1>-sdata =  ls_e1lfa1m.

*<< EOC

*    IF ls_e1lfa1m-brsch = 'WC' OR ls_e1lfa1m-brsch = 'RAMP'.

    CLEAR lv_error_dup.

    IF ls_e1lfa1m-brsch = 'WC'.
*        SELECT lifnr FROM lfa1 INTO @DATA(lv_lifnr) UP TO 1 ROWS
*                                 WHERE (   ( name1 = @ls_e1lfa1m-name1 AND name2 = @ls_e1lfa1m-name2 )
*                                        OR ( stras = @ls_e1lfa1m-stras AND pstlz = @ls_e1lfa1m-pstlz
*                                             AND ort01 = @ls_e1lfa1m-ort01 ) ).
*        ENDSELECT.
*        IF sy-subrc IS INITIAL
*          AND lv_lifnr NE ls_e1lfa1m-lifnr.
*          lv_error_dup = abap_true.
*          lv_msg_txt = ' Name/Address'.
*        ENDIF.
    ENDIF.

    IF ls_e1lfa1m-brsch NE 'WC'.
*      IF ls_e1lfa1m_1-stcd1 IS NOT INITIAL.
*        SELECT lifnr FROM lfa1 INTO @DATA(lv_lifnr) UP TO 1 ROWS
*                                 WHERE  stcd1 = @ls_e1lfa1m_1-stcd1  .
*        ENDSELECT.
*        IF sy-subrc IS INITIAL
*          AND lv_lifnr NE ls_e1lfa1m_1-lifnr.
*          lv_error_dup = abap_true.
*          lv_msg_txt = ' Tax Number'.
*        ENDIF.
*      ENDIF.

*      IF ls_e1lfa1m_1-stcd2 IS NOT INITIAL ."AND lv_error_dup IS INITIAL.
*        SELECT lifnr FROM lfa1 INTO @lv_lifnr UP TO 1 ROWS
*                                 WHERE stcd2 = @ls_e1lfa1m_1-stcd2 .
*        ENDSELECT.
*        IF sy-subrc IS INITIAL
*          AND lv_lifnr NE ls_e1lfa1m_1-lifnr.
*          lv_error_dup = abap_true.
*          lv_msg_txt = ' Tax Number'.
*        ENDIF.
*      ENDIF.
*
*        IF lv_error_dup IS INITIAL.
      SELECT lifnr FROM lfa1 INTO @DATA(lv_lifnr) UP TO 1 ROWS
                               WHERE   ( name1 = @ls_e1lfa1m_1-name1 AND name2 = @ls_e1lfa1m_1-name2 )
                                 and stcd1 = @ls_e1lfa1m_1-stcd1
                                 and stcd2 = @ls_e1lfa1m_1-stcd2 .
*                                          OR ( stras = @ls_e1lfa1m_1-stras AND pstlz = @ls_e1lfa1m_1-pstlz
*                                               AND ort01 = @ls_e1lfa1m_1-ort01 AND ort02 = @ls_e1lfa1m_1-ort02 ) ).
      ENDSELECT.
      IF sy-subrc IS INITIAL
        AND lv_lifnr NE ls_e1lfa1m_1-lifnr ."AND lv_error_dup = abap_true.
        lv_error_dup = abap_true.
        lv_msg_txt = ' Tax ID and Name'.
*      ELSE.
*        CLEAR: lv_error_dup , lv_msg_txt .
      ENDIF.
    ENDIF.
*      ENDIF.

    IF  lv_error_dup IS NOT INITIAL.
      ls_docstat = VALUE #(
           docnum = ls_idoc_ctrl-docnum status = '51' msgty = 'E' msgid = 'WN' msgno = '099'
           msgv1 = |Already vendor { lv_lifnr } |
           msgv2 = ' exists with the same'
           msgv3 = lv_msg_txt "' Name/Address'
           segnum = ls_idoc_data-segnum segfld = 'LIFNR' uname = sy-uname repid = sy-repid ).
      APPEND ls_docstat TO idoc_status[].
      CLEAR: lv_lifnr.
      lv_error = abap_true.
      CONTINUE.
    ENDIF.
*    ENDIF.

*// INT0005
    IF ls_e1lfa1m-brsch = 'WC'.
*      t_idoc_status-status = c_idoc_stat_input_error.

      LOOP AT idoc_data ASSIGNING FIELD-SYMBOL(<lfs_idoc_data>) WHERE docnum = ls_idoc_ctrl-docnum
                                                                  AND segnam EQ 'E1LFB1M'.
        ls_e1lfb1m = <lfs_idoc_data>-sdata.
        lv_prctr = ls_e1lfb1m-qsznr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_prctr
          IMPORTING
            output = lv_prctr.

        READ TABLE lt_bukrs INTO DATA(ls_bukrs) WITH KEY kokrs = 'PSCO'
                                                   prctr = lv_prctr.
        IF sy-subrc IS NOT INITIAL.
          SELECT kokrs,prctr,bukrs FROM cepc_bukrs APPENDING CORRESPONDING FIELDS OF TABLE @lt_bukrs
            WHERE kokrs = 'PSCO'
               AND prctr = @lv_prctr.
          IF sy-subrc IS INITIAL .
            READ TABLE lt_bukrs INTO ls_bukrs WITH KEY kokrs = 'PSCO'
                                                       prctr = lv_prctr.
          ENDIF.
        ENDIF.
        IF ls_bukrs-bukrs IS NOT INITIAL.
          ls_e1lfb1m-bukrs = ls_bukrs-bukrs.
        ELSE.
          ls_e1lfb1m-bukrs = ls_e1lfb1m-qsznr.
        ENDIF.

        CLEAR ls_e1lfb1m-qsznr.

        DO.
          UNASSIGN <fs_field>.
          ASSIGN COMPONENT sy-index OF STRUCTURE ls_e1lfb1m TO <fs_field>.
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.
          IF <fs_field> IS ASSIGNED.
            IF <fs_field> IS INITIAL.
              <fs_field> = '/'.
            ENDIF.
          ENDIF.
        ENDDO.

        <lfs_idoc_data>-sdata = ls_e1lfb1m.

        CLEAR ls_bukrs.
      ENDLOOP.
    ENDIF.
*// RAMP INT0003
    IF ls_e1lfa1m-brsch NE 'WC'.
      SELECT bukrs,butxt FROM t001 INTO TABLE @DATA(lt_comp).
      IF sy-subrc IS INITIAL.
        IF lr_excl_company[] IS NOT INITIAL.
          DELETE lt_comp WHERE bukrs IN lr_excl_company[].
        ENDIF.
        READ TABLE idoc_data INTO ls_idoc_data
                             WITH KEY docnum = ls_idoc_ctrl-docnum
                                      segnam = 'E1LFB1M'.
        IF sy-subrc  IS INITIAL.
          ls_e1lfb1m = ls_idoc_data-sdata.
          DELETE idoc_data WHERE docnum = ls_idoc_ctrl-docnum
                             AND segnam = 'E1LFB1M'.
          READ TABLE idoc_data INTO DATA(ls_idoc_data_1)
                               WITH KEY docnum = ls_idoc_ctrl-docnum
                                        segnam = 'E1LFBWM'.
          IF sy-subrc IS INITIAL.
            ls_e1lfbwm = ls_idoc_data_1-sdata.
            DELETE idoc_data WHERE docnum = ls_idoc_ctrl-docnum
                             AND segnam = 'E1LFBWM'.
          ENDIF.
          LOOP AT idoc_data INTO DATA(ls_idoc_data_2) WHERE  docnum = ls_idoc_ctrl-docnum
                                                   AND segnam = 'E1LFBKM' .
            ls_e1lfbkm = ls_idoc_data_2-sdata.
            DO.
              UNASSIGN <fs_field>.
              ASSIGN COMPONENT sy-index OF STRUCTURE ls_e1lfbkm TO <fs_field>.
              IF sy-subrc NE 0.
                EXIT.
              ENDIF.
              IF <fs_field> IS ASSIGNED.
                IF <fs_field> IS INITIAL.
                  <fs_field> = '/'.
                ENDIF.
              ENDIF.
            ENDDO.
            ls_idoc_data_2-sdata = ls_e1lfbkm.
            APPEND ls_idoc_data_2 TO lt_idoc_data.
          ENDLOOP.
          DELETE idoc_data WHERE docnum = ls_idoc_ctrl-docnum
                             AND segnam = 'E1LFBKM'.
*>> BOC
          DO.
            UNASSIGN <fs_field>.
            ASSIGN COMPONENT sy-index OF STRUCTURE ls_e1lfb1m TO <fs_field>.
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.
            IF <fs_field> IS ASSIGNED.
              IF <fs_field> IS INITIAL.
                <fs_field> = '/'.
              ENDIF.
            ENDIF.
          ENDDO.

          DO.
            UNASSIGN <fs_field>.
            ASSIGN COMPONENT sy-index OF STRUCTURE ls_e1lfbwm TO <fs_field>.
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.
            IF <fs_field> IS ASSIGNED.
              IF <fs_field> IS INITIAL.
                <fs_field> = '/'.
              ENDIF.
            ENDIF.
          ENDDO.
*<< EOC

          LOOP AT lt_comp INTO DATA(ls_comp).
            ls_e1lfb1m-bukrs = ls_comp-bukrs.
            ls_idoc_data-sdata = ls_e1lfb1m.
            APPEND ls_idoc_data TO idoc_data[].
            IF ls_idoc_data_1 IS NOT INITIAL.
              ls_e1lfbwm-bukrs = ls_comp-bukrs.
              ls_e1lfbwm-bukrs_gl = ls_comp-bukrs.
              ls_idoc_data_1-sdata = ls_e1lfbwm.
              APPEND ls_idoc_data_1 TO idoc_data[].
            ENDIF.
          ENDLOOP.
          APPEND LINES OF lt_idoc_data TO idoc_data[].
        ENDIF.

      ENDIF.
    ENDIF.
*// Mapping of Search Term field
    READ TABLE zcl_ps_utility_tools=>gt_vendor_idoc_data ASSIGNING FIELD-SYMBOL(<lfs_vend_data>)
                       WITH KEY lifnr =  ls_e1lfa1m-lifnr.
    IF sy-subrc IS NOT INITIAL.
      APPEND INITIAL LINE TO zcl_ps_utility_tools=>gt_vendor_idoc_data
                ASSIGNING <lfs_vend_data>.
*    IF sy-subrc IS INITIAL.
      <lfs_vend_data>-lifnr = ls_e1lfa1m-lifnr.
      IF ls_e1lfa1m-mcod2 NE '/'.
        <lfs_vend_data>-mcod2 = ls_e1lfa1m-mcod2.
      ENDIF.
      IF ls_e1lfa1m-brsch NE '/'.
        <lfs_vend_data>-brsch = ls_e1lfa1m-brsch.
      ENDIF.
    ELSE.
      IF ls_e1lfa1m-mcod2 NE '/'.
        <lfs_vend_data>-mcod2 = ls_e1lfa1m-mcod2.
      ENDIF.
      IF ls_e1lfa1m-brsch NE '/'.
        <lfs_vend_data>-brsch = ls_e1lfa1m-brsch.
      ENDIF.
    ENDIF.
  ENDIF.
ENDLOOP.
IF lv_error IS NOT INITIAL.
  RETURN.
ENDIF.

ENDENHANCEMENT.
