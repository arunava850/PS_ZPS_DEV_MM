"Name: \FU:IDOC_INPUT_MBGMC2\SE:BEGIN\EI
ENHANCEMENT 0 ZPS_MM_ENH_MBGMC2_GOODS_MVT.

DATA: ls_e1bp2017_gm_head_01 TYPE e1bp2017_gm_head_01,
      ls_docstat             TYPE bdidocstat.

READ TABLE idoc_contrl ASSIGNING FIELD-SYMBOL(<lfs_idoc_ctrl>) INDEX 1.
IF sy-subrc IS INITIAL.
  IF <lfs_idoc_ctrl> IS ASSIGNED.
    IF <lfs_idoc_ctrl>-mestyp CP 'ZMBGMCR*'.
      <lfs_idoc_ctrl>-mestyp = 'MBGMCR'.
    ENDIF.
  ENDIF.
  READ TABLE idoc_data INTO DATA(ls_idoc_data)
                       WITH KEY docnum = <lfs_idoc_ctrl>-docnum
                                segnam = 'E1BP2017_GM_HEAD_01'.
  IF sy-subrc  IS INITIAL.
    ls_e1bp2017_gm_head_01 = ls_idoc_data-sdata.
    SELECT mblnr,xblnr FROM matdoc "#EC CI_NOFIELD
      INTO @DATA(ls_matdoc) UP TO 1 ROWS
      WHERE xblnr =  @ls_e1bp2017_gm_head_01-ref_doc_no.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      ls_docstat = VALUE #(
           docnum = <lfs_idoc_ctrl>-docnum status = '51' msgty = 'E' msgid = 'WN' msgno = '099'
           msgv1 = |Already Mat.doc { ls_matdoc-mblnr } |
           msgv2 = ' exists with the same reference'
           msgv3 = |  { ls_matdoc-xblnr } |
           segnum = ls_idoc_data-segnum segfld = 'REF_DOC_NO' uname = sy-uname repid = sy-repid ).
      APPEND ls_docstat TO idoc_status[].
      RETURN.
    ENDIF.
  ENDIF.
ENDIF.
ENDENHANCEMENT.
