class ZCL_MM_PO_EMAILS_CONTROL definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_BADI_MM_PO_OC_EMAIL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MM_PO_EMAILS_CONTROL IMPLEMENTATION.


  METHOD if_badi_mm_po_oc_email~change_body.
    DATA ls_body TYPE soli.

    DATA gv_po_enh004active.
*>> BOC KAD4COB
*      DO.
*
*      ENDDO.
*<< EOC KAD4COB
    IMPORT gv_po_enh004active = gv_po_enh004active  FROM MEMORY ID 'ENH0004'.
    IF gv_po_enh004active IS NOT INITIAL."   AND
*       im_nast_entry-parnr NE '0010000001'.
*      BREAK gpylla.
      CONCATENATE 'Dear Supplier,' '<br>' INTO ls_body-line.
      APPEND ls_body TO cht_body.
      CLEAR ls_body.
      MOVE '<br>' TO ls_body-line.
      APPEND ls_body TO cht_body.
      CLEAR ls_body.
      APPEND ls_body TO cht_body.
      CONCATENATE 'Please find attached document containing Public Storage purchase order. '  '<br>' INTO ls_body-line.
      APPEND ls_body TO cht_body.
      CLEAR ls_body.
      CONCATENATE 'Please contact us if any questions.'  '<br>' INTO ls_body-line.
      APPEND ls_body TO cht_body.
      CLEAR ls_body.
      MOVE '<br>' TO ls_body-line.
      APPEND ls_body TO cht_body.
      CLEAR ls_body.
      CONCATENATE  'Thank you,'  '<br>' INTO ls_body-line.
      APPEND ls_body TO cht_body.
      CLEAR ls_body.
      CONCATENATE  'Public Storage.'  '<br>' INTO ls_body-line.
      APPEND ls_body TO cht_body.
      CLEAR ls_body.
    ENDIF.

  ENDMETHOD.


  METHOD if_badi_mm_po_oc_email~change_recipients.
    DATA: lv_vendor  TYPE bu_partner,
          lr_partner TYPE RANGE OF bu_partner.

    DATA gv_po_enh004active.
    IMPORT gv_po_enh004active = gv_po_enh004active  FROM MEMORY ID 'ENH0004'.
    IF gv_po_enh004active IS NOT INITIAL."   AND im_nast_entry-parnr NE '0010000001'.
*      BREAK gpylla.

      SELECT SINGLE ebeln, bsart, ernam, ekgrp, reswk FROM ekko
        INTO @DATA(ls_ekko)
        WHERE ebeln = @im_nast_entry-objky+0(10).
      IF sy-subrc IS INITIAL.
        SELECT SINGLE persnumber FROM usr21
          INTO @DATA(lv_persnumber)
          WHERE bname = @ls_ekko-ernam.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE smtp_addr
            FROM adr6 INTO @DATA(lv_smtp_addr)
            WHERE persnumber = @lv_persnumber.
          IF lv_smtp_addr IS NOT INITIAL.
            REFRESH : cht_cc_recipient_list.
            APPEND VALUE #( recipient = lv_smtp_addr ) TO cht_cc_recipient_list.
            CLEAR lv_smtp_addr.
          ENDIF.
        ENDIF.
      ENDIF.
*      IF sy-sysid EQ 'PRD'.
* ADD PURCHASING GROUP MAIL
      SELECT SINGLE smtp_addr FROM t024 INTO @lv_smtp_addr
        WHERE ekgrp = @ls_ekko-ekgrp.
      IF sy-subrc IS INITIAL.
        APPEND VALUE #( recipient = lv_smtp_addr ) TO cht_cc_recipient_list.
      ENDIF.
*      ENDIF.

*>> BOC KAD4COB 25/10/2023
    lv_vendor = im_nast_entry-parnr.
    IF ls_ekko-bsart EQ 'UB'.
      SELECT SINGLE werks FROM ekpo INTO @DATA(lv_plant)
        WHERE ebeln = @ls_ekko-ebeln
          AND loekz = 'L'.
      IF sy-subrc IS NOT INITIAL.
        CLEAR lv_plant.
      ENDIF.
      SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc) "#EC CI_ALL_FIELDS_NEEDED
        WHERE name = 'ZMM_PO_STO_PLANT_SUPPLIER'
          AND low  = @ls_ekko-reswk." @lv_plant.
      IF sy-subrc IS INITIAL .
        lv_vendor = ls_tvarvc-high.
        IF lv_vendor IS NOT INITIAL.
          lv_vendor = |{ lv_vendor ALPHA = IN }|.
          SELECT SINGLE adrnr FROM lfa1 INTO @DATA(lv_addrnumber)
            WHERE lifnr = @lv_vendor.
        ENDIF.
      ENDIF.
    ENDIF.
*<< EOC KAD4COB 25/10/2023

* ADD VENDER MAIL ADDRESSES
      IF lv_vendor IS NOT INITIAL.
        SELECT SINGLE addrnumber FROM but020
          INTO lv_addrnumber
          WHERE partner = im_nast_entry-parnr.
      ENDIF.
      IF lv_addrnumber IS NOT INITIAL.
        SELECT consnumber, smtp_addr
          FROM adr6 INTO TABLE @DATA(lt_adr6)
          WHERE addrnumber = @lv_addrnumber.
         IF sy-subrc IS INITIAL.
           SORT lt_adr6 BY consnumber.
         ENDIF.
        READ TABLE lt_adr6 INTO DATA(ls_adr6) INDEX 1.
        IF sy-subrc IS INITIAL.
          REFRESH : cht_to_recipient_list.
          TRANSLATE ls_adr6-smtp_addr TO LOWER CASE.
          APPEND VALUE #( recipient = ls_adr6-smtp_addr ) TO cht_to_recipient_list.
        ENDIF.
        DESCRIBE TABLE lt_adr6 LINES DATA(lv_lines).
        IF lv_lines GT 1.
          DELETE lt_adr6 INDEX 1.
          SORT lt_adr6 BY consnumber.
          DATA : ls_receipients LIKE LINE OF cht_cc_recipient_list.
          LOOP AT lt_adr6 INTO ls_adr6.
            TRANSLATE ls_adr6-smtp_addr TO LOWER CASE.
            ls_receipients = ls_adr6-smtp_addr.
            APPEND ls_receipients TO cht_cc_recipient_list.
          ENDLOOP.
        ENDIF.
*      ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  method IF_BADI_MM_PO_OC_EMAIL~CHANGE_SENDER.

    DATA(gv_po_enh004active) = abap_true.
    IMPORT gv_po_enh004active = gv_po_enh004active  FROM MEMORY ID 'ENH0004'.
    IF gv_po_enh004active IS NOT INITIAL.
      SELECT SINGLE EKGRP FROM EKKO
        INTO @DATA(LV_EKGRP)
        WHERE EBELN = @IM_NAST_ENTRY-OBJKY+0(10).




    ENDIF.



  endmethod.


  method IF_BADI_MM_PO_OC_EMAIL~CHANGE_SUBJECT.
  endmethod.
ENDCLASS.
