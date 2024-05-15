class ZCL_IM_MM_ENH_PO_PROCESS_IMPL definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_PROCESS_PO_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_ENH_PO_PROCESS_IMPL IMPLEMENTATION.


  METHOD if_ex_me_process_po_cust~check.

    TYPES: BEGIN OF ty_agent,
             user_id TYPE syuname,
           END OF ty_agent.
    DATA: lt_agent         TYPE TABLE OF ty_agent,
          lv_open_approval TYPE c.

*   CHECK sy-mandt EQ '050'.

    DATA(ls_po_hdr) = im_header->get_data( ).
    SELECT a~wi_id,a~top_wi_id,a~wi_rh_task,a~crea_tmp",b~user_id,c~wi_aagent
      FROM sww_wi2obj AS a
      INTO TABLE @DATA(lt_work_item)
      WHERE wi_rh_task = 'TS00800531' AND
         instid = @ls_po_hdr-ebeln.
    IF sy-subrc IS INITIAL.
      SORT lt_work_item BY crea_tmp DESCENDING.
      DATA(ls_work_item) = lt_work_item[ 1 ].
      DELETE lt_work_item WHERE top_wi_id NE ls_work_item-top_wi_id.

      IF lt_work_item[] IS NOT INITIAL.
        SELECT user_id FROM  swwuserwi INTO TABLE lt_agent
           FOR ALL ENTRIES IN lt_work_item
           WHERE wi_id = lt_work_item-wi_id.
        IF sy-subrc IS INITIAL.
          lv_open_approval = abap_true.
        ENDIF.

        SELECT wi_aagent FROM  swwwihead APPENDING TABLE lt_agent
           FOR ALL ENTRIES IN lt_work_item
           WHERE wi_id = lt_work_item-wi_id.
        IF sy-subrc IS INITIAL.
        ENDIF.
        DELETE lt_agent WHERE user_id IS INITIAL.
      ENDIF.

      APPEND INITIAL LINE TO lt_agent ASSIGNING FIELD-SYMBOL(<lfs_agent>).
      <lfs_agent>-user_id = ls_po_hdr-ernam.

    ENDIF.

    DATA(lt_super_users) = zcl_ps_utility_tools=>get_users_from_role(
                                               iv_role = 'ZES:FIN_PO_EDIT_SUPERUSER' ) .

    IF NOT line_exists( lt_super_users[ table_line = sy-uname ] ).
      SELECT SINGLE procstat FROM ekko INTO @DATA(lv_procstat)
        WHERE ebeln = @ls_po_hdr-ebeln.
      IF sy-subrc IS INITIAL.
        IF ( sy-tcode EQ 'ME21N' OR sy-tcode EQ 'ME22N' OR sy-tcode EQ 'ME23N'
          OR sy-tcode EQ 'ME59N' ) AND
          ( lv_procstat = '05' OR ( lv_open_approval IS NOT INITIAL AND NOT line_exists( lt_agent[ user_id = sy-uname ] ) ) ).
          ch_failed = abap_true.
          MESSAGE 'PO change is not possible since PO already released..' TYPE 'E'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method IF_EX_ME_PROCESS_PO_CUST~CLOSE.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~INITIALIZE.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~OPEN.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~POST.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ACCOUNT.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_PO_CUST~PROCESS_SCHEDULE.
  endmethod.
ENDCLASS.
