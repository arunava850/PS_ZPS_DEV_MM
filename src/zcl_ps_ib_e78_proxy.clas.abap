class ZCL_PS_IB_E78_PROXY definition
  public
  create public .

public section.

  interfaces ZII_PS_IB_E78_PROXY .

  class-data GT_INPUT type ZDT_IB_INPUT_E78_MAIN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PS_IB_E78_PROXY IMPLEMENTATION.


  METHOD zii_ps_ib_e78_proxy~post_document.

    DATA: lt_input TYPE ztt_input_e78.

*****************************
**    IF 1 = 2.
*      CONSTANTS:
*      co_class TYPE seoclsname VALUE 'CX_MDC_STANDARD_MESSAGE_FAULT'.
*      DATA: lo_app_fault TYPE REF TO cx_ai_application_fault,
*            lo_exep      TYPE REF TO cx_mdc_standard_message_fault.
*      TRY.
*          CALL METHOD /aif/cl_enabler_proxy=>process_message
*            EXPORTING
*              is_input               = input
*              iv_exception_classname = co_class.
*        CATCH cx_ai_application_fault INTO lo_app_fault.
*          lo_exep ?= lo_app_fault.
*          RAISE EXCEPTION lo_exep.
*      ENDTRY.

*    ENDIF.
**************************8*

    lt_input = CORRESPONDING #( input-mt_ps_ib_e78_proxy-it_data1[] ).
    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<lfs_input>).
*      <lfs_input>-vendor_no = |{ <lfs_input>-vendor_no ALPHA = IN }|.
      DATA(lv_len) = strlen( <lfs_input>-property_no ).
      IF lv_len > 10.
        CLEAR <lfs_input>-property_no.
      ENDIF.
    ENDLOOP.

    CALL METHOD zcl_fi_e78_ap_trans_s4=>post_document
      CHANGING
        ct_input = lt_input.
*      importing
*        et_return = DATA(lt_return).

  ENDMETHOD.
ENDCLASS.
