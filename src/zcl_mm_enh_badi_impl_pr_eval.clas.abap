class ZCL_MM_ENH_BADI_IMPL_PR_EVAL definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_SWF_FLEX_IFS_CONDITION_DEF .
  interfaces IF_SWF_FLEX_IFS_CONDITION_EVAL .
ENDCLASS.



CLASS ZCL_MM_ENH_BADI_IMPL_PR_EVAL IMPLEMENTATION.


  METHOD if_swf_flex_ifs_condition_def~get_conditions.

** condition id - value to be changed
*    CONSTANTS: co_id TYPE if_swf_flex_ifs_condition_def=>ty_condition_id VALUE 'CL_SWF_FLEX_IFS_BADI_COND_SAMP' ##NO_TEXT.
*
*    ct_condition = VALUE #(
*      ( id = co_id subject = 'Is amount higher than'(001) type = if_swf_flex_ifs_condition_def=>cs_condtype-start_step )
*      ).
*
*    ct_parameter = VALUE #(
*      ( id = co_id name = 'Amount'(002) xsd_type = if_swf_flex_ifs_condition_def=>cs_xstype-integer mandatory = abap_true )
*      ).

  ENDMETHOD.


  METHOD if_swf_flex_ifs_condition_eval~evaluate_condition.

* condition id - value to be changed
    CONSTANTS co_id TYPE if_swf_flex_ifs_condition_def=>ty_condition_id VALUE 'ZCL_MM_ENH_BADI_IMPL_PR_EVAL' ##NO_TEXT.

    cv_is_true = abap_false.
*
    IF is_condition-condition_id <> co_id.
      RAISE EXCEPTION TYPE cx_ble_runtime_error.
    ELSE.
      cv_is_true = abap_true.
    ENDIF.
*
*    READ TABLE it_parameter_value INTO DATA(ls_param_value)
*      INDEX 1.
*    IF sy-subrc <> 0 OR ls_param_value-value IS INITIAL.
** paramter is defined as mandatory
*      RAISE EXCEPTION TYPE cx_ble_runtime_error.
*    ENDIF.
*
*    TRY.
*        DATA(lv_amount) = CONV i( ls_param_value-value ).
*      CATCH cx_root INTO DATA(lx_exc) ##CATCH_ALL.
*        RAISE EXCEPTION TYPE cx_ble_runtime_error
*          EXPORTING
*            previous = lx_exc.
*    ENDTRY.
*
** evaluate condition
*    IF lv_amount = 0.
** check on business object instance not needed, condition is always TRUE
*      cv_is_true = abap_true.
*    ELSE.
*****   evaluation for your business object instance  ****
**   1: get business object instance by application key IS_SAP_OBJECT_NODE_TYPE
**     select single amount from <CDS_VIEWNAME> into @DATA(lv_inst_amount)
**        where ID = @is_sap_object_node_type-sont_key_part_1.
*
**   2: If object instance amount is greater than condition value return true else false
**    IF lv_inst_amount > lv_amount.
**     cv_is_true = abap_true.
**    ELSE.
**     cv_is_true = abap_false.
**    ENDIF.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
