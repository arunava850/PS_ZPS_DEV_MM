class ZCL_MM_PS_BADI_IMPL_PO definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_MMPUR_WORKFLOW_AGENTS_V2 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MM_PS_BADI_IMPL_PO IMPLEMENTATION.


  METHOD if_mmpur_workflow_agents_v2~get_approvers.

    DATA: lv_step TYPE numc2.

    SELECT ebeln,ebelp,loekz,brtwr FROM ekpo INTO TABLE @DATA(lt_ekpo)
      WHERE ebeln = @purchasingdocument.
    IF sy-subrc IS INITIAL.
      DELETE lt_ekpo WHERE loekz IS NOT INITIAL.
      SORT lt_ekpo BY ebeln ebelp.
      READ TABLE lt_ekpo INTO DATA(ls_ekpo) INDEX 1.
    ENDIF.

    SELECT SINGLE kostl FROM ekkn INTO @DATA(lv_kostl)
      WHERE ebeln = @purchasingdocument
        AND ebelp = @ls_ekpo-ebelp.
    IF sy-subrc IS INITIAL
      AND lv_kostl IS NOT INITIAL.
      SELECT SINGLE verak_user FROM csks INTO @DATA(lv_cost_user)
        WHERE kokrs = 'PSCO'
          AND kostl = @lv_kostl.
      IF sy-subrc IS INITIAL.
        lv_step = stepinfo-currentstep.
        IF lv_step = '01'.
          APPEND INITIAL LINE TO approverlist ASSIGNING FIELD-SYMBOL(<lfs_approver>).
          <lfs_approver> = lv_cost_user.
        ELSE.
          READ TABLE zcl_mm_badi_impl_po_wf_eval=>gt_cc_hier_levels INTO DATA(ls_cc_hier)
             WITH KEY cc_user = lv_cost_user
                      persk   = lv_step.
          IF sy-subrc IS INITIAL.
            APPEND INITIAL LINE TO approverlist ASSIGNING <lfs_approver>.
            <lfs_approver> = ls_cc_hier-approver.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*// Add substitutes for approvers
    APPEND LINES OF zcl_ps_utility_tools=>get_substitutes( it_approverlist = approverlist[] )
             TO approverlist[].

  ENDMETHOD.
ENDCLASS.
