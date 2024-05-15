class ZCL_PS_MM_BADI_IMPL_PR definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_MMPUR_WORKFLOW_AGENTS_V2 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PS_MM_BADI_IMPL_PR IMPLEMENTATION.


  METHOD if_mmpur_workflow_agents_v2~get_approvers.
    DATA:
      ls_badi_approver     TYPE if_mmpur_workflow_agents_v2=>bd_mmpur_s_badi_approver,
      lt_badi_approver     TYPE if_mmpur_workflow_agents_v2=>bd_mmpur_t_badi_approver,
      ls_previous_approver TYPE if_mmpur_workflow_agents_v2=>bd_mmpur_s_previous_approver,
      ls_new_approver      TYPE if_mmpur_workflow_agents_v2=>bd_mmpur_s_badi_approver.

    DATA: lv_level TYPE i.
    DATA: lv_previousstep TYPE i.


**  based on total number of steps determine workflow and for each workflow step determine approvers using step indo and pevious approver list
*    IF stepinfo-totalsteps = 2.
      IF stepinfo-currentstep = 1 .
       approverlist = zcl_ps_utility_tools=>get_users_from_role(
                                                iv_role = 'Z_BR_MANAGER' ) .

      ELSEIF stepinfo-currentstep = 2.
       approverlist = zcl_ps_utility_tools=>get_users_from_role(
                                                iv_role = 'Z_BR_MANAGER1' ) .
      ELSEIF stepinfo-currentstep = 3.
       approverlist = zcl_ps_utility_tools=>get_users_from_role(
                                                iv_role = 'Z_BR_MANAGER2' ) .
      ENDIF.

**  remove the previous approvers from the list of BAdI approvers.
*    LOOP AT previousapproverlist INTO ls_previous_approver.
*      READ TABLE lt_badi_approver INTO ls_badi_approver
*          WITH KEY businessuser = ls_previous_approver-businessuser.
*      CHECK sy-subrc = 0.
*      DELETE lt_badi_approver WHERE approvallevel = ls_badi_approver-approvallevel.
*    ENDLOOP.

**  determine the next approval level and appropriate approvers
*    READ TABLE lt_badi_approver INTO ls_badi_approver INDEX 1.
*    LOOP AT lt_badi_approver INTO ls_new_approver
*            WHERE approvallevel = ls_badi_approver-approvallevel.
*      APPEND ls_new_approver-businessuser TO approverlist.
*    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
