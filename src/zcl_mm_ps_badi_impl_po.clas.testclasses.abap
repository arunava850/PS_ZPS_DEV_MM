*CLASS lcl_exmpl_workflow_agents_v2 DEFINITION DEFERRED.
*CLASS cl_exmpl_workflow_agents_v2 DEFINITION LOCAL FRIENDS lcl_exmpl_workflow_agents_v2.

CLASS lcl_exmpl_workflow_agents_v2 DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PUBLIC SECTION.
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_mm_ps_badi_impl_po.  "class under test
    METHODS: get_approvers FOR TESTING.

    METHODS: setup.
    METHODS: teardown.
    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.

ENDCLASS.       "lcl_Exmpl_Workflow_Agents_V2


CLASS lcl_exmpl_workflow_agents_v2 IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.


*    f_cut = NEW cl_exmpl_workflow_agents_v2( ) .
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD get_approvers.

    DATA businessobject TYPE businessobjecttype VALUE ''.
    DATA purchasingdocument TYPE purchasingdocument.
    DATA purchasingdocumentitem TYPE purchasingdocumentitem.
    DATA workflowscenario TYPE workflowscenario.
    DATA approverlist TYPE mmpur_t_wfagent.


    f_cut = NEW zcl_mm_ps_badi_impl_po( ) .

    TRY.
        f_cut->if_mmpur_workflow_agents_v2~get_approvers(
          EXPORTING
            businessobject = businessobject
            purchasingdocument = purchasingdocument
           purchasingdocumentitem = purchasingdocumentitem
           workflowscenario = workflowscenario
          CHANGING
            approverlist = approverlist ).
      CATCH cx_ble_runtime_error.
    ENDTRY.


  ENDMETHOD.




ENDCLASS.
