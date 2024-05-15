class ZCL_MM_BADI_IMPL_PO_WF_EVAL definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_SWF_FLEX_IFS_CONDITION_DEF .
  interfaces IF_SWF_FLEX_IFS_CONDITION_EVAL .

  class-data GT_CC_HIER_LEVELS type ZTT_CC_HIER_LEVEL .
ENDCLASS.



CLASS ZCL_MM_BADI_IMPL_PO_WF_EVAL IMPLEMENTATION.


  METHOD if_swf_flex_ifs_condition_def~get_conditions.

* condition id - value to be changed
    CONSTANTS: co_id TYPE if_swf_flex_ifs_condition_def=>ty_condition_id VALUE 'CL_SWF_FLEX_IFS_BADI_COND_SAMP' ##NO_TEXT.

    ct_condition = VALUE #(
      ( id = co_id subject = 'Is amount higher than'(001) type = if_swf_flex_ifs_condition_def=>cs_condtype-start_step )
      ).

    ct_parameter = VALUE #(
      ( id = co_id name = 'Amount'(002) xsd_type = if_swf_flex_ifs_condition_def=>cs_xstype-integer mandatory = abap_true )
      ).

  ENDMETHOD.


  METHOD if_swf_flex_ifs_condition_eval~evaluate_condition.

    TYPES: BEGIN OF ty_position,
             otype TYPE otype,
             objid TYPE hrobjid,
           END OF ty_position.

    TYPES: BEGIN OF ty_hrp1001,
             objid TYPE hrobjid,
             sobid TYPE pa0001-pernr,
           END OF ty_hrp1001.

    TYPES: BEGIN OF ty_agent,
             user_id TYPE syuname,
           END OF ty_agent.

    DATA: lv_emp_number   TYPE ess_emp-employeenumber,
          lv_emp_level    TYPE ess_emp-employeesubgroup,
          lv_emp_position TYPE ess_emp-position.

    DATA: lt_agent          TYPE TABLE OF ty_agent.

    DATA: lt_hier_positions TYPE TABLE OF objec,
          lt_hier_struc     TYPE TABLE OF struc,
          lt_positions      TYPE TABLE OF ty_position,
          lt_hrp1001        TYPE TABLE OF ty_hrp1001,
          lv_current_level  TYPE numc2,
          lv_prev_level     TYPE numc2,
          lv_ebeln          TYPE ebeln.

* condition id - value to be changed
    CONSTANTS co_id TYPE if_swf_flex_ifs_condition_def=>ty_condition_id VALUE 'LEVEL_ID' ##NO_TEXT.

    cv_is_true = abap_false.

    IF is_condition-condition_id <> co_id.
      RETURN.
    ENDIF.

    READ TABLE it_parameter_value INTO DATA(ls_param_value)
      INDEX 1.

    TRY.
        DATA(lv_level) = ls_param_value-value.
      CATCH cx_root INTO DATA(lx_exc) ##CATCH_ALL.
        RAISE EXCEPTION TYPE cx_ble_runtime_error
          EXPORTING
            previous = lx_exc.
    ENDTRY.

*** New Logic START >>>>>>>>>
    DATA(lv_level_1) = lv_level.
    REPLACE 'L' IN lv_level_1 WITH space.
    lv_current_level =  lv_level_1.

    lv_ebeln = is_sap_object_node_type-sont_key_part_1.

    SELECT SINGLE ebeln,ernam FROM ekko INTO @DATA(ls_ekko)
     WHERE ebeln = @lv_ebeln.
    IF sy-subrc IS INITIAL.

    ENDIF.

    SELECT ebeln,ebelp,loekz,brtwr FROM ekpo INTO TABLE @DATA(lt_ekpo)
      WHERE ebeln = @lv_ebeln.
    IF sy-subrc IS INITIAL.
      DELETE lt_ekpo WHERE loekz IS NOT INITIAL.
      SORT lt_ekpo BY ebeln ebelp.
      READ TABLE lt_ekpo INTO DATA(ls_ekpo) INDEX 1.
    ENDIF.

    DATA(lv_po_total) =  REDUCE #(
                    INIT subtotal = 0
                    FOR ls_item IN lt_ekpo
                    NEXT subtotal  = subtotal  + ls_item-brtwr
                ).

    SELECT SINGLE kostl FROM ekkn INTO @DATA(lv_kostl)
      WHERE ebeln = @lv_ebeln
        AND ebelp = @ls_ekpo-ebelp .
    IF sy-subrc IS INITIAL
      AND lv_kostl IS NOT INITIAL.
      SELECT SINGLE verak_user FROM csks INTO @DATA(lv_cost_user)
        WHERE kokrs = 'PSCO'
          AND kostl = @lv_kostl.
      IF sy-subrc IS INITIAL.

      ENDIF.
    ENDIF.

    SELECT * FROM zappr_limit INTO TABLE @DATA(lt_appr_limit)."#EC CI_NOWHERE
    IF sy-subrc IS INITIAL.
      SORT lt_appr_limit BY zlevel .
    ENDIF.

    CALL FUNCTION 'HR_GETEMPLOYEEDATA_FROMUSER'
      EXPORTING
        username                  = lv_cost_user
        validbegin                = sy-datum
      IMPORTING
        employeenumber            = lv_emp_number
        employeesubgroup          = lv_emp_level
        position                  = lv_emp_position
      EXCEPTIONS
        user_not_found            = 1
        countrygrouping_not_found = 2
        infty_not_found           = 3
        OTHERS                    = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF line_exists( lt_appr_limit[ zlevel_hr = lv_emp_level ] ).
      DATA(lv_cc_level) = lt_appr_limit[ zlevel_hr = lv_emp_level ]-zlevel.
    ENDIF.
    lv_emp_level = lv_cc_level.

    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype       = 'S'
        act_objid       = lv_emp_position
        act_wegid       = 'A002'
        act_begda       = sy-datum
        act_endda       = sy-datum
*       ACT_TDEPTH      = 0
        act_tflag       = 'X'
        act_vflag       = 'X'
        authority_check = 'X'
      TABLES
*       result_tab      = lt_hier_positions
        result_objec    = lt_hier_positions
        result_struc    = lt_hier_struc
      EXCEPTIONS
        no_plvar_found  = 1
        no_entry_found  = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    lt_positions = CORRESPONDING #( lt_hier_positions ).
    IF lt_positions IS NOT INITIAL.
      SELECT objid, sobid FROM hrp1001 INTO TABLE @DATA(lt_hrp1001_1)
        FOR ALL ENTRIES IN @lt_positions
         WHERE otype = 'S'
           AND objid = @lt_positions-objid
           AND plvar = '01'
           AND rsign = 'A'
           AND relat = '008'
           AND endda = '99991231'.
      lt_hrp1001 = CORRESPONDING #( lt_hrp1001_1 ).
      IF sy-subrc IS INITIAL AND lt_hrp1001[] IS NOT INITIAL.
        SELECT a~pernr, a~persk AS persk_hr," a~persk,
               b~usrid FROM pa0001 AS a
          LEFT JOIN pa0105 AS b   "#EC CI_BUFFJOIN
          ON b~pernr = a~pernr
          INTO TABLE @DATA(lt_hier_levels)
          FOR ALL ENTRIES IN @lt_hrp1001
          WHERE a~pernr EQ @lt_hrp1001-sobid
            AND a~endda EQ '99991231'
            AND b~endda EQ '99991231'
            AND b~usrty EQ '0001'.
        IF sy-subrc IS INITIAL.
*          LOOP AT lt_hier_levels ASSIGNING FIELD-SYMBOL(<lfs_hier_levels>).
*             CLEAR: <lfs_hier_levels>-persk.
*             READ TABLE lt_appr_limit INTO DATA(ls_appr_limit_1)
*                       WITH KEY zlevel_hr = <lfs_hier_levels>-persk_hr.
*             IF sy-subrc IS INITIAL.
*                <lfs_hier_levels>-persk = ls_appr_limit_1-zlevel.
*             ENDIF.
*          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

* Fetch previous approvers
*    SELECT a~wi_id,a~top_wi_id,a~wi_rh_task,a~crea_tmp",b~user_id,c~wi_aagent
*      FROM sww_wi2obj AS a
*      INTO TABLE @DATA(lt_work_item)
*      WHERE wi_rh_task = 'TS00800531' AND
*         instid = @lv_ebeln.
*    IF sy-subrc IS INITIAL.
*      SORT lt_work_item BY crea_tmp DESCENDING.
*      DATA(ls_work_item) = lt_work_item[ 1 ].
*      DELETE lt_work_item WHERE top_wi_id NE ls_work_item-top_wi_id.
*      IF lt_work_item[] IS NOT INITIAL.
*        SELECT wi_aagent FROM  swwwihead APPENDING TABLE lt_agent
*           FOR ALL ENTRIES IN lt_work_item
*           WHERE wi_id = lt_work_item-wi_id.
*        IF sy-subrc IS INITIAL.
*        ENDIF.
*        DELETE lt_agent WHERE user_id IS INITIAL.
*      ENDIF.
*    ENDIF.

**>> Begin change 18/04/2024 KDURAI
    DATA: lt_valid_level TYPE ztt_cc_hier_level,
          lv_last_valid_level_1 TYPE char1,
          lv_last_valid_level_2 TYPE char1,
          lv_numc2       TYPE numc2,
          lv_error_setup TYPE char1 VALUE 'X'.

    CLEAR: lt_valid_level,lv_last_valid_level_1,lv_last_valid_level_2.

    LOOP AT lt_hier_struc INTO DATA(ls_hier_struc).

      lv_numc2 = ls_hier_struc-level.

      READ TABLE lt_hrp1001_1 INTO DATA(ls_hrp1001_1)
                 WITH KEY objid = ls_hier_struc-objid.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_hier_levels INTO DATA(ls_hier_levels)
                 WITH KEY pernr = ls_hrp1001_1-sobid.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_appr_limit INTO DATA(ls_appr_limit_1)
                 WITH KEY zlevel_hr = ls_hier_levels-persk_hr.
          IF sy-subrc IS NOT INITIAL.
            CLEAR ls_appr_limit_1.
          ENDIF.

          IF  ls_ekko-ernam NE ls_hier_levels-usrid AND
              ls_hier_levels-usrid IS NOT INITIAL." AND " Check user id exists or not
*              NOT line_exists( lt_agent[ user_id = ls_hier_levels-usrid ] ). " Skip if already approved


            APPEND INITIAL LINE TO lt_valid_level ASSIGNING FIELD-SYMBOL(<lfs_valid_level>).
            IF sy-subrc IS INITIAL.
*              lv_numc2 = lv_numc2 + 1.
              <lfs_valid_level>-persk    = lv_numc2.
              <lfs_valid_level>-approver = ls_hier_levels-usrid.
              <lfs_valid_level>-persk_hr = ls_hier_levels-persk_hr.
              <lfs_valid_level>-cc_user  = lv_cost_user.
            ENDIF.

            IF lv_po_total GE ls_appr_limit_1-zamount_from
              AND lv_po_total LE ls_appr_limit_1-zamount_to.
              CLEAR lv_error_setup.
              lv_last_valid_level_1 =  abap_true.
              EXIT.
*            ELSE.
*              IF lv_last_valid_level_1 IS NOT INITIAL.
*                 EXIT.
*              ENDIF.
            ENDIF.


*// This check will be applicable only when no approver found
            IF lv_po_total LE ls_appr_limit_1-zamount_to
              AND lv_last_valid_level_1 IS INITIAL.
              CLEAR lv_error_setup.
              lv_last_valid_level_2 = abap_true.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    gt_cc_hier_levels[] = lt_valid_level[].

    IF lv_error_setup IS NOT INITIAL OR line_exists( lt_valid_level[ persk = lv_current_level ] ).
      cv_is_true = abap_true.
      RETURN.
    ENDIF.
**<< End change 18/04/2024

**>> BOC comment KDURAI 18/04/2024
*    IF lv_current_level = '01' AND lv_cost_user EQ ls_ekko-ernam.
*      cv_is_true = abap_false.
*      RETURN.
*    ENDIF.
*
*    IF lv_current_level = '01'.
*      cv_is_true = abap_true.
*      RETURN.
*    ENDIF.
*
**// Current level user
*    IF line_exists( lt_hier_levels[ persk = lv_current_level ] ).
*      DATA(lv_curr_level_user) = lt_hier_levels[ persk = lv_current_level ]-usrid.
*    ENDIF.
*
**// Get previous level user
*    lv_prev_level =  lv_current_level - 1.
*    IF line_exists( lt_hier_levels[ persk = lv_prev_level ] ).
*      DATA(lv_prev_level_user) = lt_hier_levels[ persk = lv_prev_level ]-usrid.
*    ENDIF.
*
*    DELETE gt_cc_hier_levels WHERE cc_user = lv_cost_user.
*    LOOP AT lt_hier_levels INTO DATA(ls_hier_level).
*      APPEND INITIAL LINE TO gt_cc_hier_levels ASSIGNING FIELD-SYMBOL(<lfs_hier_level>).
*      <lfs_hier_level>-cc_user = lv_cost_user.
*      <lfs_hier_level>-persk =  ls_hier_level-persk.
*      <lfs_hier_level>-approver =  ls_hier_level-usrid.
*    ENDLOOP.
*
**>> Calculate highest level of approver for given PO amount
*      LOOP AT lt_appr_limit INTO DATA(ls_appr_limit).
**        IF ls_appr_limit-zamount_from GE lv_po_total. " KDURAI 11/04/2024
*        IF lv_po_total GE ls_appr_limit-zamount_from " KDURAI 11/04/2024
*          AND lv_po_total LE ls_appr_limit-zamount_to. " KDURAI 11/04/2024
*          READ TABLE lt_hier_levels INTO ls_hier_level
*                                    WITH KEY persk = ls_appr_limit-zlevel.
*          IF sy-subrc IS INITIAL
*            AND ls_hier_level-usrid NE ls_ekko-ernam.
*            DATA(lv_valid_last_level) = ls_appr_limit-zlevel.
**            EXIT.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*
**>> BOC " KDURAI 11/04/2024
**>> Incase last approver not found, check based on the below logic
** Could be scenario where approver not exists in organization, deactivated
*    IF lv_valid_last_level IS INITIAL.
**>> Calculate highest level of approver for given PO amount
*      LOOP AT lt_appr_limit INTO ls_appr_limit.
*        IF lv_po_total LE ls_appr_limit-zamount_to.
*          READ TABLE lt_hier_levels INTO ls_hier_level
*                                    WITH KEY persk = ls_appr_limit-zlevel.
*          IF sy-subrc IS INITIAL
*            AND ls_hier_level-usrid NE ls_ekko-ernam.
*             lv_valid_last_level = ls_appr_limit-zlevel.
*            EXIT.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
**>> Fallback
**    if any issue in last level calculation, then enable workflow to all approvers to avoid workflow approval skip
*   IF lv_valid_last_level IS INITIAL.
*     cv_is_true = abap_true.
*     RETURN.
*   ENDIF.
**<< EOC " KDURAI 11/04/2024
*
**// Skip cost center user level repeats again
*    IF lv_current_level EQ lv_emp_level.
*      RETURN.
*    ENDIF.
*
*    IF "   NOT line_exists( lt_hier_levels[ persk = lv_current_level usrid = lv_cost_user ] ) AND
*      NOT line_exists( lt_agent[ user_id = lv_curr_level_user ] )  AND " Skip previous approver
*      lv_curr_level_user NE ls_ekko-ernam " Skip if current level user match with PO creator
*      AND lv_curr_level_user IS NOT INITIAL.
*
**// Get current eligible approval amount
*       READ TABLE lt_appr_limit INTO ls_appr_limit WITH KEY zlevel = lv_current_level.
*       IF sy-subrc IS INITIAL.
**         DATA(lv_zamount) = ls_appr_limit-zamount.
*       ENDIF.
**// Check conditions
*      IF   ( lv_po_total GE ls_appr_limit-zamount_from AND lv_current_level > lv_emp_level AND lv_current_level = ls_appr_limit-zlevel )
*        OR  ( ( lv_prev_level_user EQ ls_ekko-ernam OR  lv_prev_level_user IS INITIAL ) AND lv_current_level LE lv_valid_last_level ).
*        cv_is_true = abap_true.
*      ENDIF.
*
*    ENDIF.
*******END <<<<<<<<
**<< EOC comment KDURAI 18/04/2024

  ENDMETHOD.
ENDCLASS.
