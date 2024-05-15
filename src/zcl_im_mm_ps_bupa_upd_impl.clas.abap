class ZCL_IM_MM_PS_BUPA_UPD_IMPL definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BUPA_GENERAL_UPDATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_PS_BUPA_UPD_IMPL IMPLEMENTATION.


  METHOD if_ex_bupa_general_update~change_before_update.

    DATA: lv_partner_guid TYPE bu_partner_guid,
          ls_bus000       TYPE bus000___i,
          ls_bus000_old   TYPE bus000___i,
          lt_but0is       TYPE TABLE OF vbut0is,
          lt_but0is_old   TYPE TABLE OF vbut0is.

    IF it_changed_instances[] IS NOT INITIAL.
      lv_partner_guid = it_changed_instances[ 1 ].

      CALL FUNCTION 'BUP_MEMORY_BUT000_GET'
        EXPORTING
*         IV_PARTNER       =
          iv_partner_guid  = lv_partner_guid
        IMPORTING
          es_but000        = ls_bus000
          es_but000_old    = ls_bus000_old
        EXCEPTIONS
          not_found        = 1
          parameter_error  = 2
          bpext_not_unique = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      READ TABLE zcl_ps_utility_tools=>gt_vendor_idoc_data INTO DATA(ls_vend_data)
                       WITH KEY lifnr = ls_bus000-partner.
      IF sy-subrc IS INITIAL AND ls_bus000-partner IS NOT INITIAL.
       IF ls_vend_data-brsch EQ 'WC'.
        ls_bus000-bu_sort2 = ls_vend_data-mcod2.
        CALL FUNCTION 'BUP_MEMORY_BUT000_FILL'
          EXPORTING
            is_but000       = ls_bus000
            is_but000_old   = ls_bus000_old
          EXCEPTIONS
            parameter_error = 1
            OTHERS          = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
       ENDIF.
*// Fill Industry field
        CALL FUNCTION 'BUP_MEMORY_BUT0IS_GET'
          EXPORTING
            iv_partner      = ls_bus000-partner
          TABLES
            et_but0is       = lt_but0is
            et_but0is_old   = lt_but0is_old
          EXCEPTIONS
            not_found       = 1
            parameter_error = 2
            OTHERS          = 3.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        IF lt_but0is_old[] IS NOT INITIAL.
          lt_but0is =  lt_but0is_old[].
          IF ls_vend_data-brsch IS NOT INITIAL.
            IF NOT line_exists( lt_but0is_old[ ind_sector = ls_vend_data-brsch ] ).
              APPEND INITIAL LINE TO lt_but0is ASSIGNING FIELD-SYMBOL(<lfs_but0is>).
              IF sy-subrc IS INITIAL.
                <lfs_but0is>-partner =  ls_bus000-partner.
                <lfs_but0is>-istype =  '0001'.
                <lfs_but0is>-ind_sector =  ls_vend_data-brsch.
              ENDIF.
            ENDIF.
          ENDIF.

          CALL FUNCTION 'BUP_MEMORY_BUT0IS_FILL'
            TABLES
              it_but0is       = lt_but0is
              it_but0is_old   = lt_but0is_old
            EXCEPTIONS
              parameter_error = 1
              OTHERS          = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
