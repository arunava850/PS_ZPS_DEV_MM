"Name: \PR:SAPLMECOM1\EX:MEPO_DOC_PROCESS_01\EI
ENHANCEMENT 0 ZPS_MM_ENH_PO_DETAIL.
   LOOP AT cht_items INTO DATA(ls_ekpo).
*     ls_ekpo =  <fs_ekpo>.
     READ TABLE zps_cl_mm_utility=>gt_mat_group
        ASSIGNING FIELD-SYMBOL(<lfs_mat_group>)
        WITH KEY ebeln = ls_ekpo-ebeln
                 ebelp = ls_ekpo-ebelp .
     IF sy-subrc IS INITIAL.
       <lfs_mat_group>-matkl = ls_ekpo-matkl.
       <lfs_mat_group>-loekz = ls_ekpo-loekz.
     ELSE.
       APPEND INITIAL LINE TO zps_cl_mm_utility=>gt_mat_group
        ASSIGNING <lfs_mat_group>.
       <lfs_mat_group>-ebeln = ls_ekpo-ebeln.
       <lfs_mat_group>-ebelp = ls_ekpo-ebelp.
       <lfs_mat_group>-matkl = ls_ekpo-matkl.
       <lfs_mat_group>-loekz = ls_ekpo-loekz.
     ENDIF.
   ENDLOOP.
ENDENHANCEMENT.
