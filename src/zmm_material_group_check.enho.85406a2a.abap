"Name: \FU:ME_CHECK_T160M\SE:BEGIN\EI
ENHANCEMENT 0 ZMM_MATERIAL_GROUP_CHECK.


 FIELD-SYMBOLS : <fs_ekpo>  TYPE any,
                 <fs_ebeln> TYPE ebeln.
 DATA ls_ekpo TYPE ekpo.
 DATA lv_tvarvclow TYPE tvarvc-low.
DATA: lt_abap_stack TYPE  abap_callstack,
      lt_sys_stack  TYPE  sys_callst.

CALL FUNCTION 'SYSTEM_CALLSTACK'
   IMPORTING
*     callstack          = lt_abap_stack
     et_callstack       = lt_sys_stack.
*
 IF ( sy-tcode = 'ME21N' OR sy-tcode = 'ME22N' OR sy-tcode = 'ME23N' OR sy-tcode = 'ME59N'
   OR ( sy-cprog = 'RM06BB30' AND sy-batch IS NOT INITIAL )
   OR line_exists( lt_sys_stack[ progname = 'CL_MM_PUR_PO_FLEX_WFL=========CP' eventname = 'IF_SWF_FLEX_IFS_RUN_APPL~RESULT_CALLBACK' ] ) )
   AND sy-msgid = '06' AND sy-msgno = '203'.

*    break gpylla.
   SELECT SINGLE * FROM t160m WHERE msgvs = i_msgvs
                                AND arbgb = i_arbgb
                                AND msgnr = i_msgnr.

   IF NOT sy-subrc IS INITIAL AND
          i_msgvs  NE '00'.                                 "321279
     SELECT SINGLE * FROM t160m WHERE msgvs = '00'
                                  AND arbgb = i_arbgb
                                  AND msgnr = i_msgnr.
   ENDIF.

   IF NOT sy-subrc IS INITIAL AND                           "321279
          i_msgvs  NE space.                                "321279
     SELECT SINGLE * FROM t160m WHERE msgvs = space         "321279
                                  AND arbgb = i_arbgb       "321279
                                  AND msgnr = i_msgnr.      "321279
   ENDIF.                                                   "321279
*- Setzen Exeptions ----------------------------------------------------
   IF NOT sy-subrc IS INITIAL.
     t160m-msgtp = i_msgtp_default.
   ENDIF.


   DELETE zps_cl_mm_utility=>gt_mat_group WHERE matkl IS INITIAL.

*   IF sy-tcode EQ 'ME21N'.
     ASSIGN ('(SAPLMEPO)EKPO') TO   <fs_ekpo>.
     IF <fs_ekpo> IS ASSIGNED.
       ls_ekpo =  <fs_ekpo>.
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
     ENDIF.
*   ENDIF.
*   IF sy-msgid = '06' AND sy-msgno = '203'.

   ASSIGN ('(SAPLMEPO)EKKO-EBELN') TO   <fs_ebeln>.
   IF <fs_ebeln> IS ASSIGNED.
     IF <fs_ebeln> IS NOT INITIAL.
       SELECT ebeln,ebelp,matkl,loekz FROM ekpo INTO TABLE @DATA(lt_ekpo)
         WHERE ebeln = @<fs_ebeln>
           AND loekz = @space.
     ENDIF.
   ENDIF.

   LOOP AT lt_ekpo INTO DATA(ls_ekpo_1).
     READ TABLE zps_cl_mm_utility=>gt_mat_group
        ASSIGNING <lfs_mat_group>
        WITH KEY ebeln = ls_ekpo_1-ebeln
                 ebelp = ls_ekpo_1-ebelp .
     IF sy-subrc IS NOT INITIAL.
       APPEND INITIAL LINE TO zps_cl_mm_utility=>gt_mat_group
        ASSIGNING <lfs_mat_group>.
       <lfs_mat_group>-ebeln = ls_ekpo_1-ebeln.
       <lfs_mat_group>-ebelp = ls_ekpo_1-ebelp.
       <lfs_mat_group>-matkl = ls_ekpo_1-matkl.
       <lfs_mat_group>-loekz = ls_ekpo_1-loekz.
     ENDIF.
   ENDLOOP.

   SELECT * FROM tvarvc INTO TABLE @DATA(lv_tvarvc) "#EC CI_ALL_FIELDS_NEEDED
     WHERE name = 'ZMM_PO_MATERIAL_GROUPS'.
   IF sy-subrc IS INITIAL." AND sy-msgid = '06' AND sy-msgno = '203'.
     DATA(lv_warning) = abap_true.
     LOOP AT zps_cl_mm_utility=>gt_mat_group ASSIGNING <lfs_mat_group> WHERE loekz IS INITIAL.
       READ TABLE lv_tvarvc TRANSPORTING NO FIELDS
         WITH  KEY low = <lfs_mat_group>-matkl.
       IF sy-subrc IS NOT INITIAL.
         lv_warning = space.
         EXIT.
       ENDIF.
     ENDLOOP.
     IF lv_warning IS NOT INITIAL.
       RAISE warning.
     ENDIF.
   ENDIF.
*   ENDIF.

 ENDIF.

*ENDIF.

ENDENHANCEMENT.
