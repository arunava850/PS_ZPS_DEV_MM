class ZCL_PS_UTILITY_TOOLS definition
  public
  final
  create public .

public section.

  class-data GT_VENDOR_IDOC_DATA type ZTT_PS_VENDOR_DETAILS_CREMAS .

  class-methods GET_COMPANY_CODE
    importing
      !IV_OBJECT_NUM type CHAR10
      !IV_BUSINESS_LINE type CHAR1 optional
      !IV_OBJECT_TYPE type CHAR1
    returning
      value(RV_COMPANY) type BUKRS .
  class-methods GET_USERS_FROM_ROLE
    importing
      !IV_ROLE type AGR_NAME
    returning
      value(RT_USER) type MMPUR_T_WFAGENT .
  class-methods SEND_EMAIL
    importing
      !IS_INPUT type ZST_PS_EMAIL_EXCEL_INPUT
    returning
      value(RV_SUBRC) type SYSUBRC .
  class-methods GET_SUBSTITUTES
    importing
      !IT_APPROVERLIST type MMPUR_T_WFAGENT
    returning
      value(RT_APPROVERLIST) type MMPUR_T_WFAGENT .
  class-methods GET_IDOC_STATUS_RECORDS
    importing
      !IV_IDOC_NO type EDI_DOCNUM
    exporting
      !ES_STATUS type ZSPS_IDOC_STATUS_MSSSAGES .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PS_UTILITY_TOOLS IMPLEMENTATION.


  METHOD get_company_code.

    CONSTANTS: c_kokrs TYPE kokrs VALUE 'PSCO'.
    DATA: lv_object_no TYPE char10.

    CLEAR rv_company.

*>> If line of business is passed, then will be concatenated with Object number
    IF iv_business_line IS NOT INITIAL.
      lv_object_no =  iv_object_num && iv_business_line.
    ELSE.
      lv_object_no = iv_object_num.
    ENDIF.

    CASE iv_object_type.
      WHEN 'P'. " When profit center as Input
        lv_object_no =  |{ lv_object_no ALPHA = IN }|.
        SELECT bukrs FROM cepc_bukrs INTO rv_company UP TO 1 ROWS
          WHERE kokrs = c_kokrs
            AND prctr = lv_object_no .
        ENDSELECT.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rv_company.
        ENDIF.
      WHEN 'C'. " When Cost center as Input
        lv_object_no =  |{ lv_object_no ALPHA = IN }|.
        SELECT bukrs FROM csks INTO rv_company UP TO 1 ROWS
          WHERE kokrs = c_kokrs
            AND kostl = lv_object_no.
        ENDSELECT.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rv_company.
        ENDIF.
      WHEN OTHERS.
*>> To be used in the future
    ENDCASE.



  ENDMETHOD.


  METHOD get_idoc_status_records.
    DATA: lv_msg    TYPE bapi_msg,
          lv_status TYPE edidc-status,
          lv_idocno TYPE swotobjid-objkey.
    DATA: lv_idoc_msg TYPE string.
*** Get Idoc status records from EDIDS
    SELECT docnum, countr, status, stapa1, stapa2, stapa3, stapa4, statyp, stamid, stamno
              FROM edids INTO TABLE @DATA(lt_edids) WHERE docnum = @iv_idoc_no.
    IF sy-subrc EQ 0.
      SORT lt_edids.
      DELETE ADJACENT DUPLICATES FROM lt_edids COMPARING ALL FIELDS.
      lv_idocno = iv_idoc_no.
      CALL FUNCTION 'GET_STATUS_FROM_IDOCNR'
        EXPORTING
          idocnr = lv_idocno
        IMPORTING
          status = lv_status.

      DELETE lt_edids WHERE status NE lv_status.
      SORT lt_edids  BY countr ASCENDING.
      LOOP AT lt_edids INTO DATA(ls_edids).
        IF ls_edids-stamid IS INITIAL AND ls_edids-stamno IS INITIAL.
          lv_msg = ls_edids-stapa1.
        ELSE.
          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = ls_edids-stamid
              lang      = 'Sy-langu'
              no        = ls_edids-stamno
              v1        = ls_edids-stapa1
              v2        = ls_edids-stapa2
              v3        = ls_edids-stapa3
              v4        = ls_edids-stapa4
            IMPORTING
              msg       = lv_msg
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
        ENDIF.
        IF lv_idoc_msg IS INITIAL.
          MOVE lv_msg TO lv_idoc_msg.
        ELSE.
          CONCATENATE lv_idoc_msg lv_msg INTO lv_idoc_msg SEPARATED BY ' / '.
        ENDIF.
        CLEAR: lv_msg, ls_edids.
      ENDLOOP.

*** Fill import parameter with idoc status
      es_status-idoc_no = iv_idoc_no.
      es_status-status = lv_status.
      es_status-message = lv_idoc_msg.
*      APPEND VALUE #( idoc_no = iv_idoc_no
*                      countr  = ls_edids-countr
*                      status  = ls_edids-status
*                      type    = ls_edids-statyp
*                      message = lv_idoc_msg ) TO et_status.
    ENDIF.
  ENDMETHOD.


  METHOD get_substitutes.

    DATA: r_uname TYPE RANGE OF syuname.

*// Add substitutes for approvers
    CLEAR: rt_approverlist.
    r_uname = VALUE #( FOR ls_approverlist IN it_approverlist
                            ( sign = 'I' option = 'EQ' low = ls_approverlist ) ).
    IF r_uname[] IS NOT INITIAL.
      SELECT us_name,rep_name FROM hrus_d2 INTO TABLE @DATA(lt_substitute)
        WHERE us_name IN @r_uname[]
          AND begda GE @sy-datum
          AND endda LE @sy-datum.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_substitute INTO DATA(ls_substitute).
          APPEND INITIAL LINE TO rt_approverlist ASSIGNING FIELD-SYMBOL(<lfs_approver>).
          <lfs_approver> = ls_substitute.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_users_from_role.

   SELECT agr_name,bname FROM p_user_role INTO TABLE @DATA(lt_agr_users)
     WHERE agr_name = @iv_role.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_agr_users INTO DATA(ls_agr_users).
        APPEND INITIAL LINE TO rt_user ASSIGNING FIELD-SYMBOL(<lfs_user>).
        <lfs_user> = ls_agr_users-bname.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


METHOD send_email.

*// Variables
  DATA: lv_email           TYPE ad_smtpadr.

*// Constants
  CONSTANTS: c_raw  TYPE so_obj_tp VALUE 'RAW',
             c_xls  TYPE so_obj_tp VALUE 'XLS',
             c_4103 TYPE abap_encod VALUE '4103'.

*// Data to Binary conversion
  TRY.
      IF is_input-iv_processed_data IS NOT INITIAL.
        cl_bcs_convert=>string_to_solix(
          EXPORTING
            iv_string   = is_input-iv_processed_data
            iv_codepage = c_4103  "suitable for MS Excel, leave empty
            iv_add_bom  = abap_true "for other doc types
          IMPORTING
            et_solix  = DATA(lt_binary_content)
            ev_size   = DATA(lv_size) ).
      ENDIF.

      IF is_input-iv_error_data IS NOT INITIAL.
        cl_bcs_convert=>string_to_solix(
          EXPORTING
            iv_string   = is_input-iv_error_data
            iv_codepage = c_4103  "suitable for MS Excel, leave empty
            iv_add_bom  = abap_true "for other doc types
          IMPORTING
            et_solix  = DATA(lt_binary_content1)
            ev_size   = DATA(lv_size1) ).
      ENDIF.
    CATCH cx_bcs.
      MESSAGE e445(so) INTO DATA(lv_err_text).
      rv_subrc = 1.
  ENDTRY.
  TRY.

*     -------- create persistent send request ------------------------
      DATA(lo_send_request) = cl_bcs=>create_persistent( ).

*     -------- create and set lo_document with attachment ---------------
*     create lo_document object from internal table with text
      DATA(lo_document) = cl_document_bcs=>create_document(
        i_type    = c_raw
        i_text    = is_input-mail_body_text
        i_subject = is_input-mail_subject ).                "#EC NOTEXT

*     add the spread sheet as attachment to lo_document object
      IF lt_binary_content[] IS NOT INITIAL.
        lo_document->add_attachment(
          i_attachment_type    = c_xls                      "#EC NOTEXT
          i_attachment_subject = is_input-iv_proc_file_name "#EC NOTEXT
          i_attachment_size    = lv_size
          i_att_content_hex    = lt_binary_content ).
      ENDIF.

*   add the spread sheet as attachment to lo_document object
      IF lt_binary_content1[] IS NOT INITIAL.
        lo_document->add_attachment(
          i_attachment_type    = c_xls                      "#EC NOTEXT
          i_attachment_subject = is_input-iv_error_file_name "#EC NOTEXT
          i_attachment_size    = lv_size1
          i_att_content_hex    = lt_binary_content1 ).
      ENDIF.

*     add lo_document object to send request
      lo_send_request->set_document( lo_document ).

*// TO List
      IF is_input-mail_to_tvarvc IS NOT INITIAL.
        SELECT name,low FROM tvarvc INTO TABLE @DATA(lt_tvarvc_to)
            WHERE name = @is_input-mail_to_tvarvc.
        IF sy-subrc IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_tvarvc_to
            ASSIGNING FIELD-SYMBOL(<lfs_tvarvc>).
          <lfs_tvarvc>-low = is_input-mail_to_tvarvc.
        ENDIF.
      ENDIF.

      LOOP AT lt_tvarvc_to ASSIGNING <lfs_tvarvc>.
        lv_email = <lfs_tvarvc>-low.
*     create lo_recipient object
        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( lv_email ).
*     add lo_recipient object to send request
        lo_send_request->add_recipient( lo_recipient ).
      ENDLOOP.

*// CC List
      IF is_input-mail_cc_tvarvc IS NOT INITIAL.
        SELECT name,low FROM tvarvc INTO TABLE @DATA(lt_tvarvc_cc)
            WHERE name = @is_input-mail_cc_tvarvc.
        IF sy-subrc IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_tvarvc_cc
            ASSIGNING <lfs_tvarvc>.
          <lfs_tvarvc>-low = is_input-mail_cc_tvarvc.
        ENDIF.
      ENDIF.

      LOOP AT lt_tvarvc_cc ASSIGNING <lfs_tvarvc>.
        lv_email = <lfs_tvarvc>-low.
*     create lo_recipient object
        lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).
*     add lo_recipient object to send request
        lo_send_request->add_recipient( i_recipient = lo_recipient
                                        i_copy  = abap_true ).
      ENDLOOP.
      lo_send_request->set_send_immediately( i_send_immediately = abap_true ).
*     ---------- send lo_document ---------------------------------------
      DATA(lv_sent_to_all) = lo_send_request->send( i_with_error_screen = abap_true ).

      COMMIT WORK AND WAIT.
      IF lv_sent_to_all IS INITIAL.
        MESSAGE i500(sbcoms) WITH lv_email.
      ELSE.
        MESSAGE s022(so).
      ENDIF.
*   ------------ exception handling ----------------------------------
*   replace this rudimentary exception handling with your own one !!!
    CATCH cx_bcs INTO DATA(lo_bcs_exception).
      MESSAGE i865(so) WITH lo_bcs_exception->error_type.
      rv_subrc = 1.
  ENDTRY.

ENDMETHOD.
ENDCLASS.
