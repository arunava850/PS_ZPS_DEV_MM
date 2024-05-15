*&---------------------------------------------------------------------*
*&  Include           FM06PE04
*&---------------------------------------------------------------------*
FORM adobe_print_output USING  VALUE(if_druvo)   TYPE druvo
                               VALUE(if_preview) TYPE c
                               VALUE(xfz)       TYPE c
                        CHANGING     ef_retco    LIKE sy-subrc.

  DATA: cl_output_po           TYPE REF TO cl_purchase_order_output,
        cl_output_knt          TYPE REF TO cl_purchase_contract_output,
        cl_output_schedagr     TYPE REF TO cl_purchase_sched_agr_output,
        cl_output_rfq          TYPE REF TO cl_purchase_rfq_output,    " object for rfq
        cl_output_schedagr_rel TYPE REF TO cl_purchase_sched_rel_output, "object for agree with release
        fp_docparams           TYPE sfpdocparams,
        fp_outputparams        TYPE sfpoutputparams,
        fp_formoutput          TYPE fpformoutput,
        fp_interfacetype       TYPE fpinterfacetype,
        lf_formname            TYPE fpname,
        l_errstr               TYPE string,
        l_ebeln                TYPE ebeln,
        ls_ekko                TYPE ekko,
        w_cx_root              TYPE REF TO cx_root,
        mesg                   TYPE string,
        ls_function            TYPE rs38l_fnam,
        ls_options             TYPE ssfcompop,
        ls_control             TYPE ssfctrlop.
* Setting for sending document vai E-mail.
  DATA:  os_formout TYPE fpformoutput.
* Setting for sending FAX
  DATA : lv_cam_address      TYPE REF TO cl_cam_address_bcs,
         lv_outputparams_fax TYPE sfpoutpar,
         lv_vend_cntry       TYPE lfa1-land1.

* BCS data
  DATA:
    send_request   TYPE REF TO cl_bcs,
    document       TYPE REF TO cl_document_bcs,
    recipient      TYPE REF TO if_recipient_bcs,
    bcs_exception  TYPE REF TO cx_bcs,
    lv_sent_to_all TYPE os_boolean,
    lp_pdf_size    TYPE so_obj_len,
    lv_subject     TYPE so_obj_des,
    lv_add_nr      TYPE adr6-addrnumber,
    lv_inupd       TYPE i.
* Archiving specific data declaration
  DATA: lv_pdf_size      TYPE i,
        lv_archiveformat LIKE toadd-doc_type,  "PDF or OTF
        lv_documentclass LIKE toadv-doc_type.

*External Send
  DATA: lvs_comm_type   TYPE ad_comm,
        lvs_comm_values TYPE szadr_comm_values,
        lvs_recipient   TYPE swotobjid,
        lvs_sender      TYPE swotobjid,
        intnast         TYPE snast,
        xdevice(10),
        xprogramm       TYPE tdprogram,
        xdialog.

*eMail enhancements
  DATA: lo_badi_mm_po_oc_email TYPE REF TO badi_mm_po_oc_email,
        lt_to_recipients       TYPE if_badi_mm_po_oc_email=>t_recipients,
        lt_cc_recipients       TYPE if_badi_mm_po_oc_email=>t_recipients,
        lt_bcc_recipients      TYPE if_badi_mm_po_oc_email=>t_recipients,
        lt_body                TYPE bcsy_text,
        lo_sender              TYPE REF TO if_sender_bcs,
        lv_sender_user         TYPE syst_uname,
        lv_attachment_subject  TYPE so_obj_des.

  DATA: e_result    TYPE sfpjoboutput,                      "2805618
        doc_no      TYPE so_obj_no,                         "2805618
        spoolid(10).                                        "2805618

  DATA lv_output_control_type TYPE char_01.                 "3108180

* Check if the subroutine is called in update task.
  CALL METHOD cl_system_transaction_state=>get_in_update_task
    RECEIVING
      in_update_task = lv_inupd.
*Begin of changes by GPYLLA for PO or Rel.Order form determination
  MOVE nast-objky(10) TO l_ebeln.

  SELECT SINGLE * FROM ekko INTO ls_ekko "#EC CI_ALL_FIELDS_NEEDED
  WHERE ebeln EQ l_ebeln.
  IF sy-subrc IS INITIAL.
    DATA: ls_tvarvc TYPE tvarvc.
    SELECT SINGLE * FROM tvarvc INTO ls_tvarvc "#EC CI_ALL_FIELDS_NEEDED
      WHERE name = 'ZMM_PO_DOC_TYPE_FORMS'
        AND low  = ls_ekko-bsart.
    IF sy-subrc IS INITIAL .
      tnapr-sform = ls_tvarvc-high.
*Begin of changes by GPYLLA for PO Communication via Email
        IF tnapr-sform IS NOT INITIAL.
          DATA(gv_po_enh004active) = abap_true.
          EXPORT gv_po_enh004active TO MEMORY ID 'ENH0004'.
        ENDIF.
*End of changes by GPYLLA for PO Communication via Email
    ENDIF.
*      IF ls_ekko-bsart = 'UB'.
*        tnapr-sform = 'ZMM_MEDRUCK_REL_ORDER_FORM'.
*      ELSEIF ls_ekko-bsart = 'NB'.
*        tnapr-sform = 'ZMM_MEDRUCK_PO_FORM'.
*      ENDIF.
  ENDIF.
*End of changes by GPYLLA for PO or Rel.Order form determination


  IF nast-kappl = 'EF'.
* Purchase order

    CREATE OBJECT cl_output_po
      TYPE
      cl_purchase_order_output
      EXPORTING
        c_mode     = if_druvo
        es_nast    = nast
        iv_preview = if_preview.

    CALL METHOD cl_output_po->read.
* If the medium is external send this address no is used  for fetching email-id
    lv_add_nr = cl_output_po->is_ekko-adrnr.
* if the medium is FAX then we need vendor Country details
    lv_vend_cntry = cl_output_po->is_ekko-land_vend.

    CALL METHOD cl_mmpur_simpl_suite_switch=>output_control         "^3108180
      EXPORTING
        iv_bstyp   = cl_output_po->is_ekko-bstyp
        iv_ebeln   = cl_output_po->is_ekko-ebeln
      RECEIVING
        rv_oc_type = lv_output_control_type.                          "v3108180

  ELSEIF nast-kappl = 'EL'.
*Sceduling Agreement with release

    CREATE OBJECT cl_output_schedagr_rel
      TYPE
      cl_purchase_sched_rel_output
      EXPORTING
        c_mode  = if_druvo
        xfz     = xfz
        es_nast = nast.

    CALL METHOD cl_output_schedagr_rel->read.
* If the medium is external send this address no is used  for fetching email-id
    lv_add_nr = cl_output_schedagr_rel->is_ekko-adrnr.
* if the medium is FAX then we need vendor Country details
    lv_vend_cntry = cl_output_schedagr_rel->is_ekko-land_vend.

    CALL METHOD cl_mmpur_simpl_suite_switch=>output_control         "^3108180
      EXPORTING
        iv_bstyp   = cl_output_schedagr_rel->is_ekko-bstyp
        iv_ebeln   = cl_output_schedagr_rel->is_ekko-ebeln
      RECEIVING
        rv_oc_type = lv_output_control_type.                          "v3108180

  ELSE.
    MOVE nast-objky(10) TO l_ebeln.

    SELECT SINGLE * FROM ekko INTO ls_ekko "#EC CI_ALL_FIELDS_NEEDED
    WHERE ebeln EQ l_ebeln.

    CALL METHOD cl_mmpur_simpl_suite_switch=>output_control         "^3108180
      EXPORTING
        iv_bstyp   = ls_ekko-bstyp
        iv_ebeln   = ls_ekko-ebeln
      RECEIVING
        rv_oc_type = lv_output_control_type.                          "v3108180

    IF ls_ekko-bstyp = 'K'.
* Contract
      CREATE OBJECT cl_output_knt
        TYPE
        cl_purchase_contract_output
        EXPORTING
          c_mode  = if_druvo
          es_nast = nast.

      CALL METHOD cl_output_knt->read.
* If the medium is external send this address no is used  for fetching email-id
      lv_add_nr = cl_output_knt->is_ekko-adrnr.
* if the medium is FAX then we need vendor Country details
      lv_vend_cntry = cl_output_knt->is_ekko-land_vend.

    ELSEIF ls_ekko-bstyp = 'A'.
*RFQ
      CREATE OBJECT cl_output_rfq
        TYPE
        cl_purchase_rfq_output
        EXPORTING
          c_mode  = if_druvo
          es_nast = nast.

      CALL METHOD cl_output_rfq->read.
* If the medium is external send this address no is used  for fetching email-id
      lv_add_nr = cl_output_rfq->is_ekko-adrnr.
* if the medium is FAX then we need vendor Country details
      lv_vend_cntry = cl_output_rfq->is_ekko-land_vend.

    ELSE.
* Scheduling Agreement
      CREATE OBJECT cl_output_schedagr
        TYPE
        cl_purchase_sched_agr_output
        EXPORTING
          c_mode  = if_druvo
          es_nast = nast.

      CALL METHOD cl_output_schedagr->read.
* If the medium is external send this address no is used  for fetching email-id
      lv_add_nr = cl_output_schedagr->is_ekko-adrnr.
* if the medium is FAX then we need vendor Country details
      lv_vend_cntry = cl_output_schedagr->is_ekko-land_vend.

    ENDIF.
  ENDIF.

* Check for external send
  IF nast-nacha EQ 5.
*   ... use stratagy to get communication type
    CALL FUNCTION 'ADDR_GET_NEXT_COMM_TYPE'
      EXPORTING
        strategy           = nast-tcode
        address_number     = lv_add_nr
      IMPORTING
        comm_type          = lvs_comm_type
        comm_values        = lvs_comm_values
      EXCEPTIONS
        address_not_exist  = 1
        person_not_exist   = 2
        no_comm_type_found = 3
        internal_error     = 4
        parameter_error    = 5
        OTHERS             = 6.

* convert communication data
    MOVE-CORRESPONDING nast TO intnast.
    MOVE sy-repid           TO xprogramm.
    CALL FUNCTION 'CONVERT_COMM_TYPE_DATA'
      EXPORTING
        pi_comm_type              = lvs_comm_type
        pi_comm_values            = lvs_comm_values
        pi_country                = lv_vend_cntry
        pi_repid                  = xprogramm
        pi_snast                  = intnast
      IMPORTING
        pe_itcpo                  = itcpo
        pe_device                 = fp_outputparams-device
        pe_mail_recipient         = lvs_recipient
        pe_mail_sender            = lvs_sender
      EXCEPTIONS
        comm_type_not_supported   = 1
        recipient_creation_failed = 2
        sender_creation_failed    = 3
        OTHERS                    = 4.
    IF sy-subrc <> 0.   " Note 1477015
      IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
        CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
          EXPORTING
            msg_arbgb = sy-msgid
            msg_nr    = sy-msgno
            msg_ty    = sy-msgty
            msg_v1    = sy-msgv1
            msg_v2    = sy-msgv2
            msg_v3    = sy-msgv3
            msg_v4    = sy-msgv4
          EXCEPTIONS
            OTHERS    = 1.
      ENDIF.                                                "v3108180
      ef_retco = 1.
      EXIT.
    ENDIF.

    IF fp_outputparams-device = 'MAIL'.
      CALL FUNCTION 'SX_ADDRESS_TO_DEVTYPE'
        EXPORTING
          recipient_id      = lvs_recipient
          sender_id         = lvs_sender
        EXCEPTIONS
          err_invalid_route = 1
          err_system        = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
        IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
          CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
            EXPORTING
              msg_arbgb = sy-msgid
              msg_nr    = sy-msgno
              msg_ty    = sy-msgty
              msg_v1    = sy-msgv1
              msg_v2    = sy-msgv2
              msg_v3    = sy-msgv3
              msg_v4    = sy-msgv4
            EXCEPTIONS
              OTHERS    = 1.
        ENDIF.                                              "v3108180
        ef_retco = 1.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  ls_function = tnapr-funcname.

  IF NOT tnapr-sform IS INITIAL.
    lf_formname = tnapr-sform.
    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name           = lf_formname
          IMPORTING
            e_funcname       = ls_function
            e_interface_type = fp_interfacetype.

      CATCH cx_root INTO w_cx_root.
        IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
          mesg = w_cx_root->get_text( ).
          MESSAGE e000 WITH mesg INTO mesg.                 "*2287056
          CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
            EXPORTING
              msg_arbgb = 'ME'
              msg_nr    = syst-msgno
              msg_ty    = syst-msgty
              msg_v1    = syst-msgv1
              msg_v2    = syst-msgv2
              msg_v3    = syst-msgv3
              msg_v4    = syst-msgv4
            EXCEPTIONS
              OTHERS    = 1.
        ENDIF.                                              "v3108180
        ef_retco = 1.
        EXIT.
    ENDTRY.
  ELSE.
    IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
      CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
        EXPORTING
          msg_arbgb = 'VN'
          msg_nr    = '027'
          msg_ty    = 'E'
          msg_v1    = syst-msgv1
          msg_v2    = syst-msgv2
          msg_v3    = syst-msgv3
          msg_v4    = syst-msgv4
        EXCEPTIONS
          OTHERS    = 1.
    ENDIF.                                                  "v3108180
    ef_retco = 1.
    EXIT.                                                   "v2287056
  ENDIF.
  PERFORM fill_control_structure USING    nast
                                          if_preview
                                 CHANGING fp_outputparams.

* Do FP_JOB_OPEN only if RET_CODE is 0.
  IF nast-kappl = 'EF'.
    ef_retco = cl_output_po->ret_code.
    IF ef_retco IS NOT INITIAL.                             " 1468512

      syst-msgv1 = ls_ekko-ebeln.
      syst-msgno = '140'.
    ENDIF.

  ELSEIF nast-kappl = 'EL'.
    ef_retco = cl_output_schedagr_rel->ret_code.
    IF ef_retco IS NOT INITIAL.                             " 1468512

      syst-msgv1 = ls_ekko-ebeln.
      syst-msgno = '140'.
    ENDIF.

  ELSE.

    IF ls_ekko-bstyp = 'K'.
      ef_retco = cl_output_knt->ret_code.
      IF ef_retco IS NOT INITIAL.                           " 1468512

        syst-msgv1 = ls_ekko-ebeln.
        syst-msgno = '140'.
      ENDIF.

    ELSEIF ls_ekko-bstyp = 'A'.
      ef_retco = cl_output_rfq->ret_code.
      IF ef_retco IS NOT INITIAL.                           " 1468512

        syst-msgv1 = ls_ekko-ebeln.
        syst-msgno = '140'.
      ENDIF.

    ELSE .
      ef_retco = cl_output_schedagr->ret_code.
      IF ef_retco IS NOT INITIAL.                           " 1468512

        syst-msgv1 = ls_ekko-ebeln.
        syst-msgno = '140'.
      ENDIF.

    ENDIF.
  ENDIF.

  IF ef_retco IS NOT INITIAL.
    IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
      CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
        EXPORTING
          msg_arbgb = 'ME'
          msg_nr    = syst-msgno
          msg_ty    = 'W'
          msg_v1    = syst-msgv1
          msg_v2    = syst-msgv2
          msg_v3    = syst-msgv3
          msg_v4    = syst-msgv4
        EXCEPTIONS
          OTHERS    = 1.
    ENDIF.                                                  "v3108180
  ENDIF.


  CHECK ef_retco EQ 0.

* sending Po via Mail or archiving the PDF output
  IF if_preview IS INITIAL "In case of preview message should be displayed only
    AND ( nast-nacha EQ 5 OR nast-tdarmod = 2 OR  nast-nacha EQ 2 ).
* Setting output parameters only if communication type is fax or email.
    IF nast-nacha EQ 5.
      IF ( lvs_comm_type EQ 'FAX' OR lvs_comm_type EQ 'INT' ).
        fp_outputparams-getpdf = abap_true.
        IF itcpo-tdtelenum EQ space.
          fp_outputparams-nodialog = ' '.
        ENDIF.
      ENDIF.
    ELSE.
      fp_outputparams-getpdf = abap_true.
    ENDIF.
* Specific setting for FAX
    IF nast-nacha EQ 2.
* Setting output parameters
      fp_outputparams-device = 'TELEFAX'.
      IF nast-telfx EQ space.
        fp_outputparams-nodialog = ' '.
      ENDIF.
    ENDIF.
  ENDIF.

  IF if_preview = 'W'.        "web dynpro output
* Setting output parameters
    fp_outputparams-getpdf = 'X'.
    fp_outputparams-device = ''.
    fp_outputparams-noprint = ''.                           "1704638
  ENDIF.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = fp_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
      CALL FUNCTION 'NAST_PROTOCOL_UPDATE'            "^2287056
        EXPORTING
          msg_arbgb = syst-msgid                      "2410565
          msg_nr    = syst-msgno
          msg_ty    = syst-msgty
          msg_v1    = syst-msgv1
          msg_v2    = syst-msgv2
          msg_v3    = syst-msgv3
          msg_v4    = syst-msgv4
        EXCEPTIONS
          OTHERS    = 1.
    ENDIF.                                                  "v3108180
    ef_retco = 1.
    EXIT.                                                   "v2287056
  ENDIF.
* To handle print and archive scenario
  IF nast-tdarmod EQ 3.
    fp_outputparams-getpdf = abap_true.
  ENDIF.

  CLEAR: fp_docparams.
  fp_docparams-country = nast-tland.
  IF nast-kappl = 'EF'.

    fp_docparams-langu = cl_output_po->is_ekko-spras.       "2973015
    CALL FUNCTION ls_function
      EXPORTING
        /1bcdwb/docparams  = fp_docparams
        mode               = cl_output_po->mode
        header             = cl_output_po->is_ekko
        addressinfo        = cl_output_po->is_t024e
        headerinfo         = cl_output_po->is_t166u
        contactinfo        = cl_output_po->is_t024
        headertext         = cl_output_po->it_t166k
        paymentterms       = cl_output_po->it_payment
        item               = cl_output_po->it_ekpo
        itemline           = cl_output_po->it_item
        schedule           = cl_output_po->it_eket
        conditions         = cl_output_po->it_komvd
        manfdetails        = cl_output_po->it_htnmat
        changetext         = cl_output_po->it_t166t
        account            = cl_output_po->it_ekkn
        qmtext             = cl_output_po->it_qmtext
        texts              = cl_output_po->it_t166p
        documents          = cl_output_po->it_drad
        komk               = cl_output_po->is_komk
        componentshdr      = cl_output_po->it_complist_hdr
        components         = cl_output_po->it_complist
        invoice            = cl_output_po->it_fpltdr
        invoiceper         = cl_output_po->it_fpltdr_per
        invoiceval         = cl_output_po->it_fpltdr_val
        appendixtext       = cl_output_po->it_t166a
        shipinstr          = cl_output_po->it_t027b
        variants           = cl_output_po->it_pekpov
        configmat          = cl_output_po->it_econf_out
        services           = cl_output_po->it_ml_esll
        srvlines           = cl_output_po->it_srvlines
        srvtyp             = cl_output_po->it_srvtyp
        srvhdr             = cl_output_po->it_srvhdr
        srvtime            = cl_output_po->it_time
        srvvalue           = cl_output_po->it_esuh
        formulahdr         = cl_output_po->it_formel
        formulabody        = cl_output_po->it_variablen
        srvconditions      = cl_output_po->it_komvd_srv
        srvtext            = cl_output_po->it_t166p_srv
        chngtxtsrv         = cl_output_po->it_t166t_srv
        mainthdr           = cl_output_po->it_mmpt
        maintpos           = cl_output_po->it_mpos
        serialnumber       = cl_output_po->it_objk
        conditions_hdr     = cl_output_po->it_komvd_hdr
      IMPORTING
        /1bcdwb/formoutput = os_formout
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
        CALL FUNCTION 'NAST_PROTOCOL_UPDATE'            "^2287056
          EXPORTING
            msg_arbgb = syst-msgid                      "2410565
            msg_nr    = syst-msgno
            msg_ty    = syst-msgty
            msg_v1    = syst-msgv1
            msg_v2    = syst-msgv2
            msg_v3    = syst-msgv3
            msg_v4    = syst-msgv4
          EXCEPTIONS
            OTHERS    = 1.
      ENDIF.                                                "v3108180
      ef_retco = 1.
      EXIT.                                                 "v2287056
    ENDIF.

  ELSEIF nast-kappl = 'EL'.

    fp_docparams-langu = cl_output_schedagr_rel->is_ekko-spras. "2973015
    CALL FUNCTION ls_function
      EXPORTING
        /1bcdwb/docparams  = fp_docparams
        mode               = cl_output_schedagr_rel->mode
        header             = cl_output_schedagr_rel->is_ekko
        addressinfo        = cl_output_schedagr_rel->is_t024e
        headerinfo         = cl_output_schedagr_rel->is_t166u
        contactinfo        = cl_output_schedagr_rel->is_t024
        headertext         = cl_output_schedagr_rel->it_t166k
        item               = cl_output_schedagr_rel->it_ekpo
        schedule           = cl_output_schedagr_rel->it_eket
        ekek               = cl_output_schedagr_rel->it_ekek
        changetext         = cl_output_schedagr_rel->it_t166t
        shipinstr          = cl_output_schedagr_rel->it_t027b
        componentshdr      = cl_output_schedagr_rel->it_complist_hdr
        components         = cl_output_schedagr_rel->it_complist
        xfz                = cl_output_schedagr_rel->lv_xfz
      IMPORTING
        /1bcdwb/formoutput = os_formout
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
        CALL FUNCTION 'NAST_PROTOCOL_UPDATE'            "^2287056
          EXPORTING
            msg_arbgb = syst-msgid                      "2410565
            msg_nr    = syst-msgno
            msg_ty    = syst-msgty
            msg_v1    = syst-msgv1
            msg_v2    = syst-msgv2
            msg_v3    = syst-msgv3
            msg_v4    = syst-msgv4
          EXCEPTIONS
            OTHERS    = 1.
      ENDIF.                                                "v3108180
      ef_retco = 1.
      EXIT.                                                 "v2287056
    ENDIF.

  ELSE.
    IF ls_ekko-bstyp = 'K'.

      fp_docparams-langu = cl_output_knt->is_ekko-spras.    "2973015
      CALL FUNCTION ls_function
        EXPORTING
          /1bcdwb/docparams  = fp_docparams
          mode               = cl_output_knt->mode
          header             = cl_output_knt->is_ekko
          addressinfo        = cl_output_knt->is_t024e
          headerinfo         = cl_output_knt->is_t166u
          contactinfo        = cl_output_knt->is_t024
          headertext         = cl_output_knt->it_t166k
          paymentterms       = cl_output_knt->it_payment
          item               = cl_output_knt->it_ekpo
          itemline           = cl_output_knt->it_item
          schedule           = cl_output_knt->it_eket
          contconditions     = cl_output_knt->it_ekomd
          quanconditions     = cl_output_knt->it_ekomd_quant
          valueconditions    = cl_output_knt->it_ekomd_value
          lineconditions     = cl_output_knt->it_ekomd_cond
          manfdetails        = cl_output_knt->it_htnmat
          changetext         = cl_output_knt->it_t166t
          account            = cl_output_knt->it_ekkn
          qmtext             = cl_output_knt->it_qmtext
          texts              = cl_output_knt->it_t166p
          documents          = cl_output_knt->it_drad
          komk               = cl_output_knt->is_komk
          invoice            = cl_output_knt->it_fpltdr
          invoiceper         = cl_output_knt->it_fpltdr_per
          invoiceval         = cl_output_knt->it_fpltdr_val
          appendixtext       = cl_output_knt->it_t166a
          shipinstr          = cl_output_knt->it_t027b
          variants           = cl_output_knt->it_pekpov
          configmat          = cl_output_knt->it_econf_out
          services           = cl_output_knt->it_ml_esll
          srvlines           = cl_output_knt->it_srvlines
          srvtyp             = cl_output_knt->it_srvtyp
          srvhdr             = cl_output_knt->it_srvhdr
          srvtime            = cl_output_knt->it_time
          srvvalue           = cl_output_knt->it_esuh
          formulahdr         = cl_output_knt->it_formel
          formulabody        = cl_output_knt->it_variablen
          srvconditions      = cl_output_knt->it_komvd_srv
          srvtext            = cl_output_knt->it_t166p_srv
          chngtxtsrv         = cl_output_knt->it_t166t_srv
          mainthdr           = cl_output_knt->it_mmpt
          maintpos           = cl_output_knt->it_mpos
        IMPORTING
          /1bcdwb/formoutput = os_formout
        EXCEPTIONS
          usage_error        = 1
          system_error       = 2
          internal_error     = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
        IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
          CALL FUNCTION 'NAST_PROTOCOL_UPDATE'            "^2287056
            EXPORTING
              msg_arbgb = syst-msgid                      "2410565
              msg_nr    = syst-msgno
              msg_ty    = syst-msgty
              msg_v1    = syst-msgv1
              msg_v2    = syst-msgv2
              msg_v3    = syst-msgv3
              msg_v4    = syst-msgv4
            EXCEPTIONS
              OTHERS    = 1.
        ENDIF.                                              "v3108180
        ef_retco = 1.
        EXIT.                                               "v2287056
      ENDIF.

    ELSEIF ls_ekko-bstyp = 'A'.                    "rfq

      fp_docparams-langu = cl_output_rfq->is_ekko-spras.    "2973015
      CALL FUNCTION ls_function
        EXPORTING
          /1bcdwb/docparams  = fp_docparams
          mode               = cl_output_rfq->mode
          header             = cl_output_rfq->is_ekko
          addressinfo        = cl_output_rfq->is_t024e
          headerinfo         = cl_output_rfq->is_t166u
          contactinfo        = cl_output_rfq->is_t024
          headertext         = cl_output_rfq->it_t166k
          paymentterms       = cl_output_rfq->it_payment
          item               = cl_output_rfq->it_ekpo
          itemline           = cl_output_rfq->it_item
          schedule           = cl_output_rfq->it_eket
          manfdetails        = cl_output_rfq->it_htnmat
          changetext         = cl_output_rfq->it_t166t
          qmtext             = cl_output_rfq->it_qmtext
          texts              = cl_output_rfq->it_t166p
          documents          = cl_output_rfq->it_drad
          komk               = cl_output_rfq->is_komk
          invoice            = cl_output_rfq->it_fpltdr
          invoiceper         = cl_output_rfq->it_fpltdr_per
          invoiceval         = cl_output_rfq->it_fpltdr_val
          appendixtext       = cl_output_rfq->it_t166a
          shipinstr          = cl_output_rfq->it_t027b
          variants           = cl_output_rfq->it_pekpov
          configmat          = cl_output_rfq->it_econf_out
          services           = cl_output_rfq->it_ml_esll
          srvlines           = cl_output_rfq->it_srvlines
          srvtyp             = cl_output_rfq->it_srvtyp
          srvhdr             = cl_output_rfq->it_srvhdr
          srvtime            = cl_output_rfq->it_time
          srvvalue           = cl_output_rfq->it_esuh
          formulahdr         = cl_output_rfq->it_formel
          formulabody        = cl_output_rfq->it_variablen
          srvtext            = cl_output_rfq->it_t166p_srv
          chngtxtsrv         = cl_output_rfq->it_t166t_srv
          mainthdr           = cl_output_rfq->it_mmpt
          maintpos           = cl_output_rfq->it_mpos
        IMPORTING
          /1bcdwb/formoutput = os_formout
        EXCEPTIONS
          usage_error        = 1
          system_error       = 2
          internal_error     = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
        IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
          CALL FUNCTION 'NAST_PROTOCOL_UPDATE'            "^2287056
            EXPORTING
              msg_arbgb = syst-msgid                      "2410565
              msg_nr    = syst-msgno
              msg_ty    = syst-msgty
              msg_v1    = syst-msgv1
              msg_v2    = syst-msgv2
              msg_v3    = syst-msgv3
              msg_v4    = syst-msgv4
            EXCEPTIONS
              OTHERS    = 1.
        ENDIF.                                              "v3108180
        ef_retco = 1.
        EXIT.                                               "v2287056
      ENDIF.

    ELSE.

      fp_docparams-langu = cl_output_schedagr->is_ekko-spras. "2973015
      CALL FUNCTION ls_function
        EXPORTING
          /1bcdwb/docparams  = fp_docparams
          mode               = cl_output_schedagr->mode
          header             = cl_output_schedagr->is_ekko
          addressinfo        = cl_output_schedagr->is_t024e
          headerinfo         = cl_output_schedagr->is_t166u
          contactinfo        = cl_output_schedagr->is_t024
          headertext         = cl_output_schedagr->it_t166k
          paymentterms       = cl_output_schedagr->it_payment
          item               = cl_output_schedagr->it_ekpo
          itemline           = cl_output_schedagr->it_item
          schedule           = cl_output_schedagr->it_eket
          conditions         = cl_output_schedagr->it_komvd
          contconditions     = cl_output_schedagr->it_ekomd
          quanconditions     = cl_output_schedagr->it_ekomd_quant
          valueconditions    = cl_output_schedagr->it_ekomd_value
          lineconditions     = cl_output_schedagr->it_ekomd_cond
          manfdetails        = cl_output_schedagr->it_htnmat
          changetext         = cl_output_schedagr->it_t166t
          account            = cl_output_schedagr->it_ekkn
          qmtext             = cl_output_schedagr->it_qmtext
          texts              = cl_output_schedagr->it_t166p
          documents          = cl_output_schedagr->it_drad
          komk               = cl_output_schedagr->is_komk
          invoice            = cl_output_schedagr->it_fpltdr
          invoiceper         = cl_output_schedagr->it_fpltdr_per
          invoiceval         = cl_output_schedagr->it_fpltdr_val
          appendixtext       = cl_output_schedagr->it_t166a
          shipinstr          = cl_output_schedagr->it_t027b
          variants           = cl_output_schedagr->it_pekpov
          configmat          = cl_output_schedagr->it_econf_out
          services           = cl_output_schedagr->it_ml_esll
          srvlines           = cl_output_schedagr->it_srvlines
          srvtyp             = cl_output_schedagr->it_srvtyp
          srvhdr             = cl_output_schedagr->it_srvhdr
          srvtime            = cl_output_schedagr->it_time
          srvvalue           = cl_output_schedagr->it_esuh
          formulahdr         = cl_output_schedagr->it_formel
          formulabody        = cl_output_schedagr->it_variablen
          srvconditions      = cl_output_schedagr->it_komvd_srv
          srvtext            = cl_output_schedagr->it_t166p_srv
          chngtxtsrv         = cl_output_schedagr->it_t166t_srv
          mainthdr           = cl_output_schedagr->it_mmpt
          maintpos           = cl_output_schedagr->it_mpos
        IMPORTING
          /1bcdwb/formoutput = os_formout
        EXCEPTIONS
          usage_error        = 1
          system_error       = 2
          internal_error     = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
        IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
          CALL FUNCTION 'NAST_PROTOCOL_UPDATE'            "^2287056
            EXPORTING
              msg_arbgb = syst-msgid                      "2410565
              msg_nr    = syst-msgno
              msg_ty    = syst-msgty
              msg_v1    = syst-msgv1
              msg_v2    = syst-msgv2
              msg_v3    = syst-msgv3
              msg_v4    = syst-msgv4
            EXCEPTIONS
              OTHERS    = 1.
        ENDIF.                                              "v3108180
        ef_retco = 1.
        EXIT.                                               "v2287056
      ENDIF.
    ENDIF.
  ENDIF.

*o/p contrl badi enhancement
  TRY.
      GET BADI lo_badi_mm_po_oc_email.

      IF lo_badi_mm_po_oc_email IS BOUND.

      ENDIF.
*Catch not implemented exception or multiple implementation
    CATCH cx_badi_not_implemented.
      CLEAR lo_badi_mm_po_oc_email.
  ENDTRY.

*sending Document out via mail or FAX
  IF if_preview IS INITIAL  "In case of preview message should be displayed only
      AND ( nast-nacha EQ 5 OR nast-nacha EQ 2 ) AND os_formout IS NOT INITIAL
      AND nast-tdarmod NE 2.                                "2243519

* Set FAX specific setting
    IF nast-nacha EQ 5.
      lv_outputparams_fax-telenum = itcpo-tdtelenum.
      lv_outputparams_fax-teleland = itcpo-tdteleland.
    ELSE.
      IF nast-telfx NE space.
        lv_outputparams_fax-telenum  = nast-telfx.
        IF nast-tland IS INITIAL.
          lv_outputparams_fax-teleland = lv_vend_cntry.
        ELSE.
          lv_outputparams_fax-teleland = nast-tland.
        ENDIF.
      ENDIF.
    ENDIF.
    IF lvs_comm_type EQ 'FAX' OR lvs_comm_type EQ 'INT' OR nast-nacha EQ 2.

      lv_subject = lv_attachment_subject = fp_outputparams-covtitle.
* call eMail change Badi
      REFRESH: lt_to_recipients, lt_cc_recipients, lt_bcc_recipients.
      APPEND lvs_comm_values-adsmtp-smtp_addr TO lt_to_recipients.
      lv_sender_user = sy-uname.
      IF lo_badi_mm_po_oc_email IS BOUND.
* change eMail sender

*        CALL BADI lo_badi_mm_po_oc_email->change_sender
*          EXPORTING
*            im_nast_entry  = nast
*          CHANGING
*            ch_sender_name = lv_sender_user.

* change to, cc and bcc list of eMail
        CALL BADI lo_badi_mm_po_oc_email->change_recipients
          EXPORTING
            im_nast_entry          = nast
          CHANGING
            cht_to_recipient_list  = lt_to_recipients
            cht_cc_recipient_list  = lt_cc_recipients
            cht_bcc_recipient_list = lt_bcc_recipients.
* change eMail subject
        CALL BADI lo_badi_mm_po_oc_email->change_subject
          EXPORTING
            im_nast_entry = nast
          CHANGING
            chv_subject   = lv_subject.
* change eMail body
        CALL BADI lo_badi_mm_po_oc_email->change_body
          EXPORTING
            im_nast_entry = nast
          CHANGING
            cht_body      = lt_body.

      ENDIF.
* ------------ Call BCS interface ----------------------------------
      TRY.
*   ---------- create persistent send request ----------------------
          send_request = cl_bcs=>create_persistent( ).

*   ---------- add document ----------------------------------------
*   get PDF xstring and convert it to BCS format
          lp_pdf_size = xstrlen( os_formout-pdf ).

          PERFORM xstring_to_solix
                      USING
                         os_formout-pdf.
* if BAdI is implementet create eMail with body and attach document
          IF lo_badi_mm_po_oc_email IS BOUND.
            CALL METHOD cl_document_bcs=>create_document
              EXPORTING
                i_type    = 'HTM'
                i_subject = lv_subject
*               i_language = lv_language
                i_text    = lt_body
              RECEIVING
                result    = document.
*--- Add the document as an attachment
            CALL METHOD document->add_attachment
              EXPORTING
                i_attachment_type    = 'PDF'
                i_attachment_size    = lp_pdf_size
                i_attachment_subject = lv_attachment_subject
*               i_attachment_language = lv_language
                i_att_content_hex    = pdf_content.

          ELSE.
            lv_subject = fp_outputparams-covtitle.
            document = cl_document_bcs=>create_document(
                  i_type    = 'PDF' " cf. RAW, DOC
                  i_hex     = pdf_content
                  i_length  = lp_pdf_size
                  i_subject = lv_subject ).                 "#EC NOTEXT
          ENDIF.

          CALL METHOD document->get_docno                   "^2805618
            RECEIVING
              result = doc_no.

          IF doc_no IS NOT INITIAL.
            sy-msgv1 = doc_no.
          ENDIF.

          IF nast-nacha EQ 5 OR nast-nacha EQ 2.
            IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
              MESSAGE i095(vn) WITH sy-msgv1 INTO mesg.
              CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
                EXPORTING
                  msg_arbgb = 'VN'
                  msg_nr    = '095'
                  msg_ty    = 'I'
                  msg_v1    = sy-msgv1
                EXCEPTIONS
                  OTHERS    = 1.
            ENDIF.                                          "v3108180
          ENDIF.                                            "v2805618

*   add document to send request

          send_request->set_document( document ).

* Begin of changes by GPYLLA on 09/01/2023
*          BREAK gpylla.
          DATA lv_smtp_address TYPE adr6-smtp_addr.
          DATA lv_smtp_name TYPE adr6-smtp_addr.
          DATA ls_t024 TYPE t024.

*          lo_sender = cl_sapuser_bcs=>create( lv_sender_user ) .
*
*          CALL METHOD send_request->set_sender
*            EXPORTING
*              i_sender = lo_sender.
* Change the Sender as per the User request
          IF gv_po_enh004active IS NOT INITIAL.

            SELECT SINGLE * FROM t024
              INTO ls_t024
              WHERE ekgrp = ls_ekko-ekgrp.

            lv_smtp_address = ls_t024-eknam.
            lv_smtp_name = ls_t024-smtp_addr.
            lo_sender = cl_cam_address_bcs=>create_internet_address(
                           i_address_string =  lv_smtp_name
                           i_address_name = lv_smtp_address ).
            CALL METHOD send_request->set_sender
              EXPORTING
                i_sender = lo_sender.

          ELSE.
* End of changes by GPYLLA on 09/01/2023
*     --------- set sender -------------------------------------------
*     note: this is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.

            lo_sender = cl_sapuser_bcs=>create( sy-uname ).
            CALL METHOD send_request->set_sender
              EXPORTING
                i_sender = lo_sender.
          ENDIF.
*   ---------- add recipient (e-mail address) ----------------------

          CASE nast-nacha.
            WHEN 5.
              IF lvs_comm_type EQ 'INT'.
*           add 'to' recipient (e-mail address)
                IF lo_badi_mm_po_oc_email IS BOUND.
                  LOOP AT lt_to_recipients INTO lvs_comm_values-adsmtp-smtp_addr. "2303437
                    recipient = recipient = cl_cam_address_bcs=>create_internet_address(
                    i_address_string = lvs_comm_values-adsmtp-smtp_addr ).
*   add recipient to send request
                    send_request->add_recipient( i_recipient = recipient ).
                  ENDLOOP.

*           add 'cc' recipient (e-mail address)
                  LOOP AT lt_cc_recipients INTO lvs_comm_values-adsmtp-smtp_addr. "2303437
                    recipient = recipient = cl_cam_address_bcs=>create_internet_address(
                    i_address_string = lvs_comm_values-adsmtp-smtp_addr ).
*   add recipient to send request
                    CALL METHOD send_request->add_recipient
                      EXPORTING
                        i_recipient = recipient
                        i_copy      = abap_true.

                  ENDLOOP.
*           add 'bcc' recipient (e-mail address)
                  LOOP AT lt_bcc_recipients INTO lvs_comm_values-adsmtp-smtp_addr. "2303437
                    recipient = recipient = cl_cam_address_bcs=>create_internet_address(
                    i_address_string = lvs_comm_values-adsmtp-smtp_addr ).
*   add recipient to send request
                    CALL METHOD send_request->add_recipient
                      EXPORTING
                        i_recipient  = recipient
                        i_blind_copy = abap_true.

                  ENDLOOP.
                ELSE.
*   add recipient to send request
                  recipient = recipient = cl_cam_address_bcs=>create_internet_address(
                  i_address_string = lvs_comm_values-adsmtp-smtp_addr ). "2303437
                  send_request->add_recipient( i_recipient = recipient ).
                ENDIF.
                IF NOT nast-forfb IS INITIAL AND NOT nast-prifb IS INITIAL. "1918225
                  CALL METHOD send_request->set_status_attributes
                    EXPORTING
                      i_requested_status = nast-forfb
                      i_status_mail      = nast-prifb.
                ENDIF.
              ELSE.
*           add recipient (fax address)
                recipient = cl_cam_address_bcs=>create_fax_address(
                                 i_country = lv_outputparams_fax-teleland
                                 i_number  = lv_outputparams_fax-telenum ).
              ENDIF.

            WHEN 2.
*           add recipient (fax address)
              recipient = cl_cam_address_bcs=>create_fax_address(
                               i_country = lv_outputparams_fax-teleland
                               i_number  = lv_outputparams_fax-telenum ).
          ENDCASE.

*   add recipient to send request
          send_request->add_recipient( i_recipient = recipient ).

*   ---------- send document ---------------------------------------
          lv_sent_to_all = send_request->send(
              i_with_error_screen = 'X' ).
* Issue message and COMMINT only if the subroutine is not called in update task
          IF lv_inupd = 0.
            IF lv_sent_to_all = 'X'.
              MESSAGE s022(so).                             "1945749
            ENDIF.

*   ---------- explicit 'commit work' is mandatory! ----------------
            COMMIT WORK.
          ENDIF.
* ------------------------------------------------------------------
* *            exception handling
* ------------------------------------------------------------------
        CATCH cx_bcs INTO bcs_exception.
          IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
            MESSAGE e451(so) WITH lv_outputparams_fax-telenum INTO mesg.
*      Sending fax/mail failed
            CALL FUNCTION 'NAST_PROTOCOL_UPDATE'            "^2287056
              EXPORTING
                msg_arbgb = syst-msgid                      "2410565
                msg_nr    = syst-msgno
                msg_ty    = syst-msgty
                msg_v1    = syst-msgv1
                msg_v2    = syst-msgv2
                msg_v3    = syst-msgv3
                msg_v4    = syst-msgv4
              EXCEPTIONS
                OTHERS    = 1.
          ENDIF.                                            "v3108180
          ef_retco = 1.
          EXIT.                                             "v2287056
      ENDTRY.
    ENDIF.
  ENDIF.

* Arching for adobe forms
  IF if_preview IS INITIAL  "In case of preview message should be displayed only
      AND ( nast-tdarmod = 2 OR  nast-tdarmod = 3 ) AND os_formout IS NOT INITIAL. "1604020

* Get the PDF length
    lp_pdf_size = xstrlen( os_formout-pdf ).

* defaults for archive
    IF toa_dara-function = space.
      toa_dara-function = 'DARA'.
    ENDIF.
*     which format to be used for archiving: OTF or PDF?
    CALL FUNCTION 'ARCHIV_GET_PRINTFORMAT'
      EXPORTING
        application = 'PDF'
      IMPORTING
        printformat = lv_archiveformat.

    IF lv_archiveformat EQ 'PDF'.
      lv_documentclass = 'PDF'.

      CALL FUNCTION 'ARCHIV_CREATE_OUTGOINGDOCUMENT'
        EXPORTING
          arc_p                    = arc_params
          arc_i                    = toa_dara
          pdflen                   = lv_pdf_size
          documentclass            = lv_documentclass                "Since the output is in PDF document class is also PDF
          document                 = os_formout-pdf
        EXCEPTIONS
          error_archiv             = 1
          error_communicationtable = 2
          error_connectiontable    = 3
          error_kernel             = 4
          error_parameter          = 5
          OTHERS                   = 6.
      CASE sy-subrc.
        WHEN 0. " o.k.
        WHEN 1. RAISE error_archiv.
        WHEN 2. RAISE error_communicationtable.
        WHEN 3. RAISE error_connectiontable.
        WHEN 4. RAISE error_kernel.
        WHEN 5. RAISE error_parameter.
        WHEN 6. RAISE error_archiv. "?
      ENDCASE.

    ELSE.
      IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
        " Other than PDF format raise error.
        MESSAGE e789(po) WITH lv_archiveformat INTO mesg.
        CALL FUNCTION 'NAST_PROTOCOL_UPDATE'            "^2287056
          EXPORTING
            msg_arbgb = syst-msgid                      "2410565
            msg_nr    = syst-msgno
            msg_ty    = syst-msgty
            msg_v1    = syst-msgv1
            msg_v2    = syst-msgv2
            msg_v3    = syst-msgv3
            msg_v4    = syst-msgv4
          EXCEPTIONS
            OTHERS    = 1.
      ENDIF.                                                "v3108180
      ef_retco = 1.
      EXIT.                                                 "v2287056
    ENDIF.
  ENDIF.


  CALL FUNCTION 'FP_GET_LAST_ADS_ERRSTR'
    IMPORTING
      e_adserrstr = l_errstr.
*  exceptions
*    usage_error    = 1
*    system_error   = 2
*    others         = 4.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
* To handle print and archive scenario
  IF nast-tdarmod EQ 3.
    fp_outputparams-getpdf = abap_false.
  ENDIF.

  CALL FUNCTION 'FP_JOB_CLOSE'
    IMPORTING
      e_result       = e_result      "2805618
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
      CALL FUNCTION 'NAST_PROTOCOL_UPDATE'            "^2287056
        EXPORTING
          msg_arbgb = syst-msgid                      "2410565
          msg_nr    = syst-msgno
          msg_ty    = syst-msgty
          msg_v1    = syst-msgv1
          msg_v2    = syst-msgv2
          msg_v3    = syst-msgv3
          msg_v4    = syst-msgv4
        EXCEPTIONS
          OTHERS    = 1.
    ENDIF.                                                  "v3108180
    ef_retco = 1.
    EXIT.                                                   "v2287056
  ENDIF.

  IF nast-nacha EQ 1.
    READ TABLE e_result-spoolids INTO spoolid INDEX 1.      "^2805618
    IF sy-subrc EQ 0.
      IF lv_output_control_type NE cl_mmpur_constants=>if_mmpur_constants_func_switch~output_control_brf_bopf. "v3108180
        sy-msgv1 = spoolid.
        MESSAGE w320(me) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mesg.
        CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
          EXPORTING
            msg_arbgb = 'ME'
            msg_nr    = 320
            msg_ty    = 'W'
            msg_v1    = sy-msgv1
            msg_v2    = sy-msgv2
            msg_v3    = sy-msgv3
            msg_v4    = sy-msgv4
          EXCEPTIONS
            OTHERS    = 1.
      ENDIF.                                                "v3108180
    ENDIF.
  ENDIF.                                                    "v2805618

  IF nast-kappl  EQ 'EL' AND nast-sndex IS INITIAL AND if_preview IS INITIAL. "^1536566
* missing environment for limiting update print dependend data
    IF sy-ucomm NE '9ANZ' AND sy-ucomm NE '9DPR'.
      PERFORM update_release(saplmedruck) TABLES cl_output_schedagr_rel->it_ekpo
                                                 cl_output_schedagr_rel->it_ekek
                                                 cl_output_schedagr_rel->it_ekeh
                             USING  if_druvo nast-kschl.
    ENDIF.
  ENDIF.                                                    "v1536566

  IF if_preview = 'W'. "web dynpro output
    EXPORT lv_pdf_file = os_formout-pdf TO MEMORY ID 'PDF_FILE'.
  ENDIF.

ENDFORM.                    " adobe_print_output


*&---------------------------------------------------------------------*
*&      Form  fill_control_structure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAST  text
*      -->P_IF_PREVIEW  text
*      <--P_FP_OUTPUTPARAMS  text
*----------------------------------------------------------------------*
FORM fill_control_structure  USING    VALUE(is_nast)    TYPE nast
                                      VALUE(if_preview) TYPE c
                             CHANGING es_outparms  TYPE sfpoutputparams.

  CLEAR: es_outparms.
  IF if_preview IS INITIAL.
    CLEAR: es_outparms-preview.
  ELSE.
    es_outparms-preview = 'X'.
    es_outparms-noprint = 'X'.
  ENDIF.
  es_outparms-nodialog = 'X'.
  es_outparms-dest = is_nast-ldest.
  es_outparms-reqimm = is_nast-dimme.
  es_outparms-reqdel = is_nast-delet.
  es_outparms-copies = is_nast-anzal.
  es_outparms-dataset = is_nast-dsnam.
  es_outparms-suffix1 = is_nast-dsuf1.
  es_outparms-suffix2 = is_nast-dsuf2.
  es_outparms-covtitle = is_nast-tdcovtitle.
  es_outparms-cover = is_nast-tdocover.
  es_outparms-receiver = is_nast-tdreceiver.
  es_outparms-division = is_nast-tddivision.
  es_outparms-reqfinal = 'X'.
  es_outparms-arcmode = is_nast-tdarmod.
  es_outparms-schedule = is_nast-tdschedule.
  es_outparms-senddate = is_nast-vsdat.
  es_outparms-sendtime = is_nast-vsura.


ENDFORM."#EC CI_VALPAR                    " fill_control_structure

*&---------------------------------------------------------------------*
*&      Form  xstring_to_solix
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IP_XSTRING text
*----------------------------------------------------------------------*
FORM  xstring_to_solix
  USING ip_xstring TYPE xstring.

  DATA:
    lp_offset          TYPE i,
    lt_solix           TYPE solix_tab,
    ls_solix_line      TYPE solix,
    lp_pdf_string_len  TYPE i,
    lp_solix_rows      TYPE i,
    lp_last_row_length TYPE i,
    lp_row_length      TYPE i.

  CLEAR pdf_content.

* transform xstring to SOLIX
  DESCRIBE TABLE lt_solix.
  lp_row_length = sy-tleng.
  lp_offset = 0.

  lp_pdf_string_len = xstrlen( ip_xstring ).

  lp_solix_rows = lp_pdf_string_len DIV lp_row_length.
  lp_last_row_length = lp_pdf_string_len MOD lp_row_length.
  DO lp_solix_rows TIMES.
    ls_solix_line-line =
           ip_xstring+lp_offset(lp_row_length).
    APPEND ls_solix_line TO pdf_content.
    ADD lp_row_length TO lp_offset.
  ENDDO.
  IF lp_last_row_length > 0.
    CLEAR ls_solix_line-line.
    ls_solix_line-line = ip_xstring+lp_offset(lp_last_row_length).
    APPEND ls_solix_line TO pdf_content.
  ENDIF.

ENDFORM.                    "XSTRING_TO_SOLIX
