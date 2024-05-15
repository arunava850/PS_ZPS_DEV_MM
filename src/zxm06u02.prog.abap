*&---------------------------------------------------------------------*
*& Include          ZXM06U02
*&---------------------------------------------------------------------*
 DATA: lv_test     TYPE c,
       ls_ze1edkhd TYPE ze1edkhd,
       ls_e1edk01  TYPE e1edk01,
       ls_e1edp19  TYPE e1edp19,
       ls_ze1edp19 TYPE ze1edp19,
       ls_e1edp01  TYPE e1edp01,
       lv_matnr    TYPE matnr,
       lv_gr_qty   TYPE ekbe-bpmng.

 CLEAR: lv_test,lv_gr_qty.

 DATA(lv_lines) = lines( int_edidd[] ).
 READ TABLE int_edidd ASSIGNING FIELD-SYMBOL(<lfs_edidd>) INDEX lv_lines.
 IF <lfs_edidd> IS ASSIGNED.
   DATA(ls_edidd) = <lfs_edidd>.
 ENDIF.
 CASE ls_edidd-segnam.
   WHEN  'E1EDK01'.
     ls_edidd-segnam = 'ZE1EDKHD'.
     ls_e1edk01 = ls_edidd-sdata.
     CLEAR ls_edidd-sdata.
     SELECT SINGLE name1 FROM lfa1 INTO ls_ze1edkhd-zname
        WHERE lifnr = ls_e1edk01-recipnt_no.
     IF sy-subrc IS NOT INITIAL.
       CLEAR ls_ze1edkhd-zname.
     ENDIF.
     ls_edidd-hlevel = '02'.
     ls_edidd-sdata = ls_ze1edkhd.
     APPEND ls_edidd TO int_edidd.
     CLEAR:ls_ze1edkhd,ls_e1edk01,ls_edidd.
*     DO.
*
*     ENDDO.
   WHEN 'E1EDP19'.
     ls_edidd-segnam = 'ZE1EDP19'.
     ls_e1edp19 = ls_edidd-sdata.
     CLEAR ls_edidd-sdata.
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = ls_e1edp19-idtnr
       IMPORTING
         output = lv_matnr.
     IF sy-subrc <> 0.
* Implement suitable error handling here
     ENDIF.

     SELECT SINGLE bismt FROM mara INTO ls_ze1edp19-zsku
        WHERE matnr = lv_matnr.
     IF sy-subrc IS NOT INITIAL.
       CLEAR ls_ze1edp19-zsku.
     ENDIF.
     ls_edidd-hlevel = '04'.
     ls_edidd-sdata = ls_ze1edp19.
     APPEND ls_edidd TO int_edidd.
     CLEAR:ls_ze1edp19,ls_e1edp19,ls_edidd.

   WHEN 'E1EDP01'.
     ls_e1edp01 = ls_edidd-sdata.
     IF ls_e1edp01-action = '003' OR ls_e1edp01-action = '005'.
       ls_e1edp01-bmng2 = 0.
       CONDENSE ls_e1edp01-bmng2 NO-GAPS.
       <lfs_edidd>-sdata = ls_e1edp01.
     ELSEIF ls_e1edp01-action = '002'.
       SELECT SINGLE ebeln,ebelp,elikz INTO @DATA(ls_ekpo) FROM ekpo
         WHERE ebeln = @xekko-ebeln
           AND ebelp = @ls_e1edp01-posex.
       IF sy-subrc IS INITIAL AND ls_ekpo-elikz EQ abap_true.
          ls_e1edp01-bmng2 = 0.
       ELSE.
         SELECT ebeln,ebelp,bwart,bpmng FROM ekbe
           INTO TABLE @DATA(lt_ekbe)
           WHERE ebeln = @xekko-ebeln
             AND ebelp = @ls_e1edp01-posex
             AND bwart IN ('101','102').
         IF sy-subrc IS INITIAL.
           CLEAR lv_gr_qty.
           LOOP AT lt_ekbe INTO DATA(ls_ekbe).
             IF ls_ekbe-bwart = '101'.
               lv_gr_qty = lv_gr_qty + ls_ekbe-bpmng.
             ELSEIF ls_ekbe-bwart = '102'.
               lv_gr_qty = lv_gr_qty - ls_ekbe-bpmng.
             ENDIF.
           ENDLOOP.
         ENDIF.
         ls_e1edp01-bmng2 = ls_e1edp01-bmng2 - lv_gr_qty.
       ENDIF.
       CONDENSE ls_e1edp01-bmng2 NO-GAPS.
       <lfs_edidd>-sdata = ls_e1edp01.
     ENDIF.
 ENDCASE.
*   CLEAR lv_test.
* ENDIF.
