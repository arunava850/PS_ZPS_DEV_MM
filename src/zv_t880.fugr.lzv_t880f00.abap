*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZV_T880.........................................*
FORM GET_DATA_ZV_T880.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM T880 WHERE
(VIM_WHERETAB) .
    CLEAR ZV_T880 .
ZV_T880-MANDT =
T880-MANDT .
ZV_T880-RCOMP =
T880-RCOMP .
ZV_T880-NAME1 =
T880-NAME1 .
ZV_T880-CNTRY =
T880-CNTRY .
ZV_T880-NAME2 =
T880-NAME2 .
ZV_T880-LANGU =
T880-LANGU .
ZV_T880-STRET =
T880-STRET .
ZV_T880-POBOX =
T880-POBOX .
ZV_T880-PSTLC =
T880-PSTLC .
ZV_T880-CITY =
T880-CITY .
ZV_T880-CURR =
T880-CURR .
ZV_T880-ZLEGAL_OWN_FEIN =
T880-ZLEGAL_OWN_FEIN .
<VIM_TOTAL_STRUC> = ZV_T880.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZV_T880 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZV_T880.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZV_T880-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM T880 WHERE
  RCOMP = ZV_T880-RCOMP .
    IF SY-SUBRC = 0.
    DELETE T880 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM T880 WHERE
  RCOMP = ZV_T880-RCOMP .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR T880.
    ENDIF.
T880-MANDT =
ZV_T880-MANDT .
T880-RCOMP =
ZV_T880-RCOMP .
T880-NAME1 =
ZV_T880-NAME1 .
T880-CNTRY =
ZV_T880-CNTRY .
T880-NAME2 =
ZV_T880-NAME2 .
T880-LANGU =
ZV_T880-LANGU .
T880-STRET =
ZV_T880-STRET .
T880-POBOX =
ZV_T880-POBOX .
T880-PSTLC =
ZV_T880-PSTLC .
T880-CITY =
ZV_T880-CITY .
T880-CURR =
ZV_T880-CURR .
T880-ZLEGAL_OWN_FEIN =
ZV_T880-ZLEGAL_OWN_FEIN .
    IF SY-SUBRC = 0.
    UPDATE T880 ##WARN_OK.
    ELSE.
    INSERT T880 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZV_T880-UPD_FLAG,
STATUS_ZV_T880-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZV_T880.
  SELECT SINGLE * FROM T880 WHERE
RCOMP = ZV_T880-RCOMP .
ZV_T880-MANDT =
T880-MANDT .
ZV_T880-RCOMP =
T880-RCOMP .
ZV_T880-NAME1 =
T880-NAME1 .
ZV_T880-CNTRY =
T880-CNTRY .
ZV_T880-NAME2 =
T880-NAME2 .
ZV_T880-LANGU =
T880-LANGU .
ZV_T880-STRET =
T880-STRET .
ZV_T880-POBOX =
T880-POBOX .
ZV_T880-PSTLC =
T880-PSTLC .
ZV_T880-CITY =
T880-CITY .
ZV_T880-CURR =
T880-CURR .
ZV_T880-ZLEGAL_OWN_FEIN =
T880-ZLEGAL_OWN_FEIN .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZV_T880 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZV_T880-RCOMP TO
T880-RCOMP .
MOVE ZV_T880-MANDT TO
T880-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'T880'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN T880 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'T880'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
