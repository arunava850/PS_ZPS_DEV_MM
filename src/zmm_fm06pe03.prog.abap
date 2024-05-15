*&---------------------------------------------------------------------*
*&  Include           FM06PE03
*&---------------------------------------------------------------------*



*&--------------------------------------------------------------------*
*&      Form  adobe_entry_neu
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_neu USING ent_retco  LIKE sy-subrc
                           ent_screen TYPE c.
  DATA: xdruvo TYPE c,
        L_XFZ type c.

  IF nast-aende EQ space.
    xdruvo = prntev_new.
  ELSE.
      xdruvo = prntev_chg.
  ENDIF.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.

ENDFORM.                    " adobe_entry_neu

*&--------------------------------------------------------------------*
*&      Form  adobe_entry_LPJE_CD
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_LPJE_CD USING ent_retco  LIKE sy-subrc
                           ent_screen TYPE c.
   DATA: xdruvo TYPE c,
        L_XFZ type c.

  clear l_xfz.

xdruvo = prntev_lpje.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.


ENDFORM.                    " adobe_entry_LPJE_CD

*&--------------------------------------------------------------------*
*&      Form  adobe_entry_LPHE_CD
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
* This subroutien is copy of adobe_entry_LPHE and the condition is handled internally
*---------------------------------------------------------------------*
FORM adobe_entry_LPHE_CD USING ent_retco  LIKE sy-subrc
                           ent_screen TYPE c.
    DATA: xdruvo TYPE c,
        L_XFZ type c.

  clear l_xfz.

  xdruvo = prntev_lphe.

   PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.


ENDFORM.                    " adobe_entry_LPHE_CD

*&--------------------------------------------------------------------*
*&      Form  adobe_entry_absa
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_absa USING ent_retco  LIKE sy-subrc
                           ent_screen TYPE c.
   DATA: xdruvo TYPE c,
        L_XFZ type c.

  clear l_xfz.


    xdruvo = prntev_absa.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.


ENDFORM.                    " adobe_entry_absa


*****Start of pdf coding by "C5073377"
*&--------------------------------------------------------------------*
*&      Form  Zadobe_entry_lpma
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_lpma USING ent_retco  LIKE sy-subrc
                           ent_screen TYPE c.
   DATA: xdruvo TYPE c,
        L_XFZ type c.

  clear l_xfz.

  IF nast-aende EQ space.
    xdruvo = prntev_lpma.
  ELSE.
    xdruvo = prntev_chg.
  ENDIF.

*    xdruvo = prntev_lpma.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.


ENDFORM.                    " adobe_entry_lpma

*&--------------------------------------------------------------------*
*&      Form  adobe_entry_mahn
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_mahn USING ent_retco  LIKE sy-subrc
                            ent_screen TYPE c.
  DATA: xdruvo TYPE c,
        L_XFZ type c.

  clear l_xfz.

  xdruvo = prntev_mahn.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.

ENDFORM.                    " adobe_entry_mahn

*&--------------------------------------------------------------------*
*&      Form  adobe_entry_lpet
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_lpet USING ent_retco  LIKE sy-subrc
                            ent_screen TYPE c.
  DATA: xdruvo TYPE c,
        L_XFZ type c.

  clear l_xfz.

  IF nast-aende EQ space.
    xdruvo = prntev_lpet.
  ELSE.
    xdruvo = prntev_lpet_chg.
  ENDIF.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.

ENDFORM.                    " adobe_entry_lpet.

*&--------------------------------------------------------------------*
*&      Form  adobe_entry_lphe
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_lphe USING ent_retco  LIKE sy-subrc
                            ent_screen TYPE c.
  DATA: xdruvo TYPE c,
        L_XFZ type c.

  xdruvo = prntev_lphe.
  L_XFZ = 'X'.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.

ENDFORM.                    " adobe_entry_lphe.

*&--------------------------------------------------------------------*
*&      Form  adobe_entry_lpje
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_lpje USING ent_retco  LIKE sy-subrc
                            ent_screen TYPE c.
  DATA: xdruvo TYPE c,
        L_XFZ type c.

  clear l_xfz.
  xdruvo = prntev_lpje.
  L_XFZ = 'X'.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.

ENDFORM.                    " adobe_entry_lpje.

*&--------------------------------------------------------------------*
*&      Form  adobe_entry_aufb
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_aufb USING ent_retco  LIKE sy-subrc
                            ent_screen TYPE c.
   DATA: xdruvo TYPE c,
        L_XFZ type c.

  clear l_xfz.

  xdruvo = prntev_aufb.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.

ENDFORM.                    " adobe_entry_aufb.
*&--------------------------------------------------------------------*
*&      Form  adobe_entry_lpfz
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_lpfz USING ent_retco  LIKE sy-subrc
                            ent_screen TYPE c.
    DATA: xdruvo TYPE c,
        L_XFZ type c.

  clear l_xfz.

  IF nast-aende EQ space.
    xdruvo = prntev_lpet.
  ELSE.
    xdruvo = prntev_lpet_chg.
  ENDIF.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.

ENDFORM.                    " adobe_entry_lpfz.
