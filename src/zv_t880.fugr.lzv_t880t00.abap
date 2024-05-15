*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZV_T880.........................................*
TABLES: ZV_T880, *ZV_T880. "view work areas
CONTROLS: TCTRL_ZV_T880
TYPE TABLEVIEW USING SCREEN '0160'.
DATA: BEGIN OF STATUS_ZV_T880. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZV_T880.
* Table for entries selected to show on screen
DATA: BEGIN OF ZV_T880_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZV_T880.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_T880_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZV_T880_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZV_T880.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_T880_TOTAL.

*.........table declarations:.................................*
TABLES: T880                           .
