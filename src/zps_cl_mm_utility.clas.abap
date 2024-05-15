class ZPS_CL_MM_UTILITY definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TY_MAT_GROUP,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
          matkl TYPE matkl,
          loekz TYPE loekz,
         END OF TY_MAT_GROUP .
  types:
    TYT_MAT_GROUP TYPE TABLE OF TY_MAT_GROUP .

  class-data GT_MAT_GROUP type TYT_MAT_GROUP .
protected section.
private section.
ENDCLASS.



CLASS ZPS_CL_MM_UTILITY IMPLEMENTATION.
ENDCLASS.
