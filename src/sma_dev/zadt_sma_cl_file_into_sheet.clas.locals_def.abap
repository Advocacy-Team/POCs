*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
types:
     BEGIN OF ty_filedata,
     purord    type ebeln,
     ses_name  type c length 40,
     obj_typ   type c length 2,
     post_date type c length 25,
     cnfm_qty  type c LENGTH 4,
     po_item   type c length 5,
     Acnt_cat  type c length 1,
     prfm_date type c length 25,
     END OF ty_filedata.

TYPES: lty_filedata TYPE STANDARD TABLE OF ty_filedata.
