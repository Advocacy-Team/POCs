CLASS zadt_sma_table_fill DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
      INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zadt_sma_table_fill IMPLEMENTATION.



 METHOD if_oo_adt_classrun~main.

 data: ls_ccode1 type ts_cname.
 data: ls_ccode2 type ts_cname.
 data: it_ccode type tt_cname,
       field like line of it_ccode.

it_ccode = VALUE #(
( cuser = 'CB9980000014' ccode = '100' )
( cuser = 'CB9980000044' ccode = '200' ) ).

loop at it_ccode assigning field-symbol(<fs_ccode>).
insert zadt_sma_testtab from @<fs_ccode>.
endloop.
*insert zadt_sma_testtab from @it_ccode.

 COMMIT WORK.
endmethod.
ENDCLASS.
