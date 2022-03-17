CLASS zadt_osa_cl_test DEFINITION
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
   interfaces if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zadt_osa_cl_test IMPLEMENTATION.
method if_oo_adt_classrun~main.

*INSERT ZQALS FROM TABLE @( VALUE #( ( client = '080' plant = '0001' material = 'TEST_MM_ODATA123' batch = '006' insplot = '010000000008' objnr = 'QL010000000009'
*stat35 ='X' enstehdat = '20220223' ) ) ).
INSERT ZADT_PLAF FROM TABLE @( VALUE #( ( client = '080' plnum = '12345' material = 'TEST_MM_ODATA123' planning_plant = '0001'
 ) ) ).

endmethod.
ENDCLASS.
