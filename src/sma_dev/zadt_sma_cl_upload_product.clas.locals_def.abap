*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES:
  BEGIN OF ts_filedata,
    ProductType        TYPE c LENGTH 4,
    BaseUnit           TYPE c LENGTH 3,
    IndustrySector     TYPE c LENGTH 2,
    Language           TYPE c LENGTH 3,
    ProductDescription TYPE c LENGTH 40,

  END OF ts_filedata.

TYPES: tt_filedata TYPE STANDARD TABLE OF ts_filedata.
