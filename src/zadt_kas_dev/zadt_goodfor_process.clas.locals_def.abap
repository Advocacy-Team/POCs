*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES:
  BEGIN OF ty_insplot,
    insplot  TYPE c LENGTH 12,
    plant    TYPE werks_d,
    material TYPE matnr,
    batch    TYPE c LENGTH 10,
    sloc     TYPE c LENGTH 4,
    objnr    TYPE c LENGTH 22,
  END OF ty_insplot.
