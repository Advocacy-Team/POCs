*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
types:
       begin of ts_cname,
       cuser type c length 40,
       ccode type c length 40,
       end of ts_cname.

types: tt_cname type standard table of ts_cname.
