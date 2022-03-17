CLASS zadt_cl_job DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_apj_dt_exec_object.
    INTERFACES if_apj_rt_exec_object.
PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zadt_cl_job IMPLEMENTATION.

METHOD if_apj_dt_exec_object~get_parameters.

  " Return the supported selection parameters here
  et_parameter_def = VALUE #( ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter datatype = 'C' length = 80 param_text = 'Test Description' lowercase_ind = abap_true changeable_ind = abap_true )  ).
  " Return the default parameters values here
  et_parameter_val = VALUE #( ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = 'My Default Description' ) ).

ENDMETHOD.

METHOD if_apj_rt_exec_object~execute.

  DATA: lv_obj TYPE REF TO zadt_goodfor_process,
        lv_out TYPE REF TO if_oo_adt_classrun_out.

  CREATE OBJECT lv_obj.
  lv_obj->if_oo_adt_classrun~main( lv_out ).

ENDMETHOD.

METHOD if_oo_adt_classrun~main.

  DATA: lv_obj TYPE REF TO zcl_test_transfer_posting,
        lv_out TYPE REF TO if_oo_adt_classrun_out.

  CREATE OBJECT lv_obj.
  lv_obj->if_oo_adt_classrun~main( lv_out ).

ENDMETHOD.
ENDCLASS.
