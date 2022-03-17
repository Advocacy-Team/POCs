CLASS lhc_ZADT_I_Good_For_M DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ZADT_I_Good_For_M RESULT result.

ENDCLASS.

CLASS lhc_ZADT_I_Good_For_M IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.
