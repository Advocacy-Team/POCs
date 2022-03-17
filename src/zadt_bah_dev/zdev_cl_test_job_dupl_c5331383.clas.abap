CLASS zdev_cl_test_job_dupl_c5331383 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zdev_cl_test_job_dupl_c5331383 IMPLEMENTATION.

METHOD if_oo_adt_classrun~main.

    CONSTANTS lc_catalog_name      TYPE cl_apj_dt_create_content=>ty_catalog_name  VALUE 'ZMIK_CATALOG_D1KP'.
    CONSTANTS lc_catalog_text      TYPE cl_apj_dt_create_content=>ty_text          VALUE 'D1KP Catal'.
    CONSTANTS lc_class_name        TYPE cl_apj_dt_create_content=>ty_class_name    VALUE 'ZADT_CL_JOB_DUPL_C5332309'.

    CONSTANTS lc_template_name     TYPE cl_apj_dt_create_content=>ty_template_name VALUE 'ZMIK_TEMPLATE_D1KP'.
    CONSTANTS lc_template_text     TYPE cl_apj_dt_create_content=>ty_text          VALUE 'TTD1KP'.

    CONSTANTS lc_transport_request TYPE cl_apj_dt_create_content=>ty_transport_request VALUE 'FZBK900043'.
    CONSTANTS lc_package           TYPE cl_apj_dt_create_content=>ty_package           VALUE 'ZADT_KAS_DEV'.

    DATA(lo_dt) = cl_apj_dt_create_content=>get_instance( ).

    " Create job catalog entry (corresponds to the former report incl. selection parameters)
    " Provided implementation class iv_class_name shall implement two interfaces:
    " - if_apj_dt_exec_object to provide the definition of all supported selection parameters of the job
    "   (corresponds to the former report selection parameters) and to provide the actual default values
    " - if_apj_rt_exec_object to implement the job execution
    TRY.
      DATA(lv_ret) =  lo_dt->create_job_cat_entry(
            iv_catalog_name       = lc_catalog_name
            iv_class_name         = lc_class_name
            iv_text               = lc_catalog_text
            iv_catalog_entry_type = cl_apj_dt_create_content=>class_based
            iv_transport_request  = lc_transport_request
            iv_package            = lc_package
        ).
        out->write( |Job catalog entry created successfully| ).

      CATCH cx_apj_dt_content INTO DATA(lx_apj_dt_content).
        out->write( |Creation of job catalog entry failed: { lx_apj_dt_content->get_text( ) }| ).
    ENDTRY.

    " Create job template (corresponds to the former system selection variant) which is mandatory
    " to select the job later on in the Fiori app to schedule the job
    DATA lt_parameters TYPE if_apj_dt_exec_object=>tt_templ_val.

    NEW zadt_cl_job_dupl_c5332309( )->if_apj_dt_exec_object~get_parameters(
      IMPORTING
        et_parameter_val = lt_parameters
    ).

    TRY.
      DATA(lv_ret1) = lo_dt->create_job_template_entry(
            iv_template_name     = lc_template_name
            iv_catalog_name      = lc_catalog_name
            iv_text              = lc_template_text
            it_parameters        = lt_parameters
            iv_transport_request = lc_transport_request
            iv_package           = lc_package

        ).
        out->write( |Job template created successfully| ).

      CATCH cx_apj_dt_content INTO lx_apj_dt_content.
        lo_dt->delete_job_template_entry( iv_template_name     = lc_template_name
                                          iv_transport_request = lc_transport_request ).
        lo_dt->delete_job_cat_entry( iv_catalog_name = lc_catalog_name
                                     iv_transport_request = lc_transport_request ).
        out->write( |Creation of job template failed: { lx_apj_dt_content->get_text( ) } but no they deleted| ).
        RETURN.
    ENDTRY.



*  TRY.
*      DATA(lo_dest) = cl_http_destination_provider=>create_by_url(
*           i_url    = 'https://my305270-api.s4hana.ondemand.com/sap/opu/odata/sap/API_INSPECTIONPLAN_SRV/A_InspPlanMaterialAssgmt'
*        ).
*
*      DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest ).
*
**// Set authorization to API Hub
*      lo_http_client->get_http_request( )->set_authorization_basic( i_username = 'ZOVO_TEST'
*                                                                    i_password = 'JaKshGkcbSov~ukPYs4VXZwbVYgvTAVkLRBbuglj' ).
**// Set CSRF Token for post
*      TRY.
*          lo_http_client->set_csrf_token( ).
*        CATCH cx_web_http_client_error.
*      ENDTRY.
*
*      DATA(lo_request) = lo_http_client->get_http_request( ).
*
*      "adding headers
*      DATA(lo_value) = lo_request->set_header_field( EXPORTING i_name = 'APIKey' i_value = '0OuaH2Gk4LfnZigWEiMYgoGxjKoPG4Ea' ).
*
*      lo_request->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
*      lo_request->set_header_field( EXPORTING i_name = 'Accept'             i_value = 'application/json' ).
*      lo_request->set_header_field( EXPORTING i_name = 'Content-Type'       i_value = 'application/json' ).
*
*      DATA(lr_csrf) = lo_request->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).
*
*      lo_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = lr_csrf ).
*
*      DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
**//commit WORK.
*      DATA(lv) = lo_response->get_text( ).
**                 CATCH cx_web_message_error.
**
*    CATCH cx_root INTO DATA(lx_exception).
*      out->write( lx_exception->get_text( ) ).
*  ENDTRY.

*    TRY.
*            DATA(lo_dest) = cl_http_destination_provider=>create_by_url(
*                 i_url    = 'https://my305270-api.s4hana.ondemand.com/sap/opu/odata/sap/API_INSPECTIONPLAN_SRV/A_InspPlanMaterialAssgmt'
*              ).
*
*            DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest ).
*
**// Set authorization to API Hub
*            lo_http_client->get_http_request( )->set_authorization_basic( i_username = 'ZOVO_TEST'
*                                                                          i_password = 'JaKshGkcbSov~ukPYs4VXZwbVYgvTAVkLRBbuglj' ).
**// Set CSRF Token for post
*            TRY.
*                lo_http_client->set_csrf_token( ).
*              CATCH cx_web_http_client_error.
*            ENDTRY.
*
*            DATA(lo_request) = lo_http_client->get_http_request( ).
*
**// Filter on Material Document
*            lo_request->set_query( EXPORTING query = '$filter=Material eq ''4900000000'',$filter=Plant eq ''1710'' ').
*
*            "adding headers
*            DATA(lo_value) = lo_request->set_header_field( EXPORTING i_name = 'APIKey' i_value = 'v6ndvEfG8uJYuRlSiiIxuTkd7c62qQ4J' ).
*
*            lo_request->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
*            lo_request->set_header_field( EXPORTING i_name = 'Accept'             i_value = 'application/json' ).
*            lo_request->set_header_field( EXPORTING i_name = 'Content-Type'       i_value = 'application/json' ).
*
*            DATA(lr_csrf) = lo_request->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).
*
*            lo_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = lr_csrf ).
*
*            DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
**//commit WORK.
*            out->write( lo_response->get_text( ) ).
*          CATCH cx_root INTO DATA(lx_exception).
*            out->write( lx_exception->get_text( ) ).
*        ENDTRY.



*  MODIFY zjest FROM TABLE @( VALUE #( ( objnr = 'QL010000000004' stat = 'I0001' inact = 'X' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0002' inact = 'X' chgnr = '002' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0201' inact = 'X' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0202' inact = 'X' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0205' inact = '' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0206' inact = '' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0207' inact = '' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0212' inact = 'X' chgnr = '002' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0213' inact = '' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0216' inact = '' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0217' inact = '' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0218' inact = '' chgnr = '001' _dataaging = '' )
*                                      ( objnr = 'QL010000000004' stat = 'I0221' inact = '' chgnr = '001' _dataaging = '' )  ) ).

*  UPDATE qals FROM TABLE @( VALUE #( ( werk = '0001' matnr = 'TEST_MM_ODATA123' CHARG = '' PRUEFLOS = '010000000004' objnr = 'QL010000000004' ) ) ).

*MODIFY zadt_yggood_for FROM TABLE @( VALUE #( ( plant = '0001' target_pq = 'TEST_MM_ODATA123' good_for_pq = 'TEST_MM_ODATA' priority  = '1' )
*                                           ( plant = '0001' target_pq = 'TEST_MM_ODATA123' good_for_pq = 'IP1' priority  = '2' )
*                                           ( plant = '0001' target_pq = 'TEST_MM_ODATA123' good_for_pq = 'IP2' priority  = '3' ) ) ).

*         TRY.
*        "create API destination
*        DATA(lo_http_destination) = cl_http_destination_provider=>create_by_url( 'https://my400036-api.lab.s4hana.cloud.sap:443/sap/opu/odata/sap/API_INSPECTIONPLAN_SRV/A_InspectionPlan?$inlinecount=allpages&$top=50' ).
*        DATA(lo_web_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ) .
*
*        "set required headers for request
**        lo_web_http_client->set_authn_mode( if_a4c_cp_service=>service_specific ).
*
*        DATA(lo_web_http_request) = lo_web_http_client->get_http_request( ).
*        lo_web_http_request->set_authorization_basic( i_username = 'ZOMARKOS'
*                                                      i_password = 'QVgDSeZvYG#zyeqpLLVmmulsdMggYTBnaq7jwhfl' ).
*        lo_web_http_request->set_header_fields( VALUE #( (  name = 'APIKey' value = '1ZKvZxRy6vLpFL9G1vfEbFxclGFe783D' )
*                                                         (  name = 'DataServiceVersion' value = '2.0'  )
*                                                         (  name = 'Accept' value = 'application/json' )
*                                                       )
*                                               ).
*
*        "set request method and execute request
*        DATA(lo_web_http_response) = lo_web_http_client->execute( if_web_http_client=>get ).
*
*        "receive and processing the data before response sending
*
*          DATA(lv_attachment) = lo_web_http_response->get_text( ).
*
*      CATCH cx_http_dest_provider_error cx_web_http_client_error cx_web_message_error.
*        "error handling
*    ENDTRY.

ENDMETHOD.
ENDCLASS.



