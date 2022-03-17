CLASS zadt_sma_api_ses DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor
      RAISING
        cx_web_http_client_error
        cx_http_dest_provider_error,
      get_api RETURNING VALUE(r_json) TYPE string.
    DATA: gv_web  TYPE string VALUE 'https://sandbox.api.sap.com/s4hanacloud/sap/opu/odata/sap/',
          go_http TYPE REF TO if_web_http_client.
    INTERFACES if_http_service_extension .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zadt_sma_api_ses IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.
    DATA(lt_params) = request->get_form_fields( ).
    LOOP AT lt_params ASSIGNING FIELD-SYMBOL(<fs_urlparam>).
      CASE <fs_urlparam>-value.
        WHEN 'api'.
          response->set_text( get_api(  ) ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD constructor.
    go_http = cl_web_http_client_manager=>create_by_http_destination(
    i_destination = cl_http_destination_provider=>create_by_url( gv_web ) ).
  ENDMETHOD.

  METHOD get_api.
    DATA(lo_req) = go_http->get_http_request(  ).
    lo_req->set_header_fields( VALUE #(
     ( name = 'Content-Type' value = 'application/json')
     ( name = 'Accept' value = 'application/json' )
     ( name = 'APIKey' value = '9vbvvmlDLGlGdvFSamfGorVvzapl3RJn' )
     ) ).

    lo_req->set_uri_path( i_uri_path = gv_web && 'API_SERVICE_ENTRY_SHEET_SRV/A_ServiceEntrySheet' ).

    TRY.
        DATA(lv_response) = go_http->execute( i_method = if_web_http_client=>get )->get_text(  ).
        r_json = lv_response.
      CATCH cx_web_http_client_error cx_web_message_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
