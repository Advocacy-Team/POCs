class ZADT_VKO_DEV_CL_PO_API definition
PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: constructor,
      get_order_details RETURNING VALUE(r_json) TYPE string.
    DATA: gv_web  TYPE string VALUE 'https://sandbox.api.sap.com/s4hanacloud/sap/opu/odata/sap/API_PURCHASEORDER_PROCESS_SRV',
          go_http TYPE REF TO if_web_http_client.
    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZADT_VKO_DEV_CL_PO_API IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.
    DATA(lt_params) = request->get_form_fields( ).
    LOOP AT lt_params ASSIGNING FIELD-SYMBOL(<fs_urlparam>).
    CASE <fs_urlparam>-value.
    WHEN 'api'.
    response->set_text( get_order_details(  ) ).
    ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD constructor.
    go_http = cl_web_http_client_manager=>create_by_http_destination(
    i_destination = cl_http_destination_provider=>create_by_url( gv_web ) ).
  ENDMETHOD.

  METHOD get_order_details.
    DATA(lo_req) = go_http->get_http_request(  ).
    lo_req->set_header_fields( VALUE #(
     ( name = 'Content-Type' value = 'application/json')
     ( name = 'Accept' value = 'application/json' )
     ( name = 'APIKey' value = 'hwTmbcPc1KimcX4jFm96rUR3ApgHngUs' )
     ) ).

"    go_http->get_http_request( )->set_authorization_basic(
 "                                        i_username = 'API_USER'
  "                                       i_password = 'Sap20172018201920202021*' ).
    lo_req->set_uri_path( i_uri_path = gv_web && '/A_PurchaseOrder?$top=1&format=json' ).

    TRY.
        DATA(lv_response) = go_http->execute( i_method = if_web_http_client=>get )->get_text(  ).
        r_json = lv_response.
      CATCH cx_web_http_client_error cx_web_message_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
