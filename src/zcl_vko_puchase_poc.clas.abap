class ZCL_VKO_PUCHASE_POC definition
PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: constructor,
      get_html RETURNING VALUE(ui_html) TYPE string,
     get_input_field_value IMPORTING name         TYPE string
                                             dataref      TYPE data
                                   RETURNING VALUE(value) TYPE string.
    DATA: gv_order_api_url  TYPE string VALUE 'https://my305270.s4hana.ondemand.com:443/sap/opu/odata/sap/API_PURCHASEORDER_PROCESS_SRV',
          gv_invoice_api_url TYPE string VALUE 'https://my305270.s4hana.ondemand.com:443/sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV',
          go_order_client TYPE REF TO if_web_http_client,
          go_invoice_client type ref to if_web_http_client,
          gv_order_num type string.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_VKO_PUCHASE_POC IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.

      CASE request->get_method(  ).
        WHEN CONV string( if_web_http_client=>get ).


    DATA(sap_order_request) = request->get_header_field('ordernum' ).
    IF sap_order_request is INITIAL.
    response->set_text( get_html(   ) ).
   ELSE.
   gv_order_num = sap_order_request.
    ENDIF.




      WHEN CONV string( if_web_http_client=>post ).
               go_order_client = cl_web_http_client_manager=>create_by_http_destination(
    i_destination = cl_http_destination_provider=>create_by_url( gv_order_api_url ) ).
    go_invoice_client = cl_web_http_client_manager=>create_by_http_destination(
    i_destination = cl_http_destination_provider=>create_by_url( gv_invoice_api_url ) ).
      Data header_json TYPE string.
   DATA(lo_req) = go_order_client->get_http_request(  ).
    lo_req->set_header_fields( VALUE #(
     ( name = 'Content-Type' value = 'application/json')
     ( name = 'Accept' value = 'application/json' )
     ( name = 'APIKey' value = 'hwTmbcPc1KimcX4jFm96rUR3ApgHngUs' )
     ) ).

    go_order_client->get_http_request( )->set_authorization_basic(
                                         i_username = 'ZOSA'
                                         i_password = 'WbsDMuaRLbMuxyCpvzEivmZKgj-PZBmSTZVJ9yqR' ).
    DATA lv_url TYPE string.


     " Unpack input field values such as tablename, dataoption, etc.
        DATA(ui_data) = request->get_form_field(  `filetoupload-data` ).
        DATA(ui_dataref) = /ui2/cl_json=>generate( json = ui_data ).
        IF ui_dataref IS BOUND.
          ASSIGN ui_dataref->* TO FIELD-SYMBOL(<ui_dataref>).
          gv_order_num = me->get_input_field_value( name = `TABLENAME` dataref = <ui_dataref> ).

        ENDIF.


    lv_url = 'https://my305270-api.s4hana.ondemand.com:443/sap/opu/odata/sap/API_PURCHASEORDER_PROCESS_SRV/A_PurchaseOrder('''.
    CONCATENATE lv_url gv_order_num into lv_url.
    CONCATENATE lv_url ''')' into lv_url.
   " lo_req->set_uri_path( i_uri_path ='https://my305270-api.s4hana.ondemand.com:443/sap/opu/odata/sap/API_PURCHASEORDER_PROCESS_SRV/A_PurchaseOrder(''4500000447'')' ).
    lo_req->set_uri_path( i_uri_path = lv_url ).
    TRY.
        DATA(lv_response) = go_order_client->execute( i_method = if_web_http_client=>get )->get_text(  ).
        header_json = lv_response.
        DATA: header_table TYPE STANDARD TABLE OF zvko_s_from_fzb.
        DATA: item_table TYPE STANDARD TABLE OF zvko_s_purchase_item.

       CONCATENATE lv_url '/to_PurchaseOrderItem' into lv_url.
       lo_req->set_uri_path( i_uri_path = lv_url ).
       lv_response =  go_order_client->execute( i_method = if_web_http_client=>get )->get_text(  ).

       data lv_item_json type string.
       data lv_str1 TYPE string.
       data lv_str2 TYPE string.
       data lv_str3 TYPE string.
       data lv_ret_rate TYPE string.
       data lv_ret_days TYPE string.
       data lv_company_code TYPE string.
       data lv_splitted_header TYPE string.
       data lv_splitted_item TYPE string.
       data lv_net_price_amount TYPE string.
       data lv_net_quantity TYPE string.
       data lv_paid type decfloat16.
       data lv_remain TYPE decfloat16.

   lv_item_json = lv_response.
 """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""Get header parameters from response """"""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   REPLACE ALL OCCURRENCES OF '"' IN header_json with ' '.
   SPLIT header_json AT 'YY1_RetentionRate_PDHF:' INTO lv_str1 lv_str2.
   SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
   lv_ret_rate = lv_str1.
   SPLIT header_json AT 'YY1_ZRetentionDays_PDHF:' INTO lv_str1 lv_str2.
   SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
   lv_ret_days = lv_str1.
   SPLIT header_json AT 'CompanyCode:' INTO lv_str1 lv_str2.
   SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
   lv_company_code = lv_str1.
   
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""field validation (2.3)""""""""""""""""""""""""
  if lv_ret_rate is initial  or lv_ret_days is initial.
   response->set_status( i_code = if_web_http_status=>bad_request
                                i_reason = |'Z' fields are empty. Impossible to process!| ).
          response->set_text( |'Z' fields are empty. Impossible to process!| ).
  return.
endif.
   """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""Form json for header"""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   lv_splitted_header = '[{"yy1_retentionrate_pdhf":"'.
   CONCATENATE lv_splitted_header lv_ret_rate INTO lv_splitted_header.
   CONCATENATE lv_splitted_header lv_ret_days into lv_splitted_header.
   CONCATENATE lv_splitted_header '"' into lv_splitted_header.
   CONCATENATE lv_splitted_header ',"yy1_zretentiondays_pdhf":"' into lv_splitted_header.
   CONCATENATE lv_splitted_header lv_ret_days into lv_splitted_header.
   CONCATENATE lv_splitted_header '"' into lv_splitted_header.
   CONCATENATE lv_splitted_header ',"companycode":"' into lv_splitted_header.
   CONCATENATE lv_splitted_header lv_company_code into lv_splitted_header.
   CONCATENATE lv_splitted_header '"' into lv_splitted_header.
   CONCATENATE lv_splitted_header '}]' into lv_splitted_header.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""GET PARAMETERS FROM ITEM""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   REPLACE ALL OCCURRENCES OF '"' IN lv_item_json with ' '.
   SPLIT lv_item_json AT 'NetPriceAmount:' INTO lv_str1 lv_str2.
   SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
   lv_net_price_amount = lv_str1.
   SPLIT lv_item_json AT 'NetPriceQuantity:' INTO lv_str1 lv_str2.
   SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
   lv_net_quantity = lv_str1.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""Form json for item""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   lv_splitted_item = '[{"netpriceamount":"'.
   CONCATENATE lv_splitted_item lv_net_price_amount INTO lv_splitted_item.
   CONCATENATE lv_splitted_item '"' into lv_splitted_item.
   CONCATENATE lv_splitted_item ',"netpricequantity":"' into lv_splitted_item.
   CONCATENATE lv_splitted_item lv_net_quantity into lv_splitted_item.
   CONCATENATE lv_splitted_item '"' into lv_splitted_item.
   CONCATENATE lv_splitted_item '}]' into lv_splitted_item.
.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""Jsons deserializing""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        /ui2/cl_json=>deserialize( EXPORTING json = lv_splitted_header
                                   pretty_name = /ui2/cl_json=>pretty_mode-none
                                   CHANGING data = header_table ).
        /ui2/cl_json=>deserialize( EXPORTING json = lv_splitted_item
                                   pretty_name = /ui2/cl_json=>pretty_mode-none
                                   CHANGING data = item_table ).
 "lv_paid procent
      lv_paid = item_table[ 1 ]-netpriceamount * item_table[ 1 ]-netpricequantity * header_table[ 1 ]-yy1_retentionrate_pdhf / 100.
"lv_remain to pay
      lv_remain = item_table[ 1 ]-netpricequantity * item_table[ 1 ]-netpriceamount - lv_paid.
    ENDTRY.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""Get request to get csrf token""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

   TRY.
    go_invoice_client->set_csrf_token( ).
   CATCH cx_web_http_client_error.
 ENDTRY.

     go_invoice_client->get_http_request( )->set_authorization_basic(
                                         i_username = 'ZOSA'
                                         i_password = 'WbsDMuaRLbMuxyCpvzEivmZKgj-PZBmSTZVJ9yqR' ).

 DATA(lo_req_invoice) = go_invoice_client->get_http_request(  ).
lo_req->set_uri_path( i_uri_path ='https://my305270-api.s4hana.ondemand.com:443/sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice' ).


   lo_req_invoice = go_order_client->get_http_request(  ).
    lo_req_invoice->set_header_fields( VALUE #(
     ( name = 'Content-Type' value = 'application/json')
     ( name = 'Accept' value = 'application/json' )
     ( name = 'APIKey' value = 'hwTmbcPc1KimcX4jFm96rUR3ApgHngUs' )
     ( name = 'x-csrf-token' value = 'fetch' )
     ) ).

  DATA(lv_token) =  go_invoice_client->execute( i_method = if_web_http_client=>get )->get_header_field( 'x-csrf-token' ).

         DATA(lo_dest1) = cl_http_destination_provider=>create_by_url(
            i_url                  = 'https://my305270-api.s4hana.ondemand.com:443/sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice'
            ).

          DATA(lo_http_client1) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest1 ).
*// Set authorization to API Hub
          lo_http_client1->get_http_request( )->set_authorization_basic(
                                       i_username = 'ZOSA'
                                       i_password = 'WbsDMuaRLbMuxyCpvzEivmZKgj-PZBmSTZVJ9yqR' ).
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""Post with received token"""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          TRY.
              lo_http_client1->set_csrf_token( ).
            CATCH cx_web_http_client_error.
          ENDTRY.

          DATA(lo_request1) = go_invoice_client->get_http_request( ).

     lo_request1->set_header_fields( VALUE #(
     ( name = 'Content-Type' value = 'application/json')
     ( name = 'Accept' value = 'application/json' )
     ( name = 'APIKey' value = 'peFl7bFjRZ9f4DfsAUmOlyw4k348lGnv' )
     ( name = 'x-csrf-token' value = lv_token )
     ) ).

data lv_body TYPE string.
data lo_value_r TYPE REF TO if_web_http_requesT.


lv_body = |\{"CompanyCode":"{ lv_company_code }","DocumentDate":"/Date(0000000000000)/","PostingDate":"/Date(1492041600000)/","DocumentCurrency":"USD","InvoiceGrossAmount":"{ lv_paid }"\}|.
" lv_body ='{"CompanyCode":"1710","DocumentDate":"/Date(1000000000000)/","PostingDate":"/Date(1492041600000)/","DocumentCurrency":"USD","InvoiceGrossAmount":"30"}'.
          TRY.
              lo_req_invoice->set_text(
                EXPORTING
                  i_text   = lv_body
                RECEIVING
                  r_value  = lo_value_r
              ).
            CATCH cx_web_message_error.
          ENDTRY.

      DATA(lv_post_resp) =  go_invoice_client->execute( i_method = if_web_http_client=>post )->get_text(  ).
   "  go_invoice_client->execute( i_method = if_web_http_client=>post ).
      ENDCASE.

  ENDMETHOD.
  METHOD constructor.
"    go_order_client = cl_web_http_client_manager=>create_by_http_destination(
 "   i_destination = cl_http_destination_provider=>create_by_url( gv_order_api_url ) ).
  "  go_invoice_client = cl_web_http_client_manager=>create_by_http_destination(
   " i_destination = cl_http_destination_provider=>create_by_url( gv_invoice_api_url ) ).
  ENDMETHOD.

METHOD get_input_field_value.

    FIELD-SYMBOLS: <value> TYPE data,
                   <field> TYPE any.

    ASSIGN COMPONENT name  OF STRUCTURE dataref TO <field>.
    IF <field> IS ASSIGNED.
      ASSIGN <field>->* TO <value>.
      value = condense( <value> ).
    ENDIF.

  ENDMETHOD.

    METHOD get_html.
    ui_html =
|<!DOCTYPE HTML> \r\n| &
|<html> \r\n| &
|<head> \r\n| &
|    <meta http-equiv="X-UA-Compatible" content="IE=edge"> \r\n| &
|    <meta http-equiv='Content-Type' content='text/html;charset=UTF-8' /> \r\n| &
|    <title>ABAP File Uploader</title> \r\n| &
|    <script id="sap-ui-bootstrap" src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js" \r\n| &
|        data-sap-ui-theme="sap_fiori_3_dark" data-sap-ui-xx-bindingSyntax="complex" data-sap-ui-compatVersion="edge" \r\n| &
|        data-sap-ui-async="true"> \r\n| &
|    </script> \r\n| &
|    <script> \r\n| &
|        sap.ui.require(['sap/ui/core/Core'], (oCore, ) => \{ \r\n| &
| \r\n| &
|            sap.ui.getCore().loadLibrary("sap.f", \{ \r\n| &
|                async: true \r\n| &
|            \}).then(() => \{ \r\n| &
|                let shell = new sap.f.ShellBar("shell") \r\n| &
|                shell.setTitle("Purchase Order Splitter") \r\n| &
|                shell.setShowCopilot(true) \r\n| &
|                shell.setShowSearch(true) \r\n| &
|                shell.setShowNotifications(true) \r\n| &
|                shell.setShowProductSwitcher(true) \r\n| &
|                shell.placeAt("uiArea") \r\n| &
|                sap.ui.getCore().loadLibrary("sap.ui.layout", \{ \r\n| &
|                    async: true \r\n| &
|                \}).then(() => \{ \r\n| &
|                    let layout = new sap.ui.layout.VerticalLayout("layout") \r\n| &
|                    layout.placeAt("uiArea") \r\n| &
|                    let line2 = new sap.ui.layout.HorizontalLayout("line2") \r\n| &
|                    let line3 = new sap.ui.layout.VerticalLayout("line3") \r\n| &
|      //              let line3 = new sap.ui.layout.HorizontalLayout("line3") \r\n| &
|                    let line4 = new sap.ui.layout.HorizontalLayout("line4") \r\n| &
|                    sap.ui.getCore().loadLibrary("sap.m", \{ \r\n| &
|                        async: true \r\n| &
|                    \}).then(() => \{\}) \r\n| &
|                    let button = new sap.m.Button("button") \r\n| &
|                    button.setText("Split Order") \r\n| &
|                    button.attachPress(function () \{ \r\n| &
|                        let oFileUploader = oCore.byId("fileToUpload") \r\n| &
|                        let oInput = oCore.byId("tablename") \r\n| &
|                        let oGroup = oCore.byId("grpDataOptions") \r\n| &
|                        if (!oInput.getValue())\{ \r\n| &
|                            sap.m.MessageToast.show("Order number is Required") \r\n| &
|                            return \r\n| &
|                        \} \r\n| &
|                       let param = oCore.byId("uploadParam") \r\n| &
|                       param.setValue( oInput.getValue() ) \r\n| &
|                       oFileUploader.getParameters() \r\n| &
|                       oFileUploader.setAdditionalData(JSON.stringify(\{tablename: oInput.getValue(), \r\n| &
|                           dataOption: oGroup.getSelectedIndex() \})) \r\n| &
|                       oFileUploader.upload() \r\n| &
|                    \}) \r\n| &
|                    let input = new sap.m.Input("tablename") \r\n| &
|                    input.placeAt("layout") \r\n| &
|                    input.setRequired(true) \r\n| &
|                    input.setWidth("600px") \r\n| &
|                    input.setPlaceholder("Order Number") \r\n| &
|                    input.setShowSuggestion(true) \r\n| &
|                    input.attachSuggest(function (oEvent)\{ \r\n| &
|                      jQuery.ajax(\{headers: \{ "ordernum": oEvent.getParameter("suggestValue") \r\n| &
|                          \}, \r\n| &
|                         error: function(oErr)\{ alert( JSON.stringify(oErr))\}, timeout: 30000, method:"GET",dataType: "json",success: function(myJSON) \{ \r\n| &
|                      let input = oCore.byId("tablename") \r\n| &
|                      input.destroySuggestionItems() \r\n| &
|                      for (var i = 0; i < myJSON.length; i++) \{ \r\n| &
|                          input.addSuggestionItem(new sap.ui.core.Item(\{ \r\n| &
|                              text: myJSON[i].TABLE_NAME  \r\n| &
|                          \})); \r\n| &
|                      \} \r\n| &
|                    \} \}) \r\n| &
|                    \}) \r\n| &
|                    line2.placeAt("layout") \r\n| &
|                    line3.placeAt("layout").setWidth\r\n| &
|                    line4.placeAt("layout") \r\n| &
|            \r\n| &
|                    let groupDataOptions = new sap.m.RadioButtonGroup("grpDataOptions") \r\n| &
|                    let lblGroupDataOptions = new sap.m.Label("lblDataOptions") \r\n| &
|                    lblGroupDataOptions.setLabelFor(groupDataOptions) \r\n| &
|                    lblGroupDataOptions.setText(" ") \r\n| &
|                    lblGroupDataOptions.placeAt("line3") \r\n| &
|                    groupDataOptions.placeAt("line4") \r\n| &
|                    rbAppend = new sap.m.RadioButton("rbAppend") \r\n| &
|                    rbReplace = new sap.m.RadioButton("rbReplace") \r\n| &
|                    rbAppend.setText("Append") \r\n| &
|                    rbReplace.setText("Replace") \r\n| &
|                    rbAppend.setGroupName("grpDataOptions") \r\n| &
|                    rbReplace.setGroupName("grpDataOptions") \r\n| &
|                    sap.ui.getCore().loadLibrary("sap.ui.unified", \{ \r\n| &
|                        async: true \r\n| &
|                    \}).then(() => \{ \r\n| &
|                        var fileUploader = new sap.ui.unified.FileUploader( \r\n| &
|                            "fileToUpload") \r\n| &
|                        fileUploader.setFileType("json") \r\n| &
|                        fileUploader.setWidth("100px") \r\n| &
|                        fileUploader.setStyle("Transparent")     \r\n| &
|                        fileUploader.setButtonText(" ") \r\n| &
|                        fileUploader.setStyle\r\n| &
|                        let param = new sap.ui.unified.FileUploaderParameter("uploadParam") \r\n| &
|                        param.setName("tablename") \r\n| &
|                        fileUploader.placeAt("line3") \r\n| &
|                        button.placeAt("line2") \r\n| &
|                        fileUploader.setPlaceholder( \r\n| &
|                            "SAP") \r\n| &
|                        fileUploader.attachUploadComplete(function (oEvent) \{ \r\n| &
|                           alert(oEvent.getParameters().response)  \r\n| &
|                       \})   \r\n| &
| \r\n| &
|                    \}) \r\n| &
|                \}) \r\n| &
|            \}) \r\n| &
|        \}) \r\n| &
|    </script> \r\n| &
|</head> \r\n| &
|<body class="sapUiBody"> \r\n| &
|    <div id="uiArea"></div> \r\n| &
|</body> \r\n| &
|</html>|.
  ENDMETHOD.
ENDCLASS.


