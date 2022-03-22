
CLASS zcl_vko_puchase_poc DEFINITION
PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF zasa_s_form,
             yy1_retentionrate_pdh  TYPE int4,
             yy1_zretentiondays_pdh TYPE int4,
             companycode            TYPE string,
             ordercurrency          TYPE string,
             invoicingparty         TYPE string,
             paymentterms           TYPE string,
             zpaymentterms          TYPE string,
           END OF zasa_s_form.

    TYPES: BEGIN OF  zasa_s_purchase_item,
             netpriceamount   TYPE int4,
             netpricequantity(5) TYPE p decimals 2,
             itemnumber       TYPE int4,
           END OF zasa_s_purchase_item.

    METHODS: constructor,
      get_html RETURNING VALUE(ui_html) TYPE string,
      get_input_field_value IMPORTING name         TYPE string
                                      dataref      TYPE data
                            RETURNING VALUE(value) TYPE string.
    DATA: gv_order_api_url TYPE string VALUE 'https://my305270.s4hana.ondemand.com:443/sap/opu/odata/sap/API_PURCHASEORDER_PROCESS_SRV',
          go_order_client  TYPE REF TO if_web_http_client,
          gv_order_num     TYPE string.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_vko_puchase_poc IMPLEMENTATION.
  METHOD if_http_service_extension~handle_request.

    CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>get ).

        DATA(sap_order_request) = request->get_header_field( 'ordernum' ).
        IF sap_order_request IS INITIAL.
          response->set_text( get_html(   ) ).
        ENDIF.

      WHEN CONV string( if_web_http_client=>post ).

*************************************************************************************
********AUTHORITY CHECKING, CONNECT OBJECT WITH USER ROLE TO USE*********************
*************************************************************************************
        "    AUTHORITY-CHECK OBJECT 'ZORDER_OBJ'
        "     ID 'ZORDER' FIELD '03'.
        "    IF sy-subrc <> 0.

        "response->set_status( i_code = if_web_http_status=>forbidden
        "                                i_reason = |You are not authorized!| ).
        " response->set_text( |You are not authorized!| ).
        "return.

        "    ENDIF.
***************************************************************************************

        go_order_client = cl_web_http_client_manager=>create_by_http_destination(
          i_destination = cl_http_destination_provider=>create_by_url( gv_order_api_url ) ).
        DATA header_json TYPE string.
        DATA(lo_req) = go_order_client->get_http_request(  ).
        lo_req->set_header_fields( VALUE #(
         ( name = 'Content-Type' value = 'application/json')
         ( name = 'Accept' value = 'application/json' )
         ( name = 'APIKey' value = 'hwTmbcPc1KimcX4jFm96rUR3ApgHngUs' )
         ) ).

        go_order_client->get_http_request( )->set_authorization_basic(
                                             i_username = 'API_USER'
                                             i_password = 'Welcome12345678901234567890!' ).
        "i_username = 'ZOSA'
        "i_password = 'WbsDMuaRLbMuxyCpvzEivmZKgj-PZBmSTZVJ9yqR' ).

        DATA lv_url TYPE string.

        " Unpack input field values such as tablename, dataoption, etc.
        DATA(ui_data) = request->get_form_field( `filetoupload-data` ).
        DATA(ui_dataref) = /ui2/cl_json=>generate( json = ui_data ).

        IF ui_dataref IS BOUND.
          ASSIGN ui_dataref->* TO FIELD-SYMBOL(<ui_dataref>).
          gv_order_num = me->get_input_field_value( name = `TABLENAME` dataref = <ui_dataref> ).
        ENDIF.

        lv_url = 'https://my305270-api.s4hana.ondemand.com:443/sap/opu/odata/sap/API_PURCHASEORDER_PROCESS_SRV/A_PurchaseOrder('''.
        CONCATENATE lv_url gv_order_num INTO lv_url.
        CONCATENATE lv_url ''')' INTO lv_url.
        "lv_url = 'https://my305270.lab.s4hana.cloud.sap/sap/opu/odata/sap/APS_IAM_BUSR_SRV/aps_iam_busr_ddl(''C5190879'')/to_AssignedBusinessRoles'.

        lo_req->set_uri_path( i_uri_path = lv_url ).
        TRY.
            DATA(lv_response) = go_order_client->execute( i_method = if_web_http_client=>get )->get_text( ).
            header_json = lv_response.

            DATA: header_table TYPE STANDARD TABLE OF zasa_s_form.
            DATA: item_table TYPE STANDARD TABLE OF zasa_s_purchase_item.

            CONCATENATE lv_url '/to_PurchaseOrderItem' INTO lv_url.
            lo_req->set_uri_path( i_uri_path = lv_url ).
            lv_response =  go_order_client->execute( i_method = if_web_http_client=>get )->get_text( ).

            "DATA(http_status) = go_order_client->get_header_field( i_name = '~status_code' ).
            "IF http_status = ''.

            "ENDIF.

            DATA lv_item_json TYPE string.
            DATA lv_str1 TYPE string.
            DATA lv_str2 TYPE string.
            DATA lv_str3 TYPE string.
            DATA lv_ret_rate TYPE string.
            DATA lv_ret_days TYPE string.
            DATA lv_company_code TYPE string.
            DATA lv_order_currency TYPE string.
            DATA lv_order_supplier TYPE string.
            DATA lv_splitted_header TYPE string.
            DATA lv_splitted_item TYPE string.
            DATA lv_net_price_amount TYPE string.
            DATA lv_net_quantity TYPE string.
            DATA lv_paid TYPE decfloat16.
            DATA lv_remain TYPE decfloat16.
            DATA lv_payment_term TYPE string.
            DATA lv_zpayment_term TYPE string.
            DATA lv_item_num TYPE string.
            DATA lv_item_quantity_unit TYPE string.
            lv_item_json = lv_response.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """""""""""""""""""""""Get header parameters from response """"""""""""""""
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            REPLACE ALL OCCURRENCES OF '"' IN header_json WITH ' '.
            SPLIT header_json AT 'YY1_RetentionRate_PDH:' INTO lv_str1 lv_str2.
            SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
            lv_ret_rate = lv_str1.
            SPLIT header_json AT 'YY1_ZRetentionDays_PDH:' INTO lv_str1 lv_str2.
            SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
            lv_ret_days = lv_str1.
            SPLIT header_json AT 'CompanyCode:' INTO lv_str1 lv_str2.
            SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
            lv_company_code = lv_str1.
            SPLIT header_json AT 'DocumentCurrency:' INTO lv_str1 lv_str2.
            SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
            lv_order_currency = lv_str1.
            SPLIT header_json AT 'Supplier:' INTO lv_str1 lv_str2.
            SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
            lv_order_supplier = lv_str1.
            SPLIT header_json AT 'PaymentTerms:' INTO lv_str1 lv_str2.
            SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
            lv_payment_term = lv_str1.
            SPLIT header_json AT 'YY1_Z_PAYMENT_TERM_PDH:' INTO lv_str1 lv_str2.
            SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
            lv_zpayment_term = lv_str1.

            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """"""""""""""""""""""""""""""""FIELD VALIDATION""""""""""""""""""""""""
            """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            IF lv_ret_rate EQ 0 OR lv_ret_days EQ 0 OR lv_zpayment_term EQ ' '.
              response->set_status( i_code   = if_web_http_status=>bad_request
                                    i_reason = |'Z' fields are empty. Impossible to process!| ).
              response->set_text( |Specified Purchase Order { gv_order_num } is not relevant invoicing with retention. Retention % and Retention Payment Term are not specified.| ).
              RETURN.
            ENDIF.

            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """""""""""""""""""""""Form json for header"""""""""""""""""""""""""""""""""""""""""""
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            lv_splitted_header = |[\{"yy1_retentionrate_pdh":"{ lv_ret_rate }","yy1_zretentiondays_pdh":"{ lv_ret_days }","companycode":"{ lv_company_code }","ordercurrency":"{ lv_order_currency }","invoicingparty":"{ lv_order_supplier }",| &
            |"paymentterms":"{ lv_payment_term }","zpaymentterms":"{ lv_zpayment_term }"\}]|.

            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """"""""""Form json for item""""""""""""""""""""""""""""""""""""""""""""""""""
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            DATA lv_is_end TYPE int1 VALUE 0.
            DATA lv_counter TYPE int4.
            REPLACE ALL OCCURRENCES OF '"' IN lv_item_json WITH ' '.
            REPLACE ALL OCCURRENCES OF '''' IN lv_item_json WITH ' '.
            lv_str2 = lv_item_json.
            lv_splitted_item = '['.
            WHILE lv_is_end EQ 0." To count sum of order`s items
              FIND 'metadata' IN lv_str2.
              IF sy-subrc EQ 0.
                IF lv_splitted_item <> '['.
                  CONCATENATE lv_splitted_item ',' INTO lv_splitted_item.
                ENDIF.
                SPLIT lv_str2 AT 'metadata' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT 'PurchaseOrderItem=' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT ')' INTO lv_str1 lv_str3.
                lv_item_num = lv_str1.
                SPLIT lv_str2 AT 'OrderQuantity:' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                lv_net_quantity = lv_str1.
                SPLIT lv_str2 AT 'PurchaseOrderQuantityUnit:' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                lv_item_quantity_unit = lv_str1.
                SPLIT lv_str2 AT 'NetPriceAmount:' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                lv_net_price_amount = lv_str1.

                lv_str1 = |\{"netpriceamount":"{ lv_net_price_amount }","netpricequantity":"{ lv_net_quantity }","itemnumber":"{ lv_item_num }"\}|.
                CONCATENATE lv_splitted_item lv_str1 INTO lv_splitted_item.
              ELSE.
                lv_is_end = 1.
                CONCATENATE lv_splitted_item ']' INTO lv_splitted_item.
              ENDIF.
            ENDWHILE.

            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """"""""""Jsons deserializing""""""""""""""""""""""""""""""""""""""""""""""""""
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            /ui2/cl_json=>deserialize( EXPORTING json        = lv_splitted_header
                                                 pretty_name = /ui2/cl_json=>pretty_mode-none
                                       CHANGING  data        = header_table ).
            /ui2/cl_json=>deserialize( EXPORTING json        = lv_splitted_item
                                                 pretty_name = /ui2/cl_json=>pretty_mode-none
                                       CHANGING  data        = item_table ).
            DATA lv_amount_sum TYPE int4 VALUE 0.
            DATA lv_quantity_sum TYPE int4 VALUE 0.
            LOOP AT item_table INTO DATA(ls_item_table).
              lv_amount_sum = lv_amount_sum + ls_item_table-netpriceamount * ls_item_table-netpricequantity.
              lv_quantity_sum = lv_quantity_sum + ls_item_table-netpricequantity.
            ENDLOOP.
            "lv_paid procent
            lv_paid = lv_amount_sum * header_table[ 1 ]-yy1_retentionrate_pdh / 100.
            "lv_remain to pay
            lv_remain = lv_amount_sum - lv_paid.
        ENDTRY.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        """"""""""""Get request to get csrf token""""""""""""""""""""""""""""""""""""""""""""
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DATA(lo_dest) = cl_http_destination_provider=>create_by_url( 'https://my305270.s4hana.ondemand.com:443/sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice' ).
        DATA(lo_invoice_client) = cl_web_http_client_manager=>create_by_http_destination(
          i_destination = lo_dest ).

        DATA(lo_req_invoice) = lo_invoice_client->get_http_request(  ).
        lo_invoice_client->get_http_request( )->set_authorization_basic(
                                            i_username = 'API_USER'
                                            i_password = 'Welcome12345678901234567890!' ).
        "i_username = 'ZOSA'
        "i_password = 'WbsDMuaRLbMuxyCpvzEivmZKgj-PZBmSTZVJ9yqR' ).

        lo_invoice_client->set_authn_mode( if_a4c_cp_service=>service_specific ).

        lo_req_invoice->set_header_fields( VALUE #(
         ( name = 'Content-Type' value = 'application/json')
         ( name = 'Accept' value = 'application/json' )
         ( name = 'APIKey' value = 'hwTmbcPc1KimcX4jFm96rUR3ApgHngUs' )
         ( name = 'X-CSRF-Token' value = 'fetch' )
         ) ).

        DATA(lv_token) =  lo_invoice_client->execute( i_method = if_web_http_client=>get )->get_header_field( 'X-CSRF-Token' ).

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        """"""""""""Post with received token"""""""""""""""""""""""""""""""""""""""""""
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        lo_req_invoice->set_header_field( EXPORTING i_name = 'x-csrf-token' i_value = lv_token ).
        DATA(lv_header_response) = lo_req_invoice->get_header_fields( ).
        DATA lv_body TYPE string.
        DATA lo_value_r TYPE REF TO if_web_http_requesT.
        DATA lv_current_time TYPE tzntstmpl.
        DATA lv_tstmp1 TYPE p.
        DATA lv_tstmp2 TYPE p.
        DATA lv_secs TYPE tzntstmpl.
        "ABAP Timestamp into Unix timestamp
        GET TIME STAMP FIELD lv_current_time.
        lv_tstmp1 = lv_current_time.
        lv_tstmp2 = '19700101000000'.

        TRY.
            lv_secs = cl_abap_tstmp=>subtract(
              tstmp1 = lv_tstmp1
              tstmp2 = lv_tstmp2
            ).
          CATCH cx_parameter_invalid_range .
          CATCH cx_parameter_invalid_type .
        ENDTRY.
        DATA lv_unix_time TYPE string.
        lv_str1 = lv_secs.
        lv_str2 = lv_current_time.
        lv_unix_time = lv_str1(10) && lv_str2+15(3).

        lv_body = |\{"CompanyCode":"{ lv_company_code }","InvoicingParty": "{ lv_order_supplier }","DocumentDate":"/Date({ lv_unix_time })/",\r\n| &
                  |          "SupplierInvoiceStatus" : " ","PostingDate":"/Date({ lv_unix_time })/","DocumentCurrency":"{ lv_order_currency }",\r\n| &
                  |          "InvoiceGrossAmount":"{ lv_remain }","PaymentTerms":"{ lv_payment_term }","DocumentHeaderText":"SideBySide","SupplierInvoiceIDByInvcgParty":"{ lv_unix_time }",\r\n| &
                  |"to_SupplierInvoiceTax": \{"results": [\{"TaxCode":"I0","DocumentCurrency": "{ lv_order_currency }" \}]\}| &
                  |,"to_SuplrInvcItemPurOrdRef": \{"results": [|.
        lv_counter = 0.
        DATA lv_item_sum type int4.
        DATA lv_current_quantity(5) type p decimals 2.

        LOOP AT item_table INTO ls_item_table.
        lv_item_sum = ls_item_table-netpricequantity * ls_item_table-netpriceamount.
        IF lv_counter > 0.
        concatenate lv_body ',' INTO lv_body.
        ENDIF.
        lv_counter = lv_counter + 1.
        lv_current_quantity = ls_item_table-netpricequantity - ls_item_table-netpricequantity * header_table[ 1 ]-yy1_retentionrate_pdh / 100.
        lv_str1 = |\{"SupplierInvoiceItem":"{ lv_counter }","PurchaseOrder":"{ gv_order_num }","PurchaseOrderQuantityUnit":"{ lv_item_quantity_unit }",| &
                  |"PurchaseOrderItem":"{ ls_item_table-itemnumber }","SupplierInvoiceItemAmount":"{ lv_item_sum - ls_item_table-netpricequantity * ls_item_table-netpriceamount * header_table[ 1 ]-yy1_retentionrate_pdh / 100   }",| &
                  |"QuantityInPurchaseOrderUnit": "{ lv_current_quantity  }", "DocumentCurrency": "{ lv_order_currency }"\}|."]\}\}|.
        concatenate lv_body lv_str1 into lv_body.
        ENDLOOP.
        concatenate lv_body ']}}' into lv_body.

        TRY.
            lo_req_invoice->set_text(
              EXPORTING
                i_text = lv_body ).
          CATCH cx_web_message_error.
        ENDTRY.

        DATA(lv_post_resp_paid) =  lo_invoice_client->execute( i_method = if_web_http_client=>post )->get_text( ).

        "ABAP Timestamp into Unix timestamp
        GET TIME STAMP FIELD lv_current_time.
        lv_tstmp1 = lv_current_time.
        lv_tstmp2 = '19700101000000'.

        TRY.
            lv_secs = cl_abap_tstmp=>subtract(
              tstmp1 = lv_tstmp1
              tstmp2 = lv_tstmp2
            ).
          CATCH cx_parameter_invalid_range .
          CATCH cx_parameter_invalid_type .
        ENDTRY.
        lv_str1 = lv_secs.
        lv_str2 = lv_current_time.
        lv_unix_time = lv_str1(10) && lv_str2+15(3).

       lv_body = |\{"CompanyCode":"{ lv_company_code }","InvoicingParty": "{ lv_order_supplier }","DocumentDate":"/Date({ lv_unix_time })/",\r\n| &
                  |          "SupplierInvoiceStatus" : " ","PostingDate":"/Date({ lv_unix_time })/","DocumentCurrency":"{ lv_order_currency }",\r\n| &
                  |"CashDiscount1Percent":"0","CashDiscount1Days":"0","CashDiscount2Percent":"0","CashDiscount2Days":"0","NetPaymentDays":"0","DueCalculationBaseDate":"/Date({ lv_unix_time })/",\r\n| &
                  |          "InvoiceGrossAmount":"{ lv_paid }","PaymentTerms":"{ lv_payment_term }","DocumentHeaderText":"SideBySide","SupplierInvoiceIDByInvcgParty":"{ lv_unix_time }",\r\n| &
                  |"to_SupplierInvoiceTax": \{"results": [\{"TaxCode":"I0","DocumentCurrency": "{ lv_order_currency }" \}]\}| &
                  |,"to_SuplrInvcItemPurOrdRef": \{"results": [|.
        lv_counter = 0.

        LOOP AT item_table INTO ls_item_table.
        IF lv_counter > 0.
        concatenate lv_body ',' INTO lv_body.
        ENDIF.
        lv_counter = lv_counter + 1.
        lv_current_quantity = ls_item_table-netpricequantity * header_table[ 1 ]-yy1_retentionrate_pdh / 100.
        lv_str1 = |\{"SupplierInvoiceItem":"{ lv_counter }","PurchaseOrder":"{ gv_order_num }","PurchaseOrderQuantityUnit":"{ lv_item_quantity_unit }",| &
                  |"PurchaseOrderItem":"{ ls_item_table-itemnumber }","SupplierInvoiceItemAmount":"{ ls_item_table-netpricequantity * ls_item_table-netpriceamount * header_table[ 1 ]-yy1_retentionrate_pdh / 100   }",| &
                  |"QuantityInPurchaseOrderUnit": "{ lv_current_quantity }", "DocumentCurrency": "{ lv_order_currency }"\}|."]\}\}|.
        concatenate lv_body lv_str1 into lv_body.
        ENDLOOP.
        concatenate lv_body ']}}' into lv_body.

        TRY.
            lo_req_invoice->set_text(
              EXPORTING
                i_text = lv_body ).
          CATCH cx_web_message_error.
        ENDTRY.
        DATA(lv_post_resp_remain) =  lo_invoice_client->execute( i_method = if_web_http_client=>post )->get_text( ).
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        """"""""""""""""PRINT FISCAL YEAR AND NUMBER OF CREATED INVOICES"""""""""""""""""""""""
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DATA lv_paid_num TYPE string.
        DATA lv_remain_num TYPE string.
        DATA lv_paid_year TYPE string.
        DATA lv_remain_year TYPE string.
        REPLACE ALL OCCURRENCES OF '"' IN lv_post_resp_paid WITH ' '.
        SPLIT lv_post_resp_paid AT 'SupplierInvoice:' INTO lv_str1 lv_str2.
        SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
        lv_paid_num = lv_str1.
        SPLIT lv_post_resp_paid AT 'FiscalYear:' INTO lv_str1 lv_str2.
        SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
        lv_paid_year = lv_str1.
        REPLACE ALL OCCURRENCES OF '"' IN lv_post_resp_remain WITH ' '.
        SPLIT lv_post_resp_remain AT 'SupplierInvoice:' INTO lv_str1 lv_str2.
        SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
        lv_remain_num = lv_str1.
        SPLIT lv_post_resp_remain AT 'FiscalYear:' INTO lv_str1 lv_str2.
        SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
        lv_remain_year = lv_str1.
        response->set_text( |Invoicing procedure completed for Purchase Order #{ gv_order_num } .\r\n| &
                            |Following accounting documents were created:\r\n| &
                            |Supplier Invoice: { lv_paid_num }\r\n| &
                            |Supplier Invoice with retention: { lv_remain_num } | ).
    ENDCASE.

  ENDMETHOD.

  METHOD constructor.
    "    go_order_client = cl_web_http_client_manager=>create_by_http_destination(
    "   i_destination = cl_http_destination_provider=>create_by_url( gv_order_api_url ) ).
    "  lo_invoice_client = cl_web_http_client_manager=>create_by_http_destination(
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
  |<!DOCTYPE HTML> \n| &&
     |<html> \n| &&
     |<head> \n| &&
     |    <meta http-equiv="X-UA-Compatible" content="IE=edge"> \n| &&
     |    <meta http-equiv='Content-Type' content='text/html;charset=UTF-8' /> \n| &&
     |    <title>Purchase Order Processing</title> \n| &&
     |    <script id="sap-ui-bootstrap" src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js" \n| &&
     |        data-sap-ui-theme="base" data-sap-ui-xx-bindingSyntax="complex" data-sap-ui-compatVersion="edge" \n| &&
     |        data-sap-ui-async="true"> \n| &&
     |    </script> \n| &&
     |    <script> \n| &&
     |        sap.ui.require(['sap/ui/core/Core'], (oCore, ) => \{ \n| &&
     | \n| &&
     |            sap.ui.getCore().loadLibrary("sap.f", \{ \n| &&
     |                async: true \n| &&
     |            \}).then(() => \{ \n| &&
     |                let shell = new sap.f.ShellBar("shell") \n| &&
     |                shell.setTitle("Purchase Order Processing") \n| &&
*     |                shell.setShowCopilot(true) \n| &&
*     |                shell.setShowSearch(true) \n| &&
     |                shell.setShowNotifications(true) \n| &&
     |                shell.setShowProductSwitcher(true) \n| &&
     |                shell.placeAt("uiArea") \n| &&
     |                sap.ui.getCore().loadLibrary("sap.ui.layout", \{ \n| &&
     |                    async: true \n| &&
     |                \}).then(() => \{ \n| &&
     |                    let layout = new sap.ui.layout.VerticalLayout("layout") \n| &&
     |                    layout.placeAt("uiArea") \n| &&
     |                    let line2 = new sap.ui.layout.HorizontalLayout("line2") \n| &&
     |                    let line3 = new sap.ui.layout.HorizontalLayout("line3") \n| &&
     |                    let line4 = new sap.ui.layout.HorizontalLayout("line4") \n| &&
     |                    sap.ui.getCore().loadLibrary("sap.m", \{ \n| &&
     |                        async: true \n| &&
     |                    \}).then(() => \{\}) \n| &&
     |                    let button = new sap.m.Button("button") \n| &&
     |                    button.setText("Create Invoices") \n| &&
     |                    button.setWidth("400px") \n| &&
     |                    button.attachPress(function () \{ \n| &&
     |                    let oFileUploader = oCore.byId("fileToUpload") \n| &&
  "    |                        if (!oFileUploader.getValue()) \{ \n| &&
  "   |                            sap.m.MessagLToast.show("Choose a file first") \n| &&
  "   |                            return \n| &&
  "  |                        \} \n| &&
     |                        let oInput = oCore.byId("tablename") \n| &&
     |                        let oGroup = oCore.byId("grpDataOptions") \n| &&
     |                        if (!oInput.getValue())\{ \n| &&
     |                            sap.m.MessageToast.show("Order number is Required") \n| &&
     |                            return \n| &&
     |                        \} \n| &&
     |                       let param = oCore.byId("uploadParam") \n| &&
     |                       param.setValue( oInput.getValue() ) \n| &&
     |                       oFileUploader.getParameters() \n| &&
     |                       oFileUploader.setAdditionalData(JSON.stringify(\{tablename: oInput.getValue(), \n| &&
     |                        dataOption: oGroup.getSelectedIndex() \})) \n| &&
     |                       oFileUploader.upload() \n| &&
     |                    \}) \n| &&
     |                    let input = new sap.m.Input("tablename") \n| &&
     |                    input.placeAt("layout") \n| &&
     |                    input.setRequired(true) \n| &&
     |                    input.setWidth("400px") \n| &&
     |                    input.setPlaceholder("Order Number") \n| &&
     |                    input.setShowSuggestion(true) \n| &&
     |                    input.attachSuggest(function (oEvent)\{ \n| &&
     |                      jQuery.ajax(\{headers: \{ "ordernum": oEvent.getParameter("suggestValue") \n | &&
     |                          \}, \n| &&
     |                         error: function(oErr)\{ alert( JSON.stringify(oErr))\}, timeout: 30000, method:"GET",dataType: "json",success: function(myJSON) \{ \n| &&
  "   |                      alert( 'test' ) \n| &&
     |                      let input = oCore.byId("tablename") \n | &&
     |                      input.destroySuggestionItems() \n | &&
     |                      for (var i = 0; i < myJSON.length; i++) \{ \n | &&
     |                          input.addSuggestionItem(new sap.ui.core.Item(\{ \n| &&
     |                              text: myJSON[i].TABLE_NAME  \n| &&
     |                          \})); \n| &&
     |                      \} \n| &&
     |                    \} \}) \n| &&
     |                    \}) \n| &&
     |                    line2.placeAt("layout") \n| &&
     |                    line3.placeAt("layout") \n| &&
     |                    line4.placeAt("layout") \n| &&
*     |                    line4.visible("true")  \n| &&
     |                    let groupDataOptions = new sap.m.RadioButtonGroup("grpDataOptions") \n| &&
     |                    let lblGroupDataOptions = new sap.m.Label("lblDataOptions") \n| &&
     |                    lblGroupDataOptions.setLabelFor(groupDataOptions) \n| &&
     |                    lblGroupDataOptions.setText(" ") \n| &&
     |                     lblGroupDataOptions.placeAt("line3") \n| &&
     |                    groupDataOptions.placeAt("line3") \n| &&
     |                    rbAppend = new sap.m.RadioButton("rbAppend") \n| &&
     |                    rbReplace = new sap.m.RadioButton("rbReplace") \n| &&
     |                    rbAppend.setText("Append") \n| &&
     |                    rbReplace.setText("Replace") \n| &&
     |                    rbAppend.setGroupName("grpDataOptions") \n| &&
     |                    rbReplace.setGroupName("grpDataOptions") \n| &&
     |                    sap.ui.getCore().loadLibrary("sap.ui.unified", \{ \n| &&
     |                        async: true \n| &&
     |                    \}).then(() => \{ \n| &&
     |                        var fileUploader = new sap.ui.unified.FileUploader( \n| &&
     |                            "fileToUpload") \n| &&
    |                        fileUploader.setFileType("json") \n| &&
     |                        fileUploader.setWidth("1px") \n| &&
*     |                        fileUploader.Width("1px") \n| &&
     |                        let param = new sap.ui.unified.FileUploaderParameter("uploadParam") \n| &&
     |                        param.setName("tablename") \n| &&
     |                        fileUploader.placeAt("line3") \n| &&
     |                        button.placeAt("line2") \n| &
     |                       fileUploader.setStyle("Transparent")        \n| &&
           |                    fileUploader.setButtonText(" ")        \n| &&
*             |                   fileUploader.setVisible(false) \n| &&
     |                        fileUploader.attachUploadComplete(function (oEvent) \{ \n| &&
     |                           alert(oEvent.getParameters().response)  \n| &&
     |                       \})   \n| &&
     | \n| &&
     |                    \}) \n| &&
     |                \}) \n| &&
     |            \}) \n| &&
     |        \}) \n| &&
     |    </script> \n| &&
     |</head> \n| &&
     |<body class="sapUiBody"> \n| &&
     |    <div id="uiArea"></div> \n| &&
     |</body> \n| &&
     | \n| &&
     |</html> |.
  ENDMETHOD.
ENDCLASS.

