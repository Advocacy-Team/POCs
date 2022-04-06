CLASS zcl_asa_api_po DEFINITION
PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF lty_order_ret,
             yy1_retentionrate_pdh  TYPE int4,
             yy1_zretentiondays_pdh TYPE int4,
             companycode            TYPE string,
             ordercurrency          TYPE string,
             invoicingparty         TYPE string,
             paymentterms           TYPE string,
             zpaymentterms          TYPE string,
           END OF lty_order_ret.

    TYPES: BEGIN OF  lty_purchase_item,
             netpriceamount(13)        TYPE p DECIMALS 2,
             netpricequantity(16)      TYPE p DECIMALS 3,
             itemnumber                TYPE int4,
             PurchaseOrderQuantityUnit TYPE char3,
           END OF lty_purchase_item.

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

CLASS zcl_asa_api_po IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.

    CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>get ).

        DATA(sap_order_request) = request->get_header_field( 'ordernum' ).
        IF sap_order_request IS INITIAL.
          response->set_text( get_html(   ) ).
        ENDIF.

      WHEN CONV string( if_web_http_client=>post ).
        response->set_text( |Invoicing started . | ).

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

        lo_req->set_uri_path( i_uri_path = lv_url ).
        TRY.
            DATA(lv_response) = go_order_client->execute( i_method = if_web_http_client=>get )->get_text( ).
            header_json = lv_response.

            DATA: header_table TYPE STANDARD TABLE OF lty_order_ret.
            DATA: item_table TYPE STANDARD TABLE OF lty_purchase_item.

            CONCATENATE lv_url '/to_PurchaseOrderItem' INTO lv_url.
            lo_req->set_uri_path( i_uri_path = lv_url ).
            lv_response =  go_order_client->execute( i_method = if_web_http_client=>get )->get_text( ).

            DATA lv_item_json TYPE string.
            DATA lv_item_json_gds TYPE string.
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
            """""""""""""""""""""""Get parameters from Goods Receiver """"""""""""""""""""""""""
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            CONSTANTS: lc_url_goods TYPE string VALUE 'https://my305270-api.s4hana.ondemand.com:443/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentItem'.
            " sap_order_request PurchaseOrder
            DATA: gv_order_api_url_gds TYPE string VALUE 'https://my305270.s4hana.ondemand.com:443/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV',
                  go_order_client_gds  TYPE REF TO if_web_http_client,
                  gv_order_num_gds     TYPE string.

            go_order_client_gds = cl_web_http_client_manager=>create_by_http_destination(
              i_destination = cl_http_destination_provider=>create_by_url( gv_order_api_url_gds ) ).

            DATA header_json_gds TYPE string.
            DATA(lo_req_gds) = go_order_client_gds->get_http_request(  ).

            lo_req_gds->set_header_fields( VALUE #(
             ( name = 'Content-Type' value = 'application/json')
             ( name = 'Accept' value = 'application/json' )
             ( name = 'APIKey' value = 'hwTmbcPc1KimcX4jFm96rUR3ApgHngUs' )
             ) ).

            go_order_client_gds->get_http_request( )->set_authorization_basic(
                                                 i_username = 'ZOVO_TEST'
                                                 i_password = 'JaKshGkcbSov~ukPYs4VXZwbVYgvTAVkLRBbuglj' ).

            DATA lv_url_gds TYPE string.

            " Unpack input field values such as tablename, dataoption, etc.
            DATA(ui_data_gds) = request->get_form_field( `filetoupload-data` ).
            DATA(ui_dataref_gds) = /ui2/cl_json=>generate( json = ui_data_gds ).

            IF ui_dataref_gds IS BOUND.
              ASSIGN ui_dataref_gds->* TO FIELD-SYMBOL(<ui_dataref_gds>).
              gv_order_num_gds = me->get_input_field_value( name = `TABLENAME` dataref = <ui_dataref_gds> ).
            ENDIF.

            CONCATENATE lc_url_goods `?$filter=PurchaseOrder eq'` gv_order_num_gds `'` INTO lv_url_gds.

            lo_req_gds->set_uri_path( i_uri_path = lv_url_gds ).
            TRY.
                DATA(lv_response_gds) = go_order_client_gds->execute( i_method = if_web_http_client=>get )->get_text( ).
                header_json_gds = lv_response_gds.

                """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                """"""""""Get from json Goods receipt items"""""""""""""""""""""""""""""""""""""""""
                """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                lv_item_json_gds = lv_response_gds.

                TYPES: BEGIN OF ls_item_qty,
                         PurchaseOrderItem   TYPE int4, "string,
                         QuantityInBaseUnit  TYPE string,
                         QuantityInEntryUnit TYPE string,
                       END OF ls_item_qty.

                DATA: lt_item_qty TYPE STANDARD TABLE OF ls_item_qty.

                DATA lv_is_end TYPE int1 VALUE 0.
                DATA lv_counter TYPE int4.
                lv_str2 = lv_item_json_gds.
                REPLACE ALL OCCURRENCES OF '"' IN lv_str2 WITH ' '.
                REPLACE ALL OCCURRENCES OF '''' IN lv_str2 WITH ' '.
                lv_splitted_item = '['.
                WHILE lv_is_end EQ 0." To count sum of order`s items
                  FIND 'metadata' IN lv_str2.
                  IF sy-subrc EQ 0.
                    APPEND INITIAL LINE TO lt_item_qty ASSIGNING FIELD-SYMBOL(<fs_item_qty>).
                    IF lv_splitted_item <> '['.
                      CONCATENATE lv_splitted_item ',' INTO lv_splitted_item.
                    ENDIF.
                    SPLIT lv_str2 AT 'metadata' INTO lv_str1 lv_str2.
                    SPLIT lv_str2 AT 'PurchaseOrderItem:' INTO lv_str1 lv_str2.
                    SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                    <fs_item_qty>-purchaseorderitem = lv_str1.
                    SPLIT lv_str2 AT 'QuantityInBaseUnit:' INTO lv_str1 lv_str2.
                    SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                    <fs_item_qty>-quantityinbaseunit = lv_str1.
                    SPLIT lv_str2 AT 'QuantityInEntryUnit:' INTO lv_str1 lv_str2.
                    SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                    <fs_item_qty>-quantityinentryunit = lv_str1.
                  ELSE.
                    lv_is_end = 1.
                    UNASSIGN <fs_item_qty>.
                  ENDIF.
                ENDWHILE.

                SORT lt_item_qty BY purchaseorderitem.
                DATA(lt_item_qty_un) = lt_item_qty.
                DELETE ADJACENT DUPLICATES FROM lt_item_qty COMPARING purchaseorderitem.
                LOOP AT lt_item_qty ASSIGNING <fs_item_qty>.
                  CLEAR: <fs_item_qty>-quantityinbaseunit, <fs_item_qty>-quantityinentryunit.
                  LOOP AT lt_item_qty_un ASSIGNING FIELD-SYMBOL(<fs_item_qty_un>)
                    WHERE purchaseorderitem = <fs_item_qty>-purchaseorderitem.
                    <fs_item_qty>-quantityinbaseunit = <fs_item_qty>-quantityinbaseunit + <fs_item_qty_un>-quantityinbaseunit.
                    <fs_item_qty>-quantityinentryunit = <fs_item_qty>-quantityinentryunit + <fs_item_qty_un>-quantityinentryunit.
                  ENDLOOP.
                ENDLOOP.

                UNASSIGN: <fs_item_qty>, <fs_item_qty_un>.

              CATCH cx_web_message_error.
            ENDTRY.

            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """""""""""""""""""""""Get parameters from Supplier Invoices""""""""""""""""""""""""
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            CONSTANTS: lc_url_inv TYPE string VALUE 'https://my305270.s4hana.ondemand.com:443/sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV/A_SuplrInvcItemPurOrdRef'.
            DATA: gv_order_api_url_inv TYPE string VALUE 'https://my305270.s4hana.ondemand.com:443/sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV',
                  go_order_client_inv  TYPE REF TO if_web_http_client,
                  gv_order_num_inv     TYPE string.

            go_order_client_inv = cl_web_http_client_manager=>create_by_http_destination(
              i_destination = cl_http_destination_provider=>create_by_url( gv_order_api_url_inv ) ).

            DATA item_json_inv TYPE string.
            DATA(lo_req_inv) = go_order_client_inv->get_http_request(  ).

            lo_req_inv->set_header_fields( VALUE #(
             ( name = 'Content-Type' value = 'application/json')
             ( name = 'Accept' value = 'application/json' )
             ( name = 'APIKey' value = 'hwTmbcPc1KimcX4jFm96rUR3ApgHngUs' )
             ) ).

            go_order_client_inv->get_http_request( )->set_authorization_basic(
                                                 i_username = 'API_USER'
                                                 i_password = 'Welcome12345678901234567890!' ).

            DATA lv_url_inv TYPE string.

            " Unpack input field values such as tablename, dataoption, etc.
            DATA(ui_data_inv) = request->get_form_field( `filetoupload-data` ).
            DATA(ui_dataref_inv) = /ui2/cl_json=>generate( json = ui_data_inv ).

            IF ui_dataref_inv IS BOUND.
              ASSIGN ui_dataref_inv->* TO FIELD-SYMBOL(<ui_dataref_inv>).
              gv_order_num_inv = me->get_input_field_value( name = `TABLENAME` dataref = <ui_dataref_inv> ).
            ENDIF.

            CONCATENATE lc_url_inv `?$filter=PurchaseOrder eq'` gv_order_num_inv `'` INTO lv_url_inv.

            lo_req_inv->set_uri_path( i_uri_path = lv_url_inv ).
            TRY.
                DATA(lv_response_inv) = go_order_client_inv->execute( i_method = if_web_http_client=>get )->get_text( ).
                item_json_inv = lv_response_inv.

              CATCH cx_web_message_error.
            ENDTRY.

            CLEAR: lv_is_end, lv_counter, lv_splitted_item, lv_str3, lv_str2, lv_str1, lv_item_num, lv_item_quantity_unit.

            lv_str2 = item_json_inv.
            REPLACE ALL OCCURRENCES OF '"' IN lv_str2 WITH ' '.
            REPLACE ALL OCCURRENCES OF '''' IN lv_str2 WITH ' '.

            WHILE lv_is_end EQ 0." To count sum of order`s items
              FIND 'metadata' IN lv_str2.
              IF sy-subrc EQ 0.
                IF lv_splitted_item <> '['.
                  CONCATENATE lv_splitted_item ',' INTO lv_splitted_item.
                ENDIF.
                SPLIT lv_str2 AT 'metadata' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT 'PurchaseOrderItem:' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                lv_item_num = lv_str1.
                SPLIT lv_str2 AT 'QuantityInPurchaseOrderUnit:' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                lv_item_quantity_unit = lv_str1.

                READ TABLE lt_item_qty ASSIGNING FIELD-SYMBOL(<fs_item_inv>) WITH KEY purchaseorderitem = lv_item_num.
                IF sy-subrc IS INITIAL.
                  <fs_item_inv>-quantityinbaseunit = <fs_item_inv>-quantityinbaseunit - lv_item_quantity_unit.
                  <fs_item_inv>-quantityinentryunit = <fs_item_inv>-quantityinentryunit - lv_item_quantity_unit.
                ENDIF.
              ELSE.
                lv_is_end = 1.
              ENDIF.
            ENDWHILE.

            DELETE lt_item_qty WHERE quantityinbaseunit EQ `0 ` OR quantityinbaseunit EQ `0.000` .

            IF lt_item_qty IS INITIAL.
              response->set_text( |There are no any goods receipt for Purchase Order { gv_order_num_gds }| ).
              RETURN.
            ENDIF.

            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """"""""""Form json for item""""""""""""""""""""""""""""""""""""""""""""""""""
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            CLEAR: lv_is_end, lv_counter, lv_splitted_item, lv_str2, lv_str1, lv_item_num, lv_item_quantity_unit.

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
                READ TABLE lt_item_qty WITH KEY purchaseorderitem = lv_item_num INTO DATA(ls_item_num).
                IF sy-subrc IS INITIAL.
                  lv_net_quantity = ls_item_num-quantityinbaseunit.
                ELSE.
                  CONTINUE.
                ENDIF.
                SPLIT lv_str2 AT 'PurchaseOrderQuantityUnit:' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                lv_item_quantity_unit = lv_str1.
                SPLIT lv_str2 AT 'NetPriceAmount:' INTO lv_str1 lv_str2.
                SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
                lv_net_price_amount = lv_str1.

                lv_str1 = |\{"netpriceamount":"{ lv_net_price_amount }","netpricequantity":"{ lv_net_quantity }","itemnumber":"{ lv_item_num }","PurchaseOrderQuantityUnit":"{ lv_item_quantity_unit }"\}|.
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
            DATA lv_amount_sum(13)  TYPE p DECIMALS 2 VALUE 0.
            DATA lv_quantity_sum(16) TYPE p DECIMALS 3 VALUE 0.
            LOOP AT item_table INTO DATA(ls_item_table).
              lv_amount_sum = lv_amount_sum + ls_item_table-netpriceamount * ls_item_table-netpricequantity.
              lv_quantity_sum = lv_quantity_sum + ls_item_table-netpricequantity.
            ENDLOOP.
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

        DATA: lv_item_sum(13)             TYPE p DECIMALS 2,
              lv_current_quantity(16)     TYPE p DECIMALS 3,
              lv_current_quantity_brd(16) TYPE p DECIMALS 4,
              lv_netpriceamount(13)       TYPE p DECIMALS 2.

        LOOP AT item_table INTO ls_item_table.
          IF line_exists( lt_item_qty[ purchaseorderitem = ls_item_table-itemnumber ] ).
            lv_current_quantity_brd = ls_item_table-netpricequantity * header_table[ 1 ]-yy1_retentionrate_pdh / 100.
            IF lv_current_quantity_brd LT '0.001'.
              lv_paid = lv_paid + ls_item_table-netpriceamount * ls_item_table-netpricequantity.
            ELSE.
              lv_paid = lv_paid + ls_item_table-netpriceamount * ls_item_table-netpricequantity * header_table[ 1 ]-yy1_retentionrate_pdh / 100.
              lv_remain = lv_remain + ls_item_table-netpriceamount * ls_item_table-netpricequantity * ( 100 - header_table[ 1 ]-yy1_retentionrate_pdh ) / 100.
            ENDIF.
          ENDIF.
        ENDLOOP.

        lv_body = |\{"CompanyCode":"{ lv_company_code }","InvoicingParty": "{ lv_order_supplier }","DocumentDate":"/Date({ lv_unix_time })/",\r\n| &
                  |          "SupplierInvoiceStatus" : " ","PostingDate":"/Date({ lv_unix_time })/","DocumentCurrency":"{ lv_order_currency }",\r\n| &
                  |          "InvoiceGrossAmount":"{ lv_remain }","PaymentTerms":"{ lv_payment_term }","DocumentHeaderText":"SideBySide","SupplierInvoiceIDByInvcgParty":"{ lv_unix_time }",\r\n| &
                  |"to_SupplierInvoiceTax": \{"results": [\{"TaxCode":"I0","DocumentCurrency": "{ lv_order_currency }" \}]\}| &
                  |,"to_SuplrInvcItemPurOrdRef": \{"results": [|.

        lv_counter = 0.

        LOOP AT item_table INTO ls_item_table.
          IF line_exists( lt_item_qty[ purchaseorderitem = ls_item_table-itemnumber ] ).

            lv_current_quantity_brd = ls_item_table-netpricequantity * header_table[ 1 ]-yy1_retentionrate_pdh / 100.
            IF lv_current_quantity_brd LT '0.001'.
              CONTINUE.
            ENDIF.

            lv_item_sum = ls_item_table-netpricequantity * ls_item_table-netpriceamount.
            IF lv_counter > 0.
              CONCATENATE lv_body ',' INTO lv_body.
            ENDIF.
            lv_counter = lv_counter + 1.
            lv_current_quantity = ls_item_table-netpricequantity - ls_item_table-netpricequantity * header_table[ 1 ]-yy1_retentionrate_pdh / 100.
            lv_str1 = |\{"SupplierInvoiceItem":"{ lv_counter }","PurchaseOrder":"{ gv_order_num }","PurchaseOrderQuantityUnit":"{ ls_item_table-PurchaseOrderQuantityUnit }",| &
                      |"PurchaseOrderItem":"{ ls_item_table-itemnumber }","SupplierInvoiceItemAmount":"{ lv_item_sum - ls_item_table-netpricequantity * ls_item_table-netpriceamount * header_table[ 1 ]-yy1_retentionrate_pdh / 100   }",| &
                      |"QuantityInPurchaseOrderUnit": "{ lv_current_quantity  }", "DocumentCurrency": "{ lv_order_currency }"\}|."]\}\}|.

            CONCATENATE lv_body lv_str1 INTO lv_body.
          ENDIF.
        ENDLOOP.
        CONCATENATE lv_body ']}}' INTO lv_body.

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

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        """"""""""""""""""""""""""""Payment terms date""""""""""""""""""""""""""""""""""""""
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DATA: lv_unix_time_pt  TYPE string,
*              lv_unit_time_si(13) TYPE n,
              lv_suppl_inv(10) TYPE c,
              lv_suppl_year(4) TYPE c.

        lv_str2 = lv_post_resp_paid.
        REPLACE ALL OCCURRENCES OF '"' IN lv_str2 WITH ' '.
        REPLACE ALL OCCURRENCES OF '''' IN lv_str2 WITH ' '.

        SPLIT lv_str2 AT 'SupplierInvoice=' INTO lv_str1 lv_str2.
        SPLIT lv_str2 AT ',' INTO lv_str1 lv_str3.
        lv_suppl_inv = lv_str1.
        SPLIT lv_str2 AT 'FiscalYear=' INTO lv_str1 lv_str2.
        SPLIT lv_str2 AT ')' INTO lv_str1 lv_str3.
        lv_suppl_year = lv_str1.

        go_order_client_inv = cl_web_http_client_manager=>create_by_http_destination(
          i_destination = cl_http_destination_provider=>create_by_url( gv_order_api_url_inv ) ).

        lo_req_inv = go_order_client_inv->get_http_request(  ).

        lo_req_inv->set_header_fields( VALUE #(
         ( name = 'Content-Type' value = 'application/json')
         ( name = 'Accept' value = 'application/json' )
         ( name = 'APIKey' value = 'hwTmbcPc1KimcX4jFm96rUR3ApgHngUs' )
         ) ).

        go_order_client_inv->get_http_request( )->set_authorization_basic(
                                             i_username = 'API_USER'
                                             i_password = 'Welcome12345678901234567890!' ).

        " Unpack input field values such as tablename, dataoption, etc.
        ui_data_inv = request->get_form_field( `filetoupload-data` ).
        ui_dataref_inv = /ui2/cl_json=>generate( json = ui_data_inv ).

        IF ui_dataref_inv IS BOUND.
          ASSIGN ui_dataref_inv->* TO FIELD-SYMBOL(<ui_dataref_sp>).
          gv_order_num_inv = me->get_input_field_value( name = `TABLENAME` dataref = <ui_dataref_sp> ).
        ENDIF.

        CONCATENATE gv_order_api_url_inv `/A_SupplierInvoice(SupplierInvoice='` lv_suppl_inv `',FiscalYear='` lv_suppl_year `')` INTO lv_url_inv.

        lo_req_inv->set_uri_path( i_uri_path = lv_url_inv ).
        TRY.
            lv_response_inv = go_order_client_inv->execute( i_method = if_web_http_client=>get )->get_text( ).

          CATCH cx_web_http_client_error.
          CATCH cx_web_message_error.
        ENDTRY.

        DATA lv_strs TYPE string.
        CLEAR: lv_str2, lv_str1, lv_str3.

        lv_strs = lv_response_inv.
        REPLACE ALL OCCURRENCES OF '"' IN lv_strs WITH ' '.
        REPLACE ALL OCCURRENCES OF '''' IN lv_strs WITH ' '.

        SPLIT lv_strs AT 'DueCalculationBaseDate:\/Date(' INTO lv_str1 lv_strs.
        SPLIT lv_strs AT ')' INTO lv_str1 lv_str3.
        lv_unix_time_pt = lv_str1.
*        lv_unit_time_si = lv_str1 + ( lv_ret_days * 86400 * 1000 ). "Convertation days into seconds.
*        lv_unix_time_pt = lv_unit_time_si.

        lv_body = |\{"CompanyCode":"{ lv_company_code }","InvoicingParty":"{ lv_order_supplier }","DocumentDate":"/Date({ lv_unix_time })/","DueCalculationBaseDate":"/Date({ lv_unix_time_pt })/",\r\n| &
                  |"PaymentBlockingReason":"A","SupplierInvoiceStatus" : " ","PostingDate":"/Date({ lv_unix_time })/","DocumentCurrency":"{ lv_order_currency }",\r\n| &
                  |"InvoiceGrossAmount":"{ lv_paid }","PaymentTerms":"{ lv_zpayment_term }","DocumentHeaderText":"SideBySide","SupplierInvoiceIDByInvcgParty":"{ lv_unix_time }",\r\n| &
                  |"to_SupplierInvoiceTax": \{"results": [\{"TaxCode":"I0","DocumentCurrency": "{ lv_order_currency }" \}]\}| &
                  |,"to_SuplrInvcItemPurOrdRef": \{"results": [|.
        lv_counter = 0.

        LOOP AT item_table INTO ls_item_table.
          IF line_exists( lt_item_qty[ purchaseorderitem = ls_item_table-itemnumber ] ).

            lv_current_quantity_brd = ls_item_table-netpricequantity * header_table[ 1 ]-yy1_retentionrate_pdh / 100.
            IF lv_current_quantity_brd LT '0.001'.
              lv_current_quantity = ls_item_table-netpricequantity.
              lv_netpriceamount = ls_item_table-netpriceamount * ls_item_table-netpricequantity.
            ELSE.
              lv_current_quantity = lv_current_quantity_brd.
              lv_netpriceamount = ls_item_table-netpricequantity * ls_item_table-netpriceamount * header_table[ 1 ]-yy1_retentionrate_pdh / 100.
              CLEAR: lv_current_quantity_brd.
            ENDIF.

            IF lv_counter > 0.
              CONCATENATE lv_body ',' INTO lv_body.
            ENDIF.
            lv_counter = lv_counter + 1.
            lv_str1 = |\{"SupplierInvoiceItem":"{ lv_counter }","PurchaseOrder":"{ gv_order_num }","PurchaseOrderQuantityUnit":"{ ls_item_table-PurchaseOrderQuantityUnit }",| &
                      |"PurchaseOrderItem":"{ ls_item_table-itemnumber }","SupplierInvoiceItemAmount":"{ lv_netpriceamount }",| &
                      |"QuantityInPurchaseOrderUnit": "{ lv_current_quantity }", "DocumentCurrency": "{ lv_order_currency }"\}|."]\}\}|.
            CONCATENATE lv_body lv_str1 INTO lv_body.
          ENDIF.
        ENDLOOP.
        CONCATENATE lv_body ']}}' INTO lv_body.

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
        RETURN.
    ENDCASE.

  ENDMETHOD.

  METHOD constructor.
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
     |                     alert( 'Invoice creation is in process. It can take a few seconds.' ) \n| &&
     |                    let oFileUploader = oCore.byId("fileToUpload") \n| &&
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
     |                        let param = new sap.ui.unified.FileUploaderParameter("uploadParam") \n| &&
     |                        param.setName("tablename") \n| &&
     |                        fileUploader.placeAt("line3") \n| &&
     |                        button.placeAt("line2") \n| &
     |                        fileUploader.setStyle("Transparent")        \n| &&
     |                        fileUploader.setButtonText(" ")        \n| &&
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
