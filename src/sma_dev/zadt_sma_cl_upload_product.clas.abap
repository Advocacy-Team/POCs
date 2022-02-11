CLASS zadt_sma_cl_upload_product DEFINITION

  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: lv_tablename TYPE string.
    DATA: lv_filename TYPE string.
    DATA: lv_fileext TYPE string.
    DATA: lv_dataoption TYPE string.
    DATA: lv_filedata TYPE string.
    DATA: lt_filedata TYPE tt_filedata.

    METHODS: get_input_field_value IMPORTING iv_name      TYPE string
                                             is_dataref   TYPE data
                                   RETURNING VALUE(value) TYPE string.
    METHODS: get_html RETURNING VALUE(rv_ui_html) TYPE string.

    METHODS: create_material IMPORTING it_file_data TYPE tt_filedata.

    METHODS: get_product IMPORTING io_response     TYPE REF TO if_web_http_response.

ENDCLASS.



CLASS zadt_sma_cl_upload_product IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.


    CASE request->get_method(  ).

      WHEN CONV string( if_web_http_client=>get ).

        DATA(sap_table_request) = request->get_header_field( 'sap-table-request' ).
        IF sap_table_request IS INITIAL.
          response->set_text( get_html(   ) ).

        ELSE.

          get_product( response ).

        ENDIF.

      WHEN CONV string( if_web_http_client=>post ).

* the request comes in with metadata around the actual file data,
* extract the filename and fileext from this metadata as well as the raw file data.
        SPLIT request->get_text(  )  AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(content).
        READ TABLE content REFERENCE INTO DATA(content_item) INDEX 2.
        IF sy-subrc = 0.

          SPLIT content_item->* AT ';' INTO TABLE DATA(content_dis).
          READ TABLE content_dis REFERENCE INTO DATA(content_dis_item) INDEX 3.
          IF sy-subrc = 0.
            SPLIT content_dis_item->* AT '=' INTO DATA(fn) lv_filename.
            REPLACE ALL OCCURRENCES OF `"` IN lv_filename WITH space.
            CONDENSE lv_filename NO-GAPS.
            SPLIT lv_filename AT '.' INTO lv_filename lv_fileext.
          ENDIF.

        ENDIF.

        CASE lv_fileext.
          WHEN 'json'.
            DELETE content FROM 1 TO 4.  " Get rid of the first 4 lines
          WHEN 'csv'.
            DELETE content FROM 1 TO 5.  " Get rid of the first 5 lines

        ENDCASE.
        DELETE content FROM ( lines( content ) - 12 ) TO lines( content ).  " get rid of the last 9 lines

        CASE lv_fileext.
          WHEN 'json'.


*            DATA: dynamic_table TYPE REF TO data.
*            FIELD-SYMBOLS: <table_structure> TYPE tt_filedata.
*
**        TRY.
**        DATA
**            CREATE DATA dynamic_table TYPE TABLE OF ().
**            ASSIGN lt_filedata->* TO <table_structure>.
**          CATCH cx_sy_create_data_error INTO DATA(cd_exception).
**            response->set_status( i_code = if_web_http_status=>bad_request
**                                 i_reason = cd_exception->get_text(  ) ).
**            response->set_text( cd_exception->get_text(  )  ).
**            RETURN.
**        ENDTRY.
            LOOP AT content ASSIGNING FIELD-SYMBOL(<fs_content>).
              lv_filedata = |{ lv_filedata }{ <fs_content> }|.
            ENDLOOP.

            /ui2/cl_json=>deserialize( EXPORTING json = lv_filedata
                               pretty_name = /ui2/cl_json=>pretty_mode-none
                               CHANGING data = lt_filedata ).
*           DATA(lv_json) = /ui2/cl_json=>serialize( data = lt_filedata
*                   pretty_name = /ui2/cl_json=>pretty_mode-none ).

          WHEN 'csv'.

            DATA:ls_filedata TYPE string.
            FIELD-SYMBOLS: <fs_data> TYPE ts_filedata.
            LOOP AT content REFERENCE INTO content_item.  " put it all back together again humpdy dumpdy....
*          filedata = filedata && content_item->*.
              ls_filedata = content_item->*.
              APPEND INITIAL LINE TO lt_filedata ASSIGNING <fs_data>.
              SPLIT ls_filedata AT ',' INTO <fs_data>-ProductType
                                            <fs_data>-baseunit
                                            <fs_data>-industrysector
                                            <fs_data>-language
                                            <fs_data>-productdescription.
            ENDLOOP.

*            /ui2/cl_json=>serialize( data = lv_filedata
*                   pretty_name = /ui2/cl_json=>pretty_mode-none ).

        ENDCASE.

        IF NOT lt_filedata IS INITIAL.
          CALL METHOD create_material EXPORTING it_file_data = lt_filedata.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD get_html.
    rv_ui_html =
    |<!DOCTYPE HTML> \n| &&
     |<html> \n| &&
     |<head> \n| &&
     |    <meta http-equiv="X-UA-Compatible" content="IE=edge"> \n| &&
     |    <meta http-equiv='Content-Type' content='text/html;charset=UTF-8' /> \n| &&
     |    <title>ABAP File Uploader</title> \n| &&
     |    <script id="sap-ui-bootstrap" src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js" \n| &&
     |        data-sap-ui-theme="sap_fiori_3_dark" data-sap-ui-xx-bindingSyntax="complex" data-sap-ui-compatVersion="edge" \n| &&
     |        data-sap-ui-async="true"> \n| &&
     |    </script> \n| &&
     |    <script> \n| &&
     |        sap.ui.require(['sap/ui/core/Core'], (oCore, ) => \{ \n| &&
     | \n| &&
     |            sap.ui.getCore().loadLibrary("sap.f", \{ \n| &&
     |                async: true \n| &&
     |            \}).then(() => \{ \n| &&
     |                let shell = new sap.f.ShellBar("shell") \n| &&
     |                shell.setTitle("ABAP File Uploader") \n| &&
     |                shell.setShowCopilot(true) \n| &&
     |                shell.setShowSearch(true) \n| &&
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
     |                    button.setText("Upload File") \n| &&
     |                    button.attachPress(function () \{ \n| &&
     |                        let oFileUploader = oCore.byId("fileToUpload") \n| &&
     |                        if (!oFileUploader.getValue()) \{ \n| &&
     |                            sap.m.MessageToast.show("Choose a file first") \n| &&
     |                            return \n| &&
     |                        \} \n| &&
     |                        let oInput = oCore.byId("tablename") \n| &&
     |                        let oGroup = oCore.byId("grpDataOptions") \n| &&
*     |                        if (!oInput.getValue())\{ \n| &&
*     |                            sap.m.MessageToast.show("Target Table is Required") \n| &&
*     |                            return \n| &&
*     |                        \} \n| &&
     |                       let param = oCore.byId("uploadParam") \n| &&
     |                       param.setValue( oInput.getValue() ) \n| &&
     |                       oFileUploader.getParameters() \n| &&
     |                       oFileUploader.setAdditionalData(JSON.stringify(\{tablename: oInput.getValue(), \n| &&
     |                           dataOption: oGroup.getSelectedIndex() \})) \n| &&
     |                       oFileUploader.upload() \n| &&
     |                    \}) \n| &&
     |                    let input = new sap.m.Input("tablename") \n| &&
*     |                    input.placeAt("layout") \n| &&
*     |                    input.setRequired(true) \n| &&
*     |                    input.setWidth("600px") \n| &&
*     |                    input.setPlaceholder("Target ABAP Table") \n| &&
     |                    input.setShowSuggestion(true) \n| &&
     |                    input.attachSuggest(function (oEvent)\{ \n| &&
     |                      jQuery.ajax(\{headers: \{ "sap-table-request": oEvent.getParameter("suggestValue") \n | &&
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

          |                    let downloadButton = new sap.m.Button("newButton") \n| &&
     |                    downloadButton.setText("Download File") \n| &&
     |                    downloadButton.attachPress(function () \{ \n| &&
     |                       jQuery.ajax(\{headers: \{ "sap-table-request": "testValue" \n | &&
     |                          \}, \n| &&
     |                         error: function(oErr)\{ alert( JSON.stringify(oErr))\}, timeout: 30000, method:"GET",dataType: "json",success: function(myJSON) \{ \n| &&
 "   |                      alert( 'test' ) \n| &&
     |                     let dataStr = "data:text/json;charset=utf-8," + encodeURIComponent(JSON.stringify(myJSON)) \n| &&
     |                     let downloadAnchorNode = document.createElement('a') \n | &&
     |                     downloadAnchorNode.setAttribute("href",     dataStr) \n | &&
     |                     downloadAnchorNode.setAttribute("download",   "example.json"); \n| &&
     |                     document.body.appendChild(downloadAnchorNode) \n| &&
     |                     downloadAnchorNode.click() \n| &&
     |                     downloadAnchorNode.remove() \n| &&
     |                    \} \}) \n| &&
     |                    \}) \n| &&

     |                    line2.placeAt("layout") \n| &&
     |                    line3.placeAt("layout") \n| &&
     |                    line4.placeAt("layout") \n| &&
     |                    let groupDataOptions = new sap.m.RadioButtonGroup("grpDataOptions") \n| &&
     |                    let lblGroupDataOptions = new sap.m.Label("lblDataOptions") \n| &&
     |                    lblGroupDataOptions.setLabelFor(groupDataOptions) \n| &&
     |                    sap.ui.getCore().loadLibrary("sap.ui.unified", \{ \n| &&
     |                        async: true \n| &&
     |                    \}).then(() => \{ \n| &&
     |                        var fileUploader = new sap.ui.unified.FileUploader( \n| &&
     |                            "fileToUpload") \n| &&
     |                        fileUploader.setFileType(["csv","json"]) \n| &&
     |                        fileUploader.setWidth("400px") \n| &&
     |                        let param = new sap.ui.unified.FileUploaderParameter("uploadParam") \n| &&
     |                        param.setName("tablename") \n| &&
     |                        fileUploader.addParameter(param) \n| &&
     |                        fileUploader.placeAt("line2") \n| &&
     |                        button.placeAt("line2") \n| &&
     |                        downloadButton.placeAt("line2") \n| &&
     |                        fileUploader.setPlaceholder( \n| &&
     |                            "Choose File for Upload...") \n| &&
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


  METHOD get_product.
    TRY.
        DATA(lo_http_destination) = cl_http_destination_provider=>create_by_url( 'https://my400036-api.lab.s4hana.cloud.sap:443/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product' ).
        DATA(lo_web_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ) .

*        lo_web_http_client->get_http_request( )->set_authorization_basic(
*                                   i_username = 'ZOMARKOS'
*                                   i_password = 'QVgDSeZvYG#zyeqpLLVmmulsdMggYTBnaq7jwhfl' ).

        lo_web_http_client->set_authn_mode( if_a4c_cp_service=>service_specific ).
        DATA(lo_web_http_request) = lo_web_http_client->get_http_request( ).
        lo_web_http_request->set_authorization_basic( i_username = 'ZOMARKOS'
                                                      i_password = 'QVgDSeZvYG#zyeqpLLVmmulsdMggYTBnaq7jwhfl' ).
        lo_web_http_request->set_header_fields( VALUE #(
        (  name = 'APIKey'
           value = 'fgTDyAQAB2oPextZunYysnDo4aKhLXGF' )
        (  name = 'DataServiceVersion'
           value = '2.0' )
        (  name = 'Accept'
           value = 'application/json' )
        (  name = 'X-CSRF-Token'
           value = 'Fetch' )
         ) ).
*        lo_web_http_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = 'Fetch' ). "lr_get_csrf ).

*        TRY.
*            lo_web_http_client->set_csrf_token( ).
*          CATCH cx_web_http_client_error.
*        ENDTRY.

        "set request method and execute request
        DATA(lo_web_http_response) = lo_web_http_client->execute( if_web_http_client=>get ).
*        DATA(lv_get_response) = lo_web_http_response->get_text( ).
*        data(lv_header_response) = lo_web_http_request->get_header_fields( ).
*        DATA(lv_get_response_headers) = lo_web_http_response->get_header_fields( ).

        IF io_response IS NOT INITIAL.
          io_response->set_text( lo_web_http_response->get_text( ) ). "lv_get_response ).
        ENDIF.

      CATCH cx_http_dest_provider_error cx_web_http_client_error cx_web_message_error.
        "error handling
    ENDTRY.
  ENDMETHOD.


  METHOD create_material.

    DATA: lt_filedata     TYPE tt_filedata,
          lv_body         TYPE string,
          lo_value_r      TYPE REF TO if_web_http_requesT,
          lv_x_csrf_token TYPE string.


    lt_filedata = it_file_data.

    LOOP AT lt_filedata ASSIGNING FIELD-SYMBOL(<fs_fdata>).
      DATA(lo_dest) = cl_http_destination_provider=>create_by_url( 'https://my400036-api.lab.s4hana.cloud.sap:443/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product' ).
      DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest ).

**// Set body for Header and Item values
*      lv_body = |\{ "Product": "{ <fs_fdata>-product }","ProductType": "{ <fs_fdata>-prdtyp }","BaseUnit": "{ <fs_fdata>-unit }","Division": "{ <fs_fdata>-divsn }","to_Description": \{"results": [| &&
*          |\{"Language": "EN","ProductDescription": "{ <fs_fdata>-prd_desc }"\}]\},"to_Plant":\{"results": [| &&
*          |\{"Plant": "{ <fs_fdata>-plant }","MRPType": "{ <fs_fdata>-mrptype }","BaseUnit": "{ <fs_fdata>-b_unit }"\}]\}\} |.

      lo_http_client->set_authn_mode( if_a4c_cp_service=>service_specific ).
      DATA(lo_request) = lo_http_client->get_http_request( ).
      lo_request->set_header_field( EXPORTING i_name = 'APIKey' i_value = 'fgTDyAQAB2oPextZunYysnDo4aKhLXGF' ).
      lo_request->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
      lo_request->set_header_field( EXPORTING i_name = 'Accept' i_value = '*/*' ).
      lo_request->set_header_field( EXPORTING i_name = 'Content-Type' i_value = 'application/json' ).
      lo_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = 'Fetch' ).
      lo_request->set_authorization_basic( i_username = 'ZOMARKOS'
                                           i_password = 'QVgDSeZvYG#zyeqpLLVmmulsdMggYTBnaq7jwhfl' ).

      DATA(lo_response) = lo_http_client->execute( if_web_http_client=>get ).
      lv_x_csrf_token = lo_response->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).
      lo_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = lv_x_csrf_token ).

      DATA(lv_header_response) = lo_request->get_header_fields( ).
*        DATA(lv_get_response_headers) = lo_web_http_response->get_header_fields( ).


*      DATA(lr_csrf) = lo_request->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).
*      lo_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = lr_csrf ).


*// Set body for Header and Item values

*      " TODO - change hardcoded body to body from file
*      lv_body = '{"ProductType": "MAT",'
*             && '"BaseUnit": "KG",'
*             && '"IndustrySector": "M",'
*             && '"to_Description": '
*             && '{"results": '
*             && '[{"Language": "EN",'
*             && '"ProductDescription": "Test Product from ABAP class"}'
*             && ']}}'.

*                                            <fs_data>-ProductType
*                                            <fs_data>-baseunit
*                                            <fs_data>-industrysector
*                                            <fs_data>-language
*                                            <fs_data>-productdescription.


      lv_body = |\{"ProductType": "{ <fs_fdata>-ProductType }",| &&
              |"BaseUnit": "{ <fs_fdata>-baseunit }",| &&
              |"IndustrySector": "{ <fs_fdata>-industrysector }",| &&
              |"to_Description": | &&
              |\{"results": | &&
              |[\{"Language": "{ <fs_fdata>-language }",| &&
              |"ProductDescription": "{ <fs_fdata>-productdescription }"\}| &&
              |]\}\}|.

*lv_body = |\{ "ProductType": "MAT", "BaseUnit": "KG", "IndustrySector": "M",| &&
*       |"to_Description": \{ "results": [ | &&
*         |\{ "Language": "EN", "ProductDescription": "Test Material"\}]\}\}|.

**// Set body for Header and Item values
*      lv_body = |\{ "Product": "{ <fs_fdata>-product }","ProductType": "{ <fs_fdata>-prdtyp }","BaseUnit": "{ <fs_fdata>-unit }","Division": "{ <fs_fdata>-divsn }","to_Description": \{"results": [| &&
*          |\{"Language": "EN","ProductDescription": "{ <fs_fdata>-prd_desc }"\}]\}\}\} |.


*lv_body = /ui2/cl_json=>serialize( data = lt_filedata
*                   pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
*      lv_body = lv_json.

      TRY.
          lo_request->set_text(
*            EXPORTING
              i_text   = lv_body
*            RECEIVING
*              r_value  = lo_value_r
          ).
        CATCH cx_web_message_error.
      ENDTRY.

*// Calling Post method
      TRY.

          lo_response = lo_http_client->execute( if_web_http_client=>post ).
          DATA(lv_response_text) = lo_response->get_text( ).

*        CATCH cx_web_http_client_error.

*          DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>post ).
*          DATA(http_status) = lo_response->get_header_field( i_name = '~status_code' ).
*          DATA(lv_reason) = lo_response->get_header_field( '~status_reason' ).
*
*          lv_header_response = lo_request->get_header_fields( ).
*          lv_header_response = lo_response->get_header_fields( ).
*
*          IF http_status(1) = '2'.
*            DATA(lv_response) = lo_response->get_text( ).
*            DATA(lr_data) = /ui2/cl_json=>generate( json = lv_response ).
*            SPLIT lv_response AT 'A_Product(' INTO DATA(lv_text) DATA(lv_text1).
*            SPLIT lv_text1 AT ')' INTO DATA(lv_text2) DATA(lv_text_3).
*            DATA(lv_material) = lv_text2.
*            COMMIT WORK.
*            lo_response->set_status( i_code = if_web_http_status=>ok
*                   i_reason = | 'Material "{ lv_material }" created successfully' | ) .
*            lo_response->set_text( |'Material "{ lv_material }" created successfully'| ).
*
*          ELSE.
*
*            lo_response->set_text( |{ http_status } { lv_reason }| ).
*
*          ENDIF.

        CATCH cx_root INTO DATA(lx_exception).
*            out->write( lx_exception->get_text( ) ).
      ENDTRY.
      DATA(lv_get_response_headers) = lo_response->get_header_fields( ).
    ENDLOOP.
    CLEAR: lt_filedata.

  ENDMETHOD.


  METHOD get_input_field_value.

    FIELD-SYMBOLS: <value> TYPE data,
                   <field> TYPE any.

    ASSIGN COMPONENT iv_name  OF STRUCTURE is_dataref TO <field>.
    IF <field> IS ASSIGNED.
      ASSIGN <field>->* TO <value>.
      value = condense( <value> ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
