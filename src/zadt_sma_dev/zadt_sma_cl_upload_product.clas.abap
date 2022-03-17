CLASS zadt_sma_cl_upload_product DEFINITION

  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: lv_tablename  TYPE string,
          lv_filename   TYPE string,
          lv_fileext    TYPE string,
          lv_dataoption TYPE string,
          lv_filedata   TYPE string,
          lt_filedata   TYPE tt_filedata,
          lt_user_ccode TYPE tt_user_ccode.

    METHODS: get_html RETURNING VALUE(rv_ui_html) TYPE string.

    METHODS: get_html_error RETURNING VALUE(rv_ui_html) TYPE string.

    METHODS: create_material IMPORTING it_file_data TYPE tt_filedata
                             EXPORTING ev_id        TYPE string.

    METHODS: get_product IMPORTING io_response     TYPE REF TO if_web_http_response.

ENDCLASS.



CLASS zadt_sma_cl_upload_product IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.
  .
    "Check authorization via authority object
*    AUTHORITY-CHECK OBJECT 'ZSMAAUTOBJ' ID 'ACTVT' FIELD '03'.
    AUTHORITY-CHECK OBJECT 'ZSMA_BUKRS'
    ID 'ACTVT'  field '01'
    ID 'ZSMA_BUKRS' dummy.
*    "after reviewing the decision of the additional authorization check
    "was found that the standard check is sufficient and does not need to be extended
    IF sy-subrc <> 0.

      response->set_text( get_html_error( ) ).

    ELSE.
      CASE request->get_method(  ).

        WHEN CONV string( if_web_http_client=>get ).

          DATA(sap_table_request) = request->get_header_field( 'sap-table-request' ).

          IF sap_table_request IS INITIAL.

            response->set_text( get_html( ) ).

          ELSE.

            get_product( response ).

          ENDIF.

        WHEN CONV string( if_web_http_client=>post ).

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


          "Get rid of the lines for different types of file
          CASE lv_fileext.
            WHEN 'json'.
              DELETE content FROM 1 TO 4.
            WHEN 'csv'.
              DELETE content FROM 1 TO 5.

          ENDCASE.
          DELETE content FROM ( lines( content ) - 12 ) TO lines( content ).

          "preparing data for POST processing
          CASE lv_fileext.

            WHEN 'json'.

              LOOP AT content ASSIGNING FIELD-SYMBOL(<fs_content>).
                lv_filedata = |{ lv_filedata }{ <fs_content> }|.
              ENDLOOP.

              /ui2/cl_json=>deserialize( EXPORTING json = lv_filedata
                                 pretty_name = /ui2/cl_json=>pretty_mode-none
                                 CHANGING data = lt_filedata ).

            WHEN 'csv'.

              DATA:ls_filedata TYPE string.
              FIELD-SYMBOLS: <fs_data> TYPE ts_filedata.
              LOOP AT content REFERENCE INTO content_item.
                ls_filedata = content_item->*.
                APPEND INITIAL LINE TO lt_filedata ASSIGNING <fs_data>.
                SPLIT ls_filedata AT ',' INTO <fs_data>-ProductType
                                              <fs_data>-baseunit
                                              <fs_data>-industrysector
                                              <fs_data>-language
                                              <fs_data>-productdescription
                                              <fs_data>-plant
                                              <fs_data>-mrptype.
              ENDLOOP.

          ENDCASE.

          "get ID of created Product
          DATA lv_id TYPE string.
          IF NOT lt_filedata IS INITIAL.
            CALL METHOD create_material
              EXPORTING
                it_file_data = lt_filedata
              IMPORTING
                ev_id        = lv_id.
          ENDIF.

          "sending Product ID to client
          IF lv_id IS NOT INITIAL.
            response->set_status( i_code = if_web_http_status=>ok
                                  i_reason = | 'Product created successfully' | ) .
            response->set_text( |Product "{ lv_id }" created successfully| ).

          ELSE.
            response->set_status( i_code = if_web_http_status=>bad_request
                                            i_reason = | 'Client error' | ) .
            response->set_text( |Cannot upload file. Check the file content and formating| ).

          ENDIF.

      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD get_html_error.
    rv_ui_html =
        |<!DOCTYPE HTML> \n| &&
        |<html> \n| &&
        |<head> \n| &&
        |    <title>Error page</title> \n| &&
        |    <script id="sap-ui-bootstrap" src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js" \n| &&
        |        data-sap-ui-theme="sap_fiori_3_dark" data-sap-ui-xx-bindingSyntax="complex" data-sap-ui-compatVersion="edge" \n| &&
        |        data-sap-ui-async="true"> \n| &&
        |    </script> \n| &&
        |<style>| &&
        |div \{ \n| &&
        | \n| &&
        |position: fixed;\n| &&
        |top: 50%;\n| &&
        |left: 50%;\n| &&
        |margin-top: -100px;\n| &&
        |margin-left: -200px;\n| &&
        |\}\n| &&
        |</style>| &&
        |</head> \n| &&
        |<body class="sapUiBody"> \n| &&
        |    <div id="uiArea">\n| &&
        |    <H1>You are not authorized</H1> \n| &&
        |    </div> \n| &&
        |</body> \n| &&
        | \n| &&
        |</html> |.
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
     |                shell.placeAt("uiArea") \n| &&
     |                sap.ui.getCore().loadLibrary("sap.ui.layout", \{ \n| &&
     |                    async: true \n| &&
     |                \}).then(() => \{ \n| &&
     |                    let layout = new sap.ui.layout.VerticalLayout("layout") \n| &&
     |                    layout.placeAt("uiArea") \n| &&
     |                    let line2 = new sap.ui.layout.HorizontalLayout("line2") \n| &&
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
     |                       let param = oCore.byId("uploadParam") \n| &&
     |                       param.setValue( oInput.getValue() ) \n| &&
     |                       oFileUploader.getParameters() \n| &&
     |                       oFileUploader.setAdditionalData(JSON.stringify(\{tablename: oInput.getValue(), \n| &&
     |                           dataOption: oGroup.getSelectedIndex() \})) \n| &&
     |                       oFileUploader.upload() \n| &&
     |                    \}) \n| &&
     |                    let input = new sap.m.Input("tablename") \n| &&
     |                    let downloadButton = new sap.m.Button("newButton") \n| &&
     |                    downloadButton.setText("Download Data") \n| &&
     |                    downloadButton.attachPress(function () \{ \n| &&
     |                       jQuery.ajax(\{headers: \{ "sap-table-request": "testValue" \n | &&
     |                          \}, \n| &&
     |                         error: function(oErr)\{ alert( JSON.stringify(oErr))\}, timeout: 30000, method:"GET",dataType: "json",success: function(myJSON) \{ \n| &&
     |                        let dataStr = "data:text/plain;charset=utf-8," + encodeURIComponent(JSON.stringify(myJSON)) \n| &&
     |                        let downloadAnchorNode = document.createElement('a') \n | &&
     |                        downloadAnchorNode.setAttribute("href",     dataStr) \n | &&
     |                        downloadAnchorNode.setAttribute("download",   "example.json"); \n| &&
     |                        document.body.appendChild(downloadAnchorNode) \n| &&
     |                        downloadAnchorNode.click() \n| &&
     |                        downloadAnchorNode.remove() \n| &&
     |                        \} \n| &&
     |                      \}) \n| &&
     |                    \}) \n| &&
     |                    line2.placeAt("layout") \n| &&
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
        "create API destination
        DATA(lo_http_destination) = cl_http_destination_provider=>create_by_url( |https://my400036-api.lab.s4hana.cloud.sap:443/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product?$inlinecount=allpages| &&
                                                                                 |&$orderby=LastChangeDateTime%20desc| ).
        DATA(lo_web_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ) .

        "set required headers for request
        lo_web_http_client->set_authn_mode( if_a4c_cp_service=>service_specific ).

        DATA(lo_web_http_request) = lo_web_http_client->get_http_request( ).
        lo_web_http_request->set_authorization_basic( i_username = 'ZOMARKOS'
                                                      i_password = 'QVgDSeZvYG#zyeqpLLVmmulsdMggYTBnaq7jwhfl' ).
        lo_web_http_request->set_header_fields( VALUE #( (  name = 'APIKey'
                                                            value = 'fgTDyAQAB2oPextZunYysnDo4aKhLXGF' )
                                                         (  name = 'DataServiceVersion'
                                                            value = '2.0' )
                                                         (  name = 'Accept'
                                                            value = 'application/json' )
                                                         (  name = 'X-CSRF-Token'
                                                            value = 'Fetch' )
                                                       )
                                               ).

        "set request method and execute request
        DATA(lo_web_http_response) = lo_web_http_client->execute( if_web_http_client=>get ).

        "receive and processing the data before response sending
        IF io_response IS NOT INITIAL.
          DATA(lv_attachment) = lo_web_http_response->get_text( ).

          REPLACE ALL OCCURRENCES OF REGEX '"__metadata":\C{0,400}A_ProductType"\},' IN lv_attachment WITH ''.
          REPLACE ALL OCCURRENCES OF REGEX ',"to_\C{0,2600}"\}\}' IN lv_attachment WITH ''.

          io_response->set_text( lv_attachment ).

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

    LOOP AT it_file_data ASSIGNING FIELD-SYMBOL(<fs_fdata>).

      "create API destination
      TRY.
          DATA(lo_dest) = cl_http_destination_provider=>create_by_url( 'https://my400036-api.lab.s4hana.cloud.sap:443/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product' ).
        CATCH cx_http_dest_provider_error.
      ENDTRY.
      DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest ).

      "set required headers for request
      lo_http_client->set_authn_mode( if_a4c_cp_service=>service_specific ).
      DATA(lo_request) = lo_http_client->get_http_request( ).
      lo_request->set_header_field( EXPORTING i_name = 'APIKey' i_value = 'fgTDyAQAB2oPextZunYysnDo4aKhLXGF' ).
      lo_request->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
      lo_request->set_header_field( EXPORTING i_name = 'Accept' i_value = '*/*' ).
      lo_request->set_header_field( EXPORTING i_name = 'Content-Type' i_value = 'application/json' ).
      lo_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = 'Fetch' ).
      lo_request->set_authorization_basic( i_username = 'ZOMARKOS'
                                           i_password = 'QVgDSeZvYG#zyeqpLLVmmulsdMggYTBnaq7jwhfl' ).

      "perform GET request before POST to create CSRF-Token
      DATA(lo_response) = lo_http_client->execute( if_web_http_client=>get ).
      lv_x_csrf_token = lo_response->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).
      lo_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = lv_x_csrf_token ).

      "creation of the body for POST request
      lv_body = |\{"ProductType": "{ <fs_fdata>-ProductType }",| &&
              |"BaseUnit": "{ <fs_fdata>-baseunit }",| &&
              |"IndustrySector": "{ <fs_fdata>-industrysector }",| &&
              |"to_Description": | &&
              |\{"results": | &&
              |[\{"Language": "{ <fs_fdata>-language }",| &&
              |"ProductDescription": "{ <fs_fdata>-productdescription }"\}| &&
              |]\},| &&
              |"to_Plant": | &&
              |\{"results": | &&
              |[\{"Plant": "{ <fs_fdata>-plant }",| &&
              |"MRPType": "{ <fs_fdata>-mrptype }"\}| &&
              |]\}| &&
              |\}|
              .

      TRY.
          lo_request->set_text(
              i_text   = lv_body
          ).
        CATCH cx_web_message_error.
      ENDTRY.


      "perform POST request and get ID of created Product
      TRY.

          lo_response = lo_http_client->execute( if_web_http_client=>post ).
          DATA(lv_response_text) = lo_response->get_text( ).
          DATA lv_response_status TYPE i.
          lv_response_status = lo_response->get_status( )-code.

          IF lv_response_status EQ if_web_http_status=>created.

            DATA lv_id TYPE string.
            lv_id = lv_response_text.
            REPLACE FIRST OCCURRENCE OF REGEX '\C+API_PRODUCT_SRV/A_Product\(' IN lv_id WITH ''.
            REPLACE FIRST OCCURRENCE OF REGEX '\)/to_Plant</id>\C+' IN lv_id WITH ''.

            ev_id = lv_id.

          ENDIF.

        CATCH cx_root INTO DATA(lx_exception).

      ENDTRY.
    ENDLOOP.
    CLEAR: lt_filedata.

  ENDMETHOD.

ENDCLASS.
