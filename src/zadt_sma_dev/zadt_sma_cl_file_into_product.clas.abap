CLASS zadt_sma_cl_file_into_product DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
    METHODS: constructor
      RAISING
        cx_web_http_client_error
        cx_http_dest_provider_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mv_web           TYPE string VALUE 'https://sandbox.api.sap.com/s4hanacloud/sap/opu/odata/sap/',
          mo_http          TYPE REF TO if_web_http_client,
*          lv_tablename     TYPE string,
          lv_filename      TYPE string,
          lv_fileext       TYPE string,
          lv_dataoption    TYPE string,
          lv_filedata      TYPE string,
          lt_filedata      TYPE tt_filedata,

          lv_error_message TYPE string,
          lt_get_response  TYPE tt_ServiceEntrySheet.

    METHODS:
      get_html RETURNING VALUE(rv_ui_html) TYPE string,

      create_servicentry_sheet IMPORTING it_file_data TYPE tt_filedata
                               EXPORTING ev_ses       TYPE string
                                         ev_error_msg TYPE string ,

      get_purch_orders RETURNING VALUE(rv_purch_orders) TYPE string,

      post_entry_sheet .

ENDCLASS.



CLASS zadt_sma_cl_file_into_product IMPLEMENTATION.

  METHOD constructor.
    mo_http = cl_web_http_client_manager=>create_by_http_destination(
    i_destination = cl_http_destination_provider=>create_by_url( mv_web ) ).
  ENDMETHOD.

  METHOD if_http_service_extension~handle_request.
    DATA:
       lv_ses TYPE string.

    CASE request->get_method(  ).

      WHEN CONV string( if_web_http_client=>get ).

        DATA(sap_table_request) = request->get_header_field( 'sap-table-request' ).
        IF sap_table_request IS INITIAL.
          response->set_text( get_html(   ) ).
        ELSE.
*          DATA(name_filter) = xco_cp_abap_repository=>object_name->get_filter(
*                               xco_cp_abap_sql=>constraint->contains_pattern( to_upper( sap_table_request ) && '%' )  ).
*          DATA(objects) = xco_cp_abap_repository=>objects->tabl->where( VALUE #(
*                              ( name_filter ) ) )->in( xco_cp_abap=>repository  )->get(  ).
*

*"I`l put API call here.
          DATA(lv_purch_orders) = get_purch_orders( ).
          response->set_text( get_purch_orders(  ) ). "|You are not authorized for creating service entry sheet| ).

          response->set_header_field( i_name = |Content-Type|
                                      i_value = |text/json| ).

*          response->set_header_field( i_name = |Content-Type|
*                                      i_value = |application/x-download| ).


          response->set_header_field( i_name = |Content-Disposition|
                                      i_value = |attachment; filename="example.json"| ).

*          response->set_header_field( i_name = |Content-Disposition|
*                                      i_value = |form-data; name="example"; filename="example.json"| ).


*          DATA(res) = `[`.
*          LOOP AT objects INTO DATA(object).
*            res &&= |\{ "TABLE_NAME": "{ object->name }" \}|.
*            IF sy-tabix NE lines( objects ).
*              res &&= `,`.
*            ENDIF.
*          ENDLOOP.
*          res &&= `]`.
*          response->set_text( res ).

        ENDIF.

      WHEN CONV string( if_web_http_client=>post ).

*        AUTHORITY-CHECK OBJECT 'M_SES_EKO'
**                             ID 'ZBUKRS' FIELD ''
*                             ID 'ACTVT' FIELD '03'.
*
*        IF sy-subrc <> 0.
*          response->set_status( i_code = if_web_http_status=>ok
*             i_reason = | 'You are not authorized' | ) .
*          response->set_text( |You are not authorized for creating service entry sheet| ).
*        ELSE.
*          DATA(lv_auth) = abap_true.
*        ENDIF.
** the request comes in with metadata around the actual file data,
* extract the filename and fileext from this metadata as well as the raw file data.


**********************************************************************
*" Parse file depending what`s it ext
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
**********************************************************************
*" Delete necessary amount of fields from internal table

        CASE lv_fileext.

          WHEN 'json'.
            DELETE content FROM 1 TO 4.  " Get rid of the first 4 lines
          WHEN 'csv'.
            DELETE content FROM 1 TO 5.  " Get rid of the first 5 lines

        ENDCASE.

        DELETE content FROM ( lines( content ) - 12 ) TO lines( content ).  " get rid of the last 9 lines

**********************************************************************
*"parse data into internal table for csv?

        CASE lv_fileext.
          WHEN 'json'.
            DATA: dynamic_table TYPE REF TO data.
            FIELD-SYMBOLS: <table_structure> TYPE tt_filedata.

*        TRY.
*        DATA
*            CREATE DATA dynamic_table TYPE TABLE OF ().
*            ASSIGN lt_filedata->* TO <table_structure>.
*          CATCH cx_sy_create_data_error INTO DATA(cd_exception).
*            response->set_status( i_code = if_web_http_status=>bad_request
*                                 i_reason = cd_exception->get_text(  ) ).
*            response->set_text( cd_exception->get_text(  )  ).
*            RETURN.
*        ENDTRY.
            lv_filedata = content[ 1 ].
            /ui2/cl_json=>deserialize( EXPORTING json = lv_filedata
                               pretty_name = /ui2/cl_json=>pretty_mode-none
                               CHANGING data = lt_filedata ).

          WHEN 'csv'.

            DATA:ls_filedata TYPE string.
            FIELD-SYMBOLS: <fs_data> TYPE ts_filedata.
            LOOP AT content REFERENCE INTO content_item.  " put it all back together again humpdy dumpdy....
*          filedata = filedata && content_item->*.
              ls_filedata = content_item->*.
              APPEND INITIAL LINE TO lt_filedata ASSIGNING <fs_data>.
              SPLIT ls_filedata AT ',' INTO <fs_data>-purord
                                            <fs_data>-ses_name
                                            <fs_data>-obj_typ
                                            <fs_data>-post_date
                                            <fs_data>-cnfm_qty
                                            <fs_data>-po_item
                                            <fs_data>-acnt_cat
                                            <fs_data>-prfm_date.
            ENDLOOP.
        ENDCASE.
**********************************************************************

        IF NOT lt_filedata IS INITIAL.
*          IF lv_auth = abap_true.
          CALL METHOD create_servicentry_sheet
            EXPORTING
              it_file_data = lt_filedata
            IMPORTING
              ev_ses       = lv_ses
              ev_error_msg = lv_error_message.

          IF lv_error_message IS INITIAL.
            response->set_status( i_code = if_web_http_status=>ok
                                  i_reason = | 'Service Entry Sheet created successfully' | ) .
            response->set_text( |Service Entry Sheet "{ lv_ses }" created successfully| ).
          ELSE.
            response->set_status( i_code = if_web_http_status=>bad_request
                                  i_reason = | 'Service Entry Sheet creating finished with errors' | ) .
            response->set_text( |Error: { lv_error_message }| ).

          ENDIF.
        ENDIF.
*       ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD get_html.
    rv_ui_html =
    |<!DOCTYPE HTML> \n| &&
     |<html> \n| &&
     |<head> \n| &&
     |    <meta http-equiv="X-UA-Compatible" content="IE=edge"> \n| &&
     |    <meta http-equiv='Content-Type' content='text/html;charset=UTF-8' /> \n| &&
     |    <title>Padimento</title> \n| &&
     |    <script id="sap-ui-bootstrap" src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js" \n| &&
     |        data-sap-ui-theme="sap_fiori_3_dark" data-sap-ui-xx-bindingSyntax="complex" data-sap-ui-compatVersion="edge" \n| &&
     |        data-sap-ui-async="true"> \n| &&
     |    </script> \n| &&
     |    <script> \n| &&
     |        sap.ui.require(['sap/ui/core/Core'], (oCore, ) => \{ \n| &&
     |            \n| &&
     |            sap.ui.getCore().loadLibrary("sap.f", \{ \n| &&
     |                async: true \n| &&
     |            \}).then(() => \{ \n| &&
     |                let shell = new sap.f.ShellBar("shell") \n| &&
     |                shell.setTitle("Service Entry Sheet Uploader") \n| &&
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
     |                       oFileUploader.getParameters() \n| &&
     |                       oFileUploader.upload() \n| &&
     |                    \}) \n| &&
     |                    let downloadButton = new sap.m.Button("newButton") \n| &&
     |                    downloadButton.setText("Download File") \n| &&
     |                    downloadButton.attachPress(function () \{ \n| &&
     |                       jQuery.ajax(\{headers: \{ "sap-table-request": "testValue" \n | &&
     |                          \}, \n| &&
     |                         error: function(oErr)\{ alert( JSON.stringify(oErr))\}, timeout: 300000, method:"GET", dataType: "json", success: function(myJSON) \{ \n| &&
     |                    \} \}) \n| &&
     |                    \}) \n| &&
     |                    line2.placeAt("layout") \n| &&
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
     |                    \n| &&
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


  METHOD create_Servicentry_Sheet.

    DATA:
      ls_header_data TYPE I_ServiceEntrySheetTP_2,
      ls_item_data   TYPE I_ServiceEntrySheetItemTP_2,

      lt_header_crt  TYPE TABLE FOR CREATE I_ServiceEntrySheetTP_2\\ServiceEntrySheet,
      lt_item_cba    TYPE TABLE FOR CREATE I_serviceentrysheettp_2\\ServiceEntrySheet\_ServiceEntrySheetItem.

    LOOP AT it_file_data ASSIGNING FIELD-SYMBOL(<fs_file_data>).
* Create SES with reference to a Service Purchase Order Item

*--- Prepare Header Data
      ls_header_data-ServiceEntrySheetName  = <fs_file_data>-ses_name.
      ls_header_data-PurchaseOrder          = <fs_file_data>-purord.
      ls_header_data-SESOriginObjectType    = <fs_file_data>-obj_typ.
      ls_header_data-PostingDate            = <fs_file_data>-post_date.

*--- Prepare Item Data
      ls_item_data-PurchaseOrderItem              = <fs_file_data>-po_item.
*ls_item_data-ServiceEntrySheetItem          = '2'.
      ls_item_data-ConfirmedQuantity              = <fs_file_data>-cnfm_qty.
*ls_item_data-ServiceEntrySheetItemDesc      = 'SERVICE PROCUREMENT'.
      ls_item_data-AccountAssignmentCategory      = <fs_file_data>-acnt_cat.
      ls_item_data-ServicePerformanceDate         = <fs_file_data>-prfm_date.
*      ls_item_data-MultipleAcctAssgmtDistribution = '0'.

    ENDLOOP.

    CALL METHOD post_entry_sheet.
**--- Prepare Payload
*      APPEND INITIAL LINE TO lt_header_crt ASSIGNING FIELD-SYMBOL(<ls_hdr_crt>).
*      <ls_hdr_crt> = CORRESPONDING #( ls_header_data CHANGING CONTROL ).
*      <ls_hdr_crt>-%cid     = `HEADER_1` .
*
*
*      APPEND INITIAL LINE TO lt_item_cba ASSIGNING FIELD-SYMBOL(<ls_itm_cba>).
*      <ls_itm_cba>-%cid_ref = 'HEADER_1'.
*
*      APPEND INITIAL LINE TO <ls_itm_cba>-%target ASSIGNING FIELD-SYMBOL(<item_data>).
*      <item_data>       = CORRESPONDING #( ls_item_data CHANGING CONTROL ).
*      <item_data>-%cid  = 'Item_1'.
*
*    ENDLOOP.

*
*    MODIFY ENTITIES OF I_ServiceEntrySheetTP_2
*      ENTITY serviceEntrySheet
*        CREATE
*          FROM lt_header_crt
*         CREATE BY \_ServiceEntrySheetItem
*          FROM  lt_item_cba
*      FAILED DATA(ls_failed_crt)
*      REPORTED DATA(ls_reported_crt)
*      MAPPED DATA(ls_mapped_crt).
*
*    DATA : lv_flag TYPE c.
*
*    LOOP AT ls_failed_crt-serviceentrysheet ASSIGNING FIELD-SYMBOL(<failed>).
*
*      lv_flag = ''.
*
*      IF <failed>-%fail-cause IS NOT INITIAL.
*
*        lv_flag = 'X'.
*
*      ENDIF.
*
*    ENDLOOP.
*
*    IF lv_flag <> 'X'.
*      LOOP AT ls_reported_crt-serviceentrysheet ASSIGNING FIELD-SYMBOL(<ls_reported_crt_ses>).
*        IF <ls_reported_crt_ses>-%msg->m_severity = if_abap_behv_message=>severity-error.
*          lv_flag = 'X'.
*          ev_error_msg = | { <ls_reported_crt_ses>-%msg->if_t100_message~t100key-msgid }{ <ls_reported_crt_ses>-%msg->if_t100_message~t100key-msgno }|.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*    IF lv_flag <> 'X'.
*      LOOP AT ls_reported_crt-serviceentrysheetitem ASSIGNING FIELD-SYMBOL(<ls_reported_crt_sesitem>).
*        IF <ls_reported_crt_sesitem>-%msg->m_severity = if_abap_behv_message=>severity-error.
*          lv_flag = 'X'.
*          ev_error_msg = | { <ls_reported_crt_sesitem>-%msg->if_t100_message~t100key-msgid }{ <ls_reported_crt_sesitem>-%msg->if_t100_message~t100key-msgno }|.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*    "-- Commit and get the SES number
*    COMMIT ENTITIES
*      BEGIN RESPONSE OF I_ServiceEntrySheetTP_2
*      REPORTED DATA(reported_late)
*      FAILED DATA(failed_late).
*
*    LOOP AT ls_mapped_crt-serviceentrysheet ASSIGNING FIELD-SYMBOL(<mapped>).
*      CONVERT KEY OF I_ServiceEntrySheetTP_2 FROM <mapped>-%pid TO DATA(lv_ses).
*      <mapped>-ServiceEntrySheet = lv_ses.
*    ENDLOOP.
*    IF NOT lv_ses IS INITIAL.
*      ev_ses = lv_ses.
*    ENDIF.
*    COMMIT ENTITIES END.

  ENDMETHOD.


  METHOD get_purch_orders.

    DATA: lt_response_json TYPE tt_serviceentrysheet.
    DATA(lo_req) = mo_http->get_http_request(  ).
    lo_req->set_header_fields( VALUE #(
     ( name = 'Content-Type' value = 'application/json')
     ( name = 'Accept' value = 'application/json' )
     ( name = 'APIKey' value = 'Vo1BsQzQeGMXSo4HxlQY7RlqdGwdnf0y' )
     ) ).

    lo_req->set_uri_path( i_uri_path = mv_web && 'API_PRODUCT_SRV/A_Product?$top=3' ).

    TRY.
        DATA(lv_response) = mo_http->execute( i_method = if_web_http_client=>get )->get_text(  ).

*        /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*                           pretty_name = /ui2/cl_json=>pretty_mode-none
*                           CHANGING data = lt_response_json ).

*CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT' "
** EXPORTING
**   i_field_seperator = ';'     " char01
**   i_line_header =             " char01
**   i_filename =                " rlgrap-filename
**   i_appl_keep = SPACE         " char01
*  TABLES
*    i_tab_sap_data =            " standard table
** CHANGING
**   i_tab_converted_data =      " truxs_t_text_data
*  EXCEPTIONS
*    CONVERSION_FAILED = 1       "
*    .  "  SAP_CONVERT_TO_CSV_FORMAT


        rv_purch_orders = lv_response.
      CATCH cx_web_http_client_error cx_web_message_error.
    ENDTRY.
  ENDMETHOD.

  METHOD post_entry_sheet.
    TRY.
        "create http destination by url; API endpoint for API sandbox
        DATA(lo_http_destination) =
             cl_http_destination_provider=>create_by_url( 'https://sandbox.api.sap.com/s4hanacloud/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product' ).

        "create HTTP client by destination
        DATA(lo_web_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ) .

        "adding headers with API Key for API Sandbox
        DATA(lo_web_http_request) = lo_web_http_client->get_http_request( ).
        lo_web_http_request->set_header_fields( VALUE #(
        (  name = 'APIKey' value = 'Vo1BsQzQeGMXSo4HxlQY7RlqdGwdnf0y' )
        (  name = 'DataServiceVersion' value = '2.0' )
        (  name = 'Accept' value = 'application/json' )
        (  name = 'Content-Type' value = 'application/json' )
         ) ).



lo_web_http_request->set_text('{ "Product":"22", "ProductType":"SERV", "CrossPlantStatus":"", "CrossPlantStatusValidityDate":null,' &&
' "CreationDate":"\/Date(1476662400000)\/", "CreatedByUser":"", "LastChangeDate":null, "LastChangedByUser":"",' &&
 '"LastChangeDateTime":"", "IsMarkedForDeletion":false, "ProductOldID":"", "GrossWeight":"0.000", "PurchaseOrderQuantityUnit":"",' &&
 ' "SourceOfSupply":"", "WeightUnit":"KG", "NetWeight":"0.000", "CountryOfOrigin":"", "CompetitorID":"", "ProductGroup":"P001",' &&
 ' "BaseUnit":"H", "ItemCategoryGroup":"PSTE", "ProductHierarchy":"", "Division":"00", "VarblPurOrdUnitIsActive":"", "VolumeUnit":"",' &&
 ' "MaterialVolume":"0.000", "ANPCode":"0", "Brand":"", "ProcurementRule":"", "ValidityStartDate":null, "LowLevelCode":"",' &&
 ' "ProdNoInGenProdInPrepackProd":"", "SerialIdentifierAssgmtProfile":"", "SizeOrDimensionText":"", "IndustryStandardName":"",' &&
 ' "ProductStandardID":"", "InternationalArticleNumberCat":"", "ProductIsConfigurable":false, "IsBatchManagementRequired":false,' &&
 ' "ExternalProductGroup":"", "CrossPlantConfigurableProduct":"", "SerialNoExplicitnessLevel":"", "ProductManufacturerNumber":"",' &&
 ' "ManufacturerNumber":"", "ManufacturerPartProfile":"", "QltyMgmtInProcmtIsActive":false, "IndustrySector":"M", "ChangeNumber":"",' &&
 ' "MaterialRevisionLevel":"", "HandlingIndicator":"", "WarehouseProductGroup":"", "WarehouseStorageCondition":"",' &&
 ' "StandardHandlingUnitType":"", "SerialNumberProfile":"", "AdjustmentProfile":"", "PreferredUnitOfMeasure":"", "IsPilferable":false,' &&
 ' "IsRelevantForHzdsSubstances":false, "QuarantinePeriod":"0", "TimeUnitForQuarantinePeriod":"", "QualityInspectionGroup":"",' &&
 ' "AuthorizationGroup":"", "DocumentIsCreatedByCAD":false, "HandlingUnitType":"", "HasVariableTareWeight":false,' &&
 ' "MaximumPackagingLength":"0.000", "MaximumPackagingWidth":"0.000", "MaximumPackagingHeight":"0.000",' &&
 ' "UnitForMaxPackagingDimensions":"", "YY1_BonusValidityEnd_PRD":null, "YY1_BonusPercentage_PRD":"0.000",' &&
 ' "YY1_BonusPercentage_PRDU":"", "YY1_BonusValidityStart_PRD":null }').
        "set request method and execute request
        DATA(lo_web_http_response) = lo_web_http_client->execute( if_web_http_client=>post ).
        DATA(lv_response) = lo_web_http_response->get_text( ).

      CATCH cx_http_dest_provider_error cx_web_http_client_error cx_web_message_error.
        "error handling
    ENDTRY.

    "uncomment the following line for console output; prerequisite: code snippet is implementation of if_oo_adt_classrun~main
    "out->write( |response:  { lv_response }| ).


  ENDMETHOD.

ENDCLASS.
