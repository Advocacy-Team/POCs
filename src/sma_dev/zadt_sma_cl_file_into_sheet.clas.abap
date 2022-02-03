CLASS zadt_sma_cl_file_into_sheet DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: tablename TYPE string.
    DATA: filename TYPE string.
    DATA: fileext TYPE string.
    DATA: dataoption TYPE string.
    DATA: filedata TYPE string.
    DATA: lt_filedata TYPE lty_filedata.

    METHODS: get_input_field_value IMPORTING name         TYPE string
                                             dataref      TYPE data
                                   RETURNING VALUE(value) TYPE string.
    METHODS: get_html RETURNING VALUE(ui_html) TYPE string.

    METHODS: create_servicentry_sheet IMPORTING i_file_data TYPE lty_filedata
                                      EXPORTING e_ses       TYPE string.

ENDCLASS.



CLASS zadt_sma_cl_file_into_sheet IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.
    DATA:
       lv_ses TYPE string.

    CASE request->get_method(  ).

      WHEN CONV string( if_web_http_client=>get ).

        DATA(sap_table_request) = request->get_header_field( 'sap-table-request' ).
        IF sap_table_request IS INITIAL.
          response->set_text( get_html(   ) ).
        ELSE.
          DATA(name_filter) = xco_cp_abap_repository=>object_name->get_filter(
                               xco_cp_abap_sql=>constraint->contains_pattern( to_upper( sap_table_request ) && '%' )  ).
          DATA(objects) = xco_cp_abap_repository=>objects->tabl->where( VALUE #(
                              ( name_filter ) ) )->in( xco_cp_abap=>repository  )->get(  ).

          DATA(res) = `[`.
          LOOP AT objects INTO DATA(object).
            res &&= |\{ "TABLE_NAME": "{ object->name }" \}|.
            IF sy-tabix NE lines( objects ).
              res &&= `,`.
            ENDIF.
          ENDLOOP.
          res &&= `]`.
          response->set_text( res ).
        ENDIF.

      WHEN CONV string( if_web_http_client=>post ).

*        AUTHORITY-CHECK OBJECT 'M_SES_EKO'
**                             ID 'ZBUKRS' FIELD ''
*                             ID 'ACTVT' FIELD '03'.
*
        IF sy-subrc <> 0.
          response->set_status( i_code = if_web_http_status=>ok
             i_reason = | 'You are not authorized' | ) .
          response->set_text( |You are not authorized for creating service entry sheet| ).
        ELSE.
          DATA(lv_auth) = abap_true.
        ENDIF.
* the request comes in with metadata around the actual file data,
* extract the filename and fileext from this metadata as well as the raw file data.
        SPLIT request->get_text(  )  AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(content).
        READ TABLE content REFERENCE INTO DATA(content_item) INDEX 2.
        IF sy-subrc = 0.

          SPLIT content_item->* AT ';' INTO TABLE DATA(content_dis).
          READ TABLE content_dis REFERENCE INTO DATA(content_dis_item) INDEX 3.
          IF sy-subrc = 0.
            SPLIT content_dis_item->* AT '=' INTO DATA(fn) filename.
            REPLACE ALL OCCURRENCES OF `"` IN filename WITH space.
            CONDENSE filename NO-GAPS.
            SPLIT filename AT '.' INTO filename fileext.
          ENDIF.

        ENDIF.

        DELETE content FROM 1 TO 5.  " Get rid of the first 4 lines
        DELETE content FROM ( lines( content ) - 12 ) TO lines( content ).  " get rid of the last 9 lines

        DATA:ls_filedata TYPE string.
        FIELD-SYMBOLS: <fs_data> TYPE ty_filedata.
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

        IF NOT lt_filedata IS INITIAL.
*          IF lv_auth = abap_true.
          CALL METHOD create_servicentry_sheet
            EXPORTING
              i_file_data = lt_filedata
            IMPORTING
              e_ses       = lv_ses.
          response->set_status( i_code = if_web_http_status=>ok
                                i_reason = | 'Service Entry Sheet created successfully' | ) .
          response->set_text( |Service Entry Sheet "{ lv_ses }" created successfully| ).
        ENDIF.
*       ENDIF.
    ENDCASE.

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
     |                        fileUploader.setFileType("csv") \n| &&
     |                        fileUploader.setWidth("400px") \n| &&
     |                        let param = new sap.ui.unified.FileUploaderParameter("uploadParam") \n| &&
     |                        param.setName("tablename") \n| &&
     |                        fileUploader.addParameter(param) \n| &&
     |                        fileUploader.placeAt("line2") \n| &&
     |                        button.placeAt("line2") \n| &&
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

  METHOD create_Servicentry_Sheet.

    DATA:
      ls_header_data TYPE I_ServiceEntrySheetTP_2,
      ls_item_data   TYPE I_ServiceEntrySheetItemTP_2,

      lt_header_crt  TYPE TABLE FOR CREATE I_ServiceEntrySheetTP_2\\ServiceEntrySheet,
      lt_item_cba    TYPE TABLE FOR CREATE I_serviceentrysheettp_2\\ServiceEntrySheet\_ServiceEntrySheetItem.

    LOOP AT i_file_data ASSIGNING FIELD-SYMBOL(<fs_file_data>).
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


*--- Prepare Payload
      APPEND INITIAL LINE TO lt_header_crt ASSIGNING FIELD-SYMBOL(<ls_hdr_crt>).
      <ls_hdr_crt> = CORRESPONDING #( ls_header_data CHANGING CONTROL ).
      <ls_hdr_crt>-%cid     = `HEADER_1` .


      APPEND INITIAL LINE TO lt_item_cba ASSIGNING FIELD-SYMBOL(<ls_itm_cba>).
      <ls_itm_cba>-%cid_ref = 'HEADER_1'.

      APPEND INITIAL LINE TO <ls_itm_cba>-%target ASSIGNING FIELD-SYMBOL(<item_data>).
      <item_data>       = CORRESPONDING #( ls_item_data CHANGING CONTROL ).
      <item_data>-%cid  = 'Item_1'.

    ENDLOOP.


    MODIFY ENTITIES OF I_ServiceEntrySheetTP_2
      ENTITY serviceEntrySheet
        CREATE
          FROM lt_header_crt
         CREATE BY \_ServiceEntrySheetItem
          FROM  lt_item_cba
      FAILED DATA(ls_failed_crt)
      REPORTED DATA(ls_reported_crt)
      MAPPED DATA(ls_mapped_crt).

    DATA : lv_flag TYPE c.

    LOOP AT ls_failed_crt-serviceentrysheet ASSIGNING FIELD-SYMBOL(<failed>).

      lv_flag = ''.

      IF <failed>-%fail-cause IS NOT INITIAL.

        lv_flag = 'X'.

      ENDIF.

    ENDLOOP.

    "-- Commit and get the SES number
    COMMIT ENTITIES
      BEGIN RESPONSE OF I_ServiceEntrySheetTP_2
      REPORTED DATA(reported_late)
      FAILED DATA(failed_late).

    LOOP AT ls_mapped_crt-serviceentrysheet ASSIGNING FIELD-SYMBOL(<mapped>).
      CONVERT KEY OF I_ServiceEntrySheetTP_2 FROM <mapped>-%pid TO DATA(lv_ses).
      <mapped>-ServiceEntrySheet = lv_ses.
    ENDLOOP.
    IF NOT lv_ses IS INITIAL.
      e_ses = lv_ses.
    ENDIF.
    COMMIT ENTITIES END.

  ENDMETHOD.

ENDCLASS.
