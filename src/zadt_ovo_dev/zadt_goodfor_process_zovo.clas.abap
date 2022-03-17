CLASS zadt_goodfor_process_zovo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

*    METHODS : sending_mail.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA osql_test_environment TYPE REF TO if_osql_test_environment.

ENDCLASS.


CLASS zadt_goodfor_process_zovo IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    TYPES:
      BEGIN OF ty_insplot,
        insplot  TYPE c LENGTH 12,
        plant    TYPE werks_d,
        material TYPE matnr,
        batch    TYPE c LENGTH 10,
        sloc     TYPE c LENGTH 4,
        objnr    TYPE c LENGTH 22,
      END OF ty_insplot.

    DATA: it_waiting_for    TYPE TABLE FOR CREATE ZADT_I_Waiting_For_M,
          it_up_waiting_for TYPE TABLE OF ZADT_I_Waiting_For_M,
          it_insp_details   TYPE STANDARD TABLE OF I_InspectionLot,
          it_insplot        TYPE STANDARD TABLE OF ty_insplot,
          it_insplot1       TYPE STANDARD TABLE OF ZADT_I_Waiting_For_M,
          lv_date           TYPE c LENGTH 8,
          lv_body           TYPE string,
          lo_value_r        TYPE REF TO if_web_http_request,
          lv_val            TYPE string,
          lt_yggood_for     TYPE TABLE OF ZADT_I_Good_For_M.



    lv_date = cl_abap_context_info=>get_system_date(  ) - 1.

*///receiving any new inspection lots that meet the criteria: no usage decision made, created in the current or last day
*    SELECT  a~plant,a~material,a~batch,a~InspectionLot
*     FROM I_InspectionLot AS a
*     WHERE InspectionLotHasUsageDecision IS NOT INITIAL
*       AND InspLotCreatedOnLocalDate >= @lv_date
*     INTO TABLE @it_insplot.

    SELECT * FROM
      ZADT_I_Good_For_M
      INTO TABLE @lt_yggood_for.

    SELECT * FROM
      ZADT_I_Waiting_For_M
      INTO TABLE @it_up_waiting_for.

    SELECT  a~plant,
            a~material,
            a~batch,
            a~insplot,
            a~ObjNr
         FROM ZADT_I_InspectionLot_M AS a
         INTO TABLE @it_insplot.

    IF sy-subrc = 0.

      LOOP AT it_insplot ASSIGNING FIELD-SYMBOL(<fs_insp>).

        it_waiting_for = VALUE #( ( plant    = <fs_insp>-plant material = <fs_insp>-material
                                    batch    = <fs_insp>-batch insplot  = <fs_insp>-insplot
                                    %control = VALUE #( plant    = if_abap_behv=>mk-on
                                                        material = if_abap_behv=>mk-on
                                                        batch    = if_abap_behv=>mk-on
                                                        insplot  = if_abap_behv=>mk-on ) ) ).
      ENDLOOP.

*///modify the main table that stores the records yet to be processed
*///Any valid records found from the previous step are written to table zadt_waiting_for
      MODIFY ENTITIES OF ZADT_I_Waiting_For_M
       ENTITY ZADT_I_Waiting_For_M
        CREATE FROM it_waiting_for
          FAILED DATA(it_failed)
          REPORTED DATA(it_reported).
      COMMIT ENTITIES.

*///Begin filtering out any records that do not meet additional criteria
*///check if the current inspection lot status’s meet the criteria
      SELECT inspectionlot
              FROM i_inspectionresult
             WHERE InspectionResultStatus = @abap_true
              INTO TABLE @DATA(lt_inspresult).

      SELECT * FROM zjest
       FOR ALL ENTRIES IN @it_insplot
        WHERE objnr = @it_insplot-objnr
        INTO TABLE @DATA(lt_jest).

*///Read planned orders – PLAF
*///Read planned orders with their quantities (for each plant and potential goodfor material)
      SELECT PlannedOrder, Material, MRPPlant
        FROM I_PlannedOrder FOR ALL ENTRIES IN @it_insplot
       WHERE material = @it_insplot-material
         AND mrpplant = @it_insplot-plant
        INTO TABLE @DATA(it_plord).

      IF sy-subrc = 0.

*/// 1 - API_INSPECTIONPLAN_SRV/A_InspPlanMaterialAssgmt
*/// IF planned orders exist - call API
        TRY.
            DATA(lo_dest) = cl_http_destination_provider=>create_by_url(
               i_url    = 'https://my305270.s4hana.ondemand.com/sap/opu/odata/sap/API_INSPECTIONPLAN_SRV/A_InspPlanMaterialAssgmt'
              ).

            DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest ).

*// Set authorization to API Hub
            lo_http_client->get_http_request( )->set_authorization_basic(
                                         i_username = 'ZOVO_TEST'
                                         i_password = 'JaKshGkcbSov~ukPYs4VXZwbVYgvTAVkLRBbuglj' ).

*// Set CSRF Token for post
            TRY.
                lo_http_client->set_csrf_token( ).
              CATCH cx_web_http_client_error.
            ENDTRY.

            DATA(lo_request) = lo_http_client->get_http_request( ).

*// Filter on Material Document
            lo_request->set_query( EXPORTING query = '$filter=Material eq ''4900000000'',$filter=Plant eq ''1710'' ').

            "adding headers
            DATA(lo_value) = lo_request->set_header_field( EXPORTING i_name = 'APIKey' i_value = 'TQA24nV1qwpKZ7kDAZqvYNCncjiGH8h9' ).

            lo_request->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
            lo_request->set_header_field( EXPORTING i_name = 'Accept'             i_value = 'application/json' ).
            lo_request->set_header_field( EXPORTING i_name = 'Content-Type'       i_value = 'application/json' ).

*            DATA(lr_csrf) = lo_request->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).

            lo_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = 'fetch' ).
            DATA(lr_csrf) = lo_request->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).

            DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
*//commit WORK.
            out->write( lo_response->get_text( ) ).
          CATCH cx_root INTO DATA(lx_exception).
            out->write( lx_exception->get_text( ) ).
        ENDTRY.
      ENDIF.

*///receiving all plants and materials relevant for the goodfor process
      SELECT plant,target_pq,good_for_pq
        FROM zadt_i_good_for_m FOR ALL ENTRIES IN @it_insplot
       WHERE plant = @it_insplot-plant
         AND target_pq = @it_insplot-material
        INTO TABLE @DATA(lt_good_for).

*// 2 - API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader
*      IF sy-subrc = 0.

*// Transfer Posting
      LOOP AT lt_yggood_for ASSIGNING FIELD-SYMBOL(<lfs_insp>)."lt_good_for

        TRY.
            DATA(lo_dest1) = cl_http_destination_provider=>create_by_url(
                 i_url     = 'https://my305270.s4hana.ondemand.com/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader'
              ).
          CATCH cx_http_dest_provider_error.
            "handle exception
        ENDTRY.

        TRY.
            DATA(lo_http_client1) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest1 ).
          CATCH cx_web_http_client_error.
            "handle exception
        ENDTRY.

*// Set authorization to API Hub
        lo_http_client1->get_http_request( )->set_authorization_basic(
                                        i_username = 'ZOVO_TEST'
                                        i_password = 'JaKshGkcbSov~ukPYs4VXZwbVYgvTAVkLRBbuglj' ).
*// Set CSRF Token for post
*        TRY.
*            lo_http_client1->set_csrf_token( ).
*          CATCH cx_web_http_client_error.
*        ENDTRY.

        DATA(lo_request1) = lo_http_client1->get_http_request( ).

        DATA(lv_gmcode) = '04'.



        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        """"""""""""Get request to get csrf token""""""""""""""""""""""""""""""""""""""""""""
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DATA(lo_dest2) = cl_http_destination_provider=>create_by_url( 'https://my305270.s4hana.ondemand.com/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader' ).
        DATA(lo_invoice_client) = cl_web_http_client_manager=>create_by_http_destination(
        i_destination = lo_dest2 ).

        DATA(lo_req_invoice) = lo_invoice_client->get_http_request(  ).
        lo_invoice_client->get_http_request( )->set_authorization_basic(
     i_username = 'ZOVO_TEST'
                                         i_password = 'JaKshGkcbSov~ukPYs4VXZwbVYgvTAVkLRBbuglj' ).

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
        DATA lv_current_time TYPE tzntstmpl.
        DATA lv_tstmp1 TYPE p.
        DATA lv_tstmp2 TYPE p.
        DATA lv_secs TYPE tzntstmpl.



*// Set body for Header and Item values
        lv_body = |\{ "PostingDate":"/Date(1613844869000)/",| &&
                  |"GoodsMovementCode":"{ lv_gmcode }",| &&
                  |"to_MaterialDocumentItem":\{"results":[\{"Material":"{ <lfs_insp>-target_pq }",| &&
                  |"Plant":"{ <lfs_insp>-plant }",| &&
                  |"StorageLocation":"171A",| &&
                  |"GoodsMovementType":"",| &&
                  |"EntryUnit":"PC",| &&
                  |"QuantityInEntryUnit":"1",| &&
                  |"IssgOrRcvgMaterial":"{ <lfs_insp>-good_for_pq }",| &&
                  |"to_SerialNumbers":\{"results":[\{"SerialNumber":""\}]\}\}]\}\} |.

        TRY.
            lo_req_invoice->set_text(
              EXPORTING
                i_text   = lv_body ).
          CATCH cx_web_message_error.
        ENDTRY.

        DATA(lv_post_resp_paid) =  lo_invoice_client->execute( i_method = if_web_http_client=>post )->get_text(  ).

        "adding headers
        DATA(lo_value1) = lo_request1->set_header_field( EXPORTING i_name = 'APIKey' i_value = 'TQA24nV1qwpKZ7kDAZqvYNCncjiGH8h9' ).

        lo_request1->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
        lo_request1->set_header_field( EXPORTING i_name = 'Accept' i_value = '*/*' ).
        lo_request1->set_header_field( EXPORTING i_name = 'Content-Type' i_value = 'application/json' ).

*        DATA(lr_csrf1) = lo_request1->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).

        lo_request1->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = 'fetch'  ).
*        DATA(lr_csrf1) = lo_request1->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).

      ""  TRY.
     "       lo_request1->set_text(
      ""        EXPORTING
    ""            i_text   = lv_body
        ""      RECEIVING
          ""      r_value  = lo_value_r
         ""   ).
        ""  CATCH cx_web_message_error.
       "" ENDTRY.

*// Calling Post method
        TRY.
            DATA(lo_response12) = lo_http_client1->execute( i_method = if_web_http_client=>get )->get_text(  ).
            DATA(lo_response1) = lo_http_client1->execute( i_method = if_web_http_client=>get ).
             DATA(lr_csrf1) = lo_request1->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).
            DATA(http_status)  = lo_response1->get_header_field( i_name = '~status_code' ).
            DATA(lv_reason)    = lo_response->get_header_field( '~status_reason' ).

            IF http_status = '201'.

              DATA(lv_response) = lo_response->get_text( ).
              DATA(lr_data)     = /ui2/cl_json=>generate( json = lv_response ).

              SPLIT lv_response AT 'MaterialDocument=' INTO DATA(lv_text) DATA(lv_text1).

              DATA(lv_matdoc)   = lv_text1+1(10).

              COMMIT WORK.
            ENDIF.

          CATCH cx_root INTO DATA(lx_exception1).
            out->write( lx_exception1->get_text( ) ).
        ENDTRY.

      ENDLOOP.

*// 3 - API_BATCH_SRV/Batch
*// Update Batch Characteristics
      LOOP AT lt_yggood_for  ASSIGNING FIELD-SYMBOL(<lfs_goodfor>)."lt_good_for

        TRY.
            lo_dest1 = cl_http_destination_provider=>create_by_url(
            i_url    = 'https://my305270.s4hana.ondemand.com/sap/opu/odata/sap/API_BATCH_SRV/Batch(Material="{<lfs_goodfor>-good_for_pq}", BatchIdentifyingPlant="{<lfs_goodfor>-plant}", Batch="0000000001")'
              ).
          CATCH cx_http_dest_provider_error.
            "handle exception
        ENDTRY.

        TRY.
            lo_http_client1 = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest1 ).
          CATCH cx_web_http_client_error.
            "handle exception
        ENDTRY.
*// Set authorization to API Hub
        lo_http_client1->get_http_request( )->set_authorization_basic(
                                              i_username = 'ZOVO_TEST'
                                        i_password = 'JaKshGkcbSov~ukPYs4VXZwbVYgvTAVkLRBbuglj' ).
*// Set CSRF Token for post
        TRY.
            lo_http_client1->set_csrf_token( ).
          CATCH cx_web_http_client_error.
        ENDTRY.

        lo_request1 = lo_http_client->get_http_request( ).

*// Set body for Header and Item values
        lv_body = |\{"d": \{"MatlBatchIsInRstrcdUseStock": true\}\}|.

        "adding headers
        lo_value1 = lo_request1->set_header_field( EXPORTING i_name = 'APIKey' i_value = 'TQA24nV1qwpKZ7kDAZqvYNCncjiGH8h9' ).

        lo_request1->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
        lo_request1->set_header_field( EXPORTING i_name = 'Accept' i_value = '*/*' ).
        lo_request1->set_header_field( EXPORTING i_name = 'Content-Type' i_value = 'application/json' ).

        lr_csrf1 = lo_request1->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).

        lo_request1->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = lr_csrf1 ).

        TRY.
            lo_request->set_text(
              EXPORTING
                i_text   = lv_body
              RECEIVING
                r_value  = lo_value_r
            ).
          CATCH cx_web_message_error.
        ENDTRY.

*// Calling Post method
        TRY.
            lo_response1 = lo_http_client->execute( i_method = if_web_http_client=>post ).
            http_status = lo_response1->get_header_field( i_name = '~status_code' ).
            lv_reason = lo_response->get_header_field( '~status_reason' ).

            IF http_status = '201'.

              lv_response = lo_response->get_text( ).

              lr_data = /ui2/cl_json=>generate( json = lv_response ).

              SPLIT lv_response AT 'MaterialDocument=' INTO lv_text lv_text1.

              lv_matdoc = lv_text1+1(10).

              COMMIT WORK.
            ENDIF.

          CATCH cx_root INTO lx_exception1.
            out->write( lx_exception1->get_text( ) ).
        ENDTRY.

      ENDLOOP.

*     ENDIF.
    ENDIF.


*    DATA: send_request        TYPE REF TO cl_bcs.
*    DATA: li_main_text TYPE bcsy_text,
*          lw_main_text LIKE LINE OF li_main_text.
*
*    DATA: document            TYPE REF TO cl_document_bcs.
*    DATA: recipient           TYPE REF TO if_recipient_bcs.
*    DATA: sender              TYPE REF TO if_sender_bcs.
*
*    "DATA: l_mtitle            LIKE sodocchgi1-obj_descr.
*    DATA: sent_to_all         TYPE os_boolean.
*    DATA: bcs_exception       TYPE REF TO cx_bcs.
*
*    TRY.
**     -------- create persistent send request ------------------------
*        send_request = cl_bcs=>create_persistent( ).
**     -------- create and set document---------------
**     Email title
*      "  l_mtitle = 'Testing Email from Khaliachika'.
*
**     Fill the body of the mail
*        "REFRESH li_main_text.
*
*        lw_main_text = 'Testing email'.
*        APPEND lw_main_text TO li_main_text.
*        CLEAR: lw_main_text.
*
*        document = cl_document_bcs=>create_document(
*          i_type    = 'RAW'
*          i_text    = li_main_text
*          i_subject = 'Test' ).
*
**     Add document object to send request
*        send_request->set_document( document ).
**     Sender infor
*        sender = cl_cam_address_bcs=>create_internet_address(
*                  'mikhail.laushuk@sap.com' ). "Since SDN does not allow to put email address
*        send_request->set_sender( sender ).
*
**     --------- add recipient (e-mail address) -----------------------
*        recipient =
*            cl_cam_address_bcs=>create_internet_address(
*                  'mikhail.laushuk@sap.com' ).
*
**     Add recipient object to send request
*        CALL METHOD send_request->add_recipient
*          EXPORTING
*            i_recipient = recipient
*            i_express   = 'X'.
*
*        recipient =
*            cl_cam_address_bcs=>create_internet_address(
*                  'yauheni.kastsiukou@sap.com' ).
*
**     Add recipient object to send request
*        CALL METHOD send_request->add_recipient
*          EXPORTING
*            i_recipient = recipient
*            i_express   = 'X'.
*
*                    recipient =
*            cl_cam_address_bcs=>create_internet_address(
*                  'oleksii.yaremenko@sap.com' ).
*
**     Add recipient object to send request
*        CALL METHOD send_request->add_recipient
*          EXPORTING
*            i_recipient = recipient
*            i_express   = 'X'.
**     ---------- send document ---------------------------------------
*
*        sent_to_all = send_request->send( i_with_error_screen = 'X' ).


**/// Send emails to users for any critical errors captured during the process
*    DATA: lo_clobj    TYPE REF TO cl_bcs_mail_message,
*          lv_sender   TYPE c LENGTH 512,
*          lv_receiver TYPE c LENGTH 512,
*          lv_subject  TYPE c LENGTH 1024.

* Create Instance
*    TRY.
*   CALL METHOD lo_clobj->create_instance
*   RECEIVING
*                ro_mail_message = DATA(lo_mail_msg).
*          CATCH cx_bcs_mail.
*        ENDTRY.

*****    TRY.
*****        DATA(lo_mail_msg) = cl_bcs_mail_message=>create_instance( ).
*****      CATCH cx_bcs_mail.
*****        "handle exception
*****    ENDTRY.
*****
****** Set Sender
*****    lv_sender = sy-uname.
*****    TRY.
*****        lo_mail_msg->set_sender( iv_address = lv_sender ).
*****      CATCH cx_bcs_mail.
*****    ENDTRY.
*****
****** Set Receiver
*****    lv_receiver = 'yauheni.kastsiukou@sap.com'.
*****    TRY.
*****        lo_mail_msg->add_recipient(
*****          EXPORTING
*****            iv_address = lv_receiver
*****        ).
*****      CATCH cx_bcs_mail.
*****    ENDTRY.
*****
*****    CLEAR lv_receiver.
*****
*****    lv_receiver = 'mikhail.laushuk@sap.com'.
*****    TRY.
*****        lo_mail_msg->add_recipient(
*****          EXPORTING
*****            iv_address = lv_receiver
*****        ).
*****      CATCH cx_bcs_mail.
*****    ENDTRY.
*****
*****    lv_receiver = 'oleksii.yaremenko@sap.com'.
*****    TRY.
*****        lo_mail_msg->add_recipient(
*****          EXPORTING
*****            iv_address = lv_receiver
*****        ).
*****      CATCH cx_bcs_mail.
*****    ENDTRY.
*****
*****
****** Set Subject
*****    lv_subject = 'This is a test message for testing API'.
*****    lo_mail_msg->set_subject( iv_subject = lv_subject ).
*****
****** Check email
*****    lo_mail_msg->check(
*****      IMPORTING
*****        et_exceptions = DATA(lt_exception)
*****      RECEIVING
*****        rv_all_ok     = DATA(lv_result)
*****    ).
*****
****** Send email
*****    TRY.
*****        lo_mail_msg->send(
*****          IMPORTING
*****            et_status      = DATA(lv_status)
*****            ev_mail_status = DATA(lv_mail_status)
*****        ).
*****      CATCH cx_bcs_mail.
*****    ENDTRY.
*
*    me->sending_mail( ).
*
*  ENDMETHOD.
*
*
*  METHOD sending_mail.
*
*    "https://api.mailjet.com/v3.1/send
*    "https://api.openconnectors.us2.ext.hana.ondemand.com/elements/api-v2
*    "https://api.openconnectors.us2.ext.hana.ondemand.com/elements/api-v2/instances
*
*    "-user "f3950aa65aa3a47980fd25732e76e16f:6adef5f4669e55c707a45f903c5d7ac3"
*    " api key - f3950aa65aa3a47980fd25732e76e16f
*    " secret key - 6adef5f4669e55c707a45f903c5d7ac3
*
*    DATA: response       TYPE string,
*          lv_body1       TYPE string,
*          lo_http_client TYPE REF TO if_http_destination,
*          lo_value_r1    TYPE REF TO if_web_http_request.
*
*    TRY.
*        lo_http_client = cl_http_destination_provider=>create_by_url( i_url = 'https://api.mailjet.com/v3.1/send' ).
*      CATCH cx_http_dest_provider_error.
*        "handle exception
*    ENDTRY.
*
*    TRY.
*        DATA(lo_web_http) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_http_client ).
*      CATCH cx_web_http_client_error.
*        "handle exception
*    ENDTRY.
*
*    "adding headers
*    lo_web_http->get_http_request( )->set_header_field( i_name = 'Content-Type' i_value = 'application/json' ).
*    lo_web_http->get_http_request( )->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
*    "API Key for API Sandbox
*
*    TRY.
*        lo_web_http->get_http_request( )->set_authorization_basic( EXPORTING i_username = '545904640fe2d6f71ff57f4e9e106637'
*                                                                             i_password = '4897e51abd183e9282d7e4abcd269685'  ).
*      CATCH cx_web_message_error.
*    ENDTRY.
*
*    lv_body1 = '{ "Messages":[ { "From": { "Email": "mikhail.laushuk@sap.com" },' &&
*                                 '"To": [ { "Email": "mikhail.laushuk@sap.com"} ],' &&
*                                 '"Subject": "Mikhail Dev",' &&
*                                 '"TextPart": "Test Email"  } ] }'.
*    TRY.
*        lo_web_http->get_http_request( )->set_text(
*          EXPORTING
*            i_text   = lv_body1
*          RECEIVING
*            r_value  = lo_value_r1 ).
*      CATCH cx_web_message_error.
*    ENDTRY.
*
*    TRY.
*        DATA(lo_response) = lo_web_http->execute( i_method = if_web_http_client=>post ).
*      CATCH cx_web_http_client_error.
*        "handle exception
*    ENDTRY.
*
*    DATA(lv_status) = lo_response->get_status( ).
*    DATA(lv_text) = lo_response->get_text( ).
*
  ENDMETHOD.

ENDCLASS.
