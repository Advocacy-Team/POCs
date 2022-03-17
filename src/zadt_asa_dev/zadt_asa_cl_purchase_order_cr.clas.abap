CLASS zadt_asa_cl_purchase_order_cr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zadt_asa_cl_purchase_order_cr IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
*    DATA: it_WAITING_FOR  TYPE TABLE FOR CREATE ziV_WAITING_FOR,
*          it_insp_details TYPE STANDARD TABLE OF i_inspectionlot,
*          it_insplot      TYPE STANDARD TABLE OF ty_insplot,
*          lv_date         TYPE c LENGTH 8.
*
*    DATA: lv_body    TYPE string,
*          lo_value_r TYPE REF TO if_web_http_requesT,
*          lo_modify  TYPE REF TO zcl_qinsp_bhv_impl,
*          lv_val     TYPE string.
*
*    lv_date = sy-datum - 1.
*
*    SELECT  a~plant,a~material,a~batch,a~InspectionLot
*     FROM I_InspectionLot AS a
*     WHERE InspectionLotHasUsageDecision IS NOT INITIAL
*       AND InspLotCreatedOnLocalDate >= @lv_date
*     INTO TABLE @it_insplot.
*    IF sy-subrc = 0.
*
*      LOOP AT it_insplot ASSIGNING FIELD-SYMBOL(<fs_insp>).
*
*        it_waiting_for = VALUE #( ( plant = <fs_insp>-plant material = <fs_insp>-material
*                                  batch = <fs_insp>-batch insplot = <fs_insp>-insplot
*                                  %control = VALUE #( plant = if_abap_behv=>mk-on
*                                                      material = if_abap_behv=>mk-on
*                                                      batch = if_abap_behv=>mk-on
*                                                      insplot = if_abap_behv=>mk-on )
*           ) ).
*      ENDLOOP.
*
*      MODIFY ENTITIES OF ziV_WAITING_FOR
*      ENTITY ziv_waiting_for
*      CREATE FROM it_WAITING_FOR
*      FAILED DATA(it_failed)
*      REPORTED DATA(it_reported).
*      COMMIT ENTITIES.
*
*      SELECT inspectionlot
*        FROM i_inspectionresult
*       WHERE InspectionResultStatus = @abap_true
*        INTO TABLE @DATA(lt_inspresult).
*      IF sy-subrc = 0.
*      ENDIF.
*      SELECT PlannedOrder,Material,MRPPlant
*        FROM I_PlannedOrder FOR ALL ENTRIES IN @it_insplot
*       WHERE material = @it_insplot-material
*         AND mrpplant = @it_insplot-plant
*        INTO TABLE @DATA(it_plord).
*      IF sy-subrc = 0.
    TRY.
        DATA(lo_dest) = cl_http_destination_provider=>create_by_url(
*          i_url                  = 'https://my312934-api.s4hana.ondemand.com/sap/opu/odata/sap/API_INSPECTIONPLAN_SRV/A_InspPlanMaterialAssgmt'
          i_url                  = 'https://my312934-api.s4hana.ondemand.com/sap/opu/odata/sap/API_PURCHASEORDER_PROCESS_SRV/A_PurchaseOrder'
          ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest ).
*// Set authorization to API Hub
        lo_http_client->get_http_request( )->set_authorization_basic(
*                                     i_username = 'API_USER'
                                     i_username = 'API_USER'
*                                     i_password = 'Sap20172018201920202021*' ).
                                     i_password = 'Welcome1234567890!!!' ).

*// Set CSRF Token for post
        TRY.
            lo_http_client->set_csrf_token( ).
          CATCH cx_web_http_client_error.
        ENDTRY.

        DATA(lo_request) = lo_http_client->get_http_request( ).

*// Filter on Material Document
*        lo_request->set_query( EXPORTING query = '$filter=Material eq ''4900000000'',$filter=Plant eq ''1710'' ').

        "adding headers
        DATA(lo_value) = lo_request->set_header_field( EXPORTING i_name = 'APIKey' i_value = 'v6ndvEfG8uJYuRlSiiIxuTkd7c62qQ4J' ).
        lo_request->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
        lo_request->set_header_field( EXPORTING i_name = 'Accept' i_value = 'application/json' ).
        lo_request->set_header_field( EXPORTING i_name = 'Content-Type' i_value = 'application/json' ).
        DATA(lr_csrf) = lo_request->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).
        lo_request->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = lr_csrf ).

        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
*        commit WORK.
        out->write( lo_response->get_text( ) ).
      CATCH cx_root INTO DATA(lx_exception).
        out->write( lx_exception->get_text( ) ).
    ENDTRY.
*      ENDIF.

*      SELECT plant,target_pq,good_for_pq
*        FROM ziv_good_for FOR ALL ENTRIES IN @it_insplot
*       WHERE plant = @it_insplot-plant
*         AND target_pq = @it_insplot-material
*        INTO TABLE @DATA(lt_good_for).
*      IF sy-subrc = 0.
*
**// Transfer Posting
*
*        LOOP AT lt_good_for ASSIGNING FIELD-SYMBOL(<lfs_insp>).
*
*          DATA(lo_dest1) = cl_http_destination_provider=>create_by_url(
*            i_url                  = 'https://my312743-api.s4hana.ondemand.com/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader'
*            ).
*
*          DATA(lo_http_client1) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest1 ).
**// Set authorization to API Hub
*          lo_http_client1->get_http_request( )->set_authorization_basic(
*                                       i_username = 'API_USER'
*                                       i_password = 'Welcome1234567890!!!' ).
*
**// Set CSRF Token for post
*          TRY.
*              lo_http_client1->set_csrf_token( ).
*            CATCH cx_web_http_client_error.
*          ENDTRY.
*
*          DATA(lo_request1) = lo_http_client->get_http_request( ).
*
*          DATA(lv_gmcode) = '04'.
*
**// Set body for Header and Item values
*          lv_body = |\{ "PostingDate":"/Date(1613844869000)/","GoodsMovementCode":"{ lv_gmcode }","to_MaterialDocumentItem":\{"results":[\{"Material":"{ <lfs_insp>-target_pq }","Plant":"{ <lfs_insp>-plant }","StorageLocation":"171A| &&
*        |","GoodsMovementType":"561","EntryUnit":"PC","QuantityInEntryUnit":"1","IssgOrRcvgMaterial":"{ <lfs_insp>-good_for_pq }","to_SerialNumbers":\{"results":[\{"SerialNumber":""\}]\}\}]\}\} |.
*
*          "adding headers
*          DATA(lo_value1) = lo_request1->set_header_field( EXPORTING i_name = 'APIKey' i_value = 'v6ndvEfG8uJYuRlSiiIxuTkd7c62qQ4J' ).
*          lo_request1->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
*          lo_request1->set_header_field( EXPORTING i_name = 'Accept' i_value = '*/*' ).
*          lo_request1->set_header_field( EXPORTING i_name = 'Content-Type' i_value = 'application/json' ).
*          DATA(lr_csrf1) = lo_request1->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).
*          lo_request1->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = lr_csrf1 ).
*          TRY.
*              lo_request->set_text(
*                EXPORTING
*                  i_text   = lv_body
*                RECEIVING
*                  r_value  = lo_value_r
*              ).
*            CATCH cx_web_message_error.
*          ENDTRY.
*
**// Calling Post method
*          TRY.
*              DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>post ).
*              DATA(http_status) = lo_response1->get_header_field( i_name = '~status_code' ).
*              DATA(lv_reason) = lo_response->get_header_field( '~status_reason' ).
*              IF http_status = '201'.
*                DATA(lv_response) = lo_response->get_text( ).
*                DATA(lr_data) = /ui2/cl_json=>generate( json = lv_response ).
*                SPLIT lv_response AT 'MaterialDocument=' INTO DATA(lv_text) DATA(lv_text1).
*                DATA(lv_matdoc) = lv_text1+1(10).
*
**//Modify Transfer posting status of the CDS entry
**              lo_modify->modify_status( EXPORTING mata = <lfs_insp>-materiala
**                                              matb = <lfs_insp>-materialb ).
*                COMMIT WORK.
**              out->write( |Material Document "{ lv_matdoc }" created| ).
*              ENDIF.
*
*            CATCH cx_root INTO DATA(lx_exception1).
*              out->write( lx_exception1->get_text( ) ).
*          ENDTRY.
*
*        ENDLOOP.
*********
*
**// Update Batch Characteristics
*        LOOP AT lt_good_for ASSIGNING FIELD-SYMBOL(<lfs_goodfor>).
*
*          lo_dest1 = cl_http_destination_provider=>create_by_url(
*            i_url                  = 'https://my312934-api.s4hana.ondemand.com/sap/opu/odata/sap/API_BATCH_SRV/Batch(Material="{<lfs_goodfor>-good_for_pq}", BatchIdentifyingPlant="{<lfs_goodfor>-plant}", Batch="0000000001")'
*            ).
*
*          lo_http_client1 = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_dest1 ).
**// Set authorization to API Hub
*          lo_http_client1->get_http_request( )->set_authorization_basic(
*                                       i_username = 'API_USER'
*                                       i_password = 'Welcome1234567890!!!' ).
*
**// Set CSRF Token for post
*          TRY.
*              lo_http_client1->set_csrf_token( ).
*            CATCH cx_web_http_client_error.
*          ENDTRY.
*
*          lo_request1 = lo_http_client->get_http_request( ).
*
**// Set body for Header and Item values
*          lv_body = |\{"d": \{"MatlBatchIsInRstrcdUseStock": true\}\}|.
*
*          "adding headers
*          lo_value1 = lo_request1->set_header_field( EXPORTING i_name = 'APIKey' i_value = 'v6ndvEfG8uJYuRlSiiIxuTkd7c62qQ4J' ).
*          lo_request1->set_header_field( EXPORTING i_name = 'DataServiceVersion' i_value = '2.0' ).
*          lo_request1->set_header_field( EXPORTING i_name = 'Accept' i_value = '*/*' ).
*          lo_request1->set_header_field( EXPORTING i_name = 'Content-Type' i_value = 'application/json' ).
*          lr_csrf1 = lo_request1->get_header_field( EXPORTING i_name = 'X-CSRF-Token' ).
*          lo_request1->set_header_field( EXPORTING i_name = 'X-CSRF-Token' i_value = lr_csrf1 ).
*          TRY.
*              lo_request->set_text(
*                EXPORTING
*                  i_text   = lv_body
*                RECEIVING
*                  r_value  = lo_value_r
*              ).
*            CATCH cx_web_message_error.
*          ENDTRY.
*
**// Calling Post method
*          TRY.
*              lo_response1 = lo_http_client->execute( i_method = if_web_http_client=>post ).
*              http_status = lo_response1->get_header_field( i_name = '~status_code' ).
*              lv_reason = lo_response->get_header_field( '~status_reason' ).
*              IF http_status = '201'.
*                lv_response = lo_response->get_text( ).
*                lr_data = /ui2/cl_json=>generate( json = lv_response ).
*                SPLIT lv_response AT 'MaterialDocument=' INTO lv_text lv_text1.
*                lv_matdoc = lv_text1+1(10).
*
**//Modify Transfer posting status of the CDS entry
**              lo_modify->modify_status( EXPORTING mata = <lfs_insp>-materiala
**                                              matb = <lfs_insp>-materialb ).
*                COMMIT WORK.
**              out->write( |Material Document "{ lv_matdoc }" created| ).
*              ENDIF.
*
*            CATCH cx_root INTO lx_exception1.
*              out->write( lx_exception1->get_text( ) ).
*          ENDTRY.
*
*        ENDLOOP.
*********
*
*      ENDIF.
*    ENDIF.
*
**// Send Email
*
*    DATA: lo_clobj    TYPE REF TO cl_bcs_mail_message,
*          lv_sender   TYPE c LENGTH 512,
*          lv_receiver TYPE c LENGTH 512,
*          lv_subject  TYPE c LENGTH 1024.
*
** Create Instance
*    TRY.
*        CALL METHOD lo_clobj->create_instance
*          RECEIVING
*            ro_mail_message = DATA(lo_mail_msg).
*      CATCH cx_bcs_mail.
*    ENDTRY.
*
** Set Sender
*    lv_sender = sy-uname.
*    TRY.
*        lo_mail_msg->set_sender( iv_address = lv_sender ).
*      CATCH cx_bcs_mail.
*    ENDTRY.
*
** Set Receiver
*    lv_receiver = 'apr11.ravindra@gmail.com'.
*    TRY.
*        lo_mail_msg->add_recipient(
*          EXPORTING
*            iv_address = lv_receiver
**    iv_copy    = TO
*        ).
*      CATCH cx_bcs_mail.
*    ENDTRY.
*
** Set Subject
*    lv_subject = 'Test email'.
*    lo_mail_msg->set_subject( iv_subject = lv_subject ).
*
** Check email
*    lo_mail_msg->check(
*      IMPORTING
*        et_exceptions = DATA(lt_exception)
*      RECEIVING
*        rv_all_ok     = DATA(lv_result)
*    ).
*
** Send email
*    TRY.
*        lo_mail_msg->send(
*          IMPORTING
*            et_status      = DATA(lv_status)
*            ev_mail_status = DATA(lv_mail_status)
*        ).
*      CATCH cx_bcs_mail.
*    ENDTRY.

  ENDMETHOD.
ENDCLASS.
