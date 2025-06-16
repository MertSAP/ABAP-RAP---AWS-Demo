CLASS lsc_zdm_r_invhdrtp DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.
    METHODS adjust_numbers REDEFINITION.
    METHODS save_modified REDEFINITION.
ENDCLASS.

CLASS lsc_zdm_r_invhdrtp IMPLEMENTATION.



  METHOD adjust_numbers.
    IF mapped-invoiceitem IS NOT INITIAL.

      DATA: max_item_id TYPE i VALUE 0.

      LOOP AT mapped-invoiceitem  ASSIGNING FIELD-SYMBOL(<item>).
        <item>-InvoiceID = <item>-%tmp-InvoiceID.
        IF max_item_id EQ 0.
          SELECT MAX( item_num ) FROM zdm_ainvitm WHERE invoice_id = @<item>-InvoiceID INTO @max_item_id .
        ENDIF.

        max_item_id += 1.
        <item>-ItemNum = max_item_id.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD save_modified.
    IF delete-invoice IS NOT INITIAL.

      LOOP AT delete-invoice INTO DATA(invoice).
        SELECT SINGLE FileName INTO @DATA(lv_filename) FROM zdm_ainvhdr WHERE invoice_id = @invoice-InvoiceID.
        IF lv_filename IS NOT INITIAL.
          DATA(storage_helper) = NEW zdm_cl_aws_invoice_storage(  invoice-InvoiceID ).
          storage_helper->delete_object( iv_filename = lv_filename ).
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_Invoice DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Invoice RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Invoice RESULT result.
    METHODS setstatusondraftcreate FOR DETERMINE ON SAVE
      IMPORTING keys FOR invoice~setstatusondraftcreate.
    METHODS approveinvoice FOR MODIFY
      IMPORTING keys FOR ACTION invoice~approveinvoice RESULT result.

    METHODS rejectinvoice FOR MODIFY
      IMPORTING keys FOR ACTION invoice~rejectinvoice RESULT result.
    METHODS uploadtos3 FOR DETERMINE ON SAVE
      IMPORTING keys FOR invoice~uploadtos3.
    METHODS populatefieldsfromattachment FOR DETERMINE ON MODIFY
      IMPORTING keys FOR invoice~populatefieldsfromattachment.
    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE invoice.

    METHODS get_extract_results IMPORTING
                                          iv_invoice   TYPE zdm_c_invhdrtp
                                EXPORTING et_lineitems TYPE zdm_tt_aws_extract
                                          es_invoice   TYPE zdm_c_invhdrtp.
    CONSTANTS:
      "travel status
      BEGIN OF invoice_status,
        notsubmitted TYPE c LENGTH 1 VALUE '', "Not Submitted
        open         TYPE c LENGTH 1 VALUE 'O', "Open
        approved     TYPE c LENGTH 1 VALUE 'A', "Approved
        rejected     TYPE c LENGTH 1 VALUE 'R', "Rejected
      END OF invoice_status.
ENDCLASS.

CLASS lhc_Invoice IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.
    READ ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
         ENTITY Invoice
            FIELDS ( Status )
            WITH CORRESPONDING #( keys )
          RESULT DATA(invoices)
          FAILED failed.

    " evaluate the conditions, set the operation state, and set result parameter
    result = VALUE #( FOR invoice IN invoices
                       ( %tky                   = invoice-%tky

                   "     %features-%update      = COND #( WHEN invoice-Status <> ''
                                                       "   THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )

                        %features-%delete      = COND #( WHEN invoice-Status = invoice_status-open
                                                          THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled   )

                         %action-Edit           =  COND #( WHEN invoice-Status <> invoice_status-notsubmitted
                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )


                         %action-approveInvoice   = COND #( WHEN invoice-Status EQ 'O'
                                                              THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled   )

                         %action-rejectInvoice   = COND #( WHEN invoice-Status EQ 'O'
                                                              THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled   )
                      ) ).


  ENDMETHOD.

  METHOD setStatusOnDraftCreate.
    READ ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
         ENTITY Invoice
           FIELDS ( Status )
           WITH CORRESPONDING #( keys )
         RESULT DATA(invoices).

    DELETE invoices WHERE Status IS NOT INITIAL.
    CHECK invoices IS NOT INITIAL.


    "update involved instances
    MODIFY ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
      ENTITY Invoice
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR invoice IN invoices INDEX INTO i (
                           %tky      = invoice-%tky
                           Status  =  invoice_status-open ) ).

  ENDMETHOD.

  METHOD approveInvoice.
    MODIFY ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
          ENTITY Invoice
             UPDATE FIELDS ( Status )
                WITH VALUE #( FOR key IN keys ( %tky         = key-%tky
                                                Status = invoice_status-approved ) ). " 'A' Approved



    " read changed data for result
    READ ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
      ENTITY Invoice
         ALL FIELDS WITH
         CORRESPONDING #( keys )
       RESULT DATA(invoices).

    result = VALUE #( FOR invoice IN invoices ( %tky = invoice-%tky  %param = invoice ) ).
  ENDMETHOD.

  METHOD rejectInvoice.
    MODIFY ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
          ENTITY Invoice
             UPDATE FIELDS ( Status )
                WITH VALUE #( FOR key IN keys ( %tky         = key-%tky
                                                Status = invoice_status-rejected ) ). " 'A' Approved

    " read changed data for result
    READ ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
      ENTITY Invoice
         ALL FIELDS WITH
         CORRESPONDING #( keys )
       RESULT DATA(invoices).

    result = VALUE #( FOR invoice IN invoices ( %tky = invoice-%tky  %param = invoice ) ).
  ENDMETHOD.

  METHOD earlynumbering_create.
    DATA: invoice_id_max TYPE zdm_int_invnum.

    " Ensure Travel ID is not set yet (idempotent)- must be checked when BO is draft-enabled
    LOOP AT entities INTO DATA(invoice) WHERE InvoiceID IS NOT INITIAL.
      APPEND CORRESPONDING #( invoice ) TO mapped-invoice.
    ENDLOOP.

    DATA(entities_wo_InvoiceID) = entities.
    DELETE entities_wo_InvoiceID WHERE InvoiceID IS NOT INITIAL.

    " Get Numbers
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr       = '01'
            object            = 'ZDM_INVNUM'
            quantity          = CONV #( lines( entities_wo_InvoiceID ) )
          IMPORTING
            number            = DATA(number_range_key)
            returncode        = DATA(number_range_return_code)
            returned_quantity = DATA(number_range_returned_quantity)
        ).
      CATCH cx_number_ranges INTO DATA(lx_number_ranges).
        LOOP AT entities_wo_InvoiceID INTO invoice.
          APPEND VALUE #(  %cid = invoice-%cid
                           %key = invoice-%key
                           %msg = lx_number_ranges
                        ) TO reported-invoice.
          APPEND VALUE #(  %cid = invoice-%cid
                           %key = invoice-%key
                        ) TO failed-invoice.
        ENDLOOP.
        EXIT.
    ENDTRY.


    " At this point ALL entities get a number!entities_wo_InvoiceID
    ASSERT number_range_returned_quantity = lines( entities_wo_InvoiceID ).

    invoice_id_max = number_range_key - number_range_returned_quantity.

    " Set Travel ID
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      invoice_id_max += 1.

      APPEND VALUE #( %cid  = <ls_entity>-%cid
                      %is_draft = <ls_entity>-%is_draft
                     InvoiceID = invoice_id_max
                    ) TO mapped-invoice.
    ENDLOOP.


  ENDMETHOD.

  METHOD uploadToS3.
    DATA: s3_successfull    TYPE abap_bool.

    READ ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
    ENTITY Invoice ALL FIELDS WITH CORRESPONDING #(  keys )
    RESULT FINAL(lt_invoices_entity).


    LOOP AT lt_invoices_entity INTO DATA(invoice_entity).
      IF invoice_entity-TmpAttachment IS  INITIAL.
        CONTINUE.
      ENDIF.

      DATA(storage_helper) = NEW zdm_cl_aws_invoice_storage(  invoice_entity-InvoiceID ).

      TRY.
          s3_successfull = abap_true.
          storage_helper->put_object( iv_filename     = invoice_entity-TmpFilename
                                      iv_old_filename = invoice_entity-Filename
                                      iv_body         = invoice_entity-TmpAttachment ).
        CATCH cx_root.

          INSERT VALUE #(  %tky   =  invoice_entity-%tky
                 %element-TmpAttachment =  if_abap_behv=>mk-on
                  %msg        = me->new_message_with_text(  severity = if_abap_behv_message=>severity-error
                                                           text     = 'Unable to store attachment' ) ) INTO TABLE reported-invoice.
          s3_successfull = abap_false.
      ENDTRY.

      IF s3_successfull EQ abap_true.
        invoice_entity-Filename = storage_helper->get_filename( invoice_entity-TmpFilename ).
        invoice_entity-MimeType = invoice_entity-TmpMimetype.
      ELSE.
        CLEAR invoice_entity-Filename.
        CLEAR invoice_entity-MimeType.
      ENDIF.

      CLEAR invoice_entity-TmpAttachment.
      CLEAR invoice_entity-TmpFilename.
      CLEAR invoice_entity-TmpMimetype.




      MODIFY ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
      ENTITY Invoice
      UPDATE FIELDS ( TmpAttachment TmpFilename TmpMimetype Filename Mimetype )
        WITH VALUE #( ( %key   =  invoice_entity-%key
                  %is_draft     = invoice_entity-%is_draft
                  TmpAttachment = invoice_entity-TmpAttachment
                  TmpFilename = invoice_entity-TmpFilename
                  TmpMimetype = invoice_entity-TmpMimetype
                  Filename = invoice_entity-Filename
                  MimeType =  invoice_entity-MimeType
                  ) )
                  FAILED DATA(failed_update)
                  REPORTED DATA(reported_update).

    ENDLOOP.
  ENDMETHOD.

  METHOD populateFieldsFromAttachment.
    DATA: ls_invoice TYPE zdm_c_invhdrtp,
          lt_items   TYPE ztt_invitemline.
    " Modify in local mode: BO-related updates that are not relevant for authorization checks
    READ ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
        ENTITY Invoice
          ALL FIELDS
          WITH CORRESPONDING #( keys )
        RESULT DATA(invoices).

    LOOP AT invoices INTO DATA(invoice_entity).
      IF invoice_entity-TmpAttachment IS INITIAL.
        CONTINUE.
      ENDIF.
      DATA: invoice_cp TYPE zdm_c_invhdrtp.
      invoice_cp = CORRESPONDING #(  invoice_entity ) .
      get_extract_results(  EXPORTING iv_invoice = invoice_cp IMPORTING es_invoice = ls_invoice et_lineitems = lt_items ).

      MODIFY ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE
          ENTITY Invoice
          UPDATE FIELDS (  VendorName VendorAddress DueDate InvoiceReceiptDate Total Subtotal Tax AmountDue VendorTaxNumber ExtInvoiceID PONum )
            WITH VALUE #( ( %key   =  invoice_entity-%key
                      %is_draft     = invoice_entity-%is_draft
                      VendorName  =  ls_invoice-VendorName
                      VendorAddress = ls_invoice-VendorAddress
                      DueDate = ls_invoice-DueDate
                      InvoiceReceiptDate = ls_invoice-InvoiceReceiptDate
                      Total = ls_invoice-Total
                      Subtotal = ls_invoice-Subtotal
                      Tax = ls_invoice-Tax
                      AmountDue = ls_invoice-AmountDue
                      VendorTaxNumber = ls_invoice-VendorTaxNumber
                      ExtInvoiceID = ls_invoice-ExtInvoiceID
                      PONum = ls_invoice-PONum
                      ) )
                      FAILED DATA(failed_update)
                      REPORTED DATA(reported_update).



      MODIFY ENTITIES OF zdm_r_invhdrtp IN LOCAL MODE ENTITY Invoice CREATE BY \_InvoiceItems AUTO FILL CID
                FIELDS ( InvoiceID Description ItemNum LinePrice Quantity UnitPrice ) WITH VALUE #( FOR key IN keys (

                             %is_draft = if_abap_behv=>mk-on
                             %key   =  invoice_entity-%key
                             %target = VALUE #(
                              FOR item IN lt_items (
                              %is_draft = if_abap_behv=>mk-on
                                InvoiceID = key-InvoiceID
                                Description = item-description
                                LinePrice = item-line_price
                                UnitPrice = item-unit_price
                                Quantity = item-quantity ) ) ) )
           REPORTED DATA(reported1) FAILED DATA(failed1) MAPPED DATA(mapped1).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_extract_results.
    DATA(o_extract) = NEW zdm_cl_aws_invoice_extract( ).
    DATA: message TYPE char100.
    DATA: lt_header_fields TYPE ztt_aws_keyvalue.
    DATA: value TYPE string.
    es_invoice = iv_invoice.
    o_extract->get_text_from_document( EXPORTING iv_bytes =  iv_invoice-TmpAttachment  ).

    et_lineitems = o_extract->get_lineitems(  ).
    es_invoice-VendorAddress = o_extract->get_field( 'VENDOR_ADDRESS' ).
    es_invoice-VendorName = o_extract->get_field( 'VENDOR_NAME' ).
    es_invoice-DueDate = o_extract->get_field( iv_field = 'DUE_DATE' iv_type = 'DATE' ).
    es_invoice-InvoiceReceiptDate = o_extract->get_field( iv_field = 'INVOICE_RECEIPT_DATE' iv_type = 'DATE' ).
    es_invoice-Total = o_extract->get_field( iv_field = 'TOTAL' iv_type = 'COST' ).
    es_invoice-AmountDue = o_extract->get_field( iv_field = 'AMOUNT_DUE' iv_type = 'COST' ).
    es_invoice-Tax = o_extract->get_field( iv_field = 'TAX' iv_type = 'COST' ).
    es_invoice-Subtotal = o_extract->get_field( iv_field = 'SUBTOTAL' iv_type = 'COST' ).
    es_invoice-VendorTaxNumber = o_extract->get_field( 'TAX_PAYER_ID' ).
    es_invoice-PONum = o_extract->get_field( 'PO_NUMBER' ).
    es_invoice-ExtInvoiceID = o_extract->get_field( 'INVOICE_RECEIPT_ID' ).


  ENDMETHOD.


ENDCLASS.
