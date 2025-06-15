CLASS lsc_zdm_r_invhdrtp DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.
    METHODS adjust_numbers REDEFINITION.


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
    endif.
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
    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE invoice.
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

ENDCLASS.
