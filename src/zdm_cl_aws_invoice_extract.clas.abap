CLASS zdm_cl_aws_invoice_extract DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS get_text_from_document IMPORTING iv_bytes   TYPE xstring
                                   EXPORTING

                                             ev_message TYPE char100
                                             ev_header  TYPE zdm_tt_keyvalue
                                             et_items   TYPE zdm_tt_aws_extract.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS invoke_textract
      IMPORTING iv_bytes   TYPE xstring
      EXPORTING ev_result  TYPE REF TO /aws1/cl_texanalyzeexpensersp
                ev_message TYPE char100.


    CONSTANTS: cv_pfl      TYPE /aws1/rt_profile_id VALUE 'ZINVOICE',
               cv_resource TYPE  /aws1/rt_resource_logical  VALUE 'ZINVOICE_BUCKET'.
    DATA: filename TYPE zfile_name,
          o_tex    TYPE REF TO /aws1/if_tex,
          bucket   TYPE /aws1/s3_bucketname.

ENDCLASS.



CLASS zdm_cl_aws_invoice_extract IMPLEMENTATION.
  METHOD constructor.
    DATA(lo_session) = /aws1/cl_rt_session_aws=>create( cv_pfl ).
    o_tex = /aws1/cl_tex_factory=>create(
        io_session = lo_session ).
    bucket   = lo_session->resolve_lresource( cv_resource ).
  ENDMETHOD.

  METHOD get_text_from_document.
    DATA: oo_result TYPE REF TO /aws1/cl_texanalyzeexpensersp.
    invoke_textract(  EXPORTING iv_bytes  = iv_bytes IMPORTING ev_result = oo_result ev_message = ev_message ).

    LOOP AT oo_result->get_expensedocuments( ) INTO DATA(lo_expense).
      LOOP AT lo_expense->get_summaryfields( ) INTO DATA(oo_summary_field).
        IF oo_summary_field->get_type( ) IS BOUND AND oo_summary_field->get_valuedetection(  ) IS BOUND.
          INSERT VALUE #( key = oo_summary_field->get_type( )->get_text(  ) value = oo_summary_field->get_valuedetection(  )->get_text(  ) )
            INTO TABLE ev_header.
        ENDIF.


      ENDLOOP.
    ENDLOOP.

    LOOP AT lo_expense->get_lineitemgroups( ) INTO DATA(lo_groups).
      LOOP AT lo_groups->get_lineitems(  ) INTO DATA(lo_lineitems).
        DATA: ls_lineitem TYPE zsinvoice_item.
        LOOP AT lo_lineitems->get_lineitemexpensefields(  ) INTO DATA(lo_lineitemfield).
          IF lo_lineitemfield->get_type( ) IS BOUND AND lo_lineitemfield->get_valuedetection(  ) IS BOUND.
            DATA(field) = lo_lineitemfield->get_type( )->get_text(  ).

            ASSIGN COMPONENT  field OF STRUCTURE ls_lineitem TO FIELD-SYMBOL(<fs_field>).
            IF sy-subrc = 0.
              DATA(value) = lo_lineitemfield->get_valuedetection(  )->get_text(  ).
              IF value+0(1) EQ '$'.
                value = value+1.
              ENDIF.
              <fs_field> = value.
            ENDIF.
          ENDIF.
        ENDLOOP.

        INSERT ls_lineitem INTO TABLE et_items.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD invoke_textract.
    "Create an ABAP object for the document."
    DATA(lo_document) = NEW /aws1/cl_texdocument( iv_bytes  = iv_bytes  ).

    TRY.
        ev_result = o_tex->analyzeexpense(      "oo_result is returned for testing purposes."
          io_document        = lo_document
       ).
      CATCH /aws1/cx_texaccessdeniedex.
        ev_message =  'You do not have permission to perform this action.'.
      CATCH /aws1/cx_texbaddocumentex.
        ev_message =  'Amazon Textract is not able to read the document.'.
      CATCH /aws1/cx_texdocumenttoolargeex.
        ev_message =   'The document is too large.'.
      CATCH /aws1/cx_texhlquotaexceededex.
        ev_message =   'Human loop quota exceeded.'.
      CATCH /aws1/cx_texinternalservererr.
        ev_message =   'Internal server error.'.
      CATCH /aws1/cx_texinvalidparameterex.
        ev_message =   'Request has non-valid parameters.'.

      CATCH /aws1/cx_texinvalids3objectex.
        ev_message =   'Amazon S3 object is not valid.'.
      CATCH /aws1/cx_texprovthruputexcdex.
        ev_message =  'Provisioned throughput exceeded limit.'.
      CATCH /aws1/cx_texthrottlingex.
        ev_message =   'The request processing exceeded the limit.'.
      CATCH /aws1/cx_texunsupporteddocex.
        ev_message =  'The document is not supported.'.
      CATCH cx_root.
        ev_message = 'An error occured'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
