CLASS zdm_cl_aws_invoice_storage DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_invoice_id TYPE zdm_int_invnum.

    METHODS
      get_object
        IMPORTING
          iv_filename      TYPE zfile_name
        RETURNING
          VALUE(rv_object) TYPE xstring.
    METHODS
      delete_object
        IMPORTING
          iv_filename       TYPE zfile_name
        RETURNING
          VALUE(rv_success) TYPE abap_bool.

    METHODS
      put_object
        IMPORTING
          iv_filename       TYPE zfile_name
          iv_old_filename   TYPE zfile_name
          iv_body           TYPE xstring
        RETURNING
          VALUE(rv_success) TYPE xstring.
    METHODS get_filename
      IMPORTING
                iv_filename        TYPE zfile_name
      RETURNING VALUE(rv_filename) TYPE zfile_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: o_s3       TYPE REF TO /aws1/if_s3,
          invoice_id TYPE zdm_int_invnum,
          bucket     TYPE /aws1/s3_bucketname.
    CONSTANTS: cv_pfl      TYPE /aws1/rt_profile_id VALUE 'ZINVOICE',
               cv_resource TYPE  /aws1/rt_resource_logical  VALUE 'ZINVOICE_BUCKET'.
ENDCLASS.



CLASS zdm_cl_aws_invoice_storage IMPLEMENTATION.
  METHOD constructor.
    DATA(lo_session) = /aws1/cl_rt_session_aws=>create( cv_pfl ).
    o_s3 = /aws1/cl_s3_factory=>create( lo_session ).
    bucket   = lo_session->resolve_lresource( cv_resource ).
    invoice_id = iv_invoice_id.
  ENDMETHOD.

  METHOD get_object.
    DATA(oo_result) = o_s3->getobject(
      iv_bucket = bucket
      iv_key    = CONV /aws1/s3_objectkey( iv_filename ) ).
    rv_object = oo_result->get_body( ).
  ENDMETHOD.

  METHOD delete_object.
    o_s3->deleteobject(
      iv_bucket = bucket
      iv_key    = CONV /aws1/s3_objectkey( iv_filename )
    ).
  ENDMETHOD.

  METHOD put_object.
    DATA(new_filename) = get_filename( iv_filename = iv_filename ).
    IF iv_old_filename IS NOT INITIAL AND iv_filename NE iv_old_filename.
      delete_object( iv_filename = iv_old_filename ).
    ENDIF.

    o_s3->putobject(
      iv_bucket = bucket
      iv_key    = CONV /aws1/s3_objectkey( new_filename )
      iv_body   = iv_body
    ).
  ENDMETHOD.

  METHOD get_filename.
    DATA(cl_file) = cl_fs_path=>create( iv_filename ).
    rv_filename = |{ invoice_id }{ cl_file->get_file_extension( ) }|.
  ENDMETHOD.

ENDCLASS.
