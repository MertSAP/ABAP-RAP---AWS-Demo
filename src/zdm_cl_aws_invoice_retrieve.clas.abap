CLASS zdm_cl_aws_invoice_retrieve DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zdm_cl_aws_invoice_retrieve IMPLEMENTATION.
  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA lt_original_data TYPE STANDARD TABLE OF zdm_c_invhdrtp WITH DEFAULT KEY.
    lt_original_data = CORRESPONDING #( it_original_data ).

    LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<fs_original_data>).

      IF <fs_original_data>-Filename  IS NOT INITIAL.

        DATA(storage_helper) = NEW zdm_cl_aws_invoice_storage(   <fs_original_data>-InvoiceID ).
        <fs_original_data>-Attachment = storage_helper->get_object( iv_filename = <fs_original_data>-Filename ).
      ENDIF.
    ENDLOOP.

    ct_calculated_data = CORRESPONDING #(  lt_original_data ).
  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

  ENDMETHOD.

ENDCLASS.
