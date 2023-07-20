*&---------------------------------------------------------------------*
*& Include          ZBOOK_BCS_SEND_MAIL_FORM
*&---------------------------------------------------------------------*

FORM send_mail.


  TRY.
      "Create send request
      gr_send_request = cl_bcs=>create_persistent( ).

      IF p_send IS NOT INITIAL.
        gr_sender = cl_cam_address_bcs=>create_internet_address(
        i_address_string = p_send
        i_address_name = p_send ).
        CALL METHOD gr_send_request->set_sender
          EXPORTING
            i_sender = gr_sender.

      ELSE.
        "Email FROM...
        gr_sender = cl_sapuser_bcs=>create( sy-uname ).
        CALL METHOD gr_send_request->set_sender
          EXPORTING
            i_sender = gr_sender.
      ENDIF.


      "Email TO.
      LOOP AT s_mail.
        gv_email = s_mail-low.
        gr_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
        "Add recipient to send request
        CALL METHOD gr_send_request->add_recipient
          EXPORTING
            i_recipient = gr_recipient
            i_express   = 'X'.
      ENDLOOP.




      "Email BODY
      APPEND p_cont TO gv_text.
      gr_document = cl_document_bcs=>create_document(
                      i_type    = gc_raw
                      i_text    = gv_text
                      i_length  = '12'
                      i_subject = p_subj ).
      "Add document to send request
      CALL METHOD gr_send_request->set_document( gr_document ).


      "Send email
      CALL METHOD gr_send_request->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = gv_sent_to_all ).
      IF gv_sent_to_all = 'X'.
        WRITE 'Email sent!'.
      ENDIF.

      "Commit to send email
      COMMIT WORK.


      "Exception handling
    CATCH cx_bcs INTO gr_bcs_exception.
      WRITE:
        'Error!',
        'Error type:',
        gr_bcs_exception->error_type.
  ENDTRY.




ENDFORM.
