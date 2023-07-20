*&---------------------------------------------------------------------*
*& Report ZBOOK_BCS_SEND_MAIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZAD_BCS_SEND_MAIL.
INCLUDE ZAD_BCS_SEND_MAIL_TOP.
*INCLUDE: ZBOOK_BCS_SEND_MAIL_TOP,
INCLUDE ZAD_BCS_SEND_MAIL_FORM.
*         ZBOOK_BCS_SEND_MAIL_FORM.

START-OF-SELECTION.
PERFORM send_mail.
