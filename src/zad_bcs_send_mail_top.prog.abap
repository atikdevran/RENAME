*&---------------------------------------------------------------------*
*& Include          ZBOOK_BCS_SEND_MAIL_TOP
*&---------------------------------------------------------------------*
TABLES: zbook_bcs_mail01.

CONSTANTS:
   gc_raw     TYPE char03 VALUE 'RAW'.

DATA:
  gv_mlrec         TYPE so_obj_nam,
  gv_sent_to_all   TYPE os_boolean,
  gv_email         TYPE adr6-smtp_addr,
  gv_text          TYPE bcsy_text,
  gr_send_request  TYPE REF TO cl_bcs,
  gr_bcs_exception TYPE REF TO cx_bcs,
  gr_recipient     TYPE REF TO if_recipient_bcs,
*  gr_sender        TYPE REF TO cl_sapuser_bcs,
  gr_sender        TYPE REF TO if_sender_bcs,
  gr_document      TYPE REF TO cl_document_bcs.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_send TYPE adr6-smtp_addr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002. "Seçim Parametreleri
SELECT-OPTIONS: s_mail   FOR zbook_bcs_mail01-mail_adress.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_subj TYPE so_obj_des.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
PARAMETERS: p_cont TYPE so_text255.
SELECTION-SCREEN END OF BLOCK b4.
