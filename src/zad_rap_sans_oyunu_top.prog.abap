*&---------------------------------------------------------------------*
*& Include          ZBOOK_RAP_SANS_OYUNU_TOP
*&---------------------------------------------------------------------*

CLASS: lcl_lottery DEFINITION DEFERRED,
       lcl_milli DEFINITION DEFERRED,
       lcl_handle_events DEFINITION DEFERRED,
       lcl_alv DEFINITION DEFERRED.
*TABLES: sscrfields.
TYPES: BEGIN OF gty_out,
         out1 TYPE int2,
         out2 TYPE int2,
         out3 TYPE int2,
         out4 TYPE int2,
         out5 TYPE int2,
         out6 TYPE int2,
       END OF gty_out.

TYPES: BEGIN OF gty_in,
         in1 TYPE int2,
         in2 TYPE int2,
         in3 TYPE int2,
         in4 TYPE int2,
         in5 TYPE int2,
         in6 TYPE int2,
       END OF gty_in.

TYPES: BEGIN OF gty_card,
         in1 TYPE int2,
         in2 TYPE int2,
         in3 TYPE int2,
         in4 TYPE int2,
       END OF gty_card.

DATA gv_check_rb TYPE int1 VALUE 0 .
DATA: go_report           TYPE REF TO lcl_lottery,
      go_milli            TYPE REF TO lcl_milli,
      go_alv              TYPE REF TO lcl_alv,
      event_handler       TYPE REF TO lcl_handle_events,
      gs_in               TYPE gty_in,
      gs_out              TYPE gty_out,
      match               TYPE char1,
      control             TYPE char1 VALUE abap_false,
      gv_bakiye           TYPE int4 VALUE 500,
      gv_eklenecek_miktar TYPE int4,
      gs_card_num         TYPE gty_card,
      gv_cvv_num          TYPE int2,
      gv_card_date        TYPE datum,
      gv_flag             TYPE int1 VALUE 0,
      gv_ticket_count     TYPE int2,
      gv_ticket_date      TYPE datum,
      gv_ticket_kind      TYPE zbook_ticket_type_de,
      gv_draw_date        TYPE datum.

DATA: lt_res_piyango    TYPE TABLE OF zbook_piyangotab,
      gt_bilet          TYPE TABLE OF zbook_bilet_tab,
      gs_bilet          TYPE zbook_bilet_tab,
      gs_selected_bilet TYPE zbook_bilet_tab,
      lo_alv_events     TYPE REF TO lcl_handle_events,
      lo_alv            TYPE REF TO cl_salv_table,
      gt_row            TYPE salv_t_row,
      gs_show_draw      TYPE zbook_piyangotab,
              ls_bilet_modify    TYPE zbook_bilet_tab,
      gt_show_draw      TYPE TABLE OF zbook_piyangotab,
      check             TYPE char2.
*      lt_table          TYPE zbook_bilet_tab_tt.

SELECTION-SCREEN BEGIN OF BLOCK secim WITH FRAME TITLE TEXT-001.
PARAMETERS: p_rad1 RADIOBUTTON GROUP gr1,
            p_rad2 RADIOBUTTON GROUP gr1,
            p_rad3 RADIOBUTTON GROUP gr1,
            p_rad4 RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK secim.
