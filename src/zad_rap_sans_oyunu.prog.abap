*&---------------------------------------------------------------------*
*& Report ZBOOK_RAP_SANS_OYUNU
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zad_rap_sans_oyunu.
INCLUDE ZAD_RAP_SANS_OYUNU_TOP.
*INCLUDE: zbook_rap_sans_oyunu_top,
INCLUDE ZAD_RAP_SANS_OYUNU_CLASS.
*         zbook_rap_sans_oyunu_class,
INCLUDE ZAD_RAP_SANS_OYUNU_MODULE.
*         zbook_rap_sans_oyunu_module.

START-OF-SELECTION.

  go_report = NEW lcl_lottery( ).
  go_milli = NEW lcl_milli( ).
  go_alv = NEW lcl_alv( ).
  event_handler = new lcl_handle_events( ).

  CASE abap_true.
    WHEN p_rad1.
      CALL SCREEN 0100.
    WHEN p_rad2.
      gv_check_rb = 1.
      CALL SCREEN 0300.
    WHEN p_rad3.
      gv_check_rb = 2.
      CALL SCREEN 0300.
    WHEN p_rad4.
      gv_check_rb = 3.
      CALL SCREEN 0300.
  ENDCASE.
