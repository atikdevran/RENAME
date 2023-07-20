*&---------------------------------------------------------------------*
*& Report ZBOOK_EDITABLE_ALV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZAD_EDITABLE_ALV.

INCLUDE ZAD_SALV_BUDDY_CLS.
*INCLUDE: zcl_salv_buddy,
INCLUDE ZAD_EDITABLE_ALV_MODULE.
*zbook_editable_alv_module.

TYPES: BEGIN OF ty_line,
         molga TYPE t001p-molga,
         juper TYPE t001p-juper,
         style TYPE lvc_t_styl.
TYPES: END OF ty_line.

TYPES ty_t_table TYPE TABLE OF ty_line WITH NON-UNIQUE KEY molga.

DATA: go_container   TYPE REF TO cl_gui_custom_container,
  go_alv         TYPE REF TO cl_salv_table,
  gt_table       TYPE ty_t_table,
  gv_okcode_0100 TYPE sy-ucomm.

CLASS lcl_alv_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
          e_ucomm er_data_changed sender.
ENDCLASS.

CLASS lcl_alv_event_handler IMPLEMENTATION.
  METHOD handle_data_changed.
    DATA: lv_juper TYPE t001p-juper.

    READ TABLE er_data_changed->mt_mod_cells ASSIGNING FIELD-SYMBOL(<fs_mod_cells>) INDEX 1.
    CHECK sy-subrc = 0.

    er_data_changed->get_cell_value(
      EXPORTING
        i_row_id    = <fs_mod_cells>-row_id
        i_tabix     = <fs_mod_cells>-tabix
        i_fieldname = <fs_mod_cells>-fieldname
      IMPORTING
        e_value     = lv_juper ).

    READ TABLE gt_table ASSIGNING FIELD-SYMBOL(<fs_table>) INDEX <fs_mod_cells>-row_id.
    CHECK sy-subrc = 0.

    MESSAGE |Data has changed! Old value = { <fs_table>-juper }, new value = { lv_juper }| TYPE if_xo_const_message=>info.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CALL SCREEN '0100'.


FORM f_create_objects.

  DATA: lo_columns TYPE REF TO cl_salv_columns_table,
        lo_display TYPE REF TO cl_salv_display_settings.

  CHECK go_alv IS NOT BOUND.

  CREATE OBJECT go_container
    EXPORTING
      container_name = 'SALV_CONTAINER'.


  cl_salv_table=>factory(
    EXPORTING
*      r_container = go_container
      r_container = cl_gui_custom_container=>default_screen
    IMPORTING
      r_salv_table = go_alv
    CHANGING
      t_table = gt_table ).

  gt_table[] = VALUE #( ( molga = '37' juper = '0001' )
                        ( molga = '37' juper = '0002' )
                        ( molga = '37' juper = '0003' )
                        ( molga = '37' juper = '0004' )
                        ( molga = '37' juper = '0005' ) ).

  lo_columns = go_alv->get_columns( ).
  lo_columns->set_optimize( ).

  lo_display = go_alv->get_display_settings( ).
  lo_display->set_striped_pattern( abap_true ).

  go_alv->display( ).

  lcl_salv_buddy=>set_editable(
    EXPORTING
*      i_row        = 4
      i_col        = 'JUPER'
      i_salv_table = go_alv
      i_editable   = abap_true ).

  SET HANDLER lcl_alv_event_handler=>handle_data_changed FOR ALL INSTANCES ACTIVATION abap_true.

ENDFORM.

FORM f_user_command.

  DATA: lo_alv_grid TYPE REF TO cl_gui_alv_grid,
        lv_valid    TYPE abap_bool.

  lo_alv_grid ?= lcl_salv_buddy=>get_control( go_alv ).
  lo_alv_grid->check_changed_data(
    IMPORTING
      e_valid = lv_valid ).
  CHECK lv_valid = abap_true.

  CASE gv_okcode_0100.
    WHEN 'OPEN'.
      lcl_salv_buddy=>set_editable(
        EXPORTING
*          i_row        = 4
          i_col        = 'JUPER'
          i_salv_table = go_alv
          i_editable   = abap_true ).
    WHEN 'CLOSE'.
      lcl_salv_buddy=>set_editable(
        EXPORTING
*          i_row        = 4
          i_col        = 'JUPER'
          i_salv_table = go_alv
          i_editable   = abap_false ).
   WHEN 'EXIT'.
     LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
