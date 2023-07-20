*&---------------------------------------------------------------------*
*& Include          ZBOOK_RAP_SANS_OYUNU_MODULE
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
* SET TITLEBAR 'xxx'.


  LOOP AT SCREEN.
    IF screen-group1 = 'GX'.
      IF gv_bakiye < 50.
        screen-input = 0.
        MESSAGE 'Bakiye yetersiz.' TYPE 'I'.
      ELSEIF gv_bakiye >= 50.
        screen-input = 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.



MODULE user_command_0100 INPUT.

  CASE sy-ucomm. " butona tıklayınca buonun arkasındaki fonksiyon çalışıryani, geri gitmek için falan burada yazarız.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      LEAVE PROGRAM.
    WHEN 'CEKILIS'.
      go_report->get_numbers( ).
    WHEN 'ADD_MONEY'.
      CALL SCREEN 0200 STARTING AT 10 05 ENDING AT 70 15.
    WHEN OTHERS.

  ENDCASE.



ENDMODULE.






MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
* SET TITLEBAR 'xxx'.
ENDMODULE.




MODULE user_command_0200 INPUT.
  CASE sy-ucomm. " butona tıklayınca buonun arkasındaki fonksiyon çalışıryani, geri gitmek için falan burada yazarız.
    WHEN '&OK'.
      go_report->bakiye_ekle( ).
    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.



MODULE status_0300 OUTPUT.
  SET PF-STATUS 'STATUS_0300'.
* SET TITLEBAR 'xxx'.
  go_report->set_screen( ).
ENDMODULE.


MODULE user_command_0300 INPUT.
  CASE sy-ucomm. " butona tıklayınca buonun arkasındaki fonksiyon çalışıryani, geri gitmek için falan burada yazarız.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      LEAVE PROGRAM.
    WHEN 'TAKE_TICKET'.
      IF gv_ticket_count IS NOT INITIAL AND gv_ticket_date IS NOT INITIAL AND gv_ticket_kind IS NOT INITIAL.
        PERFORM gecmis_cekilis_kontrolu.
        IF sy-subrc EQ 0.


          go_milli->bilet_al( ).
          go_alv->display_salv_bilet( ).
          CLEAR: gv_ticket_count,
                 gv_ticket_date,
                 gv_ticket_kind.
        ENDIF.
      ELSE.
        MESSAGE 'Tüm alanlar dolu olmak zorundadır' TYPE 'I'.
      ENDIF.

    WHEN 'MAKE_DRAW'.
      go_milli->cekilis_yap( ).
      IF check EQ 'xo'.
        go_alv->display_salv_piyango( ).
        check = 'xo'.
      ENDIF.

    WHEN 'SHOW_TICKETS'.
      SELECT *
          FROM zbook_bilet_tab
          INTO TABLE @DATA(lt_bilet_table)
          WHERE kullanici EQ @sy-uname AND bilet_tarihi EQ @gv_ticket_date.
      cl_salv_table=>factory(
      IMPORTING
            r_salv_table   =  lo_alv
      CHANGING
            t_table        = lt_bilet_table ).

      lo_alv->display( ).

    WHEN '&ALL_DRAW'.
      SELECT *
         FROM zbook_piyangotab
         INTO TABLE @DATA(lt_piyango_table).
      cl_salv_table=>factory(
      IMPORTING
            r_salv_table   =  lo_alv
      CHANGING
            t_table        = lt_piyango_table ).

      lo_alv->display( ).
    WHEN '&CHECK_T'.
      MESSAGE 'Henüz bir çekiliş yapılkmadı. Lütfen önce çekiliş yapın.' TYPE 'I'.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form GECMIS_CEKILIS_KONTROLU
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gecmis_cekilis_kontrolu .
  SELECT tarih
    FROM zbook_piyangotab
    INTO TABLE @DATA(lt_temp_tarih).

  TRY.
      DATA(row) = lt_temp_tarih[ tarih = gv_ticket_date ].
    CATCH cx_sy_itab_line_not_found.

  ENDTRY.

  IF row IS NOT INITIAL.
    MESSAGE 'Çekilişi yapılan tarihe bilet alınamaz' TYPE 'I'.
    sy-subrc = 4.
    CLEAR row.
  ENDIF.


ENDFORM.
