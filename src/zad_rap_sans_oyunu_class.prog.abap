*&---------------------------------------------------------------------*
*& Include          ZBOOK_RAP_SANS_OYUNU_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_lottery DEFINITION.

  PUBLIC SECTION.
    METHODS: random_numbers,
      check_draw_numbers,
      get_numbers,
      show_results,
      check_inputs,
      bakiye_ekle,
      set_screen.

    DATA: lt_out     TYPE TABLE OF zbook_input_tabl,
          lt_in      TYPE TABLE OF zbook_input_tabl,
          ls_tab     TYPE zbook_input_tabl,
          lv_message TYPE char40.


ENDCLASS. "lcl_lottery DEFINITION



CLASS lcl_lottery IMPLEMENTATION.

  METHOD random_numbers.
    DATA: lv_temp TYPE qf00-ran_int.
    CLEAR: lt_out, control.
    DO 6 TIMES.
      CALL FUNCTION 'QF05_RANDOM_INTEGER'
        EXPORTING
          ran_int_max = 49
          ran_int_min = 1
        IMPORTING
          ran_int     = lv_temp.
      ls_tab-num = lv_temp.
      APPEND ls_tab TO lt_out.
    ENDDO.
    me->check_draw_numbers( ).
  ENDMETHOD.




  METHOD check_draw_numbers.
    CALL FUNCTION 'ZBOOK_ASAL_TEK_CIFT'
      EXPORTING
        lt_tab    = lt_out
      IMPORTING
        lv_result = control.

    IF control EQ abap_true.
      READ TABLE lt_out INTO DATA(ls_temp) INDEX 1.
      gs_out-out1 = ls_temp-num.

      READ TABLE lt_out INTO ls_temp INDEX 2.
      gs_out-out2 = ls_temp-num.

      READ TABLE lt_out INTO ls_temp INDEX 3.
      gs_out-out3 = ls_temp-num.

      READ TABLE lt_out INTO ls_temp INDEX 4.
      gs_out-out4 = ls_temp-num.

      READ TABLE lt_out INTO ls_temp INDEX 5.
      gs_out-out5 = ls_temp-num.

      READ TABLE lt_out INTO ls_temp INDEX 6.
      gs_out-out6 = ls_temp-num.

      IF ( abs( gs_out-out1 - gs_out-out2 ) = 1  AND abs( gs_out-out2 - gs_out-out3 ) = 1 OR
      abs( gs_out-out2 - gs_out-out3 ) = 1  AND abs( gs_out-out3 - gs_out-out4 ) = 1 OR
      abs( gs_out-out3 - gs_out-out4 ) = 1  AND abs( gs_out-out4 - gs_out-out5 ) = 1 OR
      abs( gs_out-out4 - gs_out-out5 ) = 1  AND abs( gs_out-out5 - gs_out-out6 ) = 1 ) .
        CLEAR gs_out.
        me->random_numbers( ).
      ENDIF.

    ELSE.
      me->random_numbers( ).
    ENDIF.

  ENDMETHOD.



  METHOD get_numbers.

    CLEAR: lt_in, control.

    IF ( gs_in-in1 IS NOT INITIAL AND
         gs_in-in2 IS NOT INITIAL AND
         gs_in-in3 IS NOT INITIAL AND
         gs_in-in4 IS NOT INITIAL AND
         gs_in-in5 IS NOT INITIAL AND
         gs_in-in6 IS NOT INITIAL ) .

      ls_tab-num = gs_in-in1.
      APPEND ls_tab TO lt_in.

      ls_tab-num = gs_in-in2.
      APPEND ls_tab TO lt_in.

      ls_tab-num = gs_in-in3.
      APPEND ls_tab TO lt_in.

      ls_tab-num = gs_in-in4.
      APPEND ls_tab TO lt_in.

      ls_tab-num = gs_in-in5.
      APPEND ls_tab TO lt_in.

      ls_tab-num = gs_in-in6.
      APPEND ls_tab TO lt_in.

    ENDIF.
    me->check_inputs( ).
  ENDMETHOD.



  METHOD check_inputs.
    IF  lt_in IS NOT INITIAL.
      CALL FUNCTION 'ZBOOK_ASAL_TEK_CIFT'
        EXPORTING
          lt_tab    = lt_in   " 2 baytlık tamsayı (işaretli)
        IMPORTING
          lv_result = control    " Tek basamaklı gösterge
          ev_msg    = lv_message.
    ELSE.
      MESSAGE 'Tüm alanların doldurulması gerekiyor.' TYPE 'I'.
      EXIT.
    ENDIF.

    LOOP AT lt_in INTO DATA(ls_tab).
      IF ls_tab-num GT 49.
        DATA(flg) = 'X'.
      ENDIF.
    ENDLOOP.

    IF control NE 'X'.
      MESSAGE lv_message TYPE 'I'.
      EXIT.
    ELSEIF ( abs( gs_in-in1 - gs_in-in2 ) = 1  AND abs( gs_in-in2 - gs_in-in3 ) = 1 OR
             abs( gs_in-in2 - gs_in-in3 ) = 1  AND abs( gs_in-in3 - gs_in-in4 ) = 1 OR
             abs( gs_in-in3 - gs_in-in4 ) = 1  AND abs( gs_in-in4 - gs_in-in5 ) = 1 OR
             abs( gs_in-in4 - gs_in-in5 ) = 1  AND abs( gs_in-in5 - gs_in-in6 ) = 1 ) .
      CLEAR gs_in.
      EXIT.
    ELSEIF flg EQ 'X'.
      MESSAGE 'Sayılar 1-49 aralığında olmalı girilen.' TYPE 'I'.
    ELSE.
      me->random_numbers( ).
      me->show_results( ).
    ENDIF.
  ENDMETHOD.



  METHOD show_results.
    DATA: counter      TYPE int2 VALUE 0,
          lt_prize     TYPE TABLE OF zbook_prize_tab,
          ls_prize     TYPE zbook_prize_tab,
          prize_text   TYPE char100,
          counter_char TYPE string,
          prize_char   TYPE string.


    SORT lt_out[] BY num.
    LOOP AT lt_in INTO DATA(ls_in).
      READ TABLE lt_out TRANSPORTING NO FIELDS WITH KEY num = ls_in-num BINARY SEARCH.
      IF sy-subrc EQ 0.
        counter = counter + 1.
      ENDIF.
    ENDLOOP.

    IF counter GT 1.
      SELECT SINGLE prize
        FROM zbook_prize_tab
        INTO @DATA(lv_prize)
        WHERE bilinen EQ @counter.
      gv_bakiye = gv_bakiye - 50.
      gv_bakiye = gv_bakiye + lv_prize.
      DATA(lv_res_text) = ' Alan Bildiniz Ödülünüz {'.
      counter_char = counter.
      prize_char = lv_prize.

      CONCATENATE counter_char lv_res_text prize_char ' TL }' INTO prize_text.
      MESSAGE prize_text TYPE 'I'.
    ELSE.
      gv_bakiye = gv_bakiye - 50.
      lv_prize = 0.
      MESSAGE 'Hiçbir alanı tutturamadınız.' TYPE 'I'.
*      clear: gs_in , gs_out.

    ENDIF.


  ENDMETHOD.




  METHOD bakiye_ekle.
    DATA: lv_control TYPE char1.
****** Kart alanları kontrol etmek için
    IF ( ( gs_card_num-in1 GT 999 AND gs_card_num-in1 LT 10000 ) AND
      ( gs_card_num-in2 GT 999 AND gs_card_num-in2 LT 10000 ) AND
      ( gs_card_num-in3 GT 999 AND gs_card_num-in3 LT 10000 ) AND
      ( gs_card_num-in4 GT 999 AND gs_card_num-in4 LT 10000 ) ).

      IF ( gv_cvv_num LT 1000 ) .

        IF ( gv_card_date IS NOT INITIAL ) .
          gv_bakiye = gv_bakiye + gv_eklenecek_miktar.
          MESSAGE 'Yükleme Başarıyla gerçekleşti, ana sayfaya yönlendiriliyorsunuz.' TYPE 'I'.
          CLEAR: gv_eklenecek_miktar, gs_card_num, gv_card_date , gv_cvv_num .
          CALL SCREEN 0100.
        ELSE.
          MESSAGE 'Kartın son kullanım tarihini girmeniz gerekmektedir.' TYPE 'I'.
          EXIT.
        ENDIF.

      ELSE.
        MESSAGE 'CVV numarası 3 haneli olmalıdır. Başında 0 olan CVV numaralarını 0 olmadan da girebilirsiniz.' TYPE 'I'.
        EXIT.
      ENDIF.

    ELSE.
      MESSAGE 'Kart numarası 16 haneli olmalıdır!' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDMETHOD.




  METHOD set_screen.

    LOOP AT SCREEN.

      IF screen-group1 = 'G1'.
        CASE gv_check_rb.
          WHEN 1.
            screen-active = 1.
            screen-invisible = 0.
            MODIFY SCREEN.
          WHEN OTHERS.
            screen-active = 0.
            screen-invisible = 1.
            MODIFY SCREEN.
        ENDCASE.
      ELSEIF screen-group1 = 'G2'.
        CASE gv_check_rb.
          WHEN 2.
            screen-active = 1.
            screen-invisible = 0.
            MODIFY SCREEN.
          WHEN OTHERS.
            screen-active = 0.
            screen-invisible = 1.
            MODIFY SCREEN.
        ENDCASE.
      ELSEIF screen-group1 = 'G3'.
        CASE gv_check_rb.
          WHEN 2.
            screen-active = 1.
            screen-invisible = 0.
            MODIFY SCREEN.
          WHEN OTHERS.
            screen-active = 0.
            screen-invisible = 1.
            MODIFY SCREEN.
        ENDCASE.

      ELSEIF screen-group1 = 'G4'.
        CASE gv_check_rb.
          WHEN 3.
            screen-active = 1.
            screen-invisible = 0.
            MODIFY SCREEN.
          WHEN OTHERS.
            screen-active = 0.
            screen-invisible = 1.
            MODIFY SCREEN.
        ENDCASE.

      ELSEIF screen-group1 = 'G5'.
        CASE gv_check_rb.
          WHEN 2.
            screen-active = 0.
            screen-invisible = 1.
            MODIFY SCREEN.
          WHEN OTHERS.
            screen-active = 1.
            screen-invisible = 0.
            MODIFY SCREEN.
        ENDCASE.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.






ENDCLASS. "lcl_lottery IMPLEMENTATION

**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************


CLASS lcl_milli DEFINITION.

  PUBLIC SECTION.
    METHODS: bilet_al,
      create_random_draw_numbers IMPORTING iv_loop_count TYPE int2
                                           iv_max_num    TYPE int4
                                           iv_min_num    TYPE int4
                                 EXPORTING ev_value      TYPE string ,
      cekilis_yap,
      biletleri_kontrol_et,
      biletlerimi_goster.
*    DATA: lt_res_piyango TYPE TABLE OF zbook_piyangotab .


ENDCLASS. "lcl_milli DEFINITION




CLASS lcl_milli IMPLEMENTATION.
* ZBOOK_RANDOM_COUNTS - random fonksiyonu
* Zbook_piyangotab - çekiliş tablosu
* zbook_bilet_tab - alınan biletlerin tablosu

  METHOD bilet_al.
    DATA: it_table TYPE zbook_input_table_type.
*          ls_bilet TYPE zbook_bilet_tab.
    CLEAR: gt_bilet, gs_bilet.
    CALL FUNCTION 'ZBOOK_RANDOM_COUNTS'
      EXPORTING
        iv_loop_count   = gv_ticket_count
        iv_max_num      = 9999999
        iv_min_num      = 1000000
      IMPORTING
        et_random_table = it_table.

    LOOP AT it_table INTO DATA(ls_tab).
      gs_bilet-kullanici = sy-uname.
      gs_bilet-bilet_tarihi = gv_ticket_date.
      gs_bilet-bilet_numarasi = ls_tab-num.
      gs_bilet-bilet_niteligi = gv_ticket_kind.
      gs_bilet-mandt = sy-mandt.

      APPEND gs_bilet TO gt_bilet.
      CLEAR gs_bilet.
    ENDLOOP.

  ENDMETHOD.


  METHOD cekilis_yap.

    DATA: ls_temp          TYPE zbook_piyangotab,
          lv_temp_draw_res TYPE string.

    SELECT COUNT(*)
      FROM zbook_piyangotab
      INTO @DATA(lv_tab_row).



****************************************** 7'li ******************************************************
    me->create_random_draw_numbers(
      EXPORTING
        iv_loop_count = 1
        iv_max_num    = 9999999
        iv_min_num    = 1000000
      IMPORTING
        ev_value      = lv_temp_draw_res  ).
    ls_temp-yedili = lv_temp_draw_res.
    CLEAR lv_temp_draw_res.
****************************************** 6'lı ******************************************************
    me->create_random_draw_numbers(
      EXPORTING
        iv_loop_count = 5
        iv_max_num    = 999999
        iv_min_num    = 100000
      IMPORTING
        ev_value      = lv_temp_draw_res  ).
    ls_temp-altili = lv_temp_draw_res.
    CLEAR lv_temp_draw_res.
****************************************** 5'li ******************************************************
    me->create_random_draw_numbers(
      EXPORTING
        iv_loop_count = 10
        iv_max_num    = 99999
        iv_min_num    = 10000
      IMPORTING
        ev_value      = lv_temp_draw_res  ).
    ls_temp-besli = lv_temp_draw_res.
    CLEAR lv_temp_draw_res.
****************************************** 4'lü ******************************************************
    me->create_random_draw_numbers(
      EXPORTING
        iv_loop_count = 15
        iv_max_num    = 9999
        iv_min_num    = 1000
      IMPORTING
        ev_value      = lv_temp_draw_res  ).
    ls_temp-dortlu = lv_temp_draw_res.
    CLEAR lv_temp_draw_res.
****************************************** 3'li ******************************************************
    me->create_random_draw_numbers(
  EXPORTING
    iv_loop_count = 20
    iv_max_num    = 999
    iv_min_num    = 100
  IMPORTING
    ev_value      = lv_temp_draw_res  ).
    ls_temp-uclu = lv_temp_draw_res.
    CLEAR lv_temp_draw_res.
****************************************** 2'li ******************************************************
    me->create_random_draw_numbers(
    EXPORTING
     iv_loop_count = 25
     iv_max_num    = 99
     iv_min_num    = 10
    IMPORTING
     ev_value      = lv_temp_draw_res  ).
    ls_temp-ikili = lv_temp_draw_res.
    CLEAR lv_temp_draw_res.

****************************************** tekli ******************************************************
    me->create_random_draw_numbers(
    EXPORTING
    iv_loop_count = 2
    iv_max_num    = 9
    iv_min_num    = 1
    IMPORTING
    ev_value      = lv_temp_draw_res  ).
    ls_temp-tek = lv_temp_draw_res.
    CLEAR lv_temp_draw_res.

    ls_temp-tarih = gv_draw_date.
    ls_temp-mandt = sy-mandt.
    INSERT zbook_piyangotab FROM ls_temp.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
      check = 'xo'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE 'Bugüne ait mevcut bir çekiliş var. Farklı bir gün için çekiliş tekrarlayabilirsiniz.' TYPE 'I'.
    ENDIF.
    APPEND ls_temp TO gt_show_draw.

    CLEAR: ls_temp.



  ENDMETHOD.








  METHOD biletleri_kontrol_et.
*    data check_tab type zbook_bilet_tab.




  ENDMETHOD.












  METHOD create_random_draw_numbers.

    DATA:lv_temp  TYPE char10,
         it_table TYPE zbook_input_table_type,
         lv_space TYPE char1 VALUE '-'.
    CLEAR ev_value.

    CALL FUNCTION 'ZBOOK_RANDOM_COUNTS'
      EXPORTING
        iv_loop_count   = iv_loop_count
        iv_max_num      = iv_max_num
        iv_min_num      = iv_min_num
      IMPORTING
        et_random_table = it_table.

    LOOP AT it_table INTO DATA(ls_data).
      lv_temp = ls_data-num.
      CONCATENATE lv_temp lv_space ev_value INTO ev_value.
    ENDLOOP.

  ENDMETHOD.





  METHOD biletlerimi_goster.
  ENDMETHOD.





ENDCLASS. "lcl_milli IMPLEMENTATION


**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS: on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

    DATA: lt_selected_rows TYPE lvc_t_row,
          ls_selected_rows TYPE lvc_s_row,
          lv_lines         TYPE i,
          lo_alvgrid       TYPE REF TO cl_gui_alv_grid.

ENDCLASS. "lcl_handle_events DEFINITION


CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    CASE e_salv_function.
      WHEN '&BILET'.
        DATA: lt_temp_bilet TYPE TABLE OF zbook_bilet_tab,
              lv_message    TYPE string,
              lv_row_str    TYPE string.

        PERFORM dynp_0200_handle_selection.
        LOOP AT gt_row INTO DATA(ls_rows).
          READ TABLE gt_bilet ASSIGNING FIELD-SYMBOL(<lfs_output>) INDEX ls_rows.
          IF sy-subrc = 0.
            gs_bilet-mandt = <lfs_output>-mandt.
            gs_bilet-kullanici = <lfs_output>-kullanici.
            gs_bilet-bilet_tarihi = <lfs_output>-bilet_tarihi.
            gs_bilet-bilet_numarasi = <lfs_output>-bilet_numarasi.
            gs_bilet-bilet_niteligi = <lfs_output>-bilet_niteligi.
*            APPEND gs_bilet TO lt_temp_bilet.
            INSERT zbook_bilet_tab FROM gs_bilet .
            CLEAR gs_bilet.
          ENDIF.
        ENDLOOP.

        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
        lv_row_str = lines( gt_row ).
        lv_message = 'Toplam Alınan Bilet Miktarı : '.
        CONCATENATE lv_message lv_row_str INTO lv_message.
        MESSAGE lv_message TYPE 'I'.
        LEAVE TO SCREEN 0.
      WHEN '&EXIT'.
        LEAVE TO SCREEN 0.
      WHEN '&BACK'.
        LEAVE TO SCREEN 0.
      WHEN '&CHECK_T'.
        SELECT *
          FROM zbook_bilet_tab
          INTO TABLE @DATA(check_tab)
          WHERE bilet_tarihi EQ @gv_draw_date AND kullanici EQ @sy-uname.

        DATA: lv_biletno_str     TYPE string,
              lv_range           TYPE string,
              lv_check_val       TYPE string,
              lv_toplam_odul     TYPE i VALUE 0,
              lv_odul_string     TYPE string,
              lstr_tutan_sayilar TYPE string,
              lv_carpan          TYPE f,
              lv_nitelik         TYPE string.

        READ TABLE gt_show_draw INTO DATA(ls_check_draw) INDEX 1.
        LOOP AT check_tab INTO DATA(ls_check_tab).

          lv_biletno_str = ls_check_tab-bilet_numarasi.
          IF ls_check_tab-bilet_numarasi EQ ls_check_draw-yedili+0(7).
            ls_bilet_modify = ls_check_tab.
            lv_carpan = |{ ls_bilet_modify-bilet_niteligi ALPHA = IN }|.
            ls_bilet_modify-kazanilan_odul = lv_carpan * 2000.
            ls_bilet_modify-kazanan_bilet = lv_check_val.
            lv_nitelik = ls_bilet_modify-bilet_niteligi.
            lv_toplam_odul = lv_toplam_odul + ls_bilet_modify-kazanilan_odul.
            CONCATENATE lstr_tutan_sayilar ls_check_tab-bilet_numarasi ls_check_draw-yedili '  Bilet Türü:' lv_nitelik ' ## ' INTO lstr_tutan_sayilar.
            PERFORM zupdate.
            CONTINUE.
          ENDIF.

          lv_biletno_str = ls_check_tab-bilet_numarasi+1(6).
          lv_range = ls_check_draw-altili.
          SEARCH lv_range FOR lv_biletno_str.
          IF sy-subrc EQ 0.
            ls_bilet_modify = ls_check_tab.
            lv_check_val = lv_range+sy-fdpos(6).
            lv_carpan = |{ ls_bilet_modify-bilet_niteligi ALPHA = IN }|.
            ls_bilet_modify-kazanilan_odul = lv_carpan * 1500.
            lv_nitelik = ls_bilet_modify-bilet_niteligi.
            ls_bilet_modify-kazanan_bilet = lv_check_val.
            lv_toplam_odul = lv_toplam_odul + ls_bilet_modify-kazanilan_odul.
            CONCATENATE lstr_tutan_sayilar ls_check_tab-bilet_numarasi '-' lv_check_val '  Bilet Türü:' lv_nitelik ' ## ' INTO lstr_tutan_sayilar.
            PERFORM zupdate.
            CONTINUE.
          ENDIF.


          lv_biletno_str = ls_check_tab-bilet_numarasi+2(5).
          lv_range = ls_check_draw-besli.
          IF sy-subrc EQ 0.
            ls_bilet_modify = ls_check_tab.
            lv_check_val = lv_range+sy-fdpos(5).
            lv_carpan = |{ ls_bilet_modify-bilet_niteligi ALPHA = IN }|.
            ls_bilet_modify-kazanilan_odul = lv_carpan * 900.
            lv_nitelik = ls_bilet_modify-bilet_niteligi.
            ls_bilet_modify-kazanan_bilet = lv_check_val.
            lv_toplam_odul = lv_toplam_odul + ls_bilet_modify-kazanilan_odul.
            CONCATENATE lstr_tutan_sayilar ls_check_tab-bilet_numarasi '-' lv_check_val '  Bilet Türü:' lv_nitelik ' ## ' INTO lstr_tutan_sayilar.
            PERFORM zupdate.
            CONTINUE.
          ENDIF.

          lv_biletno_str = ls_check_tab-bilet_numarasi+3(4).
          lv_range = ls_check_draw-dortlu.
          SEARCH lv_range FOR lv_biletno_str.
          IF sy-subrc EQ 0.
            ls_bilet_modify = ls_check_tab.
            lv_check_val = lv_range+sy-fdpos(4).
            lv_carpan = |{ ls_bilet_modify-bilet_niteligi ALPHA = IN }|.
            ls_bilet_modify-kazanilan_odul = lv_carpan * 500.
            lv_nitelik = ls_bilet_modify-bilet_niteligi.
            ls_bilet_modify-kazanan_bilet = lv_check_val.
            lv_toplam_odul = lv_toplam_odul + ls_bilet_modify-kazanilan_odul.
            CONCATENATE lstr_tutan_sayilar ls_check_tab-bilet_numarasi '-' lv_check_val '  Bilet Türü:' lv_nitelik ' ## ' INTO lstr_tutan_sayilar.
            PERFORM zupdate.
            CONTINUE.
          ENDIF.

          lv_biletno_str = ls_check_tab-bilet_numarasi+4(3).
          lv_range = ls_check_draw-uclu.
          SEARCH lv_range FOR lv_biletno_str.
          IF sy-subrc EQ 0.
            ls_bilet_modify = ls_check_tab.
            lv_check_val = lv_range+sy-fdpos(3).
            lv_carpan = |{ ls_bilet_modify-bilet_niteligi ALPHA = IN }|.
            ls_bilet_modify-kazanilan_odul = lv_carpan * 300.
            lv_nitelik = ls_bilet_modify-bilet_niteligi.
            ls_bilet_modify-kazanan_bilet = lv_check_val.
            lv_toplam_odul = lv_toplam_odul + ls_bilet_modify-kazanilan_odul.
            CONCATENATE lstr_tutan_sayilar ls_check_tab-bilet_numarasi '-' lv_check_val '  Bilet Türü:' lv_nitelik ' ## ' INTO lstr_tutan_sayilar.
            PERFORM zupdate.
            CONTINUE.
          ENDIF.

          lv_biletno_str = ls_check_tab-bilet_numarasi+5(2).
          lv_range = ls_check_draw-ikili.
          SEARCH lv_range FOR lv_biletno_str.
          IF sy-subrc EQ 0.
            ls_bilet_modify = ls_check_tab.
            lv_check_val = lv_range+sy-fdpos(2).
            lv_carpan = |{ ls_bilet_modify-bilet_niteligi ALPHA = IN }|.
            ls_bilet_modify-kazanilan_odul = lv_carpan * 100.
            lv_nitelik = ls_bilet_modify-bilet_niteligi.
            ls_bilet_modify-kazanan_bilet = lv_check_val.
            lv_toplam_odul = lv_toplam_odul + ls_bilet_modify-kazanilan_odul.
            CONCATENATE lstr_tutan_sayilar ls_check_tab-bilet_numarasi '-' lv_check_val '  Bilet Türü:' lv_nitelik ' ## ' INTO lstr_tutan_sayilar.
            PERFORM zupdate.
            CONTINUE.
          ENDIF.


          lv_biletno_str = ls_check_tab-bilet_numarasi+6(1).
          lv_range = ls_check_draw-tek.
          SEARCH lv_range FOR lv_biletno_str.
          IF sy-subrc EQ 0.
            ls_bilet_modify = ls_check_tab.
            lv_check_val = lv_range+sy-fdpos(1).
            lv_carpan = |{ ls_bilet_modify-bilet_niteligi ALPHA = IN }|.
            ls_bilet_modify-kazanilan_odul = lv_carpan * 50.
            lv_nitelik = ls_bilet_modify-bilet_niteligi.
            ls_bilet_modify-kazanan_bilet = lv_check_val.
            lv_toplam_odul = lv_toplam_odul + ls_bilet_modify-kazanilan_odul.
            CONCATENATE lstr_tutan_sayilar ls_check_tab-bilet_numarasi '-' lv_check_val '  Bilet Türü: ' lv_nitelik ' ## ' INTO lstr_tutan_sayilar.
            PERFORM zupdate.
            CONTINUE.
          ENDIF.


        ENDLOOP.

        lv_odul_string = lv_toplam_odul.
        CONCATENATE 'Toplam kazanılan ödül miktarı:' lv_odul_string 'Tutan Sayılar:  ' lstr_tutan_sayilar INTO DATA(mesaj).
        MESSAGE mesaj TYPE 'I'.


      WHEN '&ALL_DRAW'.
        SELECT *
           FROM zbook_piyangotab
           INTO TABLE @DATA(lt_table).
        cl_salv_table=>factory(
        IMPORTING
              r_salv_table   =  lo_alv
        CHANGING
              t_table        = lt_table ).

        lo_alv->display( ).

    ENDCASE.
  ENDMETHOD. "on_user_command


ENDCLASS.


**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************
**************************************************************************************************************************************


CLASS lcl_alv DEFINITION.

  PUBLIC SECTION.
    METHODS: "get_selected_rows,
      display_salv_piyango,
      display_salv_bilet.

    DATA: event_handler TYPE REF TO lcl_handle_events,
          gr_events     TYPE REF TO cl_salv_events_table.

ENDCLASS. "lcl_alv DEFINITION



CLASS lcl_alv IMPLEMENTATION.

  METHOD display_salv_piyango.

    cl_salv_table=>factory(
    IMPORTING
          r_salv_table   =  lo_alv
    CHANGING
          t_table        = gt_show_draw ) ."lt_table ).




    lo_alv->set_screen_status(
      EXPORTING
       pfstatus = 'STATUS_0300'
       report = sy-repid
       set_functions = lo_alv->c_functions_all ).

    gr_events = lo_alv->get_event( ).
    CREATE OBJECT event_handler.
    SET HANDLER event_handler->on_user_command FOR gr_events.

    lo_alv->display( ).
  ENDMETHOD.

  METHOD display_salv_bilet.

    cl_salv_table=>factory(
    IMPORTING
          r_salv_table   =  lo_alv   " Basis Class Simple ALV Tables
    CHANGING
          t_table        = gt_bilet ).



************** satırların seçilebilir hale gelmesini bu sağlar*******************************************
    lo_alv->get_columns( )->set_optimize( abap_true ).
    lo_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>cell ). " if_salv_c_selection_mode=>multiple
************** multiple yapınca çalışmıyor **************************************************************


    lo_alv->set_screen_status(
      EXPORTING
       pfstatus = 'STATUS_0100'
       report = sy-repid
       set_functions = lo_alv->c_functions_all ).

    gr_events = lo_alv->get_event( ).
    CREATE OBJECT event_handler.
    SET HANDLER event_handler->on_user_command FOR gr_events.

    lo_alv->set_screen_popup(
      EXPORTING
        start_column = 10
        end_column   = 75
        start_line   = 5
        end_line     = 25 ).

    lo_alv->display( ).
  ENDMETHOD.


ENDCLASS. "lcl_alv IMPLEMENTATION


FORM dynp_0200_handle_selection.
*  lo_alv->get_metadata( ).
  gt_row = lo_alv->get_selections( )->get_selected_rows( ).

ENDFORM.

FORM zupdate.
  UPDATE zbook_bilet_tab FROM ls_bilet_modify.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  CLEAR ls_bilet_modify.
ENDFORM.
