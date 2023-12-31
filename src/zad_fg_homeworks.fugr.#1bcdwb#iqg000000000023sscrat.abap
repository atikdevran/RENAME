*----------------------------------------------------------------
*    include /1BCDWB/IQG000000000023SSCRAT
*----------------------------------------------------------------


AT SELECTION-SCREEN.
  tables sscrfields.
  call function 'RSAQRT_LAYOUT_CHECK'
       exporting variant = %layout
       changing  rtmode  = %runmode.
  call function 'RSAQRT_SSCR_TEST'
       exporting  sscruc = sscrfields-ucomm
       tables     selopt = <%selopt>
       changing   rtmode = %runmode
       exceptions others = 1.
  if sy-subrc <> 0.
    %runmode-extr_on = space.
    %runmode-show_on = space.
  endif.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR %LAYOUT.
  call function 'RSAQRT_LAYOUT_VALUE_REQUEST'
       changing rtmode  = %runmode
                variant = %layout.

AT SELECTION-SCREEN OUTPUT.
  call function 'RSAQRT_SSCR_OUTPUT'
       changing rtmode = %runmode.

INITIALIZATION.
  assign %seloptions[] to <%selopt>.
  call function 'RSAQRT_INITIALIZATION'
       exporting iqid   = %iqid
       importing rtmode = %runmode.
  perform %initialization.

*---------------------------------------------------------------*
*       FORM %initialization                                    *
*---------------------------------------------------------------*

form %initialization.


endform.
