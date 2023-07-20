*----------------------------------------------------------------
*    include /1BCDWB/IQG000000000023SSCR
*----------------------------------------------------------------

selection-screen begin of block qsel
                          with frame title text-s02.
select-options SP$00008 for VBAP-VBELN memory id AUN
                         no intervals.
select-options SP$00007 for VBAK-ERDAT
                         no-extension.
select-options SP$00003 for LIPS-VBELN memory id VL
                         no intervals.
select-options SP$00002 for LIPS-WBSTA
                         no-extension.
select-options SP$00005 for VBRP-VBELN memory id VF
                         no intervals.
select-options SP$00004 for VBRP-ERDAT
                         no-extension.
selection-screen end of block qsel.
selection-screen begin of block stdsel with frame title text-s03.
parameters %layout type slis_vari modif id lay.
selection-screen end of block stdsel.
selection-screen begin of block aging with frame title text-s04.
parameters %aging type data_temperature.
selection-screen end of block aging.
