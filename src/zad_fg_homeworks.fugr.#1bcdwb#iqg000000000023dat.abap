*----------------------------------------------------------------
*    include /1BCDWB/IQG000000000023DAT
*----------------------------------------------------------------

include <symbol>.
include <icon>.

constants:
  begin of %iqid,"type aqliqid
    workspace type aql_wsid   value 'G',
    usergroup type aql_ugroup value 'ZTEST',
    query     type aql_query  value 'ZTESTAA',
    lid       type aql_lid    value 'G00',
    struct    type aql_tname  value '/1BCDWB/IQG000000000023',
    infoset   type aql_iset   value 'ZTESTBK',
  end of %iqid.

data %runmode type aqlimode.

data %seloptions type table of rsparams with header line.

field-symbols <%selopt> type rsparams_tt.

tables VBAK.
tables VBAP.
tables LIPS.
tables LIKP.
tables VBRP.
tables DD07D.

data: begin of %joinwa,
        VBAK like VBAK,
        VBAP like VBAP,
        LIPS like LIPS,
        LIKP like LIKP,
        VBRP like VBRP,
      end of %joinwa.

data TEXT_LIPS_MATNR like MAKT-MAKTX.
data TEXT_LIPS_VBELN like LIKP-INCO2_L.
data TEXT_LIPS_WBSTA like DD07D-DDTEXT.
data TEXT_VBAP_MATNR like MAKT-MAKTX.
data TEXT_VBAP_VBELN like VBAK-KTEXT.
data TEXT_VBRP_MATNR like MAKT-MAKTX.
data TEXT_VBRP_VBELN like VBRK-INCO2_L.
