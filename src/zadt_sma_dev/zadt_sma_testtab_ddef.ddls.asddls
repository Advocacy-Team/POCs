@AbapCatalog.sqlViewName: 'ZADT_SMA_TESTTBL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'test table data definition'
define view ZADT_SMA_TESTTAB_DDEF as select from zadt_sma_testtab {
  key cuser as Cuser,
  ccode as Ccode
}
