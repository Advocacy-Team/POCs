@AbapCatalog.sqlViewName: 'ZADTIINSPLOTM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View For InspectionLot'

define view ZADT_I_InspectionLot_M
  as select from zqals
{
  key plant    as Plant,
  key material as Material,
  key batch    as Batch,
  key insplot  as Insplot
}
