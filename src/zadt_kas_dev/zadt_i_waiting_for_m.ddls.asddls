@AbapCatalog.sqlViewName: 'ZADTIWAITINGFORM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View For Waiting to Process'

define root view ZADT_I_Waiting_For_M as select from zadt_waiting_for 
{
  key plant as Plant,   
  key material as Material,
  key batch as Batch,   
  key insplot  as Insplot
}
