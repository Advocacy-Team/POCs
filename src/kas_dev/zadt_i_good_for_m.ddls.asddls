@AbapCatalog.sqlViewName: 'ZADTIGOODFORM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View For Good For'

define root view ZADT_I_Good_For_M
  as select from zadt_yggood_for
{
  key plant       as Plant,
  key target_pq   as Target_Pq,
  key good_for_pq as Good_For_Pq,
      priority    as Priority
}
