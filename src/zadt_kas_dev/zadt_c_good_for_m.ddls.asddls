@EndUserText.label: 'Consumption view for Good for'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZADT_C_Good_For_M as projection on ZADT_I_Good_For_M
 {
@EndUserText.label: 'Plant'
@Search.defaultSearchElement: true
    key Plant,
@EndUserText.label: 'Target_PQ'
@Search.defaultSearchElement: true
    key Target_Pq,
@EndUserText.label: 'Good_For_PQ'
@Search.defaultSearchElement: true
    key Good_For_Pq,
@EndUserText.label: 'Priority'
    Priority

}
