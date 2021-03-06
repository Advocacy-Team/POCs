@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Root View for Inspection Lot'
define root view entity ZADT_I_INSPECTIONLOT as select from I_InspectionLot {
  
key InspectionLot,
Material,
Batch,
Plant,
InspectionLotOrigin,
OrderInternalBillOfOperations,
ManufacturingOrder,
InspectionLotText,
InspectionLotType,
InspectionLotQuantity,
InspectionLotActualQuantity,
InspectionLotDefectiveQuantity,
InspectionLotQuantityUnit,
InspLotCreatedOnLocalDate,
InspSubsetFieldCombination,
InspLotNmbrOpenLongTermCharc,
StatusObject,
StatusObjectCategory,
InspectionLotObjectText,
Equipment,
FunctionalLocation,
StatusProfile,
MatlQualityAuthorizationGroup,
InspectionLotHasQuantity,
InspLotIsCreatedAutomatically,
InspectionLotHasPartialLots,
InspectionLotHasAppraisalCosts,
InspLotHasSubsets,
InspLotIsAutomUsgeDcsnPossible,
PurchasingDocumentCategory,
InspLotHasConfignSpecification,
GoodsReceiptIsMovedToBlkdStock,
InspLotIsDocumentationRequired,
InspLotIsTaskListRequired,
InspLotHasManualSampleSize,
InspLotHasMaterialSpec,
InspectionLotApproval,
InspLotDigitalSgntrResultsRecg,
InspLotDigitalSgntrInUsgeDcsn,
InspLotIsBatchRequired,
InspLotUsageInStabilityStudy,
InspLotIsStockPostingCompleted,
InspectionLotHasUsageDecision,
NumberOfSerialNumbers,
InspLotIsSerialNmbrPossible,
InspectionLotIsSkipped,
InspectionLotIsFullInspection,
InspectionLotDynamicLevel,
SamplingProcedure,
InspLotCreatedOnLocalTime,
InspectionLotCreatedBy,
InspectionLotCreatedOn,
InspectionLotCreatedOnTime,
InspectionLotChangedBy,
InspectionLotChangeDate,
InspectionLotChangeTime,
ChangedDateTime,
InspectionLotStartDate,
InspectionLotStartTime,
InspectionLotEndDate,
InspectionLotEndTime,
BillOfOperationsType,
BillOfOperationsGroup,
BillOfOperationsUsage,
BillOfOperationsVariant,
BillOfOperationsChangeStateID,
InspectionSubsetType,
SmplDrawingProcedure,
SmplDrawingProcedureVersion,
SmplDrwgProcedIsConfRequired,
InspLotSelectionMaterial,
InspLotSelMatlRevisionLvl,
InspLotSelectionPlant,
InspLotSelectionSupplier,
InspLotSelectionManufacturer,
InspLotSelectionCustomer,
InspLotSelBillOfOperationsUsge,
InspLotSelectionValidFromDate,
ProductionVersion,
SalesOperationsPlanningOrder,
IsBusinessPurposeCompleted,
Customer,
Supplier,
BatchBySupplier,
Manufacturer,
ManufacturerPartNmbr,
MaterialRevisionLevel,
MaterialIsBatchManaged,
BatchStorageLocation,
MaterialCompIsSpecialStock,
PurchasingOrganization,
PurchasingDocument,
PurchasingDocumentItem,
ScheduleLine,
AccountingDocumentType,
MaterialDocumentYear,
MaterialDocument,
MaterialDocumentItem,
MatlDocLatestPostgDate,
GoodsMovementType,
InspectionLotPlant,
InspectionLotStorageLocation,
Warehouse,
StorageType,
StorageBin,
SalesOrder,
SalesOrderItem,
DeliveryDocument,
DeliveryDocumentItem,
DeliveryCategory,
Route,
BillToPartyCountry,
SoldToParty,
SalesOrganization,
MaterialByCustomer,
Language,
InspLotNmbrAddlRecordedCharc,
InspLotNmbrOpenShortTermCharc,
InspectionLotContainer,
InspectionLotContainerUnit,
InspectionLotSampleQuantity,
InspectionLotSampleUnit,
InspLotDynamicRule,
InspLotDynamicTrggrPoint,
InspectionDynamicStage,
InspectionSeverity,
InspLotQtyToFree,
InspLotQtyToScrap,
InspLotQtyToSample,
InspLotQtyToBlocked,
InspLotQtyToReserves,
InspLotQtyToAnotherMaterial,
InspLotMaterialPostedTo,
InspLotBatchTransferredTo,
InspLotQtyReturnedToSupplier,
InspLotQtyToSpecialStock,
InspLotQtyToOtherStock,
InspLotQtyToBePosted,
InspLotSmplQtyForLongTermChar,
InspLotQtyInspected,
InspLotQtyDestroyed,
InspectionLotScrapRatio,
InspLotUsageDecisionTool,
InspectionLotAllowedScrapRatio,
QualityCostCollector,
ConsumptionPosting,
AccountAssignmentCategory,
PurchasingDocumentItemCategory,
InspLotAcctAssgmtKey,
CostCenter,
ReservationItem,
MasterFixedAsset,
FixedAsset,
SalesOrdStockWBSElement,
ProjectNetwork,
NetworkActivityInternalID,
InventorySpclStkSalesDocument,
InventorySpclStkSalesDocItm,
ProfitabilitySegment,
ProfitabilitySegment_2,
ProfitCenter,
BusinessArea,
GLAccount,
ControllingArea,
CompanyCode,
SerialNumberProfile,
InspLotCostCollectorSalesOrder,
InspLotCostCollectorSlsOrdItem,
InspLotCostCollectorWBSElement,
InspLotExternalNumber,
InspectionLotPriorityPoints,
MaintenancePlan,
MaintenancePlanItemIntID,
MaintenanceStrategy,
InspUsgeDcsnDgtlSgntrType,
InspUsgeDcsnDgtlSgntrStrgy,
/* Associations */
_BillOfOperationsChangeState,
_BillOfOperationsGroup,
_BillOfOperationsType,
_Customer,
_Equipment,
_FunctionalLocation,
_InspectionLotAllLongText,
_InspectionLotApproval,
_InspectionLotApprovalText,
_InspectionLotChangedBy,
_InspectionLotContainerUnit,
_InspectionLotCreatedBy,
_InspectionLotHasQuantity,
_InspectionLotHasQuantityT,
_InspectionLotHasUsgeDcsn,
_InspectionLotHasUsgeDcsnTxt,
_InspectionLotLongText,
_InspectionLotOrigin,
_InspectionLotQuantityUnit,
_InspectionLotSampleUnit,
_InspectionLotType,
_InspectionOperation,
_InspectionSubsetType,
_InspectionSubsetTypeText,
_InspLotIsBatchRequired,
_InspLotIsBatchRequiredTxt,
_InspLotMatlDocItem,
_InspLotSelectionCustomer,
_InspLotSelectionMaterial,
_InspLotSelectionSupplier,
_InspLotSelManufacturer,
_InspLotSgntrInUsgeDcsn,
_InspLotSgntrInUsgeDcsnText,
_InspLotUsageDecision,
_InspSubsetFieldCombination,
_IsAutomUsgeDcsnPossible,
_IsAutomUsgeDcsnPossibleTxt,
_IsDocumentationRqd,
_IsDocumentationRqdTxt,
_IsStockPostgCmpltd,
_IsStockPostgCmpltdTxt,
_MaintenancePlan,
_Manufacturer,
_ManufacturerPartNmbr,
_Material,
_MaterialSampleDrawing,
_MatlQualityAuthGroup,
_Plant,
_PurchasingOrganization,
_QltyMgmtPlantLvlConfign,
_SalesOrdStockWBSElement,
_SalesOrganization,
_SmplDrwgProcedIsConfRqd,
_SmplDrwgProcedIsConfRqdTxt,
_SoldToParty,
_Supplier
}
