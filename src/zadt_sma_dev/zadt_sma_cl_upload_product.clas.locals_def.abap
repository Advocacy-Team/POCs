*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES:
  BEGIN OF ts_filedata,
    ProductType        TYPE c LENGTH 4,
    BaseUnit           TYPE c LENGTH 3,
    IndustrySector     TYPE c LENGTH 2,
    Language           TYPE c LENGTH 3,
    ProductDescription TYPE c LENGTH 40,
    Plant              TYPE c LENGTH 4,
    MRPType            TYPE c LENGTH 40,

  END OF ts_filedata.

TYPES: tt_filedata TYPE STANDARD TABLE OF ts_filedata.

TYPES:
   BEGIN OF ts_attachment,

        Product TYPE c LENGTH 40,
        ProductType TYPE c LENGTH 40,
        CrossPlantStatus TYPE c LENGTH 40,
        CrossPlantStatusValidityDate TYPE c LENGTH 40,
        CreationDate TYPE c LENGTH 40,
        CreatedByUser TYPE c LENGTH 40,
        LastChangeDate TYPE c LENGTH 40,
        LastChangedByUser TYPE c LENGTH 40,
        LastChangeDateTime TYPE c LENGTH 40,
        IsMarkedForDeletion TYPE c LENGTH 40,
        ProductOldID TYPE c LENGTH 40,
        GrossWeight TYPE c LENGTH 40,
        PurchaseOrderQuantityUnit TYPE c LENGTH 40,
        SourceOfSupply TYPE c LENGTH 40,
        WeightUnit TYPE c LENGTH 40,
        NetWeight TYPE c LENGTH 40,
        CountryOfOrigin TYPE c LENGTH 40,
        CompetitorID TYPE c LENGTH 40,
        ProductGroup TYPE c LENGTH 40,
        BaseUnit TYPE c LENGTH 40,
        ItemCategoryGroup TYPE c LENGTH 40,
        ProductHierarchy TYPE c LENGTH 40,
        Division TYPE c LENGTH 40,
        VarblPurOrdUnitIsActive TYPE c LENGTH 40,
        VolumeUnit TYPE c LENGTH 40,
        MaterialVolume TYPE c LENGTH 40,
        ANPCode TYPE c LENGTH 40,
        Brand TYPE c LENGTH 40,
        ProcurementRule TYPE c LENGTH 40,
        ValidityStartDate TYPE c LENGTH 40,
        LowLevelCode TYPE c LENGTH 40,
        ProdNoInGenProdInPrepackProd TYPE c LENGTH 40,
        SerialIdentifierAssgmtProfile TYPE c LENGTH 40,
        SizeOrDimensionText TYPE c LENGTH 40,
        IndustryStandardName TYPE c LENGTH 40,
        ProductStandardID TYPE c LENGTH 40,
        InternationalArticleNumberCat TYPE c LENGTH 40,
        ProductIsConfigurable TYPE c LENGTH 40,
        IsBatchManagementRequired TYPE c LENGTH 40,
        ExternalProductGroup TYPE c LENGTH 40,
        CrossPlantConfigurableProduct TYPE c LENGTH 40,
        SerialNoExplicitnessLevel TYPE c LENGTH 40,
        ProductManufacturerNumber TYPE c LENGTH 40,
        ManufacturerNumber TYPE c LENGTH 40,
        ManufacturerPartProfile TYPE c LENGTH 40,
        QltyMgmtInProcmtIsActive TYPE c LENGTH 40,
        IndustrySector TYPE c LENGTH 40,
        ChangeNumber TYPE c LENGTH 40,
        MaterialRevisionLevel TYPE c LENGTH 40,
        HandlingIndicator TYPE c LENGTH 40,
        WarehouseProductGroup TYPE c LENGTH 40,
        WarehouseStorageCondition TYPE c LENGTH 40,
        StandardHandlingUnitType TYPE c LENGTH 40,
        SerialNumberProfile TYPE c LENGTH 40,
        AdjustmentProfile TYPE c LENGTH 40,
        PreferredUnitOfMeasure TYPE c LENGTH 40,
        IsPilferable TYPE c LENGTH 40,
        IsRelevantForHzdsSubstances TYPE c LENGTH 40,
        QuarantinePeriod TYPE c LENGTH 40,
        TimeUnitForQuarantinePeriod TYPE c LENGTH 40,
        QualityInspectionGroup TYPE c LENGTH 40,
        AuthorizationGroup TYPE c LENGTH 40,
        HandlingUnitType TYPE c LENGTH 40,
        HasVariableTareWeight TYPE c LENGTH 40,
        MaximumPackagingLength TYPE c LENGTH 40,
        MaximumPackagingWidth TYPE c LENGTH 40,
        MaximumPackagingHeight TYPE c LENGTH 40,
        UnitForMaxPackagingDimensions TYPE c LENGTH 40,

  END OF ts_attachment.

  TYPES: tt_attachment TYPE STANDARD TABLE OF ts_attachment.

 types:
        begin of ts_user_ccode,

        UserID type c length 12,
        CompanyCode type c length 40,

        end of ts_user_ccode.

     TYPES: tt_user_ccode TYPE STANDARD TABLE OF ts_user_ccode.
