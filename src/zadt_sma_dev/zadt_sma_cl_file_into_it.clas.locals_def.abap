*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES:
  BEGIN OF ts_filedata,
    purord    TYPE ebeln,
    ses_name  TYPE c LENGTH 40,
    obj_typ   TYPE c LENGTH 2,
    post_date TYPE c LENGTH 25,
    cnfm_qty  TYPE c LENGTH 4,
    po_item   TYPE c LENGTH 5,
    Acnt_cat  TYPE c LENGTH 1,
    prfm_date TYPE c LENGTH 25,
  END OF ts_filedata.

TYPES: tt_filedata TYPE STANDARD TABLE OF ts_filedata.

TYPES:
  BEGIN OF ts_ServiceEntrySheet,
    ServiceEntrySheet        TYPE mmpur_ses_serviceentrysheet,
    ApprovalDateTime         TYPE tzntstmpl,
    ApprovalStatus           TYPE c LENGTH 2,
    CreatedByUser            TYPE syuname,
    CreationDateTime         TYPE timestampl,
    PurchasingOrganization   TYPE c LENGTH 4,
    PurchasingGroup          TYPE c LENGTH 3,
    IsEndOfPurposeBlocked    TYPE c LENGTH 1,
    Currency                 TYPE waers,
    IsDeleted                TYPE c LENGTH 1,
    LastChangeDateTime       TYPE timestampl,
    LastChangedByUser        TYPE syuname,
    MaterialDocument         TYPE mblnr,
    MaterialDocumentYear     TYPE mjahr,
    OriginObjectType         TYPE c LENGTH 2,
    PurchaseOrder            TYPE c LENGTH 10,
    ResponsiblePerson        TYPE n LENGTH 8,
    ServiceEntrySheetName    TYPE n LENGTH 40,
    ServiceEntrySheetUUID    TYPE n LENGTH 16,
    Supplier                 TYPE c LENGTH 10,
    PurgDocExternalSystem    TYPE c LENGTH 60,
    PurgDocExternalReference TYPE c LENGTH 70,
    ExternalRevisionDateTime TYPE tzntstmpl,
    PostingDate              TYPE mmpur_ses_posting_date,
  END OF ts_ServiceEntrySheet,
  tt_ServiceEntrySheet TYPE STANDARD TABLE OF ts_ServiceEntrySheet.
