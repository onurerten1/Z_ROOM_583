@AbapCatalog.sqlViewName: 'Z_M_ROOM_583'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Room 583'
@Search.searchable
@UI.headerInfo: { typeName: 'Room', typeNamePlural: 'Rooms' , title : { value: 'ID' } }
define root view Z_I_ROOM_583
  as select from zroom_583 as room
  //  association [0..1] to I_BusinessUserBasic as _SAPSysAdminDataChangeUser on _SAPSysAdminDataChangeUser.UserID = room.lastchangedbyuser
{
      @UI.facet: [ { type: #COLLECTION, position: 1, id: 'ROOM', label: 'Room'  }, { type: #IDENTIFICATION_REFERENCE, position: 1, parentId: 'ROOM', label: 'General Information'}]
      @EndUserText: { label: 'ID' }
      @Search: { defaultSearchElement: true }
      @UI: { lineItem: [{ position: 1 }], identification: [{ position: 1 }] }
  key room.id,
      @EndUserText: { label: 'Seats' }
      @UI: { lineItem: [{ position: 3 }], identification: [{ position: 2 }] }
      room.seats,
      @EndUserText: { label: 'Location' }
      @UI: { lineItem: [{ position: 4 }], identification: [{ position: 3 }] }
      room.location,
      @EndUserText: { label: 'Has Beamer' }
      @UI: { lineItem: [{ position: 5 }], identification: [{ position: 4 }] }
      room.hasbeamer,
      @EndUserText: { label: 'Has Video' }
      @UI: { lineItem: [{ position: 7 }], identification: [{ position: 5 }] }
      room.hasvideo,
      @EndUserText: { label: 'User Rating' }
      @UI: { lineItem: [{ position: 8 }], identification: [{ position: 6 }] }
      userrating,
      @EndUserText: { label: 'Last Changed On' }
      @UI: { identification: [{ position: 7 }] }
      room.lastchangeddatetime,
      @EndUserText: { label: 'Last Changed By' }
      @UI: { identification: [{ position: 8 }], textArrangement: #TEXT_ONLY }
      room.lastchangedbyuser
      //@UI.hidden: true
      //_SAPSysAdminDataChangeUser
}
