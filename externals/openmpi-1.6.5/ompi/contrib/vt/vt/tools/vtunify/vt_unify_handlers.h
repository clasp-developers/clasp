/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_UNIFY_HANDLERS_H_
#define _VT_UNIFY_HANDLERS_H_

#include "vt_unify.h"
#include "vt_unify_defs_recs.h"
#include "vt_unify_lvector.hh"
#include "vt_unify_markers.h"

#include "otf.h"

//
// first handler argument structures for reading ...
//

// ... definitions
struct FirstHandlerArg_DefsS
{
   FirstHandlerArg_DefsS( LargeVectorC<DefRec_BaseS*> & _loc_defs )
      : loc_defs( _loc_defs ) {}

   LargeVectorC<DefRec_BaseS*> & loc_defs;

};

// ... marker
struct FirstHandlerArg_MarkersS
{
  FirstHandlerArg_MarkersS( LargeVectorC<DefRec_DefMarkerS*> & _loc_defs,
     LargeVectorC<MarkersC::MarkerSpotS*> & _loc_spots )
     : loc_defs( _loc_defs ), loc_spots( _loc_spots ) {}

   LargeVectorC<DefRec_DefMarkerS*> & loc_defs;
   LargeVectorC<MarkersC::MarkerSpotS*> & loc_spots;

};

// ... events
struct FirstHandlerArg_EventsS
{
   FirstHandlerArg_EventsS( OTF_WStream *& _wstream )
      : wstream( _wstream ) {}

   OTF_WStream * wstream;

};

// ... statistics
typedef FirstHandlerArg_EventsS FirstHandlerArg_StatsS;

// key-value list "record handler"
void HandleKeyValueList( const uint32_t & proc, OTF_KeyValueList * kvs );

// definition record handlers
//

int HandleDefComment( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, const char * comment );

int HandleDefCreator( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, const char * creator );

int HandleDefTimerResolution( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint64_t ticksPerSecond );

int HandleDefTimeRange( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint64_t minTime, uint64_t maxTime );

int HandleDefProcess( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, const char * name,
       uint32_t parent );

int HandleDefProcessGroup( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, const char * name,
       uint32_t n, uint32_t * array );

int HandleDefProcessGroupAttributes( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t group, uint32_t attributes );

int HandleDefSclFile( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, const char * filename );

int HandleDefScl( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, uint32_t sclfile,
       uint32_t sclline );

int HandleDefFileGroup( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, const char * name );

int HandleDefFile( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, const char * name,
       uint32_t group );

int HandleDefFunctionGroup( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, const char * name );

int HandleDefFunction( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, const char * name, uint32_t group,
       uint32_t scltoken );

int HandleDefCollOp( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t collOp, const char * name, uint32_t type );

int HandleDefCounterGroup( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, const char * name );

int HandleDefCounter( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t deftoken, const char * name,
       uint32_t properties, uint32_t countergroup, const char * unit );

int HandleDefCounterAssignments( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t counter, uint32_t n, uint32_t * array );

int HandleDefKeyValue( FirstHandlerArg_DefsS * fha,
       uint32_t streamid, uint32_t key, OTF_Type type, const char * name,
       const char * description );

// marker record handlers
//

int HandleDefMarker( FirstHandlerArg_MarkersS * fha,
       uint32_t streamid, uint32_t deftoken, const char * name, uint32_t type );

int HandleMarkerSpot( FirstHandlerArg_MarkersS * fha,
       uint64_t time, uint32_t proc, uint32_t marker, const char * text );

// event record handlers
//

int HandleEventComment( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, const char * comment,
       OTF_KeyValueList * kvs );

int HandleEnter( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t func, uint32_t proc, uint32_t scl,
       OTF_KeyValueList * kvs );

int HandleLeave( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t func, uint32_t proc, uint32_t scl,
       OTF_KeyValueList * kvs );

int HandleCounter( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint32_t counter, uint64_t value,
       OTF_KeyValueList * kvs );

int HandleBeginFileOp( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint64_t matchid, uint32_t scl,
       OTF_KeyValueList * kvs );

int HandleEndFileOp( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint32_t file, uint64_t matchid,
       uint64_t handleid, uint32_t operation, uint64_t bytes, uint32_t scl,
       OTF_KeyValueList * kvs );

int HandleSendMsg( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t sender, uint32_t receiver, uint32_t comm,
       uint32_t tag, uint32_t length, uint32_t scl, OTF_KeyValueList * kvs );

int HandleRecvMsg( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t receiver, uint32_t sender, uint32_t comm,
       uint32_t tag, uint32_t length, uint32_t scl, OTF_KeyValueList * kvs );

int HandleBeginCollOp( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint32_t operation, uint64_t matchid,
       uint32_t comm, uint32_t root, uint64_t sent, uint64_t recvd,
       uint32_t scl, OTF_KeyValueList * kvs );

int HandleEndCollOp( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint64_t matchid, OTF_KeyValueList * kvs );

int HandleRMAPut( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint32_t origin, uint32_t dest,
       uint32_t comm, uint32_t tag, uint64_t bytes, uint32_t scl,
       OTF_KeyValueList * kvs );

int HandleRMAPutRemoteEnd( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint32_t origin, uint32_t dest,
       uint32_t comm, uint32_t tag, uint64_t bytes, uint32_t scl,
       OTF_KeyValueList * kvs );

int HandleRMAGet( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint32_t origin, uint32_t dest,
       uint32_t comm, uint32_t tag, uint64_t bytes, uint32_t scl,
       OTF_KeyValueList * kvs );

int HandleRMAEnd( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint32_t remote, uint32_t comm,
       uint32_t tag, uint32_t scl, OTF_KeyValueList * kvs );

// summary record handlers
//

int HandleFunctionSummary( FirstHandlerArg_StatsS * fha,
       uint64_t time, uint32_t func, uint32_t proc, uint64_t invocations,
       uint64_t exclTime, uint64_t inclTime );

int HandleMessageSummary( FirstHandlerArg_StatsS * fha,
       uint64_t time, uint32_t proc, uint32_t peer, uint32_t comm,
       uint32_t type, uint64_t sentNum, uint64_t recvNum,
       uint64_t sentBytes, uint64_t recvBytes );

int HandleCollOpSummary( FirstHandlerArg_StatsS * fha,
       uint64_t time, uint32_t proc, uint32_t comm, uint32_t collop,
       uint64_t sentNum, uint64_t recvNum, uint64_t sentBytes,
       uint64_t recvBytes );

int HandleFileOpSummary( FirstHandlerArg_StatsS * fha,
       uint64_t time, uint32_t file, uint32_t proc, uint64_t nopen,
       uint64_t nclose, uint64_t nread, uint64_t nwrite, uint64_t nseek,
       uint64_t bytesRead, uint64_t bytesWrite );

#endif // _VT_UNIFY_HANDLERS_H_
