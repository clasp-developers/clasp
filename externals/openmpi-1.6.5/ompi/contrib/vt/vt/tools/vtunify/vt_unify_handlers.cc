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

#include "vt_unify_defs.h"
#include "vt_unify_handlers.h"
#include "vt_unify_hooks.h"
#include "vt_unify_markers.h"
#include "vt_unify_sync.h"
#include "vt_unify_tkfac.h"
#include "vt_unify_usrcom.h"

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

// check whether given stream id is available
static bool
isStreamAvail( const uint32_t streamid )
{
   return
      ( AbsentStreamIds.empty() ||
        AbsentStreamIds.find( streamid ) == AbsentStreamIds.end() );
}

#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

// translate local key tokens to global tokens
static void
handleKeyValueList( const uint32_t & proc, OTF_KeyValueList * kvs )
{
   // get number of key-value pairs
   uint32_t keys_num = OTF_KeyValueList_getCount( kvs );

   if( keys_num > 0 )
   {
      // get global token factory for DefKeyValue
      static const TokenFactoryScopeI * tkfac_defkeyval =
         theTokenFactory->getScope( DEF_REC_TYPE__DefKeyValue );

      for( uint32_t i = 0; i < keys_num; i++ )
      {
         // get pointer to key-value pair by index
         //
         OTF_KeyValuePair* pair = 0;
         OTF_KeyValueList_getPairByIndex( kvs, i, &pair );
         vt_assert( pair );

         // translate local key token
         //
         uint32_t global_key = tkfac_defkeyval->translate( proc, pair->key );
         vt_assert( global_key != 0 );
         pair->key = global_key;
      }
   }
}

// key-value list "record handler"
void
HandleKeyValueList( const uint32_t & proc, OTF_KeyValueList * kvs )
{
   // call the local version of this function which should hopefully
   // be inlined
   handleKeyValueList( proc, kvs );
}

// definition record handlers
//

int
HandleDefComment( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, const char * comment )
{
   // get common string identifiers as std::string's for more convenient use
   //
   static const std::string vt_comment_prefix =
      VT_UNIFY_STRID_VT_COMMENT;
   static const std::string starttime_prefix =
      VT_UNIFY_STRID_STARTTIME_COMMENT;
   static const std::string stoptime_prefix =
      VT_UNIFY_STRID_STOPTIME_COMMENT;
   static const std::string usrcom_send_prefix =
      VT_UNIFY_STRID_USRCOM_SEND_COMMENT;
   static const std::string usrcom_recv_prefix =
      VT_UNIFY_STRID_USRCOM_RECV_COMMENT;

   std::string _comment(comment);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefComment, 2,
      &streamid, &_comment );

   // determine comment type
   //

   DefRec_DefCommentS::CommentTypeT type;

   // start trace time comment?
   //
   if( _comment.length() > starttime_prefix.length() &&
       _comment.compare( 0, starttime_prefix.length(), starttime_prefix ) == 0 )
   {
      type = DefRec_DefCommentS::TYPE_START_TIME;
      // cut identifier prefix from comment
      _comment = _comment.substr( starttime_prefix.length() );
   }
   // stop trace time comment?
   //
   else if( _comment.length() > stoptime_prefix.length() &&
            _comment.compare( 0, stoptime_prefix.length(),
                              stoptime_prefix ) == 0 )
   {
      type = DefRec_DefCommentS::TYPE_STOP_TIME;
      // cut identifier prefix from comment
      _comment = _comment.substr( stoptime_prefix.length() );
   }
   // user send communication event?
   //
   else if( _comment.length() > usrcom_send_prefix.length() &&
            _comment.compare( 0, usrcom_send_prefix.length(),
                              usrcom_send_prefix ) == 0 )
   {
      type = DefRec_DefCommentS::TYPE_USRCOM_SEND;
      // cut identifier prefix from comment
      _comment = _comment.substr( usrcom_send_prefix.length() );
   }
   // user recv. communication event?
   //
   else if( _comment.length() > usrcom_recv_prefix.length() &&
            _comment.compare( 0, usrcom_recv_prefix.length(),
                              usrcom_recv_prefix ) == 0 )
   {
      type = DefRec_DefCommentS::TYPE_USRCOM_RECV;
      // cut identifier prefix from comment
      _comment = _comment.substr( usrcom_recv_prefix.length() );
   }
   // VT comment?
   //
   else if( _comment.length() > vt_comment_prefix.length() &&
            _comment.compare( 0, vt_comment_prefix.length(),
                              vt_comment_prefix ) == 0 )
   {
      type = DefRec_DefCommentS::TYPE_VT;
      // cut identifier prefix from comment
      _comment = _comment.substr( vt_comment_prefix.length() );
   }
   // otherwise, it's an user comment
   //
   else
   {
      type = DefRec_DefCommentS::TYPE_USER;
   }

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefCommentS
                               ( streamid, 0, type, _comment ) );

   return OTF_RETURN_OK;
}

int
HandleDefCreator( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, const char * creator )
{
   std::string _creator(creator);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefCreator, 2,
      &streamid, &_creator );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefCreatorS( _creator ) );

   return OTF_RETURN_OK;
}

int
HandleDefTimerResolution( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint64_t ticksPerSecond )
{
   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefTimerResolution, 2,
      &streamid, &ticksPerSecond );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefTimerResolutionS( ticksPerSecond ) );

   return OTF_RETURN_OK;
}

int
HandleDefTimeRange( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint64_t minTime, uint64_t maxTime )
{
   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefTimeRange, 3,
      &streamid, &minTime, &maxTime );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefTimeRangeS
                               ( streamid, minTime, maxTime ) );

   return OTF_RETURN_OK;
}

int
HandleDefProcess( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, const char * name, uint32_t parent )
{
   std::string _name(name);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefProcess, 4,
      &streamid, &deftoken, &_name, &parent );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefProcessS( deftoken, _name, parent ) );

   return OTF_RETURN_OK;
}

int
HandleDefProcessGroup( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, const char * name, uint32_t n,
   uint32_t * array )
{
   // get common string identifiers as std::string's for more convenient use
   //
   static const std::string all_name =
     VT_UNIFY_STRID_ALL_PROCGRP;
   static const std::string node_prefix =
      VT_UNIFY_STRID_NODE_PROCGRP;
   static const std::string mpi_comm_world_name =
      VT_UNIFY_STRID_MPI_COMM_WORLD_PROCGRP;
   static const std::string mpi_comm_self_name =
      VT_UNIFY_STRID_MPI_COMM_SELF_PROCGRP;
   static const std::string mpi_comm_other_name =
      VT_UNIFY_STRID_MPI_COMM_OTHER_PROCGRP;
   static const std::string mpi_group_name =
      VT_UNIFY_STRID_MPI_GROUP_PROCGRP;
   static const std::string user_comm_prefix =
      VT_UNIFY_STRID_USER_COMM_PROCGRP;

   std::string _name(name);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefProcessGroup, 5,
      &streamid, &deftoken, &_name, &n, &array );

   // determine process group type
   //

   DefRec_DefProcessGroupS::ProcessGroupTypeT type;

   if( _name.compare( all_name ) == 0 )
   {
      type = DefRec_DefProcessGroupS::TYPE_ALL;
      _name = "";
   }
   else if( _name.length() > node_prefix.length() &&
            _name.compare( 0, node_prefix.length(), node_prefix ) == 0 )
   {
      type = DefRec_DefProcessGroupS::TYPE_NODE;
      // cut identifier prefix from node name
      _name = _name.substr( node_prefix.length() );
   }
   else if( _name.compare( mpi_comm_world_name ) == 0 )
   {
      type = DefRec_DefProcessGroupS::TYPE_MPI_COMM_WORLD;
      _name = "";
   }
   else if( _name.compare( mpi_comm_self_name ) == 0 )
   {
      type = DefRec_DefProcessGroupS::TYPE_MPI_COMM_SELF;
      _name = "";
   }
   else if( _name.compare( mpi_comm_other_name ) == 0 )
   {
      type = DefRec_DefProcessGroupS::TYPE_MPI_COMM_OTHER;
      _name = "";
   }
   else if( _name.compare( mpi_group_name ) == 0 )
   {
      type = DefRec_DefProcessGroupS::TYPE_MPI_GROUP;
      _name = "";
   }
   else if( _name.length() > user_comm_prefix.length() &&
            _name.compare( 0, user_comm_prefix.length(),
               user_comm_prefix ) == 0 )
   {
      type = DefRec_DefProcessGroupS::TYPE_USER_COMM;
      // cut identifier prefix from user comm. name
      _name = _name.substr( user_comm_prefix.length() );
   }
   else
   {
      type = DefRec_DefProcessGroupS::TYPE_OTHER;
   }

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
   if( n > 0 && !AbsentStreamIds.empty() &&
       type != DefRec_DefProcessGroupS::TYPE_ALL &&
       type != DefRec_DefProcessGroupS::TYPE_NODE &&
       type != DefRec_DefProcessGroupS::TYPE_MPI_COMM_SELF )
   {
      // remove unavailable stream ids from member array
      //
      uint32_t j = 0;
      for( uint32_t i = 0; i < n; i++ )
      {
         if( isStreamAvail( array[i] ) && j < i )
            array[j++] = array[i];
      }
      n = j;
      // drop record, if member array is empty
      if( n == 0 )
         return OTF_RETURN_OK;
   }
#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefProcessGroupS
                               ( streamid, deftoken, type, _name, n,
                                 array ) );

   return OTF_RETURN_OK;
}

int
HandleDefProcessGroupAttributes( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t group, uint32_t attributes )
{
   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefProcessGroupAttributes, 3,
      &streamid, &group, &attributes );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefProcessGroupAttributesS
                               ( streamid, group, attributes ) );

   return OTF_RETURN_OK;
}

int
HandleDefSclFile( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, const char * filename )
{
   std::string _filename(filename);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefSclFile, 3,
      &streamid, &deftoken, &_filename );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefSclFileS
                               ( streamid, deftoken, _filename ) );

   return OTF_RETURN_OK;
}

int
HandleDefScl( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, uint32_t sclfile, uint32_t sclline )
{
   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefScl, 4,
      &streamid, &deftoken, &sclfile, &sclline );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefSclS
                               ( streamid, deftoken, sclfile, sclline ) );

   return OTF_RETURN_OK;
}

int
HandleDefFileGroup( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, const char * name )
{
   std::string _name(name);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefFileGroup, 3,
      &streamid, &deftoken, &_name );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefFileGroupS
                               ( streamid, deftoken, _name ) );

   return OTF_RETURN_OK;
}

int
HandleDefFile( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, const char * name, uint32_t group )
{
   std::string _name(name);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefFile, 4,
      &streamid, &deftoken, &_name, &group );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefFileS
                               ( streamid, deftoken, _name, group ) );

   return OTF_RETURN_OK;
}

int
HandleDefFunctionGroup( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, const char * name )
{
   std::string _name(name);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefFunctionGroup, 3,
      &streamid, &deftoken, &_name );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefFunctionGroupS
                               ( streamid, deftoken, _name ) );

   return OTF_RETURN_OK;
}

int
HandleDefFunction( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, const char * name, uint32_t group,
   uint32_t scltoken )
{
   std::string _name(name);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefFunction, 5,
      &streamid, &deftoken, &_name, &group, &scltoken );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefFunctionS
                               ( streamid, deftoken, _name, group,
                                 scltoken ) );

   return OTF_RETURN_OK;
}

int
HandleDefCollOp( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t collOp, const char * name, uint32_t type )
{
   std::string _name(name);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefCollOp, 4,
      &streamid, &collOp, &_name, &type );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefCollOpS
                               ( streamid, collOp, _name, type ) );

   return OTF_RETURN_OK;
}

int
HandleDefCounterGroup( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, const char * name )
{
   std::string _name(name);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefCounterGroup, 3,
      &streamid, &deftoken, &_name );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefCounterGroupS
                               ( streamid, deftoken, _name ) );

   return OTF_RETURN_OK;
}

int
HandleDefCounter( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t deftoken, const char * name, uint32_t properties,
   uint32_t countergroup, const char * unit )
{
   std::string _name(name);
   std::string _unit(unit);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefCounter, 6,
      &streamid, &deftoken, &_name, &properties, &countergroup, &unit );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefCounterS
                               ( streamid, deftoken, _name, properties,
                                 countergroup, _unit ) );

   return OTF_RETURN_OK;
}

int
HandleDefCounterAssignments( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t counter, uint32_t n, uint32_t * array )
{
   vt_assert( n == 1 ); // only one process group assignment per counter allowed
   vt_assert( array );

   uint32_t procgrp = *array;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefCounterAssignments, 3,
      &streamid, &counter, &procgrp );

   // register local process group assignment
   theDefinitions->groupCounters()->setGroup( streamid, counter, procgrp );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefCounterAssignmentsS
                               ( streamid, counter, procgrp ) );

   return OTF_RETURN_OK;
}

int
HandleDefKeyValue( FirstHandlerArg_DefsS * fha,
   uint32_t streamid, uint32_t key, OTF_Type type, const char * name,
   const char * description )
{
   std::string _name(name);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefKeyValue, 4,
      &streamid, &key, &type, &_name );

   // add local definition to vector
   fha->loc_defs.push_back( new DefRec_DefKeyValueS
                          ( streamid, key, type, _name ) );

   return OTF_RETURN_OK;
}

// marker record handlers
//

int
HandleDefMarker( FirstHandlerArg_MarkersS * fha,
   uint32_t streamid, uint32_t deftoken, const char * name, uint32_t type )
{
   std::string _name( name );

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_DefMarker, 4,
      &streamid, &deftoken, &_name, &type );

   // add local marker definition to vector
   fha->loc_defs.push_back( new DefRec_DefMarkerS
                               ( streamid, deftoken, type, _name ) );

   return OTF_RETURN_OK;
}

int
HandleMarkerSpot( FirstHandlerArg_MarkersS * fha,
   uint64_t time, uint32_t proc, uint32_t marker, const char * text )
{
   std::string _text( text );

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_MarkerSpot, 4,
      &time, &proc, &marker, &_text );

   // add local marker spot to vector
   fha->loc_spots.push_back( new MarkersC::MarkerSpotS
                                ( proc, time, marker, _text ) );

   return OTF_RETURN_OK;
}

// event record handlers
//

int
HandleEventComment( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t proc, const char * comment, OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   std::string _comment(comment);

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_EventComment, 4,
      &time, &proc, &_comment, &kvs );

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

#ifdef VT_ETIMESYNC
   // update time sync. parameters, if necessary
   //
   if( theTimeSync->getSyncMethod() == TimeSyncC::METHOD_ENHANCED &&
       _comment.compare( VT_UNIFY_STRID_ETIMESYNC_COMMENT ) == 0 )
   {
      if( theTimeSync->getSyncMethod() == TimeSyncC::METHOD_ENHANCED )
         theTimeSync->updateSyncParam( proc );

      return ret;
   }
   // otherwise, write record
   //
   else
#endif // VT_ETIMESYNC
   {
      bool do_write = true;

      // correct time
      time = theTimeSync->correctTime( proc, time );

      // trigger write record hook
      theHooks->triggerWriteRecordHook( HooksC::Record_EventComment, 6,
         &(fha->wstream), &time, &proc, &_comment, &kvs, &do_write );

      // write record
      if( do_write &&
          OTF_WStream_writeEventCommentKV( fha->wstream, time, proc,
             _comment.c_str(), kvs ) == 0 )
         ret = OTF_RETURN_ABORT;

      return ret;
   }
}

int
HandleEnter( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t func, uint32_t proc, uint32_t scl,
   OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_Enter, 5,
      &time, &func, &proc, &scl, &kvs );

   // get global token factory for DefFunction
   static const TokenFactoryScopeI * tkfac_deffunc =
      theTokenFactory->getScope( DEF_REC_TYPE__DefFunction );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

   // translate local function token
   //
   uint32_t global_func = tkfac_deffunc->translate( proc, func );
   vt_assert( global_func != 0 );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( proc, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_Enter, 7,
      &(fha->wstream), &time, &global_func, &proc, &global_scl, &kvs,
      &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeEnterKV( fha->wstream, time, global_func, proc,
          global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleLeave( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t func, uint32_t proc, uint32_t scl,
   OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_Leave, 5,
      &time, &func, &proc, &scl, &kvs );

   // get global token factory for DefFunction
   static const TokenFactoryScopeI * tkfac_deffunc =
      theTokenFactory->getScope( DEF_REC_TYPE__DefFunction );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

   // translate local function token, if necessary
   //
   uint32_t global_func = func;
   if( func != 0 )
   {
      global_func = tkfac_deffunc->translate( proc, func );
      vt_assert( global_func != 0 );
   }

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( proc, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_Leave, 7,
      &(fha->wstream), &time, &global_func, &proc, &global_scl, &kvs,
      &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeLeaveKV( fha->wstream, time, global_func, proc,
          global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleCounter( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t proc, uint32_t counter, uint64_t value,
   OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_Counter, 5,
      &time, &proc, &counter, &value, &kvs );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefCounter
   static const TokenFactoryScopeI * tkfac_defcntr =
      theTokenFactory->getScope( DEF_REC_TYPE__DefCounter );

   // try to get local process group token (!=0 if it's a group counter)
   uint32_t procgrp =
      theDefinitions->groupCounters()->getGroup( proc, counter );

   // translate local process group token, if necessary
   //
   uint32_t global_procgrp = procgrp;
   if( procgrp != 0 )
   {
      global_procgrp = tkfac_defprocgrp->translate( proc, procgrp );
      vt_assert( global_procgrp != 0 );
   }

   // translate local counter token
   //
   uint32_t global_counter = tkfac_defcntr->translate( proc, counter );
   vt_assert( global_counter != 0 );

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_Counter, 8,
      &(fha->wstream), &time, &proc, &global_procgrp, &global_counter, &value,
      &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeCounterKV( fha->wstream, time,
          global_procgrp ? global_procgrp : proc, global_counter, value,
          kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleBeginFileOp( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t proc, uint64_t matchid, uint32_t scl,
   OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_BeginFileOp, 5,
      &time, &proc, &matchid, &scl, &kvs );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( proc, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_BeginFileOp, 7,
      &(fha->wstream), &time, &proc, &matchid, &global_scl, &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeBeginFileOperationKV( fha->wstream, time, proc, matchid,
          global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleEndFileOp( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t proc, uint32_t file, uint64_t matchid,
   uint64_t handleid, uint32_t operation, uint64_t bytes, uint32_t scl,
   OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_EndFileOp, 8,
      &time, &proc, &file, &matchid, &handleid, &bytes, &scl, &kvs );

   // get global token factory for DefFile
   static const TokenFactoryScopeI * tkfac_deffile =
      theTokenFactory->getScope( DEF_REC_TYPE__DefFile );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

   // translate local file token
   //
   uint32_t global_file = tkfac_deffile->translate( proc, file );
   vt_assert( global_file != 0 );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( proc, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_EndFileOp, 12,
      &(fha->wstream), &time, &proc, &global_file, &matchid, &handleid,
      &operation, &bytes, &global_scl, &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeEndFileOperationKV( fha->wstream, time, proc,
          global_file, matchid, handleid, operation, bytes, global_scl,
          kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleSendMsg( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t sender, uint32_t receiver, uint32_t comm,
   uint32_t tag, uint32_t length, uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_SendMsg, 8,
      &time, &sender, &receiver, &comm, &tag, &length, &scl, &kvs );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

   // translate local comm. token
   //
   uint32_t global_comm = tkfac_defprocgrp->translate( sender, comm );
   vt_assert( global_comm != 0 );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( sender, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( sender, kvs );

   // correct time
   time = theTimeSync->correctTime( sender, time );

   // get receiver process id, if it's an user communication
   //
   if( theUserCom->isUserComm( global_comm ) &&
       ( receiver = theUserCom->getReceiver( global_comm, tag ) ) == 0 )
   {
      // TODO: show warning message?
      return OTF_RETURN_OK;
   }

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
   // drop record, if receiver stream isn't available
   if( !isStreamAvail( receiver ) )
      return OTF_RETURN_OK;
#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_SendMsg, 10,
      &(fha->wstream), &time, &sender, &receiver, &global_comm, &tag, &length,
      &global_scl, &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeSendMsgKV( fha->wstream, time, sender, receiver,
          global_comm, tag, length, global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleRecvMsg( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t receiver, uint32_t sender, uint32_t comm,
   uint32_t tag, uint32_t length, uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_RecvMsg, 8,
      &time, &receiver, &sender, &comm, &tag, &length, &scl, &kvs );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

   // translate local comm. token
   //
   uint32_t global_comm = tkfac_defprocgrp->translate( receiver, comm );
   vt_assert( global_comm != 0 );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( receiver, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( receiver, kvs );

   // correct time
   time = theTimeSync->correctTime( receiver, time );

   // get sender process id, if it's an user communication
   //
   if( theUserCom->isUserComm( global_comm ) &&
       ( sender = theUserCom->getSender( global_comm, tag ) ) == 0 )
   {
      // TODO: show warning message?
      return OTF_RETURN_OK;
   }

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
   // drop record, if sender stream isn't available
   if( !isStreamAvail( sender ) )
      return OTF_RETURN_OK;
#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_RecvMsg, 10,
      &(fha->wstream), &time, &receiver, &sender, &global_comm, &tag, &length,
      &scl, &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeRecvMsgKV( fha->wstream, time, receiver, sender,
          global_comm, tag, length, global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleBeginCollOp( FirstHandlerArg_EventsS * fha,
       uint64_t time, uint32_t proc, uint32_t operation, uint64_t matchid,
       uint32_t comm, uint32_t root, uint64_t sent, uint64_t recvd,
       uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_BeginCollOp, 10,
      &time, &proc, &operation, &matchid, &comm, &root, &sent, &recvd, &scl,
      &kvs );

   // get global token factory for DefCollOp
   static const TokenFactoryScopeI * tkfac_defcollop =
      theTokenFactory->getScope( DEF_REC_TYPE__DefCollOp );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

   // translate local operation token
   //
   uint32_t global_operation = tkfac_defcollop->translate( proc, operation );
   vt_assert( global_operation != 0 );

   // translate local comm. token
   //
   uint32_t global_comm = tkfac_defprocgrp->translate( proc, comm );
   vt_assert( global_comm != 0 );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( proc, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_BeginCollOp, 12,
      &(fha->wstream), &time, &proc, &global_operation, &matchid, &global_comm,
      &root, &sent, &recvd, &global_scl, &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeBeginCollectiveOperationKV( fha->wstream, time, proc,
          global_operation, matchid, global_comm, root, sent, recvd,
          global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleEndCollOp( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t proc, uint64_t matchid, OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_EndCollOp, 4,
      &time, &proc, &matchid, &kvs );

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_EndCollOp, 6,
      &(fha->wstream), &time, &proc, &matchid, &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeEndCollectiveOperationKV( fha->wstream, time, proc,
          matchid, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleRMAPut( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t proc, uint32_t origin, uint32_t dest, uint32_t comm,
   uint32_t tag, uint64_t bytes, uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_RMAPut, 9,
      &time, &proc, &origin, &dest, &comm, &tag, &bytes, &scl, &kvs );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
   // drop record, if origin or destination stream isn't available
   if( !isStreamAvail( origin ) || !isStreamAvail( dest ) )
      return OTF_RETURN_OK;
#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

   // translate local comm. token
   //
   uint32_t global_comm = tkfac_defprocgrp->translate( proc, comm );
   vt_assert( global_comm != 0 );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( proc, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_RMAPut, 11,
      &(fha->wstream), &time, &proc, &origin, &dest, &global_comm, &tag, &bytes,
      &global_scl, &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeRMAPutKV( fha->wstream, time, proc, origin, dest,
          global_comm, tag, bytes, global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleRMAPutRemoteEnd( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t proc, uint32_t origin, uint32_t dest, uint32_t comm,
   uint32_t tag, uint64_t bytes, uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_RMAPutRemoteEnd, 9,
      &time, &proc, &origin, &dest, &comm, &tag, &bytes, &scl, &kvs );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
   // drop record, if origin or destination stream isn't available
   if( !isStreamAvail( origin ) || !isStreamAvail( dest ) )
      return OTF_RETURN_OK;
#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

   // translate local comm. token
   //
   uint32_t global_comm = tkfac_defprocgrp->translate( proc, comm );
   vt_assert( global_comm != 0 );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( proc, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_RMAPutRemoteEnd, 11,
      &(fha->wstream), &time, &proc, &origin, &dest, &global_comm, &tag, &bytes,
      &global_scl, &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeRMAPutRemoteEndKV( fha->wstream, time, proc, origin,
          dest, global_comm, tag, bytes, global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleRMAGet( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t proc, uint32_t origin, uint32_t dest, uint32_t comm,
   uint32_t tag, uint64_t bytes, uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_RMAGet, 9,
      &time, &proc, &origin, &dest, &comm, &tag, &bytes, &scl, &kvs );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
   // drop record, if origin or destination stream isn't available
   if( !isStreamAvail( origin ) || !isStreamAvail( dest ) )
      return OTF_RETURN_OK;
#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

   // translate local comm. token
   //
   uint32_t global_comm = tkfac_defprocgrp->translate( proc, comm );
   vt_assert( global_comm != 0 );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( proc, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_RMAGet, 11,
      &(fha->wstream), &time, &proc, &origin, &dest, &global_comm, &tag, &bytes,
      &global_scl, &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeRMAGetKV( fha->wstream, time, proc, origin, dest,
          global_comm, tag, bytes, global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleRMAEnd( FirstHandlerArg_EventsS * fha,
   uint64_t time, uint32_t proc, uint32_t remote, uint32_t comm, uint32_t tag,
   uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_RMAEnd, 7,
      &time, &proc, &remote, &comm, &tag, &scl, &kvs );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefScl
   static const TokenFactoryScopeI * tkfac_defscl =
      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
   // drop record, if remote stream isn't available
   if( !isStreamAvail( remote ) )
      return OTF_RETURN_OK;
#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

   // translate local comm. token
   //
   uint32_t global_comm = tkfac_defprocgrp->translate( proc, comm );
   vt_assert( global_comm != 0 );

   // translate local scl token, if necessary
   //
   uint32_t global_scl = scl;
   if( scl != 0 )
   {
      global_scl = tkfac_defscl->translate( proc, scl );
      vt_assert( global_scl != 0 );
   }

   // translate local key token(s)
   handleKeyValueList( proc, kvs );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_RMAEnd, 9,
      &(fha->wstream), &time, &proc, &remote, &global_comm, &tag, &global_scl,
      &kvs, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeRMAEndKV( fha->wstream, time, proc, remote, global_comm,
          tag, global_scl, kvs ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

// summary record handlers
//

int
HandleFunctionSummary( FirstHandlerArg_StatsS * fha,
   uint64_t time, uint32_t func, uint32_t proc, uint64_t invocations,
   uint64_t exclTime, uint64_t inclTime )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_FunctionSummary, 6,
      &time, &func, &proc, &invocations, &exclTime, &inclTime );

   // get global token factory for DefFunction
   static const TokenFactoryScopeI * tkfac_deffunc =
      theTokenFactory->getScope( DEF_REC_TYPE__DefFunction );

   // translate local function token
   //
   uint32_t global_func = tkfac_deffunc->translate( proc, func );
   vt_assert( global_func != 0 );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_FunctionSummary, 8,
      &(fha->wstream), &time, &global_func, &proc, &invocations, &exclTime,
      &inclTime, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeFunctionSummary( fha->wstream, time, global_func,
          proc, invocations, exclTime, inclTime ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleMessageSummary( FirstHandlerArg_StatsS * fha,
   uint64_t time, uint32_t proc, uint32_t peer, uint32_t comm, uint32_t type,
   uint64_t sentNum, uint64_t recvNum, uint64_t sentBytes, uint64_t recvBytes )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_MessageSummary, 9,
      &time, &proc, &peer, &comm, &type, &sentNum, &recvNum, &sentBytes,
      &recvBytes );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // translate local process group, if necessary
   //
   uint32_t global_comm = comm;
   if( comm != 0 )
   {
      global_comm = tkfac_defprocgrp->translate( proc, comm );
      vt_assert( global_comm != 0 );
   }

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_MessageSummary, 10,
      &(fha->wstream), &time, &proc, &peer, &global_comm, &sentNum, &recvNum,
      &sentBytes, &recvBytes, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeMessageSummary( fha->wstream, time, proc, peer,
          global_comm, type, sentNum, recvNum, sentBytes, recvBytes ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleCollOpSummary( FirstHandlerArg_StatsS * fha,
   uint64_t time, uint32_t proc, uint32_t comm, uint32_t collop,
   uint64_t sentNum, uint64_t recvNum, uint64_t sentBytes, uint64_t recvBytes )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_CollOpSummary, 8,
      &time, &proc, &comm, &collop, &sentNum, &recvNum, &sentBytes,
      &recvBytes );

   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefCollOp
   static const TokenFactoryScopeI * tkfac_defcollop =
      theTokenFactory->getScope( DEF_REC_TYPE__DefCollOp );

   // translate local process group, if necessary
   //
   uint32_t global_comm = comm;
   if( comm != 0 )
   {
      global_comm = tkfac_defprocgrp->translate( proc, comm );
      vt_assert( global_comm != 0 );
   }

   // translate local coll. op, if necessary
   //
   uint32_t global_collop = collop;
   if( collop != 0 )
   {
      global_collop = tkfac_defcollop->translate( proc, collop );
      vt_assert( global_collop != 0 );
   }

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_CollOpSummary, 10,
      &(fha->wstream), &time, &proc, &global_comm, &global_collop, &sentNum,
      &recvNum, &sentBytes, &recvBytes, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeCollopSummary( fha->wstream, time, proc,
          global_comm, global_collop, sentNum, recvNum, sentBytes,
          recvBytes ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}

int
HandleFileOpSummary( FirstHandlerArg_StatsS * fha,
   uint64_t time, uint32_t file, uint32_t proc, uint64_t nopen, uint64_t nclose,
   uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesRead,
   uint64_t bytesWrite )
{
   int ret = OTF_RETURN_OK;

   bool do_write = true;

   // trigger read record hook
   theHooks->triggerReadRecordHook( HooksC::Record_FileOpSummary, 10,
      &time, &file, &proc, &nopen, &nclose, &nread, &nwrite, &nseek,
      &bytesRead, &bytesWrite );

   // get global token factory for DefFile
   static const TokenFactoryScopeI * tkfac_deffile =
      theTokenFactory->getScope( DEF_REC_TYPE__DefFile );

   // translate local file
   //
   uint32_t global_file = tkfac_deffile->translate( proc, file );
   vt_assert( global_file != 0 );

   // correct time
   time = theTimeSync->correctTime( proc, time );

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_FileOpSummary, 12,
      &(fha->wstream), &time, &global_file, &proc, &nopen,  &nclose, &nread,
      &nwrite, &nseek, &bytesRead, &bytesWrite, &do_write );

   // write record
   if( do_write &&
       OTF_WStream_writeFileOperationSummary( fha->wstream, time, global_file,
          proc, nopen, nclose, nread, nwrite, nseek, bytesRead,
          bytesWrite ) == 0 )
      ret = OTF_RETURN_ABORT;

   return ret;
}
