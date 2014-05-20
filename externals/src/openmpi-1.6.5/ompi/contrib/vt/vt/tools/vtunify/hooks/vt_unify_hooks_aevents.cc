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
#include "vt_unify_hooks_aevents.h"
#include "vt_unify_sync.h"
#include "vt_unify_tkfac.h"

#include <iostream>

//////////////////// class HooksAsyncEventsC ////////////////////

// class's instance object to allow access from static member methods
HooksAsyncEventsC * HooksAsyncEventsC::Obj = 0;

// maximum number of queued events per async. source
uint32_t HooksAsyncEventsC::AsyncSourceManagerS::SourceS::MaxQueueSize = 0;

// public methods
//

HooksAsyncEventsC::HooksAsyncEventsC() : HooksBaseC()
{
   Obj = this;
}

HooksAsyncEventsC::~HooksAsyncEventsC()
{
   // Empty
}

// private methods
//

// record handlers for async. events
//

bool
HooksAsyncEventsC::HandleAsyncEventPre(
   AsyncSourceManagerS::SourceS & source, const uint32_t & proc,
   uint64_t & time, OTF_KeyValueList *& kvs )
{
   bool ret = false;

   // translate local key token(s)
   HandleKeyValueList( proc, kvs );

   do
   {
      // ignore event, if it doesn't belong to the given async. source
      if( !Obj->isAsyncEvent( kvs, source.key ) )
         break;

      // get lower bound for async. timestamp (=process' start timestamp)
      const uint64_t min_async_time =
         theTimeSync->getTimeRange( proc ).first;

      uint64_t actual_time;
      uint8_t otf_rc;

      // get actual time from async. event's key-value list
      //
      otf_rc = OTF_KeyValueList_getUint64( kvs, source.key, &actual_time );
      vt_assert( otf_rc == 0 );

      // ignore async. event, if its time deceeds lower bound
      //
      if( actual_time < min_async_time )
      {
         PVPrint( 3, "    Warning: Time underflow on async. event "
            "[async. source %x time %llu < %llu] - Ignored\n",
            source.key, (unsigned long long int)actual_time,
            (unsigned long long int)min_async_time );

         break;
      }

      // correct actual time
      time = theTimeSync->correctTime( proc, actual_time );

      // get a copy of key-value list
      //
      OTF_KeyValueList * new_kvs = OTF_KeyValueList_new();
      vt_assert( new_kvs );
      otf_rc = OTF_KeyValueList_appendKeyValueList( new_kvs, kvs );
      vt_assert( otf_rc == 0 );
      kvs = new_kvs;

      // remove key of actual time from key-value list
      //
      otf_rc = OTF_KeyValueList_removeKey( kvs, source.key );
      vt_assert( otf_rc == 0 );

      ret = true;

   } while( false );

   return ret;
}

bool
HooksAsyncEventsC::HandleAsyncEventPost(
   AsyncSourceManagerS::SourceS & source, AsyncEventBaseS *& newAsyncEvent )
{
   bool error = false;

   vt_assert( newAsyncEvent );

   // abort, if time of async. event isn't increasing
   //
   if( !source.event_queue.empty() &&
       source.event_queue.back()->time > newAsyncEvent->time )
   {
      std::cerr << ExeName << ": Error: "
                << "Could not enqueue async. event: time not increasing "
                << "[async. source " << std::hex << source.key << std::dec
                << " time " << newAsyncEvent->time << "]" << std::endl;
      error = true;
   }
   // otherwise, add new async. event to queue
   //
   else
   {
      source.event_queue.push_back( newAsyncEvent );
   }

   return !error;
}

int
HooksAsyncEventsC::HandleAsyncCounter( AsyncSourceManagerS::SourceS * source,
   uint64_t time, uint32_t proc, uint32_t counter, uint64_t value,
   OTF_KeyValueList * kvs )
{
   int ret = OTF_RETURN_OK;

   // pre-handle event: get actual time of async. event from key-value list
   if( HandleAsyncEventPre( *source, proc, time, kvs ) )
   {
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

      // create new async. event
      //
      AsyncEventBaseS * new_async_event =
         new AsyncEventCounterS( time, kvs, global_procgrp, global_counter,
            value );
      vt_assert( new_async_event );

      // post-handle event: enqueue new async. event
      if( !HandleAsyncEventPost( *source, new_async_event ) )
         ret = OTF_RETURN_ABORT;
   }

   return ret;
}

// TODO: define further record handlers for async. events here

// vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

// initialization/finalization hooks
//

void
HooksAsyncEventsC::initHook()
{
   // Empty
}

void
HooksAsyncEventsC::finalizeHook( const bool & error )
{
   // Empty
}

// phase hooks
//

void
HooksAsyncEventsC::phaseHook_UnifyEvents_pre()
{
   bool error = false;

   VPrint( 2, " Preparing merge of async. events\n" );

#ifdef VT_MPI
   // share async. source keys to all ranks, if necessary
   //
   if( NumRanks > 1 )
      error = !shareSourceKeys();
#endif // VT_MPI

   // create async. source managers for each stream to read
   //
   if( !error && !m_sourceKeys.empty() )
   {
      // set maximum number of queued events per async. source
      AsyncSourceManagerS::SourceS::MaxQueueSize =
         AsyncSourceManagerS::MAX_QUEUED_EVENTS / m_sourceKeys.size();

      // iterate over all streams to read
      for( uint32_t i = 0; i < MyStreamIds.size(); i++ )
      {
         const uint32_t & stream_id = MyStreamIds[i];

         // create async. source manager for stream
         AsyncSourceManagerS & manager = m_stream2SourceManager.insert(
            std::make_pair( stream_id, AsyncSourceManagerS() ) ).first->second;

         // iterate over all async. sources
         for( std::set<uint32_t>::const_iterator it = m_sourceKeys.begin();
              it != m_sourceKeys.end(); ++it )
         {
            const uint32_t & key = *it;

            // create async. source
            manager.sources[key] = AsyncSourceManagerS::SourceS( key );
         }
      }
   }

   if( !error )
      VPrint( 2, " Continuing unification of events\n" );

   //return !error;
   vt_assert( !error );
}

// record hooks
//

void
HooksAsyncEventsC::writeRecHook_DefKeyValue( HooksC::VaArgsT & args )
{
   // return, the input trace has no events
   if( !UnifyControlS::have_events() )
      return;

   // get string identifier for async. source key as std::string
   static const std::string async_source_key_prefix =
      VT_UNIFY_STRID_ASYNC_SOURCE_KEY;

   // get hook arguments
   //

   //OTF_WStream ** wstream     = (OTF_WStream**)args[0];
   uint32_t *     key         = (uint32_t*)args[1];
   OTF_Type *     type        = (OTF_Type*)args[2];
   std::string *  name        = (std::string*)args[3];
   bool *         do_write    = (bool*)args[4];

   // is it a key for an async. source?
   if( *type == OTF_UINT64 &&
       name->length() > async_source_key_prefix.length() &&
       name->compare( 0, async_source_key_prefix.length(),
                      async_source_key_prefix ) == 0 )
   {
      // add key to set
      m_sourceKeys.insert( *key );

      // prevent writing of this key-value definition record
      *do_write = false;
   }
}

void
HooksAsyncEventsC::writeRecHook_Event( uint64_t * time, uint32_t * streamid,
   OTF_KeyValueList ** kvs, bool * dowrite )
{
   bool error = false;

   // get async. source manager by stream id, if necessary
   //
   static AsyncSourceManagerS * manager = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(manager)
#endif // HAVE_OMP
   if( !manager || manager->stream_id != *streamid )
   {
      manager = getSourceManagerByStreamId( *streamid );
      vt_assert( manager );
   }

   // ignore this event, if write record hooks are suspended
   if( !manager->hooks_suspended )
   {
      // write queued async. events
      error = !writeAsyncEvents( *manager, *time );

      // drop this event record, if it's asynchronous
      *dowrite = !isAsyncEvent( *kvs );
   }

   //return !error;
   vt_assert( !error );
}

// generic hook
void
HooksAsyncEventsC::genericHook( const uint32_t & id, HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if the input trace has no events of no async. source defined
   if( !UnifyControlS::have_events() || m_sourceKeys.empty() )
      return /* true */;

   if( ( id & VT_UNIFY_HOOKS_AEVENTS_GENID__EVENT_WSTREAM_OPEN ) != 0 )
   {
      // get hook arguments
      //
      OTF_WStream ** wstream = (OTF_WStream**)args[0];
      uint32_t * stream_id = (uint32_t*)args[1];
      std::string * stream_prefix = (std::string*)args[2];

      // get async. source manager by stream id
      //
      AsyncSourceManagerS * manager = getSourceManagerByStreamId( *stream_id );
      vt_assert( manager );

      // open reader streams of async. sources
      error = !openSources( *manager, *stream_id, *stream_prefix, *wstream );
   }
   else if( ( id & VT_UNIFY_HOOKS_AEVENTS_GENID__EVENT_WSTREAM_CLOSE ) != 0 )
   {
      // get hook arguments
      //
      //OTF_WStream ** wstream = (OTF_WStream**)args[0];
      uint32_t * stream_id = (uint32_t*)args[1];

      // get async. source manager by stream id
      //
      AsyncSourceManagerS * manager = getSourceManagerByStreamId( *stream_id );
      vt_assert( manager );

      // write remaining queued async. events and close reader streams
      error = !closeSources( *manager );
   }

   //return !error;
   vt_assert( !error );
}

// ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

bool
HooksAsyncEventsC::openSources( AsyncSourceManagerS & manager,
   const uint32_t & streamId, const std::string & streamPrefix,
   OTF_WStream *& wstream )
{
   bool error = false;

   vt_assert( !manager.opened );

   manager.stream_id = streamId;
   manager.stream_prefix = streamPrefix;
   manager.wstream = wstream;

   // open reader streams of each async. source
   //
   for( std::map<uint32_t, AsyncSourceManagerS::SourceS>::iterator it =
        manager.sources.begin(); it != manager.sources.end(); ++it )
   {
      AsyncSourceManagerS::SourceS & source = it->second;

      // open file manager
      //
      source.file_manager = OTF_FileManager_open( 1 );
      vt_assert( source.file_manager );

      // initialize IOFSL stuff for reading, if necessary
      //
      if( UnifyControlS::is_iofsl() )
      {
         OTF_IofslMode otf_iofsl_mode =
            ( UnifyControlS::iofsl_mode == VT_IOFSL_MODE_MULTIFILE ) ?
               OTF_IOFSL_MULTIFILE : OTF_IOFSL_MULTIFILE_SPLIT;

         OTF_FileManager_setIofsl( source.file_manager,
            UnifyControlS::iofsl_num_servers, 0, otf_iofsl_mode, 0, 0,
            VT_TRACEID_BITMASK );
      }

      // open stream for reading
      //
      source.rstream =
         OTF_RStream_open( manager.stream_prefix.c_str(), manager.stream_id,
            source.file_manager );
      vt_assert( source.rstream );

      PVPrint( 3, "  Opened OTF reader stream for reading async. events ahead "
               "[namestub %s id %x async. source %x]\n",
               manager.stream_prefix.c_str(), manager.stream_id, source.key );

      // set record limit
      OTF_RStream_setRecordLimit( source.rstream, 1 );

      // create record handler array
      //
      source.handler_array = OTF_HandlerArray_open();
      vt_assert( source.handler_array );

      // set record handler and its first argument for ...
      //

      // ... OTF_COUNTER_RECORD
      OTF_HandlerArray_setHandler( source.handler_array,
         (OTF_FunctionPointer*)HooksAsyncEventsC::HandleAsyncCounter,
         OTF_COUNTER_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( source.handler_array, &source,
         OTF_COUNTER_RECORD );

      // TODO: set further record handlers for async. events here
   }

   // set async. source manager status
   manager.opened = true;

   // read events ahead for all async. sources
   error = !readAhead( manager );

   return !error;
}

bool
HooksAsyncEventsC::closeSources( AsyncSourceManagerS & manager )
{
   bool error = false;

   vt_assert( manager.opened );

   // write remaining queued async. events
   error = !writeAsyncEvents( manager );

   if( !error )
   {
      // close reader streams of each async. source
      //
      for( std::map<uint32_t, AsyncSourceManagerS::SourceS>::iterator it =
           manager.sources.begin(); it != manager.sources.end(); ++it )
      {
         AsyncSourceManagerS::SourceS & source = it->second;

         // close record handler array
         OTF_HandlerArray_close( source.handler_array );
         // close reader stream
         OTF_RStream_close( source.rstream );
         // close file manager
         OTF_FileManager_close( source.file_manager );

         PVPrint( 3, "  Closed OTF reader stream for reading async. events ahead "
                  "[namestub %s id %x async. source %x]\n",
                  manager.stream_prefix.c_str(), manager.stream_id, source.key );
      }

      // set async. source manager status
      manager.opened = false;
   }

   return !error;
}

bool
HooksAsyncEventsC::readAhead( AsyncSourceManagerS & manager,
   const uint32_t & sourceKey )
{
   bool error = false;

   vt_assert( manager.opened );

   // either read events ahead for all async. sources
   //
   if( sourceKey == 0 )
   {
      // call myself for each async. source key
      //
      for( std::map<uint32_t, AsyncSourceManagerS::SourceS>::const_iterator it =
           manager.sources.begin(); it != manager.sources.end(); ++it )
      {
         const uint32_t & source_key = it->first;

         if( ( error = !readAhead( manager, source_key ) ) )
            break;
      }
   }
   // or read events ahead for certain async. source
   //
   else
   {
      // get async. source by given key
      //
      std::map<uint32_t, AsyncSourceManagerS::SourceS>::iterator it =
         manager.sources.find( sourceKey );
      vt_assert( it != manager.sources.end() );

      AsyncSourceManagerS::SourceS & source = it->second;

      // read events, if not already finished
      //
      if( !source.finished_reading )
      {
         PVPrint( 3, "   Reading ahead for async. events "
                  "[namestub %s id %x async. source %x]\n",
                  manager.stream_prefix.c_str(), manager.stream_id, source.key );

         while( true )
         {
            // read events
            uint64_t records_read =
               OTF_RStream_readEvents( source.rstream, source.handler_array );

            // stop reading if ...
            //

            // ... read error occurred,
            if( records_read == OTF_READ_ERROR )
            {
               std::cerr << ExeName << ": Error: "
                         << "Could not read ahead for async. events of "
                         << "OTF stream [namestub " << manager.stream_prefix
                         << " id " << std::hex << manager.stream_id
                         << " async. source " << source.key << std::dec << "]"
                         << std::endl;
               error = true;
               break;
            }
            // ... reading finished,
            else if( records_read == 0 )
            {
               source.finished_reading = true;
               break;
            }
            // ... or max. number of queued async. events reached
            else if( source.event_queue.size() == source.MaxQueueSize )
            {
               break;
            }
         }
      }
   }

   return !error;
}

bool
HooksAsyncEventsC::writeAsyncEvents( AsyncSourceManagerS & manager,
   const uint64_t & curTime )
{
   bool error = false;

   // flag: async. event written?
   bool written;

   do
   {
      written = false;

      // search for async. source which has the next async. event on its
      // event queue
      //

      AsyncSourceManagerS::SourceS * next_source = 0;

      // iterate over all async. sources
      for( std::map<uint32_t, AsyncSourceManagerS::SourceS>::iterator it =
           manager.sources.begin(); it != manager.sources.end(); ++it )
      {
         AsyncSourceManagerS::SourceS & source = it->second;

         // read ahead, if source's event queue is empty
         //
         if( !source.finished_reading && source.event_queue.empty() )
         {
            if( ( error = !readAhead( manager, source.key ) ) )
               break;
         }

         // go to the next async. source, if event queue is still empty
         if( source.event_queue.empty() )
            continue;

         // update pointer to async. source of next async. event, if necessary
         if( !next_source ||
             source.event_queue.front()->time <
             next_source->event_queue.front()->time )
           next_source = &source;
      }

      // write async. event
      //

      if( !error && next_source &&
          next_source->event_queue.front()->time <= curTime )
      {
         AsyncEventBaseS * top = next_source->event_queue.front();

         // suspend write record hooks to avoid recursion
         manager.hooks_suspended = true;

         // write async. event depending on its type
         //
         switch( top->type )
         {
            case ASYNC_EVENT_TYPE_COUNTER:
            {
               bool do_write = true;

               AsyncEventCounterS * record =
                  static_cast<AsyncEventCounterS*>( top );

               // trigger write record hook
               theHooks->triggerWriteRecordHook( HooksC::Record_Counter, 8,
                  &(manager.wstream), &(record->time), &(manager.stream_id),
                  &(record->procgrp), &(record->counter), &(record->value),
                  &(record->kvs), &do_write );

               // write record
               if( do_write )
                  error = ( OTF_WStream_writeCounterKV( manager.wstream,
                               record->time,
                               record->procgrp ?
                                  record->procgrp : manager.stream_id,
                               record->counter, record->value,
                               record->kvs ) == 0 );

               break;
            }
            // TODO: handle further async. event types here
            default: // ASYNC_EVENT_TYPE_UNKNOWN
            {
               vt_assert( 0 );
            }
         }

         // abort, if writing failed
         //
         if( error )
         {
            std::cerr << ExeName << ": Error: "
                      << "Could not write async. event to OTF stream "
                      << "[namestub " << manager.stream_prefix << " id "
                      << std::hex << manager.stream_id << " async. source "
                      << next_source->key << std::dec << "]" << std::endl;
            break;
         }

         // remove processed async. event from queue
         //
         delete top;
         next_source->event_queue.pop_front();

         // unsuspend write record hooks
         manager.hooks_suspended = false;

         written = true;
      }

   } while( written );

   return !error;
}

HooksAsyncEventsC::AsyncSourceManagerS *
HooksAsyncEventsC::getSourceManagerByStreamId( const uint32_t & streamId )
{
   AsyncSourceManagerS * ret = 0;

   std::map<uint32_t, AsyncSourceManagerS>::iterator it =
      m_stream2SourceManager.find( streamId );

   if( it != m_stream2SourceManager.end() )
      ret = &(it->second);

   return ret;
}

bool
HooksAsyncEventsC::isAsyncEvent( OTF_KeyValueList *& kvs,
   const uint32_t & sourceKey ) const
{
   bool ret = false;

   // either check for all defined async. source keys
   //
   if( sourceKey == 0 )
   {
      for( std::set<uint32_t>::const_iterator it = m_sourceKeys.begin();
           it != m_sourceKeys.end() && !ret; ++it )
      {
         ret = ( OTF_KeyValueList_hasKey( kvs, *it ) == 0 );
      }
   }
   // or check by given async. source key
   //
   else
   {
      ret = ( OTF_KeyValueList_hasKey( kvs, sourceKey ) == 0 );
   }

   return ret;
}

#ifdef VT_MPI

bool
HooksAsyncEventsC::shareSourceKeys()
{
   bool error = false;

   vt_assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   VPrint( 2, "  Sharing global key tokens for async. event sources\n" );

   uint32_t keys_num;

   MASTER
   {
      // get number of async. source keys (=buffer size)
      keys_num = m_sourceKeys.size();
   }

   // broadcast buffer size
   CALL_MPI( MPI_Bcast( &keys_num, 1, MPI_UNSIGNED, 0, MPI_COMM_WORLD ) );

   if( keys_num > 0 )
   {
      uint32_t * keys;

      // allocate memory for the send/receive buffer
      //
      keys = new uint32_t[keys_num];
      vt_assert( keys );

      MASTER
      {
         // fill send buffer
         //
         uint32_t i = 0;
         for( std::set<uint32_t>::const_iterator it = m_sourceKeys.begin();
              it != m_sourceKeys.end(); ++it, i++ )
         {
            keys[i] = *it;
         }
      }

      // broadcast buffer
      CALL_MPI( MPI_Bcast( keys, keys_num, MPI_UNSIGNED, 0, MPI_COMM_WORLD ) );

      SLAVE
      {
         // add async. source keys to set
         //
         for( uint32_t i = 0; i < keys_num; i++ )
         {
            m_sourceKeys.insert( keys[i] );
         }
      }

      delete [] keys;
   }

   return !error;
}

#endif // VT_MPI
