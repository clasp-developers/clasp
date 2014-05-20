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

#include "vt_unify_defs_recs.h"
#include "vt_unify_hooks_msgmatch_snaps.h"
#include "vt_unify_sync.h"
#include "vt_unify_tkfac.h"
#include "vt_unify_usrcom.h"

#include <stdio.h>
#include <string.h>

//////////////////// class HooksMsgMatchAndSnapsC ////////////////////

// public methods
//

HooksMsgMatchAndSnapsC::HooksMsgMatchAndSnapsC() : HooksBaseC(),
   m_maxThreads( 1 ), m_maxTime( (uint64_t)-1 ), m_numSnapshots( 0 ),
   m_snapshotInterval( 0 ), m_thumbnailWidth( MAX_THUMBNAIL_WIDTH ),
   m_thumbnailHeight( MAX_THUMBNAIL_HEIGHT )
{
   // initialize global key tokens for message matching
   for( uint32_t i = 0; i < KEY_NUM; i++ )
      m_msgMatchKeyTokens[i] = 0;
}

HooksMsgMatchAndSnapsC::~HooksMsgMatchAndSnapsC()
{
   // Empty
}

// private methods
//

// handler for rewriting definition comment records to insert warning
// comments about message matching irregularities
int
HooksMsgMatchAndSnapsC::HandleDefComment( MsgMatchBumpsS * msgMatchBumps,
   uint32_t stream, const char * comment )
{
   int ret = OTF_RETURN_OK;

   (void)stream;

   static uint32_t comment_idx = 0;
   comment_idx++;

   do
   {
      // copy comment record
      //
      if( OTF_WStream_writeDefinitionComment( msgMatchBumps->wstream,
             comment ) == 0 )
      {
         ret = OTF_RETURN_ABORT;
         break;
      }

      // index reached where the warning comments about message matching
      // irregularities shall be inserted?
      if( comment_idx == msgMatchBumps->def_comment_idx )
      {
         // write warning comment about
         // unmatched messages (i == 0) and reversed messages (i == 1)
         //
         for( uint8_t i = 0; i < 2; i++ )
         {
            uint64_t num;
            const char* fmt;

            if( i == 0 && msgMatchBumps->num_unmatched > 0 )
            {
               num = msgMatchBumps->num_unmatched;
               fmt = MsgMatchBumpsS::UNMATCHED_WARNING_FMT();
            }
            else if( i == 1 && msgMatchBumps->num_reversed > 0 )
            {
               num = msgMatchBumps->num_reversed;
               fmt = MsgMatchBumpsS::REVERSED_WARNING_FMT();
            }
            else
            {
               continue;
            }

            const double percent =
               ( (double)num / (double)msgMatchBumps->num_messages ) * 100.0;
            char percent_str[10];
            if( percent < 1.0 )
               strcpy( percent_str, "<1%" );
            else
               sprintf( percent_str, "%.1f%%", percent );

            char warning[STRBUFSIZE];
            snprintf( warning, sizeof(warning)-1, fmt,
                      (unsigned long long int)num, percent_str );

            if( OTF_WStream_writeDefinitionComment( msgMatchBumps->wstream,
                   warning ) == 0 )
            {
               ret = OTF_RETURN_ABORT;
               break;
            }
         }
      }

   } while( false );

   return ret;
}

// event record handlers for reading receive messages for message matching
//

int
HooksMsgMatchAndSnapsC::HandleEventComment( void * userData,
   uint64_t time, uint32_t proc, const char * comment )
{
   (void)userData;
   (void)time;

#ifdef VT_ETIMESYNC
   // update time sync. parameters, if necessary
   //
   if( theTimeSync->getSyncMethod() == TimeSyncC::METHOD_ENHANCED &&
       strcmp( comment, VT_UNIFY_STRID_ETIMESYNC_COMMENT ) == 0 )
   {
      theTimeSync->updateSyncParam( proc );
   }
#endif // VT_ETIMESYNC

   return OTF_RETURN_OK;
}

int
HooksMsgMatchAndSnapsC::HandleRecvMsg( LargeVectorC<RecvMsgS*> * recvMsgs,
   uint64_t time, uint32_t receiver, uint32_t sender, uint32_t comm,
   uint32_t tag, uint32_t length, uint32_t scl )
{
   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefScl
   //static const TokenFactoryScopeI * tkfac_defscl =
   //   theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

   // translate local comm. token
   //
   uint32_t global_comm = tkfac_defprocgrp->translate( receiver, comm );
   vt_assert( global_comm != 0 );

   // translate local scl token, if necessary
   //
   //uint32_t global_scl = scl;
   //if( scl != 0 )
   //{
   //   global_scl = tkfac_defscl.translate( receiver, scl );
   //   vt_assert( global_scl != 0 );
   //}

   // correct time
   time = theTimeSync->correctTime( receiver, time );

   // get sender process id, if it's an user communication
   //
   if( theUserCom->isUserComm( global_comm ) &&
       ( sender = theUserCom->getSender( global_comm, tag ) ) == 0 )
   {
      // ignore receive message, if no sender process id found
      if( sender == 0 )
         return OTF_RETURN_OK;
   }

   // add receive message to vector, if sender stream is available
   //
   if( StreamId2UnifyCtl[sender]->stream_avail )
   {
      recvMsgs->push_back( new RecvMsgS( time, sender, receiver, global_comm,
         tag /*, length, global_scl*/ ) );
      vt_assert( recvMsgs->back() );
   }

   return OTF_RETURN_OK;
}

// callback function for releasing snapshot event data (i.e. key-values)
int
HooksMsgMatchAndSnapsC::ReleaseEventDataCB( void * userData,
   OTF_KeyValueList * kvs )
{
   (void)userData;

   // close snapshot's key-value list
   OTF_KeyValueList_close( kvs );

   return 1;
}

// callback functions called when writing a snapshot
//

int
HooksMsgMatchAndSnapsC::WriteEnterSnapshotCB( OTF_WStream * wstream,
   uint64_t snapshotTime, uint64_t eventTime, uint64_t proc, uint32_t func,
   uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = 1;
   bool do_write = true;

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_EnterSnapshot, 8,
      &wstream, &snapshotTime, &eventTime, &func, &proc, &scl,
      &kvs, &do_write );

   // write snapshot record
   //
   if( do_write )
   {
      ret =
         OTF_WStream_writeEnterSnapshotKV( wstream, snapshotTime, eventTime,
            func, proc, scl, kvs );
   }

   return ret;
}

int
HooksMsgMatchAndSnapsC::WriteSendSnapshotCB( OTF_WStream * wstream,
   uint64_t snapshotTime, uint64_t eventTime, uint64_t sender,
   uint64_t receiver, uint32_t comm, uint32_t tag, uint32_t length,
   uint32_t scl, uint64_t recvTime, uint32_t recvLength, uint32_t recvScl,
   OTF_KeyValueList * kvs )
{
   int ret = 1;
   bool do_write = true;

   (void)recvTime;
   (void)recvLength;
   (void)recvScl;

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_SendSnapshot, 11,
      &wstream, &snapshotTime, &eventTime, &sender, &receiver, &comm,
      &tag, &length, &scl, &kvs, &do_write );

   // write snapshot record
   //
   if( do_write )
   {
      ret =
         OTF_WStream_writeSendSnapshotKV( wstream, snapshotTime, eventTime,
            sender, receiver, comm, tag, length, scl, kvs );
   }

   return ret;
}

int
HooksMsgMatchAndSnapsC::WriteOpenFileSnapshotCB( OTF_WStream * wstream,
   uint64_t snapshotTime, uint64_t eventTime, uint64_t proc, uint32_t file,
   uint64_t handleid, uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = 1;
   bool do_write = true;

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_OpenFileSnapshot, 9,
      &wstream, &snapshotTime, &eventTime, &file, &proc, &handleid, &scl,
      &kvs, &do_write );

   // write snapshot record
   //
   if( do_write )
   {
      ret =
         OTF_WStream_writeOpenFileSnapshotKV( wstream, snapshotTime, eventTime,
            file, proc, handleid, scl, kvs );
   }

   return ret;
}

int
HooksMsgMatchAndSnapsC::WriteBeginFileOpSnapshotCB( OTF_WStream * wstream,
   uint64_t snapshotTime, uint64_t eventTime, uint64_t proc, uint64_t matchid,
   uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = 1;
   bool do_write = true;

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_BeginFileOpSnapshot, 8,
      &wstream, &snapshotTime, &eventTime, &proc, &matchid, &scl, &kvs,
      &do_write );

   // write snapshot record
   //
   if( do_write )
   {
      ret =
         OTF_WStream_writeBeginFileOpSnapshotKV( wstream, snapshotTime,
            eventTime, proc, matchid, scl, kvs );
   }

   return ret;
}

int
HooksMsgMatchAndSnapsC::WriteBeginCollopSnapshotCB( OTF_WStream * wstream,
   uint64_t snapshotTime, uint64_t eventTime, uint64_t proc, uint32_t op,
   uint64_t matchid, uint32_t comm, uint32_t root, uint64_t sent,
   uint64_t recvd, uint32_t scl, OTF_KeyValueList * kvs )
{
   int ret = 1;
   bool do_write = true;

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_BeginCollOpSnapshot, 13,
      &wstream, &snapshotTime, &eventTime, &proc, &op, &matchid, &comm, &root,
      &sent, &recvd, &scl, &kvs, &do_write );

   // write snapshot record
   //
   if( do_write )
   {
      ret =
         OTF_WStream_writeBeginCollopSnapshotKV( wstream, snapshotTime,
            eventTime, proc, op, matchid, comm, root, sent, recvd, scl, kvs );
   }

   return ret;
}

int
HooksMsgMatchAndSnapsC::WriteCollopCountSnapshotCB( OTF_WStream * wstream,
   uint64_t snapshotTime, uint64_t proc, uint32_t comm, uint64_t count )
{
   int ret = 1;
   bool do_write = true;
   OTF_KeyValueList * kvs = 0;

   // trigger write record hook
   theHooks->triggerWriteRecordHook( HooksC::Record_CollOpCountSnapshot, 7,
      &wstream, &snapshotTime, &proc, &comm, &count, &kvs, &do_write );

   // write snapshot record
   //
   if( do_write )
   {
      ret =
         OTF_WStream_writeCollopCountSnapshot( wstream, snapshotTime,
            proc, comm, count, kvs );
   }

   return ret;
}

int
HooksMsgMatchAndSnapsC::WriteCounterSnapshotCB( OTF_WStream * wstream,
   uint64_t snapshotTime, uint64_t eventTime, uint64_t proc, uint32_t counter,
   uint64_t value, OTF_KeyValueList * kvs )
{
   int ret = 1;
   bool do_write = true;

   // trigger write record hook
    theHooks->triggerWriteRecordHook( HooksC::Record_CounterSnapshot, 8,
       &wstream, &snapshotTime, &eventTime, &proc, &counter, &value, &kvs,
       &do_write );

   // write snapshot record
   //
   if( do_write )
   {
      ret =
         OTF_WStream_writeCounterSnapshot( wstream, snapshotTime, eventTime,
            proc, counter, value, kvs );
   }

   return ret;
}

#ifdef VT_MPI

// user-defined reduction operation to aggregate per-rank message matching
// bumps statistics
void
HooksMsgMatchAndSnapsC::MsgMatchBumpsReduceOp( uint64_t * invec,
   uint64_t * inoutvec, VT_MPI_INT * len, MPI_Datatype * datatype )
{
   (void)len;
   (void)datatype;

   inoutvec[0] += invec[0]; // number of unmatched messages
   inoutvec[1] += invec[1]; // number of reversed messages
   inoutvec[2] += invec[2]; // total number of messages
}

#endif // VT_MPI

// vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

// initialization/finalization hooks
//

void
HooksMsgMatchAndSnapsC::initHook()
{
   // Empty
}

void
HooksMsgMatchAndSnapsC::finalizeHook( const bool & error )
{
   MASTER
   {
      if( !error && Params.domsgmatch && Params.verbose_level > 0 &&
          !m_msgMatchBumps.empty() )
      {
         // print warnings about message matching irregularities to stdout
         //

         VPrint( 1, "\n" );

         // print warning about
         // unmatched messages (i == 0) and reversed messages (i == 1)
         //
         for( uint8_t i = 0; i < 2; i++ )
         {
            uint64_t num;
            const char* fmt;

            if( i == 0 && m_msgMatchBumps.num_unmatched > 0 )
            {
               num = m_msgMatchBumps.num_unmatched;
               fmt = MsgMatchBumpsS::UNMATCHED_WARNING_FMT();
            }
            else if( i == 1 && m_msgMatchBumps.num_reversed > 0 )
            {
               num = m_msgMatchBumps.num_reversed;
               fmt = MsgMatchBumpsS::REVERSED_WARNING_FMT();
            }
            else
            {
               continue;
            }

            const double percent =
               ( (double)num / (double)m_msgMatchBumps.num_messages ) * 100.0;
            char percent_str[10];
            if( percent < 1.0 )
               strcpy( percent_str, "<1%" );
            else
               sprintf( percent_str, "%.1f%%", percent );

            VPrint( 1, fmt, (unsigned long long int)num, percent_str );
         }

         VPrint( 1, "\n" );
      }
   }
}

// phase hooks
//

void
HooksMsgMatchAndSnapsC::phaseHook_UnifyDefinitions_pre()
{
   // return, if message matching is disabled or the input trace has no events
   if( !Params.domsgmatch || !UnifyControlS::have_events() )
      return;

   MASTER
   {
      // create global key definitions for message matching
      //

      // get global token factory for DefKeyValue
      TokenFactoryScopeI * tkfac_defkeyval =
         theTokenFactory->getScope( DEF_REC_TYPE__DefKeyValue );

      DefRec_DefKeyValueS new_keyval;
      new_keyval.type = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_TYPE;
      new_keyval.name = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_NAME;
      m_msgMatchKeyTokens[KEY_TIME] = tkfac_defkeyval->create( &new_keyval );
      //new_keyval.type = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SIZE_TYPE;
      //new_keyval.name = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SIZE_NAME;
      //m_msgMatchKeyTokens[KEY_LENGTH] = tkfac_defkeyval->create( new_keyval );
      //new_keyval.type = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SCL_TYPE;
      //new_keyval.name = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SCL_NAME;
      //m_msgMatchKeyTokens[KEY_SCL] = tkfac_defkeyval->create( new_keyval );
   }
}

void
HooksMsgMatchAndSnapsC::phaseHook_UnifyEvents_pre()
{
   bool error = false;

#if defined(HAVE_OMP) && HAVE_OMP
   // update maximum number of threads to use for unifying events
   m_maxThreads = omp_get_max_threads();
#endif // HAVE_OMP

   // create stream contexts
   //
   for( uint32_t i = 0; i < MyStreamIds.size(); i++ )
   {
      const uint32_t streamid = MyStreamIds[i];
      StreamContextS * stream_context = m_streamId2StreamContext[streamid] =
         new StreamContextS( streamid );
      vt_assert( stream_context );
   }

   do
   {
      if( Params.domsgmatch )
      {
         VPrint( 2, " Preparing message matching\n" );

#ifdef VT_MPI
         if( NumRanks > 1 )
         {
            VPrint( 2, "  Sharing global key tokens for recv. message data\n" );

            // broadcast global key tokens for message matching to all ranks
            CALL_MPI( MPI_Bcast( m_msgMatchKeyTokens, KEY_NUM, MPI_UNSIGNED, 0,
                                 MPI_COMM_WORLD ) );
         }
#endif // VT_MPI

         // vector of receive messages
         LargeVectorC<RecvMsgS*> recv_msgs( 1000000 );

         // read receive messages
         //
         error = !getRecvMsgs( recv_msgs );
         if( SyncError( &error ) )
            break;

         // enqueue receive messages to sender stream contexts
         //
         error = !enqueueRecvMsgs( recv_msgs );
         if( SyncError( &error ) )
            break;
      }

      if( Params.createsnaps )
      {
         VPrint( 2, " Preparing snapshot generation\n" );

#ifdef VT_MPI
         if( NumRanks > 1 )
         {
            // broadcast maximum trace timestamp and snapshot interval time
            // to all ranks
            //

            uint64_t maxtime_and_snapintv[2];

            MASTER
            {
               maxtime_and_snapintv[0] = m_maxTime;
               maxtime_and_snapintv[1] = m_snapshotInterval;
            }


            CALL_MPI( MPI_Bcast( maxtime_and_snapintv, 2, MPI_LONG_LONG_INT, 0,
                                 MPI_COMM_WORLD ) );

            SLAVE
            {
               m_maxTime = maxtime_and_snapintv[0];
               m_snapshotInterval = maxtime_and_snapintv[1];
            }
         }
#endif // VT_MPI

         // maximum trace timestamp and snapshot interval time must be set
         // at this point
         //
         vt_assert( m_maxTime != (uint64_t)-1 );
         vt_assert( m_snapshotInterval > 0 );

         // set number of snapshots to generate
         //
         if( m_maxTime < Params.maxsnapshots )
            m_numSnapshots = m_maxTime;
         else
            m_numSnapshots = Params.maxsnapshots;

         // shrink thumbnail size, if necessary
         //
         if( m_maxTime < MAX_THUMBNAIL_WIDTH )
            m_thumbnailWidth = m_maxTime;
         if( NumAvailStreams < MAX_THUMBNAIL_HEIGHT )
            m_thumbnailHeight = NumAvailStreams;

         // select stream/process ids for the thumbnail
         //

         VPrint( 2, "  Selecting processes for thumbnail\n" );

         std::set<uint32_t> thumbnail_streams_ids;

         if( m_thumbnailHeight < NumAvailStreams )
         {
            uint32_t step = NumAvailStreams / m_thumbnailHeight;
            uint32_t i, j;

            for( i = j = 0;
                 i < UnifyCtls.size() &&
                 thumbnail_streams_ids.size() < m_thumbnailHeight;
                 i++ )
            {
               if( UnifyCtls[i]->stream_avail && ( j++ % step ) == 0 )
                  thumbnail_streams_ids.insert( UnifyCtls[i]->streamid );
            }
         }

         // initialize OTFAUX state contexts
         //
         for( uint32_t i = 0; i < MyStreamIds.size(); i++ )
         {
            const uint32_t streamid = MyStreamIds[i];
            StreamContextS * stream_context;
            int auxret;

            // get stream context by id
            //
            stream_context = getStreamContext( streamid );
            vt_assert( stream_context );

            // setup thumbnail
            //

            auxret =
               OTFAUX_State_setupThumbnail( stream_context->auxstate,
                  0, m_maxTime, m_thumbnailWidth );
            vt_assert( auxret );

            int is_thumbnail_stream = 0;
            if( thumbnail_streams_ids.empty() ||
                thumbnail_streams_ids.find( streamid ) !=
                   thumbnail_streams_ids.end() )
               is_thumbnail_stream = 1;

            auxret =
               OTFAUX_State_declareProcess( stream_context->auxstate,
                  streamid, is_thumbnail_stream );
            vt_assert( auxret );

            // register callback function for releasing snapshot event data
            // (i.e. key-values)
            //
            auxret =
               OTFAUX_State_setReleaseEventDataCallback(
                  stream_context->auxstate,
                  (OTFAUX_ReleaseEventData)ReleaseEventDataCB, 0 );
            vt_assert( auxret );

            // register callback functions for writing snapshots
            //

            auxret =
               OTFAUX_State_setWriteEnterSnapshotCallback(
                  stream_context->auxstate,
                  (OTFAUX_WriteEnterSnapshotCallback)WriteEnterSnapshotCB );
            vt_assert( auxret );

            if( Params.domsgmatch )
            {
               auxret =
                  OTFAUX_State_setWriteSendSnapshotCallback(
                     stream_context->auxstate,
                     (OTFAUX_WriteSendSnapshotCallback)WriteSendSnapshotCB );
               vt_assert( auxret );
            }

            auxret =
               OTFAUX_State_setWriteOpenFileSnapshotCallback(
                  stream_context->auxstate,
                  (OTFAUX_WriteOpenFileSnapshotCallback)
                     WriteOpenFileSnapshotCB );
            vt_assert( auxret );

            auxret =
               OTFAUX_State_setWriteBeginFileOpSnapshotCallback(
                  stream_context->auxstate,
                  (OTFAUX_WriteBeginFileOpSnapshotCallback)
                     WriteBeginFileOpSnapshotCB );
            vt_assert( auxret );

            auxret =
               OTFAUX_State_setWriteBeginCollopSnapshotCallback(
                  stream_context->auxstate,
                  (OTFAUX_WriteBeginCollopSnapshotCallback)
                     WriteBeginCollopSnapshotCB );
            vt_assert( auxret );

            auxret =
               OTFAUX_State_setWriteCollopCountSnapshotCallback(
                  stream_context->auxstate,
                  (OTFAUX_WriteCollopCountSnapshotCallback)
                     WriteCollopCountSnapshotCB );
            vt_assert( auxret );

            auxret =
               OTFAUX_State_setWriteCounterSnapshotCallback(
                  stream_context->auxstate,
                  (OTFAUX_WriteCounterSnapshotCallback)WriteCounterSnapshotCB );
            vt_assert( auxret );
         }
      }

   } while( false );

   if( !error )
      VPrint( 2, " Continuing unification of events\n" );

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::phaseHook_UnifyEvents_post()
{
   bool error = false;

   do
   {
      if( Params.domsgmatch )
      {
         VPrint( 2, " Post-processing message matching\n" );

         error = !processMsgMatchBumps();
         if( SyncError( &error ) )
            break;
      }

      if( Params.createsnaps )
      {
         VPrint( 2, " Post-processing snapshot generation\n" );

         error = !writeThumbnail();
         if( SyncError( &error ) )
            break;
      }

   } while( false );

   // delete all stream contexts
   //
   for( std::map<uint32_t, StreamContextS*>::const_iterator it =
        m_streamId2StreamContext.begin(); it != m_streamId2StreamContext.end();
        ++it )
   {
      delete it->second;
   }
   m_streamId2StreamContext.clear();

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::phaseHook_CleanUp_post()
{
   bool error = false;

   // return, if snapshot generation is disabled or the input trace has
   // no events
   if( !Params.createsnaps || !UnifyControlS::have_events() )
      return /* true */;

   // temporary output file prefix
   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   char filename1[STRBUFSIZE];
   char filename2[STRBUFSIZE];

#ifndef VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS

   int begin;
   int end;
   int step;
   int i;

   // base file type
   const OTF_FileType base_file_type = OTF_FILETYPE_SNAPS;

   // remove previous created snapshot output files
   //
   {
      // get begin, end, and step of loop removing files
      //
      begin = 0;
      end = (int)MyStreamIds.size();
      step = 1;
#if defined(HAVE_IOFSL) && HAVE_IOFSL
      if( Params.doiofsl() )
      {
         end = (int)Params.iofsl_num_servers;
#ifdef VT_MPI
         begin = MyRank;
         step = NumRanks;
#endif // VT_MPI
      }
#endif // HAVE_IOFSL

#if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp parallel for private(i, filename1)
#endif // HAVE_OMP
      for( i = begin; i < end; i+=step )
      {
         // (re)initialize file type
         OTF_FileType file_type = base_file_type;

         // iterate over compression types (compressed, uncompressed)
         for( uint8_t j = 0; j < 2; j++ )
         {
            // set compression bits of file type
            //
            if( j == 0 )
            {
               file_type &= ~OTF_FILECOMPRESSION_COMPRESSED;
               file_type |= OTF_FILECOMPRESSION_UNCOMPRESSED;
            }
            else // j == 1
            {
               file_type &= ~OTF_FILECOMPRESSION_UNCOMPRESSED;
               file_type |= OTF_FILECOMPRESSION_COMPRESSED;
            }

#if defined(HAVE_IOFSL) && HAVE_IOFSL
            if( Params.doiofsl() )
            {
               // iterate over IOFSL file types (all, idx)
               for( uint8_t k = 0; k < 2; k++ )
               {
                  // set IOFSL bits of file type
                  //
                  if( k == 0 )
                  {
                     file_type &= ~OTF_FILETYPE_IOFSL_IDX;
                     file_type |= OTF_FILETYPE_IOFSL_ALL;
                  }
                  else // k == 1
                  {
                     file_type &= ~OTF_FILETYPE_IOFSL_ALL;
                     file_type |= OTF_FILETYPE_IOFSL_IDX;
                  }

                  // get file name
                  OTF_getFilename( Params.out_file_prefix.c_str(), i, file_type,
                     STRBUFSIZE, filename1 );
                  // try to remove file
                  if( remove( filename1 ) == 0 )
                     PVPrint( 3, " Removed %s\n", filename1 );
               }
            }
            else
#endif // HAVE_IOFSL
            {
               // get file name
               OTF_getFilename( Params.out_file_prefix.c_str(), MyStreamIds[i],
                  file_type, STRBUFSIZE, filename1 );
               // try to remove file
               if( remove( filename1 ) == 0 )
                  PVPrint( 3, " Removed %s\n", filename1 );
            }
         }
      }
   }

   // rename temporary event/stat. output files
   //
   {
#if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp parallel for private(i, filename1, filename2)
#endif // HAVE_OMP
      for( i = begin; i < end; i+=step )
      {
         // (re)initialize file type
         OTF_FileType file_type =
            base_file_type | ( Params.docompress ?
               OTF_FILECOMPRESSION_COMPRESSED :
               OTF_FILECOMPRESSION_UNCOMPRESSED );

#if defined(HAVE_IOFSL) && HAVE_IOFSL
         if( Params.doiofsl() )
         {
            // iterate over IOFSL file types (all, idx)
            for( uint8_t k = 0; k < 2; k++ )
            {
               // set IOFSL bits of file type
               //
               if( k == 0 )
               {
                  file_type &= ~OTF_FILETYPE_IOFSL_IDX;
                  file_type |= OTF_FILETYPE_IOFSL_ALL;
               }
               else // k == 1
               {
                  file_type &= ~OTF_FILETYPE_IOFSL_ALL;
                  file_type |= OTF_FILETYPE_IOFSL_IDX;
               }

               // get old and new file name
               //
               OTF_getFilename( tmp_out_file_prefix.c_str(), i, file_type,
                  STRBUFSIZE, filename1 );
               OTF_getFilename( Params.out_file_prefix.c_str(), i, file_type,
                  STRBUFSIZE, filename2 );

               // try to rename file
               if( rename( filename1, filename2 ) == 0 )
                  PVPrint( 3, " Renamed %s to %s\n", filename1, filename2 );
            }
         }
         else
#endif // HAVE_IOFSL
         {
            // get old and new file name
            //
            OTF_getFilename( tmp_out_file_prefix.c_str(), MyStreamIds[i],
               file_type, STRBUFSIZE, filename1 );
            OTF_getFilename( Params.out_file_prefix.c_str(), MyStreamIds[i],
               file_type, STRBUFSIZE, filename2 );

            // try to rename file
            if( rename( filename1, filename2 ) == 0 )
               PVPrint( 3, " Renamed %s to %s\n", filename1, filename2 );
         }
      }
   }
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS

   // rename temporary thumbnail file
   //
   MASTER
   {
      // get old and new file name
      //
      snprintf( filename1, STRBUFSIZE-1, "%s%s",
                tmp_out_file_prefix.c_str(), THUMBNAIL_FILE_SUFFIX() );
      snprintf( filename2, STRBUFSIZE-1, "%s%s",
                Params.out_file_prefix.c_str(), THUMBNAIL_FILE_SUFFIX() );

      // rename file
      //
      if( rename( filename1, filename2 ) != 0 )
      {
         std::cerr << ExeName << ": Error: Could not rename "
                   << filename1 << " to " << filename2 << std::endl;
         error = true;
      }
      else
      {
         VPrint( 3, " Renamed %s to %s\n", filename1, filename2 );
      }
   }

#ifdef VT_MPI
   SyncError( &error );
#endif // VT_MPI

   //return !error;
   vt_assert( !error );
}

// record hooks
//

void
HooksMsgMatchAndSnapsC::writeRecHook_DefComment( HooksC::VaArgsT & args )
{
   // return, if message matching is disabled or the input trace has no events
   if( !Params.domsgmatch || !UnifyControlS::have_events() )
      return;

   // get hook arguments
   //

   //OTF_WStream ** wstream  = (OTF_WStream**)args[0];
   DefRec_DefCommentS::CommentTypeT * type =
      (DefRec_DefCommentS::CommentTypeT*)args[1];
   //std::string * comment   = (std::string*)args[2];
   bool *        do_write  = (bool*)args[3];

   // find the index of definition comments where the warning comments about
   // message matching irregularities will be inserted
   // (usually, index before the first user comment)
   //

   static bool stop_counting = false;

   if( *do_write && !stop_counting )
   {
      if( *type == DefRec_DefCommentS::TYPE_USER )
         stop_counting = true;
      else
         m_msgMatchBumps.def_comment_idx++;
   }
}

void
HooksMsgMatchAndSnapsC::writeRecHook_DefTimeRange( HooksC::VaArgsT & args )
{
   // return, if snapshot generation is disabled or the input trace has
   // no events
   if( !Params.createsnaps && !UnifyControlS::have_events() )
      return;

   // get hook arguments
   //

   //OTF_WStream ** wstream  = (OTF_WStream**)args[0];
   //uint64_t *     min_time = (uint64_t*)args[1];
   uint64_t *     max_time = (uint64_t*)args[2];
   bool *         do_write = (bool*)args[3];

   // set maximum trace timestamp for calculating snapshot interval time
   // and thumbnail width
   if( *do_write )
   {
      m_maxTime = *max_time;
      if( m_maxTime == 0 ) m_maxTime = 1;
   }
}

void
HooksMsgMatchAndSnapsC::writeRecHook_Enter( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          func     = (uint32_t*)args[2];
   uint32_t *          proc     = (uint32_t*)args[3];
   uint32_t *          scl      = (uint32_t*)args[4];
   OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[5];
   bool *              do_write = (bool*)args[6];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   if( !error && *do_write )
   {
      OTF_KeyValueList * snapshot_kvs = 0;
      if( OTF_KeyValueList_getCount( *kvs ) > 0 )
      {
         // clone key-values for snapshot record
         //
         snapshot_kvs = OTF_KeyValueList_clone( *kvs );
         vt_assert( snapshot_kvs );
      }

      // process record for snapshot and thumbnail generation
      //
      int auxret =
         OTFAUX_State_processEnter( stream_context->auxstate,
            *time, *proc, *func, *scl, snapshot_kvs );
      vt_assert( auxret );
   }

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_Leave( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          func     = (uint32_t*)args[2];
   uint32_t *          proc     = (uint32_t*)args[3];
   //uint32_t *          scl      = (uint32_t*)args[4];
   //OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[5];
   bool *              do_write = (bool*)args[6];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   if( !error && *do_write )
   {
      // process record for snapshot and thumbnail generation
      //
      int auxret =
         OTFAUX_State_processLeave( stream_context->auxstate, *time,
            *proc, *func );
      vt_assert( auxret );
   }

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_BeginFileOp( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   uint64_t *          matchid  = (uint64_t*)args[3];
   uint32_t *          scl      = (uint32_t*)args[4];
   OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[5];
   bool *              do_write = (bool*)args[6];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   if( !error && *do_write )
   {
      OTF_KeyValueList * snapshot_kvs = 0;
      if( OTF_KeyValueList_getCount( *kvs ) > 0 )
      {
         // clone key-values for snapshot record
         //
         snapshot_kvs = OTF_KeyValueList_clone( *kvs );
         vt_assert( snapshot_kvs );
      }

      // process record for snapshots
      //
      int auxret =
         OTFAUX_State_processBeginFileOperation( stream_context->auxstate,
            *time, *proc, *matchid, *scl, snapshot_kvs );
      vt_assert( auxret );
   }

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_EndFileOp( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   uint32_t *          file     = (uint32_t*)args[3];
   uint64_t *          matchid  = (uint64_t*)args[4];
   uint64_t *          handleid = (uint64_t*)args[5];
   uint32_t *          op       = (uint32_t*)args[6];
   //uint64_t *          bytes    = (uint64_t*)args[7];
   uint32_t *          scl      = (uint32_t*)args[8];
   OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[9];
   bool *              do_write = (bool*)args[10];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   if( !error && *do_write )
   {
      int auxret;

      // process record for snapshots
      //

      auxret =
         OTFAUX_State_processEndFileOperation( stream_context->auxstate,
            *time, *proc, *matchid );
      vt_assert( auxret );

      if( !( ( *op & OTF_IOFLAGS_BITS ) & OTF_IOFLAG_IOFAILED ) )
      {
         if( ( ( *op & OTF_FILEOP_BITS ) == OTF_FILEOP_OPEN ) )
         {
            OTF_KeyValueList * snapshot_kvs = 0;
            if( OTF_KeyValueList_getCount( *kvs ) > 0 )
            {
               // clone key-values for snapshot record
               //
               snapshot_kvs = OTF_KeyValueList_clone( *kvs );
               vt_assert( snapshot_kvs );
            }

            auxret =
               OTFAUX_State_processFileOpen( stream_context->auxstate,
                  *time, *proc, *file, *handleid, *scl, snapshot_kvs );
            vt_assert( auxret );
         }
         else if( ( ( *op & OTF_FILEOP_BITS ) == OTF_FILEOP_CLOSE ) )
         {
            //auxret =
               OTFAUX_State_processFileClose( stream_context->auxstate,
                  *time, *proc, *handleid );
            // ignore return value; the corresponding file open event might be
            // missing (i.e. filtered or located on another stream)
            //vt_assert( auxret );
         }
      }
   }

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_SendMsg( HooksC::VaArgsT & args )
{
   bool error = false;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          sender   = (uint32_t*)args[2];
   uint32_t *          receiver = (uint32_t*)args[3];
   uint32_t *          comm     = (uint32_t*)args[4];
   uint32_t *          tag      = (uint32_t*)args[5];
   uint32_t *          length   = (uint32_t*)args[6];
   uint32_t *          scl      = (uint32_t*)args[7];
   OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[8];
   bool *              do_write = (bool*)args[9];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *sender )
   {
      stream_context = getStreamContext( *sender );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   if( Params.createsnaps )
      error = !writeSnapshots( stream_context, *time, *wstream );

   if( !error && *do_write && Params.domsgmatch )
   {
      OTF_KeyValueList * snapshot_kvs = 0;
      if( Params.createsnaps )
      {
         // clone key-values for snapshot record
         //
         snapshot_kvs = OTF_KeyValueList_clone( *kvs );
         vt_assert( snapshot_kvs );
      }

      // process record for snapshots and/or message matching
      // (i.e. get timestamp, length, and scl of a matching receive message)
      //

      uint64_t recv_time;
      uint32_t recv_length;
      uint32_t recv_scl;

      int auxret =
         OTFAUX_State_processSendMsg( stream_context->auxstate,
            *time, *sender, *receiver, *comm, *tag, *length, *scl,
            &recv_time, &recv_length, &recv_scl, snapshot_kvs );
      vt_assert( auxret );

      // increment total number of messages
      stream_context->msgmatch_bumps.num_messages++;

      // message matched?
      if( auxret == 1 )
      {
         // corrupted event order (receive before send event)?
         if( *time > recv_time )
         {
            PVPrint( 3, "  Warning: Corrupted message event order "
               "[msg.: send time %llu recv. time %llu sender %u receiver %u "
               "comm %u tag %u length %u scl %u]\n",
               (unsigned long long int)*time, (unsigned long long int)recv_time,
               *sender, *receiver, *comm, *tag, *length, *scl );

            // increment number of reversed messages
            stream_context->msgmatch_bumps.num_reversed++;
         }

         // append receive timestamp, length, and scl to key-values
         //
         OTF_KeyValueList_appendUint64( *kvs, m_msgMatchKeyTokens[KEY_TIME],
            recv_time );
         //OTF_KeyValueList_appendUint32( *kvs, m_msgMatchKeyTokens[KEY_LENGTH],
         //   recv_length );
         //OTF_KeyValueList_appendUint32( *kvs, m_msgMatchKeyTokens[KEY_SCL],
         //   recv_scl );

         if( Params.createsnaps )
         {
            OTF_KeyValueList_appendUint64( snapshot_kvs,
               m_msgMatchKeyTokens[KEY_TIME], recv_time );
            //OTF_KeyValueList_appendUint32( snapshot_kvs,
            //   m_msgMatchKeyTokens[KEY_LENGTH], recv_length );
            //OTF_KeyValueList_appendUint32( snapshot_kvs,
            //   m_msgMatchKeyTokens[KEY_SCL], recv_scl );
         }
      }
      // message not matched?
      else // auxret == 2
      {
         PVPrint( 3, "  Warning: No matching message recv. event found "
            "[send msg.: time %llu sender %u receiver %u comm %u tag %u "
            "length %u scl %u]\n",
            (unsigned long long int)*time, *sender, *receiver, *comm, *tag,
            *length, *scl );

         // increment number of unmatched messages
         stream_context->msgmatch_bumps.num_unmatched++;
      }
   }

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_RecvMsg( HooksC::VaArgsT & args )
{
   bool error = false;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          receiver = (uint32_t*)args[2];
   //uint32_t *          sender   = (uint32_t*)args[3];
   //uint32_t *          comm     = (uint32_t*)args[4];
   //uint32_t *          tag      = (uint32_t*)args[5];
   //uint32_t *          length   = (uint32_t*)args[6];
   //uint32_t *          scl      = (uint32_t*)args[7];
   //OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[8];
   bool *              do_write = (bool*)args[9];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *receiver )
   {
      stream_context = getStreamContext( *receiver );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   if( Params.createsnaps )
      error = !writeSnapshots( stream_context, *time, *wstream );

   // drop message receive event, if desired
   if( !error && *do_write && Params.domsgmatch && Params.droprecvs )
      *do_write = false;

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_BeginCollOp( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   uint32_t *          op       = (uint32_t*)args[3];
   uint64_t *          matchid  = (uint64_t*)args[4];
   uint32_t *          comm     = (uint32_t*)args[5];
   uint32_t *          root     = (uint32_t*)args[6];
   uint64_t *          sent     = (uint64_t*)args[7];
   uint64_t *          recvd    = (uint64_t*)args[8];
   uint32_t *          scl      = (uint32_t*)args[9];
   OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[10];
   bool *              do_write = (bool*)args[11];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   // process record for snapshots
   //
   if( !error && *do_write )
   {
      OTF_KeyValueList * snapshot_kvs = 0;
      if( OTF_KeyValueList_getCount( *kvs ) > 0 )
      {
         // clone key-values for snapshot record
         //
         snapshot_kvs = OTF_KeyValueList_clone( *kvs );
         vt_assert( snapshot_kvs );
      }

      int auxret =
         OTFAUX_State_processBeginCollectiveOperation( stream_context->auxstate,
            *time, *proc, *comm, *root, *op, *matchid, *sent, *recvd,
            *scl, snapshot_kvs );
      vt_assert( auxret );
   }

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_EndCollOp( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   uint64_t *          matchid  = (uint64_t*)args[3];
   //OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[4];
   bool *              do_write = (bool*)args[5];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   // process record for snapshots
   //
   if( !error && *do_write )
   {
      int auxret =
         OTFAUX_State_processEndCollectiveOperation( stream_context->auxstate,
            *time, *proc, *matchid );
      vt_assert( auxret );
   }

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_RMAPut( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   //uint32_t *          origin   = (uint32_t*)args[3];
   //uint32_t *          dest     = (uint32_t*)args[4];
   //uint32_t *          comm     = (uint32_t*)args[5];
   //uint32_t *          tag      = (uint32_t*)args[6];
   //uint32_t *          scl      = (uint32_t*)args[7];
   //OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[8];
   //bool *              do_write = (bool*)args[9];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   //uint32_t *          origin   = (uint32_t*)args[3];
   //uint32_t *          dest     = (uint32_t*)args[4];
   //uint32_t *          comm     = (uint32_t*)args[5];
   //uint32_t *          tag      = (uint32_t*)args[6];
   //uint32_t *          scl      = (uint32_t*)args[7];
   //OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[8];
   //bool *              do_write = (bool*)args[9];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_RMAGet( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   //uint32_t *          origin   = (uint32_t*)args[3];
   //uint32_t *          dest     = (uint32_t*)args[4];
   //uint32_t *          comm     = (uint32_t*)args[5];
   //uint32_t *          tag      = (uint32_t*)args[6];
   //uint32_t *          scl      = (uint32_t*)args[7];
   //OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[8];
   //bool *              do_write = (bool*)args[9];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_RMAEnd( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   //uint32_t *          remote   = (uint32_t*)args[3];
   //uint32_t *          comm     = (uint32_t*)args[4];
   //uint32_t *          tag      = (uint32_t*)args[5];
   //uint32_t *          scl      = (uint32_t*)args[6];
   //OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[7];
   //bool *              do_write = (bool*)args[8];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_Counter( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   uint32_t *          procgrp  = (uint32_t*)args[3];
   uint32_t *          counter  = (uint32_t*)args[4];
   uint64_t *          value    = (uint64_t*)args[5];
   OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[6];
   bool *              do_write = (bool*)args[7];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   // process record for snapshots
   //
   if( !error && *do_write &&
       *procgrp == 0 /* TODO: consider group counters */ )
   {
      OTF_KeyValueList * snapshot_kvs = 0;
      if( OTF_KeyValueList_getCount( *kvs ) > 0 )
      {
         // clone key-values for snapshot record
         //
         snapshot_kvs = OTF_KeyValueList_clone( *kvs );
         vt_assert( snapshot_kvs );
      }

      int auxret =
         OTFAUX_State_processCounter( stream_context->auxstate, *time, *proc,
            *counter, *value, snapshot_kvs );
      vt_assert( auxret );
   }

   //return !error;
   vt_assert( !error );
}

void
HooksMsgMatchAndSnapsC::writeRecHook_EventComment( HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled
   if( !Params.createsnaps )
      return /* true */;

   // get hook arguments
   //

   OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          proc     = (uint32_t*)args[2];
   //std::string *       comment  = (std::string*)args[3];
   //OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[4];
   //bool *              do_write = (bool*)args[5];

   // get stream context by stream/process id, if necessary
   //
   static StreamContextS * stream_context = 0;
#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp threadprivate(stream_context)
#endif // HAVE_OMP
   if( !stream_context || stream_context->streamid != *proc )
   {
      stream_context = getStreamContext( *proc );
      vt_assert( stream_context );
   }

   // write outstanding snapshots
   error = !writeSnapshots( stream_context, *time, *wstream );

   //return !error;
   vt_assert( !error );
}

// generic hook
void
HooksMsgMatchAndSnapsC::genericHook( const uint32_t & id,
   HooksC::VaArgsT & args )
{
   bool error = false;

   // return, if snapshot generation is disabled or the input trace has
   // no events
   if( !Params.createsnaps || !UnifyControlS::have_events() )
      return /* true */;

   if( ( id & VT_UNIFY_HOOKS_MSGMATCH_SNAPS_GENID__DEF_WSTREAM_CLOSE ) != 0 )
   {
      // get OTF writer stream from hook arguments
      OTF_WStream ** wstream = (OTF_WStream**)args[0];

      // maximum trace timestamp must be set at this point
      vt_assert( m_maxTime != (uint64_t)-1 );

      // calculate snapshot interval time
      //
      if( m_maxTime <= Params.maxsnapshots )
         m_snapshotInterval = 1;
      else
         m_snapshotInterval =
            (uint64_t)( ( (double)m_maxTime /
                          (double)( Params.maxsnapshots + 1 ) ) + 0.5 );

      // write auxiliary sample point definition records
      //
      for( uint64_t time = m_snapshotInterval; time <= m_maxTime;
           time += m_snapshotInterval )
      {
         // TODO: trigger hook for writing this record

         error =
            ( OTF_WStream_writeDefAuxSamplePoint( *wstream, time,
#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS
                 OTF_AUX_SAMPLE_POINT_INLINE_SNAPSHOT,
#else // VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS
                 OTF_AUX_SAMPLE_POINT_SNAPSHOT,
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS
                 0 ) == 0 );
      }
   }
#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS
   else if(
      ( id & VT_UNIFY_HOOKS_MSGMATCH_SNAPS_GENID__EVENT_WSTREAM_OPEN ) != 0 )
   {
      // get hook arguments
      //
      OTF_WStream ** wstream = (OTF_WStream**)args[0];
      //uint32_t * stream_id = (uint32_t*)args[1];
      //std::string * stream_prefix = (std::string*)args[2];

      // set OTF output format to get inline snapshots
      OTF_WStream_setFormat( *wstream, OTF_WSTREAM_FORMAT_INLINE_SNAPSHOTS );
   }
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS
   else if(
      ( id & VT_UNIFY_HOOKS_MSGMATCH_SNAPS_GENID__EVENT_WSTREAM_CLOSE ) != 0 )
   {
      // get hook arguments
      //
      OTF_WStream ** wstream = (OTF_WStream**)args[0];
      uint32_t * streamid = (uint32_t*)args[1];

      // maximum trace timestamp must be set at this point
      vt_assert( m_maxTime != (uint64_t)-1 );

      // get stream context by id
      //
      StreamContextS * stream_context = getStreamContext( *streamid );
      vt_assert( stream_context );

      // write outstanding snapshots
      //
      error = !writeSnapshots( stream_context, m_maxTime, *wstream );
   }

   //return !error;
   vt_assert( !error );
}

// ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

bool
HooksMsgMatchAndSnapsC::getRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs )
{
   bool error = false;

   VPrint( 2, "  Reading receive messages\n" );

   // get input file prefix
   const std::string & in_file_prefix = Params.in_file_prefix;

   // create receive message vectors for each thread
   //
   LargeVectorC<RecvMsgS*> ** recv_msgs =
      new LargeVectorC<RecvMsgS*>*[m_maxThreads];
   vt_assert( recv_msgs );
   *recv_msgs = &recvMsgs;
   for( int i = 1; i < m_maxThreads; i++ )
   {
      recv_msgs[i] = new LargeVectorC<RecvMsgS*>( recvMsgs.chunkSize() );
      vt_assert( recv_msgs[i] );
   }

   int streams_num = (int)MyStreamIds.size();
   int i;

#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp parallel for private(i) shared(error)
#endif // HAVE_OMP
   for( i = 0; i < streams_num; i++ )
   {
#if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp flush(error)
      if( error ) continue;
      const int threadid = omp_get_thread_num();
#else // HAVE_OMP
      if( error ) break;
      const int threadid = 0;
#endif // HAVE_OMP

      const uint32_t & streamid = MyStreamIds[i];

      // open file manager
      //
      OTF_FileManager * manager = OTF_FileManager_open( 1 );
      vt_assert( manager );

      // initialize IOFSL stuff for reading, if necessary
      //
      if( UnifyControlS::is_iofsl() )
      {
         OTF_IofslMode otf_iofsl_mode =
            ( UnifyControlS::iofsl_mode == VT_IOFSL_MODE_MULTIFILE ) ?
               OTF_IOFSL_MULTIFILE : OTF_IOFSL_MULTIFILE_SPLIT;

         OTF_FileManager_setIofsl( manager, UnifyControlS::iofsl_num_servers,
            0, otf_iofsl_mode, 0, 0, VT_TRACEID_BITMASK );
      }

      // open stream for reading
      //
      OTF_RStream * rstream =
      OTF_RStream_open( in_file_prefix.c_str(), streamid, manager );
      vt_assert( rstream );

      PVPrint( 3, "   Opened OTF reader stream [namestub %s id %x]\n",
               in_file_prefix.c_str(), streamid );

      do
      {
         // try to get events buffer
         //
         if( !OTF_RStream_getEventBuffer( rstream ) )
         {
            PVPrint( 3, "    No events found in this OTF reader stream "
                        "- Ignored\n" );
            break;
         }

         // close events buffer
         OTF_RStream_closeEventBuffer( rstream );

         // create record handler array
         //
         OTF_HandlerArray * handler_array = OTF_HandlerArray_open();
         vt_assert( handler_array );

         // set record handler and its first argument for ...
         //

         // ... OTF_EVENTCOMMENT_RECORD
         OTF_HandlerArray_setHandler( handler_array,
            (OTF_FunctionPointer*)HandleEventComment, OTF_EVENTCOMMENT_RECORD );

         // ... OTF_RECEIVE_RECORD
         OTF_HandlerArray_setHandler( handler_array,
            (OTF_FunctionPointer*)HandleRecvMsg, OTF_RECEIVE_RECORD );
         OTF_HandlerArray_setFirstHandlerArg( handler_array,
            recv_msgs[threadid], OTF_RECEIVE_RECORD );

         // read events
         //
         if( OTF_RStream_readEvents( rstream, handler_array ) ==
             OTF_READ_ERROR )
         {
            std::cerr << ExeName << ": Error: "
                      << "Could not read events of OTF stream [namestub "
                      << in_file_prefix << " id "
                      << std::hex << streamid << "]"
                      << std::dec << std::endl;
            error = true;
         }

         // close record handler
         OTF_HandlerArray_close( handler_array );

      } while( false );

      // close reader stream
      OTF_RStream_close( rstream );
      // close file manager
      OTF_FileManager_close( manager );

      PVPrint( 3, "   Closed OTF reader stream [namestub %s id %x]\n",
               in_file_prefix.c_str(), streamid );

#ifdef VT_ETIMESYNC
      // reset time sync. parameters, if necessary
      if( !error && theTimeSync->getSyncMethod() == TimeSyncC::METHOD_ENHANCED )
         theTimeSync->resetSyncParam( streamid );
#endif // VT_ETIMESYNC
   }

   if( !SyncError( &error ) )
   {
      // merge thread's receive message vectors to one
      //
      for( int i = 1; i < m_maxThreads; i++ )
      {
         for( uint32_t j = 0; j < recv_msgs[i]->size(); j++ )
            recvMsgs.push_back( (*(recv_msgs[i]))[j] );
         recv_msgs[i]->clear();
         delete recv_msgs[i];
      }

#ifdef VT_MPI
      // distribute receive messages to ranks which handle the corresponding
      // sender streams, if necessary
      //
      if( NumRanks > 1 )
      {
         error = !distRecvMsgs( recvMsgs );
//         SyncError( &error );
      }
#endif // VT_MPI
   }

   delete [] recv_msgs;

   return !error;
}

#ifdef VT_MPI

bool
HooksMsgMatchAndSnapsC::distRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs )
{
   bool error = false;

   vt_assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   VPrint( 2, "  Distributing receive messages\n" );

   // get ranks which processes the corresponding sender streams
   //

   // map rank (processes the sender stream) <-> receive messages
   std::map<VT_MPI_INT, LargeVectorC<RecvMsgS*> > rank2recv_msgs;

   for( uint32_t i = 0; i < recvMsgs.size(); i++ )
   {
      // search rank which processes the sender stream
      //
      std::map<uint32_t, VT_MPI_INT>::const_iterator it =
         StreamId2Rank.find( recvMsgs[i]->sender );
      vt_assert( it != StreamId2Rank.end() );

      // add receive message to map
      rank2recv_msgs[it->second].push_back( recvMsgs[i] );
   }

   // create MPI datatype for RecvMsgS
   //

   MPI_Datatype recv_msg_type;

   {
      RecvMsgS recv_msg_struct;

      VT_MPI_INT bcounts[2] = { 1, 4/*+1(length) +1(scl)*/ };
      MPI_Aint bdispl[2];
      MPI_Datatype btypes[2] = { MPI_LONG_LONG_INT, MPI_UNSIGNED };

      CALL_MPI( MPI_Address( &(recv_msg_struct.time), &bdispl[0] ) );
      CALL_MPI( MPI_Address( &(recv_msg_struct.sender), &bdispl[1] ) );
      bdispl[1] = bdispl[1] - bdispl[0];
      bdispl[0] = 0;
      CALL_MPI( MPI_Type_struct( 2, bcounts, bdispl, btypes, &recv_msg_type ) );
      CALL_MPI( MPI_Type_commit( &recv_msg_type ) );
   }

   // allocate memory for the send buffer
   //
   RecvMsgS * sendbuf = new RecvMsgS[recvMsgs.size()];
   vt_assert( sendbuf );

   // get sendcounts, senddispls, and fill the send buffer
   //

   VT_MPI_INT * sendcounts = new VT_MPI_INT[NumRanks];
   vt_assert( sendcounts );
   VT_MPI_INT * senddispls = new VT_MPI_INT[NumRanks];
   vt_assert( senddispls );

   for( VT_MPI_INT rank = 0; rank < NumRanks; rank++ )
   {
      // initialize sendcounts
      sendcounts[rank] = 0;

      // compute senddispls
      //
      if( rank == 0 )
         senddispls[rank] = 0;
      else
         senddispls[rank] = senddispls[rank-1] + sendcounts[rank-1];

      // search for receive messages of certain rank
      std::map<VT_MPI_INT, LargeVectorC<RecvMsgS*> >::iterator it =
         rank2recv_msgs.find( rank );

      // found?
      if( it != rank2recv_msgs.end() )
      {
         // update sendcounts
         sendcounts[rank] = it->second.size();

         // add receive messages to send buffer
         //
         for( uint32_t i = 0; i < it->second.size(); i++ )
         {
            (sendbuf+senddispls[rank])[i] = *(it->second[i]);
            delete it->second[i];
         }

         // clear receive message vector
         it->second.clear();
      }
   }
   rank2recv_msgs.clear();
   recvMsgs.clear();

   // get receive counts
   //

   VT_MPI_INT * recvcounts = new VT_MPI_INT[NumRanks];
   vt_assert( recvcounts );

   CALL_MPI( MPI_Alltoall( sendcounts, 1, MPI_INT, recvcounts, 1, MPI_INT,
                           MPI_COMM_WORLD ) );

   // get receive displs
   //

   VT_MPI_INT * recvdispls = new VT_MPI_INT[NumRanks];
   vt_assert( recvdispls );

   recvdispls[0] = 0;
   for( VT_MPI_INT rank = 1; rank < NumRanks; rank++ )
      recvdispls[rank] = recvdispls[rank-1] + recvcounts[rank-1];

   // allocate receive buffer
   //
   RecvMsgS * recvbuf =
      new RecvMsgS[recvdispls[NumRanks-1] + recvcounts[NumRanks-1]];
   vt_assert( recvbuf );

   // distribute receive messages
   CALL_MPI( MPI_Alltoallv( sendbuf, sendcounts, senddispls,
                            recv_msg_type, recvbuf, recvcounts,
                            recvdispls, recv_msg_type, MPI_COMM_WORLD ) );

   // free send buffer, -counts, and -displs
   //
   delete [] sendbuf;
   delete [] sendcounts;
   delete [] senddispls;

   // extract receive messages from receive buffer and add these to vector
   //
   for( VT_MPI_INT rank = 0; rank < NumRanks; rank++ )
   {
      for( VT_MPI_INT i = 0; i < recvcounts[rank]; i++ )
      {
         recvMsgs.push_back(
            new RecvMsgS( (recvbuf + recvdispls[rank])[i] ) );
         vt_assert( recvMsgs.back() );
      }
   }

   // free receive buffer, -counts, and -displs
   //
   delete [] recvbuf;
   delete [] recvcounts;
   delete [] recvdispls;

   // free MPI datatype
   CALL_MPI( MPI_Type_free( &recv_msg_type ) );

   return !error;
}

#endif // VT_MPI

bool
HooksMsgMatchAndSnapsC::enqueueRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs )
{
   bool error = false;

   VPrint( 2, "  Enqueuing receive messages\n" );

   // return, if there are no receive messages to match
   if( recvMsgs.empty() )
      return true;

   // release unused memory every N enqueued receive message
   const uint32_t release_mem_interval = 100000;

#if defined(HAVE_OMP) && HAVE_OMP
   if( m_maxThreads > 1 )
   {
#     pragma omp parallel
      {
         const int streams_num = (int)MyStreamIds.size();
         int i;

         // get streams of interest for my thread
         //
         std::set<uint32_t> streams_of_interest;
#        pragma omp for nowait
         for( i = 0; i < streams_num; i++ )
            streams_of_interest.insert( MyStreamIds[i] );

         // iterate over all receive messages to enqueue
         for( uint32_t j = 0; j < recvMsgs.size(); j++ )
         {
            const RecvMsgS * recv_msg = recvMsgs[j];

            // enqueue receive message, if it's of interest for my thread
            //
            if( streams_of_interest.find( recv_msg->sender ) !=
                streams_of_interest.end() )
            {
               // get stream context of sender stream
               //
               const StreamContextS * stream_context =
                  getStreamContext( recv_msg->sender );
               vt_assert( stream_context );

               // enqueue receive message
               //
               int auxret =
                  OTFAUX_State_enqueueRecvMsg( stream_context->auxstate,
                     recv_msg->time, recv_msg->receiver, recv_msg->sender,
                     recv_msg->comm, recv_msg->tag,
                     0/*recv_msg->length*/, 0/*recv_msg->scl*/ );
               vt_assert( auxret );
            }

            // release memory of already enqueued receive messages
            //
            if( j > 0 && j % release_mem_interval == 0 )
            {
#              pragma omp barrier
#              pragma omp single
               {
                  for( uint32_t k = j - release_mem_interval; k < j; k++ )
                     delete recvMsgs[k];
               }
            }
         }
      }
   }
   else
#endif // HAVE_OMP
   {
      for( uint32_t i = 0; i < recvMsgs.size(); i++ )
      {
         const RecvMsgS * recv_msg = recvMsgs[i];

         // get stream context of sender stream
         //
         const StreamContextS * stream_context =
            getStreamContext( recv_msg->sender );
         vt_assert( stream_context );

         // enqueue receive message
         //
         int auxret =
            OTFAUX_State_enqueueRecvMsg( stream_context->auxstate,
               recv_msg->time, recv_msg->receiver, recv_msg->sender,
               recv_msg->comm, recv_msg->tag,
               0/*recv_msg->length*/, 0/*recv_msg->scl*/ );
         vt_assert( auxret );

         // release memory of already enqueued receive messages
         //
         if( i > 0 && i % release_mem_interval == 0 )
         {
            for( uint32_t j = i - release_mem_interval; j < i; j++ )
               delete recvMsgs[j];
         }
      }
   }

   // release remaining memory of enqueued receive messages
   //
   for( uint32_t i = ( recvMsgs.size() / release_mem_interval )
           * release_mem_interval; i < recvMsgs.size(); i++ )
      delete recvMsgs[i];
   recvMsgs.clear();

   return !error;
}

bool
HooksMsgMatchAndSnapsC::processMsgMatchBumps()
{
   bool error = false;

   // aggregate per-stream message matching bumps statistics
   //

   VPrint( 2, "  Aggregating message matching bumps statistics\n" );

   for( std::map<uint32_t, StreamContextS*>::const_iterator it =
        m_streamId2StreamContext.begin();
        it != m_streamId2StreamContext.end(); ++it )
   {
      m_msgMatchBumps += it->second->msgmatch_bumps;
   }

#ifdef VT_MPI
   if( NumRanks > 1 )
   {
      MPI_Op op;
      uint64_t sendbuf[3] = {
         m_msgMatchBumps.num_unmatched, m_msgMatchBumps.num_reversed,
         m_msgMatchBumps.num_messages
      };
      uint64_t recvbuf[3] = { 0, 0, 0 };

      CALL_MPI( MPI_Op_create( (MPI_User_function*)MsgMatchBumpsReduceOp,
                1, &op ) );

      CALL_MPI( MPI_Reduce( sendbuf, recvbuf, 3, MPI_LONG_LONG_INT, op, 0,
                MPI_COMM_WORLD ) );

      CALL_MPI( MPI_Op_free( &op ) );

      MASTER
      {
         m_msgMatchBumps.num_unmatched = recvbuf[0];
         m_msgMatchBumps.num_reversed = recvbuf[1];
         m_msgMatchBumps.num_messages = recvbuf[2];
      }
   }
#endif // VT_MPI

   MASTER
   {
      if( !m_msgMatchBumps.empty() )
      {
         // rewrite global definitions to insert warning comments about message
         // matching irregularities
         //

         VPrint( 2, "  Rewriting global definitions\n" );

         // index of definition comments where the warning comments will be
         // inserted must be set at this point
         vt_assert( m_msgMatchBumps.def_comment_idx > 0 );

         // get temporary input/output file prefix
         //
         const std::string tmp_in_file_prefix =
            Params.out_file_prefix + TmpFileSuffix;
         const std::string tmp_out_file_prefix =
            Params.out_file_prefix + TmpFileSuffix + '2';

         // open file manager for reader and writer stream
         //
         OTF_FileManager * manager = OTF_FileManager_open( 2 );
         vt_assert( manager );

         // open stream for reading
         //
         OTF_RStream * rstream =
            OTF_RStream_open( tmp_in_file_prefix.c_str(), 0, manager );
         vt_assert( rstream );

         PVPrint( 3, "   Opened OTF reader stream [namestub %s id 0]\n",
                  tmp_in_file_prefix.c_str() );

         // reader stream's definition buffer must exist
         //
         OTF_RBuffer * rbuffer = OTF_RStream_getDefBuffer( rstream );
         vt_assert( rbuffer );
         OTF_RStream_closeDefBuffer( rstream );

         // open stream for writing
         //
         OTF_WStream * wstream =
            OTF_WStream_open( tmp_out_file_prefix.c_str(), 0, manager );
         vt_assert( wstream );

         PVPrint( 3, "   Opened OTF writer stream [namestub %s id 0]\n",
                  tmp_out_file_prefix.c_str() );

         // set file compression
         //
         if( Params.docompress )
         {
            OTF_WStream_setCompression( wstream,
               OTF_FILECOMPRESSION_COMPRESSED );
         }

         // create record handler array
         //
         OTF_HandlerArray * handler_array = OTF_HandlerArray_open();
         vt_assert( handler_array );

         // set record handlers
         //
         OTF_HandlerArray_getCopyHandler_stream( handler_array, wstream );
         OTF_HandlerArray_setHandler( handler_array,
            (OTF_FunctionPointer*)HandleDefComment,
            OTF_DEFINITIONCOMMENT_RECORD );
         OTF_HandlerArray_setFirstHandlerArg( handler_array,
            &m_msgMatchBumps, OTF_DEFINITIONCOMMENT_RECORD );
         m_msgMatchBumps.wstream = wstream;

         // rewrite global definitions
         //
         if( OTF_RStream_readDefinitions( rstream, handler_array ) ==
            OTF_READ_ERROR )
         {
            std::cerr << ExeName << ": Error: "
                      << "Could not rewrite global definitions" << std::endl;
            error = true;
         }

         // close record handler
         OTF_HandlerArray_close( handler_array );

         // close writer stream
         OTF_WStream_close( wstream );

         PVPrint( 3, "   Closed OTF writer stream [namestub %s id 0]\n",
                  tmp_out_file_prefix.c_str() );

         // close reader stream
         OTF_RStream_close( rstream );

         PVPrint( 3, "   Closed OTF reader stream [namestub %s id 0]\n",
                  tmp_in_file_prefix.c_str() );

         // close file manager for reader and writer stream
         OTF_FileManager_close( manager );

         if( !error )
         {
            // rename temporary output file
            //

            char filename1[STRBUFSIZE];
            char filename2[STRBUFSIZE];

            // initialize file type
            OTF_FileType file_type =
               OTF_FILETYPE_DEF | ( Params.docompress ?
                  OTF_FILECOMPRESSION_COMPRESSED :
                  OTF_FILECOMPRESSION_UNCOMPRESSED );

            // get old and new file name
            //
            OTF_getFilename( tmp_out_file_prefix.c_str(), 0, file_type,
               STRBUFSIZE, filename1 );
            OTF_getFilename( tmp_in_file_prefix.c_str(), 0, file_type,
               STRBUFSIZE, filename2 );

            // rename file
            //
            if( rename( filename1, filename2 ) != 0 )
            {
               std::cerr << ExeName << ": Error: Could not rename "
                         << filename1 << " to " << filename2 << std::endl;
               error = true;
            }
            else
            {
               VPrint( 3, "   Renamed %s to %s\n", filename1, filename2 );
            }
         }
      }
   }

   return !error;
}

bool
HooksMsgMatchAndSnapsC::writeThumbnail()
{
   bool error = false;

   VPrint( 2, "  Writing thumbnail\n" );

   // get temporary output file prefix
   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

#ifdef VT_MPI
   for( int rank = 0; rank < NumRanks; rank++ )
   {
   // my rank's turn?
   if( rank == MyRank )
   {
#endif // VT_MPI

   // iterate over all stream contexts
   for( std::map<uint32_t, StreamContextS*>::const_iterator it =
        m_streamId2StreamContext.begin();
        it != m_streamId2StreamContext.end() && !error; ++it )
   {
      const StreamContextS * stream_context = it->second;
      int create_file;
      int auxret;

      // the first "writer" has to create the thumbnail file; the other ones
      // appends its data to it
      //
#ifdef VT_MPI
      create_file =
         ( rank == 0 && it == m_streamId2StreamContext.begin() ) ? 1 : 0;
#else // VT_MPI
      create_file = ( it == m_streamId2StreamContext.begin() ) ? 1 : 0;
#endif // VT_MPI

      // write thumbnail data to file
      //
      auxret =
         OTFAUX_State_writeThumbnail( stream_context->auxstate,
            tmp_out_file_prefix.c_str(), create_file, m_thumbnailHeight );
      if( !auxret )
      {
         std::cerr << ExeName << ": Error: "
                   << "Could not write thumbnail file "
                   << tmp_out_file_prefix << THUMBNAIL_FILE_SUFFIX()
                   << std::endl;
         error = true;
      }
   }
#ifdef VT_MPI
   }
   if( SyncError( &error ) )
      break;

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );
   }
#endif // VT_MPI

   return !error;
}

bool
HooksMsgMatchAndSnapsC::writeSnapshots( StreamContextS * streamContext,
   const uint64_t time, OTF_WStream * wstream ) const
{
   bool error = false;

   // get next timestamp where should be a snapshot
   uint64_t next_snapshot_time =
      streamContext->last_snapshot_time + m_snapshotInterval;

   // write outstanding snapshots until given timestamp but not at the very end
   // of the trace
   //
   while( next_snapshot_time <= time && next_snapshot_time < m_maxTime &&
          streamContext->snapshot_cnt < m_numSnapshots )
   {
      PVPrint( 3, "  Writing snapshot to OTF writer stream "
                  "[namestub %s id %x time %llu]\n",
                  Params.in_file_prefix.c_str(), streamContext->streamid,
                  (unsigned long long int)next_snapshot_time );

      // write snapshot
      //
      int auxret =
         OTFAUX_State_writeSnapshot( streamContext->auxstate,
            next_snapshot_time, wstream );
      if( !auxret )
      {
         std::cerr << ExeName << ": Error: "
                   << "Could not write snapshot to OTF writer stream [namestub "
                   << Params.in_file_prefix << " id "
                   << std::hex << streamContext->streamid << "]"
                   << std::dec << std::endl;
         error = true;
         break;
      }

      // increment snapshot counter and update last/next snapshot timestamp
      //
      streamContext->snapshot_cnt++;
      streamContext->last_snapshot_time = next_snapshot_time;
      next_snapshot_time += m_snapshotInterval;
   }

   return !error;
}

HooksMsgMatchAndSnapsC::StreamContextS *
HooksMsgMatchAndSnapsC::getStreamContext( const uint32_t streamid ) const
{
   std::map<uint32_t, StreamContextS*>::const_iterator it =
      m_streamId2StreamContext.find( streamid );

   if( it != m_streamId2StreamContext.end() )
      return it->second;
   else
      return 0;
}
