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

#include "vt_unify_events_stats.h"
#include "vt_unify_handlers.h"
#include "vt_unify_hooks.h"

#include "otf.h"

#include <iostream>

// instances of class EventsAndStatsC
//
EventsAndStatsC * theEvents = 0;
EventsAndStatsC * theStatistics = 0;

//////////////////// class EventsAndStatsC ////////////////////

// public methods
//

EventsAndStatsC::EventsAndStatsC( const ScopeTypeT & scope )
   : m_scope( scope )
{
   // Empty
}

EventsAndStatsC::~EventsAndStatsC()
{
   // Empty
}

bool
EventsAndStatsC::run()
{
   bool error = false;

#ifdef VT_MPI
   // block until all ranks have reached this point
   if( NumRanks > 1 )
      CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );
#endif // VT_MPI

   if( m_scope == SCOPE_EVENTS )
   {
      VPrint( 1, "Unifying events\n" );

      // trigger phase pre hook
      theHooks->triggerPhaseHook( HooksC::Phase_UnifyEvents_pre );
   }
   else // m_scope == SCOPE_STATS
   {
      VPrint( 1, "Unifying statistics\n" );

      // trigger phase pre hook
      theHooks->triggerPhaseHook( HooksC::Phase_UnifyStatistics_pre );
   }

   // rewrite events/statistics
   //
   error = !rewrite();
   SyncError( &error );

   // show an error message, if necessary
   //
   MASTER
   {
      if( error )
      {
         std::cerr << ExeName << ": "
                   << "An error occurred during unifying "
                   << ( m_scope == SCOPE_EVENTS ? "events. " : "statistics. " )
                   << "Aborting." << std::endl;
      }
   }

   // trigger phase post hook, if no error occurred
   //
   if( !error )
   {
      theHooks->triggerPhaseHook( m_scope == SCOPE_EVENTS ?
         HooksC::Phase_UnifyEvents_post : HooksC::Phase_UnifyStatistics_post );
   }

   return !error;
}

bool
EventsAndStatsC::cleanUp()
{
   bool error = false;

   char filename1[STRBUFSIZE];
   char filename2[STRBUFSIZE];

   int begin;
   int end;
   int step;
   int i;

   // base file type
   const OTF_FileType base_file_type =
      ( m_scope == SCOPE_EVENTS ) ? OTF_FILETYPE_EVENT : OTF_FILETYPE_STATS;

   // temporary output file prefix
   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // remove local event/stat. files, if necessary
   //
   if( Params.doclean )
   {
      // get begin, end, and step of loop removing files
      //
      begin = 0;
      end = (int)MyStreamIds.size();
      step = 1;
      if( UnifyControlS::is_iofsl() )
      {
         end = (int)UnifyControlS::iofsl_num_servers;
#ifdef VT_MPI
         begin = MyRank;
         step = NumRanks;
#endif // VT_MPI
      }

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

            if( UnifyControlS::is_iofsl() )
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
                  OTF_getFilename( Params.in_file_prefix.c_str(), i,
                     file_type, STRBUFSIZE, filename1 );
                  // try to remove file
                  if( remove( filename1 ) == 0 )
                     PVPrint( 3, " Removed %s\n", filename1 );
               }
            }
            else
            {
               // get file name
               OTF_getFilename( Params.in_file_prefix.c_str(), MyStreamIds[i],
                  file_type, STRBUFSIZE, filename1 );
               // try to remove file
               if( remove( filename1 ) == 0 )
                  PVPrint( 3, " Removed %s\n", filename1 );
            }
         }
      }
   }

   // remove previous created event/stat. output files
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

   return !error;
}

// private methods
//

bool
EventsAndStatsC::rewrite()
{
   bool error = false;

   // get input file prefix
   const std::string & in_file_prefix = Params.in_file_prefix;

   // get temporary output file prefix
   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

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
#else // HAVE_OMP
      if( error ) break;
#endif // HAVE_OMP

      const uint32_t & streamid = MyStreamIds[i];

      // open file manager for reading
      //
      OTF_FileManager * rmanager = OTF_FileManager_open( 1 );
      vt_assert( rmanager );

      // initialize IOFSL stuff for reading, if necessary
      //
      if( UnifyControlS::is_iofsl() )
      {
         OTF_IofslMode otf_iofsl_mode =
            ( UnifyControlS::iofsl_mode == VT_IOFSL_MODE_MULTIFILE ) ?
               OTF_IOFSL_MULTIFILE : OTF_IOFSL_MULTIFILE_SPLIT;

         OTF_FileManager_setIofsl( rmanager, UnifyControlS::iofsl_num_servers,
            0, otf_iofsl_mode, 0, 0, VT_TRACEID_BITMASK );
      }

      // open stream for reading
      //
      OTF_RStream * rstream =
      OTF_RStream_open( in_file_prefix.c_str(), streamid, rmanager );
      vt_assert( rstream );

      PVPrint( 3, " Opened OTF reader stream [namestub %s id %x]\n",
               in_file_prefix.c_str(), streamid );

      do
      {
         // try to get events/statistics buffer
         //
         if( m_scope == SCOPE_EVENTS )
         {
            if( !OTF_RStream_getEventBuffer( rstream ) )
            {
               PVPrint( 3, "  No events found in this OTF reader stream "
                           "- Ignored\n" );
               break;
            }

            // close events buffer
            OTF_RStream_closeEventBuffer( rstream );
         }
         else // m_scope == SCOPE_STATS
         {
            if( !OTF_RStream_getStatsBuffer( rstream ) )
            {
               PVPrint( 3, "  No statistics found in this OTF reader stream "
                           "- Ignored\n" );
               break;
            }

            // close statistics buffer
            OTF_RStream_closeStatsBuffer( rstream );
         }

         // open file manager for writing
         //
         OTF_FileManager * wmanager =
            OTF_FileManager_open( 2 /* 1xevent + 1xsnaps */ );
         vt_assert( wmanager );

#if defined(HAVE_IOFSL) && HAVE_IOFSL
         // initialize IOFSL stuff for writing, if necessary
         //
         if( Params.doiofsl() )
         {
            OTF_IofslMode otf_iofsl_mode =
               ( Params.iofsl_mode == VT_IOFSL_MODE_MULTIFILE ) ?
                  OTF_IOFSL_MULTIFILE : OTF_IOFSL_MULTIFILE_SPLIT;

            uint32_t otf_iofsl_flags = 0;
            if( ( Params.iofsl_flags & VT_IOFSL_FLAG_ASYNC_IO ) != 0 )
               otf_iofsl_flags |= OTF_IOFSL_FLAG_NONBLOCKING;

            OTF_FileManager_setIofsl( wmanager, Params.iofsl_num_servers,
               Params.iofsl_servers, otf_iofsl_mode, otf_iofsl_flags, 0,
               VT_TRACEID_BITMASK );
         }
#endif // HAVE_IOFSL

         // open stream for writing
         //
         OTF_WStream * wstream =
         OTF_WStream_open( tmp_out_file_prefix.c_str(), streamid, wmanager );
         vt_assert( wstream );

         PVPrint( 3, " Opened OTF writer stream [namestub %s id %x]\n",
                  tmp_out_file_prefix.c_str(), streamid );

#if (defined(VT_UNIFY_HOOKS_AEVENTS) || defined(VT_UNIFY_HOOKS_MARGINS) || \
     defined(VT_UNIFY_HOOKS_MSGMATCH_SNAPS))
         if( m_scope == SCOPE_EVENTS )
         {
            // trigger generic hooks for opened event writer stream
            theHooks->triggerGenericHook(
               VT_UNIFY_HOOKS_AEVENTS_GENID__EVENT_WSTREAM_OPEN |
               VT_UNIFY_HOOKS_MARGINS_GENID__EVENT_WSTREAM_OPEN |
               VT_UNIFY_HOOKS_MSGMATCH_SNAPS_GENID__EVENT_WSTREAM_OPEN, 3,
               &wstream, const_cast<uint32_t*>( &streamid ),
               const_cast<std::string*>( &in_file_prefix ) );
         }
#endif // VT_UNIFY_HOOKS_AEVENTS || VT_UNIFY_HOOKS_MARGINS ||
       // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

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

         if( m_scope == SCOPE_EVENTS )
         {
            // create first handler argument
            FirstHandlerArg_EventsS fha( wstream );

            // set record handler and its first argument for ...
            //

            // ... OTF_EVENTCOMMENT_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleEventComment,
               OTF_EVENTCOMMENT_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_EVENTCOMMENT_RECORD );

            // ... OTF_ENTER_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleEnter,
               OTF_ENTER_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_ENTER_RECORD );

            // ... OTF_LEAVE_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleLeave,
               OTF_LEAVE_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_LEAVE_RECORD );

            // ... OTF_COUNTER_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleCounter,
               OTF_COUNTER_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_COUNTER_RECORD );

            // ... OTF_BEGINFILEOPERATION_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleBeginFileOp,
               OTF_BEGINFILEOP_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_BEGINFILEOP_RECORD );

            // ... OTF_ENDFILEOPERATION_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleEndFileOp,
               OTF_ENDFILEOP_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_ENDFILEOP_RECORD );

            // ... OTF_SEND_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleSendMsg,
               OTF_SEND_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_SEND_RECORD );

            // ... OTF_RECEIVE_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRecvMsg,
               OTF_RECEIVE_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RECEIVE_RECORD );

            // ... OTF_BEGINCOLLOP_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleBeginCollOp,
               OTF_BEGINCOLLOP_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_BEGINCOLLOP_RECORD );

            // ... OTF_ENDCOLLOP_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleEndCollOp,
               OTF_ENDCOLLOP_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_ENDCOLLOP_RECORD );

            // ... OTF_RMAPUT_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRMAPut,
               OTF_RMAPUT_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RMAPUT_RECORD );

            // ... OTF_RMAPUTRE_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRMAPutRemoteEnd,
               OTF_RMAPUTRE_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RMAPUTRE_RECORD );

            // ... OTF_RMAGET_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRMAGet,
               OTF_RMAGET_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RMAGET_RECORD );

            // ... OTF_RMAEND_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRMAEnd,
               OTF_RMAEND_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RMAEND_RECORD );

            // rewrite events
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
         }
         else // m_scope == SCOPE_STATS
         {
            // create first handler argument
            FirstHandlerArg_StatsS fha( wstream );

            // set record handler and its first argument for ...
            //

            // ... OTF_FUNCTIONSUMMARY_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleFunctionSummary,
               OTF_FUNCTIONSUMMARY_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_FUNCTIONSUMMARY_RECORD );

            // ... OTF_MESSAGESUMMARY_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleMessageSummary,
               OTF_MESSAGESUMMARY_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_MESSAGESUMMARY_RECORD );

            // ... OTF_COLLOPSUMMARY_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleCollOpSummary,
               OTF_COLLOPSUMMARY_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_COLLOPSUMMARY_RECORD );

            // ... OTF_FILEOPERATIONSUMMARY_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleFileOpSummary,
               OTF_FILEOPERATIONSUMMARY_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_FILEOPERATIONSUMMARY_RECORD );

            // rewrite statistics
            //
            if( OTF_RStream_readStatistics( rstream, handler_array ) ==
                OTF_READ_ERROR )
            {
               std::cerr << ExeName << ": Error: "
                         << "Could not read statistics of OTF stream [namestub "
                         << in_file_prefix << " id "
                         << std::hex << streamid << "]"
                         << std::dec << std::endl;
               error = true;
            }
         }

#if (defined(VT_UNIFY_HOOKS_AEVENTS) || defined(VT_UNIFY_HOOKS_MARGINS) || \
     defined(VT_UNIFY_HOOKS_MSGMATCH_SNAPS))
         if( !error && m_scope == SCOPE_EVENTS )
         {
            // trigger generic hooks for closing event writer stream
            theHooks->triggerGenericHook(
               VT_UNIFY_HOOKS_AEVENTS_GENID__EVENT_WSTREAM_CLOSE |
               VT_UNIFY_HOOKS_MARGINS_GENID__EVENT_WSTREAM_CLOSE |
               VT_UNIFY_HOOKS_MSGMATCH_SNAPS_GENID__EVENT_WSTREAM_CLOSE, 2,
               &wstream, const_cast<uint32_t*>( &streamid ) );
         }
#endif // VT_UNIFY_HOOKS_AEVENTS || VT_UNIFY_HOOKS_MARGINS ||
       // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

         // close writer stream
         OTF_WStream_close( wstream );
         // close file manager for writing
         OTF_FileManager_close( wmanager );

         PVPrint( 3, " Closed OTF writer stream [namestub %s id %x]\n",
               tmp_out_file_prefix.c_str(), streamid );

         // close record handler
         OTF_HandlerArray_close( handler_array );

      } while( false );

      // close reader stream
      OTF_RStream_close( rstream );
      // close file manager for reading
      OTF_FileManager_close( rmanager );

      PVPrint( 3, " Closed OTF reader stream [namestub %s id %x]\n",
               in_file_prefix.c_str(), streamid );
   }

   return !error;
}
