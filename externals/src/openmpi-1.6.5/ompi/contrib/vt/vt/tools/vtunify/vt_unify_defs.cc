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
#include "vt_unify_sync.h"
#include "vt_unify_tkfac.h"
#include "vt_unify_usrcom.h"

#include "otf.h"

#include <iostream>
#include <sstream>

#include <stdio.h>
#include <string.h>
#include <time.h>

// function for resorting global definitions based on T::SortS
template <class T>
static void resortGlobDefs( const std::set<T> & in,
   std::set<const T*, typename T::SortS> & out )
{
   for( typename std::set<T>::const_iterator it =
        in.begin(); it != in.end(); ++it )
   {
      out.insert( &(*it) );
   }
}

DefinitionsC * theDefinitions = 0; // instance of class DefinitionsC

//////////////////// class DefinitionsC ////////////////////

// public methods
//

DefinitionsC::DefinitionsC()
{
   // create token factory scopes for def. record type ...
   //

   vt_assert( theTokenFactory );

   // ... DEF_REC_TYPE__DefProcessGroup
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefProcessGroup,
      new TokenFactoryScopeC<DefRec_DefProcessGroupS>
         ( &(m_globDefs.procGrps), 1000000000 ) );

   // ... DEF_REC_TYPE__DefSclFile
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefSclFile,
      new TokenFactoryScopeC<DefRec_DefSclFileS>
         ( &(m_globDefs.sclFiles) ) );

   // ... DEF_REC_TYPE__DefScl
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefScl,
      new TokenFactoryScopeC<DefRec_DefSclS>( &(m_globDefs.scls) ) );

   // ... DEF_REC_TYPE__DefFileGroup
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefFileGroup,
      new TokenFactoryScopeC<DefRec_DefFileGroupS>
         ( &(m_globDefs.fileGrps) ) );

   // ... DEF_REC_TYPE__DefFile
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefFile,
      new TokenFactoryScopeC<DefRec_DefFileS>( &(m_globDefs.files) ) );

   // ... DEF_REC_TYPE__DefFunctionGroup
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefFunctionGroup,
      new TokenFactoryScopeC<DefRec_DefFunctionGroupS>
         ( &(m_globDefs.funcGrps) ) );

   // ... DEF_REC_TYPE__DefFunction
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefFunction,
      new TokenFactoryScopeC<DefRec_DefFunctionS>
         ( &(m_globDefs.funcs) ) );

   // ... DEF_REC_TYPE__DefCollOp
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefCollOp,
      new TokenFactoryScopeC<DefRec_DefCollOpS>
         ( &(m_globDefs.collops) ) );

   // ... DEF_REC_TYPE__DefCounterGroup
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefCounterGroup,
      new TokenFactoryScopeC<DefRec_DefCounterGroupS>
         ( &(m_globDefs.cntrGrps) ) );

   // ... DEF_REC_TYPE__DefCounter
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefCounter,
      new TokenFactoryScopeC<DefRec_DefCounterS>
         ( &(m_globDefs.cntrs) ) );

   // ... DEF_REC_TYPE__DefKeyValue
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefKeyValue,
      new TokenFactoryScopeC<DefRec_DefKeyValueS>
         ( &(m_globDefs.keyVals) ) );

   // create instance of sub-class GroupCountersC
   //
   m_groupCntrs = new GroupCountersC( *this );
   vt_assert( m_groupCntrs );

   // create instance of sub-class CommentsC
   //
   m_comments = new CommentsC( *this );
   vt_assert( m_comments );

   // create instance of sub-class ProcessGroupsC
   //
   m_procGrps = new ProcessGroupsC( *this );
   vt_assert( m_procGrps );
}

DefinitionsC::~DefinitionsC()
{
   // delete instance of sub-class GroupCountersC
   delete m_groupCntrs;

   // delete instance of sub-class CommentsC
   delete m_comments;

   // delete instance of sub-class ProcessGroupsC
   delete m_procGrps;

   // delete token factory scopes of def. record types ...
   //

   vt_assert( theTokenFactory );

   // ... DEF_REC_TYPE__DefProcessGroup
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefProcessGroup );

   // ... DEF_REC_TYPE__DefSclFile
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefSclFile );

   // ... DEF_REC_TYPE__DefScl
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefScl );

   // ... DEF_REC_TYPE__DefFileGroup
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefFileGroup );

   // ... DEF_REC_TYPE__DefFile
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefFile );

   // ... DEF_REC_TYPE__DefFunctionGroup
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefFunctionGroup );

   // ... DEF_REC_TYPE__DefFunction
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefFunction );

   // ... DEF_REC_TYPE__DefCollOp
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefCollOp );

   // ... DEF_REC_TYPE__DefCounterGroup
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefCounterGroup );

   // ... DEF_REC_TYPE__DefCounter
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefCounter );

   // ... DEF_REC_TYPE__DefKeyValue
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefKeyValue );
}

bool
DefinitionsC::run()
{
   bool error = false;

#ifdef VT_MPI
   // block until all ranks have reached this point
   if( NumRanks > 1 )
      CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );
#endif // VT_MPI

   VPrint( 1, "Unifying definitions\n" );

   // trigger phase pre hook
   theHooks->triggerPhaseHook( HooksC::Phase_UnifyDefinitions_pre );

   do
   {
      // read local definitions
      //
      error = !readLocal();
      if( SyncError( &error ) )
         break;

      // all local time ranges are known at this point;
      // initialize time synchronization
      //
      theTimeSync->initialize();
      if( SyncError( &error ) )
         break;

      MASTER
      {
         do
         {
            // finish global process group definitions
            if( (error = !m_procGrps->finish()) )
               break;

            // finish global definition comments
            if( (error = !m_comments->finish()) )
               break;

            // set content of global time range definition record
            //
            TimeSyncC::TimeRangeT time_range = theTimeSync->getTimeRange();
            m_globDefs.timerange.minTime = time_range.first;
            m_globDefs.timerange.maxTime = time_range.second;

            // write global definitions
            if( (error = !writeGlobal()) )
               break;

         } while( false );
      }
      SyncError( &error );

   } while( false );

   // show an error message, if necessary
   //
   MASTER
   {
      if( error )
      {
         std::cerr << ExeName << ": "
                   << "An error occurred during unifying definitions. Aborting."
                   << std::endl;
      }
   }

   // trigger phase post hook, if no error occurred
   //
   if( !error )
      theHooks->triggerPhaseHook( HooksC::Phase_UnifyDefinitions_post );

   return !error;
}

bool
DefinitionsC::cleanUp()
{
   bool error = false;

   char filename1[STRBUFSIZE];
   char filename2[STRBUFSIZE];

   // base file type
   const OTF_FileType base_file_type = OTF_FILETYPE_DEF;

   // temporary output file prefix
   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // remove local files, if necessary
   //
   if( Params.doclean )
   {
      int begin;
      int end;
      int step;
      int i;

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

   MASTER
   {
      // remove previous created output file
      //
      {
         // initialize file type
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

            // NOTE: IOFSL handling not needed here, global definitions files
            // are not handled by IOFSL

            // get file name
            OTF_getFilename( Params.out_file_prefix.c_str(), 0, file_type,
               STRBUFSIZE, filename1 );
            // try to remove file
            if( remove( filename1 ) == 0 )
               PVPrint( 3, " Removed %s\n", filename1 );
         }
      }

      // rename temporary output file
      //
      {
         // NOTE: IOFSL handling not needed here, global definitions files
         // are not handled by IOFSL

         // initialize file type
         OTF_FileType file_type =
            base_file_type | ( Params.docompress ?
               OTF_FILECOMPRESSION_COMPRESSED :
               OTF_FILECOMPRESSION_UNCOMPRESSED );

         // get old and new file name
         //
         OTF_getFilename( tmp_out_file_prefix.c_str(), 0, file_type,
            STRBUFSIZE, filename1 );
         OTF_getFilename( Params.out_file_prefix.c_str(), 0, file_type,
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
            VPrint( 3, " Renamed %s to %s\n", filename1, filename2 );
         }
      }
   }

#ifdef VT_MPI
   SyncError( &error );
#endif // VT_MPI

   return !error;
}

// private methods
//

bool
DefinitionsC::readLocal()
{
   bool error = false;

   VPrint( 2, " Reading local definitions\n" );

#ifdef VT_MPI
   // list of request handles and send buffers which are in use
   std::list<std::pair<MPI_Request, char*> > send_buffers;

   // message tag to use for p2p communication
   const VT_MPI_INT msg_tag = 100;

   // minimum message size
   const VT_MPI_INT min_msg_size = 100 * 1024 * 1024;
#endif // VT_MPI

   // read local definitions of each given stream
   //

   // vector of local definitions
   LargeVectorC<DefRec_BaseS*> loc_defs;

   for( uint32_t i = 0; i < MyStreamIds.size(); i++ )
   {
      uint32_t defs_read = loc_defs.size(); // N defs. read in this iteration

      // put local definitions of streams which belonging together into
      // one vector
      //
      for( ; i < MyStreamIds.size(); i++ )
      {
         // read local definitions of stream
         if( (error = !readLocal( MyStreamIds[i], loc_defs )) )
            break;

         // continue reading if the next stream is a child
         if( i == MyStreamIds.size() - 1 ||
             StreamId2UnifyCtl[MyStreamIds[i+1]]->pstreamid == 0 )
            break;
      }
      if( error )
         break;

      // calculate number of local definitions read
      defs_read = loc_defs.size() - defs_read;

      // continue, if nothing is read
      if( i < MyStreamIds.size() - 1 && defs_read == 0 )
         continue;

      if( defs_read > 0 )
      {
         // pre-sort subset of local definitions
         //

         // get begin iterator of subset
         //
         LargeVectorC<DefRec_BaseS*>::iterator sort_begin_it = loc_defs.begin();
         if( loc_defs.size() != defs_read )
            sort_begin_it += ( loc_defs.size() - defs_read - 1 );

         // pre-sort
         std::stable_sort( sort_begin_it, loc_defs.end(), DefRec_LocCmp );
      }

      MASTER
      {
         // add local to global definitions
         if( (error = !processLocal( loc_defs )) )
            break;
      }
#ifdef VT_MPI
      else // SLAVE
      {
         // send local definitions to rank 0
         //

         // remove request handles and send buffers from list which are
         // not in use
         //
         VT_MPI_INT not_in_use = 1;
         while( send_buffers.size() > 0 && not_in_use )
         {
            // get the first request handle and send buffer from list
            //
            MPI_Request & request = send_buffers.front().first;
            char *& buffer = send_buffers.front().second;

            // test for completed send
            //
            MPI_Status status;
            CALL_MPI( MPI_Test( &request, &not_in_use, &status ) );

            // free send buffer, if it isn't in use
            //
            if( not_in_use )
            {
               delete [] buffer;
               send_buffers.pop_front();
            }
         }

         char * buffer;
         VT_MPI_INT buffer_pos;
         VT_MPI_INT buffer_size;
         VT_MPI_INT size;

         // get size needed for the send buffer
         //

         // loc_defs.size()
         CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD,
                                  &buffer_size ) );

         // loc_defs
         //
         for( uint32_t j = 0; j < loc_defs.size(); j++ )
            buffer_size += loc_defs[j]->getPackSize();

         // finished flag
         //
         CALL_MPI( MPI_Pack_size( 1, MPI_CHAR, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // continue reading, if minimum buffer size isn't reached
         if( i < MyStreamIds.size() - 1 && buffer_size < min_msg_size )
            continue;

         // allocate memory for the send buffer
         //
         buffer = new char[buffer_size];
         vt_assert( buffer );

         // pack send buffer
         //

         buffer_pos = 0;

         // loc_defs.size()
         //
         uint32_t loc_defs_size = loc_defs.size();
         CALL_MPI( MPI_Pack( &loc_defs_size, 1, MPI_UNSIGNED, buffer,
                             buffer_size, &buffer_pos, MPI_COMM_WORLD ) );

         // loc_defs
         //
         for( uint32_t j = 0; j < loc_defs.size(); j++ )
            loc_defs[j]->pack( buffer, buffer_size, buffer_pos );

         // finished flag
         //
         char finished = ( i == MyStreamIds.size() - 1 );
         CALL_MPI( MPI_Pack( &finished, 1, MPI_CHAR, buffer, buffer_size,
                             &buffer_pos, MPI_COMM_WORLD ) );

         // send buffer to rank 0
         //

         PVPrint( 3, "  Sending local definitions to rank 0\n" );

         MPI_Request request;
         CALL_MPI( MPI_Isend( buffer, buffer_size, MPI_PACKED, 0, msg_tag,
                              MPI_COMM_WORLD, &request ) );

         // add request handle and send buffer to list
         send_buffers.push_back( std::make_pair( request, buffer ) );
      }
#endif // VT_MPI

      // free vector of local definitions
      //
      for( uint32_t j = 0; j < loc_defs.size(); j++ )
         delete loc_defs[j];
      loc_defs.clear();
   }

#ifdef VT_MPI

   // all ranks are finished reading local definitions at this point
   //

   if( NumRanks > 1 && !SyncError( &error ) )
   {
      MASTER
      {
         // receive local definitions from all participating ranks
         //

         // get number of finished ranks
         //
         VT_MPI_INT finished_ranks_num = 1; // =rank 0
         for( VT_MPI_INT i = 1; i < NumRanks; i++ )
         {
            if( Rank2StreamIds[i].empty() ) // rank i has nothing to do?
               finished_ranks_num++;
         }

         // repeat until all ranks are finished reading local definitions
         //
         while( finished_ranks_num < NumRanks )
         {
            char * buffer;
            VT_MPI_INT buffer_size;
            VT_MPI_INT buffer_pos;
            VT_MPI_INT rank;
            MPI_Status status;

            // test for a message from any source rank
            CALL_MPI( MPI_Probe( MPI_ANY_SOURCE, msg_tag, MPI_COMM_WORLD,
                                 &status ) );

            // get source rank
            rank = status.MPI_SOURCE;

            // get size needed for the receive buffer
            CALL_MPI( MPI_Get_count( &status, MPI_PACKED, &buffer_size ) );

            // allocate memory for the receive buffer
            //
            buffer = new char[buffer_size];
            vt_assert( buffer );

            PVPrint( 3, "  Receiving local definitions from rank %d\n",
                     rank );

            // receive buffer
            CALL_MPI( MPI_Recv( buffer, buffer_size, MPI_PACKED, rank, msg_tag,
                                MPI_COMM_WORLD, &status ) );

            // unpack receive buffer
            //

            buffer_pos = 0;

            // loc_defs.size()
            //
            uint32_t loc_defs_size;
            CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos,
                                  &loc_defs_size, 1, MPI_UNSIGNED,
                                  MPI_COMM_WORLD ) );

            // loc_defs
            //
            for( uint32_t i = 0; i < loc_defs_size; i++ )
            {
               // definition type
               // (don't increment current buffer position;
               //  def. type will be unpacked again by DefRec_*S::unpack())
               //
               DefRecTypeT def_type;
               VT_MPI_INT tmp_buffer_pos = buffer_pos;
               CALL_MPI( MPI_Unpack( buffer, buffer_size, &tmp_buffer_pos,
                                     &def_type, 1, MPI_UNSIGNED,
                                     MPI_COMM_WORLD ) );

               // create instance for local definition relating to its type
               //

               DefRec_BaseS * new_loc_def = 0;

               switch( def_type )
               {
                  case DEF_REC_TYPE__DefCreator:
                  {
                     new_loc_def = new DefRec_DefCreatorS();
                     break;
                  }
                  case DEF_REC_TYPE__DefComment:
                  {
                     new_loc_def = new DefRec_DefCommentS();
                     break;
                  }
                  case DEF_REC_TYPE__DefTimerResolution:
                  {
                     new_loc_def = new DefRec_DefTimerResolutionS();
                     break;
                  }
                  case DEF_REC_TYPE__DefTimeRange:
                  {
                     new_loc_def = new DefRec_DefTimeRangeS();
                     break;
                  }
                  case DEF_REC_TYPE__DefProcess:
                  {
                     new_loc_def = new DefRec_DefProcessS();
                     break;
                  }
                  case DEF_REC_TYPE__DefProcessGroup:
                  {
                     new_loc_def = new DefRec_DefProcessGroupS();
                     break;
                  }
                  case DEF_REC_TYPE__DefProcessGroupAttributes:
                  {
                     new_loc_def = new DefRec_DefProcessGroupAttributesS();
                     break;
                  }
                  case DEF_REC_TYPE__DefSclFile:
                  {
                     new_loc_def = new DefRec_DefSclFileS();
                     break;
                  }
                  case DEF_REC_TYPE__DefScl:
                  {
                     new_loc_def = new DefRec_DefSclS();
                     break;
                  }
                  case DEF_REC_TYPE__DefFileGroup:
                  {
                     new_loc_def = new DefRec_DefFileGroupS();
                     break;
                  }
                  case DEF_REC_TYPE__DefFile:
                  {
                     new_loc_def = new DefRec_DefFileS();
                     break;
                  }
                  case DEF_REC_TYPE__DefFunctionGroup:
                  {
                     new_loc_def = new DefRec_DefFunctionGroupS();
                     break;
                  }
                  case DEF_REC_TYPE__DefFunction:
                  {
                     new_loc_def = new DefRec_DefFunctionS();
                     break;
                  }
                  case DEF_REC_TYPE__DefCollOp:
                  {
                     new_loc_def = new DefRec_DefCollOpS();
                     break;
                  }
                  case DEF_REC_TYPE__DefCounterGroup:
                  {
                     new_loc_def = new DefRec_DefCounterGroupS();
                     break;
                  }
                  case DEF_REC_TYPE__DefCounter:
                  {
                     new_loc_def = new DefRec_DefCounterS();
                     break;
                  }
                  case DEF_REC_TYPE__DefCounterAssignments:
                  {
                     new_loc_def = new DefRec_DefCounterAssignmentsS();
                     break;
                  }
                  case DEF_REC_TYPE__DefKeyValue:
                  {
                     new_loc_def = new DefRec_DefKeyValueS();
                     break;
                  }
                  default:
                  {
                     vt_assert( 0 );
                  }
               }
               vt_assert( new_loc_def );

               // unpack local definition from receive buffer
               new_loc_def->unpack( buffer, buffer_size, buffer_pos );

               // add local definition to vector
               loc_defs.push_back( new_loc_def );
            }

            // finished flag
            //
            char finished;
            CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos, &finished,
                                  1, MPI_CHAR, MPI_COMM_WORLD ) );

            // free memory of receive buffer
            delete [] buffer;

            // add local to global definitions
            if( (error = !processLocal( loc_defs )) )
               break;

            // free vector of local definitions
            //
            for( uint32_t i = 0; i < loc_defs.size(); i++ )
               delete loc_defs[i];
            loc_defs.clear();

            // is source rank finished reading local definitions?
            if( finished )
            {
               // increment number of finished ranks
               finished_ranks_num++;

               // send token translations to finished rank
               if( (error =
                       !theTokenFactory->distTranslations( rank,
                          finished_ranks_num == NumRanks )) )
                  break;
            }
         }
      }
      else // SLAVE
      {
         if( !MyStreamIds.empty() )
         {
            // complete all sends and remove request handles and send buffers
            // from list
            while( send_buffers.size() > 0 )
            {
               // get the first request handle and send buffer from list
               //
               MPI_Request & request = send_buffers.front().first;
               char *& buffer = send_buffers.front().second;

               // wait until send is completed
               //
               MPI_Status status;
               CALL_MPI( MPI_Wait( &request, &status ) );

               // free memory of send buffer
               delete [] buffer;
               // remove request handle and send buffer from list
               send_buffers.pop_front();
            }

            // receive token translations from rank 0
            error = !theTokenFactory->distTranslations();
         }
      }
   }
#endif // VT_MPI

   return !error;
}

bool
DefinitionsC::readLocal( const uint32_t & streamId,
                         LargeVectorC<DefRec_BaseS*> & locDefs )
{
   bool error = false;

   // open file manager for reader stream
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
      OTF_RStream_open( Params.in_file_prefix.c_str(), streamId, manager );
   vt_assert( rstream );

   PVPrint( 3, "  Opened OTF reader stream [namestub %s id %x]\n",
            Params.in_file_prefix.c_str(), streamId );

   do
   {
      // try to get def. buffer
      //
      if( !OTF_RStream_getDefBuffer( rstream ) )
      {
         PVPrint( 3, "   No definitions found in this OTF reader stream "
                     "- Ignored\n" );
         break;
      }

      // close definitions buffer
      OTF_RStream_closeDefBuffer( rstream );

      // create record handler array
      //
      OTF_HandlerArray * handler_array = OTF_HandlerArray_open();
      vt_assert( handler_array );

      // create first handler argument
      FirstHandlerArg_DefsS fha( locDefs );

      // set record handler and its first argument for ...
      //

      // ... OTF_DEFINITIONCOMMENT_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefComment,
         OTF_DEFINITIONCOMMENT_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFINITIONCOMMENT_RECORD );

      // ... OTF_DEFCREATOR_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefCreator,
         OTF_DEFCREATOR_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFCREATOR_RECORD );

      // ... OTF_DEFTIMERRESOLUTION_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefTimerResolution,
         OTF_DEFTIMERRESOLUTION_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFTIMERRESOLUTION_RECORD );

      // ... OTF_DEFTIMERANGE_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefTimeRange,
         OTF_DEFTIMERANGE_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFTIMERANGE_RECORD );

      // ... OTF_DEFPROCESSGROUP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefProcessGroup,
         OTF_DEFPROCESSGROUP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFPROCESSGROUP_RECORD );

      // ... OTF_DEFPROCESSORGROUPATTR_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefProcessGroupAttributes,
         OTF_DEFPROCESSORGROUPATTR_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFPROCESSORGROUPATTR_RECORD );

      // ... OTF_DEFPROCESS_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefProcess,
         OTF_DEFPROCESS_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFPROCESS_RECORD );

      // ... OTF_DEFSCLFILE_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefSclFile,
         OTF_DEFSCLFILE_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFSCLFILE_RECORD );

      // ... OTF_DEFSCL_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefScl,
         OTF_DEFSCL_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFSCL_RECORD );

      // ... OTF_DEFFILEGROUP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefFileGroup,
         OTF_DEFFILEGROUP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFFILEGROUP_RECORD );

      // ... OTF_DEFFILE_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefFile,
         OTF_DEFFILE_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFFILE_RECORD );

      // ... OTF_DEFFUNCTIONGROUP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefFunctionGroup,
         OTF_DEFFUNCTIONGROUP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFFUNCTIONGROUP_RECORD );

      // ... OTF_DEFFUNCTION_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefFunction,
         OTF_DEFFUNCTION_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFFUNCTION_RECORD );

      // ... OTF_DEFCOLLOP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefCollOp,
         OTF_DEFCOLLOP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFCOLLOP_RECORD );

      // ... OTF_DEFCOUNTERGROUP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefCounterGroup,
         OTF_DEFCOUNTERGROUP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFCOUNTERGROUP_RECORD );

      // ... OTF_DEFCOUNTER_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefCounter,
         OTF_DEFCOUNTER_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFCOUNTER_RECORD );

      // ... OTF_DEFCOUNTERASSIGNMENTS_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefCounterAssignments,
         OTF_DEFCOUNTERASSIGNMENTS_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFCOUNTERASSIGNMENTS_RECORD );

      // ... OTF_DEFKEYVALUE_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefKeyValue,
         OTF_DEFKEYVALUE_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFKEYVALUE_RECORD );

      // read local definitions
      //
      if( OTF_RStream_readDefinitions( rstream, handler_array ) ==
          OTF_READ_ERROR )
      {
         std::cerr << ExeName << ": Error: "
                   << "Could not read definitions of OTF stream [namestub "
                   << Params.in_file_prefix << " id "
                   << std::hex << streamId << "]"
                   << std::dec << std::endl;
         error = true;
      }

      // close record handler
      OTF_HandlerArray_close( handler_array );

   } while( false );

   // close reader stream
   OTF_RStream_close( rstream );
   // close file manager for reader stream
   OTF_FileManager_close( manager );

   PVPrint( 3, "  Closed OTF reader stream [namestub %s id %x]\n",
            Params.in_file_prefix.c_str(), streamId );

   return !error;
}

bool
DefinitionsC::processLocal( const LargeVectorC<DefRec_BaseS*> & locDefs )
{
   bool error = false;

   for( uint32_t i = 0; i < locDefs.size() && !error; i++ )
   {
      // handle local definition depending on its type
      //
      switch( locDefs[i]->dtype )
      {
         case DEF_REC_TYPE__DefComment:
         {
            // get local definition entry
            const DefRec_DefCommentS * loc_def_entry =
               static_cast<DefRec_DefCommentS*>( locDefs[i] );

            // process local definition comment
            error = !m_comments->processLocal( *loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefCreator:
         {
            // get local definition entry
            const DefRec_DefCreatorS * loc_def_entry =
               static_cast<DefRec_DefCreatorS*>( locDefs[i] );

            // create global creator definition only once
            //
            static bool creator_added = false;
            if( !creator_added )
            {
               m_globDefs.creator = *loc_def_entry;
               creator_added = true;
            }

            break;
         }
         case DEF_REC_TYPE__DefTimerResolution:
         {
            // get local definitions entry
            const DefRec_DefTimerResolutionS * loc_def_entry =
               static_cast<DefRec_DefTimerResolutionS*>( locDefs[i] );

            // create global timer res. definition only once
            //
            static bool timeres_added = false;
            if( !timeres_added )
            {
               m_globDefs.timeres = *loc_def_entry;
               timeres_added = true;
            }

            break;
         }
         case DEF_REC_TYPE__DefTimeRange:
         {
            // get local definitions entry
            const DefRec_DefTimeRangeS * loc_def_entry =
               static_cast<DefRec_DefTimeRangeS*>( locDefs[i] );

            // temporary store local time range
            theTimeSync->setTimeRange( loc_def_entry->loccpuid,
               loc_def_entry->minTime, loc_def_entry->maxTime );

            break;
         }
         case DEF_REC_TYPE__DefProcess:
         {
            // get local definition entry
            const DefRec_DefProcessS * loc_def_entry =
               static_cast<DefRec_DefProcessS*>( locDefs[i] );

            // process definitions don't need to be unified, so add it
            // to global definitions without any change
            m_globDefs.procs.insert( *loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefProcessGroup:
         {
            // get local definition entry
            DefRec_DefProcessGroupS * loc_def_entry =
               static_cast<DefRec_DefProcessGroupS*>( locDefs[i] );

            // process local process group definition
            error = !m_procGrps->processLocal( *loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefProcessGroupAttributes:
         {
            // get local definition entry
            DefRec_DefProcessGroupAttributesS * loc_def_entry =
               static_cast<DefRec_DefProcessGroupAttributesS*>( locDefs[i] );

            // get global token factory for DefProcessGroup
            static TokenFactoryScopeI * tkfac_defprocgrp =
               theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

            // get global token for DefProcessGroup
            //
            uint32_t global_procgrp =
               tkfac_defprocgrp->translate( loc_def_entry->loccpuid,
                  loc_def_entry->deftoken );
            vt_assert( global_procgrp != 0 );

            // search for global attributes by process group token
            std::map<uint32_t, DefRec_DefProcessGroupAttributesS>::iterator
               procgrp_attrs_it =
                  m_globDefs.procGrpAttrs.find( global_procgrp );

            // add global process group attributes, if not found
            //
            if( procgrp_attrs_it == m_globDefs.procGrpAttrs.end() )
            {
               procgrp_attrs_it =
                  m_globDefs.procGrpAttrs.insert(
                     std::make_pair( global_procgrp,
                        DefRec_DefProcessGroupAttributesS() ) ).first;

               procgrp_attrs_it->second.deftoken = global_procgrp;
            }

            // add attributes to global process group attributes
            procgrp_attrs_it->second.attributes |= loc_def_entry->attributes;

            break;
         }
         case DEF_REC_TYPE__DefSclFile:
         {
            // get local definition entry
            const DefRec_DefSclFileS * loc_def_entry =
               static_cast<DefRec_DefSclFileS*>( locDefs[i] );

            // get global token factory for DefSclFile
            static TokenFactoryScopeI * tkfac_defsclfile =
               theTokenFactory->getScope( DEF_REC_TYPE__DefSclFile );

            // create global definition for DefSclFile
            tkfac_defsclfile->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefScl:
         {
            // get local definition entry
            DefRec_DefSclS * loc_def_entry =
               static_cast<DefRec_DefSclS*>( locDefs[i] );

            // get global token factory for DefSclFile
            static TokenFactoryScopeI * tkfac_defsclfile =
               theTokenFactory->getScope( DEF_REC_TYPE__DefSclFile );

            // get global token factory for DefScl
            static TokenFactoryScopeI * tkfac_defscl =
               theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

            // get global token for DefSclFile
            //
            uint32_t global_sclfile =
               tkfac_defsclfile->translate(
                  loc_def_entry->loccpuid, loc_def_entry->sclfile );
            vt_assert( global_sclfile != 0 );
            loc_def_entry->sclfile = global_sclfile;

            // create global token for DefScl
            tkfac_defscl->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefFileGroup:
         {
            // get local definition entry
            const DefRec_DefFileGroupS * loc_def_entry =
               static_cast<DefRec_DefFileGroupS*>( locDefs[i] );

            // get global token factory for DefFileGroup
            static TokenFactoryScopeI * tkfac_deffilegroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFileGroup );

            // create global definition for DefFileGroup
            tkfac_deffilegroup->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefFile:
         {
            // get local definition entry
            DefRec_DefFileS * loc_def_entry =
               static_cast<DefRec_DefFileS*>( locDefs[i] );

            // get global token factory for DefFileGroup
            static TokenFactoryScopeI * tkfac_deffilegroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFileGroup );

            // get global token factory for DefFile
            static TokenFactoryScopeI * tkfac_deffile =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFile );

            // get global token for DefFileGroup
            //
            uint32_t global_filegroup =
               tkfac_deffilegroup->translate(
                  loc_def_entry->loccpuid, loc_def_entry->group );
            vt_assert( global_filegroup != 0 );
            loc_def_entry->group = global_filegroup;

            // create global token for DefFile
            tkfac_deffile->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefFunctionGroup:
         {
            // get local definition entry
            const DefRec_DefFunctionGroupS * loc_def_entry =
               static_cast<DefRec_DefFunctionGroupS*>( locDefs[i] );

            // get global token factory for DefFunctionGroup
            static TokenFactoryScopeI * tkfac_deffuncgroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFunctionGroup );

            // create global definition for DefFunctionGroup
            tkfac_deffuncgroup->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefFunction:
         {
            // get local definition entry
            DefRec_DefFunctionS * loc_def_entry =
               static_cast<DefRec_DefFunctionS*>( locDefs[i] );

            // get global token factory for DefFunctionGroup
            static TokenFactoryScopeI * tkfac_deffuncgroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFunctionGroup );

            // get global token factory for DefScl
            static TokenFactoryScopeI * tkfac_defscl =
               theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

            // get global token factory for DefFunction
            static TokenFactoryScopeI * tkfac_deffunc =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFunction );

            // get global token for DefFunctionGroup
            //
            uint32_t global_funcgroup =
               tkfac_deffuncgroup->translate(
                  loc_def_entry->loccpuid, loc_def_entry->group );
            vt_assert( global_funcgroup != 0 );
            loc_def_entry->group = global_funcgroup;

            // get global token for DefScl
            //
            if( loc_def_entry->scltoken != 0 )
            {
               uint32_t global_scl =
                  tkfac_defscl->translate(
                     loc_def_entry->loccpuid, loc_def_entry->scltoken );
               vt_assert( global_scl != 0 );
               loc_def_entry->scltoken = global_scl;
            }

            // create global token for DefFunction
            tkfac_deffunc->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefCollOp:
         {
            // get local definition entry
            const DefRec_DefCollOpS * loc_def_entry =
               static_cast<DefRec_DefCollOpS*>( locDefs[i] );

            // get global token factory for DefCollOp
            static TokenFactoryScopeI * tkfac_defcollop =
               theTokenFactory->getScope( DEF_REC_TYPE__DefCollOp );

            // create global definition for DefCollOp
            tkfac_defcollop->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefCounterGroup:
         {
            // get local definition entry
            const DefRec_DefCounterGroupS * loc_def_entry =
               static_cast<DefRec_DefCounterGroupS*>( locDefs[i] );

            // get global token factory for DefCounterGroup
            static TokenFactoryScopeI * tkfac_defcntrgroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefCounterGroup );

            // create global definition for DefCounterGroup
            tkfac_defcntrgroup->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefCounter:
         {
            // get local definition entry
            DefRec_DefCounterS * loc_def_entry =
               static_cast<DefRec_DefCounterS*>( locDefs[i] );

            // get global token factory for DefCounterGroup
            static TokenFactoryScopeI * tkfac_defcntrgroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefCounterGroup );

            // get global token factory for DefCounter
            static TokenFactoryScopeI * tkfac_defcntr =
               theTokenFactory->getScope( DEF_REC_TYPE__DefCounter );

            // get global token for DefCounterGroup
            //
            uint32_t global_cntrgroup =
               tkfac_defcntrgroup->translate(
                  loc_def_entry->loccpuid, loc_def_entry->group );
            vt_assert( global_cntrgroup != 0 );
            loc_def_entry->group = global_cntrgroup;

            // create global token for DefCounter
            tkfac_defcntr->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefCounterAssignments:
         {
            // get local definition entry
            DefRec_DefCounterAssignmentsS * loc_def_entry =
               static_cast<DefRec_DefCounterAssignmentsS*>( locDefs[i] );

            // get global token factory for DefCounter
            static TokenFactoryScopeI * tkfac_defcntr =
               theTokenFactory->getScope( DEF_REC_TYPE__DefCounter );

            // get global token factory for DefProcessGroup
            static TokenFactoryScopeI * tkfac_defprocgrp =
               theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

            // get global token for DefCounter
            //
            uint32_t global_cntr =
               tkfac_defcntr->translate(
                  loc_def_entry->loccpuid, loc_def_entry->deftoken );
            vt_assert( global_cntr != 0 );

            // search for global counter assignments by counter token
            std::map<uint32_t, DefRec_DefCounterAssignmentsS>::iterator
               cntr_assigns_it =
                  m_globDefs.cntrAssigns.find( global_cntr );

            // add global counter assignments, if not found
            //
            if( cntr_assigns_it == m_globDefs.cntrAssigns.end() )
            {
               cntr_assigns_it =
                  m_globDefs.cntrAssigns.insert( std::make_pair( global_cntr,
                     DefRec_DefCounterAssignmentsS() ) ).first;

               cntr_assigns_it->second.deftoken = global_cntr;
            }

            // get global token for DefProcessGroup
            //
            uint32_t global_procgrp =
               tkfac_defprocgrp->translate( loc_def_entry->loccpuid,
                  *(loc_def_entry->groups.begin()) );
            vt_assert( global_procgrp != 0 );

            // add process group token to global counter assignments
            cntr_assigns_it->second.groups.insert( global_procgrp );

            // add process group token to stream
            m_groupCntrs->addGroupToStream( loc_def_entry->loccpuid,
               global_procgrp );

            break;
         }
         case DEF_REC_TYPE__DefKeyValue:
         {
            // get local definition entry
            const DefRec_DefKeyValueS * loc_def_entry =
               static_cast<DefRec_DefKeyValueS*>( locDefs[i] );

            // get global token factory for DefKeyValue
            static TokenFactoryScopeI * tkfac_defkeyval =
               theTokenFactory->getScope( DEF_REC_TYPE__DefKeyValue );

            // create global definition for DefKeyValue
            tkfac_defkeyval->create( loc_def_entry );

            break;
         }
         default:
         {
            vt_assert( 0 );
         }
      }
   }

   return !error;
}

bool
DefinitionsC::writeGlobal()
{
   bool error = false;

   VPrint( 2, " Writing global definitions\n" );

   // get temporary output file prefix
   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // open file manager for writer stream
   //
   OTF_FileManager * manager = OTF_FileManager_open( 1 );
   vt_assert( manager );

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

      OTF_FileManager_setIofsl( manager, Params.iofsl_num_servers,
         Params.iofsl_servers, otf_iofsl_mode, otf_iofsl_flags, 0,
         VT_TRACEID_BITMASK );
   }
#endif // HAVE_IOFSL

   // open stream for writing (stream id = 0)
   //
   OTF_WStream * wstream =
      OTF_WStream_open( tmp_out_file_prefix.c_str(), 0, manager );
   vt_assert( wstream );

   VPrint( 3, "  Opened OTF writer stream [namestub %s id 0]\n",
           tmp_out_file_prefix.c_str() );

   // set file compression
   if( Params.docompress )
      OTF_WStream_setCompression( wstream, OTF_FILECOMPRESSION_COMPRESSED );

   do
   {
      // write OTF version record
      error = ( OTF_WStream_writeOtfVersion( wstream ) == 0 );

      // write unique id record
      error = ( OTF_WStream_writeUniqueId( wstream ) == 0 );

      // write global definition records
      //
      for( uint32_t t = 0; t < (uint32_t)DEF_REC_TYPE__Num && !error; t++ )
      {
         DefRecTypeT def_type = static_cast<DefRecTypeT>( t );

         switch( def_type )
         {
            case DEF_REC_TYPE__DefCreator:
            {
               bool do_write = true;

               // get reference to definition record for more convenient access
               DefRec_DefCreatorS & record = m_globDefs.creator;

               // trigger write record hook
               theHooks->triggerWriteRecordHook( HooksC::Record_DefCreator, 3,
                  &wstream, &(record.creator), &do_write );

               // write record
               //
               if( do_write )
               {
                  error =
                     ( OTF_WStream_writeDefCreator( wstream,
                          record.creator.c_str() ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefTimerResolution:
            {
               bool do_write = true;

               // get reference to definition record for more convenient access
               DefRec_DefTimerResolutionS & record = m_globDefs.timeres;

               // trigger write record hook
               theHooks->triggerWriteRecordHook(
                  HooksC::Record_DefTimerResolution, 3, &wstream,
                  &(record.ticksPerSecond), &do_write );

               // write record
               //
               if( do_write )
               {
                  error =
                     ( OTF_WStream_writeDefTimerResolution( wstream,
                          record.ticksPerSecond ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefTimeRange:
            {
               bool do_write = true;

               // get reference to definition record for more convenient access
               DefRec_DefTimeRangeS & record = m_globDefs.timerange;

               // trigger write record hook
               theHooks->triggerWriteRecordHook(
                  HooksC::Record_DefTimeRange, 4, &wstream,
                  &(record.minTime), &(record.maxTime), &do_write );

               // write record
               //
               if( do_write )
               {
                  error =
                     ( OTF_WStream_writeDefTimeRange( wstream, record.minTime,
                          record.maxTime, 0 ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefComment:
            {
               // resort definition comments
               //

               typedef
                  std::set<const DefRec_DefCommentS*,
                     DefRec_DefCommentS::SortS> resorted_comments_t;

               resorted_comments_t resorted_comments;

               resortGlobDefs<DefRec_DefCommentS>(
                  m_globDefs.comments, resorted_comments );

               // iterate over all definition comments
               for( resorted_comments_t::const_iterator it =
                    resorted_comments.begin(); it != resorted_comments.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefCommentS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook(
                     HooksC::Record_DefComment, 4, &wstream, &(record.type),
                     &(record.comment), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefinitionComment( wstream,
                             record.comment.c_str() ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefProcess:
            {
               // resort process definitions
               //

               typedef
                  std::set<const DefRec_DefProcessS*,
                     DefRec_DefProcessS::SortS> resorted_procs_t;

               resorted_procs_t resorted_procs;

               resortGlobDefs<DefRec_DefProcessS>(
                  m_globDefs.procs, resorted_procs );

               // iterate over all process definitions
               for( resorted_procs_t::const_iterator it =
                    resorted_procs.begin(); it != resorted_procs.end(); ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefProcessS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefProcess,
                     5, &wstream, &(record.deftoken), &(record.name),
                     &(record.parent), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefProcess( wstream, record.deftoken,
                             record.name.c_str(), record.parent ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefProcessGroup:
            {
               // resort process group definitions
               //

               typedef
                  std::set<const DefRec_DefProcessGroupS*,
                     DefRec_DefProcessGroupS::SortS> resorted_proc_grps_t;

               resorted_proc_grps_t resorted_proc_grps;

               resortGlobDefs<DefRec_DefProcessGroupS>(
                  m_globDefs.procGrps, resorted_proc_grps );

               // iterate over all process group definitions
               for( resorted_proc_grps_t::const_iterator it =
                    resorted_proc_grps.begin(); it != resorted_proc_grps.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefProcessGroupS record = **it;

                  // inflate group members
                  m_procGrps->inflateMembers( record );

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook(
                     HooksC::Record_DefProcessGroup, 6, &wstream,
                     &(record.deftoken), &(record.name), &(record.nmembers),
                     &(record.members), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefProcessGroup( wstream,
                             record.deftoken, record.name.c_str(),
                             record.nmembers, record.members ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefProcessGroupAttributes:
            {
               if( !m_globDefs.procGrpAttrs.empty() )
               {
                  typedef DefRec_DefProcessGroupAttributesS
                     attrs_list_t;

                  // storage of global attributes list definitions
                  std::set<attrs_list_t> global_attrs_lists;

                  // create global attributes list definitions
                  //
                  {
                     // create global token factory scope for attributes list
                     // definitions
                     //
                     TokenFactoryScopeC<attrs_list_t> * tkfac_attrs_list =
                        new TokenFactoryScopeC<attrs_list_t>
                           ( &global_attrs_lists );
                     vt_assert( tkfac_attrs_list );

                     // iterate over all process group attr. definitions
                     for( std::map<uint32_t,
                          DefRec_DefProcessGroupAttributesS>::iterator
                          procgrp_attrs_it = m_globDefs.procGrpAttrs.begin();
                          procgrp_attrs_it != m_globDefs.procGrpAttrs.end();
                          ++procgrp_attrs_it )
                     {
                        bool do_write = true;

                        DefRec_DefProcessGroupAttributesS & procgrp_attrs =
                           procgrp_attrs_it->second;

                        // trigger write record hook
                        theHooks->triggerWriteRecordHook(
                           HooksC::Record_DefProcessGroupAttributes, 4,
                           &wstream, &(procgrp_attrs.deftoken),
                           &(procgrp_attrs.attributes), &do_write );

                        if( do_write )
                        {
                           // create global attributes list definition
                           //

                           attrs_list_t attrs_list =
                              attrs_list_t( 0, 0, procgrp_attrs.attributes );

                           procgrp_attrs.attributes =
                              tkfac_attrs_list->create( &attrs_list );
                        }
                        else
                        {
                           procgrp_attrs.attributes = 0;
                        }
                     }

                     // delete global token factory scope for attributes list
                     // definitions
                     delete tkfac_attrs_list;
                  }

                  // write global attributes list definitions
                  //
                  {
                     // iterate over all attributes list definitions
                     for( std::set<attrs_list_t>::const_iterator attrs_list_it =
                          global_attrs_lists.begin(); attrs_list_it !=
                          global_attrs_lists.end() && !error; ++attrs_list_it )
                     {
                        // convert bitmask to array
                        //
                        uint32_t n = 0;
                        OTF_ATTR_TYPE array[32];
                        for( uint32_t i = 0; i < 32; i++ )
                        {
                           if( attrs_list_it->attributes & (1<<i) )
                              array[n++] = static_cast<OTF_ATTR_TYPE>( i );
                        }

                        // write record
                        error =
                           ( OTF_WStream_writeDefAttributeList( wstream,
                                attrs_list_it->deftoken, n, array ) == 0 );
                     }
                  }

                  // write global process group attr. definitions
                  //
                  {
                     // iterate over all process group attr. definitions
                     for( std::map<uint32_t,
                          DefRec_DefProcessGroupAttributesS>::const_iterator
                          procgrp_attrs_it = m_globDefs.procGrpAttrs.begin();
                          procgrp_attrs_it != m_globDefs.procGrpAttrs.end() &&
                          !error; ++procgrp_attrs_it )
                     {
                        const DefRec_DefProcessGroupAttributesS & procgrp_attrs
                           = procgrp_attrs_it->second;

                        // write record, if global attributes token is present
                        //
                        if( procgrp_attrs.attributes != 0 )
                        {
                           error =
                              ( OTF_WStream_writeDefProcessOrGroupAttributes(
                                   wstream, procgrp_attrs.deftoken,
                                   procgrp_attrs.attributes ) == 0 );
                        }
                     }
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefSclFile:
            {
               // resort scl file definitions
               //

               typedef
                  std::set<const DefRec_DefSclFileS*,
                     DefRec_DefSclFileS::SortS> resorted_scl_files_t;

               resorted_scl_files_t resorted_scl_files;

               resortGlobDefs<DefRec_DefSclFileS>(
                  m_globDefs.sclFiles, resorted_scl_files );

               // iterate over all scl file definitions
               for( resorted_scl_files_t::const_iterator it =
                    resorted_scl_files.begin(); it != resorted_scl_files.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefSclFileS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefSclFile,
                     4, &wstream, &(record.deftoken), &(record.filename),
                     &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefSclFile( wstream, record.deftoken,
                             record.filename.c_str() ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefScl:
            {
               // resort scl definitions
               //

               typedef
                  std::set<const DefRec_DefSclS*, DefRec_DefSclS::SortS>
                     resorted_scls_t;

               resorted_scls_t resorted_scls;

               resortGlobDefs<DefRec_DefSclS>(
                  m_globDefs.scls, resorted_scls );

               // iterate over all scl definitions
               for( resorted_scls_t::const_iterator it =
                    resorted_scls.begin(); it != resorted_scls.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefSclS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefScl, 5,
                     &wstream, &(record.deftoken), &(record.sclfile),
                     &(record.sclline), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefScl( wstream, record.deftoken,
                             record.sclfile, record.sclline ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefFileGroup:
            {
               // resort file group definitions
               //

               typedef
                  std::set<const DefRec_DefFileGroupS*,
                     DefRec_DefFileGroupS::SortS> resorted_file_grps_t;

               resorted_file_grps_t resorted_file_grps;

               resortGlobDefs<DefRec_DefFileGroupS>(
                  m_globDefs.fileGrps, resorted_file_grps );

               // iterate over all file group definitions
               for( resorted_file_grps_t::const_iterator it =
                    resorted_file_grps.begin(); it != resorted_file_grps.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefFileGroupS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefFileGroup,
                     4, &wstream, &(record.deftoken), &(record.name),
                     &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error = ( OTF_WStream_writeDefFileGroup( wstream,
                                  record.deftoken, record.name.c_str() ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefFile:
            {
               // resort file definitions
               //

               typedef
                  std::set<const DefRec_DefFileS*, DefRec_DefFileS::SortS>
                     resorted_files_t;

               resorted_files_t resorted_files;

               resortGlobDefs<DefRec_DefFileS>(
                  m_globDefs.files, resorted_files );

               // iterate over all file definitions
               for( resorted_files_t::const_iterator it =
                    resorted_files.begin(); it != resorted_files.end(); ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefFileS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefFile, 5,
                     &wstream, &(record.deftoken), &(record.name),
                     &(record.group), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefFile( wstream, record.deftoken,
                             record.name.c_str(), record.group ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefFunctionGroup:
            {
               // resort function group definitions
               //

               typedef
                  std::set<const DefRec_DefFunctionGroupS*,
                     DefRec_DefFunctionGroupS::SortS> resorted_func_grps_t;

               resorted_func_grps_t resorted_func_grps;

               resortGlobDefs<DefRec_DefFunctionGroupS>(
                  m_globDefs.funcGrps, resorted_func_grps );

               // iterate over all function group definitions
               for( resorted_func_grps_t::const_iterator it =
                    resorted_func_grps.begin(); it != resorted_func_grps.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefFunctionGroupS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook(
                     HooksC::Record_DefFunctionGroup, 4, &wstream,
                     &(record.deftoken), &(record.name), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefFunctionGroup( wstream,
                             record.deftoken, record.name.c_str() ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefFunction:
            {
               // resort function definitions
               //

               typedef
                  std::set<const DefRec_DefFunctionS*,
                     DefRec_DefFunctionS::SortS> resorted_funcs_t;

               resorted_funcs_t resorted_funcs;

               resortGlobDefs<DefRec_DefFunctionS>(
                  m_globDefs.funcs, resorted_funcs );

               // iterate over all function definitions
               for( resorted_funcs_t::const_iterator it =
                    resorted_funcs.begin(); it != resorted_funcs.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefFunctionS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefFunction,
                     6, &wstream, &(record.deftoken), &(record.name),
                     &(record.group), &(record.scltoken), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefFunction( wstream,
                             record.deftoken, record.name.c_str(),
                             record.group, record.scltoken ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefCollOp:
            {
               // resort collop. definitions
               //

               typedef
                  std::set<const DefRec_DefCollOpS*, DefRec_DefCollOpS::SortS>
                     resorted_collops_t;

               resorted_collops_t resorted_collops;

               resortGlobDefs<DefRec_DefCollOpS>(
                  m_globDefs.collops, resorted_collops );

               // iterate over all collop. definitions
               for( resorted_collops_t::const_iterator it =
                    resorted_collops.begin(); it != resorted_collops.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefCollOpS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefCollOp, 5,
                     &wstream, &(record.deftoken), &(record.name),
                     &(record.type), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefCollectiveOperation( wstream,
                             record.deftoken, record.name.c_str(),
                             record.type ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefCounterGroup:
            {
               // resort counter group definitions
               //

               typedef
                  std::set<const DefRec_DefCounterGroupS*,
                     DefRec_DefCounterGroupS::SortS> resorted_cntr_grps_t;

               resorted_cntr_grps_t resorted_cntr_grps;

               resortGlobDefs<DefRec_DefCounterGroupS>(
                  m_globDefs.cntrGrps, resorted_cntr_grps );

               // iterate over all counter group definitions
               for( resorted_cntr_grps_t::const_iterator it =
                    resorted_cntr_grps.begin(); it != resorted_cntr_grps.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefCounterGroupS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook(
                     HooksC::Record_DefCounterGroup, 4, &wstream,
                     &(record.deftoken), &(record.name), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefCounterGroup( wstream,
                             record.deftoken, record.name.c_str() ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefCounter:
            {
               // resort counter definitions
               //

               typedef
                  std::set<const DefRec_DefCounterS*, DefRec_DefCounterS::SortS>
                     resorted_cntrs_t;

               resorted_cntrs_t resorted_cntrs;

               resortGlobDefs<DefRec_DefCounterS>(
                  m_globDefs.cntrs, resorted_cntrs );

               // iterate over all counter definitions
               for( resorted_cntrs_t::const_iterator it =
                    resorted_cntrs.begin(); it != resorted_cntrs.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefCounterS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefCounter,
                     7, &wstream, &(record.deftoken), &(record.name),
                     &(record.properties), &(record.group), &(record.unit),
                     &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefCounter( wstream,
                             record.deftoken, record.name.c_str(),
                             record.properties, record.group,
                             record.unit.c_str() ) == 0 );
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefCounterAssignments:
            {
              // iterate over all counter assignment definitions
              for( std::map<uint32_t, DefRec_DefCounterAssignmentsS>::iterator
                   cntr_assigns_it = m_globDefs.cntrAssigns.begin();
                   cntr_assigns_it != m_globDefs.cntrAssigns.end();
                   ++cntr_assigns_it )
              {
                 bool do_write = true;

                 DefRec_DefCounterAssignmentsS & record =
                    cntr_assigns_it->second;

                 // trigger write record hook
                 theHooks->triggerWriteRecordHook(
                    HooksC::Record_DefCounterAssignments, 4,
                    &wstream, &(record.deftoken), &(record.groups), &do_write );

                 // write record
                 //
                 if( do_write )
                 {
                    // convert std::set to C-array
                    //
                    uint32_t n = record.groups.size();
                    uint32_t * array = new uint32_t[n];
                    vt_assert( array );
                    uint32_t i = 0;
                    for( std::set<uint32_t>::const_iterator it =
                         record.groups.begin(); it != record.groups.end();
                         ++it, i++ )
                    {
                       array[i] = *it;
                    }

                    // write record
                    error =
                       ( OTF_WStream_writeDefCounterAssignments( wstream,
                            record.deftoken, n, array, 0 ) == 0 );

                    delete [] array;
                 }
              }

              break;
            }
            case DEF_REC_TYPE__DefKeyValue:
            {
               // resort key-value definitions
               //

               typedef
                  std::set<const DefRec_DefKeyValueS*,
                     DefRec_DefKeyValueS::SortS> resorted_keyvals_t;

               resorted_keyvals_t resorted_keyvals;

               resortGlobDefs<DefRec_DefKeyValueS>(
                  m_globDefs.keyVals, resorted_keyvals );

               // iterate over all key-value definitions
               for( resorted_keyvals_t::const_iterator it =
                    resorted_keyvals.begin(); it != resorted_keyvals.end();
                    ++it )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefKeyValueS record = **it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefKeyValue,
                     5, &wstream, &(record.deftoken), &(record.type),
                     &(record.name), &do_write );

                  // write record
                  //
                  if( do_write )
                  {
                     error =
                        ( OTF_WStream_writeDefKeyValue( wstream,
                             record.deftoken, record.type,
                             record.name.c_str(),
                             "" /* description */ ) == 0 );
                  }
               }

               break;
            }
            default:
            {
               break;
            }
         }
      }

   } while( false );

   // show an error message, if necessary
   //
   if( error )
   {
      std::cerr << ExeName << ": Error: "
                << "Could not write global definitions to OTF stream [namestub "
                << tmp_out_file_prefix.c_str() << " id 0]" << std::endl;
   }

#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
   if( !error )
   {
      // trigger generic hooks for closing definition writer stream
      theHooks->triggerGenericHook(
         VT_UNIFY_HOOKS_MSGMATCH_SNAPS_GENID__DEF_WSTREAM_CLOSE, 1,
         &wstream );
   }
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

   // close writer stream
   OTF_WStream_close( wstream );
   // close file manager for writer stream
   OTF_FileManager_close( manager );

   VPrint( 3, "  Closed OTF writer stream [namestub %s id 0]\n",
           tmp_out_file_prefix.c_str() );

   return !error;
}

//////////////////// sub-class DefinitionsC::CommentsC ////////////////////

// public methods
//

bool
DefinitionsC::CommentsC::processLocal( const DefRec_DefCommentS & locComment )
{
   bool error = false;

   std::istringstream iss( locComment.comment );
   vt_assert( iss );

   switch( locComment.type )
   {
      case DefRec_DefCommentS::TYPE_START_TIME:
      {
         // get minimum start time from comment
         //
         uint64_t starttime;
         iss >> starttime;
         vt_assert( iss );

         // update minimum start time, if necessary
         if( starttime < m_minStartTimeEpoch )
           m_minStartTimeEpoch = starttime;

         break;
      }
      case DefRec_DefCommentS::TYPE_STOP_TIME:
      {
         // get maximum stop time from comment
         //
         uint64_t stoptime;
         iss >> stoptime;
         vt_assert( iss );

         // update maximum stop time, if necessary
         if( stoptime > m_maxStopTimeEpoch )
           m_maxStopTimeEpoch = stoptime;

         break;
      }
      case DefRec_DefCommentS::TYPE_USRCOM_SEND:
      case DefRec_DefCommentS::TYPE_USRCOM_RECV:
      {
         // get peer process id, local communicator token, and tag from comment
         //

         uint32_t peer = locComment.loccpuid;
         uint32_t comm;
         uint32_t tag;
         char delim;

         for( uint8_t i = 0; i < 4; i++ )
         {
            switch( i )
            {
               case 0: // 'C'
               {
                  iss >> delim;
                  vt_assert( iss );
                  vt_assert( delim == 'C' );
                  break;
               }
               case 1: // communicator token
               {
                  iss >> std::hex >> comm;
                  vt_assert( iss );
                  break;
               }
               case 2: // 'T'
               {
                  iss >> delim;
                  vt_assert( iss );
                  vt_assert( delim == 'T' );
                  break;
               }
               case 3: // tag
               default:
               {
                  iss >> std::hex >> tag;
                  vt_assert( iss );
                  break;
               }
            }
         }

         // get global token factory for DefProcessGroup
         TokenFactoryScopeI * tkfac_defprocgrp =
            theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

         // translate local comm. token
         //
         uint32_t global_comm =
            tkfac_defprocgrp->translate( locComment.loccpuid, comm );
         vt_assert( global_comm != 0 );

         // add process id to members of user communicator
         m_defs.m_procGrps->m_userCom.addCommMember( global_comm,
            locComment.loccpuid );

         // register communication id and its peer
         //
         if( locComment.type == DefRec_DefCommentS::TYPE_USRCOM_SEND )
         {
            theUserCom->addSender( UserComC::ComIdS( global_comm, tag ),
               peer );
         }
         else // locComment.type == DefRec_DefCommentS::TYPE_USRCOM_RECV
         {
            theUserCom->addReceiver( UserComC::ComIdS( global_comm, tag ),
               peer );
         }

         break;
      }
      case DefRec_DefCommentS::TYPE_VT:
      case DefRec_DefCommentS::TYPE_USER:
      {
         // get reference to global definition comments
         std::set<DefRec_DefCommentS> & glob_comments =
            m_defs.m_globDefs.comments;

         // create new comment
         DefRec_DefCommentS new_comment = locComment;

         // user comment?
         if( locComment.type == DefRec_DefCommentS::TYPE_USER )
         {
            static bool first_user_comment = true;

            // create headline for user comments, if it's the first one
            //
            if( first_user_comment )
            {
               glob_comments.insert( DefRec_DefCommentS( 0, m_seqOrderIdx++,
                  DefRec_DefCommentS::TYPE_USER, "User Comments:" ) );
               first_user_comment = false;
            }

            // indent comment
            new_comment.comment = std::string( " " ) + locComment.comment;
         }

         // get order index
         new_comment.deftoken = m_seqOrderIdx++;

         // search for already created global definition comment
         std::set<DefRec_DefCommentS>::const_iterator it =
            glob_comments.find( new_comment );

         // add global definition comment to set, if not found
         if( it == glob_comments.end() )
            glob_comments.insert( new_comment );

         break;
      }
      default: // DefRec_DefCommentS::TYPE_UNKNOWN
      {
         vt_assert( 0 );
      }
   }

   return !error;
}

bool
DefinitionsC::CommentsC::finish( void )
{
   bool error = false;

   // add time comments to global definitions, if present
   //
   if( m_minStartTimeEpoch != (uint64_t)-1 && m_maxStopTimeEpoch != 0 )
   {
#ifdef VT_UNIFY_HOOKS_TDB
      // trigger HooksTdbC's generic hook to set trace times
      theHooks->triggerGenericHook(
         VT_UNIFY_HOOKS_TDB_GENID__STARTSTOPTIME_EPOCH,
         2, &m_minStartTimeEpoch, &m_maxStopTimeEpoch );
#endif // VT_UNIFY_HOOKS_TDB

      // get reference to global definition comments
      std::set<DefRec_DefCommentS> & glob_comments = m_defs.m_globDefs.comments;

      // add trace time comments to global definitions
      // (0=headline, 1=start time, 2=stop time, 3=elasped time)
      //
      for( uint32_t i = 0; i < 4; i++ )
      {
         DefRec_DefCommentS new_comment;

         // set comment's type and order index
         //
         new_comment.type = DefRec_DefCommentS::TYPE_START_TIME;
         new_comment.deftoken = m_seqOrderIdx++;

         // compose/set comment's text
         //

         switch( i )
         {
            case 0: // headline
            {
               new_comment.comment = "Trace Times:";
               break;
            }
            case 1: // min. start time
            case 2: // max. stop time
            {
               time_t tt;
               std::ostringstream ss;

               if( i == 1 )
               {
                  tt = (time_t)(m_minStartTimeEpoch / 1e6);
                  ss << " Start: " << asctime(localtime(&tt)) << "("
                     << m_minStartTimeEpoch << ")";
               }
               else // i == 2
               {
                  tt = (time_t)(m_maxStopTimeEpoch / 1e6);
                  ss << " Stop: " << asctime(localtime(&tt)) << "("
                     << m_maxStopTimeEpoch << ")";
               }

               new_comment.comment = ss.str();
               ss.str(""); ss.clear();

               break;
            }
            default: // elapsed time
            {
               time_t tt;
               struct tm elapsed_tm;
               std::ostringstream ss;

               tt =
                  (time_t)((m_maxStopTimeEpoch - m_minStartTimeEpoch) / 1e6);
               gmtime_r(&tt, &elapsed_tm);
               ss << " Elapsed: "
                  << (elapsed_tm.tm_hour < 10 ? "0" : "")
                  << elapsed_tm.tm_hour << ":"
                  << (elapsed_tm.tm_min  < 10 ? "0" : "")
                  << elapsed_tm.tm_min << ":"
                  << (elapsed_tm.tm_sec  < 10 ? "0" : "")
                  << elapsed_tm.tm_sec
                  << " (" << m_maxStopTimeEpoch - m_minStartTimeEpoch << ")";

               new_comment.comment = ss.str();
               ss.str(""); ss.clear();

               break;
            }
         }

         // add comment to global definitions
         glob_comments.insert( new_comment );
      }
   }

   return !error;
}

//////////////////// sub-class DefinitionsC::ProcessGroupsC ////////////////////

// public methods
//

bool
DefinitionsC::ProcessGroupsC::processLocal(
   DefRec_DefProcessGroupS & locProcGrp )
{
   bool error = false;

   // get global token factory for DefProcessGroup
   static TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // handle local process group depending on its type
   //
   switch( locProcGrp.type )
   {
      case DefRec_DefProcessGroupS::TYPE_MPI_COMM_WORLD:
      {
         // create global process group token, if not already done
         if( m_mpi.worldComm.global_token == 0 )
            m_mpi.worldComm.global_token = tkfac_defprocgrp->getNextToken();

         // create global process group definition, only if members are present
         // (only the first available (not disabled) process contains a
         // filled process group for MPI_COMM_WORLD)
         //
         if( locProcGrp.nmembers > 0 )
         {
            // deflate process group members
            deflateMembers( locProcGrp );

            // set process group name
            locProcGrp.name = MpiS::WorldCommS::NAME();

            // create global process group definition with previous created
            // global token
            tkfac_defprocgrp->create( &locProcGrp,
               m_mpi.worldComm.global_token );
         }
         // otherwise, set token translation for process
         //
         else
         {
            tkfac_defprocgrp->setTranslation( locProcGrp.loccpuid,
               locProcGrp.deftoken, m_mpi.worldComm.global_token );
         }

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_MPI_COMM_SELF:
      {
         // process group member = id of defining process
         const uint32_t member = locProcGrp.loccpuid & VT_TRACEID_BITMASK;

         // compose and set process group name
         //
         std::ostringstream name;
         name << MpiS::SelfCommsS::NAME() << " " << member - 1;
         locProcGrp.name = name.str();

         // set process group member
         locProcGrp.assignMembers( 1, &member, &member + 1 );

         // create global process group definition
         tkfac_defprocgrp->create( &locProcGrp );

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_MPI_COMM_OTHER:
      case DefRec_DefProcessGroupS::TYPE_MPI_GROUP:
      {
	 const bool is_mpi_group =
	    ( locProcGrp.type == DefRec_DefProcessGroupS::TYPE_MPI_GROUP );

         // process group must have members
         vt_assert( locProcGrp.nmembers > 0 );

         // deflate process group members and get its unique id
         //
         deflateMembers( locProcGrp );
         const uint32_t membersid = locProcGrp.members[1];

         // get count by process and members (0 if it's a group)
         const uint32_t count =
	    is_mpi_group ? 0 : ++(m_mpi.commsAndGroups.counts
               [ std::make_pair( locProcGrp.loccpuid, membersid ) ]);

         // get global token by members and count
         uint32_t & global_token = m_mpi.commsAndGroups.global_tokens
            [ std::make_pair( membersid, count ) ];

         // create global process group definition, if not already done
         //
         if( global_token == 0 )
         {
            // compose and set process group name
            //
            std::ostringstream name;
	    if( is_mpi_group )
	    {
	       name << MpiS::CommsAndGroupsS::GROUP_NAME() << " "
	            << m_mpi.commsAndGroups.group_seqno++;
            }
	    else
	    {
	       name << MpiS::CommsAndGroupsS::COMM_NAME() << " "
	            << m_mpi.commsAndGroups.comm_seqno++;
	    }
            locProcGrp.name = name.str();

            // create global process group definition and store its token
            global_token = tkfac_defprocgrp->create( &locProcGrp );
         }
         // otherwise, set token translation for process
         //
         else
         {
            tkfac_defprocgrp->setTranslation( locProcGrp.loccpuid,
               locProcGrp.deftoken, global_token );
         }

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_USER_COMM:
      {
         // search for communicator by its name
         std::map<std::string, UserComS::CommS*>::iterator it =
            m_userCom.name2Comm.find( locProcGrp.name );

         // add communicator, if not found
         //
         if( it == m_userCom.name2Comm.end() )
         {
            // create global communicator token
            uint32_t global_token = tkfac_defprocgrp->getNextToken();

            it =
               m_userCom.name2Comm.insert( std::make_pair( locProcGrp.name,
                  new UserComS::CommS() ) ).first;
            vt_assert( it->second );

            it->second->global_token = global_token;

            m_userCom.globTk2Comm[global_token] = it->second;

            // register global communicator token
            theUserCom->addUserComm( global_token );
         }

         // set token translation for process
         tkfac_defprocgrp->setTranslation( locProcGrp.loccpuid,
            locProcGrp.deftoken, it->second->global_token );

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_ALL:
      case DefRec_DefProcessGroupS::TYPE_NODE:
      case DefRec_DefProcessGroupS::TYPE_OTHER:
      {
         if( locProcGrp.type == DefRec_DefProcessGroupS::TYPE_ALL )
         {
            // set process group name
            locProcGrp.name = OtherS::ALL_NAME();
         }
         else if( locProcGrp.type == DefRec_DefProcessGroupS::TYPE_NODE )
         {
            // add member process ids to the "All" process group
            m_other.name2Group[OtherS::ALL_NAME()].members.insert(
               locProcGrp.members, locProcGrp.members + locProcGrp.nmembers );
         }

         // get reference to process group by name
         OtherS::GroupS & group = m_other.name2Group[locProcGrp.name];

         // create global process group token, if not already done
         if( group.global_token == 0 )
            group.global_token = tkfac_defprocgrp->getNextToken();

         // set token translation for process
         tkfac_defprocgrp->setTranslation( locProcGrp.loccpuid,
            locProcGrp.deftoken, group.global_token );

         // add member process ids
         // (the members of the "All" process group will be collected from the
         //  node process groups)
         //
         if( locProcGrp.type != DefRec_DefProcessGroupS::TYPE_ALL )
         {
            // add id of defining process, if no members are present
            //
            if( locProcGrp.nmembers == 0 )
            {
               group.members.insert( locProcGrp.loccpuid );
            }
            else
            {
               group.members.insert( locProcGrp.members,
                  locProcGrp.members + locProcGrp.nmembers );
            }
         }

         break;
      }
      default: // DefRec_DefProcessGroupS::TYPE_UNKNOWN
      {
         vt_assert( 0 );
      }
   }

   return !error;
}

bool
DefinitionsC::ProcessGroupsC::finish( void )
{
   bool error = false;

   // get global token factory for DefProcessGroup
   TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // add user communicator groups to global definitions
   //
   if( !m_userCom.name2Comm.empty() )
   {
      // initialize new process group definition
      //
      DefRec_DefProcessGroupS new_proc_grp;
      new_proc_grp.type = DefRec_DefProcessGroupS::TYPE_USER_COMM;

      for( std::map<std::string, UserComS::CommS*>::iterator comm_it =
           m_userCom.name2Comm.begin(); comm_it != m_userCom.name2Comm.end();
           ++comm_it )
      {
         vt_assert( comm_it->second->global_token != 0 );

         new_proc_grp.name = comm_it->first;
         new_proc_grp.assignMembers( comm_it->second->members.size(),
            comm_it->second->members.begin(), comm_it->second->members.end() );

         // create global definition with previous created global token
         tkfac_defprocgrp->create( &new_proc_grp,
            comm_it->second->global_token );

         delete comm_it->second;
      }

      m_userCom.name2Comm.clear();
      m_userCom.globTk2Comm.clear();
   }

   // add other process groups to global definitions
   //
   if( !m_other.name2Group.empty() )
   {
      // initialize new process group definition
      //
      DefRec_DefProcessGroupS new_proc_grp;
      new_proc_grp.type = DefRec_DefProcessGroupS::TYPE_OTHER;

      for( std::map<std::string, OtherS::GroupS>::const_iterator group_it =
           m_other.name2Group.begin();
           group_it != m_other.name2Group.end(); ++group_it )
      {
         vt_assert( group_it->second.global_token != 0 );

         new_proc_grp.name = group_it->first;
         new_proc_grp.assignMembers( group_it->second.members.size(),
            group_it->second.members.begin(), group_it->second.members.end() );

         // create global definition with previous created global token
         tkfac_defprocgrp->create( &new_proc_grp,
            group_it->second.global_token );
      }

      m_other.name2Group.clear();
   }

   return !error;
}

void
DefinitionsC::ProcessGroupsC::deflateMembers(
   DefRec_DefProcessGroupS & procGrp )
{
   // return, if input group member array is empty or already deflated
   if( procGrp.nmembers == 0 || procGrp.members[0] == DEFLATED_MEMBERS_TAG )
      return;

   // search for already deflated group member array and get its unique id
   //

   std::pair<std::multimap<uint32_t, UniqueMembersS*>::const_iterator,
      std::multimap<uint32_t, UniqueMembersS*>::const_iterator> range =
         m_hash2UniqueMembers.equal_range( procGrp.members_hash );

   uint32_t id = (uint32_t)-1;

   for( std::multimap<uint32_t, UniqueMembersS*>::const_iterator it =
        range.first; it != range.second; ++it )
   {
      if( it->second->nmembers != procGrp.nmembers )
         continue;

      if( memcmp( it->second->members, procGrp.members,
          procGrp.nmembers * sizeof( uint32_t ) ) == 0 )
      {
         id = it->second->id;
         break;
      }
   }

   // if not found, create new unique id and assign input group member
   // array to it
   //
   if( id == (uint32_t)-1 )
   {
      id = m_uniqueMembers.size();

      UniqueMembersS * new_unique_members =
         new UniqueMembersS( id, procGrp.nmembers, procGrp.members );
      vt_assert( new_unique_members );

      m_uniqueMembers.push_back( new_unique_members );
      m_hash2UniqueMembers.insert(
         std::make_pair( procGrp.members_hash, new_unique_members ) );
   }

   // deflate input group member array
   //
   delete [] procGrp.members;
   procGrp.nmembers = 2;
   procGrp.members = new uint32_t[2];
   vt_assert( procGrp.members );
   procGrp.members[0] = DEFLATED_MEMBERS_TAG;
   procGrp.members[1] = id;
}

void
DefinitionsC::ProcessGroupsC::inflateMembers(
   DefRec_DefProcessGroupS & procGrp )
{
   // return, if input group member array is empty or not deflated
   if( procGrp.nmembers == 0 || procGrp.members[0] != DEFLATED_MEMBERS_TAG )
      return;

   vt_assert( procGrp.nmembers == 2 );

   // get unique id of deflated input group member array
   //
   uint32_t id = procGrp.members[1];
   vt_assert( id < m_uniqueMembers.size() );

   // inflate input group member array
   //
   delete [] procGrp.members;
   procGrp.nmembers = m_uniqueMembers[id]->nmembers;
   procGrp.members = new uint32_t[procGrp.nmembers];
   vt_assert( procGrp.members );
   memcpy( procGrp.members, m_uniqueMembers[id]->members,
      procGrp.nmembers * sizeof( uint32_t ) );
}

