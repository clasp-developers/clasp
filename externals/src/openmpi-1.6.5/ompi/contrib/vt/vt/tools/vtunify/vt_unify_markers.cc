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

#include "vt_unify_handlers.h"
#include "vt_unify_hooks.h"
#include "vt_unify_markers.h"
#include "vt_unify_sync.h"

#include "otf.h"

#include <algorithm>
#include <iostream>

MarkersC * theMarkers = 0; // instance of class MarkersC

//////////////////// class MarkersC ////////////////////

// public methods
//

MarkersC::MarkersC() : m_tkfacScope( 0 )
{
   MASTER
   {
      // create global token factory scope for marker definitions
      //
      m_tkfacScope =
         new TokenFactoryScopeC<DefRec_DefMarkerS>( &m_globDefs );
      vt_assert( m_tkfacScope );
   }
}

MarkersC::~MarkersC()
{
   MASTER
   {
      // delete global token factory scope for marker definitions
      delete m_tkfacScope;
   }
}

bool
MarkersC::run()
{
   bool error = false;

#ifdef VT_MPI
   // block until all ranks have reached this point
   if( NumRanks > 1 )
      CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );
#endif // VT_MPI

   VPrint( 1, "Unifying markers\n" );

   // trigger phase pre hook
   theHooks->triggerPhaseHook( HooksC::Phase_UnifyMarkers_pre );

   do
   {
      // read local markers
      //
      error = !readLocal();
      if( SyncError( &error ) )
         break;

      MASTER
      {
         // write global markers, if present
         if( !m_globDefs.empty() || !m_globSpots.empty() )
            error = !writeGlobal();
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
                   << "An error occurred during unifying markers. Aborting."
                   << std::endl;
      }
   }

   // trigger phase post hook, if no error occurred
   //
   if( !error )
      theHooks->triggerPhaseHook( HooksC::Phase_UnifyMarkers_post );

   return !error;
}

bool
MarkersC::cleanUp()
{
   bool error = false;

   char filename1[STRBUFSIZE];
   char filename2[STRBUFSIZE];

   // base file type
   const OTF_FileType base_file_type = OTF_FILETYPE_MARKER;

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

            // NOTE: IOFSL handling not needed here, global marker files
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
         // NOTE: IOFSL handling not needed here, global marker files
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
         if( rename( filename1, filename2 ) == 0 )
            VPrint( 3, " Renamed %s to %s\n", filename1, filename2 );
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
MarkersC::readLocal()
{
   bool error = false;

   VPrint( 2, " Reading local markers\n" );

   // vector of local marker definitions
   LargeVectorC<DefRec_DefMarkerS*> loc_defs;

   // vector of local marker spots
   LargeVectorC<MarkerSpotS*> loc_spots;

   do
   {
      // TODO: parallelize this loop with OpenMP
      for( uint32_t i = 0; i < MyStreamIds.size(); i++ )
      {
         const uint32_t & streamid = MyStreamIds[i];

         // read local markers of stream
         if( (error = !readLocal( streamid, loc_defs, loc_spots )) )
            break;
      }
      if( SyncError( &error ) )
         break;

      // process local marker definitions
      //

#ifdef VT_MPI
      if( NumRanks > 1 )
      {
         // gather local marker definitions from all ranks
         //
         error = !gatherLocal( GATHER_TYPE_DEFS, &loc_defs );
//         if( SyncError( &error ) )
//            break;

         // local marker definitions not needed anymore on slave ranks;
         // delete them
         //
         SLAVE
         {
            for( uint32_t i = 0; i < loc_defs.size(); i++ )
               delete loc_defs[i];
            loc_defs.clear();
         }
      }
#endif // VT_MPI

      MASTER
      {
         // create global marker definitions
         //
         for( uint32_t i = 0; i < loc_defs.size(); i++ )
         {
            // create global definition
            m_tkfacScope->create( loc_defs[i] );
            // delete local definition
            delete loc_defs[i];
         }
         // clear vector of local marker definitions
         loc_defs.clear();
      }

      // process local marker spots
      //

#ifdef VT_MPI
      if( NumRanks > 1 )
      {
         // gather local marker spots from all ranks
         //
         error = !gatherLocal( GATHER_TYPE_SPOTS, &loc_spots );
         if( SyncError( &error ) )
            break;

         // local marker spots not needed anymore on slave ranks;
         // delete them
         //
         SLAVE
         {
            for( uint32_t i = 0; i < loc_spots.size(); i++ )
               delete loc_spots[i];
            loc_spots.clear();
         }
      }
#endif // VT_MPI

      MASTER
      {
         // create global marker spots
         //
         for( uint32_t i = 0; i < loc_spots.size(); i++ )
         {
            MarkerSpotS new_glob_spot = *(loc_spots[i]);

            // correct time
            new_glob_spot.time =
               theTimeSync->correctTime( new_glob_spot.proc,
                  new_glob_spot.time );

            // correct marker token
            new_glob_spot.marker =
               m_tkfacScope->translate
                  ( new_glob_spot.proc, new_glob_spot.marker );
            vt_assert( new_glob_spot.marker != 0 );

            // add new global marker spot to vector
            m_globSpots.push_back( new_glob_spot );

            // delete local spot
            delete loc_spots[i];
         }
         // clear vector of local marker spots
         loc_spots.clear();

         // sort global marker spots
         std::stable_sort( m_globSpots.begin(), m_globSpots.end(),
                           std::less<MarkerSpotS>() );
      }

   } while( false );

   return !error;
}

bool
MarkersC::readLocal( const uint32_t & streamId,
    LargeVectorC<DefRec_DefMarkerS*> & locDefs,
    LargeVectorC<MarkerSpotS*> & locSpots )
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
      // try to get markers buffer
      //
      if( !OTF_RStream_getMarkerBuffer( rstream ) )
      {
         PVPrint( 3, "   No markers found in this OTF reader stream "
                     "- Ignored\n" );
         break;
      }

      // close markers buffer
      OTF_RStream_closeMarkerBuffer( rstream );

      // create record handler array
      //
      OTF_HandlerArray * handler_array = OTF_HandlerArray_open();
      vt_assert( handler_array );

      // create first handler argument
      FirstHandlerArg_MarkersS fha( locDefs, locSpots );

      // set record handler and its first argument for ...
      //

      // ... OTF_DEFMARKER_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleDefMarker,
         OTF_DEFMARKER_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_DEFMARKER_RECORD );

      // ... OTF_MARKER_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)HandleMarkerSpot,
         OTF_MARKER_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &fha, OTF_MARKER_RECORD );

      // read local markers
      //
      if( OTF_RStream_readMarker( rstream, handler_array ) ==
          OTF_READ_ERROR )
      {
         std::cerr << ExeName << ": Error: "
                   << "Could not read markers of OTF stream [namestub "
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
MarkersC::writeGlobal()
{
   bool error = false;

   VPrint( 2, " Writing global markers\n" );

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
      // resort marker definitions
      //

      typedef
         std::set<const DefRec_DefMarkerS*, DefRec_DefMarkerS::SortS>
            resorted_markers_t;

      resorted_markers_t resorted_markers;

      for( std::set<DefRec_DefMarkerS>::const_iterator it =
           m_globDefs.begin(); it != m_globDefs.end(); ++it )
      {
         resorted_markers.insert( &(*it) );
      }

      // write global marker definition records
      //

      // iterate over all marker definitions
      for( resorted_markers_t::const_iterator it = resorted_markers.begin();
           it != resorted_markers.end(); ++it )
      {
         bool do_write = true;

         // get copy of marker def. record in order that hook(s) can
         // modify it
         DefRec_DefMarkerS record = **it;

         // trigger write record hook
         theHooks->triggerWriteRecordHook( HooksC::Record_DefMarker, 5,
            &wstream, &(record.deftoken), &(record.type), &(record.name),
            &do_write );

         // write record
         if( do_write )
            error = ( OTF_WStream_writeDefMarker( wstream, record.deftoken,
                         record.name.c_str(), record.type ) == 0 );
      }

      // write global marker spot records
      //
      for( uint32_t i = 0; i < m_globSpots.size() && !error; i++ )
      {
         bool do_write = true;

         // reference to marker spot record
         MarkerSpotS & record = m_globSpots[i];

         // trigger write record hook
         theHooks->triggerWriteRecordHook( HooksC::Record_MarkerSpot, 6,
            &wstream, &(record.time), &(record.proc), &(record.marker),
            &(record.text), &do_write );

         // write record
         if( do_write )
            error = ( OTF_WStream_writeMarker( wstream, record.time, record.proc,
                         record.marker, record.text.c_str() ) == 0 );
      }

   } while( false );

   // show an error message, if necessary
   //
   if( error )
   {
      std::cerr << ExeName << ": Error: "
                << "Could not write global markers to OTF stream [namestub "
                << tmp_out_file_prefix.c_str() << " id 0]" << std::endl;
   }

   // close writer stream
   OTF_WStream_close( wstream );
   // close file manager for writer stream
   OTF_FileManager_close( manager );

   VPrint( 3, "  Closed OTF writer stream [namestub %s id 0]\n",
           tmp_out_file_prefix.c_str() );

   return !error;
}

#ifdef VT_MPI

bool
MarkersC::gatherLocal( const GatherTypeT & type, void * locRecs )
{
   bool error = false;

   vt_assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   // cast input vector for more convenient access
   //

   LargeVectorC<DefRec_DefMarkerS*> * loc_defs = 0;
   LargeVectorC<MarkerSpotS*> * loc_spots = 0;

   if( type == GATHER_TYPE_DEFS )
   {
      loc_defs = static_cast<LargeVectorC<DefRec_DefMarkerS*>*>( locRecs );
      VPrint( 2, " Gathering local marker definitions\n" );
   }
   else // type == GATHER_TYPE_SPOTS
   {
      loc_spots = static_cast<LargeVectorC<MarkerSpotS*>*>( locRecs );
      VPrint( 2, " Gathering local marker spots\n" );
   }

   uint32_t loc_recs_size;

   char * send_buffer;
   VT_MPI_INT send_buffer_size;
   VT_MPI_INT send_buffer_pos;

   // get size needed for the send buffer
   //

   // locRecs.size()
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD,
                            &send_buffer_size ) );

   SLAVE
   {
      // locRecs (loc_defs/loc_spots)
      //
      if( type == GATHER_TYPE_DEFS )
      {
         for( uint32_t i = 0; i < loc_defs->size(); i++ )
            send_buffer_size += (*loc_defs)[i]->getPackSize();
      }
      else // type == GATHER_TYPE_SPOTS
      {
         for( uint32_t i = 0; i < loc_spots->size(); i++ )
            send_buffer_size += (*loc_spots)[i]->getPackSize();
      }
   }

   // allocate memory for the send buffer
   //
   send_buffer = new char[send_buffer_size];
   vt_assert( send_buffer );

   // pack send buffer
   //

   send_buffer_pos = 0;

   // locRecs.size()
   //
   if( type == GATHER_TYPE_DEFS )
   {
      loc_recs_size = loc_defs->size();
   }
   else // type == GATHER_TYPE_SPOTS
   {
      loc_recs_size = loc_spots->size();
   }

   CALL_MPI( MPI_Pack( &loc_recs_size, 1, MPI_UNSIGNED, send_buffer,
                       send_buffer_size, &send_buffer_pos, MPI_COMM_WORLD ) );

   SLAVE
   {
      // locRecs (loc_defs/loc_spots)
      //
      if( type == GATHER_TYPE_DEFS )
      {
         for( uint32_t i = 0; i < loc_defs->size(); i++ )
            (*loc_defs)[i]->pack( send_buffer, send_buffer_size,
                                  send_buffer_pos );
      }
      else // type == GATHER_TYPE_SPOTS
      {
         for( uint32_t i = 0; i < loc_spots->size(); i++ )
            (*loc_spots)[i]->pack( send_buffer, send_buffer_size,
                                   send_buffer_pos );
      }
   }

   char * recv_buffer = 0;
   VT_MPI_INT recv_buffer_size = 0;
   VT_MPI_INT * recv_buffer_sizes = 0;
   VT_MPI_INT * recv_buffer_displs = 0;

   MASTER
   {
      // allocate memory for the receive buffer sizes
      //
      recv_buffer_sizes = new VT_MPI_INT[NumRanks];
      vt_assert( recv_buffer_sizes );
   }

   // gather buffer sizes
   CALL_MPI( MPI_Gather( &send_buffer_size, 1, MPI_INT, recv_buffer_sizes, 1,
                         MPI_INT, 0, MPI_COMM_WORLD ) );

   MASTER
   {
      // allocate memory for displacements
      //
      recv_buffer_displs = new VT_MPI_INT[NumRanks];
      vt_assert( recv_buffer_displs );

      // compute displacements and receive buffer size
      //
      for( VT_MPI_INT i = 0; i < NumRanks; i++ )
      {
         recv_buffer_size += recv_buffer_sizes[i];
         recv_buffer_displs[i] = 0;
         if( i > 0 )
         {
            recv_buffer_displs[i] =
               recv_buffer_displs[i-1] + recv_buffer_sizes[i-1];
         }
      }

      // allocate memory for the receive buffer
      //
      recv_buffer = new char[recv_buffer_size];
      vt_assert( recv_buffer );
   }

   // gather packed local marker definitions
   CALL_MPI( MPI_Gatherv( send_buffer, send_buffer_size, MPI_PACKED,
                          recv_buffer, recv_buffer_sizes, recv_buffer_displs,
                          MPI_PACKED, 0, MPI_COMM_WORLD ) );

   // free memory of send buffer
   delete [] send_buffer;

   MASTER
   {
      // unpack receive buffer
      //
      for( VT_MPI_INT i = 1; i < NumRanks; i++ )
      {
         char * buffer = recv_buffer + recv_buffer_displs[i];
         VT_MPI_INT buffer_size = recv_buffer_sizes[i];
         VT_MPI_INT buffer_pos = 0;

         // locDefs.size()
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos,
                               &loc_recs_size, 1, MPI_UNSIGNED,
                               MPI_COMM_WORLD ) );

         // locDefs
         //
         for( uint32_t j = 0; j < loc_recs_size; j++ )
         {
            if( type == GATHER_TYPE_DEFS )
            {
               DefRec_DefMarkerS * new_def = new DefRec_DefMarkerS();
               new_def->unpack( buffer, buffer_size, buffer_pos );

               loc_defs->push_back( new_def );
            }
            else // type == GATHER_TYPE_SPOTS
            {
               MarkerSpotS * new_spot = new MarkerSpotS();
               new_spot->unpack( buffer, buffer_size, buffer_pos );

               loc_spots->push_back( new_spot );
            }
         }
      }

      // free some memory
      delete [] recv_buffer;
      delete [] recv_buffer_sizes;
      delete [] recv_buffer_displs;
   }

   return !error;
}

#endif // VT_MPI

//////////////////// struct MarkersC::MarkerSpotS ////////////////////

#ifdef VT_MPI

VT_MPI_INT
MarkersC::MarkerSpotS::getPackSize()
{
   VT_MPI_INT buffer_size = 0;
   VT_MPI_INT size;

   // proc + marker
   CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // time
   CALL_MPI( MPI_Pack_size( 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // text.length()
   uint32_t text_length = text.length();
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // text
   CALL_MPI( MPI_Pack_size( text_length + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
MarkersC::MarkerSpotS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                             VT_MPI_INT & bufferPos )
{
   // proc
   CALL_MPI( MPI_Pack( &proc, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );

   // time
   CALL_MPI( MPI_Pack( &time, 1, MPI_LONG_LONG_INT, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // marker
   CALL_MPI( MPI_Pack( &marker, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // text.length()
   uint32_t text_length = text.length();
   CALL_MPI( MPI_Pack( &text_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // text
   char * c_text = new char[text_length+1];
   strcpy( c_text, text.c_str() );
   CALL_MPI( MPI_Pack( c_text, text_length + 1, MPI_CHAR, buffer,
                       bufferSize, &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_text;
}

void
MarkersC::MarkerSpotS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                               VT_MPI_INT & bufferPos )
{
   // proc
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &proc, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // time
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &time, 1,
                         MPI_LONG_LONG_INT, MPI_COMM_WORLD ) );

   // marker
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &marker, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // text.length()
   uint32_t text_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &text_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // text
   char * c_text = new char[text_length+1];
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_text,
                         text_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   text = c_text;
   delete [] c_text;
}

#endif // VT_MPI
