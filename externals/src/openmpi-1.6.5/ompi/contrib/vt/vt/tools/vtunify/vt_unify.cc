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

#include "vt_unify.h"
#include "vt_unify_defs.h"
#include "vt_unify_events_stats.h"
#include "vt_unify_hooks.h"
#include "vt_unify_markers.h"
#include "vt_unify_sync.h"
#include "vt_unify_tkfac.h"
#include "vt_unify_usrcom.h"

#ifdef VT_LIB
#  include "vt_unify_lib.h"
#  define VTUNIFY_MAIN VTUnify
#else // VT_LIB
#  define VTUNIFY_MAIN main
#endif // VT_LIB

#include "otf.h"

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

// local functions
//

// get unify parameters
static bool getParams( int argc, char ** argv );

// get unify controls
static bool getUnifyControls( void );

// parse command line options
static bool parseCommandLine( int argc, char ** argv );

#if defined(HAVE_IOFSL) && HAVE_IOFSL
   // parse comma-separated list of IOFSL server addresses
   static bool parseIofslServerList( char* list );
#endif // HAVE_IOFSL

// write OTF master control file
static bool writeMasterControl( void );

// clean up (e.g. rename temporary output files)
static bool cleanUp( void );

// show usage text
static void showUsage( void );

#ifdef VT_MPI
   // share unify parameters to all ranks
   static bool shareParams( void );

   // share unify controls to all ranks
   static bool shareUnifyControls( void );
#endif // VT_MPI

// local variables
//

// output stream for verbose messages
// (stdout if vtunify is started from the command line, otherwise stderr)
static FILE * verboseStream = stderr;

// global variables
//

// name of program's executable
//
#ifdef VT_MPI
#  ifdef VT_LIB
      const std::string ExeName = "libvt-mpi-unify";
#  else // VT_LIB
      const std::string ExeName = "vtunify-mpi";
#  endif // VT_LIB
#else // VT_MPI
   const std::string ExeName = "vtunify";
#endif // VT_MPI

// temporary output file suffix
const std::string TmpFileSuffix = "__ufy.tmp";

// output file prefix which used if local input files shall be kept
const std::string UniFilePrefix = "u_";

// unify parameters
ParamsS Params;

// vector of unify controls
std::vector<UnifyControlS*> UnifyCtls;

// map stream id <-> unify control
std::map<uint32_t, UnifyControlS*> StreamId2UnifyCtl;

// vector of stream ids to process by my rank
std::vector<uint32_t> MyStreamIds;

// number of available streams
uint32_t NumAvailStreams = 0;

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
   // set of absent stream ids (for fast searching)
   std::set<uint32_t> AbsentStreamIds;
#endif /* VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES */

#ifdef VT_MPI
   // number of MPI-ranks
   VT_MPI_INT NumRanks;

   // MPI-rank of calling process
   VT_MPI_INT MyRank;

   // map stream id <-> processing MPI-rank
   std::map<uint32_t, VT_MPI_INT> StreamId2Rank;

   // map MPI-rank <-> stream ids
   std::map<VT_MPI_INT, std::set<uint32_t> > Rank2StreamIds;
#endif // VT_MPI

// VT mode flags bitmask (VT_MODE_TRACE and/or VT_MODE_STAT)
uint32_t UnifyControlS::mode_flags = (uint32_t)-1;

// unify control's IOFSL properties
//
uint32_t UnifyControlS::iofsl_num_servers = (uint32_t)-1;
uint32_t UnifyControlS::iofsl_mode = (uint32_t)-1;

int
VTUNIFY_MAIN( int argc, char ** argv )
{
   bool error = false;

#ifdef VT_MPI
   // initialize MPI
   //
#  ifndef VT_LIB
   CALL_MPI( MPI_Init( (VT_MPI_INT*)&argc, &argv ) );
#  endif // !VT_LIB
   CALL_MPI( MPI_Comm_size( MPI_COMM_WORLD, &NumRanks ) );
   CALL_MPI( MPI_Comm_rank( MPI_COMM_WORLD, &MyRank ) );
#endif // VT_MPI

   // create instance of classes ...
   //

   // ... HooksC
   theHooks = new HooksC();
   vt_assert( theHooks );
   // ... TokenFactoryC
   theTokenFactory = new TokenFactoryC();
   vt_assert( theTokenFactory );
   // ... DefinitionsC
   theDefinitions = new DefinitionsC();
   vt_assert( theDefinitions );
   // ... MarkersC
   theMarkers = new MarkersC();
   vt_assert( theMarkers );
   // ... EventsAndStatsC (for unifying events)
   theEvents = new EventsAndStatsC( EventsAndStatsC::SCOPE_EVENTS );
   vt_assert( theEvents );
   // ... EventsAndStatsC (for unifying statistics)
   theStatistics = new EventsAndStatsC( EventsAndStatsC::SCOPE_STATS );
   vt_assert( theStatistics );
   // ... TimeSyncC
   theTimeSync = new TimeSyncC();
   vt_assert( theTimeSync );
   // ... UserComC
   theUserCom = new UserComC();
   vt_assert( theUserCom );

   do
   {
      // get unify parameters
      if( (error = !getParams( argc, argv )) )
         break;

      // show usage text, if necessary/desired
      //
      if( Params.showusage )
      {
         MASTER showUsage();
         break;
      }

      // show VT version, if desired
      //
      if( Params.showversion )
      {
         MASTER std::cout << PACKAGE_VERSION << std::endl;
         break;
      }

#ifndef VT_LIB
      // set output stream for verbose messages to stdout, if vtunify is
      // started from the command line
      if( !Params.autostart )
         verboseStream = stdout;
#endif // VT_LIB

      // register hook classes
      theHooks->registerHooks();

      // trigger initialization hook
      theHooks->triggerInitHook();

      // read unify control files (*.uctl)
      if( (error = !getUnifyControls()) )
         break;

      // unify definitions
      if( (error = !theDefinitions->run()) )
         break;

      // unify markers
      if( (error = !theMarkers->run()) )
         break;

#ifdef VT_MPI
     if( NumRanks > 1 )
     {
        // share user communication ids to all ranks
        if( (error = !theUserCom->share()) )
           break;
     }
#endif // VT_MPI

     // unify events
     if( UnifyControlS::have_events() && !Params.onlystats &&
         (error = !theEvents->run()) )
        break;

     // unify statistics
     if( UnifyControlS::have_stats() && (error = !theStatistics->run()) )
        break;

     // create OTF master control file
     //
     MASTER error = !writeMasterControl();
     if( SyncError( &error ) )
        break;

     // finally, clean up (e.g. rename temporary output files)
     if( (error = !cleanUp()) )
        break;

     // trigger finalization hook
     theHooks->triggerFinalizeHook( error );

     VPrint( 1, "Done\n" );

   } while( false );

   // delete instance of classes ...
   //

   // ... HooksC
   delete theHooks;
   // ... DefinitionsC
   delete theDefinitions;
   // ... MarkersC
   delete theMarkers;
   // ... EventsAndStatsC (for unifying events)
   delete theEvents;
   // ... EventsAndStatsC (for unifying statistics)
   delete theStatistics;
   // ... TokenFactoryC
   delete theTokenFactory;
   // ... TimeSyncC
   delete theTimeSync;
   // ... UserComC
   delete theUserCom;

   // clear vector of unify controls
   //
   for( uint32_t i = 0; i < UnifyCtls.size(); i++ )
      delete UnifyCtls[i];

#if (defined(VT_MPI) && !defined(VT_LIB))
   // either abort on error ...
   //
   if( error )
   {
      CALL_MPI( MPI_Abort( MPI_COMM_WORLD, 1 ) );
   }
   // ... or finalize
   else
   {
      if( NumRanks > 1 )
         CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

      CALL_MPI( MPI_Finalize() );
   }
#endif // VT_MPI && !VT_LIB

   return (error) ? 1 : 0;
}

static bool
getParams( int argc, char ** argv )
{
   bool error = false;

   MASTER
   {
      // parse command line parameters
      error = !parseCommandLine( argc, argv );

      if( !error && !Params.showusage && !Params.showversion )
      {
         // set namestub of output streams, if necessary
         if( Params.out_file_prefix.length() == 0 )
            Params.out_file_prefix = Params.in_file_prefix;

         // if input files shall be kept and output filename
         // is equal to input filename, then prefix output filename
         if( !Params.doclean &&
            Params.out_file_prefix == Params.in_file_prefix )
         {
            int32_t fileidx = Params.out_file_prefix.rfind('/');

            if( fileidx > -1 )
            {
               Params.out_file_prefix =
                  Params.out_file_prefix.substr( 0, fileidx + 1 ) +
                  UniFilePrefix + Params.out_file_prefix.substr( fileidx + 1 );
            }
            else
            {
               Params.out_file_prefix = UniFilePrefix + Params.out_file_prefix;
            }
         }

#if defined(HAVE_IOFSL) && HAVE_IOFSL
         if( Params.doiofsl() )
         {
            // in IOFSL mode, the absolute path of the output trace file prefix
            // is required
            //

            std::string out_file_dir = "./";
            std::string out_file_prefix = Params.out_file_prefix;

            std::string::size_type si = Params.out_file_prefix.rfind('/');
            if( si != std::string::npos )
            {
               out_file_dir = Params.out_file_prefix.substr( 0, si );
               out_file_prefix = Params.out_file_prefix.substr( si + 1 );
            }

            char* abs_out_file_dir = realpath( out_file_dir.c_str(), 0 );
            if( !abs_out_file_dir )
            {
               std::cerr << ExeName << ": Error: "
                         << "Could not retrieve the absolute path of "
                         << out_file_dir << ": "
                         << strerror( errno ) << std::endl;
               error = true;
            }
            else
            {
               Params.out_file_prefix =
                  std::string( abs_out_file_dir ) + '/' + out_file_prefix;
               free( abs_out_file_dir );
            }
         }
#endif // HAVE_IOFSL

#ifdef VT_UNIFY_HOOKS_PROF
         // set profile output filename, if necessary
         if( !error && Params.prof_out_file.length() == 0 )
            Params.prof_out_file = Params.out_file_prefix + ".prof.txt";
#endif // VT_UNIFY_HOOKS_PROF
      }
   } // MASTER

#ifdef VT_MPI
   SyncError( &error );

   // share unify parameters to all ranks, if necessary
   //
   if( !error && NumRanks > 1 )
   {
      error = !shareParams();
      SyncError( &error );
   }
#endif // VT_MPI

   return !error;
}

static bool
getUnifyControls()
{
   bool error = false;

   VPrint( 1, "Reading unify control file\n" );

   // trigger phase pre hook
   theHooks->triggerPhaseHook( HooksC::Phase_GetUnifyControls_pre );

   std::ifstream in;
   char filename[STRBUFSIZE];

   MASTER
   {
      // compose unify control file name
      snprintf( filename, sizeof( filename ) - 1, "%s.uctl",
                Params.in_file_prefix.c_str() );

      // open unify control file for reading
      //
      in.open( filename );
      if( !in )
      {
         std::cerr << ExeName << ": Error: "
                   << "Could not open file " << filename << std::endl;

         error = true;
      }

      VPrint( 3, " Opened %s for reading\n", filename );
   } // MASTER
   if( SyncError( &error ) )
      return false;

   MASTER
   {
      // indicator for compatibility error
      bool compat_error = false;

      // indicator for available streams
      bool any_stream_avail = false;

      char buffer[STRBUFSIZE];
      char delim = '\n';
      uint32_t line_no = 1;
      uint32_t i;

      do
      {
         uint32_t line_no_sec = 1;

         std::vector<std::pair<uint32_t, bool> > streamids;
         std::vector<uint32_t> col_no;
         int64_t ltime[2] = { 0, 1 };
         int64_t offset[2] = { 0, 0 };
#ifdef VT_ETIMESYNC
         std::vector<ETimeSyncC::SyncPhaseS>         sync_phases;
         std::vector<ETimeSyncC::SyncTimeS>          sync_times;
         std::vector<std::pair<uint32_t, uint32_t> > sync_pairs;
#endif // VT_ETIMESYNC

         // read file content
         //
         while( !error && !in.getline((char*)buffer, STRBUFSIZE, delim).eof() )
         {
            // header with VT version and compatibility id at the first line?
            if( line_no == 1 )
            {
               char version[STRBUFSIZE];
               char tail[STRBUFSIZE];
               uint32_t compat_id;

               // read VT version and compatibility id
               if( sscanf( buffer, "<VTUCTL %s %x%s",
                           version, &compat_id, tail ) != 3 ||
                   strcmp( tail, ">" ) != 0 )
               {
                  error = true;
               }
               // check for trace/vtunify compatibility
               //
               else if( compat_id < VT_UNIFY_COMPAT_ID )
               {
                  std::cerr << ExeName << ": Error: "
                            << "You tried to unify a trace file that was "
                            << "generated with a VampirTrace version that is "
                            << "too old (" << version << "). Please use this "
                            << "older version to unify the trace file instead "
                            << "(Your installed version is "
                            << PACKAGE_VERSION << "). Aborting." << std::endl;
                  error = compat_error = true;
               }
               else if( compat_id > VT_UNIFY_COMPAT_ID )
               {
                  std::cerr << ExeName << ": Error: "
                            << "You tried to unify a trace file that was "
                            << "generated with a VampirTrace version that is "
                            << "too recent (" << version << "). Please use "
                            << "this newer version to unify the trace file "
                            << "instead (Your installed version is "
                            << PACKAGE_VERSION << "). Aborting." << std::endl;
                  error = compat_error = true;
               }
               else
               {
                  line_no++;
                  delim = ':';
               }

               continue;
            }
            // VT mode and IOFSL properties at the second line?
            else if( line_no == 2 )
            {
               std::istringstream iss( buffer );
               vt_assert( iss );

               // read VT mode flags
               if( UnifyControlS::mode_flags == (uint32_t)-1 )
               {
                  error =
                     ( !( iss >> std::hex >> UnifyControlS::mode_flags )
                       || UnifyControlS::mode_flags == (uint32_t)-1
                       || ( !UnifyControlS::have_events() &&
                            !UnifyControlS::have_stats() ) );
               }
               // read number of IOFSL servers used
               else if( UnifyControlS::iofsl_num_servers == (uint32_t)-1 )
               {
                  error =
                     ( !( iss >> std::hex >> UnifyControlS::iofsl_num_servers )
                       || UnifyControlS::iofsl_num_servers == (uint32_t)-1 );
               }
               // read IOFSL mode
               else if( UnifyControlS::iofsl_mode == (uint32_t)-1 )
               {
                  error =
                     ( !( iss >> std::hex >> UnifyControlS::iofsl_mode )
                       || UnifyControlS::iofsl_mode == (uint32_t)-1 );
               }
               // end of line
               else
               {
                  if( buffer[0] != '\n' )
                     error = true;
                  else
                     line_no++;
               }

               continue;
            }

            // new unify control section except the first one?
            if( std::string(buffer).find( '*' ) != std::string::npos )
            {
               // leave read loop to finalize previous section
               line_no++;
               break;
            }

            // increment line number and remove new-line
            //
            if( buffer[0] == '\n' )
            {
               buffer[0] = '0'; // replace new-line by zero
               line_no_sec++;
               line_no++;
               col_no.push_back(0);
            }

            switch( line_no_sec )
            {
               // line_no_sec = 1: ids of input streams
               //
               case 1:
               {
                  std::pair<uint32_t, bool> stream_id;
                  std::istringstream iss( buffer );
                  vt_assert( iss );

                  if( ( error = !( iss >> std::hex >> stream_id.first ) ) )
                     break;

                  if( iss.get() == '!' )
                     stream_id.second = false;
                  else
                     stream_id.second = any_stream_avail = true;

                  streamids.push_back( stream_id );

                  break;
               }
               // line_no_sec = 2: read chronological offsets to global time
               //                  and local times
               //
               case 2:
               {
                  std::istringstream iss( buffer );
                  vt_assert( iss );

                  switch( ++(col_no[line_no_sec-2]) )
                  {
                     case 1:
                     {
                        error = !( iss >> std::hex >> ltime[0] );
                        break;
                     }
                     case 2:
                     {
                        // std::stringstream expects unsigned values after
                        // switching format to std::hex; read unsigned and
                        // convert to signed afterwards
                        uint64_t tmp;
                        if( !( error = !(iss >> std::hex >> tmp ) ) )
                           offset[0] = tmp;
                        break;
                     }
                     case 3:
                     {
                        error = !( iss >> std::hex >> ltime[1] );
                        break;
                     }
                     case 4:
                     {
                        // std::stringstream expects unsigned values after
                        // switching format to std::hex; read unsigned and
                        // convert to signed afterwards
                        uint64_t tmp;
                        if( !( error = !(iss >> std::hex >> tmp ) ) )
                           offset[1] = tmp;
                        break;
                     }
                     default:
                     {
                        error = true;
                        break;
                     }
                  }
                  break;
               }
#ifdef VT_ETIMESYNC
               // line_no_sec = 3: read synchronization mapping information
               //
               case 3:
               {
                  static ETimeSyncC::SyncPhaseS sync_phase;
                  std::istringstream iss( buffer );
                  vt_assert( iss );

                  theTimeSync->setSyncMethod( TimeSyncC::METHOD_ENHANCED );

                  switch( ++(col_no[line_no_sec-2]) )
                  {
                     case 1:
                     {
                        error = !( iss >> std::hex >> sync_phase.mapid );
                        break;
                     }
                     case 2:
                     {
                        error = !( iss >> std::hex >> sync_phase.time );
                        break;
                     }
                     case 3:
                     {
                        error = !( iss >> std::hex >> sync_phase.duration );

                        if( !error )
                        {
                           sync_phases.push_back( sync_phase );
                           col_no[line_no_sec-2] = 0;
                        }

                        break;
                     }
                     default:
                     {
                        error = true;
                        break;
                     }
                  }
                  break;
               }
               // line_no_sec = 4-n: read synchronization timestamps of each
               //                    synchronization phase (each per line)
               //
               default:
               {
                  static std::pair<uint32_t, uint32_t> sync_pair;
                  static ETimeSyncC::SyncTimeS sync_time;
                  std::istringstream iss( buffer );
                  vt_assert( iss );

                  if( 4 <= line_no_sec &&
                      line_no_sec <= 4 + sync_phases.size() - 1 )
                  {
                     switch( ++(col_no[line_no_sec-2]) )
                     {
                        case 1:
                        {
                           error = !( iss >> std::hex >> sync_pair.first );
                           break;
                        }
                        case 2:
                        {
                           error = !( iss >> std::hex >> sync_pair.second );
                           break;
                        }
                        case 3:
                        {
                           error = !( iss >> std::hex >> sync_time.t[0] );
                           break;
                        }
                        case 4:
                        {
                           error = !( iss >> std::hex >> sync_time.t[1] );
                           break;
                        }
                        case 5:
                        {
                           error = !( iss >> std::hex >> sync_time.t[2] );
                           break;
                        }
                        case 6:
                        {
                           error = !( iss >> std::hex >> sync_time.t[3] );

                           if( !error )
                           {
                              sync_time.phase_idx = line_no_sec - 4;

                              sync_pairs.push_back( sync_pair );
                              sync_times.push_back( sync_time );

                              col_no[line_no_sec-2] = 0;
                           }

                           break;
                        }
                        default:
                        {
                           error = true;
                           break;
                        }
                     }
                  }
                  else
                  {
                    error = true;
                  }
                  break;
               }
#else // VT_ETIMESYNC
               // line_no_sec = 3-n: stuff for enhanced time sync.
               //
               default:
               {
                  error = true;
                  break;
               }
#endif // VT_ETIMESYNC
            }
         }

         // time sync. information parsed in the right way? */
         //
         if( !error )
         {
#ifdef VT_ETIMESYNC
            if( theTimeSync->getSyncMethod() == TimeSyncC::METHOD_ENHANCED )
            {
               for( i = 0; i < line_no_sec; i++ )
               {
                  if( (i+1 == 2 && col_no[i-1] != 4) ||
                      ((i+1>=3 && i+1<=4+sync_phases.size()-1) &&
                      (col_no.size() >= i) && col_no[i-1] != 0) )
                  {
                     line_no_sec = i+1;
                     error = true;
                  }
               }
            }
            else
#endif // VT_ETIMESYNC
            {
               if( col_no.empty() || col_no[0] != 4 )
               {
                  line_no_sec = 2;
                  error = true;
               }
            }
         }

         // show error message on parse error
         //
         if( error )
         {
            if( !compat_error )
            {
               std::cerr << filename << ":" << line_no
                         << ": Could not be parsed" << std::endl;
            }
         }
         // otherwise, add unify control to vector
         //
         else
         {
            for( i = 0; i < streamids.size(); i++ )
            {
               uint32_t stream_id = streamids[i].first;
               uint32_t pstream_id = (i == 0) ? 0 : streamids[0].first;
               bool     stream_avail = streamids[i].second;

#ifdef VT_ETIMESYNC
               UnifyCtls.push_back(
                  new UnifyControlS( stream_id, pstream_id, stream_avail, ltime,
                     offset, sync_phases, sync_times, sync_pairs ) );
#else // VT_ETIMESYNC
               UnifyCtls.push_back(
                  new UnifyControlS( stream_id, pstream_id, stream_avail, ltime,
                     offset) );
#endif // VT_ETIMESYNC
            }

#ifdef VT_ETIMESYNC
            sync_phases.clear();
            sync_times.clear();
            sync_pairs.clear();
#endif // VT_ETIMESYNC
         }

      } while( !error && in.good() );

      // close unify control file
      in.close();

      if( !error )
      {
         VPrint( 3, " Closed %s\n", filename );

         if( Params.onlystats && !UnifyControlS::have_stats() )
         {
            std::cout << ExeName << ": Error: "
                      << "The input trace file does not contain the requested "
                      << "summarized information. Aborting." << std::endl;
            error = true;
         }
         else if( !any_stream_avail )
         {
            std::cerr << ExeName << ": Error: "
                      << "The input trace file does not have any available "
                      << "stream; nothing to do. Aborting."
                      << std::endl;
            error = true;
         }
      }

   } // MASTER

#ifdef VT_MPI
   SyncError( &error );

   // share unify parameters to all ranks, if necessary
   //
   if( !error && NumRanks > 1 )
   {
      error = !shareUnifyControls();
      SyncError( &error );
   }
#endif // VT_MPI

   if( !error )
   {
#ifdef VT_MPI
      VT_MPI_INT rank = 0;

#if defined(HAVE_IOFSL) && HAVE_IOFSL
      uint32_t iofsl_num_workers = 0;

      if( Params.doiofsl() )
      {
         // in IOFSL mode, calculate number of ranks for unification; must be a
         // multiple or less than Params.iofsl_num_servers
         //
         iofsl_num_workers =
            ( NumRanks / Params.iofsl_num_servers ) * Params.iofsl_num_servers;
         if( iofsl_num_workers == 0 )
            iofsl_num_workers = NumRanks;
      }
#endif // HAVE_IOFSL
#endif // VT_MPI

      // get stream context information
      //
      for( uint32_t i = 0; i < UnifyCtls.size(); i++ )
      {
         UnifyControlS * uctl = UnifyCtls[i];

         // set stream id/unify control mapping
         StreamId2UnifyCtl[uctl->streamid] = uctl;

         if( uctl->stream_avail )
         {
#ifdef VT_MPI
            if( NumRanks > 1 )
            {
               // assign stream id to rank, whereas childs will not be
               // separated from its parent stream id
               //

#if defined(HAVE_IOFSL) && HAVE_IOFSL
               if( Params.doiofsl() )
               {
                  // in IOFSL mode, calculate rank for the current stream id
                  rank =
                     ( uctl->streamid & VT_TRACEID_BITMASK ) %
                     iofsl_num_workers;
               }
#endif // HAVE_IOFSL

               // assign stream id to rank
               if( rank == MyRank )
                  MyStreamIds.push_back( uctl->streamid );

               // set stream id/rank mapping
               StreamId2Rank[uctl->streamid] = rank;

               // add stream id to processing rank
               Rank2StreamIds[rank].insert( uctl->streamid );

               // in non-IOFSL mode, get rank for the next stream id
               //
#if defined(HAVE_IOFSL) && HAVE_IOFSL
               if( !Params.doiofsl() )
#endif // HAVE_IOFSL
               if( i < UnifyCtls.size() - 1 && UnifyCtls[i+1]->pstreamid == 0 )
               {
                  if( rank + 1 == NumRanks )
                     rank = 0;
                  else
                     rank++;
               }
            }
            else
#endif // VT_MPI
            {
               // take all stream ids, if serial
               MyStreamIds.push_back( uctl->streamid );
            }

            // increment number of available streams
            NumAvailStreams++;
         }
#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
         else
         {
            // store absent stream id
            AbsentStreamIds.insert( uctl->streamid );
         }
#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
      }

#if defined(HAVE_OMP) && HAVE_OMP
      // reset max. number of threads
      //

      int new_max_threads = 0;

      // due to not yet thread-safe implementation in OTF disable OpenMP if
      // IOFSL mode is enabled for reading and/or writing
      //
      if( UnifyControlS::is_iofsl() ) new_max_threads = 1;
#if defined(HAVE_IOFSL) && HAVE_IOFSL
      if( Params.doiofsl() ) new_max_threads = 1;
#endif // HAVE_IOFSL

      // adapt max. number of threads to number of stream ids, if necessary
      //
      if( new_max_threads == 0 && !MyStreamIds.empty() &&
          (int)MyStreamIds.size() < omp_get_max_threads() )
      {
         new_max_threads = (int)MyStreamIds.size();
      }

      // set new max. number of threads, if it differs from the current setting
      //
      if( new_max_threads != 0 && new_max_threads != omp_get_max_threads() )
      {
         omp_set_num_threads( new_max_threads );
         PVPrint( 3, "Reset maximum number of threads to %d\n",
                  omp_get_max_threads() );
      }
#endif // HAVE_OMP

      // trigger phase post hook
      theHooks->triggerPhaseHook( HooksC::Phase_GetUnifyControls_post );
   }

   return !error;
}

static bool
parseCommandLine( int argc, char ** argv )
{
   bool error = false;

   // read environment variables
   //

#if defined(HAVE_IOFSL) && HAVE_IOFSL

   char* env;

   // VT_IOFSL_SERVERS
   //
   if( ( env = getenv( "VT_IOFSL_SERVERS" ) ) && strlen( env ) > 0 )
   {
      // get a copy of env. value
      //
      char* tmp = new char[strlen( env ) + 1];
      vt_assert( tmp );
      strcpy( tmp, env );

      // extract IOFSL servers from env. value
      //
      if( !parseIofslServerList( tmp ) )
      {
         std::cerr << ExeName << ": invalid value for environment variable -- "
                   << "VT_IOFSL_SERVERS" << std::endl;
         error = true;
      }

      delete [] tmp;
   }
   // VT_IOFSL_MODE
   //
   if( ( env = getenv( "VT_IOFSL_MODE" ) ) && strlen( env ) > 0 )
   {
      // get a copy of env. value
      //
      char* tmp = new char[strlen( env ) + 1];
      vt_assert( tmp );
      strcpy( tmp, env );

      // convert env. value's letter to lower case
      //
      char* p = tmp;
      while( *p ) { *p = tolower(*p); p++; }

      // set IOFSL mode
      //
      if( strcmp( tmp, "multifile" ) == 0 )
      {
         Params.iofsl_mode = VT_IOFSL_MODE_MULTIFILE;
      }
      else if( strcmp( tmp, "multifile_split" ) == 0 )
      {
         Params.iofsl_mode = VT_IOFSL_MODE_MULTIFILE_SPLIT;
      }
      else
      {
         std::cerr << ExeName << ": invalid value for environment "
                   << "variable -- VT_IOFSL_MODE" << std::endl;
         error = true;
      }

      delete [] tmp;
   }
   // VT_IOFSL_ASYNC_IO
   //
   if( ( env = getenv( "VT_IOFSL_ASYNC_IO" ) ) && strlen( env ) > 0 )
   {
      // get a copy of env. value
      //
      char* tmp = new char[strlen( env ) + 1];
      vt_assert( tmp );
      strcpy( tmp, env );

      // convert env. value's letter to lower case
      //
      char* p = tmp;
      while( *p ) { *p = tolower(*p); p++; }

      // set IOFSL flag
      //
      if( strcmp( tmp, "yes" ) == 0 || strcmp( tmp, "true" ) == 0 ||
          strcmp( tmp, "1" ) == 0 )
      {
         Params.iofsl_flags |= VT_IOFSL_FLAG_ASYNC_IO;
      }

      delete [] tmp;
   }

#endif // HAVE_IOFSL

   // parse command line options
   //
   for( int i = 1; i < argc && !error; i++ )
   {
      // -h, --help
      //
      if( strcmp( argv[i], "-h" ) == 0
          || strcmp( argv[i], "--help" ) == 0 )
      {
         Params.showusage = true;
         break;
      }
      // -V, --version
      //
      else if( strcmp( argv[i], "-V" ) == 0
               || strcmp( argv[i], "--version" ) == 0 )
      {
         Params.showversion = true;
         break;
      }
      // -o
      //
      else if( strcmp( argv[i], "-o" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": PREFIX expected -- -o" << std::endl;
            error = true;
         }
         else
         {
            Params.out_file_prefix = argv[++i];
            if( Params.out_file_prefix.compare( 0, 1, "/" ) != 0 &&
                Params.out_file_prefix.compare( 0, 2, "./" ) != 0 )
               Params.out_file_prefix = std::string("./") + Params.out_file_prefix;
         }
      }
#ifdef VT_UNIFY_HOOKS_PROF
      // -f
      //
      else if( strcmp( argv[i], "-f" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": FILE expected -- -f" << std::endl;
            error = true;
         }
         else
         {
            Params.prof_out_file = argv[++i];
            if( Params.prof_out_file.compare( 0, 1, "/" ) != 0 &&
                Params.prof_out_file.compare( 0, 2, "./" ) != 0 )
               Params.prof_out_file = std::string("./") + Params.prof_out_file;
         }
      }
#endif // VT_UNIFY_HOOKS_PROF
#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
      // --snapshots
      //
      else if( strcmp( argv[i], "--nosnapshots" ) == 0 )
      {
         Params.createsnaps = false;
      }
      // --maxsnapshots
      //
      else if( strcmp( argv[i], "--maxsnapshots" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": N expected -- --maxsnapshots" << std::endl;
            error = true;
         }
         else
         {
            int tmp = atoi( argv[++i] );
            if( tmp < 1 )
            {
               std::cerr << ExeName << ": invalid argument for option -- "
                         << "--maxsnapshots" << std::endl;
               error = true;
            }
            else
            {
               Params.maxsnapshots = (uint32_t)tmp;
            }
         }
      }
      // --nomsgmatch
      //
      else if( strcmp( argv[i], "--nomsgmatch" ) == 0 )
      {
         Params.domsgmatch = false;
      }
      // --droprecvs
      //
      else if( strcmp( argv[i], "--droprecvs" ) == 0 )
      {
         Params.droprecvs = true;
      }
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS
#if defined(HAVE_ZLIB) && HAVE_ZLIB
      // --nocompress
      //
      else if( strcmp( argv[i], "--nocompress" ) == 0 )
      {
         Params.docompress = false;
      }
#endif // HAVE_ZLIB
#if defined(HAVE_IOFSL) && HAVE_IOFSL
      // --iofsl-servers
      //
      else if( strcmp( argv[i], "--iofsl-servers" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": LIST expected -- --iofsl-servers"
                      << std::endl;
            error = true;
         }
         else
         {
            if( !parseIofslServerList( argv[++i] ) )
            {
               std::cerr << ExeName << ": invalid argument for option -- "
                         << "--iofsl-servers" << std::endl;
               error = true;
            }
         }
      }
      // --iofsl-mode
      //
      else if( strcmp( argv[i], "--iofsl-mode" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": MODE expected -- --iofsl-mode"
                      << std::endl;
            error = true;
         }
         else
         {
            char* p = argv[++i];
            while( *p ) { *p = tolower(*p); p++; }

            if( strcmp( argv[i], "multifile" ) == 0 )
            {
               Params.iofsl_mode = VT_IOFSL_MODE_MULTIFILE;
            }
            else if( strcmp( argv[i], "multifile_split" ) == 0 )
            {
               Params.iofsl_mode = VT_IOFSL_MODE_MULTIFILE_SPLIT;
            }
            else
            {
               std::cerr << ExeName << ": invalid argument for option -- "
                         << "--iofsl-mode" << std::endl;
               error = true;
            }
         }
      }
      // --iofsl-asyncio
      //
      else if( strcmp( argv[i], "--iofsl-asyncio" ) == 0 )
      {
         Params.iofsl_flags |= VT_IOFSL_FLAG_ASYNC_IO;
      }
#endif // HAVE_IOFSL
      // -k, --keeplocal
      //
      else if( strcmp( argv[i], "-k" ) == 0
               || strcmp( argv[i], "--keeplocal" ) == 0 )
      {
         Params.doclean = false;
      }
      // -p, --progress
      //
      else if( strcmp( argv[i], "-p" ) == 0
               || strcmp( argv[i], "--progress" ) == 0 )
      {
         Params.showprogress = true;
         std::cerr << ExeName << ": Warning: Progress not yet implemented -- "
                   << argv[i] << std::endl;
      }
      // -q, --quiet
      //
      else if( strcmp( argv[i], "-q" ) == 0 
               || strcmp( argv[i], "--quiet" ) == 0 )
      {
         Params.bequiet = true;
         Params.showprogress = false;
         Params.verbose_level = 0;
      }
      // --stat, --stats
      //
      else if( strcmp( argv[i], "--stat" ) == 0
               ||strcmp( argv[i], "--stats" ) == 0 )
      {
         Params.onlystats = true;
      }
      // -v, --verbose
      //
      else if( strcmp( argv[i], "-v" ) == 0 
               || strcmp( argv[i], "--verbose" ) == 0 )
      {
         Params.verbose_level++;
      }
#ifndef VT_LIB
      // --autostart (hidden)
      //
      else if( strcmp( argv[i], "--autostart" ) == 0 )
      {
         Params.autostart = true;
      }
#endif // VT_LIB
      // input trace file prefix
      //
      else if( Params.in_file_prefix.length() == 0 )
      {
         Params.in_file_prefix = argv[i];
         if( Params.in_file_prefix.compare( 0, 1, "/" ) != 0 &&
             Params.in_file_prefix.compare( 0, 2, "./" ) != 0 )
            Params.in_file_prefix = std::string("./") + Params.in_file_prefix;
      }
      // unrecognized option
      //
      else
      {
         std::cerr << ExeName << ": invalid option -- " << argv[i] << std::endl;
         error = true;
      }
   }

   // set flag to show usage text, if command line parameters are incomplete
   //
   if( !error && !Params.showusage && !Params.showversion &&
       Params.in_file_prefix.length() == 0 )
   {
      Params.showusage = true;
   }

   return !error;
}

#if defined(HAVE_IOFSL) && HAVE_IOFSL

static bool
parseIofslServerList( char* list )
{
   bool error = false;

   // re-initialize list of IOFSL servers, if necessary
   //
   if( Params.iofsl_num_servers > 0 )
   {
      for( uint32_t i = 0; i < Params.iofsl_num_servers; i++ )
         delete [] Params.iofsl_servers[i];
      delete [] Params.iofsl_servers;

      Params.iofsl_num_servers = 0;
   }

   // get number of IOFSL servers by counting separators in list
   //
   char* p = list;
   uint32_t n = 0;
   while( *p )
   {
      if( *p == ',' ) n++;
         p++;
   }
   n++;

   // allocate list of IOFSL servers
   //
   Params.iofsl_servers = new char*[n];
   vt_assert( Params.iofsl_servers );

   // extract IOFSL servers from list
   //
   Params.iofsl_num_servers = 0;
   char* token = strtok( list, "," );
   do
   {
      if( !token || strlen( token ) == 0 )
      {
         error = true;
         break;
      }

      Params.iofsl_servers[Params.iofsl_num_servers] =
         new char[strlen( token ) + 1];
      vt_assert( Params.iofsl_servers[Params.iofsl_num_servers] );
      strcpy( Params.iofsl_servers[Params.iofsl_num_servers], token );
      Params.iofsl_num_servers++;

  } while( ( token = strtok( 0, "," ) ) );

  return !error;
}

#endif // HAVE_IOFSL

static bool
writeMasterControl()
{
   bool error = false;

   VPrint( 1, "Writing OTF master control\n" );

   // trigger phase pre hook
   theHooks->triggerPhaseHook( HooksC::Phase_WriteMasterControl_pre );

   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // open file manager
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

   // create master control
   //
   OTF_MasterControl * mc = OTF_MasterControl_new( manager );
   vt_assert( mc );

   // add stream/process[group] mappings to master control
   //
   for( uint32_t i = 0; i < UnifyCtls.size() && !error; i++ )
   {
      // add only available streams/processes
      //
      if( UnifyCtls[i]->stream_avail )
      {
         const uint32_t & streamid = UnifyCtls[i]->streamid;

         // get additional process group tokens of stream
         const std::set<uint32_t> * procgrps =
            theDefinitions->groupCounters()->getGroupsOfStream( streamid );

         // add mappings
         //
         std::set<uint32_t>::const_iterator procgrp_it;
         if( procgrps ) procgrp_it = procgrps->begin();
         uint32_t proc_or_group = streamid;
         while( proc_or_group != 0 )
         {
            if( OTF_MasterControl_append( mc, streamid, proc_or_group ) == 0 )
            {
               std::cerr << ExeName << ": Error: "
                         << "Could not append mapping " << std::hex
                         << streamid << ":" << proc_or_group << std::dec
                         << " to OTF master control" << std::endl;
               error = true;
               break;
            }

            VPrint( 3, " Added mapping %x:%x to OTF master control\n",
                    streamid, proc_or_group );

            // get next process group token to add
            //
            if( procgrps && procgrp_it != procgrps->end() )
            {
               proc_or_group = *procgrp_it;
               ++procgrp_it;
            }
            else
            {
               proc_or_group = 0;
            }
         }
      }
   }

   // write master control
   //
   if( !error )
   {
      OTF_MasterControl_write( mc, tmp_out_file_prefix.c_str() );

      VPrint( 3, " Opened OTF master control [namestub %s]\n",
              tmp_out_file_prefix.c_str() );
   }

   // close file master control
   OTF_MasterControl_close( mc );
   // close file manager
   OTF_FileManager_close( manager );

   if( !error )
   {
      VPrint( 3, " Closed OTF master control [namestub %s]\n",
              tmp_out_file_prefix.c_str() );

      // trigger phase post hook
      theHooks->triggerPhaseHook( HooksC::Phase_WriteMasterControl_post );
   }

   return !error;
}

static bool
cleanUp()
{
   bool error = false;

#ifdef VT_MPI
   // block until all ranks have reached this point
   if( NumRanks > 1 )
      CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );
#endif // VT_MPI

   VPrint( 1, "Cleaning up\n" );

   // trigger phase pre hook
   theHooks->triggerPhaseHook( HooksC::Phase_CleanUp_pre );

   do
   {
      // rename temporary definition output file
      if( (error = !theDefinitions->cleanUp()) )
         break;

      // rename temporary marker output file
      if( (error = !theMarkers->cleanUp()) )
         break;

      // rename temporary event output files
      if( UnifyControlS::have_events() && !Params.onlystats &&
          (error = !theEvents->cleanUp()) )
         break;

      // rename temporary statistic output files
      if( UnifyControlS::have_stats() && (error = !theStatistics->cleanUp()) )
         break;

      // remove unify control file and rename temporary OTF master control file
      //

      if( SyncError( &error ) )
         break;

      MASTER
      {
         char filename1[STRBUFSIZE];
         char filename2[STRBUFSIZE];

         std::string tmp_out_file_prefix =
            Params.out_file_prefix + TmpFileSuffix;

         // remove unify control file, if necessary
         //
         if( Params.doclean )
         {
            snprintf( filename1, sizeof( filename1 ) - 1, "%s.uctl",
                      Params.in_file_prefix.c_str() );

            if( remove( filename1 ) != 0 )
            {
               std::cerr << ExeName << ": Error: Could not remove "
                         << filename1 << std::endl;
               error = true;
               break;
            }
            VPrint( 3, " Removed %s\n", filename1 );
         }

         // rename temporary OTF master control file
         //
         OTF_getFilename( tmp_out_file_prefix.c_str(), 0,
                          OTF_FILETYPE_MASTER, STRBUFSIZE, filename1 );
         OTF_getFilename( Params.out_file_prefix.c_str(), 0,
                          OTF_FILETYPE_MASTER, STRBUFSIZE, filename2 );
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
      SyncError( &error );

   } while( false );

   // trigger phase post hook, if no error occurred
   if( !error )
      theHooks->triggerPhaseHook( HooksC::Phase_CleanUp_post );

   return !error;
}

static void
showUsage()
{
   std::cout << std::endl
      << " " << ExeName << " - local trace unifier for VampirTrace."  << std::endl
      << std::endl
      << " Syntax: " << ExeName << " [options] <input trace prefix>" << std::endl
      << std::endl
      << "   options:" << std::endl
      << "     -h, --help          Show this help message." << std::endl
      << std::endl
      << "     -V, --version       Show VampirTrace version." << std::endl
      << std::endl
      << "     -o PREFIX           Prefix of output trace filename." << std::endl
      << std::endl
#ifdef VT_UNIFY_HOOKS_PROF
      << "     -f FILE             Function profile output filename." << std::endl
      << "                         (default: PREFIX.prof.txt)" << std::endl
      << std::endl
#endif // VT_UNIFY_HOOKS_PROF
      << "     -k, --keeplocal     Don't remove input trace files." << std::endl
      << std::endl
      << "     -p, --progress      Show progress." << std::endl
      << std::endl
      << "     -v, --verbose       Increase output verbosity." << std::endl
      << "                         (can be used more than once)" << std::endl
      << std::endl
      << "     -q, --quiet         Enable quiet mode." << std::endl
      << "                         (only emergency output)" << std::endl
      << std::endl
#if defined(HAVE_IOFSL) && HAVE_IOFSL
      << "     --iofsl-servers LIST" << std::endl
      << "                         Enable IOFSL mode where LIST contains a comma-separated" << std::endl
      << "                         list of IOFSL server addresses." << std::endl
      << std::endl
      << "     --iofsl-mode MODE   IOFSL mode (MULTIFILE or MULTIFILE_SPLIT)." << std::endl
      << "                         (default: MULTIFILE_SPLIT)" << std::endl
      << std::endl
      << "     --iofsl-asyncio     Use asynchronous I/O in IOFSL mode." << std::endl
      << std::endl
#endif // HAVE_IOFSL
      << "     --stats             Unify only summarized information (*.stats), no events" << std::endl
      << std::endl
#if defined(HAVE_ZLIB) && HAVE_ZLIB
      << "     --nocompress        Don't compress output trace files." << std::endl
      << std::endl
#endif // HAVE_ZLIB
#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
      << "     --nosnapshots       Don't create snapshots." << std::endl
      << std::endl
      << "     --maxsnapshots N    Maximum number of snapshots." << std::endl
      << "                         (default: 1024)" << std::endl
      << std::endl
      << "     --nomsgmatch        Don't match messages." << std::endl
      << std::endl
      << "     --droprecvs         Drop message receive events, if msg. matching" << std::endl
      << "                         is enabled." << std::endl
      << std::endl
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS
#if defined(HAVE_IOFSL) && HAVE_IOFSL
      << "   environment variables:" << std::endl
      << "     VT_IOFSL_SERVERS=LIST" << std::endl
      << "                         equivalent to '--iofsl-servers'" << std::endl
      << "     VT_IOFSL_MODE       equivalent to '--iofsl-mode'" << std::endl
      << "     VT_IOFSL_ASYNC_IO=<yes|true|1>" << std::endl
      << "                         equivalent to '--iofsl-asyncio'" << std::endl
      << std::endl
#endif // HAVE_IOFSL
      ;
}

#ifdef VT_MPI

static bool
shareParams()
{
   bool error = false;

   vt_assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   char * buffer;
   VT_MPI_INT buffer_size;
   VT_MPI_INT position;

   // get buffer size needed to pack unify parameters
   //
   MASTER
   {
      VT_MPI_INT size;

      buffer_size = 0;

      // in_file_prefix.length() + out_file_prefix.length() + verbose_level
      //
      CALL_MPI( MPI_Pack_size( 3, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
      buffer_size += size;

      // in_file_prefix + out_file_prefix + docompress + doclean + showusage +
      // showversion + showprogress + bequiet + onlystats
      //
      CALL_MPI( MPI_Pack_size( (VT_MPI_INT)Params.in_file_prefix.length()+1+
                               (VT_MPI_INT)Params.out_file_prefix.length()+1+
                               7, MPI_CHAR, MPI_COMM_WORLD, &size ) );
      buffer_size += size;

#ifndef VT_LIB
      // autostart
      //
      CALL_MPI( MPI_Pack_size( 1, MPI_CHAR, MPI_COMM_WORLD, &size ) );
      buffer_size += size;
#endif // VT_LIB

#if defined(HAVE_IOFSL) && HAVE_IOFSL
      // iofsl_mode + iofsl_flags + iofsl_num_servers
      //
      CALL_MPI( MPI_Pack_size( 3, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
      buffer_size += size;

      // iofsl_servers
      //
      for( uint32_t i = 0; i < Params.iofsl_num_servers; i++ )
      {
         // strlen(iofsl_servers[i])
         //
         CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // iofsl_servers[i]
         //
         CALL_MPI( MPI_Pack_size( strlen( Params.iofsl_servers[i] ) + 1,
                                  MPI_CHAR, MPI_COMM_WORLD, &size ) );
         buffer_size += size;
      }
#endif // HAVE_IOFSL

#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
      // domsgmatch + droprecvs + createsnaps
      //
      CALL_MPI( MPI_Pack_size( 3, MPI_CHAR, MPI_COMM_WORLD, &size ) );
      buffer_size += size;
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

#ifdef VT_UNIFY_HOOKS_PROF
      // prof_out_file.length() + prof_sort_flags
      //
      CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
      buffer_size += size;

      // prof_out_file
      //
      CALL_MPI( MPI_Pack_size( (VT_MPI_INT)Params.prof_out_file.length()+1,
                               MPI_CHAR, MPI_COMM_WORLD, &size ) );
      buffer_size += size;
#endif // VT_UNIFY_HOOKS_PROF
   } // MASTER

   // share buffer size
   CALL_MPI( MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD ) );

   // allocate buffer
   //
   buffer = new char[buffer_size];
   vt_assert( buffer );

   // pack unify parameters
   //
   MASTER
   {
      position = 0;

      // in_file_prefix.length()
      //
      uint32_t in_file_prefix_length = Params.in_file_prefix.length() + 1;
      CALL_MPI( MPI_Pack( &in_file_prefix_length, 1, MPI_UNSIGNED, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // in_file_prefix
      CALL_MPI( MPI_Pack( const_cast<char*>(Params.in_file_prefix.c_str()),
                          (VT_MPI_INT)in_file_prefix_length, MPI_CHAR,
                          buffer, buffer_size, &position, MPI_COMM_WORLD ) );

      // out_file_prefix.length()
      //
      uint32_t out_file_prefix_length = Params.out_file_prefix.length() + 1;
      CALL_MPI( MPI_Pack( &out_file_prefix_length, 1, MPI_UNSIGNED, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // out_file_prefix
      CALL_MPI( MPI_Pack( const_cast<char*>(Params.out_file_prefix.c_str()),
                          (VT_MPI_INT)out_file_prefix_length, MPI_CHAR,
                          buffer, buffer_size, &position, MPI_COMM_WORLD ) );

      // verbose_level
      CALL_MPI( MPI_Pack( &(Params.verbose_level), 1, MPI_UNSIGNED, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // docompress
      CALL_MPI( MPI_Pack( &(Params.docompress), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // doclean
      CALL_MPI( MPI_Pack( &(Params.doclean), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // showusage
      CALL_MPI( MPI_Pack( &(Params.showusage), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // showversion
      CALL_MPI( MPI_Pack( &(Params.showversion), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // showprogress
      CALL_MPI( MPI_Pack( &(Params.showprogress), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // bequiet
      CALL_MPI( MPI_Pack( &(Params.bequiet), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // onlystats
      CALL_MPI( MPI_Pack( &(Params.onlystats), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

#ifndef VT_LIB
      // autostart
      CALL_MPI( MPI_Pack( &(Params.autostart), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );
#endif // VT_LIB

#if defined(HAVE_IOFSL) && HAVE_IOFSL
      // iofsl_mode
      CALL_MPI( MPI_Pack( &(Params.iofsl_mode), 1, MPI_UNSIGNED, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // iofsl_flags
      CALL_MPI( MPI_Pack( &(Params.iofsl_flags), 1, MPI_UNSIGNED, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // iofsl_num_servers
      CALL_MPI( MPI_Pack( &(Params.iofsl_num_servers), 1, MPI_UNSIGNED, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // iofsl_servers
      //
      for( uint32_t i = 0; i < Params.iofsl_num_servers; i++ )
      {
         // strlen(iofsl_servers[i])
         //
         uint32_t len = strlen( Params.iofsl_servers[i] ) + 1;
         CALL_MPI( MPI_Pack( &len, 1, MPI_UNSIGNED, buffer, buffer_size,
                             &position, MPI_COMM_WORLD ) );

         // iofsl_servers[i]
         CALL_MPI( MPI_Pack( Params.iofsl_servers[i], len, MPI_CHAR, buffer,
                             buffer_size, &position, MPI_COMM_WORLD ) );
      }
#endif // HAVE_IOFSL

#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
      // domsgmatch
      CALL_MPI( MPI_Pack( &(Params.domsgmatch), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // droprecvs
      CALL_MPI( MPI_Pack( &(Params.droprecvs), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // createsnaps
      CALL_MPI( MPI_Pack( &(Params.createsnaps), 1, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

#ifdef VT_UNIFY_HOOKS_PROF
      // prof_out_file.length()
      //
      uint32_t prof_out_file_length = Params.prof_out_file.length() + 1;
      CALL_MPI( MPI_Pack( &prof_out_file_length, 1, MPI_UNSIGNED, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // prof_out_file
      CALL_MPI( MPI_Pack( const_cast<char*>(Params.prof_out_file.c_str()),
                          (VT_MPI_INT)prof_out_file_length, MPI_CHAR, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );

      // prof_sort_flags
      CALL_MPI( MPI_Pack( &(Params.prof_sort_flags), 1, MPI_UNSIGNED, buffer,
                          buffer_size, &position, MPI_COMM_WORLD ) );
#endif // VT_UNIFY_HOOKS_PROF
   } // MASTER

   // share packed unify parameters
   CALL_MPI( MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD ) );

   // unpack unify parameters
   //
   SLAVE
   {
      position = 0;

      // in_file_prefix.length()
      //
      uint32_t in_file_prefix_length;
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &in_file_prefix_length, 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );

      // in_file_prefix
      //
      char* in_file_prefix = new char[in_file_prefix_length];
      vt_assert( in_file_prefix );
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            in_file_prefix, (VT_MPI_INT)in_file_prefix_length,
                            MPI_CHAR, MPI_COMM_WORLD ) );
      Params.in_file_prefix = in_file_prefix;
      delete [] in_file_prefix;

      // out_file_prefix.length()
      //
      uint32_t out_file_prefix_length;
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &out_file_prefix_length, 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );

      // out_file_prefix
      //
      char* out_file_prefix = new char[out_file_prefix_length];
      vt_assert( out_file_prefix );
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            out_file_prefix, (VT_MPI_INT)out_file_prefix_length,
                            MPI_CHAR, MPI_COMM_WORLD ) );
      Params.out_file_prefix = out_file_prefix;
      delete [] out_file_prefix;

      // verbose_level
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.verbose_level), 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );

      // docompress
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.docompress), 1, MPI_CHAR,
                            MPI_COMM_WORLD ) );

      // doclean
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &(Params.doclean),
                            1, MPI_CHAR, MPI_COMM_WORLD ) );

      // showusage
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &(Params.showusage),
                            1, MPI_CHAR, MPI_COMM_WORLD ) );

      // showversion
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.showversion), 1, MPI_CHAR,
                            MPI_COMM_WORLD ) );

      // showprogress
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.showprogress), 1, MPI_CHAR,
                            MPI_COMM_WORLD ) );

      // bequiet
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &(Params.bequiet),
                            1, MPI_CHAR, MPI_COMM_WORLD ) );

      // onlystats
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &(Params.onlystats),
                            1, MPI_CHAR, MPI_COMM_WORLD ) );

#ifndef VT_LIB
      // autostart
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &(Params.autostart),
                            1, MPI_CHAR, MPI_COMM_WORLD ) );
#endif // VT_LIB

#if defined(HAVE_IOFSL) && HAVE_IOFSL
      // iofsl_mode
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.iofsl_mode), 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );

      // iofsl_flags
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.iofsl_flags), 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );

      // iofsl_num_servers
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.iofsl_num_servers), 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );

      // iofsl_servers
      //
      if( Params.iofsl_num_servers > 0 )
      {
         Params.iofsl_servers = new char*[Params.iofsl_num_servers];
         vt_assert( Params.iofsl_servers );

         for( uint32_t i = 0; i < Params.iofsl_num_servers; i++ )
         {
            // strlen(iofsl_servers[i])
            //
            uint32_t len;
            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &len, 1,
                                  MPI_UNSIGNED, MPI_COMM_WORLD ) );

            // iofsl_servers[i]
            //
            Params.iofsl_servers[i] = new char[len];
            vt_assert( Params.iofsl_servers[i] );
            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                                  Params.iofsl_servers[i], len, MPI_CHAR,
                                  MPI_COMM_WORLD ) );
         }
      }
#endif // HAVE_IOFSL

#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
      // domsgmatch
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.domsgmatch), 1, MPI_CHAR,
                            MPI_COMM_WORLD ) );

      // droprecvs
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &(Params.droprecvs),
                            1, MPI_CHAR, MPI_COMM_WORLD ) );

      // createsnaps
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.createsnaps), 1, MPI_CHAR,
                            MPI_COMM_WORLD ) );
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

#ifdef VT_UNIFY_HOOKS_PROF
      // prof_out_file.length()
      //
      uint32_t prof_out_file_length;
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &prof_out_file_length, 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );

      // prof_out_file
      //
      char* prof_out_file = new char[prof_out_file_length];
      vt_assert( prof_out_file );
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            prof_out_file, (VT_MPI_INT)prof_out_file_length,
                            MPI_CHAR, MPI_COMM_WORLD ) );
      Params.prof_out_file = prof_out_file;
      delete [] prof_out_file;

      // prof_sort_flags
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(Params.prof_sort_flags), 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );
#endif // VT_UNIFY_HOOKS_PROF
   } // SLAVE

   // free buffer
   delete [] buffer;

   return !error;
}

static bool
shareUnifyControls()
{
   bool error = false;

   vt_assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   VPrint( 2, " Sharing unify control data\n" );

   char * buffer;
   VT_MPI_INT buffer_size;
   VT_MPI_INT position;
   uint32_t unify_ctl_size;
   uint32_t i;

   MASTER unify_ctl_size = UnifyCtls.size();

#ifdef VT_ETIMESYNC
   // create MPI datatypes for ...
   //

   VT_MPI_INT blockcounts[3];
   MPI_Aint displ[3];
   MPI_Datatype oldtypes[3];

   // ... ETimeSyncC::SyncPhaseS
   //
   ETimeSyncC::SyncPhaseS sync_phase_struct;
   MPI_Datatype sync_phase_newtype;

   blockcounts[0] = blockcounts[1] = blockcounts[2] = 1;
   oldtypes[0] = MPI_UNSIGNED;
   oldtypes[1] = oldtypes[2] = MPI_LONG_LONG_INT;

   CALL_MPI( MPI_Address( &(sync_phase_struct.mapid), &displ[0] ) );
   CALL_MPI( MPI_Address( &(sync_phase_struct.time), &displ[1] ) );
   CALL_MPI( MPI_Address( &(sync_phase_struct.duration), &displ[2] ) );
   displ[1] -= displ[0]; displ[2] -= displ[0]; displ[0] = 0;

   CALL_MPI( MPI_Type_struct( 3, blockcounts, displ, oldtypes,
                              &sync_phase_newtype ) );
   CALL_MPI( MPI_Type_commit( &sync_phase_newtype ) );

   // ... ETimeSyncC::SyncTimeS
   //
   ETimeSyncC::SyncTimeS sync_time_struct;
   MPI_Datatype sync_time_newtype;

   blockcounts[0] = 4; blockcounts[1] = 1;
   oldtypes[0] = MPI_LONG_LONG_INT;
   oldtypes[1] = MPI_UNSIGNED;

   CALL_MPI( MPI_Address( &(sync_time_struct.t), &displ[0] ) );
   CALL_MPI( MPI_Address( &(sync_time_struct.phase_idx), &displ[1] ) );
   displ[1] -= displ[0]; displ[0] = 0;

   CALL_MPI( MPI_Type_struct( 2, blockcounts, displ, oldtypes,
                              &sync_time_newtype ) );
   CALL_MPI( MPI_Type_commit( &sync_time_newtype ) );
#endif // VT_ETIMESYNC

   // get buffer size needed to pack unify control data
   //
   MASTER
   {
      VT_MPI_INT size;

      buffer_size = 0;

      // UnifyCtls.size() + mode_flags + iofsl_num_servers + iofsl_mode
      //
      CALL_MPI( MPI_Pack_size( 4, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
      buffer_size += size;

      for( i = 0; i < unify_ctl_size; i++ )
      {
         // streamid + pstreamid
         //
         CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // stream_avail
         //
         CALL_MPI( MPI_Pack_size( 1, MPI_CHAR, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // ltime, offset
         //
         CALL_MPI( MPI_Pack_size( 4, MPI_LONG_LONG_INT, MPI_COMM_WORLD,
                                  &size ) );
         buffer_size += size;

#ifdef VT_ETIMESYNC
         // sync_offset
         //
         CALL_MPI( MPI_Pack_size( 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD,
                                  &size ) );
         buffer_size += size;

         // sync_drift
         //
         CALL_MPI( MPI_Pack_size( 1, MPI_DOUBLE, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // sync_phases.size(), sync_times.size(),
         // sync_pairs.size()
         //
         CALL_MPI( MPI_Pack_size( 3, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // sync_phases
         //
         if( UnifyCtls[i]->sync_phases.size() > 0 )
         {
            CALL_MPI( MPI_Pack_size(
                         (VT_MPI_INT)UnifyCtls[i]->sync_phases.size(),
                         sync_phase_newtype,
                         MPI_COMM_WORLD, &size ) );
            buffer_size += size;
         }

         // sync_times
         if( UnifyCtls[i]->sync_times.size() > 0 )
         {
            CALL_MPI( MPI_Pack_size(
                         (VT_MPI_INT)UnifyCtls[i]->sync_times.size(),
                         sync_time_newtype,
                         MPI_COMM_WORLD, &size ) );
            buffer_size += size;
         }

         // sync_pairs
         //
         if( UnifyCtls[i]->sync_pairs.size() > 0 )
         {
            CALL_MPI(
               MPI_Pack_size(
                  (VT_MPI_INT)UnifyCtls[i]->sync_pairs.size() * 2,
                  MPI_UNSIGNED, MPI_COMM_WORLD,
                  &size ) );
            buffer_size += size;
         }
#endif // VT_ETIMESYNC
      }
   } // MASTER

   // share buffer size
   CALL_MPI( MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD ) );

   // allocate buffer
   //
   buffer = new char[buffer_size];
   vt_assert( buffer );

   // pack unify control data
   //
   MASTER
   {
      position = 0;

      // UnifyCtls.size()
      CALL_MPI( MPI_Pack( &unify_ctl_size, 1, MPI_UNSIGNED, buffer, buffer_size,
                          &position, MPI_COMM_WORLD ) );

      // mode_flags
      CALL_MPI( MPI_Pack( &(UnifyControlS::mode_flags), 1, MPI_UNSIGNED,
                          buffer, buffer_size, &position, MPI_COMM_WORLD ) );

      // iofsl_num_servers
      CALL_MPI( MPI_Pack( &(UnifyControlS::iofsl_num_servers), 1, MPI_UNSIGNED,
                          buffer, buffer_size, &position, MPI_COMM_WORLD ) );

      // iofsl_mode
      CALL_MPI( MPI_Pack( &(UnifyControlS::iofsl_mode), 1, MPI_UNSIGNED,
                          buffer, buffer_size, &position, MPI_COMM_WORLD ) );

      for( i = 0; i < unify_ctl_size; i++ )
      {
         // streamid
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->streamid), 1, MPI_UNSIGNED,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

         // pstreamid
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->pstreamid), 1, MPI_UNSIGNED,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

         // stream_avail
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->stream_avail), 1, MPI_CHAR,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );
         // ltime
         CALL_MPI( MPI_Pack( UnifyCtls[i]->ltime, 2, MPI_LONG_LONG_INT,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

         // offset
         CALL_MPI( MPI_Pack( UnifyCtls[i]->offset, 2, MPI_LONG_LONG_INT,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

#ifdef VT_ETIMESYNC
         // sync_offset
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->sync_offset), 1,
                             MPI_LONG_LONG_INT, buffer, buffer_size, &position,
                             MPI_COMM_WORLD ) );

         // sync_drift
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->sync_drift), 1, MPI_DOUBLE,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

         // sync_phases.size()
         //
         uint32_t sync_phases_size = UnifyCtls[i]->sync_phases.size();
         CALL_MPI( MPI_Pack( &sync_phases_size, 1, MPI_UNSIGNED, buffer,
                             buffer_size, &position, MPI_COMM_WORLD ) );

         // sync_phases
         //
         if( sync_phases_size > 0 )
         {
            uint32_t j;

            ETimeSyncC::SyncPhaseS * sync_phases =
               new ETimeSyncC::SyncPhaseS[sync_phases_size];
            for( j = 0; j < sync_phases_size; j++ )
               sync_phases[j] = UnifyCtls[i]->sync_phases[j];

            CALL_MPI( MPI_Pack( sync_phases, (VT_MPI_INT)sync_phases_size,
                                sync_phase_newtype, buffer, buffer_size,
                                &position, MPI_COMM_WORLD ) );

            delete [] sync_phases;
         }

         // sync_times.size()
         //
         uint32_t sync_times_size = UnifyCtls[i]->sync_times.size();
         CALL_MPI( MPI_Pack( &sync_times_size, 1, MPI_UNSIGNED, buffer,
                             buffer_size, &position, MPI_COMM_WORLD ) );

         // sync_times
         //
         if( sync_times_size > 0 )
         {
            uint32_t j;

            ETimeSyncC::SyncTimeS * sync_times =
               new ETimeSyncC::SyncTimeS[sync_times_size];
            for( j = 0; j < sync_times_size; j++ )
               sync_times[j] = UnifyCtls[i]->sync_times[j];

            CALL_MPI( MPI_Pack( sync_times, (VT_MPI_INT)sync_times_size,
                                sync_time_newtype, buffer, buffer_size,
                                &position, MPI_COMM_WORLD ) );

            delete [] sync_times;
         }

         // sync_pairs.size()
         //
         uint32_t sync_pairs_size = UnifyCtls[i]->sync_pairs.size();
         CALL_MPI( MPI_Pack( &sync_pairs_size, 1, MPI_UNSIGNED, buffer,
                             buffer_size, &position, MPI_COMM_WORLD ) );

         // sync_pairs
         //
         if( sync_pairs_size > 0 )
         {
            uint32_t * sync_pairs_firsts = new uint32_t[sync_pairs_size];
            uint32_t * sync_pairs_seconds = new uint32_t[sync_pairs_size];
            uint32_t j;

            for( j = 0; j < sync_pairs_size; j++ )
            {
               sync_pairs_firsts[j] = UnifyCtls[i]->sync_pairs[j].first;
               sync_pairs_seconds[j] = UnifyCtls[i]->sync_pairs[j].second;
            }

            CALL_MPI( MPI_Pack( sync_pairs_firsts, (VT_MPI_INT)sync_pairs_size,
                                MPI_UNSIGNED, buffer, buffer_size, &position,
                                MPI_COMM_WORLD ) );
            CALL_MPI( MPI_Pack( sync_pairs_seconds, (VT_MPI_INT)sync_pairs_size,
                                MPI_UNSIGNED, buffer, buffer_size, &position,
                                MPI_COMM_WORLD ) );

            delete [] sync_pairs_firsts;
            delete [] sync_pairs_seconds;
         }
#endif // VT_ETIMESYNC
      }
   } // MASTER

   // share packed unify control data
   CALL_MPI( MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD ) );

   // unpack unify control data
   //
   SLAVE
   {
      position = 0;

      // UnifyCtls.size()
      //
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &unify_ctl_size, 1,
                            MPI_UNSIGNED, MPI_COMM_WORLD ) );
      UnifyCtls.resize( unify_ctl_size );

      // mode_flags
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(UnifyControlS::mode_flags), 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );

      // iofsl_num_servers
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(UnifyControlS::iofsl_num_servers), 1,
                            MPI_UNSIGNED, MPI_COMM_WORLD ) );

      // iofsl_mode
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                            &(UnifyControlS::iofsl_mode), 1, MPI_UNSIGNED,
                            MPI_COMM_WORLD ) );

      for( i = 0; i < unify_ctl_size; i++ )
      {
         // create new unify control element
         UnifyCtls[i] = new UnifyControlS();

         // streamid
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->streamid), 1, MPI_UNSIGNED,
                               MPI_COMM_WORLD ) );

         // pstreamid
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->pstreamid), 1, MPI_UNSIGNED,
                               MPI_COMM_WORLD ) );

         // stream_avail
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->stream_avail), 1, MPI_CHAR,
                               MPI_COMM_WORLD ) );

         // ltime
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               UnifyCtls[i]->ltime, 2, MPI_LONG_LONG_INT,
                               MPI_COMM_WORLD ) );

         // offset
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               UnifyCtls[i]->offset, 2, MPI_LONG_LONG_INT,
                               MPI_COMM_WORLD ) );

#ifdef VT_ETIMESYNC
         // sync_offset
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->sync_offset), 1,
                               MPI_LONG_LONG_INT, MPI_COMM_WORLD ) );

         // sync_drift
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->sync_drift), 1, MPI_DOUBLE,
                               MPI_COMM_WORLD ) );

         // sync_phases.size()
         //
         uint32_t sync_phases_size;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &sync_phases_size, 1, MPI_UNSIGNED,
                               MPI_COMM_WORLD ) );

         // sync_phases
         //
         if( sync_phases_size > 0 )
         {
            uint32_t j;

            theTimeSync->setSyncMethod( TimeSyncC::METHOD_ENHANCED );

            ETimeSyncC::SyncPhaseS * sync_phases =
               new ETimeSyncC::SyncPhaseS[sync_phases_size];

            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, sync_phases,
                                  (VT_MPI_INT)sync_phases_size,
                                  sync_phase_newtype, MPI_COMM_WORLD ) );

            for( j = 0; j < sync_phases_size; j++ )
               UnifyCtls[i]->sync_phases.push_back( sync_phases[j] );

            delete [] sync_phases;
         }

         // sync_times.size()
         //
         uint32_t sync_times_size;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &sync_times_size,
                               1, MPI_UNSIGNED, MPI_COMM_WORLD ) );

         // sync_times
         //
         if( sync_times_size > 0 )
         {
            uint32_t j;

            ETimeSyncC::SyncTimeS * sync_times =
               new ETimeSyncC::SyncTimeS[sync_times_size];

            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, sync_times,
                                  (VT_MPI_INT)sync_times_size,
                                  sync_time_newtype, MPI_COMM_WORLD ) );

            for( j = 0; j < sync_times_size; j++ )
               UnifyCtls[i]->sync_times.push_back( sync_times[j] );

            delete [] sync_times;
         }

         // sync_pairs.size()
         //
         uint32_t sync_pairs_size;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &sync_pairs_size,
                               1, MPI_UNSIGNED, MPI_COMM_WORLD ) );

         // sync_pairs
         //
         if( sync_pairs_size > 0 )
         {
            uint32_t * sync_pairs_firsts = new uint32_t[sync_pairs_size];
            uint32_t * sync_pairs_seconds = new uint32_t[sync_pairs_size];
            uint32_t j;

            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                                  sync_pairs_firsts,
                                  (VT_MPI_INT)sync_pairs_size, MPI_UNSIGNED,
                                  MPI_COMM_WORLD ) );
            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                                  sync_pairs_seconds,
                                  (VT_MPI_INT)sync_pairs_size, MPI_UNSIGNED,
                                  MPI_COMM_WORLD ) );

            for( j = 0; j < sync_pairs_size; j++ )
            {
               UnifyCtls[i]->sync_pairs.push_back(
                  std::make_pair( sync_pairs_firsts[j], sync_pairs_seconds[j] ) );
            }

            delete [] sync_pairs_firsts;
            delete [] sync_pairs_seconds;
         }
#endif // VT_ETIMESYNC
      }
   } // SLAVE

   // free buffer
   delete [] buffer;

#ifdef VT_ETIMESYNC
   // free MPI datatypes
   CALL_MPI( MPI_Type_free( &sync_phase_newtype ) );
   CALL_MPI( MPI_Type_free( &sync_time_newtype ) );
#endif // VT_ETIMESYNC

   return !error;
}

#endif // VT_MPI

void
VPrint( uint8_t level, const char * fmt, ... )
{
   va_list ap;

   if( Params.verbose_level >= level )
   {
      MASTER
      {
#ifdef VT_UNIFY_VERBOSE_TIME_PREFIX
         char tstamp[STRBUFSIZE];
         time_t t;
         time( &t );
         ctime_r( &t, tstamp );
         tstamp[strlen(tstamp)-1] = '\0';
         printf( "%s: ", tstamp );
#endif // VT_UNIFY_VERBOSE_TIME_PREFIX

         va_start( ap, fmt );
         vfprintf( verboseStream, fmt, ap );
         va_end( ap );
      } // MASTER
   }
}

void
PVPrint( uint8_t level, const char * fmt, ... )
{
   va_list ap;

   if( Params.verbose_level >= level )
   {
#ifdef VT_UNIFY_VERBOSE_TIME_PREFIX
      char tstamp[STRBUFSIZE];
      time_t t;
      time( &t );
      ctime_r( &t, tstamp );
      tstamp[strlen(tstamp)-1] = '\0';
      printf( "%s: ", tstamp );
#endif // VT_UNIFY_VERBOSE_TIME_PREFIX

      va_start( ap, fmt );
#if !(defined(VT_MPI) || (defined(HAVE_OMP) && HAVE_OMP))
      vfprintf( verboseStream, fmt, ap );
#else // !(VT_MPI || HAVE_OMP)
      char msg[STRBUFSIZE] = "";
#  if defined(VT_MPI) && !(defined(HAVE_OMP) && HAVE_OMP)
      snprintf( msg, sizeof(msg)-1, "[%d] ", (int)MyRank );
#  elif !defined(VT_MPI) && (defined(HAVE_OMP) && HAVE_OMP)
      if( omp_in_parallel() )
         snprintf( msg, sizeof(msg)-1, "[%d] ", omp_get_thread_num() );
#  else // !VT_MPI && HAVE_OMP
      if( omp_in_parallel() )
      {
         snprintf( msg, sizeof(msg)-1, "[%d:%d] ", (int)MyRank,
                   omp_get_thread_num() );
      }
      else
      {
         snprintf( msg, sizeof(msg)-1, "[%d] ", (int)MyRank );
      }
#  endif // !VT_MPI && HAVE_OMP
      vsnprintf(msg + strlen(msg), sizeof(msg)-1, fmt, ap);
#  if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp critical
#  endif // HAVE_OMP
      fprintf( verboseStream, "%s", msg );
#endif // !(VT_MPI || HAVE_OMP)
      va_end( ap );
   }
}

bool
SyncError( bool * error )
{
#if defined(VT_MPI) && defined(SYNC_ERROR)
   if( NumRanks > 1 )
   {
      VT_MPI_INT sendbuf = *error ? 1 : 0;
      VT_MPI_INT recvbuf;
      CALL_MPI( MPI_Allreduce( &sendbuf, &recvbuf, 1, MPI_INT, MPI_MAX,
                               MPI_COMM_WORLD ) );
      *error = ( recvbuf > 0 );
   }
#endif // VT_MPI && SYNC_ERROR

   return *error;
}
