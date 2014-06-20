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

#ifndef _VT_UNIFY_H_
#define _VT_UNIFY_H_

#include "vt_unify_config.h"

#ifdef VT_MPI
#  include "vt_unify_mpi.h"
#endif // VT_MPI

#ifdef VT_ETIMESYNC
#  include "vt_unify_esync.h"
#endif // VT_ETIMESYNC

#include "vt_defs.h"
#include "vt_inttypes.h"

#include <map>
#include <set>
#include <string>
#include <vector>

#if defined(HAVE_OMP) && HAVE_OMP
#  include <omp.h>
#endif // HAVE_OMP

#define STRBUFSIZE 1024

// define to prefix each verbose message with a timestamp
//#define VT_UNIFY_VERBOSE_TIME_PREFIX

// define to remove references to undefined processes from records
#define VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

#ifdef VT_MPI
#  define MASTER if( MyRank == 0 )
#  define SLAVE  if( MyRank != 0 )
   // define to synchronize the error indicator between all ranks
//#  define SYNC_ERROR
#else // VT_MPI
#  define MASTER
#endif // VT_MPI

//
// unify parameter structure (contains the program options)
//
struct ParamsS
{
   ParamsS()
      : verbose_level( 0 ), docompress( false ), doclean( true ),
        showusage( false ), showversion( false ), showprogress( false ),
        bequiet( false ), onlystats( false )
   {
#ifndef VT_LIB
      autostart = false;
#endif // VT_LIB

#if defined(HAVE_ZLIB) && HAVE_ZLIB
      docompress = true;
#endif // HAVE_ZLIB

#if defined(HAVE_IOFSL) && HAVE_IOFSL
      iofsl_mode = VT_IOFSL_MODE_MULTIFILE_SPLIT;
      iofsl_flags = 0;
      iofsl_num_servers = 0;
      iofsl_servers = 0;
#endif // HAVE_IOFSL

#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
      domsgmatch = true;
      droprecvs = false;
      createsnaps = true;
      maxsnapshots = 1024;
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

#ifdef VT_UNIFY_HOOKS_PROF
      prof_sort_flags = 0x22;
#endif // VT_UNIFY_HOOKS_PROF
   }

   ~ParamsS()
   {
#if defined(HAVE_IOFSL) && HAVE_IOFSL
      if( iofsl_num_servers > 0 )
      {
         for( uint32_t i = 0; i < iofsl_num_servers; i++ )
            delete [] iofsl_servers[i];
         delete [] iofsl_servers;
      }
#endif // HAVE_IOFSL
   }

   std::string in_file_prefix;    // input trace file prefix
   std::string out_file_prefix;   // output trace file prefix
   uint32_t    verbose_level;     // verbose level
   bool        docompress;        // flag: do compress output trace?
   bool        doclean;           // flag: do remove local trace?
   bool        showusage;         // flag: show usage text?
   bool        showversion;       // flag: show VampirTrace version?
   bool        showprogress;      // flag: show progress?
   bool        bequiet;           // flag: print no messages?
   bool        onlystats;         // flag: unify only summarized information?

#ifndef VT_LIB
   bool        autostart;         // flag: vtunify started automatically after
                                  //       program termination?
#endif // VT_LIB

#if defined(HAVE_IOFSL) && HAVE_IOFSL
   // IOFSL parameters
   //
   bool        doiofsl() const { return iofsl_num_servers > 0; }
   uint32_t    iofsl_mode;        // IOFSL mode (either VT_IOFSL_MODE_MULTIFILE
                                  // or VT_IOFSL_MODE_MULTIFILE_SPLIT)
   uint32_t    iofsl_flags;       // IOFSL flags bitmask
   uint32_t    iofsl_num_servers; // number of IOFSL servers
   char**      iofsl_servers;     // IOFSL server addresses
#endif // HAVE_IOFSL

#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
   // HooksMsgMatchAndSnapsC's parameters
   //
   bool        domsgmatch;        // flag: do match messages?
   bool        droprecvs;         // flag: drop receive messages, if matching?
   bool        createsnaps;       // flag: create snapshots?
   uint32_t    maxsnapshots;      // max. number of snapshots to generate
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

#ifdef VT_UNIFY_HOOKS_PROF
   // HooksProfC's parameters
   //
   std::string prof_out_file;     // profile output file
   uint32_t    prof_sort_flags;   // profile sort flags
#endif // VT_UNIFY_HOOKS_PROF

};

//
// unify control structure for each input stream
//
struct UnifyControlS
{
#ifndef VT_ETIMESYNC
   UnifyControlS()
      : streamid( 0 ), pstreamid( 0 ), stream_avail( true )
   {
      ltime[0] = ltime[1] = offset[0] = offset[1] = 0;
   }

   UnifyControlS( const uint32_t & _streamid, const uint32_t & _pstreamid,
                  const bool & _stream_avail, const int64_t * _ltime,
                  const int64_t * _offset )
      : streamid( _streamid ), pstreamid( _pstreamid ),
        stream_avail( _stream_avail )
   {
      ltime[0] = _ltime[0]; ltime[1] = _ltime[1];
      offset[0] = _offset[0]; offset[1] = _offset[1];
   }
#else // VT_ETIMESYNC
   UnifyControlS()
      : streamid( 0 ), pstreamid( 0 ), stream_avail( true ), sync_offset( 0 ),
        sync_drift( 0.0 )
   {
      ltime[0] = ltime[1] = offset[0] = offset[1] = 0;
   }

   UnifyControlS( const uint32_t & _streamid, const uint32_t & _pstreamid,
                  const bool & _stream_avail, const int64_t * _ltime,
                  const int64_t * _offset,
                  const std::vector<ETimeSyncC::SyncPhaseS> & _sync_phases,
                  const std::vector<ETimeSyncC::SyncTimeS> & _sync_times,
                  const std::vector<std::pair<uint32_t, uint32_t> > & _sync_pairs)
      : streamid( _streamid ), pstreamid( _pstreamid ),
        stream_avail( _stream_avail ), sync_offset( 0 ), sync_drift( 1.0 ),
        sync_phases( _sync_phases ), sync_times( _sync_times ),
        sync_pairs( _sync_pairs )
   {
      ltime[0] = _ltime[0]; ltime[1] = _ltime[1];
      offset[0] = _offset[0]; offset[1] = _offset[1];
   }
#endif // VT_ETIMESYNC

   static bool have_events() { return ( mode_flags & VT_MODE_TRACE ) != 0; }
   static bool have_stats()  { return ( mode_flags & VT_MODE_STAT ) != 0;  }
   static uint32_t mode_flags;        // VT mode flags bitmask
                                      // (VT_MODE_TRACE and/or VT_MODE_STAT)

   // IOFSL properties
   //
   static bool is_iofsl() { return iofsl_num_servers > 0; }
   static uint32_t iofsl_num_servers; // number of IOFSL servers used
   static uint32_t iofsl_mode;        // IOFSL mode
                                      // (VT_IOFSL_MODE_MULTIFILE or
                                      //  VT_IOFSL_MODE_MULTIFILE_SPLIT)

   uint32_t    streamid;              // id of input stream
   uint32_t    pstreamid;             // id of parent input stream
   bool        stream_avail;          // is stream available?

   // time sync. information
   //
   int64_t     ltime[2];              // local times ...
   int64_t     offset[2];             // ... and chronological offsets to
                                      // global time
#ifdef VT_ETIMESYNC
   uint64_t    sync_offset;
   double      sync_drift;
   std::vector<ETimeSyncC::SyncPhaseS>         sync_phases;
   std::vector<ETimeSyncC::SyncTimeS>          sync_times;
   std::vector<std::pair<uint32_t, uint32_t> > sync_pairs;
#endif // VT_ETIMESYNC

};

// print verbose message
void VPrint( uint8_t level, const char * fmt, ... );

// print verbose message in a parallel region
void PVPrint( uint8_t level, const char * fmt, ... );

// synchronize error indicator between all ranks
bool SyncError( bool * error );

// global variables
//

// name of program's executable
extern const std::string ExeName;

// temporary output file suffix
extern const std::string TmpFileSuffix;

// output file prefix which used if local input files shall be kept
extern const std::string UniFilePrefix;

// unify parameters
extern ParamsS Params;

// vector of unify controls
extern std::vector<UnifyControlS*> UnifyCtls;

// map stream id <-> unify control
extern std::map<uint32_t, UnifyControlS*> StreamId2UnifyCtl;

// vector of stream ids to process by my rank
extern std::vector<uint32_t> MyStreamIds;

// number of available streams
extern uint32_t NumAvailStreams;

#ifdef VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES
   // set of absent stream ids (for fast searching)
   extern std::set<uint32_t> AbsentStreamIds;
#endif // VT_UNIFY_REMOVE_UNDEF_PROCID_REFERENCES

#ifdef VT_MPI
   // number of MPI-ranks
   extern VT_MPI_INT NumRanks;

   // MPI-rank of calling process
   extern VT_MPI_INT MyRank;

   // map stream id <-> processing MPI-rank
   extern std::map<uint32_t, VT_MPI_INT> StreamId2Rank;

   // map MPI-rank <-> stream ids
   extern std::map<VT_MPI_INT, std::set<uint32_t> > Rank2StreamIds;
#endif // VT_MPI

#endif // _VT_UNIFY_H_
