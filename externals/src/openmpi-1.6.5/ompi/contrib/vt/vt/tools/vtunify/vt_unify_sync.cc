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

#include "vt_unify_hooks.h"
#include "vt_unify_sync.h"

// instance of class TimeSyncC
TimeSyncC * theTimeSync = 0;

//////////////////// class TimeSyncC ////////////////////

// public methods
//

TimeSyncC::TimeSyncC()
   : m_syncMethod( METHOD_OFFSET ), m_minStartTime( 0 )
{
#ifdef VT_ETIMESYNC
   m_eTimeSync = 0;
#endif // VT_ETIMESYNC
}

TimeSyncC::~TimeSyncC()
{
#ifdef VT_ETIMESYNC
   if( m_eTimeSync )
      delete m_eTimeSync;
#endif // VT_ETIMESYNC
}

bool
TimeSyncC::initialize()
{
   bool error = false;

#ifdef VT_MPI
   // block until all ranks have reached this point
   if( NumRanks > 1 )
      CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );
#endif // VT_MPI

   VPrint( 2, " Initializing time synchronization\n" );

   do
   {
#ifdef VT_ETIMESYNC
      // initialize enhanced time sync., if necessary
      //
      if( m_syncMethod == METHOD_ENHANCED )
      {
         // create instance of class ETimeSyncC
         //
         m_eTimeSync = new ETimeSyncC();
         vt_assert( m_eTimeSync );

         MASTER
         {
            // set start times of input streams
            //
            for( std::map<uint32_t, TimeRangeT>::const_iterator it =
                 m_proc2TimeRange.begin(); it != m_proc2TimeRange.end(); ++it )
            {
               const uint32_t & streamid = it->first;
               const uint64_t & mintime = it->second.first;

               m_eTimeSync->setStartTime( streamid, mintime );
            }
         }

         // calculate timer parameters
         //
         error = !m_eTimeSync->initialize();
         if( SyncError( &error ) )
            break;
      }
#endif // VT_ETIMESYNC

      MASTER
      {
         // get minimum start time
         //
         uint64_t global_mintime = (uint64_t)-1;
         for( std::map<uint32_t, TimeRangeT>::const_iterator it =
              m_proc2TimeRange.begin(); it != m_proc2TimeRange.end(); ++it )
         {
            const uint32_t & proc = it->first;
            const uint64_t mintime = correctTime( proc, it->second.first );

            // update minimum start time, if necessary
            if( mintime < global_mintime )
               global_mintime = mintime;
         }
         m_minStartTime = global_mintime;

         // set global time range
         //
         uint64_t global_maxtime = 0;
         for( std::map<uint32_t, TimeRangeT>::const_iterator it =
              m_proc2TimeRange.begin(); it != m_proc2TimeRange.end(); ++it )
         {
            const uint32_t & proc = it->first;
            const uint64_t maxtime = correctTime( proc, it->second.second );

            // update maximum time, if necessary
            if( maxtime > global_maxtime )
               global_maxtime = maxtime;
         }
         m_proc2TimeRange[0] = TimeRangeT( 0, global_maxtime );
      }

#ifdef VT_MPI
      // share minimum start time to all ranks
      //
      if( NumRanks > 1 )
      {
         CALL_MPI( MPI_Bcast( &m_minStartTime, 1, MPI_LONG_LONG_INT, 0,
                              MPI_COMM_WORLD ) );
      }
#endif // VT_MPI

   } while( false );

   return !error;
}
