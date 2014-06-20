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

#ifndef _VT_UNIFY_ESYNC_H_
#define _VT_UNIFY_ESYNC_H_

#include "vt_unify_config.h"

#include "vt_inttypes.h"

#include <map>
#include <vector>

//
// ETimeSyncC class
//
class ETimeSyncC
{
public:

   //
   // synchronization phase structure
   //
   struct SyncPhaseS
   {
      SyncPhaseS()
         : mapid( 0 ), time( 0 ), duration( 0 ) {}

      SyncPhaseS( const uint32_t & _mapid, const uint64_t & _time,
                  const uint64_t & _duration )
         : mapid( _mapid ), time( _time ), duration( _duration ) {}

      uint32_t mapid;
      uint64_t time;
      uint64_t duration;
   };

   //
   // synchronization timestamp structure
   //
   struct SyncTimeS
   {
      SyncTimeS()
         : phase_idx ( 0 )
      {
         t[0] = t[1] = t[2] = t[3] = 0;
      }

      ~SyncTimeS() {}

      uint64_t t[4];
      uint32_t phase_idx;
   };

   // constructor
   ETimeSyncC();

   // destructor
   ~ETimeSyncC();

   // calculate timer parameters
   bool initialize();

   // update timer parameters of certain process
   void updateSyncParam( const uint32_t & proc );

   // reset timer parameters of certain process
   void resetSyncParam( const uint32_t & proc );

   // set start time of certain stream
   void setStartTime( const uint32_t & streamId, const uint64_t & startTime )
   {
      m_streamId2StartTime.insert(
         std::make_pair( streamId, startTime ) );
   }

   // get start time of certain stream
   uint64_t getStartTime( const uint32_t & streamId ) const
   {
      std::map<uint32_t, uint64_t>::const_iterator it =
         m_streamId2StartTime.find( streamId );
      vt_assert( it != m_streamId2StartTime.end() );

      return it->second;
   }

private:

   //
   // sychronization parameter structure
   //
   struct SyncParamS
   {
      SyncParamS()
         : offset( 0 ), drift( 1.0 ) {}

      SyncParamS( const int64_t & _offset, const double & _drift )
         : offset( _offset ), drift( _drift ) {}

      ~SyncParamS() {}

      int64_t offset;
      double  drift;
   };

   bool calcSync( uint32_t round,
           std::map<std::pair<uint32_t, uint32_t>, SyncTimeS*> & firstTimeStamps,
           std::map<std::pair<uint32_t, uint32_t>, SyncTimeS*> & lastTimeStamps );

   void print(double *a, int m, int n, char* info);

#ifdef VT_MPI

   bool distStartTimes();

#endif // VT_MPI

   std::map<uint32_t, std::map<std::pair<uint32_t, uint32_t>, SyncTimeS*>*>
      m_syncPhasemapProcessIds2Timestamps;
   std::map<uint32_t, std::vector<struct SyncParamS*>*> m_idxvecSyncParam;
   std::map<uint32_t, uint64_t> m_streamId2StartTime;
   std::vector<double> m_syncPreCorrection;
   std::map<uint32_t, uint32_t> m_streamId2RoundNum;
   uint32_t m_procNum;
   uint32_t m_roundMax;

};

#endif // _VT_UNIFY_ESYNC_H_
