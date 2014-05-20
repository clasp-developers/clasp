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
#include "vt_unify_esync.h"

#include <fstream>
#include <iostream>
#include <list>
#include <sstream>
#include <string>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if defined(HAVE_MKL) && HAVE_MKL
#  include "mkl.h"
#  define CLAPACK_INT    MKL_INT
#  define CLAPACK_DOUBLE double
#  define dgemm_ dgemm

#elif defined(HAVE_ACML) && HAVE_ACML
#  include "acml.h"
#  define CLAPACK_INT    int
#  define CLAPACK_DOUBLE double
#  define dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, \
                 _beta, _c, _ldc) \
   dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, _beta, \
          _c, _ldc, 1, 1)
#  define dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
                 _info) \
   dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
          _info, 1)

#elif defined(HAVE_ESSL) && HAVE_ESSL
#  include "essl.h"
#  define CLAPACK_INT    _ESVINT
#  define CLAPACK_DOUBLE double
#  define dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, \
                 _beta, _c, _ldc) \
   esvdgemm(_transa, _transb, *(_m), *(_n), *(_k), *(_alpha), (void*)(_a), \
            *(_lda), (void*)(_b), *(_ldb), *(_beta), (void*)(_c), *(_ldc))
#  define dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
                 _info) \
   esvdgels(_trans, *(_m), *(_n), *(_nrhs), (void*)(_a), *(_lda), \
            (void*)(_b), *(_ldb), _work, *(_lwork), *(_info))

#elif defined(HAVE_SUNPERF) && HAVE_SUNPERF
#  include "sunperf.h"
#  define CLAPACK_INT    int
#  define CLAPACK_DOUBLE double
  // since Sun Studio 12 the prototype was extended
#  if __SUNPRO_CC >= 0x590
#     define dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, \
                    _beta, _c, _ldc)                                      \
      dgemm_(_transa, _transb, _m, _n, _k, _alpha, _a, _lda, _b, _ldb, _beta, \
             _c, _ldc, 1, 1)
#     define dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
                    _info)                                                \
      dgels_(_trans, _m, _n, _nrhs, _a, _lda, _b, _ldb, _work, _lwork, \
             _info, 1)
#  endif // __SUNPRO_CC >= 0x590

#else // CLAPACK
   extern "C" {
#  include "f2c.h"
#  include "clapack.h"
#  include "blaswrap.h"
   int dgemm_(char*, char*, integer*, integer*, integer*, doublereal*,
              doublereal*, integer*, doublereal*, integer*, doublereal*,
              doublereal*, integer*);
   } // extern "C"
#  define CLAPACK_INT    integer
#  define CLAPACK_DOUBLE doublereal
#endif // CLAPACK

//////////////////// class ETimeSyncC ////////////////////

// public methods
//

ETimeSyncC::ETimeSyncC()
{
   uint32_t i, j;

   m_procNum = 0;

   for( i = 0; i < UnifyCtls.size(); i++ )
   {
      vt_assert( UnifyCtls[i]->sync_phases.size() > 0 );

      // insert map entry for stream id / round num
      m_streamId2RoundNum.insert(
         std::make_pair( (uint32_t)UnifyCtls[i]->streamid, 0 ) );

      if( !UnifyCtls[i]->sync_times.empty() )
      {
         vt_assert( !UnifyCtls[i]->sync_pairs.empty() );
      }
      if( !UnifyCtls[i]->sync_pairs.empty() )
      {
         vt_assert( !UnifyCtls[i]->sync_times.empty() );
      }

      // continue if current process doesn't have sync. information
      if( UnifyCtls[i]->sync_times.empty() &&
          UnifyCtls[i]->sync_pairs.empty() )
      {
         continue;
      }

      vt_assert( UnifyCtls[i]->sync_times.size() ==
              UnifyCtls[i]->sync_pairs.size() );

      // set number of sync. phases, if first process
      if( i == 0 )
      {
         m_roundMax = UnifyCtls[i]->sync_phases.size();
      }
      // check whether number of sync. phases is equal to first process
      else
      {
         vt_assert( UnifyCtls[i]->sync_phases.size() == m_roundMax );
      }

      for( j = 0; j < UnifyCtls[i]->sync_times.size(); j++ )
      {
         // add timestamps to the global map
         //

         std::pair<uint32_t, uint32_t> key =
            UnifyCtls[i]->sync_pairs[j];
         SyncTimeS sync_time = UnifyCtls[i]->sync_times[j];

         std::map<uint32_t, std::map<std::pair<uint32_t, uint32_t>,
            SyncTimeS*>*>::iterator iter_map_timestamps =
            m_syncPhasemapProcessIds2Timestamps.find(sync_time.phase_idx);

         if( iter_map_timestamps == m_syncPhasemapProcessIds2Timestamps.end() )
         {
            iter_map_timestamps =
               m_syncPhasemapProcessIds2Timestamps.insert(
                  std::make_pair( sync_time.phase_idx, new std::map<std::pair<uint32_t, uint32_t>,
                                  SyncTimeS*>() ) ).first;
         }

         // update timestamps
         //

         std::map<std::pair<uint32_t, uint32_t>, SyncTimeS*>::iterator
            iter_timestamps = (iter_map_timestamps->second)->find(key);

         if( iter_timestamps != (iter_map_timestamps->second)->end() )
         {
            delete(iter_timestamps->second);
            (iter_map_timestamps->second)->erase(iter_timestamps);
         }

         iter_timestamps = (iter_map_timestamps->second)->insert(
                              std::make_pair( key, new SyncTimeS(sync_time) ) ).first;

         // set number of processes
         m_procNum = VT_MAX(m_procNum, key.second + 1);
      }
   }
}

ETimeSyncC::~ETimeSyncC()
{
   std::map<uint32_t, std::map<std::pair<uint32_t, uint32_t>,
      SyncTimeS*>*>::iterator iter_phase_time_stamps;

   // clean up timestamp map
   //
   for( iter_phase_time_stamps = m_syncPhasemapProcessIds2Timestamps.begin();
        iter_phase_time_stamps != m_syncPhasemapProcessIds2Timestamps.end();
        ++iter_phase_time_stamps )
   {
      std::map<std::pair<uint32_t, uint32_t>,
         SyncTimeS*>::iterator iter_time_stamps;

      // delete timestamps
      //
      for( iter_time_stamps  = (iter_phase_time_stamps->second)->begin() ;
           iter_time_stamps != (iter_phase_time_stamps->second)->end() ;
           ++iter_time_stamps )
      {
         if( iter_time_stamps->second != NULL )
            delete(iter_time_stamps->second);
      }
      (iter_phase_time_stamps->second)->clear();
      delete(iter_phase_time_stamps->second);
   }

   // clean up timer paramaters map
   std::map<uint32_t, std::vector<SyncParamS*>*>::iterator iter_param;

   for( iter_param  = m_idxvecSyncParam.begin();
        iter_param != m_idxvecSyncParam.end();
        ++iter_param )
   {
      // delete timer parameter
      //
      for( uint32_t i = 0; i < (iter_param->second)->size(); i++ )
      {
         if( ( *(iter_param->second))[i] != NULL )
            delete( ( *(iter_param->second))[i] );
      }
      (iter_param->second)->clear();
      delete(iter_param->second);
   }

   m_syncPhasemapProcessIds2Timestamps.clear();
   m_idxvecSyncParam.clear();
   m_streamId2StartTime.clear();
   m_syncPreCorrection.clear();
}

bool
ETimeSyncC::initialize()
{
   bool error = false;

   VPrint( 2, "  Initializing enhanced time synchronization\n" );

#ifdef VT_MPI
   // first, we need the start times of each input stream
   // which isn't guaranteed at this point; distribute them to all ranks
   if( NumRanks > 1 )
      distStartTimes();
#endif // VT_MPI

   VPrint( 2, "   Calculating timer parameters\n" );

   std::map<std::pair<uint32_t, uint32_t>, SyncTimeS*>
      first_time_stamps, last_time_stamps;

   first_time_stamps =
      *((m_syncPhasemapProcessIds2Timestamps.find(0))->second);

   last_time_stamps  =
      *((m_syncPhasemapProcessIds2Timestamps.find(m_roundMax-1))->second);

   // presynchronization step
   error = !calcSync(0, first_time_stamps, last_time_stamps);

   // calculate global time for each phase
   //
   for( uint32_t i = 1; i < m_roundMax && !error; i++ )
   {
      first_time_stamps =
         *((m_syncPhasemapProcessIds2Timestamps.find(i-1))->second);
      last_time_stamps  =
         *((m_syncPhasemapProcessIds2Timestamps.find(i))->second);
      error = !calcSync(i, first_time_stamps, last_time_stamps);
   }

   return !error;
}

void
ETimeSyncC::updateSyncParam( const uint32_t & proc )
{
   std::map<uint32_t, UnifyControlS*>::const_iterator iter_uctl =
      StreamId2UnifyCtl.find( proc );
   vt_assert( iter_uctl != StreamId2UnifyCtl.end() );

   UnifyControlS * uctl = iter_uctl->second;
   const std::vector<SyncPhaseS> & sync_phases = uctl->sync_phases;

   std::map<uint32_t, uint32_t>::iterator iter_round_num =
      m_streamId2RoundNum.find( proc );
   vt_assert( iter_round_num != m_streamId2RoundNum.end() );

   uint32_t & round_num = iter_round_num->second;
   const SyncPhaseS & phase = sync_phases[round_num];

   if( round_num >= 1 && round_num < m_roundMax - 1 )
   {
      std::map<uint32_t, std::vector<SyncParamS*>*>::const_iterator iter_param =
         m_idxvecSyncParam.find( phase.mapid );

      uctl->sync_offset = (*(iter_param->second))[round_num+1]->offset;
      uctl->sync_drift  = (*(iter_param->second))[round_num+1]->drift;
   }

   round_num++;
}

void
ETimeSyncC::resetSyncParam( const uint32_t & proc )
{
   // reset round number of given stream
   std::map<uint32_t, uint32_t>::iterator iter_round_num =
      m_streamId2RoundNum.find( proc );
   vt_assert( iter_round_num != m_streamId2RoundNum.end() );

   iter_round_num->second = 0;

   // reset offset and drift of given process
   std::map<uint32_t, UnifyControlS*>::const_iterator iter_uctl =
      StreamId2UnifyCtl.find( proc );
   vt_assert( iter_uctl != StreamId2UnifyCtl.end() );

   UnifyControlS * uctl = iter_uctl->second;
   std::vector<SyncPhaseS> & sync_phases = uctl->sync_phases;

   SyncPhaseS phase = sync_phases[0];
   std::map<uint32_t, std::vector<SyncParamS*>*>::const_iterator iter_param =
      m_idxvecSyncParam.find( phase.mapid );
   vt_assert( iter_param != m_idxvecSyncParam.end() );

   uctl->sync_offset = (*(iter_param->second))[1]->offset;
   uctl->sync_drift  = (*(iter_param->second))[1]->drift;
}

// private methods
//

bool
ETimeSyncC::calcSync( uint32_t round,
   std::map<std::pair<uint32_t, uint32_t>, SyncTimeS*> & firstTimeStamps,
   std::map<std::pair<uint32_t, uint32_t>, SyncTimeS*> & lastTimeStamps )
{
   bool error = false;

   std::cout.precision(20);
   CLAPACK_DOUBLE alpha, beta;
   CLAPACK_INT k,l,m,n;
   CLAPACK_INT lda,ldb,ldc,lwork,nrhs;
   CLAPACK_INT info;
   CLAPACK_DOUBLE* p_work;
   uint32_t id1, id2;
   char trans[2] = "N";

   k=0; l=0; m=0, n=0;

   if( m_syncPreCorrection.size() == 0 )
      m_syncPreCorrection.assign( m_procNum, 1.0 );

   // system R*X2=S (drift)
   //
   int dim_x_1 = 4*firstTimeStamps.size();
   int dim_y_1 = m_procNum;

   double *p_dmatrix_r = (double *) calloc( dim_x_1 * dim_y_1, sizeof(double) );
   double *p_dmatrix_s = (double *) calloc( dim_x_1, sizeof(double) );

   memset( (void*) p_dmatrix_r, 0, dim_x_1 * dim_y_1 * sizeof(double) );
   memset( (void*) p_dmatrix_s, 0, dim_x_1 * sizeof(double) );


   // system G*X1=D D=D+H*X2(offset)
   //
   int dim_x_2 = 3*firstTimeStamps.size();
   int dim_y_2 = m_procNum;

   double *p_dmatrix_g = (double *) calloc( dim_x_2 * dim_y_2, sizeof(double) );
   double *p_dmatrix_h = (double *) calloc( dim_x_2 * dim_y_2, sizeof(double) );
   double *p_dmatrix_d = (double *) calloc( dim_x_2, sizeof(double) );

   memset( (void*) p_dmatrix_g, 0, dim_x_2 * dim_y_2 * sizeof(double) );
   memset( (void*) p_dmatrix_h, 0, dim_x_2 * dim_y_2 * sizeof(double) );
   memset( (void*) p_dmatrix_d, 0, dim_x_2 * sizeof(double) );


   // solution matrizes
   //

   double *p_dmatrix_x  = (double *) calloc( dim_y_1+dim_y_2, sizeof(double) );
   double *p_dmatrix_x1 = (double *) calloc( dim_y_2, sizeof(double) );
   double *p_dmatrix_x2 = (double *) calloc( dim_y_1, sizeof(double) );

   memset( (void*) p_dmatrix_x , 0, ( dim_y_1 + dim_y_2 ) * sizeof(double) );
   memset( (void*) p_dmatrix_x1, 0, dim_y_2 * sizeof(double) );
   memset( (void*) p_dmatrix_x2, 0, dim_y_1 * sizeof(double) );

#if (defined (__DEBUG))
   int dim_x_3 = 12*first_time_stamps.size();
   int dim_y_3 = 2 * m_procNum;

   double *p_dmatrix_a = (double *) calloc( dim_x_3*dim_y_3 , sizeof(double) );
   double *p_dmatrix_y = (double *) calloc( dim_x_3, sizeof(double) );
   memset( (void*) p_dmatrix_a, 0, dim_x_3*dim_y_3*sizeof(double) );
   memset( (void*) p_dmatrix_y, 0, dim_x_3*sizeof(double) );
#endif

   // offset vector
   //

   int64_t *p_imatrix_offset = (int64_t*) calloc( dim_y_2, sizeof(int64_t) );

   memset( (void*) p_imatrix_offset, 0, dim_y_2 * sizeof(int64_t) );

   std::map<std::pair<uint32_t, uint32_t>, SyncTimeS*>::iterator iter;
   std::map<std::pair<uint32_t, uint32_t>, SyncTimeS*>::iterator iter2;

   // insert synchronization timestamps into the matrices
   //

   for( iter = firstTimeStamps.begin(); iter != firstTimeStamps.end(); ++iter )
   {
      id1 = (iter->first).first;
      id2 = (iter->first).second;

      SyncTimeS * p_data = (iter->second);
      SyncTimeS * p_data2;
      iter2 = lastTimeStamps.find(iter->first);

      vt_assert( iter2 != lastTimeStamps.end() );

      p_data2=iter2->second;

#if (defined (__DEBUG))

      p_dmatrix_a[ m + id1*dim_x_3 ] = 1.0;
      p_dmatrix_a[ m + (m_procNum + id1)*dim_x_3 ] = (double)p_data->t[0];
      m++;
      p_dmatrix_a[ m + id1*dim_x_3 ] = (double) 1;
      p_dmatrix_a[ m + (m_procNum + id1)*dim_x_3 ] = (double)p_data->t[3];
      m++;
      p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
      p_dmatrix_a[ m + (m_procNum + id2)*dim_x_3 ] = (double)p_data->t[1];
      m++;
      p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
      p_dmatrix_a[ m + (m_procNum + id2)*dim_x_3 ] = (double)p_data->t[2];
      m++;
      p_dmatrix_a[ m + id1*dim_x_3] = (double) -1;
      p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
      p_dmatrix_a[ m + (m_procNum + id1)*dim_x_3 ] = -(double)p_data->t[0];
      p_dmatrix_a[ m + (m_procNum + id2)*dim_x_3 ] = (double)p_data->t[1];
      m++;
      p_dmatrix_a[ m + id1*dim_x_3] = (double) 1;
      p_dmatrix_a[ m + id2*dim_x_3] = (double) -1;
      p_dmatrix_a[ m + (m_procNum + id1)*dim_x_3 ] = (double)p_data->t[3];
      p_dmatrix_a[ m + (m_procNum + id2)*dim_x_3 ] = -(double)p_data->t[2];
      m++;

      p_dmatrix_a[ m + id1*dim_x_3 ] = 1.0;
      p_dmatrix_a[ m + (m_procNum + id1)*dim_x_3 ] = (double)p_data2->t[0];
      m++;
      p_dmatrix_a[ m + id1*dim_x_3 ] = (double) 1;
      p_dmatrix_a[ m + (m_procNum + id1)*dim_x_3 ] = (double)p_data2->t[3];
      m++;
      p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
      p_dmatrix_a[ m + (m_procNum + id2)*dim_x_3 ] = (double)p_data2->t[1];
      m++;
      p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
      p_dmatrix_a[ m + (m_procNum + id2)*dim_x_3 ] = (double)p_data2->t[2];
      m++;
      p_dmatrix_a[ m + id1*dim_x_3] = (double) -1;
      p_dmatrix_a[ m + id2*dim_x_3] = (double) 1;
      p_dmatrix_a[ m + (m_procNum + id1)*dim_x_3 ] = -(double)p_data2->t[0];
      p_dmatrix_a[ m + (m_procNum + id2)*dim_x_3 ] = (double)p_data2->t[1];
      m++;
      p_dmatrix_a[ m + id1*dim_x_3] = (double) 1;
      p_dmatrix_a[ m + id2*dim_x_3] = (double) -1;
      p_dmatrix_a[ m + (m_procNum + id1)*dim_x_3 ] = (double)p_data2->t[3];
      p_dmatrix_a[ m + (m_procNum + id2)*dim_x_3 ] = -(double)p_data2->t[2];
      m++;
#endif

      double delta_offset = (double)( (int64_t)( (p_data2->t[2] - p_data->t[2]) +
                                                 (p_data2->t[1] - p_data->t[1]) ) -
                                      (int64_t)( (p_data2->t[0] - p_data->t[0]) +
                                                 (p_data2->t[3] - p_data->t[3]) ) ) / 2;

#if (defined(__DEBUG))
      std::cout << " Delta - Offset " << id1 << " " << id2 << " :"<< delta_offset << std::endl;
#endif

      // equation (1) (k-r)
      //
      p_dmatrix_r [ k + id1 * dim_x_1 ] = ( (double) ( p_data2->t[0] - p_data->t[0] ) *
                                            m_syncPreCorrection[id1] );
      p_dmatrix_r [ k + id2 * dim_x_1 ] = ( (double) ( p_data2->t[1] - p_data->t[1] ) *
                                            m_syncPreCorrection[id2] * (double)(-1.0) );
      p_dmatrix_s [ k ]                 = ( (double)( (int64_t) ( p_data2->t[0] - p_data->t[0] ) -
                                                      (int64_t) ( p_data2->t[1] - p_data->t[1] ) )
                                            + delta_offset );
      k++;

      // equation (2) (k-s)
      //

      p_dmatrix_r [ k + id1 * dim_x_1 ] = ( (double) ( p_data2->t[3] - p_data->t[0] ) *
                                            m_syncPreCorrection[id1] );
      p_dmatrix_r [ k + id2 * dim_x_1 ] = ( (double) ( p_data2->t[2] - p_data->t[1] ) *
                                            m_syncPreCorrection[id2] * (double)(-1.0) );
      p_dmatrix_s [ k ]                 = ( (double)( (int64_t) ( p_data2->t[3] - p_data->t[0] ) -
                                                      (int64_t) ( p_data2->t[2] - p_data->t[1] ) )
                                            + delta_offset );

      k++;

      // equation (3) (l-r)
      //

      p_dmatrix_r [ k + id1 * dim_x_1 ] = ( (double) ( p_data2->t[0] - p_data->t[3] ) *
                                            m_syncPreCorrection[id1] );
      p_dmatrix_r [ k + id2 * dim_x_1 ] = ( (double) ( p_data2->t[1] - p_data->t[2] ) *
                                            m_syncPreCorrection[id2] * (double)(-1.0) );
      p_dmatrix_s [ k ]                 = ( (double)( (int64_t) ( p_data2->t[0] - p_data->t[3] ) -
                                                      (int64_t) ( p_data2->t[1] - p_data->t[2] ) )
                                            + delta_offset );

      k++;

      // equation (4) (l-s)
      //

      p_dmatrix_r [ k + id1 * dim_x_1 ] = ( (double) ( p_data2->t[3] - p_data->t[3] ) *
                                            m_syncPreCorrection[id1] );
      p_dmatrix_r [ k + id2 * dim_x_1 ] = ( (double) ( p_data2->t[1] - p_data->t[1] ) *
                                            m_syncPreCorrection[id2] * (double)(-1.0) );
      p_dmatrix_s [ k ]                 = ( (double)( (int64_t) ( p_data2->t[3] - p_data->t[3]) -
                                                      (int64_t) ( p_data2->t[1] - p_data->t[1]) )
                                            + delta_offset );

      k++;
   }

   // ***** drift calculation *****

   // solve system of linear equations R*X2=S
   //

   m    = dim_x_1;
   n    = dim_y_1;
   nrhs = 1;
   lda  = dim_x_1;
   ldb  = dim_x_1;
   lwork= 2*m*n;
   p_work = new double[lwork];

   dgels_( trans, &m, &n, &nrhs, p_dmatrix_r, &lda, p_dmatrix_s, &ldb,
           p_work, &lwork, &info );

   if ( info < 0 )
   {
      std::cerr << ExeName << ": Error: Lapack routine dgels returned error in round "
                << round << std::endl;
      return false;
   }

   delete [] p_work;

   // normalization of drift vector
   //

   double sum = 0.0;
   for( uint32_t i = 0; i < m_procNum; i++ )
      sum += p_dmatrix_s[i] * m_syncPreCorrection[i];

   double normalize = 0.0;
   normalize = 1 - sum / m_procNum;

   for( uint32_t i = 0; i < m_procNum; i++ )
      p_dmatrix_x2 [i] = p_dmatrix_s[i] * m_syncPreCorrection[i] + normalize;

   if ( round == 0 )
   {
      m_syncPreCorrection.clear();
      for ( uint32_t i = 0; i < m_procNum; i++ )
         m_syncPreCorrection.push_back( p_dmatrix_x2[i] );
   }

   // ***** offset calculation *****

   if( round > 0)
   {
      for( iter = firstTimeStamps.begin(); iter != firstTimeStamps.end(); ++iter )
      {
         id1 = (iter->first).first;
         id2 = (iter->first).second;

         // insert data into the matrix
         //

         SyncTimeS *p_data = (iter->second);

//         double offset = ( (double) ( p_data->t[1] + p_data->t[2]) -
//                           (double) ( p_data->t[3] + p_data->t[0]) )/2.0;

         int64_t offset = ( (int64_t)( p_data->t[1] - p_data->t[0] ) +
                            (int64_t)( p_data->t[2] - p_data->t[3] ) )/2;

         // equation offset (1)
         //

         p_dmatrix_g [ l + id1*dim_x_2 ] = -1.0;
         p_dmatrix_g [ l + id2*dim_x_2 ] = 1.0;
         p_dmatrix_d [ l ]               = (double)( (int64_t)p_data->t[1] -
                                                     (int64_t)p_data->t[0] -
                                                     offset );
         p_dmatrix_h [ l + id1*dim_x_2 ] =  (double)p_data->t[0];
         p_dmatrix_h [ l + id2*dim_x_2 ] = -(double)p_data->t[1];

         l++;

         // equation offset (2)
         //

         p_dmatrix_g [ l + id1*dim_x_2 ] = 1.0;
         p_dmatrix_g [ l + id2*dim_x_2 ] = -1.0;
         p_dmatrix_d [ l ]               = (double)( (int64_t)p_data->t[3] -
                                                     (int64_t)p_data->t[2] +
                                                     offset );
         p_dmatrix_h [ l + id1*dim_x_2 ] = -(double)p_data->t[3];
         p_dmatrix_h [ l + id2*dim_x_2 ] =  (double)p_data->t[2];

         l++;

         // equation offset (3)
         //

         p_dmatrix_g [ l + id1*dim_x_2 ] = 2.0;
         p_dmatrix_g [ l + id2*dim_x_2 ] = -2.0;
         p_dmatrix_h [ l + id1*dim_x_2 ] = -(double)(p_data->t[0]+p_data->t[3]);
         p_dmatrix_h [ l + id2*dim_x_2 ] = (double)(p_data->t[1]+p_data->t[2]);

         l++;
      }
   }

   // system G*X1=D D=D+H*X2
   // solve equation D=D+H*X2
   //
   m = dim_x_2;
   n = 1;
   k = dim_y_2;
   alpha = 1.0;
   lda = dim_x_2;
   ldb = dim_y_2;
   beta = 1.0;
   ldc = dim_x_2;

   //  print(p_dmatrix_d, dim_x_2,1, "matrix d: ");
   //  print(p_dmatrix_h, dim_x_2,dim_y_2, "matrix h: ");

   dgemm_( trans, trans, &m, &n, &k, &alpha, p_dmatrix_h, &lda, p_dmatrix_x2, &ldb,
           &beta, p_dmatrix_d, &ldc );

   //  print(p_dmatrix_d, dim_x_2, 1, "matrix d: ");
   //  print(p_dmatrix_g, dim_x_2,dim_y_2, "matrix g: ");

   // solve linear system G*X1=D
   //
   m    = dim_x_2;
   n    = dim_y_2;
   nrhs = 1;
   lda  = dim_x_2;
   ldb  = dim_x_2;
   lwork= 2*m*n;
   p_work = new double[lwork];

   dgels_( trans, &m, &n, &nrhs, p_dmatrix_g, &lda, p_dmatrix_d, &ldb,
           p_work, &lwork, &info );

   if ( info < 0 )
   {
      std::cerr << ExeName << ": Error: Lapack routine dgels returned error in round "
                << round << std::endl;
      return false;
   }

   delete [] p_work;

   double min = p_dmatrix_d[0];
   for( uint32_t i=1; i< m_procNum; i++)
   {
      min = VT_MIN( min, p_dmatrix_d[i] );
   }

   for( uint32_t i=0; i< m_procNum; i++)
   {
      p_dmatrix_x1 [ i ] = p_dmatrix_d [ i ] - min;
   }

   for( uint32_t i=0; i< m_procNum; i++)
   {
      p_dmatrix_x [ i ] = p_dmatrix_x1 [ i ];
      p_dmatrix_x [ m_procNum + i ] = p_dmatrix_x2[ i ];
   }

   int64_t offset_corr = 0;

   if( round == 1 )
   {
      // determine the best offset correction to garantuee always time > 0
      //
      for( uint32_t i=0; i< m_procNum; i++)
      {
         std::map<uint32_t, UnifyControlS*>::const_iterator iter_uctl =
            StreamId2UnifyCtl.find(i+1);
         vt_assert( iter_uctl != StreamId2UnifyCtl.end() );

         UnifyControlS * uctl = iter_uctl->second;

         SyncPhaseS act_phase = uctl->sync_phases[round-1];
         uint32_t mapid = act_phase.mapid;

         // offset + offset_corr + drift * starttime = 0
         int64_t corr =
            (int64_t)( -p_dmatrix_x[ mapid-1 ] ) -
            (int64_t)( p_dmatrix_x[ mapid - 1 + m_procNum ] *
                       (double)( m_streamId2StartTime.find(mapid) )->second );

         if ( i==0 ) offset_corr = corr;

         if ( corr > offset_corr ) offset_corr = corr;
      }

      // correct offset, to garantuee time > 0 over all synchronization processes
      //
      for( uint32_t i = 0; i < m_procNum; i++)
      {
         p_imatrix_offset[ i ] = (int64_t)p_dmatrix_x[ i ] + offset_corr;
         p_dmatrix_x[ i ] = (double) p_imatrix_offset[ i ];
      }
   }

#if (defined (__DEBUG))
   m = dim_x_3;
   n = 1;
   k = dim_y_3;
   alpha = 1.0;
   lda = dim_x_3;
   ldb = dim_y_3;
   beta = 0.0;
   ldc = dim_x_3;

   print( p_dmatrix_a, dim_x_3,dim_y_3,"matrix a:" );

   dgemm_( "N", "N", &m, &n, &k, &alpha, p_dmatrix_a, &lda, p_dmatrix_x, &ldb,
           &beta, p_dmatrix_y, &ldc );

   print( p_dmatrix_x, 2*dim_y_2,1, "matrix x:" );
   print( p_dmatrix_y, dim_x_3,1, "matrix y:" );
#endif

   if( round > 1)
   {
      // determine an approximate duration time for the synchronization phase
      // might be inaccurate, because of overhead in the synchronization phase
      // while using only the measurement data for approximation
      //

      for( uint32_t i = 0; i < m_procNum; i++ )
      {
         std::map<uint32_t, UnifyControlS*>::const_iterator iter_uctl =
            StreamId2UnifyCtl.find(i+1);
         vt_assert( iter_uctl != StreamId2UnifyCtl.end() );

         UnifyControlS * uctl = iter_uctl->second;

         SyncPhaseS last_phase = uctl->sync_phases[round-1];
         uint32_t mapid = last_phase.mapid;

         // determine offset correction for a monotone increasing time function
         std::map<uint32_t, std::vector<SyncParamS*>*>::const_iterator iter_param =
            m_idxvecSyncParam.find(mapid);

         SyncParamS * p_param = (*(iter_param->second))[round-1];

         uint64_t time1 = (uint64_t)( p_param->offset + (int64_t)( p_param->drift * (double) last_phase.time ) );
         uint64_t time2 = time1 + (uint64_t)( (double) last_phase.duration *
                                              ( p_param->drift + p_dmatrix_x[ m_procNum+i ] ) / 2.0 );

         // offset + offsetcorr + drift_act( lastphase->time + duration ) = time2
         offset_corr =  ( time2 - (int64_t)p_dmatrix_x[ i ] -
                          (int64_t)( (double)( last_phase.time + last_phase.duration ) *
                                     p_dmatrix_x[ m_procNum + i ] ) );

         p_imatrix_offset[ i ] = (int64_t) p_dmatrix_x[ i ] + offset_corr;
      }
   }

   // insert correction parameters for time synchronization into global map
   //
   for( uint32_t i=0; i < m_procNum; i++ )
   {
      SyncParamS *data =
         new SyncParamS( p_imatrix_offset[ i ], p_dmatrix_x[ i + m_procNum ] );

      std::map<uint32_t, std::vector<SyncParamS*>*>:: iterator it_param =
         m_idxvecSyncParam.find(i+1);

      if( it_param == m_idxvecSyncParam.end() )
      {
         it_param =
            m_idxvecSyncParam.insert(
               std::make_pair( i+1, new std::vector<SyncParamS*>() )).first;
      }
      (it_param->second)->push_back( data );
   }

   if( round == 1 )
   {
      // initial update of synchronization parameters overall processes
      //
      for( uint32_t i=0; i < UnifyCtls.size(); i++ )
      {
         // get the synchronization reference process id for i
         //
         std::vector<SyncPhaseS> & sync_phases = UnifyCtls[i]->sync_phases;
         SyncPhaseS phase = sync_phases[0];
         uint32_t mapid = phase.mapid;

         std::map<uint32_t, std::vector<SyncParamS*>*>:: iterator iter =
            m_idxvecSyncParam.find(mapid);
         UnifyCtls[i]->sync_offset = (*(iter->second))[round]->offset;
         UnifyCtls[i]->sync_drift = (*(iter->second))[round]->drift;
      }
   }

   // remove matrices
   //
   free(p_dmatrix_r);
   free(p_dmatrix_s);
   free(p_dmatrix_g);
   free(p_dmatrix_h);
   free(p_dmatrix_d);
   free(p_dmatrix_x);
   free(p_dmatrix_x1);
   free(p_dmatrix_x2);
   free(p_imatrix_offset);

   return !error;
}

void
ETimeSyncC::print( double *a, int m, int n, char* info )
{
   std::cout << info << std::endl;
   for ( int i= 0; i < m; i++ )
   {
      for ( int j = 0; j < n; j++)
         std::cout << a[ i + j*m ] << " ";
      std::cout << std::endl;
   }
}

#ifdef VT_MPI

bool
ETimeSyncC::distStartTimes()
{
   bool error = false;

   vt_assert( NumRanks > 1 );

   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   VPrint( 2, "   Distributing start times\n" );

   char * buffer;
   VT_MPI_INT buffer_size;
   VT_MPI_INT buffer_pos;

   // get size needed for the send buffer
   //

   VT_MPI_INT size;

   buffer_size = 0;

   // m_streamId2StartTime.size()
   //
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // m_streamId2StartTime.first's
   //
   CALL_MPI( MPI_Pack_size( m_streamId2StartTime.size(), MPI_UNSIGNED,
                            MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // m_streamId2StartTime.second's
   //
   CALL_MPI( MPI_Pack_size( m_streamId2StartTime.size(), MPI_LONG_LONG_INT,
                            MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // allocate memory for the send buffer
   //
   buffer = new char[buffer_size];
   vt_assert( buffer );

   // pack send buffer
   //

   buffer_pos = 0;

   {
      // m_streamId2StartTime.size()
      //
      uint32_t map_size = m_streamId2StartTime.size();
      CALL_MPI( MPI_Pack( &map_size, 1, MPI_UNSIGNED, buffer, buffer_size,
                          &buffer_pos, MPI_COMM_WORLD ) );

      // m_streamId2StartTime
      //
      for( std::map<uint32_t, uint64_t>::const_iterator iter_starttime =
           m_streamId2StartTime.begin();
           iter_starttime != m_streamId2StartTime.end(); ++iter_starttime )
      {
         // m_streamId2StartTime.first
         //
         uint32_t map_first = iter_starttime->first;
         CALL_MPI( MPI_Pack( &map_first, 1, MPI_UNSIGNED, buffer, buffer_size,
                    &buffer_pos, MPI_COMM_WORLD ) );

         // m_streamId2StartTime.second
         //
         uint64_t map_second = iter_starttime->second;
         CALL_MPI( MPI_Pack( &map_second, 1, MPI_LONG_LONG_INT, buffer,
                             buffer_size, &buffer_pos, MPI_COMM_WORLD ) );
      }
   }

   VT_MPI_INT * rbuffer_sizes = 0;

   // allocate memory for the receive buffer sizes
   //
   rbuffer_sizes = new VT_MPI_INT[NumRanks];
   vt_assert( rbuffer_sizes );

   // gather receive buffer sizes
   CALL_MPI( MPI_Allgather( &buffer_size, 1, MPI_INT, rbuffer_sizes, 1, MPI_INT,
                            MPI_COMM_WORLD ) );

   char * rbuffer = 0;
   VT_MPI_INT rbuffer_size = 0;
   VT_MPI_INT * rdispls = 0;

   // allocate memory for displacements
   //
   rdispls = new VT_MPI_INT[NumRanks];
   vt_assert( rdispls );

   // compute displacements and receive buffer size
   //
   for( VT_MPI_INT i = 0; i < NumRanks; i++ )
   {
      rbuffer_size += rbuffer_sizes[i];
      rdispls[i] = (i == 0) ? 0 : rdispls[i-1] + rbuffer_sizes[i-1];
   }

   // allocate memory for the receive buffer
   //
   rbuffer = new char[rbuffer_size];
   vt_assert( rbuffer );

   // gather start times
   CALL_MPI( MPI_Allgatherv( buffer, buffer_size, MPI_PACKED, rbuffer,
                             rbuffer_sizes, rdispls, MPI_PACKED,
                             MPI_COMM_WORLD ) );

   // free memory of send buffer
   delete [] buffer;

   // unpack receive buffer
   //

   for( VT_MPI_INT i = 0; i < NumRanks; i++ )
   {
      buffer = rbuffer + rdispls[i];

      buffer_pos = 0;

      // m_streamId2StartTime.size()
      //
      uint32_t map_size;
      CALL_MPI( MPI_Unpack( buffer, rbuffer_sizes[i], &buffer_pos, &map_size,
                            1, MPI_UNSIGNED, MPI_COMM_WORLD ) );

      // m_streamId2StartTime
      //
      for( uint32_t j = 0; j < map_size; j++ )
      {
         // m_streamId2StartTime.first
         //
         uint32_t map_first;
         CALL_MPI( MPI_Unpack( buffer, rbuffer_sizes[i], &buffer_pos,
                               &map_first, 1, MPI_UNSIGNED,
                               MPI_COMM_WORLD ) );

         // m_streamId2StartTime.second
         //
         uint64_t map_second;
         CALL_MPI( MPI_Unpack( buffer, rbuffer_sizes[i], &buffer_pos,
                               &map_second, 1, MPI_LONG_LONG_INT,
                                  MPI_COMM_WORLD ) );

         // set start time of stream
         setStartTime( map_first, map_second );
      }
   }

   // free some memory
   //
   delete [] rbuffer;
   delete [] rbuffer_sizes;
   delete [] rdispls;

   return !error;
}

#endif // VT_MPI
