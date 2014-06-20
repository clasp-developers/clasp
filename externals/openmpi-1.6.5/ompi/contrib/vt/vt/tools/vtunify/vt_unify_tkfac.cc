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

#include "vt_unify_tkfac.h"

TokenFactoryC * theTokenFactory = 0; // instance of class TokenFactoryC

//////////////////// class TokenFactoryC ////////////////////

// public methods
//

TokenFactoryC::TokenFactoryC()
{
   // Empty
}

TokenFactoryC::~TokenFactoryC()
{
   // Empty
}

void
TokenFactoryC::addScope( const DefRecTypeT & type, TokenFactoryScopeI * scope )
{
   vt_assert( type < DEF_REC_TYPE__Num );
   vt_assert( scope );

   // search for already added scope instance
   std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator it = m_def2scope.find( type );

   // add scope instance to map, if not found
   if( it == m_def2scope.end() )
      m_def2scope[type] = scope;
}

void
TokenFactoryC::deleteScope( const DefRecTypeT & type )
{
   vt_assert( type < DEF_REC_TYPE__Num );

   // search scope instance
   std::map<DefRecTypeT, TokenFactoryScopeI*>::iterator it = m_def2scope.find( type );

   // erase map entry, if found
   if( it != m_def2scope.end() )
   {
      delete it->second;
      m_def2scope.erase( it );
   }
}

TokenFactoryScopeI *
TokenFactoryC::getScope( const DefRecTypeT & type ) const
{
   vt_assert( type < DEF_REC_TYPE__Num );

   // search scope instance
   std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator it_scope =
      m_def2scope.find( type );

   // if found, return scope instance; otherwise, return 0
   return ( it_scope != m_def2scope.end() ) ? it_scope->second : 0;
}

#ifdef VT_MPI

bool
TokenFactoryC::distTranslations( const VT_MPI_INT & destRank,
   const bool wait )
{
   bool error = false;

   vt_assert( NumRanks > 1 );

   // message tag to use for p2p communication
   const VT_MPI_INT msg_tag = 200;

   VT_MPI_INT buffer_pos;
   VT_MPI_INT buffer_size;
   MPI_Status status;

   MASTER
   {
      vt_assert( destRank != 0 );

      // send token translation tables to given destination rank
      //

      PVPrint( 3, "  Sending token translation tables to rank %d\n", destRank );

      // request handle for non-blocking send
      static MPI_Request request = MPI_REQUEST_NULL;

      // send buffer
      static char * buffer = 0;

      // get stream ids associated with given destination rank
      const std::set<uint32_t> & stream_ids = Rank2StreamIds[destRank];

      // convert stream ids to master process ids
      // (=keys of token translation tables)
      //
      std::set<uint32_t> mprocess_ids;
      for( std::set<uint32_t>::const_iterator stream_it = stream_ids.begin();
           stream_it != stream_ids.end(); ++stream_it )
         mprocess_ids.insert( *stream_it & VT_TRACEID_BITMASK );

      // get size needed for the send buffer
      //

      VT_MPI_INT size;

      buffer_size = 0;

      for( std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator scope_it =
           m_def2scope.begin(); scope_it != m_def2scope.end(); ++scope_it )
      {
         // get scope
         TokenFactoryScopeC<DefRec_BaseS> * scope =
            static_cast<TokenFactoryScopeC<DefRec_BaseS>*>( scope_it->second );

         // get size needed to pack the number of translation tables into
         // the send buffer
         //
         CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // get size needed to pack the token translation tables into the
         // send buffer
         //
         for( std::set<uint32_t>::const_iterator proc_it = mprocess_ids.begin();
              proc_it != mprocess_ids.end(); ++proc_it )
            buffer_size += scope->getPackSize( *proc_it );
      }

      // wait until previous send is completed and free memory of the
      // send buffer
      //
      if( request != MPI_REQUEST_NULL )
      {
         vt_assert( buffer );

         CALL_MPI( MPI_Wait( &request, &status ) );
         delete [] buffer;
      }

      // allocate memory for the send buffer
      //
      buffer = new char[buffer_size];
      vt_assert( buffer );

      // pack send buffer
      //

      buffer_pos = 0;

      for( std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator scope_it =
           m_def2scope.begin(); scope_it != m_def2scope.end(); ++scope_it )
      {
         // get scope
         TokenFactoryScopeC<DefRec_BaseS> * scope =
            static_cast<TokenFactoryScopeC<DefRec_BaseS>*>( scope_it->second );

         // pack number of token translation tables into the send buffer
         //
         uint32_t mprocess_size = mprocess_ids.size();
         CALL_MPI( MPI_Pack( &mprocess_size, 1, MPI_UNSIGNED, buffer,
                             buffer_size, &buffer_pos, MPI_COMM_WORLD ) );

         // pack token translation tables into the send buffer
         //
         for( std::set<uint32_t>::const_iterator proc_it = mprocess_ids.begin();
              proc_it != mprocess_ids.end(); ++proc_it )
            scope->pack( *proc_it, buffer, buffer_size, buffer_pos );
      }

      // send buffer
      CALL_MPI( MPI_Isend( buffer, buffer_size, MPI_PACKED, destRank, msg_tag,
                           MPI_COMM_WORLD, &request ) );

      // if it's the last send, wait until completion and free memory of the
      // send buffer
      //
      if( wait )
      {
         CALL_MPI( MPI_Wait( &request, &status ) );
         delete [] buffer;
      }
   }
   else // SLAVE
   {
      // receive token translation tables from rank 0
      //

      PVPrint( 3, "  Receiving token translation tables from rank 0\n" );

      // receive buffer
      char * buffer;

      // test for a message from rank 0
      CALL_MPI( MPI_Probe( 0, msg_tag, MPI_COMM_WORLD, &status ) );

      // get size needed for the receive buffer
      CALL_MPI( MPI_Get_count( &status, MPI_PACKED, &buffer_size ) );

      // allocate memory for the receive buffer
      //
      buffer = new char[buffer_size];
      vt_assert( buffer );

      // receive buffer
      CALL_MPI( MPI_Recv( buffer, buffer_size, MPI_PACKED, 0, msg_tag,
                          MPI_COMM_WORLD, &status ) );

      // unpack receive buffer
      //

      buffer_pos = 0;

      for( std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator scope_it =
           m_def2scope.begin(); scope_it != m_def2scope.end(); ++scope_it )
      {
         // get scope
         TokenFactoryScopeC<DefRec_BaseS> * scope =
            static_cast<TokenFactoryScopeC<DefRec_BaseS>*>( scope_it->second );

         // unpack the number of token translation tables from the
         // receive buffer
         uint32_t mprocess_size;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos, &mprocess_size, 1,
                               MPI_UNSIGNED, MPI_COMM_WORLD ) );

         // unpack token translation tables from the receive buffer
         //
         for( uint32_t i = 0; i < mprocess_size; i++ )
            scope->unpack( buffer, buffer_size, buffer_pos );
      }

      // free memory of the receive buffer
      delete [] buffer;
   }

   return !error;
}

#endif // VT_MPI
