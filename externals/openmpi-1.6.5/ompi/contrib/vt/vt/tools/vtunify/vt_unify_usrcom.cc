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

#include "vt_unify_usrcom.h"

UserComC * theUserCom = 0; // instance of class UserComC

//////////////////// class UserComC ////////////////////

// public methods
//

UserComC::UserComC()
{
   // Empty
}

UserComC::~UserComC()
{
   // Empty
}

bool
UserComC::addSender( const ComIdS & comId, const uint32_t & sender )
{
   bool error = false;

   // search for already registered communication id
   std::map<ComIdS, ComPairS>::iterator it =
      m_comId2ComPair.find( comId );

   if( it == m_comId2ComPair.end() )
   {
      // add new communication id
      m_comId2ComPair[comId] = ComPairS( sender, 0 );
   }
   else
   {
      if( it->second.sender == 0 )
      {
         // set sender process id
         it->second.sender = sender;
      }
      else
      {
         // delete communication id, if sender is already set
         // TODO: show warning message?
         m_comId2ComPair.erase( it );
         error = true;
      }
   }

   return !error;
}

bool
UserComC::addReceiver( const ComIdS & comId, const uint32_t & receiver )
{
   bool error = false;

   // search for already registered communication id
   std::map<ComIdS, ComPairS>::iterator it =
      m_comId2ComPair.find( comId );

   if( it == m_comId2ComPair.end() )
   {
      // add new communication id
      m_comId2ComPair[comId] = ComPairS( 0, receiver );
   }
   else
   {
      if( it->second.receiver == 0 )
      {
         // set receiver process id
         it->second.receiver = receiver;
      }
      else
      {
         // delete communication id, if receiver is already set
         // TODO: show warning message?
         m_comId2ComPair.erase( it );
         error = true;
      }
   }

   return !error;
}

#ifdef VT_MPI

bool
UserComC::share()
{
   bool error = false;

   vt_assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   VPrint( 1, "Sharing user communication IDs and pairs\n" );

   char * buffer;
   VT_MPI_INT buffer_pos;
   VT_MPI_INT buffer_size;

   MASTER
   {
      // get size needed for the send buffer
      //

      VT_MPI_INT size;

      buffer_size = 0;

      // there are any user communication?
      if( !m_comId2ComPair.empty() )
      {
         // m_userComms.size() + m_userComms
         //
         CALL_MPI( MPI_Pack_size( 1 + m_userComms.size(), MPI_UNSIGNED,
                                  MPI_COMM_WORLD, & size ) );
         buffer_size += size;

         // m_comId2ComPair.size()
         //
         CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // m_comId2ComPair.first's
         //
         CALL_MPI( MPI_Pack_size( m_comId2ComPair.size() * 2, MPI_UNSIGNED,
                                  MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // m_comId2ComPair.second's
         //
         CALL_MPI( MPI_Pack_size( m_comId2ComPair.size() * 2, MPI_UNSIGNED,
                                  MPI_COMM_WORLD, &size ) );
         buffer_size += size;
      }
   }

   // broadcast buffer size
   CALL_MPI( MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD ) );

   // return, if there are no user communication
   if( buffer_size == 0 )
      return true;

   // allocate memory for the send/receive buffer
   //
   buffer = new char[buffer_size];
   vt_assert( buffer );

   MASTER
   {
      // pack send buffer
      //

      buffer_pos = 0;

      // m_userComms.size()
      //
      uint32_t comm_size = m_userComms.size();
      CALL_MPI( MPI_Pack( &comm_size, 1, MPI_UNSIGNED, buffer, buffer_size,
                          &buffer_pos, MPI_COMM_WORLD ) );

      // m_userComms
      //
      for( std::set<uint32_t>::const_iterator it = m_userComms.begin();
           it != m_userComms.end(); ++it )
      {
         uint32_t comm = *it;
         CALL_MPI( MPI_Pack( &comm, 1, MPI_UNSIGNED, buffer, buffer_size,
                             &buffer_pos, MPI_COMM_WORLD ) );
      }

      // m_comId2ComPair.size()
      //
      uint32_t map_size = m_comId2ComPair.size();
      CALL_MPI( MPI_Pack( &map_size, 1, MPI_UNSIGNED, buffer, buffer_size,
                          &buffer_pos, MPI_COMM_WORLD ) );

      // m_comId2ComPair
      //
      for( std::map<ComIdS, ComPairS>::const_iterator it =
           m_comId2ComPair.begin(); it != m_comId2ComPair.end(); ++it )
      {
         // m_comId2ComPair.first
         //
         uint32_t comm = it->first.comm;
         uint32_t tag = it->first.tag;
         CALL_MPI( MPI_Pack( &comm, 1, MPI_UNSIGNED, buffer, buffer_size,
                             &buffer_pos, MPI_COMM_WORLD ) );
         CALL_MPI( MPI_Pack( &tag, 1, MPI_UNSIGNED, buffer, buffer_size,
                             &buffer_pos, MPI_COMM_WORLD ) );

         // m_comId2ComPair.second
         //
         uint32_t sender = it->second.sender;
         uint32_t receiver = it->second.receiver;
         CALL_MPI( MPI_Pack( &sender, 1, MPI_UNSIGNED, buffer, buffer_size,
                             &buffer_pos, MPI_COMM_WORLD ) );
         CALL_MPI( MPI_Pack( &receiver, 1, MPI_UNSIGNED, buffer, buffer_size,
                             &buffer_pos, MPI_COMM_WORLD ) );
      }
   }

   // broadcast buffer
   CALL_MPI( MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD ) );

   SLAVE
   {
      // unpack receive buffer
      //

      buffer_pos = 0;

      // m_userComms.size()
      //
      uint32_t comm_size;
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos, &comm_size, 1,
                            MPI_UNSIGNED, MPI_COMM_WORLD ) );

      // m_userComms
      //
      for( uint32_t i = 0; i < comm_size; i++ )
      {
         uint32_t comm;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos, &comm, 1,
                               MPI_UNSIGNED, MPI_COMM_WORLD ) );
         m_userComms.insert( comm );
      }

      // m_comId2ComPair.size()
      //
      uint32_t map_size;
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos, &map_size, 1,
                            MPI_UNSIGNED, MPI_COMM_WORLD ) );

      // m_comId2ComPair
      //
      for( uint32_t i = 0; i < map_size; i++ )
      {
         // m_comId2ComPair.first
         //
         uint32_t comm;
         uint32_t tag;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos, &comm, 1,
                               MPI_UNSIGNED, MPI_COMM_WORLD ) );
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos, &tag, 1,
                               MPI_UNSIGNED, MPI_COMM_WORLD ) );

         // m_comId2ComPair.second
         //
         uint32_t sender;
         uint32_t receiver;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos, &sender, 1,
                               MPI_UNSIGNED, MPI_COMM_WORLD ) );
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos, &receiver, 1,
                               MPI_UNSIGNED, MPI_COMM_WORLD ) );

         m_comId2ComPair[ComIdS( comm, tag )] = ComPairS( sender, receiver );
      }
   }

   // free memory of send/receive buffer
   delete [] buffer;

//   SyncError( &error );

   return !error;
}

#endif // VT_MPI
