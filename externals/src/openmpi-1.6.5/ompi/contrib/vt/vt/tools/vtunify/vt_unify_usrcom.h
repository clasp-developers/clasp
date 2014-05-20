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

#ifndef _VT_UNIFY_USRCOM_H_
#define _VT_UNIFY_USRCOM_H_

#include "vt_unify.h"

//
// UserComC class
//
class UserComC
{
public:

   //
   // structure for user communication ids
   //
   struct ComIdS
   {
      // constructors
      ComIdS()
         : comm( 0 ), tag( 0 ) {}
      ComIdS( const uint32_t & _comm, const uint32_t & _tag )
         : comm( _comm ), tag( _tag ) {}

      // operator for searching
      bool operator<( const ComIdS & a ) const
      {
         if( comm == a.comm )
            return tag < a.tag;
         else
            return comm < a.comm;
      }

      uint32_t comm; // communicator token
      uint32_t tag;  // unique tag

   };

   //
   // structure for user communication pairs
   //
   struct ComPairS
   {
      // constructors
      ComPairS()
         : sender( 0 ), receiver( 0 ) {}
      ComPairS( const uint32_t & _sender, const uint32_t & _receiver )
         : sender( _sender ), receiver( _receiver ) {}

      uint32_t sender;   // sender process id
      uint32_t receiver; // receiver process id
   };

   // constructor
   UserComC();

   // destructor
   ~UserComC();

   // add global user communicator token
   void addUserComm( const uint32_t & comm )
   {
      m_userComms.insert( comm );
   }

   // check whether process group token is an user communicator
   bool isUserComm( const uint32_t & comm ) const
   {
      return ( m_userComms.find( comm ) != m_userComms.end() );
   }

   // register sender process id for certain user communication id
   bool addSender( const ComIdS & comId, const uint32_t & sender );

   // register receiver process id for certain user communication id
   bool addReceiver( const ComIdS & comId, const uint32_t & receiver );

   // get sender process id by comm. and tag
   uint32_t getSender( const uint32_t & comm, const uint32_t & tag ) const
   {
      uint32_t sender = 0;

      std::map<ComIdS, ComPairS>::const_iterator it =
         m_comId2ComPair.find( ComIdS( comm, tag ) );
      if( it != m_comId2ComPair.end() )
         sender = it->second.sender;

      return sender;
   }

   // get receiver process id by comm. and tag
   uint32_t getReceiver( const uint32_t & comm, const uint32_t & tag ) const
   {
      uint32_t receiver = 0;

      std::map<ComIdS, ComPairS>::const_iterator it =
         m_comId2ComPair.find( ComIdS( comm, tag ) );
      if( it != m_comId2ComPair.end() )
         receiver = it->second.receiver;

      return receiver;
   }

#ifdef VT_MPI
   // share user communication ids and pairs all ranks
   bool share();
#endif // VT_MPI

private:

   // set of global user communicator tokens
   std::set<uint32_t> m_userComms;

   // map user communication id <-> pair (sender/receiver process ids)
   std::map<ComIdS, ComPairS> m_comId2ComPair;

};

// instance of class UserComC
extern UserComC * theUserCom;

#endif // _VT_UNIFY_USRCOM_H_
