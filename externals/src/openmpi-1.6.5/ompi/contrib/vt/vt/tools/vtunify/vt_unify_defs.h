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

#ifndef _VT_UNIFY_DEFS_H_
#define _VT_UNIFY_DEFS_H_

#include "vt_unify.h"
#include "vt_unify_defs_recs.h"
#include "vt_unify_lvector.hh"

#include <algorithm>
#include <list>

//
// DefinitionsC class
//
class DefinitionsC
{
public:

   // forward declaration of sub-class GroupCountersC
   class GroupCountersC;

   // constructor
   DefinitionsC();

   // destructor
   ~DefinitionsC();

   // unify definitions
   bool run();

   // rename temporary output files
   bool cleanUp();

   // get instance of sub-class GroupCountersC
   GroupCountersC * groupCounters() const { return m_groupCntrs; }

private:

   // forward declaration of sub-class CommentsC
   class CommentsC;

   // forward declaration of sub-class ProcessGroupsC
   class ProcessGroupsC;

   // read local definitions
   bool readLocal();

   // read local definitions of certain single stream
   bool readLocal( const uint32_t & streamId,
                   LargeVectorC<DefRec_BaseS*> & locDefs );

   // process local definitions
   // (i.e. create global tokens)
   bool processLocal( const LargeVectorC<DefRec_BaseS*> & locDefs );

   // write global definitions
   bool writeGlobal();

   // instance of sub-class GroupCountersC
   GroupCountersC * m_groupCntrs;

   // instance of sub-class CommentsC
   CommentsC * m_comments;

   // instance of sub-class ProcessGroupsC
   ProcessGroupsC * m_procGrps;

   //
   // storage of global definitions
   //
   struct
   {
      // global definitions created by TokenFactoryScopeC
      // (concerns all definitions which define an identifier token
      //  that has to be unified)
      //

      std::set<DefRec_DefProcessGroupS>           procGrps;
      std::set<DefRec_DefSclFileS>                sclFiles;
      std::set<DefRec_DefSclS>                    scls;
      std::set<DefRec_DefFileGroupS>              fileGrps;
      std::set<DefRec_DefFileS>                   files;
      std::set<DefRec_DefFunctionGroupS>          funcGrps;
      std::set<DefRec_DefFunctionS>               funcs;
      std::set<DefRec_DefCollOpS>                 collops;
      std::set<DefRec_DefCounterGroupS>           cntrGrps;
      std::set<DefRec_DefCounterS>                cntrs;
      std::set<DefRec_DefKeyValueS>               keyVals;

      // miscellaneous global definitions
      //

      DefRec_DefCreatorS                          creator;
      DefRec_DefTimerResolutionS                  timeres;
      DefRec_DefTimeRangeS                        timerange;
      std::set<DefRec_DefCommentS>                comments;
      std::set<DefRec_DefProcessS>                procs;
      // map global counter token <-> process group assignments
      std::map<uint32_t, DefRec_DefCounterAssignmentsS>
                                                  cntrAssigns;
      // map global process group token <-> attributes
      std::map<uint32_t, DefRec_DefProcessGroupAttributesS>
                                                  procGrpAttrs;

   } m_globDefs;

};

//
// DefinitionsC::GroupCountersC sub-class
// (manages process group assignments of group counters)
//
class DefinitionsC::GroupCountersC
{
public:

   // constructor
   GroupCountersC( DefinitionsC & _defs ) : m_defs( _defs ) {}

   // destructor
   ~GroupCountersC() {}

   // set local process group token for local process/counter token
   // (only one process group assignment per counter allowed)
   void setGroup( const uint32_t & proc, const uint32_t & counter,
           const uint32_t & procGrp )
   {
      m_cntr2ProcGrp[std::make_pair( proc, counter )] = procGrp;
   }

   // get local process group token of certain local process/counter token
   uint32_t getGroup( const uint32_t & proc, const uint32_t & counter ) const
   {
      // search for process group assignment
      std::map<std::pair<uint32_t, uint32_t>, uint32_t>::const_iterator it =
         m_cntr2ProcGrp.find( std::make_pair( proc, counter ) );

      // if found, return local process group token; otherwise, return 0
      //
      if( it != m_cntr2ProcGrp.end() )
         return it->second;
      else
         return 0;
   }

   // The following methods are significant for the final stream/process[group]
   // mapping in the OTF master control file.
   //

   // add global process group token to stream
   void addGroupToStream( const uint32_t & streamid, const uint32_t & procGrp )
   {
      if( m_streamId2ProcGrps[streamid].insert( procGrp ).second )
      {
         // catch multiple added process groups
         //
         bool added_once = m_procGrps.insert( procGrp ).second;
         vt_assert( added_once );
      }
   }

   // get global process group tokens of certain stream
   const std::set<uint32_t> * getGroupsOfStream(
                                 const uint32_t & streamid ) const
   {
      // search for stream id
      std::map<uint32_t, std::set<uint32_t> >::const_iterator it =
         m_streamId2ProcGrps.find( streamid );

      // if found, return pointer to set of global process group tokens;
      // otherwise, return 0
      //
      if( it != m_streamId2ProcGrps.end() )
         return &(it->second);
      else
         return 0;
   }

private:

   // reference to parent class instance
   DefinitionsC & m_defs;

   // set of global process group tokens which have counters
   std::set<uint32_t> m_procGrps;

   // map stream id <-> global process group tokens
   std::map<uint32_t, std::set<uint32_t> > m_streamId2ProcGrps;

   // map local process/counter token <-> local process group token
   std::map<std::pair<uint32_t, uint32_t>, uint32_t> m_cntr2ProcGrp;

};

//
// DefinitionsC::CommentsC sub-class
// (pre-processes comments before adding these to global definitions)
//
class DefinitionsC::CommentsC
{
public:

   // constructor
   CommentsC( DefinitionsC & _defs )
      : m_defs( _defs ), m_minStartTimeEpoch( (uint64_t)-1 ),
        m_maxStopTimeEpoch( 0 ), m_seqOrderIdx( 0 ) {}

   // destructor
   ~CommentsC() {}

   // process local definition comment
   bool processLocal( const DefRec_DefCommentS & locComment );

   // finish global definition comments
   // (i.e. add trace time comments to global definitions)
   bool finish();

private:

   // reference to parent class instance
   DefinitionsC & m_defs;

   // trace times
   //
   uint64_t m_minStartTimeEpoch;
   uint64_t m_maxStopTimeEpoch;

   // sequential order index
   uint32_t m_seqOrderIdx;

};

//
// DefinitionsC::ProcessGroupsC sub-class
// (pre-processes process groups before adding these to global definitions)
//
class DefinitionsC::ProcessGroupsC
{
   // friend declaration for sub-class DefinitionsC::CommentsC;
   // needs access to m_userCom to add member process ids to certain user
   // communicators
   friend class DefinitionsC::CommentsC;

public:

   // constructor
   ProcessGroupsC( DefinitionsC & _defs ) : m_defs( _defs ) {}

   // destructor
   ~ProcessGroupsC()
   {
      for( uint32_t i = 0; i < m_uniqueMembers.size(); i++ )
         delete m_uniqueMembers[i];
   }

   // process local process group definition
   bool processLocal( DefRec_DefProcessGroupS & locProcGrp );

   // finish global process group definitions
   // (i.e. add process groups for nodes and MPI-comms. to global defs.)
   bool finish();

   // deflate group member array of certain process group definition
   // (replaces array elements by an unique id)
   inline void deflateMembers( DefRec_DefProcessGroupS & procGrp );

   // inflate group members array of certain process group definition
   // (replaces unique id by the actual array elements)
   inline void inflateMembers( DefRec_DefProcessGroupS & procGrp );

private:

   // identifier for deflated group member arrays
   // (will be putted at the first array element)
   static const uint32_t DEFLATED_MEMBERS_TAG = (uint32_t)-1;

   //
   // compare structure for sorting process ids
   //
   struct ProcCmpS
   {
      bool operator()( const uint32_t & a, const uint32_t & b ) const
      {
         if( ( a & VT_TRACEID_BITMASK ) == ( b & VT_TRACEID_BITMASK ) )
            return a < b;
         else
            return ( a & VT_TRACEID_BITMASK ) < ( b & VT_TRACEID_BITMASK );
      }

   };

   //
   // structure for storing unique (un-deflated) group member arrays
   //
   struct UniqueMembersS
   {
      UniqueMembersS( uint32_t _id, uint32_t _nmembers,
         const uint32_t * _members )
         : id( _id ), nmembers( _nmembers ), members( 0 )
      {
         vt_assert( nmembers > 0 );

         members = new uint32_t[nmembers];
         vt_assert( members );

         memcpy( members, _members, nmembers * sizeof( uint32_t ) );
      }
      ~UniqueMembersS()
      {
         delete [] members;
      }

      uint32_t   id;       // unique id representing this group member array
      uint32_t   nmembers; // number of group members
      uint32_t * members;  // array of group members

   };

   //
   // scope for MPI communicators and groups
   //
   struct MpiS
   {
      //
      // sub-scope for MPI_COMM_WORLD
      //
      struct WorldCommS
      {
         WorldCommS() : global_token( 0 ) {}

         // name of final process group
         static const char * NAME() { return "MPI_COMM_WORLD"; }

         // global process group token
         uint32_t global_token;

      } worldComm;

      //
      // sub-scope for MPI_COMM_SELFs
      //
      struct SelfCommsS
      {
         // name (prefix) of final process groups
         static const char * NAME() { return "MPI_COMM_SELF"; }

      } selfComms;

      //
      // sub-scope for user created MPI communicators and groups
      //
      struct CommsAndGroupsS
      {
         CommsAndGroupsS() : comm_seqno( 0 ), group_seqno( 0 ) {}

         // name (prefix) of final process groups
         //
         static const char * COMM_NAME() { return "MPI Communicator"; }
         static const char * GROUP_NAME() { return "MPI Group"; }

         // communicator/group sequential number (=name suffix)
         //
         uint32_t comm_seqno;
         uint32_t group_seqno;

         // map process/membersid <-> count
         std::map<std::pair<uint32_t, uint32_t>, uint32_t> counts;

         // map membersid/count <-> global token
         std::map<std::pair<uint32_t, uint32_t>, uint32_t> global_tokens;

      } commsAndGroups;

   } m_mpi;

   //
   // scope for communicators of user communication
   //
   struct UserComS
   {
      //
      // structure for user communicators
      //
      struct CommS
      {
         CommS() : global_token( 0 ) {}

         uint32_t global_token;                // global comm. token
         std::set<uint32_t, ProcCmpS> members; // member process ids

      };

      // add member process id to certain user communicator
      void addCommMember( const uint32_t & comm, const uint32_t & member )
      {
         std::map<uint32_t, CommS*>::iterator it =
            globTk2Comm.find( comm );
         vt_assert( it != globTk2Comm.end() );

         it->second->members.insert( member );
      }

      // map name <-> communicator
      std::map<std::string, CommS*> name2Comm;

      // map global token <-> communicator
      std::map<uint32_t, CommS*> globTk2Comm;

   } m_userCom;

   //
   // scope for other process groups (e.g. nodes, GPU comms./groups)
   //
   struct OtherS
   {
      // name of final process group containing all processes
      static const char * ALL_NAME() { return "All"; }

      //
      // structure for other process groups
      //
      struct GroupS
      {
         GroupS() : global_token( 0 ) {}

         uint32_t global_token;                // global process group token
         std::set<uint32_t, ProcCmpS> members; // member process ids

      };

      // map name <-> process group
      std::map<std::string, GroupS> name2Group;

   } m_other;

   // map hash <-> unique group members array(s)
   std::multimap<uint32_t, UniqueMembersS*> m_hash2UniqueMembers;

   // vector of unique group member arrays
   std::vector<UniqueMembersS*> m_uniqueMembers;

   // reference to parent class instance
   DefinitionsC & m_defs;

};

// instance of class DefinitionsC
extern DefinitionsC * theDefinitions;

#endif // _VT_UNIFY_DEFS_H_
