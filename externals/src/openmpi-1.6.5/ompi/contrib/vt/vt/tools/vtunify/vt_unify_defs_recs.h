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

#ifndef _VT_UNIFY_DEFS_RECS_H_
#define _VT_UNIFY_DEFS_RECS_H_

#include "vt_unify_config.h"

#include "vt_defs.h"
#include "vt_inttypes.h"

#include "util/hash.h"

#include "otf.h"

#include <algorithm>
#include <set>
#include <string>

//
// definition record types
//
typedef enum
{
   DEF_REC_TYPE__DefCreator,
   DEF_REC_TYPE__DefTimerResolution,
   DEF_REC_TYPE__DefTimeRange,
   DEF_REC_TYPE__DefProcess,
   DEF_REC_TYPE__DefProcessGroup,
   DEF_REC_TYPE__DefProcessGroupAttributes,
   DEF_REC_TYPE__DefSclFile,
   DEF_REC_TYPE__DefScl,
   DEF_REC_TYPE__DefFileGroup,
   DEF_REC_TYPE__DefFile,
   DEF_REC_TYPE__DefFunctionGroup,
   DEF_REC_TYPE__DefFunction,
   DEF_REC_TYPE__DefCollOp,
   DEF_REC_TYPE__DefCounterGroup,
   DEF_REC_TYPE__DefCounter,
   DEF_REC_TYPE__DefCounterAssignments,
   DEF_REC_TYPE__DefKeyValue,
   DEF_REC_TYPE__DefMarker,
   DEF_REC_TYPE__DefComment,
   DEF_REC_TYPE__Num
} DefRecTypeT;

//
// DefRec_BaseS
//
struct DefRec_BaseS
{
   //
   // compare structure for final sort
   //
   struct SortS
   {
      bool operator()( const DefRec_BaseS * a, const DefRec_BaseS * b ) const
      {
         return a->deftoken < b->deftoken;
      }

   };

   DefRec_BaseS( DefRecTypeT _dtype )
      : dtype( _dtype ), loccpuid( 0 ), deftoken( 0 ) {}
   DefRec_BaseS( const DefRecTypeT & _dtype, const uint32_t & _loccpuid,
      const uint32_t & _deftoken )
      : dtype( _dtype ), loccpuid( _loccpuid ), deftoken( _deftoken ) {}
   virtual ~DefRec_BaseS() {}

#ifdef VT_MPI
   virtual VT_MPI_INT getPackSize();
   virtual void pack( char *& buffer, const VT_MPI_INT & bufferSize,
                      VT_MPI_INT & bufferPos );
   virtual void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                        VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // NOTE: this operator is actually not used but necessary to work around
   // a build error with the Clang++ compiler
   // (see http://www.open-mpi.org/community/lists/devel/2012/02/10419.php)
   bool operator<( const DefRec_BaseS & a ) const
   {
      return dtype < a.dtype;
   }

   DefRecTypeT dtype;
   uint32_t    loccpuid;
   uint32_t    deftoken;

};

//
// DefRec_DefCommentS
//
struct DefRec_DefCommentS : DefRec_BaseS
{
   //
   // compare structure for final sort
   //
   struct SortS
   {
      bool operator()( const DefRec_DefCommentS * a,
                       const DefRec_DefCommentS * b ) const
      {
         if( a->type == b->type )
            return a->deftoken < b->deftoken; // order index
         else
            return a->type < b->type;
      }

   };

   typedef enum
   {
      TYPE_START_TIME, TYPE_STOP_TIME, TYPE_VT, TYPE_USER,
      TYPE_USRCOM_SEND, TYPE_USRCOM_RECV, TYPE_UNKNOWN
   } CommentTypeT;

   DefRec_DefCommentS()
      : DefRec_BaseS( DEF_REC_TYPE__DefComment ), type( TYPE_UNKNOWN ) {}
   DefRec_DefCommentS( const uint32_t & _loccpuid, const uint32_t _orderidx,
       const CommentTypeT & _type, const std::string & _comment )
      : DefRec_BaseS( DEF_REC_TYPE__DefComment, _loccpuid, _orderidx ),
        type( _type ), comment( _comment ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefCommentS & a ) const
   {
      if( type == a.type )
         return comment < a.comment;
      else
         return type < a.type;
   }

   CommentTypeT type;
   std::string  comment;

};

//
// DefRec_DefCreatorS
//
struct DefRec_DefCreatorS : DefRec_BaseS
{
   DefRec_DefCreatorS()
      : DefRec_BaseS( DEF_REC_TYPE__DefCreator ) {}
   DefRec_DefCreatorS( const std::string & _creator )
      : DefRec_BaseS( DEF_REC_TYPE__DefCreator ),
        creator( _creator ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   std::string creator;

};

//
// DefRec_DefTimerResolutionS
//
struct DefRec_DefTimerResolutionS : DefRec_BaseS
{
   DefRec_DefTimerResolutionS()
      : DefRec_BaseS( DEF_REC_TYPE__DefTimerResolution ),
        ticksPerSecond( 0 ) {}
   DefRec_DefTimerResolutionS( const uint64_t & _ticksPerSecond )
      : DefRec_BaseS( DEF_REC_TYPE__DefTimerResolution ),
        ticksPerSecond( _ticksPerSecond ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   uint64_t ticksPerSecond;

};

//
// DefRec_DefTimeRangeS
//
struct DefRec_DefTimeRangeS : DefRec_BaseS
{
   DefRec_DefTimeRangeS()
      : DefRec_BaseS( DEF_REC_TYPE__DefTimeRange ),
        minTime( 0 ), maxTime( 0 ) {}
   DefRec_DefTimeRangeS( const uint32_t & _loccpuid, const uint64_t & _minTime,
      const uint64_t & _maxTime )
      : DefRec_BaseS( DEF_REC_TYPE__DefTimeRange, _loccpuid, 0 ),
        minTime( _minTime ), maxTime( _maxTime ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   uint64_t minTime;
   uint64_t maxTime;

};

//
// DefRec_DefProcessS
//
struct DefRec_DefProcessS : DefRec_BaseS
{
   //
   // compare structure for final sort
   //
   struct SortS
   {
      bool operator()( const DefRec_DefProcessS * a,
                       const DefRec_DefProcessS * b ) const
      {
         if( ( a->deftoken & VT_TRACEID_BITMASK ) ==
             ( b->deftoken & VT_TRACEID_BITMASK ) )
            return a->deftoken < b->deftoken;
         else
            return ( a->deftoken & VT_TRACEID_BITMASK ) <
                   ( b->deftoken & VT_TRACEID_BITMASK );
      }

   };

   DefRec_DefProcessS()
      : DefRec_BaseS( DEF_REC_TYPE__DefProcess ),
        parent( 0 ) {}
   DefRec_DefProcessS( const uint32_t & _deftoken, const std::string & _name,
      const uint32_t & _parent )
      : DefRec_BaseS( DEF_REC_TYPE__DefProcess, 0, _deftoken ),
        name( _name ), parent( _parent ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefProcessS & a ) const
   {
      if( parent == a.parent )
         return name < a.name;
      else
         return parent < a.parent;
   }

   std::string name;
   uint32_t    parent;

};

//
// DefRec_DefProcessGroupS
//
struct DefRec_DefProcessGroupS : DefRec_BaseS
{
   typedef enum
   {
      TYPE_ALL, TYPE_NODE, TYPE_MPI_COMM_WORLD, TYPE_MPI_COMM_SELF,
      TYPE_MPI_COMM_OTHER, TYPE_MPI_GROUP, TYPE_USER_COMM, TYPE_OTHER,
      TYPE_UNKNOWN
   } ProcessGroupTypeT;

   DefRec_DefProcessGroupS()
      : DefRec_BaseS( DEF_REC_TYPE__DefProcessGroup ), type( TYPE_UNKNOWN ),
        members_hash( 0 ), nmembers( 0 ), members( 0 ) {}
   DefRec_DefProcessGroupS( const uint32_t & _loccpuid,
      const uint32_t & _deftoken, const ProcessGroupTypeT & _type,
      const std::string & _name, const uint32_t & _nmembers,
      const uint32_t * _members )
      : DefRec_BaseS( DEF_REC_TYPE__DefProcessGroup, _loccpuid, _deftoken ),
        type( _type ), name( _name ), members_hash( 0 ), nmembers( 0 ),
        members( 0 )
   {
      assignMembers( _nmembers, _members, _members + _nmembers );

      if( nmembers > 0 &&
          ( type == TYPE_MPI_COMM_WORLD || type == TYPE_MPI_COMM_OTHER ||
            type == TYPE_MPI_GROUP ) )
      {
         members_hash = vt_hashword( members, nmembers, 0 );
      }
   }
   DefRec_DefProcessGroupS( const DefRec_DefProcessGroupS & a )
      : DefRec_BaseS( DEF_REC_TYPE__DefProcessGroup, a.loccpuid, a.deftoken ),
        type( a.type ), name( a.name ), members_hash( a.members_hash ),
        nmembers( 0 ), members( 0 )
   {
      assignMembers( a.nmembers, a.members, a.members + a.nmembers );
   }
   ~DefRec_DefProcessGroupS()
   {
      if( nmembers > 0 )
         delete [] members;
   }

   template <class InputIterator>
   void assignMembers( uint32_t n, InputIterator first, InputIterator last )
   {
      if( nmembers > 0 )
         delete [] members;

      nmembers = n;
      members = 0;
      if( nmembers > 0 )
      {
         members = new uint32_t[nmembers];
         vt_assert( members );
         std::copy( first, last, members );
      }
   }

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefProcessGroupS & a ) const
   {
      if( type == a.type )
      {
         if( nmembers == a.nmembers )
         {
            if( name == a.name )
            {
               return
                  memcmp( members, a.members,
                     nmembers * sizeof( uint32_t ) ) < 0;
            }
            else
            {
               return name < a.name;
            }
         }
         else
         {
            return nmembers < a.nmembers;
         }
      }
      else
      {
         return type < a.type;
      }
   }

   ProcessGroupTypeT type;
   std::string       name;
   uint32_t          members_hash;
   uint32_t          nmembers;
   uint32_t *        members;

};

//
// DefRec_DefProcessGroupAttributesS
//
struct DefRec_DefProcessGroupAttributesS : DefRec_BaseS
{
   DefRec_DefProcessGroupAttributesS()
      : DefRec_BaseS( DEF_REC_TYPE__DefProcessGroupAttributes ),
        attributes( 0 ) {}
   DefRec_DefProcessGroupAttributesS( const uint32_t & _loccpuid,
      const uint32_t & _deftoken, const uint32_t & _attributes )
      : DefRec_BaseS( DEF_REC_TYPE__DefProcessGroupAttributes, _loccpuid,
          _deftoken ), attributes( _attributes ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefProcessGroupAttributesS & a ) const
   {
      return attributes < a.attributes;
   }

   uint32_t attributes;

};

//
// DefRec_DefSclFileS
//
struct DefRec_DefSclFileS : DefRec_BaseS
{
   DefRec_DefSclFileS()
      : DefRec_BaseS( DEF_REC_TYPE__DefSclFile ) {}
   DefRec_DefSclFileS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      std::string _filename)
      : DefRec_BaseS( DEF_REC_TYPE__DefSclFile, _loccpuid, _deftoken ),
        filename( _filename ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefSclFileS & a ) const
   {
      return filename < a.filename;
   }

   std::string filename;

};

//
// DefRec_DefSclS
//
struct DefRec_DefSclS : DefRec_BaseS
{
   DefRec_DefSclS()
      : DefRec_BaseS( DEF_REC_TYPE__DefScl ),
        sclfile( 0 ), sclline( 0 ) {}
   DefRec_DefSclS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const uint32_t & _sclfile, const uint32_t & _sclline )
      : DefRec_BaseS( DEF_REC_TYPE__DefScl, _loccpuid, _deftoken ),
        sclfile( _sclfile ), sclline( _sclline ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefSclS & a ) const
   {
      if( sclfile == a.sclfile )
         return sclline < a.sclline;
      else
         return sclfile < a.sclfile;
   }

   uint32_t sclfile;
   uint32_t sclline;

};

//
// DefRec_DefFileGroupS
//
struct DefRec_DefFileGroupS : DefRec_BaseS
{
   DefRec_DefFileGroupS()
      : DefRec_BaseS( DEF_REC_TYPE__DefFileGroup ) {}
   DefRec_DefFileGroupS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name)
      : DefRec_BaseS( DEF_REC_TYPE__DefFileGroup, _loccpuid, _deftoken ),
        name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefFileGroupS & a ) const
   {
      return name < a.name;
   }

   std::string name;

};

//
// DefRec_DefFileS
//
struct DefRec_DefFileS : DefRec_BaseS
{
   DefRec_DefFileS()
      : DefRec_BaseS( DEF_REC_TYPE__DefFile ),
        group( 0 ) {}
   DefRec_DefFileS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name, const uint32_t & _group )
      : DefRec_BaseS( DEF_REC_TYPE__DefFile, _loccpuid, _deftoken ),
        name( _name ), group( _group ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefFileS & a ) const
   {
      if( group == a.group )
         return name < a.name;
      else
         return group < a.group;
   }

   std::string name;
   uint32_t    group;

};

//
// DefRec_DefFunctionGroupS
//
struct DefRec_DefFunctionGroupS : DefRec_BaseS
{
   DefRec_DefFunctionGroupS()
      : DefRec_BaseS( DEF_REC_TYPE__DefFunctionGroup ) {}
   DefRec_DefFunctionGroupS( const uint32_t & _loccpuid,
      const uint32_t & _deftoken, const std::string & _name )
      : DefRec_BaseS( DEF_REC_TYPE__DefFunctionGroup, _loccpuid, _deftoken ),
        name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefFunctionGroupS & a ) const
   {
      return name < a.name;
   }

   std::string name;

};

//
// DefRec_DefFunctionS
//
struct DefRec_DefFunctionS : DefRec_BaseS
{
   DefRec_DefFunctionS()
      : DefRec_BaseS( DEF_REC_TYPE__DefFunction ),
        group( 0 ), scltoken( 0 ) {}
   DefRec_DefFunctionS(const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name, const uint32_t & _group,
      const uint32_t & _scltoken )
      : DefRec_BaseS( DEF_REC_TYPE__DefFunction, _loccpuid, _deftoken ),
        name( _name ), group( _group ), scltoken( _scltoken ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefFunctionS & a ) const
   {
      if( group == a.group )
      {
         if( scltoken == a.scltoken )
         {
            return name < a.name;
         }
         else
         {
            return scltoken < a.scltoken;
         }
      }
      else
      {
         return group < a.group;
      }
   }

   std::string name;
   uint32_t    group;
   uint32_t    scltoken;

};

//
// DefRec_DefCollOpS
//
struct DefRec_DefCollOpS : DefRec_BaseS
{
   DefRec_DefCollOpS()
      : DefRec_BaseS( DEF_REC_TYPE__DefCollOp ),
      type( 0 ) {}
   DefRec_DefCollOpS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name, const uint32_t & _type)
      : DefRec_BaseS( DEF_REC_TYPE__DefCollOp, _loccpuid, _deftoken ),
        name( _name ), type( _type ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefCollOpS & a ) const
   {
      if( type == a.type )
         return name < a.name;
      else
         return type < a.type;
   }

   std::string name;
   uint32_t    type;

};

//
// DefRec_DefCounterGroupS
//
struct DefRec_DefCounterGroupS : DefRec_BaseS
{
   DefRec_DefCounterGroupS()
      : DefRec_BaseS( DEF_REC_TYPE__DefCounterGroup ) {}
   DefRec_DefCounterGroupS( const uint32_t & _loccpuid,
      const uint32_t & _deftoken, const std::string & _name)
      : DefRec_BaseS( DEF_REC_TYPE__DefCounterGroup, _loccpuid, _deftoken ),
        name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefCounterGroupS & a ) const
   {
      return name < a.name;
   }

   std::string name;

};

//
// DefRec_DefCounterS
//
struct DefRec_DefCounterS : DefRec_BaseS
{
   DefRec_DefCounterS()
      : DefRec_BaseS( DEF_REC_TYPE__DefCounter ),
        properties( 0 ), group( 0 ) {}
   DefRec_DefCounterS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name, const uint32_t & _properties,
      const uint32_t & _group, const std::string & _unit)
      : DefRec_BaseS( DEF_REC_TYPE__DefCounter, _loccpuid, _deftoken ),
        name( _name ), properties( _properties ), group( _group ),
        unit( _unit ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefCounterS & a ) const
   {
      if( properties == a.properties )
      {
         if( group == a.group )
         {
            if( name == a.name )
            {
               return unit < a.unit;
            }
            else
            {
               return name < a.name;
            }
         }
         else
         {
            return group < a.group;
         }
      }
      else
      {
         return properties < a.properties;
      }
   }

   std::string name;
   uint32_t    properties;
   uint32_t    group;
   std::string unit;

};

//
// DefRec_DefCounterAssignmentsS
//
struct DefRec_DefCounterAssignmentsS : DefRec_BaseS
{
   DefRec_DefCounterAssignmentsS()
      : DefRec_BaseS( DEF_REC_TYPE__DefCounterAssignments ) {}
   DefRec_DefCounterAssignmentsS( const uint32_t & _loccpuid,
      const uint32_t & _counter, const uint32_t & _group )
      : DefRec_BaseS( DEF_REC_TYPE__DefCounterAssignments, _loccpuid, _counter )
   {
      groups.insert( _group );
   }
   DefRec_DefCounterAssignmentsS( const uint32_t & _loccpuid,
      const uint32_t & _counter, const std::set<uint32_t> & _groups )
      : DefRec_BaseS( DEF_REC_TYPE__DefCounterAssignments, _loccpuid,
           _counter ), groups( _groups ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   std::set<uint32_t> groups;

};

//
// DefRec_DefKeyValueS
//
struct DefRec_DefKeyValueS : DefRec_BaseS
{
   DefRec_DefKeyValueS()
      : DefRec_BaseS( DEF_REC_TYPE__DefKeyValue ),
        type( OTF_UNKNOWN ) {}
   DefRec_DefKeyValueS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const OTF_Type & _type, const std::string & _name )
      : DefRec_BaseS( DEF_REC_TYPE__DefKeyValue, _loccpuid, _deftoken ),
        type( _type ), name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefKeyValueS & a ) const
   {
      if( type == a.type )
         return name < a.name;
      else
         return type < a.type;
   }

   OTF_Type type;
   std::string name;

};

//
// DefRec_DefMarkerS
//
struct DefRec_DefMarkerS: DefRec_BaseS
{
   //
   // compare structure for final sort
   //
   struct SortS
   {
      bool operator()( const DefRec_DefMarkerS * a,
                       const DefRec_DefMarkerS * b ) const
      {
         if( a->type == b->type )
            return a->deftoken < b->deftoken;
         else
            return a->type < b->type;
      }

   };

   DefRec_DefMarkerS()
      : DefRec_BaseS( DEF_REC_TYPE__DefMarker ),
        type( 0 ) {}
   DefRec_DefMarkerS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const uint32_t & _type, const std::string & _name)
      : DefRec_BaseS( DEF_REC_TYPE__DefMarker, _loccpuid, _deftoken ),
        type( _type ), name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   // operator for searching
   bool operator<( const DefRec_DefMarkerS & a ) const
   {
      if( type == a.type )
         return name < a.name;
      else
         return type < a.type;
   }

   uint32_t    type;
   std::string name;

};

// compare function for pre-sorting local definitions
inline bool DefRec_LocCmp( const DefRec_BaseS * a, const DefRec_BaseS * b )
{
   // if both are process group definitions (i.e. user created MPI comms.),
   // sort by its local token to preserve the order of creation
   if( a->dtype == DEF_REC_TYPE__DefProcessGroup &&
       b->dtype == DEF_REC_TYPE__DefProcessGroup )
      return a->deftoken < b->deftoken;
   // otherwise, sort by definition type
   else
      return a->dtype < b->dtype;
}

#endif // _VT_UNIFY_DEFS_RECS_H_
