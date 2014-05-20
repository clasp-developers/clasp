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

#ifndef _VT_UNIFY_MARKERS_H_
#define _VT_UNIFY_MARKERS_H_

#include "vt_unify.h"
#include "vt_unify_defs_recs.h"
#include "vt_unify_lvector.hh"
#include "vt_unify_tkfac.h"

//
// MarkersC class
//
class MarkersC
{
public:

   //
   // marker spot structure
   //
   struct MarkerSpotS
   {
      MarkerSpotS()
         : proc( 0 ), time( 0 ), marker( 0 ) {}
      MarkerSpotS( const uint32_t & _proc, const uint64_t & _time,
                   const uint32_t & _marker, const std::string & _text )
         : proc( _proc ), time( _time ), marker( _marker ), text( _text ) {}

#ifdef VT_MPI
      // get size needed to pack marker spot
      VT_MPI_INT getPackSize();
      // pack marker spot into a buffer
      void pack( char *& buffer, const VT_MPI_INT & bufferSize,
                 VT_MPI_INT & bufferPos );
      // unpack marker spot from a buffer
      void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                   VT_MPI_INT & bufferPos );
#endif // VT_MPI

      // operator for sorting global marker spots
      // (not really necessary - but it can't hurt)
      bool operator<( const MarkerSpotS & a ) const
      {
         if( proc == a.proc )
            return time < a.time;
         else
            return proc < a.proc;
      }

      uint32_t proc;    // process id
      uint64_t time;    // timestamp
      uint32_t marker;  // marker id
      std::string text; // marker text

   };

   // constructor
   MarkersC();

   // destructor
   ~MarkersC();

   // unify markers
   bool run();

   // rename temporary output files
   bool cleanUp();

private:

   // read local markers
   bool readLocal();

   // read local markers of certain stream
   bool readLocal( const uint32_t & streamId,
           LargeVectorC<DefRec_DefMarkerS*> & locDefs,
           LargeVectorC<MarkerSpotS*> & locSpots );

   // write global markers
   bool writeGlobal();

#ifdef VT_MPI

   // gather either local marker defs. or spots
   typedef enum { GATHER_TYPE_DEFS, GATHER_TYPE_SPOTS } GatherTypeT;
   bool gatherLocal( const GatherTypeT & type, void * locRecs );

#endif // VT_MPI

   // token factory scope for marker definitions
   TokenFactoryScopeC<DefRec_DefMarkerS> * m_tkfacScope;

   // global marker definitions
   std::set<DefRec_DefMarkerS> m_globDefs;

   // global marker spots
   LargeVectorC<MarkerSpotS> m_globSpots;

};

// instance of class MarkersC
extern MarkersC * theMarkers;

#endif // _VT_UNIFY_MARKERS_H_
