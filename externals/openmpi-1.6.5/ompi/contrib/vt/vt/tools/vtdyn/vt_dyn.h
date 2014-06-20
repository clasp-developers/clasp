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

#ifndef _VT_DYN_H_
#define _VT_DYN_H_

#include "config.h"

#include "vt_defs.h"
#include "vt_inttypes.h"

#include "rfg_filter.h"

#include "BPatch.h"
#include "BPatch_addressSpace.h"
#include "BPatch_flowGraph.h"
#include "BPatch_function.h"
#include "BPatch_image.h"

#include <iostream>
#include <string>
#include <vector>

#define STRBUFSIZE 1024

// define the following macro to exclude regions from instrumentation which
// require using a trap
// (see description of BPatch_addressSpace::allowTraps and
//  BPatch_point::usesTrap_NP in the Dyninst Programmer's Guide for more
//  details)
#define NOTRAPS

//
// mutation modes
//
typedef enum
{
   // either create/attach to a process, instrument, and execute
   //
   MODE_CREATE,
   MODE_ATTACH,
   // or open, instrument, and rewrite binary
   MODE_REWRITE

} MutationT;

//
// structure that contains the mutator parameters
// (i.e. command line options)
//
struct ParamsS
{
   ParamsS()
      : mode( MODE_CREATE ), mutatee_pid( -1 ), verbose_level( 1 ),
        detach( true ), outer_loops( false ), inner_loops( false ),
        loop_iters( false ), ignore_no_dbg( false ), show_usage( false ),
        show_version( false ) {}

   MutationT                mode;          // mutation mode
   std::string              mutatee;       // mutatee executable name
   int                      mutatee_pid;   // mutatee PID
   std::vector<std::string> mutatee_args;  // mutatee arguments
   std::vector<std::string> shlibs;        // shared libs. to instrument
   std::string              filtfile;      // pathname of filter file
   std::string              outfile;       // file name of binary to rewrite
   uint32_t                 verbose_level; // verbose level
   bool                     detach;        // flag: detach from mutatee?
   bool                     outer_loops;   // flag: instrument outer loops?
   bool                     inner_loops;   // flag: instrument inner loops?
   bool                     loop_iters;    // flag: instrument loop iterations?
   bool                     ignore_no_dbg; // flag: ignore funcs. without debug?
   bool                     show_usage;    // flag: show usage text?
   bool                     show_version;  // flag: show VampirTrace version?

};

//
// MutatorC class
//
class MutatorC
{
public:

   // constructor
   MutatorC();

   // destructor
   ~MutatorC();

   // run the mutator
   bool run();

private:

   //
   // base structure for regions (=functions, loop, or loop iterations)
   // to instrument
   //
   struct RegionS
   {
      //
      // structure for region source code location
      //
      struct SclS
      {
         SclS() : line_number( 0 ) {}

         // check whether source code location is valid
         bool valid() const
         {
            return ( line_number > 0 && file_name.length() > 0 );
         }

         std::string file_name; // source file name
         uint32_t line_number;  // line number within source file

      };

      //
      // structure for region instrumentation points
      //
      struct InstPointsS
      {
         InstPointsS() : entries( 0 ), exits( 0 ) {}

         const BPatch_Vector<BPatch_point*> * entries; // entry points
         const BPatch_Vector<BPatch_point*> * exits;   // exit points

      };

      // constructor
      RegionS( const std::string & _name, const SclS & _scl,
         const InstPointsS & _inst_points );

      // destructor
      virtual ~RegionS();

      // new operator to check number of created regions
      // (returns 0 if VT_MAX_DYNINST_REGIONS will be exceeded)
      static inline void * operator new( size_t size ) throw();

      // the overloaded new operator calls malloc(), so we have to have a
      // delete operator which calls free()
      static inline void operator delete( void * ptr );

      // counter of regions to instrument (max. VT_MAX_DYNINST_REGIONS)
      static uint32_t Count;

      // region index
      uint32_t index;

      // region name
      std::string name;

      // region source code location
      SclS scl;

      // region instrumentation points
      InstPointsS inst_points;

   };

   //
   // structure for loop regions to instrument
   //
   struct LoopS : RegionS
   {
      //
      // type for loop iteration regions
      //
      typedef RegionS IterationT;

      // constructor
      LoopS( const std::string & _name, const SclS & _scl,
         const InstPointsS & _inst_points, IterationT * _iteration = 0 )
         : RegionS( _name, _scl, _inst_points ), iteration( _iteration ) {}

      // destructor
      ~LoopS()
      {
         if( iteration )
            delete iteration;
      }

      // loop iteration region to instrument
      IterationT * iteration;

   };

   //
   // structure for function regions to instrument
   //
   struct FunctionS : RegionS
   {
      // constructor
      FunctionS( const std::string & _name, const SclS & _scl,
         const InstPointsS & _inst_points,
         const std::vector<LoopS*> & _loops = std::vector<LoopS*>() )
         : RegionS( _name, _scl, _inst_points ), loops( _loops ) {}

      // loops within the function
      std::vector<LoopS*> loops;

      // destructor
      ~FunctionS()
      {
         for( uint32_t i = 0; i < loops.size(); i++ )
            delete loops[i];
      }
   };

   // create/attach to a process or open binary for rewriting
   bool initialize();

   // continue execution of mutatee or rewrite binary
   bool finalize( bool & error );

   // get functions to instrument
   bool getFunctions( std::vector<FunctionS*> & funcRegions ) const;

   // instrument functions
   bool instrumentFunctions(
           const std::vector<FunctionS*> & funcRegions ) const;

   // instrument a region entry
   inline bool instrumentRegionEntry( const RegionS * region,
                  const bool isLoop ) const;

   // instrument a region exit
   inline bool instrumentRegionExit( const RegionS * region,
                  const bool isLoop ) const;

   // check whether module is excluded from instrumentation
   inline bool constraintModule( const std::string & name ) const;

   // check whether region is excluded from instrumentation
   inline bool constraintRegion( const std::string & name ) const;

   // check whether mutatee uses MPI
   inline bool isMPI() const;

   // find function in mutatee
   inline BPatch_function * findFunction( const std::string & name ) const;

   // find instrumentation point(s) in a function (where=BPatch_function*)
   // or in a loop (where=BPatch_flowGraph*)
   // If NOTRAPS is defined, this function returns NULL, if inserting
   // instrumentation at found point(s) requires using a trap.
   inline BPatch_Vector<BPatch_point*> * findPoint( void * where,
                  BPatch_procedureLocation loc,
                  BPatch_basicBlockLoop * loop = 0 ) const;

   // entire Dyninst library object
   BPatch m_bpatch;

   // mutatee's process or binary edit object
   BPatch_addressSpace * m_appAddrSpace;

   // mutatee's image object
   BPatch_image * m_appImage;

   // instrumentation functions to insert at entry/exit points
   //
   BPatch_function * m_vtStartFunc;
   BPatch_function * m_vtEndFunc;

   // RFG filter object to include/exclude functions from instrumenting
   RFG_Filter * m_filter;

};

#endif // _VT_DYN_H_
