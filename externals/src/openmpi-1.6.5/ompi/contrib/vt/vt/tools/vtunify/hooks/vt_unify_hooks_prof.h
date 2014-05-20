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

#ifndef _VT_UNIFY_HOOKS_PROF_H_
#define _VT_UNIFY_HOOKS_PROF_H_

#include "vt_unify.h"
#include "vt_unify_hooks_base.h"

#include <algorithm>

//
// HooksProfC class
//
class HooksProfC : public HooksBaseC
{
public:

   //
   // function profile sort flags
   //
   enum
   {
      FUNC_PROF_SORT_FLAG_DIR_UP    = 0x1,
      FUNC_PROF_SORT_FLAG_DIR_DOWN  = 0x2,
      FUNC_PROF_SORT_FLAG_FUNCNAME  = 0x4,
      FUNC_PROF_SORT_FLAG_COUNT     = 0x8,
      FUNC_PROF_SORT_FLAG_INCL      = 0x10,
      FUNC_PROF_SORT_FLAG_EXCL      = 0x20,
      FUNC_PROF_SORT_FLAG_INCL_CALL = 0x40,
      FUNC_PROF_SORT_FLAG_EXCL_CALL = 0x80

   };

   //
   // function profile structure
   //
   struct FuncProfS
   {
      FuncProfS()
         : funcid( 0 ), count( 0.0 ), incl( 0 ), excl( 0 ) {}

      FuncProfS( const uint32_t & _funcid )
         : funcid( _funcid ), count( 0.0 ), incl( 0 ), excl( 0 ) {}

      FuncProfS( const uint32_t & _funcid, const std::string & _funcname,
                 const double & _count, const uint64_t & _incl,
                 const uint64_t & _excl )
         : funcid( _funcid ), funcname( _funcname ), count( _count ),
           incl( _incl ), excl( _excl ) {}

#ifdef VT_MPI
      VT_MPI_INT getPackSize();
      void pack( char *& buffer, const VT_MPI_INT & bufferSize,
                 VT_MPI_INT & bufferPos );
      void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                   VT_MPI_INT & bufferPos );
#endif // VT_MPI

      inline FuncProfS & operator+=( const FuncProfS & a );
      inline bool operator==( const FuncProfS & a ) const;
      inline bool operator<( const FuncProfS & a) const;

      uint32_t    funcid;   // function identifier
      std::string funcname; // function name
      double      count;    // number of calls
      uint64_t    incl;     // inclusive time
      uint64_t    excl;     // exclusive time

   };

   // constructor
   HooksProfC();

   // destructor
   ~HooksProfC();

   // is this hook enabled?
   static bool isEnabled() { return true; }

private:

   // vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

   // initialization/finalization hooks
   //

   void initHook();
   void finalizeHook( const bool & error );

   // phase hooks
   //

   void phaseHook_GetUnifyControls_post();
   void phaseHook_UnifyStatistics_post();

   // record hooks
   //

   void writeRecHook_DefTimerResolution( HooksC::VaArgsT & args );
   void writeRecHook_DefProcess( HooksC::VaArgsT & args );
   void writeRecHook_DefFunction( HooksC::VaArgsT & args );
   void writeRecHook_FunctionSummary( HooksC::VaArgsT & args );

   // ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

   // process function statistics
   void processFuncStat( const uint32_t & procId, const uint32_t & funcId,
                         const uint64_t & count, const uint64_t & incl,
                         const uint64_t & excl );

   // get function profile
   // (procId=0 -> summary over all processes)
   void getFuncProf( std::vector<FuncProfS> & funcProfs,
                     const uint32_t & procId = 0 );

   // print/write function profile to stdout/file
   // (outFile=<empty> -> print to stdout)
   bool printFuncProf( const std::vector<FuncProfS> & funcProfs,
                       const std::string & outFile = "" );

   // check for available function profile
   // (procId=0 -> any process)
   bool haveFuncProf( const uint32_t & procId = 0 );

   // get function name by id
   std::string getFuncNameById( const uint32_t & funcId );

   // trim function name (only for output on stdout)
   std::string shortName( const std::string & longName, uint32_t len = 20 );

   // convert timestamp to a human readable format
   std::string formatTime( const uint64_t & time );

#ifdef VT_MPI

   // gather function profiles from all ranks
   void gatherFuncProfs();

#endif // VT_MPI

   // map function id <-> function name
   // (only significant for rank 0)
   std::map<uint32_t, std::string> m_funcId2Name;

   // vector of summary function profiles
   // (only significant for rank 0)
   std::vector<FuncProfS> m_sumFuncProfs;

   // map process id <-> map function id <-> function profile
   std::map<uint32_t, std::map<uint32_t, FuncProfS> > m_procId2FuncProf;

   // number of processes
   uint32_t m_numProcs;

   // timer resolution
   uint64_t m_timerRes;

};

#endif // _VT_UNIFY_HOOKS_PROF_H_
