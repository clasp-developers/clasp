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

#ifndef _VT_UNIFY_HOOKS_MARGINS_H_
#define _VT_UNIFY_HOOKS_MARGINS_H_

#include "vt_unify.h"
#include "vt_unify_hooks_base.h"

#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
#  include "vt_unify_hooks_msgmatch_snaps.h"
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

#include "otf.h"

//
// HooksProcessMarginsC class
//
class HooksProcessMarginsC : public HooksBaseC
{
public:

   // constructor
   HooksProcessMarginsC();

   // destructor
   ~HooksProcessMarginsC();

   // is this hook enabled?
   static bool isEnabled() { return true; }

private:

   //
   // thread context structure
   //
   struct ThreadContextS
   {
      ThreadContextS()
         : wstream( 0 ), streamid( 0 ), first_event( true ), last_time( 0 ) {}

      OTF_WStream * wstream;     // OTF writer stream
      uint32_t      streamid;    // stream id
      bool          first_event; // flag: first event record to write?
      uint64_t      last_time;   // last written timestamp

   };

   // vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

   // initialization/finalization hooks
   //

   void initHook();
   void finalizeHook( const bool & error );

   // phase hooks
   //

   void phaseHook_UnifyEvents_pre();
   void phaseHook_UnifyEvents_post();

   // record hooks
   //

   // event records

   // common stuff for write event record hooks
   void writeRecHook_Event( OTF_WStream ** wstream, uint64_t * time,
           uint32_t * streamid, bool * dowrite );

   void writeRecHook_EventComment( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[5] );
   }

   void writeRecHook_Enter( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[3], (bool*)args[6] );
   }

   void writeRecHook_Leave( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[3], (bool*)args[6] );
   }

   void writeRecHook_Counter( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[7] );
   }

   void writeRecHook_BeginFileOp( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[6] );
   }

   void writeRecHook_EndFileOp( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[10] );
   }

   void writeRecHook_SendMsg( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[9] );
   }

   void writeRecHook_RecvMsg( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[9] );
   }

   void writeRecHook_BeginCollOp( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[11] );
   }

   void writeRecHook_EndCollOp( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[5] );
   }

   void writeRecHook_RMAPut( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[10] );
   }

   void writeRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[10] );
   }
   void writeRecHook_RMAGet( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[10] );
   }

   void writeRecHook_RMAEnd( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[8] );
   }

#if defined(VT_UNIFY_HOOKS_MSGMATCH_SNAPS) && \
    defined(VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS)

   // snapshot records
   // (only needed when generating inline snapshots)

   void writeRecHook_EnterSnapshot( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[4], (bool*)args[7] );
   }

   void writeRecHook_SendSnapshot( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[3], (bool*)args[10] );
   }

   void writeRecHook_OpenFileSnapshot( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[4], (bool*)args[8] );
   }

   void writeRecHook_BeginFileOpSnapshot( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[3], (bool*)args[7] );
   }

   void writeRecHook_BeginCollOpSnapshot( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[3], (bool*)args[12] );
   }

   void writeRecHook_CollOpCountSnapshot( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[2], (bool*)args[6] );
   }

   void writeRecHook_CounterSnapshot( HooksC::VaArgsT & args )
   {
      writeRecHook_Event( (OTF_WStream**)args[0], (uint64_t*)args[1],
         (uint32_t*)args[3], (bool*)args[7] );
   }

#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS &&
       // VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS

   // generic hook
   void genericHook( const uint32_t & id, HooksC::VaArgsT & args );

   // ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

   // maximum number of threads to use for unifying events
   int m_maxThreads;

   // array of thread contexts
   ThreadContextS * m_threadContexts;

};

#endif // _VT_UNIFY_HOOKS_MARGINS_H_
