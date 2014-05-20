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

#ifndef _VT_UNIFY_HOOKS_RAW_H_
#define _VT_UNIFY_HOOKS_RAW_H_

#include "vt_unify.h"
#include "vt_unify_hooks_base.h"

//
// HooksRawC class
//
class HooksRawC : public HooksBaseC
{
public:

   // constructor
   HooksRawC();

   // destructor
   ~HooksRawC();

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

   void phaseHook_GetUnifyControls_pre();
   void phaseHook_GetUnifyControls_post();

   void phaseHook_UnifyDefinitions_pre();
   void phaseHook_UnifyDefinitions_post();

   void phaseHook_UnifyMarkers_pre();
   void phaseHook_UnifyMarkers_post();

   void phaseHook_UnifyStatistics_pre();
   void phaseHook_UnifyStatistics_post();

   void phaseHook_UnifyEvents_pre();
   void phaseHook_UnifyEvents_post();

   void phaseHook_WriteMasterControl_pre();
   void phaseHook_WriteMasterControl_post();

   void phaseHook_CleanUp_pre();
   void phaseHook_CleanUp_post();

   // record hooks
   //

   // definition records

   void readRecHook_DefComment( HooksC::VaArgsT & args );
   void writeRecHook_DefComment( HooksC::VaArgsT & args );

   void readRecHook_DefCreator( HooksC::VaArgsT & args );
   void writeRecHook_DefCreator( HooksC::VaArgsT & args );

   void readRecHook_DefTimerResolution( HooksC::VaArgsT & args );
   void writeRecHook_DefTimerResolution( HooksC::VaArgsT & args );

   void readRecHook_DefTimeRange( HooksC::VaArgsT & args );
   void writeRecHook_DefTimeRange( HooksC::VaArgsT & args );

   void readRecHook_DefProcessGroup( HooksC::VaArgsT & args );
   void writeRecHook_DefProcessGroup( HooksC::VaArgsT & args );

   void readRecHook_DefProcessGroupAttributes( HooksC::VaArgsT & args );
   void writeRecHook_DefProcessGroupAttributes( HooksC::VaArgsT & args );

   void readRecHook_DefProcess( HooksC::VaArgsT & args );
   void writeRecHook_DefProcess( HooksC::VaArgsT & args );

   void readRecHook_DefSclFile( HooksC::VaArgsT & args );
   void writeRecHook_DefSclFile( HooksC::VaArgsT & args );

   void readRecHook_DefScl( HooksC::VaArgsT & args );
   void writeRecHook_DefScl( HooksC::VaArgsT & args );

   void readRecHook_DefFileGroup( HooksC::VaArgsT & args );
   void writeRecHook_DefFileGroup( HooksC::VaArgsT & args );

   void readRecHook_DefFile( HooksC::VaArgsT & args );
   void writeRecHook_DefFile( HooksC::VaArgsT & args );

   void readRecHook_DefFunctionGroup( HooksC::VaArgsT & args );
   void writeRecHook_DefFunctionGroup( HooksC::VaArgsT & args );

   void readRecHook_DefFunction( HooksC::VaArgsT & args );
   void writeRecHook_DefFunction( HooksC::VaArgsT & args );

   void readRecHook_DefCollOp( HooksC::VaArgsT & args );
   void writeRecHook_DefCollOp( HooksC::VaArgsT & args );

   void readRecHook_DefCounterGroup( HooksC::VaArgsT & args );
   void writeRecHook_DefCounterGroup( HooksC::VaArgsT & args );

   void readRecHook_DefCounter( HooksC::VaArgsT & args );
   void writeRecHook_DefCounter( HooksC::VaArgsT & args );

   void readRecHook_DefCounterAssignments( HooksC::VaArgsT & args );
   void writeRecHook_DefCounterAssignments( HooksC::VaArgsT & args );

   void readRecHook_DefKeyValue( HooksC::VaArgsT & args );
   void writeRecHook_DefKeyValue( HooksC::VaArgsT & args );

   // summary records

   void readRecHook_FunctionSummary( HooksC::VaArgsT & args );
   void writeRecHook_FunctionSummary( HooksC::VaArgsT & args );

   void readRecHook_MessageSummary( HooksC::VaArgsT & args );
   void writeRecHook_MessageSummary( HooksC::VaArgsT & args );

   void readRecHook_CollOpSummary( HooksC::VaArgsT & args );
   void writeRecHook_CollOpSummary( HooksC::VaArgsT & args );

   void readRecHook_FileOpSummary( HooksC::VaArgsT & args );
   void writeRecHook_FileOpSummary( HooksC::VaArgsT & args );

   // marker records

   void readRecHook_DefMarker( HooksC::VaArgsT & args );
   void writeRecHook_DefMarker( HooksC::VaArgsT & args );

   void readRecHook_MarkerSpot( HooksC::VaArgsT & args );
   void writeRecHook_MarkerSpot( HooksC::VaArgsT & args );

   // event records

   void readRecHook_Enter( HooksC::VaArgsT & args );
   void writeRecHook_Enter( HooksC::VaArgsT & args );

   void readRecHook_Leave( HooksC::VaArgsT & args );
   void writeRecHook_Leave( HooksC::VaArgsT & args );

   void readRecHook_BeginFileOp( HooksC::VaArgsT & args );
   void writeRecHook_BeginFileOp( HooksC::VaArgsT & args );

   void readRecHook_EndFileOp( HooksC::VaArgsT & args );
   void writeRecHook_EndFileOp( HooksC::VaArgsT & args );

   void readRecHook_SendMsg( HooksC::VaArgsT & args );
   void writeRecHook_SendMsg( HooksC::VaArgsT & args );

   void readRecHook_RecvMsg( HooksC::VaArgsT & args );
   void writeRecHook_RecvMsg( HooksC::VaArgsT & args );

   void readRecHook_BeginCollOp( HooksC::VaArgsT & args );
   void writeRecHook_BeginCollOp( HooksC::VaArgsT & args );

   void readRecHook_EndCollOp( HooksC::VaArgsT & args );
   void writeRecHook_EndCollOp( HooksC::VaArgsT & args );

   void readRecHook_RMAPut( HooksC::VaArgsT & args );
   void writeRecHook_RMAPut( HooksC::VaArgsT & args );

   void readRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args );
   void writeRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args );

   void readRecHook_RMAGet( HooksC::VaArgsT & args );
   void writeRecHook_RMAGet( HooksC::VaArgsT & args );

   void readRecHook_RMAEnd( HooksC::VaArgsT & args );
   void writeRecHook_RMAEnd( HooksC::VaArgsT & args );

   void readRecHook_Counter( HooksC::VaArgsT & args );
   void writeRecHook_Counter( HooksC::VaArgsT & args );

   void readRecHook_EventComment( HooksC::VaArgsT & args );
   void writeRecHook_EventComment( HooksC::VaArgsT & args );

   // snapshot records

   void writeRecHook_EnterSnapshot( HooksC::VaArgsT & args );
   void writeRecHook_SendSnapshot( HooksC::VaArgsT & args );
   void writeRecHook_OpenFileSnapshot( HooksC::VaArgsT & args );
   void writeRecHook_BeginFileOpSnapshot( HooksC::VaArgsT & args );
   void writeRecHook_BeginCollOpSnapshot( HooksC::VaArgsT & args );
   void writeRecHook_CollOpCountSnapshot( HooksC::VaArgsT & args );
   void writeRecHook_CounterSnapshot( HooksC::VaArgsT & args );

   // generic hook
   void genericHook( const uint32_t & id, HooksC::VaArgsT & args );

   // ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

};

#endif // _VT_UNIFY_HOOKS_RAW_H_
