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

#include "vt_unify_hooks_raw.h"

#include <iostream>

//////////////////// class HooksRawC ////////////////////

// public methods
//

HooksRawC::HooksRawC() : HooksBaseC()
{
   // Empty
}

HooksRawC::~HooksRawC()
{
   // Empty
}

// private methods
//

#define DOSOMETHING \
   PVPrint( 3, "Triggered raw hook %s\n", __func__ )

// vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

// initialization/finalization hooks
//

void HooksRawC::initHook( void ) { DOSOMETHING; }
void HooksRawC::finalizeHook( const bool & error ) { DOSOMETHING; }

// phase hooks
//

void HooksRawC::phaseHook_GetUnifyControls_pre() { DOSOMETHING; }
void HooksRawC::phaseHook_GetUnifyControls_post() { DOSOMETHING; }

void HooksRawC::phaseHook_UnifyDefinitions_pre() { DOSOMETHING; }
void HooksRawC::phaseHook_UnifyDefinitions_post() { DOSOMETHING; }

void HooksRawC::phaseHook_UnifyMarkers_pre() { DOSOMETHING; }
void HooksRawC::phaseHook_UnifyMarkers_post() { DOSOMETHING; }

void HooksRawC::phaseHook_UnifyStatistics_pre() { DOSOMETHING; }
void HooksRawC::phaseHook_UnifyStatistics_post() { DOSOMETHING; }

void HooksRawC::phaseHook_UnifyEvents_pre() { DOSOMETHING; }
void HooksRawC::phaseHook_UnifyEvents_post() { DOSOMETHING; }

void HooksRawC::phaseHook_WriteMasterControl_pre() { DOSOMETHING; }
void HooksRawC::phaseHook_WriteMasterControl_post() { DOSOMETHING; }

void HooksRawC::phaseHook_CleanUp_pre() { DOSOMETHING; }
void HooksRawC::phaseHook_CleanUp_post() { DOSOMETHING; }

// record hooks
//

// definition records

void HooksRawC::readRecHook_DefComment( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefComment( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefCreator( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefCreator( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefTimerResolution( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefTimerResolution( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefTimeRange( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefTimeRange( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefProcessGroup( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefProcessGroup( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefProcessGroupAttributes( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefProcessGroupAttributes( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefProcess( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefProcess( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefSclFile( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefSclFile( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefScl( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefScl( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefFileGroup( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefFileGroup( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefFile( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefFile( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefFunctionGroup( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefFunctionGroup( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefFunction( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefFunction( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefCollOp( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefCollOp( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefCounterGroup( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefCounterGroup( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefCounter( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefCounter( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefCounterAssignments( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefCounterAssignments( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_DefKeyValue( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefKeyValue( HooksC::VaArgsT & args ) { DOSOMETHING; }

// summary records

void HooksRawC::readRecHook_FunctionSummary( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_FunctionSummary( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_MessageSummary( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_MessageSummary( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_CollOpSummary( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_CollOpSummary( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_FileOpSummary( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_FileOpSummary( HooksC::VaArgsT & args ) { DOSOMETHING; }

// marker records

void HooksRawC::readRecHook_DefMarker( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_DefMarker( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_MarkerSpot( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_MarkerSpot( HooksC::VaArgsT & args ) { DOSOMETHING; }

// event records

void HooksRawC::readRecHook_Enter( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_Enter( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_Leave( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_Leave( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_BeginFileOp( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_BeginFileOp( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_EndFileOp( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_EndFileOp( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_SendMsg( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_SendMsg( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_RecvMsg( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_RecvMsg( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_BeginCollOp( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_BeginCollOp( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_EndCollOp( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_EndCollOp( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_RMAPut( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_RMAPut( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_RMAGet( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_RMAGet( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_RMAEnd( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_RMAEnd( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_Counter( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_Counter( HooksC::VaArgsT & args ) { DOSOMETHING; }

void HooksRawC::readRecHook_EventComment( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_EventComment( HooksC::VaArgsT & args ) { DOSOMETHING; }

// snapshot records

void HooksRawC::writeRecHook_EnterSnapshot( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_SendSnapshot( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_OpenFileSnapshot( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_BeginFileOpSnapshot( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_BeginCollOpSnapshot( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_CollOpCountSnapshot( HooksC::VaArgsT & args ) { DOSOMETHING; }
void HooksRawC::writeRecHook_CounterSnapshot( HooksC::VaArgsT & args ) { DOSOMETHING; }

// generic hook
void HooksRawC::genericHook( const uint32_t & id, HooksC::VaArgsT & args ) { DOSOMETHING; }

// ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^
