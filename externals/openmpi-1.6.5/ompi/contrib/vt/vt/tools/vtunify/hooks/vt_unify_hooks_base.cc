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

#include "vt_unify_hooks_base.h"

//////////////////// class HooksBaseC ////////////////////

// public methods
//

HooksBaseC::HooksBaseC()
{
   // register pointer of hook methods
   //

   // phase hooks
   //

   m_phaseMethods.resize( HooksC::Phase_Num, 0 );

   m_phaseMethods[HooksC::Phase_GetUnifyControls_pre] =
      &HooksBaseC::phaseHook_GetUnifyControls_pre;
   m_phaseMethods[HooksC::Phase_GetUnifyControls_post] =
      &HooksBaseC::phaseHook_GetUnifyControls_post;

   m_phaseMethods[HooksC::Phase_UnifyDefinitions_pre] =
      &HooksBaseC::phaseHook_UnifyDefinitions_pre;
   m_phaseMethods[HooksC::Phase_UnifyDefinitions_post] =
      &HooksBaseC::phaseHook_UnifyDefinitions_post;

   m_phaseMethods[HooksC::Phase_UnifyMarkers_pre] =
      &HooksBaseC::phaseHook_UnifyMarkers_pre;
   m_phaseMethods[HooksC::Phase_UnifyMarkers_post] =
      &HooksBaseC::phaseHook_UnifyMarkers_post;

   m_phaseMethods[HooksC::Phase_UnifyStatistics_pre] =
      &HooksBaseC::phaseHook_UnifyStatistics_pre;
   m_phaseMethods[HooksC::Phase_UnifyStatistics_post] =
      &HooksBaseC::phaseHook_UnifyStatistics_post;

   m_phaseMethods[HooksC::Phase_UnifyEvents_pre] =
      &HooksBaseC::phaseHook_UnifyEvents_pre;
   m_phaseMethods[HooksC::Phase_UnifyEvents_post] =
      &HooksBaseC::phaseHook_UnifyEvents_post;

   m_phaseMethods[HooksC::Phase_WriteMasterControl_pre] =
      &HooksBaseC::phaseHook_WriteMasterControl_pre;
   m_phaseMethods[HooksC::Phase_WriteMasterControl_post] =
      &HooksBaseC::phaseHook_WriteMasterControl_post;

   m_phaseMethods[HooksC::Phase_CleanUp_pre] =
      &HooksBaseC::phaseHook_CleanUp_pre;
   m_phaseMethods[HooksC::Phase_CleanUp_post] =
      &HooksBaseC::phaseHook_CleanUp_post;

   // record hooks
   //

   m_readRecHookMethods.resize( HooksC::Record_Num, 0 );
   m_writeRecHookMethods.resize( HooksC::Record_Num, 0 );

   // definition records

   m_readRecHookMethods[HooksC::Record_DefComment] =
      &HooksBaseC::readRecHook_DefComment;
   m_writeRecHookMethods[HooksC::Record_DefComment] =
      &HooksBaseC::writeRecHook_DefComment;

   m_readRecHookMethods[HooksC::Record_DefCreator] =
      &HooksBaseC::readRecHook_DefCreator;
   m_writeRecHookMethods[HooksC::Record_DefCreator] =
      &HooksBaseC::writeRecHook_DefCreator;

   m_readRecHookMethods[HooksC::Record_DefTimerResolution] =
      &HooksBaseC::readRecHook_DefTimerResolution;
   m_writeRecHookMethods[HooksC::Record_DefTimerResolution] =
      &HooksBaseC::writeRecHook_DefTimerResolution;

   m_readRecHookMethods[HooksC::Record_DefTimeRange] =
      &HooksBaseC::readRecHook_DefTimeRange;
   m_writeRecHookMethods[HooksC::Record_DefTimeRange] =
      &HooksBaseC::writeRecHook_DefTimeRange;

   m_readRecHookMethods[HooksC::Record_DefProcessGroup] =
      &HooksBaseC::readRecHook_DefProcessGroup;
   m_writeRecHookMethods[HooksC::Record_DefProcessGroup] =
      &HooksBaseC::writeRecHook_DefProcessGroup;

   m_readRecHookMethods[HooksC::Record_DefProcessGroupAttributes] =
      &HooksBaseC::readRecHook_DefProcessGroupAttributes;
   m_writeRecHookMethods[HooksC::Record_DefProcessGroupAttributes] =
      &HooksBaseC::writeRecHook_DefProcessGroupAttributes;

   m_readRecHookMethods[HooksC::Record_DefProcess] =
      &HooksBaseC::readRecHook_DefProcess;
   m_writeRecHookMethods[HooksC::Record_DefProcess] =
      &HooksBaseC::writeRecHook_DefProcess;

   m_readRecHookMethods[HooksC::Record_DefSclFile] =
      &HooksBaseC::readRecHook_DefSclFile;
   m_writeRecHookMethods[HooksC::Record_DefSclFile] =
      &HooksBaseC::writeRecHook_DefSclFile;

   m_readRecHookMethods[HooksC::Record_DefScl] =
      &HooksBaseC::readRecHook_DefScl;
   m_writeRecHookMethods[HooksC::Record_DefScl] =
      &HooksBaseC::writeRecHook_DefScl;

   m_readRecHookMethods[HooksC::Record_DefFileGroup] =
      &HooksBaseC::readRecHook_DefFileGroup;
   m_writeRecHookMethods[HooksC::Record_DefFileGroup] =
      &HooksBaseC::writeRecHook_DefFileGroup;

   m_readRecHookMethods[HooksC::Record_DefFile] =
      &HooksBaseC::readRecHook_DefFile;
   m_writeRecHookMethods[HooksC::Record_DefFile] =
      &HooksBaseC::writeRecHook_DefFile;

   m_readRecHookMethods[HooksC::Record_DefFunctionGroup] =
      &HooksBaseC::readRecHook_DefFunctionGroup;
   m_writeRecHookMethods[HooksC::Record_DefFunctionGroup] =
      &HooksBaseC::writeRecHook_DefFunctionGroup;

   m_readRecHookMethods[HooksC::Record_DefFunction] =
      &HooksBaseC::readRecHook_DefFunction;
   m_writeRecHookMethods[HooksC::Record_DefFunction] =
      &HooksBaseC::writeRecHook_DefFunction;

   m_readRecHookMethods[HooksC::Record_DefCollOp] =
      &HooksBaseC::readRecHook_DefCollOp;
   m_writeRecHookMethods[HooksC::Record_DefCollOp] =
      &HooksBaseC::writeRecHook_DefCollOp;

   m_readRecHookMethods[HooksC::Record_DefCounterGroup] =
      &HooksBaseC::readRecHook_DefCounterGroup;
   m_writeRecHookMethods[HooksC::Record_DefCounterGroup] =
      &HooksBaseC::writeRecHook_DefCounterGroup;

   m_readRecHookMethods[HooksC::Record_DefCounter] =
      &HooksBaseC::readRecHook_DefCounter;
   m_writeRecHookMethods[HooksC::Record_DefCounter] =
      &HooksBaseC::writeRecHook_DefCounter;

   m_readRecHookMethods[HooksC::Record_DefCounterAssignments] =
      &HooksBaseC::readRecHook_DefCounterAssignments;
   m_writeRecHookMethods[HooksC::Record_DefCounterAssignments] =
      &HooksBaseC::writeRecHook_DefCounterAssignments;

   m_readRecHookMethods[HooksC::Record_DefKeyValue] =
      &HooksBaseC::readRecHook_DefKeyValue;
   m_writeRecHookMethods[HooksC::Record_DefKeyValue] =
      &HooksBaseC::writeRecHook_DefKeyValue;

   // summary records

   m_readRecHookMethods[HooksC::Record_FunctionSummary] =
      &HooksBaseC::readRecHook_FunctionSummary;
   m_writeRecHookMethods[HooksC::Record_FunctionSummary] =
      &HooksBaseC::writeRecHook_FunctionSummary;

   m_readRecHookMethods[HooksC::Record_MessageSummary] =
      &HooksBaseC::readRecHook_MessageSummary;
   m_writeRecHookMethods[HooksC::Record_MessageSummary] =
      &HooksBaseC::writeRecHook_MessageSummary;

   m_readRecHookMethods[HooksC::Record_CollOpSummary] =
      &HooksBaseC::readRecHook_CollOpSummary;
   m_writeRecHookMethods[HooksC::Record_CollOpSummary] =
      &HooksBaseC::writeRecHook_CollOpSummary;

   m_readRecHookMethods[HooksC::Record_FileOpSummary] =
      &HooksBaseC::readRecHook_FileOpSummary;
   m_writeRecHookMethods[HooksC::Record_FileOpSummary] =
      &HooksBaseC::writeRecHook_FileOpSummary;

   // marker records

   m_readRecHookMethods[HooksC::Record_DefMarker] =
      &HooksBaseC::readRecHook_DefMarker;
   m_writeRecHookMethods[HooksC::Record_DefMarker] =
      &HooksBaseC::writeRecHook_DefMarker;

   m_readRecHookMethods[HooksC::Record_MarkerSpot] =
      &HooksBaseC::readRecHook_MarkerSpot;
   m_writeRecHookMethods[HooksC::Record_MarkerSpot] =
      &HooksBaseC::writeRecHook_MarkerSpot;

   // event records

   m_readRecHookMethods[HooksC::Record_Enter] =
      &HooksBaseC::readRecHook_Enter;
   m_writeRecHookMethods[HooksC::Record_Enter] =
      &HooksBaseC::writeRecHook_Enter;

   m_readRecHookMethods[HooksC::Record_Leave] =
      &HooksBaseC::readRecHook_Leave;
   m_writeRecHookMethods[HooksC::Record_Leave] =
      &HooksBaseC::writeRecHook_Leave;

   m_readRecHookMethods[HooksC::Record_BeginFileOp] =
      &HooksBaseC::readRecHook_BeginFileOp;
   m_writeRecHookMethods[HooksC::Record_BeginFileOp] =
      &HooksBaseC::writeRecHook_BeginFileOp;

   m_readRecHookMethods[HooksC::Record_EndFileOp] =
      &HooksBaseC::readRecHook_EndFileOp;
   m_writeRecHookMethods[HooksC::Record_EndFileOp] =
      &HooksBaseC::writeRecHook_EndFileOp;

   m_readRecHookMethods[HooksC::Record_SendMsg] =
      &HooksBaseC::readRecHook_SendMsg;
   m_writeRecHookMethods[HooksC::Record_SendMsg] =
      &HooksBaseC::writeRecHook_SendMsg;

   m_readRecHookMethods[HooksC::Record_RecvMsg] =
      &HooksBaseC::readRecHook_RecvMsg;
   m_writeRecHookMethods[HooksC::Record_RecvMsg] =
      &HooksBaseC::writeRecHook_RecvMsg;

   m_readRecHookMethods[HooksC::Record_BeginCollOp] =
      &HooksBaseC::readRecHook_BeginCollOp;
   m_writeRecHookMethods[HooksC::Record_BeginCollOp] =
      &HooksBaseC::writeRecHook_BeginCollOp;

   m_readRecHookMethods[HooksC::Record_EndCollOp] =
      &HooksBaseC::readRecHook_EndCollOp;
   m_writeRecHookMethods[HooksC::Record_EndCollOp] =
      &HooksBaseC::writeRecHook_EndCollOp;

   m_readRecHookMethods[HooksC::Record_RMAPut] =
      &HooksBaseC::readRecHook_RMAPut;
   m_writeRecHookMethods[HooksC::Record_RMAPut] =
      &HooksBaseC::writeRecHook_RMAPut;

   m_readRecHookMethods[HooksC::Record_RMAPutRemoteEnd] =
      &HooksBaseC::readRecHook_RMAPutRemoteEnd;
   m_writeRecHookMethods[HooksC::Record_RMAPutRemoteEnd] =
      &HooksBaseC::writeRecHook_RMAPutRemoteEnd;

   m_readRecHookMethods[HooksC::Record_RMAGet] =
      &HooksBaseC::readRecHook_RMAGet;
   m_writeRecHookMethods[HooksC::Record_RMAGet] =
      &HooksBaseC::writeRecHook_RMAGet;

   m_readRecHookMethods[HooksC::Record_RMAEnd] =
      &HooksBaseC::readRecHook_RMAEnd;
   m_writeRecHookMethods[HooksC::Record_RMAEnd] =
      &HooksBaseC::writeRecHook_RMAEnd;

   m_readRecHookMethods[HooksC::Record_Counter] =
      &HooksBaseC::readRecHook_Counter;
   m_writeRecHookMethods[HooksC::Record_Counter] =
      &HooksBaseC::writeRecHook_Counter;

   m_readRecHookMethods[HooksC::Record_EventComment] =
      &HooksBaseC::readRecHook_EventComment;
   m_writeRecHookMethods[HooksC::Record_EventComment] =
      &HooksBaseC::writeRecHook_EventComment;

   // snapshot records

   m_writeRecHookMethods[HooksC::Record_EnterSnapshot] =
      &HooksBaseC::writeRecHook_EnterSnapshot;
   m_writeRecHookMethods[HooksC::Record_SendSnapshot] =
      &HooksBaseC::writeRecHook_SendSnapshot;
   m_writeRecHookMethods[HooksC::Record_OpenFileSnapshot] =
      &HooksBaseC::writeRecHook_OpenFileSnapshot;
   m_writeRecHookMethods[HooksC::Record_BeginFileOpSnapshot] =
      &HooksBaseC::writeRecHook_BeginFileOpSnapshot;
   m_writeRecHookMethods[HooksC::Record_BeginCollOpSnapshot] =
      &HooksBaseC::writeRecHook_BeginCollOpSnapshot;
   m_writeRecHookMethods[HooksC::Record_CollOpCountSnapshot] =
      &HooksBaseC::writeRecHook_CollOpCountSnapshot;
   m_writeRecHookMethods[HooksC::Record_CounterSnapshot] =
      &HooksBaseC::writeRecHook_CounterSnapshot;
}

HooksBaseC::~HooksBaseC()
{
   // Empty
}
