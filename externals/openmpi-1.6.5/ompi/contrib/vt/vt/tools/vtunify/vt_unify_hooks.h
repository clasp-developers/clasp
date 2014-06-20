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

#ifndef _VT_UNIFY_HOOKS_H_
#define _VT_UNIFY_HOOKS_H_

#include "vt_unify.h"

// generic hooks' identifier bits
//
enum
{
   // HooksRawC's (example; not used)
   //
   VT_UNIFY_HOOKS_RAW_GENID__SOMETHING1                                 = 1<<0,
   VT_UNIFY_HOOKS_RAW_GENID__SOMETHING2                                 = 1<<1,

   // HooksAsyncEventsC's
   //
   VT_UNIFY_HOOKS_AEVENTS_GENID__EVENT_WSTREAM_OPEN                     = 1<<2,
   VT_UNIFY_HOOKS_AEVENTS_GENID__EVENT_WSTREAM_CLOSE                    = 1<<3,

   // HooksTdbC's
   //
   VT_UNIFY_HOOKS_TDB_GENID__STARTSTOPTIME_EPOCH                        = 1<<4,

   // HooksProcessMarginsC's
   //
   VT_UNIFY_HOOKS_MARGINS_GENID__EVENT_WSTREAM_OPEN                     = 1<<5,
   VT_UNIFY_HOOKS_MARGINS_GENID__EVENT_WSTREAM_CLOSE                    = 1<<6,

   // HooksMsgMatchAndSnapsC's
   //
   VT_UNIFY_HOOKS_MSGMATCH_SNAPS_GENID__DEF_WSTREAM_CLOSE               = 1<<7,
   VT_UNIFY_HOOKS_MSGMATCH_SNAPS_GENID__EVENT_WSTREAM_OPEN              = 1<<8,
   VT_UNIFY_HOOKS_MSGMATCH_SNAPS_GENID__EVENT_WSTREAM_CLOSE             = 1<<9

};

//
// HooksC class
//

// forward declaration
class HooksBaseC;

class HooksC
{
public:

   // maximum number of variable arguments for record and generic hooks
   static const uint32_t MAX_VA_ARGS = 14;

   //
   // variable hook arguments type
   //
   typedef void *VaArgsT[MAX_VA_ARGS];

   //
   // phase hooks
   //
   typedef enum
   {
      Phase_GetUnifyControls_pre, Phase_GetUnifyControls_post,
      Phase_UnifyDefinitions_pre, Phase_UnifyDefinitions_post,
      Phase_UnifyMarkers_pre, Phase_UnifyMarkers_post,
      Phase_UnifyStatistics_pre, Phase_UnifyStatistics_post,
      Phase_UnifyEvents_pre, Phase_UnifyEvents_post,
      Phase_WriteMasterControl_pre, Phase_WriteMasterControl_post,
      Phase_CleanUp_pre, Phase_CleanUp_post,
      Phase_Num
   } PhaseTypeT;

   //
   // record hooks
   //
   typedef enum
   {
      // definition records
      //
      Record_DefComment,
      Record_DefCreator,
      Record_DefTimerResolution,
      Record_DefTimeRange,
      Record_DefProcessGroup,
      Record_DefProcessGroupAttributes,
      Record_DefProcess,
      Record_DefSclFile,
      Record_DefScl,
      Record_DefFileGroup,
      Record_DefFile,
      Record_DefFunctionGroup,
      Record_DefFunction,
      Record_DefCollOp,
      Record_DefCounterGroup,
      Record_DefCounter,
      Record_DefCounterAssignments,
      Record_DefKeyValue,

      // summary records
      //
      Record_FunctionSummary,
      Record_MessageSummary,
      Record_CollOpSummary,
      Record_FileOpSummary,

      // marker records
      //
      Record_DefMarker,
      Record_MarkerSpot,

      // event records
      //
      Record_Enter,
      Record_Leave,
      Record_BeginFileOp,
      Record_EndFileOp,
      Record_SendMsg,
      Record_RecvMsg,
      Record_BeginCollOp,
      Record_EndCollOp,
      Record_RMAPut,
      Record_RMAPutRemoteEnd,
      Record_RMAGet,
      Record_RMAEnd,
      Record_Counter,
      Record_EventComment,

      // snapshot records
      //
      Record_EnterSnapshot,
      Record_SendSnapshot,
      Record_OpenFileSnapshot,
      Record_BeginFileOpSnapshot,
      Record_BeginCollOpSnapshot,
      Record_CollOpCountSnapshot,
      Record_CounterSnapshot,

      // number of records
      Record_Num
   } RecordTypeT;

   // constructor
   HooksC();

   // destructor
   ~HooksC();

   // register hook classes
   void registerHooks();

   // trigger init hook
   void triggerInitHook();

   // trigger finalize hook
   void triggerFinalizeHook( const bool & error );

   // trigger phase hook
   void triggerPhaseHook( const PhaseTypeT & phase );

   // trigger read record hook
   void triggerReadRecordHook( const RecordTypeT & rectype, const uint32_t & n,
                               void * a0 = 0, void * a1 = 0, void * a2 = 0,
                               void * a3 = 0, void * a4 = 0, void * a5 = 0,
                               void * a6 = 0, void * a7 = 0, void * a8 = 0,
                               void * a9 = 0, void * a10 = 0, void * a11 = 0,
                               void * a12 = 0, void * a13 = 0 );

   // trigger write record hook
   void triggerWriteRecordHook( const RecordTypeT & rectype, const uint32_t & n,
                                void * a0 = 0, void * a1 = 0, void * a2 = 0,
                                void * a3 = 0, void * a4 = 0, void * a5 = 0,
                                void * a6 = 0, void * a7 = 0, void * a8 = 0,
                                void * a9 = 0, void * a10 = 0, void * a11 = 0,
                                void * a12 = 0, void * a13 = 0 );

   // trigger generic hook
   void triggerGenericHook( const uint32_t & id, const uint32_t & n,
                            void * a0 = 0, void * a1 = 0, void * a2 = 0,
                            void * a3 = 0, void * a4 = 0, void * a5 = 0,
                            void * a6 = 0, void * a7 = 0, void * a8 = 0,
                            void * a9 = 0, void * a10 = 0, void * a11 = 0,
                            void * a12 = 0, void * a13 = 0 );

private:

  // vector of hook class instances
  std::vector<HooksBaseC*> m_hooks;

};

// instance of class Hooks
extern HooksC * theHooks;

#endif // _VT_UNIFY_HOOKS_H_
