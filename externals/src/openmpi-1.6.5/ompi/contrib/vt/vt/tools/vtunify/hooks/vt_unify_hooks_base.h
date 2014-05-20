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

#ifndef _VT_UNIFY_HOOKS_BASE_H_
#define _VT_UNIFY_HOOKS_BASE_H_

#include "vt_unify_hooks.h"

//
// HooksBaseC class
//
class HooksBaseC
{
   friend class HooksC;

public:

   // constructor
   HooksBaseC();

   // destructor
   virtual ~HooksBaseC();

private:

   // trigger phase hook
   void triggerPhaseHook( const HooksC::PhaseTypeT & phase )
   {
      vt_assert( m_phaseMethods.size() > (uint32_t)phase );
      vt_assert( m_phaseMethods[phase] != 0 );

      ( this->*( m_phaseMethods[phase] ) )();
   }

   // trigger read record hook
   void triggerReadRecordHook( const HooksC::RecordTypeT & rectype,
                               HooksC::VaArgsT & args )
   {
      vt_assert( m_readRecHookMethods.size() > (uint32_t)rectype );
      vt_assert( m_readRecHookMethods[rectype] != 0 );

      ( this->*( m_readRecHookMethods[rectype] ) )( args );
   }

   // trigger write record hook
   void triggerWriteRecordHook( const HooksC::RecordTypeT & rectype,
                                HooksC::VaArgsT & args )
   {
      vt_assert( m_writeRecHookMethods.size() > (uint32_t)rectype );
      vt_assert( m_writeRecHookMethods[rectype] != 0 );

      ( this->*( m_writeRecHookMethods[rectype] ) )( args );
   }

   // vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

   // initialization/finalization hooks
   // (must be defined in inheriting hook classes)
   //

   virtual void initHook() = 0;
   virtual void finalizeHook( const bool & error ) = 0;

   // phase hooks
   //

   virtual void phaseHook_GetUnifyControls_pre() {}
   virtual void phaseHook_GetUnifyControls_post() {}

   virtual void phaseHook_UnifyDefinitions_pre() {}
   virtual void phaseHook_UnifyDefinitions_post() {}

   virtual void phaseHook_UnifyMarkers_pre() {}
   virtual void phaseHook_UnifyMarkers_post() {}

   virtual void phaseHook_UnifyStatistics_pre() {}
   virtual void phaseHook_UnifyStatistics_post() {}

   virtual void phaseHook_UnifyEvents_pre() {}
   virtual void phaseHook_UnifyEvents_post() {}

   virtual void phaseHook_WriteMasterControl_pre() {}
   virtual void phaseHook_WriteMasterControl_post() {}

   virtual void phaseHook_CleanUp_pre() {}
   virtual void phaseHook_CleanUp_post() {}

   // record hooks
   //

   // definition records

   virtual void readRecHook_DefComment( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefComment( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefCreator( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefCreator( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefTimerResolution( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefTimerResolution( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefTimeRange( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefTimeRange( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefProcessGroup( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefProcessGroup( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefProcessGroupAttributes( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefProcessGroupAttributes( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefProcess( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefProcess( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefSclFile( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefSclFile( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefScl( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefScl( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefFileGroup( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefFileGroup( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefFile( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefFile( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefFunctionGroup( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefFunctionGroup( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefFunction( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefFunction( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefCollOp( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefCollOp( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefCounterGroup( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefCounterGroup( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefCounter( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefCounter( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefCounterAssignments( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefCounterAssignments( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_DefKeyValue( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefKeyValue( HooksC::VaArgsT & args ) { (void)args; }

   // summary records

   virtual void readRecHook_FunctionSummary( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_FunctionSummary( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_MessageSummary( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_MessageSummary( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_CollOpSummary( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_CollOpSummary( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_FileOpSummary( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_FileOpSummary( HooksC::VaArgsT & args ) { (void)args; }

   // marker records

   virtual void readRecHook_DefMarker( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_DefMarker( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_MarkerSpot( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_MarkerSpot( HooksC::VaArgsT & args ) { (void)args; }

   // event records

   virtual void readRecHook_Enter( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_Enter( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_Leave( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_Leave( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_BeginFileOp( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_BeginFileOp( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_EndFileOp( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_EndFileOp( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_SendMsg( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_SendMsg( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_RecvMsg( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_RecvMsg( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_BeginCollOp( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_BeginCollOp( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_EndCollOp( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_EndCollOp( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_RMAPut( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_RMAPut( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_RMAGet( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_RMAGet( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_RMAEnd( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_RMAEnd( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_Counter( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_Counter( HooksC::VaArgsT & args ) { (void)args; }

   virtual void readRecHook_EventComment( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_EventComment( HooksC::VaArgsT & args ) { (void)args; }

   // snapshot records
   //

   virtual void writeRecHook_EnterSnapshot( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_SendSnapshot( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_OpenFileSnapshot( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_BeginFileOpSnapshot( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_BeginCollOpSnapshot( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_CollOpCountSnapshot( HooksC::VaArgsT & args ) { (void)args; }
   virtual void writeRecHook_CounterSnapshot( HooksC::VaArgsT & args ) { (void)args; }

   // generic hook
   virtual void genericHook( const uint32_t & id, HooksC::VaArgsT & args ) { (void)args; }

   // ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

   // vector of phase hook methods
   std::vector<void ( HooksBaseC::* )( void )> m_phaseMethods;

   // vector of record read hook methods
   std::vector<void ( HooksBaseC::* )( HooksC::VaArgsT& )> m_readRecHookMethods;

   // vector of record write hook methods
   std::vector<void ( HooksBaseC::* )( HooksC::VaArgsT& )> m_writeRecHookMethods;

};

#endif // _VT_UNIFY_HOOKS_BASE_H_
