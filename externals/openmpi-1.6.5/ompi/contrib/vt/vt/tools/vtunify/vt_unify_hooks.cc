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

#include "vt_unify_hooks.h"

#include "hooks/vt_unify_hooks_base.h"

#ifdef VT_UNIFY_HOOKS_RAW
#  include "hooks/vt_unify_hooks_raw.h"
#endif // VT_UNIFY_HOOKS_RAW
#ifdef VT_UNIFY_HOOKS_AEVENTS
#  include "hooks/vt_unify_hooks_aevents.h"
#endif // VT_UNIFY_HOOKS_AEVENTS
#ifdef VT_UNIFY_HOOKS_MARGINS
#  include "hooks/vt_unify_hooks_margins.h"
#endif // VT_UNIFY_HOOKS_MARGINS
#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
#  include "hooks/vt_unify_hooks_msgmatch_snaps.h"
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS
#ifdef VT_UNIFY_HOOKS_PROF
#  include "hooks/vt_unify_hooks_prof.h"
#endif // VT_UNIFY_HOOKS_PROF
#ifdef VT_UNIFY_HOOKS_TDB
#  include "hooks/vt_unify_hooks_tdb.h"
#endif // VT_UNIFY_HOOKS_TDB


HooksC * theHooks = 0; // instance of class HooksC

//////////////////// class HooksC ////////////////////

// public methods
//

HooksC::HooksC()
{
   // Empty
}

HooksC::~HooksC()
{
   // destruct registered hook classes
   //
   for( uint32_t i = 0; i < m_hooks.size(); i++ )
      delete m_hooks[i];
}

void
HooksC::registerHooks()
{
#ifdef VT_UNIFY_HOOKS_RAW
   if( HooksRawC::isEnabled() )
      m_hooks.push_back( new HooksRawC() );
#endif // VT_UNIFY_HOOKS_RAW

#ifdef VT_UNIFY_HOOKS_AEVENTS
   if( HooksAsyncEventsC::isEnabled() )
      m_hooks.push_back( new HooksAsyncEventsC() );
#endif // VT_UNIFY_HOOKS_AEVENTS

#ifdef VT_UNIFY_HOOKS_MSGMATCH_SNAPS
   if( HooksMsgMatchAndSnapsC::isEnabled() )
      m_hooks.push_back( new HooksMsgMatchAndSnapsC() );
#endif // VT_UNIFY_HOOKS_MSGMATCH_SNAPS

#ifdef VT_UNIFY_HOOKS_PROF
   if( HooksProfC::isEnabled() )
      m_hooks.push_back( new HooksProfC() );
#endif // VT_UNIFY_HOOKS_PROF

#ifdef VT_UNIFY_HOOKS_TDB
   if( HooksTdbC::isEnabled() )
      m_hooks.push_back( new HooksTdbC() );
#endif // VT_UNIFY_HOOKS_TDB

#ifdef VT_UNIFY_HOOKS_MARGINS
   if( HooksProcessMarginsC::isEnabled() )
      m_hooks.push_back( new HooksProcessMarginsC() );
#endif // VT_UNIFY_HOOKS_MARGINS
}

void
HooksC::triggerInitHook()
{
   if( m_hooks.empty() ) return;

   // forward this hook to all registered hook classes
   //
   for( uint32_t i = 0; i < m_hooks.size(); i++ )
      m_hooks[i]->initHook();
}

void
HooksC::triggerFinalizeHook( const bool & error )
{
   if( m_hooks.empty() ) return;

   // forward this hook to all registered hook classes
   //
   for( uint32_t i = 0; i < m_hooks.size(); i++ )
      m_hooks[i]->finalizeHook( error );
}

void
HooksC::triggerPhaseHook( const PhaseTypeT & phase )
{
   if( m_hooks.empty() ) return;

   // forward this hook to all registered hook classes
   //
   for( uint32_t i = 0; i < m_hooks.size(); i++ )
      m_hooks[i]->triggerPhaseHook( phase );
}

void
HooksC::triggerReadRecordHook( const RecordTypeT & rectype, const uint32_t & n,
                               void * a0, void * a1, void * a2, void * a3,
                               void * a4, void * a5, void * a6, void * a7,
                               void * a8, void * a9, void * a10, void * a11,
                               void * a12, void * a13 )
{
   if( m_hooks.empty() ) return;

   // put arguments into an array
   VaArgsT args =
      { a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13 };

   // forward this hook to all registered hook classes
   //
   for( uint32_t i = 0; i < m_hooks.size(); i++ )
      m_hooks[i]->triggerReadRecordHook( rectype, args );
}

void
HooksC::triggerWriteRecordHook( const RecordTypeT & rectype, const uint32_t & n,
                                void * a0, void * a1, void * a2, void * a3,
                                void * a4, void * a5, void * a6, void * a7,
                                void * a8, void * a9, void * a10, void * a11,
                                void * a12, void * a13 )
{
   if( m_hooks.empty() ) return;

   // put arguments into an array
   VaArgsT args =
      { a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13 };

   // forward this hook to all registered hook classes
   //
   for( uint32_t i = 0; i < m_hooks.size(); i++ )
      m_hooks[i]->triggerWriteRecordHook( rectype, args );
}

void
HooksC::triggerGenericHook( const uint32_t & id, const uint32_t & n,
                            void * a0, void * a1, void * a2, void * a3,
                            void * a4, void * a5, void * a6, void * a7,
                            void * a8, void * a9, void * a10, void * a11,
                            void * a12, void * a13 )
{
   if( m_hooks.empty() ) return;

   // put arguments into an array
   VaArgsT args =
      { a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13 };

   // forward this hook to all registered hook classes
   //
   for( uint32_t i = 0; i < m_hooks.size(); i++ )
      m_hooks[i]->genericHook( id, args );
}
