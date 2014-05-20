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

#include "vt_unify_hooks_margins.h"

//////////////////// class HooksProcessMarginsC ////////////////////

// public methods
//

HooksProcessMarginsC::HooksProcessMarginsC() : HooksBaseC(),
   m_maxThreads( 1 ), m_threadContexts( 0 )
{
   // Empty
}

HooksProcessMarginsC::~HooksProcessMarginsC()
{
   // Empty
}

// private methods
//

// vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

// initialization/finalization hooks
//

void
HooksProcessMarginsC::initHook()
{
   // Empty
}

void
HooksProcessMarginsC::finalizeHook( const bool & error )
{
   // Empty
}

// phase hooks
//

void
HooksProcessMarginsC::phaseHook_UnifyEvents_pre()
{
#if defined(HAVE_OMP) && HAVE_OMP
   // update maximum number of threads to use for unifying events
   m_maxThreads = omp_get_max_threads();
#endif // HAVE_OMP

   // create array of thread contexts
   //
   m_threadContexts = new ThreadContextS[m_maxThreads];
   vt_assert( m_threadContexts );
}

void
HooksProcessMarginsC::phaseHook_UnifyEvents_post()
{
   // delete array of thread contexts
   delete [] m_threadContexts;
}

// record hooks
//

void
HooksProcessMarginsC::writeRecHook_Event( OTF_WStream ** wstream,
   uint64_t * time, uint32_t * streamid, bool * dowrite )
{
   bool error = false;

#if defined(HAVE_OMP) && HAVE_OMP
   ThreadContextS & context = m_threadContexts[omp_get_thread_num()];
#else // HAVE_OMP
   ThreadContextS & context = m_threadContexts[0];
#endif // HAVE_OMP

   if( *dowrite )
   {
      // update last written timestamp
      context.last_time = *time;

      // first event record to write?
      if( context.first_event )
      {
         context.first_event = false;

         // write begin process record
         error = ( OTF_WStream_writeBeginProcess( *wstream, *time,
                      *streamid ) == 0 );
      }
   }

   //return !error;
   vt_assert( !error );
}

// generic hook
void
HooksProcessMarginsC::genericHook( const uint32_t & id, HooksC::VaArgsT & args )
{
   bool error = false;

   if( ( id & VT_UNIFY_HOOKS_MARGINS_GENID__EVENT_WSTREAM_OPEN ) != 0 )
   {
#if defined(HAVE_OMP) && HAVE_OMP
      ThreadContextS & context = m_threadContexts[omp_get_thread_num()];
#else // HAVE_OMP
      ThreadContextS & context = m_threadContexts[0];
#endif // HAVE_OMP

      // get hook arguments
      //
      OTF_WStream ** wstream = (OTF_WStream**)args[0];
      uint32_t * stream_id = (uint32_t*)args[1];

      // [re-]initialize thread context
      //
      context.wstream = *wstream;
      context.streamid = *stream_id;
      context.first_event = true;
      context.last_time = 0;
   }
   else if( ( id & VT_UNIFY_HOOKS_MARGINS_GENID__EVENT_WSTREAM_CLOSE ) != 0 )
   {
#if defined(HAVE_OMP) && HAVE_OMP
      ThreadContextS & context = m_threadContexts[omp_get_thread_num()];
#else // HAVE_OMP
      ThreadContextS & context = m_threadContexts[0];
#endif // HAVE_OMP

      // get hook arguments
      //OTF_WStream ** wstream = (OTF_WStream**)args[0];
      uint32_t * stream_id = (uint32_t*)args[1];

      vt_assert( context.streamid == *stream_id );

      // write process end record
      error = ( OTF_WStream_writeEndProcess( context.wstream, context.last_time,
                   context.streamid ) == 0 );
   }

   //return !error;
   vt_assert( !error );
}

// ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^
