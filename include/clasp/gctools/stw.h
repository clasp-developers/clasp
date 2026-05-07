#pragma once

// GC-independent stop-the-world mechanism.
//
// A thread can be two levels of "stopped". First, it may be in a GC-safe state.
// In this case it is actually allowed to continue running, with restrictions.
// A thread in a GC-safe state has no live GC references
// above its park frame that the collector doesn't already know about, and does
// not touch (reading or writing) GC-managed memory. This is because the GC may be
// doing arbitrary things to GC objects (e.g. copying them and updating pointers),
// so any infringement here will probably result in memory corruption.
// In threadlocal.h this is GCState::GCless.
//
// Secondly, a thread may be doing some blocking call, like nanosleep(2). In this
// case it "parks" through begin_park()/end_park() (BEGIN_PARK/END_PARK macros).
// This is a superset of being in a GC-safe state. The difference is that
// additionally, parked threads may be interrupted by signals. If a thread is
// blockingp, signal handlers will try to handle the signal immediately and
// wake the thread from its blocking sleep. But if the world has been stopped by
// GC, the thread will wait for that before running any Lisp signal handlers.
// In threadlocal.h this is GCState::Parked.
//
// clasp_stop_the_world() blocks until every registered mutator thread is in a
// GC-safe state.  The caller is NOT assumed to be a registered mutator (MMTk
// GC worker threads are not).  Registered mutator callers must remove
// themselves via stw_mutator_stop() before calling and stw_mutator_resume()
// after clasp_resume_the_world().
//
// end_park() checks whether the world is currently stopped and if so blocks
// until clasp_resume_the_world() is called, so threads leaving a GC-safe
// region do not race ahead of a running collector.

namespace gctools {

// Called from ThreadLocalState constructor: register this thread as a running
// mutator so clasp_stop_the_world() knows to wait for it.
void stw_register_thread();

// Called from ThreadLocalState destructor: remove this thread.  If the thread
// is currently parked the count was already decremented by begin_park_internal
// and this is a no-op.
void stw_unregister_thread();

// Enter GC-safe state. Sets thread's GCState and does begin_gcless_shared().
void begin_gcless();

// Enter GC-safe state without altering thread's GCState. Called by begin_park().
void begin_gcless_shared();

// For use by call_with_stopped_world() in GC-specific code: remove/re-add the
// calling registered mutator from the running count around a stop/resume pair.
// Unlike begin_gcless_shared(), stw_mutator_resume() does NOT wait for
// world_stopped to clear (the caller is the one who cleared it).
void stw_mutator_stop();
void stw_mutator_resume();

// Leave GC-safe state. Sets thread's GCState and does end_gcless_shared().
void end_gcless();

// Leave GC-safe state.  If the world is currently stopped, blocks until
// clasp_resume_the_world() is called. Called by end_park().
void end_gcless_shared();

// Check if the GC has asked the world to stop, and stop if so.
// This is the slow path called from gc_yield below.
void gc_yield_slow();

extern std::atomic<bool> world_stopped;

// Check if the GC has stop the world to stop, and stop if so.
// This is called extremely frequently at safepoints, like the top of bytecode
// functions, so ideally it will be fast.
inline void gc_yield() {
  if (world_stopped.load(std::memory_order_relaxed))
    gc_yield_slow();
}

} // namespace gctools

// Extern "C" so the Rust MMTk binding and plain C code can call these.
extern "C" {

// Block until all registered mutator threads have entered a GC-safe state.
// The caller need not be (and in the MMTk case, is not) a registered mutator.
void clasp_stop_the_world();

// Resume all threads stopped by clasp_stop_the_world().
void clasp_resume_the_world();

// Pause the calling mutator and wait until the world is resumed.
// Used by MMTk's block_for_gc callback.
void clasp_pause_thread_for_gc();

} // extern "C"
