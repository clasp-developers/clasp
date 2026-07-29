#pragma once

#include <concepts>
#include <type_traits>

// GC-independent stop-the-world mechanism.
//
// A thread can be two levels of "stopped". First, it may be in a GC-safe state.
// In this case it is actually allowed to continue running, with restrictions.
// A thread in a GC-safe state has no live GC references
// above its park frame that the collector doesn't already know about, and does
// not touch (reading or writing) GC-managed memory. This is because the GC may be
// doing arbitrary things to GC objects (e.g. copying them and updating pointers),
// so any infringement here will probably result in memory corruption.
// In threadlocal.h this is GCState::GCsafe.
//
// Secondly, a thread may be doing some blocking call, like nanosleep(2). In this
// case it "parks" through begin_park()/end_park() (BEGIN_PARK/END_PARK macros).
// This is also a GC-safe state with those restrictions. The difference is that
// additionally, parked threads may be interrupted by signals. If a thread is
// blockingp, signal handlers will try to handle the signal immediately and
// wake the thread from its blocking sleep. But if the world has been stopped by
// GC, the thread will wait for that before running any Lisp signal handlers.
// In threadlocal.h this is GCState::Parked.
//
// clasp_stop_the_world() blocks until every registered mutator thread is in a
// GC-safe state.  The caller is NOT assumed to be a registered mutator (MMTk
// GC worker threads are not). Registered mutator callers must remove
// themselves via begin_gcsafe() before calling and end_gcsafe()
// after clasp_resume_the_world().
//
// end_park() checks whether the world is currently stopped and if so blocks
// until clasp_resume_the_world() is called, so threads leaving a GC-safe
// region do not race ahead of a running collector.

// Unfortunately necessary forward declaration.
namespace core {
class ThreadLocalState;
}

namespace gctools {

// Called from ThreadLocalState constructor: register this thread as a running
// mutator so clasp_stop_the_world() knows to wait for it.
void stw_register_thread(core::ThreadLocalState*);

// Called from ThreadLocalState destructor: remove this thread.  If the thread
// is currently parked the count was already decremented by begin_park_internal
// and this is a no-op.
void stw_unregister_thread(core::ThreadLocalState*);

// Enter GC-safe state.
// Sets thread's GCState and does begin_gcsafe_shared().
// The TLS must be my_thread, but it is provided as an argument so that my_thread
// only needs to be accessed once.
// The second argument is a stack pointer which must be prior to the current stack
// pointer. Nothing more recent than this pointer is scannable so it must not
// refer to live objects.
void begin_gcsafe(core::ThreadLocalState*, void*);

// Enter GC-safe state without altering thread's GCState. Called by begin_park().
void begin_gcsafe_shared(core::ThreadLocalState*, void*);

// Leave GC-safe state. Sets thread's GCState and does end_gcsafe_shared().
void end_gcsafe(core::ThreadLocalState*);

// Leave GC-safe state.  If the world is currently stopped, blocks until
// clasp_resume_the_world() is called. Called by end_park() and ~ThreadLocalState.
// Returns the stack pointer that was passed to begin_gcsafe_shared, which is used
// for temporary unparking (call_unparked).
void* end_gcsafe_shared(core::ThreadLocalState*);

// Check if the GC has asked the world to stop, and stop if so.
// This is the slow path called from gc_yield below.
void gc_yield_slow();

extern std::atomic<bool> world_stopped;

// Check if the GC has asked the world to stop, and stop if so.
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

namespace gctools {
// Do some stuff with the world stopped, from a mutator.
template <std::invocable<> F>
requires (!std::same_as<void, std::invoke_result_t<F>>)
decltype(auto) call_with_stopped_world(F f) {
  core::ThreadLocalState* me = my_thread;
  begin_gcsafe(me, __builtin_frame_address(0));
  clasp_stop_the_world();
  decltype(auto) result = f();
  clasp_resume_the_world();
  end_gcsafe(me);
  return result;
}
// special version for F returning void since "void result;" doesn't work.
template <std::invocable<> F>
requires std::same_as<void, std::invoke_result_t<F>>
void call_with_stopped_world(F f) {
  core::ThreadLocalState* me = my_thread;
  begin_gcsafe(me, __builtin_frame_address(0));
  clasp_stop_the_world();
  f();
  clasp_resume_the_world();
  end_gcsafe(me);
}

// Do some stuff in a GC-safe but not parked state, from a mutator.
template <std::invocable<> F>
requires (!std::same_as<void, std::invoke_result_t<F>>)
decltype(auto) call_gcsafe(F f) {
  core::ThreadLocalState* me = my_thread;
  begin_gcsafe(me, __builtin_frame_address(0));
  decltype(auto) result = f();
  end_gcsafe(me);
  return result;
};
template <std::invocable<> F>
requires std::same_as<void, std::invoke_result_t<F>>
void call_gcsafe(F f) {
  core::ThreadLocalState* me = my_thread;
  begin_gcsafe(me, __builtin_frame_address(0));
  f();
  end_gcsafe(me);
}
}; // namespace gctools
