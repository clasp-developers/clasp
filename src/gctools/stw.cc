// GC-independent stop-the-world implementation.
// See include/clasp/gctools/stw.h for design notes.

#include <clasp/core/foundation.h>
#include <clasp/gctools/stw.h>
#include <clasp/gctools/memoryManagement.h> // my_thread, ThreadLocalState

#include <atomic>
#include <condition_variable>
#include <mutex>

namespace gctools {

// Number of mutator threads currently in a running (non-GC-safe) state.
// clasp_stop_the_world() waits for this to reach 0.
static std::atomic<size_t> running_count{0};

static std::mutex stw_mutex;
// Signaled when running_count reaches 0 (all mutators have parked).
static std::condition_variable all_parked_cv;
// Signaled when the world resumes after a stop.
static std::condition_variable world_resumed_cv;
// Signaled when world_stopped transitions to true (GC is beginning a stop).
// Lets clasp_pause_thread_for_gc wait until the GC is actually stopping before
// it parks, ensuring that pause_thread_for_gc does genuinely pause before
// returning. If it doesn't that violates the definition of MMTK's block_for_gc.
// Without world_stopping_cv I also observed a deadlock where the (single) Lisp
// thread was waiting on world_resumed_cv while the GC worker was waiting on
// all_parked_cv, but I'm not entirely sure I understand what happened there.
// An alternate design would be to wait on a counter that stop_the_world
// increments. OpenJDK does this, but it (technically) falls victim to ABA
// and I'd prefer a CV to a semaphore regardless.
static std::condition_variable world_stopping_cv;
std::atomic<bool> world_stopped{false};

// Yet another guard on stopping. This one is to make sure that GC doesn't run
// while a mutator thread has stopped the world for its own purposes (e.g. ROOM),
// and vice versa.
enum class StopType { Running, MutatorStop, GCStop };
std::atomic<StopType> stop_type{StopType::Running};

void gc_yield_slow_thread(core::ThreadLocalState*);

void stw_register_thread(core::ThreadLocalState* thread) {
  running_count.fetch_add(1, std::memory_order_acq_rel);
  // Now that we're registered, yield in case a GC is underway.
  if (world_stopped.load(std::memory_order_relaxed))
    gc_yield_slow_thread(thread);
}

void stw_unregister_thread(core::ThreadLocalState* thread) {
  (void)thread;
  int prev = running_count.fetch_sub(1, std::memory_order_acq_rel);
  if (prev == 1) {
    all_parked_cv.notify_all();
  }
}

void begin_gcsafe_shared(core::ThreadLocalState* thread, void* sp) {
  // Save the stack pointer before decrementing running_count.
  // The acq_rel fence on fetch_sub below ensures the GC thread sees this
  // store after observing running_count == 0.
  thread->_LowLevel._ControlStackPointer = sp;
  int prev = running_count.fetch_sub(1, std::memory_order_acq_rel);
  if (prev == 1) {
    // It is possible for this to go off during GC if thread is newly created
    // and about to wait within stw_register_thread. That should be harmless
    // to lose since nothing is waiting on it during GC.
    all_parked_cv.notify_all();
  }
}

void begin_gcsafe(core::ThreadLocalState* thread, void* sp) {
  thread->gcsafe();
  begin_gcsafe_shared(thread, sp);
}

static void wait_for_world_resumption() {
  std::unique_lock<std::mutex> lock(stw_mutex);
  world_resumed_cv.wait(lock, [] { return !world_stopped.load(std::memory_order_acquire); });

}

void* end_gcsafe_shared(core::ThreadLocalState* thread) {
  wait_for_world_resumption();
  running_count.fetch_add(1, std::memory_order_acq_rel);
  return thread->_LowLevel._ControlStackPointer;
}

void end_gcsafe(core::ThreadLocalState* thread) {
  end_gcsafe_shared(thread);
  thread->unblock();
}

void try_invoking_finalizers(); // defined in gcFunctions.cc

// see gc_yield
void gc_yield_slow_thread(core::ThreadLocalState* thread) {
  // Don't need to wait on world_stopping_cv, since in gc_yield we already
  // checked that world_stopped is true.
  begin_gcsafe(thread, __builtin_frame_address(0));
  end_gcsafe(thread);
  // Try some finalizers.
  // NOTE: This is maybe slower than it has to be right now - every thread will
  // come out of a GC pause simultaneously, and they'll all race to grab the lock
  // so they can run finalizers, and only one will win. Needless contention.
  // But maybe that doesn't matter if you're already GCing.
  try_invoking_finalizers();
}

void gc_yield_slow() { gc_yield_slow_thread(my_thread); }

// Block until all mutators have gotten out of a mutator stop
// (for call_with_stopped_world)
void wait_for_mutator_running() {
  StopType mystop;
  bool swapped;
  do {
    // Wait until the world is really running, in case another mutator is in ROOM
    // or the like. Alternately another thread could be racing with us here in
    // clasp_pause_thread_for_gc (I think? Not 100% clear on MMTk
    // synchronization) in which who cares.
    stop_type.wait(StopType::MutatorStop, std::memory_order_acquire);
    // Now try to plug GCStop into the variable.
    mystop = StopType::Running;
    swapped = stop_type.compare_exchange_weak(mystop, StopType::GCStop,
                                              std::memory_order_acq_rel);
    // A successful swap, or another thread beating us to it, are both
    // acceptable outcomes.
  } while (!swapped && mystop != StopType::GCStop);
  // Notify anyone else that's waiting on stop_type.
  stop_type.notify_all();
}

// Wait for any GC to finish so we can do call_with_stopped_world.
void wait_for_gc_finished() {
  StopType mystop;
  bool swapped;
  do {
    stop_type.wait(StopType::GCStop, std::memory_order_acquire);
    mystop = StopType::Running;
    swapped = stop_type.compare_exchange_weak(mystop, StopType::MutatorStop,
                                              std::memory_order_acq_rel);
  } while (!swapped && mystop != StopType::MutatorStop);
  stop_type.notify_all();
}

} // namespace gctools

extern "C" {

void clasp_stop_the_world() {
  // Signal that the world is stopping, then wait for all registered mutator
  // threads to reach a GC-safe state (running_count == 0).
  // The caller is NOT assumed to be a registered mutator: MMTk GC worker
  // threads call this but are never registered with stw_register_thread().
  // Callers that ARE registered mutators (e.g. Boehm's call_with_stopped_world)
  // must remove themselves from running_count before calling this,
  // using stw_mutator_stop.
  std::unique_lock<std::mutex> lock(gctools::stw_mutex);
  gctools::world_stopped.store(true, std::memory_order_release);
  gctools::world_stopping_cv.notify_all();
  gctools::all_parked_cv.wait(lock, [] {
    return gctools::running_count.load(std::memory_order_acquire) == 0;
  });
}

void clasp_resume_the_world() {
  std::unique_lock<std::mutex> lock(gctools::stw_mutex);
  gctools::world_stopped.store(false, std::memory_order_release);
  gctools::world_resumed_cv.notify_all();
  // This was set by call_with_stopped_world or pause_thread_for_gc.
  gctools::stop_type.store(gctools::StopType::Running, std::memory_order_release);
  gctools::stop_type.notify_all();
}

// GC-safe this mutator for GC (used by MMTk's block_for_gc).
void clasp_pause_thread_for_gc() {
  gctools::wait_for_mutator_running();
  // Wait until the GC has actually set world_stopped=true before parking.
  // Without this, if block_for_gc is called before stop_all_mutators sets
  // world_stopped, end_gcsafe_shared returns immediately (world_stopped=false)
  // running_count goes back up, and clasp_stop_the_world waits forever.
  {
    std::unique_lock<std::mutex> lock(gctools::stw_mutex);
    gctools::world_stopping_cv.wait(lock, [] {
      return gctools::world_stopped.load(std::memory_order_acquire);
    });
  }
  gctools::gc_yield_slow_thread(my_thread);
}

} // extern "C"
