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

void stw_register_thread() {
  running_count.fetch_add(1, std::memory_order_acq_rel);
}

void stw_unregister_thread() {
  // If the thread is currently paused, begin_gcless already decremented
  // the count; don't double-decrement.
  if (my_thread && my_thread->gclessp())
    return;
  int prev = running_count.fetch_sub(1, std::memory_order_acq_rel);
  if (prev == 1) {
    all_parked_cv.notify_all();
  }
}

void begin_gcless_shared() {
  // Save the current stack pointer before decrementing running_count.
  // The acq_rel fence on fetch_sub below ensures the GC thread sees this
  // store after observing running_count == 0.
  if (my_thread_low_level) {
    my_thread_low_level->_ControlStackPointer = __builtin_frame_address(0);
  }
  int prev = running_count.fetch_sub(1, std::memory_order_acq_rel);
  if (prev == 1) {
    all_parked_cv.notify_all();
  }
}

void stw_mutator_stop() {
  // Same as begin_gcless_shared but used by call_with_stopped_world callers
  // that are registered mutators and need to remove themselves from the count
  // before calling clasp_stop_the_world().
  begin_gcless_shared();
}

void stw_mutator_resume() {
  // Re-add the mutator after clasp_resume_the_world(). Does NOT wait for
  // world_stopped because the caller just cleared it.
  running_count.fetch_add(1, std::memory_order_acq_rel);
}

void begin_gcless() {
  my_thread->gcless();
  begin_gcless_shared();
}

void end_gcless_shared() {
  std::unique_lock<std::mutex> lock(stw_mutex);
  world_resumed_cv.wait(lock, [] { return !world_stopped.load(std::memory_order_acq_rel); });
  running_count.fetch_add(1, std::memory_order_acq_rel);
}

void end_gcless() {
  end_gcless_shared();
  my_thread->unblock();
}

// see gc_yield
void gc_yield_slow() {
  // Don't need to wait on world_stopping_cv, since in gc_yield we already
  // checked that world_stopped is true.
  begin_gcless();
  end_gcless();
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
  gctools::world_stopped.store(true, std::memory_order_acq_rel);
  // Notify before taking stw_mutex so threads waiting in clasp_pause_thread_for_gc
  // can wake, see world_stopped=true, and call begin_gcless() to decrement running_count.
  gctools::world_stopping_cv.notify_all();
  std::unique_lock<std::mutex> lock(gctools::stw_mutex);
  gctools::all_parked_cv.wait(lock, [] {
    return gctools::running_count.load(std::memory_order_acq_rel) == 0;
  });
}

void clasp_resume_the_world() {
  gctools::world_stopped.store(false, std::memory_order_acq_rel);
  gctools::world_resumed_cv.notify_all();
}

// Park this mutator for GC (used by MMTk's block_for_gc).
void clasp_pause_thread_for_gc() {
  // Wait until the GC has actually set world_stopped=true before parking.
  // Without this, if block_for_gc is called before stop_all_mutators sets
  // world_stopped, end_gcless_shared returns immediately (world_stopped=false),
  // running_count goes back up, and clasp_stop_the_world waits forever.
  {
    std::unique_lock<std::mutex> lock(gctools::stw_mutex);
    gctools::world_stopping_cv.wait(lock, [] {
      return gctools::world_stopped.load(std::memory_order_acq_rel);
    });
  }
  gctools::begin_gcless();
  // end_gcless blocks until world_stopped becomes false.
  gctools::end_gcless();
}

} // extern "C"
