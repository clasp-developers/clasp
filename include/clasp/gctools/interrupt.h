#pragma once

namespace gctools {

template <typename Stage = RuntimeStage> void handle_all_queued_interrupts();

template <>
inline void handle_all_queued_interrupts<SnapshotLoadStage>(){
    // Do nothing
};

void initialize_signals();

// Use the following mechanism to pause clasp
#define USE_USER_SIGNAL 1
extern bool global_user_signal;
void wait_for_user_signal(const char* message);
void setup_user_signal();

/* BEGIN_PARK and END_PARK are used to indicate that the current thread is
 * about to enter a period of seclusion, e.g. blocking on a syscall.
 * They do the following:
 * 1) Handle all pending interrupts.
 * 2) Inform the GC that this thread will not be mutating any GC memory.
 *    (Currently this doesn't really happen since the GC doesn't need it.)
 * 3) Reconfigure signal handlers for this thread such that they will handle
 *    signals immediately rather than defer, so that this thread can be woken
 *    from its slumber by signals.
 * 4) Do whatever is between BEGIN_PARK and END_PARK.
 * 5) Undo #2 and #3.
 * This is useful for grabbing a lock, sleeping, whatever.
 * Make sure you don't do anything GC-y (like allocate!) and are prepared to be
 * interrupted by signals whenever. Ideally do as little as possible inside,
 * like your single syscall.
 * It is okay to longjmp/whatever out of the block. The signal handler unparks
 * the thread before running user code.
 * See clasp_musleep for an example of usage.
 */
#define BEGIN_PARK\
  gctools::handle_all_queued_interrupts();\
  my_thread->set_blockingp(true);\
  do
#define END_PARK\
  while (false);\
  my_thread->set_blockingp(false)

}; // namespace gctools
