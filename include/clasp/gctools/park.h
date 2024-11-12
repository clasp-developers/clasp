#pragma once

// This file is included in some very early places (e.g. mpPackage.fwd.h)
// so make sure it includes very little.

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
 * They're macros because they might need to end up calling some kinda
 * call_without_gc(void (*)(void*), void*) eventually.
 */
#define BEGIN_PARK gctools::begin_park(); do
#define END_PARK while (false); gctools::end_park()

namespace gctools {
  // Implementation details, defined in park.cc. See above note about including.
  void begin_park();
  void end_park();
};
