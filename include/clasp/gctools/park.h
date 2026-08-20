#pragma once

#include <concepts>

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
 * Make sure you unpark. If you longjmp you need to ensure that you unpark first.
 * The signal handler unparks the thread before running user code.
 * See clasp_musleep for an example of usage.
 */
#define BEGIN_PARK gctools::call_parked([&]()
#define END_PARK );

namespace gctools {
// Implementation details, defined in park.cc. See above note about including.
int begin_park(void*);
void end_park(int);
void* end_park_temporarily();

// Restores the park state on any exit. A parked thread that unwinds -- which is
// exactly what a cancelled blocking call does -- would otherwise keep running
// Lisp while still marked GC-safe and blocking.
struct ParkGuard {
  int _OldState;
  explicit ParkGuard(void* sp) : _OldState(begin_park(sp)) {}
  ~ParkGuard() { end_park(_OldState); }
  ParkGuard(const ParkGuard&) = delete;
  ParkGuard& operator=(const ParkGuard&) = delete;
};

template <std::invocable<> F>
requires (!std::same_as<void, std::invoke_result_t<F>>)
decltype(auto) call_parked(F f) {
  ParkGuard guard(__builtin_frame_address(0));
  return f();
}

template <std::invocable<> F>
requires (std::same_as<void, std::invoke_result_t<F>>)
void call_parked(F f) {
  ParkGuard guard(__builtin_frame_address(0));
  f();
}

// While parked, temporarily unpark (blocking if needed) to call a thunk.
// This is used in interrupt.cc for wakeups.
// Mirror of ParkGuard: re-parks on any exit, including an unwind.
struct UnparkGuard {
  void* _Old;
  UnparkGuard() : _Old(end_park_temporarily()) {}
  ~UnparkGuard() { begin_park(_Old); }
  UnparkGuard(const UnparkGuard&) = delete;
  UnparkGuard& operator=(const UnparkGuard&) = delete;
};

template <std::invocable<> F>
requires (!std::same_as<void, std::invoke_result_t<F>>)
decltype(auto) call_unparked(F f) {
  UnparkGuard guard;
  return f();
}
template <std::invocable<> F>
requires std::same_as<void, std::invoke_result_t<F>>
void call_unparked(F f) {
  UnparkGuard guard;
  f();
}
};
