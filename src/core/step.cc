#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/threadlocal.h>

namespace core {

// RAII thing to toggle breakstep while respecting nonlocal exit.
// TODO: Check that this works with our unwinding? Not sure it does
struct BreakstepToggle {
  ThreadLocalState* mthread;
  bool old_breakstep;
  BreakstepToggle(ThreadLocalState* thread, bool new_breakstep) {
    mthread = thread;
    old_breakstep = thread->_Breakstep;
    thread->_Breakstep = new_breakstep;
  }
  ~BreakstepToggle() { mthread->_Breakstep = old_breakstep; }
};

void breakstep(T_sp source, void* frame) {
  void* bframe = my_thread->_BreakstepFrame;
  // If bframe is NULL, we are doing step-into.
  // Otherwise, we are doing step-over, and we need to check
  // if we've returned yet. bframe is the frame step-over was initiated
  // from, and lframe/frame is the caller frame.
  // We have to check here because a function being stepped over may
  // nonlocally exit past the caller, and in that situation we want to
  // resume stepping.
  // FIXME: We assume stack growth direction here.
  if (!bframe || (frame >= bframe)) {
    // Make sure we don't invoke the stepper recursively,
    // but can do so again once we're out of the Lisp interaction.
    BreakstepToggle tog(my_thread, false);
    T_sp res = core::eval::funcall(core::_sym_breakstep, source);
    if (res.fixnump()) {
      switch (res.unsafe_fixnum()) {
      case 0:
          goto stop_stepping;
      case 1:
          my_thread->_BreakstepFrame = NULL;
          return;
      case 2:
          my_thread->_BreakstepFrame = frame;
          return;
      }
    }
    SIMPLE_ERROR("BUG: Unknown return value from {}: {}", _rep_(core::_sym_breakstep), _rep_(res));
  } else
    return;
 stop_stepping: // outside the scope of tog
  my_thread->_Breakstep = false;
  return;
}

// when we have a call but no source - bytecode for now FIXME
void breakstep_args(void* frame, Function_sp function, List_sp args) {
  void* bframe = my_thread->_BreakstepFrame;
  // FIXME: We assume stack growth direction here.
  if (!bframe || (frame >= bframe)) {
    // Make sure we don't invoke the stepper recursively,
    // but can do so again once we're out of the Lisp interaction.
    BreakstepToggle tog(my_thread, false);
    T_sp res = core::eval::funcall(core::_sym_breakstep_arguments,
                                   function, args);
    if (res.fixnump()) {
      switch (res.unsafe_fixnum()) {
      case 0:
          goto stop_stepping;
      case 1:
          my_thread->_BreakstepFrame = NULL;
          return;
      case 2:
          my_thread->_BreakstepFrame = frame;
          return;
      }
    }
    SIMPLE_ERROR("BUG: Unknown return value from {}: {}", _rep_(core::_sym_breakstep), _rep_(res));
  } else
    return;
 stop_stepping: // outside the scope of tog
  my_thread->_Breakstep = false;
  return;
}

}; // namespace core
