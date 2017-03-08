#include <llvm/Support/ErrorHandling.h>
#include <clasp/core/foundation.h>
#include <clasp/core/symbol.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lisp.h>
#include <clasp/gctools/processes.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispList.h>
#include <clasp/gctools/interrupt.h>

namespace gctools {

void lisp_enable_interrupts(core::ThreadLocalState* thread) {
  thread->_DisableInterrupts = false;
}

void lisp_disable_interrupts(core::ThreadLocalState* thread) {
  thread->_DisableInterrupts = true;
}

inline bool interrupts_disabled_by_C(core::ThreadLocalState* thread) {
  return thread->_DisableInterrupts;
}

inline bool interrupts_disabled_by_lisp(core::ThreadLocalState* thread) {
  return thread->_Bindings.value(ext::_sym_STARinterrupts_enabledSTAR).notnilp();
}

void handle_signal_now( core::T_sp signal_code, core::T_sp process ) {
  if ( signal_code.fixnump() ) {
    core::cl__cerror(ext::_sym_ignore_signal->symbolValue(),ext::_sym_unix_signal_received,
                     core::Cons_O::createList(kw::_sym_code, signal_code));
  } else if (gc::IsA<core::Symbol_sp>(signal_code)) {
    if (core::cl__find_class(signal_code,false,_Nil<core::T_O>()).notnilp()) {
      core::cl__cerror(ext::_sym_ignore_signal->symbolValue(),signal_code,_Nil<core::T_O>());
    } else if (process.notnilp()) {
      core::eval::funcall(signal_code,kw::_sym_process,process);
    } else {
      core::eval::funcall(signal_code);
    }
  } else if (gc::IsA<core::Function_sp>(signal_code)) {
    core::eval::funcall(signal_code);
  }
  printf("%s:%d Hit the bottom of handle_signal_now with unknown object: %s\n", __FILE__, __LINE__, _rep_(signal_code).c_str());
}



static void queue_signal(core::ThreadLocalState* thread, core::T_sp code, bool allocate)
{
  mp::SafeSpinLock spinlock(thread->_SparePendingInterruptRecordsSpinLock);
  core::T_sp record;
  if (allocate) {
    record = core::Cons_O::create(_Nil<core::T_O>(),_Nil<core::T_O>());
  } else {
    record = thread->_SparePendingInterruptRecords;
    if (record.consp()) {
      thread->_SparePendingInterruptRecords = record.unsafe_cons()->_Cdr;
    }
  }
  if (record.consp()) {
    record.unsafe_cons()->_Car = record;
    thread->_PendingInterrupts = clasp_nconc(thread->_PendingInterrupts,record);
  }
}

core::T_sp pop_signal(core::ThreadLocalState* thread) {
  core::T_sp record, value;
  if (!thread->_PendingInterrupts.consp()) return _Nil<core::T_O>();
  {
    mp::SafeSpinLock spinlock(thread->_SparePendingInterruptRecordsSpinLock);
    record = thread->_PendingInterrupts;
    value = record.unsafe_cons()->_Car;
    thread->_PendingInterrupts = record.unsafe_cons()->_Cdr;
    if (value.fixnump() || gc::IsA<core::Symbol_sp>(value)) {
      // Conses that contain fixnum or symbol values are recycled onto the
      // _SparePendingInterruptRecords stack
      record.unsafe_cons()->_Cdr = thread->_SparePendingInterruptRecords;
      thread->_SparePendingInterruptRecords = record;
    }
  }
  return value;
}
static void handle_all_queued(core::ThreadLocalState* thread)
{
  unlikely_if (!thread->_PendingInterrupts) {
    // While initializing thread->_PendingInterrupts will be 0x00
    // and we will ignore it until it is set up properly
    return;
  }
  while (thread->_PendingInterrupts.notnilp()) {
    core::T_sp sig = pop_signal(thread);
    printf("%s:%d  handle_all_queued  sig = %s\n", __FILE__, __LINE__, _rep_(sig).c_str());
    handle_signal_now(sig, thread->_Process);
  }
}

void handle_or_queue(core::ThreadLocalState* thread, core::T_sp signal_code ) {
  if (signal_code.nilp() || !signal_code) return;
  if (interrupts_disabled_by_lisp(thread)) {
    queue_signal(thread,signal_code,false);
  }
  else if(interrupts_disabled_by_C(thread)) {
    thread->_DisableInterrupts = 3;
    queue_signal(thread,signal_code,false);
//    set_guard_page(thread);
  }
  else {
//    if (code) unblock_signal(thread,code);
//    si_trap_fpe(cl::_sym_last,_lisp->_true());
    handle_signal_now(signal_code,thread->_Process);
  }
}
  
// false == SIGABRT invokes debugger, true == terminate (used in core__exit)
bool global_debuggerOnSIGABRT = true;
#define INITIAL_GLOBAL_POLL_TICKS_PER_CLEANUP 16386
int global_pollTicksPerCleanup = INITIAL_GLOBAL_POLL_TICKS_PER_CLEANUP;
int global_signalTrap = 0;
int global_pollTicksGC = INITIAL_GLOBAL_POLL_TICKS_PER_CLEANUP;

void do_pollSignals() {
  int signo = global_signalTrap;
  SET_SIGNAL(0);
  if (signo == SIGINT) {
    printf("You pressed Ctrl+C - ignoring for now\n");
#if 0
    core::eval::funcall(cl::_sym_break, core::SimpleBaseString_O::make("Break on Ctrl+C"));
#endif
      //    core__invoke_internal_debugger(_Nil<core::T_O>());
    printf("Resuming after Ctrl+C\n");
  } else if (signo == SIGCHLD) {
      //            printf("A child terminated\n");
  } else if (signo == SIGFPE) {
    printf("%s:%d A floating point error occurred\n", __FILE__, __LINE__);
    core__invoke_internal_debugger(_Nil<core::T_O>());
  } else if (signo == SIGABRT) {
    printf("ABORT was called!!!!!!!!!!!!\n");
    core__invoke_internal_debugger(_Nil<core::T_O>());
      //    core:eval::funcall(cl::_sym_break,core::SimpleBaseString_O::make("ABORT was called"));
  }
#ifdef USE_MPS
  if (--global_pollTicksGC == 0 ) {
    global_pollTicksGC = global_pollTicksPerCleanup;
    gctools::gctools__cleanup();
  }
#endif
}


void handle_signals(int signo) {
  //
  // Indicate that a signal was caught and handle it at a safe-point
  //
  handle_or_queue(my_thread,core::clasp_make_fixnum(signo));
}




void lisp_check_pending_interrupts(core::ThreadLocalState* thread)
{
  handle_all_queued(thread);
}


void fatal_error_handler(void *user_data, const std::string &reason, bool gen_crash_diag) {
  printf("Hit a fatal error in llvm/clang: %s\n", reason.c_str());
  printf("Clasp is terminating via abort(0)\n");
  abort();
}



void initialize_signals() {
  if (signal(SIGINT, handle_signals) == SIG_ERR) {
    printf("failed to register SIGINT signal-handler with kernel\n");
  }
  if (signal(SIGCHLD, handle_signals) == SIG_ERR) {
    printf("failed to register SIGCHLD signal-handler with kernel\n");
  }
  if (signal(SIGABRT, handle_signals) == SIG_ERR) {
    printf("failed to register SIGABRT signal-handler with kernel\n");
  }
#if 0
#ifdef _TARGET_OS_LINUX
  feenableexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
#endif
#ifdef _TARGET_OS_DARWIN
  feenableexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
#endif
  if (signal(SIGFPE, handle_signals) == SIG_ERR) {
    printf("failed to register SIGFPE signal-handler with kernel\n");
  }
#endif
  llvm::install_fatal_error_handler(fatal_error_handler, NULL);
}


};
