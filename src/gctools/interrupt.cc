#include <signal.h>
#include <llvm/Support/ErrorHandling.h>
#include <clasp/core/foundation.h>
#include <clasp/core/symbol.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lisp.h>
#include <clasp/core/debugger.h>
#include <clasp/gctools/threadlocal.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/designators.h>
#include <clasp/core/lispList.h>
#include <clasp/core/fp_env.h>
#include <clasp/gctools/interrupt.h>

SYMBOL_EXPORT_SC_(CorePkg,terminal_interrupt);
SYMBOL_EXPORT_SC_(CorePkg,wake_up_thread);
SYMBOL_EXPORT_SC_(ExtPkg,illegal_instruction);
SYMBOL_EXPORT_SC_(ExtPkg,segmentation_violation);
SYMBOL_EXPORT_SC_(ExtPkg,bus_error);
SYMBOL_EXPORT_SC_(CorePkg,wait_for_all_processes);

namespace gctools {

/*! The value of the signal that clasp uses to interrupt threads */
int global_signal = SIGUSR2;

/*! Signal info is in CONS set by ADD_SIGNAL macro at bottom */
core::T_sp safe_signal_name(int sig) {
  WITH_READ_LOCK(_lisp->_Roots._UnixSignalHandlersMutex);
  core::T_sp key = core::clasp_make_fixnum(sig);
  if (_lisp->_Roots._Booted) {
    core::T_sp cur = core__alist_assoc_eql(_lisp->_Roots._UnixSignalHandlers,key);
    if (cur.notnilp()) {
      return oCadr(cur); // return the signal name
    }
  }
  return key;
}

/*! Signal info is in CONS set by ADD_SIGNAL macro at bottom */
core::T_sp safe_signal_handler(int sig) {
  WITH_READ_LOCK(_lisp->_Roots._UnixSignalHandlersMutex);
  core::T_sp key = core::clasp_make_fixnum(sig);
  if (_lisp->_Roots._Booted) { 
    core::T_sp cur = core__alist_assoc_eql(_lisp->_Roots._UnixSignalHandlers,key);
    if (cur.notnilp()) {
      return oCaddr(cur); // return the signal handler
    }
  }
  return key;
}

core::T_mv gctools__signal_info(int sig) {
  return Values(safe_signal_name(sig),safe_signal_handler(sig));
}

// INTERRUPTS

static bool do_interrupt_thread(mp::Process_sp process)
{
  fflush(stdout);
# ifdef ECL_WINDOWS_THREADS
#  ifndef ECL_USE_GUARD_PAGE
#   error "Cannot implement ecl_interrupt_process without guard pages"
#  endif
  HANDLE thread = (HANDLE)process->process.thread;
  CONTEXT context;
  void *trap_address = process->process.env;
  DWORD guard = PAGE_GUARD | PAGE_READWRITE;
  int ok = 1;
  if (SuspendThread(thread) == (DWORD)-1) {
    FEwin32_error("Unable to suspend thread ~A", 1,
                  process);
    ok = 0;
    goto EXIT;
  }
  process->process.interrupt = ECL_T;
  if (!VirtualProtect(process->process.env,
                      sizeof(struct cl_env_struct),
                      guard,
                      &guard))
  {
    FEwin32_error("Unable to protect memory from thread ~A",
                  1, process);
    ok = 0;
  }
 RESUME:
  if (!QueueUserAPC(wakeup_function, thread, 0)) {
    FEwin32_error("Unable to queue APC call to thread ~A",
                  1, process);
    ok = 0;
  }
  if (ResumeThread(thread) == (DWORD)-1)  {
    FEwin32_error("Unable to resume thread ~A", 1,
                  process);
    ok = 0;
    goto EXIT;
  }
 EXIT:
  return ok;
# else
  int signal = global_signal;
  if (pthread_kill(process->_Thread,signal)) {
    FElibc_error("Unable to interrupt process ~A", 1,
                 process);
  }
  return 1;
# endif
}

static void queue_signal_or_interrupt(core::ThreadLocalState*, core::T_sp, bool);
void clasp_interrupt_process(mp::Process_sp process, core::T_sp function)
{
        /*
         * Lifted from the ECL source code.  meister 2017
         * We first ensure that the process is active and running
         * and past the initialization phase, where it has set up
         * the environment. Then:
         * - In Windows it sets up a trap in the stack, so that the
         *   uncaught exception handler can catch it and process it.
         * - In POSIX systems it sends a user level interrupt to
         *   the thread, which then decides how to act.
         *
         * If FUNCTION is NIL, we just intend to wake up the process
         * from some call to ecl_musleep() Queue the interrupt for any
         * process stage that can potentially receive a signal  */
  if (function.notnilp() && (process->_Phase >= mp::Booting)) {
    printf("%s:%d clasp_interrupt_process queuing signal\n", __FILE__, __LINE__);
    function = core::coerce::functionDesignator(function);
    queue_signal_or_interrupt(process->_ThreadInfo, function, true);
  }
        /* ... but only deliver if the process is still alive */
  if (process->_Phase == mp::Active) do_interrupt_thread(process);
}

inline bool interrupts_disabled_by_C() {
  return my_thread_low_level->_DisableInterrupts;
}

inline bool interrupts_disabled_by_lisp() {
  return core::_sym_STARinterrupts_enabledSTAR->symbolValue().notnilp();
}

// SIGNAL QUEUE
// This is a regular lisp list. We keep a few extra conses lying around
// and use those rather than allocate within signal handlers.
// Objects in the queue are either fixnums, representing signals, or
// functions, representing interrupts.

static void queue_signal_or_interrupt(core::ThreadLocalState* thread, core::T_sp thing, bool allocate)
{
  mp::SafeSpinLock spinlock(thread->_SparePendingInterruptRecordsSpinLock);
  core::T_sp record;
  if (allocate) {
    record = core::Cons_O::create(_Nil<core::T_O>(),_Nil<core::T_O>());
  } else {
    record = thread->_SparePendingInterruptRecords;
    if (record.consp()) {
      thread->_SparePendingInterruptRecords = record.unsafe_cons()->cdr();
    }
  }
  if (record.consp()) {
    record.unsafe_cons()->rplaca(thing);
    record.unsafe_cons()->rplacd(_Nil<core::T_O>());
    thread->_PendingInterrupts = clasp_nconc(thread->_PendingInterrupts,record);
  }
}

static void queue_signal(int signo) {
  queue_signal_or_interrupt(my_thread, core::clasp_make_fixnum(signo), false);
}

// Pop a thing from the queue.
// NOTE: Don't call this unless you're holding the spare records spinlock.
core::T_sp pop_signal_or_interrupt(core::ThreadLocalState* thread) {
  core::T_sp value;
  core::Cons_sp record;
  { // <---- brace for spinlock scope
    mp::SafeSpinLock spinlock(thread->_SparePendingInterruptRecordsSpinLock);
    record = gc::As<core::Cons_sp>(thread->_PendingInterrupts);
    value = record->ocar();
    thread->_PendingInterrupts = record->cdr();
    if (value.fixnump() || gc::IsA<core::Symbol_sp>(value)) {
      // Conses that contain fixnum or symbol values are recycled onto the
      // _SparePendingInterruptRecords stack
      record->rplacd(thread->_SparePendingInterruptRecords);
      thread->_SparePendingInterruptRecords = record;
    }
  }
  return value;
}

// Perform one action (presumably popped from the queue).
void handle_queued_signal_or_interrupt(core::T_sp signal_code) {
  if (signal_code.fixnump()) { // signal
    handle_signal_now(signal_code.unsafe_fixnum());
  } else if (gc::IsA<core::Function_sp>(signal_code)) { // interrupt
    core::eval::funcall(signal_code);
  }
}

// Do all the queued actions, emptying the queue.
void handle_all_queued_interrupts()
{
  while (my_thread->_PendingInterrupts.consp()) {
    printf("%s:%d:%s Handling a signal - there are pending interrupts\n", __FILE__, __LINE__, __FUNCTION__ );
    core::T_sp sig = pop_signal_or_interrupt(my_thread);
    printf("%s:%d:%s Handling a signal: %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(sig).c_str() );
    handle_queued_signal_or_interrupt(sig);
  }
}

CL_DEFUN void core__check_pending_interrupts() {
  handle_all_queued_interrupts();
}

// HANDLERS

// This is both a signal handler and called by signal handlers.
void handle_signal_now(int sig) {
  // If there's a handler function in the alist, call it.
  // Otherwise signal a generic, resumable error.
  core::Symbol_sp handler = safe_signal_handler(sig);
  if (handler->fboundp()) {
    core::eval::funcall(handler->symbolFunction());
  } else {
    core::T_sp signal_code = core::clasp_make_fixnum(sig);
    core::cl__cerror(ext::_sym_ignore_signal->symbolValue(),
                     ext::_sym_unix_signal_received,
                     core::Cons_O::createList(kw::_sym_code, signal_code, kw::_sym_handler, handler));
  }
}

void handle_or_queue_signal(int signo) {
  if (interrupts_disabled_by_lisp()) {
    queue_signal(signo);
  }
  else if(interrupts_disabled_by_C()) {
    my_thread_low_level->_DisableInterrupts = 3;
    queue_signal(signo);
  }
  else {
    handle_signal_now(signo);
  }
}
  
// false == SIGABRT invokes debugger, true == terminate (used in core__exit)
bool global_debuggerOnSIGABRT = true;
#define INITIAL_GLOBAL_POLL_TICKS_PER_CLEANUP 16386
int global_pollTicksPerCleanup = INITIAL_GLOBAL_POLL_TICKS_PER_CLEANUP;
int global_signalTrap = 0;
int global_pollTicksGC = INITIAL_GLOBAL_POLL_TICKS_PER_CLEANUP;

void handle_fpe(int signo, siginfo_t* info, void* context) {
  (void)context; // unused
  // printf("Enter handle_fpe Signo: %d Errno:%d Code:%d\n", (info->si_signo), (info->si_errno), (info->si_code));
  // init_float_traps(); // WHY
  // TODO: Get operation and operands when possible.
  // Probably off the call stack.
  switch (info->si_code) {
    #ifdef _TARGET_OS_DARWIN
  case FPE_NOOP:   NO_INITIALIZERS_ERROR(cl::_sym_arithmeticError);
    #endif
  case FPE_INTDIV: NO_INITIALIZERS_ERROR(cl::_sym_divisionByZero);
  case FPE_INTOVF: NO_INITIALIZERS_ERROR(cl::_sym_arithmeticError);
  case FPE_FLTDIV: NO_INITIALIZERS_ERROR(cl::_sym_divisionByZero);
  case FPE_FLTOVF: NO_INITIALIZERS_ERROR(cl::_sym_floatingPointOverflow);
  case FPE_FLTUND: NO_INITIALIZERS_ERROR(cl::_sym_floatingPointUnderflow);
  case FPE_FLTRES: NO_INITIALIZERS_ERROR(cl::_sym_floatingPointInexact);
  case FPE_FLTINV: NO_INITIALIZERS_ERROR(cl::_sym_floatingPointInvalidOperation);
  case FPE_FLTSUB: NO_INITIALIZERS_ERROR(cl::_sym_arithmeticError);
  default: // FIXME: signal a better error.
      // Can end up here with e.g. SI_USER if it originated from kill
      handle_signal_now(signo);
  }
}

void handle_segv(int signo, siginfo_t* info, void* context) {
  (void)context; // unused
  core::eval::funcall(ext::_sym_segmentation_violation,
                      core::Integer_O::create((uintptr_t)(info->si_addr)));
}

void handle_bus(int signo, siginfo_t* info, void* context) {
  (void)context;
  core::eval::funcall(ext::_sym_bus_error,
                      core::Integer_O::create((uintptr_t)(info->si_addr)));
}
  

void fatal_error_handler(void *user_data, const std::string &reason, bool gen_crash_diag) {
  printf("%s:%d Hit a fatal error in llvm: %s\n", __FILE__, __LINE__, reason.c_str());
  printf("Clasp is sleeping for 1000 seconds in case you want to connect in with the debugger - after which it will abort().\n");
  printf("    Clasp pid -> %d\n", getpid());
  sleep(1000);
  abort();
}

void wake_up_thread(int sig)
{
  const char* msg = "In wake_up_thread interrupt.cc:296\n";
  int len = strlen(msg);
  write(1,msg,len);
}

// SIGNALS INITIALIZATION

void initialize_signals(int clasp_signal) {
  // clasp_signal is the signal that we use as a thread interrupt.

#define INIT_SIGNAL(sig,flags,handler)         \
  new_action.sa_handler = handler;             \
  sigemptyset (&new_action.sa_mask);           \
  new_action.sa_flags = flags;                 \
  if (sigaction (sig, &new_action, NULL) != 0) \
    printf("failed to register " #sig " signal-handler with kernel error: %s\n", strerror(errno));

  // identical but with a sigaction. CLEANUP
#define INIT_SIGNALI(sig,flags,handler)        \
  new_action.sa_sigaction = handler;           \
  sigemptyset (&new_action.sa_mask);           \
  new_action.sa_flags = SA_SIGINFO | (flags);  \
  if (sigaction (sig, &new_action, NULL) != 0) \
    printf("failed to register " #sig " signal-handler with kernel error: %s\n", strerror(errno));

  struct sigaction new_action;

  // NOTE that for most signals we specify SA_NODEFER. This is because
  // the handlers often signal errors (in handle_signal_now), and if they
  // do a restart could wrest control from the handler back to normal
  // Lisp code. If that happens, and a signal is received again, we want
  // to deal with it the same way - not defer.
  
  INIT_SIGNAL(clasp_signal, (SA_NODEFER | SA_RESTART | SA_ONSTACK), handle_or_queue_signal);
  INIT_SIGNAL(global_signal, (SA_RESTART), wake_up_thread);
  INIT_SIGNAL(SIGINT, (SA_NODEFER | SA_RESTART), handle_or_queue_signal);
#ifdef SIGINFO
  INIT_SIGNAL(SIGINFO, (SA_NODEFER | SA_RESTART), handle_or_queue_signal);
#endif
  if (!getenv("CLASP_DONT_HANDLE_CRASH_SIGNALS")) {
    INIT_SIGNAL(SIGABRT, (SA_NODEFER | SA_RESTART), handle_or_queue_signal);
    INIT_SIGNALI(SIGSEGV, (SA_NODEFER | SA_RESTART | SA_ONSTACK), handle_segv);
    INIT_SIGNALI(SIGBUS, (SA_NODEFER | SA_RESTART), handle_bus);
  }
  INIT_SIGNALI(SIGFPE, (SA_NODEFER | SA_RESTART), handle_fpe);
  INIT_SIGNAL(SIGILL, (SA_NODEFER | SA_RESTART), handle_signal_now);
  // FIXME: Move?
  init_float_traps();
  llvm::install_fatal_error_handler(fatal_error_handler, NULL);
}

#define ADD_SIGNAL_SYMBOL(sig,sigsym,handler) {\
    core::List_sp info = core::Cons_O::createList(core::clasp_make_fixnum(sig),sigsym,handler); \
    _lisp->_Roots._UnixSignalHandlers = core::Cons_O::create(info,_lisp->_Roots._UnixSignalHandlers); \
  }
#define ADD_SIGNAL(sig,name,handler) {\
    core::Symbol_sp sigsym = _lisp->intern(name,KeywordPkg); \
    ADD_SIGNAL_SYMBOL(sig,sigsym,handler); \
  }

void gctools__push_unix_signal_handler(int signal, core::Symbol_sp name, core::Function_sp handler) {
  WITH_READ_WRITE_LOCK(_lisp->_Roots._UnixSignalHandlersMutex);
  ADD_SIGNAL_SYMBOL(signal,name,handler);
}

void initialize_unix_signal_handlers() {
#ifdef SIGHUP
        ADD_SIGNAL( SIGHUP, "SIGHUP", _Nil<core::T_O>());
#endif
#ifdef SIGINT
        ADD_SIGNAL( SIGINT, "SIGINT", core::_sym_terminal_interrupt);
#endif
#ifdef SIGQUIT
        ADD_SIGNAL( SIGQUIT, "SIGQUIT", _Nil<core::T_O>());
#endif
#ifdef SIGILL
        ADD_SIGNAL( SIGILL, "SIGILL", ext::_sym_illegal_instruction);
#endif
#ifdef SIGTRAP
        ADD_SIGNAL( SIGTRAP, "SIGTRAP", _Nil<core::T_O>());
#endif
#ifdef SIGABRT
        ADD_SIGNAL( SIGABRT, "SIGABRT", _Nil<core::T_O>());
#endif
#ifdef SIGEMT
        ADD_SIGNAL( SIGEMT, "SIGEMT", _Nil<core::T_O>());
#endif
/*
// We do install a sigfpe handler in initialize_signals
#ifdef SIGFPE
        ADD_SIGNAL( SIGFPE, "SIGFPE", _Nil<core::T_O>());
#endif
*/
#ifdef SIGKILL
        ADD_SIGNAL( SIGKILL, "SIGKILL", _Nil<core::T_O>());
#endif
/*
// These take a parameter, so will fail if called here, since handle_signal_now call with no parameters
// We do install correct handlers in initialize_signals
#ifdef SIGBUS
        ADD_SIGNAL( SIGBUS, "SIGBUS", ext::_sym_bus_error);
#endif
#ifdef SIGSEGV
        ADD_SIGNAL( SIGSEGV, "SIGSEGV", ext::_sym_segmentation_violation);
#endif
*/
#ifdef SIGSYS
        ADD_SIGNAL( SIGSYS, "SIGSYS", _Nil<core::T_O>());
#endif
#ifdef SIGPIPE
        ADD_SIGNAL( SIGPIPE, "SIGPIPE", _Nil<core::T_O>());
#endif
#ifdef SIGALRM
        ADD_SIGNAL( SIGALRM, "SIGALRM", _Nil<core::T_O>());
#endif
#ifdef SIGTERM
        ADD_SIGNAL( SIGTERM, "SIGTERM", _Nil<core::T_O>());
#endif
#ifdef SIGURG
        ADD_SIGNAL( SIGURG, "SIGURG", _Nil<core::T_O>());
#endif
#ifdef SIGSTOP
        ADD_SIGNAL( SIGSTOP, "SIGSTOP", _Nil<core::T_O>());
#endif
#ifdef SIGTSTP
        ADD_SIGNAL( SIGTSTP, "SIGTSTP", _Nil<core::T_O>());
#endif
#ifdef SIGCONT
        ADD_SIGNAL( SIGCONT, "SIGCONT", _Nil<core::T_O>());
#endif
/*
// core::_sym_wait_for_all_processes is undefined
#ifdef SIGCHLD
        ADD_SIGNAL( SIGCHLD, "SIGCHLD", core::_sym_wait_for_all_processes);
#endif
*/
#ifdef SIGTTIN
        ADD_SIGNAL( SIGTTIN, "SIGTTIN", _Nil<core::T_O>());
#endif
#ifdef SIGTTOU
        ADD_SIGNAL( SIGTTOU, "SIGTTOU", _Nil<core::T_O>());
#endif
#ifdef SIGIO
        ADD_SIGNAL( SIGIO, "SIGIO", _Nil<core::T_O>());
#endif
#ifdef SIGXCPU
        ADD_SIGNAL( SIGXCPU, "SIGXCPU", _Nil<core::T_O>());
#endif
#ifdef SIGXFSZ
        ADD_SIGNAL( SIGXFSZ, "SIGXFSZ", _Nil<core::T_O>());
#endif
#ifdef SIGVTALRM
        ADD_SIGNAL( SIGVTALRM, "SIGVTALRM", _Nil<core::T_O>());
#endif
#ifdef SIGPROF
        ADD_SIGNAL( SIGPROF, "SIGPROF", _Nil<core::T_O>());
#endif
#ifdef SIGWINCH
        ADD_SIGNAL( SIGWINCH, "SIGWINCH", _Nil<core::T_O>());
#endif
#ifdef SIGINFO
        ADD_SIGNAL( SIGINFO, "SIGINFO", ext::_sym_information_interrupt);
#endif
#ifdef SIGUSR1
        ADD_SIGNAL( SIGUSR1, "SIGUSR1", _Nil<core::T_O>());
#endif
#ifdef SIGUSR2
#ifdef _TARGET_OS_DARWIN
        ADD_SIGNAL( SIGUSR2, "SIGUSR2", _Nil<core::T_O>());
#endif
#ifdef _TARGET_OS_LINUX
        ADD_SIGNAL( SIGUSR2, "SIGUSR2", ext::_sym_information_interrupt);
#endif
#endif
#ifdef SIGTHR
        ADD_SIGNAL( SIGTHR, "SIGTHR", _Nil<core::T_O>());
#endif
};
  


};
