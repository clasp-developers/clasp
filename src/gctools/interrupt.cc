#include <signal.h>
#include <llvm/Support/ErrorHandling.h>
#include <signal.h>
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
#include <clasp/gctools/interrupt.h>

SYMBOL_EXPORT_SC_(CorePkg,terminal_interrupt);
SYMBOL_EXPORT_SC_(ExtPkg,illegal_instruction);
SYMBOL_EXPORT_SC_(ExtPkg,segmentation_violation);
SYMBOL_EXPORT_SC_(CorePkg,wait_for_all_processes);

namespace gctools {


/*! The value of the signal that clasp uses to interrupt threads */
int global_signal = 0;

static void queue_signal(core::ThreadLocalState* thread, core::T_sp code, bool allocate);

/*! Signal info is in CONS set by ADD_SIGNAL macro at bottom */
core::T_sp safe_signal_name(int sig) {
  core::T_sp key = core::clasp_make_fixnum(sig);
  if (_lisp->_Roots._Booted) {
    core::T_sp value = _lisp->_Roots._KnownSignals->gethash(key);
    if (value.notnilp()) {
      return oCar(value);
    }
  }
  return key;
}

/*! Signal info is in CONS set by ADD_SIGNAL macro at bottom */
core::T_sp safe_signal_handler(int sig) {
  core::T_sp key = core::clasp_make_fixnum(sig);
  if (_lisp->_Roots._Booted) {
    core::T_sp value = _lisp->_Roots._KnownSignals->gethash(key);
    if (value.notnilp()) {
      return oCdr(value);
    }
  }
  return key;
}


static bool do_interrupt_thread(mp::Process_sp process)
{
  printf("%s:%d do_interrupt_thread of process %s\n", __FILE__, __LINE__, _rep_(process).c_str());
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
  printf("%s:%d clasp_interrupt_process process: %s\n", __FILE__, __LINE__, _rep_(process).c_str());
  fflush(stdout);
  if (function.notnilp() && (process->_Phase >= mp::Booting)) {
    printf("%s:%d clasp_interrupt_process queuing signal\n", __FILE__, __LINE__);
    function = core::coerce::functionDesignator(function);
    queue_signal(process->_ThreadInfo, function, true);
  }
        /* ... but only deliver if the process is still alive */
  if (process->_Phase == mp::Active) do_interrupt_thread(process);
}







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
  return thread->_Bindings.value(core::_sym_STARinterrupts_enabledSTAR,
                                 &core::_sym_STARinterrupts_enabledSTAR->_GlobalValue).notnilp();
}

void handle_signal_now( core::T_sp signal_code, core::T_sp process ) {
  if ( signal_code.fixnump() ) {
    printf("%s:%d Received a unix signal %s\n", __FILE__, __LINE__, _rep_(safe_signal_name(signal_code.unsafe_fixnum())).c_str());
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
}



static void queue_signal(core::ThreadLocalState* thread, core::T_sp code, bool allocate)
{
  if (!code) {
    printf("%s:%d queue_signal code = NULL\n", __FILE__, __LINE__ );
  }
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
//    printf("%s:%d  queue_signal\n",__FILE__, __LINE__ );
//    core::dbg_lowLevelDescribe(code);
    record.unsafe_cons()->_Car = code;
    record.unsafe_cons()->_Cdr = _Nil<core::T_O>();
    thread->_PendingInterrupts = clasp_nconc(thread->_PendingInterrupts,record);
//    core::dbg_lowLevelDescribe(thread->_PendingInterrupts);
  }
}

core::T_sp pop_signal(core::ThreadLocalState* thread) {
  core::T_sp record, value;
  if (!thread->_PendingInterrupts.consp()) return _Nil<core::T_O>();
  { // <---- brace for spinlock scope
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
//    printf("%s:%d  handle_all_queued \n", __FILE__, __LINE__);
//    core::dbg_lowLevelDescribe(sig);
    handle_signal_now(sig, thread->_Process);
  }
}

void handle_or_queue(core::ThreadLocalState* thread, core::T_sp signal_code ) {
  if (!signal_code) {
    printf("%s:%d handle_or_queue   signal_code is NULL\n", __FILE__, __LINE__ );
  }
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


#if 0
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
#endif


void handle_signals(int signo) {
  //
  // Indicate that a signal was caught and handle it at a safe-point
  //
  handle_or_queue(my_thread,core::clasp_make_fixnum(signo));
}


void interrupt_handle_signals(int signo) {
  //
  // Indicate that a signal was caught and handle it at a safe-point
  //
  printf("%s:%d  Process %s received an interrupt signal %d\n", __FILE__, __LINE__, _rep_(my_thread->_Process).c_str(), signo);
  handle_or_queue(my_thread,core::clasp_make_fixnum(signo));
}




void lisp_check_pending_interrupts(core::ThreadLocalState* thread)
{
  handle_all_queued(thread);
}


CL_DEFUN void core__check_pending_interrupts() {
  handle_all_queued(my_thread);
}


void fatal_error_handler(void *user_data, const std::string &reason, bool gen_crash_diag) {
  printf("%s:%d Hit a fatal error in llvm: %s\n", __FILE__, __LINE__, reason.c_str());
  printf("Clasp is sleeping for 1000 seconds in case you want to connect in with the debugger - after which it will abort().\n");
  printf("    Clasp pid -> %d\n", getpid());
  sleep(1000);
  abort();
}

#ifdef HAVE_SIGPROCMASK
static sigset_t main_thread_sigmask;
# define handler_fn_prototype(name, sig, info, aux) name(sig, info, aux)
# define call_handler(name, sig, info, aux) name(sig, info, aux)
# define reinstall_signal(x,y)
#if 0
static void
mysignal(int code, void *handler)
{
        struct sigaction action;
        sigaction(code, NULL, &action);
        if (handler == SIG_IGN || handler == SIG_DFL) {
                action.sa_handler = handler;
        } else {
#ifdef SA_SIGINFO
                /* void (*handler)(int, siginfo_t *, void*) */
                action.sa_sigaction = handler;
                action.sa_flags = SA_SIGINFO;
#else
                /* void (*handler)(int) */
                action.sa_handler = handler;
                action.sa_flags = 0;
#endif
                sigfillset(&action.sa_mask);
        }
        sigaction(code, &action, NULL);
}
#endif
#else /* HAVE_SIGPROCMASK */
# define handler_fn_prototype(name, sig, info, aux) name(sig)
# define call_handler(name, sig, info, aux) name(sig)
# define mysignal(x,y) signal(x,y)
# define reinstall_signal(x,y) signal(x,y)
#endif

#if 0
static void
handler_fn_prototype(non_evil_signal_handler, int sig, siginfo_t *siginfo, void *data)
{
        int old_errno = errno;
        cl_env_ptr the_env;
        cl_object signal_object;
        reinstall_signal(sig, non_evil_signal_handler);
        /* The lisp environment might not be installed. */
        the_env = ecl_process_env();
        unlikely_if (zombie_process(the_env))
                return;
        signal_object = ecl_gethash_safe(ecl_make_fixnum(sig),
                                         cl_core.known_signals,
                                         ECL_NIL);
        handle_or_queue(the_env, signal_object, sig);
        errno = old_errno;
}
#endif


void initialize_signals(int clasp_signal) {
  struct sigaction new_action, old_action;
  /* Set up the structure to specify the new action. */
  new_action.sa_handler = interrupt_handle_signals;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = SA_RESTART | SA_ONSTACK;
  if (sigaction (clasp_signal, &new_action, NULL) != 0) {
    printf("failed to register clasp_signal signal-handler with kernel error: %s\n", strerror(errno));
  }
  new_action.sa_handler = handle_signals;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = SA_RESTART;
  if (sigaction (SIGINT, &new_action, NULL) != 0) {
    printf("failed to register SIGINT signal-handler with kernel error: %s\n", strerror(errno));
  }
  new_action.sa_handler = handle_signals;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = SA_RESTART | SA_ONSTACK;
  if (sigaction (SIGABRT, &new_action, NULL) != 0) {
    printf("failed to register SIGABRT signal-handler with kernel error: %s\n", strerror(errno));
  }
  new_action.sa_handler = handle_signals;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = SA_RESTART | SA_ONSTACK;
  if (sigaction (SIGSEGV, &new_action, NULL) != 0) {
    printf("failed to register SIGSEGV signal-handler with kernel error: %s\n", strerror(errno));
  }
#if 0
  if (signal(SIGCHLD, handle_signals) == SIG_ERR) {
    printf("failed to register SIGCHLD signal-handler with kernel\n");
  }
#endif
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




void initialize_signal_constants() {
  _lisp->_Roots._KnownSignals = core::HashTableEq_O::create_default();
#define ADD_SIGNAL(sig,name,handler) {\
    core::Symbol_sp sigsym = _lisp->intern(name,CorePkg); \
    _lisp->_Roots._KnownSignals->hash_table_setf_gethash(core::clasp_make_fixnum(sig),core::Cons_O::create(sigsym,handler));\
  }
                                                       
#ifdef SIGHUP
        ADD_SIGNAL( SIGHUP, "+SIGHUP+", _Nil<core::T_O>());
#endif
#ifdef SIGINT
        ADD_SIGNAL( SIGINT, "+SIGINT+", core::_sym_terminal_interrupt);
#endif
#ifdef SIGQUIT
        ADD_SIGNAL( SIGQUIT, "+SIGQUIT+", _Nil<core::T_O>());
#endif
#ifdef SIGILL
        ADD_SIGNAL( SIGILL, "+SIGILL+", ext::_sym_illegal_instruction);
#endif
#ifdef SIGTRAP
        ADD_SIGNAL( SIGTRAP, "+SIGTRAP+", _Nil<core::T_O>());
#endif
#ifdef SIGABRT
        ADD_SIGNAL( SIGABRT, "+SIGABRT+", _Nil<core::T_O>());
#endif
#ifdef SIGEMT
        ADD_SIGNAL( SIGEMT, "+SIGEMT+", _Nil<core::T_O>());
#endif
#ifdef SIGFPE
        ADD_SIGNAL( SIGFPE, "+SIGFPE+", _Nil<core::T_O>());
#endif
#ifdef SIGKILL
        ADD_SIGNAL( SIGKILL, "+SIGKILL+", _Nil<core::T_O>());
#endif
#ifdef SIGBUS
        ADD_SIGNAL( SIGBUS, "+SIGBUS+", ext::_sym_segmentation_violation);
#endif
#ifdef SIGSEGV
        ADD_SIGNAL( SIGSEGV, "+SIGSEGV+", ext::_sym_segmentation_violation);
#endif
#ifdef SIGSYS
        ADD_SIGNAL( SIGSYS, "+SIGSYS+", _Nil<core::T_O>());
#endif
#ifdef SIGPIPE
        ADD_SIGNAL( SIGPIPE, "+SIGPIPE+", _Nil<core::T_O>());
#endif
#ifdef SIGALRM
        ADD_SIGNAL( SIGALRM, "+SIGALRM+", _Nil<core::T_O>());
#endif
#ifdef SIGTERM
        ADD_SIGNAL( SIGTERM, "+SIGTERM+", _Nil<core::T_O>());
#endif
#ifdef SIGURG
        ADD_SIGNAL( SIGURG, "+SIGURG+", _Nil<core::T_O>());
#endif
#ifdef SIGSTOP
        ADD_SIGNAL( SIGSTOP, "+SIGSTOP+", _Nil<core::T_O>());
#endif
#ifdef SIGTSTP
        ADD_SIGNAL( SIGTSTP, "+SIGTSTP+", _Nil<core::T_O>());
#endif
#ifdef SIGCONT
        ADD_SIGNAL( SIGCONT, "+SIGCONT+", _Nil<core::T_O>());
#endif
#ifdef SIGCHLD
        ADD_SIGNAL( SIGCHLD, "+SIGCHLD+", core::_sym_wait_for_all_processes);
#endif
#ifdef SIGTTIN
        ADD_SIGNAL( SIGTTIN, "+SIGTTIN+", _Nil<core::T_O>());
#endif
#ifdef SIGTTOU
        ADD_SIGNAL( SIGTTOU, "+SIGTTOU+", _Nil<core::T_O>());
#endif
#ifdef SIGIO
        ADD_SIGNAL( SIGIO, "+SIGIO+", _Nil<core::T_O>());
#endif
#ifdef SIGXCPU
        ADD_SIGNAL( SIGXCPU, "+SIGXCPU+", _Nil<core::T_O>());
#endif
#ifdef SIGXFSZ
        ADD_SIGNAL( SIGXFSZ, "+SIGXFSZ+", _Nil<core::T_O>());
#endif
#ifdef SIGVTALRM
        ADD_SIGNAL( SIGVTALRM, "+SIGVTALRM+", _Nil<core::T_O>());
#endif
#ifdef SIGPROF
        ADD_SIGNAL( SIGPROF, "+SIGPROF+", _Nil<core::T_O>());
#endif
#ifdef SIGWINCH
        ADD_SIGNAL( SIGWINCH, "+SIGWINCH+", _Nil<core::T_O>());
#endif
#ifdef SIGINFO
        ADD_SIGNAL( SIGINFO, "+SIGINFO+", _Nil<core::T_O>());
#endif
#ifdef SIGUSR1
        ADD_SIGNAL( SIGUSR1, "+SIGUSR1+", _Nil<core::T_O>());
#endif
#ifdef SIGUSR2
        ADD_SIGNAL( SIGUSR2, "+SIGUSR2+", _Nil<core::T_O>());
#endif
#ifdef SIGTHR
        ADD_SIGNAL( SIGTHR, "+SIGTHR+", _Nil<core::T_O>());
#endif
};
  


};
