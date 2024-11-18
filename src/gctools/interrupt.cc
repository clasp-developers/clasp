#include <unistd.h>
#include <signal.h>
#if defined(__i386__) || defined(__x86_64__)
#include <xmmintrin.h>
#endif
#include <llvm/Support/ErrorHandling.h>
#include <clasp/core/foundation.h>
#include <clasp/core/symbol.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lisp.h>
#include <clasp/core/debugger.h>
#include <clasp/gctools/threadlocal.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/lispList.h>
#include <clasp/gctools/interrupt.h>
#include <clasp/core/numbers.h>

/*
 * Interrupts! Oh boy! Get ready for chaos and difficulty.
 * Generally speaking we try to avoid truly asynchronous interrupt
 * signals. Instead, we queue them, and only fire at given safe points
 * by calling handle_all_queued_interrupts
 * (aka core:check-pending-interrupts, mp:signal-pending-interrupts).
 * Currently these safe points are when we allocate but that will
 * probably change in the future.
 * To fire an interrupt, we make an interrupt object, which is just a
 * Lisp condition (defined in conditions.lisp), and SIGNAL it. It can
 * then be handled. If it's not, we call SERVICE-INTERRUPT to do
 * whatever the interrupt says to do.
 *
 * We do accept asynchronous interrupts when we're just waiting, e.g.
 * from sleep, waiting to grab a lock, etc. See park.cc for details.
 *
 * POSIX signals are treated as interrupts for the most part because
 * they can always arrive asynchronously (via kill(2) etc). Some
 * of them we handle immediately because they can arrive such that if
 * the signal handler returns or is the default, the process will be
 * killed. For example SEGV will be signaled like this if we have a
 * memory problem. It is nontrivial to differentiate whether a signal
 * is used in this way and we're probably not doing it correctly
 * in all cases.
 */

SYMBOL_EXPORT_SC_(CorePkg, terminal_interrupt);
SYMBOL_EXPORT_SC_(ExtPkg, illegal_instruction);
SYMBOL_EXPORT_SC_(ExtPkg, segmentation_violation);
SYMBOL_EXPORT_SC_(ExtPkg, bus_error);

/* Stupid preprocessor nonsense to save some typing.
 * Undefined at the bottom of the file. */
#ifdef SIGSYS
#define DOSIGSYS(MAC) MAC(SYS)
#else
#define DOSIGSYS(MAC)
#endif
#ifdef SIGTRAP
#define DOSIGTRAP(MAC) MAC(TRAP)
#else
#define DOSIGTRAP(MAC)
#endif
#ifdef SIGVTALRM
#define DOSIGVTALRM(MAC) MAC(VTALRM)
#else
#define DOSIGVTALRM(MAC)
#endif
#ifdef SIGXCPU
#define DOSIGXCPU(MAC) MAC(XCPU)
#else
#define DOSIGXCPU(MAC)
#endif
#ifdef SIGXFSZ
#define DOSIGXFSZ(MAC) MAC(XFSZ)
#else
#define DOSIGXFSZ(MAC)
#endif
#ifdef SIGPOLL
#define DOSIGPOLL(MAC) MAC(POLL)
#else
#define DOSIGPOLL(MAC)
#endif
#ifdef SIGPROF
#define DOSIGPROF(MAC) MAC(PROF)
#else
#define DOSIGPROF(MAC)
#endif
#ifdef SIGEMT
#define DOSIGEMT(MAC) MAC(EMT)
#else
#define DOSIGEMT(MAC)
#endif
#ifdef SIGIO
#define DOSIGIO(MAC) MAC(IO)
#else
#define DOSIGIO(MAC)
#endif
#ifdef SIGPWR
#define DOSIGPWR(MAC) MAC(PWR)
#else
#define DOSIGPWR(MAC)
#endif
#ifdef SIGTHR
#define DOSIGTHR(MAC) MAC(THR)
#else
#define DOSIGTHR(MAC)
#endif

#define DO_ALL_SIGNALS(MAC) \
  MAC(ABRT); MAC(ALRM); MAC(BUS); MAC(CHLD); MAC(CONT); MAC(FPE); MAC(HUP);\
  MAC(ILL); MAC(INT); MAC(KILL); MAC(PIPE); MAC(QUIT); MAC(SEGV); MAC(STOP);\
  MAC(TERM); MAC(TSTP); MAC(TTIN); MAC(TTOU); MAC(USR1); MAC(USR2);\
  MAC(WINCH); MAC(URG);\
  DOSIGSYS(MAC); DOSIGTRAP(MAC); DOSIGVTALRM(MAC); DOSIGXCPU(MAC);\
  DOSIGXFSZ(MAC); DOSIGPOLL(MAC); DOSIGPROF(MAC); DOSIGEMT(MAC);\
  DOSIGIO(MAC); DOSIGPWR(MAC); DOSIGTHR(MAC);

#define SIGEXPORT(NAME) SYMBOL_EXPORT_SC_(CorePkg, SIG##NAME)
DO_ALL_SIGNALS(SIGEXPORT)
#undef SIGNEXPORT

namespace gctools {

// Flag used in wait_for_user_signal.
bool global_user_signal = false;

// INTERRUPTS

inline bool interrupts_disabled_p() {
  return my_thread_low_level->_DisableInterrupts
    || (my_thread->interrupt_queue_validp() // KLUDGE to not trigger problems if we do this early.
        && core::_sym_STARinterrupts_enabledSTAR->symbolValue().nilp());
}

// Perform one action (presumably popped from the queue).
void handle_queued_interrupt(core::T_sp signal_code) {
  if (mp::_sym_signal_interrupt->fboundp()) {
    core::eval::funcall(mp::_sym_signal_interrupt->symbolFunction(),
                        signal_code);
  }
  // otherwise we're really early,
  // but this is pretty low level so just silently ignore
}

void handle_signal_now(int);
static void handle_one_signal(sigset_t* pending, int signum) {
  if (sigismember(pending, signum)) {
    sigdelset(pending, signum);
    handle_signal_now(signum);
  }
}

// Enqueue a signal unless we're in a blocking call - in that case handle it now.
static void enqueue_or_handle_signal(int signo) {
  if (my_thread->blockingp())
    handle_signal_now(signo);
  else
    my_thread->enqueue_signal(signo);
}

static void handle_pending_signals() {
  my_thread->clear_pending_signals_p();
  sigset_t* pending = my_thread->pending_signals();
#define TRYSIG(NAME) handle_one_signal(pending, SIG##NAME)
  DO_ALL_SIGNALS(TRYSIG);
#undef TRYSIG
}

// Handle just interrupts and not signals. Used in the SIGCONT handler,
// which checks if interrupts are disabled itself.
static void handle_queued_interrupts() {
  // Check that the queue has actually been created.
  if (my_thread->interrupt_queue_validp()) {
    while (true) {
      core::T_sp i = my_thread->dequeue_interrupt();
      if (i.nilp()) break;
      handle_queued_interrupt(i);
    }
  }
}

// Do all the queued actions, emptying the queue -
// unless interrupts have been disabled.
template <> void handle_all_queued_interrupts<RuntimeStage>() {
  if (!interrupts_disabled_p()) {
    if (my_thread->pending_signals_p())
      handle_pending_signals();
    handle_queued_interrupts();
  }
}

DOCGROUP(clasp);
CL_DEFUN void core__check_pending_interrupts() { handle_all_queued_interrupts(); }

// HANDLERS

// This is both a signal handler and called by signal handlers.
void handle_signal_now(int sig) {
  // calls mp:posix-interrupt.
  mp::posix_signal_interrupt(sig);
}

// This is both a signal handler and called by signal handlers.
void handle_SIGUSR1(int sig) { global_user_signal = true; }

// This handler is a bit special because we use SIGCONT to interrupt threads
// that are blocked on a syscall or whatever.
void handle_SIGCONT(int sig) {
  if (my_thread->blockingp() && !interrupts_disabled_p()) {
    // Disable (most) async interrupts as we call user code.
    my_thread->set_blockingp(false);
    handle_queued_interrupts();
    handle_signal_now(sig); // pass on the SIGCONT, in case someone cares
    // Nothing jumped out of here, so we're back to blocking.
    my_thread->set_blockingp(true);
  } else my_thread->enqueue_signal(sig);
}

void setup_user_signal() { signal(SIGUSR1, handle_SIGUSR1); }

void wait_for_user_signal(const char* message) {
  printf("%s:%d:%s\n"
         "Paused for SIGUSR1 pid is %d\n"
         "%s\n",
         __FILE__, __LINE__, __FUNCTION__, getpid(), message);
  if (std::getenv("CLASP_EXIT_ON_WAIT_FOR_USER_SIGNAL")) {
    exit(1);
  }
  while (!global_user_signal) pause();
  printf("%s:%d:%s Received SIGUSR1\n", __FILE__, __LINE__, __FUNCTION__);
  global_user_signal = false;
}

CL_DEFUN void gctools__wait_for_user_signal(const std::string& msg) { wait_for_user_signal(msg.c_str()); }

// false == SIGABRT invokes debugger, true == terminate (used in core__exit)
bool global_debuggerOnSIGABRT = true;

DOCGROUP(clasp);
CL_DEFUN int core__fe_enable_except(int ex) {
  feclearexcept(FE_ALL_EXCEPT);
  int prev = feenableexcept(ex);
  _lisp->setTrapFpeBits(fegetexcept());
  return prev;
}

DOCGROUP(clasp);
CL_DEFUN int core__fe_disable_except(int ex) {
  feclearexcept(FE_ALL_EXCEPT);
  int prev = fedisableexcept(ex);
  _lisp->setTrapFpeBits(fegetexcept());
  return prev;
}

DOCGROUP(clasp);
CL_DEFUN int core__fe_get_except() {
  return fegetexcept();
}

DOCGROUP(clasp);
CL_DEFUN int core__fe_restore_except(int ex) {
  feclearexcept(FE_ALL_EXCEPT);
  int prev = feenableexcept(ex);
  fedisableexcept(~ex);
  _lisp->setTrapFpeBits(fegetexcept());
  return prev;
}

void handle_fpe(int signo, siginfo_t* info, void* context) {
  (void)context; // unused
  if (_lisp) {
    // If _lisp has started then restore the traps that existed before the SIGFPE. This is needed on at least amd64 because the
    // masked traps are reset before SIGFPE is raised.
    feenableexcept(_lisp->getTrapFpeBits());
    fedisableexcept(~_lisp->getTrapFpeBits());
  }

  // TODO: Get operation and operands when possible.
  // Probably off the call stack.
  switch (info->si_code) {
#ifdef _TARGET_OS_DARWIN
  case FPE_NOOP:
    ARITHMETIC_ERROR(nil<core::T_O>(), nil<core::T_O>());
#endif
  case FPE_INTDIV:
    DIVISION_BY_ZERO(nil<core::T_O>(), nil<core::T_O>());
  case FPE_INTOVF:
    ARITHMETIC_ERROR(nil<core::T_O>(), nil<core::T_O>());
  case FPE_FLTDIV:
    DIVISION_BY_ZERO(nil<core::T_O>(), nil<core::T_O>());
  case FPE_FLTOVF:
    FLOATING_POINT_OVERFLOW(nil<core::T_O>(), nil<core::T_O>());
  case FPE_FLTUND:
    FLOATING_POINT_UNDERFLOW(nil<core::T_O>(), nil<core::T_O>());
  case FPE_FLTRES:
    FLOATING_POINT_INEXACT(nil<core::T_O>(), nil<core::T_O>());
  case FPE_FLTINV:
    FLOATING_POINT_INVALID_OPERATION(nil<core::T_O>(), nil<core::T_O>());
  case FPE_FLTSUB:
    ARITHMETIC_ERROR(nil<core::T_O>(), nil<core::T_O>());
  default: // FIXME: signal a better error.
    // Can end up here with e.g. SI_USER if it originated from kill
    enqueue_or_handle_signal(signo);
  }
}

#ifdef CLASP_APPLE_SILICON
void handle_ill(int signo, siginfo_t* info, void* context) {
  int esr;
  if (info->si_code == ILL_ILLTRP && ((esr = static_cast<ucontext_t*>(context)->uc_mcontext->__es.__esr) >> 26 & 0x3f) == 0x2C) {
    if (esr & FE_INEXACT) {
      FLOATING_POINT_INEXACT(nil<core::T_O>(), nil<core::T_O>());
    } else if (esr & FE_UNDERFLOW) {
      FLOATING_POINT_UNDERFLOW(nil<core::T_O>(), nil<core::T_O>());
    } else if (esr & FE_OVERFLOW) {
      FLOATING_POINT_OVERFLOW(nil<core::T_O>(), nil<core::T_O>());
    } else if (esr & FE_DIVBYZERO) {
      DIVISION_BY_ZERO(nil<core::T_O>(), nil<core::T_O>());
    } else if (esr & FE_INVALID) {
      FLOATING_POINT_INVALID_OPERATION(nil<core::T_O>(), nil<core::T_O>());
    }
  } else
    enqueue_or_handle_signal(signo);
}
#endif

void handle_segv(int signo, siginfo_t* info, void* context) {
  (void)context; // unused
  core::eval::funcall(ext::_sym_segmentation_violation, core::Integer_O::create((uintptr_t)(info->si_addr)));
}

void handle_bus(int signo, siginfo_t* info, void* context) {
  (void)context;
  core::eval::funcall(ext::_sym_bus_error, core::Integer_O::create((uintptr_t)(info->si_addr)));
}

void fatal_error_handler(void* user_data, const char* reason, bool gen_crash_diag) {
  printf("%s:%d Hit a fatal error in llvm: %s\n", __FILE__, __LINE__, reason);
  printf("Clasp is sleeping for 1000 seconds in case you want to connect in with the debugger - after which it will abort().\n");
  printf("    Clasp pid -> %d\n", getpid());
  sleep(1000);
  abort();
}

// SIGNALS INITIALIZATION

void initialize_signals() {
#define INIT_SIGNAL(sig, flags, handler)                                                                                           \
  new_action.sa_handler = handler;                                                                                                 \
  sigemptyset(&new_action.sa_mask);                                                                                                \
  new_action.sa_flags = flags;                                                                                                     \
  if (sigaction(sig, &new_action, NULL) != 0)                                                                                      \
    printf("failed to register " #sig " signal-handler with kernel error: %s\n", strerror(errno));

  // identical but with a sigaction. CLEANUP
#define INIT_SIGNALI(sig, flags, handler)                                                                                          \
  new_action.sa_sigaction = handler;                                                                                               \
  sigemptyset(&new_action.sa_mask);                                                                                                \
  new_action.sa_flags = SA_SIGINFO | (flags);                                                                                      \
  if (sigaction(sig, &new_action, NULL) != 0)                                                                                      \
    printf("failed to register " #sig " signal-handler with kernel error: %s\n", strerror(errno));

  struct sigaction new_action;

  // NOTE that for most signals we specify SA_NODEFER. This is because
  // the handlers often signal errors (in handle_signal_now), and if they
  // do a restart could wrest control from the handler back to normal
  // Lisp code. If that happens, and a signal is received again, we want
  // to deal with it the same way - not defer.

  INIT_SIGNAL(SIGINT, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
#ifdef SIGINFO
  INIT_SIGNAL(SIGINFO, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
#endif
  if (!getenv("CLASP_DONT_HANDLE_CRASH_SIGNALS")) {
    INIT_SIGNAL(SIGABRT, (SA_NODEFER | SA_RESTART), handle_signal_now);
    INIT_SIGNALI(SIGSEGV, (SA_NODEFER | SA_RESTART | SA_ONSTACK), handle_segv);
    INIT_SIGNALI(SIGBUS, (SA_NODEFER | SA_RESTART), handle_bus);
  }
  INIT_SIGNALI(SIGFPE, (SA_NODEFER | SA_RESTART), handle_fpe);
#ifdef CLASP_APPLE_SILICON
  INIT_SIGNALI(SIGILL, (SA_NODEFER | SA_RESTART), handle_ill);
#else
  INIT_SIGNAL(SIGILL, (SA_NODEFER | SA_RESTART), handle_signal_now);
#endif
  // Handle all signals that would terminate clasp (and can be caught)
  INIT_SIGNAL(SIGPIPE, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  INIT_SIGNAL(SIGALRM, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  INIT_SIGNAL(SIGTTIN, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  INIT_SIGNAL(SIGTTOU, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  INIT_SIGNAL(SIGPROF, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  INIT_SIGNAL(SIGUSR1, (SA_NODEFER | SA_RESTART), handle_SIGUSR1);
  INIT_SIGNAL(SIGUSR2, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  INIT_SIGNAL(SIGSYS, (SA_NODEFER | SA_RESTART), handle_signal_now); // can be signaled synchronously by bad syscall
  INIT_SIGNAL(SIGTRAP, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  // These termination signals we respond to when we're good and ready.
  INIT_SIGNAL(SIGTERM, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  INIT_SIGNAL(SIGQUIT, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  INIT_SIGNAL(SIGHUP, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
#ifdef SIGVTALRM
  INIT_SIGNAL(SIGVTALRM, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
#endif
  INIT_SIGNAL(SIGURG, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  // SIGXCPU is used by boehm to stop threads - this causes problems with boehm in the precise mode
#if !(defined(USE_BOEHM) && defined(USE_PRECISE_GC))
  INIT_SIGNAL(SIGXCPU, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
#endif
  INIT_SIGNAL(SIGXFSZ, (SA_NODEFER | SA_RESTART), handle_signal_now); // signaled synchronously by some syscalls
//  INIT_SIGNAL(SIGWINCH, (SA_NODEFER | SA_RESTART), enqueue_or_handle_signal);
  // This one we use specially to wake up blocking threads.
  INIT_SIGNAL(SIGCONT, (SA_NODEFER | SA_RESTART), handle_SIGCONT);

  llvm::install_fatal_error_handler(fatal_error_handler, NULL);
}

CL_LAMBDA();
CL_DOCSTRING(R"dx(Get alist of Signal-name . Signal-code alist of known signal (Posix + extra))dx");
DOCGROUP(clasp);
CL_DEFUN core::List_sp core__signal_code_alist() {
  core::List_sp alist = nil<core::T_O>();
#define DEFSIG(NAME) \
  alist = core::Cons_O::create(core::Cons_O::create(core::_sym_SIG##NAME, core::clasp_make_fixnum(SIG##NAME)), alist);
  DO_ALL_SIGNALS(DEFSIG);
#undef DEFSIG
  return alist;
}

#undef DOSIGTHR
#undef DOSIGPWR
#undef DOSIGIO
#undef DOSIGEMT
#undef DOSIGPROF
#undef DOSIGPOLL
#undef DOSIGXFSZ
#undef DOSIGXCPU
#undef DOSIGVTALRM
#undef DOSIGTRAP
#undef DOSIGSYS
#undef DO_ALL_SIGNALS

}; // namespace gctools
