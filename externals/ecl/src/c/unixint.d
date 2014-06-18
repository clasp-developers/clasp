/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    unixint.c -- Unix interrupt interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/**********************************************************************
 * HOW WE HANDLE SIGNALS AND EXCEPTIONS
 *
 * (the following should be correlated with the manual)
 *
 * POSIX contemplates the notion of "signals", which are events that
 * cause a process or a thread to be interrupted. Windows uses the
 * term "exception", which includes also a more general kind of
 * errors.
 *
 * In both cases the consequence is that a thread or process may be
 * interrupted at any time, either by causes which are intrinsic to
 * them (synchronous signals), such as floating point exceptions, or
 * extrinsic (asynchronous signals), such as the process being aborted
 * by the user.
 * 
 * Of course, those interruptions are not always welcome. When the
 * interrupt is delivered and a handler is invoked, the thread or even
 * the whole program may be in an inconsistent state. For instance the
 * thread may have acquired a lock, or it may be in the process of
 * filling the fields of a structure. Understanding this POSIX
 * restricts severely what functions can be called from a signal
 * handler, thereby limiting its usefulness.
 *
 * There is a simple solution, which ECL uses, and which is to mark
 * sections of code which are interruptible, and in which it is safe
 * for the handler to run arbitrary code, protect anything else. In
 * principle this "marking" can be done using POSIX functions such as
 * pthread_sigmask() or sigprocmask().
 *
 * However in practice this is slow, as it involves at least a
 * function call, resolving thread-local variables, etc, etc, and it
 * will not work in Windows. Furthermore, sometimes we want signals to
 * be detected but not to be immediately processed. For instance, when
 * reading from the terminal we want to be able to interrupt the
 * process, but we can not execute the code from the handler, since
 * read() may leave the input stream in an inconsistent, or even
 * locked state.
 *
 * Our approach is slightly different: we install our own signal
 * hander which reads a single, thread-local variable stored in the
 * ecl_process_env()->disable_interrupts. If the variable marks that
 * signals should be postponed, then the information about the signal
 * is queued. Otherwise the appropriate code is executed: for instance
 * invoking the debugger, jumping to a condition handler, quitting,
 * etc.
 */

#ifdef __sun__
/* For SA_SIGINFO in Solaris. We could have used _XOPEN_SOURCE=600, but
 * this requires C99 and the default GCC for Solaris (3.4.3) does not
 * support this C standard. */
# define __EXTENSIONS__
#endif
#include <errno.h>
#include <string.h>
#include <stdio.h>
/* To get APCProc calls */
#define _WIN32_WINNT 0x400
#include <signal.h>

#if defined(_MSC_VER) || defined(__MINGW32__)
# include <windows.h>
#endif
#if !defined(_MSC_VER)
# include <unistd.h>
#endif

#include <ecl/ecl.h>

#ifdef ECL_USE_MPROTECT
# ifndef SA_SIGINFO
#  error "We cannot use the mmap code without siginfo"
# endif
# include <sys/mman.h>
#endif
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>
#include <ecl/impl/math_fenv.h>

static struct {
	int code;
	char *name;
	cl_object handler;
} known_signals[] = {
#ifdef SIGHUP
	{ SIGHUP, "+SIGHUP+", ECL_NIL},
#endif
#ifdef SIGINT
	{ SIGINT, "+SIGINT+", @'si::terminal-interrupt'},
#endif
#ifdef SIGQUIT
	{ SIGQUIT, "+SIGQUIT+", ECL_NIL},
#endif
#ifdef SIGILL
	{ SIGILL, "+SIGILL+", @'ext::illegal-instruction'},
#endif
#ifdef SIGTRAP
	{ SIGTRAP, "+SIGTRAP+", ECL_NIL},
#endif
#ifdef SIGABRT
	{ SIGABRT, "+SIGABRT+", ECL_NIL},
#endif
#ifdef SIGEMT
	{ SIGEMT, "+SIGEMT+", ECL_NIL},
#endif
#ifdef SIGFPE
	{ SIGFPE, "+SIGFPE+", ECL_NIL},
#endif
#ifdef SIGKILL
	{ SIGKILL, "+SIGKILL+", ECL_NIL},
#endif
#ifdef SIGBUS
	{ SIGBUS, "+SIGBUS+", @'ext::segmentation-violation'},
#endif
#ifdef SIGSEGV
	{ SIGSEGV, "+SIGSEGV+", @'ext::segmentation-violation'},
#endif
#ifdef SIGSYS
	{ SIGSYS, "+SIGSYS+", ECL_NIL},
#endif
#ifdef SIGPIPE
	{ SIGPIPE, "+SIGPIPE+", ECL_NIL},
#endif
#ifdef SIGALRM
	{ SIGALRM, "+SIGALRM+", ECL_NIL},
#endif
#ifdef SIGTERM
	{ SIGTERM, "+SIGTERM+", ECL_NIL},
#endif
#ifdef SIGURG
	{ SIGURG, "+SIGURG+", ECL_NIL},
#endif
#ifdef SIGSTOP
	{ SIGSTOP, "+SIGSTOP+", ECL_NIL},
#endif
#ifdef SIGTSTP
	{ SIGTSTP, "+SIGTSTP+", ECL_NIL},
#endif
#ifdef SIGCONT
	{ SIGCONT, "+SIGCONT+", ECL_NIL},
#endif
#ifdef SIGCHLD
	{ SIGCHLD, "+SIGCHLD+", @'si::wait-for-all-processes'},
#endif
#ifdef SIGTTIN
	{ SIGTTIN, "+SIGTTIN+", ECL_NIL},
#endif
#ifdef SIGTTOU
	{ SIGTTOU, "+SIGTTOU+", ECL_NIL},
#endif
#ifdef SIGIO
	{ SIGIO, "+SIGIO+", ECL_NIL},
#endif
#ifdef SIGXCPU
	{ SIGXCPU, "+SIGXCPU+", ECL_NIL},
#endif
#ifdef SIGXFSZ
	{ SIGXFSZ, "+SIGXFSZ+", ECL_NIL},
#endif
#ifdef SIGVTALRM
	{ SIGVTALRM, "+SIGVTALRM+", ECL_NIL},
#endif
#ifdef SIGPROF
	{ SIGPROF, "+SIGPROF+", ECL_NIL},
#endif
#ifdef SIGWINCH
	{ SIGWINCH, "+SIGWINCH+", ECL_NIL},
#endif
#ifdef SIGINFO
	{ SIGINFO, "+SIGINFO+", ECL_NIL},
#endif
#ifdef SIGUSR1
	{ SIGUSR1, "+SIGUSR1+", ECL_NIL},
#endif
#ifdef SIGUSR2
	{ SIGUSR2, "+SIGUSR2+", ECL_NIL},
#endif
#ifdef SIGTHR
	{ SIGTHR, "+SIGTHR+", ECL_NIL},
#endif
	{ -1, "", ECL_NIL }
};

#ifdef HAVE_SIGPROCMASK
static sigset_t main_thread_sigmask;
# define handler_fn_prototype(name, sig, info, aux) name(sig, info, aux)
# define call_handler(name, sig, info, aux) name(sig, info, aux)
# define reinstall_signal(x,y)
# define copy_siginfo(x,y) memcpy(x, y, sizeof(struct sigaction))
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
#else /* HAVE_SIGPROCMASK */
# define handler_fn_prototype(name, sig, info, aux) name(sig)
# define call_handler(name, sig, info, aux) name(sig)
# define mysignal(x,y) signal(x,y)
# define reinstall_signal(x,y) signal(x,y)
# define copy_siginfo(x,y)
#endif

static bool
zombie_process(cl_env_ptr the_env)
{
#ifdef ECL_THREADS
	if (the_env == NULL) {
		return 1;
	} else {
		/* When we are exiting a thread, we simply ignore all signals. */
		cl_object process = the_env->own_process;
		return (process->process.phase == ECL_PROCESS_INACTIVE);
	}
#else
	return !the_env;
#endif
}

static ECL_INLINE bool
interrupts_disabled_by_C(cl_env_ptr the_env)
{
	return the_env->disable_interrupts;
}

static ECL_INLINE bool
interrupts_disabled_by_lisp(cl_env_ptr the_env)
{
	return !ecl_option_values[ECL_OPT_BOOTED] ||
		Null(ECL_SYM_VAL(the_env, @'ext::*interrupts-enabled*'));
}

static void early_signal_error() ecl_attr_noreturn;

static void
early_signal_error()
{
	ecl_internal_error("Got signal before environment was installed"
			   " on our thread");
}

static void illegal_signal_code(cl_object code) ecl_attr_noreturn;

static void
illegal_signal_code(cl_object code)
{
	FEerror("Unknown signal code: ~D", 1, code);
}

/* On platforms in which mprotect() works, we block all write access
 * to the environment for a cheap check of pending interrupts. On
 * other platforms we change the value of disable_interrupts to 3, so
 * that we detect changes. */
static ECL_INLINE void
set_guard_page(cl_env_ptr the_env)
{
#if defined(ECL_USE_MPROTECT)
	if (mprotect(the_env, sizeof(*the_env), PROT_READ) < 0) {
		ecl_internal_error("Unable to mprotect environment.");
	}
#elif defined(ECL_USE_GUARD_PAGE)
	if (!VirtualProtect(the_env, sizeof(*the_env), PAGE_GUARD, NULL)) {
		ecl_internal_error("Unable to mprotect environment.");
	}
#endif
}

static cl_object pop_signal(cl_env_ptr env);

#define unblock_signal(env, sig)
#ifdef HAVE_SIGPROCMASK
# undef unblock_signal
static void
unblock_signal(cl_env_ptr the_env, int signal)
{
	/*
	 * We do not really "unblock" the signal, but rather restore
	 * ECL's default sigmask.
	 */
# ifdef ECL_THREADS
	pthread_sigmask(SIG_SETMASK, the_env->default_sigmask, NULL);
# else
	sigprocmask(SIG_SETMASK, the_env->default_sigmask, NULL);
# endif
}
#endif

ecl_def_ct_base_string(str_ignore_signal,"Ignore signal",13,static,const);

static void
handle_signal_now(cl_object signal_code, cl_object process)
{
        switch (ecl_t_of(signal_code)) {
        case t_fixnum:
                cl_cerror(4, str_ignore_signal, @'ext::unix-signal-received',
                          @':code', signal_code);
                break;
        case t_symbol:
		/*
		 * When we bind a handler to a signal, it may either
		 * be a function, a symbol denoting a function or
		 * a symbol denoting a condition.
		 */
		if (cl_find_class(2, signal_code, ECL_NIL) != ECL_NIL)
			cl_cerror(2, str_ignore_signal, signal_code);
#ifdef ECL_THREADS
		else if (!Null(process))
			_ecl_funcall3(signal_code, @':process', process);
#endif
		else
			_ecl_funcall1(signal_code);
                break;
        case t_cfun:
        case t_cfunfixed:
        case t_cclosure:
        case t_bytecodes:
        case t_bclosure:
                _ecl_funcall1(signal_code);
        default:
                break;
        }
}

cl_object
si_handle_signal(cl_object signal_code, cl_object process)
{
	handle_signal_now(signal_code, process);
	@(return)
}

static void
handle_all_queued(cl_env_ptr env)
{
	while (env->pending_interrupt != ECL_NIL) {
		handle_signal_now(pop_signal(env), env->own_process);
	}
}

static void
queue_signal(cl_env_ptr env, cl_object code, int allocate)
{
	ECL_WITH_SPINLOCK_BEGIN(env, &env->signal_queue_spinlock) {
		cl_object record;
		if (allocate) {
			record = ecl_list1(ECL_NIL);
		} else {
			record = env->signal_queue;
			if (record != ECL_NIL) {
				env->signal_queue = ECL_CONS_CDR(record);
			}
		}
		if (record != ECL_NIL) {
			ECL_RPLACA(record, code);
			env->pending_interrupt =
				ecl_nconc(env->pending_interrupt,
					  record);
		}
	} ECL_WITH_SPINLOCK_END;
}

static cl_object
pop_signal(cl_env_ptr env)
{
	cl_object record, value;
	if (env->pending_interrupt == ECL_NIL) {
		return ECL_NIL;
	}
	ECL_WITH_SPINLOCK_BEGIN(env, &env->signal_queue_spinlock) {
		record = env->pending_interrupt;
		value = ECL_CONS_CAR(record);
		env->pending_interrupt = ECL_CONS_CDR(record);
		/* Save some conses for future use, to avoid allocating */
		if (ECL_SYMBOLP(value) || ECL_FIXNUMP(value)) {
			ECL_RPLACD(record, env->signal_queue);
			env->signal_queue = record;
		}
	} ECL_WITH_SPINLOCK_END;
	return value;
}

static void
handle_or_queue(cl_env_ptr the_env, cl_object signal_code, int code)
{
        if (Null(signal_code) || signal_code == NULL)
                return;
	/*
	 * If interrupts are disabled by lisp we are not so eager on
	 * detecting when the interrupts become enabled again. We
	 * queue the signal and are done with that.
	 */
	if (interrupts_disabled_by_lisp(the_env)) {
		queue_signal(the_env, signal_code, 0);
	}
	/*
	 * If interrupts are disabled by C, and we have not pushed a
	 * pending signal, save this signal and return.
	 */
	else if (interrupts_disabled_by_C(the_env)) {
		the_env->disable_interrupts = 3;
		queue_signal(the_env, signal_code, 0);
		set_guard_page(the_env);
	}
	/*
	 * If interrupts are enabled, that means we are in a safe area
	 * and may execute arbitrary lisp code. We can thus call the
	 * appropriate handlers.
	 */
	else {
                if (code) unblock_signal(the_env, code);
		si_trap_fpe(@'last', ECL_T); /* Clear FPE exception flag */
                handle_signal_now(signal_code, the_env->own_process);
        }
}

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

static void
handler_fn_prototype(evil_signal_handler, int sig, siginfo_t *siginfo, void *data)
{
        int old_errno = errno;
	cl_env_ptr the_env;
        cl_object signal_object;
	reinstall_signal(sig, evil_signal_handler);
        /* The lisp environment might not be installed. */
        the_env = ecl_process_env();
        unlikely_if (zombie_process(the_env))
                return;
        signal_object = ecl_gethash_safe(ecl_make_fixnum(sig),
					 cl_core.known_signals,
					 ECL_NIL);
        handle_signal_now(signal_object, the_env->own_process);
        errno = old_errno;
}

#if defined(ECL_THREADS) && defined(HAVE_SIGPROCMASK)
typedef struct {
	cl_object process;
	int signo;
} signal_thread_message;
static cl_object signal_thread_process = ECL_NIL;
static signal_thread_message signal_thread_msg;
static cl_object signal_thread_spinlock = ECL_NIL;
static int signal_thread_pipe[2] = {-1,-1};

static void
handler_fn_prototype(deferred_signal_handler, int sig, siginfo_t *siginfo, void *data)
{
        int old_errno = errno;
	cl_env_ptr the_env;
	signal_thread_message msg;
	reinstall_signal(sig, deferred_signal_handler);
        /* The lisp environment might not be installed. */
        the_env = ecl_process_env();
        unlikely_if (zombie_process(the_env))
                return;
	msg.signo = sig;
	msg.process = the_env->own_process;
	if (msg.process == signal_thread_process) {
		/* The signal handling thread may also receive signals. In
		 * this case we do not use the pipe, but just copy the message
		 * Note that read() will abort the thread will get notified. */
		signal_thread_msg = msg;
	} else if (signal_thread_pipe[1] > 0) {
		ecl_get_spinlock(the_env, &signal_thread_spinlock);
		write(signal_thread_pipe[1], &msg, sizeof(msg));
		ecl_giveup_spinlock(&signal_thread_spinlock);
	} else {
		/* Nothing to do. There is no way to handle this signal because
		 * the responsible thread is not running */
	}
        errno = old_errno;
}

static cl_object
asynchronous_signal_servicing_thread()
{
	const cl_env_ptr the_env = ecl_process_env();
	int interrupt_signal = -1;
	/*
	 * We block all signals except the usual interrupt thread.
	 */
	{
		sigset_t handled_set;
		sigfillset(&handled_set);
		if (ecl_option_values[ECL_OPT_TRAP_INTERRUPT_SIGNAL]) {
			interrupt_signal =
				ecl_option_values[ECL_OPT_THREAD_INTERRUPT_SIGNAL];
			sigdelset(&handled_set, interrupt_signal);
		}
		pthread_sigmask(SIG_BLOCK, &handled_set, NULL);
	}
	/*
	 * We create the object for communication. We need a lock to prevent other
	 * threads from writing before the pipe is created.
	 */
	ecl_get_spinlock(the_env, &signal_thread_spinlock);
	pipe(signal_thread_pipe);
	ecl_giveup_spinlock(&signal_thread_spinlock);
	signal_thread_msg.process = ECL_NIL;
	for (;;) {
		cl_object signal_code;
		signal_thread_msg.process = ECL_NIL;
		if (read(signal_thread_pipe[0], &signal_thread_msg,
			 sizeof(signal_thread_msg)) < 0)
		{
			/* Either the pipe errs or we have received an interrupt
			 * from a different thread */
			if (errno != EINTR ||
			    signal_thread_msg.process != the_env->own_process)
				break;
		}
		/* We have queued ourselves an interrupt event */
		if (signal_thread_msg.signo == interrupt_signal &&
		    signal_thread_msg.process == the_env->own_process) {
			break;
		}
#ifdef SIGCHLD
		if (signal_thread_msg.signo == SIGCHLD) {
			si_wait_for_all_processes(0);
			continue;
		}
#endif
		signal_code = ecl_gethash_safe(ecl_make_fixnum(signal_thread_msg.signo),
					       cl_core.known_signals,
					       ECL_NIL);
		if (!Null(signal_code)) {
			mp_process_run_function(4, @'si::handle-signal',
						@'si::handle-signal',
						signal_code,
						signal_thread_msg.process);
		}
	}
# if defined(ECL_USE_MPROTECT)
	/* We might have protected our own environment */
	mprotect(the_env, sizeof(*the_env), PROT_READ | PROT_WRITE);
# endif /* ECL_USE_MPROTECT */
	close(signal_thread_pipe[0]);
	close(signal_thread_pipe[1]);
	ecl_return0(the_env);
}
#endif /* ECL_THREADS && !ECL_MS_WINDOWS_HOST */

#if defined(ECL_THREADS) && !defined(ECL_MS_WINDOWS_HOST)
static void
handler_fn_prototype(process_interrupt_handler, int sig, siginfo_t *siginfo, void *data)
{
        int old_errno = errno;
	cl_env_ptr the_env;
	reinstall_signal(sig, process_interrupt_handler);
        /* The lisp environment might not be installed. */
        the_env = ecl_process_env();
        if (zombie_process(the_env))
                return;
	if (!Null(the_env->pending_interrupt)) {
		if (interrupts_disabled_by_C(the_env)) {
			set_guard_page(the_env);
		} else if (!interrupts_disabled_by_lisp(the_env)) {
			unblock_signal(the_env, sig);
			handle_all_queued(the_env);
		}
	}
        errno = old_errno;
}
#endif /* ECL_THREADS && !ECL_MS_WINDOWS_HOST */

static void
handler_fn_prototype(fpe_signal_handler, int sig, siginfo_t *info, void *data)
{
        int code;
	cl_object condition;
	cl_env_ptr the_env;
	reinstall_signal(sig, fpe_signal_handler);
        /* The lisp environment might not be installed. */
	unlikely_if (!ecl_option_values[ECL_OPT_BOOTED]) {
		early_signal_error();
	}
        the_env = ecl_process_env();
        unlikely_if (zombie_process(the_env))
                return;
	condition = @'arithmetic-error';
	code = 0;
#ifdef _MSC_VER
	switch (_fpecode) {
	case _FPE_INVALID:
		condition = @'floating-point-invalid-operation';
		code = FE_INVALID;
		break;
	case _FPE_OVERFLOW:
		condition = @'floating-point-overflow';
		code = FE_OVERFLOW;
		break;
	case _FPE_UNDERFLOW:
		condition = @'floating-point-underflow';
		code = FE_UNDERFLOW;
		break;
	case _FPE_ZERODIVIDE:
		condition = @'division-by-zero';
		code = FE_DIVBYZERO;
		break;
	}
#else /* !_MSC_VER */
# if defined(HAVE_FENV_H) & !defined(ECL_AVOID_FENV_H)
	code = fetestexcept(FE_ALL_EXCEPT);
	if (code & FE_DIVBYZERO) {
		condition = @'division-by-zero';
		code = FE_DIVBYZERO;
	} else if (code & FE_INVALID) {
		condition = @'floating-point-invalid-operation';
		code = FE_INVALID;
	} else if (code & FE_OVERFLOW) {
		condition = @'floating-point-overflow';
		code = FE_OVERFLOW;
	} else if (code & FE_UNDERFLOW) {
		condition = @'floating-point-underflow';
		code = FE_UNDERFLOW;
	} else if (code & FE_INEXACT) {
		condition = @'floating-point-inexact';
		code = FE_INEXACT;
	}
	feclearexcept(FE_ALL_EXCEPT);
# endif
#endif /* !_MSC_VER */
#ifdef SA_SIGINFO
	if (info) {
		if (info->si_code == FPE_INTDIV || info->si_code == FPE_FLTDIV) {
			condition = @'division-by-zero';
			code = FE_DIVBYZERO;
		} else if (info->si_code == FPE_FLTOVF) {
			condition = @'floating-point-overflow';
			code = FE_OVERFLOW;
		} else if (info->si_code == FPE_FLTUND) {
			condition = @'floating-point-underflow';
			code = FE_UNDERFLOW;
		} else if (info->si_code == FPE_FLTRES) {
			condition = @'floating-point-inexact';
			code = FE_INEXACT;
		} else if (info->si_code == FPE_FLTINV) {
			condition = @'floating-point-invalid-operation';
			code = FE_INVALID;
		}
	}
#endif /* SA_SIGINFO */
	/*
	  if (code && !(code & the_env->trap_fpe_bits))
	  condition = ECL_NIL;
	*/
	si_trap_fpe(@'last', ECL_T); /* Clear FPE exception flag */
	unblock_signal(the_env, code);
	handle_signal_now(condition, the_env->own_process);
	/* We will not reach past this point. */
}

static void
handler_fn_prototype(sigsegv_handler, int sig, siginfo_t *info, void *aux)
{
	int old_errno = errno;
        static const char *stack_overflow_msg =
                "\n;;;\n;;; Stack overflow.\n"
                ";;; Jumping to the outermost toplevel prompt\n"
                ";;;\n\n";
        static const char *segv_msg =
                "\n;;;\n"
                ";;; Detected access to protected memory, "
                "also kwown as 'bus or segmentation fault'.\n"
                ";;; Jumping to the outermost toplevel prompt\n"
                ";;;\n\n";
	cl_env_ptr the_env;
	reinstall_signal(sig, sigsegv_handler);
        /* The lisp environment might not be installed. */
	unlikely_if (!ecl_option_values[ECL_OPT_BOOTED]) {
		early_signal_error();
	}
	the_env = ecl_process_env();
	unlikely_if (zombie_process(the_env))
		return;
#if defined(SA_SIGINFO)
# if defined(ECL_USE_MPROTECT)
	/* We access the environment when it was protected. That
	 * means there was a pending signal. */
	if (((char*)the_env <= (char*)info->si_addr) &&
            ((char*)info->si_addr <= (char*)(the_env+1)))
        {
		mprotect(the_env, sizeof(*the_env), PROT_READ | PROT_WRITE);
                the_env->disable_interrupts = 0;
                unblock_signal(the_env, sig);
		handle_all_queued(the_env);
                return;
	}
# endif /* ECL_USE_MPROTECT */
# ifdef ECL_DOWN_STACK
	if (sig == SIGSEGV &&
	    (char*)info->si_addr > the_env->cs_barrier &&
	    (char*)info->si_addr <= the_env->cs_org) {
                unblock_signal(the_env, sig);
		ecl_unrecoverable_error(the_env, stack_overflow_msg);
                return;
	}
# else
	if (sig == SIGSEGV &&
	    (char*)info->si_addr < the_env->cs_barrier &&
	    (char*)info->si_addr >= the_env->cs_org) {
                unblock_signal(the_env, sig);
		ecl_unrecoverable_error(the_env, stack_overflow_msg);
                return;
	}
# endif /* ECL_DOWN_STACK */
	/* Do not attempt an error handler if we nest two serious 
	 * errors in the same thread */
	if (the_env->fault_address == info->si_addr) {
		the_env->fault_address = info->si_addr;
		unblock_signal(the_env, sig);
		ecl_unrecoverable_error(the_env, segv_msg);
	} else {
		the_env->fault_address = info->si_addr;
		handle_or_queue(the_env, @'ext::segmentation-violation', sig);
	}
#else
	/*
	 * We cannot distinguish between a stack overflow and a simple
	 * access violation. Thus we assume the worst case and jump to
	 * the outermost handler.
	 */
        unblock_signal(the_env, sig);
	ecl_unrecoverable_error(the_env, segv_msg);
#endif /* SA_SIGINFO */
	errno = old_errno;
}

cl_object
si_check_pending_interrupts(void)
{
	handle_all_queued(ecl_process_env());
	@(return)
}

void
ecl_check_pending_interrupts(cl_env_ptr env)
{
	handle_all_queued(env);
}

static cl_object
do_catch_signal(int code, cl_object action, cl_object process)
{
        if (action == ECL_NIL || action == @':ignore') {
                mysignal(code, SIG_IGN);
                return ECL_T;
        } else if (action == @':default') {
                mysignal(code, SIG_DFL);
                return ECL_T;
        } else if (action == @':mask' || action == @':unmask') {
#ifdef HAVE_SIGPROCMASK
# ifdef ECL_THREADS
		/* When a process object is supplied, the changes take care
		 * on the process structure and will only take effect when
		 * the process is enabled. */
		if (ecl_t_of(process) == t_process) {
			cl_env_ptr env = process->process.env;
			sigset_t *handled_set = (sigset_t *)env->default_sigmask;
			if (action == @':mask') {
				sigaddset(handled_set, code);
			} else {
				sigdelset(handled_set, code);
			}
			return ECL_T;
		} else {
			sigset_t handled_set;
			pthread_sigmask(SIG_SETMASK, NULL, &handled_set);
			if (action == @':mask') {
				sigaddset(&handled_set, code);
			} else {
				sigdelset(&handled_set, code);
			}
			pthread_sigmask(SIG_SETMASK, &handled_set, NULL);
			return ECL_T;
		}
# else
		{
			sigset_t handled_set;
			sigprocmask(SIG_SETMASK, NULL, &handled_set);
			if (action == @':mask') {
				sigaddset(&handled_set, code);
			} else {
				sigdelset(&handled_set, code);
			}
			sigprocmask(SIG_SETMASK, &handled_set, NULL);
			return ECL_T;
		}
# endif /* !ECL_THREADS */
#else /* !HAVE_SIGPROCMASK */
		return ECL_NIL;
#endif /* !HAVE_SIGPROCMASK */
	} else if (action == ECL_T || action == @':catch') {
                if (code == SIGSEGV) {
                        mysignal(code, sigsegv_handler);
                }
#ifdef SIGBUS
                else if (code == SIGBUS) {
                        mysignal(code, sigsegv_handler);
                }
#endif
#ifdef SIGILL
		else if (code == SIGILL) {
			mysignal(SIGILL, evil_signal_handler);
		}
#endif
#if defined(SIGCHLD) && defined(ECL_THREADS)
                else if (code == SIGCHLD &&
			 ecl_option_values[ECL_OPT_SIGNAL_HANDLING_THREAD])
		{
			/* Do nothing. This is taken care of in
			 * the asynchronous signal handler. */
                }
#endif
                else {
                        mysignal(code, non_evil_signal_handler);
                }
		return ECL_T;
        } else {
		FEerror("Unknown 2nd argument to EXT:CATCH-SIGNAL: ~A", 1,
			action);
	}
}

cl_object
si_get_signal_handler(cl_object code)
{
	cl_object handler = ecl_gethash_safe(code, cl_core.known_signals, OBJNULL);
	unlikely_if (handler == OBJNULL) {
		illegal_signal_code(code);
	}
	@(return handler)
}

cl_object
si_set_signal_handler(cl_object code, cl_object handler)
{
	cl_object action = ecl_gethash_safe(code, cl_core.known_signals, OBJNULL);
	unlikely_if (action == OBJNULL) {
		illegal_signal_code(code);
	}
	ecl_sethash(code, cl_core.known_signals, handler);
	si_catch_signal(2, code, ECL_T);
	@(return handler)
}

@(defun ext::catch-signal (code flag &key process)
@
{
	int code_int;
	unlikely_if (ecl_gethash_safe(code, cl_core.known_signals, OBJNULL) == OBJNULL) {
		illegal_signal_code(code);
	}
	code_int = ecl_fixnum(code);
#ifdef GBC_BOEHM
# ifdef SIGSEGV
	unlikely_if ((code == ecl_make_fixnum(SIGSEGV)) &&
		     ecl_option_values[ECL_OPT_INCREMENTAL_GC])
		FEerror("It is not allowed to change the behavior of SIGSEGV.",
			0);
# endif
# ifdef SIGBUS
	unlikely_if (code_int == SIGBUS)
		FEerror("It is not allowed to change the behavior of SIGBUS.",
			0);
# endif
#endif
#if defined(ECL_THREADS) && !defined(ECL_MS_WINDOWS_HOST)
	unlikely_if (code_int == ecl_option_values[ECL_OPT_THREAD_INTERRUPT_SIGNAL]) {
		FEerror("It is not allowed to change the behavior of signal ~D", 1,
                        code);
	}
#endif
#ifdef SIGFPE
	unlikely_if (code_int == SIGFPE) {
		FEerror("The signal handler for SIGPFE cannot be uninstalled. Use SI:TRAP-FPE instead.", 0);
	}
#endif
	@(return do_catch_signal(code_int, flag, process));
}
@)

#ifdef ECL_THREADS
# ifdef ECL_WINDOWS_THREADS
static VOID CALLBACK
wakeup_function(ULONG_PTR foo)
{
	cl_env_ptr env = ecl_process_env();
	volatile i = env->nvalues;
	env->nvalues = i;
}

static VOID CALLBACK
wakeup_noop(ULONG_PTR foo)
{
}
# endif

static bool
do_interrupt_thread(cl_object process)
{
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
        int signal = ecl_option_values[ECL_OPT_THREAD_INTERRUPT_SIGNAL];
        if (pthread_kill(process->process.thread, signal)) {
		FElibc_error("Unable to interrupt process ~A", 1,
			     process);
	}
	return 1;
# endif
}

void
ecl_interrupt_process(cl_object process, cl_object function)
{
        /*
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
	if (!Null(function) &&
	    (process->process.phase >= ECL_PROCESS_BOOTING))
	{
		function = si_coerce_to_function(function);
		queue_signal(process->process.env, function, 1);
	}
	/* ... but only deliver if the process is still alive */
	if (process->process.phase == ECL_PROCESS_ACTIVE)
		do_interrupt_thread(process);
}

void
ecl_wakeup_process(cl_object process)
{
# ifdef ECL_WINDOWS_THREADS
        HANDLE thread = (HANDLE)process->process.thread;
	if (!QueueUserAPC(wakeup_noop, thread, 0)) {
		FEwin32_error("Unable to queue APC call to thread ~A",
			      1, process);
	}
# else
	do_interrupt_thread(process);
# endif
}
#endif /* ECL_THREADS */

#ifdef ECL_WINDOWS_THREADS
static LPTOP_LEVEL_EXCEPTION_FILTER old_W32_exception_filter = NULL;

LONG WINAPI
_ecl_w32_exception_filter(struct _EXCEPTION_POINTERS* ep)
{
	LONG excpt_result;
	cl_env_ptr the_env = ecl_process_env();

	excpt_result = EXCEPTION_CONTINUE_EXECUTION;
	switch (ep->ExceptionRecord->ExceptionCode)
	{
                /* Access to guard page */
        	case STATUS_GUARD_PAGE_VIOLATION: {
                        cl_object process = the_env->own_process;
                        if (!Null(process->process.interrupt)) {
                                cl_object signal = pop_signal(the_env);
                                process->process.interrupt = ECL_NIL;
                                while (signal != ECL_NIL && signal) {
                                        handle_signal_now(signal, the_env->own_process);
                                        signal = pop_signal(the_env);
                                }
                                return EXCEPTION_CONTINUE_EXECUTION;
                        }
                }
		/* Catch all arithmetic exceptions */
		case EXCEPTION_INT_DIVIDE_BY_ZERO:
			feclearexcept(FE_ALL_EXCEPT);
                        handle_signal_now(@'division-by-zero', the_env->own_process);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_INT_OVERFLOW:
			feclearexcept(FE_ALL_EXCEPT);
                        handle_signal_now(@'arithmetic-error', the_env->own_process);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_DIVIDE_BY_ZERO:
			feclearexcept(FE_ALL_EXCEPT);
                        handle_signal_now(@'floating-point-overflow', the_env->own_process);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_OVERFLOW:
			feclearexcept(FE_ALL_EXCEPT);
                        handle_signal_now(@'floating-point-overflow', the_env->own_process);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_UNDERFLOW:
			feclearexcept(FE_ALL_EXCEPT);
                        handle_signal_now(@'floating-point-underflow', the_env->own_process);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_INEXACT_RESULT:
			feclearexcept(FE_ALL_EXCEPT);
                        handle_signal_now(@'floating-point-inexact', the_env->own_process);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_DENORMAL_OPERAND:
		case EXCEPTION_FLT_INVALID_OPERATION:
			feclearexcept(FE_ALL_EXCEPT);
                        handle_signal_now(@'floating-point-invalid-operation', the_env->own_process);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_STACK_CHECK:
                        handle_signal_now(@'arithmetic-error', the_env->own_process);
                        return EXCEPTION_CONTINUE_EXECUTION;
		/* Catch segmentation fault */
		case EXCEPTION_ACCESS_VIOLATION:
                        handle_signal_now(@'ext::segmentation-violation', the_env->own_process);
                        return EXCEPTION_CONTINUE_EXECUTION;
		/* Catch illegal instruction */
		case EXCEPTION_ILLEGAL_INSTRUCTION:
			handle_signal_now(@'ext::illegal-instruction', the_env->own_process);
			return EXCEPTION_CONTINUE_EXECUTION;
		/* Do not catch anything else */
		default:
			excpt_result = EXCEPTION_CONTINUE_SEARCH;
			break;
	}
        if (old_W32_exception_filter)
                return old_W32_exception_filter(ep);
	return excpt_result;
}

static cl_object
W32_handle_in_new_thread(cl_object signal_code)
{
	int outside_ecl = ecl_import_current_thread(@'si::handle-signal', ECL_NIL);
	mp_process_run_function(4, @'si::handle-signal',
				@'si::handle-signal',
				signal_code, ECL_NIL);
	if (outside_ecl) ecl_release_current_thread();
}

BOOL WINAPI W32_console_ctrl_handler(DWORD type)
{
	switch (type)
	{
		/* Catch CTRL-C */
	case CTRL_C_EVENT: {
		cl_object function = ECL_SYM_FUN(@'si::terminal-interrupt');
		if (function)
			W32_handle_in_new_thread(function);
		return TRUE;
	}
	}
	return FALSE;
}
#endif /* ECL_WINDOWS_THREADS */

#if 0
static cl_object
asynchronous_signal_servicing_thread()
{
	const cl_env_ptr the_env = ecl_process_env();
	sigset_t handled_set;
	cl_object signal_code;
	int signo;
	int interrupt_signal = 0;
	if (ecl_option_values[ECL_OPT_TRAP_INTERRUPT_SIGNAL]) {
		interrupt_signal = ecl_option_values[ECL_OPT_THREAD_INTERRUPT_SIGNAL];
	}
        /*
         * We wait here for all signals that are blocked in all other
         * threads. It would be desirable to be able to wait for _all_
         * signals, but this can not be done for SIGFPE, SIGSEGV, etc.
         */
	pthread_sigmask(SIG_SETMASK, NULL, &handled_set);
	/*
	 * Under OS X we also have to explicitely add the signal we
	 * use to communicate process interrupts. For some unknown
	 * reason those signals may get lost.
	 */
	if (interrupt_signal) {
		sigaddset(&handled_set, interrupt_signal);
		pthread_sigmask(SIG_SETMASK, &handled_set, NULL);
	}
	ECL_CATCH_ALL_BEGIN(the_env) {
	for (;;) {
		/* Waiting may fail! */
		int status = sigwait(&handled_set, &signo);
		if (status == 0) {
#if 0
			if (signo == interrupt_signal) {
				/* If we get this signal it may be because
				 * of two reasons. One is that it is just
				 * an awake message. Then the queue is empty
				 * and we continue ... */
				signal_code = pop_signal(the_env);
				if (Null(signal_code))
					continue;
				/* ... the other one is that we are being
				 * interrupted, but this only happens when
				 * we quit */
				break;
			}
#else
			if (signo == interrupt_signal) {
				break;
			}
#endif
#ifdef SIGCHLD
                        if (signo == SIGCHLD) {
                                si_wait_for_all_processes(0);
                                continue;
                        }
#endif
			signal_code = ecl_gethash_safe(ecl_make_fixnum(signo),
						       cl_core.known_signals,
						       ECL_NIL);
			if (!Null(signal_code)) {
				mp_process_run_function(3, @'si::handle-signal',
							@'si::handle-signal',
							signal_code);
			}
		}
	}
	} ECL_CATCH_ALL_END;
	ecl_return0(the_env);
}
#endif

cl_object
si_trap_fpe(cl_object condition, cl_object flag)
{
        cl_env_ptr the_env = ecl_process_env();
#ifndef FE_ALL_EXCEPT
# define FE_ALL_EXCEPT FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID
#endif
        const int all = FE_ALL_EXCEPT;
	int bits = 0;
        if (condition == @'last') {
		bits = the_env->trap_fpe_bits;
        } else {
                if (condition == ECL_T)
                        bits = FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID;
		else if (condition == @'division-by-zero')
                        bits = FE_DIVBYZERO;
                else if (condition == @'floating-point-overflow')
                        bits = FE_OVERFLOW;
                else if (condition == @'floating-point-underflow')
                        bits = FE_UNDERFLOW;
                else if (condition == @'floating-point-invalid-operation')
                        bits = FE_INVALID;
                else if (condition == @'floating-point-inexact')
                        bits = FE_INEXACT;
                else if (ECL_FIXNUMP(condition))
			bits = ecl_fixnum(condition) & all;
                if (flag == ECL_NIL) {
                        bits = the_env->trap_fpe_bits & ~bits;
                } else {
                        bits = the_env->trap_fpe_bits | bits;
                }
        }
#if !defined(ECL_AVOID_FPE_H)
# ifdef HAVE_FENV_H
        feclearexcept(all);
# endif
# if defined(ECL_MS_WINDOWS_HOST)
	_fpreset();
# endif
# ifdef HAVE_FEENABLEEXCEPT
        fedisableexcept(all & ~bits);
        feenableexcept(all & bits);
# endif
#endif
        the_env->trap_fpe_bits = bits;
	@(return ecl_make_fixnum(bits))
}

/*
 * In this code we decide whether to install a process-wide signal
 * handler for each of the asynchronous signals (SIGINT, SIGTERM,
 * SIGCHLD...) or we block the signal and let the background thread
 * detect and process them.
 */
static void
install_asynchronous_signal_handlers()
{
#if defined(ECL_MS_WINDOWS_HOST)
# define async_handler(signal,handler,mask)
#else
# if defined(ECL_THREADS) && defined(HAVE_SIGPROCMASK)
#  define async_handler(signal,handler,mask)  {				\
		if (ecl_option_values[ECL_OPT_SIGNAL_HANDLING_THREAD]) { \
			mysignal(signal, deferred_signal_handler);	\
		} else {						\
			mysignal(signal,handler);			\
		}}
# else
#  define async_handler(signal,handler,mask)	\
	mysignal(signal,handler)
# endif
#endif
#ifdef HAVE_SIGPROCMASK
	sigset_t *sigmask = cl_core.default_sigmask = &main_thread_sigmask;
        cl_core.default_sigmask_bytes = sizeof(sigset_t);
# ifdef ECL_THREADS
	pthread_sigmask(SIG_SETMASK, NULL, sigmask);
# else
        sigprocmask(SIG_SETMASK, NULL, sigmask);
# endif
#endif
#ifdef SIGINT
	if (ecl_option_values[ECL_OPT_TRAP_SIGINT]) {
		async_handler(SIGINT, non_evil_signal_handler, sigmask);
	}
#endif
#ifdef SIGCHLD
	if (ecl_option_values[ECL_OPT_TRAP_SIGCHLD]) {
                /* We have to set the process signal handler explicitly,
                 * because on many platforms the default is SIG_IGN. */
		mysignal(SIGCHLD, non_evil_signal_handler);
		async_handler(SIGCHLD, non_evil_signal_handler, sigmask);
	}
#endif
#ifdef HAVE_SIGPROCMASK
# if defined(ECL_THREADS)
	pthread_sigmask(SIG_SETMASK, sigmask, NULL);
# else
	sigprocmask(SIG_SETMASK, sigmask, NULL);
# endif
#endif
#ifdef ECL_WINDOWS_THREADS
	old_W32_exception_filter =
		SetUnhandledExceptionFilter(_ecl_w32_exception_filter);
	if (ecl_option_values[ECL_OPT_TRAP_SIGINT]) {
		SetConsoleCtrlHandler(W32_console_ctrl_handler, TRUE);
	}
#endif
#undef async_handler
}

/*
 * In POSIX systems we may set up a background thread that detects
 * synchronous signals and spawns a new thread to handle each of them.
 */
static void
install_signal_handling_thread()
{
#if defined(ECL_THREADS) && defined(HAVE_SIGPROCMASK)
	ecl_process_env()->default_sigmask = &main_thread_sigmask;
	if (ecl_option_values[ECL_OPT_SIGNAL_HANDLING_THREAD]) {
		cl_object fun =
			ecl_make_cfun((cl_objectfn_fixed)
				      asynchronous_signal_servicing_thread,
				      @'si::signal-servicing',
				      ECL_NIL,
				      0);
		cl_object process =
			signal_thread_process =
			mp_process_run_function_wait(2,
                                                     @'si::signal-servicing',
                                                     fun);
		if (Null(process)) {
			ecl_internal_error("Unable to create signal "
					   "servicing thread");
		}
	}
#endif
}

/*
 * This routine sets up handlers for all exceptions, such as access to
 * restricted regions of memory. They have to be set up before we call
 * init_GC().
 */
static void
install_synchronous_signal_handlers()
{
#ifdef SIGBUS
	if (ecl_option_values[ECL_OPT_TRAP_SIGBUS]) {
		do_catch_signal(SIGBUS, ECL_T, ECL_NIL);
	}
#endif
#ifdef SIGSEGV
	if (ecl_option_values[ECL_OPT_TRAP_SIGSEGV]) {
		do_catch_signal(SIGSEGV, ECL_T, ECL_NIL);
	}
#endif
#ifdef SIGPIPE
	if (ecl_option_values[ECL_OPT_TRAP_SIGPIPE]) {
		do_catch_signal(SIGPIPE, ECL_T, ECL_NIL);
	}
#endif
#ifdef SIGILL
	if (ecl_option_values[ECL_OPT_TRAP_SIGILL]) {
		do_catch_signal(SIGILL, ECL_T, ECL_NIL);
	}
#endif
	/* In order to implement MP:INTERRUPT-PROCESS, MP:PROCESS-KILL
	 * and the like, we use signals. This sets up a synchronous
	 * signal handler for that particular signal.
	 */
#ifdef SIGRTMIN
# define DEFAULT_THREAD_INTERRUPT_SIGNAL SIGRTMIN + 2
#else
# define DEFAULT_THREAD_INTERRUPT_SIGNAL SIGUSR1
#endif
#if defined(ECL_THREADS) && !defined(ECL_MS_WINDOWS_HOST)
	if (ecl_option_values[ECL_OPT_TRAP_INTERRUPT_SIGNAL]) {
		int signal = ecl_option_values[ECL_OPT_THREAD_INTERRUPT_SIGNAL];
		if (signal == 0) {
			signal = DEFAULT_THREAD_INTERRUPT_SIGNAL;
			ecl_set_option(ECL_OPT_THREAD_INTERRUPT_SIGNAL,
				       signal);
		}
		mysignal(signal, process_interrupt_handler);
#ifdef HAVE_SIGPROCMASK
                sigdelset(&main_thread_sigmask, signal);
                pthread_sigmask(SIG_SETMASK, &main_thread_sigmask, NULL);
#endif
	}
#endif
}

/*
 * This routine sets up handlers for floating point exceptions. We
 * cannot do it earlier because it requires the memory allocator to
 * be set up.
 */
static void
install_fpe_signal_handlers()
{
#ifdef SIGFPE
	if (ecl_option_values[ECL_OPT_TRAP_SIGFPE]) {
		mysignal(SIGFPE, fpe_signal_handler);
		si_trap_fpe(ECL_T, ECL_T);
# ifdef ECL_IEEE_FP
		/* By default deactivate errors and accept
		 * denormals in floating point computations */
		si_trap_fpe(@'floating-point-invalid-operation', ECL_NIL);
		si_trap_fpe(@'division-by-zero', ECL_NIL);
		si_trap_fpe(@'floating-point-overflow', ECL_NIL);
# endif
	}
#endif
}

/*
 * Create one Common Lisp constant for each signal that we know,
 * such as +SIGINT+ for SIGINT, etc.
 */
static void
add_one_signal(cl_object hash_table, int signal, cl_object name, cl_object handler)
{
	cl_object code = ecl_make_fixnum(signal);
	cl_export2(name, cl_core.ext_package);
	si_Xmake_constant(name, code);
	ecl_sethash(code, hash_table, handler);
}

static void
create_signal_code_constants()
{
	cl_object hash =
		cl_core.known_signals =
		cl__make_hash_table(@'eql', ecl_make_fixnum(128),
				    cl_core.rehash_size,
				    cl_core.rehash_threshold);
	int i;
	for (i = 0; known_signals[i].code >= 0; i++) {
		add_one_signal(hash, known_signals[i].code,
			       _ecl_intern(known_signals[i].name,
					   cl_core.ext_package),
			       known_signals[i].handler);
	}
#ifdef SIGRTMIN
	for (i = SIGRTMIN; i <= SIGRTMAX; i++) {
		int intern_flag[1];
		char buffer[64];
		cl_object name;
		sprintf(buffer, "+SIGRT%d+", i-SIGRTMIN);
		name = ecl_intern(make_base_string_copy(buffer),
				  cl_core.ext_package,
				  intern_flag);
		add_one_signal(hash, i, name, ECL_NIL);
	}
	add_one_signal(hash, SIGRTMIN,
		       _ecl_intern("+SIGRTMIN+", cl_core.ext_package),
		       ECL_NIL);
	add_one_signal(hash, SIGRTMAX,
		       _ecl_intern("+SIGRTMAX+", cl_core.ext_package),
		       ECL_NIL);
#endif
}

void
init_unixint(int pass)
{
	if (pass == 0) {
		install_asynchronous_signal_handlers();
		install_synchronous_signal_handlers();
	} else {
		create_signal_code_constants();
		install_fpe_signal_handlers();
		install_signal_handling_thread();
		ECL_SET(@'ext::*interrupts-enabled*', ECL_T);
		ecl_process_env()->disable_interrupts = 0;
	}
}
