/*
    File: mpPackage.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

#include <sched.h>
#include <signal.h>
#include <sys/types.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/core/symbol.h>
#include <clasp/core/pointer.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/primitives.h>
#include <clasp/core/designators.h>
#include <clasp/core/compiler.h>
#include <clasp/core/package.h>
#include <clasp/core/lispList.h>
#include <clasp/gctools/interrupt.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/unwind.h>

extern "C" {
void mutex_lock_enter(char* nameword) { (void)0; };
void mutex_lock_return(char* nameword) { (void)0; };
};

namespace mp {

std::atomic<uintptr_t> global_process_UniqueID;

#ifdef CLASP_THREADS
std::atomic<size_t> global_LastBindingIndex;
Mutex global_BindingIndexPoolMutex(BINDINDX_NAMEWORD, false);
std::vector<size_t> global_BindingIndexPool;
#endif

#ifdef DEBUG_THREADS
struct DebugThread {
  DebugThread* _Next;
  std::string _Message;
  size_t _Tid;
  DebugThread(DebugThread* next, const std::string& msg, size_t tid) : _Next(next), _Message(msg), _Tid(tid){};
};

std::atomic<DebugThread*> global_DebugThreadList;

void dump_debug_threads(const char* filename) {
  FILE* fout = fopen(filename, "w");
  DebugThread* cur = global_DebugThreadList.load();
  while (cur) {
    fprintf(fout, "Tid[%lu] %s\n", cur->_Tid, cur->_Message.c_str());
    cur = cur->_Next;
  }
  fclose(fout);
}
#endif

}; // namespace mp
namespace mp {

#ifdef DEBUG_THREADS
void debug_mutex_lock(Mutex* m) {
  if (core::_sym_STARdebug_threadsSTAR && !core::_sym_STARdebug_threadsSTAR.unboundp() &&
      core::_sym_STARdebug_threadsSTAR->boundP() && core::_sym_STARdebug_threadsSTAR->symbolValue().notnilp()) {
    stringstream ss;
    ss << "lock " << (char*)&(m->_NameWord) << std::endl;
    DebugThread* cur = new DebugThread(global_DebugThreadList.load(), ss.str(), my_thread->_Tid);
    bool exchanged = false;
    do {
      exchanged = global_DebugThreadList.compare_exchange_strong(cur->_Next, cur);
      if (!exchanged) {
        cur->_Next = global_DebugThreadList.load();
      }
    } while (!exchanged);
  }
};

void debug_mutex_unlock(Mutex* m) {
  if (core::_sym_STARdebug_threadsSTAR && !core::_sym_STARdebug_threadsSTAR.unboundp() &&
      core::_sym_STARdebug_threadsSTAR->boundP() && core::_sym_STARdebug_threadsSTAR->symbolValue().notnilp()) {
    stringstream ss;
    ss << "UNlock " << (char*)&(m->_NameWord) << std::endl;
    DebugThread* cur = new DebugThread(global_DebugThreadList.load(), ss.str(), my_thread->_Tid);
    bool exchanged = false;
    do {
      exchanged = global_DebugThreadList.compare_exchange_strong(cur->_Next, cur);
      if (!exchanged) {
        cur->_Next = global_DebugThreadList.load();
      }
    } while (!exchanged);
  }
};
#endif // DEBUG_THREADS

}; // namespace mp

namespace mp {

SYMBOL_EXPORT_SC_(MpPkg, STARcurrent_processSTAR);
SYMBOL_EXPORT_SC_(MpPkg, signal_interrupt);

CL_DEFUN
Process_sp mp__current_process() {
  Process_sp this_process = gc::As<Process_sp>(_sym_STARcurrent_processSTAR->symbolValue());
  return this_process;
}

void Process_O::runInner(core::List_sp bindings) {
  core::DynamicScopeManager scope(_sym_STARcurrent_processSTAR, this->asSmartPtr());
  if (bindings.consp()) {
    core::Cons_sp pair = gc::As<core::Cons_sp>(CONS_CAR(bindings));
    core::DynamicScopeManager scope(gc::As<core::Symbol_sp>(pair->car()), core::eval::evaluate(pair->cdr(), nil<core::T_O>()));
    runInner(CONS_CDR(bindings));
  } else {
    updatePhase(Running);
    core::T_mv result_mv;
    try {
      result_mv = core::core__apply0(core::coerce::calledFunctionDesignator(_Function), _Arguments);
    } catch (ExitProcess& e) {
        // Exiting specially. Don't touch _ReturnValuesList - it's initialized to NIL just fine,
        // and may have been set by mp:exit-process.
      updatePhase(Exited);
      return;
    } catch (AbortProcess& e) {
        // Exiting specially for some weird reason. Mark this as an abort.
        // NOTE: Should probably catch all attempts to exit, i.e. catch (...),
        // but that might be a problem for the main thread.
      _Aborted = true;
      updatePhase(Exited);
      return;
    }
    updatePhase(Exited);
    ql::list return_values;
    int nv = result_mv.number_of_values();
    core::MultipleValues& mv = core::lisp_multipleValues();
    if (nv > 0) {
      core::T_sp result0 = result_mv;
      return_values << result0;
      for (int i = 1; i < nv; ++i)
        return_values << mv.valueGet(i, result_mv.number_of_values());
    }
    _ReturnValuesList = return_values.result();
  }
}

__attribute__((noinline))
void Process_O::run(void* cold_end_of_stack) {
  gctools::ThreadLocalStateLowLevel thread_local_state_low_level(cold_end_of_stack);
  core::ThreadLocalState thread_local_state;
  my_thread_low_level = &thread_local_state_low_level;
  my_thread = &thread_local_state;
  my_thread->startUpVM();
  my_thread->initialize_thread(this->asSmartPtr(), true);
  //  my_thread->create_sigaltstack();
  _ThreadInfo = my_thread;

  // We're ready to run Lisp
  runInner(core::cl__reverse(_InitialSpecialBindings));

  // Remove the process
  _lisp->remove_process(this->asSmartPtr());
#ifdef DEBUG_MONITOR_SUPPORT
  // When enabled, maintain a thread-local map of strings to FILE*
  // used for logging. This is so that per-thread log files can be
  // generated.  These log files are automatically closed here when
  // the thread exits.
  for (auto it : my_thread->_MonitorFiles) {
    fclose(it.second);
  }
#endif
#ifdef USE_MPS
  gctools::my_thread_allocation_points.destroyAllocationPoints();
  mps_root_destroy(root._value);
  mps_thread_dereg(thr_o._value);
#endif
};

// This is the function actually passed to pthread_create.
void* start_thread(void* vinfo) {
  void* cold_end_of_stack = &cold_end_of_stack;
  static_cast<Process_O*>(vinfo)->run(cold_end_of_stack);
  return NULL;
}

string Mutex_O::__repr__() const {
  stringstream ss;
  ss << "#<MUTEX ";
  ss << _rep_(this->_Name);
  ss << " :owner " << _rep_(this->_Owner) << " :counter " << this->counter();
#ifdef NON_MOVING_GC // things don't move in boehm
  ss << " @" << (void*)(this->asSmartPtr().raw_());
#endif
  ss << ">";
  return ss.str();
}

CL_LAMBDA(mutex &optional (upgrade nil));
CL_DOCSTRING(
    R"dx(Obtain the write lock for this mutex. upgradep should be true if and only if this thread currently holds the shared lock for the same mutex.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__write_lock(SharedMutex_sp m, bool upgrade) { m->lock(upgrade); }

CL_LAMBDA(mutex &optional (upgrade nil));
CL_DOCSTRING(
    R"dx(Try to obtain the write lock for this mutex. If it cannot be obtained immediately, return false. Otherwise, return true.)dx");
DOCGROUP(clasp);
CL_DEFUN bool mp__write_try_lock(SharedMutex_sp m, bool upgrade) { return m->try_lock(upgrade); }

CL_LAMBDA(mutex &optional (release_read_lock nil));
CL_DOCSTRING(
    R"dx(Release the write lock. If releasep is true and the current thread holds the shared lock, it is released as well.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__write_unlock(SharedMutex_sp m, bool release_read_lock) { m->unlock(release_read_lock); }

CL_LAMBDA(mutex);
CL_DOCSTRING(R"dx(Obtain the shared lock for this mutex.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__shared_lock(SharedMutex_sp m) { m->lock_shared(); }

CL_LAMBDA(mutex);
CL_DOCSTRING(R"dx(Release the shared lock for this mutex.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__shared_unlock(SharedMutex_sp m) { m->unlock_shared(); }

void SharedMutex_O::setLockNames(core::SimpleBaseString_sp readLockName, core::SimpleBaseString_sp writeLockName) {
  this->_SharedMutex.mReadMutex._value._NameWord = lisp_nameword(readLockName);
  this->_SharedMutex.mWriteMutex._value._NameWord = lisp_nameword(writeLockName);
}

string SharedMutex_O::__repr__() const {
  stringstream ss;
  ss << "#<SHARED-MUTEX ";
  ss << _rep_(this->_Name);
#ifdef NON_MOVING_GC // things don't move in boehm
  ss << " @" << (void*)(this->asSmartPtr().raw_());
#endif
  ss << ">";
  return ss.str();
}

int Process_O::startProcess() {
  if (!updatePhaseFrom(Nascent, Booting))
    // Some other process has started this up - bow out gracefully.
    return 0;
  _lisp->add_process(this->asSmartPtr());
  pthread_attr_t attr;
  int result;
  result = pthread_attr_init(&attr);
  result = pthread_attr_setstacksize(&attr, this->_StackSize);
  if (result != 0)
    return result;
  // We pass ourselves into the thread being created.
  // There is a subtlety here - something needs to be keeping the thread alive.
  // If this function returns before the thread is fully created, this process
  // could hypothetically garbage collected if it's not otherwise accessible.
  // That's why we have to do _lisp->add_process - it keeps it globally accessible
  // and so the problem is solved.
  result = pthread_create(&this->_TheThread._value, &attr, start_thread, (void*)this);
  pthread_attr_destroy(&attr);
  return result;
}

void Process_O::interrupt(core::T_sp interrupt) {
   /*
   * Lifted from the ECL source code.  meister 2017
   * We first ensure that the process is active and running
   * and past the initialization phase, where it has set up
   * the environment. Then add the interrupt to the process's
   * queue, and it will examine it at its own leisure.
   */
  do {
    ProcessPhase p = phase();
    switch (p) {
    case Running:
    case Suspended: {
        _ThreadInfo->enqueue_interrupt(interrupt);
        if (_ThreadInfo->blockingp())
          // The thread is blocked on something, so wake it up with a signal.
          // We use SIGCONT since it's kinda obscure and waking up processes is
          // what it's for, though perhaps not in this way originally.
          // FIXME?: We could use pthread_sigqueue to stick in some extra info
          // in order to disambiguate our wakeups from others' a bit.
          pthread_kill(_TheThread._value, SIGCONT);
        return;
      }
    case Exited: return; // we were too slow! oh well, who cares.
    case Nascent: SIMPLE_ERROR("Cannot interrupt unstarted process.");
    case Booting: waitPhase(p);
    }
  } while (true);
}

// This function must only be called from within the process.
// which in turn means the _Phase must already be Running.
void Process_O::suspend() {
  updatePhase(Suspended);
  waitPhase(Suspended); // c++20 guarantees this never wakes spuriously.
}

// This function is called from outside the process.
void Process_O::resume() { updatePhaseFrom(Suspended, Running); }

string Process_O::phase_as_string() const {
  switch (phase()) {
  case Nascent:
    return "(Not yet started)";
  case Booting:
      return "(Booting)";
  case Running:
    return "(Running)";
  case Suspended:
    return "(Suspended)";
  case Exited:
    if (this->_Aborted)
      return "(Aborted)";
    else
      return "(Completed)";
  default:
    return "(Unknown Phase)";
  }
}

string Process_O::__repr__() const {
  stringstream ss;
  ss << "#<PROCESS ";
  ss << _rep_(this->_Name);
#ifdef NON_MOVING_GC // things don't move in boehm
  ss << " @" << (void*)(this->asSmartPtr().raw_());
#endif
  ss << " " << this->phase_as_string();
  ss << ">";
  return ss.str();
}

CL_DOCSTRING(R"dx(Current Phase of the process as String (Not yet started, Running, Suspended, Aborted, Completed))dx");
DOCGROUP(clasp);
CL_DEFUN core::SimpleBaseString_sp mp__process_phase_string(Process_sp process) {
  return core::SimpleBaseString_O::make(process->phase_as_string());
};

CL_DOCSTRING(R"dx(Current Phase of the process. Nascent = 0, Running = 1, Suspended = 2, Exited = 3)dx");
DOCGROUP(clasp);
CL_DEFUN int mp__process_phase(Process_sp process) { return process->phase(); };

CL_DOCSTRING(R"dx(Return the owner of the lock - this may be NIL if it's not locked.)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp mp__lock_owner(Mutex_sp m) { return m->_Owner; }

CL_DOCSTRING(R"dx(Start execution of a nascent process. If the process has already started, does nothing. Return no values.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__process_start(Process_sp process) {
  process->startProcess();
};

CL_DOCSTRING(
    R"dx(Convenience function that creates a process and then immediately starts it. Arguments are as in MAKE-PROCESS; the ARGUMENTS parameter is always NIL.)dx");
CL_LAMBDA(name function &optional special_bindings);
DOCGROUP(clasp);
CL_DEFUN Process_sp mp__process_run_function(core::T_sp name, core::T_sp function, core::List_sp special_bindings) {
#ifdef DEBUG_FASTGF
  core::Cons_sp fastgf = core::Cons_O::create(core::_sym_STARdebug_fastgfSTAR, nil<core::T_O>());
  special_bindings = core::Cons_O::create(fastgf, special_bindings);
#endif
  if (cl__functionp(function)) {
    Process_sp process = Process_O::make_process(name, function, nil<core::T_O>(), special_bindings, DEFAULT_THREAD_STACK_SIZE);
    process->startProcess();
    return process;
  }
  SIMPLE_ERROR("{} is not a function - you must provide a function to run in a separate process", _rep_(function));
};

CL_DOCSTRING(
    R"dx(Return a list of all processes that have been enabled and have not yet exited, i.e. all active and suspended processes.)dx");
DOCGROUP(clasp);
CL_DEFUN core::List_sp mp__all_processes() { return _lisp->processes(); }

CL_DOCSTRING(R"dx(Return the name of a process, as provided at its creation.)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp mp__process_name(Process_sp p) { return p->_Name; }

CL_LAMBDA(&optional (process mp:*current-process*));
DOCGROUP(clasp);
CL_DEFUN core::T_sp mp__thread_id(Process_sp p) {
  auto tid = p->_ThreadInfo->_Tid;
  return core::Integer_O::create((uintptr_t)tid);
}

CL_DOCSTRING(
    R"dx(Return true iff the process is active, i.e. is currently executing. More specifically, this means it has been started and is not currently suspended.)dx");
DOCGROUP(clasp);
CL_DEFUN bool mp__process_active_p(Process_sp p) {
  auto phase = p->phase();
  return phase == Running || phase == Booting;
}

// Internal function used only for process-suspend (which is external).
// FIXME: Don't actually export.
SYMBOL_EXPORT_SC_(MpPkg, suspend_loop);
DOCGROUP(clasp);
CL_DEFUN void mp__suspend_loop() {
  Process_sp this_process = gc::As<Process_sp>(_sym_STARcurrent_processSTAR->symbolValue());
  this_process->suspend();
};

CL_DOCSTRING(R"dx(Restart execution in a suspended process.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__process_resume(Process_sp process) { process->resume(); }

CL_DOCSTRING(
    R"dx(Inform the scheduler that the current process doesn't need control for the moment. It may or may not use this information. Returns no values.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__process_yield() {
  // On success, sched_yield() returns 0.
  // On error, -1 is returned, and errno is set appropriately.
  int res = sched_yield();
  if (res == -1) {
    SIMPLE_ERROR("sched_yield returned the error {}", res);
  }
  //  core::clasp_musleep(0.5,true);
}

SYMBOL_EXPORT_SC_(MpPkg, process_error);
SYMBOL_EXPORT_SC_(MpPkg, process_error_process);
SYMBOL_EXPORT_SC_(MpPkg, process_join_error);
SYMBOL_EXPORT_SC_(MpPkg, process_join_error_original_condition);
SYMBOL_EXPORT_SC_(MpPkg, process_join_error_aborted);
SYMBOL_EXPORT_SC_(KeywordPkg, original_condition);

CL_DOCSTRING(
    R"dx(Wait for the given process to finish executing. If the process's function returns normally, those values are returned. If the process exited due to EXIT-PROCESS, the values provided to that function are returned. If the process was not started or aborted by ABORT-PROCESS or a control transfer, an error of type PROCESS-JOIN-ERROR is signaled.)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_mv mp__process_join(Process_sp process) {
  if (process->phase() == Nascent)
    ERROR(_sym_process_join_error, core::lisp_createList(kw::_sym_process, process));
  if (process->phase() != Exited) {
    pthread_join(process->_TheThread._value, NULL);
  }
  if (process->_Aborted)
    ERROR(_sym_process_join_error, core::lisp_createList(kw::_sym_process, process, kw::_sym_original_condition,
                                                         process->_AbortCondition, kw::_sym_aborted, _lisp->_true()));

  return cl__values_list(process->_ReturnValuesList);
}

CL_LAMBDA(process function &rest args);
CL_DEFUN core::T_sp mp__process_preset(Process_sp process, core::T_sp function, core::T_sp args) {
  process->_Function = function;
  process->_Arguments = args;
  return process;
}

CL_DOCSTRING(R"dx(Internal. Enqueue the given interrupt to the thread's pending interrupt list. Returns no values.")dx");
DOCGROUP(clasp);
CL_DEFUN void mp__enqueue_interrupt(Process_sp process, core::T_sp interrupt) {
  process->interrupt(interrupt);
}

SYMBOL_EXPORT_SC_(MpPkg, posix_interrupt);
void posix_signal_interrupt(int sig) {
  // Save multiple values so that everything's as it was
  // if we return from these calls.
  core::MultipleValues& multipleValues = core::lisp_multipleValues();
  size_t nvals = multipleValues.getSize();
  core::T_O* mv_temp[nvals];
  multipleValues.saveToTemp(nvals, mv_temp);
  // Signal our Lisp signal.
  // mp:posix-interrupt is defined in clos/conditions.lisp.
  if (_sym_posix_interrupt->fboundp())
    core::eval::funcall(_sym_posix_interrupt->symbolFunction(),
                        core::clasp_make_fixnum(sig));
  // If it's too early to call into Lisp, we do nothing
  // and return. This makes it so that, for example, an ABRT signal
  // will not be handled and thus terminate the process, rather than
  // be "handled" so a few dozen ABRTs need to be sent to actually
  // kill the process.
  multipleValues.loadFromTemp(nvals, mv_temp);
}

CL_LAMBDA(&rest values);
CL_DOCSTRING(R"dx(Immediately end the current process)dx");
CL_DOCSTRING_LONG(
    R"dx(The arguments to this function are returned from any PROCESS-JOIN calls with the current process as argument. Does not return.)doc")
DOCGROUP(clasp)dx");
CL_DEFUN void mp__exit_process(core::List_sp values) {
  Process_sp this_process = gc::As<Process_sp>(_sym_STARcurrent_processSTAR->symbolValue());
  this_process->_ReturnValuesList = values;
  throw ExitProcess();
};

// See abort-process in mp.lisp
CL_LISPIFY_NAME("mp:%abort-process");
CL_LAMBDA(maybe-condition);
CL_DOCSTRING(R"dx(Internal function; use ABORT-PROCESS instead.)dx");
CL_DOCSTRING_LONG(
    R"dx(Immediately end the current process abnormally. If PROCESS-JOIN is called on this process thereafter, it will signal an error of type PROCESS-JOIN-ERROR with the given condition (or NIL) attached.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__PERCENTabort_process(core::T_sp maybe_condition) {
  Process_sp this_process = gc::As<Process_sp>(_sym_STARcurrent_processSTAR->symbolValue());
  this_process->_AbortCondition = maybe_condition;
  throw AbortProcess();
}

CL_DOCSTRING(R"dx(Return the name of the mutex, as provided at creation. The mutex may be normal or recursive.)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp mp__mutex_name(Mutex_sp m) { return m->_Name; }

CL_LAMBDA(mutex &optional (waitp t));
CL_DOCSTRING(R"dx(Try to obtain exclusion on the given mutex)dx");
CL_DOCSTRING_LONG(
    R"dx(If WAITP is true, this function will not return until exclusion is obtained.\n\nReturn true iff exclusion was obtained, otherwise false.)dx");
DOCGROUP(clasp);
CL_DEFUN bool mp__get_lock(core::T_sp m, bool waitp) {
  if (*(void**)&*m == NULL) {
    printf("%s:%d:%s The Mutex @%p has been wiped out\n", __FILE__, __LINE__, __FUNCTION__, (void*)&*m);
    abort();
  }
  if (!gc::IsA<Mutex_sp>(m)) {
    TYPE_ERROR(m, mp::_sym_Mutex_O);
  }
  Mutex_sp mm = gc::As_unsafe<Mutex_sp>(m);
  return mm->lock(waitp);
}

CL_LAMBDA(mutex);
CL_DOCSTRING(R"dx(Release exclusion on the given mutex. Return no values.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__giveup_lock(Mutex_sp m) { m->unlock(); }

DOCGROUP(clasp);
CL_DEFUN bool mp__recursive_lock_p(Mutex_sp m) { return gc::IsA<RecursiveMutex_sp>(m); }

DOCGROUP(clasp);
CL_DEFUN bool mp__holding_lock_p(Mutex_sp m) { return m->_Owner == _sym_STARcurrent_processSTAR->symbolValue(); }

DOCGROUP(clasp);
CL_DEFUN core::Fixnum_sp mp__lock_count(Mutex_sp m) { return core::clasp_make_fixnum(m->counter()); }

CL_LAMBDA(&key name);
CL_DOCSTRING(R"dx(Make a new condition variable)dx");
CL_DOCSTRING_LONG(R"dx(The NAME argument is stored with the condition variable for debugging purposes.)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp mp__make_condition_variable(core::T_sp name) { return ConditionVariable_O::make_ConditionVariable(name); }

CL_DOCSTRING(R"dx(Wait on the given condition variable with the given mutex)dx");
CL_DOCSTRING_LONG(
    R"dx(In more detail: The mutex must already be held by this thread. Then, atomically, the mutex is released and this thread blocks on the condition variable (i.e. execution will not continue).\n\nLater, the thread will resume and obtain the mutex again. Ideally this will be when the process is properly notified (see below), but occasionally the thread may resume spuriously, so make sure to check that the condition is actually true after this function returns.)dx");
DOCGROUP(clasp);
CL_DEFUN bool mp__condition_variable_wait(ConditionVariable_sp cv, Mutex_sp mutex) { return cv->wait(mutex); };

CL_DOCSTRING(R"dx(Like CONDITION-VARIABLE-WAIT, except that a timeout (in seconds) may be provided.)dx");
DOCGROUP(clasp);
CL_DEFUN bool mp__condition_variable_timedwait(ConditionVariable_sp cv, Mutex_sp mutex, double timeout_seconds) {
  //  printf("%s:%d   timeout_seconds = %lf\n", __FILE__, __LINE__, timeout_seconds );
  return cv->timed_wait(mutex, timeout_seconds);
};

CL_DOCSTRING(R"dx(Notify at least one thread that is currently waiting on the given condition variable (i.e. wake it up))dx");
CL_DOCSTRING_LONG(R"dx(If no threads are waiting on it, there is no effect. Return no values.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__condition_variable_signal(ConditionVariable_sp cv) { cv->signal(); };

CL_DOCSTRING(R"dx(Notify all threads currently waiting on the given condition variable)dx");
CL_DOCSTRING_LONG(R"dx(If no threads are waiting on it, there is no effect. Return no values.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__condition_variable_broadcast(ConditionVariable_sp cv) { cv->broadcast(); };

string ConditionVariable_O::__repr__() const {
  stringstream ss;
  ss << "#<CONDITION-VARIABLE ";
  ss << _rep_(this->_Name);
  ss << ">";
  return ss.str();
}

DOCGROUP(clasp);
CL_DEFUN void mp__push_default_special_binding(core::Symbol_sp symbol, core::T_sp form) {
  _lisp->push_default_special_binding(symbol, form);
}

DOCGROUP(clasp);
CL_DEFUN core::List_sp mp__copy_default_special_bindings() { return _lisp->copy_default_special_bindings(); }

DOCGROUP(clasp);
CL_DEFUN core::List_sp mp__process_initial_special_bindings(Process_sp p) {
  return core::cl__copy_list(p->_InitialSpecialBindings);
}

DOCGROUP(clasp);
CL_DEFUN void mp__check_pending_interrupts() { gctools::handle_all_queued_interrupts(); }

CL_DOCSTRING(
    R"dx(Establish a fence that prevents the compiler or CPU from reordering some accesses across it. Returns no values.)dx");
CL_DOCSTRING_LONG(
    R"dx(ORDER is the same as accepted by ATOMIC, except that :relaxed does not make sense for fences and will be rejected")dx");
DOCGROUP(clasp);
CL_DEFUN void mp__fence(core::T_sp order) {
  // For the function call, we do not check the order. This is kind of a KLUDGE,
  // but really, if you're doing the kind of programming that needs fences you
  // don't want a runtime branch.
  // cclasp will inline calls to this function with constant ORDER.
  (void)order;
  std::atomic_thread_fence(std::memory_order_seq_cst);
}

}; // namespace mp
