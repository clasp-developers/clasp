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

// This keeps track of a process on the list of active threads.
// Also makes sure its phase is set as it exits.
struct SafeRegisterDeregisterProcessWithLisp {
  Process_sp _Process;
  SafeRegisterDeregisterProcessWithLisp(Process_sp p) : _Process(p) { _lisp->add_process(_Process); }
  ~SafeRegisterDeregisterProcessWithLisp() {
    _Process->_Phase = Exited;
    _lisp->remove_process(_Process);
  }
};

void do_start_thread_inner(Process_sp process, core::List_sp bindings) {
  if (bindings.consp()) {
    core::Cons_sp pair = gc::As<core::Cons_sp>(CONS_CAR(bindings));
    core::DynamicScopeManager scope(gc::As<core::Symbol_sp>(pair->car()), core::eval::evaluate(pair->cdr(), nil<core::T_O>()));
    do_start_thread_inner(process, CONS_CDR(bindings));
  } else {
    core::List_sp args = process->_Arguments;
    process->_Phase = Active;
    core::T_mv result_mv;
    {
      try {
        result_mv = core::core__apply0(core::coerce::calledFunctionDesignator(process->_Function), args);
      } catch (ExitProcess& e) {
        // Exiting specially. Don't touch _ReturnValuesList - it's initialized to NIL just fine,
        // and may have been set by mp:exit-process.
        return;
      } catch (AbortProcess& e) {
        // Exiting specially for some weird reason. Mark this as an abort.
        // NOTE: Should probably catch all attempts to exit, i.e. catch (...),
        // but that might be a problem for the main thread.
        process->_Aborted = true;
        return;
      }
    }
    ql::list return_values;
    int nv = result_mv.number_of_values();
    core::MultipleValues& mv = core::lisp_multipleValues();
    if (nv > 0) {
      core::T_sp result0 = result_mv;
      return_values << result0;
      for (int i = 1; i < nv; ++i)
        return_values << mv.valueGet(i, result_mv.number_of_values());
    }
    process->_ReturnValuesList = return_values.result();
  }
}

__attribute__((noinline)) void start_thread_inner(uintptr_t uniqueId, void* cold_end_of_stack) {
#ifdef USE_MPS
  // use mask
  mps_thr_t thr_o;
  mps_res_t res = mps_thread_reg(&thr_o, global_arena);
  if (res != MPS_RES_OK) {
    printf("%s:%d Could not register thread\n", __FILE__, __LINE__);
    abort();
  }
  mps_root_t root;
  res = mps_root_create_thread_tagged(&root, global_arena, mps_rank_ambig(), 0, thr_o, mps_scan_area_masked,
                                      gctools::pointer_tag_mask, gctools::pointer_tag_eq,
                                      reinterpret_cast<mps_addr_t>(const_cast<void*>(cold_end_of_stack)));
  if (res != MPS_RES_OK) {
    printf("%s:%d Could not create thread stack roots\n", __FILE__, __LINE__);
    abort();
  };
#endif
  // Look for the process
  Process_sp process;
  core::List_sp processes;
  {
    WITH_READ_LOCK(globals_->_ActiveThreadsMutex);
    processes = _lisp->_Roots._ActiveThreads;
    bool foundIt = false;
    for (auto cur : processes) {
      Process_sp proc = gc::As<Process_sp>(CONS_CAR(cur));
      if (proc->_UniqueID == uniqueId) {
        foundIt = true;
        process = proc;
      }
    }
    if (!foundIt) {
      printf("%s:%d A child process started up with the uniqueId %lu but its Process_O could not be found\n", __FILE__, __LINE__,
             uniqueId);
      abort();
    }
  }
  // Tell the process what MPS thr_o and root is
#ifdef USE_MPS
  process->thr_o = thr_o;
  process->root = root;
#endif
  gctools::ThreadLocalStateLowLevel thread_local_state_low_level(cold_end_of_stack);
  core::ThreadLocalState thread_local_state;
  thread_local_state.startUpVM();
  my_thread_low_level = &thread_local_state_low_level;
  my_thread = &thread_local_state;
//  printf("%s:%d entering start_thread  &my_thread -> %p \n", __FILE__, __LINE__, (void*)&my_thread);
#ifdef USE_MPS
  gctools::my_thread_allocation_points.initializeAllocationPoints();
#endif
  my_thread->initialize_thread(process, true);
  //  my_thread->create_sigaltstack();
  process->_ThreadInfo = my_thread;
  // Set the mp:*current-process* variable to the current process
  core::DynamicScopeManager scope(_sym_STARcurrent_processSTAR, process);
  core::List_sp reversed_bindings = core::cl__reverse(process->_InitialSpecialBindings);
  do_start_thread_inner(process, reversed_bindings);
  // Remove the process
  process->_Phase = Exited;
  _lisp->remove_process(process);
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
  mps_root_destroy(process->root._value);
  mps_thread_dereg(process->thr_o._value);
#endif
};

// This is the function actually passed to pthread_create.
void* start_thread(void* vinfo) {
  ThreadStartInfo* info = (ThreadStartInfo*)vinfo;
  uintptr_t uniqueId = info->_UniqueID;
  delete info;
  void* cold_end_of_stack = &cold_end_of_stack;
  ////////////////////////////////////////////////////////////
  //
  // MPS setup of thread
  //
  start_thread_inner(uniqueId, cold_end_of_stack);
  //  my_thread->destroy_sigaltstack();
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
CL_DEFUN void mp__write_lock(SharedMutex_sp m, bool upgrade) { m->write_lock(upgrade); }

CL_LAMBDA(mutex &optional (upgrade nil));
CL_DOCSTRING(
    R"dx(Try to obtain the write lock for this mutex. If it cannot be obtained immediately, return false. Otherwise, return true.)dx");
DOCGROUP(clasp);
CL_DEFUN bool mp__write_try_lock(SharedMutex_sp m, bool upgrade) { return m->write_try_lock(upgrade); }

CL_LAMBDA(mutex &optional (release_read_lock nil));
CL_DOCSTRING(
    R"dx(Release the write lock. If releasep is true and the current thread holds the shared lock, it is released as well.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__write_unlock(SharedMutex_sp m, bool release_read_lock) { m->write_unlock(release_read_lock); }

CL_LAMBDA(mutex);
CL_DOCSTRING(R"dx(Obtain the shared lock for this mutex.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__shared_lock(SharedMutex_sp m) { m->read_lock(); }

CL_LAMBDA(mutex);
CL_DOCSTRING(R"dx(Release the shared lock for this mutex.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__shared_unlock(SharedMutex_sp m) { m->read_unlock(); }

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

string Process_O::phase_as_string() const {
  switch (this->_Phase) {
  case Nascent:
    return "(Not yet started)";
  case Active:
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

CL_DOCSTRING(R"dx(Current Phase of the process. Nascent = 0, Active = 1, Suspended = 2, Exited = 3)dx");
DOCGROUP(clasp);
CL_DEFUN int mp__process_phase(Process_sp process) { return process->_Phase; };

CL_DOCSTRING(R"dx(Return the owner of the lock - this may be NIL if it's not locked.)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp mp__lock_owner(Mutex_sp m) { return m->_Owner; }

CL_DOCSTRING(R"dx(Start execution of a nascent process. Return no values.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__process_start(Process_sp process) {
  if (process->_Phase == Nascent) {
    _lisp->add_process(process);
    process->startProcess();
  } else
    SIMPLE_ERROR("The process {} has already started.", core::_rep_(process));
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
    // The process needs to be added to the list of processes before process->start() is called.
    // The child process code needs this to find the process in the list
    _lisp->add_process(process);
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
    R"dx(Return true iff the process is active, i.e. is currently executed. More specifically, this means it has been started and is not currently suspended.)dx");
DOCGROUP(clasp);
CL_DEFUN bool mp__process_active_p(Process_sp p) { return (p->_Phase == Active); }

// Internal function used only for process-suspend (which is external).
// FIXME: Don't actually export.
SYMBOL_EXPORT_SC_(MpPkg, suspend_loop);
DOCGROUP(clasp);
CL_DEFUN void mp__suspend_loop() {
  Process_sp this_process = gc::As<Process_sp>(_sym_STARcurrent_processSTAR->symbolValue());
  RAIILock<Mutex> lock(this_process->_SuspensionMutex._value);
  this_process->_Phase = Suspended;
  while (this_process->_Phase == Suspended) {
    if (!(this_process->_SuspensionCV._value.wait(this_process->_SuspensionMutex._value)))
      SIMPLE_ERROR("BUG: pthread_cond_wait ran into an error");
  }
};

CL_DOCSTRING(R"dx(Restart execution in a suspended process.)dx");
DOCGROUP(clasp);
CL_DEFUN void mp__process_resume(Process_sp process) {
  if (process->_Phase == Suspended) {
    RAIILock<Mutex> lock(process->_SuspensionMutex._value);
    process->_Phase = Active;
    if (!(process->_SuspensionCV._value.signal()))
      SIMPLE_ERROR("BUG: pthread_cond_signal ran into an error");
  } else
    SIMPLE_ERROR("Cannot resume a process ({}) that has not been suspended", core::_rep_(process));
};

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
  if (process->_Phase == Nascent)
    ERROR(_sym_process_join_error, core::lisp_createList(kw::_sym_process, process));
  if (process->_Phase != Exited) {
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

CL_DEFUN core::T_sp mp__process_enable(Process_sp process) {
  /* process_env and ok are changed after the setjmp call in
   * ECL_UNWIND_PROTECT_BEGIN, so they need to be declared volatile */
  // core::cl_env_ptr the_env = core::clasp_process_env();
  volatile int ok = 0;
  core::funwind_protect(
      [&]() {
        /* Try to gain exclusive access to the process at the same
         * time we ensure that it is inactive. This prevents two
         * concurrent calls to process-enable from different threads
         * on the same process */
        auto inactive = Inactive;
        unlikely_if(!process->_Phase.compare_exchange_strong(inactive, Booting)) {
          FEerror("Cannot enable the running process ~A.", 1, process);
        }
        process->_Parent = mp__current_process();
#if 0
    process->process.trap_fpe_bits =
        process->process.parent->process.env->trap_fpe_bits;
#endif
    /* Link environment and process together */
#if 0
    // We don't have process environments
    process_env = _ecl_alloc_env(the_env);
    process_env->own_process = process;
    process->process.env = process_env;
    /* Immediately list the process such that its environment is
     * marked by the gc when its contents are allocated */
    ecl_list_process(process);

    /* Now we can safely allocate memory for the environment contents
     * and store pointers to it in the environment */
    ecl_init_env(process_env);

    process_env->trap_fpe_bits = process->process.trap_fpe_bits;
    process_env->bindings_array = process->process.initial_bindings;
    process_env->thread_local_bindings_size = 
        process_env->bindings_array->vector.dim;
    process_env->thread_local_bindings =
        process_env->bindings_array->vector.self.t;
#endif

        SIMPLE_WARN("Handle the exit_barrier");
#if 0
    /* Activate the barrier so that processes can immediately start waiting. */
    mp_barrier_unblock(1, process->process.exit_barrier);

    /* Block the thread with this spinlock until it is ready */
    process->process.start_stop_spinlock = ECL_T;

    ecl_disable_interrupts_env(the_env);
#ifdef ECL_WINDOWS_THREADS
    {
      HANDLE code;
      DWORD threadId;

      code = (HANDLE)CreateThread(NULL, 0, thread_entry_point, process, 0, &threadId);
      ok = (process->process.thread = code) != NULL;
    }
#else
    {
      int code;
      pthread_attr_t pthreadattr;

      pthread_attr_init(&pthreadattr);
      pthread_attr_setdetachstate(&pthreadattr, PTHREAD_CREATE_DETACHED);
      /*
       * Block all asynchronous signals until the thread is completely
       * set up. The synchronous signals SIGSEGV and SIGBUS are needed
       * by the gc and thus can't be blocked.
       */
#ifdef HAVE_SIGPROCMASK
      {
        sigset_t new, previous;
        sigfillset(&new);
        sigdelset(&new, SIGSEGV);
        sigdelset(&new, SIGBUS);
        pthread_sigmask(SIG_BLOCK, &new, &previous);
        code = pthread_create(&process->process.thread, &pthreadattr,
                              thread_entry_point, process);
        pthread_sigmask(SIG_SETMASK, &previous, NULL);
      }
#else
      code = pthread_create(&process->process.thread, &pthreadattr,
                            thread_entry_point, process);
#endif
      ok = (code == 0);
    }
#endif
    ecl_enable_interrupts_env(the_env);
#endif // #if 0
        return Values0<core::T_O>();
      },
      [&]() { // ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT {
        if (!ok) {
          SIMPLE_WARN("ecl_unlist_process");
#if 0
        /* INV: interrupts are already disabled through thread safe
         * unwind-protect */
        ecl_unlist_process(process);
        /* Disable the barrier and alert possible waiting processes. */
        mp_barrier_unblock(3, process->process.exit_barrier,
                           @':disable', ECL_T);
        process->process.phase = ECL_PROCESS_INACTIVE;
        process->process.env = NULL;
        if (process_env != NULL)
          _ecl_dealloc_env(process_env);
#endif
        }
        /* Unleash the thread */
        SIMPLE_WARN("ecl_giveup_spinlock");
#if 0      
      ecl_giveup_spinlock(&process->process.start_stop_spinlock);
#endif
      });

  if (ok) {
    return process;
  } else {
    return nil<core::T_O>();
  }
}

CL_DOCSTRING(R"dx(Internal. Queue the given interrupt to the thread's pending interrupt list. Returns no values.")dx");
DOCGROUP(clasp);
CL_DEFUN void mp__queue_interrupt(Process_sp process, core::T_sp interrupt) {
  if (process->_Phase != Active) [[unlikely]]
    FEerror("Cannot interrupt the inactive process ~a", 1, process);
  clasp_interrupt_process(process, interrupt);
}

SYMBOL_EXPORT_SC_(MpPkg, posix_interrupt);
void posix_signal_interrupt(int sig) {
  if (_sym_posix_interrupt->fboundp())
    core::eval::funcall(_sym_posix_interrupt->symbolFunction(),
                        core::clasp_make_fixnum(sig));
  else
    core::cl__cerror(core::SimpleBaseString_O::make("Ignore signal"),
                     core::SimpleBaseString_O::make("Received POSIX signal ~d"),
                     core::Cons_O::createList(core::clasp_make_fixnum(sig)));
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
