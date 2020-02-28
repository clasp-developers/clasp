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
#include <clasp/core/compiler.h>
#include <clasp/core/package.h>
#include <clasp/core/lispList.h>
#include <clasp/gctools/interrupt.h>
#include <clasp/core/evaluator.h>


extern "C" {
void mutex_lock_enter(char* nameword) {
  (void)0;
};
void mutex_lock_return(char* nameword) {
  (void)0;
};

};

namespace mp {

#ifdef CLASP_THREADS
std::atomic<size_t> global_LastBindingIndex = ATOMIC_VAR_INIT(0);
Mutex global_BindingIndexPoolMutex(BINDINDX_NAMEWORD,false);
std::vector<size_t> global_BindingIndexPool;
#endif

#ifdef DEBUG_THREADS
struct DebugThread {
  DebugThread* _Next;
  std::string  _Message;
  size_t       _Tid;
  DebugThread(DebugThread* next, const std::string& msg, size_t tid) : _Next(next), _Message(msg), _Tid(tid) {};
};

std::atomic<DebugThread*> global_DebugThreadList;

void dump_debug_threads(const char* filename) {
  FILE* fout = fopen(filename,"w");
  DebugThread* cur = global_DebugThreadList.load();
  while (cur) {
    fprintf( fout, "Tid[%lu] %s\n", cur->_Tid, cur->_Message.c_str());
    cur = cur->_Next;
  }
  fclose(fout);
}
#endif

};
namespace mp {

#ifdef DEBUG_THREADS
void debug_mutex_lock(Mutex* m) {
  if (core::_sym_STARdebug_threadsSTAR
      && !core::_sym_STARdebug_threadsSTAR.unboundp()
      && core::_sym_STARdebug_threadsSTAR->boundP()
      && core::_sym_STARdebug_threadsSTAR->symbolValue().notnilp()) {
    stringstream ss;
    ss << "lock " << (char*) &(m->_NameWord) << std::endl;
    DebugThread* cur = new DebugThread(global_DebugThreadList.load(),ss.str(),my_thread->_Tid);
    bool exchanged = false;
    do {
      exchanged = global_DebugThreadList.compare_exchange_strong(cur->_Next,cur);
      if (!exchanged) {
        cur->_Next = global_DebugThreadList.load();
      }
    } while (!exchanged);
  }
};

void debug_mutex_unlock(Mutex* m) {
  if (core::_sym_STARdebug_threadsSTAR
      && !core::_sym_STARdebug_threadsSTAR.unboundp()
      && core::_sym_STARdebug_threadsSTAR->boundP()
      && core::_sym_STARdebug_threadsSTAR->symbolValue().notnilp()) {
    stringstream ss;
    ss << "UNlock " << (char*) &(m->_NameWord) << std::endl;
    DebugThread* cur = new DebugThread(global_DebugThreadList.load(),ss.str(),my_thread->_Tid);
    bool exchanged = false;
    do {
      exchanged = global_DebugThreadList.compare_exchange_strong(cur->_Next,cur);
      if (!exchanged) {
        cur->_Next = global_DebugThreadList.load();
      }
    } while (!exchanged);
  }
};
#endif // DEBUG_THREADS

};


namespace mp {

SYMBOL_EXPORT_SC_(MpPkg, STARcurrent_processSTAR);

// This keeps track of a process on the list of active threads.
// Also makes sure its phase is set as it exits.
struct SafeRegisterDeregisterProcessWithLisp {
  Process_sp _Process;
  SafeRegisterDeregisterProcessWithLisp(Process_sp p) : _Process(p)
  {
    _lisp->add_process(_Process);
  }
  ~SafeRegisterDeregisterProcessWithLisp() {
    _Process->_Phase = Exiting;
    _lisp->remove_process(_Process);
  }
};

void do_start_thread_inner(Process_sp process, core::List_sp bindings) {
  if (bindings.consp()) {
    core::Cons_sp pair = gc::As<core::Cons_sp>(CONS_CAR(bindings));
    core::DynamicScopeManager scope(pair->ocar(),core::eval::evaluate(pair->cdr(),_Nil<core::T_O>()));
    do_start_thread_inner(process,CONS_CDR(bindings));
  } else {
    core::List_sp args = process->_Arguments;
    process->_Phase = Active;
    core::T_mv result_mv;
    {
      SafeRegisterDeregisterProcessWithLisp reg(process);
      try {
        result_mv = core::eval::applyLastArgsPLUSFirst(process->_Function,args);
      } catch (ExitProcess& e) {
        // Exiting specially. Don't touch _ReturnValuesList - it's initialized to NIL just fine,
        // and may have been set by mp:exit-process.
        return;
      }
    }
    ql::list return_values;
    int nv = result_mv.number_of_values();
    if (nv > 0) {
      core::T_sp result0 = result_mv;
      return_values << result0;
      for (int i = 1; i < nv; ++i)
        return_values << result_mv.valueGet_(i);
    }
    process->_ReturnValuesList = return_values.result();
  }
}

__attribute__((noinline))
void start_thread_inner(Process_sp process, void* cold_end_of_stack) {
#ifdef USE_MPS
  // use mask
  mps_res_t res = mps_thread_reg(&process->thr_o,global_arena);
  if (res != MPS_RES_OK) {
    printf("%s:%d Could not register thread\n", __FILE__, __LINE__ );
    abort();
  }
  res = mps_root_create_thread_tagged(&process->root,
                                      global_arena,
                                      mps_rank_ambig(),
                                      0,
                                      process->thr_o,
                                      mps_scan_area_tagged_or_zero,
                                      gctools::pointer_tag_mask,
                                      gctools::pointer_tag_eq,
                                      reinterpret_cast<mps_addr_t>(const_cast<void*>(cold_end_of_stack)));
  if (res != MPS_RES_OK) {
    printf("%s:%d Could not create thread stack roots\n", __FILE__, __LINE__ );
    abort();
  };
#endif
  gctools::ThreadLocalStateLowLevel thread_local_state_low_level(cold_end_of_stack);
  core::ThreadLocalState thread_local_state;
  my_thread_low_level = &thread_local_state_low_level;
  my_thread = &thread_local_state;
//  printf("%s:%d entering start_thread  &my_thread -> %p \n", __FILE__, __LINE__, (void*)&my_thread);
#ifdef USE_MPS
  gctools::my_thread_allocation_points.initializeAllocationPoints();
#endif
  my_thread->initialize_thread(process,true);
  my_thread->create_sigaltstack();
  process->_ThreadInfo = my_thread;
  // Set the mp:*current-process* variable to the current process
  core::DynamicScopeManager scope(_sym_STARcurrent_processSTAR,process);
  core::List_sp reversed_bindings = core::cl__reverse(process->_InitialSpecialBindings);
  do_start_thread_inner(process,reversed_bindings);
};

// This is the function actually passed to pthread_create.
void* start_thread(void* claspProcess) {
  Process_sp process((Process_O*)claspProcess);
  void* cold_end_of_stack = &cold_end_of_stack;
  ////////////////////////////////////////////////////////////
  //
  // MPS setup of thread
  //
  start_thread_inner(process,cold_end_of_stack);

#ifdef DEBUG_MONITOR_SUPPORT
    // When enabled, maintain a thread-local map of strings to FILE*
    // used for logging. This is so that per-thread log files can be
    // generated.  These log files are automatically closed here when
    // the thread exits.
  for ( auto it : my_thread->_MonitorFiles ) {
    fclose(it.second);
  }
#endif
#ifdef USE_MPS
  gctools::my_thread_allocation_points.destroyAllocationPoints();
  mps_root_destroy(process->root);
  mps_thread_dereg(process->thr_o);
#endif
  my_thread->destroy_sigaltstack();
  return NULL;
}

string Mutex_O::__repr__() const {
  stringstream ss;
  ss << "#<MUTEX ";
  ss << _rep_(this->_Name);
  ss << " :owner " << _rep_(this->_Owner) << " :counter " << this->counter();
#ifdef USE_BOEHM // things don't move in boehm
  ss << " @" << (void*)(this->asSmartPtr().raw_());
#endif
  ss << ">";
  return ss.str();
}

CL_LAMBDA(mutex &optional (upgrade nil));
CL_DOCSTRING("Obtain the write lock for this mutex. upgradep should be true if and only if this thread currently holds the shared lock for the same mutex.");
CL_DEFUN void mp__write_lock(SharedMutex_sp m, bool upgrade) {
  m->write_lock(upgrade);
}

CL_LAMBDA(mutex &optional (upgrade nil));
CL_DOCSTRING("Try to obtain the write lock for this mutex. If it cannot be obtained immediately, return false. Otherwise, return true.");
CL_DEFUN bool mp__write_try_lock(SharedMutex_sp m, bool upgrade) {
  return m->write_try_lock(upgrade);
}

CL_LAMBDA(mutex &optional (release_read_lock nil));
CL_DOCSTRING("Release the write lock. If releasep is true and the current thread holds the shared lock, it is released as well.");
CL_DEFUN void mp__write_unlock(SharedMutex_sp m, bool release_read_lock) {
  m->write_unlock(release_read_lock);
}

CL_LAMBDA(mutex);
CL_DOCSTRING("Obtain the shared lock for this mutex.");
CL_DEFUN void mp__shared_lock(SharedMutex_sp m) {
  m->read_lock();
}

CL_LAMBDA(mutex);
CL_DOCSTRING("Release the shared lock for this mutex.");
CL_DEFUN void mp__shared_unlock(SharedMutex_sp m) {
  m->read_unlock();
}


void SharedMutex_O::setLockNames(core::SimpleBaseString_sp readLockName, core::SimpleBaseString_sp writeLockName)
{
  this->_SharedMutex.mReadMutex._NameWord = lisp_nameword(readLockName);
  this->_SharedMutex.mWriteMutex._NameWord = lisp_nameword(writeLockName);
}


string SharedMutex_O::__repr__() const {
  stringstream ss;
  ss << "#<SHARED-MUTEX ";
  ss << _rep_(this->_Name);
#ifdef USE_BOEHM // things don't move in boehm
  ss << " @" << (void*)(this->asSmartPtr().raw_());
#endif
  ss << ">";
  return ss.str();
}



string Process_O::__repr__() const {
  stringstream ss;
  ss << "#<PROCESS ";
  ss << _rep_(this->_Name);
#ifdef USE_BOEHM // things don't move in boehm
  ss << " @" << (void*)(this->asSmartPtr().raw_());
#endif
  ss << ">";
  return ss.str();
}

CL_DOCSTRING("Return the owner of the lock - this may be NIL if it's not locked.");
CL_DEFUN core::T_sp mp__lock_owner(Mutex_sp m) {
  return m->_Owner;
}

CL_DOCSTRING("Enable a process that has not yet been started, so that it begins executing.");
CL_DEFUN int mp__process_enable(Process_sp process)
{
  return process->enable();
};

CL_DOCSTRING("Convenience function that creates a process and then immediately enables it. Arguments are as in MAKE-PROCESS; the ARGUMENTS parameter is always NIL.");
CL_LAMBDA(name function &optional special_bindings);
CL_DEFUN Process_sp mp__process_run_function(core::T_sp name, core::T_sp function, core::List_sp special_bindings) {
#ifdef DEBUG_FASTGF
  core::Cons_sp fastgf = core::Cons_O::create(core::_sym_STARdebug_fastgfSTAR,_Nil<core::T_O>());
  special_bindings = core::Cons_O::create(fastgf,special_bindings);
#endif
  if (cl__functionp(function)) {
    Process_sp process = Process_O::make_process(name,function,_Nil<core::T_O>(),special_bindings,DEFAULT_THREAD_STACK_SIZE);
    process->enable();
    return process;
  }
  SIMPLE_ERROR(BF("%s is not a function - you must provide a function to run in a separate process") % _rep_(function));
};

CL_DOCSTRING("Return a list of all processes that have been enabled and have not yet exited, i.e. all active and suspended processes.");
CL_DEFUN core::List_sp mp__all_processes() {
  return _lisp->processes();
}

CL_DOCSTRING("Return the name of a process, as provided at its creation.");
CL_DEFUN core::T_sp mp__process_name(Process_sp p) {
  return p->_Name;
}

CL_LAMBDA(&optional (process mp:*current-process*));
CL_DEFUN core::T_sp mp__thread_id(Process_sp p) {
  auto tid = p->_ThreadInfo->_Tid;
  return core::Integer_O::create((uintptr_t)tid);
}

CL_DOCSTRING("Return true iff the process is active, i.e. is currently executed. More specifically, this means it has been enabled and is not currently suspended.");
CL_DEFUN core::T_sp mp__process_active_p(Process_sp p) {
  return (p->_Phase == Active) ? _lisp->_true() : _Nil<core::T_O>();
}

// Internal function used only in process_suspend (which is external).
// FIXME: Don't actually export.
SYMBOL_EXPORT_SC_(MpPkg,suspend_loop);
CL_DEFUN void mp__suspend_loop() {
  Process_sp this_process = gc::As<Process_sp>(_sym_STARcurrent_processSTAR->symbolValue());
  RAIILock<Mutex> lock(this_process->_SuspensionMutex);
  this_process->_Phase = Suspended;
  while (this_process->_Phase == Suspended) {
    if (!(this_process->_SuspensionCV.wait(this_process->_SuspensionMutex)))
      SIMPLE_ERROR(BF("BUG: pthread_cond_wait ran into an error"));
  }
};

CL_DOCSTRING("Stop a process from executing temporarily. Execution may be restarted with PROCESS-RESUME.");
CL_DEFUN void mp__process_suspend(Process_sp process) {
  if (process->_Phase == Active)
    mp__interrupt_process(process,_sym_suspend_loop);
  else
    SIMPLE_ERROR(BF("Cannot suspend inactive process %s") % process);
};

CL_DOCSTRING("Restart execution in a suspended process.");
CL_DEFUN void mp__process_resume(Process_sp process) {
  if (process->_Phase == Suspended) {
    RAIILock<Mutex> lock(process->_SuspensionMutex);
    process->_Phase = Active;
    if (!(process->_SuspensionCV.signal()))
      SIMPLE_ERROR(BF("BUG: pthread_cond_signal ran into an error"));
  } else
    SIMPLE_ERROR(BF("Cannot resume a process (%s) that has not been suspended") % process);
};

CL_DOCSTRING("Inform the scheduler that the current process doesn't need control for the moment. It may or may not use this information. Returns no values.");
CL_DEFUN void mp__process_yield() {
  // There doesn't appear to be any way to exit sched_yield()
  // On success, sched_yield() returns 0.
  // On error, -1 is returned, and errno is set appropriately.
  int res = sched_yield();
  if (res == -1) {
    SIMPLE_ERROR(BF("sched_yield returned the error %d") % res);
  }
//  core::clasp_musleep(0.5,true);
}

CL_DOCSTRING("Wait for the given process to finish executing. If the process's function returns normally, those values are returned. If the process exited due to EXIT-PROCESS, the values provided to that function are returned. Otherwise, the return values are undefined.");
CL_DEFUN core::T_mv mp__process_join(Process_sp process) {
  // ECL has a much more complicated process_join function
  if (process->_Phase>0) {
    pthread_join(process->_Thread,NULL);
  }
  return cl__values_list(process->_ReturnValuesList);
}

CL_DOCSTRING("Interrupt the given process to make it call the given function with no arguments. Return no values.");
CL_DEFUN void mp__interrupt_process(Process_sp process, core::T_sp func) {
  unlikely_if (mp__process_active_p(process).nilp()) {
    FEerror("Cannot interrupt the inactive process ~A", 1, process);
  }
  clasp_interrupt_process(process,func);
};

SYMBOL_EXPORT_SC_(MpPkg,exit_process);
CL_DOCSTRING("Force a process to end. This function is not intended for regular usage and is not reliable.");
CL_DEFUN void mp__process_kill(Process_sp process)
{
  mp__interrupt_process(process, _sym_exit_process);
}

CL_LAMBDA(&rest values);
CL_DOCSTRING("Immediately end the current process abnormally. The arguments to this function are returned from any PROCESS-JOIN calls with the current process as argument. Does not return.");
CL_DEFUN void mp__exit_process(core::List_sp values) {
  Process_sp this_process = gc::As<Process_sp>(_sym_STARcurrent_processSTAR->symbolValue());
  this_process->_ReturnValuesList = values;
  throw ExitProcess();
};

CL_DOCSTRING("Return the name of the mutex, as provided at creation. The mutex may be normal or recursive.");
CL_DEFUN core::T_sp mp__mutex_name(Mutex_sp m) {
  return m->_Name;
}

CL_LAMBDA(mutex &optional (waitp t));
CL_DOCSTRING("Try to obtain exclusion on the given mutex. If WAITP is true, this function will not return until exclusion is obtained.\n\nReturn true iff exclusion was obtained, otherwise false.");
CL_DEFUN bool mp__get_lock(Mutex_sp m, bool waitp) {
  return m->lock(waitp);
}

CL_LAMBDA(mutex);
CL_DOCSTRING("Release exclusion on the given mutex. Return no values.");
CL_DEFUN void mp__giveup_lock(Mutex_sp m) {
  m->unlock();
}

CL_DEFUN bool mp__recursive_lock_p(Mutex_sp m) {
  return gc::IsA<RecursiveMutex_sp>(m);
}

CL_DEFUN core::Fixnum_sp mp__lock_count(Mutex_sp m) {
  return core::clasp_make_fixnum(m->counter());
}

CL_LAMBDA(&key name)
CL_DOCSTRING("Make a new condition variable. The NAME argument is stored with the condition variable for debugging purposes.");
CL_DEFUN core::T_sp mp__make_condition_variable(core::T_sp name) {
  return ConditionVariable_O::make_ConditionVariable(name);
}

CL_DOCSTRING("Wait on the given condition variable with the given mutex. In more detail: The mutex must already be held by this thread. Then, atomically, the mutex is released and this thread blocks on the condition variable (i.e. execution will not continue).\n\nLater, the thread will resume and obtain the mutex again. Ideally this will be when the process is properly notified (see below), but occasionally the thread may resume spuriously, so make sure to check that the condition is actually true after this function returns.");
CL_DEFUN bool mp__condition_variable_wait(ConditionVariable_sp cv, Mutex_sp mutex) {
  return cv->wait(mutex);
};

CL_DOCSTRING("Like CONDITION-VARIABLE-WAIT, except that a timeout (in seconds) may be provided.");
CL_DEFUN bool mp__condition_variable_timedwait(ConditionVariable_sp cv, Mutex_sp mutex, double timeout_seconds) {
//  printf("%s:%d   timeout_seconds = %lf\n", __FILE__, __LINE__, timeout_seconds );
  return cv->timed_wait(mutex,timeout_seconds);
};

CL_DOCSTRING("Notify at least one thread that is currently waiting on the given condition variable (i.e. wake it up). If no threads are waiting on it, there is no effect. Return no values.");
CL_DEFUN void mp__condition_variable_signal(ConditionVariable_sp cv) {
  cv->signal();
};

CL_DOCSTRING("Notify all threads currently waiting on the given condition variable. If no threads are waiting on it, there is no effect. Return no values.");
CL_DEFUN void mp__condition_variable_broadcast(ConditionVariable_sp cv) {
  cv->broadcast();
};

string ConditionVariable_O::__repr__() const {
  stringstream ss;
  ss << "#<CONDITION-VARIABLE ";
  ss << _rep_(this->_Name);
  ss << ">";
  return ss.str();
}

CL_DEFUN void mp__push_default_special_binding(core::Symbol_sp symbol, core::T_sp form)
{
  _lisp->push_default_special_binding(symbol,form);
}

CL_DEFUN core::List_sp mp__copy_default_special_bindings()
{
  return _lisp->copy_default_special_bindings();
}

CL_DEFUN core::List_sp mp__process_initial_special_bindings(Process_sp p) {
  return core::cl__copy_list(p->_InitialSpecialBindings);
}

CL_DEFUN void mp__check_pending_interrupts() {
  gctools::handle_all_queued_interrupts();
}

};

