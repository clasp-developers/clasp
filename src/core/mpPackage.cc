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

#ifdef DEBUG_THREADS
void debug_mutex_lock(Mutex* m) {
  if (core::_sym_STARdebug_threadsSTAR
      && !core::_sym_STARdebug_threadsSTAR.unboundp()
      && core::_sym_STARdebug_threadsSTAR->boundP()
      && core::_sym_STARdebug_threadsSTAR->symbolValue().notnilp()) {
    printf("%s:%d     LOCKING mutex@%p\n", __FILE__, __LINE__, (void*)m);
    fflush(stdout);
  }
};
void debug_mutex_unlock(Mutex* m) {
  if (core::_sym_STARdebug_threadsSTAR
      && !core::_sym_STARdebug_threadsSTAR.unboundp()
      && core::_sym_STARdebug_threadsSTAR->boundP()
      && core::_sym_STARdebug_threadsSTAR->symbolValue().notnilp()) {
    printf("%s:%d   Unlocking mutex@%p\n", __FILE__, __LINE__, (void*)m);
    fflush(stdout);
  }
};
#endif

struct RAIIMutexLock {
  Mutex _Mutex;
  RAIIMutexLock(Mutex& m) : _Mutex(m) {
    int res = this->_Mutex.lock();
    printf("%s:%d RAIIMutexLock res = %d\n", __FILE__, __LINE__, res );
  };
  ~RAIIMutexLock() {
    this->_Mutex.unlock();
  }
};

};

namespace mp {

#ifdef CLASP_THREADS
std::atomic<size_t> global_LastBindingIndex = ATOMIC_VAR_INIT(0);
Mutex global_BindingIndexPoolMutex(BINDINDX_NAMEWORD,false);
std::vector<size_t> global_BindingIndexPool;
#endif


SYMBOL_SC_(MpPkg, aSingleMpSymbol);
SYMBOL_EXPORT_SC_(MpPkg, STARcurrent_processSTAR);
SYMBOL_EXPORT_SC_(MpPkg, roo);

struct SafeRegisterDeregisterProcessWithLisp {
  Process_sp _Process;
  SafeRegisterDeregisterProcessWithLisp(Process_sp p) : _Process(p)
  {
    _lisp->add_process(_Process);
  }
  ~SafeRegisterDeregisterProcessWithLisp() {
    _lisp->remove_process(_Process);
  }
};


__attribute__((noinline))
void start_thread_inner(Process_sp process, void* cold_end_of_stack) {
  process->_ExitBarrier.lock();
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
  for ( auto cur : reversed_bindings ) {
    core::Cons_sp pair = gc::As<core::Cons_sp>(oCar(cur));
//    printf("%s:%d  start_thread   setting special variable/(eval value) -> %s\n", __FILE__, __LINE__, _rep_(pair).c_str());
    scope.pushSpecialVariableAndSet(pair->_Car,core::eval::evaluate(pair->_Cdr,_Nil<core::T_O>()));
  }
//  gctools::register_thread(process,stack_base);
  core::List_sp args = process->_Arguments;
  
#if 0
#ifdef USE_BOEHM
  GC_stack_base gc_stack_base;
  GC_get_stack_base(&gc_stack_base);
  GC_register_my_thread(&gc_stack_base);
#endif
#endif

  process->_Phase = Active;
  process->_Active.signal();
  process->_ExitBarrier.unlock();
  core::T_mv result_mv;
  {
    SafeRegisterDeregisterProcessWithLisp reg(process);
//    RAIIMutexLock exitBarrier(p->_ExitBarrier);
//    printf("%s:%d:%s  process locking the ExitBarrier\n", __FILE__, __LINE__, __FUNCTION__);
    try {
      result_mv = core::eval::applyLastArgsPLUSFirst(process->_Function,args);
    } catch (ExitProcess& e) {
      // Do nothing - exiting
    }
//    printf("%s:%d:%s  process releasing the ExitBarrier\n", __FILE__, __LINE__, __FUNCTION__);
  }
  process->_Phase = Exiting;
  core::T_sp result0 = result_mv;
  core::List_sp result_list = _Nil<core::T_O>();
  for ( int i=result_mv.number_of_values(); i>0; --i ) {
    result_list = core::Cons_O::create(result_mv.valueGet_(i),result_list);
  }
  result_list = core::Cons_O::create(result0,result_list);
  process->_ReturnValuesList = result_list;
  
//  gctools::unregister_thread(process);
//  printf("%s:%d leaving start_thread\n", __FILE__, __LINE__);

};


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
#if 0
#ifdef USE_BOEHM
  GC_unregister_my_thread();
#endif
#endif
#ifdef USE_MPS
  gctools::my_thread_allocation_points.destroyAllocationPoints();
  mps_root_destroy(process->root);
  mps_thread_dereg(process->thr_o);
#endif
  my_thread->destroy_sigaltstack();
//  printf("%s:%d  really leaving start_thread\n", __FILE__, __LINE__ );
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

CL_LAMBDA(m &optional (upgrade nil))
CL_DEFUN void mp__write_lock(SharedMutex_sp m, bool upgrade) {
  m->write_lock(upgrade);
}

CL_LAMBDA(m &optional (upgrade nil))
CL_DEFUN bool mp__write_try_lock(SharedMutex_sp m, bool upgrade) {
  return m->write_try_lock(upgrade);
}

CL_LAMBDA(m &optional (release_read_lock nil))
CL_DEFUN void mp__write_unlock(SharedMutex_sp m, bool release_read_lock) {
  m->write_unlock(release_read_lock);
}

CL_DEFUN void mp__read_lock(SharedMutex_sp m) {
  m->read_lock();
}

CL_DEFUN void mp__read_unlock(SharedMutex_sp m) {
  m->read_unlock();
}

CL_DEFUN void mp__shared_lock(SharedMutex_sp m) {
  m->read_lock();
}

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

CL_DEFUN int mp__process_enable(Process_sp process)
{
  return process->enable();
};

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

CL_DEFUN core::List_sp mp__all_processes() {
  return _lisp->processes();
}

CL_DEFUN core::T_sp mp__process_name(Process_sp p) {
  return p->_Name;
}

CL_LAMBDA(&optional (process mp:*current-process*));
CL_DEFUN core::T_sp mp__thread_id(Process_sp p) {
  auto tid = p->_ThreadInfo->_Tid;
  return core::Integer_O::create((uintptr_t)tid);
}



CL_LAMBDA(&key name recursive)
CL_DEFUN core::T_sp mp__make_lock(core::T_sp name, bool recursive) {
  if (!recursive) {
    return Mutex_O::make_mutex(name);
  }
  return RecursiveMutex_O::make_recursive_mutex(name);
}
  
CL_DEFUN core::T_sp mp__process_active_p(Process_sp p) {
  return (p->_Phase == Active) ? _lisp->_true() : _Nil<core::T_O>();
}

SYMBOL_EXPORT_SC_(MpPkg,suspend_loop);
SYMBOL_EXPORT_SC_(MpPkg,break_suspend_loop);
CL_DEFUN void mp__suspend_loop() {
  printf("%s:%d %s\n", __FILE__, __LINE__, __FUNCTION__);
  SafeExceptionStackPush save(&my_thread->exceptionStack(), core::CatchFrame,_sym_suspend_loop);
  for ( ; ; ) {
    core::cl__sleep(core::make_fixnum(100));
  }
};

CL_DEFUN void mp__break_suspend_loop() {
  printf("%s:%d %s\n", __FILE__, __LINE__, __FUNCTION__);
  core::core__throw_function(_sym_suspend_loop,_Nil<core::T_O>());
};

CL_DEFUN void mp__process_suspend(Process_sp process) {
  printf("%s:%d %s\n", __FILE__, __LINE__, __FUNCTION__);
  mp__interrupt_process(process,_sym_suspend_loop);
};

CL_DEFUN void mp__process_resume(Process_sp process) {
  printf("%s:%d %s\n", __FILE__, __LINE__, __FUNCTION__);
  mp__interrupt_process(process,_sym_break_suspend_loop);
};

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

CL_DEFUN core::T_mv mp__process_join(Process_sp process) {
  // ECL has a much more complicated process_join function
  if (process->_Phase>0) {
    pthread_join(process->_Thread,NULL);
#if 0
    printf("%s:%d:%s About to lock the ExitBarrier\n", __FILE__,__LINE__,__FUNCTION__);
    RAIIMutexLock join_(process->_ExitBarrier);
    printf("          ExitBarrier count = %ld\n", join_._Mutex._Counter);
    printf("%s:%d:%s Releasing the ExitBarrier\n", __FILE__,__LINE__,__FUNCTION__);
#endif
  }
  return cl__values_list(process->_ReturnValuesList);
}


    
CL_DEFUN core::T_sp mp__interrupt_process(Process_sp process, core::T_sp func) {
  unlikely_if (mp__process_active_p(process).nilp()) {
    FEerror("Cannot interrupt the inactive process ~A", 1, process);
  }
  clasp_interrupt_process(process,func);
  return _lisp->_true();
};

SYMBOL_EXPORT_SC_(MpPkg,exit_process);
CL_DEFUN core::T_sp mp__process_kill(Process_sp process)
{
  return mp__interrupt_process(process, _sym_exit_process);
}


CL_DEFUN void mp__exit_process() {
  throw ExitProcess();
};


CL_DEFUN core::T_sp mp__mutex_name(Mutex_sp m) {
  return m->_Name;
}

CL_LAMBDA(m &optional (waitp t));
CL_DEFUN bool mp__get_lock(Mutex_sp m, bool waitp) {
  return m->lock(waitp);
}


CL_DEFUN bool mp__recursive_lock_p(Mutex_sp m) {
  return gc::IsA<RecursiveMutex_sp>(m);
}


CL_DEFUN bool mp__giveup_lock(Mutex_sp m) {
  m->unlock();
  return true;
}

CL_DEFUN core::Fixnum_sp mp__lock_count(Mutex_sp m) {
  return core::clasp_make_fixnum(m->counter());
}
CL_LAMBDA(&key name)
CL_DEFUN core::T_sp mp__make_condition_variable(core::T_sp name) {
  return ConditionVariable_O::make_ConditionVariable(name);
}

CL_DEFUN bool mp__condition_variable_wait(ConditionVariable_sp cv, Mutex_sp mutex) {
  return cv->wait(mutex);
};

CL_DEFUN bool mp__condition_variable_timedwait(ConditionVariable_sp cv, Mutex_sp mutex, double timeout_seconds) {
//  printf("%s:%d   timeout_seconds = %lf\n", __FILE__, __LINE__, timeout_seconds );
  return cv->timed_wait(mutex,timeout_seconds);
};

CL_DEFUN void mp__condition_variable_signal(ConditionVariable_sp cv) {
  cv->signal();
};

CL_DEFUN void mp__condition_variable_broadcast(ConditionVariable_sp cv) {
  cv->broadcast();
};

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

