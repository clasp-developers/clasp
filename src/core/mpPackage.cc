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
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/core/symbol.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/primitives.h>
#include <clasp/core/package.h>
#include <clasp/gctools/interrupt.h>
#include <clasp/core/evaluator.h>



namespace mp {

struct RAIIMutexLock {
  Mutex _Mutex;
  RAIIMutexLock(Mutex& m) : _Mutex(m) {
    this->_Mutex.lock();
  };
  ~RAIIMutexLock() {
    this->_Mutex.unlock();
  }
};

};

namespace mp {

#ifdef CLASP_THREADS
std::atomic<size_t> global_LastBindingIndex = ATOMIC_VAR_INIT(0);
GlobalMutex global_BindingIndexPoolMutex(false);
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

void* start_thread(void* claspProcess) {
  Process_O* my_claspProcess = (Process_O*)claspProcess;
  Process_sp p(my_claspProcess);
  void* stack_base;
  core::ThreadLocalState my_thread_local_state(&stack_base);
  printf("%s:%d entering start_thread  &my_thread -> %p \n", __FILE__, __LINE__, (void*)&my_thread);
  my_thread = &my_thread_local_state;
  my_thread->initialize_thread();
  p->_ThreadInfo = my_thread;
  // Set the mp:*current-process* variable to the current process
  core::DynamicScopeManager scope(_sym_STARcurrent_processSTAR,p);
#if 0
#ifdef USE_BOEHM
  GC_stack_base gc_stack_base;
  GC_get_stack_base(&gc_stack_base);
  GC_register_my_thread(&gc_stack_base);
#endif
#endif
#ifdef USE_MPS
  printf("%s:%d Handle threads for MPS\n", __FILE__, __LINE__ );
  abort();
#endif
//  gctools::register_thread(process,stack_base);
  core::List_sp args = my_claspProcess->_Arguments;
  p->_Phase = Active;
  core::T_mv result_mv;
  {
    SafeRegisterDeregisterProcessWithLisp reg(p);
    RAIIMutexLock exitBarrier(p->_ExitBarrier);
    result_mv = core::eval::applyLastArgsPLUSFirst(my_claspProcess->_Function,args);
  }
  p->_Phase = Exiting;
  core::T_sp result0 = result_mv;
  core::List_sp result_list = _Nil<core::T_O>();
  for ( int i=result_mv.number_of_values(); i>0; --i ) {
    result_list = core::Cons_O::create(result_mv.valueGet_(i),result_list);
  }
  result_list = core::Cons_O::create(result0,result_list);
  my_claspProcess->_ReturnValuesList = result_list;
//  gctools::unregister_thread(process);
  printf("%s:%d leaving start_thread\n", __FILE__, __LINE__);
#if 0
#ifdef USE_BOEHM
  GC_unregister_my_thread();
#endif
#endif
#ifdef USE_MPS
  printf("%s:%d Handle threads for MPS\n", __FILE__, __LINE__ );
  abort();
#endif
  printf("%s:%d  really leaving start_thread\n", __FILE__, __LINE__ );
  return NULL;
}




CL_DEFUN Process_sp mp__lock_owner(Mutex_sp m) {
  return m->_Owner;
}

CL_DEFUN int mp__process_enable(Process_sp process)
{
  return process->enable();
};

CL_DEFUN core::List_sp mp__all_processes() {
  return _lisp->processes();
}

CL_DEFUN core::T_sp mp__process_name(Process_sp p) {
  return p->_Name;
}

CL_LAMBDA(&key name (recursive nil))
CL_DEFUN core::T_sp mp__make_lock(core::T_sp name, bool recursive) {
  if (!recursive) {
    return Mutex_O::make_mutex(name);
  }
  return RecursiveMutex_O::make_recursive_mutex(name);
}
  
CL_DEFUN core::T_sp mp__process_active_p(Process_sp p) {
  return p->_Phase ? _lisp->_true() : _Nil<core::T_O>();
}


CL_DEFUN void mp__process_suspend(Process_sp process) {
  printf("%s:%d  process_suspend - implement me\n", __FILE__, __LINE__ );
};

CL_DEFUN void mp__process_resume(Process_sp process) {
  printf("%s:%d  process_resume - implement me\n", __FILE__, __LINE__ );
};

CL_DEFUN void mp__process_yield() {
  int res = sched_yield();
//  core::clasp_musleep(0.0,true);
}

CL_DEFUN core::T_mv mp__process_join(Process_sp process) {
  // ECL has a much more complicated process_join function
  if (process->_Phase>0) {
    RAIIMutexLock join_(process->_ExitBarrier);
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

CL_DEFUN core::T_sp mp__mutex_name(Mutex_sp m) {
  return m->_Name;
}

CL_DEFUN bool mp__get_lock(Mutex_sp m, bool waitp) {
  return m->lock(waitp);
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

CL_DEFUN void mp__condition_variable_signal(ConditionVariable_sp cv) {
  cv->signal();
};


};

