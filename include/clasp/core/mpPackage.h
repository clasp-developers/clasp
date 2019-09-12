/*
    File: mpPackage.h
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

#ifndef _clasp_mpPackage_H
#define _clasp_mpPackage_H
#include <clasp/core/mpPackage.fwd.h>

namespace mp {
  FORWARD(Process);
  FORWARD(Mutex);
  FORWARD(SharedMutex);
  FORWARD(RecursiveMutex);
  FORWARD(ConditionVariable);
};


namespace mp {
  struct ExitProcess {};
  
#ifdef CLASP_THREADS
  /*! Keep track of binding indices for symbols */
  extern Mutex global_BindingIndexPoolMutex;
  extern std::vector<size_t> global_BindingIndexPool;
  extern std::atomic<size_t> global_LastBindingIndex;
#endif
};

#ifdef CLASP_THREADS
template <typename T>
struct RAIILock {
RAIILock(T& m) : _Mutex(m) {
  this->_Mutex.lock();
};
  ~RAIILock() {
    this->_Mutex.unlock();
  }
  T& _Mutex;
};
#endif


namespace mp {
  inline core::T_sp atomic_get_and_set_to_Nil(mp::SpinLock& spinlock, core::T_sp& slot) noexcept {
    mp::SafeSpinLock l(spinlock);
    core::T_sp old = slot;
    slot = _Nil<core::T_O>();
    return old;
  }
  inline void atomic_push(mp::SpinLock& spinlock, core::T_sp& slot, core::T_sp object) {
    core::Cons_sp cons = core::Cons_O::create(object,_Nil<core::T_O>());
    mp::SafeSpinLock l(spinlock);
    core::T_sp car = slot;
    cons->rplacd(car);
    slot = cons;
  }
};

#define DEFAULT_THREAD_STACK_SIZE 8388608
namespace mp {

  typedef enum {Inactive=0,Booting,Active,Exiting} ProcessPhase;
  
  class Process_O : public core::CxxObject_O {
    LISP_CLASS(mp, MpPkg, Process_O, "Process",core::CxxObject_O);
  public:
    CL_LISPIFY_NAME("make_process");
    CL_LAMBDA(name function &optional arguments special_bindings (stack-size 0))
      CL_DOCSTRING("doc(Create a process that evaluates the function with arguments. The special-bindings are bound in reverse so that earlier bindings override later ones.)doc");
    CL_DEF_CLASS_METHOD static Process_sp make_process(core::T_sp name, core::T_sp function, core::T_sp arguments, core::T_sp special_bindings, size_t stack_size) {
      core::List_sp passed_bindings = core::cl__reverse(special_bindings);
      core::List_sp all_bindings = core::lisp_copy_default_special_bindings();
      for ( auto cur : passed_bindings) {
        all_bindings = core::Cons_O::create(oCar(cur),all_bindings);
      }
      if (stack_size==0) stack_size = DEFAULT_THREAD_STACK_SIZE;
      GC_ALLOCATE_VARIADIC(Process_O,p,name,function,arguments,all_bindings,stack_size);
      return p;
    };
  public:
    core::T_sp  _Name;
    core::T_sp  _Function;
    core::List_sp  _Arguments;
    core::List_sp  _InitialSpecialBindings;
    core::List_sp  _ReturnValuesList;
    core::ThreadLocalState* _ThreadInfo;
    std::atomic<ProcessPhase>  _Phase;
    size_t _StackSize;
    pthread_t _Thread;
    ConditionVariable _Active;
    Mutex _ExitBarrier;
#ifdef USE_MPS
    mps_thr_t thr_o;
    mps_root_t root;
#endif
  public:
    Process_O(core::T_sp name, core::T_sp function, core::List_sp arguments, core::List_sp initialSpecialBindings=_Nil<core::T_O>(), size_t stack_size=8*1024*1024) : _Name(name), _Function(function), _Arguments(arguments), _InitialSpecialBindings(initialSpecialBindings), _ThreadInfo(NULL), _ReturnValuesList(_Nil<core::T_O>()), _StackSize(stack_size), _Phase(Booting), _ExitBarrier(EXITBARR_NAMEWORD) {
      if (!function) {
        printf("%s:%d Trying to create a process and the function is NULL\n", __FILE__, __LINE__ );
      }
    };
    
    int enable() {
      pthread_attr_t attr;
      int result;
      result = pthread_attr_init(&attr);
      result = pthread_attr_setstacksize(&attr,this->_StackSize);
      if (result!=0) return result;
      this->_ExitBarrier.lock();
      // I'm not sure what to do with the this->_Phase variable - if anything.
      // Does the mutex this->_ExitBarrier and the condition variable this->_Active
      // take care of all aspects of the synchronization?
      this->_Phase = Booting;
      result = pthread_create(&this->_Thread, &attr, start_thread, (void*)this );
      this->_Active.wait(this->_ExitBarrier);
      this->_ExitBarrier.unlock();
//      while (this->_Phase == Booting) {};
      pthread_attr_destroy(&attr);
      return result;
    }
    string __repr__() const;
  };
};

template <>
struct gctools::GCInfo<mp::Mutex_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace mp {

  FORWARD(Mutex);
  class Mutex_O : public core::CxxObject_O {
    LISP_CLASS(mp, MpPkg, Mutex_O, "Mutex",core::CxxObject_O);
  public:
    CL_LISPIFY_NAME("make_mutex");
    CL_LAMBDA(&optional name)
      CL_DEF_CLASS_METHOD static Mutex_sp make_mutex(core::T_sp name) {
      GC_ALLOCATE_VARIADIC(Mutex_O,l,name,false);
      return l;
    };
  public:
    core::T_sp  _Name;
    core::T_sp  _Owner;
    Mutex _Mutex;
    Mutex_O(core::T_sp name, bool recursive) : _Name(name), _Owner(_Nil<T_O>()), _Mutex(lisp_nameword(name),recursive) {};
    CL_DEFMETHOD bool lock(bool waitp) {
      bool locked = this->_Mutex.lock(waitp);
      if (locked) this->_Owner = my_thread->_Process;
      return locked;
    };
    CL_DEFMETHOD void unlock() {
      if (this->_Mutex.counter()==1) {
        this->_Owner = _Nil<T_O>();
      }
      this->_Mutex.unlock();
    };
    gctools::Fixnum counter() const{
      return this->_Mutex.counter();
    }
    string __repr__() const;
  };
};




template <>
struct gctools::GCInfo<mp::SharedMutex_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace mp {

  FORWARD(SharedMutex);
  class SharedMutex_O : public core::CxxObject_O {
    LISP_CLASS(mp, MpPkg, SharedMutex_O, "SharedMutex",core::CxxObject_O);
  public:
    CL_LISPIFY_NAME("make-shared-mutex");
    CL_LAMBDA(&optional name)
    CL_DEF_CLASS_METHOD static SharedMutex_sp make_shared_mutex(core::T_sp readName,core::T_sp writeLockName) {
      GC_ALLOCATE_VARIADIC(SharedMutex_O,l,readName,writeLockName);
      return l;
    };
  public:
    core::T_sp  _Name;
    core::T_sp  _Owner;
    UpgradableSharedMutex _SharedMutex;
    SharedMutex_O(core::T_sp readName, core::T_sp writeName=_Nil<core::T_O>()) : _Name(readName), _Owner(_Nil<T_O>()),_SharedMutex(lisp_nameword(readName), 256, writeName.nilp() ? lisp_nameword(readName) : lisp_nameword(writeName)) {};
    void write_lock(bool upgrade=false) {
      this->_SharedMutex.writeLock(upgrade);
    };
    bool write_try_lock(bool upgrade=false) {
      return this->_SharedMutex.writeTryLock(upgrade);
    };
void write_unlock(bool release_read_lock=false) {
      this->_SharedMutex.writeUnlock(release_read_lock);
    };
    
 void read_lock() {
      this->_SharedMutex.readLock();
    };
 void read_unlock() {
      this->_SharedMutex.readUnlock();
    };
 void shared_lock() {
      this->_SharedMutex.readLock();
    };
 void shared_unlock() {
      this->_SharedMutex.readUnlock();
    };
    void setLockNames(core::SimpleBaseString_sp readLockName, core::SimpleBaseString_sp writeLockName);
    string __repr__() const;
  };
};



template <>
struct gctools::GCInfo<mp::RecursiveMutex_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace mp {

  FORWARD(RecursiveMutex);
  class RecursiveMutex_O : public Mutex_O {
    LISP_CLASS(mp, MpPkg, RecursiveMutex_O, "RecursiveMutex",Mutex_O);
  public:
    CL_LAMBDA(&optional name);
    CL_DEF_CLASS_METHOD static RecursiveMutex_sp make_recursive_mutex(core::T_sp name) {
      GC_ALLOCATE_VARIADIC(RecursiveMutex_O,l, name);
      return l;
    };
  RecursiveMutex_O(core::T_sp name) :Mutex_O(name,true) {};
  };

};

template <>
struct gctools::GCInfo<mp::ConditionVariable_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace mp {

  FORWARD(ConditionVariable);
  class ConditionVariable_O : public core::CxxObject_O {
    LISP_CLASS(mp, MpPkg, ConditionVariable_O, "ConditionVariable",core::CxxObject_O);
  public:
    CL_LAMBDA(&optional name)
    CL_DEF_CLASS_METHOD static ConditionVariable_sp make_ConditionVariable(core::T_sp name) {
      GC_ALLOCATE_VARIADIC(ConditionVariable_O,l,name);
      return l;
    };
  public:
    ConditionVariable _ConditionVariable;
    core::T_sp _Name;
    ConditionVariable_O(core::T_sp name) : _Name(name) {};
    bool wait(Mutex_sp m) {return this->_ConditionVariable.wait(m->_Mutex);};
    bool timed_wait(Mutex_sp m,double timeout) {return this->_ConditionVariable.timed_wait(m->_Mutex,timeout);};
    void signal() { this->_ConditionVariable.signal();};
    void broadcast() { this->_ConditionVariable.broadcast();};
  };
  core::T_sp mp__interrupt_process(Process_sp process, core::T_sp func);
};

#endif
