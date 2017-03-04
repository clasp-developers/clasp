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
#include <clasp/core/foundation.h>
#include <clasp/core/mpPackage.fwd.h>

namespace mp {
  FORWARD(Process);
  FORWARD(Mutex);
  FORWARD(RecursiveMutex);
  FORWARD(ConditionVariable);
};


namespace mp {

  struct GlobalMutex {
    pthread_mutex_t _Mutex;
    GlobalMutex() {
      this->_Mutex = PTHREAD_MUTEX_INITIALIZER;
    };
    void lock() { pthread_mutex_lock(&this->_Mutex); };
    void unlock() { pthread_mutex_unlock(&this->_Mutex); };
    bool try_lock() { return pthread_mutex_trylock(&this->_Mutex)==0; };
    ~GlobalMutex() {
    };
  };

  struct GlobalRecursiveMutex {
    pthread_mutex_t _Mutex;
    GlobalRecursiveMutex() {
#if defined(_TARGET_OS_LINUX)
      this->_Mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#elif defined(_TARGET_OS_DARWIN)
      this->_Mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER;
#else
      #error "You need to initialize this->_Mutex"
#endif
    };
    void lock() { pthread_mutex_lock(&this->_Mutex); };
    void unlock() { pthread_mutex_lock(&this->_Mutex); };
    bool try_lock() { return pthread_mutex_trylock(&this->_Mutex)==0; };
    ~GlobalRecursiveMutex() {
    };
  };
  
  struct Mutex {
    pthread_mutex_t _Mutex;
    Mutex() {
      pthread_mutex_init(&this->_Mutex,NULL);
    };
    void lock() { pthread_mutex_lock(&this->_Mutex); };
    void unlock() { pthread_mutex_lock(&this->_Mutex); };
    bool try_lock() { return pthread_mutex_trylock(&this->_Mutex)==0; };
    ~Mutex() {
      pthread_mutex_destroy(&this->_Mutex);
    };
  };

  struct RecursiveMutex {
    pthread_mutex_t _Mutex;
    RecursiveMutex() {
      pthread_mutexattr_t Attr;
      pthread_mutexattr_init(&Attr);
      pthread_mutexattr_settype(&Attr, PTHREAD_MUTEX_RECURSIVE);
      pthread_mutex_init(&this->_Mutex,&Attr);
      pthread_mutexattr_destroy(&Attr);
    };
    void lock() { pthread_mutex_lock(&this->_Mutex); };
    void unlock() { pthread_mutex_lock(&this->_Mutex); };
    bool try_lock() { return pthread_mutex_trylock(&this->_Mutex)==0; };
    ~RecursiveMutex() {
      pthread_mutex_destroy(&this->_Mutex);
    };
  };

  struct ConditionVariable {
    pthread_cond_t _ConditionVariable;
    ConditionVariable() {
      pthread_cond_init(&this->_ConditionVariable,NULL);
    };
    ~ConditionVariable() {
      pthread_cond_destroy(&this->_ConditionVariable);
    };
  };


  
  void* start_thread(void* claspProcess);

  inline void ClaspThreads_exit() {
//    printf("%s:%d Exiting pthread\n", __FILE__, __LINE__ );
//    pthread_exit(NULL);
//    printf("%s:%d Done pthread\n", __FILE__, __LINE__ );
  }
};

namespace mp {
#ifdef CLASP_THREADS
  /*! Keep track of binding indices for symbols */
  extern GlobalMutex global_BindingIndexPoolMutex;
  extern std::vector<size_t> global_BindingIndexPool;
  extern std::atomic<size_t> global_LastBindingIndex;
#endif
};

#ifdef CLASP_THREADS
template <typename T>
struct SafeMutex {
SafeMutex(T& m) : _Mutex(m) {
  this->_Mutex.lock();
};
  ~SafeMutex() {
    this->_Mutex.unlock();
  }
  T& _Mutex;
};
#endif

      
namespace mp {
    
  class Process_O : public core::CxxObject_O {
    LISP_CLASS(mp, MpPkg, Process_O, "Process",core::CxxObject_O);
  public:
    CL_LISPIFY_NAME("make_process");
    CL_LAMBDA(name function &optional arguments (stack-size 8388608))
    CL_DEF_CLASS_METHOD static Process_sp make_process(core::T_sp name, core::T_sp function, core::T_sp arguments, size_t stack_size) {
      GC_ALLOCATE_VARIADIC(Process_O,p,name,function,arguments,stack_size);
      return p;
    };
  public:
    core::T_sp  _Name;
    core::T_sp _Function;
    core::T_sp _Arguments;
    core::T_sp  _ReturnValuesList;
    size_t _StackSize;
    pthread_t _Thread;
//    pthread_mutex_t  _ExitBarrier;
  public:
  Process_O(core::T_sp name, core::T_sp function, core::T_sp arguments, size_t stack_size=8*1024*1024) : _Name(name), _Function(function), _Arguments(arguments), _ReturnValuesList(_Nil<core::T_O>()), _StackSize(stack_size) {};
    
    int enable() {
      pthread_attr_t attr;
      int result;
      result = pthread_attr_init(&attr);
      result = pthread_attr_setstacksize(&attr,this->_StackSize);
      if (result!=0) return result;
      result = pthread_create(&this->_Thread, &attr, start_thread, (void*)this );
      pthread_attr_destroy(&attr);
      return result;
    }
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
    CL_DEF_CLASS_METHOD static Mutex_sp make_mutex() {
      GC_ALLOCATE_VARIADIC(Mutex_O,l);
      return l;
    };
  public:
    Mutex _Mutex;
    Mutex_O() {};
    CL_DEFMETHOD void lock() { this->_Mutex.lock(); };
    CL_DEFMETHOD void unlock() { this->_Mutex.unlock(); };
    CL_DEFMETHOD bool try_lock() { return this->_Mutex.try_lock(); };
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
  class RecursiveMutex_O : public core::CxxObject_O {
    LISP_CLASS(mp, MpPkg, RecursiveMutex_O, "RecursiveMutex",core::CxxObject_O);
  public:
    CL_DEF_CLASS_METHOD static RecursiveMutex_sp make_recursive_mutex() {
      GC_ALLOCATE_VARIADIC(RecursiveMutex_O,l);
      return l;
    };
  public:
    RecursiveMutex _Mutex;
    RecursiveMutex_O() {};
    CL_DEFMETHOD void lock() { this->_Mutex.lock(); };
    CL_DEFMETHOD void unlock() { this->_Mutex.unlock(); };
    CL_DEFMETHOD bool try_lock() { return this->_Mutex.try_lock(); };
  };

};

#if 0
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
    ConditionVariable_sp make_condition_variable();
    CL_DEF_CLASS_METHOD static ConditionVariable_sp make_ConditionVariable() {
      GC_ALLOCATE_VARIADIC(ConditionVariable_O,l);
      return l;
    };
  public:
    ConditionVariable _ConditionVariable;
    
    ConditionVariable_O() {};
//    CL_DEFMETHOD void notify_one() { this->_ConditionVariable.notify_one(); };
//    CL_DEFMETHOD void notify_all() { this->_ConditionVariable.notify_all(); };
//    CL_DEFMETHOD void wait(Mutex_sp m) { this->_ConditionVariable.wait(m->_UniqueMutex); };
  };

};
#endif

#endif
