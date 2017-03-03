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
#include <thread>

#include <clasp/core/mpPackage.fwd.h>

namespace mp {
  FORWARD(Process);
  FORWARD(Mutex);
  FORWARD(RecursiveMutex);
  FORWARD(ConditionVariable);

  void start_thread(Process_sp process, core::T_sp function);

    
  class Process_O : public core::CxxObject_O {
    LISP_CLASS(mp, MpPkg, Process_O, "Process",core::CxxObject_O);
  public:
    CL_LISPIFY_NAME("make_process");
    CL_DEF_CLASS_METHOD static Process_sp make_process(core::T_sp name, core::T_sp function) {
      GC_ALLOCATE_VARIADIC(Process_O,p,name,function);
      return p;
    };
  public:
    core::T_sp  _Name;
    core::T_sp  _Function;
    core::T_sp  _ReturnValuesList;
    std::thread _Thread;
    std::mutex  _ExitBarrier;
    Process_O(core::T_sp name, core::T_sp function) : _Name(name), _Function(function), _ReturnValuesList(_Nil<core::T_O>()), _Thread(start_thread,this->asSmartPtr(),function) {};
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
    std::mutex _Mutex;
    
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
    std::recursive_mutex _Mutex;
    
    RecursiveMutex_O() {};

    CL_DEFMETHOD void lock() { this->_Mutex.lock(); };
    CL_DEFMETHOD void unlock() { this->_Mutex.unlock(); };
    CL_DEFMETHOD bool try_lock() { return this->_Mutex.try_lock(); };

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
    ConditionVariable_sp make_condition_variable();
    CL_DEF_CLASS_METHOD static ConditionVariable_sp make_ConditionVariable() {
      GC_ALLOCATE_VARIADIC(ConditionVariable_O,l);
      return l;
    };
  public:
    std::condition_variable _ConditionVariable;
    
    ConditionVariable_O() {};
    CL_DEFMETHOD void notify_one() { this->_ConditionVariable.notify_one(); };
    CL_DEFMETHOD void notify_all() { this->_ConditionVariable.notify_all(); };
//    CL_DEFMETHOD void wait(Mutex_sp m) { this->_ConditionVariable.wait(m->_UniqueMutex); };
  };

};

#endif
