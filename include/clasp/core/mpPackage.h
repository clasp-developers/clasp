#pragma once
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

#include <clasp/core/mpPackage.fwd.h>
#include <clasp/core/sequence.h> // cl__reverse
#include <clasp/gctools/park.h>

namespace mp {
FORWARD(Process);
FORWARD(Mutex);
FORWARD(SharedMutex);
FORWARD(RecursiveMutex);
FORWARD(ConditionVariable);
}; // namespace mp

namespace mp {
struct ExitProcess {};
struct AbortProcess {};

#ifdef CLASP_THREADS
/*! Keep track of binding indices for symbols */
extern Mutex global_BindingIndexPoolMutex;
extern std::vector<size_t> global_BindingIndexPool;
extern std::atomic<size_t> global_LastBindingIndex;
#endif
}; // namespace mp

#ifdef CLASP_THREADS
template <typename T> struct RAIILock {
  RAIILock(T& m) : _Mutex(m) { this->_Mutex.lock(); };
  ~RAIILock() { this->_Mutex.unlock(); }
  T& _Mutex;
};
#endif

#define DEFAULT_THREAD_STACK_SIZE 8388608
namespace mp {

typedef enum {
  Nascent = 0, // Has not yet started; proceeds to Booting when started
  Booting, // Has been started but isn't ready for Lisp; proceeds to Running
  Running, // Running normally; may proceed to Suspended or Exited
  Suspended, // Temporarily paused; may proceed to Running or Exited
  Exited // Halt state
} // Finished running, permanent state.
  ProcessPhase;
// Graph of all legal transitions:
// Nascent -> Booting -> Running -> Exited, Running -> Suspended -> Running

class Process_O : public core::CxxObject_O {
  LISP_CLASS(mp, MpPkg, Process_O, "Process", core::CxxObject_O);

public:
  CL_LISPIFY_NAME("make_process");
  CL_LAMBDA(name function &optional arguments special_bindings (stack-size 0));
  CL_DOCSTRING("Make and return a new process object. The new process is inactive; it can be started with PROCESS-START.\n\nNAME "
               "is the name of the process for display purposes. FUNCTION is the function that the process should execute. "
               "ARGUMENTS is a list of arguments that will be passed to the function when the process is enabled; the default is "
               "NIL. SPECIAL-BINDINGS is an alist of (symbol . form): the forms will be evaluated in a null lexical environment, "
               "and their values bound to the symbols (as if by PROGV) when the process is started.")
  CL_DEF_CLASS_METHOD static Process_sp make_process(core::T_sp name, core::T_sp function, core::T_sp arguments,
                                                     core::T_sp special_bindings, size_t stack_size) {
    core::List_sp passed_bindings = core::cl__reverse(special_bindings);
    core::List_sp all_bindings = core::lisp_copy_default_special_bindings();
    for (auto cur : passed_bindings) {
      all_bindings = core::Cons_O::create(oCar(cur), all_bindings);
    }
    if (stack_size == 0)
      stack_size = DEFAULT_THREAD_STACK_SIZE;
    auto p = gctools::GC<Process_O>::allocate(name, function, arguments, all_bindings, stack_size);
    return p;
  };

public:
  core::T_sp _Parent;
  core::T_sp _Name;
  core::T_sp _Function;
  core::List_sp _Arguments;
  core::List_sp _InitialSpecialBindings;
  core::List_sp _ReturnValuesList;
  bool _Aborted;
  core::T_sp _AbortCondition;
  core::ThreadLocalState* _ThreadInfo;
  std::atomic<ProcessPhase> _Phase;
  dont_expose<Mutex> _SuspensionMutex;
  dont_expose<ConditionVariable> _SuspensionCV;
  //    dont_expose<ConditionVariable> _ExitBarrier;
  size_t _StackSize;
  dont_expose<pthread_t> _TheThread;
  // Need to match fields in the two GC's
#if defined(USE_BOEHM) || defined(USE_MMTK)
  dont_expose<void*> thr_o;
  dont_expose<void*> root;
#elif defined(USE_MPS)
  dont_expose<mps_thr_t> thr_o;
  dont_expose<mps_root_t> root;
#endif
public:
  Process_O(core::T_sp name, core::T_sp function, core::List_sp arguments, core::List_sp initialSpecialBindings = nil<core::T_O>(),
            size_t stack_size = 8 * 1024 * 1024)
      : _Parent(nil<core::T_O>()), _Name(name), _Function(function), _Arguments(arguments),
        _InitialSpecialBindings(initialSpecialBindings), _ReturnValuesList(nil<core::T_O>()), _Aborted(false),
        _AbortCondition(nil<core::T_O>()), _ThreadInfo(NULL), _Phase(Nascent), _SuspensionMutex(SUSPBARR_NAMEWORD),
        _StackSize(stack_size) {
    if (!function) {
      printf("%s:%d Trying to create a process and the function is NULL\n", __FILE__, __LINE__);
    }
  };

  int startProcess();
  void run(void* stackTop); // the function the thread actually runs, mostly
  string __repr__() const override;
  inline ProcessPhase phase() const { return _Phase.load(std::memory_order_acquire); }
  string phase_as_string() const;
  void interrupt(core::T_sp interrupt);
  void suspend();
  void resume();
private:
  void runInner(core::List_sp bindings);
  inline void updatePhase(ProcessPhase n) {
    _Phase.store(n, std::memory_order_release);
    _Phase.notify_all();
  }
  // Like the above, but CASs and tells you if it worked.
  inline bool updatePhaseFrom(ProcessPhase old, ProcessPhase n) {
    bool r = _Phase.compare_exchange_strong(old, n, std::memory_order_acq_rel);
    if (r) _Phase.notify_all();
    return r;
  }
  inline void waitPhase(ProcessPhase old) {
    BEGIN_PARK { _Phase.wait(old, std::memory_order_acquire); } END_PARK;
  }
};
}; // namespace mp

template <> struct gctools::GCInfo<mp::Mutex_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace mp {

FORWARD(Mutex);
class Mutex_O : public core::CxxObject_O {
  LISP_CLASS(mp, MpPkg, Mutex_O, "Mutex", core::CxxObject_O);

public:
  CLASP_DEFAULT_CTOR Mutex_O(){};

public:
  CL_LISPIFY_NAME("make-lock");
  CL_DOCSTRING("Create and return a fresh mutex with the given name.")
  CL_LAMBDA(&key (name "Anonymous Mutex"));
  CL_DEF_CLASS_METHOD static Mutex_sp make_mutex(core::T_sp name) {
    auto l = gctools::GC<Mutex_O>::allocate(name, false);
    return l;
  };

public:
  core::T_sp _Name;
  core::T_sp _Owner;
  dont_expose<Mutex> _Mutex;
  Mutex_O(core::T_sp name, bool recursive) : _Name(name), _Owner(nil<T_O>()), _Mutex(Mutex(lisp_nameword(name), recursive)){};
  ~Mutex_O() { printf("%s:%d:%s Finalizing Mutex_O @ %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)this); }
  bool lock(bool waitp) {
    bool locked = this->_Mutex._value.lock(waitp);
    if (locked)
      this->_Owner = my_thread->_Process;
    return locked;
  };
  void unlock() {
    if (this->_Mutex._value.counter() == 1) {
      this->_Owner = nil<T_O>();
    }
    this->_Mutex._value.unlock();
  };
  gctools::Fixnum counter() const { return this->_Mutex._value.counter(); }
  string __repr__() const override;
};
}; // namespace mp

template <> struct gctools::GCInfo<mp::SharedMutex_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace mp {

FORWARD(SharedMutex);
class SharedMutex_O : public core::CxxObject_O {
  LISP_CLASS(mp, MpPkg, SharedMutex_O, "SharedMutex", core::CxxObject_O);

public:
  CL_LISPIFY_NAME("make-shared-mutex");
  CL_LAMBDA(&optional (name "Anonymous Read Mutex") (write-lock-name "Anonymous Write Mutex"));
  CL_DOCSTRING("Create and return a fresh shared mutex with the given name.")
  CL_DEF_CLASS_METHOD static SharedMutex_sp make_shared_mutex(core::T_sp readName, core::T_sp writeLockName) {
    auto l = gctools::GC<SharedMutex_O>::allocate(readName, writeLockName);
    return l;
  };

public:
  core::T_sp _Name;
  core::T_sp _Owner;
  UpgradableSharedMutex _SharedMutex;
  SharedMutex_O(core::T_sp readName, core::T_sp writeName = nil<core::T_O>())
      : _Name(readName), _Owner(nil<T_O>()),
        _SharedMutex(lisp_nameword(readName), 256, writeName.nilp() ? lisp_nameword(readName) : lisp_nameword(writeName)){};
  void write_lock(bool upgrade = false) { this->_SharedMutex.writeLock(upgrade); };
  bool write_try_lock(bool upgrade = false) { return this->_SharedMutex.writeTryLock(upgrade); };
  void write_unlock(bool release_read_lock = false) { this->_SharedMutex.writeUnlock(release_read_lock); };

  void read_lock() { this->_SharedMutex.readLock(); };
  void read_unlock() { this->_SharedMutex.readUnlock(); };
  void shared_lock() { this->_SharedMutex.readLock(); };
  void shared_unlock() { this->_SharedMutex.readUnlock(); };
  void setLockNames(core::SimpleBaseString_sp readLockName, core::SimpleBaseString_sp writeLockName);
  string __repr__() const override;

  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
      //        printf("%s:%d:%s About to initialize an mp::SharedMutex for a Package_O object\n", __FILE__, __LINE__, __FUNCTION__
      //        );
      new (&this->_SharedMutex) mp::UpgradableSharedMutex(core::lisp_nameword(this->_Name));
    }
  }
};
}; // namespace mp

template <> struct gctools::GCInfo<mp::RecursiveMutex_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace mp {

FORWARD(RecursiveMutex);
class RecursiveMutex_O : public Mutex_O {
  LISP_CLASS(mp, MpPkg, RecursiveMutex_O, "RecursiveMutex", Mutex_O);

public:
  CL_LAMBDA(&optional name);
  CL_DOCSTRING("Create and return a recursive mutex with the given name.")
  CL_DEF_CLASS_METHOD static RecursiveMutex_sp make_recursive_mutex(core::T_sp name) {
    auto l = gctools::GC<RecursiveMutex_O>::allocate(name);
    return l;
  };
  RecursiveMutex_O(core::T_sp name) : Mutex_O(name, true){};
};

}; // namespace mp

template <> struct gctools::GCInfo<mp::ConditionVariable_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace mp {

FORWARD(ConditionVariable);
class ConditionVariable_O : public core::CxxObject_O {
  LISP_CLASS(mp, MpPkg, ConditionVariable_O, "ConditionVariable", core::CxxObject_O);

public:
  static ConditionVariable_sp make_ConditionVariable(core::T_sp name) {
    auto l = gctools::GC<ConditionVariable_O>::allocate(name);
    return l;
  };

public:
  dont_expose<ConditionVariable> _ConditionVariable;
  core::T_sp _Name;
  ConditionVariable_O(core::T_sp name) : _Name(name){};
  bool wait(Mutex_sp m) { return this->_ConditionVariable._value.wait(m->_Mutex._value); };
  bool timed_wait(Mutex_sp m, double timeout) { return this->_ConditionVariable._value.timed_wait(m->_Mutex._value, timeout); };
  void signal() { this->_ConditionVariable._value.signal(); };
  void broadcast() { this->_ConditionVariable._value.broadcast(); };
  CL_DOCSTRING("Return the name of the condition variable.")
  CL_DEFMETHOD core::T_sp condition_variable_name() { return _Name; }
  string __repr__() const override;
};
void mp__interrupt_process(Process_sp process, core::T_sp func);
}; // namespace mp

namespace mp {
void posix_signal_interrupt(int);
}; // namespace mp
