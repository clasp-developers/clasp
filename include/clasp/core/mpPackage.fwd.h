#pragma once
/*
    File: mpPackage.fwd.h
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

#include <sys/time.h>
#include <cassert>
#include <atomic>
#include <thread>
#include <mutex>
#include <shared_mutex>
#include <array>
#include <unordered_map>
#include <vector>
#include <clasp/gctools/park.h> // BEGIN_PARK, END_PARK

PACKAGE_USE("COMMON-LISP");
NAMESPACE_PACKAGE_ASSOCIATION(mp, MpPkg, "MP")

namespace sf {

//
// From https://www.codeproject.com/Articles/1183423/We-Make-a-std-shared-mutex-10-Times-Faster
//
//

// contention free shared mutex (same-lock-type is recursive for X->X, X->S or S->S locks), but (S->X - is UB)
template <unsigned contention_free_count = 36, bool shared_flag = false> class contention_free_shared_mutex {
  std::atomic<bool> want_x_lock;
  // struct cont_free_flag_t { alignas(std::hardware_destructive_interference_size) std::atomic<int> value; cont_free_flag_t() {
  // value = 0; } }; // C++17
  struct cont_free_flag_t {
    char tmp[60];
    std::atomic<int> value;
    cont_free_flag_t() { value = 0; }
  }; // tmp[] to avoid false sharing
  typedef std::array<cont_free_flag_t, contention_free_count> array_slock_t;

  const std::shared_ptr<array_slock_t> shared_locks_array_ptr; // 0 - unregistred, 1 registred & free, 2... - busy
  char avoid_falsesharing_1[64];

  array_slock_t& shared_locks_array;
  char avoid_falsesharing_2[64];

  int recursive_xlock_count;

  enum index_op_t { unregister_thread_op, get_index_op, register_thread_op };

#if (_WIN32 && _MSC_VER < 1900) // only for MSVS 2013
  typedef int64_t thread_id_t;
  std::atomic<thread_id_t> owner_thread_id;
  std::array<int64_t, contention_free_count> register_thread_array;
  int64_t get_fast_this_thread_id() {
    static __declspec(thread) int64_t fast_this_thread_id = 0; // MSVS 2013 thread_local partially supported - only POD
    if (fast_this_thread_id == 0) {
      std::stringstream ss;
      ss << std::this_thread::get_id(); // https://connect.microsoft.com/VisualStudio/feedback/details/1558211
      fast_this_thread_id = std::stoll(ss.str());
    }
    return fast_this_thread_id;
  }

  int get_or_set_index(index_op_t index_op = get_index_op, int set_index = -1) {
    if (index_op == get_index_op) { // get index
      auto const thread_id = get_fast_this_thread_id();

      for (size_t i = 0; i < register_thread_array.size(); ++i) {
        if (register_thread_array[i] == thread_id) {
          set_index = i; // thread already registred
          break;
        }
      }
    } else if (index_op == register_thread_op) { // register thread
      register_thread_array[set_index] = get_fast_this_thread_id();
    }
    return set_index;
  }

#else
  typedef std::thread::id thread_id_t;
  std::atomic<std::thread::id> owner_thread_id;
  std::thread::id get_fast_this_thread_id() { return std::this_thread::get_id(); }

  struct unregister_t {
    int thread_index;
    std::shared_ptr<array_slock_t> array_slock_ptr;
    unregister_t(int index, std::shared_ptr<array_slock_t> const& ptr) : thread_index(index), array_slock_ptr(ptr) {}
    unregister_t(unregister_t&& src) : thread_index(src.thread_index), array_slock_ptr(std::move(src.array_slock_ptr)) {}
    ~unregister_t() {
      if (array_slock_ptr.use_count() > 0)
        (*array_slock_ptr)[thread_index].value--;
    }
  };

  int get_or_set_index(index_op_t index_op = get_index_op, int set_index = -1) {
    thread_local static std::unordered_map<void*, unregister_t> thread_local_index_hashmap;
    // get thread index - in any cases
    auto it = thread_local_index_hashmap.find(this);
    if (it != thread_local_index_hashmap.cend())
      set_index = it->second.thread_index;

    if (index_op == unregister_thread_op) {         // unregister thread
      if (shared_locks_array[set_index].value == 1) // if isn't shared_lock now
        thread_local_index_hashmap.erase(this);
      else
        return -1;
    } else if (index_op == register_thread_op) { // register thread
      thread_local_index_hashmap.emplace(this, unregister_t(set_index, shared_locks_array_ptr));

      // remove info about deleted contfree-mutexes
      for (auto it = thread_local_index_hashmap.begin(), ite = thread_local_index_hashmap.end(); it != ite;) {
        if (it->second.array_slock_ptr->at(it->second.thread_index).value < 0) // if contfree-mtx was deleted
          it = thread_local_index_hashmap.erase(it);
        else
          ++it;
      }
    }
    return set_index;
  }

#endif

public:
  contention_free_shared_mutex()
      : want_x_lock(false), shared_locks_array_ptr(std::make_shared<array_slock_t>()), shared_locks_array(*shared_locks_array_ptr),
        recursive_xlock_count(0), owner_thread_id(thread_id_t()) {}

  ~contention_free_shared_mutex() {
    for (auto& i : shared_locks_array)
      i.value = -1;
  }

  bool unregister_thread() { return get_or_set_index(unregister_thread_op) >= 0; }

  int register_thread() {
    int cur_index = get_or_set_index();

    if (cur_index == -1) {
      if (shared_locks_array_ptr.use_count() <= (int)shared_locks_array.size()) // try once to register thread
      {
        for (size_t i = 0; i < shared_locks_array.size(); ++i) {
          int unregistred_value = 0;
          if (shared_locks_array[i].value == 0)
            if (shared_locks_array[i].value.compare_exchange_strong(unregistred_value, 1)) {
              cur_index = i;
              get_or_set_index(register_thread_op, cur_index); // thread registred success
              break;
            }
        }
        // std::cout << "\n thread_id = " << std::this_thread::get_id() << ", register_thread_index = " << cur_index <<
        //     ", shared_locks_array[cur_index].value = " << shared_locks_array[cur_index].value << std::endl;
      }
    }
    return cur_index;
  }

  void lock_shared() {
    int const register_index = register_thread();

    if (register_index >= 0) {
      int recursion_depth = shared_locks_array[register_index].value.load(std::memory_order_acquire);
      assert(recursion_depth >= 1);

      if (recursion_depth > 1)
        shared_locks_array[register_index].value.store(recursion_depth + 1, std::memory_order_release); // if recursive -> release
      else {
        shared_locks_array[register_index].value.store(recursion_depth + 1, std::memory_order_seq_cst); // if first -> sequential
        BEGIN_PARK {
          while (want_x_lock.load(std::memory_order_seq_cst)) {
            shared_locks_array[register_index].value.store(recursion_depth, std::memory_order_seq_cst);
            for (size_t i = 0; want_x_lock.load(std::memory_order_seq_cst); ++i)
              if (i % 100000 == 0)
                std::this_thread::yield();
            shared_locks_array[register_index].value.store(recursion_depth + 1, std::memory_order_seq_cst);
          }
        } END_PARK;
      }
      // (shared_locks_array[register_index] == 2 && want_x_lock == false) ||     // first shared lock
      // (shared_locks_array[register_index] > 2)                                 // recursive shared lock
    } else {
      if (owner_thread_id.load(std::memory_order_acquire) != get_fast_this_thread_id()) {
        size_t i = 0;
        BEGIN_PARK {
          for (bool flag = false; !want_x_lock.compare_exchange_weak(flag, true, std::memory_order_seq_cst); flag = false)
            if (++i % 100000 == 0)
              std::this_thread::yield();
        } END_PARK;
        owner_thread_id.store(get_fast_this_thread_id(), std::memory_order_release);
      }
      ++recursive_xlock_count;
    }
  }

  void unlock_shared() {
    int const register_index = get_or_set_index();

    if (register_index >= 0) {
      int const recursion_depth = shared_locks_array[register_index].value.load(std::memory_order_acquire);
      assert(recursion_depth > 1);

      shared_locks_array[register_index].value.store(recursion_depth - 1, std::memory_order_release);
    } else {
      if (--recursive_xlock_count == 0) {
        owner_thread_id.store(decltype(owner_thread_id)(), std::memory_order_release);
        want_x_lock.store(false, std::memory_order_release);
      }
    }
  }

  void lock() {
    // forbidden upgrade S-lock to X-lock - this is an excellent opportunity to get deadlock
    int const register_index = get_or_set_index();
    if (register_index >= 0)
      assert(shared_locks_array[register_index].value.load(std::memory_order_acquire) == 1);

    if (owner_thread_id.load(std::memory_order_acquire) != get_fast_this_thread_id()) {
      BEGIN_PARK {
        size_t i = 0;
        for (bool flag = false; !want_x_lock.compare_exchange_weak(flag, true, std::memory_order_seq_cst); flag = false)
          if (++i % 1000000 == 0)
            std::this_thread::yield();

        owner_thread_id.store(get_fast_this_thread_id(), std::memory_order_release);

        for (auto& i : shared_locks_array)
          while (i.value.load(std::memory_order_seq_cst) > 1);
      } END_PARK;
    }

    ++recursive_xlock_count;
  }

  void unlock() {
    assert(recursive_xlock_count > 0);
    if (--recursive_xlock_count == 0) {
      owner_thread_id.store(decltype(owner_thread_id)(), std::memory_order_release);
      want_x_lock.store(false, std::memory_order_release);
    }
  }
};

template <typename mutex_t> struct shared_lock_guard {
  mutex_t& ref_mtx;
  shared_lock_guard(mutex_t& mtx) : ref_mtx(mtx) { ref_mtx.lock_shared(); }
  ~shared_lock_guard() { ref_mtx.unlock_shared(); }
};

using default_contention_free_shared_mutex = contention_free_shared_mutex<>;
// ---------------------------------------------------------------

} // namespace sf

namespace mp {
class Process_O;
typedef gctools::smart_ptr<Process_O> Process_sp;
}; // namespace mp

namespace core {
extern int clasp_musleep(double dsec, bool alertable);
}

namespace mp {

extern "C" void mutex_lock_enter(char* nameword);
extern "C" void mutex_lock_return(char* nameword);

struct DtraceLockProbe {
  char* _NameWord;
  DtraceLockProbe(char* nw) : _NameWord(nw) { mutex_lock_enter(nw); };
  ~DtraceLockProbe() { mutex_lock_return(this->_NameWord); }
};

struct Mutex;
void debug_mutex_lock(Mutex* m);
void debug_mutex_unlock(Mutex* m);

#define DEFAULT__NAMEWORD 0x0045454545454545
#define PACKAGE__NAMEWORD 0x004547414b434150
#define INTRFUNC_NAMEWORD 0x004e554652544e49
#define STRTFUNC_NAMEWORD 0x004e554654525453
#define BINDINDX_NAMEWORD 0x00444e49444e4942
#define SDISPATC_NAMEWORD 0x0053444953504154
#define ACTVTHRD_NAMEWORD 0x0052485456544341
#define SPCLBIND_NAMEWORD 0x004e49424c435053
#define SRCFILES_NAMEWORD 0x00454c4946435253
#define PKGSMUTX_NAMEWORD 0x0054554d53474b50
#define SETFDEFS_NAMEWORD 0x0046454446544553
#define SINGDISP_NAMEWORD 0x00534944474e4953
#define LOGMUTEX_NAMEWORD 0x004554554d474f4c
#define PNTRANSL_NAMEWORD 0x00534e4152544e50
#define JITLOG___NAMEWORD 0x005f474f4c54494a
#define CODEBLOK_NAMEWORD 0x004f4445424c4f4b
#define DEBGINFO_NAMEWORD 0x00464e4947424544
#define OPENDYLB_NAMEWORD 0x004c59444e45504f
#define STCKMAPS_NAMEWORD 0x0050414d4b435453
#define DISSASSM_NAMEWORD 0x0053534153534944
#define JITGDBIF_NAMEWORD 0x004942444754494a
#define MPSMESSG_NAMEWORD 0x005353454d53504d // MPSMESSG

struct Mutex {
  uint64_t _NameWord;
  pthread_mutex_t _Mutex;
  gctools::Fixnum _Counter;
  bool _Recursive;
  Mutex(uint64_t nameword, bool recursive = false) : _NameWord(nameword), _Counter(0), _Recursive(recursive) {
    if (!recursive) {
      pthread_mutex_init(&this->_Mutex, NULL);
    } else {
      pthread_mutexattr_t Attr;
      pthread_mutexattr_init(&Attr);
      pthread_mutexattr_settype(&Attr, PTHREAD_MUTEX_RECURSIVE);
      pthread_mutex_init(&this->_Mutex, &Attr);
      pthread_mutexattr_destroy(&Attr);
    }
  };
  Mutex() : _NameWord(DEFAULT__NAMEWORD), _Counter(0), _Recursive(false) { pthread_mutex_init(&this->_Mutex, NULL); };
  bool lock(bool waitp = true) {
#ifdef DEBUG_THREADS
    debug_mutex_lock(this);
#endif
    if (waitp) {
#ifdef DEBUG_DTRACE_LOCK_PROBE
      DtraceLockProbe _guard((char*)&this->_NameWord);
#endif
      bool result;
      BEGIN_PARK {
        result = (pthread_mutex_lock(&this->_Mutex) == 0);
      } END_PARK;
      ++this->_Counter;
      return result;
    }
    return pthread_mutex_trylock(&this->_Mutex) == 0;
  };
  void unlock() {
#ifdef DEBUG_THREADS
    debug_mutex_unlock(this);
#endif
    --this->_Counter;
    pthread_mutex_unlock(&this->_Mutex);
  };
  size_t counter() const { return this->_Counter; }
  ~Mutex() { pthread_mutex_destroy(&this->_Mutex); };
};

#if 0
struct SharedMutex {
  mp::Mutex _r;
  mp::Mutex _g;
  size_t _b;
  SharedMutex(uint64_t nameword) : _r(nameword,false), _g(nameword,false), _b(0) {};
    // shared access
  void shared_lock() {
    this->_r.lock(true);
    ++this->_b;
    if (this->_b==256) this->_g.lock(true);
    this->_r.unlock();
  }
  void shared_unlock() {
    this->_r.lock(true);
    --this->_b;
    if (this->_b==0) this->_g.unlock();
    this->_r.unlock();
  }
    // exclusive access
  void lock() {
    this->_g.lock(true);
  }
  void unlock() {
    this->_g.unlock();
  }
};
#else
struct SharedMutex : public sf::contention_free_shared_mutex<> {
  SharedMutex(){};
  uint64_t _r;
  SharedMutex(uint64_t nameword) : _r(nameword){};
  // shared access
  void shared_lock() { this->lock_shared(); }
  void shared_unlock() { this->unlock_shared(); }
};
#endif

inline void muSleep(uint usec) { core::clasp_musleep(usec / 1000000.0, false); };

/* I derived this code from https://oroboro.com/upgradable-read-write-locks/ */
// NOTE: We don't need to BEGIN_PARK/END_PARK since it uses Mutex, which
// already does that.
class UpgradableSharedMutex {
public:
  UpgradableSharedMutex(){};

public:
  dont_expose<Mutex> mReadMutex;
  dont_expose<Mutex> mWriteMutex;
  bool mReadsBlocked;
  uint mMaxReaders;
  uint mReaders;

public:
  UpgradableSharedMutex(uint64_t nameword, uint maxReaders = 64, uint64_t writenameword = 0)
      : mReadMutex(nameword), mWriteMutex(writenameword ? writenameword : nameword), mReadsBlocked(false), mMaxReaders(maxReaders),
        mReaders(0){};
  void readLock() {
    while (1) {
      mReadMutex._value.lock();
      if ((!mReadsBlocked) && (mReaders < mMaxReaders)) {
        mReaders++;
        mReadMutex._value.unlock();
        return;
      }
      mReadMutex._value.unlock();
      muSleep(0);
    }
    assert(0);
  };
  void readUnlock() {
    mReadMutex._value.lock();
    assert(mReaders);
    mReaders--;
    mReadMutex._value.unlock();
  };

  /* Pass true for upgrade if you want to upgrade a read lock to a write lock.
    Be careful though!!!! If two threads try to upgrade at the same time
    there will be a deadlock unless they do it in a loop using withTryLock(true).
   */
  bool writeTryLock(bool upgrade = false) {
    if (!mWriteMutex._value.lock(false))
      return false;
    waitReaders(upgrade ? 1 : 0);
    return true;
  }
  void writeLock(bool upgrade = false) {
    mWriteMutex._value.lock();
    waitReaders(upgrade ? 1 : 0);
  }
  /*! Pass true releaseReadLock if when you release the write lock it also
     releases the read lock */
  void writeUnlock(bool releaseReadLock = false) {
    mReadMutex._value.lock();
    if (releaseReadLock) {
      assert(mReaders <= 1);
      if (mReaders == 1)
        mReaders--;
    }
    mReadsBlocked = false;
    mReadMutex._value.unlock();
    mWriteMutex._value.unlock();
  }

public:
  void waitReaders(uint numReaders) {
    // block new readers
    mReadMutex._value.lock();
    mReadsBlocked = true;
    mReadMutex._value.unlock();
    // wait for current readers to finish
    while (1) {
      mReadMutex._value.lock();
      if (mReaders == numReaders) {
        mReadMutex._value.unlock();
        break;
      }
      mReadMutex._value.unlock();
      muSleep(0);
    }
    assert(mReaders == numReaders);
  }
};

struct ConditionVariable {
  pthread_cond_t _ConditionVariable;
  ConditionVariable() { pthread_cond_init(&this->_ConditionVariable, NULL); };
  ~ConditionVariable() { pthread_cond_destroy(&this->_ConditionVariable); };
  bool wait(Mutex& m) {
    BEGIN_PARK {
      return pthread_cond_wait(&this->_ConditionVariable, &m._Mutex) == 0;
    } END_PARK;
  }
  bool timed_wait(Mutex& m, double timeout) {
    struct timespec timeToWait;
    struct timeval now;
    gettimeofday(&now, NULL);
    double dtimeout_sec = floor(timeout);
    size_t timeout_sec = dtimeout_sec;
    size_t timeout_nsec = static_cast<size_t>((timeout - dtimeout_sec) * 1000000000.0);
    timeToWait.tv_sec = now.tv_sec;
    timeToWait.tv_nsec = (now.tv_usec * 1000UL);
    timeToWait.tv_sec += timeout_sec;
    timeToWait.tv_nsec += timeout_nsec;
    if (timeToWait.tv_nsec > 1000000000) {
      timeToWait.tv_sec++;
      timeToWait.tv_nsec -= 1000000000;
    }
    int rt;
    BEGIN_PARK {
      rt = pthread_cond_timedwait(&this->_ConditionVariable, &m._Mutex, &timeToWait);} END_PARK;
    return rt == 0;
  }
  bool signal() { return pthread_cond_signal(&this->_ConditionVariable) == 0; }
  bool broadcast() { return pthread_cond_broadcast(&this->_ConditionVariable) == 0; }
};

#ifdef CLASP_THREADS
#define WITH_READ_LOCK(mutex) std::shared_lock lock__(mutex)
#define WITH_READ_WRITE_LOCK(mutex) std::unique_lock lock__(mutex)
#else
#define WITH_READ_LOCK(m)
#define WITH_READ_WRITE_LOCK(m)
#endif

void* start_thread(void* info);

inline void ClaspThreads_exit() {
  //    printf("%s:%d Exiting pthread\n", __FILE__, __LINE__ );
  //    pthread_exit(NULL);
  //    printf("%s:%d Done pthread\n", __FILE__, __LINE__ );
}

class SharedMutex_O;
typedef gctools::smart_ptr<SharedMutex_O> SharedMutex_sp;
}; // namespace mp
