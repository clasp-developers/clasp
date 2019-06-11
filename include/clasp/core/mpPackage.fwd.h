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
#ifndef mpPackage_fwd_H
#define mpPackage_fwd_H

#include <sys/time.h>
#include <cassert>

PACKAGE_USE("COMMON-LISP");
NAMESPACE_PACKAGE_ASSOCIATION(mp, MpPkg, "MP")

namespace mp {
  class Process_O;
  typedef gctools::smart_ptr<Process_O> Process_sp;
};

namespace core {
  extern int clasp_musleep(double dsec, bool alertable);
}

namespace mp {
  struct SpinLock {
  public:
    void lock()
    {
      while(lck.test_and_set(std::memory_order_acquire))
      {}
    }
 
    void unlock()
    {
      lck.clear(std::memory_order_release);
    }
 
  private:
    std::atomic_flag lck = ATOMIC_FLAG_INIT;
  };
  struct SafeSpinLock {
    SpinLock& _SpinLock;
  SafeSpinLock(SpinLock& l) : _SpinLock(l) {
    _SpinLock.lock();
  };
    ~SafeSpinLock() {
      _SpinLock.unlock();
    }
  };
    

extern "C" void mutex_lock_enter(char* nameword);
extern "C" void mutex_lock_return(char* nameword);

struct DtraceLockProbe {
  char* _NameWord;
  DtraceLockProbe(char* nw) : _NameWord(nw) {
    mutex_lock_enter(nw);
  };
  ~DtraceLockProbe() {
    mutex_lock_return(this->_NameWord);
  }
};

struct Mutex;
void debug_mutex_lock(Mutex* m);
void debug_mutex_unlock(Mutex* m);

#define PACKAGE__NAMEWORD 0x004547414b434150
#define INTRFUNC_NAMEWORD 0x004e554652544e49
#define STRTFUNC_NAMEWORD 0x004e554654525453
#define BINDINDX_NAMEWORD 0x00444e49444e4942
#define ACTVTHRD_NAMEWORD 0x0052485456544341
#define SPCLBIND_NAMEWORD 0x004e49424c435053
#define SYSPROC__NAMEWORD 0x00434f5250535953
#define CLASSTBL_NAMEWORD 0x0042545353414c43
#define SRCFILES_NAMEWORD 0x00454c4946435253
#define PKGSMUTX_NAMEWORD 0x0054554d53474b50
#define SETFDEFS_NAMEWORD 0x0046454446544553
#define SINGDISP_NAMEWORD 0x00534944474e4953
#define LOGMUTEX_NAMEWORD 0x004554554d474f4c
#define PNTRANSL_NAMEWORD 0x00534e4152544e50
#define UNIXSIGN_NAMEWORD 0x0047495358494e55
#define DEBGINFO_NAMEWORD 0x00464e4947424544
#define OPENDYLB_NAMEWORD 0x004c59444e45504f
#define STCKMAPS_NAMEWORD 0x0050414d4b435453
#define JITDOBJS_NAMEWORD 0x004a424f4454494a
#define EXITBARR_NAMEWORD 0x0052414254495845
#define DISSASSM_NAMEWORD 0x0053534153534944
#define JITGDBIF_NAMEWORD 0x004942444754494a

struct Mutex {
  uint64_t _NameWord;
  pthread_mutex_t _Mutex;
  gctools::Fixnum _Counter;
  bool _Recursive;
#if 0
  Mutex() : _Counter(0), _Recursive(false) {
    pthread_mutex_init(&this->_Mutex,NULL);
  }
#endif
  Mutex(uint64_t nameword, bool recursive=false) : _NameWord(nameword), _Counter(0), _Recursive(recursive) {
    
    if (!recursive) {
//      printf("%s:%d Creating Mutex@%p\n", __FILE__, __LINE__, (void*)&this->_Mutex);
      pthread_mutex_init(&this->_Mutex,NULL);
    } else {
      pthread_mutexattr_t Attr;
      pthread_mutexattr_init(&Attr);
      pthread_mutexattr_settype(&Attr, PTHREAD_MUTEX_RECURSIVE);
      pthread_mutex_init(&this->_Mutex, &Attr);
      pthread_mutexattr_destroy(&Attr);
    }
  };
  bool lock(bool waitp=true) {
//      printf("%s:%d locking Mutex@%p\n", __FILE__, __LINE__, (void*)&this->_Mutex); fflush(stdout);
#ifdef DEBUG_THREADS
    debug_mutex_lock(this);
#endif
    if (waitp) {
#ifdef DEBUG_DTRACE_LOCK_PROBE
      DtraceLockProbe _guard((char*)&this->_NameWord);
#endif
      bool result = (pthread_mutex_lock(&this->_Mutex)==0);
      ++this->_Counter;
      return result;
    }
    return pthread_mutex_trylock(&this->_Mutex)==0;
  };
  void unlock() {
//      printf("%s:%d unlocking Mutex@%p\n", __FILE__, __LINE__, (void*)&this->_Mutex); fflush(stdout);
#ifdef DEBUG_THREADS
    debug_mutex_unlock(this);
#endif
    --this->_Counter;
    pthread_mutex_unlock(&this->_Mutex);
  };
  size_t counter() const {
    return this->_Counter;
  }
  ~Mutex() {
    pthread_mutex_destroy(&this->_Mutex);
  };
};

  

  struct RecursiveMutex : public Mutex {
    RecursiveMutex(uint64_t nameword) : Mutex(nameword,true) {};
  };

  
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

  inline void muSleep(uint usec) { core::clasp_musleep(usec/1000000.0,false); };

/* I derived this code from https://oroboro.com/upgradable-read-write-locks/ */
  class UpgradableSharedMutex {
  public:
    Mutex mReadMutex;
    Mutex mWriteMutex;
    bool    mReadsBlocked;
    uint     mMaxReaders;
    uint     mReaders;
  public:
    UpgradableSharedMutex(uint64_t nameword, uint maxReaders = 64, uint64_t writenameword=0 ) :
      mReadMutex(nameword),
      mWriteMutex(writenameword ? writenameword : nameword),
      mReadsBlocked( false ), mMaxReaders( maxReaders ), mReaders( 0 ) {};
    void readLock() {
      while ( 1 ) {
        mReadMutex.lock();
        if (( !mReadsBlocked ) && ( mReaders < mMaxReaders )) {
          mReaders++; 
          mReadMutex.unlock();
          return;
        }
        mReadMutex.unlock();
        muSleep( 0 );
      }
      assert( 0 );
    };
    void readUnlock() { 
      mReadMutex.lock(); 
      assert( mReaders );
      mReaders--; 
      mReadMutex.unlock(); 
    };

    /* Pass true for upgrade if you want to upgrade a read lock to a write lock.
      Be careful though!!!! If two threads try to upgrade at the same time 
      there will be a deadlock unless they do it in a loop using withTryLock(true).
      See the hashTable.cc rehash_upgrade_write_lock for an example. */
    bool writeTryLock(bool upgrade = false) {
      if ( !mWriteMutex.lock(false))
        return false;
      waitReaders( upgrade ? 1 : 0 );
      return true;
    }
    void writeLock(bool upgrade = false) {
      mWriteMutex.lock(); 
      waitReaders( upgrade ? 1 : 0 );
    }
    /*! Pass true releaseReadLock if when you release the write lock it also
       releases the read lock */
    void writeUnlock(bool releaseReadLock = false) {
      mReadMutex.lock();
      if ( releaseReadLock ) {
        assert( mReaders <= 1 );
        if ( mReaders == 1 )
          mReaders--; 
      }
      mReadsBlocked = false;
      mReadMutex.unlock();
      mWriteMutex.unlock(); 
    }
  public:
    void waitReaders(uint numReaders) {
   // block new readers 
      mReadMutex.lock();
      mReadsBlocked = true;
      mReadMutex.unlock();  
   // wait for current readers to finish
      while ( 1 ) {
        mReadMutex.lock();
        if ( mReaders == numReaders )
        {
          mReadMutex.unlock();  
          break;
        }
        mReadMutex.unlock();  
        muSleep( 0 );
      }
      assert( mReaders == numReaders );
    }
  };


  struct ConditionVariable {
    pthread_cond_t _ConditionVariable;
    ConditionVariable() {
      pthread_cond_init(&this->_ConditionVariable,NULL);
    };
    ~ConditionVariable() {
      pthread_cond_destroy(&this->_ConditionVariable);
    };
    bool wait(Mutex& m) {
      return pthread_cond_wait(&this->_ConditionVariable,&m._Mutex)==0;
    }
    bool timed_wait(Mutex& m, double timeout) {
      struct timespec timeToWait;
      struct timeval now;
      gettimeofday(&now,NULL);
      double dtimeout_sec = floor(timeout);
      size_t timeout_sec = dtimeout_sec;
      size_t timeout_nsec = static_cast<size_t>((timeout-dtimeout_sec)*1000000000.0);
      timeToWait.tv_sec = now.tv_sec;
      timeToWait.tv_nsec = (now.tv_usec*1000UL);
#if 0
      printf("%s:%d pthread_cond_timedwait    timeout = %lf\n",  __FILE__, __LINE__, timeout);
      printf("%s:%d pthread_cond_timedwait    timeout_sec = %lu  timeout_nsec = %lu\n", __FILE__, __LINE__, timeout_sec, timeout_nsec );
      printf("%s:%d pthread_cond_timedwait    now.tv_sec = %lu  now.tv_nsec = %lu\n", __FILE__, __LINE__, timeToWait.tv_sec, timeToWait.tv_nsec );
#endif
      timeToWait.tv_sec += timeout_sec;
      timeToWait.tv_nsec += timeout_nsec;
      if (timeToWait.tv_nsec>1000000000) {
        timeToWait.tv_sec++;
        timeToWait.tv_nsec -= 1000000000;
      }
//      printf("%s:%d pthread_cond_timedwait    timeToWait.tv_sec = %lu  timeToWait.tv_nsec = %lu\n", __FILE__, __LINE__, timeToWait.tv_sec, timeToWait.tv_nsec );
//      m.lock();
      int rt = pthread_cond_timedwait(&this->_ConditionVariable,&m._Mutex,&timeToWait);
//      m.unlock();
      return rt==0;
    }
    bool signal() {
      return pthread_cond_signal(&this->_ConditionVariable)==0;
    }
    bool broadcast() {
      return pthread_cond_broadcast(&this->_ConditionVariable)==0;
    }
      
  };

#ifdef CLASP_THREADS
  template <typename T>
    struct RAIIReadLock {
      T& _Mutex;
    RAIIReadLock(T& p) : _Mutex(p) {
      _Mutex.shared_lock();
    }
      ~RAIIReadLock() {
        _Mutex.shared_unlock();
      }
    };

    template <typename T>
 struct RAIIReadWriteLock {
   T& _Mutex;
 RAIIReadWriteLock(T& p) : _Mutex(p) {
   _Mutex.lock();
 }
   ~RAIIReadWriteLock() {
     _Mutex.unlock();
   }
 };

#define WITH_READ_LOCK(mutex) mp::RAIIReadLock<decltype(mutex)> lock__(mutex)
#define WITH_READ_WRITE_LOCK(mutex) mp::RAIIReadWriteLock<decltype(mutex)> lock__(mutex)
#else
#define WITH_READ_LOCK(m)
#define WITH_READ_WRITE_LOCK(m)
#endif

  
  void* start_thread(void* claspProcess);

  inline void ClaspThreads_exit() {
//    printf("%s:%d Exiting pthread\n", __FILE__, __LINE__ );
//    pthread_exit(NULL);
//    printf("%s:%d Done pthread\n", __FILE__, __LINE__ );
  }

  class SharedMutex_O;
  typedef gctools::smart_ptr<SharedMutex_O> SharedMutex_sp;
};

#endif
