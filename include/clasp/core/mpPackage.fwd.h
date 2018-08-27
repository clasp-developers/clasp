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
      
  struct GlobalMutex {
    pthread_mutex_t _Mutex;
    bool _Recursive;


    GlobalMutex(bool recursive) : _Recursive(recursive) {
      pthread_mutexattr_t Attr;
      pthread_mutexattr_init(&Attr);
      if (recursive) {
        pthread_mutexattr_settype(&Attr, PTHREAD_MUTEX_RECURSIVE);
      }
      pthread_mutex_init(&this->_Mutex, &Attr);
    };
    bool lock(bool waitp=true) {
//      printf("%s:%d  locking mutex %p\n", __FILE__, __LINE__, &this->_Mutex); fflush(stdout);
       if (waitp) return pthread_mutex_lock(&this->_Mutex)==0;
       return pthread_mutex_trylock(&this->_Mutex)==0;
    }
    void unlock() {
//      printf("%s:%d  unlocking mutex %p\n", __FILE__, __LINE__, &this->_Mutex); fflush(stdout);
      pthread_mutex_unlock(&this->_Mutex);
    };
    bool recursive_lock_p() {
      return this->_Recursive;
    }
    ~GlobalMutex() {
    };
  };

  struct GlobalRecursiveMutex : public GlobalMutex {
    pthread_mutex_t _Mutex;
  GlobalRecursiveMutex() : GlobalMutex(true) {};
    ~GlobalRecursiveMutex() {};
  };

  struct Mutex;
  void debug_mutex_lock(Mutex* m);
  void debug_mutex_unlock(Mutex* m);

    struct Mutex {
      pthread_mutex_t _Mutex;
      gctools::Fixnum _Counter;
      bool _Recursive;
    Mutex() : _Counter(0), _Recursive(false) {
      pthread_mutex_init(&this->_Mutex,NULL);
    }
    Mutex(bool recursive) : _Counter(0), _Recursive(recursive) {
    
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
  RecursiveMutex() : Mutex(true) {};
  };

  
  struct SharedMutex {
    mp::Mutex _r;
    mp::Mutex _g;
    size_t _b;
  SharedMutex() : _r(false), _g(false), _b(0) {};
    // shared access
    void shared_lock() {
      this->_r.lock(true);
      ++this->_b;
      if (this->_b==1) this->_g.lock(true);
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

  class UpgradableSharedMutex {
  public:
  UpgradableSharedMutex( uint maxReaders = 64 ) : 
    mReadsBlocked( false ), mMaxReaders( maxReaders ), mReaders( 0 ) {}
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
 
    void writeLock(bool upgrade = false) {
      mWriteMutex.lock(); 
      waitReaders( upgrade ? 1 : 0 );
    }
    bool writeTryLock(bool upgrade = false) {
      if ( !mWriteMutex.lock())
        return false;
      waitReaders( upgrade ? 1 : 0 );
      return true;
    }
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
    bool    mReadsBlocked;
    uint     mMaxReaders;
    uint     mReaders;
    Mutex mReadMutex;
    Mutex mWriteMutex;
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
