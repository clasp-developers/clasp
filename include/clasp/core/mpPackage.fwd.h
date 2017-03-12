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

PACKAGE_USE("COMMON-LISP");
NAMESPACE_PACKAGE_ASSOCIATION(mp, MpPkg, "MP")

namespace mp {

  class Process_O;
  typedef gctools::smart_ptr<Process_O> Process_sp;
};



namespace mp {

  struct GlobalMutex {
    pthread_mutex_t _Mutex;
    bool _Recursive;
    GlobalMutex(bool recursive) : _Recursive(recursive) {
      if (!recursive) {
        this->_Mutex = PTHREAD_MUTEX_INITIALIZER;
      } else {
#if defined(_TARGET_OS_LINUX)
      this->_Mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#elif defined(_TARGET_OS_DARWIN)
      this->_Mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER;
#else
      #error "You need to initialize this->_Mutex"
#endif
      }
    };
    bool lock(bool waitp=true) {
       if (waitp) return pthread_mutex_lock(&this->_Mutex)==0;
       return pthread_mutex_trylock(&this->_Mutex)==0;
    }
    void unlock() { pthread_mutex_unlock(&this->_Mutex); };
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
  
  struct Mutex {
    pthread_mutex_t _Mutex;
    bool _Recursive;
  Mutex() : _Recursive(false) {
    pthread_mutex_init(&this->_Mutex,NULL);
  }
  Mutex(bool recursive) : _Recursive(recursive) {
      if (!recursive) {
//      printf("%s:%d Creating Mutex@%p\n", __FILE__, __LINE__, (void*)&this->_Mutex);
        pthread_mutex_init(&this->_Mutex,NULL);
      } else {
        pthread_mutexattr_t Attr;
        pthread_mutexattr_init(&Attr);
        pthread_mutexattr_settype(&Attr, PTHREAD_MUTEX_RECURSIVE);
        pthread_mutex_init(&this->_Mutex,&Attr);
        pthread_mutexattr_destroy(&Attr);
      }
    };
    bool lock(bool waitp=true) {
//      printf("%s:%d locking Mutex@%p\n", __FILE__, __LINE__, (void*)&this->_Mutex);
      if (waitp) {
        return (pthread_mutex_lock(&this->_Mutex)==0);
      }
      return pthread_mutex_trylock(&this->_Mutex)==0;
    };
    void unlock() {
//      printf("%s:%d unlocking Mutex@%p\n", __FILE__, __LINE__, (void*)&this->_Mutex);
      pthread_mutex_unlock(&this->_Mutex);
    };
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
  SharedMutex() : _r(false), _g(false) {};
    // shared access
    void shared_lock() {
      this->_r.lock(false);
      ++this->_b;
      if (this->_b==1) this->_g.lock(false);
      this->_r.unlock();
    }
    void shared_unlock() {
      this->_r.lock(false);
      --this->_b;
      if (this->_b==0) this->_g.unlock();
      this->_r.unlock();
    }
    // exclusive access
    void lock() {
      this->_g.lock(false);
    }
    void unlock() {
      this->_g.unlock();
    }
  };

  struct SharedRecursiveMutex {
    mp::Mutex _r;
    mp::RecursiveMutex _g;
    size_t _b;
  SharedRecursiveMutex() : _r(false){};
    // shared access
    void shared_lock() {
      this->_r.lock();
      ++this->_b;
      if (this->_b==1) this->_g.lock();
      this->_r.unlock();
    }
    void shared_unlock() {
      this->_r.lock();
      --this->_b;
      if (this->_b==0) this->_g.unlock();
      this->_r.unlock();
    }
    // exclusive access
    void lock() {
      this->_g.lock();
    }
    void unlock() {
      this->_g.unlock();
    }
  };

    template <typename T>
 struct RAIISharedReadLock {
   T& _SharedMutex;
 RAIISharedReadLock(T& p) : _SharedMutex(p) {
   _SharedMutex.shared_lock();
 }
   ~RAIISharedReadLock() {
     _SharedMutex.shared_unlock();
   }
 };

    template <typename T>
 struct RAIISharedReadWriteLock {
   T& _SharedMutex;
 RAIISharedReadWriteLock(T& p) : _SharedMutex(p) {
   _SharedMutex.lock();
 }
   ~RAIISharedReadWriteLock() {
     _SharedMutex.unlock();
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
  };

#ifdef CLASP_THREADS
#define WITH_READ_LOCK(mutex) mp::RAIISharedReadLock<decltype(mutex)> lock__(mutex)
#define WITH_READ_WRITE_LOCK(mutex) mp::RAIISharedReadWriteLock<decltype(mutex)> lock__(mutex)
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
};

#endif
