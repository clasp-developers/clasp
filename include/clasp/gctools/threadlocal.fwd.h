#ifndef gctools_threadlocal_fwd_H
#define gctools_threadlocal_fwd_H

#include <signal.h>

namespace gctools {

  extern "C" void HitAllocationSizeThreshold();
  extern "C" void HitAllocationNumberThreshold();

  extern void monitorAllocation(stamp_t k, size_t sz);
  extern void count_allocation(const stamp_t k);

  
#ifdef DEBUG_MONITOR_ALLOCATIONS
  // This may be deprecated
  struct MonitorAllocations {
    bool on;
    bool stackDump;
    int counter;
    int start;
    int end;
    int backtraceDepth;
  MonitorAllocations() : on(false), stackDump(false), counter(0){};
  };
#endif
   

 struct GlobalAllocationProfiler {
   std::atomic<int64_t> _BytesAllocated;
   std::atomic<int64_t> _AllocationNumberCounter;
   std::atomic<int64_t> _AllocationSizeCounter;
   std::atomic<int64_t> _HitAllocationNumberCounter;
   std::atomic<int64_t> _HitAllocationSizeCounter;
   size_t               _AllocationNumberThreshold;
   size_t               _AllocationSizeThreshold;
#ifdef DEBUG_MONITOR_ALLOCATIONS
   MonitorAllocations _Monitor;
#endif
   
 GlobalAllocationProfiler() :
   _AllocationSizeThreshold(1024*1024)
   , _AllocationNumberThreshold(16386)
     , _HitAllocationNumberCounter(0)
     , _HitAllocationSizeCounter(0)
   {};
 GlobalAllocationProfiler(size_t size, size_t number) : _AllocationSizeThreshold(size), _AllocationNumberThreshold(number)
     , _HitAllocationNumberCounter(0)
     , _HitAllocationSizeCounter(0)
   {};
    
   inline void registerAllocation(stamp_t stamp, size_t size) {
     this->_BytesAllocated += size;
     this->_AllocationSizeCounter += size;
     this->_AllocationNumberCounter++;
#if defined(DEBUG_COUNT_ALLOCATIONS) && defined(DEBUG_SLOW)
    gctools::count_allocation(stamp);
#endif
#ifdef DEBUG_MEMORY_PROFILE
     if (this->_AllocationSizeCounter >= this->_AllocationSizeThreshold) {
       HitAllocationSizeThreshold();
       this->_AllocationSizeCounter -= this->_AllocationSizeThreshold;
     }
     if (this->_AllocationNumberCounter >= this->_AllocationNumberThreshold) {
       HitAllocationNumberThreshold();
       this->_AllocationNumberCounter = 0;
     }
#endif
#if defined(GC_MONITOR_ALLOCATIONS) && defined(DEBUG_SLOW)
    if ( this->_Monitor.on ) {
      monitorAllocation(stamp,sz);
    }
#endif
   };
 };



  struct ThreadLocalStateLowLevel {
    void*                  _StackTop;
    int                    _DisableInterrupts;
    GlobalAllocationProfiler _Allocations;
#ifdef DEBUG_COUNT_ALLOCATIONS
    std::vector<size_t>    _CountAllocations;
    bool                   _BacktraceAllocationsP;
    Fixnum                 _BacktraceStamp;
    int                    _BacktraceFd;
#endif
#if defined(DEBUG_RECURSIVE_ALLOCATIONS)
    int                    _RecursiveAllocationCounter;
#endif
    ThreadLocalStateLowLevel(void* stack_top);
    ~ThreadLocalStateLowLevel();
  };
};

extern THREAD_LOCAL gctools::ThreadLocalStateLowLevel *my_thread_low_level;

#if defined(DEBUG_RECURSIVE_ALLOCATIONS)
#endif

namespace gctools {
  void lisp_increment_recursive_allocation_counter(ThreadLocalStateLowLevel* thread);
  void lisp_decrement_recursive_allocation_counter(ThreadLocalStateLowLevel* thread);
};


namespace gctools {
  struct RAIIDisableInterrupts {
    ThreadLocalStateLowLevel* this_thread;
  RAIIDisableInterrupts(ThreadLocalStateLowLevel* t) : this_thread(t) {
    this->this_thread->_DisableInterrupts = true;
  }
    ~RAIIDisableInterrupts() {
      this->this_thread->_DisableInterrupts = false;
    }
  };
};

// Defined in threadlocal.fwd.h
// extern THREAD_LOCAL core::ThreadLocalStateLowLevel *my_thread_low_level;
#define RAII_DISABLE_INTERRUPTS() gctools::RAIIDisableInterrupts disable_interrupts__(my_thread_low_level)



namespace gctools {
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
struct RecursiveAllocationCounter {
  RecursiveAllocationCounter() {
    lisp_increment_recursive_allocation_counter(my_thread_low_level);
  };
  ~RecursiveAllocationCounter() {
    lisp_decrement_recursive_allocation_counter(my_thread_low_level);
  }
};
#endif
};


#ifdef DEBUG_RECURSIVE_ALLOCATIONS
#define RAII_DEBUG_RECURSIVE_ALLOCATIONS() ::gctools::RecursiveAllocationCounter _rac_;
#else
#define RAII_DEBUG_RECURSIVE_ALLOCATIONS()
#endif



#endif
