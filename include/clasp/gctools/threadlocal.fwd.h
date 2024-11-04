#pragma once

#include <signal.h>
#include <chrono>

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
  size_t _AllocationSizeThreshold;
  size_t _AllocationNumberThreshold;
  std::atomic<int64_t> _AllocationSizeCounter;
  std::atomic<int64_t> _AllocationNumberCounter;
  std::atomic<int64_t> _HitAllocationNumberCounter;
  std::atomic<int64_t> _HitAllocationSizeCounter;
#ifdef DEBUG_MONITOR_ALLOCATIONS
  MonitorAllocations _Monitor;
#endif

  GlobalAllocationProfiler()
      : _AllocationSizeThreshold(1024 * 1024), _AllocationNumberThreshold(16386), _HitAllocationNumberCounter(0),
        _HitAllocationSizeCounter(0){};
  GlobalAllocationProfiler(size_t size, size_t number)
      : _AllocationSizeThreshold(size), _AllocationNumberThreshold(number), _HitAllocationNumberCounter(0),
        _HitAllocationSizeCounter(0){};

  inline void registerAllocation(stamp_t stamp, size_t size) {
    this->_BytesAllocated += size;
    this->_AllocationSizeCounter += size;
    this->_AllocationNumberCounter++;
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
    if (this->_Monitor.on) {
      monitorAllocation(stamp, sz);
    }
#endif
  };
  inline void registerWeakAllocation(uintptr_t stamp, size_t size) {
    this->_BytesAllocated += size;
    this->_AllocationSizeCounter += size;
    this->_AllocationNumberCounter++;
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
    if (this->_Monitor.on) {
      monitorWeakAllocation(stamp, sz);
    }
#endif
  };
};

struct ThreadLocalStateLowLevel {
  void* _StackTop;
  bool _DisableInterrupts;
  GlobalAllocationProfiler _Allocations;
  // Time unwinds
  std::chrono::time_point<std::chrono::high_resolution_clock> _start_unwind;
  std::chrono::duration<size_t, std::nano> _unwind_time;
#if defined(DEBUG_RECURSIVE_ALLOCATIONS)
  int _RecursiveAllocationCounter;
  size_t _RecursiveAllocationHeaderValue;
#endif
  ThreadLocalStateLowLevel(void* stack_top);
  ~ThreadLocalStateLowLevel();
};
}; // namespace gctools

extern THREAD_LOCAL gctools::ThreadLocalStateLowLevel* my_thread_low_level;

#if defined(DEBUG_RECURSIVE_ALLOCATIONS)
#endif

namespace gctools {
void lisp_increment_recursive_allocation_counter(ThreadLocalStateLowLevel* thread, size_t header_value);
void lisp_decrement_recursive_allocation_counter(ThreadLocalStateLowLevel* thread);
}; // namespace gctools

namespace gctools {
struct RAIIDisableInterrupts {
  ThreadLocalStateLowLevel* this_thread;
  RAIIDisableInterrupts(ThreadLocalStateLowLevel* t) : this_thread(t) { this->this_thread->_DisableInterrupts = true; }
  ~RAIIDisableInterrupts() { this->this_thread->_DisableInterrupts = false; }
};
}; // namespace gctools

// Defined in threadlocal.fwd.h
// extern THREAD_LOCAL core::ThreadLocalStateLowLevel *my_thread_low_level;

namespace gctools {

//
// We need to allocate Code_O objects in snapshot_load
//  from threads that are not under our control.
//  The threads don't have thread local state setup so we
//  don't want to disable interrupts or register allocations.
//
struct RuntimeStage {};
struct SnapshotLoadStage {};

template <typename Stage> struct RAIIAllocationStage {
private:
  RAIIAllocationStage(ThreadLocalStateLowLevel* t) {
    printf("%s:%d:%s This should never be called\n", __FILE__, __LINE__, __FUNCTION__);
  };
};

template <> struct RAIIAllocationStage<RuntimeStage> {
  ThreadLocalStateLowLevel* _threadLocalStateLowLevel;
  RAIIDisableInterrupts _disableInterrupts;

  RAIIAllocationStage(ThreadLocalStateLowLevel* t) : _threadLocalStateLowLevel(t), _disableInterrupts(t){};
  void registerAllocation(uintptr_t ustamp, size_t size) {
    this->_threadLocalStateLowLevel->_Allocations.registerAllocation(ustamp, size);
  }
};

template <> struct RAIIAllocationStage<SnapshotLoadStage> {

  RAIIAllocationStage(ThreadLocalStateLowLevel* t){};
  void registerAllocation(uintptr_t ustamp, size_t size){};
};

}; // namespace gctools
#define RAII_DISABLE_INTERRUPTS() gctools::RAIIDisableInterrupts disable_interrupts__(my_thread_low_level)

namespace gctools {
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
struct RecursiveAllocationCounter {
  RecursiveAllocationCounter(size_t header_value) {
    lisp_increment_recursive_allocation_counter(my_thread_low_level, header_value);
  };
  ~RecursiveAllocationCounter() { lisp_decrement_recursive_allocation_counter(my_thread_low_level); }
};
#endif
}; // namespace gctools

#ifdef DEBUG_RECURSIVE_ALLOCATIONS
#define RAII_DEBUG_RECURSIVE_ALLOCATIONS(header_value) ::gctools::RecursiveAllocationCounter _rac_(header_value);
#else
#define RAII_DEBUG_RECURSIVE_ALLOCATIONS(header_value)
#endif
