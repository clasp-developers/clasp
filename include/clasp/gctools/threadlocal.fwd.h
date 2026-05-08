#pragma once

#include <signal.h>
#include <chrono>
#ifdef USE_MMTK
#include <clasp/gctools/mmtk_clasp.h>
#endif

namespace gctools {

struct AllocationProfiler {
  // These counters live in the THREAD_LOCAL ThreadLocalStateLowLevel (as the
  // member _Allocations) and are only ever accessed via
  // my_thread_low_level->_Allocations, i.e. by the owning thread alone (see
  // gcalloc_boehm.h, gcFunctions.cc, startRunStop.cc, memoryManagement.cc).
  int64_t _BytesAllocated = 0;
  size_t _AllocationSizeThreshold;
  size_t _AllocationNumberThreshold;
  int64_t _AllocationSizeCounter = 0;
  int64_t _AllocationNumberCounter = 0;

  AllocationProfiler()
    : _AllocationSizeThreshold(1024 * 1024), _AllocationNumberThreshold(16386) {};
  AllocationProfiler(size_t size, size_t number)
    : _AllocationSizeThreshold(size), _AllocationNumberThreshold(number) {};

  inline void registerAllocation(stamp_t stamp, size_t size) {
    this->_BytesAllocated += size;
    this->_AllocationSizeCounter += size;
    this->_AllocationNumberCounter++;
  };
  inline void registerWeakAllocation(uintptr_t stamp, size_t size) {
    this->_BytesAllocated += size;
    this->_AllocationSizeCounter += size;
    this->_AllocationNumberCounter++;
  };
};

struct ThreadLocalStateLowLevel {
  void* _ControlStackTop;
  void* _ControlStackBottom;
  // Saved stack pointer when this thread last entered a GC-safe state.
  // Valid only while the thread is GC-safe (gclessp() == true).
  void* _ControlStackPointer = nullptr;
  bool _DisableInterrupts;
  AllocationProfiler _Allocations;
#ifdef USE_MMTK
  MMTkClaspMutator _mmtk_mutator;
#endif
  // Time unwinds
  std::chrono::time_point<std::chrono::high_resolution_clock> _start_unwind;
  std::chrono::duration<size_t, std::nano> _unwind_time;
  ThreadLocalStateLowLevel();
  ~ThreadLocalStateLowLevel();
};
}; // namespace gctools

extern THREAD_LOCAL gctools::ThreadLocalStateLowLevel* my_thread_low_level;

namespace gctools {
struct RAIIDisableInterrupts {
  ThreadLocalStateLowLevel* this_thread;
  RAIIDisableInterrupts(ThreadLocalStateLowLevel* t = my_thread_low_level) : this_thread(t) { this->this_thread->_DisableInterrupts = true; }
  ~RAIIDisableInterrupts() { this->this_thread->_DisableInterrupts = false; }

};

struct RuntimeStage {};
struct SnapshotLoadStage {};

}; // namespace gctools
