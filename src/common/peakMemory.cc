// joker-eph gave me this on #llvm
// It will report peak memory usage.
//
// Build with this:
// cc -std=c++11 -dynamiclib -o peakMemory.dylib peakMemory.cc 
//
// Use it with this:
// DYLD_INSERT_LIBRARIES=path/to/dylib <cmd>`
//

#include <atomic>
#include <stdio.h>
#include <stdlib.h>
#include <malloc/malloc.h>
// Taken from dyld-interposing.h

#define DYLD_INTERPOSE(_replacment,_replacee) \
__attribute__((used)) static struct{ const void* replacment; const void* replacee; } _interpose_##_replacee \
__attribute__ ((section ("__DATA,__interpose"))) = { (const void*)(unsigned long)&_replacment, (const void*)(unsigned long)&_replacee };


static std::atomic<uint64_t> NumAllocCalls;
static std::atomic<uint64_t> NumFreeCalls;
static std::atomic<uint64_t> PeakMemoryUsage;
static std::atomic<uint64_t> CurrentMemoryUsage;

template<typename T>
void update_maximum(std::atomic<T>& maximum_value, uint64_t value) noexcept
{
  T prev_value = maximum_value;
  while(prev_value < value &&
        !maximum_value.compare_exchange_weak(prev_value, value))
    ;
}

void *tracking_malloc(size_t size) {
  ++NumAllocCalls;
  void *allocatedMemory = malloc(size);
  CurrentMemoryUsage += malloc_size(allocatedMemory);
  update_maximum(PeakMemoryUsage, CurrentMemoryUsage);
  //printf("tracking_malloc peak: %zu\n", PeakMemoryUsage);
  return allocatedMemory;
}
DYLD_INTERPOSE(tracking_malloc, malloc)

void* tracking_calloc(size_t num, size_t size) {
  ++NumAllocCalls;
  void *allocatedMemory = calloc(num, size);
  CurrentMemoryUsage += malloc_size(allocatedMemory);
  update_maximum(PeakMemoryUsage, CurrentMemoryUsage);
  //printf("tracking_calloc peak: %zu\n", PeakMemoryUsage);
  return allocatedMemory;
}
DYLD_INTERPOSE(tracking_calloc, calloc)

void* tracking_realloc(void* ptr, size_t size) {
  ++NumAllocCalls;
  if (ptr) {
    CurrentMemoryUsage -= malloc_size(ptr);
  }
  void *allocatedMemory = realloc(ptr, size);
  CurrentMemoryUsage += malloc_size(allocatedMemory);
  update_maximum(PeakMemoryUsage, CurrentMemoryUsage);
  //printf("tracking_realloc peak: %zu\n", PeakMemoryUsage);
  return allocatedMemory;
}
DYLD_INTERPOSE(tracking_realloc, realloc)

void tracking_free(void* ptr) {
  ++NumFreeCalls;
  if (ptr) {
    CurrentMemoryUsage -= malloc_size(ptr);
    //printf("tracking_free peak: %zu\n", PeakMemoryUsage);
  }
  return free(ptr);
}
DYLD_INTERPOSE(tracking_free, free)

void __attribute__ ((constructor)) initializer(void) {
  NumAllocCalls = 0;
  NumFreeCalls = 0;
  PeakMemoryUsage = 0;
  CurrentMemoryUsage = 0;
}

void __attribute__ ((destructor)) finalizer(void) {
  const char *progname = getprogname();
  fprintf(stderr, "### Memory stats for '%s' ###\n", progname);
  fprintf(stderr, "Peak '%s': %llu\n", progname, (uint64_t)PeakMemoryUsage);
  fprintf(stderr, "CurrentMemoryUsage : %llu\n", (uint64_t)CurrentMemoryUsage);
  fprintf(stderr, "NumAllocCalls : %llu\n", (uint64_t)NumAllocCalls);
  fprintf(stderr, "NumFreeCalls  : %llu\n", (uint64_t)NumFreeCalls);
}
