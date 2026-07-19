#pragma once

// Allocation via MMTk.
//
// MMTk allocates a block starting at alloc_start.  Clasp's Header_s sits at
// alloc_start, so the MMTk ObjectReference (client pointer) is
// alloc_start + sizeof(Header_s) = alloc_start + OBJECT_REF_OFFSET.

#include <cstdlib>
#include <cstring>

namespace core {
struct ThreadLocalState;
bool thread_local_gcsafep(ThreadLocalState* athread);
};

namespace gctools {


inline void* mmtk_alloc_raw(size_t size, MMTkClaspAllocSemantics semantics) {
  // A thread must be GC-UNSAFE (Running) to allocate. If it's gc-safe here, some
  // caller wrapped this allocation in begin_gcsafe()/end_gcsafe() (or is
  // allocating inside another gc-safe region). That's the bug: a gc-safe thread
  // has told the collector "I'm stopped, collect now" — but it's about to run
  // MMTk's allocator, so a collection can start while it's inside the allocator.
  if (my_thread && thread_local_gcsafep(my_thread)) {
    fprintf(stderr,
            "%s:%d GC-SAFETY VIOLATION: thread %p entered the allocator while "
            "GC-safe. Allocation must run with the thread Running (GC-unsafe). "
            "Look for a begin_gcsafe()/end_gcsafe() wrapping this allocation. "
            "Aborting for a backtrace.\n",
            __FILE__, __LINE__, (void*)my_thread);
    fflush(stderr);
    abort();
  }

  // If we are supposed to yield to the GC then do so here
  gctools::gc_yield();

  // Do the allocation and if it is out of memory and needs to GC it will park me and wait for all
  // other threads to park and then do the GC and free up memory and return.
  return mmtk_clasp_alloc(my_thread_low_level->_mmtk_mutator, size, CLASP_ALIGNMENT, semantics);
}

// --- Cons allocation ---

template <typename Stage, typename Cons>
inline ConsHeader_s* do_cons_allocation(size_t size) {
  RAIIDisableInterrupts disable_interrupts;
  void* alloc_start;
  alloc_start = mmtk_alloc_raw(size, MMTK_CLASP_ALLOC_DEFAULT);
  ConsHeader_s* header = reinterpret_cast<ConsHeader_s*>(alloc_start);
  const ConsHeader_s::StampWtagMtag stamp(ConsHeader_s::BadgeStampWtagMtag::make<Cons>());
  new (header) ConsHeader_s(stamp);
  my_thread_low_level->_Allocations.registerAllocation(STAMPWTAG_CONS, size);
  return header;
}

// --- Atomic allocation (no pointer fields) ---

template <typename Stage = RuntimeStage>
inline Header_s* do_atomic_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  RAIIDisableInterrupts disable_interrupts;
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((rand() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
  void* alloc_start;
  alloc_start = mmtk_alloc_raw(true_size, MMTK_CLASP_ALLOC_DEFAULT);
  Header_s* header = reinterpret_cast<Header_s*>(alloc_start);
  my_thread_low_level->_Allocations.registerAllocation(the_header.unshifted_stamp(), true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, size, tail_size, true_size);
#else
  new (header) Header_s(the_header);
#endif
  return header;
}

// --- General allocation (contains pointers) ---

template <typename Stage = RuntimeStage>
inline Header_s* do_general_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  RAIIDisableInterrupts disable_interrupts;
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((rand() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
  void* alloc_start;
  alloc_start = mmtk_alloc_raw(true_size, MMTK_CLASP_ALLOC_DEFAULT);
  Header_s* header = reinterpret_cast<Header_s*>(alloc_start);
  my_thread_low_level->_Allocations.registerAllocation(the_header.unshifted_stamp(), true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, size, tail_size, true_size);
#else
  new (header) Header_s(the_header);
#endif
  return header;
}

// --- Uncollectable / non-moving allocation ---

inline Header_s* do_uncollectable_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((rand() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
  void* alloc_start = mmtk_alloc_raw(true_size, MMTK_CLASP_ALLOC_NON_MOVING);
  Header_s* header = reinterpret_cast<Header_s*>(alloc_start);
  my_thread_low_level->_Allocations.registerAllocation(the_header.unshifted_stamp(), true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, size, tail_size, true_size);
#else
  new (header) Header_s(the_header);
#endif
  return header;
}

// --- Zero-initialised allocation for the bytecode VM root vector ---
// This allocates a flat array of num void* slots (no Clasp header).

inline void* do_allocate_zero(size_t num) {
  size_t total = sizeof(void*) * num;
  void* buffer = std::malloc(total);
  std::memset(buffer, 0, total);
  return buffer;
}

// --- Free ---

inline void do_free(void* ptr) {
  // MMTk manages GC memory; use free only for do_allocate_zero buffers.
  std::free(ptr);
}

// --- Destructor finalizer registration ---

template <class OT>
inline void do_register_destructor_finalizer(void* baseptr) {
  (void)baseptr;
}


// Publish a fully-constructed object to MMTk: sets the valid-object bit and
// per-object metadata so the collector will scan it (and, under moving Immix,
// copy/forward it). MUST be called only AFTER the object is fully initialized.
inline void do_post_alloc(void* base, size_t size, bool non_moving,
                          size_t header_size = sizeof(Header_s)) {
  void* client = reinterpret_cast<char*>(base) + header_size;
  mmtk_clasp_post_alloc(my_thread_low_level->_mmtk_mutator, client, size,
                        non_moving ? MMTK_CLASP_ALLOC_NON_MOVING
                        : MMTK_CLASP_ALLOC_DEFAULT);
}


}; // namespace gctools
