#pragma once

// Allocation via MMTk.
//
// MMTk allocates a block starting at alloc_start.  Clasp's Header_s sits at
// alloc_start, so the MMTk ObjectReference (client pointer) is
// alloc_start + sizeof(Header_s) = alloc_start + OBJECT_REF_OFFSET.

#include <cstdlib>
#include <cstring>

namespace gctools {

inline void* mmtk_alloc_raw(size_t size, MMTkClaspAllocSemantics semantics) {
  return mmtk_clasp_alloc(my_thread_low_level->_mmtk_mutator, size, CLASP_ALIGNMENT, semantics);
}

inline void mmtk_post_alloc(void* alloc_start, size_t size, MMTkClaspAllocSemantics semantics,
                             size_t header_size = sizeof(Header_s)) {
  void* client = reinterpret_cast<char*>(alloc_start) + header_size;
  mmtk_clasp_post_alloc(my_thread_low_level->_mmtk_mutator, client, size, semantics);
}

// --- Cons allocation ---

template <typename Stage, typename Cons>
inline ConsHeader_s* do_cons_allocation(size_t size) {
  RAIIDisableInterrupts disable_interrupts;
  void* alloc_start;
  alloc_start = mmtk_alloc_raw(size, MMTK_CLASP_ALLOC_DEFAULT);
  mmtk_post_alloc(alloc_start, size, MMTK_CLASP_ALLOC_DEFAULT, sizeof(ConsHeader_s));
  ConsHeader_s* header = reinterpret_cast<ConsHeader_s*>(alloc_start);
  const ConsHeader_s::StampWtagMtag stamp(ConsHeader_s::cons_mtag);
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
  mmtk_post_alloc(alloc_start, true_size, MMTK_CLASP_ALLOC_DEFAULT);
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
  mmtk_post_alloc(alloc_start, true_size, MMTK_CLASP_ALLOC_DEFAULT);
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
  mmtk_post_alloc(alloc_start, true_size, MMTK_CLASP_ALLOC_NON_MOVING);
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

}; // namespace gctools
