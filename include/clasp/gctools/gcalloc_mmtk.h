#pragma once

// Allocation via MMTk.
//
// MMTk allocates a block starting at alloc_start.  Clasp's Header_s sits at
// alloc_start, so the MMTk ObjectReference (client pointer) is
// alloc_start + sizeof(Header_s) = alloc_start + OBJECT_REF_OFFSET.

#include <cstdlib>
#include <cstring>

namespace gctools {

inline void* mmtk_alloc_raw(ThreadLocalStateLowLevel* thread_ll,
                            size_t size, MMTkClaspAllocSemantics semantics) {
  return mmtk_clasp_alloc(thread_ll->_mmtk_mutator,
                          size, CLASP_ALIGNMENT, semantics);
}

inline void mmtk_post_alloc(ThreadLocalStateLowLevel* thread_ll,
                            void* alloc_start, size_t size, MMTkClaspAllocSemantics semantics,
                             size_t header_size = sizeof(Header_s)) {
  void* client = reinterpret_cast<char*>(alloc_start) + header_size;
  mmtk_clasp_post_alloc(thread_ll->_mmtk_mutator, client, size, semantics);
}

// --- Cons allocation ---

template <typename Stage, typename Cons, size_t Size>
inline ConsHeader_s* do_cons_allocation() {
  // TLS access is expensive so we want to do as little of it as we can.
  ThreadLocalStateLowLevel* thread_ll = my_thread_low_level;
  RAIIDisableInterrupts disable_interrupts(thread_ll);
  void* alloc_start;
  alloc_start = mmtk_alloc_raw(thread_ll, Size, MMTK_CLASP_ALLOC_DEFAULT);
  ConsHeader_s* header = reinterpret_cast<ConsHeader_s*>(alloc_start);
  const ConsHeader_s::StampWtagMtag stamp(ConsHeader_s::BadgeStampWtagMtag::make<Cons>());
  new (header) ConsHeader_s(stamp);
  thread_ll->_Allocations.registerAllocation(STAMPWTAG_CONS, Size);
  mmtk_post_alloc(thread_ll, alloc_start, Size, MMTK_CLASP_ALLOC_DEFAULT, sizeof(ConsHeader_s));
  return header;
}

static inline Header_s* do_generic_slow_allocation(const Header_s::StampWtagMtag& the_header, size_t size, MMTkClaspAllocSemantics semantics) {
  ThreadLocalStateLowLevel* thread_ll = my_thread_low_level;
  RAIIDisableInterrupts disable_interrupts(thread_ll);
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((rand() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
  void* alloc_start;
  alloc_start = mmtk_alloc_raw(thread_ll, true_size, semantics);
  Header_s* header = reinterpret_cast<Header_s*>(alloc_start);
  thread_ll->_Allocations.registerAllocation(the_header.unshifted_stamp(), true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, size, tail_size, true_size);
#else
  new (header) Header_s(the_header);
#endif
  mmtk_post_alloc(thread_ll, alloc_start, true_size, semantics);
  return header;
}

template <size_t Size>
static inline Header_s* do_generic_slow_allocation(const Header_s::StampWtagMtag& the_header, MMTkClaspAllocSemantics semantics) {
#ifdef DEBUG_GUARD
  // size is variable, give up
  return do_generic_slow_allocation(the_header, Size, semantics);
#else
  ThreadLocalStateLowLevel* thread_ll = my_thread_low_level;
  RAIIDisableInterrupts disable_interrupts(thread_ll);
  void* alloc_start;
  alloc_start = mmtk_alloc_raw(thread_ll, Size, semantics);
  Header_s* header = reinterpret_cast<Header_s*>(alloc_start);
  thread_ll->_Allocations.registerAllocation(the_header.unshifted_stamp(), Size);
  new (header) Header_s(the_header);
  mmtk_post_alloc(thread_ll, alloc_start, Size, semantics);
  return header;
#endif
}

// --- Atomic allocation (no pointer fields) ---

template <typename Stage = RuntimeStage>
inline Header_s* do_atomic_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  return do_generic_slow_allocation(the_header, size, MMTK_CLASP_ALLOC_DEFAULT);
}
template <typename Stage = RuntimeStage, size_t Size>
inline Header_s* do_atomic_allocation(const Header_s::StampWtagMtag& the_header) {
  return do_generic_slow_allocation<Size>(the_header, MMTK_CLASP_ALLOC_DEFAULT);
}

// --- General allocation (contains pointers) ---

template <typename Stage = RuntimeStage>
inline Header_s* do_general_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  return do_generic_slow_allocation(the_header, size, MMTK_CLASP_ALLOC_DEFAULT);
}
template <typename Stage = RuntimeStage, size_t Size>
inline Header_s* do_general_allocation(const Header_s::StampWtagMtag& the_header) {
  return do_generic_slow_allocation<Size>(the_header, MMTK_CLASP_ALLOC_DEFAULT);
}

// --- Non-moving allocation ---
template <typename Stage = RuntimeStage>
inline Header_s* do_immobile_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  return do_generic_slow_allocation(the_header, size, MMTK_CLASP_ALLOC_NON_MOVING);
}
template <typename Stage = RuntimeStage, size_t Size>
inline Header_s* do_immobile_allocation(const Header_s::StampWtagMtag& the_header) {
  return do_generic_slow_allocation<Size>(the_header, MMTK_CLASP_ALLOC_NON_MOVING);
}

// --- Uncollectable & non-moving allocation ---

inline Header_s* do_uncollectable_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  return do_generic_slow_allocation(the_header, size, MMTK_CLASP_ALLOC_IMMORTAL);
}
template <size_t Size>
inline Header_s* do_uncollectable_allocation(const Header_s::StampWtagMtag& the_header) {
  return do_generic_slow_allocation<Size>(the_header, MMTK_CLASP_ALLOC_IMMORTAL);
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
