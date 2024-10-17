#pragma once

extern THREAD_LOCAL MMTk_Mutator my_mutator;

namespace gctools {

#define DOALLOC(true_size) reinterpret_cast<Header_s*>(alloc(my_mutator, true_size, 8, 0, 0))
#define DO_CONS_ALLOC(true_size) DOALLOC(true_size)
#define DO_ATOMIC_ALLOC(true_size) DOALLOC(true_size)
#define DO_WEAK_ALLOC(true_size) DOALLOC(true_size)
#define DO_GENERAL_ALLOC(true_size) DOALLOC(true_size)
#define DO_UNCOLLECTABLE_ALLOC(true_size) DOALLOC(true_size)

template <typename Cons, typename... ARGS> inline Cons* do_cons_allocation(size_t true_size, ARGS&&... args) {
  RAII_DISABLE_INTERRUPTS();
#ifdef USE_PRECISE_GC
  Header_s* header = DO_CONS_ALLOC(true_size);
#ifdef DEBUG_BOEHMPRECISE_ALLOC
  printf("%s:%d:%s cons = %p\n", __FILE__, __LINE__, __FUNCTION__, cons);
#endif
#else
  Header_s* header = DO_CONS_ALLOC(true_size);
#endif
  Cons* cons = (Cons*)HeaderPtrToConsPtr(header);
  new (header) Header_s::StampWtagMtag(cons);
  new (cons) Cons(std::forward<ARGS>(args)...);
  return cons;
}

inline Header_s* do_atomic_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  RAII_DISABLE_INTERRUPTS();
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((rand() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
#ifdef USE_PRECISE_GC
  uintptr_t stamp = the_header.stamp();
  Header_s* header = DO_ATOMIC_ALLOC(true_size);
#else
  Header_s* header = DO_ATOMIC_ALLOC(true_size);
#endif
  my_thread_low_level->_Allocations.registerAllocation(the_header.unshifted_stamp(), true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, size, tail_size, true_size);
#else
  new (header) Header_s(the_header);
#endif
  return header;
};

inline Header_s* do_weak_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  RAII_DISABLE_INTERRUPTS();
  size_t true_size = size;
#ifdef USE_PRECISE_GC
  Header_s* header = DO_WEAK_ALLOC(true_size);
#else
  Header_s* header = DO_WEAK_ALLOC(true_size);
#endif
  my_thread_low_level->_Allocations.registerWeakAllocation(the_header._value, true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, 0, 0, true_size);
#else
  new (header) Header_s(the_header);
#endif
  return header;
};

inline Header_s* do_general_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  RAII_DISABLE_INTERRUPTS();
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((rand() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
#ifdef USE_PRECISE_GC
  Header_s* header = DO_GENERAL_ALLOC(true_size);
#ifdef DEBUG_BOEHMPRECISE_ALLOC
  printf("%s:%d:%s header = %p\n", __FILE__, __LINE__, __FUNCTION__, header);
#endif
#else
  Header_s* header = DO_GENERAL_ALLOC(true_size);
#endif
  my_thread_low_level->_Allocations.registerAllocation(the_header.unshifted_stamp(), true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, size, tail_size, true_size);
#else
  new (header) Header_s(the_header);
#endif
  return header;
};

inline Header_s* do_uncollectable_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  RAII_DISABLE_INTERRUPTS();
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((rand() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
#ifdef USE_PRECISE_GC
  Header_s* header = DO_UNCOLLECTABLE_ALLOC(true_size);
#ifdef DEBUG_BOEHMPRECISE_ALLOC
  printf("%s:%d:%s header = %p\n", __FILE__, __LINE__, __FUNCTION__, header);
#endif
#else
  Header_s* header = DO_UNCOLLECTABLE_ALLOC(true_size);
#endif
  my_thread_low_level->_Allocations.registerAllocation(the_header.unshifted_stamp(), true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, size, tail_size, true_size);
#else
  new (header) Header_s(the_header);
#endif
  return header;
};

}; // namespace gctools
