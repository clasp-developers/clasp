#pragma once

#if TAG_BITS == 3
#define ALIGNED_GC_MALLOC(sz) MAYBE_MONITOR_ALLOC(GC_MALLOC(sz), sz)
#define ALIGNED_GC_MALLOC_ATOMIC(sz) MAYBE_MONITOR_ALLOC(GC_MALLOC_ATOMIC(sz), sz)
#define ALIGNED_GC_MALLOC_UNCOLLECTABLE(sz) MAYBE_MONITOR_ALLOC(GC_MALLOC_UNCOLLECTABLE(sz), sz)
#define ALIGNED_GC_MALLOC_KIND(stmp, sz, knd, kndaddr) MAYBE_MONITOR_ALLOC(GC_malloc_kind_global(sz, knd), sz)
#define ALIGNED_GC_MALLOC_STRONG_WEAK_KIND(sz, knd) MAYBE_MONITOR_ALLOC(GC_malloc_kind_global(sz, knd), sz)
#define ALIGNED_GC_MALLOC_ATOMIC_KIND(stmp, sz, knd, kndaddr)                                                                      \
  MAYBE_MONITOR_ALLOC(                                                                                                             \
      (knd == GC_I_PTRFREE) ? GC_malloc_kind_global(sz, knd) : malloc_kind_error(GC_I_PTRFREE, knd, sz, stmp, kndaddr), sz)
#define ALIGNED_GC_MALLOC_UNCOLLECTABLE_KIND(stmp, sz, knd, kndaddr)                                                               \
  MAYBE_MONITOR_ALLOC(GC_generic_malloc_uncollectable(sz, knd), sz)
#else
#error "There is more work to do to support more than 3 tag bits"
#define ALIGNED_GC_MALLOC(sz) MAYBE_VERIFY_ALIGNMENT(GC_memalign(Alignment(), sz))
#define ALIGNED_GC_MALLOC_ATOMIC(sz) MAYBE_VERIFY_ALIGNMENT(GC_memalign(Alignment(), sz))
#define ALIGNED_GC_MALLOC_UNCOLLECTABLE(sz)                                                                                        \
  MAYBE_VERIFY_ALIGNMENT((void*)gctools::AlignUp((uintptr_t)GC_MALLOC_UNCOLLECTABLE(sz + Alignment())))
#endif

namespace gctools {
template <typename Stage, typename Cons, typename... ARGS> inline Cons* do_cons_allocation(size_t size, ARGS&&... args) {
  RAIIAllocationStage<Stage> stage(my_thread_low_level);
#ifdef USE_PRECISE_GC
  ConsHeader_s* header = reinterpret_cast<ConsHeader_s*>(
      ALIGNED_GC_MALLOC_KIND(STAMP_UNSHIFT_WTAG(STAMPWTAG_CONS), size, global_cons_kind, &global_cons_kind)); // wasMTAG
#ifdef DEBUG_BOEHMPRECISE_ALLOC
  printf("%s:%d:%s cons = %p\n", __FILE__, __LINE__, __FUNCTION__, cons);
#endif
#else
  ConsHeader_s* header = reinterpret_cast<ConsHeader_s*>(ALIGNED_GC_MALLOC(size));
#endif
  Cons* cons = (Cons*)HeaderPtrToConsPtr(header);
  new (header) ConsHeader_s(cons);
  new (cons) Cons(std::forward<ARGS>(args)...);
  return cons;
}

template <typename Stage = RuntimeStage>
inline Header_s* do_atomic_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  RAIIAllocationStage<Stage> stage(my_thread_low_level);
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((my_thread_random() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
#ifdef USE_PRECISE_GC
  uintptr_t stamp = the_header.stamp();
  Header_s* header = reinterpret_cast<Header_s*>(ALIGNED_GC_MALLOC_ATOMIC_KIND(
      stamp, true_size, global_stamp_layout[stamp].boehm._kind, &global_stamp_layout[stamp].boehm._kind));
#else
  Header_s* header = reinterpret_cast<Header_s*>(ALIGNED_GC_MALLOC_ATOMIC(true_size));
#endif
  stage.registerAllocation(the_header.unshifted_stamp(), true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, size, tail_size, true_size);
#else
  new (header) Header_s(the_header);
#endif
  return header;
};

inline Header_s* do_weak_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  size_t true_size = size;
#ifdef USE_PRECISE_GC
  Header_s* header = reinterpret_cast<Header_s*>(ALIGNED_GC_MALLOC_ATOMIC(true_size));
//   Header_s* header = reinterpret_cast<Header_s*>(ALIGNED_GC_MALLOC_STRONG_WEAK_KIND_ATOMIC(true_size,global_strong_weak_kind));
#else
  Header_s* header = reinterpret_cast<Header_s*>(ALIGNED_GC_MALLOC_ATOMIC(true_size));
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

template <typename Stage = RuntimeStage>
inline Header_s* do_general_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  RAIIAllocationStage<Stage> stage(my_thread_low_level);
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((my_thread_random() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
#ifdef USE_PRECISE_GC
  auto stamp = the_header.stamp();
  auto& kind = global_stamp_layout[stamp].boehm._kind;
  GCTOOLS_ASSERT(kind != KIND_UNDEFINED);
  Header_s* header = reinterpret_cast<Header_s*>(ALIGNED_GC_MALLOC_KIND(stamp, true_size, kind, &kind));
#ifdef DEBUG_BOEHMPRECISE_ALLOC
  printf("%s:%d:%s header = %p\n", __FILE__, __LINE__, __FUNCTION__, header);
#endif
#else
  Header_s* header = reinterpret_cast<Header_s*>(ALIGNED_GC_MALLOC(true_size));
#endif
  stage.registerAllocation(the_header.unshifted_stamp(), true_size);
#ifdef DEBUG_GUARD
  memset(header, 0x00, true_size);
  new (header) Header_s(the_header, size, tail_size, true_size);
#else
  new (header) Header_s(the_header);
#endif
  return header;
};

inline Header_s* do_uncollectable_allocation(const Header_s::StampWtagMtag& the_header, size_t size) {
  size_t true_size = size;
#ifdef DEBUG_GUARD
  size_t tail_size = ((my_thread_random() % 8) + 1) * Alignment();
  true_size += tail_size;
#endif
#ifdef USE_PRECISE_GC
  Header_s* header = reinterpret_cast<Header_s*>(
      ALIGNED_GC_MALLOC_UNCOLLECTABLE_KIND(the_header.stamp(), true_size, global_stamp_layout[the_header.stamp()].boehm._kind,
                                           &global_stamp_layout[the_header.stamp()].boehm._kind));
#ifdef DEBUG_BOEHMPRECISE_ALLOC
  printf("%s:%d:%s header = %p\n", __FILE__, __LINE__, __FUNCTION__, header);
#endif
#else
  Header_s* header = reinterpret_cast<Header_s*>(ALIGNED_GC_MALLOC_UNCOLLECTABLE(true_size));
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
