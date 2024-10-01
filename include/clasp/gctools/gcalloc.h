#pragma once

/*
    File: gcalloc.h
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

// #define BOEHM_ONE_BIG_STACK 1
// #define DEBUG_BOEHM_STACK 1
//  #define DEBUG_BOEHMPRECISE_ALLOC 1
// #define DEBUG_CONS_ALLOC 1

#include <limits>
#include <clasp/gctools/interrupt.h>
#include <clasp/gctools/threadlocal.fwd.h>
#include <clasp/gctools/snapshotSaveLoad.fwd.h>

#define STACK_ALIGNMENT alignof(char*)
#define STACK_ALIGN_UP(size) (((size) + STACK_ALIGNMENT - 1) & ~(STACK_ALIGNMENT - 1))

namespace gctools {
extern uintptr_t global_strong_weak_kind;
};

namespace gctools {
template <class OT, bool Needed = true> struct GCObjectInitializer {};

template <class OT> struct GCObjectInitializer<OT, true> {
  typedef smart_ptr<OT> smart_pointer_type;
  static void initializeIfNeeded(smart_pointer_type sp) {
    if (sp.generalp()) {
      sp.unsafe_general()->initialize();
    }
  };
};

template <class OT> struct GCObjectInitializer<OT, false> {
  typedef smart_ptr<OT> smart_pointer_type;
  static void initializeIfNeeded(smart_pointer_type sp){
      // initialize not needed
  };
};

#ifdef TAGGED_POINTER
template <class OT> struct GCObjectInitializer<tagged_pointer<OT>, true> {
  typedef tagged_pointer<OT> functor_pointer_type;
  static void initializeIfNeeded(functor_pointer_type sp) {
    throw_hard_error("Figure out why this is being invoked, you should never need to initialize a functor!");
  };
};
template <class OT> struct GCObjectInitializer<tagged_pointer<OT>, false> {
  typedef tagged_pointer<OT> functor_pointer_type;
  static void initializeIfNeeded(functor_pointer_type sp){
      // initialize not needed
  };
};
#endif // end TAGGED_POINTER
} // namespace gctools

#if defined(USE_BOEHM) || defined(USE_MPS) || defined(USE_MMTK)

#if defined(USE_BOEHM)
namespace gctools {
template <class T> class root_allocator {};
}; // namespace gctools
#endif

inline void* verify_alignment(void* ptr) {
  if ((((uintptr_t)ptr) & gctools::ptr_mask) != (uintptr_t)ptr) {
    printf("%s:%d The pointer at %p is not aligned properly\n", __FILE__, __LINE__, ptr);
    abort();
  }
  return ptr;
}

inline void* monitor_alloc(void* ptr, size_t sz) {
  // printf("%s:%d Allocate pointer size: %lu at %p\n", __FILE__, __LINE__, sz, ptr);
  return ptr;
}

namespace gctools {
extern void* malloc_kind_error(uintptr_t expected_kind, uintptr_t kind, uintptr_t size, uintptr_t stmp, void* addr);
};

#ifdef DEBUG_ALLOC_ALIGNMENT
#define MAYBE_VERIFY_ALIGNMENT(ptr) verify_alignment(ptr)
#else
#define MAYBE_VERIFY_ALIGNMENT(ptr) (void*)ptr
#endif

#if 1
#define MAYBE_MONITOR_ALLOC(_alloc_, _sz_) monitor_alloc(_alloc_, _sz_)
#else
#define MAYBE_MONITOR_ALLOC(_alloc_, _sz_) (_alloc_)
#endif

namespace gctools {

class DontRegister {};
class DoRegister {};

template <int F> struct Foo {};

template <typename Stage, typename Cons, typename Register = DontRegister> struct ConsSizeCalculator {
  static inline size_t value() {
    //    printf("%s:%d:%s AlignUp(sizeof(Cons) %lu +SizeofConsHeader() %lu) = %lu  must be 24\n", __FILE__, __LINE__, __FUNCTION__,
    //    sizeof(Cons),SizeofConsHeader(), AlignUp(sizeof(Cons)+SizeofConsHeader()) );
    static_assert(sizeof(Cons) == 16);
    static_assert(AlignUp(sizeof(Cons)) == 16);
    static_assert(AlignUp(SizeofConsHeader()) == 8);
    static_assert(AlignUp(sizeof(Cons) + SizeofConsHeader()) == 24);
    size_t size = AlignUp(sizeof(Cons) + SizeofConsHeader());
    return size;
  }
};

template <typename Cons> struct ConsSizeCalculator<gctools::RuntimeStage, Cons, DoRegister> {
  static inline size_t value() {
    size_t size = ConsSizeCalculator<RuntimeStage, Cons, DontRegister>::value();
    my_thread_low_level->_Allocations.registerAllocation(STAMPWTAG_CONS, size);
    return size;
  }
};
}; // namespace gctools

uint32_t my_thread_random();

#if defined(USE_BOEHM)
#include <clasp/gctools/gcalloc_boehm.h>
#elif defined(USE_MPS)
#include <clasp/gctools/gcalloc_mps.h>
#elif defined(USE_MMTK)
#include <clasp/gctools/gcalloc_mmtk.h>
#else
#error "Unsupported GC"
#endif

namespace gctools {

/*! Allocate regular C++ classes that are considered roots */
template <class T> struct RootClassAllocator {
  template <class... ARGS> static gctools::tagged_pointer<T> allocate(ARGS&&... args) {
    return allocate_kind(Header_s::StampWtagMtag::make<T>(), sizeof_with_header<T>(), std::forward<ARGS>(args)...);
  };

  template <class... ARGS>
  static gctools::tagged_pointer<T> allocate_kind(const Header_s::StampWtagMtag& the_header, size_t size, ARGS&&... args) {
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_uncollectable_allocation(the_header, size);
    T* obj = HeaderPtrToGeneralPtr<T>(base);
    new (obj) T(std::forward<ARGS>(args)...);
    handle_all_queued_interrupts();
    gctools::tagged_pointer<T> tagged_obj(obj);
    return tagged_obj;
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_uncollectable_allocation(the_header, size);
    T* obj = HeaderPtrToGeneralPtr<T>(base);
    new (obj) T(std::forward<ARGS>(args)...);
    handle_all_queued_interrupts();
    gctools::tagged_pointer<T> tagged_obj(obj);
    return tagged_obj;
#elif defined(USE_MPS)
    globalMpsMetrics.nonMovingAllocations++;
    tagged_pointer<T> tagged_obj = general_mps_allocation<tagged_pointer<T>>(
        the_header, size, my_thread_allocation_points._non_moving_allocation_point, std::forward<ARGS>(args)...);
    return tagged_obj;
#endif
  }

  template <class... ARGS> static T* untagged_allocate(ARGS&&... args) { return allocate(args...); }

  static void deallocate(gctools::tagged_pointer<T> memory) {
#if defined(USE_BOEHM)
    GC_FREE(&*memory);
#elif defined(USE_MPS) && !defined(RUNNING_PRECISEPREP)
    throw_hard_error("I need a way to deallocate MPS allocated objects that are not moveable or collectable");
    GCTOOLS_ASSERT(false); // ADD SOME WAY TO FREE THE MEMORY
#elif defined(USE_MMTK)
    MISSING_GC_SUPPORT();
#endif
  };

  static void untagged_deallocate(void* memory) {
#if defined(USE_BOEHM)
    GC_FREE(memory);
#elif defined(USE_MPS)
    GCTOOLS_ASSERT(false); // ADD SOME WAY TO FREE THE MEMORY
#elif defined(USE_MMTK)
    MISSING_GC_SUPPORT();
#endif
  };

  static void* allocateRootsAndZero(size_t num) {
#if defined(USE_BOEHM)
    void* buffer = ALIGNED_GC_MALLOC_UNCOLLECTABLE(sizeof(void*) * num);
    memset(buffer, 0, sizeof(void*) * num);
    return buffer;
#elif defined(USE_MPS)
    void* buffer = NULL;
    printf("%s:%d:%s Add support\n", __FILE__, __LINE__, __FUNCTION__);
    return buffer;
#elif defined(USE_MMTK)
    MISSING_GC_SUPPORT();
#endif
  }

  static void freeRoots(void* roots) {
#if defined(USE_BOEHM)
    GC_FREE(roots);
#else
    MISSING_GC_SUPPORT();
#endif
  };
};

template <class Stage, class Cons, class Register> struct ConsAllocator {
  template <class... ARGS>
#ifdef ALWAYS_INLINE_MPS_ALLOCATIONS
  __attribute__((always_inline))
#else
  inline
#endif
  static smart_ptr<Cons>
  allocate(ARGS&&... args) {
    DO_DRAG_CONS_ALLOCATION();
#if defined(USE_BOEHM)
    Cons* cons;
    size_t cons_size = ConsSizeCalculator<Stage, Cons, Register>::value();
    cons = do_boehm_cons_allocation<Stage, Cons, ARGS...>(cons_size, std::forward<ARGS>(args)...);
    handle_all_queued_interrupts<Stage>();
    return smart_ptr<Cons>((Tagged)tag_cons(cons));
#elif defined(USE_MMTK)
    Cons* cons;
    size_t cons_size = ConsSizeCalculator<Stage, Cons, Register>::value();
    cons = do_mmtk_cons_allocation<Stage, Cons, ARGS...>(cons_size, std::forward<ARGS>(args)...);
    handle_all_queued_interrupts<Stage>();
    return smart_ptr<Cons>((Tagged)tag_cons(cons));
#elif defined(USE_MPS)
    mps_ap_t obj_ap = my_thread_allocation_points._cons_allocation_point;
    //        globalMpsMetrics.consAllocations++;
    smart_ptr<Cons> obj = do_cons_mps_allocation<Stage, Cons, Register>(obj_ap, "CONS", std::forward<ARGS>(args)...);
    return obj;
#endif
  }

#ifdef USE_PRECISE_GC
  static smart_ptr<Cons> snapshot_save_load_allocate(Header_s::BadgeStampWtagMtag& the_header, core::T_sp car, core::T_sp cdr) {
#if defined(USE_BOEHM)
    Header_s* header = reinterpret_cast<Header_s*>(ALIGNED_GC_MALLOC_KIND(
        STAMP_UNSHIFT_WTAG(STAMPWTAG_CONS), SizeofConsHeader() + sizeof(Cons), global_cons_kind, &global_cons_kind)); // wasMTAG
    header->_badge_stamp_wtag_mtag._header_badge.store(the_header._header_badge.load());
    header->_badge_stamp_wtag_mtag._value = the_header._value;
#elif defined(USE_MMTK)
    Header_s* header = reinterpret_cast<Header_s*> do_mmtk_allocate_cons(STAMP_UNSHIFT_WTAG(STAMPWTAG_CONS),
                                                                         SizeofConsHeader() + sizeof(Cons)); // wasMTAG
    header->_badge_stamp_wtag_mtag = the_header;
#else
    MISSING_GC_SUPPORT();
#endif
    Cons* cons = (Cons*)HeaderPtrToConsPtr(header);
    new (cons) Cons(car, cdr);
    return smart_ptr<Cons>((Tagged)tag_cons(cons));
  }
#endif
};
}; // namespace gctools

namespace gctools {
template <class OT, GCInfo_policy Policy = normal> struct GCObjectAppropriatePoolAllocator {
  typedef OT value_type;
  typedef OT* pointer_type;
  typedef smart_ptr<OT> smart_pointer_type;
  template <typename Stage, typename... ARGS>
  static smart_pointer_type allocate_in_appropriate_pool_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size,
                                                              ARGS&&... args) {
    DO_DRAG_GENERAL_ALLOCATION();
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_general_allocation<Stage>(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_general_allocation(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MPS)
    mps_ap_t obj_ap = my_thread_allocation_points._automatic_mostly_copying_allocation_point;
    globalMpsMetrics.movingAllocations++;
    smart_ptr<OT> sp = general_mps_allocation<smart_ptr<OT>>(the_header, size, obj_ap, std::forward<ARGS>(args)...);
    return sp;
#endif
  };

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init) {
    size_t sizeWithHeader = sizeof(Header_s) + (snapshot_save_load_init->_clientEnd - snapshot_save_load_init->_clientStart);
    DO_DRAG_GENERAL_ALLOCATION();
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_general_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, sizeWithHeader);
    // transfer the badge
    base->_badge_stamp_wtag_mtag._header_badge.store(
        snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag._header_badge.load());
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = snapshot_save_load_init->_headStart->_source;
#endif
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
#ifdef DEBUG_GUARD
    uintptr_t guardBefore0 = *(uintptr_t*)((uintptr_t*)ptr - 1);
    uintptr_t guardAfter0 = *(uintptr_t*)((uintptr_t*)((char*)ptr + sizeWithHeader - sizeof(Header_s)) + 1);
#endif
    new (ptr) OT(snapshot_save_load_init);
#ifdef DEBUG_GUARD
    uintptr_t guardBefore1 = *(uintptr_t*)((uintptr_t*)ptr - 1);
    uintptr_t guardAfter1 = *(uintptr_t*)((uintptr_t*)((char*)ptr + sizeWithHeader - sizeof(Header_s)) + 1);
    if (guardBefore0 != guardBefore1) {
      printf("%s:%d:%s We stomped on the memory before the object\n", __FILE__, __LINE__, __FUNCTION__);
    }
    if (guardAfter0 != guardAfter1) {
      printf("%s:%d:%s We stomped on the memory after the object\n", __FILE__, __LINE__, __FUNCTION__);
    }
#endif
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_general_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, sizeWithHeader);
    base->_badge_stamp_wtag_mtag._header_badge.store(
        snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag._header_badge.load());
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = snapshot_save_load_init->_headStart->_source;
#endif
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
#ifdef DEBUG_GUARD
    uintptr_t guardBefore0 = *(uintptr_t*)((uintptr_t*)ptr - 1);
    uintptr_t guardAfter0 = *(uintptr_t*)((uintptr_t*)((char*)ptr + sizeWithHeader - sizeof(Header_s)) + 1);
#endif
    new (ptr) OT(snapshot_save_load_init);
#ifdef DEBUG_GUARD
    uintptr_t guardBefore1 = *(uintptr_t*)((uintptr_t*)ptr - 1);
    uintptr_t guardAfter1 = *(uintptr_t*)((uintptr_t*)((char*)ptr + sizeWithHeader - sizeof(Header_s)) + 1);
    if (guardBefore0 != guardBefore1) {
      printf("%s:%d:%s We stomped on the memory before the object\n", __FILE__, __LINE__, __FUNCTION__);
    }
    if (guardAfter0 != guardAfter1) {
      printf("%s:%d:%s We stomped on the memory after the object\n", __FILE__, __LINE__, __FUNCTION__);
    }
#endif
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MPS)
    printf("%s:%d:%s add support for mps\n", __FILE__, __LINE__, __FUNCTION__);
#endif
  };

  static void deallocate(OT* memory){
      // Nothing needs to be done but this function needs to be here
      // so that the static analyzer has something to call
  };
};

template <class OT> struct GCObjectAppropriatePoolAllocator<OT, /* Policy= */ atomic> {
  typedef OT value_type;
  typedef OT* pointer_type;
  typedef smart_ptr<OT> smart_pointer_type;
  template <typename Stage, typename... ARGS>
  static smart_pointer_type allocate_in_appropriate_pool_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size,
                                                              ARGS&&... args) {
#if defined(USE_BOEHM)
    // Atomic objects (do not contain pointers) are allocated in separate pool
    Header_s* base = do_boehm_atomic_allocation<Stage>(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MMTK)
    // Atomic objects (do not contain pointers) are allocated in separate pool
    Header_s* base = do_mmtk_atomic_allocation(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MPS)
    mps_ap_t obj_ap = my_thread_allocation_points._automatic_mostly_copying_zero_rank_allocation_point;
    globalMpsMetrics.movingZeroRankAllocations++;
    smart_pointer_type sp = general_mps_allocation<smart_pointer_type>(the_header, size, obj_ap, std::forward<ARGS>(args)...);
    return sp;
#endif
  };
  static void deallocate(OT* memory){
      // Nothing needs to be done but this function needs to be here
      // so that the static analyzer has something to call
  };

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init,
                                                        size_t size) {
#if defined(USE_BOEHM)
    Header_s* base =
        do_boehm_atomic_allocation<SnapshotLoadStage>(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, size);
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = snapshot_save_load_init->_headStart->_source;
#endif
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(snapshot_save_load_init);
    printf("%s:%d:%s This is where we should copy in the stuff from the snapshot_save_load_init object\n", __FILE__, __LINE__,
           __FUNCTION__);
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_atomic_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, size);
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = snapshot_save_load_init->_headStart->_source;
#endif
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(snapshot_save_load_init);
    printf("%s:%d:%s This is where we should copy in the stuff from the snapshot_save_load_init object\n", __FILE__, __LINE__,
           __FUNCTION__);
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MPS)
    printf("%s:%d:%s add support for mps\n", __FILE__, __LINE__, __FUNCTION__);
#endif
  };
};

/*! This Policy of collectible_immobile may not be a useful policy.
When would I ever want the GC to automatically collect objects but not move them?
*/
template <class OT> struct GCObjectAppropriatePoolAllocator<OT, /* Policy= */ collectable_immobile> {
  typedef OT value_type;
  typedef OT* pointer_type;
  typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
  template <typename Stage, typename... ARGS>
  static smart_pointer_type allocate_in_appropriate_pool_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size,
                                                              ARGS&&... args) {
    DO_DRAG_GENERAL_ALLOCATION();
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_general_allocation<Stage>(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_general_allocation(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MPS)
    mps_ap_t obj_ap = my_thread_allocation_points._non_moving_allocation_point;
    globalMpsMetrics.nonMovingAllocations++;
    smart_pointer_type sp = general_mps_allocation<smart_pointer_type>(the_header, size, obj_ap, std::forward<ARGS>(args)...);
    return sp;
#endif
  };
  static void deallocate(OT* memory){
      // Nothing needs to be done but this function needs to be here
      // so that the static analyzer has something to call
  };

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init,
                                                        size_t size) {
    DO_DRAG_GENERAL_ALLOCATION();
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_general_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, size);
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = snapshot_save_load_init->_headStart->_source;
#endif
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(snapshot_save_load_init);
    printf("%s:%d:%s This is where we should copy in the stuff from the snapshot_save_load_init object\n", __FILE__, __LINE__,
           __FUNCTION__);
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_general_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, size);
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = snapshot_save_load_init->_headStart->_source;
#endif
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(snapshot_save_load_init);
    printf("%s:%d:%s This is where we should copy in the stuff from the snapshot_save_load_init object\n", __FILE__, __LINE__,
           __FUNCTION__);
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MPS)
    printf("%s:%d:%s add support for mps\n", __FILE__, __LINE__, __FUNCTION__);
#endif
  };
};

/*! This is for CL classes that derive from C++ classes and other CL classes that
should not be managed by the GC */
template <class OT> struct GCObjectAppropriatePoolAllocator<OT, unmanaged> {
  typedef OT value_type;
  typedef OT* pointer_type;
  typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
  template <typename Stage, typename... ARGS>
  static smart_pointer_type allocate_in_appropriate_pool_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size,
                                                              ARGS&&... args) {
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_uncollectable_allocation(the_header, size);
    OT* obj = HeaderPtrToGeneralPtr<OT>(base);
    new (obj) OT(std::forward<ARGS>(args)...);
    handle_all_queued_interrupts();
    gctools::smart_ptr<OT> sp(obj);
    return sp;
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_uncollectable_allocation(the_header, size);
    OT* obj = HeaderPtrToGeneralPtr<OT>(base);
    new (obj) OT(std::forward<ARGS>(args)...);
    handle_all_queued_interrupts();
    gctools::smart_ptr<OT> sp(obj);
    return sp;
#elif defined(USE_MPS)
    mps_ap_t obj_ap = my_thread_allocation_points._non_moving_allocation_point;
    globalMpsMetrics.nonMovingAllocations++;
    gctools::smart_ptr<OT> sp =
        general_mps_allocation<gctools::smart_ptr<OT>>(the_header, size, obj_ap, std::forward<ARGS>(args)...);
    return sp;
#endif
  }

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init,
                                                        size_t size) {
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_uncollectable_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, size);
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = snapshot_save_load_init->_headStart->_source;
#endif
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(snapshot_save_load_init);
    printf("%s:%d:%s This is where we should copy in the stuff from the snapshot_save_load_init object\n", __FILE__, __LINE__,
           __FUNCTION__);
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_uncollectable_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, size);
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = snapshot_save_load_init->_headStart->_source;
#endif
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(snapshot_save_load_init);
    printf("%s:%d:%s This is where we should copy in the stuff from the snapshot_save_load_init object\n", __FILE__, __LINE__,
           __FUNCTION__);
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
#elif defined(USE_MPS)
    printf("%s:%d:%s add support for mps\n", __FILE__, __LINE__, __FUNCTION__);
#endif
  };

  static void deallocate(OT* memory) {
#if defined(USE_BOEHM)
    printf("%s:%d Using GC_FREE to free memory at@%p\n", __FILE__, __LINE__, memory);
    GC_FREE(memory);
#elif defined(USE_MMTK)
    MISSING_GC_SUPPORT();
#elif defined(USE_MPS) && !defined(RUNNING_PRECISEPREP)
    throw_hard_error(" GCObjectAppropriatePoolAllocator<OT, unmanaged > I need a way to deallocate MPS allocated objects that are "
                     "not moveable or collectable");
    GCTOOLS_ASSERT(false); // ADD SOME WAY TO FREE THE MEMORY
#endif
  };
};
} // namespace gctools

typedef void (*BoehmFinalizerFn)(void* obj, void* data);

extern "C" {
void my_mps_finalize(core::T_O* tagged);
};

namespace gctools {
extern void boehm_general_finalizer_from_BoehmFinalizer(void* client, void* dummy);

#ifdef USE_BOEHM
template <class OT> void BoehmFinalizer(void* base, void* data) {
  //  printf("%s:%d:%s Finalizing base=%p\n", __FILE__, __LINE__, __FUNCTION__, base);
  OT* client = HeaderPtrToGeneralPtr<OT>(base);
  boehm_general_finalizer_from_BoehmFinalizer((void*)client, data);
  client->~OT();
  GC_FREE(base);
}
#endif

template <class OT, bool Needed = true> struct GCObjectFinalizer {
  typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
  static void finalizeIfNeeded(smart_pointer_type sp) {
#if defined(USE_BOEHM)
    void* dummyData;
    BoehmFinalizerFn dummyFn;
    //    printf("%s:%d About to register finalize base -> %p\n", __FILE__, __LINE__, (void*)sp.tagged_());
    GC_register_finalizer_no_order(SmartPtrToBasePtr(sp), BoehmFinalizer<OT>, NULL, &dummyFn, &dummyData);
//    printf("%s:%d Finished finalize sp -> %p\n", __FILE__, __LINE__, (void*)sp.tagged_());
#elif defined(USE_MMTK)
    printf("%s:%d:%s Add finalization support for mmtk\n", __FILE__, __LINE__, __FUNCTION__);
#elif defined(USE_MPS)
    // Defined in mpsGarbageCollection.cc
    my_mps_finalize(sp.raw_());
#endif
  };
};

template <class OT> struct GCObjectFinalizer<OT, false> {
  typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
  static void finalizeIfNeeded(smart_pointer_type sp){
      // finalize not needed
  };
};
} // namespace gctools

namespace gctools {

template <class OT> class GCObjectAllocator {
public:
  typedef OT value_type;
  typedef OT* pointer_type;
  typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;

public:
  template <typename... ARGS> static smart_pointer_type root_allocate(ARGS&&... args) {
    return root_allocate_kind(GCStamp<OT>::Stamp, sizeof_with_header<OT>(), std::forward<ARGS>(args)...);
  }
  template <typename... ARGS>
  static smart_pointer_type root_allocate_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size, ARGS&&... args) {
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_uncollectable_allocation(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    GCObjectInitializer<OT, /*gctools::*/ GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_uncollectable_allocation(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    GCObjectInitializer<OT, /*gctools::*/ GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#elif defined(USE_MPS)
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind(
        the_header, size, std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    handle_all_queued_interrupts();
    return sp;
#endif
  };

  template <typename... ARGS>
  static smart_pointer_type allocate_kind_partial_scan(size_t scanSize, const Header_s::BadgeStampWtagMtag& the_header, size_t size,
                                                       ARGS&&... args) {
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind_partial_scan(
            scanSize, the_header, size, std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, /*gctools::*/ GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    handle_all_queued_interrupts();
    return sp;
  };

  template <typename Stage, typename... ARGS>
  static smart_pointer_type allocate_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size, ARGS&&... args) {
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::template allocate_in_appropriate_pool_kind<Stage>(
            the_header, size, std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    handle_all_queued_interrupts<Stage>();
    return sp;
  };

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init) {
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::snapshot_save_load_allocate(snapshot_save_load_init);
    // No initializer
    //      GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    return sp;
  };

  template <typename... ARGS>
  static smart_pointer_type static_allocate_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size, ARGS&&... args) {
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, unmanaged>::template allocate_in_appropriate_pool_kind<gctools::RuntimeStage>(
            the_header, size, std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    handle_all_queued_interrupts();
    return sp;
  };

  static smart_pointer_type register_class_with_redeye() {
    throw_hard_error("Never call this - it's only used to register with the redeye static analyzer");
  }
  static smart_pointer_type copy_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size, const OT& that) {
#if defined(USE_BOEHM)
    // Copied objects must be allocated in the appropriate pool
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::template allocate_in_appropriate_pool_kind<gctools::RuntimeStage>(
            the_header, size, that);
    // Copied objects are not initialized.
    // Copied objects are finalized if necessary
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#elif defined(USE_MMTK)
    MISSING_GC_SUPPORT();
#elif defined(USE_MPS)
    // Copied objects must be allocated in the appropriate pool
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::template allocate_in_appropriate_pool_kind<gctools::RuntimeStage>(
            the_header, size, that);
    // Copied objects are not initialized.
    // Copied objects are finalized if necessary
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#endif
  }
};
}; // namespace gctools

namespace gctools {
template <class OT, bool Can> struct GCObjectDefaultConstructorAllocator {};

template <class OT> struct GCObjectDefaultConstructorAllocator<OT, true> {
  static smart_ptr<OT> allocate(const Header_s::BadgeStampWtagMtag& kind) {
    // FIXSTAMP
    return GCObjectAllocator<OT>::template allocate_kind<gctools::RuntimeStage>(kind, sizeof_with_header<OT>());
  }
};

template <class OT> struct GCObjectDefaultConstructorAllocator<OT, false> {
  [[noreturn]] static smart_ptr<OT> allocate(const Header_s::BadgeStampWtagMtag& kind) {
    lisp_errorCannotAllocateInstanceWithMissingDefaultConstructor(OT::static_classSymbol());
  }
};
}; // namespace gctools

namespace gctools {
/*! This is the public interface to the GCObjectAllocator */
template <class OT> class GC {
public:
  typedef OT value_type;
  typedef OT* pointer_type;
  typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;

public:
  template <typename... ARGS> static smart_pointer_type root_allocate(ARGS&&... args) {
    return GCObjectAllocator<OT>::root_allocate_kind(Header_s::StampWtagMtag::make<OT>(), sizeof_with_header<OT>(),
                                                     std::forward<ARGS>(args)...);
  }

  template <typename... ARGS> static smart_pointer_type root_allocate_with_stamp(ARGS&&... args) {
    return GCObjectAllocator<OT>::root_allocate_kind(Header_s::BadgeStampWtagMtag::make<OT>(), sizeof_with_header<OT>(),
                                                     std::forward<ARGS>(args)...);
  }

  template <typename... ARGS> static smart_pointer_type never_invoke_allocator(ARGS&&... args) {
    auto kind = GCStamp<OT>::StampWtag;
    return GCObjectAllocator<OT>::allocate_kind(kind, 0, std::forward<ARGS>(args)...);
  }

  template <typename... ARGS> static smart_pointer_type allocate_kind(const Header_s::BadgeStampWtagMtag& kind, ARGS&&... args) {
    size_t size = sizeof_with_header<OT>();
    return GCObjectAllocator<OT>::allocate_kind(kind, size, std::forward<ARGS>(args)...);
  }

  template <typename... ARGS>
  static smart_pointer_type allocate_instance(const Header_s::BadgeStampWtagMtag& kind, size_t size, ARGS&&... args) {
    return GCObjectAllocator<OT>::template allocate_kind<gctools::RuntimeStage>(kind, size, std::forward<ARGS>(args)...);
  }

  template <typename Stage = RuntimeStage, typename... ARGS> static smart_pointer_type allocate(ARGS&&... args) {
    auto kind = Header_s::StampWtagMtag::make_StampWtagMtag(OT::static_ValueStampWtagMtag);
    size_t size = sizeof_with_header<OT>();
    return GCObjectAllocator<OT>::template allocate_kind<Stage>(kind, size, std::forward<ARGS>(args)...);
  }

  static smart_pointer_type allocate_with_default_constructor() {
    return GCObjectDefaultConstructorAllocator<OT, std::is_default_constructible<OT>::value>::allocate(
        Header_s::BadgeStampWtagMtag::make_StampWtagMtag(OT::static_ValueStampWtagMtag));
  }

  /*! Allocate enough space for capacity elements, but set the length to length */

  // Allocates an object with proper header and everything.
  // Uses the underlying constructor. Like, GC<SimpleVector_O>::allocate_container(...)
  // ends up passing the ... to the SimpleVector_O constructor.
  template <typename Stage, typename... ARGS>
  static smart_pointer_type allocate_container(bool static_container_p, int64_t length, ARGS&&... args) {
    size_t capacity = std::abs(length);
    size_t size = sizeof_container_with_header<OT>(capacity);
    if (static_container_p)
      return GCObjectAllocator<OT>::static_allocate_kind(
          Header_s::BadgeStampWtagMtag::make_StampWtagMtag(OT::static_ValueStampWtagMtag), size, length,
          std::forward<ARGS>(args)...);
    return GCObjectAllocator<OT>::template allocate_kind<Stage>(Header_s::BadgeStampWtagMtag(OT::static_ValueStampWtagMtag), size,
                                                                length, std::forward<ARGS>(args)...);
  }

  template <typename Stage, typename... ARGS>
  static smart_pointer_type allocate_container_null_terminated_string(bool static_container_p, size_t length, ARGS&&... args) {
    size_t capacity = length + 1;
    size_t size = sizeof_container_with_header<OT>(capacity);
    if (static_container_p)
      return GCObjectAllocator<OT>::static_allocate_kind(
          Header_s::BadgeStampWtagMtag::make_StampWtagMtag(OT::static_ValueStampWtagMtag), size, length,
          std::forward<ARGS>(args)...);
    else
      return GCObjectAllocator<OT>::template allocate_kind<Stage>(
          Header_s::BadgeStampWtagMtag::make_StampWtagMtag(OT::static_ValueStampWtagMtag), size, length,
          std::forward<ARGS>(args)...);
  }

  /*! Allocate enough space for capacity elements, but set the length to length */

  // Allocates an object with proper header and everything.
  // Uses the underlying constructor. Like, GC<SimpleVector_O>::allocate_container(...)
  // ends up passing the ... to the SimpleVector_O constructor.
  template <typename... ARGS>
  static smart_pointer_type allocate_container_partial_scan(size_t dataScanSize, int64_t length, ARGS&&... args) {
    size_t capacity = std::abs(length);
    size_t size = sizeof_container_with_header<OT>(capacity);
    size_t scanSize = sizeof_container_with_header<OT>(dataScanSize);
    return GCObjectAllocator<OT>::allocate_kind_partial_scan(
        scanSize, Header_s::BadgeStampWtagMtag::make_StampWtagMtag(OT::static_ValueStampWtagMtag), size, length,
        std::forward<ARGS>(args)...);
  }

  template <typename... ARGS>
  static smart_pointer_type allocate_bitunit_container(bool static_container_p, size_t length, ARGS&&... args) {
    size_t size = sizeof_bitunit_container_with_header<OT>(length);
#ifdef DEBUG_BITUNIT_CONTAINER
    printf("%s:%d  In allocate_bitunit_container length = %lu  size= %lu\n", __FILE__, __LINE__, length, size);
#endif
    smart_pointer_type result;
    if (static_container_p)
      result = GCObjectAllocator<OT>::static_allocate_kind(Header_s::BadgeStampWtagMtag::make<OT>(), size, length,
                                                           std::forward<ARGS>(args)...);
    else
      result = GCObjectAllocator<OT>::template allocate_kind<RuntimeStage>(Header_s::BadgeStampWtagMtag::make<OT>(), size, length,
                                                                           std::forward<ARGS>(args)...);
#if DEBUG_BITUNIT_CONTAINER
    {
      printf("%s:%d allocate_bitunit_container \n", __FILE__, __LINE__);
      printf("            Allocated object tagged ptr = %p\n", (void*)result.raw_());
    }
#endif

    return result;
  }

  static smart_pointer_type copy(const OT& that) {
    return GCObjectAllocator<OT>::copy_kind(Header_s::BadgeStampWtagMtag::make<OT>(), sizeof_with_header<OT>(), that);
  }

  static void deallocate_unmanaged_instance(OT* obj) { GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::deallocate(obj); }
};
}; // namespace gctools

namespace gctools {
template <class TY> class GCContainerAllocator /* : public GCAlloc<TY> */ {
public:
  // type definitions
  typedef TY container_type;
  typedef container_type* container_pointer;
  typedef typename container_type::value_type value_type;
  typedef value_type* pointer;
  typedef const value_type* const_pointer;
  typedef value_type& reference;
  typedef const value_type& const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  /* constructors and destructor
   * - nothing to do because the allocator has no state
   */
  GCContainerAllocator() throw() {}
  GCContainerAllocator(const GCContainerAllocator&) throw() {}
  template <class U> GCContainerAllocator(const GCContainerAllocator<U>&) throw() {}
  ~GCContainerAllocator() throw() {}

  // return maximum number of elements that can be allocated
  size_type max_size() const throw() { return std::numeric_limits<std::size_t>::max() / sizeof(value_type); }

  // allocate but don't initialize num elements of type value_type
  gc::tagged_pointer<container_type> allocate(size_type num, const void* = 0) {
    return allocate_kind(Header_s::BadgeStampWtagMtag::make<TY>(), num);
  }

  // allocate but don't initialize num elements of type value_type
  gc::tagged_pointer<container_type> allocate_kind(const Header_s::BadgeStampWtagMtag& the_header, size_type num, const void* = 0) {
    DO_DRAG_GENERAL_ALLOCATION();
#if defined(USE_BOEHM)
    size_t size = sizeof_container_with_header<TY>(num);
    Header_s* base = do_boehm_general_allocation(the_header, size);
    container_pointer myAddress = HeaderPtrToGeneralPtr<TY>(base);
    handle_all_queued_interrupts();
    return gctools::tagged_pointer<container_type>(myAddress);
#elif defined(USE_MMTK)
    size_t size = sizeof_container_with_header<TY>(num);
    Header_s* base = do_mmtk_general_allocation(the_header, size);
    container_pointer myAddress = HeaderPtrToGeneralPtr<TY>(base);
    handle_all_queued_interrupts();
    return gctools::tagged_pointer<container_type>(myAddress);
#elif defined(USE_MPS)
    size_t size = sizeof_container_with_header<TY>(num);
    mps_ap_t obj_ap = my_thread_allocation_points._automatic_mostly_copying_allocation_point;
    globalMpsMetrics.movingAllocations++;
    gc::tagged_pointer<container_type> obj =
        general_mps_allocation<gc::tagged_pointer<container_type>>(the_header, size, obj_ap, num);
    return obj;
#endif
  }

  // initialize elements of allocated storage p with value value
  template <typename... ARGS> void construct(const_pointer p, ARGS&&... args) {
    // initialize memory with placement new
    new ((void*)p) value_type(std::forward<ARGS>(args)...);
  }

  // destroy elements of initialized storage p
  void destroy(pointer p) {
    // Do nothing
  }

  // deallocate storage p of deleted elements
  void deallocate(gctools::tagged_pointer<container_type> p, size_type num) {
    // Do nothing
  }
};
}; // namespace gctools

namespace gctools {
template <class TY> class GCAbstractAllocator /* : public GCAlloc<TY> */ {
public:
  // type definitions
  typedef TY container_type;
  typedef container_type* container_pointer;
  typedef typename container_type::value_type value_type;
  typedef value_type* pointer;
  typedef const value_type* const_pointer;
  typedef value_type& reference;
  typedef const value_type& const_reference;
  typedef std::size_t size_type;
  /* constructors and destructor
   * - nothing to do because the allocator has no state
   */
  GCAbstractAllocator() throw() {}
  ~GCAbstractAllocator() throw() {}

  // allocate but don't initialize num elements of type value_type
  void never_invoke_allocate(){};
};
}; // namespace gctools

namespace gctools {
/*! This allocator is for allocating containers that are fixed in position and Capacity.
      Things like the MultipleValues for multiple value return are allocated with this.
      */

template <class TY> class GCContainerNonMoveableAllocator /* : public GCAlloc<TY> */ {
public:
  // type definitions
  typedef TY container_type;
  typedef container_type* container_pointer;
  typedef typename container_type::value_type value_type;
  typedef value_type* pointer;
  typedef const value_type* const_pointer;
  typedef value_type& reference;
  typedef const value_type& const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  /* constructors and destructor
   * - nothing to do because the allocator has no state
   */
  GCContainerNonMoveableAllocator() throw() {}
  GCContainerNonMoveableAllocator(const GCContainerNonMoveableAllocator&) throw() {}
  template <class U> GCContainerNonMoveableAllocator(const GCContainerNonMoveableAllocator<U>&) throw() {}
  ~GCContainerNonMoveableAllocator() throw() {}

  // return maximum number of elements that can be allocated
  size_type max_size() const throw() { return std::numeric_limits<std::size_t>::max() / sizeof(value_type); }

  // allocate but don't initialize num elements of type value_type
  gctools::tagged_pointer<container_type> allocate_kind(const Header_s::BadgeStampWtagMtag& the_header, size_type num,
                                                        const void* = 0) {
    DO_DRAG_GENERAL_ALLOCATION();
#if defined(USE_BOEHM)
    size_t size = sizeof_container_with_header<TY>(num);
    // prepend a one pointer header with a pointer to the typeinfo.name
    Header_s* base = do_boehm_general_allocation(the_header, size);
    container_pointer myAddress = HeaderPtrToGeneralPtr<TY>(base);
    handle_all_queued_interrupts();
    return myAddress;
#elif defined(USE_MMTK)
    size_t size = sizeof_container_with_header<TY>(num);
    // prepend a one pointer header with a pointer to the typeinfo.name
    Header_s* base = do_mmtk_general_allocation(the_header, size);
    container_pointer myAddress = HeaderPtrToGeneralPtr<TY>(base);
    handle_all_queued_interrupts();
    return myAddress;
#elif defined(USE_MPS)
    size_t size = sizeof_container_with_header<TY>(num);
    mps_ap_t obj_ap = my_thread_allocation_points._non_moving_allocation_point;
    globalMpsMetrics.nonMovingAllocations++;
    gctools::tagged_pointer<container_type> obj =
        general_mps_allocation<gc::tagged_pointer<container_type>>(the_header, size, obj_ap, num);
    return obj;
#endif
  }

  // initialize elements of allocated storage p with value value
  template <typename... ARGS> void construct(pointer p, ARGS&&... args) {
    // initialize memory with placement new
    new ((void*)p) value_type(std::forward<ARGS>(args)...);
  }

  // destroy elements of initialized storage p
  void destroy(pointer p) {
    // Do nothing
  }

  // deallocate storage p of deleted elements
  void deallocate(gctools::tagged_pointer<container_type> p, size_type num) {
    // Do nothing
  }
};
}; // namespace gctools

namespace gctools {

#ifdef USE_BOEHM
inline void BoehmWeakLinkDebugFinalizer(void* base, void* data) {
  printf("%s:%d Boehm finalized weak linked address %p at %p\n", __FILE__, __LINE__, base, data);
}
#endif

struct WeakLinks {};
struct StrongLinks {};

template <class KT, class VT, class LT> struct Buckets;

#ifdef USE_MPS
//
// Allocation point for weak links is different from strong links
//
template <typename StrongWeakType = StrongLinks> struct StrongWeakAllocationPoint {
  static mps_ap_t& get() { return my_thread_allocation_points._strong_link_allocation_point; };
  static const char* name() { return "strong_links"; };
};

template <> struct StrongWeakAllocationPoint<WeakLinks> {
  static mps_ap_t& get() { return my_thread_allocation_points._weak_link_allocation_point; };
  static const char* name() { return "weak_links"; };
};

#endif

template <class VT> class GCBucketAllocator {};

template <class VT, class StrongWeakLinkType> class GCBucketAllocator<Buckets<VT, VT, StrongWeakLinkType>> {
public:
  typedef Buckets<VT, VT, StrongWeakLinkType> TY;
  typedef TY container_type;
  typedef container_type* container_pointer;
  typedef typename container_type::value_type value_type;
  typedef value_type* pointer;
  typedef const value_type* const_pointer;
  typedef value_type& reference;
  typedef const value_type& const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  /* constructors and destructor
   * - nothing to do because the allocator has no state
   */
  GCBucketAllocator() throw() {}
  GCBucketAllocator(const GCBucketAllocator&) throw() {}
  ~GCBucketAllocator() throw() {}

  // return maximum number of elements that can be allocated
  size_type max_size() const throw() { return std::numeric_limits<std::size_t>::max() / sizeof(value_type); }

  // allocate but don't initialize num elements of type value_type
  static gctools::tagged_pointer<container_type> allocate(Header_s::BadgeStampWtagMtag the_header, size_type num, const void* = 0) {
    size_t size = sizeof_container_with_header<container_type>(num);
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_weak_allocation(the_header, size);
    container_pointer myAddress = (container_pointer)HeaderPtrToWeakPtr(base);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) container_type(num);
#ifdef DEBUG_GCWEAK
    printf("%s:%d Check if Buckets has been initialized to unbound\n", __FILE__, __LINE__);
#endif
    return gctools::tagged_pointer<container_type>(myAddress);
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_weak_allocation(the_header, size);
    container_pointer myAddress = (container_pointer)HeaderPtrToWeakPtr(base);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) container_type(num);
#ifdef DEBUG_GCWEAK
    printf("%s:%d Check if Buckets has been initialized to unbound\n", __FILE__, __LINE__);
#endif
    return gctools::tagged_pointer<container_type>(myAddress);
#elif defined(USE_MPS)
    printf("%s:%d:%s Handle allocation\n", __FILE__, __LINE__, __FUNCTION__);
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    printf("%s:%d:%s Handle weak object allocation properly - I added normal headers\n", __FILE__, __LINE__, __FUNCTION__);
    gctools::tagged_pointer<container_type> obj = do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(
        size, StrongWeakAllocationPoint<StrongWeakLinkType>::get(), StrongWeakAllocationPoint<StrongWeakLinkType>::name(), num);
    return obj;
#endif
  }

  static gctools::tagged_pointer<container_type> snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* init) {
    size_t size = (init->_clientEnd - init->_clientStart) + SizeofWeakHeader();
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_weak_allocation(init->_headStart->_badge_stamp_wtag_mtag, size);
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = init->_headStart->_source;
#endif
    container_pointer myAddress = (container_pointer)HeaderPtrToWeakPtr(base);
    new (myAddress) container_type(init);
    return gctools::tagged_pointer<container_type>(myAddress);
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_weak_allocation(init->_headStart->_badge_stamp_wtag_mtag, size);
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = init->_headStart->_source;
#endif
    container_pointer myAddress = (container_pointer)HeaderPtrToWeakPtr(base);
    new (myAddress) container_type(init);
    return gctools::tagged_pointer<container_type>(myAddress);
#elif defined(USE_MPS)
    printf("%s:%d:%s Handle allocation in MPS\n", __FILE__, __LINE__, __FUNCTION__);
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    printf("%s:%d:%s Handle weak object allocation properly - I added normal headers\n", __FILE__, __LINE__, __FUNCTION__);
    gctools::tagged_pointer<container_type> obj = do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(
        size, StrongWeakAllocationPoint<StrongWeakLinkType>::get(), StrongWeakAllocationPoint<StrongWeakLinkType>::name());
    return obj;
#endif
  }

  // initialize elements of allocated storage p with value value
  template <typename... ARGS> void construct(pointer p, ARGS&&... args) {
    // initialize memory with placement new
    throw_hard_error("What do I do here");
    //            new((void*)p)value_type(std::forward<ARGS>(args)...);
  }

  // destroy elements of initialized storage p
  void destroy(pointer p) {
    // Do nothing
  }

  // deallocate storage p of deleted elements
  void deallocate(gctools::tagged_pointer<container_type> p, size_type num) {
    // Do nothing
  }
};

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

template <class KT, class VT, class LT> struct Mapping;

template <class TY> class GCMappingAllocator /* : public GCAlloc<TY> */ {};

template <class VT, class StrongWeakLinkType> class GCMappingAllocator<Mapping<VT, VT, StrongWeakLinkType>> {
public:
  typedef Mapping<VT, VT, StrongWeakLinkType> TY;
  typedef TY container_type;
  typedef TY* container_pointer;
  /* constructors and destructor
   * - nothing to do because the allocator has no state
   */
  GCMappingAllocator() throw() {}
  GCMappingAllocator(const GCMappingAllocator&) throw() {}
  ~GCMappingAllocator() throw() {}

  // allocate but don't initialize num elements of type value_type
  static gctools::tagged_pointer<container_type> allocate(Header_s::BadgeStampWtagMtag the_header, const VT& val) {
    size_t size = sizeof_with_header<container_type>();
#if defined(USE_BOEHM)
    Header_s* base = do_boehm_weak_allocation(the_header, size);
    container_pointer myAddress = (container_pointer)HeaderPtrToWeakPtr(base);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) container_type(val);
    printf("%s:%d Check if Mapping has been initialized to unbound\n", __FILE__, __LINE__);
    return gctools::tagged_pointer<container_type>(myAddress);
#elif defined(USE_MMTK)
    Header_s* base = do_mmtk_weak_allocation(the_header, size);
    container_pointer myAddress = (container_pointer)HeaderPtrToWeakPtr(base);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) container_type(val);
    printf("%s:%d Check if Mapping has been initialized to unbound\n", __FILE__, __LINE__);
    return gctools::tagged_pointer<container_type>(myAddress);
#elif defined(USE_MPS)
    typedef typename GCHeader<TY>::HeaderType HeadT;
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    printf("%s:%d:%s Handle weak object allocation properly - I added normal headers\n", __FILE__, __LINE__, __FUNCTION__);
    gctools::tagged_pointer<container_type> obj = do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(
        size, StrongWeakAllocationPoint<StrongWeakLinkType>::get(), StrongWeakAllocationPoint<StrongWeakLinkType>::name());
    return obj;
#endif
  }
};

template <class VT> class GCWeakPointerAllocator {
public:
  typedef VT value_type;
  typedef value_type* value_pointer;
  typedef typename VT::value_type contained_type;
  /* constructors and destructor
   * - nothing to do because the allocator has no state
   */
  GCWeakPointerAllocator() throw() {}
  GCWeakPointerAllocator(const GCWeakPointerAllocator&) throw() {}
  ~GCWeakPointerAllocator() throw() {}

  // allocate but don't initialize num elements of type value_type
  static gctools::tagged_pointer<value_type> allocate(Header_s::BadgeStampWtagMtag the_header, const contained_type& val) {
    size_t size = sizeof_with_header<VT>();
#if defined(USE_BOEHM)
#ifdef DEBUG_GCWEAK
    printf("%s:%d Allocating WeakPointer with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__);
#endif
    Header_s* base = do_boehm_weak_allocation(the_header, size);
    VT* myAddress = (VT*)HeaderPtrToWeakPtr(base);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) VT(val);
    return gctools::tagged_pointer<value_type>(myAddress);
#elif defined(USE_MMTK)
#ifdef DEBUG_GCWEAK
    printf("%s:%d Allocating WeakPointer with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__);
#endif
    Header_s* base = do_mmtk_weak_allocation(the_header, size);
    VT* myAddress = (VT*)HeaderPtrToWeakPtr(base);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) VT(val);
    return gctools::tagged_pointer<value_type>(myAddress);
#elif defined(USE_MPS)
    mps_addr_t addr;
    value_pointer myAddress;
    gctools::tagged_pointer<value_type> obj = do_mps_weak_allocation<gctools::tagged_pointer<value_type>>(
        size, my_thread_allocation_points._weak_link_allocation_point, "weak_link3_Allocator", val);
    return obj;
#endif
  }
};
}; // namespace gctools

#endif // USE_BOEHM || USE_MPS || USE_MMTK
