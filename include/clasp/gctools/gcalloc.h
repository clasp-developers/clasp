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
#include <type_traits> // is_same_v
#include <clasp/gctools/interrupt.h>
#include <clasp/gctools/smart_pointers.h>
#include <clasp/gctools/threadlocal.fwd.h>
#include <clasp/gctools/snapshotSaveLoad.fwd.h>

#define STACK_ALIGNMENT alignof(char*)
#define STACK_ALIGN_UP(size) (((size) + STACK_ALIGNMENT - 1) & ~(STACK_ALIGNMENT - 1))

namespace gctools {
extern uintptr_t global_strong_weak_kind;
};

namespace gctools {
template <class OT>
static void initializeIfNeeded(smart_ptr<OT> sp) {
  if constexpr (GCInfo<OT>::NeedsInitialization) {
    if (sp.generalp()) // FIXME: Ought to be statically determinable
      sp.unsafe_general()->initialize();
  }
}
}; // namespace gctools

#if defined(USE_BOEHM) || defined(USE_MPS) || defined(USE_MMTK)

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
    Header_s* base = do_uncollectable_allocation(the_header, size);
    T* obj = HeaderPtrToGeneralPtr<T>(base);
    new (obj) T(std::forward<ARGS>(args)...);
    gctools::tagged_pointer<T> tagged_obj(obj);
    return tagged_obj;
  }

  static void deallocate(gctools::tagged_pointer<T> memory) {
    do_free(&*memory);
  };

  static void* allocateRootsAndZero(size_t num) {
    return do_allocate_zero(num);
  }

  static void freeRoots(void* roots) {
    do_free(roots);
  };
};

template <class Stage, class Cons> struct ConsAllocator {
  template <class... ARGS>
#ifdef ALWAYS_INLINE_MPS_ALLOCATIONS
  __attribute__((always_inline))
#else
  inline
#endif
  static smart_ptr<Cons>
  allocate(ARGS&&... args) {
    DO_DRAG_CONS_ALLOCATION();
    size_t cons_size = AlignUp(sizeof(Cons) + SizeofConsHeader());
    ConsHeader_s* header = do_cons_allocation<Stage, Cons>(cons_size);
    Cons* cons = (Cons*)HeaderPtrToConsPtr(header);
    new (cons) Cons(std::forward<ARGS>(args)...);
    return smart_ptr<Cons>((Tagged)tag_cons(cons));
  }

#ifdef USE_PRECISE_GC
  static smart_ptr<Cons> snapshot_save_load_allocate(Header_s::BadgeStampWtagMtag& the_header, core::T_sp car, core::T_sp cdr) {
    ConsHeader_s* header = do_cons_allocation<SnapshotLoadStage, Cons>(AlignUp(SizeofConsHeader() + sizeof(Cons)));
    header->_badge_stamp_wtag_mtag._header_badge.store(the_header._header_badge.load());
    header->_badge_stamp_wtag_mtag._value = the_header._value;
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
    Header_s* base = do_general_allocation<Stage>(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = smart_ptr<value_type>(ptr);
    return sp;
  };

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init) {
    size_t sizeWithHeader = sizeof(Header_s) + (snapshot_save_load_init->_clientEnd - snapshot_save_load_init->_clientStart);
    DO_DRAG_GENERAL_ALLOCATION();
    Header_s* base = do_general_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, sizeWithHeader);
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
    // Atomic objects (do not contain pointers) are allocated in separate pool
    Header_s* base = do_atomic_allocation<Stage>(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    return sp;
  };
  static void deallocate(OT* memory){
      // Nothing needs to be done but this function needs to be here
      // so that the static analyzer has something to call
  };

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init,
                                                        size_t size) {
    Header_s* base =
        do_atomic_allocation<SnapshotLoadStage>(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, size);
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
    Header_s* base = do_general_allocation<Stage>(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    return sp;
  };
  static void deallocate(OT* memory){
      // Nothing needs to be done but this function needs to be here
      // so that the static analyzer has something to call
  };

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init,
                                                        size_t size) {
    DO_DRAG_GENERAL_ALLOCATION();
    Header_s* base = do_general_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, size);
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
    Header_s* base = do_uncollectable_allocation(the_header, size);
    OT* obj = HeaderPtrToGeneralPtr<OT>(base);
    new (obj) OT(std::forward<ARGS>(args)...);
    gctools::smart_ptr<OT> sp(obj);
    return sp;
  }

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init,
                                                        size_t size) {
    Header_s* base = do_uncollectable_allocation(snapshot_save_load_init->_headStart->_badge_stamp_wtag_mtag, size);
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
  };

  static void deallocate(OT* memory) {
    do_free(memory);
  };
};
} // namespace gctools

extern "C" {
void my_mps_finalize(core::T_O* tagged);
};

namespace gctools {
template <class OT>
static void finalizeIfNeeded(smart_ptr<OT> sp) {
  if constexpr(GCInfo<OT>::NeedsFinalization)
    do_register_destructor_finalizer<OT>(SmartPtrToBasePtr(sp));
}
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
    Header_s* base = do_uncollectable_allocation(the_header, size);
    pointer_type ptr = HeaderPtrToGeneralPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    initializeIfNeeded(sp);
    finalizeIfNeeded(sp);
    return sp;
  };

  template <typename Stage, typename... ARGS>
  static smart_pointer_type allocate_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size, ARGS&&... args) {
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::template allocate_in_appropriate_pool_kind<Stage>(
            the_header, size, std::forward<ARGS>(args)...);
    initializeIfNeeded(sp);
    finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    return sp;
  };

  static smart_pointer_type snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* snapshot_save_load_init) {
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::snapshot_save_load_allocate(snapshot_save_load_init);
    // No initializer
    finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    return sp;
  };

  template <typename... ARGS>
  static smart_pointer_type static_allocate_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size, ARGS&&... args) {
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, unmanaged>::template allocate_in_appropriate_pool_kind<gctools::RuntimeStage>(
            the_header, size, std::forward<ARGS>(args)...);
    initializeIfNeeded(sp);
    finalizeIfNeeded(sp);
    return sp;
  };

  static smart_pointer_type copy_kind(const Header_s::BadgeStampWtagMtag& the_header, size_t size, const OT& that) {
    // Copied objects must be allocated in the appropriate pool
    smart_pointer_type sp =
        GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::template allocate_in_appropriate_pool_kind<gctools::RuntimeStage>(
            the_header, size, that);
    // Copied objects are not initialized.
    // Copied objects are finalized if necessary
    finalizeIfNeeded(sp);
    return sp;
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
    if constexpr(std::is_default_constructible_v<OT>) {
      auto kind = Header_s::StampWtagMtag::make_StampWtagMtag(OT::static_ValueStampWtagMtag);
      return GCObjectAllocator<OT>::template allocate_kind<gctools::RuntimeStage>(kind, sizeof_with_header<OT>());
    } else
      lisp_errorCannotAllocateInstanceWithMissingDefaultConstructor(OT::static_classSymbol());
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
template <class container_type>
class GCContainerAllocator /* : public GCAlloc<container_type> */ {
public:
  // type definitions
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
    return allocate_kind(Header_s::BadgeStampWtagMtag::make<container_type>(), num);
  }

  // allocate but don't initialize num elements of type value_type
  gc::tagged_pointer<container_type> allocate_kind(const Header_s::BadgeStampWtagMtag& the_header, size_type num, const void* = 0) {
    DO_DRAG_GENERAL_ALLOCATION();
    size_t size = sizeof_container_with_header<container_type>(num);
    Header_s* base = do_general_allocation(the_header, size);
    container_pointer myAddress = HeaderPtrToGeneralPtr<container_type>(base);
    return gctools::tagged_pointer<container_type>(myAddress);
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

struct WeakLinks {};
struct StrongLinks {};

template <class KT, class VT, class LT> struct Buckets;

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
  static constexpr bool weakp = std::is_same_v<StrongWeakLinkType, WeakLinks>;

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
    Header_s* base = do_weak_allocation<weakp>(the_header, size);
    container_pointer myAddress = (container_pointer)HeaderPtrToWeakPtr(base);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) container_type(num);
#ifdef DEBUG_GCWEAK
    printf("%s:%d Check if Buckets has been initialized to unbound\n", __FILE__, __LINE__);
#endif
    return gctools::tagged_pointer<container_type>(myAddress);
  }

  static gctools::tagged_pointer<container_type> snapshot_save_load_allocate(snapshotSaveLoad::snapshot_save_load_init_s* init) {
    size_t size = (init->_clientEnd - init->_clientStart) + SizeofWeakHeader();
    Header_s* base = do_weak_allocation<weakp>(init->_headStart->_badge_stamp_wtag_mtag, size);
#ifdef DEBUG_GUARD
    // Copy the source from the image save/load memory.
    base->_source = init->_headStart->_source;
#endif
    container_pointer myAddress = (container_pointer)HeaderPtrToWeakPtr(base);
    new (myAddress) container_type(init);
    return gctools::tagged_pointer<container_type>(myAddress);
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
    Header_s* base = do_weak_allocation<std::is_same_v<StrongWeakLinkType, WeakLinks>>(the_header, size);
    container_pointer myAddress = (container_pointer)HeaderPtrToWeakPtr(base);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) container_type(val);
    printf("%s:%d Check if Mapping has been initialized to unbound\n", __FILE__, __LINE__);
    return gctools::tagged_pointer<container_type>(myAddress);
  }
};
}; // namespace gctools

#endif // USE_BOEHM || USE_MPS || USE_MMTK
