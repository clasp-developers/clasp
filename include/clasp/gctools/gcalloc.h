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
#ifndef gc_gcalloc_H
#define gc_gcalloc_H

#define BOEHM_ONE_BIG_STACK 1
//#define DEBUG_BOEHM_STACK 1

#include <limits>

#define STACK_ALIGNMENT alignof(char *)
#define STACK_ALIGN_UP(size) \
  (((size)+STACK_ALIGNMENT - 1) & ~(STACK_ALIGNMENT - 1))

namespace gctools {
extern uint64_t globalBytesAllocated;
};

namespace gctools {
template <class OT, bool Needed = true>
struct GCObjectInitializer {};

template <class OT>
struct GCObjectInitializer<OT, true> {
  typedef smart_ptr<OT> smart_pointer_type;
  static void initializeIfNeeded(smart_pointer_type sp) {
    if ( sp.generalp() ) {
      sp.unsafe_general()->initialize();
    }
  };
};

template <class OT>
struct GCObjectInitializer<OT, false> {
  typedef smart_ptr<OT> smart_pointer_type;
  static void initializeIfNeeded(smart_pointer_type sp){
      // initialize not needed
  };
};

template <class OT>
struct GCObjectInitializer<tagged_pointer<OT>, true> {
  typedef tagged_pointer<OT> functor_pointer_type;
  static void initializeIfNeeded(functor_pointer_type sp) {
    THROW_HARD_ERROR(BF("Figure out why this is being invoked, you should never need to initialize a functor!"));
  };
};
template <class OT>
struct GCObjectInitializer<tagged_pointer<OT>, false> {
  typedef tagged_pointer<OT> functor_pointer_type;
  static void initializeIfNeeded(functor_pointer_type sp){
      // initialize not needed
  };
};
}

#if defined(USE_BOEHM) || defined(USE_MPS)

#if defined USE_BOEHM
namespace gctools {
template <class T>
class root_allocator : public traceable_allocator<T> {};
};
#endif


namespace gctools {

#ifdef USE_MPS
  template <typename Cons, typename... ARGS>
    inline smart_ptr<Cons> cons_mps_allocation(mps_ap_t& allocation_point,
                                        const char* ap_name,
                                        size_t& globalMpsMetrics_countAllocations,
                                        ARGS &&... args) {
    mps_addr_t addr;
    gc::smart_ptr<Cons> tagged_obj;
    Cons* cons;
    do {
      mps_res_t res = mps_reserve(&addr, allocation_point, sizeof(Cons));
      if ( res != MPS_RES_OK ) {
        printf("%s:%d Bad mps_reserve\n", __FILE__, __LINE__ );
      }
      cons = reinterpret_cast<Cons*>(addr);
      new (cons) Cons(std::forward<ARGS>(args)...);
      tagged_obj = smart_ptr<Cons>((Tagged)tag_cons(cons));
    } while (!mps_commit(allocation_point, addr, sizeof(Cons)));
    GC_TELEMETRY4(telemetry::label_cons_allocation,
                  (uintptr_t)addr,
                  (uintptr_t)cons,
                  (uintptr_t)((char*)addr+sizeof(Cons)),
                  KIND_CONS);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    POLL_SIGNALS();
    globalMpsMetrics.totalMemoryAllocated += sizeof(Cons);
    ++globalMpsMetrics_countAllocations;
    return tagged_obj;
  };

  template <class PTR_TYPE, typename... ARGS>
    inline PTR_TYPE do_mps_allocation(kind_t the_kind,
                                      size_t size,
                                      mps_ap_t& allocation_point,
                                      const char* ap_name,
                                      size_t& globalMpsMetrics_countAllocations,
                                      ARGS &&... args) {
    mps_addr_t addr;
    typedef typename PTR_TYPE::Type T;
    typedef typename GCHeader<T>::HeaderType HeadT;
    GCTOOLS_ASSERT(the_kind>=kind_first_general);
    PTR_TYPE tagged_obj;
    T* obj;
    size_t true_size = size;
#ifdef DEBUG_GUARD
    size_t tail_size = ((rand()%8)+1)*Alignment();
    true_size += tail_size;
#endif
    do {
      mps_res_t res = mps_reserve(&addr, allocation_point, true_size);
      if ( res != MPS_RES_OK ) {
        printf("%s:%d Bad mps_reserve\n", __FILE__, __LINE__ );
      }
      HeadT *header = reinterpret_cast<HeadT *>(addr);
#ifdef DEBUG_GUARD
      memset(header,0x00,true_size);
      new (header) HeadT(the_kind,size,tail_size);
#else
      new (header) HeadT(the_kind);
#endif
      obj = BasePtrToMostDerivedPtr<typename PTR_TYPE::Type>(addr);
      new (obj) (typename PTR_TYPE::Type)(std::forward<ARGS>(args)...);
      tagged_obj = PTR_TYPE(obj);
    } while (!mps_commit(allocation_point, addr, true_size));
    GC_TELEMETRY4(telemetry::label_allocation,
                  (uintptr_t)addr,
                  (uintptr_t)obj,
                  (uintptr_t)((char*)addr+true_size),
                  the_kind);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    POLL_SIGNALS();
    globalMpsMetrics.totalMemoryAllocated += true_size;
    ++globalMpsMetrics_countAllocations;
    return tagged_obj;
  };



  template <class PTR_TYPE, typename... ARGS>
    inline PTR_TYPE do_mps_weak_allocation(size_t size,
                                           mps_ap_t& allocation_point,
                                           const char* ap_name,
                                           ARGS &&... args) {
    typedef typename PTR_TYPE::Type T;
    typedef typename GCHeader<T>::HeaderType HeadT;
    PTR_TYPE tagged_obj;
    mps_addr_t addr;
    T* myAddress;
    do {
      mps_res_t res = mps_reserve(&addr, allocation_point, size);
      if (res != MPS_RES_OK)
        THROW_HARD_ERROR(BF("Out of memory"));
      GC_LOG(("allocated @%p %zu bytes\n", addr, size));
      myAddress = reinterpret_cast<T*>(addr);
      if (!myAddress)
        THROW_HARD_ERROR(BF("NULL address in allocate!"));
      new (myAddress) T(std::forward<ARGS>(args)...);
      tagged_obj = PTR_TYPE(myAddress);
    } while (!mps_commit(allocation_point, addr, size));
    GC_TELEMETRY4(telemetry::label_allocation,
                  (uintptr_t)addr,
                  (uintptr_t)myAddress,
                  (uintptr_t)((char*)addr+size),
                  KIND_null);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    if (!myAddress)
      THROW_HARD_ERROR(BF("Could not allocate from GCBucketAllocator<Buckets<VT,VT,WeakLinks>>"));
    GC_LOG(("malloc@%p %zu bytes\n", myAddress, size));
    return tagged_obj;
  }
#endif // #ifdef USE_MPS
    
/*! Allocate regular C++ classes that are considered roots */
  template <class T>
    struct RootClassAllocator {
      template <class... ARGS>
      static gctools::tagged_pointer<T> allocate( ARGS &&... args) {
        return allocate_kind(GCKind<T>::Kind,std::forward<ARGS>(args)...);
      };

      template <class... ARGS>
      static gctools::tagged_pointer<T> allocate_kind(kind_t the_kind, ARGS &&... args) {
#ifdef USE_BOEHM
        size_t sz = sizeof_with_header<T>();
        monitor_allocation(the_kind,sz);
        Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_UNCOLLECTABLE(sz));
        new (base) Header_s(the_kind);
        T *obj = BasePtrToMostDerivedPtr<T>(base);
        new (obj) T(std::forward<ARGS>(args)...);
        POLL_SIGNALS();
        gctools::tagged_pointer<T> tagged_obj(obj);
        return tagged_obj;
#endif
#ifdef USE_MPS
        size_t sz = sizeof_with_header<T>();
        monitor_allocation(the_kind,sz);
        tagged_pointer<T> tagged_obj =
          do_mps_allocation<tagged_pointer<T>>(the_kind,
                                               sz,
                                               global_non_moving_ap,
                                               "NON_MOVING_POOL",
                                               globalMpsMetrics.nonMovingAllocations,
                                               std::forward<ARGS>(args)...);
        return tagged_obj;
#endif
      }

      template <class... ARGS>
      static T *untagged_allocate(ARGS &&... args) {
        gctools::tagged_pointer<T> tagged_obj = allocate(args...);
        return &*tagged_obj;
      }

      static void deallocate(gctools::tagged_pointer<T> memory) {
#ifdef USE_BOEHM
        GC_FREE(&*memory);
#endif
#if defined(USE_MPS) && !defined(RUNNING_GC_BUILDER)
        THROW_HARD_ERROR(BF("I need a way to deallocate MPS allocated objects that are not moveable or collectable"));
        GCTOOLS_ASSERT(false); // ADD SOME WAY TO FREE THE MEMORY
#endif
      };

      static void untagged_deallocate(void *memory) {
#ifdef USE_BOEHM
        GC_FREE(memory);
#endif
#ifdef USE_MPS
        GCTOOLS_ASSERT(false); // ADD SOME WAY TO FREE THE MEMORY
#endif
      };
    };

  template <class Cons>
  struct ConsAllocator {
    template <class... ARGS>
    static smart_ptr<Cons> allocate(ARGS &&... args) {
#ifdef USE_BOEHM
      monitor_allocation(kind_cons,sizeof(Cons));
      Cons* cons = reinterpret_cast<Cons*>(GC_MALLOC(sizeof(Cons)));
      new (cons) Cons(std::forward<ARGS>(args)...);
      POLL_SIGNALS();
      return smart_ptr<Cons>((Tagged)tag_cons(cons));
#endif
#ifdef USE_MPS
        monitor_allocation(kind_cons,sizeof(Cons));
        mps_ap_t obj_ap = global_amc_cons_allocation_point;
        smart_ptr<Cons> obj =
          cons_mps_allocation<Cons>(obj_ap,"CONS",
                              globalMpsMetrics.consAllocations,
                              std::forward<ARGS>(args)...);
        return obj;
#endif
    }
  };
  
  template <class T>
    struct ClassAllocator {

      template <class... ARGS>
      static tagged_pointer<T> allocate_class(ARGS &&... args) {
        return ClassAllocator<T>::allocate_class_kind(GCKind<T>::Kind,std::forward<ARGS>(args)...);
      };
  /*! Allocate regular C++ classes that will be garbage collected as soon as nothing points to them */
      template <class... ARGS>
      static tagged_pointer<T> allocate_class_kind(kind_t the_kind, ARGS &&... args) {
#ifdef USE_BOEHM
        size_t sz = sizeof_with_header<T>();
        monitor_allocation(the_kind,sz);
        Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(sz));
        new (base) Header_s(the_kind);
        T *obj = BasePtrToMostDerivedPtr<T>(base);
        new (obj) T(std::forward<ARGS>(args)...);
        POLL_SIGNALS();
        return tagged_pointer<T>(obj);
#endif
#ifdef USE_MPS
        size_t sz = sizeof_with_header<T>();
        monitor_allocation(the_kind,sz);
        mps_ap_t obj_ap = GCAllocationPoint<T>::get();
        tagged_pointer<T> tagged_obj =
          do_mps_allocation<tagged_pointer<T>>(the_kind,sz,obj_ap,"AP",
                                               globalMpsMetrics.unknownAllocations,
                                               std::forward<ARGS>(args)...);
        return tagged_obj;
#endif
      }
    };
};

namespace gctools {

  template <class OT, GCInfo_policy Policy = normal>
    struct GCObjectAppropriatePoolAllocator {
      typedef OT value_type;
      typedef OT *pointer_type;
      typedef smart_ptr<OT> smart_pointer_type;
      template <typename... ARGS>
      static smart_pointer_type allocate_in_appropriate_pool_kind(kind_t the_kind, ARGS &&... args) {
#ifdef USE_BOEHM
        size_t size = sizeof_with_header<OT>();
        monitor_allocation(the_kind,size);
    // By default allocate in the normal pool for objects that contain pointers
    // to other objects.
        Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(size));
        new (base) Header_s(the_kind);
        pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
        new (ptr) OT(std::forward<ARGS>(args)...);
        smart_pointer_type sp = smart_ptr<value_type>(ptr);
        return sp;
#endif
#ifdef USE_MPS
        size_t size = sizeof_with_header<OT>();
        monitor_allocation(the_kind,size);
        mps_ap_t obj_ap = _global_automatic_mostly_copying_allocation_point;
        smart_ptr<OT> sp =
          do_mps_allocation<smart_ptr<OT>>(the_kind,size,obj_ap,"AMC",
                                           globalMpsMetrics.movingAllocations,
                                           std::forward<ARGS>(args)...);
        return sp;
#endif
      };
      static void deallocate(OT* memory) {
      // Nothing needs to be done but this function needs to be here
      // so that the static analyzer has something to call
      };
    };

  template <class OT>
    struct GCObjectAppropriatePoolAllocator<OT, /* Policy= */ atomic> {
    typedef OT value_type;
    typedef OT *pointer_type;
    typedef smart_ptr<OT> smart_pointer_type;
    template <typename... ARGS>
      static smart_pointer_type allocate_in_appropriate_pool_kind( kind_t the_kind, ARGS &&... args) {
#ifdef USE_BOEHM
      size_t size = sizeof_with_header<OT>();
      monitor_allocation(the_kind,size);
    // Atomic objects (do not contain pointers) are allocated in separate pool
      Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_ATOMIC(size));
      new (base) Header_s(the_kind);
      pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
      new (ptr) OT(std::forward<ARGS>(args)...);
      smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
      return sp;
#endif
#ifdef USE_MPS
      size_t size = sizeof_with_header<OT>();
      monitor_allocation(the_kind,size);
      mps_ap_t obj_ap = _global_automatic_mostly_copying_zero_rank_allocation_point;
      smart_pointer_type sp =
        do_mps_allocation<smart_pointer_type>(the_kind,size,obj_ap,"AMCZ",
                                              globalMpsMetrics.movingZeroRankAllocations,
                                              std::forward<ARGS>(args)...);
      return sp;
#endif
    };
    static void deallocate(OT* memory) {
      // Nothing needs to be done but this function needs to be here
      // so that the static analyzer has something to call
    };

  };

  /*! This Policy of collectible_immobile may not be a useful policy.
When would I ever want the GC to automatically collect objects but not move them?
*/
  template <class OT>
    struct GCObjectAppropriatePoolAllocator<OT,  /* Policy= */ collectable_immobile > {
    typedef OT value_type;
    typedef OT *pointer_type;
    typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
    template <typename... ARGS>
      static smart_pointer_type allocate_in_appropriate_pool_kind( kind_t the_kind, ARGS &&... args) {
#ifdef USE_BOEHM
      size_t size = sizeof_with_header<OT>();
      monitor_allocation(the_kind,size);
    // By default allocate in the normal pool for objects that contain pointers
    // to other objects.
      Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(size));
      new (base) Header_s(the_kind);
      pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
      new (ptr) OT(std::forward<ARGS>(args)...);
      smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
      return sp;
#endif
#ifdef USE_MPS
      size_t size = sizeof_with_header<OT>();
      monitor_allocation(the_kind,size);
      mps_ap_t obj_ap = global_non_moving_ap;
      smart_pointer_type sp =
        do_mps_allocation<smart_pointer_type>(the_kind,size,obj_ap,"NON_MOVING_POOL",
                                              globalMpsMetrics.nonMovingAllocations,
                                              std::forward<ARGS>(args)...);
      return sp;
#endif
    };
    static void deallocate(OT* memory) {
      // Nothing needs to be done but this function needs to be here
      // so that the static analyzer has something to call
    };

  };


  /*! This is for CL classes that derive from C++ classes and other CL classes that
should not be managed by the GC */
  template <class OT>
    struct GCObjectAppropriatePoolAllocator<OT, unmanaged > {
    typedef OT value_type;
    typedef OT *pointer_type;
    typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
    template <typename... ARGS>
      static smart_pointer_type allocate_in_appropriate_pool_kind( kind_t the_kind, ARGS &&... args) {
#ifdef USE_BOEHM
      size_t sz = sizeof_with_header<OT>();
      monitor_allocation(the_kind,sz);
      Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_UNCOLLECTABLE(sz));
      new (base) Header_s(the_kind);
      OT *obj = BasePtrToMostDerivedPtr<OT>(base);
      new (obj) OT(std::forward<ARGS>(args)...);
      POLL_SIGNALS();
      gctools::smart_ptr<OT> sp(obj);
      return sp;
#endif
#ifdef USE_MPS
      size_t sz = sizeof_with_header<OT>();
      monitor_allocation(the_kind, sz);
      mps_ap_t obj_ap = global_non_moving_ap;
      gctools::smart_ptr<OT> sp =
        do_mps_allocation<gctools::smart_ptr<OT>>(the_kind,sz,obj_ap,"NON_MOVING_POOL",
                                                  globalMpsMetrics.nonMovingAllocations,
                                                  std::forward<ARGS>(args)...);
      return sp;
#endif
    }

    static void deallocate(OT* memory) {
#ifdef USE_BOEHM
      printf("%s:%d Using GC_FREE to free memory at@%p\n", __FILE__, __LINE__, memory );
      GC_FREE(memory);
#endif
#if defined(USE_MPS) && !defined(RUNNING_GC_BUILDER)
    THROW_HARD_ERROR(BF(" GCObjectAppropriatePoolAllocator<OT, unmanaged > I need a way to deallocate MPS allocated objects that are not moveable or collectable"));
      GCTOOLS_ASSERT(false); // ADD SOME WAY TO FREE THE MEMORY
#endif
    };
  };
}

typedef void (*BoehmFinalizerFn)(void *obj, void *data);

namespace gctools {

#ifdef USE_BOEHM
template <class OT>
void BoehmFinalizer(void *base, void *data) {
  OT *obj = BasePtrToMostDerivedPtr<OT>(base);
  //        printf("%s:%d Finalizing ptr=%p\n", __FILE__, __LINE__, obj);
  obj->~OT();
}
#endif

template <class OT, bool Needed = true>
struct GCObjectFinalizer {
  typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
  static void finalizeIfNeeded(smart_pointer_type sp) {
#ifdef USE_BOEHM
    void *dummyData;
    BoehmFinalizerFn dummyFn;
    //            printf("%s:%d About to finalize sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
    GC_register_finalizer_ignore_self(SmartPtrToBasePtr(sp),
                                      BoehmFinalizer<OT>, NULL,
                                      &dummyFn, &dummyData);
//            printf("%s:%d Just completed finalize sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
#endif
#ifdef USE_MPS
    void *client = &*sp; // SmartPtrToBasePtr(sp);
    mps_finalize(_global_arena, &client);
    ++globalMpsMetrics.finalizationRequests;
#endif
  };
};

template <class OT>
struct GCObjectFinalizer<OT, false> {
  typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
  static void finalizeIfNeeded(smart_pointer_type sp){
      // finalize not needed
  };
};
}

namespace gctools {

template <class OT>
class GCObjectAllocator {
public:
  typedef OT value_type;
  typedef OT *pointer_type;
  typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
public:
  template <typename... ARGS>
    static smart_pointer_type root_allocate(ARGS &&... args) {
    return root_allocate_kind(GCKind<OT>::Kind,std::forward<ARGS>(args)...);
  }
  template <typename... ARGS>
    static smart_pointer_type root_allocate_kind(kind_t the_kind, ARGS &&... args) {
#ifdef USE_BOEHM
    size_t sz = sizeof_with_header<OT>(); // USE HEADER FOR BOEHM ROOTS BUT NOT MPS
    monitor_allocation(the_kind,sz);
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_UNCOLLECTABLE(sz));
    new (base) Header_s(the_kind);
    pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    GCObjectInitializer<OT, /*gctools::*/ GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#endif
#ifdef USE_MPS
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind(the_kind,std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    POLL_SIGNALS();
    return sp;
#endif
  };

  template <typename... ARGS>
    static smart_pointer_type allocate( ARGS &&... args) {
    return GCObjectAllocator<OT>::allocate_kind(GCKind<OT>::Kind,std::forward<ARGS>(args)...);
  }

  template <typename... ARGS>
    static smart_pointer_type allocate_kind(kind_t the_kind, ARGS &&... args) {
#ifdef USE_BOEHM
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind(the_kind,std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, /*gctools::*/ GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    POLL_SIGNALS();
    return sp;
#endif
#ifdef USE_MPS
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind( the_kind, std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    POLL_SIGNALS();
    return sp;
#endif
  };

    static smart_pointer_type copy(const OT &that) {
      return copy_kind(GCKind<OT>::Kind,that);
    }

  static smart_pointer_type copy_kind(kind_t the_kind, const OT &that) {
#ifdef USE_BOEHM
    // Copied objects must be allocated in the appropriate pool
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind( the_kind, that);
    // Copied objects are not initialized.
    // Copied objects are finalized if necessary
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#endif
#ifdef USE_MPS
    // Copied objects must be allocated in the appropriate pool
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind( the_kind, that);
    // Copied objects are not initialized.
    // Copied objects are finalized if necessary
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#endif
  }

   static void deallocate_unmanaged_instance(OT* obj) {
     GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::deallocate(obj);
  }
};
};

namespace gctools {
template <class TY>
class GCContainerAllocator /* : public GCAlloc<TY> */ {
public:
  // type definitions
  typedef TY container_type;
  typedef container_type *container_pointer;
  typedef typename container_type::value_type value_type;
  typedef value_type *pointer;
  typedef const value_type *const_pointer;
  typedef value_type &reference;
  typedef const value_type &const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
  GCContainerAllocator() throw() {}
  GCContainerAllocator(const GCContainerAllocator &) throw() {}
  template <class U>
  GCContainerAllocator(const GCContainerAllocator<U> &) throw() {}
  ~GCContainerAllocator() throw() {}

  // return maximum number of elements that can be allocated
  size_type max_size() const throw() {
    return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
  }

    // allocate but don't initialize num elements of type value_type
  gc::tagged_pointer<container_type> allocate(size_type num, const void * = 0) {
    return allocate_kind(GCKind<TY>::Kind,num);
  }

  // allocate but don't initialize num elements of type value_type
  gc::tagged_pointer<container_type> allocate_kind(kind_t the_kind, size_type num, const void * = 0) {
#ifdef USE_BOEHM
    size_t size = sizeof_container_with_header<TY>(num);
    monitor_allocation(the_kind,size);
    // prepend a one pointer header with a pointer to the typeinfo.name
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(size));
    if (!base)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (base) Header_s(the_kind);
    container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
    POLL_SIGNALS();
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    size_t size = sizeof_container_with_header<TY>(num);
    monitor_allocation(the_kind,size);
    mps_ap_t obj_ap = _global_automatic_mostly_copying_allocation_point;
    gc::tagged_pointer<container_type> obj =
      do_mps_allocation<gc::tagged_pointer<container_type>>(the_kind,size,obj_ap,"containerAMC",
                                                            globalMpsMetrics.movingAllocations,
                                                            num);
    return obj;
#endif
  }

  // initialize elements of allocated storage p with value value
  template <typename... ARGS>
  void construct(pointer p, ARGS &&... args) {
    // initialize memory with placement new
    new ((void *)p) value_type(std::forward<ARGS>(args)...);
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
};

namespace gctools {
/*! This allocator is for allocating containers that are fixed in position and Capacity.
      Things like the MultipleValues for multiple value return are allocated with this.
      */

template <class TY>
class GCContainerNonMoveableAllocator /* : public GCAlloc<TY> */ {
public:
  // type definitions
  typedef TY container_type;
  typedef container_type *container_pointer;
  typedef typename container_type::value_type value_type;
  typedef value_type *pointer;
  typedef const value_type *const_pointer;
  typedef value_type &reference;
  typedef const value_type &const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
  GCContainerNonMoveableAllocator() throw() {}
  GCContainerNonMoveableAllocator(const GCContainerNonMoveableAllocator &) throw() {}
  template <class U>
  GCContainerNonMoveableAllocator(const GCContainerNonMoveableAllocator<U> &) throw() {}
  ~GCContainerNonMoveableAllocator() throw() {}

  // return maximum number of elements that can be allocated
  size_type max_size() const throw() {
    return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
  }

  // allocate but don't initialize num elements of type value_type
  gctools::tagged_pointer<container_type> allocate_kind( kind_t the_kind, size_type num, const void * = 0) {
#ifdef USE_BOEHM
    size_t size = sizeof_container_with_header<TY>(num);
    monitor_allocation(the_kind,size);
    // prepend a one pointer header with a pointer to the typeinfo.name
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(size));
    if (!base)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (base) Header_s(the_kind);
    container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
    POLL_SIGNALS();
    return myAddress;
#endif
#ifdef USE_MPS
    size_t size = sizeof_container_with_header<TY>(num);
    monitor_allocation(the_kind,size);
    mps_ap_t obj_ap = global_non_moving_ap;
    gctools::tagged_pointer<container_type> obj =
      do_mps_allocation<gc::tagged_pointer<container_type>>(the_kind,size,obj_ap,"container_non_moving_ap",
                                                            globalMpsMetrics.nonMovingAllocations,
                                                            num);
    return obj;
#endif
  }

  // initialize elements of allocated storage p with value value
  template <typename... ARGS>
  void construct(pointer p, ARGS &&... args) {
    // initialize memory with placement new
    new ((void *)p) value_type(std::forward<ARGS>(args)...);
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
};

namespace gctools {
template <class TY>
class GCStringAllocator /* : public GCAlloc<TY> */ {
public:
  // type definitions
  typedef TY container_type;
  typedef container_type *container_pointer;
  typedef typename container_type::value_type value_type;
  typedef value_type *pointer;
  typedef const value_type *const_pointer;
  typedef value_type &reference;
  typedef const value_type &const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
  GCStringAllocator() throw() {}
  GCStringAllocator(const GCStringAllocator &) throw() {}
  template <class U>
  GCStringAllocator(const GCStringAllocator<U> &) throw() {}
  ~GCStringAllocator() throw() {}

  // return maximum number of elements that can be allocated
  size_type max_size() const throw() {
    return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
  }

  // allocate but don't initialize num elements of type value_type
  gctools::tagged_pointer<container_type> allocate_kind( kind_t the_kind, size_type num, const void * = 0) {
#if defined(USE_BOEHM)
    size_t sz = sizeof_container_with_header<container_type>(num);
    monitor_allocation(the_kind,sz);
    // prepend a one pointer header with a pointer to the typeinfo.name
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_ATOMIC(sz));
    if (!base)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (base) Header_s(the_kind);
    container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
    new (myAddress) TY(num);
    POLL_SIGNALS();
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#if defined(USE_MPS)
    size_t sz = sizeof_container_with_header<container_type>(num);
    monitor_allocation(the_kind,sz);
    gctools::tagged_pointer<container_type> obj =
      do_mps_allocation<gctools::tagged_pointer<container_type>>(the_kind,
                                                                 sz,
                                                                 _global_automatic_mostly_copying_zero_rank_allocation_point,
                                                                 "string_AMCZ",
                                                                 globalMpsMetrics.movingZeroRankAllocations,
                                                                 num);
    return obj;
#endif
  }

  void deallocate(gctools::tagged_pointer<container_type> p, size_type num) {
    // Do nothing
  }
};
};

namespace gctools {

#ifdef USE_BOEHM
inline void BoehmWeakLinkDebugFinalizer(void *base, void *data) {
  printf("%s:%d Boehm finalized weak linked address %p at %p\n", __FILE__, __LINE__, base, data);
}
#endif

struct WeakLinks {};
struct StrongLinks {};

template <class KT, class VT, class LT>
struct Buckets;

template <class TY>
class GCBucketAllocator /* : public GCAlloc<TY> */ {};

template <class VT>
class GCBucketAllocator<Buckets<VT, VT, WeakLinks>> {
public:
  typedef Buckets<VT, VT, WeakLinks> TY;
  typedef TY container_type;
  typedef container_type *container_pointer;
  typedef typename container_type::value_type value_type;
  typedef value_type *pointer;
  typedef const value_type *const_pointer;
  typedef value_type &reference;
  typedef const value_type &const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
  GCBucketAllocator() throw() {}
  GCBucketAllocator(const GCBucketAllocator &) throw() {}
  ~GCBucketAllocator() throw() {}

  // return maximum number of elements that can be allocated
  size_type max_size() const throw() {
    return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
  }

  // allocate but don't initialize num elements of type value_type
  static gctools::tagged_pointer<container_type> allocate( size_type num, const void * = 0) {
    size_t size = sizeof_container<container_type>(num); // NO HEADER FOR BUCKETS
    monitor_allocation(KIND_null,size);
#ifdef USE_BOEHM
#ifdef DEBUG_GCWEAK
    printf("%s:%d Allocating Bucket with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__);
#endif
    container_pointer myAddress = (container_pointer)GC_MALLOC_ATOMIC(size);
    if (!myAddress)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (myAddress) container_type(num);
#ifdef DEBUG_GCWEAK
    printf("%s:%d Check if Buckets has been initialized to unbound\n", __FILE__, __LINE__);
#endif
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    gctools::tagged_pointer<container_type> obj =
      do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(size,_global_weak_link_allocation_point,"weak_link_Bucket",num);
    return obj;
#endif
  }

  // initialize elements of allocated storage p with value value
  template <typename... ARGS>
  void construct(pointer p, ARGS &&... args) {
    // initialize memory with placement new
    THROW_HARD_ERROR(BF("What do I do here"));
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

//
// Specialize for strong links
//
template <class VT>
class GCBucketAllocator<Buckets<VT, VT, StrongLinks>> {
public:
  typedef Buckets<VT, VT, StrongLinks> TY;
  typedef TY container_type;
  typedef container_type *container_pointer;
  typedef typename container_type::value_type value_type;
  typedef value_type *pointer;
  typedef const value_type *const_pointer;
  typedef value_type &reference;
  typedef const value_type &const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
  GCBucketAllocator() throw() {}
  GCBucketAllocator(const GCBucketAllocator &) throw() {}
  ~GCBucketAllocator() throw() {}

  // return maximum number of elements that can be allocated
  size_type max_size() const throw() {
    return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
  }

  // allocate but don't initialize num elements of type value_type
  static gctools::tagged_pointer<container_type> allocate( size_type num, const void * = 0) {
    size_t size = sizeof_container<container_type>(num); // NO HEADER FOR BUCKETS
    monitor_allocation(KIND_null,size);
#ifdef USE_BOEHM
#ifdef DEBUG_GCWEAK
    printf("%s:%d Allocating Bucket with GC_MALLOC\n", __FILE__, __LINE__);
#endif
    container_pointer myAddress = (container_pointer)GC_MALLOC(size);
    if (!myAddress)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (myAddress) container_type(num);
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    gctools::tagged_pointer<container_type> obj =
      do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(size,_global_strong_link_allocation_point,"strong_link_Bucket",num);
    return obj;
#endif
  }

  // initialize elements of allocated storage p with value value
  template <typename... ARGS>
  void construct(pointer p, ARGS &&... args) {
    // initialize memory with placement new
    THROW_HARD_ERROR(BF("What do I do here"));
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

template <class KT, class VT, class LT>
struct Mapping;

template <class TY>
class GCMappingAllocator /* : public GCAlloc<TY> */ {};

template <class VT>
class GCMappingAllocator<Mapping<VT, VT, WeakLinks>> {
public:
  typedef Mapping<VT, VT, WeakLinks> TY;
  typedef TY container_type;
  typedef TY *container_pointer;
  /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
  GCMappingAllocator() throw() {}
  GCMappingAllocator(const GCMappingAllocator &) throw() {}
  ~GCMappingAllocator() throw() {}

  // allocate but don't initialize num elements of type value_type
  static gctools::tagged_pointer<container_type> allocate( const VT &val) {
    size_t size = sizeof(container_type);
    monitor_allocation(KIND_null,size);
#ifdef USE_BOEHM
    printf("%s:%d Allocating Mapping with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__);
    container_pointer myAddress = (container_pointer)GC_MALLOC_ATOMIC(size);
    if (!myAddress)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (myAddress) container_type(val);
    printf("%s:%d Check if Mapping has been initialized to unbound\n", __FILE__, __LINE__);
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    typedef typename GCHeader<TY>::HeaderType HeadT;
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    gctools::tagged_pointer<container_type> obj =
      do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(size,_global_weak_link_allocation_point,"weak_link_Allocator",val);
    return obj;
#endif
  }
};

template <class VT>
class GCMappingAllocator<Mapping<VT, VT, StrongLinks>> {
public:
  typedef Mapping<VT, VT, StrongLinks> TY;
  typedef TY container_type;
  typedef TY *container_pointer;
  /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
  GCMappingAllocator() throw() {}
  GCMappingAllocator(const GCMappingAllocator &) throw() {}
  ~GCMappingAllocator() throw() {}

  // allocate but don't initialize num elements of type value_type
  static gctools::tagged_pointer<container_type> allocate(const VT &val) {
    size_t size = sizeof(container_type);
    monitor_allocation(KIND_null,size);
#ifdef USE_BOEHM
    printf("%s:%d Allocating Mapping with GC_MALLOC\n", __FILE__, __LINE__);
    container_pointer myAddress = (container_pointer)GC_MALLOC(size);
    if (!myAddress)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (myAddress) container_type(val);
    printf("%s:%d Check if Mapping has been initialized to unbound\n", __FILE__, __LINE__);
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    typedef typename GCHeader<TY>::HeaderType HeadT;
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    gctools::tagged_pointer<container_type> obj =
      do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(size,_global_strong_link_allocation_point,"weak_link2_Allocator",val);
    return obj;
#endif
  }
};

template <class VT>
class GCWeakPointerAllocator {
public:
  typedef VT value_type;
  typedef value_type *value_pointer;
  typedef typename VT::value_type contained_type;
  /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
  GCWeakPointerAllocator() throw() {}
  GCWeakPointerAllocator(const GCWeakPointerAllocator &) throw() {}
  ~GCWeakPointerAllocator() throw() {}

  // allocate but don't initialize num elements of type value_type
  static gctools::tagged_pointer<value_type> allocate(const contained_type &val) {
    size_t size = sizeof(VT);
    monitor_allocation(KIND_null,size);
#ifdef USE_BOEHM
    printf("%s:%d Allocating WeakPointer with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__);
    value_pointer myAddress = (value_pointer)GC_MALLOC_ATOMIC(size);
    if (!myAddress)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (myAddress) VT(val);
    return gctools::tagged_pointer<value_type>(myAddress);
#endif
#ifdef USE_MPS
    mps_addr_t addr;
    value_pointer myAddress;
    gctools::tagged_pointer<value_type> obj =
      do_mps_weak_allocation<gctools::tagged_pointer<value_type>>(size,_global_weak_link_allocation_point,"weak_link3_Allocator",val);
    return obj;
#endif
  }
};
};


namespace gctools {

/*! Maintain a stack containing pointers that are garbage collected
*/
class GCStack {
public:
  typedef enum { undefined_t,
                 frame_t,
                 pad_t } frameType;
  size_t _MaxSize;
  size_t _TotalSize;
  size_t _TotalAllocations;
#ifdef USE_BOEHM
#ifdef BOEHM_ONE_BIG_STACK
  uintptr_t *_StackCur;
  uintptr_t *_StackBottom;
  size_t _StackMinOffset;
  size_t _StackMiddleOffset;
  uintptr_t *_StackLimit;
#else
// Nothing
#endif
#endif
#ifdef USE_MPS
  mps_pool_t _Pool;
  mps_ap_t _AllocationPoint;
  mps_fmt_t _ObjectFormat;
  bool _IsActive;
  vector<mps_frame_t> frames;
#endif
  //! Return true if this Stack object is active and can receive pushFrame/popFrame messages
public:
  size_t maxSize() const { return this->_MaxSize; };
  bool isActive() {
#ifdef USE_BOEHM
    return true;
#endif
#ifdef USE_MPS
    return _IsActive;
#endif
  };
#ifdef BOEHM_ONE_BIG_STACK
  void growStack();
  void shrinkStack();
#endif
  //*! Allocate a buffer for this
  bool allocateStack(size_t bufferSize) {
    bufferSize = STACK_ALIGN_UP(bufferSize);
#ifdef USE_BOEHM
#ifdef BOEHM_ONE_BIG_STACK
    this->_StackBottom = (uintptr_t *)GC_MALLOC(bufferSize);
    this->_StackMiddleOffset = (bufferSize / 2);
    this->_StackLimit = (uintptr_t *)((char *)this->_StackBottom + bufferSize);
    this->_StackMinOffset = bufferSize;
    this->_StackCur = this->_StackBottom;
    memset(this->_StackBottom, 0, bufferSize);
#else
// Do nothing
#endif
#endif
#ifdef USE_MPS
    mpsAllocateStack(this);
#endif
    return true;
  };
  void deallocateStack() {
#ifdef USE_BOEHM
#ifdef BOEHM_ONE_BIG_STACK
    if (this->_StackCur != this->_StackBottom) {
      THROW_HARD_ERROR(BF("The stack is not empty"));
    }
    GC_FREE(this->_StackBottom);
#else
// Do nothing
#endif
#endif
#ifdef USE_MPS
    mpsDeallocateStack(this);
#endif
  };

  size_t totalSize() const {
    return this->_TotalSize;
  }

  size_t totalAllocations() const {
    return this->_TotalAllocations;
  }

#define FRAME_HEADER_SIZE (sizeof(int) * 2)
#define FRAME_HEADER_TYPE_FIELD(hptr) *(((int *)hptr))
#define FRAME_HEADER_SIZE_FIELD(hptr) *(((int *)hptr) + 1)
#define FRAME_START(hptr) (uintptr_t *)(((char *)hptr) + FRAME_HEADER_SIZE)
#define FRAME_HEADER(fptr) (uintptr_t *)(((char *)fptr) - FRAME_HEADER_SIZE)
  void *frameImplHeaderAddress(void *frameImpl) {
    return (void *)((char *)frameImpl - FRAME_HEADER_SIZE);
  }
  GCStack::frameType frameImplHeaderType(void *frameImpl) {
    void *frameImplHeader = (void *)((char *)frameImpl - FRAME_HEADER_SIZE);
    return (GCStack::frameType)(FRAME_HEADER_TYPE_FIELD(frameImplHeader));
  }
  int frameImplHeaderSize(void *frameImpl) {
    void *frameImplHeader = (void *)((char *)frameImpl - FRAME_HEADER_SIZE);
    return (int)(FRAME_HEADER_SIZE_FIELD(frameImplHeader));
  }

  int frameImplBodySize(void *frameImpl) {
    void *frameImplHeader = (void *)((char *)frameImpl - FRAME_HEADER_SIZE);
    return (int)(FRAME_HEADER_SIZE_FIELD(frameImplHeader) - FRAME_HEADER_SIZE);
  }

  void *pushFrameImpl(size_t frameSize);
  void popFrameImpl(void *frameImpl) {
#ifdef USE_BOEHM
    uintptr_t *frameHeaderP = reinterpret_cast<uintptr_t *>(frameImpl) - 1;
    uintptr_t headerAndFrameSize = FRAME_HEADER_SIZE_FIELD(frameHeaderP);
    this->_TotalSize = this->_TotalSize - headerAndFrameSize;
#ifdef BOEHM_ONE_BIG_STACK
    memset(frameHeaderP, 0, headerAndFrameSize);
    this->_StackCur = frameHeaderP;
    if (this->_StackMinOffset <= (this->_StackLimit - this->_StackBottom) && (this->_StackCur - this->_StackBottom) < this->_StackMiddleOffset)
      this->shrinkStack();
#ifdef DEBUG_BOEHM_STACK
    size_t calcSize = (char *)this->_StackTop - (char *)this->_StackBottom;
    if (calcSize != this->_TotalSize) {
      THROW_HARD_ERROR(BF("The side-stack has gotten out of whack!  this->_TotalSize = %u  calcSize = %u\n") % this->_TotalSize % calcSize);
    }
    for (char *i = (char *)this->_StackTop; i < (char *)this->_StackLimit; ++i) {
      if (*i) {
        THROW_HARD_ERROR(BF("The side-stack has garbage in it!"));
      }
    }
#endif
#else
    GC_FREE(frameHeaderP);
#endif
#endif // USE_BOEHM
#ifdef USE_MPS
    uintptr_t *frameHeaderP = FRAME_HEADER(frameImpl);
    uintptr_t headerAndFrameSize = FRAME_HEADER_SIZE_FIELD(frameHeaderP);
    this->_TotalSize -= headerAndFrameSize;
    mps_frame_t frame_o = this->frames.back();
    this->frames.pop_back();
    STACK_TELEMETRY2(telemetry::label_stack_pop, this->_AllocationPoint, frame_o);
    mps_res_t res = mps_ap_frame_pop(this->_AllocationPoint, frame_o);
    if (res != MPS_RES_OK) {
      THROW_HARD_ERROR(BF("There was a problem with mps_app_frame_pop result = %d") % res);
    }
#endif // USE_MPS
  }

 GCStack() : _TotalSize(0), _TotalAllocations(0), _MaxSize(0)
#ifdef USE_BOEHM
#endif
#ifdef USE_MPS
// What do I do here?
#endif
              {};
  virtual ~GCStack(){
#ifdef USE_BOEHM
// Nothing to do
#endif
#ifdef USE_MPS
// What do I do here?
#endif
  };
};
};

#endif // USE_BOEHM || USE_MPS

#endif
