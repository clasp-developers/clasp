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

#ifdef TAGGED_POINTER
template <class OT>
struct GCObjectInitializer<tagged_pointer<OT>, true> {
  typedef tagged_pointer<OT> functor_pointer_type;
  static void initializeIfNeeded(functor_pointer_type sp) {
    throw_hard_error("Figure out why this is being invoked, you should never need to initialize a functor!");
  };
};
template <class OT>
struct GCObjectInitializer<tagged_pointer<OT>, false> {
  typedef tagged_pointer<OT> functor_pointer_type;
  static void initializeIfNeeded(functor_pointer_type sp){
      // initialize not needed
  };
};
#endif // end TAGGED_POINTER
}

#if defined(USE_BOEHM) || defined(USE_MPS)

#if defined USE_BOEHM
namespace gctools {
template <class T>
class root_allocator : public traceable_allocator<T> {};
};
#endif


namespace gctools {
#ifdef USE_BOEHM
  inline Header_s* do_boehm_atomic_allocation(const Header_s::Value& the_header, size_t size) 
  {
    RAII_DISABLE_INTERRUPTS();
    size_t true_size = size;
#ifdef DEBUG_GUARD
    size_t tail_size = ((rand()%8)+1)*Alignment();
    true_size += tail_size;
#endif
    Header_s* header = reinterpret_cast<Header_s*>(GC_MALLOC_ATOMIC(true_size));
    global_AllocationProfiler.registerAllocation(true_size);
#ifdef DEBUG_GUARD
    memset(header,0x00,true_size);
    new (header) Header_s(the_header,size,tail_size,true_size);
#else
    new (header) Header_s(the_header);
#endif
    return header;
  };
  inline Header_s* do_boehm_normal_allocation(const Header_s::Value& the_header, size_t size) 
  {
    RAII_DISABLE_INTERRUPTS();
    size_t true_size = size;
#ifdef DEBUG_GUARD
    size_t tail_size = ((rand()%8)+1)*Alignment();
    true_size += tail_size;
#endif
    Header_s* header = reinterpret_cast<Header_s*>(GC_MALLOC(true_size));
    global_AllocationProfiler.registerAllocation(true_size);
#ifdef DEBUG_GUARD
    memset(header,0x00,true_size);
    new (header) Header_s(the_header,size,tail_size,true_size);
#else
    new (header) Header_s(the_header);
#endif
    return header;
  };
  inline Header_s* do_boehm_uncollectable_allocation(const Header_s::Value& the_header, size_t size) 
  {
    RAII_DISABLE_INTERRUPTS();
    size_t true_size = size;
#ifdef DEBUG_GUARD
    size_t tail_size = ((rand()%8)+1)*Alignment();
    true_size += tail_size;
#endif
    Header_s* header = reinterpret_cast<Header_s*>(GC_MALLOC_UNCOLLECTABLE(true_size));
    global_AllocationProfiler.registerAllocation(true_size);
#ifdef DEBUG_GUARD
    memset(header,0x00,true_size);
    new (header) Header_s(the_header,size,tail_size,true_size);
#else
    new (header) Header_s(the_header);
#endif
    return header;
  };
#endif
};


namespace gctools {
#ifdef USE_MPS
  template <typename Cons, typename... ARGS>
    inline smart_ptr<Cons> cons_mps_allocation(mps_ap_t& allocation_point,
                                        const char* ap_name,
                                        ARGS &&... args) {
    gc::smart_ptr<Cons> tagged_obj;
    DO_DEBUG_RECURSIVE_ALLOCATIONS();
    { RAII_DISABLE_INTERRUPTS();
      mps_addr_t addr;
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
      global_AllocationProfiler.registerAllocation(sizeof(Cons));
    }
    DEBUG_MPS_UNDERSCANNING_TESTS();
    lisp_check_pending_interrupts(my_thread);
    globalMpsMetrics.totalMemoryAllocated += sizeof(Cons);
    ++globalMpsMetrics.consAllocations;
    return tagged_obj;
  };

  template <class PTR_TYPE, typename... ARGS>
    inline PTR_TYPE do_mps_allocation_(const Header_s::Value& the_header,
                                      size_t size,
                                      mps_ap_t& allocation_point,
                                      const char* ap_name,
                                      ARGS &&... args) {
    mps_addr_t addr;
    typedef typename PTR_TYPE::Type T;
    typedef typename GCHeader<T>::HeaderType HeadT;
//    GCTOOLS_ASSERT(the_header.stamp()>=stamp_first_general);
    DO_DEBUG_RECURSIVE_ALLOCATIONS();
    GCTOOLS_ASSERT(the_header.stamp()!=STAMP_core__General_O);
    // BF("The kind value[%d] must be > %d - if the type being allocated is a templated type then it should have the same kind as its TemplateBase ... eg:\n"\
      // "template <typename T>\n"\
           // "class gctools::GCStamp<clbind::Wrapper<T, T *>> {\n"\
    // "public:\n"\
         // "  static gctools::GCStampEnum const Kind = gctools::GCStamp<typename clbind::Wrapper<T, T *>::TemplatedBase>::Stamp;\n"\
              // "};") % the_header % stamp_first_general );
    PTR_TYPE tagged_obj;
    T* obj;
    size_t true_size = size;
#ifdef DEBUG_GUARD
    size_t tail_size = ((rand()%8)+1)*Alignment();
    true_size += tail_size;
#endif
    HeadT *header;
    { RAII_DISABLE_INTERRUPTS(); 
      do {
        mps_res_t res = mps_reserve(&addr, allocation_point, true_size);
        if ( res != MPS_RES_OK ) {
          printf("%s:%d Bad mps_reserve\n", __FILE__, __LINE__ );
        }
        header = reinterpret_cast<HeadT *>(addr);
#ifdef DEBUG_GUARD
        memset(header,0x00,true_size);
        new (header) HeadT(the_header,size,tail_size, true_size);
#else
        new (header) HeadT(the_header);
#endif
        obj = BasePtrToMostDerivedPtr<typename PTR_TYPE::Type>(addr);
        new (obj) (typename PTR_TYPE::Type)(std::forward<ARGS>(args)...);
        tagged_obj = PTR_TYPE(obj);
      } while (!mps_commit(allocation_point, addr, true_size));
      global_AllocationProfiler.registerAllocation(true_size);
    }
#ifdef DEBUG_VALIDATE_GUARD
    header->validate();
#endif
    DEBUG_MPS_UNDERSCANNING_TESTS();
    lisp_check_pending_interrupts(my_thread);
    globalMpsMetrics.totalMemoryAllocated += true_size;
#ifdef DEBUG_MPS_SIZE
    {
      mps_addr_t nextClient = obj_skip((mps_addr_t)obj);
      int skip_size = (int)((char*)nextClient-(char*)obj);
      if (skip_size != true_size) {
        printf("%s:%d Bad size calc obj_skip(stamp %u) - true_size -> %lu  obj_skip -> %d delta -> %d\n",
               __FILE__, __LINE__, header->stamp(), true_size, skip_size, ((int)true_size-(int)skip_size) );
        mps_addr_t againNextClient = obj_skip((mps_addr_t)obj);
#ifdef DEBUG_GUARD
        printf("      header-size= %lu size= %zu tail_size=%lu \n", sizeof(HeadT), size, tail_size );
#else        
        printf("      header-size= %lu size= %zu\n", sizeof(HeadT), size );
#endif
      }
    }
#endif
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
    DO_DEBUG_RECURSIVE_ALLOCATIONS();
    { RAII_DISABLE_INTERRUPTS();
      do {
        mps_res_t res = mps_reserve(&addr, allocation_point, size);
        if (res != MPS_RES_OK)
          throw_hard_error("Out of memory");
        GC_LOG(("allocated @%p %zu bytes\n", addr, size));
        myAddress = reinterpret_cast<T*>(addr);
        if (!myAddress)
          throw_hard_error("NULL address in allocate!");
        new (myAddress) T(std::forward<ARGS>(args)...);
        tagged_obj = PTR_TYPE(myAddress);
      } while (!mps_commit(allocation_point, addr, size));
      global_AllocationProfiler.registerAllocation(size);
    }
    lisp_check_pending_interrupts(my_thread);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    if (!myAddress)
      throw_hard_error("Could not allocate from GCBucketAllocator<Buckets<VT,VT,WeakLinks>>");
    GC_LOG(("malloc@%p %zu bytes\n", myAddress, size));
    return tagged_obj;
  }
#endif // #ifdef USE_MPS

#ifdef TAGGED_POINTER
/*! Allocate regular C++ classes that are considered roots */
  template <class T>
    struct RootClassAllocator {
      template <class... ARGS>
      static gctools::tagged_pointer<T> allocate( ARGS &&... args) {
        return allocate_kind(Header_s::Value::make<T>(),sizeof_with_header<T>(),std::forward<ARGS>(args)...);
      };

      template <class... ARGS>
      static gctools::tagged_pointer<T> allocate_kind(const Header_s::Value& the_header, size_t size, ARGS &&... args) {
#ifdef USE_BOEHM
        monitor_allocation(the_header.stamp(),size);
        Header_s* base = do_boehm_uncollectable_allocation(the_header,size);
        T *obj = BasePtrToMostDerivedPtr<T>(base);
        new (obj) T(std::forward<ARGS>(args)...);
        lisp_check_pending_interrupts(my_thread);
        gctools::tagged_pointer<T> tagged_obj(obj);
        return tagged_obj;
#endif
#ifdef USE_MPS
        monitor_allocation(the_header.stamp(),size);
        globalMpsMetrics.nonMovingAllocations++;
        tagged_pointer<T> tagged_obj =
          do_mps_allocation_<tagged_pointer<T>>(the_header,
                                               size,
                                               my_thread_allocation_points._non_moving_allocation_point,
                                               "NON_MOVING_POOL",
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
        throw_hard_error("I need a way to deallocate MPS allocated objects that are not moveable or collectable");
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
#endif // end TAGGED_POINTER
  
  template <class Cons>
  struct ConsAllocator {
    template <class... ARGS>
    static smart_ptr<Cons> allocate(ARGS &&... args) {
#ifdef USE_BOEHM
      Cons* cons;
      { RAII_DISABLE_INTERRUPTS();
        monitor_allocation(STAMP_CONS,sizeof(Cons));
        cons = reinterpret_cast<Cons*>(GC_MALLOC(sizeof(Cons)));
        global_AllocationProfiler.registerAllocation(sizeof(Cons));
        new (cons) Cons(std::forward<ARGS>(args)...);
      }
      lisp_check_pending_interrupts(my_thread);
      return smart_ptr<Cons>((Tagged)tag_cons(cons));
#endif
#ifdef USE_MPS
        monitor_allocation(STAMP_CONS,sizeof(Cons));
        mps_ap_t obj_ap = my_thread_allocation_points._amc_cons_allocation_point;
        globalMpsMetrics.consAllocations++;
        smart_ptr<Cons> obj =
          cons_mps_allocation<Cons>(obj_ap,"CONS",
                              std::forward<ARGS>(args)...);
        return obj;
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
      static smart_pointer_type allocate_in_appropriate_pool_kind(const Header_s::Value& the_header, size_t size, ARGS &&... args) {
#ifdef USE_BOEHM
        monitor_allocation(the_header.stamp(),size);
        Header_s* base = do_boehm_normal_allocation(the_header,size);
        pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
        new (ptr) OT(std::forward<ARGS>(args)...);
        smart_pointer_type sp = smart_ptr<value_type>(ptr);
        return sp;
#endif
#ifdef USE_MPS
        monitor_allocation(the_header.stamp(),size);
        mps_ap_t obj_ap = my_thread_allocation_points._automatic_mostly_copying_allocation_point;
        globalMpsMetrics.movingAllocations++;
        smart_ptr<OT> sp =
          do_mps_allocation_<smart_ptr<OT>>(the_header,size,obj_ap,"AMC",
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
      static smart_pointer_type allocate_in_appropriate_pool_kind( const Header_s::Value& the_header, size_t size, ARGS &&... args) {
#ifdef USE_BOEHM
      monitor_allocation(the_header.stamp(),size);
    // Atomic objects (do not contain pointers) are allocated in separate pool
      Header_s* base = do_boehm_atomic_allocation(the_header,size);
      pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
      new (ptr) OT(std::forward<ARGS>(args)...);
      smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
      return sp;
#endif
#ifdef USE_MPS
      monitor_allocation(the_header.stamp(),size);
      mps_ap_t obj_ap = my_thread_allocation_points._automatic_mostly_copying_zero_rank_allocation_point;
      globalMpsMetrics.movingZeroRankAllocations++;
      smart_pointer_type sp =
        do_mps_allocation_<smart_pointer_type>(the_header,size,obj_ap,"AMCZ",
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
      static smart_pointer_type allocate_in_appropriate_pool_kind( const Header_s::Value& the_header, size_t size, ARGS &&... args) {
#ifdef USE_BOEHM
      monitor_allocation(the_header.stamp(),size);
      Header_s* base = do_boehm_normal_allocation(the_header,size);
      pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
      new (ptr) OT(std::forward<ARGS>(args)...);
      smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
      return sp;
#endif
#ifdef USE_MPS
      monitor_allocation(the_header.stamp(),size);
      mps_ap_t obj_ap = my_thread_allocation_points._non_moving_allocation_point;
      globalMpsMetrics.nonMovingAllocations++;
      smart_pointer_type sp =
        do_mps_allocation_<smart_pointer_type>(the_header,size,obj_ap,"NON_MOVING_POOL",
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
      static smart_pointer_type allocate_in_appropriate_pool_kind( const Header_s::Value& the_header, size_t size, ARGS &&... args) {
#ifdef USE_BOEHM
      monitor_allocation(the_header.stamp(),size);
      Header_s* base = do_boehm_uncollectable_allocation(the_header,size);
      OT *obj = BasePtrToMostDerivedPtr<OT>(base);
      new (obj) OT(std::forward<ARGS>(args)...);
      lisp_check_pending_interrupts(my_thread);
      gctools::smart_ptr<OT> sp(obj);
      return sp;
#endif
#ifdef USE_MPS
      monitor_allocation(the_header.stamp(), size);
      mps_ap_t obj_ap = my_thread_allocation_points._non_moving_allocation_point;
      globalMpsMetrics.nonMovingAllocations++;
      gctools::smart_ptr<OT> sp =
        do_mps_allocation_<gctools::smart_ptr<OT>>(the_header,size,obj_ap,"NON_MOVING_POOL",
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
      throw_hard_error(" GCObjectAppropriatePoolAllocator<OT, unmanaged > I need a way to deallocate MPS allocated objects that are not moveable or collectable");
      GCTOOLS_ASSERT(false); // ADD SOME WAY TO FREE THE MEMORY
#endif
    };
  };
}

typedef void (*BoehmFinalizerFn)(void *obj, void *data);

extern "C" {
void my_mps_finalize(void* client);
};

namespace gctools {
  extern void boehm_general_finalizer(void* object, void* dummy);
  
#ifdef USE_BOEHM
template <class OT>
void BoehmFinalizer(void *base, void *data) {
  //        printf("%s:%d Finalizing ptr=%p\n", __FILE__, __LINE__, obj);
  boehm_general_finalizer(base,data);
  OT *obj = BasePtrToMostDerivedPtr<OT>(base);
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
//    printf("%s:%d About to finalize sp -> %p\n", __FILE__, __LINE__, (void*)sp.tagged_());
    GC_register_finalizer_no_order(SmartPtrToBasePtr(sp),
                                   BoehmFinalizer<OT>, NULL,
                                   &dummyFn, &dummyData);
//    printf("%s:%d Finished finalize sp -> %p\n", __FILE__, __LINE__, (void*)sp.tagged_());
#endif
#ifdef USE_MPS
    // Defined in mpsGarbageCollection.cc
    my_mps_finalize(&*sp);
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
      return root_allocate_kind(GCStamp<OT>::Stamp,sizeof_with_header<OT>(),std::forward<ARGS>(args)...);
    }
    template <typename... ARGS>
      static smart_pointer_type root_allocate_kind( const Header_s::Value& the_header, size_t size, ARGS &&... args) {
#ifdef USE_BOEHM
      monitor_allocation(the_header.stamp(),size);
      Header_s* base = do_boehm_uncollectable_allocation(the_header,size);
      pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
      new (ptr) OT(std::forward<ARGS>(args)...);
      smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
      GCObjectInitializer<OT, /*gctools::*/ GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
      GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
      return sp;
#endif
#ifdef USE_MPS
      smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind(the_header,size,std::forward<ARGS>(args)...);
      GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
      GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
      lisp_check_pending_interrupts(my_thread);
      return sp;
#endif
    };

    template <typename... ARGS>
      static smart_pointer_type allocate_kind(const Header_s::Value& the_header, size_t size, ARGS &&... args) {
#ifdef USE_BOEHM
      smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind(the_header,size,std::forward<ARGS>(args)...);
      GCObjectInitializer<OT, /*gctools::*/ GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
      GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
      lisp_check_pending_interrupts(my_thread);
      return sp;
#endif
#ifdef USE_MPS
      smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind(  the_header, size, std::forward<ARGS>(args)...);
      GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
      GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
      lisp_check_pending_interrupts(my_thread);
      return sp;
#endif
    };
    static smart_pointer_type register_class_with_redeye() {
      throw_hard_error("Never call this - it's only used to register with the redeye static analyzer");
    }
    static smart_pointer_type copy_kind(const Header_s::Value& the_header, size_t size, const OT &that) {
#ifdef USE_BOEHM
    // Copied objects must be allocated in the appropriate pool
      smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind(  the_header, size, that);
    // Copied objects are not initialized.
    // Copied objects are finalized if necessary
      GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
      return sp;
#endif
#ifdef USE_MPS
    // Copied objects must be allocated in the appropriate pool
      smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocate_in_appropriate_pool_kind(  the_header, size, that);
    // Copied objects are not initialized.
    // Copied objects are finalized if necessary
      GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
      return sp;
#endif
    }
  };
};


  
namespace gctools {
  template <class OT,bool Can>
  struct GCObjectDefaultConstructorAllocator {};

  template <class OT>
    struct GCObjectDefaultConstructorAllocator<OT,true> {
    static smart_ptr<OT> allocate(const Header_s::Value& kind) {
      // FIXSTAMP
      return GCObjectAllocator<OT>::allocate_kind(kind, sizeof_with_header<OT>());
    }
  };

  template <class OT>
    struct GCObjectDefaultConstructorAllocator<OT,false> {
    static smart_ptr<OT> allocate(const Header_s::Value& kind) {
      lisp_errorCannotAllocateInstanceWithMissingDefaultConstructor(OT::static_classSymbol());
      return _Nil<core::T_O>(); // it should never get here but shut up the compiler about return values
    }
  };
};

namespace gctools {
  /*! This is the public interface to the GCObjectAllocator */
  template <class OT>
    class GC {
  public:
    typedef OT value_type;
    typedef OT *pointer_type;
    typedef /*gctools::*/ smart_ptr<OT> smart_pointer_type;
  public:
    template <typename... ARGS>
      static smart_pointer_type root_allocate(ARGS &&... args) {
      return GCObjectAllocator<OT>::root_allocate_kind(Header_s::Value::make<OT>(),sizeof_with_header<OT>(),std::forward<ARGS>(args)...);
    }

    template <typename... ARGS>
      static smart_pointer_type root_allocate_with_stamp(ARGS &&... args) {
      return GCObjectAllocator<OT>::root_allocate_kind(Header_s::Value::make<OT>(),sizeof_with_header<OT>(),std::forward<ARGS>(args)...);
    }

    template <typename... ARGS>
      static smart_pointer_type never_invoke_allocator( ARGS &&... args) {
      auto kind = GCStamp<OT>::Stamp;
      return GCObjectAllocator<OT>::allocate_kind(kind,0, std::forward<ARGS>(args)...);
    }

    template <typename... ARGS>
      static smart_pointer_type allocate_kind( const Header_s::Value& kind, ARGS &&... args) {
      size_t size = sizeof_with_header<OT>();
      return GCObjectAllocator<OT>::allocate_kind(kind,size, std::forward<ARGS>(args)...);
    }

    template <typename... ARGS>
      static smart_pointer_type allocate_instance(const Header_s::Value& kind, size_t size, ARGS &&... args) {
      return GCObjectAllocator<OT>::allocate_kind(kind,size, std::forward<ARGS>(args)...);
    }

    template <typename... ARGS>
      static smart_pointer_type allocate( ARGS &&... args) {
      auto kind = OT::static_HeaderValue;
      size_t size = sizeof_with_header<OT>();
      return GCObjectAllocator<OT>::allocate_kind(kind,size, std::forward<ARGS>(args)...);
    }


    static smart_pointer_type allocate_with_default_constructor() {
      return GCObjectDefaultConstructorAllocator<OT,std::is_default_constructible<OT>::value>::allocate(OT::static_HeaderValue);
    }

    /*! Allocate enough space for capacity elements, but set the length to length */

    template <typename... ARGS>
      static smart_pointer_type allocate_container( size_t length, /*const typename OT::value_type& initial_element,*/ ARGS &&... args) {
      size_t capacity = length;
      size_t size = sizeof_container_with_header<OT>(capacity);
      return GCObjectAllocator<OT>::allocate_kind(OT::static_HeaderValue,size,length,/*initial_element,*/std::forward<ARGS>(args)...);
    }

        template <typename... ARGS>
      static smart_pointer_type allocate_container_null_terminated_string( size_t length, /*const typename OT::value_type& initial_element,*/ ARGS &&... args) {
      size_t capacity = length+1;
      size_t size = sizeof_container_with_header<OT>(capacity);
      return GCObjectAllocator<OT>::allocate_kind(OT::static_HeaderValue,size,length,/*initial_element,*/std::forward<ARGS>(args)...);
    }


    
    template <typename... ARGS>
      static smart_pointer_type allocate_bitunit_container( size_t length, ARGS &&... args) {
      size_t size = sizeof_bitunit_container_with_header<OT>(length);
#ifdef DEBUG_BITUNIT_CONTAINER
      printf("%s:%d  In allocate_bitunit_container length = %lu  size= %lu\n", __FILE__, __LINE__, length, size );
#endif
      smart_pointer_type result = GCObjectAllocator<OT>::allocate_kind(Header_s::Value::make<OT>(),size,length,std::forward<ARGS>(args)...);
#if DEBUG_BITUNIT_CONTAINER
      {
        printf("%s:%d allocate_bitunit_container \n", __FILE__, __LINE__ );
        printf("            Allocated object tagged ptr = %p\n", (void*)result.raw_());
      }
#endif

      return result;
    }
    
    static smart_pointer_type copy(const OT &that) {
      return GCObjectAllocator<OT>::copy_kind(Header_s::Value::make<OT>(),sizeof_with_header<OT>(),that);
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
    return allocate_kind(Header_s::Value::make<TY>(),num);
  }

  // allocate but don't initialize num elements of type value_type
  gc::tagged_pointer<container_type> allocate_kind(const Header_s::Value& the_header, size_type num, const void * = 0) {
#ifdef USE_BOEHM
    size_t size = sizeof_container_with_header<TY>(num);
    monitor_allocation(the_header.stamp(),size);
    Header_s* base = do_boehm_normal_allocation(the_header,size);
    container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
    lisp_check_pending_interrupts(my_thread);
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    size_t size = sizeof_container_with_header<TY>(num);
    monitor_allocation(the_header.stamp(),size);
    mps_ap_t obj_ap = my_thread_allocation_points._automatic_mostly_copying_allocation_point;
    globalMpsMetrics.movingAllocations++;
    gc::tagged_pointer<container_type> obj =
      do_mps_allocation_<gc::tagged_pointer<container_type>>(the_header,
                                                            size,obj_ap,"containerAMC",
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
class GCAbstractAllocator /* : public GCAlloc<TY> */ {
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
  /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
  GCAbstractAllocator() throw() {}
  ~GCAbstractAllocator() throw() {}

    // allocate but don't initialize num elements of type value_type
  void never_invoke_allocate() {};
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
  gctools::tagged_pointer<container_type> allocate_kind( const Header_s::Value& the_header, size_type num, const void * = 0) {
#ifdef USE_BOEHM
    size_t size = sizeof_container_with_header<TY>(num);
    monitor_allocation(the_header.stamp(),size);
    // prepend a one pointer header with a pointer to the typeinfo.name
    Header_s* base = do_boehm_normal_allocation(the_header,size);
    container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
    lisp_check_pending_interrupts(my_thread);
    return myAddress;
#endif
#ifdef USE_MPS
    size_t size = sizeof_container_with_header<TY>(num);
    monitor_allocation(the_header.stamp(),size);
    mps_ap_t obj_ap = my_thread_allocation_points._non_moving_allocation_point;
    globalMpsMetrics.nonMovingAllocations++;
    gctools::tagged_pointer<container_type> obj =
      do_mps_allocation_<gc::tagged_pointer<container_type>>(the_header,size,obj_ap,"container_non_moving_ap",
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
    monitor_allocation(STAMP_null,size);
#ifdef USE_BOEHM
#ifdef DEBUG_GCWEAK
    printf("%s:%d Allocating Bucket with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__);
#endif
    container_pointer myAddress = (container_pointer)GC_MALLOC_ATOMIC(size);
    global_AllocationProfiler.registerAllocation(size);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
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
      do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(size,my_thread_allocation_points._weak_link_allocation_point,"weak_link_Bucket",num);
    return obj;
#endif
  }

  // initialize elements of allocated storage p with value value
  template <typename... ARGS>
  void construct(pointer p, ARGS &&... args) {
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
    monitor_allocation(STAMP_null,size);
#ifdef USE_BOEHM
#ifdef DEBUG_GCWEAK
    printf("%s:%d Allocating Bucket with GC_MALLOC\n", __FILE__, __LINE__);
#endif
    container_pointer myAddress = (container_pointer)GC_MALLOC(size);
    global_AllocationProfiler.registerAllocation(size);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) container_type(num);
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    gctools::tagged_pointer<container_type> obj =
      do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(size,my_thread_allocation_points._strong_link_allocation_point,"strong_link_Bucket",num);
    return obj;
#endif
  }

  // initialize elements of allocated storage p with value value
  template <typename... ARGS>
  void construct(pointer p, ARGS &&... args) {
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
    monitor_allocation(STAMP_null,size);
#ifdef USE_BOEHM
    printf("%s:%d Allocating Mapping with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__);
    container_pointer myAddress = (container_pointer)GC_MALLOC_ATOMIC(size);
    global_AllocationProfiler.registerAllocation(size);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) container_type(val);
    printf("%s:%d Check if Mapping has been initialized to unbound\n", __FILE__, __LINE__);
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    typedef typename GCHeader<TY>::HeaderType HeadT;
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    gctools::tagged_pointer<container_type> obj =
      do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(size,my_thread_allocation_points._weak_link_allocation_point,"weak_link_Allocator",val);
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
    monitor_allocation(STAMP_null,size);
#ifdef USE_BOEHM
    printf("%s:%d Allocating Mapping with GC_MALLOC\n", __FILE__, __LINE__);
    container_pointer myAddress = (container_pointer)GC_MALLOC(size);
    global_AllocationProfiler.registerAllocation(size);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) container_type(val);
    printf("%s:%d Check if Mapping has been initialized to unbound\n", __FILE__, __LINE__);
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    typedef typename GCHeader<TY>::HeaderType HeadT;
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    gctools::tagged_pointer<container_type> obj =
      do_mps_weak_allocation<gctools::tagged_pointer<container_type>>(size,my_thread_allocation_points._strong_link_allocation_point,"weak_link2_Allocator",val);
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
    monitor_allocation(STAMP_null,size);
#ifdef USE_BOEHM
    printf("%s:%d Allocating WeakPointer with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__);
    value_pointer myAddress = (value_pointer)GC_MALLOC_ATOMIC(size);
    global_AllocationProfiler.registerAllocation(size);
    if (!myAddress)
      throw_hard_error("Out of memory in allocate");
    new (myAddress) VT(val);
    return gctools::tagged_pointer<value_type>(myAddress);
#endif
#ifdef USE_MPS
    mps_addr_t addr;
    value_pointer myAddress;
    gctools::tagged_pointer<value_type> obj =
      do_mps_weak_allocation<gctools::tagged_pointer<value_type>>(size,my_thread_allocation_points._weak_link_allocation_point,"weak_link3_Allocator",val);
    return obj;
#endif
  }
};
};


namespace gctools {

#if 0
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
  uintptr_clasp_t *_StackCur;
  uintptr_clasp_t *_StackBottom;
  size_t _StackMinOffset;
  size_t _StackMiddleOffset;
  uintptr_clasp_t *_StackLimit;
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
    this->_StackBottom = (uintptr_clasp_t *)GC_MALLOC(bufferSize);
    this->_StackMiddleOffset = (bufferSize / 2);
    this->_StackLimit = (uintptr_clasp_t *)((char *)this->_StackBottom + bufferSize);
    this->_StackMinOffset = bufferSize;
    this->_StackCur = this->_StackBottom;
    memset(this->_StackBottom, 0, bufferSize);
#else
// Do nothing
#endif
#endif
    return true;
  };
  void deallocateStack() {
#ifdef USE_BOEHM
#ifdef BOEHM_ONE_BIG_STACK
    if (this->_StackCur != this->_StackBottom) {
      throw_hard_error("The stack is not empty");
    }
    GC_FREE(this->_StackBottom);
#else
// Do nothing
#endif
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
#define FRAME_START(hptr) (uintptr_clasp_t *)(((char *)hptr) + FRAME_HEADER_SIZE)
#define FRAME_HEADER(fptr) (uintptr_clasp_t *)(((char *)fptr) - FRAME_HEADER_SIZE)
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
    uintptr_clasp_t *frameHeaderP = reinterpret_cast<uintptr_clasp_t *>(frameImpl) - 1;
    uintptr_clasp_t headerAndFrameSize = FRAME_HEADER_SIZE_FIELD(frameHeaderP);
    this->_TotalSize = this->_TotalSize - headerAndFrameSize;
#ifdef BOEHM_ONE_BIG_STACK
    memset(frameHeaderP, 0, headerAndFrameSize);
    this->_StackCur = frameHeaderP;
    if (this->_StackMinOffset <= (this->_StackLimit - this->_StackBottom) && (this->_StackCur - this->_StackBottom) < this->_StackMiddleOffset)
      this->shrinkStack();
#ifdef DEBUG_BOEHM_STACK
    size_t calcSize = (char *)this->_StackTop - (char *)this->_StackBottom;
    if (calcSize != this->_TotalSize) {
      throw_hard_error_side_stack_damaged(this->_TotalSize,calcSize);
    }
    for (char *i = (char *)this->_StackTop; i < (char *)this->_StackLimit; ++i) {
      if (*i) {
        throw_hard_error("The side-stack has garbage in it!");
      }
    }
#endif
#else
    GC_FREE(frameHeaderP);
#endif
#endif // USE_BOEHM
#ifdef USE_MPS
    uintptr_clasp_t *frameHeaderP = FRAME_HEADER(frameImpl);
    uintptr_clasp_t headerAndFrameSize = FRAME_HEADER_SIZE_FIELD(frameHeaderP);
    this->_TotalSize -= headerAndFrameSize;
    mps_frame_t frame_o = this->frames.back();
    this->frames.pop_back();
    mps_res_t res = mps_ap_frame_pop(this->_AllocationPoint, frame_o);
    if (res != MPS_RES_OK) {
      throw_hard_error_mps_bad_result(res);
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
#endif
 
};

namespace gctools {

  extern void* malloc_uncollectable_and_zero(size_t size);
};

#endif // USE_BOEHM || USE_MPS

#endif
