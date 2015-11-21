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
    sp->initialize();
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

/*! Maintain a stack containing pointers that are garbage collected
*/
class GCStack {
public:
  typedef enum { undefined_t,
                 frame_t,
                 pad_t } frameType;
  size_t _MaxSize;
  size_t _TotalSize;
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

  GCStack() : _TotalSize(0), _MaxSize(0)
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

namespace gctools {

/*! Allocate regular C++ classes that are considered roots */
template <class T>
struct RootClassAllocator {
  template <class... ARGS>
  static gctools::tagged_pointer<T> allocate(ARGS &&... args) {
    size_t sz = sizeof_with_header<T>();
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += sz;
    MONITOR_ALLOCATION(GCKind<T>::Kind, sz);
#endif
#ifdef USE_BOEHM
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_UNCOLLECTABLE(sz));
    new (base) Header_s(GCKind<T>::Kind);
    T *obj = BasePtrToMostDerivedPtr<T>(base);
    new (obj) T(std::forward<ARGS>(args)...);
    POLL_SIGNALS();
    gctools::tagged_pointer<T> tagged_obj(obj);
    return tagged_obj;
#endif
#ifdef USE_MPS
    // Different classes can have different Headers
    typedef typename GCHeader<T>::HeaderType HeadT;
    T *obj;
    mps_ap_t obj_ap = global_non_moving_ap;
    mps_addr_t addr;
    gctools::tagged_pointer<T> tagged_obj;
    do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
      mps_res_t res = mps_reserve(&addr, obj_ap, sz);
#pragma clang diagnostic pop
      HeadT *header = reinterpret_cast<HeadT *>(addr);
      new (header) HeadT(GCKind<T>::Kind);
      obj = BasePtrToMostDerivedPtr<T>(addr);
      new (obj) T(std::forward<ARGS>(args)...);
      tagged_obj = gctools::tagged_pointer<T>(obj);
    } while (!mps_commit(obj_ap, addr, sz));
    globalMpsMetrics.nonMovingAllocation(sz);
    DEBUG_MPS_ALLOCATION("NON_MOVING_POOL", addr, obj, sz, gctools::GCKind<T>::Kind);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    POLL_SIGNALS();
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
 #error "I need a way to deallocate MPS allocated objects that are not moveable or collectable"
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

template <class T>
struct ClassAllocator {
  /*! Allocate regular C++ classes that will be garbage collected as soon as nothing points to them */
  template <class... ARGS>
  static tagged_pointer<T> allocateClass(ARGS &&... args) {
    size_t sz = sizeof_with_header<T>();
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += sz;
    MONITOR_ALLOCATION(GCKind<T>::Kind, sz);
#endif
#ifdef USE_BOEHM
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(sz));
    new (base) Header_s(GCKind<T>::Kind);
    T *obj = BasePtrToMostDerivedPtr<T>(base);
    new (obj) T(std::forward<ARGS>(args)...);
    POLL_SIGNALS();
    return tagged_pointer<T>(obj);
#endif
#ifdef USE_MPS
    // Different classes can have different Headers
    typedef typename GCHeader<T>::HeaderType HeadT;
    T *obj;
    tagged_pointer<T> tagged_obj;
    mps_ap_t obj_ap = GCAllocationPoint<T>::get();
    mps_addr_t addr;
    do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
      mps_res_t res = mps_reserve(&addr, obj_ap, sz);
#pragma clang diagnostic pop
      HeadT *header = reinterpret_cast<HeadT *>(addr);
      new (header) HeadT(GCKind<T>::Kind);
      obj = BasePtrToMostDerivedPtr<T>(addr);
      new (obj) T(std::forward<ARGS>(args)...);
      tagged_obj = tagged_pointer<T>(obj);
    } while (!mps_commit(obj_ap, addr, sz));
    globalMpsMetrics.unknownAllocation(sz);
    DEBUG_MPS_ALLOCATION("AP", addr, obj, sz, gctools::GCKind<T>::Kind);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    POLL_SIGNALS();
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
      static smart_pointer_type allocateInAppropriatePool(ARGS &&... args) {
        size_t size = sizeof_with_header<OT>();
#ifdef TRACK_ALLOCATIONS
        globalBytesAllocated += size;
        MONITOR_ALLOCATION(GCKind<OT>::Kind, size);
#endif
#ifdef USE_BOEHM
    // By default allocate in the normal pool for objects that contain pointers
    // to other objects.
        Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(size));
        new (base) Header_s(GCKind<OT>::Kind);
        pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
        new (ptr) OT(std::forward<ARGS>(args)...);
        smart_pointer_type sp = smart_ptr<value_type>(ptr);
        return sp;
#endif
#ifdef USE_MPS
        typedef typename GCHeader<OT>::HeaderType HeadT;
        OT *obj;
        mps_ap_t obj_ap = _global_automatic_mostly_copying_allocation_point;
        mps_addr_t addr;
        smart_pointer_type sp;
        do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
          mps_res_t res = mps_reserve(&addr, obj_ap, size);
#pragma clang diagnostic pop
          HeadT *header = reinterpret_cast<HeadT *>(addr);
          new (header) HeadT(GCKind<OT>::Kind);
          obj = BasePtrToMostDerivedPtr<OT>(addr);
          new (obj) OT(std::forward<ARGS>(args)...);
          sp = gctools::smart_ptr<value_type>(obj);
        } while (!mps_commit(obj_ap, addr, size));
        globalMpsMetrics.movingAllocation(size);
        DEBUG_MPS_ALLOCATION("AMC", addr, obj, size, gctools::GCKind<OT>::Kind);
        DEBUG_MPS_UNDERSCANNING_TESTS();
        return sp;
#endif
      };
      static void deallocate(gctools::smart_ptr<OT> memory) {
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
      static smart_pointer_type allocateInAppropriatePool(ARGS &&... args) {
      size_t size = sizeof_with_header<OT>();
#ifdef TRACK_ALLOCATIONS
      globalBytesAllocated += size;
      MONITOR_ALLOCATION(GCKind<OT>::Kind, size);
#endif
#ifdef USE_BOEHM
    // Atomic objects (do not contain pointers) are allocated in separate pool
      Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_ATOMIC(size));
      new (base) Header_s(GCKind<OT>::Kind);
      pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
      new (ptr) OT(std::forward<ARGS>(args)...);
      smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
      return sp;
#endif
#ifdef USE_MPS
      typedef typename GCHeader<OT>::HeaderType HeadT;
      OT *obj;
      mps_ap_t obj_ap = _global_automatic_mostly_copying_zero_rank_allocation_point;
      mps_addr_t addr;
      smart_pointer_type sp;
      do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
        mps_res_t res = mps_reserve(&addr, obj_ap, size);
#pragma clang diagnostic pop
        HeadT *header = reinterpret_cast<HeadT *>(addr);
        new (header) HeadT(GCKind<OT>::Kind);
        obj = BasePtrToMostDerivedPtr<OT>(addr);
        new (obj) OT(std::forward<ARGS>(args)...);
        sp = /*gctools::*/ smart_ptr<value_type>(obj);
      } while (!mps_commit(obj_ap, addr, size));
      globalMpsMetrics.movingZeroRankAllocation(size);
      DEBUG_MPS_ALLOCATION("AMCZ", addr, obj, size, /*gctools::*/ GCKind<OT>::Kind);
      DEBUG_MPS_UNDERSCANNING_TESTS();
      return sp;
#endif
    };
    static void deallocate(gctools::smart_ptr<OT> memory) {
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
      static smart_pointer_type allocateInAppropriatePool(ARGS &&... args) {
      size_t size = sizeof_with_header<OT>();
#ifdef TRACK_ALLOCATIONS
      globalBytesAllocated += size;
      MONITOR_ALLOCATION(GCKind<OT>::Kind, size);
#endif
#ifdef USE_BOEHM
    // By default allocate in the normal pool for objects that contain pointers
    // to other objects.
      Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(size));
      new (base) Header_s(GCKind<OT>::Kind);
      pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
      new (ptr) OT(std::forward<ARGS>(args)...);
      smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
      return sp;
#endif
#ifdef USE_MPS
      typedef typename GCHeader<OT>::HeaderType HeadT;
      OT *obj;
      mps_ap_t obj_ap = global_non_moving_ap;
      mps_addr_t addr;
      smart_pointer_type sp;
      do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
        mps_res_t res = mps_reserve(&addr, obj_ap, size);
#pragma clang diagnostic pop
        HeadT *header = reinterpret_cast<HeadT *>(addr);
        new (header) HeadT(GCKind<OT>::Kind);
        obj = BasePtrToMostDerivedPtr<OT>(addr);
        new (obj) OT(std::forward<ARGS>(args)...);
        sp = /*gctools::*/ smart_ptr<value_type>(obj);
      } while (!mps_commit(obj_ap, addr, size));
      globalMpsMetrics.nonMovingAllocation(size);
      DEBUG_MPS_ALLOCATION("NON_MOVING_POOL", addr, obj, size, /*gctools::*/ GCKind<OT>::Kind);
      DEBUG_MPS_UNDERSCANNING_TESTS();
      return sp;
#endif
    };
    static void deallocate(gctools::smart_ptr<OT> memory) {
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
      static smart_pointer_type allocateInAppropriatePool(ARGS &&... args) {
      size_t sz = sizeof_with_header<OT>();
#ifdef TRACK_ALLOCATIONS
      globalBytesAllocated += sz;
      MONITOR_ALLOCATION(GCKind<OT>::Kind, sz);
#endif
#ifdef USE_BOEHM
      Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_UNCOLLECTABLE(sz));
      new (base) Header_s(GCKind<OT>::Kind);
      OT *obj = BasePtrToMostDerivedPtr<OT>(base);
      new (obj) OT(std::forward<ARGS>(args)...);
      POLL_SIGNALS();
      gctools::smart_ptr<OT> sp(obj);
      return sp;
#endif
#ifdef USE_MPS
    // Different classes can have different Headers
      typedef typename GCHeader<T>::HeaderType HeadT;
      T *obj;
      mps_ap_t obj_ap = global_non_moving_ap;
      mps_addr_t addr;
      gctools::smart_ptr<T> sp;
      do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
        mps_res_t res = mps_reserve(&addr, obj_ap, sz);
#pragma clang diagnostic pop
        HeadT *header = reinterpret_cast<HeadT *>(addr);
        new (header) HeadT(GCKind<T>::Kind);
        obj = BasePtrToMostDerivedPtr<T>(addr);
        new (obj) T(std::forward<ARGS>(args)...);
        sp = gctools::smart_ptr<T>(obj);
      } while (!mps_commit(obj_ap, addr, sz));
      globalMpsMetrics.nonMovingAllocation(sz);
      DEBUG_MPS_ALLOCATION("NON_MOVING_POOL", addr, obj, sz, gctools::GCKind<T>::Kind);
      DEBUG_MPS_UNDERSCANNING_TESTS();
      POLL_SIGNALS();
      return sp;
#endif
    }

    static void deallocate(gctools::smart_ptr<OT> memory) {
#ifdef USE_BOEHM
      GC_FREE(&*memory);
#endif
#if defined(USE_MPS) && !defined(RUNNING_GC_BUILDER)
 #error "I need a way to deallocate MPS allocated objects that are not moveable or collectable"
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
  static smart_pointer_type rootAllocate(ARGS &&... args) {
#ifdef USE_BOEHM
    size_t sz = sizeof_with_header<OT>(); // USE HEADER FOR BOEHM ROOTS BUT NOT MPS
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += sz;
    MONITOR_ALLOCATION(GCKind<OT>::Kind, sz);
#endif
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_UNCOLLECTABLE(sz));
    new (base) Header_s(GCKind<OT>::Kind);
    pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
    new (ptr) OT(std::forward<ARGS>(args)...);
    smart_pointer_type sp = /*gctools::*/ smart_ptr<value_type>(ptr);
    GCObjectInitializer<OT, /*gctools::*/ GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#endif
#ifdef USE_MPS
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    POLL_SIGNALS();
    return sp;
#endif
  };

  template <typename... ARGS>
  static smart_pointer_type allocate(ARGS &&... args) {
#ifdef USE_BOEHM
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, /*gctools::*/ GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    POLL_SIGNALS();
    return sp;
#endif
#ifdef USE_MPS
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
    GCObjectInitializer<OT, GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    //            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
    POLL_SIGNALS();
    return sp;
#endif
  };

  static smart_pointer_type copy(const OT &that) {
#ifdef USE_BOEHM
    // Copied objects must be allocated in the appropriate pool
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocateInAppropriatePool(that);
    // Copied objects are not initialized.
    // Copied objects are finalized if necessary
    GCObjectFinalizer<OT, /*gctools::*/ GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#endif
#ifdef USE_MPS
    // Copied objects must be allocated in the appropriate pool
    smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::allocateInAppropriatePool(that);
    // Copied objects are not initialized.
    // Copied objects are finalized if necessary
    GCObjectFinalizer<OT, GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
    return sp;
#endif
  }

   static void deallocate_unmanaged_instance(const smart_ptr<OT> that) {
     GCObjectAppropriatePoolAllocator<OT, GCInfo<OT>::Policy>::deallocate(that);
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
    size_t size = sizeof_container_with_header<TY>(num);
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += size;
    MONITOR_ALLOCATION(GCKind<TY>::Kind, size);
#endif
#ifdef USE_BOEHM
    // prepend a one pointer header with a pointer to the typeinfo.name
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(size));
    if (!base)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (base) Header_s(GCKind<TY>::Kind);
    container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
    POLL_SIGNALS();
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#ifdef USE_MPS
    typedef typename GCHeader<TY>::HeaderType HeadT;
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    mps_ap_t obj_ap = _global_automatic_mostly_copying_allocation_point;
    gc::tagged_pointer<container_type> obj;
    do {
      mps_res_t res = mps_reserve(&addr, obj_ap, size);
      if (res != MPS_RES_OK)
        THROW_HARD_ERROR(BF("Out of memory in GCContainerAllocator_mps"));
      HeadT *header = reinterpret_cast<HeadT *>(addr);
      new (header) HeadT(GCKind<TY>::Kind);
      myAddress = (BasePtrToMostDerivedPtr<TY>(addr));
      new (myAddress) TY(num);
      obj = gctools::tagged_pointer<container_type>(myAddress);
    } while (!mps_commit(obj_ap, addr, size));
    globalMpsMetrics.movingAllocation(size);
    GC_LOG(("malloc@%p %zu bytes\n", myAddress, size));
    DEBUG_MPS_ALLOCATION("containerAMC", addr, myAddress, size, /*gctools::*/ GCKind<TY>::Kind);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    POLL_SIGNALS();
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
  gctools::tagged_pointer<container_type> allocate(size_type num, const void * = 0) {
    size_t size = sizeof_container_with_header<TY>(num);
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += size;
    MONITOR_ALLOCATION(GCKind<TY>::Kind, size);
#endif
#ifdef USE_BOEHM
    // prepend a one pointer header with a pointer to the typeinfo.name
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC(size));
    if (!base)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (base) Header_s(GCKind<TY>::Kind);
    container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
    POLL_SIGNALS();
    return myAddress;
#endif
#ifdef USE_MPS
    typedef typename GCHeader<TY>::HeaderType HeadT;
    mps_addr_t addr;
    container_pointer myAddress(NULL);
    mps_ap_t obj_ap = global_non_moving_ap;
    gctools::tagged_pointer<container_type> obj;
    do {
      mps_res_t res = mps_reserve(&addr, obj_ap, size);
      if (res != MPS_RES_OK)
        THROW_HARD_ERROR(BF("Out of memory in GCContainerNonMoveableAllocator_mps"));
      HeadT *header = reinterpret_cast<HeadT *>(addr);
      new (header) HeadT(GCKind<TY>::Kind);
      myAddress = (BasePtrToMostDerivedPtr<TY>(addr));
      new (myAddress) TY(num);
      obj = gctools::tagged_pointer<container_type>(myAddress);
    } while (!mps_commit(obj_ap, addr, size));
    globalMpsMetrics.nonMovingAllocation(size);
    GC_LOG(("malloc@%p %zu bytes\n", myAddress, size));
    POLL_SIGNALS();
    DEBUG_MPS_ALLOCATION("container_MVFF", addr, myAddress, size, /*gctools::*/ GCKind<TY>::Kind);
    DEBUG_MPS_UNDERSCANNING_TESTS();
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
  gctools::tagged_pointer<container_type> allocate(size_type num, const void * = 0) {
    size_t sz = sizeof_container_with_header<container_type>(num);
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += sz;
    MONITOR_ALLOCATION(GCKind<TY>::Kind, sz);
#endif
#if defined(USE_BOEHM)
    // prepend a one pointer header with a pointer to the typeinfo.name
    Header_s *base = reinterpret_cast<Header_s *>(GC_MALLOC_ATOMIC(sz));
    if (!base)
      THROW_HARD_ERROR(BF("Out of memory in allocate"));
    new (base) Header_s(GCKind<TY>::Kind);
    container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
    new (myAddress) TY(num);
    POLL_SIGNALS();
    return gctools::tagged_pointer<container_type>(myAddress);
#endif
#if defined(USE_MPS)
    typedef typename GCHeader<TY>::HeaderType HeadT;
    mps_ap_t obj_ap = _global_automatic_mostly_copying_zero_rank_allocation_point;
    mps_addr_t base;
    container_pointer myAddress;
    gctools::tagged_pointer<container_type> obj;
    do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
      mps_res_t res = mps_reserve(&base, obj_ap, sz);
#pragma clang diagnostic pop
      HeadT *header = reinterpret_cast<HeadT *>(base);
      new (header) HeadT(GCKind<TY>::Kind);
      //                header->kind._Kind = /*gctools::*/GCKind<container_type>::Kind;
      myAddress = BasePtrToMostDerivedPtr<TY>(base);
      obj = gctools::tagged_pointer<container_type>(myAddress);
    } while (!mps_commit(obj_ap, base, sz));
    globalMpsMetrics.movingZeroRankAllocation(sz);
    new (myAddress) TY(num);
    POLL_SIGNALS();
    DEBUG_MPS_ALLOCATION("string_AMCZ", base, myAddress, sz, /*gctools::*/ GCKind<TY>::Kind);
    DEBUG_MPS_UNDERSCANNING_TESTS();
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
  static gctools::tagged_pointer<container_type> allocate(size_type num, const void * = 0) {
    size_t size = sizeof_container<container_type>(num); // NO HEADER FOR BUCKETS
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += size;
    MONITOR_ALLOCATION(KIND_null, size);
#endif
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
    gctools::tagged_pointer<container_type> obj;
    do {
      mps_res_t res = mps_reserve(&addr, _global_weak_link_allocation_point, size);
      if (res != MPS_RES_OK)
        THROW_HARD_ERROR(BF("Out of memory in GCBucketsAllocator_mps"));
      GC_LOG(("allocated @%p %zu bytes\n", addr, size));
      myAddress = reinterpret_cast<container_pointer>(addr);
      if (!myAddress)
        THROW_HARD_ERROR(BF("NULL address in allocate!"));
      new (myAddress) container_type(num);
      obj = gctools::tagged_pointer<container_type>(myAddress);
    } while (!mps_commit(_global_weak_link_allocation_point, addr, size));
    DEBUG_MPS_ALLOCATION("weak_link_Bucket", addr, myAddress, size, /*gctools::*/ KIND_null);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    if (!myAddress)
      THROW_HARD_ERROR(BF("Could not allocate from GCBucketAllocator<Buckets<VT,VT,WeakLinks>>"));
    GC_LOG(("malloc@%p %zu bytes\n", myAddress, size));
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
  static gctools::tagged_pointer<container_type> allocate(size_type num, const void * = 0) {
    size_t size = sizeof_container<container_type>(num); // NO HEADER FOR BUCKETS
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += size;
    MONITOR_ALLOCATION(KIND_null, size);
#endif
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
    gctools::tagged_pointer<container_type> obj;
    do {
      mps_res_t res = mps_reserve(&addr, _global_strong_link_allocation_point, size);
      if (res != MPS_RES_OK)
        THROW_HARD_ERROR(BF("Out of memory in GCBucketsAllocator_mps"));
      GC_LOG(("allocated @%p %zu bytes\n", addr, size));
      myAddress = reinterpret_cast<container_pointer>(addr);
      if (!myAddress)
        THROW_HARD_ERROR(BF("NULL address in allocate!"));
      new (myAddress) container_type(num);
      obj = gctools::tagged_pointer<container_type>(myAddress);
    } while (!mps_commit(_global_strong_link_allocation_point, addr, size));
    DEBUG_MPS_ALLOCATION("strong_link_Bucket", addr, myAddress, size, /*gctools::*/ KIND_null);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    if (!myAddress)
      THROW_HARD_ERROR(BF("Could not allocate from GCBucketAllocator<Buckets<VT,VT,StrongLinks>>"));
    GC_LOG(("malloc@%p %zu bytes\n", myAddress, size));
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
  static gctools::tagged_pointer<container_type> allocate(const VT &val) {
    size_t size = sizeof(container_type);
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += size;
    MONITOR_ALLOCATION(KIND_null, size);
#endif
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
    gctools::tagged_pointer<container_type> obj;
    do {
      mps_res_t res = mps_reserve(&addr, _global_weak_link_allocation_point, size);
      if (res != MPS_RES_OK)
        THROW_HARD_ERROR(BF("Out of memory in GCMappingAllocator_mps"));
      myAddress = reinterpret_cast<container_pointer>(addr);
      new (myAddress) container_type(val);
      obj = gctools::tagged_pointer<container_type>(myAddress);
    } while (!mps_commit(_global_weak_link_allocation_point, addr, size));
    DEBUG_MPS_ALLOCATION("weak_link_Allocator", addr, myAddress, size, /*gctools::*/ KIND_null);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    GC_LOG(("malloc@%p %zu bytes\n", myAddress, size));
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
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += size;
    MONITOR_ALLOCATION(KIND_null, size);
#endif
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
    gctools::tagged_pointer<container_type> obj;
    do {
      mps_res_t res = mps_reserve(&addr, _global_strong_link_allocation_point, size);
      if (res != MPS_RES_OK)
        THROW_HARD_ERROR(BF("Out of memory in GCMappingAllocator_mps"));
      myAddress = reinterpret_cast<container_pointer>(addr);
      new (myAddress) container_type(val);
      obj = gctools::tagged_pointer<container_type>(myAddress);
    } while (!mps_commit(_global_weak_link_allocation_point, addr, size));
    DEBUG_MPS_ALLOCATION("weak_link2_Allocator", addr, myAddress, size, /*gctools::*/ KIND_null);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    GC_LOG(("malloc@%p %zu bytes\n", myAddress, size));
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
#ifdef TRACK_ALLOCATIONS
    globalBytesAllocated += size;
    MONITOR_ALLOCATION(KIND_null, size);
#endif
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
    gctools::tagged_pointer<value_type> obj;
    do {
      mps_res_t res = mps_reserve(&addr, _global_weak_link_allocation_point, size);
      if (res != MPS_RES_OK)
        THROW_HARD_ERROR(BF("Out of memory in GCWeakPointerAllocator_mps"));
      myAddress = reinterpret_cast<value_pointer>(addr);
      new (myAddress) VT(val);
      obj = gctools::tagged_pointer<value_type>(myAddress);
    } while (!mps_commit(_global_weak_link_allocation_point, addr, size));
    DEBUG_MPS_ALLOCATION("weak_link3_Allocator", addr, myAddress, size, /*gctools::*/ KIND_null);
    DEBUG_MPS_UNDERSCANNING_TESTS();
    return obj;
#endif
  }
};
};

#endif // USE_BOEHM || USE_MPS

#endif
