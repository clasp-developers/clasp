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


#include <limits>




namespace gctools {


};

namespace gctools {
    template <class OT, bool Needed=true>
    struct GCObjectInitializer {
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        static void initializeIfNeeded(smart_pointer_type sp)
        {
            sp->initialize();
        };
    };

    template <class OT>
    struct GCObjectInitializer<OT,false> {
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        static void initializeIfNeeded(smart_pointer_type sp)
        {
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


    /*! Allocate regular C++ classes that are considered roots */
    template <class T>
    struct RootClassAllocator {
        template <class...ARGS>
        static T* allocate(ARGS&&...args)
        {
#ifdef USE_BOEHM
            size_t sz = sizeof_with_header<T>();
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC_UNCOLLECTABLE(sz));
            new (base) Header_s(const_cast<char*>(typeid(T).name()),BoehmClassKind);
            T* obj = BasePtrToMostDerivedPtr<T>(base);
            new (obj) T(std::forward<ARGS>(args)...);
            POLL_SIGNALS();
            return obj;
#endif
#ifdef USE_MPS
            // Different classes can have different Headers
            size_t sz = sizeof_with_header<T>();
            typedef typename GCHeader<T>::HeaderType HeadT;
            T* obj;
            mps_ap_t obj_ap = global_non_moving_ap;
            mps_addr_t addr;
            do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
                mps_res_t res = mps_reserve(&addr,obj_ap,sz);
#pragma clang diagnostic pop
                HeadT* header = reinterpret_cast<HeadT*>(addr);
                new (header) HeadT(GCKind<T>::Kind);
                obj = BasePtrToMostDerivedPtr<T>(addr);
                new (obj) T(std::forward<ARGS>(args)...);
            } while (!mps_commit(obj_ap,addr,sz) );
            globalMpsMetrics.nonMovingAllocation(sz);
            DEBUG_MPS_ALLOCATION("NON_MOVING_POOL", addr,obj,sz,gctools::GCKind<T>::Kind);
            POLL_SIGNALS();
            return obj;
#endif
        }
    };


    template <class T>
    struct ClassAllocator {
        /*! Allocate regular C++ classes that will be garbage collected as soon as nothing points to them */
        template <class...ARGS>
        static T* allocateClass(ARGS&&...args)
        {
#ifdef USE_BOEHM
            size_t sz = sizeof_with_header<T>();
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC(sz));
            new (base) Header_s(typeid(T).name(),BoehmClassKind);
            T* obj = BasePtrToMostDerivedPtr<T>(base);
            new (obj) T(std::forward<ARGS>(args)...);
            POLL_SIGNALS();
            return obj;
#endif
#ifdef USE_MPS
            // Different classes can have different Headers
            size_t sz = sizeof_with_header<T>();
            typedef typename GCHeader<T>::HeaderType HeadT;
            T* obj;
            mps_ap_t obj_ap = GCAllocationPoint<T>::get();
            mps_addr_t addr;
            do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
                mps_res_t res = mps_reserve(&addr,obj_ap,sz);
#pragma clang diagnostic pop
                HeadT* header = reinterpret_cast<HeadT*>(addr);
                new (header) HeadT(GCKind<T>::Kind);
                obj = BasePtrToMostDerivedPtr<T>(addr);
                new (obj) T(std::forward<ARGS>(args)...);
            } while (!mps_commit(obj_ap,addr,sz) );
            globalMpsMetrics.unknownAllocation(sz);
            DEBUG_MPS_ALLOCATION("AP", addr,obj,sz,gctools::GCKind<T>::Kind);
            POLL_SIGNALS();
            return obj;
#endif
        }

        /*! Allocate regular C++ classes that will be garbage collected as soon as nothing points to them */
        static void deallocateClass(T* ptr)
        {
            // BOEHM and MPS do nothing
        }
    };
};


namespace gctools {

    template <class OT, bool Atomic=false, bool Moveable=true>
    struct GCObjectAppropriatePoolAllocator {
        typedef OT                      value_type;
        typedef OT*                     pointer_type;
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        template <typename...ARGS>
        static smart_pointer_type allocateInAppropriatePool(ARGS&&...args)
        {
#ifdef USE_BOEHM
            // By default allocate in the normal pool for objects that contain pointers
            // to other objects.
            size_t sz = sizeof_with_header<OT>();
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC(sz));
            new (base) Header_s(typeid(OT).name(),BoehmLispKind);
            pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            return sp;
#endif
#ifdef USE_MPS
            typedef typename GCHeader<OT>::HeaderType HeadT;
            OT* obj;
            mps_ap_t obj_ap = _global_automatic_mostly_copying_allocation_point;
            mps_addr_t addr;
            size_t size = sizeof_with_header<OT>();
            do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
                mps_res_t res = mps_reserve(&addr,obj_ap,size);
#pragma clang diagnostic pop
                HeadT* header = reinterpret_cast<HeadT*>(addr);
                new (header) HeadT(GCKind<OT>::Kind);
                obj = BasePtrToMostDerivedPtr<OT>(addr);
                new (obj) OT(std::forward<ARGS>(args)...);
            } while (!mps_commit(obj_ap,addr,size) );
            globalMpsMetrics.movingAllocation(size);
            DEBUG_MPS_ALLOCATION("AMC", addr,obj,size,gctools::GCKind<OT>::Kind);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(obj);
            return sp;
#endif
        };
    };


    template <class OT>
    struct GCObjectAppropriatePoolAllocator<OT, /*Atomic=*/ true, /*Moveable=*/ true> {
        typedef OT                      value_type;
        typedef OT*                     pointer_type;
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        template <typename...ARGS>
        static smart_pointer_type allocateInAppropriatePool(ARGS&&...args)
        {
#ifdef USE_BOEHM
            // Atomic objects (do not contain pointers) are allocated in separate pool
            size_t sz = sizeof_with_header<OT>();
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC_ATOMIC(sz));
            new (base) Header_s(typeid(OT).name(),BoehmLispKind);
            pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            return sp;
#endif
#ifdef USE_MPS
            typedef typename GCHeader<OT>::HeaderType HeadT;
            OT* obj;
            mps_ap_t obj_ap = _global_automatic_mostly_copying_zero_rank_allocation_point;
            mps_addr_t addr;
            size_t size = sizeof_with_header<OT>();
            do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
                mps_res_t res = mps_reserve(&addr,obj_ap,size);
#pragma clang diagnostic pop
                HeadT* header = reinterpret_cast<HeadT*>(addr);
                new (header) HeadT(GCKind<OT>::Kind);
                obj = BasePtrToMostDerivedPtr<OT>(addr);
                new (obj) OT(std::forward<ARGS>(args)...);
            } while (!mps_commit(obj_ap,addr,size) );
            globalMpsMetrics.movingZeroRankAllocation(size);
            DEBUG_MPS_ALLOCATION("AMCZ", addr,obj,size,gctools::GCKind<OT>::Kind);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(obj);
            return sp;
#endif
        };
    };




    template <class OT>
    struct GCObjectAppropriatePoolAllocator<OT, /*Atomic=*/false, /*Moveable=*/false> {
        typedef OT                      value_type;
        typedef OT*                     pointer_type;
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        template <typename...ARGS>
        static smart_pointer_type allocateInAppropriatePool(ARGS&&...args)
        {
#ifdef USE_BOEHM
            // By default allocate in the normal pool for objects that contain pointers
            // to other objects.
            size_t sz = sizeof_with_header<OT>();
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC(sz));
            new (base) Header_s(typeid(OT).name(),BoehmLispKind);
            pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            return sp;
#endif
#ifdef USE_MPS
            typedef typename GCHeader<OT>::HeaderType HeadT;
            OT* obj;
            mps_ap_t obj_ap = global_non_moving_ap;
            mps_addr_t addr;
            size_t size = sizeof_with_header<OT>();
            do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
                mps_res_t res = mps_reserve(&addr,obj_ap,size);
#pragma clang diagnostic pop
                HeadT* header = reinterpret_cast<HeadT*>(addr);
                new (header) HeadT(GCKind<OT>::Kind);
                obj = BasePtrToMostDerivedPtr<OT>(addr);
                new (obj) OT(std::forward<ARGS>(args)...);
            } while (!mps_commit(obj_ap,addr,size) );
            globalMpsMetrics.nonMovingAllocation(size);
            DEBUG_MPS_ALLOCATION("NON_MOVING_POOL", addr,obj,size,gctools::GCKind<OT>::Kind);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(obj);
            return sp;
#endif
        };
    };


}


typedef void (*BoehmFinalizerFn)(void* obj, void* data);


namespace gctools {

#ifdef USE_BOEHM
    template <class OT>
    void BoehmFinalizer(void* base, void* data) {
        OT* obj = BasePtrToMostDerivedPtr<OT>(base);
//        printf("%s:%d Finalizing ptr=%p\n", __FILE__, __LINE__, obj);
        obj->~OT();
    }
#endif

    template <class OT, bool Needed=true>
    struct GCObjectFinalizer {
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        static void finalizeIfNeeded(smart_pointer_type sp)
        {
#ifdef USE_BOEHM
            void* dummyData;
            BoehmFinalizerFn dummyFn;
//            printf("%s:%d About to finalize sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
            GC_register_finalizer_ignore_self(SmartPtrToBasePtr(sp),
                                              BoehmFinalizer<OT>, NULL,
                                              &dummyFn, &dummyData);
//            printf("%s:%d Just completed finalize sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
#endif
#ifdef USE_MPS  
            void* client = sp.pbase(); // SmartPtrToBasePtr(sp);
            mps_finalize(_global_arena,&client);
            ++globalMpsMetrics.finalizationRequests;
#endif
        };
    };


    template <class OT>
    struct GCObjectFinalizer<OT,false> {
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        static void finalizeIfNeeded(smart_pointer_type sp)
        {
            // finalize not needed
        };
    };


}


namespace gctools {



    template <class OT>
    class GCObjectAllocator {
    public:
        typedef OT      value_type;
        typedef OT*     pointer_type;
        typedef gctools::smart_ptr<OT> smart_pointer_type;
    public:

        template <typename...ARGS>
        static smart_pointer_type rootAllocate(ARGS&&...args)
        {
#ifdef USE_BOEHM
            size_t sz = sizeof_with_header<OT>(); // USE HEADER FOR BOEHM ROOTS BUT NOT MPS
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC_UNCOLLECTABLE(sz));
            new (base) Header_s(typeid(OT).name(),BoehmLispKind);
            pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            GCObjectInitializer<OT,gctools::GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            GCObjectFinalizer<OT,gctools::GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
            return sp;
#endif
#ifdef USE_MPS
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT,GCInfo<OT>::Atomic,GCInfo<OT>::Moveable>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
            GCObjectInitializer<OT,GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            GCObjectFinalizer<OT,GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
//            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
            POLL_SIGNALS();
            return sp;
#endif
        };

        template <typename...ARGS>
        static smart_pointer_type allocate(ARGS&&...args)
        {
#ifdef USE_BOEHM
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT,gctools::GCInfo<OT>::Atomic>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
            GCObjectInitializer<OT,gctools::GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            GCObjectFinalizer<OT,gctools::GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
//            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
            POLL_SIGNALS();
            return sp;
#endif
#ifdef USE_MPS
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT,GCInfo<OT>::Atomic,GCInfo<OT>::Moveable>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
            GCObjectInitializer<OT,GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            GCObjectFinalizer<OT,GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
//            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
            POLL_SIGNALS();
            return sp;
#endif
        };


        static smart_pointer_type copy(const OT& that)
        {
#ifdef USE_BOEHM
            // Copied objects must be allocated in the appropriate pool
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT,gctools::GCInfo<OT>::Atomic>::allocateInAppropriatePool(that);
            // Copied objects are not initialized.
            // Copied objects are finalized if necessary
            GCObjectFinalizer<OT,gctools::GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
            return sp;
#endif
#ifdef USE_MPS
            // Copied objects must be allocated in the appropriate pool
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator<OT,GCInfo<OT>::Atomic,GCInfo<OT>::Moveable>::allocateInAppropriatePool(that);
            // Copied objects are not initialized.
            // Copied objects are finalized if necessary
            GCObjectFinalizer<OT,GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
            return sp;
#endif
        }


    };
};



namespace gctools {
    template <class TY>
    class GCContainerAllocator /* : public GCAlloc<TY> */ {
    public:

        // type definitions
        typedef TY                container_type;
        typedef container_type*   container_pointer;
        typedef typename container_type::value_type          value_type;
        typedef value_type*       pointer;
        typedef const value_type* const_pointer;
        typedef value_type&       reference;
        typedef const value_type& const_reference;
        typedef std::size_t      size_type;
        typedef std::ptrdiff_t   difference_type;

        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        GCContainerAllocator() throw() {}
        GCContainerAllocator(const GCContainerAllocator&) throw() {}
        template <class U>
        GCContainerAllocator (const GCContainerAllocator<U>&) throw() {}
        ~GCContainerAllocator() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size () const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        container_pointer allocate (size_type num, const void* = 0) {
#ifdef USE_BOEHM
            size_t sz = sizeof_container_with_header<TY>(num);
            // prepend a one pointer header with a pointer to the typeinfo.name
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC(sz));
            if (!base) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (base) Header_s(typeid(TY).name(),BoehmContainerKind);
            container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
            POLL_SIGNALS();
            return myAddress;
#endif
#ifdef USE_MPS
            typedef typename GCHeader<TY>::HeaderType HeadT;
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = sizeof_container_with_header<TY>(num);
            mps_ap_t obj_ap = _global_automatic_mostly_copying_allocation_point;
            do {
                mps_res_t res = mps_reserve(&addr,obj_ap,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCContainerAllocator_mps"));
                HeadT* header = reinterpret_cast<HeadT*>(addr);
                new (header) HeadT(GCKind<TY>::Kind);
                myAddress = (BasePtrToMostDerivedPtr<TY>(addr));
                new (myAddress) TY(num);
            } while (!mps_commit(obj_ap,addr,size));
            globalMpsMetrics.movingAllocation(size);
            GC_LOG(("malloc@%p %zu bytes\n",myAddress,newBytes));
            POLL_SIGNALS();
            DEBUG_MPS_ALLOCATION("containerAMC", addr, myAddress, size, gctools::GCKind<TY>::Kind);
            return myAddress;
#endif
        }


        // initialize elements of allocated storage p with value value
        template <typename...ARGS>
        void construct (pointer p, ARGS&&...args) {
            // initialize memory with placement new
            new((void*)p) value_type(std::forward<ARGS>(args)...);
        }

        // destroy elements of initialized storage p
        void destroy (pointer p) {
            // Do nothing
        }

        // deallocate storage p of deleted elements
        void deallocate (container_pointer p, size_type num) {
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
        typedef TY                container_type;
        typedef container_type*   container_pointer;
        typedef typename container_type::value_type          value_type;
        typedef value_type*       pointer;
        typedef const value_type* const_pointer;
        typedef value_type&       reference;
        typedef const value_type& const_reference;
        typedef std::size_t      size_type;
        typedef std::ptrdiff_t   difference_type;

        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        GCContainerNonMoveableAllocator() throw() {}
        GCContainerNonMoveableAllocator(const GCContainerNonMoveableAllocator&) throw() {}
        template <class U>
        GCContainerNonMoveableAllocator (const GCContainerNonMoveableAllocator<U>&) throw() {}
        ~GCContainerNonMoveableAllocator() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size () const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        container_pointer allocate (size_type num, const void* = 0) {
#ifdef USE_BOEHM
            size_t sz = sizeof_container_with_header<TY>(num);
            // prepend a one pointer header with a pointer to the typeinfo.name
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC(sz));
            if (!base) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (base) Header_s(typeid(TY).name(),BoehmContainerKind);
            container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
            POLL_SIGNALS();
            return myAddress;
#endif
#ifdef USE_MPS
            typedef typename GCHeader<TY>::HeaderType HeadT;
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = sizeof_container_with_header<TY>(num);
            mps_ap_t obj_ap = global_non_moving_ap;
            do {
                mps_res_t res = mps_reserve(&addr,obj_ap,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCContainerNonMoveableAllocator_mps"));
                HeadT* header = reinterpret_cast<HeadT*>(addr);
                new (header) HeadT(GCKind<TY>::Kind);
                myAddress = (BasePtrToMostDerivedPtr<TY>(addr));
                new (myAddress) TY(num);
            } while (!mps_commit(obj_ap,addr,size));
            globalMpsMetrics.nonMovingAllocation(size);
            GC_LOG(("malloc@%p %zu bytes\n",myAddress,newBytes));
            POLL_SIGNALS();
            DEBUG_MPS_ALLOCATION("container_MVFF", addr, myAddress, size, gctools::GCKind<TY>::Kind);
            return myAddress;
#endif
        }


        // initialize elements of allocated storage p with value value
        template <typename...ARGS>
        void construct (pointer p, ARGS&&...args) {
            // initialize memory with placement new
            new((void*)p) value_type(std::forward<ARGS>(args)...);
        }

        // destroy elements of initialized storage p
        void destroy (pointer p) {
            // Do nothing
        }

        // deallocate storage p of deleted elements
        void deallocate (container_pointer p, size_type num) {
            // Do nothing
        }
    };
};








namespace gctools {
    template <class TY>
    class GCStringAllocator /* : public GCAlloc<TY> */ {
    public:

        // type definitions
        typedef TY                container_type;
        typedef container_type*   container_pointer;
        typedef typename container_type::value_type          value_type;
        typedef value_type*       pointer;
        typedef const value_type* const_pointer;
        typedef value_type&       reference;
        typedef const value_type& const_reference;
        typedef std::size_t      size_type;
        typedef std::ptrdiff_t   difference_type;

        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        GCStringAllocator() throw() {}
        GCStringAllocator(const GCStringAllocator&) throw() {}
        template <class U>
        GCStringAllocator(const GCStringAllocator<U>&) throw() {}
        ~GCStringAllocator() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size() const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        container_pointer allocate(size_type num, const void* = 0) {
#if defined(USE_BOEHM)
            size_t sz = sizeof_container_with_header<container_type>(num);
            // prepend a one pointer header with a pointer to the typeinfo.name
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC_ATOMIC(sz));
            if (!base) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (base) Header_s(typeid(TY).name(),BoehmStringKind);
            container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
            new (myAddress) TY(num);
            POLL_SIGNALS();
            return myAddress;
#endif
#if defined(USE_MPS)
            size_t sz = sizeof_container_with_header<container_type>(num);
            typedef typename GCHeader<TY>::HeaderType HeadT;
            mps_ap_t obj_ap = _global_automatic_mostly_copying_zero_rank_allocation_point;
            mps_addr_t base;
            do {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
                mps_res_t res = mps_reserve(&base,obj_ap,sz);
#pragma clang diagnostic pop
                HeadT* header = reinterpret_cast<HeadT*>(base);
                new (header) HeadT(GCKind<TY>::Kind);
//                header->kind._Kind = gctools::GCKind<container_type>::Kind;
            } while (!mps_commit(obj_ap,base,sz) );
            globalMpsMetrics.movingZeroRankAllocation(sz);
            container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
            new (myAddress) TY(num);
            POLL_SIGNALS();
            DEBUG_MPS_ALLOCATION("string_AMCZ", base, myAddress, sz, gctools::GCKind<TY>::Kind);
            return myAddress;
#endif
        }

        void deallocate(container_pointer p, size_type num) {
            // Do nothing
        }
    };
};









namespace gctools {

#ifdef USE_BOEHM
    inline void BoehmWeakLinkDebugFinalizer(void* base, void* data) {
        printf("%s:%d Boehm finalized weak linked address %p at %p\n", __FILE__, __LINE__, base, data );
    }
#endif




    struct WeakLinks {};
    struct StrongLinks {};

    template <class KT, class VT, class LT>
    struct Buckets;

    template <class TY>
    class GCBucketAllocator /* : public GCAlloc<TY> */ {};

    template <class VT>
    class GCBucketAllocator<Buckets<VT,VT,WeakLinks>> {
    public:
        typedef Buckets<VT,VT,WeakLinks>       TY;
        typedef TY                container_type;
        typedef container_type*   container_pointer;
        typedef typename container_type::value_type          value_type;
        typedef value_type*       pointer;
        typedef const value_type* const_pointer;
        typedef value_type&       reference;
        typedef const value_type& const_reference;
        typedef std::size_t      size_type;
        typedef std::ptrdiff_t   difference_type;

        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        GCBucketAllocator() throw() {}
        GCBucketAllocator(const GCBucketAllocator&) throw() {}
        ~GCBucketAllocator() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size () const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        static container_pointer allocate (size_type num, const void* = 0) {
#ifdef USE_BOEHM
            printf("%s:%d Allocating Bucket with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__ );
            size_t newBytes = sizeof_container<container_type>(num); // NO HEADER FOR BUCKETS
            container_pointer myAddress = (container_pointer)GC_MALLOC_ATOMIC(newBytes);
            if (!myAddress) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (myAddress) container_type(num);
            printf("%s:%d Check if Buckets has been initialized to unbound\n", __FILE__, __LINE__ );
            return myAddress;
#endif
#ifdef USE_MPS
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = sizeof_container<container_type>(num);
            do {
                mps_res_t res = mps_reserve(&addr,_global_weak_link_allocation_point,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCBucketsAllocator_mps"));
                container_pointer myAddress = reinterpret_cast<container_pointer>(addr);
                new (myAddress) container_type(num);
            }
            while (!mps_commit(_global_weak_link_allocation_point,addr,size));
            GC_LOG(("malloc@%p %zu bytes\n",myAddress,newBytes));
            return myAddress;
#endif
        }

        // initialize elements of allocated storage p with value value
        template <typename...ARGS>
        void construct (pointer p, ARGS&&...args) {
            // initialize memory with placement new
            THROW_HARD_ERROR(BF("What do I do here"));
//            new((void*)p)value_type(std::forward<ARGS>(args)...);
        }

        // destroy elements of initialized storage p
        void destroy (pointer p) {
            // Do nothing
        }

        // deallocate storage p of deleted elements
        void deallocate (container_pointer p, size_type num) {
            // Do nothing
        }
    };


    //
    // Specialize for strong links
    //
    template <class VT>
    class GCBucketAllocator<Buckets<VT,VT,StrongLinks>> {
    public:
        typedef Buckets<VT,VT,StrongLinks>       TY;
        typedef TY                container_type;
        typedef container_type*   container_pointer;
        typedef typename container_type::value_type          value_type;
        typedef value_type*       pointer;
        typedef const value_type* const_pointer;
        typedef value_type&       reference;
        typedef const value_type& const_reference;
        typedef std::size_t      size_type;
        typedef std::ptrdiff_t   difference_type;

        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        GCBucketAllocator() throw() {}
        GCBucketAllocator(const GCBucketAllocator&) throw() {}
        ~GCBucketAllocator() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size () const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        static container_pointer allocate (size_type num, const void* = 0) {
#ifdef USE_BOEHM
            printf("%s:%d Allocating Bucket with GC_MALLOC\n", __FILE__, __LINE__ );
            size_t newBytes = sizeof_container<container_type>(num); // NO HEADER FOR BUCKETS
            container_pointer myAddress = (container_pointer)GC_MALLOC(newBytes);
            if (!myAddress) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (myAddress) container_type(num);
            return myAddress;
#endif
#ifdef USE_MPS
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = sizeof_container<container_type>(num);
            do {
                mps_res_t res = mps_reserve(&addr,_global_strong_link_allocation_point,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCBucketsAllocator_mps"));
                container_pointer myAddress = reinterpret_cast<container_pointer>(addr);
                new (myAddress) container_type(num);
            }
            while (!mps_commit(_global_strong_link_allocation_point,addr,size));
            GC_LOG(("malloc@%p %zu bytes\n",myAddress,newBytes));
            return myAddress;
#endif
        }

        // initialize elements of allocated storage p with value value
        template <typename...ARGS>
        void construct (pointer p, ARGS&&...args) {
            // initialize memory with placement new
            THROW_HARD_ERROR(BF("What do I do here"));
//            new((void*)p)value_type(std::forward<ARGS>(args)...);
        }

        // destroy elements of initialized storage p
        void destroy (pointer p) {
            // Do nothing
        }

        // deallocate storage p of deleted elements
        void deallocate (container_pointer p, size_type num) {
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
    class GCMappingAllocator<Mapping<VT,VT,WeakLinks>> {
    public:
        typedef Mapping<VT,VT,WeakLinks>       TY;
        typedef TY                container_type;
        typedef TY*             container_pointer;
        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        GCMappingAllocator() throw() {}
        GCMappingAllocator(const GCMappingAllocator&) throw() {}
        ~GCMappingAllocator() throw() {}


        // allocate but don't initialize num elements of type value_type
        static container_pointer allocate (const VT& val) {
#ifdef USE_BOEHM
            printf("%s:%d Allocating Mapping with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__ );
            size_t newBytes = sizeof(container_type);
            container_pointer myAddress = (container_pointer)GC_MALLOC_ATOMIC(newBytes);
            if (!myAddress) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (myAddress) container_type(val);
            printf("%s:%d Check if Mapping has been initialized to unbound\n", __FILE__, __LINE__ );
            return myAddress;
#endif
#ifdef USE_MPS
            typedef typename GCHeader<TY>::HeaderType HeadT;
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = sizeof(container_type);
            do {
                mps_res_t res = mps_reserve(&addr,_global_weak_link_allocation_point,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCMappingAllocator_mps"));
                myAddress = reinterpret_cast<container_pointer>(addr);
                new (myAddress) container_type(val);
            }
            while (!mps_commit(_global_weak_link_allocation_point,addr,size));
            GC_LOG(("malloc@%p %zu bytes\n",myAddress,newBytes));
            return myAddress;
#endif
        }

    };



    template <class VT>
    class GCMappingAllocator<Mapping<VT,VT,StrongLinks>> {
    public:
        typedef Mapping<VT,VT,StrongLinks>       TY;
        typedef TY                container_type;
        typedef TY*             container_pointer;
        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        GCMappingAllocator() throw() {}
        GCMappingAllocator(const GCMappingAllocator&) throw() {}
        ~GCMappingAllocator() throw() {}


        // allocate but don't initialize num elements of type value_type
        static container_pointer allocate (const VT& val) {
#ifdef USE_BOEHM
            printf("%s:%d Allocating Mapping with GC_MALLOC\n", __FILE__, __LINE__ );
            size_t newBytes = sizeof(container_type);
            container_pointer myAddress = (container_pointer)GC_MALLOC(newBytes);
            if (!myAddress) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (myAddress) container_type(val);
            printf("%s:%d Check if Mapping has been initialized to unbound\n", __FILE__, __LINE__ );
            return myAddress;
#endif
#ifdef USE_MPS
            typedef typename GCHeader<TY>::HeaderType HeadT;
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = sizeof(container_type);
            do {
                mps_res_t res = mps_reserve(&addr,_global_strong_link_allocation_point,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCMappingAllocator_mps"));
                myAddress = reinterpret_cast<container_pointer>(addr);
                new (myAddress) container_type(val);
            }
            while (!mps_commit(_global_weak_link_allocation_point,addr,size));
            GC_LOG(("malloc@%p %zu bytes\n",myAddress,newBytes));
            return myAddress;
#endif
        }

    };




    template <class VT>
    class GCWeakPointerAllocator {
    public:
        typedef VT* value_pointer;
        typedef typename VT::value_type contained_type;
        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        GCWeakPointerAllocator() throw() {}
        GCWeakPointerAllocator(const GCWeakPointerAllocator&) throw() {}
        ~GCWeakPointerAllocator() throw() {}

        // allocate but don't initialize num elements of type value_type
        static value_pointer allocate (const contained_type& val) {
#ifdef USE_BOEHM
            printf("%s:%d Allocating WeakPointer with GC_MALLOC_ATOMIC\n", __FILE__, __LINE__ );
            size_t newBytes = sizeof(VT);
            value_pointer myAddress = (value_pointer)GC_MALLOC_ATOMIC(newBytes);
            if (!myAddress) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (myAddress) VT(val);
            return myAddress;
#endif
#ifdef USE_MPS
            mps_addr_t  addr;
            size_t size = sizeof(VT);
            value_pointer myAddress;
            do {
                mps_res_t res = mps_reserve(&addr,_global_weak_link_allocation_point,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCWeakPointerAllocator_mps"));
                myAddress = reinterpret_cast<value_pointer>(addr);
                new (myAddress) VT(val);
            } while (!mps_commit(_global_weak_link_allocation_point,addr,size));
            return myAddress;
#endif
        }
    };




};
    


#endif // USE_BOEHM || USE_MPS






#endif
