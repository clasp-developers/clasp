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






#ifdef USE_REFCOUNT
#define REFCOUNT_DEBUG_HEADER

namespace gctools {
    template <class T>
    class root_allocator : public std::allocator<T> {};
};







namespace gctools {
    /*! Allocate regular C++ classes that are considered roots */
    template <class T>
    T* allocateRootClass()
    {
        return new T();
    }
};



namespace gctools {
    /*! Allocate regular C++ classes that are considered roots */
    template <class T,class...ARGS>
    T* allocateRootClass(ARGS&&...args)
    {
        T* base = new T(std::forward<ARGS>(args)...);
        return base;
    }


    /*! Allocate regular C++ classes that will be garbage collected as soon as nothing points to them */
    template <class T,class...ARGS>
    T* allocateClass(ARGS&&...args)
    {
        T* base = new T(std::forward<ARGS>(args)...);
        return base;
    }

    /*! Allocate regular C++ classes that will be garbage collected as soon as nothing points to them */
    template <class T>
    void deallocateClass(T* ptr)
    {
        delete ptr;
    }
};



namespace gctools {

    template <class OT>
    class GCObjectAllocator_refcount {
    public:
        typedef OT      value_type;
        typedef OT*     pointer_type;
        typedef gctools::smart_ptr<OT> smart_pointer_type;
    public:

        template <typename...ARGS>
        static smart_pointer_type rootAllocate(ARGS&&...args)
        {
#ifdef REFCOUNT_DEBUG_HEADER
            char** base = reinterpret_cast<char**>(malloc(sizeof(OT)+sizeof(char*)));
            *base = const_cast<char*>(typeid(OT).name());
            pointer_type ptr = reinterpret_cast<OT*>(base+1);
            new (ptr) OT(std::forward<ARGS>(args)...);
//            printf("%s:%d rootAllocate base@%p   typeid().name() = %s\n", __FILE__, __LINE__, base, typeid(OT).name() );
#else
            pointer_type ptr = new OT(std::forward<ARGS>(args)...);
#endif
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            GCObjectInitializer<OT,gctools::GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            return sp;
        };

        template <typename...ARGS>
        static smart_pointer_type allocate(ARGS&&...args)
        {
#ifdef REFCOUNT_DEBUG_HEADER
            char** base = reinterpret_cast<char**>(malloc(sizeof(OT)+sizeof(char*)));
            *base = const_cast<char*>(typeid(OT).name());
            pointer_type ptr = reinterpret_cast<OT*>(base+1);
            new (ptr) OT(std::forward<ARGS>(args)...);
//            printf("%s:%d allocate base@%p   typeid().name() = %s\n", __FILE__, __LINE__, base, typeid(OT).name() );
#else
            pointer_type ptr = new OT(std::forward<ARGS>(args)...);
#endif
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            GCObjectInitializer<OT,gctools::GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            return sp;
        };


        static smart_pointer_type copy(const OT& that)
        {
#ifdef REFCOUNT_DEBUG_HEADER
            char** base = reinterpret_cast<char**>(malloc(sizeof(OT)+sizeof(char*)));
            *base = const_cast<char*>(typeid(OT).name());
            pointer_type ptr = reinterpret_cast<OT*>(base+1);
            new (ptr) OT(that);
//            printf("%s:%d copy base@%p   typeid().name() = %s\n", __FILE__, __LINE__, base, typeid(OT).name() );
#else
            pointer_type ptr = new OT(that);
#endif
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            return sp;
        }



        static void deallocate(OT* thatP)
        {
#ifdef REFCOUNT_DEBUG_HEADER
            void* mostDerived = dynamic_cast<void*>(thatP);
            char** base = reinterpret_cast<char**>(mostDerived);
            --base;
//            printf("%s:%d deallocate base@%p thatP@%p mostDerived@%p type=%s\n", __FILE__, __LINE__, base, thatP, mostDerived, *base);
            free(base);
#else
            delete thatP;
#endif
        };

    };
};



namespace gctools {
    template <class OT>
    class GCObjectAllocator : public GCObjectAllocator_refcount<OT> {};
};





namespace gctools {
    template <class TY>
    class GCContainerAllocator_refcount /* : public GCAlloc<TY> */ {
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
        GCContainerAllocator_refcount() throw() {}
        GCContainerAllocator_refcount(const GCContainerAllocator_refcount&) throw() {}
        template <class U>
        GCContainerAllocator_refcount (const GCContainerAllocator_refcount<U>&) throw() {}
        ~GCContainerAllocator_refcount() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size () const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        container_pointer allocate (size_type num, const void* = 0) {
            size_t newBytes = sizeof_container<container_type>(num);
            container_pointer myAddress = (container_pointer)malloc(newBytes);
//           pointer ret = (pointer)(::operator new(num*sizeof(value_type)));
            
            if (!myAddress) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            GC_LOG(("malloc@%p %lu bytes\n",myAddress,newBytes));
            return myAddress;
        }


        // initialize elements of allocated storage p with value value
        template <typename...ARGS>
        void construct (pointer p, ARGS&&...args) {
            // initialize memory with placement new
            new((void*)p)value_type(args...);
        }

        // destroy elements of initialized storage p
        void destroy (pointer p) {
            // destroy objects by calling their destructor
            p->~value_type();
        }

        // deallocate storage p of deleted elements
        void deallocate (container_pointer p, size_type num) {
            GC_LOG(("free@%p\n", p));
            // Destroy all managed contents
//            p->_End = 0; // Abandon every element in this vector and then destroy them
            for ( size_t i(0); i<num; ++i ) {
                this->destroy(&((*p)[i]));
            }
            free(p);
        }
    };
};

#endif



#ifdef USE_BOEHM


namespace gctools {
    template <class T>
    class root_allocator : public traceable_allocator<T> {};
};



namespace gctools {


    /*! Allocate regular C++ classes that are considered roots */
    template <class T,class...ARGS>
    T* allocateRootClass(ARGS&&...args)
    {
        size_t sz = sizeof_with_header<T>();
        Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC_UNCOLLECTABLE(sz));
        new (base) Header_s(const_cast<char*>(typeid(T).name()),BoehmClassKind);
        T* obj = BasePtrToMostDerivedPtr<T>(base);
        new (obj) T(std::forward<ARGS>(args)...);
        POLL_SIGNALS();
        return obj;
    }


    template <class T>
    struct ClassAllocator {
        /*! Allocate regular C++ classes that will be garbage collected as soon as nothing points to them */
        template <class...ARGS>
        static T* allocateClass(ARGS&&...args)
        {
            size_t sz = sizeof_with_header<T>();
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC(sz));
            new (base) Header_s(typeid(T).name(),BoehmClassKind);
            T* obj = BasePtrToMostDerivedPtr<T>(base);
            new (obj) T(std::forward<ARGS>(args)...);
            POLL_SIGNALS();
            return obj;
        }

        /*! Allocate regular C++ classes that will be garbage collected as soon as nothing points to them */
        static void deallocateClass(T* ptr)
        {
            // BOEHM and MPS do nothing
        }
    };
};


namespace gctools {

    template <class OT, bool Atomic=false>
    struct GCObjectAppropriatePoolAllocator_boehm {
        typedef OT                      value_type;
        typedef OT*                     pointer_type;
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        template <typename...ARGS>
        static smart_pointer_type allocateInAppropriatePool(ARGS&&...args)
        {
            // By default allocate in the normal pool for objects that contain pointers
            // to other objects.
            size_t sz = sizeof_with_header<OT>();
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC(sz));
            new (base) Header_s(typeid(OT).name(),BoehmLispKind);
            pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            return sp;
        };
    };


    template <class OT>
    struct GCObjectAppropriatePoolAllocator_boehm<OT, /*Atomic=*/ true> {
        typedef OT                      value_type;
        typedef OT*                     pointer_type;
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        template <typename...ARGS>
        static smart_pointer_type allocateInAppropriatePool(ARGS&&...args)
        {
            // Atomic objects (do not contain pointers) are allocated in separate pool
            size_t sz = sizeof_with_header<OT>();
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC_ATOMIC(sz));
            new (base) Header_s(typeid(OT).name(),BoehmLispKind);
            pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            return sp;
        };
    };


}


typedef void (*BoehmFinalizerFn)(void* obj, void* data);


namespace gctools {


    template <class OT>
    void BoehmFinalizer(void* base, void* data) {
        OT* obj = BasePtrToMostDerivedPtr<OT>(base);
//        printf("%s:%d Finalizing ptr=%p\n", __FILE__, __LINE__, obj);
        obj->~OT();
    }

    template <class OT, bool Needed=true>
    struct GCObjectFinalizer_boehm {
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        static void finalizeIfNeeded(smart_pointer_type sp)
        {
            void* dummyData;
            BoehmFinalizerFn dummyFn;
//            printf("%s:%d About to finalize sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
            GC_register_finalizer_ignore_self(SmartPtrToBasePtr(sp),
                                              BoehmFinalizer<OT>, NULL,
                                              &dummyFn, &dummyData);
//            printf("%s:%d Just completed finalize sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
        };
    };


    template <class OT>
    struct GCObjectFinalizer_boehm<OT,false> {
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        static void finalizeIfNeeded(smart_pointer_type sp)
        {
            // finalize not needed
        };
    };


}


namespace gctools {



    template <class OT>
    class GCObjectAllocator_boehm {
    public:
        typedef OT      value_type;
        typedef OT*     pointer_type;
        typedef gctools::smart_ptr<OT> smart_pointer_type;
    public:

        template <typename...ARGS>
        static smart_pointer_type rootAllocate(ARGS&&...args)
        {
            size_t sz = sizeof_with_header<OT>(); // USE HEADER FOR BOEHM ROOTS BUT NOT MPS
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC_UNCOLLECTABLE(sz));
            new (base) Header_s(typeid(OT).name(),BoehmLispKind);
            pointer_type ptr = BasePtrToMostDerivedPtr<OT>(base);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
//            printf("%s:%d In rootAllocate sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
            GCObjectInitializer<OT,gctools::GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
//            printf("%s:%d In rootAllocate sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
            GCObjectFinalizer_boehm<OT,gctools::GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
//            printf("%s:%d In rootAllocate sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
//            printf("%s:%d In rootAllocate ptr = %p\n", __FILE__, __LINE__, ptr );
            return sp;
        };

        template <typename...ARGS>
        static smart_pointer_type allocate(ARGS&&...args)
        {
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator_boehm<OT,gctools::GCInfo<OT>::Atomic>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
            GCObjectInitializer<OT,gctools::GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            GCObjectFinalizer_boehm<OT,gctools::GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
//            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
            POLL_SIGNALS();
            return sp;
        };


        static smart_pointer_type copy(const OT& that)
        {
            // Copied objects must be allocated in the appropriate pool
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator_boehm<OT,gctools::GCInfo<OT>::Atomic>::allocateInAppropriatePool(that);
            // Copied objects are not initialized.
            // Copied objects are finalized if necessary
            GCObjectFinalizer_boehm<OT,gctools::GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
            return sp;
        }


    };
};


namespace gctools {
    template <class OT>
    class GCObjectAllocator : public GCObjectAllocator_boehm<OT> {};
};


namespace gctools {
    template <class TY>
    class GCContainerAllocator_boehm /* : public GCAlloc<TY> */ {
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
        GCContainerAllocator_boehm() throw() {}
        GCContainerAllocator_boehm(const GCContainerAllocator_boehm&) throw() {}
        template <class U>
        GCContainerAllocator_boehm (const GCContainerAllocator_boehm<U>&) throw() {}
        ~GCContainerAllocator_boehm() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size () const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        container_pointer allocate (size_type num, const void* = 0) {
            size_t sz = sizeof_container_with_header<container_type>(num);
            // prepend a one pointer header with a pointer to the typeinfo.name
            Header_s* base = reinterpret_cast<Header_s*>(GC_MALLOC(sz));
#if 0
            if ( sz > 100000 ) {
                printf("%s:%d Allocating large container containing %lu bytes at address: %p\n", __FILE__, __LINE__, sz, base );
            }
#endif
            if (!base) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (base) Header_s(typeid(TY).name(),BoehmContainerKind);
            container_pointer myAddress = BasePtrToMostDerivedPtr<TY>(base);
            POLL_SIGNALS();
            return myAddress;
        }


        // initialize elements of allocated storage p with value value
        template <typename...ARGS>
        void construct (pointer p, ARGS&&...args) {
            // initialize memory with placement new
            new((void*)p) value_type(args...);
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

    struct WeakLinks {};
    struct StrongLinks {};

    template <class TY, class LinkType >
    class GCBucketAllocator_boehm /* : public GCAlloc<TY> */ {
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
        GCBucketAllocator_boehm() throw() {}
        GCBucketAllocator_boehm(const GCBucketAllocator_boehm&) throw() {}
        template <class U>
        GCBucketAllocator_boehm (const GCBucketAllocator_boehm<U,LinkType>&) throw() {}
        ~GCBucketAllocator_boehm() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size () const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        static container_pointer allocate (size_type num, const void* = 0) {
            size_t newBytes = sizeof_container<container_type>(num); // NO HEADER FOR BUCKETS
            container_pointer myAddress = (container_pointer)GC_MALLOC(newBytes);
            if (!myAddress) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            new (myAddress) container_type(num);
            return myAddress;
        }

        /*! Register that this is a weak pointer
          On Boehm that means call GC_register_disappearing_link on it
          On MPS we don't do anything */
        static void weakLink(void** ptr)
        {
            int res = GC_register_disappearing_link(ptr);
            printf("%s:%d  Registered disappearing link %p  return val = %d\n", __FILE__, __LINE__, ptr, res );
        }


        // initialize elements of allocated storage p with value value
        template <typename...ARGS>
        void construct (pointer p, ARGS&&...args) {
            // initialize memory with placement new
            THROW_HARD_ERROR(BF("What do I do here"));
//            new((void*)p)value_type(args...);
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



    template <class TY, class Strength>
    class GCBucketAllocator : public GCBucketAllocator_boehm<TY,Strength> /* : public GCAlloc<TY> */ {
    };


};
    


#endif // USE_BOEHM


// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

#ifdef USE_MPS

namespace gctools {

    template <class T, class...ARGS>
    T* allocateRootClass(ARGS&&...args)
    {
        T* base = new T(std::forward<ARGS>(args)...);
        return base;
    }

    template <class T>
    struct ClassAllocator {
        template <class...ARGS>
        static T* allocateClass(ARGS&&...args)
        {
            T* obj;
            mps_ap_t obj_ap = gctools::allocation_point<T>::get();
            mps_addr_t addr;
            size_t size = sizeof_with_header<T>();
            do {
                mps_res_t res = mps_reserve(&addr,obj_ap,size);
                gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(addr);
                new (header) gctools::Header_s(gctools::GCKind<T>::Kind);
                T* obj = BasePtrToMostDerivedPtr<T>(addr);
                new (obj) T(std::forward<ARGS>(args)...);
            } while (!mps_commit(obj_ap,addr,size) );
            POLL_SIGNALS();
            return obj;
        }

        /*! Allocate regular C++ classes that will be garbage collected as soon as nothing points to them */
        static void deallocateClass(T* ptr)
        {
            // BOEHM and MPS do nothing
        }

    };


        

};


namespace gctools {

    template <class OT, bool Atomic=false>
    struct GCObjectAppropriatePoolAllocator_mps {
        typedef OT                      value_type;
        typedef OT*                     pointer_type;
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        template <typename...ARGS>
        static smart_pointer_type allocateInAppropriatePool(ARGS&&...args)
        {
            OT* obj;
            mps_ap_t obj_ap = _global_automatic_mostly_copying_allocation_point;
            mps_addr_t addr;
            size_t size = sizeof_with_header<OT>();
            do {
                mps_res_t res = mps_reserve(&addr,obj_ap,size);
                gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(addr);
                new (header) gctools::Header_s(gctools::GCKind<OT>::Kind);
                OT* obj = BasePtrToMostDerivedPtr<OT>(addr);
                new (obj) OT(std::forward<ARGS>(args)...);
            } while (!mps_commit(obj_ap,addr,size) );
            return obj;
        };
    };


    template <class OT>
    struct GCObjectAppropriatePoolAllocator_mps<OT, /*Atomic=*/ true> {
        typedef OT                      value_type;
        typedef OT*                     pointer_type;
        typedef gctools::smart_ptr<OT>      smart_pointer_type;
        template <typename...ARGS>
        static smart_pointer_type allocateInAppropriatePool(ARGS&&...args)
        {
            OT* obj;
            mps_ap_t obj_ap = _global_automatic_mostly_copying_zero_rank_allocation_point;
            mps_addr_t addr;
            size_t size = sizeof_with_header<OT>();
            do {
                mps_res_t res = mps_reserve(&addr,obj_ap,size);
                gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(addr);
                new (header) gctools::Header_s(gctools::GCKind<OT>::Kind);
                OT* obj = BasePtrToMostDerivedPtr<OT>(addr);
                new (obj) OT(std::forward<ARGS>(args)...);
            } while (!mps_commit(obj_ap,addr,size) );
            return obj;
        };


    };
}




namespace gctools {


    template <class OT, bool Needed=true>
    struct GCObjectFinalizer_mps {
        typedef smart_ptr<OT>      smart_pointer_type;
        static void finalizeIfNeeded(smart_pointer_type sp)
        {
            void* base = SmartPtrToBasePtr(sp);
            mps_finalize(_global_arena,&base);
        };
    };


    template <class OT>
    struct GCObjectFinalizer_mps<OT,false> {
        typedef smart_ptr<OT>      smart_pointer_type;
        static void finalizeIfNeeded(smart_pointer_type sp)
        {
            // finalize not needed
        };
    };


}



namespace gctools {

    template <class OT>
    class GCObjectAllocator_mps {
    public:
        typedef OT      value_type;
        typedef OT*     pointer_type;
        typedef smart_ptr<OT> smart_pointer_type;
    public:

        template <typename...ARGS>
        static smart_pointer_type rootAllocate(ARGS&&...args)
        {
            THROW_HARD_ERROR(BF("Implement me"));
#if 0
            char** base = reinterpret_cast<char**>(GC_MALLOC_UNCOLLECTABLE(sizeof(OT)+sizeof(char*)));
            *base = const_cast<char*>(typeid(OT).name());
            pointer_type ptr = reinterpret_cast<OT*>(base+1);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = gctools::smart_ptr<value_type>(ptr);
            GCObjectInitializer<OT,gctools::GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            GCObjectFinalizer_mps<OT,gctools::GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
            return sp;
#endif
        };

        template <typename...ARGS>
        static smart_pointer_type allocate(ARGS&&...args)
        {
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator_mps<OT,GCInfo<OT>::Atomic>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
            GCObjectInitializer<OT,GCInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            GCObjectFinalizer_mps<OT,GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
//            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
            POLL_SIGNALS();
            return sp;
        };


        static smart_pointer_type copy(const OT& that)
        {
            // Copied objects must be allocated in the appropriate pool
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator_mps<OT,GCInfo<OT>::Atomic>::allocateInAppropriatePool(that);
            // Copied objects are not initialized.
            // Copied objects are finalized if necessary
            GCObjectFinalizer_mps<OT,GCInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
            return sp;
        }


    };
};


namespace gctools {
    template <class OT>
    class GCObjectAllocator : public GCObjectAllocator_mps<OT> {};
};



namespace gctools {
    //
    // Allocator for MPS
    //
    template <class TY>
    class GCContainerAllocator_mps /* : public GCAlloc<TY> */ {
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
        GCContainerAllocator_mps() throw() {}
        GCContainerAllocator_mps(const GCContainerAllocator_mps&) throw() {}
        template <class U>
        GCContainerAllocator_mps (const GCContainerAllocator_mps<U>&) throw() {}
        ~GCContainerAllocator_mps() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size () const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        container_pointer allocate (size_type num, const void* = 0) {
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = sizeof_container_with_header<container_type>(num);
            do {
                mps_res_t res = mps_reserve(&addr,_global_automatic_mostly_copying_allocation_point,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCContainerAllocator_mps"));
                Header_s* header = reinterpret_cast<Header_s*>(addr);
                new (header) Header_s(GCKind<TY>::Kind);
                myAddress = (BasePtrToMostDerivedPtr<container_type>(addr));
                new (myAddress) container_type(num);
            } while (!mps_commit(_global_automatic_mostly_copying_allocation_point,addr,size));
            GC_LOG(("malloc@%p %lu bytes\n",myAddress,newBytes));
            POLL_SIGNALS();
            return myAddress;
        }
        // initialize elements of allocated storage p with value value
        template <typename...ARGS>
        void construct (pointer p, ARGS&&...args) {
            // initialize memory with placement new
            new((void*)p)value_type(args...);
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

    struct WeakLinks {};
    struct StrongLinks {};

    template <class TY, class LinkType >
    class GCBucketAllocator_mps /* : public GCAlloc<TY> */ {
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
        GCBucketAllocator_mps() throw() {}
        GCBucketAllocator_mps(const GCBucketAllocator_mps&) throw() {}
        template <class U>
        GCBucketAllocator_mps (const GCBucketAllocator_mps<U,LinkType>&) throw() {}
        ~GCBucketAllocator_mps() throw() {}

        // return maximum number of elements that can be allocated
        size_type max_size () const throw() {
            return std::numeric_limits<std::size_t>::max() / sizeof(value_type);
        }

        // allocate but don't initialize num elements of type value_type
        static container_pointer allocate (size_type num, const void* = 0) {
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = sizeof_container_with_header<container_type>(num);
            do {
                mps_res_t res = mps_reserve(&addr,_global_automatic_weak_link_allocation_point,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCContainerAllocator_mps"));
                Header_s* header = reinterpret_cast<Header_s*>(addr);
                new (header) Header_s(GCKind<TY>::Kind);
                myAddress = BasePtrToMostDerivedPtr<container_type>(addr);
                new (myAddress) container_type(num);
            }
            while (!mps_commit(_global_automatic_weak_link_allocation_point,addr,size));
            GC_LOG(("malloc@%p %lu bytes\n",myAddress,newBytes));
            return myAddress;

        }

        /*! Register that this is a weak pointer
          On Mps that means call GC_register_disappearing_link on it
          On MPS we don't do anything */
        static void weakLink(void** ptr)
        {
            // Nothing needs to be done
        }


        // initialize elements of allocated storage p with value value
        template <typename...ARGS>
        void construct (pointer p, ARGS&&...args) {
            // initialize memory with placement new
            THROW_HARD_ERROR(BF("What do I do here"));
//            new((void*)p)value_type(args...);
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



    template <class TY, class Strength>
    class GCBucketAllocator : public GCBucketAllocator_mps<TY,Strength> /* : public GCAlloc<TY> */ {
    };


};



#endif // USE_MPS




#endif
