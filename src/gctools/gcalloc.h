#ifndef gc_gcalloc_H
#define gc_gcalloc_H


#include <limits>




namespace gctools {



    /*! Top of hierarchy class so that the static analyzer can identify subclasses */


    template <typename Cont_impl>
    size_t gc_sizeof(size_t n)
    {
        size_t headerSz = sizeof(Cont_impl);
        size_t dataSz = sizeof(typename Cont_impl::value_type)*n;
        size_t totalSz = headerSz+dataSz;
        GC_LOG(("headerSz[%lu] + ( value_size[%lu] * n[%lu] -> dataSz[%lu] ) --> totalSz[%lu]\n",
             headerSz, sizeof(typename Cont_impl::value_type), n, dataSz, totalSz));
        return totalSz;
    };

};

namespace gctools {
    template <class OT, bool Needed=true>
    struct GCObjectInitializer {
        typedef mem::smart_ptr<OT>      smart_pointer_type;
        static void initializeIfNeeded(smart_pointer_type sp)
        {
            sp->initialize();
        };
    };

    template <class OT>
    struct GCObjectInitializer<OT,false> {
        typedef mem::smart_ptr<OT>      smart_pointer_type;
        static void initializeIfNeeded(smart_pointer_type sp)
        {
            // initialize not needed
        };
    };
}






#ifdef USE_REFCOUNT

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

    template <class OT>
    class GCObjectAllocator_refcount {
    public:
        typedef OT      value_type;
        typedef OT*     pointer_type;
        typedef mem::smart_ptr<OT> smart_pointer_type;
    public:

        template <typename...ARGS>
        static smart_pointer_type rootAllocate(ARGS&&...args)
        {
            pointer_type ptr = new OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = mem::smart_ptr<value_type>(ptr);
            GCObjectInitializer<OT,gctools::GCAllocatorInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            return sp;
        };

        template <typename...ARGS>
        static smart_pointer_type allocate(ARGS&&...args)
        {
            pointer_type ptr = new OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = mem::smart_ptr<value_type>(ptr);
            GCObjectInitializer<OT,gctools::GCAllocatorInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            return sp;
        };


        static smart_pointer_type copy(const OT& that)
        {
            pointer_type ptr = new OT(that);
            smart_pointer_type sp = mem::smart_ptr<value_type>(ptr);
            return sp;
        }
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
            size_t newBytes = gc_sizeof<container_type>(num);
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
    template <class T>
    T* allocateRootClass()
    {
        T* base = reinterpret_cast<T*>(GC_MALLOC_UNCOLLECTABLE(sizeof(T)));
        new (base) T();
        return base;
    }
};







namespace gctools {

    template <class OT, bool Atomic=false>
    struct GCObjectAppropriatePoolAllocator_boehm {
        typedef OT                      value_type;
        typedef OT*                     pointer_type;
        typedef mem::smart_ptr<OT>      smart_pointer_type;
        template <typename...ARGS>
        static smart_pointer_type allocateInAppropriatePool(ARGS&&...args)
        {
            // By default allocate in the normal pool for objects that contain pointers
            // to other objects.
            char** base = reinterpret_cast<char**>(GC_MALLOC(sizeof(OT)+sizeof(char*)));
            *base = const_cast<char*>(typeid(OT).name());
            pointer_type ptr = reinterpret_cast<OT*>(base+1);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = mem::smart_ptr<value_type>(ptr);
            return sp;
        };
    };


    template <class OT>
    struct GCObjectAppropriatePoolAllocator_boehm<OT, /*Atomic=*/ true> {
        typedef OT                      value_type;
        typedef OT*                     pointer_type;
        typedef mem::smart_ptr<OT>      smart_pointer_type;
        template <typename...ARGS>
        static smart_pointer_type allocateInAppropriatePool(ARGS&&...args)
        {
            // Atomic objects (do not contain pointers) are allocated in separate pool
            char** base = reinterpret_cast<char**>(GC_MALLOC_ATOMIC(sizeof(OT)+sizeof(char*)));
            *base = const_cast<char*>(typeid(OT).name());
            pointer_type ptr = reinterpret_cast<OT*>(base+1);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = mem::smart_ptr<value_type>(ptr);
            return sp;
        };
    };


}


typedef void (*BoehmFinalizerFn)(void* obj, void* data);





namespace gctools {


    template <class OT>
    void BoehmFinalizer(void* optr, void* data) {
        OT* obj = reinterpret_cast<OT*>(optr);
//        printf("%s:%d Finalizing ptr=%p\n", __FILE__, __LINE__, obj);
        obj->~OT();
    }

    template <class OT, bool Needed=true>
    struct GCObjectFinalizer_boehm {
        typedef mem::smart_ptr<OT>      smart_pointer_type;
        static void finalizeIfNeeded(smart_pointer_type sp)
        {
            void* dummyData;
            BoehmFinalizerFn dummyFn;
//            printf("%s:%d About to finalize sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
            GC_register_finalizer_ignore_self(sp.px_ref(),
                                  BoehmFinalizer<OT>, NULL,
                                  &dummyFn, &dummyData);
//            printf("%s:%d Just completed finalize sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
        };
    };


    template <class OT>
    struct GCObjectFinalizer_boehm<OT,false> {
        typedef mem::smart_ptr<OT>      smart_pointer_type;
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
        typedef mem::smart_ptr<OT> smart_pointer_type;
    public:

        template <typename...ARGS>
        static smart_pointer_type rootAllocate(ARGS&&...args)
        {
            char** base = reinterpret_cast<char**>(GC_MALLOC_UNCOLLECTABLE(sizeof(OT)+sizeof(char*)));
            *base = const_cast<char*>(typeid(OT).name());
            pointer_type ptr = reinterpret_cast<OT*>(base+1);
            new (ptr) OT(std::forward<ARGS>(args)...);
            smart_pointer_type sp = mem::smart_ptr<value_type>(ptr);
//            printf("%s:%d In rootAllocate sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
            GCObjectInitializer<OT,gctools::GCAllocatorInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
//            printf("%s:%d In rootAllocate sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
            GCObjectFinalizer_boehm<OT,gctools::GCAllocatorInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
//            printf("%s:%d In rootAllocate sp@%p sp.px_ref()@%p\n", __FILE__, __LINE__, &sp, sp.px_ref());
//            printf("%s:%d In rootAllocate ptr = %p\n", __FILE__, __LINE__, ptr );
            return sp;
        };

        template <typename...ARGS>
        static smart_pointer_type allocate(ARGS&&...args)
        {
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator_boehm<OT,gctools::GCAllocatorInfo<OT>::Atomic>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
            GCObjectInitializer<OT,gctools::GCAllocatorInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            GCObjectFinalizer_boehm<OT,gctools::GCAllocatorInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
//            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
            return sp;
        };


        template <typename...ARGS>
        static smart_pointer_type leafAllocate(ARGS&&...args)
        {
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator_boehm<OT,gctools::GCAllocatorInfo<OT>::Atomic>::allocateInAppropriatePool(std::forward<ARGS>(args)...);
            GCObjectInitializer<OT,gctools::GCAllocatorInfo<OT>::NeedsInitialization>::initializeIfNeeded(sp);
            GCObjectFinalizer_boehm<OT,gctools::GCAllocatorInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
//            printf("%s:%d About to return allocate result ptr@%p\n", __FILE__, __LINE__, sp.px_ref());
            return sp;
        };





        static smart_pointer_type copy(const OT& that)
        {
            // Copied objects must be allocated in the appropriate pool
            smart_pointer_type sp = GCObjectAppropriatePoolAllocator_boehm<OT,gctools::GCAllocatorInfo<OT>::Atomic>::allocateInAppropriatePool(that);
            // Copied objects are not initialized.
            // Copied objects are finalized if necessary
            GCObjectFinalizer_boehm<OT,gctools::GCAllocatorInfo<OT>::NeedsFinalization>::finalizeIfNeeded(sp);
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
            size_t newBytes = gc_sizeof<container_type>(num);
#if 0
            container_pointer myAddress = (container_pointer)GC_MALLOC(newBytes);
            if (!myAddress) THROW_HARD_ERROR(BF("Out of memory in allocate"));
#else
            // prepend a one pointer header with a pointer to the typeinfo.name
            char** base = reinterpret_cast<char**>(GC_MALLOC(newBytes+sizeof(char*)));
            if (!base) THROW_HARD_ERROR(BF("Out of memory in allocate"));
            *base = const_cast<char*>(typeid(TY).name());
            container_pointer myAddress = reinterpret_cast<TY*>(base+1);
#endif
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
#ifdef USE_AMC_POOL
        container_pointer allocate (size_type num, const void* = 0) {
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = ALIGN_UP(gc_sizeof<container_type>(num))+ALIGN_UP(sizeof(gctools::Header_s));
            do {
                mps_res_t res = mps_reserve(&addr,_global_automatic_mostly_copying_allocation_point,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCContainerAllocator_mps"));
                gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(addr);
                header->kind._Kind = gctools::GCInfo<TY>::Kind;
                DEBUG_VALIDATE_HEADER(header);
                myAddress = reinterpret_cast<container_pointer>(BASE_TO_OBJ_PTR(addr));
                new (myAddress) container_type(num);
            }
            while (!mps_commit(_global_automatic_mostly_copying_allocation_point,addr,size));
            GC_LOG(("malloc@%p %lu bytes\n",myAddress,newBytes));
            return myAddress;
        }
#else // USE AMS pool
        container_pointer allocate (size_type num, const void* = 0) {
            mps_addr_t  addr;
            container_pointer myAddress(NULL);
            size_t size = ALIGN_UP(gc_sizeof<container_type>(num))+ALIGN_UP(sizeof(gctools::Header_s));
            do {
                mps_res_t res = mps_reserve(&addr,_global_automatic_mark_sweep_allocation_point,size);
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCContainerAllocator_mps"));
                gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(addr);
                header->kind._Kind = gctools::GCInfo<TY>::Kind;
                DEBUG_VALIDATE_HEADER(header);
                myAddress = reinterpret_cast<container_pointer>(BASE_TO_OBJ_PTR(addr));
                new (myAddress) container_type(num);
            }
            while (!mps_commit(_global_automatic_mark_sweep_allocation_point,addr,size));
            GC_LOG(("malloc@%p %lu bytes\n",myAddress,newBytes));
            return myAddress;
        }
#endif

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
#endif // USE_MPS




#endif
