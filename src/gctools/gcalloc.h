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

#ifndef USE_MPS
    template <class TY>
    class GCAlloc_malloc /* : public GCAlloc<TY> */ {
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
        GCAlloc_malloc() throw() {}
        GCAlloc_malloc(const GCAlloc_malloc&) throw() {}
        template <class U>
        GCAlloc_malloc (const GCAlloc_malloc<U>&) throw() {}
        ~GCAlloc_malloc() throw() {}

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
#else
    //
    // Allocator for MPS
    //
    template <class TY>
    class GCAlloc_mps /* : public GCAlloc<TY> */ {
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
        GCAlloc_mps() throw() {}
        GCAlloc_mps(const GCAlloc_mps&) throw() {}
        template <class U>
        GCAlloc_mps (const GCAlloc_mps<U>&) throw() {}
        ~GCAlloc_mps() throw() {}

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
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCAlloc_mps"));
                gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(addr);
                DEBUG_MARK_BLOCK_START(header);
                header->kind._Kind = gctools::GCInfo<TY>::Kind;
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
                if ( res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Out of memory in GCAlloc_mps"));
                gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(addr);
                DEBUG_MARK_BLOCK_START(header);
                header->kind._Kind = gctools::GCInfo<TY>::Kind;
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
#endif // USE_MPS



#if 0
    // return that all specializations of this allocator are interchangeable
    template <class T1, class T2>
    bool operator== (const GCAlloc<T1>&,
                     const GCAlloc<T2>&) throw() {
        return true;
    }
    template <class T1, class T2>
    bool operator!= (const GCAlloc<T1>&,
                     const GCAlloc<T2>&) throw() {
        return false;
    }
#endif

}; // namespace gctools
#endif
