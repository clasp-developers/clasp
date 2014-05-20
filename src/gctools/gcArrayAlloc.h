#ifndef gc_gcArrayAlloc_H
#define gc_gcArrayAlloc_H


#include <limits>



namespace gctools {



    /*! Top of hierarchy class so that the static analyzer can identify subclasses */



    template <class X>
    class GCArrayAlloc {
    public:
        // type definitions
        typedef X                 container_type;
        typedef container_type*   container_pointer;
        typedef typename container_type::value_type          value_type;
        typedef value_type*       pointer;
        typedef const value_type* const_pointer;
        typedef value_type&       reference;
        typedef const value_type& const_reference;
        typedef std::size_t      size_type;
        typedef std::ptrdiff_t   difference_type;

        // rebind allocator to type U
        template <class U>
        struct rebind {
            typedef GCArrayAlloc<U> other;
        };

        // return address of values
        pointer address (reference value) const {
            return &value;
        }
        const_pointer address (const_reference value) const {
            return &value;
        }


        /* constructors and destructor
         * - nothing to do because the allocator has no state
         */
        GCArrayAlloc() throw() {}
        GCArrayAlloc(const GCArrayAlloc&) throw() {}
        template <class U>
        GCArrayAlloc (const GCArrayAlloc<U>&) throw() {}
        ~GCArrayAlloc() throw() {}

    };



    template <class X>
    class GCArrayAlloc_malloc : public GCArrayAlloc<X> {
    public:

        // type definitions
        typedef X                 container_type;
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
        GCArrayAlloc_malloc() throw() {}
        GCArrayAlloc_malloc(const GCArrayAlloc_malloc&) throw() {}
        template <class U>
        GCArrayAlloc_malloc (const GCArrayAlloc_malloc<U>&) throw() {}
        ~GCArrayAlloc_malloc() throw() {}

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
            new (myAddress) container_type(num);
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
            size_t myEnd(p->_End);
            p->_End = 0; // Abandon every element in this vector and then destroy them
            for ( size_t i(0); i<myEnd; ++i ) {
                this->destroy(&((*p)[i]));
            }
            free(p);
        }
    };





    // return that all specializations of this allocator are interchangeable
    template <class T1, class T2>
    bool operator== (const GCArrayAlloc<T1>&,
                     const GCArrayAlloc<T2>&) throw() {
        return true;
    }
    template <class T1, class T2>
    bool operator!= (const GCArrayAlloc<T1>&,
                     const GCArrayAlloc<T2>&) throw() {
        return false;
    }


}; // namespace gctools
#endif
