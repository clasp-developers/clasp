#ifndef _TAGGED_PTR_HPP_INCLUDED
#define _TAGGED_PTR_HPP_INCLUDED

//
//  tagged_ptr.hpp
//
//  Modifed by Christian Schafmeister 2013 to add tagging using the 2 least significant bits
//  Copyright (c) 2001, 2002 Peter Dimov
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
//  See http://www.boost.org/libs/smart_ptr/intrusive_ptr.html for documentation.
//

#include <boost/config.hpp>
#include <boost/utility/binary.hpp>
#include <boost/assert.hpp>
#include <boost/detail/workaround.hpp>
#include <boost/smart_ptr/detail/sp_convertible.hpp>

#include <boost/config/no_tr1/functional.hpp>           // for std::less

#if !defined(BOOST_NO_IOSTREAM)
#if !defined(BOOST_NO_IOSFWD)
#include <iosfwd>               // for std::basic_ostream
#else
#include <ostream>
#endif
#endif


namespace gctools {

    // Dummy class to store tagged fixnums as tagged_ptr<Fixnum_ty>
    class Fixnum_ty {};



    template<class T> class tagged_ptr
    {
    private:

        typedef T* PointerType;
        typedef tagged_ptr this_type;

    public:
        static const uintptr_t tag_mask 	  = BOOST_BINARY(0011);
        static const uintptr_t ptr_tag        = BOOST_BINARY(0000); // xxx00 means ptr
        static const uintptr_t special_tag    = BOOST_BINARY(0001); // xxx01 means special val
        static const uintptr_t character_tag  = BOOST_BINARY(0010); // xxx10 means character
        static const uintptr_t fixnum_tag     = BOOST_BINARY(0011); // xxx11 means fixnum
        static const uintptr_t ptr_mask = ~tag_mask;
    public:
        /*! Special taged values */
        static const uintptr_t tagged_NULL 	  = BOOST_BINARY(00000)|special_tag;
        static const uintptr_t tagged_unbound = BOOST_BINARY(00100)|special_tag; // 0x05
        static const uintptr_t tagged_nil 	  = BOOST_BINARY(01000)|special_tag; // 0x09
        static const uintptr_t tagged_deleted = BOOST_BINARY(01100)|special_tag; // 0x0D - used by WeakHashTable


    public:

        typedef T element_type;

        tagged_ptr():
            px( 0 )
#ifdef USE_TAGGED_PTR_P0
            , pbase(0)
#endif
        {
        }


        tagged_ptr( const T * p, bool add_ref = true )
        {
            typedef typename std::remove_const<T>::type *no_const_T_ptr;
            this->px = const_cast<no_const_T_ptr>(p);
#ifdef USE_TAGGED_PTR_P0
            this->pbase = ClientPtrToBasePtr(dynamic_cast<void*>(const_cast<no_const_T_ptr>(this->px)));
#endif
            BOOST_ASSERT(p==0 || pointerp());
        }


        explicit tagged_ptr( int p ): px( reinterpret_cast<T*>((p<<2) | fixnum_tag)) {};


        explicit tagged_ptr(uintptr_t p) : px((T*)p)
        {
            // ASSERT that the p does not correspond to a pointer
            BOOST_ASSERT( (p&tag_mask)!=ptr_tag );
#ifdef USE_TAGGED_PTR_P0
            this->pbase = NULL;
#endif
        }




        template<class U>
            tagged_ptr( tagged_ptr<U> const & rhs )
        {
            if ( rhs.pointerp() ) {
                px = dynamic_cast<T*>(rhs.pxget());
#ifdef USE_TAGGED_PTR_P0
                pbase = ClientPtrToBasePtr(dynamic_cast<void*>(px));
#endif
            } else {
                uintptr_t upx = reinterpret_cast<uintptr_t>(rhs.pxget());
                px = (T*)upx;
#ifdef USE_TAGGED_PTR_P0
                pbase = NULL;
#endif
            }
        }


        tagged_ptr(tagged_ptr const & rhs): px( rhs.px )
#ifdef USE_TAGGED_PTR_P0
                                          ,pbase(rhs.pbase)
#endif
        {
        }

        ~tagged_ptr()
        {
        }

#if !defined(BOOST_NO_MEMBER_TEMPLATES) || defined(BOOST_MSVC6_MEMBER_TEMPLATES)

        template<class U> tagged_ptr & operator=(tagged_ptr<U> const & rhs)
        {
            this_type(rhs).swap(*this);
            return *this;
        }

#endif

// Move support

#if defined( BOOST_HAS_RVALUE_REFS )

        tagged_ptr(tagged_ptr && rhs): px( rhs.px )
        {
            rhs.px = 0;
#ifdef USE_TAGGED_PTR_P0
            rhs.pbase = 0;
#endif
        }

        tagged_ptr & operator=(tagged_ptr && rhs)
        {
            this_type( static_cast< tagged_ptr && >( rhs ) ).swap(*this);
            return *this;
        }

#endif

        tagged_ptr & operator=(tagged_ptr const & rhs)
        {
            this_type(rhs).swap(*this);
            return *this;
        }

        tagged_ptr & operator=(T * rhs)
        {
            this_type(rhs).swap(*this);
            return *this;
        }

        void reset()
        {
            this_type().swap( *this );
        }

        void reset( T * rhs )
        {
            this_type( rhs ).swap( *this );
        }


        /*! Return true if px contains a pointer */
        bool pointerp() const
        {
            return ((uintptr_t)(this->px)&tag_mask)==ptr_tag
                && ((uintptr_t)(this->px)&ptr_mask);
        };

        bool not_pointerp() const { return !this->pointerp();};
    
        /*! THROW exception if px is not a pointer */
        void assert_pointer() const { BOOST_ASSERT(this->pointerp());}

        uintptr_t tag() const { return reinterpret_cast<uintptr_t>(this->px)&tag_mask;};
        bool taggedp() const { return (this->px&&tag_mask);};

        bool _NULLp() const { return (uintptr_t)this->px == tagged_NULL;};

        bool deletedp() const { return (uintptr_t)this->px == tagged_deleted;};

        bool unboundp() const { return (uintptr_t)this->px == tagged_unbound;};
        bool notunboundp() const { return !this->unboundp();};


        bool nilp() const { return (uintptr_t)this->px == tagged_nil;};

        bool characterp() const { return ((reinterpret_cast<uintptr_t>(this->px)&tag_mask)==character_tag);};
        bool fixnump() const { return ((reinterpret_cast<uintptr_t>(this->px)&tag_mask)==fixnum_tag);};
        // Handle get_fixnum

        inline brclChar character() const { return ((reinterpret_cast<uintptr_t>(this->px)>>2));};
        inline Fixnum fixnum() const { return ((reinterpret_cast<uintptr_t>(this->px)>>2));};



        T * pxget() const
        {
            return px;
        }


        T * get() const
        {
            if ( LIKELY(px==0 || pointerp()) ) {return px;}
            if ( nilp() ) lisp_errorDereferencedNil();
            if ( unboundp() ) lisp_errorDereferencedUnbound();
            BOOST_ASSERT_MSG( !nilp(), "You tried to dereference NIL");
            BOOST_ASSERT_MSG( !unboundp(), "You tried to dereference UNBOUND");
//	nilp() and unboundp() no longer dereference to nil and unbound instances
//	if ( nilp() ) return intrusive_ptr_nil_instance<T>();
//	if ( unboundp() ) return intrusive_ptr_unbound_instance<T>();
            // handle more cases
            BOOST_ASSERT_MSG(false,"You tried to dereference something that wasn't a pointer");
            return NULL;
//        return px;
        }

        T & operator*() const
        {
            return *(this->get());
        }

        T * operator->() const
        {
            return this->get();
        }

// implicit conversion to "bool"
#include "tagged_operator_bool.h"
//<boost/smart_ptr/detail/operator_bool.hpp>

        void swap(tagged_ptr & rhs)
        {
            T * tmp = this->px;
            this->px = rhs.px;
            rhs.px = tmp;
#ifdef USE_TAGGED_PTR_P0
            void* tmp0 = pbase;
            pbase = rhs.pbase;
            rhs.pbase = tmp0;
#endif
        }

	PointerType& px_ref() const { return this->px;};

        void _pbase_set(void* npbase) {
            this->pbase = reinterpret_cast<PointerType>(npbase);
        }

        /* Satisfy const correctness checks but p0 is mutable */
        void _pbase_set(void* npbase) const {
            this->pbase = reinterpret_cast<PointerType>(npbase);
        }

        void _pxset(void* npx) {
            this->px = reinterpret_cast<PointerType>(npx);
        }

        /* Satisfy const correctness checks but px is mutable */
        void _pxset(void* npx) const {
            this->px = reinterpret_cast<PointerType>(npx);
        }

#ifdef USE_TAGGED_PTR_P0
        void*& pbase_ref() const
        {
            return this->pbase;
        }
#endif

        void* pbase() const
        {
#ifdef USE_TAGGED_PTR_P0
            return this->pbase;
#else   
            if (this->pointerp()) {
                // Shouldn't this point to the mps base?
                return dynamic_cast<void*>(this->px);
            }
            return NULL;
#endif
        }


    protected:
        mutable PointerType px;
#ifdef USE_TAGGED_PTR_P0
        mutable void*  p0;
#endif
    };

};


namespace gctools {


/* working on base_ptr */

    /*! A class that stores a base pointer of a garbage collected object.
      It can be constructed from any smart_ptr or any pointer to any kind
      of GC managed object */
    class tagged_base_ptr {
    public:
        static const uintptr_t unused = tagged_ptr<typename GCHeader<void>::HeaderType>::tagged_unbound;
        static const uintptr_t deleted = tagged_ptr<typename GCHeader<void>::HeaderType>::tagged_deleted;
        tagged_base_ptr(uintptr_t v) : base(v) {};

        template <class U>
        static typename GCHeader<void>::HeaderType* toBasePtr(U* ptr) {
            return reinterpret_cast<typename GCHeader<void>::HeaderType*>(ClientPtrToBasePtr(dynamic_cast<void*>(ptr)));
        }
            
        template <class U>
        tagged_base_ptr(tagged_ptr<U> objPtr) {
            if (objPtr.pointerp() ) {
                this->base = toBasePtr(objPtr.px_ref());
            } else {
                this->base.px_ref() = reinterpret_cast<typename GCHeader<void>::HeaderType*>(objPtr.px_ref());
            }
        }

        tagged_ptr<typename GCHeader<void>::HeaderType>& base_ref() { return this->base;};
    public:
        tagged_ptr<typename GCHeader<void>::HeaderType> base;
    };


    /*! A class that inherits from tagged_base_ptr 
      and can be backcast back to one type of internal pointer.
      It does this by calculating and storing an offset to the internal pointer.
    */
    template <class T>
    class tagged_backcastable_base_ptr : public tagged_base_ptr {
    public:
        tagged_backcastable_base_ptr(uintptr_t v) : tagged_base_ptr(v) {};
        tagged_backcastable_base_ptr(tagged_ptr<T> objPtr) : tagged_base_ptr(objPtr) {
            if ( objPtr.pointerp() ) {
                const char* base_addr = reinterpret_cast<const char*>(this->base.px_ref());
                const char* obj_addr = reinterpret_cast<const char*>(objPtr.px_ref());
                int diff = obj_addr - base_addr; 
                this->offset = tagged_ptr<Fixnum_ty>(diff);
            } else {
                this->offset = tagged_ptr<Fixnum_ty>(gctools::tagged_ptr<Fixnum_ty>::tagged_nil);
            }
        }

        tagged_ptr<T> backcast() const {
            if ( this->offset.fixnump() ) {
                char* base_addr = reinterpret_cast<char*>(this->base.px_ref());
                int diff = this->offset.fixnum();
                char* obj_addr = base_addr + diff;
                return tagged_ptr<T>(reinterpret_cast<T*>(obj_addr));
            }
            return tagged_ptr<T>(reinterpret_cast<uintptr_t>(this->base.px_ref()));
        }
    public:
        tagged_ptr<Fixnum_ty> offset; // tagged fixnum to offset base
    };

};






namespace gctools {


    template<class T, class U> inline bool operator==(tagged_ptr<T> const & a, tagged_ptr<U> const & b)
    {
        if (! (a.pointerp() && b.pointerp()) ) {
            return a.px_ref() == b.px_ref();
            THROW_HARD_ERROR(BF("Implement more == tagged_ptr comparisons"));
        };
        // Both a and b are pointers to objects in memory - compare their pbases
        return a.pbase() == b.pbase();
    }

    template<class T, class U> inline bool operator!=(tagged_ptr<T> const & a, tagged_ptr<U> const & b)
    {
        if (! (a.pointerp() && b.pointerp()) ) {
            return a.px_ref() != b.px_ref();
            THROW_HARD_ERROR(BF("Implement more != tagged_ptr comparisons"));
        };
        // Both a and b are pointers to objects in memory - compare their pbases
        return a.pbase() != b.pbase();
    }

    template<class T, class U> inline bool operator==(tagged_ptr<T> const & a, U * b)
    {
        return a.pxget() == b;
    }

    template<class T, class U> inline bool operator!=(tagged_ptr<T> const & a, U * b)
    {
        return a.pxget() != b;
    }

    template<class T, class U> inline bool operator==(T * a, tagged_ptr<U> const & b)
    {
        return a == b.pxget();
    }

    template<class T, class U> inline bool operator!=(T * a, tagged_ptr<U> const & b)
    {
        return a != b.pxget();
    }

#if __GNUC__ == 2 && __GNUC_MINOR__ <= 96

// Resolve the ambiguity between our op!= and the one in rel_ops

    template<class T> inline bool operator!=(tagged_ptr<T> const & a, tagged_ptr<T> const & b)
    {
        return a.pxget() != b.pxget();
    }

#endif

    template<class T> inline bool operator<(tagged_ptr<T> const & a, tagged_ptr<T> const & b)
    {
        return std::less<T *>()(a.pxget(), b.pxget());
    }

    template<class T> void swap(tagged_ptr<T> & lhs, tagged_ptr<T> & rhs)
    {
        lhs.swap(rhs);
    }

// mem_fn support

    template<class T> T * get_pointer(tagged_ptr<T> const & p)
    {
        return p.get();
    }

    template<class T, class U> tagged_ptr<T> static_pointer_cast(tagged_ptr<U> const & p)
    {
        return static_cast<T *>(p.get());
    }

    template<class T, class U> tagged_ptr<T> const_pointer_cast(tagged_ptr<U> const & p)
    {
        return const_cast<T *>(p.get());
    }

    template<class T, class U> tagged_ptr<T> dynamic_pointer_cast(tagged_ptr<U> const & p)
    {
        return dynamic_cast<T *>(p.get());
    }

}; // namespace gctools


// operator<<

#if !defined(BOOST_NO_IOSTREAM)

#if defined(BOOST_NO_TEMPLATED_IOSTREAMS) || ( defined(__GNUC__) &&  (__GNUC__ < 3) )

template<class Y> std::ostream & operator<< (std::ostream & os, tagged_ptr<Y> const & p)
{
    os << p.get();
    return os;
}

#else

// in STLport's no-iostreams mode no iostream symbols can be used
#ifndef _STLP_NO_IOSTREAMS

# if defined(BOOST_MSVC) && BOOST_WORKAROUND(BOOST_MSVC, < 1300 && __SGI_STL_PORT)
// MSVC6 has problems finding std::basic_ostream through the using declaration in namespace _STL
using std::basic_ostream;
template<class E, class T, class Y> basic_ostream<E, T> & operator<< (basic_ostream<E, T> & os, tagged_ptr<Y> const & p)
# else
    template<class E, class T, class Y> std::basic_ostream<E, T> & operator<< (std::basic_ostream<E, T> & os, gctools::tagged_ptr<Y> const & p)
# endif 
{
    os << p.get();
    return os;
}

#endif // _STLP_NO_IOSTREAMS

#endif // __GNUC__ < 3

#endif // !defined(BOOST_NO_IOSTREAM)

#endif  // #ifndef _TAGGED_PTR_HPP_INCLUDED


