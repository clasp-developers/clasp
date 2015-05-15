// Disable this once we have List_sp working
#define USE_BAD_CAST_ERROR 1
//Turn these on only if x.nilp() are found in ASSERT(...) statements
#define ALLOW_NIL_OTHER 1
//#define ALLOW_CONS_NIL 1


/*
  File: smart_pointers.h
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
       
       
//
// (C) 2004 Christian E. Schafmeister
//

#ifndef	_core_pointer_tagging_H
#define	_core_pointer_tagging_H


#include <boost/utility/binary.hpp>

#include <iostream>
#include <cstring>
//#include "tagged_ptr.h"
//#define TAGGED_PTR_BASE tagged_ptr




//#define	IsUndefined(x) (x)
//#define	NotUndefined(x) (!(x))

//#define	_FWPLock(x)	(x)



//#define	TAGGED_PTR core::T_O*


namespace gctools
{
    /*! Tagged pointer to the global nil */
    extern core::Symbol_O* global_Symbol_OP_nil;
    /*! Tagged pointer to the global UNBOUND */
    extern core::Symbol_O* global_Symbol_OP_unbound;
    /*! Tagged pointer to the global DELETED - used in weak hash tables */
    extern core::Symbol_O* global_Symbol_OP_deleted;
    /*! Tagged pointer to the global SAME-AS-KEY - used in weak hash tables */
    extern core::Symbol_O* global_Symbol_OP_sameAsKey;
};    


extern void lisp_errorUnexpectedNil(type_info const& toType );
extern void lisp_errorBadCast(type_info const& toType, type_info const& fromType, core::T_O* objP );
extern void lisp_errorBadCastFromT_O(type_info const& toType,core::T_O* objP );
extern void lisp_errorBadCastToFixnum_O(type_info const& fromType,core::T_O* objP );
extern void lisp_errorBadCastFromT_OToCons_O(core::T_O* objP );
extern void lisp_errorBadCastFromSymbol_O(type_info const& toType,core::Symbol_O* objP );
extern void lisp_errorDereferencedNonPointer(core::T_O* objP );

namespace gctools {


    //    typedef core::T_O Fixnum_ty;
    
    template <class T> class smart_ptr;

    void initialize_smart_pointers();

#ifdef _ADDRESS_MODEL_64
    static const uintptr_t alignment         = 8; // 16 byte alignment for all pointers
    static const uintptr_t pointer_size      = 8; // 8 byte words 64-bits
    //! Fixnum definition for 64 bit system
    typedef long int Fixnum;
    typedef Fixnum cl_fixnum;
    /*! A pointer that is already tagged can be passed to smart_ptr constructors
      by first reinterpret_casting it to Tagged */
    typedef uintptr_t Tagged;
    static const int fixnum_bits = 63;
    static const int fixnum_shift = 1;
    static const int most_positive_int = std::numeric_limits<int>::max();
    static const int most_negative_int = std::numeric_limits<int>::min();
    static const uint most_positive_uint = std::numeric_limits<unsigned int>::max();
    static const uint64_t most_positive_uint64 = std::numeric_limits<uint64_t>::max();
    static const unsigned long long most_positive_unsigned_long_long = std::numeric_limits<unsigned long long>::max();
    static const long int most_positive_fixnum =  4611686018427387903;
    static const long int most_negative_fixnum = -4611686018427387904;
#define MOST_POSITIVE_FIXNUM gctools::most_positive_fixnum
#define MOST_NEGATIVE_FIXNUM gctools::most_negative_fixnum
#define FIXNUM_BITS gctools::fixnum_bits
#endif
#ifdef _ADDRESS_MODEL_32
#error "Add support for 32 bits - squeeze, Squeeze, SQUEEZE!"
#endif
    static const uintptr_t tag_mask 	      = BOOST_BINARY(    111);
    static const uintptr_t fixnum_tag         = BOOST_BINARY(      0); // xxx0 means fixnum
    static const uintptr_t fixnum_mask        = BOOST_BINARY(      1);
    // other == non cons
    // cons
    // Together they are objects
    
    static const uintptr_t ptr_mask           =~BOOST_BINARY(   111);
    static const uintptr_t other_tag          = BOOST_BINARY(   001); // means ptr
    static const uintptr_t cons_tag           = BOOST_BINARY(   011); // means a cons
    static const uintptr_t frame_tag          = BOOST_BINARY(   101); // means a frame on the stack
    /*! Character */
    static const uintptr_t immediate_mask     = BOOST_BINARY( 11111);
    static const uintptr_t character_tag      = BOOST_BINARY( 00111); // Character
    static const uintptr_t character_shift    = 5;
    static const uintptr_t single_float_tag   = BOOST_BINARY( 01111); // single-float
    static const uintptr_t single_float_shift = 5;
    static const uintptr_t single_float_mask  = 0x1FFFFFFFFF;  // single-floats are in these 32+5bits

    template <class T>
	T tag(T ptr) { return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(ptr)&tag_mask); };


    template <class T> inline bool tagged_consp(T ptr) {
	return (reinterpret_cast<uintptr_t>(tag(ptr))==cons_tag);
    };

    template <class T> inline T tag_cons(T p) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(p)&tag_mask) == 0);
	return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(p)+cons_tag);
    }
    
    template <class T> inline T untag_cons(T ptr) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask) == cons_tag);
	return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(ptr)-cons_tag);
    }


    template <class T> inline T tag_other(T p) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(p)&tag_mask) == 0);
	return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(p)+other_tag);
    }

    template <class T> inline T tag_object(T ptr) {
	return tag_other<T>(ptr);
    }
    template <> inline core::Cons_O* tag_object<core::Cons_O*>(core::Cons_O* ptr) {
	return tag_cons<core::Cons_O*>(ptr);
    }

    template <class T> inline bool tagged_nilp(T ptr) {
	return (reinterpret_cast<void*>(ptr) == global_Symbol_OP_nil);
    }
    template <class T> inline bool tagged_unboundp(T ptr) {
	return (reinterpret_cast<void*>(ptr) == global_Symbol_OP_unbound);
    }
    template <class T> inline bool tagged_deletedp(T ptr) {
	return (reinterpret_cast<void*>(ptr) == global_Symbol_OP_deleted);
    }
    template <class T> inline bool tagged_sameAsKeyp(T ptr) {
	return (reinterpret_cast<void*>(ptr) == global_Symbol_OP_sameAsKey);
    }

    template <class T> inline T tag_nil() {
	GCTOOLS_ASSERT(tagged_nilp(global_Symbol_OP_nil));
	return reinterpret_cast<T>(global_Symbol_OP_nil);
    }
    template <class T> inline T tag_unbound() {
	GCTOOLS_ASSERT(tagged_unboundp(global_Symbol_OP_unbound));
	return reinterpret_cast<T>(global_Symbol_OP_unbound);
    }
    template <class T> inline T tag_deleted() {
	GCTOOLS_ASSERT(tagged_deletedp(global_Symbol_OP_deleted));
	return reinterpret_cast<T>(global_Symbol_OP_deleted);
    }
    template <class T> inline T tag_sameAsKey() {
	GCTOOLS_ASSERT(tagged_sameAsKeyp(global_Symbol_OP_sameAsKey));
	return reinterpret_cast<T>(global_Symbol_OP_sameAsKey);
    }
    template <class T> inline T tag_frame(core::T_O** p) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(p)&tag_mask)==0);
	return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(p)+frame_tag);
    }

    template <class T> inline T untag_other(T ptr) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==other_tag);
	return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(ptr)-other_tag);
    }
    template <class T> inline core::T_O** untag_frame(T ptr) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==frame_tag);
	return reinterpret_cast<core::T_O**>(reinterpret_cast<uintptr_t>(ptr)-frame_tag);
    }


    template <class T> inline T tag_fixnum(Fixnum fn) {
	return reinterpret_cast<T>((fn<<fixnum_shift));
    }
    template <class T> inline Fixnum untag_fixnum(T const ptr)  {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&fixnum_mask)==0);
	return (Fixnum)(reinterpret_cast<Fixnum>(ptr)>>fixnum_shift);
    }
    template <class T> inline bool tagged_fixnump(T ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&fixnum_mask)==fixnum_tag);};
    template <class T> inline T tag_character(int ch) {
	return reinterpret_cast<T>((ch<<character_shift)|character_tag);
    }
    template <class T> inline int untag_character(T ptr) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&fixnum_mask)==character_tag);
	return (int)(reinterpret_cast<uintptr_t>(ptr)>>character_shift);
    }
    template <class T> inline bool tagged_characterp(T ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==character_tag);
    };
    template <class T> inline T tag_single_float(float fn) {
	GCTOOLS_ASSERT(sizeof(uintptr_t)==8);
	GCTOOLS_ASSERT(sizeof(float)==4);
	uintptr_t val;
	memcpy(&val,&fn,sizeof(fn));
	return reinterpret_cast<T>((val<<single_float_shift)+single_float_tag);
    }
    template <class T> inline uintptr_t tagged_single_float_masked(T const ptr) {
	return reinterpret_cast<uintptr_t>(reinterpret_cast<uintptr_t>(ptr)&single_float_mask);
    }
    template <class T> inline float untag_single_float(T const ptr)  {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==single_float_tag);
	GCTOOLS_ASSERT(sizeof(uintptr_t)==8);
	GCTOOLS_ASSERT(sizeof(float)==4);
	uintptr_t val(reinterpret_cast<uintptr_t>(ptr));
	float result;
	val >>= single_float_shift;
	memcpy(&result,&val,sizeof(result));
	return result;
    }
    template <class T> inline bool tagged_single_floatp(T ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==single_float_tag);
    };

    template <class T> inline bool tagged_otherp(T ptr) {
	return ((uintptr_t)(ptr)&tag_mask)==other_tag;
    }
    template <class T> inline bool tagged_framep(T ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==frame_tag);
    };

    template <class Type> inline Type untag_object(Type tagged_obj) {
	if ( gctools::tagged_otherp<Type>(tagged_obj) ) {
	    return gctools::untag_other<Type>(tagged_obj);
	} else if ( gctools::tagged_consp<Type>(tagged_obj) ) {
	    return gctools::untag_cons<Type>(tagged_obj);
	} else {
	    THROW_HARD_ERROR(BF("Trying to untag non-other or non-cons: %p") % (void*)(tagged_obj));
	}
    };

};


#endif // pointer_tagging
