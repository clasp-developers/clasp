// Disable this once we have List_sp working
#define ALLOW_CONS_NIL 1


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

#ifndef	_core_smart_pointers_H
#define	_core_smart_pointers_H


#include <boost/utility/binary.hpp>

#include <iostream>
//#include "tagged_ptr.h"
#define TAGGED_PTR_BASE tagged_ptr




#define	IsUndefined(x) (x)
#define	NotUndefined(x) (!(x))

#define	_FWPLock(x)	(x)



#define	TAGGED_PTR core::T_O*


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


namespace gctools
{


    typedef core::T_O Fixnum_ty;
    
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
    static const long int mostPositiveFixnum =  4611686018427387903;
    static const long int mostNegativeFixnum = -4611686018427387904;
#define MOST_POSITIVE_FIXNUM gctools::mostPositiveFixnum
#define MOST_NEGATIVE_FIXNUM gctools::mostNegativeFixnum
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

    template <class T>
    uintptr_t tag(T* ptr) { return reinterpret_cast<uintptr_t>(ptr)&tag_mask; };


    template <class T> inline bool tagged_consp(T* ptr) {
	return (tag(ptr)==cons_tag);
    };

    template <class T> inline T* tag_cons(T* p) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(p)&tag_mask) == 0);
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(p)+cons_tag);
    }
    template <class T> inline T* untag_cons(T* ptr) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask) == cons_tag);
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(ptr)-cons_tag);
    }


    template <class T> inline T* tag_other(T* p) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(p)&tag_mask) == 0);
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(p)+other_tag);
    }

    template <class T> inline T* tag_object(T* ptr) {
	return tag_other<T>(ptr);
    }
    template <> inline core::Cons_O* tag_object<core::Cons_O>(core::Cons_O* ptr) {
	return tag_cons<core::Cons_O>(ptr);
    }

        template <class T> inline bool tagged_nilp(T* ptr) {
	return (reinterpret_cast<void*>(ptr) == global_Symbol_OP_nil);
    }
    template <class T> inline bool tagged_unboundp(T* ptr) {
	return (reinterpret_cast<void*>(ptr) == global_Symbol_OP_unbound);
    }
    template <class T> inline bool tagged_deletedp(T* ptr) {
	return (reinterpret_cast<void*>(ptr) == global_Symbol_OP_deleted);
    }
    template <class T> inline bool tagged_sameAsKeyp(T* ptr) {
	return (reinterpret_cast<void*>(ptr) == global_Symbol_OP_sameAsKey);
    }

    template <class T> inline T* tag_nil() {
	GCTOOLS_ASSERT(tagged_nilp(global_Symbol_OP_nil));
	return reinterpret_cast<T*>(global_Symbol_OP_nil);
    }
    template <class T> inline T* tag_unbound() {
	GCTOOLS_ASSERT(tagged_unboundp(global_Symbol_OP_unbound));
	return reinterpret_cast<T*>(global_Symbol_OP_unbound);
    }
    template <class T> inline T* tag_deleted() {
	GCTOOLS_ASSERT(tagged_deletedp(global_Symbol_OP_deleted));
	return reinterpret_cast<T*>(global_Symbol_OP_deleted);
    }
    template <class T> inline T* tag_sameAsKey() {
	GCTOOLS_ASSERT(tagged_sameAsKeyp(global_Symbol_OP_sameAsKey));
	return reinterpret_cast<T*>(global_Symbol_OP_sameAsKey);
    }
    template <class T> inline T* tag_frame(core::T_O** p) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(p)&tag_mask)==0);
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(p)+frame_tag);
    }

    template <class T> inline T* untag_other(T* ptr) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==other_tag);
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(ptr)-other_tag);
    }
    template <class T> inline core::T_O** untag_frame(T* ptr) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==frame_tag);
	return reinterpret_cast<core::T_O**>(reinterpret_cast<uintptr_t>(ptr)-frame_tag);
    }


    template <class T> inline T* tag_fixnum(Fixnum fn) {
	return reinterpret_cast<T*>((fn<<fixnum_shift));
    }
    template <class T> inline Fixnum untag_fixnum(T* const ptr)  {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&fixnum_mask)==0);
	return (Fixnum)(reinterpret_cast<uintptr_t>(ptr)>>fixnum_shift);
    }
    template <class T> inline bool tagged_fixnump(T* ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&fixnum_mask)==fixnum_tag);};
    template <class T> inline T* tag_character(int ch) {
	return reinterpret_cast<T*>((ch<<character_shift)|character_tag);
    }
    template <class T> inline int untag_character(T* ptr) {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&fixnum_mask)==character_tag);
	return (int)(reinterpret_cast<uintptr_t>(ptr)>>character_shift);
    }
    template <class T> inline bool tagged_characterp(T* ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==character_tag);
    };
    template <class T> inline T* tag_single_float(float fn) {
	GCTOOLS_ASSERT(sizeof(uintptr_t)==8);
	GCTOOLS_ASSERT(sizeof(float)==4);
	uintptr_t val;
	memcpy(&val,&fn,sizeof(fn));
	return reinterpret_cast<T*>((val<<single_float_shift)+single_float_tag);
    }
    template <class T> inline float untag_single_float(T* const ptr)  {
	GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==single_float_tag);
	GCTOOLS_ASSERT(sizeof(uintptr_t)==8);
	GCTOOLS_ASSERT(sizeof(float)==4);
	uintptr_t val(reinterpret_cast<uintptr_t>(ptr));
	float result;
	val >>= single_float_shift;
	memcpy(&result,&val,sizeof(result));
	return result;
    }
    template <class T> inline bool tagged_single_floatp(T* ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==single_float_tag);
    };

    template <class T> inline bool tagged_otherp(T* ptr) {
	return ((uintptr_t)(ptr)&tag_mask)==other_tag;
    }
    template <class T> inline bool tagged_framep(T* ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==frame_tag);
    };





    

    template <class T>
	class smart_ptr /*: public tagged_ptr<T>*/ {
    public:
	typedef T Type;
	T* theObject;
    public:
	//Default constructor, set theObject to NULL
    smart_ptr() : theObject(NULL) {};
	//    	explicit smart_ptr(uintptr_t p) : theObject(p) {}; // TODO: this converts ints to smart_ptr's - its dangerous
	//! Construct a FRAME object - I need to get rid of these
	//smart_ptr( core::T_O** p ) : theObject(tag_frame(p)) { /*printf("%s:%d Creating Frame \n", __FILE__, __LINE__ );*/ };
	//smart_ptr( T* objP) : theObject(tag_object(objP)) {};
	// explicit smart_ptr( void* objP) : theObject(reinterpret_cast<T*>(objP)) {};

	/*! Create a smart pointer from an existing tagged pointer */
	explicit inline smart_ptr(Tagged ptr) : theObject(reinterpret_cast<T*>(ptr)) {
	    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask)!=0);
	};

	explicit inline smart_ptr(T* ptr) : theObject(tag_other<T>(ptr)) {
	    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==0);
	};
	
	inline smart_ptr(const smart_ptr<T>& obj) : theObject(obj.theObject) {};


	template<class From>
        inline smart_ptr( smart_ptr<From> const & rhs )
        {
            if ( LIKELY(rhs.objectp()) ) {
		T* px = DynamicCast<T*,From*>::castOrNULL(untag_other<From>(rhs.theObject));
                if ( px==0 ) {
                    THROW_HARD_ERROR(BF("DynamicCast<T*,From*> failed due to an illegal cast T* = %s  From* = %s") % typeid(T*).name() % typeid(From*).name() );
                }
		this->theObject = tag_other<T>(px);
            } else {
                this->theObject = reinterpret_cast<T*>(rhs.theObject);
            }
        }


	uintptr_t tag() const { return reinterpret_cast<uintptr_t>(this->theObject)&tag_mask; };
	
	// Convert one type of smart_ptr to another
	// TODO: Implement this but check for type conversions!!!!!
#if 0
	template <class Y> smart_ptr(const smart_ptr<T>& yy) : theObject(yy.theObject) {
	    GCTOOLS_ASSERT(false);
	};
#endif


    public:
	//----------------------------------------------------------------------
	//
	// Constructors
	//
	//
	// Make a tagged fixnum
	inline static smart_ptr<T> make_tagged_fixnum(Fixnum val) {return smart_ptr<T>(tag_fixnum<T>(val));}
	inline static smart_ptr<T> make_tagged_other(T* p) {return smart_ptr<T>(p);}
	inline static smart_ptr<T> make_tagged_nil() {return smart_ptr<T>(reinterpret_cast<T*>(global_Symbol_OP_nil));};
	inline static smart_ptr<T> make_tagged_unbound() {return smart_ptr<T>(reinterpret_cast<T*>(global_Symbol_OP_unbound));};
	inline static smart_ptr<T> make_tagged_deleted() {return smart_ptr<T>(reinterpret_cast<T*>(global_Symbol_OP_deleted));};
	inline static smart_ptr<T> make_tagged_sameAsKey() {return smart_ptr<T>(reinterpret_cast<T*>(global_Symbol_OP_sameAsKey));};

	/*! Get the pointer typcast to an integer quantity for hashing */
	cl_intptr_t intptr() const { return ((uintptr_t)(this->theObject));};

	void reset_() { this->theObject = NULL; };

	template <class o_class>
	    inline smart_ptr<o_class> pointerAsUnsafe()
	    {
		o_class* new_px = dynamic_cast<o_class*>(this->px);
		return smart_ptr<o_class>(new_px);
	    }

	inline void swap(smart_ptr<T>& other)
	{
	    T* temp;
	    temp = this->theObject;
	    this->theObject = other.theObject;
	    other.theObject = temp;
	}
#if 0   /// Figure out what to do with this
	template <class o_class>
	    inline smart_ptr<o_class> pointerAsUnsafe() const
	    {
		o_class* new_px = dynamic_cast<o_class*>(this->px);
		return smart_ptr<o_class>(new_px);
	    }
#endif

	template <class o_class>
	    void errorExpectedType() const {
	    class_id expected_typ = reg::registered_class<o_class>::id;
	    class_id this_typ = reg::registered_class<T>::id;
	    lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->untag_object()));
	};

	template <class o_class>
	    inline smart_ptr<o_class> asOrNull() {
	    if (this->otherp()) {
		o_class* cast = gctools::DynamicCast<o_class*,T*>::castOrNULL(untag_other<T>(this->theObject));
		if ( cast == NULL ) return smart_ptr<o_class>();
		smart_ptr<o_class> ret((Tagged)tag_other<o_class>(cast));
		return ret;
	    } else if (this->consp()) {
		o_class* cast = gctools::DynamicCast<o_class*,T*>::castOrNULL(untag_cons<T>(this->theObject));
		if ( cast == NULL ) return smart_ptr<o_class>();
		smart_ptr<o_class> ret((Tagged)tag_cons<o_class>(cast));
		return ret;
	    }
	    this->errorExpectedType<o_class>();
	    // unreachable
	    smart_ptr<o_class> fail;
	    return fail;
	}

	template <class o_class>
	    inline smart_ptr<o_class> asOrNull() const {
	    if (this->otherp()) {
		o_class* cast = gctools::DynamicCast<o_class*,T*>::castOrNULL(untag_other<T>(this->theObject));
		if ( cast == NULL ) return smart_ptr<o_class>();
		smart_ptr<o_class> ret((Tagged)tag_other<o_class>(cast));
		return ret;
	    } else if (this->consp()) {
		o_class* cast = gctools::DynamicCast<o_class*,T*>::castOrNULL(untag_cons<T>(this->theObject));
		if ( cast == NULL ) return smart_ptr<o_class>();
		smart_ptr<o_class> ret((Tagged)tag_cons<o_class>(cast));
		return ret;
	    }
	    this->errorExpectedType<o_class>();
	    // unreachable
	    smart_ptr<o_class> fail;
	    return fail;
	}

	template <class o_class>
	    inline smart_ptr<o_class> as()
	    {
		smart_ptr<o_class> ret = this->asOrNull<o_class>();
		if (!ret) {
			class_id expected_typ = reg::registered_class<o_class>::id;
			class_id this_typ = reg::registered_class<T>::id;
			lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->untag_object()));
		    }
		return ret;
	    }

	template <class o_class>
	    inline smart_ptr< o_class> as() const
	    {
		smart_ptr< o_class> ret = this->asOrNull<o_class>();
		if (!ret) {
			class_id expected_typ = reg::registered_class<o_class>::id;
			class_id this_typ = reg::registered_class<T>::id;
			lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->untag_object()));
		    }
		return ret;
	    }
	/*! Downcast to o_class - if ptr cannot be downcast throw exception.
	  If the ptr is nil, return a nil ptr.
	TODO:  replace this with as<Cons_O>()*/
	template <class o_class>
	    inline smart_ptr<o_class> as_or_nil()
	    {
		if ( this->nilp() ) {
		    smart_ptr<o_class> nil((Tagged)tag_nil<o_class>());
		    return nil;
		}
		smart_ptr<o_class> ret = this->asOrNull<o_class>();
		if (!ret)
		    {
			class_id expected_typ = reg::registered_class<o_class>::id;
			class_id this_typ = reg::registered_class<T>::id;
			lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->untag_object()));
		    }
		return ret;
	    }
	template <class o_class>
	    inline smart_ptr<o_class> as_or_nil() const
	    {
		if ( this->nilp() ) {
		    smart_ptr<o_class> nil((Tagged)tag_nil<o_class>());
		    return nil;
		}
		smart_ptr<o_class> ret = this->asOrNull<o_class>();
		if (!ret)
		    {
			class_id expected_typ = reg::registered_class<o_class>::id;
			class_id this_typ = reg::registered_class<T>::id;
			lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->untag_object()));
		    }
		return ret;
	    }

	/*! Downcast to o_class - if ptr cannot be downcast throw exception.
	  If the ptr is nil, throw an error */
	template <class o_class>
	    inline smart_ptr< o_class> asNotNil() const
	    {
		if ( this->nilp() ) {
		    class_id expected_typ = reg::registered_class<o_class>::id;
		    lisp_errorUnexpectedNil(expected_typ);
		}
		smart_ptr< o_class> ret = this->asOrNull<o_class>();
		if (!ret) {
		    class_id expected_typ = reg::registered_class<o_class>::id;
		    class_id this_typ = reg::registered_class<T>::id;
		    lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->untag_object()));
		}
		return ret;
	    }

#if 0
	/*! Downcast to o_class - if ptr cannot be downcast throw exception.
	  If the ptr is nil, return a nil ptr */
	template <class o_class>
	    inline smart_ptr< o_class> as_or_nil() const
	    {
		if ( this->nilp() ) {
		    smart_ptr<o_class> nil(smart_ptr<o_class>::tagged_nil);
		    return nil;
		}
		smart_ptr< o_class> ret = this->asOrNull<o_class>();
		if (!ret)
		    {
			class_id expected_typ = reg::registered_class<o_class>::id;
			class_id this_typ = reg::registered_class<T>::id;
			lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->px));
		    }
		return ret;
	    }
#endif

	template <class o_class>
	    inline bool isA() const
	    {
		if ( this->objectp() ) {
		    if ( smart_ptr<const o_class> ret = this->asOrNull<const o_class>() ) {
			return true;
		    }
		}
		return false;
	    }


	template <class o_class>
	    inline bool isA()
	    {
		if ( this->objectp() ) {
		    if ( smart_ptr<o_class> ret = this->asOrNull<o_class>() ) {
			return true;
		    }
		}
		return false;
	    }


	/*! Return the offset in bytes between this.px and this - you need to modify the base
	  class of smart_ptr to make px protected */
	//	int offset_of_px_from_this() const { return ((char*)(&this->px)) - ((char*)(this));}
	/*! Return the size in bytes of px - you need to modify the base class
	  of smart_ptr to make px protected */
	//	int size_of_px() const { return sizeof(this->px); };

	int number_of_values() const { return this->theObject == NULL ? 0 : 1;};

	/*! Dereferencing operator - remove the other tag */
	T* operator->() {
	    GCTOOLS_ASSERT(this->otherp());
	    return untag_other(this->theObject);
	};

	T* operator->() const {
	    GCTOOLS_ASSERT(this->otherp());
	    return untag_other(this->theObject);
	};

	T& operator*() const {
	    GCTOOLS_ASSERT(this->objectp());
	    return *(this->untag_object());
	};
	
	T* untag_object() const {
	    GCTOOLS_ASSERT(this->otherp()||this->consp());
	    if ( this->otherp() ) {
		return untag_other<T>(this->theObject);
	    } else if ( this->consp() ) {
		return untag_cons<T>(this->theObject);
	    } 		
	    THROW_HARD_ERROR(BF("This should never happen"));
	}

	T* get() const { return this->untag_object(); };
	bool _NULLp() const { return this->theObject==NULL; };
#if 0
	operator smart_ptr<core::Cons_O> () {
	    if ( this->consp() ) {
		smart_ptr<core::Cons_O> ret(tag_cons<core::Cons_O>(gctools::DynamicCast<core::Cons_O*,T*>::castOrNULL(untag_cons<T>(this->theObject))));
		return ret;
	    } else if ( this->nilp() ) {
		return smart_ptr<core::Cons_O>(tag_nil<core::Cons_O>());
	    }
	    this->errorExpectedType<core::Cons_O>();
	}
#endif
	    
	/*! If theObject!=NULL then return true */
	explicit operator bool() const { return this->theObject != NULL; };

	operator smart_ptr<core::T_O>() const { return smart_ptr<core::T_O>((Tagged)this->theObject);};

								    
	bool nilp() const { return tagged_nilp(this->theObject); }
	bool notnilp() const { return (!this->nilp());};
	bool isTrue() const { return !this->nilp(); };
	bool fixnump() const {return tagged_fixnump(this->theObject);};
	Fixnum unsafe_fixnum() const {return untag_fixnum(this->theObject);};
	bool unboundp() const { return tagged_unboundp(this->theObject);};
	bool deletedp() const { return tagged_deletedp(this->theObject);};
	bool sameAsKeyp() const { return tagged_sameAsKeyp(this->theObject);};
	bool characterp() const {return tagged_characterp<T>(this->theObject);};
	int unsafe_character() const {return untag_character(this->theObject);};
	bool single_floatp() const {return tagged_single_floatp<T>(this->theObject);};
	float unsafe_single_float() const {return untag_single_float<T>(this->theObject);};
	// This replaces pointerp()
	bool objectp() const { return this->otherp() || this->consp(); };
	bool otherp() const { return tagged_otherp<T>(this->theObject);};
	bool consp() const { return tagged_consp<T>(this->theObject);};
	core::Cons_O* unsafe_cons() const {
	    GCTOOLS_ASSERT(this->consp());
	    return reinterpret_cast<core::Cons_O*>(reinterpret_cast<uintptr_t>(this->theObject)-cons_tag); };

	Fixnum asFixnum() const {
	    GCTOOLS_ASSERT(this->fixnump());
	    return untag_fixnum<T>(this->theObject);
	};

	bool framep() const { return tagged_framep(this->theObject);};
	core::T_O** unsafe_frame() const { return untag_frame(this->theObject); };
	core::T_O** safe_frame() const { GCTOOLS_ASSERT(this->framep()); return this->unsafe_frame(); };

	bool sameAsKeyP() const { return tagged_sameAsKeyp(this->theObject); }
	
	/*! Return the raw smart_ptr value interpreted as a T_O* */
	core::T_O* raw_() const { return reinterpret_cast<core::T_O*>(this->theObject);}

	void setRaw_(core::T_O* p) {this->theObject = reinterpret_cast<core::T_O*>(p);}

	/*! This should almost NEVER be used!!!!!!   
	  The only reason to ever use this is when theObject will be set to NULL
	  and you are sure that it will not be interpreted as a Fixnum!!!

	  List actual uses here:
	  gcweak.h>>WeakPointerManager
	  gcweak.h>>~WeakPointerManager
	  gcweak.h>>Mapping(const T& val)
	  gcweak.h>>Buckets::set
	  intrinsics.cc>>cc_loadTimeValueReference
	*/
	T*& rawRef_() { return this->theObject; };

	
	/*! Check if this tagged theObject matches the templated type.
	  The most common case is this is an object.*/
	bool valid() const {
	    GCTOOLS_ASSERT(false); // BF("Implement me"));
	}


	template <class U> inline bool operator==(smart_ptr<U> const other) const {
	    return this->theObject == other.theObject;
	}

	template <class U> inline bool operator!=(smart_ptr<U> const other) const {
	    return this->theObject != other.theObject;
	}

    };

};

namespace core {
    class Fixnum_I;
};



namespace core {
    class List_V {}; // Virtual class representing Common Lisp LIST
    extern gctools::smart_ptr<core::T_O> cons_car(core::Cons_O* cur);
    extern gctools::smart_ptr<core::T_O> cons_cdr(core::Cons_O* cur);
};



namespace gctools {
    
    template <>
	class smart_ptr<core::Cons_O> {
    public:
	typedef core::Cons_O Type;
	core::Cons_O* theObject;
    public:
	//! The default constructor returns an invalid smart_ptr
    smart_ptr() : theObject(NULL) {};
	// Constructor that takes Cons_O* assumes its untagged
	explicit inline smart_ptr(core::Cons_O* ptr) : theObject(tag_cons<core::Cons_O>(ptr)) {
	    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==0);
	};
	/*! Constructor that takes Tagged assumes that the pointer is tagged.
	  Any ptr passed to this constructor must have the CONS tag.
	*/
#ifdef ALLOW_CONS_NIL
	explicit inline smart_ptr(Tagged ptr) : theObject(reinterpret_cast<core::Cons_O*>(ptr)) {
	    GCTOOLS_ASSERT(tagged_consp<core::Cons_O>(reinterpret_cast<core::Cons_O*>(ptr))
			   ||tagged_nilp<core::Cons_O>(reinterpret_cast<core::Cons_O*>(ptr))
			   );
#else
	explicit inline smart_ptr(Tagged ptr) : theObject(reinterpret_cast<core::Cons_O*>(ptr)) {
	    GCTOOLS_ASSERT(tagged_consp<core::Cons_O>(reinterpret_cast<core::Cons_O*>(ptr))
			   //			   ||tagged_nilp<core::Cons_O>(reinterpret_cast<core::Cons_O*>(ptr))
			   );
#endif
	   
	};
	
    public:
	explicit operator bool() const { return this->theObject!=NULL; }
    public:
	void reset_() { this->theObject = NULL; };
	inline bool otherp() const { return tagged_otherp<core::Cons_O>(this->theObject);};
	inline bool objectp() const { return this->otherp() || this->consp(); };
#ifdef ALLOW_CONS_NIL // DISABLE THESE AND USE List_sp
	inline bool consp() const { return tagged_consp(this->theObject) || tagged_nilp(this->theObject); };
	inline bool nilp() const { return tagged_nilp(this->theObject); };
	inline bool notnilp() const { return !this->nilp(); };
	inline bool isTrue() const { return !this->nilp(); };
	inline bool valid() const { return this->consp() || this->nilp(); };
#else
	inline bool isTrue() const { return true; };
	inline bool consp() const { return tagged_consp(this->theObject); };
	inline bool valid() const { return this->consp(); } // || this->nilp(); };
#endif
	inline bool unboundp() const { return tagged_unboundp(this->theObject); };

	operator smart_ptr<core::T_O>() const { return smart_ptr<core::T_O>((Tagged)const_cast<core::T_O* const>(reinterpret_cast<core::T_O*>(this->theObject)));};

	//	operator smart_ptr<core::List_V>() const { return smart_ptr<core::List_V>((Tagged)const_cast<core::T_O* const>(reinterpret_cast<core::T_O*>(this->theObject)));};


	core::Cons_O* untag_object() const {
	    GCTOOLS_ASSERT(this->otherp()||this->consp());
	    if ( this->consp() ) {
		return untag_cons<core::Cons_O>(this->theObject);
	    }
#ifdef ALLOW_CONS_NIL
	    else {
		if ( this->nilp() ) {
		    return tag_nil<core::Cons_O>();
		}
	    }
#endif
	    THROW_HARD_ERROR(BF("Figure out what to do when untag_object Cons_O would not return a Cons_O*"));
	}

	inline void swap(smart_ptr<core::Cons_O>& other)
	{
	    core::Cons_O* temp;
	    temp = this->theObject;
	    this->theObject = other.theObject;
	    other.theObject = temp;
	}

		/*! Dereferencing operator - remove the other tag */
	inline core::Cons_O* operator->() {
	    GCTOOLS_ASSERT(this->objectp());
	    return this->untag_object();
	};
	inline core::Cons_O* operator->() const {
	    GCTOOLS_ASSERT(this->objectp());
	    return this->untag_object();
	};

	inline core::Cons_O& operator*() {
	    GCTOOLS_ASSERT(this->objectp());
	    return *(this->untag_object());
	};

	core::T_O* raw_() const { return reinterpret_cast<core::T_O*>(this->theObject);}
	bool _NULLp() const { return this->theObject==NULL; };

	template <class U> inline bool operator==(smart_ptr<U> const other) const {
	    return this->theObject == other.theObject;
	}

	template <class U> inline bool operator!=(smart_ptr<U> const other) const {
	    return this->theObject != other.theObject;
	}



    };


};






/*! List_sp implementation with iterator by Georgiy Tugai  April 27, 2015
 */


 
namespace gctools {
    typedef smart_ptr<core::List_V> List_sp;
    class List_sp_iterator_nil;
    class List_sp_iterator;
    template <> class smart_ptr<core::List_V> {
	friend class List_sp_iterator_nil;
	friend class List_sp_iterator;
    public:
	typedef core::T_O
	    Type; // The best common type for both Cons_O and Symbol_O is T_O
	Type *theObject;
 
    public:
	//! The default constructor returns an invalid smart_ptr
    smart_ptr() : theObject(NULL){};
	inline smart_ptr(const smart_ptr<core::Cons_O>& other)
	    : theObject(other.raw_()) {
	    GCTOOLS_ASSERT(other.consp());
	};
	// Constructor that takes Cons_O* assumes its untagged
	explicit inline smart_ptr(core::Cons_O *ptr)
	    : theObject(tag_cons<Type>(reinterpret_cast<core::T_O*>(ptr))) {
	    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & tag_mask) == 0);
	};
	explicit inline smart_ptr(core::Symbol_O *ptr)
	    : theObject(tag_other<Type>(reinterpret_cast<core::T_O*>(ptr))) {
	    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & tag_mask) == 0);
	};
	/*! Constructor that takes Tagged assumes that the pointer is tagged.
	  Any ptr passed to this constructor must have the CONS tag.
	*/
	explicit inline smart_ptr(Tagged ptr)
	    : theObject(reinterpret_cast<Type *>(ptr)) {
	    GCTOOLS_ASSERT(tagged_consp<Type>(reinterpret_cast<Type *>(ptr)) ||
			   tagged_nilp<Type>(reinterpret_cast<Type *>(ptr)));
	};
 
    public:
	explicit operator bool() const { return this->theObject != NULL; }
 
    public:
	void reset_() { this->theObject = NULL; };
	inline bool otherp() const { return tagged_otherp<Type>(this->theObject); };
	inline bool consp() const {
	    return tagged_consp(this->theObject) || tagged_nilp(this->theObject);
	};
	inline bool objectp() const { return this->otherp() || this->consp(); };
	inline bool nilp() const { return tagged_nilp(this->theObject); };
	inline bool notnilp() const { return this->nilp(); };
	inline bool isTrue() const { return !this->nilp(); };
	inline bool unboundp() const { return tagged_unboundp(this->theObject); };

	//	inline operator smart_ptr<core::Cons_O>() const { GCTOOLS_ASSERT(this->consp());return smart_ptr<core::Cons_O>((Tagged)this->theObject); };
	inline smart_ptr<core::Cons_O> asCons() const { GCTOOLS_ASSERT(this->consp());return smart_ptr<core::Cons_O>((Tagged)this->theObject); };

	inline bool valid() const { return this->consp() || this->nilp(); };
 
	operator smart_ptr<core::T_O>() const {
	    return smart_ptr<Type>((Tagged) const_cast<Type *const>(reinterpret_cast<Type *>(this->theObject)));
	};
 
	Type *untag_object() const {
	    GCTOOLS_ASSERT(this->otherp() || this->consp());
	    if (this->consp()) {
		return untag_cons<Type>(this->theObject);
	    } else {
		if (this->nilp()) {
		    return tag_nil<Type>();
		}
	    }
	    THROW_HARD_ERROR(BF("Figure out what to do when untag_object doesn't have "
				"a Cons_O or NIL"));
	}
 
	inline void swap(smart_ptr<core::List_V> &other) {
	    Type *temp;
	    temp = this->theObject;
	    this->theObject = other.theObject;
	    other.theObject = temp;
	}
 
	/*! Dereferencing operator - remove the other tag */
	inline Type *operator->() {
	    GCTOOLS_ASSERT(this->objectp());
	    return this->untag_object();
	};
	inline Type *operator->() const {
	    GCTOOLS_ASSERT(this->objectp());
	    return this->untag_object();
	};
 
	inline Type &operator*() {
	    GCTOOLS_ASSERT(this->objectp());
	    return *(this->untag_object());
	};
 
	core::T_O *raw_() const { return reinterpret_cast<Type *>(this->theObject); }
	bool _NULLp() const { return this->theObject == NULL; };
 
	template <class U> inline bool operator==(smart_ptr<U> const other) const {
	    return this->theObject == other.theObject;
	}
 
	template <class U> inline bool operator!=(smart_ptr<U> const other) const {
	    return this->theObject != other.theObject;
	}

    public:
	class List_sp_iterator {
	    friend class List_sp_iterator_nil;
	public:
	List_sp_iterator(const List_sp& ptr) : ptr(ptr.theObject) {};
	    List_sp_iterator &operator++() {
		GCTOOLS_ASSERT(tagged_consp<Type>(this->ptr));
		this->ptr = cons_cdr(untag_cons<core::Cons_O>(reinterpret_cast<core::Cons_O*>(this->ptr))).theObject;
		return *this;
	    }
	    List_sp_iterator &operator++(int) { // postfix
		auto clone = new List_sp_iterator(*this);
		++*this;
		return *clone;
	    }
#if 0
	    bool operator==(const List_sp_iterator &b) const {
		if (b.ptr->consp())
		    return ptr == (b.ptr);
		else
		    return !ptr->consp();
	    }
	    bool operator==(const List_sp_iterator_nil &b) const { return !ptr->consp(); }
	    bool operator!=(const List_sp_iterator_nil &b) const { return ptr->consp(); }
#endif
	    bool operator!=(const List_sp_iterator &b) const {
		return this->ptr != b.ptr;
		//!(*this == b); }
	    };
#if 0
	    List_sp &operator->() { return *ptr; }
	    List_sp &operator->() const { return *ptr; }
#endif
	    // Unsafe but fast cast of T_O* to Cons_O* - should only be done within a loop
	    smart_ptr<core::Cons_O> operator*() { return smart_ptr<core::Cons_O>((Tagged)(ptr)); }
	public:
	    Type* ptr;
	    //	friend class List_sp;
	};

	
	class List_sp_iterator_nil : public List_sp_iterator {
	public:
	List_sp_iterator_nil() : List_sp_iterator(smart_ptr<core::List_V>((Tagged)global_Symbol_OP_nil)) {}; // nullptr){};
#if 0
	    bool operator==(const List_sp_iterator &b) const { return !b.ptr->consp(); }
	    bool operator!=(const List_sp_iterator &b) const { return b.ptr->consp(); }
#endif
	    List_sp &operator->() = delete;
	    List_sp &operator->() const = delete;
	    List_sp operator*() = delete;
	    List_sp &operator++() = delete;
	    List_sp &operator++(int) = delete;
	};

    public:
	List_sp_iterator begin() { return List_sp_iterator(*this); }
	List_sp_iterator end() { return List_sp_iterator_nil(); }

	List_sp_iterator const begin() const { return List_sp_iterator(*this); }
	List_sp_iterator const end() const { return List_sp_iterator_nil(); }


    };







#if 0    

    template <> inline List_sp_iterator& List_sp_iterator::operator++() {
	GCTOOLS_ASSERT(tagged_consp<core::List_V>(this->ptr->theObject));
	this->ptr = cons_cdr(reinterpret_cast<core::Cons_O*>((this->ptr)->raw_()));
	return *this;
    }

    template <> inline bool List_sp_iterator::operator==(const List_sp_iterator &b) const {
	if (b.ptr->consp())
	    return ptr == (b.ptr);
	else
	    return !ptr->consp();
    }

    template <> inline bool List_sp_iterator::operator==(const List_sp_iterator_nil &b) const { return !ptr->consp(); }

    template <> inline bool List_sp_iterator::operator!=(const List_sp_iterator_nil &b) const { return ptr->consp(); }

    template <> inline 	List_sp List_sp_iterator::operator*() { return *ptr; }
#endif
};


namespace core {
    using gctools::List_sp;
};








template <class T>
gctools::smart_ptr<T> _Nil()
{
    gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_nil<T>());
    return x;
}



template <class T>
gctools::smart_ptr<T> _Unbound()
{
    gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_unbound<T>());
    return x;
}

template <class T>
gctools::smart_ptr<T> _Deleted()
{
    gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_deleted<T>());
    return x;
}

template <class T>
gctools::smart_ptr<T> _SameAsKey()
{
    gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_sameAsKey<T>());
    return x;
}



//template <class T> inline bool Null(const gctools::smart_ptr<T>& ptr) { return ptr.nilp();};


namespace gctools {

// LambdaListHandler_sp llh(ptr)

#if defined(USE_BOEHM) || defined(USE_MPS)

    template <class TO, class FROM>
    smart_ptr<TO> dynamic_pointer_cast(const smart_ptr<FROM>& ptr)
    {
	return smart_ptr<TO>(dynamic_cast<TO*>(ptr.pxget()));
    };

    template <class TO,class FROM>
    smart_ptr<TO> dynamic_pointer_cast(FROM ptr)
    {
	return smart_ptr<TO>(dynamic_cast<typename TO::PointerType>(ptr.pxget()));
    };

#else

    template <class TO, class FROM>
    smart_ptr<TO> dynamic_pointer_cast(const smart_ptr<FROM>& ptr)
    {
	return smart_ptr<TO>(boost::dynamic_pointer_cast<TO>(ptr));
    };

    template <class TO,class FROM>
    smart_ptr<TO> dynamic_pointer_cast(FROM ptr)
    {
	return smart_ptr<TO>(boost::dynamic_pointer_cast<TO>(ptr));
    };
#endif


#if 0
    template <>
	bool smart_ptr<core::T_O>::isA() const {return true;}
    // Do I need ther isA tests for Fixnum, Character - etc?
#endif




};

namespace core {
    using gctools::Fixnum;
};


#endif
