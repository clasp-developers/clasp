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
    /*! Point to the global nil */
    extern void* global_voidP_nil;
    /*! Point to the global UNBOUND */
    extern void* global_voidP_unbound;
    /*! Point to the global DELETED - used in weak hash tables */
    extern void* global_voidP_deleted;
    /*! Point to the global SAME-AS-KEY - used in weak hash tables */
    extern void* global_voidP_sameAsKey;
};    


namespace gctools
{


    typedef core::T_O Fixnum_ty;
    
    template <class T> class smart_ptr;

    void initialize_smart_pointers();





#ifdef _ADDRESS_MODEL_64
    static const uintptr_t alignment         = 16; // 16 byte alignment for all pointers
    //! Fixnum definition for 64 bit system
    typedef long int Fixnum;
    typedef Fixnum cl_fixnum;
    static const int fixnum_bits = 63;
    static const int fixnum_shift = 1;
    static const long int mostPositiveFixnum =  4611686018427387903;
    static const long int mostNegativeFixnum = -4611686018427387904;
#endif
#ifdef _ADDRESS_MODEL_32
#error "Add support for 32 bits - squeeze, Squeeze, SQUEEZE!"
#endif
    static const uintptr_t tag_mask 	 = BOOST_BINARY(   1111);
    static const uintptr_t fixnum_tag        = BOOST_BINARY(      0); // xxx0 means fixnum
    static const uintptr_t fixnum_mask       = BOOST_BINARY(      1);
    // other == non cons
    // cons
    // Together they are objects
    
    static const uintptr_t ptr_mask         = ~BOOST_BINARY(   1111);
    static const uintptr_t other_tag        = BOOST_BINARY(   0001); // means ptr
    static const uintptr_t cons_tag          = BOOST_BINARY(   0101); // means a cons
    static const uintptr_t frame_tag         = BOOST_BINARY(   0111); // means a frame on the stack
    /*! Character */
    static const uintptr_t character_tag     = BOOST_BINARY(   1011); // Character
    static const uintptr_t character_shift = 4;


    template <class T> inline bool tagged_consp(T* ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==cons_tag);
    };

    template <class T> inline T* tag_cons(T* p) {
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(p)+cons_tag);
    }
    template <class T> inline T* untag_cons(T* ptr) {
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(ptr)-cons_tag);
    }


    template <class T> inline T* tag_other(T* p) {
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(p)+other_tag);
    }

    template <class T> inline T* tag_object(T* ptr) {
	return tag_other<T>(ptr);
    }
    template <> inline core::Cons_O* tag_object<core::Cons_O>(core::Cons_O* ptr) {
	return tag_cons<core::Cons_O>(ptr);
    }
	
    template <class T> inline T* tag_nil() {
	return tag_other(reinterpret_cast<T*>(global_voidP_nil));
    }
    template <class T> inline T* tag_unbound() {
	return tag_other(reinterpret_cast<T*>(global_voidP_unbound));
    }
    template <class T> inline T* tag_deleted() {
	return tag_other(reinterpret_cast<T*>(global_voidP_deleted));
    }
    template <class T> inline T* tag_sameAsKey() {
	return tag_other(reinterpret_cast<T*>(global_voidP_sameAsKey));
    }
    template <class T> inline T* tag_frame(core::T_O** p) {
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(p)+frame_tag);
    }

    template <class T> inline T* untag_other(T* ptr) {
	return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(ptr)-other_tag);
    }
    template <class T> inline core::T_O** untag_frame(T* ptr) {
	return reinterpret_cast<core::T_O**>(reinterpret_cast<uintptr_t>(ptr)-frame_tag);
    }


    template <class T> inline T* tag_fixnum(Fixnum fn) {
	return reinterpret_cast<T*>((fn<<fixnum_shift));
    }
    template <class T> inline int untag_fixnum(T* const ptr)  {
	return (Fixnum)(reinterpret_cast<uintptr_t>(ptr)>>fixnum_shift);
    }
    template <class T> inline bool tagged_fixnump(T* ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&fixnum_mask)==fixnum_tag);};
    template <class T> inline T* tag_character(int ch) {
	return reinterpret_cast<T*>((ch<<character_shift)|character_tag);
    }
    template <class T> inline int untag_character(T* ptr) {
	return (int)(reinterpret_cast<uintptr_t>(ptr)>>character_shift);
    }
    template <class T> inline bool tagged_characterp(T* ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==character_tag);
    };
    template <class T> inline bool tagged_otherp(T* ptr) {
	return ((uintptr_t)(ptr)&tag_mask)==other_tag;
    }
    template <class T> inline bool tagged_nilp(T* ptr) {
	return (reinterpret_cast<void*>(ptr) == global_voidP_nil);
    }
    template <class T> inline bool tagged_unboundp(T* ptr) {
	return (reinterpret_cast<void*>(ptr) == global_voidP_unbound);
    }
    template <class T> inline bool tagged_deletedp(T* ptr) {
	return (reinterpret_cast<void*>(ptr) == global_voidP_deleted);
    }
    template <class T> inline bool tagged_sameAsKeyp(T* ptr) {
	return (reinterpret_cast<void*>(ptr) == global_voidP_sameAsKey);
    }
    template <class T> inline bool tagged_framep(T* ptr) {
	return ((reinterpret_cast<uintptr_t>(ptr)&tag_mask)==frame_tag);
    };





    

    template <class T>
	class smart_ptr /*: public tagged_ptr<T>*/ {
    private:
	T* theObject;
    public:
	//Default constructor, set theObject to NULL
    smart_ptr() : theObject(NULL) {};
	//    	explicit smart_ptr(uintptr_t p) : theObject(p) {}; // TODO: this converts ints to smart_ptr's - its dangerous
	//! Construct a FRAME object - I need to get rid of these
	//smart_ptr( core::T_O** p ) : theObject(tag_frame(p)) { /*printf("%s:%d Creating Frame \n", __FILE__, __LINE__ );*/ };
	//smart_ptr( T* objP) : theObject(tag_object(objP)) {};
	// explicit smart_ptr( void* objP) : theObject(reinterpret_cast<T*>(objP)) {};

	explicit inline smart_ptr(T* ptr) : theObject(tag_other<T>(ptr)) {};
	
	inline smart_ptr(const smart_ptr<T>& obj) : theObject(obj.theObject) {};

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
	inline static smart_ptr<T> make_tagged_fixnum(int val) {return smart_ptr<T>(tag_fixnum<T>(val));}
	inline static smart_ptr<T> make_tagged_other(T* p) {return smart_ptr<T>(tag_other<T>(p));}
	inline static smart_ptr<T> make_tagged_nil() {return smart_ptr<T>(tag_nil<T>());};
	inline static smart_ptr<T> make_tagged_unbound() {return smart_ptr<T>(tag_unbound<T>());};

	/*! Get the pointer typcast to an integer quantity for hashing */
	cl_intptr_t intptr() const { return ((uintptr_t)(this->theObject));};


	template <class o_class>
	    inline smart_ptr<o_class> pointerAsUnsafe()
	    {
		o_class* new_px = dynamic_cast<o_class*>(this->px);
		return smart_ptr<o_class>(new_px);
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
		smart_ptr<o_class> ret(tag_other<o_class>(gctools::DynamicCast<o_class*,T*>::castOrNULL(untag_other<T>(this->theObject))));
		return ret;
	    } else if (this->consp()) {
		smart_ptr<o_class> ret(tag_cons<o_class>(gctools::DynamicCast<o_class*,T*>::castOrNULL(untag_cons<T>(this->theObject))));
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
		smart_ptr<o_class> ret(tag_other(gctools::DynamicCast<o_class*,T*>::castOrNULL(untag_other<T>(this->theObject))));
		return ret;
	    } else if (this->consp()) {
		smart_ptr<o_class> ret(tag_cons(gctools::DynamicCast<o_class*,T*>::castOrNULL(untag_cons<T>(this->theObject))));
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
		if (!ret)
		    {
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
		if (!ret)
		    {
			class_id expected_typ = reg::registered_class<o_class>::id;
			class_id this_typ = reg::registered_class<T>::id;
			lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->px));
		    }
		return ret;
	    }
#if 0
	/*! Downcast to o_class - if ptr cannot be downcast throw exception.
	  If the ptr is nil, return a nil ptr */
	template <class o_class>
	    inline smart_ptr<o_class> as_or_nil()
	    {
		if ( this->nilp() ) {
		    smart_ptr<o_class> nil(smart_ptr<o_class>::tagged_nil);
		    return nil;
		}
		smart_ptr<o_class> ret = this->asOrNull<o_class>();
		if (!ret)
		    {
			class_id expected_typ = reg::registered_class<o_class>::id;
			class_id this_typ = reg::registered_class<T>::id;
			lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->px));
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
		    lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->px));
		}
		return ret;
	    }

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
	    GCTOOLS_ASSERT(this->tagged_otherp(this->theObject));
	    return untag_other(this->theObject);
	};

	T* operator->() const {
	    GCTOOLS_ASSERT(this->tagged_otherp(this->theObject));
	    return untag_other(this->theObject);
	};

	T* untag_object() const {
	    GCTOOLS_ASSERT(this->otherp()||this->consp());
	    if ( this->otherp() ) {
		return untag_other<T>(this->theObject);
	    } else if ( this->consp() ) {
		return untag_cons<T>(this->theObject);
	    } 		
	    GCTOOLS_ASSERT(false);
	}

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
	operator bool() const { return this->theObject != NULL; };
	
	bool nilp() const { return tagged_nilp(this->theObject); }
	bool notnilp() const { return (!this->nilp());};
	bool isTrue() const { return !this->nilp(); };
	bool fixnump() const {return tagged_fixnump(this->theObject);};
	bool unboundp() const { return tagged_unboundp(this->theObject);};
	bool characterp() const {return tagged_characterp<T>(this->theObject);};
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

	bool framep() const { return smart_ptr<T*>::tagged_framep(this->theObject);};
	core::T_O** unsafe_frame() const { return untag_frame(this->theObject); };
	core::T_O** safe_frame() const { GCTOOLS_ASSERT(this.framep); return this->unsafe_frame(); };

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
	*/
	T*& rawRef_() { return this->theObject; };

	
	/*! Check if this tagged theObject matches the templated type.
	  The most common case is this is an object.*/
	bool valid() const {
	    GCTOOLS_ASSERT(false); // BF("Implement me"));
	}
	    
    };

};

namespace gctools {
    class Fixnum_I;

    template <>
	class smart_ptr<core::Cons_O> {
    private:
	core::Cons_O* theObject;
    public:
	inline bool consp() const { return tagged_consp(this->theObject) || tagged_nilp(this->theObject);};
	inline bool nilp() const { return tagged_nilp(this->theObject); };
	inline bool notnilp() const { return this->nilp(); };

	inline bool valid() const { return this->consp() || this->nilp(); };


		/*! Dereferencing operator - remove the other tag */
	inline core::Cons_O* operator->() {
	    GCTOOLS_ASSERT(this->tagged_consp(this->theObject));
	    return untag_cons(this->theObject);
	};
	inline core::Cons_O* operator->() const {
	    GCTOOLS_ASSERT(this->tagged_consp(this->theObject));
	    return untag_cons(this->theObject);
	};

    };

};


template <class T>
gctools::smart_ptr<T> _Nil()
{
    gctools::smart_ptr<T> x(gctools::tag_nil<T>());
    return x;
}



template <class T>
gctools::smart_ptr<T> _Unbound()
{
    gctools::smart_ptr<T> x(gctools::tag_unbound<T>());
    return x;
}

template <class T>
gctools::smart_ptr<T> _Deleted()
{
    gctools::smart_ptr<T> x(gctools::smart_ptr<T>::tagged_deleted);
}



template <class T>
inline bool Null(const gctools::smart_ptr<T>& ptr) { return ptr.nilp();};


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
