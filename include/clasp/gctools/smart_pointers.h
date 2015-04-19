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

    template <class T> class smart_ptr;

    void initialize_smart_pointers();

    template <class T>
	class smart_ptr /*: public tagged_ptr<T>*/ {
    public:
	//	static const uintptr_t tag_mask 	 = BOOST_BINARY(   1111);
        static const uintptr_t fixnum_tag        = BOOST_BINARY(      0); // xxx0 means fixnum
	static const uintptr_t fixnum_mask       = BOOST_BINARY(      1);
        static const uintptr_t fixnum_shift = 1;
	static const int fixnum_bits = 63; // TODO: Assume 64 bit system

	static const uintptr_t alignment         = 8; // 16 byte alignment for all pointers
    public:
	static const uintptr_t ptr_mask         = ~BOOST_BINARY(    111);
        static const uintptr_t object_tag        = BOOST_BINARY(    001); // means ptr
        static const uintptr_t cons_tag          = BOOST_BINARY(    011); // means a ValueFrame stored entirely on the stack
        static const uintptr_t frame_tag         = BOOST_BINARY(    101); // means a character
	
        /*! Special non-pointer tagged values */
        static const uintptr_t special_tag       = BOOST_BINARY(0000111); // means special val
        static const uintptr_t tagged_unbound    = BOOST_BINARY(0010000)|special_tag;
        static const uintptr_t tagged_nil        = BOOST_BINARY(0100000)|special_tag;
        static const uintptr_t tagged_deleted    = BOOST_BINARY(0110000)|special_tag;
        static const uintptr_t tagged_sameAsKey  = BOOST_BINARY(1000000)|special_tag;
	
	/*! Character */
	static const uintptr_t character_tag     = BOOST_BINARY(01111111);
        static const uintptr_t character_shift = 8;
    public:
        static T* make_tagged_nil() {
            return reinterpret_cast<T*>(tagged_nil);
        }
        static T* make_tagged_unbound() {
            return reinterpret_cast<T*>(tagged_unbound);
        } 
        static T* make_tagged_frame(core::T_O** p) {
            return reinterpret_cast<T*>((reinterpret_cast<uintptr_t>(p)|frame_tag));
        }
        static T* make_tagged_object(core::T_O* p) {
		return reinterpret_cast<T*>((reinterpret_cast<uintptr_t>(p)|object_tag));
        }

	static T* make_tagged_cons(core::Cons_O* p) {
            return reinterpret_cast<T*>((reinterpret_cast<uintptr_t>(p)|object_tag));
        }

        static core::T_O** untagged_frame(T* ptr) {
            return reinterpret_cast<core::T_O**>(reinterpret_cast<uintptr_t>(ptr)-frame_tag);
        }

	static core::T_O* untagged_object(T* ptr) {
            return reinterpret_cast<core::T_O*>(reinterpret_cast<uintptr_t>(ptr)-object_tag);
        }

	static core::Cons_O* untagged_cons(T* ptr) {
            return reinterpret_cast<core::Cons_O*>(reinterpret_cast<uintptr_t>(ptr)-cons_tag);
        }

	static bool tagged_fixnump(T* ptr) {
	    return ((reinterpret_cast<uintptr_t>(ptr)&fixnum_mask)==fixnum_tag);};

        static T* make_tagged_fixnum(int fn) {
            return reinterpret_cast<T*>((fn<<fixnum_shift));
        }

        static int untagged_fixnum(T* ptr) {
            return (int)(reinterpret_cast<uintptr_t>(ptr)>>fixnum_shift);
        }

	static bool tagged_objectp(T* ptr) {
            return ((uintptr_t)(ptr)&ptr_mask)==object_tag;
	}

        static bool tagged_nilp(T* ptr) {
            return ((uintptr_t)ptr == tagged_nil);
        }
        static bool tagged_unboundp(T* ptr) {
            return ((uintptr_t)ptr == tagged_unbound);
        }
    private:
	T* theObject;
    public:
    smart_ptr() {};
    	explicit smart_ptr(uintptr_t p) : theObject(p) {}; // TODO: this converts ints to smart_ptr's - its dangerous
    //! Construct a FRAME object - I need to get rid of these
 smart_ptr( core::T_O** p ) : theObject(make_tagged_frame(p)) { /*printf("%s:%d Creating Frame \n", __FILE__, __LINE__ );*/ };
 smart_ptr( T* objP) : theObject(make_tagged_object(objP)) {};








    
    explicit smart_ptr( void* objP) : theObject(reinterpret_cast<T*>(objP)) {};
    inline smart_ptr(const smart_ptr<T>& obj) : theObject(obj.theObject) {};

    // Convert one type of smart_ptr to another
    template <class Y> smart_ptr(const smart_ptr<Y>& yy) : theObject(yy.theObject) {};

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
	    lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->px));
	};

	template <class o_class>
	    inline smart_ptr<o_class> asOrNull()
	    {
		/*! this->nilp() should only return nil for Null_O,Symbol_O,List_O,Sequence_O */
		if (this->pointerp()) {
		    smart_ptr<o_class> ret(gctools::DynamicCast<o_class*,T*>::castOrNULL(this->pxget()));
		    return ret;
		} else if ( this->fixnump()) {
		    THROW_HARD_ERROR(BF("Never call asOrNull with a FIXNUM"));
		    return smart_ptr<o_class>(reinterpret_cast<uintptr_t>(this->px));
		} else if ( this->framep()) {
		    THROW_HARD_ERROR(BF("Never call asOrNull with a FRAME"));
		    return smart_ptr<o_class>(reinterpret_cast<uintptr_t>(this->px));
		} else if ( this->nilp()) {
		    return *this;
		} else if ( this->unboundp() ) {
		    return *this;
		}
		this->errorExpectedType<o_class>();
		// unreachable
		smart_ptr<o_class> fail;
		return fail;

	    }

	template <class o_class>
	    inline smart_ptr<o_class> asOrNull() const
	    {
		if (this->objectp()) {
		    smart_ptr<o_class> ret(const_cast<o_class*>(gctools::DynamicCast<const o_class*,T*>::castOrNULL(smart_ptr<T*>(untagged_object(this->theObject)))));
		    //		smart_ptr</* TODO: const */ o_class> ret(const_cast<o_class*>(dynamic_cast<const o_class*>(this->px)));
		    return ret;
		} else if ( smart_ptr<T*>::tagged_fixnump(this->theObject) ) {  // this->BaseType::fixnump()) {
		    THROW_HARD_ERROR(BF("Never call asOrNull with a FIXNUM"));
		    // return smart_ptr<o_class>(reinterpret_cast<uintptr_t>(this->px));
		} else if ( smart_ptr<T*>::tagged_framep(this->theObject) ) {
		    THROW_HARD_ERROR(BF("Never call asOrNull with a FRAME"));
		    // return smart_ptr<o_class>(reinterpret_cast<uintptr_t>(this->px));
		} else if ( this->nilp()) {
		    THROW_HARD_ERROR(BF("Never call asOrNull with a nil"));
		    smart_ptr<o_class> nil(smart_ptr<o_class>::tagged_nil);
		    return nil;
		} else if ( this->unboundp() ) {
		    THROW_HARD_ERROR(BF("Never call asOrNull with a unbound"));
		    smart_ptr<o_class> unbound(smart_ptr<o_class>::tagged_unbound);
		    return unbound;
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
			lisp_errorUnexpectedType(expected_typ,this_typ,static_cast<core::T_O*>(this->px));
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


	template <class o_class>
	    inline bool isA() const
	    {
		if ( this->pointerp() ) {
		    if ( smart_ptr<const o_class> ret = this->asOrNull<const o_class>() ) {
			return true;
		    }
		}
		return false;
	    }


	template <class o_class>
	    inline bool isA()
	    {
		if ( this->pointerp() ) {
		    if ( smart_ptr<o_class> ret = this->asOrNull<o_class>() ) {
			return true;
		    }
		}
		return false;
	    }


	/*! Return the offset in bytes between this.px and this - you need to modify the base
	  class of smart_ptr to make px protected */
	int offset_of_px_from_this() const { return ((char*)(&this->px)) - ((char*)(this));}
	/*! Return the size in bytes of px - you need to modify the base class
	  of smart_ptr to make px protected */
	int size_of_px() const { return sizeof(this->px); };

	    int number_of_values() const { return this->_NULLp() ? 0 : 1;};

	bool notnilp() const { return (!this->nilp());};
	bool isTrue() const { return !this->nilp(); };
	//	bool tagged_fixnump() const { return this->fixnump(); };
	bool fixnump() const {return smart_ptr<T*>::tagged_fixnump(this->theObject);};
	bool characterp() const {return smart_ptr<T*>::tagged_characterp(this->theObject);};
	bool NULLp() const { return this->theObject == NULL; };
	Fixnum asFixnum() const { return smart_ptr<T*>::untagged_fixnum(this->theObject);};
    };


};


template <class T>
gctools::smart_ptr<T> _Nil()
{
    gctools::smart_ptr<T> x(gctools::smart_ptr<T>::tagged_nil);
    return x;
}



template <class T>
gctools::smart_ptr<T> _Unbound()
{
    gctools::smart_ptr<T> x(gctools::smart_ptr<T>::tagged_unbound);
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





};


#endif
