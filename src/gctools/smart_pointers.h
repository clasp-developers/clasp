       
       
//
// (C) 2004 Christian E. Schafmeister
//

#ifndef	_core_smart_pointers_H
#define	_core_smart_pointers_H


//#define POLYMORPHIC_SMART_PTR


#include <boost/utility/binary.hpp>

#include <iostream>
#if defined(USE_REFCOUNT)
 #include "tagged_intrusive_ptr.h"
 #define TAGGED_PTR_BASE tagged_intrusive_ptr
#else // boehm or mps
 #include "tagged_ptr.h"
 #define TAGGED_PTR_BASE tagged_ptr
#endif




#ifdef	REFCOUNTDEBUG //[
#define	VPRP(x,d,c)	printf(x,d,c);
#else
#define	VPRP(x,d,c)	{}
#endif //]



#define	IsUndefined(x) (x)
#define	NotUndefined(x) (!(x))

#define	_FWPLock(x)	(x)



#define	TAGGED_PTR core::T_O*


namespace gctools
{



    template <class T> class smart_ptr;
    template <class T> class weak_smart_ptr;


    void initialize_smart_pointers();

#if defined(USE_REFCOUNT)
#define SMART_PTR_BASE(T) boost::tagged_intrusive_ptr<T>
#else
#define SMART_PTR_BASE(T) tagged_ptr<T>
#endif

    template <class T>
    class smart_ptr : public SMART_PTR_BASE(T)
    {
    public:
	typedef SMART_PTR_BASE(T) BaseType;
    public:
	typedef T Type;
	typedef T* PointerType;
	static const uintptr_t _NULL = BaseType::tagged_NULL;
	static const uintptr_t nil = BaseType::tagged_nil;
	static const uintptr_t unbound = BaseType::tagged_unbound;
    public:
	smart_ptr() : BaseType() {};
	explicit smart_ptr(uintptr_t p) : BaseType(p) {}; // TODO: this converts ints to smart_ptr's - its dangerous
        smart_ptr( core::T_O** p ) : BaseType(p) {};
	smart_ptr( T* objP) : BaseType(objP) {};
	explicit smart_ptr( void* objP) : BaseType(objP) {};
	smart_ptr(const smart_ptr<T>& obj) : BaseType(obj) {};
//	smart_ptr(int f) : BaseType(f) {};    // DONT ENABLE THIS UNTIL WE ARE READY TO USE TAGGED FIXNUMS IF EVER!!!!!!    IMPLICIT CONVERTION OF INT TO SMART_PTR
	template <class Y> smart_ptr(const smart_ptr<Y>& yy) : BaseType(yy) {} ; //.get()) {};
//	template <class Y> smart_ptr(const smart_ptr<const Y>& yy) : BaseType(/*const_cast<T*>*/(yy.pxget())) {} ; //.get()) {};
#if !defined(USE_REFCOUNT)
	template <class Y>  smart_ptr(const tagged_ptr<Y>& yy) : BaseType(yy) {};
#else
	template <class Y>  smart_ptr(const boost::tagged_intrusive_ptr<Y>& yy) : BaseType(yy) {};
#endif

	/*! Get the pointer typcast to an integer quantity for hashing */
	cl_intptr_t intptr() const { return ((uintptr_t)(this->px));};

        smart_ptr<T> lock() const {
            return *this; 
        }

	template <class o_class>
            inline smart_ptr<o_class> pointerAsUnsafe()
	{
	    o_class* new_px = dynamic_cast<o_class*>(this->px);
	    return smart_ptr<o_class>(new_px);
	}

	template <class o_class>
            inline smart_ptr<o_class> pointerAsUnsafe() const
	{
	    o_class* new_px = dynamic_cast<o_class*>(this->px);
	    return smart_ptr<o_class>(new_px);
	}

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
		smart_ptr<o_class> ret(dynamic_cast<o_class*>(this->px));
		return ret;
	    }
	    if ( this->BaseType::fixnump()) {
		return smart_ptr<o_class>(reinterpret_cast<uintptr_t>(this->px));
	    }
	    if ( this->nilp()) {
		smart_ptr<o_class> nil(smart_ptr<o_class>::tagged_nil);
		return nil;
	    }
	    if ( this->unboundp() ) {
		smart_ptr<o_class> unbound(smart_ptr<o_class>::tagged_unbound);
		return unbound;
	    }
	    this->errorExpectedType<o_class>();
	    // unreachable
	    smart_ptr<o_class> fail;
	    return fail;

	}

	template <class o_class>
            inline smart_ptr<o_class> asOrNull() const
	{
	    if (this->pointerp()) {
		smart_ptr</* TODO: const */ o_class> ret(const_cast<o_class*>(dynamic_cast<const o_class*>(this->px)));
		return ret;
	    }
	    if ( this->BaseType::fixnump()) {
		return smart_ptr<o_class>(reinterpret_cast<uintptr_t>(this->px));
	    }

	    if ( this->nilp()) {
		smart_ptr<o_class> nil(smart_ptr<o_class>::tagged_nil);
		return nil;
	    }
	    if ( this->unboundp() ) {
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
	    smart_ptr<const o_class> ret = this->asOrNull<const o_class>();
	    if (!ret) return false;
	    return true;
	}


	template <class o_class>
            inline bool isA()
	{
	    smart_ptr<o_class> ret = this->asOrNull<o_class>();
	    if (!ret) return false;
	    return true;
	}



	/*! Return the offset in bytes between this.px and this - you need to modify the base
	  class of smart_ptr to make px protected */
	int offset_of_px_from_this() const { return ((char*)(&this->px)) - ((char*)(this));}
	/*! Return the size in bytes of px - you need to modify the base class
	  of smart_ptr to make px protected */
	int size_of_px() const { return sizeof(this->px); };

#ifdef POLYMORPHIC_SMART_PTR
	virtual
#endif
            int number_of_values() const { return this->_NULLp() ? 0 : 1;};

	bool notnilp() const { return (!this->nilp());};
	bool isTrue() const { return !this->nilp(); };
        bool base_fixnump() const { return this->BaseType::fixnump(); };
	bool fixnump() const {return lisp_fixnumP(*this);};
	bool characterp() const {return lisp_characterP(*this);};

        Fixnum asFixnum() const { return lisp_asFixnum(*this);};

#ifdef POLYMORPHIC_SMART_PTR
	virtual ~smart_ptr() {};
#endif
    };


};

template <class T>
gctools::smart_ptr<T> _NULL()
{
    gctools::smart_ptr<T> x(gctools::smart_ptr<T>::BaseType::tagged_NULL);
    return x;
}

template <class T>
gctools::smart_ptr<T> _Nil()
{
    gctools::smart_ptr<T> x(gctools::smart_ptr<T>::BaseType::tagged_nil);
    return x;
}



template <class T>
gctools::smart_ptr<T> _Unbound()
{
    gctools::smart_ptr<T> x(gctools::smart_ptr<T>::BaseType::tagged_unbound);
    return x;
}

template <class T>
gctools::smart_ptr<T> _Deleted()
{
    gctools::smart_ptr<T> x(gctools::smart_ptr<T>::BaseType::tagged_deleted);
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



#ifdef USE_REFCOUNT
#ifndef USE_GC_REF_COUNT_WRAPPER
#define INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_T_)		\
    namespace boost							\
    {									\
	/* non-const versions */					\
	template <>  void intrusive_ptr_add_ref<_T_ const>(_T_ const* p) {p->_ReferenceCount++;} \
	template <>  void intrusive_ptr_release<_T_ const>(_T_ const* p) {if ( --(p->_ReferenceCount) == 0 ) gctools::GCObjectAllocator<_T_>::deallocate(const_cast<_T_*>(p));} \
	template <>  void intrusive_ptr_add_ref<_T_ >(_T_* p) {p->_ReferenceCount++;} \
	template <>  void intrusive_ptr_release<_T_>(_T_* p) {if ( --(p->_ReferenceCount) == 0 ) gctools::GCObjectAllocator<_T_>::deallocate(p);} \
    }
#else
#define INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_T_)		\
    namespace boost							\
    {									\
	/* non-const versions */					\
	template <>  void intrusive_ptr_add_ref<_T_ const>(_T_ const* p) {gctools::GCWrapper<_T_>::gcAddRef(p);} \
	template <>  void intrusive_ptr_release<_T_ const>(_T_ const* p) {gctools::GCWrapper<_T_>::gcRelease(p);} \
	template <>  void intrusive_ptr_add_ref<_T_>(_T_* p) {gctools::GCWrapper<_T_>::gcAddRef(p);} \
	template <>  void intrusive_ptr_release<_T_>(_T_* p) {gctools::GCWrapper<_T_>::gcRelease(p);} \
    }
#endif
#endif






};


#endif



