       
       
//
// (C) 2004 Christian E. Schafmeister
//

#ifndef	_core_smart_pointers_H
#define	_core_smart_pointers_H


//#define POLYMORPHIC_SMART_PTR


#include <boost/utility/binary.hpp>

#include <iostream>
#if defined(USE_MPS)
 #include "tagged_ptr.h"
 #define TAGGED_PTR_BASE tagged_ptr
#else
 #include "tagged_intrusive_ptr.h"
 #define TAGGED_PTR_BASE tagged_intrusive_ptr
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


namespace mem
{



    template <class T> class smart_ptr;
    template <class T> class weak_smart_ptr;


    template <class T> inline bool isNilDowncastableTo() {return false;};

    void initialize_smart_pointers();

    template <class T>
#if defined(USE_MPS)
    class smart_ptr : public mem::tagged_ptr<T>
    {
    public:
	typedef mem::tagged_ptr<T> 	BaseType;
#else
	class smart_ptr : public boost::tagged_intrusive_ptr<T>
    {
    public:
	typedef boost::tagged_intrusive_ptr<T>	BaseType;
#endif
    public:
	typedef T Type;
	typedef T* PointerType;
	static const uintptr_t _NULL = BaseType::tagged_NULL;
	static const uintptr_t nil = BaseType::tagged_nil;
	static const uintptr_t unbound = BaseType::tagged_unbound;
    public:
	smart_ptr() : BaseType() {};
	explicit smart_ptr(uintptr_t p) : BaseType(p) {}; // TODO: this converts ints to smart_ptr's - its dangerous
	smart_ptr( T* objP) : BaseType(objP) {};
	smart_ptr(const smart_ptr<T>& obj) : BaseType(obj) {};
//	smart_ptr(int f) : BaseType(f) {};    // DONT ENABLE THIS UNTIL WE ARE READY TO USE TAGGED FIXNUMS IF EVER!!!!!!    IMPLICIT CONVERTION OF INT TO SMART_PTR
	template <class Y> smart_ptr(const smart_ptr<Y>& yy) : BaseType(yy) {} ; //.get()) {};
//	template <class Y> smart_ptr(const smart_ptr<const Y>& yy) : BaseType(/*const_cast<T*>*/(yy.pxget())) {} ; //.get()) {};
#if defined(USE_MPS)
	template <class Y>  smart_ptr(const mem::tagged_ptr<Y>& yy) : BaseType(yy) {};
#else
	template <class Y>  smart_ptr(const boost::tagged_intrusive_ptr<Y>& yy) : BaseType(yy) {};
#endif
	/*! Get the pointer typcast to an integer quantity for hashing */
	cl_intptr_t intptr() const { return ((uintptr_t)(this->px));};


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
	    if ( this->nilp()
#if 0  // everything downcasts to nil
		 && isNilDowncastableTo<o_class>()
#endif
		) {
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

	    if ( this->nilp()
#if 0  // everything downcasts to nil
		 && isNilDowncastableTo<o_class>()
#endif
		) {
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
	bool isTrue() const { return (!this->nilp() || internal_isTrue(this)); };
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
mem::smart_ptr<T> _NULL()
{
    mem::smart_ptr<T> x(mem::smart_ptr<T>::BaseType::tagged_NULL);
    return x;
}

template <class T>
mem::smart_ptr<T> _Nil()
{
    mem::smart_ptr<T> x(mem::smart_ptr<T>::BaseType::tagged_nil);
    return x;
}


template <class T>
mem::smart_ptr<T> _Unbound()
{
    mem::smart_ptr<T> x(mem::smart_ptr<T>::BaseType::tagged_unbound);
    return x;
}


template <class T>
inline bool Null(const mem::smart_ptr<T>& ptr) { return ptr.nilp();};


namespace mem {

// LambdaListHandler_sp llh(ptr)

    template <class T>
#if defined(USE_MPS)
    class weak_smart_ptr : public mem::tagged_ptr<T>
    {
    public:
	typedef mem::tagged_ptr<T> 	BaseType;
#else
    class weak_smart_ptr : public boost::tagged_intrusive_ptr<T>
    {
    public:
	typedef boost::tagged_intrusive_ptr<T>	BaseType;
#endif
    public:
	weak_smart_ptr() : BaseType() {};
	weak_smart_ptr(const T& obj) : BaseType(obj) {};
	weak_smart_ptr(const smart_ptr<T>& obj_sp) : BaseType(obj_sp) {} ;
	weak_smart_ptr(T* objP) : BaseType(objP) {} ;
	template <class Y> weak_smart_ptr(const smart_ptr<Y>& yy) : BaseType(yy) {} ;
	template <class Y> weak_smart_ptr(const weak_smart_ptr<Y>& yy) : BaseType(yy) {} ;
	bool notnilp() const { return (!this->nilp());};
	smart_ptr<T> lock() const
	{
	    if ( this->nilp() ) return _Nil<T>();
	    if ( this->unboundp() ) return _Unbound<T>();
	    return smart_ptr<T>(this->get());
	}
	virtual int number_of_values() const {return 1;};
	virtual ~weak_smart_ptr() {};
    };




#if defined(USE_MPS)

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



#if !defined(USE_MPS)
#ifndef USE_GC_REF_COUNT_WRAPPER
#define INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_T_)		\
    namespace boost							\
    {									\
	/* non-const versions */					\
	template <>  void intrusive_ptr_add_ref<_T_ const>(_T_ const* p) {p->_ReferenceCount++;} \
	template <>  void intrusive_ptr_release<_T_ const>(_T_ const* p) {if ( --(p->_ReferenceCount) == 0 ) delete const_cast<_T_*>(p);} \
	template <>  void intrusive_ptr_add_ref<_T_ >(_T_* p) {p->_ReferenceCount++;} \
	template <>  void intrusive_ptr_release<_T_>(_T_* p) {if ( --(p->_ReferenceCount) == 0 ) delete p;} \
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
#endif // !defined(USE_MPS)







template <class OT>
class StackRootedPointerToSmartPtr : public gctools::StackRoot {
public:
    typedef mem::smart_ptr<OT>  SmartPtrType;
    mem::smart_ptr<OT>* _px;
public:
    StackRootedPointerToSmartPtr() : _px(NULL) {};
    StackRootedPointerToSmartPtr(SmartPtrType* p) : _px(p) {};
//    SmartPtrType operator*() const {return *this->_px;};
    void setPointee(SmartPtrType x) { *this->_px = x; };
    SmartPtrType getPointee() const { return *this->_px;};
    void setPointer(SmartPtrType* px) { this->_px = px;};
    DECLARE_onStackScanGCRoots(); 

    virtual ~StackRootedPointerToSmartPtr() {this->_px = NULL;};
};



    };

    namespace gctools {


    template <typename OT>
    class StackRootedStlContainer<vector<mem::smart_ptr<OT> > > : public gctools::StackRoot  {
    public:
        typedef vector<mem::smart_ptr<OT> >   StlContainerType;
        StlContainerType                _Container;
    public:
        StlContainerType& get() { return this->_Container;};
        GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) 
#ifndef USE_MPS
        {
            return GC_RES_OK;
        }
#else
        {
            GC_SCANNER_BEGIN() {
                for ( auto& it_gc_safe : this->_Container ) {
                    SMART_PTR_FIX(it_gc_safe);
                }
            } GC_SCANNER_END();
            return GC_RES_OK;
        }
#endif
    };




    template <typename FirstOTy, typename SecondTy>
    class StackRootedStlContainer<map<mem::smart_ptr<FirstOTy>,SecondTy> > : public gctools::StackRoot  {
    public:
        typedef map<mem::smart_ptr<FirstOTy>,SecondTy>   StlContainerType;
        StlContainerType                _Container;
    public:
        StlContainerType& get() { return this->_Container;};
        GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) 
#ifndef USE_MPS
        {
            return GC_RES_OK;
        }
#else
        {
            GC_SCANNER_BEGIN() {
                for ( auto& it_gc_safe : this->_Container ) {
                    SMART_PTR_FIX(it_gc_safe.first);
                }
            } GC_SCANNER_END();
            return GC_RES_OK;
        }
#endif
    };

    template <typename FirstTy, typename SecondOTy>
    class StackRootedStlContainer<map<FirstTy,mem::smart_ptr<SecondOTy> > > : public gctools::StackRoot  {
    public:
        typedef map<FirstTy,mem::smart_ptr<SecondOTy> >   StlContainerType;
        StlContainerType                _Container;
    public:
        StlContainerType& get() { return this->_Container;};
        GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) 
#ifndef USE_MPS
        {
            return GC_RES_OK;
        }
#else
        {
            GC_SCANNER_BEGIN() {
                for ( auto& it_gc_safe : this->_Container ) {
                    SMART_PTR_FIX(it_gc_safe.second);
                }
            } GC_SCANNER_END();
            return GC_RES_OK;
        }
#endif
    };



    template <typename FirstOTy, typename SecondOTy>
    class StackRootedStlContainer<map<mem::smart_ptr<FirstOTy>,mem::smart_ptr<SecondOTy> > > : public gctools::StackRoot  {
    public:
        typedef map<mem::smart_ptr<FirstOTy>,mem::smart_ptr<SecondOTy> >   StlContainerType;
        StlContainerType                _Container;
    public:
        StlContainerType& get() { return this->_Container;};
        GC_RESULT onStackScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) 
#ifndef USE_MPS
        {
            return GC_RES_OK;
        }
#else
        {
            GC_SCANNER_BEGIN() {
                for ( auto& it_gc_safe : this->_Container ) {
                    SMART_PTR_FIX(it_gc_safe.first);
                    SMART_PTR_FIX(it_gc_safe.second);
                }
            } GC_SCANNER_END();
            return GC_RES_OK;
        }
#endif
    };



};





#endif



