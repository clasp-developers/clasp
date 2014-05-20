#ifndef ExternalObject_H
#define ExternalObject_H


#include "foundation.h"
#include "object.h"
#include "standardObject.h"

#if 0
namespace core
{
    SMART(ExternalObjectManager);
    c l a s s ExternalObjectManager_O : public T_O
    {
	L I S P _BASE1(T_O);
	L I S P _CLASS(core,CorePkg,ExternalObjectManager_O,"ExternalObjectManager");
	void	initialize();
    private:
	map<void*,ExternalObject_sp>	_ExternalPointersToObjects;
    public:
	void registerExternal(void* ptr, ExternalObject_sp obj, Lisp_sp lisp);
	bool recognizesExternal(void* ptr);
	ExternalObject_sp objectForExternal(void* ptr);

	DEFAULT_CTOR_DTOR(ExternalObjectManager_O);
    };
TRANSLATE(core::ExternalObjectManager_O);

};
#endif


namespace core {

// set this class up by hand
    SMART(ExternalObject);
    class ExternalObject_O : public T_O // StandardObject_O
    {
	LISP_BASE1(T_O); // LISP_BASE1(StandardObject_O);
	LISP_CLASS(core,CorePkg,ExternalObject_O,"ExternalObject");
    private:
        Class_sp _Class;
    public:
	virtual bool eq(T_sp obj) const;
	virtual bool isUndefined() const { return this->externalObject()==NULL; };
	virtual void* externalObject() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual void set_externalObject(void* ptr) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    public:
	explicit ExternalObject_O() : Base(), _Class(_Nil<Class_O>()) {};
	virtual ~ExternalObject_O() {};
    };



    template <class OT,class WT>
    mem::smart_ptr<OT> RP_Create_wrapped(WT ptr)
    {_G();
	GC_RESERVE_BEGIN(OT,wrapper) {
	    GC_RESERVE_GET(OT,wrapper);
	} GC_RESERVE_END(OT,wrapper);
        wrapper->set_wrapped(ptr);
	return wrapper;
    }






// public:							


#define LISP_EXTERNAL_CLASS(oNamespace,oPackage,wrappedClass,o_nameOfWrappedClass,nameOfWrappedClass,o_nameOfWrappedClassBase) \
    /* */    LISP_BASE1(o_nameOfWrappedClassBase);			\
    /* */	__COMMON_CLASS_PARTS(oNamespace,oPackage,o_nameOfWrappedClass,nameOfWrappedClass) \
public:											\
    typedef wrappedClass	WrappedClass;						\
public:											\
		/*Derived from StandardObject so it supports slots*/			\
	static bool static_supportsSlots() {return true;};				\
    	/* end */




};

TRANSLATE(core::ExternalObject_O);




namespace core {

    typedef enum { DeleteOnDtor=1, Copyable=2,  } ForeignDataFlagEnum;

// set this class up by hand
SMART(ForeignData);
    /* Maintain a pointer to a block of Foreign data that we may or may not own depending on _OwnershipFlags */


    class ForeignData_O : public ExternalObject_O // StandardObject_O
    {
	LISP_BASE1(ExternalObject_O); // LISP_BASE1(StandardObject_O);
	LISP_CLASS(core,CorePkg,ForeignData_O,"ForeignData");
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
//	string	__repr__() const;
	T_sp 				_Kind;
	int				_OwnershipFlags;
	size_t				_Size;
	void*				_Data;
    public:
	static ForeignData_sp allocateForeignObject(T_sp kind);

    public:
	template <class T>
	T data() { return reinterpret_cast<T>(this->_Data);};
    private:
	void allocate(T_sp kind, int ownershipFlags, size_t size);

	void freeForeignObject();
    public:
	explicit ForeignData_O();
	virtual ~ForeignData_O();
    };
};

TRANSLATE(core::ForeignData_O);


#endif
