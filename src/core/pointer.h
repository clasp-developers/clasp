#ifndef _core_pointer_H_
#define _core_pointer_H_

#include "core/foundation.h"
#include "core/object.h"
#include "core/pointer.fwd.h"

namespace core
{
    class Pointer_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,Pointer_O,"Pointer");
//	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(Pointer_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit Pointer_O(core::Class_sp const& mc) : T_O(mc), T(mc) {};
//    virtual ~Pointer_O() {};
    public:
	void initialize();
	    
    private: // instance variables here
	void*	_Pointer;
    public:
	static Pointer_sp create(void* p);
	/*! Create a pointer to a T_sp shared-ptr */
	static Pointer_sp createForT_sp(T_sp obj);
    public: // Functions here
	void* ptr() const { return this->_Pointer;};

	virtual bool eql(T_sp obj) const;

	string __repr__() const;

    }; // Pointer class
	
}; // core namespace
    TRANSLATE(core::Pointer_O);
    
    
#endif /* _core_pointer_H_ */
