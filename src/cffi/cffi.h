#ifndef _cffi_H_
#define _cffi_H_

#include "core/foundation.h"
#include "core/object.h"
#include "cffiPackage.h"
#include "cffi.fwd.h"







namespace cffi
{
    class Pointer_O : public core::T_O
    {
	LISP_BASE1(core::T_O);
	LISP_CLASS(cffi,CffiPkg,Pointer_O,"Pointer");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	explicit Pointer_O();
	virtual ~Pointer_O();
    public: // ctor/dtor for classes with shared virtual base
	
    private: // instance variables here
	void*	_ptr;
	
    public: // static functions here
	static Pointer_sp null_pointer();
	static Pointer_sp make(core::Number_sp address);
	static Pointer_sp create(void* p);
    public: // Functions here
	bool	pointerP() const { IMPLEMENT_ME();};
	bool	null_pointerP() const;

	Pointer_sp inc_pointer(core::Integer_sp offset);

	string __repr__() const;

	/*! Free memory */
	void	foreign_free();

	core::T_sp PERCENTmem_ref(core::Symbol_sp atype, core::Integer_sp offset);
	core::T_sp PERCENTsetf_mem_ref(core::Symbol_sp atype, core::Cons_sp rest);

    }; // Pointer class
    
}; // cffi namespace
TRANSLATE(cffi::Pointer_O);



namespace cffi
{
    







    void	initialize_cffi();

};
#endif /* _core_ffi_H_ */
