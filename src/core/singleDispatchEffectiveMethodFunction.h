#ifndef _core_singleDispatchEffectiveMethodFunction_H_
#define _core_singleDispatchEffectiveMethodFunction_H_

#include "core/foundation.h"
#include "core/object.h"
#include "core/executables.h"
#include "core/singleDispatchEffectiveMethodFunction.fwd.h"



namespace core
{
    class SingleDispatchEffectiveMethodFunction_O : public Function_O
    {
	LISP_BASE1(Function_O);
	LISP_CLASS(core,CorePkg,SingleDispatchEffectiveMethodFunction_O,"SingleDispatchEffectiveMethodFunction");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(SingleDispatchEffectiveMethodFunction_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit SingleDispatchEffectiveMethodFunction_O(core::Class_sp const& mc) : T_O(mc), T(mc) {};
//    virtual ~SingleDispatchEffectiveMethodFunction_O() {};
    public:
	static SingleDispatchEffectiveMethodFunction_sp create(Cons_sp methods,Lisp_sp lisp);
    public:
	void initialize();
	
    private: // instance variables here
	Cons_sp	_Methods;

    private:
	T_mv INVOKE(int nargs, ArgArray args);
	
    public: // Functions here

	string __repr__() const;


    }; // SingleDispatchEffectiveMethodFunction class
    
}; // core namespace
TRANSLATE(core::SingleDispatchEffectiveMethodFunction_O);





#endif // _core_singleDispatchEffectiveMethodFunction_H_
