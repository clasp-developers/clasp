#ifndef _core_multipleDispatchEffectiveMethodFunction_H_
#define _core_multipleDispatchEffectiveMethodFunction_H_

#include "core/foundation.h"
#include "core/object.h"
#include "core/executables.h"
#include "core/multipleDispatchEffectiveMethodFunction.fwd.h"



namespace core
{
    class MultipleDispatchEffectiveMethodFunction_O : public Function_O
    {
	LISP_BASE1(Function_O);
	LISP_CLASS(CorePkg,MultipleDispatchEffectiveMethodFunction_O,"MultipleDispatchEffectiveMethodFunction");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(MultipleDispatchEffectiveMethodFunction_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit MultipleDispatchEffectiveMethodFunction_O(core::MetaClass_sp const& mc) : T_O(mc), T(mc) {};
//    virtual ~MultipleDispatchEffectiveMethodFunction_O() {};
    public:
	static MultipleDispatchEffectiveMethodFunction_sp create(Cons_sp methods,Lisp_sp lisp);
    public:
	void initialize();
	
    private: // instance variables here
	Cons_sp	_Methods;

    private:
	T_sp INVOKE(Cons_sp args);
	
    public: // Functions here

	string __repr__() const;


    }; // MultipleDispatchEffectiveMethodFunction class
    
}; // core namespace
TRANSLATE(core::MultipleDispatchEffectiveMethodFunction_O);





#endif // _core_multipleDispatchEffectiveMethodFunction_H_
