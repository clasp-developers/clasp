
#define	DEBUG_LEVEL_FULL
#include "core/common.h"
#include "core/environment.h"
#include "core/symbolTable.h"
#include "multipleDispatchMethod.h"
#include "multipleDispatchEffectiveMethodFunction.h"
#include "core/wrappers.h"
namespace core
{
    
// ----------------------------------------------------------------------
//
    
    EXPOSE_CLASS(core,multipleDispatchEffectiveMethodFunction_O);
    
    void MultipleDispatchEffectiveMethodFunction_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<MultipleDispatchEffectiveMethodFunction_O>()
//	.initArgs("(self)")
	    ;
    }
    
    void MultipleDispatchEffectiveMethodFunction_O::exposePython(::core::Lisp_sp lisp)
    {
	PYTHON_CLASS(Pkg(),MultipleDispatchEffectiveMethodFunction,"","",lisp->lisp())
//	.initArgs("(self)")
	    ;
    }
    
    
    ::core::T_sp MultipleDispatchEffectiveMethodFunction_O::__init__(::core::Executable_sp exec, ::core::Cons_sp args, ::core::Environment_sp env, ::core::Lisp_sp lisp)
    {_G();
//      this->Base::__init__(exec,args,env,lisp);
//      arg = translate::from_object<XXXX>::convert(env->lookup(this->Package(),"YYY"));
	return _lisp->onil();
    }
    
#if 0
    void MultipleDispatchEffectiveMethodFunction_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
    
    void MultipleDispatchEffectiveMethodFunction_O::archiveBase(::core::ArchiveP node)
    {
	IMPLEMENT_ME();
        this->Base1::archiveBase(node);
	// Archive other instance variables here
    }
#endif
    

    MultipleDispatchEffectiveMethodFunction_sp MultipleDispatchEffectiveMethodFunction_O::create(Cons_sp methods, Lisp_sp lisp)
    {_G();
	GC_RESERVE_TRY(MultipleDispatchEffectiveMethodFunction_O,emf ){
	    GC_RESERVE_GET(MultipleDispatchEffectiveMethodFunction_O,emf );
	    emf->_Methods = methods;
	}
	return emf;
    }


    void MultipleDispatchEffectiveMethodFunction_O::initialize()
    {_OF();
        this->Base::initialize();
    }


    string MultipleDispatchEffectiveMethodFunction_O::__repr__() const
    {_OF();
	stringstream ss;
	for (Cons_sp cur=this->_Methods; cur->notNil(); cur=cur->cdr())
	{
	    ss << "method has Receiver class[" << cur->ocar()->as<MultipleDispatchMethod_O>()->receiver_class()->__repr__() << "]" << endl;
	}
	return ss.str();
    }


    T_sp MultipleDispatchEffectiveMethodFunction_O::INVOKE(Cons_sp args)
    {_OF();
	LOG(BF("My methods --> %s") % this->__repr__() );
	IMPLEMENT_MEF(BF("Implement emf"));
    }

    
    
}; /* core */
