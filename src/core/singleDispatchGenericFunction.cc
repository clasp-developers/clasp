


#define	DEBUG_LEVEL_FULL
#include "core/foundation.h"
#include "core/common.h"
#include "core/environment.h"
#include "symbolTable.h"
#include "core/hashTable.h"
#include "core/hashTableEql.h"
#include "core/primitives.h"
#include <core/vectorObjectsWithFillPtr.h>
#include "core/package.h"
#include <core/str.h>
#include "core/executables.h"
#include "core/multipleValues.h"
#include "core/lambdaListHandler.h"
#include "core/singleDispatchEffectiveMethodFunction.h"
#include "core/singleDispatchGenericFunction.h"
#include "core/singleDispatchMethod.h"
#include "core/wrappers.h"
#include "core/sort.h"

namespace core
{



#define DOCS_af_ensure_single_dispatch_generic_function "ensure_single_dispatch_generic_function"
#define ARGS_af_ensure_single_dispatch_generic_function "(gfname llhandler)"
#define DECL_af_ensure_single_dispatch_generic_function ""    
    void af_ensure_single_dispatch_generic_function(Symbol_sp gfname, LambdaListHandler_sp llhandler )
    {_G();
	SingleDispatchGenericFunction_sp gf = SingleDispatchGenericFunction_O::create(gfname,llhandler);
	Lisp_O::setf_find_single_dispatch_generic_function(gfname,gf);
	gfname->setf_symbolFunction(gf);
    };


#define DOCS_af_ensure_single_dispatch_method "ensure_single_dispatch_method creates a method and adds it to the single-dispatch-generic-function"
#define LOCK_af_ensure_single_dispatch_method 0
#define ARGS_af_ensure_single_dispatch_method "(gfname receiver-class &key lambda-list-handler declares (docstring \"\") body )"
#define DECL_af_ensure_single_dispatch_method ""    
    void af_ensure_single_dispatch_method(Symbol_sp gfname, Class_sp receiver_class, LambdaListHandler_sp lambda_list_handler, Cons_sp declares, Str_sp docstring, CompiledBody_sp body )
    {_G();
//	string docstr = docstring->get();
	SingleDispatchGenericFunction_sp gf = gfname->symbolFunction().as<SingleDispatchGenericFunction_O>();//Lisp_O::find_single_dispatch_generic_function(gfname,true);
	SingleDispatchMethod_sp method = SingleDispatchMethod_O::create(gfname,receiver_class,lambda_list_handler,declares,docstring,body);
        if ( !gf.pointerp() )
        {
            SIMPLE_ERROR(BF("single-dispatch-generic-function %s is not defined") % _rep_(gfname));
        }

        LambdaListHandler_sp gf_llh = gf->getLambdaListHandler();
        if (lambda_list_handler->numberOfRequiredArguments() != gf_llh->numberOfRequiredArguments() )
        {
            SIMPLE_ERROR(BF("There is a mismatch between the number of required arguments\n"
                            " between the single-dispatch-generic-function %s which expects %d arguments\n"
                            " and another method with the same name in %s which expects %d arguments\n"
                            " - this is probably due to the way you can overload method names with\n"
                            " different argument signatures in C++ which does not translate well\n"
                            " to Common Lisp.\n"
                            " --> The solution is to give the most recent Common Lisp method you defined\n"
                            " a new name by prefixing it with the class name\n"
                            " eg: getFilename -> PresumedLoc-getFilename")
                         % _rep_(gfname)
                         % gf_llh->numberOfRequiredArguments()
                         % _rep_(receiver_class)
                         % lambda_list_handler->numberOfRequiredArguments() );
        }
	gf->defmethod(gfname,method);
    };

    
// ----------------------------------------------------------------------
//
    
    EXPOSE_CLASS(core,SingleDispatchGenericFunction_O);
    
    void SingleDispatchGenericFunction_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<SingleDispatchGenericFunction_O>()
//	    .def_readonly("single_dispatch_generic_function_methods",&SingleDispatchGenericFunction_O::_Methods,"Access the methods of the single-dispatch-generic-function")
//	.initArgs("(self)")
//	    .def("single-dispatch-generic-function-dispatch-on-index",&SingleDispatchGenericFunction_O::dispatch_on_index)
	    .def("SingleDispatchGenericFunction-methods",&SingleDispatchGenericFunction_O::methods)
	    ;
	SYMBOL_SC_(CorePkg,ensure_single_dispatch_generic_function);
	Defun(ensure_single_dispatch_generic_function);
	SYMBOL_SC_(CorePkg,ensure_single_dispatch_generic_function);
	Defun(ensure_single_dispatch_method);
    }
    
    void SingleDispatchGenericFunction_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),SingleDispatchGenericFunction,"","",_LISP)
//	.initArgs("(self)")
	    ;
#endif
    }




    SingleDispatchGenericFunction_sp SingleDispatchGenericFunction_O::create(T_sp name, LambdaListHandler_sp llh)
    {_G();
	GC_RESERVE_BEGIN(SingleDispatchGenericFunction_O,gf ){
	    GC_RESERVE_GET(SingleDispatchGenericFunction_O,gf );
	} GC_RESERVE_END(SingleDispatchGenericFunction_O,gf );
	gf->setFunctionName(name);
	gf->_Kind = kw::_sym_function;
	gf->_LambdaListHandler = llh;
	return gf;
    }
    
    ::core::T_sp SingleDispatchGenericFunction_O::__init__(::core::Function_sp exec, ::core::Cons_sp args, ::core::Environment_sp env, ::core::Lisp_sp lisp)
    {_G();
//      this->Base::__init__(exec,args,env,lisp);
//      arg = translate::from_object<XXXX>::convert(env->lookup(this->Package(),"YYY"));
	return _Nil<T_O>();
    }
    
#if 0
#if defined(OLD_SERIALIZE)
    void SingleDispatchGenericFunction_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif
#endif    
    
    
    void SingleDispatchGenericFunction_O::initialize()
    {_OF();
        this->Base::initialize();
	this->_Methods = _Nil<Cons_O>();
	this->_classes_to_emf_table = HashTableEql_O::create_default();
    }



    void SingleDispatchGenericFunction_O::defmethod(Symbol_sp name, SingleDispatchMethod_sp method)
    {_OF();
	// Look to see if the method is already defined
	LOG(BF("defmethod for symbol[%s] called with method with receiverClass[%s]")
	    % _rep_(name) % _rep_(method->receiver_class()) );
	bool replacedMethod = false;
	{_BLOCK_TRACEF(BF("Checking if the receiver class already has a method"));
	    for ( Cons_sp cur = this->_Methods; cur.notnilp(); cur=cCdr(cur) )
	    {
		SingleDispatchMethod_sp existing = oCar(cur).as<SingleDispatchMethod_O>();
		LOG(BF("An existing method has receiverClass[%s]") % _rep_(existing->receiver_class()) );
		if ( existing->receiver_class() == method->receiver_class() )
		{
                    cur->setCar(method);
                    replacedMethod = true;
                    break;
//		    SIMPLE_ERROR(BF("You tried to overwrite a locked method with name[%s]") % _rep_(name));
		}
	    }
	}
	if ( !replacedMethod )
	{
	    LOG(BF("This is a new method - adding it to the Methods list"));
	    this->_Methods = Cons_O::create(method,this->_Methods);
	}
	this->_classes_to_emf_table->clrhash();
    }




    /*! I think this fills the role of the lambda returned by
      std-compute-discriminating-function (gf) AMOP-303 top
    */
    T_mv SingleDispatchGenericFunction_O::INVOKE(int nargs, ArgArray args)
    {_OF();
	if ( nargs <= 0 ) {
	    SIMPLE_ERROR(BF("Insufficient arguments %d for single-dispatch-generic-function - dispatching on %d") % nargs % 0 );
	}
	T_sp dispatchArg = args[0];
	ASSERTF(!dispatchArg.unboundp(),BF("The dispatch object is UNBOUND"));
	LispCompiledFunctionIHF _frame(_lisp->invocationHistoryStack(),this->asSmartPtr());
	Class_sp dispatchArgClass = lisp_instance_class(dispatchArg);
	ASSERTF(!dispatchArgClass.nilp(),BF("The dispatch class is NIL!!!! for dispatch obj: %s") % _rep_(dispatchArg));
	LOG(BF("Invoked SingleDispatchGenericFunction[%s] with receiver class[%s]")
	    % _rep_(this->getFunctionName()) % _rep_(dispatchArgClass) );
	Function_sp emf =
	    this->_classes_to_emf_table->gethash(dispatchArgClass,_Nil<SingleDispatchEffectiveMethodFunction_O>()).as<Function_O>();
	if ( emf.nilp() )
	{
	    LOG(BF("There was no effective method function defined - building one"));
	    emf = this->slow_method_lookup(dispatchArgClass);
	    this->_classes_to_emf_table->hash_table_setf_gethash(dispatchArgClass,emf);
//	    printf( "%s:%d - performed slow_method_lookup for sdgf: %s dispatchArgClass: %s\n", __FILE__, __LINE__, this->getFunctionName()->__repr__().c_str(), dispatchArgClass->__repr__().c_str() );
	} else
	{
//	    printf( "%s:%d - used cached sdgf: %s dispatchArgClass: %s\n", __FILE__, __LINE__, this->getFunctionName()->__repr__().c_str(), dispatchArgClass->__repr__().c_str() );
	    LOG(BF("Found hashed effective method - using that"));
	}
	LOG(BF("Invoking the effective method function"));
	return emf->INVOKE(nargs,args);
    };


    class SingleDispatch_OrderByClassPrecedence
    {
    public:
	bool operator()(T_sp const& x, T_sp const& y )
	{
            SingleDispatchMethod_sp sx = x.as<SingleDispatchMethod_O>();
            SingleDispatchMethod_sp sy = y.as<SingleDispatchMethod_O>();
	    return sx->receiver_class()->isSubClassOf(sy->receiver_class()); // or should it be y->isSubClassOf(x)?????
	}
    };  

    Function_sp SingleDispatchGenericFunction_O::slow_method_lookup(Class_sp mc)
    {_OF();
	LOG(BF("Looking for applicable methods for receivers of class[%s]") % _rep_(mc) );
        VectorObjects_sp applicableMethods = VectorObjectsWithFillPtr_O::make(_Nil<T_O>(),_Nil<Cons_O>(),16,0,true);
	for ( Cons_sp cur = this->_Methods; cur.notnilp(); cur=cCdr(cur) )
	{
	    SingleDispatchMethod_sp sdm = oCar(cur).as<SingleDispatchMethod_O>();
	    Class_sp ac = sdm->receiver_class();
	    if ( mc->isSubClassOf(ac) )
	    {
		LOG(BF("Found applicable method with receiver class[%s]") % _rep_(ac) );
                applicableMethods->vectorPushExtend(sdm);
            }
	}
	if ( UNLIKELY(applicableMethods->length() == 0 ))
	{
	    SIMPLE_ERROR(BF("There are no applicable methods of %s for receiver class %s")
			 % _rep_(this->getFunctionName())
			 % mc->instanceClassName() );
	}
#if 1
	/* Sort the methods from most applicable to least applicable */
	SingleDispatch_OrderByClassPrecedence sort_by_class_precedence;
	sort::quickSort(applicableMethods->begin(),applicableMethods->end(),sort_by_class_precedence);
#else // look for the most applicable method
        applicable_method = applicableMethods->elt(0).as<SingleDispatchMethod_O>();
        for ( int i(1),iEnd(applicableMethods->length()); i<iEnd; ++i ) {
            SingleDispatchMethod_sp sdm = applicableMethods->elt(i).as<SingleDispatchMethod_O>();
            if ( sdm->receiver_class()->isSubClassOf(applicable_method->receiver_class()) ) {
                applicable_method = sdm;
            }
        }
        printf("%s:%d:%s The most applicable method is for class %s\n", __FILE__,__LINE__,__FUNCTION__,_rep_(applicable_method).c_str());
#endif
	Function_sp emf = this->compute_effective_method_function(applicableMethods);
	return emf;
    }





    Lambda_emf::Lambda_emf(const string& name,
		   SingleDispatchGenericFunction_sp gf,
		   Symbol_sp emf_name,
		   SingleDispatchMethod_sp cur_method ) : Functoid("Lambda_emf->"+name)
	{_G();
	    this->_name = emf_name;
	    this->_method_function = cur_method->_method_builtin;
	}






    Function_sp SingleDispatchGenericFunction_O::compute_effective_method_function(VectorObjects_sp applicableMethods)
    {_OF();
        SingleDispatchMethod_sp cur_method = applicableMethods->elt(0).as<SingleDispatchMethod_O>();
        ASSERTF(cur_method.notnilp(),BF("There is no method to compute_effective_method_function for"));
        // Construct a name for the emf by stringing together the generic function name
        // with the name of the receiver class - this is to help with debugging
        stringstream emf_name_ss;
        string gfname = _rep_(this->getFunctionName());
        Class_sp receiverClass = cur_method->receiver_class();
        LambdaListHandler_sp method_llh = cur_method->method_lambda_list_handler();
        Symbol_sp receiverClassNameSymbol = receiverClass->className();
        string receiverClassName = receiverClassNameSymbol->symbolNameAsString();
        emf_name_ss << gfname << "->" << receiverClassName;
        Symbol_sp emf_name = _lisp->intern(emf_name_ss.str(),af_functionBlockName(this->getFunctionName())->getPackage());
        gctools::StackRootedPointer<Lambda_emf> l_emf(new Lambda_emf(emf_name_ss.str(),this->sharedThis<SingleDispatchGenericFunction_O>(), emf_name, cur_method));
        CompiledBody_sp cb_l_emf = CompiledBody_O::create(l_emf.get(),_Nil<T_O>());
        Function_sp emf = BuiltIn_O::create(emf_name,
                                            _Nil<LambdaListHandler_O>(),// LambdaListHandler_O::create(method_llh->numberOfLexicalVariables()), // _Nil<LambdaListHandler_O>(),
                                            cb_l_emf,
                                            _Nil<ActivationFrame_O>(),
                                            kw::_sym_function );
        return emf;
    }



#if 0 // old algorithm



        class Lambda_emf : public Functoid
        {
        private:
            /*! Store the name of the function that this Lambda_emf invokes - for debugging */
            Symbol_sp		_name;
            /*! Store the method_function that this emf invokes.
              This function takes two arguments: (args next-emfun) */
            Function_sp		_method_function;
            /*! Store the next-emfun that will be passed to the _method_function */
            Function_sp		_next_emfun;
        public:
            string describe() const { return "Lambda_emf";};
            bool requires_activation_frame() const { return true;};
        public:
            Lambda_emf(const string& name,
                       SingleDispatchGenericFunction_sp gf,
                       Symbol_sp emf_name,
                       SingleDispatchMethod_sp cur_method,
                       Cons_sp next_methods) : Functoid("Lambda_emf->"+name)
            {_G();
                this->_name = emf_name;
                this->_method_function = cur_method->_chainable_method_function;
                ASSERTF(this->_method_function->getLambdaListHandler().notnilp()
                        , BF("The method-function should never have a nil LambdaListHandler"));
                // Calculate the function to call with call_next_method
                // Do this by recursively calling gf->compute_effective_method_function with next_methods
                if ( next_methods.nilp() )
                {
                    this->_next_emfun = _Nil<Function_O>();
                } else
                {
                    this->_next_emfun = gf->compute_effective_method_function(next_methods);
                }
            }

            virtual T_mv activate(ActivationFrame_sp frame)
            {
                // TODO:: Here I'm converting the ActivationFrame back into a Cons and passing it
                // as the first argument to the _method_function and the second argument is _next_emfun
                // I should pass (frame) within (method_function_args) [an ActivationFrame within an ActivationFrame ]
                // Then I don't need to convert it back into an activation frame on the receiving end
                ValueFrame_sp method_function_args(ValueFrame_O::create_fill(frame,this->_next_emfun,_Nil<ActivationFrame_O>()));
                return this->_method_function->INVOKE(method_function_args);
	    
            }
        };


        Function_sp SingleDispatchGenericFunction_O::compute_effective_method_function(Cons_sp applicable_methods)
        {_OF();
            if ( applicable_methods.nilp() )
            {
                SIMPLE_ERROR(BF("You cannot compute_effective_method_function for gf[%s] because there are no methods!") % _rep_(this->getFunctionName()) );
            }
            SingleDispatchMethod_sp cur_method = oCar(applicable_methods).as<SingleDispatchMethod_O>();
            ASSERTF(cur_method.notnilp(),BF("There is no method to compute_effective_method_function for"));
            // Construct a name for the emf by stringing together the generic function name
            // with the name of the receiver class - this is to help with debugging
            stringstream emf_name_ss;
            string gfname = _rep_(this->getFunctionName());
            Class_sp receiverClass = cur_method->receiver_class();
            LambdaListHandler_sp method_llh = cur_method->method_lambda_list_handler();
            Symbol_sp receiverClassNameSymbol = receiverClass->className();
            string receiverClassName = receiverClassNameSymbol->symbolNameAsString();
            emf_name_ss << gfname << "->" << receiverClassName;
            Symbol_sp emf_name = _lisp->intern(emf_name_ss.str(),af_functionBlockName(this->getFunctionName())->getPackage());
            Lambda_emf* l_emf = _NEW_(Lambda_emf(emf_name_ss.str(),this->sharedThis<SingleDispatchGenericFunction_O>(),
                                                 emf_name, cur_method,cCdr(applicable_methods)));
            CompiledBody_sp cb_l_emf = CompiledBody_O::create(l_emf,_Nil<T_O>(),_lisp);
            Function_sp emf = BuiltIn_O::create(emf_name,
                                                _Nil<LambdaListHandler_O>(),
                                                cb_l_emf,
                                                _Nil<ActivationFrame_O>(),
                                                kw::_sym_function );
            return emf;
        }
#endif





    }; /* core */
