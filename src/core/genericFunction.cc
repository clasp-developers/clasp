#define DEBUG_LEVEL_FULL

#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/instance.h"
#include "core/primitives.h"
#include "core/evaluator.h"
#include "core/multipleValues.h"
#include "core/predicates.h"
#include "core/vectorObjectsWithFillPtr.h"
#include "core/cache.h"
#include "symbolTable.h"
#include "genericFunction.h"
#include "core/wrappers.h"

#define CACHE_METHOD_LOOKUP


namespace core
{

    /*! Trying to figure out dispatching.
      ecl/src/c/eval.d:68 - calls functions in instance.entry
	case t_instance:
		switch (fun->instance.isgf) {
		case ECL_STANDARD_DISPATCH:
		case ECL_RESTRICTED_DISPATCH:
			return _ecl_standard_dispatch(frame, fun);
		case ECL_USER_DISPATCH:
			fun = fun->instance.slots[fun->instance.length - 1];
                        goto AGAIN;
		case ECL_READER_DISPATCH:
		case ECL_WRITER_DISPATCH:
			return APPLY(narg, fun->instance.entry, sp);
		default:
			FEinvalid_function(fun);
		}


In ecl/src/c/interpreter.d  is the following code
		case t_instance:
			switch (reg0->instance.isgf) {
			case ECL_STANDARD_DISPATCH:
			case ECL_RESTRICTED_DISPATCH:
				reg0 = _ecl_standard_dispatch(frame, reg0);
				break;
			case ECL_USER_DISPATCH:
				reg0 = reg0->instance.slots[reg0->instance.length - 1];
				goto AGAIN;
			case ECL_READER_DISPATCH:
			case ECL_WRITER_DISPATCH:
				the_env->function = reg0;
				reg0 = APPLY(narg, reg0->instance.entry, frame_aux.base);
				break;
			default:
				FEinvalid_function(reg0);
			}
			break;
    */





    Cons_sp frame_to_classes(int nargs, ArgArray args)
    {
	Cons_sp arglist = _Nil<Cons_O>();
	for ( int p=nargs-1; p>=0; --p)
	{
	    T_sp co = af_classOf(args[p]);
#if DEBUG_CLOS>=2
	    printf("MLOG frame_to_classes class[%d] --> %lX  --> %s\n",
		   p, co.intptr(), co->__repr__().c_str() );
#endif
	    arglist = Cons_O::create(co,arglist);
	}
	return arglist;
    }


#if 0
    Cons_sp frame_to_list(int nargs, ArgArray args )
    {
	Cons_sp arglist = _Nil<Cons_O>();
	for ( int p=nargs-1; p>=0; --p)
	{
	    T_sp co = args[p];
#if DEBUG_CLOS>=2
	    printf("MLOG frame_to_list p[%d] --> %lX  --> %s\n",
		   p, co.intptr(), co->__repr__().c_str() );
#endif
	    arglist = Cons_O::create(co,arglist);
	}
	return arglist;
    }
#endif


    SYMBOL_EXPORT_SC_(ClPkg,compute_applicable_methods);
    SYMBOL_SC_(ClosPkg,compute_applicable_methods_using_classes);
    SYMBOL_SC_(ClosPkg,compute_effective_method_function);

    /*! This function copies ECL>>gfun.d generic_compute_applicable_method */
    T_mv generic_compute_applicable_method(int nargs, ArgArray args,
					   const Instance_O& gf)
    {
	/* method not cached */
	//cl_object memoize;
	T_sp memoize;
	T_mv methods = eval::funcall(clos::_sym_compute_applicable_methods_using_classes,
				     gf.const_sharedThis<Instance_O>(),frame_to_classes(nargs,args));
	memoize = methods.valueGet(1); // unlikely_if (Null(memoize = env->values[1])) {
	if (memoize.nilp())
	{
	    T_sp arglist = lisp_ArgArrayToCons(nargs,args); // used to be frame_to_list
	    methods = eval::funcall(cl::_sym_compute_applicable_methods,
				    gf.const_sharedThis<Instance_O>(),arglist);
	    if ( methods.nilp() )
	    {
		SYMBOL_EXPORT_SC_(ClPkg,no_applicable_method);
		T_sp func = eval::funcall(cl::_sym_no_applicable_method,
					  gf.const_sharedThis<Instance_O>(),arglist);
		args[0] = _Nil<T_O>();
		return(Values(func,_Nil<T_O>()));
	    }
	}
	methods = eval::funcall(clos::_sym_compute_effective_method_function,
				gf.const_sharedThis<Instance_O>(),gf.GFUN_COMB(),methods);
	return(Values(methods,_lisp->_true()));
    }

    SYMBOL_SC_(ClosPkg,std_compute_applicable_methods);
    SYMBOL_SC_(ClosPkg,std_compute_effective_method);

    /*! This function copies ECL>>gfun.d restricted_compute_applicable_method */
    T_mv restricted_compute_applicable_method(int nargs, ArgArray args,
					      const Instance_O& gf)
    {
	Instance_sp igf = gf.const_sharedThis<Instance_O>();
	/* method not cached */
	T_sp arglist = lisp_ArgArrayToCons(nargs,args); // used to be frame_to_list
	T_sp methods = eval::funcall(clos::_sym_std_compute_applicable_methods,igf,arglist);
	if ( methods.nilp() )
	{
	    Function_sp func = eval::funcall(cl::_sym_no_applicable_method,
					     igf,arglist).as<Function_O>();
	    args[0] = _Nil<T_O>();
	    return(Values(func,_Nil<T_O>()));
	}
	methods = eval::funcall(clos::_sym_std_compute_effective_method,igf,gf.GFUN_COMB(),methods);
	return(Values(methods,_lisp->_true()));
    }





    T_mv compute_applicable_method(int nargs, ArgArray args, T_sp gf)
{
    if (gf.as<Instance_O>()->isgf() == ECL_RESTRICTED_DISPATCH )
    {
	return restricted_compute_applicable_method(nargs,args, *(gf.as<Instance_O>().get()));
    } else
    {
	return generic_compute_applicable_method(nargs,args, *(gf.as<Instance_O>().get()));
    }
}







/*! Mimic ECL>>gfun.d fill_spec_vector */
    VectorObjectsWithFillPtr_sp fill_spec_vector(VectorObjectsWithFillPtr_sp vektor,
						  int nargs, ArgArray args, T_sp& gf)
    {
#if DEBUG_CLOS>=2
	printf("MLOG fill_spec_vector - entered with gf  %s\n", gf.as<Instance_O>()->GFUN_NAME()->__repr__().c_str() );
#endif
	int narg = nargs;
	T_sp spec_how_list = gf.as<Instance_O>()->GFUN_SPECIALIZERS();
	// cl_object *argtype = vector->vector.self.t; // ??
	VectorObjectsWithFillPtr_sp argtype = vektor;
	int spec_no = 1;
	argtype->operator[](0) = gf;
#if DEBUG_CLOS>=2
	printf("MLOG fill_spec_vector - writing to argtype[%d] at %p wrote: %lX\n",
	       0, argtype->operator[](0).px_address(), argtype->operator[](0).intptr());
#endif
	for ( ; spec_how_list.notnilp(); spec_how_list=cCdr(spec_how_list.as_or_nil<Cons_O>()) )
	{
	    Cons_sp spec_how = oCar(spec_how_list).as_or_nil<Cons_O>();
	    T_sp spec_type = oCar(spec_how);
	    int spec_position = oCdr(spec_how).as<Fixnum_O>()->get();
	    if ( spec_position >= narg )
	    {
		SIMPLE_ERROR(BF("Wrong number of arguments"));
	    } else if ( spec_no >= vektor->dimension() )
	    {
		SIMPLE_ERROR(BF("Too many arguments to fill_spec_vector()"));
	    }
	    if (!af_listp(spec_type) || 
		spec_type.as_or_nil<Cons_O>()->memberEql(args[spec_position]).nilp() )
	    {
		Class_sp mc = lisp_instance_class(args[spec_position]);
#if DEBUG_CLOS>=2
		printf("MLOG fill_spec_vector argtype[%d] using class_of(args[%d]): %s\n", spec_no, spec_position, mc->__repr__().c_str() );
#endif
		argtype->operator[](spec_no) = mc;
	    } else
	    {
#if DEBUG_CLOS>=2
		printf("MLOG fill_spec_vector argtype[%d] using args[%d]\n", spec_no, spec_position );
#endif
		argtype->operator[](spec_no) = args[spec_position];
	    }
#if DEBUG_CLOS>=2
	    printf("MLOG fill_spec_vector - from arg[%d] val=%lX writing to argtype[%d] at %p wrote: %lX --> argtype/%s",
		   spec_position, args[spec_position].intptr(), spec_no,argtype->operator[](spec_no).px_address(),argtype->operator[](spec_no).intptr(), argtype->operator[](spec_no)->__repr__().c_str());
	    if ( args[spec_position]->consP() )
	    {
		printf( " arg/CONS...\n");
	    } else
	    {
		printf( " arg/%s\n" , args[spec_position]->__repr__().c_str());
	    }
#endif
	    ++spec_no;
	}
	vektor->setf_fillPointer(spec_no);
	return vektor;
    }




    T_mv standard_dispatch(T_sp gf, int nargs, ArgArray args)
    {
	Function_sp func;
#if defined(CACHE_METHOD_LOOKUP)
        gctools::StackRootedPointer<Cache> cache(_lisp->methodCachePtr());
	VectorObjectsWithFillPtr_sp vektor = fill_spec_vector(cache->keys(), nargs, args, gf); // Was ref
        gctools::StackRootedPointer<CacheRecord> e;
        try {
            cache->search_cache(e); // e = ecl_search_cache(cache);
        } catch (CacheError& err) {
            SIMPLE_ERROR(BF("Try #1 generic function cache search error looking for %s") % _rep_(gf));
        }
	ASSERT(!e.nullP());
	if (e->_key.notnilp()) {
	    func = e->_value.as<Function_O>();
	} else
	{
	    /* The keys and the cache may change while we
	     * compute the applicable methods. We must save
	     * the keys and recompute the cache location if
	     * it was filled. */
	    T_mv mv = compute_applicable_method( nargs, args, gf);
	    func = mv.as<Function_O>();
	    if (mv.valueGet(1).notnilp() )
	    {
		Sequence_sp keys = vektor->subseq(0,_Nil<T_O>());
		if (e->_key.notnilp())
		{
                    try {
                        cache->search_cache(e); // e = ecl_search_cache(cache);
                    } catch (CacheError& err) {
                        SIMPLE_ERROR(BF("Try #2 generic function cache search error looking for %s") % _rep_(gf));
                    }
		}
		e->_key = keys;
		e->_value = func;
	    }
	}
#else
	T_mv applicable_method = compute_applicable_method(frame,gf);
	// This is where it fails because applicable_method does not downcast to a Function
	func = applicable_method.as<Function_O>();
#endif
            // TODO: This is used for all generic function calls - is there a better way than copying the ValueFrame??????
	ValueFrame_sp frame(ValueFrame_O::createForArgArray(nargs,args,_Nil<ActivationFrame_O>()));
	return eval::funcall(func,frame,_Nil<T_O>());
    }















    /*! Reproduces functionality in generic_function_dispatch_vararg
static cl_object
generic_function_dispatch_vararg(cl_narg narg, ...)
{
        cl_object output;
        ECL_STACK_FRAME_VARARGS_BEGIN(narg, narg, frame) {
		output = _ecl_standard_dispatch(frame, frame->frame.env->function);
	} ECL_STACK_FRAME_VARARGS_END(frame);
        return output;
}
 */
 T_mv generic_function_dispatch( const Instance_O& gf, int nargs, ArgArray args)
    {
	return standard_dispatch(gf.const_sharedThis<Instance_O>(),nargs,args);
    }


    
    /*! Reproduces functionality in ecl_slot_reader_dispatch */
T_mv slotReaderDispatch( const Instance_O& gf, int nargs, ArgArray args )
    {
	IMPLEMENT_MEF(BF("Implement slotReaderDispatch"));
    }



    /*! Reproduces functionality in ecl_slot_writer_dispatch */
T_mv slotWriterDispatch( const Instance_O& gf, int nargs, ArgArray args )
    {
	IMPLEMENT_MEF(BF("Implement slotWriterDispatch"));
    }

    /*! Reproduces functionality in user_function_dispatch */
T_mv userFunctionDispatch( const Instance_O& gf, int nargs, ArgArray args )
    {
	IMPLEMENT_MEF(BF("Implement userFunctionDispatch"));
    }

    /*! Reproduces functionality in FEnot_funcallable_vararg */
T_mv notFuncallableDispatch( const Instance_O& gf, int nargs, ArgArray args )
    {
	IMPLEMENT_MEF(BF("Implement notFuncallableDispatch"));
    }




    
    
#define ARGS_af_clearGfunHash "(what)"
#define DECL_af_clearGfunHash ""
#define DOCS_af_clearGfunHash "See ecl/src/c/gfun.d:si_clear_gfun_hash. This function clears the generic function call hashes selectively. If what=T then clear the hash completely.  If what=generic_function then clear only these entries."
    void af_clearGfunHash(T_sp what)
    {_G();
	ASSERT(_lisp->methodCachePtr());
	ASSERT(_lisp->slotCachePtr());
	_lisp->methodCachePtr()->removeOne(what);
	_lisp->slotCachePtr()->removeOne(what);
    };



    void initialize_genericFunction()
    {
	SYMBOL_SC_(ClosPkg,clearGfunHash);
	Defun(clearGfunHash);
    }



};
