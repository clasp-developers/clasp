/*
    File: genericFunction.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/instance.h>
#include <clasp/core/primitives.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/str.h>
#include <clasp/core/predicates.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/cache.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/wrappers.h>

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
    T_mv generic_compute_applicable_method(Instance_sp gf, int nargs, ArgArray args)
    {
	/* method not cached */
	//cl_object memoize;
	T_sp memoize;
	T_mv methods = eval::funcall(clos::_sym_compute_applicable_methods_using_classes,
				     gf,frame_to_classes(nargs,args));
	memoize = methods.valueGet(1); // unlikely_if (Null(memoize = env->values[1])) {
	if (memoize.nilp())
	{
	    T_sp arglist = lisp_ArgArrayToCons(nargs,args); // used to be frame_to_list
	    methods = eval::funcall(cl::_sym_compute_applicable_methods,
				    gf,arglist);
	    if ( methods.nilp() )
	    {
		SYMBOL_EXPORT_SC_(ClPkg,no_applicable_method);
		T_sp func = eval::funcall(cl::_sym_no_applicable_method,
					  gf,arglist);
		args[0] = (T_O*)(gctools::tagged_ptr<T_O>::tagged_nil);
		return(Values(func,_Nil<T_O>()));
	    }
	}
	methods = eval::funcall(clos::_sym_compute_effective_method_function,
				gf,gf->GFUN_COMB(),methods);
	return(Values(methods,_lisp->_true()));
    }

    SYMBOL_SC_(ClosPkg,std_compute_applicable_methods);
    SYMBOL_SC_(ClosPkg,std_compute_effective_method);

    /*! This function copies ECL>>gfun.d restricted_compute_applicable_method */
    T_mv restricted_compute_applicable_method(Instance_sp gf, int nargs, ArgArray args )
    {
	Instance_sp igf = gf;
	/* method not cached */
	T_sp arglist = lisp_ArgArrayToCons(nargs,args); // used to be frame_to_list
	T_sp methods = eval::funcall(clos::_sym_std_compute_applicable_methods,igf,arglist);
	if ( methods.nilp() )
	{
	    Function_sp func = eval::funcall(cl::_sym_no_applicable_method,
					     igf,arglist).as<Function_O>();
	    args[0] = (T_O*)(gctools::tagged_ptr<T_O>::tagged_nil);
	    return(Values(func,_Nil<T_O>()));
	}
	methods = eval::funcall(clos::_sym_std_compute_effective_method,igf,gf->GFUN_COMB(),methods);
	return(Values(methods,_lisp->_true()));
    }


#define ARGS_core_maybeExpandGenericFunctionArguments "(args)"
#define DECL_core_maybeExpandGenericFunctionArguments ""
#define DOCS_core_maybeExpandGenericFunctionArguments "maybeExpandGenericFunctionArguments: expands first argument into a list if it is a Frame or an ActivationFrame"
    T_sp core_maybeExpandGenericFunctionArguments(T_sp args)
    {
	if ( cl_consp(args) ) {
	    T_sp first = oCar(args);
	    if ( first.nilp() ) {
		return args;
	    } else if ( first.framep() ) {
		Cons_sp expanded = _Nil<Cons_O>();
		core::T_O** frameImpl(gctools::tagged_ptr<core::STACK_FRAME>::untagged_frame(first.px));
		frame::ElementType* values(frame::ValuesArray(frameImpl));
		for ( int i(0), iEnd(frame::ValuesArraySize(frameImpl)); i<iEnd; ++i ) {
		    expanded = Cons_O::create(gctools::smart_ptr<T_O>(values[i]),expanded);
		}
		return cl_nreverse(expanded);
	    } else {
		SIMPLE_ERROR(BF("Handle %s") % _rep_(first));
	    }
	}
	return args;
    }





    T_mv compute_applicable_method(Instance_sp gf, int nargs, ArgArray args)
{
    if (gf.as<Instance_O>()->isgf() == ECL_RESTRICTED_DISPATCH )
    {
	return restricted_compute_applicable_method(gf,nargs,args);
    } else
    {
	return generic_compute_applicable_method(gf,nargs,args);
    }
}







/*! Mimic ECL>>gfun.d fill_spec_vector */
    gctools::Vec0<T_sp>& fill_spec_vector(Instance_sp gf, gctools::Vec0<T_sp>& vektor,
						  int nargs, ArgArray args)
    {
#if DEBUG_CLOS>=2
	printf("MLOG fill_spec_vector - entered with gf  %s\n", gf->GFUN_NAME()->__repr__().c_str() );
#endif
	int narg = nargs;
	T_sp spec_how_list = gf->GFUN_SPECIALIZERS();
	// cl_object *argtype = vector->vector.self.t; // ??
	gctools::Vec0<T_sp>& argtype = vektor;
	int spec_no = 1;
	argtype[0] = gf;
	vektor.resize(1+cl_length(spec_how_list),_Nil<T_O>());
#if DEBUG_CLOS>=2
	printf("MLOG fill_spec_vector - writing to argtype[%d] at %p wrote: %lX\n",
	       0, argtype[0].px_address(), argtype[0].intptr());
#endif
	for ( ; spec_how_list.notnilp(); spec_how_list=cCdr(spec_how_list.as_or_nil<Cons_O>()) )
	{
	    Cons_sp spec_how = oCar(spec_how_list).as_or_nil<Cons_O>();
	    T_sp spec_type = oCar(spec_how);
	    int spec_position = oCdr(spec_how).as<Fixnum_O>()->get();
	    if ( spec_position >= narg )
	    {
		SIMPLE_ERROR(BF("Insufficient arguments for %s - expected specializer argument at position %d of specializer-type %s but only %d arguments were passed") % _rep_(gf) % (spec_position+1) % _rep_(spec_type) % narg );
	    } else if ( spec_no >= vektor.capacity() )
	    {
		SIMPLE_ERROR(BF("Too many arguments to fill_spec_vector()"));
	    }
	    if (!cl_listp(spec_type) || 
		spec_type.as_or_nil<Cons_O>()->memberEql(args[spec_position]).nilp() )
	    {
		Class_sp mc = lisp_instance_class(args[spec_position]);
#if DEBUG_CLOS>=2
		printf("MLOG fill_spec_vector argtype[%d] using class_of(args[%d]): %s\n", spec_no, spec_position, mc->__repr__().c_str() );
#endif
		argtype[spec_no] = mc;
	    } else
	    {
#if DEBUG_CLOS>=2
		printf("MLOG fill_spec_vector argtype[%d] using args[%d]\n", spec_no, spec_position );
#endif
		// For immediate types we need to make sure that EQL will be true
		argtype[spec_no] = args[spec_position];
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
	ASSERT(spec_no==vektor.size());
	return vektor;
    }



    // Arguments are passed in the multiple_values array
    T_mv standard_dispatch(T_sp gf)
    {
	// Save the arguments in the ArgumentsReturnValues for completing the call
#if 0
	/* Allocate a ValueFrame on the heap and copy the generic function arguments into the
	   top MultipleValues object on the stack.
	   This saves them so that later they can have a function applied to them by cl_apply.
	   This is slow because it requires allocation/garbage collection on the heap.
	*/
	ValueFrame_sp frame(ValueFrame_O::createForMultipleValues(_Nil<ActivationFrame_O>()));
#else
	/* Copy the arguments from the MultipleValues structure into the stack based tagged frame allocated below.
	   See gctools/tagged_ptr.h frame_tag
	   This saves them so that later they can have a function applied to them by cl_apply.
	*/
	MultipleValues& mv = lisp_callArgs();
	ALLOC_STACK_VALUE_FRAME_WITH_VALUES(frameImpl,frame,mv.getSize(), mv.callingArgsStart());
#endif

	/* Lookup the generic-function/arguments invocation in a cache and if an effective-method
	   exists then use that.   If an effective-method does not exist then calculate it and put it in the cache.

	   Then call the effective method with the saved arguments.
	*/
        Cache* cache(_lisp->methodCachePtr()); 
	MultipleValues& callArgs = lisp_callArgs();
	gctools::Vec0<T_sp>& vektor = fill_spec_vector(gf, cache->keys(), callArgs.getSize(), callArgs.callingArgsStart() ); 
        CacheRecord* e; //gctools::StackRootedPointer<CacheRecord> e;
        try {
            cache->search_cache(e); // e = ecl_search_cache(cache);
        } catch (CacheError& err) {
            printf("%s:%d - There was an CacheError searching the GF cache for the keys"
		   "  You should try and get into cache->search_cache to see where the error is\n", __FILE__, __LINE__);
            SIMPLE_ERROR(BF("Try #1 generic function cache search error looking for %s") % _rep_(gf));
        }
	ASSERT(e!=NULL);
	Function_sp func;
	if (e->_key.notnilp()) {
	    func = e->_value.as<Function_O>();
	} else {
	    /* The keys and the cache may change while we
	     * compute the applicable methods. We must save
	     * the keys and recompute the cache location if
	     * it was filled. */
	    T_mv mv = compute_applicable_method(gf, callArgs.getSize(), callArgs.callingArgsStart() );
	    func = mv.as<Function_O>();
	    if (mv.valueGet(1).notnilp() ) {
	      T_sp keys = VectorObjects_O::create(vektor);
		if (e->_key.notnilp()) {
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
#if 0
        if ( _sym_STARdebugGenericDispatchSTAR->symbolValue().notnilp() ) {
	    Function_sp gff = eval::lookupFunction(gf,frame);
            printf("%s:%d:%s Entered standard_dispatch  gf->%s\n",
		   __FILE__, __LINE__, __FUNCTION__, gff->closure->nameAsString().c_str());
        }
#endif
	/* The call below depends on the modified cl_apply function defined in lisp.cc
	   it is set up to accept either a ValueFrame as the second (and last) argument
	   or a StackFrame and treat it like a list of arguments.
	*/
	return eval::funcall(func,frame,_Nil<T_O>());
#if 0	
        if ( _sym_STARdebugGenericDispatchSTAR->symbolValue().notnilp() ) {
	    Function_sp gff = eval::lookupFunction(gf,frame);
            printf("%s:%d:%s Returning from standard_dispatch  gf->%s\n",
		   __FILE__, __LINE__, __FUNCTION__, gff->closure->nameAsString().c_str());
	}
	return result;
#endif	
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
 T_mv generic_function_dispatch( Instance_sp gf)
    {
	return standard_dispatch(gf);
    }


    
    /*! Reproduces functionality in ecl_slot_reader_dispatch */
T_mv slotReaderDispatch( Instance_sp gf, int nargs, ArgArray args )
    {
	IMPLEMENT_MEF(BF("Implement slotReaderDispatch"));
    }



    /*! Reproduces functionality in ecl_slot_writer_dispatch */
T_mv slotWriterDispatch( Instance_sp gf, int nargs, ArgArray args )
    {
	IMPLEMENT_MEF(BF("Implement slotWriterDispatch"));
    }

    /*! Reproduces functionality in user_function_dispatch */
T_mv userFunctionDispatch( Instance_sp gf, int nargs, ArgArray args )
    {
	IMPLEMENT_MEF(BF("Implement userFunctionDispatch"));
    }

    /*! Reproduces functionality in FEnot_funcallable_vararg */
T_mv notFuncallableDispatch( Instance_sp gf)
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
	CoreDefun(maybeExpandGenericFunctionArguments);
    }



};
