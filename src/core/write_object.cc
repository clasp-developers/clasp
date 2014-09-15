/* This is copied from ECL write_object.c and modified for C++ */

#define	DEBUG_LEVEL_FULL

#include "core/foundation.h"
#include "core/object.h"
#include "core/cons.h"
#include "core/symbolTable.h"
#include "core/str.h"
#include "core/designators.h"
#include "core/predicates.h"
#include "core/lispStream.h"
#include "core/hashTable.h"
#include "core/arguments.h"
#include <core/evaluator.h>
#include "core/write_ugly.h"
#include "core/print.h"


#include "core/character.h"


#include "core/wrappers.h"

namespace core
{


    bool will_print_as_hash(T_sp x)
    {
	T_sp circle_counter = _sym_STARcircle_counterSTAR->symbolValue();
	HashTable_sp circle_stack = _sym_STARcircle_stackSTAR->symbolValue().as<HashTable_O>();
	T_sp code = circle_stack->gethash(x,_NULL<T_O>());
	if ( circle_counter.fixnump() ) {
	    return !(code._NULLp() || code.nilp());
	} else if (code._NULLp()) {
	    /* Was not found before */
	    circle_stack->hash_table_setf_gethash(x,_Nil<T_O>());
	    return 0;
	} else {
	    return 1;
	}
    }

/* To print circular structures, we traverse the structure by adding
   a pair <element, flag> to the interpreter stack for each element visited.
   flag is initially NIL and becomes T if the element is visited again.
   After the visit we squeeze out all the non circular elements.
   The flags is used during printing to distinguish between the first visit
   to the element.
*/

    Fixnum search_print_circle(T_sp x)
    {
//        printf("%s:%d Entered search_print_circle with x.px=%p\n", __FILE__, __LINE__, x.px_ref());
	T_sp circle_counter = _sym_STARcircle_counterSTAR->symbolValue();
	HashTable_sp circle_stack = _sym_STARcircle_stackSTAR->symbolValue().as<HashTable_O>();
	T_sp code;
    	if (!circle_counter.fixnump()) {
	    code = circle_stack->gethash(x,_NULL<T_O>());
	    if (code._NULLp()) {
		/* Was not found before */
		circle_stack->hash_table_setf_gethash(x,_Nil<T_O>());
		return 0;
	    } else if (code.nilp()) {
		/* This object is referenced twice */
		circle_stack->hash_table_setf_gethash(x,_lisp->_true());
		return 1;
	    } else {
		return 2;
	    }
	} else {
	    code = circle_stack->gethash(x,_NULL<T_O>());
	    if (code._NULLp() || code.nilp()) {
		/* Is not referenced or was not found before */
		/* _ecl_sethash(x, circle_stack, ECL_NIL); */
		return 0;
	    } else if (code == _lisp->_true() ) {
		/* This object is referenced twice, but has no code yet */
		Fixnum new_code = circle_counter.as<Fixnum_O>()->get() + 1;
		circle_counter = Fixnum_O::create(new_code);
		circle_stack->hash_table_setf_gethash(x,circle_counter);
		_sym_STARcircle_counterSTAR->setf_symbolValue(circle_counter);
		return -new_code;
	    } else {
		return code.as<Fixnum_O>()->get();
	    }
	}
    }






    T_sp write_object(T_sp x, T_sp stream)
    {
#if 1 //def ECL_CMU_FORMAT   // Disable this for now - until we get Grey streams working
	if (!cl::_sym_STARprint_prettySTAR.unboundp()
            && cl::_sym_STARprint_prettySTAR->symbolValueUnsafe().notnilp() ) {
            T_sp objx = x;
	    T_mv mv_f = eval::funcall(cl::_sym_pprint_dispatch, objx);
            T_sp f0 = mv_f;
            T_sp f1 = mv_f.valueGet(1);
            if ( f1.notnilp() ) {
                eval::funcall(f0,stream,objx);
                return objx;
            }
        }
#endif /* ECL_CMU_FORMAT */
	bool circle = brcl_print_circle();
	if (circle && !x._NULLp() && !x.fixnump() && !x.framep() && !x.characterp() &&
            !x.specialp() && (af_listp(x) || !af_symbolp(x) || !x.as<Symbol_O>()->homePackage().nilp() ))
	{
	    Fixnum code;
	    T_sp circle_counter = _sym_STARcircle_counterSTAR->symbolValue();
	    if (circle_counter.nilp() ) {
		HashTable_sp hash = af_make_hash_table(cl::_sym_eq,
						       Fixnum_O::create(1024),
						       _lisp->rehashSize(),
						       _lisp->rehashThreshold() );
		DynamicScopeManager scope;
		scope.pushSpecialVariableAndSet(_sym_STARcircle_counterSTAR,_lisp->_true());
		scope.pushSpecialVariableAndSet(_sym_STARcircle_stackSTAR,hash);
		write_object(x,_lisp->nullStream());
		_sym_STARcircle_counterSTAR->setf_symbolValue(Fixnum_O::create(0));
		write_object(x, stream);
		hash->clrhash();
		goto OUTPUT;
	    }
	    code = search_print_circle(x);
	    if (!circle_counter.fixnump()) {
		/* We are only inspecting the object to be printed. */
		/* Only run X if it was not referenced before */
		if (code != 0)
		    goto OUTPUT;
	    } else if (code == 0) {
		/* Object is not referenced twice */
	    } else if (code < 0) {
		/* Object is referenced twice. We print its definition */
                stringstream ss;
                ss << '#' << -code << '=';
                Str_sp out = Str_O::create(ss.str());
                clasp_writeString(out,stream);
	    } else {
		/* Second reference to the object */
                stringstream ss;
                ss << '#' << code << '#';
                Str_sp out = Str_O::create(ss.str());
                clasp_writeString(out,stream);
		goto OUTPUT;
	    }
	}
	return write_ugly_object(x,stream);
    OUTPUT:
	return x;
    }



    
    
    
#define ARGS_af_writeObject "(obj &optional strm)"
#define DECL_af_writeObject ""
#define DOCS_af_writeObject "writeObject"
    T_sp af_writeObject(T_sp obj, T_sp ostrm)
    {_G();
        T_sp strm = coerce::outputStreamDesignator(ostrm);
        return write_object(obj,strm);
    };

    void initialize_write_object() {
        Defun(writeObject);
    }


};
