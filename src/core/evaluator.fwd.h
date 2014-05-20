#ifndef _evaluator_fwd_H_
#define _evaluator_fwd_H_

namespace core
{
    namespace eval
    {
	extern T_mv evaluate(T_sp exp, Environment_sp environment);
/*! See the CLHS for "apply" - all arguments are in args 
  (functionDesignator) can be a Symbol or an Function
*/
//extern T_sp apply(T_sp functionDesignator, Cons_sp args, Lisp_sp lisp);

//	extern T_mv applyFunctionToActivationFrame(Function_sp func, ActivationFrame_sp args );

	extern T_mv sp_trace( Cons_sp args, Environment_sp env );
	extern T_mv sp_untrace( Cons_sp args, Environment_sp env );


    };
};

#endif /* _evaluator_fwd_H_ */
