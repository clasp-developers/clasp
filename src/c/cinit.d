/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    init.c  -- Lisp Initialization.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <stdio.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

/*
 * HOOKS.
 *
 * The following functions are only used to bootstrap ECL. They divert
 * the calls to the interpreted code which is loaded by bare.lsp. Once
 * the whole of ECL is built, the file cinit.o will be replaced by the
 * actual initialization code, and the compiled function will be
 * called instead.
 */

extern cl_object
cl_upgraded_array_element_type(cl_narg narg, cl_object type, ...)
{
	return _ecl_funcall2(@'upgraded-array-element-type', type);
}

extern cl_object
si_safe_eval(cl_narg narg, cl_object form, cl_object env, ...)
{
        if (narg == 3) {
                cl_object err_value;
                va_list args; va_start(args, env);
                err_value = va_arg(args, cl_object);
                return _ecl_funcall4(@'ext::safe-eval', form, env, err_value);
        }
        return _ecl_funcall3(@'ext::safe-eval', form, env);
}

extern cl_object
cl_slot_value(cl_object instance, cl_object name)
{
	return _ecl_funcall3(@'slot-value', instance, name);
}

extern cl_object
clos_slot_value_set(cl_object value, cl_object instance, cl_object name)
{
	return _ecl_funcall4(@'clos::slot-value-set', value, instance, name);
}

extern cl_object
clos_std_compute_applicable_methods(cl_object gf, cl_object arglist)
{
	return _ecl_funcall3(@'clos::std-compute-applicable-methods', gf, arglist);
}

extern cl_object
si_bind_simple_restarts(cl_object tag, cl_object names)
{
	if (ECL_SYM_FUN(@'si::bind-simple-restarts') != Cnil)
		return _ecl_funcall3(@'si::bind-simple-restarts', tag, names);
	else
		return ECL_SYM_VAL(ecl_process_env(), @'si::*restart-clusters*');
}

extern cl_object
si_bind_simple_handlers(cl_object tag, cl_object names)
{
	if (ECL_SYM_FUN(@'si::bind-simple-handlers') != Cnil)
		return _ecl_funcall3(@'si::bind-simple-handlers', tag, names);
	else
		return ECL_SYM_VAL(ecl_process_env(), @'si::*handler-clusters*');
}

extern cl_object
clos_std_compute_effective_method(cl_object gf, cl_object combination, cl_object methods_list)
{
	return _ecl_funcall4(@'clos::std-compute-effective-method', gf, combination, methods_list);
}

extern cl_object
clos_compute_effective_method_function(cl_object gf, cl_object combination, cl_object methods_list)
{
	return _ecl_funcall4(@'clos::compute-effective-method-function', gf, combination, methods_list);
}

extern cl_object
si_string_to_object(cl_narg narg, cl_object string, ...)
{
        if (narg == 2) {
                cl_object err_value;
                va_list args; va_start(args, string);
                err_value = va_arg(args, cl_object);
                return _ecl_funcall3(@'si::string-to-object', string, err_value);
        }
        return _ecl_funcall2(@'si::string-to-object', string);
}

extern cl_object
si_signal_simple_error(cl_narg narg, cl_object condition, cl_object continuable, cl_object format, cl_object format_args, ...)
{
	ecl_va_list args;
	cl_object rest;
	ecl_va_start(args, format_args, narg, 4);
	rest = cl_grab_rest_args(args);
	return cl_apply(6, @'si::signal-simple-error', condition, continuable,
			format, format_args, rest);
}

extern cl_object
cl_set_difference(cl_narg narg, cl_object l1, cl_object l2, ...)
{
        @(return l1)
}

extern cl_object
cl_array_dimensions(cl_object array)
{
	return _ecl_funcall2(@'ARRAY-DIMENSIONS', array);
}

extern cl_object
si_find_relative_package(cl_narg narg, cl_object package, ...)
{
	@(return ECL_NIL);
}

extern cl_object
si_wrong_type_argument(cl_narg narg, cl_object object, cl_object type, ...)
{
	return _ecl_funcall3(@'si::wrong-type-argument', object, type);
}

extern cl_object
si_make_encoding(cl_object mapping)
{
	return _ecl_funcall2(@'ext::make-encoding', mapping);
}

static cl_object si_simple_toplevel ()
{
        cl_env_ptr env = ecl_process_env();
	cl_object output = cl_core.standard_output;
	cl_object sentence;
	int i;

	/* Simple minded top level loop */
        ECL_CATCH_ALL_BEGIN(env) {
                writestr_stream(";*** Lisp core booted ****\n"
                                "ECL (Embeddable Common Lisp)\n",
                                output);
                ecl_force_output(output);
                for (i = 1; i<ecl_fixnum(si_argc()); i++) {
                        cl_object arg = si_argv(ecl_make_fixnum(i));
                        cl_load(1, arg);
                }
                while (1) {
                        writestr_stream("\n> ", output);
                        sentence = @read(3, ECL_NIL, ECL_NIL, OBJNULL);
                        if (sentence == OBJNULL)
                                @(return);
			sentence = si_eval_with_env(1, sentence);
                        ecl_prin1(sentence, output);
                }
        } ECL_CATCH_ALL_END;
}

int
main(int argc, char **args)
{
	cl_object top_level, features;

	/* This should be always the first call */
	cl_boot(argc, args);

	/* We are computing unnormalized numbers at some point */
	si_trap_fpe(ECL_T, ECL_NIL);

#ifdef ECL_CMU_FORMAT
	ECL_SET(@'*load-verbose*', ECL_NIL);
#endif
	ECL_SET(@'*package*', cl_core.system_package);

	features = ecl_symbol_value(@'*features*');
	features = CONS(ecl_make_keyword("ECL-MIN"), features);
#ifdef HAVE_UNAME
	features = CONS(ecl_make_keyword("UNAME"), features);
#endif
	ECL_SET(@'*features*', features);
	top_level = _ecl_intern("TOP-LEVEL", cl_core.system_package);
	ecl_def_c_function(top_level, si_simple_toplevel, 0);
	_ecl_funcall1(top_level);
	return(0);
}

#ifdef __cplusplus
extern "C" void init_lib_LSP(cl_object);
#endif

void init_lib_LSP(cl_object o) {}
