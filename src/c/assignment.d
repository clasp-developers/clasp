/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    assignment.c  -- Assignment.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

static void FEconstant_assignment(cl_object var) ecl_attr_noreturn;

static void
FEconstant_assignment(cl_object var)
{
	FEinvalid_variable("Cannot assign to the constant ~S.", var);
}

cl_object
cl_set(cl_object var, cl_object value)
{
	const cl_env_ptr env = ecl_process_env();
	unlikely_if (Null(var)) {
		FEconstant_assignment(var);
	}
	unlikely_if (ecl_t_of(var) != t_symbol) {
		FEwrong_type_nth_arg(@[setq], 1, var, @[symbol]);
	}
	unlikely_if (var->symbol.stype & ecl_stp_constant)
		FEconstant_assignment(var);
	ecl_return1(env, ECL_SETQ(env, var, value));
}

cl_object
ecl_setq(cl_env_ptr env, cl_object var, cl_object value)
{
	unlikely_if (Null(var)) {
		FEconstant_assignment(var);
	}
	unlikely_if (ecl_t_of(var) != t_symbol) {
		FEwrong_type_nth_arg(@[setq], 1, var, @[symbol]);
	}
	return ECL_SETQ(env, var, value);
}

static cl_object
unbound_setf_function_error(cl_narg narg, ...)
{
	const cl_env_ptr the_env = ecl_process_env();
        cl_object name = the_env->function->cclosure.env;
	FEundefined_function(cl_list(2, @'setf', name));
}

static cl_object
make_setf_function_error(cl_object name)
{
	return ecl_make_cclosure_va((cl_objectfn)unbound_setf_function_error,
				    name, ECL_NIL);
}

cl_object
ecl_setf_definition(cl_object sym, cl_object createp)
{
	cl_env_ptr the_env = ecl_process_env();
	cl_object pair;
        ECL_WITH_GLOBAL_ENV_RDLOCK_BEGIN(the_env) {
                pair = ecl_gethash_safe(sym, cl_core.setf_definitions, ECL_NIL);
		if (Null(pair) && !Null(createp)) {
			createp = make_setf_function_error(sym);
			pair = ecl_cons(createp, ECL_NIL);
			ecl_sethash(sym, cl_core.setf_definitions, pair);
		}
        } ECL_WITH_GLOBAL_ENV_RDLOCK_END;
	return pair;
}

cl_object
si_setf_definition(cl_object sym, cl_object value)
{
	@(return ecl_setf_definition(sym, value))
}

static void
ecl_rem_setf_definition(cl_object sym)
{
	cl_env_ptr the_env = ecl_process_env();
        ECL_WITH_GLOBAL_ENV_WRLOCK_BEGIN(the_env) {
                cl_object pair = ecl_gethash_safe(sym, cl_core.setf_definitions, ECL_NIL);
		if (!Null(pair)) {
			ECL_RPLACA(pair, make_setf_function_error(sym));
			ECL_RPLACD(pair, ECL_NIL);
			/*
			  FIXME: This leaks resources
			  We actually cannot remove it, because some compiled
			  code might be using it!
			ecl_remhash(sym, cl_core.setf_definitions);
			*/
		}
        } ECL_WITH_GLOBAL_ENV_WRLOCK_END;
}

@(defun si::fset (fname def &optional macro pprint)
	cl_object sym = si_function_block_name(fname);
	cl_object pack;
	bool mflag;
	int type;
@
	if (Null(cl_functionp(def)))
		FEinvalid_function(def);
	pack = ecl_symbol_package(sym);
	if (pack != ECL_NIL && pack->pack.locked) {
		CEpackage_error("Attempt to redefine function ~S in locked package.",
				"Ignore lock and proceed", pack, 1, fname);
	}
	mflag = !Null(macro);
	type = ecl_symbol_type(sym);
	if ((type & ecl_stp_special_form) && !mflag) {
		FEerror("Given that ~S is a special form, ~S cannot be defined as a function.",
			2, sym, fname);
	}
	if (ECL_SYMBOLP(fname)) {
		if (mflag) {
			type |= ecl_stp_macro;
		} else {
			type &= ~ecl_stp_macro;
		}
		ecl_symbol_type_set(sym, type);
		ECL_SYM_FUN(sym) = def;
		ecl_clear_compiler_properties(sym);
#ifndef ECL_CMU_FORMAT
		if (pprint == ECL_NIL)
			si_rem_sysprop(sym, @'si::pretty-print-format');
		else
			si_put_sysprop(sym, @'si::pretty-print-format', pprint);
#endif
	} else if (mflag) {
		FEerror("~S is not a valid name for a macro.", 1, fname);
	} else {
		cl_object pair = ecl_setf_definition(sym, def);
		ECL_RPLACA(pair, def);
		ECL_RPLACD(pair, sym);
	}
	@(return def)
@)

cl_object
cl_makunbound(cl_object sym)
{
	if (ecl_symbol_type(sym) & ecl_stp_constant)
		FEinvalid_variable("Cannot unbind the constant ~S.", sym);
	/* FIXME! The semantics of MAKUNBOUND is not very clear with local
	   bindings ... */
	ECL_SET(sym, OBJNULL);
	@(return sym)
}

cl_object
cl_fmakunbound(cl_object fname)
{
	cl_object sym = si_function_block_name(fname);
	cl_object pack = ecl_symbol_package(sym);
	if (pack != ECL_NIL && pack->pack.locked) {
		CEpackage_error("Attempt to redefine function ~S in locked package.",
				"Ignore lock and proceed", pack, 1, fname);
	}
	if (ECL_SYMBOLP(fname)) {
		ecl_clear_compiler_properties(sym);
		ECL_SYM_FUN(sym) = ECL_NIL;
		ecl_symbol_type_set(sym, ecl_symbol_type(sym) & ~ecl_stp_macro);
	} else {
		ecl_rem_setf_definition(sym);
		si_rem_sysprop(sym, @'si::setf-method');
	}
	@(return fname)
}

void
ecl_clear_compiler_properties(cl_object sym)
{
	if (ecl_option_values[ECL_OPT_BOOTED]) {
		funcall(2, @'si::clear-compiler-properties', sym);
	}
}

cl_object
si_get_sysprop(cl_object sym, cl_object prop)
{
	cl_env_ptr the_env = ecl_process_env();
        ECL_WITH_GLOBAL_ENV_RDLOCK_BEGIN(the_env) {
                cl_object plist = ecl_gethash_safe(sym, cl_core.system_properties, ECL_NIL);
                prop = ecl_getf(plist, prop, OBJNULL);
        } ECL_WITH_GLOBAL_ENV_RDLOCK_END;
        if (prop == OBJNULL) {
                @(return ECL_NIL ECL_NIL);
        } else {
                @(return prop ECL_T);
        }
}

cl_object
si_put_sysprop(cl_object sym, cl_object prop, cl_object value)
{
	cl_env_ptr the_env = ecl_process_env();
        ECL_WITH_GLOBAL_ENV_WRLOCK_BEGIN(the_env) {
                cl_object plist = ecl_gethash_safe(sym, cl_core.system_properties, ECL_NIL);
                ecl_sethash(sym, cl_core.system_properties, si_put_f(plist, value, prop));
        } ECL_WITH_GLOBAL_ENV_WRLOCK_END;
	@(return value);
}

cl_object
si_rem_sysprop(cl_object sym, cl_object prop)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object plist, found;
	plist = ecl_gethash_safe(sym, cl_core.system_properties, ECL_NIL);
	plist = si_rem_f(plist, prop);
	found = ecl_nth_value(the_env, 1);
	ecl_sethash(sym, cl_core.system_properties, plist);
	ecl_return1(the_env, found);
}
