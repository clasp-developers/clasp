/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    error.c -- Error handling.
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

#include <ecl/ecl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#if defined(ECL_MS_WINDOWS_HOST) || defined(cygwin)
# include <windows.h>
#endif
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

static cl_object
cl_symbol_or_object(cl_object x)
{
        if (ECL_FIXNUMP(x))
                return (cl_object)(cl_symbols + ecl_fixnum(x));
        return x;
}

void
_ecl_unexpected_return()
{
        ecl_internal_error(
"*** \n"
"*** A call to ERROR returned without handling the error.\n"
"*** This should have never happened and is usually a signal\n"
"*** that the debugger or the universal error handler were\n"
"*** improperly coded or altered. Please contact the maintainers\n"
"***\n");
}

void
ecl_internal_error(const char *s)
{
        int saved_errno = errno;
	fprintf(stderr, "\nInternal or unrecoverable error in:\n%s\n", s);
        if (saved_errno) {
                fprintf(stderr, "  [%d: %s]\n", saved_errno,
                        strerror(saved_errno));
        }
	fflush(stderr);
        si_dump_c_backtrace(ecl_make_fixnum(32));
#ifdef SIGIOT
	signal(SIGIOT, SIG_DFL); /* avoid getting into a loop with abort */
#endif
	abort();
}


void
ecl_unrecoverable_error(cl_env_ptr the_env, const char *message)
{
	/*
	 * Right now we have no means of specifying a jump point
	 * for really bad events. We just jump to the outermost
	 * frame, which is equivalent to quitting, and wait for
	 * someone to intercept this jump.
	 */
        ecl_frame_ptr destination;
        cl_object tag;

        /*
         * We output the error message with very low level routines
         * because we can not risk another stack overflow.
         */
        writestr_stream(message, cl_core.error_output);

        tag = ECL_SYM_VAL(the_env, @'si::*quit-tag*');
        the_env->nvalues = 0;
        if (tag) {
                destination = frs_sch(tag);
                if (destination) {
                        ecl_unwind(the_env, destination);
                }
        }
	if (the_env->frs_org <= the_env->frs_top) {
		destination = ecl_process_env()->frs_org;
		ecl_unwind(the_env, destination);
	} else {
		ecl_internal_error("\n;;;\n;;; No frame to jump to\n;;; Aborting ECL\n;;;");
	}
}

/*****************************************************************************/
/*		Support for Lisp Error Handler				     */
/*****************************************************************************/

void
FEerror(const char *s, int narg, ...)
{
	ecl_va_list args;
	ecl_va_start(args, narg, narg, 0);
	ecl_enable_interrupts();
	funcall(4, @'si::universal-error-handler',
		ECL_NIL,                    /*  not correctable  */
		make_constant_base_string(s),	 /*  condition text  */
		cl_grab_rest_args(args));
        _ecl_unexpected_return();
}

cl_object
CEerror(cl_object c, const char *err, int narg, ...)
{
	ecl_va_list args;
	ecl_va_start(args, narg, narg, 0);
	ecl_enable_interrupts();
	return funcall(4, @'si::universal-error-handler',
		       c,			/*  correctable  */
		       make_constant_base_string(err),	/*  continue-format-string  */
		       cl_grab_rest_args(args));
}

/***********************
 * Conditions signaler *
 ***********************/

void
FEprogram_error(const char *s, int narg, ...)
{
	cl_object real_args, text;
	ecl_va_list args;
	ecl_va_start(args, narg, narg, 0);
	text = make_constant_base_string(s);
	real_args = cl_grab_rest_args(args);
	if (cl_boundp(@'si::*current-form*') != ECL_NIL) {
	    /* When FEprogram_error is invoked from the compiler, we can
	     * provide information about the offending form.
	     */
	    cl_object stmt = ecl_symbol_value(@'si::*current-form*');
	    if (stmt != ECL_NIL) {
		real_args = @list(3, stmt, text, real_args);
		text = make_constant_base_string("In form~%~S~%~?");
	    }
	}
	si_signal_simple_error(4, 
			       @'program-error', /* condition name */
			       ECL_NIL, /* not correctable */
			       text,
			       real_args);
}

void
FEprogram_error_noreturn(const char *s, int narg, ...)
{
	cl_object real_args, text;
	ecl_va_list args;
	ecl_va_start(args, narg, narg, 0);
	text = make_constant_base_string(s);
	real_args = cl_grab_rest_args(args);
	if (cl_boundp(@'si::*current-form*') != ECL_NIL) {
	    /* When FEprogram_error is invoked from the compiler, we can
	     * provide information about the offending form.
	     */
	    cl_object stmt = ecl_symbol_value(@'si::*current-form*');
	    if (stmt != ECL_NIL) {
		real_args = @list(3, stmt, text, real_args);
		text = make_constant_base_string("In form~%~S~%~?");
	    }
	}
	si_signal_simple_error(4, 
			       @'program-error', /* condition name */
			       ECL_NIL, /* not correctable */
			       text,
			       real_args);
}

void
FEcontrol_error(const char *s, int narg, ...)
{
	ecl_va_list args;
	ecl_va_start(args, narg, narg, 0);
	si_signal_simple_error(4,
			       @'control-error', /* condition name */
			       ECL_NIL, /* not correctable */
			       make_constant_base_string(s), /* format control */
			       cl_grab_rest_args(args)); /* format args */
}

void
FEreader_error(const char *s, cl_object stream, int narg, ...)
{
        cl_object message = make_constant_base_string(s);
        cl_object args_list;
	ecl_va_list args;
	ecl_va_start(args, narg, narg, 0);
        args_list = cl_grab_rest_args(args);
        if (Null(stream)) {
                /* Parser error */
                si_signal_simple_error(4,
                                       @'parse-error', /* condition name */
                                       ECL_NIL, /* not correctable */
                                       message, /* format control */
                                       args_list);
        } else {
                /* Actual reader error */
                cl_object prefix = make_constant_base_string("Reader error in file ~S, "
                                                             "position ~D:~%");
                cl_object position = cl_file_position(1, stream);
                message = si_base_string_concatenate(2, prefix, message);
                args_list = cl_listX(3, stream, position, args_list);
                si_signal_simple_error(6,
                                       @'reader-error', /* condition name */
                                       ECL_NIL, /* not correctable */
                                       message, /* format control */
                                       args_list, /* format args */
                                       @':stream', stream);
        }
}


void
FEcannot_open(cl_object fn)
{
	cl_error(3, @'file-error', @':pathname', fn);
}

void
FEend_of_file(cl_object strm)
{
	cl_error(3, @'end-of-file', @':stream', strm);
}

void
FEclosed_stream(cl_object strm)
{
	cl_error(3, @'stream-error', @':stream', strm);
}

cl_object
si_signal_type_error(cl_object value, cl_object type)
{
	return cl_error(5, @'type-error', @':expected-type', type,
                        @':datum', value);
}

void
FEwrong_type_argument(cl_object type, cl_object value)
{
        si_signal_type_error(value, cl_symbol_or_object(type));
}

void
FEwrong_type_only_arg(cl_object function, cl_object value, cl_object type)
{
        const char *message =
                "In ~:[an anonymous function~;~:*function ~A~], "
                "the value of the only argument is~&  ~S~&which is "
                "not of the expected type ~A";
        cl_env_ptr env = ecl_process_env();
        struct ecl_ihs_frame tmp_ihs;
        function = cl_symbol_or_object(function);
        type = cl_symbol_or_object(type);
        if (!Null(function) && env->ihs_top && env->ihs_top->function != function) {
                ecl_ihs_push(env,&tmp_ihs,function,ECL_NIL);
        }        
        si_signal_simple_error(8,
                               @'type-error', /* condition name */
                               ECL_NIL, /* not correctable */
                               make_constant_base_string(message), /* format control */
                               cl_list(3, function, value, type),
                               @':expected-type', type,
                               @':datum', value);
}

void
FEwrong_type_nth_arg(cl_object function, cl_narg narg, cl_object value, cl_object type)
{
        const char *message =
                "In ~:[an anonymous function~;~:*function ~A~], "
                "the value of the ~:R argument is~&  ~S~&which is "
                "not of the expected type ~A";
        cl_env_ptr env = ecl_process_env();
        struct ecl_ihs_frame tmp_ihs;
        function = cl_symbol_or_object(function);
        type = cl_symbol_or_object(type);
        if (!Null(function) && env->ihs_top && env->ihs_top->function != function) {
                ecl_ihs_push(env,&tmp_ihs,function,ECL_NIL);
        }        
        si_signal_simple_error(8,
                               @'type-error', /* condition name */
                               ECL_NIL, /* not correctable */
                               make_constant_base_string(message), /* format control */
                               cl_list(4, function, ecl_make_fixnum(narg),
                                       value, type),
                               @':expected-type', type,
                               @':datum', value);
}

void
FEwrong_type_key_arg(cl_object function, cl_object key, cl_object value, cl_object type)
{
        const char *message =
                "In ~:[an anonymous function~;~:*function ~A~], "
                "the value of the argument ~S is~&  ~S~&which is "
                "not of the expected type ~A";
        cl_env_ptr env = ecl_process_env();
        struct ecl_ihs_frame tmp_ihs;
        function = cl_symbol_or_object(function);
        type = cl_symbol_or_object(type);
        key = cl_symbol_or_object(key);
        if (!Null(function) && env->ihs_top && env->ihs_top->function != function) {
                ecl_ihs_push(env,&tmp_ihs,function,ECL_NIL);
        }        
        si_signal_simple_error(8,
                               @'type-error', /* condition name */
                               ECL_NIL, /* not correctable */
                               make_constant_base_string(message), /* format control */
                               cl_list(4, function, key, value, type),
                               @':expected-type', type,
                               @':datum', value);
}

void
FEwrong_index(cl_object function, cl_object a, int which, cl_object ndx,
              cl_index nonincl_limit)
{
        const char *message1 =
                "In ~:[an anonymous function~;~:*function ~A~], "
                "the ~*index into the object~% ~A.~%"
                "takes a value ~D out of the range ~A.";
        const char *message2 =
                "In ~:[an anonymous function~;~:*function ~A~], "
                "the ~:R index into the object~% ~A~%"
                "takes a value ~D out of the range ~A.";
        cl_object limit = ecl_make_integer(nonincl_limit-1);
	cl_object type = ecl_make_integer_type(ecl_make_fixnum(0), limit);
        cl_object message = make_constant_base_string((which<0) ? message1 : message2);
        cl_env_ptr env = ecl_process_env();
        struct ecl_ihs_frame tmp_ihs;
        function = cl_symbol_or_object(function);
        if (!Null(function) && env->ihs_top && env->ihs_top->function != function) {
                ecl_ihs_push(env,&tmp_ihs,function,ECL_NIL);
        }        
        cl_error(9,
                 @'simple-type-error', /* condition name */
                 @':format-control', message,
                 @':format-arguments',
                 cl_list(5, function, ecl_make_fixnum(which+1), a, ndx, type),
                 @':expected-type', type,
                 @':datum', ndx);
}

void
FEunbound_variable(cl_object sym)
{
	cl_error(3, @'unbound-variable', @':name', sym);
}

void
FEundefined_function(cl_object fname)
{
	cl_error(3, @'undefined-function', @':name', fname);
}

void
FEprint_not_readable(cl_object x)
{
	cl_error(3, @'print-not-readable', @':object', x);
}

/*************
 * Shortcuts *
 *************/

void
FEwrong_num_arguments(cl_object fun)
{
        fun = cl_symbol_or_object(fun);
	FEprogram_error("Wrong number of arguments passed to function ~S.",
			1, fun);
}

void
FEwrong_num_arguments_anonym(void)
{
	FEprogram_error("Wrong number of arguments passed to an anonymous function", 0);
}

void
FEinvalid_macro_call(cl_object name)
{
	FEerror("Invalid macro call to ~S.", 1, name);
}

void
FEinvalid_variable(const char *s, cl_object obj)
{
	FEerror(s, 1, obj);
}

void
FEassignment_to_constant(cl_object v)
{
	FEprogram_error("SETQ: Tried to assign a value to the constant ~S.", 1, v);
}

void
FEinvalid_function(cl_object obj)
{
	FEwrong_type_argument(@'function', obj);
}

void
FEinvalid_function_name(cl_object fname)
{
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("Not a valid function name ~D"),
		 @':format-arguments', cl_list(1, fname),
		 @':expected-type', cl_list(2, @'satisfies', @'si::valid-function-name-p'),
		 @':datum', fname);
}

/*      bootstrap version                */
static int recursive_error = 0;

static cl_object
universal_error_handler(cl_object continue_string, cl_object datum,
                        cl_object args)
{
        const cl_env_ptr the_env = ecl_process_env();
        cl_object stream;
        if (recursive_error)
                goto ABORT;
        recursive_error = 1;
        stream = cl_core.error_output;
        if (!Null(stream)) {
                ecl_bds_bind(the_env, @'*print-readably*', ECL_NIL);
                ecl_bds_bind(the_env, @'*print-level*', ecl_make_fixnum(3));
                ecl_bds_bind(the_env, @'*print-length*', ecl_make_fixnum(3));
                ecl_bds_bind(the_env, @'*print-circle*', ECL_NIL);
                ecl_bds_bind(the_env, @'*print-base*', ecl_make_fixnum(10));
                writestr_stream("\n;;; Unhandled lisp initialization error",
                                stream);
                writestr_stream("\n;;; Message:\n", stream);
                si_write_ugly_object(datum, stream);
                writestr_stream("\n;;; Arguments:\n", stream);
                si_write_ugly_object(datum, args);
                ecl_bds_unwind_n(the_env, 5);
        }
 ABORT:
	ecl_internal_error("\nLisp initialization error.\n");
}

void
FEdivision_by_zero(cl_object x, cl_object y)
{
	cl_error(5, @'division-by-zero', @':operation', @'/',
		 @':operands', cl_list(2, x, y));
}

cl_object
_ecl_strerror(int code)
{
	const char *error = strerror(code);
	return make_base_string_copy(error);
}

/*************************************
 * Errors generated by the C library *
 *************************************/
/*
 * Interprets an error code from the C library according to the POSIX
 * standard, and produces a suitable error message by combining the user
 * supplied format with an explanation of the cause of the error.
 */
void
FElibc_error(const char *msg, int narg, ...)
{
	ecl_va_list args;
	cl_object rest, error = _ecl_strerror(errno);

	ecl_va_start(args, narg, narg, 0);
	rest = cl_grab_rest_args(args);

	FEerror("~?~%C library explanation: ~A.", 3,
		make_constant_base_string(msg), rest,
		error);
}

#if defined(ECL_MS_WINDOWS_HOST) || defined(cygwin)
ecl_def_ct_base_string(unknown_error,"[Unable to get error message]",28,static,const);

void
FEwin32_error(const char *msg, int narg, ...)
{
	ecl_va_list args;
	cl_object rest, win_msg_obj;
	char *win_msg;

	if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER,
	                  0, GetLastError(), 0, (void*)&win_msg, 0, NULL) == 0)
		win_msg_obj = unknown_error;
	else {
		win_msg_obj = make_base_string_copy(win_msg);
		LocalFree(win_msg);
	}

	ecl_va_start(args, narg, narg, 0);
	rest = cl_grab_rest_args(args);
	FEerror("~?~%Windows library explanation: ~A.", 3,
		make_constant_base_string(msg), rest,
		win_msg_obj);
}
#endif

/************************************
 * Higher level interface to errors *
 ************************************/

@(defun error (eformat &rest args)
@
	ecl_enable_interrupts();
	funcall(4, @'si::universal-error-handler', ECL_NIL, eformat,
                cl_grab_rest_args(args));
	_ecl_unexpected_return();
	@(return);
@)

@(defun cerror (cformat eformat &rest args)
@
	ecl_enable_interrupts();
	return funcall(4, @'si::universal-error-handler', cformat, eformat,
		       cl_grab_rest_args(args));
@)

void
init_error(void)
{
	ecl_def_c_function(@'si::universal-error-handler',
                           (cl_objectfn_fixed)universal_error_handler,
                           3);
}
