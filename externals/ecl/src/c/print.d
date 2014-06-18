/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    print.d -- Print.
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
#include <stdio.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

cl_object
_ecl_stream_or_default_output(cl_object stream)
{
	if (Null(stream))
		return ecl_symbol_value(@'*standard-output*');
	else if (stream == ECL_T)
		return ecl_symbol_value(@'*terminal-io*');
	return stream;
}

int
ecl_print_base(void)
{
	cl_object object = ecl_symbol_value(@'*print-base*');
	cl_fixnum base;
	unlikely_if (!ECL_FIXNUMP(object) || (base = ecl_fixnum(object)) < 2 || base > 36) {
		ECL_SETQ(ecl_process_env(), @'*print-base*', ecl_make_fixnum(10));
		FEerror("The value of *PRINT-BASE*~%  ~S~%"
                        "is not of the expected type (INTEGER 2 36)", 1, object);
	}
	return base;
}

cl_fixnum
ecl_print_level(void)
{
	cl_object object = ecl_symbol_value(@'*print-level*');
	cl_fixnum level;
	if (object == ECL_NIL) {
		level = MOST_POSITIVE_FIXNUM;
	} else if (ECL_FIXNUMP(object)) {
		level = ecl_fixnum(object);
		if (level < 0) {
		ERROR:	ECL_SETQ(ecl_process_env(), @'*print-level*', ECL_NIL);
			FEerror("The value of *PRINT-LEVEL*~%  ~S~%"
                                "is not of the expected type (OR NULL (INTEGER 0 *))",
                                1, object);
		}
	} else if (ecl_unlikely(!ECL_BIGNUMP(object))) {
		goto ERROR;
	} else {
		level = MOST_POSITIVE_FIXNUM;
	}
	return level;
}

cl_fixnum
ecl_print_length(void)
{
	cl_object object = ecl_symbol_value(@'*print-length*');
	cl_fixnum length;
	if (object == ECL_NIL) {
		length = MOST_POSITIVE_FIXNUM;
	} else if (ECL_FIXNUMP(object)) {
		length = ecl_fixnum(object);
		unlikely_if (length < 0) {
		ERROR:	ECL_SETQ(ecl_process_env(), @'*print-length*', ECL_NIL);
			FEerror("The value of *PRINT-LENGTH*~%  ~S~%"
                                "is not of the expected type (OR NULL (INTEGER 0 *))",
                                1, object);
		}
	} else if (ecl_unlikely(!ECL_BIGNUMP(object))) {
		goto ERROR;
	} else {
		length = MOST_POSITIVE_FIXNUM;
	}
	return length;
}

bool
ecl_print_radix(void)
{
	return ecl_symbol_value(@'*print-radix*') != ECL_NIL;
}

cl_object
ecl_print_case(void)
{
	cl_object output = ecl_symbol_value(@'*print-case*');
	unlikely_if (output != @':upcase' &&
                     output != @':downcase' &&
                     output != @':capitalize')
        {
		ECL_SETQ(ecl_process_env(), @'*print-case*', @':downcase');
		FEerror("The value of *PRINT-CASE*~%  ~S~%"
                        "is not of the expected type "
                        "(MEMBER :UPCASE :DOWNCASE :CAPITALIZE)", 1, output);
	}
	return output;
}

bool
ecl_print_gensym(void)
{
	return ecl_symbol_value(@'*print-gensym*') != ECL_NIL;
}

bool
ecl_print_array(void)
{
	return ecl_symbol_value(@'*print-array*') != ECL_NIL;
}

bool
ecl_print_readably(void)
{
	return ecl_symbol_value(@'*print-readably*') != ECL_NIL;
}

bool
ecl_print_escape(void)
{
	return ecl_symbol_value(@'*print-escape*') != ECL_NIL;
}

bool
ecl_print_circle(void)
{
	return ecl_symbol_value(@'*print-circle*') != ECL_NIL;
}

@(defun write (x
	       &key ((:stream strm) ECL_NIL)
		    (array ecl_symbol_value(@'*print-array*'))
		    (base ecl_symbol_value(@'*print-base*'))
		    ((:case cas) ecl_symbol_value(@'*print-case*'))
		    (circle ecl_symbol_value(@'*print-circle*'))
		    (escape ecl_symbol_value(@'*print-escape*'))
		    (gensym ecl_symbol_value(@'*print-gensym*'))
		    (length ecl_symbol_value(@'*print-length*'))
		    (level ecl_symbol_value(@'*print-level*'))
		    (lines ecl_symbol_value(@'*print-lines*'))
		    (miser_width ecl_symbol_value(@'*print-miser-width*'))
		    (pprint_dispatch ecl_symbol_value(@'*print-pprint-dispatch*'))
		    (pretty ecl_symbol_value(@'*print-pretty*'))
		    (radix ecl_symbol_value(@'*print-radix*'))
		    (readably ecl_symbol_value(@'*print-readably*'))
		    (right_margin ecl_symbol_value(@'*print-right-margin*')))
@{
	ecl_bds_bind(the_env, @'*print-array*', array);
	ecl_bds_bind(the_env, @'*print-base*', base);
	ecl_bds_bind(the_env, @'*print-case*', cas);
	ecl_bds_bind(the_env, @'*print-circle*', circle);
	ecl_bds_bind(the_env, @'*print-escape*', escape);
	ecl_bds_bind(the_env, @'*print-gensym*', gensym);
	ecl_bds_bind(the_env, @'*print-level*', level);
	ecl_bds_bind(the_env, @'*print-length*', length);
	ecl_bds_bind(the_env, @'*print-lines*', lines);
	ecl_bds_bind(the_env, @'*print-miser-width*', miser_width);
	ecl_bds_bind(the_env, @'*print-pprint-dispatch*', pprint_dispatch);
	ecl_bds_bind(the_env, @'*print-pretty*', pretty);
	ecl_bds_bind(the_env, @'*print-radix*', radix);
	ecl_bds_bind(the_env, @'*print-readably*', readably);
	ecl_bds_bind(the_env, @'*print-right-margin*', right_margin);

	strm = _ecl_stream_or_default_output(strm);
	si_write_object(x, strm);
	ecl_force_output(strm);

	ecl_bds_unwind_n(the_env, 15);
	@(return x)
@)

@(defun prin1 (obj &optional strm)
@
	ecl_prin1(obj, strm);
	@(return obj)
@)

@(defun print (obj &optional strm)
@
	ecl_print(obj, strm);
	@(return obj)
@)

@(defun pprint (obj &optional strm)
@
	strm = _ecl_stream_or_default_output(strm);
	ecl_bds_bind(the_env, @'*print-escape*', ECL_T);
	ecl_bds_bind(the_env, @'*print-pretty*', ECL_T);
	ecl_write_char('\n', strm);
	si_write_object(obj, strm);
	ecl_force_output(strm);
	ecl_bds_unwind_n(the_env, 2);
	@(return)
@)

@(defun princ (obj &optional strm)
@
	ecl_princ(obj, strm);
	@(return obj)
@)

@(defun write-char (c &optional strm)
@
	/* INV: ecl_char_code() checks the type of `c' */
 	strm = _ecl_stream_or_default_output(strm);
	c = ECL_CODE_CHAR(ecl_write_char(ecl_char_code(c), strm));
	@(return c)
@)

@(defun write-string (strng &o strm &k (start ecl_make_fixnum(0)) end)
@
        unlikely_if (!ECL_STRINGP(strng))
                FEwrong_type_nth_arg(@[write-string], 1, strng, @[string]);
	strm = _ecl_stream_or_default_output(strm);
#ifdef ECL_CLOS_STREAMS
        if (!ECL_ANSI_STREAM_P(strm))
		_ecl_funcall5(@'gray::stream-write-string', strm, strng, start, end);
	else
#endif
		si_do_write_sequence(strng, strm, start, end);
	@(return strng)
@)

@(defun write-line (strng &o strm &k (start ecl_make_fixnum(0)) end)
@
        unlikely_if (!ECL_STRINGP(strng))
                FEwrong_type_nth_arg(@[write-line], 1, strng, @[string]);
	strm = _ecl_stream_or_default_output(strm);
#ifdef ECL_CLOS_STREAMS
	if (!ECL_ANSI_STREAM_P(strm))
		_ecl_funcall5(@'gray::stream-write-string', strm, strng,
			      start, end);
	else
#endif
		si_do_write_sequence(strng, strm, start, end);
	ecl_terpri(strm);
	@(return strng)
@)

@(defun terpri (&optional strm)
@
	ecl_terpri(strm);
	@(return ECL_NIL)
@)

@(defun fresh-line (&optional strm)
@
 	strm = _ecl_stream_or_default_output(strm);
#ifdef ECL_CLOS_STREAMS
	if (!ECL_ANSI_STREAM_P(strm)) {
		return _ecl_funcall2(@'gray::stream-fresh-line', strm);
	}
#endif
	if (ecl_file_column(strm) == 0)
		@(return ECL_NIL)
	ecl_write_char('\n', strm);
	ecl_force_output(strm);
	@(return ECL_T)
@)

@(defun finish-output (&o strm)
@
 	strm = _ecl_stream_or_default_output(strm);
#ifdef ECL_CLOS_STREAMS
        if (!ECL_ANSI_STREAM_P(strm)) {
		return _ecl_funcall2(@'gray::stream-finish-output', strm);
	}
#endif
	ecl_force_output(strm);
	@(return ECL_NIL)
@)

@(defun force-output (&o strm)
@
 	strm = _ecl_stream_or_default_output(strm);
	ecl_force_output(strm);
	@(return ECL_NIL)
@)

@(defun clear-output (&o strm)
@
 	strm = _ecl_stream_or_default_output(strm);
	ecl_clear_output(strm);
	@(return ECL_NIL)
@)

cl_object
cl_write_byte(cl_object integer, cl_object binary_output_stream)
{
	ecl_write_byte(integer, binary_output_stream);
	@(return integer)
}

@(defun write-sequence (sequence stream &key (start ecl_make_fixnum(0)) end)
@
#ifdef ECL_CLOS_STREAMS
	if (!ECL_ANSI_STREAM_P(stream)) {
		return _ecl_funcall5(@'gray::stream-write-sequence',
				     stream, sequence, start, end);
	} else
#endif
		return si_do_write_sequence(sequence, stream, start, end);
@)

cl_object
ecl_princ(cl_object obj, cl_object strm)
{
	const cl_env_ptr the_env = ecl_process_env();
	strm = _ecl_stream_or_default_output(strm);
	ecl_bds_bind(the_env, @'*print-escape*', ECL_NIL);
	ecl_bds_bind(the_env, @'*print-readably*', ECL_NIL);
	si_write_object(obj, strm);
	ecl_bds_unwind_n(the_env, 2);
	return obj;
}

cl_object
ecl_prin1(cl_object obj, cl_object strm)
{
	const cl_env_ptr the_env = ecl_process_env();
	strm = _ecl_stream_or_default_output(strm);
	ecl_bds_bind(the_env, @'*print-escape*', ECL_T);
	si_write_object(obj, strm);
	ecl_force_output(strm);
	ecl_bds_unwind1(the_env);
	return obj;
}

cl_object
ecl_print(cl_object obj, cl_object strm)
{
	strm = _ecl_stream_or_default_output(strm);
	ecl_terpri(strm);
	ecl_prin1(obj, strm);
	ecl_princ_char(' ', strm);
	return obj;
}

cl_object
ecl_terpri(cl_object strm)
{
	strm = _ecl_stream_or_default_output(strm);
#ifdef ECL_CLOS_STREAMS
	if (!ECL_ANSI_STREAM_P(strm)) {
		return _ecl_funcall2(@'gray::stream-terpri', strm);
	}
#endif
	ecl_write_char('\n', strm);
	ecl_force_output(strm);
	return(ECL_NIL);
}

void
ecl_write_string(cl_object strng, cl_object strm)
{
	cl_index i;

	strm = _ecl_stream_or_default_output(strm);
	switch(ecl_t_of(strng)) {
#ifdef ECL_UNICODE
	case t_string:
		for (i = 0;  i < strng->string.fillp;  i++)
			ecl_write_char(strng->string.self[i], strm);
		break;
#endif
	case t_base_string:
		for (i = 0;  i < strng->base_string.fillp;  i++)
			ecl_write_char(strng->base_string.self[i], strm);
		break;
	default:
                FEwrong_type_nth_arg(@[write-string], 1, strng, @[string]);
	}
		
	ecl_force_output(strm);
}

/*
	THE ULTRA-SPECIAL-DINNER-SERVICE OPTIMIZATION
*/
void
ecl_princ_str(const char *s, cl_object strm)
{
	strm = _ecl_stream_or_default_output(strm);
	writestr_stream(s, strm);
}

int
ecl_princ_char(int c, cl_object strm)
{
	strm = _ecl_stream_or_default_output(strm);
	ecl_write_char(c, strm);
	if (c == '\n') {
		ecl_force_output(strm);
	}
        return c;
}
