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

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#ifndef _MSC_VER
# include <unistd.h>
#endif
#include <ecl/ecl.h>
#include <ecl/internal.h>

static void
write_readable_pathname(cl_object path, cl_object stream)
{
        cl_object l =
                cl_list(15, @'make-pathname',
                        @':host', path->pathname.host,
                        @':device', path->pathname.device,
                        @':directory',
                        _ecl_funcall2(@'ext::maybe-quote', path->pathname.directory),
                        @':name', path->pathname.name,
                        @':type', path->pathname.type,
                        @':version', path->pathname.version,
                        @':defaults', ECL_NIL);
        writestr_stream("#.", stream);
        si_write_object(l, stream);
}

static void
write_pathname(cl_object path, cl_object stream)
{
        cl_object namestring = ecl_namestring(path, 0);
        bool readably = ecl_print_readably();
        if (namestring == ECL_NIL) {
                if (readably) {
                        write_readable_pathname(path, stream);
                        return;
                }
                namestring = ecl_namestring(path, 1);
                if (namestring == ECL_NIL) {
                        writestr_stream("#<Unprintable pathname>", stream);
                        return;
                }
        }
        if (readably || ecl_print_escape())
                writestr_stream("#P", stream);
        si_write_ugly_object(namestring, stream);
}

static void
write_integer(cl_object number, cl_object stream)
{
        cl_object s = si_get_buffer_string();
        int print_base = ecl_print_base();
        si_integer_to_string(s, number,
                             ecl_make_fixnum(print_base),
                             ecl_symbol_value(@'*print-radix*'),
                             ECL_T /* decimal syntax */);
        si_do_write_sequence(s, stream, ecl_make_fixnum(0), ECL_NIL);
        si_put_buffer_string(s);
}

void
_ecl_write_fixnum(cl_fixnum i, cl_object stream)
{
        cl_object s = si_get_buffer_string();
        si_integer_to_string(s, ecl_make_fixnum(i), ecl_make_fixnum(10), ECL_NIL, ECL_NIL);
        si_do_write_sequence(s, stream, ecl_make_fixnum(0), ECL_NIL);
        si_put_buffer_string(s);
}

static void
write_ratio(cl_object r, cl_object stream)
{
        cl_object s = si_get_buffer_string();
        int print_base = ecl_print_base();
        si_integer_to_string(s, r->ratio.num, ecl_make_fixnum(print_base),
                             ecl_symbol_value(@'*print-radix*'),
                             ECL_NIL /* decimal syntax */);
        ecl_string_push_extend(s, '/');
        si_integer_to_string(s, r->ratio.den,
                             ecl_make_fixnum(print_base),
                             ECL_NIL, ECL_NIL);
        si_do_write_sequence(s, stream, ecl_make_fixnum(0), ECL_NIL);
        si_put_buffer_string(s);
}

static void
write_complex(cl_object x, cl_object stream)
{
        writestr_stream("#C(", stream);
        si_write_ugly_object(x->complex.real, stream);
        ecl_write_char(' ', stream);
        si_write_ugly_object(x->complex.imag, stream);
        ecl_write_char(')', stream);
}

static void
write_float(cl_object f, cl_object stream)
{
        cl_object s = si_get_buffer_string();
        s = si_float_to_string_free(s, f, ecl_make_fixnum(-3), ecl_make_fixnum(8));
        si_do_write_sequence(s, stream, ecl_make_fixnum(0), ECL_NIL);
        si_put_buffer_string(s);
}

static void
write_character(cl_object x, cl_object stream)
{
        int i = ECL_CHAR_CODE(x);
	if (!ecl_print_escape() && !ecl_print_readably()) {
		ecl_write_char(i, stream);
	} else {
		writestr_stream("#\\", stream);
		if (i < 32 || i >= 127) {
			cl_object name = cl_char_name(ECL_CODE_CHAR(i));
			writestr_stream((char*)name->base_string.self, stream);
		} else {
			ecl_write_char(i, stream);
		}
	}
}

static void
write_package(cl_object x, cl_object stream)
{
        if (ecl_print_readably()) FEprint_not_readable(x);
        writestr_stream("#<", stream);
        si_write_ugly_object(x->pack.name, stream);
        writestr_stream(" package>", stream);
}

static void
write_hashtable(cl_object x, cl_object stream)
{
	if (ecl_print_readably() && !Null(ecl_symbol_value(@'*read-eval*'))) {
		cl_object make =
			cl_list(9, @'make-hash-table',
				@':size', cl_hash_table_size(x),
				@':rehash-size', cl_hash_table_rehash_size(x),
				@':rehash-threshold', cl_hash_table_rehash_threshold(x),
				@':test', cl_list(2, @'quote', cl_hash_table_test(x)));
		cl_object init =
			cl_list(3, @'ext::hash-table-fill', make,
				cl_list(2, @'quote', si_hash_table_content(x)));
		writestr_stream("#.", stream);
		si_write_ugly_object(init, stream);
	} else {
		_ecl_write_unreadable(x, "hash-table", ECL_NIL, stream);
	}
}

static void
write_random(cl_object x, cl_object stream)
{
        if (ecl_print_readably()) {
                writestr_stream("#$", stream);
                _ecl_write_vector(x->random.value, stream);
        } else {
                _ecl_write_unreadable(x->random.value, "random-state", ECL_NIL, stream);
        }
}

static void
write_stream(cl_object x, cl_object stream)
{
        const char *prefix;
        cl_object tag;
        union cl_lispunion str;
#ifdef ECL_UNICODE
        ecl_character buffer[10];
#else
        ecl_base_char buffer[10];
#endif
        switch ((enum ecl_smmode)x->stream.mode) {
        case ecl_smm_input_file:
                prefix = "closed input file";
                tag = IO_STREAM_FILENAME(x);
                break;
        case ecl_smm_input:
                prefix = "closed input stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case ecl_smm_output_file:
                prefix = "closed output file";
                tag = IO_STREAM_FILENAME(x);
                break;
        case ecl_smm_output:
                prefix = "closed output stream";
                tag = IO_STREAM_FILENAME(x);
                break;
#ifdef ECL_MS_WINDOWS_HOST
        case ecl_smm_input_wsock:
                prefix = "closed input win32 socket stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case ecl_smm_output_wsock:
                prefix = "closed output win32 socket stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case ecl_smm_io_wsock:
                prefix = "closed i/o win32 socket stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case ecl_smm_io_wcon:
                prefix = "closed i/o win32 console stream";
                tag = IO_STREAM_FILENAME(x);
                break;
#endif
        case ecl_smm_io_file:
                prefix = "closed io file";
                tag = IO_STREAM_FILENAME(x);
                break;
        case ecl_smm_io:
                prefix = "closed io stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case ecl_smm_probe:
                prefix = "closed probe stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case ecl_smm_synonym:
                prefix = "closed synonym stream to";
                tag = SYNONYM_STREAM_SYMBOL(x);
                break;
        case ecl_smm_broadcast:
                prefix = "closed broadcast stream";
                tag = ECL_NIL;
                break;
        case ecl_smm_concatenated:
                prefix = "closed concatenated stream";
                tag = ECL_NIL;
                break;
        case ecl_smm_two_way:
                prefix = "closed two-way stream";
                tag = ECL_NIL;
                break;
        case ecl_smm_echo:
                prefix = "closed echo stream";
                tag = ECL_NIL;
                break;
        case ecl_smm_string_input: {
                cl_object text = x->stream.object0;
                cl_index ndx, l = ecl_length(text);
                for (ndx = 0; (ndx < 8) && (ndx < l); ndx++) {
                        buffer[ndx] = ecl_char(text, ndx);
                }
                if (l > ndx) {
                        buffer[ndx-1] = '.';
                        buffer[ndx-2] = '.';
                        buffer[ndx-3] = '.';
                }
                buffer[ndx++] = 0;
                prefix = "closed string-input stream from";
                tag = &str;
#ifdef ECL_UNICODE
                tag->string.t = t_string;
                tag->string.self = buffer;
#else
                tag->base_string.t = t_base_string;
                tag->base_string.self = buffer;
#endif
                tag->base_string.dim = ndx;
                tag->base_string.fillp = ndx-1;
                break;
        }
        case ecl_smm_string_output:
                prefix = "closed string-output stream";
                tag = ECL_NIL;
                break;
        case ecl_smm_sequence_input:
                prefix = "closed sequence-input stream";
                tag = ECL_NIL;
                break;
        case ecl_smm_sequence_output:
                prefix = "closed sequence-output stream";
                tag = ECL_NIL;
                break;
        default:
                ecl_internal_error("illegal stream mode");
        }
        if (!x->stream.closed)
                prefix = prefix + 7;
        _ecl_write_unreadable(x, prefix, tag, stream);
}

#ifdef CLOS
static void
write_instance(cl_object x, cl_object stream)
{
        _ecl_funcall3(@'print-object', x, stream);
}
#else
static void
write_structure(cl_object x, cl_object stream)
{
        cl_object print_function;
        unlikely_if (ecl_t_of(x->str.name) != t_symbol)
                FEerror("Found a corrupt structure with an invalid type name~%"
                        "  ~S", x->str.name);
        print_function = si_get_sysprop(x->str.name, @'si::structure-print-function');
        if (Null(print_function) || !ecl_print_structure()) {
                writestr_stream("#S", stream);
                /* structure_to_list conses slot names and values into
                 * a list to be printed.  print shouldn't allocate
                 * memory - Beppe
                 */
                x = structure_to_list(x);
                si_write_object(x, stream);
        } else {
                _ecl_funcall4(print_function, x, stream, ecl_make_fixnum(0));
        }
}
#endif /* !CLOS */

static void
write_readtable(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "readtable", ECL_NIL, stream);
}

static void
write_cfun(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "compiled-function", x->cfun.name, stream);
}

static void
write_codeblock(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "codeblock", x->cblock.name, stream);
}

static void
write_cclosure(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "compiled-closure", ECL_NIL, stream);
}

static void
write_foreign(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "foreign", x->foreign.tag, stream);
}

static void
write_frame(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "frame", ecl_make_fixnum(x->frame.size), stream);
}

static void
write_weak_pointer(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "weak-pointer", ECL_NIL, stream);
}

#ifdef ECL_THREADS
static void
write_process(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "process", x->process.name, stream);
}

static void
write_lock(cl_object x, cl_object stream)
{
        const char *prefix = x->lock.recursive?
                "lock" : "lock (nonrecursive)";
        _ecl_write_unreadable(x, prefix, x->lock.name, stream);
}

static void
write_condition_variable(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "semaphore", ECL_NIL, stream);
}

static void
write_semaphore(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "semaphore", ECL_NIL, stream);
}

static void
write_barrier(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "barrier", ECL_NIL, stream);
}

static void
write_mailbox(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "mailbox", ECL_NIL, stream);
}

#endif /* ECL_THREADS */

static void
write_illegal(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "illegal pointer", ECL_NIL, stream);
}

typedef void (*printer)(cl_object x, cl_object stream);

static printer dispatch[FREE+1] = {
        0 /* t_start = 0 */,
	_ecl_write_list, /* t_list = 1 */
	write_character, /* t_character = 2 */
	write_integer, /* t_fixnum = 3 */
	write_integer, /* t_bignum = 4 */
	write_ratio, /* t_ratio */
	write_float, /* t_singlefloat */
	write_float, /* t_doublefloat */
#ifdef ECL_LONG_FLOAT
	write_float, /* t_longfloat */
#endif
	write_complex, /* t_complex */
	_ecl_write_symbol, /* t_symbol */
	write_package, /* t_package */
	write_hashtable, /* t_hashtable */
	_ecl_write_array, /* t_array */
	_ecl_write_vector, /* t_vector */
#ifdef ECL_UNICODE
	_ecl_write_string, /* t_string */
#endif
	_ecl_write_base_string, /* t_base_string */
	_ecl_write_bitvector, /* t_bitvector */
	write_stream, /* t_stream */
	write_random, /* t_random */
	write_readtable, /* t_readtable */
	write_pathname, /* t_pathname */
	_ecl_write_bytecodes, /* t_bytecodes */
	_ecl_write_bclosure, /* t_bclosure */
	write_cfun, /* t_cfun */
	write_cfun, /* t_cfunfixed */
	write_cclosure, /* t_cclosure */
#ifdef CLOS
	write_instance, /* t_instance */
#else
	write_structure, /* t_structure */
#endif /* CLOS */
#ifdef ECL_THREADS
	write_process, /* t_process */
	write_lock, /* t_lock */
	write_lock, /* t_rwlock */
	write_condition_variable, /* t_condition_variable */
        write_semaphore, /* t_semaphore */
        write_barrier, /* t_barrier */
        write_mailbox, /* t_mailbox */
#endif
	write_codeblock, /* t_codeblock */
	write_foreign, /* t_foreign */
	write_frame, /* t_frame */
	write_weak_pointer, /* t_weak_pointer */
#ifdef ECL_SSE2
	_ecl_write_sse, /* t_sse_pack */
#endif
	/* t_end */
};

cl_object
si_write_ugly_object(cl_object x, cl_object stream)
{
	if (x == OBJNULL) {
		if (ecl_print_readably())
                        FEprint_not_readable(x);
		writestr_stream("#<OBJNULL>", stream);
	} else {
                int t = ecl_t_of(x);
                printer f = (t >= t_end)? write_illegal : dispatch[t];
                f(x, stream);
        }
	@(return x)
}
