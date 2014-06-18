/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    write_array.d -- File interface.
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
#include <ecl/internal.h>

static void
write_array_inner(bool vector, cl_object x, cl_object stream)
{
	cl_env_ptr env = ecl_process_env();
	const cl_index *adims;
	cl_index subscripts[ECL_ARRAY_RANK_LIMIT];
	cl_fixnum n, j, m, k, i;
	cl_fixnum print_length;
	cl_fixnum print_level;
	bool readably = ecl_print_readably();

	if (vector) {
		adims = &x->vector.fillp;
		n = 1;
	} else {
		adims = x->array.dims;
		n = x->array.rank;
	}
	if (readably) {
		print_length = MOST_POSITIVE_FIXNUM;
		print_level = MOST_POSITIVE_FIXNUM;
	} else {
		if (!ecl_print_array()) {
			writestr_stream(vector? "#<vector " : "#<array ", stream);
			_ecl_write_addr(x, stream);
			ecl_write_char('>', stream);
			return;
		}
		print_level = ecl_print_level();
		print_length = ecl_print_length();
	}
	ecl_write_char('#', stream);
	if (print_level == 0)
		return;
	if (readably) {
		ecl_write_char('A', stream);
		ecl_write_char('(', stream);
		si_write_object(ecl_elttype_to_symbol(ecl_array_elttype(x)), stream);
		ecl_write_char(' ', stream);
		if (n > 0) {
			ecl_write_char('(', stream);
			for (j=0; j<n; j++) {
				si_write_object(ecl_make_fixnum(adims[j]), stream);
				if (j < n-1)
					ecl_write_char(' ', stream);
			}
			ecl_write_char(')', stream);
		} else {
			si_write_object(ECL_NIL, stream);
		}
		ecl_write_char(' ', stream);
	} else if (!vector) {
		_ecl_write_fixnum(n, stream);
		ecl_write_char('A', stream);
	}
	if (print_level >= n) {
		/* We can write the elements of the array */
		print_level -= n;
		ecl_bds_bind(env, @'*print-level*', ecl_make_fixnum(print_level));
	} else {
		/* The elements of the array are not printed */
		n = print_level;
		print_level = -1;
	}
	for (j = 0;  j < n;  j++)
		subscripts[j] = 0;
	for (m = 0, j = 0;;) {
		for (i = j;  i < n;  i++) {
			if (subscripts[i] == 0) {
				ecl_write_char('(', stream);
				if (adims[i] == 0) {
					ecl_write_char(')', stream);
					j = i-1;
					k = 0;
					goto INC;
				}
			}
			if (subscripts[i] > 0)
				ecl_write_char(' ', stream);
			if (subscripts[i] >= print_length) {
				writestr_stream("...)", stream);
				k=adims[i]-subscripts[i];
				subscripts[i] = 0;
				for (j = i+1;  j < n;  j++)
					k *= adims[j];
				j = i-1;
				goto INC;
			}
		}
		/* FIXME: This conses! */
		if (print_level >= 0)
			si_write_object(ecl_aref_unsafe(x, m), stream);
		else
			ecl_write_char('#', stream);
		j = n-1;
		k = 1;

	INC:
		while (j >= 0) {
			if (++subscripts[j] < adims[j])
				break;
			subscripts[j] = 0;
			ecl_write_char(')', stream);
			--j;
		}
		if (j < 0)
			break;
		m += k;
	}
	if (print_level >= 0) {
		ecl_bds_unwind1(env);
	}
	if (readably) {
		ecl_write_char(')', stream);
	}
}

void
_ecl_write_array(cl_object x, cl_object stream)
{
        write_array_inner(0, x, stream);
}

void
_ecl_write_vector(cl_object x, cl_object stream)
{
        write_array_inner(1, x, stream);
}

#ifdef ECL_UNICODE
void
_ecl_write_string(cl_object x, cl_object stream)
{
        cl_index ndx;
        if (!ecl_print_escape() && !ecl_print_readably()) {
                for (ndx = 0;  ndx < x->string.fillp;  ndx++)
                        ecl_write_char(x->string.self[ndx], stream);
        } else {
		ecl_write_char('"', stream);
		for (ndx = 0;  ndx < x->string.fillp;  ndx++) {
			ecl_character c = x->string.self[ndx];
			if (c == '"' || c == '\\')
				ecl_write_char('\\', stream);
			ecl_write_char(c, stream);
		}
		ecl_write_char('"', stream);
        }
}
#endif

void
_ecl_write_base_string(cl_object x, cl_object stream)
{
        cl_index ndx;
        if (!ecl_print_escape() && !ecl_print_readably()) {
                for (ndx = 0;  ndx < x->base_string.fillp;  ndx++)
                        ecl_write_char(x->base_string.self[ndx], stream);
        } else {
		ecl_write_char('"', stream);
		for (ndx = 0;  ndx < x->base_string.fillp;  ndx++) {
			int c = x->base_string.self[ndx];
			if (c == '"' || c == '\\')
				ecl_write_char('\\', stream);
			ecl_write_char(c, stream);
		}
		ecl_write_char('"', stream);
        }
}

void
_ecl_write_bitvector(cl_object x, cl_object stream)
{
        if (!ecl_print_array() && !ecl_print_readably()) {
                writestr_stream("#<bit-vector ", stream);
                _ecl_write_addr(x, stream);
                ecl_write_char('>', stream);
        } else {
                cl_index ndx;
		writestr_stream("#*", stream);
		for (ndx = 0;  ndx < x->vector.fillp;  ndx++)
			if (x->vector.self.bit[(ndx+x->vector.offset)/8] & (0200 >> (ndx+x->vector.offset)%8))
				ecl_write_char('1', stream);
			else
				ecl_write_char('0', stream);
        }
}
