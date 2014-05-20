/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    write_list.d -- ugly printer for SSE types
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

#ifdef ECL_SSE2
static int
is_all_FF(void *ptr, int size) {
	int i;
	for (i = 0; i < size; i++)
		if (((unsigned char*)ptr)[i] != 0xFF)
			return 0;
	return 1;
}

static void
write_sse_float(float v, cl_object stream)
{
	if (is_all_FF(&v, sizeof(float))) {
		writestr_stream(" TRUE", stream);
	} else {
                ecl_write_char(' ', stream);
                si_write_ugly_object(ecl_make_single_float(v), stream);
	}
}

static void
write_sse_double(double v, cl_object stream)
{
	if (is_all_FF(&v, sizeof(double)))
		writestr_stream(" TRUE", stream);
        else {
                ecl_write_char(' ', stream);
                si_write_ugly_object(ecl_make_double_float(v), stream);
	}
}

static void
write_sse_pack(cl_object x, cl_object stream)
{
	int i;
	cl_elttype etype = x->sse.elttype;
	cl_object mode = ecl_symbol_value(@'ext::*sse-pack-print-mode*');

	if (mode != ECL_NIL) {
		if (mode == @':float') etype = ecl_aet_sf;
		else if (mode == @':double') etype = ecl_aet_df;
		else etype = ecl_aet_b8;
	}

	switch (etype) {
	case ecl_aet_sf:
		for (i = 0; i < 4; i++)
                        write_sse_float(x->sse.data.sf[i], stream);
		break;
	case ecl_aet_df:
		write_sse_double(x->sse.data.df[0], stream);
		write_sse_double(x->sse.data.df[1], stream);
		break;
	default: {
                cl_object buffer = si_get_buffer_string();
		for (i = 0; i < 16; i++) {
                        ecl_string_push_extend(buffer, ' ');
                        if (i%4 == 0) ecl_string_push_extend(buffer, ' ');
                        si_integer_to_string(buffer, ecl_make_fixnum(x->sse.data.b8[i]),
                                             ecl_make_fixnum(16), ECL_NIL, ECL_NIL);
		}
                si_do_write_sequence(buffer, stream, ecl_make_fixnum(0), ECL_NIL);
                si_put_buffer_string(buffer);
                break;
        }
	}
}

void
_ecl_write_sse(cl_object x, cl_object stream)
{
        if (ecl_print_readably()) FEprint_not_readable(x);
        writestr_stream("#<SSE", stream);
        write_sse_pack(x, stream);
        ecl_write_char('>', stream);
}
#endif
