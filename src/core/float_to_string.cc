/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#define ECL_INCLUDE_MATH_H
#include <float.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/character.h>
#include <clasp/core/array.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/numerics.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/num_co.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/float_to_digits.h>
namespace core {
T_sp
_clasp_ensure_buffer(T_sp buffer, gc::Fixnum length) {
  if (buffer.nilp()) {
          buffer = Str8Ns_O::make(length, ' ', true, clasp_make_fixnum(0));
  }
  return buffer;
}

void
_clasp_string_push_c_string(StrNs_sp s, const char *c) {
    for (; *c; c++) {
	s->vectorPushExtend(clasp_make_character(*c));
    }
}

static void
insert_char(StrNs_sp buffer, cl_index where, gc::Fixnum c) {
    ASSERT(buffer->arrayHasFillPointerP());
    gc::Fixnum end = buffer->fillPointer();
    buffer->vectorPushExtend(clasp_make_character('.'));
    if ( Str8Ns_sp buffer8 = buffer.asOrNull<Str8Ns_O>() ) {
	memmove(&(*buffer8)[where + 1], &(*buffer8)[where], (end - where)*buffer8->elementSizeInBytes());
	(*buffer8)[where] = c;
    } else {
	StrWNs_sp bufferw = gc::As_unsafe<StrWNs_sp>(buffer);
	memmove(&(*bufferw)[where + 1], &(*bufferw)[where], (end - where)*bufferw->elementSizeInBytes());
	(*bufferw)[where] = c;
    }
}

/**********************************************************************
     * FREE FORMAT (FIXED OR EXPONENT) OF FLOATS
     */

static void
print_float_exponent(T_sp buffer, T_sp number, gc::Fixnum exp) {
  T_sp r = cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue();
  gc::Fixnum e;
  switch (clasp_t_of(gc::As<Number_sp>(number))) {
  case number_SingleFloat:
    e = (r == cl::_sym_single_float || r == cl::_sym_ShortFloat_O) ? 'e' : 'f';
    break;
  case number_ShortFloat:
    e = (r == cl::_sym_single_float || r == cl::_sym_ShortFloat_O) ? 'e' : 'f';
    break;
#ifdef ECL_LONG_FLOAT
  case number_LongFloat:
    e = (r == @'long-float') ? 'e' : 'l';
    break;
  case number_DoubleFloat:
    e = (r == @'double-float') ? 'e' : 'd';
    break;
#else
  case number_DoubleFloat:
    e = (r == cl::_sym_DoubleFloat_O || r == cl::_sym_LongFloat_O) ? 'e' : 'd';
    break;
#endif
  default:
                  SIMPLE_ERROR(BF("Handle additional enumeration values value=%s t_of=%d") % _rep_(number).c_str() % clasp_t_of(gc::As<Number_sp>(number)));
  }
  if (e != 'e' || exp != 0) {
    StrNs_sp sbuffer = gc::As<StrNs_sp>(buffer);
    sbuffer->vectorPushExtend(clasp_make_character(e));
    core__integer_to_string(sbuffer, clasp_make_fixnum(exp), clasp_make_fixnum(10),
                         false, false);
  }
}

T_sp core_float_to_string_free(Float_sp number, Number_sp e_min, Number_sp e_max) {
  gc::Fixnum base = 0, e;
  if (clasp_float_nan_p(number)) {
    return eval::funcall(ext::_sym_float_nan_string, number);
  } else if (clasp_float_infinity_p(number)) {
    return eval::funcall(ext::_sym_float_infinity_string, number);
  }
  T_mv mv_exp = core__float_to_digits(_Nil<T_O>(), number, _Nil<T_O>(), _Nil<T_O>());
  Fixnum_sp exp = gc::As_unsafe<Fixnum_sp>(mv_exp);
  StrNs_sp buffer = gc::As<StrNs_sp>(mv_exp.second());
  e = exp.unsafe_fixnum();
  if (clasp_signbit(number)) {
    insert_char(buffer, base++, '-');
  }
  /* Do we have to print in exponent notation? */
  if (clasp_lowereq(exp, e_min) || clasp_lowereq(e_max, exp)) {
    insert_char(buffer, base + 1, '.');
    print_float_exponent(buffer, number, e - 1);
  } else if (e > 0) {
          gc::Fixnum l = gc::As<StrNs_sp>(buffer)->fillPointer() - base;
    while (l++ <= e) {
            buffer->vectorPushExtend(clasp_make_character('0'));
    }
    insert_char(buffer, base + e, '.');
    print_float_exponent(buffer, number, 0);
  } else {
    insert_char(buffer, base++, '0');
    insert_char(buffer, base++, '.');
    for (e = -e; e; e--) {
      insert_char(buffer, base++, '0');
    }
    print_float_exponent(buffer, number, 0);
  }
  return buffer;
}
};
