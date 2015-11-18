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
#include <clasp/core/strWithFillPtr.h>
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
    buffer = StrWithFillPtr_O::create(' ', length, 0, true);
#if 0
	    buffer = core_make_vector(cl::_sym_base_char, clasp_make_fixnum(length),
				      _lisp->_true() /* adjustable */,
				      clasp_make_fixnum(0) /* fill pointer */,
				      _Nil<T_O>() /* displacement */,
				      _Nil<T_O>() /* displ. offset */);
#endif
  }
  return buffer;
}

void
_clasp_string_push_c_string(StrWithFillPtr_sp s, const char *c) {
  for (; *c; c++) {
    s->pushCharExtend(*c);
  }
}

static void
insert_char(StrWithFillPtr_sp buffer, cl_index where, gc::Fixnum c) {
  gc::Fixnum end = buffer->fillPointer();
  buffer->pushCharExtend('.');
  memmove(&(*buffer)[where + 1], &(*buffer)[where], end - where);
  //clasp_copy_subarray(buffer, where+1, buffer, where, end - where);
  (*buffer)[where] = c; // clasp_char_set(buffer, where, c);
}

static T_sp
push_base_string(T_sp buffer, StrWithFillPtr_sp s) {
  buffer = _clasp_ensure_buffer(buffer, s->fillPointer());
  gc::As<StrWithFillPtr_sp>(buffer)->pushString(s->c_str());
  return buffer;
}

/**********************************************************************
     * FREE FORMAT (FIXED OR EXPONENT) OF FLOATS
     */

static void
print_float_exponent(T_sp buffer, T_sp number, gc::Fixnum exp) {
  T_sp r = cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue();
  gc::Fixnum e;
  switch (clasp_t_of(number)) {
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
    SIMPLE_ERROR(BF("Handle additional enumeration values value=%s t_of=%d") % _rep_(number).c_str() % clasp_t_of(number));
  }
  if (e != 'e' || exp != 0) {
    StrWithFillPtr_sp sbuffer = gc::As<StrWithFillPtr_sp>(buffer);
    sbuffer->pushCharExtend(e);
    core_integerToString(sbuffer, clasp_make_fixnum(exp), clasp_make_fixnum(10),
                         false, false);
  }
}

T_sp
core_float_to_string_free(T_sp buffer_or_nil, Float_sp number,
                          T_sp e_min, T_sp e_max) {
  gc::Fixnum base, e;
  if (clasp_float_nan_p(number)) {
    T_sp s = eval::funcall(ext::_sym_float_nan_string, number);
    return push_base_string(buffer_or_nil, s);
  } else if (clasp_float_infinity_p(number)) {
    T_sp s = eval::funcall(ext::_sym_float_infinity_string, number);
    return push_base_string(buffer_or_nil, s);
  }
  base = cl_length(buffer_or_nil);
  T_mv mv_exp = core_float_to_digits(buffer_or_nil, number, _Nil<T_O>(), _Nil<T_O>());
  T_sp exp = mv_exp;
  StrWithFillPtr_sp buffer = gc::As<StrWithFillPtr_sp>(mv_exp.second());
  e = clasp_to_fixnum(exp);
  if (clasp_signbit(number)) {
    insert_char(buffer, base++, '-');
  }
  /* Do we have to print in exponent notation? */
  if (clasp_lowereq(exp, e_min) || clasp_lowereq(e_max, exp)) {
    insert_char(buffer, base + 1, '.');
    print_float_exponent(buffer, number, e - 1);
  } else if (e > 0) {
    gc::Fixnum l = gc::As<StrWithFillPtr_sp>(buffer)->fillPointer() - base;
    while (l++ <= e) {
      buffer->pushCharExtend('0');
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
