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
T_sp _clasp_ensure_buffer(T_sp buffer, gc::Fixnum length) {
  if (buffer.nilp()) {
    buffer = Str8Ns_O::make(length, ' ', true, clasp_make_fixnum(0));
  }
  return buffer;
}

void _clasp_string_push_c_string(StrNs_sp s, const char* c) {
  for (; *c; c++) {
    s->vectorPushExtend(clasp_make_character(*c));
  }
}

static void insert_char(StrNs_sp buffer, cl_index where, gc::Fixnum c) {
  ASSERT(buffer->arrayHasFillPointerP());
  gc::Fixnum end = buffer->fillPointer();
  buffer->vectorPushExtend(clasp_make_character('.'));
  if (Str8Ns_sp buffer8 = buffer.asOrNull<Str8Ns_O>()) {
    memmove(&(*buffer8)[where + 1], &(*buffer8)[where], (end - where) * buffer8->elementSizeInBytes());
    (*buffer8)[where] = c;
  } else {
    StrWNs_sp bufferw = gc::As_unsafe<StrWNs_sp>(buffer);
    memmove(&(*bufferw)[where + 1], &(*bufferw)[where], (end - where) * bufferw->elementSizeInBytes());
    (*bufferw)[where] = c;
  }
}

/**********************************************************************
 * FREE FORMAT (FIXED OR EXPONENT) OF FLOATS
 */

static void print_float_exponent(T_sp buffer, Float_sp number, gc::Fixnum exp) {
  T_sp r = cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue();
  char e = 'e';
#ifdef CLASP_SHORT_FLOAT
  if (number.short_floatp())
    e = (r == cl::_sym_short_float) ? 'e' : 's';
  else if (number.single_floatp())
    e = (r == cl::_sym_single_float) ? 'e' : 'f';
#else
  if (number.single_floatp())
    e = (r == cl::_sym_single_float || r == cl::_sym_short_float) ? 'e' : 'f';
#endif
#ifdef CLASP_LONG_FLOAT
  else if (number.isA<DoubleFloat_O>())
    e = (r == cl::_sym_double_float) ? 'e' : 'd';
  else if (number.isA<LongFloat_O>())
    e = (r == cl::_sym_long_float) ? 'e' : 'l';
#else
  else if (number.isA<DoubleFloat_O>())
    e = (r == cl::_sym_double_float || r == cl::_sym_long_float) ? 'e' : 'd';
#endif
  if (e != 'e' || exp != 0) {
    StrNs_sp sbuffer = gc::As<StrNs_sp>(buffer);
    sbuffer->vectorPushExtend(clasp_make_character(e));
    core__integer_to_string(sbuffer, clasp_make_fixnum(exp), clasp_make_fixnum(10), false, false);
  }
}

T_sp core_float_to_string_free(Float_sp number, Number_sp e_min, Number_sp e_max) {
  gc::Fixnum base = 0, e;
  if (Float_O::isnan(number)) {
    return eval::funcall(ext::_sym_float_nan_string, number);
  } else if (Float_O::isinf(number)) {
    return eval::funcall(ext::_sym_float_infinity_string, number);
  }
  T_mv mv_exp = core__float_to_digits(nil<T_O>(), number, nil<T_O>(), nil<T_O>());
  Fixnum_sp exp = gc::As_unsafe<Fixnum_sp>(mv_exp);
  MultipleValues& mv = core::lisp_multipleValues();
  StrNs_sp buffer = gc::As<StrNs_sp>(mv.second(mv_exp.number_of_values()));
  e = exp.unsafe_fixnum();
  if (clasp_signbit(number)) {
    insert_char(buffer, base++, '-');
  }
  /* Do we have to print in exponent notation? */
  if (clasp_lowereq(exp, e_min) || clasp_lowereq(e_max, exp)) {
    insert_char(buffer, base + 1, '.');
    if (gc::As<StrNs_sp>(buffer)->fillPointer() == base + 2)
      buffer->vectorPushExtend(clasp_make_character('0'));
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
}; // namespace core
