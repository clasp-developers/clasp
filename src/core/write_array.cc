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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/print.h>
#include <clasp/core/array.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/str.h>
#include <clasp/core/bitVector.h>

namespace core {
static void
write_array_inner(bool vector, T_sp x, T_sp stream) {
  //	printf("%s:%d write_array_inner\n", __FILE__, __LINE__ );
  //cl_env_ptr env = ecl_process_env();
  std::vector<cl_index> adims;
  cl_index subscripts[CLASP_ARRAY_RANK_LIMIT];
  Fixnum n, j, m, k, i;
  Fixnum print_length;
  Fixnum print_level;
  bool readably = clasp_print_readably();

  if (vector) {
    adims = gc::As<Vector_sp>(x)->dimensions();
    n = 1;
  } else {
    Array_sp arr = gc::As<Array_sp>(x);
    adims = arr->dimensions();
    n = arr->rank();
  }
  if (readably) {
    print_length = MOST_POSITIVE_FIXNUM;
    print_level = MOST_POSITIVE_FIXNUM;
  } else {
    if (!clasp_print_array()) {
      writestr_stream(vector ? "#<vector " : "#<array ", stream);
      clasp_write_addr(x, stream);
      clasp_write_char('>', stream);
      return;
    }
    print_level = clasp_print_level();
    print_length = clasp_print_length();
  }
  clasp_write_char('#', stream);
  if (print_level == 0)
    return;
  if (readably) {
    clasp_write_char('A', stream);
    clasp_write_char('(', stream);
    write_object(gc::As<Array_sp>(x)->element_type_as_symbol(), stream);
    clasp_write_char(' ', stream);
    if (n > 0) {
      clasp_write_char('(', stream);
      for (j = 0; j < n; j++) {
        write_object(clasp_make_fixnum(adims[j]), stream);
        if (j < n - 1)
          clasp_write_char(' ', stream);
      }
      clasp_write_char(')', stream);
    } else {
      write_object(_Nil<T_O>(), stream);
    }
    clasp_write_char(' ', stream);
  } else if (!vector) {
    _clasp_write_fixnum(n, stream);
    clasp_write_char('A', stream);
  }
  DynamicScopeManager scope;
  if (print_level >= n) {
    /* We can write the elements of the array */
    print_level -= n;
    scope.pushSpecialVariableAndSet(cl::_sym_STARprint_levelSTAR, clasp_make_fixnum(print_level));
  } else {
    /* The elements of the array are not printed */
    n = print_level;
    print_level = -1;
  }
  for (j = 0; j < n; j++)
    subscripts[j] = 0;
  for (m = 0, j = 0;;) {
    for (i = j; i < n; i++) {
      if (subscripts[i] == 0) {
        clasp_write_char('(', stream);
        if (adims[i] == 0) {
          clasp_write_char(')', stream);
          j = i - 1;
          k = 0;
          goto INC;
        }
      }
      if (subscripts[i] > 0)
        clasp_write_char(' ', stream);
      if (subscripts[i] >= print_length) {
        writestr_stream("...)", stream);
        k = adims[i] - subscripts[i];
        subscripts[i] = 0;
        for (j = i + 1; j < n; j++)
          k *= adims[j];
        j = i - 1;
        goto INC;
      }
    }
    /* FIXME: This conses! */
    if (print_level >= 0)
      write_object(gc::As<Array_sp>(x)->aref_unsafe(m), stream);
    else
      clasp_write_char('#', stream);
    j = n - 1;
    k = 1;

  INC:
    while (j >= 0) {
      if (++subscripts[j] < adims[j])
        break;
      subscripts[j] = 0;
      clasp_write_char(')', stream);
      --j;
    }
    if (j < 0)
      break;
    m += k;
  }
#if 0
	if (print_level >= 0) {
	    clasp_bds_unwind1(env);
	}
#endif
  if (readably) {
    clasp_write_char(')', stream);
  }
}

void Array_O::__write__(T_sp stream) const {
  if (this->rank() == 0) {
    writestr_stream("#0A0", stream);
  } else if (this->rank() == 1) {
    write_array_inner(true, this->asSmartPtr(), stream);
  } else {
    write_array_inner(0, this->asSmartPtr(), stream);
  }
}

void Vector_O::__write__(T_sp stream) const {
  write_array_inner(1, this->asSmartPtr(), stream);
}

#ifdef CLASP_UNICODE
void _clasp_write_string(T_sp x, T_sp stream) {
  cl_index ndx;
  if (!clasp_print_escape() && !clasp_print_readably()) {
    for (ndx = 0; ndx < x->string.fillp; ndx++)
      clasp_write_char(x->string.self[ndx], stream);
  } else {
    clasp_write_char('"', stream);
    for (ndx = 0; ndx < x->string.fillp; ndx++) {
      clasp_character c = x->string.self[ndx];
      if (c == '"' || c == '\\')
        clasp_write_char('\\', stream);
      clasp_write_char(c, stream);
    }
    clasp_write_char('"', stream);
  }
}
#endif

void BitVector_O::__write__(T_sp stream) const {
  if (!clasp_print_array() && !clasp_print_readably()) {
    writestr_stream("#<bit-vector ", stream);
    clasp_write_addr(this->asSmartPtr(), stream);
    clasp_write_char('>', stream);
  } else {
    cl_index ndx;
    writestr_stream("#*", stream);
    for (ndx = 0; ndx < this->dimension(); ndx++)
      //      if (x->vector.self.bit[(ndx /*+ x->vector.offset*/) / 8] & (0200 >> (ndx /*+ x->vector.offset*/) % 8))
      if (this->testBit(ndx))
        clasp_write_char('1', stream);
      else
        clasp_write_char('0', stream);
  }
}
};
