/*
    File: num_arith.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_arith.c  -- Arithmetic operations
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/numbers.h>
#include <clasp/core/bignum.h>
#include <clasp/core/num_arith.h>
#include <clasp/core/wrappers.h>

namespace core {

SYMBOL_EXPORT_SC_(CorePkg, integer_divide);
Integer_sp clasp_integer_divide(Integer_sp x, Integer_sp y) {
  NumberType tx, ty;
  tx = clasp_t_of(x);
  ty = clasp_t_of(y);
  if (tx == number_Fixnum) {
    if (ty == number_Fixnum) {
      if (y.unsafe_fixnum() == 0)
        ERROR_DIVISION_BY_ZERO(x, y);
      return (clasp_make_fixnum(x.unsafe_fixnum() / y.unsafe_fixnum()));
    } else if (ty == number_Bignum) {
      return _clasp_fix_divided_by_big(x.unsafe_fixnum(), gc::As_unsafe<Bignum_sp>(y)->get());
    } else {
      ERROR_WRONG_TYPE_NTH_ARG(core::_sym_integer_divide, 2, y, cl::_sym_Integer_O);
    }
  }
  if (tx == number_Bignum) {
    if (ty == number_Bignum) {
      return _clasp_big_divided_by_big(gc::As_unsafe<Bignum_sp>(x)->get(), gc::As_unsafe<Bignum_sp>(y)->get());
    } else if (ty == number_Fixnum) {
      return _clasp_big_divided_by_fix(gc::As_unsafe<Bignum_sp>(x)->get(), y.unsafe_fixnum());
    } else {
      QERROR_WRONG_TYPE_NTH_ARG(2, y, cl::_sym_Integer_O);
    }
  }
  ERROR_WRONG_TYPE_NTH_ARG(core::_sym_integer_divide, 1, x, cl::_sym_Integer_O);
  UNREACHABLE();
}

CL_LAMBDA(&rest nums);
CL_DECLARE();
CL_DOCSTRING("gcd");
CL_DEFUN Integer_sp cl__gcd(List_sp nums) {
  if (nums.nilp())
    return clasp_make_fixnum(0);
  /* INV: clasp_gcd() checks types */
  Integer_sp gcd = gc::As<Integer_sp>(oCar(nums));
  nums = oCdr(nums);
  if (nums.nilp()) {
    return (clasp_minusp(gcd) ? gc::As<Integer_sp>(clasp_negate(gcd)) : gcd);
  }
  while (nums.consp()) {
    gcd = clasp_gcd(gcd, gc::As<Integer_sp>(oCar(nums)));
    nums = oCdr(nums);
  }
  return gcd;
}

gc::Fixnum gcd(gc::Fixnum a, gc::Fixnum b)
{
    if (a == 0)
      return (abs(b));
    return gcd(b % a, a);
}

Integer_sp clasp_gcd(Integer_sp x, Integer_sp y, int yidx) {
  switch (clasp_t_of(x)) {
  case number_Fixnum: {
    if (clasp_t_of(y) == number_Fixnum)
      return clasp_make_fixnum(gcd(x.unsafe_fixnum(), y.unsafe_fixnum()));
    Bignum_sp big(Bignum_O::create(x.unsafe_fixnum()));
    x = big;
  }
  case number_Bignum:
    break;
  default:
    QERROR_WRONG_TYPE_NTH_ARG(yidx, x, cl::_sym_Integer_O);
  }
  switch (clasp_t_of(y)) {
  case number_Fixnum: {
    Bignum_sp big(Bignum_O::create(y.unsafe_fixnum()));
    y = big;
  }
  case number_Bignum:
    break;
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1 + yidx, y, cl::_sym_Integer_O);
  }
  Bignum_sp temp = gc::As<Bignum_sp>(_clasp_big_gcd(gc::As<Bignum_sp>(x), gc::As<Bignum_sp>(y)));
  if (temp->fits_sint_p())
    return clasp_make_fixnum(temp->as_int());
  else return temp;                                 
}

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("lcm");
CL_DEFUN Integer_sp cl__lcm(List_sp nums) {
  if (nums.nilp())
    return clasp_make_fixnum(1);
  /* INV: clasp_gcd() checks types. By placing `numi' before `lcm' in
	   this call, we make sure that errors point to `numi' */
  Integer_sp lcm = gc::As<Integer_sp>(oCar(nums));
  int yidx = 1;
  nums = oCdr(nums);
  while (nums.consp()) {
    Integer_sp numi = gc::As<Integer_sp>(oCar(nums));
    nums = oCdr(nums);
    yidx++;
    Number_sp t = clasp_times(lcm, numi);
    Number_sp g = clasp_gcd(numi, lcm);
    if (!clasp_zerop(g)) {
      lcm = gc::As<Integer_sp>(clasp_divide(t, g));
    }
  }
  return clasp_minusp(lcm) ? gc::As<Integer_sp>(clasp_negate(lcm)) : gc::As<Integer_sp>(lcm);
};

  SYMBOL_EXPORT_SC_(ClPkg, gcd);
  SYMBOL_EXPORT_SC_(ClPkg, lcm);

};
