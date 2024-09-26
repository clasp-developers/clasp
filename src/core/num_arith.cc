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

// This is a truncating division.
Integer_sp clasp_integer_divide(Integer_sp x, Integer_sp y) {
  Bignum_sp bx = x.asOrNull<Bignum_O>(), by = y.asOrNull<Bignum_O>();
  if (bx && by)
    return core__next_truncate(bx, by).as_unsafe<Integer_O>();
  if (bx)
    return core__next_ftruncate(bx, y.unsafe_fixnum()).as_unsafe<Integer_O>();
  if (by)
    return fix_divided_by_next(x.unsafe_fixnum(), by);

  Fixnum fy = y.unsafe_fixnum();
  if (fy == 0)
    ERROR_DIVISION_BY_ZERO(x, y);
  else
    // Note that / truncates towards zero as of C++11, as we want.
    return clasp_make_fixnum(x.unsafe_fixnum() / fy);
}

CL_LAMBDA(&rest nums);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(gcd)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__gcd(List_sp nums) {
  if (nums.nilp())
    return clasp_make_fixnum(0);
  /* INV: clasp_gcd() checks types */
  Integer_sp gcd = gc::As<Integer_sp>(oCar(nums));
  nums = oCdr(nums);
  if (nums.nilp()) {
    return (Real_O::minusp(gcd) ? gc::As<Integer_sp>(clasp_negate(gcd)) : gcd);
  }
  while (nums.consp()) {
    gcd = clasp_gcd(gcd, gc::As<Integer_sp>(oCar(nums)));
    nums = oCdr(nums);
  }
  return gcd;
}

// NOTE: C++17 defines a gcd which could hypothetically be faster than
// Euclid's algorithm. (Probably not though, machine integers are small.)
// In any case we should probably use it, if C++ implementations ever
// reliably get that far.
gc::Fixnum gcd(gc::Fixnum a, gc::Fixnum b) {
  if (a == 0)
    return (std::abs(b));
  return gcd(b % a, a);
}

Integer_sp clasp_gcd(Integer_sp x, Integer_sp y, int yidx) {
  Bignum_sp bx = x.asOrNull<Bignum_O>(), by = y.asOrNull<Bignum_O>();
  if (bx && by)
    return core__next_gcd(bx, by);
  if (bx)
    return core__next_fgcd(bx, y.unsafe_fixnum());
  if (by)
    return core__next_fgcd(by, x.unsafe_fixnum());

  return clasp_make_fixnum(gcd(x.unsafe_fixnum(), y.unsafe_fixnum()));
}

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(lcm)dx");
DOCGROUP(clasp);
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
    Number_sp t = lcm * numi;
    Number_sp g = clasp_gcd(numi, lcm);
    if (!Number_O::zerop(g)) {
      lcm = gc::As<Integer_sp>(t / g);
    }
  }
  return Real_O::minusp(lcm) ? gc::As<Integer_sp>(clasp_negate(lcm)) : gc::As<Integer_sp>(lcm);
};

SYMBOL_EXPORT_SC_(ClPkg, gcd);
SYMBOL_EXPORT_SC_(ClPkg, lcm);

}; // namespace core
