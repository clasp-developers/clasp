/*
    File: random.cc
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
//#define DEBUG_LEVEL_FULL

#include <boost/format.hpp>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/random.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(&optional state);
CL_PKG_NAME(ClPkg,make-random-state);
CL_DEFUN RandomState_sp RandomState_O::make(T_sp state) {
  if (RandomState_sp rs = state.asOrNull<RandomState_O>()) {
    return RandomState_O::create(rs);
  } else if (state.nilp()) {
    RandomState_sp currentState = gc::As<RandomState_sp>(cl::_sym_STARrandom_stateSTAR->symbolValue());
    return RandomState_O::create(currentState);
  } else if (state == _lisp->_true()) {
    return RandomState_O::create_random();
  }
  TYPE_ERROR(state, Cons_O::createList(cl::_sym_or, cl::_sym_RandomState_O, cl::_sym_null,
                                       Cons_O::createList(cl::_sym_eql, cl::_sym_T_O)));
}

// sbcl says this type is needed for random  (OR (SINGLE-FLOAT (0.0)) (DOUBLE-FLOAT (0.0d0)) (INTEGER 1))
// better use just float instead of using all subtypes
#define TYPE_ERROR_cl_random(_datum_)                                                                              \
  TYPE_ERROR(_datum_, Cons_O::createList(cl::_sym_or,                                                              \
                                         Cons_O::createList(cl::_sym_Integer_O, make_fixnum(1)),                   \
                                         Cons_O::createList(cl::_sym_float, Cons_O::createList(clasp_make_single_float(0.0)))))

CL_LAMBDA(olimit &optional (random-state cl:*random-state*));
CL_DECLARE();
CL_DOCSTRING("random");
CL_DEFUN T_sp cl__random(Number_sp olimit, RandomState_sp random_state) {
  // olimit---a positive integer, or a positive float.
  // Fixing #292
  if (olimit.fixnump()) {
    gc::Fixnum n = olimit.unsafe_fixnum();
    if (n > 0) {
      boost::random::uniform_int_distribution<uint64_t> range(0, n - 1);
      return make_fixnum(range(random_state->_Producer));
    } else TYPE_ERROR_cl_random(olimit);
  } else if (gc::IsA<Bignum_sp>(olimit)) {
    Bignum_sp gbn = gc::As_unsafe<Bignum_sp>(olimit);
    if (clasp_plusp (gbn)) {
      boost::uniform_int<bmp::mpz_int> gen(0, bmp::mpz_int(gbn->get().get_mpz_t()));
      auto rnd = gen(random_state->_Producer);
      bmp::mpz_int v = rnd;
      mpz_t z;
      mpz_init(z);
      mpz_set(z,v.backend().data());
      return Integer_O::create(mpz_class(z));
    }
    else TYPE_ERROR_cl_random(olimit);
  } else if (DoubleFloat_sp df = olimit.asOrNull<DoubleFloat_O>()) {
    if (df->get() > 0.0) {
      boost::random::uniform_real_distribution<> range(0.0, df->get());
      return DoubleFloat_O::create(range(random_state->_Producer));
    } else TYPE_ERROR_cl_random(olimit);
  } else if (olimit.single_floatp()) {
    float flimit = olimit.unsafe_single_float();
    if (flimit >  0.0f) {
      boost::random::uniform_real_distribution<> range(0.0, flimit);
      return clasp_make_single_float(range(random_state->_Producer));
    } else TYPE_ERROR_cl_random(olimit);
  }
  TYPE_ERROR_cl_random(olimit);
}





};
