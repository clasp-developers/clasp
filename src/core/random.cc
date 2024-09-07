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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/lispStream.fwd.h>
#include <clasp/core/print.h>
#include <clasp/core/random.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(&optional state);
CL_PKG_NAME(ClPkg, make-random-state);
DOCGROUP(clasp);
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
#define TYPE_ERROR_cl_random(_datum_)                                                                                              \
  TYPE_ERROR(_datum_, Cons_O::createList(cl::_sym_or, Cons_O::createList(cl::_sym_Integer_O, make_fixnum(1)),                      \
                                         Cons_O::createList(cl::_sym_float, Cons_O::createList(clasp_make_single_float(0.0)))))

CL_LAMBDA(olimit &optional (random-state cl:*random-state*));
CL_DECLARE();
CL_DOCSTRING(R"dx(random)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__random(Number_sp olimit, RandomState_sp random_state) {
  // olimit---a positive integer, or a positive float.
  // Fixing #292
  if (olimit.fixnump()) {
    gc::Fixnum n = olimit.unsafe_fixnum();
    if (n > 0) {
      std::uniform_int_distribution<uint64_t> range(0, n - 1);
      return make_fixnum(range(random_state->_Producer._value));
    } else
      TYPE_ERROR_cl_random(olimit);
  } else if (gc::IsA<Bignum_sp>(olimit)) {
    Bignum_sp gbn = gc::As_unsafe<Bignum_sp>(olimit);
    mp_size_t len = gbn->length();
    if (len < 1)
      TYPE_ERROR_cl_random(olimit); // positive only
    mp_limb_t res[len];
    const mp_limb_t minlimb = std::numeric_limits<mp_limb_t>::min();
    const mp_limb_t maxlimb = std::numeric_limits<mp_limb_t>::max();
    std::uniform_int_distribution<mp_limb_t> range(minlimb, maxlimb);
    for (mp_size_t i = 0; i < len; ++i)
      res[i] = range(random_state->_Producer._value);
    // FIXME: We KLUDGE the range by doing mod (basically).
    // This will in general result in deviations from a truly uniform
    // distribution.
    // For comparison, the native mpz code tries 80 rounds of rejection
    // sampling, and then gives up and uses mod.
    // FIXME: Also we could avoid actually consing a bignum for the
    // intermediate here.
    BIGNUM_NORMALIZE(len, res);
    return cl__mod(bignum_result(len, res), gbn);
  } else if (DoubleFloat_sp df = olimit.asOrNull<DoubleFloat_O>()) {
    if (df->get() == std::numeric_limits<double>::denorm_min()) {
      return DoubleFloat_O::create(0.0);
    } else if (df->get() > 0.0) {
      std::uniform_real_distribution<double_float_t> range(0.0, df->get());
      return DoubleFloat_O::create(range(random_state->_Producer._value));
    } else
      TYPE_ERROR_cl_random(olimit);
#ifdef CLASP_SHORT_FLOAT
  } else if (olimit.short_floatp()) {
    short_float_t flimit = olimit.unsafe_short_float();
    if (flimit == std::numeric_limits<short_float_t>::denorm_min()) {
      return ShortFloat_O::create(short_float_t{0});
    } else if (flimit > short_float_t{0}) {
      std::uniform_real_distribution<single_float_t> range(short_float_t{0}, flimit);
      return ShortFloat_O::create(range(random_state->_Producer._value));
    } else
      TYPE_ERROR_cl_random(olimit);
#endif
#ifdef CLASP_LONG_FLOAT
  } else if (LongFloat_sp lf = olimit.asOrNull<LongFloat_O>()) {
    if (lf->get() == std::numeric_limits<long_float_t>::denorm_min()) {
      return LongFloat_O::create(long_float_t{0.0});
    } else if (lf->get() > long_float_t{0.0}) {
      std::uniform_real_distribution<long_float_t> range(long_float_t{0.0}, lf->get());
      return LongFloat_O::create(range(random_state->_Producer._value));
    } else
      TYPE_ERROR_cl_random(olimit);
#endif
  } else if (olimit.single_floatp()) {
    float flimit = olimit.unsafe_single_float();
    if (flimit == std::numeric_limits<float>::denorm_min()) {
      return clasp_make_single_float(0.0f);
    } else if (flimit > 0.0f) {
      std::uniform_real_distribution<single_float_t> range(0.0, flimit);
      return clasp_make_single_float(range(random_state->_Producer._value));
    } else
      TYPE_ERROR_cl_random(olimit);
  }
  TYPE_ERROR_cl_random(olimit);
}

// Return a double, sampled from a unit uniform distribution.
// This used to be done through a totally different random mechanism, and is now
// only here to be compatible with cando (chem/energySketchNonbond.cc).
double randomNumber01() {
  RandomState_sp random_state = gc::As<RandomState_sp>(cl::_sym_STARrandom_stateSTAR->symbolValue());
  std::uniform_real_distribution<> range(0.0, 1.0);
  return range(random_state->_Producer._value);
}

// Like the above, including being defined for compatibility (chem/twister.cc),
// but samples a normal distribution with mean 0 and stddev 1.
double randomNumberNormal01() {
  RandomState_sp random_state = gc::As<RandomState_sp>(cl::_sym_STARrandom_stateSTAR->symbolValue());
  std::normal_distribution<> gauss(0.0, 1.0);
  return gauss(random_state->_Producer._value);
}

void RandomState_O::__write__(T_sp stream) const {
  bool readably = clasp_print_readably();
  if (readably) {
    this->__writeReadable__(stream);
    return;
  }
  clasp_write_string("#<RANDOM-STATE>", stream);
}

void RandomState_O::__writeReadable__(T_sp stream) const {
  clasp_write_string("#.(core:random-state-set (make-random-state t) \"", stream);
  std::string state = this->random_state_get();
  clasp_write_string(state, stream);
  clasp_write_string("\")", stream);
}

}; // namespace core
