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
#define DEBUG_LEVEL_FULL

#include <boost/format.hpp>
#include <clasp/core/common.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/conditions.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/random.h>
#include <clasp/core/wrappers.h>

namespace core {

#define ARGS_RandomState_O_make "(&optional state)"
#define DECL_RandomState_O_make ""
#define DOCS_RandomState_O_make "getUniversalTime"
RandomState_sp RandomState_O::make(T_sp state) {
  if (RandomState_sp rs = state.asOrNull<RandomState_O>()) {
    return RandomState_O::create(rs);
  } else if (state.nilp()) {
    RandomState_sp currentState = gc::As<RandomState_sp>(cl::_sym_STARrandom_stateSTAR->symbolValue());
    return RandomState_O::create(currentState);
  } else if (state == _lisp->_true()) {
    return RandomState_O::create();
  }
  SIMPLE_ERROR(BF("Illegal argument for make-random-state: ~a") % _rep_(state));
}

#define ARGS_cl_random "(olimit &optional (random-state cl:*random-state*))"
#define DECL_cl_random ""
#define DOCS_cl_random "random"
T_sp cl_random(T_sp olimit, RandomState_sp random_state) {
  if (olimit.fixnump()) {
    boost::random::uniform_int_distribution<> range(0, olimit.unsafe_fixnum() - 1);
    return make_fixnum(range(random_state->_Producer));
  } else if (gc::IsA<Bignum_sp>(olimit)) {
    IMPLEMENT_MEF(BF("Implement generating Bignum random numbers"));
  } else if (DoubleFloat_sp df = olimit.asOrNull<DoubleFloat_O>()) {
    boost::random::uniform_real_distribution<> range(0.0, df->get());
    return DoubleFloat_O::create(range(random_state->_Producer));
  } else if (olimit.single_floatp()) {
    float flimit = olimit.unsafe_single_float();
    boost::random::uniform_real_distribution<> range(0.0, flimit);
    return clasp_make_single_float(range(random_state->_Producer));
  }
  SIMPLE_ERROR(BF("Illegal limit for random"));
}

EXPOSE_CLASS(core, RandomState_O);

void RandomState_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<RandomState_O>();
  af_def(ClPkg, "make-random-state", &RandomState_O::make, ARGS_RandomState_O_make, DECL_RandomState_O_make, DOCS_RandomState_O_make);
  af_def(ClPkg, "random", &cl_random, ARGS_cl_random, DECL_cl_random, DOCS_cl_random);
}

void RandomState_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CurrentPkg, RandomState, "", "", _lisp);
#endif
}
};
