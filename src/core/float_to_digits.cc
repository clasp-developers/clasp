/*
    File: float_to_digits.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/numbers.h>
#include <clasp/core/num_co.h>
#include <clasp/core/character.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/bignum.h>
#include <clasp/core/wrappers.h>
#include "../schubfach/schubfach.hpp"

namespace core {

template <typename Float> T_mv float_to_digits(T_sp tdigits, Float number, T_sp round_position, T_sp relativep) {
  const char* num_to_text = "0123456789";
  schubfach::decimal_float decimal = number;
  auto digit_count = decimal.math.count_digits(decimal.significand);
  auto position = decimal.exponent + digit_count;

  StrNs_sp digits;

  if (round_position.notnilp()) {
    int pos = gc::As<Fixnum_sp>(round_position).unsafe_fixnum();
    pos = relativep.nilp() ? (position - pos) : (pos + 1);

    if (pos < 0) {
      position -= pos;
      pos = 0;
    }

    if (pos < digit_count) {
      decltype(decimal.significand) divisor = std::pow(10, digit_count - pos);
      decimal.significand = (decimal.significand + (divisor / 2)) / divisor;
      digit_count = decimal.math.count_digits(decimal.significand);
    }
  }

  if (decimal.significand == 0)
    position = 0;

  if (tdigits.nilp()) {
    digits = gc::As<StrNs_sp>(core__make_vector(cl::_sym_base_char, digit_count, true, clasp_make_fixnum(digit_count)));
  } else {
    digits = gc::As<StrNs_sp>(tdigits);
    digits->resize(digit_count);
  }

  if (Str8Ns_sp buffer8 = digits.asOrNull<Str8Ns_O>()) {
    for (size_t i = 0; i < digit_count; i++) {
      auto rem = decimal.significand % 10u;
      decimal.significand /= 10u;
      (*buffer8)[digit_count - i - 1] = num_to_text[rem];
    }
  } else if (StrWNs_sp bufferw = digits.asOrNull<StrWNs_O>()) {
    for (size_t i = 0; i < digit_count; i++) {
      auto rem = decimal.significand % 10u;
      decimal.significand /= 10u;
      (*bufferw)[digit_count - i - 1] = num_to_text[rem];
    }
  }

  return Values(clasp_make_fixnum(position), digits);
}

CL_LAMBDA(digits number position relativep);
CL_DECLARE();
CL_DOCSTRING(R"dx(float_to_digits)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__float_to_digits(T_sp tdigits, Float_sp number, T_sp position, T_sp relativep) {
  ASSERT(tdigits.nilp() || gc::IsA<Str8Ns_sp>(tdigits));
#ifdef CLASP_LONG_FLOAT
  if (number.isA<LongFloat_O>())
    return float_to_digits<long_float_t>(tdigits, number.as_unsafe<LongFloat_O>()->get(), position, relativep);
#endif
  if (number.isA<DoubleFloat_O>())
    return float_to_digits<double_float_t>(tdigits, number.as_unsafe<DoubleFloat_O>()->get(), position, relativep);
  if (number.single_floatp())
    return float_to_digits<single_float_t>(tdigits, number.unsafe_single_float(), position, relativep);
#ifdef CLASP_SHORT_FLOAT
  if (number.short_floatp())
    return float_to_digits<short_float_t>(tdigits, number.unsafe_short_float(), position, relativep);
#endif
  SIMPLE_ERROR("Illegal type");
}

SYMBOL_EXPORT_SC_(CorePkg, float_to_digits);

}; // namespace core
