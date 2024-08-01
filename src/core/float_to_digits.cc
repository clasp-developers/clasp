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

namespace core {

template <typename T> T_mv float_to_digits(T_sp tdigits, T number, T_sp round_position, T_sp relativep) {
  using significand_type = typename fmt::detail::dragonbox::decimal_fp<T>::significand_type;

  StrNs_sp digits = tdigits.nilp() ? gc::As<StrNs_sp>(core__make_vector(cl::_sym_base_char, 10, true, clasp_make_fixnum(0)))
                                   : gc::As<StrNs_sp>(tdigits);
  auto decimal = fmt::detail::dragonbox::to_decimal(number);
  int digit_count = fmt::detail::count_digits(decimal.significand);
  auto position = decimal.exponent + digit_count;

  if (round_position.notnilp()) {
    int pos = gc::As<Fixnum_sp>(round_position).unsafe_fixnum();
    pos = relativep.nilp() ? (position - pos) : (pos + 1);

    if (pos < 0) {
      position -= pos;
      pos = 0;
    }

    if (pos < digit_count) {
      significand_type divisor = std::pow(10, digit_count - pos);
      decimal.significand = (decimal.significand + (divisor / 2)) / divisor;
    }
  }

  for (auto ch : std::to_string(decimal.significand))
    digits->vectorPushExtend(clasp_make_character(ch), 64);

  return Values(clasp_make_fixnum(significand == 0 ? 0 : position), digits);
}

CL_LAMBDA(digits number position relativep);
CL_DECLARE();
CL_DOCSTRING(R"dx(float_to_digits)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__float_to_digits(T_sp tdigits, Float_sp number, T_sp position, T_sp relativep) {
  ASSERT(tdigits.nilp() || gc::IsA<Str8Ns_sp>(tdigits));

  switch (clasp_t_of(number)) {
  case number_SingleFloat:
    return float_to_digits<float>(tdigits, unbox_single_float(gc::As<SingleFloat_sp>(number)), position, relativep);
  case number_DoubleFloat:
    return float_to_digits<double>(tdigits, gc::As<DoubleFloat_sp>(number)->get(), position, relativep);
    break;
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat:
    return float_to_digits<LongFloat>(tdigits, gc::As<LongFloat_sp>(number)->get(), position, relativep);
#endif
  default:
    SIMPLE_ERROR("Illegal type");
  }
}

SYMBOL_EXPORT_SC_(CorePkg, float_to_digits);

}; // namespace core
