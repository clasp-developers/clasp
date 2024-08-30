#pragma once
/*
    File: mathDispatch.h
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

//
// Macros to assist in dispatching for mathematical operations between mixed types
//
// These macros closely follow the approach used by ECL
// in math_dispatch2.h
//
//
#define MATH_DISPATCH_BEGIN(a, b)                                                                                                  \
  {                                                                                                                                \
    uint8_t ta = (uint8_t)(clasp_t_of(a));                                                                                                 \
    uint8_t tb = (uint8_t)(clasp_t_of(b));                                                                                                 \
    uint8_t dispatch_combo = ta * (uint8_t)(NumberType::NUM) + tb;                                                                                   \
    switch (dispatch_combo)

#define MDL(na, nb) ((uint8_t)(na) * (uint8_t)(NumberType::NUM) + (uint8_t)(nb))
#define case_Fixnum_v_Fixnum case MDL(NumberType::Fixnum, NumberType::Fixnum)
#define case_Fixnum_v_Bignum case MDL(NumberType::Fixnum, NumberType::Bignum)
#define case_Fixnum_v_ShortFloat case MDL(NumberType::Fixnum, NumberType::ShortFloat)
#define case_Fixnum_v_SingleFloat case MDL(NumberType::Fixnum, NumberType::SingleFloat)
#define case_Fixnum_v_DoubleFloat case MDL(NumberType::Fixnum, NumberType::DoubleFloat)
#define case_Fixnum_v_LongFloat case MDL(NumberType::Fixnum, NumberType::LongFloat)
#define case_Fixnum_v_Ratio case MDL(NumberType::Fixnum, NumberType::Ratio)
#define case_Fixnum_v_Complex case MDL(NumberType::Fixnum, NumberType::Complex)

#define case_Bignum_v_Fixnum case MDL(NumberType::Bignum, NumberType::Fixnum)
#define case_Bignum_v_Bignum case MDL(NumberType::Bignum, NumberType::Bignum)
#define case_Bignum_v_ShortFloat case MDL(NumberType::Bignum, NumberType::ShortFloat)
#define case_Bignum_v_SingleFloat case MDL(NumberType::Bignum, NumberType::SingleFloat)
#define case_Bignum_v_DoubleFloat case MDL(NumberType::Bignum, NumberType::DoubleFloat)
#define case_Bignum_v_LongFloat case MDL(NumberType::Bignum, NumberType::LongFloat)
#define case_Bignum_v_Ratio case MDL(NumberType::Bignum, NumberType::Ratio)
#define case_Bignum_v_Complex case MDL(NumberType::Bignum, NumberType::Complex)

#define case_ShortFloat_v_Fixnum case MDL(NumberType::ShortFloat, NumberType::Fixnum)
#define case_ShortFloat_v_Bignum case MDL(NumberType::ShortFloat, NumberType::Bignum)
#define case_ShortFloat_v_ShortFloat case MDL(NumberType::ShortFloat, NumberType::ShortFloat)
#define case_ShortFloat_v_SingleFloat case MDL(NumberType::ShortFloat, NumberType::SingleFloat)
#define case_ShortFloat_v_DoubleFloat case MDL(NumberType::ShortFloat, NumberType::DoubleFloat)
#define case_ShortFloat_v_LongFloat case MDL(NumberType::ShortFloat, NumberType::LongFloat)
#define case_ShortFloat_v_Ratio case MDL(NumberType::ShortFloat, NumberType::Ratio)
#define case_ShortFloat_v_Complex case MDL(NumberType::ShortFloat, NumberType::Complex)

#define case_SingleFloat_v_Fixnum case MDL(NumberType::SingleFloat, NumberType::Fixnum)
#define case_SingleFloat_v_Bignum case MDL(NumberType::SingleFloat, NumberType::Bignum)
#define case_SingleFloat_v_ShortFloat case MDL(NumberType::SingleFloat, NumberType::ShortFloat)
#define case_SingleFloat_v_SingleFloat case MDL(NumberType::SingleFloat, NumberType::SingleFloat)
#define case_SingleFloat_v_DoubleFloat case MDL(NumberType::SingleFloat, NumberType::DoubleFloat)
#define case_SingleFloat_v_LongFloat case MDL(NumberType::SingleFloat, NumberType::LongFloat)
#define case_SingleFloat_v_Ratio case MDL(NumberType::SingleFloat, NumberType::Ratio)
#define case_SingleFloat_v_Complex case MDL(NumberType::SingleFloat, NumberType::Complex)

#define case_DoubleFloat_v_Fixnum case MDL(NumberType::DoubleFloat, NumberType::Fixnum)
#define case_DoubleFloat_v_Bignum case MDL(NumberType::DoubleFloat, NumberType::Bignum)
#define case_DoubleFloat_v_ShortFloat case MDL(NumberType::DoubleFloat, NumberType::ShortFloat)
#define case_DoubleFloat_v_SingleFloat case MDL(NumberType::DoubleFloat, NumberType::SingleFloat)
#define case_DoubleFloat_v_DoubleFloat case MDL(NumberType::DoubleFloat, NumberType::DoubleFloat)
#define case_DoubleFloat_v_LongFloat case MDL(NumberType::DoubleFloat, NumberType::LongFloat)
#define case_DoubleFloat_v_Ratio case MDL(NumberType::DoubleFloat, NumberType::Ratio)
#define case_DoubleFloat_v_Complex case MDL(NumberType::DoubleFloat, NumberType::Complex)

#define case_LongFloat_v_Fixnum case MDL(NumberType::LongFloat, NumberType::Fixnum)
#define case_LongFloat_v_Bignum case MDL(NumberType::LongFloat, NumberType::Bignum)
#define case_LongFloat_v_ShortFloat case MDL(NumberType::LongFloat, NumberType::ShortFloat)
#define case_LongFloat_v_SingleFloat case MDL(NumberType::LongFloat, NumberType::SingleFloat)
#define case_LongFloat_v_DoubleFloat case MDL(NumberType::LongFloat, NumberType::DoubleFloat)
#define case_LongFloat_v_LongFloat case MDL(NumberType::LongFloat, NumberType::LongFloat)
#define case_LongFloat_v_Ratio case MDL(NumberType::LongFloat, NumberType::Ratio)
#define case_LongFloat_v_Complex case MDL(NumberType::LongFloat, NumberType::Complex)

#define case_Ratio_v_Fixnum case MDL(NumberType::Ratio, NumberType::Fixnum)
#define case_Ratio_v_Bignum case MDL(NumberType::Ratio, NumberType::Bignum)
#define case_Ratio_v_ShortFloat case MDL(NumberType::Ratio, NumberType::ShortFloat)
#define case_Ratio_v_SingleFloat case MDL(NumberType::Ratio, NumberType::SingleFloat)
#define case_Ratio_v_DoubleFloat case MDL(NumberType::Ratio, NumberType::DoubleFloat)
#define case_Ratio_v_LongFloat case MDL(NumberType::Ratio, NumberType::LongFloat)
#define case_Ratio_v_Ratio case MDL(NumberType::Ratio, NumberType::Ratio)
#define case_Ratio_v_Complex case MDL(NumberType::Ratio, NumberType::Complex)

#define case_Complex_v_Fixnum case MDL(NumberType::Complex, NumberType::Fixnum)
#define case_Complex_v_Bignum case MDL(NumberType::Complex, NumberType::Bignum)
#define case_Complex_v_ShortFloat case MDL(NumberType::Complex, NumberType::ShortFloat)
#define case_Complex_v_SingleFloat case MDL(NumberType::Complex, NumberType::SingleFloat)
#define case_Complex_v_DoubleFloat case MDL(NumberType::Complex, NumberType::DoubleFloat)
#define case_Complex_v_LongFloat case MDL(NumberType::Complex, NumberType::LongFloat)
#define case_Complex_v_Ratio case MDL(NumberType::Complex, NumberType::Ratio)
#define case_Complex_v_Complex case MDL(NumberType::Complex, NumberType::Complex)

#define MATH_DISPATCH_END()                                                                                                        \
  }                                                                                                                                \
  ;
