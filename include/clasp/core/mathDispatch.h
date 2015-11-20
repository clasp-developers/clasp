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
#ifndef _core_mathDispatch_H
#define _core_mathDispatch_H

//
// Macros to assist in dispatching for mathematical operations between mixed types
//
// These macros closely follow the approach used by ECL
// in math_dispatch2.h
//
//
#define MATH_DISPATCH_BEGIN(a, b)              \
  {                                            \
    int ta = (int)(clasp_t_of(a));             \
    int tb = (int)(clasp_t_of(b));             \
    int dispatch_combo = ta * (number_NUM)+tb; \
    switch (dispatch_combo)

#define MDL(na, nb) ((int)(na) * (int)(number_NUM) + (int)(nb))
#define case_Fixnum_v_Fixnum case MDL(number_Fixnum, number_Fixnum)
#define case_Fixnum_v_Bignum case MDL(number_Fixnum, number_Bignum)
#define case_Fixnum_v_ShortFloat case MDL(number_Fixnum, number_ShortFloat)
#define case_Fixnum_v_SingleFloat case MDL(number_Fixnum, number_SingleFloat)
#define case_Fixnum_v_DoubleFloat case MDL(number_Fixnum, number_DoubleFloat)
#define case_Fixnum_v_LongFloat case MDL(number_Fixnum, number_LongFloat)
#define case_Fixnum_v_Ratio case MDL(number_Fixnum, number_Ratio)
#define case_Fixnum_v_Complex case MDL(number_Fixnum, number_Complex)

#define case_Bignum_v_Fixnum case MDL(number_Bignum, number_Fixnum)
#define case_Bignum_v_Bignum case MDL(number_Bignum, number_Bignum)
#define case_Bignum_v_ShortFloat case MDL(number_Bignum, number_ShortFloat)
#define case_Bignum_v_SingleFloat case MDL(number_Bignum, number_SingleFloat)
#define case_Bignum_v_DoubleFloat case MDL(number_Bignum, number_DoubleFloat)
#define case_Bignum_v_LongFloat case MDL(number_Bignum, number_LongFloat)
#define case_Bignum_v_Ratio case MDL(number_Bignum, number_Ratio)
#define case_Bignum_v_Complex case MDL(number_Bignum, number_Complex)

#define case_ShortFloat_v_Fixnum case MDL(number_ShortFloat, number_Fixnum)
#define case_ShortFloat_v_Bignum case MDL(number_ShortFloat, number_Bignum)
#define case_ShortFloat_v_ShortFloat case MDL(number_ShortFloat, number_ShortFloat)
#define case_ShortFloat_v_SingleFloat case MDL(number_ShortFloat, number_SingleFloat)
#define case_ShortFloat_v_DoubleFloat case MDL(number_ShortFloat, number_DoubleFloat)
#define case_ShortFloat_v_LongFloat case MDL(number_ShortFloat, number_LongFloat)
#define case_ShortFloat_v_Ratio case MDL(number_ShortFloat, number_Ratio)
#define case_ShortFloat_v_Complex case MDL(number_ShortFloat, number_Complex)

#define case_SingleFloat_v_Fixnum case MDL(number_SingleFloat, number_Fixnum)
#define case_SingleFloat_v_Bignum case MDL(number_SingleFloat, number_Bignum)
#define case_SingleFloat_v_ShortFloat case MDL(number_SingleFloat, number_ShortFloat)
#define case_SingleFloat_v_SingleFloat case MDL(number_SingleFloat, number_SingleFloat)
#define case_SingleFloat_v_DoubleFloat case MDL(number_SingleFloat, number_DoubleFloat)
#define case_SingleFloat_v_LongFloat case MDL(number_SingleFloat, number_LongFloat)
#define case_SingleFloat_v_Ratio case MDL(number_SingleFloat, number_Ratio)
#define case_SingleFloat_v_Complex case MDL(number_SingleFloat, number_Complex)

#define case_DoubleFloat_v_Fixnum case MDL(number_DoubleFloat, number_Fixnum)
#define case_DoubleFloat_v_Bignum case MDL(number_DoubleFloat, number_Bignum)
#define case_DoubleFloat_v_ShortFloat case MDL(number_DoubleFloat, number_ShortFloat)
#define case_DoubleFloat_v_SingleFloat case MDL(number_DoubleFloat, number_SingleFloat)
#define case_DoubleFloat_v_DoubleFloat case MDL(number_DoubleFloat, number_DoubleFloat)
#define case_DoubleFloat_v_LongFloat case MDL(number_DoubleFloat, number_LongFloat)
#define case_DoubleFloat_v_Ratio case MDL(number_DoubleFloat, number_Ratio)
#define case_DoubleFloat_v_Complex case MDL(number_DoubleFloat, number_Complex)

#define case_LongFloat_v_Fixnum case MDL(number_LongFloat, number_Fixnum)
#define case_LongFloat_v_Bignum case MDL(number_LongFloat, number_Bignum)
#define case_LongFloat_v_ShortFloat case MDL(number_LongFloat, number_ShortFloat)
#define case_LongFloat_v_SingleFloat case MDL(number_LongFloat, number_SingleFloat)
#define case_LongFloat_v_DoubleFloat case MDL(number_LongFloat, number_DoubleFloat)
#define case_LongFloat_v_LongFloat case MDL(number_LongFloat, number_LongFloat)
#define case_LongFloat_v_Ratio case MDL(number_LongFloat, number_Ratio)
#define case_LongFloat_v_Complex case MDL(number_LongFloat, number_Complex)

#define case_Ratio_v_Fixnum case MDL(number_Ratio, number_Fixnum)
#define case_Ratio_v_Bignum case MDL(number_Ratio, number_Bignum)
#define case_Ratio_v_ShortFloat case MDL(number_Ratio, number_ShortFloat)
#define case_Ratio_v_SingleFloat case MDL(number_Ratio, number_SingleFloat)
#define case_Ratio_v_DoubleFloat case MDL(number_Ratio, number_DoubleFloat)
#define case_Ratio_v_LongFloat case MDL(number_Ratio, number_LongFloat)
#define case_Ratio_v_Ratio case MDL(number_Ratio, number_Ratio)
#define case_Ratio_v_Complex case MDL(number_Ratio, number_Complex)

#define case_Complex_v_Fixnum case MDL(number_Complex, number_Fixnum)
#define case_Complex_v_Bignum case MDL(number_Complex, number_Bignum)
#define case_Complex_v_ShortFloat case MDL(number_Complex, number_ShortFloat)
#define case_Complex_v_SingleFloat case MDL(number_Complex, number_SingleFloat)
#define case_Complex_v_DoubleFloat case MDL(number_Complex, number_DoubleFloat)
#define case_Complex_v_LongFloat case MDL(number_Complex, number_LongFloat)
#define case_Complex_v_Ratio case MDL(number_Complex, number_Ratio)
#define case_Complex_v_Complex case MDL(number_Complex, number_Complex)

#define MATH_DISPATCH_END() \
  }                         \
  ;

#endif
