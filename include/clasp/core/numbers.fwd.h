/*
    File: numbers.fwd.h
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
#ifndef values_fwd_H
#define values_fwd_H
namespace core {
FORWARD(Number);
FORWARD(Real);
FORWARD(Rational);
FORWARD(Integer);
#ifdef USE_HEAP_FIXNUM
FORWARD(Fixnum);
#endif
FORWARD(Float);
FORWARD(ShortFloat);
FORWARD(DoubleFloat);
#ifdef CLASP_LONG_FLOAT
FORWARD(LongFloat);
#endif
FORWARD(Complex);
FORWARD(Ratio);
FORWARD(Bool);

   typedef double LongFloat;

   Fixnum clasp_to_fixnum( core::Integer_sp );
   double clasp_to_double( core::Number_sp );
   float clasp_to_float( core::Number_sp );
   double clasp_to_double( core::T_sp );
   double clasp_to_double( core::General_sp );
   double clasp_to_double( core::Number_sp );
   double clasp_to_double( core::Real_sp );
   double clasp_to_double( core::Integer_sp );
   double clasp_to_double( core::DoubleFloat_sp );
   float clasp_to_float( core::SingleFloat_sp );
   float clasp_to_float( core::T_sp );
   float clasp_to_float( core::General_sp );
   Fixnum_sp clasp_make_fixnum(gc::Fixnum i);
   Fixnum_sp make_fixnum(gc::Fixnum i);
   SingleFloat_sp clasp_make_single_float(float d);
   DoubleFloat_sp clasp_make_double_float(double d);

   cl_index            clasp_to_size( core::T_sp );
   Integer_sp clasp_make_integer(size_t i);


     Fixnum              clasp_to_fixnum( core::T_sp );
  short               clasp_to_short( core::T_sp );
  unsigned short      clasp_to_ushort( core::T_sp );
  int                 clasp_to_int( core::T_sp );
  unsigned int        clasp_to_uint( core::T_sp );
  long                clasp_to_long( core::T_sp );
  unsigned long       clasp_to_ulong( core::T_sp );
  long long           clasp_to_longlong( core::T_sp );
  unsigned long long  clasp_to_ulonglong( core::T_sp );
  int8_t              clasp_to_int8_t( core::T_sp );
  uint8_t             clasp_to_uint8_t( core::T_sp );
  int16_t             clasp_to_int16_t( core::T_sp );
  uint16_t            clasp_to_uint16_t( core::T_sp );
  int32_t             clasp_to_int32_t( core::T_sp );
  uint32_t            clasp_to_uint32_t( core::T_sp );
  int64_t             clasp_to_int64_t( core::T_sp );
  uint64_t            clasp_to_uint64_t( core::T_sp );

  // THE NEXT TWO FUNCTIONS ARE HERE FOR BACKWARDS COMPATIBILITY
  // frgo, 2017-01-21

  inline int64_t clasp_to_int64(Integer_sp x)
  {
    return clasp_to_int64_t( x );
  }
  inline uint64_t clasp_to_uint64(Integer_sp x)
  {
    return clasp_to_uint64_t( x );
  }

  cl_intptr_t         clasp_to_cl_intptr_t( core::T_sp );
  mpz_class           clasp_to_mpz( core::T_sp );
  cl_index            clasp_to_size( core::T_sp );

  float               clasp_to_float( core::Number_sp );
  double              clasp_to_double( core::Number_sp );
  LongFloat           clasp_to_long_float( core::Number_sp );
  LongFloat           clasp_to_long_double( core::Number_sp );


}
#endif
