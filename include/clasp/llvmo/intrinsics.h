/*
    File: intrinsics.h
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
#ifndef llvmo_intrinsics_H
#define llvmo_intrinsics_H

extern "C" {

void cc_call_with_variable_bound(core::T_mv *result, core::T_O *symbol, core::T_O *value, core::T_O *thunk);
void cc_funwind_protect(core::T_mv *result, core::T_O *protected_fn, core::T_O *cleanup_fn);
void cc_catch(core::T_mv *result, core::T_O *tag, core::T_O *func);
void cc_throw(core::T_O *tag, core::T_O *resultP);

void cc_invoke_startup_functions();

// HELPER FUNCTIONS FOR MAKING CLASP LISP OBJECTS

core::T_sp mk_fixnum_short( short value );
core::T_sp mk_fixnum_ushort( unsigned short value );
core::T_sp mk_fixnum_int( int value );
core::T_sp mk_fixnum_uint( unsigned int value );
core::T_sp mk_fixnum_int8( int8_t value );
core::T_sp mk_fixnum_uint8( uint8_t value );
core::T_sp mk_fixnum_int16( int16_t value );
core::T_sp mk_fixnum_uint16( uint16_t value );
core::T_sp mk_fixnum_int32( int32_t value );
core::T_sp mk_fixnum_uint32( uint32_t value );
core::T_sp mk_integer_int64( int64_t value );
core::T_sp mk_integer_uint64( uint64_t value );
core::T_sp mk_integer_long( long value );
core::T_sp mk_integer_ulong( unsigned long value );
core::T_sp mk_integer_longlong( long long value );
core::T_sp mk_integer_ulonglong( unsigned long long value );
core::T_sp mk_double_float( double value );
core::T_sp mk_single_float( float value );
core::T_sp mk_long_double( long double value );
core::T_sp mk_time( time_t value );
core::T_sp mk_pointer( void * value );
core::T_sp mk_size( size_t value );
core::T_sp mk_ssize( ssize_t value );
core::T_sp mk_ptrdiff( ptrdiff_t value );
core::T_sp mk_char( char value );

// TRANSLATORS

gctools::Fixnum from_object_fixnum( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_fixnum( core::T_O* lisp_object_ptr );
core::T_sp to_object_fixnum( gctools::Fixnum value );
core::T_sp tr_to_object_fixnum( core::T_O* raw_ptr );

short from_object_short( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_short( core::T_O* lisp_object_ptr );
core::T_sp to_object_short( short value );
core::T_sp tr_to_object_short( core::T_O* raw_ptr );

int from_object_int( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_int( core::T_O* lisp_object_ptr );
core::T_sp to_object_int( int value );
core::T_sp tr_to_object_int( core::T_O* raw_ptr );

unsigned int from_object_unsigned_int( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_unsigned_int( core::T_O* lisp_object_ptr );
core::T_sp to_object_unsigned_int( unsigned int value );
core::T_sp tr_to_object_unsigned_int( core::T_O* raw_ptr );

int8_t from_object_int8( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_int8( core::T_O* lisp_object_ptr );
core::T_sp to_object_int8( int8_t value );
core::T_sp tr_to_object_int8( core::T_O* raw_ptr );

uint8_t from_object_uint8( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_uint8( core::T_O* lisp_object_ptr );
core::T_sp to_object_uint8( uint8_t value );
core::T_sp tr_to_object_uint8( core::T_O* raw_ptr );

int16_t from_object_int16( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_int16( core::T_O* lisp_object_ptr );
core::T_sp to_object_int16( int16_t value );
core::T_sp tr_to_object_int16( core::T_O* raw_ptr );

uint16_t from_object_uint16( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_uint16( core::T_O* lisp_object_ptr );
core::T_sp to_object_uint16( uint16_t value );
core::T_sp tr_to_object_uint16( core::T_O* raw_ptr );

int32_t from_object_int32( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_int32( core::T_O* lisp_object_ptr );
core::T_sp to_object_int32( int32_t value );
core::T_sp tr_to_object_int32( core::T_O* raw_ptr );

uint32_t from_object_uint32( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_uint32( core::T_O* lisp_object_ptr );
core::T_sp to_object_uint32( uint32_t value );
core::T_sp tr_to_object_uint32( core::T_O* raw_ptr );

int64_t from_object_int64( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_int64( core::T_O* lisp_object_ptr );
core::T_sp to_object_int64( int64_t value );
core::T_sp tr_to_object_int64( core::T_O* raw_ptr );

uint64_t from_object_uint64( core::T_O* lisp_object_ptr );
core::T_O* tr_from_object_uint64( core::T_O* lisp_object_ptr );
core::T_sp to_object_uint64( uint64_t value );
core::T_sp tr_to_object_uint64( core::T_O* raw_ptr );


};

namespace llvmo {

  void redirect_llvm_interface_addSymbol();

  void initialize_intrinsics();
  void initialize_link_intrinsics();

  typedef enum { noFunctionBoundToSymbol,
                 badKeywordArgument,
                 couldNotCoerceToClosure,
                 destinationMustBeActivationFrame,
                 invalidIndexForFunctionFrame,
                 unboundSymbolValue,
                 unboundSymbolFunction,
                 unboundSymbolSetfFunction
  } ErrorCode;

  extern void intrinsic_error(ErrorCode err, core::T_sp arg0 = _Nil<core::T_O>(), core::T_sp arg1 = _Nil<core::T_O>(), core::T_sp arg2 = _Nil<core::T_O>());

  void initialize_raw_translators( void );
}

#endif
