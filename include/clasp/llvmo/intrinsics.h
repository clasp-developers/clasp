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

// If functions are defined with primitive-nounwind that means that they never unwind the stack and they can
//   be invoked from generated code using 'call'.  If they do unwind the stack, then any function that invokes
//   them with 'call' will fail to cleanup the stack and that will cause a failure.
//   These 'nounwind' intrinsic functions can be diagnosed by wrapping them in NO_UNWIND_BEGIN()/NO_UNWIND_END()
//   it wraps the body of the function in a try{...}catch(...){ERROR} block.
//   This is zero-cost at runtime other than increasing the size of unwind tables.

#ifdef DEBUG_NO_UNWIND
  #define NO_UNWIND_BEGIN() try {
  #define NO_UNWIND_END() } catch (...) {printf("%s:%d:%s  The stack is being unwound out of a function declared nounwind!!!\n", __FILE__, __LINE__, __FUNCTION__ );abort();}
#else
  #define NO_UNWIND_BEGIN()
  #define NO_UNWIND_END()
#endif

namespace llvmo {
  extern core::T_sp  global_arg0;
  extern core::T_sp  global_arg1;
  extern core::T_sp  global_arg2;
};
extern "C" {

void cc_call_with_variable_bound(core::T_mv *result, core::T_O *symbol, core::T_O *value, core::T_O *thunk);
void cc_funwind_protect(core::T_mv *result, core::T_O *protected_fn, core::T_O *cleanup_fn);
void cc_catch(core::T_mv *result, core::T_O *tag, core::T_O *func);
void cc_throw(core::T_O *tag, core::T_O *resultP);

void cc_invoke_startup_functions();

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// HELPER FUNCTIONS FOR MAKING CLASP LISP OBJECTS
//
// These functions are part of the Foreign Language Interface and are
// referenced from the FLI functions in fli.cc.

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

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// T R A N S L A T O R S
//
// These functions are part of the Foreign Language Interface and are
// referenced from the FLI functions in fli.cc.

gctools::Fixnum from_object_fixnum( core::T_O* obj );
core::T_O* to_object_fixnum( gctools::Fixnum x );

short from_object_short( core::T_O* obj );
core::T_O* to_object_short( short x );

int from_object_int( core::T_O* obj );
core::T_O* to_object_int( int x );

unsigned int from_object_unsigned_int( core::T_O* obj );
core::T_O* to_object_unsigned_int( unsigned int x );

long from_object_long( core::T_O* obj );
core::T_O* to_object_long( long x );

unsigned long from_object_unsigned_long( core::T_O* obj );
core::T_O* to_object_unsigned_long( unsigned long x );

int8_t from_object_int8( core::T_O* obj );
core::T_O* to_object_int8( int8_t x );

uint8_t from_object_uint8( core::T_O* obj );
core::T_O* to_object_uint8( uint8_t x );

int16_t from_object_int16( core::T_O* obj );
core::T_O* to_object_int16( int16_t x );

uint16_t from_object_uint16( core::T_O* obj );
core::T_O* to_object_uint16( uint16_t x );

int32_t from_object_int32( core::T_O* obj );
core::T_O* to_object_int32( int32_t x );

uint32_t from_object_uint32( core::T_O* obj );
core::T_O* to_object_uint32( uint32_t x );

int64_t from_object_int64( core::T_O* obj );
core::T_O* to_object_int64( int64_t x );

uint64_t from_object_uint64( core::T_O* obj );
core::T_O* to_object_uint64( uint64_t x );

long long from_object_long_long( core::T_O* obj );
core::T_O* to_object_long_long( long long x );

unsigned long long from_object_unsigned_long_long( core::T_O* obj );
core::T_O* to_object_unsigned_long_long( unsigned long long x );

size_t from_object_size( core::T_O* obj );
core::T_O* to_object_size( size_t x );

size_t from_object_ssize( core::T_O* obj );
core::T_O* to_object_ssize( ssize_t x );

ptrdiff_t from_object_ptrdiff( core::T_O* obj );
core::T_O* to_object_ptrdiff( ptrdiff_t x );

time_t from_object_time( core::T_O* obj );
core::T_O* to_object_time( time_t x );

char from_object_char( core::T_O* obj );
core::T_O* to_object_char( char x );

unsigned char from_object_unsigned_char( core::T_O* obj );
core::T_O* to_object_unsigned_char( unsigned char x );

float from_object_float( core::T_O* obj );
core::T_O* to_object_float( float x );

double from_object_double( core::T_O* obj );
core::T_O* to_object_double( double x );

long double from_object_long_double( core::T_O* obj );
core::T_O* to_object_long_double( long double x );

core::T_O* to_object_void( void );

void * from_object_pointer( core::T_O* obj );
core::T_O* to_object_pointer( void * x );

// END OF T R A N S L A T O R S
// ----------------------------------------------------------------------------

gctools::return_type cc_dispatch_effective_method(core::T_O* teffective_method, core::T_O* tgf, core::T_O* tgf_args_valist_s);
};

extern "C" {
extern int64_t cc_read_stamp(void* tagged_pointer);
[[noreturn]] void cc_error_too_few_arguments(size_t nargs, size_t minargs, core::FunctionDescription* functionDescription);
[[noreturn]] void cc_error_too_many_arguments(size_t nargs, size_t maxargs,  core::FunctionDescription* functionDescription);


}
namespace llvmo {

  void redirect_llvm_interface_addSymbol();

  void initialize_intrinsics();
  void initialize_link_intrinsics();

  typedef enum { noFunctionBoundToSymbol,
                 badKeywordArgument,
                 badCell,
                 couldNotCoerceToClosure,
                 destinationMustBeActivationFrame,
                 invalidIndexForFunctionFrame,
                 unboundSymbolValue,
                 unboundSymbolFunction,
                 unboundSymbolSetfFunction,
                 slot_reader_problem,
                 slot_writer_problem,
                 dummyErrorCode
  } ErrorCode;

  core::T_sp functionNameOrNilFromFunctionDescription(core::FunctionDescription* functionDescription);

  [[noreturn]]extern void intrinsic_error(ErrorCode err, core::T_sp arg0 = _Nil<core::T_O>(), core::T_sp arg1 = _Nil<core::T_O>(), core::T_sp arg2 = _Nil<core::T_O>());


  core::T_sp intrinsic_slot_unbound(core::T_sp info, core::T_sp instance);

  [[noreturn]] void not_function_designator_error(core::T_sp datum);
  void initialize_raw_translators( void );
}

#endif
