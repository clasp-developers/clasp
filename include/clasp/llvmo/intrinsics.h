#pragma once

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

#include <clasp/llvmo/intrinsics.fwd.h>

// If functions are defined with primitive-nounwind that means that they never unwind the stack and they can
//   be invoked from generated code using 'call'.  If they do unwind the stack, then any function that invokes
//   them with 'call' will fail to cleanup the stack and that will cause a failure.
//   These 'nounwind' intrinsic functions can be diagnosed by wrapping them in NO_UNWIND_BEGIN()/NO_UNWIND_END()
//   it wraps the body of the function in a try{...}catch(...){ERROR} block.
//   This is zero-cost at runtime other than increasing the size of unwind tables.

#ifdef DEBUG_NO_UNWIND
#define NO_UNWIND_BEGIN() try {
#define NO_UNWIND_END()                                                                                                            \
  }                                                                                                                                \
  catch (...) {                                                                                                                    \
    printf("%s:%d:%s  The stack is being unwound out of a function declared nounwind!!!\n", __FILE__, __LINE__, __FUNCTION__);     \
    abort();                                                                                                                       \
  }
#else
#define NO_UNWIND_BEGIN()
#define NO_UNWIND_END()
#endif

extern "C" {

typedef void LtvcReturn;

LtvcReturn ltvc_make_closurette(gctools::GCRootsInModule* holder, char tag, size_t index,
                                /*size_t function_index,*/ size_t entry_point_index);
LtvcReturn ltvc_make_closurette_no_function_info(gctools::GCRootsInModule* holder, char tag, size_t index, size_t function_index);
LtvcReturn ltvc_make_nil(gctools::GCRootsInModule* holder, char tag, size_t index);
LtvcReturn ltvc_make_t(gctools::GCRootsInModule* holder, char tag, size_t index);
LtvcReturn ltvc_make_ratio(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* num, core::T_O* denom);
LtvcReturn ltvc_make_complex(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* real, core::T_O* imag);
LtvcReturn ltvc_make_cons(gctools::GCRootsInModule* holder, char tag, size_t index);
LtvcReturn ltvc_rplaca(gctools::GCRootsInModule* holder, core::T_O* cons_t, core::T_O* car_t);
LtvcReturn ltvc_rplacd(gctools::GCRootsInModule* holder, core::T_O* cons_t, core::T_O* cdr_t);
LtvcReturn ltvc_make_list(gctools::GCRootsInModule* holder, char tag, size_t index, size_t len);
LtvcReturn ltvc_fill_list(gctools::GCRootsInModule* holder, core::T_O* list, size_t len, ...);
LtvcReturn ltvc_make_array(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* telement_type,
                           core::T_O* tdimensions);
LtvcReturn ltvc_make_hash_table(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* test_t);
void ltvc_setf_row_major_aref(gctools::GCRootsInModule* holder, core::T_O* array_t, size_t row_major_index, core::T_O* value_t);
void ltvc_setf_gethash(gctools::GCRootsInModule* holder, core::T_O* hash_table_t, core::T_O* key_index_t, core::T_O* value_index_t);
LtvcReturn ltvc_make_fixnum(gctools::GCRootsInModule* holder, char tag, size_t index, int64_t val);
LtvcReturn ltvc_make_bignum(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* bignum_string_t);
LtvcReturn ltvc_make_next_bignum(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* bignum);
LtvcReturn ltvc_make_bitvector(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* bitvector_string_t);
LtvcReturn ltvc_make_symbol(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* name_t, core::T_O* package_t);
LtvcReturn ltvc_make_character(gctools::GCRootsInModule* holder, char tag, size_t index, uintptr_t val);
LtvcReturn ltvc_make_base_string(gctools::GCRootsInModule* holder, char tag, size_t index, const char* str);
LtvcReturn ltvc_make_pathname(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* host_t, core::T_O* device_t,
                              core::T_O* directory_t, core::T_O* name_t, core::T_O* type_t, core::T_O* version_t);

LtvcReturn ltvc_make_function_description(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* sourcePathname_t,
                                          core::T_O* functionName_t, core::T_O* lambdaList_t, core::T_O* docstring_t,
                                          core::T_O* declares_t, size_t lineno, size_t column, size_t filepos);

LtvcReturn ltvc_make_local_entry_point(gctools::GCRootsInModule* holder, char tag, size_t index, size_t functionIndex,
                                       core::T_O* functionDescription_t);

LtvcReturn ltvc_make_global_entry_point(gctools::GCRootsInModule* holder, char tag, size_t index, size_t functionIndex,
                                        core::T_O* functionDescription_t, size_t localEntryPointIndex);

LtvcReturn ltvc_ensure_fcell(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* fname);
LtvcReturn ltvc_ensure_vcell(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* vname);

LtvcReturn ltvc_make_package(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* package_name_t);
LtvcReturn ltvc_make_random_state(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* random_state_string_t);
LtvcReturn ltvc_find_class(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* class_name_t);
LtvcReturn ltvc_make_binary16(gctools::GCRootsInModule* holder, char tag, size_t index, core::short_float_t f);
LtvcReturn ltvc_make_binary32(gctools::GCRootsInModule* holder, char tag, size_t index, core::single_float_t f);
LtvcReturn ltvc_make_binary64(gctools::GCRootsInModule* holder, char tag, size_t index, core::double_float_t f);
LtvcReturn ltvc_make_binary80(gctools::GCRootsInModule* holder, char tag, size_t index, core::long_float_t f);
LtvcReturn ltvc_make_binary128(gctools::GCRootsInModule* holder, char tag, size_t index, core::long_float_t f);
LtvcReturn ltvc_enclose(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* lambdaName, size_t function_index,
                        size_t function_info_index);
LtvcReturn ltvc_allocate_instance(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* klass);
LtvcReturn ltvc_set_mlf_creator_funcall(gctools::GCRootsInModule* holder, char tag, size_t index, size_t fptr_index,
                                        const char* name);
LtvcReturn ltvc_mlf_init_funcall(gctools::GCRootsInModule* holder, size_t fptr_index, const char* name);
LtvcReturn ltvc_set_ltv_funcall(gctools::GCRootsInModule* holder, char tag, size_t index, size_t fptr_index, const char* name);
LtvcReturn ltvc_toplevel_funcall(gctools::GCRootsInModule* holder, size_t fptr_index, const char* name);

void cc_invoke_startup_functions();
void cc_validate_tagged_pointer(core::T_O* ptr);

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// HELPER FUNCTIONS FOR MAKING CLASP LISP OBJECTS
//
// These functions are part of the Foreign Language Interface and are
// referenced from the FLI functions in fli.cc.

core::T_sp mk_fixnum_short(short value);
core::T_sp mk_fixnum_ushort(unsigned short value);
core::T_sp mk_fixnum_int(int value);
core::T_sp mk_fixnum_uint(unsigned int value);
core::T_sp mk_fixnum_int8(int8_t value);
core::T_sp mk_fixnum_uint8(uint8_t value);
core::T_sp mk_fixnum_int16(int16_t value);
core::T_sp mk_fixnum_uint16(uint16_t value);
core::T_sp mk_fixnum_int32(int32_t value);
core::T_sp mk_fixnum_uint32(uint32_t value);
core::T_sp mk_integer_int64(int64_t value);
core::T_sp mk_integer_uint64(uint64_t value);
core::T_sp mk_integer_long(long value);
core::T_sp mk_integer_ulong(unsigned long value);
core::T_sp mk_integer_longlong(long long value);
core::T_sp mk_integer_ulonglong(unsigned long long value);
core::T_sp mk_double_float(double value);
core::T_sp mk_single_float(float value);
core::T_sp mk_long_double(long double value);
core::T_sp mk_time(time_t value);
core::T_sp mk_pointer(void* value);
core::T_sp mk_size(size_t value);
core::T_sp mk_ssize(ssize_t value);
core::T_sp mk_ptrdiff(ptrdiff_t value);
core::T_sp mk_char(char value);

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// T R A N S L A T O R S
//
// These functions are part of the Foreign Language Interface and are
// referenced from the FLI functions in fli.cc.

gctools::Fixnum from_object_fixnum(core::T_O* obj);
core::T_O* to_object_fixnum(gctools::Fixnum x);

short from_object_short(core::T_O* obj);
core::T_O* to_object_short(short x);

int from_object_int(core::T_O* obj);
core::T_O* to_object_int(int x);

unsigned int from_object_unsigned_int(core::T_O* obj);
core::T_O* to_object_unsigned_int(unsigned int x);

long from_object_long(core::T_O* obj);
core::T_O* to_object_long(long x);

unsigned long from_object_unsigned_long(core::T_O* obj);
core::T_O* to_object_unsigned_long(unsigned long x);

int8_t from_object_int8(core::T_O* obj);
core::T_O* to_object_int8(int8_t x);

uint8_t from_object_uint8(core::T_O* obj);
core::T_O* to_object_uint8(uint8_t x);

int16_t from_object_int16(core::T_O* obj);
core::T_O* to_object_int16(int16_t x);

uint16_t from_object_uint16(core::T_O* obj);
core::T_O* to_object_uint16(uint16_t x);

int32_t from_object_int32(core::T_O* obj);
core::T_O* to_object_int32(int32_t x);

uint32_t from_object_uint32(core::T_O* obj);
core::T_O* to_object_uint32(uint32_t x);

int64_t from_object_int64(core::T_O* obj);
core::T_O* to_object_int64(int64_t x);

uint64_t from_object_uint64(core::T_O* obj);
core::T_O* to_object_uint64(uint64_t x);

long long from_object_long_long(core::T_O* obj);
core::T_O* to_object_long_long(long long x);

unsigned long long from_object_unsigned_long_long(core::T_O* obj);
core::T_O* to_object_unsigned_long_long(unsigned long long x);

size_t from_object_size(core::T_O* obj);
core::T_O* to_object_size(size_t x);

size_t from_object_ssize(core::T_O* obj);
core::T_O* to_object_ssize(ssize_t x);

ptrdiff_t from_object_ptrdiff(core::T_O* obj);
core::T_O* to_object_ptrdiff(ptrdiff_t x);

time_t from_object_time(core::T_O* obj);
core::T_O* to_object_time(time_t x);

char from_object_char(core::T_O* obj);
core::T_O* to_object_char(char x);

unsigned char from_object_unsigned_char(core::T_O* obj);
core::T_O* to_object_unsigned_char(unsigned char x);

float from_object_float(core::T_O* obj);
core::T_O* to_object_float(float x);

double from_object_double(core::T_O* obj);
core::T_O* to_object_double(double x);

long double from_object_long_double(core::T_O* obj);
core::T_O* to_object_long_double(long double x);

core::T_O* to_object_void(void);

void* from_object_pointer(core::T_O* obj);
core::T_O* to_object_pointer(void* x);

// END OF T R A N S L A T O R S
// ----------------------------------------------------------------------------
};

extern "C" {
extern gctools::Tagged cx_read_stamp(core::T_O* tagged_pointer);
[[noreturn]] void invalid_index_error(void* fixnum_index, void* fixnum_max, void* fixnum_axis);
core::T_O* makeCompiledFunction(core::T_O* tentrypoint, core::T_O* frameP);
}
namespace llvmo {

void redirect_llvm_interface_addSymbol();

void initialize_intrinsics();
void initialize_link_intrinsics();

typedef enum {
  noFunctionBoundToSymbol,
  badKeywordArgument,
  badCell,
  couldNotCoerceToClosure,
  destinationMustBeActivationFrame,
  invalidIndexForFunctionFrame,
  dummyErrorCode
} ErrorCode;

[[noreturn]] extern void intrinsic_error(ErrorCode err, core::T_sp arg0 = nil<core::T_O>(), core::T_sp arg1 = nil<core::T_O>(),
                                         core::T_sp arg2 = nil<core::T_O>());

[[noreturn]] void not_function_designator_error(core::T_sp datum);
void initialize_raw_translators(void);
} // namespace llvmo
