/*
    File: foreign_data.cc
*/

/*
Copyright (c) 2016, Christian E. Schafmeister
Copyright (c) 2016 and beyond, Frank Goenninger

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

// ===========================================================================
//
//  FOREIGN DATA - Foreign Data Interface for allocating, setting,
//                 getting and freeing foreign memory.
//
//  Augiust 2016 by Frank Goenninger, Gönninger B&T UG, Germany
//
// ===========================================================================

// --- IMPLEMEMTATION NOTES ---
//
// HOW-TO: Global symbol instantion with gc::Initializer's :
//
// 20:33:22	drmeister	Yeah, declare a function like:
//                               void initialize_symbol_foo() {...}
// 20:33:57	drmeister	Then declare a global: core::Initializer
//                              static_initializer_for_symbol_foo(initialize
//                              _symbol_foo);
// 20:34:15	drmeister	You have to #include <clasp/core/compiler.h>
// 20:34:53	drmeister	That will register initialize_symbol_foo as a
//                              callback to be invoked once clasp is at a
//                              reasonable level of initialization.
//
// --- END OF IMPLEMEMTATION NOTES ---

// ---------------------------------------------------------------------------
//   SYSTEM INCLUDES
// ---------------------------------------------------------------------------

#include <cstdint>
#include <map>
#include <type_traits>

// ---------------------------------------------------------------------------
//   LLVM INCLUDES
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//   CLASP INCLUDES
// ---------------------------------------------------------------------------

#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/compiler.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/foreign_data.h>
#include <clasp/core/numbers.h>
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h> // last include is wrappers.h

// ---------------------------------------------------------------------------
//   I M P L E M E N T A T I O N
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//   NAMESPACE
// ---------------------------------------------------------------------------

namespace core {

// ---------------------------------------------------------------------------
//   FORWARD DECLARATIONS
// ---------------------------------------------------------------------------

static void register_foreign_types(void);
static void register_foreign_type_spec(const Symbol_sp lispSymbol,
                                       const std::string &lispName,
                                       const size_t size,
                                       const size_t alignment,
                                       const std::string &cxxName);

// ---------------------------------------------------------------------------
//   TYPE DEFINTITIONS
// ---------------------------------------------------------------------------

using foreign_type_spec_t = struct {
  Symbol_sp lispSymbol;
  Str_sp lispName;
  Fixnum_sp size;
  Integer_sp alignment;
  string cxxName;
};

using foreign_type_spec_table_t = std::vector<core::foreign_type_spec_t>;

template <class T>
struct register_foreign_type {
  static void doit(Symbol_sp lispSymbol, const std::string &cxxName) {
    size_t size = sizeof(T);
    size_t alignment = std::alignment_of<T>::value;

    register_foreign_type_spec(lispSymbol,
                               lispSymbol->symbolNameAsString(),
                               size,
                               alignment,
                               cxxName);
  };
};

// ---------------------------------------------------------------------------
//   LISP EXPORTS
// ---------------------------------------------------------------------------

// Export missing symbols for foreign objects

SYMBOL_EXPORT_SC_(CorePkg, STARdefault_data_kind_foreign_data_kindSTAR);

// ---------------------------------------------------------------------------
//   GLOBAL DECLAFATIONS AND SPECIAL VARS
// ---------------------------------------------------------------------------

static foreign_type_spec_table_t FOREIGN_TYPE_SPEC_TABLE;

Initializer global_initializer_for_foreign_types(register_foreign_types);

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

void register_foreign_type_spec(const Symbol_sp lispSymbol,
                                const std::string &lispName,
                                const size_t size,
                                const size_t alignment,
                                const std::string &cxxName) {

  foreign_type_spec_t foreign_type_spec = {lispSymbol,
                                           Str_O::create( lispName ),
                                           make_fixnum(size),
                                           make_fixnum(alignment),
                                           cxxName};
  FOREIGN_TYPE_SPEC_TABLE.emplace_back(foreign_type_spec);
};

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

void register_foreign_types(void) {
  // CHAR
  register_foreign_type<char>::doit(INTERN_(kw, char),
                                    ":char");

  register_foreign_type<unsigned char>::doit(INTERN_(kw, unsigned_char),
                                             "unsigned char");

  // SHORT
  register_foreign_type<short>::doit(INTERN_(kw, short),
                                     ":short");

  register_foreign_type<unsigned short>::doit(INTERN_(kw, unsigned_short),
                                              "unsigned short");

  // INT
  register_foreign_type<int>::doit(INTERN_(kw, int),
                                   ":int");

  register_foreign_type<unsigned int>::doit(INTERN_(kw, unsigned_int),
                                            ":unsigned-int");

  // LONG
  register_foreign_type<long>::doit(INTERN_(kw, long),
                                    ":long");

  register_foreign_type<unsigned long>::doit(INTERN_(kw, unsigned_long),
                                             ":unsigned-long");

  // LONG LONG
  register_foreign_type<long long>::doit(INTERN_(kw, long_long),
                                         ":long-long");

  register_foreign_type<unsigned long long>::doit(INTERN_(kw,
                                                          unsigned_long_long),
                                                  ":unsigned-long-long");

  // UCHAR
  register_foreign_type<unsigned char>::doit(INTERN_(kw, uchar),
                                             ":uchar");

  // USHORT
  register_foreign_type<unsigned short>::doit(INTERN_(kw, ushort),
                                              ":ushort");

  // UINT
  register_foreign_type<unsigned int>::doit(INTERN_(kw, uint),
                                            ":uint");

  // ULONG
  register_foreign_type<unsigned long>::doit(INTERN_(kw, ulong),
                                             ":ulong");

  // LLONG
  register_foreign_type<long long>::doit(INTERN_(kw, llong),
                                         ":llong");

  // ULLONG
  register_foreign_type<unsigned long long>::doit(INTERN_(kw, ullong),
                                                  ":ullong");

  // INT8
  register_foreign_type<int8_t>::doit(INTERN_(kw, int8),
                                      ":int8");

  register_foreign_type<uint8_t>::doit(INTERN_(kw, uint8),
                                       ":uint8");

  // INT16
  register_foreign_type<int16_t>::doit(INTERN_(kw, int16),
                                       ":int16");

  register_foreign_type<uint16_t>::doit(INTERN_(kw, uint16),
                                        ":uint16");

  // INT32
  register_foreign_type<int32_t>::doit(INTERN_(kw, int32),
                                       ":int32");

  register_foreign_type<uint32_t>::doit(INTERN_(kw, uint32),
                                        ":uint32");

  // INT64
  register_foreign_type<int64_t>::doit(INTERN_(kw, int64),
                                       ":int64");

  register_foreign_type<uint64_t>::doit(INTERN_(kw, uint64),
                                        ":uint64");

  // DOUBLE
  register_foreign_type<double>::doit(INTERN_(kw, double),
                                      ":double");

  // FLOAT
  register_foreign_type<float>::doit(INTERN_(kw, float),
                                     ":float");

  // LONG DOUBLE
  register_foreign_type<long double>::doit(INTERN_(kw, long_double),
                                           ":long-double");

  // POINTER
  register_foreign_type<void *>::doit(INTERN_(kw, pointer), ":pointer");
};

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_O::ForeignData_O() : m_kind(_Nil<T_O>()),
                                 m_ownership_flags(ForeignDataFlagEnum::None),
                                 m_size(0),
                                 m_orig_data_ptr(nullptr),
                                 m_raw_data(nullptr) {
  // NOTHIHG TO DO
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_O::~ForeignData_O() {
  if ( this->m_ownership_flags & ForeignDataFlagEnum::DeleteOnDtor ) {
    this->free();
  }
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
inline string ForeignData_O::__repr__() const {

  stringstream ss;

  ss << "#<"
     << this->_instanceClass()->classNameAsString()
     << " @ " << (BF("%p") % this)
     << " :size " << this->m_size
     << " :data-ptr "
     << (BF("%p") % this->m_raw_data)
     << " :orig-ptr "
     << (BF("%p") % this->m_orig_data_ptr)
     << ">";

  return ss.str();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
void * ForeignData_O::externalObject() const {
  return this->m_raw_data;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
bool ForeignData_O::eql_(T_sp obj) const {
  if (core__external_object_p(obj)) {
    return (gc::As<ExternalObject_sp>(obj)->externalObject() == this->externalObject());
  }
  return false;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
void ForeignData_O::allocate(T_sp kind, ForeignDataFlagEnum ownership_flags, size_t size) {
  this->m_kind = kind;
  this->m_ownership_flags = ownership_flags;
  this->m_size = size;
  this->m_raw_data = (void *)clasp_alloc_atomic(size);
  this->m_orig_data_ptr = this->m_raw_data;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
void ForeignData_O::free() {
  clasp_dealloc( (char *)this->m_orig_data_ptr );
  this->m_orig_data_ptr = nullptr;
  this->m_raw_data      = nullptr;
  this->m_size          = 0;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DOCSTRING("Allocate a chunk of memory for foreign-data");
CL_DEFUN ForeignData_sp ForeignData_O::PERCENTallocate_foreign_object(T_sp kind) {
  GC_ALLOCATE(ForeignData_O, obj);
  Cons_sp ckind = gc::As<Cons_sp>(kind);
  ASSERTF(oCar(ckind) == cl::_sym_array || oCar(ckind) == kw::_sym_array, BF("The first element of a foreign-data type must be ARRAY or :ARRAY"));
  ASSERTF(oCadr(ckind) == cl::_sym_UnsignedByte || oCadr(ckind) == kw::_sym_UnsignedByte, BF("The first element of a foreign-data type must be UNSIGNED-BYTE or :UNSIGNED-BYTE"));
  size_t size = unbox_fixnum(gc::As<Fixnum_sp>(oCaddr(ckind)));
  obj->allocate(kind, DeleteOnDtor, size);
  return obj;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFMETHOD void ForeignData_O::PERCENTfree_foreign_object() {
  this->free();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFUN ForeignData_sp ForeignData_O::PERCENTallocate_foreign_data(Integer_sp size) {
  size_t _size = unbox_fixnum( size );
  GC_ALLOCATE(ForeignData_O, self);
  self->allocate( _Nil<T_O>(), DeleteOnDtor, _size);
  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFMETHOD void ForeignData_O::PERCENTfree_foreign_data() {
  this->free();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_sp ForeignData_O::create(const cl_intptr_t address) {
  GC_ALLOCATE(ForeignData_O, self);
  self->m_raw_data = reinterpret_cast<void *>( address );
  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFUN ForeignData_sp ForeignData_O::PERCENTmake_pointer(T_sp address) {
  return ForeignData_O::create( clasp_to_cl_intptr_t( gc::As<Integer_sp>( address )) );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFUN ForeignData_sp ForeignData_O::PERCENTmake_nullpointer() {
  return ForeignData_O::create( (const cl_intptr_t) 0 );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFMETHOD ForeignData_sp ForeignData_O::PERCENTinc_pointer( Integer_sp offset) {
  cl_intptr_t new_address = 0;
  cl_intptr_t raw_data_address = reinterpret_cast<cl_intptr_t>( this->raw_data() );
  cl_intptr_t offset_ = clasp_to_cl_intptr_t( offset );

  new_address = raw_data_address + offset_;
  this->m_raw_data  = reinterpret_cast<void *>( new_address );

  return this->asSmartPtr();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DOCSTRING("Return the memory alignment of a foreign type");
CL_DEFUN core::Fixnum_sp core__PERCENTforeign_type_alignment(core::Symbol_sp atype) {

  Fixnum_sp result = nullptr;

  auto iterator = FOREIGN_TYPE_SPEC_TABLE.begin();
  auto it_end = FOREIGN_TYPE_SPEC_TABLE.end();

  for ( ; iterator != it_end; iterator++ ) {
    if ( iterator->lispSymbol->eql_( atype ) ) {
      result = iterator->alignment;
      goto RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_ALIGNMENT;
    }
  }

  SIMPLE_ERROR(BF("No foreign type alignment available for %s") % _rep_(atype));
  return nullptr;

RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_ALIGNMENT:

  return result;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DOCSTRING("Return the size of a foreign type");
CL_DEFUN core::Fixnum_sp core__PERCENTforeign_type_size(core::Symbol_sp atype) {
  Fixnum_sp result = nullptr;

  auto iterator = FOREIGN_TYPE_SPEC_TABLE.begin();
  auto it_end = FOREIGN_TYPE_SPEC_TABLE.end();

  for (; iterator != it_end; iterator++) {
    if ( iterator->lispSymbol->eql_( atype ) ) {
      result = iterator->size;
      goto RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_SIZE;
    }
  }

  SIMPLE_ERROR(BF("No foreign type size available for %s") % _rep_(atype));
  return nullptr;

RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_SIZE:

  return result;
}


}; // namespace core
