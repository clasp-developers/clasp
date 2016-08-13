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

#include <dlfcn.h>

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
#include <clasp/core/designators.h>
#include <clasp/core/wrappers.h> // last include is wrappers.h

// ---------------------------------------------------------------------------
//   I M P L E M E N T A T I O N
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//   LOCAL DEFINES
// ---------------------------------------------------------------------------

#undef CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE
#define CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(LISP_SYM_TYPE,CXX_TYPE,CXX_SYM_VAR,CXX_DESC_STR) \
  SYMBOL_EXPORT_SC_(KeywordPkg,LISP_SYM_TYPE); \
  register_foreign_type<CXX_TYPE>::doit(kw::CXX_SYM_VAR,CXX_DESC_STR);

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

// NONE

// ---------------------------------------------------------------------------
//   GLOBAL DECLAFATIONS AND SPECIAL VARS
// ---------------------------------------------------------------------------

static foreign_type_spec_table_t FOREIGN_TYPE_SPEC_TABLE;

Initializer global_initializer_for_foreign_types(register_foreign_types);

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

void register_foreign_type_spec(const Symbol_sp lispSymbol,
                                const std::string& lispName,
                                const size_t size,
                                const size_t alignment,
                                const std::string& cxxName) {

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

  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(char,char,_sym_char,":char");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(unsigned_char,unsigned char,_sym_unsigned_char,":unsigned-char");

  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(short,short,_sym_short,":short");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(unsigned_short,unsigned short,_sym_short,":short");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(int,int,_sym_int,":int");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(unsigned_int,unsigned int,_sym_unsigned_int,":unsigned int");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(long,long,_sym_long,":long");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(unsigned_long,unsigned long,_sym_long,":unsigned long");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(long_long,long long,_sym_long_long,":long long");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(unsigned_long_long,unsigned long long,_sym_long_long,":unsigned long long");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(unsigned_char,unsigned char,_sym_unsigned_char,":unsigned-char");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(uchar,unsigned char,_sym_char,":uchar");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(ushort,unsigned short,_sym_ushort,":ushort");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(uint,unsigned int,_sym_uint,":uint");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(ulong,unsigned long,_sym_ulong,":ulong");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(llong,long long,_sym_llong,":llong");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(ullong,unsigned long long,_sym_ullong,":ullong");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(int8,int8_t,_sym_int8,":int8");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(uint8,uint8_t,_sym_uint8,":uint8");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(int16,int16_t,_sym_int16,":int16");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(uint16,uint16_t,_sym_uint16,":uint16");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(int32,int32_t,_sym_int32,":int32");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(uint32,uint32_t,_sym_uint32,":uint32");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(int64,int64_t,_sym_int64,":int64");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(uint64,uint64_t,_sym_uint64,":uint64");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(double,double,_sym_double,":double");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(single_float,float,_sym_single_float,":float");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(long_double,long double,_sym_long_double,":long-double");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(time,time_t,_sym_time,":time");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(pointer,void *,_sym_pointer,":pointer");

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
     << " :kind " << this->m_kind
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
  this->m_size          = 0;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFMETHOD T_sp ForeignData_O::PERCENTkind() {
  return this->kind();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFMETHOD Integer_sp ForeignData_O::PERCENTownership_flags() {
  return Integer_O::create( (gctools::Fixnum) this->ownership_flags());
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFMETHOD Integer_sp ForeignData_O::PERCENTforeign_data_address() {
  return Integer_O::create(this->data<cl_intptr_t>());
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DOCSTRING("Allocate a chunk of memory for foreign-data");
CL_DEFUN ForeignData_sp ForeignData_O::PERCENTallocate_foreign_object(T_sp kind) {
  GC_ALLOCATE(ForeignData_O, obj);
  Cons_sp ckind = gc::As<Cons_sp>(kind);
  ASSERTF(oCar(ckind) == cl::_sym_array || oCar(ckind) == kw::_sym_array, BF("The first element of a foreign-data type must be ARRAY or :ARRAY"));
  ASSERTF(oCadr(ckind) == cl::_sym_UnsignedByte || oCadr(ckind) == kw::_sym_UnsignedByte, BF("The second element of a foreign-data type must be UNSIGNED-BYTE or :UNSIGNED-BYTE"));
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
  self->allocate( kw::_sym_clasp_foreign_data_kind_data, DeleteOnDtor, _size);
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
  self->m_kind = kw::_sym_clasp_foreign_data_kind_pointer;
  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_sp ForeignData_O::create(void * p_address) {
  GC_ALLOCATE(ForeignData_O, self);
  self->m_raw_data = p_address;
  self->m_kind = kw::_sym_clasp_foreign_data_kind_pointer;
  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFUN ForeignData_sp ForeignData_O::PERCENTmake_pointer(T_sp address) {
  ForeignData_sp ptr = ForeignData_O::create( clasp_to_cl_intptr_t( gc::As<Integer_sp>( address )) );
  ptr->m_kind = kw::_sym_clasp_foreign_data_kind_pointer;
  return ptr;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFUN ForeignData_sp ForeignData_O::PERCENTmake_nullpointer() {
  ForeignData_sp ptr = ForeignData_O::create( (const cl_intptr_t) 0 );
  ptr->m_kind = kw::_sym_clasp_foreign_data_kind_pointer;
  return ptr;
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
  return _Nil<T_O>();

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
  return _Nil<T_O>();

RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_SIZE:

  return result;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
template<class T>
T mem_ref( cl_intptr_t address ) {
  T *ptr = reinterpret_cast< T*>( address );
  return (*ptr);
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFUN T_sp core__PERCENTmem_ref( T_sp address_or_foreign_data_ptr,
                                    T_sp atype,
                                    Integer_sp offset ) {
  IMPLEMENT_ME();

#if 0
  cl_intptr_t address = reinterpret_cast<cl_intptr_t>( this->raw_data() );
  cl_intptr_t _offset = offset->as_cl_intptr_t_();

  address += _offset;

  auto iterator = FOREIGN_TYPE_SPEC_TABLE.begin();
  auto it_end = FOREIGN_TYPE_SPEC_TABLE.end();

  for (; iterator != it_end; iterator++) {
    if ( iterator->lispSymbol->eql_( atype ) ) {
      // TODO: Implement MEM-REF using mem_ref ...
      goto RETURN_FROM_CORE__PERCENT_MEM_REF;
    }
  }

  SIMPLE_ERROR(BF("No foreign type %s found!") % _rep_(atype));
  return _Nil<T_O>();

 RETURN_FROM_CORE__PERCENT_MEM_REF:
#endif
  return _Nil<T_O>();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFUN T_sp core__PERCENTmem_sef( T_sp address_or_foreign_data_ptr,
                                    T_sp atype,
                                    Integer_sp offset,
                                    T_sp value) {
  IMPLEMENT_ME();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DOCSTRING("%dlopen - Open a dynamic library and return the handle. Returns (values returned-value error-message(or nil if no error))");
CL_DEFUN T_sp core__PERCENTdlopen( T_sp path_designator ) {

  ForeignData_sp sp_handle;
  int n_mode = RTLD_NOW | RTLD_GLOBAL;

  Path_sp path = coerce::pathDesignator( path_designator );
  string str_path = path->asString();

  auto result = do_dlopen( str_path, n_mode );
  void * p_handle = std::get<0>( result );

  if( p_handle == nullptr ) {
    return ( Values(_Nil<T_O>(), Str_O::create( get<1>( result ))) );
  }

  sp_handle = ForeignData_O::create( p_handle );
  sp_handle->set_kind( kw::_sym_clasp_foreign_data_kind_dynamic_library );

  return ( Values( sp_handle, _Nil<T_O>()) );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFUN T_sp core__PERCENTdlclose( ForeignData_sp handle ) {

  auto result = do_dlclose( handle->raw_data() );
  int n_rc = std::get<0>( result );

  if( n_rc != 0 ) {
    return ( Values(_Nil<T_O>(), Str_O::create( get<1>( result ))) );
  }
  return ( Values( _lisp->_true(), _Nil<T_O>()) );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
CL_DEFUN T_sp core__PERCENTdlsym( Str_sp name ) {

  ForeignData_sp sp_sym;
  auto result = do_dlsym( RTLD_DEFAULT, name->get().c_str() );
  void *p_sym = std::get<0>( result );

  if( ! p_sym ) {
    return ( Values(_Nil<T_O>(), Str_O::create( get<1>( result ))) );
  }

  sp_sym = ForeignData_O::create( p_sym );
  sp_sym->set_kind( kw::_sym_clasp_foreign_data_kind_symbol_pointer );

  return ( Values( sp_sym, _Nil<T_O>()) );
}


}; // namespace core
