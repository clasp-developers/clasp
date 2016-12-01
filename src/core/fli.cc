/*
    File: fli.cc
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
//  FLI - Foreign Language Interface for allocating, setting,
//        getting and freeing foreign memory and calling foreign
//        functions.
//
//  Augiust 2016 by Frank Goenninger, Gönninger B&T UG, Germany
//
// ===========================================================================

// --- IMPLEMEMTATION NOTES ---
//
// The complete FLI is comprised of the following files:
// .../src/core/fli.cc            - this file
// .../include/clasp/core/fli.h   - corresponding .h file
// .../src/lisp/kernel/fli.lsp    - Lisp land macros and functions
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

//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/compiler.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/fli.h>
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
#define CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(TST,IDX,LISP_SYM_TYPE,CXX_TYPE,CXX_SYM_VAR,CXX_DESC_STR) \
  SYMBOL_EXPORT_SC_(KeywordPkg,LISP_SYM_TYPE); \
  register_foreign_type<CXX_TYPE>::doit(TST,IDX,kw::CXX_SYM_VAR,CXX_DESC_STR);

// ---------------------------------------------------------------------------
//   NAMESPACE
// ---------------------------------------------------------------------------

namespace clasp_ffi {

// ---------------------------------------------------------------------------
//   FORWARD DECLARATIONS
// ---------------------------------------------------------------------------

void clasp_ffi_initialization(void);
static void register_foreign_type_spec(core::VectorObjects_sp sp_tst,
                                       uint32_t n_index,
                                       const core::Symbol_sp lispSymbol,
                                       const std::string &lispName,
                                       const size_t size,
                                       const size_t alignment,
                                       const std::string &cxxName);

// ---------------------------------------------------------------------------
//   TYPE DEFINTITIONS
// ---------------------------------------------------------------------------

template <class T>
struct register_foreign_type {
  static void doit( core::VectorObjects_sp sp_tst,
                    uint32_t n_index,
                    core::Symbol_sp lispSymbol,
                    const std::string &cxxName) {
    size_t size = sizeof(T);
    size_t alignment = std::alignment_of<T>::value;

    register_foreign_type_spec(sp_tst,
                               n_index,
                               lispSymbol,
                               lispSymbol->symbolNameAsString(),
                               size,
                               alignment,
                               cxxName);
  };
};

// ---------------------------------------------------------------------------
//   GLOBAL DECLARATIONS AND SPECIAL VARS
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

void register_foreign_type_spec(core::VectorObjects_sp sp_tst,
                                uint32_t n_index,
                                const core::Symbol_sp lisp_symbol,
                                const std::string& lisp_name,
                                const size_t size,
                                const size_t alignment,
                                const std::string& cxx_name) {

  ForeignTypeSpec_sp sp_fts =
    ForeignTypeSpec_O::create( lisp_symbol,
                               core::Str_O::create( lisp_name ),
                               core::make_fixnum(size),
                               core::make_fixnum(alignment),
                               core::Str_O::create( cxx_name ));
  //sp_tst->vectorPushExtend( sp_fts->asSmartPtr() );
  sp_tst->setf_elt( n_index, sp_fts->asSmartPtr() );
};

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

CL_INITIALIZER void clasp_fli_initialization(void) {

  uint32_t n_index = 0;

  // STEP 1 : REGISTER FOREIGN TYPES

  core::VectorObjects_sp sp_tst = core::VectorObjects_O::make(_Nil<core::T_O>(), _Nil<core::T_O>(), 64, true, cl::_sym_T_O);

  //  - 1.1 : CREATE FOREIGN TYPE SPECS

  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,char,char,_sym_char,":char");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,unsigned_char,unsigned char,_sym_unsigned_char,":unsigned-char");

  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,short,short,_sym_short,":short");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,unsigned_short,unsigned short,_sym_short,":short");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,int,int,_sym_int,":int");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,unsigned_int,unsigned int,_sym_unsigned_int,":unsigned int");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,long,long,_sym_long,":long");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,unsigned_long,unsigned long,_sym_long,":unsigned long");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,long_long,long long,_sym_long_long,":long long");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,unsigned_long_long,unsigned long long,_sym_long_long,":unsigned long long");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,unsigned_char,unsigned char,_sym_unsigned_char,":unsigned-char");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,uchar,unsigned char,_sym_char,":uchar");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,ushort,unsigned short,_sym_ushort,":ushort");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,uint,unsigned int,_sym_uint,":uint");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,ulong,unsigned long,_sym_ulong,":ulong");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,llong,long long,_sym_llong,":llong");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,ullong,unsigned long long,_sym_ullong,":ullong");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,int8,int8_t,_sym_int8,":int8");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,uint8,uint8_t,_sym_uint8,":uint8");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,int16,int16_t,_sym_int16,":int16");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,uint16,uint16_t,_sym_uint16,":uint16");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,int32,int32_t,_sym_int32,":int32");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,uint32,uint32_t,_sym_uint32,":uint32");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,int64,int64_t,_sym_int64,":int64");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,uint64,uint64_t,_sym_uint64,":uint64");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,double,double,_sym_double,":double");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,single_float,float,_sym_single_float,":float");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,long_double,long double,_sym_long_double,":long-double");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,time,time_t,_sym_time,":time");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,pointer,void *,_sym_pointer,":pointer");

  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,size,size_t,_sym_size,":size");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,ssize,ssize_t,_sym_ssize,":ssize");
  CLASP_CORE_FLI_REGISTER_FOREIGN_TYPE(sp_tst,n_index++,ptrdiff,ptrdiff_t,_sym_ptrdiff,":ptrdiff");

  //  - 1.2 : ASSIGN FOREGN TYPE SPEC TABLE TO GLOBAL SYMBOL

  _sym_STARforeign_type_spec_tableSTAR->defparameter( sp_tst );

  // CLASP FFI INITIALIZATION DONE
};

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

//  F O R E I G N   D A T A

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_O::ForeignData_O() : m_kind(_Nil<T_O>()),
                                 m_ownership_flags(core::ForeignDataFlagEnum::None),
                                 m_size(0),
                                 m_orig_data_ptr(nullptr),
                                 m_raw_data(nullptr) {
  // NOTHIHG TO DO
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_O::~ForeignData_O() {
  if ( this->m_ownership_flags & core::ForeignDataFlagEnum::DeleteOnDtor ) {
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
bool ForeignData_O::eql_(core::T_sp obj) const {
  if (core__external_object_p(obj)) {
    return (gc::As<core::ExternalObject_sp>(obj)->externalObject() == this->externalObject());
  }
  return false;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
void ForeignData_O::allocate(core::T_sp kind, core::ForeignDataFlagEnum ownership_flags, size_t size) {
  this->m_kind = kind;
  this->m_ownership_flags = ownership_flags;
  this->m_size = size;
  this->m_raw_data = (void *)core::clasp_alloc_atomic(size);
  this->m_orig_data_ptr = this->m_raw_data;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
void ForeignData_O::free() {
  core::clasp_dealloc( (char *)this->m_orig_data_ptr );
  this->m_orig_data_ptr = nullptr;
  this->m_size          = 0;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool ForeignData_O::null_pointer_p() {
  return ( this->raw_data() == nullptr );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::T_sp ForeignData_O::PERCENTkind() {
  return this->kind();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::Integer_sp ForeignData_O::PERCENTownership_flags() {
  return core::Integer_O::create( (gctools::Fixnum) this->ownership_flags());
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::Integer_sp ForeignData_O::PERCENTforeign_data_address() {
  return core::Integer_O::create(this->data<cl_intptr_t>());
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_sp PERCENTallocate_foreign_object(core::T_sp kind) {
  GC_ALLOCATE(ForeignData_O, obj);
  core::Cons_sp ckind = gc::As<core::Cons_sp>(kind);
  if ( ! (oCar(ckind)==cl::_sym_array || oCar(ckind)==kw::_sym_array ) ) {
    SIMPLE_ERROR(BF("The first element of a foreign-data type must be ARRAY or :ARRAY"));
  }
  if (!(oCadr(ckind) == cl::_sym_UnsignedByte || oCadr(ckind) == kw::_sym_UnsignedByte)) {
    SIMPLE_ERROR(BF("The second element of a foreign-data type must be UNSIGNED-BYTE or :UNSIGNED-BYTE"));
  }
  size_t size = unbox_fixnum(gc::As<core::Fixnum_sp>(oCaddr(ckind)));
  obj->allocate(kind, core::DeleteOnDtor, size);
  return obj;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
void ForeignData_O::PERCENTfree_foreign_object() {
  this->free();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_sp PERCENTallocate_foreign_data(core::Integer_sp size) {
  size_t _size = unbox_fixnum( size );
  GC_ALLOCATE(ForeignData_O, self);
  self->allocate( kw::_sym_clasp_foreign_data_kind_data, core::DeleteOnDtor, _size);
  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
void ForeignData_O::PERCENTfree_foreign_data() {
  this->free();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_sp ForeignData_O::create(const cl_intptr_t address) {
  GC_ALLOCATE(ForeignData_O, self);
  self->m_raw_data = reinterpret_cast<void *>( address );
  self->set_kind( kw::_sym_clasp_foreign_data_kind_pointer );
  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_sp ForeignData_O::create(void * p_address) {
  GC_ALLOCATE(ForeignData_O, self);
  self->m_raw_data = p_address;
  self->set_kind( kw::_sym_clasp_foreign_data_kind_pointer );
  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_sp PERCENTmake_pointer(core::T_sp address) {
  ForeignData_sp ptr = ForeignData_O::create( clasp_to_cl_intptr_t( gc::As<core::Integer_sp>( address )) );
  ptr->set_kind( kw::_sym_clasp_foreign_data_kind_pointer );
  return ptr;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_sp PERCENTmake_nullpointer() {
  ForeignData_sp ptr = ForeignData_O::create( (const cl_intptr_t) 0 );
  ptr->set_kind( kw::_sym_clasp_foreign_data_kind_pointer );
  return ptr;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::T_sp PERCENTpointerp( core::T_sp obj ) {

  ForeignData_sp sp_foreign_data = obj.asOrNull<ForeignData_O>();

  if( sp_foreign_data.nilp() ) {
    return _Nil<core::T_O>();
  }
  else {
    return _lisp->_true();
  }
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::T_sp PERCENTnull_pointer_p( core::T_sp obj ) {

  ForeignData_sp sp_foreign_data = obj.asOrNull<ForeignData_O>();

  if( sp_foreign_data.nilp() ) {
    return _Nil<core::T_O>();
  }
  else {
    if( sp_foreign_data->null_pointer_p() ) {
      return _lisp->_true();
    }
    else {
      return _Nil<core::T_O>();
    }
  }
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignData_sp ForeignData_O::PERCENTinc_pointer( core::Integer_sp offset) {
  cl_intptr_t new_address = 0;
  cl_intptr_t raw_data_address = reinterpret_cast<cl_intptr_t>( this->raw_data() );
  cl_intptr_t offset_ = clasp_to_cl_intptr_t( offset );

  new_address = raw_data_address + offset_;
  this->m_raw_data  = reinterpret_cast<void *>( new_address );

  return this->asSmartPtr();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::Fixnum_sp PERCENTforeign_type_alignment(core::Symbol_sp atype) {

  core::Fixnum_sp result = nullptr;

  core::VectorObjects_sp sp_tst = _sym_STARforeign_type_spec_tableSTAR->symbolValue();
  auto iterator = sp_tst->begin();
  auto it_end = sp_tst->end();

  for (; iterator != it_end; iterator++) {
    ForeignTypeSpec_sp sp_fts = iterator->asOrNull<ForeignTypeSpec_O>();
    if ( sp_fts.notnilp() ) {
      if ( sp_fts->PERCENTlisp_symbol()->eql_( atype ) ) {
        result = sp_fts->PERCENTalignment();
        goto RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_ALIGNMENT;
      }
    }
  }

  SIMPLE_ERROR(BF("No foreign type alignment available for %s") % _rep_(atype));
  return _Nil<core::T_O>();

RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_ALIGNMENT:

  return result;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::Fixnum_sp PERCENTforeign_type_size(core::Symbol_sp atype) {

  core::Fixnum_sp result = nullptr;

  core::VectorObjects_sp sp_tst = _sym_STARforeign_type_spec_tableSTAR->symbolValue();
  auto iterator = sp_tst->begin();
  auto it_end = sp_tst->end();

  for (; iterator != it_end; iterator++) {
    ForeignTypeSpec_sp sp_fts = iterator->asOrNull<ForeignTypeSpec_O>();
    if ( sp_fts.notnilp() ) {
      if ( sp_fts->PERCENTlisp_symbol()->eql_( atype ) ) {
        result = sp_fts->PERCENTsize();
        goto RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_SIZE;
      }
    }
  }

  SIMPLE_ERROR(BF("No foreign type size available for %s") % _rep_(atype));
  return _Nil<core::T_O>();

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
core::T_sp PERCENTmem_ref( core::T_sp address_or_foreign_data_ptr,
                           core::T_sp atype,
                           core::Integer_sp offset ) {
  IMPLEMENT_ME();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
void PERCENTmem_set( core::T_sp address_or_foreign_data_ptr,
                     core::T_sp atype,
                     core::Integer_sp offset,
                     core::T_sp value) {
  IMPLEMENT_ME();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::T_sp PERCENTdlopen( core::T_sp path_designator ) {

  ForeignData_sp sp_handle;
  int n_mode = RTLD_NOW | RTLD_GLOBAL;

  core::Path_sp path = core::coerce::pathDesignator( path_designator );
  string str_path = path->asString();

  auto result = core::do_dlopen( str_path, n_mode );
  void * p_handle = std::get<0>( result );

  if( p_handle == nullptr ) {
    return ( Values(_Nil<core::T_O>(), core::Str_O::create( get<1>( result ))) );
  }

  sp_handle = ForeignData_O::create( p_handle );
  sp_handle->set_kind( kw::_sym_clasp_foreign_data_kind_dynamic_library );

  return ( Values( sp_handle, _Nil<core::T_O>()) );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::T_sp PERCENTdlclose( ForeignData_sp handle ) {

  auto result = core::do_dlclose( handle->raw_data() );
  int n_rc = std::get<0>( result );

  if( n_rc != 0 ) {
    return ( Values(_Nil<core::T_O>(), core::Str_O::create( get<1>( result ))) );
  }
  return ( Values( _lisp->_true(), _Nil<core::T_O>()) );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::T_sp PERCENTdlsym( core::Str_sp name ) {

  ForeignData_sp sp_sym;
  auto result = core::do_dlsym( RTLD_DEFAULT, name->get().c_str() );
  void *p_sym = std::get<0>( result );

  if( ! p_sym ) {
    return ( Values(_Nil<core::T_O>(), core::Str_O::create( get<1>( result ))) );
  }

  sp_sym = ForeignData_O::create( p_sym );
  sp_sym->set_kind( kw::_sym_clasp_foreign_data_kind_symbol_pointer );

  return ( Values( sp_sym, _Nil<core::T_O>()) );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

//  F O R E I G N   T Y P E   S P E C

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignTypeSpec_O::ForeignTypeSpec_O() : m_lisp_symbol( _Nil<T_O>() ),
                                         m_lisp_name( _Nil<T_O>() ),
                                         m_size( (gc::Fixnum) 0 ),
                                         m_alignment( (gc::Fixnum) 0 ),
                                         m_cxx_name( _Nil<T_O>() ) {
  // NOTHIHG TO DO
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignTypeSpec_O::~ForeignTypeSpec_O() {
  // TODO: Do we need to delete ourselves from the foregn type spec table?


}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
inline string ForeignTypeSpec_O::__repr__() const {

  stringstream ss;

  ss << "#<"
     << this->_instanceClass()->classNameAsString()
     << " @ " << (BF("%p") % this)
     << " :lisp-symbol " << this->m_lisp_symbol
     << " :lisp-name "   << this->m_lisp_name
     << " :size "        << this->m_size
     << " :alignment "   << this->m_alignment
     << " :cxx-name "    << this->m_cxx_name
     << ">";

  return ss.str();
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
ForeignTypeSpec_sp ForeignTypeSpec_O::create( core::Symbol_sp   lisp_symbol,
                                              core::Str_sp      lisp_name,
                                              core::Integer_sp  size,
                                              core::Fixnum_sp   alignment,
                                              core::Str_sp      cxx_name ) {
  GC_ALLOCATE(ForeignTypeSpec_O, self);

  self->m_lisp_symbol = lisp_symbol;
  self->m_lisp_name   = lisp_name;
  self->m_size        = size;
  self->m_alignment   = alignment;
  self->m_cxx_name    = cxx_name;

  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
bool ForeignTypeSpec_O::eql_(ForeignTypeSpec_sp sp_obj) const {

  if( this->m_lisp_symbol == sp_obj->m_lisp_symbol )
    return true;
  else
    return false;
}

}; // namespace clasp_ffi
