/*
    File: fli.h
*/

/*
Copyright (c) 2016, Christian E. Schafmeister
Copyright (c) 2016, Frank Gönninger

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
//  Augiust 2016 by Frank Gönninger, Gönninger B&T UG, Germany
//
// ===========================================================================

// --- IMPLEMEMTATION NOTES ---
//
// The complete FLI is comprised of the following files:
// .../src/core/fli.cc            - corresponding .cc file
// .../include/clasp/core/fli.h   - this file
// .../src/lisp/kernel/fli.lsp    - Lisp land macros and functions
//
// --- END OF IMPLEMEMTATION NOTES ---

#if !defined(__CLASP_CORE_FLI_H__)
#define __CLASP_CORE_FLI_H__ __FILE__" $Id$"

// ---------------------------------------------------------------------------
//    SYSTEM INCLUDES
// ---------------------------------------------------------------------------

// --- Standard C++ Includes ---
// NONE

// --- Platform-specific Includes ---
// NONE

// ---------------------------------------------------------------------------
//    APPLICATION INCLUDES
// ---------------------------------------------------------------------------

#include <clasp/core/clasp_ffi_package.fwd.h>
#include <clasp/core/array.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/pointer.h>

#if defined(__cplusplus)

// ---------------------------------------------------------------------------
//   NAMESPACE
// ---------------------------------------------------------------------------

namespace clasp_ffi {

// ---------------------------------------------------------------------------
//   LISP SYMBOLS
// ---------------------------------------------------------------------------

  // Keyword symbols designating ForeignData_O kinds
  SYMBOL_EXPORT_SC_(KeywordPkg,clasp_foreign_data_kind_dynamic_library);
  SYMBOL_EXPORT_SC_(KeywordPkg,clasp_foreign_data_kind_built_in_type);
  SYMBOL_EXPORT_SC_(KeywordPkg,clasp_foreign_data_kind_data);
  SYMBOL_EXPORT_SC_(KeywordPkg,clasp_foreign_data_kind_pointer);
  SYMBOL_EXPORT_SC_(KeywordPkg,clasp_foreign_data_kind_symbol_pointer);
  SYMBOL_EXPORT_SC_(KeywordPkg,clasp_foreign_data_kind_time);

  // The Foreign Type Spec Table, accessible from Lisp
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,STARforeign_type_spec_tableSTAR);

  // Endianness
  SYMBOL_EXPORT_SC_(KeywordPkg,big_endian);
  SYMBOL_EXPORT_SC_(KeywordPkg,little_endian);

  // Type Symbols
  SYMBOL_EXPORT_SC_(KeywordPkg,char);
  SYMBOL_EXPORT_SC_(KeywordPkg,void);

// ---------------------------------------------------------------------------
//   TYPEDEFS & CLASSES & METHODS & FUNCTIONS
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
  // FLI INITIALIZER

  void clasp_fli_initialization( void );

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
  // CLASS ForeignData_O

  SMART(ForeignData);

  class ForeignData_O : public core::Pointer_O {
    LISP_CLASS(clasp_ffi, Clasp_ffi_pkg,
               ForeignData_O, "ForeignData", core::Pointer_O);

  public:

    // CTOR & DTOR
    explicit ForeignData_O();
    virtual ~ForeignData_O(); // non-trivial

    // OVERLADED FUNCTIONS
    void *externalObject( void ) const;
    bool eql_( core::T_sp obj ) const;

    // SLOT ACCESS

    // -- SETTER & GETTER --
    const core::T_sp kind() { return m_kind; };
    void set_kind( core::T_sp kind ) { this->m_kind = kind; };

    const int ownership_flags( void ) { return m_ownership_flags; };
    const void *orig_data_ptr( void ) { return m_orig_data_ptr; };
    void *raw_data( void ) { return m_raw_data; };
    void *ptr( void ) { return m_raw_data; };
    
    CL_DEFMETHOD size_t foreign_data_size(void) { return m_size; };
    
    // -- TRANSFORMING ACCESS --
    template <class T>
      T data() { return reinterpret_cast<T>(this->raw_data()); };

    // LISP EXPOSED SETTER & GETTER METHODS
    CL_DEFMETHOD core::T_sp PERCENTkind();
    CL_DEFMETHOD core::Integer_sp PERCENTownership_flags();
    CL_DEFMETHOD core::Integer_sp PERCENTforeign_data_address();

    // MAKE AND CREATE
    static ForeignData_sp create(const cl_intptr_t address = 0);
    static ForeignData_sp create(void * p_address = nullptr);

    CL_DEFMETHOD void PERCENTfree_foreign_object();
    CL_DEFMETHOD void PERCENTfree_foreign_data();

    // POINTER ADDRESS MANIPULATION
    CL_DEFMETHOD ForeignData_sp PERCENTinc_pointer(core::Integer_sp offset);

    // OBJECT PRINTING
    string __repr__() const;
    bool null_pointer_p();

    // MENORY MGMT
    void allocate(core::T_sp kind, core::ForeignDataFlagEnum ownership_flags, size_t size);
    void free_();

  private:

    // SLOTS
    core::T_sp m_kind;
    core::ForeignDataFlagEnum m_ownership_flags;
    size_t m_size;

    void *m_orig_data_ptr;

  }; // ForeignData_O

  // ---------------------------------------------------------------------------
  // MAKE AND TEST FOREIGN DATA POINTER
  CL_DEFUN ForeignData_sp PERCENTallocate_foreign_object( core::T_sp kind );
  CL_DEFUN ForeignData_sp PERCENTallocate_foreign_data( core::Integer_sp size );
  ForeignData_sp allocate_foreign_data( uint64_t size );

  CL_DEFUN ForeignData_sp PERCENTmake_pointer( core::Integer_sp address );
  ForeignData_sp make_pointer( void * p_address );
  CL_DEFUN core::T_sp PERCENTpointerp( core::T_sp obj );

  CL_DEFUN ForeignData_sp PERCENTmake_nullpointer();
  CL_DEFUN core::T_sp PERCENTnull_pointer_p( core::T_sp obj );

  CL_DEFUN core::Integer_sp PERCENToffset_address_as_integer( core::T_sp address_or_foreign_data_ptr, core::Integer_sp offset );

  CL_DEFUN core::Pointer_sp PERCENTcore_pointer_from_foreign_data( ForeignData_sp fd_ptr ) ;
  CL_DEFUN core::T_sp PERCENTforeign_data_pointerp( core::T_sp obj );

  // ---------------------------------------------------------------------------
  // FOREIGN TYPE SIZE - Only needed in C++ land. For Lisp, there is
  // %foreign-type-size implemented in Lisp
  int64_t foreign_type_size( core::Symbol_sp atype );

  // ---------------------------------------------------------------------------
  // DYNAMIC LIBRARY HANDLING
  CL_DEFUN core::T_sp PERCENTdlopen( core::T_sp path_designator );
  CL_DEFUN core::T_sp PERCENTdlclose( ForeignData_sp handle );
  CL_DEFUN core::T_sp PERCENTdlsym( core::String_sp name );

}; // namespace clasp_ffi

// GC Policy Info for ForeignPataPtr_O instances
template <>
struct gctools::GCInfo<clasp_ffi::ForeignData_O>
{
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace clasp_ffi
{
  // FOREIGN MEMORY DIRECT ACCESS - MEM REF
  template <typename T>
    T mem_ref(cl_intptr_t address);

  // MEM-REF
  CL_DEFUN core::T_sp PERCENTmem_ref_short( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_short);

  CL_DEFUN core::T_sp PERCENTmem_ref_unsigned_short( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_unsigned_short);

  CL_DEFUN core::T_sp PERCENTmem_ref_int( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_int);

  CL_DEFUN core::T_sp PERCENTmem_ref_unsigned_int( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_unsigned_int);

  CL_DEFUN core::T_sp PERCENTmem_ref_int8( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_int8);

  CL_DEFUN core::T_sp PERCENTmem_ref_uint8( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_uint8);

  CL_DEFUN core::T_sp PERCENTmem_ref_int16( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_int16);

  CL_DEFUN core::T_sp PERCENTmem_ref_uint16( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_uint16);

  CL_DEFUN core::T_sp PERCENTmem_ref_int32( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_int32);

  CL_DEFUN core::T_sp PERCENTmem_ref_uint32( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_uint32);

  CL_DEFUN core::T_sp PERCENTmem_ref_int64( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_int64);

  CL_DEFUN core::T_sp PERCENTmem_ref_uint64( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_uint64);

  CL_DEFUN core::T_sp PERCENTmem_ref_long( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_long);

  CL_DEFUN core::T_sp PERCENTmem_ref_unsigned_long( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_unsigned_long);

  CL_DEFUN core::T_sp PERCENTmem_ref_long_long( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_long_long);

  CL_DEFUN core::T_sp PERCENTmem_ref_unsigned_long_long( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_unsigned_long_long);

  CL_DEFUN core::T_sp PERCENTmem_ref_double( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_double);

  CL_DEFUN core::T_sp PERCENTmem_ref_float( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_float);

  CL_DEFUN core::T_sp PERCENTmem_ref_long_double( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_long_doublee);

  CL_DEFUN core::T_sp PERCENTmem_ref_time( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_time);

  CL_DEFUN core::T_sp PERCENTmem_ref_pointer( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_pointer);

  CL_DEFUN core::T_sp PERCENTmem_ref_size( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_size);

  CL_DEFUN core::T_sp PERCENTmem_ref_ssize( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_ssize);

  CL_DEFUN core::T_sp PERCENTmem_ref_ptrdiff( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_ptrdiff);

  CL_DEFUN core::T_sp PERCENTmem_ref_char( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_char);

  CL_DEFUN core::T_sp PERCENTmem_ref_unsigned_char( core::Integer_sp address );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_ref_unsigned_char);

  // FOREIGN MEMORY DIRECT ACCESS - MEM SET
  template <typename T>
    T mem_set(cl_intptr_t address, T value);

  // HELPER FUNCTIONS FOR MAKING C++ VALUES FROM CLASP LISP OBJECTS
  ptrdiff_t clasp_to_ptrdiff( core::T_sp sp_lisp_value );
  char clasp_to_char( core::T_sp sp_lisp_value );
  unsigned char clasp_to_unsigned_char( core::T_sp sp_lisp_value );

  // FOREIGN MEMORY DIRECT ACCESS - MEM SET
  template<typename T>
    T mem_set( cl_intptr_t address, T value );

  // MEM-SET

  CL_DEFUN core::T_sp PERCENTmem_set_short( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_short);

  CL_DEFUN core::T_sp PERCENTmem_set_unsigned_short( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_unsigned_short);

  CL_DEFUN core::T_sp PERCENTmem_set_int( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_int);

  CL_DEFUN core::T_sp PERCENTmem_set_unsigned_int( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_unsigned_int);

  CL_DEFUN core::T_sp PERCENTmem_set_int8( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_int8);

  CL_DEFUN core::T_sp PERCENTmem_set_uint8( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_uint8);

  CL_DEFUN core::T_sp PERCENTmem_set_int16( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_int16);

  CL_DEFUN core::T_sp PERCENTmem_set_uint16( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_uint16);

  CL_DEFUN core::T_sp PERCENTmem_set_int32( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_int32);

  CL_DEFUN core::T_sp PERCENTmem_set_uint32( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_uint32);

  CL_DEFUN core::T_sp PERCENTmem_set_int64( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_int64);

  CL_DEFUN core::T_sp PERCENTmem_set_uint64( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_uint64);

  CL_DEFUN core::T_sp PERCENTmem_set_long( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_long);

  CL_DEFUN core::T_sp PERCENTmem_set_unsigned_long( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_unsigned_long);

  CL_DEFUN core::T_sp PERCENTmem_set_long_long( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_long_long);

  CL_DEFUN core::T_sp PERCENTmem_set_unsigned_long_long( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_unsigned_long_long);

  CL_DEFUN core::T_sp PERCENTmem_set_double( core::Integer_sp address, core::T_sp value ) ;
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_double);

  CL_DEFUN core::T_sp PERCENTmem_set_float( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_float);

  CL_DEFUN core::T_sp PERCENTmem_set_long_double( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_long_double);

  CL_DEFUN core::T_sp PERCENTmem_set_time( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_time);

  CL_DEFUN core::T_sp PERCENTmem_set_pointer( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_pointer);

  CL_DEFUN core::T_sp PERCENTmem_set_size( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_size);

  CL_DEFUN core::T_sp PERCENTmem_set_ssize( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_ssize);

  CL_DEFUN core::T_sp PERCENTmem_set_ptrdiff( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_ptrdiff);

  CL_DEFUN core::T_sp PERCENTmem_set_char( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_char);

  CL_DEFUN core::T_sp PERCENTmem_set_unsigned_char( core::Integer_sp address, core::T_sp value );
  SYMBOL_EXPORT_SC_(Clasp_ffi_pkg,PERCENTmem_set_unsigned_char);

  void * clasp_to_void_pointer( ForeignData_sp sp_lisp_value );

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
  // CLASS ForeignTypeSpec_O

  SMART(ForeignTypeSpec);

  class ForeignTypeSpec_O : public core::General_O
  {
    LISP_CLASS(clasp_ffi, Clasp_ffi_pkg,
               ForeignTypeSpec_O, "ForeignTypeSpec", core::General_O);

  public:

    // CTOR & DTOR
    explicit ForeignTypeSpec_O( core::Symbol_sp   lisp_symbol,
                                core::String_sp   lisp_name,
                                core::Integer_sp  size,
                                core::Fixnum_sp   alignment,
                                core::String_sp   cxx_name,
                                core::Function_sp llvm_type_symbol_fn,
                                core::String_sp   to_object_fn_name,
                                core::String_sp   from_object_fn_name,
                                ForeignData_sp    to_object_fn_ptr,
                                ForeignData_sp    from_object_fn_ptr )
      : m_lisp_symbol(lisp_symbol)
      , m_lisp_name(lisp_name)
      , m_size(size)
      , m_alignment(alignment)
      , m_cxx_name(cxx_name)
      , m_llvm_type_symbol_fn(llvm_type_symbol_fn)
      , m_to_object_fn_name(to_object_fn_name)
      , m_from_object_fn_name(from_object_fn_name)
      , m_to_object_fn_ptr(to_object_fn_ptr)
      , m_from_object_fn_ptr(from_object_fn_ptr)
    {};
    virtual ~ForeignTypeSpec_O();

    // OVERLADED FUNCTIONS
    bool eql_(ForeignTypeSpec_sp sp_obj) const;

    // OBJECT PRINTING
    string __repr__() const;

    // MAKE AND CREATE - LISP EXPOSED FUNCTIONS

    static ForeignTypeSpec_sp create( core::Symbol_sp   lisp_symbol,
                                      core::String_sp   lisp_name,
                                      core::Integer_sp  size,
                                      core::Fixnum_sp   alignment,
                                      core::String_sp   cxx_name,
                                      core::Function_sp llvm_type_symbol_fn,
                                      core::String_sp   to_object_fn_name,
                                      core::String_sp   from_object_fn_name,
                                      ForeignData_sp    to_object_fn_ptr,
                                      ForeignData_sp    from_object_fn_ptr );

    // SLOT ACCESS
    CL_DEFMETHOD core::Symbol_sp      PERCENTlisp_symbol() { return m_lisp_symbol; }; // e.g. :unsigned-int
    CL_DEFMETHOD core::String_sp      PERCENTlisp_name() { return m_lisp_name; }; // e.g. unisgned_int
    CL_DEFMETHOD core::Integer_sp     PERCENTsize() { return m_size; }; // size in bytes
    CL_DEFMETHOD core::Fixnum_sp      PERCENTalignment() { return m_alignment; }; // alignment in bytes
    CL_DEFMETHOD core::String_sp      PERCENTcxx_name() { return m_cxx_name; }; // e.g. "unsigned int"

    CL_DEFMETHOD core::String_sp      PERCENTto_object_fn_name() { return m_to_object_fn_name; };
    CL_DEFMETHOD core::String_sp      PERCENTfrom_object_fn_name() { return m_from_object_fn_name; };
    CL_DEFMETHOD ForeignData_sp      PERCENTto_object_fn_ptr() { return m_to_object_fn_ptr; };
    CL_DEFMETHOD ForeignData_sp      PERCENTfrom_object_fn_ptr() { return m_from_object_fn_ptr; };

    CL_DEFMETHOD core::Function_sp    PERCENTllvm_type_symbol_fn() { return m_llvm_type_symbol_fn; };

    CL_DEFMETHOD void                 PERCENTset_llvm_type_symbol_fn( core::Function_sp llvm_type_symbol_fn );

    // SLOTS
    core::Symbol_sp       m_lisp_symbol;
    core::String_sp       m_lisp_name;
    core::Integer_sp      m_size;
    core::Fixnum_sp       m_alignment;
    core::String_sp       m_cxx_name;
    core::Function_sp     m_llvm_type_symbol_fn;

    core::String_sp       m_to_object_fn_name;
    core::String_sp       m_from_object_fn_name;
    ForeignData_sp        m_to_object_fn_ptr;
    ForeignData_sp        m_from_object_fn_ptr;

  }; // ForeignTypeSpec_O

}; // namespace clasp_ffi

// GC Policy Info for ForeignTypeSpec_O instances
template <>
struct gctools::GCInfo<clasp_ffi::ForeignTypeSpec_O>
{
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};


// ---------------------------------------------------------------------------
//   END OF FILE
// ---------------------------------------------------------------------------

#endif // __cplusplus
#endif // __CLASP_CORE_FLI_H__
