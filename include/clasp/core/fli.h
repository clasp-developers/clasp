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
//  Augiust 2016 by Frank Goenninger, Gönninger B&T UG, Germany
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

#include <clasp/core/foundation.h>
#include <clasp/core/clasp_ffi_package.fwd.h>
#include <clasp/core/externalObject.h>

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

// ---------------------------------------------------------------------------
//   CLASSES & METHODS & FUNCTIONS
// ---------------------------------------------------------------------------

  // CLASS ForeignData_O

  SMART(ForeignData);

  class ForeignData_O : public core::ExternalObject_O {
    LISP_CLASS(clasp_ffi, Clasp_ffi_pkg,
               ForeignData_O, "ForeignData", core::ExternalObject_O);

  public:
  // CTOR & DTOR
    explicit ForeignData_O();
    virtual ~ForeignData_O(); // non-trivial

  // OVERLADED FUNCTIONS
    void *externalObject() const;
    bool eql_(core::T_sp obj) const;

  // SLOT ACCESS

  // -- SETTER & GETTER --
    const core::T_sp kind() { return m_kind; };
    void set_kind( core::T_sp kind ) { this->m_kind = kind; };

    const int ownership_flags() { return m_ownership_flags; };
    const void *orig_data_ptr() { return m_orig_data_ptr; };
    void *raw_data() { return m_raw_data; };

  // -- TRANSFORMING ACCESS --
    template <class T>
      T data() { return reinterpret_cast<T>(this->raw_data()); };

  // LISP EXPOSED SETTER & GETTER METHODS
    core::T_sp PERCENTkind();
    core::Integer_sp PERCENTownership_flags();
    core::Integer_sp PERCENTforeign_data_address();

  // MAKE AND CREATE - LISP EXPOSED FUNCTIONS

    static ForeignData_sp create(const cl_intptr_t address = 0);
    static ForeignData_sp create(void * p_address = nullptr);

    static ForeignData_sp PERCENTmake_pointer(core::T_sp address);
    static ForeignData_sp PERCENTmake_nullpointer();

    static ForeignData_sp PERCENTallocate_foreign_object(core::T_sp kind);
    void PERCENTfree_foreign_object();

    static ForeignData_sp PERCENTallocate_foreign_data(core::Integer_sp size);
    void PERCENTfree_foreign_data();

  // POINTER ADDRESS MANIPULATION
    ForeignData_sp PERCENTinc_pointer(core::Integer_sp offset);

  // OBJECT PRINTING
    string __repr__() const;
    bool null_pointer_p();

  private:
  // MENORY MGMT
    void allocate(core::T_sp kind, core::ForeignDataFlagEnum ownership_flags, size_t size);
    void free();

  // SLOTS
    core::T_sp m_kind;
    core::ForeignDataFlagEnum m_ownership_flags;
    size_t m_size;

    void *m_orig_data_ptr;
  // If we allocate memory then we save the ptr to the original address.
  // This enables changing the pointer of m_raw_data without loosing the ability
  // to free the originally allocated memory.

    void *m_raw_data;
  }; // ForeignData_O

// ---------------------------------------------------------------------------
  // POINTER UTILS
  core::T_sp core__PERCENTnull_pointer_p( core::T_sp obj );
  core::T_sp core__PERCENTpointerp( core::T_sp obj );


// ---------------------------------------------------------------------------
  // FOREIGN TYPE SIZE AND ALIGNMENT
  core::Fixnum_sp core__PERCENTforeign_type_alignment(core::Symbol_sp atype);
  core::Fixnum_sp core__PERCENTforeign_type_size(core::Symbol_sp atype);

  // LISP MEMORY ACEESS / MEMORY CONTENT CONVERSION
  core::T_sp core__PERCENTmem_ref( core::T_sp address_or_foreign_data_ptr,
                             core::T_sp type,
                             core::Integer_sp offset);
  void core__PERCENTmem_set( core::T_sp address_or_foreign_data_ptr,
                             core::T_sp type,
                             core::Integer_sp offset,
                             core::T_sp value);

// ---------------------------------------------------------------------------
  // FOREIGN MEMORY DIRECT ACCESS
  template <class T>
    T mem_ref(cl_intptr_t address);

// ---------------------------------------------------------------------------
  // DYNAMIC LIBRARY LOADING AND UNLOADING
  core::T_sp core__PERCENTdlopen( core::T_sp path_designator);
  core::T_sp core__PERCENTdlclose( ForeignData_sp handle );

}; // namespace clasp_ffi

// GC Policy Info for ForeignPataPtr_O imstances
  template <>
    struct gctools::GCInfo<clasp_ffi::ForeignData_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static GCInfo_policy constexpr Policy = normal;
  };

// ---------------------------------------------------------------------------
//   END OF FILE
// ---------------------------------------------------------------------------

#endif // __cplusplus
#endif // __CLASP_CORE_FLI_H__
