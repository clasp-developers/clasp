/*
    File: foreign_data.h
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
//  FOREIGN DATA  - Foreign Data Interface for allocating, setting,
//                  getting and freeing foreign memory.
//
//  August 2016 by Frank Goenninger, Gönninger B&T UG, Germany
//
// ===========================================================================

#if !defined(__CLASP_CORE_FOREIGN_DATA_H__)
#define __CLASP_CORE_FOREIGN_DATA_H__ __FILE__" $Id$"

// ---------------------------------------------------------------------------
//    SYSTEM INCLUDES
// ---------------------------------------------------------------------------

// --- Standard C++ Includes ---
// NONE

// --- Platform-specific Includes ---
// NONE

// ---------------------------------------------------------------------------
//    APPLICaATION INCLUDES
// ---------------------------------------------------------------------------

#include <clasp/core/externalObject.h>

#if defined(__cplusplus)

// ---------------------------------------------------------------------------
//   NAMESPACE
// ---------------------------------------------------------------------------

namespace core {

// ---------------------------------------------------------------------------
//   CLASSES & METHODS & FUNCTIONS
// ---------------------------------------------------------------------------

  SMART(ForeignData);

  class ForeignData_O : public ExternalObject_O {
    LISP_CLASS(core, CorePkg,
               ForeignData_O, "ForeignData", ExternalObject_O);

  public:
  // CTOR & DTOR
    explicit ForeignData_O();
    virtual ~ForeignData_O(); // non-trivial

  // OVERLADED FUNCTIONS
    void *externalObject();
    bool eql_(T_sp obj);

    static ForeignData_sp create(const cl_intptr_t address = 0);

  // LISP EXPOSED CTORs
    static ForeignData_sp PERCENTmake_pointer(T_sp address);
    static ForeignData_sp PERCENTmake_nullpointer();

  // SLOT ACCESS

  // -- SETTER & GETTER --
    const T_sp kind() { return m_kind; };
    const int ownership_flags() { return m_ownership_flags; };
    const void *orig_data_ptr() { return m_orig_data_ptr; };
    const void *raw_data() { return m_raw_data; };

  // -- TRANSFORMING ACCESS --
    template <class T>
      T data() { return reinterpret_cast<T>(this->raw_data()); };

  // LISP EXPOSED SETTER & GETTER METHODS
    T_sp PERCENTkind() { return this->kind(); };
    Integer_sp PERCENTownership_flags() { return Integer_O::create((gctools::Fixnum)this->ownership_flags()); };
    Integer_sp PERCENTforeign_data_ptr_address() { return Integer_O::create((uint64_t)this->data<cl_intptr_t>()); };

  // MAKE AND CREATE - LISP EXPOSED FUNCTIONS

  // static ForeignData_sp PERCENTallocate_foreign_object(T_sp kind);
    static ForeignData_sp PERCENTallocate_foreign_data(Integer_sp size);
    void PERCENTfree_foreign_data();

  // POINTER ADDRESS MANIPULATION
    ForeignData_sp PERCENTinc_pointer(Integer_sp offset);

  // MEMORY ACEESS / MEMORY CONTENT CONVERSION
    static T_sp PERCENTmem_ref(Integer_sp address);
    static void PERCENTset_mem_ref(Integer_sp address, T_sp value);

  // OBJECT PRINTING
    string __repr__() const;

  private:
  // MENORY MGMT
    void allocate(T_sp kind, ForeignDataFlagEnum ownership_flags, size_t size);
    void free();

  // SLOTS
    T_sp m_kind;
    ForeignDataFlagEnum m_ownership_flags;
    size_t m_size;

    void *m_orig_data_ptr;
  // If we allocate memory then we save the address o  the original address.
  // This enables changing the poimter of m__ata without loosing the ability
  // to free the originally allocated memory.

    void *m_raw_data;
  }; // ForeignData_O

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
  CL_DEFUN Fixnum_sp core__PERCENTforeign_type_alignment(Symbol_sp atype) {
    Fixnum_sp result = nullptr;

    auto iterator = FOREIGN_TYPE_SPEC_TABLE.begin();
    auto it_end = FOREIGN_TYPE_SPEC_TABLE.end();

    for (; iterator != it_end; iterator++) {
      if (iterator->first == atype) {
        result = iterator->second;
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
  CL_DEFUN Fixnum_sp core__PERCENTforeign_type_size(Symbol_sp atype) {
    Fixnum_sp result = nullptr;

    auto iterator = FOREIGN_TYPE_SPEC_TABLE.begin();
    auto it_end = FOREIGN_TYPE_SPEC_TABLE.end();

    for (; iterator != it_end; iterator++) {
      if (iterator->first == atype) {
        result = iterator->second;
        goto RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_SIZE;
      }
    }

    SIMPLE_ERROR(BF("No foreign type size available for %s") % _rep_(atype));
    return nullptr;

  RETURN_FROM_CORE__PERCENT_FOREIGN_TYPE_SIZE:

    return result;
  }

}; // namespace core

// GC Policy Info for ForeignPataPtr_O imstances
template <>
struct gctools::GCInfo<core::ForeignData_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

// ---------------------------------------------------------------------------
//   END OF FILE
// ---------------------------------------------------------------------------

#endif // __cplusplus
#endif // __CLASP_CORE_FOREIGN_DATA_H__
