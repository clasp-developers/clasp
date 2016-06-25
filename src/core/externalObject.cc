/*
    File: externalObject.cc
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

#if defined(__cplusplus)

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

#include "clasp/core/foundation.h"
#include "clasp/core/symbolTable.h"
#include "clasp/core/lisp.h"

#include "clasp/core/externalObject.h"

// last include is wrappers.h
#include "clasp/core/wrappers.h"

// ---------------------------------------------------------------------------
//   I M P L E M E N T A T I O N
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//   NAMESPACE
// ---------------------------------------------------------------------------

namespace core {

  // ---------------------------------------------------------------------------
  //   TYPE DEFINTITIONS
  // ---------------------------------------------------------------------------

  typedef std::vector<std::pair<Symbol_sp, Fixnum_sp>> symbol_alignment_pair_vector_t;
  typedef std::vector<std::pair<Symbol_sp, Fixnum_sp>> symbol_size_pair_vector_t;

  // ---------------------------------------------------------------------------
  //   LISP EXPORTS
  // ---------------------------------------------------------------------------

  // Export missing symbols for foreign objects

  SYMBOL_EXPORT_SC_(CorePkg,unsigned_char);
  SYMBOL_EXPORT_SC_(CorePkg,short);
  SYMBOL_EXPORT_SC_(CorePkg,unsigned_short);
  SYMBOL_EXPORT_SC_(CorePkg,int);
  SYMBOL_EXPORT_SC_(CorePkg,unsigned_int);
  SYMBOL_EXPORT_SC_(CorePkg,long);
  SYMBOL_EXPORT_SC_(CorePkg,unsigned_long);
  SYMBOL_EXPORT_SC_(CorePkg,long_long);
  SYMBOL_EXPORT_SC_(CorePkg,unsigned_long_long);
  SYMBOL_EXPORT_SC_(CorePkg,int8);
  SYMBOL_EXPORT_SC_(CorePkg,uint8);
  SYMBOL_EXPORT_SC_(CorePkg,int16);
  SYMBOL_EXPORT_SC_(CorePkg,uint16);
  SYMBOL_EXPORT_SC_(CorePkg,int32);
  SYMBOL_EXPORT_SC_(CorePkg,uint32);
  SYMBOL_EXPORT_SC_(CorePkg,int64);
  SYMBOL_EXPORT_SC_(CorePkg,uint64);
  SYMBOL_EXPORT_SC_(CorePkg,size);
  SYMBOL_EXPORT_SC_(CorePkg,ssize);
  SYMBOL_EXPORT_SC_(CorePkg,ptrdiff);
  SYMBOL_EXPORT_SC_(CorePkg,double);

  // ---------------------------------------------------------------------------
  //   Foreign type alignment constants
  // ---------------------------------------------------------------------------
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_CHAR(std::alignment_of<char>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UNSIGNED_CHAR(std::alignment_of<unsigned char>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_SHORT(std::alignment_of<short>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UNSIGNED_SHORT(std::alignment_of<unsigned short>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_INT(std::alignment_of<int>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UNSIGNED_INT(std::alignment_of<unsigned int>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_LONG(std::alignment_of<long>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UNSIGNED_LONG(std::alignment_of<unsigned long>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_LONG_LONG(std::alignment_of<long long>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UNSIGNED_LONG_LONG(std::alignment_of<unsigned long long>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_INT8(std::alignment_of<int8_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UINT8(std::alignment_of<uint8_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_INT16(std::alignment_of<int16_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UINT16(std::alignment_of<uint16_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_INT32(std::alignment_of<int32_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UINT32(std::alignment_of<uint32_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_INT64(std::alignment_of<int64_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UINT64(std::alignment_of<uint64_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_SIZE(std::alignment_of<size_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_SSIZE(std::alignment_of<ssize_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_PTRDIFF(std::alignment_of<ptrdiff_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_INTPTR(std::alignment_of<intptr_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_UINTPTR(std::alignment_of<uintptr_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_TIME(std::alignment_of<time_t>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_FLOAT(std::alignment_of<float>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_DOUBLE(std::alignment_of<double>::value);
  const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_POINTER(std::alignment_of<void *>::value);
  // const Fixnum_sp FOREIGN_TYPE_ALIGNMENT_VOID               ( std::alignment_of< void >::value );

  // ---------------------------------------------------------------------------
  //   Mapping between a symbol representing a foreign type and its
  //   alignment in bytes
  // ---------------------------------------------------------------------------
  const symbol_alignment_pair_vector_t FOREIGN_TYPE_ALIGNMENT_VALUES = {
    {cl::_sym_char, FOREIGN_TYPE_ALIGNMENT_CHAR},
    {core::_sym_unsigned_char, FOREIGN_TYPE_ALIGNMENT_UNSIGNED_CHAR},
    {core::_sym_short, FOREIGN_TYPE_ALIGNMENT_SHORT},
    {core::_sym_unsigned_short, FOREIGN_TYPE_ALIGNMENT_UNSIGNED_SHORT},
    {core::_sym_int, FOREIGN_TYPE_ALIGNMENT_INT},
    {core::_sym_unsigned_int, FOREIGN_TYPE_ALIGNMENT_UNSIGNED_INT},
    {core::_sym_long, FOREIGN_TYPE_ALIGNMENT_LONG},
    {core::_sym_unsigned_long, FOREIGN_TYPE_ALIGNMENT_UNSIGNED_LONG},
    {core::_sym_long_long, FOREIGN_TYPE_ALIGNMENT_LONG_LONG},
    {core::_sym_unsigned_long_long, FOREIGN_TYPE_ALIGNMENT_UNSIGNED_LONG_LONG},
    {core::_sym_int8, FOREIGN_TYPE_ALIGNMENT_INT8},
    {core::_sym_uint8, FOREIGN_TYPE_ALIGNMENT_UINT8},
    {core::_sym_int16, FOREIGN_TYPE_ALIGNMENT_INT16},
    {core::_sym_uint16, FOREIGN_TYPE_ALIGNMENT_UINT16},
    {core::_sym_int32, FOREIGN_TYPE_ALIGNMENT_INT32},
    {core::_sym_uint32, FOREIGN_TYPE_ALIGNMENT_UINT32},
    {core::_sym_int64, FOREIGN_TYPE_ALIGNMENT_INT64},
    {core::_sym_uint64, FOREIGN_TYPE_ALIGNMENT_UINT64},
    {core::_sym_size, FOREIGN_TYPE_ALIGNMENT_SIZE},
    {core::_sym_ssize, FOREIGN_TYPE_ALIGNMENT_SSIZE},
    {core::_sym_ptrdiff, FOREIGN_TYPE_ALIGNMENT_PTRDIFF},
    // { _sym_intptr,              FOREIGN_TYPE_ALIGNMENT_INTPTR              },
    // { _sym_uintptr,             FOREIGN_TYPE_ALIGNMENT_UINTPTR             },
    // {cl::_sym_time, FOREIGN_TYPE_ALIGNMENT_TIME},
    {cl::_sym_float, FOREIGN_TYPE_ALIGNMENT_FLOAT},
    {core::_sym_double, FOREIGN_TYPE_ALIGNMENT_DOUBLE},
    {core::_sym_pointer, FOREIGN_TYPE_ALIGNMENT_POINTER},
    // { _sym_void,                FOREIGN_TYPE_ALIGNMENT_VOID                }
  };

  // ---------------------------------------------------------------------------
  //   Foreign type size constants
  // ---------------------------------------------------------------------------
  const Fixnum_sp FOREIGN_TYPE_SIZE_CHAR(sizeof(char));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UNSIGNED_CHAR(sizeof(unsigned char));
  const Fixnum_sp FOREIGN_TYPE_SIZE_SHORT(sizeof(short));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UNSIGNED_SHORT(sizeof(unsigned short));
  const Fixnum_sp FOREIGN_TYPE_SIZE_INT(sizeof(int));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UNSIGNED_INT(sizeof(unsigned int));
  const Fixnum_sp FOREIGN_TYPE_SIZE_LONG(sizeof(long));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UNSIGNED_LONG(sizeof(unsigned long));
  const Fixnum_sp FOREIGN_TYPE_SIZE_LONG_LONG(sizeof(long long));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UNSIGNED_LONG_LONG(sizeof(unsigned long long));
  const Fixnum_sp FOREIGN_TYPE_SIZE_INT8(sizeof(int8_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UINT8(sizeof(uint8_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_INT16(sizeof(int16_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UINT16(sizeof(uint16_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_INT32(sizeof(int32_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UINT32(sizeof(uint32_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_INT64(sizeof(int64_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UINT64(sizeof(uint64_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_SIZE(sizeof(size_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_SSIZE(sizeof(ssize_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_PTRDIFF(sizeof(ptrdiff_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_INTPTR(sizeof(intptr_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_UINTPTR(sizeof(uintptr_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_TIME(sizeof(time_t));
  const Fixnum_sp FOREIGN_TYPE_SIZE_FLOAT(sizeof(float));
  const Fixnum_sp FOREIGN_TYPE_SIZE_DOUBLE(sizeof(double));
  const Fixnum_sp FOREIGN_TYPE_SIZE_POINTER(sizeof(void *));
  // const Fixnum_sp FOREIGN_TYPE_SIZE_VOID               ( sizeof( void ));

  // ---------------------------------------------------------------------------
  //   @brief    Mapping between a symbol representing a foreign type and its
  //   @bbrief   size in bytes
  // ---------------------------------------------------------------------------
  const symbol_size_pair_vector_t FOREIGN_TYPE_SIZE_VALUES = {
    {cl::_sym_char, FOREIGN_TYPE_SIZE_CHAR},
    {core::_sym_unsigned_char, FOREIGN_TYPE_SIZE_UNSIGNED_CHAR},
    {core::_sym_short, FOREIGN_TYPE_SIZE_SHORT},
    {core::_sym_unsigned_short, FOREIGN_TYPE_SIZE_UNSIGNED_SHORT},
    {core::_sym_int, FOREIGN_TYPE_SIZE_INT},
    {core::_sym_unsigned_int, FOREIGN_TYPE_SIZE_UNSIGNED_INT},
    {core::_sym_long, FOREIGN_TYPE_SIZE_LONG},
    {core::_sym_unsigned_long, FOREIGN_TYPE_SIZE_UNSIGNED_LONG},
    {core::_sym_long_long, FOREIGN_TYPE_SIZE_LONG_LONG},
    {core::_sym_unsigned_long_long, FOREIGN_TYPE_SIZE_UNSIGNED_LONG_LONG},
    {core::_sym_int8, FOREIGN_TYPE_SIZE_INT8},
    {core::_sym_uint8, FOREIGN_TYPE_SIZE_UINT8},
    {core::_sym_int16, FOREIGN_TYPE_SIZE_INT16},
    {core::_sym_uint16, FOREIGN_TYPE_SIZE_UINT16},
    {core::_sym_int32, FOREIGN_TYPE_SIZE_INT32},
    {core::_sym_uint32, FOREIGN_TYPE_SIZE_UINT32},
    {core::_sym_int64, FOREIGN_TYPE_SIZE_INT64},
    {core::_sym_uint64, FOREIGN_TYPE_SIZE_UINT64},
    {core::_sym_size, FOREIGN_TYPE_SIZE_SIZE},
    {core::_sym_ssize, FOREIGN_TYPE_SIZE_SSIZE},
    {core::_sym_ptrdiff, FOREIGN_TYPE_SIZE_PTRDIFF},
    // { _sym_intptr,              FOREIGN_TYPE_SIZE_INTPTR              },
    // { _sym_uintptr,             FOREIGN_TYPE_SIZE_UINTPTR             },
    // {cl::_sym_time, FOREIGN_TYPE_SIZE_TIME},
    {cl::_sym_float, FOREIGN_TYPE_SIZE_FLOAT},
    {core::_sym_double, FOREIGN_TYPE_SIZE_DOUBLE},
    {core::_sym_pointer, FOREIGN_TYPE_SIZE_POINTER},
    // { _sym_void,                FOREIGN_TYPE_SIZE_VOID                }
  };

  // ---------------------------------------------------------------------------
  //   @method   ForeignData_O CONSTRUCTOR
  // ---------------------------------------------------------------------------
  ForeignData_O::ForeignData_O() : _Kind(_Nil<T_O>()),
				   _OwnershipFlags(0),
				   _Size(0),
				   _Data(nullptr) {
    // NOTHIHG TO DO
  }

  // ---------------------------------------------------------------------------
  //   @method   ForeignData_O DESTRUCTOR
  // ---------------------------------------------------------------------------
  ForeignData_O::~ForeignData_O() {
    if (this->_OwnershipFlags & DeleteOnDtor) {
      this->freeForeignObject();
    }
  }

  // ---------------------------------------------------------------------------
  //   @method   ForeignData_O allocate
  // ---------------------------------------------------------------------------
  void ForeignData_O::allocate(T_sp kind, int ownershipFlags, size_t size) {
    this->_Kind = kind;
    this->_OwnershipFlags = ownershipFlags;
    this->_Size = size;
    this->_Data = (void *)malloc(size);
  }

  // ---------------------------------------------------------------------------
  //   @method   allocateForeignObject
  //   @brief    Return the memory alignemt of a foreign type
  // ---------------------------------------------------------------------------
  CL_LISPIFY_NAME("allocateForeignObject");
  CL_DOCSTRING("Allocate a chunk of memory for foreign data");
  CL_DEFUN ForeignData_sp ForeignData_O::allocateForeignObject(T_sp kind) {
    size_t size = 0;
    Cons_sp ckind = gc::As<Cons_sp>(kind);

    ASSERTF((oCar(ckind) == cl::_sym_array) || (oCar(ckind) == kw::_sym_array),
	    BF("The first element of a foreign-data type must be either ARRAY or :ARRAY !"));

    ASSERTF((oCadr(ckind) == cl::_sym_UnsignedByte) || (oCadr(ckind) == kw::_sym_UnsignedByte),
	    BF("The second element of a foreign-data type must be UNSIGNED-BYTE or :UNSIGNED-BYTE !"));

    size = unbox_fixnum(gc::As<Fixnum_sp>(oCaddr(ckind)));

    GC_ALLOCATE(ForeignData_O, self);
    self->initialize();

    self->allocate(kind, DeleteOnDtor, size);
    return self;
  }

  // ---------------------------------------------------------------------------
  //   @method   freeForeignObject
  //   @brief    free memory previously allocated via allocateForeignObject()
  // ---------------------------------------------------------------------------
  CL_LISPIFY_NAME("freeForeignObject");
  CL_DEFMETHOD void ForeignData_O::freeForeignObject() {
    if (this->_Data != nullptr) {
      free(this->_Data);
      this->_Data = nullptr;
    }
  }

  // ---------------------------------------------------------------------------
  //   @function core__PERCENTforeign_type_alignment
  //   @brief    Return the memory alignment of a foreign type
  // ---------------------------------------------------------------------------
  CL_DEFUN core::Fixnum_sp core__PERCENTforeign_type_alignment(core::Symbol_sp atype) {
    Fixnum_sp result = nullptr;

    auto iterator = FOREIGN_TYPE_SIZE_VALUES.begin();
    auto it_end = FOREIGN_TYPE_SIZE_VALUES.end();

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
  //   @function core__PERCENTforeign_type_size
  //   @brief    Return the size of a foreign type
  // ---------------------------------------------------------------------------
  CL_DEFUN core::Fixnum_sp core__PERCENTforeign_type_size(core::Symbol_sp atype) {
    Fixnum_sp result = nullptr;

    auto iterator = FOREIGN_TYPE_SIZE_VALUES.begin();
    auto it_end = FOREIGN_TYPE_SIZE_VALUES.end();

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

  // ---------------------------------------------------------------------------
  //   @function ExternalObject_O::eql_()
  //   @brief    EQL for ExternalObject_O
  // ---------------------------------------------------------------------------
  bool ExternalObject_O::eql_(T_sp obj) const {
    if (core__external_object_p(obj)) {
      return (gc::As<ExternalObject_sp>(obj)->externalObject() == this->externalObject());
    }

    return false;
  }

  // ---------------------------------------------------------------------------
  //   @method   ExternalObject_O::archiveBase()
  //   @brief    ???
  // ---------------------------------------------------------------------------
#if defined(XML_ARCHIVE)
  void ExternalObject_O::archiveBase(ArchiveP node) {
    this->Base::archiveBase(node);
    IMPLEMENT_ME();
  }
#endif // defined(XML_ARCHIVE)

}; // namespace

#endif // __cplusplus
