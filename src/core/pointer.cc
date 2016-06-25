/*
    File: pointer.cc
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
//    SYSTEM INCLUDES
// ---------------------------------------------------------------------------

// --- Standard C++ Includes ---
// None

// --- Platform-specific Includes ---
// NONE

// ---------------------------------------------------------------------------
//    APPLICaATION INCLUDES
// ---------------------------------------------------------------------------

#include "clasp/core/pointer.h"
#include "clasp/core/common.h"
#include "clasp/core/wrappers.h"

// ---------------------------------------------------------------------------
//   NAMESPACE
// ---------------------------------------------------------------------------

namespace core {

// ---------------------------------------------------------------------------
//   SPECIAL / GLOBAL DEFINES AND VARIABLES
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//   METHODS & FUNCTIONS
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//   @method  Pointer_O::initialize
//   @brief   Initlalize Pointer_o instance
// ---------------------------------------------------------------------------
void Pointer_O::initialize(void) {
  this->Base::initialize();
  this->m_rawptr = nullptr;
}

// ---------------------------------------------------------------------------
//   @method  Pointer_O :: create
//   @brief   Create Pointer_O instance
// ---------------------------------------------------------------------------
Pointer_sp Pointer_O::create(void *ptr) {
  GC_ALLOCATE(Pointer_O, self);
  self->initialize();
  self->m_rawptr = ptr;
  return self;
}

// ---------------------------------------------------------------------------
//   @method Pointer_O::null_pointer()
//   @brief  Make a nullptr
// ---------------------------------------------------------------------------
CL_PKG_NAME(CorePkg, null - pointer);
CL_DEFUN Pointer_sp Pointer_O::null_pointer(void) {
  return Pointer_O::create(nullptr);
}

// ---------------------------------------------------------------------------
//   @method Pointer_O::inc_pointer
//   @brief  Increase foreign pointer address by offset
// ---------------------------------------------------------------------------
CL_LISPIFY_NAME("core:inc-pointer");
CL_DEFMETHOD Pointer_sp Pointer_O::inc_pointer(core::Integer_sp offset) {
  void *new_ptr = (void *)((char *)(this->m_rawptr) + clasp_to_int(offset));
  return Pointer_O::create(new_ptr);
}

// ---------------------------------------------------------------------------
//   @method  Pointer_O :: eql_
//   @brief   EQL for Pointer_O
// ---------------------------------------------------------------------------
bool Pointer_O::eql_(T_sp obj) const {
  if (this->eq(obj)) {
    return true;
  } else {
    Pointer_sp p_obj;

    p_obj = obj.asOrNull<Pointer_O>();

    if (p_obj) {
      return (this->m_rawptr == p_obj->m_rawptr);
    }
  }

  return false;
}

// ---------------------------------------------------------------------------
//   @method  Pointer_O :: __repr__
//   @brief   Print string representation of Pointer_O
// ---------------------------------------------------------------------------
string Pointer_O::__repr__() const {
  stringstream ss;

  ss << "#<"
     << this->_instanceClass()->classNameAsString()
     << " :ptr "
     << (BF("0x%x") % this->m_rawptr)
     << ">";

  return ss.str();
}

// ---------------------------------------------------------------------------
//   @method Pointer_O::make()
//   @brief  Makes a pointer
// ---------------------------------------------------------------------------
CL_PKG_NAME(CorePkg, make - pointer);
CL_DEFUN Pointer_sp Pointer_O::make(core::Number_sp arg) {
  if (core__fixnump(arg)) {
    if (sizeof(unbox_fixnum(gc::As<core::Fixnum_sp>(arg))) != sizeof(void *)) {
      SIMPLE_ERROR(BF("You cannot make a pointer using an integer as the address sizeof(void*)=%d sizeof(Fixnum)=%d") % sizeof(void *) % sizeof(unbox_fixnum(gc::As<core::Fixnum_sp>(arg))));
    }

    IMPLEMENT_MEF(BF("Deal with converting Fixnum or Bignum to void*"));

#if 0
    return Pointer_O::create( (void*) ( arg.as< core::Fixnum_O >()->get()) );
#endif
  }

  SIMPLE_ERROR(BF("Illegal type for address of pointer"));
};

}; // namespace

#endif // __cplusplus
