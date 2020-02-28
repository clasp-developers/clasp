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
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/pointer.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/fli.h>

namespace core {

CL_DOCSTRING(R"doc(Create a core:pointer using a fixnum as the address.)doc");
CL_DEFUN Pointer_sp core__make_pointer(T_sp address)
{
  if (address.fixnump()) {
    return Pointer_O::create((void*)address.unsafe_fixnum());
  }
  SIMPLE_ERROR(BF("Cannot convert %s to pointer") % _rep_(address));
}

CL_DEFUN SimpleBaseString_sp core__pointer_as_string(Pointer_sp p) {
  SimpleBaseString_sp s = SimpleBaseString_O::make((BF("%p") % p->ptr()).str());
  return s;
}
  
Pointer_sp Pointer_O::create(void *p) {
  GC_ALLOCATE(Pointer_O, ptr);
  ptr->m_raw_data = p;
  return ptr;
}






bool Pointer_O::eql_(T_sp obj) const {
  if (this->eq(obj)) return true;
  if (Pointer_sp pobj = obj.asOrNull<Pointer_O>()) {
    return (this->m_raw_data == pobj->m_raw_data);
  }
  return false;
}

string Pointer_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " :ptr " << (BF("%p") % this->m_raw_data).str() << ">";
  return ss.str();
}

CL_DEFUN bool core__pointer_in_pointer_range(Pointer_sp test, Pointer_sp low, T_sp high_or_size) {
  if (gc::IsA<Pointer_sp>(high_or_size)) {
    return test->in_pointer_range(low,gc::As_unsafe<Pointer_sp>(high_or_size));
  } else if (high_or_size.fixnump()) {
    return test->in_pointer_range(low,(uintptr_t)high_or_size.unsafe_fixnum());
  }
  SIMPLE_ERROR(BF("Illegal range for pointer comparison %s - %s") % _rep_(low) % _rep_(high_or_size));
}

CL_DEFUN void core__fill_foreign_memory(clasp_ffi::ForeignData_sp ptr, size_t length, size_t value)
{
  memset(ptr->ptr(), value&0xFF,length);
}

CL_DEFUN void core__replace_foreign_memory(clasp_ffi::ForeignData_sp dest, clasp_ffi::ForeignData_sp src, size_t length)
{
  memcpy(dest->ptr(),src->ptr(),length);
}


CL_DEFMETHOD Integer_sp Pointer_O::pointer_integer() const {
  return Integer_O::create((uintptr_t)this->ptr());
};

CL_DEFUN Pointer_sp core__pointer_increment(Pointer_sp ptr,Fixnum inc)
{
  return Pointer_O::create((void*)(((intptr_t)ptr->ptr())+inc));
};

};
