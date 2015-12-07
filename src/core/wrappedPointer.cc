/*
    File: wrappedPointer.cc
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
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/wrappers.h>

namespace core {

#define ARGS_core__pointer_release "(arg)"
#define DECL_core__pointer_release ""
#define DOCS_core__pointer_release "pointerRelease"
Pointer_sp core__pointer_release(T_sp ptr) {
  _G();
  if (ptr.nilp()) {
    return _Nil<Pointer_O>();
  };
  if (WrappedPointer_sp wp = ptr.asOrNull<WrappedPointer_O>()) {
    return Pointer_O::create(wp->pointerRelease());
  }
  SIMPLE_ERROR(BF("Could not release pointer for %s") % _rep_(ptr));
}

#define ARGS_core__pointer_delete "(arg)"
#define DECL_core__pointer_delete ""
#define DOCS_core__pointer_delete "pointerDelete"
void core__pointer_delete(T_sp ptr) {
  _G();
  if (ptr.nilp()) {
    return;
  };
  if (WrappedPointer_sp wp = ptr.asOrNull<WrappedPointer_O>()) {
    wp->pointerDelete();
    return;
  }
  SIMPLE_ERROR(BF("Could not release pointer for %s") % _rep_(ptr));
}

EXPOSE_CLASS(core, WrappedPointer_O);

T_sp WrappedPointer_O::instanceClassSet(Class_sp cl) {
  this->_Class = cl;
  return this->asSmartPtr();
}

void WrappedPointer_O::setInstanceClassUsingSymbol(Symbol_sp classSymbol) {
  Class_sp cl = gc::As<Class_sp>(cl__find_class(classSymbol));
  this->instanceClassSet(cl);
}

bool WrappedPointer_O::eql_(T_sp obj) const {
  _G();
  if (WrappedPointer_sp wo = obj.asOrNull<WrappedPointer_O>()) {
    return (wo->mostDerivedPointer() == this->mostDerivedPointer());
  }
  return false;
}

Pointer_sp WrappedPointer_O::address() const {
  _G();
  void *addr = this->mostDerivedPointer();
  return Pointer_O::create(addr);
}

#define ARGS_core__pointer_address "(arg)"
#define DECL_core__pointer_address ""
#define DOCS_core__pointer_address "pointerAddress"
T_sp core__pointer_address(T_sp ptr) {
  _G();
  if (ptr.nilp()) {
    return _Nil<Pointer_O>();
  };
  if (WrappedPointer_sp wp = ptr.asOrNull<WrappedPointer_O>()) {
    return wp->address();
  }
  SIMPLE_ERROR(BF("Could not get address of pointer for %s") % _rep_(ptr));
};

void WrappedPointer_O::exposeCando(core::Lisp_sp e) {
  class_<WrappedPointer_O>()
      .def("validp", &WrappedPointer_O::validp);
  Core_temp_Defun(pointer_release);
  Core_temp_Defun(pointer_delete);
  Core_temp_Defun(pointer_address);
}

void WrappedPointer_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, WrappedPointer, "", "", _lisp);
#endif //]
}
};
