/*
    File: wrappedPointer.h
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
#ifndef core_wrappedPointer_H
#define core_wrappedPointer_H

#include <clasp/core/object.h>
#include <clasp/core/instance.h>

namespace core {

// set this class up by hand
SMART(WrappedPointer);
class WrappedPointer_O : public core::General_O {
  FRIEND_GC_SCANNER(core::WrappedPointer_O);
  LISP_CLASS(core, CorePkg, WrappedPointer_O, "WrappedPointer",core::General_O);
GCPROTECTED:
  gctools::ShiftedStamp ShiftedStamp_;
  core::Instance_sp Class_;

public:
  virtual core::Instance_sp _instanceClass() const { return this->Class_; };
  virtual T_sp _instanceClassSet(Instance_sp mc);
  void _setInstanceClassUsingSymbol(core::Symbol_sp classSymbol);
public:
CL_LISPIFY_NAME("validp");
CL_DEFMETHOD   virtual bool validp() const { SUBIMP(); };
  virtual size_t templatedSizeof() const { SUBIMP(); };
  virtual bool eql_(core::T_sp obj) const;
  virtual void *mostDerivedPointer() const {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
  Pointer_sp address() const;
  virtual void *castTo(class_id cid) const { SUBIMP(); };
  virtual class_id classId() const { SUBIMP(); };
  template <typename T>
  T *castOrNull() const {
    return static_cast<T *>(this->castTo(reg::registered_class<T>::id));
  }
  template <typename T>
  T *cast() const {
    T *result = this->castOrNull<T>();
    if (!result) {
      SIMPLE_ERROR_SPRINTF("Is inheritance defined correctly? Could not cast WrappedPointer of class %s to %s class_id/from=%d/%s class_id/to=%d/%s", _rep_(this->_instanceClass()).c_str(), _rep_(reg::lisp_classSymbol<T>()).c_str(), this->classId(), _rep_(reg::lisp_classSymbolFromClassId(this->classId())).c_str(), reg::registered_class<T>::id, _rep_(reg::lisp_classSymbolFromClassId(reg::registered_class<T>::id)).c_str());
    }
    return result;
  }
  virtual void *pointerRelease() { SUBIMP(); };
  virtual void pointerDelete() { SUBIMP(); };

public:
  explicit WrappedPointer_O() : Base(), Class_(_Nil<core::Instance_O>()){};
  virtual ~WrappedPointer_O(){};
};
};

#endif
