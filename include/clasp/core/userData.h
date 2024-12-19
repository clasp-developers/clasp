#pragma once
/*
    File: userData.h
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

#include <clasp/core/object.h>
#include <clasp/core/lisp.h>

namespace core {

// set this class up by hand
SMART(LightUserData);
class LightUserData_O : public core::General_O {
  LISP_CLASS(core, CorePkg, LightUserData_O, "LightUserData", General_O);

public:
  void* _ptr;

public:
  static LightUserData_sp create(void* ptr) {
    auto v = gctools::GC<LightUserData_O>::allocate_with_default_constructor();
    v->_ptr = ptr;
    return v;
  }

public:
  virtual bool eql_(core::T_sp obj) const {
    if (LightUserData_sp lud = obj.asOrNull<LightUserData_O>()) {
      return (lud->_ptr == this->_ptr);
    }
    return false;
  }
  void* ptr() const { return this->_ptr; };
  explicit LightUserData_O() : Base(), _ptr(NULL){};
};

typedef void (*DestructUserDataFn)(void* data);

// set this class up by hand
SMART(UserData);
class UserData_O : public core::LightUserData_O {
  LISP_CLASS(core, CorePkg, UserData_O, "UserData", core::LightUserData_O);

private:
  DestructUserDataFn _Dtor;

public:
  static UserData_sp create(size_t size, DestructUserDataFn dtor) {
    auto v = gctools::GC<UserData_O>::allocate_with_default_constructor();
    v->_ptr = (void*)malloc(size);
    v->_Dtor = dtor;
    return v;
  }

public:
  explicit UserData_O() : Base(), _Dtor(NULL){};
  virtual ~UserData_O() {
    if (this->_Dtor)
      (this->_Dtor)(this->_ptr);
    if (this->_ptr != NULL) {
      free(this->_ptr);
      this->_ptr = NULL;
    }
  };
};
}; // namespace core
