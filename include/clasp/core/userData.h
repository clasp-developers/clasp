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
#ifndef core_userData_H
#define core_userData_H

#include <clasp/core/object.h>
#include <clasp/core/lisp.h>

namespace core {

// set this class up by hand
SMART(LightUserData);
class LightUserData_O : public core::General_O // StandardObject_O
{
  LISP_CLASS(core, CorePkg, LightUserData_O, "LightUserData",General_O);

public:
  void *_ptr;

public:
  static LightUserData_sp create(void *ptr) {
    GC_ALLOCATE(LightUserData_O, v);
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
  void *ptr() const { return this->_ptr; };
  explicit LightUserData_O() : Base(), _ptr(NULL){};
  virtual ~LightUserData_O(){};
};

typedef void (*DestructUserDataFn)(void *data);

// set this class up by hand
SMART(UserData);
class UserData_O : public core::LightUserData_O
{
  LISP_CLASS(core, CorePkg, UserData_O, "UserData",core::LightUserData_O);

private:
  DestructUserDataFn _Dtor;

public:
  static UserData_sp create(size_t size, DestructUserDataFn dtor) {
    GC_ALLOCATE(UserData_O, v);
    v->_ptr = (void *)malloc(size);
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
};

#endif
